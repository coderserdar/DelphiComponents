{$B-,S-,R-,V-,X+}

{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{
  A file transfer utility that uses NetWare SPX services.

  Usage: NSSend [/Cxxxx] [/R] [FileMask [FileMask...]]

    /Cxxxx  Set connect retry time to xxxx seconds (default 60)
    /R      Enable receive mode

 On the sending station, call NSSend with at least one FileMask, where
 FileMask can specify a single file or use DOS wildcards. On the
 receiving station, call NSSend with the /R option but without any
 file masks.

 Each station uses a known socket (HandshakeSocket) to look for the
 other. Each side retries up to the specified retry time.

 Received files are always stored in the current directory of the
 receiver, regardless of the pathname specified by the sender. The
 full pathname is sent to the receiver, but this implementation of
 NSSend ignores the path information. The receiver sets the timestamp
 of the file to match that of the sender. Existing files are
 overwritten without warning.

 Press <Esc> at any time to abort the transfer. The receiver deletes
 any partial files.

 Note that this version of NSSend when compiled for Windows uses
 strings in preference over PChars, except when dealing with routines
 exported from the WinDOS unit.
}

program NSSend;

uses
  {$IFDEF Windows}
  {$IFDEF Ver80}
  SysUtils,
  {$ELSE}
  Strings,
  {$ENDIF}
  WinCrt,
  WinDos,
  {$ELSE}
  Crt,
  Dos,
  {$ENDIF}
  NWIPXSPX,
  NWBase,
  NWConn;

const
  MaxPacketSize = 512;            {bytes sent per packet}

const
  TxtFileInfo  = 1;               {file info message}
  TxtFileData  = 2;               {file data message}
  TxtFileEnd   = 3;               {file end message}
  TxtJobEnd    = 4;               {job end message}
  TxtFileDel   = 5;               {file delete message}

{Structure of each message passed through SPX}
type
  PDataPacket = ^DataPacket;
  DataPacket = record
    TxtType   : word;                    {one of the values defined above}
    TxtLength : word;                    {number of bytes in Txt}
    Txt       : array [0..pred(MaxPacketSize)] of char;
                                         {message buffer}
  end;

  FileDirEntry = record
    Name : string[12];
    Size : longint;
    Time : longint;
    Attr : byte;
  end;

{IPX addressing}
const
  SenderSocket    = $4445;        {socket for data transfer - sender}
  ReceiverSocket  = $4446;        {socket for data transfer - receiver}
  HandshakeSocket = $4447;        {separate channel for IPX handshake}
var
  Receiver   : IPXAddress;        {address of partner}
  SPXVersion : word;              {returned by SPXServicesAvail}
  MaxSPXConn : word;
  AvailSPXConn : word;

{Options}
const
  ConnectRetryTime : word = 60;     {seconds to retry when connecting}
  InReceiveMode : boolean = false;  {set to 1 with /R option}
  SessionCreated : boolean = false; {true when session established}

{Variables used for data transfer}
const
  IPXEvent : PipxEventRec = nil;         {used to find receiver}
  IPXBuf   : PPacket = nil;         { --ditto-- }
  DataBuf  : PDataPacket = nil;     {send buffer}
  Event    : PspxEventRec = nil;         {send or listen event}
  DataBytes : longint = 0;            {total bytes transferred}
var
  DataIndex : byte;                   {index of last packet received}
  DataBufL  : PDataPacket;          {pointer to last received packet}
  DataStart : longint;                {time when transfer started}

{Special error code}
const
  UserBreak   = 127;
  RemoteBreak = 126;

var
  IOerror : integer;
  ExitSave : pointer;

procedure ShowHelp;
  {-Display help screen}
  begin
    writeln('Usage: NSSEND [/Cxxxx] [/R] [FileMask [FileMask...]]');
    writeln('  /Cxxxx Set connect retry time to xxxx seconds (default ', ConnectRetryTime, ')');
    writeln('  /R     Enable receive mode');
    Halt(0);
  end;

function IOfailed : boolean;
  begin
    IOerror := IOresult;
    IOfailed := IOerror <> 0;
  end;

procedure InvalidParam(Param : string);
  begin
    writeln(Param, ' is an invalid parameter');
    Halt(1);
  end;

procedure EvaluateOptions;
  {-Set global options based on command line}
  var
    i, ec    : integer;
    len      : integer;
    FileArgs : integer;
    Param    : string[128];
  begin
    FileArgs := 0;

    for i := 1 to ParamCount do
      begin
        Param := ParamStr(i);
        if ((Param[1] = '-') or (Param[1] = '/')) then
          begin
            len := length(Param);
            if (len = 1) then
              InvalidParam(Param)
            else
              begin
                case UpCase(Param[2]) of
                  'C' :
                    begin
                      if (len < 3) then
                        InvalidParam(Param);
                      Val(Copy(Param, 3, 255), ConnectRetryTime, ec);
                      if (ConnectRetryTime > 9999) then
                        ConnectRetryTime := 9999;
                    end;
                  'R' :
                    begin
                      if (len <> 2) then
                        InvalidParam(Param);
                      InReceiveMode := true;
                    end;
                else
                  InvalidParam(Param);
                end;{case}
              end;
          end
        else
          inc(FileArgs);
      end;

    if (FileArgs = 0) and (not InReceiveMode) then
      begin
        writeln('No filenames specified');
        Halt(1);
      end;
  end;

procedure AllocateBuffers;
  {-Allocate packet and event buffers}
  begin
    IPXEvent := IPXAllocEventRec(IPXDoNothingESR);
    IPXBuf := IPXAllocPacket(1);
    if InReceiveMode then
      {receiver may need lots of ECBs to keep up with sender}
      Event := SPXAllocEventRec(10, sizeof(DataPacket))
    else
      {sender only needs 2 ECBs}
      Event := SPXAllocEventRec(2, sizeof(DataPacket));
    DataBuf := IPXAllocPacket(sizeof(DataPacket));
    if ((IPXEvent = nil) or
        (IPXBuf = nil) or
        (Event = nil) or
        (DataBuf = nil)) then
      begin
        writeln('Unable to allocate packet buffers');
        Halt(1);
      end;
  end;

function UserHasAborted : boolean;
  {-Return true if user pressed <Esc>}
  var
    ch : char;
  begin
    UserHasAborted := false;
    if KeyPressed then
      begin
        Ch := ReadKey;
        UserHasAborted := (Ch = #27);
      end;
  end;

procedure StatusBackup;
  {-Backspace over the status display}
  begin
    write(^H^H^H^H);
  end;

procedure StatusVal(Value : word);
  {-Display the value as a status}
  begin
    write(Value:4);
  end;

procedure StatusPercent(Percent : word);
  {-Display the percentage as a status}
  begin
    write(Percent:3, '%');
  end;

procedure TerminateOnUserBreak;
  {-Abort if user pressed <Esc>}
  begin
    if UserHasAborted then
      begin
        StatusBackup;
        writeln('(user break)');
        Halt(2);
      end;
  end;

function WaitForEventComplete(SPXEvent : PspxEventRec) : byte;
  {-Wait for event complete, user break, or timeout}
  var
    Status : byte;
  begin
    while not SPXEventComplete(SPXEvent, Status) do
      if UserHasAborted then
        begin
          WaitForEventComplete := UserBreak;
          Exit;
        end;
    WaitForEventComplete := Status;
  end;

function GetTimeInSecs : longint;
  var
    H, M, S, T : word;
  begin
    GetTime(H, M, S, T);
    GetTimeInSecs := (((longint(H) * 60) + longint(M)) * 60) + longint(S);
  end;

procedure OurExitFunc; far;
  {-Clean up when exiting}
  var
    DataEnd : longint;
  begin
    ExitProc := ExitSave;
    if (not InReceiveMode) and (DataBytes <> 0) then
      begin
        {write out transfer rate}
        DataEnd := GetTimeInSecs;
        if (DataEnd <> DataStart) then
          writeln('Transfer rate ',
                  (DataBytes div (DataEnd - DataStart)),
                  ' bytes/sec');
      end;

    {shut down the IPX handshaking event}
    IPXCloseSocket(HandShakeSocket);
    IPXFreeEventRec(IPXEvent);
    IPXFreePacket(IPXBuf);

    {shut down the SPX transfer}
    if (Event <> nil) then
      begin
        if SessionCreated then
          SPXTerminateConn(Event);
        SPXFreeEventRec(Event);
      end;
  end;

procedure ExitBadMessage(Status : byte; Direction : string);
  {-Report error and exit}
  begin
    StatusBackup;
    case Status of
      UserBreak   : writeln('(user break)');
      RemoteBreak : writeln('(remote break)');
      {...}
    else
      writeln('(', Direction, ' error ', Status, ')');
    end;{case}
    Halt(2);
  end;

procedure SendNetMessage;
  {-Send message and wait for event to complete}
  var
    Status   : byte;
    MessSize : word;
  begin
    MessSize := sizeof(DataPacket) - MaxPacketSize + DataBuf^.TxtLength;
    Status := SPXSend(Event, false, MessSize, DataBuf);
    if (Status = 0) then
      Status := WaitForEventComplete(Event);
    if (Status <> 0) then
      ExitBadMessage(Status, 'send');
    inc(DataBytes, MessSize);
  end;

procedure SendInfoMessage(MsgNum : word);
  {-Send an information-only message}
  begin
    with DataBuf^ do
      begin
        TxtType := MsgNum;
        TxtLength := 0;
      end;
    SendNetMessage;
  end;

function ReceiveNetMessage : byte;
  {-Receive a message}
  var
    Status : byte;
    DataType : byte;
  begin
    while not SPXPacketReceived(Event, DataIndex,
                                DataType, Status, pointer(DataBufL)) do
      if UserHasAborted then
        begin
          ReceiveNetMessage := UserBreak;
          Exit;
        end;
    ReceiveNetMessage := Status;
  end;

function IPXAddressesEqual(var Addr1, Addr2 : IPXAddress) : boolean;
  type
    IAarray = array [1..10] of byte;
  var
    i  : integer;
    A1 : IAarray absolute Addr1;
    A2 : IAarray absolute Addr2;
  begin
    IPXAddressesEqual := false;
    for i := 1 to 10 do
      if (A1[i] <> A2[i]) then
        Exit;
    IPXAddressesEqual := true;
  end;



procedure OpenSession;
  {-Make connection between sender and receiver}
  var
    Status          : byte;
    EstablishPosted : boolean;
    NetNo           : word;
    StartTime       : longint;
    EndTime         : longint;
    PrevTime        : longint;
    CurTime         : longint;
    Self            : IPXAddress;
    NetList         : TnwNetworkList;
  begin
    if (InReceiveMode) then
      begin
        writeln('Finding sender ');

        {get list of network numbers}
        nwGetNetworkList(nwDefaultServer, NetList);

        {prepare Receiver address for broadcast}
        FillChar(Receiver, sizeof(Receiver), 0);
        Receiver.Node := IPXAllNodes;

        {post listen for sender's SPX connection}
        Status := SPXListenForConn(Event, ReceiverSocket, false);
      end
    else
      begin
        writeln('Finding receiver ');

        {post listen for receiver's IPX broadcast}
        Status := IPXListen(IPXEvent, HandshakeSocket, false, 1, IPXBuf);
        EstablishPosted := false;
      end;
    if (Status <> 0) then
      begin
        writeln('(unable to post listen event)');
        Halt(1);
      end;

    StartTime := GetTimeInSecs;
    EndTime := StartTime + ConnectRetryTime;
    PrevTime := StartTime;
    StatusVal(0);
    CurTime := GetTimeInSecs;
    while (not SessionCreated) and (CurTime <= EndTime) do
      begin
        {update status ticker}
        if (CurTime <> PrevTime) then
          begin
            StatusBackup;
            StatusVal(CurTime - StartTime);
            PrevTime := CurTime;
          end;

        {provide a chance to break}
        TerminateOnUserBreak;

        if InReceiveMode then
          begin
            {has SPX connection been made?}
            if SPXEventComplete(Event, Status) then
              begin
                if (Status = 0) then
                  SessionCreated := true;
              end
            else
              {send IPX broadcast to all networks}
              for NetNo := 1 to NetList.Count do
                begin
                  Receiver.Network := NetList.List[NetNo];
                  Status := IPXSend(IPXEvent, Receiver, HandshakeSocket, true, 1, IPXBuf);
                end;
          end
        else {in send mode}
          if EstablishPosted then
            begin
              if SPXEventComplete(Event, Status) then
                if (Status = 0) then
                  SessionCreated := true;
            end
          else
            begin
              {has IPX broadcast been received?}
              if IPXEventComplete(IPXEvent, Status) then
                if (Status = 0) then
                  begin
                    {proper message received, save partner's address}
                    Receiver := IPXEvent^.Header.Source;

                    {establish SPX connection given receiver's address}
                    Status := SPXEstablishConn(Event, Receiver, SenderSocket,
                                               ReceiverSocket, false);
                    if (Status = 0) then
                      EstablishPosted := true;
                  end;
            end;
        CurTime := GetTimeInSecs;
      end;

    StatusBackup;
    if not SessionCreated then
      begin
        writeln('(unable to open session)');
        Halt(2);
      end;

    write('(session open');

    {detect window to window transfer in Windows}
    if not InReceiveMode then
      begin
        IPXInternetAddress(Self);
        if IPXAddressesEqual(Self, Receiver) then
            write(', same station');
      end;

    writeln(')');
  end;

procedure SendFile(PName : string; var FRec : FileDirEntry);
  {-Send one file to the receiver}
  var
    Len    : word;
    Status : byte;
    Bytes  : longint;
    Percent: word;
    EndTime: longint;
    F      : file;
  begin
    write('Sending ', PName, ' ');

    {assure file can be opened}
    Assign(F, PName);
    Reset(F, 1);
    if IOfailed then
      begin
        writeln('(unable to open, error ', IOerror, ')');
        Exit;
      end;

    {send file info packet}
    with DataBuf^ do
      begin
        TxtType := TxtFileInfo;
        TxtLength := sizeof(FRec) + length(PName) + 1;
        Move(FRec, Txt[0], sizeof(FRec));
        Move(PName, Txt[0+sizeof(FRec)], length(PName)+1);
      end;
    StatusPercent(0);
    SendNetMessage;

    {send the file itself}
    Bytes := 0;
    EndTime := GetTimeInSecs + 1;
    repeat
      BlockRead(F, DataBuf^.Txt, MaxPacketSize, Len);
      if (Len <> 0) then
        begin
          with DataBuf^ do
            begin
              TxtType := TxtFileData;
              TxtLength := Len;
            end;
          SendNetMessage;
          inc(Bytes, Len);
          if (GetTimeInSecs >= EndTime) then
            begin
              Percent := (100 * Bytes) div FRec.Size;
              StatusBackup;
              StatusPercent(Percent);
              EndTime := GetTimeInSecs+1;
            end;
        end;
    until (Len = 0);

    Close(F);

    if (Bytes <> FRec.Size) then
      begin
        {error reading file}
        StatusBackup;
        writeln('(error reading file)');
        SendInfoMessage(TxtFileDel);
      end
    else
      begin
        StatusBackup;
        StatusPercent(100);
        {send end of file packet}
        SendInfoMessage(TxtFileEnd);
        StatusBackup;
        writeln('(ok)');
      end;
  end;

procedure SendFiles;
  {-Send all files matching specified masks}
{$IFDEF Windows}
  var
    i, j : integer;
    W    : word;
    FullPath : array [0..80] of char;
    FullPathPas : string[80];
    Path     : array [0..80] of char;
    Name     : array [0..8] of char;
    Ext      : array [0..4] of char;
    Param    : array [0..128] of char;
    SRec     : TSearchRec;
    FRec     : FileDirEntry;
  begin
    DataStart := GetTimeInSecs;

    for i := 1 to ParamCount do
      begin
        StrPCopy(Param, ParamStr(i));
        if ((Param[0] <> '-') and (Param[0] <> '/')) then
          begin
            {a file argument to evaluate}
            for j := 0 to StrLen(Param) do
              Param[j] := Upcase(Param[j]);
            W := FileSplit(Param, Path, Name, Ext);

            FindFirst(Param, faArchive, SRec);
            while (DosError = 0) do
              begin
                {got a matching file, build its full pathname}
                StrCopy(FullPath, Path);
                StrCat(FullPath, SRec.Name);

                {convert the search rec type into a file dir entry}
                FRec.Name := StrPas(SRec.Name);
                FRec.Size := SRec.Size;
                FRec.Time := SRec.Time;
                FRec.Attr := SRec.Attr;

                {send the file to the receiver}
                FullPathPas := StrPas(FullPath);
                SendFile(FullPathPas, FRec);

                {look for next matching directory entry}
                FindNext(SRec);
              end;
          end;
      end;

    {tell the receiver transfer is done}
    SendInfoMessage(TxtJobEnd);
  end;
{$ELSE}
  var
    i, j : integer;
    FullPath : string[80];
    Path     : string[80];
    Name     : string[8];
    Ext      : string[4];
    Param    : string[128];
    SRec     : SearchRec;
    FRec     : FileDirEntry;
  begin
    DataStart := GetTimeInSecs;

    for i := 1 to ParamCount do
      begin
        Param := ParamStr(i);
        if ((Param[1] <> '-') and (Param[1] <> '/')) then
          begin
            {a file argument to evaluate}
            for j := 1 to length(Param) do
              Param[j] := Upcase(Param[j]);
            FSplit(Param, Path, Name, Ext);

            FindFirst(Param, Archive, SRec);
            while (DosError = 0) do
              begin
                {got a matching file, build its full pathname}
                FullPath := FExpand(Path + SRec.Name);

                {convert the search rec type into a file dir entry}
                FRec.Name := SRec.Name;
                FRec.Size := SRec.Size;
                FRec.Time := SRec.Time;
                FRec.Attr := SRec.Attr;

                {send the file to the receiver}
                SendFile(FullPath, FRec);

                {look for next matching directory entry}
                FindNext(SRec);
              end;
          end;
      end;

    {tell the receiver transfer is done}
    SendInfoMessage(TxtJobEnd);
  end;
{$ENDIF}

procedure ReceiveFiles;
  {-Receive all files sent by the sender}
  type
    StateMachine = (WaitingForName, GettingData, AllDone);
  var
    State    : StateMachine;
    Len      : word;
    Status   : byte;
    Bytes    : longint;
    TType    : word;
    Percent  : word;
    EndTime  : longint;
    RecvPath : string[80];
    FRec     : FileDirEntry;
    F        : file;
  begin
    State := WaitingForName;
    repeat
      {get next message from sender}
      Status := ReceiveNetMessage;
      {if error in ReceiveNetMessage: force current file to be deleted}
      if (Status <> 0) then
           TType := TxtFileDel
      else TType := DataBufL^.TxtType;

      case State of
        WaitingForName :
          case TType of
            TxtJobEnd :
              State := AllDone;
            TxtFileInfo :
              begin
                {copy the file info into local variables}
                with DataBufL^ do
                  begin
                    Move(Txt[0], FRec, sizeof(FRec));
                    Move(Txt[sizeof(FRec)], RecvPath, TxtLength-sizeof(FRec));
                  end;
                write('Receiving ', FRec.Name, ' ');
                {RecvPath is ignored here; file is created in current directory}
                Assign(F, FRec.Name);
                Rewrite(F, 1);
                if IOfailed then
                  writeln('(unable to create, error ', IOerror, ')')
                else
                  begin
                    Bytes := 0;
                    StatusPercent(0);
                    EndTime := GetTimeInSecs + 1;
                    State := GettingData;
                  end;
              end;
            TxtFileDel :
              begin
                if (Status <> 0) then
                  begin
                    {error in ReceiveNetMessage}
                    ExitBadMessage(Status, 'receive');
                  end;
              end;
          end;{case}
        GettingData :
          case TType of
            TxtJobEnd :
              begin
                StatusBackup;
                writeln('(error receiving file)');
                Close(F);
                if IOfailed then {nothing};
                Erase(F);
                if IOfailed then {nothing};
                State := AllDone;
              end;
            TxtFileData :
              begin
                BlockWrite(F, DataBufL^.Txt, DataBufL^.TxtLength, Len);
                if (Len <> DataBufL^.TxtLength) then
                  begin
                    StatusBackup;
                    writeln('(error writing file)');
                    Close(F);
                    if IOfailed then {nothing};
                    Erase(F);
                    if IOfailed then {nothing};
                    State := WaitingForName;
                  end
                else
                  begin
                    inc(Bytes, Len);
                    if (GetTimeInSecs >= EndTime) then
                      begin
                        Percent := (100 * Bytes) div FRec.Size;
                        StatusBackup;
                        StatusPercent(Percent);
                        EndTime := GetTimeInSecs+1;
                      end;
                  end;
              end;
            TxtFileEnd :
              begin
                StatusBackup;
                StatusPercent(100);
                SetFTime(F, FRec.Time);
                if (DosError <> 0) then
                  begin
                    StatusBackup;
                    write('(unable to set timestamp)     ');
                  end;
                Close(F);
                if IOfailed then
                  begin
                    StatusBackup;
                    writeln('(unable to close)');
                    {delete the partial file}
                    Erase(F);
                    if IOfailed then {nothing};
                  end
                else
                  begin
                    StatusBackup;
                    writeln('(ok)');
                  end;
                State := WaitingForName;
              end;
            TxtFileDel :
              begin
                Close(F);
                if IOfailed then {nothing};
                Erase(F);
                if IOfailed then {nothing};
                State := WaitingForName;
                if (Status <> 0) then
                  begin
                    {error in ReceiveNetMessage}
                    ExitBadMessage(Status, 'receive');
                  end;
              end;
          end;{case}
      end;{case}

      {reactivate buffer for reuse}
      SPXReactivateECB(Event, DataIndex);

    until (State = AllDone);
  end;

begin
  {perform startup checking}
  writeln('NSSend  Copyright (c) 1993 by TurboPower Software.  Version 5.55');
  if (not SPXServicesAvail(SPXVersion, MaxSPXConn, AvailSPXConn)) then
    begin
      writeln('An SPX driver must be installed');
      halt(1);
    end;;
  if (ParamCount <= 1) then
    ShowHelp;

  {evaluate command line options}
  EvaluateOptions;

  {install exit function}
  ExitSave := ExitProc;
  ExitProc := @OurExitFunc;

  {allocate the packet and event buffers}
  AllocateBuffers;

  {open the session}
  OpenSession;

  {send or receive the files}
  if (InReceiveMode) then
       ReceiveFiles
  else SendFiles;

  {the exit procedure will clean up}
end.
