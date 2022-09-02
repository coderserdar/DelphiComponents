{$B-,S-,R-,V-,X+}

{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{
  A file transfer utility that uses NetBIOS services.

  Usage: NBSend [/Cxxxx] [/R] [FileMask [FileMask...]]

    /Cxxxx  Set connect retry time to xxxx seconds (default 60)
    /R      Enable receive mode

 On the sending station, call NBSend with at least one FileMask, where
 FileMask can specify a single file or use DOS wildcards. On the
 receiving station, call NBSend with the /R option but without any
 file masks.

 Each station adds a NetBIOS name and then attempts to open a session
 with the other station. Each side retries up to the specified retry
 time.

 Received files are always stored in the current directory of the
 receiver, regardless of the pathname specified by the sender. The
 full pathname is sent to the receiver, but this implementation of
 NBSend ignores the path information. The receiver sets the timestamp
 of the file to match that of the sender. Existing files are
 overwritten without warning.

 Press <Esc> at any time to abort the transfer. The receiver deletes
 any partial files. (Since NetBIOS does not allow cancelling a
 NetBiosAddName command, NBSEND does not allow you to abort while names
 are being added.)

 Note that this version of NBSend when compiled for Windows uses
 strings in preference over PChars, except when dealing with routines
 exported from the WinDOS unit.
}

program NBSend;

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
  NetBios;

const
  {$IFDEF Windows}
  MaxPacketSize = 512;            {bytes sent per packet - Windows doesn't like large packets}
  {$ELSE}
  MaxPacketSize = 16*1024;        {bytes sent per packet}
  {$ENDIF}

const
  TxtFileInfo  = 1;               {file info message}
  TxtFileData  = 2;               {file data message}
  TxtFileEnd   = 3;               {file end message}
  TxtJobEnd    = 4;               {job end message}
  TxtFileDel   = 5;               {file delete message}
  TxtPacketSize= 6;               {message to agree max packet size}

{Structure of each message passed through NetBIOS}
type
  DataPacketPtr = ^DataPacket;
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

const
  InfoMsgSize = sizeof(DataPacket) - MaxPacketSize;

{NetBIOS addressing}
const
  SendSessName : NBNameStr = 'NBSENDER';
  RecvSessName : NBNameStr = 'NBRECEIVER';
var
  SessionNumber : byte;             {local session number}

{Options}
const
  ConnectRetryTime : word = 60;     {seconds to retry when connecting}
  InReceiveMode : boolean = false;  {set to 1 with /R option}
  NameAdded : boolean = false;      {true once the name has been added}
  SessionCreated : boolean = false; {true when session established}

{Variables used for data transfer}
const
  DataBuf   : DataPacketPtr = nil;  {data transfer buffer}
  Event     : PNCB = nil;           {NetBIOS control block}
  DataBytes : longint = 0;          {total bytes transferred}
var
  DataStart : longint;              {time when transfer started}
  PacketSize: word;                 {actual packet size used}

{Special error code}
const
  UserBreak   = 127;

var
  IOerror : integer;
  ExitSave : pointer;

procedure ShowHelp;
  {-Display help screen}
  begin
    writeln('Usage: NBSend [/Cxxxx] [/R] [FileMask [FileMask...]]');
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
    Event := NetBiosAllocNCB;
    DataBuf := NetBiosAllocPacket(sizeof(DataPacket));
    if ((Event = nil) or
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

function WaitForEventComplete(Event : PNCB) : byte;
  {-Wait for event complete, user break, or timeout}
  var
    Status : byte;
  begin
    while not NetBiosCmdCompleted(Event, Status) do
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
    Status  : byte;
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

    {shut down the NetBIOS event}
    if (Event <> nil) then
      begin
        if not NetBiosCmdCompleted(Event, Status) then
          Status := NetBiosCancelRequest(Event);
        NetBiosFreeNCB(Event);
      end;
    {destroy the rest}
    if (DataBuf <> nil) then
      NetBiosFreePacket(DataBuf);
    if SessionCreated then
      Status := NetBiosHangup(SessionNumber);
    if NameAdded then
      if InReceiveMode then
           Status := NetBiosDeleteName(RecvSessName)
      else Status := NetBiosDeleteName(SendSessName);
  end;

procedure ExitBadMessage(Status : byte; Direction : string);
  {-Report error and exit}
  begin
    StatusBackup;
    case Status of
      UserBreak   : writeln('(user break)');
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
    MessSize := InfoMsgSize + DataBuf^.TxtLength;
    NetBiosSendNoWait(Event, nil, SessionNumber, MessSize, DataBuf);
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
    NetBiosReceiveNoWait(Event, nil, SessionNumber, sizeof(DataPacket), DataBuf);
    ReceiveNetMessage := WaitForEventComplete(Event);
  end;

procedure OpenSession;
  {-Make connection between sender and receiver}
  var
    NameNum   : byte;
    Status    : byte;
    StartTime : longint;
    EndTime   : longint;
    PrevTime  : longint;
    CurTime   : longint;
  begin
    write('Adding NetBIOS name ');
    if (InReceiveMode) then
         Status := NetBiosAddName(RecvSessName, NameNum)
    else Status := NetBiosAddName(SendSessName, NameNum);
    if (Status <> 0) then
      begin
        writeln('(error=', Status, ')');
        Halt(2);
      end
    else
      writeln('(ok)');
    NameAdded := true;

    {start a session with our partner}
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

        if InReceiveMode then
             NetBiosListenNoWait(Event, nil, SendSessName, RecvSessName, 5, 5)
        else NetBiosOpenNoWait(Event, nil, RecvSessName, SendSessName, 5, 5);

        {loop until either the command completes, the user breaks, or our
         time is up}
        repeat
          {provide a chance to break}
          TerminateOnUserBreak;
          {update the display}
          CurTime := GetTimeInSecs;
          if (CurTime <> PrevTime) then
            begin
              StatusBackup;
              StatusVal(CurTime - StartTime);
              PrevTime := CurTime;
              if (CurTime > EndTime) then
                Status := NetBiosCancelRequest(Event);
            end;
        until NetBiosCmdCompleted(Event, Status);

        if (Status = 0) then
          begin
            SessionNumber := Event^.LSN;
            SessionCreated := true;
            StatusBackup;
            writeln('(contact)');

            {agree on a packet size - Windows and DOS use different ones}
            if InReceiveMode then
              begin
                with DataBuf^ do
                  begin
                    TxtType := TxtPacketSize;
                    TxtLength := sizeof(word);
                    PacketSize := MaxPacketSize;
                    Move(PacketSize, Txt[0], sizeof(word));
                  end;
                SendNetMessage;
              end
            else
              begin
                Status := ReceiveNetMessage;
                with DataBuf^ do
                  if (Status = 0) and (TxtType = TxtPacketSize) then
                    begin
                      Move(Txt[0], PacketSize, sizeof(word));
                      if (PacketSize > MaxPacketSize) then
                        PacketSize := MaxPacketSize;
                      writeln('(agreed on ', PacketSize, ' bytes per packet)');
                    end;
              end;
          end
        else if (Status = NBECommandCancelled) then
          Status := UserBreak;
      end;

    StatusBackup;
    if not SessionCreated then
      begin
        writeln('(unable to open session)');
        Halt(2);
      end;

    writeln('(session open)');
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
      BlockRead(F, DataBuf^.Txt, PacketSize, Len);
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

            {$IFDEF Ver80}
            StrPCopy(Path, ExtractFilePath(Param));
            StrPCopy(Name, ExtractFileName(Param));
            StrPCopy(Ext, ExtractFileExt(Param));
            {$ELSE}
            W := FileSplit(Param, Path, Name, Ext);
            {$ENDIF}
            FindFirst(Param, faArchive, SRec);
            {$IFNDEF Ver80}
            while (DosError = 0) do
              begin
            {$ENDIF}
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
              {$IFNDEF Ver80}
              end;
              {$ENDIF}
          end;
      end;

    {tell the receiver transfer is done}
    SendInfoMessage(TxtJobEnd);
  end;
{$ELSE}
  var
    i, j : integer;
    FullPath : string[80];
    Path     : DirStr;
    Name     : NameStr;
    Ext      : ExtStr;
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
      else TType := DataBuf^.TxtType;

      case State of
        WaitingForName :
          case TType of
            TxtJobEnd :
              State := AllDone;
            TxtFileInfo :
              begin
                {copy the file info into local variables}
                with DataBuf^ do
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
                BlockWrite(F, DataBuf^.Txt, DataBuf^.TxtLength, Len);
                if (Len <> DataBuf^.TxtLength) then
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

    until (State = AllDone);
  end;

begin
  {perform startup checking}
  writeln('NBSend  Copyright (c) 1993 by TurboPower Software.  Version 5.55');
  if not NetBiosInstalled then
    begin
      writeln('A NetBIOS driver must be installed');
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
