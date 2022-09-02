{$B-,S-,R-,V-,X+}

{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{$I BTDEFINE.INC}

program SPX2Way;

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
  NWBase,
  NWConn,
  NWIPXSPX;

const
  MaxPacketSize = 70;     {bytes sent per packet - limited by screen width}

{Structure of each message passed through SPX}
type
  DataPacketPtr = ^DataPacket;
  DataPacket = record
    TxtLength : word;                             {number of bytes in Txt}
    Txt       : array [1..MaxPacketSize] of char; {message buffer}
  end;

{IPX addressing}
const
  MasterSocket = $4567;    {socket for sender (the one who tries to establish conn)}
  SlaveSocket  = $4568;    {socket for receiver (the one who listens for establish conn)}
  HandshakeSocket = $4569; {the IPX handshake socket}
var
  Receiver : IPXAddress;   {address of receiving partner}

{Options}
const
  ConnectRetryTime : word = 60;      {seconds to retry when connecting}
  InReceiveMode : boolean = false;   {set true with /R option}
  SessionCreated : boolean = false;  {set true when session created}
var
  EstablishPosted : boolean;         {set true when SPX establish has been posted}

{Variables used for data transfer}
const
  IPXEvent : PipxEventRec = nil;      {used to find receiver}
  IPXBuf   : PPacket = nil;           { --ditto-- }
  DataBuf  : DataPacketPtr = nil;     {send buffer}
  Event    : PspxEventRec = nil;      {send or listen event}
var
  DataIndex : byte;                   {index of last packet received}
  DataBufL  : DataPacketPtr;          {pointer to last received packet}

{Special error codes}
const
  UserBreak   = 127;
  RemoteBreak = 126;

var
  ExitSave : pointer;

procedure  ShowHelp;
 {-Display help screen}
  begin
    writeln('Usage: SPX2WAY [/Cxxxx] [/R]');
    writeln('  /Cxxxx Set connect retry time to xxxx seconds (default ',
                                                       ConnectRetryTime, ')');
    writeln('  /R     Enable receive mode');
    writeln('  /?     Display this help screen');
    Halt(0);
  end;

procedure InvalidParam(Param : string);
  {-Report an invalid parameter}
  begin
    writeln(Param, ' is an invalid parameter');
    ShowHelp;
    Halt(1);
  end;

procedure EvaluateOptions;
  {-Set global options based on command line}
  var
    i, ec : integer;
    len   : integer;
    param : string[128];
  begin
    for i := 1 to ParamCount do
      begin
        Param := ParamStr(i);
        if ((Param[1] = '-') or (Param[1] = '/')) then
          begin
            len := length(Param);
            if (len = 1) then
              InvalidParam(Param)
            else
              case Param[2] of
                'c', 'C' :
                  begin
                    if (len < 3) then
                      InvalidParam(Param);
                    Val(Copy(Param, 3, 255), ConnectRetryTime, ec);
                    if (ConnectRetryTime > 9999) then
                      ConnectRetryTime := 9999;
                  end;
                'r', 'R' :
                  begin
                    if (len <> 2) then
                       InvalidParam(Param);
                    InReceiveMode := true;
                  end;
                '?' :
                  ShowHelp;
              else
                InvalidParam(param);
              end;{case}
          end
        else
          InvalidParam(Param);
      end;
  end;

procedure AllocateBuffers;
  {-Allocate packet and event buffers}
  begin
    IPXEvent := IPXAllocEventRec(IPXDoNothingESR);
    IPXBuf := IPXAllocPacket(4);
    Event := SPXAllocEventRec(8, sizeof(DataPacket));
    DataBuf := IPXAllocPacket(sizeof(DataPacket));
    if ((IPXEvent = nil) or (IPXBuf = nil) or
        (Event = nil) or (DataBuf = nil)) then
      begin
        writeln('Unable to allocate packet buffers');
        Halt(1);
      end;
  end;

function UserHasAborted : boolean;
  {-Return non-zero if user pressed <Esc>}
  var
    Ch : char;
  begin
    UserHasAborted := false;
    if KeyPressed then
      begin
        Ch := ReadKey;
        if (Ch = #0) then
          Ch := ReadKey
        else if (Ch = #27) then
          UserHasAborted := true;
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
  {-Wait for event complete or user break}
  var
    Status : byte;
  begin
    while (not SPXEventComplete(SPXEvent, Status)) do
      if UserHasAborted then
        begin
          WaitForEventComplete := UserBreak;
          Exit;
        end;
    WaitForEventComplete := Status;
  end;

procedure MyExitProc; far;
  {-Clean up when exiting}
  var
    Status : byte;
  begin
    ExitProc := ExitSave;
    {shut down the IPX handshaking event}
    IPXCloseSocket(HandshakeSocket);
    if (IPXEvent <> nil) then
      IPXFreeEventRec(IPXEvent);
    if (IPXBuf <> nil) then
      IPXFreePacket(IPXBuf);

    {shut down the SPX transfer}
    if (Event <> nil) then
      begin
        if SessionCreated then
          SPXTerminateConn(Event)
        else
          if (InReceiveMode) then
            SPXCancelListenForConn(Event)
          else
            if EstablishPosted then
              SPXAbortConn(Event);
        SPXFreeEventRec(Event);
      end;
    if InReceiveMode then
         IPXCloseSocket(SlaveSocket)
    else IPXCloseSocket(MasterSocket);
  end;

procedure ExitBadMessage(Status : byte; Direction: string);
  {-Report error and exit}
  begin
    case Status of
      UserBreak :
        writeln('(user break)');
      RemoteBreak,
      $EC,
      $ED,
      $EE :
        writeln('(remote break)');
    else
      writeln('(', Direction, ' error ', Status, ')');
    end;
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
  end;

function GetTimeInSecs : longint;
  {-return the current time in seconds}
  var
    H, M, S, T : word;
  begin
    GetTime(H, M, S, T);
    GetTimeInSecs := (((longint(H) * 60) + longint(M)) * 60) + longint(S);
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
    NetNo           : word;
    StartTime       : longint;
    EndTime         : longint;
    PrevTime        : longint;
    CurTime         : longint;
    Self            : IPXAddress;
    NetList         : TnwNetworkList;
    ConnStatus      : TspxStatus;
  begin
    if (InReceiveMode) then
      begin
        writeln('Finding sender ');

        {get list of networks accessible to listener}
        nwGetNetworkList(nwDefaultServer, NetList);

        {prepare Receiver address for broadcast}
        FillChar(Receiver, sizeof(Receiver), 0);
        Receiver.Node := IPXAllNodes;

        {post listen for sender's SPX connection}
        Status := SPXListenForConn(Event, SlaveSocket, false);
      end
    else
      begin
        writeln('Finding receiver ');

        {post listen for receiver's IPX broadcast}
        Status := IPXListen(IPXEvent, HandshakeSocket, false, 4, IPXBuf);
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
              {send IPX broadcast to each accessible network}
              for NetNo := 1 to NetList.Count do
                begin
                  Receiver.Network := NetList.List[NetNo];
                  IPXSend(IPXEvent, Receiver, HandshakeSocket, true, 4, IPXBuf);
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
                    Status := SPXEstablishConn(Event, Receiver, MasterSocket,
                                               SlaveSocket, false);
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
          begin
            write(', same station)');
            writeln('This version of SPX2WAY does not support communications');
            writeln('between two windows in Windows.');
            Halt(2);
          end;
      end;

    writeln(')');
    writeln('ECBs listening: ', SPXECBsListening(Event));

    spxGetConnStatus(Event, ConnStatus);
    writeln('Our partner is on: ', nwIPXAddressStr(ConnStatus.Destination));
  end;

procedure PrintBuf(DataP : DataPacketPtr);
  {-Write message buffer to screen}
  var
    i : word;
  begin
    for i := 1 to DataP^.TxtLength do
      write(DataP^.Txt[i]);
  end;

procedure SendAndReceive;
  {-Send or receive messages}
  var
    Status   : byte;
    DataType : byte;
    Ch       : char;
  begin
    writeln('Type <Esc> to quit, <Enter> to send message');
    DataBuf^.TxtLength := 0;
    Status := 0;
    while (Status = 0) do
      begin
        if KeyPressed then
          begin
            Ch := ReadKey;
            case Ch of
              ^H : begin
                     write(^H' '^H);
                     if (DataBuf^.TxtLength <> 0) then
                       dec(DataBuf^.TxtLength);
                   end;
              ^M : begin
                     write(^M^J);
                     SendNetMessage;
                     write('Sent: ');
                     PrintBuf(DataBuf);
                     writeln;
                     DataBuf^.TxtLength := 0;
                   end;
              #27: begin
                     Status := UserBreak;
                   end;
            else {characters for message}
              if (DataBuf^.TxtLength < MaxPacketSize) then
                begin
                  write(ch);
                  inc(DataBuf^.TxtLength);
                  DataBuf^.Txt[DataBuf^.TxtLength] := ch;
                end;
            end;{case}
          end
        else
          if SPXPacketReceived(Event, DataIndex, DataType,
                               Status, pointer(DataBufL)) then
            if (Status = 0) then {successful message}
              begin
                if (DataBuf^.TxtLength <> 0) then
                  {partial send message typed in}
                  writeln;
                write('Received (', DataType, '): ');
                PrintBuf(DataBufL);
                writeln;
                if (DataBuf^.TxtLength <> 0) then
                  PrintBuf(DataBuf);
                SPXReactivateECB(Event, DataIndex);
              end
            else if (Status = SPXErrRemoteHasGone) then
              begin
                writeln('Partner has gone.');
                Halt;
              end
            else if (Status = SPXErrFailedConn) then
              begin
                writeln('Watchdog says partner has gone.');
                Halt;
              end;
      end;
    if (DataBuf^.TxtLength <> 0) then
      writeln;
    ExitBadMessage(Status, 'transmit');
  end;

var
  SPXVersion   : word;            {returned by SPXServicesAvail}
  MaxSPXConn   : word;
  AvailSPXConn : word;
  Self         : IPXAddress;
begin
  {perform startup checking}
  writeln('SPX2WAY Copyright (c) 1993,1994 by TurboPower Software.  Version 5.55');
  if (not SPXServicesAvail(SPXVersion, MaxSPXConn, AvailSPXConn)) then
    begin
      writeln('An SPX driver must be installed');
      Halt(1);
    end;

  writeln('SPX version is: ', Hi(SPXVersion), '.', Lo(SPXVersion));
  IPXInternetAddress(Self);
  writeln('Our IPX address is: ', nwIPXAddressStr(Self));

  {evaluate command line options}
  EvaluateOptions;

  {install exit function}
  ExitSave := ExitProc;
  ExitProc := @MyExitProc;

  {allocate the packet and event buffers}
  SPXWatchdog := false{true};
  AllocateBuffers;

  {open the session}
  OpenSession;

  {send and receive messages}
  SendAndReceive;

  {the exit function will clean up}
end.
