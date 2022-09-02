{*********************************************************}
(*                B-Tree Filer Ver 5.55                  *)
{*     Copyright (c) TurboPower Software Co 1986-99      *}
{*                 All rights reserved.                  *}
(*      Based in part on code written by Ralf Nagel      *)
{*********************************************************}

{--Conditional defines and compiler options}
  {$I BTDefine.Inc}
  {$F-,V-,B-,S-,I-,R-,X+}
  {$IFDEF CanSetOvrflowCheck}
    {$Q-}
  {$ENDIF}

program MesExamp;

{
  This program is meant as an example of the various different messaging and
  packet exchange facilities available in the NWIPXSPX and NetBIOS units.

  The sending side runs this program with no command line parameters.  The
  receiving side must specify a parameter (it can be anything) on the command
  line.

  Note how an ExitProc is set up to "undo" the changes to the environment
  made by this program in the event of fatal runtime error or user abort.

  This program will check to see if Novell's NetWare and/or a NetBIOS
  compatible operating system are present. The program will then use each of
  the message services available.

  This program also contains an IPX Event Service Routine. Event Service
  Routines are in essence hardware interrupt service routines that get
  control immediately when an IPX event occurs. The IPX routines in the
  NWIPXSPX unit allow a Pascal ESR to be specified for use by the
  IPX routines. The procedure OurPascalESR is such a routine.

  Because of the changes in B-Tree Filer 5.42, this program has largely
  been rewritten.
}

uses
  {$IFDEF DPMIorWnd}
  DPMI,
  {$ENDIF}
  {$IFDEF Windows}
  WinCrt,
   {$IFDEF Ver80}
  DosSupp,
   {$ELSE}
  WinDOS,
   {$ENDIF}
  {$ELSE}
  Crt,
  DOS,
  {$ENDIF}
  NWBase, NWConn, {Services for Novell's NetWare}
  NWIPXSPX,{Services for IPX/SPX}
  NetBIOS; {Services for NetBIOS}

type
  StateMachine = (Preliminary,
                  InNetBIOS, InNetBIOSSession,
                  InIPX, InSPX, ClosingDown);

const
  {NetBIOS constants}
  SenderName   : NBNameStr = 'MESEXAMPSend';
  ReceiverName : NBNameStr = 'MESEXAMPRcv';
  BroadcastDatagrams : Boolean = TRUE;

  {IPX/SPX constants}
  SPXSendSocket = $7001;   {the socket for the SPX sender}
  SPXRecvSocket = $7002;   {the socket for the SPX receiver}
  IPXSocket = $7005;       {the socket for the IPX exchange}

var
  State : StateMachine;       {the program's current state}

  InReceiveMode : boolean;    {true if this partner is receiving}
  LoggedIntoNetware : boolean;{true if we are logged into a NetWare server}
  NetBIOSIsPresent  : boolean;{true if a NetBIOS driver was found}
  IPXIsPresent      : boolean;{true if an IPX driver was found}
  SPXIsPresent      : boolean;{true if an SPX driver was found}

  DefServer : TnwServer;      {the default NetWare server}
  PartnerConn : word;         {our partner's connection ID}

  SaveExitProc : pointer;     {saved ExitProc value}

  {NetBIOS global variables}
  SessionNumber : byte;       {the NetBIOS session number}

  {IPX/SPX global variables}
  ESRCalled : boolean;        {true if the IPX event service routine was called}
  Partner : IPXAddress;       {our partner's IPX address for IPX/SPX test}
  IPXEvent : PipxEventRec;    {our IPX event record}
  SPXEvent : PspxEventRec;    {our SPX event record}


{=== HELPER ROUTINES =================================================}
procedure OurExitProc; far;
  {-Exit procedure, closes down incompletely terminated sessions}
  var
    Result : byte;
  begin
    ExitProc := SaveExitProc;
    case State of
      InNetBIOS,
      InNetBIOSSession :
        begin
          if (State = InNetBIOSSession) then
            NetBiosHangup(SessionNumber);
          if InReceiveMode then
               NetBiosDeleteName(ReceiverName)
          else NetBiosDeleteName(SenderName);
          NetBiosResetAdapter(0, 0);
        end;
      InIPX :
        begin
          if (IPXEvent <> nil) then
            begin
              Result := IPXCancelEvent(IPXEvent);
              IPXFreeEventRec(IPXEvent);
              IPXEvent := nil;
            end;
          IPXCloseSocket(IPXSocket);
        end;
      InSPX :
        begin
          if (SPXEvent <> nil) then
            begin
              SPXAbortConn(SPXEvent);
              SPXFreeEventRec(SPXEvent);
              SPXEvent := nil;
            end;
          if InReceiveMode then
               IPXCloseSocket(SPXRecvSocket)
          else IPXCloseSocket(SPXSendSocket);
        end;
      Preliminary :
        begin
          if (IPXEvent <> nil) then
            IPXFreeEventRec(IPXEvent);
          if (SPXEvent <> nil) then
            SPXFreeEventRec(SPXEvent);
        end;
    end;
  end;

function GetPartner : word;
  {-Prompts the user for a partner name, returns his connection number}
  var
    OurConnNo : word;
    C         : word;
    Status    : word;
    User      : string[49];
  begin
    OurConnNo := nwGetConnNo(DefServer);
    if InReceiveMode then
         write('Enter user to receive from: ')
    else write('Enter user to send to: ');
    readln(User);
    C := 0;
    Status := nwGetConnNoForUser(DefServer, User, C);
    if (C = OurConnNo) then
      Status := nwGetConnNoForUser(DefServer, User, C);
    GetPartner := C;
  end;

procedure Pause(TestType : string);
  {-Pauses for the user to type a key}
  var
    Ch : char;
  begin
    write('Press any key to start ', TestType, ' tests...');
    Ch := ReadKey;
    if (Ch = #0) then
      Ch := ReadKey;
    writeln;
  end;

procedure UpdateSpinner(var Spinner : word);
  {-A simple 'I am doing something' visual spinner}
  const
    SpinnerChar : string[8] = ' .o*O*o.';
    OldT : word = 0;
  var
    H, M, S, T : word;
  begin
    GetTime(H, M, S, T);
    T := T div 25;
    if (T <> OldT) then
      begin
        OldT := T;
        Spinner := Succ(Spinner) and $7;
        write(^H, SpinnerChar[Spinner+1]);
      end;
  end;

function UserHasAborted : boolean;
  {-Checks for the user pressing Esc to terminate a process}
  begin
    UserHasAborted := false;
    if KeyPressed then
      UserHasAborted := ReadKey = #27;
  end;


{=== NETBIOS MESSAGE ROUTINES ========================================}
procedure DemoSendNetBIOS;
  {-Send a datagram, broadcast datagram, and session packet}
  var
    Msg : ^string;
    OurNCB : PNCB;
  {------}
  procedure FreeMemory;
    begin
      if (Msg <> nil) then
        NetBiosFreePacket(Msg);
      if (OurNCB <> nil) then
        NetBiosFreeNCB(OurNCB);
    end;
  {------}
  const
    DataGramMsg  = 'DataGrams are fun!';
    BroadcastMsg = 'Broadcast DataGrams are fun too!';
    SessionMsg   = 'Session packets are versatile';
  var
    Spinner : word;
    Result  : byte;
    NameNum : byte;
  begin
    writeln('---NETBIOS SEND TEST---');

    {allocate our buffers before we start}
    Msg := nil;
    OurNCB := nil;
    Msg := NetBiosAllocPacket(sizeof(string));
    if (Msg = nil) then
      begin
        writeln('**Error: Out of memory allocating Msg buffer.');
        Halt(1);
      end;
    OurNCB := NetBiosAllocNCB;
    if (OurNCB = nil) then
      begin
        FreeMemory;
        writeln('**Error: Out of memory allocating NCB.');
        Halt(1);
      end;

    {add the our NetBIOS name}
    write('Adding NetBIOS name... ');
    Result := NetBiosAddName(SenderName, NameNum);
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln('(error: ', Result, ')');
        Halt(1);
      end;
    writeln('(ok)');
    State := InNetBIOS;

    {pause until the user is ready}
    Pause('NetBIOS');

    {send a datagram}
    Msg^ := DataGramMsg;
    write('Sending datagram [', Msg^, ']  ');
    NetBiosSendDGNoWait(OurNCB, nil, NameNum, ReceiverName,
                        Succ(length(Msg^)), Msg);
    {wait for the command to complete}
    Spinner := 0;
    while not NetBiosCmdCompleted(OurNCB, Result) do
      begin
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            FreeMemory;
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    {check the result of the netBIOS command}
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');

    {if allowed, send a broadcast datagram}
    if BroadcastDataGrams then
      begin
        Msg^ := BroadcastMsg;
        write('Broadcasting datagram [', Msg^, ']  ');
        NetBiosSendBDGNoWait(OurNCB, nil, NameNum, Succ(length(Msg^)), Msg);
        {wait for the command to complete}
        Spinner := 0;
        while not NetBiosCmdCompleted(OurNCB, Result) do
          begin
            UpdateSpinner(Spinner);
            if UserHasAborted then
              begin
                FreeMemory;
                writeln(^H'(user abort)');
                Halt(1);
              end;
          end;
        {check the result of the netBIOS command}
        if (Result <> 0) then
          begin
            FreeMemory;
            writeln(^H'(error: ', Result, ')');
            Halt(1);
          end;
        writeln(^H'(ok)');
      end;

    {now test the session stuff}

    {open a new session}
    write('Setting up a NetBIOS session  ');
    NetBiosOpenNoWait(OurNCB, nil, ReceiverName, SenderName, 5, 5);
    Spinner := 0;
    repeat
      while not NetBiosCmdCompleted(OurNCB, Result) do
        begin
          UpdateSpinner(Spinner);
          if UserHasAborted then
            begin
              FreeMemory;
              writeln(^H'(user abort)');
              Halt(1);
            end;
        end;
      if (Result <> 0) then
        NetBiosOpenNoWait(OurNCB, nil, ReceiverName, SenderName, 5, 5);
    until (Result = 0);
    writeln(^H'(ok)');
    SessionNumber := OurNCB^.LSN;
    State := InNetBIOSSession;

    {send a message via the session services}
    Msg^ := SessionMsg;
    write('Sending session packet [', Msg^, ']  ');
    NetBiosSendNoWait(OurNCB, nil, SessionNumber, Succ(length(Msg^)), Msg);
    {wait for the command to complete}
    Spinner := 0;
    while not NetBiosCmdCompleted(OurNCB, Result) do
      begin
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            FreeMemory;
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    {check the result of the netBIOS command}
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');

    {close the session by hanging up}
    NetBiosHangup(SessionNumber);

    {clean up - free memory and delete the name}
    FreeMemory;
    NetBiosDeleteName(SenderName);
    State := Preliminary;
    writeln('All done');
  end;
{--------}
procedure DemoRecvNetBIOS;
  {-Receive a datagram, broadcast datagram, and session packet}
  var
    Msg : ^string;
    OurNCB : PNCB;
  {------}
  procedure FreeMemory;
    begin
      if (Msg <> nil) then
        NetBiosFreePacket(Msg);
      if (OurNCB <> nil) then
        NetBiosFreeNCB(OurNCB);
    end;
  {------}
  var
    Spinner : word;
    Result  : byte;
    NameNum : byte;
  begin
    writeln('---NETBIOS RECEIVE TEST---');

    {allocate our buffers before we start}
    Msg := nil;
    OurNCB := nil;
    Msg := NetBiosAllocPacket(sizeof(string));
    if (Msg = nil) then
      begin
        writeln('**Error: Out of memory allocating Msg buffer.');
        Halt(1);
      end;
    OurNCB := NetBiosAllocNCB;
    if (OurNCB = nil) then
      begin
        FreeMemory;
        writeln('**Error: Out of memory allocating NCB.');
        Halt(1);
      end;

    {add the our NetBIOS name}
    write('Adding NetBIOS name... ');
    Result := NetBiosAddName(ReceiverName, NameNum);
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln('(error: ', Result, ')');
        Halt(1);
      end;
    writeln('(ok)');
    State := InNetBIOS;

    {pause until the user is ready}
    Pause('NetBIOS');

    {receive a datagram}
    write('Trying to receive a datagram  ');
    NetBiosReceiveDGNoWait(OurNCB, nil, NameNum, sizeof(string), Msg);
    {wait for the command to complete}
    Spinner := 0;
    while not NetBiosCmdCompleted(OurNCB, Result) do
      begin
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            FreeMemory;
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    {check the result of the netBIOS command}
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');
    writeln('..message is [', Msg^, ']');

    {if allowed, receive a broadcast datagram}
    if BroadcastDataGrams then
      begin
        write('Trying to receive a broadcast datagram  ');
        NetBiosReceiveBDGNoWait(OurNCB, nil, NameNum, sizeof(string), Msg);
        {wait for the command to complete}
        Spinner := 0;
        while not NetBiosCmdCompleted(OurNCB, Result) do
          begin
            UpdateSpinner(Spinner);
            if UserHasAborted then
              begin
                FreeMemory;
                writeln(^H'(user abort)');
                Halt(1);
              end;
          end;
        {check the result of the netBIOS command}
        if (Result <> 0) then
          begin
            FreeMemory;
            writeln(^H'(error: ', Result, ')');
            Halt(1);
          end;
        writeln(^H'(ok)');
        writeln('..message is [', Msg^, ']');
      end;

    {now test the session stuff}

    {Listen for a new session being opened}
    write('Listening for a NetBIOS session  ');
    NetBiosListenNoWait(OurNCB, nil, SenderName, ReceiverName, 5, 5);
    Spinner := 0;
    repeat
      while not NetBiosCmdCompleted(OurNCB, Result) do
        begin
          UpdateSpinner(Spinner);
          if UserHasAborted then
            begin
              FreeMemory;
              writeln(^H'(user abort)');
              Halt(1);
            end;
        end;
      if (Result <> 0) then
        NetBiosListenNoWait(OurNCB, nil, SenderName, ReceiverName, 5, 5);
    until (Result = 0);
    writeln(^H'(ok)');
    SessionNumber := OurNCB^.LSN;
    State := InNetBIOSSession;

    {receive a message via the session services}
    write('Trying to receive a session packet  ');
    NetBiosReceiveNoWait(OurNCB, nil, SessionNumber, sizeof(string), Msg);
    {wait for the command to complete}
    Spinner := 0;
    while not NetBiosCmdCompleted(OurNCB, Result) do
      begin
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            FreeMemory;
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    {check the result of the netBIOS command}
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');
    writeln('..message is [', Msg^, ']');

    {close the session by hanging up}
    NetBiosHangup(SessionNumber);

    {clean up - free memory and delete the name}
    FreeMemory;
    NetBiosDeleteName(ReceiverName);
    State := Preliminary;
    writeln('All done');
  end;


{=== IPX MESSAGE ROUTINES ============================================}
procedure OurIPXESR(FromAES : boolean; Event : PipxEventRec); far;
  {-An example IPX event service routine}
  begin
    {all we do is set a flag to say we've been called}
    ESRCalled := true;
  end;
{--------}
procedure DemoSendIPX;
  var
    Msg : ^string;
  {------}
  procedure FreeMemory;
    begin
      if (Msg <> nil) then
        IPXFreePacket(Msg);
    end;
  {------}
  const
    IPXMsg = 'IPX packet transmissions are f-a-s-t.';
  var
    Result : byte;
    Spinner : word;
  begin
    writeln('---IPX SEND TEST---');

    {allocate our buffers before we start}
    Msg := nil;
    IPXEvent := nil;
    Msg := IPXAllocPacket(sizeof(string));
    if (Msg = nil) then
      begin
        writeln('**Error: Out of memory allocating Msg buffer.');
        Halt(1);
      end;
    IPXEvent := IPXAllocEventRec(OurIPXESR);
    if (IPXEvent = nil) then
      begin
        FreeMemory;
        writeln('**Error: Out of memory allocating IPX Event record.');
        Halt(1);
      end;

    {get our partner's IPX address}
    nwGetInternetAddress(DefServer, PartnerConn, Partner);

    {pause until the user is ready}
    Pause('IPX');

    State := InIPX;

    {send a packet using IPX}
    Msg^ := IPXMsg;
    write('Sending IPX packet [', Msg^, ']  ');
    ESRCalled := false;
    IPXSend(IPXEvent, Partner, IPXSocket, false, succ(length(Msg^)), Msg);
    Spinner := 0;
    while not ESRCalled do
      begin
        IPXRelinquish;
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            FreeMemory;
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    if not IPXEventComplete(IPXEvent, Result) then
      {nothing - impossible to get here};
    {check the result of the IPX event}
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');

    {clean up - free memory and close the socket}
    IPXCloseSocket(IPXSocket);
    FreeMemory;
    IPXFreeEventRec(IPXEvent);
    IPXEvent := nil;
    State := Preliminary;
    writeln('All done');
  end;
{--------}
procedure DemoRecvIPX;
  var
    Msg : ^string;
  {------}
  procedure FreeMemory;
    begin
      if (Msg <> nil) then
        IPXFreePacket(Msg);
    end;
  {------}
  var
    Result : byte;
    Spinner : word;
  begin
    writeln('---IPX RECEIVE TEST---');

    {allocate our buffers before we start}
    Msg := nil;
    IPXEvent := nil;
    Msg := IPXAllocPacket(sizeof(string));
    if (Msg = nil) then
      begin
        writeln('**Error: Out of memory allocating Msg buffer.');
        Halt(1);
      end;
    IPXEvent := IPXAllocEventRec(OurIPXESR);
    if (IPXEvent = nil) then
      begin
        FreeMemory;
        writeln('**Error: Out of memory allocating IPX Event record.');
        Halt(1);
      end;

    {get our partner's IPX address}
    nwGetInternetAddress(DefServer, PartnerConn, Partner);

    {pause until the user is ready}
    Pause('IPX');

    State := InIPX;

    {receive a packet using IPX}
    write('Trying to receive IPX packet  ');
    ESRCalled := false;
    IPXListen(IPXEvent, IPXSocket, false, sizeof(string), Msg);
    Spinner := 0;
    while not ESRCalled do
      begin
        IPXRelinquish;
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            FreeMemory;
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    if not IPXEventComplete(IPXEvent, Result) then
      {nothing - impossible to get here};
    {check the result of the IPX event}
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');
    writeln('..message is [', Msg^, ']');

    {clean up - free memory and close the socket}
    IPXCloseSocket(IPXSocket);
    FreeMemory;
    IPXFreeEventRec(IPXEvent);
    IPXEvent := nil;
    State := Preliminary;
    writeln('All done');
  end;


{=== SPX MESSAGE ROUTINES ============================================}
procedure DemoSendSPX;
  var
    Msg : ^string;
  {------}
  procedure FreeMemory;
    begin
      if (Msg <> nil) then
        IPXFreePacket(Msg);
    end;
  {------}
  const
    SPXMsg = 'SPX packet transmissions are safe.';
  var
    Result : byte;
    Spinner : word;
  begin
    writeln('---SPX SEND TEST---');

    {allocate our buffers before we start}
    Msg := nil;
    SPXEvent := nil;
    Msg := IPXAllocPacket(sizeof(string));
    if (Msg = nil) then
      begin
        writeln('**Error: Out of memory allocating Msg buffer.');
        Halt(1);
      end;
    if InReceiveMode then
         SPXEvent := SPXAllocEventRec(10, sizeof(string))
    else SPXEvent := SPXAllocEventRec(2, sizeof(string));
    if (SPXEvent = nil) then
      begin
        FreeMemory;
        writeln('**Error: Out of memory allocating SPX Event record.');
        Halt(1);
      end;

    {pause until the user is ready}
    Pause('SPX');

    State := InSPX;

    {establish a connection}
    write('Trying to establish a connection  ');
    SPXEstablishConn(SPXEvent, Partner, SPXSendSocket, SPXRecvSocket, false);
    Spinner := 0;
    while not SPXEventComplete(SPXEvent, Result) do
      begin
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            FreeMemory;
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    {check the result of the SPX event}
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');

    {send a packet using SPX}
    Msg^ := SPXMsg;
    write('Sending SPX packet [', Msg^, ']  ');
    SPXSend(SPXEvent, false, succ(length(Msg^)), Msg);
    Spinner := 0;
    while not SPXEventComplete(SPXEvent, Result) do
      begin
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            FreeMemory;
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    {check the result of the SPX event}
    if (Result <> 0) then
      begin
        FreeMemory;
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');

    {clean up - free memory and close the socket}
    SPXTerminateConn(SPXEvent);
    IPXCloseSocket(SPXSendSocket);
    FreeMemory;
    SPXFreeEventRec(SPXEvent);
    SPXEvent := nil;
    State := Preliminary;
    writeln('All done');
  end;
{--------}
procedure DemoRecvSPX;
  var
    Msg : ^string;
    Result : byte;
    Index, DataType : byte;
    Spinner : word;
  begin
    writeln('---SPX RECEIVE TEST---');

    {allocate our buffers before we start}
    SPXEvent := nil;
    if InReceiveMode then
         SPXEvent := SPXAllocEventRec(10, sizeof(string))
    else SPXEvent := SPXAllocEventRec(2, sizeof(string));
    if (SPXEvent = nil) then
      begin
        writeln('**Error: Out of memory allocating SPX Event record.');
        Halt(1);
      end;

    {pause until the user is ready}
    Pause('SPX');

    State := InSPX;

    {establish a connection}
    write('Listening to get the establish connection request  ');
    SPXListenForConn(SPXEvent, SPXRecvSocket, false);
    Spinner := 0;
    while not SPXEventComplete(SPXEvent, Result) do
      begin
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    {check the result of the SPX event}
    if (Result <> 0) then
      begin
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');

    {receive a packet using SPX}
    write('Trying to receive an SPX packet  ');
    Spinner := 0;
    while not SPXPacketReceived(SPXEvent, Index, DataType, Result, pointer(Msg)) do
      begin
        UpdateSpinner(Spinner);
        if UserHasAborted then
          begin
            writeln(^H'(user abort)');
            Halt(1);
          end;
      end;
    {check the result of the SPX event}
    if (Result <> 0) then
      begin
        writeln(^H'(error: ', Result, ')');
        Halt(1);
      end;
    writeln(^H'(ok)');
    writeln('..message is [', Msg^, ']');

    {clean up - free memory and close the socket}
    SPXTerminateConn(SPXEvent);
    IPXCloseSocket(SPXSendSocket);
    SPXFreeEventRec(SPXEvent);
    SPXEvent := nil;
    State := Preliminary;
    writeln('All done');
  end;


{=== MAIN PROGRAM ====================================================}
var
  SPXVersion, MaxSPXConn, AvailSPXConn : word;

begin
  writeln('MesExamp - Network message example program, version 5.55');
  writeln('Copyright (c) TurboPower Software 1993');

  State := Preliminary;

  {see what network facilities we have...}
  LoggedIntoNetware := (nwShellType <> nsNone);
  NetBIOSIsPresent := NetBIOSInstalled;

  {if we have none, tell the user so and stop}
  if (not NetBIOSIsPresent) and (not LoggedIntoNetware) then
    begin
      writeln('***Error - No usable network found.');
      writeln('           (either you are not logged on, or NetBIOS is not present');
      Halt(1);
    end;

  {NetWare's NetBIOS does not do broadcast datagrams}
  if LoggedIntoNetware then
    BroadcastDatagrams := false;

  {are we the sender or receiver?}
  InReceiveMode := (ParamCount <> 0);

  {check for IPX and SPX (note that we will be using NetWare services
   to find the partner, so it must also be present)}
  IPXIsPresent := LoggedIntoNetWare and IPXServicesAvail;
  SPXIsPresent := IPXIsPresent and
                     SPXServicesAvail(SPXVersion, MaxSPXConn, AvailSPXConn);

  {Get our partner's connection number}
  if IPXIsPresent then
    begin
      DefServer := nwDefaultServer;
      PartnerConn := GetPartner;
      if (PartnerConn = 0) and IPXIsPresent then
        begin
          writeln('***Warning - Partner is not logged on.');
          writeln('             IPX and SPX tests will be ignored.');
          IPXIsPresent := false;
          SPXIsPresent := false;
        end;
    end;

  SaveExitProc := ExitProc;  {save original exitproc}
  ExitProc := @OurExitProc;  {setup our exit procedure}

  {send or receive, as appropriate}
  if InReceiveMode then
    begin
      if NetBIOSIsPresent then
        DemoRecvNetBIOS;
      if IPXIsPresent then
        DemoRecvIPX;
      if SPXIsPresent then
        DemoRecvSPX;
    end
  else {we are the sender}
    begin
      if NetBIOSIsPresent then
        DemoSendNetBIOS;
      if IPXIsPresent then
        DemoSendIPX;
      if SPXIsPresent then
        DemoSendSPX;
    end;

  State := ClosingDown;

  {NOTE: OurExitProc is called no matter how we terminate}
end.

