{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Nathan Anderson
Description:  TTimeServ is a time protocol server using TWSocketon.
              Conforms to both UDP and TCP portions of RFC-868
EMail:        nathan@dreamscape.com
Creation:     February 6, 1999.
Version:      1.00
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1999 Nathan Anderson

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

Updates:
7 March 2001, Angus - replaced htonl with ByteSwaps since it went to hyper space
                      using UTC time to avoid messing with timezones
                   UDP server does not seem to respond....

26 July 2002, Angus - added SNTP time server, fairly minimal
                      trigger when UTP completes OK
                      added public SrcIPAddr to make available during end trigger

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TimeServ;

interface

uses
    Classes, WSocket, WinSock, WSTimeClient ;

const
  TIME_ZONE_ID_UNKNOWN  = 0;
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;

type
    TTimeProtocol = (tpUDP, tpTCP, tpSNTP);
    TProtocolSet = set of TTimeProtocol;

    TQuery = procedure (Sender: TObject; SocketAddr : TSockAddrIn;
                        TimeProtocol : TTimeProtocol;
                        var Continue : Boolean) of object;
    TQueryDone = procedure (Sender: TObject; Error : Word) of object;

    TTimeServ = class(TComponent)
    public
        SrcIPAddr  : TSockAddrIn ;  // angus
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Start;
        procedure   Stop;
    protected
        FWUDPSocket         : TWSocket;
        FWTCPSocket         : TWSocket;
        FWSNTPSocket        : TWSocket;
        FClientSocket       : TWSocket;
        FOnSessionConnected : TSessionConnected;
        FOnSessionClosed    : TSessionClosed;
        FOnQuery            : TQuery;
        FOnQueryDone        : TQueryDone;
        FTimeProtocol       : TProtocolSet;
        FRunning            : Boolean;
        FOnStart            : TNotifyEvent;
        FOnStop             : TNotifyEvent;
        procedure WUDPSocketSessionConnected(Sender: TObject; Error: Word);
        procedure WUDPSocketDataAvailable(Sender: TObject; Error: Word);
        procedure WUDPSocketSessionClosed(Sender: TObject; Error: Word);
        procedure WSNTPSocketSessionConnected(Sender: TObject; Error: Word);
        procedure WSNTPSocketDataAvailable(Sender: TObject; Error: Word);
        procedure WSNTPSocketSessionClosed(Sender: TObject; Error: Word);
        procedure ClientOnDataSent(Sender: TObject; Error: word);
        procedure TriggerQueryDone(Error: Word);
        procedure TriggerServerStart;
        procedure TriggerServerStop;
        procedure ChangeState(Sender: TObject;
                              OldState, NewState : TSocketState);
        procedure WTCPSocketSessionAvailable(Sender: TObject; Error: Word);
        procedure CreateUDPSocket;
        procedure CreateSNTPSocket;
        procedure CreateTCPSocket;
        procedure StartTCPServer;
        procedure StartUDPServer;
        procedure StartSNTPServer;
        procedure SetTimeProtocol(NewProtocolSet : TProtocolSet);
    published
        property TimeProtocol : TProtocolSet            read FTimeProtocol
                                                        write SetTimeProtocol;
        property OnStop : TNotifyEvent                  read FOnStop
                                                        write FOnStop;
        property OnStart : TNotifyEvent                 read FOnStart
                                                        write FOnStart;
        property OnSessionConnected : TSessionConnected read  FOnSessionConnected
                                                        write FOnSessionConnected;
        property OnSessionClosed : TSessionClosed       read  FOnSessionClosed
                                                        write FOnSessionClosed;
        property OnQueryDone : TQueryDone               read  FOnQueryDone
                                                        write FOnQueryDone;
        property OnQuery : TQuery                       read  FOnQuery
                                                        write FOnQuery;
        property IsRunning : Boolean                    read FRunning
                                                        write FRunning;
    end;

procedure Register;

implementation

uses SysUtils, WinTypes, Windows;
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TTimeServ]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TTimeServ.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FTimeProtocol := [tpUDP,tpTCP,tpSNTP];
    IsRunning := FALSE;
    CreateUDPSocket;
    CreateTCPSocket;
    CreateSNTPSocket;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TTimeServ.Destroy;
begin
    FWUDPSocket.Free;
    FWTCPSocket.Free;
    FClientSocket.Free;
    FWSNTPSocket.Free;
    FWUDPSocket := nil;
    FWTCPSocket := nil;
    FClientSocket := nil;
    FWSNTPSocket := nil;

    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.Start;
begin
   if FRunning then
      exit;      {If at least one server is running, then don't start again}

  CreateUDPSocket;
  CreateTCPSocket;
  CreateSNTPSocket;
  if tpUDP in FTimeProtocol then
  begin
     try
         StartUDPServer;
     except
          FWUDPSocket.Free;
          FWUDPSocket := nil;
     end ;
  end
  else
  begin
     FWUDPSocket.Close;
  end; {if..then..else}

  if tpTCP in FTimeProtocol then
  begin
    try
        StartTCPServer;
    except
      FWTCPSocket.Free;
      FClientSocket.Free;
      FWTCPSocket := nil;
      FClientSocket := nil;
    end ;
  end
  else
  begin
     FWTCPSocket.Close;
     FClientSocket.Close;
  end; {if..then..else}

  if tpSNTP in FTimeProtocol then
  begin
     try
         StartSNTPServer;
     except
       FWSNTPSocket.Free;
       FWSNTPSocket := nil;
     end ;
  end
  else
  begin
     FWSNTPSocket.Close;
  end; {if..then..else}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.Stop;
begin
   if FRunning then begin
      FWUDPSocket.Close;
      FWTCPSocket.Close;
      FClientSocket.Close;
      FWSNTPSocket.Close;
   end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.WUDPSocketSessionConnected(Sender: TObject; Error: Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);

    if Error <> 0 then begin
        TriggerQueryDone(Error);
        FWUDPSocket.Close;
     // FWTCPSocket.Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.WUDPSocketDataAvailable(Sender: TObject; Error: Word);
var
    Buffer : array [0..1023] of char;
    Src    : TSockAddrIn;
    SrcLen : Integer;
    NewTime   : LongWord;
    Continue : Boolean;
begin
    SrcLen := SizeOf(Src);
    FWUDPSocket.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);
    SrcIPAddr := Src ;
    Continue := TRUE;
    if Assigned(FOnQuery) then
    begin
       FOnQuery(Self, Src, tpUDP, Continue);
    end;

    if Continue then
    begin
        NewTime := Trunc ((GetUTCTime - 2) * 24 * 60 * 60) ;
        WSTimeClient.ByteSwaps (@NewTime, SizeOf(NewTime)) ;
        if FWUDPSocket.Sendto (Src, SrcLen, @NewTime, SizeOf(NewTime)) =
                    SOCKET_ERROR then TriggerQueryDone(FWUDPSocket.LastError);
        TriggerQueryDone(0);
    end
    else
    begin
        TriggerQueryDone($FFFF);
          {According to RFC 868, don't reply if we don't know what time it is}
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.WSNTPSocketSessionConnected(Sender: TObject; Error: Word);
begin
    if Assigned(FOnSessionConnected) then
        FOnSessionConnected(Self, Error);

    if Error <> 0 then begin
        TriggerQueryDone(Error);
        FWSNTPSocket.Close;
     // ??   FWTCPSocket.Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.WSNTPSocketDataAvailable(Sender: TObject; Error: Word);
var
    Buffer : array [0..1023] of char;
    Src    : TSockAddrIn;
    SrcLen : Integer;
    Continue : Boolean;
    SNTP   : SNTPheader ;
    ServVN: integer ;
    ServMode: integer ;
begin
    SrcLen := SizeOf(Src);
  	FillChar (Buffer, SizeOf (Buffer), #0);
    FWSNTPSocket.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);
    SrcIPAddr := Src ;
    Continue := FALSE;

// check we got an SNTP request
    Move (Buffer [0], SNTP, SizeOf (SNTP)) ;
    SNTP.RecvTimeStamp := GetUTCNtpTime ;

    ServVN := (SNTP.LIvnMmode and $38) shr 3 ;  // version (most seem to be 6)
    ServMode := SNTP.LIvnMmode and $07 ;        // 3=client, 4=server, 5=broadcast

// ensure that we found an SNTP server meeting RFC 2030
    if (ServMode = 3) or (ServVN <= 3) then Continue := TRUE;

    if Continue then
    begin
        if Assigned(FOnQuery) then
        begin
            FOnQuery(Self, Src, tpSNTP, Continue);
        end;
    end ;

    if Continue then
    begin
        SNTP.XmitTimeStamp := GetUTCNtpTime ;
        SNTP.LIvnMmode := $1C ;  // version 3, mode 4 server
        SNTP.Statum := 2 ; // secondary
        SNTP.Poll := 0 ;
        SNTP.Precision := -28 ;  // poor
        SNTP.RootDispersion := 0 ;
        SNTP.RootDelay := 0 ;
        SNTP.RefIdent := 0 ;
        if FWSNTPSocket.Sendto (Src, SrcLen, @SNTP, SizeOf(SNTP)) =
                    SOCKET_ERROR then TriggerQueryDone(FWSNTPSocket.LastError);
        TriggerQueryDone(0);
    end
    else
    begin
        TriggerQueryDone($FFFF);
          {According to RFC 868, don't reply if we don't know what time it is}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.TriggerQueryDone(Error: Word);
begin
    if Assigned(FOnQueryDone) then
       FOnQueryDone(Self, Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.WUDPSocketSessionClosed(Sender: TObject; Error: Word);
begin
    TriggerQueryDone(Error);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.WSNTPSocketSessionClosed(Sender: TObject; Error: Word);
begin
    TriggerQueryDone(Error);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.WTCPSocketSessionAvailable(Sender: TObject; Error: Word);
var
    NewHSocket : TSocket;
    NewTime   : longword;
    Src    : TSockAddrIn;
    Continue : Boolean;
begin
    NewHSocket := FWTCPSocket.Accept;
    FClientSocket.Dup(NewHSocket);
    FClientSocket.GetPeerName(Src, SizeOf(Src));
    SrcIPAddr := Src ;
    Continue := TRUE;
    if Assigned(FOnQuery) then
    begin
        FOnQuery(Self, Src, tpTCP, Continue);
    end;

    if Continue then
    begin
        NewTime := Trunc ((GetUTCTime - 2) * 24 * 60 * 60) ;
        WSTimeClient.ByteSwaps (@NewTime, SizeOf(NewTime)) ;
        if FClientSocket.Send (@NewTime, SizeOf(NewTime)) = SOCKET_ERROR then
                                 TriggerQueryDone(FClientSocket.LastError) ;
    end
    else
    begin
        TriggerQueryDone($FFFF);
        {According to RFC 868, don't reply if we don't know what time it is}
        FClientSocket.Shutdown(2);
        FClientSocket.Close;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.CreateTCPSocket;
begin
   if not Assigned(FWTCPSocket) then begin
      try
         FWTCPSocket                    := TWSocket.Create(Self);
         FClientSocket                  := TWSocket.Create(Self);
         FWTCPSocket.OnSessionConnected := WUDPSocketSessionConnected;
         FWTCPSocket.OnSessionAvailable := WTCPSocketSessionAvailable;
         FClientSocket.OnDataSent       := ClientOnDataSent;
         FWTCPSocket.OnChangeState      := ChangeState;
      except
         FWTCPSocket.Free;
         FClientSocket.Free;
         FWTCPSocket := nil;
         FClientSocket := nil;
      end;
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.CreateUDPSocket;
begin
   if not Assigned(FWUDPSocket) then begin
      try
         FWUDPSocket                    := TWSocket.Create(Self);
         FWUDPSocket.OnSessionConnected := WUDPSocketSessionConnected;
         FWUDPSocket.OnSessionClosed    := WUDPSocketSessionClosed;
         FWUDPSocket.OnDataAvailable    := WUDPSocketDataAvailable;
         FWUDPSocket.OnChangeState      := ChangeState;
      except
         FWUDPSocket.Free;
         FWUDPSocket := nil;
      end;
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.CreateSNTPSocket;
begin
   if not Assigned(FWSNTPSocket) then begin
      try
         FWSNTPSocket                    := TWSocket.Create(Self);
         FWSNTPSocket.OnSessionConnected := WSNTPSocketSessionConnected;
         FWSNTPSocket.OnSessionClosed    := WSNTPSocketSessionClosed;
         FWSNTPSocket.OnDataAvailable    := WSNTPSocketDataAvailable;
         FWSNTPSocket.OnChangeState      := ChangeState;
      except
         FWSNTPSocket.Free;
         FWSNTPSocket := nil;
      end;
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.ClientOnDataSent(Sender: TObject; Error: word);
begin
    {RFC says that the time server client will first close its
     connection and then the server will, but I think it's safer
     for this server to NOT wait for the client to close.}
    FClientSocket.Shutdown(2);
    FClientSocket.Close;
    TriggerQueryDone(Error);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.SetTimeProtocol(NewProtocolSet : TProtocolSet);
begin
   if FTimeProtocol = NewProtocolSet then
      exit;
   if not (csDesigning in ComponentState) and FRunning then begin
      if (tpTCP in FTimeProtocol) and not (tpTCP in NewProtocolSet) then begin
         FWTCPSocket.Close;
         FClientSocket.Close;
      end else if not (tpTCP in FTimeProtocol) and
                      (tpTCP in NewProtocolSet) then begin
         StartTCPServer;
      end;

      if (tpUDP in FTimeProtocol) and not (tpUDP in NewProtocolSet) then begin
         FWUDPSocket.Close;
      end else if not (tpUDP in FTimeProtocol) and
                      (tpUDP in NewProtocolSet) then begin
         StartUDPServer
      end;
      if (tpSNTP in FTimeProtocol) and not (tpSNTP in NewProtocolSet) then begin
         FWSNTPSocket.Close;
      end else if not (tpSNTP in FTimeProtocol) and
                      (tpSNTP in NewProtocolSet) then begin
         StartSNTPServer
      end;
   end;
   FTimeProtocol := NewProtocolSet;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.ChangeState(Sender: TObject;
                                OldState, NewState : TSocketState);
begin
   if (NewState = wsListening) and not FRunning then begin
      TriggerServerStart;
      FRunning := TRUE;
   end else if (NewState = wsClosed) and
               (FWUDPSocket.State = wsClosed) and
               (FWSNTPSocket.State = wsClosed) and
               (FWTCPSocket.State = wsClosed)
   then begin
      TriggerServerStop;
      FRunning := FALSE;
   end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.TriggerServerStart;
begin
   if Assigned(FOnStart) then
      FOnStart(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.TriggerServerStop;
begin
   if Assigned(FOnStop) then
      FOnStop(Self);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.StartTCPServer;
begin
   FWTCPSocket.Proto := 'tcp';
   FWTCPSocket.Addr  := '0.0.0.0';
   FWTCPSocket.Port  := 'time';
   FWTCPSocket.Listen;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.StartUDPServer;
begin
   FWUDPSocket.Proto := 'udp';
   FWUDPSocket.Addr  := '0.0.0.0';
   FWUDPSocket.Port  := 'time';
   FWUDPSocket.Listen;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTimeServ.StartSNTPServer;
begin
   FWSNTPSocket.Proto := 'udp';
   FWSNTPSocket.Addr  := '0.0.0.0';
   FWSNTPSocket.Port  := 'ntp';
   FWSNTPSocket.Listen;
end;

end.

