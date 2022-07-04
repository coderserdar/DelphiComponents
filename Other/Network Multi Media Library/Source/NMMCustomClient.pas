(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomClient;

interface

uses
  Classes, Controls, ExtCtrls,
  IdAssignedNumbers, IdBaseComponent, IdComponent, IdGlobal,
  IdSocks, IdIOHandler, IdIOHandlerSocket,
  IdTCPConnection, IdTCPClient,
  NMMStatistics, SysUtils, NMMCommon,
  NMMUserData;

type
 TRefuseReason= (rrUnknown,rrInvalidUserName,rrInvalidPassword,rrUserLoginExpired,rrUserChangedLocationTo);
 TTextMessageEvent= procedure(Sender: TObject; Msg: string) of object;
 TImageUpdateEvent= procedure(Sender: TObject) of object;
 TConnectionTerminatedEvent= procedure(Sender: TObject; ErrorMsg: string) of object;
 TConnectionRefusedEvent= procedure(Sender: TObject; RefuseReason: TRefuseReason; ExtendedInfo: string) of object;
 TPeriodChangedEvent= procedure(Sender: TObject; NewPeriod: Integer) of object;
 TStatusChangedEvent= procedure(Sender: TObject; Connected: Boolean; ExtendedStatusInfo: string) of object;


 TNMMCustomClientThread = class;


 TNMMCustomClient = class(TComponent)
 protected
   FTCPClient: TIdTCPClient;
   FIdIOHandlerSocket: TIdIOHandlerSocket;
   FIdSocksInfo: TIdSocksInfo;
   FThread: TNMMCustomClientThread;
   FServerHost: String;
   FServerPort: Integer;
   FUserData: TNMMUserData;
   FActive: Boolean;
   FTerminated: Boolean;
   FConnectionError: String;
   FKeepConnection: Boolean;
   FReduceLoadOnDisconnect: Boolean;
   FReconnectTimer: TTimer;
   FLoadingIni: Boolean; // todo: rename
   FDeltaWaitTime: Int64; // todo: rename
   FLastDeltaTime: TDateTime; // todo: rename
   FAutoReconnectInterval: Integer;
   FReadLnTimeout: Integer;
   {Proxy}
   FSocksProxyAuthentication: TSocksAuthentication;
   FSocksProxyHost: string;
   FSocksProxyPassword: string;
   FSocksProxyPort: Integer;
   FSocksProxyUsername: string;
   FSocksProxyVersion: TSocksVersion;
   {Events}
   FOnTextMessage: TTextMessageEvent;
   FSingleTextMessageEvent: TTextMessageEvent;
   FOnConnected: TNotifyEvent;
   FOnDisconnected: TNotifyEvent;
   FOnConnectionTeminated: TConnectionTerminatedEvent;
   FPeriodChangedEvent: TPeriodChangedEvent;
   FOnConnectionTooSlow: TNotifyEvent;
   FOnStatusChanged: TStatusChangedEvent;
   FOnConnectionRefused: TConnectionRefusedEvent;

   procedure SetServerHost(AValue: String);
   procedure SetServerPort(AValue: Integer);
   procedure SetPeriod(AValue: Integer ); virtual;
   function GetPeriod: Integer;
   procedure SetUser(AUser: String);
   function GetUser: String;
   procedure SetPassword(APassword: String);
   function GetPassword: String;
   procedure SetAutoReconnectInterval(AValue: Integer);
   procedure ConnectionClosed(Sender: TObject);
   procedure FIdTCPClientWorkBegin(Sender: TObject;
     AWorkMode: TWorkMode; const AWorkCountMax: Integer);
   procedure FIdTCPClientWorkEnd(Sender: TObject;
     AWorkMode: TWorkMode);
   procedure FIdTCPClientWork(Sender: TObject; AWorkMode: TWorkMode;
     const AWorkCount: Integer);
   procedure OnReconnectTimer(Sender: TObject);
   procedure OnTCPClientConnect(Sender: TObject);
   procedure OnTCPClientDisconnect(Sender: TObject);
   procedure DoDisconnect;
   function CreateThread(CreateSuspended: Boolean; AParent: TNMMCustomClient): TNMMCustomClientThread; virtual; abstract;
   procedure ReduceLoad; virtual;
   procedure ClearData; virtual;
   procedure DoOnConnect; virtual;
   procedure DoOnDisconnect; virtual;
 public
   Statistics: TNMMStatistics;

   constructor Create(AOwner: TComponent); override;
   destructor Destroy; override;
   procedure Connect; virtual;
   procedure Disconnect; virtual;
   procedure SendCustomCommand(ACommand: String);

   property TCPClient: TIdTCPClient read FTCPClient;
   property IdIOHandlerSocket: TIdIOHandlerSocket read FIdIOHandlerSocket;
   property IdSocksInfo: TIdSocksInfo read FIdSocksInfo;
   property Active: Boolean read FActive;
   property ConnectionError: String read FConnectionError;
   property LoadingIni: Boolean read FLoadingIni write FLoadingIni; // todo: rename
   property DeltaWaitTime: Int64 read FDeltaWaitTime write FDeltaWaitTime; // todo: rename
   property LastDeltaTime: TDateTime read FLastDeltaTime write FLastDeltaTime; // todo: rename
   property Period: Integer read GetPeriod write SetPeriod default DefaultPeriod;
   property ReduceLoadOnDisconnect: Boolean
     read FReduceLoadOnDisconnect write FReduceLoadOnDisconnect default true;

 published
   property User: String read GetUser write SetUser;
   property Password: String read GetPassword write SetPassword;
   property ServerHost: String read FServerHost write SetServerHost;
   property ServerPort: Integer read FServerPort write SetServerPort default DefaultServerPort;
   property KeepConnection: Boolean
     read FKeepConnection write FKeepConnection default true;
   property AutoReconnectInterval: Integer
     read FAutoReconnectInterval write SetAutoReconnectInterval default 1000;

   {proxy params}
   property SocksProxyAuthentication: TSocksAuthentication
     read FSocksProxyAuthentication write FSocksProxyAuthentication default ID_SOCKS_AUTH;
   property SocksProxyHost: string read FSocksProxyHost write FSocksProxyHost;
   property SocksProxyPassword: string
     read FSocksProxyPassword write FSocksProxyPassword;
   property SocksProxyPort: Integer
     read FSocksProxyPort write FSocksProxyPort default IdPORT_SOCKS;
   property SocksProxyUsername: string
     read FSocksProxyUsername write FSocksProxyUsername;
   property SocksProxyVersion: TSocksVersion
     read FSocksProxyVersion write FSocksProxyVersion default ID_SOCKS_VER;

   {events}
   property OnConnectionTeminated: TConnectionTerminatedEvent read FOnConnectionTeminated
                                                             write FOnConnectionTeminated;

   property PeriodChangedEvent: TPeriodChangedEvent read FPeriodChangedEvent
                                write FPeriodChangedEvent;
   property OnConnectionTooSlow: TNotifyEvent read FOnConnectionTooSlow
                                              write FOnConnectionTooSlow;
   property OnConnected: TNotifyEvent
     read FOnConnected write FOnConnected;
   property OnDisconnected: TNotifyEvent
     read FOnDisconnected write FOnDisconnected;
   property OnStatusChanged: TStatusChangedEvent
     read FOnStatusChanged write FOnStatusChanged;
   property OnConnectionRefused: TConnectionRefusedEvent
     read FOnConnectionRefused write FOnConnectionRefused;
   property OnTextMessage: TTextMessageEvent read FOnTextMessage write FOnTextMessage;
 end;

 TNMMCustomClientThread = class(TThread)
 protected
   FParent: TNMMCustomClient;
   FTextMessage: String;
   FExtendedStatusInfo: String;
   FLoadingIni: Boolean; //todo: rename
   FMessageForUser: String;
   procedure DoWork; virtual;
   procedure PutTextMessage;
   procedure ChangeStatus(AExtendedStatusInfo: String);
   procedure ChangeStatusSynchronized;
   procedure DoHandleCommand(S: String); virtual;
   procedure ReadStream(S: String; const Tag: String; AStream: TStream);
   procedure Idle; virtual;
   procedure ShowMessage;
   procedure DoOnTextMessage;
 public
   constructor Create(CreateSuspended: Boolean; AParent: TNMMCustomClient);
   destructor Destroy; override;
   procedure Execute; override;
 end;


implementation
uses DateUtils, Forms, Windows;


{TNMMCustomClient}
constructor TNMMCustomClient.Create(AOwner: TComponent);
begin
 inherited;
 FUserData:= TNMMUserData.Create;
 FServerHost:= '';
 FServerPort:= DefaultServerPort;
 SetPeriod(DefaultPeriod);
 FKeepConnection:= true;
 FReduceLoadOnDisconnect:= true;
 FAutoReconnectInterval:= 1000;
 FReadLnTimeout:= 30000;

 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   FTCPClient:= TIdTCPClient.Create(self);
   FTCPClient.RecvBufferSize:= ClientReceiveBufferSize;
   FTCPClient.OnWorkBegin:= FIdTCPClientWorkBegin;
   FTCPClient.OnWorkEnd:= FIdTCPClientWorkEnd;
   FTCPClient.OnWork:= FIdTCPClientWork;
   FTCPClient.OnConnected:= OnTCPClientConnect;
   FTCPClient.OnDisconnected:= OnTCPClientDisconnect;
   
   FIdIOHandlerSocket:= TIdIOHandlerSocket.Create(self);
   FIdSocksInfo:= TIdSocksInfo.Create(self);
   FReconnectTimer:= TTimer.Create(self);
   FReconnectTimer.Interval:= FAutoReconnectInterval;
   FReconnectTimer.OnTimer:= OnReconnectTimer;
   FReconnectTimer.Enabled:= false;
 end;
end;

destructor TNMMCustomClient.Destroy;
begin
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   FOnStatusChanged:= nil;
   FOnConnected:= nil;
   FOnDisconnected:= nil;
   FOnConnectionTeminated:= nil;
   FPeriodChangedEvent:= nil;
   FOnConnectionRefused:= nil;
   FOnConnectionTooSlow:= nil;
   FPeriodChangedEvent:= nil;
   FOnConnectionTeminated:= nil;
   FreeAndNil(FTCPClient);
   FreeAndNil(FIdIOHandlerSocket);
   FreeAndNil(FIdSocksInfo);
   FreeAndNil(FReconnectTimer);
 end;
 FreeAndNil(FUserData);
 inherited;
end;

procedure TNMMCustomClient.Connect;
begin
 FTerminated:= false;
 FConnectionError:= '';
 FLoadingIni:= false;

 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   Statistics.DeltaBytesReceived:= 0;
   Statistics.IniBytesReceived:= 0;
   Statistics.TextBytesReceived:= 0;
   Statistics.DeltasReceivedCount:= 0;
   Statistics.DeltasProcessedCount:= 0;
   Statistics.LastDeltaSize:= 0;
   FTCPClient.Host:= FServerHost;
   FTCPClient.Port:= FServerPort;
   if FSocksProxyVersion<>svNoSocks then
   begin
     FIdSocksInfo.Authentication:= FSocksProxyAuthentication;
     FIdSocksInfo.Host:= FSocksProxyHost;
     FIdSocksInfo.Password:= FSocksProxyPassword;
     FIdSocksInfo.Port:= FSocksProxyPort;
     FIdSocksInfo.Username:= FSocksProxyUsername;
     FIdSocksInfo.Version:= FSocksProxyVersion;
     FIdIOHandlerSocket.SocksInfo:= FIdSocksInfo;
     FTCPClient.IOHandler:= FIdIOHandlerSocket;
   end
   else
   begin
     FTCPClient.IOHandler:= nil;
   end;
   ClearData;

   FThread:= CreateThread(true,self);
   try
     if Assigned(FOnStatusChanged) then
       FOnStatusChanged(Self,FActive,'Connecting');

     FTCPClient.Connect( IdTimeoutInfinite );
     if Assigned(FOnStatusChanged) then
       FOnStatusChanged(Self,FActive,'Connected');

     FThread.Resume;

     if Assigned(FOnStatusChanged) then
       FOnStatusChanged(Self,FActive,'Sending user info');

     FTCPClient.WriteLn(FUserData.UserData);
   except
     on E: Exception do
     begin
       try
         FThread.Terminate;
       finally
         if Assigned(FOnStatusChanged) then
           FOnStatusChanged(Self,FActive,'Connection error: '+E.Message);
         AddErrorToLog('Connection error: '+E.Message);
       end;
     end;
   end;
   FReconnectTimer.Enabled:= true;
 end;
end;

procedure TNMMCustomClient.Disconnect;
begin
 FReconnectTimer.Enabled:= false;
 DoDisconnect;
end;

procedure TNMMCustomClient.DoDisconnect;
begin
 FActive:= false;
 if (not (csDesigning in ComponentState)) and
    (not (csLoading in ComponentState)) then
 begin
   ClearData;
   try
     if FTCPClient.Connected then
     begin
       if Assigned(FOnStatusChanged) then
       begin
         FOnStatusChanged(Self,FActive,'Disconnecting');
       end;
       try
         FTCPClient.WriteLn(cmdDisconnect);
         sleep(200);
       except
         FTCPClient.Disconnect;
         Sleep(50);
         FThread.Terminate;
         raise;
       end;
       if FTCPClient.Connected then
       begin
         FTCPClient.Disconnect;
         Sleep(50);
         FThread.Terminate;
         Sleep(100);
       end;
       if Assigned(FOnStatusChanged) then
       begin
         FOnStatusChanged(Self,FActive,'Connection closed');
       end;
     end;
   except
     on E: Exception do
     begin
       if Assigned(FOnStatusChanged) then
       begin
         FOnStatusChanged(Self,FActive,'Connection error: '+E.Message);
       end;
     end;
   end;
 end;
end;


function SanctionedDisconnect(AConnectionError: String): Boolean;
{this is a "sanctioned" disconnect}
begin
  result:=
    (Pos(cmdRefused,AConnectionError)>0) or
    (Pos(cmdInvalidUser,AConnectionError)>0) or
    (Pos(cmdInvalidPassword,AConnectionError)>0) or
    (Pos(cmdUserLoginExpired,AConnectionError)>0) or
    (Pos(cmdUserChangedLocationTo,AConnectionError)>0);
end;

procedure TNMMCustomClient.ReduceLoad;
begin
  if Assigned(FPeriodChangedEvent) then
     FPeriodChangedEvent(self,FUserData.Period);
  if Assigned(FOnConnectionTooSlow) then
     FOnConnectionTooSlow(self);
  FUserData.Period:= FUserData.Period * 2;//NextPeriod(FUserData.Period);
end;

procedure TNMMCustomClient.ConnectionClosed(Sender: TObject);
 var LExtendedInfo: String;
 function GetRefuseReason: TRefuseReason;
 var LTagLen: Integer;
 begin
   LExtendedInfo:= '';
   Result:= rrUnknown;
   if Pos(cmdRefused,FConnectionError)>0 then
      Result:= rrUnknown
   else if Pos(cmdInvalidUser,FConnectionError)>0 then
      Result:= rrInvalidUserName
   else if Pos(cmdInvalidPassword,FConnectionError)>0 then
      Result:= rrInvalidPassword
   else if Pos(cmdUserLoginExpired,FConnectionError)>0 then
      Result:= rrUserLoginExpired
   else if Pos(cmdUserChangedLocationTo,FConnectionError)>0 then
   begin
      Result:= rrUserChangedLocationTo;
      LTagLen:= Length(cmdUserChangedLocationTo);
      LExtendedInfo:= Copy(FConnectionError,LTagLen+1,Length(FConnectionError)-LTagLen);
   end;
 end;

var LRefuseReason: TRefuseReason;
begin
 ClearData;

 if Assigned(FOnStatusChanged) then
   FOnStatusChanged(Self,false,'Connection closed');

 if not FActive then
 {disconnected by the client app}
 begin
   exit;
 end;

 try
   if SanctionedDisconnect(FConnectionError) then
   begin
     Disconnect
   end
   else
   begin
     DoDisconnect;
   end;
 finally
   if (FTerminated=true) or SanctionedDisconnect(FConnectionError) then
   begin
     if Assigned(FOnConnectionTeminated) then
     begin
       FOnConnectionTeminated(self,FConnectionError);
     end;
   end;
   if SanctionedDisconnect(FConnectionError) then
   begin
     if Assigned(FOnConnectionRefused) then
     begin
       LRefuseReason:= GetRefuseReason;
       FOnConnectionRefused(self,LRefuseReason,LExtendedInfo);
     end;
   end;
 end;

 if Pos(cmdDisconnectSlowClient,FConnectionError)>0 then
 begin
   ReduceLoad;
 end;

 if ( FKeepConnection and not SanctionedDisconnect(FConnectionError) and
      (Trim(FConnectionError)='')
      ) or FReduceLoadOnDisconnect then
 begin
   if FReduceLoadOnDisconnect then
   begin
     ReduceLoad;
     if Assigned(FOnStatusChanged) then
       FOnStatusChanged(Self,FActive,'Reducing loading and reconnecting');
   end
   else
   begin
     if Assigned(FOnStatusChanged) then
       FOnStatusChanged(Self,FActive,'Reconnecting');
   end;
 end;
end;

procedure TNMMCustomClient.SetServerHost( AValue: String );
begin
 if FServerHost<>AValue then
 begin
   FServerHost:= AValue;
 end;
end;

procedure TNMMCustomClient.SetServerPort( AValue: Integer );
begin
 if FServerPort<>AValue then
 begin
   FServerPort:= AValue;
 end;
end;

procedure TNMMCustomClient.SetPeriod( AValue: Integer );
begin
 if FUserData.Period<>AValue then
 begin
   if ValidPeriod(AValue) then
   begin
     FUserData.Period:= AValue;
   end
   else
   begin
     raise EInvalidUpdatePeriod.Create(AValue);
   end;
 end;
end;

function TNMMCustomClient.GetPeriod: Integer;
begin
 if Assigned(FUserData)  then
   result:= FUserData.Period
 else
   result:= 0;
end;

function TNMMCustomClient.GetUser: String;
begin
 if Assigned(FUserData)  then
   result:= FUserData.User
 else
   result:= '';
end;

procedure TNMMCustomClient.SetUser(AUser: String);
begin
 if Assigned(FUserData)  then
   FUserData.User:= AUser;
end;

function TNMMCustomClient.GetPassword: String;
begin
 if Assigned(FUserData)  then
   result:= FUserData.Password
 else
   result:= '';
end;

procedure TNMMCustomClient.SetPassword(APassword: String);
begin
 if Assigned(FUserData)  then
   FUserData.Password:= APassword;
end;

procedure TNMMCustomClient.SetAutoReconnectInterval(AValue: Integer);
begin
 if FAutoReconnectInterval<>AValue then
 begin
   FAutoReconnectInterval:= AValue;
   if (not (csDesigning in ComponentState)) then
   begin
     if FReconnectTimer<>nil then
       FReconnectTimer.Interval:= AValue;
   end;
 end;
end;

procedure TNMMCustomClient.FIdTCPClientWorkBegin(Sender: TObject;
  AWorkMode: TWorkMode; const AWorkCountMax: Integer);
begin
end;

procedure TNMMCustomClient.FIdTCPClientWorkEnd(Sender: TObject;
  AWorkMode: TWorkMode);
begin
end;

procedure TNMMCustomClient.FIdTCPClientWork(Sender: TObject; AWorkMode: TWorkMode;
  const AWorkCount: Integer);
begin
end;

procedure TNMMCustomClient.OnReconnectTimer(Sender: TObject);
begin
 if FKeepConnection and not TCPClient.Connected then
 begin
   Connect;
 end;
end;

procedure TNMMCustomClient.OnTCPClientConnect(Sender: TObject);
begin
 FActive:= true;
 if Assigned(FOnConnected) then
   FOnConnected(Self);
 if Assigned(FOnStatusChanged) then
   FOnStatusChanged(Self,FActive,'');
 DoOnConnect;
end;

procedure TNMMCustomClient.OnTCPClientDisconnect(Sender: TObject);
begin
 FActive:= false;
 FThread.Terminate;
 if Assigned(FOnDisconnected) then
   FOnDisconnected(Self);
 if Assigned(FOnStatusChanged) then
   FOnStatusChanged(Self,FActive,'');
 DoOnDisconnect;  
end;

procedure TNMMCustomClient.SendCustomCommand(ACommand: String);
begin
 try
   FTCPClient.WriteLn(ACommand);
 except
   on E: Exception do
   begin
     if Assigned(FOnStatusChanged) then
       FOnStatusChanged(Self,FActive,'Connection error: '+E.Message);
   end;
 end;
end;

procedure TNMMCustomClient.ClearData;
begin
end;

procedure TNMMCustomClient.DoOnConnect;
begin
end;

procedure TNMMCustomClient.DoOnDisconnect;
begin
end;


{==============================================================================}

{TNMMCustomClientThread}
constructor TNMMCustomClientThread.Create(CreateSuspended: Boolean; AParent: TNMMCustomClient);
begin
 inherited Create(CreateSuspended);
 FParent:= AParent;
 FTextMessage:= '';
 OnTerminate:= FParent.ConnectionClosed;
 FreeOnTerminate:= true;
end;

destructor TNMMCustomClientThread.Destroy;
begin
 inherited;
end;

procedure TNMMCustomClientThread.ShowMessage;
begin
  Application.MessageBox(PChar(FMessageForUser),'Exception',MB_OK);
end;

procedure TNMMCustomClientThread.Execute;
begin
 while not Terminated do
 begin
   try
     Idle;
     DoWork;
   except
     on E: NMMExceptionForUser do
     begin
       FMessageForUser:= E.Message;
       Synchronize(ShowMessage);
       Terminate;
     end;
     on E: Exception do
     begin
       FParent.FReadLnTimeout:= FParent.FReadLnTimeout * 2;
       AddErrorToLog('Connection error: '+E.Message);
       if Assigned(FParent.FOnStatusChanged) then
         FParent.FOnStatusChanged(Self,FParent.FActive,'Connection error: '+E.Message);
       Terminate;
     end;
   end;
 end;
end;

procedure TNMMCustomClientThread.ReadStream(S: String; const Tag: String; AStream: TStream);
begin
  AStream.Size:= 0;
  FParent.FTCPClient.ReadStream(AStream,{LIntStreamSize}-1,False);
  AStream.Seek(0,soFromBeginning);
end;

procedure TNMMCustomClientThread.DoWork;
var S: String;
    LTagLen: Integer;
begin
 FParent.FLastDeltaTime:= Now;
 try
   if FParent.FTCPClient.Connected and FParent.FActive then
   begin
     if FParent.Statistics.DeltasProcessedCount > 0 then
     begin
       FParent.FDeltaWaitTime:= MillisecondsBetween(FParent.FLastDeltaTime,Now);
       if (FParent.FDeltaWaitTime>(FParent.FUserData.Period*{1000*}MaxTimeToWaitForNetworkInPeriods)) or
          (FParent.FDeltaWaitTime>MaxTimeToWaitForNetwork*1000) then
       begin
         if FParent.FReduceLoadOnDisconnect then
         begin
           if Assigned(FParent.FOnStatusChanged) then
             ChangeStatus('Disconnecting to reduce loading');
           FParent.FTCPClient.Disconnect;
           Terminate;
           Sleep(100);
           exit;
         end;
       end;
     end;

     S:= FParent.FTCPClient.ReadLn( LF{, FParent.FReadLnTimeout {ATimeout});

     if Length(Trim(S))>0 then
     begin
       inc( FParent.Statistics.TextBytesReceived,Length(S));
     end
     else
     begin
       Sleep(100);
       Exit;
     end;

     if (Pos(cmdDisconnect,S)=1) or SanctionedDisconnect(S) then
     begin
       FParent.FTerminated:= true;
       FParent.FConnectionError:= S;
       Terminate;
       ChangeStatus('Terminating connection');
     end;

     if Pos(cmdTxtMsg,S)=1 then
     begin
       LTagLen:= Length(cmdTxtMsg);
       FTextMessage:= Copy(S,LTagLen+1,Length(S)-LTagLen);
       Synchronize(PutTextMessage);
     end else
     if Trim(S)<>'' then
     begin
       DoHandleCommand(S);
     end;
   end
   else
   begin
     Terminate;
   end;
 except
   on E: NMMExceptionForUser do
   begin
     FMessageForUser:= E.Message;
     Synchronize(ShowMessage);
     Terminate;
   end;
   on E: Exception do
   begin
     if Assigned(FParent.FOnStatusChanged) then
       FParent.FOnStatusChanged(Self,FParent.FActive,'Connection error: '+E.Message);
     FParent.FTerminated:= true;
     Terminate;
     raise;
   end;
 end;
end;

procedure TNMMCustomClientThread.ChangeStatus(AExtendedStatusInfo: String);
begin
 FExtendedStatusInfo:= AExtendedStatusInfo;
 Synchronize(ChangeStatusSynchronized);
end;

procedure TNMMCustomClientThread.ChangeStatusSynchronized;
begin
 if Assigned(FParent.FOnStatusChanged) then
   FParent.FOnStatusChanged(FParent,FParent.FActive,FExtendedStatusInfo);
end;

procedure TNMMCustomClientThread.PutTextMessage;
begin
 if Assigned(FParent.FOnTextMessage) then
    FParent.FOnTextMessage(FParent,FTextMessage);
 FTextMessage:= '';
end;

procedure TNMMCustomClientThread.DoHandleCommand(S: String);
var LTagLen: Integer;
begin
  if Pos(cmdTxtMsg,S)=1 then
  begin
    LTagLen:= Length(cmdSingleTxtMsg);
    FTextMessage:= Copy(S,LTagLen+1,Length(S)-LTagLen);
    Synchronize(DoOnTextMessage);
  end;
end;

procedure TNMMCustomClientThread.DoOnTextMessage;
begin
 if Assigned(FParent.FOnTextMessage) then
    FParent.FOnTextMessage(FParent,FTextMessage);
 FTextMessage:= '';
end;

procedure TNMMCustomClientThread.Idle;
begin
end;

end.
