(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCustomConnectionHandle;

interface
uses Classes, IdTCPServer, IdTCPConnection,
     NMMCommon, NMMUserData, NMMCustomDataProcessors;

type
 TNMMConnectionState = set of (csNewConnection,csInvalidConnection);

 TNMMCustomConnectionHandle = class(TNMMMultiReferedObject)
 protected
   FPeerThread: TIdPeerThread;
   FConnection: TIdTCPConnection;
   FState: TNMMConnectionState;
   FUserData: TNMMUserData;
   FDeltasCount: Integer;
   FInStream: TMemoryStream;
   procedure SetPeriod( AValue: Integer );
   function GetPeriod: Integer;
   procedure SetUser(AUser: String);
   function GetUser: String;
   procedure SetPassword(APassword: String);
   function GetPassword: String;
   procedure SetConnectionTime(AConnectionTime: TDateTime);
   function GetConnectionTime: TDateTime;
   procedure SetState(AState: TNMMConnectionState);
   procedure InternalSetState(AState: TNMMConnectionState); virtual;
 public
   constructor Create(AUserData: TNMMUserData);
   destructor Destroy; override;
   procedure SendIniData(AIniData: TStream; AWidth, AHeight: Integer); virtual; abstract;
   procedure SendData(AData: TStream); virtual; abstract;
   procedure SendRawText(AText: String); virtual; abstract;
   procedure PullData;
   procedure ReceiveData(AData: TStream); virtual;
   procedure Disconnect; virtual;
   procedure SetInvalid;
   function Busy: Boolean; virtual;
   function Overloaded: Boolean; virtual;

   property State: TNMMConnectionState read FState write SetState;
   property PeerThread: TIdPeerThread read FPeerThread;
   property Connection: TIdTCPConnection read FConnection;
   property Period: Integer read GetPeriod write SetPeriod default DefaultPeriod;
   property User: String read GetUser write SetUser;
   property Password: String read GetPassword write SetPassword;
   property DeltasCount: Integer read FDeltasCount;
   property ConnectionTime: TDateTime read GetConnectionTime write SetConnectionTime;
 end;

implementation
uses SysUtils;


{TNMMCustomConnectionHandle}
constructor TNMMCustomConnectionHandle.Create(AUserData: TNMMUserData);
begin
 inherited Create;
 FInStream:= TMemoryStream.Create;
 FState:= [csNewConnection];
 FUserData:= AUserData;
end;

destructor TNMMCustomConnectionHandle.Destroy;
begin
 Assert( RefCount = 0,
         'TConnectionHandle.Destroy: Deletion of the ConnectionHandle with RefCount=' +
         IntToStr(RefCount) );
 FreeAndNil(FUserData);
 FreeAndNil(FInStream);
 inherited;
end;

procedure TNMMCustomConnectionHandle.SetState(AState: TNMMConnectionState);
begin
 if AState<>FState then
   InternalSetState(AState);
end;

procedure TNMMCustomConnectionHandle.InternalSetState(AState: TNMMConnectionState);
begin
 FState:= AState;
end;

procedure TNMMCustomConnectionHandle.Disconnect;
begin
end;

procedure TNMMCustomConnectionHandle.SetPeriod( AValue: Integer );
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

function TNMMCustomConnectionHandle.GetPeriod: Integer;
begin
 result:= FUserData.Period;
end;

function TNMMCustomConnectionHandle.GetUser: String;
begin
 result:= FUserData.User;
end;

procedure TNMMCustomConnectionHandle.SetUser(AUser: String);
begin
 FUserData.User:= AUser;
end;

function TNMMCustomConnectionHandle.GetPassword: String;
begin
 result:= FUserData.Password;
end;

procedure TNMMCustomConnectionHandle.SetPassword(APassword: String);
begin
 FUserData.Password:= APassword;
end;

procedure TNMMCustomConnectionHandle.SetConnectionTime(AConnectionTime: TDateTime);
begin
 FUserData.ConnectionTime:= AConnectionTime;
end;

function TNMMCustomConnectionHandle.GetConnectionTime: TDateTime;
begin
 result:= FUserData.ConnectionTime;
end;

procedure TNMMCustomConnectionHandle.SetInvalid;
begin
 State:= State + [csInvalidConnection];
end;

function TNMMCustomConnectionHandle.Overloaded: Boolean;
begin
 result:= false;
end;

function TNMMCustomConnectionHandle.Busy: Boolean;
begin
 result:= false;
end;

procedure TNMMCustomConnectionHandle.ReceiveData(AData: TStream);
begin
end;

procedure TNMMCustomConnectionHandle.PullData;
begin
  if Assigned(FConnection) then
  begin
    FInStream.Size:= 0;
    FConnection.ReadStream(FInStream,-1,False);
    FInStream.Seek(0,soFromBeginning);
    ReceiveData(FInStream);
  end;
end;

end.
