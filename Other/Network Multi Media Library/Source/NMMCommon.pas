(*
  This file is a part of Network Multimedia Library v 1.0.
  Copyright (c) 2004 Ivan Babikov. All rights reserved.
  See the LICENSE file for more details.
  You can contact me at babikov@mail.ru

  Author contact info:
  EMail: babikov@mail.ru, i-software.narod.ru
  Project home: www.i-software.narod.ru
*)
unit NMMCommon;

{$DEFINE DEBUG}


interface
uses Types, IdTCPConnection, IdTCPServer, SyncObjs, SysUtils;

const
 DefaultServerPort= 3300;
 DefaultPeriod= 1000;

 ServerSendBufferSize= GSendBufferSizeDefault;
 ClientReceiveBufferSize= GSendBufferSizeDefault;

 MaxTimeToWaitForNetwork= 120; {secs}
 MaxTimeToWaitForNetworkInPeriods= 10; 
 DiffStreamCompressMaxLev= 100 ;
 DiffStreamCompressMinLength= 7;

 MaxDeltasInCash= 300;

// Client's commands
 cmdConnect= 'Connect';
 cmdUpdPer=  'UpdPer:';
 cmdGetRecentMessages= 'GetRecentMessages';
// Client's and server's commands
 cmdDisconnect= 'Disconnect';

// Server's command
 // Broadcast commands
 cmdIniData=  'Ini:';
  // Params
  prmWidth= 'prmWidth';
  prmHeight= 'prmHeight';
  prmImageSize= 'prmImageSize';
 cmdData=  'D:';
 cmdNoImages= 'NoImages';
 cmdSingleTxtMsg= 'SingleTxtMsg:';
 cmdTxtMsg=  'TxtMsg:';
 // Disconnection commands
 cmdRefused= 'Refused';
 cmdInvalidUser= 'InvalidUser';
 cmdInvalidPassword= 'InvalidPassword';
 cmdUserLoginExpired= 'LoginExpired';
 cmdDisconnectSlowClient= 'DisconSlowClient';
 cmdUserChangedLocationTo= 'UserChangedLocationTo:';


type
 EInvalidUpdatePeriod = class(Exception)
   constructor Create(AValue: Integer);
 end;

 NMMExceptionForUser = class(Exception);

 TImageServerThread = class(TIdPeerThread);

 TPictureWndUpdateEvent= procedure (Sender: TObject; R: TRect) of object;

 TStringWrapper = class(TObject)
 protected
   FValue: String;
 public
   constructor Create(AValue: String);
 published
   property Value: String read FValue write FValue;
 end;


function MakeHostAndPortString( Host: String; Port: Integer): String;
function ParseHostAndPortString( HostAndPortString: String; var Host: String; var Port: Integer): Boolean;
function ValidPeriod(APeriod: Integer): Boolean;
procedure AddErrorToLog(AError: String);
function DecodeParam(AParam,AString: String): String;
function EncodeParam(AParam,AValue: String): String;


implementation

var
 CS: TCriticalSection;

function MakeHostAndPortString( Host: String; Port: Integer): String;
begin
 if Port<>0 then
 begin
   result:= Host +':'+ IntToStr(Port)
 end
 else
 begin
   result:= '';
 end;
end;

function ParseHostAndPortString( HostAndPortString: String; var Host: String; var Port: Integer): Boolean;
var LP: Integer;
    LPort: String;
begin
 result:= false;
 LP:= Pos(':',HostAndPortString);
 if LP>0 then
 begin
   Host:= Copy(HostAndPortString,1,LP-1);
   LPort:= Copy(HostAndPortString,LP+1,Length(HostAndPortString));
   try
     Port:= StrToInt(LPort);
   except
     exit;
   end;
   result:= true;
 end;
end;

function ValidPeriod(APeriod: Integer): Boolean;
begin
  result:= (100 < APeriod) and (APeriod < 60*60*1000 {1 hour} );
end;

procedure AddErrorToLog(AError: String);
var LFile: TextFile;
begin
{$IFDEF DEBUG}
 CS.Enter;
 try
   AssignFile(LFile,'errors.log');
   if FileExists('errors.log') then
     Append(LFile)
   else
     Rewrite(LFile);
   try
     WriteLn(LFile,DateTimeToStr(Now)+' '+AError)
   finally
     CloseFile(LFile);
   end;
 finally
   CS.Leave;
 end;
{$ENDIF}
end;

{EInvalidUpdatePeriod}
constructor EInvalidUpdatePeriod.Create(AValue: Integer);
begin
  inherited Create('Invalid update period: '+IntToStr(AValue));
end;

{EInvalidMDIWindowName}
{
constructor EInvalidMDIWindowName.Create(AMDIWindowName: String);
begin
  inherited Create('Invalid MDI window name: "'+AMDIWindowName+'"');
end;
}


function EncodeParam(AParam,AValue: String): String;
begin
 result:= ' '+AParam+'='+AValue+'; '
end;

function DecodeParam(AParam,AString: String): String;
var
 LParam, LTmpStr: String;
 LValuePos, LP, i: Integer;
begin
 result:= '';
 LParam:= AParam+'=';
 LP:= Pos(LParam,AString);
 if LP>0 then
 begin
   LValuePos:= LP+Length(LParam);
   LTmpStr:= Copy(AString,LValuePos,Length(AString)-LValuePos+1);
   for i:=1 to Length(LTmpStr) do
   begin
     if (LTmpStr[i]=';') or (LTmpStr[i]=' ') then
     begin
       break;
     end
     else
     begin
       result:= result + LTmpStr[i];
     end;
   end;
 end;
end;



{TStringWrapper}
constructor TStringWrapper.Create(AValue: String);
begin
 inherited Create;
 FValue:= AValue;
end;


initialization
  CS:= TCriticalSection.Create;
finalization
  FreeAndNil(CS);
end.

