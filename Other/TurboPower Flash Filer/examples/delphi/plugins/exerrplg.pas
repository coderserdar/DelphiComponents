{*********************************************************}
{* FlashFiler: Example Error Logging Plugin              *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

unit ExErrPlg;

interface

uses
  Classes,
  FFLLBase,
  FFLLComm,
  FFLLLog,
  FFNetMsg;

  { The purpose of this plugin is to record client application errors within
    a centralized log file. }

  { Message constants and data structures for the plugin. }
  const
    ffnmLogError = ffnmUser + 1;

    ffc_ClientErrLog = '.\ClientErrors.log';
      { The file to which errors are logged. }

    ffc_CompExPage = 'FlashFiler Examples';

type
  { The following record structure is used to record the error.  Because
    the error message is dynamically-sized, we will dynamically allocate memory
    for each message.  MsgSize indicates the size of the stream. }
  PffnmLogError = ^TffnmLogError;
  TffnmLogError = packed record
    UserName : TffName;
      { We don't need to pass clientID as that will be part of the message
        header. }
    ErrorCode : LongInt;
    MsgSize : LongInt;
    Msg : TffVarMsgField;
  end;
    { For fast performance, we won't have the client apps wait for a reply
      to this message.  They'll just toss the error to the server and continue
      on their way. }

  { This is the abstract class defining the plugin's interface. }
  TffBaseErrorPlugin = class(TffBasePluginEngine)
  protected

    procedure scInitialize; override;

    procedure scPrepareForShutdown; override;

    procedure scShutdown; override;

    procedure scStartup; override;

  public
    procedure WriteError(const ClientID : TffClientID;
                         const ErrCode : LongInt;
                         const UserName : TffName;
                         const Msg : string); virtual; abstract;
  end;

  { This is the class that implements the plugin's interface.  Since
    TffLoggableComponent is one of its ancestors we will log all errors
    to TffLoggableComponent.EventLog. }
  TffServerErrorPlugin = class(TffBaseErrorPlugin)
  protected
  public
    procedure WriteError(const ClientID : TffClientID;
                         const ErrCode : LongInt;
                         const UserName : TffName;
                         const Msg : string); override;

  end;

  { This is the client-side class that implements a remote interface to the
    actual plugin engine. }
  TffRemoteErrorPlugin = class(TffBaseErrorPlugin)
  protected
    FClientID : TffClientID;
      { The clientID returned when a connection is established. }

    FTransport : TffBaseTransport;

    procedure ceSetTransport(aTransport : TffBaseTransport);
      { Called when setting the Transport property. }

    procedure scPrepareForShutdown; override;

    procedure scStartup; override;

  public
    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;                                      {!!.12}

    procedure WriteError(const ClientID : TffClientID;
                         const ErrCode : LongInt;
                         const UserName : TffName;
                         const Msg : string); override;

    procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent;  {!!.12}
                               const AData : TffWord32); override;      {!!.12}

  published
    property Transport : TffBaseTransport read FTransport write ceSetTransport;
      { The transport through which this class talks to the remote server. }
  end;

  { This is the plugin command handler that translates messages from the
    remote interface into method calls of the actual plugin engine. }
  TffErrorPluginCmdHandler = class(TffBasePluginCommandHandler)
  protected
    function epcGetPluginEngine : TffServerErrorPlugin;

    procedure epcSetPluginEngine(anEngine : TffServerErrorPlugin);

    procedure nmLogError(var Msg : TffDataMessage); message ffnmLogError;
      { Called by Process for ffnmLogError message. }

    procedure scInitialize; override;

    procedure scPrepareForShutdown; override;

    procedure scShutdown; override;

    procedure scStartup; override;

  public
    procedure Process(Msg : PffDataMessage; var handled : boolean); override;
      { This method is called by a command handler when it has a message that
        may be processed by a plugin.  If the plugin handles the message,
        set handled to True. }

  published
    property PluginEngine : TffServerErrorPlugin
      read epcGetPluginEngine write epcSetPluginEngine;

  end;

procedure Register;

implementation

uses
  SysUtils,
  FFLLComp,
  FFLLReq,
  FFSrBDE;

{===TffBaseErrorPlugin===============================================}
procedure TffBaseErrorPlugin.scInitialize;
begin
  { Do nothing. }
end;
{--------}
procedure TffBaseErrorPlugin.scPrepareForShutdown;
begin
  { Do nothing. }
end;
{--------}
procedure TffBaseErrorPlugin.scShutdown;
begin
  { Do nothing. }
end;
{--------}
procedure TffBaseErrorPlugin.scStartup;
begin
  { Do nothing. }
end;
{====================================================================}

{===TffServerErrorPlugin=============================================}
procedure TffServerErrorPlugin.WriteError(const ClientID : TffClientID;
                                          const ErrCode : LongInt;
                                          const UserName : TffName;
                                          const Msg : string);
const
  errMsgFormat = '%d %s %s [%d]';
    { <clientID> <userName> <message> [error code]

      Since we are using TffEventLog, the entire string is prefixed with the
      value of GetTickCount and the current thread ID. }
begin
  FEventLog.WriteStringFmt(errMsgFormat, [ClientID, UserName, Msg, ErrCode]);
end;
{====================================================================}

{===TffRemoteErrorPlugin=============================================}
constructor TffRemoteErrorPlugin.Create(aOwner : TComponent);
begin
  inherited;
  FTransport := nil;
end;
{Begin !!.12}
{--------}
destructor TffRemoteErrorPlugin.Destroy;
begin
  if FTransport <> nil then
    FTransport.FFRemoveDependent(Self);
  inherited;
end;
{End !!.12}
{--------}
procedure TffRemoteErrorPlugin.ceSetTransport(aTransport : TffBaseTransport);
begin
  { Check to make sure the new property is different.}
  if FTransport = aTransport then Exit;

  { Are we already connected to a transport? }
  if assigned(FTransport) then begin
    { Yes.  Notify it that we are disconnecting. }
    FTransport.FFRemoveDependent(Self);
      { D3 compatible. }
    FTransport := nil;
  end;

  { Do we have a new value for the Transport property? }
  if assigned(aTransport) then begin
    { Yes.  Set our internal variable and make sure the transport tells us
      when it is freed. }
    FTransport := aTransport;
    FTransport.FFAddDependent(Self);
  end;
end;
{--------}
{Begin !!.12}
procedure TffRemoteErrorPlugin.FFNotificationEx(const AOp : Byte;
                                                      AFrom : TffComponent;
                                                const AData : TffWord32);
begin
  inherited;
  if AOp in [ffn_Destroy, ffn_Remove] then
    if (AFrom = FTransport) then begin
      FTransport.FFRemoveDependent(Self);
      FTransport := nil;
    end;
end;
{End !!.12}
{--------}
procedure TffRemoteErrorPlugin.scPrepareForShutdown;
begin
  FTransport.TerminateConnection(FClientID, 5000);
end;
{--------}
procedure TffRemoteErrorPlugin.scStartup;
var
  Result : TffResult;
begin
  { Assumption: No login required. }
  Result := FTransport.EstablishConnection('', 0, 5000, FClientID);
  if Result <> DBIERR_NONE then
    raise Exception.Create('Error plugin could not establish connection.');
end;
{--------}
procedure TffRemoteErrorPlugin.WriteError(const ClientID : TffClientID;
                                          const ErrCode : LongInt;
                                          const UserName : TffName;
                                          const Msg : string);
var
  MsgSize : integer;
  Request : PffnmLogError;
  ReqLen  : integer;
begin
  { Requirement: Transport must be assigned. }
  if not assigned(FTransport) then
    raise Exception.Create('Must specify a transport in order to log errors.');

  { Requirement: Plugin must be started. }
  if scState <> ffesStarted then
    raise Exception.Create('Plugin must be started.');

  { Build the request.  We must dynamically allocate memory since the string
    may be of variable size. }
  MsgSize := Length(Msg);
  ReqLen := SizeOf(TffnmLogError) + Length(Msg) - SizeOf(TffVarMsgField);
  FFGetMem(Request, ReqLen);
  try
    Request^.UserName := UserName;
    Request^.ErrorCode := ErrCode;
    Request^.MsgSize := MsgSize;
    Move(PChar(Msg)^, Request^.Msg, MsgSize);
    FTransport.Post(0, FClientID, ffnmLogError, Request, ReqLen, 1000,
                    ffrmNoReplyExpected);
  finally
    FFFreeMem(Request, ReqLen);
  end;
end;
{====================================================================}

{===TffErrorPluginCmdHandler=========================================}
function TffErrorPluginCmdHandler.epcGetPluginEngine : TffServerErrorPlugin;
begin
  if Assigned(FPluginEngine) then
    Result := TffServerErrorPlugin(FPluginEngine)
  else
    Result := nil;
end;
{--------}
procedure TffErrorPluginCmdHandler.epcSetPluginEngine(anEngine : TffServerErrorPlugin);
begin
  if assigned(FPluginEngine) then begin
    FPluginEngine.FFRemoveDependent(Self);
    FPluginEngine := nil;
  end;
  if assigned(anEngine) then begin
    anEngine.FFAddDependent(Self);
    FPluginEngine := anEngine;
  end;
end;
{--------}
procedure TffErrorPluginCmdHandler.nmLogError(var Msg : TffDataMessage);
var
  aMsg : String;
  aStream : TMemoryStream;
begin
  with Msg, PffnmLogError(dmData)^ do begin
    aStream := TMemoryStream.Create;
    try
      aStream.Write(Msg, MsgSize);
      aStream.Position := 0;
      SetLength(aMsg, MsgSize);
      aStream.Read(aMsg[1], MsgSize);
      TffServerErrorPlugin(FPluginEngine).WriteError(dmClientID,
                                                     ErrorCode,
                                                     UserName,
                                                     aMsg);
    finally
      aStream.Free;
    end;
  end;
end;
{--------}
procedure TffErrorPluginCmdHandler.Process(Msg : PffDataMessage; var handled : boolean);
begin
  handled := (Msg^.dmMsg = ffnmLogError);
  if handled then
    Dispatch(Msg^);
      { Note: We don't have to free the message data because the a) we were
        called by TffBaseCommandHandler.Process and b) that particular method
        frees the data for us. }
end;
{--------}
procedure TffErrorPluginCmdHandler.scInitialize;
begin
  { Do nothing. }
end;
{--------}
procedure TffErrorPluginCmdHandler.scPrepareForShutdown;
begin
  { Do nothing. }
end;
{--------}
procedure TffErrorPluginCmdHandler.scShutdown;
begin
  { Do nothing. }
end;
{--------}
procedure TffErrorPluginCmdHandler.scStartup;
begin
  { Do nothing. }
end;
{====================================================================}

procedure Register;
begin
  RegisterComponents(ffc_CompExPage, [TffServerErrorPlugin,
                                      TffRemoteErrorPlugin,
                                      TffErrorPluginCmdHandler]);
end;

end.
