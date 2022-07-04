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
 * The Original Code is TurboPower XMLPartner
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)
{*********************************************************}
{* XMLPartner: XpFtpSh.PAS                               *}
{*********************************************************}
{* XMLPartner: XMLPartner's FTP shell                    *}
{*********************************************************}

{$I XpDefine.inc}

unit XpFtpSh;

interface

uses
  SysUtils,
  QDialogs,
  Classes,
  Libc;

type
  TTPSOnCommandOutput = procedure(Sender : TObject; Output : string) of object;

  TXpFtpShell = class
    private
      FFtp : boolean;
      FHttp : boolean;
      FUser : string;
      FPassword : string;
      FAnonymousLogin : boolean;
      FForceActive : boolean;
      FErrorString : string;
      FOutPutFile : string;
      FSaveOutput : Boolean;
      FCaptureStdErr : Boolean;
      FCommandOutput : TStringList;
      FOnCommandOutput : TTPSOnCommandOutput;
      procedure SetFtp(value : boolean);
      procedure SetHttp(value : boolean);
      procedure SetUser(value : string);
      procedure SetPassword(value : string);
      procedure SetAnonymousLogin(value : boolean);
      procedure SetForceActive( value  : boolean );
      procedure SetErrorString(_errno : integer);
      procedure SetOutPutFile(value : string);
      procedure SetSaveOutput(value : Boolean);
      procedure SetCaptureStdErr(value : Boolean);
      procedure SetCommandOutput(value : TStringList);
      function Execute(exe: string): boolean;
    published
      property Ftp : boolean
        read FFtp write SetFtp;
      property Http : boolean
        read FHttp write SetHttp;
      property User : string
        read FUser write SetUser;
      property Password : string
        read FPassword write SetPassword;
      property AnonymousLogin : boolean
        read FAnonymousLogin write SetAnonymousLogin;
      property ForceActive : boolean
        read FForceActive write SetForceActive;

      property ErrorString : string
        read FErrorString;
      property OutPutFile : string
        read FOutPutFile write SetOutPutFile;
      property SaveOutput : Boolean
        read FSaveOutput write SetSaveOutput;
      property CaptureStdErr : Boolean
        read FCaptureStdErr write SetCaptureStdErr;
      property CommandOutput : TStringList
        read FCommandOutput write SetCommandOutput;
    public
      constructor Create;
      destructor Destroy; override;
      function Get(URL : string) : boolean;
  end;

implementation

const
  cFtp = 'ftp';
  cFtpFull = ' ftp://';
  cHttpFull = ' http://';
  cForceActive = ' -A';
  cAnonymousLogin = ' -a';
  cEnableDebuggin = ' -d';
  cCacheReload = ' -f';
  cNoFileGlobbing = ' -g';
  cPrompt = ' -i';
  cNoAuthLogin = ' -n';
  cOutPutFile = ' -o';
  cPassiveMode = ' -p';
  cPort = ' -P';
  cWait = ' -r';
  cRestart = ' -R';
  cPacketTrace = ' -t';
  cNoVerbose = ' -V';
  cColon = ':';
  cAt = '@';

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
constructor TXpFtpShell.Create;
begin
  inherited;
  OutPutFile := '/tmp/tmpexml';
  Ftp := true;
  FCaptureStdErr := False;
  FCommandOutput := TStringList.Create;
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
destructor TXpFtpShell.Destroy;
begin
  inherited;
end;

function TXpFtpShell.Get(URL : string) : boolean;
var
  exe : string;
begin
  exe := cFtp;
  if FFtp then begin
    if not AnonymousLogin then
      if(FUser <> '')and(FPassword <> '')then begin
        exe := exe + cFtpFull;
        exe := exe + ' ' + User + cColon + Password + cAt;
      end else
        exe := exe + cAnonymousLogin + cFtpFull
    else
      exe := exe + cAnonymousLogin + cFtpFull;
  end else
    exe := exe + cHttpFull;

  exe := exe + URL;

  if( OutPutFile <> '' )then
    exe := exe + cOutPutFile + ' ' + OutPutFile;

  result := Execute(exe);
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
function TXpFtpShell.Execute(exe: string): boolean;
var
  fpIn : PIOFile;
  Buffer : PChar;
begin
  FCommandOutput.Clear;
  if FCaptureStdErr then begin
    fpIn := popen(PChar(exe + ' 2>&1'), 'r');
    if(fpIn = Nil)then begin
      result := false;
      SetErrorString(errno);
      exit;
    end;
  end else begin
    fpIn := popen(PChar(exe), 'r');
    if(fpIn = Nil)then begin
      result := false;
      SetErrorString(errno);
      exit;
    end;
  end;
  try
    try
      Buffer := StrAlloc(1024);
      try
        while(assigned(fpIn)) and (feof(fpIn) = 0)do begin
          memset(Buffer, 1024, 0);
          fgets(Buffer, 1020, fpIn);
          if FSaveOutput then
            FCommandOutput.Add(string(Buffer));
          if Assigned(FOnCommandOutput) then
            FOnCommandOutput(Self, string(Buffer));
        end;
      finally
        StrDispose(Buffer);
      end;
    finally
      if(pclose(fpIn) < 0) then begin
        SetErrorString(errno);
        Result := false;
      end else 
        Result := true;
    end;
  except
    on Exception do begin
      Result := false;
      SetErrorString(errno);
    end;
  end;
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetFtp(value  : boolean);
begin
  if(FFtp <> value)then
    FFtp := value;

  Fhttp := not FFtp;
end;

{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetHttp(value  : boolean);
begin
  if(FHttp <> value)then
    FHttp := value;

  FFtp := not FHttp;
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetUser(value : string);
begin
  if(FUser <> value)then
    FUser := value;
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetPassword(value : string);
begin
  if(FPassword <> value)then
    FPassword := value;
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetAnonymousLogin(value : boolean);
begin
  if(FAnonymousLogin <> value)then
    FAnonymousLogin := value;
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetErrorString(_errno : integer);
begin
  FErrorString := string(strerror(_errno));
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetOutPutFile(value  : string);
begin
  if(FOutPutFile <> value)then
    FOutPutFile := value;
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetSaveOutput(value : Boolean);
begin
  if value <> FSaveOutput then
    FSaveOutput := value;
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetCaptureStdErr(value : Boolean);
begin
  if value <> FCaptureStdErr then
    FCaptureStdErr := value;
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetCommandOutput(value : TStringList);
begin
  if assigned(value)then 
    FCommandOutput.Assign(value);
end;
{++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
procedure TXpFtpShell.SetForceActive(value : boolean);
begin
  if( FForceActive <> value )then
    FForceActive := value;
end;
end.
