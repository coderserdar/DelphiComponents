{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.200, 2008-09-18

  Copyright (c) 2001-2009 Edward Benson

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbcASA;

interface

uses
  Windows, SysUtils;

const
  ASA_REGISTER_MESSAGE_CALLBACK = 1904;

var
  AsaCallbackMessage: AnsiString = 'WM_ASA_MESSAGE';
  WM_AsaCallback: Word;
  WM_AsaCallback_Supported: Boolean;

function ASA_MESSAGE_INIT(bWindow: THandle): Integer; stdcall;
procedure ASA_MESSAGE_CALLBACK(sqlca: pointer; msg_type: AnsiChar; code: longint; len: word; msg: PAnsiChar); stdcall;

exports
  ASA_MESSAGE_INIT, ASA_MESSAGE_CALLBACK;

implementation

uses
  DbxOpenOdbc;

var
  aWindow: THandle = 0;

function ASA_MESSAGE_INIT(bWindow: THandle): Integer; stdcall;
begin
  aWindow := bWindow;
  Result := Integer(WM_AsaCallback_Supported);
end;

procedure ASA_MESSAGE_CALLBACK(sqlca: pointer; msg_type: AnsiChar; code: longint; len: word; msg: PAnsiChar); stdcall;
var
  S: AnsiString;
begin
  if aWindow <> 0 then
  begin
    S := '';
    if len > 0 then
      S := StrPas(msg);
    SendMessage(aWindow, WM_AsaCallback, WPARAM(@S), LPARAM(msg_type));
  end;
end;

initialization
  WM_AsaCallback := RegisterWindowMessageA(PAnsiChar(AsaCallbackMessage));
end.
