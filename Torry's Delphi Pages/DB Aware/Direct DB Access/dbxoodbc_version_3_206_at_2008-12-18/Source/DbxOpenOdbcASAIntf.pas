{
  Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.100, 2008-02-06

  Copyright (c) 2001-2008 Edward Benson

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}
unit DbxOpenOdbcASAIntf;

interface

uses
  Windows;

function ASA_MESSAGE_INIT(bWindow: THandle): Integer; stdcall;
procedure ASA_MESSAGE_CALLBACK(sqlca: pointer; msg_type: AnsiChar; code: longint; len: word; msg: PAnsiChar); stdcall;

implementation

function ASA_MESSAGE_INIT(bWindow: THandle): Integer; stdcall; external 'dbxoodbc.dll';
procedure ASA_MESSAGE_CALLBACK(sqlca: pointer; msg_type: AnsiChar; code: longint; len: word; msg: PAnsiChar); stdcall; external 'dbxoodbc.dll';

(*
--------------------------------------------------------------
---                          DEMO:                         ---
--------------------------------------------------------------

procedure TMainForm.ASASQLConnectionAfterConnect(Sender: TObject);
begin
  if ASA_MESSAGE_INIT(Application.Handle) = 0 then
    ShowMessage('ASA Callback is not supported');
end;

--------------------------------------------------------------

procedure TMainForm.ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
var
  pS: PAnsiString;
begin
  if Msg.message = WM_AsaCallback then
  begin
    pS := PAnsiString(Pointer(Msg.wParam));
    if pS <> nil then
      ShowMessage(Format('Message type: %d; Message text: %s', [Msg.lParam, pS^])
    else
      ShowMessage(Format('Message type: %d', [Msg.lParam]);
  end;
end;

--------------------------------------------------------------
*)

end.
