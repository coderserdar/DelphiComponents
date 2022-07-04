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
{* XMLPartner: ExUtil.PAS 2.57                           *}
{*********************************************************}
{* XMLPartner: XML Editor Utility routines               *}
{*********************************************************}
unit ExUtil;

interface

uses
{$IFDEF WIN32}
  forms,
{$ENDIF}
{$IFDEF LINUX}
  Qt,
  Types,
  QForms,
{$ENDIF}
  inifiles;

{$IFDEF LINUX}
type
  pExMessage = ^TExMessage;
  TExMessage = packed record
    Msg: dword;
    case Integer of
      0: (
        WParam: Longint;
        LParam: Longint;
        Result: Longint);
      1: (
        WParamLo: Word;
        WParamHi: Word;
        LParamLo: Word;
        LParamHi: Word;
        ResultLo: Word;
        ResultHi: Word);
  end;

  pExMouseMessage = ^TExMouseMessage;
  TExMouseMessage = packed record
    Msg: dword;
    XPos: Longint;
    YPos: Longint;
  end;

function ExPostMessage(Handle: QObjectH; Msg: dword; wParam: Longint;
  lParam: Longint): Boolean;

function ExSendMessage(Handle: QObjectH; Msg: dword; wParam: Longint;
  lParam: Longint): Integer;
{$ENDIF}

procedure RestoreFormState(aForm : TForm; anINIFile : TINIFile;
                     const aSection : string);

procedure SaveFormState(aForm : TForm; anINIFile : TINIFile;
                  const aSection : string);

implementation

const
  csLeft   = 'Left';
  csTop    = 'Top';
  csWidth  = 'Width';
  csHeight = 'Height';
  csState  = 'State';

{$IFDEF LINUX}
function ExPostMessage(Handle: QObjectH; Msg: dword; wParam: Longint;
  lParam: Longint): Boolean;
var
  E: QCustomEventH;
  M: pExMessage;
begin
  Result := True;
  E := nil;
  GetMem(M, SizeOf(TExMessage));
  try
    M^.lParam := lParam;
    M^.wParam := wParam;
    M^.Msg := Msg;
    M^.Result := 0;

    E := QCustomEvent_create(QEventType(Msg), M);
    M := nil;

    QApplication_postEvent(Handle, E);
  except
    if Assigned(E) then QCustomEvent_destroy(E);
    if Assigned(M) then FreeMem(M);
    Result := False;
  end;
end;

function ExSendMessage(Handle: QObjectH; Msg: dword; wParam: Longint;
  lParam: Longint): Integer;
var
  E: QCustomEventH;
  M: pExMessage;
begin
  E := nil;
  GetMem(M, SizeOf(TExMessage));
  try
    M^.lParam := lParam;
    M^.wParam := wParam;
    M^.Msg := Msg;
    M^.Result := 0;

    E := QCustomEvent_create(QEventType(Msg), M);
    QApplication_sendEvent(Handle, E);
    Result := M^.Result;
  finally
    if Assigned(M) then FreeMem(M);
    if Assigned(E) then QCustomEvent_destroy(E);
  end;
end;
{$ENDIF}

procedure RestoreFormState(aForm : TForm; anINIFile : TINIFile;
                     const aSection : string);
begin
  with aForm, anINIFile do begin
    Left   := ReadInteger(aSection, csLeft, Left);
    Top    := ReadInteger(aSection, csTop, Top);
    Width  := ReadInteger(aSection, csWidth, Width);
    Height := ReadInteger(aSection, csHeight, Height);
    WindowState := TWindowState(anINIFile.ReadInteger(aSection, csState,
                                            Ord(WindowState)));
  end;
end;

procedure SaveFormState(aForm : TForm; anINIFile : TINIFile;
                  const aSection : string);
begin
  with aForm, anINIFile do begin
    WriteInteger(aSection, csState, Ord(WindowState));
    if WindowState <> wsMaximized then begin
      WriteInteger(aSection, csLeft, Left);
      WriteInteger(aSection, csTop, Top);
      WriteInteger(aSection, csWidth, Width);
      WriteInteger(aSection, csHeight, Height);
    end;
{$IFDEF LINUX}
    anINIFile.UpdateFile;
{$ENDIF}
  end;
end;

end.
