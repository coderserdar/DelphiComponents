
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
 * The Original Code is TurboPower Essentials Vol I
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

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsWebPE;
  {-component editor to provide web access}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Dialogs,
{$IFDEF AboveD6}
  DesignIntf,                                                        {!!.09}
  DesignEditors,                                                     {!!.09}
{$ELSE}
  dsgnintf,
{$ENDIF}
  Classes, ShellApi,
  EsData;

const
  WebText1  = 'TurboPower on the Web';
  WebText2  = 'Essentials Home Page';
  MailText  = 'Send a Support Message';

type
  TEsWebEditor = class(TDefaultEditor)
  public
    procedure ExecuteVerb(Index : Integer);
      override;
    function GetVerb(Index : Integer) : AnsiString;
      override;
    function GetVerbCount : Integer;
      override;
  end;

procedure ShellWebCall1;
procedure ShellWebCall2;
procedure ShellMailCall;

implementation

procedure ShellWebCall1;
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage('Unable to start web browser. Make sure you have it properly set-up on your system.');
end;

procedure ShellWebCall2;
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com/products/essentials', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage('Unable to start web browser. Make sure you have it properly set-up on your system.');
end;

procedure ShellMailCall;
begin
  if ShellExecute(0, 'open', 'mailto:support@turbopower.com', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage('Unable to start mail client. Make sure you have it properly set-up on your system.');
end;


{*** TEsWebEditor ***}

procedure TEsWebEditor.ExecuteVerb(Index : Integer);
begin
  case Index of
    0 : ShellWebCall1;
    1 : ShellWebCall2;
    2 : ShellMailCall;
  end;
end;

function TEsWebEditor.GetVerb(Index : Integer) : AnsiString;
begin
  case Index of
    0 : Result := WebText1;
    1 : Result := WebText2;
    2 : Result := MailText;
  else
    Result := '?';
  end;
end;

function TEsWebEditor.GetVerbCount : Integer;
begin
  Result := 3;
end;

end.
