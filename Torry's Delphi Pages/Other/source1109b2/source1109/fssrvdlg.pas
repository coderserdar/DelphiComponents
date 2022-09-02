{*********************************************************}
{* FlashFiler: Server Selection Dialog                   *}
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

{$I fsdefine.inc}

Unit fssrvdlg;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Forms,
  Controls,
  Stdctrls,
  Buttons,
  ExtCtrls;

Type
  TFSPickServerDlg = Class(TForm)
    Bevel1: TBevel;
    CBNames: TComboBox;
    Label1: TLabel;
    OKBtn: TBitBtn;
  Private
    { Private declarations }
  Public
    { Public declarations }
  End;

Var
  FSPickServerDlg: TFSPickServerDlg;

Implementation

{$R *.DFM}

End.

