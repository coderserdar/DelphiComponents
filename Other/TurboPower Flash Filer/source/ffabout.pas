{*********************************************************}
{* FlashFiler: About box                                 *}
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

{$I ffdefine.inc}

unit ffabout;

interface

uses
  Windows,
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  TFFAboutBox = class(TForm)
    Bevel2: TBevel;
    Panel1: TPanel;
    Image1: TImage;
    ProgramName: TLabel;
    VersionNumber: TLabel;
    Label3: TLabel;
    lblTurboLink: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    OKButton: TButton;
    Label4: TLabel;
    lblNewsGeneral: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lblTurboLinkClick(Sender: TObject);
    procedure lblTurboLinkMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblNewsGeneralClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IsServer : boolean;
  end;

var
  FFAboutBox: TFFAboutBox;

implementation

{$R *.DFM}

uses
  ShellAPI, ffllbase;

resourcestring
  cBrowserError = 'Unable to start web browser. Make sure you have it properly setup on your system.';

procedure TFFAboutBox.OKButtonClick(Sender : TObject);
begin
  Close;
end;

const
  Domains : array [boolean] of string[6] = ('Client', 'Server');

procedure TFFAboutBox.FormActivate(Sender: TObject);
begin
  VersionNumber.Caption := Format('%d-bit %s: Version %5.4f %s',
      [
      32,
      Domains[IsServer],
      ffVersionNumber / 10000.0,
      ffSpecialString
      ]);
end;

procedure TFFAboutBox.lblTurboLinkClick(Sender: TObject);
begin
  ShellToWWW;
end;

procedure TFFAboutBox.lblTurboLinkMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.Style := [fsUnderline];
end;

procedure TFFAboutBox.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblTurboLink.Font.Style := [];
  lblNewsGeneral.Font.Style := [];
end;

procedure TFFAboutBox.lblNewsGeneralClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'http://sourceforge.net/forum/?group_id=72211', '',
                  '', SW_SHOWNORMAL) <= 32 then
    ShowMessage(cBrowserError);
end;

end.
