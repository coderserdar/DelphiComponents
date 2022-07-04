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
{*  XPABOUTW.PAS                                         *}
{*********************************************************}
{* XMLPartner: About box - Windows version               *}
{*********************************************************}

{$I XpDefine.inc}

unit XpAboutw;

interface

uses
  Windows,
  forms,
  Messages,
  Graphics,
  Controls,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Classes,
  SysUtils;

type
  TXpAboutBox = class(TForm)
    Bevel2: TBevel;
    Panel1: TPanel;
    Image1: TImage;
    ProgramName: TLabel;
    VersionNumber: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblTurboLink: TLabel;
    lblNewsGeneral: TLabel;
    Label7: TLabel;
    lblNewsSpecific: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    OKButton: TButton;
    Bevel3: TBevel;
    Label13: TLabel;
    Label14: TLabel;
    lblFreeUpdateCenter: TLabel;
    lblTurboPowerLive: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure lblTurboLinkClick(Sender: TObject);
    procedure lblFreeUpdateCenterClick(Sender: TObject);
    procedure lblTurboPowerLiveClick(Sender: TObject);
    procedure lblTurboLinkMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure lblNewsGeneralClick(Sender: TObject);
    procedure lblNewsSpecificClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    IsServer : boolean;
  end;

var
  XpAboutBox: TXpAboutBox;

implementation

{$R *.dfm}

uses
  ShellAPI,
  XpBase;

resourcestring
  cBrowserError = 'Unable to start web browser. Make sure you have it properly setup on your system.';

{===WWW Shell Routines===============================================}
procedure ShellToWWW;
  {-Shell out to TurboPower WWW site}
resourcestring
  EX_Error = 'Unable to start web browser. Make sure you have it properly setup on your system.';
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage(EX_Error);
end;
{====================================================================}
procedure TXpAboutBox.OKButtonClick(Sender : TObject);
begin
  Close;
end;

procedure TXpAboutBox.FormActivate(Sender: TObject);
begin
  ProgramName.Caption := XpProductName;
  VersionNumber.Caption := Format('Version %5.4f %s',
      [XpVersionNumber / 10000.0,
       XPReleaseString]);
end;

procedure TXpAboutBox.lblTurboLinkClick(Sender: TObject);
begin
  ShellToWWW;
end;

procedure TXpAboutBox.lblFreeUpdateCenterClick(Sender : TObject);
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com/updates', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage(cBrowserError);
end;

procedure TXpAboutBox.lblTurboPowerLiveClick(Sender : TObject);
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com/tpslive', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage(cBrowserError);
end;

procedure TXpAboutBox.lblTurboLinkMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.Style := [fsUnderline];
end;

procedure TXpAboutBox.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblTurboLink.Font.Style := [];
  lblFreeUpdateCenter.Font.Style := [];
  lblTurboPowerLive.Font.Style := [];
  lblNewsGeneral.Font.Style := [];
  lblNewsSpecific.Font.Style := [];
end;

procedure TXpAboutBox.lblNewsGeneralClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'news://news.turbopower.com', '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage(cBrowserError);
end;

procedure TXpAboutBox.lblNewsSpecificClick(Sender: TObject);
begin
  if ShellExecute(0, 'open',
                  'news://news.turbopower.com/turbopower.public.support.xmlpartner',
                  '', '', SW_SHOWNORMAL) <= 32 then
    ShowMessage(cBrowserError);
end;

end.
