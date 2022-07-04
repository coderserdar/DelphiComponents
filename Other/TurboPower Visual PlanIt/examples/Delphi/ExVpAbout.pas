{* ***** BEGIN LICENSE BLOCK *****                                            *}
{* Version: MPL 1.1                                                           *}
{*                                                                            *}
{* The contents of this file are subject to the Mozilla Public License        *}
{* Version 1.1 (the "License"); you may not use this file except in           *}
{* compliance with the License. You may obtain a copy of the License at       *}
{* http://www.mozilla.org/MPL/                                                *}
{*                                                                            *}
{* Software distributed under the License is distributed on an "AS IS" basis, *}
{* WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License   *}
{* for the specific language governing rights and limitations under the       *}
{* License.                                                                   *}
{*                                                                            *}
{* The Original Code is TurboPower Visual PlanIt                              *}
{*                                                                            *}
{* The Initial Developer of the Original Code is TurboPower Software          *}
{*                                                                            *}
{* Portions created by TurboPower Software Inc. are Copyright (C) 2002        *}
{* TurboPower Software Inc. All Rights Reserved.                              *}
{*                                                                            *}
{* Contributor(s):                                                            *}
{*                                                                            *}
{* ***** END LICENSE BLOCK *****                                              *}

unit ExVpAbout;

interface

uses
  Windows, Forms, Messages, Graphics, Controls, Dialogs, StdCtrls, ExtCtrls,
  Classes, SysUtils;

type
  TfrmAbout = class(TForm)
    Bevel2: TBevel;
    Panel1: TPanel;
    Image1: TImage;
    ProgramName: TLabel;
    VisitUsLabel: TLabel;
    GeneralNewsgroupsLabel: TLabel;
    lblTurboLink: TLabel;
    lblNewsGeneral: TLabel;
    SpecificNewsgroupLabel: TLabel;
    lblNewsSpecific: TLabel;
    StayUpToDateLabel: TLabel;
    InTheLabel: TLabel;
    CopyrightLabel: TLabel;
    RightsReservedLabel: TLabel;
    OKButton: TButton;
    Bevel3: TBevel;
    SignUpLabel: TLabel;
    ForThisProductLabel: TLabel;
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

    procedure Execute;
  end;

var
  frmAbout : TfrmAbout;

implementation

{$R *.dfm}

uses
  ShellAPI, VpConst;

resourcestring
  cBrowserError = 'Unable to start web browser. Make sure you have it properly setup on your system.';

{====================================================================}
procedure TfrmAbout.OKButtonClick(Sender : TObject);
begin
  Close;
end;
{=====}

procedure TfrmAbout.FormActivate(Sender: TObject);
var
  Year, Junk: Word;
begin
  ProgramName.Caption := VpProductName + ' ' + VpVersionStr;
  DecodeDate(Now, Year, junk, junk);
  CopyrightLabel.Caption := #169 + ' Copyright 2002 - ' + IntToStr(Year)
    + ', TurboPower Software Company.';
end;
{=====}

procedure TfrmAbout.lblTurboLinkClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com', '', '', SW_SHOWNORMAL) = 0 then
    ShowMessage(cBrowserError);
end;
{=====}

procedure TfrmAbout.lblFreeUpdateCenterClick(Sender : TObject);
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com/updates', '', '', SW_SHOWNORMAL) = 0 then
    ShowMessage(cBrowserError);
end;
{=====}

procedure TfrmAbout.lblTurboPowerLiveClick(Sender : TObject);
begin
  if ShellExecute(0, 'open', 'http://www.turbopower.com/tpslive', '', '', SW_SHOWNORMAL) = 0 then
    ShowMessage(cBrowserError);
end;
{=====}

procedure TfrmAbout.lblTurboLinkMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  TLabel(Sender).Font.Style := [fsUnderline];
end;
{=====}

procedure TfrmAbout.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblTurboLink.Font.Style := [];
  lblFreeUpdateCenter.Font.Style := [];
  lblTurboPowerLive.Font.Style := [];
  lblNewsGeneral.Font.Style := [];
  lblNewsSpecific.Font.Style := [];
end;
{=====}

procedure TfrmAbout.lblNewsGeneralClick(Sender: TObject);
begin
  if ShellExecute(0, 'open', 'news://news.turbopower.com', '', '', SW_SHOWNORMAL) = 0 then
    ShowMessage(cBrowserError);
end;
{=====}

procedure TfrmAbout.lblNewsSpecificClick(Sender: TObject);
begin
  if ShellExecute(0, 'open',
    'news://news.turbopower.com/turbopower.public.support.visualplanit',
    '', '', SW_SHOWNORMAL) = 0
  then
    ShowMessage(cBrowserError);
end;
{=====}

procedure TfrmAbout.Execute;
begin
  ShowModal;
end;

end.
 
