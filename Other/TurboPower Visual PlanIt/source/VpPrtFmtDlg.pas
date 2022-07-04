{*********************************************************}
{*                VPPRTFMTDLG.PAS 1.03                   *}
{*********************************************************}

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

{$I Vp.INC}

unit VpPrtFmtDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, TypInfo, ExtCtrls,

  VpPrtFmt, VpBase, VpBaseDS, VpDlg, VpDBDS, VpBDEDS, VpPrtPrv, VpPrtFmtCBox,
  VpEdFmtLst;

type

  TVpPrintFormatEditDialog = class (TVpBaseDialog)
  private
    FControlLink: TVpControlLink;
    FWindowState: TWindowState;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetControlLink(const Value: TVpControlLink);
  public
    constructor Create (AOwner : TComponent); override;
    function Execute : Boolean; override;

  published
    property ControlLink : TVpControlLink
      read FControlLink write SetControlLink;

    property WindowState : TWindowState
      read FWindowState write FWindowState default wsNormal;

    property Options;
    property Placement;
  end;


implementation


{ TVpPrintFormatEditDialog }

constructor TVpPrintFormatEditDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControlLink      := SearchControlLink (Owner);

  FPlacement.Height := 480;
  FPlacement.Width  := 640;
end;

function TVpPrintFormatEditDialog.Execute: Boolean;
var
  PrtFmtDlg : TfrmPrnFormat;
begin
  Application.CreateForm (TfrmPrnFormat, PrtFmtDlg);
  try
    DoFormPlacement(PrtFmtDlg);
    PrtFmtDlg.WindowState := WindowState;
    PrtFmtDlg.ControlLink := ControlLink;
    Result := PrtFmtDlg.Execute;

  finally
    PrtFmtDlg.Release;
  end;
end;

procedure TVpPrintFormatEditDialog.Notification (AComponent : TComponent;
                                               Operation  : TOperation);
  {-Handle new/deleted components}
begin
  inherited Notification (AComponent, Operation);

  if Operation = opRemove then begin
    {Owned components going away}
    if AComponent = FControlLink then begin
      FControlLink := nil;
    end;
  end else if Operation = opInsert then begin
    if AComponent is TVpControlLink then begin
      if not Assigned (FControlLink) then begin
        FControlLink := TVpControlLink (AComponent);
      end;
    end;
  end;
end;


procedure TVpPrintFormatEditDialog.SetControlLink(
  const Value: TVpControlLink);
begin
  if FControlLink <> Value then
    FControlLink := Value;
end;

end.
