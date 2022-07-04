{*********************************************************}
{*                VPPRTPRVDLG.PAS 1.03                   *}
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

unit VpPrtPrvDlg;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  {$IFDEF VERSION6} Variants, {$ENDIF}
  VpMisc,
  VpBase,
  VpException,
  VpData,
  VpPrtPrv,
  VpSR,
  VpBaseDS,
  VpDlg,
  Buttons,
  VpPrtFmtCBox,
  Printers, ImgList, ComCtrls, ToolWin, ActnList;

type
  TVpPrintPreviewDialog = class;

  TfrmPrintPreview = class (TForm)
      Panel1                 : TPanel;
      cboxZoom               : TComboBox;
      VpPrintPreview1        : TVpPrintPreview;
      VpPrintFormatComboBox1 : TVpPrintFormatComboBox;
      ToolBar1: TToolBar;
      btnPrint: TToolButton;
      ToolButton3: TToolButton;
      btnFirstPage: TToolButton;
      btnPrevPage: TToolButton;
      btnNextPage: TToolButton;
      btnLastPage: TToolButton;
      imMain: TImageList;
      ToolButton8: TToolButton;
      actMain: TActionList;
      actPrint: TAction;
      actFirstPage: TAction;
      actPrevPage: TAction;
      actNextPage: TAction;
      actLastPage: TAction;
      actCancel: TAction;
      btnCancel: TSpeedButton;

      procedure FormCreate (Sender : TObject);
      procedure OKBtnClick (Sender : TObject);
      procedure btnFirstPageClick(Sender: TObject);
      procedure btnPrevPageClick(Sender: TObject);
      procedure btnNextPageClick(Sender: TObject);
      procedure btnLastPageClick(Sender: TObject);
      procedure cboxZoomChange(Sender: TObject);
      procedure actPrintExecute(Sender: TObject);
      procedure actFirstPageExecute(Sender: TObject);
      procedure actPrevPageExecute(Sender: TObject);
      procedure actNextPageExecute(Sender: TObject);
      procedure actLastPageExecute(Sender: TObject);
      procedure actMainUpdate(Action: TBasicAction; var Handled: Boolean);
      procedure actCancelExecute(Sender: TObject);
      procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);

    private
      procedure SetCaptions;

    public
      Resource   : TVpResource;
      Contact    : TVpContact;
      ReturnCode : TVpEditorReturnCode;
  end;

  TVpPrintPreviewDialog = class (TVpBaseDialog)
    private
      FControlLink  : TVpControlLink;
      FAutoPrint    : Boolean;
      FBottomMargin : Extended;
      FEndDate      : TDateTime;
      FLeftMargin   : Extended;
      FMarginUnits  : TVpItemMeasurement;
      FRightMargin  : Extended;
      FStartDate    : TDateTime;
      FTopMargin    : Extended;
      FZoomFactor   : TVpPPZoomFactor;
      FWindowState  : TWindowState;
      FPrinter      : TPrinter;

    protected
      procedure Notification (AComponent : TComponent;
                              Operation  : TOperation); override;
      procedure SetAutoPrint (const v : Boolean);
      procedure SetBottomMargin (const v : Extended);
      procedure SetControlLink (const v : TVpControlLink);
      procedure SetEndDate (const v : TDateTime);
      procedure SetLeftMargin (const v : Extended);
      procedure SetMarginUnits (const v : TVpItemMeasurement);
      procedure SetRightMargin (const v : Extended);
      procedure SetStartDate (const v : TDateTime);
      procedure SetTopMargin (const v : Extended);
      procedure SetZoomFactor (const v : TVpPPZoomFactor);

    public
      constructor Create (AOwner : TComponent); override;
      function Execute : Boolean; override;

      property Printer : TPrinter read FPrinter write FPrinter;

    published
      property AutoPrint : Boolean read FAutoPrint write SetAutoPrint
               default False;
      property BottomMargin : Extended
               read FBottomMargin write SetBottomMargin;
      property ControlLink : TVpControlLink
               read FControlLink write SetControlLink;
      property EndDate : TDateTime read FEndDate write SetEndDate;
      property LeftMargin : Extended read FLeftMargin write SetLeftMargin;
      property MarginUnits : TVpItemMeasurement
               read FMarginUnits write SetMarginUnits default imInches;
      property RightMargin : Extended read FRightMargin write SetRightMargin;
      property StartDate : TDateTime read FStartDate write SetStartDate;
      property TopMargin : Extended read FTopMargin write SetTopMargin;
      property WindowState : TWindowState read FWindowState write FWindowState
               default wsNormal;
      property ZoomFactor : TVpPPZoomFactor
               read FZoomFactor write SetZoomFactor default zfFitToControl;

      property DataStore;
      property Options;
      property Placement;
  end;

implementation

{$R *.dfm}

procedure TfrmPrintPreview.FormCreate(Sender: TObject);
begin
  ReturnCode := rtAbandon;
  SetCaptions;
end;

procedure TfrmPrintPreview.SetCaptions;
begin
  Self.Caption := RSDlgPrintPreview;
  actCancel.Caption := RSCancelBtn;
end;


procedure TfrmPrintPreview.OKBtnClick(Sender: TObject);
begin
  ReturnCode := rtCommit;
  Close;
end;

constructor TVpPrintPreviewDialog.Create (AOwner : TComponent);
begin
  inherited Create (AOwner);
  FPlacement.Height := 480;
  FPlacement.Width  := 640;
  StartDate         := Now;
  EndDate           := Now + 7;
  FZoomFactor       := zfFitToControl;
  FWindowState      := wsNormal;
  FAutoPrint        := False;
  FControlLink      := SearchControlLink (Owner);
  FPrinter          := Printer;
end;

function TVpPrintPreviewDialog.Execute : Boolean;
var
  EditForm : TfrmPrintPreview;
begin
  Result := False;
  Application.CreateForm (TfrmPrintPreview, EditForm);
  try
    DoFormPlacement(EditForm);
    EditForm.WindowState := WindowState;
    EditForm.VpPrintPreview1.ControlLink := ControlLink;
    EditForm.VpPrintFormatComboBox1.ControlLink := ControlLink;
    EditForm.VpPrintPreview1.StartDate := StartDate;
    EditForm.VpPrintPreview1.EndDate := EndDate;
    EditForm.VpPrintPreview1.ZoomFactor := ZoomFactor;
    EditForm.cboxZoom.ItemIndex := Integer (ZoomFactor);
    EditForm.VpPrintPreview1.Printer := Printer;
    EditForm.VpPrintPreview1.FirstPage;
    EditForm.ShowModal;
    if EditForm.ReturnCode = rtCommit then begin
      Result := True;
    end;
    if AutoPrint and Assigned (FControlLink) and Result then begin
      Printer.BeginDoc;
      try
        FControlLink.Printer.Print (Printer, StartDate, EndDate);
      finally
        Printer.EndDoc;
      end;
    end;
  finally
    EditForm.Release;
  end;
end;

procedure TVpPrintPreviewDialog.Notification (AComponent : TComponent;
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

procedure TVpPrintPreviewDialog.SetAutoPrint (const v : Boolean);
begin
  if v <> FAutoPrint then
    FAutoPrint := v;
end;

procedure TVpPrintPreviewDialog.SetBottomMargin (const v : Extended);
begin
  if v <> FBottomMargin then
    FBottomMargin := v;
end;

procedure TVpPrintPreviewDialog.SetControlLink (const v : TVpControlLink);
begin
  if FControlLink <> v then
    FControlLink := v;
end;

procedure TVpPrintPreviewDialog.SetEndDate (const v : TDateTime);
begin
  if v <> FEndDate then
    FEndDate := v;
end;

procedure TVpPrintPreviewDialog.SetLeftMargin (const v : Extended);
begin
  if v <> FLeftMargin then
    FLeftMargin := v;
end;

procedure TVpPrintPreviewDialog.SetMarginUnits (const v : TVpItemMeasurement);
begin
  if v <> FMarginUnits then
    FMarginUnits := v;
end;

procedure TVpPrintPreviewDialog.SetRightMargin (const v : Extended);
begin
  if v <> FRightMargin then
    FRightMargin := v;
end;

procedure TVpPrintPreviewDialog.SetStartDate (const v : TDateTime);
begin
  if v <> FStartDate then
    FStartDate := v;
end;

procedure TVpPrintPreviewDialog.SetTopMargin (const v : Extended);
begin
  if v <> FTopMargin then
    FTopMargin := v;
end;

procedure TVpPrintPreviewDialog.SetZoomFactor (const v : TVpPPZoomFactor);
begin
  if v <> FZoomFactor then
    FZoomFactor := v;
end;

{ TfrmPrintPreview }
procedure TfrmPrintPreview.btnFirstPageClick(Sender: TObject);
begin
  VpPrintPreview1.FirstPage;
end;

procedure TfrmPrintPreview.btnPrevPageClick(Sender: TObject);
begin
  VpPrintPreview1.PrevPage;
end;

procedure TfrmPrintPreview.btnNextPageClick(Sender: TObject);
begin
  VpPrintPreview1.NextPage;
end;

procedure TfrmPrintPreview.btnLastPageClick(Sender: TObject);
begin
  VpPrintPreview1.LastPage;
end;

procedure TfrmPrintPreview.cboxZoomChange(Sender: TObject);
begin
  case cboxZoom.ItemIndex of
    0 : VpPrintPreview1.ZoomFactor := zfFitToControl;
    1 : VpPrintPreview1.ZoomFactor := zfActualSize;
    2 : VpPrintPreview1.ZoomFactor := zf25Percent;
    3 : VpPrintPreview1.ZoomFactor := zf33Percent;
    4 : VpPrintPreview1.ZoomFactor := zf50Percent;
    5 : VpPrintPreview1.ZoomFactor := zf67Percent;
    6 : VpPrintPreview1.ZoomFactor := zf75Percent;
  end;
end;

procedure TfrmPrintPreview.actPrintExecute(Sender: TObject);
begin
  ReturnCode := rtCommit;
  Close;
end;

procedure TfrmPrintPreview.actFirstPageExecute(Sender: TObject);
begin
  VpPrintPreview1.FirstPage;
end;

procedure TfrmPrintPreview.actPrevPageExecute(Sender: TObject);
begin
  VpPrintPreview1.PrevPage;
end;

procedure TfrmPrintPreview.actNextPageExecute(Sender: TObject);
begin
  VpPrintPreview1.NextPage;
end;

procedure TfrmPrintPreview.actLastPageExecute(Sender: TObject);
begin
  VpPrintPreview1.LastPage;
end;

procedure TfrmPrintPreview.actMainUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if VpPrintPreview1.IsFirstPage then begin
    actFirstPage.Enabled := False;
    actPrevPage.Enabled := False;
  end else begin
    actFirstPage.Enabled := True;
    actPrevPage.Enabled := True;
  end;

  if VpPrintPreview1.IsLastPage then begin
    actLastPage.Enabled := False;
    actNextPage.Enabled := False;
  end else begin
    actLastPage.Enabled := True;
    actNextPage.Enabled := True;
  end;
end;

procedure TfrmPrintPreview.actCancelExecute(Sender: TObject);
begin
  ReturnCode := rtAbandon;
  Close;
end;

procedure TfrmPrintPreview.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    actCancel.Execute;
end;

end.
  
