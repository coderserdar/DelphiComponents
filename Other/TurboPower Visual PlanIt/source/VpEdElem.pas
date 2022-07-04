{*********************************************************}
{*                VPEDELEM.PAS 1.03                      *}
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

unit VpEdElem;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,

  VpBase, VpSR, VpPrtFmt, ComCtrls;

type
  TfrmEditElement = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    btnShape: TButton;
    edName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    rgDayOffset: TRadioGroup;
    rgItemType: TRadioGroup;
    gbVisual: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    rgMeasurement: TRadioGroup;
    rgRotation: TRadioGroup;
    edTop: TEdit;
    edLeft: TEdit;
    edHeight: TEdit;
    edWidth: TEdit;
    chkVisible: TCheckBox;
    gbCaption: TGroupBox;
    btnCaptionFont: TButton;
    FontDialog1: TFontDialog;
    edCaptionText: TEdit;
    lbCaptionText: TLabel;
    edOffset: TEdit;
    udOffset: TUpDown;
    udTop: TUpDown;
    udLeft: TUpDown;
    udHeight: TUpDown;
    udWidth: TUpDown;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgItemTypeClick(Sender: TObject);
    procedure btnShapeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCaptionFontClick(Sender: TObject);
    procedure edCaptionTextChange(Sender: TObject);
    procedure rgMeasurementClick(Sender: TObject);
    procedure PosEditExit(Sender: TObject);
    procedure PosEditEnter(Sender: TObject);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
  private
    procedure SetMaxSpin(Spin: Integer);
  protected
    TheShape : TVpPrintShape;
    TheCaption : TVpPrintCaption;
    CurEdit : TEdit;

    MaxSpin : Integer;
    procedure SaveData(AnElement: TVpPrintFormatElementItem);
    procedure SetData(AnElement: TVpPrintFormatElementItem);
    procedure SetItemType(Index: Integer);
    function Validate: Boolean;
    { Private declarations }
  public
    function Execute(AnElement : TVpPrintFormatElementItem) : Boolean;
    { Public declarations }
  end;


implementation

uses VpEdShape;

{$R *.DFM}
function EvalFmt(Val : Extended) : string;
begin
  Result := FormatFloat('0.00', Val);
end;
{=====}
procedure TfrmEditElement.FormCreate(Sender: TObject);
begin
  btnShape.Enabled := False;

  gbCaption.Enabled := False;
  edCaptionText.Enabled := False;
  lbCaptionText.Enabled := False;
  btnCaptionFont.Enabled := False;
end;
{=====}
procedure TfrmEditElement.FormShow(Sender: TObject);
begin
  edName.SetFocus;
end;
{=====}
procedure TfrmEditElement.btnCaptionFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    TheCaption.Font := FontDialog1.Font;
end;
{=====}
procedure TfrmEditElement.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{=====}
procedure TfrmEditElement.btnOkClick(Sender: TObject);
begin
  if Validate then
    ModalResult := mrOk
  else begin
    ShowMessage(RSNeedElementName);
    edName.SetFocus;
    Exit;
  end;
end;
{=====}
procedure TfrmEditElement.btnShapeClick(Sender: TObject);
var
  frmEditShape: TfrmEditShape;
begin
  Application.CreateForm(TfrmEditShape, frmEditShape);
  frmEditShape.Execute(TheShape);
  frmEditShape.Free;
end;
{=====}
procedure TfrmEditElement.edCaptionTextChange(Sender: TObject);
begin
  TheCaption.Caption := edCaptionText.Text;
end;
{=====}
function TfrmEditElement.Execute(AnElement : TVpPrintFormatElementItem) : Boolean;
begin
  SetData(AnElement);
  Result := ShowModal = mrOk;
  if Result then
    SaveData(AnElement);
end;
{=====}
procedure TfrmEditElement.PosEditEnter(Sender: TObject);
begin
  CurEdit := (Sender as TEdit);
end;
{=====}
procedure TfrmEditElement.PosEditExit(Sender: TObject);
var
  ed : TEdit;
  Val : Extended;
begin
  ed := (Sender as TEdit);
  try
    Val := StrToFloat(ed.Text);
    if Val > MaxSpin then begin
      ed.Text := EvalFmt(MaxSpin);
    end else
    if Val < 0.0 then begin
      ed.Text := EvalFmt(0);
    end;
  except
    on EConvertError do begin
      ShowMessage('Please Enter a Floating Point Value');
      ed.SetFocus;
    end;
  end;
end;
{=====}
procedure TfrmEditElement.rgItemTypeClick(Sender: TObject);
begin
  SetItemType(rgItemType.ItemIndex);
end;
{=====}
procedure TfrmEditElement.rgMeasurementClick(Sender: TObject);
begin
  SetMaxSpin(rgMeasurement.ItemIndex);
end;
{=====}
procedure TfrmEditElement.SaveData(AnElement : TVpPrintFormatElementItem);
begin
  AnElement.ElementName := edName.Text;

  AnElement.DayOffset := udOffset.Position;

  AnElement.Top   := StrToFloat(edTop.Text);
  AnElement.Left  := StrToFloat(edLeft.Text);
  AnElement.Height:= StrToFloat(edHeight.Text);
  AnElement.Width := StrToFloat(edWidth.Text);

  AnElement.ItemType       :=  TVpItemType(rgItemType.ItemIndex);

  AnElement.DayOffsetUnits :=  TVpDayUnits(rgDayOffset.ItemIndex);
  AnElement.Rotation       :=  TVpRotationAngle(rgRotation.ItemIndex);
  AnElement.Measurement    :=  TVpItemMeasurement(rgMeasurement.ItemIndex);

  AnElement.Visible := chkVisible.Checked;
end;
{=====}
procedure TfrmEditElement.SetData(AnElement : TVpPrintFormatElementItem);
begin
  edName.Text := AnElement.ElementName;

  udOffset.Position := AnElement.DayOffset;

  rgItemType.ItemIndex := Ord(AnElement.ItemType);
  TheShape := AnElement.Shape;
  TheCaption := AnElement.Caption;

  rgDayOffset.ItemIndex := Ord(AnElement.DayOffsetUnits);
  rgRotation.ItemIndex := Ord(AnElement.Rotation);
  rgMeasurement.ItemIndex := Ord(AnElement.Measurement);
  SetMaxSpin(rgMeasurement.ItemIndex);

  edTop.Text := EvalFmt(AnElement.Top);
  udTop.Position := Trunc(AnElement.Top);
  edLeft.Text := EvalFmt(AnElement.Left);
  udLeft.Position := Trunc(AnElement.Left);
  edHeight.Text := EvalFmt(AnElement.Height);
  udHeight.Position := Trunc(AnElement.Height);
  edWidth.Text := EvalFmt(AnElement.Width);
  udWidth.Position := Trunc(AnElement.Width);

  edCaptionText.Text := AnElement.Caption.Caption;
  FontDialog1.Font := AnElement.Caption.Font;

  chkVisible.Checked := AnElement.Visible;
end;
{=====}
procedure TfrmEditElement.SetItemType(Index : Integer);
begin
  rgItemType.ItemIndex := Index;
  gbCaption.Enabled := False;
  edCaptionText.Enabled := False;
  lbCaptionText.Enabled := False;
  btnCaptionFont.Enabled := False;


  btnShape.Enabled := Index = 4;
  if Index = 5 then begin
    gbCaption.Enabled := True;
    edCaptionText.Enabled := True;
    lbCaptionText.Enabled := True;
    btnCaptionFont.Enabled := True;
  end;
end;
{=====}
procedure TfrmEditElement.SetMaxSpin(Spin : Integer);
begin
  case Spin of
    0: MaxSpin := 2000;
    1: MaxSpin := 100;
    2: MaxSpin := 50;
  end;

  udLeft.Max := MaxSpin;
  udTop.Max := MaxSpin;
  udHeight.Max := MaxSpin;
  udWidth.Max := MaxSpin;

end;
{=====}
procedure TfrmEditElement.UpDownClick(Sender: TObject; Button: TUDBtnType);
var
  Val, Inc : Extended;
begin
  if Sender = udLeft   then CurEdit := edLeft  ;
  if Sender = udTop    then CurEdit := edTop   ;
  if Sender = udHeight then CurEdit := edHeight;
  if Sender = udWidth  then CurEdit := edWidth ;

  Val := 0.0;
  try
    Val := StrToFloat(CurEdit.Text);
  except
    on EConvertError do begin
      ShowMessage('Please Enter a Floating Point Value');
      CurEdit.SetFocus;
    end;
  end;

  Inc := udLeft.Increment / 100;
  case Button of
    btNext: begin
      if Trunc(Val + Inc) > Trunc(Val) then
        (Sender as TUpDown).Position := (Sender as TUpDown).Position + 1;
      CurEdit.Text := FormatFloat('0.00 ', Val + Inc);
    end;
    btPrev: begin
      if Trunc(Val - Inc) < Trunc(Val) then
        (Sender as TUpDown).Position := (Sender as TUpDown).Position - 1;
      CurEdit.Text := FormatFloat('0.00 ', Val - Inc);
    end;
  end;
end;
{=====}
function TfrmEditElement.Validate : Boolean;
begin
  Result := edName.Text <> '';
end;
{=====}

end.
  
