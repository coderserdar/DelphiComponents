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

unit EdElement;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls,

  VpBase, VpPrtFmt;

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
    spedOffset: TSpinEdit;
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
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgItemTypeClick(Sender: TObject);
    procedure btnShapeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCaptionFontClick(Sender: TObject);
    procedure edCaptionTextChange(Sender: TObject);
  protected
    TheShape : TVpPrintShape;
    TheCaption : TVpPrintCaption;
    procedure SaveData(AnElement: TVpPrintFormatElementItem);
    procedure SetData(AnElement: TVpPrintFormatElementItem);
    procedure SetItemType(Index: Integer);
    function Validate: Boolean;
    { Private declarations }
  public
    function Execute(AnElement : TVpPrintFormatElementItem) : Boolean;
    { Public declarations }
  end;

var
  frmEditElement: TfrmEditElement;

implementation

uses EdShape;

{$R *.DFM}

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
    ShowMessage('Please supply an Element Name');
    edName.SetFocus;
    Exit;
  end;
end;
{=====}
procedure TfrmEditElement.btnShapeClick(Sender: TObject);
begin
  frmEditShape.Execute(TheShape);
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
procedure TfrmEditElement.rgItemTypeClick(Sender: TObject);
begin
  SetItemType(rgItemType.ItemIndex);
end;
{=====}
procedure TfrmEditElement.SaveData(AnElement : TVpPrintFormatElementItem);
begin
  AnElement.ElementName := edName.Text;

  AnElement.DayOffset := spedOffset.Value;

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

  spedOffset.Value := AnElement.DayOffset;

  edTop.Text := FloatToStr(AnElement.Top);
  edLeft.Text := FloatToStr(AnElement.Left);
  edHeight.Text := FloatToStr(AnElement.Height);
  edWidth.Text := FloatToStr(AnElement.Width);

  rgItemType.ItemIndex := Ord(AnElement.ItemType);
  TheShape := AnElement.Shape;
  TheCaption := AnElement.Caption;

  rgDayOffset.ItemIndex := Ord(AnElement.DayOffsetUnits);
  rgRotation.ItemIndex := Ord(AnElement.Rotation);
  rgMeasurement.ItemIndex := Ord(AnElement.Measurement);

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
function TfrmEditElement.Validate : Boolean;
begin
  Result := edName.Text <> '';
end;
{=====}
procedure TfrmEditElement.btnCaptionFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then
    TheCaption.Font := FontDialog1.Font;
end;
{=====}
procedure TfrmEditElement.edCaptionTextChange(Sender: TObject);
begin
  TheCaption.Caption := edCaptionText.Text;
end;
{=====}

end.
 
