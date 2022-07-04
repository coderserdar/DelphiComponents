{*********************************************************}
{*                VPEDSHAPE.PAS 1.03                     *}
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

unit VpEdShape;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ColorGrd, ExtCtrls, TypInfo, ComCtrls,
  VpPrtFmt;

type
  TfrmEditShape = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    cbBrushStyle: TComboBox;
    cbPenMode: TComboBox;
    cbPenStyle: TComboBox;
    cgBrushColor: TColorGrid;
    cgPenColor: TColorGrid;
    gbBrush: TGroupBox;
    gbPen: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    rgShapeType: TRadioGroup;
    udPenWidth: TUpDown;
    edPenWidth: TEdit;
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cbBrushStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure cbPenStyleDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    procedure FillBrushStyleList;
    procedure FillPenStyleList;
    procedure FillPenModeList;
  protected
    procedure SaveData(AShape: TVpPrintShape);
    procedure SetData(AShape: TVpPrintShape);

    { Private declarations }
  public
    function Execute(AShape : TVpPrintShape) : Boolean;
    { Public declarations }
  end;


implementation

{$R *.DFM}

{ TfrmEditShape }
procedure TfrmEditShape.FormCreate(Sender: TObject);
begin
  FillBrushStyleList;
  FillPenStyleList;
  FillPenModeList;
end;
{=====}
function TfrmEditShape.Execute(AShape: TVpPrintShape): Boolean;
begin
  SetData(AShape);
  Result := ShowModal = mrOk;
  if Result then
    SaveData(AShape);
end;
{=====}
procedure TfrmEditShape.btnOkClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;
{=====}
procedure TfrmEditShape.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
{=====}
procedure TfrmEditShape.FillBrushStyleList;
var
  Style : TBrushStyle;
  StyleName : string;
begin
  for Style := Low(TBrushStyle) to High(TBrushStyle) do begin
    StyleName := GetEnumName(TypeInfo(TBrushStyle), Ord(Style));
    cbBrushStyle.Items.Add(StyleName);
  end;
end;
{=====}
procedure TfrmEditShape.FillPenModeList;
var
  Mode : TPenMode;
  ModeName : string;
begin
  for Mode := Low(TPenMode) to High(TPenMode) do begin
    ModeName := GetEnumName(TypeInfo(TPenMode), Ord(Mode));
    cbPenMode.Items.Add(ModeName);
  end;
end;
{=====}
procedure TfrmEditShape.FillPenStyleList;
var
  Style : TPenStyle;
  StyleName : string;
begin
  for Style := Low(TPenStyle) to High(TPenStyle) do begin
    StyleName := GetEnumName(TypeInfo(TPenStyle), Ord(Style));
    cbPenStyle.Items.Add(StyleName);
  end;
end;
{=====}
procedure TfrmEditShape.SaveData(AShape: TVpPrintShape);
begin
  AShape.Shape := TVpShapeType(rgShapeType.ItemIndex);
  AShape.Pen.Width := udPenWidth.Position;
end;
{=====}
procedure TfrmEditShape.SetData(AShape: TVpPrintShape);
var
  StyleStr : string;
begin
  rgShapeType.ItemIndex := Ord(AShape.Shape);

  { pen settings }
  udPenWidth.Position := AShape.Pen.Width;
  cgPenColor.ForegroundIndex := cgPenColor.ColorToIndex(AShape.Pen.Color);
  StyleStr := GetEnumName(TypeInfo(TPenStyle), Ord(AShape.Pen.Style));
  cbPenStyle.ItemIndex := cbPenStyle.Items.IndexOf(StyleStr);
  StyleStr := GetEnumName(TypeInfo(TPenMode), Ord(AShape.Pen.Mode));
  cbPenMode.ItemIndex := cbPenMode.Items.IndexOf(StyleStr);

  { brush settings }
  cgBrushColor.ForegroundIndex := cgBrushColor.ColorToIndex(AShape.Brush.Color);
  StyleStr := GetEnumName(TypeInfo(TBrushStyle), Ord(AShape.Brush.Style));
  cbBrushStyle.ItemIndex := cbBrushStyle.Items.IndexOf(StyleStr);

end;
{=====}

procedure TfrmEditShape.cbBrushStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  SavePenColor, SaveBrushColor: TColor;
  Rgt: Integer;
  SaveBrushStyle: TBrushStyle;
  Item : string;
  TxtRect : TRect;
begin
  Item := cbBrushStyle.Items[Index];
  Rgt := (Rect.Bottom - Rect.Top) + Rect.Left;
  with cbBrushStyle.Canvas do
  try
    { keep old settings }
    SavePenColor := Pen.Color;
    SaveBrushColor := Brush.Color;
    SaveBrushStyle := Brush.Style;

    { draw frame }
    Pen.Color := Brush.Color;
    Brush.Color := cbBrushStyle.Brush.Color;
    Rectangle(Rect.Left, Rect.Top, Rgt, Rect.Bottom);

    { set up for drawing sample }
    Brush.Style := TBrushStyle(GetEnumValue(TypeInfo(TBrushStyle), Item));
    Pen.Color := cbBrushStyle.Font.Color;

    { special handling for bsClear }
    if Brush.Style = bsClear then
    begin
      Brush.Color := cbBrushStyle.Brush.Color;
      Brush.Style := bsSolid;
    end
    else
      Brush.Color := cbBrushStyle.Font.Color;

    { Draw sample }
    Rectangle(Rect.Left + 1, Rect.Top + 1, Rgt - 1, Rect.Bottom - 1);

    { restore settings }
    Brush.Color := SaveBrushColor;
    Brush.Style := SaveBrushStyle;
    Pen.Color := SavePenColor;
  finally
    { draw the item text }
    TxtRect := Classes.Rect(Rgt, Rect.Top, Rect.Right, Rect.Bottom);
    TextRect(TxtRect, TxtRect.Left + 1, TxtRect.Top + 1, Item);
  end;
end;
{=====}

procedure TfrmEditShape.cbPenStyleDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  SavePenColor, SaveBrushColor: TColor;
  Rgt, Top: Integer;
  SavePenStyle: TPenStyle;
  Item: string;
  TxtRect : TRect;
begin
  Item := cbPenStyle.Items[Index];
  Rgt := (Rect.Bottom - Rect.Top) * 2 + Rect.Left;
  Top := (Rect.Bottom - Rect.Top) div 2 + Rect.Top;
  with cbPenStyle.Canvas do
  try
    { keep old settings }
    SavePenColor := Pen.Color;
    SaveBrushColor := Brush.Color;
    SavePenStyle := Pen.Style;

    { draw frame }
    Pen.Color := Brush.Color;
    Rectangle(Rect.Left, Rect.Top, Rgt, Rect.Bottom);

    { set up for drawing sample }
    Brush.Color := cbPenStyle.Brush.Color;
    Pen.Color := cbPenStyle.Font.Color;
    Rectangle(Rect.Left + 1, Rect.Top + 1, Rgt - 1, Rect.Bottom - 1);

    { Draw sample }
    Pen.Style := TPenStyle(GetEnumValue(TypeInfo(TPenStyle), Item));
    Pen.Color := cbPenStyle.Font.Color;

    { Sample Line }
    MoveTo(Rect.Left + 1, Top);
    LineTo(Rgt - 1, Top);
    MoveTo(Rect.Left + 1, Top + 1);
    LineTo(Rgt - 1, Top + 1);

    { restore settings }
    Brush.Color := SaveBrushColor;
    Pen.Style := SavePenStyle;
    Pen.Color := SavePenColor;
  finally
    { draw the item text }
    TxtRect := Classes.Rect(Rgt, Rect.Top, Rect.Right, Rect.Bottom);
    TextRect(TxtRect, TxtRect.Left + 1, TxtRect.Top + 1, Item);
  end;
end;
{=====}

end.
  
