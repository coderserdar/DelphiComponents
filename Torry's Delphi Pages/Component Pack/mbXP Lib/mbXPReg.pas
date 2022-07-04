unit mbXPReg;

interface

{$I mxs.inc}

uses
  Windows, SysUtils, Classes, Controls, ImgList, Graphics,
  {$IFDEF DELPHI_6_UP} DesignIntf, DesignEditors, VCLEditors
  {$ELSE} DsgnIntf {$ENDIF}, mbXPArtCombo, mbXPBooleanCombo,
  mbXPBrushStyleCombo, mbXPFontCombo, mbXPImageComboBox, mbXPImageListCombo,
  mbXPPenStyleCombo, mbXPPenWidthCombo, mbXPSpin, mbXPSpinEdit,
  mbXPFloatSpinEdit, mbXPArrowButton, mbXPArrowEdit, mbXPBevel, mbXPImageRadio,
  mbXPImageCheck, mbXPSizeGrip, mbXPJustCombo, mbXPCheckRadio, mbXPHorizontalRuler,
  mbXPVerticalRuler, mbXPScrollButton;

type
  TmbXPImageIndexPropertyEditor = class(TIntegerProperty
    {$IFDEF DELPHI_6_UP} , ICustomPropertyListDrawing {$ENDIF})
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetImageListAt(Index: Integer): TCustomImageList; virtual;

    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer); {$IFNDEF DELPHI_6_UP} override; {$ENDIF}
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer); {$IFNDEF DELPHI_6_UP} override; {$ENDIF}
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean); {$IFNDEF DELPHI_6_UP} override; {$ENDIF}
  end;

  TmbXPControlImageIndexPropertyEditor = class(TmbXPImageIndexPropertyEditor)
  public
    function GetImageListAt (Index: Integer): TCustomImageList; override;
  end;

procedure Register;

implementation

function TmbXPImageIndexPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

function TmbXPImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
begin
  Result := nil;
end;

procedure TmbXPImageIndexPropertyEditor.GetValues(Proc: TGetStrProc);
var
  ImgList: TCustomImageList;
  I: Integer;
begin
  ImgList := GetImageListAt(0);
  if Assigned(ImgList) then
    for I := 0 to ImgList.Count-1 do
      Proc(IntToStr(I));
end;

procedure TmbXPImageIndexPropertyEditor.ListDrawValue(const Value: string;
  ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
var
  ImgList: TCustomImageList;
  X: Integer;
begin
  ImgList := GetImageListAt(0);
  ACanvas.FillRect(ARect);
  X := ARect.Left + 2;
  if Assigned(ImgList) then
   begin
    ImgList.Draw(ACanvas, X, ARect.Top + 2, StrToInt(Value));
    Inc(X, ImgList.Width);
   end;
  ACanvas.TextOut(X + 3, ARect.Top + 1, Value);
end;

procedure TmbXPImageIndexPropertyEditor.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AHeight := ACanvas.TextHeight(Value) + 2;
  if Assigned(ImgList) and (ImgList.Height + 4 > AHeight) then
    AHeight := ImgList.Height + 4;
end;

procedure TmbXPImageIndexPropertyEditor.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
var
  ImgList: TCustomImageList;
begin
  ImgList := GetImageListAt(0);
  AWidth := ACanvas.TextWidth(Value) + 4;
  if Assigned(ImgList) then
    Inc(AWidth, ImgList.Width);
end;

function TmbXPControlImageIndexPropertyEditor.GetImageListAt(Index: Integer): TCustomImageList;
var
  C: TPersistent;
begin
  Result := nil;
  C := GetComponent(Index);
  if C is TmbXPArrowButton then
   Result := (C as TmbXPArrowButton).Images;

  if C is TmbXPArrowEdit then
   Result := (C as TmbXPArrowEdit).ButtonImageList;

  if C is TmbXPImageCheck then
   Result := (C as TmbXPImageCheck).Images;

  if C is TmbXPImageRadio then
   Result := (C as TmbXPImageRadio).Images;
end;

procedure Register;
begin
 RegisterComponents('mbXP Lib', [TmbXPArrowButton]);
 RegisterPropertyEditor(TypeInfo(TImageIndex), TmbXPArrowButton, 'ImageIndex',
    TmbXPControlImageIndexPropertyEditor);

 RegisterComponents('mbXP Lib', [TmbXPArrowEdit]);
 RegisterPropertyEditor(TypeInfo(TImageIndex), TmbXPArrowEdit, 'ButtonImageIndex',
    TmbXPControlImageIndexPropertyEditor);

 RegisterComponents('mbXP Lib', [TmbXPArtCombo]);
 RegisterComponents('mbXP Lib', [TmbXPBevel]);
 RegisterComponents('mbXP Lib', [TmbXPBooleanCombo]);
 RegisterComponents('mbXP Lib', [TmbXPBrushStyleCombo]);
 RegisterComponents('mbXP Lib', [TmbXPFloatSpinEdit]);
 RegisterComponents('mbXP Lib', [TmbXPFontCombo]);
 RegisterComponents('mbXP Lib', [TmbXPHorizontalRuler]);

 RegisterComponents('mbXP Lib', [TmbXPImageCheck]);
 RegisterPropertyEditor(TypeInfo(TImageIndex), TmbXPImageCheck, 'ImageIndex',
    TmbXPControlImageIndexPropertyEditor);

 RegisterComponents('mbXP Lib', [TmbXPImageComboBox]);
 RegisterComponents('mbXP Lib', [TmbXPImageListCombo]);

 RegisterComponents('mbXP Lib', [TmbXPImageRadio]);
 RegisterPropertyEditor(TypeInfo(TImageIndex), TmbXPImageRadio, 'ImageIndex',
    TmbXPControlImageIndexPropertyEditor);

 RegisterComponents('mbXP Lib', [TmbXPPenStyleCombo]);
 RegisterComponents('mbXP Lib', [TmbXPPenWidthCombo]);
 RegisterComponents('mbXP Lib', [TmbXPSpinButton]);
 RegisterComponents('mbXP Lib', [TmbXPSpinEdit]);
 RegisterComponents('mbXP Lib', [TmbXPVerticalRuler]);
 RegisterComponents('mbXP Lib', [TmbXPCheckRadio]);
 RegisterComponents('mbXP Lib', [TmbXPJustCombo]);
 RegisterComponents('mbXP Lib', [TmbXPScrollButton]);
 RegisterComponents('mbXP Lib', [TmbXPSizeGrip]);
end;

end.
