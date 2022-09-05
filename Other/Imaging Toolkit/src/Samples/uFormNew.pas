// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  27271: uFormNew.pas
//
//    Rev 1.4    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.3    04-03-2006 18:18:04  mcm    Version: IMG 2.16
// Clean-up.
//
//    Rev 1.2    26-02-2006 10:56:38  mcm
// Changed number of used decimals to 3.
//
//    Rev 1.1    19-02-2006 21:16:56  mcm    Version: IMG 2.15
// Added selection of fill color.
// Finalised unit/width/height scale.
//
//    Rev 1.0    19/02/2006 11:53:28  mcm
unit uFormNew;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
     {$ENDIF}
     mcmImageTypeDef, umcmIntE, mcmImage;

type
  TFormNewImage = class(TForm)
    btnOK                  : TButton;
    btnCancel              : TButton;
    btnHelp                : TButton;
    gbImageDimension       : TGroupBox;
    lWidth                 : TLabel;
    lHeight                : TLabel;
    lResolution            : TLabel;
    rsWidth                : TmcmRealSpin;
    rsHeight               : TmcmRealSpin;
    cbUnit                 : TComboBox;
    rsResolution           : TmcmRealSpin;
    cbResPerUnit           : TComboBox;
    gbImageCharacteristics : TGroupBox;
    lColorDepth            : TLabel;
    cbColorDepth           : TComboBox;
    lColorBk               : TLabel;
    icColorBk              : TmcmImageCtrl;
    procedure FormCreate(Sender : TObject);
    procedure btnHelpClick(Sender : TObject);
    procedure cbUnitChange(Sender : TObject);
    procedure cbResPerUnitChange(Sender: TObject);
    procedure icColorBkClick(Sender: TObject);
  private
    { Private declarations }
    FUnitIndex : integer;
    FResIndex  : integer;
    FFillColor : TColor;
    procedure ConvertToInch(Value : TmcmRealSpin);
    procedure ConvertToCm(Value : TmcmRealSpin);

    function  GetImageFormat : TmcmImageFormat;
    function  GetImageHeight : integer;
    function  GetImageWidth : integer;
    function  GetImageResolution : double;
    procedure SetFillColor(Value : TColor);
    procedure SetImageFormat(Value : TmcmImageFormat);
    procedure SetImageHeight(Value : integer);
    procedure SetImageWidth(Value : integer);
    procedure SetImageResolution(Value : double);
  public
    { Public declarations }
    // Get/Set image colour resolution
    property FillColor : TColor
      read   FFillColor
      write  SetFillColor;
    property ImageFormat : TmcmImageFormat
      read   GetImageFormat
      write  SetImageFormat;
    // Get/Set image height in pixels
    property ImageHeight : integer
      read   GetImageHeight
      write  SetImageHeight;
    // Get/Set image width in pixels
    property ImageWidth : integer
      read   GetImageWidth
      write  SetImageWidth;
    // Get/Set resolution in Pixels per meter
    property ImageResolution : double
      read   GetImageResolution
      write  SetImageResolution;
end;

var FormNewImage : TFormNewImage;

implementation

{$R *.DFM}

uses uFormColorSelect;

procedure TFormNewImage.FormCreate(Sender : TObject);
begin
  icColorBk.Image.ImageFormat := IF_RGBA32;
  icColorBk.Image.Width  := icColorBk.Width;
  icColorBk.Image.Height := icColorBk.Height;
  FFillColor := integer($FFFFFFFF);
  icColorBk.Image.FillRGB(FFillColor);
  FUnitIndex := 0;
  FResIndex  := 0;
  cbUnit.ItemIndex := 0;
  cbUnitChange(Self);
  cbResPerUnit.ItemIndex := 0;
  cbColorDepth.ItemIndex := 5;
end; // TFormNewImage.FormCreate.


procedure TFormNewImage.btnHelpClick(Sender : TObject);
begin
  Application.HelpContext(HelpContext);
end; // TFormNewImage.btnHelpClick


procedure TFormNewImage.cbUnitChange(Sender : TObject);
var OldHeight : double;
    OldWidth  : double;
    Res       : double;
begin
  if (FUnitIndex <> cbUnit.ItemIndex)
  then begin
       Res := GetImageResolution / 100.0; // Pixels per centimeter
       case FUnitIndex of
       1 : begin // Inch
             OldHeight := rsHeight.Value * Res * 2.54;
             OldWidth  := rsWidth.Value * Res * 2.54;
           end;
       2 : begin // Centimeter
             OldHeight := rsHeight.Value * Res;
             OldWidth  := rsWidth.Value * Res;
           end;
       else begin // Pixels
             OldHeight := rsHeight.Value;
             OldWidth  := rsWidth.Value;
           end;
       end;

       FUnitIndex := cbUnit.ItemIndex;
       case FUnitIndex of
       0 : begin // Pixels
             rsWidth.Decimals  := 0;
             rsHeight.Decimals := 0;
             rsHeight.Value := OldHeight;
             rsWidth.Value  := OldWidth;
           end;
       1 : begin // Inch
             rsWidth.Decimals  := 3;
             rsHeight.Decimals := 3;
             rsHeight.Value := OldHeight / (2.54 * Res);
             rsWidth.Value  := OldWidth / (2.54 * Res);
           end;
       2 : begin // Centimeter
             rsWidth.Decimals  := 3;
             rsHeight.Decimals := 3;
             rsHeight.Value := OldHeight / Res;
             rsWidth.Value  := OldWidth / Res;
           end;
       end;
  end;
end; // TFormNewImage.cbUnitChange.


function TFormNewImage.GetImageFormat : TmcmImageFormat;
begin
  case cbColorDepth.ItemIndex of
  0 : Result := IF_BW;
  1 : Result := IF_PAL4;
  2 : Result := IF_GREY4;
  3 : Result := IF_PAL8;
  4 : Result := IF_GREY8;
  5 : Result := IF_RGB24;
  6 : Result := IF_RGBA32;
  else Result := IF_RGB24;
  end;
end; // TFormNewImage.GetImageFormat.


function TFormNewImage.GetImageHeight : integer;
var UnitScale : double;
begin
  case cbUnit.ItemIndex of
  0 : UnitScale := 1.0; // Pixel
  1 : UnitScale := 2.54 * GetImageResolution / 100.0; // Inch
  2 : UnitScale := GetImageResolution / 100.0; // Centimeter
  else UnitScale := 1.0;
  end;
  Result := Round(rsHeight.Value * UnitScale);
end; // TFormNewImage.GetImageHeight.


function TFormNewImage.GetImageWidth : integer;
var UnitScale : double;
begin
  case cbUnit.ItemIndex of
  0 : UnitScale := 1.0; // Pixel
  1 : UnitScale := 2.54 * GetImageResolution / 100.0; // Inch
  2 : UnitScale := GetImageResolution / 100.0; // Centimeter
  else UnitScale := 1.0;
  end;
  Result := Round(rsWidth.Value * UnitScale);
end; // TFormNewImage.GetImageWidth.


function TFormNewImage.GetImageResolution : double;
begin
  case cbResPerUnit.ItemIndex of
   // Pixels / Inch
  0 : Result := rsResolution.Value * 100.0 / 2.54;
   // Pixels / cm
  else Result := rsResolution.Value * 100.0;
  end;
end; // TFormNewImage.GetImageResolution.


procedure TFormNewImage.SetFillColor(Value : TColor);
begin
  FFillColor := Value;
  icColorBk.Image.FillRGB(FFillColor);
  icColorBk.DrawImage;
end; // TFormNewImage.SetFillColor.


procedure TFormNewImage.SetImageFormat(Value : TmcmImageFormat);
begin
  case Value of
  IF_BW     : cbColorDepth.ItemIndex := 0;
  IF_PAL4   : cbColorDepth.ItemIndex := 1;
  IF_GREY4  : cbColorDepth.ItemIndex := 2;
  IF_PAL8   : cbColorDepth.ItemIndex := 3;
  IF_GREY8  : cbColorDepth.ItemIndex := 4;
  IF_RGB24  : cbColorDepth.ItemIndex := 5;
  IF_RGBA32 : cbColorDepth.ItemIndex := 6;
  else cbColorDepth.ItemIndex := 5;
  end;
end; // TFormNewImage.SetImageFormat.


procedure TFormNewImage.SetImageHeight(Value : integer);
var UnitScale : double;
begin
  case cbUnit.ItemIndex of
  0 : UnitScale := 1.0; // Pixel
  1 : UnitScale := 2.54 * GetImageResolution / 100.0; // Inch
  2 : UnitScale := GetImageResolution / 100.0; // Centimeter
  else UnitScale := 1.0;
  end;
  rsHeight.Value := Value / UnitScale;
end; // TFormNewImage.SetImageHeight.


procedure TFormNewImage.SetImageWidth(Value : integer);
var UnitScale : double;
begin
  case cbUnit.ItemIndex of
  0 : UnitScale := 1.0; // Pixel
  1 : UnitScale := 2.54 * GetImageResolution / 100.0; // Inch
  2 : UnitScale := GetImageResolution / 100.0; // Centimeter
  else UnitScale := 1.0;
  end;
  rsWidth.Value := Value / UnitScale;
end; // TFormNewImage.SetImageWidth.


procedure TFormNewImage.SetImageResolution(Value : double);
begin
  case cbResPerUnit.ItemIndex of
   // Pixels / Inch
  0 : rsResolution.Value := 2.54 * Value / 100.0;
   // Pixels / cm
  1 : rsResolution.Value := Value / 100.0;
  end;
end; // TFormNewImage.SetImageResolution.


procedure TFormNewImage.ConvertToInch(Value : TmcmRealSpin);
var TempValue : double;
begin
  // Convert cemtimeter value to inch
  if Assigned(Value)
  then begin
       TempValue := Value.Value;
       Value.MaxValue := Value.MaxValue * 2.54;
       Value.MinValue := Value.MinValue * 2.54;
       Value.Value := TempValue * 2.54;
  end;
end; // TFormNewImage.ConvertToInch.


procedure TFormNewImage.ConvertToCm(Value : TmcmRealSpin);
var TempValue : double;
begin
  // Convert inch value cemtimeter.
  if Assigned(Value)
  then begin
       TempValue := Value.Value;
       Value.MinValue := Value.MinValue / 2.54;
       Value.MaxValue := Value.MaxValue / 2.54;
       Value.Value := TempValue / 2.54;
  end;
end; // TFormNewImage.ConvertToCm.


procedure TFormNewImage.cbResPerUnitChange(Sender : TObject);
begin
  if (FResIndex <> cbResPerUnit.ItemIndex)
  then begin
       FResIndex := cbResPerUnit.ItemIndex;
       case cbResPerUnit.ItemIndex of
       0 : begin // Pixels / Inch
             ConvertToInch(rsResolution);
           end;
       1 : begin // Pixels / cm
             ConvertToCm(rsResolution);
           end;
       end;
  end;
end; // TFormNewImage.cbResPerUnitChange.


procedure TFormNewImage.icColorBkClick(Sender : TObject);
begin
  FormColorSelect := TFormColorSelect.Create(Self);
  FormColorSelect.OldColor := icColorBk.Image.Pixel[1,1];
  if (FormColorSelect.ShowModal = mrOK)
  then SetFillColor(FormColorSelect.NewColor);
  FormColorSelect.Free;
end; // TFormNewImage.icColorBkClick.

end.
