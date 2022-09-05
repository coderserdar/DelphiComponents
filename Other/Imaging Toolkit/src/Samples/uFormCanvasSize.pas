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
// $Log:  27357: uFormCanvasSize.pas 
//
//    Rev 1.1    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.0    04-03-2006 18:24:24  mcm    Version: IMG 2.16
// Initial revision
//
//    Rev 1.1    19-02-2006 21:16:56  mcm    Version: IMG 2.15
// Added selection of fill color.
// Finalised unit/width/height scale.
//
//    Rev 1.0    19/02/2006 11:53:28  mcm
unit uFormCanvasSize;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      Buttons, StdCtrls, ComCtrls, ToolWin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls, Vcl.ComCtrls,
      Vcl.ToolWin, 
     {$ENDIF}
     mcmImageTypeDef, mcmImage, umcmIntE;

type
  TFormCanvasSize = class(TForm)
    btnOK                  : TButton;
    btnCancel              : TButton;
    btnHelp                : TButton;

    gbNewDimension         : TGroupBox;
    lWidth                 : TLabel;
    lHeight                : TLabel;
    rsWidth                : TmcmRealSpin;
    rsHeight               : TmcmRealSpin;
    cbUnit                 : TComboBox;
    cbLockAspect           : TCheckBox;
    lOrgAspect             : TLabel;

    gbImageCharacteristics : TGroupBox;
    lColorBk               : TLabel;
    icColorBk              : TmcmImageCtrl;

    gbCurrentDimensions    : TGroupBox;
    lCurWidth              : TLabel;
    lCurHeight             : TLabel;
    lWidthVal              : TLabel;
    lHeightVal             : TLabel;

    gbPlacement            : TGroupBox;
    lLeft                  : TLabel;
    lTop                   : TLabel;
    rsLeft                 : TmcmRealSpin;
    rsTop                  : TmcmRealSpin;
    ilPlacement            : TImageList;
    tbPlacement            : TToolBar;
    tbLeftTop              : TToolButton;
    tbCenterTop            : TToolButton;
    tbRightTop             : TToolButton;
    tbLeftCentre           : TToolButton;
    tbCentre               : TToolButton;
    tbRightCentre          : TToolButton;
    tbLeftBottom           : TToolButton;
    tbCentreBottom         : TToolButton;
    tbRightBotom           : TToolButton;

    procedure FormCreate(Sender : TObject);
    procedure btnHelpClick(Sender : TObject);
    procedure cbUnitChange(Sender : TObject);
    procedure icColorBkClick(Sender : TObject);
    procedure rsWidthChange(Sender : TObject);
    procedure rsHeightChange(Sender : TObject);
    procedure rsLeftChange(Sender : TObject);
    procedure rsTopChange(Sender : TObject);
    procedure tbLeftTopClick(Sender : TObject);
    procedure tbCenterTopClick(Sender : TObject);
    procedure tbRightTopClick(Sender : TObject);
    procedure tbLeftCentreClick(Sender : TObject);
    procedure tbCentreClick(Sender : TObject);
    procedure tbRightCentreClick(Sender : TObject);
    procedure tbLeftBottomClick(Sender : TObject);
    procedure tbCentreBottomClick(Sender : TObject);
    procedure tbRightBotomClick(Sender : TObject);
  private
    { Private declarations }
    FUnitIndex  : integer;
    FResIndex   : integer;
    FFillColor  : TColor;
    FImage      : TmcmImage;
    FAspect     : double;
    FUpdating   : boolean;

    function  GetImage : TmcmImage;
    function  GetImageHeight : integer;
    function  GetImageWidth : integer;
    function  GetLeftOffset : integer;
    function  GetTopOffset : integer;
    function  GetXResScale : double;
    function  GetYResScale : double;
    procedure SetFillColor(const Value : TColor);
    procedure SetImage(const Value : TmcmImage);
    procedure SetLeftOffset(const Value : integer);
    procedure SetTopOffset(const Value : integer);
    procedure SetOffset(const x, y : integer);
    procedure RealSpinUnitChanged(RealSpin : TmcmRealSpin; Resolution : double);
  public
    { Public declarations }
    // Get/Set image colour resolution
    property FillColor : TColor
      read   FFillColor
      write  SetFillColor;
    // Get/Set image
    property Image : TmcmImage
      read   GetImage
      write  SetImage;
    property ImageHeight : integer
      read   GetImageHeight;
    property ImageWidth : integer
      read   GetImageWidth;
    property LeftOffset : integer
      read   GetLeftOffset
      write  SetLeftOffset;
    property TopOffset : integer
      read   GetTopOffset
      write  SetTopOffset;
end;

var FormCanvasSize : TFormCanvasSize;

implementation

{$R *.DFM}

uses uFormColorSelect;

procedure TFormCanvasSize.FormCreate(Sender : TObject);
begin
  FImage := Nil;
  FUpdating := False;
  icColorBk.Image.ImageFormat := IF_RGBA32;
  icColorBk.Image.Width  := icColorBk.Width;
  icColorBk.Image.Height := icColorBk.Height;
  FFillColor := integer($FFFFFFFF);
  icColorBk.Image.FillRGB(FFillColor);
  FUnitIndex := 0;
  FResIndex  := 0;
  cbUnit.ItemIndex := 0;
  cbUnitChange(Self);
  rsLeft.Value := 0;
  rsTop.Value  := 0;
end; // TFormNewImage.FormCreate.


procedure TFormCanvasSize.btnHelpClick(Sender : TObject);
begin
  Application.HelpContext(HelpContext);
end; // TFormNewImage.btnHelpClick


function TFormCanvasSize.GetImageHeight : integer;
begin
  Result := rsHeight.Tag;
end; // TFormCanvasSize.GetImageHeight.


function TFormCanvasSize.GetImageWidth : integer;
begin
  Result := rsWidth.Tag;
end; // TFormCanvasSize.GetImageWidth.


function TFormCanvasSize.GetXResScale : double;
begin
  case cbUnit.ItemIndex of
  1 : Result := 2.54 * GetImage.XResolution / 100.0; // Pixels per inch
  2 : Result := GetImage.XResolution / 100.0; // Pixels per centimeter
  else Result := 1; // Pixels
  end;
end; // TFormCanvasSize.GetXResScale.


function TFormCanvasSize.GetYResScale : double;
begin
  case cbUnit.ItemIndex of
  1 : Result := 2.54 * GetImage.YResolution / 100.0; // Pixels per inch
  2 : Result := GetImage.YResolution / 100.0; // Pixels per centimeter
  else Result := 1; // Pixels
  end;
end; // TFormCanvasSize.GetYResScale.


procedure TFormCanvasSize.RealSpinUnitChanged(RealSpin : TmcmRealSpin; Resolution : double);
begin
  if (Resolution > 0.0)
  then begin
  (*
       Scale := Resolution; // Pixels per centimeter
       case FUnitIndex of
       1 : begin // Inch
             OldValue := RealSpin.Value * Scale * 2.54;
           end;
       2 : begin // Centimeter
             OldValue := RealSpin.Value * Scale;
           end;
       else begin // Pixels
             OldValue := RealSpin.Value;
           end;
       end;
  *)
       case cbUnit.ItemIndex of
       0 : begin // Pixels
             RealSpin.Increment := 1.0;
             RealSpin.Value := RealSpin.Tag;
             RealSpin.Decimals  := 0;
           end;
       1 : begin // Inch
             RealSpin.Decimals  := 3;
             RealSpin.Increment := 0.1;
             RealSpin.Value := RealSpin.Tag / Resolution;
           end;
       2 : begin // Centimeter
             RealSpin.Decimals  := 3;
             RealSpin.Increment := 0.1;
             RealSpin.Value := RealSpin.Tag / Resolution;
           end;
       end;
  end;
end; // TFormCanvasSize.RealSpinUnitChanged.


procedure TFormCanvasSize.cbUnitChange(Sender : TObject);
var XResScale : double;
    YResScale : double;
begin
  if (FImage = Nil)
  then Exit;

  if (FUnitIndex <> cbUnit.ItemIndex)
  then begin
       FUpdating := True;

       XResScale := GetXResScale;
       YResScale := GetYResScale;

       RealSpinUnitChanged(rsWidth, XResScale);
       RealSpinUnitChanged(rsHeight, YResScale);
       RealSpinUnitChanged(rsLeft, XResScale);
       RealSpinUnitChanged(rsTop, YResScale);

       case cbUnit.ItemIndex of
       0 : begin // Pixels
             lHeightVal.Caption := IntToStr(FImage.Height);
             lWidthVal.Caption := IntToStr(FImage.Width);
           end;
       else begin // Inch & Centimeter
             if (YResScale > 0.0)
             then lHeightVal.Caption := FloatToStrF(FImage.Height / YResScale, ffFixed, 9, 2);
             if (XResScale > 0.0)
             then lWidthVal.Caption := FloatToStrF(FImage.Width / XResScale, ffFixed, 9, 2);
           end;
       end;


       case cbUnit.ItemIndex of
       0 : begin // Pixels
       (*
             FxScale := 1;
             FyScale := 1;
             rsWidth.Increment := 1.0;
             rsWidth.Value  := FNewWidth;
             rsWidth.Decimals  := 0;

             rsHeight.Increment := 1.0;
             rsHeight.Value := FNewHeight;
             rsHeight.Decimals := 0;
             lHeightVal.Caption := IntToStr(FImage.Height);
             lWidthVal.Caption := IntToStr(FImage.Width);
       *)
           end;
       1 : begin // Inch
       (*
             FxScale := 2.54 * GetImage.XResolution / 100.0; // Pixels per inch
             FyScale := 2.54 * GetImage.YResolution / 100.0; // Pixels per inch
             rsWidth.Decimals  := 3;
             rsWidth.Increment := 0.1;
             rsWidth.Value  := OldWidth / FxScale;

             rsHeight.Decimals := 3;
             rsHeight.Increment := 0.1;
             rsHeight.Value := OldHeight / FyScale;
             if (FyScale > 0.0)
             then lHeightVal.Caption := FloatToStrF(FImage.Height / FyScale, ffFixed, 9, 2);
             if (FxScale > 0.0)
             then lWidthVal.Caption := FloatToStrF(FImage.Width / FxScale, ffFixed, 9, 2);
       *)
           end;
       2 : begin // Centimeter
       (*
             FxScale := GetImage.XResolution / 100.0; // Pixels per centimeter
             FyScale := GetImage.YResolution / 100.0; // Pixels per centimeter
             rsWidth.Decimals  := 3;
             rsWidth.Increment := 0.1;
             rsWidth.Value  := OldWidth / FxScale;

             rsHeight.Decimals := 3;
             rsHeight.Increment := 0.1;
             rsHeight.Value := OldHeight / FyScale;
             if (FyScale > 0.0)
             then lHeightVal.Caption := FloatToStrF(FImage.Height / FyScale, ffFixed, 9, 2);
             if (FxScale > 0.0)
             then lWidthVal.Caption := FloatToStrF(FImage.Width / FxScale, ffFixed, 9, 2);
       *)
           end;
       end;
       FUnitIndex := cbUnit.ItemIndex;
       FUpdating := False;
  end;
end; // TFormNewImage.cbUnitChange.


function TFormCanvasSize.GetImage : TmcmImage;
begin
  Result := FImage;
end; // TFormNewImage.GetImage.


procedure TFormCanvasSize.SetFillColor(const Value : TColor);
begin
  FFillColor := Value;
  icColorBk.Image.FillRGB(FFillColor);
  icColorBk.DrawImage;
end; // TFormNewImage.SetFillColor.


procedure TFormCanvasSize.SetImage(const Value : TmcmImage);
begin
  FImage := Value;
  if (FImage <> Nil)
  then begin
       cbUnit.Enabled := (FImage.XResolution > 0) and (FImage.YResolution > 0);

       rsWidth.Tag  := FImage.Width;
       rsHeight.Tag := FImage.Height;
       lHeightVal.Caption := IntToStr(rsHeight.Tag);
       lWidthVal.Caption  := IntToStr(rsWidth.Tag);
       rsHeight.Value := rsHeight.Tag;
       rsWidth.Value  := rsWidth.Tag;

       if (rsHeight.Tag > 0)
       then FAspect := 1.0 * rsWidth.Tag / rsHeight.Tag
       else FAspect := 1.0;
       lOrgAspect.Caption := FloatToStrF(FAspect, ffFixed, 9, 2);

  end;
end; // TFormNewImage.SetImage.


procedure TFormCanvasSize.icColorBkClick(Sender : TObject);
begin
  FormColorSelect := TFormColorSelect.Create(Self);
  FormColorSelect.OldColor := icColorBk.Image.Pixel[1,1];
  if (FormColorSelect.ShowModal = mrOK)
  then SetFillColor(FormColorSelect.NewColor);
  FormColorSelect.Free;
end; // TFormNewImage.icColorBkClick.


procedure TFormCanvasSize.rsWidthChange(Sender : TObject);
begin
  if cbLockAspect.Checked and Not(FUpdating)
  then begin
       FUpdating := True;
       rsHeight.Value := rsWidth.Value / FAspect;
       FUpdating := False;
  end;
  rsWidth.Tag := Round(GetXResScale * rsWidth.Value);
end; // TFormCanvasSize.rsWidthChange.


procedure TFormCanvasSize.rsHeightChange(Sender : TObject);
begin
  if cbLockAspect.Checked and Not(FUpdating)
  then begin
       FUpdating := True;
       rsWidth.Value := rsHeight.Value * FAspect;
       FUpdating := False;
  end;
  rsHeight.Tag := Round(GetYResScale * rsHeight.Value);
end; // TFormCanvasSize.rsHeightChange.


function TFormCanvasSize.GetLeftOffset : integer;
begin
  Result := rsLeft.Tag;
end; // TFormCanvasSize.GetLeftOffset.


function TFormCanvasSize.GetTopOffset : integer;
begin
  Result := rsTop.Tag;
end; // TFormCanvasSize.GetTopOffset.


procedure TFormCanvasSize.SetLeftOffset(const Value : integer);
begin
  rsLeft.Value := Value / GetXResScale;
  rsLeft.Tag   := Value;
end; // TFormCanvasSize.SetLeftOffset.


procedure TFormCanvasSize.SetTopOffset(const Value : integer);
begin
  rsTop.Value  := Value / GetYResScale;
  rsTop.Tag    := Value;
end; // TFormCanvasSize.SetTopOffset.


procedure TFormCanvasSize.SetOffset(const x, y : integer);
begin
  rsLeft.Value := x / GetXResScale;
  rsLeft.Tag   := x;
  rsTop.Value  := y / GetYResScale;
  rsTop.Tag    := y;
end; // TFormCanvasSize.SetOffset.


procedure TFormCanvasSize.rsLeftChange(Sender : TObject);
begin
  rsLeft.Tag := Round(rsLeft.Value * GetXResScale);
end; // TFormCanvasSize.rsLeftChange.


procedure TFormCanvasSize.rsTopChange(Sender : TObject);
begin
  rsTop.Tag := Round(rsTop.Value * GetYResScale);
end; // TFormCanvasSize.rsTopChange.


procedure TFormCanvasSize.tbLeftTopClick(Sender : TObject);
begin
  SetOffset(0, 0);
end; // TFormCanvasSize.tbLeftTopClick.


procedure TFormCanvasSize.tbCenterTopClick(Sender : TObject);
begin
  SetOffset((rsWidth.Tag - FImage.Width) div 2, 0);
end; // TFormCanvasSize.tbCenterTopClick.


procedure TFormCanvasSize.tbRightTopClick(Sender : TObject);
begin
  SetOffset(rsWidth.Tag - FImage.Width, 0);
end; // TFormCanvasSize.tbRightTopClick.


procedure TFormCanvasSize.tbLeftCentreClick(Sender : TObject);
begin
  SetOffset(0, (rsHeight.Tag - FImage.Height) div 2);
end; // TFormCanvasSize.tbLeftCentreClick.


procedure TFormCanvasSize.tbCentreClick(Sender : TObject);
begin
  SetOffset((rsWidth.Tag - FImage.Width) div 2, (rsHeight.Tag - FImage.Height) div 2);
end; // TFormCanvasSize.tbCentreClick.


procedure TFormCanvasSize.tbRightCentreClick(Sender : TObject);
begin
  SetOffset(rsWidth.Tag - FImage.Width, (rsHeight.Tag - FImage.Height) div 2);
end; // TFormCanvasSize.tbRightCentreClick.


procedure TFormCanvasSize.tbLeftBottomClick(Sender : TObject);
begin
  SetOffset(0, rsHeight.Tag - FImage.Height);
end; // TFormCanvasSize.tbLeftBottomClick.


procedure TFormCanvasSize.tbCentreBottomClick(Sender : TObject);
begin
  SetOffset((rsWidth.Tag - FImage.Width) div 2, rsHeight.Tag - FImage.Height);
end; // TFormCanvasSize.tbCentreBottomClick.


procedure TFormCanvasSize.tbRightBotomClick(Sender : TObject);
begin
  SetOffset(rsWidth.Tag - FImage.Width, rsHeight.Tag - FImage.Height);
end; // TFormCanvasSize.tbRightBotomClick.


end.
