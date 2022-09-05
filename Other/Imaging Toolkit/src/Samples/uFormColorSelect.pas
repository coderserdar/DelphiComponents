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
// $Log:  27259: uFormColorSelect.pas
//
//    Rev 1.6    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.5    05-03-2006 13:05:34  mcm    Version: IMG 2.16
// Added context menu to select between rectangular and circullar presentation
// mode.
//
//    Rev 1.4    22/02/2006 00:09:40  mcm
//
//    Rev 1.3    19-02-2006 21:15:14  mcm
// Corrected initialization of cursor-markers on the HSV and Value image.
//
//    Rev 1.2    19/02/2006 11:56:18  mcm    Version: IMG 2.15
//
//    Rev 1.1    18-02-2006 20:05:22  mcm
//
//    Rev 1.0    18-02-2006 17:24:28  mcm
// Initial revision
unit uFormColorSelect;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      Menus, StdCtrls, ComCtrls, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
      Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls,
      Vcl.ComCtrls, Vcl.ExtCtrls,
     {$ENDIF}
     mcmImage,
     mcmShape,
     umcmIntE;

const WM_HSVCOLORCIRCLE = WM_USER  + 100;

type
  TFormColorSelect = class(TForm)
    btnOK             : TButton;
    btnCancel         : TButton;
    btnHelp           : TButton;
    pcColor           : TPageControl;

    tsHSV             : TTabSheet;
    mcmImageHSV       : TmcmImageCtrl;
    mcmShape1         : TmcmShape;
    mcmImageValue     : TmcmImageCtrl;
    msValue           : TmcmShape;
    miOldColor        : TmcmImageCtrl;
    lPrevColor        : TLabel;
    miNewColor        : TmcmImageCtrl;
    lCurrColor        : TLabel;

    gbRGB             : TGroupBox;
    lRed              : TLabel;
    lGreen            : TLabel;
    lBlue             : TLabel;
    isRed             : TmcmIntSpin;
    isGreen           : TmcmIntSpin;
    isBlue            : TmcmIntSpin;
    gbHSV             : TGroupBox;
    lHue              : TLabel;
    lSaturation       : TLabel;
    lValue            : TLabel;
    isHue             : TmcmIntSpin;
    isSaturation      : TmcmIntSpin;
    isValue           : TmcmIntSpin;
    
    pmColorSelect     : TPopupMenu;
    HSVCircular1      : TMenuItem;
    HSVRectangular1   : TMenuItem;

    procedure FormCreate(Sender : TObject);
    procedure FormClose(Sender : TObject; var Action : TCloseAction);
    procedure btnOKClick(Sender : TObject);
    procedure btnCancelClick(Sender : TObject);
    procedure btnHelpClick(Sender : TObject);
    procedure mcmImageHSVMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure mcmImageHSVMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure mcmImageValueMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    procedure mcmImageValueMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure tbRedChange(Sender : TObject);
    procedure tbGreenChange(Sender : TObject);
    procedure tbBlueChange(Sender : TObject);
    procedure tbHueChange(Sender : TObject);
    procedure tbSaturationChange(Sender : TObject);
    procedure tbValueChange(Sender : TObject);
    procedure HSVCircular1Click(Sender : TObject);
    procedure HSVRectangular1Click(Sender : TObject);
    procedure mcmImageHSVContextPopup(Sender : TObject; MousePos : TPoint; var Handled : Boolean);
  private
    // Private declarations
    FIsUpdating  : boolean;
    FEnableAlpha : boolean;
    FColorMode   : integer;
    FNewColor    : TColor;
    FOldColor    : TColor;
    Fcx          : integer;
    Fcy          : integer;
    Fpx          : integer;
    Fpy          : integer;
    FRadius      : integer;
    procedure HSVColorRectangle;
    procedure HSVColorCircle(UpdateBk : boolean);
    procedure HSValueRect;
    procedure GetCircleHSVColor(x, y : integer; UpdateHSV : boolean);
    procedure GetRectangleHSVColor(x, y : integer; UpdateHSV : boolean);
    procedure GetXYFromCircleHSV(var x, y : integer);
    procedure GetXYFromRectangleHSV(var x, y : integer);
    procedure UpdateHSVColor;
    procedure UpdateRGBColor;
    procedure SetEnableAlpha(Value : boolean);
    procedure SetNewColor(Value : TColor);
    procedure SetOldColor(Value : TColor);
    procedure UpdateCircleCursor;
    procedure UpdateValueCursor;
    procedure WMHSVColorCircle(var Message : TMessage); message WM_HSVCOLORCIRCLE;
  public
    // Public declarations
    property EnableAlpha : boolean
      read   FEnableAlpha
      write  SetEnableAlpha;
    property NewColor : TColor
      read   FNewColor
      write  SetNewColor;
    property OldColor : TColor
      read   FOldColor
      write  SetOldColor;
  end;

var FormColorSelect : TFormColorSelect;

implementation

{$R *.DFM}

uses {$IFDEF GE_DXE2}
       System.Types, System.UITypes,
     {$ENDIF}
     mcmImageTypeDef, mcmImageColor;

procedure TFormColorSelect.FormCreate(Sender : TObject);
begin
  FIsUpdating := False;

  // Previous color
  miOldColor.Image.ImageFormat := IF_RGBA32;
  miOldColor.Image.Width := miOldColor.Width - 1;
  miOldColor.Image.Height := miOldColor.Height - 1;

  // Current color.
  miNewColor.Image.ImageFormat := IF_RGBA32;
  miNewColor.Image.Width := miNewColor.Width - 1;
  miNewColor.Image.Height := miNewColor.Height - 1;

  SetOldColor(RGB(0, 0, 0));

  // Create color selection bitmaps.
  FColorMode := 1;
  if (FColorMode = 0)
  then HSVColorCircle(True)
  else HSVColorRectangle;
  HSValueRect;

  {$IFNDEF DCB3_4}
    mcmImageHSV.OnContextPopup := mcmImageHSVContextPopup;
    HSVCircular1.Checked := (FColorMode = 0);
    HSVRectangular1.Checked := (FColorMode = 1);
  {$ENDIF}

  SetOldColor(RGB(255, 255, 255));

  // Create cursor used for "Value" selection.
  msValue.Shape := ST_POLYGON;
  msValue.SetPoints([Point(5, 3),
                     Point(8, 0),
                     Point(0, 0),
                     Point(4, 4),
                     Point(4, 11),
                     Point(0, 15),
                     Point(8, 15),
                     Point(4, 11)]);
  msValue.Width := 9;
end; // TFormColorSelect.FormCreate.


procedure TFormColorSelect.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  Action := caFree;
  //FormColorSelect := Nil;
end; // TFormColorSelect.FormClose.


procedure TFormColorSelect.btnOKClick(Sender : TObject);
begin
//  ModalResult := mrOK;
end; // TFormColorSelect.btnOKClick.


procedure TFormColorSelect.btnCancelClick(Sender : TObject);
begin
//  ModalResult := mrCancel;
end; // TFormColorSelect.btnCancelClick.


procedure TFormColorSelect.btnHelpClick(Sender : TObject);
begin
  Application.HelpContext(HelpContext);
end; // TFormColorSelect.btnHelpClick.


procedure TFormColorSelect.SetEnableAlpha(Value : boolean);
begin
  FEnableAlpha := Value;
end; // TFormColorSelect.SetEnableAlpha.


procedure TFormColorSelect.SetOldColor(Value : TColor);
var Color : TColor;
begin
  FOldColor := Value;
  miOldColor.Image.FillRGB(FOldColor);
  miOldColor.DrawImage;
  SetNewColor(Value);
  if ((GetRValue(Value) + GetGValue(Value) + GetBValue(Value)) div 3 > 127)
  then Color := clBlack
  else Color := clWhite;
  if (lPrevColor.Font.Color <> Color)
  then lPrevColor.Font.Color := Color;
  UpdateValueCursor;
end; // TFormColorSelect.SetOldColor.


procedure TFormColorSelect.SetNewColor(Value : TColor);
var Color : TColor;
begin
  FNewColor := Value;

  if (isRed.Value <> GetRValue(Value))
  then isRed.Value   := GetRValue(Value);
  if (isGreen.Value <> GetGValue(Value))
  then isGreen.Value := GetGValue(Value);
  if (isBlue.Value <> GetBValue(Value))
  then isBlue.Value  := GetBValue(Value);

  miNewColor.Image.FillRGB(FNewColor);
  miNewColor.DrawImage;
  if ((GetRValue(Value) + GetGValue(Value) + GetBValue(Value)) div 3 > 127)
  then Color := clBlack
  else Color := clWhite;
  if (lCurrColor.Font.Color <> Color)
  then lCurrColor.Font.Color := Color;
end; // TFormColorSelect.SetNewColor.


procedure TFormColorSelect.HSVColorRectangle;
var i, x, y   : integer;
    Red       : integer;
    Green     : integer;
    Blue      : integer;
    Hue       : integer;
    Intensity : integer;
    Color     : TColor;
    pLine     : PVectorB;
begin
  // Create HSV bitmap.
  if (mcmImageHSV.Image.DibHandle = 0)
  then begin
       // mcmImageHSV.Image.ImageFormat := IF_RGBA32;
       mcmImageHSV.Image.ImageFormat := IF_RGB24;
       mcmImageHSV.Image.Width := 256; // mcmImageHSV.Width - 1;
       mcmImageHSV.Image.Height := 256; // mcmImageHSV.Height - 1;

       //Color := ColorToRGB(Self.Color);
       Color := ColorToRGB(tsHSV.Brush.Color);
       mcmImageHSV.Image.FillRGB(Color);
  end;

  Intensity := isValue.Value;

  // Calc HSV bitmap data.
  for y := 0 to (mcmImageHSV.Image.Height - 1)
  do begin
     pLine := mcmImageHSV.Image.ScanLine[y];
     i := 0;
     for x := 0 to (mcmImageHSV.Image.Width - 1)
     do begin
        Hue := Round(359 * (1.0 - x / (mcmImageHSV.Image.Width - 1) ));
        HSVToRGB(Hue, y, Intensity, Red, Green, Blue);
        pLine^[i] := Blue;
        inc(i);
        pLine^[i] := Green;
        inc(i);
        pLine^[i] := Red;
        inc(i);
     end;
  end;
  mcmImageHSV.DrawImage;
  mcmImageHSV.Update;
end; // TFormColorSelect.HSVColorRectangle.


procedure TFormColorSelect.HSVColorCircle(UpdateBk : boolean);
var i, x, y    : integer;
    dx, dy     : integer;
    SqrRadius  : integer;
    Angle      : double;
    Red        : integer;
    Green      : integer;
    Blue       : integer;
    Hue        : integer;
    Saturation : integer;
    Intensity  : integer;
    Color      : TColor;
    pLine      : PVectorB;
begin
  // Create HSV bitmap.
  if (mcmImageHSV.Image.DibHandle = 0)
  then begin
       // mcmImageHSV.Image.ImageFormat := IF_RGBA32;
       mcmImageHSV.Image.ImageFormat := IF_RGB24;
       mcmImageHSV.Image.Width := 256; // mcmImageHSV.Width;
       mcmImageHSV.Image.Height := 256; // mcmImageHSV.Height;
  end;

  if UpdateBk
  then begin
       Color := ColorToRGB(tsHSV.Brush.Color);
       mcmImageHSV.Image.FillRGB(Color);
  end;

  // Get centre x, y
  Fcx := mcmImageHSV.Image.Width div 2;
  Fcy := mcmImageHSV.Image.Height div 2;
  if (Fcx < Fcy)
  then FRadius := Fcx
  else FRadius := Fcy;

  // Calc Radius
  SqrRadius := FRadius * FRadius;

  Intensity := isValue.Value;

  // Calc HSV bitmap data.
  for y := 0 to (mcmImageHSV.Image.Height - 1)
  do begin
     pLine := mcmImageHSV.Image.ScanLine[y];
     i := 0;
     for x := 0 to (mcmImageHSV.Image.Width - 1)
     do begin
        dx := Fcx - x;
        dy := y - Fcy;
        if (SqrRadius >= dx * dx + dy * dy)
        then begin
             // We're inside the Hue circle.
             if (dy <> 0)
             then Angle := ArcTan(dx / dy)
             else begin
                  if (x < Fcx)
                  then Angle := pi / 2
                  else Angle := 3 * pi / 2;
             end;
             if (y >= Fcy)
             then Angle := (Angle + pi / 2) / (2 * pi)
             else Angle := 0.5 + (Angle + pi / 2) / (2 * pi);

             Hue := Round(360 * (1.0 - Angle));
             Saturation := Round(255.0 * Sqrt(dx * dx + dy * dy) / FRadius);
             HSVToRGB(Hue, Saturation, Intensity, Red, Green, Blue);

             pLine^[i] := Blue;
             inc(i);
             pLine^[i] := Green;
             inc(i);
             pLine^[i] := Red;
             inc(i);
        end
        else inc(i, 3);
     end;
  end;
  mcmImageHSV.DrawImage;
  mcmImageHSV.Update;
end; // TFormColorSelect.HSVColorCircle.


procedure TFormColorSelect.WMHSVColorCircle(var Message : TMessage);
begin
  if (FColorMode = 0)
  then HSVColorCircle(False)
  else HSVColorRectangle;
  Message.Result := 0;
end; // TFormColorSelect.WMHSVColorCircle.


procedure TFormColorSelect.HSValueRect;
var x, y       : integer;
    Red        : integer;
    Green      : integer;
    Blue       : integer;
    Hue        : integer;
    Saturation : integer;
begin
  if (mcmImageValue.Image.DibHandle = 0)
  then begin
       // mcmImageValue.Image.ImageFormat := IF_RGBA32;
       mcmImageValue.Image.ImageFormat := IF_RGB24;
       mcmImageValue.Image.Width := mcmImageValue.Width;
       mcmImageValue.Image.Height := mcmImageValue.Height;

       //Color := ColorToRGB(Self.Color);
       Color := ColorToRGB(tsHSV.Brush.Color);
       mcmImageValue.Image.FillRGB(Color);
  end;

  Hue := isHue.Value;
  Saturation := isSaturation.Value;
  y := mcmImageValue.Image.Height - 1;
  for x := 0 to (mcmImageValue.Image.Width - 1)
  do begin
     HSVToRGB(Hue, Saturation, x, Red, Green, Blue);
     mcmImageValue.Image.Canvas.Pen.Color := RGB(Red, Green, Blue);
     mcmImageValue.Image.Canvas.MoveTo(x, 0);
     mcmImageValue.Image.Canvas.LineTo(x, y);
  end;

  mcmImageValue.DrawImage;
  mcmImageValue.Update;
end; // TFormColorSelect.HSValueRect.


procedure TFormColorSelect.GetXYFromCircleHSV(var x, y : integer);
var Angle   : double;
    Sradius : double;
begin
  Sradius := FRadius * isSaturation.Value / 255.0;
  Angle := 2 * pi * isHue.Value / 359.0;
  x := Fcx + Round(Sradius * cos(Angle));
  y := Fcy - Round(Sradius * sin(Angle));
end; // TFormColorSelect.GetXYFromCircleHSV.


procedure TFormColorSelect.GetXYFromRectangleHSV(var x, y : integer);
begin
  x := Round((mcmImageHSV.Image.Width - 1) * (359 - isHue.Value) / 359.0);
  y := isSaturation.Value;
end; // TFormColorSelect.GetXYFromRectangleHSV.


procedure TFormColorSelect.UpdateCircleCursor;
begin
  if Not(FIsUpdating)
  then begin
       if (FColorMode = 0)
       then GetXYFromCircleHSV(Fpx, Fpy)
       else GetXYFromRectangleHSV(Fpx, Fpy);
       mcmShape1.Left := Fpx + mcmImageHSV.Left - mcmShape1.Width div 2;
       mcmShape1.Top := Fpy + mcmImageHSV.Top - mcmShape1.Height div 2;
  end;
end; // TFormColorSelect.UpdateCircleCursor.


procedure TFormColorSelect.GetCircleHSVColor(x, y : integer; UpdateHSV : boolean);
var dx, dy        : integer;
    Angle         : double;
    Radius        : integer;
    H, S, V       : integer;
    R, G, B       : integer;
    MaxSaturation : boolean;
begin
  dx := Fcx - x;
  dy := y - Fcy;
  Radius := Round(2.0 * Sqrt(dx * dx + dy * dy));

  // Calculate angle.
  if (dy <> 0)
  then Angle := ArcTan(dx / dy)
  else begin
       if (x < Fcx)
       then Angle := pi / 2
       else Angle := 3 * pi / 2;
  end;
  if (y >= Fcy)
  then Angle := (Angle + pi / 2)
  else Angle := pi + (Angle + pi / 2);

  // Adjust x, y if outside HSV circle.
  if (2 * FRadius < Radius)
  then begin
       x := Fcx + Round(Cos(Angle) * FRadius);
       y := Fcy + Round(Sin(Angle) * FRadius);

       dx := Fcx - x;
       dy := y - Fcy;
       Radius := Round(2.0 * Sqrt(dx * dx + dy * dy));

       MaxSaturation := True;
  end
  else MaxSaturation := False;

  if (2 * FRadius < Radius)
  then Radius := 255;

  Angle := 360.0 * Angle / (2 * pi);
  H := 360 - Round(Angle);
  if MaxSaturation
  then S := 255
  else S := Radius;
  V := isValue.Value;

  HSVToRGB(H, S, V, R, G, B);

  FIsUpdating := True;

  // Update H S V edit controls.
  if UpdateHSV
  then begin
       isHue.Value := H;
       isSaturation.Value := S
       // isValue.Value := V;
  end;
  // Update R G B edit controls.
  SetNewColor(RGB(R, G, B));
  FIsUpdating := False;

  mcmShape1.Left := x + mcmImageHSV.Left - mcmShape1.Width div 2;
  mcmShape1.Top := y + mcmImageHSV.Top - mcmShape1.Height div 2;
end; // TFormColorSelect.GetCircleHSVColor.


procedure TFormColorSelect.GetRectangleHSVColor(x, y : integer; UpdateHSV : boolean);
var H, S, V : integer;
    R, G, B : integer;
    W       : integer;
begin
  W := mcmImageHSV.Image.Width - 1;
  if (x < 0)
  then x := 0;
  if (x > W)
  then x := W;
  if (y < 0)
  then y := 0;
  if (y > mcmImageHSV.Image.Height - 1)
  then y := mcmImageHSV.Image.Height - 1;

  H := Round(359.0 * (W - x) / W);
  S := y;
  V := isValue.Value;

  HSVToRGB(H, S, V, R, G, B);

  FIsUpdating := True;

  // Update H S V edit controls.
  if UpdateHSV
  then begin
       isHue.Value := H;
       isSaturation.Value := S
       // isValue.Value := V;
  end;

  // Update R G B edit controls.
  SetNewColor(RGB(R, G, B));

  FIsUpdating := False;

  mcmShape1.Left := x + mcmImageHSV.Left - mcmShape1.Width div 2;
  mcmShape1.Top := y + mcmImageHSV.Top - mcmShape1.Height div 2;
end; // TFormColorSelect.GetRectangleHSVColor.


procedure TFormColorSelect.UpdateHSVColor;
var H, S, V : integer;
begin
  if Not(FIsUpdating)
  then begin
       FIsUpdating := True;
       try
         RGBToHSV(isRed.Value, isGreen.Value, isBlue.Value, H, S, V);
         isHue.Value := H;
         isSaturation.Value := S;
         if (isValue.Value <> V)
         then begin
              isValue.Value := V;
              if (FColorMode = 0)
              then HSVColorCircle(False)
              else HSVColorRectangle;
         end
         else isValue.Value := V;

         HSValueRect;
       finally
         FIsUpdating := False;
       end;
  end;
  SetNewColor(RGB(isRed.Value, isGreen.Value, isBlue.Value));
end; // TFormColorSelect.UpdateHSVColor.


procedure TFormColorSelect.UpdateRGBColor;
var R, G, B : integer;
begin
  if Not(FIsUpdating)
  then begin
       FIsUpdating := True;
       try
         HSValueRect;
         HSVToRGB(isHue.Value, isSaturation.Value, isValue.Value, R, G, B);
         isRed.Value := R;
         isGreen.Value := G;
         isBlue.Value := B;

         SetNewColor(RGB(R, G, B));
       finally
         FIsUpdating := False;
       end;
  end;
end; // TFormColorSelect.UpdateRGBColor.


procedure TFormColorSelect.mcmImageHSVMouseDown(Sender : TObject;
                                                   Button : TMouseButton;
                                                   Shift  : TShiftState;
                                                   X, Y   : Integer);
var pt : TPoint;
begin
  if (ssLeft in Shift)
  then begin
       if (Sender is TmcmShape)
       then begin
            pt := mcmShape1.ClientToScreen(Point(x, y));
            pt := mcmImageHSV.ScreenToClient(pt);
            x := pt.x;
            y := pt.y;
       end;

       Fpx := x;
       Fpy := y;
       if (FColorMode = 0)
       then GetCircleHSVColor(x, y, True)
       else GetRectangleHSVColor(x, y, True);
       HSValueRect;
  end;
end; // TFormColorSelect.mcmImageHSVMouseDown.


procedure TFormColorSelect.mcmImageHSVMouseMove(Sender : TObject;
                                                   Shift  : TShiftState;
                                                   X, Y   : Integer);
var pt : TPoint;
begin
  if (ssLeft in Shift)
  then begin
       if (Sender is TmcmShape)
       then begin
            pt := mcmShape1.ClientToScreen(Point(x, y));
            pt := mcmImageHSV.ScreenToClient(pt);
            x := pt.x;
            y := pt.y;
       end;

       Fpx := x;
       Fpy := y;
       if (FColorMode = 0)
       then GetCircleHSVColor(x, y, True)
       else GetRectangleHSVColor(x, y, True);
       HSValueRect;
  end;
end; // TFormColorSelect.mcmImageHSVMouseMove.


procedure TFormColorSelect.mcmImageValueMouseDown(Sender : TObject;
                                                  Button : TMouseButton;
                                                  Shift  : TShiftState;
                                                  X, Y   : Integer);
var pt : TPoint;
begin
  if (ssLeft in Shift)
  then begin
       if (Sender is TmcmShape)
       then begin
            pt := msValue.ClientToScreen(Point(x, y));
            pt := mcmImageValue.ScreenToClient(pt);
            x := pt.x;
       end;
       isValue.Value := x;
  end;
end; // TFormColorSelect.mcmImageValueMouseDown.


procedure TFormColorSelect.mcmImageValueMouseMove(Sender : TObject;
                                                  Shift  : TShiftState;
                                                  X, Y   : Integer);
var pt : TPoint;
begin
  if (ssLeft in Shift)
  then begin
       if (Sender is TmcmShape)
       then begin
            pt := msValue.ClientToScreen(Point(x, y));
            pt := mcmImageValue.ScreenToClient(pt);
            x := pt.x;
       end;
       isValue.Value := x;
  end;
end; // TFormColorSelect.mcmImageValueMouseMove.


procedure TFormColorSelect.UpdateValueCursor;
begin
  if Not(FIsUpdating)
  then begin
       msValue.Left := isValue.Value + msValue.Width shr 1;
       msValue.Top  := mcmImageValue.Top;
       if (128 < isValue.Value)
       then msValue.Pen.Mode := pmBlack
       else msValue.Pen.Mode := pmWhite;
  end;
end; // TFormColorSelect.UpdateValueCursor.


procedure TFormColorSelect.tbRedChange(Sender : TObject);
begin
  UpdateHSVColor;
  UpdateCircleCursor;
end; // TFormColorSelect.tbRedChange.


procedure TFormColorSelect.tbGreenChange(Sender : TObject);
begin
  UpdateHSVColor;
  UpdateCircleCursor;
end; // TFormColorSelect.tbGreenChange.


procedure TFormColorSelect.tbBlueChange(Sender : TObject);
begin
  UpdateHSVColor;
  UpdateCircleCursor;
end; // TFormColorSelect.tbBlueChange.


procedure TFormColorSelect.tbHueChange(Sender : TObject);
begin
  UpdateRGBColor;
  UpdateCircleCursor;
end; // TFormColorSelect.tbHueChange.


procedure TFormColorSelect.tbSaturationChange(Sender : TObject);
begin
  UpdateRGBColor;
  UpdateCircleCursor;
end; // TFormColorSelect.tbSaturationChange.


procedure TFormColorSelect.tbValueChange(Sender : TObject);
begin
  UpdateValueCursor;
  Update;
  PostMessage(Handle, WM_HSVCOLORCIRCLE, 0, 0);
  if Not(FIsUpdating)
  then begin
       if (FColorMode = 0)
       then GetCircleHSVColor(Fpx, Fpy, False)
       else GetRectangleHSVColor(Fpx, Fpy, False);
       HSValueRect;
  end
  else SetNewColor(RGB(isRed.Value, isGreen.Value, isBlue.Value));
end; // TFormColorSelect.tbValueChange.


procedure TFormColorSelect.mcmImageHSVContextPopup(    Sender   : TObject;
                                                       MousePos : TPoint;
                                                   var Handled  : Boolean);
begin
  MousePos := ClientToScreen(MousePos);
  pmColorSelect.Popup(MousePos.x, MousePos.y);
end; // TFormColorSelect.mcmImageHSVContextPopup.


procedure TFormColorSelect.HSVCircular1Click(Sender : TObject);
begin
  FColorMode := 0;
  HSVCircular1.Checked := (FColorMode = 0);
  HSVRectangular1.Checked := (FColorMode = 1);
  HSVColorCircle(True);
  UpdateCircleCursor;
end; // TFormColorSelect.HSVCircular1Click.


procedure TFormColorSelect.HSVRectangular1Click(Sender : TObject);
begin
  FColorMode := 1;
  HSVCircular1.Checked := (FColorMode = 0);
  HSVRectangular1.Checked := (FColorMode = 1);
  HSVColorRectangle;
  UpdateCircleCursor;
end; // TFormColorSelect.HSVRectangular1Click.


end.
