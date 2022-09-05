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
// $Log:  17593: uchildwin.pas 
//
//    Rev 1.19    2014-02-02 21:10:08  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.18    01-07-2006 11:38:30  mcm    Version: IMG 3.0
// Enabled displaying "fax" images in correct dimension by using the horizontal
// and vertical resolution to scale the vertical direction additionally.
//
//    Rev 1.17    04-03-2006 18:26:52  mcm    Version: IMG 2.16
// Added drawing tools.
//
//    Rev 1.16    22/02/2006 00:09:58  mcm
// Added newdrawing tools.
//
//    Rev 1.15    22/11/2005 18:41:40  mcm    Version: IMG 2.10
// Images are now show using half-tone when zoom to asize less than 1:1
// pixel-wise. 
//
//   Rev 1.14    23-08-2005 22:24:16  mcm    Version: IMG 2.9
// Minor change to Drag & Drop usage.

//
//   Rev 1.13    24-07-2005 18:55:56  mcm
// Drag & Drop Support.

//
//   Rev 1.12    28-10-2004 19:44:06  mcm    Version: IMG 2.6
// Modified to improve display of BW images when stretching.

//
//   Rev 1.11    02-09-2004 21:38:28  mcm    Version: IMG 2.6
// Added minor correction to extend ScrollBar range above 32767 when using
// Delphi 3, 4 and 5.

//
//   Rev 1.10    27-07-2004 15:11:16  mcm    Version: IMG 2.5

//
//   Rev 1.9    07-03-2004 14:48:00  mcm    Version: IMG 2.4

//
//   Rev 1.8    30-01-2004 20:43:44  mcm    Version: IMG 2.3
// Added Mouse wheel support.
// Added use of TmcmProfile.
// Modified sizing a new region view to be more flexible.

//
//   Rev 1.7    17-11-2003 09:58:14  mcm    Version: IMG 2.0
// Sets the wndows Brush.Style to clear.

//
//   Rev 1.6    07-11-2003 18:10:16  mcm    Version: IMG 2.0

//
//   Rev 1.5    25-09-2003 23:35:48  mcm    Version: IMG 1.5

//
//   Rev 1.4    15-06-2003 13:22:46  mcm    Version: IMG 1.3.4

//
//   Rev 1.3    11-03-2003 00:04:14  mcm    Version: IMG 1.3.2
// Improved handling image FitToWindow when window is maximized.

//
//   Rev 1.2    27-01-2003 13:51:04  mcm
// Added RegionImage property to show Copy/Past image section.

//
//   Rev 1.1    27-09-2002 13:29:02  mcm    Version: IMG 1.2

//
//   Rev 1.0    27-05-2002 16:22:30  mcm

unit uchildwin;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
      Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
     {$ENDIF}
     mcmImage, mcmRegion, mcmGraphics, mcmImageTypeDef, mcmDragDrop;

type
  TImageTool     = (IT_PICK, IT_PIPETTE, IT_PEN, IT_LINE, IT_RECT, IT_CIRCLE, IT_REGION, IT_PROFILE);

  TNotifyXYColor   = procedure(Sender : TObject;
                               Shift  : TShiftState;
                               X, Y   : integer) of object;
  TOnRegionChanged = procedure(Sender : TObject;
                               Region : TRect) of object;

  TFormChild = class(TForm)
    ImageCtrl     : TmcmImageCtrl;  // Image container
    mcmRegion     : TmcmRegion;     // Copy/Paste region control
    mcmProfile    : TmcmProfile;    // Intensity profile.
    mcmDropSource : TmcmDropSource; // Enables dragging image to other applications.
    procedure FormCloseQuery     (    Sender   : TObject;
                                  var CanClose : Boolean);
    procedure FormClose          (    Sender   : TObject;
                                  var Action   : TCloseAction);
    procedure ImageCtrlChange    (    Sender   : TObject);
    procedure FormCreate         (    Sender   : TObject);
    procedure FormActivate       (    Sender   : TObject);
    procedure ImageCtrlMouseMove (    Sender   : TObject;
                                      Shift    : TShiftState;
                                      X, Y     : Integer);
    procedure mcmRegionMouseUp   (    Sender   : TObject;
                                      Button   : TMouseButton;
                                      Shift    : TShiftState;
                                      X, Y     : Integer);
    procedure mcmRegionMouseDown (    Sender   : TObject;
                                      Button   : TMouseButton;
                                      Shift    : TShiftState;
                                      X, Y     : Integer);
    procedure FormResize         (    Sender   : TObject);
    procedure ImageCtrlMouseDown (    Sender   : TObject;
                                      Button   : TMouseButton;
                                      Shift    : TShiftState;
                                      X, Y     : Integer);
    procedure ImageCtrlMouseUp   (    Sender   : TObject;
                                      Button   : TMouseButton;
                                      Shift    : TShiftState;
                                      X, Y     : Integer);
    procedure RegionChanged      (    Sender   : TObject);
    procedure mcmControlMouseMove(    Sender   : TObject;
                                      Shift    : TShiftState;
                                      X, Y     : Integer);
    procedure ImageCtrlMouseWheel(    Sender     : TObject;
                                      Shift      : TShiftState;
                                      WheelDelta : Integer;
                                      MousePos   : TPoint;
                                  var Handled    : Boolean);
  private
    { Private declarations }
    FGettingFocus    : TNotifyEvent;    // Event fired when this window gains focus.
    FLoosingFocus    : TNotifyEvent;    // Event fired when this window looses focus.
    FOnXYColor       : TNotifyXYColor;  // Event fired when the mouse is moved.
    FOnRegionChanged : TOnRegionChanged;// Event fired when region is moved, resize etc.
    FImageTool       : TImageTool;      // The selected tool (Speed buttons in main window).
    FLButtonDown     : boolean;         // Left mouse button down indicator.
    FRButtonDown     : boolean;         // Right mouse button down indicator.
    Foh, Fow         : integer;         // Old image height & width.
    FxStart          : integer;         // x/yStart used when sizing a new region.
    FyStart          : integer;         // -
    FxEnd            : integer;         // x/yEnd .
    FyEnd            : integer;         // -
    Fsx, Fsy         : integer;
    Fex, Fey         : integer;

    procedure AdjustScrollBar;
    function  GetImage : TmcmImage;
    function  GetRegion : TRect;
    function  GetRegionImage : TmcmImage;
    function  GetScale : double;
    function  GetScaleToFit : boolean;
    procedure SetImage(Value : TmcmImage);
    procedure SetImageTool(Value : TImageTool);
    procedure SetRegion(Value : TRect);
    procedure SetRegionImage(Value : TmcmImage);
    procedure SetScale(Value : double);
    procedure SetScaleToFit(Value : boolean);
  public
    { Public declarations }
    function AddRegion(Rect : TRect; Color : TColor) : TmcmRegion;
    procedure ClearRegions;

    property  Image : TmcmImage
      read    GetImage
      write   SetImage;
    property  ImageTool : TImageTool
      read    FImageTool
      write   SetImageTool;
    property  Scale : double
      read    GetScale
      write   SetScale;
    property  ScaleToFit : boolean
      read    GetScaleToFit
      write   SetScaleToFit;
    property  Region : TRect
      read    GetRegion
      write   SetRegion;
    property  RegionImage : TmcmImage
      read    GetRegionImage
      write   SetRegionImage;
    property  GettingFocus : TNotifyEvent
      read    FGettingFocus
      write   FGettingFocus;
    property  LoosingFocus : TNotifyEvent
      read    FLoosingFocus
      write   FLoosingFocus;
    property  OnRegionChanged : TOnRegionChanged
      read    FOnRegionChanged
      write   FOnRegionChanged;
    property  OnXYColor    : TNotifyXYColor
      read    FOnXYColor
      write   FOnXYColor;
  end;

var FormChild : TFormChild;

implementation

{$R *.DFM}

uses {$IFDEF GE_DXE2}
       System.UITypes,
     {$ENDIF}
     uFormPaintBox;


procedure TFormChild.FormCreate(Sender : TObject);
begin
  // Initialize window variables.
  Foh := 0;
  Fow := 0;
  FGettingFocus     := Nil;
  FLoosingFocus     := Nil;
  FOnXYColor        := Nil;
  FOnRegionChanged  := Nil;
  FImageTool        := IT_PICK;
  mcmRegion.Visible := False;
  mcmProfile.Left   := 0;
  mcmProfile.Top    := 0;
  FLButtonDown      := False;
  FRButtonDown      := False;
  Width  := 50;
  Height := 50;
  Brush.Style := bsClear; // This remove flicker - but requires that the client
                          // area is covered, or not covered areas are painted
                          // using other methods.
end; // TFormChild.FormCreate.


procedure TFormChild.FormClose(Sender : TObject; var Action : TCloseAction);
begin
  Action := caFree; // Free window when closed.
end; // TFormChild.FormClose.


procedure TFormChild.FormCloseQuery(Sender : TObject; var CanClose : Boolean);
begin
  CanClose := True;
  if Assigned(FLoosingFocus)
  then FLoosingFocus(Self);
end; // TFormChild.FormCloseQuery.


procedure TFormChild.FormActivate(Sender : TObject);
begin
  if Assigned(FGettingFocus)
  then FGettingFocus(Self);
end; // TFormChild.FormActivate.


procedure TFormChild.FormResize(Sender : TObject);
begin
  ;
end; // TFormChild.FormResize.


procedure TFormChild.AdjustScrollBar;
begin
  // Set new scrollbar range.
  HorzScrollBar.Range := Round(ImageCtrl.Scale * ImageCtrl.Image.DispWidth);
  VertScrollBar.Range := Round(ImageCtrl.Scale * ImageCtrl.Image.DispHeight);
end; // TFormChild.AdjustScrollBar.


procedure TFormChild.ImageCtrlChange(Sender : TObject);
var i, w, h  : integer;
    mh, mw   : integer;
    TmpScale : double;
begin
  // Get Width and Height of image.
  w := ImageCtrl.Image.DispWidth;
  h := ImageCtrl.Image.DispHeight;

  if (Foh = h) and (Fow = w)
  then exit; // Exit if image height and width hasn't changed.
  Foh := h;
  Fow := w;

  // Get max Client-width and -height that keeps the window within MainForm's
  // client area.
  mw := Application.MainForm.ClientWidth - Left  -
        6 * GetSystemMetrics(SM_CXEDGE);
        // Below, 19 = Statusbar 29 Speedbutton
  mh := Application.MainForm.ClientHeight - Top -
        6 * GetSystemMetrics(SM_CYEDGE) - GetSystemMetrics(SM_CYCAPTION) - 19 - 29;
  // Add scrollbar horizontal height/vertical width to our client height/width
  // if image height/width is greater than MainForm's client height/width.
  if (h > mh)
  then w := w + GetSystemMetrics(SM_CXVSCROLL);
  if (w > mw)
  then h := h + GetSystemMetrics(SM_CYVSCROLL);

  // Determind the scale factor that will show the entire image in this window.
  TmpScale := ImageCtrl.Scale;
  if Assigned(Application.MainForm)
  then begin
       i := 0;
       while ((w > i * mw) or
              (h > i * mh)) and (i < 8)
       do begin
          inc(i);
       end;
       if (i < 8)
       then begin
            if (i <> 0)
            then TmpScale := 1.0 / i
            else TmpScale := 1.0;

            if (WindowState = wsNormal)
            then begin
                 ClientWidth  := Round(w * TmpScale);
                 ClientHeight := Round(h * TmpScale);
            end;
       end;
  end;
  ImageCtrl.Scale := TmpScale;
  AdjustScrollBar;

  mcmProfile.Image    := ImageCtrl.Image;
  mcmProfile.Scale    := ImageCtrl.Scale;
  mcmProfile.Width    := ImageCtrl.Image.Width - 1;
  mcmProfile.Height   := ImageCtrl.Image.Height - 1;

  mcmRegion.Scale     := ImageCtrl.Scale;
  mcmRegion.MaxWidth  := Round(ImageCtrl.Scale * ImageCtrl.Image.Width);
  mcmRegion.MaxHeight := Round(ImageCtrl.Scale * ImageCtrl.Image.Height);

  // Make sure that images are displayed showing as many details when zooming out.
  if (ImageCtrl.Scale < 1.0)
  then ImageCtrl.Image.SetStretchMode(HALFTONE);

  mcmDropSource.Control := ImageCtrl;
  FormActivate(Self);
end; // TFormChild.ImageCtrlChange.


function TFormChild.GetImage : TmcmImage;
begin
  Result := ImageCtrl.Image;
end; // TFormChild.GetImage.


procedure TFormChild.SetImage(Value : TmcmImage);
begin
  ImageCtrl.Image := Value;
  ImageCtrlChange(Self);
  FormActivate(Self);

  mcmProfile.Image  := Value;
  mcmProfile.Scale  := 1.0;

  // Adjust the profile control's size to that of the image.
  mcmProfile.Left   := 0;
  mcmProfile.Top    := 0;
  mcmProfile.Width  := mcmProfile.Image.Width - 1;
  mcmProfile.Height := mcmProfile.Image.Height - 1;
end; // TFormChild.SetImage.


function TFormChild.GetScale : double;
begin
  Result := ImageCtrl.Scale;
end; // TFormChild.GetScale.


procedure TFormChild.SetScale(Value : double);
var w, h, i  : integer;
    OldScale : double;
    mw, mh   : integer;
begin
  // Set new scale (Zoom factor) to the image control.
  OldScale := ImageCtrl.Scale;
  ImageCtrl.Scale := Value;
  w := Round(Value * ImageCtrl.Image.DispWidth);
  h := Round(Value * ImageCtrl.Image.DispHeight);

  // Make sure that images are displayed showing as many details when zooming out.
  if (ImageCtrl.Scale < 1.0)
  then ImageCtrl.Image.SetStretchMode(HALFTONE)
  ELSE ImageCtrl.Image.SetStretchMode(COLORONCOLOR);

  // Scroll to left and top.
  HorzScrollBar.Range := 0;
  VertScrollBar.Range := 0;

  // Set scale factor to additional controls.
  for i := 0 to (ControlCount - 1)
  do begin
     if (Controls[i] is TmcmRegion)
     then begin
          if (OldScale < Value)
          then begin
               TmcmRegion(Controls[i]).MaxWidth  := w;
               TmcmRegion(Controls[i]).MaxHeight := h;
               TmcmRegion(Controls[i]).Scale := Value;
          end
          else begin
               TmcmRegion(Controls[i]).Scale := Value;
               TmcmRegion(Controls[i]).MaxWidth  := w;
               TmcmRegion(Controls[i]).MaxHeight := h;
          end;
     end;
     if (Controls[i] is TmcmProfile)
     then TmcmProfile(Controls[i]).Scale := Value;
  end;

  // Confine this window to our main form's client area.
  if (WindowState = wsNormal)
  then begin
       mw := Application.MainForm.ClientWidth - Left -
             6 * GetSystemMetrics(SM_CXEDGE);

       // Below, 19 = Statusbar 29 Speedbutton
       mh := Application.MainForm.ClientHeight - Top -
             6 * GetSystemMetrics(SM_CYEDGE) - GetSystemMetrics(SM_CYCAPTION) - 19 - 29;
       if (h > mh)
       then w := w + GetSystemMetrics(SM_CXVSCROLL);
       if (w > mw)
       then h := h + GetSystemMetrics(SM_CYVSCROLL);

       if (w < mw)
       then ClientWidth  := w
       else ClientWidth  := mw;
       if (h < mh)
       then ClientHeight := h
       else ClientHeight := mh;
  end;
  AdjustScrollBar;
end; // TFormChild.SetScale.


function TFormChild.GetScaleToFit : boolean;
begin
  Result := ImageCtrl.ScaleToFit;
end; // TFormChild.GetScaleToFit.


procedure TFormChild.SetScaleToFit(Value : boolean);
begin
  // Resize window and image to show the entire image.
  if Value
  then begin
       VertScrollBar.Range := 0;
       HorzScrollBar.Range := 0;
  end;
  ImageCtrl.ScaleToFit := Value;
  if Not(Value)
  then AdjustScrollBar
  else begin
       if (WindowState = wsNormal)
       then begin
            ClientWidth  := Round(ImageCtrl.Image.DispWidth * ImageCtrl.Scale);
            ClientHeight := Round(ImageCtrl.Image.DispHeight * ImageCtrl.Scale);
       end;
  end;
end; // TFormChild.SetScaleToFit.


(*
procedure TFormChild.FixEndCoordinate(var x0, y0, x1, y1);
begin
  if (x0 <> x1)
  then begin
       if (x0 < x1)
       then inc(x1)
       else dec(x1);
  end;
  if (y0 <> y1)
  then begin
       if (y0 < y1)
       then inc(y1)
       else dec(y1);
  end;
end; //
*)


procedure TFormChild.ImageCtrlMouseDown(Sender : TObject;
                                        Button : TMouseButton;
                                        Shift  : TShiftState;
                                        X, Y   : Integer);
begin
  FxStart := Trunc(x / ImageCtrl.Scale);
  FyStart := Trunc(y / ImageCtrl.Scale);

  case Button of
  mbLeft  : begin
              FLButtonDown := True;
              case FImageTool of
              IT_PIPETTE : begin
                             FormPaintBox.FrontColor := Image.Pixel[FxStart,FyStart];
                           end;
              IT_PEN     : begin
                             Image.Canvas.Pen.Color := $00FFFFFF and FormPaintBox.FrontColor;
                             FxEnd := FxStart;
                             FyEnd := FyStart;
                             Image.Pixel[FxStart,FyStart] := $00FFFFFF and FormPaintBox.FrontColor;
                           end;
              IT_LINE, //    : begin
                       //    end;
              IT_RECT,
              IT_CIRCLE  : begin
                             // Set-up pen and brush for drag draw.
                             Canvas.Pen.Color := RGB(0, 0, 0);
                             Canvas.Pen.Mode := pmNotXor;
                             Canvas.Brush.Style := bsClear;

                             FxEnd := FxStart;
                             FyEnd := FyStart;
                             Canvas.Rectangle(x, y, x, y);
                             Fsx := x;
                             Fsy := y;
                             Fex := x;
                             Fey := y;
                           end;
              IT_REGION  : begin
                             // Resize Region.
                             mcmRegion.Visible := False;
                             // Save initial x,y
                             FxStart := x - HorzScrollBar.Position;
                             FyStart := y - VertScrollBar.Position;
                             // Clear Region image.
                             mcmRegion.Image  := Nil;
                             mcmRegion.Left   := FxStart;
                             mcmRegion.Top    := FyStart;
                             mcmRegion.Width  := 0;
                             mcmRegion.Height := 0;
                           end;
              end;
            end;
  mbRight : begin
              FRButtonDown := True;
              case FImageTool of
              IT_PIPETTE : begin
                             FormPaintBox.BackColor := Image.Pixel[FxStart,FyStart];
                           end;
              IT_PEN     : begin
                             Image.Canvas.Pen.Color := $00FFFFFF and FormPaintBox.BackColor;
                             FxEnd := FxStart;
                             FyEnd := FyStart;
                             Image.Pixel[FxStart,FyStart] := $00FFFFFF and FormPaintBox.BackColor;
                           end;
              IT_REGION  : begin
                             // Clear Region image.
                             mcmRegion.Image := Nil;
                           end;
              end;
            end;
  end;
end; // TFormChild.ImageCtrlMouseDown.


procedure TFormChild.ImageCtrlMouseUp(Sender : TObject;
                                      Button : TMouseButton;
                                      Shift  : TShiftState;
                                      X, Y   : Integer);
var dx, dy : integer;
begin
  FxEnd := Trunc(x / ImageCtrl.Scale);
  FyEnd := Trunc(y / ImageCtrl.Scale);

  case Button of
  mbLeft  : begin
              case FImageTool of
              IT_PEN     : begin
                             Image.Pixel[FxEnd,FyEnd] := $00FFFFFF and FormPaintBox.FrontColor;
                           end;
              IT_LINE    : begin
                             if (FxStart <> FxEnd) or (FyStart <> FyEnd)
                             then begin
                                  Image.Canvas.Pen.Mode := pmCopy;
                                  Image.Canvas.Pen.Color := $00FFFFFF and FormPaintBox.FrontColor;
                                  Image.Canvas.MoveTo(FxStart, FyStart);
                                  Image.Canvas.LineTo(FxEnd, FyEnd);
                             end;
                             ImageCtrl.DrawImage;
                           end;
              IT_RECT    : begin
                             dx := FxStart - FxEnd;
                             dy := FyStart - FyEnd;
                             if (ssShift in Shift)
                             then begin
                                  if (Abs(dx) > Abs(dy))
                                  then begin
                                       if (dx <> 0)
                                       then FxEnd := FxStart - Abs(dy) * (dx div Abs(dx))
                                       else FxEnd := FxStart;
                                  end
                                  else begin
                                       if (dy <> 0)
                                       then FyEnd := FyStart - Abs(dx) * (dy div Abs(dy))
                                       else FyEnd := FyStart;
                                  end;
                             end;
                             if (FxStart <> FxEnd) or (FyStart <> FyEnd)
                             then begin
                                  Image.Canvas.Pen.Mode := pmCopy;
                                  Image.Canvas.Brush.Style := bsSolid;
                                  Image.Canvas.Pen.Color := $00FFFFFF and FormPaintBox.FrontColor;
                                  Image.Canvas.Brush.Color := $00FFFFFF and FormPaintBox.BackColor;
                                  Image.Canvas.Rectangle(FxStart, FyStart, FxEnd, FyEnd);
                             end;
                             ImageCtrl.DrawImage;
                           end;
              IT_CIRCLE  : begin
                             dx := FxStart - FxEnd;
                             dy := FyStart - FyEnd;
                             if (ssShift in Shift)
                             then begin
                                  if (Abs(dx) > Abs(dy))
                                  then begin
                                       if (dx <> 0)
                                       then FxEnd := FxStart - Abs(dy) * (dx div Abs(dx))
                                       else FxEnd := FxStart;
                                  end
                                  else begin
                                       if (dy <> 0)
                                       then FyEnd := FyStart - Abs(dx) * (dy div Abs(dy))
                                       else FyEnd := FyStart;
                                  end;
                             end;
                             if (FxStart <> FxEnd) or (FyStart <> FyEnd)
                             then begin
                                  Image.Canvas.Pen.Color := $00FFFFFF and FormPaintBox.FrontColor;
                                  Image.Canvas.Brush.Color := $00FFFFFF and FormPaintBox.BackColor;
                                  Image.Canvas.Ellipse(FxStart, FyStart, FxEnd, FyEnd);
                             end;
                             ImageCtrl.DrawImage;
                           end;
              IT_REGION  : begin
                             if Not(mcmRegion.Visible)
                             then mcmRegion.Visible := True;
                           end;
              end;
            end;
  mbRight : begin
              case FImageTool of
              IT_PEN     : begin
                             Image.Pixel[FxEnd,FyEnd] := $00FFFFFF and FormPaintBox.BackColor;
                           end;
              end;
            end;
  end;
  FLButtonDown := False;
  FRButtonDown := False;
end; // TFormChild.ImageCtrlMouseUp.


procedure TFormChild.ImageCtrlMouseMove(Sender : TObject;
                                        Shift  : TShiftState;
                                        X, Y   : Integer);
var dx, dy : integer;
begin
  {$IFDEF DCB3_5} // Delphi 3, 4 & 5.
    // Handle special case for [X,Y] position exceeding [32767,32767] which
    // swaps to [-32768,-32768] on succeding pixel position.
    // Limitation - Width and Height of window cannot exceed 65536.
    {
    if (X < 0)
    then X := 65537 + X;
    if (Y < 0)
    then Y := 65537 + Y;
    }
  {$ENDIF}

  // Notify main form about current coordinate and color.
  if Assigned(FOnXYColor)
  then FOnXYColor(Self, Shift, X, Y);

  FxEnd := Trunc(x / ImageCtrl.Scale);
  FyEnd := Trunc(y / ImageCtrl.Scale);

  if FLButtonDown
  then begin
       case FImageTool of
       IT_PIPETTE : begin
                      FormPaintBox.FrontColor := Image.Pixel[FxEnd,FyEnd];
                    end;
       IT_PEN     : begin
                      Image.Canvas.MoveTo(FxStart, FyStart);
                      Image.Canvas.LineTo(FxEnd, FyEnd);
                      FxStart := FxEnd;
                      FyStart := FyEnd;
                      Image.Pixel[FxStart,FyStart] := $00FFFFFF and FormPaintBox.FrontColor;
                      ImageCtrl.DrawImage;
                    end;
       IT_LINE    : begin
                      Canvas.MoveTo(Fsx - HorzScrollBar.Position, Fsy - VertScrollBar.Position);
                      Canvas.LineTo(Fex - HorzScrollBar.Position, Fey - VertScrollBar.Position);
                      Fex := x;
                      Fey := y;

                      Canvas.MoveTo(Fsx - HorzScrollBar.Position, Fsy - VertScrollBar.Position);
                      Canvas.LineTo(Fex - HorzScrollBar.Position, Fey - VertScrollBar.Position);
                    end;
       IT_RECT    : begin
                      Canvas.Rectangle(Fsx - HorzScrollBar.Position, Fsy - VertScrollBar.Position,
                                       Fex - HorzScrollBar.Position, Fey - VertScrollBar.Position);
                      Fex := x;
                      Fey := y;
                      dx := Fsx - Fex;
                      dy := Fsy - Fey;
                      if (ssShift in Shift)
                      then begin
                           if (Abs(dx) > Abs(dy))
                           then begin
                                if (dx <> 0)
                                then Fex := Fsx - Abs(dy) * (dx div Abs(dx))
                                else Fex := Fsx;
                           end
                           else begin
                                if (dy <> 0)
                                then Fey := Fsy - Abs(dx) * (dy div Abs(dy))
                                else Fey := Fsy;
                           end;
                      end;
                      Canvas.Rectangle(Fsx - HorzScrollBar.Position, Fsy - VertScrollBar.Position,
                                       Fex - HorzScrollBar.Position, Fey - VertScrollBar.Position);
                    end;
       IT_CIRCLE  : begin
                      Canvas.Ellipse(Fsx - HorzScrollBar.Position, Fsy - VertScrollBar.Position,
                                     Fex - HorzScrollBar.Position, Fey - VertScrollBar.Position);
                      Fex := x;
                      Fey := y;
                      dx := Fsx - Fex;
                      dy := Fsy - Fey;
                      if (ssShift in Shift)
                      then begin
                           if (Abs(dx) > Abs(dy))
                           then begin
                                if (dx <> 0)
                                then Fex := Fsx - Abs(dy) * (dx div Abs(dx))
                                else Fex := Fsx;
                           end
                           else begin
                                if (dy <> 0)
                                then Fey := Fsy - Abs(dx) * (dy div Abs(dy))
                                else Fey := Fsy;
                           end;
                      end;
                      Canvas.Ellipse(Fsx - HorzScrollBar.Position, Fsy - VertScrollBar.Position,
                                     Fex - HorzScrollBar.Position, Fey - VertScrollBar.Position);
                    end;
       IT_REGION  : begin
                      // The mcmRegion control is positioned relative to the
                      // windows client rectangle which doesn't include window
                      // scroll position.
                      // Therefore, we offset the x,y coordinate by the scrollbars
                      // position.
                      X := X - HorzScrollBar.Position;
                      Y := Y - VertScrollBar.Position;
                      if (FxStart < x)
                      then mcmRegion.Right := x
                      else begin
                           mcmRegion.Width := FxStart - x;
                           mcmRegion.Left  := x;
                      end;
                      if (FyStart < y)
                      then mcmRegion.Bottom := y
                      else begin
                           mcmRegion.Height := FyStart - y;
                           mcmRegion.Top    := y;
                      end;
                      if Not(mcmRegion.Visible)
                      then mcmRegion.Visible := True;
                    end;
       end;
  end;
  if FRButtonDown
  then begin
       case FImageTool of
       IT_PIPETTE : begin
                      FormPaintBox.BackColor := Image.Pixel[FxEnd,FyEnd];
                    end;
       IT_PEN     : begin
                      Image.Canvas.MoveTo(FxStart, FyStart);
                      Image.Canvas.LineTo(FxEnd, FyEnd);
                      FxStart := FxEnd;
                      FyStart := FyEnd;
                      Image.Pixel[FxStart,FyStart] := $00FFFFFF and FormPaintBox.BackColor;
                      ImageCtrl.DrawImage;
                    end;
       end;
  end;
end; // TFormChild.ImageCtrlMouseMove.


procedure TFormChild.ImageCtrlMouseWheel(    Sender     : TObject;
                                             Shift      : TShiftState;
                                             WheelDelta : Integer;
                                             MousePos   : TPoint;
                                         var Handled    : Boolean);
var IncValue     : integer;
begin
  if (ssShift in Shift)
  then begin // If Shift is pressed scroll in horizontal direction.
       if (WheelDelta > 0)
       then IncValue := - HorzScrollBar.Increment
       else IncValue := HorzScrollBar.Increment;
       HorzScrollBar.Position := HorzScrollBar.Position + IncValue;
  end
  else begin // Otherwise scroll in vertical direction.
       if (WheelDelta > 0)
       then IncValue := - VertScrollBar.Increment
       else IncValue := VertScrollBar.Increment;
       VertScrollBar.Position := VertScrollBar.Position + IncValue;
  end;

  // Notify main form about nw cursor coordinate and color.
  if Assigned(FOnXYColor)
  then begin
       GetCursorPos(MousePos);
       MousePos := ScreenToClient(MousePos);
       MousePos.X := MousePos.X + HorzScrollBar.Position;
       MousePos.Y := MousePos.Y + VertScrollBar.Position;
       FOnXYColor(Self, Shift, MousePos.X, MousePos.Y);
  end;
end; // TFormChild.ImageCtrlMouseWheel.


procedure TFormChild.mcmRegionMouseDown(Sender : TObject;
                                        Button : TMouseButton;
                                        Shift  : TShiftState;
                                        X, Y   : Integer);
begin
  case Button of
  mbLeft  : ;
  mbRight : ;
  end;
end; // TFormChild.mcmRegionMouseDown.


procedure TFormChild.mcmRegionMouseUp(Sender : TObject;
                                      Button : TMouseButton;
                                      Shift  : TShiftState;
                                      X, Y   : Integer);
var SourceImage : TmcmImage;
begin
  case Button of
  mbLeft  : begin
            end;
  mbRight : begin
              // A right click event in the mcmRegion
              if (Sender is TmcmRegion)
              then if (Sender <> mcmRegion)
                   then begin
                        // is used to remove the control if it's created at
                        // run-time.
                        Self.RemoveControl((Sender as TmcmRegion));
                        Update;
                        (Sender as TmcmRegion).Free;
                   end
                   else begin
                        // or is used to merge region/pasted image into the image.
                        if (mcmRegion.Image <> Nil)
                        then begin
                             if Not(mcmRegion.Image.Empty)
                             then begin
                                  SourceImage := mcmRegion.Image;
                                  ImageCtrl.Image.PasteRegion(SourceImage, Region);
                                  InvalidateRect(Handle, Nil, False);
                             end;
                        end;
                        // Hide region.
                        mcmRegion.Visible := False;
                   end;
            end;
  end;
end; // TFormChild.mcmRegionMouseUp.


procedure TFormChild.mcmControlMouseMove(Sender : TObject;
                                         Shift  : TShiftState;
                                         X, Y   : Integer);
begin
  // Mouse move events from TmcmRegion and TmcmProfile.
  // To get the correct x,y coordinate within the image we must add scrollbar
  // positions.
  if Assigned(FOnXYColor)
  then FOnXYColor(Self, Shift, X + HorzScrollBar.Position,
                               Y + VertScrollBar.Position);
end; // TFormChild.mcmControlMouseMove.


procedure TFormChild.ClearRegions;
var i : integer;
begin
  // Free all mcmRegion's created at run-time.
  for i := (ControlCount - 1) downto 0
  do if (Controls[i] is TmcmRegion)
     then begin
          if ((Controls[i] as TmcmRegion).LinePen.Color = CLRED) 
          then begin
               if (Controls[i] as TmcmRegion).Visible
               then (Controls[i] as TmcmRegion).Visible := False;
               (Controls[i] as TmcmRegion).Image := Nil;
          end
          else (Controls[i] as TmcmRegion).Free;
     end;
end; // TFormChild.ClearRegions.


function TFormChild.GetRegionImage : TmcmImage;
begin
  Result := mcmRegion.Image;
end; // TFormChild.GetRegionImage.


procedure TFormChild.SetRegionImage(Value : TmcmImage);
begin
  mcmRegion.Image := Value;
  mcmRegion.Visible := True;
end; // TFormChild.SetRegionImage.


function TFormChild.AddRegion(Rect : TRect; Color : TColor) : TmcmRegion;
var NewRegion : TmcmRegion;
begin
  NewRegion := TmcmRegion.Create(Self);
  NewRegion.Visible   := False;
  Self.InsertControl(NewRegion);
  NewRegion.MaxWidth  := Round(ImageCtrl.Scale * ImageCtrl.Image.Width);
  NewRegion.MaxHeight := Round(ImageCtrl.Scale * ImageCtrl.Image.Height);
  NewRegion.Scale := 1;
  NewRegion.BoundsRect := Rect;
  NewRegion.LinePen.Color := Color;
  NewRegion.OnMouseDown := mcmRegionMouseDown;
  NewRegion.OnMouseUp   := mcmRegionMouseUp;
  if (Color = CLRED)
  then begin
       NewRegion.OnMoved   := RegionChanged;
       NewRegion.OnResized := RegionChanged;
  end;
  NewRegion.Scale := ImageCtrl.Scale;
  NewRegion.Visible := True;
  Result := NewRegion;
end; // TFormChild.AddRegion.


function TFormChild.GetRegion : TRect;
var sa : single;
begin
  if mcmRegion.Visible
  then begin
       sa := 1.0 / ImageCtrl.Scale;
       Result.Left   := Trunc(sa * (mcmRegion.Left + HorzScrollBar.Position));
       Result.Top    := Trunc(sa * (mcmRegion.Top + VertScrollBar.Position));
       Result.Right  := Trunc(sa * (mcmRegion.Right + HorzScrollBar.Position));
       Result.Bottom := Trunc(sa * (mcmRegion.Bottom + VertScrollBar.Position));
  end
  else begin
       Result.Left   := 0;
       Result.Top    := 0;
       Result.Right  := ImageCtrl.Image.Width;
       Result.Bottom := ImageCtrl.Image.Height;
  end;
end; // TFormChild.GetRegion.


procedure TFormChild.SetRegion(Value : TRect);
var sa : single;
begin
  mcmRegion.Visible := False;
  sa := 1.0 / ImageCtrl.Scale;
  mcmRegion.Left   := Round(sa * (Value.Left - HorzScrollBar.Position));
  mcmRegion.Top    := Round(sa * (Value.Top - VertScrollBar.Position));
  mcmRegion.Right  := Round(sa * (Value.Right - HorzScrollBar.Position));
  mcmRegion.Bottom := Round(sa * (Value.Bottom - VertScrollBar.Position));
  mcmRegion.Visible := True;
end; // TFormChild.SetRegion.


procedure TFormChild.RegionChanged(Sender : TObject);
begin
  if Assigned(FOnRegionChanged)
  then FOnRegionChanged(Self, (Sender as TmcmRegion).BoundsRect);
end; // TFormChild.RegionChanged.


procedure TFormChild.SetImageTool(Value : TImageTool);
begin
  FImageTool := Value;
  mcmRegion.Visible := False;
  mcmDropSource.DragMode := dmManual; // Disable dragging the image.
  case FImageTool of
  IT_PICK    : begin
                 Cursor := crArrow;
                 mcmDropSource.DragMode := dmAutomatic; // Enable dragging the image.
               end;
  IT_PIPETTE : begin
                 Cursor := crPipette;
               end;
  IT_PEN     : begin
                 Cursor := crPencil;
               end;
  IT_LINE    : begin
                 Cursor := crCross;
               end;
  IT_RECT    : begin
                 Cursor := crRectangle;
               end;
  IT_CIRCLE  : begin
                 Cursor := crCircle;
               end;
  IT_REGION  : begin
                 Cursor := crArrow;
                 mcmRegion.Visible := True;
               end;
  IT_PROFILE : begin
                 Cursor := crCross;
               end;
  end;
end; // TFormChild.SetImageTool.


end.
