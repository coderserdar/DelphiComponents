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
// $Log:  19426: mcmImageDualView.pas 
//
//    Rev 1.10    2014-02-02 21:09:58  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.9    22/11/2005 18:56:14  mcm    Version: IMG 2.10
// do.
//
//    Rev 1.8    22/11/2005 18:37:04  mcm    Version: IMG 2.10
// Added Get/SetStretchMode used when the image(s) are zoom out to a size less
// than 1:1 pixel-wise.
//
//   Rev 1.7    16-10-2004 12:59:50  mcm    Version: IMG 2.6
// Changed name of internal TmcmImage class.

//
//   Rev 1.6    27-07-2004 15:10:38  mcm    Version: IMG 2.5
// Corrected positioning of the Select view window. 
// Corrected bug disallowing setting the glyphs.
// Added property to set border size.
// Added properties to set view's height and width.

//
//   Rev 1.5    06-11-2003 19:35:06  mcm    Version: IMG 2.0

//
//   Rev 1.4    29-09-2003 18:44:34  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.3    25-09-2003 23:33:02  mcm    Version: IMG 1.5
// Added PaintSourceView method and MoveScaleResult property.
// Added  shortcut keys "+" for zoom in and "-" for zoom out.

//
//   Rev 1.2    25-07-2003 00:18:38  mcm
// Reduced memory usages by modifying SetResultImage/SetSourceImage.
// Fixed problem viewing palette indexed images.

//
//   Rev 1.1    10-03-2003 23:05:10  mcm    Version: IMG 1.3.2
// Changed icon.

//
//   Rev 1.0    08-03-2003 19:08:32  mcm    Version: IMG 1.3.2
// The TmcmImageDualView is intended to be used in dialogues to display the
// source and resulting image from a process. Includes buttons to zoom in and
// out, plus select an area of interest via the TFormSelectView.

unit mcmImageDualView;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, Classes, Controls, Buttons, Graphics, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Buttons, Vcl.ExtCtrls,
     {$ENDIF}
     mcmImage, mcmImageTypeDef, uFormSelectView;

const
  WM_SELECTFIELD = WM_USER + 1;

type
  TmcmImageDualView = class(TWinControl) //TCustomPanel)
  private
    // Private declarations
  protected
    // Protected declarations
    mcmImageCtrlSrc  : TmcmImageCtrl;
    mcmImageCtrlRes  : TmcmImageCtrl;
    sbZoomIn         : TSpeedButton;
    sbSelectField    : TSpeedButton;
    sbZoomOut        : TSpeedButton;

    FOnImageMoved    : TNotifyEvent;
    FResultImage     : TmcmImage;
    FSourceImage     : TmcmImage;
    FLeftDown        : boolean;
    Fsx, Fsy         : integer;
    Fzn, Fzd         : integer;
    FOrigo           : TPoint;
    FCurrent         : TPoint;
    FSaveCursor      : TCursor;
    FSelectView      : TFormSelectView;

    FMoveScaleRes    : boolean;
    FMargin          : word;
    FViewHeight      : integer;
    FViewWidth       : integer;
    FBtnSpace        : integer;

    FSBltMode        : integer; // Stretch mode used when Scale < 1.0
 
    procedure    ReCalcSize;
    function     GetBorderStyle : TmcmBorderStyle;
    procedure    SetBorderStyle(Value : TmcmBorderStyle);

    function     GetViewHeight : integer;
    procedure    SetViewHeight(Value : integer);
    function     GetViewWidth : integer;
    procedure    SetViewWidth(Value : integer);

    function     GetImageSelectView : TBitmap;
    procedure    SetImageSelectView(Value : TBitmap);
    function     GetImageZoomIn : TBitmap;
    procedure    SetImageZoomIn(Value : TBitmap);
    function     GetImageZoomOut : TBitmap;
    procedure    SetImageZoomOut(Value : TBitmap);

    procedure    SetMargin(Value : word);

    function     CheckOrigo : boolean;
    function     GetResultImage : TmcmImage;
    function     GetSourceImage : TmcmImage;
    procedure    SetResultImage(Image : TmcmImage);
    procedure    SetSourceImage(Image : TmcmImage);

    procedure    ImageCtrlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure    ImageCtrlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure    ImageCtrlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure    sbZoomInClick(Sender: TObject);
    procedure    sbZoomOutClick(Sender: TObject);
    procedure    sbSelectFieldMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure    WMSelectField(var Msg : TMessage); Message WM_SELECTFIELD;
  public
    // Public declarations
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetStretchMode : integer;
    procedure   PaintSourceView(Image : TmcmImage);
    procedure   SetStretchMode(Value : integer);
    procedure   UpdateResultView;

    property    ResultImage : TmcmImage
      read      GetResultImage
      write     SetResultImage;
    property    SourceImage : TmcmImage
      read      GetSourceImage
      write     SetSourceImage;
  published
    // Published declarations
    property    Align;
    property    Color;
    property    Enabled;
    property    GlyphSelectView : TBitmap
      read      GetImageSelectView
      write     SetImageSelectView;
    property    GlyphZoomIn : TBitmap
      read      GetImageZoomIn
      write     SetImageZoomIn;
    property    GlyphZoomOut : TBitmap
      read      GetImageZoomOut
      write     SetImageZoomOut;
    property    ImageBorderStyle : TmcmBorderStyle
      read      GetBorderStyle
      write     SetBorderStyle default BS_SINGLE;
    property    Margin : word
      read      FMargin
      write     SetMargin default 8;
    property    MoveScaleResult : boolean
      read      FMoveScaleRes
      write     FMoveScaleRes default True;
    property    ParentColor;
    property    ShowHint;
    property    TabOrder;
    property    TabStop;
    property    ViewHeight : integer
      read      GetViewHeight
      write     SetViewHeight default 153;
    property    ViewWidth : integer
      read      GetViewWidth
      write     SetViewWidth default 153;
    property    Visible;

    property    OnImageMoved : TNotifyEvent
      read      FOnImageMoved
      write     FOnImageMoved;
  end;

  TDualViewImage = class(TmcmImage)
  public
    property    DibInfo;
  end;

implementation

{$R mcmImageDualView.RES}


constructor TmcmImageDualView.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReflector];

  Caption     := ' ';
  FMargin     := 8;
  FBtnSpace   := 39;
  FViewHeight := 153;
  FViewWidth  := 153;
  Height      := FViewHeight + 2 * FMargin;
  Width       := FBtnSpace + 2 * FViewWidth + 2 * FMargin;

  FSBltMode   := HALFTONE;
  
  mcmImageCtrlSrc  := TmcmImageCtrl.Create(Self);
  InsertControl(mcmImageCtrlSrc);
  mcmImageCtrlSrc.BoundsRect := Rect(FMargin, FMargin,
                                     FViewWidth + FMargin, FViewHeight + FMargin);
  mcmImageCtrlSrc.BorderStyle := BS_SINGLE;
  mcmImageCtrlSrc.Center := False;
  mcmImageCtrlSrc.Scale := 1.0;
  mcmImageCtrlSrc.ScaleToFit := False;
  mcmImageCtrlSrc.Transparent := False;
  mcmImageCtrlSrc.OnMouseDown := ImageCtrlMouseDown;
  mcmImageCtrlSrc.OnMouseMove := ImageCtrlMouseMove;
  mcmImageCtrlSrc.OnMouseUp   := ImageCtrlMouseUp;

  mcmImageCtrlRes  := TmcmImageCtrl.Create(Self);
  InsertControl(mcmImageCtrlRes);
  mcmImageCtrlRes.BoundsRect := Rect(FViewWidth + FBtnSpace + FMargin,
                                     FMargin, 2 * FViewWidth + FBtnSpace + FMargin, FViewHeight + FMargin);
  mcmImageCtrlRes.BorderStyle := BS_SINGLE;
  mcmImageCtrlRes.Center := False;
  mcmImageCtrlRes.Scale := 1.0;
  mcmImageCtrlRes.ScaleToFit := False;
  mcmImageCtrlRes.Transparent := False;
  mcmImageCtrlRes.OnMouseDown := ImageCtrlMouseDown;
  mcmImageCtrlRes.OnMouseMove := ImageCtrlMouseMove;
  mcmImageCtrlRes.OnMouseUp   := ImageCtrlMouseUp;

  sbZoomIn := TSpeedButton.Create(Self);
  InsertControl(sbZoomIn);
  sbZoomIn.Enabled := False;
  sbZoomIn.Left := 160 + FMargin;
  sbZoomIn.Top  := 32 + FMargin;
  sbZoomIn.Flat := True;
  sbZoomIn.Glyph.LoadFromResourceName(HInstance, 'MCMDUALVIEWZOOMIN');
  sbZoomIn.NumGlyphs := 1;
  sbZoomIn.OnClick := sbZoomInClick;
  sbZoomIn.Spacing := 0;
  sbZoomIn.Font.Name := 'Small Fonts';
  sbZoomIn.Font.Color := Self.Color;
  sbZoomIn.Font.Height := -1;
  sbZoomIn.Font.PixelsPerInch := 0;
  sbZoomIn.Margin := -1;
  sbZoomIn.Caption := '&+';


  sbSelectField := TSpeedButton.Create(Self);
  InsertControl(sbSelectField);
  sbSelectField.Enabled := False;
  sbSelectField.Glyph.LoadFromResourceName(HInstance, 'MCMDUALVIEWSELECT');
  sbSelectField.NumGlyphs := 1;
  sbSelectField.Left := 160 + FMargin;
  sbSelectField.Top  := 64 + FMargin;
  sbSelectField.Flat := True;
  sbSelectField.OnMouseDown := sbSelectFieldMouseDown;

  sbZoomOut := TSpeedButton.Create(Self);
  InsertControl(sbZoomOut);
  sbZoomOut.Enabled := False;
  sbZoomOut.Glyph.LoadFromResourceName(HInstance, 'MCMDUALVIEWZOOMOUT');
  sbZoomOut.NumGlyphs := 1;
  sbZoomOut.Left := 160 + FMargin;
  sbZoomOut.Top  := 96 + FMargin;
  sbZoomOut.Flat := True;
  sbZoomOut.OnClick := sbZoomOutClick;
  sbZoomOut.Font.Size := 0;
  sbZoomOut.Spacing := 0;
  sbZoomOut.Font.Name := 'Small Fonts';
  sbZoomOut.Font.Color := Self.Color;
  sbZoomOut.Font.Height := -1;
  sbZoomOut.Font.PixelsPerInch := 0;
  sbZoomOut.Margin := -1;
  sbZoomOut.Caption := '&-';

  FMoveScaleRes := True;

  FOnImageMoved := Nil;
  FResultImage := Nil;
  FSourceImage := Nil;
  FLeftDown := False;
  Fzn := 1;
  Fzd := 1;
end; // TmcmImageDualView.Create.


destructor TmcmImageDualView.Destroy;
begin
  if Assigned(mcmImageCtrlSrc)
  then mcmImageCtrlSrc.Free;
  if Assigned(mcmImageCtrlRes)
  then mcmImageCtrlRes.Free;
  if Assigned(sbZoomIn)
  then sbZoomIn.Free;
  if Assigned(sbSelectField)
  then sbSelectField.Free;
  if Assigned(sbZoomOut)
  then sbZoomOut.Free;
  Inherited Destroy;
end; // TmcmImageDualView.Destroy.


function TmcmImageDualView.GetStretchMode : integer;
begin
  Result := FSBltMode;
end; // TmcmImageDualView.GetStretchMode.


procedure TmcmImageDualView.SetStretchMode(Value : integer);
begin
  FSBltMode := Value;
end; // TmcmImageDualView.SetStretchMode.


function TmcmImageDualView.GetImageSelectView : TBitmap;
begin
  Result := sbSelectField.Glyph;
end; // TmcmImageDualView.GetImageSelectView.


procedure TmcmImageDualView.SetImageSelectView(Value : TBitmap);
begin
  sbSelectField.Glyph := Value;
end; // TmcmImageDualView.SetImageSelectView.


function TmcmImageDualView.GetImageZoomIn : TBitmap;
begin
  Result := sbZoomIn.Glyph;
end; // TmcmImageDualView.GetImageZoomIn.


procedure TmcmImageDualView.SetImageZoomIn(Value : TBitmap);
begin
  sbZoomIn.Glyph := Value;
end; // TmcmImageDualView.SetImageZoomIn.


function TmcmImageDualView.GetImageZoomOut : TBitmap;
begin
  Result := sbZoomOut.Glyph;
end; // TmcmImageDualView.GetImageZoomOut.


procedure TmcmImageDualView.SetImageZoomOut(Value : TBitmap);
begin
  sbZoomOut.Glyph := Value;
end; // TmcmImageDualView.SetImageZoomOut.


function TmcmImageDualView.GetResultImage : TmcmImage;
begin
  Result := FResultImage;
end; // TmcmImageDualView.GetResultImage.


function TmcmImageDualView.GetSourceImage : TmcmImage;
begin
  Result := FSourceImage;
end; // TmcmImageDualView.GetSourceImage.


function TmcmImageDualView.GetBorderStyle : TmcmBorderStyle;
begin
  if Assigned(mcmImageCtrlRes)
  then Result := mcmImageCtrlRes.BorderStyle
  else Result := BS_SINGLE;
end; // TmcmImageDualView.GetBorderStyle.


procedure TmcmImageDualView.SetBorderStyle(Value : TmcmBorderStyle);
begin
  mcmImageCtrlSrc.BorderStyle := Value;
  mcmImageCtrlRes.BorderStyle := Value;
end; // TmcmImageDualView.SetBorderStyle.


procedure TmcmImageDualView.ReCalcSize;
var dH : integer;
begin
  Height  := FViewHeight + 2 * FMargin;
  Width   := FBtnSpace + 2 * FViewWidth + 2 * FMargin;

  mcmImageCtrlSrc.BoundsRect := Rect(FMargin, FMargin, FViewWidth + FMargin, FViewHeight + FMargin);
  mcmImageCtrlRes.BoundsRect := Rect(FViewWidth + FBtnSpace + FMargin,
                                     FMargin, 2 * FViewWidth + FBtnSpace + FMargin, FViewHeight + FMargin);

  dH := FViewHeight div 2 + FMargin - (sbZoomIn.Height) div 2;
  sbZoomIn.Left := ((FBtnSpace - sbZoomIn.Width) div 2) + FViewWidth + FMargin;
  sbZoomIn.Top  := dH - 32;

  sbSelectField.Left := sbZoomIn.Left;
  sbSelectField.Top  := dH;

  sbZoomOut.Left := sbZoomIn.Left;
  sbZoomOut.Top  := dH + 32; 

  Invalidate;
end; // TmcmImageDualView.ReCalcSize.


procedure TmcmImageDualView.SetMargin(Value : word);
begin
  if (FMargin <> Value)
  then begin
       FMargin := Value;
       ReCalcSize;
  end;
end; // TmcmImageDualView.SetMargin.


function TmcmImageDualView.GetViewHeight : integer;
begin
  Result := FViewHeight;
end; // TmcmImageDualView.GetViewHeight.


procedure TmcmImageDualView.SetViewHeight(Value : integer);
begin
  if (FViewHeight <> Value)
  then begin
       FViewHeight := Value;
       if (FViewHeight < sbZoomIn.Height + sbSelectField.Height + sbZoomOut.Height + 14)
       then FViewHeight := sbZoomIn.Height + sbSelectField.Height + sbZoomOut.Height + 14;
       ReCalcSize;
  end;
end; // TmcmImageDualView.SetViewHeight.


function TmcmImageDualView.GetViewWidth : integer;
begin
  Result := FViewWidth;
end; // TmcmImageDualView.GetViewWidth.


procedure TmcmImageDualView.SetViewWidth(Value : integer);
begin
  if (FViewWidth <> Value)
  then begin
       FViewWidth := Value;
       ReCalcSize;
  end;
end; // TmcmImageDualView.SetViewWidth.


procedure TmcmImageDualView.SetResultImage(Image : TmcmImage);
begin
  if Assigned(Image)
  then begin
       FResultImage := Image;
       TDualViewImage(mcmImageCtrlRes.Image).SetDIB(TDualViewImage(FResultImage).DibHandle);
       TDualViewImage(mcmImageCtrlRes.Image).FRefCount := 0;

       if Assigned(mcmImageCtrlSrc.Image) and FMoveScaleRes
       then mcmImageCtrlRes.Image.SetOrigo(mcmImageCtrlSrc.Image.GetOrigo);

       // Fire event to get result image processed.
       if Assigned(FOnImageMoved)
       then FOnImageMoved(Self);
       mcmImageCtrlRes.Update;
  end;
end; // TmcmImageDualView.SetResultImage.


procedure TmcmImageDualView.SetSourceImage(Image : TmcmImage);
begin
  if Assigned(Image)
  then begin
       FSourceImage := Image;
       TDualViewImage(mcmImageCtrlSrc.Image).SetDIB(TDualViewImage(FSourceImage).DibHandle);
       TDualViewImage(mcmImageCtrlSrc.Image).FRefCount := 0;

       mcmImageCtrlSrc.Update;
       CheckOrigo;

       // Fire event to get result image processed.
       if Assigned(FOnImageMoved)
       then FOnImageMoved(Self);
  end;
  sbZoomIn.Enabled := (mcmImageCtrlSrc.Image.DibHandle <> 0);
  sbZoomOut.Enabled := sbZoomIn.Enabled;
end; // TmcmImageDualView.SetSourceImage.


procedure TmcmImageDualView.UpdateResultView;
begin
  if Assigned(FResultImage)
  then begin
       mcmImageCtrlRes.Image.Palette := FResultImage.Palette;
       mcmImageCtrlRes.DrawImage;
  end;
end; // TmcmImageDualView.UpdateResultView.


function TmcmImageDualView.CheckOrigo : boolean;
var xs, ys   : integer;
    OldOrigo : TPoint;
begin
  Result := False;
  if (FCurrent.x > 0)
  then FCurrent.x := 0;
  xs := Round(mcmImageCtrlSrc.Scale * mcmImageCtrlSrc.Image.Width);
  if (xs < mcmImageCtrlSrc.Width)
  then FCurrent.x := (mcmImageCtrlSrc.Width - xs) div 2
  else begin
       if (FCurrent.x < (mcmImageCtrlSrc.Width - xs))
       then FCurrent.x := mcmImageCtrlSrc.Width - xs;
  end;

  if (FCurrent.y > 0)
  then FCurrent.y := 0;
  ys := Round(mcmImageCtrlSrc.Scale * mcmImageCtrlSrc.Image.Height);
  if (ys < mcmImageCtrlSrc.Height)
  then FCurrent.y := (mcmImageCtrlSrc.Height - ys) div 2
  else begin
       if (FCurrent.y < mcmImageCtrlSrc.Height - ys)
       then FCurrent.y := mcmImageCtrlSrc.Height - ys;
  end;
  OldOrigo := mcmImageCtrlSrc.Image.GetOrigo;
  if (OldOrigo.x <> FCurrent.x) or (OldOrigo.y <> FCurrent.y)
  then begin
       mcmImageCtrlSrc.Image.SetOrigo(FCurrent);
       Result := True;
  end;

  if (xs <= mcmImageCtrlSrc.Width) and (ys <= mcmImageCtrlSrc.Height)
  then sbSelectField.Enabled := False
  else sbSelectField.Enabled := True;
end; // TmcmImageDualView.CheckOrigo.


procedure TmcmImageDualView.sbZoomInClick(Sender : TObject);
var zf     : double;
    hh, hw : integer;
begin
  if (Fzd > 1)
  then dec(Fzd)
  else inc(Fzn);
  zf := Fzn / Fzd;
  FCurrent := mcmImageCtrlSrc.Image.GetOrigo;
  hh := mcmImageCtrlSrc.Height shr 1;
  hw := mcmImageCtrlSrc.Width shr 1;
  FCurrent.x := Round(zf * (FCurrent.x - hw) / mcmImageCtrlSrc.Scale) + hw;
  FCurrent.y := Round(zf * (FCurrent.y - hh) / mcmImageCtrlSrc.Scale) + hh;
  mcmImageCtrlSrc.Scale := zf;
  if (zf < 1.0)
  then mcmImageCtrlSrc.Image.SetStretchMode(FSBltMode)
  else mcmImageCtrlSrc.Image.SetStretchMode(COLORONCOLOR);

  // Need to check origo of image and adjust this if necessary.
  CheckOrigo;

  if FMoveScaleRes
  then begin
       if (mcmImageCtrlRes.Scale <> mcmImageCtrlSrc.Scale)
       then mcmImageCtrlRes.Scale := mcmImageCtrlSrc.Scale;
       if (zf < 1.0)
       then mcmImageCtrlRes.Image.SetStretchMode(FSBltMode)
       else mcmImageCtrlRes.Image.SetStretchMode(COLORONCOLOR);
       mcmImageCtrlRes.Image.SetOrigo(mcmImageCtrlSrc.Image.GetOrigo);
  end;
  // Fire event to get result image processed.
  if Assigned(FOnImageMoved)
  then FOnImageMoved(Self);
end; // TmcmImageDualView.sbZoomInClick.


procedure TmcmImageDualView.sbZoomOutClick(Sender : TObject);
var zf  : double;
    hh, hw : integer;
begin
  if (Fzn > 1)
  then dec(Fzn)
  else inc(Fzd);
  zf := Fzn / Fzd;
  FCurrent := mcmImageCtrlSrc.Image.GetOrigo;
  hh := mcmImageCtrlSrc.Height shr 1;
  hw := mcmImageCtrlSrc.Width shr 1;
  FCurrent.x := Round(zf * (FCurrent.x - hw) / mcmImageCtrlSrc.Scale) + hw;
  FCurrent.y := Round(zf * (FCurrent.y - hh) / mcmImageCtrlSrc.Scale) + hh;
  mcmImageCtrlSrc.Scale := zf;
  if (zf < 1.0)
  then mcmImageCtrlSrc.Image.SetStretchMode(FSBltMode)
  else mcmImageCtrlSrc.Image.SetStretchMode(COLORONCOLOR);

  // Need to check origo of image and adjust this if necessary.
  CheckOrigo;

  if FMoveScaleRes
  then begin
       if (mcmImageCtrlRes.Scale <> mcmImageCtrlSrc.Scale)
       then mcmImageCtrlRes.Scale := mcmImageCtrlSrc.Scale;
       if (zf < 1.0)
       then mcmImageCtrlRes.Image.SetStretchMode(FSBltMode)
       else mcmImageCtrlRes.Image.SetStretchMode(COLORONCOLOR);
       mcmImageCtrlRes.Image.SetOrigo(mcmImageCtrlSrc.Image.GetOrigo);
  end;
  // Fire event to get result image processed.
  if Assigned(FOnImageMoved)
  then FOnImageMoved(Self);
end; // TmcmImageDualView.sbZoomOutClick.


procedure TmcmImageDualView.ImageCtrlMouseDown(Sender : TObject;
                                               Button : TMouseButton;
                                               Shift  : TShiftState;
                                               X, Y   : Integer);
begin
  if Not(FLeftDown)
  then begin
       FLeftDown := True;
       FSaveCursor := Cursor;
       Fsx := X;
       Fsy := Y;
       FOrigo := mcmImageCtrlSrc.Image.GetOrigo;
  end;
end; // TmcmImageDualView.ImageCtrlMouseDown.


procedure TmcmImageDualView.ImageCtrlMouseMove(Sender : TObject;
                                               Shift  : TShiftState;
                                               X, Y   : Integer);
begin
  if FLeftDown
  then begin
       // Move Source image to another field of view.
       FCurrent := mcmImageCtrlSrc.Image.GetOrigo;
       FCurrent.x := FOrigo.x + (X - Fsx);
       FCurrent.y := FOrigo.y + (Y - Fsy);

       if CheckOrigo
       then begin
            mcmImageCtrlSrc.DrawImage;
            if FMoveScaleRes
            then mcmImageCtrlRes.Image.SetOrigo(mcmImageCtrlSrc.Image.GetOrigo);
            mcmImageCtrlRes.DrawImage;
       end;
  end;
end; // TmcmImageDualView.ImageCtrlMouseMove.


procedure TmcmImageDualView.PaintSourceView(Image : TmcmImage);
begin
  mcmImageCtrlSrc.Image.Draw(Image.Canvas.Handle, mcmImageCtrlSrc.Scale);
end; // TmcmImageDualView.PaintSourceView.


procedure TmcmImageDualView.ImageCtrlMouseUp(Sender : TObject;
                                             Button : TMouseButton;
                                             Shift  : TShiftState;
                                             X, Y   : integer);
begin
  if FLeftDown
  then begin
       FLeftDown := False;
       try
         // Fire event to get result image processed.
         if Assigned(FOnImageMoved)
         then FOnImageMoved(Self);
       finally
         Cursor := FSaveCursor;
       end;

       // Update result window to same origo.
       mcmImageCtrlRes.DrawImage;
  end;
end; // TmcmImageDualView.ImageCtrlMouseUp.


procedure TmcmImageDualView.sbSelectFieldMouseDown(Sender : TObject;
                                                   Button : TMouseButton;
                                                   Shift  : TShiftState;
                                                   X, Y   : integer);
begin
  if (Button <> mbLeft)
  then Exit;
  sbSelectField.Perform(WM_LBUTTONUP, 0, 0);
  Update;
  PostMessage(Handle, WM_SELECTFIELD, 0, 0);
end; // TmcmImageDualView.sbSelectFieldMouseDown.


procedure TmcmImageDualView.WMSelectField(var Msg : TMessage);
var Region     : TRect;
    CursorPos  : TPoint;
    wpt        : TPoint;
    ADC        : HDC;
    MaxW, MaxH : integer;
begin
  Update;
  ReleaseCapture;

  // Save cursor position.
  GetCursorPos(CursorPos);

  // Show a larger image for selecting field of view.
  FSelectView := TFormSelectView.Create(Self);

  wpt.x := 0; // Left;
  wpt.y := 0; // Top;
  wpt := ClientToScreen(wpt);

  // Assign source image and adjust pop-up window size & position.
  FSelectView.Image := mcmImageCtrlSrc.Image;
  FSelectView.Left := wpt.x + sbSelectField.Left - (FSelectView.Width - sbSelectField.Width) div 2;
  FSelectView.Top  := wpt.y + sbSelectField.Top - (FSelectView.Height - sbSelectField.Height) div 2;
  if (FSelectView.Left < 0)
  then FSelectView.Left := 0;
  if (FSelectView.Top < 0)
  then FSelectView.Top := 0;

  // Check that the pop-up window does not exceed screen.
  ADC := GetDC(Handle);
  MaxW := GetDeviceCaps(ADC, HORZRES);
  MaxH := GetDeviceCaps(ADC, VERTRES);
  if (FSelectView.Top + FSelectView.Height > MaxH)
  then FSelectView.Top := MaxH - FSelectView.Height;
  if (FSelectView.Left + FSelectView.Width > MaxW)
  then FSelectView.Left := MaxW - FSelectView.Width;
  ReleaseDC(Handle, ADC);

  // Set region of interest.
  Region.TopLeft := mcmImageCtrlSrc.Image.GetOrigo;
  // Scale coordinates to match image's original size.
  Region.Left := -Round(Region.Left / mcmImageCtrlSrc.Scale);
  Region.Top := -Round(Region.Top / mcmImageCtrlSrc.Scale);
  Region.Right := Round(mcmImageCtrlSrc.Width / mcmImageCtrlSrc.Scale) + Region.Left;
  Region.Bottom := Round(mcmImageCtrlSrc.Height / mcmImageCtrlSrc.Scale) + Region.Top;
  FSelectView.Region := Region;

  FSelectView.Image.SetStretchMode(FSBltMode);
  FSelectView.ShowModal;
  Region := FSelectView.Region; // Get new region.
  FSelectView.Free;

  // Apply regions top-left corner to thumb views.
  Region.Left := -Round(Region.Left * mcmImageCtrlSrc.Scale);
  Region.Top := -Round(Region.Top * mcmImageCtrlSrc.Scale);
  mcmImageCtrlSrc.Image.SetOrigo(Region.TopLeft);
  if FMoveScaleRes
  then mcmImageCtrlRes.Image.SetOrigo(Region.TopLeft);
  mcmImageCtrlSrc.DrawImage;
  // Fire event to get result image processed.
  if Assigned(FOnImageMoved)
  then FOnImageMoved(Self);
  mcmImageCtrlRes.DrawImage;

  // Return cursor to position before click.
  SetCursorPos(CursorPos.x, CursorPos.y);

  sbSelectField.Perform(WM_LBUTTONUP, 0, 0);
  Msg.Result := 0;
end; // TmcmImageDualView.WMSelectField.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
