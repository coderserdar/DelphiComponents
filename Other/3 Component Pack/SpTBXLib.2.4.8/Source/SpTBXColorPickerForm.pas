unit SpTBXColorPickerForm;

{==============================================================================
The contents of this file are subject to the SpTBXLib License; you may
not use or distribute this file except in compliance with the
SpTBXLib License.
A copy of the SpTBXLib License may be found in SpTBXLib-LICENSE.txt or at:
  http://www.silverpointdevelopment.com/sptbxlib/SpTBXLib-LICENSE.htm

Alternatively, the contents of this file may be used under the terms of the
Mozilla Public License Version 1.1 (the "MPL v1.1"), in which case the provisions
of the MPL v1.1 are applicable instead of those in the SpTBXLib License.
A copy of the MPL v1.1 may be found in MPL-LICENSE.txt or at:
  http://www.mozilla.org/MPL/

If you wish to allow use of your version of this file only under the terms of
the MPL v1.1 and not to allow others to use your version of this file under the
SpTBXLib License, indicate your decision by deleting the provisions
above and replace them with the notice and other provisions required by the
MPL v1.1. If you do not delete the provisions above, a recipient may use your
version of this file under either the SpTBXLib License or the MPL v1.1.

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The initial developer of this code is Robert Lee.

Requirements:
For Delphi/C++Builder 2009 or newer:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
For Delphi/C++Builder 7-2007:
  - Jordan Russell's Toolbar 2000
    http://www.jrsoftware.org
  - Troy Wolbrink's TNT Unicode Controls
    http://www.tntware.com/delphicontrols/unicode/

Development notes:
  - All the Windows and Delphi bugs fixes are marked with '[Bugfix]'.
  - All the theme changes and adjustments are marked with '[Theme-Change]'.

History:
  -

==============================================================================}

interface

{$BOOLEVAL OFF} // Unit depends on short-circuit boolean evaluation

uses
  Windows, Messages, Classes, SysUtils, Controls, Graphics, ImgList, Forms,
  Menus, StdCtrls, ExtCtrls, ActnList, Dialogs,
  TB2Dock, TB2Toolbar, TB2Item, TB2ExtItems,
  SpTBXSkins, SpTBXItem, SpTBXControls, SpTBXEditors, SpTBXFormPopupMenu,
  SpTBXExtEditors, SpTBXTabs;

type
  { TSpTBXColorPickerDragObject }

  TSpTBXColorPickerDragObject = class(TDragControlObject)
  protected
    function GetDragCursor(Accepted: Boolean; X, Y: Integer): TCursor; override;
  end;

  { TSpTBXColorEditPopupMenu }

  TSpTBXColorEditPopupMenu = class(TSpTBXFormPopupMenu)
  protected
    procedure DoGetPopupFormClass(var AFormClass: TCustomFormClass); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TSpTBXColorPickerForm }

  TSpTBXColorPickerForm = class(TForm)
    Timer1: TTimer;
    imgColorPicker: TImage;
    SpTBXTabControl1: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    SpTBXTabItem2: TSpTBXTabItem;
    SpTBXTabSheet2: TSpTBXTabSheet;
    imgPalette: TImage;
    btnColorPicker: TSpTBXSpeedButton;
    SpTBXTabItem3: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    ImageList1: TImageList;
    SpTBXColorListBox1: TSpTBXColorListBox;
    ColorDialog1: TColorDialog;
    SpTBXPanel1: TSpTBXPanel;
    btnColorNone: TSpTBXSpeedButton;
    btnColorDialog: TSpTBXSpeedButton;
    btnColor: TSpTBXSpeedButton;
    btnLabel: TSpTBXLabel;
    procedure Timer1Timer(Sender: TObject);
    procedure imgPaletteMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure btnColorPickerStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure FormCreate(Sender: TObject);
    procedure btnColorPickerEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure SpTBXTabControl1ActiveTabChange(Sender: TObject;
      TabIndex: Integer);
    procedure SpTBXColorListBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnColorDraw(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
    procedure btnColorNoneClick(Sender: TObject);
    procedure btnColorNoneMouseEnter(Sender: TObject);
    procedure btnColorDialogClick(Sender: TObject);
    procedure btnColorDialogMouseEnter(Sender: TObject);
    procedure btnColorDialogDraw(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
    procedure SpTBXPanel1DrawBackground(Sender: TObject; ACanvas: TCanvas;
      ARect: TRect; const PaintStage: TSpTBXPaintStage;
      var PaintDefault: Boolean);
    procedure btnColorDialogMouseLeave(Sender: TObject);
    procedure btnColorNoneMouseLeave(Sender: TObject);
    procedure imgPaletteMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  protected
    FSelectedColor: TColor;
    FPrevLabelColor: TColor;
    FColorPickerDragObject: TSpTBXColorPickerDragObject;
    procedure CenterImages;
  public
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(AColor: TColor);
    procedure UpdateColorLabel(AColor: TColor; AButtonType: Integer = -1);
    property SelectedColor: TColor read FSelectedColor write FSelectedColor;
  end;

{ Helpers }
procedure SpScreenShot(SourceR: TRect; DestCanvas: TCanvas; DestR: TRect);
procedure SpScreenShotMagnify(DestCanvas: TCanvas; DestR: TRect; DrawCrosshair: Boolean; out CenterColor: TColor; ZoomFactor: Double = 200);

const
  crSpTBXEyeDropper = 103;   // Cursor ID used for Eye Dropper cursor

resourcestring
  SSpTBXTransparentColor = 'Transparent Color';
  SSpTBXColorPicker = 'Color Picker';
  SSpTBXClickAndDrag = 'Drag && Drop';

implementation

{$R *.dfm}

uses
  Themes, UxTheme;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ Helpers }

procedure SpScreenShot(SourceR: TRect; DestCanvas: TCanvas; DestR: TRect);
var
  DesktopDC: HDC;
begin
  DesktopDC := GetDC(0);
  try
    Windows.StretchBlt(
      DestCanvas.Handle, DestR.Left, DestR.Top, DestR.Right - DestR.Left, DestR.Bottom - DestR.Top,
      DesktopDC, SourceR.Left, SourceR.Top, SourceR.Right - SourceR.Left, SourceR.Bottom - SourceR.Top,
      SRCCOPY);
  finally
    ReleaseDC(0, DesktopDC);
  end;
end;

procedure SpScreenShotMagnify(DestCanvas: TCanvas; DestR: TRect; DrawCrosshair: Boolean; out CenterColor: TColor; ZoomFactor: Double = 200);
var
  W, H, zoomW, zoomH: integer;
  CursorP, CenterP: TPoint;
  SourceR: TRect;
  Multiplier: Double;
begin
  GetCursorPos(CursorP);

  W := DestR.Right - DestR.Left;
  H := DestR.Bottom - DestR.Top;

  // Get the zoom width and height
  if ZoomFactor < 0 then ZoomFactor := 100;
  Multiplier := ZoomFactor / 100;
  zoomW := Round(W * Multiplier);
  zoomH := Round(H * Multiplier);

  // Get the zoomed Rect
  SourceR.Left := CursorP.X - (zoomW div 2);
  SourceR.Top := CursorP.Y - (zoomH div 2);
  SourceR.Right := SourceR.Left + zoomW;
  SourceR.Bottom := SourceR.Top + zoomH;

  DestCanvas.Lock;
  try
    // Draw the screenshot
    DestCanvas.FillRect(DestR);
    SpScreenShot(SourceR, DestCanvas, DestR);
    CenterP := Point(W div 2, H div 2);
    CenterColor := DestCanvas.Pixels[CenterP.X, CenterP.Y];

    // Draw the crosshair
    if DrawCrosshair then begin
      DestCanvas.MoveTo(CenterP.X - (CenterP.X div 2), CenterP.Y);
      DestCanvas.LineTo(CenterP.X + (CenterP.X div 2), CenterP.Y);
      DestCanvas.MoveTo(CenterP.X, CenterP.Y - (CenterP.Y div 2));
      DestCanvas.LineTo(CenterP.X, CenterP.Y + (CenterP.Y div 2));
    end;
  finally
    DestCanvas.Unlock;
  end;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXColorPickerDragObject }

function TSpTBXColorPickerDragObject.GetDragCursor(Accepted: Boolean; X,
  Y: Integer): TCursor;
begin
  // Make sure we always use crSpTBXEyeDropper
  Result := crSpTBXEyeDropper;
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXColorEditPopupMenu }

constructor TSpTBXColorEditPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  BorderStyle := pbsSizeableRightBottom;
end;

procedure TSpTBXColorEditPopupMenu.DoGetPopupFormClass(var AFormClass: TCustomFormClass);
begin
  AFormClass := TSpTBXColorPickerForm;
  inherited DoGetPopupFormClass(AFormClass);
end;

//WMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWMWM
{ TSpTBXColorPickerForm }

procedure TSpTBXColorPickerForm.FormCreate(Sender: TObject);
begin
  btnColorPicker.Caption := SSpTBXClickAndDrag;
  SpTBXTabControl1.DoubleBuffered := True;
  imgPalette.Cursor := crSpTBXEyeDropper;
end;

procedure TSpTBXColorPickerForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FColorPickerDragObject);
end;

procedure TSpTBXColorPickerForm.FormResize(Sender: TObject);
begin
  CenterImages;
end;

procedure TSpTBXColorPickerForm.FormShow(Sender: TObject);
begin
  if SkinManager.GetSkinType <> sknSkin then
    SpTBXTabControl1.TabBackgroundColor := clBtnFace;
  UpdateColorLabel(GetSelectedColor);
  CenterImages;
end;

procedure TSpTBXColorPickerForm.CenterImages;
begin
  // Center the images
  case SpTBXTabControl1.ActiveTabIndex of
    0: begin
         imgPalette.Left := (imgPalette.Parent.Width - imgPalette.Width) div 2;
         imgPalette.Top := (imgPalette.Parent.Height - imgPalette.Height) div 2;
       end;
    2: begin
         imgColorPicker.Picture := nil;
       end;
  end;
end;

procedure TSpTBXColorPickerForm.btnColorDraw(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if PaintStage = pstPrePaint then begin
    PaintDefault := False;
    InflateRect(ARect, -3, -3);
    if btnColor.CaptionGlowColor = clNone then
      SpDrawCheckeredBackground(ACanvas, ARect)
    else begin
      ACanvas.Brush.Color := btnColor.CaptionGlowColor;
      ACanvas.FillRect(ARect);
    end;
    SpDrawRectangle(ACanvas, ARect, 0, clBtnShadow, clBtnHighlight);
  end;
end;

procedure TSpTBXColorPickerForm.btnColorDialogClick(Sender: TObject);
var
  EditButton: TSpTBXColorEditButton;
  P: TSpTBXFormPopupMenu;
begin
  P := ActiveFormPopupMenu;
  if Assigned(P) and Assigned(P.PopupComponent) and (P.PopupComponent is TSpTBXColorEditButton) then begin
    EditButton := TSpTBXColorEditButton(P.PopupComponent);
    Parent.Visible := False;
    ColorDialog1.Color := btnColor.CaptionGlowColor;
    if ColorDialog1.Execute then
      EditButton.SelectedColor := ColorDialog1.Color;
    P.ClosePopup(False);
  end;
end;

procedure TSpTBXColorPickerForm.btnColorDialogDraw(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
// Used by btnColorDialog and btnColorNone
var
  B: TSpTBXSpeedButton;
  Flags: Integer;
  State: TSpTBXSkinStatesType;
begin
  if (PaintStage = pstPrePaint) and (Sender is TSpTBXSpeedButton) then begin
    PaintDefault := False;
    B := Sender as TSpTBXSpeedButton;
    if B.MouseInControl then begin
      case SkinManager.GetSkinType of
        sknNone:
          PaintDefault := True;
        sknWindows, sknDelphiStyle:
          begin
            if B.Pushed then Flags := TS_PRESSED
            else Flags := TS_HOT;
            DrawThemeBackground(SpTBXThemeServices.Theme[teToolBar], ACanvas.Handle, TP_BUTTON, Flags, ARect, nil);
          end;
        sknSkin:
          begin
            if B.Pushed then State := sknsPushed
            else State := sknsHotTrack;
            CurrentSkin.PaintBackground(ACanvas, ARect, skncToolbarItem, State, True, True);
          end;
      end;
    end;
  end;
end;

procedure TSpTBXColorPickerForm.btnColorDialogMouseEnter(Sender: TObject);
begin
  FPrevLabelColor := btnColor.CaptionGlowColor;
  btnLabel.Caption := SSpTBXColorPicker;
end;

procedure TSpTBXColorPickerForm.btnColorDialogMouseLeave(Sender: TObject);
begin
  UpdateColorLabel(FPrevLabelColor);
end;

procedure TSpTBXColorPickerForm.btnColorNoneClick(Sender: TObject);
begin
  SetSelectedColor(clNone);
end;

procedure TSpTBXColorPickerForm.btnColorNoneMouseEnter(Sender: TObject);
begin
  FPrevLabelColor := btnColor.CaptionGlowColor;
  UpdateColorLabel(clNone);
end;

procedure TSpTBXColorPickerForm.btnColorNoneMouseLeave(Sender: TObject);
begin
  UpdateColorLabel(FPrevLabelColor);
end;

procedure TSpTBXColorPickerForm.btnColorPickerStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FPrevLabelColor := btnColor.CaptionGlowColor;
  Timer1.Enabled := True;
  FColorPickerDragObject := TSpTBXColorPickerDragObject.Create(btnColorPicker);
  DragObject := FColorPickerDragObject;
end;

procedure TSpTBXColorPickerForm.btnColorPickerEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var
  CursorP: TPoint;
begin
  Timer1.Enabled := False;
  if imgColorPicker.Visible then begin
    imgColorPicker.Visible := False;
    btnColorPicker.Visible := True;
    SpTBXTabControl1.InvalidateBackground;
  end;

  if Assigned(FColorPickerDragObject) then begin
    GetCursorPos(CursorP);
    CursorP := ScreenToClient(CursorP);
    if FColorPickerDragObject.Dropped and not PtInRect(ClientRect, CursorP) then
      SetSelectedColor(btnColor.CaptionGlowColor)
    else
      UpdateColorLabel(FPrevLabelColor);
    FreeAndNil(FColorPickerDragObject);
  end;
end;

procedure TSpTBXColorPickerForm.imgPaletteMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetSelectedColor(btnColor.CaptionGlowColor);
end;

procedure TSpTBXColorPickerForm.imgPaletteMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  C: TColor;
begin
  C := imgPalette.Picture.Bitmap.Canvas.Pixels[X, Y];
  if C <> $00010101 then
    UpdateColorLabel(C);
end;

procedure TSpTBXColorPickerForm.SpTBXTabControl1ActiveTabChange(Sender: TObject;
  TabIndex: Integer);
begin
  CenterImages;
end;

procedure TSpTBXColorPickerForm.SpTBXColorListBox1Click(Sender: TObject);
begin
  SetSelectedColor(SpTBXColorListBox1.Selected);
end;

procedure TSpTBXColorPickerForm.SpTBXPanel1DrawBackground(Sender: TObject;
  ACanvas: TCanvas; ARect: TRect; const PaintStage: TSpTBXPaintStage;
  var PaintDefault: Boolean);
begin
  if PaintStage = pstPrePaint then begin
    PaintDefault := False;
    SpDrawXPDock(ACanvas, ARect);
    SpDrawXPToolbar(ACanvas, ARect, True, False, False, True, False);
  end;
end;

procedure TSpTBXColorPickerForm.Timer1Timer(Sender: TObject);
var
  CursorP: TPoint;
  Zoom: Double;
  C: TColor;
begin
  if not IsIconic(Application.Handle) then begin
    GetCursorPos(CursorP);
    CursorP := ScreenToClient(CursorP);
    if btnColorPicker.Dragging and not PtInRect(ClientRect, CursorP) then begin
      if not imgColorPicker.Visible then begin
        imgColorPicker.Visible := True;
        btnColorPicker.Visible := False;
        SpTBXTabControl1.InvalidateBackground;
      end;
      Zoom := 100 / 5; // x5 factor
      SpScreenShotMagnify(imgColorPicker.Canvas, Rect(0, 0, imgColorPicker.Width, imgColorPicker.Height), True, C, Zoom);
      UpdateColorLabel(C);
    end
    else
      if imgColorPicker.Visible then begin
        imgColorPicker.Visible := False;
        btnColorPicker.Visible := True;
        SpTBXTabControl1.InvalidateBackground;
      end;
  end;
end;

function TSpTBXColorPickerForm.GetSelectedColor: TColor;
var
  B: TSpTBXColorEditButton;
begin
  Result := clNone;
  if Assigned(ActiveFormPopupMenu) then
    if Assigned(ActiveFormPopupMenu.PopupComponent) and (ActiveFormPopupMenu.PopupComponent is TSpTBXColorEditButton) then begin
      B := TSpTBXColorEditButton(ActiveFormPopupMenu.PopupComponent);
      Result := B.SelectedColor;
    end;
end;

procedure TSpTBXColorPickerForm.SetSelectedColor(AColor: TColor);
var
  B: TSpTBXColorEditButton;
begin
  // Inform the ActiveFormPopupMenu that a selection was made.
  UpdateColorLabel(AColor);
  FSelectedColor := AColor;
  if Assigned(ActiveFormPopupMenu) then begin
    if Assigned(ActiveFormPopupMenu.PopupComponent) and (ActiveFormPopupMenu.PopupComponent is TSpTBXColorEditButton) then begin
      B := TSpTBXColorEditButton(ActiveFormPopupMenu.PopupComponent);
      B.SelectedColor := AColor;
    end;
    ActiveFormPopupMenu.ClosePopup(True);
  end;
end;

procedure TSpTBXColorPickerForm.UpdateColorLabel(AColor: TColor; AButtonType: Integer = -1);
begin
  btnColor.CaptionGlowColor := AColor;
  if AColor = clNone then
    btnLabel.Caption := SSpTBXTransparentColor
  else
    btnLabel.Caption := SpColorToHTML(AColor);
end;

end.
