
{*******************************************************}
{                                                       }
{                 Delphi TKnob Component                }
{       Copyright © 1999, Gary Bradley Hardman.         }
{                 Written 29th May 1997                 }
{                     Version 1.0                       }
{              Released November 29th 1999              }
{   No warranties whatsoever are implied or offered     }
{                    for this unit                      }
{                                                       }
{*******************************************************}

// (Following updates are written by Silhwan Hyun, hyunsh@hanafos.com)
//
// * Functional update
//   - The outer circle area of KNOB is filled with its parent's image(= background image).
//   - Drawing procedures are substituted with antialised procedures.
//   - The image of inner circle area of KNOB can be rotated according to the change of
//     Position, if the inner circle area of KNOB's surface is drawn with some image.
//   - Added new type for indicator mark : circle
//   - Prohibited jumping from minimum Position to maximum Position, or maximum Position to
//     minimum Position when a user drags(rotates) the Knob.
//
// * Added properties,
//   - Transparent : Determines whether the inner circle area of KNOB is to be
//                   filled with its parent's image(= background image).
//                   (default value : false)
//   - KnobImage   : This property is valid only if Transparent is false.
//                  If user assigns a Bitmap image for this property then inner
//                  circle area of KNOB is painted with this image.
//                  Bitmap image is taken by 1:1 copy(non-shrink, non-enlargement),
//                  with matched coordinate of center to center.
//   - ShowIndicator : Determines whether Indicator - mark to show current position -
//                    is to be shown.
//                    If Transparent is false and a Bitmap image is not assigned for
//                    KnobImage then Indicator is shown regardless of the value of
//                    this property.
//                    (default value : true)
//   - IndicatorType : Specifies indicator type("itLine" for line, "itCircle" for
//                     circle).
//                    (default value : itLine)
//   - DrawBorder : Determines whether the border circle of KNOB is to be drawn.
//                    (default value : true)
//   - RotateImage : Determines whether the image on KNOB surface is to be rotated
//                   according to the change of Position.
//                   This property is valid only if Transparent is true or user assigned
//                   a bitmap image to KnobImage (= not fKnobImage.Empty).
//                    (default value : true)
//   - ShowFocusedState : Determines whether the border circle of KNOB is drawn with
//                   dark color at focused state.
//                   This property is valid only if DrawBorder is true.
//                   (default value : true)
//   - Color        : Specifies the color of inner circle area of KNOB.
//                   This property is valid only if Transparent is false.
//                   If a Bitmap image is assigned for KnobImage then only the free space
//                   (for the case the size of Bitmap image is smaller than KNOB size) is
//                   filled with this color.
//                   (default value : clBtnFace)
//   - LineLength    : Specifies the length(IndicatorType = itLine) or the radius(IndicatorType
//                     = itCircle) of indicator mark.
//                   (single type, Min. 3.0 ~ Max. fRadius - 9.0, default value : 3.0)
//   - LineWidth     : Specifies the width of indicator mark.
//                    This property is valid only if IndicatorType is itLine.
//                   (single type, Min. 1.0 ~ Max. 8.0, default value : 2.0)
//
//  Updated Version Number : Version 2.0   ( 11 Mar 2009 )
//
//  Replaced non-antialiased image rotation unit with fast antialiased unit.
//  Updated Version Number : Version 2.1   ( 18 Mar 2009 )
//

unit KNOB;

interface

uses

  WinTypes, WinProcs, Classes, Graphics, Controls, Messages,
  Menus, Stdctrls, SysUtils, Math, AARotate_Fast, EZLines;

type

  TIndicatorType = (itLine, itCircle);

  TKnob = class(TCustomControl)
  private

    fLineColor: TColor;        { Colour of the Indicator line}
    fKnobColor: TColor;        { Colour of the Knob}  // ** Added
    fMin: Integer;             { Minimum value }
    fMax: Integer;             { Maximum value }
    fPosition: Integer;        { From fMin to fMax inclusive }
    fPositionLabel: TLabel;    { A label control to which position info is sent}
    fSpringLoaded: Boolean;    { True - knob returns to zero when released}
    fPageSize: Word;           { The increment/decrement of pgup, pgdown}
    fDiameter: Integer;        { Current knob width/height}
    fRadius: single;           { Half knob-width/height}
    fSteps: Integer;           { Number of steps from Min to Max }
    fAngleInterval: Single;    { The angle each step represents }
    fAngle: Integer;           { The current angle of the indicator }
    fMouseAngle: Integer;      { The current mouse 'angle' over the knob }
    fDragging: Boolean;        { Knob position is being 'changed' }

    fTransparent: Boolean;     // ** Added
    fKnobImage: TBitmap;       // ** Added
    fIndicatorType: TIndicatorType; // ** Added
    fShowIndicator: Boolean;   // ** Added
    fDrawBorder: Boolean;      // ** Added
    fRotateImage: Boolean;     // ** Added
    fShowFocusedState: Boolean; // ** Added
    sImage: TBitmap;           // ** Added   (Source Image)
    mImage: TBitmap;           // ** Added   (Mask Image)
    outer_Image: TBitmap;      // ** Added   (Image of outer circle area)
    fImage: TBitmap;           // ** Added   (Semi-finished Image)

    fLoadedKnobImage: boolean; // ** Added
    fLineLength: single;       // ** Added
    fLineWidth: single;        // ** Added
    fEZLine: TEZLine;          // ** Added

  //  fIndicatorInnerPosition: TPoint;
  //  fIndicatorOuterPosition: TPoint;
  //  fLastIndicatorInnerPosition: TPoint;
  //  fLastIndicatorOuterPosition: TPoint;

    fOnChange: TNotifyEvent;

    procedure SetMin(const NewMinValue: Integer);
    procedure SetMax(const NewMaxValue: Integer);
    procedure SetPosition(const NewPosition: Integer);
    procedure SetParams(APosition, AMin, AMax: Integer);

    procedure SetSteps;
    procedure CalcAngle;
    function  CalcPosition(const TheAngle: Integer): Integer;

    procedure SetPositionLabel(const NewLabel: TLabel);
    procedure ShowPosition(const ThePosition: Integer);
    procedure SetSpringLoaded(const Sprung: Boolean);

    procedure SetLineColor(NewColor : TColor);
    procedure SetKnobColor(NewColor : TColor);  // ** Added

    procedure SetCircleMask;                    // ** Added
 //   procedure SetKnobImage(Value: TBitmap);     // ** Added
    procedure SetTransparent(Value: Boolean);   // ** Added
    procedure SetIndicatorType(Value: TIndicatorType); // ** Added
    procedure SetShowIndicator(Value: Boolean); // ** Added
    procedure SetLineLength(Value: single);     // ** Added
    procedure SetLineWidth(Value: single);      // ** Added
    procedure SetDrawBorder(Value: Boolean);    // ** Added
    procedure SetRotateImage(Value: Boolean);   // ** Added

    {Windows Messages}
    procedure WMCreate(var Message: TWMCreate);
              message WM_CREATE;
    procedure WMSize(var Message: TWMSize);
              message WM_SIZE;
    procedure WMSetFocus(var Message: TWMSETFOCUS);
              message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKILLFOCUS);
              message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode);
              message WM_GETDLGCODE;

    {Delphi Component Messages}
    procedure CMMouseLeave(var Message: TMessage);
              message cm_MouseLeave;
    procedure CMEnabledChanged(var Message: TMessage);
              message cm_EnabledChanged;
    procedure CMVisibleChanged(var Message: TMessage);
              message cm_VisibleChanged;
    procedure CM_ParentColorChanged(var Msg: TMessage);
              message cm_ParentColorChanged;
    procedure CM_TextChanged(var Msg: TMessage);
              message cm_TextChanged;

  protected

    procedure Paint; override;
    procedure PaintKnobFace(DestBMP: TBitmap; RefreshParent: boolean);  // ** Added
    procedure PaintKnobBorder(DestBMP: TBitmap);
    procedure PaintState(DestBMP: TBitmap; Restore : boolean);
    procedure PaintIndicator(DestBMP: TBitmap);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure GetParentImage(Control: TControl; Dest: TCanvas);  // ** Added
    procedure GetOuterCircleImage(Src, Dest: TCanvas);           // ** Added
    procedure PutKnobImage(Dest: TBitmap);                       // ** Added
    procedure AdjustKnobState;                                   // ** Added
    procedure RedrawKnob;                                        // ** Added
    procedure DrawAntialiasedDisk(ABitmap: TBitmap; CenterX, CenterY, Radius,
                                  Feather: single; DiskColor : TColor);  // ** Added
    procedure DrawGradientCircle(ABitmap: TBitmap; CenterX, CenterY, Radius,
                                 LineWidth, Feather: single;
                                 CircleColor_A, CircleColor_B : TColor;
                                 Range_A_S, Range_A_E : word);          // ** Added
    procedure DrawAntialiasedCircle(ABitmap: TBitmap; CenterX, CenterY, Radius,
                                 LineWidth, Feather: single; CircleColor : TColor); // ** Added

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetKnobImage(Value: TBitmap);     // ** Added

  published

  { property Anchors; } {Commented out for compatibility with Delphi 1,2 & 3}

    property ParentShowHint;
    property ShowHint;

    property Max: Integer
      read fMax write SetMax default 127;
    property Min: Integer
      read fMin write SetMin default 0;
    property Caption;
    property LineColor: TColor
      read fLineColor write SetLineColor default clActiveCaption;
    property Position: Integer
      read fPosition write SetPosition default 0;
    property PageSize: Word
      read fPageSize write fPageSize default 10;
    property PositionLabel: TLabel
      read fPositionLabel write SetPositionLabel;
    property SpringLoaded: Boolean
      read fSpringLoaded write SetSpringLoaded default False;

    property Color: TColor              // ** Added
      read fknobColor write SetKnobColor;
    property Transparent: Boolean       // ** Added
      read fTransparent write SetTransparent default False;
    property KnobImage: TBitmap         // ** Added
      read fKnobImage write SetKnobImage;
    property IndicatorType: TIndicatorType  // ** Added
      read fIndicatorType write SetIndicatorType;
    property ShowIndicator: Boolean     // ** Added
      read fShowIndicator write SetShowIndicator;
    property LineLength: single         // ** Added
      read fLineLength write SetLineLength;
    property LineWidth: single          // ** Added
      read fLineWidth write SetLineWidth;
    property DrawBorder: Boolean        // ** Added
      read fDrawBorder write SetDrawBorder default True;
    property RotateImage: Boolean       // ** Added
      read fRotateImage write SetRotateImage;
    property ShowFocusedState: Boolean   // ** Added
      read fShowFocusedState write fShowFocusedState;

    property TabStop
      default True;
    property TabOrder;

    property PopupMenu;
    property Visible;
    property Enabled;

    property OnChange: TNotifyEvent
      read fOnChange write fOnChange;

    property OnClick;      
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

  end;

  procedure Register;

{ TKnob }
implementation

{$INCLUDE DELPHIAREA.INC}


const
  MaxPixelCount = MaxInt div SizeOf(TRGBTriple);

  MinLineLength = 3.0;
  MinLineWidth  = 1.0;
  MaxLineWidth  = 8.0;

  Color_Edge_Dark = TColor($007F7F7F);
  Color_Edge_Bright = TColor($00DFDFDF);
  Color_Edge_Circle = TColor($00EFEFEF);
  Color_Image_Edge = clWhite;
  Color_DisabledBorder = clGray;
  Color_FocusedBorder = clBlack;
  Color_UnfocusedBorder = TColor($003F3F3F);

  Line_OuterPos_Offset1 = 3.0;   // with image, no border line
  Line_OuterPos_Offset2 = 5.0;   // with image, with border line
  Line_OuterPos_Offset3 = 3.0;   // no image, no border line
  Line_OuterPos_Offset4 = 6.0;   // no image, with border line

{Returns the largest of the two parameters, or the first parameter if equal}
function MaxInt(Value1, Value2: Integer) : Integer;
begin
  if Value2 > Value1 then Result := Value2 else Result := Value1;
end;

{Returns the smallest of the two parameters, or the first parameter if equal}
function MinInt(Value1, Value2: Integer) : Integer;
begin
  if Value1 > Value2 then Result := Value2 else Result := Value1;
end;

constructor TKnob.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 40;
  Height := 40;

  fMin := 0;
  fMax:= 127;
  fPosition := 0;
  fPageSize:= 10;
  fSpringLoaded := False;
  IndicatorType := itLine;
  fShowIndicator := true;
  fDrawBorder := true;
  fRotateImage := true;
  fShowFocusedState := true;
  fLineColor := clActiveCaption;
  fLineLength := 3.0;
  fLineWidth := 2.0;
  fDiameter := Width;
  fRadius := fDiameter / 2.0;
  fKnobColor := clBtnFace;
  TabStop := True;

  fKnobImage := TBitmap.Create;  // holds user assigned image for KNOB surface
  sImage := TBitmap.Create;      // source image
  sImage.Width := Width;
  sImage.Height := Height;
  mImage := TBitmap.Create;      // mask image
  mImage.Width := Width;
  mImage.Height := Height;
  outer_Image := TBitmap.Create; // the image of outer circle area
  outer_Image.Width := Width;
  outer_Image.Height := Height;
  fImage := TBitmap.Create;      // finalized image
  fImage.Width := Width;
  fImage.Height := Height;

  fEZLine := TEZLine.Create;
  fEZLine.SetProp(fLineColor, fLineWidth, 1.0{Transparancy}, 0{StartDistance},
                  true{AntiAlias}, true{CutOff}, psSolid{PenStyle}, pmCopy{PenMode});
  ControlStyle := ControlStyle + [csOpaque];
end;

destructor TKnob.Destroy;
begin
   fKnobImage.Free;
   sImage.Free;
   mImage.Free;
   outer_Image.Free;
   fImage.Free;
   fEZLine.Free;

   inherited Destroy;
end;

procedure TKnob.SetPositionLabel(const NewLabel: TLabel);
begin
  if fPositionLabel <> NewLabel then fPositionLabel:= NewLabel;
  if fPositionLabel <> nil then ShowPosition(fPosition);
end;

{If the Position-Label is removed then point PositionLabel to nil }
procedure TKnob.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = fPositionLabel) and (Operation = opRemove) then
    PositionLabel:= nil;
end;

{If the caption changes then re-draw on Position Label as suffix}
procedure TKnob.CM_TextChanged(var MSG: TMessage);
begin
  ShowPosition(fPosition);
end;

{Display the Position value in an associated PositionLabel control}
procedure TKnob.ShowPosition(const ThePosition: Integer);
begin
  if fPositionLabel <> nil then
  fPositionLabel.Caption := Format('%d ' + Caption, [ThePosition]);
end;

procedure TKnob.WMCreate(var Message: TWMCreate);
begin
  inherited;

  SetSteps;
  CalcAngle;
end;

procedure TKnob.WMSize(var Message: TWMSize);
var
  H: Integer;
begin
  if (Height > fDiameter) or (Width > fDiameter) {growing}
    then H := MaxInt(Height, Width) else
  if (Height < fDiameter) or (Width < fDiameter) {shrinking}
    then H := MinInt(Height, Width)
  else H := fDiameter;
  Setbounds(Left,Top,H,H);
  fDiameter := H;
  fRadius := fDiameter / 2.0;
  sImage.Width := H;
  sImage.Height := H;
  mImage.Width := H;
  mImage.Height := H;
  outer_Image.Width := H;
  outer_Image.Height := H;
  fImage.Width := H;
  fImage.Height := H;

  SetCircleMask;

  inherited;
end;

procedure TKnob.AdjustKnobState;
var
   tmpBMP : TBitmap;
begin
   tmpBMP := TBitmap.Create;
   tmpBMP.Width := fDiameter;
   tmpBMP.Height := fDiameter;
   tmpBMP.PixelFormat := pf24bit;
   PaintState(tmpBMP, true);
   PaintIndicator(tmpBMP);
   BitBlt(Self.Canvas.Handle, 0, 0, fDiameter, fDiameter,
          tmpBMP.Canvas.Handle, 0, 0, SRCCOPY);
   tmpBMP.Free;
end;

procedure TKnob.WMSetFocus(var Message: TWMSETFOCUS);
begin
  if fShowFocusedState then
     AdjustKnobState;

  inherited;
end;

procedure TKnob.WMKillFocus(var Message: TWMKILLFOCUS);
begin
  fDragging := False; // Release dragging flag
  if SpringLoaded then Position := 0;
  if fShowFocusedState then
     AdjustKnobState;

  inherited;
end;

procedure TKnob.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewPosition: Integer;
begin
  if fDragging then
  begin
    if SpringLoaded then Position := 0 else
    begin
      NewPosition := CalcPosition(fMouseAngle);
      if Position <> NewPosition then Position := NewPosition;
    end;

    fDragging := False;
    AdjustKnobState;
  end; {if dragging}

  inherited MouseUp(Button,Shift,X,Y);
 end;

procedure TKnob.RedrawKnob;
var
   tmpBMP : TBitmap;
begin
   tmpBMP := TBitmap.Create;
   tmpBMP.Width := fDiameter;
   tmpBMP.Height := fDiameter;
   tmpBMP.PixelFormat := pf24bit;
   PaintKnobFace(tmpBMP, false);
   if fDrawBorder then
      PaintKnobBorder(tmpBMP);
   PaintState(tmpBMP, false);
   PaintIndicator(tmpBMP);
   BitBlt(Self.Canvas.Handle, 0, 0, fDiameter, fDiameter,
          tmpBMP.Canvas.Handle, 0, 0, SRCCOPY);
   tmpBMP.Free;
end;

procedure TKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  RegionHandle: HRgn;
begin
  if Button = mbLeft then
  begin
    RegionHandle:= CreateEllipticRgnIndirect(ClientRect);
    if RegionHandle > 0 then
      if PtInRegion(RegionHandle, X,Y) then
      try
        SetFocus;
        fDragging := True;
        Position:= CalcPosition(fMouseAngle);

        {Ensure the created region is deleted}
      finally
        DeleteObject(RegionHandle);
      end; {try/finally}
  end; {if mbLeft}

  inherited MouseDown(Button,Shift,X,Y);
end;

{This is where the MouseAngle value is changed,
 (the 'Position' value is calculated from fMouseAngle) }
procedure TKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewPosition: Integer;
  NewAngle : Integer;
begin
  if X = Width div 2 then
  begin
    if Y > Height div 2
    then NewAngle := 240 {Correctly 270, but 240 reflects real world knob }
    else NewAngle := 90; {rotation behaviour}
  end
  else
  begin
    NewAngle := Round(ArcTan(((Height div 2)-Y)/(X-(Width div 2)))*180/pi);
    if X < Width div 2 then NewAngle := (NewAngle + 540) MOD 360;
  end;

  {Limit movement to between 240 clockwise through zero to 300 degrees}
  if (NewAngle <= 270) and (NewAngle > 240) then NewAngle := 240;
  if (NewAngle < -60) then NewAngle := -60;

  {Prohibit jumping from minimum to maximum, or maximum to minimum}
  if fDragging then
    if ((fPosition = fMin) and (NewAngle < 90)) or
       ((fPosition = fMax) and (NewAngle > 90)) then
    begin
      inherited MouseMove(Shift,X,Y);
      exit;
    end;

  fMouseAngle := NewAngle;
  {Where would this set Position to, if clicked?}
  NewPosition := CalcPosition(fMouseAngle);

  {if the knob is being dragged and the position has changed then update
   Position to this new position}
  if fDragging and (fPosition <> NewPosition) then Position:= NewPosition;

 {Show the Angle on a label (if associated)}
  ShowPosition(NewPosition);

 {Enable default OnMouseMove processing}
  inherited MouseMove(Shift,X,Y);
end;

{Reset the PositionLabel caption on mouse-exit}
procedure TKnob.CMMouseLeave(var Message: TMessage);
begin
  ShowPosition(Position);

  inherited;
end;

procedure TKnob.CMEnabledChanged(var Message: TMessage);
begin
  {Force the knob to re-paint its new state}
  AdjustKnobState;
    
  inherited;
end;

procedure TKnob.CMVisibleChanged(var Message: TMessage);
begin
  {Show or hide the position Label in sync with the Knob}
  if PositionLabel <> nil then PositionLabel.Visible := Self.Visible;

  inherited;
end;

procedure TKnob.CM_ParentColorChanged(var Msg: TMessage);
begin
  Canvas.Brush.Color:= TWinControl(Parent).Brush.Color;
  Canvas.FillRect(ClientRect);
  Paint;

  inherited;
end;

procedure TKnob.SetSpringLoaded(const Sprung: Boolean);
begin
  if fSpringLoaded <> Sprung then fSpringLoaded := Sprung;
  if Sprung then Position := 0;
end;

procedure TKnob.SetPosition(const NewPosition: Integer);
begin
  SetParams(NewPosition, fMin, fMax);
end;

procedure TKnob.SetMin(const NewMinValue: Integer);
begin
  SetParams(fPosition, NewMinValue, fMax);
end;

procedure TKnob.SetMax(const NewMaxValue: Integer);
begin
  SetParams(fPosition, fMin, NewMaxValue);
end;

{Calculate characteristics of knob when Position, Max or Min are changed}
procedure TKnob.SetParams(APosition, AMin, AMax: Integer);
begin
  if (fMin <> AMin) then {Min has Changed}
  begin
    fMin := AMin;
    SetSteps;     {updates fSteps and fAngleInterval}
  end;

  if (fMax <> AMax) then
  begin
    fMax := AMax;
    SetSteps;     {updates fSteps and fAngleInterval}
  end;

  if fAngleInterval >= 0 then {Max is greater than Min}
  begin
    if APosition < AMin then APosition := AMin;
    if APosition > AMax then APosition := AMax;
  end else
  begin                      {Min is Greater than Max}
    if APosition > AMin then APosition := AMax;
    if APosition < AMax then APosition := AMin;
  end;

  if fPosition <> APosition then fPosition := APosition;

  CalcAngle;                {Set fAngle}

  RedrawKnob;
 // PaintIndicator;           {And paint the new indicator position}
  ShowPosition(Position);   {Update the PositionLabel caption}

  {Fire the OnChange event if not in Designing state}
  if (Assigned(fOnChange)) and not (csDesigning in ComponentState)
  then fOnChange(Self);
end;

{Called whenever Min or Max is changed}
procedure TKnob.SetSteps;
begin
  fSteps := fMax - fMin;
  if fSteps = 0 then fAngleInterval:= 0 else
  begin
    fAngleInterval := 300 / fSteps;
    fSteps := Abs(fSteps);
  end;
end;

{Calculate fAngle based on fMin, fPosition and fAngleInterval}
procedure TKnob.CalcAngle;
begin
  fAngle := 240 - Round((fPosition - fMin) * fAngleInterval);
end;

{Calculate fPosition based on fMin, fMax, Angle parameter and fAngleInterval}
function TKnob.CalcPosition(const TheAngle: Integer): Integer;
begin
  if fAngleInterval = 0 then Result := fMin else
    Result := fMin + Round((240 - TheAngle) / fAngleInterval);
end;

{Set the colour of the indicator line}
procedure TKnob.SetLineColor(NewColor : TColor);
begin
  if (fLineColor <> NewColor) then
  begin
    fLineColor := NewColor;
    fEZLine.LineColor := fLineColor;
    Invalidate;
  end;
end;

procedure TKnob.SetKnobColor(NewColor : TColor);   // ** Added
begin
  if (fKnobColor <> NewColor) then
  begin
    fKnobColor := NewColor;
    Invalidate;
  end;
end;

procedure TKnob.SetCircleMask;                    // ** Added
begin
   with mImage.Canvas, ClientRect do
   begin
      Brush.Style := bsSolid;
      Brush.Color := clBlack;
      FillRect(Rect(0, 0, mImage.Width, mImage.Height));

      Pen.Width := 1;
      Pen.Color := clWhite;
      Brush.Color := clWhite;
      Ellipse(2, 2, Width-2, Height-2)
   end;
end;

procedure TKnob.PaintKnobFace(DestBMP: TBitmap; RefreshParent: boolean);  // ** Added
var
  rImage, tmpBMP2 : TBitmap;
begin
 // Get the parent's image of the area where Knob is located.
 // (Needed to replace the outer circle area of Knob with parent's image)
   if RefreshParent then
  {$IFDEF COMPILER7_UP}
     GetParentImage(Self, DestBMP.Canvas);
  {$ELSE}
  // I have experenced fatal error at loading a unit including TKNOB with Delphi 4.
  // And "GetParentImage" is the cause of the fatal error at design time.
  // Delphi 7 does not shows this error.  Because I do not have Delphi 5 and 6, I am
  //  not sure that this error is persisting with them.
     if not (csDesigning in ComponentState) then
        GetParentImage(Self, DestBMP.Canvas);
  {$ENDIF}

  if ((not fTransparent) and fKnobImage.Empty) then
  begin
    if RefreshParent then begin
     // Save parent's image to sImage
       BitBlt(sImage.Canvas.Handle, 0, 0, fDiameter, fDiameter,
              DestBMP.Canvas.Handle, 0, 0, SRCCOPY);
     end else
     // Restore parent's image from sImage
        BitBlt(DestBMP.Canvas.Handle, 0, 0, fDiameter, fDiameter,
               sImage.Canvas.Handle, 0, 0, SRCCOPY);

   // Draws a disk image on DestBMP
     if fDrawBorder then
        DrawAntialiasedDisk(DestBMP, fRadius, fRadius, fRadius - 3.5,
                            1.4 {Feather}, fknobColor)
     else
        DrawAntialiasedDisk(DestBMP, fRadius, fRadius, fRadius - 1.5,
                            1.4 {Feather}, fknobColor);
  end else
  begin
    // Get the image of outer circle area of Knob
    if RefreshParent then
       GetOuterCircleImage(DestBMP.Canvas, outer_Image.Canvas);

    if fTransparent then
    begin
       if RefreshParent then
       // Save parent's image to sImage
          BitBlt(sImage.Canvas.Handle, 0, 0, fDiameter, fDiameter,
                 DestBMP.Canvas.Handle, 0, 0, SRCCOPY)
    end else if (not fLoadedKnobImage) then
       PutKnobImage(sImage);  // put user assigned image to sImage

    tmpBMP2 := TBitmap.Create;
    tmpBMP2.Width := fDiameter;
    tmpBMP2.Height := fDiameter;

    if fRotateImage then
    begin
    // rImage := CreateRotatedBitmap(sImage, fAngle - 240, clWhite);
      rImage := FastAARotatedBitmap(sImage, -(fAngle - 240), clWhite, false, false, true, 1.0);
    // Copy valid region of rotated image
      BitBlt(tmpBMP2.Canvas.Handle, 0, 0, fDiameter, fDiameter,
             rImage.Canvas.Handle,
             (rImage.Width - fDiameter) div 2, (rImage.Height - fDiameter) div 2, SRCCOPY);
    end else
      BitBlt(tmpBMP2.Canvas.Handle, 0, 0, fDiameter, fDiameter,
             sImage.Canvas.Handle, 0, 0, SRCCOPY);

  // Mask out outer circle area
    BitBlt(tmpBMP2.Canvas.Handle, 0, 0, fDiameter, fDiameter,
           mImage.Canvas.Handle, 0, 0, SRCAND);
  // Combine with outer circle image
    BitBlt(tmpBMP2.Canvas.Handle, 0, 0, fDiameter, fDiameter,
           outer_Image.Canvas.Handle, 0, 0, SRCPAINT);
  // Copy finished image to DestBMP's canvas
    BitBlt(DestBMP.Canvas.Handle, 0, 0, fDiameter, fDiameter,
           tmpBMP2.Canvas.Handle, 0, 0, SRCCOPY);

    tmpBMP2.Free;
    if fRotateImage then
       rImage.Free;
  end;

  if not fDrawBorder then
  // Save finalized Knob Image
     BitBlt(fImage.Canvas.Handle, 0, 0, fDiameter, fDiameter,
            DestBMP.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TKnob.PaintKnobBorder(DestBMP: TBitmap);
// var
//  adjAngle : integer;

begin
  if not fDrawBorder then exit;

 // with DestBMP.Canvas, ClientRect do
 // begin
  {Draw the circular patch - 2 pixels is the width of the highlight
     & shadow arcs, as drawn below. Test showed that drawing it this small
     reduced flicker by not obliterating the arcs before they're re-drawn}
   { if fDrawBorder then
       if (not fTransparent) and fKnobImage.Empty then begin
       // Draw the upper highlighted arc
          Pen.Width := 2;
          Pen.Color := clBtnHighlight;
          Arc(Left+2,Top+2,Right,Bottom,Right,Top,Left,Bottom);

       // Draw the lower shadowed arc
          Pen.Color := clBtnShadow;
          Arc(Left+1,Top+1,Right-1,Bottom-1,Left,Bottom,Right,Top);
       end;  }
 // end; {with DestBMP.Canvas, ClientRect}

   // Above sentences to decorate boundary area of Knob for better looking
   //  are substituted with antialiased drawing procedures.

    if (not fTransparent) and fKnobImage.Empty then
    begin
    // Thes sentences makes Knob looks like being rotated when fAngle is changed.
      if (fAngle >= 150) and (fAngle < 270) then
          DrawGradientCircle(DestBMP, fRadius, fRadius, fRadius - 2.4,
                         1.8{LineWidth}, 1.0{Feather},
                         Color_Edge_Dark, Color_Edge_Bright, fAngle - 150, fAngle - 60)
      else if (fAngle >= -30) and (fAngle < 150) then
          DrawGradientCircle(DestBMP, fRadius, fRadius, fRadius - 2.4,
                         1.8{LineWidth}, 1.0{Feather},
                         Color_Edge_Bright, Color_Edge_Dark, fAngle + 30, fAngle + 120)
      else if (fAngle >= -60) and (fAngle < -30) then
          DrawGradientCircle(DestBMP, fRadius, fRadius, fRadius - 2.4,
                         1.8{LineWidth}, 1.0{Feather},
                         Color_Edge_Dark, Color_Edge_Bright, fAngle + 210, fAngle + 300);
      DrawAntialiasedCircle(DestBMP, fRadius, fRadius, fRadius - 3.8,
                                    0.8, 0.8, Color_Edge_Circle);
   // Following is test code to check the operation of DrawGradientCircle
    {  if fAngle < 0 then adjAngle := fAngle + 360 else adjAngle := fAngle;
      DrawGradientCircle(DestBMP, fRadius, fRadius, fRadius - 10,
                         10.0, 1.0,
                         clGreen, clRed, adjAngle, adjAngle);  }
    end else
       DrawAntialiasedCircle(DestBMP, fRadius, fRadius, fRadius - 2.5,
                             1.0, 1.0, Color_Image_Edge);

 // Save finalized Knob Image
    BitBlt(fImage.Canvas.Handle, 0, 0, fDiameter, fDiameter,
           DestBMP.Canvas.Handle, 0, 0, SRCCOPY);
end;

procedure TKnob.PaintState(DestBMP: TBitmap; Restore : boolean);
var
  BorderColor : TColor;
begin
  if PositionLabel <> nil then PositionLabel.Enabled := Self.Enabled;

 // Restore previously finalized Image.
 // This restoring is needed to prevent repeated color changing(= more similar to
 //  drawing color) of adjacent area of boundary per each call of this procedure.
 // ( The antialiased drawings changes the color of adjacent area of boundary.)
 // Not necessary if DestBMP has been refreshed just before this call.
  if Restore then
     BitBlt(DestBMP.Canvas.Handle, 0, 0, DestBMP.Width, DestBMP.Height,
            fImage.Canvas.Handle, 0, 0, SRCCOPY);

  if not fDrawBorder then exit;

 { with DestBMP.Canvas, ClientRect do
  begin
    Pen.Width:= 1;
    Brush.Style:= bsClear;
    if Enabled then Pen.Color := clBlack else Pen.Color := clBtnShadow;
    Ellipse(0, 0, Width, Height);

    if focused then
       Pen.Color := clBlack
    else Pen.Color := clBtnShadow;
    Arc(Left,Top,Right,Bottom,Right,Top,Left,Bottom);
  end; }

  // Above sentences are substituted with antialiased drawing procedure.
  if not Enabled then
     BorderColor := Color_DisabledBorder
  else if fDragging or (fShowFocusedState and focused) then
     BorderColor := Color_FocusedBorder
  else
     BorderColor := Color_UnfocusedBorder;
  DrawAntialiasedCircle(DestBMP, fRadius, fRadius, fRadius - 1.0,
                                    1.0, 1.0, BorderColor);
end;

{Called when Enabled is changed or the knob loses/gains focus}
procedure TKnob.PaintIndicator(DestBMP: TBitmap);
var
  AngleInRadians, CosAngle, SinAngle: Single;
  Offset : single;
  iRadius, iCenterX, iCenterY : single;

  IndicatorInnerPos_X, IndicatorInnerPos_Y : single;
  IndicatorOuterPos_X, IndicatorOuterPos_Y : single;

begin
  if not(fShowIndicator or ((not fTransparent) and fKnobImage.Empty)) then
     exit;

  AngleInRadians := fAngle*pi/180;
  CosAngle := Cos(AngleInRadians);
  SinAngle := Sin(AngleInRadians);

 { with DestBMP.Canvas do
  begin
    if ((not fTransparent) and (fKnobImage.Empty)) then
    begin  // Erase Indicator
      Pen.Color := fKnobColor;
      Pen.Width := 2;
      with fIndicatorInnerPosition do MoveTo(X+1,Y+1);
      with fIndicatorOuterPosition do LineTo(X+1,Y+1);
      Pen.Width := 1;
      PolyLine([fIndicatorInnerPosition, fIndicatorOuterPosition]);
    end;

    if fShowIndicator or ((not fTransparent) and fKnobImage.Empty) then
    begin
    // Store the current Indicator positions as the 'last' ones before...
      fLastIndicatorInnerPosition := fIndicatorInnerPosition;
      fLastIndicatorOuterPosition := fIndicatorOuterPosition;

    // Calculating the new Indicator positions
      if fDrawBorder then Offset := 7 else Offset := 5;
      fIndicatorInnerPosition.X := Round(fRadius + (fRadius - Offset - fLineLength)*CosAngle);
      fIndicatorInnerPosition.Y := Round(fRadius - (fRadius - Offset - fLineLength)*SinAngle);
      fIndicatorOuterPosition.X := Round(fRadius + (fRadius - Offset)*CosAngle);
      fIndicatorOuterPosition.Y := Round(fRadius - (fRadius - Offset)*SinAngle);

      Pen.Color := fLineColor;
      Pen.Width := 2;
      with fIndicatorInnerPosition do MoveTo(X+1,Y+1);
      with fIndicatorOuterPosition do LineTo(X+1,Y+1);
      Pen.Width := 1;
      PolyLine([fIndicatorInnerPosition, fIndicatorOuterPosition]);
    end;
  end; // with DestBMP.Canvas
      }

 // Above sentences to draw indicator mark on KNOB are substituted with antialiased
 // line drawing procedure.

   if (not fTransparent) and fKnobImage.Empty then
      if fDrawBorder then
         Offset := Line_OuterPos_Offset4
      else
         Offset := Line_OuterPos_Offset3
   else if fDrawBorder then
         Offset := Line_OuterPos_Offset2
      else
         Offset := Line_OuterPos_Offset1;

   if fIndicatorType = itLine then
      Offset := Offset + (fLineWidth / 2.0);
   IndicatorInnerPos_X := fRadius + (fRadius - Offset - fLineLength)*CosAngle;
   IndicatorInnerPos_Y := fRadius - (fRadius - Offset - fLineLength)*SinAngle;
   IndicatorOuterPos_X := fRadius + (fRadius - Offset)*CosAngle;
   IndicatorOuterPos_Y := fRadius - (fRadius - Offset)*SinAngle;
   if fIndicatorType = itLine then
   begin
     fEZLine.StartDistance := 0;
  // fEZLine.Lin2 accepts real type parameters for line position.
     fEZLine.Line2(IndicatorInnerPos_X, IndicatorInnerPos_Y,
                   IndicatorOuterPos_X, IndicatorOuterPos_Y, DestBMP);
   end else
   begin
     iRadius := fLineLength / 2.0;
    { if iRadius <= 5.0 then
        iRadius := 5.0;  }
     iCenterX := fRadius + (fRadius - Offset - 1.0 - iRadius)*CosAngle;
     iCenterY := fRadius - (fRadius - Offset - 1.0 - iRadius)*SinAngle;
     DrawAntialiasedDisk(DestBMP, iCenterX, iCenterY, iRadius,
                            1.6 {Feather}, fLineColor);
   //  if iRadius >= 5.0 then
   //     DrawGradientCircle(DestBMP, iCenterX, iCenterY, iRadius - 1.2,
   //                        1.2{LineWidth}, 1.0{Feather},
   //                        Color_Edge_Dark, Color_Edge_Bright, 90, 180);
   end;
end;

procedure TKnob.GetParentImage(Control: TControl; Dest: TCanvas);  // ** Added
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then
      Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
    {$IFDEF PDJ_2}
    GetViewportOrgEx(DC, @Position);
    {$ELSE}
    GetViewportOrgEx(DC, Position);
    {$ENDIF}
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    {Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0); }
    ///Fixed Thorsten Claus {25.01.2002}
    Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(0));
    Parent.Perform(WM_PAINT, Integer(DC), Integer(0));
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TKnob.GetOuterCircleImage(Src, Dest: TCanvas);   // ** Added
var
  tmpBMP : TBitmap;
begin
 // Make mask image to remove the image of inner circle area
   tmpBMP := TBitmap.Create;
   tmpBMP.Width := fDiameter;
   tmpBMP.Height := fDiameter;
   with tmpBMP.Canvas, ClientRect do
   begin
      Brush.Style := bsSolid;
      Brush.Color := clWhite;
      FillRect(Rect(0, 0, tmpBMP.Width, tmpBMP.Height));

      Pen.Width := 1;
      Pen.Color := clBlack;
      Brush.Color := clBlack;
      Ellipse(2, 2, Width-2, Height-2)
   end;

 // Copy overall area occupied by KNOB
   BitBlt(Dest.Handle, 0, 0, Width, Height,
          Src.Handle, 0, 0, SRCCOPY);
 // Mask out inner circle area of KNOB
   BitBlt(Dest.Handle, 0, 0, Width, Height,
          tmpBMP.Canvas.Handle, 0, 0, SRCAND);
   tmpBMP.Free;
end;

procedure TKnob.PutKnobImage(Dest: TBitmap);  // ** Added
var
  S_Left, S_Top : integer;
  D_Left, D_Top : integer;
  T_Width, T_Height : integer;
 // Source_Size : integer;
begin
 // Fill Dest with fKnobColor
  with Dest.Canvas do
  begin
    Brush.Color := fknobColor;
    Brush.Style := bsSolid;
    FillRect(ClientRect);

    if fKnobImage.Empty then
       exit;

  end;

 // The image quality of StretchBlt's result is very poor. So I decided to apply
 //  1:1 copy mode only.
 //  applied
 { if (fKnobImage.Width >= Dest.Width) and (fKnobImage.Height >= Dest.Height) then
  begin
     if (fKnobImage.Height > fKnobImage.Width) then
     begin
        S_Top := (fKnobImage.Height - fKnobImage.Width) div 2;
        S_Left := 0;
        Source_Size := fKnobImage.Width;
     end else
     if (fKnobImage.Width > fKnobImage.Height) then
     begin
        S_Top := 0;
        S_Left := (fKnobImage.Width - fKnobImage.Height) div 2;
        Source_Size := fKnobImage.Height;
     end else   // fKnobImage.Height = fKnobImage.Width
     begin
        S_Top := 0;
        S_Left := 0;
        Source_Size := fKnobImage.Width;
     end;

     StretchBlt(Dest.Canvas.Handle, 0, 0,
                Dest.Width, Dest.Height,
                fKnobImage.Canvas.Handle, S_Left, S_Top,
                Source_Size, Source_Size, SRCCOPY);
  end else
  begin }

  if Dest.Width > fKnobImage.Width then
  begin
     S_Left := 0;
     D_Left := (Dest.Width - fKnobImage.Width) div 2;
     T_Width := fKnobImage.Width;
  end else
  begin
     S_Left := (fKnobImage.Width - Dest.Width) div 2;
     D_Left := 0;
     T_Width := Dest.Width;
  end;

  if Dest.Height > fKnobImage.Height then
  begin
     S_Top := 0;
     D_Top := (Dest.Height - fKnobImage.Height) div 2;
     T_Height := fKnobImage.Height;
  end else
  begin
     S_Top := (fKnobImage.Height - Dest.Height) div 2;
     D_Top := 0;
     T_Height := Dest.Height;
  end;

  BitBlt(Dest.Canvas.Handle, D_Left, D_Top, T_Width, T_Height,
         fKnobImage.Canvas.Handle, S_Left, S_Top, SRCCOPY);

 //  end;

  fLoadedKnobImage := true;

end;

{Full-bodied paint procedure}
procedure TKnob.Paint;
var
  tmpBMP : TBitmap;
begin
 // Draw on temporary canvas to avoid flickering
  tmpBMP := TBitmap.Create;
  tmpBMP.Width := fDiameter;
  tmpBMP.Height := fDiameter;
  tmpBMP.PixelFormat := pf24bit;
  PaintKnobFace(tmpBMP, true);
  if fDrawBorder then
     PaintKnobBorder(tmpBMP);
  PaintState(tmpBMP, false);
  PaintIndicator(tmpBMP);

 // Copy finished image to Knob canvas
  BitBlt(Self.Canvas.Handle, 0, 0, fDiameter, fDiameter,
         tmpBMP.Canvas.Handle, 0, 0, SRCCOPY);

  tmpBMP.Free;
end;

procedure TKnob.SetKnobImage(Value: TBitmap);   // ** Added
begin
   if (Value = nil) then
   begin
      if (not fKnobImage.Empty) then
      begin
      // free and re-create fKnobImage to revert it to initial state (Empty = true)
         fKnobImage.Free;
         fKnobImage := TBitmap.Create;
      end; 
   end else
   begin
      fKnobImage.Assign(Value);

      if (not fTransparent) then
         PutKnobImage(sImage);
   end;

   Invalidate;
end;

procedure TKnob.SetTransparent(Value: boolean);  // ** Added
begin
   if Value <> fTransparent then
   begin
     fTransparent := Value;
     if (not fTransparent) then
        PutKnobImage(sImage)
     else
        fLoadedKnobImage := false;

     Invalidate;
   end;
end;

procedure TKnob.SetIndicatorType(Value: TIndicatorType); // ** Added
begin
   if Value <> fIndicatorType then
   begin
     fIndicatorType := Value;

     Invalidate;
   end;
end;

procedure TKnob.SetShowIndicator(Value: boolean); // ** Added
begin
   if Value <> fShowIndicator then
   begin
     fShowIndicator := Value;

     Invalidate;
   end;
end;

procedure TKnob.SetLineLength(Value: single);       // ** Added
var
   OrgLength : single;
begin
   if Value <> fLineLength then
   begin
     OrgLength := fLineLength;
     if Value <= MinLineLength then
        fLineLength := MinLineLength
     else if Value >= (fRadius - 9.0) then
        fLineLength := fRadius - 9.0
     else
        fLineLength := Value;

     if fLineLength <> OrgLength then
        Invalidate;
   end;
end;

procedure TKnob.SetLineWidth(Value: single);    // ** Added
var
   OrgWidth : single;
begin
   if Value <> fLineWidth then
   begin
     OrgWidth := fLineWidth;
     if Value <= MinLineWidth then
        fLineWidth := MinLineWidth
     else if Value >= MaxLineWidth then
        fLineWidth := MaxLineWidth
     else
        fLineWidth := Value;

     if fLineWidth <> OrgWidth then
     begin
        fEZLine.LineWidth := fLineWidth;
        Invalidate;
     end;
   end;
end;

procedure TKnob.SetDrawBorder(Value: Boolean); // ** Added
begin
   if Value <> fDrawBorder then
   begin
     fDrawBorder := Value;

     Invalidate;
   end;
end;

procedure TKnob.SetRotateImage(Value: Boolean);   // ** Added
begin
   if Value <> fRotateImage then
   begin
     fRotateImage := Value;

     Invalidate;
   end;
end;

{Knob control to process Arrow, Page and Home/End keys}
procedure TKnob.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  Message.Result := DLGC_WANTARROWS;
end;

procedure TKnob.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_PRIOR : Position:= fPosition + fPageSize;
    VK_NEXT  : Position:= fPosition - fPageSize;
    VK_END   : Position:= fMax;
    VK_HOME  : Position:= fMin;
    VK_LEFT  : Position:= fPosition -1;
    VK_UP    : Position:= fPosition +1;
    VK_RIGHT : Position:= fPosition +1;
    VK_DOWN  : Position:= fPosition -1;
  end;
end;


// ------------------------ Support for Antaliased Drawing -----------------------

type
  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..MaxPixelCount-1] of TRGBTriple;

  TRGBColor = record
    peRed   : byte;
    peGreen : byte;
    peBlue  : byte;
  end;

// This function returns the mixed color of two Colors.
// Parameter "MixRate" controls the mixing rate of two Colors.
function GetMixedColor(ColorA_R, ColorA_G, ColorA_B : byte;
                       ColorB_R, ColorB_G, ColorB_B : byte;
                       MixRate : integer) : TRGBColor;
var
   MixedColor : TRGBColor;
begin
 // MixRate : 0(ColorA only) ~ 255(ColorB only)
   with MixedColor do begin
     peRed   := round(MixRate / 255 * ColorB_R + (255 - MixRate) / 255 * ColorA_R);
     peGreen := round(MixRate / 255 * ColorB_G + (255 - MixRate) / 255 * ColorA_G);
     peBlue  := round(MixRate / 255 * ColorB_B + (255 - MixRate) / 255 * ColorA_B);
   end;

   Result := MixedColor;
end;

function Calc_Angle(CenterX, CenterY, X, Y : single) : integer;
var
   Angle : integer;
   DeltaX, DeltaY : single;
begin

   DeltaX := abs(X - CenterX);
   DeltaY := abs(CenterY - Y);
   if DeltaX = 0 then
      if (CenterY - Y) > 0 then
         Result := 90
      else
         Result := 270
   else if DeltaY = 0 then
      if (X - CenterX) > 0 then
         Result := 0
      else
         Result := 180
   else begin
      Angle := Round(ArcTan(DeltaY / DeltaX) * 180 / pi);
      if ((X - CenterX) > 0) and ((CenterY - Y) > 0) then
         Result := Angle
      else if ((X - CenterX) > 0) and ((CenterY - Y) < 0) then
         Result := 360 - Angle
      else if ((X - CenterX) < 0) and ((CenterY - Y) > 0) then
         Result := 180 - Angle
      else
         Result := 180 + Angle;
   end;
end;

procedure TKnob.DrawAntialiasedDisk(ABitmap: TBitmap; CenterX, CenterY, Radius,
                                    Feather: single; DiskColor : TColor);
// This procedures Draws antialiased Disk on true color (pf32bit) Bitmap.
// This procedure is modified version of the procedure DrawDisk in DrawCirclesMain.pas
//  written by Nils Haeck M.Sc. (Simdesign), which draws antialsed circle on 256 color
//  (pf8bit) palette bitmap .

// Parts outside the circle will not be changed, parts inside will get
// DiskColor, and in the antialiased area (feather), the pixels will
// get values inbetween.
// ***Parameters***
// Bitmap:
//   The bitmap to draw on
// CenterX, CenterY:
//   The center of the circle (float precision). Note that [0, 0] would be the
//   center of the first pixel. To draw in the exact middle of a 100x100 bitmap,
//   use CenterX = 49.5 and CenterY = 49.5
// Radius:
//   The radius of the drawn circle in pixels (float precision)
// Feather:
//   The feather area. Use 1 pixel for a 1-pixel antialiased area. Pixel centers
//   outside 'Radius + Feather / 2' become 0, pixel centers inside 'Radius - Feather/2'
//   become 255. Using a value of 0 will yield a bilevel image. Note that Feather
//   must be equal or smaller than LineWidth (or it will be adjusted internally)

var
  x, y: integer;
  LX, RX, LY, RY: integer;
  Fact: integer;
  RPF2, RMF2: single;
  P: PRGBArray;
  SqY, SqDist: single;
  sqX: array of single;
  ColorVal : longint;
  DiskColor_R, DiskColor_G, DiskColor_B : byte;
  MixedColor : TRGBColor;

begin
  // Determine some helpful values (singles)
   RPF2 := sqr(Radius + Feather/2);
   RMF2 := sqr(Radius - Feather/2);

  // Determine bounds:
 {  LX := MaxInt(floor(CenterX - RPF2), 0);
   RX := MinInt(ceil (CenterX + RPF2), ABitmap.Width - 1);
   LY := MaxInt(floor(CenterY - RPF2), 0);
   RY := MinInt(ceil (CenterY + RPF2), ABitmap.Height - 1);  }
   LX := MaxInt(floor(CenterX - (Radius + Feather/2)), 0);
   RX := MinInt(ceil(CenterX + (Radius + Feather/2)), ABitmap.Width - 1);
   LY := MaxInt(floor(CenterY - (Radius + Feather/2)), 0);
   RY := MinInt(ceil(CenterY + (Radius + Feather/2)), ABitmap.Height - 1);

  // Optimization run: find squares of X first
   SetLength(SqX, RX - LX + 1);
   for x := LX to RX do
     SqX[x - LX] := sqr(x - CenterX);

 // Get intensity of color element (to eleminate redundant operation)
   ColorVal := ColorToRGB(DiskColor);
   DiskColor_R := GetRValue(ColorVal);  // (ColorVal AND $FF);
   DiskColor_G := GetGValue(ColorVal);  // (ColorVal shr 8  AND $FF);
   DiskColor_B := GetBValue(ColorVal);  // (ColorVal shr 16 AND $FF);

  // Loop through Y values
   for y := LY to RY do begin
     P := ABitmap.Scanline[y];
     SqY := Sqr(y - CenterY);
    // Loop through X values
     for x := LX to RX do begin
      // determine squared distance from center for this pixel
       SqDist := SqY + SqX[x - LX];

       if sqdist < RMF2 then begin   // We are inside inner circle ?  Most often..
        // inside the inner circle.. just give the scanline the new color
         P[x].rgbtRed := DiskColor_R;
         P[x].rgbtGreen := DiskColor_G;
         P[x].rgbtBlue := DiskColor_B;
       end else begin
         if sqdist < RPF2 then begin  // We are inside outer circle?
          // We are inbetween the inner and outer bound, now mix the color
           Fact := round(((Radius - sqrt(sqdist)) * 2 / Feather) * 127.5 + 127.5);
           MixedColor := GetMixedColor(P[x].rgbtRed, P[x].rgbtGreen, P[x].rgbtBlue,
                                       DiskColor_R, DiskColor_G, DiskColor_B,
                                       MaxInt(0, MinInt(Fact, 255)));
           P[x].rgbtRed := MixedColor.peRed;
           P[x].rgbtGreen := MixedColor.peGreen;
           P[x].rgbtBlue := MixedColor.peBlue;
         end {else begin
        //  We are completely outside, Nothing to do.
        end};
       end;
     end;
   end;

   SetLength(SqX, 0);
end;

procedure TKnob.DrawGradientCircle(ABitmap: TBitmap; CenterX, CenterY, Radius,
                                   LineWidth, Feather: single;
                                   CircleColor_A, CircleColor_B : TColor;
                                   Range_A_S, Range_A_E : word);
// This procedures Draws antialiased, two color gradient circle on true color (pf32bit)
//  Bitmap.
// Range_A_S and Range_A_E defines non-gradient region for CircleColor_A in degree.
// The symmetrically opposite side is to be the non-gradient region for CircleColor_B.
// If Range_A_S is equal to Range_A_E then there is no non-gradient region. (= gradient
//  in the full range of circle)
//    written by Silhwan Hyun

var
  MixingTable : array[0..359] of single;  // Defines the intensity ratio of CircleColor_B

  tmpVal : word;
  StepInterval : single;
  i, j : integer;
  Range_B_S : word;

  x, y: integer;
  LX, RX, LY, RY: integer;
  Fact: integer;
  ROPF2, ROMF2, RIPF2, RIMF2: single;
  OutRad, InRad: single;
  P: PRGBArray;
  SqY, SqDist: single;
  sqX: array of single;

  ColorVal : longint;
  Angle_ : integer;
  Color_A_R, Color_A_G, Color_A_B : byte;
  Color_B_R, Color_B_G, Color_B_B : byte;
  Circle_R, Circle_G, Circle_B : byte;
  MixedColor : TRGBColor;
  DrawingColor : TRGBColor;

begin
   if Range_A_S <> Range_A_E then
   begin // check validty of parameters
     if Range_A_E < Range_A_S then
     begin
       tmpVal := Range_A_S;
       Range_A_S := Range_A_E;
       Range_A_E := tmpVal;
     end;
     if Range_A_S >= 180 then exit;   // Range_A_S ~ Range_A_E : unmixed color range for Color_A
     if Range_A_E >= (Range_A_S + 180) then exit;
   end else   // Range_A_S = Range_A_E
     if Range_A_S >= 360 then
       exit;

   if Range_A_S = Range_A_E then
   begin
     for i := Range_A_S to (Range_A_S + 359) do
     begin
        j := i;
        if j >= 360 then
           j := j - 360;
      // Fill MixingTable for non-unmixed color range
        MixingTable[j] := (i - Range_A_S) / 359.0;
     end;
   end else
   begin
   // Fill MixingTable for single color range
     for i := Range_A_S to Range_A_E do
     begin
        MixingTable[i] := 0;     // CircleColor_A
        j := i + 180;
        if j >= 360 then
           j := j - 360;
        MixingTable[j] := 1.0;   // CircleColor_B
     end;

   // Fill MixingTable for mixed color range
     Range_B_S := Range_A_S + 180;
     StepInterval := 1.0 /(Range_B_S - Range_A_E);
     for i := (Range_A_E + 1) to (Range_B_S - 1) do
     begin
       MixingTable[i] := (i - Range_A_E ) * StepInterval;
       j := i + 180;
       if j >= 360 then
           j := j - 360;
       MixingTable[j] := 1.0 - MixingTable[i];
     end;
   end;

  // Determine some helpful values (singles)
   OutRad := Radius + LineWidth/2;
   InRad  := Radius - LineWidth/2;
  // Checks
   if Feather > LineWidth then Feather := LineWidth;
   ROPF2 := sqr(OutRad + Feather/2);
   ROMF2 := sqr(OutRad - Feather/2);
   RIPF2 := sqr(InRad  + Feather/2);
   RIMF2 := sqr(InRad  - Feather/2);

  // Determine bounds:
   LX := MaxInt(floor(CenterX - (OutRad + Feather/2)), 0);
   RX := MinInt(ceil (CenterX + (OutRad + Feather/2)), ABitmap.Width - 1);
   LY := MaxInt(floor(CenterY - (OutRad + Feather/2)), 0);
   RY := MinInt(ceil (CenterY + (OutRad + Feather/2)), ABitmap.Height - 1);

  // Optimization run: find squares of X first
   SetLength(SqX, RX - LX + 1);
   for x := LX to RX do
     SqX[x - LX] := sqr(x - CenterX);

  // Get intensity of color element (to eleminate redundant operation)
   ColorVal := ColorToRGB(CircleColor_A);
   Color_A_R := GetRValue(ColorVal);  // (ColorVal AND $FF);
   Color_A_G := GetGValue(ColorVal);  // (ColorVal shr 8  AND $FF);
   Color_A_B := GetBValue(ColorVal);  // (ColorVal shr 16 AND $FF);
   ColorVal := ColorToRGB(CircleColor_B);
   Color_B_R := GetRValue(ColorVal);  // (ColorVal AND $FF);
   Color_B_G := GetGValue(ColorVal);  // (ColorVal shr 8  AND $FF);
   Color_B_B := GetBValue(ColorVal);  // (ColorVal shr 16 AND $FF);

  // Loop through Y values
   for y := LY to RY do begin
     P := ABitmap.Scanline[y];
     SqY := Sqr(y - CenterY);

    // Loop through X values
     for x := LX to RX do begin
      // determine squared distance from center for this pixel
       SqDist := SqY + SqX[x - LX];

       if SqDist > RIMF2 then begin      // not completely inside?
         if SqDist < ROPF2 then begin    // not completely outside?
           Angle_ := Calc_Angle(CenterX, CenterY, x, y);
           DrawingColor := GetMixedColor(Color_A_R, Color_A_G, Color_A_B,
                                          Color_B_R, Color_B_G, Color_B_B,
                                          round(MixingTable[Angle_] * 255));
           Circle_R := DrawingColor.peRed;
           Circle_G := DrawingColor.peGreen;
           Circle_B := DrawingColor.peBlue;

           if SqDist < ROMF2 then begin  // inside outer line ~ feather?
             if SqDist < RIPF2 then begin  // Check if we're in inside feather area
              // We are in the feather area of inner line, now mix the color
               Fact := round(((sqrt(sqdist) - InRad) * 2 / Feather) * 127.5 + 127.5);

              // Sometimes "Invalid floating point operation" error occurs if exception handling
              //  statement is not present at certain cases.
              // But the error messagebox defined in "except" phrase does not appear at any cases
              //  if exception handling statement is present.
              // If someone knows the reason, please let me know. (hyunsh@hanafos.com)
               try
                 MixedColor := GetMixedColor(P[x].rgbtRed, P[x].rgbtGreen, P[x].rgbtBlue,
                                             Circle_R, Circle_G, Circle_B,
                                             MaxInt(0, MinInt(Fact, 255)));
               except
                 MessageBox(0, 'Error occured at internal code of TKnob', 'Error', MB_OK);
               end;

               P[x].rgbtRed := MixedColor.peRed;
               P[x].rgbtGreen := MixedColor.peGreen;
               P[x].rgbtBlue := MixedColor.peBlue;
             end else begin
              // We are just on the line, Use CircleColor.
               P[x].rgbtRed := Circle_R;
               P[x].rgbtGreen := Circle_G;
               P[x].rgbtBlue := Circle_B;
             end;
           end else begin
            // We are in the feather area of outer line, now mix the color
             Fact := round(((OutRad - sqrt(sqdist)) * 2 / Feather) * 127.5 + 127.5);
             MixedColor := GetMixedColor(P[x].rgbtRed, P[x].rgbtGreen, P[x].rgbtBlue,
                                         Circle_R, Circle_G, Circle_B,
                                         MaxInt(0, MinInt(Fact, 255)));
             P[x].rgbtRed := MixedColor.peRed;
             P[x].rgbtGreen := MixedColor.peGreen;
             P[x].rgbtBlue := MixedColor.peBlue;
           end;
         end;  // end of for (SqDist < ROPF2)
       end;  // end of for (SqDist > RIMF2)
     end;  // end of for x := LX to RX
   end;  // end of for y := LY to RY

   SetLength(SqX, 0);
end;

procedure TKnob.DrawAntialiasedCircle(ABitmap: TBitmap; CenterX, CenterY, Radius,
                                      LineWidth, Feather: single; CircleColor : TColor);
// This procedures Draws antialiased circle on true color (pf32bit) Bitmap.
// This procedure is modified version of the procedure DrawCircle in DrawCirclesMain.pas
//  written by Nils Haeck M.Sc. (Simdesign), which draws antialsed circle on 256 color
//  (pf8bit) palette bitmap .

// Parts outside the circle will not be changed, parts inside will get
// LineColor, and in the antialiased area (feather), the pixels will
// get values inbetween.
// ***Parameters***
// Bitmap:
//   The bitmap to draw on
// CenterX, CenterY:
//   The center of the circle (float precision). Note that [0, 0] would be the
//   center of the first pixel. To draw in the exact middle of a 100x100 bitmap,
//   use CenterX = 49.5 and CenterY = 49.5
// Radius:
//   The radius of the drawn circle in pixels (float precision)
// LineWidth
//   The line width of the drawn circle in pixels (float precision)
// Feather:
//   The feather area. Use 1 pixel for a 1-pixel antialiased area. Pixel centers
//   outside 'Radius + Feather / 2' become 0, pixel centers inside 'Radius - Feather/2'
//   become 255. Using a value of 0 will yield a bilevel image. Note that Feather
//   must be equal or smaller than LineWidth (or it will be adjusted internally)

var
  x, y: integer;
  LX, RX, LY, RY: integer;
  Fact: integer;
  ROPF2, ROMF2, RIPF2, RIMF2: single;
  OutRad, InRad: single;
  P: PRGBArray;
  SqY, SqDist: single;
  sqX: array of single;

  ColorVal : longint;
  Circle_R, Circle_G, Circle_B : byte;
  MixedColor : TRGBColor;

begin
  // Determine some helpful values (singles)
  OutRad := Radius + LineWidth/2;
  InRad  := Radius - LineWidth/2;
  // Checks
  if Feather > LineWidth then Feather := LineWidth;
  ROPF2 := sqr(OutRad + Feather/2);
  ROMF2 := sqr(OutRad - Feather/2);
  RIPF2 := sqr(InRad  + Feather/2);
  RIMF2 := sqr(InRad  - Feather/2);

  // Determine bounds:
 { LX := MaxInt(floor(CenterX - ROPF2), 0);
  RX := MinInt(ceil (CenterX + ROPF2), ABitmap.Width - 1);
  LY := MaxInt(floor(CenterY - ROPF2), 0);
  RY := MinInt(ceil (CenterY + ROPF2), ABitmap.Height - 1); }
  LX := MaxInt(floor(CenterX - (OutRad + Feather/2)), 0);
  RX := MinInt(ceil (CenterX + (OutRad + Feather/2)), ABitmap.Width - 1);
  LY := MaxInt(floor(CenterY - (OutRad + Feather/2)), 0);
  RY := MinInt(ceil (CenterY + (OutRad + Feather/2)), ABitmap.Height - 1);

  // Optimization run: find squares of X first
  SetLength(SqX, RX - LX + 1);
  for x := LX to RX do
    SqX[x - LX] := sqr(x - CenterX);

  // Get intensity of color element (to eleminate redundant operation)
  ColorVal := ColorToRGB(CircleColor);
  Circle_R := GetRValue(ColorVal);  // (ColorVal AND $FF);
  Circle_G := GetGValue(ColorVal);  // (ColorVal shr 8  AND $FF);
  Circle_B := GetBValue(ColorVal);  // (ColorVal shr 16 AND $FF);

  // Loop through Y values
  for y := LY to RY do begin
    P := ABitmap.Scanline[y];
    SqY := Sqr(y - CenterY);
    // Loop through X values
    for x := LX to RX do begin
      // determine squared distance from center for this pixel
      SqDist := SqY + SqX[x - LX];

    {  if SqDist < RIMF2 then begin
        // We're completely inside, nothing to do

      end else begin }
      if SqDist > RIMF2 then begin      // not completely inside?
        if SqDist < ROPF2 then begin    // not completely outside?
          if SqDist < ROMF2 then begin  // inside outer line ~ feather?
            if SqDist < RIPF2 then begin  // Check if we're in inside feather area
              // We are in the feather area of inner line, now mix the color
              Fact := round(((sqrt(sqdist) - InRad) * 2 / Feather) * 127.5 + 127.5);
              MixedColor := GetMixedColor(P[x].rgbtRed, P[x].rgbtGreen, P[x].rgbtBlue,
                                          Circle_R, Circle_G, Circle_B,
                                          MaxInt(0, MinInt(Fact, 255)));
              P[x].rgbtRed := MixedColor.peRed;
              P[x].rgbtGreen := MixedColor.peGreen;
              P[x].rgbtBlue := MixedColor.peBlue;
            end else begin
              // We are just on the line, Use CircleColor.
              P[x].rgbtRed := Circle_R;
              P[x].rgbtGreen := Circle_G;
              P[x].rgbtBlue := Circle_B;
            end;
          end else begin
            // We are in the feather area of outer line, now mix the color
            Fact := round(((OutRad - sqrt(sqdist)) * 2 / Feather) * 127.5 + 127.5);
            MixedColor := GetMixedColor(P[x].rgbtRed, P[x].rgbtGreen, P[x].rgbtBlue,
                                        Circle_R, Circle_G, Circle_B,
                                        MaxInt(0, MinInt(Fact, 255)));
            P[x].rgbtRed := MixedColor.peRed;
            P[x].rgbtGreen := MixedColor.peGreen;
            P[x].rgbtBlue := MixedColor.peBlue;
          end;
        end
        {else begin
          // for SqDist >= ROPF2
          //  We're completely outside, nothing to do
        end};

      end;
    end;
  end;

  SetLength(SqX, 0);
end;


// ---------------------- end of  Support for Antaliased Drawing --------------------

procedure Register;
begin
  RegisterComponents('Custom', [TKnob]);
end;

end.

