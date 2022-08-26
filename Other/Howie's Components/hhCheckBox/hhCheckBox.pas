unit hhCheckBox;
                                               
// ThhCheckBox version 2.00 Derived from TPCheck version 3.00
// Freeware Component for for D2,D3,D4,D5,D6
// Copyright © 2000-2001 by Peric
// Birthday of Component 30.03.2001
// E-mail: pericddn@ptt.yu
// If I' find any errors or rubbish in TPCheck please send me Your suggest or Reclamation.

{****
   28 October 2001 TPcheck Version 3.10
   Modifications by Howard Harvey (hharvey@picknowl.com.au):
       Added several more checkbox images, including:
           Blue, Green, Red for each coloured image
           Standard tick,
           Standard cross,
           Black ball
           "RxLib" green checkmark tick
       Added AllowGrayed property
       Added State property
       Corrected spelling for Alignment property
       Corrected spelling for ColorSimpleMargin
       Renamed ImageType property to Glyph (commonality)
       Unified the checkbox image property names
       Tidied up the code
   5 December 2001 ThhCheckBox Version 1.10
       Fixed failure to show tick state when grayed
       Changed grayed background to lighter grey
   16 June 2003 ThhCheckBox Version 1.20
       Added picture to checkbox (renamed to glyphs in V2)
   24 June 2003 ThhCheckBox Version 2.0
       Changed property names, Added multiple glyphs.
**** }

{ **** Glyph details **** }
{ NumGlyphs is set to 1 if no glyphs are present.
  If a new glyph is specified, the following criteria
  will be initially applied in sequence:
  if glyph.width = 2*glyph.height then NumGlyphs = 2
  if glyph.width = 4*glyph.height then NumGlyphs = 4
  if glyph.width = 3*glyph.height then NumGlyphs = 3
  else NumGlyphs = 1  }

{ For NumGlyphs to be > 1 it must divide exactly into the
  total glyph width }
{ Glyph display states }
{ Glyphs    Checked Unchecked Grayed Disabled }
{   1          1       1         1      N/A   }
{   2          1       2         1      N/A   }
{   3          1       2         3      N/A   }
{   4+         1       2         3      4     }

{$IFDEF VER90}
  {$DEFINE HJH_D2}
{$ELSE}
  {$IFDEF VER100}
    {$DEFINE HJH_D3}
  {$ELSE}
    {$IFDEF VER120}
      {$DEFINE HJH_D4}
    {$ELSE}
      {$DEFINE HJH_D5Up}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE HJH_D6}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, MMSystem, StdCtrls;

{$R hhCheckBox.res}

type
  TOnMouseOverEvent = procedure(Sender: TObject) of object;
  TOnMouseOutEvent = procedure(Sender: TObject) of object;
  TLeft=(taRightJustify,taLeftJustify);
  TMargin=(cbsNone,cbsLowered,cbsRaised,cbsSimple);
  TCheckKind=(ckBall, ckBallBlue, ckBallGreen, ckBallRed,
              ckCross, ckCrossBlue,ckCrossGreen,ckCrossRed,
              ckRxLib,
              ckTick, ckTickBlue, ckTickGreen, ckTickRed);

  ThhCustomCheckBox = class(TCustomControl)

  private
    FTransparent : boolean;
    R            : TRect;
    CaptRect     : TRect;
    TransColor   : Tcolor;
    FShowFocused:boolean;
    FFlat:boolean;
    Findex:integer;
    FChecked:boolean;
    FShowHand:boolean;
    FPlay:boolean;
    FColorHot:boolean;
    FCheckKind:TCheckKind;
    FMargin:TMargin;
    FLeft:Tleft;
    FCheckGlyph:TBitmap;
    FNumGlyphs:integer;
    FGlyphOuter:boolean;
    FGlyphWidth:integer ;
    FOnMouseOver: TOnMouseOverEvent;
    FOnMouseOut: TOnMouseOutEvent;
    FColorSimpleMargin:TColor;
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    FShowGlyph: boolean;
    FGlyph: TBitMap;
    FSpacing : integer ;
{$IFDEF HJH_D3}
{$ELSE}
    BiDiFlags: Longint ;
{$ENDIF}
    procedure SetSpacing(value:integer) ;
    procedure SetGlyph(value:TBitMap) ;
    procedure SetGlyphOuter(value:boolean) ;
    procedure SetNumGlyphs(value:integer) ;
    procedure SetShowGlyph(value:boolean) ;
    procedure SetState(Value: TCheckBoxState);
    procedure DrawParentImage (Control: TControl; Dest: TCanvas);
    procedure SetTransparent(value:boolean);
    procedure SetShowFocused(value:boolean);
    procedure SetFlat(value:boolean);
    procedure SetShowHand(AShowHand:boolean);
    procedure SetColorHot(value:boolean);
    procedure SetMargin(value:TMargin);
    procedure DrawCaptionEnabled;
    procedure CreateBrushPattern;
    procedure DrawMargin;
    procedure DrawImage;
    procedure DrawImageHot;
    procedure DrawGlyph;
    procedure DrawCaption;
    procedure SetSound(Value: boolean);
    procedure SetCheckKind(Value:TCheckKind);
    procedure DoEndKey(Sender: TObject);
    procedure CMMouseEnter(var AMsg: TMessage);
              message CM_MOUSEENTER;
    procedure CMMouseLeave(var AMsg: TMessage);
              message CM_MOUSELEAVE;
    procedure SetChecked(AChecked:boolean);
    procedure SetLeft(value:TLeft);
    procedure SetColorSimpleMargin(value:tColor);
    procedure CmEnabledChanged(var Message: TWmNoParams);
              message CM_ENABLEDCHANGED;
    procedure CmParentColorChanged(var Message: TWMNoParams);
              message CM_PARENTCOLORCHANGED;
    procedure CmTextChanged(var Message: TWmNoParams);
              message CM_TEXTCHANGED;
    procedure CmVisibleChanged(var Message: TWmNoParams);
              message CM_VISIBLECHANGED;
    procedure CmParentFontChanged(var Message: TWMNoParams);
              message CM_FONTCHANGED;
    procedure WMLButtonDblClk (var Message: TWMLButtonDown);
              message WM_LBUTTONDBLCLK;
    procedure WMSetFocus(var Message: TWMSetFocus);
              message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus);
              message WM_KILLFOCUS;
    procedure CmDialogChar(var Message: TCMDialogChar);
              message CM_DIALOGCHAR;
  protected
    procedure Click;override;
    procedure Sound(song : pchar);
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
                        X, Y: Integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    property ShowFocused:boolean read FShowFocused write SetShowFocused default True;
    property Flat: boolean read FFlat write SetFlat default False;
    property HotTrack:boolean read FColorHot write SetColorHot default True;
    property OnMouseEnter: TOnMouseOverEvent read FOnMouseOver write FOnMouseOver;
    property OnMouseExit: TOnMouseOutEvent read FOnMouseOut write FOnMouseOut;
    property Checked: boolean read FChecked write SetChecked default False;
    property Alignment: Tleft read FLeft write SetLeft default taRightJustify;
    property CheckKind: TCheckKind read FCheckKind write SetCheckKind default ckRxLib;
    property ShowHandCursor: boolean read FShowHand write SetShowHand default true;
    property PlaySound: boolean read FPlay write SetSound default True;
    property Style: TMargin read FMargin write SetMargin default cbsNone;
    property ColorSimpleMargin: TColor read FColorSimpleMargin write SetColorSimpleMargin default clBlue;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property Glyph: TBitMap read FGlyph write SetGlyph ;
    property GlyphOuter: boolean read FGlyphOuter write SetGlyphOuter default false ;
    property ShowGlyph: Boolean read FShowGlyph write SetShowGlyph default False;
    property Spacing: integer read FSpacing write SetSpacing default 6;
    property NumGlyphs: integer read FNumGlyphs write SetNumGlyphs default 1;

  public

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Transparent:boolean read FTransparent write SetTransparent default False;

  end;

 ThhCheckBox = class (ThhCustomCheckBox)

  published
    property Alignment;
    property AllowGrayed;
    property Caption;
    property Checked;
    property CheckKind;
    property Color;
    property ColorSimpleMargin;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property GlyphOuter;
    property NumGlyphs;
    property HotTrack;
    property ParentColor;
    property ParentFont;
    property PlaySound;
    property PopupMenu;
    property ShowFocused;
    property ShowHandCursor;
    property ShowHint;
    property ShowGlyph;
    property Spacing;
    property State;
    property Style;
    property TabOrder;
    property TabStop default True;
    property Transparent;
    property Visible;
    property OnMouseEnter;
    property OnMouseExit;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;

{$IFDEF HJH_D5up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF HJH_D3}
{$ELSE}
    property DragKind;
    property Anchors;
    property Constraints;
    property BiDiMode;
    property ParentBiDiMode;
    property OnStartDock;
    property OnEndDock;
{$ENDIF}
  end;
  
procedure Register;

implementation

const
  CheckGlyphWidth = 16 ;    { Basic check indicator glyph W,H }

var
  Pattern: TBitmap = nil;
  GlyphNormal : array [TCheckKind] of pchar = (
                'BALL','BALLB','BALLG','BALLR',
                'SCROSS','CROSSB','CROSSG','CROSSR',
                'RXLIB',
                'STICK','TICKB','TICKG','TICKR' ) ;
  GlyphGrayed : array [TCheckKind] of pchar = (
                'BALLS','BALLS','BALLS','BALLS',
                'SCROSSS','CROSSS','CROSSS','CROSSS',
                'RXLIBS',
                'STICKS','TICKS','TICKS','TICKS' ) ;

{ -------------------------------------------------------------------- }

procedure Register;
begin
  RegisterComponents('Howie', [ThhCheckBox]);
end;

{ -------------------------------------------------------------------- }

constructor ThhCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  SetBounds(0, 0, 97, 19);
  FAllowGrayed := false ;
  FChecked     := false;
  FCheckGlyph  := TBitmap.Create;
  FCheckKind   := ckRxLib;
  FColorHot    := True;
  FColorSimpleMargin := clBlue;
  FFlat        := False;
  FGlyph       := TBitMap.Create;
  FGlyphOuter  := false ;
  FGlyphWidth  := 16 ;
  FLeft        := taRightJustify;
  FMargin      := cbsNone;
  FNumGlyphs   := 1 ;
  FPlay        := true;
  FShowFocused := True;
  FShowGlyph   := true ;
  FShowHand    := true;
  FSpacing     := 6 ;
  FState       := cbUnChecked ;
  TabStop      := True;
end;

{ -------------------------------------------------------------------- }

destructor ThhCustomCheckBox.Destroy;
begin
  inherited Destroy;
  FCheckGlyph.Free;
  FGlyph.Free;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetShowFocused(value:boolean);
begin
  if value<>FShowFocused then FShowFocused  := value;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetTransparent(value:boolean);
begin
  if value<>Ftransparent then Ftransparent := value;
  repaint;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DoEndKey(Sender: TObject);
begin
  if assigned(OnClick) then OnClick(Self);
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
  begin
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
      SetFocus;
      if FChecked then SetChecked(False) else SetChecked(True);
    end
    else begin
      inherited;
    end ;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Repaint;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Repaint;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DoEnter;
begin
  inherited DoEnter;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DoExit;
begin
  inherited DoExit;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then DoEndKey(Self);
  inherited KeyDown(Key, Shift);
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetFlat(value:boolean);
begin
  if value<>FFlat then FFlat := value;
  Repaint;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetColorSimpleMargin(value:tColor);
begin
  if value <> FColorSimpleMargin
  then begin
    FColorSimpleMargin := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetColorHot(value:boolean);
begin
  if value <> FColorHot
  then begin
    FColorHot := value;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetGlyphOuter(Value: boolean);
begin
  if value <> FGlyphOuter
  then begin
    FGlyphOuter := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetLeft(Value: TLeft);
begin
  if value <> FLeft
  then begin
    FLeft := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetState(Value: TCheckBoxState);
begin
  if FState <> Value then
  begin
    if (Value = cbGrayed) AND (NOT FAllowGrayed)
    then Value := cbChecked ;
    FState := Value;
    FChecked := (FState <> cbUnChecked) ;
    Repaint ;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.Click;
begin
  inherited;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.MouseDown( Button: TMouseButton; Shift: TShiftState;
                                   X, Y: Integer);
begin
  if Button<>mbleft
  then exit
  else begin
    if not enabled then exit;
    SetFocus;
    case FState of
      cbUnchecked:
        if AllowGrayed
            then FState := cbGrayed
            else FState := cbChecked;
      cbChecked: FState := cbUnchecked;
      cbGrayed:  FState := cbChecked;
    end ;
    Fchecked := FState <> cbUnChecked ;
    Repaint ;
    if FPlay then Sound('KLIK');
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.WMLButtonDblClk (var Message: TWMLButtonDown);
begin
  inherited;
  Click;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetChecked(AChecked:boolean);
begin
  Fchecked := AChecked;
  if Fchecked
  then FState := cbChecked
  else FState := cbUnchecked;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetMargin(Value:TMargin);
begin
  if value <> FMargin
  then begin
    FMargin := value;
    Invalidate;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DrawParentImage(Control: TControl; Dest: TCanvas);
var
  SaveIndex: Integer;
  DC: HDC;
  Position: TPoint;
begin
  with Control do
  begin
    if Parent = nil then Exit;
    DC := Dest.Handle;
    SaveIndex := SaveDC(DC);
{$IFDEF HJH_2}
    GetViewportOrgEx(DC, @Position);
{$ELSE}
    GetViewportOrgEx(DC, Position);
{$ENDIF}
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.paint;
var
  n  : Integer;
  X1 : integer ;
  X2 : integer ;
  X3 : integer ;
  Z  : integer;
  FocusRect:TRect;

begin
  DrawMargin;
  if FTransparent then DrawParentImage(Self, Self.Canvas);
  with Canvas do
  begin
    Z := R.top+(((R.bottom-R.top)-TextHeight(Caption)) div 2);
    begin
      if pos('&', Caption)>0 then n := TextWidth('&') else n := 0;
      X2 := TextWidth(Caption)-n+3 ;
      if (NOT FShowGlyph)
      OR (FLeft = taLeftJustify)
      then X3 := 0
      else X3 := FGlyphWidth + FSpacing ;

      case FLeft of
        taLeftJustify: X1 := R.Left+2 ;
        else           X1 := R.Left+CheckGlyphWidth+2+FSpacing ;
      end;
      FocusRect := Rect( X1+X3, Z,
                         X1+X2+X3, Z+TextHeight(Caption));
    end;
  end;
  
  if FShowFocused     { was ShowFocused }
  then begin
    if Focused
    then DrawFocusrect(Canvas.Handle,Rect( FocusRect.left-1,
                                           FocusRect.top-1,
                                           FocusRect.Right+1,
                                           FocusRect.bottom+1));
  end;
  DrawCaption;
  DrawImage;
  if FChecked then DrawImageHot;
  DrawGlyph;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DrawMargin;
begin
  R := clientRect;
  canvas.brush.color := color;
  Canvas.Brush.Style := bsSolid;
  begin
  case FMargin of
    cbsNone:
      begin
      Frame3d(canvas, R, clBtnFace,clBtnFace,0);
      end;
    cbsLowered:
      begin
      Frame3d(canvas, R,clBtnShadow,clBtnHighlight,1);
      end;
    cbsRaised:
      begin
      Frame3d(canvas, R,clBtnHighlight,clBtnShadow,1);
      end;
    cbsSimple:
      begin
      Frame3d(canvas, R,FColorSimpleMargin,FColorSimpleMargin,1);
      end;
  end;
  end;
  if Findex=1
  then begin
    if Pattern = nil then CreateBrushPattern;
    canvas.Brush.Bitmap := Pattern;
    canvas.FillRect(R);
  end
  else begin
    canvas.FillRect(R);
  end ;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CreateBrushPattern;

var
  X, Y: Integer;

begin
  Pattern := TBitmap.Create;
  Pattern.Width := 8;
  Pattern.Height := 8;
  with Pattern.Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    FillRect(Rect(0, 0, Pattern.Width, Pattern.Height));
    for Y := 0 to 7 do
      for X := 0 to 7 do
        if (Y mod 2) = (X mod 2) then
          Pixels[X, Y] := clBtnHighlight;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CMMouseEnter(var AMsg: TMessage);
begin
  if not enabled then exit;
  if FColorHot
  then begin
    Findex := 1;
    repaint;
  end;
  if FShowHand
  then begin
    Screen.Cursors[1] := LoadCursor(hinstance,'HANDD');
    Cursor := 1;
  end;
  if Assigned(FOnMouseOver) then FOnMouseOver(Self);
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CMMouseLeave(var AMsg: TMessage);
begin
  if not enabled then exit;
  if FColorHot
  then begin
    Findex := 0;
    repaint;
  end;
  if FShowHand
  then begin
    Screen.Cursors[0] := crDefault;
    Cursor := 0;
  end;
  if Assigned(FOnMouseOut) then FOnMouseOut(Self);
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DrawCaption;

{ Draw the caption at the focus rect }

var
  Z        : integer ;
  X1       : integer ;
  X2       : integer ;
  FntColor : TColor;

begin
  with Canvas do
  begin
    Font := Self.Font;
    FntColor := Font.Color;

    if FColorHot
    then
      if Findex=1
      then Font.color := clBlue
      else Font.color :=  FntColor;

    Brush.Style := bsClear;
    Z := R.top+(((R.bottom-R.top)-TextHeight(Caption)) div 2);
    case FLeft of
      taLeftJustify: X1 := R.Left+2+1 ;
      else           X1 := R.Left+CheckGlyphWidth+1 ;
    end;
    
    if (Fleft = taLeftJustify)
    then X2 := 0
    else begin
      if (FShowGlyph) AND (FGlyph.Width > 0)
      then X2 := FGlyphWidth + FSpacing*2
      else X2 := FSpacing ;
    end ;

    CaptRect := Rect( X1+X2, Z,
                      X1+X2+TextWidth(Caption),
                      Z+TextHeight(Caption));
                      
    if not Enabled then DrawCaptionEnabled;
{$IFDEF HJH_D2}
    DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
{$ENDIF}
{$IFDEF HJH_D3}
    DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
{$ELSE}
    DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT or BiDiFlags );
{$ENDIF}
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DrawCaptionEnabled;   //fixed 05.05.2001.

var
  ECaptRect:TRect;

begin
  with canvas do
  begin
     Font := Self.Font;
     brush.style := bsClear;
{$IFDEF HJH_D2}
     DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
{$ENDIF}
{$IFDEF HJH_D3}
     DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
{$ELSE}
      DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT or BiDiFlags );
{$ENDIF}
     font.Color  := clBtnHighlight;

     ECaptRect := Rect(CaptRect.Left+1,CaptRect.top+1,CaptRect.Right+1,CaptRect.Bottom+1);
{$IFDEF HJH_D2}
     DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT );
{$ENDIF}
{$IFDEF HJH_D3}
     DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT );
{$ELSE}
        DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT or BiDiFlags );
{$ENDIF}
     font.color  := clBtnShadow;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DrawImage;

{ Draws the checkbox status indicator }

var
  BitRect : TRect ;
  Image   : TRect;
  Z       : integer;
  X1      : integer ;
  X2      : integer ;

begin
  if FFlat
  then begin
    if enabled
    then FCheckGlyph.Handle := LoadBitmap(hInstance, 'BlankFlat')
    else FCheckGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat');
  end
  else begin
    if enabled
    then FCheckGlyph.Handle := LoadBitmap(hInstance, 'Blank')
    else FCheckGlyph.Handle := LoadBitmap(hInstance, 'Enabled');
  end;

  TransColor := FCheckGlyph.Canvas.Pixels[0, FCheckGlyph.Height - 1];
  Image := Rect(0, 0, FCheckGlyph.Width, FCheckGlyph.Height);
  Z := R.top+(((R.bottom-R.top)-FCheckGlyph.Height) div 2);
  if FGlyphOuter
  then begin
   if FShowGlyph AND (FGlyph.Width > 0)
      then X2 := FGlyphWidth + Fspacing
      else X2 := 0 ;
    case FLeft of
      taLeftJustify:  X1 := R.right - CheckGlyphWidth - X2 - 2 ;
      else            X1 := R.Left + X2 + 2 ;
    end;
  end
  else begin
    case FLeft of
      taLeftJustify:  X1 := R.right - CheckGlyphWidth - 2 ;
      else            X1 := R.Left + 2 ;
    end;
  end ;
  BitRect  := Rect( X1, Z,
                    X1 + FCheckGlyph.width,
                    Z  + FCheckGlyph.height);
{$IFDEF HJH_D2}
  canvas.BrushCopy(Bitrect, FCheckGlyph,Image, clFuchsia);
{$ELSE}
  canvas.BrushCopy(Bitrect, FCheckGlyph,Image, TransColor);
{$ENDIF}
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DrawGlyph;

{ Draw the Glyph image }

var
  BitRect    : TRect ;
  Image      : TRect;
  Z          : integer;
  X1         : integer ;
  GlyphRight : integer ;
  GlyphLeft  : integer ;

begin
(*----
  if FFlat
  then begin
    if enabled
    then FCheckGlyph.Handle := LoadBitmap(hInstance, 'BlankFlat')
    else FCheckGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat');
  end
  else begin
    if enabled
    then FCheckGlyph.Handle := LoadBitmap(hInstance, 'Blank')
    else FCheckGlyph.Handle := LoadBitmap(hInstance, 'Enabled');
  end;
----*)

  if (FGlyph.Width = 0) OR (NOT FShowGlyph) then exit ;
  
  case FNumGlyphs of
    1:
      begin
        GlyphLeft := 0 ;
        GlyphRight := FGlyph.Width ;
      end ;
    2:
      begin
        if FChecked
        then GlyphRight := FGlyphWidth
        else GlyphRight := FGlyphWidth * 2 ;
        GlyphLeft  := GlyphRight - FGlyphWidth ;
      end ;
    else
      begin
        if Enabled OR (FNumGlyphs = 3)
        then begin
          if FState = cbChecked
          then GlyphRight := FGlyphWidth
          else if FState = cbUnchecked
            then GlyphRight := FGlyphWidth * 2
            else GlyphRight := FGlyphWidth * 3 ;
        end
        else begin
          GlyphRight := FGlyphWidth * 4 ;
        end ;
        GlyphLeft  := GlyphRight - FGlyphWidth ;
      end ;
  end ;

  TransColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  Image := Rect(GlyphLeft, 0, GlyphRight, FGlyph.Height);
  Z := R.top+(((R.bottom-R.top)-FGlyph.Height) div 2);
  if FGlyphOuter
  then begin
    case FLeft of
      taLeftJustify:  X1 := R.right - FGlyphWidth - 2;
      else            X1 := R.Left  + 2 ;
    end;
  end
  else begin
    case FLeft of
      taLeftJustify:  X1 := R.right - CheckGlyphWidth - 2 - FGlyphWidth - FSpacing ;
      else            X1 := R.Left  + CheckGlyphWidth + 2 + FSpacing ;
    end;
  end ;
  BitRect  := Rect( X1, Z,
                    X1 + FGlyphwidth,
                    Z  + FGlyph.height);
{$IFDEF HJH_D2}
  canvas.BrushCopy(Bitrect, FGlyph, Image, clFuchsia);
{$ELSE}
  canvas.BrushCopy(Bitrect, FGlyph, Image, TransColor);
{$ENDIF}
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetSpacing(Value:integer);
begin
  if (value >= 0) AND (value <> FSpacing)
  then begin
    FSpacing := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetCheckKind(Value:TCheckKind);
begin
  if value <> FCheckKind
  then begin
    FCheckKind := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetShowGlyph(Value:Boolean);
begin
  if value <> FShowGlyph
  then begin
    FShowGlyph := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetGlyph(Value: TBitMap);

{ Set new glyph and FNumGlyph value }

begin
  FGlyph.Assign(Value);
  if Value = nil then FNumGlyphs := 1
  else begin
    if FGlyph.Width = 2 * FGlyph.Height then FNumGlyphs := 2
    else
    if FGlyph.Width = 4 * FGlyph.Height then FNumGlyphs := 4
    else
    if FGlyph.Width = 3 * FGlyph.Height then FNumGlyphs := 3
    else FNumGlyphs := 1 ;
    FGlyphWidth := FGlyph.Width DIV FNumGlyphs ;
  end ;
  RecreateWnd;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetNumGlyphs(Value:integer);
begin
  if (FGlyph <> nil) AND (value > 0)
  AND (value <> FNumGlyphs)
  AND (((FGlyph.Width DIV value)*value) = FGlyph.Width)
  then begin
    FGlyphWidth := (FGlyph.Width DIV value) ;
    FNumGlyphs := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.DrawImageHot;

var
  BitHotRect : TRect ;
  ImageHot   : TRect ;
  Z          : integer ;
  X1         : integer ;
  X2         : integer ;
  ImageName  : pchar ;

begin

{ Fixes the failure to show grayed ticks when not enabled! }

  if (FState = cbGrayed) OR (fChecked AND NOT enabled)
  then ImageName := GlyphGrayed[FCheckKind]
  else ImageName := GlyphNormal[FCheckKind] ;
  FCheckGlyph.Handle := LoadBitmap(hInstance, ImageName) ;
(*----
  if enabled
  then FCheckGlyph.Handle := LoadBitmap(hInstance, ImageName)
  else begin
    if FFlat
    then FCheckGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat')
    else FCheckGlyph.Handle := LoadBitmap(hInstance, 'Enabled') ;
  end ;
---- *)

  TransColor := FCheckGlyph.Canvas.Pixels[0, FCheckGlyph.Height - 1];
  ImageHot   := Rect(0, 0, FCheckGlyph.Width, FCheckGlyph.Height);
  Z := (R.top+((R.bottom-R.top)-FCheckGlyph.Height) div 2);

  if FGlyphOuter
  then begin
   if FShowGlyph AND (FGlyph.Width > 0)
      then X2 := FGlyphWidth + Fspacing
      else X2 := 0 ;
     case FLeft of
      taLeftJustify:  X1 := R.right - CheckGlyphWidth - X2 - 2 ;
      else            X1 := R.Left + X2 + 2 ;
    end;
  end
  else begin
    case FLeft of
      taLeftJustify:  X1 := R.right - CheckGlyphWidth - 2 ;
      else            X1 := R.Left + 2 ;
    end;
  end ;
  BitHotRect := Rect( X1, Z, X1+FCheckGlyph.width, Z+FCheckGlyph.height);

{$IFDEF HJH_D2}
   canvas.BrushCopy(BitHotrect, FCheckGlyph, ImageHot, clFuchsia);
{$ELSE}
   canvas.BrushCopy(BitHotrect, FCheckGlyph, ImageHot, TransColor);
{$ENDIF}
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetShowHand(AShowHand:boolean);
begin
  fShowHand := AShowHand;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CmParentFontChanged(var Message: TWMNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CmTextChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CmVisibleChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CmParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.CmEnabledChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.Sound(song : pchar);
var
  h: THandle;
  p: pointer;
begin
  h := FindResource(hInstance,song,'WAV');
  h := LoadResource(hInstance, h);
  p := LockResource(h);
  sndPlaySound(p,SND_MEMORY or SND_SYNC);
  UnLockResource(h);
  FreeResource(h);
end;

{ -------------------------------------------------------------------- }

procedure ThhCustomCheckBox.SetSound(Value:boolean);
begin
  FPlay := Value;
end;

end.


