unit RxCheckBox;

// TRxCheckBox version 1.10 Based on PCheck V3.00
// Freeware Component for for D2,D3,D4,D5,D6
// Copyright © 2000-2001 by Peric
// Birthday of Component 30.03.2001
// E-mail: pericddn@ptt.yu
// If I' find any errors or rubbish in TPCheck please send me Your suggest or Reclamation.

{****
   28 October 2001  Version 3.10
   Modifications by Howard Harvey (hharvey@picknowl.com.au):
       Changed to make compatible with RxCheckListBox:
       Added AllowGrayed property
       Added State property
       Extraneous properties removed
       Changed check marks to match RxLib
       Corrected spelling for Alignment property
       Corrected spelling for ColorSimpleMargin
       Renamed ImageType property to Glyph (commonality)
       Unified the checkbox image property names
       Tidied up the code
**** }

{$IFDEF VER90}
  {$DEFINE PDJ_D2}
{$ELSE}
  {$IFDEF VER100}
    {$DEFINE PDJ_D3}
  {$ELSE}
    {$IFDEF VER120}
      {$DEFINE PDJ_D4}
    {$ELSE}
      {$DEFINE PDJ_D5Up}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE PDJ_D6}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, MMSystem, StdCtrls;

{$R RxCheckBox.res}

type
  TOnMouseOverEvent = procedure(Sender: TObject) of object;
  TOnMouseOutEvent = procedure(Sender: TObject) of object;
  TLeft=(taRightJustify,taLeftJustify);
  TMargin=(bmNone,bmLowered,bmRaised,bmSimple);
  TCheckKind=( ckCheckBox, ckCheckMark);

  TRxCustomCheck = class(TCustomControl)

  private
    FTransparent:boolean;
    R,CaptRect:TRect;
    TransColor:Tcolor;
{$IFDEF PDJ_D3}
{$ELSE}
    BiDiFlags: Longint ;
{$ENDIF}
    FShowFocused:boolean;
    FFlat:boolean;
    index:integer;
    Kolorit:Tcolor;
    FChecked,
    FColorHot:boolean;
    FCheckKind:TCheckKind;
    FMargin:TMargin;
    FLeft:Tleft;
    FGlyph:TBitmap;
    FOnMouseOver: TOnMouseOverEvent;
    FOnMouseOut: TOnMouseOutEvent;
    FColorSimpleMargin:TColor;
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    FHintTwo: string;
    function  GetHintTwo: string;
    procedure SetState(Value: TCheckBoxState);
    procedure DrawParentImage (Control: TControl; Dest: TCanvas);
    procedure SetTransparent(value:boolean);
    procedure SetShowFocused(value:boolean);
    procedure SetFlat(value:boolean);
    procedure SetHintTwo(AHintTwo: string);
    procedure SetColorHot(value:boolean);
    procedure SetMargin(value:TMargin);
    procedure DrawCaptionEnabled;
    procedure CreateBrushPattern;
    procedure DrawMargin;
    procedure DrawImage;
    procedure DrawImageHot;
    procedure DrawCaption;
    procedure SetCheckKind(Value:TCheckKind);
    procedure DoKliknime(Sender: TObject);
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
    property HotTrack:boolean read FColorHot write SetColorHot default False;
    property OnMouseEnter: TOnMouseOverEvent read FOnMouseOver write FOnMouseOver;
    property OnMouseExit: TOnMouseOutEvent read FOnMouseOut write FOnMouseOut;
    property Checked: boolean read FChecked write SetChecked default False;
    property Alignment: Tleft read FLeft write SetLeft default taRightJustify;
    property CheckKind: TCheckKind read FCheckKind write SetCheckKind default ckCheckBox;
    property Style: TMargin read FMargin write SetMargin default bmNone;
    property ColorSimpleMargin: TColor read FColorSimpleMargin write SetColorSimpleMargin default clBlue;
    property HintSecondLine:string read GetHintTwo  write SetHintTwo;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed default False;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;

  public

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Transparent:boolean read FTransparent write SetTransparent default False;

  end;

 TRxCheckBox = class (TRxCustomCheck)

  published
    property Alignment;
    property AllowGrayed;
    property Caption;
    property Checked;
    property Color;
    property ColorSimpleMargin;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Flat;
    property Font;
    property CheckKind;
    property HintSecondLine;
    property HotTrack;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property ShowFocused;
    property ShowHint;
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

{$IFDEF PDJ_D5up}
    property OnContextPopup;
{$ENDIF}
{$IFDEF PDJ_D3}
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

var
  Pattern: TBitmap = nil;
  GlyphNormal : array [TCheckKind] of pchar = (
                'RXSTICK','RXLIBTICK') ;
  GlyphGrayed : array [TCheckKind] of pchar = (
                'RXSTICKS','RXLIBSTICK') ;

{ -------------------------------------------------------------------- }

procedure Register;
begin
  RegisterComponents('Rx Controls', [TRxCheckBox]);
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetShowFocused(value:boolean);
begin
  if value<>FShowFocused then FShowFocused  := value;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetTransparent(value:boolean);
begin
  if value<>Ftransparent then Ftransparent := value;
  repaint;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DoKliknime(Sender: TObject);
begin
  if assigned(OnClick) then OnClick(Self);
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.CMDialogChar(var Message: TCMDialogChar);
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

procedure TRxCustomCheck.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Repaint;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Repaint;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DoEnter;
begin
  inherited DoEnter;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DoExit;
begin
  inherited DoExit;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then doKliknime(Self);
  inherited KeyDown(Key, Shift);
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetFlat(value:boolean);
begin
  if value<>FFlat then FFlat := value;
  Repaint;
end;

{ -------------------------------------------------------------------- }

function TRxCustomCheck.GetHintTwo: string;
begin
  Result := FHintTwo;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetHintTwo(AHintTwo:String);
begin
  FHintTwo := AHintTwo;
  if csDesigning in ComponentState then Exit;
  if hint<>''
  then begin
    if FHintTwo<>''
    then hint := hint+#13+FHintTwo
    else hint := hint;
  end ;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetColorSimpleMargin(value:tColor);
begin
  if value <> FColorSimpleMargin
  then begin
    FColorSimpleMargin := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetColorHot(value:boolean);
begin
  if value <> FColorHot
  then begin
    FColorHot := value;
  end;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetLeft(Value: TLeft);
begin
  if value <> FLeft
  then begin
    FLeft := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetState(Value: TCheckBoxState);
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

procedure TRxCustomCheck.Click;
begin
  inherited;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.MouseDown( Button: TMouseButton; Shift: TShiftState;
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
(*
    if not FChecked
    then begin
      FChecked := true;
      FState := cbChecked ;
      Repaint;
    end
    else begin
      FChecked := false;
      FState := cbUnchecked ;
      Repaint;
    end;
*)
//    if FPlay then Sound('klik');
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.WMLButtonDblClk (var Message: TWMLButtonDown);
begin
  inherited;
  Click;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetChecked(AChecked:boolean);
begin
  Fchecked := AChecked;
  if Fchecked
  then FState := cbChecked
  else FState := cbUnchecked;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetMargin(Value:TMargin);
begin
  if value <> FMargin
  then begin
    FMargin := value;
    Invalidate;
  end;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DrawParentImage(Control: TControl; Dest: TCanvas);
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
{$IFDEF PDJ_2}
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

procedure TRxCustomCheck.paint;
var
  z:integer;
  FocusRect:TRect;
  n: Integer;
begin
  DrawMargin;
  if FTransparent then DrawParentImage(Self, Self.Canvas);
  with Canvas do
  begin
    z := (R.top+((R.bottom-R.top)-TextHeight(Caption)) div 2);
    begin
      if pos('&', Caption)>0 then n := TextWidth('&') else n := 0;
      case FLeft of
        taRightJustify:FocusRect := Rect( R.Left+22, z ,
                                          R.Left+22+TextWidth(Caption)-n+3, z+TextHeight(Caption));
        taLeftJustify:FocusRect := Rect(  R.Left+2, z ,
                                          R.Left+2+TextWidth(Caption)-n+3, z+TextHeight(Caption));
      end;
    end;
  end;
  if ShowFocused
  then begin
    if Focused
    then DrawFocusrect(Canvas.Handle,Rect(FocusRect.left-1,FocusRect.top-1,
                                          FocusRect.Right+1,FocusRect.bottom+1));
  end;
  DrawCaption;
  DrawImage;
{
if not FChecked then
DrawImage else DrawImageHot; }

  if FChecked then DrawImageHot;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DrawMargin;
begin
  R := clientRect;
  canvas.brush.color := color;
  Canvas.Brush.Style := bsSolid;
  begin
  case FMargin of
    bmNone:
      begin
      Frame3d(canvas, R, clBtnFace,clBtnFace,0);
      end;
    bmLowered:
      begin
      Frame3d(canvas, R,clBtnShadow,clBtnHighlight,1);
      end;
    bmRaised:
      begin
      Frame3d(canvas, R,clBtnHighlight,clBtnShadow,1);
      end;
    bmSimple:
      begin
      Frame3d(canvas, R,FColorSimpleMargin,FColorSimpleMargin,1);
      end;
  end;
  end;
  if index=1
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

procedure TRxCustomCheck.CreateBrushPattern;
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

procedure TRxCustomCheck.CMMouseEnter(var AMsg: TMessage);
begin
  if not enabled then exit;
  if FColorHot
  then begin
    index := 1;
    repaint;
  end;
  if Assigned(FOnMouseOver) then FOnMouseOver(Self);
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.CMMouseLeave(var AMsg: TMessage);
begin
  if not enabled then exit;
  if FColorHot
  then begin
    index := 0;
    repaint;
  end;
  if Assigned(FOnMouseOut) then FOnMouseOut(Self);
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DrawCaption;
var
  x:integer;
begin
  with Canvas do
  begin
    Font := Self.Font;
    Kolorit := Font.Color;

    if FColorHot
    then
      if index=1
      then Font.color := clBlue
      else Font.color :=  Kolorit;

    Brush.Style := bsClear;
    x := (R.top+((R.bottom-R.top)-TextHeight(Caption)) div 2);
    case FLeft of
      taRightJustify:CaptRect := Rect(R.Left+22, x , R.Left+22+TextWidth(Caption), x+TextHeight(Caption));
      taLeftJustify:CaptRect := Rect(R.Left+2, x , R.Left+2+TextWidth(Caption), x+TextHeight(Caption));
    end;
    if not Enabled then DrawCaptionEnabled;
{$IFDEF PDJ_D2}
    DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
{$ENDIF}
{$IFDEF PDJ_D3}
    DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
{$ELSE}
    DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT or BiDiFlags );
{$ENDIF}
  end;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DrawCaptionEnabled;   //fixed 05.05.2001.
var ECaptRect:TRect;
begin
  with canvas do
  begin
     Font := Self.Font;
     brush.style := bsClear;
{$IFDEF PDJ_D2}
     DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
{$ENDIF}
{$IFDEF PDJ_D3}
     DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
{$ELSE}
      DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT or BiDiFlags );
{$ENDIF}
     font.Color  := clBtnHighlight;

     ECaptRect := Rect(CaptRect.Left+1,CaptRect.top+1,CaptRect.Right+1,CaptRect.Bottom+1);
{$IFDEF PDJ_D2}
     DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT );
{$ENDIF}
{$IFDEF PDJ_D3}
     DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT );
{$ELSE}
        DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT or BiDiFlags );
{$ENDIF}
     font.color  := clBtnShadow;
  end;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DrawImage;
var
  BitRect,Image: TRect;
  x:integer;
begin
  if not FFlat
  then begin
    if enabled
    then FGlyph.Handle := LoadBitmap(hInstance, 'RxBlank')
    else FGlyph.Handle := LoadBitmap(hInstance, 'RxEnabled');
  end
  else begin
    if enabled
    then FGlyph.Handle := LoadBitmap(hInstance, 'RxBlankFlat')
    else FGlyph.Handle := LoadBitmap(hInstance, 'RxEnabledFlat');
  end;

  TransColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  Image := Rect(0, 0, FGlyph.Width, FGlyph.Height);
  x := (R.top+((R.bottom-R.top)-FGlyph.Height) div 2);
  case FLeft of
    taRightJustify:BitRect := Rect( R.Left+2, x ,
                                    R.Left+2+FGlyph.width, x+FGlyph.height);
    taLeftJustify:BitRect  := Rect( R.right-18, x ,
                                    R.right-18+FGlyph.width, x+FGlyph.height);
  end;
{$IFDEF PDJ_D2}
  canvas.BrushCopy(Bitrect, FGlyph,Image, clFuchsia);
{$ELSE}
  canvas.BrushCopy(Bitrect, FGlyph,Image, TransColor);
{$ENDIF}
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.SetCheckKind(Value:TCheckKind);
begin
  if value <> FCheckKind
  then begin
    FCheckKind := value;
    RecreateWnd;
  end;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.DrawImageHot;
var
  BitHotRect,ImageHot: TRect;
  x:integer;
  ImageName : pchar ;

begin
  if FState = cbGrayed
  then ImageName := GlyphGrayed[FCheckKind]
  else ImageName := GlyphNormal[FCheckKind] ;

  if enabled
  then FGlyph.Handle := LoadBitmap(hInstance, ImageName)
  else begin
    if not FFlat
    then FGlyph.Handle := LoadBitmap(hInstance, 'RxEnabled')
    else FGlyph.Handle := LoadBitmap(hInstance, 'RxEnabledFlat') ;
  end ;
  TransColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  ImageHot := Rect(0, 0, FGlyph.Width, FGlyph.Height);
  x := (R.top+((R.bottom-R.top)-FGlyph.Height) div 2);

  case FLeft of
    taRightJustify:BitHotRect := Rect( R.Left+2, x ,
                                      R.Left+2+FGlyph.width, x+FGlyph.height);
    taLeftJustify: BitHotRect := Rect( R.right-18, x ,
                                      R.right-18+FGlyph.width, x+FGlyph.height);
  end;

{$IFDEF PDJ_D2}
   canvas.BrushCopy(BitHotrect, FGlyph,ImageHot, clFuchsia);
{$ELSE}
   canvas.BrushCopy(BitHotrect, FGlyph,ImageHot, TransColor);
{$ENDIF}
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.CmParentFontChanged(var Message: TWMNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.CmTextChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.CmVisibleChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.CmParentColorChanged(var Message: TWMNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.CmEnabledChanged(var Message: TWmNoParams);
begin
  inherited;
  Invalidate;
end;

{ -------------------------------------------------------------------- }

procedure TRxCustomCheck.Sound(song : pchar);
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

constructor TRxCustomCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  SetBounds(0, 0, 97, 19);
  FAllowGrayed := false ;
  Fchecked  := false;
  FColorHot := False;
  FColorSimpleMargin := clBlue;
  FFlat := False;
  FLeft := taRightJustify;
  FCheckKind := ckCheckBox;
  FMargin := bmNone;
  FShowFocused := True;
  FState    := cbUnChecked ;
  TabStop  := True;
  FGlyph   := TBitmap.Create;
end;

{ -------------------------------------------------------------------- }

destructor TRxCustomCheck.Destroy;
begin
  inherited Destroy;
  FGlyph.Free;
end;

end.


