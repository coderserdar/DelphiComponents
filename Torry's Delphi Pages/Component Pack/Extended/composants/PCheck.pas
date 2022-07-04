unit PCheck;

// TPCheck version 3.10
// Freeware Component for for D2,D3,D4,D5,D6
// Copyright © 2000-2001 by Peric
// Birthday of Component 30.03.2001
// E-mail: pericddn@ptt.yu
// http://www.ptt.yu/korisnici/p/e/pericddn/
// If I' find any errors or rubbish in TPCheck please send me Your suggest or Reclamation.


{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

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
{$IFDEF FPC}
      LCLIntf, lMessages, LCLType, lresources, IntfGraphics,
{$ELSE}
       Windows,MMSystem,
{$ENDIF}
  Messages, SysUtils,
  GraphType, Classes, Controls, Forms,
  Dialogs,ExtCtrls, Graphics;

{$IFDEF FPC}
  {$MODE Delphi}
{$ELSE}
  {$R PCheck.res}
{$ENDIF}


type

    TOnMouseOverEvent = procedure(Sender: TObject) of object;
    TOnMouseOutEvent = procedure(Sender: TObject) of object;
    TLeft=(taRightJustify,taLeftJustify);
    TMargine=(bmNone,bmLowered,bmRaised,bmSimple);
    TImageType=(igCrossRed,igHookBlue,igCirce,igCrossBlue,igCrossGreen,igHookLime,igHookRed);

    TPCustomCheck = class(TCustomControl)

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
    FVersion: string;
    FChecked,
    FShowHand,
    Fsong,
    FColorHot:boolean;
    FImageType:TImageType;
    FMargine:TMargine;
    FLeft:Tleft;
    FGlyph:TBitmap;
    FOnMouseOver: TOnMouseOverEvent;
    FOnMouseOut: TOnMouseOutEvent;
    FColorSipleMargine:TColor;
    FHintTwo: string;
    function  GetHintTwo: string;
    function GetVersion: string;
    procedure DrawParentImage (Control: TControl; Dest: TCanvas);
    procedure SetTransparent(value:boolean);
    procedure SetShowFocused(value:boolean);
    procedure SetFlat(value:boolean);
    procedure SetVersion(const AValue: string);
    procedure SetHintTwo(AHintTwo: string);
    procedure SetShowHand(AShowHand:boolean);
    procedure SetColorHot(value:boolean);
    procedure SetMargine(value:Tmargine);
    procedure DrawCaptionEnabled;
    procedure CreateBrushPattern;
    procedure DrawMargin;
    procedure DrawImage;
    procedure DrawImageHot;
    procedure DrawCaption;
    procedure SetSong(Asong: boolean);
    procedure SetImageType(Value:TImageType);
    procedure DoKliknime(Sender: TObject);
    procedure CMMouseEnter(var AMsg: TMessage);
              message CM_MOUSEENTER;
    procedure CMMouseLeave(var AMsg: TMessage);
              message CM_MOUSELEAVE;
    procedure SetChecked(AChecked:boolean);
    procedure SetLeft(value:TLeft);
    procedure SetColorSipleMargine(value:tColor);

{$IFDEF FPC}
    procedure CmEnabledChanged(var Message: TLmNoParams);
              message CM_ENABLEDCHANGED;
    procedure CmParentColorChanged(var Message: TLmNoParams);
              message CM_PARENTCOLORCHANGED;
    procedure CmTextChanged(var Message: TLmNoParams);
              message CM_TEXTCHANGED;
    procedure CmVisibleChanged(var Message: TLmNoParams);
              message CM_VISIBLECHANGED;
    procedure CmParentFontChanged(var Message: TLmNoParams);
              message CM_FONTCHANGED;
    procedure WMLButtonDblClk (var Message: TLMLButtonDown);
              message WM_LBUTTONDBLCLK;
{$ELSE}
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
{$ENDIF}
    procedure CmDialogChar(var Message: TCMDialogChar);
              message CM_DIALOGCHAR;
    procedure WMSetFocus(var Message: TWMSetFocus);
              message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus);
              message WM_KILLFOCUS;
  protected
  
      procedure Click;override;
      procedure Music(song : pchar);
      procedure Paint; override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
      procedure DoEnter; override;
      procedure DoExit; override;
      procedure KeyDown(var Key: Word; Shift: TShiftState); override;
      property ShowFocused:boolean read FShowFocused write SetShowFocused default True;
     property Flat: boolean read FFlat write SetFlat default False;
     property Version: string read GetVersion write SetVersion;
     property HotTrack:boolean read FColorHot write SetColorHot default True;
     property OnMouseEnter: TOnMouseOverEvent read FOnMouseOver write FOnMouseOver;
     property OnMouseExit: TOnMouseOutEvent read FOnMouseOut write FOnMouseOut;
     property Checked: boolean read FChecked write SetChecked default False;
     property Alignment: Tleft read FLeft write SetLeft default taRightJustify;
     property ImageType: TImageType read FImageType write SetImageType default igCrossRed;
     property ShowHandCursor: boolean read FShowHand write SetShowHand default true;
     property PlayMusic: boolean read FSong write SetSong default True;
     property Style: TMargine read FMargine write SetMargine default bmNone;
     property ColorSipleMargine: TColor read FColorSipleMargine write SetColorSipleMargine default clBlue;
     property HintSecondLine:string read GetHintTwo  write SetHintTwo;

  public

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property Transparent:boolean read FTransparent write SetTransparent default False;

  end;

 TPCheck = class (TPCustomCheck)

  published

     property Transparent;
     property ShowFocused;
     property Flat;
     property Version;
     property HotTrack;
     property OnMouseEnter;
     property OnMouseExit;
     property Checked;
     property Alignment;
     property ImageType;
     property ShowHandCursor;
     property PlayMusic;
     property Style;
     property ColorSipleMargine;
     property HintSecondLine;
     property Enabled;
     property DragCursor;
     property DragMode;
     property Caption;
     property Font;
     property ParentFont;
     property Color;
     property ParentColor;
     property Visible;
     property ShowHint;
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
     property TabStop default True;
     property TabOrder;
     property OnKeyDown;
     property OnKeyPress;
     property OnKeyUp;
     property PopupMenu;
     
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
procedure Register;
begin
  RegisterComponents('PDJ', [TPCheck]);
end;

procedure TPCustomCheck.SetShowFocused(value:boolean);
begin
if value<>FShowFocused then
FShowFocused :=value;
end;

procedure TPCustomCheck.SetTransparent(value:boolean);
begin
if value<>Ftransparent then
Ftransparent:=value;
repaint;
end;


procedure TPCustomCheck.DoKliknime(Sender: TObject);
begin
 if assigned(OnClick) then OnClick(Self);
end;

procedure TPCustomCheck.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
      SetFocus;
    if FChecked then SetChecked(False) else SetChecked(True);
    end
    else
      inherited;
end;

procedure TPCustomCheck.WMSetFocus(var Message: TWMSetFocus);
begin
inherited;
Repaint;
end;

procedure TPCustomCheck.WMKillFocus(var Message: TWMKillFocus);
begin
inherited;
Repaint;
end;

procedure TPCustomCheck.DoEnter;
begin
  inherited DoEnter;
end;

procedure TPCustomCheck.DoExit;
begin
  inherited DoExit;
end;

procedure TPCustomCheck.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then doKliknime(Self);
  inherited KeyDown(Key, Shift);
end;

procedure TPCustomCheck.SetFlat(value:boolean);
begin
if value<>FFlat then
FFlat:=value;
Repaint;
end;

function TPCustomCheck.GetHintTwo: string;
begin
     Result:=FHintTwo;
end;

procedure TPCustomCheck.SetHintTwo(AHintTwo:String);
begin
FHintTwo:=AHintTwo;
if csDesigning in ComponentState then
 Exit;
  if hint<>'' then
  if FHintTwo<>'' then
hint:=hint+#13+FHintTwo else
hint:=hint;
end;

procedure TPCustomCheck.SetColorSipleMargine(value:tColor);
begin
  if value <> FColorSipleMargine then
  begin
    FColorSipleMargine := value;
{$IFDEF FPC}
    Refresh;
{$ELSE}
    RecreateWnd;
{$ENDIF}
  end;
end;

procedure TPCustomCheck.SetColorHot(value:boolean);
begin
  if value <> FColorHot then
  begin
    FColorHot := value;
  end;
end;

procedure TPCustomCheck.SetLeft(Value: TLeft);
begin
  if value <> FLeft then
  begin
    FLeft := value;
{$IFDEF FPC}
    Refresh;
{$ELSE}
    RecreateWnd;
{$ENDIF}
  end;
end;

procedure TPCustomCheck.Click;
begin
inherited;
end;

procedure TPCustomCheck.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Begin
if Button<>mbleft then exit else begin
if not enabled then exit;
SetFocus;
if  not FChecked then
begin
FChecked:=true;
Repaint;
end
else
begin
FChecked:=false;
Repaint;
end;
if Fsong then music('klik');
end;

inherited MouseDown(Button, Shift, X, Y);
end;

procedure TPCustomCheck.WMLButtonDblClk (var Message: {$IFDEF FPC}TLMLButtonDown{$ELSE}TWMLButtonDown{$ENDIF});
begin
  inherited;
  Click;
end;

procedure TPCustomCheck.SetChecked(AChecked:boolean);
begin
Fchecked:=AChecked;
Invalidate;
end;

procedure TPCustomCheck.SetMargine(Value:TMargine);
begin
  if value <> FMargine then
  begin
    FMargine := value;
    Invalidate;
  end;
end;

procedure TPCustomCheck.DrawParentImage(Control: TControl; Dest: TCanvas);
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
    {$IFDEF DELPHI}
    GetViewportOrgEx(DC, Position);
    {$ENDIF}
    {$ENDIF}
    {$IFDEF FPC}
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    ///Fixed Thorsten Claus {25.01.2002}
    Parent.Perform(LM_ERASEBKGND, Integer(DC), Integer(0));
    Parent.Perform(LM_PAINT, Integer(DC), Integer(0));

    {$ELSE}

    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    {Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0); }
    ///Fixed Thorsten Claus {25.01.2002}
    Parent.Perform(WM_ERASEBKGND, Integer(DC), Integer(0));
    Parent.Perform(WM_PAINT, Integer(DC), Integer(0));
    {$ENDIF}

    RestoreDC(DC, SaveIndex);

  end;
end;


procedure TPCustomCheck.paint;
var
z:integer;
FocusRect:TRect;
n: Integer;
begin
  DrawMargin;
  if FTransparent then
    DrawParentImage(Self, Self.Canvas);
  with Canvas do begin
    z:=(R.top+((R.bottom-R.top)-TextHeight(Caption)) div 2);
    if pos('&', Caption)>0 then n:=TextWidth('&') else n:=0;
        case FLeft of
          taRightJustify:FocusRect := Rect(R.Left+22, z , R.Left+22+TextWidth(Caption)-n+3, z+TextHeight(Caption));
          taLeftJustify:FocusRect := Rect(R.Left+2, z , R.Left+2+TextWidth(Caption)-n+3, z+TextHeight(Caption));
        end;
  end;
  if ShowFocused
   then
    begin
      if Focused then
        Begin
          DrawFocusrect(Canvas.Handle,Rect(FocusRect.left-1,FocusRect.top-1,FocusRect.Right+1,FocusRect.bottom+1));
        end;

    end;
 DrawCaption;
 DrawImage;

 if FChecked then DrawImageHot;

end;

procedure TPCustomCheck.DrawMargin;
begin
  R:=clientRect;

  canvas.brush.color:=color;
  Canvas.Brush.Style := bsSolid;
  case Fmargine of
    bmNone:
      begin
    {$IFDEF FPC}
        canvas.Brush.Color := clBtnFace;
        canvas.Pen.Color := clBtnFace;
        canvas.Frame3d( R, 0, bvNone );
    {$ELSE}
        Frame3d(canvas, R, clBtnFace,clBtnFace,0);
    {$ENDIF}
      end;
    bmLowered:
       begin
    {$IFDEF FPC}
         canvas.Brush.Color := clBtnShadow;
         canvas.Pen.Color := clBtnHighlight;
         canvas.Frame3d( R, 1, bvLowered );
    {$ELSE}
         Frame3d(canvas, R,clBtnShadow,clBtnHighlight,1);
    {$ENDIF}
      end;
    bmRaised:
      begin
    {$IFDEF FPC}
        canvas.Brush.Color := clBtnHighlight;
        canvas.Pen.Color := clBtnShadow;
        canvas.Frame3d( R, 1, bvRaised );
    {$ELSE}
        Frame3d(canvas, R,clBtnHighlight,clBtnShadow,1);
    {$ENDIF}
      end;
    bmSimple:
      begin
    {$IFDEF FPC}
        canvas.Brush.Color := FColorSipleMargine;
        canvas.Pen.Color := FColorSipleMargine;
        canvas.Frame3d( R, 1, bvSpace );
    {$ELSE}
        Frame3d(canvas, R,FColorSipleMargine,FColorSipleMargine,1);
    {$ENDIF}
      end;
    end;
    if index=1 then
    begin
      if Pattern = nil then CreateBrushPattern;
      canvas.Brush.Bitmap := Pattern ;
      canvas.FillRect(R);
    end
    else
      Begin
        if  assigned ( Pattern ) Then
         Begin
      {$IFNDEF FPC}
          Pattern.Dormant;
      {$ENDIF}
          Pattern.FreeImage;
          Pattern.Free;
          Pattern := nil;
          canvas.Brush.Bitmap := nil;
         end;
       canvas.Brush.Canvas.Clear;
       canvas.FillRect(R);

      end;
end;

procedure TPCustomCheck.CreateBrushPattern;
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

procedure TPCustomCheck.CMMouseEnter(var AMsg: TMessage);
begin
  if not enabled then exit;
  if FColorHot then
   begin
    index:=1;
    repaint;
   end;
  IF FShowHand then
   begin
    Cursor := crHandPoint;
   end;
  if Assigned(FOnMouseOver) then FOnMouseOver(Self);
end;

procedure TPCustomCheck.CMMouseLeave(var AMsg: TMessage);
begin
if not enabled then exit;
if FColorHot then
begin
index:=0;
repaint;
end;
IF FShowHand then begin
 Cursor := crDefault;
end;
if Assigned(FOnMouseOut) then FOnMouseOut(Self);
end;

procedure TPCustomCheck.DrawCaption;
var
  x:integer;
begin
  with Canvas do
  begin
    Font := Self.Font;
    Kolorit:=Font.Color;

    if FColorHot then
    if index=1 then
    Font.color:=clBlue else Font.color:= Kolorit;

    Brush.Style := bsClear;
    x:=(R.top+((R.bottom-R.top)-TextHeight(Caption)) div 2);
    begin
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
  end;
  

procedure TPCustomCheck.DrawCaptionEnabled;   //fixed 05.05.2001.
var ECaptRect:TRect;
begin
with canvas do
  begin
     Font := Self.Font;
     brush.style:=bsClear;
     {$IFDEF PDJ_D2}
     DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
      {$ENDIF}
     {$IFDEF PDJ_D3}
     DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT );
      {$ELSE}
      DrawText(Handle, PChar(Caption), Length(Caption), CaptRect, DT_LEFT or BiDiFlags );
     {$ENDIF}
     font.Color :=clBtnHighlight;

     ECaptRect:=Rect(CaptRect.Left+1,CaptRect.top+1,CaptRect.Right+1,CaptRect.Bottom+1);
            {$IFDEF PDJ_D2}
     DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT );
      {$ENDIF}
       {$IFDEF PDJ_D3}
     DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT );
        {$ELSE}
        DrawText(Handle, PChar(Caption), Length(Caption), ECaptRect, DT_LEFT or BiDiFlags );
        {$ENDIF}
     font.color :=clBtnShadow;
  end;
end;

procedure TPCustomCheck.DrawImage;
var
  BitRect,Image: TRect;
  {$IFDEF FPC}
  FImage:TBitmap;
  {$ENDIF}
  x:integer;
begin


  FImage:=TBitmap.Create;
  if not FFlat then
   begin

    if enabled then

     {$IFDEF FPC}
    FImage.LoadFromLazarusResource( 'BLANK')
    else
    FImage.LoadFromLazarusResource( 'ENABLED');
    end
    else
    begin
     if enabled then
      FImage.LoadFromLazarusResource( 'BLANKFLAT')
     else
      FImage.LoadFromLazarusResource( 'ENABLEDFLAT');

     {$ELSE}
      FGlyph.Handle := LoadBitmap(hInstance, 'Blank')
     else
      FGlyph.Handle := LoadBitmap(hInstance, 'Enabled');
    end
    else
     begin
      if enabled then
       FGlyph.Handle := LoadBitmap(hInstance, 'BlankFlat')
      else
       FGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat');
    {$ENDIF}
     end;



  {$IFDEF FPC}
  FGlyph.Width:=FImage.Width;
  FGlyph.Height:=FImage.Height;
  FGlyph.Canvas.Brush.Color:=Color;
  FGlyph.Canvas.FillRect(0,0,FImage.Width,FImage.Height);
  TransColor := FImage.Canvas.Pixels[0, FImage.Height - 1];
  Image := Rect(0, 0, FImage.Width, FImage.Height);
  x:=(R.top+((R.bottom-R.top)-FImage.Height) div 2);
  case FLeft of
    taRightJustify:BitRect := Rect(R.Left+2, x , R.Left+2+FImage.width, x+FImage.height);
    taLeftJustify:BitRect := Rect(R.right-18, x , R.right-18+FImage.width, x+FImage.height);
    end;
  {$ELSE}
  TransColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  Image := Rect(0, 0, FGlyph.Width, FGlyph.Height);
  x:=(R.top+((R.bottom-R.top)-FGlyph.Height) div 2);
  case FLeft of
    taRightJustify:BitRect := Rect(R.Left+2, x , R.Left+2+FGlyph.width, x+FGlyph.height);
    taLeftJustify:BitRect := Rect(R.right-18, x , R.right-18+FGlyph.width, x+FGlyph.height);
    end;
  {$ENDIF}
  {$IFDEF PDJ_D2}
  canvas.BrushCopy(Bitrect, FGlyph,Image, clFuchsia);
  {$ELSE}
    {$IFDEF FPC}
      FImage.TransparentColor:= TransColor;
      FImage.TransparentMode:= tmAuto;
      FGlyph.Canvas.Draw(0,0, FImage );
      Canvas.Draw(Bitrect.Left,Bitrect.Top,FGlyph);
    {$ELSE}
      canvas.BrushCopy(Bitrect, FGlyph,Image, TransColor);
    {$ENDIF}
  {$ENDIF}

end;

procedure TPCustomCheck.SetImageType(Value:TImageType);
begin
  if value <> FImageType then
  begin
    FImageType := value;
    {$IFDEF FPC}
    Refresh;
    {$ELSE}
    RecreateWnd;
    {$ENDIF}
  end;
end;

procedure TPCustomCheck.DrawImageHot;
var
  BitHotRect,ImageHot: TRect;
    {$IFDEF FPC}
    Image : TBitmap;
    {$ENDIF}
  x:integer;
begin

  Image := TBitmap.Create;
  case FImageType of
    igCrossRed:

      begin
      if enabled then

      {$IFDEF FPC}
        Image.LoadFromLazarusResource( 'CHECKIKS')
       else
       if not FFlat then
        Image.LoadFromLazarusResource( 'ENABLED')
       else
        Image.LoadFromLazarusResource( 'ENABLEDFLAT')
      end;

     igHookBlue:
      begin
       if enabled then
        Image.LoadFromLazarusResource( 'CHECKOK')
       else
       if not FFlat then
        Image.LoadFromLazarusResource( 'ENABLED')
        else
        Image.LoadFromLazarusResource( 'ENABLEDFLAT')
      end;

     igHookRed:
      begin
       if enabled then
        Image.LoadFromLazarusResource( 'CHECKOKR')
       else
        if not FFlat then
         Image.LoadFromLazarusResource( 'ENABLED')
        else
         Image.LoadFromLazarusResource( 'ENABLEDFLAT')
      end;

     igCirce:
      begin
       if enabled then
        Image.LoadFromLazarusResource( 'CHECKELLI')
       else
        if not FFlat then
         Image.LoadFromLazarusResource( 'ENABLED')
        else
        Image.LoadFromLazarusResource( 'ENABLEDFLAT')
      end;

    igCrossBlue:
      begin
       if enabled then
       Image.LoadFromLazarusResource( 'CHECKEIKSB')
        else
         if not FFlat then
          Image.LoadFromLazarusResource( 'ENABLED')
         else
        Image.LoadFromLazarusResource( 'ENABLEDFLAT')
      end;

     igCrossGreen:
      begin
       if enabled then
        Image.LoadFromLazarusResource( 'CHECKIKSG')
       else
        if not FFlat then
         Image.LoadFromLazarusResource( 'ENABLED')
        else
         Image.LoadFromLazarusResource( 'ENABLEDFLAT')
      end;

     igHookLime:
      begin
       if enabled then
        Image.LoadFromLazarusResource( 'CHECKOKG')
       else
        if not FFlat then
         Image.LoadFromLazarusResource( 'ENABLED')
        else
        Image.LoadFromLazarusResource( 'ENABLEDFLAT')
      {$ELSE}
      FGlyph.Handle := LoadBitmap(hInstance, 'CheckIks')
      else
      if not FFlat then
      FGlyph.Handle := LoadBitmap(hInstance, 'Enabled')
      else
      FGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat')
      end;

    igHookBlue:
      begin
      if enabled then
      FGlyph.Handle := LoadBitmap(hInstance, 'CheckOk')
      else
      if not FFlat then
      FGlyph.Handle := LoadBitmap(hInstance, 'Enabled')
      else
      FGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat')
      end;

    igHookRed:
      begin
      if enabled then
      FGlyph.Handle := LoadBitmap(hInstance, 'CheckOkR')
      else
      if not FFlat then
      FGlyph.Handle := LoadBitmap(hInstance, 'Enabled')
      else
      FGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat')
      end;

    igCirce:
      begin
      if enabled then
      FGlyph.Handle := LoadBitmap(hInstance, 'CHECKELLI')
      else
      if not FFlat then
      FGlyph.Handle := LoadBitmap(hInstance, 'Enabled')
      else
      FGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat')
      end;

    igCrossBlue:
      begin
      if enabled then
      FGlyph.Handle := LoadBitmap(hInstance, 'CHECKEIKSB')
      else
      if not FFlat then
      FGlyph.Handle := LoadBitmap(hInstance, 'Enabled')
      else
      FGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat')
      end;

    igCrossGreen:
      begin
      if enabled then
      FGlyph.Handle := LoadBitmap(hInstance, 'CHECKIKSG')
      else
      if not FFlat then
      FGlyph.Handle := LoadBitmap(hInstance, 'Enabled')
      else
      FGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat')
      end;

    igHookLime:
      begin
      if enabled then
      FGlyph.Handle := LoadBitmap(hInstance, 'CHECKOKG')
      else
      if not FFlat then
      FGlyph.Handle := LoadBitmap(hInstance, 'Enabled')
      else
      FGlyph.Handle := LoadBitmap(hInstance, 'EnabledFlat')
      {$ENDIF}
      end;
    end;
  TransColor := FGlyph.Canvas.Pixels[0, FGlyph.Height - 1];
  ImageHot := Rect(0, 0, FGlyph.Width, FGlyph.Height);
  x:=(R.top+((R.bottom-R.top)-FGlyph.Height) div 2);

  case FLeft of
    taRightJustify:BitHotRect:=Rect(R.Left+2, x , R.Left+2+FGlyph.width, x+FGlyph.height);
    taLeftJustify:BitHotRect:=Rect(R.right-18, x , R.right-18+FGlyph.width, x+FGlyph.height);
  end;

  {$IFDEF PDJ_D2}
  canvas.BrushCopy(BitHotrect, FGlyph,ImageHot, clFuchsia);
  {$ELSE}
    {$IFDEF FPC}
      Image.Transparent:=True;
      FGlyph.Canvas.Draw(0,0, Image );
      canvas.Draw(0,0,FGlyph);
      Image.FreeImage ;
      Image.Handle := 0 ;
      Image.Free ;
      {$ELSE}
      canvas.BrushCopy(BitHotrect, FGlyph,ImageHot, TransColor);
    {$ENDIF}
  {$ENDIF}

end;

procedure TPCustomCheck.SetShowHand(AShowHand:boolean);
begin
fShowHand:=AShowHand;
end;



procedure TPCustomCheck.CmParentFontChanged(var Message: {$IFDEF FPC}TLmNoParams{$ELSE}TWmNoParams{$ENDIF});
begin
  inherited;
  Invalidate;
end;

procedure TPCustomCheck.CmTextChanged(var Message: {$IFDEF FPC}TLmNoParams{$ELSE}TWmNoParams{$ENDIF});
begin
  inherited;
  Invalidate;
end;

procedure TPCustomCheck.CmVisibleChanged(var Message: {$IFDEF FPC}TLmNoParams{$ELSE}TWmNoParams{$ENDIF});
begin
  inherited;
  Invalidate;
end;

procedure TPCustomCheck.CmParentColorChanged(var Message: {$IFDEF FPC}TLmNoParams{$ELSE}TWmNoParams{$ENDIF});
begin
  inherited;
  Invalidate;
end;

procedure TPCustomCheck.CmEnabledChanged(var Message: {$IFDEF FPC}TLmNoParams{$ELSE}TWmNoParams{$ENDIF});
begin
  inherited;
  Invalidate;
end;

procedure TPCustomCheck.Music(song : pchar);
var
  h: THandle;
  p: pointer;
begin
  {$IFDEF DELPHI}
  h := FindResource(hInstance,song,'WAV');
  h := LoadResource(hInstance, h);
  p := LockResource(h);
  sndPlaySound(p,SND_MEMORY or SND_SYNC);
  UnLockResource(h);
  FreeResource(h);
  {$ENDIF}
 end;

procedure TPCustomCheck.SetSong(Asong:boolean);
begin
 Fsong:=Asong;
end;


procedure TPCustomCheck.SetVersion(const AValue: string);
begin
  FVersion:=FVersion;
end;

function TPCustomCheck.GetVersion: string;
begin
  Result:=FVersion;
end;


constructor TPCustomCheck.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];
  SetBounds(0, 0, 97, 19);
  Fchecked:=false;
  FColorSipleMargine:=clBlue;
  FLeft:=taRightJustify;
  FImageType:=igCrossRed;
  FMargine:=bmNone;
  FShowHand:=true;
  FSong:=true;
  FColorHot:=True;
  FFlat:=False;
  FVersion:='Version 3.20, Copyright © 2000-2001 by Peric, E-mail: pericddn@ptt.yu';
  FShowFocused:=True;
  TabStop:=True;
  FGlyph := TBitmap.Create;
end;

destructor TPCustomCheck.Destroy;
begin
  inherited Destroy;
  FGlyph.Free;
end;

{$IFDEF FPC}
initialization
  {$i PCheck.lrs}
  {$i PCheck2.res}
{$ENDIF}

end.


