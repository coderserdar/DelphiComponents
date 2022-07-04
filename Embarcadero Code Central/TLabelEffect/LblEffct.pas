unit LblEffct;

{
  This unit implements a label component with 3D effects.

  The label has a highlight and a shadow.  The colours of these
  can be controlled through properties as can their distance
  from the label and their direction.  There are also preset
  combinations of direction/distance and colours.

  The label can be set to operate as a button, ie. initially
  raised it will depress when clicked.

  The label can be rotated to any angle.

  Written by Keith Wood (kbwood@compuserve.com)
  Version 1.0 - 27 May 1995
  Version 2.0 - 1 Feb 1996
      Added extrusion, keep letters vertical, graduated face and version.
  Version 2.1 - 14 Jun 1996
      Update to work with Delphi 2.0, fix bug with Transparent property,
      fix bug with Alignment property, prepare label offscreen and then display.
  Version 3.0 - 3 Jan 1997
      Added separate graduation colours for highlight and shadow.
      Added extra face graduation options.
      Added resize of highlight/shadow.
      Added bitmap background for text.
  Version 4.0 - 30 Sep 1997
      Added image list animation capabilities (not Delphi 1)

  Thanks to Paradox Informant and their starter article on 3D labels.
  Thanks to Bill Murto and his RotateLabel.
  Thanks to Curtis Keisler and his TxtRotat example.
}

interface

uses
  SysUtils, WinTypes, WinProcs, Classes, Graphics, Controls, StdCtrls,
  ExtCtrls;

type
  { Range of offsets for the shadows }
  TEffectDepth = 0..10;

  { Directions in which the offsets can be positioned }
  TEffectDirection = (edNone, edUp, edUpRight, edRight, edDownRight, edDown,
      edDownLeft, edLeft, edUpLeft);

  { Constants for specifying direction component }
  TDirXY = (drX, drY);

  { The preset styles of label effects available }
  TEffectStyle = (esNone, esCustom, esRaised, esSunken, esShadow, esFlying);

  { The preset colour schemes available }
  TColourScheme = (csCustom, csText, csWindows, csEmbossed, csGold, csSteel);

  { Constants for specifying positions of colours }
  TColourPosition = (cpHighlight, cpShadow, cpFace);

  { Range for rotation }
  TAngleRange = 0..360;

  { Options for varying the shadow/highlight for the label }
  TEffectOption = (eoNormal, eoReal, eoExtrude, eoGraduated);

  { Options for varying the face of the label }
  TGraduateOption = (goNone, goVertical, goHorizontal, goFDiagonal,
    goBDiagonal, goBoxed, goRIndented, goLIndented);

  { Options for varying the text size of the label }
  TResizeOption = (rsNone, rsExpand, rsReduce);

  { Animation styles }
  TLEAnimationStyle = (asForwards, asBackwards, asBounce);

  { Animation properties }
  TLabelEffectAnimation = class(TPersistent)
  private
    FContinuous: Boolean;
    FEnabled: Boolean;
    FExternalTiming: Boolean;
    FFrameIndex: Integer;
    FFrames: TImageList;
    FInterval: Cardinal;
    FStyle: TLEAnimationStyle;
    FOnChange: TNotifyEvent;
    iIncrement: Integer;
    bFirstTime: Boolean;
    procedure SetContinuous(bContinuous: Boolean);
    procedure SetEnabled(bEnabled: Boolean);
    procedure SetExternalTiming(bExternal: Boolean);
    procedure SetFrameIndex(iIndex: Integer);
    procedure SetFrames(imlFrames: TImageList);
    procedure SetInterval(iInterval: Cardinal);
    procedure SetStyle(iStyle: TLEAnimationStyle);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    procedure NextFrame; virtual;
  published
    property Continuous: Boolean read FContinuous write SetContinuous
      default True;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property ExternalTiming: Boolean read FExternalTiming
      write SetExternalTiming default False;
    property Frames: TImageList read FFrames write SetFrames;
    property FrameIndex: Integer read FFrameIndex write SetFrameIndex default 0;
    property Interval: Cardinal read FInterval write SetInterval default 500;
    property Style: TLEAnimationStyle read FStyle write SetStyle
      default asForwards;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  { The label itsef }
  TLabelEffect = class(TCustomLabel)
  private
    FDepthHighlight,
    FDepthShadow: TEffectDepth;
    FDirectionHighlight,
    FDirectionShadow: TEffectDirection;
    FColourHighlight,
    FColourShadow,
    FGraduateHighlight,
    FGraduateShadow,
    FColourFace: TColor;
    FGraduateFace: TGraduateOption;
    FGraduateFrom: TColor;
    FStyleHighlight,
    FStyleShadow: TEffectOption;
    FResizeHighlight,
    FResizeShadow: TResizeOption;
    FEffectStyle: TEffectStyle;
    FColourScheme: TColourScheme;
    FBitmap: TBitmap;
    FAsButton: Boolean;
    FAngle: TAngleRange;
    FKeepLettersVertical: Boolean;
    FVersion: string;
    FAnimation: TLabelEffectAnimation;
    tmrAnim: TTimer;
    bChangingStyle,                { Is preset style being invoked ? }
    bChangingScheme: Boolean;      { Is preset colour scheme being invoked ? }
    clrSchemes: array [TColourScheme,TColourPosition] of TColor;
    dDegToRad, dCosAngle, dSinAngle, dCosSquared, dSinSquared: Double;
    procedure SetDepthHighlight(iDepth: TEffectDepth);
    procedure SetDepthShadow(iDepth: TEffectDepth);
    procedure SetDirectionHighlight(edDirection: TEffectDirection);
    procedure SetDirectionShadow(edDirection: TEffectDirection);
    procedure SetColourHighlight(clrHighlight: TColor);
    procedure SetColourShadow(clrShadow: TColor);
    procedure SetGraduateHighlight(clrHighlight: TColor);
    procedure SetGraduateShadow(clrShadow: TColor);
    procedure SetColourFace(clrFace: TColor);
    procedure SetGraduateFace(goGrad: TGraduateOption);
    procedure SetGraduateFrom(clrGrad: TColor);
    procedure SetStyleHighlight(eoStyle: TEffectOption);
    procedure SetStyleShadow(eoStyle: TEffectOption);
    procedure SetResizeHighlight(rsSize: TResizeOption);
    procedure SetResizeShadow(rsSize: TResizeOption);
    procedure SetEffectStyle(esStyle: TEffectStyle);
    procedure SetColourScheme(csScheme: TColourScheme);
    procedure SetBitmap(bmp: TBitmap);
    procedure SetAsButton(bBtn: Boolean);
    procedure SetAngle(aAngle: TAngleRange);
    procedure SetTextAngle(cnv: TCanvas; aAngle: TAngleRange; iHeight: Integer);
    procedure SetKeepLettersVertical(bKeep: Boolean);
    procedure SetAnimation(leaAnim: TLabelEffectAnimation);
    procedure AnimationChanged(Sender: TObject);
    procedure AnimationTimer(Sender: TObject);
    function GetColourFace: TColor;
    procedure ChangeFont(Sender: TObject);
    procedure ChangeBitmap(Sender: TObject);
    function IsCustomEffect: Boolean;
    function IsCustomScheme: Boolean;
  protected
    procedure Paint; override;
    procedure MouseDown(mbBtn: TMouseButton; ssShift: TShiftState;
      x, y: Integer); override;
    procedure MouseMove(ssShift: TShiftState; x, y: Integer); override;
    procedure MouseUp(mbBtn: TMouseButton; ssShift: TShiftState;
      x, y: Integer); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Version: string read FVersion;         { Read-only }
    procedure NextAnimationFrame;
    procedure ShowAnimationFrame(iIndex: Integer);
  published
    property DepthHighlight: TEffectDepth read FDepthHighlight
      write SetDepthHighlight stored IsCustomEffect default 1;
    property DepthShadow: TEffectDepth read FDepthShadow
      write SetDepthShadow stored IsCustomEffect default 1;
    property DirectionHighlight: TEffectDirection read FDirectionHighlight
      write SetDirectionHighlight stored IsCustomEffect default edUpLeft;
    property DirectionShadow: TEffectDirection read FDirectionShadow
      write SetDirectionShadow stored IsCustomEffect default edDownRight;
    property ColourHighlight: TColor read FColourHighlight
      write SetColourHighlight stored IsCustomScheme default clWhite;
    property ColourShadow: TColor read FColourShadow
      write SetColourShadow stored IsCustomScheme default clBlack;
    property GraduateHighlight: TColor read FGraduateHighlight
      write SetGraduateHighlight default clGray;
    property GraduateShadow: TColor read FGraduateShadow
      write SetGraduateShadow default clGray;
    property ColourFace: TColor read GetColourFace write SetColourFace;
    property GraduateFace: TGraduateOption read FGraduateFace
      write SetGraduateFace default goNone;
    property GraduateFrom: TColor read FGraduateFrom
      write SetGraduateFrom default clGray;
    property StyleHighlight: TEffectOption read FStyleHighlight
      write SetStyleHighlight default eoNormal;
    property StyleShadow: TEffectOption read FStyleShadow
      write SetStyleShadow default eoNormal;
    property ResizeHighlight: TResizeOption read FResizeHighlight
      write SetResizeHighlight default rsNone;
    property ResizeShadow: TResizeOption read FResizeShadow
      write SetResizeShadow default rsNone;
    property EffectStyle: TEffectStyle read FEffectStyle
      write SetEffectStyle default esRaised;
    property ColourScheme: TColourScheme read FColourScheme
      write SetColourScheme default csWindows;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
    property AsButton: Boolean read FAsButton write SetAsButton default False;
    property Angle: TAngleRange read FAngle write SetAngle default 0;
    property KeepLettersVertical: Boolean read FKeepLettersVertical
      write SetKeepLettersVertical default False;
    property Animation: TLabelEffectAnimation read FAnimation
      write SetAnimation;
    { Publish inherited properties }
    property Align;
    property Alignment;
    property Caption;
    property Color;
    property Cursor;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowAccelChar;
    property ShowHint;
    property Transparent default True;
    property Visible;
    property WordWrap;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

const
  sLabelEffectVersion = '4.1';

procedure Register;
begin
  RegisterComponents('Standard', [TLabelEffect]);
  RegisterClasses([TLabelEffectAnimation]);
end;

{ TLabelEffect ----------------------------------------------------------------}

{ Initialisation }
constructor TLabelEffect.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  { Colour schemes cannot be constant since Custom version varies }
  clrSchemes[csText, cpHighlight]     := clWhite;
  clrSchemes[csText, cpFace]          := clBlack;
  clrSchemes[csText, cpShadow]        := clGray;

  clrSchemes[csWindows, cpHighlight]  := clWhite;
  clrSchemes[csWindows, cpFace]       := clGray;
  clrSchemes[csWindows, cpShadow]     := clBlack;

  clrSchemes[csEmbossed, cpHighlight] := clWhite;
  clrSchemes[csEmbossed, cpFace]      := clSilver;
  clrSchemes[csEmbossed, cpShadow]    := clGray;

  clrSchemes[csGold, cpHighlight]     := clYellow;
  clrSchemes[csGold, cpFace]          := clOlive;
  clrSchemes[csGold, cpShadow]        := clBlack;

  clrSchemes[csSteel, cpHighlight]    := clAqua;
  clrSchemes[csSteel, cpFace]         := clTeal;
  clrSchemes[csSteel, cpShadow]       := clNavy;

  clrSchemes[csCustom, cpHighlight]   := clrSchemes[csWindows, cpHighlight];
  clrSchemes[csCustom, cpFace]        := clrSchemes[csWindows, cpFace];
  clrSchemes[csCustom, cpShadow]      := clrSchemes[csWindows, cpShadow];

  { Initialise default values for internal fields }
  FDepthHighlight      := 1;
  FDepthShadow         := 1;
  FDirectionHighlight  := edUpLeft;
  FDirectionShadow     := edDownRight;
  FStyleHighlight      := eoNormal;
  FStyleShadow         := eoNormal;
  FResizeHighlight     := rsNone;
  FResizeShadow        := rsNone;
  FEffectStyle         := esRaised;
  FColourScheme        := csWindows;
  FColourHighlight     := clrSchemes[FColourScheme, cpHighlight];
  FColourShadow        := clrSchemes[FColourScheme, cpShadow];
  FGraduateFace        := goNone;
  FGraduateFrom        := clrSchemes[FColourScheme, cpFace];
  FGraduateHighlight   := clrSchemes[FColourScheme, cpFace];
  FGraduateShadow      := clrSchemes[FColourScheme, cpFace];
  FBitmap              := TBitmap.Create;
  FBitmap.OnChange     := ChangeBitmap;
  FAsButton            := False;
  FAngle               := 0;
  FKeepLettersVertical := False;
  FVersion             := sLabelEffectVersion;
  FAnimation           := TLabelEffectAnimation.Create;
  FAnimation.OnChange  := AnimationChanged;

  bChangingStyle  := False;
  bChangingScheme := False;
  dDegToRad       := PI / 180;
  dCosAngle       := 1;         { Cos(FAngle * dDegToRad) }
  dCosSquared     := 1;
  dSinAngle       := 0;         { Sin(FAngle * dDegToRad) }
  dSinSquared     := 0;

  AutoSize      := False;
  Height        := 33;
  Width         := 142;
  Transparent   := True;
  Font.Color    := clrSchemes[FColourScheme, cpFace];
  Font.Name     := 'Times New Roman';
  Font.Size     := 20;
  Font.OnChange := ChangeFont;
end;

{ Free resources }
destructor TLabelEffect.Destroy;
begin
  FBitmap.Free;
  FAnimation.Free;
  tmrAnim.Free;
  inherited Destroy;
end;

{ Watch for animation images disappearing }
procedure TLabelEffect.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FAnimation.Frames = AComponent) then
    FAnimation.Frames := nil;
end;

{ Copy from another label }
procedure TLabelEffect.Assign(Source: TPersistent);
begin
  if Source is TLabelEffect then  { Can copy }
    with TLabelEffect(Source) do
    begin
      Self.Alignment      := Alignment;
      Self.DragCursor     := DragCursor;
      Self.DragMode       := DragMode;
      Self.Enabled        := Enabled;
      Self.FocusControl   := FocusControl;
      Self.ShowAccelChar  := ShowAccelChar;
      Self.Hint           := Hint;
      Self.ParentShowHint := ParentShowHint;
      Self.ShowHint       := ShowHint;
      Self.Tag            := Tag;
      Self.Transparent    := Transparent;
      Self.Visible        := Visible;
      Self.WordWrap       := WordWrap;

      Self.Bitmap.Assign(Bitmap);
      Self.Caption             := Caption;
      Self.ParentColor         := ParentColor;
      Self.Color               := Color;
      Self.ColourHighlight     := ColourHighlight;
      Self.ColourShadow        := ColourShadow;
      Self.ParentFont          := ParentFont;
      Self.Font.Assign(Font);
      Self.ColourScheme        := ColourScheme;
      Self.Cursor              := Cursor;
      Self.DepthHighlight      := DepthHighlight;
      Self.DepthShadow         := DepthShadow;
      Self.DirectionHighlight  := DirectionHighlight;
      Self.DirectionShadow     := DirectionShadow;
      Self.EffectStyle         := EffectStyle;
      Self.GraduateFace        := GraduateFace;
      Self.GraduateFrom        := GraduateFrom;
      Self.GraduateHighlight   := GraduateHighlight;
      Self.GraduateShadow      := GraduateShadow;
      Self.ResizeHighlight     := ResizeHighlight;
      Self.ResizeShadow        := ResizeShadow;
      Self.StyleHighlight      := StyleHighlight;
      Self.StyleShadow         := StyleShadow;
      Self.Angle               := Angle;
      Self.KeepLettersVertical := KeepLettersVertical;
      Self.AsButton            := AsButton;
      Self.Animation.Assign(Animation);
      Self.Animation.OnChange  := Self.AnimationChanged;
    end
  else  { Try default assign }
    inherited Assign(Source);
end;

{ Only store these properties if a custom effect style }
function TLabelEffect.IsCustomEffect: Boolean;
begin
  Result := (EffectStyle = esCustom);
end;

{ Only store these properties if a custom colour scheme }
function TLabelEffect.IsCustomScheme: Boolean;
begin
  Result := (ColourScheme = csCustom);
end;

{ Change face colour too }
procedure TLabelEffect.ChangeFont(Sender: TObject);
begin
  SetColourFace(Font.Color);
  Invalidate;
end;

{ Set highlight depth and repaint }
procedure TLabelEffect.SetDepthHighlight(iDepth: TEffectDepth);
begin
  if FDepthHighlight <> iDepth then
  begin
    FDepthHighlight := iDepth;
    if not bChangingStyle then  { Default to custom style when changed }
      SetEffectStyle(esCustom);
    Invalidate;
  end;
end;

{ Set shadow depth and repaint }
procedure TLabelEffect.SetDepthShadow(iDepth: TEffectDepth);
begin
  if FDepthShadow <> iDepth then
  begin
    FDepthShadow := iDepth;
    if not bChangingStyle then  { Default to custom style when changed }
      SetEffectStyle(esCustom);
    Invalidate;
  end;
end;

{ Set highlight direction and repaint }
procedure TLabelEffect.SetDirectionHighlight(edDirection: TEffectDirection);
begin
  if FDirectionHighlight <> edDirection then
  begin
    FDirectionHighlight := edDirection;
    if not bChangingStyle then  { Default to custom style when changed }
      SetEffectStyle(esCustom);
    Invalidate;
  end;
end;

{ Set shadow direction and repaint }
procedure TLabelEffect.SetDirectionShadow(edDirection: TEffectDirection);
begin
  if FDirectionShadow <> edDirection then
  begin
    FDirectionShadow := edDirection;
    if not bChangingStyle then  { Default to custom style when changed }
      SetEffectStyle(esCustom);
    Invalidate;
  end;
end;

{ Set highlight colour and repaint }
procedure TLabelEffect.SetColourHighlight(clrHighlight: TColor);
begin
  if FColourHighlight <> clrHighlight then
  begin
    FColourHighlight                  := clrHighlight;
    clrSchemes[csCustom, cpHighlight] := clrHighlight;
    if not bChangingScheme then  { Default to custom colour scheme when changed }
      SetColourScheme(csCustom);
    Invalidate;
  end;
end;

{ Set shadow colour and repaint }
procedure TLabelEffect.SetColourShadow(clrShadow: TColor);
begin
  if FColourShadow <> clrShadow then
  begin
    FColourShadow                  := clrShadow;
    clrSchemes[csCustom, cpShadow] := clrShadow;
    if not bChangingScheme then  { Default to custom colour scheme when changed }
      SetColourScheme(csCustom);
    Invalidate;
  end;
end;

{ Set highlight graduate colour and repaint }
procedure TLabelEffect.SetGraduateHighlight(clrHighlight: TColor);
begin
  if FGraduateHighlight <> clrHighlight then
  begin
    FGraduateHighlight := clrHighlight;
    if StyleHighlight = eoGraduated then  { Only has effect if highlight is graduated }
      Invalidate;
  end;
end;

{ Set shadow graduate colour and repaint }
procedure TLabelEffect.SetGraduateShadow(clrShadow: TColor);
begin
  if FGraduateShadow <> clrShadow then
  begin
    FGraduateShadow := clrShadow;
    if StyleShadow = eoGraduated then  { Only has effect if shadow is graduated }
      Invalidate;
  end;
end;

{ Set text face colour and repaint }
procedure TLabelEffect.SetColourFace(clrFace: TColor);
begin
  if Font.Color <> clrFace then
  begin
    { Graduate colours follow until different }
    if GraduateHighlight = Font.Color then
      GraduateHighlight := clrFace;
    if GraduateShadow = Font.Color then
      GraduateShadow := clrFace;
    Font.Color                   := clrFace;
    clrSchemes[csCustom, cpFace] := clrFace;
    if not bChangingScheme then  { Default to custom colour scheme when changed }
      SetColourScheme(csCustom);
    Invalidate;
  end;
end;

{ Set text graduate option and repaint }
procedure TLabelEffect.SetGraduateFace(goGrad: TGraduateOption);
begin
  if FGraduateFace <> goGrad then
  begin
    FGraduateFace := goGrad;
    Invalidate;
  end;
end;

{ Set text graduate colour and repaint }
procedure TLabelEffect.SetGraduateFrom(clrGrad: TColor);
begin
  if FGraduateFrom <> clrGrad then
  begin
    FGraduateFrom := clrGrad;
    if GraduateFace <> goNone then  { Only has effect if GraduateFace is not goNone }
      Invalidate;
  end;
end;

{ Set highlight style and repaint }
procedure TLabelEffect.SetStyleHighlight(eoStyle: TEffectOption);
begin
  if FStyleHighlight <> eoStyle then
  begin
    FStyleHighlight := eoStyle;
    if FStyleHighlight in [eoNormal, eoReal] then
      ResizeHighlight := rsNone;
    Invalidate;
  end;
end;

{ Set shadow style and repaint }
procedure TLabelEffect.SetStyleShadow(eoStyle: TEffectOption);
begin
  if FStyleShadow <> eoStyle then
  begin
    FStyleShadow := eoStyle;
    if FStyleShadow in [eoNormal, eoReal] then
      ResizeShadow := rsNone;
    Invalidate;
  end;
end;

{ Set highlight resize option and repaint }
procedure TLabelEffect.SetResizeHighlight(rsSize: TResizeOption);
begin
  if FResizeHighlight <> rsSize then
  begin
    FResizeHighlight := rsSize;
    if StyleHighlight <> eoGraduated then
      StyleHighlight := eoExtrude;
    Invalidate;
  end;
end;

{ Set shadow resize option and repaint }
procedure TLabelEffect.SetResizeShadow(rsSize: TResizeOption);
begin
  if FResizeShadow <> rsSize then
  begin
    FResizeShadow := rsSize;
    if StyleShadow <> eoGraduated then
      StyleShadow := eoExtrude;
    Invalidate;
  end;
end;

{ Set overall effect style - combination of highlight and shadow directions and depths }
procedure TLabelEffect.SetEffectStyle(esStyle: TEffectStyle);
begin
  if FEffectStyle <> esStyle then
  begin
    bChangingStyle  := True;   { So it doesn't reset to custom }
    bChangingScheme := True;  {          "                    }
    FEffectStyle    := esStyle;
    SetColourHighlight(clrSchemes[ColourScheme, cpHighlight]);
    case FEffectStyle of
    esRaised:
      begin
        SetDirectionHighlight(edUpLeft);
        SetDirectionShadow(edDownRight);
        SetDepthHighlight(1);
        SetDepthShadow(1);
      end;
    esSunken:
      begin
        SetDirectionHighlight(edDownRight);
        SetDirectionShadow(edUpLeft);
        SetDepthHighlight(1);
        SetDepthShadow(1);
      end;
    esShadow:
      begin
        SetDirectionHighlight(edNone);
        SetDirectionShadow(edDownRight);
        SetDepthHighlight(0);
        SetDepthShadow(2);
        SetAsButton(False);
      end;
    esFlying:
      begin
        SetDirectionHighlight(edDownRight);
        SetDirectionShadow(edDownRight);
        SetDepthHighlight(1);
        SetDepthShadow(5);
        SetColourHighlight(clrSchemes[ColourScheme, cpShadow]);  { Flying has two shadows }
        SetAsButton(False);
      end;
    esNone:
      begin
        SetDirectionHighlight(edNone);
        SetDirectionShadow(edNone);
        SetDepthHighlight(0);
        SetDepthShadow(0);
        SetAsButton(False);
      end;
    else
      SetAsButton(False);
    end;
    bChangingStyle  := False;  { So further changes set style to custom }
    bChangingScheme := False;  { So further changes set colour scheme to custom }
  end;
end;

{ Set overall colour scheme }
procedure TLabelEffect.SetColourScheme(csScheme: TColourScheme);
begin
  if FColourScheme <> csScheme then
  begin
    bChangingScheme := True;  { So it doesn't reset to custom }
    FColourScheme   := csScheme;
    SetColourHighlight(clrSchemes[FColourScheme, cpHighlight]);
    SetColourFace(clrSchemes[FColourScheme, cpFace]);
    SetColourShadow(clrSchemes[FColourScheme, cpShadow]);
    if FColourScheme <> csCustom then  { Save for future reference }
    begin
      clrSchemes[csCustom, cpHighlight] := clrSchemes[FColourScheme, cpHighlight];
      clrSchemes[csCustom, cpFace]      := clrSchemes[FColourScheme, cpFace];
      clrSchemes[csCustom, cpShadow]    := clrSchemes[FColourScheme, cpShadow];
    end;
    bChangingScheme := False;  { So further changes set colour scheme to custom }
  end;
end;

{ Set background bitmap to be masked and repaint }
procedure TLabelEffect.SetBitmap(bmp: TBitmap);
begin
  FBitmap.Assign(bmp);
end;

{ Change background bitmap }
procedure TLabelEffect.ChangeBitmap(Sender: TObject);
begin
  Invalidate;
end;

{ Set text to act like a button }
procedure TLabelEffect.SetAsButton(bBtn: Boolean);
begin
  if FAsButton <> bBtn then
  begin
    FAsButton := bBtn;
    if bBtn then    { If not already raised, raise it }
      SetEffectStyle(esRaised);
  end;
end;

{ Set angle of text and repaint }
procedure TLabelEffect.SetAngle(aAngle: TAngleRange);
begin
  if FAngle <> aAngle then
  begin
    FAngle      := aAngle;
    dCosAngle   := Cos(FAngle * dDegToRad);  { Calculate values for later use }
    dCosSquared := dCosAngle * dCosAngle;
    dSinAngle   := Sin(FAngle * dDegToRad);
    dSinSquared := dSinAngle * dSinAngle;
    if FAngle <> 0 then
      Alignment := taLeftJustify;  { Cannot align when rotated }
    Invalidate;
  end;
end;

{ Set rotated text to stay vertical and repaint }
procedure TLabelEffect.SetKeepLettersVertical(bKeep: Boolean);
begin
  if FKeepLettersVertical <> bKeep then
  begin
    FKeepLettersVertical := bKeep;
    if Angle <> 0 then  { Only has effect if Angle is non-zero }
      Invalidate;
  end;
end;

{ Set animation options }
procedure TLabelEffect.SetAnimation(leaAnim: TLabelEffectAnimation);
begin
  if FAnimation <> leaAnim then
    FAnimation.Assign(leaAnim);
end;

{ Animation options have changed }
procedure TLabelEffect.AnimationChanged(Sender: TObject);
begin
  with FAnimation do
  begin
    if ExternalTiming then
    begin
      tmrAnim.Free;
      tmrAnim := nil;
    end
    else
    begin
      if not Assigned(tmrAnim) then
        tmrAnim := TTimer.Create(Self);
      tmrAnim.Interval := Interval;
      tmrAnim.OnTimer  := AnimationTimer;
      tmrAnim.Enabled  := Enabled;
    end;
  end;
  Invalidate;
end;

{ Internal animation timer has expired }
procedure TLabelEffect.AnimationTimer(Sender: TObject);
begin
  NextAnimationFrame;
end;

{ Show next animation frame }
procedure TLabelEffect.NextAnimationFrame;
begin
  FAnimation.NextFrame;
end;

{ Show specified animation frame }
procedure TLabelEffect.ShowAnimationFrame(iIndex: Integer);
begin
  FAnimation.FrameIndex := iIndex;
end;

{ Return text face colour }
function TLabelEffect.GetColourFace: TColor;
begin
  Result := Font.Color;
end;

{ Return minimum of values }
function Min(iValues: array of Integer): Integer;
var
  i: Integer;
begin
  Result := iValues[0];
  if High(iValues) > 0 then
    for i := 1 to High(iValues) do
      if iValues[i] < Result then
        Result := iValues[i];
end;

{ Return maximum of values }
function Max(iValues: array of Integer): Integer;
var
  i: Integer;
begin
  Result := iValues[0];
  if High(iValues) > 0 then
    for i := 1 to High(iValues) do
      if iValues[i] > Result then
        Result := iValues[i];
end;

{ Extract red, green and blue values from a colour }
procedure GetRGB(clr: TColor; var iR, iG, iB: Byte);
begin
  iR := GetRValue(clr);
  iG := GetGValue(clr);
  iB := GetBValue(clr);
end;

{ Generate a rotated font and apply it to the specified canvas }
procedure TLabelEffect.SetTextAngle(cnv: TCanvas; aAngle: TAngleRange; iHeight: Integer);
var
  fntLogRec: TLogFont;    { Storage area for font information }
begin
  { Get the current font information. We only want to modify the angle }
  GetObject(cnv.Font.Handle, SizeOf(fntLogRec), Addr(fntLogRec));

  { Modify the angle. "The angle, in tenths of a degrees, between the base
    line of a character and the x-axis." (Windows API Help file.)}
  fntLogRec.lfEscapement := aAngle * 10;
  { Change size of font.
    "If this value is greater than zero, it specifies the cell height of the font.
    If it is less than zero, it specifies the character height of the font." }
  fntLogRec.lfHeight := iHeight;
  { Request TrueType precision }
  fntLogRec.lfOutPrecision := OUT_TT_ONLY_PRECIS;

  { Delphi will handle the deallocation of the old font handle }
  cnv.Font.Handle := CreateFontIndirect(fntLogRec);
end;

{ And now apply all these parameters and draw the thing! }
procedure TLabelEffect.Paint;
const
  wAlignments: array [TAlignment] of word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  { Offsets for drawing in the nominated directions }
  iOffsets: array [TEffectDirection, TDirXY] of -1..1 =
    ((0, 0), (0, -1), (+1, -1), (+1, 0), (+1, +1),
    (0, +1), (-1, +1), (-1, 0), (-1, -1));
  { Offsets for changing text sizes }
  iResizes: array [TResizeOption] of -1..1 = (0, +1, -1);
  rExpand: array [TEffectDirection, TDirXY] of Real =
    ((-0.5, -1.0), (-0.5, -2.0), (0.0, -2.0), (0.0, -1.0), (0.0, 0.0),
    (-0.5, 0.0), (-1.0, 0.0), (-1.0, -1.0), (-1.0, -2.0));
  rReduce: array [TEffectDirection, TDirXY] of Real =
    ((0.5, 0.5), (0.5, -1.0), (2.0, -1.0), (2.0, 0.5), (2.0, 2.0),
    (0.5, 2.0), (-1.0, 2.0), (-1.0, 0.5), (-1.0, -1.0));
  { Offsets for indent graduation }
  iIndents: array [0..5] of Byte = (1, 2, 4, 7, 9, 10);
  edIndents: array [1..9, goRIndented..goLIndented] of TEffectDirection =
    ((edUpLeft, edUpRight), (edLeft, edRight), (edUp, edUp),
    (edDownLeft, edDownRight), (edNone, edNone), (edUpRight, edUpLeft),
    (edDown, edDown), (edRight, edLeft), (edDownRight, edDownLeft));
var
  iMinOffset, iMaxOffset: Integer;
  rctTemp, rctFirst, rctLast: TRect;
  sText: array [0..255] of char;
  i, iMid, iH, iW, iX, iY, iU, iV, iLimit, iL, iLen: Integer;
  i1, i2, i3, i4, iAdj, iAdjX, iAdjY: Integer;
  iTextHeight, iTextWidth: Integer;
  p1, p2, p3, p4, p5, p6, p7, p8: TPoint;
  iFromR, iFromG, iFromB, iToR, iToG, iToB: Byte;
  rAdjustR, rAdjustG, rAdjustB: Real;
  bmpTemp, bmpWork, bmpForeground, bmpBackground, bmpSource: TBitmap;
  cnvWork: TCanvas;

  { Set offsets based on resize parameters }
  procedure SetResizeOffsets(rsResize: TResizeOption; edDir: TEffectDirection;
    iDepth: Integer; var iAdjX, iAdjY: Integer);
  var
    iNewWidth: Integer;
  begin
    if (Angle <> 0) and not KeepLettersVertical then  { Need to generate an angled font }
      SetTextAngle(cnvWork, Angle, Self.Font.Height - iResizes[rsResize] * iDepth)
    else
      cnvWork.Font.Size := Self.Font.Size + iResizes[rsResize] * iDepth;
    iNewWidth := cnvWork.TextWidth(Caption);
    case rsResize of
      rsExpand:
        begin
          iAdjX := Round(rExpand[edDir, drX] * (iNewWidth - iTextWidth));
          iAdjY := Round(rExpand[edDir, drY] * iDepth);
        end;
      rsReduce:
        begin
          iAdjX := Round(rReduce[edDir, drX] * (iTextWidth - iNewWidth));
          iAdjY := Round(rReduce[edDir, drY] * iDepth);
        end;
    end;
  end;

  { Apply all the effects for highlight or shadow }
  procedure PaintEffect(clrMain, clrGrad: TColor; edDir: TEffectDirection;
    iDepth: TEffectDepth; eoStyle: TEffectOption; rsResize: TResizeOption;
    iToReal: Byte; bIsFace: Boolean; iExtraY: Byte);
  var
    i, iL, iU, iV: Integer;
    iFromR, iFromG, iFromB, iToR, iToG, iToB: Byte;
    rAdjustR, rAdjustG, rAdjustB: Real;
  begin
    { Prepare for extruding highlight/shadow, if requested }
    GetRGB(clrMain, iFromR, iFromG, iFromB);
    rAdjustR := 0;
    rAdjustG := 0;
    rAdjustB := 0;
    iLimit   := iDepth;
    if (eoStyle = eoReal) or bIsFace then  { Keep black - replaced later }
      GetRGB(clBlack, iFromR, iFromG, iFromB)
    else if (eoStyle <> eoNormal) and (iDepth > 1) then
    begin
      iLimit := 1;
      if eoStyle = eoGraduated then  { Set changes in RGB colours }
      begin
        GetRGB(clrGrad, iToR, iToG, iToB);
        rAdjustR := (iToR - iFromR) / (iDepth - 1);
        rAdjustG := (iToG - iFromG) / (iDepth - 1);
        rAdjustB := (iToB - iFromB) / (iDepth - 1);
      end;
    end;

    { Set font }
    cnvWork.Font := Self.Font;
    if (Angle <> 0) and not KeepLettersVertical then
      SetTextAngle(cnvWork, Angle, Self.Font.Height);

    { Process for each copy of the highlight/shadow - several if extruding }
    for i := iDepth downto iLimit do
    begin
      if rsResize = rsNone then
      begin
        iAdjX := iOffsets[edDir, drX] * i - iAdj;
        iAdjY := iOffsets[edDir, drY] * i + iExtraY;
      end
      else  { Resize text and calculate offsets }
      begin
        SetResizeOffsets(rsResize, edDir, i, iAdjX, iAdjY);
        Dec(iAdjX, iAdj);
        Inc(iAdjY, iExtraY);
      end;
      { Set current colour }
      cnvWork.Font.Color := RGB(iFromR + Round(rAdjustR * (iDepth - i)),
        iFromG + Round(rAdjustG * (iDepth - i)), iFromB + Round(rAdjustB * (iDepth - i)));
      { And draw the text }
      if Angle = 0 then
      begin
        { Create a rect that is offset for the highlight/shadow/text }
        rctTemp:= Rect(
          Self.ClientRect.Left - iMinOffset + iAdjX, Self.ClientRect.Top - iMinOffset + iAdjY,
          Self.ClientRect.Right - iMinOffset + iAdjX, Self.ClientRect.Bottom - iMinOffset + iAdjY);
        { Draw highlight text with alignment }
        DrawText(cnvWork.Handle, sText, StrLen(sText), rctTemp,
            DT_EXPANDTABS or DT_WORDBREAK or wAlignments[Alignment]);
      end
      else if KeepLettersVertical then
        for iL := 1 to iLen do  { For each character }
        begin
          iW := 0; iH := 0;
          if iL > 1 then  { Position based on substring length and height }
          begin
            iH := Round((iTextHeight * (iL - 1) * dSinSquared) +
              (cnvWork.TextWidth(Copy(Caption, 1, iL - 1)) * dCosSquared));
            iW := Round(iH * dCosAngle);
            iH := Round(iH * dSinAngle);
          end;
          cnvWork.TextOut(iX - iMinOffset + iW + iOffsets[edDir, drX] * i,
            iY - iMinOffset - iH + iOffsets[edDir, drY] * i,
            Copy(Caption, iL, 1));
        end
      else
        { Draw angled highlight/shadow text without alignment }
        cnvWork.TextOut(iX - iMinOffset + iOffsets[edDir, drX] * i,
          iY - iMinOffset + iOffsets[edDir, drY] * i, Caption);
    end;

    if eoStyle = eoReal then    { Real/transparent highlight/shadow }
    begin
      for iU := 0 to Width do
        for iV := 0 to Height do
          if bmpTemp.Canvas.Pixels[iU, iV] = clBlack then
          begin    { Make halfway to white/black }
            GetRGB(bmpWork.Canvas.Pixels[iU, iV], iToR, iToG, iToB);
            bmpWork.Canvas.Pixels[iU, iV] :=
              RGB((iToReal + iToR) div 2, (iToReal + iToG) div 2, (iToReal + iToB) div 2);
          end;
    end;
  end;

begin
  { Create temporary drawing surfaces }
  bmpTemp   := TBitmap.Create;
  bmpWork   := TBitmap.Create;
  bmpSource := TBitmap.Create;

  try
    { Same sizes as original }
    bmpTemp.Height := Self.Height;
    bmpTemp.Width  := Self.Width;
    bmpWork.Height := Self.Height;
    bmpWork.Width  := Self.Width;

    { Get reference to current bitmap (if any) }
    if Assigned(FAnimation.Frames) then
      FAnimation.Frames.GetBitmap(FAnimation.FrameIndex, bmpSource)
    else
      bmpSource.Assign(FBitmap);

    with bmpWork.Canvas do
    begin
      { Initialise work bitmap with current screen image }
      CopyRect(Self.ClientRect, Self.Canvas, Self.ClientRect);
      { Set font }
      Font := Self.Font;
      if (Angle <> 0) and not KeepLettersVertical then
        SetTextAngle(bmpWork.Canvas, Angle, Self.Font.Height);
      iTextHeight := TextHeight(Caption);
      iTextWidth  := TextWidth(Caption);
      iLen        := Length(Caption);

      { Find minimum and maximum of all offsets (including font itself) }
      iMinOffset := Min([iOffsets[DirectionHighlight, drX] * DepthHighlight,
                    iOffsets[DirectionShadow, drX] * DepthShadow,
                    iOffsets[DirectionHighlight, drY] * DepthHighlight,
                    iOffsets[DirectionShadow, drY] * DepthShadow, 0]);
      iMaxOffset := Max([iOffsets[DirectionHighlight, drX] * DepthHighlight,
                    iOffsets[DirectionShadow, drX] * DepthShadow,
                    iOffsets[DirectionHighlight, drY] * DepthHighlight,
                    iOffsets[DirectionShadow, drY] * DepthShadow, 0]);
      case Alignment of
        taLeftJustify:  iAdj := 0;
        taCenter:       iAdj := (iMaxOffset - iMinOffset) div 2;
        taRightJustify: iAdj := iMaxOffset - iMinOffset;
      end;

      { Adjust offsets for resizing }
      cnvWork := bmpWork.Canvas;
      if ResizeHighlight <> rsNone then
      begin
        SetResizeOffsets(ResizeHighlight, DirectionHighlight, DepthHighlight, iAdjX, iAdjY);
        iMinOffset := Min([iAdjX, iAdjY, iMinOffset]);
        iMaxOffset := Max([iAdjX, iAdjY, iMaxOffset]);
      end;
      if ResizeShadow <> rsNone then
      begin
        SetResizeOffsets(ResizeShadow, DirectionShadow, DepthShadow, iAdjX, iAdjY);
        iMinOffset := Min([iAdjX, iAdjY, iMinOffset]);
        iMaxOffset := Max([iAdjX, iAdjY, iMaxOffset]);
      end;

      { Set starting point for text - iX, iY }
      if Angle = 0 then
      begin
        iX := 0;
        iY := 0;
      end
      else if KeepLettersVertical then
      begin
        iH := Round((iTextHeight * (iLen - 1) * dSinSquared) +
          (TextWidth(Copy(Caption, 1, iLen - 1)) * dCosSquared));
        iX := Max([0, - Round(iH * dCosAngle)]) + 10;
        iY := Max([0, Round(iH * dSinAngle)]) + 10;
        iL := iMaxOffset - iMinOffset + 4;
        { Find rectangles surrounding first and last characters }
        rctFirst := Bounds(iX - 2, iY - 2,
          TextWidth(Copy(Caption, 1, 1)) + iL, iTextHeight + iL);
        rctLast := Bounds(iX - 2 + Round(iH * dCosAngle), iY - 2 - Round(iH * dSinAngle),
          TextWidth(Copy(Caption, iLen, 1)) + iL, iTextHeight + iL);
      end
      else  { Offset from centre of rotation for text }
      begin
        iW   := iTextWidth;
        iH   := iTextHeight;
        iMid := TextWidth(Caption + '   ') div 2;
        iX   := iMid - Trunc(iW / 2 * dCosAngle) - Trunc(iH / 2 * dSinAngle);
        iY   := iMid + Trunc(iW / 2 * dSinAngle) - Trunc(iH / 2 * dCosAngle);

        iMid := iMid + (iMaxOffset - iMinOffset + 4) div 2;
        iW   := iW + iMaxOffset + iMinOffset + 4;
        iH   := iH + iMaxOffset + iMinOffset + 4;
        i1   := Trunc(iW / 2 * dCosAngle);
        i2   := Trunc(iH / 2 * dSinAngle);
        i3   := Trunc(iW / 2 * dSinAngle);
        i4   := Trunc(iH / 2 * dCosAngle);
        p1   := Point(iMid - i1 - i2 + 2, iMid + i3 - i4 + 2);
        p2   := Point(iMid + i1 - i2 + 2, iMid - i3 - i4 + 2);
        p3   := Point(iMid + i1 + i2 + 2, iMid - i3 + i4 + 2);
        p4   := Point(iMid - i1 + i2 + 2, iMid + i3 + i4 + 2);
      end;

      if not Transparent then  { Fill in background on offscreen copy canvas }
      begin
        Brush.Color := Self.Color;
        Brush.Style := bsSolid;
        if Angle = 0 then
          FillRect(ClientRect)
        else if KeepLettersVertical then
        begin                  { Calculate enclosing polygon }
          p1 := Point(rctFirst.Left, rctFirst.Top);
          p2 := Point(rctFirst.Left, rctFirst.Bottom);
          p3 := Point(rctFirst.Right, rctFirst.Top);
          p4 := Point(rctFirst.Right, rctFirst.Bottom);
          p5 := Point(rctLast.Left, rctLast.Top);
          p6 := Point(rctLast.Left, rctLast.Bottom);
          p7 := Point(rctLast.Right, rctLast.Top);
          p8 := Point(rctLast.Right, rctLast.Bottom);
          case Angle of
          1..90    : Polygon([p1, p5, p7, p8, p4, p2]);
          91..180  : Polygon([p3, p7, p5, p6, p2, p4]);
          181..270 : Polygon([p1, p5, p6, p8, p4, p3]);
          271..360 : Polygon([p3, p7, p8, p6, p2, p1]);
          end;
        end
        else
          Polygon([p1, p2, p3, p4]);
      end;
      Brush.Style := bsClear;         { Don't overwrite background above }
    end;

    GetTextBuf(sText, SizeOf(sText));  { Get label's caption }

    { Paint shadow }
    if StyleShadow = eoReal then  { Work on copy then transfer }
      cnvWork := bmpTemp.Canvas
    else                          { Work directly on offscreen canvas }
      cnvWork := bmpWork.Canvas;
    PaintEffect(ColourShadow, GraduateShadow, DirectionShadow,
      DepthShadow, StyleShadow, ResizeShadow, 0, False, 0);

    { Paint highlight }
    if StyleHighlight = eoReal then  { Work on copy then transfer }
    begin
      bmpTemp.Canvas.FillRect(ClientRect);
      cnvWork := bmpTemp.Canvas;
    end
    else                             { Work directly on offscreen canvas }
      cnvWork := bmpWork.Canvas;
    PaintEffect(ColourHighlight, GraduateHighlight, DirectionHighlight,
      DepthHighlight, StyleHighlight, ResizeHighlight, 255, False, 0);

    { Paint original text }
    if (bmpSource.Width <> 0) or (GraduateFace <> goNone) then  { Work on copy then transfer }
    begin
      bmpTemp.Canvas.FillRect(ClientRect);
      cnvWork := bmpTemp.Canvas;
    end
    else                                                      { Work directly on offscreen canvas }
      cnvWork := bmpWork.Canvas;
    PaintEffect(ColourFace, ColourFace, edNone, 0, eoNormal, rsNone, 0,
      (bmpSource.Width <> 0) or (GraduateFace <> goNone), 0);

    if (bmpSource.Width <> 0) or (GraduateFace <> goNone) then  { Create extra bitmaps }
    begin
      bmpForeground := TBitmap.Create;
      bmpForeground.Assign(bmpWork);
      bmpBackground := TBitmap.Create;
      bmpBackground.Assign(bmpWork);
    end;

    { Prepare for applying effects to the original text face }
    if bmpSource.Width <> 0 then  { Copy bitmap to foreground bitmap }
    begin
      iU := 0;
      while iU < bmpForeground.Width do
      begin
        iV := 0;
        while iV < bmpForeground.Height do
        begin
          bmpForeground.Canvas.Draw(iU, iV, bmpSource);
          Inc(iV, bmpSource.Height);
        end;
        Inc(iU, bmpSource.Width);
      end;
    end
    else if GraduateFace <> goNone then  { Set up graduated bitmap }
    begin                                { Calculate start point and extent }
      if Angle = 0 then
      begin
        iV := iY - iMinOffset;
        case Alignment of
          taLeftJustify:  iU := iX - iMinOffset;
          taCenter:       iU := (Self.Width - iTextWidth) div 2 - iMinOffset - iAdj;
          taRightJustify: iU := Self.Width - iMaxOffset - iTextWidth;
        end;
        iW := iTextWidth;
        iH := iTextHeight;
      end
      else if KeepLettersVertical then
      begin
        iU := Min([rctFirst.Left, rctLast.Left]);
        iV := Min([rctFirst.Top, rctLast.Top]);
        iW := Max([rctFirst.Right, rctLast.Right]) - iU;
        iH := Max([rctFirst.Bottom, rctLast.Bottom]) - iV;
      end
      else
      begin
        iU := Min([p1.X, p2.X, p3.X, p4.X]);
        iV := Min([p1.Y, p2.Y, p3.Y, p4.Y]);
        iW := Max([p1.X, p2.X, p3.X, p4.X]) - iU;
        iH := Max([p1.Y, p2.Y, p3.Y, p4.Y]) - iV;
      end;
      case GraduateFace of
        goVertical:                      iLimit := iH;
        goHorizontal:                    iLimit := iW;
        goFDiagonal, goBDiagonal:        iLimit := iW + iH;
        goBoxed:                         iLimit := iH div 2;
        goRIndented, goLIndented: iLimit := 4;
      end;

      { Calculate change in colour at each step }
      GetRGB(GraduateFrom, iFromR, iFromG, iFromB);
      GetRGB(ColourFace, iToR, iToG, iToB);
      rAdjustR := (iFromR - iToR) / iLimit;
      rAdjustG := (iFromG - iToG) / iLimit;
      rAdjustB := (iFromB - iToB) / iLimit;

      { And draw it onto the foreground canvas }
      bmpForeground.Canvas.Brush.Style := bsSolid;
      if GraduateFace in [goRIndented, goLIndented] then
      begin
        bmpBackground.Assign(bmpTemp);
        bmpTemp.Canvas.FillRect(ClientRect);
        bmpTemp.Canvas.Brush.Style := bsClear;
        cnvWork                    := bmpTemp.Canvas;
        if GraduateFace = goRIndented then
        begin
          Dec(iMinOffset);
          i2 := 0;
        end
        else
        begin
          Inc(iMinOffset);
          i2 := 2;
        end;
      end;
      for i := 0 to iLimit do
      begin
        bmpForeground.Canvas.Brush.Color := RGB(iFromR - Round(rAdjustR * i),
          iFromG - Round(rAdjustG * i), iFromB - Round(rAdjustB * i));
        bmpForeground.Canvas.Pen.Color := bmpForeground.Canvas.Brush.Color;
        case GraduateFace of
          goVertical:
            bmpForeground.Canvas.FillRect(Rect(iU, iV + i, iU + iW, iV + i + 1));
          goHorizontal:
            bmpForeground.Canvas.FillRect(Rect(iU + i, iV, iU + i + 1, iV + iH));
          goFDiagonal:
            bmpForeground.Canvas.Polygon([Point(iU + i, iV), Point(iU + i - iH, iV + iH),
              Point(iU + i - iH + 2, iV + iH), Point(iU + i + 2, iV)]);
          goBDiagonal:
            bmpForeground.Canvas.Polygon([Point(iU + i - iH, iV), Point(iU + i, iV + iH),
              Point(iU + i + 2, iV + iH), Point(iU + i - iH + 2, iV)]);
          goBoxed:
            bmpForeground.Canvas.FillRect(Rect(iU + i, iV + i, iU + iW - i, iV + iH - i));
          goRIndented, goLIndented:
            for i1 := iIndents[i] to iIndents[i + 1] - 1 do
              PaintEffect(bmpForeground.Canvas.Brush.Color, bmpForeground.Canvas.Brush.Color,
                edIndents[i1, GraduateFace], 1, eoNormal, rsNone, 0, False, i2);
        end;
      end;
      if GraduateFace in [goRIndented, goLIndented] then
      begin
        bmpForeground.Assign(bmpTemp);
        bmpTemp.Assign(bmpBackground);
        if GraduateFace = goRIndented then
          Inc(iMinOffset)
        else
          Dec(iMinOffset);
      end;
    end;

    { Apply bitmap to font image }
    if (bmpSource.Width <> 0) or (GraduateFace <> goNone) then
    begin
      { Mask out background }
      bmpBackground.Canvas.CopyMode := cmSrcCopy;
      bmpBackground.Canvas.CopyRect(ClientRect, bmpWork.Canvas, ClientRect);
      bmpBackground.Canvas.CopyMode := cmSrcAnd;
      bmpBackground.Canvas.CopyRect(ClientRect, bmpTemp.Canvas, ClientRect);
      { Mask out foreground }
      bmpWork.Canvas.CopyMode := cmSrcCopy;
      bmpWork.Canvas.CopyRect(ClientRect, bmpTemp.Canvas, ClientRect);
      bmpWork.Canvas.CopyMode := cmSrcErase;
      bmpWork.Canvas.CopyRect(ClientRect, bmpForeground.Canvas, ClientRect);
      { And combine the two }
      bmpWork.Canvas.CopyMode := cmSrcPaint;
      bmpWork.Canvas.CopyRect(ClientRect, bmpBackground.Canvas, ClientRect);
      bmpForeground.Free;
      bmpBackground.Free;
    end;

    { Copy final result back to the screen }
    Self.Canvas.CopyRect(Self.ClientRect, bmpWork.Canvas, Self.ClientRect);
  finally
    bmpTemp.Free;
    bmpWork.Free;
    bmpSource.Free;
  end;
end;

procedure TLabelEffect.MouseDown(mbBtn: TMouseButton; ssShift: TShiftState;
    x, y: Integer);
begin
  if AsButton then
  begin    { If left button and label isn't sunken }
    if (mbBtn = mbLeft) and (EffectStyle <> esSunken) and Enabled then
      SetEffectStyle(esSunken);
  end
  else
    inherited MouseDown(mbBtn, ssShift, x, y);
end;

procedure TLabelEffect.MouseMove(ssShift: TShiftState; x, y: Integer);
begin
  if AsButton then
  begin
    if ssShift = [ssLeft] then  { Left mouse button down }
    begin                       { If within label's client area }
      if  (x >= 0) and (x <= ClientWidth) and (y >= 0) and (y <= ClientHeight) then
        SetEffectStyle(esSunken)
      else
        SetEffectStyle(esRaised);
    end;
  end
  else
    inherited MouseMove(ssShift, x, y);
end;

procedure TLabelEffect.MouseUp(mbBtn: TMouseButton; ssShift: TShiftState;
    x, y: Integer);
begin
  if AsButton then
  begin    { If within label's client area }
    if (x >= 0) and (x <= ClientWidth) and (y >= 0) and (y <= ClientHeight) then
      SetEffectStyle(esRaised);
  end
  else
    inherited MouseUp(mbBtn, ssShift, x, y);
end;

{ TLabelEffectAnimation -------------------------------------------------------}

{ Initialise animation options }
constructor TLabelEffectAnimation.Create;
begin
  inherited Create;
  FContinuous     := True;
  FEnabled        := False;
  FExternalTiming := False;
  FFrameIndex     := 0;
  FFrames         := nil;
  FInterval       := 500;
  FStyle          := asForwards;
  FOnChange       := nil;
  iIncrement      := +1;
  bFirstTime      := True;
end;

{ Copy from other animation options }
procedure TLabelEffectAnimation.Assign(Source: TPersistent);
begin
  if Source is TLabelEffectAnimation then  { Can copy }
    with TLabelEffectAnimation(Source) do
    begin
      Self.FContinuous     := Continuous;
      Self.FEnabled        := Enabled;
      Self.FExternalTiming := ExternalTiming;
      Self.FFrameIndex     := FrameIndex;
      Self.FFrames         := Frames;
      Self.FInterval       := Interval;
      Self.FStyle          := Style;
      Self.FOnChange       := OnChange;
      Changed;
    end
  else  { Try default assign }
    inherited Assign(Source);
end;

{ Change continuous option }
procedure TLabelEffectAnimation.SetContinuous(bContinuous: Boolean);
begin
  if FContinuous <> bContinuous then
  begin
    FContinuous := bContinuous;
    Changed;
  end;
end;

{ Dis/enable animation }
procedure TLabelEffectAnimation.SetEnabled(bEnabled: Boolean);
begin
  if FEnabled <> bEnabled then
  begin
    FEnabled := bEnabled;
    Changed;
  end;
end;

{ Use internal or external timer }
procedure TLabelEffectAnimation.SetExternalTiming(bExternal: Boolean);
begin
  if FExternalTiming <> bExternal then
  begin
    FExternalTiming := bExternal;
    Changed;
  end;
end;

{ Change current image in animation }
procedure TLabelEffectAnimation.SetFrameIndex(iIndex: Integer);
begin
  if (FFrameIndex <> iIndex) and (iIndex >= 0) then
  begin
    FFrameIndex := iIndex;
    Changed;
  end;
end;

{ Change list of images to use for animation }
procedure TLabelEffectAnimation.SetFrames(imlFrames: TImageList);
begin
  if FFrames <> imlFrames then
  begin
    FFrames     := imlFrames;
    FFrameIndex := 0;
    if not Assigned(FFrames) then
      FEnabled := False;
    Changed;
  end;
end;

{ Change interval for the timer }
procedure TLabelEffectAnimation.SetInterval(iInterval: Cardinal);
begin
  if FInterval <> iInterval then
  begin
    FInterval := iInterval;
    Changed;
  end;
end;

{ Change animation style }
procedure TLabelEffectAnimation.SetStyle(iStyle: TLEAnimationStyle);
begin
  if FStyle <> iStyle then
  begin
    FStyle      := iStyle;
    FFrameIndex := 0;
    bFirstTime  := True;
    if FStyle = asBackwards then
    begin
      iIncrement := -1;
      if Assigned(FFrames) then
        FFrameIndex := FFrames.Count - 1;
    end
    else
      iIncrement := +1;
    Changed;
  end;
end;

{ Something has changed - tell everyone about it }
procedure TLabelEffectAnimation.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ Move to next image }
procedure TLabelEffectAnimation.NextFrame;
var
  iCount: Integer;
begin
  Inc(FFrameIndex, iIncrement);
  iCount := 0;
  if Assigned(FFrames) then
    iCount := FFrames.Count;
  case FStyle of
    asForwards:  if FFrameIndex > iCount - 1 then
                   if FContinuous then
                     FFrameIndex := 0
                   else
                   begin
                     FFrameIndex := iCount - 1;
                     FEnabled    := False;
                   end;
    asBackwards: if FFrameIndex < 0 then
                   if FContinuous then
                     FFrameIndex := iCount - 1
                   else
                   begin
                     FFrameIndex := 0;
                     FEnabled    := False;
                   end;
    asBounce:    if (FFrameIndex < 0) or (FFrameIndex > iCount - 1) then
                 begin
                   iIncrement := -iIncrement;
                   Inc(FFrameIndex, iIncrement);
                   if not FContinuous and not bFirstTime then
                     FEnabled := False;
                   bFirstTime := False;
                 end;
  end;
  Changed;
end;

end.
