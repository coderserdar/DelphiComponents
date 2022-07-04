unit FileBtn;

{ TFileButton (C)opyright 2002 Version 1.01
  Autor : Simon Reinhardt
  eMail : reinhardt@picsoft.de
  Internet : http://www.picsoft.de

  Die Komponente TFileButton ist eine SpeedButton-ähnliche Komponente. Sie ist
  abgeleteitet von TGraphicControl und dient dem einfachen Öffnen von Dateien.

  Die Komponente ist Public Domain, das Urheberrecht liegt aber beim Autor. }

interface

{$I SRDefine.inc}

uses
  {$IFDEF SR_Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF} Classes,
  Graphics, Controls, SysUtils, Messages;

const
  bsDown = -1;
  bsFlat = 0;
  bsUp   = 1;

type
  TAppShowStates = (SW_Hide, SW_ShowNormal, SW_ShowMinimized, SW_ShowMaximized,
                    SW_ShowNoActivate, SW_Show, SW_Minimize, SW_ShowMinNoActive,
                    SW_ShowNA, SW_Restore, SW_ShowDefault);
  TBevelWidth    = 1..10;
  TContrast      = 0..9;

  TFileButton = class(TGraphicControl)
  private
    FAppShowState     : TAppShowStates;
    FAsynchronous     : boolean;
    FBackgroundColor  : TColor;
    FBevelContrast    : TContrast;
    FBevelWidth       : TBevelWidth;
    FBitmap           : TBitmap;
    FCaption          : string;
    FColor,
    FColorActive,
    FColorHighlight,
    FColorShadow      : TColor;
    FDirectory,
    FDisplayName,
    FFileName         : string;
    FFlat             : boolean;
    FIcon             : TIcon;
    FMargin           : shortint;
    FMouseDown,
    FMouseOverButton  : boolean;
    FOldHeight,
    FOldWidth         : integer;
    FParameters       : string;
    FShowFilename,
    FShowIconFrame    : boolean;
    FSpacing,
    FState            : shortint;
    FTransparent      : boolean;
    FTransparentColor : TColor;

    FOnClick,
    FOnMouseEnter,
    FOnMouseExit      : TNotifyEvent;

    procedure CMEnabledChanged(var Message:TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

  protected
    procedure AssignBevelColors;
    procedure AssignFileIcon;
    procedure Click; override;
    procedure Paint; override;
    procedure PaintBevel(ARect:TRect);
    procedure PaintButton;

    procedure SetBackgroundColor(newValue: TColor);
    procedure SetBevelContrast(newValue: TContrast);
    procedure SetBevelWidth(newValue: TBevelWidth);
    procedure SetCaption(newValue: string);
    procedure SetColor(newValue: TColor);
    procedure SetDisplayName;
    procedure SetFileName(newValue: string);
    procedure SetFlat(newValue: boolean);
    procedure SetIcon(newValue: TIcon);
    procedure SetMargin(newValue: shortint);
    procedure SetShowFilename(newValue: boolean);
    procedure SetShowIconFrame(newValue: boolean);
    procedure SetSpacing(newValue: shortint);
    procedure SetTransparent(newValue: boolean);

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    {$IFDEF SR_Delphi5_Up}
    property Action;
    property Anchors;
    {$ENDIF}
    property AppShowState: TAppShowStates read FAppShowState write FAppShowState;
    property Asynchronous: boolean read FAsynchronous write FAsynchronous;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BevelContrast: TContrast read FBevelContrast write SetBevelContrast;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth;
    property Caption: string read FCaption write SetCaption;
    property Color: TColor read FColor write SetColor;
    property Directory: string read FDirectory write FDirectory;
    property Enabled;
    property FileName: string read FFileName write SetFileName;
    property Flat: boolean read FFlat write SetFlat;
    property Font;
    property Icon: TIcon read FIcon write SetIcon;
    property Margin: shortint read FMargin write SetMargin;
    property Parameters: string read FParameters write FParameters;
    property ParentFont;
    property ParentShowHint;
    {$IFDEF SR_Delphi3_Up}
    property PopupMenu;
    {$ENDIF}
    property ShowFilename: boolean read FShowFilename write SetShowFilename;
    property ShowIconFrame: boolean read FShowIconFrame write SetShowIconFrame;
    property ShowHint;
    property Spacing: shortint read FSpacing write SetSpacing;
    property Transparent: boolean read FTransparent write SetTransparent;
    property Visible;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit  write FOnMouseExit;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

{$IFDEF SR_Delphi2_Up}
{$R *.D32}
uses SRUtils, ShellAPI, rrColors;
{$ELSE}
{$R *.D16}
uses SRUtils, ShellAPI;
{$ENDIF}

const
  DefaultWidth    = 40;
  DefaultHeight   = 40;
  DefaultContrast = 5;
  ActContrast     = 2;

{$IFDEF SR_Delphi1}
function ChangeBrightness(Color:TColor;Percentage:longint):TColor;
var RGBColor       : longint;
    Red,Green,Blue : byte;
    NewR,NewG,NewB : longint;
    Overflow       : longint;
begin
  RGBColor:=ColorToRGB(Color);
  Overflow:=0;
  {Rot}
  Red:=GetRValue(RGBColor);
  NewR:=Red+(Percentage*Red div 100);
  if NewR>255 then begin
    Overflow:=NewR-255;
    NewG:=Overflow;
    NewB:=Overflow;
  end
  else begin
    NewG:=0;
    NewB:=0;
  end;
  {Grün}
  Green:=GetGValue(RGBColor);
  NewG:=NewG+Green+(Percentage*Green div 100);
  if NewG>255 then begin
    Overflow:=NewG-255;
    NewR:=NewR+Overflow;
    NewB:=Overflow;
  end;
  {Blau}
  Blue:=GetBValue(RGBColor);
  NewB:=NewB+Blue+(Percentage*Blue div 100);
  if NewB>255 then begin
    Overflow:=NewB-255;
    if NewG<=255 then
      NewR:=NewR+Overflow;
  end;
  if NewR>255 then
    NewR:=255;
  if NewG>255 then
    NewG:=255;
  if NewB>255 then
    NewB:=255;
  if NewR<0 then
    NewR:=0;
  if NewG<0 then
    NewG:=0;
  if NewB<0 then
    NewB:=0;
  Result:=NewR+(NewG shl 8)+(NewB shl 16);
end;

function ExtractAssociatedIcon(HInst:THandle; FileName:string; AIndex:word):HIcon;
var FileExt : string;
    ExeName,
    Buf     : PChar;
begin
  FileExt:=LowerCase(ExtractFileExt(FileName));
  if (FileExt<>'.exe') and (FileExt<>'.dll') and (FileExt<>'.ico') then begin
    Buf:=StrAlloc(255);
    ExeName:=StrAlloc(255);
    StrPCopy(Buf, FileName);
    if FindExecutable(Buf, nil, ExeName)>32 then
      FileName:=StrPas(ExeName);
    StrDispose(ExeName);
    StrDispose(Buf);
  end;
  Buf:=StrAlloc(255);
  StrPCopy(Buf, FileName);
  Result:=ExtractIcon(HInst, Buf, AIndex);
  StrDispose(Buf);
end;
{$ENDIF}


{ Komponente TFileButton }
constructor TFileButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FIcon:=TIcon.Create;
  FBitmap:=TBitmap.Create;
  FBitmap.Width:=FIcon.Width;
  FBitmap.Height:=FIcon.Height;
  {$IFDEF SR_Delphi3_Up}
  FBitmap.PixelFormat:=pf24Bit;
  {$ENDIF}

  {Vorgabewerte setzen}
  FAppShowState:=SW_ShowNormal;
  FAsynchronous:=true;
  FBackgroundColor:=clBtnFace;
  FBevelContrast:=DefaultContrast;
  FBevelWidth:=1;
  FColor:=clBtnFace;
  FDirectory:='';
  AssignBevelColors;
  FFileName:='';
  FFlat:=false;
  FMargin:=-1;
  FParameters:='';
  FSpacing:=-1;
  FState:=bsUp;
  FTransparent:=false;

  Height:=DefaultHeight;
  Width:=DefaultWidth;
  FOldHeight:=Height;
  FOldWidth:=Width;
end;

destructor TFileButton.Destroy;
begin
  FIcon.Free;
  FBitmap.Free;
  inherited Destroy;
end;

procedure TFileButton.AssignBevelColors;
var HLContrast,
    ShContrast : TContrast;
begin
  HLContrast:=FBevelContrast;
  if HLContrast<High(TContrast) then
    HLContrast:=HLContrast+1;
  ShContrast:=FBevelContrast;
  {$IFDEF SR_Delphi1}
  FColorActive:=ChangeBrightness(FColor, -100 div 10*ActContrast);
  FColorHighlight:=ChangeBrightness(FColor, 100 div 10*HLContrast);
  FColorShadow:=ChangeBrightness(FColor, -100 div 10*ShContrast);
  {$ELSE}
  Get3DColors(FColor, FColorHighlight, FColorActive, 1, (10-ActContrast)/10);
  Get3DColors(FColor, FColorHighlight, FColorShadow, (10-HLContrast)/10, (10-ShContrast)/10);
  {$ENDIF}
end;

procedure TFileButton.AssignFileIcon;
var AIndex : word;
begin
  if FileExists(FFilename) then begin
    AIndex:=0;
    {$IFDEF SR_Delphi1}
    FIcon.Handle:=ExtractAssociatedIcon(HInstance, FFileName, AIndex);
    {$ELSE}
    FIcon.Handle:=ExtractAssociatedIcon(HInstance, PChar(FFileName), AIndex);
    {$ENDIF}
    if FTransparent then
      FBitmap.Canvas.Brush.Color:=FBackgroundColor
    else
      FBitmap.Canvas.Brush.Color:=FColor;
    FBitmap.Canvas.FillRect(Rect(0, 0, 31, 31));
    if FIcon.Handle>0 then begin
      FBitmap.Canvas.Draw(0, 0, FIcon);
      FTransparentColor:=FBitmap.Canvas.Pixels[0, 0];
    end;
  end
  else begin
    FIcon.Handle:=0;
    FBitmap.Canvas.Brush.Color:=FColor;
    FBitmap.Canvas.FillRect(Rect(0, 0, 31, 31));
  end;
end;

procedure TFileButton.Click;
{$IFDEF SR_Delphi1}
var FText,
    DText : PChar;
{$ENDIF}
begin
  if Enabled and FMouseOverButton then begin
    if FAsynchronous then begin
      {$IFDEF SR_Delphi1}
      FText:=StrAlloc(255);
      DText:=StrAlloc(255);
      StrPCopy(FText, '"'+FFileName+'" '+FParameters);
      StrPCopy(DText, FDirectory);
      ShellExecute(0, 'open', FText, nil, DText, ord(FAppShowState));
      StrDispose(DText);
      StrDispose(FText);
      {$ELSE}
      ShellExecute(0, 'open',
                   PChar(FFileName), nil,
                   PChar(FDirectory),
                   ord(FAppShowState))
      {$ENDIF}
    end
    else begin
      Enabled:=false;
      ExecAndWait(FFileName, '', ord(FAppShowState));
      Enabled:=true;
    end;
  end;
end;

procedure TFileButton.CMEnabledChanged(var Message:TMessage);
begin
  inherited;
  if assigned(FBitmap) then begin
    FBitmap.Monochrome:=not Enabled;
    {$IFDEF SR_Delphi3_Up}
    if Enabled then
      FBitmap.PixelFormat:=pf24Bit;
    {$ENDIF}
    AssignFileIcon;
  end;
  Invalidate;
end;

procedure TFileButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TFileButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Enabled then begin
    FMouseOverButton:=true;
    if FFlat and (FState<>bsUp) then begin
      FState:=bsUp;
      Invalidate;
    end;
  end;
  if assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TFileButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverButton:=false;
  FMouseDown:=false;
  if Enabled then begin
    if FFlat and (FState<>bsFlat) then begin
      FState:=bsFlat;
      Invalidate;
    end;
    if not FFlat and (FState<>bsUp) then begin
      FState:=bsUp;
      Invalidate;
    end;
  end;
  if assigned(FOnMouseExit) then
    FOnMouseExit(Self);
end;

procedure TFileButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    inherited MouseDown(Button, Shift, X, Y);
    { Im gedrückten Zustand neu zeichnen }
    FState:=bsDown;
    FMouseDown:=true;
    Invalidate;
  end;
end;

procedure TFileButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Enabled then begin
    inherited MouseUp(Button, Shift, X, Y);
    if FMouseDown and FMouseOverButton then begin
      { Im ungedrückten Zustand neu zeichnen }
      FState:=bsUp;
      FMouseDown:=false;
      Invalidate;
      if Assigned(FOnClick) then
         FOnClick(Self);
    end;
  end;
end;

procedure TFileButton.Paint;
begin
  if FOldWidth<>Width then begin
    FOldWidth:=Width;
    if FShowFilename then
      SetDisplayName;
    if Width<36 then
      Width:=36;
  end;
  if FOldHeight<>Height then begin
    FOldHeight:=Height;
    if Height<36 then
      Height:=36;
  end;

  Canvas.Font.Assign(Font);
  with Canvas do begin
    Brush.Style:=bsSolid;
    Pen.Width:=1;
  end;

  { Den Rest zeichnen }
  PaintButton;
end;

procedure TFileButton.PaintBevel(ARect:TRect);
var i : integer;
begin
  with Canvas do begin
    if not FFlat then begin
      { Rahmen }
      { links und oben }
      if FState=bsUp then
        Pen.Color:=FColor
      else
        Pen.Color:=clBlack;
      MoveTo(ARect.Right-1, ARect.Top);
      LineTo(ARect.Left, ARect.Top);
      LineTo(ARect.Left, ARect.Bottom-1);
      { rechts und unten }
      if FState=bsUp then
        Pen.Color:=clBlack
      else
        Pen.Color:=FColor;
      MoveTo(ARect.Left, ARect.Bottom-1);
      LineTo(ARect.Right-1, ARect.Bottom-1);
      LineTo(ARect.Right-1, ARect.Top);
      InflateRect(ARect, -1, -1);
    end;

    { Bevel }
    { links und oben }
    if FState=bsUp then
      Pen.Color:=FColorHighlight
    else
      Pen.Color:=FColorShadow;
    for i:=1 to FBevelWidth do begin
      MoveTo(ARect.Right-i, ARect.Top+i-1);
      LineTo(ARect.Left+i-1, ARect.Top+i-1);
      LineTo(ARect.Left+i-1, ARect.Bottom-i);
    end;
    { rechts und unten }
    if FState=bsUp then
      Pen.Color:=FColorShadow
    else
      Pen.Color:=FColorHighlight;
    for i:=1 to FBevelWidth do begin
      MoveTo(ARect.Left+i-1, ARect.Bottom-i);
      LineTo(ARect.Right-i, ARect.Bottom-i);
      LineTo(ARect.Right-i, ARect.Top+i-1);
    end;
  end;
end;

procedure TFileButton.PaintButton;
var CRect,
    Dest    : TRect;
    AWidth,
    AHeight,
    ALeft,
    ATop    : integer;
    OutText : string;
begin
  CRect:=GetClientRect;
  if FState<>bsFlat then
    PaintBevel(CRect);
  InflateRect(CRect, -FBevelWidth, -FBevelWidth);
  if not FFlat then
    InflateRect(CRect, -1, -1);
  if FShowFilename then
    OutText:=FDisplayName
  else
    OutText:=FCaption;
  AWidth:=CRect.Right-CRect.Left;
  AHeight:=CRect.Bottom-CRect.Top;
  if OutText<>'' then begin
    AHeight:=AHeight-Canvas.TextHeight(OutText);
    if FSpacing>0 then
      AHeight:=AHeight-FSpacing;
  end;

  with Canvas do begin
    if not FAsynchronous and (FState=bsDown) then
      Brush.Color:=FColorActive
    else
      Brush.Color:=FColor;
    FillRect(CRect);
    if Assigned(FIcon) and (FIcon.Handle>0) then begin
      ALeft:=CRect.Left+(AWidth-34) div 2;
      if FMargin>=0 then
        ATop:=CRect.Top+FMargin+1
      else
        ATop:=CRect.Top+((AHeight-34) div 2);
      Dest:=Rect(ALeft, ATop, ALeft+34, ATop+34);
      if FState=bsDown then
        { Glyph um 1 Pixel nach rechts unten verschieben }
        OffsetRect(Dest, 1, 1);

      { Icon zeichnen }
      if ShowIconFrame then begin
        Pen.Color:=clBlack;
        Rectangle(Dest.Left, Dest.Top, Dest.Right, Dest.Bottom);
      end;
      InflateRect(Dest, -1, -1);
      Brush.Color:=FBackgroundColor;
      FillRect(Dest);
      if FTransparent then
        BrushCopy(Dest, FBitmap, Rect(0, 0, FBitmap.Width-1, FBitmap.Height-1), FTransparentColor)
      else
        CopyRect(Dest, FBitmap.Canvas, Rect(0, 0, FBitmap.Width-1, FBitmap.Height-1));
    end
    else begin
      Dest:=CRect;
      Dest.Bottom:=Dest.Top;
    end;

    { Caption zeichnen }
    if OutText<>'' then begin
      Brush.Color:=FColor;
      ALeft:=CRect.Left+(AWidth-TextWidth(OutText)) div 2;
      if FSpacing>=0 then
        ATop:=Dest.Bottom+FSpacing+1
      else
        ATop:=Dest.Bottom+((CRect.Bottom-Dest.Bottom)-TextHeight(OutText)) div 2;
      if FState=bsDown then begin
        { Text um 1 Pixel nach rechts unten verschieben }
        inc(ALeft);
        inc(ATop);
      end;
      {$IFDEF SR_Delphi2_Up}
      if not Enabled then
        Font.Color:=clGrayText
      else
        if not FAsynchronous and (FState=bsDown) and (GetLuminance(FColorActive)<0.5) then
          Font.Color:=clWhite;
      {$ELSE}
      if not Enabled then
        Font.Color:=clGrayText;
      {$ENDIF}
      TextOut(ALeft, ATop, OutText);
    end;
  end;
end;

procedure TFileButton.SetBackgroundColor(newValue: TColor);
begin
  if FBackgroundColor<>newValue then begin
    FBackgroundColor:=newValue;
    AssignFileIcon;
    if FTransparent then
      Invalidate;
  end;
end;

procedure TFileButton.SetBevelContrast(newValue: TContrast);
begin
  if FBevelContrast<>newValue then begin
    FBevelContrast:=newValue;
    AssignBevelColors;
    Invalidate;
  end;
end;

procedure TFileButton.SetBevelWidth(newValue: TBevelWidth);
begin
  if FBevelWidth<>newValue then begin
    FBevelWidth:=newValue;
    if FShowFilename then
      SetDisplayName;
    Invalidate;
  end;
end;

procedure TFileButton.SetCaption(newValue: string);
begin
  if FCaption<>newValue then begin
    FCaption:=newValue;
    Invalidate;
  end;
end;

procedure TFileButton.SetColor(newValue: TColor);
begin
  if FColor<>newValue then begin
    FColor:=newValue;
    AssignBevelColors;
    Invalidate;
  end;
end;

procedure TFileButton.SetDisplayName;
var AWidth : integer;
    FName,
    Temp   : string;
begin
  FName:=ExtractFileName(FFileName);
  FDisplayName:=FName;
  AWidth:=Width-(2*FBevelWidth)-4;
  if not FFlat then
    dec(AWidth, 2);
  while Canvas.TextWidth(FDisplayName)>=AWidth do
    FDisplayName:=copy(FDisplayName, 1, length(FDisplayName)-1);
  if (FDisplayName<>FName) and (FDisplayName<>'') then begin
    Temp:=copy(FDisplayName, 1, length(FDisplayName)-1);
    FName:=ExtractRawFileName(FFileName);
    if (FDisplayName<>FName) and (FCaption<>(FName+'.')) then
      FDisplayName:=Temp+'..'
    else
      FDisplayName:=FName;
  end;
end;

procedure TFileButton.SetFileName(newValue: string);
begin
  if FFileName<>newValue then begin
    FFileName:=newValue;
    SetDisplayName;
    if Hint='' then
      Hint:=FFileName;
    if assigned(FIcon) then
      AssignFileIcon;
    Invalidate;
  end;
end;

procedure TFileButton.SetFlat(newValue: boolean);
begin
  if FFlat<>newValue then begin
    FFlat:=newValue;
    if FFlat then
      FState:=0
    else
      FState:=1;
    Invalidate;
  end;
end;

procedure TFileButton.SetIcon(newValue: TIcon);
begin
  if assigned(newValue) then begin
    FIcon.Assign(newValue);
    if FTransparent then
      FBitmap.Canvas.Brush.Color:=FBackgroundColor
    else
      FBitmap.Canvas.Brush.Color:=FColor;
    FBitmap.Canvas.FillRect(Rect(0, 0, 31, 31));
    if FIcon.Handle>0 then begin
      FBitmap.Canvas.Draw(0, 0, FIcon);
      FTransparentColor:=FBitmap.Canvas.Pixels[0, 0];
    end;
  end
  else begin
    FIcon:=nil;
    FIcon.Handle:=0;
    FBitmap.Canvas.Brush.Color:=FColor;
    FBitmap.Canvas.FillRect(Rect(0, 0, 31, 31));
  end;
  Invalidate;
end;

procedure TFileButton.SetMargin(newValue: shortint);
begin
  if FMargin<>newValue then begin
    FMargin:=newValue;
    Invalidate;
  end;
end;

procedure TFileButton.SetShowFilename(newValue: boolean);
begin
  if FShowFilename<>newValue then begin
    FShowFilename:=newValue;
    Invalidate;
  end;
end;

procedure TFileButton.SetShowIconFrame(newValue: boolean);
begin
  if FShowIconFrame<>newValue then begin
    FShowIconFrame:=newValue;
    Invalidate;
  end;
end;

procedure TFileButton.SetSpacing(newValue: shortint);
begin
  if FSpacing<>newValue then begin
    FSpacing:=newValue;
    Invalidate;
  end;
end;

procedure TFileButton.SetTransparent(newValue: boolean);
begin
  if FTransparent<>newValue then begin
    FTransparent:=newValue;
    AssignFileIcon;
    Invalidate;
  end;
end;

procedure Register;
begin
  RegisterComponents('Simon', [TFileButton]);
end;

end.
