
(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Essentials Vol I
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1997-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ES.INC}

{$B-} {Complete Boolean Evaluation}
{$I+} {Input/Output-Checking}
{$P+} {Open Parameters}
{$T-} {Typed @ Operator}
{$W-} {Windows Stack Frame}
{$X+} {Extended Syntax}

{$IFNDEF Win32}
  {$G+} {286 Instructions}
  {$N+} {Numeric Coprocessor}
  {$C MOVEABLE,DEMANDLOAD,DISCARDABLE}
{$ENDIF}

unit EsLabel;
  {-label component}

interface

uses
  {$IFDEF Win32} Windows, {$ELSE} WinTypes, WinProcs, {$ENDIF}
  Classes, Controls, Graphics, Messages, StdCtrls, SysUtils,
  EsConst, EsData, EsUtil;

type
  {preset "looks"}
  TEsAppearance = (apNone, apCustom, apFlying, apRaised, apSunken, apShadow);
  {preset color schemes}
  TEsColorScheme = (csCustom, csText, csWindows, csEmbossed, csGold, csSteel);
  {options for varying the shadow/highlight for the label}
  TEsGraduateStyle = (gsNone, gsHorizontal, gsVertical);
  {directions for shading (highlights and shadows)}
  TEsShadeDirection = (sdNone, sdUp, sdUpRight, sdRight, sdDownRight, sdDown,
                     sdDownLeft, sdLeft, sdUpLeft);
  {options for varying the text of the label}
  TEsShadeStyle = (ssPlain, ssExtrude, ssGraduated);

  TEsDepth = 0..255;

const
  lblDefAppearance         = apRaised;
  lblDefAutoSize           = False;
  lblDefColorScheme        = csWindows;
  lblDefFontName           = 'Times New Roman';
  lblDefFontSize           = 20;
  lblDefGraduateFromColor  = clGray;
  lblDefGraduateStyle      = gsNone;
  lblDefHighlightColor     = clWhite;
  lblDefHighlightDepth     = 1;
  lblDefHighlightDirection = sdUpLeft;
  lblDefHighlightStyle     = ssPlain;
  lblDefShadowColor        = clBlack;
  lblDefShadowDepth        = 1;
  lblDefShadowDirection    = sdDownRight;
  lblDefShadowStyle        = ssPlain;
  lblDefTransparent        = True;
  lblDefWordWrap           = True;

type
  TEsCustomSettings = class(TPersistent)
  private
    {.Z+}
    {property variables}
    FGraduateFromColor  : TColor;
    FGraduateStyle      : TEsGraduateStyle;
    FHighlightColor     : TColor;
    FHighlightDepth     : TEsDepth;
    FHighlightDirection : TEsShadeDirection;
    FHighlightStyle     : TEsShadeStyle;
    FShadowColor        : TColor;
    FShadowDepth        : TEsDepth;
    FShadowDirection    : TEsShadeDirection;
    FShadowStyle        : TEsShadeStyle;

    {event variables}
    FOnColorChange      : TNotifyEvent;
    FOnStyleChange      : TNotifyEvent;

    {internal variables}
    FUpdating           : Boolean;

    {internal methods}
    procedure DoOnColorChange;
    procedure DoOnStyleChange;

    {property methods}
    procedure SetGraduateFromColor(Value : TColor);
    procedure SetGraduateStyle(Value : TEsGraduateStyle);
    procedure SetHighlightColor(Value : TColor);
    procedure SetHighlightDepth(Value : TEsDepth);
    procedure SetHighlightDirection(Value : TEsShadeDirection);
    procedure SetHighlightStyle(Value : TEsShadeStyle);
    procedure SetShadowColor(Value : TColor);
    procedure SetShadowDepth(Value : TEsDepth);
    procedure SetShadowDirection(Value : TEsShadeDirection);
    procedure SetShadowStyle(Value : TEsShadeStyle);
    {.Z-}
  public
    procedure Assign(Source : TPersistent);
      override;

    procedure BeginUpdate;
    procedure EndUpdate;

    {.Z+}
    property OnColorChange : TNotifyEvent
      read FOnColorChange
      write FOnColorChange;

    property OnStyleChange : TNotifyEvent
      read FOnStyleChange
      write FOnStyleChange;
    {.Z-}

  published
    property GraduateFromColor : TColor
      read FGraduateFromColor
      write SetGraduateFromColor
      default lblDefGraduateFromColor;

    property GraduateStyle : TEsGraduateStyle
      read FGraduateStyle
      write SetGraduateStyle
      default lblDefGraduateStyle;

    property HighlightColor : TColor
      read FHighlightColor
      write SetHighlightColor
      default lblDefHighlightColor;

    property HighlightDepth : TEsDepth
      read FHighlightDepth
      write SetHighlightDepth
      default lblDefHighlightDepth;

    property HighlightDirection : TEsShadeDirection
      read FHighlightDirection
      write SetHighlightDirection
      default lblDefHighlightDirection;

    property HighlightStyle : TEsShadeStyle
      read FHighlightStyle
      write SetHighlightStyle
      default lblDefHighlightStyle;

    property ShadowColor : TColor
      read FShadowColor
      write SetShadowColor
      default lblDefShadowColor;

    property ShadowDepth : TEsDepth
      read FShadowDepth
      write SetShadowDepth
      default lblDefShadowDepth;

    property ShadowDirection : TEsShadeDirection
      read FShadowDirection
      write SetShadowDirection
      default lblDefShadowDirection;

    property ShadowStyle : TEsShadeStyle
      read FShadowStyle
      write SetShadowStyle
      default lblDefShadowStyle;
  end;

  TEsCustomLabel = class(TCustomLabel)
  protected {private}
    {.Z+}
    {property variables}
    FAppearance         : TEsAppearance;
    FColorScheme        : TEsColorScheme;
    FCustomSettings     : TEsCustomSettings;

    {interal variables}
    eslSchemes          : array [TEsColorScheme, (cpHighlight, cpShadow, cpFace)] of TColor;
    SettingColorScheme  : Boolean;
    SettingAppearance   : Boolean;

    {property methods}
    function GetVersion : string;
    function GetWordWrap : Boolean;
    procedure SetAppearance(Value : TEsAppearance);
    procedure SetColorScheme(Value : TEsColorScheme);
    procedure SetWordWrap(Value : Boolean);
    procedure SetVersion(const Value : string);

    {internal methods}
    procedure PaintPrim(CR : TRect; Flags : Word);
    procedure ColorChanged(Sender : TObject);
    procedure StyleChanged(Sender : TObject);
    {.Z-}

  protected
    {.Z+}
    procedure Paint;
      override;
    {.Z-}

    {protected properties} {can be published by descendants}
    property Appearance : TEsAppearance
      read FAppearance
      write SetAppearance
      default lblDefAppearance;

    property ColorScheme : TEsColorScheme
      read FColorScheme
      write SetColorScheme
      default lblDefColorScheme;

    property CustomSettings : TEsCustomSettings
      read FCustomSettings
      write FCustomSettings;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

    property WordWrap : Boolean
      read GetWordWrap
      write SetWordWrap
      default lblDefWordWrap;

  public
    {.Z+}
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;

    procedure PaintTo(DC : TEshDC; CR : TRect; Flags : Word);

    property AutoSize;
    {.Z-}
  end;

  TEsLabel = class(TEsCustomLabel)
  published
    {$IFDEF VERSION4}                                                {!!.06}
    property Anchors;                                                {!!.06}
    property Constraints;                                            {!!.06}
    property DragKind;                                               {!!.06}
    {$ENDIF}                                                         {!!.06}
   {properties}
    property Align;
    property Alignment;
    property Appearance;
    property Caption;
    property Color;
    property ColorScheme;
    property Cursor;
    property CustomSettings;
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
    property Transparent
      default lblDefTransparent;
    property Version;
    property Visible;
    property WordWrap;

    {events}
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


implementation


{*** TEsCustomSettings ***}

procedure TEsCustomSettings.Assign(Source : TPersistent);
var
  LS : TEsCustomSettings absolute Source;
begin
  if Assigned(Source) and (Source is TEsCustomSettings) then begin
    FGraduateFromColor := LS.GraduateFromColor;
    FGraduateStyle := LS.GraduateStyle;
    FHighlightColor := LS.HighlightColor;
    FHighlightDepth := LS.HighlightDepth;
    FHighlightDirection := LS.HighlightDirection;
    FHighlightStyle := LS.HighlightStyle;
    FShadowColor := LS.ShadowColor;
    FShadowDepth := LS.ShadowDepth;
    FShadowDirection := LS.ShadowDirection;
    FShadowStyle := LS.ShadowStyle;
  end else
    inherited Assign(Source);
end;

procedure TEsCustomSettings.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TEsCustomSettings.EndUpdate;
begin
  FUpdating := False;
  DoOnColorChange;
  DoOnStyleChange;
end;

procedure TEsCustomSettings.DoOnColorChange;
begin
  if not FUpdating and Assigned(FOnColorChange) then
    FOnColorChange(Self);
end;

procedure TEsCustomSettings.DoOnStyleChange;
begin
  if not FUpdating and Assigned(FOnStyleChange) then
    FOnStyleChange(Self);
end;

procedure TEsCustomSettings.SetGraduateFromColor(Value : TColor);
begin
  if Value <> FGraduateFromColor then begin
    FGraduateFromColor := Value;
    DoOnColorChange;
  end;
end;

procedure TEsCustomSettings.SetGraduateStyle(Value : TEsGraduateStyle);
begin
  if Value <> FGraduateStyle then begin
    FGraduateStyle := Value;
    DoOnStyleChange;
  end;
end;

procedure TEsCustomSettings.SetHighlightColor(Value : TColor);
begin
  if Value <> FHighlightColor then begin
    FHighlightColor := Value;
    DoOnColorChange;
  end;
end;

procedure TEsCustomSettings.SetHighlightDepth(Value : TEsDepth);
begin
  if Value <> FHighlightDepth then begin
    FHighlightDepth := Value;
    DoOnStyleChange;
  end;
end;

procedure TEsCustomSettings.SetHighlightDirection(Value : TEsShadeDirection);
begin
  if Value <> FHighlightDirection then begin
    FHighlightDirection := Value;
    DoOnStyleChange;
  end;
end;

procedure TEsCustomSettings.SetHighlightStyle(Value : TEsShadeStyle);
begin
  if Value <> FHighlightStyle then begin
    FHighlightStyle := Value;
    DoOnStyleChange;
  end;
end;

procedure TEsCustomSettings.SetShadowColor(Value : TColor);
begin
  if Value <> FShadowColor then begin
    FShadowColor := Value;
    DoOnColorChange;
  end;
end;

procedure TEsCustomSettings.SetShadowDepth(Value : TEsDepth);
begin
  if Value <> FShadowDepth then begin
    FShadowDepth := Value;
    DoOnStyleChange;
  end;
end;

procedure TEsCustomSettings.SetShadowDirection(Value : TEsShadeDirection);
begin
  if Value <> FShadowDirection then begin
    FShadowDirection := Value;
    DoOnStyleChange;
  end;
end;

procedure TEsCustomSettings.SetShadowStyle(Value : TEsShadeStyle);
begin
  if Value <> FShadowStyle then begin
    FShadowStyle := Value;
    DoOnStyleChange;
  end;
end;


{*** TEsCustomLabel ***}

constructor TEsCustomLabel.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  eslSchemes[csWindows, cpHighlight] := lblDefHighlightColor;
  eslSchemes[csWindows, cpFace] := clGray;
  eslSchemes[csWindows, cpShadow] := lblDefShadowColor;

  eslSchemes[csText, cpHighlight] := clWhite;
  eslSchemes[csText, cpFace] := clBlack;
  eslSchemes[csText, cpShadow] := clGray;

  eslSchemes[csEmbossed, cpHighlight] := clWhite;
  eslSchemes[csEmbossed, cpFace] := clSilver;
  eslSchemes[csEmbossed, cpShadow] := clBlack;

  eslSchemes[csGold, cpHighlight] := clYellow;
  eslSchemes[csGold, cpFace] := clOlive;
  eslSchemes[csGold, cpShadow] := clBlack;

  eslSchemes[csSteel, cpHighlight] := clAqua;
  eslSchemes[csSteel, cpFace] := clTeal;
  eslSchemes[csSteel, cpShadow] := clNavy;

  eslSchemes[csCustom, cpHighlight] := eslSchemes[csWindows,cpHighlight];
  eslSchemes[csCustom, cpFace] := eslSchemes[csWindows,cpFace];
  eslSchemes[csCustom, cpShadow] := eslSchemes[csWindows,cpShadow];

  {initialize defaults}
  FAppearance                         := lblDefAppearance;
  FColorScheme                        := lblDefColorScheme;
  FCustomSettings                     := TEsCustomSettings.Create;
  FCustomSettings.FGraduateFromColor  := lblDefGraduateFromColor;
  FCustomSettings.FGraduateStyle      := lblDefGraduateStyle;
  FCustomSettings.FHighlightColor     := eslSchemes[csWindows, cpHighlight];
  FCustomSettings.FHighlightDepth     := lblDefHighlightDepth;
  FCustomSettings.FHighlightDirection := lblDefHighlightDirection;
  FCustomSettings.FHighlightStyle     := lblDefHighlightStyle;
  FCustomSettings.FShadowColor        := eslSchemes[csWindows, cpShadow];
  FCustomSettings.FShadowDepth        := lblDefShadowDepth;
  FCustomSettings.FShadowDirection    := lblDefShadowDirection;
  FCustomSettings.FShadowStyle        := lblDefShadowStyle;
  FCustomSettings.OnColorChange       := ColorChanged;
  FCustomSettings.OnStyleChange       := StyleChanged;

  AutoSize            := lblDefAutoSize;
  Height              := 35;
  Width               := 150;
  Transparent         := lblDefTransparent;
  Font.Name           := lblDefFontName;
  Font.Size           := lblDefFontSize;
  Font.Color          := eslSchemes[FColorScheme, cpFace];
  WordWrap            := lblDefWordWrap;

  SettingColorScheme  := False;
  SettingAppearance   := False;

end;

destructor TEsCustomLabel.Destroy;
begin
  FCustomSettings.Free;
  FCustomSettings := nil;

  inherited Destroy;
end;

function TEsCustomLabel.GetVersion : string;
begin
  Result := EsVersionStr;
end;

function TEsCustomLabel.GetWordWrap : Boolean;
begin
  Result := inherited WordWrap;
end;

procedure TEsCustomLabel.Paint;
const
  Alignments : array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  Wrap : array[Boolean] of Word = (0, DT_WORDBREAK);
  Prefix : array[Boolean] of Word = (DT_NOPREFIX, 0);
begin
  PaintPrim(ClientRect, Wrap[WordWrap] or DT_EXPANDTABS or
                      Alignments[Alignment] or Prefix[ShowAccelChar]);
end;

procedure TEsCustomLabel.PaintPrim(CR : TRect; Flags : Word);
const
  DrawingOffset : array [TEsShadeDirection, (ioX, ioY)] of -1..1 =
      ((0,0),(0,-1),(+1,-1),(+1,0),(+1,+1),(0,+1),(-1,+1),(-1,0),(-1,-1));
  BandCount = 16;
var
  I          : Integer;
  MinOffset  : Integer;
  MaxOffset  : Integer;
  IX, IY     : Integer;
  IU, IV     : Integer;
  Limit      : Integer;
  Adjustment : Integer;
  AdjustR    : Double;
  AdjustG    : Double;
  AdjustB    : Double;
  Step       : Double;
  RctTemp    : TRect;
  FromR      : Byte;
  FromG      : Byte;
  FromB      : Byte;
  ToR        : Byte;
  ToG        : Byte;
  ToB        : Byte;
  BmpTemp    : TBitmap;
  BmpWork    : TBitmap;
  CnvWork    : TCanvas;
  Buf        : PChar;                                                  {!!.03}
begin
  {get offsets based on shadow and highlight directions and depths}
  MinOffset := Min(Min(Min(Min(DrawingOffset[FCustomSettings.HighlightDirection, ioX] * FCustomSettings.HighlightDepth,
               DrawingOffset[FCustomSettings.ShadowDirection, ioX] * FCustomSettings.ShadowDepth),
               DrawingOffset[FCustomSettings.HighlightDirection, ioY] * FCustomSettings.HighlightDepth),
               DrawingOffset[FCustomSettings.ShadowDirection, ioY] * FCustomSettings.ShadowDepth), 0);
  MaxOffset := Max(Max(Max(Max(DrawingOffset[FCustomSettings.HighlightDirection, ioX] * FCustomSettings.HighlightDepth,
               DrawingOffset[FCustomSettings.ShadowDirection, ioX] * FCustomSettings.ShadowDepth),
               DrawingOffset[FCustomSettings.HighlightDirection, ioY] * FCustomSettings.HighlightDepth),
               DrawingOffset[FCustomSettings.ShadowDirection, ioY] * FCustomSettings.ShadowDepth), 0);

  if Flags and DT_CENTER <> 0 then
    Adjustment := (MaxOffset - MinOffset) div 2
  else if Flags and DT_RIGHT <> 0 then
    Adjustment := MaxOffset - MinOffset
  else
    Adjustment := 0;

  {create temporary drawing surfaces}
  BmpTemp := TBitmap.Create;
  BmpWork := TBitmap.Create;
  try
    BmpTemp.Height := CR.Bottom-CR.Top;
    BmpTemp.Width := CR.Right-CR.Left;
    BmpTemp.Canvas.Font := Self.Font;

    BmpWork.Height := CR.Bottom-CR.Top;
    BmpWork.Width := CR.Right-CR.Left;
    BmpWork.Canvas.Font := Self.Font;

    {get copy of our canvas}
    BmpWork.Canvas.CopyRect(CR, Self.Canvas, CR);

    {set starting point for text - IX, IY}
    IX := 0; IY := 0;
    if not Transparent then begin
      BmpWork.Canvas.Brush.Color := Self.Color;
      BmpWork.Canvas.Brush.Style := bsSolid;
      BmpWork.Canvas.FillRect(CR);
    end;
    BmpWork.Canvas.Brush.Style := bsClear;

    Buf := StrAlloc(GetTextLen+1);                                     {!!.03}
    try                                                                {!!.03}
      {get label's caption}                                            {!!.03}
      GetTextBuf(Buf, GetTextLen+1);                                   {!!.03}

      {prepare for extruding shadow, if requested}
      GetRGB(FCustomSettings.ShadowColor, FromR, FromG, FromB);
      AdjustR := 0;
      AdjustG := 0;
      AdjustB := 0;
      Limit := FCustomSettings.ShadowDepth;
      if (FCustomSettings.ShadowStyle <> ssPlain) and (FCustomSettings.ShadowDepth > 1) then begin
        Limit := 1;
        {find changes in RGB colors}
        if FCustomSettings.ShadowStyle = ssGraduated then begin
          GetRGB(Font.Color, ToR, ToG, ToB);
          AdjustR := (ToR - FromR) / (FCustomSettings.ShadowDepth - 1);
          AdjustG := (ToG - FromG) / (FCustomSettings.ShadowDepth - 1);
          AdjustB := (ToB - FromB) / (FCustomSettings.ShadowDepth - 1);
        end;
      end;
      CnvWork := BmpWork.Canvas;

      {process for each copy of the shadow}
      for I := FCustomSettings.ShadowDepth downto Limit do begin
        CnvWork.Font.Color :=
          RGB(FromR + Round(AdjustR * (FCustomSettings.ShadowDepth - I)),
              FromG + Round(AdjustG * (FCustomSettings.ShadowDepth - I)),
              FromB + Round(AdjustB * (FCustomSettings.ShadowDepth - I)));
        {create a rect that is offset for the shadow}
        RctTemp:= Rect(
          CR.Left - MinOffset -Adjustment + DrawingOffset[FCustomSettings.ShadowDirection, ioX] * I,
          CR.Top - MinOffset + DrawingOffset[FCustomSettings.ShadowDirection, ioY] * I,
          CR.Right - MinOffset - Adjustment + DrawingOffset[FCustomSettings.ShadowDirection, ioX] * I,
          CR.Bottom - MinOffset + DrawingOffset[FCustomSettings.ShadowDirection, ioY] * I);
        {draw shadow text with alignment}
        DrawText(CnvWork.Handle, Buf, StrLen(Buf), RctTemp, Flags);
      end;

      {prepare for extruding highlight, if requested}
      GetRGB(FCustomSettings.HighlightColor, FromR, FromG, FromB);
      AdjustR := 0;
      AdjustG := 0;
      AdjustB := 0;
      Limit := FCustomSettings.HighlightDepth;
      if (FCustomSettings.HighlightStyle <> ssPlain) and (FCustomSettings.HighlightDepth > 1) then begin
        Limit := 1;
        if FCustomSettings.HighlightStyle = ssGraduated then begin {find changes in RGB Colors}
          GetRGB(Font.Color, ToR, ToG, ToB);
          AdjustR := (ToR - FromR) / (FCustomSettings.HighlightDepth - 1);
          AdjustG := (ToG - FromG) / (FCustomSettings.HighlightDepth - 1);
          AdjustB := (ToB - FromB) / (FCustomSettings.HighlightDepth - 1);
        end;
      end;

      CnvWork := BmpWork.Canvas;

      {process for each copy of the highlight}
      for I := FCustomSettings.HighlightDepth downto Limit do begin
        CnvWork.Font.Color :=
          RGB(FromR + Round(AdjustR * (FCustomSettings.HighlightDepth - I)),
              FromG + Round(AdjustG * (FCustomSettings.HighlightDepth - I)),
              FromB + Round(AdjustB * (FCustomSettings.HighlightDepth - I)));
        {create a rect that is offset for the highlight}
        RctTemp:= Rect(
          CR.Left - MinOffset - Adjustment + DrawingOffset[FCustomSettings.HighlightDirection, ioX] * I,
          CR.Top - MinOffset + DrawingOffset[FCustomSettings.HighlightDirection, ioY] * I,
          CR.Right - MinOffset - Adjustment + DrawingOffset[FCustomSettings.HighlightDirection, ioX] * I,
          CR.Bottom - MinOffset + DrawingOffset[FCustomSettings.HighlightDirection, ioY] * I);
        {draw highlight text with alignment}
        DrawText(CnvWork.Handle, Buf, StrLen(Buf), RctTemp, Flags);
      end;

      if FCustomSettings.GraduateStyle <> gsNone then begin
        {copy original canvas to work area}
        BmpTemp.Canvas.CopyRect(CR, BmpWork.Canvas, CR);
        {choose an unusual color}
        BmpTemp.Canvas.Font.Color := $00FE09F1;
        BmpTemp.Canvas.Brush.Style := bsClear;
        CnvWork := BmpTemp.Canvas;
      end else begin
        BmpWork.Canvas.Font.Color := Font.Color;  {restore original font Color}
        CnvWork := BmpWork.Canvas;
      end;

      {create a rect that is offset for the original text}
      RctTemp:= Rect(CR.Left - MinOffset - Adjustment,
                     CR.Top - MinOffset,
                     CR.Right - MinOffset - Adjustment,
                     CR.Bottom - MinOffset);

      {draw original text with alignment}
      DrawText(CnvWork.Handle, Buf, StrLen(Buf), RctTemp, Flags);
    finally                                                            {!!.03}
      StrDispose(Buf);                                                 {!!.03}
    end;                                                               {!!.03}

    if FCustomSettings.GraduateStyle <> gsNone then begin
      {transfer graduations from temporary canvas}
      {calculate start point and extent}
      Limit := BmpWork.Canvas.TextWidth(Caption);
      IV := IY - MinOffset;

      if Flags and DT_CENTER <> 0 then
        IU := (CR.Right-CR.Left - Limit) div 2 - MinOffset - Adjustment
      else if Flags and DT_RIGHT <> 0 then
        IU := CR.Bottom-CR.Top - MaxOffset - Limit
      else
        IU := IX - MinOffset;

      if FCustomSettings.GraduateStyle = gsVertical then
        Limit := CR.Bottom-CR.Top-1                                    {!!.04}
      else
        Dec(Limit);

      {calculate change in color at each step}
      GetRGB(FCustomSettings.GraduateFromColor, FromR, FromG, FromB);
      GetRGB(Font.Color, ToR, ToG, ToB);
      AdjustR := (ToR - FromR) / Pred(BandCount);
      AdjustG := (ToG - FromG) / Pred(BandCount);
      AdjustB := (ToB - FromB) / Pred(BandCount);

      Step := Limit / Pred(BandCount);

      {and draw it onto the canvas}
      BmpWork.Canvas.Brush.Style := bsSolid;
      for I := 0 to Pred(BandCount) do begin
        BmpWork.Canvas.Brush.Color := RGB(FromR + Round(AdjustR * I),
                                          FromG + Round(AdjustG * I),
                                          FromB + Round(AdjustB * I));
        if FCustomSettings.GraduateStyle = gsVertical then
          RctTemp := Rect(0, IV + Round(I*Step), CR.Right-CR.Left, IV + Round((I+1)*Step))
        else
          RctTemp := Rect(IU + Round(I*Step), 0, IU + Round((I+1)*Step), CR.Bottom-CR.Top);
        BmpWork.Canvas.BrushCopy(RctTemp, BmpTemp, RctTemp, BmpTemp.Canvas.Font.Color);
      end;
    end;

    Canvas.CopyRect(CR, BmpWork.Canvas, CR);
  finally
    BmpTemp.Free;
    BmpWork.Free;
  end;
end;

procedure TEsCustomLabel.PaintTo(DC : TEshDC; CR : TRect; Flags : Word);
begin
  Canvas.Handle := DC;
  try
    if not Transparent then begin
      Canvas.Brush.Color := Self.Color;
      Canvas.Brush.Style := bsSolid;
      {clear complete client area}
      Canvas.FillRect(Rect(0, 0, CR.Right, CR.Bottom));
    end;
    Canvas.Brush.Style := bsClear;
    PaintPrim(CR, Flags)
  finally
    Canvas.Handle := 0;
  end;
end;

procedure TEsCustomLabel.SetAppearance(Value : TEsAppearance);
begin
  if FAppearance <> Value then begin
    SettingAppearance := True;
    try
      FAppearance := Value;
      FCustomSettings.BeginUpdate;
      try
        FCustomSettings.HighlightColor := eslSchemes[ColorScheme,cpHighlight];
        case FAppearance of
          apRaised:
            begin
              FCustomSettings.HighlightDirection := sdUpLeft;
              FCustomSettings.ShadowDirection := sdDownRight;
              FCustomSettings.HighlightDepth := 1;
              FCustomSettings.ShadowDepth := 1;
            end;
          apSunken:
            begin
              FCustomSettings.HighlightDirection := sdDownRight;
              FCustomSettings.ShadowDirection := sdUpLeft;
              FCustomSettings.HighlightDepth := 1;
              FCustomSettings.ShadowDepth := 1;
            end;
          apShadow:
            begin
              FCustomSettings.HighlightDirection := sdNone;
              FCustomSettings.ShadowDirection := sdDownRight;
              FCustomSettings.HighlightDepth := 0;
              FCustomSettings.ShadowDepth := 2;
            end;
          apFlying:
            begin
              FCustomSettings.HighlightDirection := sdDownRight;
              FCustomSettings.ShadowDirection := sdDownRight;
              FCustomSettings.HighlightDepth :=1;
              FCustomSettings.ShadowDepth :=5;
              {flying has two shadows}
              FCustomSettings.HighlightColor := eslSchemes[ColorScheme, cpShadow];
            end;
          apNone:
            begin
              FCustomSettings.HighlightDirection := sdNone;
              FCustomSettings.ShadowDirection := sdNone;
              FCustomSettings.HighlightDepth :=0;
              FCustomSettings.ShadowDepth :=0;
            end;
        end;
      finally
        FCustomSettings.EndUpdate;
      end;
    finally
      SettingAppearance := False;
      Perform(CM_TEXTCHANGED, 0, 0);
    end;
  end;
end;

procedure TEsCustomLabel.SetColorScheme(Value : TEsColorScheme);
begin
  if FColorScheme <> Value then begin
    SettingColorScheme := True;
    try
      FColorScheme := Value;
      FCustomSettings.BeginUpdate;
      try
        FCustomSettings.HighlightColor := eslSchemes[FColorScheme, cpHighlight];
        Font.Color := eslSchemes[FColorScheme, cpFace];
        FCustomSettings.ShadowColor := eslSchemes[FColorScheme, cpShadow];
        if FColorScheme <> csCustom then begin
          eslSchemes[csCustom, cpHighlight] := eslSchemes[FColorScheme, cpHighlight];
          eslSchemes[csCustom, cpFace] := eslSchemes[FColorScheme, cpFace];
          eslSchemes[csCustom, cpShadow] := eslSchemes[FColorScheme, cpShadow];
        end;
      finally
        FCustomSettings.EndUpdate;
      end;
    finally
      SettingColorScheme := False;
      Perform(CM_TEXTCHANGED, 0, 0);
    end;
  end;
end;

procedure TEsCustomLabel.ColorChanged(Sender : TObject);
begin
  if csLoading in ComponentState then
    Exit;

  Invalidate;

  if not SettingColorScheme then
    FColorScheme := csCustom;

  if not SettingColorScheme then
    Perform(CM_COLORCHANGED, 0, 0);
end;

procedure TEsCustomLabel.StyleChanged(Sender : TObject);
begin
  if csLoading in ComponentState then
    Exit;

  Invalidate;

  if not SettingAppearance then begin
    FAppearance := apCustom;
    Perform(CM_TEXTCHANGED, 0, 0);
  end;
end;

procedure TEsCustomLabel.SetVersion(const Value : string);
begin
end;

procedure TEsCustomLabel.SetWordWrap(Value : Boolean);
begin
  if Value <> WordWrap then begin
    inherited WordWrap := Value;
    Invalidate;
  end;
end;

end.
