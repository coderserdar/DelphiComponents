(* *****************************************************************************
TFreeButton
-----------

TFreeButton draws a beveled, coloured and shadowed button onto the form. The text
has the option of a drop shadow. Text and glyphs can be placed in any of nine
positions. Glyphs can be transparent. Can also draw a light, which automatically
turns itself on or off when clicked, but when disabled does not alter in state.
The button 'pops out' when moused over (the drop shadow is deeper), and the entire
control is drawn highlighted.


» Version:  1.0
» Features: Shadow (Text & Button) | Glyphs | Custom Colouring | Toggle Light
» ToDo:     Add Shadow to Glyph
» License:  Free for non-commercial use. Any alterations or bugs, please contact
            me at TheB_6030@caramail.com .

***************************************************************************** *)


unit FreeButton;

interface

uses
  Controls, Classes, Messages, Windows, Graphics;

type
  TButtonState = (bsUp, bsDown, bsOver);
  TTextPosition = (tpTopLeft, tpMiddleLeft, tpBottomLeft, tpTopCenter, tpMiddleCenter,
                   tpMiddleBottom, tpTopRight, tpMiddleRight, tpBottomRight);
  TGlyphPosition = (gpTopLeft, gpMiddleLeft, gpBottomLeft, gpTopCenter, gpMiddleCenter,
                    gpMiddleBottom, gpTopRight, gpMiddleRight, gpBottomRight);
  TCustomColor = (ccLeather,ccOldGold,ccBuffedGold,ccBottleGreen,ccEmerald,ccGlass,
                  ccNight,ccGirder,ccBrushedSteel,ccTwilight,ccSapphire,ccSky,
                  ccRoyalPurple,ccShadows,ccPastels,ccRoyal,ccRuby,ccRose,
                  ccNone);
  TFreeButton = class(TGraphicControl)
  private
  // FVariables
     FDrawColor: TColor;
     FLightColor: TColor;
     FBackColor: TColor;
     FCaption: String;
     FFont: TFont;
     FTextPosition: TTextPosition;
     FGlyphPosition: TGlyphPosition;
     FGlyph: TBitmap;
     FTransparentGlyph: Boolean;
     FDrawDropShadow: Boolean;
     FDrawLight: Boolean;
     FAutoLightColor: Boolean;
     FLightSize: Integer;
     FCustomColor: TCustomColor;
  // setProcedures
     procedure setDrawColor(const Value: TColor);
     procedure setLightColor(const Value: TColor);
     procedure setBackColor(const Value: TColor);
     procedure setCaption(const Value: String);
     procedure setFont(const Value: TFont);
     procedure setTextPosition(const Value: TTextPosition);
     procedure setGlyphPosition(const Value: TGlyphPosition);
     procedure setGlyph(const Value: TBitmap);
     procedure setTransparentGlyph(const Value: Boolean);
     procedure setDrawDropShadow(const Value: Boolean);
     procedure setDrawLight(const Value: Boolean);
     procedure setAutoLightColor(const Value: Boolean);
     procedure setLightSize(const Value: Integer);
     procedure setCustomColor(const Value: TCustomColor);
  // Functions
     function GetNewColor(InputColor: TColor; Number: Integer; Additive: Boolean): TColor;
     function RoundNumber(n: Extended): integer;
     function GetColor(Color: TColor): TColor;
     function GetTextPosition(CanvasWidth,CanvasHeight:Integer): TPoint;
     function GetGlyphPosition(CanvasWidth,CanvasHeight:Integer): TPoint;
     function GetCustomColor: TColor;
  protected
     FButtonState: TButtonState;
  // Overridden
     procedure Paint; override;
     procedure CMMouseEnter(var msg: TMessage); message CM_MOUSEENTER;
     procedure CMMouseLeave(var msg: TMessage); message CM_MOUSELEAVE;
     procedure WMLMOUSEDOWN (var msg: TMessage); message WM_LBUTTONDOWN;
     procedure WMLMouseUP(var msg: TMessage); message WM_LBUTTONUP;
  public
  // vars
     LightOn: Boolean;
  // constuct
     constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
     property DrawColor: TColor read FDrawColor write setDrawColor default $00444444;
    property LightColor: TColor read FLightColor write setLightColor default $0088FF88;
    property BackColor: TColor read FBackColor write setBackColor default clBtnFace;
    property CustomColor: TCustomColor read FCustomColor write setCustomColor default ccNone;
     property Caption: string read FCaption write setCaption;
    property Font: TFont read FFont write setFont;
    property TextPosition: TTextPosition read FTextPosition write setTextPosition default tpMiddleCenter;
    property DrawDropShadow: Boolean read FDrawDropShadow write setDrawDropShadow default True;
     property Glyph: TBitmap read FGlyph write setGlyph;
    property GlyphPosition: TGlyphPosition read FGlyphPosition write setGlyphPosition default gpTopCenter;
    property TransparentGlyph: Boolean read FTransparentGlyph write setTransparentGlyph default True;
     property DrawLight: Boolean read FDrawLight write setDrawLight default True;
    property LightSize: Integer read FLightSize write setLightSize default 8;
    property AutoLightColor: Boolean read FAutoLightColor write setAutoLightColor default True;

    property Align;

    property onClick;
    property onDblClick;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TFreeButton]);
end;

constructor TFreeButton.Create(Aowner: TComponent);
begin
     inherited;
     ControlStyle := ControlStyle-[csOpaque,csAcceptsControls];
     Width := 83;
     Height := 31;
     DrawColor := GetColor($00444444);
     LightColor := GetColor($0088FF88);
     BackColor := GetColor(clBtnFace);
     Caption := '';
     FFont := TFont.Create;
     FFont.Color := GetColor(clWhite);
     FFont.Name := 'Verdana';
     FFont.Size := 10;
     FFont.Style := [];
     TextPosition := tpMiddleCenter;
     GlyphPosition := gpTopCenter;
     FGlyph := TBitmap.Create;
     TransparentGlyph := True;
     DrawDropShadow := True;
     DrawLight := True;
     LightSize := 8;
     CustomColor := ccNone;
     AutoLightColor := True;
end;

procedure TFreeButton.setDrawColor(const Value: TColor);
begin
     FDrawColor := GetColor(Value);
     Invalidate;
end;

 procedure TFreeButton.setLightColor(const Value: TColor);
begin
     FLightColor := GetColor(Value);
     Invalidate;
end;

procedure TFreeButton.setBackColor(const Value: TColor);
begin
     FBackColor := GetColor(Value);
     Invalidate;
end;

procedure TFreeButton.setCaption(const Value: String);
begin
     FCaption := Value;
     Invalidate;
end;

procedure TFreeButton.setFont(const Value: TFont);
begin
     FFont := Value;
     Invalidate;
end;

procedure TFreeButton.setTextPosition(const Value: TTextPosition);
begin
  FTextPosition := Value;
  Invalidate;
end;

procedure TFreeButton.setGlyphPosition(const Value: TGlyphPosition);
begin
  FGlyphPosition := Value;
  Invalidate;
end;

procedure TFreeButton.setGlyph(const Value: TBitmap);
begin
     FGlyph.Assign(Value);
     Invalidate;
end;

procedure TFreeButton.setTransparentGlyph(const Value: Boolean);
begin
     FTransparentGlyph := Value;
     Invalidate;
end;

procedure TFreeButton.setDrawDropShadow(const Value: Boolean);
begin
     FDrawDropShadow := Value;
     Invalidate;
end;

procedure TFreeButton.setDrawLight(const Value: Boolean);
begin
     FDrawLight := Value;
     Invalidate;
end;

procedure TFreeButton.setLightSize(const Value: Integer);
begin
     FLightSize := Value;
     Invalidate;
end;

procedure TFreeButton.setCustomColor(const Value: TCustomColor);
begin
     FCustomColor := Value;
     Invalidate;
end;

procedure TFreeButton.setAutoLightColor(const Value: Boolean);
begin
     FAutoLightColor := Value;
     Invalidate;
end;

procedure TFreeButton.Paint;
var ButtonMap,MainMap: TBitmap;
    TextPoint,GlyphPoint: TPoint;
    MainColor,LightDrawColor: TColor;
begin
IF TransparentGlyph = True then begin
  Glyph.TransparentColor := Glyph.Canvas.Pixels[0,0];
  Glyph.Transparent := True;
end else Glyph.Transparent := False;
MainMap := TBitmap.Create;
MainMap.Width := Width;
MainMap.Height := Height;
MainMap.Canvas.Brush.Color := BackColor;
MainMap.Canvas.FillRect(rect(0,0,Width,Height));
ButtonMap := TBitmap.Create;
ButtonMap.Width := Width-4;
ButtonMap.Height := Height-4;
ButtonMap.Canvas.Draw(0,0,MainMap);
TextPoint := GetTextPosition(ButtonMap.Width,ButtonMap.Height);
GlyphPoint := GetGlyphPosition(ButtonMap.Width,ButtonMap.Height);
IF CustomColor <> ccNone then MainColor := GetCustomColor
  else MainColor := GetColor(DrawColor);
IF AutoLightColor = True then LightDrawColor := GetNewColor(MainColor,20,True)
  else LightDrawColor := LightColor;
////////////////////////////////////////////////////////////////////////////////
Case FButtonState of
  bsUp: begin
    with ButtonMap.Canvas do begin
      Pen.Color := GetNewColor(MainColor,40,True);
      Brush.Color := MainColor;
      RoundRect(0,0,ButtonMap.Width,ButtonMap.Height,4,4);
      Pen.Color := GetNewColor(MainColor,40,False);
      RoundRect(1,1,ButtonMap.Width-1,ButtonMap.Height-1,4,4);
      Pen.Color := GetNewColor(MainColor,30,True);
      Pen.Width := 2;
      MoveTo(3,3);
      LineTo(ButtonMap.Width-3,3);
      MoveTo(3,3);
      LineTo(3,ButtonMap.Height-3);
      Pen.Color := GetNewColor(MainColor,30,False);
      Pen.Width := 1;
      MoveTo(ButtonMap.Width-3,ButtonMap.Height-3);
      LineTo(3,ButtonMap.Height-3);
      MoveTo(ButtonMap.Width-3,ButtonMap.Height-3);
      LineTo(ButtonMap.Width-3,3);
      Brush.Style := bsClear;
      ButtonMap.Canvas.Font.Assign(FFont);
      IF DrawDropShadow = True then begin
        ButtonMap.Canvas.Font.Color := GetNewColor(MainColor,40,False);
        TextOut(TextPoint.x+2,TextPoint.y+2,Caption);
        ButtonMap.Canvas.Font.Color := FFont.Color;
      end;
      TextOut(TextPoint.x,TextPoint.y,Caption);
      IF Glyph.Empty = False then Draw(GlyphPoint.x,GlyphPoint.y,Glyph);
      IF DrawLight = True then begin
        Case LightOn of
          True: begin
            Pen.Color := GetNewColor(LightDrawColor,80,False);
            Brush.Color := LightDrawColor;
          end;
          False: begin
            Pen.Color := GetNewColor(LightDrawColor,80,False);
            Brush.Color := GetNewColor(LightDrawColor,40,False);
          end;
        end;
        Polygon([Point(ButtonMap.Width-8,ButtonMap.Height-8),
                 Point(ButtonMap.Width-8,ButtonMap.Height-8-LightSize),
                 Point(ButtonMap.Width-8-LightSize,ButtonMap.Height-8)]);
      end;
    end;
    MainMap.Canvas.Draw(2,2,ButtonMap);
    with MainMap.Canvas do begin
      Pen.Color := GetNewColor(BackColor,100,False);
      MoveTo(Width-2,4);
      LineTo(Width-2,Height-2);
      LineTo(3,Height-2);
      Pen.Color := GetNewColor(BackColor,50,False);
      MoveTo(Width-1,5);
      LineTo(Width-1,Height-1);
      LineTo(4,Height-1);
      Pixels[Width-3,Height-3] := GetNewColor(BackColor,100,False);
      Pixels[Width-2,Height-2] := GetNewColor(BackColor,50,False);
      Pixels[Width-1,Height-1] := BackColor;
    end;
  end;
////////////////////////////////////////////////////////////////////////////////
  bsOver: begin
    with ButtonMap.Canvas do begin
      Pen.Color := GetNewColor(MainColor,55,True);
      Brush.Color := GetNewColor(MainColor,15,True);
      RoundRect(0,0,ButtonMap.Width,ButtonMap.Height,4,4);
      Pen.Color := GetNewColor(MainColor,5,True);
      RoundRect(1,1,ButtonMap.Width-1,ButtonMap.Height-1,4,4);
      Pen.Color := GetNewColor(MainColor,45,True);
      Pen.Width := 2;
      MoveTo(3,3);
      LineTo(ButtonMap.Width-3,3);
      MoveTo(3,3);
      LineTo(3,ButtonMap.Height-3);
      Pen.Color := GetNewColor(MainColor,5,False);
      Pen.Width := 1;
      MoveTo(ButtonMap.Width-3,ButtonMap.Height-3);
      LineTo(3,ButtonMap.Height-3);
      MoveTo(ButtonMap.Width-3,ButtonMap.Height-3);
      LineTo(ButtonMap.Width-3,3);
      Brush.Style := bsClear;
      ButtonMap.Canvas.Font.Assign(FFont);
      IF DrawDropShadow = True then begin
        ButtonMap.Canvas.Font.Color := GetNewColor(MainColor,25,False);
        TextOut(TextPoint.x+2,TextPoint.y+2,Caption);
        ButtonMap.Canvas.Font.Color := FFont.Color;
      end;
      TextOut(TextPoint.x,TextPoint.y,Caption);
      IF Glyph.Empty = False then Draw(GlyphPoint.x,GlyphPoint.y,Glyph);
      IF DrawLight = True then begin
        Case LightOn of
          True: begin
            Pen.Color := GetNewColor(LightDrawColor,60,False);
            Brush.Color := GetNewColor(LightDrawColor,20,True);
          end;
          False: begin
            Pen.Color := GetNewColor(LightDrawColor,60,False);
            Brush.Color := GetNewColor(LightDrawColor,20,False);
          end;
        end;
        Polygon([Point(ButtonMap.Width-8,ButtonMap.Height-8),
                 Point(ButtonMap.Width-8,ButtonMap.Height-8-LightSize),
                 Point(ButtonMap.Width-8-LightSize,ButtonMap.Height-8)]);
      end;
    end;
    MainMap.Canvas.Draw(0,0,ButtonMap);
    with MainMap.Canvas do begin
      Pen.Color := GetNewColor(BackColor,100,False);
      MoveTo(Width-4,4);
      LineTo(Width-4,Height-4);
      LineTo(3,Height-4);
      Pen.Color := GetNewColor(BackColor,75,False);
      MoveTo(Width-3,4);
      LineTo(Width-3,Height-3);
      LineTo(3,Height-3);
      Pen.Color := GetNewColor(BackColor,50,False);
      MoveTo(Width-2,5);
      LineTo(Width-2,Height-2);
      Lineto(4,Height-2);
      Pen.Color := GetNewColor(BackColor,25,False);
      MoveTo(Width-1,5);
      LineTo(Width-1,Height-1);
      LineTo(4,Height-1);
      Pixels[Width-5,Height-5] := GetNewColor(BackColor,100,False);
      Pixels[Width-4,Height-4] := GetNewColor(BackColor,75,False);
      Pixels[Width-3,Height-3] := GetNewColor(BackColor,50,False);
      Pixels[Width-2,Height-2] := GetNewColor(BackColor,25,False);
      Pixels[Width-1,Height-1] := BackColor;
    end;
  end;
////////////////////////////////////////////////////////////////////////////////
  bsDown: begin
    with ButtonMap.Canvas do begin
      Pen.Color := GetNewColor(MainColor,55,True);
      Brush.Color := GetNewColor(MainColor,15,True);
      RoundRect(0,0,ButtonMap.Width,ButtonMap.Height,4,4);
      Pen.Color := GetNewColor(MainColor,5,True);
      RoundRect(1,1,ButtonMap.Width-1,ButtonMap.Height-1,4,4);
      Pen.Color := GetNewColor(MainColor,5,False);
      Pen.Width := 2;
      MoveTo(3,3);
      LineTo(ButtonMap.Width-3,3);
      MoveTo(3,3);
      LineTo(3,ButtonMap.Height-3);
      Pen.Color := GetNewColor(MainColor,45,True);
      Pen.Width := 1;
      MoveTo(ButtonMap.Width-3,ButtonMap.Height-3);
      LineTo(3,ButtonMap.Height-3);
      MoveTo(ButtonMap.Width-3,ButtonMap.Height-3);
      LineTo(ButtonMap.Width-3,3);
      Brush.Style := bsClear;
      ButtonMap.Canvas.Font.Assign(FFont);
      IF DrawDropShadow = True then begin
        ButtonMap.Canvas.Font.Color := GetNewColor(MainColor,40,False);
        TextOut(TextPoint.x+2,TextPoint.y+2,Caption);
        ButtonMap.Canvas.Font.Color := FFont.Color;
      end;
      TextOut(TextPoint.x,TextPoint.y,Caption);
      IF Glyph.Empty = False then Draw(GlyphPoint.x,GlyphPoint.y,Glyph);
      IF DrawLight = True then begin
        Case LightOn of
          True: begin
            Pen.Color := GetNewColor(LightDrawColor,60,False);
            Brush.Color := GetNewColor(LightDrawColor,20,True);
          end;
          False: begin
            Pen.Color := GetNewColor(LightDrawColor,60,False);
            Brush.Color := GetNewColor(LightDrawColor,20,False);
          end;
        end;
        Polygon([Point(ButtonMap.Width-8,ButtonMap.Height-8),
                 Point(ButtonMap.Width-8,ButtonMap.Height-8-LightSize),
                 Point(ButtonMap.Width-8-LightSize,ButtonMap.Height-8)]);
      end;
    end;
    MainMap.Canvas.Pen.Color := BackColor;
    MainMap.Canvas.Brush.Color := BackColor;
    MainMap.Canvas.Rectangle(0,0,Width,Height);
    MainMap.Canvas.Draw(3,3,ButtonMap);
  end;
////////////////////////////////////////////////////////////////////////////////
end;
ButtonMap.Canvas.Brush.Style := bsSolid;
Canvas.Draw(0,0,MainMap);
ButtonMap.Free;
MainMap.Free;
end;

procedure TFreeButton.CMMouseEnter(var msg: TMessage);
begin
     inherited;
     FButtonState := bsOver;
     Repaint;
end;

procedure TFreeButton.CMMouseLeave(var msg: TMessage);
begin
     inherited;
     FButtonState := bsUp;
     Repaint;
end;

procedure TFreeButton.WMLMOUSEDOWN(var msg: TMessage);
begin
     inherited;
     FButtonState := bsDown;
     Repaint;
end;

procedure TFreeButton.WMLMOUSEUP(var msg: TMessage);
begin
     inherited;
     FButtonState := bsOver;
     IF DrawLight = True then
       IF LightOn = True then LightOn := False
       else LightOn := True;
     Repaint;
end;

function TFreeButton.GetNewColor(InputColor: TColor; Number: Integer; Additive: Boolean): TColor;
var r,g,b: Integer;
begin
     r := GetRValue(InputColor);
     g := GetGValue(InputColor);
     b := GetBValue(InputColor);
     if Additive = True then begin
       r := r+Number;
       g := g+Number;
       b := b+Number;
     end else begin
       r := r-Number;
       g := g-Number;
       b := b-Number;
     end;
     if r > 255 then r := 255;
     if r < 0 then r := 0;
     if g > 255 then g := 255;
     if g < 0 then g := 0;
     if b > 255 then b := 255;
     if b < 0 then b := 0;

     Result := rgb(byte(r),byte(g),byte(b));
end;

function TFreeButton.RoundNumber(n: Extended): integer;
begin
     Result := Round(Int(n));
end;


function TFreeButton.GetColor(Color: TColor): TColor;
var NewColor: TColor;
begin
  NewColor := Color;
  case Color of
    cl3DDkShadow: NewColor := GetSysColor(COLOR_3DDKSHADOW);
    clBtnFace: NewColor := GetSysColor(COLOR_BTNFACE);
    clBtnHighlight: NewColor := GetSysColor(COLOR_BTNHIGHLIGHT);
    cl3DLight: NewColor := GetSysColor(COLOR_3DLIGHT);
    clBtnShadow: NewColor := GetSysColor(COLOR_BTNSHADOW);
    clActiveBorder: NewColor := GetSysColor(COLOR_ACTIVEBORDER);
    clActiveCaption: NewColor := GetSysColor(COLOR_ACTIVECAPTION);
    clAppWorkspace: NewColor := GetSysColor(COLOR_APPWORKSPACE);
    clBackground: NewColor := GetSysColor(COLOR_BACKGROUND);
    clBtnText: NewColor := GetSysColor(COLOR_BTNTEXT);
    clCaptionText: NewColor := GetSysColor(COLOR_CAPTIONTEXT);
    clGrayText: NewColor := GetSysColor(COLOR_GRAYTEXT);
    clHighlight: NewColor := GetSysColor(COLOR_HIGHLIGHT);
    clHighlightText: NewColor := GetSysColor(COLOR_HIGHLIGHTTEXT);
    clInactiveBorder: NewColor := GetSysColor(COLOR_INACTIVEBORDER);
    clInactiveCaption: NewColor := GetSysColor(COLOR_INACTIVECAPTION);
    clInactiveCaptionText: NewColor := GetSysColor(COLOR_INACTIVECAPTIONTEXT);
    clInfoBk: NewColor := GetSysColor(COLOR_INFOBK);
    clInfoText: NewColor := GetSysColor(COLOR_INFOTEXT);
    clMenu: NewColor := GetSysColor(COLOR_MENU);
    clMenuText: NewColor := GetSysColor(COLOR_MENUTEXT);
    clScrollBar: NewColor := GetSysColor(COLOR_SCROLLBAR);
    clWindow: NewColor := GetSysColor(COLOR_WINDOW);
    clWindowFrame: NewColor := GetSysColor(COLOR_WINDOWFRAME);
    clWindowText: NewColor := GetSysColor(COLOR_WINDOWTEXT);
  end;
  Result := NewColor;
end;

function TFreeButton.GetTextPosition(CanvasWidth,CanvasHeight:Integer): TPoint;
begin
Canvas.Font.Assign(FFont);
Case TextPosition of
  tpTopLeft: begin
    Result.y := 4;
    Result.x := 4;
  end;
  tpMiddleLeft: begin
    Result.y := RoundNumber((CanvasHeight / 2)-(Canvas.TextHeight(Caption) / 2));
    Result.x := 4;
  end;
  tpBottomLeft: begin
    Result.y := CanvasHeight-Canvas.TextHeight(Caption)-8;
    Result.x := 4;
  end;
  tpTopCenter: begin
    Result.y := 4;
    Result.x := RoundNumber(((CanvasWidth-4) / 2)-(Canvas.TextWidth(Caption) / 2));
  end;
  tpMiddleCenter: begin
    Result.y := RoundNumber((CanvasHeight / 2)-(Canvas.TextHeight(Caption) / 2));
    Result.x := RoundNumber(((CanvasWidth-4) / 2)-(Canvas.TextWidth(Caption) / 2));
  end;
  tpMiddleBottom: begin
    Result.y := CanvasHeight-Canvas.TextHeight(Caption)-8;
    Result.x := RoundNumber(((CanvasWidth-4) / 2)-(Canvas.TextWidth(Caption) / 2));
  end;
  tpTopRight: begin
    Result.y := 4;
    Result.x := CanvasWidth-Canvas.TextWidth(Caption)-8;
  end;
  tpMiddleRight: begin
    Result.y := RoundNumber((CanvasHeight / 2)-(Canvas.TextHeight(Caption) / 2));
    Result.x := CanvasWidth-Canvas.TextWidth(Caption)-8;
  end;
  tpBottomRight: begin
    Result.y := CanvasHeight-Canvas.TextHeight(Caption)-8;
    Result.x := CanvasWidth-Canvas.TextWidth(Caption)-8;
  end;
end;

end;

function TFreeButton.GetGlyphPosition(CanvasWidth,CanvasHeight:Integer): TPoint;
begin
Case GlyphPosition of
  gpTopLeft: begin
    Result.y := 4;
    Result.x := 4;
  end;
  gpMiddleLeft: begin
    Result.y := RoundNumber((CanvasHeight / 2)-(Glyph.Height / 2));
    Result.x := 4;
  end;
  gpBottomLeft: begin
    Result.y := CanvasHeight-Glyph.Height-8;
    Result.x := 4;
  end;
  gpTopCenter: begin
    Result.y := 4;
    Result.x := RoundNumber((CanvasWidth / 2)-(Glyph.Width / 2));
  end;
  gpMiddleCenter: begin
    Result.y := RoundNumber((CanvasHeight / 2)-(Glyph.Height / 2));
    Result.x := RoundNumber((CanvasWidth / 2)-(Glyph.Width / 2));
  end;
  gpMiddleBottom: begin
    Result.y := CanvasHeight-Glyph.Height-8;
    Result.x := RoundNumber((CanvasWidth / 2)-(Glyph.Width / 2));
  end;
  gpTopRight: begin
    Result.y := 4;
    Result.x := CanvasWidth-Glyph.Width-8;
  end;
  gpMiddleRight: begin
    Result.y := RoundNumber((CanvasHeight / 2)-(Glyph.Height / 2));
    Result.x := CanvasWidth-Glyph.Width-8;
  end;
  gpBottomRight: begin
    Result.y := CanvasHeight-Glyph.Height-8;
    Result.x := CanvasWidth-Glyph.Width-8;
  end;
end;

end;

function TFreeButton.GetCustomColor: TColor;
begin
     Case CustomColor of
       ccLeather: Result := $002f454f;
       ccOldGold: Result := $00417787;
       ccBuffedGold: Result := $000090c8;
       ccBottleGreen: Result := $003c601e;
       ccEmerald: Result := $00609830;
       ccGlass: Result := $008ecc5b;
       ccNight: Result := $004f452f;
       ccGirder: Result := $006f6042;
       ccBrushedSteel: Result := $009d805e;
       ccTwilight: Result := $00602c1e;
       ccSapphire: Result := $00985030;
       ccSky: Result := $00cc7e5b;
       ccRoyalPurple: Result := $004f2f45;
       ccShadows: Result := $006f4260;
       ccPastels: Result := $009d5e89;
       ccRoyal: Result := $003c1e60;
       ccRuby: Result := $00603098;
       ccRose: Result := $008e5bcc;
     end;

end;

end.
