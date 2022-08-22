unit Label3D;
{--------------------------------------------------------------------------}
{ JL's RotateLabel with 3D-effects                                         }
{                                                                          }
{ Copyright © 1996 by Jörg Lingner, Munich, Germany (jlingner@t-online.de) }
{                                                                          }
{ FREEWARE                                                                 }
{ Free to use and redistribute.                                            }
{ No warranty is given by the author, expressed or implied.                }
{                                                                          }
{ 3D-effects: from RZLABEL-component                                       }
{ Copyright © by Ray Konopka (Raize Software Solutions, Inc.)              }
{--------------------------------------------------------------------------}
{ This component works like TLabel and has 2 additional properties:        }
{                                                                          }
{ Escapement: draw text with angle (0..360 deg)                            }
{             selected font must be a TrueType!!!                          }
{                                                                          }
{ TextStyle:  draw text with 3D-effects  tsRecessed                        }
{                                        tsRaised                          }
{                                        tsNone                            }
{                                                                          }
{--------------------------------------------------------------------------}
{ Vers.  Date   Remarks                                                    }
{ 1.0  30.03.97 Initial release                                            }
{ 2.0   8. 8.01 Enhacenment of component by Jaro Benes JBenes@micrel.cz    }
{                added/changes new features by 3D-effects                  }
{                      tsLightRaised,tsHeavyRecessed + tsShadow            }
{                added E-mail/WWW address accesss                          }
{                added border like bevel/rectangle with adjust             }
{                added layout 16/32bit                                     }
{                *** tested under D1, D3, D4, D5, D6 ***                   }
{ 2.1  14.11.07 Added efect colored for coloring each character            }
{               like html coding. Part code from RALib adopted             }
{--------------------------------------------------------------------------}

interface

{$INCLUDE jbSupp.inc}

uses
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Menus,
  {$IFDEF VER17UP}System.Types, System.UITypes,{$ENDIF}
  ShellApi;

type
  {  TTextStyle  }

  TTextStyle = (tsNone, tsRaised, tsLightRaised, tsRecessed, tsHeavyRecessed, tsShadow);

  {  TBorderType  }

  TBorderType = (btDefault, btRecessed, btRaised);

  {  TLabelStyle  }

  TLabelStyle = (lsDefault, lsWWWAddress, lsEMailAddress);

  {  TEllipsesStyle  }

  TEllipsesStyle = (esNone, esEnd, esPath);

  {  TLabel3D  }

  TLabel3D = class(TCustomLabel)
  private
    fEscapement: Integer;
    fTextStyle: TTextStyle;
    FLabelStyle: TLabelStyle;
    FColorRecess: TColor;
    FColorRaise: TColor;
    FActiveColor: TColor;
    FOldColor: TColor;
    FAddress: string;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FBorderStyle: TPenStyle;
    FBorderType: TBorderType;
    FBorder: Boolean;
    FEllipsesStyle: TEllipsesStyle;
    FShift: Integer;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FColored: Boolean;
    procedure SetEscapement(aVal: Integer);
    procedure SetTextStyle(aVal: TTextStyle);
    procedure SetLabelStyle(aVal: TLabelStyle);
    procedure SetBorderColor(Value: Tcolor);
    procedure SetBorderStyle(Value: TpenStyle);
    procedure SetBorderWidth(Value: Integer);
    procedure SetBorder(Value: Boolean);
    procedure SetBorderType(Value: TBorderType);
    procedure SetEllipsesStyle(Value: TEllipsesStyle);
    procedure SetShift(Value: Integer);
    procedure CalcTextPos(var aRect: TRect; aAngle: Integer; aTxt: string); {$IFDEF SUPP_INL}inline;{$ENDIF}
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetColored(const Value: Boolean);
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); {$IFDEF VER4UP} override; {$ENDIF}
    procedure Paint; override;
    procedure Click; override;
  public
    constructor Create(AOwner: TComponent); override;
    {for easy changes of color in one time - J.B.}
    procedure SetColorRR(RecessColor, RaiseColor: TColor);
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderPenStyle: TPenStyle read FBorderStyle write SetBorderStyle;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property Border: Boolean read FBorder write SetBorder;
    property BorderType: TBorderType read FBorderType write SetBorderType;
    property Escapement: Integer read fEscapement write SetEscapement;
    property TextStyle: TTextStyle read fTextStyle write SetTextStyle;
    property LabelStyle: TLabelStyle read fLabelStyle write SetLabelStyle;
    property Address: string read FAddress write FAddress;
    property EllipsesStyle: TEllipsesStyle read FEllipsesStyle write SetEllipsesStyle;
    property Shift: Integer read FShift write SetShift;
    property Colored: Boolean read FColored write SetColored default False;
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEf VER14UP}
    property EllipsisPosition;
    {$ENDIF}
    property Enabled;
    property FocusControl;
    property Font;
    property Layout;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    {$IFDEf VER14UP}
    property Touch;
    {$ENDIF}
    property Transparent;
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
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseExit: TNotifyEvent read FOnMouseExit write FOnMouseExit;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Support', [TLabel3D]);
end;

constructor TLabel3D.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fEscapement := 0;
  fTextStyle := tsRaised;
  Font.Name := 'Arial';
  FActiveColor := clRed;
  FLabelStyle := lsDefault;
  FBorderWidth := 1;
  FBorderStyle := psSolid;
  FBorder := False;
  FBorderColor := clRed;
  FBorderType := btDefault;
  FColorRecess := clBtnShadow;
  FColorRaise := clBtnHighlight;
  FEllipsesStyle := esNone;
  FShift := 1;
end;

procedure TLabel3D.SetEscapement(aVal: Integer);
begin
  if fEscapement <> aVal then
  begin
    if aVal < 0 then
    begin
      while aVal < -360 do aVal := aVal + 360;
      aVal := 360 + aVal;
    end;
    while aVal > 360 do aVal := aVal - 360;
    fEscapement := aVal;
    Invalidate;
  end;
end;

procedure TLabel3D.SetTextStyle(aVal: TTextStyle);
begin
  if fTextStyle <> aVal then
  begin
    fTextStyle := aVal;
    Invalidate;
  end;
end;

procedure TLabel3D.Paint;

  procedure DoBevel(TheCanvas: TCanvas; Rect: TRect; Color1, Color2: TColor); {$IFDEF SUPP_INL}inline;{$ENDIF}
  //very simple, sorry, like as TBevel -J.B.
  begin
    with TheCanvas do
    begin
      Pen.Color := Color1;
      MoveTo(Rect.Left, Rect.Top);
      LineTo(Rect.Right, Rect.Top);
      MoveTo(Rect.Left, Rect.Top);
      LineTo(Rect.Left, Rect.Bottom);
      Pen.Color := Color2;
      MoveTo(Rect.Right, Rect.Bottom);
      LineTo(Rect.Right, Rect.Top);
      MoveTo(Rect.Right, Rect.Bottom);
      LineTo(Rect.Left, Rect.Bottom);
    end;
  end;

const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  Rect, TempRect: TRect;
  I: Integer;
begin
  with Canvas do
  begin
    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClientRect);
    end;
    Brush.Style := bsClear;
    Rect := ClientRect;
    if FBorder and (FBorderWidth > 0) then {changes J.B.}
    begin
      TempRect := Rect;
      for I := 1 to FBorderWidth do
        //with TempRect do
        begin
          InFlateRect(TempRect, -1, -1);
          Pen.Color := FBorderColor;
          case FBorderType of
            btDefault: Canvas.Rectangle(0, 0, Width, Height);
            btRaised: DoBevel(Canvas, Classes.Rect(0, 0, Width - 1, Height - 1), clWhite, clGray);
            btRecessed: DoBevel(Canvas, Classes.Rect(0, 0, Width - 1, Height - 1), clGray, clWhite);
          end;
        end;
    end;
    DoDrawText(Rect, DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment]);
  end;
end;

procedure TLabel3D.CalcTextPos(var aRect: TRect; aAngle: Integer; aTxt: string);
{ Calculate text pos. depend. on: Font, Escapement, Alignment and length   }
{ if AutoSize true : set properties Height and Width                       }
{--------------------------------------------------------------------------}
var DC: HDC;
  hSavFont: HFont;
  Size: TSize;
  x, y: Integer;
  cStr: array[0..255] of Char;

begin
  StrPCopy(cStr, aTxt);
  DC := GetDC(0);
  hSavFont := SelectObject(DC, Font.Handle);
  GetTextExtentPoint32(DC, cStr, Length(aTxt), Size);
  SelectObject(DC, hSavFont);
  ReleaseDC(0, DC);
  x := 0;
  y := 0;
  if aAngle <= 90 then
  begin { 1.Quadrant }
    x := 0;
    y := Trunc(Size.cx * sin(aAngle * Pi / 180));
  end
  else
  if aAngle <= 180 then
  begin { 2.Quadrant }
    x := Trunc(Size.cx * -cos(aAngle * Pi / 180));
    y := Trunc(Size.cx * sin(aAngle * Pi / 180) + Size.cy * cos((180 - aAngle) * Pi / 180));
  end
  else
  if aAngle <= 270 then
  begin { 3.Quadrant }
    x := Trunc(Size.cx * -cos(aAngle * Pi / 180) + Size.cy * sin((aAngle - 180) * Pi / 180));
    y := Trunc(Size.cy * sin((270 - aAngle) * Pi / 180));
  end
  else
  if aAngle <= 360 then
  begin { 4.Quadrant }
    x := Trunc(Size.cy * sin((360 - aAngle) * Pi / 180));
    y := 0;
  end;
  aRect.Top := aRect.Top + y;
  aRect.Left := aRect.Left + x;

  x := Abs(Trunc(Size.cx * cos(aAngle * Pi / 180))) + Abs(Trunc(Size.cy * sin(aAngle * Pi / 180)));
  y := Abs(Trunc(Size.cx * sin(aAngle * Pi / 180))) + Abs(Trunc(Size.cy * cos(aAngle * Pi / 180)));

  if Autosize then
  begin
    Width := x;
    Height := y;
  end
  else
    if Alignment = taCenter then
    begin
      aRect.Left := aRect.Left + ((Width - x) div 2);
    end
    else
      if Alignment = taRightJustify then
      begin
        aRect.Left := aRect.Left + Width - x;
      end;
end;

procedure TLabel3D.DoDrawText(var Rect: TRect; Flags: Longint);
{--------------------------------------------------------------------------}
{ Draw the text normal or with angle and with 3D-effects                   }
{                                                                          }
{ 3D-effects: RZLABEL-component                                            }
{ (c) by Ray Konopka (Raize Software Solutions, Inc.)                      }
{ enhanced 3D-effect (c)2001 by Jaro Benes mailto:JBenes@micrel.cz         }
{--------------------------------------------------------------------------}
  procedure DrawAngleText(aCanvas: TCanvas; aRect: tRect; aAngle: Integer; aTxt: string);
  
  {--------------------------------------------------------------------------}
  { Draw text with FontIndirect (angle -> escapement)                        }
  {--------------------------------------------------------------------------}
  var
    LFont: TLogFont;
    hOldFont, hNewFont: HFont;
  begin
    CalcTextPos(aRect, aAngle, aTxt);

    GetObject(aCanvas.Font.Handle, SizeOf(LFont), Addr(LFont));
    LFont.lfEscapement := aAngle * 10;
    hNewFont := CreateFontIndirect(LFont);
    hOldFont := SelectObject(aCanvas.Handle, hNewFont);

    aCanvas.TextOut(aRect.Left, aRect.Top, aTxt);

    hNewFont := SelectObject(aCanvas.Handle, hOldFont);
    DeleteObject(hNewFont);
  end;
  function Darker(Col: TColor; Percent: Byte): TColor; {$IFDEF SUPP_INL}inline;{$ENDIF}
  var R, G, B: Byte;
  begin
    R := GetRValue(Col);
    G := GetGValue(Col);
    B := GetBValue(Col);
    R := Round(R * Percent / 100);
    G := Round(G * Percent / 100);
    B := Round(B * Percent / 100);
    Result := RGB(R, G, B);
  end;

  function Lighter(Col: TColor; Percent: Byte): TColor; {$IFDEF SUPP_INL}inline;{$ENDIF}
  var R, G, B: Byte;
  begin
    R := GetRValue(Col);
    G := GetGValue(Col);
    B := GetBValue(Col);
    R := Round(R * Percent / 100) + Round(255 - Percent / 100 * 255);
    G := Round(G * Percent / 100) + Round(255 - Percent / 100 * 255);
    B := Round(B * Percent / 100) + Round(255 - Percent / 100 * 255);
    Result := RGB(R, G, B);
  end;

  // This function mixes two bytes According to value of TRANS
  // The value of TRANS is between 0 (result then will be equal to FG)
  // and 255 (result then will be equal to BG)
  function MixBytes(FG, BG, TRANS: byte): byte;
  {$IFDEF WIN64}
  var
    _R: Word;
  begin
    if Trans = 0 then Result := FG
    else
      if Trans = $FF then Result := BG
      else
      begin
        _R := (TRANS * FG) + (255 - TRANS) * BG;
        Result := Hi(_R);
      end;
  end;
  {$ELSE}
  asm
    push bx  // push some regs
    push cx
    push dx
    mov DH,TRANS // remembering Transparency value (or Opacity - as you like)
    mov BL,FG    // filling registers with our values
    mov AL,DH    // BL = ForeGround (FG)
    mov CL,BG    // CL = BackGround (BG)
    xor AH,AH    // Clear High-order parts of regs
    xor BH,BH
    xor CH,CH
    mul BL       // AL=AL*BL
    mov BX,AX    // BX=AX
    xor AH,AH
    mov AL,DH
    xor AL,$FF   // AX=(255-TRANS)
    mul CL       // AL=AL*CL
    add AX,BX    // AX=AX+BX
    shr AX,8     // Fine! Here we have mixed value in AL
    pop dx       // Hm... No rubbish after us, ok?
    pop cx
    pop bx       // Bye, dear Assembler - we go home to Delphi!
  end;
  {$ENDIF}

  // Here we mix R,G and B channels of our colors separately.
  // The value of T is between 0 and 255 as described above.

  // As you know, TColor value is 4 bytes length integer value where
  // low byte is red channel, 2nd byte is green and 3rd byte is blue

  function MixColors(FG, BG: TColor; T: byte = 255): TColor;
  var r,g,b:byte;
  begin
    R := MixBytes(FG and 255,BG and 255,T); // extracting and mixing Red
    G := MixBytes((FG shr 8) and 255,(BG shr 8) and 255,T); // the same with green
    B := MixBytes((FG shr 16) and 255,(BG shr 16) and 255,T); // and blue, of course
    Result := r+g*256+b*65536; // finishing with combining all channels together
  end;

//  function MixColors(C1, C2: TColor): TColor;
//  begin
//    Result := RGB((GetRValue(C1) + GetRValue(C2)) div 2,
//      (GetGValue(C1) + GetGValue(C2)) div 2,
//      (GetBValue(C1) + GetBValue(C2)) div 2);
//  end;

  type
    TLightDark = (ldLighter, ldNormal, ldDarker);

  procedure DoColoredPaint(iRect: TRect; iColor: TColor; LightDark: TLightDark = ldNormal);
    {Copy of the part of the RALib}
    function SubStr(const S: string; const index: integer; const Separator: string): string; //cannot be inline !!!
    var
      i: integer;
      pB, pE: PChar;
    begin
      Result := '';
      if ((index < 0) or ((index = 0) and (Length(S) > 0) and (S[1] = Separator))) or (Length(S) = 0) then
        Exit;
      pB := PChar(S);
      for i := 1 to index do
      begin
        pB := StrPos(pB, PChar(Separator));
        if pB = nil then exit;
        pB := pB + Length(Separator);
        if pB[0] = #0 then exit;
      end;
      pE := StrPos(pB + 1, PChar(Separator));
      if pE = nil then pE := PChar(S) + Length(S);
      if not (AnsiStrLIComp(pB, PChar(Separator), Length(Separator)) = 0) then
        SetString(Result, pB, pE - pB);
    end;

    procedure ItemHtDrawEx(Canvas: TCanvas; Rect: TRect;
      const State: TOwnerDrawState; const Text: string;
      const HideSelColor: Boolean; var PlainItem: string;
      var Width: Integer; CalcWidth: Boolean; LightDark: TLightDark = ldNormal);
    var
      CL: string;
      i: integer;
      M1: string;
      OriRect: TRect; // it's added

      function Cmp(M1: string): boolean;
      begin
        Result := ANSIStrLIComp(PChar(Text) + i, PChar(M1), Length(M1)) = 0;
      end;

      function Cmp1(M1: string): boolean;
      begin
        Result := ANSIStrLIComp(PChar(Text) + i, PChar(M1), Length(M1)) = 0;
        if Result then inc(i, Length(M1));
      end;

      function CmpL(M1: string): boolean;
      begin
        Result := Cmp(M1 + '>');
      end;

      function CmpL1(M1: string): boolean;
      begin
        Result := Cmp1(M1 + '>');
      end;

      procedure Draw(const M: string);
      begin
        if not Assigned(Canvas) then Exit;
        if not CalcWidth then
          Canvas.TextOut(Rect.Left, Rect.Top, M);
        Rect.Left := Rect.Left + Canvas.TextWidth(M);
      end;

      procedure Style(const Style: TFontStyle; const Include: boolean);
      begin
        if not Assigned(Canvas) then Exit;
        if Include then
          Canvas.Font.Style := Canvas.Font.Style + [Style]
        else
          Canvas.Font.Style := Canvas.Font.Style - [Style];
      end; { if }

    var
      oldFontStyles: TFontStyles;
      oldFontColor: TColor;
      tmpColor: TColor;
    begin
      PlainItem := '';
      oldFontColor := 0; { satisfy compiler }
      if Canvas <> nil then
      begin
        oldFontStyles := Canvas.Font.Style;
        oldFontColor := iColor;//Canvas.Font.Color;
      end;
      try
        if HideSelColor and Assigned(Canvas) then
        begin
          Canvas.Brush.Color := clWindow;
          Canvas.Font.Color := clWindowText;
        end;
        if Assigned(Canvas) then
          Canvas.FillRect(Rect);

        Width := Rect.Left;
        Rect.Left := Rect.Left + 2;

        OriRect := Rect; //save origin rectangle

        M1 := '';
        i := 1;
        while i <= Length(Text) do
        begin
          if (Text[i] = '<') and
            (CmpL('b') or CmpL('/b') or
            CmpL('i') or CmpL('/i') or
            CmpL('u') or CmpL('/u') or
            Cmp('c:') or CmpL('/c')) then
          begin
            Draw(M1);
            PlainItem := PlainItem + M1;

            if CmpL1('b') then
              Style(fsBold, True)
            else if CmpL1('/b') then
              Style(fsBold, False)
            else if CmpL1('i') then
              Style(fsItalic, True)
            else if CmpL1('/i') then
              Style(fsItalic, False)
            else if CmpL1('u') then
              Style(fsUnderline, True)
            else if CmpL1('/u') then
              Style(fsUnderline, False)
            else if CmpL1('/c') then
              Canvas.Font.Color := oldFontColor
            else if Cmp1('c:') then
            begin
              CL := SubStr(PChar(Text) + i, 0, '>');
              if (HideSelColor or not (odSelected in State)) and Assigned(Canvas) then
              try
                if (Length(CL) > 0) and (CL[1] <> '$') then
                  tmpColor := StringToColor('cl' + CL)
                else
                  tmpColor := StringToColor(CL);
                case LightDark of
                  ldLighter: Canvas.Font.Color := Lighter(tmpColor,10);
                  ldDarker: Canvas.Font.Color := Darker(tmpColor,10);
                else
                  Canvas.Font.Color := tmpColor
                end;
                
              except
              end;
              inc(i, Length(CL) + 1 {'>'});
            end;

            M1 := '';
          end
          else
            // next lines were added
            if (Text[i] = chr(13)) and (Cmp1(string(chr(10)))) then
            begin
              // new line
              Draw(M1);
              PlainItem := PlainItem + M1;
              if (Canvas <> nil) then
              begin
                Rect.Left := OriRect.Left;
                Rect.Top := Rect.Top + Canvas.TextHeight(M1);
              end;
              M1 := '';
            end
            else
              // add text
              M1 := M1 + Text[i];
          inc(i);
        end; { for }
        Draw(M1);
        PlainItem := PlainItem + M1;
      finally
        if Canvas <> nil then
        begin
          Canvas.Font.Style := oldFontStyles;
          Canvas.Font.Color := oldFontColor;
        end;
      end;
      Width := Rect.Left - Width + 2;
    end;

    function ItemHtDraw(Canvas: TCanvas; Rect: TRect;
      const State: TOwnerDrawState; const Text: string;
      const HideSelColor: Boolean; LightDark: TLightDark): string;
    var
      S: string;
      W: Integer;
    begin
      ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, False, LightDark);
    end;

    function ItemHtWidth(Canvas: TCanvas; Rect: TRect;
      const State: TOwnerDrawState; const Text: string;
      const HideSelColor: Boolean): Integer;
    var
      S: string;
      W: Integer;
    begin
      ItemHtDrawEx(Canvas, Rect, State, Text, HideSelColor, S, W, True);
      Result := W;
    end;
  var
    S: string;
    H, W, i: Integer;
    Rect: TRect;
    Ss: TStrings;
  begin
    H := Canvas.TextHeight('W');
    Ss := TStringList.Create;
    Ss.Text := Caption;
    try
      for i := 0 to Ss.Count - 1 do { Iterate }
      begin
        S := Ss[i];
        Rect := iRect;//ClientRect;
       {$IFDEF VER3UP}
        case Layout of { }
          tlTop:
            Inc(Rect.Top, H * i);
          tlBottom:
            Rect.Top := Rect.Bottom - (Ss.Count - i) * H;
          tlCenter:
            Rect.Top := (Rect.Bottom - Rect.Top - Ss.Count * H) div 2 + H * i;
        end; { case }
       {$ELSE}
        Inc(Rect.Top, H * i);
       {$ENDIF VER3UP}
        case Alignment of { }
          taLeftJustify: {nothing};
          taRightJustify:
            begin
              W := ItemHtWidth(Canvas, Rect, [], S, False);
              Rect.Left := Rect.Right - W;
            end;
          taCenter:
            begin
              W := ItemHtWidth(Canvas, Rect, [], S, False);
              Rect.Left := Rect.Left + (Rect.Right - Rect.Left - W) div 2;
            end;
        end; { case }
        ItemHtDraw(Canvas, Rect, [], S, False, LightDark);
      end;
    finally
      Ss.Free;
    end;
  end;
var
  Text: string;
  TmpRect: TRect;
  UpperColor: TColor;
  LowerColor: TColor;
{$IFDEF WINDOWS}
  cStr: array[0..1023] of Char;
{$ENDIF}
  ldtype: TLightDark;
begin
  Text := Caption;
{$IFDEF WINDOWS}
  StrPCopy(cStr, Text);
{$ENDIF}

  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and (Text[1] = '&') and (Text[2] = #0)) then
    Text := Text + ' ';

  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;
  if FEllipsesStyle = esEnd then Flags := Flags or DT_END_ELLIPSIS
  else if FEllipsesStyle = esPath then Flags := Flags or DT_PATH_ELLIPSIS;
  if FEscapement = 0 then
    case Layout of
      tlTop: Flags := Flags or DT_TOP;
      tlCenter: Flags := Flags or DT_VCENTER or DT_SINGLELINE;
      tlBottom: Flags := Flags or DT_BOTTOM or DT_SINGLELINE;
    end;

  Canvas.Font := Font;

  if FEscapement in [46..134] then ;
    {special manipulation with shadow position is unsupported- J.B.}

  UpperColor := FColorRaise;
  LowerColor := FColorRecess;

  if FTextStyle <> tsNone then
  begin
    TmpRect := Rect;
    if FTextStyle <> tsShadow then OffsetRect(TmpRect, 1, 1)
    else OffsetRect(TmpRect, FShift, FShift);
    case FTextStyle of
      tsRecessed: Canvas.Font.Color := UpperColor;
      tsHeavyRecessed: Canvas.Font.Color := UpperColor;
      tsRaised: Canvas.Font.Color := LowerColor;
      tsLightRaised: Canvas.Font.Color := LowerColor;
      tsShadow: Canvas.Font.Color := LowerColor;
    end;
    if fTextStyle in [tsRecessed, tsHeavyRecessed] then ldtype := ldLighter
    else ldtype := ldDarker;

    if fEscapement <> 0 then
      DrawAngleText(Canvas, TmpRect, fEscapement, Text)
    else
      if not FColored then
        DrawText(Canvas.Handle, PChar(Text), Length(Text), TmpRect, Flags)
      else
        DoColoredPaint(TmpRect, Canvas.Font.Color, ldtype);

    if FTextStyle in [tsHeavyRecessed, tsLightRaised] then
    begin
      TmpRect := Rect;
      OffsetRect(TmpRect, -1, -1);
      case FTextStyle of
        tsHeavyRecessed: Canvas.Font.Color := LowerColor;
        tsLightRaised: Canvas.Font.Color := UpperColor;
      end;
      if fTextStyle in [tsRecessed, tsHeavyRecessed] then ldtype := ldDarker
      else ldtype := ldLighter;
      if fEscapement <> 0 then
        DrawAngleText(Canvas, TmpRect, fEscapement, Text)
      else
        if not FColored then
          DrawText(Canvas.Handle, PChar(Text), Length(Text), TmpRect, Flags)
        else
          DoColoredPaint(TmpRect, Canvas.Font.Color, ldtype);
    end
  end;
  Canvas.Font.Color := Font.Color;
  if not Enabled then Canvas.Font.Color := clGrayText;
    {main color on font is upper}
  if fEscapement <> 0 then
    DrawAngleText(Canvas, Rect, fEscapement, Text)
  else
    if not FColored then
      DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags)
    else
      DoColoredPaint(Rect, Canvas.Font.Color);
end;

{=new features by J.B.==================================added 8.8.2001=====}

procedure TLabel3D.SetLabelStyle(aVal: TLabelStyle);
begin
  if aVal <> FLabelStyle then
    FLabelStyle := aVal;
end;

procedure TLabel3D.Click;
{$IFDEF Ver80}
var P: PChar;
{$ENDIF}
begin
  try
    try
      if FLabelStyle in [lsWWWAddress, lsEMailAddress] then
      begin
        if FAddress = '' then Exit;
        Cursor := {$IFNDEF Ver80}crAppStart{$ELSE}crHourGlass{$ENDIF};
        Application.ProcessMessages; {otherwise cursor change will be missed}
      {$IFNDEF Ver80}
        if FLabelStyle = lsWWWAddress then
          ShellExecute(0, nil, PChar(FAddress), nil, nil, SW_NORMAL)
        else
          ShellExecute(0, nil, PChar('mailto:' + FAddress), nil, nil, SW_NORMAL);
        Cursor := crHandPoint;
      {$ELSE}
        if FLabelStyle = lsWWWAddress then
        begin
          GetMem(P, Length(FAddress) + 1);
          StrPCopy(P, FAddress);
          try
            ShellExecute(0, nil, P, nil, nil, SW_NORMAL)
          finally
            FreeMem(P, Length(FAddress) + 1);
          end;
        end
        else
        begin
          GetMem(P, Length('mailto:' + FAddress) + 1);
          StrPCopy(P, 'mailto:' + FAddress);
          try
            ShellExecute(0, nil, P, nil, nil, SW_NORMAL);
          finally
            FreeMem(P, Length('mailto:' + FAddress) + 1);
          end;
        end;
        Cursor := crDefault;
      {$ENDIF}
      end; {www,email only}
    except
      {silent except, do nothing}
    end;
  finally
    inherited Click;
  end;
end;

procedure TLabel3D.CMMouseEnter(var Message: TMessage);
begin
  if FLabelStyle in [lsWWWAddress, lsEMailAddress] then
  begin
    FOldColor := Font.Color;
    Font.Style := Font.Style + [fsUnderline];
    Cursor := {$IFNDEF Ver80}crHandPoint{$ELSE}crDefault{$ENDIF};
    Application.Processmessages;
    Font.Color := FActiveColor;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

procedure TLabel3D.CMMouseLeave(var Message: TMessage);
begin
  if FLabelStyle in [lsWWWAddress, lsEMailAddress] then
  begin
    Cursor := crDefault;
    Font.Style := Font.Style - [fsUnderline];
    Application.Processmessages;
    Font.Color := FOldColor;
  end;
  if Assigned(FOnMouseExit) then
    FOnMouseExit(self);
end;

procedure TLabel3D.SetBorderColor(value: Tcolor);
begin
  FBorderColor := value;
  Canvas.Pen.Color := value;
  Invalidate;
end;

procedure TLabel3D.SetBorderWidth(value: integer);
begin
  FBorderWidth := value;
  Canvas.Pen.Width := Value;
  Invalidate;
end;

procedure TLabel3D.SetBorder(value: boolean);
begin
  FBorder := value;
  Invalidate;
end;

procedure TLabel3D.SetBorderStyle(value: TPenStyle);
begin
  Canvas.Pen.Style := value;
  Invalidate;
end;

procedure TLabel3D.SetBorderType(Value: TBorderType);
begin
  if FBorderType <> Value then
  begin
    FBorderType := Value;
    Invalidate;
  end;
end;

procedure TLabel3D.SetColored(const Value: Boolean);
begin
  if Value <> FColored then
  begin
    FColored := Value;
    Invalidate;
  end;
end;

procedure TLabel3D.SetColorRR(RecessColor, RaiseColor: TColor);
var
  DoRepaint: Boolean;
begin
  DoRepaint := False;
  if FColorRecess <> RecessColor then
  begin
    FColorRecess := RecessColor;
    DoRepaint := True;
  end;
  if FColorRaise <> RaiseColor then
  begin
    FColorRaise := RaiseColor;
    DoRepaint := True;
  end;
  if DoRepaint then
    Invalidate;
end;

procedure TLabel3D.SetEllipsesStyle(Value: TEllipsesStyle);
begin
  if FEllipsesStyle <> Value then
  begin
    if Value <> esNone then
      AutoSize := false;
    FEllipsesStyle := Value;
    Invalidate;
  end;
end;

procedure TLabel3D.SetShift(Value: Integer);
begin
  if Value <> FShift then
  begin
    FShift := Value;
    Invalidate;
  end;
end;

{=end of tail==============================================================}

end.
