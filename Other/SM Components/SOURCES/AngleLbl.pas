{ Copyright (C) 1998-2008, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  This component is a extended TLabel component and allows
  the next additional features:
   - you can define an angle for rotating text (in degrees) - see Angle property
   - you can define a text style - Style property:
     slNormal - usual normal 2D text
     slRaised - raised 3D text
     slLowered - lowered 3D text
     For slRaised and slLowered style you can define a shadow color
     in ShadowColor property
}
unit AngleLbl;

interface

{$I SMVersion.inc}

uses
  Windows, Classes, Graphics, StdCtrls;

type
  TStyleLabel = (slNormal, slLowered, slRaised);

  TAngleLabel = class(TCustomLabel)
  private
    { Private declarations }
    FAngle: Integer;
    FStyle: TStyleLabel;
    FShadowColor: TColor;
    procedure SetAngle(Value: Integer);
    procedure SetStyle(Value: TStyleLabel);
    procedure SetShadowColor(Value: TColor);

    procedure DoDrawLblText(var Rect: TRect; Flags: Word);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  published
    { Published declarations }
    property Angle: Integer read FAngle write SetAngle;
    property Style: TStyleLabel read FStyle write SetStyle;
    property ShadowColor: TColor read FShadowColor write SetShadowColor;

    property Align;
    property Alignment;
{$IFDEF SMForDelphi4}
    property Anchors;
{$ENDIF}
    property Caption;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
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
    property OnStartDrag;
  end;

procedure Register;

implementation
{$R *.RES}

procedure Register;
begin
  RegisterComponents('SMComponents', [TAngleLabel]);
end;

constructor TAngleLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  AutoSize := False;
  FAngle := 0;
  FStyle := slRaised;
  FShadowColor := clHighlightText;
  Font.Name := 'Arial';
end;

procedure TAngleLabel.SetAngle(Value: Integer);
begin
  if (Value <> FAngle) then
  begin
    FAngle := Value;
    if (FAngle < 0) or (FAngle > 359) then
      FAngle := 0;
    Invalidate;
  end;
end;

procedure TAngleLabel.SetStyle(Value: TStyleLabel);
begin
  if (Value <> FStyle) then
  begin
    FStyle := Value;
    Invalidate;
  end;
end;

procedure TAngleLabel.SetShadowColor(Value: TColor);
begin
  if (Value <> FShadowColor) then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure DrawWrapText(Canvas: TCanvas; Text: string; ARect: TRect; Flags: Word);
begin
  Canvas.TextOut(ARect.Left, ARect.Top, Text)
//  DrawText(Canvas.Handle, PChar(Text), Length(Text), ARect, Flags)
end;

procedure TAngleLabel.DoDrawLblText(var Rect: TRect; Flags: Word);
var Text: string;
    LogRec: TLogFont;
    OldFontHandle, NewFontHandle: hFont;
    fDegToRad, fCosAngle, fSinAngle: Double;
    H, W, X, Y: Integer;
    BRect: TRect;
begin
  Text := GetLabelText;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then Text := Text + ' ';
  if not ShowAccelChar then Flags := Flags or DT_NOPREFIX;

  fDegToRad := PI / 180;
  fCosAngle := cos(FAngle * fDegToRad);
  fSinAngle := sin(FAngle * fDegToRad);
  with Canvas do
  begin
    Font := Self.Font;
    Brush.Style := bsClear;

    {-create a rotated font based on the font object Font}
    GetObject(Font.Handle, SizeOf(LogRec), Addr(LogRec));
    LogRec.lfEscapement := FAngle*10;
    LogRec.lfOrientation := FAngle*10;
    LogRec.lfOutPrecision := OUT_DEFAULT_PRECIS;
    LogRec.lfClipPrecision := OUT_DEFAULT_PRECIS;
    NewFontHandle := CreateFontIndirect(LogRec);

    W := TextWidth(Text);
    H := TextHeight(Text);
    case Alignment of
      taLeftJustify: begin
                       X := 0;
                       case FAngle of
                          91..180: X := X - Trunc(W*fCosAngle);
                         181..270: X := X - Trunc(W*fCosAngle) - Trunc(H*fSinAngle);
                         271..359: X := X - Trunc(H*fSinAngle);
                       end;
                       if X > Width then X := Width;
                     end;
      taRightJustify: begin
                        X := Width;
                        case FAngle of
                             0..90: X := X - Trunc(W*fCosAngle) - Trunc(H*fSinAngle);
                           91..180: X := X - Trunc(H*fSinAngle);
                          271..359: X := X - Trunc(W*fCosAngle);
                        end;
                        if X < 0 then X := 0;
                      end;
      else//  taCenterJustify
        X := (Width div 2) - Trunc(W/2*fCosAngle) - Trunc(H/2*fSinAngle);
    end;

    case Layout of
      tlTop: begin
               Y := 0;
               case FAngle of
                    0..90: Y := Y + Trunc(W*fSinAngle);
                  91..180: Y := Y + Trunc(W*fSinAngle) - Trunc(H*fCosAngle);
                 181..270: Y := Y - Trunc(H*fCosAngle);
               end;
               if Y > Height then Y := Height;
             end;
      tlBottom: begin
                  Y := Height;
                  case FAngle of
                      0..90: Y := Y - Trunc(H*fCosAngle);
                    181..270: Y := Y + Trunc(W*fSinAngle);
                    271..359: Y := Y + Trunc(W*fSinAngle) - Trunc(H*fCosAngle);
                  end;
                  if Y < 0 then Y := 0;
                end;
      else//  tlCenter
        Y := (Height div 2) + Trunc(W/2*fSinAngle) - Trunc(H/2*fCosAngle);
    end;

    if not Enabled then
    begin
      Font.Color := clBtnHighlight;
      {OldFontHandle := }SelectObject(Handle, NewFontHandle);
      with BRect do
      begin
        Left := X+1;
        Top := Y+1;
        Right := Rect.Right;
        Bottom := Rect.Bottom;
      end;
//      BRect := Rect;
      DrawWrapText(Canvas, Text, BRect, Flags);
      Font.Color := clBtnShadow;
    end
    else
      if (FStyle <> slNormal) then
      begin
        Font.Color := FShadowColor;

        {OldFontHandle := }SelectObject(Handle, NewFontHandle);
        if (FStyle = slRaised) then
          with BRect do
          begin
            Left := X-1;
            Top := Y-1;
            Right := Rect.Right;
            Bottom := Rect.Bottom;
          end
        else
          with BRect do
          begin
            Left := X+1;
            Top := Y+1;
            Right := Rect.Right;
            Bottom := Rect.Bottom;
          end;
//      BRect := Rect;
        DrawWrapText(Canvas, Text, BRect, Flags);

        Font.Color := Self.Font.Color;
      end;

    OldFontHandle := SelectObject(Handle, NewFontHandle);
    with BRect do
    begin
      Left := X;
      Top := Y;
      Right := Rect.Right;
      Bottom := Rect.Bottom;
    end;
//      BRect := Rect;
    DrawWrapText(Canvas, Text, BRect, Flags);

    NewFontHandle := SelectObject(Canvas.Handle, OldFontHandle);
    DeleteObject(NewFontHandle);
  end;
end;

procedure TAngleLabel.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);

var Rect, CalcRect: TRect;
    DrawStyle: Integer;
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
    DrawStyle := DT_EXPANDTABS or WordWraps[WordWrap] or Alignments[Alignment];
    { Calculate vertical layout }
    if (Layout <> tlTop) then
    begin
      CalcRect := Rect;
      DoDrawLblText(CalcRect, DrawStyle or DT_CALCRECT);
      if (Layout = tlBottom) then
        OffsetRect(Rect, 0, Height - CalcRect.Bottom)
      else
        OffsetRect(Rect, 0, (Height - CalcRect.Bottom) div 2);
    end;
    DoDrawLblText(Rect, DrawStyle);
  end;
end;

end.
