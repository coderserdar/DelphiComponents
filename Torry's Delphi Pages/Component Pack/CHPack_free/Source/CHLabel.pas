unit CHLabel;

{ ##############################################################################
  TCHLabel

  Version   		:   1.0.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)
  Licence    		:   without Source --> Freeware
  									with Source    --> Shareware
  Original code	:   TLabel

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  NOTE: TCHLabel is not longer supported. In the future use TCHAdvancedLabel.
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  History:
  1.0.0 - 21.07.2002    - First Release
  1.0.1 - 30.08.2002    - BUG: "WordWrap" und mehrzeiliger Text, Speicherleck in Paint behoben

  ############################################################################ }

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, Controls, StdCtrls, Graphics, ShellApi,

  _CHClassProperty, _CHClassFunction, _CHFunction;

type

  TCHCustomLabel = class(TLabel)
  private
    FCaptionLayout : TCHCaptionLayout;

    FOnClick : TNotifyEvent;
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;

    FExecuteActive : Boolean;
    FExecuteText : string;
    FExecuteParameter : string;
    FExecuteType : TExecuteType;

    procedure SetTextStyle(const Value: TTextStyle);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure UpdateChanges(Sender: TObject);
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure AutosizeCorrect(var x, y, nText_X, nText_Y : Integer);
  public
    constructor Create(AOwner : TComponent); override;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    property CaptionLayout : TCHCaptionLayout read FCaptionLayout write FCaptionLayout;
    property ExecuteActive : Boolean read FExecuteActive write FExecuteActive;
    property ExecuteText : string read FExecuteText write FExecuteText;
    property ExecuteParameter : string read FExecuteParameter write FExecuteParameter;
    property ExecuteType : TExecuteType read FExecuteType write FExecuteType;
  end;


  TCHLabel = class(TCHCustomLabel)
  published
    property Alignment;
    property Align;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property CaptionLayout;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExecuteActive;
    property ExecuteText;
    property ExecuteParameter;
    property ExecuteType;
    property FocusControl;
    property Font;
    property Hint;
    property Layout;
    property Left;
    property Name;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Tag;
    property Top;
    property Transparent;
    property Visible;
    property Width;
    property WordWrap;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnEndDock;
    property OnStartDock;
    property OnStartDrag;

  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHLabel]);
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCaptionLayout := TCHCaptionLayout.Create;
  FCaptionLayout.OnChange := UpdateChanges;

  Font.Color := clBlack;
  FExecuteType := etWWW;
  FExecuteText := 'http://www.Blue-Xplosion.de';
  FExecuteActive := False;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLabel.UpdateChanges(Sender: TObject);
begin
  if csLoading in ComponentState then
    Exit;
  Invalidate;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLabel.Click;
var
  sExecute, sParam : string;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);

  if Enabled = True then
  begin
    if FExecuteActive and (FExecuteType <> etNone) and (FExecuteText <> '') then
    begin
      sParam := FExecuteParameter;

      if FExecuteType = etEMail then
        sExecute := 'mailto:' + FExecuteText + sParam
      else if FExecuteType = etNews then
        sExecute := 'news:' + FExecuteText
      else if FExecuteType = etWWW then
        sExecute := FExecuteText
      else if FExecuteType = etEXE then
        sExecute := FExecuteText;

      ShellExecute(0, 'open', PChar(sExecute), PChar(sParam), nil, SW_ShowNormal);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLabel.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLabel.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLabel.Paint;
var
  Text, TmpText, SubText: string;
  Flags, nTextWidth, nNewTextWidth : Integer;
  x, y, nText_X, nText_Y, TextRow, CaptionWidth, CaptionHeight : Integer;
  nAngle : Word;

  WorkRect, CaptionRect, TempRect : TRect;
  SavFontColor : TColor;
  LogFont : TLogFont;
  Size : TSize;

type
  TOffsetDirection = (Off_X, Off_Y);

const
  Offset_Values : array[TDirection, TOffsetDirection] of -1..1 =
  ((0, 0), (0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (1, -1), (-1, 1), (1, 1));

  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

  { **************************************************** }
  procedure DoDrawText(var Rect : TRect);
  begin
    if (Flags and DT_CALCRECT <> 0) and
       ((Text = '') or (ShowAccelChar = True) and (Text[1] = '&') and (Text[2] = #0)) then
    begin
      Text := Text + ' ';
    end;

    // Ausgabe Text
    with Canvas do
    begin
      if FCaptionLayout.Angle <> 0 then
        TextOut(Rect.Left, Rect.Top, Text)
      else
        DrawText(Canvas.Handle, PChar(Text), Length(Text), Rect, Flags);
    end;
  end;
  { **************************************************** }
begin
  x := 0;
  y := 0;
  nText_X := 0;
  nText_Y := 0;
  TextRow := 1;
  nNewTextWidth := 0;
  Text := Caption;
  SavFontColor := Font.Color;
  nAngle := FCaptionLayout.Angle;

  // Text Flags
  Flags := DT_EXPANDTABS or Alignments[Alignment];
  if WordWrap then
    Flags := Flags or DT_WORDBREAK;
  if not ShowAccelChar then
      Flags := Flags or DT_NOPREFIX;

  // TextStyle
  SetTextStyle(FCaptionLayout.TextStyle);

  with Canvas do
  begin
    WorkRect := ClientRect;

    Size.cy := TextHeight(Text);
    Size.cx := TextWidth(Text);

    if WordWrap then
    begin
      TmpText := Text;
      while TmpText <> '' do
      begin
        SubText := GetStringPartFromLeft(TmpText, #10, True, True);
        nTextWidth := TextWidth(SubText);
        if nNewTextWidth < nTextWidth then
          nNewTextWidth := nTextWidth;

        if SubText <> '' then
        begin
          Inc(TextRow);
        end;
      end;
      Dec(TextRow);
      CaptionRect := Rect(0, 0, nNewTextWidth, Size.cy * TextRow);
    end
    else
    begin
      CaptionRect := Rect(0, 0, Size.cx, Size.cy * TextRow);
    end;

    CaptionWidth := CaptionRect.Right - CaptionRect.Left;
    CaptionHeight := CaptionRect.Bottom - CaptionRect.Top;

    // Alignment anpassen (WICHTIG für WordWrap in Verbindung mit CaptionLayout.Alignment)
    with CaptionLayout do
    begin
      if (Alignment = tgCenter) or (Alignment = tgCenterBottom) or (Alignment = tgCenterTop) then
        Self.Alignment := taCenter
      else if (Alignment = tgBottomLeft) or (Alignment = tgCenterLeft) or (Alignment = tgTopLeft) then
        Self.Alignment := taLeftJustify
      else if (Alignment = tgBottomRight) or (Alignment = tgCenterRight) or (Alignment = tgTopRight) then
        Self.Alignment := taRightJustify
      else
        Self.Alignment := taLeftJustify;

    end;

    //*****************************
    // +++ Transparent +++
    //*****************************
    if Transparent = False then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(WorkRect);
    end;
    // weiteres zeichnen auf Canvas verhindern
    Brush.Style := bsClear;

    //*****************************
    // +++ Enable +++
    //*****************************
    if Enabled = False then
    begin
      OffsetRect(CaptionRect, 1, 1);
      Canvas.Font.Color := clBtnHighlight;
      DoDrawText(CaptionRect);

      OffsetRect(CaptionRect, -1, -1);
      Canvas.Font.Color := clBtnShadow;
      DoDrawText(CaptionRect);
    end;

    //*****************************
    // +++ Aligment +++
    //*****************************
    if Self.AutoSize = False then
      TextAlignment(CaptionRect, WorkRect, CaptionLayout.Alignment, Size, CaptionLayout.PosX, CaptionLayout.PosY);


    GetObject(Font.Handle, SizeOf(LogFont), Addr(LogFont));
    //*****************************
    // +++ Angle +++
    //*****************************
    if (nAngle <> 0) then
    begin
      LogFont.lfEscapement := nAngle * 10;
      LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;
      if CaptionLayout.Antialiasing then
        LogFont.lfQuality := ANTIALIASED_QUALITY;

      if nAngle <= 90  then
      begin
         x := 0;
         y := Trunc(Size.cx * sin(nAngle * Pi / 180));
      end
      else if nAngle <= 180 then
      begin
         x := Trunc(Size.cx * -cos(nAngle * Pi / 180));
         y := Trunc(Size.cx *  sin(nAngle * Pi / 180) +
          Size.cy * cos((180 - nAngle) * Pi / 180));
      end
      else if nAngle <= 270 then
      begin
         x := Trunc(Size.cx * -cos(nAngle * Pi / 180) +
          Size.cy * sin((nAngle - 180) * Pi / 180));
         y := Trunc(Size.cy * sin((270 - nAngle) * Pi / 180));
      end
      else if nAngle <= 360 then
      begin
         x := Trunc(Size.cy * sin((360 - nAngle) * Pi / 180));
         y := 0;
      end;
      CaptionRect.Top := CaptionRect.Top + y;
      CaptionRect.Left := CaptionRect.Left + x;
    end
    else
    begin
      LogFont.lfEscapement := 0;
      if CaptionLayout.Antialiasing then
        LogFont.lfQuality := ANTIALIASED_QUALITY;
    end;
    Font.Handle := CreateFontIndirect(LogFont);

    // Maße Control setzen
    if Self.Autosize then
    begin
      // Maße für Zeichenbereich (Control) festlegen
//      x := Abs(Trunc(Size.cx * cos(nAngle * Pi / 180))) +
//        Abs(Trunc(Size.cy * sin(nAngle * Pi / 180)));
//      y := Abs(Trunc(Size.cx * sin(nAngle * Pi / 180))) +
//        Abs(Trunc(Size.cy * cos(nAngle * Pi / 180)));

      x := Abs(Trunc(CaptionWidth * cos(nAngle * Pi / 180))) +
        Abs(Trunc(CaptionHeight * sin(nAngle * Pi / 180)));
      y := Abs(Trunc(CaptionWidth * sin(nAngle * Pi / 180))) +
        Abs(Trunc(CaptionHeight * cos(nAngle * Pi / 180)));

      AutosizeCorrect(x, y, nText_X, nText_Y);
      Width  := x;
      Height := y;
      OffsetRect(CaptionRect, nText_X, nText_Y);
    end;

    //***************
    // E F F E K T E
    //***************
    if FCaptionLayout.TextStyle <> tsNone then
    begin
      // Original Rect zwischenspeichern
      TempRect := CaptionRect;

      Font.Color := Self.Font.Color;

      // Rect für Shadow festlegen
      OffsetRect(CaptionRect,
        FCaptionLayout.ShadowDepth * Offset_Values[FCaptionLayout.ShadowDirection, Off_X],
        FCaptionLayout.ShadowDepth * Offset_Values[FCaptionLayout.ShadowDirection, Off_Y]);
      // Font-Color für Shadow setzen
      if FCaptionLayout.ShadowDepth > 0 then
        Font.Color := FCaptionLayout.ShadowColor;
      // Shadow zeichnen
      DoDrawText(CaptionRect);

      CaptionRect := TempRect;
      // Rect für Highlight festlegen
      OffsetRect(CaptionRect,
        FCaptionLayout.HighlightDepth * Offset_Values[FCaptionLayout.HighlightDirection, Off_X],
        FCaptionLayout.HighlightDepth * Offset_Values[FCaptionLayout.HighlightDirection, Off_Y]);
      // Font-Color für Highlight setzen
      if FCaptionLayout.HighlightDepth > 0 then
        Font.Color := FCaptionLayout.HighlightColor;
      // Highlight zeichnen
      DoDrawText(CaptionRect);

      // Original Rect restaurieren
      CaptionRect := TempRect;
    end;
    // Original Font zuweisen und zeichnen
    Font.Color := SavFontColor;
    DoDrawText(CaptionRect);

  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomLabel.SetTextStyle(const Value: TTextStyle);
begin
  with FCaptionLayout do
  begin
    TextStyle := Value;

    { ssNone }
    if Value = tsNone then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drNone;
      HighlightDepth := 0;
      HighlightColor := clBtnHighlight;
    end
    { ssRaised }
    else if Value = tsRaised then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRaisedColor }
    else if Value = tsRaisedColor then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;
      Self.Font.Color := clBtnFace;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRaisedShadow }
    else if Value = tsRaisedShadow then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;

      HighlightDirection := drUpLeft;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessed }
    else if Value = tsRecessed then
    begin
      ShadowDirection := drNone;
      ShadowDepth := 0;
      ShadowColor := clBtnShadow;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessedColor }
    else if Value = tsRecessedColor then
    begin
      ShadowDirection := drUpLeft;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;
      Self.Font.Color := clBtnFace;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssRecessedShadow }
    else if Value = tsRecessedShadow then
    begin
      ShadowDirection := drUpLeft;
      ShadowDepth := 1;
      ShadowColor := clBtnShadow;

      HighlightDirection := drDownRight;
      HighlightDepth := 1;
      HighlightColor := clBtnHighlight;
    end
    { ssShadow }
    else if Value = tsShadow then
    begin
      ShadowDirection := drDownRight;
      ShadowDepth := 2;
      ShadowColor := clBtnShadow;

      HighlightDirection := drNone;
      HighlightDepth := 0;
      HighlightColor := clBtnHighlight;
    end;

    //Invalidate;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ Korrigiert Autosize, damit auch Texteffekte angezeigt werden }
procedure TCHCustomLabel.AutosizeCorrect(var x, y, nText_X, nText_Y : Integer);
var
  nHighlight_Y, nShadow_Y, nHighlight_X, nShadow_X, nComp_X, nComp_Y : Integer;
begin
  with FCaptionLayout do
  begin
    // Maße von Autosize korrigieren, wenn Effekt
    if (ShadowDirection <> drNone) or (HighlightDirection <> drNone) then
    begin
      // B R E I T E
      nHighlight_Y := 0;
      nShadow_Y := 0;
      // Shadow Y wenn Up
      if (ShadowDirection = drUp) or (ShadowDirection = drUpLeft) or (ShadowDirection = drUpRight) then
        nShadow_Y := ShadowDepth;
      // Highlight Y wenn Up
      if (HighlightDirection = drUp) or (HighlightDirection = drUpLeft) or (HighlightDirection = drUpRight) then
        nHighlight_Y := HighlightDepth;
      // Y-Korrektur-Wert wenn xxUp
      nComp_Y := GetLargerInt(nShadow_Y, nHighlight_Y);
      nText_Y := nComp_Y;

      nHighlight_Y := 0;
      nShadow_Y := 0;
      // Shadow Y wenn Down
      if (ShadowDirection = drDown) or (ShadowDirection = drDownLeft) or (ShadowDirection = drDownRight) then
        nShadow_Y := ShadowDepth;
      // Highlight Y wenn Down
      if (HighlightDirection = drDown) or (HighlightDirection = drDownLeft) or (HighlightDirection = drDownRight) then
        nHighlight_Y := HighlightDepth;

      // Y-Korrektur-Wert wenn xxDown
      nComp_Y := nComp_Y + GetLargerInt(nShadow_Y, nHighlight_Y);
      y := y + nComp_Y;


      // H Ö H E
      nHighlight_X := 0;
      nShadow_X := 0;
      // Shadow X wenn Left
      if (ShadowDirection = drLeft) or (ShadowDirection = drUpLeft) or (ShadowDirection = drDownLeft) then
        nShadow_X := ShadowDepth;
      // Highlight X wenn Left
      if (HighlightDirection = drLeft) or (HighlightDirection = drUpLeft) or (HighlightDirection = drDownLeft) then
        nHighlight_X := HighlightDepth;
      // X-Korrektur-Wert wenn xxLeft
      nComp_X := GetLargerInt(nShadow_X, nHighlight_X);
      nText_X := nComp_X;

      nHighlight_X := 0;
      nShadow_X := 0;
      // Shadow X wenn Right
      if (ShadowDirection = drRight) or (ShadowDirection = drUpRight) or (ShadowDirection = drDownRight) then
        nShadow_X := ShadowDepth;
      // Highlight X wenn Right
      if (HighlightDirection = drRight) or (HighlightDirection = drUpRight) or (HighlightDirection = drDownRight) then
        nHighlight_X := HighlightDepth;

      // X-Korrektur-Wert wenn xxRight
      nComp_X := nComp_X + GetLargerInt(nShadow_X, nHighlight_X);
      x := x + nComp_X;
    end;
  end;
end;


end.
