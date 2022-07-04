unit _CHClassFunction;

interface                  

uses Windows, Messages, Classes, Controls, Graphics,
  _CHClassProperty;


procedure DrawGradient(Canvas : TCanvas; Rect: TRect; aColor: array of TColor;
  GradinetStyle : TGradientStyle; GradientRotation : TGradientStep);

procedure DrawCaption(
  Ctl : TControl;
  Canvas : TCanvas;
  var CaptionRect, WorkRect, ClientRect : TRect;
  Angle : TAngle;
  HDepth, SDepth : Integer;
  HDirection, SDirection : TDirection;
  HColor, SColor, CtlColor, DisabelColor, CtlFontColor, TransColor : TColor;
  Text : string;
  FontBmp : TBitmap;
  TextStyle : TTextStyle;
  Alignment : TTextAlign;
  FillStyle : TFillStyle;
  GlyphTransMode : TTransMode;
  GlyphAlignMode : TGlyphAlignMode;
  GlyphAlignment : TGlyphAlignment;
  GlyphEnabled : TGlyphEnabled;
  Glyph : TBitmap;
  GlyphXPos, GlyphYPos, GlyphSpace, TextXPos, TextYPos : Integer;
  Autosize, State, Antialiasing, Accel, Effect : Boolean;
  FCompType : TCompType);

procedure DrawNormal(Canvas : TCanvas; WorkRect : TRect; ContolColor : TColor);
procedure DrawTransparent(Control : TControl; Canvas : TCanvas; WorkRect : TRect);
procedure DrawTransparentBmp(Control: TControl; Dest: TCanvas);
procedure DrawGlyph(Ctl : TControl; Canvas : TCanvas; Glyph : TBitmap; GlyphAlignMode : TGlyphAlignMode;
  GlyphAlignment : TGlyphAlignment; GlyphXPos, GlyphYPos, GlyphSpace : Integer;
  TransMode : TTransMode; TransColor : TColor; CaptionRect, ClientRect : TRect;
  FontSize : TSize; var NewGlyph_X, NewGlyph_Y : Integer; EnabledMode : TGlyphEnabled);

procedure TextAlignment(var CaptionRect : TRect; WorkRect : TRect; Alignment : TTextAlign; Size : TSize;
  TextXPos, TextYPos : Integer);

procedure AlignPos(var nGlyph_X, nGlyph_Y : Integer; const XPos, YPos, Space : Integer;
  ButtonRect, ClientRect : TRect;
  Size : TSize; Glyph : TBitmap; GlyphAlignMode : TGlyphAlignMode; GlyphAlignment : TGlyphAlignment);

procedure CorrectSize(var nWidth, nHeight : Integer; SDirection, HDirection : TDirection;
  SDepth, HDepth : Integer);

procedure CorrectCaptionPos(var xPos, yPos : Integer; SDirection, HDirection : TDirection;
  SDepth, HDepth : Integer);

procedure Dummy;


implementation


{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure DrawGradient(Canvas : TCanvas; Rect: TRect; aColor: array of TColor;
  GradinetStyle : TGradientStyle; GradientRotation : TGradientStep);
var
  SubRow, ColorCount, ColorPart, ColPos, MaxColor, ColorEnd, ColorDivider, PixCount : integer;
  SavPenWidth, nBitCount,  nGradEnd, nHBmpWidth, nVBmpHeight  : Integer;
  I, nX, nDivY, nDivX, nStart, nStep : Integer;
  CtlWidth, CtlHeight : Integer;
  nFaktor, nGradStep, nStepSum, ColFaktor : Double;

  SavPenStyle : TPenStyle;
  SavPenColor : TColor;

  aSubRGB : TRGBArray;
  aRGBArray : array of TRGBArray;
  HBitmap, VBitmap, TmpHBitmap, TmpVBitmap : TBitmap;
begin
  nBitCount := 0;
  nX := 0;
  nStepSum := 0;
  MaxColor := High(aColor);
  CtlWidth := Rect.Right - Rect.Left;
  CtlHeight := Rect.Bottom - Rect.Top;
  if MaxColor > 0 then
  begin
    with Canvas do
    begin
      Pen.Width := 1;
      Pen.Style := psSolid;
      SavPenWidth := Pen.Width;
      SavPenStyle := Pen.Style;
      SavPenColor := Pen.Color;

      HBitmap := TBitmap.Create;
      HBitmap.Width := CtlWidth * 4;
      HBitmap.Height := 1;

      VBitmap := TBitmap.Create;
      VBitmap.Width := 1;
      VBitmap.Height := CtlHeight * 4;

      Setlength(aRGBArray, MaxColor + 1);

      for ColorCount := 0 to MaxColor do
      begin
        aColor[ColorCount] := colortorgb(aColor[ColorCount]);
        aRGBArray[ColorCount][0] := GetRValue(aColor[ColorCount]);
        aRGBArray[ColorCount][1] := GetGValue(aColor[ColorCount]);
        aRGBArray[ColorCount][2] := GetBValue(aColor[ColorCount]);
      end;

      // ******************* Gradient Horizontal **************************
      PixCount := (Rect.Right - Rect.Left);

      ColorDivider := Round(PixCount / MaxColor);
      for ColorCount := 0 to MaxColor - 1 do
      begin
        if ColorCount = MaxColor - 1 then
          ColorEnd := PixCount - ColorCount * ColorDivider - 1
        else
          ColorEnd := ColorDivider;

        for SubRow := 0 to ColorEnd do
        begin
          ColPos := SubRow + ColorCount * ColorDivider;
          ColFaktor := SubRow / ColorEnd;

          for ColorPart := 0 to 3 do
            aSubRGB[ColorPart] := trunc(aRGBArray[ColorCount][ColorPart] +
              ((aRGBArray[ColorCount + 1][ColorPart] - aRGBArray[ColorCount][ColorPart]) * ColFaktor));

          Pen.Color := RGB(aSubRGB[0], aSubRGB[1], aSubRGB[2]);

          // horizontales Bitmap zeichnen
          HBitmap.Canvas.Pixels[ColPos, 0] := Pen.Color;
          HBitmap.Canvas.Pixels[((CtlWidth * 2) - 1) - ColPos, 0] := Pen.Color;
          HBitmap.Canvas.Pixels[(CtlWidth * 2) + ColPos, 0] := Pen.Color;
          HBitmap.Canvas.Pixels[(HBitmap.Width - 1) - ColPos, 0] := Pen.Color;
        end;
      end;

      // ******************* Gradient Vertikal **************************
      PixCount := Rect.Bottom - Rect.Top;

      ColorDivider := Round(PixCount / MaxColor);
      for ColorCount := 0 to MaxColor - 1 do
      begin
        if ColorCount = MaxColor - 1 then
          ColorEnd := PixCount - ColorCount * ColorDivider - 1
        else
          ColorEnd := ColorDivider;

        for SubRow := 0 to ColorEnd do
        begin
          ColPos := SubRow + ColorCount * ColorDivider;
          ColFaktor := SubRow / ColorEnd;

          for ColorPart := 0 to 3 do
            aSubRGB[ColorPart] := trunc(aRGBArray[ColorCount][ColorPart] +
              ((aRGBArray[ColorCount + 1][ColorPart] - aRGBArray[ColorCount][ColorPart]) * ColFaktor));

          Pen.Color := RGB(aSubRGB[0], aSubRGB[1], aSubRGB[2]);

          // veticales Bitmap zeichnen
          VBitmap.Canvas.Pixels[0, ColPos] := Pen.Color;
          VBitmap.Canvas.Pixels[0, ((CtlHeight * 2) - 1) - ColPos] := Pen.Color;
          VBitmap.Canvas.Pixels[0, (CtlHeight * 2) + ColPos] := Pen.Color;
          VBitmap.Canvas.Pixels[0, (VBitmap.Height - 1) - ColPos] := Pen.Color;

        end;
      end;

      // Größe horizontales Bitmap
      while (CtlHeight * 2) > HBitmap.Width do
      begin
        nGradEnd := HBitmap.Width;
        HBitmap.Width := HBitmap.Width * 2;
        HBitmap.Canvas.Draw(nGradend, 0, HBitmap);
      end;
      // Größe vertikales Bitmap
      while (CtlWidth * 2) > VBitmap.Height do
      begin
        nGradEnd := VBitmap.Height;
        VBitmap.Height := VBitmap.Height * 2;
        VBitmap.Canvas.Draw(0, nGradend, VBitmap);
      end;


      nHBmpWidth := CtlWidth;
      nVBmpHeight := CtlHeight;

      if (GradinetStyle = gsHorizontal_L) or
        (GradinetStyle = gsHorizontal_R) or
        (GradinetStyle = gsArrow_L) or
        (GradinetStyle = gsArrow_R) or
        (GradinetStyle = gsDiagonal_R) or
        (GradinetStyle = gsDiagonal_L) then
      begin
        nBitCount := Rect.Bottom - Rect.Top;
      end
      else if (GradinetStyle = gsVertical_T) or
        (GradinetStyle = gsVertical_B) or
        (GradinetStyle = gsArrow_T) or
        (GradinetStyle = gsArrow_B) then
      begin
        nBitCount := Rect.Right - Rect.Left;
      end;


      // Control mit Gradientbitmap füllen
      if GradinetStyle <> gsRotation then
      begin
        for I := 0 to nBitCount do
        begin
          // gsHorizontal_L
          if GradinetStyle = gsHorizontal_L then
            Draw(-nHBmpWidth, I, HBitmap)
          // gsHorizontal_R
          else if GradinetStyle = gsHorizontal_R then
            Draw(0, I, HBitmap)
          // gsVertical_T
          else if GradinetStyle = gsVertical_T then
            Draw(I, -nVBmpHeight, VBitmap)
          // gsVertical_B
          else if GradinetStyle = gsVertical_B then
            Draw(I, 0, VBitmap)
          // gsArrow_R
          else if GradinetStyle = gsArrow_R then
          begin
            nDivY := CtlHeight div 2;
            if I < (nBitCount div 2) then
              Draw(I - nDivY, I, HBitmap)
            else
              Draw(-I + nDivY, I, HBitmap);
          end
          // gsArrow_L
          else if GradinetStyle = gsArrow_L then
          begin
            if I < (nBitCount div 2) then
              Draw(-I, I, HBitmap)
            else
            begin
              Inc(nX);
              Draw(-I + (nX * 2), I, HBitmap);
            end;
          end
          // gsArrow_B
          else if GradinetStyle = gsArrow_B then
          begin
            nDivX := CtlWidth div 2;
            if I < (nBitCount div 2) then
              Draw(I, I - nDivX, VBitmap)
            else
              Draw(I, -I + nDivX, VBitmap);
          end
          // gsArrow_T
          else if GradinetStyle = gsArrow_T then
          begin
            if I < (nBitCount div 2) then
              Draw(I, -I, VBitmap)
            else
            begin
              Inc(nX);
              Draw(I, -I + (nX * 2), VBitmap);
            end;
          end
          // gsDiagonal_L (Left)
          else if GradinetStyle = gsDiagonal_L then
          begin
            nStep := 0;
            nFaktor := (CtlWidth / 2) / 100;
            nStart := Round(nStep * nFaktor);
            nGradStep := (CtlWidth / CtlHeight) - ((nStep / 100) * (CtlWidth / CtlHeight));
            nStepSum := nStepSum + nGradStep;
            nX := (nStart + (CtlWidth * 2)) * (-1);
            nX := Round(nX - nStepSum);
            Draw(nX, I, HBitmap)
          end
          // gsDiagonal_R (Right)
          else if GradinetStyle = gsDiagonal_R then
          begin
            nStep := 200;
            nFaktor := (CtlWidth / 2) / 100;
            nStart := Round(nStep * nFaktor);
            nGradStep := (CtlWidth / CtlHeight) - ((nStep / 100) * (CtlWidth / CtlHeight));
            nStepSum := nStepSum + nGradStep;
            nX := (nStart + (CtlWidth * 2)) * (-1);
            nX := Round(nX - nStepSum);
            Draw(nX, I, HBitmap)
          end
        end;
      end
      // gsCustom
      else if GradinetStyle = gsRotation then
      begin
        if GradientRotation >= 0 then
          nBitCount := Rect.Bottom - Rect.Top
        else
          nBitCount := Rect.Right - Rect.Left;

        for I := 0 to nBitCount do
        begin
          if GradientRotation >= 0 then
          begin
            nStep := GradientRotation * 2;
            nFaktor := (CtlWidth / 2) / 100;
            nStart := Round(nStep * nFaktor);
            nGradStep := (CtlWidth / CtlHeight) - ((nStep / 100) * (CtlWidth / CtlHeight));
            nStepSum := nStepSum + nGradStep;
            nX := (nStart + (CtlWidth * 2)) * (-1);
            nX := Round(nX - nStepSum);
            Draw(nX, I, HBitmap);
          end
          else
          begin
            nStep := (GradientRotation * 2) * (-1);
            nFaktor := (CtlHeight / 2) / 100;
            nStart := Round(nStep * nFaktor);
            nGradStep := (CtlHeight / CtlWidth) - ((nStep / 100) * (CtlHeight / CtlWidth));
            nStepSum := nStepSum + nGradStep;
            nX := (nStart + (CtlHeight * 2)) * (-1);
            nX := Round(nX - nStepSum);
            Draw(I, nX, VBitmap);
          end;
        end;
      end;

      VBitmap.Free;
      HBitmap.Free;

      aRGBArray := nil;
      Pen.Width := SavPenWidth;
      Pen.Style := SavPenStyle;
      Pen.Color := SavPenColor;
    end;
  end;
end;


{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ zeichnet Caption }
procedure DrawCaption(
  Ctl : TControl;
  Canvas : TCanvas;
  var CaptionRect, WorkRect, ClientRect : TRect;
  Angle : TAngle;
  HDepth, SDepth : Integer;
  HDirection, SDirection : TDirection;
  HColor, SColor, CtlColor, DisabelColor, CtlFontColor, TransColor : TColor;
  Text : string;
  FontBmp : TBitmap;
  TextStyle : TTextStyle;
  Alignment : TTextAlign;
  FillStyle : TFillStyle;
  GlyphTransMode : TTransMode;
  GlyphAlignMode : TGlyphAlignMode;
  GlyphAlignment : TGlyphAlignment;
  GlyphEnabled : TGlyphEnabled;
  Glyph : TBitmap;
  GlyphXPos, GlyphYPos, GlyphSpace, TextXPos, TextYPos : Integer;
  Autosize, State, Antialiasing, Accel, Effect : Boolean;
  FCompType : TCompType);

type
  TOffsetDirection = (Off_X, Off_Y);

const
  Offset_Values : array[TDirection, TOffsetDirection] of -1..1 =
  ((0, 0), (0, -1), (0, 1), (-1, 0), (1, 0), (-1, -1), (1, -1), (-1, 1), (1, 1));

var
  xPos, yPos, nHeight, nWidth, nTmpSizeX, nAngle, nLines, Flags : Integer;
  xTextLength, yTextLength, xTextHeight, yTextHeight : Integer;
  NewGlyph_X, NewGlyph_Y : Integer;
  SavFontColor : TColor;
  Size : TSize;
  LogFont : TLogFont;
  PLeftTop, PCap : TPoint;
  Grad : Double;
  SRect, HRect : TRect;
  nError : Cardinal;
  tmpBmp : TBitmap;
  tmpStream : TStream;
begin
  SavFontColor := CtlFontColor;
  NewGlyph_X := 0;
  NewGlyph_Y := 0;
  PLeftTop.X := 0;
  PLeftTop.Y := 0;
  xTextLength := 0;
  yTextLength := 0;
  xTextHeight := 0;
  yTextHeight := 0;
  nAngle := 0;
  PCap.X := 0;
  PCap.Y := 0;
  nHeight := 0;
  nWidth := 0;

  // Text Flags
  Flags := DT_EXPANDTABS;
  if not Accel then
    Flags := Flags or DT_NOPREFIX;

  with Canvas do
  begin
    Brush.Style := bsClear;

    Size.cy := TextHeight(Text);
    Size.cx := TextWidth(Text);
    if Size.cy <= 0 then
      Size.cy := TextHeight('I');
    if Size.cx <= 0 then
      Size.cx := TextWidth('I');

    // set TextRect
    CaptionRect.Left := WorkRect.Left;
    CaptionRect.Top := WorkRect.Top;
    CaptionRect.Right := WorkRect.Left + Size.cx;
    CaptionRect.Bottom := WorkRect.Top + Size.cy;

    // ++++ RADIOBUTTON or CHECKBOX
    if (FCompType = ctRadiobutton) or (FCompType = ctCheckbox) then
    begin
      // draw glyphbox
      DrawGlyph(Ctl, Canvas, Glyph, GlyphAlignMode, GlyphAlignment, GlyphXPos, GlyphYPos,
        GlyphSpace, GlyphTransMode, TransColor, CaptionRect, ClientRect, Size, NewGlyph_X,
        NewGlyph_Y, GlyphEnabled);

      // do alignmet
      if Alignment = tgAuto then
      begin
        TextXPos := NewGlyph_X + Glyph.Width + 4;
        if CaptionRect.Bottom > Glyph.Height then
          TextYPos := NewGlyph_Y - ((CaptionRect.Bottom - Glyph.Height) div 2)
        else
          TextYPos := NewGlyph_Y + ((Glyph.Height - CaptionRect.Bottom) div 2)
      end;

      TextAlignment(CaptionRect, WorkRect, Alignment, Size, TextXPos, TextYPos);
    end
    // ++++ BUTTON ++++
    else if (FCompType = ctButton) then
    begin
      // do alignment
      if Alignment = tgAuto then
        Alignment := tgCenter;

      TextAlignment(CaptionRect, WorkRect, Alignment, Size, TextXPos, TextYPos);

      // draw glyph
      DrawGlyph(Ctl, Canvas, Glyph, GlyphAlignMode, GlyphAlignment, GlyphXPos, GlyphYPos,
        GlyphSpace, GlyphTransMode, TransColor, CaptionRect, ClientRect, Size, NewGlyph_X,
        NewGlyph_Y, GlyphEnabled);

      // Button state (down/up)
      if State then
      begin
        CaptionRect.Left := CaptionRect.Left + 1;
        CaptionRect.Top := CaptionRect.Top + 1;
        CaptionRect.Right := CaptionRect.Right + 1;
        CaptionRect.Bottom := CaptionRect.Bottom + 1;
      end;
    end
    // ++++ ADVANCEDLABEL ++++
    else
    begin
      if Alignment = tgAuto then
        Alignment := tgCenter;

      // do alignment
      if (Autosize = False) or (Angle <> 0) then
        TextAlignment(CaptionRect, WorkRect, Alignment, Size, TextXPos, TextYPos);
    end;


    // +++++ Angle +++++
    if Angle >= 360 then
      Angle := 0;

    if (Angle <> 0) then
    begin
      GetObject(Font.Handle, SizeOf(LogFont), Addr(LogFont));
      LogFont.lfEscapement := Angle * 10;
      LogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;
      if Antialiasing then
        LogFont.lfQuality := ANTIALIASED_QUALITY;

      Font.Handle := CreateFontIndirect(LogFont);

      if (Angle >= 1) and (Angle <= 90) then
      begin
        nAngle := Angle;
        Grad := (Pi * nAngle / 180);
        yTextLength := Round(sin(Grad) * (Size.cx div 2));
        xTextLength := -Round(cos(Grad) * (Size.cx div 2));
        xTextHeight := Round(sin(Grad) * (Size.cy div 2));
        yTextHeight := Round(cos(Grad) * (Size.cy div 2));
      end
      else if (Angle >= 91) and (Angle <= 180) then
      begin
        nAngle := Angle - 90;
        Grad := (Pi * nAngle / 180);
        yTextLength := Round(cos(Grad) * (Size.cx div 2));
        xTextLength := Round(sin(Grad) * (Size.cx div 2));
        xTextHeight := Round(cos(Grad) * (Size.cy div 2));
        yTextHeight := -Round(sin(Grad) * (Size.cy div 2));
      end
      else if (Angle >= 181) and (Angle <= 270) then
      begin
        nAngle := Angle - 180;
        Grad := (Pi * nAngle / 180);
        yTextLength := -Round(sin(Grad) * (Size.cx div 2));
        xTextLength := Round(cos(Grad) * (Size.cx div 2));
        xTextHeight := -Round(sin(Grad) * (Size.cy div 2));
        yTextHeight := -Round(cos(Grad) * (Size.cy div 2));
      end
      else if (Angle >= 271) and (Angle <= 359) then
      begin
        nAngle := Angle - 270;
        Grad := (Pi * nAngle / 180);
        yTextLength := -Round(cos(Grad) * (Size.cx div 2));
        xTextLength := -Round(sin(Grad) * (Size.cx div 2));
        xTextHeight := -Round(cos(Grad) * (Size.cy div 2));
        yTextHeight := Round(sin(Grad) * (Size.cy div 2));
      end;

      PLeftTop.X := CaptionRect.Left + (xTextLength - xTextHeight) + (Size.cx div 2);
      PLeftTop.Y := CaptionRect.Top + (yTextLength - yTextHeight) + (Size.cy div 2);
    end
    else
    begin
      GetObject(Font.Handle, SizeOf(LogFont), Addr(LogFont));
      LogFont.lfEscapement := 0;
      if Antialiasing then
        LogFont.lfQuality := ANTIALIASED_QUALITY;

      Font.Handle := CreateFontIndirect(LogFont);

      PLeftTop.X := CaptionRect.Left;
      PLeftTop.Y := CaptionRect.Top;
    end;



    if Autosize then
    begin
      // set the size of the control
      nWidth := Abs(Trunc(Size.cx * cos(nAngle * Pi / 180))) +
        Abs(Trunc(Size.cy * sin(nAngle * Pi / 180)));
      nHeight := Abs(Trunc(Size.cx * sin(nAngle * Pi / 180))) +
        Abs(Trunc(Size.cy * cos(nAngle * Pi / 180)));

      // if caption have Highlight or Shadow the size must correct
      CorrectSize(nWidth, nHeight, SDirection, HDirection, SDepth, HDepth);
      Ctl.Width := nWidth;
      Ctl.Height := nHeight;

      // if caption have Highlight or Shadow the captionposition must correct
      if Angle = 0 then
      begin
        CorrectCaptionPos(xPos, yPos, SDirection, HDirection, SDepth, HDepth);
        OffsetRect(CaptionRect, xPos, yPos);
        PLeftTop.X := CaptionRect.Left;
        PLeftTop.Y := CaptionRect.Top;
      end;
    end
    else
    begin
      nWidth := Ctl.Width;
      nHeight := Ctl.Height;
    end;

    // draw caption if effect is active
    if Effect and (not FontBmp.Empty) then
    begin
      tmpBmp := TBitmap.Create;
      try
        with tmpBmp do
        begin
          Width := Ctl.Width;
          Height := Ctl.Height;
          Canvas.Font.Handle := Font.Handle;
          if Angle = 0 then
            DrawText(Canvas.Handle, PChar(Text), Length(Text), CaptionRect, Flags)
          else
            Canvas.TextOut(PLeftTop.X, PLeftTop.Y, Text);
        end;
        BitBlt(FontBmp.Canvas.Handle, 0,0,FontBmp.Width, FontBmp.Height, tmpBmp.Canvas.Handle, 0,0, SRCPAINT );
      finally
        tmpBmp.Free;
      end;
      FontBmp.Transparent := True;
      FontBmp.TransparentColor := clWhite;
      Draw(0,0,FontBmp);
    end
    // draw caption with captionlayout
    else
    begin
      // +++++ Enable +++++
      if Ctl.Enabled = False then
      begin
        Font.Color := DisabelColor;
        if Angle = 0 then
          DrawText(Canvas.Handle, PChar(Text), Length(Text), CaptionRect, Flags)
        else
          TextOut(PLeftTop.X, PLeftTop.Y, Text);
      end
      // +++++ Highlight or Shadow +++++
      else
      begin
        if (TextStyle <> tsNone) then
        begin
          if (SDirection <> drNone) then
          begin
            // set Pos. for Rect-Shadow
            PCap.X := PLeftTop.X + (SDepth * Offset_Values[SDirection, Off_X]);
            PCap.Y := PLeftTop.Y + (SDepth * Offset_Values[SDirection, Off_Y]);
            // set Font-Color for Shadow
            Font.Color := SColor;
            // draw Shadow-Text
            if Angle = 0 then
            begin
              SRect.Left := PCap.X;
              SRect.Top := PCap.Y;
              SRect.Right := CaptionRect.Right + SDepth;
              SRect.Bottom := CaptionRect.Bottom + SDepth;
              DrawText(Canvas.Handle, PChar(Text), Length(Text), SRect, Flags);
            end
            else
              TextOut(PCap.X, PCap.Y, Text);
          end;

          if (HDirection <> drNone) then
          begin
            // set Pos. for Rect-Highlight
            PCap.X := PLeftTop.X + (HDepth * Offset_Values[HDirection, Off_X]);
            PCap.Y := PLeftTop.Y + (HDepth * Offset_Values[HDirection, Off_Y]);
            // set Font-Color for Highlight
            Font.Color := HColor;
            // draw Highlight-Text
            if Angle = 0 then
            begin
              HRect.Left := PCap.X;
              HRect.Top := PCap.Y;
              HRect.Right := CaptionRect.Right + HDepth;
              HRect.Bottom := CaptionRect.Bottom + HDepth;
              DrawText(Canvas.Handle, PChar(Text), Length(Text), HRect, Flags);
            end
            else
              TextOut(PCap.X, PCap.Y, Text);
          end;
        end;
        // set back saved Control-Font
        Font.Color := SavFontColor;

        // draw Control-Text
        if Angle = 0 then
          DrawText(Canvas.Handle, PChar(Text), Length(Text), CaptionRect, Flags)
        else
          TextOut(PLeftTop.X, PLeftTop.Y, Text);
      end;
    end;
    DeleteObject(Font.Handle);
  end;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure DrawNormal(Canvas : TCanvas; WorkRect : TRect; ContolColor : TColor);
begin
  with Canvas do
  begin
    Brush.Color := ContolColor;
    Brush.Style := bsSolid;
    FillRect(WorkRect);

    Brush.Style := bsClear;
  end;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure DrawTransparent(Control : TControl; Canvas : TCanvas; WorkRect : TRect);
begin
  DrawTransparentBmp(Control, Canvas);
  Canvas.CopyRect(WorkRect, Canvas, WorkRect);
  Canvas.Brush.Style := bsClear;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure DrawTransparentBmp(Control: TControl; Dest: TCanvas);
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
    GetViewportOrgEx(DC, Position);
    SetViewportOrgEx(DC, Position.X - Left, Position.Y - Top, nil);
    IntersectClipRect(DC, 0, 0, Parent.ClientWidth, Parent.ClientHeight);
    Parent.Perform(WM_ERASEBKGND, DC, 0);
    Parent.Perform(WM_PAINT, DC, 0);
    RestoreDC(DC, SaveIndex);
  end;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure DrawGlyph(Ctl : TControl; Canvas : TCanvas; Glyph : TBitmap; GlyphAlignMode : TGlyphAlignMode;
  GlyphAlignment : TGlyphAlignment; GlyphXPos, GlyphYPos, GlyphSpace : Integer;
  TransMode : TTransMode; TransColor : TColor; CaptionRect, ClientRect : TRect;
  FontSize : TSize; var NewGlyph_X, NewGlyph_Y : Integer; EnabledMode : TGlyphEnabled);
type 
  TRGBArray = array[0..32767] of TRGBTriple; 
  PRGBArray = ^TRGBArray;

var
  x, y, Gray: Integer; 
  Row: PRGBArray;
begin
  NewGlyph_X := 0;
  NewGlyph_X := 0;
  Glyph.Transparent := True;

  if Assigned(Glyph) then
  begin
    // get alignpos
    AlignPos(NewGlyph_X, NewGlyph_Y, GlyphXPos, GlyphYPos, GlyphSpace, CaptionRect,
      ClientRect, FontSize, Glyph, GlyphAlignMode, GlyphAlignment);

    with Glyph do
    begin
      if TransMode = tmCustom then
        TransparentColor := TransColor
      else
        TransparentColor := Glyph.Canvas.Pixels[0, Glyph.Height - 1];

      // Enabled Mode
      if Ctl.Enabled = False then
      begin
        if EnabledMode = geGray then
        begin
          PixelFormat := pf24Bit;
          for y := 0 to Height - 1 do
          begin
            Row := ScanLine[y];
            for x := 0 to Width - 1 do
            begin
              Gray := (Row[x].rgbtRed + Row[x].rgbtGreen + Row[x].rgbtBlue) div 3;
              Row[x].rgbtRed := Gray;
              Row[x].rgbtGreen := Gray;
              Row[x].rgbtBlue := Gray;
            end;
          end;
        end;
      end;
    end;

    // draw glyph
    Canvas.Draw(NewGlyph_X, NewGlyph_Y, Glyph);
  end;
end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TextAlignment(var CaptionRect : TRect; WorkRect : TRect; Alignment : TTextAlign; Size : TSize;
  TextXPos, TextYPos : Integer);
var
  nWorkWidth, nWorkHeight, nCaptionWidth, nCaptionHeight : Integer;
begin
    nWorkWidth := WorkRect.Right - WorkRect.Left;
    nWorkHeight := WorkRect.Bottom - WorkRect.Top;
    nCaptionWidth := CaptionRect.Right - CaptionRect.Left;
    nCaptionHeight := CaptionRect.Bottom - CaptionRect.Top;

    if (Alignment = tgCustom) or (Alignment = tgAuto) then
    begin
      CaptionRect.Left := TextXPos;
      CaptionRect.Top := TextYPos;
    end
    else if Alignment = tgCenter then
    begin
      CaptionRect.Left := WorkRect.Left + Round((nWorkWidth / 2) - (nCaptionWidth / 2));
      CaptionRect.Top := WorkRect.Top +  Round((nWorkHeight / 2) - (nCaptionHeight / 2));
    end
    else if Alignment = tgCenterLeft then
    begin
      CaptionRect.Left := WorkRect.Left;
      CaptionRect.Top := WorkRect.Top + Round((nWorkHeight / 2) - (nCaptionHeight / 2));
    end
    else if Alignment = tgCenterRight then
    begin
      CaptionRect.Left := (WorkRect.Right - nCaptionWidth);
      CaptionRect.Top := WorkRect.Top + Round((nWorkHeight / 2) - (nCaptionHeight / 2));
    end
    else if Alignment = tgCenterTop then
    begin
      CaptionRect.Left := WorkRect.Left + Round((nWorkWidth / 2) - (nCaptionWidth / 2));
      CaptionRect.Top := WorkRect.Top;
    end
    else if Alignment = tgCenterBottom then
    begin
      CaptionRect.Left := WorkRect.Left + Round((nWorkWidth / 2) - (nCaptionWidth / 2));
      CaptionRect.Top := (WorkRect.Bottom - nCaptionHeight);
    end
    else if Alignment = tgTopLeft then
    begin
      CaptionRect.Left := WorkRect.Left;
      CaptionRect.Top := WorkRect.Top;
    end
    else if Alignment = tgTopRight then
    begin
      CaptionRect.Left := (WorkRect.Right - nCaptionWidth);
      CaptionRect.Top := WorkRect.Top;
    end
    else if Alignment = tgBottomLeft then
    begin
      CaptionRect.Left := WorkRect.Left;
      CaptionRect.Top := (WorkRect.Bottom - nCaptionHeight);
    end
    else if Alignment = tgBottomRight then
    begin
      CaptionRect.Left := (WorkRect.Right - nCaptionWidth);
      CaptionRect.Top := (WorkRect.Bottom - nCaptionHeight);
    end;

    CaptionRect.Right := CaptionRect.Left + nCaptionWidth;
    CaptionRect.Bottom := CaptionRect.Top + nCaptionHeight;

end;

{ +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure AlignPos(var nGlyph_X, nGlyph_Y : Integer; const XPos, YPos, Space : Integer;
  ButtonRect, ClientRect : TRect;
  Size : TSize; Glyph : TBitmap; GlyphAlignMode : TGlyphAlignMode; GlyphAlignment : TGlyphAlignment);
begin
  // Align Mode "gmCaption"
    if GlyphAlignMode = gmCaption then
    begin
      if GlyphAlignment = gaLeft then
      begin
        nGlyph_X := ButtonRect.Left - Glyph.Width - Space;
        nGlyph_Y := ButtonRect.Top + Round((Size.cy / 2) - (Glyph.Height / 2));
      end
      else if GlyphAlignment = gaRight then
      begin
        nGlyph_X := ButtonRect.Left + Size.cx + Space;
        nGlyph_Y := ButtonRect.Top + Round((Size.cy / 2) - (Glyph.Height / 2));
      end
      else if GlyphAlignment = gaTop then
      begin
        nGlyph_X := ButtonRect.Left + Round((Size.cx / 2) - (Glyph.Width / 2));
        nGlyph_Y := ButtonRect.Top - Glyph.Height - Space;
      end
      else if GlyphAlignment = gaBottom then
      begin
        nGlyph_X := ButtonRect.Left + Round((Size.cx / 2) - (Glyph.Width / 2));
        nGlyph_Y := ButtonRect.Top + Size.cy + Space;
      end
      else if GlyphAlignment = gaCenter then
      begin
        nGlyph_X := ButtonRect.Left + Round((Size.cx / 2) - (Glyph.Width / 2));
        nGlyph_Y := ButtonRect.Top + Round((Size.cy / 2) - (Glyph.Height / 2));
      end;
    end
    // Align Mode "gmControl"
    else if GlyphAlignMode = gmControl then
    begin
      if GlyphAlignment = gaLeft then
      begin
        nGlyph_X := ClientRect.Left;
        nGlyph_Y := Round((ClientRect.Bottom / 2) - (Glyph.Height / 2));
      end
      else if GlyphAlignment = gaRight then
      begin
        nGlyph_X := ClientRect.Right - Glyph.Width;
        nGlyph_Y := Round((ClientRect.Bottom / 2) - (Glyph.Height / 2));
      end
      else if GlyphAlignment = gaTop then
      begin
        nGlyph_X := Round((ClientRect.Right / 2) - (Glyph.Width / 2));
        nGlyph_Y := ClientRect.Top;
      end
      else if GlyphAlignment = gaBottom then
      begin
        nGlyph_X := Round((ClientRect.Right / 2) - (Glyph.Width / 2));
        nGlyph_Y := ClientRect.Bottom - Glyph.Height;
      end
      else if GlyphAlignment = gaCenter then
      begin
        nGlyph_X := Round((ClientRect.Right / 2) - (Glyph.Width / 2));
        nGlyph_Y := Round((ClientRect.Bottom / 2) - (Glyph.Height / 2));
      end;
    end
    // Align Mode "gmCustom"
    else if GlyphAlignMode = gmCustom then
    begin
      nGlyph_X := XPos;
      nGlyph_Y := YPos;
    end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ correct control size, if autosize and if caption has shadow or highlight effect }
procedure CorrectSize(var nWidth, nHeight : Integer; SDirection, HDirection : TDirection;
  SDepth, HDepth : Integer);
var
  nHighlight_Y, nShadow_Y, nHighlight_X, nShadow_X, nComp_X, nComp_Y : Integer;

  { gibt den größeren der beiden Werte zurück }
  function GetLargerInt(nValue1, nValue2 : Integer) : Integer;
  begin
    if nValue1 < nValue2 then
      Result := nValue2
    else
      Result := nValue1;
  end;

begin
  if (SDirection <> drNone) or (HDirection <> drNone) then
  begin
    // WIDTH
    nHighlight_Y := 0;
    nShadow_Y := 0;
    // Shadow Y if Up
    if (SDirection = drUp) or (SDirection = drUpLeft) or (SDirection = drUpRight) then
      nShadow_Y := SDepth;
    // Highlight Y if Up
    if (HDirection = drUp) or (HDirection = drUpLeft) or (HDirection = drUpRight) then
      nHighlight_Y := HDepth;
    // Y-Correct-Value if Up
    nComp_Y := GetLargerInt(nShadow_Y, nHighlight_Y);

    nHighlight_Y := 0;
    nShadow_Y := 0;
    // Shadow Y wenn Down
    if (SDirection = drDown) or (SDirection = drDownLeft) or (SDirection = drDownRight) then
      nShadow_Y := SDepth;
    // Highlight Y wenn Down
    if (HDirection = drDown) or (HDirection = drDownLeft) or (HDirection = drDownRight) then
      nHighlight_Y := HDepth;

    // Y-Korrektur-Wert wenn xxDown
    nComp_Y := nComp_Y + GetLargerInt(nShadow_Y, nHighlight_Y);
    nHeight := nHeight + nComp_Y;


    // H Ö H E
    nHighlight_X := 0;
    nShadow_X := 0;
    // Shadow X wenn Left
    if (SDirection = drLeft) or (SDirection = drUpLeft) or (SDirection = drDownLeft) then
      nShadow_X := SDepth;
    // Highlight X wenn Left
    if (HDirection = drLeft) or (HDirection = drUpLeft) or (HDirection = drDownLeft) then
      nHighlight_X := HDepth;
    // X-Korrektur-Wert wenn xxLeft
    nComp_X := GetLargerInt(nShadow_X, nHighlight_X);


    nHighlight_X := 0;
    nShadow_X := 0;
    // Shadow X wenn Right
    if (SDirection = drRight) or (SDirection = drUpRight) or (SDirection = drDownRight) then
      nShadow_X := SDepth;
    // Highlight X wenn Right
    if (HDirection = drRight) or (HDirection = drUpRight) or (HDirection = drDownRight) then
      nHighlight_X := HDepth;

    // X-Korrektur-Wert wenn xxRight
    nComp_X := nComp_X + GetLargerInt(nShadow_X, nHighlight_X);
    nWidth := nWidth + nComp_X;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure CorrectCaptionPos(var xPos, yPos : Integer; SDirection, HDirection : TDirection;
  SDepth, HDepth : Integer);
var
  nHighlight_Y, nShadow_Y, nHighlight_X, nShadow_X : Integer;

  { gibt den größeren der beiden Werte zurück }
  function GetLargerInt(nValue1, nValue2 : Integer) : Integer;
  begin
    if nValue1 < nValue2 then
      Result := nValue2
    else
      Result := nValue1;
  end;

begin
  xPos := 0;
  yPos := 0;

  if (SDirection <> drNone) or (HDirection <> drNone) then
  begin
    // WIDTH
    nHighlight_Y := 0;
    nShadow_Y := 0;
    // Shadow Y if Up
    if (SDirection = drUp) or (SDirection = drUpLeft) or (SDirection = drUpRight) then
      nShadow_Y := SDepth;
    // Highlight Y if Up
    if (HDirection = drUp) or (HDirection = drUpLeft) or (HDirection = drUpRight) then
      nHighlight_Y := HDepth;
    // Y-Correct-Value if Up
    yPos := GetLargerInt(nShadow_Y, nHighlight_Y);

    // H Ö H E
    nHighlight_X := 0;
    nShadow_X := 0;
    // Shadow X wenn Left
    if (SDirection = drLeft) or (SDirection = drUpLeft) or (SDirection = drDownLeft) then
      nShadow_X := SDepth;
    // Highlight X wenn Left
    if (HDirection = drLeft) or (HDirection = drUpLeft) or (HDirection = drDownLeft) then
      nHighlight_X := HDepth;
    // X-Korrektur-Wert wenn xxLeft
    xPos := GetLargerInt(nShadow_X, nHighlight_X);
  end;
end;


procedure Dummy;
begin
end;


end.
