{
ATBinHex printing code.
File must be included in ATBinHex.pas.
}

function TATBinHex.PrinterCaption: AnsiString;
begin
  if FFileName <> '' then
    Result := MsgViewerCaption + ' - ' + SExtractFileName(FFileName)
  else
    Result := MsgViewerCaption;
end;

function TATBinHex.PrinterFooter(APageNumber: Integer): WideString;
begin
  if FFileName <> '' then
    Result := SExtractFileName(FFileName) + ' - ' + IntToStr(APageNumber)
  else
    Result := IntToStr(APageNumber);
end;


function PrinterPageWidth: Integer;
begin
  Result := Trunc(Printer.PageWidth / (GetDeviceCaps(Printer.Handle, LOGPIXELSX) / Screen.PixelsPerInch));
end;

function PrinterPageHeight: Integer;
begin
  Result := Trunc(Printer.PageHeight / (GetDeviceCaps(Printer.Handle, LOGPIXELSY) / Screen.PixelsPerInch));
end;


function TATBinHex.MarginsRectPx(
  ATargetWidth,
  ATargetHeight: Integer;
  ATargetPPIX,
  ATargetPPIY: Integer): TRect;
const
  cUnitIn = 2.54 * 10;
begin
  Result := Rect(
    Trunc(FMarginLeft / cUnitIn * ATargetPPIX),
    Trunc(FMarginTop / cUnitIn * ATargetPPIY),
    ATargetWidth - Trunc(FMarginRight / cUnitIn * ATargetPPIX),
    ATargetHeight - Trunc(FMarginBottom / cUnitIn * ATargetPPIY)
    );
end;

function TATBinHex.MarginsRectRealPx: TRect;
begin
  Result := MarginsRectPx(
    Printer.PageWidth,
    Printer.PageHeight,
    GetDeviceCaps(Printer.Handle, LOGPIXELSX),
    GetDeviceCaps(Printer.Handle, LOGPIXELSY));
end;


(*
//Old printing procedure: prints the same in Text/Binary/Hex modes
//and doesn't support Unicode modes at all.
procedure TATBinHex.PrintOld;
const
  BlockSize = 64 * 1024;
var
  Buffer: array[0 .. BlockSize - 1] of AnsiChar;
  PosStart, PosEnd: Int64;
  BytesRead: DWORD;
  SBuffer: AnsiString;
  LenAll, Len: Int64;
  f: TextFile;
begin
  Printer.Canvas.Font := Self.Font;
    //Not ActiveFont! We always use ANSI font here, because Windows
    //doesn't allow to print by OEM fonts in most cases
    //(it substitutes other font instead of a given one).

  if ACopies > 0 then
    Printer.Copies := ACopies
  else
    Printer.Copies := 1;

  if ACaption <> '' then
    Printer.Title := ACaption
  else
    Printer.Title := PrinterCaption;

  if ASelectionOnly then
  begin
    PosStart := FSelStart;
    PosEnd := FSelStart + FSelLength - 1;
  end
  else
  begin
    PosStart := 0;
    PosEnd := PosLast;
  end;

  try
    AssignPrn(f);
    Rewrite(f);
    try
      repeat
        if not ReadSource(PosStart, @Buffer, BlockSize, BytesRead) then
        begin
          MsgReadError;
          Exit
        end;

        LenAll := PosEnd - PosStart + 1;
        Len := LenAll;
        I64LimitMax(Len, BytesRead);
        SetString(SBuffer, Buffer, Len);

        //OEM decoding:
        if FEncoding = vencOEM then
          SBuffer := ToANSI(SBuffer);

        Write(f, SBuffer);

        Inc(PosStart, BlockSize);
      until (BytesRead < BlockSize) or (LenAll <= BytesRead);
    finally
      CloseFile(f);
    end;
  except
  end;
end;
*)


const
  cFooterColor = clWhite;

procedure TATBinHex.PrintTo(
  ACanvas: TCanvas; //ACanvas may be assigned only for Print Preview
  APageWidth,
  APageHeight: Integer;
  APrintRange: TPrintRange;
  AFromPage,
  AToPage: Integer);
var
  APageNumber: Integer;
  //
  procedure DrawFooter(ACanvas: TCanvas; const ARect: TRect);
  var
    AText: WideString;
    ALeft: Integer;
  begin
    AText := PrinterFooter(APageNumber);
    ACanvas.Brush.Color := cFooterColor;
    ACanvas.FillRect(ARect);
    ACanvas.Font.Assign(FontFooter);
    ALeft := (ARect.Right + ARect.Left - STextWidth(ACanvas, AText)) div 2;
    ILimitMin(ALeft, ARect.Left);
    STextOut(ACanvas, ALeft, ARect.Top, AText);
  end;
  //
  procedure DrawRect(ACanvas: TCanvas; const ARect: TRect);
  begin
    ACanvas.Brush.Style := bsClear;
    ACanvas.Pen.Color := clBlack;
    ACanvas.Pen.Style := psDash;
    ACanvas.Rectangle(ARect);
  end;
  //
  function FooterHeightSrc: Integer;
  var
    C: TCanvas;
  begin
    C := TCanvas.Create;
    try
      C.Handle := Self.Handle;
      C.Font.Assign(FontFooter);
      Result := Trunc(FontHeight(C) * 1.2);
    finally
      C.Free;
    end;
  end;
  //
var
  APosStart, APosEnd,
  APosOld, ASelStartOld, ASelLengthOld: Int64;
  AViewPageSize: Int64;
  AViewAtEnd: Boolean;
  ABitmap,
  ABitmapOld: TBitmap;
  ATextWrapOld,
  ATextNonPrintOld,
  ATextGutterOld: Boolean;
  AMarginsRect: TRect;
  ADrawWidth,
  ADrawHeight,
  AFooterHeightSrc,
  AFooterHeightReal: Integer;
  APrintPreview,
  APrintThisPage: Boolean;
begin
  if (APrintRange = prSelection) then
  begin
    APosStart := FSelStart;
    APosEnd := FSelStart + FSelLength - CharSize;
  end
  else
  begin
    APosStart := 0;
    APosEnd := PosLast;
  end;

  //Calc margins
  AMarginsRect := MarginsRectPx(
    APageWidth,
    APageHeight,
    Screen.PixelsPerInch,
    Screen.PixelsPerInch);

  ADrawWidth := AMarginsRect.Right - AMarginsRect.Left;
  ADrawHeight := AMarginsRect.Bottom - AMarginsRect.Top;
  AFooterHeightSrc := FooterHeightSrc;
  Dec(ADrawHeight, AFooterHeightSrc);

  //Initialize
  Enabled2 := False; //During printing even redraw is not allowed
                     //(it will break some properties)

  ABitmap := TBitmap.Create;
  ABitmap.PixelFormat := pf24bit;
  APageNumber := 1;

  //Prepare for printing:
  // - scroll to block start
  // - clear selection
  // - set WordWrap = True
  // - set NonPrintable = False
  // - set Gutter = False
  // - change active bitmap to working one

  APosOld := PosOffset;
  ASelStartOld := SelStart;
  ASelLengthOld := SelLength;
  ATextWrapOld := TextWrap;
  ATextNonPrintOld := TextNonPrintable;
  ATextGutterOld := TextGutter;
  ABitmapOld := FBitmap;

  PosOffset := APosStart;
  SetSelection(0, 0, False, False);
  FTextWrap := True;
  FTextNonPrintable := False;
  FTextGutter := False;
  FBitmap := ABitmap;

  try
    repeat
      DrawTo(
        ABitmap,
        ADrawWidth,
        ADrawHeight,
        nil, //AStringsObject not needed
        True, //APrintMode
        APosEnd, //AFinalPos
        FTextWidth,
        FTextWidthHex,
        FTextWidthUHex,
        AViewPageSize,
        AViewAtEnd
        );

      APrintPreview := Assigned(ACanvas);
      APrintThisPage :=
        (APrintRange <> prPageNums) or
        (APageNumber >= AFromPage);

      if APrintThisPage then
      begin //Drawing begin
        if APrintPreview then
        begin
          ACanvas.Brush.Style := bsSolid;
          ACanvas.Draw(
            AMarginsRect.Left,
            AMarginsRect.Top,
            ABitmap);

          DrawFooter(ACanvas, Rect(
            AMarginsRect.Left,
            AMarginsRect.Bottom - AFooterHeightSrc,
            AMarginsRect.Right,
            AMarginsRect.Bottom));

          DrawRect(ACanvas, AMarginsRect);

          Break; //Don't preview other pages
        end
        else
        begin
          AMarginsRect := MarginsRectRealPx;
          AFooterHeightReal := Trunc(AFooterHeightSrc *
            (GetDeviceCaps(Printer.Handle, LOGPIXELSY) / Screen.PixelsPerInch));

          Printer.Canvas.StretchDraw(Rect(
            AMarginsRect.Left,
            AMarginsRect.Top,
            AMarginsRect.Right,
            AMarginsRect.Bottom - AFooterHeightReal),
            ABitmap);

          DrawFooter(Printer.Canvas, Rect(
            AMarginsRect.Left,
            AMarginsRect.Bottom - AFooterHeightReal,
            AMarginsRect.Right,
            AMarginsRect.Bottom));

          //DrawRect(Printer.Canvas, AMarginsRect); //d
        end;
      end; //Drawing end

      //If EOF reached then stop
      if AViewAtEnd then Break;
      if not FFileOK then Break;

      //Move page down
      Inc(APageNumber);
      PosLineDown(AViewAtEnd, LinesNum(ABitmap));

      //If page is below the selection then stop
      case APrintRange of
        prPageNums:
          if (APageNumber > AToPage) then Break;
        else
          if (FViewPos > APosEnd) then Break;
      end;

      //Change printer page
      if APrintThisPage then
        if not APrintPreview then
          Printer.NewPage;
    until False;

  finally
    //Restore saved properties
    FBitmap := ABitmapOld;
    FTextWrap := ATextWrapOld;
    FTextNonPrintable := ATextNonPrintOld;
    FTextGutter := ATextGutterOld;
    SetSelection(ASelStartOld, ASelLengthOld, False, False);
    PosOffset := APosOld;

    //Finalize
    ABitmap.Free;
    Enabled2 := True;
  end;
end;


{$ifdef PREVIEW}
type
  TTextPreviewHelper = class
    FBinHex: TATBinHex;
    FPrintRange: TPrintRange;
    FFromPage: Integer;
    FToPage: Integer;
    procedure Callback(
      ACanvas: TCanvas;
      AOptPosition: TATPrintPosition;
      AOptFit: TATPrintFitMode;
      AOptFitSize: TFloatSize;
      const AOptMargins: TFloatRect;
      const AOptGamma: Extended;
      const AOptFooter: TATPrintFooter);
  end;

procedure TTextPreviewHelper.Callback;
begin
  FBinHex.PrintTo(
    ACanvas,
    PrinterPageWidth,
    PrinterPageHeight,
    FPrintRange,
    FFromPage,
    FToPage);
end;
{$endif}


procedure TATBinHex.Print(
  APrintRange: TPrintRange;
  AFromPage: Integer = 1;
  AToPage: Integer = MaxInt;
  ACopies: Integer = 1;
  const ACaption: AnsiString = '');
begin
  {$ifdef PREVIEW}
  with TTextPreviewHelper.Create do
    try
      FBinHex := Self;
      FPrintRange := APrintRange;
      FFromPage := AFromPage;
      FToPage := AToPage;
      if not ShowTextPreviewDialog(Callback, False) then Exit;
    finally
      Free;
    end;
  {$endif}

  if ACopies > 0 then
    Printer.Copies := ACopies
  else
    Printer.Copies := 1;

  if ACaption <> '' then
    Printer.Title := ACaption
  else
    Printer.Title := PrinterCaption;

  Printer.BeginDoc;
  PrintTo(
    nil,
    PrinterPageWidth,
    PrinterPageHeight,
    APrintRange,
    AFromPage,
    AToPage);
  Printer.EndDoc;
end;


procedure TATBinHex.PrintPreview;
begin
  {$ifdef PREVIEW}
  if (FSelLength <> 0) then
    Print(prSelection)
  else
    Print(prAllPages);
  {$endif}
end;
