{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                              TZReportControl                           !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZReportControl.Create(aOwner: TComponent);
begin
  inherited;
  // inherited properties
  ControlStyle          := ControlStyle + [csOpaque];
  inherited ParentColor := False;
  inherited ParentFont  := True;
  inherited Color       := clControl;
  // new properties
  fFrame      := TZRFrame.Create(Self);
  Align       := zalNone;
  FontStyles  := [];
  Visible     := True;
  Enabled     := True;
  Width       := 8;
  Height      := 1;
end;

destructor TZReportControl.Destroy;
begin
  inherited;
  fFrame.Free;
end;

procedure TZReportControl.Draw;

  procedure DrawBackground;
  begin
    with Canvas do begin
      Brush.Style := bsSolid;
      Brush.Color := Color;
      FillRect(ClientRect);
    end;
  end;

  procedure DrawBorder;
  begin
    with Canvas do begin
      Brush.Style := bsClear;
      Pen  .Style := psSolid;
      Pen  .Width := 1;
      Pen  .Color := clControlBorder;
      with ClientRect do Rectangle(Left, Top, Right, Bottom);
    end;
  end;

begin
  DrawBackground;
  Frame.Draw;
  DrawBorder;
end;

procedure TZReportControl.Paint;
begin
  if (csDesigning in ComponentState) and (
     (Self is TZReport) or Assigned(Report) and not Report.Minimized) then Draw;
end;

procedure TZReportControl.SetName(const Value: TComponentName);
begin
  inherited;
  Notify(zopRename);
end;

procedure TZReportControl.Notify(Operation: TZROperation);
begin
  Perform(ZR_NOTIFY, Integer(Operation), Integer(Self));
end;

procedure TZReportControl.ZRNotify(var Message: TZRNotify);
begin
  if Assigned(Report) and not (csDestroying in Report.ComponentState) then Report.Dispatch(TMessage(Message));
end;

function TZReportControl.FramedWidth: Integer;
begin
  Result := Width;
  if Assigned(Frame) then Dec(Result, Frame.Width);
end;

function TZReportControl.FramedHeight: Integer;
begin
  Result := Height;
  if Assigned(Frame) then Dec(Result, Frame.Height);
end;

function TZReportControl.FramedClientRect: TRect;
var
  C: TPoint;
begin
  Result := inherited ClientRect;
  if Assigned(Frame) then begin
    C:= CharSize;
    Inc(Result.Left  , C.X * Frame.Left  );
    Inc(Result.Top   , C.Y * Frame.Top   );
    Dec(Result.Right , C.X * Frame.Right );
    Dec(Result.Bottom, C.Y * Frame.Bottom);
  end;
end;

function TZReportControl.CharSize: TPoint;
var
  TM: TTextMetric;
begin
  with Result do
    if Assigned(Report) and Assigned(Report.Parent) and
       GetTextMetrics(Report.Canvas.Handle, TM) then begin
      X:= TM.tmAveCharWidth;
      Y:= TM.tmHeight;
    end else begin
      X:=  8;   X:=  7;
      Y:= 16;   Y:= 14;
    end;
end;

function TZReportControl.GetBound(Index: Integer): Integer;
begin
  if IsPrinting then
    case Index of
      0  : Result := pLeft;
      1  : Result := pTop;
      2  : Result := pWidth;
      3  : Result := pHeight;
      4  : Result := pLeft + pWidth - 1;
      5  : Result := pTop + pHeight - 1;
      else Result := 0;
    end
  else
    case Index of
      0  : Result := fLeft;
      1  : Result := fTop;
      2  : Result := fWidth;
      3  : Result := fHeight;
      4  : Result := fLeft + fWidth - 1;
      5  : Result := fTop + fHeight - 1;
      else Result := 0;
    end;
end;

procedure TZReportControl.SetBound(Index: Integer; Value: Integer);
begin
  if Value <> GetBound(Index) then begin
    if IsPrinting then
      case Index of
        0: pLeft   := Value;
        1: pTop    := Value;
        2: if Value >= 0 then pWidth  := Value else pWidth  := 0;
        3: if Value >= 0 then pHeight := Value else pHeight := 0;
      end
    else begin
      case Index of
        0: fLeft   := Value;
        1: fTop    := Value;
        2: if Value > 0 then fWidth  := Value else fWidth  := 1;
        3: if Value > 0 then fHeight := Value else fHeight := 1;
      end;
      if (Self is TZReport) and TZReport(Self).Minimized then begin
        TZReport(Self).mWidth  := fWidth;
        TZReport(Self).mHeight := fHeight;
      end;
      UpdateBounds;
    end;
  end;
end;

procedure TZReportControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
// При изменении графических координат корректировать символьные
var
  CS: TPoint;
begin
  inherited;
  // Установаить символьные координаты в соответствии с графическими
  if Assigned(Report) and not IsUpdating then begin
    CS:= CharSize;
    if (Self is TZReport) then begin
      fLeft := inherited Left;
      fTop  := inherited Top;
      if not TZReport(Self).Minimized then begin
        fWidth  := inherited Width  div CS.X;
        fHeight := inherited Height div CS.Y;
      end;
    end else begin
      fLeft   := inherited Left   div CS.X;
      fTop    := inherited Top    div CS.Y;
      fWidth  := inherited Width  div CS.X;
      fHeight := inherited Height div CS.Y;
    end;
    UpdateBounds;
  end;
end;

procedure TZReportControl.UpdateBounds;
// Установаить графические координаты в соответствии с символьными
var
  CS: TPoint;
begin
  if Assigned(Report) and not IsUpdating and
     not (csDestroying in ComponentState) then try

    fIsUpdating:= True;
    AdjustBounds;
    if not IsPrinting then begin
      CS:= CharSize;
      if (Self is TZReport) then
         if TZReport(Self).Minimized then
          SetBounds(Left, Top, 32, 32)
        else
          SetBounds(Left, Top, Width * CS.X + 1, Height * CS.Y + 1)
      else
          SetBounds(Left  * CS.X    , Top    * CS.Y,
                    Width * CS.X + 1, Height * CS.Y + 1)
    end;
  finally;
    fIsUpdating:= False;
  end;
end;

procedure TZReportControl.SetParent(aParent: TWinControl);

  procedure CheckParent;
  begin
    if not (aParent is TZRCustomBand) then ZRError(szrParentReport, [ClassName]);
    if not (Self is TZRCustomBand) and (aParent is TZReport) then ZRError(szrParentBand, [ClassName]);
    if (Self is TZRCustomBand) and not (aParent is TZReport) then aParent := (aParent as TZRCustomController).Report;
  end;

  procedure NormalizeBounds;
  var
    CS: TPoint;
  begin
    CS := CharSize;
    if Self is TZReport then
      UpdateBounds
    else
      SetBounds(inherited Left, inherited Top, Width*CS.X, Height*CS.Y);
  end;

begin
  if Self is TZReport then
    inherited
  else begin
    if aParent <> nil then CheckParent;
    try
      fBand   := nil;
      fReport := nil;
      inherited;
    finally
      if Assigned(Parent) then begin
        fBand   := TZRCustomBand(Parent);
        fReport := TZRCustomBand(Parent).Report;
        NormalizeBounds;
      end;
      Perform(CM_FONTCHANGED, 0, 0);
    end;
  end;
end;

procedure TZReportControl.AdjustBounds;
begin
  if Assigned(Report) and not (csDestroying in ComponentState) then begin
    RequestBounds;
    AlignBounds;
  end;
end;

procedure TZReportControl.RequestBounds;
begin
  if IsPrinting then begin
    // Перед печатью runtime-изменения размеров игнорируем
    pLeft   := fLeft;
    pTop    := fTop;
    pWidth  := fWidth;
    pHeight := fHeight;
  end;
end;

procedure TZReportControl.AlignBounds;
begin
  if (Band = nil) or (Self is TZReport) then Exit;
  case Align of
    zalTop,
    zalBottom,
    zalWidth  : begin
                  Left  := Band.Frame.Left;
                  Width := Band.FramedWidth;
                  case Align of
                    zalTop    : Top := Band.Frame.Top;
                    zalBottom : Top := Band.Frame.Top + Band.FramedHeight - Height;
                  end
                end;
    zalLeft,
    zalRight,
    zalHeight : begin
                  Top    := Band.Frame.Top;
                  Height := Band.FramedHeight;
                  case Align of
                    zalLeft  : Left := Band.Frame.Left;
                    zalRight : Left := Band.Frame.Left + Band.FramedWidth - Width;
                  end;
                end;
  end;
end;

procedure TZReportControl.SetAlign(Value: TZRAlign);
begin
  if Align <> Value then begin
    fAlign:= Value;
    UpdateBounds;
  end;
end;

procedure TZReportControl.SetFontStyles(Value: TZRFontStyles);
begin
  if FontStyles <> Value then begin
    fFontStyles:= Value;
    Perform(CM_FONTCHANGED, 0, 0);
  end;
end;

procedure TZReportControl.SetFrame(Value: TZRFrame);
begin
  fFrame.Assign(Value);
end;

procedure TZReportControl.CMFontChanged(var Message: TMessage);
var
  zfs : TZRFontStyle;
begin
  inherited;
  if Assigned(Report) then begin
    Font:= Report.Font;
    Canvas.Font:= Font;
    with Canvas.Font do begin
      Style:= [];
      for zfs := Low(TZRFontStyle) to High(TZRFontStyle) do
        if zfs in FontStyles then Style := Style + ZRFontStyleMap[zfs];
    end;
    UpdateBounds;
    NotifyControls(CM_FONTCHANGED);
  end;
end;

procedure TZReportControl.Prepare;
begin
  fIsPrinting := True;
  pDoPrint    := Enabled;
end;

procedure TZReportControl.Unprepare;
begin
  fIsPrinting := False;
end;

procedure TZReportControl.DoBeforePrint;
begin
  pDoPrint := Enabled;
  if pDoPrint then
    try
      if not (csDesigning in ComponentState) and Assigned(BeforePrint) then
        BeforePrint(Self, pDoPrint)
      else
        pDoPrint := Visible;
    except
      pDoPrint := False;
      raise;                       // ???
    end;
end;

procedure TZReportControl.DoAfterPrint;
begin
  if pDoPrint and not (csDesigning in ComponentState) and Assigned(AfterPrint) then
    try
      AfterPrint(Self);
    except
      raise;                       // ???
    end;
end;

procedure TZReportControl.DoPrint(OfsX, OfsY: Integer);
begin
  if not (Self is TZRCustomBand) then begin
    Inc(OfsX, Left);
    Inc(OfsY, Top );
  end;
  Frame.Print(OfsX, OfsY, Width, Height);
end;

procedure TZReportControl.PrintString(X, Y: Integer; const S: String);
begin
  with Report.Printer do
    Line[Y] := EscapeStuff(EscapeCopy(S, 1, Report.PageWidth-X), Line[Y], Succ(X));
end;


