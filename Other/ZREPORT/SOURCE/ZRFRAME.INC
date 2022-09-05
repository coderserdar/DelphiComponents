{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}
{!!!                                 TZRFrame                               !!!}
{!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!}

constructor TZRFrame.Create(aOwner: TZReportControl);
begin
  inherited Create;
  fOwner:= aOwner;
end;

function TZRFrame.GetValue(Index: Integer): Integer;
begin
  case Index of
    4  : Result:= Right + Left;
    5  : Result:= Bottom + Top;
    else Result:= fValues[Index];
  end;
end;

procedure TZRFrame.SetValue(Index: Integer; Value: Integer);
var
  DX, DY, i: Integer;
begin
  if fValues[Index] <> Value then begin
    if Index in [0,2] then DX:= Value - fValues[Index] else DX:= 0;
    if Index in [1,3] then DY:= Value - fValues[Index] else DY:= 0;
    fValues[Index]:= Value;
    if (csDesigning in Owner.ComponentState) and not
       (csLoading   in Owner.ComponentState) then begin
      Owner.Width  := Owner.Width  + DX;
      Owner.Height := Owner.Height + DY;
      if Index < 2 then
        for i:=0 to Owner.ControlCount-1 do
          if (Owner.Controls[i] is TZReportControl) then
            with (Owner.Controls[i] as TZReportControl) do begin
              Left:= Left + DX;
              Top := Top  + DY;
            end;
      Owner.UpdateBounds;
      Owner.Invalidate;
    end;
  end;
end;

procedure TZRFrame.Assign(Source: TPersistent);
begin
  if Source is TZRFrame then
    fValues:= TZRFrame(Source).fValues
  else
    inherited;
end;

procedure TZRFrame.Draw;
var
  R: TRect;
  C: TPoint;
begin
  R := Owner.ClientRect;
  C := Owner.CharSize;
  if Left   > 0 then Inc(R.Left  , {Left   * C.X - }C.X div 2);
  if Top    > 0 then Inc(R.Top   , {Top    * C.Y - }C.Y div 2);
  if Right  > 0 then Dec(R.Right , {Right  * C.X - }C.X div 2);
  if Bottom > 0 then Dec(R.Bottom, {Bottom * C.Y - }C.Y div 2);
  with Owner.Canvas do begin
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    Pen.Width := 2;
    Pen.Color := clFrame;
    with R do Rectangle(Left+1, Top+1, Right, Bottom);
  end;
end;

procedure TZRFrame.Print(aLeft, aTop, aWidth, aHeight: Integer);
type
  TZRFrameCorner = (zfcHorizontal, zfcVertical, zfcCross,
                    zfcTopLeft, zfcTopRight,
                    zfcBottomLeft, zfcBottomRight);
const
  Corners : array[TZRFrameCorner] of Char = (
  '-', '|', '+', '+', '+', '+', '+');
var
  Y      : Integer;
  aRight : Integer;
  S      : String;
  LimitW,
  LimitH : Integer;
begin
{  if Owner is TZRCustomBand then
    aBand := TZRCustomBand(Owner)
  else
    aBand := Owner.Band;}
  if Owner is TZRCustomBand then begin
    LimitW := aWidth;
    LimitH := aHeight;
  end else with Owner do begin 
    LimitW := Min(Band.Width -Band.Frame.Right -Left, aWidth );
    LimitH := Min(Band.Height-Band.Frame.Bottom-Top , aHeight);
  end;
  if (LimitW = 0) or (LimitH = 0) then Exit;
  Y := 0;
  aRight := aLeft + aWidth - 1;
  S := Replicate(Corners[zfcHorizontal], aWidth);
  if (Top > 0) then begin
    if (Left  > 0) then S[     1] := Corners[zfcTopLeft ];
    if (Right > 0) then S[aWidth] := Corners[zfcTopRight];
    Inc(Y);
    Owner.PrintString(aLeft, aTop, copy(S, 1, LimitW));
  end;
  if (Bottom > 0) then begin
    if (Left  > 0) then S[     1] := Corners[zfcBottomLeft ];
    if (Right > 0) then S[aWidth] := Corners[zfcBottomRight];
    if LimitH = aHeight then
      Owner.PrintString(aLeft, aTop + aHeight - 1, copy(S, 1, LimitW));
    Dec(aHeight);
  end else
    aHeight := LimitH;
  while Y < aHeight do begin
    if Left  > 0 then Owner.PrintString(aLeft , aTop+Y, Corners[zfcVertical]);
    if (Right > 0) and (LimitW = aWidth) then Owner.PrintString(aRight, aTop+Y, Corners[zfcVertical]);
    Inc(Y);
  end;
end;


