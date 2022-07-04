//-----------------------
function TFormViewUV.GetImageBorderWidth: Integer;
begin
  Result := 0;
  if Assigned(Viewer.ImageBox) then
    with Viewer.ImageBox do
      Result := Width - ClientWidth;
end;

function TFormViewUV.GetImageBorderHeight: Integer;
begin
  Result := 0;
  if Assigned(Viewer.ImageBox) then
    with Viewer.ImageBox do
      Result := Height - ClientHeight;
end;

function TFormViewUV.GetImageWidthActual: Integer;
begin
  Result := 0;
  if Assigned(Viewer.ImageBox) then
    with Viewer.ImageBox do
    begin
      if ImageFitToWindow then
        Result := ImageWidth
      else
        Result := Image.Width;
    end;
end;

function TFormViewUV.GetImageHeightActual: Integer;
begin
  Result := 0;
  if Assigned(Viewer.ImageBox) then
    with Viewer.ImageBox do
    begin
      if ImageFitToWindow then
        Result := ImageHeight
      else
        Result := Image.Height;
    end;
end;

function TFormViewUV.GetImageWidthActual2: Integer;
begin
  Result := 0;
  if Assigned(Viewer.ImageBox) then
    with Viewer.ImageBox do
      Result := Image.Width;
end;

function TFormViewUV.GetImageHeightActual2: Integer;
begin
  Result := 0;
  if Assigned(Viewer.ImageBox) then
    with Viewer.ImageBox do
      Result := Image.Height;
end;


function TFormViewUV.GetImageScrollVisible: Boolean;
begin
  Result := False;
  if Assigned(Viewer.ImageBox) then
    with Viewer.ImageBox do
      Result := HorzScrollBar.Visible or VertScrollBar.Visible;
end;

procedure TFormViewUV.SetImageScrollVisible(AValue: Boolean);
begin
  if Assigned(Viewer.ImageBox) then
    with Viewer.ImageBox do
    begin
      HorzScrollBar.Visible := AValue;
      VertScrollBar.Visible := AValue;
    end;
end;

//---------------------------------------------------------------
procedure TFormViewUV.UpdateFitWindow(AUseOriginalImageSizes: boolean);

  function WidthIncrement: integer;
  begin
    Result:= 0;
  end;
  function HeightIncrement: integer;
  begin
    Result:= 0;
    if StatusBar1.Visible then
      Inc(Result, StatusBar1.Height);
    if Toolbar.Visible then
      Inc(Result, Toolbar.Height);
  end;

var
  ALeft, ATop, AWidth, AHeight: integer;
begin
  if (Viewer.Mode=vmodeMedia) and Viewer.IsImage then
    if MediaFitWindow then
      begin
      //Set window sizes
      ImageScrollVisible:= false;

      if AUseOriginalImageSizes then
        begin
        AWidth:= ImageWidthActual + ImageBorderWidth;
        AHeight:= ImageHeightActual + ImageBorderHeight;
        end
      else
        begin
        AWidth:= ImageWidthActual2 + ImageBorderWidth;
        AHeight:= ImageHeightActual2 + ImageBorderHeight;
        end;

      //Return back from Maximized and Full Screen states:
      ShowFullScreen:= false;
      WindowState:= wsNormal;

      ClientWidth:= AWidth + WidthIncrement;
      ClientHeight:= AHeight + HeightIncrement;

      ImageScrollVisible:= true;

      //Move window
      ALeft:= IMax(IMin(Left, Screen.WorkAreaWidth - Width), Screen.WorkAreaLeft);
      ATop:= IMax(IMin(Top, Screen.WorkAreaHeight- Height), Screen.WorkAreaTop);
      AWidth:= IMin(Width, Screen.WorkAreaWidth);
      AHeight:= IMin(Height, Screen.WorkAreaHeight);
      SetBounds(ALeft, ATop, AWidth, AHeight);
      end;
end;
