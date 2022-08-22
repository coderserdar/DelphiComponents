// Author - Dmitry Belyaev
unit aceColorSelector;
{$I sDefs.inc}

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons,
  {$IFDEF DELPHI6UP} Variants, {$ENDIF}
  sDialogs, sSpeedButton;


type
  TaceColorSelectEvent = procedure(Sender: TObject; Color: TColor) of object;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TacColorSelector = class(TsSpeedButton)
  private
    FDropDownForm: TForm;
    FColorRowCount: Integer;
    FUserColorRowCount: Integer;
    FColorIndex: Integer;
    FUserColorIndex: Integer;
    FMoreButtonActive: Boolean;
    FColors: TStrings;
    FUserColors: TStrings;
    FColorDialog: TsColorDialog;  //georges
    FDropDownButton: Boolean;
    FSelectedColor: TColor;
    FOnColorSelect: TaceColorSelectEvent;
    FColumnCount: Integer;
    FShowColorButtonsHint: Boolean;
    FColorBtnSize : integer;   //georges
    FColorBtnSpace : integer;   //georges
    FMoreBtnSize :Integer;  //georges
    procedure DropDownFormShow(Sender: TObject);
    procedure DropDownFormDeactivate(Sender: TObject);
    procedure DropDownFormPaint(Sender: TObject);
    procedure DropDownFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DropDownFormClick(Sender: TObject);
    procedure SetColorBtnSize (Value : integer);  //georges
    procedure SetColorBtnSpace (Value : integer);  //georges
    procedure SetMoreBtnSize (Value : integer);   //georges
    procedure SetColors(const Value: TStrings);
    procedure SetUserColors(const Value: TStrings);
    procedure SetDropDownButton(const Value: Boolean);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetColumnCount(const Value: Integer);
    function EvaluteRowCount(Count: Integer): Integer;
    function GetColorBtnSize : integer;  //georges
    function GetColorBtnSpace : integer;  //georges
    function GetMoreBtnSize : integer;   //georges
    function GetColor(Index: Integer): TColor;
    function GetUserColor(Index: Integer): TColor;
    function GetColorHint(Index: Integer): string;
    function GetUserColorHint(Index: Integer): string;
    procedure GetPosition(Index: Integer; var Row, Col: Integer);
    procedure DropDown;
    procedure DrawColorButtonsHint(Text: string);
    procedure DrawColorButton(dY, Row, Col: Integer; Color: TColor; Selected: Boolean);
    procedure DrawMoreButton(Selected: Boolean);
    function ColorExist(Color: TColor): Boolean;
  protected
    function ColorRect: TRect;
    function PrepareCache: boolean; override;
    procedure Paint; override;
    procedure DoColorSelectEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
  published
    property Colors: TStrings read FColors write SetColors;
    property UserColors: TStrings read FUserColors write SetUserColors;
    property DropDownButton: Boolean read FDropDownButton write SetDropDownButton default False;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clBlack;
    property ColumnCount: Integer read FColumnCount write SetColumnCount default 8;
    property ShowColorButtonsHint: Boolean read FShowColorButtonsHint write FShowColorButtonsHint default False;
    property OnColorSelect: TaceColorSelectEvent read FOnColorSelect write FOnColorSelect;
    property DropDownForm : TForm read FDropDownForm write FDropDownForm; //georges
    property ColorBtnSize : integer read GetColorBtnSize write SetColorBtnSize; //georges
    property ColorBtnSpace : integer read GetColorBtnSpace write SetColorBtnSpace; //georges
    property MoreBtnSize : integer read GetMoreBtnSize write SetMoreBtnSize; //georges
  end;


implementation

uses
  Math,
  sGraphUtils, sConst, sCommonData;


type
  TaceVerticalAlignment = (vaTop, vaMiddle, vaBottom);


//const     //georges
//  ColorBtnSize = 32;
//  MoreBtnSize = 40;


var
  TextAlignments: array [0..2] of ShortInt = (DT_LEFT, DT_RIGHT, DT_CENTER);
  ScrRatio : integer;  //georges


procedure DrawTextAligned(const Text: string; Canvas: TCanvas; var Rect: TRect; Alignment: TAlignment; VerticalAlignment: TaceVerticalAlignment; WordWrap: Boolean);
var
  P: array [0..$FF] of Char;
  H: Integer;
  T: TRect;
  F: Word;
begin
  if Alignment = taRightJustify then
    StrPCopy(P, Text + ' ')
  else
    StrPCopy(P, Text);

  Canvas.FillRect(Rect);
  Canvas.Font.Size := 8*ScrRatio;  //georges
  Rect.Top := Rect.Top + 2;
  Rect.Left := Rect.Left + 2;
  Rect.Right := Rect.Right - 2;
  Rect.Bottom := Rect.Bottom - 2;
  T := Rect;
  Canvas.FillRect(T);
  F := DT_CALCRECT or DT_EXPANDTABS or DT_VCENTER or TextAlignments[Integer(Alignment)];
  if WordWrap then
    F := F or DT_WORDBREAK;

  H := DrawText(Canvas.Handle, P, -1, T, F);
  H := Min(H, Rect.Bottom - Rect.Top);
  if VerticalAlignment = vaMiddle then begin
    Rect.Top := ((Rect.Bottom + Rect.Top) - H) div 2;
    Rect.Bottom := Rect.Top + H;
  end
  else
    if VerticalAlignment = vaBottom then
      Rect.Top := Rect.Bottom - H - 1;
      
  F := DT_EXPANDTABS or DT_VCENTER or TextAlignments[Integer(Alignment)];
  if WordWrap then
    F := F or DT_WORDBREAK;

    
  DrawText(Canvas.Handle, P, -1, Rect, F);
end;


function RGBToString(Color: TColor): string;
begin
  Result := 'R: ' + IntToStr(GetRValue(Color)) + '; G: ' + IntToStr(GetGValue(Color)) + '; B: ' + IntToStr(GetBValue(Color));
end;


procedure TacColorSelector.Click;
var
  DoDropDown: Boolean;
begin
  if FDropDownButton then
    DoDropDown := ScreenToClient(Mouse.CursorPos).X >= ClientWidth - 10
  else
    DoDropDown := True;

  if DoDropDown then
    DropDown
  else
    DoColorSelectEvent;

  inherited;
end;


function TacColorSelector.ColorExist(Color: TColor): Boolean;
var
  i: Integer;
  s: string;
begin
  Result := False;
  s := ColorToString(Color);
  for i := 0 to FColors.Count - 1 do
    if Pos(s, FColors[i]) = 1 then begin
      Result := True;
      Exit;
    end;

  for i := 0 to FUserColors.Count - 1 do
    if Pos(s, FUserColors[i]) = 1 then begin
      Result := True;
      Exit;
    end;
end;


resourcestring
  scBlack   = 'scBlack';
  scNavy    = 'scNavy';
  scMaroon  = 'scMaroon';
  scOlive   = 'scOlive';
  scGreen   = 'scGreen';
  scTeal    = 'scTeal';
  scBlue    = 'scBlue';
  scGray    = 'scGray';
  scRed     = 'scRed';
  scPurple  = 'scPurple';
  scFuchsia = 'scFuchsia';
  scYellow  = 'scYellow';
  scLime    = 'scLime';
  scAqua    = 'scAqua';
  scSilver  = 'scSilver';
  scWhite   = 'scWhite';
  

constructor TacColorSelector.Create(AOwner: TComponent);
begin
  inherited;
  FDropDownForm := TForm.Create(nil);
  FDropDownForm.BorderStyle := bsNone;
  FDropDownForm.Scaled := TForm(GetParentForm(TControl(AOwner))).Scaled;
  FDropDownForm.Position := poDesigned;
  FDropDownForm.OnShow := DropDownFormShow;
  FDropDownForm.OnDeactivate := DropDownFormDeactivate;
  FDropDownForm.OnPaint := DropDownFormPaint;
  FDropDownForm.OnMouseMove := DropDownFormMouseMove;
  FDropDownForm.OnClick := DropDownFormClick;
  FColorDialog := TsColorDialog.Create(nil); //georges
  FColorDialog.Options := [cdFullOpen];
  FDropDownButton := False;
  FSelectedColor := clBlack;
  FColumnCount := 8;
  FShowColorButtonsHint := False;
  FColors := TStringList.Create;
  FColors.Add(ColorToString(clBlack)  + '|' + scBlack);
  FColors.Add(ColorToString($00003399));
  FColors.Add(ColorToString($00003333));
  FColors.Add(ColorToString($00003300));
  FColors.Add(ColorToString($00663300));
  FColors.Add(ColorToString(clNavy)   + '|' + scNavy);
  FColors.Add(ColorToString($00353333));
  FColors.Add(ColorToString($00333333));
  FColors.Add(ColorToString(clMaroon) + '|' + scMaroon);
  FColors.Add(ColorToString($000066FF));
  FColors.Add(ColorToString(clOlive)  + '|' + scOlive);
  FColors.Add(ColorToString(clGreen)  + '|' + scGreen);
  FColors.Add(ColorToString(clTeal)   + '|' + scTeal);
  FColors.Add(ColorToString(clBlue)   + '|' + scBlue);
  FColors.Add(ColorToString($00996666));
  FColors.Add(ColorToString(clGray)   + '|' + scGray);
  FColors.Add(ColorToString(clRed)    + '|' + scRed);
  FColors.Add(ColorToString($000099FF));
  FColors.Add(ColorToString($0000CC99));
  FColors.Add(ColorToString($00669933));
  FColors.Add(ColorToString($00CCCC33));
  FColors.Add(ColorToString($00FF6633));
  FColors.Add(ColorToString(clPurple) + '|' + scPurple);
  FColors.Add(ColorToString($00999999));
  FColors.Add(ColorToString(clFuchsia)+ '|' + scFuchsia);
  FColors.Add(ColorToString($0000CCFF));
  FColors.Add(ColorToString(clYellow) + '|' + scYellow);
  FColors.Add(ColorToString(clLime)   + '|' + scLime);
  FColors.Add(ColorToString(clAqua)   + '|' + scAqua);
  FColors.Add(ColorToString($00FFCC00));
  FColors.Add(ColorToString($00663399));
  FColors.Add(ColorToString(clSilver) + '|' + scSilver);
  FColors.Add(ColorToString($00CC99FF));
  FColors.Add(ColorToString($0099CCFF));
  FColors.Add(ColorToString($0099FFFF));
  FColors.Add(ColorToString($00CCFFCC));
  FColors.Add(ColorToString($00FFFFCC));
  FColors.Add(ColorToString($00FFCC99));
  FColors.Add(ColorToString($00FF99CC));
  FColors.Add(ColorToString(clWhite)  + '|' + scWhite);
  FUserColors := TStringList.Create;

  FColorBtnSize := 16;  //georges
  FColorBtnSpace := 3;  //georges
  FMoreBtnSize := 20;   //georges
  ScrRatio := 1;        //georges
end;


destructor TacColorSelector.Destroy;
begin
  FUserColors.Free;
  FColors.Free;
  FColorDialog.Free;
  FDropDownForm.Free;
  inherited;
end;


procedure TacColorSelector.DoColorSelectEvent;
begin
  if Assigned(FOnColorSelect) then
    FOnColorSelect(Self, FSelectedColor);
end;

procedure TacColorSelector.DrawColorButton(dY, Row, Col: Integer; Color: TColor; Selected: Boolean);
var
  r: TRect;
begin
  r.Left := Col * ColorBtnSize + 2;
  r.Top := Row * ColorBtnSize + dY;
  r.Right := r.Left + ColorBtnSize;
  r.Bottom := r.Top + ColorBtnSize;
  FDropDownForm.Canvas.Brush.Style := bsSolid;
  FDropDownForm.Canvas.Brush.Color := FDropDownForm.Color;
  FDropDownForm.Canvas.FillRect(r);
  if Selected then begin
    FDropDownForm.Canvas.Pen.Color := clNavy;
    FDropDownForm.Canvas.RoundRect(r.Left, r.Top, r.Right, r.Bottom, 3, 3);
  end;
  r.Left := r.Left + FColorBtnSpace;   //georges
  r.Top := r.Top + FColorBtnSpace;     //georges
  r.Right := r.Right - FColorBtnSpace;  //georges
  r.Bottom := r.Bottom - FColorBtnSpace;  //georges
  FDropDownForm.Canvas.Pen.Color := clGray;
  FDropDownForm.Canvas.Brush.Color := Color;
  FDropDownForm.Canvas.Rectangle(r);
end;


procedure TacColorSelector.DrawColorButtonsHint(Text: string);
var
  r: TRect;
begin
  if not FShowColorButtonsHint then
    Exit;

  r.Left := 2;
  r.Top := 2;
  r.Right := FDropDownForm.ClientWidth - 2;
  r.Bottom := r.Top + ColorBtnSize;
  FDropDownForm.Canvas.Brush.Color := FDropDownForm.Color;
  DrawTextAligned(Text, FDropDownForm.Canvas, r, taCenter, vaMiddle, False);
end;


procedure TacColorSelector.DrawMoreButton(Selected: Boolean);
var
  r: TRect;
begin
  r.Left := 2;
  r.Top := FDropDownForm.ClientHeight - MoreBtnSize - 2;
  r.Right := FDropDownForm.ClientWidth - 2;
  r.Bottom := r.Top + MoreBtnSize;
  FDropDownForm.Canvas.Brush.Style := bsSolid;
  FDropDownForm.Canvas.Brush.Color := FDropDownForm.Color;
  FDropDownForm.Canvas.FillRect(r);
  if Selected then begin
    FDropDownForm.Canvas.Pen.Color := clNavy;
    FDropDownForm.Canvas.RoundRect(r.Left, r.Top, r.Right, r.Bottom, 3, 3);
  end;
  r.Left := r.Left + 2;
  r.Top := r.Top + 1;
  r.Right := r.Right - 2;
  r.Bottom := r.Bottom - 1;
  DrawTextAligned('More colors', FDropDownForm.Canvas, r, taCenter, vaMiddle, False);  //georges
end;


procedure TacColorSelector.DropDown;
var
  P: TPoint;
  h: Integer;
begin
  FColorIndex := -1;
  FUserColorIndex := -1;
  FMoreButtonActive := False;
//  FDropDownForm.Width := ColorBtnSize * FColumnCount + 4;   //georges see below
  FColorRowCount := EvaluteRowCount(FColors.Count);
  h := ColorBtnSize * FColorRowCount;
  if FShowColorButtonsHint then
    Inc(h, ColorBtnSize);

  if FUserColors.Count > 0 then begin
    FUserColorRowCount := EvaluteRowCount(FUserColors.Count);
    Inc(h, ColorBtnSize * FUserColorRowCount + 5);
  end;
//  FDropDownForm.Height := h + MoreBtnSize + 4; //georges see below
  P := ClientToScreen(Point(0, Height));
  FDropDownForm.Left := P.X;
  FDropDownForm.Top := P.Y;
  FDropDownForm.Width := ColorBtnSize * FColumnCount + 4; //georges
  FDropDownForm.Height := h + MoreBtnSize + 4; //georges
  if not FDropDownForm.Visible then
    FDropDownForm.Show;
end;


procedure TacColorSelector.DropDownFormClick(Sender: TObject);
begin
  if FMoreButtonActive then begin
    FDropDownForm.OnDeactivate := nil;
    if FColorDialog.Execute then begin
      SelectedColor := FColorDialog.Color;
      if not ColorExist(FSelectedColor) then
        FUserColors.Add(ColorToString(FSelectedColor));

      FDropDownForm.Hide;
      DoColorSelectEvent;
    end;
    FDropDownForm.OnDeactivate := DropDownFormDeactivate;
  end
  else begin
    if FColorIndex <> -1 then
      SelectedColor := GetColor(FColorIndex)
    else
      if FUserColorIndex <> -1 then
        SelectedColor := GetUserColor(FUserColorIndex);

    DoColorSelectEvent;
    FDropDownForm.Hide;
  end;
end;


procedure TacColorSelector.DropDownFormDeactivate(Sender: TObject);
begin
  if FDropDownForm.Visible then
    FDropDownForm.Hide;
end;


procedure TacColorSelector.DropDownFormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  r, c: Integer;
  LineY: Integer;

  procedure SetColorIndex(const Value: Integer);
  var
    ar, ac: Integer;
  begin
    if FColorIndex <> Value then begin
      if FColorIndex <> -1 then begin
        GetPosition(FColorIndex, ar, ac);
        if FShowColorButtonsHint then
          DrawColorButton(ColorBtnSize + 2, ar, ac, GetColor(FColorIndex), False)
        else
          DrawColorButton(2, ar, ac, GetColor(FColorIndex), False);
      end;
      if Value >= FColors.Count then
        FColorIndex := -1
      else
        FColorIndex := Value;

      if FColorIndex <> -1 then begin
        if FShowColorButtonsHint then
          DrawColorButton(ColorBtnSize + 2, r, c, GetColor(FColorIndex), True)
        else
          DrawColorButton(2, r, c, GetColor(FColorIndex), True);

        DrawColorButtonsHint(GetColorHint(FColorIndex));
      end
      else
        DrawColorButtonsHint('');
    end;
  end;

  procedure SetUserColorIndex(const Value: Integer);
  var
    ar, ac: Integer;
  begin
    if FUserColorIndex <> Value then begin
      if FUserColorIndex <> -1 then begin
        GetPosition(FUserColorIndex, ar, ac);
        DrawColorButton(LineY, ar, ac, GetUserColor(FUserColorIndex), False);
      end;
      if Value >= FUserColors.Count then
        FUserColorIndex := -1
      else
        FUserColorIndex := Value;

      if FUserColorIndex <> -1 then begin
        DrawColorButton(LineY, r, c, GetUserColor(FUserColorIndex), True);
        DrawColorButtonsHint(GetUserColorHint(FUserColorIndex));
      end
      else
        DrawColorButtonsHint('');
    end;
  end;

begin
  if (X < 2) or (X > FDropDownForm.ClientWidth - 3) then
    Exit;

  if FShowColorButtonsHint then
    begin
    if Y < 2 + ColorBtnSize then
      Exit;
    end
  else
    if Y < 2 then Exit;

  if Y > FDropDownForm.ClientHeight - 3 then
    Exit;

  c := (X - 2) div ColorBtnSize;
  LineY := FDropDownForm.ClientHeight - MoreBtnSize;
  if FUserColors.Count > 0 then
    Dec(LineY, FUserColorRowCount * ColorBtnSize + 2);

  if Y < LineY then
    begin
    if FShowColorButtonsHint then
      r := (Y - 2 - ColorBtnSize) div ColorBtnSize
    else
      r := (Y - 2) div ColorBtnSize;

    SetColorIndex(r * FColumnCount + c);
    SetUserColorIndex(-1);
    FDropDownForm.Hint := GetColorHint(FColorIndex);
    if FMoreButtonActive then
      begin
      FMoreButtonActive := False;
      DrawMoreButton(False);
      end;
    end
  else
    if Y < FDropDownForm.ClientHeight - MoreBtnSize then
      begin
      r := (Y - LineY - 2) div ColorBtnSize;
      SetUserColorIndex(r * FColumnCount + c);
      SetColorIndex(-1);
      FDropDownForm.Hint := GetUserColorHint(FUserColorIndex);
      if FMoreButtonActive then
        begin
        FMoreButtonActive := False;
        DrawMoreButton(False);
        end
      end
    else
      begin
      SetColorIndex(-1);
      SetUserColorIndex(-1);
      FDropDownForm.Hint := '';
      if not FMoreButtonActive then
        begin
        FMoreButtonActive := True;
        DrawMoreButton(True);
        end;
      end;
end;


procedure TacColorSelector.DropDownFormPaint(Sender: TObject);
var
  r, c, i, dY: Integer;
begin
  FDropDownForm.Canvas.Pen.Color := clNavy;
  FDropDownForm.Canvas.Brush.Color := clNone;
  FDropDownForm.Canvas.Brush.Style := bsClear;
  FDropDownForm.Canvas.Rectangle(FDropDownForm.ClientRect);
  dY := 2;
  if FShowColorButtonsHint then
    Inc(dY, ColorBtnSize);

  for i := 0 to FColors.Count - 1 do
    begin
    GetPosition(i, r, c);
    DrawColorButton(dY, r, c, GetColor(i), False);
    end;
  if FUserColors.Count > 0 then
    begin
    FDropDownForm.Canvas.Pen.Color := clNavy;
    dY := 2 + (r + 1) * ColorBtnSize + 2;
    if FShowColorButtonsHint then
      Inc(dY, ColorBtnSize);

    FDropDownForm.Canvas.MoveTo(4, dY);
    FDropDownForm.Canvas.LineTo(FDropDownForm.ClientWidth - 4, Dy);
    Inc(dY, 3);
    for i := 0 to FUserColors.Count - 1 do
      begin
      GetPosition(i, r, c);
      DrawColorButton(dY, r, c, GetUserColor(i), False);
      end;
    end;

  DrawMoreButton(False);
end;


procedure TacColorSelector.DropDownFormShow(Sender: TObject);
begin
  FDropDownForm.SetFocus;
end;


function TacColorSelector.EvaluteRowCount(Count: Integer): Integer;
begin
  Result := Count div FColumnCount;
  if Count > Result * FColumnCount then
    Inc(Result);
end;

function TacColorSelector.GetColorBtnSize : integer;  //georges
begin
  ScrRatio := Round(GetPPI(SkinData)/96);
  Result := FColorBtnSize*ScrRatio;
end;

function TacColorSelector.GetColorBtnSpace : integer;  //georges
begin
  ScrRatio := Round(GetPPI(SkinData)/96);
  Result := FColorBtnSpace*ScrRatio;
end;

function TacColorSelector.GetMoreBtnSize : integer;   //georges
begin
  ScrRatio := Round(GetPPI(SkinData)/96);
  Result := FMoreBtnSize*ScrRatio;

end;

function TacColorSelector.GetColor(Index: Integer): TColor;
var
  s: string;
begin
  Result := clNone;
  if (Index >= FColors.Count) or (Index < 0) or (FColors.Count = 0) then
    Exit;

  s := FColors[Index];
  if Pos('|', s) > 0 then
    s := Copy(s, 1, Pos('|', s) - 1);

  try
    Result := StringToColor(s);
  except
    Result := clNone;
  end;
end;


function TacColorSelector.GetColorHint(Index: Integer): string;
var
  s: string;
  cl: TColor;
begin
  Result := '';
  if (Index >= FColors.Count) or (Index < 0) or (FColors.Count = 0) then
    Exit;

  s := FColors[Index];
  if Pos('|', s) > 0 then begin
    Delete(s, 1, Pos('|', s));
    Result := s;
  end
  else
    try
      cl := StringToColor(s);
      Result := RGBToString(cl);
    except
      Result := 'Îøèáêà';
    end;
end;


procedure TacColorSelector.GetPosition(Index: Integer; var Row, Col: Integer);
begin
  Row := Index div FColumnCount;
  Col := Index - Row * FColumnCount;
end;


function TacColorSelector.GetUserColor(Index: Integer): TColor;
var
  s: string;
begin
  Result := clNone;
  if (Index >= FUserColors.Count) or (Index < 0) or (FUserColors.Count = 0) then
    Exit;

  s := FUserColors[Index];
  if Pos('|', s) > 0 then
    s := Copy(s, 1, Pos('|', s) - 1);

  try
    Result := StringToColor(s);
  except
    Result := clNone;
  end;
end;


function TacColorSelector.GetUserColorHint(Index: Integer): string;
var
  s: string;
  cl: TColor;
begin
  Result := '';
  if (Index >= FUserColors.Count) or (Index < 0) or (FUserColors.Count = 0) then
    Exit;

  s := FUserColors[Index];
  if Pos('|', s) > 0 then begin
    Delete(s, 1, Pos('|', s));
    Result := s;
  end
  else
    try
      cl := StringToColor(s);
      Result := RGBToString(cl);
    except
      Result := 'Error';
    end;
end;


procedure TacColorSelector.Paint;

  procedure DrawArrow(ArP: TPoint; ArClr: TColor);
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X + 5, ArP.Y);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 4, ArP.Y + 1);
    Canvas.Pixels[ArP.X + 2, ArP.Y + 2] := ArClr;
  end;

var
  r, BtnR: TRect;
  AP: TPoint;
begin
  if SkinData.Skinned then
    inherited
  else begin
    inherited;
    r := ClientRect;
    if FDropDownButton then begin
      BtnR := Rect(r.Right - 10 - 1, r.Top + 1, r.Right - 1, r.Bottom - 1);
      AP.X := BtnR.Left + ((BtnR.Right - BtnR.Left - 5) div 2) + 1;
      AP.Y := BtnR.Top + ((BtnR.Bottom - BtnR.Top - 3) div 2) + 1;
      DrawArrow(AP, clBlack);
      r.Right := r.Right - 10;
    end
    else
      r.Right := r.Right - 4;

    r.Left := r.Left + 4;
    r.Top := r.Top + 4;
    r.Bottom := r.Bottom - 4;
    FillDC(Canvas.Handle, R, FSelectedColor);
  end;
end;


procedure TacColorSelector.SetColumnCount(const Value: Integer);
begin
  FColumnCount := Max(4, Value);
end;

procedure TacColorSelector.SetColorBtnSize (Value : integer);  //georges
begin
  FColorBtnSize := Value;
  if FColorBtnSize<16 then FColorBtnSize := 16;

end;

procedure TacColorSelector.SetColorBtnSpace (Value : integer);  //georges
begin
  FColorBtnSpace := Value;
  if FColorBtnSpace<0 then FColorBtnSpace := 0;

end;

procedure TacColorSelector.SetMoreBtnSize (Value : integer);   //georges
begin
  FMoreBtnSize := Value;
  if FMoreBtnSize<20 then FMoreBtnSize := 20;

end;


procedure TacColorSelector.SetColors(const Value: TStrings);
begin
  FColors.Assign(Value);
end;


procedure TacColorSelector.SetDropDownButton(const Value: Boolean);
begin
  if FDropDownButton <> Value then begin
    FDropDownButton := Value;
    if Value then
      Width := Width + 6
    else
      Width := Width - 6;

    Repaint;
  end;
end;


procedure TacColorSelector.SetSelectedColor(const Value: TColor);
begin
  if FSelectedColor <> Value then begin
    FSelectedColor := Value;
    SkinData.BGChanged := True;
    if (Visible or (csDesigning in ComponentState)) and not (csLoading in ComponentState) then
      GraphRepaint;
  end;
end;


procedure TacColorSelector.SetUserColors(const Value: TStrings);
begin
  FUserColors.Assign(Value);
end;


function TacColorSelector.PrepareCache: boolean;
var
  R: TRect;
  C: TColor;
  State: integer;
begin
  Result := inherited PrepareCache;
  if Result then begin
    R := ColorRect;
    FillDC(SkinData.FCacheBmp.Canvas.Handle, R, FSelectedColor);
    if FDropDownButton then begin
      State := max(SkinData.CommonSkinData.gd[SkinData.SkinIndex].States - 1, CurrentState);
      C := SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[State].FontColor.Color;
      DrawColorArrow(SkinData.FCacheBmp.Canvas, C, Rect(R.Right, R.Top, Width - 2, R.Bottom), asBottom, SkinData.CommonSkinData.PPI);
    end;
  end;
end;


function TacColorSelector.ColorRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -5, -5);
  if assigned(Glyph) then   //georges
     Dec(Result.Bottom,((ClientRect.Height div 6)*5)-7) ;
  if FDropDownButton then
    dec(Result.Right, 10);
end;

end.
