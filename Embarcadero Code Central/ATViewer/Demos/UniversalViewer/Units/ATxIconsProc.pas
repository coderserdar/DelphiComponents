unit ATxIconsProc;

interface

uses
  Windows, Controls;

procedure FSaveIcons(IL: TImageList; const FN: string);
procedure FLoadIcons(IL: TImageList; const FN: string);

implementation

uses
  SysUtils, Classes, Graphics, ATxParamStr;

const
  clTool: TColor = $00FFFFEF; //Back color for saving

//----------------------------------------------------------
//Get sizes from "Name WWxHH text.bmp".
// 0 if size not found.
procedure FGetIconSizes(const FN: string; var Width, Height: Integer);
const
  Dig = ['0'..'9'];
var
  S, S1, S2: string;
  i, i1, i2: integer;
begin
  Width:= 0;
  Height:= 0;

  S:= FN;
  i:= Length(S) - 1;
  //Search last 'x' with next digit:
  while (i > 0) and
    not ( (S[i] = 'x') and (S[i+1] in Dig) ) do Dec(i);
  if i = 0 then Exit;

  i1:= i - 1;
  while (i1 > 0) and (S[i1] in Dig) do Dec(i1);
  if i1 = 0 then Exit;

  i2:= i + 1;
  while (i2 <= Length(S)) and (S[i2] in Dig) do Inc(i2);
  if i2 >= Length(S) then Exit;

  S1:= Copy(S, i1 + 1, i - i1 - 1);
  S2:= Copy(S, i + 1, i2 - i - 1);
  //MessageBox(0, PChar(S1 + #13 + S2), 'Test', MB_OK);

  Width:= StrToIntDef(S1, Width);
  Height:= StrToIntDef(S2, Height);
end;


//----------------------------------------------------------
procedure FSaveIcons(IL: TImageList; const FN: string);
var
  b: TBitmap;
  i: Integer;
begin
  b := TBitmap.Create;
  b.PixelFormat := pf24bit;

  try
    with IL do
    begin
      b.Height := Height;
      b.Width := Width * Count;

      b.Canvas.Brush.Color := clTool;
      b.Canvas.FillRect(Rect(0, 0, b.Width, b.Height));

      for i := 0 to Count - 1 do
        Draw(b.Canvas, i * Width, 0, i);
    end;
    b.SaveToFile(FN);
  finally
    b.Free;
  end;
end;

//----------------------------------------------------------
procedure FLoadIcons(IL: TImageList; const FN: string);
var
  b: TBitmap;
  W, H: Integer;
begin
  b:= TBitmap.Create;
  try
    try
      b.LoadFromFile(FN);
    except
      Exit;
    end;

    with IL do
    begin
      Clear;
      FGetIconSizes(FN, W, H);
      if W = 0 then
        W := b.Height;
      if H = 0 then
        H := b.Height;
      Width := W;
      Height := H;
      AddMasked(b, b.Canvas.Pixels[0, H - 1]);
    end;
  finally
    b.Free;
  end;
end;


end.
