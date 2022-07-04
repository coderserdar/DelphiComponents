unit ipsGrUtils;

interface

uses Windows, Graphics;

function WidthOf(R:TRect):Integer;
function HeightOf(R:TRect):Integer;

procedure SeparateColor(color: TColor; var red, green, blue: Integer);

procedure RotatePicture(  c_from, c_to : TCanvas; theta: Single;
  Source, Dest : TRect);

implementation


uses Math;


function WidthOf(R:TRect):Integer;
begin
        Result := R.Right-R.Left;
end;

function HeightOf(R:TRect):Integer;
begin
        Result := R.Bottom - R.Top;
End;

// Separate a color into red, green, and blue components.
procedure SeparateColor(color: TColor;
  var red, green, blue: Integer);
begin
  red :=  color mod 256;
  green := (color div 256) mod 256;
  blue :=  color div 65536;
end;


procedure RotatePicture( c_from, c_to: TCanvas; theta: Single;
  Source, Dest : TRect);
var
  sin_theta, cos_theta   : Extended;
  from_cx, from_cy : Single;
  to_cx, to_cy : Single;
  sfrom_y, sfrom_x : Single;
  ifrom_y, ifrom_x : Integer;
  to_y, to_x : Integer;
  weight_x, weight_y : array[0..1] of Single;
  weight : Single;
  new_red, new_green : Integer;
  new_blue : Integer;
  total_red, total_green : Single;
  total_blue : Single;
  ix, iy : Integer;
begin
  SinCos(theta, sin_theta, cos_theta);

  // centers of canvases
  from_cx := WidthOf(Source)/ 2;
  from_cy := HeightOf(Source) / 2;
  to_cx := WidthOf(Dest)/ 2;
  to_cy := HeightOf(Dest) / 2;

  // Perform the rotation.
  for to_y := Dest.Top to Dest.Bottom do begin
    for to_x := Dest.Left to Dest.Right do begin
      sfrom_x := from_cx + (to_x - to_cx) * cos_theta -
                            (to_y - to_cy) * sin_theta;
      ifrom_x := Trunc(sfrom_x);

      sfrom_y := from_cy + (to_x - to_cx) * sin_theta +
                            (to_y - to_cy) * cos_theta;
      ifrom_y := Trunc(sfrom_y);

      // Only process this pixel if all four adjacent input
// pixels are inside the allowed input area.
      if (ifrom_x >= Source.Left) and (ifrom_x <   Source.Right) and
          (ifrom_y >= Source.Top) and (ifrom_y < Source.Bottom) then
      begin
        // Calculate the weights.
        weight_y[1] := sfrom_y - ifrom_y;
        weight_y[0] := 1 - weight_y[1];
        weight_x[1] := sfrom_x - ifrom_x;
        weight_x[0] := 1 - weight_x[1];

        // Average the color components of the four
        // nearest pixels in from_canvas.
        total_red := 0.0;
        total_green := 0.0;
        total_blue := 0.0;
        for ix := 0 to 1 do begin
          for iy := 0 to 1 do begin
            SeparateColor(
              c_from.Pixels[ifrom_x + ix,
              ifrom_y + iy], new_red, new_green, new_blue);
            weight := weight_x[ix] * weight_y[iy];
            total_red := total_red + new_red   * weight;
            total_green:= total_green + new_green * weight;
            total_blue := total_blue  + new_blue * weight;
          end;
        end;

        // Set the output pixel's value.
        c_to.Pixels[to_x, to_y] := RGB(
          Round(total_red), Round(total_green),
          Round(total_blue));
      end;  // End if adjacent pixels in bounds.
    end;  // End for to_x := to_x1 to to_x2 loop. 
  end;  // End for to_y := to_y1 to to_y2 loop. 
end;



end.
