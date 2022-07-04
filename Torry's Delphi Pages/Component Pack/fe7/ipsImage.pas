unit ipsImage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type


  TpsImage = class(TImage)
  private
    FExtStretch: Boolean;
    FAngle: Double;
    procedure SetExtStretch(const Value: Boolean);
    procedure SetAngle(const Value: Double);
    { Private declarations }
  protected
    { Protected declarations }
  public
        procedure Paint; override;
  published
        property ExtStretch:Boolean read FExtStretch write SetExtStretch;
        property Angle:Double read FAngle write SetAngle;
  end;



implementation

uses ipsGrUtils;

{ TpsImage }


// Combine red, green, and blue color components.
function RGB(red, green, blue: Integer): TColor;
begin
  Result := red + 256 * (green + 256 * blue);
end;

procedure ShrinkPicture(from_canvas, to_canvas: TCanvas;
        from_x1, from_y1, from_x2, from_y2: Integer;
        to_x1, to_y1, to_x2, to_y2: Integer);
var xscale, yscale : Single;
    to_y, to_x     : Integer;
    x1, x2, y1, y2 : Integer;
ix, iy : Integer;
  new_red, new_green : Integer;
  new_blue : Integer;
  total_red, total_green : Single;
  total_blue : Single;
  ratio : Single;
begin
  // Compute the scaling parameters. This is useful if
  // the image is not being scaled proportionally.
  xscale := (to_x2 - to_x1 + 1) / (from_x2 - from_x1);
  yscale := (to_y2 - to_y1 + 1) / (from_y2 - from_y1);

  // Perform the reduction.
  for to_y := to_y1 to to_y2 do begin
    y1 := Trunc((to_y - to_y1) / yscale + from_y1);
    y2 := Trunc((to_y + 1 - to_y1) / yscale + from_y1) - 1;
    for to_x := to_x1 to to_x2 do begin
      x1 := Trunc((to_x - to_x1) / xscale + from_x1);
      x2 := Trunc((to_x + 1 - to_x1) / xscale + from_x1) - 1;

      // Average the values in from_canvas within
      // the box (x1, y1) - (x2, y2). 
total_red := 0;
      total_green := 0;
      total_blue := 0;
      for iy := y1 to y2 do begin
        for ix := x1 to x2 do begin
          SeparateColor(from_canvas.Pixels[ix, iy],
                        new_red, new_green, new_blue);
          total_red := total_red + new_red;
     total_green := total_green + new_green;
          total_blue := total_blue + new_blue;
        end;
      end;
      ratio := 1 / (x2 - x1 + 1) / (y2 - y1 + 1);
      to_canvas.Pixels[to_x, to_y] := RGB(
        Round(total_red   * ratio),
        Round(total_green * ratio),
        Round(total_blue  * ratio));
    end; // End for to_x := to_x1 to to_x2 - 1 loop.
  end; // End for to_y := to_y1 to to_y2 - 1 loop.
end;


// Enlarge the picture in from_canvas and place it
// in to_canvas.
procedure EnlargePicture(
  from_canvas, to_canvas: TCanvas;
  from_x1, from_y1, from_x2, from_y2: Integer;
  to_x1, to_y1, to_x2, to_y2: Integer);
var
  xscale, yscale : Single;
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
  // Compute the scaling parameters. This is useful if
  // the image is not being scaled proportionally.
  xscale := (to_x2 - to_x1 + 1) / (from_x2 - from_x1);
  yscale := (to_y2 - to_y1 + 1) / (from_y2 - from_y1);

  // Perform the enlargement. 
  for to_y := to_y1 to to_y2 do begin
    sfrom_y := (to_y - to_y1) / yscale + from_y1;
    ifrom_y := Trunc(sfrom_y);
    weight_y[1] := sfrom_y - ifrom_y;
    weight_y[0] := 1 - weight_y[1];
    for to_x := to_x1 to to_x2 do begin
      sfrom_x := (to_x - to_x1) / xscale + from_x1;
      ifrom_x := Trunc(sfrom_x);
      weight_x[1] := sfrom_x - ifrom_x;
      weight_x[0] := 1 - weight_x[1];
      // Average the color components of the four
      // nearest pixels in from_canvas.
        total_red := 0.0;
      total_green := 0.0;
        total_blue := 0.0;
        for ix := 0 to 1 do begin
          for iy := 0 to 1 do begin
            SeparateColor(from_canvas.Pixels[
              ifrom_x + ix, ifrom_y + iy],
              new_red, new_green, new_blue);
  weight := weight_x[ix] * weight_y[iy];
            total_red := total_red + new_red   * weight;
            total_green := total_green + new_green * weight;
            total_blue  := total_blue + new_blue  * weight;
          end;
        end;

        // Set the output pixel's value.
        to_canvas.Pixels[to_x, to_y] := RGB(
            Round(total_red),
            Round(total_green),
            Round(total_blue));
    end; // End for to_x := to_x1 to to_x2 loop. 
  end; // End for to_y := to_y1 to to_y2 loop. 
end;





procedure TpsImage.Paint;
var C:TCanvas;
begin
        if (not ExtStretch) then inherited Paint
        else begin
                C:= (inherited Canvas);
                if Angle<>0 then
                        RotatePicture(Canvas, C, Angle,
                        Rect(0, 0, Picture.Width-1, Picture.Height-1),
                        Rect(0,0, Width-1, Height-1))
                else
                EnlargePicture(Canvas, C , 0, 0, Picture.Width-1, Picture.Height-1,
                        0,0, Width-1, Height-1);
        end;

end;

procedure TpsImage.SetAngle(const Value: Double);
begin
        if FAngle<>Value then begin
                FAngle := Value;
                Invalidate;
        end;
end;

procedure TpsImage.SetExtStretch(const Value: Boolean);
begin
        if FExtStretch<>Value then begin
                FExtStretch := Value;
                Invalidate;
        end;
end;

end.
