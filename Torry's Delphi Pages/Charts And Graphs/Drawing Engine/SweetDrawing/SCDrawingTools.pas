{*******************************************************}
{                                                       }
{              CA SweetDrawing Library                  }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCDrawingTools;

{$I SweetDrawing.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Math, SCDECommon, SCDEConsts, SCDrawingCommons, SCDrawingShapes,
  SCDrawingSurface, SCDrawingEditor;

type
  TSCDeKnifeTool = class(TSCDeToolBase)
  protected
    procedure Cut(S: TSCDeShapeBase);
  public
    procedure Process; override;

    procedure Paint(C: TCanvas; X, Y: Double;
      const Data: TSCDeToolPaintData; Zoom: Double); override;
    procedure Add(const P: TDoublePoint); override;
  end;

implementation

{ TSCDeKnifeTool }

procedure TSCDeKnifeTool.Add(const P: TDoublePoint);
begin
  if Count = 2 then
    Points[2] := P
  else
    inherited Add(P);
end;

procedure TSCDeKnifeTool.Cut(S: TSCDeShapeBase);
begin
  //
end;

procedure TSCDeKnifeTool.Paint(C: TCanvas; X, Y: Double;
  const Data: TSCDeToolPaintData; Zoom: Double);
var
  I: Integer;
  P: TDoublePoint;
  Pts: array of TPoint;
begin
  if (Count > 0) and (Data.LineColor <> clNone) and
    (Data.LineStyle <> psClear) then
  begin
    with C, Data do
    begin
      Brush.Style := bsClear;

      Pen.Color := LineColor;
      Pen.Mode  := LineMode;
      Pen.Style := LineStyle;
      Pen.Width := 1;
    end;

    SetLength(Pts, Count);

    for I := 0 to Count-1 do
    begin
      P := Points[I];

      P.x := X + Zoom*P.x;
      P.y := Y + Zoom*P.y;

      Pts[I].x := TSCDeUtils.Round(P.x);
      Pts[I].y := TSCDeUtils.Round(P.y);
    end;

    C.Polyline(Pts);
  end;
end;

procedure TSCDeKnifeTool.Process;
begin

end;

end.
