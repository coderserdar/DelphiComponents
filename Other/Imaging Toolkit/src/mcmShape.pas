// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  17587: mcmShape.pas 
//
//    Rev 1.3    2014-02-02 21:10:06  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.2    18-02-2006 20:05:04  mcm
// Modified to handle closed polygons.
//
//   Rev 1.1    27-09-2002 13:25:40  mcm

//
//   Rev 1.0    27-05-2002 16:22:26  mcm

unit mcmShape;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Controls;
     {$ENDIF}

type
  TmcmShapeType = (ST_RECTANGLE,
                   ST_SQUARE,
                   ST_ROUNDRECT,
                   ST_ROUNDSQUARE,
                   ST_ELLIPSE,
                   ST_CIRCLE,
                   ST_LINE,
                   ST_POLYGON);


  TmcmShape = class(TGraphicControl)
  private
    { Private declarations }
    FAngle        : integer;
    FLength       : integer;
    FStartArrow   : boolean;
    FEndArrow     : boolean;
    FArrowLen     : integer;
    FArrowWidth   : integer;
    FLineCentre   : boolean;
    FStartEndLine : boolean;
    FEndEndLine   : boolean;
    FPen          : TPen;
    FBrush        : TBrush;
    FShape        : TmcmShapeType;
    Fcx, Fcy      : integer;
    FNoPoints     : cardinal;
    FPolyPoints   : array[0..2048] of TPoint;
    procedure SetBrush(Value : TBrush);
    procedure SetPen(Value : TPen);
    procedure SetShape(Value : TmcmShapeType);
    procedure SetAngle(Value : integer);
    procedure SetLength(Value : integer);
    procedure SetStartArrow(Value : boolean);
    procedure SetEndArrow(Value : boolean);
    procedure SetStartEndLine(Value : boolean);
    procedure SetEndEndLine(Value : boolean);
    procedure SetLineCentre(Value : boolean);
    procedure SetArrowLen(Value : integer);
    procedure PerpendicularLine(    rx, ry             : double;
                                    dx, dy             : longint;
                                    PerLineWidth       : longint;
                                var dlx, drx, duy, ddy : longint);
  protected
    { Protected declarations }
    procedure Paint; override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure  SetPoints(Points : array of TPoint);
  published
    { Published declarations }
    procedure StyleChanged(Sender : TObject);
    property Align;
    property Angle : integer
      read   FAngle
      write  SetAngle;
    property ArrowLength : integer
      read   FArrowLen
      write  SetArrowLen default 6;
    property ArrowWidth : integer
      read   FArrowWidth;
    property Brush : TBrush
      read   FBrush
      write  SetBrush;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Length : integer
      read   FLength
      write  SetLength;
    property ParentShowHint;
    property Pen : TPen
      read   FPen
      write  SetPen;
    property Shape : TmcmShapeType
      read   FShape
      write  SetShape default ST_RECTANGLE;
    property ShowArrowEnd : boolean
      read   FEndArrow
      write  SetEndArrow default False;
    property ShowArrowStart : boolean
      read   FStartArrow
      write  SetStartArrow default False;
    property ShowEndEndLine : boolean
      read   FEndEndLine
      write  SetEndEndLine default False;
    property ShowLineCentre : boolean
      read   FLineCentre
      write  SetLineCentre default False;
    property ShowStartEndLine : boolean
      read   FStartEndLine
      write  SetStartEndLine default False;
    property ShowHint;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

{$IFDEF GE_DXE2}
 uses System.Types, System.UITypes;
{$ELSE}
 {$IFDEF GE_DXE} uses Types; {$ENDIF}
{$ENDIF}

constructor TmcmShape.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csReplicatable];

  FPen := TPen.Create;
  FPen.OnChange := StyleChanged;
  FBrush := TBrush.Create;
  FBrush.OnChange := StyleChanged;

  Width        := 65;
  Height       := 65;
  FAngle       := 90;
  FStartArrow  := False;
  FEndArrow    := False;
  FLength      := Width;
  FArrowLen    := 6;

  FNoPoints := 6;
  SetPoints([Point(0, 32),
             Point(10, 32),
             Point(20, 64),
             Point(40, 0),
             Point(50, 32),
             Point(65, 32)]);
end; // TmcmShape.Create.


destructor TmcmShape.Destroy;
begin
  if Assigned(FPen)
  then FPen.Free;
  FPen := Nil;
  if Assigned(FBrush)
  then FBrush.Free;
  FBrush := Nil;
  inherited Destroy;
end; // TmcmShape.Destroy.


procedure TmcmShape.PerpendicularLine(    rx, ry             : double;
                                          dx, dy             : longint;
                                          PerLineWidth       : longint;
                                      var dlx, drx, duy, ddy : longint);
var a, b     : extended;
    p        : extended;
    rdx      : extended;
begin
  if (dx <> 0)
  then begin
       a := 1.0 * dy / dx;
       if (dy <> 0.0)
       then begin
            PerLineWidth := Sqr(PerLineWidth div 4);
            
            // Perpendicular line.
            b   := -1.0 / a;
            p   := -(b * rx - ry);

            rdx := sqrt(1.0 * PerLineWidth / (1.0 + sqr(b)));

            dlx := round(rx - rdx);
            duy := round(b * (rx - rdx) + p);

            // if Odd(FArrowLen)
            // then rdx := rdx + 1.0;
            drx := round(rx + rdx);
            ddy := round(b * (rx + rdx) + p);
       end
       else begin
            // Perpendicular line (Horizontal).
            dlx := Round(rx);
            drx := Round(rx);
            duy := Round(ry - PerLineWidth / 4.0);
            ddy := Round(ry + PerLineWidth / 4.0);
       end;
  end
  else begin
       // Perpendicular line (Vertical).
       dlx := Round(rx - PerLineWidth / 4.0);
       drx := Round(rx + PerLineWidth / 4.0);
       duy := Round(ry);
       ddy := Round(ry);
  end;
end; // TmcmShape.PerpendicularLine.


procedure TmcmShape.Paint;
var x, y     : integer;
    sx, sy   : integer;
    ex, ey   : integer;
    dx, dy   : integer;
    W, H, S  : integer;
    Degree   : double;

    rx, ry   : double;
    dlx, drx : longint; // Left & right x.
    duy, ddy : longint; // Up & down y.

    sColor   : TColor;
begin
  try
    with Canvas
    do begin
       Pen := FPen;
       Brush := FBrush;
       x := Pen.Width div 2;
       y := X;
       W := Width - Pen.Width + 1;
       H := Height - Pen.Width + 1;
       if Pen.Width = 0
       then begin
            Dec(W);
            Dec(H);
       end;
       if W < H
       then S := W
       else S := H;
       if FShape in [ST_SQUARE, ST_ROUNDRECT, ST_CIRCLE]
       then begin
            Inc(x, (W - S) div 2);
            Inc(y, (H - S) div 2);
            W := S;
            H := S;
       end;
       case FShape of
       ST_RECTANGLE,
       ST_SQUARE      : Rectangle(x, y, x + W, y + H);
       ST_ROUNDRECT,
       ST_ROUNDSQUARE : RoundRect(x, y, x + W, y + H, S div 4, S div 4);
       ST_CIRCLE,
       ST_ELLIPSE     : Ellipse(x, y, x + W, y + H);
       ST_LINE        : begin
                          Fcx := Width div 2;
                          Fcy := Height div 2;
                          Degree := pi * FAngle / 180.0;
                          dx := Round(FLength * cos(Degree) / 2.0);
                          dy := Round(FLength * sin(Degree) / 2.0);
                          sx := Fcx + dx;
                          sy := Fcy - dy;
                          ex := Fcx - dx;
                          ey := Fcy + dy;

                          MoveTo(sx, sy);
                          LineTo(ex, ey);

                          if (FLineCentre)
                          then Ellipse(Fcx - (2 + 2 * Pen.Width), Fcy - (2 + 2 * Pen.Width),
                                       Fcx + (2 + 2 * Pen.Width), Fcy + (2 + 2 * Pen.Width));

                          dx := ex - sx;
                          dy := ey - sy;

                          if (FStartArrow)
                          then begin
                               sColor := Brush.Color;
                               Brush.Color := Pen.Color;

                               rx := Fcx + (FLength - FArrowLen) * cos(Degree) / 2.0;
                               ry := Fcy - (FLength - FArrowLen) * sin(Degree) / 2.0;

                               PerpendicularLine(rx, ry, dx, dy, FArrowLen, dlx, drx, duy, ddy);

                               // Just for fun.
                               FArrowWidth := Round(Sqrt(Sqr(dlx - drx) + Sqr(duy - ddy)));

                               Canvas.Polygon([Point(sx, sy), Point(dlx, duy),
                                               Point(drx, ddy), Point(sx, sy)]);
                               Brush.Color := sColor;
                          end;

                          if (FEndArrow)
                          then begin
                               sColor := Brush.Color;
                               Brush.Color := Pen.Color;

                               rx := Fcx - (FLength - FArrowLen) * cos(Degree) / 2.0;
                               ry := Fcy + (FLength - FArrowLen) * sin(Degree) / 2.0;
                               PerpendicularLine(rx, ry, dx, dy, FArrowLen, dlx, drx, duy, ddy);

                               Canvas.Polygon([Point(ex, ey), Point(dlx, duy),
                                               Point(drx, ddy), Point(ex, ey)]);
                               Brush.Color := sColor;
                          end;

                          if FStartEndLine
                          then begin
                              PerpendicularLine(1.0 * sx, 1.0 * sy, dx, dy, FArrowLen, dlx, drx, duy, ddy);
                              MoveTo(dlx, duy);
                              LineTo(drx, ddy);
                          end;

                          if (FEndEndLine)
                          then begin
                               PerpendicularLine(1.0 * ex, 1.0 * ey, dx, dy, FArrowLen, dlx, drx, duy, ddy);
                               MoveTo(dlx, duy);
                               LineTo(drx, ddy);
                          end;
                        end;
       ST_POLYGON     : begin
                          if (FPolyPoints[0].x = FPolyPoints[FNoPoints-1].x) and
                             (FPolyPoints[0].y = FPolyPoints[FNoPoints-1].y)
                          then Polygon(Slice(FPolyPoints, FNoPoints))
                          else PolyLine(Slice(FPolyPoints, FNoPoints));
                        end;
       end;
    end;
  finally
  end;
end; // TmcmShape.Paint.


procedure TmcmShape.SetAngle(Value : integer);
begin
  FAngle := Value;
  Invalidate;
end; // TmcmShape.SetAngle.


procedure TmcmShape.SetLength(Value : integer);
begin
  FLength := Value;
  Invalidate;
end; // TmcmShape.SetLength.


procedure TmcmShape.StyleChanged(Sender : TObject);
begin
  Invalidate;
end; // TmcmShape.StyleChanged.


procedure TmcmShape.SetBrush(Value : TBrush);
begin
  FBrush.Assign(Value);
end; // TmcmShape.SetBrush.


procedure TmcmShape.SetPen(Value : TPen);
begin
  FPen.Assign(Value);
end; // TmcmShape.SetPen.


procedure TmcmShape.SetShape(Value : TmcmShapeType);
begin
  if (FShape <> Value)
  then begin
       FShape := Value;
       Invalidate;
  end;
end; // TmcmShape.SetShape.


procedure TmcmShape.SetStartArrow(Value : boolean);
begin
  FStartArrow := Value;
  Invalidate;
end; // TmcmShape.SetStartArrow.


procedure TmcmShape.SetEndArrow(Value : boolean);
begin
  FEndArrow := Value;
  Invalidate;
end; // TmcmShape.SetEndArrow.


procedure TmcmShape.SetLineCentre(Value : boolean);
begin
  FLineCentre := Value;
  Invalidate;
end; // TmcmShape.SetLineCentre.


procedure TmcmShape.SetArrowLen(Value : integer);
begin
  FArrowLen := Value;
  Invalidate;
end; // TmcmShape.SetArrowLen.


procedure TmcmShape.SetStartEndLine(Value : boolean);
begin
  FStartEndLine := Value;
  Invalidate;
end; // TmcmShape.SetStartEndLine.


procedure TmcmShape.SetEndEndLine(Value : boolean);
begin
  FEndEndLine := Value;
  Invalidate;
end; // TmcmShape.SetEndEndLine.


procedure TmcmShape.SetPoints(Points : array of TPoint);
var i : integer;
begin
  FNoPoints := High(Points) + 1;
  for i := 0 to (FNoPoints - 1)
  do FPolyPoints[i] := Points[i];
  Invalidate;
end; // TmcmShape.SetPoints.

end.
