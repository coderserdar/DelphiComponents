unit RSCollisionMap;
//=== File Prolog ============================================================
//	This code was developed by RiverSoftAVG.
//
//--- Notes ------------------------------------------------------------------
//
//--- Development History  ---------------------------------------------------
//
//	  02/2004	T. Grubb
//		        Initial version.
//
//      File Contents:
//
//
//--- Warning ----------------------------------------------------------------
//	This software is property of RiverSoftAVG. Unauthorized use
//  or duplication of this software is strictly prohibited. Authorized users
//  are subject to the following restrictions:
//	*	RiverSoftAVG is not responsible for
//		any consequence of the use of this software.
//	*	The origin of this software must not be misrepresented either by
//		explicit claim or by omission.
//	*	Altered versions of this software must be plainly marked as such.
//	*	This notice may not be removed or altered.
//
//  © 2004, RiverSoftAVG.com
//
//=== End File Prolog ========================================================

interface

uses
  SysUtils, Classes, Types, Graphics;

type
  TRSCollisionMap = class(TPersistent)
  { Purpose: To encapsulate a 2D collision map }
  private
    { Private declarations }
    FHeight: Integer;
    FWidth: Integer;
  protected
    { Protected declarations }
    function GetValues(const X, Y: Integer): Integer; virtual; abstract;
    procedure SetValues(const X, Y, Value: Integer); virtual; abstract;
    procedure SetWidth(const Value: Integer); virtual;
    procedure SetHeight(const Value: Integer); virtual;
  public
    { Public declarations }
    procedure FillRect( const Rect: TRect; Value: Integer ); virtual; abstract;
    procedure Clear; overload;
    procedure Clear(const ARect: TRect); overload;
    procedure Copy(X, Y: Integer; Graphic: TGraphic); virtual; abstract;
    procedure CopyPortion( const Dest: TRect; Bitmap: TBitmap; const Source: TRect; Color: TColor); virtual; abstract;
    procedure CopyToRect( const Rect: TRect; Graphic: TGraphic ); virtual; abstract;
    function IsCircleCollision( const CircleRect: TRect; var Point: TPoint ): Boolean; virtual;
    function IsCollision( const Rect: TRect; var Point: TPoint; QuickCheck: Boolean = True ): Boolean; overload; virtual; abstract;
    function IsCollision( const Rect: TRect; QuickCheck: Boolean = True ): Boolean; overload; virtual;
    function IsCollision( const Point: TPoint ): Boolean; overload; virtual; abstract;
    function IsCollision( const X, Y: Integer ): Boolean; overload; virtual; abstract;
    procedure LoadFromFile( const Filename: TFilename ); virtual;
    procedure LoadFromStream( const Stream: TStream ); virtual; abstract;
    property Values[ const X, Y: Integer ]: Integer read GetValues write SetValues;
  published
    { Published declarations }
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
  end; { TRSCollisionMap }

  TRSCollisionBitmap = class(TRSCollisionMap)
  { Purpose: To encapsulate a 2D collision map using a bitmap/canvas, where every pixel
    may be either 0 (empty) or > 0 {Index or Pointer to something)
    Note: if GetValues returns -1 then means that x,y are out of bounds }
  private
    { Private declarations }
    FBitmap: TBitmap;
    procedure SetBitmap(const Value: TBitmap);
  protected
    { Protected declarations }
    function GetValues(const X, Y: Integer): Integer; override;
    procedure SetValues(const X, Y, Value: Integer); override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetHeight(const Value: Integer); override;
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Copy(X, Y: Integer; Graphic: TGraphic); override;
    procedure CopyPortion(const Dest: TRect; Bitmap: TBitmap; const Source: TRect; Color: TColor); override;
    procedure CopyToRect( const Rect: TRect; Graphic: TGraphic ); override;
    procedure FillRect(const Rect: TRect; Value: Integer); override;
    function IsCollision( const Rect: TRect; var Point: TPoint; QuickCheck: Boolean = True ): Boolean; overload; override;
    function IsCollision( const Point: TPoint ): Boolean; overload; override;
    function IsCollision( const X, Y: Integer ): Boolean; overload; override;
    procedure LoadFromStream( const Stream: TStream ); override;
    property Bitmap: TBitmap read FBitmap write SetBitmap;
  published
    { Published declarations }
  end; { TRSCollisionBitmap }

implementation

{ TRSCollisionMap }

procedure TRSCollisionMap.Clear;
begin
     FillRect(Rect(0,0,Width,Height),0);
end;

procedure TRSCollisionMap.Clear(const ARect: TRect);
begin
     FillRect(ARect,0);
end;

function TRSCollisionMap.IsCircleCollision(const CircleRect: TRect;
  var Point: TPoint): Boolean;
  function GetCollisionPoint( const X, Y: Integer ): Boolean;
  begin
       result := IsCollision(X, Y);
       if result then
       begin
            Point.X := X;
            Point.Y := Y;
       end;
  end;
var
   CPt: TPoint;
   CornerPoint: TPoint;
   i: Integer;
   xSign, ySign: Integer;
   a, b: Integer;
   x, y: Single;
   MidX, MidY: Integer;
begin
     // returns true if collision with Ellipse that is closest to a circle for
     // that rect, e.g.,
     // ellipse equation: X^2/a^2 + Y^2/b^2 = 1
     // where a := MidX-1 and b := MidY-1;
     if IsCollision(CircleRect, CornerPoint) then
     begin
          // make sure it is a true collision
          // if collides at mid-points then true collision
          CPt := CenterPoint(CircleRect);
          result := True;
          if GetCollisionPoint(CPt.X, CircleRect.Top) or
             GetCollisionPoint(CPt.X, CircleRect.Bottom) or
             GetCollisionPoint(CircleRect.Left, CPt.Y) or
             GetCollisionPoint(CircleRect.Right, CPt.Y) then
             Exit;
          // ok, we have a chance it didn't collide,
          // we have collision corner that signalled a collision
          if CornerPoint.X = CircleRect.Left then
             xSign := -1
          else
              xSign := 1;
          if CornerPoint.Y = CircleRect.Top then
             ySign := -1
          else
              ySign := 1;
          MidX := (CircleRect.Right - CircleRect.Left) div 2;
          MidY := (CircleRect.Bottom - CircleRect.Top) div 2;
          a := MidX-1;
          b := MidY-1;
          for i := 0 to a do
          begin
               x := i;
               y := (1 - (Sqr(x) / Sqr(a)))*Sqr(b);
               if y > 0 then
                  y := Sqrt(y);
               // point on circumference by angle and circle rectangle
               if IsCollision(Round(CircleRect.Left+MidX+xSign*x),Round(CircleRect.Top+MidY+ySign*y)) then
               begin
                    Point := Types.Point(Round(CircleRect.Left+MidX+xSign*x),Round(CircleRect.Top+MidY+ySign*y));
                    Exit;
               end;
          end;
          result := False;
     end
     else
         result := False;
end;

function TRSCollisionMap.IsCollision(const Rect: TRect;
  QuickCheck: Boolean): Boolean;
var
   APoint: TPoint;
begin
     result := IsCollision( Rect, APoint, QuickCheck );
end;

procedure TRSCollisionMap.LoadFromFile(const Filename: TFilename);
var
   FileStream: TFileStream;
begin
     FileStream := TFileStream.Create(Filename, fmOpenRead);
     try
        LoadFromStream(FileStream);
     finally
        FileStream.Free;
     end;
end;

procedure TRSCollisionMap.SetHeight(const Value: Integer);
begin
  FHeight := Value;
end;

procedure TRSCollisionMap.SetWidth(const Value: Integer);
begin
  FWidth := Value;
end;

{ TRSCollisionBitmap }

constructor TRSCollisionBitmap.Create;
begin
     inherited Create;
     FBitmap := TBitmap.Create;
     FBitmap.PixelFormat := pf32bit;
end;

destructor TRSCollisionBitmap.Destroy;
begin
     FBitmap.Free;
     inherited Destroy;
end;

function TRSCollisionBitmap.IsCollision(const Rect: TRect;
  var Point: TPoint; QuickCheck: Boolean): Boolean;
  function GetCollisionPoint( const X, Y: Integer ): Boolean;
  begin
       result := IsCollision(X, Y);
       if result then
       begin
            Point.X := X;
            Point.Y := Y;
       end;
  end;
var
   i, j: Integer;
begin
     if QuickCheck then
     begin
          result := GetCollisionPoint(Rect.Left, Rect.Top) or
                    GetCollisionPoint(Rect.Left, Rect.Bottom) or
                    GetCollisionPoint(Rect.Right, Rect.Top) or
                    GetCollisionPoint(Rect.Right, Rect.Bottom);
     end
     else
     begin
          Result := True;
          for i := Rect.Left to Rect.Right do
              for j := Rect.Top to Rect.Bottom do
                  if IsCollision(i,j) then
                  begin
                       Point.X := i;
                       Point.Y := j;
                       Exit;
                  end;
          result := False;
     end;
end;

function TRSCollisionBitmap.IsCollision(const Point: TPoint): Boolean;
begin
     result := (Bitmap.Canvas.Pixels[Point.X, Point.Y] <> TColor(0)) and
               (Point.X >= 0) and (Point.X < Width) and
               (Point.Y >= 0) and (Point.Y < Height);
end;

function TRSCollisionBitmap.IsCollision(const X, Y: Integer): Boolean;
begin
     result := (Bitmap.Canvas.Pixels[X, Y] <> TColor(0)) and
               (X >= 0) and (X < Width) and
               (Y >= 0) and (Y < Height);
end;

function TRSCollisionBitmap.GetValues(const X, Y: Integer): Integer;
begin
     result := Integer(Bitmap.Canvas.Pixels[X, Y]);
end;

procedure TRSCollisionBitmap.SetValues(const X, Y, Value: Integer);
begin
     Bitmap.Canvas.Pixels[X,Y] := TColor(Value);
end;

procedure TRSCollisionBitmap.LoadFromStream(const Stream: TStream);
begin
     Bitmap.LoadFromStream(Stream);
     inherited Height := Bitmap.Height;
     inherited Width := Bitmap.Width;
end;

procedure TRSCollisionBitmap.SetBitmap(const Value: TBitmap);
begin
  FBitmap.Assign( Value );
end;

procedure TRSCollisionBitmap.FillRect(const Rect: TRect; Value: Integer);
begin
     if (Rect.Right = Rect.Left) and (Rect.Bottom = Rect.Top) then Exit;
     Bitmap.Canvas.Brush.Style := bsSolid;
     Bitmap.Canvas.Brush.Color := TColor(Value);
     Bitmap.Canvas.FillRect(Rect);
end;

procedure TRSCollisionBitmap.SetHeight(const Value: Integer);
begin
     inherited SetHeight(Value);
     Bitmap.Height := Value;
     Clear;
end;

procedure TRSCollisionBitmap.SetWidth(const Value: Integer);
begin
     inherited SetWidth(Value);
     Bitmap.Width := Value;
     Clear;
end;

procedure TRSCollisionBitmap.Copy(X, Y: Integer; Graphic: TGraphic);
begin
     Bitmap.Canvas.Draw(X, Y, Graphic);
end;

procedure TRSCollisionBitmap.CopyPortion(const Dest: TRect;
  Bitmap: TBitmap; const Source: TRect; Color: TColor);
begin
     Self.Bitmap.Canvas.BrushCopy(Dest, Bitmap, Source, Color);
end;

procedure TRSCollisionBitmap.CopyToRect(const Rect: TRect;
  Graphic: TGraphic);
begin
     Bitmap.Canvas.StretchDraw(Rect, Graphic);
end;

procedure TRSCollisionBitmap.Assign(Source: TPersistent);
begin
     if Source is TRSCollisionBitmap then
     begin
          Bitmap.Assign(TRSCollisionBitmap(Source).Bitmap);
          inherited Height := TRSCollisionBitmap(Source).Height;
          inherited Width := TRSCollisionBitmap(Source).Width;
     end
     else if Source is TBitmap then
     begin
          Bitmap.Assign(TBitmap(Source));
          inherited Height := TBitmap(Source).Height;
          inherited Width := TBitmap(Source).Width;
     end
     else
         inherited Assign(Source);
end;

end.
