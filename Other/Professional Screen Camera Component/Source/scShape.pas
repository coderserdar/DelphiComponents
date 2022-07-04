{ *********************************************************************** }
{                                                                         }
{ Shape                                                                   }
{                                                                         }
{ Copyright (c) 2003-2004 Pisarev Yuriy (mail@pisarev.net)                }
{                                                                         }
{ *********************************************************************** }

unit scShape;

{$B-}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes;

type
  TRange = record
    Left, Top, Width, Height: Integer;
  end;

  TShapeType = (stTriangle0, stTriangle1, stTriangle2, stTriangle3, stRhomb,
    stCross);
  TPoints = array of TPoint;

  TShapePoints = class
  private
    FOffset: Integer;
    FPoints: TPoints;
    FRange: TRange;
    FShapeType: TShapeType;
    procedure SetShapeType(const Value: TShapeType);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Update(AShapeType: TShapeType); virtual;
    property Range: TRange read FRange write FRange;
    property Offset: Integer read FOffset write FOffset;
    property ShapeType: TShapeType read FShapeType write SetShapeType;
    property Points: TPoints read FPoints;
  end;

function Range(ALeft, ATop, AWidth, AHeight: Integer): TRange;

implementation

function Range(ALeft, ATop, AWidth, AHeight: Integer): TRange;
begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Width := AWidth;
    Height := AHeight;
  end;
end;

{ TShapePoints }

constructor TShapePoints.Create;
begin
  FOffset := 2;
  SetShapeType(stTriangle0);
end;

destructor TShapePoints.Destroy;
begin
  FPoints := nil;
  inherited;
end;

procedure TShapePoints.SetShapeType(const Value: TShapeType);
begin
  FShapeType := Value;
  Update(Value);
end;

procedure TShapePoints.Update(AShapeType: TShapeType);
var
  I, J: Integer;
  ARange: TRange;
begin
  ARange := FRange;
  I := 2 * FOffset;
  if ARange.Width > I then
  begin
    Dec(ARange.Left, FOffset);
    Dec(ARange.Width, FOffset);
  end;
  if ARange.Height > I then
  begin
    Dec(ARange.Top, FOffset);
    Dec(ARange.Height, FOffset);
  end;
  case AShapeType of
    stTriangle0: begin
      SetLength(FPoints, 3);
      with ARange do
      begin
        FPoints[0] := Point(Left + Width div 2, Top);
        FPoints[1] := Point(Left + Width, Top + Height);
        FPoints[2] := Point(Left, Top + Height);
      end;
    end;
    stTriangle1: begin
      SetLength(FPoints, 3);
      with ARange do
      begin
        FPoints[0] := Point(Left, Top);
        FPoints[1] := Point(Left + Width, Top + Height div 2);
        FPoints[2] := Point(Left, Top + Height);
      end;
    end;
    stTriangle2: begin
      SetLength(FPoints, 3);
      with ARange do
      begin
        FPoints[0] := Point(Left, Top);
        FPoints[1] := Point(Left + Width div 2, Top + Height);
        FPoints[2] := Point(Left + Width, Top);
      end;
    end;
    stTriangle3: begin
      SetLength(FPoints, 3);
      with ARange do
      begin
        FPoints[0] := Point(Left + Width, Top);
        FPoints[1] := Point(Left + Width, Top + Height);
        FPoints[2] := Point(Left, Top + Height div 2);
      end;
    end;
    stRhomb: begin
      SetLength(FPoints, 4);
      with ARange do
      begin
        I := Width div 2;
        J := Height div 2;
        FPoints[0] := Point(Left, Top + J);
        FPoints[1] := Point(Left + I, Top + Height);
        FPoints[2] := Point(Left + Width, Top + J);
        FPoints[3] := Point(Left + I, Top);
      end;
    end;
    stCross: begin
      SetLength(FPoints, 12);
      with ARange do
      begin
        I := Width div 3;
        J := Height div 3;
        FPoints[0] := Point(Left, Top + J);
        FPoints[1] := Point(Left + I, Top + J);
        FPoints[2] := Point(Left + I, Top);
        FPoints[3] := Point(Left + Width - I, Top);
        FPoints[4] := Point(Left + Width - I, Top + J);
        FPoints[5] := Point(Left + Width, Top + J);
        FPoints[6] := Point(Left + Width, Top + Height - J);
        FPoints[7] := Point(Left + Width - I, Top + Height - J);
        FPoints[8] := Point(Left + Width - I, Top + Height);
        FPoints[9] := Point(Left + I, Top + Height);
        FPoints[10] := Point(Left + I, Top + Height - J);
        FPoints[11] := Point(Left, Top + Height - J);
      end;
    end;
  end;
end;

end.
