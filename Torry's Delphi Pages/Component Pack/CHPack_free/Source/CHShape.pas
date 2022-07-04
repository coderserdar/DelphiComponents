unit CHShape;

{ ##############################################################################
  TCHShape

  Version   		:   1.0.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 27.03.2003    - First Release


  ############################################################################ }

interface

uses
  Windows, Classes, Controls, Graphics, Messages;

type
  TShapeType = (stRectangle, stRectangleRound, stEllipse, stCircle, stLine,
    stTriangle, stArrowSmall, stArrowLarge, stTrapezoid, stHexagon, stOctagon);
  TShapeDirection = (sdLeft, sdRight, sdUp, sdDown);

  TCHCustomShape = class(TGraphicControl)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FShapeType: TShapeType;
    FBrush: TBrush;
    FPen: TPen;
    FShapeDirection: TShapeDirection;
    FDrawX : Integer;
    FDrawY : Integer;
    procedure SetShapeType(const Value: TShapeType);
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message : TMessage); message CM_MOUSELEAVE;
    procedure SetBrush(const Value: TBrush);
    procedure SetPen(const Value: TPen);
    procedure SetShapeDirection(const Value: TShapeDirection);
    procedure DoChange(Sender: TObject);

    procedure DrawRectangle;
    procedure DrawRectangleRound;
    procedure DrawEllipse;
    procedure DrawCircle;
    procedure DrawTriangle;
    procedure DrawArrowSmall;
    procedure DrawArrowLarge;
    procedure DrawTrapezoid;
    procedure DrawHexagon;
    procedure DrawOctagon;
    procedure DrawLine;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
  published
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave : TNotifyEvent read FOnMouseLeave write FOnMouseLeave;

    property Brush: TBrush read FBrush write SetBrush;
    property Pen: TPen read FPen write SetPen;
    property Shape : TShapeType read FShapeType write SetShapeType;
    property ShapeDirection : TShapeDirection read FShapeDirection write SetShapeDirection;
  end;

  TCHShape = class(TCHCustomShape)
  published
    property OnClick;
    property OnDragDrop;
    property OnEndDrag;
    property OnMouseMove;
    property OnDblClick;
    property OnDragOver;
    property OnMouseDown;
    property OnMouseUp;

    property Popupmenu;
    property ShowHint;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHShape]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHCustomShape.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Width := 100;
  Height := 100;
  ControlStyle := ControlStyle + [csAcceptsControls, csReplicatable	];
  FPen := TPen.Create;
  FPen.OnChange := DoChange;
  FBrush := TBrush.Create;
  FBrush.OnChange := DoChange;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHCustomShape.Destroy;
begin
  FPen.Free;
  FBrush.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.CMMouseEnter(var Message: TMessage);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.CMMouseLeave(var Message: TMessage);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.Paint;
begin
  inherited Paint;

  with Canvas do
  begin
    Brush := FBrush;
    Pen := FPen;
  end;

  FDrawX := FPen.Width div 2;
  if FDrawX = 0 then
    FDrawX := 1;
  FDrawY := FPen.Width div 2;
  if FDrawY = 0 then
    FDrawY := 1;

  case FShapeType of
    stRectangle : DrawRectangle;
    stRectangleRound : DrawRectangleRound;
    stEllipse : DrawEllipse;
    stCircle : DrawCircle;
    stTriangle : DrawTriangle;
    stArrowSmall : DrawArrowSmall;
    stArrowLarge : DrawArrowLarge;
    stTrapezoid : DrawTrapezoid;
    stHexagon : DrawHexagon;
    stOctagon : DrawOctagon;
    stLine : DrawLine;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
  Refresh;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
  Refresh;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.SetShapeType(const Value: TShapeType);
begin
  if FShapeType <> Value then
  begin
    FShapeType := Value;
    Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.SetShapeDirection(
  const Value: TShapeDirection);
begin
  if FShapeDirection <> Value then
  begin
    FShapeDirection := Value;
    Repaint;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DoChange(Sender: TObject);
begin
  Repaint;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawRectangle;
begin
  Canvas.Rectangle(FDrawX, FDrawY, Width-FDrawX, Height-FDrawY);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawRectangleRound;
begin
  Canvas.RoundRect(FDrawX, FDrawY, Width-FDrawX, Height-FDrawY,
    Width div 4, Height div 4);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawArrowSmall;
begin
  with Canvas do
  begin
    if FShapeDirection = sdUp then
    begin
      MoveTo(Width div 2, Height);
      LineTo(Width div 2, FDrawX);
      LineTo(0, Width div 2);
      MoveTo(Width div 2, FDrawX);
      LineTo(Width, Width div 2);
    end
    else if FShapeDirection = sdDown then
    begin
      MoveTo(Width div 2, 0);
      LineTo(Width div 2, Height-FDrawY);
      LineTo(0, Height-(Width div 2));
      MoveTo(Width div 2, Height-FDrawY);
      LineTo(Width-1, Height-(Width div 2));
    end
    else if FShapeDirection = sdLeft then
    begin
      MoveTo(Width, Height div 2);
      LineTo(FDrawX, Height div 2);
      LineTo(Height div 2, Height);
      MoveTo(FDrawX, Height div 2);
      LineTo(Height div 2, 0);
    end
    else if FShapeDirection = sdRight then
    begin
      MoveTo(0, Height div 2);
      LineTo(Width-FDrawX, Height div 2);
      LineTo(Width-(Height div 2), Height);
      MoveTo(Width-FDrawX, Height div 2);
      LineTo(Width-(Height div 2), 0);
    end
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawArrowLarge;
var
  pArrow : array[1..7] of TPoint;
  nHTriple, nWTriple, nHHalf, nWHalf : Integer;
begin
  nHTriple := Height div 3;
  nWTriple := Width div 3;
  nHHalf := Height div 2;
  nWHalf := Width div 2;

  if FShapeDirection = sdUp then
  begin
    pArrow[1] := Point(nWHalf, FDrawY);
    pArrow[2] := Point(Width - FDrawX, nWHalf);
    pArrow[3] := Point(2 * nWTriple, nWHalf);
    pArrow[4] := Point(2 * nWTriple, Height - FDrawY);
    pArrow[5] := Point(nWTriple, Height - FDrawY);
    pArrow[6] := Point(nWTriple, nWHalf);
    pArrow[7] := Point(FDrawX, nWHalf);
  end
  else if FShapeDirection = sdDown then
  begin
    pArrow[1] := Point(nWTriple, FDrawY);
    pArrow[2] := Point(2 * nWTriple, FDrawY);
    pArrow[3] := Point(2 * nWTriple, Height - nWHalf);
    pArrow[4] := Point(Width - FDrawX, Height - nWHalf);
    pArrow[5] := Point(nWHalf, Height - FDrawY);
    pArrow[6] := Point(FDrawX, Height - nWHalf);
    pArrow[7] := Point(nWTriple, Height - nWHalf);
  end
  else if FShapeDirection = sdLeft then
  begin
    pArrow[1] := Point(FDrawX, nHHalf);
    pArrow[2] := Point(Height - nHHalf, FDrawY);
    pArrow[3] := Point(Height - nHHalf, nHTriple);
    pArrow[4] := Point(Width - FDrawX, nHTriple);
    pArrow[5] := Point(Width - FDrawX, 2 * nHTriple);
    pArrow[6] := Point(Height - nHHalf, 2 * nHTriple);
    pArrow[7] := Point(Height - nHHalf, Height - FDrawY);
  end
  else
  begin
    pArrow[1] := Point(FDrawX, nHTriple);
    pArrow[2] := Point(Width - (Height - nHHalf), nHTriple);
    pArrow[3] := Point(Width - (Height - nHHalf), FDrawY);
    pArrow[4] := Point(Width - FDrawX, nHHalf);
    pArrow[5] := Point(Width - (Height - nHHalf), Height - FDrawY);
    pArrow[6] := Point(Width - (Height - nHHalf), 2 * nHTriple);
    pArrow[7] := Point(FDrawX, 2 * nHTriple);
  end;

  Canvas.Polygon(pArrow);

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawCircle;
var
  X, Y, X2, Y2, nMin : Integer;
begin
  if Width <= Height then
  begin
    nMin := Width;
    X := FDrawX;
    Y := (Height div 2) - (nMin div 2);
    X2 := Width - FDrawX;
    Y2 := (Height div 2) + (nMin div 2);
  end
  else
  begin
    nMin := Height;
    X := (Width div 2) - (nMin div 2);
    Y := FDrawY;
    X2 := (Width div 2) + (nMin div 2);
    Y2 := Height - FDrawY;
  end;

  Canvas.Ellipse(X, Y, X2, Y2);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawEllipse;
begin
  Canvas.Ellipse(FDrawX, FDrawY, Width-FDrawX, Height-FDrawY);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawHexagon;
var
  pHex : array[1..6] of TPoint;
  nHQuad, nWQuad, nHHalf, nWHalf : Integer;
begin
  nHQuad := Height div 4;
  nWQuad := Width div 4;
  nHHalf := Height div 2;
  nWHalf := Width div 2;

  if (FShapeDirection = sdLeft) or (FShapeDirection = sdRight) then
  begin
    pHex[1] := Point(3 * nWQuad, FDrawY);
    pHex[2] := Point(Width - FDrawX, nHHalf);
    pHex[3] := Point(3 * nWQuad, Height - FDrawY);
    pHex[4] := Point(nWQuad, Height - FDrawY);
    pHex[5] := Point(FDrawX, nHHalf);
    pHex[6] := Point(nWQuad, FDrawY);
  end
  else
  begin
    pHex[1] := Point(Width - FDrawX, 3 * nHQuad);
    pHex[2] := Point(nWHalf, Height - FDrawY);
    pHex[3] := Point(FDrawX, 3 * nHQuad);
    pHex[4] := Point(FDrawX, nHQuad);
    pHex[5] := Point(nWHalf, FDrawY);
    pHex[6] := Point(Width - FDrawX, nHQuad);
  end;

  Canvas.Polygon(pHex);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawOctagon;
var
  pOct : array[1..8] of TPoint;
  nHTriple, nWTriple : Integer;
begin
  nHTriple := Height div 3;
  nWTriple := Width div 3;

  pOct[1] := Point(nWTriple, FDrawY);
  pOct[2] := Point(2 * nWTriple, FDrawY);
  pOct[3] := Point(Width - FDrawX, nHTriple);
  pOct[4] := Point(Width - FDrawX, 2 * nHTriple);
  pOct[5] := Point(2 * nWTriple, Height - FDrawY);
  pOct[6] := Point(nWTriple, Height - FDrawY);
  pOct[7] := Point(FDrawX, 2 * nHTriple);
  pOct[8] := Point(FDrawX, nHTriple);

  Canvas.Polygon(pOct);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawTrapezoid;
var
  pTrap : array[1..4] of TPoint;
  nHTriple, nWTriple : Integer;
begin
  nHTriple := Height div 3;
  nWTriple := Width div 3;

  if FShapeDirection = sdUp then
  begin
    pTrap[1] := Point(nWTriple, FDrawY);
    pTrap[2] := Point(2 * nWTriple, FDrawY);
    pTrap[3] := Point(Width - FDrawX, Height - FDrawY);
    pTrap[4] := Point(FDrawX, Height - FDrawY);
  end
  else if FShapeDirection = sdDown then
  begin
    pTrap[1] := Point(FDrawX, FDrawY);
    pTrap[2] := Point(Width - FDrawX, FDrawY);
    pTrap[3] := Point(2 * nWTriple, Height - FDrawY);
    pTrap[4] := Point(nWTriple, Height - FDrawY);
  end
  else if FShapeDirection = sdLeft then
  begin
    pTrap[1] := Point(FDrawX, nHTriple);
    pTrap[2] := Point(Width - FDrawX, FDrawY);
    pTrap[3] := Point(Width - FDrawX, Height - FDrawY);
    pTrap[4] := Point(FDrawX, 2 * nHTriple);
  end
  else
  begin
    pTrap[1] := Point(FDrawX, FDrawY);
    pTrap[2] := Point(Width - FDrawX, nHTriple);
    pTrap[3] := Point(Width - FDrawX, 2 * nHTriple);
    pTrap[4] := Point(FDrawX, Height - FDrawY);
  end;

  Canvas.Polygon(pTrap);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawTriangle;
var
  pTrip : array[1..3] of TPoint;
  nWHalf, nHHalf : Integer;
begin
  nHHalf := Height div 2;
  nWHalf := Width div 2;

  if FShapeDirection = sdUp then
  begin
    pTrip[1] := Point(nWHalf, FDrawY);
    pTrip[2] := Point(Width - FDrawX, Height - FDrawY);
    pTrip[3] := Point(FDrawX, Height - FDrawY);
  end
  else if FShapeDirection = sdDown then
  begin
    pTrip[1] := Point(FDrawX, FDrawY);
    pTrip[2] := Point(Width - FDrawX, FDrawY);
    pTrip[3] := Point(nWHalf, Height - FDrawY);
  end
  else if FShapeDirection = sdLeft then
  begin
    pTrip[1] := Point(Width - FDrawX, FDrawY);
    pTrip[2] := Point(Width - FDrawX, Height - FDrawY);
    pTrip[3] := Point(FDrawX, nHHalf);
  end
  else
  begin
    pTrip[1] := Point(FDrawX, FDrawY);
    pTrip[2] := Point(Width - FDrawX, nHHalf);
    pTrip[3] := Point(FDrawX, Height - FDrawY);
  end;

  Canvas.Polygon(pTrip);

end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHCustomShape.DrawLine;
var
  nWHalf, nHHalf, XStart, YStart, XEnd, YEnd : Integer;
begin
  nHHalf := Height div 2;
  nWHalf := Width div 2;

  if (FShapeDirection = sdUp) or (FShapeDirection = sdDown) then
  begin
    XStart := nWHalf;
    YStart := 0;
    XEnd := nWHalf;
    YEnd := Height;
  end
  else
  begin
    XStart := 0;
    YStart := nHHalf;
    XEnd := Width;
    YEnd :=nHHalf;
  end;

  Canvas.MoveTo(XStart, YStart);
  Canvas.LineTo(XEnd, YEnd);


end;

end.
