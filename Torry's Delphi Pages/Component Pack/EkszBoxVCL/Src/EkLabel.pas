unit EkLabel;

//=================
//  EkLabel
//=================

// Copyright (C) 2007-2008 Kernel Master
// Author: Kernel Master
// kmeksz[At]yahoo.com

//==================================================================

//{$WARNINGS OFF}
//{$HINTS OFF}
{$O+} // Optimizations

//==================================================================

interface

uses
	Windows, Classes, Controls, Graphics, Messages, EkTypes;


type
	TEkLabel = class(TGraphicControl)

	private
    FTextEllipsis     : TEkTextEllipsis;
		FColor1, FColor2  : TColor;
    FText1, FText2    : String;
    FTextLength       : Integer;

    procedure DrawText(pX : Integer);
    procedure SetColor1(value : TColor);
    procedure SetColor2(value : TColor);
    procedure SetText1(value : String);
    procedure SetText2(value : String);
    procedure SetTextEllipse(value : TEkTextEllipsis);
    procedure TextCanvasChanged(Sender : TObject);

  protected

		procedure Paint; override;
    procedure WMEraseBkgnd(var Msg: TWMEraseBkgnd); message WM_ERASEBKGND;

  public
		constructor Create(AOwner: TComponent); override;
		destructor  Destroy; override;

	published
    property Action;
    property DragCursor;
    property DragKind;
    property DragMode;
    property ParentShowHint;
    property Anchors;
    property PopupMenu;

    property Visible;
	  property ShowHint;
	  property ParentFont;
    property Font;
    property TextEllipse : TEkTextEllipsis read FTextEllipsis write SetTextEllipse default etlNone;
    property Color1 : TColor read FColor1 write SetColor1 default clBlack;
    property Color2 : TColor read FColor2 write SetColor2 default clRed;
    property Text1 : String read FText1 write SetText1;
    property Text2 : String read FText2 write SetText2;

	end;

procedure Register;

implementation

//==============================================================================

constructor TEkLabel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

ControlStyle := ControlStyle + [csOpaque];

Canvas.OnChange := TextCanvasChanged;
FTextEllipsis := etlNone;
FColor1 := clBlack;
FColor2 := clRed;
FText1 := 'Ek';
FText2 := 'Label';
Width := 40;
Height := 13;

end;

//==============================================================================

destructor TEkLabel.Destroy;
begin
   
inherited Destroy;
end;

//==============================================================================

procedure TEkLabel.Paint;
begin
  if not Visible then Exit;
  DrawText(0);
end;

//==============================================================================

procedure TEkLabel.DrawText(pX : Integer);
const
  ELLIPSIS : String = '...';
var
  txt1, txt2 : String;
  txt1Len, txt2Len : Integer;
  rect1, rect2 : TRect;
begin

SetBkMode(Canvas.Handle, 1); // Transparent

Canvas.Font.Assign(Font);
Canvas.Font.Color := FColor1;

txt1 := FText1;
txt2 := FText2;
txt1Len := Canvas.TextWidth(FText1);
txt2Len := Canvas.TextWidth(FText2);
FTextLength := txt1Len + txt2Len;

if (FTextEllipsis = etlEndEllipsis) and (Canvas.TextWidth(txt1+txt2) > Width) then
begin
  while Canvas.TextWidth(txt1+txt2) > Width - Canvas.TextWidth(ELLIPSIS)
  do

    if Length(txt2) > 0 then
    begin
      SetLength(txt2, txt2Len);
      Dec(txt2Len);
    end
    else
    begin
      SetLength(txt1, txt1Len);
      Dec(txt1Len);
    end;

    if txt1Len = Canvas.TextWidth(FText1) then // txt1 has not been cut
    begin
      txt2 := txt2 + ELLIPSIS;
      //txt2Len := Canvas.TextWidth(txt2);
    end
    else
      txt1 := txt1 + ELLIPSIS;
    txt1Len := Canvas.TextWidth(txt1);

end;


// Text1
rect1.Left := pX;
rect1.Top := 0;
rect1.Right := rect1.Left + txt1Len;
rect1.Bottom := Height;
ExtTextOut(Canvas.Handle, rect1.Left, 0, ETO_CLIPPED,
@rect1, PChar(txt1), txt1Len, nil);

// Text2
if txt1Len = Canvas.TextWidth(FText1) then // txt1 has not been cut, so show txt2
begin
  rect2.Left := rect1.Right; //+ Spacing
  rect2.Top := 0;
  rect2.Right := rect2.Left + Canvas.TextWidth(txt2);
  rect2.Bottom := Height;
  Canvas.Font.Color := FColor2;
  ExtTextOut(Canvas.Handle, rect2.Left, 0, ETO_CLIPPED,
  @rect2, PChar(txt2), Length(txt2), nil);
end;

end;

//==============================================================================

procedure TEkLabel.SetColor1(value : TColor);
begin
  if value <> FColor1 then
  begin
    FColor1 := value;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkLabel.SetColor2(value : TColor);
begin
  if value <> FColor2 then
  begin
    FColor2 := value;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkLabel.SetText1(value : String);
begin
  if value <> FText1 then
  begin
    FText1 := value;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkLabel.SetText2(value : String);
begin
  if value <> FText2 then
  begin
    FText2 := value;
    Invalidate;
  end;
end;

//==============================================================================

procedure TEkLabel.SetTextEllipse(value : TEkTextEllipsis);
begin
  if value <> FTextEllipsis then
    begin
      FTextEllipsis := value;
      Invalidate;
    end;
end;

//==============================================================================

procedure TEkLabel.TextCanvasChanged(Sender : TObject);
begin
  Invalidate;
end;

//==============================================================================

procedure TEkLabel.WMEraseBkgnd(var Msg: TWMEraseBkgnd);
begin
  Msg.Result := 1;
end;

//==============================================================================

procedure Register;
begin
	RegisterComponents('EkszBoxVCL', [TEkLabel]);
end;

end.
