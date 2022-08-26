unit hhHTMLcol;

{ ----------------------------------------------------------------------
Unit to display and select the strict Netscape series HTML colours.

Author: Howard Harvey
Dated:  24/MAY/1999
Email:  hharvey@dove.net.au

This component is freeware - send me an email to acknowledge your use.
Long live Freeware! 
Give to the net freely and you'll get from it freely.
If you charge, you in turn deserve to be charged!

Each colour (R,G,B) has values from the set [$00,$33,$66,$99,$CC,$FF]
such that #RRGGBB=$000000 is clBlack and #RRGGBB=$FFFFFF is clWhite
with all other colours between.

This unit shows the 216 (6x6x6) colours in a 18x12 matrix.  An extra
row at the bottom displays the grey scale (16 greys from clBlack to
clWhite) made up from the set [$00,$11,$22,...,$DD,$EE,$FF] for each
R,G,B set (eg mid grey is $888888)

Initial values are stored in the Tcolor values "fgPCcol" and "bgPCcol"
Results for text (fg) and background (bg) are returned as properties
in two formats:
    (1) Tcolor values "fgPCcol" and "bgPCcol" (where red is in LSB)
    (2) HTML longints "fgHTMLcol" and "bgMTMLcol" (where blue is in LSB)
---------------------------------------------------------------------- }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons;

type
  ThhHTMLColorForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image1: TImage;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Timer1: TTimer;
    procedure fgKillOutline ;
    procedure bgKillOutline ;
    procedure fgDrawOutline( X, Y : integer ) ;
    procedure bgDrawOutline( X, Y : integer ) ;
    procedure ClosestColour( InColour : TColor ; var X,Y : integer ) ;
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  end;

  ThhHTMLColorDialog = class(TComponent)
  private
    bgSPen   : TPenStyle ;
    fgSPen   : TPenStyle ;
    bgPcolor : Tcolor;
    bgHcolor : longint;
    fgPcolor : Tcolor;
    fgHcolor : longint;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean;
  published
    property bgPCCol   : Tcolor  read bgPcolor write bgPcolor;
    property bgHTMLCol : longint read bgHcolor write bgHcolor;
    property fgPCCol   : Tcolor  read fgPcolor write fgPcolor;
    property fgHTMLCol : longint read fgHcolor write fgHcolor;
    property bgOutline : TPenStyle read bgSPen write bgSPen default psDot;
    property fgOutline : TPenStyle read fgSPen write fgSPen default psSolid;
  end;

  procedure Register ;

implementation

{$R *.DFM}

var
  r,g,b      : integer ;
  bgIcolor   : longint ;
  bgJcolor   : TColor ;
  fgIcolor   : longint ;
  fgJcolor   : TColor ;
  bgMyPen    : TPenStyle ;
  fgMyPen    : TPenStyle ;
  bgOdrawn   : boolean = (false) ;
  fgOdrawn   : boolean = (false) ;
  fgOline  : array [0..4] of TPoint ;
  bgOline  : array [0..4] of TPoint ;
  ForeGround : boolean =(false) ;

procedure ExtractRGB( ColorNum : longint ; VAR Red,Green,Blue : integer ) ;

begin
  Red   :=  ColorNum AND $FF ;
  Green := (ColorNum SHR  8) AND $FF ;
  Blue  := (ColorNum SHR 16) AND $FF ;
end ;

function ColorToHTML( InColor : TColor ) : longint ;
var
  r,g,b : integer ;
begin
  ExtractRGB( ColorToRGB(InColor),r,g,b ) ;
  ColorToHTML := (r SHL 16) +(g SHL 8) + b ;
end ;

procedure ThhHTMLColorForm.ClosestColour( InColour : TColor ; var X,Y : integer ) ;
var
  InRed    : integer ;
  InGreen  : integer ;
  InBlue   : integer ;
  CkRed    : integer ;
  CkGreen  : integer ;
  CkBlue   : integer ;
  CkColour : longint ;
  Diff     : longint ;
  Xindex   : integer ;
  Yindex   : integer ;
  Ystart   : integer ;
  MinDiff  : longint ;

begin
  MinDiff := 257*257*257 ;
  X := -1 ;
  Y := -1 ;
  CkColour := ColorToRGB( InColour ) ;
  ExtractRGB( CkColour , InRed, InGreen, InBlue ) ;

{ Greys use only the bottom line, all others the full matrix }

  if (InRed = InGreen) and (InRed = InBlue)
  then Ystart := 12
  else Ystart := 0 ;

{ Scan the matrix for a near match }

  for Yindex := Ystart to 12 do
  begin
    for Xindex := 0 to 17 do
    begin

{ Get colour in the block clicked }

      CkColour := ColorToRGB(Canvas.Pixels[Xindex*18+8,Yindex*18+8]) ;
      ExtractRGB( CkColour , CkRed, CkGreen, CkBlue ) ;
      Diff := (CkRed-InRed)*(CkRed-InRed)
            + (CkGreen-InGreen)*(CkGreen-InGreen)
            + (CkBlue-InBlue)*(CkBlue-InBlue) ;
      if Diff < MinDiff then
      begin
        MinDiff := Diff ;
        X := Xindex ;
        Y := Yindex ;
        if (MinDiff = 0) then EXIT ;
      end ;
    end ;
  end ;
end ;

constructor ThhHTMLColorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  bgOutline  := psDot ;
  fgOutline  := psSolid ;
  ForeGround := false ;
end;

function ThhHTMLColorDialog.Execute: Boolean;
begin
  with ThhHTMLColorForm.Create(Application) do
  try
    bgJcolor := bgPcolor ;
    bgIcolor := ColorToHTML( bgJcolor ) ;
    fgJcolor := fgPcolor ;
    fgIcolor := ColorToHTML( fgJcolor ) ;
    Panel1.color := bgJcolor ;
    Panel1.Font.Color := fgJColor ;
    Panel1.caption := Format( 'fg=#%6.6x bg=#%6.6x' , [fgIcolor,bgIcolor]) ;

    bgMyPen := bgSPen ;
    fgMyPen := fgSPen ;
    Result := (ShowModal = mrOk);

{ Update results if OK return }

    if Result
    then begin
      bgPcolor := bgJcolor ;
      bgHcolor := bgIcolor ;
      fgPcolor := fgJcolor ;
      fgHcolor := fgIcolor ;
    end ;
  finally
    Free;
  end;
end;

procedure ThhHTMLColorForm.fgKillOutline ;

begin
  Canvas.Pen.Width := 1 ;
  if fgOdrawn
  then begin
    Canvas.Pen.Style := psSolid ;
    Canvas.Pen.Color := clSilver ;
    Canvas.PolyLine( fgOline ) ;
    fgOdrawn := false ;
  end ;
end ;

procedure ThhHTMLColorForm.bgKillOutline ;

begin
  Canvas.Pen.Width := 1 ;
  if bgOdrawn
  then begin
    Canvas.Pen.Style := psSolid ;
    Canvas.Pen.Color := clSilver ;
    Canvas.PolyLine( bgOline ) ;
    bgOdrawn := false ;
  end ;
end ;

procedure ThhHTMLColorForm.fgDrawOutline( X, Y : integer ) ;
var
  Yext : integer ;
  Xext : integer ;
  ImgColor : TColor ;

begin
  Canvas.Pen.Width := 1 ;
  Xext := 16 ;
  Yext := 16 ;
  if (X >= 0) AND (Y >= 0)
  then begin
    if (Y=12) AND (X IN [0,1,16,17])
    then begin
      if (X=1) or (X=17) then X := X-1 ;
      Xext := Xext+18;
    end ;
    X := X*18 + 1 ;
    Y := Y*18 + 1 ;
    fgOline[0].X := X ;      fgOline[0].Y := Y ;
    fgOline[1].X := X+Xext ; fgOline[1].Y := Y ;
    fgOline[2].X := X+Xext ; fgOline[2].Y := Y+Yext ;
    fgOline[3].X := X ;      fgOline[3].Y := Y+Yext ;
    fgOline[4].X := X ;      fgOline[4].Y := Y ;

{ Get colour in the block clicked }

    ImgColor := ColorToRGB(Canvas.Pixels[x+8,y+8]) ;

{ Break down to colours and force to $11 boundaries }

    ExtractRGB( ImgColor , r , g , b ) ;
    r := ((r + 8) DIV $11) * $11 ;
    g := ((g + 8) DIV $11) * $11 ;
    b := ((b + 8) DIV $11) * $11 ;

    if (r+g+b < 384) AND (fgMyPen =  psSolid)
    then Canvas.Pen.Color := clWhite
    else Canvas.Pen.Color := clBlack ;
    Canvas.Pen.Style := fgMyPen ;
    Canvas.PolyLine( fgOline ) ;
    fgOdrawn := true ;
   end ;
end ;

procedure ThhHTMLColorForm.bgDrawOutline( X, Y : integer ) ;
var
  Yext : integer ;
  Xext : integer ;
  ImgColor : TColor ;

begin
  Canvas.Pen.Width := 1 ;
  Xext := 16 ;
  Yext := 16 ;
  if (X >= 0) AND (Y >= 0)
  then begin
    if (Y=12) AND (X IN [0,1,16,17])
    then begin
      if (X=1) or (X=17) then X := X-1 ;
      Xext := Xext+18;
    end ;
    X := X*18 + 1 ;
    Y := Y*18 + 1 ;
    bgOline[0].X := X ;      bgOline[0].Y := Y ;
    bgOline[1].X := X+Xext ; bgOline[1].Y := Y ;
    bgOline[2].X := X+Xext ; bgOline[2].Y := Y+Yext ;
    bgOline[3].X := X ;      bgOline[3].Y := Y+Yext ;
    bgOline[4].X := X ;      bgOline[4].Y := Y ;

{ Get colour in the block clicked }

    ImgColor := ColorToRGB(Canvas.Pixels[x+8,y+8]) ;

{ Break down to colours and force to $11 boundaries }

    ExtractRGB( ImgColor , r , g , b ) ;
    r := ((r + 8) DIV $11) * $11 ;
    g := ((g + 8) DIV $11) * $11 ;
    b := ((b + 8) DIV $11) * $11 ;

    if (r+g+b < 384) AND (bgMyPen =  psSolid)
    then Canvas.Pen.Color := clWhite
    else Canvas.Pen.Color := clBlack ;
    Canvas.Pen.Style := bgMyPen ;
    Canvas.PolyLine( bgOline ) ;
    bgOdrawn := true ;
  end ;
end ;

procedure ThhHTMLColorForm.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ImgColor : longint ;

{ Get colour at mouse and process it }

begin
  ForeGround := (Button = mbRight) ;
  with ThhHTMLcolorDialog do
  begin
    x := (x DIV 18);
    if x > 17 then x := 17 ;
    x := x*18 + 9 ;
    y := (y DIV 18);
    if y > 12 then y := 12 ;
    y := y*18 + 9 ;

{ Get colour in the block clicked }

    ImgColor := ColorToRGB(Canvas.Pixels[x,y]) ;

{ Break down to colours and force to $11 boundaries }

    ExtractRGB( ImgColor , r , g , b ) ;
    r := ((r + 8) DIV $11) * $11 ;
    g := ((g + 8) DIV $11) * $11 ;
    b := ((b + 8) DIV $11) * $11 ;

 { Convert back to colour, save in various formats and display }

    if ForeGround
    then begin
      bgKillOutline;
      fgKillOutline;
      bgDrawOutline( (bgOline[0].X+9) DIV 18,
                     (bgOline[0].Y+9) DIV 18) ;
      fgDrawOutline( x DIV 18,y DIV 18) ;
      fgJcolor := Tcolor((b SHL 16) + (G SHL 8) + r) ;
      fgIcolor := ColorToHTML( fgJcolor ) ;
      Panel1.Font.Color := fgJColor ;
    end
    else begin
      bgKillOutline;
      fgKillOutline;
      fgDrawOutline( (fgOline[0].X+9) DIV 18,
                     (fgOline[0].Y+9) DIV 18) ;
      bgDrawOutline( x DIV 18,y DIV 18) ;
      bgJcolor := Tcolor((b SHL 16) + (G SHL 8) + r) ;
      bgIcolor := ColorToHTML( bgJcolor ) ;
      Panel1.color := bgJcolor ;
    end ;
  end ;

{ Display as an HTML formatted string }

  Panel1.caption := Format( 'fg=#%6.6x bg=#%6.6x' , [fgIcolor,bgIcolor]) ;

end;

procedure ThhHTMLColorForm.SpeedButton1Click(Sender: TObject);
begin
  ModalResult := mrOK ;
end;

procedure ThhHTMLColorForm.SpeedButton2Click(Sender: TObject);
begin
  ModalResult := mrCancel ;
end;

procedure Register ;
begin
  RegisterComponents('Howie',[ThhHTMLColorDialog] ) ;
end ;

procedure ThhHTMLColorForm.Timer1Timer(Sender: TObject);
var
  X,Y : integer ;

begin
  Timer1.enabled := false ;
  ClosestColour( bgJColor , X , Y ) ;
  bgDrawOutline( X , Y ) ;
  ClosestColour( fgJColor , X , Y ) ;
  fgDrawOutline( X , Y ) ;
end;

end.
