unit ipsPrintFn;

interface

uses Classes, Graphics, Windows, Printers, SysUtils, Math;


type

    TmmRect = record
        Left, Top, Right, Bottom : Double;
    end;



        TpsDeviceParams = class
        private
                procedure MMToPixels(x1, y1: Double; var _x1, _y1: Integer);
        public
                Canvas                    : TCanvas;
                DpiX , DpiY               : Integer;
                Margins                   : TRect;
                PageWidth, PageHeight     : Integer;
                PageWidthMM, PageHeightMM : Double;
                PrintableWidthMM, PrintableHeightMM : Double;
                OffsetXmm, OffsetYmm      : Double;
                FZoom                     : Double;
                Papier_Prekryv            : Double;
                procedure Initialize(C:TCanvas; Zoom:Double; MapMode:Integer;
                        bgMode:Integer);
                procedure SetPen(Width:Integer; Style:TPenStyle; Color:TColor);
                procedure SetPenMM(Width: Double; Style: TPenStyle; Color: TColor);
                procedure SetBrush(Color:TColor; Style:TBrushStyle);
                procedure SetFont(Name:String; Size:Integer; Style:TFontStyles);
                procedure SetFontMM(Name:String; Size:Double; Style:TFontStyles);
                procedure SetPrintedPageView(PageX,PageY:Integer);

                procedure RotateFont(Angle:Integer);

                procedure TahajCiaruMM(X1, Y1, X2, Y2: Double);
                procedure TahajRectMM(X1, Y1, X2, Y2: Double; Style:TBrushStyle);
                procedure KresliElipsuMM(X1, Y1, X2, Y2: Double; Style:TBrushStyle);
                procedure KresliVysecMM(X1, Y1, X2, Y2: Double;
                        Start,Stop:Integer; Style:TBrushStyle);
                procedure KresliOblukMM(X1, Y1, X2, Y2: Double;
                        Start,Stop:Integer; Style:TBrushStyle);
                procedure KresliKruhMM(X1,Y1,R:Double);
                procedure DrawTextMM(X1, Y1: Double; Text: String); overload;
                procedure DrawTextMM(X1, Y1: Double; D:Double); overload;
                procedure DrawMemoMM(X1, Y1, X2, Y2: Double; Text: String;
                        Alignment: TAlignment);
                procedure DrawTextMMRect(MM:TmmRect; Text: String;
                    al:TAlignment; Rotated:Boolean=False);
                procedure TahajObojstrannuSipkuMM(x1,x2,y:Double);
                procedure KresliObrazokMM(MM:TmmRect; G: TGraphic);
        end;



    function MMRect(Left,Top,Right,Bottom:Double):TmmRect;

implementation

{ TpsDeviceParams }

procedure NormalizeRect(var R:TRect);
var x:Integer;
begin
        if R.Left>R.Right then begin x:=R.Left; R.Left:=R.Right; R.Right:=x; end;
        if R.Top>R.Bottom then begin x:=R.Top;  R.Top:=R.Bottom; R.Bottom:=x; end;
end;


procedure TpsDeviceParams.Initialize(C: TCanvas; Zoom : Double;
        MapMode:Integer; bgMode:Integer);
begin
        FZoom        := Zoom;
        Canvas       := C;
        SetGraphicsMode(Canvas.Handle, GM_ADVANCED);
        SetMapMode(Canvas.Handle, MapMode);
        SetBkMode(Canvas.Handle, TRANSPARENT);

        DPIX         := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
        DPIY         := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
        Margins.Left := GetDeviceCaps(Canvas.Handle, PHYSICALOFFSETX	);
        Margins.Top  := GetDeviceCaps(Canvas.Handle, PHYSICALOFFSETY	);
        PageWidth    := GetDeviceCaps(Canvas.Handle, PHYSICALWIDTH	);
        PageHeight   := GetDeviceCaps(Canvas.Handle, PHYSICALHEIGHT	);
        PageWidthMM  := Trunc((PageWidth /DPIX)*25.4);
        PageHeightMM := Trunc((PageHeight/DPIY)*25.4);

        PrintableWidthMM  :=Trunc(((PageWidth -2*Margins.Left) /DpiX)*25.4);
        PrintableHeightMM :=Trunc(((PageHeight-2*Margins.Top) /DpiY)*25.4);
end;


procedure TpsDeviceParams.MMToPixels(x1, y1: Double; var _x1, _y1: Integer);
var k:Double;
begin
    if Canvas=Printer.Canvas then k :=1
    else                          k := FZoom;

    _x1 := Trunc((X1+OffsetXmm)*k*DpiX/25.4); // + Margins.Left;
    _y1 := Trunc((Y1+OffsetYmm)*k*DpiY/25.4); // + Margins.Top;
end;


procedure TpsDeviceParams.RotateFont(Angle: Integer);
var LF : TLogFont;
begin
  GetObject(Canvas.Font.Handle, SizeOf(lf), @lf);

  LF.lfEscapement  := Angle*10;
  LF.lfOrientation := Angle*10;

  Canvas.Font.Handle := CreateFontIndirect(LF);
end;

procedure TpsDeviceParams.SetBrush(Color: TColor; Style: TBrushStyle);
begin
        Canvas.Brush.Color := clWhite;
        Canvas.Brush.Style := bsSolid;
end;

procedure TpsDeviceParams.SetFont(Name: String; Size: Integer;
  Style: TFontStyles);
begin
        Canvas.Font.Name := Name;
        Canvas.Font.Size := Size;
        Canvas.Font.Style:= Style;
end;

procedure TpsDeviceParams.SetPen(Width: Integer; Style: TPenStyle;
  Color: TColor);
begin
        Canvas.Pen.Width := Width;
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := clBlack;
end;

procedure TpsDeviceParams.SetPenMM(Width: Double; Style: TPenStyle;
  Color: TColor);
const
    PenStyles: array[TPenStyle] of Word =
            (PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT, PS_NULL,
             PS_INSIDEFRAME);
var br:TLogBrush;
begin
    if Style=psSolid then begin
        Canvas.Pen.Width := Trunc(Width*DPIX/25.4);
        Canvas.Pen.Style := psSolid;
        Canvas.Pen.Color := clBlack;
    end else begin
        br.lbHatch := 0;
        br.lbStyle := BS_SOLID;
        br.lbHatch := 0;
        br.lbColor := ColorToRGB(Color);

        Canvas.Pen.Handle := ExtCreatePen(
                PS_GEOMETRIC
                or PenStyles[Style]
                or PS_ENDCAP_SQUARE or PS_JOIN_BEVEL,
                Trunc(Width*DPIX/25.4),
                br,
                0,
                nil
                );
    end;
end;

procedure TpsDeviceParams.SetPrintedPageView(PageX, PageY: Integer);
var xp,yp : Integer;
begin
        xp := Trunc( PageX*(PrintableWidthMM-Papier_Prekryv)*DpiX/25.4);
        yp := Trunc(PageY*PrintableHeightMM*DpiY/25.4);

        SetViewPortExtEx(Canvas.Handle, PageWidth, PageHeight, nil);
        SetViewPortOrgEx(Canvas.Handle, -xp, -yp, nil);
end;


procedure TpsDeviceParams.TahajCiaruMM(X1, Y1, X2, Y2: Double);
var _x1,_x2,_y1,_y2:Integer;
begin
    try
        MMToPixels(X1,Y1, _x1,_y1);
        MMToPixels(X2,Y2, _x2,_y2);
    except
    end;
    Canvas.MoveTo(_x1,_y1);
    Canvas.LineTo(_x2,_y2);
end;


procedure TpsDeviceParams.DrawTextMM(X1, Y1: Double; Text: String);
var _x1, _y1 : Integer;
begin
    MMtoPixels(X1,Y1, _x1, _y1);
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(_X1, _Y1, Text);
end;


procedure TpsDeviceParams.SetFontMM(Name: String; Size: Double;
  Style: TFontStyles);
begin
        Canvas.Font.Name  := Name;
        Canvas.Font.Style := Style;
        Canvas.Font.Height:= - Trunc(DPIY*size/25.4);
end;

procedure TpsDeviceParams.DrawMemoMM(X1, Y1, X2, Y2: Double; Text: String;
  Alignment: TAlignment);
var R:TRect;
    A:Integer;
begin
    MMtoPixels(X1,Y1, R.Left,  R.Top);
    MMtoPixels(X2,Y2, R.Right, R.Bottom);

    A:=0;
    case Alignment of
        taLeftJustify : A := A + DT_LEFT;
        taCenter      : A := A + DT_CENTER;
        taRightJustify: A := A + DT_RIGHT;
    end;

    DrawText(Canvas.Handle, PChar(Text), Length(Text), R, A);
end;


procedure TpsDeviceParams.DrawTextMM(X1, Y1, D: Double);
begin
        if D<>-1 then
                DrawTextMM(X1, Y1, Format('%6.2f',[D]));
end;

procedure TpsDeviceParams.TahajRectMM(X1, Y1, X2, Y2: Double; Style:TBrushStyle);
var R:TRect;
begin
    try
        MMToPixels(X1,Y1, R.Left, R.Top);
        MMToPixels(X2,Y2, R.Right,R.Bottom);
    except
    end;

    NormalizeRect(R);
    Canvas.Brush.Style := Style;
    if Style=bsClear then Canvas.Brush.Color := clWhite
    else                  Canvas.Brush.Color := clBlack;
    Canvas.Pen.Color   := clBlack;
    Canvas.Rectangle(R);
end;

procedure TpsDeviceParams.KresliKruhMM(X1, Y1, R: Double);
var _x1,_y1,_x2,_y2 : Integer;
begin
    try
        MMToPixels(X1-R,Y1-R, _x1,_y1);
        MMToPixels(X1+R,Y1+R, _x2,_y2);
    except
    end;
    Canvas.Brush.Style := bsClear;
    Canvas.Ellipse(_x1,_y1,_x2,_y2);
end;


procedure TpsDeviceParams.KresliElipsuMM(X1, Y1, X2, Y2: Double;
  Style: TBrushStyle);
var R:TRect;
begin
    try
        MMToPixels(X1,Y1, R.Left, R.Top);
        MMToPixels(X2,Y2, R.Right,R.Bottom);
    except
    end;

    NormalizeRect(R);
    Canvas.Brush.Style := Style;
    Canvas.Ellipse(R);
end;

procedure TpsDeviceParams.KresliVysecMM(X1, Y1, X2, Y2: Double; Start,
  Stop: Integer; Style: TBrushStyle);
var _x1,_y1,_x2,_y2 : Integer;
begin
    try
        MMToPixels(X1,Y1, _x1,_y1);
        MMToPixels(X2,Y2, _x2,_y2);
    except
    end;
    Canvas.Brush.Style := Style;
    if Style=bsClear then Canvas.Brush.Color := clWhite
    else                  Canvas.Brush.Color := clBlack;

    Canvas.Pie(_x1, _y1,
               _x2, _y2,
               _x1, (_y1+_y2) div 2,
               _x2, (_y1+_y2) div 2);
end;

procedure TpsDeviceParams.KresliOblukMM(X1, Y1, X2, Y2: Double; Start,
  Stop: Integer; Style: TBrushStyle);
var _x1,_y1,_x2,_y2, _x3,_y3, _x4, _y4 : Integer;
    sa,ca : Extended;
    rx,ry : Integer;
begin
    try
        MMToPixels(X1,Y1, _x1,_y1);
        MMToPixels(X2,Y2, _x2,_y2);
    except
    end;
    Canvas.Brush.Style := Style;
    if Style=bsClear then Canvas.Brush.Color := clWhite
    else                  Canvas.Brush.Color := clBlack;

    rx := _x2-_x1;
    ry := _y2-_y1;

    SinCos( DegToRad(Start), sa, ca);
    _x3 := ((_x1+_x2) div 2) + Trunc(ca*rx);
    _y3 := ((_y1+_y2) div 2) + Trunc(sa*ry);

    SinCos( DegToRad(Stop), sa, ca);
    _x4 := ((_x1+_x2) div 2) + Trunc(ca*rx);
    _y4 := ((_y1+_y2) div 2) + Trunc(sa*ry);
    Canvas.Arc(_x1, _y1,
               _x2, _y2,
               _x3, _y3,
               _x4, _y4);
end;

procedure TpsDeviceParams.DrawTextMMRect(MM:TmmRect; Text: String;
    Al:TAlignment; Rotated:Boolean=False);
var R : TRect;
    w,h : Integer;
begin
    MMtoPixels(MM.Left,MM.Top, R.Left,  R.Top);
    MMtoPixels(MM.Right,MM.Bottom, R.Right, R.Bottom);
    NormalizeRect(R);

    w:=Canvas.TextWidth(Text);
    h:=Canvas.TextHeight(Text);
    Canvas.Brush.Style := bsClear;

    if Rotated then begin
        // Inc(R.Left,  (R.Right-R.Left-h) div 2);

        case Al of
            taLeftJustify  : ;
            taCenter       : Dec(R.Bottom, (R.Bottom-R.Top-w) div 2);
            taRightJustify : R.Bottom:= R.Bottom- w;
        end;
        Canvas.TextOut(R.Left, R.Bottom, Text);
    end else begin
        w:=Canvas.TextWidth(Text);
        h:=Canvas.TextHeight(Text);

        Inc(R.Top,  (R.Bottom-R.Top -h) div 2);

        case Al of
            taLeftJustify  : ;
            taCenter       : Inc(R.Left, (R.Right -R.Left-w) div 2);
            taRightJustify : R.Left := R.Right - w;
        end;
        Canvas.TextOut(R.Left, R.Top, Text);
    end;


end;


function MMRect(Left,Top,Right,Bottom:Double):TmmRect;
begin
    Result.Left   := Left;
    Result.Top    := Top;
    Result.Right  := Right;
    Result.Bottom := Bottom;
end;

procedure TpsDeviceParams.TahajObojstrannuSipkuMM(x1, x2, y: Double);
begin
    TahajCiaruMM(x1,y, x2,   y);
    TahajCiaruMM(x1,y, x1+3, y-0.5);
    TahajCiaruMM(x1,y, x1+3, y+0.5);
    TahajCiaruMM(x2,y, x2-3, y-0.5);
    TahajCiaruMM(x2,y, x2-3, y+0.5);
end;

procedure TpsDeviceParams.KresliObrazokMM(MM: TmmRect; G: TGraphic);
var _x1,_y1,_x2,_y2 : Integer;
begin
    try
        MMToPixels(MM.Left, MM.Top,    _x1,_y1);
        MMToPixels(MM.Right,MM.Bottom, _x2,_y2);
    except
    end;

    Canvas.Draw( {Rect(}_x1,_y1{, _x2,_y2)}, G);

end;

end.
