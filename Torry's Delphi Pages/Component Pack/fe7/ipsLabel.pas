unit ipsLabel;


// nefunguje flip ... daco hlupe
// AutoCaption neakceptuje diakritiku

interface

uses Classes, Windows, Graphics, Controls, StdCtrls, Forms,
     Dialogs, ipsBrush, ipsShadow;

const MaxPoints      = 5000;
      ParseLineCount = 5;
type



  TpsAutoCaption = (acNone, acDateTime, acPriceUp, acPriceDown,
        acCompanyName, acUserName, acComputerName);

  TpsBgBitmapMode = (bmStretch, bmTile);

  TpsTextCurveParams = record
        x,y   : Integer;
        R     : TRect;
        Angle : Integer;
        Size  : Integer;
  end;

  TpsLabelEffect=(csStandard, cs1, cs2, cs3, cs4, cs5, cs6, cs7, cs8, cs9, cs10,
        cs11,cs12,cs13,cs14,cs15);

  TpsLabel = class(TGraphicControl)
  private
         FPen         : TpsPen;
         FBrush       : TpsBrush;
         FBgBrush     : TpsBrush;
         FCaption     : String;
         FpsFont      : TpsFeFont;
         FTransparent: Boolean;
         FAlignment: TAlignment;
         FAlignmentVertical: TTextLayout;
         FAngle: Integer;
    FShadow: TpsShadow;
    FStyle: TpsBrushFillStyle;
    FPar3: Integer;
    FPar1: Integer;
    FPar2: Integer;
    FLabelEffect: TpsLabelEffect;
    FSplineDetails: Integer;
    FFlipHorizontal: Boolean;
    FFlipVertical: Boolean;
    FFlipEffectV: Boolean;
    FFlipEffectH: Boolean;
    FAutoCaptionFormat: String;
    FAutoCaption: TpsAutoCaption;

    FAfterDecimal : Boolean;
    FFillMode: TFillMode;

         sa,ca : Extended;
         FBgBitmap: TBitmap;
    FBgBitmapMode: TpsBgBitmapMode;
    FBgGradient: TpsGradient;
    FGradient: TpsGradient;

         procedure SetPen(const Value: TpsPen);
         procedure SetBrush(const Value: TpsBrush);
         procedure SetpsFont(const Value: TpsFeFont);
         procedure SetAlignment(const Value: TAlignment);
         procedure SetAlignmentVertical(const Value: TTextLayout);
         procedure SetTransparent(const Value: Boolean);
         procedure SetAngle(const Value: Integer);
         procedure SetShadow(const Value: TpsShadow);
         procedure SetStyle(const Value: TpsBrushFillStyle);
         procedure SetPar1(const Value: Integer);
         procedure SetPar2(const Value: Integer);
         procedure SetPar3(const Value: Integer);
         procedure SetLabelEffect(const Value: TpsLabelEffect);
         procedure SetCaption(const Value: String);
         procedure SetFlipHorizontal(const Value: Boolean);
         procedure SetFlipVertical(const Value: Boolean);
         procedure SetFlipEffectH(const Value: Boolean);
         procedure SetFlipEffectV(const Value: Boolean);
         procedure SetAutoCaption(const Value: TpsAutoCaption);
         procedure SetAutoCaptionFormat(const Value: String);
         procedure SetFillMode(const Value: TFillMode);
    procedure SetBgBrush(const Value: TpsBrush);
    procedure SetBgBitmap(const Value: TBitmap);
    procedure SetBgBitmapMode(const Value: TpsBgBitmapMode);
    procedure SetBgGradient(const Value: TpsGradient);
    procedure SetGradient(const Value: TpsGradient);
    function  GetAutoCaptionText:String;
  protected
         // for special effects - word art
         CenterLbl, CenterChar, OffsetChar : TPoint;
         CenterText                        : TPoint;
         CharRect,LabelRect                : TRect;
         metrics                           : TGlyphMetrics;
         tm                                : TTextMetricA;

         procedure DrawChar(C: TCanvas; _char: Char; R: TRect);
         procedure SetSplineDetails(const Value: Integer);
         function  TransformPoint(dbl_x, dbl_y: Double): TPoint;
         procedure DrawTextOnCurve(C: TCanvas; S:String; R:TRect);
  public
         constructor Create(AOwner:TComponent); override;
         destructor  Destroy; override;
         procedure   PaintTo(C: TCanvas; PaintRect: TRect); virtual;
         procedure   Paint; override;
         procedure   CorrectPoint(var P:TPoint; var CorrectY:Boolean); virtual;
  published
         // property Canvas;
         property Pen:TpsPen read FPen write SetPen;
         property Brush:TpsBrush read FBrush write SetBrush;
         property BgBrush:TpsBrush read FBgBrush write SetBgBrush;
         property BgBitmap:TBitmap read FBgBitmap write SetBgBitmap;
         property BgGradient:TpsGradient read FBgGradient write SetBgGradient;
         property Gradient:TpsGradient read FGradient write SetGradient;
         property BgBitmapMode:TpsBgBitmapMode read FBgBitmapMode write SetBgBitmapMode;
         property psFont:TpsFeFont read FpsFont write SetpsFont;
         property Align;
         property Alignment:TAlignment read FAlignment write SetAlignment;
         property Transparent:Boolean read FTransparent write SetTransparent;
         property AlignmentVertical:TTextLayout read FAlignmentVertical write SetAlignmentVertical;
         property Color;
         property Angle:Integer read FAngle write SetAngle;
         property Shadow:TpsShadow Read FShadow Write SetShadow;
         property Style:TpsBrushFillStyle read FStyle write SetStyle;

         property Par1:Integer read FPar1 write SetPar1;
         property Par2:Integer read FPar2 write SetPar2;
         property Par3:Integer read FPar3 write SetPar3;

         property LabelEffect:TpsLabelEffect read FLabelEffect write SetLabelEffect;
         property Caption:String read FCaption write SetCaption;
         property SplineDetails:Integer read FSplineDetails write SetSplineDetails;
         property FlipHorizontal:Boolean read FFlipHorizontal write SetFlipHorizontal;
         property FlipVertical:Boolean read FFlipVertical write SetFlipVertical;
         property FlipEffectH:Boolean read FFlipEffectH write SetFlipEffectH;
         property FlipEffectV:Boolean read FFlipEffectV write SetFlipEffectV;

         property AutoCaption:TpsAutoCaption read FAutoCaption write SetAutoCaption;
         property AutoCaptionFormat:String read FAutoCaptionFormat write SetAutoCaptionFormat;
         property AutoCaptionText:String read GetAutoCaptionText;
         property FillMode:TFillMode read FFillMode write SetFillMode;

         property Anchors;

  end;


implementation

uses SysUtils, Math, Registry, ipsGrUtils;

procedure TpsLabel.Paint;
begin
        PaintTo(Canvas, ClientRect);
end;



procedure TpsLabel.PaintTo(C:TCanvas; PaintRect:TRect);
var s,s1,s2 : String;
    si      : TSize;
    H       : THandle;
    dx,dy,hy,i: Integer;
    kx,ky   : Extended;
    dw      : DWord;
    ExtStyle: LongInt;
    R       : TRect;
    _str    : TStringList;

    procedure BasicPaintText2(sh:Boolean);
    var rgn : hrgn;
        x,y : Integer;
    begin
        H := C.Handle;
        BeginPath(H);
        DrawTextOnCurve(C, S, R);
        EndPath(H);

        if (not sh) and (not FbgBitmap.Empty) then begin
           rgn := PathToRegion(H);
           SelectClipRgn(H, rgn);
           if FBgBitmapMode=bmStretch then
                C.StretchDraw(R, FBgBitmap)
           else begin
                x:=0;
                while x<R.Right do begin
                     y := 0;
                     while y<R.Bottom do begin
                        C.Draw(x,y, FBgBitmap);
                        Inc(y, FBgBitmap.Height);
                     end;
                     Inc(x, FBgBitmap.Width);
                end;
           end;
        end
        else if (not sh) and (FGradient.Enabled) then begin
                rgn := PathToRegion(H);
                SelectClipRgn(H, rgn);
                FGradient.Paint(C, R);
                DeleteObject(rgn);
        end else
           case FStyle of
             loOutlined            : StrokePath(H);
             loBrushFilled         : FillPath(H);
             loBrushFilledOutlined : StrokeAndFillPath(H);
           end;
    end;

    procedure LblCreateFont;
    begin
        if FpsFont.AutoWidthMultiLine then
                FpsFont.CreateHandle(C, FAngle, R, S)
        else
                FpsFont.CreateHandle(C, FAngle, R, S1);
        GetTextMetrics(C.Handle, tm);
    end;

begin
   SinCos( DegToRad(FAngle/10), sa, ca);
   LabelRect := PaintRect;

   FAfterDecimal := False;
   BgBrush.FreeHandle;
   Brush.FreeHandle;
   Pen.FreeHandle;

   s2:=FCaption;
   if FAutoCaption <> acNone then
        s2 := GetAutoCaptionText;

   dw       := 100;
   s1       := s2;

   _str := TStringList.Create;
   try
        _str.Text := s2;

        s1 := '';
        for i:=0 to _str.Count-1 do
            if Length(s1)<Length(_str[i]) then
                s1:=_str[i];

        if not Self.Transparent then begin
            if FBgGradient.Enabled then
                FBgGradient.Paint(C, PaintRect)
            else begin
                C.Brush.Handle := FBgBrush.Handle;
                SetBrushOrgEx(C.Handle, FBgBrush.OffsetX, FBgBrush.OffsetY, nil);
                C.FillRect(PaintRect);
                BgBrush.FreeHandle;
            end;
        end;


        C.Pen.Handle   := Pen.Handle;

        SetBkMode(C.Handle, Windows.Transparent);
        if FFillMode=fmAlternate then SetPolyFillMode(C.Handle, ALTERNATE)
        else                          SetPolyFillMode(C.Handle, WINDING);

        hy:=HeightOf(PaintRect) div _str.Count;
        R.Left   := PaintRect.Left;
        R.Right  := PaintRect.Right;
        R.Top    := PaintRect.Top;
        R.Bottom := R.Top + hy;
        lblCreateFont;
        si:=C.TextExtent(S1);

        if not FpsFont.AutoHeight then hy:=tm.tmHeight+FpsFont.LineSpacing;

        for i:=0 to _str.Count-1 do begin
                s        := _str[i];
                R.Left   := PaintRect.Left;
                R.Right  := PaintRect.Right;

                if FpsFont.AutoWidthMultiLine then
                        lblCreateFont;
                si:=C.TextExtent(S);

                case Alignment of
                        taLeftJustify  : ;
                        taCenter       : Inc(R.Left, (WidthOf(R)-si.cx) div 2 );
                        taRightJustify : R.Left := R.Right - si.cx;
                end;

                case FAlignmentVertical of
                        tlTop     : R.Top := PaintRect.Top + i*hy;
                        tlCenter  : R.Top := PaintRect.Top + i*hy+((HeightOf(PaintRect)-(_str.Count*hy)) div 2);
                        tlBottom  : R.Top := PaintRect.Bottom - (_str.Count-i)*hy;
                end;
                R.Bottom := R.Top + hy;



                if Shadow.Enabled then begin
                        SinCos(DegToRad(Shadow.Angle), kx, ky);
                        dx := Round(kx*Shadow.Width);
                        dy := Round(ky*Shadow.Width);

                        C.Pen.Color   := Shadow.Color;
                        C.Brush.Style := bsSolid;
                        C.Brush.Color := Shadow.Color;

                        OffsetRect(R, dx, dy);
                        BasicPaintText2(True);
                        OffsetRect(R,-dx,-dy);
                        Brush.FreeHandle;
                end;

                FAfterDecimal := False;
                Pen.FreeHandle;
                C.Pen.Handle  := Pen.Handle;
                C.Font.Color  := FpsFont.Color;

                C.Brush.style  := bsClear;
                C.Brush.Handle := Brush.Handle;
                SetBrushOrgEx(C.Handle, Brush.OffsetX, Brush.OffsetY, nil);
                BasicPaintText2(False);
        end;
   finally
        _str.Free;
   end;
end;

procedure TpsLabel.SetPen(const Value: TpsPen);
begin
        FPen.Assign(Value);
        Invalidate;
end;

procedure TpsLabel.SetBrush(const Value: TpsBrush);
begin
        FBrush.Assign(Value);
        Invalidate;
end;

constructor TpsLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // ControlStyle := ControlStyle + [csOpaque];
  FCaption:='PSOFT';
  FTransparent := True;

  FAlignment         := taCenter;
  FAlignmentVertical := tlCenter;
  FBgBitmap   := TBitmap.Create;
  FBgGradient := TpsGradient.CreateParent(Self);
  FGradient   := TpsGradient.CreateParent(Self);

  FPen    := TpsPen.CreateParent(Self);
  FBrush  := TpsBrush.CreateParent(Self);
  FBrush.Color := clRed;

  FbgBrush:= TpsBrush.CreateParent(Self);
  FbgBrush.Color := clWhite;
  FbgBrush.Style := bsSolid;
  FpsFont := TpsFeFont.Create(Self);

  FSplineDetails := 10;

  // SetlabelEffects here
  FPar1 := 50;
  FPar2 := 50;
  FPar3 := 50;
  FLabelEffect := csStandard;
  FStyle       := loBrushFilled;

  FShadow := TpsShadow.Create(Self);
  Width   := 200;
  Height  := 40;
end;

destructor TpsLabel.Destroy;
begin
  FBgGradient.Free;
  FGradient.Free;
  FBgBitmap.Free;
  FShadow.Free;
  FpsFont.Free;
  FPen.Free;
  FBrush.Free;
  FbgBrush.Free;
  inherited Destroy;
end;


procedure TpsLabel.SetpsFont(const Value: TpsFeFont);
begin
        FpsFont.Assign(Value);
end;


procedure TpsLabel.SetAlignment(const Value: TAlignment);
begin
        if FAlignment<>Value then begin
                FAlignment := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetAlignmentVertical(const Value: TTextLayout);
begin
        if FAlignmentVertical <> Value then begin
                FAlignmentVertical := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetTransparent(const Value: Boolean);
begin
        if FTransparent<>Value then begin
                FTransparent := Value;
                Invalidate;
        end;
end;


procedure TpsLabel.SetAngle(const Value: Integer);
begin
        if FAngle<>Value then begin
                FAngle := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetShadow(const Value: TpsShadow);
begin
        FShadow.Assign(Value);
end;

procedure TpsLabel.SetStyle(const Value: TpsBrushFillStyle);
begin
        if FStyle<>Value then begin
                FStyle := Value;
                Invalidate;
        end;
end;


procedure TpsLabel.SetPar1(const Value: Integer);
begin
        if FPar1<>Value then begin
                FPar1 := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetPar2(const Value: Integer);
begin
        if FPar2<>Value then begin
                FPar2 := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetPar3(const Value: Integer);
begin
        if FPar3<>Value then begin
                FPar3 := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetLabelEffect(const Value: TpsLabelEffect);
begin
        if FLabelEffect<>Value then begin
                FLabelEffect := Value;
                {case FLabelEffect of
                        cs1..cs4 : begin Par1:=100; Par2:=40; Par3:=0; end;
                        cs5..cs6 : begin Par1:=270; Par2:=180; Par3:= 20; end;
                end;
                }
                Invalidate;
        end;
end;






procedure TpsLabel.SetCaption(const Value: String);
begin
        if FCaption<>Value then begin
                FCaption := Value;
                Invalidate;
        end;
end;

function TpsLabel.TransformPoint(dbl_x, dbl_y: Double): TPoint;
var CorrectY:Boolean;
begin
        Result.X := CharRect.Left - CenterLbl.X +
                    Round( dbl_x + metrics.gmptGlyphOrigin.x);
        Result.Y := Round(dbl_y);

        if FAfterDecimal then begin
            if FAutoCaption=acPriceDown then Result.y := (tm.tmDescent + Result.Y) div 2;
            if FAutoCaption=acPriceUp   then Result.y := - tm.tmDescent + (tm.tmHeight + Result.Y) div 2;
        end;

        CorrectPoint(Result, CorrectY);

        if FFlipVertical then
                Result.Y := tm.tmAscent-tm.tmDescent- Result.Y;
        if FFlipHorizontal then
                Result.X := -Result.X;

        if CorrectY then
                Result.Y := CharRect.Top + tm.tmAscent - Result.Y;

        if FAngle<>0 then begin
            Dec(Result.Y, CenterLbl.Y);
            Result.X := Round(  Result.X * ca + Result.Y*sa);
            Result.Y := Round(- Result.X * sa + Result.Y*ca);
            Inc(Result.Y, CenterLbl.Y);
        end;

        Inc(Result.X, CenterLbl.X);
        // Inc(Result.Y, CenterLbl.Y);
end;


function FixedToDouble(B:TFixed):Double;
begin
     Result := Longint(B)/65536;
end;


procedure TpsLabel.DrawChar(C:TCanvas; _char: Char; R:TRect);
var le, poc_he : DWORD;
    mat     : TMat2;
    PH      : PTTPolygonHeader;
    CU      : PTTPolyCurve;
    POINT   : PPointFx;
    wo      : word;
    Bits    : array[0..MaxPoints] of Byte;

    i,j     : Integer;
    start, BodX  : TPoint;

    ax,ay,bx,by,cx,cy,xx,xy : Double;

    procedure DrawSpline;
    var j:Integer;
        t:Double;
    begin
        for j:=1 to FSplineDetails do begin
                t := j/FSplineDetails;
                xx := (AX-2*BX+CX)*t*t + (2*BX-2*AX)*t + AX;
                xy := (AY-2*BY+CY)*t*t + (2*BY-2*AY)*t + AY;
                BodX := TransformPoint(xx,xy);
                C.LineTo(BodX.X, BodX.Y);
        end;
        ax   := cx;
        ay   := cy;
    end;

begin

  mat.eM11.fract := 0; mat.eM11.value:= 1;
  mat.eM21.fract := 0; mat.eM21.value:= 0;
  mat.eM12.fract := 0; mat.eM12.value:= 0;
  mat.eM22.fract := 0; mat.eM22.value:= 1;

  le := GetGlyphOutline(C.Handle, Ord(_char), GGO_NATIVE,
                  metrics, MaxPoints, @Bits, mat);

  wo := 0;

  // BeginPath(C.Handle);
  while wo<le do begin
        PH := @Bits[wo];
        Inc(wo, SizeOf(PH^));
        poc_he := 0;
        Inc(poc_he, SizeOf(PH^));

        ax := FixedToDouble(PH^.pfxStart.x);
        ay := FixedToDouble(PH^.pfxStart.y);

        Start := TransformPoint(ax,ay);
        C.MoveTo( Start.X, Start.Y);

        while poc_he<PH^.CB do begin
              CU := @Bits[wo];
              Inc(wo,     4 );
              Inc(poc_he, 4 );

           if CU^.wType=TT_PRIM_LINE then
              for i:=1 to cu^.cpfx do begin
                  POINT := @Bits[wo];
                  Inc(wo,    SizeOf(POINT^));
                  Inc(poc_he,SizeOf(POINT^));
                  bx := ax;
                  by := ay;
                  ax := FixedToDouble(Point^.x);
                  ay := FixedToDouble(Point^.y);
                  if FLabelEffect=csStandard then begin
                     BodX :=TransformPoint(ax,ay);
                     C.LineTo(BodX.X, BodX.Y);
                  end else
                     for j:=1 to ParseLineCount do begin
                          cx   := bx + (ax-bx)*j/ParseLineCount;
                          cy   := by + (ay-by)*j/ParseLineCount;
                          BodX := TransformPoint(cx,cy);
                          C.LineTo(BodX.X, BodX.Y);
                     end;
              end;

           if CU^.wType=TT_PRIM_QSPLINE then begin
              for i:=0 to cu^.cpfx-2 do begin
                  POINT := @Bits[wo];
                  Inc(wo,    SizeOf(POINT^));
                  Inc(poc_he,SizeOf(POINT^));

                  bx := FixedToDouble(Point^.x);
                  by := FixedToDouble(Point^.y);

                  POINT := @Bits[wo];
                  if i<cu^.cpfx-2 then begin
                        cx := (bx+FixedToDouble(Point^.x))/2;
                        cy := (by+FixedToDouble(Point^.y))/2;
                  end else begin
                        cx := FixedToDouble(Point^.x);
                        cy := FixedToDouble(Point^.y);
                  end;
                  DrawSpline;
              end;
           end;
        end;

        C.LineTo(Start.X, Start.Y);

        ax := FixedToDouble(PH^.pfxStart.x);
        ay := FixedToDouble(PH^.pfxStart.y);
        CloseFigure(C.Handle);
  end;

  // EndPath(C.Handle);

  {case FStyle of
        loOutlined                : StrokePath(C.Handle);
        loBrushFilled             : FillPath(C.Handle);
        loBrushFilledOutlined     : StrokeAndFillPath(C.Handle);
  end;
  }
end;

procedure TpsLabel.SetSplineDetails(const Value: Integer);
begin
        if FSplineDetails<>Value then begin
                FSplineDetails := Value;
                if FSplineDetails<5    then FSplineDetails := 5;
                if FSplineDetails>1000 then FSplineDetails := 1000;
                Invalidate;
        end;
end;


procedure TpsLabel.DrawTextOnCurve(C:TCanvas; S:String; R:TRect);
var i,w   : Integer;
begin
        CenterLbl       := Point((R.Left+R.Right) div 2,(R.Top+R.Bottom) div 2);
        CharRect.Top    := R.Top;
        CharRect.Bottom := R.Top+50;

        CharRect.Left   := R.Left;
        CharRect.Right  := R.Left+100;

        CenterText.X := CenterLbl.X;
        CenterText.Y := R.top+tm.tmHeight - tm.tmDescent;

        for i:=1 to Length(s) do begin

           if FFlipHorizontal then DrawChar(C, s[Length(s)-i+1], CharRect)
           else                    DrawChar(C, s[i], CharRect);

           Inc(CharRect.Left, Metrics.gmCellIncX + psFont.TextExtraSpace);
           Inc(CharRect.Top , Metrics.gmCellIncY);

           if (not FAfterDecimal) and (FAutoCaption in [acPriceUp, acPriceDown]) then
              if s[i]=DecimalSeparator then begin
                 FAfterDecimal := True;
                 w     := (CharRect.Bottom - CharRect.Top) div 2;

                 case FAutoCaption of
                     acPriceUp   : Dec(CharRect.Bottom, w);
                     acPriceDown : Inc(CharRect.Top, w);
                 end;
              end;

        end;
end;

procedure TpsLabel.SetFlipHorizontal(const Value: Boolean);
begin
        if FFlipHorizontal<>Value then begin
                FFlipHorizontal := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetFlipVertical(const Value: Boolean);
begin
        if FFlipVertical<>Value then begin
                FFlipVertical := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetFlipEffectH(const Value: Boolean);
begin
        if FFlipEffectH<>Value then begin
                FFlipEffectH := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetFlipEffectV(const Value: Boolean);
begin
        if FFlipEffectV<>Value then begin
                FFlipEffectV := Value;
                Invalidate;
        end;
end;

function psIIF(b:Boolean; X1,X2:Integer):Integer;
begin
        if B then Result := X1
        else      Result := X2;
end;


procedure TpsLabel.CorrectPoint(var P:TPoint; var CorrectY:Boolean);
var kx,ky : Integer;
    dx,dy : Integer;
    sx,sy : Integer;
    alpha : Double;
    p1,p2,r : Integer;
    sa,ca,r1,r2 : Extended;
    W, H        : Integer;
begin
   CorrectY := True;
   W := LabelRect.Right-LabelRect.Left;
   H := LabelRect.Bottom-LabelRect.Top;

   case FLabelEffect of
      // O.K.
      cs1 : begin
        dx := psIIF(FFlipEffectH, (W div 2)- P.X, P.X+ W div 2);
        ky := MulDiv( dx, MulDiv( tm.tmAscent, FPar1, 100), W );
        //  ky := MulDiv( ky, tm.tmAscent-P.Y, tm.tmAscent);
        if FFlipEffectV then
                Dec(P.y,  tm.tmAscent);
        ky := MulDiv( ky, P.Y, tm.tmAscent);

        Dec(P.y,  ky);
        if FFlipEffectV then
                Inc(P.y,  tm.tmAscent);
      end;


      cs2 : begin
            dx := CenterLbl.X;
            dy := MulDiv(tm.tmHeight, FPar1, 100);
            ky := MulDiv( psIIF(FFlipEffectH, dx-Abs(P.X),Abs(P.X)), dy, dx);
            ky := MulDiv(ky, P.Y, tm.tmHeight);
            Dec(P.y, ky);
      end;

      cs3 : begin
            dx := CenterLbl.X;
            dy := MulDiv(tm.tmHeight, FPar1, 100);
            ky := MulDiv( psIIF(FFlipEffectH, sqr(Abs(P.X)), sqr(dx) -sqr(Abs(P.X))),
                dy, sqr(dx));
            ky := MulDiv(ky, P.Y, tm.tmHeight);
            Dec(P.y, ky);
      end;

      cs4 : begin
            dx := CenterLbl.X;
            dy := MulDiv(tm.tmHeight, FPar1, 100);
            ky := MulDiv( psIIF(FFlipEffectH, dx-Abs(P.X),Abs(P.X)), dy, dx);
            P.y := MulDiv(P.Y, FPar1,100) ;
            Dec(P.y, ky);
      end;

      // pisanie do kruhu
      cs5 : begin
            sx := W div 2;
            sy := H div 2;
            if FPar3=0 then FPar3:=10;

            R1    := sx - MulDiv(P.Y,FPar3,100);
            R2    := sy - MulDiv(P.Y,FPar3,100);

            dx    := P.X+CenterLbl.X;
            alpha := DegToRad(Par1+(Par2-Par1)*dx/W);
            sinCos(Alpha, sa, ca);
            P.X :=  Round(R1*ca);
            P.Y :=  LabelRect.Top+sy + Round(-R2*sa);
            CorrectY := False;
      end;

      cs6 : begin
            p1 := MulDiv(tm.tmHeight, FPar1, 100);
            p2 := MulDiv(tm.tmHeight, FPar2, 100);
            dx := CenterLbl.X;

            ky := MulDiv(dx-p.x, p1, 2*dx);
            ky := MulDiv(ky, p.y, tm.tmHeight);

            dy := MulDiv(dx+p.x, p2, 2*dx);
            dy := MulDiv(dy, tm.tmHeight-p.y-tm.tmDescent, tm.tmHeight);
            Dec(P.y, ky-dy);
      end;
      cs7 : begin
            p1 := MulDiv(tm.tmHeight, FPar1, 100);
            p2 := MulDiv(tm.tmHeight, FPar2, 100);
            dx := CenterLbl.X;

            ky := MulDiv(p1, Abs(p.x), dx);
            ky := MulDiv(ky, p.y, tm.tmHeight);

            dy := MulDiv(Abs(dx-p.x), p2, 2*dx);
            dy := MulDiv(dy, tm.tmHeight-p.y-tm.tmDescent, tm.tmHeight);
            Dec(P.y, dy+ky);
      end;
      cs8 : begin
            p1 := MulDiv(tm.tmHeight, FPar1, 100);
            p2 := MulDiv(tm.tmHeight, FPar2, 100);
            dx := CenterLbl.X;

            ky := MulDiv(p1, Abs(p.x), dx);
            ky := MulDiv(ky, p.y, tm.tmHeight);

            dy := MulDiv(dx-Abs(p.x), p2, 2*dx);
            dy := MulDiv(dy, tm.tmHeight-p.y-tm.tmDescent, tm.tmHeight);
            Dec(P.y, ky-dy);
      end;
      cs9 : begin
            p1 := WidthOf(LabelRect) div 2;
            p2 := tm.tmHeight div 2;

            dy := Round(p2-p2*(sqrt( 1 - sqr(p.x/p1) )) );
            Dec(P.Y, dy - tm.tmDescent);
            P.Y := MulDiv(P.Y, FPar1, 100);
      end;

      cs10 : begin
             end;
      cs11 : begin
             end;
      cs12 : begin
             end;
      cs13 : begin
             end;
      cs14 : begin
             end;
      cs15 : begin
             end;
   end;
end;



procedure TpsLabel.SetAutoCaption(const Value: TpsAutoCaption);
begin
        if FAutoCaption<>Value then begin
                FAutoCaption := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetAutoCaptionFormat(const Value: String);
begin
        if FAutoCaptionFormat<>Value then begin
                FAutoCaptionFormat := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetFillMode(const Value: TFillMode);
begin
        if FFillMode<>Value then begin
                FFillMode := Value;
                Invalidate;
        end;
end;



procedure TpsLabel.SetBgBrush(const Value: TpsBrush);
begin
  FBgBrush.Assign(Value);
  Invalidate;
end;


procedure TpsLabel.SetBgBitmap(const Value: TBitmap);
begin
  FBgBitmap.Assign(Value);
  Invalidate;
end;

procedure TpsLabel.SetBgBitmapMode(const Value: TpsBgBitmapMode);
begin
        if FBgBitmapMode<>Value then begin
                FBgBitmapMode := Value;
                Invalidate;
        end;
end;

procedure TpsLabel.SetBgGradient(const Value: TpsGradient);
begin
  FBgGradient.Assign(Value);
  Invalidate;
end;

procedure TpsLabel.SetGradient(const Value: TpsGradient);
begin
  FGradient.Assign(Value);
  Invalidate;
end;

function TpsLabel.GetAutoCaptionText: String;
var dw:DWord;
    r : TRegIniFile;
begin
   case FAutoCaption of
        acDateTime     : Result:=FormatDateTime(FAutoCaptionFormat,Now);
        acCompanyName  : begin
                r := TRegIniFile.Create('SOFTWARE\MICROSOFT\MS SETUP (ACME)\');
                try
                        r.RootKey := HKEY_CURRENT_USER;
                        Result := r.ReadString('USER INFO', 'DefCompany', '<none>') ;
                finally
                        r.Free ;
                end;
            end;
        acUserName     : begin
                r := TRegIniFile.Create('SOFTWARE\MICROSOFT\MS SETUP (ACME)\');
                try
                        r.RootKey := HKEY_CURRENT_USER;
                        Result := r.ReadString('USER INFO', 'DefName', '<none>') ;
                finally
                        r.Free ;
                end;
            end;
        acComputerName : begin
                SetLength(Result, 100);
                GetComputerName(PChar(Result), dw) ;
                SetLength(Result, dw);
            end;
   end;
end;

end.

// TFillMode

