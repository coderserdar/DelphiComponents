// The original code of this unit is written by E.Z. Jordens.

// I only modified following two,
//  - Added a function Line2 to use real type parameters for cooridinate.
//  - Renamed TRGBTriple to TRGBTriple2 to avoid possible conflict because TRGBTriple
//     is defined with different item names in Windows.pas.
//
//   by Silhwan Hyun  (hyunsh@hanafos.com)

unit EZLines;

interface

uses
  Windows, math, Graphics, SysUtils;


var
  eex  : integer;
const
  N3   : double = 3;
  N0   : double = 0;
  N05  : double = 0.5;
  N255 : double = 255;
  N04  : double = 0.499999999999999;

type
  PRGBTriple = ^TRGBTriple2;
  TRGBTriple2 = packed record
     B: byte;  {easier to type than rgbtBlue}
     G: byte;
     R: byte;
  end;
  pRGBTripleArray = ^TRGBTripleArray;
  TRGBTripleArray = array[0..32767] of TRGBTriple2;

  PByte = ^TByte;
  TByte = byte;
  pByteArray = ^TByteArray;
  TByteArray = array[0..32767] of TByte;

  TEZAV = record
    Rdis,Sdis,Rreal:double;
    L,sqrL,Yab,Yba,Xba:double;
    E : Double;
  end;

  TEZLine = class
  private
    EZAV      : TEZAV;
    FColor    : TColor;
    FWidth    : double;
    FAA       : boolean;
    FPenS     : TPenStyle;
    FPenM     : TPenMode;
    FTrans    : double;
    FStartDis : double;
    FCutOff   : boolean;
    procedure SetDisConst(XA,YA,XB,YB:double);
    function  DistanceLine(XC,YC,XA,YA,XB,YB,W:Double;Sw:Boolean): double;
    function  Line(X1,Y1,X2,Y2:Integer;Bitmap:TBitmap;StP,EndP:Boolean;An1,An2,An0:double;StartDis:Double):double;overload;
    function  Line2(X1,Y1,X2,Y2:single;Bitmap:TBitmap;StP,EndP:Boolean;An1,An2,An0:double;StartDis:Double):double;overload;
    function  LineCut(X0,Y0,X1,Y1,X2,Y2,X3,Y3:Integer;StartP,EndP:boolean;Bitmap:TBitmap;StartDis:double):double;
  public
    constructor Create; virtual;

    procedure SetProp(Color:TColor;LineWidth,Transparancy,StartDistance:Double;AntiAlias,CutOff:Boolean;PenStyle:TPenStyle;PenMode:TPenMode);

    function  Bezier(X1,Y1,X2,Y2,X3,Y3,X4,Y4:Integer;Bitmap:TBitmap):double;
    procedure Ellips(X1,Y1,X2,Y2:Integer;Bitmap:TBitmap);
    function  Line(X1,Y1,X2,Y2:Integer;Bitmap:TBitmap):double;overload;
    function  Line2(X1,Y1,X2,Y2:single;Bitmap:TBitmap):double;overload;
    function  PolyLine(Arr:array of TPoint;Bitmap:TBitmap):double;
    procedure Recangle(X1,Y1,X2,Y2:Integer;Bitmap:TBitmap);
    procedure SuperShape(X0,Y0:integer;Radius,Angle,M,N1,N2,N3,Phi,Increment:Double;Bitmap:TBitmap);

    property  LineColor:TColor        read FColor                write FColor;
    property  LineWidth:double        read FWidth                write FWidth;
    property  AntiAlias:boolean       read FAA                   write FAA;
    property  PenStyle:TPenStyle      read FPenS                 write FPenS;
    property  PenMode:TPenMode        read FPenM                 write FPenM;
    property  PenTransparency:double  read FTrans                write FTrans;
    property  StartDistance:double    read FStartDis             write FStartDis;
    property  UseCutoff:boolean       read FCutOff               write FCutOff;
  end;

implementation
////////////////////////////////////////////////////////////////////////////////
function TruncC(D:Double):integer;
// Just a Trunc
asm
   Push    esp
   FLD     D
   FSUB    N04
   FISTP   dword ptr [esp]
   POP     eax
end;
//-----------------------------------------------------------------------------
function RoundC(D:Double):integer;
// Just a Round
asm
   Push    esp
   FLD     D
   FISTP   dword ptr [esp]
   POP     eax
end;
//-----------------------------------------------------------------------------
procedure Swap(var A, B: Integer);
// Swaps the values A and B
asm
   mov   ecx,[eax]
   xchg  ecx,[edx]
   mov  [eax],ecx
end;
//------------------------------------------------------------------------------
function Distance(X1,Y1,X2,Y2:Double):double;
// Result := SQRT(SQR(X1-X2)+SQR(Y1-Y2));
asm
   FLD   X1
   FLD   X2
   FSUB
   FLD   st(0)
   FMUL
   FLD   Y1
   FLD   Y2
   FSUB
   FLD   st(0)
   FMUL
   FADD
   FSQRT
   FWAIT
end;
//------------------------------------------------------------------------------
function DisL(X1,Y1,X2,Y2,X3,Y3,L:Double):Double;
// result := ((Y1-Y2)*Y3-(X1-X2)*X3)/L;
asm
   FLD   Y1
   FLD   Y2
   FSUB
   FLD   Y3
   FMUL
   FLD   X1
   FSUB  X2
   FLD   X3
   FMUL
   FSUB
   FLD   L
   FDIV
   FWAIT
end;
//------------------------------------------------------------------------------
function NormCol(Col:TColor):TColor;
// checks if a color is a system color. If so, it will return the actual color. If not it just returns the color;
asm
    cmp  eax,0
    jge  @End
    add  eax,$80000000
    push eax
    call getsyscolor
    @End:
end;
//------------------------------------------------------------------------------
function Limit(Val,Min,Max:integer):integer;
// Limits the value val to a minimum or maximimum of min/max
asm
    cmp   eax,edx
    jl    @Min
    cmp   eax,ecx
    jg    @Max
    jmp   @End
  @Max:
    mov   eax,ecx
    jmp   @End
  @Min:
    mov   eax,edx
  @End:
end;
function Limit2(Val,Min,Max:single):single;
// Limits the value val to a minimum or maximimum of min/max
begin
   if Val < Min then
      result := Min
   else if Val > Max then
      result := Max
   else
      result := Val;
end;
//------------------------------------------------------------------------------
function GetB(D,W:Double):byte;
// Retuns the pixel byte value depending the distance from the line relative to the line width
asm
    fld      W
    fsub     D
    fst      D
    fcomp    N3
    fstsw    ax
    sahf
    jb       @NotBig
    mov      al,$ff
    jmp      @End
  @NotBig:
    fld      D
    fcomp    N0
    fstsw    ax
    sahf
    jnb      @SinV
    xor      eax,eax
    jmp      @End
  @SinV:
    FLD      D
    FLD      N05
    FMUL
    FSIN
    FLD      st(0)
    FMUL
    FLD      N255
    FMUL
    FWAIT
    Push     esp
    FSUB     N04
    FISTP    dword ptr [esp]
    POP      eax
  @End:
end;
//------------------------------------------------------------------------------
// The following combine the pixel byte values for RGB according to the TPenMode, adjusting the resulting color  by the intensity of B
function Black(Bgr,B:integer):integer;
asm
  xor  edx,$ff
  imul eax,edx
  shr  eax,$08
end;
//------------------------------------------------------------------------------
function Copy(Col,Bgr,B:integer):integer;
asm
  imul eax,ecx
  xor  ecx,$ff
  imul ecx,edx
  add  eax,ecx
  shr  eax,$08
end;
//------------------------------------------------------------------------------
function NotC(Col,Bgr,B:integer):integer;
asm
  mov   eax,$ff
  sub   eax,edx
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function NotCopy(Col,Bgr,B:integer):integer;
asm
  xor  eax,$ff
  imul eax,ecx
  xor  ecx,$ff
  imul ecx,edx
  add  eax,ecx
  shr  eax,$08
end;
//------------------------------------------------------------------------------
function MergePenNot(Col,Bgr,B:integer):integer;
asm
  mov   eex,$ff
  sub   eex,edx
  or    eax,eex
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function MaskPenNot(Col,Bgr,B:integer):integer;
asm
  not   eax
  or    eax,edx
  not   eax
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function MergeNotPen(Col,Bgr,B:integer):integer;
asm
  mov  eex,$ff
  sub  eex,eax
  mov  eax,eex
  or   eax,edx
  imul eax,ecx
  xor  ecx,$ff
  imul ecx,edx
  add  eax,ecx
  shr  eax,$08
end;
//------------------------------------------------------------------------------
function MaskNotPen(Col,Bgr,B:integer):integer;
asm
  mov  eex,$ff
  sub  eex,edx
  or   eax,eex
  mov  eex,$ff
  sub  eex,eax
  mov  eax,eex
  imul eax,ecx
  xor  ecx,$ff
  imul ecx,edx
  add  eax,ecx
  shr  eax,$08
end;
//------------------------------------------------------------------------------
function Merge(Col,Bgr,B:integer):integer;
asm
  or    eax,edx
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function NotMerge(Col,Bgr,B:integer):integer;
asm
  mov   eex,$ff
  or    eax,edx
  sub   eex,eax
  mov   eax,eex
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function Mask(Col,Bgr,B:integer):integer;
asm
  and   eax,edx
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function NotMask(Col,Bgr,B:integer):integer;
asm
  and   eax,edx
  mov   eex,$ff
  sub   eex,eax
  mov   eax,eex
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function XorC(Col,Bgr,B:integer):integer;
asm
  xor   eax,edx
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function NotXor(Bgr,Col,B:integer):integer;
asm
  xor   eax,edx
  mov   eex,$ff
  sub   eex,eax
  mov   eax,eex
  imul  eax,ecx
  xor   ecx,$ff
  imul  ecx,edx
  add   eax,ecx
  shr   eax,$08
end;
//------------------------------------------------------------------------------
function ColPenMode(Bgr,Col,B:byte;PenMode:TPenMode):byte;
begin
 Result := Bgr;
 case PenMode of
   pmNop           : Result := Bgr;
   pmBlack         : Result := Black(Bgr,B);
   pmWhite         : Result := Copy($ff,Bgr,B);
   pmCopy          : Result := Copy(Col,Bgr,B);
   pmNot           : Result := NotC(0,Bgr,B);
   pmNotCopy       : Result := NotCopy(Col,Bgr,B);
   pmMergePenNot   : Result := MergePenNot(Col,Bgr,B);
   pmMaskPenNot    : Result := MaskPenNot(Col,Bgr,B);
   pmMergeNotPen   : Result := MergeNotPen(Col,Bgr,B);
   pmMaskNotPen    : Result := MaskNotPen(Col,Bgr,B);
   pmMerge         : Result := Merge(Col,Bgr,B);
   pmNotMerge      : Result := NotMerge(Col,Bgr,B);
   pmMask          : Result := Mask(Col,Bgr,B);
   pmNotMask       : Result := NotMask(Col,Bgr,B);
   pmXor           : Result := XorC(Col,Bgr,B);
   pmNotXor        : Result := NotXor(Col,Bgr,B);
 end;
end;
//------------------------------------------------------------------------------
function ColByPenMode(Bgr,Col:TRGBTriple2;B:byte;PenMode:TPenMode):TRGBTriple2;
begin
  Result.B := ColPenMode(Bgr.B,Col.B,B,PenMode);
  Result.G := ColPenMode(Bgr.G,Col.G,B,PenMode);
  Result.R := ColPenMode(Bgr.R,Col.R,B,PenMode);
end;
//------------------------------------------------------------------------------
function Interrupt(D:Double;StartDis:Double;PenStyle:TPenStyle):double;
  //------
  function Da(D:double):double;
  var
   J :double;
  begin
    J := abs(3*Sin(D/4))-0.5;
    if (J > 1) then J := 1;
    if (J < 0) then J := 0;
    Result := J;
  end;
  //------
  function DaDo(D:double):double;
  var
   J,K :double;
  begin
    J := Sin(D);
    K := 3*Sin(D/3);
    if (K > 1) then K := 1;
    if (K > 0) then J := k;
    if (J < 0) then J := 0;
    Result := J;
  end;
  //------
  function Dot(D:double):double;
  var
   J :double;
  begin
    J := Sin(D)*2;
    if (J < 0) then J := 0;
    if (J > 1) then J := 1;
    Result := J;
  end;
  //------
  function DaDoDo(D:double):double;
  var
   J,K :double;
  begin
    J := Sin(D-2);
    K := 3*Sin(D/4);
    if (K > 1) then K := 1;
    if (K > 0) then J := k;
    if (J < 0) then J := 0;
    Result := J;
  end;
  //------
begin
  Result := 0;
  D := D+StartDis;
  Case Penstyle of
    psInsideFrame: Result := 1;
    psSolid      : Result := 1;
    psDot        : Result := Dot(D);
    psDash       : Result := Da(D);
    psDashDot    : Result := DaDo(D);
    psDashDotDot : Result := DaDoDo(D);
  end;
end;
//------------------------------------------------------------------------------
function Outside(X1,X2,Y1,Y2,Dif:integer;Bitmap:TBitmap):boolean;
begin
 result := false;
 if ((X1 >= Bitmap.Width+Dif) and (X2 >= Bitmap.Width+Dif)) or ((X1 < -Dif) and (X2 < -Dif)) or
    ((Y1 >= Bitmap.Height+Dif) and (Y2 >= Bitmap.Height+Dif)) or ((Y1 < -Dif) and (Y2 < -Dif))
    then result := true;
end;
function Outside2(X1,X2,Y1,Y2,Dif:single;Bitmap:TBitmap):boolean;
begin
 result := false;
 if ((X1 >= Bitmap.Width+Dif) and (X2 >= Bitmap.Width+Dif)) or ((X1 < -Dif) and (X2 < -Dif)) or
    ((Y1 >= Bitmap.Height+Dif) and (Y2 >= Bitmap.Height+Dif)) or ((Y1 < -Dif) and (Y2 < -Dif))
    then result := true;
end;
////////////////////////////////////////////////////////////////////////////////
constructor TEZLine.Create;
begin
    FColor    := clRed;
    FWidth    := 2;
    FAA       := True;
    FPenS     := psSolid;
    FPenM     := pmCopy;
    FTrans    := 1;
    FStartDis := 0;
    FCutOff   := True;
end;
//------------------------------------------------------------------------------
procedure TEZLine.SetDisConst(XA,YA,XB,YB:double);
begin
  with EZAV do begin
    L   := Distance(XA,YA,XB,YB);
    sqrL:= SQR(L);
    Yab := YA-YB;
    Yba := YB-YA;
    Xba := XB-XA;
  end;
end;
//------------------------------------------------------------------------------
function TEZLine.DistanceLine(XC,YC,XA,YA,XB,YB,W:Double;Sw:Boolean): double;
var
  R,S: Double;
begin
  Result     := 0;
  R          := 0;
  S          := 0;
  EZAV.Rdis  := 0;
  EZAV.Sdis  := 0;
  EZAV.Rreal := 0;

  if (EZAV.L <> 0) then begin
    R := DisL(XA,YA,XC,YC,EZAV.Xba,EZAV.Yab,EZAV.sqrL);
    S := DisL(XA,YA,XC,YC,EZAV.Yba,EZAV.Xba,EZAV.sqrL);
    if (R >= 0) and (R <= 1) then Result := abs(EZAV.L*S)
    else if (R > 1) then Result := Distance(XB,YB,XC,YC)
    else if (R < 0) then Result := Distance(XA,YA,XC,YC);
  end else Result := Distance(XA,YA,XC,YC);

  if Sw then EZAV.Rdis := (EZAV.L*(1-R)) else EZAV.Rdis := (EZAV.L*R);
  EZAV.Sdis := S*EZAV.L;
  EZAV.Rreal := R;
end;
//------------------------------------------------------------------------------
function LineCutOff(StP,EndP,Sw:Boolean;An1,An2,An0:double;I,J,X1,Y1,X2,Y2:Integer):boolean;
  //-------------
  function LineRespect(XA,YA,XB,YB:integer;An:double):boolean;
  var
   A : double;
  begin
    if Sw then A := -(YA-YB)*An-(XA-XB)
    else A := (YA-YB)-(XA-XB)*An;

    if Sw and (An0 >= 1E30) then A := (YA-YB)-(XA-XB)*An;

    if (A >= 0) then Result := False else Result := True;

    if Sw then begin
      if (An <= 0) then begin
         if (An0 <> 0) and (An0 < 1E30) then if (An > -1/An0) then Result := not Result;
      end else begin
         if (An0 <> 0) then if (An > -1/An0) and (An0 < 1E30) then Result := not Result;
      end;
    end else begin
      if (An <= 0) then begin
         if not (An0 = 0) and (An < An0) then Result := not Result;
         if (An0 < 0) then Result := not Result;
      end else if (An > 0) then begin
         if (An0 <= 0) then Result := not Result;
         if (An < An0) then Result := not Result;
      end else;
    end;

    if (A > -0.001) and (A < 0.001) then Result := False;

  end;
  //-------------
begin
  Result := True;
  if not EndP then begin
    if not Sw then begin
       if LineRespect(I,J,X2,Y2,An2) then Result := false
       else Result := True;
    end else begin
       if LineRespect(I,J,X1,Y1,An2) then Result := false
       else Result := True;
    end;
  end;

  if Result and not StP then begin
    if not Sw then begin
       if LineRespect(I,J,X1,Y1,An1) then Result := True
       else Result := False;
    end else begin
       if LineRespect(I,J,X2,Y2,An1) then Result := True
       else Result := False;
    end;
 end;
end;
function LineCutOff2(StP,EndP,Sw:Boolean;An1,An2,An0:double;I,J:integer;X1,Y1,X2,Y2:single):boolean;
  //-------------
  function LineRespect(XA,YA:integer; XB,YB:single;An:double):boolean;
  var
   A : double;
  begin
    if Sw then A := -(YA-YB)*An-(XA-XB)
    else A := (YA-YB)-(XA-XB)*An;

    if Sw and (An0 >= 1E30) then A := (YA-YB)-(XA-XB)*An;

    if (A >= 0) then Result := False else Result := True;

    if Sw then begin
      if (An <= 0) then begin
         if (An0 <> 0) and (An0 < 1E30) then if (An > -1/An0) then Result := not Result;
      end else begin
         if (An0 <> 0) then if (An > -1/An0) and (An0 < 1E30) then Result := not Result;
      end;
    end else begin
      if (An <= 0) then begin
         if not (An0 = 0) and (An < An0) then Result := not Result;
         if (An0 < 0) then Result := not Result;
      end else if (An > 0) then begin
         if (An0 <= 0) then Result := not Result;
         if (An < An0) then Result := not Result;
      end else;
    end;

    if (A > -0.001) and (A < 0.001) then Result := False;

  end;
  //-------------
begin
  Result := True;
  if not EndP then begin
    if not Sw then begin
       if LineRespect(I,J,X2,Y2,An2) then Result := false
       else Result := True;
    end else begin
       if LineRespect(I,J,X1,Y1,An2) then Result := false
       else Result := True;
    end;
  end;

  if Result and not StP then begin
    if not Sw then begin
       if LineRespect(I,J,X1,Y1,An1) then Result := True
       else Result := False;
    end else begin
       if LineRespect(I,J,X2,Y2,An1) then Result := True
       else Result := False;
    end;
 end;
end;
//------------------------------------------------------------------------------
function TEZLine.Line(X1,Y1,X2,Y2:Integer;Bitmap:TBitmap;StP,EndP:Boolean;An1,An2,An0:double;StartDis:Double):double;
var
I,J,dif,dy1,dy2,dx1,dx2,X1a,Y1a,X2a,Y2a: Integer;
D,Lw,AnT: Double;
Clrt   : TRGBTriple2;
Row    : pRGBTripleArray;
B,By   : byte;
Sw,Ep  : boolean;
Col    : TColor;
  //-------------
  Function SetPix(Pix:TRGBTriple2;B:Byte;Col:TRGBTriple2):TRGBTriple2;
  begin
     By := TruncC(Interrupt(EZAV.Rdis,StartDis,FPenS)*B*FTrans);
     if (B > 0) and (By > 0) then Result := ColByPenMode(Pix,Col,By,FPenM)
     else Result := Pix;
  end;
  //-------------
begin
  Result := StartDis + Distance(X1,Y1,X2,Y2);
  if not AntiAlias then exit;
  if LineWidth < 0.01 then Exit;
  if LineWidth < 1 then LW := FWidth - (1-FWidth)
  else Lw := FWidth;
  if Outside(X1,X2,Y1,Y2,Round(2+LW/2),Bitmap) then exit;

  Col := NormCol(FColor);
  if ((X1 >= X2) and (Y1 > Y2)) or (X1 <= X2) and (Y1 >= Y2) then begin
    Swap(X1,X2);
    Swap(Y1,Y2);
    Sw := true;
  end else Sw := false;

  if not FCutOff then begin
     StP  := True;
     EndP := True;
  end;

  LW := LW+2;
  dif := TruncC(1+LW/2);

  with clrt do begin
    R := GetRValue(Col);
    G := GetGValue(Col);
    B := GetBValue(Col);
  end;

  SetDisConst(X1,Y1,X2,Y2);

  X1a := Limit(X1,Dif,Bitmap.Width -1-dif);
  X2a := Limit(X2,Dif,Bitmap.Width -1-dif);
  Y1a := Limit(Y1,Dif,Bitmap.Height-1-Dif);
  Y2a := Limit(Y2,Dif,Bitmap.Height-1-Dif);

 if ((X2-X1)*(Y2-Y1) >= 0) and ((Y2-Y1) <> 0) then begin
  dy1 := Y1a-Dif;
  dy2 := Y2a+dif+1;
  dx1 := X1a-Dif;
  dx2 := X2a+dif+1;
  J   := dy1;
  repeat
    Row := Bitmap.ScanLine[J];
    I   := dx1;
    repeat
      D := DistanceLine(I,J,X1,Y1,X2,Y2,LW,Sw)*2;
      B := GetB(D,LW);
      Ep:= LineCutOff(StP,EndP,Sw,An1,An2,An0,I,J,X1,Y1,X2,Y2);

      if Ep then Row[I] := SetPix(Row[I],B,Clrt);

      if (EZAV.SDis < -LW/2+1) then dx1 := I;
      if (EZAV.Sdis > LW/2)    then I := dx2;

      Inc(I);
    until (I >= dx2);
    Inc(J);
  until (J >= dy2);
 end else begin
  dy1 := Y1a-dif;
  dy2 := Y2a+dif+1;
  dx1 := X1a+Dif;
  dx2 := X2a-dif-1;
  J   := dy1;
  repeat
    Row := Bitmap.ScanLine[J];
    I   := dx1;
   repeat
      D := DistanceLine(I,J,X1,Y1,X2,Y2,LW,Sw)*2;
      B := GetB(D,LW);
      Ep:= LineCutOff(StP,EndP,Sw,An1,An2,An0,I,J,X1,Y1,X2,Y2);

      if Ep then Row[I] := SetPix(Row[I],B,Clrt);

      if (EZAV.SDis >  LW/2-1) then dx1 := I;
      if (EZAV.Sdis < -LW/2)   then I := dx2;

      I := I-1;
    until (I <= dx2);
    Inc(J);
  until (J >= dy2);
 end;
end;

function TEZLine.Line2(X1,Y1,X2,Y2:single;Bitmap:TBitmap;StP,EndP:Boolean;An1,An2,An0:double;StartDis:Double):double;
var
I,J,{dif,}dy1,dy2,dx1,dx2{,X1a,Y1a,X2a,Y2a}: Integer;
dif,X1a,Y1a,X2a,Y2a : single;
D,Lw,AnT: Double;
Clrt   : TRGBTriple2;
Row    : pRGBTripleArray;
B,By   : byte;
Sw,Ep  : boolean;
Col    : TColor;
tmpVal : single;

  //-------------
  Function SetPix(Pix:TRGBTriple2;B:Byte;Col:TRGBTriple2):TRGBTriple2;
  begin
     By := TruncC(Interrupt(EZAV.Rdis,StartDis,FPenS)*B*FTrans);
     if (B > 0) and (By > 0) then Result := ColByPenMode(Pix,Col,By,FPenM)
     else Result := Pix;
  end;
  //-------------
begin
  Result := StartDis + Distance(X1,Y1,X2,Y2);
  if not AntiAlias then exit;
  if LineWidth < 0.01 then Exit;
  if LineWidth < 1 then LW := FWidth - (1-FWidth)
  else Lw := FWidth;
  if Outside2(X1,X2,Y1,Y2,Round(2+LW/2),Bitmap) then exit;

  Col := NormCol(FColor);
  if ((X1 >= X2) and (Y1 > Y2)) or (X1 <= X2) and (Y1 >= Y2) then begin
  //  Swap(X1,X2);
    tmpVal := X1;
    X1 := X2;
    X2 := tmpVal;

  //  Swap(Y1,Y2);
    tmpVal := Y1;
    Y1 := Y2;
    Y2 := tmpVal;
    Sw := true;
  end else Sw := false;

  if not FCutOff then begin
     StP  := True;
     EndP := True;
  end;

  LW := LW+2;
 // dif := TruncC(1+LW/2);
  dif := 1+LW/2;

  with clrt do begin
    R := GetRValue(Col);
    G := GetGValue(Col);
    B := GetBValue(Col);
  end;

  SetDisConst(X1,Y1,X2,Y2);

  X1a := Limit2(X1,Dif,Bitmap.Width -1-dif);
  X2a := Limit2(X2,Dif,Bitmap.Width -1-dif);
  Y1a := Limit2(Y1,Dif,Bitmap.Height-1-Dif);
  Y2a := Limit2(Y2,Dif,Bitmap.Height-1-Dif);

 if ((X2-X1)*(Y2-Y1) >= 0) and ((Y2-Y1) <> 0) then begin
  dy1 := round(Y1a-Dif);
  dy2 := round(Y2a+dif+1);
  dx1 := round(X1a-Dif);
  dx2 := round(X2a+dif+1);
  J   := dy1;
  repeat
    Row := Bitmap.ScanLine[J];
    I   := dx1;
    repeat
      D := DistanceLine(I,J,X1,Y1,X2,Y2,LW,Sw)*2;
      B := GetB(D,LW);
      Ep:= LineCutOff2(StP,EndP,Sw,An1,An2,An0,I,J,X1,Y1,X2,Y2);

      if Ep then Row[I] := SetPix(Row[I],B,Clrt);

      if (EZAV.SDis < -LW/2+1) then dx1 := I;
      if (EZAV.Sdis > LW/2)    then I := dx2;

      Inc(I);
    until (I >= dx2);
    Inc(J);
  until (J >= dy2);
 end else begin
  dy1 := round(Y1a-dif);
  dy2 := round(Y2a+dif+1);
  dx1 := round(X1a+Dif);
  dx2 := round(X2a-dif-1);
  J   := dy1;
  repeat
    Row := Bitmap.ScanLine[J];
    I   := dx1;
   repeat
      D := DistanceLine(I,J,X1,Y1,X2,Y2,LW,Sw)*2;
      B := GetB(D,LW);
      Ep:= LineCutOff2(StP,EndP,Sw,An1,An2,An0,I,J,X1,Y1,X2,Y2);

      if Ep then Row[I] := SetPix(Row[I],B,Clrt);

      if (EZAV.SDis >  LW/2-1) then dx1 := I;
      if (EZAV.Sdis < -LW/2)   then I := dx2;

      I := I-1;
    until (I <= dx2);
    Inc(J);
  until (J >= dy2);
 end;
end;

//------------------------------------------------------------------------------
function TEZLine.LineCut(X0,Y0,X1,Y1,X2,Y2,X3,Y3:Integer;StartP,EndP:boolean;Bitmap:TBitmap;StartDis:double):double;
  //------
  function LimDo(A,B:integer):double;
  begin
    Result := 0;
    if (A <> 0) then Result := B/A
    else if (A = 0) then result := 1E30;
  end;
  //------
  function Sign(A:double):integer;
  begin
    if (A >= 0) then Result := 1
    else Result := -1;
  end;
  //------
  function AvAn(A,B:double;S1,S2,S3:integer):double;
  var
    U,Z : double;
  begin
   U := B;
   B := ArcTan(B)+2*Pi;
   if (S1 = -1) then B := B-Pi;
   if (S2 = -1) and (U >= 1E30) then B :=  1.5*Pi;
   if (S2 =  1) and (U >= 1E30) then B := -1.5*Pi;

   Z := A;
   U := A;
   A := ArcTan(A)+2*Pi;
   if (S3 = -1) and (U >= 1E30) then A :=  1.5*Pi;
   if (S3 =  1) and (U >= 1E30) then A := -1.5*Pi;

   A := (A+B)/2;
   if (Z > 0) then A := A+0.5*Pi;
   Result := Tan(A);
  end;
  //------
var
  An2,An1,A : double;
begin
  An1 := 0;
  An2 := 0;
  A   := LimDo((X2-X1),(Y2-Y1));
  if not StartP then An1 := AvAn(A,LimDo((X1-X0),(Y1-Y0)),Sign(X1-X0),Sign(Y1-Y0),Sign(Y2-Y1));
  if not EndP   then An2 := AvAn(A,LimDo((X3-X2),(Y3-Y2)),Sign(X3-X2),Sign(Y3-Y2),Sign(Y2-Y1));
  Result := Line(X1,Y1,X2,Y2,Bitmap,StartP,EndP,An1,An2,A,StartDis);
end;
//------------------------------------------------------------------------------
function TEZLine.Line(X1,Y1,X2,Y2:Integer;Bitmap:TBitmap):double;
begin
  Result := FStartDis;
  if assigned(Bitmap) and not Bitmap.Empty then begin
    Bitmap.Canvas.Lock;
    Result := Line(X1,Y1,X2,Y2,Bitmap,True,True,0,0,0,FStartDis);
    if not AntiAlias then begin
      Bitmap.Canvas.Pen.Width  := TruncC(LineWidth);
      Bitmap.Canvas.Pen.Color  := FColor;
      Bitmap.Canvas.Pen.Style  := FPenS;
      Bitmap.Canvas.Pen.Mode   := FPenM;
      Bitmap.Canvas.MoveTo(X1,Y1);
      Bitmap.Canvas.LineTo(X2,Y2);
    end;
    Bitmap.Canvas.Unlock;
  end;
end;
function TEZLine.Line2(X1,Y1,X2,Y2:single;Bitmap:TBitmap):double;
begin
  Result := FStartDis;
  if assigned(Bitmap) and not Bitmap.Empty then begin
    Bitmap.Canvas.Lock;
    Result := Line2(X1,Y1,X2,Y2,Bitmap,True,True,0,0,0,FStartDis);
    if not AntiAlias then begin
      Bitmap.Canvas.Pen.Width  := TruncC(LineWidth);
      Bitmap.Canvas.Pen.Color  := FColor;
      Bitmap.Canvas.Pen.Style  := FPenS;
      Bitmap.Canvas.Pen.Mode   := FPenM;
      Bitmap.Canvas.MoveTo(round(X1),round(Y1));
      Bitmap.Canvas.LineTo(round(X2),round(Y2));
    end;
    Bitmap.Canvas.Unlock;
  end;
end;
//------------------------------------------------------------------------------
procedure TEZLine.SetProp(Color:TColor;LineWidth,Transparancy,StartDistance:Double;AntiAlias,CutOff:Boolean;PenStyle:TPenStyle;PenMode:TPenMode);
begin
  FColor   := Color;
  FWidth   := LineWidth;
  FAA      := AntiAlias;
  FPenS    := PenStyle;
  FPenM    := PenMode;
  FTrans   := Transparancy;
  FCutOff  := CutOff;
  FStartDis:= StartDistance;
end;
//------------------------------------------------------------------------------
function TEZLine.Bezier(X1,Y1,X2,Y2,X3,Y3,X4,Y4:Integer;Bitmap:TBitmap):double;
  //-------
  function BezierPos(T:double):TPoint;
  begin
     Result.X := RoundC(Power((1-T),3)*X1 + 3*sqr((1-T))*T*X2 + 3*(1-T)*sqr(T)*X3 + Power(T,3)*X4);
     Result.Y := RoundC(Power((1-T),3)*Y1 + 3*sqr((1-T))*T*Y2 + 3*(1-T)*sqr(T)*Y3 + Power(T,3)*Y4);
  end;
  //--------
const
 Inv = 30;
var
  I,C,L,J : integer;
  P1: TPoint;
  D : double;
  Arr : Array of TPoint;
begin
   D := FStartDis;
   if assigned(Bitmap) and not Bitmap.Empty then
   if not AntiAlias then begin
     Bitmap.Canvas.Pen.Width  := TruncC(LineWidth);
     Bitmap.Canvas.Pen.Color  := FColor;
     Bitmap.Canvas.Pen.Style  := FPenS;
     Bitmap.Canvas.Pen.Mode   := FPenM;
     SetLength(Arr,4);
     Arr[0].X := X1;Arr[0].Y := Y1;Arr[1].X := X2;Arr[1].Y := Y2;
     Arr[2].X := X3;Arr[2].Y := Y3;Arr[3].X := X4;Arr[3].Y := Y4;
     Bitmap.Canvas.PolyBezier(Arr);
   end else begin
     D  := FStartDis;
     C  := 0;
     SetLength(Arr,1);
     Arr[C] := BezierPos(0);
     for I := 1 to Inv do begin
       P1 := BezierPos(I/Inv);
       if (Arr[C].X <> P1.X) or (Arr[C].Y <> P1.Y) then begin
         Inc(C);
         SetLength(Arr,C+1);
         Arr[C] := P1;
       end;
     end;

     Bitmap.Canvas.Lock;
     L := Length(Arr);
     if (L > 1) then
     for J := 0 to L-3 do begin
       if (J = 0) and (L > 2) then D := LineCut(0,0,Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,True,False,Bitmap,D)
       else if (J = 0) and (L = 2) then D := Line(Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Bitmap,True,True,0,0,0,D);
      if (J > 0) and (J < L-1) then D := LineCut(Arr[J-1].X,Arr[J-1].Y,Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,False,False,Bitmap,D);
      if (J = L-3) and (J <> 0) and (L > 2) then D := LineCut(Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,0,0,False,True,Bitmap,D);
      FStartDis := D;
     end;
     Bitmap.Canvas.Unlock;
   end;
   Result := D;
end;
//------------------------------------------------------------------------------
function TEZLine.PolyLine(Arr:array of TPoint;Bitmap:TBitmap):double;
var
 J,K : integer;
 D   : double;
begin
 Result := FStartDis;
 K := Length(Arr);
 D := FStartDis;
   if assigned(Bitmap) and not Bitmap.Empty then
   if not AntiAlias then begin
     Bitmap.Canvas.Pen.Width  := TruncC(LineWidth);
     Bitmap.Canvas.Pen.Color  := FColor;
     Bitmap.Canvas.Pen.Style  := FPenS;
     Bitmap.Canvas.Pen.Mode   := FPenM;
     Bitmap.Canvas.PolyLine(Arr);
   end else begin
  Bitmap.Canvas.Lock;
  for J := 0 to K-2 do begin
     if (J = 0) then D := LineCut(0,0,Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,True,False,Bitmap,D)
     else if (J = K-2) then D := LineCut(Arr[J-1].X,Arr[J-1].Y,Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,0,0,False,True,Bitmap,D)
     else D := LineCut(Arr[J-1].X,Arr[J-1].Y,Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,False,False,Bitmap,D);
     FStartDis := D;
  end;
  Result := D;
  Bitmap.Canvas.Unlock;
 end;
end;
//------------------------------------------------------------------------------
procedure TEZLine.SuperShape(X0,Y0:integer;Radius,Angle,M,N1,N2,N3,Phi,Increment:Double;Bitmap:TBitmap);
  function SuperShapes(M,N1,N2,N3,ph,An:Double):TPoint;
  const
   a = 1;
   b = 1;
  var
    r,t1,t2 : double;
  begin
    if (N1 < 0.01) or (N2 <= 0.01) or (N3 <= 0.01) then Exit;
    t1 := cos(m * ph / 4) / a;
    t1 := ABS(t1);
    t1 := power(t1,n2);
    t2 := sin(m * ph / 4) / b;
    t2 := ABS(t2);
    t2 := power(t2,n3);

    r  := power(t1+t2,1/n1);
    if (abs(r) = 0) then begin
      Result.x := 0;
      Result.y := 0;
    end else begin
      r := Radius / r;
      Result.x := RoundC(r * cos(ph+An))+X0;
      Result.y := RoundC(r * sin(ph+An))+Y0;
    end;
  end;
  //-------
var
  D,I   : double;
  C,J,L : integer;
  P1    : TPoint;
  Arr   : Array of TPoint;
begin
 if assigned(Bitmap) and not Bitmap.Empty then begin
   D  := 0;
   C  := 0;
   I  := Increment;
   SetLength(Arr,1);
   Arr[C] := SuperShapes(M,N1,N2,N3,0,DegToRad(Angle));
   repeat
     P1 := SuperShapes(M,N1,N2,N3,I,DegToRad(Angle));
     if (Arr[C].X <> P1.X) or (Arr[C].Y <> P1.Y) then begin
       Inc(C);
       SetLength(Arr,C+1);
       Arr[C] := P1;
     end;
     I   := I + Increment;
   until (I > Phi);

   Bitmap.Canvas.Lock;
   L := Length(Arr);
   if (L > 1) then
   for J := 0 to L-3 do begin
     if (J = 0) and (L > 2) then D := LineCut(0,0,Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,True,False,Bitmap,D)
     else if (J = 0) and (L = 2) then D := Line(Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Bitmap,True,True,0,0,0,D);
     if (J > 0) and (J < L-1) then D := LineCut(Arr[J-1].X,Arr[J-1].Y,Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,False,False,Bitmap,D);
     if (J = L-3) and (J <> 0) and (L > 2) then D := LineCut(Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,0,0,False,True,Bitmap,D);
   end;
   Bitmap.Canvas.Unlock;
 end;
end;
//------------------------------------------------------------------------------
procedure TEZLine.Ellips(X1,Y1,X2,Y2:Integer;Bitmap:TBitmap);
type
  TFPoint = record
    x,y: integer;
  end;

  TFPoints = array of TFPoint;

  function CalculateEllipse(xc,yc,rad,xrad,yrad: Double): TFPoints;
  var
    u,v,da,dx,dy: Double;
    angle: Double;
    segs: Integer;
    A: TFPoints;
    circumference: Double;
  begin
    segs:=0;
    Circumference:= 2*pi*rad;
    if circumference=0 then circumference:=1;
    da:=circumference/(Pi*15); //this controls the number of segments
    angle:= 0;
    while angle <= Circumference+da do begin
      dx:= Cos(2*angle*(pi/circumference))* xrad;
      dy:= Sin(2*angle*(pi/circumference))* yrad;
      u:=xc+dx;
      v:=yc+dy;
      segs:=segs+1;
      setLength(A,segs);
      A[segs-1].X := RoundC(u);
      A[segs-1].Y := RoundC(v);
      angle:=angle+da;
    end;
    Result:=A;
  end;
  //----------
  function CalculateEllipseE(xc,yc,xp,yp: Double):TFPoints;
  var
    dx,dy, rad: Double;
  begin
    dx  := Abs(xp-xc);
    dy  := Abs(yp-yc);
    rad := max(dx,dy);
    Result:= CalculateEllipse(xc,yc,rad,dx,dy);
  end;
  //----------
var
  L,J,dX,dY : Integer;
  D : double;
  Arr: TFPoints;
begin
   SetLength(Arr,0);
   if assigned(Bitmap) and not Bitmap.Empty then
   if not AntiAlias then begin
     Bitmap.Canvas.Pen.Width  := TruncC(LineWidth);
     Bitmap.Canvas.Pen.Color  := FColor;
     Bitmap.Canvas.Pen.Style  := FPenS;
     Bitmap.Canvas.Pen.Mode   := FPenM;
     Bitmap.Canvas.Brush.Style:= bsClear;
     dX := X2-X1;
     dY := Y2-Y1;
     Bitmap.Canvas.Ellipse(X1-dx,Y1-dy,X2+1,Y2+1);
   end else begin
     Arr := CalculateEllipseE(X1,Y1,X2,Y2);
     D := 0;
     Bitmap.Canvas.Lock;
     L := Length(Arr);
     for J := 0 to L-2 do begin
       if (J = 0) then D := LineCut(Arr[L-2].X,Arr[L-2].Y,Arr[0].X,Arr[0].Y,Arr[1].X,Arr[1].Y,Arr[2].X,Arr[2].Y,False,False,Bitmap,D)
       else if (J = L-2) then LineCut(Arr[J-1].X,Arr[J-1].Y,Arr[J].X,Arr[J].Y,Arr[0].X,Arr[0].Y,Arr[1].X,Arr[1].Y,False,False,Bitmap,D)
       else D := LineCut(Arr[J-1].X,Arr[J-1].Y,Arr[J].X,Arr[J].Y,Arr[J+1].X,Arr[J+1].Y,Arr[J+2].X,Arr[J+2].Y,False,False,Bitmap,D);
       FStartDis := D;
     end;
     Bitmap.Canvas.Unlock;
   end;
end;
//------------------------------------------------------------------------------
procedure TEZLine.Recangle(X1,Y1,X2,Y2:Integer;Bitmap:TBitmap);
var
  D : double;
begin
   if assigned(Bitmap) and not Bitmap.Empty then
   if not AntiAlias then begin
     Bitmap.Canvas.Pen.Width  := TruncC(LineWidth);
     Bitmap.Canvas.Pen.Color  := FColor;
     Bitmap.Canvas.Pen.Style  := FPenS;
     Bitmap.Canvas.Pen.Mode   := FPenM;
     Bitmap.Canvas.Brush.Style:= bsClear;
     Bitmap.Canvas.Rectangle(X1,Y1,X2+1,Y2+1);
   end else begin
     D := 0;
     Bitmap.Canvas.Lock;
     D := LineCut(X1,Y2,X1,Y1,X2,Y1,X2,Y2,False,False,Bitmap,D);
     D := LineCut(X1,Y1,X2,Y1,X2,Y2,X1,Y2,False,False,Bitmap,D);
     D := LineCut(X2,Y1,X2,Y2,X1,Y2,X1,Y1,False,False,Bitmap,D);
          LineCut(X2,Y2,X1,Y2,X1,Y1,X2,Y1,False,False,Bitmap,D);
     Bitmap.Canvas.Unlock;
   end;
end;
//------------------------------------------------------------------------------
end.
