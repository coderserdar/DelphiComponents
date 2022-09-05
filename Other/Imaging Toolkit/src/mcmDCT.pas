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
// $Log:  19041: mcmDCT.pas 
//
//    Rev 1.4    2014-02-02 21:09:54  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.3    03-01-2004 13:00:32  mcm

//
//   Rev 1.2    29-09-2003 18:44:32  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.1    06-07-2003 10:42:48  mcm    Version: IMG 1.3.4
// Modified to work in BCB.

//
//   Rev 1.0    29-01-2003 16:06:16  mcm
// Initial revision, performs DCT on 8x8 pixels or IDCT on 8x8 coefficients
// (JPEG).

unit mcmDCT;

//------------------------------------------------------------------------------
// Discrete Cosine Transformation
//
// 1 Dimensional
// -------------
// Forward DCT
//
//             N-1          (2·n+1)·i·pi
// T[i] = c(i)·Sum(V[n]·Cos(------------))
//             n=0               2·N
// where
//
//  c(0) = Sqrt(1/N), and
//  c(i) = Sqrt(2/N), when i <> 0.
//
// Inverse DCT.
//
//        N-1               (2·i+1)·n·pi
// V[i] = Sum(c(n)·T[n]·Cos(------------))
//        n=0                    2·N
//
// 2 Dimensional
// -------------
// Forward DCT
//
//                 N-1 N-1            (2·y+1)·i·pi      (2·x+1)·j·pi
// T[i,j] = c(i,j)·Sum(Sum(V[x,y]·Cos(------------)·Cos(------------)))
//                 x=0 y=0                 2·N               2·N
// where
//
//  c(i,j) = Sqrt(1/N), when i or j = 0
//  c(i,j) = Sqrt(2/N), when i and j <> 0.
//
// Inverse DCT.
//
//          N-1 N-1                    (2·y+1)·i·pi      (2·x+1)·j·pi
// V[x,y] = Sum(Sum( c(i,j)·T[i,j]·Cos(------------)·Cos(------------)))
//          i=0 j=0                         2·N               2·N
//
// Note: for JPEG N = 8.
//------------------------------------------------------------------------------

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes,
     {$ENDIF}
     mcmImageTypeDef, DefJPEG;

type

//------------------------------------------------------------------------------
// TmcmDCT
//------------------------------------------------------------------------------
  TmcmJPGDCTData = array[0..JPEGSampleWidth-1,0..JPEGSampleWidth-1] of byte;
  PmcmJPGDCTData = ^TmcmJPGDCTData;

  TmcmDCT = class(TPersistent)
  private
    FMMX        : boolean;
    FIc2By16    : longint;
    FIc4By16    : longint;
    FIc6By16    : longint;
    FFc2By16    : single;
    FFc4By16    : single;
    FFc6By16    : single;
    FRounding   : longint;
    FRoundFloat : single;

    // Quantification tables (Int and Real).
    FQTDbl    : array[0..JPEGSampleWidth-1,0..JPEGSampleWidth-1] of single;
    FQTInt    : array[0..JPEGSampleWidth-1,0..JPEGSampleWidth-1] of longint;

    // Transformed data.
    FData     : TmcmJPGDCTData;
  protected
    function    ByteRange(Value : longint) : byte;
    function    InverseScale(Value : longint; Factor : integer) : longint;
    function    GetData : PmcmJPGDCTData;
    procedure   SetData(Value : PmcmJPGDCTData);
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    procedure   InitialiseDCT;
    procedure   ForwardDCT(Coef : PJPEGCoefficient);
    procedure   InverseDCT(Coef : PJPEGCoefficient);
    procedure   InverseDCTInt(Coef : PJPEGCoefficient);
    procedure   SetIDCTTable(JFIFDQT : TJFIFDQT);
    procedure   SetDCTTable(JFIFDQT : TJFIFDQT);
    property    Data : PmcmJPGDCTData
      read      GetData
      write     SetData;
  published
  end;


//------------------------------------------------------------------------------
// TmcmJPEGQuant (Quantification Table/class).
//------------------------------------------------------------------------------

implementation

const
  DCTIntScale   : longint = 6;
//  QuantIntScale : longint = 12;

//------------------------------------------------------------------------------
// Quantification scaling table.
//------------------------------------------------------------------------------

// Table consists of the values
//   T(i,j) = x(i) * x(i) / 8
// where
//   x(n) = 1 for n = 0 and 4
// otherwise
//   x(n) = 1 / sqrt(2) / cos(n * pi /16)

const
  QuantDblScale : array[0..JPEGSampleWidth-1,0..JPEGSampleWidth-1] of single =
  ((0.125,                  0.09011997775086849627, 0.09567085809127244544, 0.1063037618459070632,
    0.125,                  0.159094822571604233,   0.2309698831278216846,  0.4530637231764438333),
   (0.09011997775086849627, 0.0649728831185362593,  0.0689748448207357645,  0.07664074121909414394,
    0.09011997775086849627, 0.1147009749634507608,  0.1665200058287998886,  0.3266407412190940884),
   (0.09567085809127244544, 0.0689748448207357645,  0.0732233047033631207,  0.08136137691302557096,
    0.09567085809127244544, 0.1217659055464329343,  0.1767766952966368932,  0.3467599613305368256),
   (0.1063037618459070632,  0.07664074121909414394, 0.08136137691302557096, 0.09040391826073060355,
    0.1063037618459070632,  0.135299025036549253,   0.1964237395967755595,  0.3852990250365491698),
   (0.125,                  0.09011997775086849627, 0.09567085809127244544, 0.1063037618459070632,
    0.125,                  0.159094822571604233,   0.2309698831278216846,  0.4530637231764438333),
   (0.159094822571604233,   0.1147009749634507608,  0.1217659055464329343,  0.135299025036549253,
    0.159094822571604233,   0.2024893005527218515,  0.2939689006048396558,  0.5766407412190940329),
   (0.2309698831278216846,  0.1665200058287998886,  0.1767766952966368932,  0.1964237395967755595,
    0.2309698831278216846,  0.2939689006048396558,  0.4267766952966368654,  0.8371526015321518744),
   (0.4530637231764438333,  0.3266407412190940884,  0.3467599613305368256,  0.3852990250365491698,
    0.4530637231764438333,  0.5766407412190940329,  0.8371526015321518744,  1.642133898068010689));



constructor TmcmDCT.Create;
var bMMX : boolean;
begin
  InitialiseDCT;
  FMMX := False;
  try
    asm
      mov   bMMX,$00
      mov   eax,1
      {$IFNDEF DCB3_5}
      cpuid
      {$ELSE}
      dw    CPUID
      {$ENDIF}
      test  edx,$800000
      jz    @NoMMX
      mov   bMMX,$01
      @NoMMX:
    end;
  except
  // do nothing.
  end;
  FMMX := bMMX;
end; // TmcmDCT.Ceate.


destructor TmcmDCT.Destroy;
begin
end; // TmcmDCT.Destroy.


function TmcmDCT.GetData : PmcmJPGDCTData;
begin
  Result := @FData;
end; // TmcmDCT.GetData.


procedure TmcmDCT.SetData(Value : PmcmJPGDCTData);
begin
  // Empty....
end; // TmcmDCT.SetData.


function TmcmDCT.ByteRange(Value : longint) : byte;
begin
  if (Value < 0)
  then Result := 0
  else if (Value > 255)
       then Result := 255
       else Result := Value;
end; // TmcmDCT.ByteRange.


function TmcmDCT.InverseScale(Value : longint; Factor : integer) : longint;
// Inverse scale a scaled integer value.
begin
  // Result := (Value  + (1  shl (Factor + 1))) shr Factor; // Slower but more accurate.
  if (Value < 0)
  then begin
       Result := -(Abs(Value) shr Factor);
  end
  else Result := Value shr Factor; // Fast but less accurate.
end; // TmcmDCT.InverseScale.


procedure TmcmDCT.InitialiseDCT;
begin
  // Inverse integer.
  FIc2By16 := Round((1 shl (DCTIntScale - 1)) / Cos(PI * 2.0 / 16.0));
  FIc4By16 := Round((1 shl DCTIntScale) * Cos(PI * 4.0 / 16.0));
  FIc6By16 := Round((1 shl (DCTIntScale - 1)) / Cos(PI * 6.0 / 16.0));

  // Inverse float (double).
  FFc2By16 := 0.5 / Cos(PI * 2.0 / 16.0);
  FFc4By16 := Cos(PI * 4.0 / 16.0);
  FFc6By16 := 0.5 / Cos(PI * 6.0 / 16.0);

  FRounding   := (JpegMaxSampleValue + 2) shl (JPEGQuantizationIntegerScale - 1);
  FRoundFloat := JpegMidpointSampleValue + 0.5;
end; // TmcmDCT.InitialiseDCT.


procedure TmcmDCT.SetDCTTable(JFIFDQT : TJFIFDQT);
var i, j, k    : integer;
begin
  k := 0;
  for i := 0 to (JPEGSampleWidth - 1)
  do for j := 0 to (JPEGSampleWidth - 1)
     do begin
        FQTDbl[i,j] := QuantDblScale[i,j] / JFIFDQT.Values[JPEGZigZagOutputOrder[k]];
        inc(k);
     end;
end; // TmcmDCT.SetDCTTable.


procedure TmcmDCT.SetIDCTTable(JFIFDQT : TJFIFDQT);
var i, j, k    : integer;
    ScaleValue : longint;
begin
//  FID := JFIFDQT.Identifier;
  ScaleValue := 1 shl JPEGQuantizationIntegerScale;
  k := 0;
  for i := 0 to (JPEGSampleWidth - 1)
  do for j := 0 to (JPEGSampleWidth - 1)
     do begin
        FQTDbl[i,j] := JFIFDQT.Values[JPEGZigZagOutputOrder[k]] * QuantDblScale[i,j];
        FQTInt[i,j] := Round(ScaleValue * FQTDbl[i,j]);
        inc(k);
     end;
end; // TmcmDCT.SetIDCTTable.


procedure TmcmDCT.ForwardDCT(Coef : PJPEGCoefficient);
var i        : integer;
    Temp     : array[0..JPEGSampleWidth-1,0..JPEGSampleWidth-1] of single;
    //ZeroVal  : shortint;
    a0, a1, a2, a3, a4, a5, a6, a7 : single;
    b0, b1, b2, b3{, b4, b5, b6, b7 }: single;
    {c0, c1,} c2, {c3,} c4, c5, c6, c7 : single;
    {d0, d1, d2, d3,} d4{, d5, d6, d7} : single;
    e0, e1, e2, {e3,} e4, e5{, e6, e7} : single;
    {f0, f1, f2, f3,} f4,{ f5,} f6{, f7} : single;
    {g0, g1, g2, g3,} g4, g5, g6, g7 : single;
    {h0, h1,} h2, h3, h4, h5, h6, h7 : single;
    i0, i1, i2, i3, i4, i5, i6, i7 : single;
begin
  for i := 0 to (JPEGSampleWidth - 1)
  do begin
     // Shift this first matrix multiplication includes the range shift from
     // 0...255 to -128...127. Notice that the shift term is canceled out
     // in the last four elements.
    a0 := Data[0,i] + Data[7,i] - JPEGMidPointSampleValueX2;
    a1 := Data[1,i] + Data[6,i] - JPEGMidPointSampleValueX2;
    a2 := Data[2,i] + Data[5,i] - JPEGMidPointSampleValueX2;
    a3 := Data[3,i] + Data[4,i] - JPEGMidPointSampleValueX2;
    a4 := Data[3,i] - Data[4,i];
    a5 := Data[2,i] - Data[5,i];
    a6 := Data[1,i] - Data[6,i];
    a7 := Data[0,i] - Data[7,i];

    b0 := a0 + a3;
    b1 := a1 + a2;
    b2 := a1 - a2;
    b3 := a0 - a3;
    //b4 := a4;
    //b5 := a5;
    //b6 := a6;
    //b7 := a7;

    //c0 := b0;
    //c1 := b1;
    c2 := b2 + b3;
    //c3 := b3;
    c4 := a4 + a5; // b4 + b5;
    c5 := a5 + a6; // b5 + b6;
    c6 := a6 + a7; // b6 + b7;
    //c7 := b7;

    //d0 := c0;
    //d1 := c1;
    //d2 := c2;
    //d3 := c3;
    d4 := c4 + c6;
    //d5 := c5;
    //d6 := c6;
    //d7 := c7;

    e0 := b0 + b1; // d0 + d1;
    e1 := b0 - b1; // d0 - d1;
    e2 := FFc4By16 * c2; // FFc4By16 * d2;
    //e3 := d3;
    e4 := FFc4By16 * d4;
    e5 := FFc4By16 * c5; // FFc4By16 * d5;
    //e6 := d6;
    //e7 := d7;

    //f0 := e0;
    //f1 := e1;
    //f2 := e2;
    //f3 := e3;
    f4 := e4 + c6; // e4 + e6;
    //f5 := e5;
    f6 := e4 - c6; // e4 - e6;
    //f7 := e7;

    //g0 := f0;
    //g1 := f1;
    //g2 := f2;
    //g3 := f3;
    g4 := FFc2By16 * f4;
    g5 := a7 - e5; // f7 - f5;
    g6 := FFc6By16 * f6;
    g7 := e5 + a7; // f5 + f7;

    //h0 := g0;
    //h1 := g1;
    //h2 := e2 + b3; // g2 + g3;
    //h3 := b3 - e2; // g3 - g2;
    //h4 := g4 + g7;
    //h5 := g5 + g6;
    //h6 := g5 - g6;
    //h7 := g7 - g4;

    Temp[0,i] := e0; // h0;
    Temp[1,i] := g4 + g7; // h4;
    Temp[2,i] := e2 + b3; // h2;
    Temp[3,i] := g5 - g6; // h6;
    Temp[4,i] := e1; // h1;
    Temp[5,i] := g5 + g6; // h5;
    Temp[6,i] := b3 - e2; // h3;
    Temp[7,i] := g7 - g4; // h7;
  end;

  for i := 0 to (JPEGSampleWidth - 1)
  do begin
     a0 := Temp[i,0] + Temp[i,7];
     a1 := Temp[i,1] + Temp[i,6];
     a2 := Temp[i,2] + Temp[i,5];
     a3 := Temp[i,3] + Temp[i,4];
     a4 := Temp[i,3] - Temp[i,4];
     a5 := Temp[i,2] - Temp[i,5];
     a6 := Temp[i,1] - Temp[i,6];
     a7 := Temp[i,0] - Temp[i,7];

     b0 := a0 + a3;
     b1 := a1 + a2;
     b2 := a1 - a2;
     b3 := a0 - a3;
     //b4 := a4;
     //b5 := a5;
     //b6 := a6;
     //b7 := a7;

     //c0 := b0;
     //c1 := b1;
     c2 := b2 + b3;
     //c3 := b3;
     c4 := a4 + a5; // b4 + b5;
     c5 := a5 + a6; // b5 + b6;
     c6 := a6 + a7; // b6 + b7;
     //c7 := b7;

     //d0 := c0;
     //d1 := c1;
     //d2 := c2;
     //d3 := c3;
     d4 := c4 + c6;
     //d5 := c5;
     //d6 := c6;
     //d7 := c7;

     e0 := b0 + b1; // d0 + d1;
     e1 := b0 - b1; // d0 - d1;
     e2 := FFc4By16 * c2; // FFc4By16 * d2;
     //e3 := d3;
     e4 := FFc4By16 * d4;
     e5 := FFc4By16 * c5; // FFc4By16 * d5;
     //e6 := d6;
     //e7 := d7;

     //f0 := e0;
     //f1 := e1;
     //f2 := e2;
     //f3 := e3;
     f4 := e4 + c6; // e4 + e6;
     //f5 := e5;
     f6 := e4 - c6; // e4 - e6;
     //f7 := e7;

     //g0 := f0;
     //g1 := f1;
     //g2 := f2;
     //g3 := f3;
     g4 := FFc2By16 * f4;
     g5 := a7 - e5; // f7 - f5;
     g6 := FFc6By16 * f6;
     g7 := e5 + a7; // f5 + f7;

     //h0 := g0;
     //h1 := g1;
     h2 := e2 + b3; // g2 + g3;
     h3 := b3 - e2; // g3 - g2;
     h4 := g4 + g7;
     h5 := g5 + g6;
     h6 := g5 - g6;
     h7 := g7 - g4;

     i0 := e0 * FQTDbl[i,0]; // h0 * FQTDbl[i,0];
     i1 := h4 * FQTDbl[i,1];
     i2 := h2 * FQTDbl[i,2];
     i3 := h6 * FQTDbl[i,3];
     i4 := e1 * FQTDbl[i,4]; // h1 * FQTDbl[i,4];
     i5 := h5 * FQTDbl[i,5];
     i6 := h3 * FQTDbl[i,6];
     i7 := h7 * FQTDbl[i,7];
{
     if (i0 >= 0.0)
     then Coef^[i,0] := Round(i0 + 0.5)
     else Coef^[i,0] := Round(i0 - 0.5);
     if (i1 >= 0.0)
     then Coef^[i,1] := Round(i1 + 0.5)
     else Coef^[i,1] := Round(i1 - 0.5);
     if (i2 >= 0.0)
     then Coef^[i,2] := Round(i2 + 0.5)
     else Coef^[i,2] := Round(i2 - 0.5);
     if (i3 >= 0.0)
     then Coef^[i,3] := Round(i3 + 0.5)
     else Coef^[i,3] := Round(i3 - 0.5);
     if (i4 >= 0.0)
     then Coef^[i,4] := Round(i4 + 0.5)
     else Coef^[i,4] := Round(i4 - 0.5);
     if (i5 >= 0.0)
     then Coef^[i,5] := Round(i5 + 0.5)
     else Coef^[i,5] := Round(i5 - 0.5);
     if (i6 >= 0.0)
     then Coef^[i,6] := Round(i6 + 0.5)
     else Coef^[i,6] := Round(i6 - 0.5);
     if (i7 >= 0.0)
     then Coef^[i,7] := Round(i7 + 0.5)
     else Coef^[i,7] := Round(i7 - 0.5);
}
     // Produces too big a difference!
     Coef^[i,0] := Round(i0);
     Coef^[i,1] := Round(i1);
     Coef^[i,2] := Round(i2);
     Coef^[i,3] := Round(i3);
     Coef^[i,4] := Round(i4);
     Coef^[i,5] := Round(i5);
     Coef^[i,6] := Round(i6);
     Coef^[i,7] := Round(i7);
  end;
end; // TmcmDCT.ForwardDCT.


procedure TmcmDCT.InverseDCTInt(Coef : PJPEGCoefficient);
var i        : integer;
    Temp     : array[0..JPEGSampleWidth-1,0..JPEGSampleWidth-1] of longint;
    ZeroVal  : shortint;
    a0, a1, a2, a3, a4, a5, a6, a7 : longint;
    b0, b1, b2, b3, b4, b5, b6, b7 : longint;
    c0, c1, c2, c3, c4, c5, c6, c7 : longint;
    d0, d1, d2, d3, d4, d5, d6, d7 : longint;
    e0, e1, e2, e3, e4, e5, e6, e7 : longint;
    f0, f1, f2, f3, f4, f5, f6, f7 : longint;
    g0, g1, g2, g3, g4, g5, g6, g7 : longint;
    h0, h1, h2, h3, h4, h5, h6, h7 : longint;
begin
  for i := 0 to (JPEGSampleWidth - 1)
  do begin
     // This optimization does not seem to be worth the trouble in the
     // second loop.

     ZeroVal := Coef^[i,1] or Coef^[i,2] or Coef^[i,3] or Coef^[i,4] or
                Coef^[i,5] or Coef^[i,6] or Coef^[i,7];
     if (ZeroVal = 0)
     then begin
          Temp[i,0] := Coef^[i,0] * FQTInt[i,0];
          Temp[i,1] := Temp[i,0];
          Temp[i,2] := Temp[i,0];
          Temp[i,3] := Temp[i,0];
          Temp[i,4] := Temp[i,0];
          Temp[i,5] := Temp[i,0];
          Temp[i,6] := Temp[i,0];
          Temp[i,7] := Temp[i,0];
     end
     else begin
          a0 := Coef^[i,0] * FQTInt[i,0];
          a1 := Coef^[i,4] * FQTInt[i,4];
          a2 := Coef^[i,2] * FQTInt[i,2];
          a3 := Coef^[i,6] * FQTInt[i,6];
          a4 := Coef^[i,1] * FQTInt[i,1];
          a5 := Coef^[i,5] * FQTInt[i,5];
          a6 := Coef^[i,3] * FQTInt[i,3];
          a7 := Coef^[i,7] * FQTInt[i,7];

          //b0 := a0;
          //b1 := a1;
          b2 := a2 - a3;
          b3 := a2 + a3;
          b4 := a4 - a7;
          b5 := a5 + a6;
          b6 := a5 - a6;
          b7 := a4 + a7;

          //c0 := b0;
          //c1 := b1;
          //c2 := b2;
          //c3 := b3;
          c4 := InverseScale(FIc2By16 * b4, DCTIntScale);
          c5 := b7 - b5;
          c6 := InverseScale(FIc6By16 * b6, DCTIntScale);
          c7 := b5 + b7;

          //d0 := c0;
          //d1 := c1;
          //d2 := c2;
          //d3 := c3;
          d4 := c4 + c6;
          //d5 := c5;
          d6 := c4 - c6;
          //d7 := c7;

          e0 := a0 + a1; // d0 + d1;
          e1 := a0 - a1; // d0 - d1;
          e2 := InverseScale(b2 * FIc4By16, DCTIntScale); // InverseScale(d2 * FIc4By16, DCTIntScale);
          //e3 := d3;
          e4 := InverseScale(d4 * FIc4By16, DCTIntScale);
          e5 := InverseScale(c5 * FIc4By16, DCTIntScale); // InverseScale(d5 * FIc4By16, DCTIntScale);
          //e6 := d6;
          //e7 := d7;

          //f0 := e0;
          //f1 := e1;
          //f2 := e2;
          //f3 := e3;
          //f4 := e4;
          //f5 := e5;
          f6 := e4 + d6; // e4 + e6;
          //f7 := e7;

          //g0 := f0;
          //g1 := f1;
          //g2 := f2;
          g3 := e2 + b3; // f2 + f3;
          //g4 := f4;
          g5 := e4 + e5; // f4 + f5;
          g6 := e5 + f6; // f5 + f6;
          g7 := f6 + c7; // f6 + f7;

          h0 := e0 + g3; // g0 + g3;
          h1 := e1 + e2; // g1 + g2;
          h2 := e1 - e2; // g1 - g2;
          h3 := e0 - g3; // g0 - g3;
          //h4 := g4;
          //h5 := g5;
          //h6 := g6;
          //h7 := g7;

          Temp[i,0] := h0 + g7; // h0 + h7;
          Temp[i,1] := h1 + g6; // h1 + h6;
          Temp[i,2] := h2 + g5; // h2 + h5;
          Temp[i,3] := h3 + e4; // h3 + h4;
          Temp[i,4] := h3 - e4; // h3 - h4;
          Temp[i,5] := h2 - g5; // h2 - h5;
          Temp[i,6] := h1 - g6; // h1 - h6;
          Temp[i,7] := h0 - g7; // h0 - h7;
     end;
  end;

  for i := 0 to (JPEGSampleWidth - 1)
  do begin
     a0 := Temp[0,i];
     a1 := Temp[4,i];
     a2 := Temp[2,i];
     a3 := Temp[6,i];
     a4 := Temp[1,i];
     a5 := Temp[5,i];
     a6 := Temp[3,i];
     a7 := Temp[7,i];

     //b0 := a0;
     //b1 := a1;
     b2 := a2 - a3;
     b3 := a2 + a3;
     b4 := a4 - a7;
     b5 := a5 + a6;
     b6 := a5 - a6;
     b7 := a4 + a7;

     //c0 := b0;
     //c1 := b1;
     //c2 := b2;
     //c3 := b3;
     c4 := InverseScale(FIc2By16 * b4, DCTIntScale);
     c5 := b7 - b5;
     c6 := InverseScale(FIc6By16 * b6, DCTIntScale);
     c7 := b5 + b7;

     //d0 := c0;
     //d1 := c1;
     //d2 := c2;
     //d3 := c3;
     d4 := c4 + c6;
     //d5 := c5;
     d6 := c4 - c6;
     //d7 := c7;

     e0 := a0 + a1; // d0 + d1;
     e1 := a0 - a1; // d0 - d1;
     e2 := InverseScale(b2 * FIc4By16, DCTIntScale); // InverseScale(d2 * FIc4By16, DCTIntScale);
     //e3 := d3;
     e4 := InverseScale(d4 * FIc4By16, DCTIntScale);
     e5 := InverseScale(c5 * FIc4By16, DCTIntScale); // InverseScale(d5 * FIc4By16, DCTIntScale);
     //e6 := d6;
     //e7 := d7;

     //f0 := e0;
     //f1 := e1;
     //f2 := e2;
     //f3 := e3;
     //f4 := e4;
     //f5 := e5;
     f6 := e4 + d6; // e4 + e6;
     //f7 := e7;

     g0 := e0 + FRounding; // f0 + FRounding;
     g1 := e1 + FRounding; // f1 + FRounding;
     //g2 := f2;
     g3 := e2 + b3; // f2 + f3;
     //g4 := f4;
     g5 := e4 + e5; // f4 + f5;
     g6 := e5 + f6; // f5 + f6;
     g7 := f6 + c7; // f6 + f7;

     h0 := g0 + g3;
     h1 := g1 + e2; // g1 + g2;
     h2 := g1 - e2; // g1 - g2;
     h3 := g0 - g3;
     //h4 := g4;
     //h5 := g5;
     //h6 := g6;
     //h7 := g7;

     FData[0,i] := ByteRange(InverseScale(h0 + g7, JPEGQuantizationIntegerScale)); // ByteRange(InverseScale(h0 + h7, JPEGQuantizationIntegerScale));
     FData[1,i] := ByteRange(InverseScale(h1 + g6, JPEGQuantizationIntegerScale)); // ByteRange(InverseScale(h1 + h6, JPEGQuantizationIntegerScale));
     FData[2,i] := ByteRange(InverseScale(h2 + g5, JPEGQuantizationIntegerScale)); // ByteRange(InverseScale(h2 + h5, JPEGQuantizationIntegerScale));
     FData[3,i] := ByteRange(InverseScale(h3 + e4, JPEGQuantizationIntegerScale)); // ByteRange(InverseScale(h3 + h4, JPEGQuantizationIntegerScale));
     FData[4,i] := ByteRange(InverseScale(h3 - e4, JPEGQuantizationIntegerScale)); // ByteRange(InverseScale(h3 - h4, JPEGQuantizationIntegerScale));
     FData[5,i] := ByteRange(InverseScale(h2 - g5, JPEGQuantizationIntegerScale)); // ByteRange(InverseScale(h2 - h5, JPEGQuantizationIntegerScale));
     FData[6,i] := ByteRange(InverseScale(h1 - g6, JPEGQuantizationIntegerScale)); // ByteRange(InverseScale(h1 - h6, JPEGQuantizationIntegerScale));
     FData[7,i] := ByteRange(InverseScale(h0 - g7, JPEGQuantizationIntegerScale)); // ByteRange(InverseScale(h0 - h7, JPEGQuantizationIntegerScale));
  end;
end; // TmcmDCT.InverseDCTInt.


procedure TmcmDCT.InverseDCT(Coef : PJPEGCoefficient);
var i        : integer;
    Temp     : array[0..JPEGSampleWidth-1,0..JPEGSampleWidth-1] of single;
    ZeroVal  : shortint;
    a0, a1, a2, a3, a4, a5, a6, a7 : single;
    b0, b1, b2, b3, b4, b5, b6, b7 : single;
    c0, c1, c2, c3, c4, c5, c6, c7 : single;
    d0, d1, d2, d3, d4, d5, d6, d7 : single;
    e0, e1, e2, e3, e4, e5, e6, e7 : single;
    f0, f1, f2, f3, f4, f5, f6, f7 : single;
    g0, g1, g2, g3, g4, g5, g6, g7 : single;
    h0, h1, h2, h3, h4, h5, h6, h7 : single;
begin
  for i := 0 to (JPEGSampleWidth - 1)
  do begin
     // This optimization does not seem to be worth the trouble in the
     // second loop.
     a0 := Coef^[i,0] * FQTDbl[i,0];
     a1 := Coef^[i,4] * FQTDbl[i,4];
     a2 := Coef^[i,2] * FQTDbl[i,2];
     a3 := Coef^[i,6] * FQTDbl[i,6];
     a4 := Coef^[i,1] * FQTDbl[i,1];
     a5 := Coef^[i,5] * FQTDbl[i,5];
     a6 := Coef^[i,3] * FQTDbl[i,3];
     a7 := Coef^[i,7] * FQTDbl[i,7];

     //b0 := a0;
     //b1 := a1;
     b2 := a2 - a3;
     b3 := a2 + a3;
     b4 := a4 - a7;
     b5 := a5 + a6;
     b6 := a5 - a6;
     b7 := a4 + a7;

     //c0 := b0;
     //c1 := b1;
     //c2 := b2;
     //c3 := b3;
     c4 := FFc2By16 * b4;
     c5 := b7 - b5;
     c6 := FFc6By16 * b6;
     c7 := b5 + b7;

     //d0 := c0;
     //d1 := c1;
     //d2 := c2;
     //d3 := c3;
     d4 := c4 + c6;
     //d5 := c5;
     d6 := c4 - c6;
     //d7 := c7;

     e0 := a0 + a1; // d0 + d1;
     e1 := a0 - a1; // d0 - d1;
     e2 := b2 * FFc4By16; // d2 * FFc4By16;
     //e3 := d3;
     e4 := d4 * FFc4By16;
     e5 := c5 * FFc4By16; // d5 * FFc4By16;
     //e6 := d6;
     //e7 := d7;

     //f0 := e0;
     //f1 := e1;
     //f2 := e2;
     //f3 := e3;
     //f4 := e4;
     //f5 := e5;
     f6 := e4 + d6; // e4 + e6;
     //f7 := e7;

     //g0 := f0;
     //g1 := f1;
     //g2 := f2;
     g3 := e2 + b3; // f2 + f3;
     //g4 := f4;
     g5 := e4 + e5; // f4 + f5;
     g6 := e5 + f6; // f5 + f6;
     g7 := f6 + c7; // f6 + f7;

     h0 := e0 + g3; // g0 + g3;
     h1 := e1 + e2; // g1 + g2;
     h2 := e1 - e2; // g1 - g2;
     h3 := e0 - g3; // g0 - g3;
     //h4 := g4;
     //h5 := g5;
     //h6 := g6;
     //h7 := g7;

     Temp[i,0] := h0 + g7; // h0 + h7;
     Temp[i,1] := h1 + g6; // h1 + h6;
     Temp[i,2] := h2 + g5; // h2 + h5;
     Temp[i,3] := h3 + e4; // h3 + h4;
     Temp[i,4] := h3 - e4; // h3 - h4;
     Temp[i,5] := h2 - g5; // h2 - h5;
     Temp[i,6] := h1 - g6; // h1 - h6;
     Temp[i,7] := h0 - g7; // h0 - h7;
  end;

  for i := 0 to (JPEGSampleWidth - 1)
  do begin
     a0 := Temp[0,i];
     a1 := Temp[4,i];
     a2 := Temp[2,i];
     a3 := Temp[6,i];
     a4 := Temp[1,i];
     a5 := Temp[5,i];
     a6 := Temp[3,i];
     a7 := Temp[7,i];

     //b0 := a0;
     //b1 := a1;
     b2 := a2 - a3;
     b3 := a2 + a3;
     b4 := a4 - a7;
     b5 := a5 + a6;
     b6 := a5 - a6;
     b7 := a4 + a7;

     //c0 := b0;
     //c1 := b1;
     //c2 := b2;
     //c3 := b3;
     c4 := FFc2By16 * b4;
     c5 := b7 - b5;
     c6 := FFc6By16 * b6;
     c7 := b5 + b7;

     //d0 := c0;
     //d1 := c1;
     //d2 := c2;
     //d3 := c3;
     d4 := c4 + c6;
     //d5 := c5;
     d6 := c4 - c6;
     //d7 := c7;

     e0 := a0 + a1; // d0 + d1;
     e1 := a0 - a1; // d0 - d1;
     e2 := b2 * FFc4By16; // d2 * FFc4By16;
     //e3 := d3;
     e4 := d4 * FFc4By16;
     e5 := c5 * FFc4By16; // d5 * FFc4By16;
     //e6 := d6;
     //e7 := d7;

     //f0 := e0;
     //f1 := e1;
     //f2 := e2;
     //f3 := e3;
     //f4 := e4;
     //f5 := e5;
     f6 := e4 + d6; // e4 + e6;
     //f7 := e7;

     //g0 := f0;
     //g1 := f1;
     //g2 := f2;
     g3 := e2 + b3; // f2 + f3;
     //g4 := f4;
     g5 := e4 + e5; // f4 + f5;
     g6 := e5 + f6; // f5 + f6;
     g7 := f6 + c7; // f6 + f7;

     h0 := e0 + g3; // g0 + g3;
     h1 := e1 + e2; // g1 + g2;
     h2 := e1 - e2; // g1 - g2;
     h3 := e0 - g3; // g0 - g3;
     //h4 := g4;
     //h5 := g5;
     //h6 := g6;
     //h7 := g7;

     FData[0,i] := ByteRange(Round((h0 + g7) + FRoundFloat)); // ByteRange(Round((h0 + h7) + FRoundFloat));
     FData[1,i] := ByteRange(Round((h1 + g6) + FRoundFloat)); // ByteRange(Round((h1 + h6) + FRoundFloat));
     FData[2,i] := ByteRange(Round((h2 + g5) + FRoundFloat)); // ByteRange(Round((h2 + h5) + FRoundFloat));
     FData[3,i] := ByteRange(Round((h3 + e4) + FRoundFloat)); // ByteRange(Round((h3 + h4) + FRoundFloat));
     FData[4,i] := ByteRange(Round((h3 - e4) + FRoundFloat)); // ByteRange(Round((h3 - h4) + FRoundFloat));
     FData[5,i] := ByteRange(Round((h2 - g5) + FRoundFloat)); // ByteRange(Round((h2 - h5) + FRoundFloat));
     FData[6,i] := ByteRange(Round((h1 - g6) + FRoundFloat)); // ByteRange(Round((h1 - h6) + FRoundFloat));
     FData[7,i] := ByteRange(Round((h0 - g7) + FRoundFloat)); // ByteRange(Round((h0 - h7) + FRoundFloat));
  end;
end; // TmcmDCT.InverseDCT.


end.
