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
// $Log:  17555: mcmImageTransform.pas 
//
//    Rev 1.22    2014-02-02 21:10:04  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.21    05-11-2006 18:50:54  mcm    Version: IMG 3.1
// In Destroy, moved call to interited to the end of the destroy method.
//
//   Rev 1.20    20-06-2005 10:11:54  mcm
// Corrected rotating 90 and 270 in non-clockwise direction.

//
//   Rev 1.19    20-03-2005 18:25:32  mcm    Version: IMG 2.9
// Added missing IF_PAL8 in Rotate.

//
//   Rev 1.18    13-02-2005 19:59:42  mcm    Version: IMG 2.8
// Optimization of Deskew.

//
//   Rev 1.17    03-02-2005 21:18:26  mcm
// Completed Deskew method.

//
//   Rev 1.16    29-01-2005 20:40:48  mcm
// Deskew.

//
//   Rev 1.15    20-01-2005 22:25:16  mcm
// Added support in Affine for BW images.
// Implemented Deskew method supporting BW, Grey, RGB and RGBA images.

//
//   Rev 1.14    28-10-2004 19:23:52  mcm    Version: IMG 2.6
// Improved checing and auto-creation of ResultImage.
// Improved error checking and reporting.

//
//   Rev 1.13    02-08-2004 11:39:12  mcm    Version: IMG 2.5
// Modified Flip, Mirror and Rotate90 to re-use the instance assigned to
// ResultImage when the format (Height, Width and ImageFormat) had to be changed.
// Corrected the missing return value in Rotate90 on success.

//
//   Rev 1.12    24-11-2003 20:14:32  mcm

//
//   Rev 1.11    17-11-2003 10:06:46  mcm    Version: IMG 2.0
// Fixed an exception occuring when transforming one image followed by a taller
// image (DoneAffine). 

//
//   Rev 1.10    03-10-2003 14:52:58  mcm
// Added support for rotating 1-bit images 90, 180 and 270 degrees.

//
//   Rev 1.9    29-09-2003 18:44:36  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.8    25-09-2003 23:40:22  mcm    Version: IMG 1.5
// Included Affine transformation and Nearest, Bilinear, Biquadratic, bicubic
// and Hermite interpolations.
// Coordinate calculation is enhanced using MMX.

//
//   Rev 1.7    06-07-2003 10:50:34  mcm    Version: IMG 1.3.4
// Modified to work in BCB.

//
//   Rev 1.6    12-05-2003 15:18:18  mcm    Version: IMG 1.3.4
// Corrected x and y offset in rotate.

//
//   Rev 1.5    17-02-2003 10:30:38  mcm    Version: IMG 1.3
// Implemented creating an instance for ResultImage if non is assigned for the
// methods: Flip, Mirror and Rotate.

//
//   Rev 1.4    05-02-03 17:42:12  mcm
// Delphi 6 does not support op-code xlat - replaced by constant.

//
//   Rev 1.3    05-02-03 16:26:22  mcm
// Added Mirror of 1 bit images.

//
//   Rev 1.2    27-01-2003 13:41:06  mcm

//
//   Rev 1.1    02-08-2002 12:37:26  mcm
// Fixed Flip.

//
//   Rev 1.0    27-05-2002 16:22:06  mcm

unit mcmImageTransform;

interface

{$Include 'mcmDefines.pas'}

{$DEFINE MCMSTRETCH}

uses {$IFNDEF GE_DXE2}
      Windows, Classes,
     {$ELSE}
      WinApi.Windows, System.Classes, 
     {$ENDIF}
     mcmImageKernel,
     mcmImageTypeDef,
     mcmImage;

// All transformations uses inverse mapping, i.e. the coordinates in the
// ResultImage are mapped back onto the SourceImage.

type
  TmcmGeoMatrix = array[0..5,0..5] of single;
  PmcmGeoMatrix = ^TmcmGeoMatrix;

  TNeighbour = record
               x1, y1 : integer;
               x2, y2 : integer;
               Up     : boolean;
               end;
  TVectorNeighbour = array[0..0] of TNeighbour;
  PVectorNeighbour = ^TVectorNeighbour;

  TInterpolate = Procedure(const y : integer) of object;

  TmcmImageTransform = class(TmcmImageKernel)
  private
    // Private declarations
    FDataCount   : integer;       // Pixels per line.
    Fdp          : integer;       // Planes per pixel
    Fxy          : PVectorPt;     // Correspondance between result(in) and
                                  // source(out) coordinates.
    Fdxdy        : PVectorPt;     // dx and dy between actual and truncated
                                  // coordinate.
    FScanLines   : PVectorL;      //
    Fp2, Fp3     : array[0..255] of smallint;
    FAffineInt   : array[0..4,0..3] of smallint;
    Fm, Fim      : TmcmGeoMatrix; // Affine forward and inverse matrix.

    FInterpolate : TmcmInterpolate;
    FDegree      : double;        // Rotation
    FRadian      : double;
    FClockwise   : boolean;
    FxDist       : double;        // Translation
    FyDist       : double;
    FxScale      : double;        // Scale/Stretch
    FyScale      : double;
    FxShear      : double;        // Shearing
    FyShear      : double;
    FKeepResSize : boolean;       // If true, then do not change ResultImage's
                                  // dimension (Width & Height).
    FIMethod     : TInterpolate;  // Interpolation method
    FBKColor     : TColorRef;     // Background fill color.

    // Deskew parameters.
    FBlx, FBby   : integer;
    FBrx, FBty   : integer;
    FUseBaird    : boolean;
    FReturnHough : boolean;
    FDeskewMaxInt: word;          // Max intensity considered text when deskewing.
    FDeskewRange : integer;
    FDeskewStep  : double;
    FNeighbourSize : integer;
    FNeighbour   : PVectorNeighbour;
  protected
    // Protected declarations
    procedure   InitAffine;
    procedure   DoneAffine;
    function    CalcAffineLine(y : integer) : TmcmErrorCode;
    procedure   Bicubic(const y : integer);
    procedure   Bicubic2(const y : integer);
    procedure   Bilinear(const y : integer);
    procedure   Biquadratic(const y : integer);
    procedure   Hermite(const y : integer);
    procedure   NearestNeighbour(const y : integer);
    function    Rotate90(Is90 : boolean) : TmcmImage;
    procedure   SetBKColor(Value : TColorRef);

    // Deskew methods
    function    CreateBairdImage : TmcmImage;
    procedure   Baird(BairdImage, DotImage : TmcmImage);
    procedure   BairdMark(AImage : TmcmImage; x, y : integer);

    procedure   SetDeskewRange(Value : integer);
    procedure   SetDeskewStep(Value : double);
    procedure   SetDeskewMaxInt(Value : word);
  public
    // Public declarations
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    function    Affine : TmcmImage;
    function    Flip : TmcmImage;
    function    Mirror : TmcmImage;
    function    Resize(Method : TmcmInterpolate) : TmcmImage;
    function    Rotate(Clockwise : boolean; Degree : double) : TmcmImage;
    function    Deskew : TmcmImage;


    property    BKColor : TColorRef
      read      FBKColor
      write     SetBKColor default 0;
    property    Clockwise : boolean
      read      FClockwise
      write     FClockwise default True;
    property    Degree : double
      read      FDegree
      write     FDegree;
    property    Interpolate : TmcmInterpolate
      read      FInterpolate
      write     FInterpolate default ST_NEAREST;
    property    KeepResultSize : boolean
      read      FKeepResSize
      write     FKeepResSize default False;
    property    xDist : double
      read      FxDist
      write     FxDist;
    property    xScale : double
      read      FxScale
      write     FxScale;
    property    xShear : double
      read      FxShear
      write     FxShear;
    property    yDist : double
      read      FyDist
      write     FyDist;
    property    yScale : double
      read      FyScale
      write     FyScale;
    property    yShear : double
      read      FyShear
      write     FyShear;

    property    DeskewMaxInt : word
      read      FDeskewMaxInt
      write     SetDeskewMaxInt default 1;
    property    DeskewPreprocess : boolean
      read      FUseBaird
      write     FUseBaird default True;
    property    DeskewRange : integer
      read      FDeskewRange
      write     SetDeskewRange default 180;
    property    DeskewStep : double
      read      FDeskewStep
      write     SetDeskewStep;
    property    ReturnHoughImage : boolean
      read      FReturnHough
      write     FReturnHough default False;
  published
    // Published declarations
  end;


implementation

uses {$IFNDEF GE_DXE2}
      SysUtils,
     {$ELSE}
      System.SysUtils,
     {$ENDIF}
     mcmImageColor; // Included to support deskew - Baird (Conversion to grey and threshold).

const BairdMarker : byte = 2; // Used with Deskew.

constructor TmcmImageTransform.Create(AOwner : TComponent);
var i, x : integer;
begin
  Inherited Create(AOwner);
  Fdp := 1;
  Fxy := Nil;
  Fdxdy   := Nil;
  FClockwise   := True;
  FDegree      := 0.0;
  FxScale      := 1.0;
  FyScale      := 1.0;
  FxShear      := 0.0;
  FyShear      := 0.0;
  FxDist       := 0.0;
  FyDist       := 0.0;
  FKeepResSize := False;
  FInterpolate := ST_NEAREST;
  FIMethod     := NearestNeighbour;
  FBKColor     := 0;

  // Deskew parameters.
  FUseBaird     := True;
  FReturnHough  := False;
  FDeskewMaxInt := 1;
  FDeskewRange  := 180;
  FDeskewStep   := 1.0;

  // Real values conversion to integer values.
  // As decimal's are scaled from 0.0 <= t < 1.0 to 0 <= t < 256
  // t^2 is solved by t * t / 256, and
  // t^3 is solved by t * t * t / 65536

  // Look-up tables for t^2 and t^3.
  for i := 0 to 255
  do begin
     x := i * i;
     Fp2[i] := x div 256;
     x := x * i;
     Fp3[i] := x div 65536;
  end;
end; // TmcmImageTransform.Create.


destructor TmcmImageTransform.Destroy;
begin
  if (Fxy <> Nil)
  then FreeMem(Fxy);
  Fxy := Nil;
  if (Fdxdy <> Nil)
  then FreeMem(Fdxdy);
  Fdxdy := Nil;
  Inherited Destroy;
end; // TmcmImageTransform.Destroy.


procedure TmcmImageTransform.SetBKColor(Value : TColorRef);
begin
  FBKColor := Value;
end; // TmcmImageTransform.SetBKColor.


procedure TmcmImageTransform.InitAffine;
var i : integer;
begin
  // FDataCount must always be dividable by 2.
  FDataCount := ((FResImage.Width + 1) div 2) * 2;
  if (Fxy = Nil)
  then GetMem(Fxy, FDataCount * SizeOf(TPoint));
  if (Fdxdy = Nil)
  then GetMem(Fdxdy, FDataCount * SizeOf(TPoint));

  // Look-up table for Scanlines[]
  if (FScanLines = Nil)
  then GetMem(FScanLines, FSrcImage[0].Height * SizeOf(pointer));
  for i := 0 to (FSrcImage[0].Height - 1)
  do FScanLines^[i] := longint(FSrcImage[0].ScanLine[i]);

  Fdp := FSrcImage[0].BitCount div 8;
  if (Fdp = 0)
  then Fdp := 1;

  if FMMX
  then begin
       for i := 0 to 1
       do begin
          FAffineInt[i,0] := Round(1024.0 * Fim[i,0]);
          FAffineInt[i,1] := Round(1024.0 * Fim[i,1]);
          FAffineInt[i,2] := FAffineInt[i,0];
          FAffineInt[i,3] := FAffineInt[i,1];
       end;
       PInteger(@FAffineInt[2,0])^ := Round(1024.0 * Fim[0,2]);
       PInteger(@FAffineInt[2,2])^ := Round(1024.0 * Fim[1,2]);

       //
       FAffineInt[3,0] := 2;
       FAffineInt[3,1] := 0;
       FAffineInt[3,2] := 2;
       FAffineInt[3,3] := 0;

       // Coordinate multiplier for RGB/24 bit and RGBA/32 bit
       FAffineInt[4,0] := Fdp;
       FAffineInt[4,1] := 0;
       FAffineInt[4,2] := 1;
       FAffineInt[4,3] := 0;
  end;
end; // TmcmImageTransform.InitAffine.


procedure TmcmImageTransform.DoneAffine;
begin
  // Clean up!
  if (FScanLines <> Nil)
  then FreeMem(FScanLines);
  FScanLines := Nil;
  if (Fxy <> Nil)
  then FreeMem(Fxy);
  Fxy := Nil;
  if (Fdxdy <> Nil)
  then FreeMem(Fdxdy);
  Fdxdy := Nil;
end; // TmcmImageTransform.DoneAffine.

//------------------------------------------------------------------------------
// MMX Calculate coordinates.
//------------------------------------------------------------------------------

function TmcmImageTransform.CalcAffineLine(y : integer) : TmcmErrorCode;
var x          : integer;
    nx, ny     : single;
    pAffineInt : pointer;
    pxy        : pointer;
    pdxdy      : pointer;
    Count      : integer;
begin
  // Calculates the SourceImage coordinates for one line (y) in the ResultImage.
  // Affine transformation - translate, rotate, scale/stretch and shear.
  // x' = ax + by + c
  // y' = dx + ey + f
  Result := EC_OK;
  if FMMX // and False
  then begin
       pxy        := Fxy;
       pdxdy      := Fdxdy;
       pAffineInt := @FAffineInt;
       Count      := FDataCount;
       asm
         // Save registers to stack
         push      ebx
         push      edi
         push      esi

         // data points to process.
         mov       ecx,Count
         dec       ecx
         test      ecx,ecx

         jz        @EndOfData   // Check that data count > zero bytes.

         // Load initial x, y
         mov       ax,smallint(y)
         shl       eax,16
         mov       ax,cx        // ax = FDataCount
         {$IFNDEF DCB3_5}
         pxor      mm0,mm0
         movd      mm1,eax
         dec       ax
         movd      mm0,eax
         psllq     mm1,32
         por       mm0,mm1      // mm0 = y1,x1,y0,x0 - ResultImage coordinates.
         {$ELSE}
         db        $0F,$EF,$C0
         db        $0F,$6E,$C8
         dec       ax
         db        $0F,$6E,$C0
         db        $0F,$73,$F1,$20
         db        $0F,$EB,$C1
         {$ENDIF}

         // Load affine matrix coefficients.
         mov       ebx,pAffineInt

         {$IFNDEF DCB3_5}
         movq      mm5,[ebx]    // b,a,b,a
         movq      mm6,[ebx+8]  // e,d,e,d
         movq      mm7,[ebx+16] // f,c
         {$ELSE}
         db        $0F,$6F,$2B
         db        $0F,$6F,$73,$08
         db        $0F,$6F,$7B,$10
         {$ENDIF}

         // Set-up initial pointers to coordinates and delta.
         mov       edi,pxy
         mov       esi,pdxdy

         // Optimise further.
         // move these to lines up before loop and  again down before loop ends (last dec ecx).
         //         movq      mm2,mm0    // mm2 = y1, x1, y0, x0
         //         movq      mm3,mm0    // mm3 = y1,x1,y0,x0

         // process coordinates
         @LoopQWORD:

         {$IFNDEF DCB3_5}
         // calc x'
         movq      mm2,mm0    // mm2 = y1, x1, y0, x0
         pmaddwd   mm2,mm5    // mm2 = ax1 + by1, ax0 + by0

         // calc y'
         movq      mm3,mm0    // mm3 = y1,x1,y0,x0
         pmaddwd   mm3,mm6    // mm3 = dx1 + ey1, dx0 + ey0

         punpckhdq mm4,mm2    // mm4 = ax1 + by1, -
         punpckhdq mm4,mm3    // mm4 = dx1 + ey1, ax1 + by1
         paddd     mm4,mm7    // mm4 = dx1 + ey1 + f, ax1 + by1 + c

         movq      mm1,mm4
         pslld     mm1,22
         psrld     mm1,24
         movq      qword ptr [esi+ecx*8],mm1

         psrad     mm4,10      // mm4 scaled down to true values ( / 1024).
         movq      qword ptr [edi+ecx*8],mm4
         {$ELSE}
         db        $0F,$6F,$D0
         db        $0F,$F5,$D5

         db        $0F,$6F,$D8
         db        $0F,$F5,$DE

         db        $0F,$6A,$E2
         db        $0F,$6A,$E3
         db        $0F,$FE,$E7

         db        $0F,$6F,$CC
         db        $0F,$72,$F1,$16
         db        $0F,$72,$D1,$18
         db        $0F,$7F,$0C,$CE

         db        $0F,$72,$E4,$0A
         db        $0F,$7F,$24,$CF
         {$ENDIF}

         dec       ecx        // dec index to coordinate buffers.

         {$IFNDEF DCB3_5}
         movq      mm4,mm2    // mm4 = -, ax0 + by0
         punpckldq mm4,mm3    // mm4 = dx0 + ey0, ax0 + by0
         paddd     mm4,mm7    // mm4 = dx0 + ey0 + f, ax0 + by0 + c

         movq      mm1,mm4
         pslld     mm1,22
         psrld     mm1,24
         movq      qword ptr [esi+ecx*8],mm1

         psrad     mm4,10      // mm4 scaled down to true values ( / 1024).
         movq      qword ptr [edi+ecx*8],mm4

         psubw     mm0,[ebx+24]    // increment x coordinate
         {$ELSE}
         db        $0F,$6F,$E2
         db        $0F,$62,$E3
         db        $0F,$FE,$E7

         db        $0F,$6F,$CC
         db        $0F,$72,$F1,$16
         db        $0F,$72,$D1,$18
         db        $0F,$7F,$0C,$CE

         db        $0F,$72,$E4,$0A 
         db        $0F,$7F,$24,$CF

         db        $0F,$F9,$43,$18
         {$ENDIF}
         dec       ecx
         jns       @LoopQWORD

         @EndOfData:

         // Restore stack
         pop       esi
         pop       edi
         pop       ebx

         // Empty EMMS registers.
         {$IFNDEF DCB3_5}
         emms
         {$ELSE}
         db        $0F,$77  // emms - clean-up que.
         {$ENDIF}
       end;
  end
  else begin
       for x := 0 to (ResultImage.Width - 1)
       do begin
          // Calculate nearest point.
          nx := x * Fim[0,0] + y * Fim[0,1] + Fim[0,2];
          ny := x * Fim[1,0] + y * Fim[1,1] + Fim[1,2];

          // Nearest point in Source.
          Fxy^[x].x := Round(nx - 0.499999);
          Fxy^[x].y := Round(ny - 0.499999);
          {
          Much slower than above - saving in time is approx. 26 %
          Fxy^[x].x := Trunc(nx);
          Fxy^[x].y := Trunc(ny);
          }

          // Nearest point in Source.
          Fdxdy^[x].x := Round(256.0 * (nx - Fxy^[x].x));
          Fdxdy^[x].y := Round(256.0 * (ny - Fxy^[x].y));
          {
          Much slower than above!!! - NOTE: Trunc is slower than Round!!!
          Fdxdy^[x].x := Round(255 * Frac(nx));
          Fdxdy^[x].y := Round(255 * Frac(ny));
          }
       end;
  end;
end; // TmcmImageTransform.CalcAffineLine.


//------------------------------------------------------------------------------
// Bell filter
//------------------------------------------------------------------------------

function BellFilter(Value : Single) : Single;
begin
  if (Value < 0.0)
  then Value := -Value;
  if (Value < 0.5)
  then Result := 0.75 - Sqr(Value)
  else if (Value < 1.5)
       then begin
            Value := Value - 1.5;
            Result := 0.5 * Sqr(Value);
       end
       else Result := 0.0;
end; // BellFilter.


//------------------------------------------------------------------------------
// B-spline filter
//------------------------------------------------------------------------------

function SplineFilter(Value : Single) : Single;
var t : single;
begin
  if (Value < 0.0)
  then Value := -Value;
  if (Value < 1.0)
  then begin
       t := Sqr(Value);
       Result := 0.5 * t * Value - t + 2.0 / 3.0;
  end
  else if (Value < 2.0)
       then begin
            Value := 2.0 - Value;
            Result := 1.0 / 6.0 * Sqr(Value) * Value;
       end
       else Result := 0.0;
end; // SplineFilter.


//------------------------------------------------------------------------------
// Lanczos3 filter
//------------------------------------------------------------------------------

function Lanczos3Filter(Value : Single) : Single;

  function SinC(Value : Single) : Single;
  begin
    if (Value <> 0.0)
    then begin
         Value := Value * PI;
         Result := Sin(Value) / Value
    end
    else Result := 1.0;
  end; { End SinC.                                                             }

begin
  if (Value < 0.0)
  then Value := -Value;
  if (Value < 3.0)
  then Result := SinC(Value) * SinC(Value / 3.0)
  else Result := 0.0;
end; // Lanczos3Filter.


//------------------------------------------------------------------------------
// Mitchell filter
//------------------------------------------------------------------------------

function MitchellFilter(Value : Single) : Single;
const B = (1.0 / 3.0);
      C = (1.0 / 3.0);
var   t : single;
begin
  if (Value < 0.0)
  then Value := -Value;
  t := Sqr(Value);
  if (Value < 1.0)
  then begin
       Value := (((12.0 - 9.0 * B - 6.0 * C) * (Value * t)) +
                 ((-18.0 + 12.0 * B + 6.0 * C) * t) + (6.0 - 2 * B));
       Result := Value / 6.0;
  end
  else if (Value < 2.0)
       then begin
            Value := (((-1.0 * B - 6.0 * C) * (Value * t)) +
                      ((6.0 * B + 30.0 * C) * t) +
                      ((-12.0 * B - 48.0 * C) * Value) +
                      (8.0 * B + 24 * C));
            Result := Value / 6.0;
       end
       else Result := 0.0;
end; // MitchellFilter.


function TmcmImageTransform.Resize(Method : TmcmInterpolate) : TmcmImage;
var TempKeep : boolean;
begin
  FError := EC_OK;
  Result := Nil;
  if Assigned(FSrcImage[0])
  then begin
       if Assigned(FResImage)
       then begin
            FInterpolate := Method;
            if (FSrcImage[0].Width > 0)
            then FxScale := FResImage.Width / FSrcImage[0].Width
            else begin
                 FError := EC_BADFORMAT;
                 Exit;
            end;
            if (FSrcImage[0].Height > 0)
            then FyScale := FResImage.Height / FSrcImage[0].Height
            else begin
                 FError := EC_BADFORMAT;
                 Exit;
            end;
            FDegree := 0.0;
            FxDist  := 0.0;
            FyDist  := 0.0;
            FxShear := 0.0;
            FyShear := 0.0;
            FxDist  := 0.0;
            FyDist  := 0.0;
            {
            if (Method = ST_SMART)
            then begin
                 if (FxScale <= 0.50)
                 then FxDist := -0.25;
                 if (FxScale <= 0.250)
                 then FxDist := -0.5;
                 if (FyScale <= 0.50)
                 then FyDist := -0.25;
                 if (FyScale <= 0.250)
                 then FyDist := -0.5;
            end;
            }
            TempKeep := FKeepResSize;
            FKeepResSize := True;
            Result := Affine;
            FKeepResSize := TempKeep;
       end
       else FError := EC_MISRESULTIMAGE;
  end
  else FError := EC_MISSOURCEIMAGE;
end; // TmcmImageTransform.Resize.


function TmcmImageTransform.Flip : TmcmImage;
var yb, yt    : cardinal;
    pS, pT    : PVectorB;
    pL1, pL2  : PVectorB;
    LongWidth : cardinal;
    yLoop     : longint;
begin
  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if (FResImage <> Nil)
       then begin
            if (FResImage.ImageFormat <> FSrcImage[0].ImageFormat) or
               (FResImage.Width <> FSrcImage[0].Width) or
               (FResImage.Height <> FSrcImage[0].Height)
            then begin
                 FResImage.Clear;
                 FResImage.Height      := FSrcImage[0].Height;
                 FResImage.Width       := FSrcImage[0].Width;
                 FResImage.ImageFormat := FSrcImage[0].ImageFormat;
                 FResImage.XResolution := FSrcImage[0].XResolution;
                 FResImage.YResolution := FSrcImage[0].YResolution;
                 FResImage.Palette     := FSrcImage[0].Palette;
            end;
       end
       else CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Width, FSrcImage[0].Height, True);

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then Exit;

            LongWidth := FSrcImage[0].LongLineWidth;
            GetMem(pL1, LongWidth * SizeOf(byte));
            GetMem(pL2, LongWidth * SizeOf(byte));
            try
              if Odd(FSrcImage[0].Height)
              then yLoop := (FSrcImage[0].Height div 2)
              else yLoop := (FSrcImage[0].Height div 2 - 1);

              yt := FSrcImage[0].Height - 1;
              for yb := 0 to yLoop
              do begin
                 pS := FSrcImage[0].ScanLine[yb];
                 pT := FSrcImage[0].ScanLine[yt];
                 CopyMemory(pL1, pS, LongWidth);
                 CopyMemory(pL2, pT, LongWidth);
                 pS := FResImage.ScanLine[yb];
                 pT := FResImage.ScanLine[yt];
                 CopyMemory(pT, pL1, LongWidth);
                 CopyMemory(pS, pL2, LongWidth);
                 dec(yt);
              end;
              Result := FResImage;
            finally
              FreeMem(pL1);
              FreeMem(pL2);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end
  else FError := EC_MISSOURCEIMAGE; // No source.
end; // TmcmImageTransform.Flip.


function TmcmImageTransform.Mirror : TmcmImage;
var y, x, w, r : cardinal;
    pS, pT     : PVectorB;
    pL         : PVectorB;
    LongWidth  : cardinal;
    TransBit   : PvectorB;
begin
  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if (FResImage <> Nil)
       then begin
            if (FResImage.ImageFormat <> FSrcImage[0].ImageFormat) or
               (FResImage.Width <> FSrcImage[0].Width) or
               (FResImage.Height <> FSrcImage[0].Height)
            then begin
                 FResImage.Clear;
                 FResImage.Height      := FSrcImage[0].Height;
                 FResImage.Width       := FSrcImage[0].Width;
                 FResImage.ImageFormat := FSrcImage[0].ImageFormat;
                 FResImage.XResolution := FSrcImage[0].XResolution;
                 FResImage.YResolution := FSrcImage[0].YResolution;
                 FResImage.Palette     := FSrcImage[0].Palette;
            end;
       end
       else CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Width, FSrcImage[0].Height, True);

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then Exit;
            LongWidth := FSrcImage[0].LongLineWidth;
            GetMem(pL, LongWidth * SizeOf(byte));
            try
              if (FSrcImage[0].BitCount = 1)
              then begin
                   // Create a look-up table with reversed bits in the range
                   // $00 - $FF
                   GetMem(TransBit, 255 * SizeOf(Byte));
                   asm
                     // Save registers to stack
                     push  ebx
                     push  edi

                     mov   ebx,255 // ebx (bl) is the index into the table and
                                   // the bits that needs reversed.
                     mov   ah,bl
                     // Set-up initial pointers to look-up table.
                     mov   edi,TransBit
                     @NextRotate:
                     mov   cx,8
                     mov   ah,bl
                     @EightBits:   // invert the bit sequence.
                     shl   ah,1
                     rcr   al,1
                     dec   cx
                     jnz   @EightBits

                     mov   [edi+ebx],al // Move reversed bit sequence to look-up table.
                     dec   ebx
                     jns   @NextRotate

                     // Restore stack
                     pop   edi
                     pop   ebx
                   end;
                   r := (FSrcImage[0].Width and $07);
                   w := (FSrcImage[0].Width and $FFFFFFF8) div 8 - 1;
              end
              else begin
                   TransBit := Nil;
                   w := FSrcImage[0].Width - 1;
              end;

              for y := 0 to (FSrcImage[0].Height - 1)
              do begin
                 pS := FSrcImage[0].ScanLine[y];
                 pT := FResImage.ScanLine[y];
                 CopyMemory(pL, pS, LongWidth);

                 case FSrcImage[0].BitCount of
                 1  : begin
                        asm
                          // Save registers to stack
                          push  ebx
                          push  edi
                          push  esi

                          mov   ebx,TransBit  // Get pointer to look-up table
                          mov   edx,w         // Set line width in bytes

                          mov   esi,pL        // Get Pointers to source and
                          mov   edi,pT        // target lines.

                          mov   ecx,r         // Check for bits less than 8 at
                          test  cx,cx         // line end.
                          jz    @NextByte
                          inc   edx
                          mov   ch,8          // Set-up bit shift size
                          sub   ch,cl
                          xchg  cl,ch
                          mov   al,[esi+edx]  // Get first byte
                          {$IFNDEF VER140}
                          xlat                // Reverse bits
                          {$ELSE}
                          db    $D7
                          {$ENDIF}
                          shl   ax,8          // Shift first byte into ah
                          dec   edx

                          @NextShift:
                          mov   al,[esi+edx]  // Get next byte
                          {$IFNDEF VER140}
                          xlat                // Reverse bits
                          {$ELSE}
                          db    $D7
                          {$ENDIF}
                          shl   ax,cl         // shift remaining bits into ah
                          xchg  cl,ch
                          mov   [edi],ah      // Move 8 bits to target line
                          shl   ax,cl         // Shift remaining bits into ah
                          xchg  cl,ch
                          inc   edi
                          dec   edx
                          jns   @NextShift
                          shl   ax,cl         // Shift bits to MSB in ah
                          mov   [edi],ah      // Move 8 (r) bits to target line
                          jmp   @EndOfLine

                          @NextByte:
                          mov   al,[esi+edx]  // Get next byte
                          {$IFNDEF VER140}
                          xlat                // Reverse bits
                          {$ELSE}
                          db    $D7
                          {$ENDIF}
                          mov   [edi],al      // Move 8 bits to target line
                          inc   edi
                          dec   edx
                          jns   @NextByte

                          @EndOfLine:
                          // Restore stack
                          pop   esi
                          pop   edi
                          pop   ebx
                        end;
                      end;
                 4  : begin
                      end;
                 8  : begin
                        for x := 0 to w
                        do pT^[w-x] := pL^[x];
                      end;
                 24 : begin
                        for x := 0 to w
                        do PVectorRGB(pT)^[w-x] := PVectorRGB(pL)^[x];
                      end;
                 32 : begin
                        for x := 0 to w
                        do PVectorRGBA(pT)^[w-x] := PVectorRGBA(pL)^[x];
                      end;
                 end;
              end;
              Result := FResImage;
            finally
              FreeMem(pL);
              if (TransBit <> Nil)
              then FreeMem(TransBit);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end
  else FError := EC_MISSOURCEIMAGE; // No source.
end; // TmcmImageTransform.Mirror.


function TmcmImageTransform.Rotate90(Is90 : boolean) : TmcmImage;
// With Interpolate = ST_NEAREST, this method rotates an image either 90 or 270
// degrees.
var y, x, w, h : cardinal;
    pS, pT     : PVectorL;
begin
  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Result Image.
  if Assigned(FSrcImage[0])
  then begin
       if (FResImage <> Nil)
       then begin
            if (FResImage.ImageFormat <> FSrcImage[0].ImageFormat) or
               (FResImage.Height <> FSrcImage[0].Width) or
               (FResImage.Width <> FSrcImage[0].Height)
            then begin
                 FResImage.Clear;
                 FResImage.Width       := FSrcImage[0].Height;
                 FResImage.Height      := FSrcImage[0].Width;
                 FResImage.ImageFormat := FSrcImage[0].ImageFormat;
                 FResImage.XResolution := FSrcImage[0].XResolution;
                 FResImage.YResolution := FSrcImage[0].YResolution;
                 FResImage.Palette     := FSrcImage[0].Palette;
            end;
       end
       else CheckResult(FSrcImage[0].ImageFormat, FSrcImage[0].Height, FSrcImage[0].Width, True);

       if Assigned(FResImage)
       then begin
            if (FSrcImage[0].Empty or FResImage.Empty)
            then Exit;

            h := FSrcImage[0].Height;
            w := FSrcImage[0].Width;
            // Allocate ScanLine look-up tables
            GetMem(pS, h * SizeOf(Longint));
            GetMem(pT, w * SizeOf(Longint));

            try
              if (pS <> Nil) and (pT <> Nil)
              then begin
                   dec(w);
                   dec(h);
                   // Fill ScanLine look-up tables
                   if Is90
                   then begin
                        for y := 0 to h
                        do pS^[y] := longint(FSrcImage[0].ScanLine[h-y]);
                        for x := 0 to w
                        do pT^[x] := longint(FResImage.ScanLine[x]);
                   end
                   else begin
                        for y := 0 to h
                        do pS^[y] := longint(FSrcImage[0].ScanLine[y]);
                        for x := 0 to w
                        do pT^[x] := longint(FResImage.ScanLine[w-x]);
                   end;

                   case FSrcImage[0].BitCount of
                   1  : begin
                          // Note: A text page where the background is 1's
                          // it does pay to invert the image, rotate and invert
                          // back again.
                          for y := 0 to h
                          do begin
                             asm
                               // Save registers to stack
                               push  ebx
                               push  edi
                               push  esi

                               mov   edx,pS         // Get source Scanline from lookup table
                               mov   ebx,y          // y is index to table.
                               mov   esi,[edx+ebx*4]

                               mov   eax,ebx
                               mov   cl,$07
                               and   eax,$07
                               sub   cl,al          // cl  = iy

                               mov   ah,01
                               shl   ah,cl          // Pixel in byte to set for this line.

                               shr   ebx,$03        // ebx = y8
                               xor   edx,edx        // edx = x loop from 0 to (Width - 1)
                               mov   ecx,w          // ecx = line width
                               // Process line
                               @NextByte:
                               mov   al,[esi]       // Get next byte
                               inc   esi            // Inc source addr to next 8 bit
                               test  al,al
                               jz    @SkipByte      // Pixels are 0 and we only set 1's.
                                                    // therefore skip byte is 0.

                               sub   ecx,edx
                               cmp   ecx,07
                               jle   @NextBit       // ch = "No pixels to iterate through" max 8
                               mov   cl,7           // We have more than 8 bits left

                               @NextBit:
                               mov   edi,pT         // Get dest Scaline from lookup table
                               sal   al,1           // Shift MSB into CF
                               jnc   @SkipBit       // Is MSB set? If CF = 1 then yes

                               mov   edi,[edi+edx*4]// Add dest index
                               add   edi,ebx        // Add (y shl 3)
                               or    [edi],ah       // Dest = Dest or ah

                               @SkipBit:
                               inc   edx            // Inc dest index to next line
                               dec   cl             // dec bits to loop
                               jns   @NextBit

                               mov   ecx,w          // ecx = line width
                               cmp   edx,ecx        // Compare with line width
                               jle   @NextByte
                               jmp   @EndOfLine     // We're done with this line

                               @SkipByte:
                               add   edx,$8
                               mov   ecx,w          // ecx = line width
                               cmp   edx,ecx        // Compare with line width
                               jle   @NextByte

                               @EndOfLine:

                               // Restore stack
                               pop   esi
                               pop   edi
                               pop   ebx
                             end;
                          end;
                          (*
                          // Pascal code rotated the test image in 34 ms
                          // where asm code does the same in 23 ms.
                          for y := 0 to h
                          do begin
                             y8 := y shr 3;
                             iy := 7 - y mod 8;
                             x  := 0;
                             x8 := 0;
                             while (x <= w)
                             do begin
                                b := PVectorB(pS^[y])^[x8];
                                if (b = 0)
                                then inc(x, 8)
                                else begin
                                     ix := $80;
                                     while (x <= w) and (ix > 0)
                                     do begin
                                        if ((b and ix) <> 0)
                                        then PVectorB(pT^[x])^[y8] := PVectorB(pT^[x])^[y8] or (1 shl iy);
                                        ix := ix shr 1;
                                        inc(x);
                                     end;
                                end;
                                inc(x8);
                             end;
                          end;
                          *)
                        end;
                   4  : begin
                        end;
                   8  : begin
                          for y := 0 to h
                          do begin
                             for x := 0 to w
                             do PVectorB(pT^[x])^[y] := PVectorB(pS^[y])^[x];
                          end;
                        end;
                   24 : begin
                          for y := 0 to h
                          do begin
                             for x := 0 to w
                             do PVectorRGB(pT^[x])^[y] := PVectorRGB(pS^[y])^[x];
                          end;
                        end;
                   32 : begin
                          for y := 0 to h
                          do begin
                             for x := 0 to w
                             do PVectorRGBA(pT^[x])^[y] := PVectorRGBA(pS^[y])^[x];
                          end;
                        end;
                   end;
              end
              else FError := EC_NOMEMORY;
              Result := FResImage;
            finally
              if (pS <> Nil)
              then FreeMem(pS);
              if (pT <> Nil)
              then FreeMem(pT);
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end
  else FError := EC_MISSOURCEIMAGE; // No source.
end; // TmcmImageTransform.Rotate90.


function TmcmImageTransform.Rotate(Clockwise : boolean; Degree : double) : TmcmImage;
var ASource  : TKernelImage;
var TempKeep : boolean;
begin
  FError := EC_OK;
  Result := Nil;
  if CheckSource(0, [IF_BW,IF_GREY8,IF_PAL8,IF_RGB24,IF_RGBA32])
  then begin
       if (FSrcImage[0].ImageFormat in [IF_BW,IF_PAL8])
       then FInterpolate := ST_NEAREST;
       if (FInterpolate = ST_NEAREST)
       then begin
            if (Degree = 90) or (Degree = -270)
            then begin
                 Result := Rotate90(Clockwise);
                 Exit;
            end;
            if (Degree = 270) or (Degree = -90)
            then begin
                 Result := Rotate90(Not(Clockwise));
                 Exit;
            end;
            if (Abs(Degree) = 180)
            then begin
                 ASource := FSrcImage[0];
                 Flip;
                 if (FError = EC_OK)
                 then begin
                      FSrcImage[0] := FResImage;
                      Result := Mirror;
                      FSrcImage[0] := ASource;
                 end;
                 Exit;
            end;
       end;

       FClockwise := Clockwise;
       FDegree := Degree;
       FxScale := 1.0;
       FyScale := 1.0;
       FxShear := 0.0;
       FyShear := 0.0;
       FxDist  := 0.0;
       FyDist  := 0.0;
       TempKeep := FKeepResSize;
       FKeepResSize := False;
       Result := Affine;
       FKeepResSize := TempKeep;
  end;
end; // TmcmImageTransform.Rotate.


//------------------------------------------------------------------------------
// Interpolation methods.
//------------------------------------------------------------------------------

procedure TmcmImageTransform.NearestNeighbour(const y : integer);
// NearestNeighbour interpolation picks the pixel at truncated (x,y) coordinate.
var x        : integer;
    pT       : PVectorB;
    pTrgb    : PVectorRGB;
    pTrgba   : PVectorRGBA;
    sx, sy   : integer;
    sbx, sbi : integer;
    tbx, tbi : integer;
    bit      : byte;
begin
  x := 0;
  while (x < FResImage.Width)
  do begin
     sy := Fxy^[x].y;
     if (0 <= sy) and (sy < FSrcHeight)
     then begin
          sx := Fxy^[x].x;
          if (0 <= sx) and (sx < FSrcWidth)
          then break;
     end;
     inc(x);
  end;

  case FSrcImage[0].BitCount{Fdp} of
  1  : begin
         pT := FResImage.ScanLine[y];
         tbx := x shr 3;
         tbi := 7 - (x and $07);
         while (x < FResImage.Width)
         do begin
            sy := Fxy^[x].y;
            if (0 <= sy) and (sy < FSrcHeight)
            then begin
                 sx := Fxy^[x].x;
                 if (0 <= sx) and (sx < FSrcWidth)
                 then begin
                      sbx := sx shr 3;
                      sbi := 7 - (sx and $07);
                      bit := (PVectorB(FScanLines^[sy])^[sbx] and BitMask[sbi]);
                      if (bit <> 0)
                      then pT^[tbx] := pT^[tbx] or BitMask[tbi]
                      else pT^[tbx] := pT^[tbx] and Not(BitMask[tbi]);
                 end
                 else x := FResImage.Width;
            end
            else x := FResImage.Width;

            dec(tbi);
            if (tbi < 0)
            then begin
                 tbi := 7;
                 inc(tbx);
            end;
            inc(x);
         end;
       end;
  8  : begin
         pT := FResImage.ScanLine[y];
         while (x < FResImage.Width)
         do begin
            sy := Fxy^[x].y;
            if (0 <= sy) and (sy < FSrcHeight)
            then begin
                 sx := Fxy^[x].x;
                 if (0 <= sx) and (sx < FSrcWidth)
                 then pT^[x] := PVectorB(FScanLines^[sy])^[sx]
                 else x := FResImage.Width;
            end
            else x := FResImage.Width;
            inc(x);
         end;
       end;
  24 : begin
         pTrgb := FResImage.ScanLine[y];
         while (x < FResImage.Width)
         do begin
            sy := Fxy^[x].y;
            if (0 <= sy) and (sy < FSrcHeight)
            then begin
                 sx := Fxy^[x].x;
                 if (0 <= sx) and (sx < FSrcWidth)
                 then pTrgb^[x] := PVectorRGB(FScanLines^[sy])^[sx]
                 else x := FResImage.Width;
            end
            else x := FResImage.Width;
            inc(x);
         end;
       end;
  32 : begin
         pTrgba := FResImage.ScanLine[y];
         while (x < FResImage.Width)
         do begin
            sy := Fxy^[x].y;
            if (0 <= sy) and (sy < FSrcHeight)
            then begin
                 sx := Fxy^[x].x;
                 if (0 <= sx) and (sx < FSrcWidth)
                 then pTrgba^[x] := PVectorRGBA(FScanLines^[sy])^[sx]
                 else x := FResImage.Width;
            end
            else x := FResImage.Width;
            inc(x);
         end;
       end;
  end;
end; // TmcmImageTransform.NearestNeighbour.


procedure TmcmImageTransform.Bilinear(const y : integer);
// Bilinear Interpolation.
//
// |-----------|
// | p00 | p01 |
// |-----------|
// | p10 | p11 |
// |-----------|
//
// Pnm where m,n = [0,1] are pixels from SourceImage
// Pr point in ResultImage
//
//   v1 := p00 + (p01 - p00) * dx
//   v2 := p10 + (p11 - p10) * dx
//   Pr := v1 + (v2 - v1) * dy
//
// or
//
//   v1 := p00 * (1.0 - dx) + p01 * dx
//   v2 := p10 * (1.0 - dx) + p11 * dx
//   Pr := v1 * (1.0 - dy) + v2 * dy
//
var i, x, x3 : integer;
    pS, pSu, pT : PVectorB;
    {
    pSrgb   : PVectorRGB;
    pTrgb   : PVectorRGB;
    pSrgba  : PVectorRGBA;
    pTrgba  : PVectorRGBA;
    }
    v1, v2  : integer;
    {
    g1, g2  : integer;
    b1, b2  : integer;
    a1, a2  : integer;
    }
    dx, dy  : integer;
    ndx, ndy: integer;
    sy      : integer;
    s1, s2  : integer;
begin
  x := 0;
  while (x < FResImage.Width)
  do begin
     if (0 <= Fxy^[x].y) and (Fxy^[x].y < FSrcHeight)
     then if (0 <= Fxy^[x].x) and (Fxy^[x].x < FSrcWidth)
          then break;
     inc(x);
  end;

  case Fdp of
  1 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     pS := pointer(FScanLines^[sy]);
                     dx := Fdxdy^[x].x;
                     ndx := 256 - dx;

                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := s1;

                     v1 := pS^[s1] * ndx + pS^[s2] * dx;
                     inc(sy);
                     if (sy < FSrcHeight)
                     then begin
                          pS := pointer(FScanLines^[sy]);
                          dy := Fdxdy^[x].y;
                          v2 := pS^[s1] * ndx + pS^[s2] * dx;
                          v1 := v1 * (256 - dy) + v2 * dy;
                          pT^[x] := v1 shr 16;
                     end
                     else pT^[x] := v1 shr 8;
                end;
           end;
           inc(x);
        end;
      end;
  3,
  4 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := s1;
                     s1 := s1 * Fdp;
                     s2 := s2 * Fdp;
                     x3 := x * Fdp;

                     pS := pointer(FScanLines^[sy]);
                     dx := Fdxdy^[x].x;
                     dy := Fdxdy^[x].y;
                     ndx := 256 - dx;
                     ndy := 256 - dy;

                     inc(sy);
                     if (sy < FSrcHeight)
                     then pSu := pointer(FScanLines^[sy])
                     else pSu := pS;

                     for i := 1 to Fdp
                     do begin
                        v1 := pS^[s1] * ndx + pS^[s2] * dx;
                        v2 := pSu^[s1] * ndx + pSu^[s2] * dx;
                        v1 := v1 * ndy + v2 * dy;
                        inc(s1);
                        inc(s2);
                        pT^[x3] := v1 shr 16;
                        inc(x3);
                     end;
                end;
           end;
           inc(x);
        end;
        {
        pTrgb := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     pSrgb := pointer(FScanLines^[sy]);
                     dx := Fdxdy^[x].x;
                     ndx := 256 - dx;

                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := s1;
                     b1 := pSrgb^[s1].rgbtBlue * ndx + pSrgb^[s2].rgbtBlue * dx;
                     g1 := pSrgb^[s1].rgbtGreen * ndx + pSrgb^[s2].rgbtGreen * dx;
                     v1 := pSrgb^[s1].rgbtRed * ndx + pSrgb^[s2].rgbtRed * dx;

                     inc(sy);
                     if (sy < FSrcHeight)
                     then begin
                          pSrgb := pointer(FScanLines^[sy]);
                          dy := Fdxdy^[x].y;
                          ndy := 256 - dy;

                          b2 := pSrgb^[s1].rgbtBlue * ndx + pSrgb^[s2].rgbtBlue * dx;
                          g2 := pSrgb^[s1].rgbtGreen * ndx + pSrgb^[s2].rgbtGreen * dx;
                          v2 := pSrgb^[s1].rgbtRed * ndx + pSrgb^[s2].rgbtRed * dx;

                          b1 := b1 * ndy + b2 * dy;
                          g1 := g1 * ndy + g2 * dy;
                          v1 := v1 * ndy + v2 * dy;

                          pTrgb^[x].rgbtBlue := b1 shr 16;
                          pTrgb^[x].rgbtGreen := g1 shr 16;
                          pTrgb^[x].rgbtRed := v1 shr 16;
                     end
                     else begin
                          pTrgb^[x].rgbtBlue  := b1 shr 8;
                          pTrgb^[x].rgbtGreen := g1 shr 8;
                          pTrgb^[x].rgbtRed   := v1 shr 8;
                     end;
                end;
           end;
           inc(x);
        end;
        }
      end;
{
  4 : begin
        pTrgba := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     pSrgba := pointer(FScanLines^[sy]);
                     dx := Fdxdy^[x].x;
                     ndx := 256 - dx;

                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := s1;
                     v1 := pSrgba^[s1].rgbRed * ndx + pSrgba^[s2].rgbRed * dx;
                     g1 := pSrgba^[s1].rgbGreen * ndx + pSrgba^[s2].rgbGreen * dx;
                     b1 := pSrgba^[s1].rgbBlue * ndx + pSrgba^[s2].rgbBlue * dx;
                     a1 := pSrgba^[s1].rgbReserved * ndx + pSrgba^[s2].rgbReserved * dx;

                     inc(sy);
                     if (sy < FSrcHeight)
                     then begin
                          pSrgba := pointer(FScanLines^[sy]);
                          dy := Fdxdy^[x].y;
                          ndy := 256 - dy;

                          v2 := pSrgba^[s1].rgbRed * ndx + pSrgba^[s2].rgbRed * dx;
                          g2 := pSrgba^[s1].rgbGreen * ndx + pSrgba^[s2].rgbGreen * dx;
                          b2 := pSrgba^[s1].rgbBlue * ndx + pSrgba^[s2].rgbBlue * dx;
                          a2 := pSrgba^[s1].rgbReserved * ndx + pSrgba^[s2].rgbReserved * dx;

                          v1 := v1 * ndy + v2 * dy;
                          g1 := g1 * ndy + g2 * dy;
                          b1 := b1 * ndy + b2 * dy;
                          a1 := a1 * ndy + a2 * dy;

                          pTrgba^[x].rgbRed := v1 shr 16;
                          pTrgba^[x].rgbGreen := g1 shr 16;
                          pTrgba^[x].rgbBlue := b1 shr 16;
                          pTrgba^[x].rgbReserved := a1 shr 16;
                     end
                     else begin
                          pTrgba^[x].rgbRed   := v1 shr 8;
                          pTrgba^[x].rgbGreen := g1 shr 8;
                          pTrgba^[x].rgbBlue  := b1 shr 8;
                          pTrgba^[x].rgbReserved  := a1 shr 8;
                     end;
                end;
           end;
           inc(x);
        end;
      end;
}
  end;
end; // TmcmImageTransform.Bilinear.


procedure TmcmImageTransform.Biquadratic(const y : integer);
// |-----------------|
// | p00 | p01 | p02 |
// |-----------------|
// | p10 | p11 | p12 |
// |-----------------|
// | p20 | p21 | p22 |
// |-----------------|
var x, i, j : integer;
    x3      : integer;
    pS, pT  : PVectorB;
    v       : array[0..2] of integer;
    dx, dy  : integer;
    sy      : integer;
    a, b, c : integer;
    s0, s1  : integer;
    s2      : integer;
begin
  pS := Nil;

  x := 0;
  while (x < FResImage.Width)
  do begin
     if (0 <= Fxy^[x].y) and (Fxy^[x].y < FSrcHeight)
     then if (0 <= Fxy^[x].x) and (Fxy^[x].x < FSrcWidth)
          then break;
     inc(x);
  end;

  case Fdp of
  1 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     dx := Fdxdy^[x].x;
                     dy := Fdxdy^[x].y;

                     s0 := s1 - 1;
                     if (s0 < 0)
                     then s0 := 0;
                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := FSrcWidth - 1;

                     for i := 0 to 2
                     do begin
                        if (0 < sy + i)
                        then begin
                             if (sy + i < FSrcHeight)
                             then pS := pointer(FScanLines^[sy+i-1])
                        end
                        else pS := pointer(FScanLines^[sy]);

                        a := pS^[s0] - 2 * pS^[s1] + pS^[s2];
                        b := pS^[s2] - pS^[s0];
                        c := pS^[s1] shl 8;
                        v[i] := ((a * Fp2[dx] + b * dx) div 2) + c;
                     end;

                     a := v[0] - 2 * v[1] + v[2];
                     b := v[2] - v[0];
                     c := v[1] shl 8;
                     // Delphi does strange things here : truncates to a
                     // smalling/16bit rather than a integer/32bit.
                     a := smallint((((a * Fp2[dy] + b * dy) div 2) + c) shr 16);
                     if (a < 0)
                     then pT^[x] := 0
                     else if (a > 255)
                          then pT^[x] := 255
                          else pT^[x] := a;
                end;
           end;
           inc(x);
        end;
      end;
  3,
  4 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     dx := Fdxdy^[x].x;
                     dy := Fdxdy^[x].y;

                     s0 := s1 - 1;
                     if (s0 < 0)
                     then s0 := 0;
                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := FSrcWidth - 1;
                     s0 := s0 * Fdp;
                     s1 := s1 * Fdp;
                     s2 := s2 * Fdp;
                     x3 := x * Fdp;

                     for j := 1 to Fdp
                     do begin
                        for i := 0 to 2
                        do begin
                           if (0 < sy + i)
                           then begin
                                if (sy + i < FSrcHeight)
                                then pS := pointer(FScanLines^[sy+i-1])
                           end
                           else pS := pointer(FScanLines^[sy]);

                           a := pS^[s0] - 2 * pS^[s1] + pS^[s2];
                           b := pS^[s2] - pS^[s0];
                           c := pS^[s1] shl 8;
                           v[i] := ((a * Fp2[dx] + b * dx) div 2) + c;
                        end;

                        a := v[0] - 2 * v[1] + v[2];
                        b := v[2] - v[0];
                        c := v[1] shl 8;
                        // Delphi does strange things here : truncates to a smalling ??
                        a := smallint((((a * Fp2[dy] + b * dy) div 2) + c) shr 16);
                        if (a < 0)
                        then pT^[x3] := 0
                        else if (a > 255)
                             then pT^[x3] := 255
                             else pT^[x3] := a;

                        inc(s0);
                        inc(s1);
                        inc(s2);
                        inc(x3);
                     end;
                end;
           end;
           inc(x);
        end;
      end;
  end;
end; // TmcmImageTransform.Biquadratic.


procedure TmcmImageTransform.Bicubic(const y : integer);
// |-----------------|
// | p00 | p01 | p02 |
// |-----------------|
// | p10 | p11 | p12 |
// |-----------------|
// | p20 | p21 | p22 |
// |-----------------|
//
// f(x) = ax^3 + bx^2 + cx + d
//
// where
//
// a = p01 + p03 - (p00 + p02)
// b = 2 * (p00 - p01) + p02 - p03
// c = p02 - p00
// d = p01
//
var x, i, j : integer;
    x3      : integer;
    pS, pT  : PVectorB;
    v       : array[0..3] of integer;
    dx, dy  : integer;
    sy      : integer;
    a, b    : integer;
    c, d    : integer;
    s0, s1  : integer;
    s2, s3  : integer;
begin
  pS := Nil;

  x := 0;
  while (x < FResImage.Width)
  do begin
     if (0 <= Fxy^[x].y) and (Fxy^[x].y < FSrcHeight)
     then if (0 <= Fxy^[x].x) and (Fxy^[x].x < FSrcWidth)
          then break;
     inc(x);
  end;

  case Fdp of
  1 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     dx := Fdxdy^[x].x;
                     dy := Fdxdy^[x].y;

                     s0 := s1 - 1;
                     if (s0 < 0)
                     then s0 := 0;
                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := FSrcWidth - 1;
                     s3 := s1 + 2;
                     if (s3 >= FSrcWidth)
                     then s3 := FSrcWidth - 1;

                     for i := 0 to 3
                     do begin
                        if (0 < sy + i)
                        then begin
                             if (sy + i < FSrcHeight)
                             then pS := pointer(FScanLines^[sy+i-1])
                        end
                        else pS := pointer(FScanLines^[sy]);

                        a := pS^[s1] + pS^[s3] - (pS^[s0] + pS^[s2]);
                        b := 2 * (pS^[s0] - pS^[s1]) + pS^[s2] - pS^[s3];
                        c := pS^[s2] - pS^[s0];
                        d := pS^[s1] shl 8;
                        v[i] := a * Fp3[dx] + b * Fp2[dx] + c * dx + d;
                     end;

                     a := v[1] + v[3] - (v[0] + v[2]);
                     b := 2 * (v[0] - v[1]) + v[2] - v[3];
                     c := v[2] - v[0];
                     d := v[1] shl 8;
                     // Delphi does strange things here : truncates to a smalling ??
                     a := smallint((a * Fp3[dy] + b * Fp2[dy] + c * dy + d) shr 16);
                     if (a < 0)
                     then pT^[x] := 0
                     else if (a > 255)
                          then pT^[x] := 255
                          else pT^[x] := a;
                end;
           end;
           inc(x);
        end;
      end;
  3,
  4 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     dx := Fdxdy^[x].x;
                     dy := Fdxdy^[x].y;

                     s0 := s1 - 1;
                     if (s0 < 0)
                     then s0 := 0;
                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := FSrcWidth - 1;
                     s3 := s1 + 2;
                     if (s3 >= FSrcWidth)
                     then s3 := FSrcWidth - 1;
                     s0 := s0 * Fdp;
                     s1 := s1 * Fdp;
                     s2 := s2 * Fdp;
                     s3 := s3 * Fdp;
                     x3 := x * Fdp;

                     for j := 1 to Fdp
                     do begin
                        for i := 0 to 3
                        do begin
                           if (0 < sy + i)
                           then begin
                                if (sy + i < FSrcHeight)
                                then pS := pointer(FScanLines^[sy+i-1])
                           end
                           else pS := pointer(FScanLines^[sy]);

                           a := pS^[s1] + pS^[s3] - (pS^[s0] + pS^[s2]);
                           b := 2 * (pS^[s0] - pS^[s1]) + pS^[s2] - pS^[s3];
                           c := pS^[s2] - pS^[s0];
                           d := pS^[s1] shl 8;
                           v[i] := a * Fp3[dx] + b * Fp2[dx] + c * dx + d;
                        end;

                        a := v[1] + v[3] - (v[0] + v[2]);
                        b := 2 * (v[0] - v[1]) + v[2] - v[3];
                        c := v[2] - v[0];
                        d := v[1] shl 8;
                        // Delphi does strange things here : truncates to a smalling ??
                        a := smallint((a * Fp3[dy] + b * Fp2[dy] + c * dy + d) shr 16);
                        if (a < 0)
                        then pT^[x3] := 0
                        else if (a > 255)
                             then pT^[x3] := 255
                             else pT^[x3] := a;

                        inc(s0);
                        inc(s1);
                        inc(s2);
                        inc(s3);
                        inc(x3);
                     end;
                end;
           end;
           inc(x);
        end;
      end;
  end;
end; // TmcmImageTransform.Bicubic.


procedure TmcmImageTransform.Bicubic2(const y : integer);
// |-----------------|
// | p00 | p01 | p02 |
// |-----------------|
// | p10 | p11 | p12 |
// |-----------------|
// | p20 | p21 | p22 |
// |-----------------|
//
// f(x) = ax^3 + bx^2 + cx + d
//
// where
//
// a = -1/6 * (p00 - p01) - 1/2 * (p02 - p01) + 1/6 * (p03 - p01)
// b =  1/2 * (p00 - p01) + 1/2 * (p02 - p01)
// c = -1/3 * (p00 - p01) + (p02 - p01) - 1/6 * (p03 - p01)
// d = p01
//
var x, i, j : integer;
    x3      : integer;
    pS, pT  : PVectorB;
    v       : array[0..3] of integer;
    dx, dy  : integer;
    sy      : integer;
    a, b    : integer;
    c, d    : integer;
    s0, s1  : integer;
    s2, s3  : integer;
    d01, d21, d31 : integer;
begin
  pS := Nil;

  x := 0;
  while (x < FResImage.Width)
  do begin
     if (0 <= Fxy^[x].y) and (Fxy^[x].y < FSrcHeight)
     then if (0 <= Fxy^[x].x) and (Fxy^[x].x < FSrcWidth)
          then break;
     inc(x);
  end;

  case Fdp of
  1 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     dx := Fdxdy^[x].x;
                     dy := Fdxdy^[x].y;

                     s0 := s1 - 1;
                     if (s0 < 0)
                     then s0 := 0;
                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := FSrcWidth - 1;
                     s3 := s1 + 2;
                     if (s3 >= FSrcWidth)
                     then s3 := FSrcWidth - 1;

                     for i := 0 to 3
                     do begin
                        if (0 < sy + i)
                        then begin
                             if (sy + i < FSrcHeight)
                             then pS := pointer(FScanLines^[sy+i-1])
                        end
                        else pS := pointer(FScanLines^[sy]);

                        d01 := pS^[s0] - pS^[s1];
                        d21 := pS^[s2] - pS^[s1];
                        d31 := pS^[s3] - pS^[s1];

                        a := (d31 - d01) div 6 - d21 div 2;
                        b := (d01 + d21) div 2;
                        c := d21 - d31 div 6 - d01 div 3;
                        d := pS^[s1] shl 8;
                        v[i] := a * Fp3[dx] + b * Fp2[dx] + c * dx + d;
                     end;

                     d01 := v[0] - v[1];
                     d21 := v[2] - v[1];
                     d31 := v[3] - v[1];
                     a := (d31 - d01) div 6 - d21 div 2;
                     b := (d01 + d21) div 2;
                     c := d21 - d31 div 6 - d01 div 3;
                     d := v[1] shl 8;
                     // Delphi does strange things here : truncates to a smalling ??
                     a := smallint((a * Fp3[dy] + b * Fp2[dy] + c * dy + d) shr 16);
                     if (a < 0)
                     then pT^[x] := 0
                     else if (a > 255)
                          then pT^[x] := 255
                          else pT^[x] := a;
                end;
           end;
           inc(x);
        end;
      end;
  3,
  4 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     dx := Fdxdy^[x].x;
                     dy := Fdxdy^[x].y;

                     s0 := s1 - 1;
                     if (s0 < 0)
                     then s0 := 0;
                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := FSrcWidth - 1;
                     s3 := s1 + 2;
                     if (s3 >= FSrcWidth)
                     then s3 := FSrcWidth - 1;
                     s0 := s0 * Fdp;
                     s1 := s1 * Fdp;
                     s2 := s2 * Fdp;
                     s3 := s3 * Fdp;
                     x3 := x * Fdp;

                     for j := 1 to Fdp
                     do begin
                        for i := 0 to 3
                        do begin
                           if (0 < sy + i)
                           then begin
                                if (sy + i < FSrcHeight)
                                then pS := pointer(FScanLines^[sy+i-1])
                           end
                           else pS := pointer(FScanLines^[sy]);

                           d01 := pS^[s0] - pS^[s1];
                           d21 := pS^[s2] - pS^[s1];
                           d31 := pS^[s3] - pS^[s1];

                           a := (d31 - d01) div 6 - d21 div 2;
                           b := (d01 + d21) div 2;
                           c := d21 - d31 div 6 - d01 div 3;
                           d := pS^[s1] shl 8;
                           v[i] := a * Fp3[dx] + b * Fp2[dx] + c * dx + d;
                        end;

                        d01 := v[0] - v[1];
                        d21 := v[2] - v[1];
                        d31 := v[3] - v[1];
                        a := (d31 - d01) div 6 - d21 div 2;
                        b := (d01 + d21) div 2;
                        c := d21 - d31 div 6 - d01 div 3;
                        d := v[1] shl 8;
                        // Delphi does strange things here : truncates to a smalling ??
                        a := smallint((a * Fp3[dy] + b * Fp2[dy] + c * dy + d) shr 16);
                        if (a < 0)
                        then pT^[x3] := 0
                        else if (a > 255)
                             then pT^[x3] := 255
                             else pT^[x3] := a;

                        inc(s0);
                        inc(s1);
                        inc(s2);
                        inc(s3);
                        inc(x3);
                     end;
                end;
           end;
           inc(x);
        end;
      end;
  end;
end; // TmcmImageTransform.Bicubic2.


procedure TmcmImageTransform.Hermite(const y : integer);
// Hermite Interpolation.
//
// |-----------|
// | p00 | p01 |
// |-----------|
// | p10 | p11 |
// |-----------|
//
// Pnm where m,n = [0,1] are pixels from SourceImage
// Pr point in ResultImage
//
//   f(x) = 2*x^3 - 3*x^2 + 1
//
//   v(j) := (2*dx^3 - 3*dx^2 + 1) * p(j0) + (2*(1-dx)^3 - 3*(1-dx)^2 + 1) * p(j1)
//   j = [0,1]
//   Pr := (2*dy^3 - 3*dy^2 + 1) * v(0) + (2*(1-dy)^3 - 3*(1-dy)^2 + 1) * v(1)
//
var x, x3   : integer;
    pS, pT  : PVectorB;
    pSu     : PVectorB;
    v1, v2  : integer;
    dx, dy  : integer;
    sy      : integer;
    s1, s2  : integer;
    a, b    : integer;
    c, d    : integer;
    i       : integer;
begin
  x := 0;
  while (x < FResImage.Width)
  do begin
     if (0 <= Fxy^[x].y) and (Fxy^[x].y < FSrcHeight)
     then if (0 <= Fxy^[x].x) and (Fxy^[x].x < FSrcWidth)
          then break;
     inc(x);
  end;

  case Fdp of
  1 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     pS := pointer(FScanLines^[sy]);
                     dx := Fdxdy^[x].x;
                     a := (2 * Fp3[dx] - 3 * Fp2[dx] + 256);
                     b := 256 - a;

                     if (s1 + 1 < FSrcWidth)
                     then v1 := pS^[s1] * a + pS^[s1+1] * b
                     else v1 := pS^[s1] shl 8;

                     inc(sy);
                     if (sy < FSrcHeight)
                     then begin
                          pS := pointer(FScanLines^[sy]);
                          if (s1 + 1 < FSrcWidth)
                          then v2 := pS^[s1] * a + pS^[s1+1] * b
                          else v2 := pS^[s1] shl 8;

                          dy := Fdxdy^[x].y;
                          a := (2 * Fp3[dy] - 3 * Fp2[dy] + 256);
                          b := 256 - a;
                          v1 := v1 * a + v2 * b;
                          pT^[x] := v1 shr 16;
                     end
                     else pT^[x] := v1 shr 8;
                end;
           end;
           inc(x);
        end;
      end;
  3,
  4 : begin
        pT := FResImage.ScanLine[y];
        while (x < FResImage.Width)
        do begin
           sy := Fxy^[x].y;
           if (0 <= sy) and (sy < FSrcHeight)
           then begin
                s1 := Fxy^[x].x;
                if (0 <= s1) and (s1 < FSrcWidth)
                then begin
                     s2 := s1 + 1;
                     if (s2 >= FSrcWidth)
                     then s2 := s1;
                     s1 := s1 * Fdp;
                     s2 := s2 * Fdp;
                     x3 := x * Fdp;

                     pS := pointer(FScanLines^[sy]);

                     dx := Fdxdy^[x].x;
                     a := (2 * Fp3[dx] - 3 * Fp2[dx] + 256);
                     b := 256 - a;

                     dy := Fdxdy^[x].y;
                     c := (2 * Fp3[dy] - 3 * Fp2[dy] + 256);
                     d := 256 - c;

                     inc(sy);
                     if (sy < FSrcHeight)
                     then pSu := pointer(FScanLines^[sy])
                     else pSu := pS;    

                     for i := 1 to Fdp
                     do begin
                        v1 := pS^[s1] * a + pS^[s2] * b;
                        v2 := pSu^[s1] * a + pSu^[s2] * b;
                        v1 := v1 * c + v2 * d;
                        inc(s1);
                        inc(s2);
                        pT^[x3] := v1 shr 16;
                        inc(x3);
                     end;
                end;
           end;
           inc(x);
        end;
      end;
  end;
end; // TmcmImageTransform.Hermite.


function TmcmImageTransform.Affine : TmcmImage;
var y          : integer;
    xr, yr     : double;
    xsc, ysc   : double;
    rw, rh     : integer;
    xMin, xMax : double;
    yMin, yMax : double;
    Ftm        : TmcmGeoMatrix;
    FmDet      : double;
    dx, dy     : double;
    fCos, fSin : double;
    bDelRes    : boolean;

    procedure CalcMinMax(x, y : integer);
    var xr, yr : double;
    begin
      xr := (x * Fm[0,0] + y * Fm[0,1] + Fm[0,2]);
      if (xMin > xr)
      then xMin := xr;
      if (xMax < xr)
      then xMax := xr;
      yr := (x * Fm[1,0] + y * Fm[1,1] + Fm[1,2]);
      if (yMin > yr)
      then yMin := yr;
      if (yMax < yr)
      then yMax := yr;
    end; // CalcMínMax.

begin
  FError := EC_OK;
  Result := Nil;
  // Make sure we have a valid Source and Result Image.
  if CheckSource(0, [IF_BW,IF_PAL8,IF_GREY8,IF_RGB24,IF_RGBA32])
  then begin
       if (FResImage = Nil)
       then begin
            bDelRes := True;
            FResImage := TKernelImage(TmcmImage.Create);
            FResImage.ImageFormat := FSrcImage[0].ImageFormat;
            FResImage.XResolution := FSrcImage[0].XResolution;
            FResImage.YResolution := FSrcImage[0].YResolution;
            FResImage.Palette     := FSrcImage[0].Palette;
            if FKeepResSize
            then begin
                 FResImage.Width  := FSrcImage[0].Width;
                 FResImage.Height := FSrcImage[0].Height;
            end;
       end
       else bDelRes := False;

       if Assigned(FResImage)
       then begin
            if (FResImage.ImageFormat <> FSrcImage[0].ImageFormat)
            then begin
                 FResImage.ImageFormat := FSrcImage[0].ImageFormat;
                 FResImage.Palette     := FSrcImage[0].Palette;
            end;
            
            if (FSrcImage[0].ImageFormat in [IF_BW,IF_PAL8])
            then FInterpolate := ST_NEAREST;

            case FInterpolate of
            ST_NEAREST     : FIMethod := NearestNeighbour;
            ST_BILINEAR    : FIMethod := Bilinear;
            ST_BIQUADRATIC : FIMethod := Biquadratic;
            ST_BICUBIC     : FIMethod := Bicubic;
            ST_HERMITE     : FIMethod := Hermite;
            {
            ST_BELL        : FIMethod := Bell;
            ST_SPLINE      : FIMethod := Spline;
            ST_LANCZOS     : FIMethod := Lanczos3;
            ST_MITCHELL    : FIMethod := Mitchell;
            }
            else begin
                 // Unknown interpolation method.
                 FError := EC_NOINTERPOLATE;
                 Exit;
            end;
            end;

            // Convert degree to radian.
            if FClockwise
            then FRadian := pi * (360.0 - FDegree) / 180.0
            else FRadian := pi * FDegree / 180.0;

            // Matrix Fm = Scale plus Translation
            Fm[0,0] := FxScale;
            if (FxShear <> 0.0)
            then Fm[0,1] := FxShear
            else Fm[0,1] := 0.0;
            Fm[0,2] := FxDist;
            if (FyShear <> 0.0)
            then Fm[1,0] := FyShear
            else Fm[1,0] := 0.0;
            Fm[1,1] := FyScale;
            Fm[1,2] := FyDist;

            // Forward: Multiply matrix [Scale & Shear] by [Rotation]
            //
            //   -         -     -        -     -                                -
            //  | a00   a01 |   | b00  b01 |   | a00*b00+a01*b10  a00*b01+a01*b11 |
            //  |           | · |          | = |                                  |
            //  | a10   a11 |   | b10  b11 |   | a10*b00+a11*b10  a10*b01+a11*b11 |
            //   -         -     -        -     -                                -
            //
            //      Scale   ·      Rotation       =              Final matrix
            //   -               -     -       -     -                                      -
            //  |  cos(a)  sin(a) |   | Sx    i |   |  cos(a)·Sx+sin(a)·j  cos(a)·i+sin(a)·Sy |
            //  |                 | · |         | = |                                         |
            //  | -sin(a)  cos(a) |   | j   Sy  |   | -sin(a)·Sx+cos(a)·j -sin(a)·i+cos(a)·Sy |
            //   -               -     -       -     -                                      -
            //

            if (FDegree <> 0)
            then begin
                 // Compensate for Delphi's calculation of Cos & sin.
                 // Degree at 90, 180 and 270 doesn't result in zero values!
                 fCos := cos(FRadian);
                 if (abs(fCos) < 0.000000001)
                 then fCos := 0.0;
                 fSin := sin(FRadian);
                 if (abs(fSin) < 0.000000001)
                 then fSin := 0.0;

                 // Forward matrix
                 Ftm     := Fm;
                 Fm[0,0] := fCos * Ftm[0,0] + fSin * Ftm[1,0];
                 Fm[0,1] := fCos * Ftm[0,1] + fSin * Ftm[1,1];
                 //Fm[0,2] := Ftm[0,2];

                 Fm[1,0] := -fSin * Ftm[0,0] + fCos * Ftm[1,0];
                 Fm[1,1] := -fSin * Ftm[0,1] + fCos * Ftm[1,1];
                 //Fm[1,2] := Ftm[1,2];

                 {
                 // Inverse matrix
                 Ftm      := Fim;
                 Fim[0,0] := Ftm[0,0] * cos(-FRadian) - Ftm[0,1] * sin(-FRadian);
                 Fim[0,1] := Ftm[0,0] * sin(-FRadian) + Ftm[0,1] * cos(-FRadian);
                 //Fim[0,2] := Ftm[0,2];

                 Fim[1,0] := Ftm[1,0] * cos(-FRadian) - Ftm[1,1] * sin(-FRadian);
                 Fim[1,1] := Ftm[1,0] * sin(-FRadian) + Ftm[1,1] * cos(-FRadian);
                 //Fim[1,2] := Ftm[1,2];
                 }
            end;

            // Calculate Fim - inverse matrix of Fm.
            FmDet := Fm[0,0] * Fm[1,1] - Fm[0,1] * Fm[1,0];
            if (FmDet = 0.0)
            then FmDet := 10000.0 // <- approx. as FmDet goes towards zero!
            else FmDet := 1.0 / FmDet;
            Fim[0,0] := Fm[1,1] * FmDet;
            Fim[0,1] := -Fm[0,1] * FmDet;
            Fim[1,0] := -Fm[1,0] * FmDet;
            Fim[1,1] := Fm[0,0] * FmDet;

            // Add Translation to inverse matrix.
            Fim[0,2] := -FxDist * Fim[0,0];
            Fim[1,2] := -FyDist * Fim[1,1];

            // Determin Min and Max (x,y) in ResultImage, i.e. the size.
            xMin := 65536.0;
            yMin := 65536.0;
            xMax := -65536.0;
            yMax := -65536.0;
            CalcMinMax(0, 0);
            CalcMinMax(0, FSrcHeight);
            CalcMinMax(FSrcWidth, 0);
            CalcMinMax(FSrcWidth, FSrcHeight);

            // rw,rh is the max width and height of the transformation.
            rw := Round(xMax - xMin);
            rh := Round(yMax - yMin);

            // Check that the rw,rh dimension does not exceed max allowed size.
            if (rw > 65535)
            then rw := 65535;
            if (rh > 65535)
            then rh := 65535;

            if FKeepResSize
            then begin
                 // We'll keep ResultImage's dimension
                 rw := FResImage.Width;
                 rh := FResImage.Height;
            end
            else begin
                 // ResultImage is sized to fit the entire transformed image.
                 if (FResImage.Width <> rw)
                 then FResImage.Width := rw;
                 if (FResImage.Height <> rh)
                 then FResImage.Height := rh;
            end;

            // Centre rotated image.
            if (FDegree <> 0.0)
            then begin
                 // xr,yr - centre point in ResultImage.
                 xr := (rw - 1.0) / 2.0;
                 yr := (rh - 1.0) / 2.0;
                 xsc := (FSrcWidth - 1.0) / 2.0;
                 ysc := (FSrcHeight - 1.0) / 2.0;

                 // ADJUSTMENT for integer round-off on real calculus.
                 //
                 Fim[0,2] := Fim[0,2] + (xsc) - (xr * Fim[0,0] + yr * Fim[0,1]);
                 Fim[1,2] := Fim[1,2] + (ysc) - (xr * Fim[1,0] + yr * Fim[1,1]);

                 if odd(FSrcWidth)
                 then dx := int(xr * Fim[0,0] + yr * Fim[0,1] + Fim[0,2] + 0.00001) + 0.50
                 else dx := int(xr * Fim[0,0] + yr * Fim[0,1] + Fim[0,2] + 0.50001);

                 if odd(FSrcHeight)
                 then dy := int(xr * Fim[1,0] + yr * Fim[1,1] + Fim[1,2] + 0.00001) + 0.50
                 else dy := int(xr * Fim[1,0] + yr * Fim[1,1] + Fim[1,2] + 0.50001);

                 Fim[0,2] := dx - (xr * Fim[0,0] + yr * Fim[0,1]);
                 Fim[1,2] := dy - (xr * Fim[1,0] + yr * Fim[1,1]);
            end;

            if (FResImage.Empty)
            then begin
                 FError := EC_MISRESULTIMAGE;
                 Exit;
            end;

            // Should we fill the background with BKcolor.
            if (FDegree <> 0.0) or
               (FxShear <> 0.0) or
               (FyShear <> 0.0) or
               (FxDist  <> 0.0) or
               (FyDist  <> 0.0)
            then begin
                 // Only fill background if this will be visible in the
                 // resulting image.
                 if ((FBKColor and $FF) <> 0) or
                    ((FBKColor and $FF00) <> 0) or
                    ((FBKColor and $FF0000) <> 0)
                 then FResImage.FillRGB(FBKColor);
            end;

            try
              try
                InitAffine;
                case FSrcImage[0].BitCount of
                1,
                8  : for y := 0 to (FResImage.Height - 1)
                     do begin
                        CalcAffineLine(y);
                        FIMethod(y);
                     end;
                24 : for y := 0 to (FResImage.Height - 1)
                     do begin
                        CalcAffineLine(y);
                        FIMethod(y);
                     end;
                32 : for y := 0 to (FResImage.Height - 1)
                     do begin
                        CalcAffineLine(y);
                        FIMethod(y);
                     end;
                else FError := EC_NOMATCHFORMAT;
                end;
                Result := FResImage;
              except
                On E:Exception
                do FError := EC_UNKNOWN;
              end;
            finally
              DoneAffine;
            end;

            if (FError <> EC_OK) and bDelRes
            then begin
                 FResImage.Free;
                 FResImage := Nil;
            end;
       end
       else FError := EC_MISRESULTIMAGE; // No result
  end;
end; // TmcmImageTransform.Affine.


//------------------------------------------------------------------------------
// Deskew
//------------------------------------------------------------------------------

procedure TmcmImageTransform.SetDeskewRange(Value : integer);
begin
  if (Value < 2.0 * FDeskewStep)
  then Value := Round(2 * FDeskewStep);
  if (Value > 180)
  then Value := 180;
  if (Value < 1)
  then Value := 1;
  if (FDeskewRange <> Value)
  then FDeskewRange := Value;
end; // TmcmImageTransform.SetDeskewRange.


procedure TmcmImageTransform.SetDeskewStep(Value : double);
begin
  if (Value < 0.01)
  then Value := 0.01;
  if (Value > FDeskewRange / 2.0)
  then Value := FDeskewRange / 2.0;
  if (FDeskewStep <> Value)
  then FDeskewStep := Value;
end; // TmcmImageTransform.SetDeskewStep.


procedure TmcmImageTransform.SetDeskewMaxInt(Value : word);
begin
  if (Value > 254)
  then Value := 254;
  if (FDeskewMaxInt <> Value)
  then FDeskewMaxInt := Value;
end; // TmcmImageTransform.SetDeskewMaxInt.


{$IFNDEF REQBLAIRD}

procedure TmcmImageTransform.BairdMark(AImage : TmcmImage; x, y : integer);
const LPixON  : byte = 0;
      LPixOFF : byte = 255;

     function GetPrevious(sx, sy : integer; Up : boolean) : byte;
     var a          : integer;
         Apt, Bpt   : PVectorB;
     begin
       // Get current line source address.
       Apt := AImage.ScanLine[sy];

       // Search for features above (Up=True) or below current line.
       if Up
       then Bpt := AImage.ScanLine[sy+1]
       else Bpt := AImage.ScanLine[sy-1];

       a := sx;
       while (a < FSrcWidth) and
             (Apt^[a] = LPixON) and
             ((Bpt^[a] = LPixON) or (Bpt^[a] = LPixOFF))
       do inc(a);

       if (Bpt^[a] = LPixON) or (Bpt^[a] = LPixOFF)
       then Result := LPixOFF
       else Result := Bpt^[a];
     end; // GetPrevious.


     function HasNeighbour(sx, sy, ex, ey : integer; Up : boolean; Value : byte) : byte;
     var a, am, ap  : integer;
         maxx       : integer;
         Apt, Bpt   : PVectorB;
     begin
       // Calculate Current Line source address.
       Apt := AImage.ScanLine[sy];

       // Search for neighbours above (Up=True) or below current line.
       if Up
       then Bpt := AImage.ScanLine[sy+1]
       else Bpt := AImage.ScanLine[sy-1];

       // Start search one pixel "early".
       a := sx - 1;
       if (a < 0)
       then a := 0;

       am := a - 1;
       if (am < 0)
       then am := 0;

       ap := a + 1;
       if (ap >= FSrcWidth)
       then ap := FSrcWidth - 1;

       maxx := ex + 1;
       if (maxx > FSrcWidth)
       then maxx := FSrcWidth;

       // Search for neighbour pixel being "ON" next to a pixel having feature
       // "Value".
       while (a < maxx{AImage.Width}) and
             Not(((Apt^[a] = Value) or (Apt^[am] = Value) or (Apt^[ap] = Value)) and
                 (Bpt^[a] = LPixON))
       do begin
          inc(a);
          am := a - 1;
          inc(ap);
          if (ap >= FSrcWidth)
          then ap := FSrcWidth - 1;
       end;

       // Return pixel value.
       if (a < FSrcWidth)
       then Result := Bpt^[a]
       else Result := LPixOFF;
     end; // HasNeighbour.


     function FirstLine(var sx, sy, ex, ey : integer) : longint;
     var a          : integer;
         Count      : longint;
         ALine      : PvectorB;
     begin
       Count := 0;

       // Calculate Current Line source address.
       ALine := AImage.ScanLine[sy];

       // Fill coherent "ON"-pixels with feature "Value".
       a := sx;
       while (0 <= a) and (a < FSrcWidth) and (ALine^[a] = LPixON)
       do begin
          ALine^[a] := BairdMarker;
          inc(a);
          inc(Count);
       end;
       dec(a);
       if (a < 0)
       then a := 0;
       ex := a;
       Result := Count;
     end; // FirstLine.


     function FillLine(var sx, sy, ex, ey : integer; Up : boolean) : longint;
     var a, b, c, d : integer;
         bm, bp     : integer;
         Count      : longint;
         ALine      : PvectorB;
         BLine      : PvectorB;
     begin
       Count := 0;

       dec(sx);
       if (sx < 0)
       then sx := 0;
       inc(ex);
       if (ex > (FSrcWidth - 1))
       then ex := FSrcWidth - 1;

       // Get current line source address.
       ALine := AImage.ScanLine[sy];

       if Up
       then BLine := AImage.ScanLine[sy-1]
       else BLine := AImage.ScanLine[sy+1];

       a := sx;
       d := sx;
       while (a <= ex) and (ALine^[a] = LPixOFF)
       do inc(a);

       if (a <= ex)
       then begin
            b  := a;
            bm := b - 1;
            if (bm < 0)
            then bm := 0;

            bp := b + 1;
            if (bp >= FSrcWidth)
            then bp := FSrcWidth - 1;

            while (b <= ex)
            do begin
               if (ALine^[b] = LPixON)
               then begin
                    if (BLine^[b]  = BairdMarker) or
                       (BLine^[bm] = BairdMarker) or
                       (BLine^[bp] = BairdMarker)
                    then begin
                         c := b;
                         while (c >= 0) and (ALine^[c] = LPixON)
                         do begin
                            ALine^[c] := BairdMarker;
                            dec(c);
                            inc(Count);
                         end;
                         if (a > c)
                         then a := c + 1;
                         if (d <= c)
                         then d := c + 1;

                         inc(b);
                         while (b < FSrcWidth) and (ALine^[b] = LPixON)
                         do begin
                            ALine^[b] := BairdMarker;
                            d := b;
                            inc(b);
                            inc(Count);
                         end;
                    end
                    else inc(b);
               end
               else inc(b);
               bm := b - 1;
               bp := b + 1;
               if (bp >= FSrcWidth)
               then bp := FSrcWidth - 1;
            end;
       end;

       if (Count = 0)
       then begin
            a := ex;
            d := 0;
       end;

       if (a < 0)
       then a := 0;
       sx := a;
       ex := d;
       ey := sy;
       Result := Count;
     end; // FillLine.


var NIndex     : longint;
    ix, iy     : integer;
    ox, oy     : integer;
    Dir        : boolean;
begin
  NIndex := 0;

  ix := x;
  iy := y;
  ox := FSrcWidth;
  oy := y;

  // Feature fill current line segment.
  FirstLine(ix, iy, ox, oy);

  // Save x and/or y position if it marks a new boundary of the feature.
  if (FBlx > ox)
  then FBlx := ox;
  if (FBby < iy)
  then FBby := iy;
  if (FBrx < ox)
  then FBrx := ox;
  if (FBty > iy)
  then FBty := iy;

  inc(iy);

  Dir := True;
  repeat
    // If at end of feature (top or bottom) check for saved
    // lines to search for in opersite direction.
    if Not(ix <= ox) or (0 > iy) or (iy >= FSrcHeight)
    then begin
         inc(iy);
         if (NIndex > 0)
         then begin
              dec(NIndex);
              ix := FNeighbour[NIndex].x1;
              iy := FNeighbour[NIndex].y1;
              ox := FNeighbour[NIndex].x2;
              oy := FNeighbour[NIndex].y2;
              Dir := FNeighbour[NIndex].Up;
         end;
    end;

    while (ix <= ox) and (0 <= iy) and (iy < FSrcHeight)
    do begin
       // Fill line segments belonging to current feature.
       if (FillLine(ix, iy, ox, oy, Dir) = 0)
       then begin
            // Save x and/or y position if it marks a new boundary of the feature.
            if (FBlx > ix)
            then FBlx := ix;
            if (FBby < iy)
            then FBby := iy;
            if (FBrx < ox)
            then FBrx := ox;
            if (FBty > iy)
            then FBty := iy;

            // If at end of feature (top or bottom) check for saved
            // lines to search for in opersite direction.
            inc(iy);
            if (NIndex > 0)
            then begin
                 dec(NIndex);
                 ix := FNeighbour[NIndex].x1;
                 iy := FNeighbour[NIndex].y1;
                 ox := FNeighbour[NIndex].x2;
                 oy := FNeighbour[NIndex].y2;
                 Dir := FNeighbour[NIndex].Up;
            end;
       end
       else begin
            // Save x and/or y position if it marks a new boundary of the feature.
            if (FBlx > ix)
            then FBlx := ix;
            if (FBby < iy)
            then FBby := iy;
            if (FBrx < ox)
            then FBrx := ox;
            if (FBty > iy)
            then FBty := iy;

            // Check if line-segments in the line in opersite
            // direction (the line above or below) belongs to
            // the current feature (pixels = PixON).
            if (HasNeighbour(ix, iy, ox, oy, Not(Dir), BairdMarker) = LPixON)
            then begin
                 // Increase allocated memory if required.
                 if (NIndex >= FNeighbourSize)
                 then begin
                      ReallocMem(FNeighbour, (FNeighbourSize + FSrcHeight) * SizeOf(TNeighbour));
                      if (FNeighbour <> Nil)
                      then FNeighbourSize := FNeighbourSize + FSrcHeight
                      else FNeighbourSize := 0;
                 end;

                 // Save left- and rightmost x coordinate for
                 // "later" filling.
                 if (NIndex < FNeighbourSize)
                 then begin
                      FNeighbour[NIndex].x1 := ix;
                      FNeighbour[NIndex].x2 := ox;
                      if Dir
                      then begin
                           FNeighbour[NIndex].y1 := iy - 1;
                           FNeighbour[NIndex].y2 := iy - 1;
                           inc(iy);
                      end
                      else begin
                           FNeighbour[NIndex].y1 := iy + 1;
                           FNeighbour[NIndex].y2 := iy + 1;
                           dec(iy);
                      end;
                      FNeighbour[NIndex].Up := Not(Dir);

                      if (0 <= FNeighbour[NIndex].y1) and (FNeighbour[NIndex].y1 < FSrcHeight)
                      then Inc(NIndex);
                 end;
            end
            else begin
                 if Dir
                 then inc(iy)
                 else dec(iy);
            end;
       end;
    end;
  until (NIndex <= 0);
end; // TmcmImageTransform.BairdMark.

{$ELSE}

procedure TmcmImageTransform.BairdMark(AImage : TmcmImage; x, y : integer);
var pB : PVectorB;
begin
  // BairdMark searchs for neighbour (in 8-connection) pixels, and markes these.
  // Problem with this routine: Can cause stack overrun when features are too
  // large.

  // Mark current (x, y) pixel.
  pB := AImage.ScanLine[y];
  pB^[x] := BairdMarker;

  // Get line above.
  pB := AImage.ScanLine[y-1];
  if (y - 1 >= 0) and (x - 1 >= 0) and (pB^[x-1] = 0)
  then BairdMark(AImage, x - 1, y - 1);
  if (y - 1 >= 0) and (pB^[x] = 0)
  then BairdMark(AImage, x, y - 1);
  if (y - 1 >= 0) and (x + 1 < AImage.Width) and (pB^[x+1] = 0)
  then BairdMark(AImage, x + 1, y - 1);

  // Get current line.
  pB := AImage.ScanLine[y];
  if (x - 1 >= 0) and (pB^[x-1] = 0)
  then BairdMark(AImage, x - 1, y);
  if (x + 1 < AImage.Width) and (pB^[x+1] = 0)
  then BairdMark(AImage, x + 1, y);

  // Get Line below.
  pB := AImage.ScanLine[y+1];
  if (y + 1 < AImage.Height) and (x - 1 >= 0) and (pB^[x-1] = 0)
  then BairdMark(AImage, x - 1, y + 1);
  if (y + 1 < AImage.Height) and (pB^[x] = 0)
  then BairdMark(AImage, x, y + 1);
  if (y + 1 < AImage.Height) and (x + 1 < AImage.Width) and (pB^[x+1] = 0)
  then BairdMark(AImage, x + 1, y + 1);

  // Save x and/or y position if it marks a new boundary of the feature.
  if (FBlx > x)
  then FBlx := x;
  if (FBby < y)
  then FBby := y;
  if (FBrx < x)
  then FBrx := x;
  if (FBty > y)
  then FBty := y;
end; // TmcmImageTransform.BairdMark.

{$ENDIF}

procedure TmcmImageTransform.Baird(BairdImage, DotImage : TmcmImage);
var x, y   : integer;
    pB, pD : PVectorB;
begin
  // Replace each feature/character with a "dot" at the features bottom line and
  // at it's horizontal centre.
  // This preprocess reduces the time-cost of calculating the hough
  // transformation, as fewer pixels need transformed.

  // FNeighbour is a stack of coordinates used to link parts of a feature.
  FNeighbourSize := 4 * FSrcHeight;
  GetMem(FNeighbour, FNeighbourSize * SizeOf(TNeighbour));
  try
    for y := 0 to (FSrcHeight - 1)
    do begin
       pB := BairdImage.ScanLine[y];
       for x := 0 to (FSrcWidth - 1)
       do begin
          if (pB^[x] = 0)
          then begin
               FBlx := x;
               FBby := y;
               FBrx := x;
               FBty := y;
               BairdMark(BairdImage, x, y);
               pD := DotImage.ScanLine[FBby];
               pD^[(FBrx + FBlx) shr 1] := 0;
          end;
       end;
    end;
  finally
    if (FNeighbour <> Nil)
    then FreeMem(FNeighbour);
  end;
end; // TmcmImageTransform.Baird.



function TmcmImageTransform.CreateBairdImage : TmcmImage;
var BairdImage  : TmcmImage;
    ImageColor  : TmcmImageColor;
    SourceImage : TmcmImage;
begin
  FError := EC_OK;
  BairdImage := Nil;

  // Make sure we have a valid Source and Result Image.
  if CheckSource(0, [IF_BW,IF_PAL8,IF_GREY8,IF_RGB24,IF_RGBA32])
  then begin
       // Create a Baird image.
       BairdImage := TmcmImage.Create;
       BairdImage.Width := FSrcImage[0].Width;
       BairdImage.Height := FSrcImage[0].Height;
       BairdImage.ImageFormat := IF_GREY8;
       BairdImage.CreateGreyPalette;
       BairdImage.FillAll(255); // Fill image with background color.

       // Create copy of FSrcImage[0] as the Baird process will modify the
       // the content.
       // Further we need the image to be an 8 bit grey thresholded image.
       ImageColor := TmcmImageColor.Create(Nil);
       ImageColor.SourceImage[0] := FSrcImage[0];
       SourceImage := TmcmImage.Create;
       SourceImage.Width := FSrcImage[0].Width;
       SourceImage.Height := FSrcImage[0].Height;
       SourceImage.ImageFormat := IF_GREY8;
       SourceImage.CreateGreyPalette;
       ImageColor.ResultImage := SourceImage;

       case FSrcImage[0].ImageFormat of
       IF_BW     : ImageColor.GetIntensity;
       IF_GREY8  : begin
                     ImageColor.Threshold(TH_LEVEL, FDeskewMaxInt, 0, 0);
                   end;
       IF_PAL8,
       IF_RGB24,
       IF_RGBA32 : begin
                     ImageColor.GetLuminanceChannel;
                     ImageColor.SourceImage[0] := ImageColor.ResultImage;
                     ImageColor.Threshold(TH_LEVEL, FDeskewMaxInt, 0, 0);
                   end;
       end;
       FError := ImageColor.Error;
       ImageColor.Free;

       // Process image - get "one" dot per character at approxiamately their
       // lower-right position.
       Baird(SourceImage, BairdImage);
       SourceImage.Free;
  end;
  Result := BairdImage;
end; // TmcmImageTransform.CreateBairdImage.


function TmcmImageTransform.Deskew : TmcmImage;
var x, y, z, i   : integer;
    j, dj        : integer;
    OmegaSteps   : integer;
    dOmega       : double;
    Omega        : integer;
    cx, cy       : integer;
    dx, dy       : integer;
    SourceImage  : TmcmImage;
    BairdImage   : TmcmImage;
    HoughImage   : TmcmImage;
    HoughMatrix  : PMatrixI;
    pi180        : double;
    r, rmax      : double;
    tmax         : double;
    tmval        : integer;
    pS           : PVectorB;
    pH           : PVectorI;
    CosVec       : PVectorD;
    SinVec       : PVectorD;
    Lum          : word;
    // BW helper variabled.
    WColor       : word; // White color
    BColor       : word; // Black color
begin
  FError := EC_OK;
  HoughImage := Nil;

  // Make sure we have a valid Source and Result Image.
  if CheckSource(0, [IF_BW,IF_PAL8,IF_GREY8,IF_RGB24,IF_RGBA32])
  then begin
       BairdImage := Nil;
       if FUseBaird
       then begin
            BairdImage  := CreateBairdImage;
            SourceImage := BairdImage;
       end
       else SourceImage := FSrcImage[0];

       // Calculate Omega (degree) offset and number of steps.
       dOmega := (180.0 - FDeskewRange) / 2.0;
       dOmega := dOmega - (dOmega - Trunc(dOmega));
       OmegaSteps := Round(FDeskewRange / FDeskewStep);

       if (OmegaSteps < 2)
       then FError := EC_BADPARAMETER;

       // Allocate memory for cos & sin look-up tables.
       GetMem(HoughMatrix, OmegaSteps * SizeOf(Pointer));
       GetMem(CosVec, OmegaSteps * SizeOf(double));
       GetMem(SinVec, OmegaSteps * SizeOf(double));

       try
         if (FError = EC_OK)
         then begin
              // Calculate the cos and sin's of the angles.
              pi180 := pi / 180.0;
              for Omega := 0 to (OmegaSteps - 1)
              do begin
                 SinVec^[Omega] := sin(pi180 * (Omega * FDeskewStep + dOmega));
                 CosVec^[Omega] := cos(pi180 * (Omega * FDeskewStep + dOmega));
              end;

              // Calculate centre x and y.
              cx := FSrcImage[0].Width div 2;
              cy := FSrcImage[0].Height div 2;

              // Calculate half width.
              rmax := Sqrt(FSrcImage[0].Width * FSrcImage[0].Width +
                           FSrcImage[0].Height * FSrcImage[0].Height) / 2.0;

              // Create an image for the Hough space.
              HoughImage := TmcmImage.Create;
              HoughImage.Width  := Round(2.0 * rmax + 1.0);
              HoughImage.Height := OmegaSteps;
              // A 32 bit image should be sufficient in depth.
              HoughImage.ImageFormat := IF_RGBA32;

              // Map lines in image.
              for Omega := 0 to (OmegaSteps - 1)
              do HoughMatrix^[Omega] := HoughImage.ScanLine[Omega];

              case SourceImage.ImageFormat of
              IF_BW     : begin
                            // Get black and white colors.
                            if (FSrcImage[0].DibInfo.bmiColors[0].rgbBlue = 0)
                            then begin
                                 BColor  := 0;
                                 WColor  := 255;
                            end
                            else begin
                                 BColor  := 255;
                                 WColor  := 0;
                            end;

                            for y := 0 to (FSrcImage[0].Height - 1)
                            do begin
                               dy := (y - cy);
                               pS := SourceImage.ScanLine[y];

                               j := 0;
                               z := 7;
                               for x := 0 to (FSrcImage[0].Width - 1)
                               do begin
                                  if ((pS[j] and BitMask[z]) <> 0)
                                  then Lum := WColor
                                  else Lum := BColor;

                                  // Calculate bit shift and line index.
                                  dec(z);
                                  if (z < 0)
                                  then begin
                                       z := 7;
                                       inc(j);
                                  end;

                                  if (Lum <= FDeskewMaxInt)
                                  then begin
                                       dx := (x - cx);
                                       // Omega is indirectly the angle.
                                       for Omega := 0 to (OmegaSteps - 1)
                                       do begin
                                          // Radius r = x cos(w) + y sin(w)
                                          r := dy * SinVec^[Omega] + dx * CosVec^[Omega];
                                          i := Round(r + rmax);
                                          HoughMatrix^[Omega]^[i] := HoughMatrix^[Omega]^[i] + 1;
                                       end;
                                  end;
                               end;
                            end;
                          end;
              IF_PAL8   : for y := 0 to (FSrcImage[0].Height - 1)
                          do begin
                             dy := (y - cy);
                             pS := FSrcImage[0].ScanLine[y];
                             for x := 0 to (FSrcImage[0].Width - 1)
                             do begin
                                // Lum = Round(0.2989 * Red + 0.5867 * Green + 0.1144 * Blue)
                                with FSrcImage[0].DibInfo^
                                do Lum := Round(0.2989 * bmiColors[pS^[x]].rgbRed +
                                                0.5867 * bmiColors[pS^[x]].rgbGreen +
                                                0.1144 * bmiColors[pS^[x]].rgbBlue);
                                if (Lum <= FDeskewMaxInt)
                                then begin
                                     dx := (x - cx);
                                     for Omega := 0 to (OmegaSteps - 1)
                                     do begin
                                        r := dy * SinVec^[Omega] + dx * CosVec^[Omega];
                                        i := Round(r + rmax);
                                        HoughMatrix^[Omega]^[i] := HoughMatrix^[Omega]^[i] + 1;
                                     end;
                                end;
                             end;
                          end;
              IF_GREY8  : // Note: the first and last Row and Column in the source image is
                          //       not used (when a Baird image is created these typically
                          //       contains misleading information).
                          for y := 1 to (FSrcImage[0].Height - 2)
                          do begin
                             dy := (y - cy);
                             pS := SourceImage.ScanLine[y];
                             for x := 1 to (FSrcImage[0].Width - 2)
                             do begin
                                if (pS^[x] <= FDeskewMaxInt)
                                then begin
                                     dx := (x - cx);
                                     // Omega is indirectly the angle.
                                     for Omega := 0 to (OmegaSteps - 1)
                                     do begin
                                        // Radius r = x cos(w) + y sin(w)
                                        r := dy * SinVec^[Omega] + dx * CosVec^[Omega];
                                        i := Round(r + rmax);
                                        HoughMatrix^[Omega]^[i] := HoughMatrix^[Omega]^[i] + 1;
                                     end;
                                end;
                             end;
                          end;
              IF_RGB24,
              IF_RGBA32 : begin
                            if (FSrcImage[0].ImageFormat = IF_RGB24)
                            then dj := 3
                            else dj := 4;
                            for y := 0 to (FSrcImage[0].Height - 1)
                            do begin
                               dy := (y - cy);
                               pS := FSrcImage[0].ScanLine[y];
                               for x := 0 to (FSrcImage[0].Width - 1)
                               do begin
                                  j := x * dj;
                                  // Luminance = Round(0.2989 * Red + 0.5867 * Green + 0.1144 * Blue)
                                  Lum := Round(0.2989 * pS^[j+2] + 0.5867 * pS^[j+1] + 0.1144 * pS^[j]);
                                  if (Lum <= FDeskewMaxInt)
                                  then begin
                                       dx := (x - cx);
                                       for Omega := 0 to (OmegaSteps - 1)
                                       do begin
                                          r := dy * SinVec^[Omega] + dx * CosVec^[Omega];
                                          i := Round(r + rmax);
                                          HoughMatrix^[Omega]^[i] := HoughMatrix^[Omega]^[i] + 1;
                                       end;
                                  end;
                               end;
                            end;
                          end;
              end;

              // Now, find the deskew angle - this correspond to the line index at
              // which the max value in Hough image is found.
              tmax := 90; // Correspond to no rotation.
              tmval := 0;
              for Omega := 0 to (HoughImage.Height - 1)
              do begin
                 pH := HoughImage.ScanLine[Omega];
                 for i := 0 to (HoughImage.Width - 1)
                 do begin
                    if (tmval < pH^[i])
                    then begin
                         if (tmax + 1 = Omega) and (abs(tmval - pH^[i]) < 2)
                         then begin
                              // Handles situation where the deskew angle falls
                              // in between two consecutive lines.
                              // Increases resolution.
                              tmval := pH^[i];
                              tmax  := (Omega + tmax) / 2.0;
                         end
                         else begin
                              tmval := pH^[i];
                              tmax  := Omega;
                         end;
                    end;
                 end;
              end;

              if (tmax <= 2) or (tmax >= (HoughImage.Height - 2))
              then FError := EC_DESKEWMAXMIN; // Deskew angle may be incorrect.

              // Set-up parameters to Rotate the image.
              FDegree      := (tmax * FDeskewStep + dOmega) - 90.0; // Rotation
              FClockwise   := False;
              FxDist       := 0.0; // Translation
              FyDist       := 0.0;
              FxScale      := 1.0; // Scale/Stretch
              FyScale      := 1.0;
              FxShear      := 0.0; // Shearing
              FyShear      := 0.0;
         end;
       finally
         // Deallocate memory.
         FreeMem(HoughMatrix);
         FreeMem(CosVec);
         FreeMem(SinVec);
         if Assigned(BairdImage)
         then BairdImage.Free;
         if Not(FReturnHough)
         then begin
              HoughImage.Free;
              HoughImage := Nil;
         end;
       end;
  end;

  Result := HoughImage;
end; // TmcmImageTransform.Deskew.


{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.


