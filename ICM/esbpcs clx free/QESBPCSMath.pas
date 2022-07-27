{: Contains the Basic Math Routines used by ESBPCS for CLX.

 This is designed to work in Borland Delphi 6 CLX and above, Borland
 C++ Builder 6 CLX and above, and Borland Kylix 2 and above.
 Most if not all features will work in Kylix 1 but it is not currently supported.<p>

 Supplies various optimised and flexible Math Routines, including
 support for Special Functions. Use of BASM to get better FPU
 performance is done in a number of routines. We have also tried
 to minimise the amount of division and math function calls as the
 FPU is not as good at these as it is Addition, Multiplication etc.<p>

 No dependency on the Delphi Math unit.<p>

 Copyright © 1999-2002 ESB Consultancy<p>

 v2.3 - 14 September 2002
}
unit QESBPCSMath;

{$I esbpcs.inc}

interface

uses
     QESBPCSGlobals;

{--- 80x87 Handling ---}

type
     {: Different ways in which the Maths Processor will handle Rounding.
      @enum ertBankers This is the default. 5 is rounded to the nearest
       even. When ever the Maths Processor is Initialised it returns
       to this state.
      @enum ertDown 5 is always rounded down.
      @enum ertUp 5 us always rounded up.
      @enum ertChop - Rounding always Truncates. }
     TESBRoundingType = (ertBankers, ertDown, ertUp, ertChop);

     {: Returns the 80x87 Control Word.
       15-12 Reserved  <p>
       On 8087/80287 12 was Infinity Control <p>
       0 Projective <p>
       1 Affine <p>
      11-10 Rounding Control <p>
       00 Round to nearest even <p>
       01 Round Down <p>
       10 Round Up <p>
       11 Chop - Truncate towards Zero <p>
      9-8  Precision Control <p>
       00 24 bits Single Precision <p>
       01 Reserved <p>
      10 53 bits Double Precision <p>
      11 64 bits Extended Precision (Default) <p>
      7-6  Reserved <p>
       On 8087 7 was Interrupt Enable Mask <p>
      5  Precesion Exception Mask <p>
      4  Underflow Exception Mask <p>
      3  Overflow Exception Mask <p>
      2  Zero Divide Exception Mask <p>
      1  Denormalised Operand Exception Mask <p>
      0  Invalid Operation Exception Mask <p>
     }
function Get87ControlWord: TESBBitList;

{: Sets the 80x87 Control Word.
  15-12 Reserved  <p>
  On 8087/80287 12 was Infinity Control <p>
  0 Projective <p>
  1 Affine <p>
 11-10 Rounding Control <p>
  00 Round to nearest even <p>
  01 Round Down <p>
  10 Round Up <p>
  11 Chop - Truncate towards Zero <p>
 9-8  Precision Control <p>
  00 24 bits Single Precision <p>
  01 Reserved <p>
 10 53 bits Double Precision <p>
 11 64 bits Extended Precision (Default) <p>
 7-6  Reserved <p>
  On 8087 7 was Interrupt Enable Mask <p>
 5  Precesion Exception Mask <p>
 4  Underflow Exception Mask <p>
 3  Overflow Exception Mask <p>
 2  Zero Divide Exception Mask <p>
 1  Denormalised Operand Exception Mask <p>
 0  Invalid Operation Exception Mask <p>
}
procedure Set87ControlWord (const ControlWord: TESBBitList);

{: Get the Current Rounding Type.
 ertBankers - This is the default. 5 is rounded to the nearest even. When
  ever the Maths Processor is Initialised it returns to this state.<p>
 ertDown - 5 is always rounded down.<p>
 ertUp - 5 us always rounded up.<p>
 ertChop - Rounding always Truncates.
 @cat FloatMath
}
function GetRoundingType: TESBRoundingType;

{: Allows you to change the Current Rounding Type.
 @param RT Desired Rounding Type.<p>
 ertBankers - This is the default. 5 is rounded to the nearest even. When
  ever the Maths Processor is Initialised it returns to this state.<p>
 ertDown - 5 is always rounded down.<p>
 ertUp - 5 us always rounded up.<p>
 ertChop - Rounding always Truncates.
 @cat FloatMath
}
procedure SetRoundingType (const RT: TESBRoundingType);

{: Returns X rounded to the given number of Decimal Places. When DecimalPlaces
 is negative then Integral places are implied, ie -2 would round to the nearest
 hundred. DecimalPlaces = 0 implies rounding to nearest Integer Value. Designed
     to work similar to the Round Function in MS Excel.
 @param X Value to process.
     @param DecimalPlaces Number of Decimal Places to round to.
 @cat FloatMath
}
function RoundDP (const X: Extended; const DecimalPlaces: ShortInt): Extended;

{: Returns an Extended as its Mantissa and Exponent base 10.
 For Values with large negative exponents, <See Var=ESBTolerance> will need to be
 changed to a smaller value.
 @param X Float to process
 @param Mantissa Resultant Mantissa in the form x.yyyyyyy.
 @param Exponent Resultant Exponent of the float, ie power of 10.
 @cat FloatMath
}
procedure ExtractParts (const X: Extended; out Mantissa: Extended;
     out Exponent: Integer);

{: Returns an Extended constructed from a Mantissa and Exponent base 10.
 For Values with large negative exponents, <See Var=ESBTolerance> will need to be
 changed to a smaller value.
 @param Mantissa Resultant Mantissa in the form x.yyyyyyy.
 @param Exponent Resultant Exponent of the float, ie power of 10.
 @returns x.yyyyyy * 10^Exponent.
 @cat FloatMath
}
function BuildFromParts (const Mantissa: Extended;
     const Exponent: Integer): Extended;

{--- Comparisons ---}

{: Returns True if X1 and X2 are within a "small" value of each other.
 For Values with lots of significant figures, <See Var=ESBPrecision> may need to be
 changed to a smaller value - <See Var=ESBDoublePrecision> for Doubles and
 <See Var=ESBSinglePrecision> for Singles.
 @param X1 First Float to process.
 @param X2 Second Float to process.
 @returns abs (X1 - X2) < ESBTolerance.
 @cat NumComparison
}
function SameFloat (const X1, X2: Extended): Boolean; overload;
function SameFloat (const X1, X2: Double): Boolean; overload;
function SameFloat (const X1, X2: Single): Boolean; overload;

{: Returns -1 if X1 < X2, 0 if they are the same, 1 if X1 > X2.
 For Values with lots of significant figures, <See Var=ESBPrecision> may need to be
 changed to a smaller value - <See Var=ESBDoublePrecision> for Doubles and
 <See Var=ESBSinglePrecision> for Singles.
 @param X1 First Float to process.
 @param X2 Second Float to process.
 @cat NumComparison
}
function CompareFloat (const X1, X2: Extended): Integer; overload;
function CompareFloat (const X1, X2: Double): Integer; overload;
function CompareFloat (const X1, X2: Single): Integer; overload;

{: Returns True if X1 > X2.
 For Values with lots of significant figures, <See Var=ESBPrecision> may need to be
 changed to a smaller value - <See Var=ESBDoublePrecision> for Doubles and
 <See Var=ESBSinglePrecision> for Singles.
 @param X1 First Float to process.
 @param X2 Second Float to process.
 @returns X1 - X2 >= ESBTolerance.
 @cat NumComparison
}
function GreaterFloat (const X1, X2: Extended): Boolean; overload;
function GreaterFloat (const X1, X2: Double): Boolean; overload;
function GreaterFloat (const X1, X2: Single): Boolean; overload;

{: Returns True if X1 < X2.
 For Values with lots of significant figures, <See Var=ESBPrecision> may need to be
 changed to a smaller value - <See Var=ESBDoublePrecision> for Doubles and
 <See Var=ESBSinglePrecision> for Singles.
 @param X1 First Float to process.
 @param X2 Second Float to process.
 @returns X1 - X2 <= -ESBTolerance.
 @cat NumComparison
}
function LesserFloat (const X1, X2: Extended): Boolean; overload;
function LesserFloat (const X1, X2: Double): Boolean; overload;
function LesserFloat (const X1, X2: Single): Boolean; overload;

{: Returns True if X is within <See Var=ESBTolerance> of 0.
 For Values with large negative exponents, <See Var=ESBTolerance> will need to be
 changed to a smaller value.
 For an alternate approach try <See Routine=SameFloat> (X, 0) when you wish a value to be
 within "precision" of 0.
 @param X  Float to process.
 @returns abs (X) < ESBTolerance.
 @cat NumComparison
}
function FloatIsZero (const X: Extended): Boolean;

{: Returns True if X is Positive, ie X > <See Var=ESBTolerance>.
 For Values with large negative exponents, <See Var=ESBTolerance> will need to be
 changed to a smaller value.
 @param X  Float to process.
 @returns X >= ESBTolerance.
 @cat NumComparison
}
function FloatIsPositive (const X: Extended): Boolean;

{: Returns True if X is Negative, ie X < -<See Var=ESBTolerance>.
 For Values with large negative exponents, <See Var=ESBTolerance> will need to be
 changed to a smaller value.
 @param X  Float to process.
 @returns X <= -ESBTolerance.
 @cat NumComparison
}
function FloatIsNegative (const X: Extended): Boolean;

{--- Float Functions ---}

{ Returns the Ceiling of X, ie Integral Value above.
 @param X Float to process
 @cat FloatMath
}
function ESBCeil (const X: Extended): Extended;

{: Returns the Floor of X, ie Integral Value Below.
 @param X Float to process
 @cat FloatMath
}
function ESBFloor (const X: Extended): Extended;

{: Returns Floating Point Modulus, given as X - <See Routine=ESBFloor> ( X / Y ) * Y.
 <See Var=ESBTolerance> may need to be changed to a different value.
 @param X First Float to process
 @param Y Second Float to process
 @cat FloatMath
}
function ESBMod (const X, Y: Extended): Extended;

{: Returns Floating Point Remainder, given as X - Int ( X / Y ) * Y.
 <See Var=ESBTolerance> may need to be changed to a different value.
 @param X First Float to process
 @param Y Second Float to process
 @cat FloatMath
}
function ESBRem (const X, Y: Extended): Extended;

{: Returns the Sign of the Value.
 -1 if X < 0.<p>
  0 if X = 0.<p>
  1 if X > 0.
 @param X Value to process
 @cat FloatMath
 @cat IntMath
 }
function ESBSign (const X: Extended): ShortInt; overload;
function ESBSign (const X: Int64): ShortInt; overload;
function ESBSign (const X: LongInt): ShortInt; overload;

{: Returns the FORTRAN type SIGN of the Values - basically it returns a value
 with the Magnitude of X and the Sign of Y.
 if Y < 0 then Returns - Abs (X)<p>
 else Returns Abs (X)
 @param X Value whose magnitude to process
 @param Y Value whose sign to process
 @cat FloatMath
 @cat IntMath
 }
function SignXY (const X, Y: Extended): Extended; overload;
function SignXY (const X, Y: Int64): Int64; overload;
function SignXY (const X, Y: LongInt): LongInt; overload;
function SignXY (const X: Extended; const Y: Int64): Extended; overload;

{--- Trigonometric Functions ---}

{: Returns Tangent of Angle given in Radians.
 @param Angle Angle in Radians.
 @cat FloatMath
}
function ESBTan (const Angle: Extended): Extended;

{: Returns CoTangent of the Angle given in Radians.
 @param Angle Angle in Radians.
 @cat FloatMath
}
function ESBCot (const Angle: Extended): Extended;

{: Returns CoSecant of the Angle given in Radians.
 @param Angle Angle in Radians.
 @cat FloatMath
}
function ESBCosec (const Angle: Extended): Extended;

{: Returns Secant of the Angle given in Radians.
 @param Angle Angle in Radians.
 @cat FloatMath
}
function ESBSec (const Angle: Extended): Extended;

{: Fast Computation of Sin and Cos, where Angle is in Radians.
 @param Angle Angle in Radians.
 @cat FloatMath
}
procedure ESBSinCos (const Angle: Extended; out SinX, CosX: Extended);

{: Returns the ArcTangent of Y / X - Result is in Radians.
 Results are given between -Pi and Pi.
 @param X First Float to process
 @param Y Second Float to process
 @cat FloatMath
}
function ESBArcTan (const X, Y: Extended): Extended;

{: Given a Value returns the Angle whose Cosine it is, in Radians.
 Return Values are between 0 and Pi.
 @param X Float to process.
 @cat FloatMath
}
function ESBArcCos (const X: Extended): Extended;

{: Given a Value returns the Angle whose Sine it is, in Radians.
 Return Values are between -Pi/2 and Pi/2.
 @param X Float to process.
 @cat FloatMath
}
function ESBArcSin (const X: Extended): Extended;

{: Given a Value returns the Angle whose Secant it is, in Radians.
 Return Values are between 0 and Pi.
 @param X Float to process.
 @cat FloatMath
}
function ESBArcSec (const X: Extended): Extended;

{: Given a Value returns the Angle whose Cosecant it is, in Radians.
 Return Values are between -Pi/2 and Pi/2.
 @param X Float to process.
 @cat FloatMath
}
function ESBArcCosec (const X: Extended): Extended;

{: Converts Radians into Degrees.
 @param X Value in Radians.
 @cat FloatMath
}
function Rad2Deg (const X: Extended): Extended;

{: Converts Degrees into Radians.
 @param X Value in Degrees.
 @cat FloatMath
}
function Deg2Rad (const X: Extended): Extended;

{: Converts Degrees/Minutes/Seconds into Degrees as Decimal.
 @param Degs Degrees Component as Input.
 @param Mins Minutes Component as Input.
 @param Secs Seconds Component as Input.
 @cat FloatMath
 @cat PosConv
}
function DMS2Deg (const Degs, Mins, Secs: Extended): Extended; overload;
function DMS2Deg (const Degs, Mins: Integer; const Secs: Extended): Extended; overload;

{: Converts Degrees as Decimal into Degrees/Minutes/Seconds.
 @param Deg Degrees as a Float to process.
 @param Degs Degrees Component as Output - if 0 then check Sign field.
 @param Mins Minutes Component as Output - always >= 0.
 @param Secs Seconds Component as Output - always >= 0.
 @param Sign -1 if Value is Negative, 0 if Value is Zero, 1 if Value is Positive.
 @cat FloatMath
 @cat PosConv
}
procedure Deg2DMS (const Deg: Extended; out Degs, Mins, Secs: Extended;
     out Sign: ShortInt); overload;
procedure Deg2DMS (const Deg: Extended; out Degs, Mins: Integer;
     out Secs: Extended; out Sign: ShortInt); overload;

{: Converts Gradients into Radians.
 @param X Value in Gradients.
 @cat FloatMath
}
function Grad2Rad (const Grad: Extended): Extended;

{: Converts Radians into Gradients.
 @param X Value in Radians.
 @cat FloatMath
}
function Rad2Grad (const Rad: Extended): Extended;

{: Converts Gradients into Degrees.
 @param X Value in Gradients.
 @cat FloatMath
}
function Grad2Deg (const Grad: Extended): Extended;

{: Converts Degrees into Gradients.
 @param X Value in Degrees.
 @cat FloatMath
}
function Deg2Grad (const Deg: Extended): Extended;

{: Adjusts a value in Degrees, so that it lies in between 0 and 360.
 @param X Value in Degrees.
 @cat FloatMath
}
function AdjustAngleDegree (const X: Extended): Extended;

{: Adjusts a value in Radians, so that it lies in between 0 and 2 * Pi.
 @param X Value in Radians.
 @cat FloatMath
}
function AdjustAngleRad (const X: Extended): Extended;

{: Returns Tangent of Angle given in Degrees.
 @param Angle Angle in Degrees.
 @cat FloatMath
}
function ESBTanDeg (const Angle: Extended): Extended;

{: Returns Sine of Angle given in Degrees.
 @param Angle Angle in Degrees.
 @cat FloatMath
}
function ESBSinDeg (const Angle: Extended): Extended;

{: Returns Cosine of Angle given in Degrees.
 @param Angle Angle in Degrees.
 @cat FloatMath
}
function ESBCosDeg (const Angle: Extended): Extended;

{: Returns Cotangent of Angle given in Degrees.
 @param Angle Angle in Degrees.
 @cat FloatMath
}
function ESBCotDeg (const Angle: Extended): Extended;

{: Returns Secant of Angle given in Degrees.
 @param Angle Angle in Degrees.
 @cat FloatMath
}
function ESBSecDeg (const Angle: Extended): Extended;

{: Returns Cosecant of Angle given in Degrees.
 @param Angle Angle in Degrees.
 @cat FloatMath
}
function ESBCosecDeg (const Angle: Extended): Extended;

{: Fast Computation of Sin and Cos, where Angle is in Degrees.
 @param Angle Angle in Degrees.
 @cat FloatMath
}
procedure ESBSinCosDeg (const Angle: Extended; out SinX, CosX: Extended);

{: Returns the ArcTangent of Y / X - Result is in Degrees.
 Results are given between -180 and 180.
 @param X First Float to process
 @param Y Second Float to process
 @cat FloatMath
}
function ESBArcTanDeg (const X, Y: Extended): Extended;

{: Given a Value returns the Angle whose Tangent it is, in Degrees.
 Return Values are between -90 and 90.
 @param X First Float to process
 @param Y Second Float to process
 @cat FloatMath
}
function ESBArcTanDeg2 (const X: Extended): Extended;

{: Given a Value returns the Angle whose Cosine it is, in Degrees.
 Return Values are between 0 and 180.
 @param X Float to process.
 @cat FloatMath
}
function ESBArcCosDeg (const X: Extended): Extended;

{: Given a Value returns the Angle whose Sine it is, in Degrees.
 Return Values are between -90 and 90.
 @param X Float to process.
 @cat FloatMath
}
function ESBArcSinDeg (const X: Extended): Extended;

{: Given a Value returns the Angle whose Secant it is, in Degrees.
 @param X Float to process.
 @cat FloatMath
}
function ESBArcSecDeg (const X: Extended): Extended;

{: Given a Value returns the Angle whose Cosecant it is, in Degrees.
 @param X Float to process.
 @cat FloatMath
}
function ESBArcCosecDeg (const X: Extended): Extended;

{--- Logarithm & Power Functions ---}

{: Returns Logarithm of X to Base 10.
 @param X Value to process.
 @cat FloatMath
}
function ESBLog10 (const X: Extended): Extended;

{: Returns Logarithm of X to Base 2.
 @param X Value to process.
 @cat FloatMath
}
function ESBLog2 (const X: Extended): Extended;

{: Returns Logarithm of X to Given Base.
 @param X Value to process.
 @param Base Logarithm Base to use.
 @cat FloatMath
}
function ESBLogBase (const X, Base: Extended): Extended;

{: Calculate 2 to the given floating point power. Developed by Rory Daulton
 and used with permission. December 1998.<p>
 EOverflow Exception when X >= 16384.
 (if there was no other FPU error condition, such as underflow or denormal,
 before entry to this routine)<p>
 EInvalidOp Exception on some occasions when EOverflow would be expected,
 due to some other FPU error condition (such as underflow) before entry to
 this routine.<p>
 NOTES: 1. This routine is faster and more accurate than Power(2, X).
    It is also faster and more accurate than Exp(X), so if
    exponentials and logarithms are needed in a program and the
    base is not important, use Pow2 and Log2 rather than Exp and
    Ln or even Power(10, X) and Log10.<p>
     2. The algorithm used is to scale the power of the fractional part
    of X, using FPU commands.<p>
     3. Although the FPU (Floating Point Unit) is used, the answer is
    exact for integral X, since the FSCALE FPU command is.<p>
     4. The result underflows to zero at a little less than -16445 and
    is a normal Extended value for -16382 <= X < 16384.<p>
     5. The comments in the code assume that the FPU rounding is set to
    banker's rounding.  The code should work for any kind of
    rounding, but the precise integer and fractional parts of X
    would vary depending on the rounding.<p>

 @param: X power to take of 2 [so the result is 2**X]
}
function Pow2 (const X: Extended): Extended;

{: Calculate any float to non-negative integer power. Developed by Rory Daulton
 and used with permission. Last modified December 1998.<p>
 @param: Base Value to use as the base.
 @param: Exponent power to take of Base [so the result is Base**Exponent]
}
function IntPow (const Base: Extended; const Exponent: LongWord): Extended;

{: Raises Values to an Integer Power. The different overloaded routines
 allow for better precision depending on the data types involved. The
 LongInt/Byte is for strictly Integer operations.<p>
 Zero to a Negative power raises an Exception.<p>
 The LongInt/Byte Routine cannot handle N > 62, as this raises an exception,
 so for N > 63, take the longint value and multiply it by 1.0 to ensure a
 different routine gets called.<p>
 Thanks to Rory Daulton for improvements.
 @param X Value to use as the Base
 @param N Integer Value to raise the Base to
 @cat IntMath
 @cat FloatMath
}
function ESBIntPower (const X: LongInt; const N: Byte): Int64; overload;
function ESBIntPower (const X: Extended; const N: LongInt): Extended; overload;
function ESBIntPower (const X: Extended; const N: LongWord): Extended; overload;

{: Returns X^Y - handles all cases (except those mentioned below).
 Zero to a Negative power raises an Exception.<p>
 If the routine can be handled with <See Routine=ESBIntPower> then it is
 used internally.<p>
 Thanks to Rory Daulton for improvements.
 @param X Value to use as Base.
 @param Y Value to use as Power.
 @returns X^Y.
 @cat FloatMath
}
function XtoY (const X, Y: Extended): Extended;

{: Returns 10^Y - handles all cases.
 Thanks to Rory Daulton for improvements.
 @param Y Value to use as Power.
 @cat FloatMath
}
function TenToY (const Y: Extended): Extended;

{: Returns 2^Y - handles all cases.
 Thanks to Rory Daulton for improvements.
 @param Y Value to use as Power.
 @cat FloatMath
}
function TwoToY (const Y: Extended): Extended;

{: ISqrt (I) computes INT (SQRT (I)), that is, the integral part of the
 square root of integer I.
 Code  originally developed by Marcel Martin, used with permission.
 Rory Daulton introduced a faster routine (based on Marcel's) for most
 occassions and this is now used with Permission.
 @param I Positive Integer Value to process.
 @cat IntMath
}
function ISqrt (const I: LongWord): Longword;

{: Calculate the integer part of the logarithm base 2 of an integer.
 Developed by Rory Daulton and used with Permission.<p>
 An Exception is raised if I is Zero.
 @param I Positive Integer Value to process.
 @cat IntMath
}
function ILog2 (const I: LongWord): LongWord;

{--- Hyperbolic Functions ---}

{: Returns the inverse hyperbolic cosine of X.
 Only Positive solutions are returned. Be aware that -X is also a valid
     result.
 @param X Value to process.
 @cat FloatMath
}
function ESBArCosh (const X: Extended): Extended;

{: Returns the inverse hyperbolic sine of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBArSinh (const X: Extended): Extended;

{: Returns the inverse hyperbolic tangent of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBArTanh (const X: Extended): Extended;

{: Returns the inverse hyperbolic cosecant of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBArCosech (const X: Extended): Extended;

{: Returns the inverse hyperbolic secant of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBArSech (const X: Extended): Extended;

{: Returns the inverse hyperbolic cotangent of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBArCoth (const X: Extended): Extended;

{: Returns the hyperbolic cosine of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBCosh (const X: Extended): Extended;

{: Returns the hyperbolic sine of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBSinh (const X: Extended): Extended;

{: Returns the hyperbolic tangent of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBTanh (const X: Extended): Extended;

{: Returns the hyperbolic cosecant of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBCosech (const X: Extended): Extended;

{: Returns the hyperbolic secant of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBSech (const X: Extended): Extended;

{: Returns the hyperbolic cotangent of X.
 @param X Value to process.
 @cat FloatMath
}
function ESBCoth (const X: Extended): Extended;

{--- Increment/Decrements ---}

{: Increments Value but won't go above specified Limit.
 If Value is already above limit than that value is returned.
 @param B Value to process.
 @param Limit Value that Increment won't go pass.
 @cat IntMath
}
procedure IncLim (var B: Byte; const Limit: Byte); overload;
procedure IncLim (var B: ShortInt; const Limit: ShortInt); overload;
procedure IncLim (var B: SmallInt; const Limit: SmallInt); overload;
procedure IncLim (var B: Word; const Limit: Word); overload;
procedure IncLim (var B: LongInt; const Limit: LongInt); overload;
procedure IncLim (var B: LongWord; const Limit: LongWord); overload;
procedure IncLim (var B: Int64; const Limit: Int64); overload;

{: Decrements Value but won't go below specified Limit.
 If Value is already below limit than that value is returned.
 @param B Value to process.
 @param Limit Value that Decrement won't go pass.
 @cat IntMath
}
procedure DecLim (var B: Byte; const Limit: Byte); overload;
procedure DecLim (var B: ShortInt; const Limit: ShortInt); overload;
procedure DecLim (var B: SmallInt; const Limit: SmallInt); overload;
procedure DecLim (var B: Word; const Limit: Word); overload;
procedure DecLim (var B: LongInt; const Limit: LongInt); overload;
procedure DecLim (var B: LongWord; const Limit: LongWord); overload;
procedure DecLim (var B: Int64; const Limit: Int64); overload;

{--- Maximums/Minimums ---}

{: Returns the Maximum of 2 Values.
 @param X First Value to Process.
 @param Y Second Value to Process.
 @cat IntMath
 @cat FloatMath
}
function MaxXY (const X, Y: Byte): Byte; overload;
function MaxXY (const X, Y: ShortInt): ShortInt; overload;
function MaxXY (const X, Y: Word): Word; overload;
function MaxXY (const X, Y: SmallInt): SmallInt; overload;
function MaxXY (const X, Y: LongWord): LongWord; overload;
function MaxXY (const X, Y: LongInt): LongInt; overload;
function MaxXY (const X, Y: Int64): Int64; overload;
function MaxXY (const X, Y: Extended): Extended; overload;
function MaxXY (const X, Y: Double): Double; overload;
function MaxXY (const X, Y: Single): Single; overload;

{ Returns the Minimum of 2 Values.
 @param X First Value to Process.
 @param Y Second Value to Process.
 @cat IntMath
 @cat FloatMath
}
function MinXY (const X, Y: Byte): Byte; overload;
function MinXY (const X, Y: ShortInt): ShortInt; overload;
function MinXY (const X, Y: Word): Word; overload;
function MinXY (const X, Y: SmallInt): SmallInt; overload;
function MinXY (const X, Y: LongWord): LongWord; overload;
function MinXY (const X, Y: LongInt): LongInt; overload;
function MinXY (const X, Y: Int64): Int64; overload;
function MinXY (const X, Y: Extended): Extended; overload;
function MinXY (const X, Y: Double): Double; overload;
function MinXY (const X, Y: Single): Single; overload;

{ Returns the Maximum of 3 Values.
 @param X First Value to Process.
 @param Y Second Value to Process.
 @param Z Third Value to Process.
 @cat IntMath
 @cat FloatMath
}
function MaxXYZ (const X, Y, Z: Byte): Byte; overload;
function MaxXYZ (const X, Y, Z: ShortInt): ShortInt; overload;
function MaxXYZ (const X, Y, Z: Word): Word; overload;
function MaxXYZ (const X, Y, Z: SmallInt): SmallInt; overload;
function MaxXYZ (const X, Y, Z: LongWord): LongWord; overload;
function MaxXYZ (const X, Y, Z: LongInt): LongInt; overload;
function MaxXYZ (const X, Y, Z: Int64): Int64; overload;
function MaxXYZ (const X, Y, Z: Extended): Extended; overload;
function MaxXYZ (const X, Y, Z: Double): Double; overload;
function MaxXYZ (const X, Y, Z: Single): Single; overload;

{: Returns the Minimum of 3 Values.
 @param X First Value to Process.
 @param Y Second Value to Process.
 @param Z Third Value to Process.
 @cat IntMath
 @cat FloatMath
}
function MinXYZ (const X, Y, Z: Byte): Byte; overload;
function MinXYZ (const X, Y, Z: ShortInt): ShortInt; overload;
function MinXYZ (const X, Y, Z: Word): Word; overload;
function MinXYZ (const X, Y, Z: SmallInt): SmallInt; overload;
function MinXYZ (const X, Y, Z: LongWord): LongWord; overload;
function MinXYZ (const X, Y, Z: LongInt): LongInt; overload;
function MinXYZ (const X, Y, Z: Int64): Int64; overload;
function MinXYZ (const X, Y, Z: Extended): Extended; overload;
function MinXYZ (const X, Y, Z: Double): Double; overload;
function MinXYZ (const X, Y, Z: Single): Single; overload;

{: Returns the Maximum of 4 Values.
 @param W First Value to Process.
 @param X Second Value to Process.
 @param Y Third Value to Process.
 @param Z Fourth Value to Process.
 @cat IntMath
 @cat FloatMath
}
function MaxWXYZ (const W, X, Y, Z: Byte): Byte; overload;
function MaxWXYZ (const W, X, Y, Z: ShortInt): ShortInt; overload;
function MaxWXYZ (const W, X, Y, Z: Word): Word; overload;
function MaxWXYZ (const W, X, Y, Z: SmallInt): SmallInt; overload;
function MaxWXYZ (const W, X, Y, Z: LongWord): LongWord; overload;
function MaxWXYZ (const W, X, Y, Z: LongInt): LongInt; overload;
function MaxWXYZ (const W, X, Y, Z: Int64): Int64; overload;
function MaxWXYZ (const W, X, Y, Z: Extended): Extended; overload;
function MaxWXYZ (const W, X, Y, Z: Double): Double; overload;
function MaxWXYZ (const W, X, Y, Z: Single): Single; overload;

{: Returns the Minimum of 4 Values.
 @param W First Value to Process.
 @param X Second Value to Process.
 @param Y Third Value to Process.
 @param Z Fourth Value to Process.
 @cat IntMath
 @cat FloatMath
}
function MinWXYZ (const W, X, Y, Z: Byte): Byte; overload;
function MinWXYZ (const W, X, Y, Z: ShortInt): ShortInt; overload;
function MinWXYZ (const W, X, Y, Z: Word): Word; overload;
function MinWXYZ (const W, X, Y, Z: SmallInt): SmallInt; overload;
function MinWXYZ (const W, X, Y, Z: LongWord): LongWord; overload;
function MinWXYZ (const W, X, Y, Z: LongInt): LongInt; overload;
function MinWXYZ (const W, X, Y, Z: Int64): Int64; overload;
function MinWXYZ (const W, X, Y, Z: Extended): Extended; overload;
function MinWXYZ (const W, X, Y, Z: Double): Double; overload;
function MinWXYZ (const W, X, Y, Z: Single): Single; overload;

{--- Special Functions ---}

{: Returns 1/Gamma(X) using a Series Expansion as defined in Abramowitz &
 Stegun. Defined for all values of X.<p>
 Accuracy: Gives about 15 digits.
 @param X Value to process.
 @cat FloatMath
}
function InverseGamma (const X: Extended): Extended;

{: Returns Gamma(X) using a Series Expansion for 1/Gamma (X) as defined in
 Abramowitz & Stegun. Defined for all values of X except negative
 integers and 0.<p>
 Accuracy: Gives about 15 digits.
 @param X Value to process.
 @cat FloatMath
}
function Gamma (const X: Extended): Extended;

{: Logarithm to base e of the gamma function. Defined for all positive values of X.<p>
 Accurate to about 14 digits.<p>
 Programmer: Alan Miller - developed for Fortan 77, converted with permission.
 @param X Value to process.
 @cat FloatMath
}
function LnGamma (const X: Extended): Extended;

{: Returns Beta(X,Y) using a Series Expansion for 1/Gamma (X) as defined in
 Abramowitz & Stegun. Defined for all values of X and Y except negative
 integers and 0.<p>
 Accuracy: Gives about 15 digits.
 @param X First Value to process.
 @param Y Second Value to process.
 @cat FloatMath
}
function Beta (const X, Y: Extended): Extended;

{: Returns the Natural Logarithm of Beta(X,Y) using a LnGamma (X) as defined above.
 X and Y must be positive.<p>
 Accuracy: Gives about 15 digits.
 @param X First Value to process.
 @param Y Second Value to process.
 @cat FloatMath
}
function LnBeta (const X, Y: Extended): Extended;

{: Returns the Incomplete Beta Ix(P, Q), where 0 <= X <= 1 and
 P and Q are positive. The Incomplete Beta function is the probability that
 a random variable from a Beta distribution having parameters P and Q will
 be less than or equal to X.
 Accuracy: Gives about 17 digits.
 Adapted From Collected Algorithms from CACM,
 Algorithm 179 - Incomplete Beta Function Ratios,
 Oliver G Ludwig.
 @cat FloatMath
}
function IncompleteBeta (X: Extended; P, Q: Extended): Extended;

{: Returns a Hermite Polynomial of order N evaluated at X.
 @param X Value to process.
 @param N Order of Hermite.
 @cat FloatMath
}
function Hermite (const X: Extended; const N: LongWord): Extended;

{--- Integer Routines ---}

{: Returns the Greatest Common (Positive) Divisor (GCD)of two Integers. Also
 Refered to as the Highest Common Factor (HCF). Uses Euclid's Algorithm.
 Please note that routine assumes GCD(0,0) to be 0.
 BASM Routine donated by Marcel Martin.
 @param X First Value to process.
 @param Y Second Value to process.
 @cat IntMath
}
function GCD (const X, Y: LongWord): LongWord;

{: Returns the Least Common Multiple of two Integers.
 Please note that routine assumes LCM (0, 0) to be invalid and
  raises an error;
 @param X First Value to process.
 @param Y Second Value to process.
 @cat IntMath
}
function LCM (const X, Y: LongInt): Int64;

{: If two Integers are Relative Prime to each other then GCD (X, Y) = 1.
 CoPrime is another term for Relative Prime. Some interpretive
 problems may arise when '0' and/or '1' are used.
 @param X First Value to process.
 @param Y Second Value to process.
 @cat IntMath
 @cat NumComparison
}
function RelativePrime (const X, Y: LongWord): Boolean;

{: Algebraically calculates the sum of a series. If only a single parameter
 is used, then it assumes that 1 is the StartValue. Returns the sum
 of all the integers between StartValue & EndValue inclusive.
 Optimised BASM supplied by Marcel Martin.
 @param StartValue Value to Start Summation from. Optional - 1 is assumed if omitted.
 @param EndValue Last Value of Series to Sum.
 @cat IntMath
}
function SumOfSeries (EndValue: LongWord): Int64; overload;
function SumOfSeries (StartValue, EndValue: LongWord): Int64; overload;

{--- Polynomials ---}

{: Solves a Quadratic Equation of the form AX^2 + BX + C = 0. The function
 returns the number of roots found, and X1 & X2 are the values.
 @param A Coefficient of X^2 in the Quadratic Equation.
 @param B Coefficient of X in the Quadratic Equation.
 @param C Constant in the Quadratic Equation.
 @param X1 First Root of the Quadratic Equation.
 @param X2 Second Root of the Quadratic Equation.
 @Returns 0 then there are no roots, X1 = X2 = 0. 1 then there is only one
  root, X1 = X2. 2 then there are two distinct roots X1 <> X2.
 @cat FloatMath
}
function SolveQuadratic (const A, B, C: Extended; out X1, X2: Extended): Byte;

{: Returns the straight line Distance between (X1, Y1) and (X2, Y2) }
function ESBDistance (const X1, Y1, X2, Y2: Extended): Extended;

implementation

uses
     SysUtils,
     QESBPCS_RS_Math,
     QESBPCSSystem;

{--- 80x87 Handling ---}

function Get87ControlWord: TESBBitList;
var
     Temp: Word;
asm
	fstcw 	[Temp]  		// Get '87 Control Word
	mov  	ax, [Temp] 	// Leave in AX for function
end;

procedure Set87ControlWord (const ControlWord: TESBBitList);
var
     Temp: Word;
asm
	mov   	[Temp], ax    // Move Control Word into Memory location
	fldcw 	[Temp] 		// Load '87 Control Word
end;

function GetRoundingType: TESBRoundingType;
var
     CW: TESBBitList;
     B: Byte;
begin
     CW := Get87ControlWord;
     if BitIsSet (CW, 11) then
          B := 2
     else
          B := 0;
     if BitIsSet (CW, 10) then
          B := B + 1;
     Result := TESBRoundingType (B);
end;

procedure SetRoundingType (const RT: TESBRoundingType);
var
     CW: TESBBitList;
     B: Byte;
begin
     CW := Get87ControlWord;
     B := Byte (RT);
     if B >= 2 then
          SetBit (CW, 11)
     else
          ClearBit (CW, 11);
     if B mod 2 = 1 then
          SetBit (CW, 10)
     else
          ClearBit (CW, 10);
     Set87ControlWord (CW);
end;

procedure ExtractParts (const X: Extended; out Mantissa: Extended;
     out Exponent: Integer);
begin
     if FloatIsZero (X) then
     begin
          Exponent := 0;
          Mantissa := 0;
     end
     else
     begin
          Exponent := Round (ESBFloor (ESBLog10 (abs (X))));
          Mantissa := X / TenToY (Exponent);
     end;
end;

function BuildFromParts (const Mantissa: Extended;
     const Exponent: Integer): Extended;
begin
     Result := Mantissa * TenToY (Exponent);
end;

{--- Comparisons ---}

function ExtendedPrecision (const X1, X2: Extended): Extended;
begin
     if (X1 = 0) or (X2 = 0) then
          Result := ESBPrecision
     else
          Result := MinXY (abs (X1), abs (X2)) * ESBPrecision
end;

function DoublePrecision (const X1, X2: Double): Double;
begin
     if (X1 = 0) or (X2 = 0) then
          Result := ESBDoublePrecision
     else
          Result := MinXY (abs (X1), abs (X2)) * ESBDoublePrecision
end;

function SinglePrecision (const X1, X2: Single): Single;
begin
     if (X1 = 0) or (X2 = 0) then
          Result := ESBSinglePrecision
     else
          Result := MinXY (abs (X1), abs (X2)) * ESBSinglePrecision
end;

function SameFloat (const X1, X2: Extended): Boolean;
begin
     Result := abs (X1 - X2) <= ExtendedPrecision (X1, X2);
end;

function SameFloat (const X1, X2: Double): Boolean;
begin
     Result := abs (X1 - X2) <= DoublePrecision (X1, X2);
end;

function SameFloat (const X1, X2: Single): Boolean;
begin
     Result := abs (X1 - X2) <= SinglePrecision (X1, X2);
end;

function CompareFloat (const X1, X2: Extended): Integer;
begin
     if SameFloat (X1, X2) then
          Result := 0
     else if X1 < X2 then
          Result := -1
     else
          Result := 1;
end;

function CompareFloat (const X1, X2: Double): Integer;
begin
     if SameFloat (X1, X2) then
          Result := 0
     else if X1 < X2 then
          Result := -1
     else
          Result := 1;
end;

function CompareFloat (const X1, X2: Single): Integer;
begin
     if SameFloat (X1, X2) then
          Result := 0
     else if X1 < X2 then
          Result := -1
     else
          Result := 1;
end;

function GreaterFloat (const X1, X2: Extended): Boolean;
begin
     Result := X1 - X2 > ExtendedPrecision (X1, X2);
end;

function GreaterFloat (const X1, X2: Double): Boolean;
begin
     Result := X1 - X2 > DoublePrecision (X1, X2);
end;

function GreaterFloat (const X1, X2: Single): Boolean;
begin
     Result := X1 - X2 > SinglePrecision (X1, X2);
end;

function LesserFloat (const X1, X2: Extended): Boolean;
begin
     Result := X1 - X2 < -ExtendedPrecision (X1, X2);
end;

function LesserFloat (const X1, X2: Double): Boolean;
begin
     Result := X1 - X2 < -DoublePrecision (X1, X2);
end;

function LesserFloat (const X1, X2: Single): Boolean;
begin
     Result := X1 - X2 < -SinglePrecision (X1, X2);
end;

function FloatIsZero (const X: Extended): Boolean;
begin
     Result := abs (X) < ESBTolerance
end;

function FloatIsPositive (const X: Extended): Boolean;
begin
     Result := (X >= ESBTolerance);
end;

function FloatIsNegative (const X: Extended): Boolean;
begin
     Result := (X <= -ESBTolerance);
end;

{--- Float Functions ---}

function ESBCeil (const X: Extended): Extended;
begin
     Result := Int (X);
     if Result < X then
          Result := Result + 1.0;
end;

function ESBFloor (const X: Extended): Extended;
begin
     Result := Int (X);
     if Result > X then
          Result := Result - 1.0;
end;

function ESBMod (const X, Y: Extended): Extended;
begin
     if FloatIsZero (Y) then
          raise EMathError.Create (rsDivideByZero);

     Result := X - ESBFloor (X / Y) * Y
end;

function ESBRem (const X, Y: Extended): Extended;
begin
     if FloatIsZero (Y) then
          raise EMathError.Create (rsDivideByZero);

     Result := X - Int (X / Y) * Y
end;

function ESBSign (const X: Extended): ShortInt;
begin
     if FloatIsNegative (X) then
          Result := -1
     else if FloatIsZero (X) then
          Result := 0
     else
          Result := 1
end;

function ESBSign (const X: Int64): ShortInt;
begin
     if X < 0 then
          Result := -1
     else if X = 0 then
          Result := 0
     else
          Result := 1
end;

function ESBSign (const X: LongInt): ShortInt;
begin
     if X < 0 then
          Result := -1
     else if X = 0 then
          Result := 0
     else
          Result := 1
end;

function SignXY (const X, Y: Extended): Extended; overload;
begin
     if Y < 0 then
          Result := -Abs (X)
     else
          Result := Abs (X);
end;

function SignXY (const X, Y: Int64): Int64; overload;
begin
     if Y < 0 then
          Result := -Abs (X)
     else
          Result := Abs (X);
end;

function SignXY (const X, Y: LongInt): LongInt; overload;
begin
     if Y < 0 then
          Result := -Abs (X)
     else
          Result := Abs (X);
end;

function SignXY (const X: Extended; const Y: Int64): Extended; overload;
begin
     if Y < 0 then
          Result := -Abs (X)
     else
          Result := Abs (X);
end;

{--- Trigonometric Functions ---}

function ESBTanDeg (const Angle: Extended): Extended;
begin
     Result := ESBTan (Deg2Rad (Angle));
end;

function ESBSecDeg (const Angle: Extended): Extended;
begin
     Result := ESBSec (Deg2Rad (Angle));
end;

function ESBCosecDeg (const Angle: Extended): Extended;
begin
     Result := ESBCosec (Deg2Rad (Angle));
end;

function ESBSinDeg (const Angle: Extended): Extended;
begin
     Result := Sin (Deg2Rad (Angle));
end;

function ESBCosDeg (const Angle: Extended): Extended;
begin
     Result := Cos (Deg2Rad (Angle));
end;

function ESBCotDeg (const Angle: Extended): Extended;
begin
     Result := ESBCot (Deg2Rad (Angle));
end;

procedure ESBSinCosDeg (const Angle: Extended; out SinX, CosX: Extended);
begin
     ESBSinCos (Deg2Rad (Angle), SinX, CosX);
end;

function ESBArcTanDeg (const X, Y: Extended): Extended;
begin
     Result := Rad2Deg (ESBArcTan (X, Y));
end;

function ESBArcTanDeg2 (const X: Extended): Extended;
begin
     Result := Rad2Deg (ArcTan (X));
end;

function ESBArcCosDeg (const X: Extended): Extended;
begin
     Result := Rad2Deg (ESBArcCos (X));
end;

function ESBArcSinDeg (const X: Extended): Extended;
begin
     Result := Rad2Deg (ESBArcSin (X));
end;

function ESBArcSecDeg (const X: Extended): Extended;
begin
     Result := Rad2Deg (ESBArcSec (X));
end;

function ESBArcCosecDeg (const X: Extended): Extended;
begin
     Result := Rad2Deg (ESBArcCosec (X));
end;

function ESBTan (const Angle: Extended): Extended;

     function FTan (Angle: Extended): Extended;
     asm
		fld		[Angle]	// St(0) <- Angle
		ffree     st(7)	// Ensure st(7) is free
		fptan		     // St(1) <- Tan (Angle), St(0) <- 1
		fstp		st(0)	// Dispose of 1
		fwait

     end;
begin
     if abs (Angle) >= TwoToPower63 then // must be less then 2^63
          raise EMathError.Create (rsAngleTooLarge);
     Result := FTan (Angle);
end;

function ESBCot (const Angle: Extended): Extended;

     function FCot (Angle: Extended): Extended;
     asm
		fld		[Angle]	// St(0) <- Angle
		ffree     st(7)	// Ensure st(7) is free
		fptan		     // St(1) <- Tan (Angle), St(0) <- 1
		fdivrp			// St(0) <- St(0)/St(1) which is Cot
		fwait

     end;
begin
     if FloatIsZero (Angle) then
          raise EMathError.Create (rsDivideByZero);
     if abs (Angle) >= TwoToPower63 then // must be less then 2^63
          raise EMathError.Create (rsAngleTooLarge);
     Result := FCot (Angle);
end;

function ESBArcTan (const X, Y: Extended): Extended;

     function FArcTan (X, Y: Extended): Extended;
     asm
		fld		[Y]		// St(0) <- Y
		fld	     [X]		// St(0) <- X, St (1) <- Y
		fpatan			// St(0) <- ArcTan (Y/X)
		fwait

     end;
begin
     if FloatIsZero (X) then
     begin
          if FloatIsZero (Y) then
               Result := 0
          else if Y > 0 then
               Result := PiOn2
          else
               Result := -PiOn2
     end
     else if FloatIsZero (Y) then
     begin
          if X > 0 then
               Result := 0
          else
               Result := ESBPi
     end
     else
          Result := FArcTan (X, Y);
end;

procedure ESBSinCos (const Angle: Extended; out SinX, CosX: Extended);

     procedure FSinCos (Angle: Extended; var SinX, CosX: Extended);
     asm
		fld		[Angle]			// St(0) <- Angle
		fsincos
		fstp		tbyte ptr [edx]    // St(0) -> CosX
		fstp    	tbyte ptr [eax]    // St(0) -> SinX
		fwait

     end;
begin
     if abs (Angle) >= TwoToPower63 then // must be less then 2^63
          raise EMathError.Create (rsAngleTooLarge);

     FSinCos (Angle, SinX, CosX);
end;

function ESBArcCos (const X: Extended): Extended;
var
     Y: Extended;
begin
     if abs (X) > 1 then
          raise EMathError.Create (rsValueLEOne);

     if X = 0 then
          Result := PiOn2
     else
     begin
          Y := Sqrt (1 - Sqr (X));
          if FloatIsZero (Y) then
          begin
               if X > 0 then
                    Result := 0
               else
                    Result := ESBPi
          end
          else
               Result := ESBArcTan (X, Y)
     end;
end;

function ESBArcSin (const X: Extended): Extended;
var
     Y: Extended;
begin
     if abs (X) > 1 then
          raise EMathError.Create (rsValueLEOne);

     if X = 0 then
          Result := 0
     else
     begin
          Y := Sqrt (1 - Sqr (X));
          if FloatIsZero (Y) then
          begin
               if X > 0 then
                    Result := PiOn2
               else
                    Result := -PiOn2
          end
          else
               Result := ESBArcTan (Y, X)
     end;
end;

function ESBCosec (const Angle: Extended): Extended;
var
     Y: Extended;
begin
     Y := Sin (Angle);
     if FloatIsZero (Y) then
          raise EMathError.Create (rsDivideByZero);

     Result := 1 / Y;
end;

function ESBSec (const Angle: Extended): Extended;
var
     Y: Extended;
begin
     Y := Cos (Angle);
     if FloatIsZero (Y) then
          raise EMathError.Create (rsDivideByZero);

     Result := 1 / Y;
end;

function ESBArcSec (const X: Extended): Extended;
begin
     if abs (X) < 1 then
          raise EMathError.Create (rsDivideByZero);

     Result := ESBArcCos (1 / X);
end;

function ESBArcCosec (const X: Extended): Extended;
begin
     if abs (X) < 1 then
          raise EMathError.Create (rsDivideByZero);

     Result := ESBArcSin (1 / X);
end;

function Rad2Deg (const X: Extended): Extended;
begin
     Result := X * OneRadian
end;

function Deg2Rad (const X: Extended): Extended;
begin
     Result := X * OneDegree
end;

function DMS2Deg (const Degs, Mins, Secs: Extended): Extended;
begin
     Result := Degs + Mins / 60 + Secs / 3600
end;

function DMS2Deg (const Degs, Mins: Integer; const Secs: Extended): Extended;
begin
     Result := Degs + Mins / 60 + Secs / 3600
end;

procedure Deg2DMS (const Deg: Extended; out Degs, Mins, Secs: Extended;
     out Sign: ShortInt);
var
     X: Extended;
begin
     Sign := ESBSign (Deg);
     Degs := Int (Deg);
     X := Frac (Abs (Deg)) * 60;
     Mins := Int (X);
     Secs := Frac (X) * 60;
end;

procedure Deg2DMS (const Deg: Extended; out Degs, Mins: Integer;
     out Secs: Extended; out Sign: ShortInt);
var
     X: Extended;
begin
     Sign := ESBSign (Deg);
     Degs := Trunc (Deg);
     X := Frac (abs (Deg)) * 60;
     Mins := Trunc (X);
     Secs := Frac (X) * 60;
end;

function Grad2Rad (const Grad: Extended): Extended;
begin
     Result := 0.005 * ESBPi * Grad;
end;

function Rad2Grad (const Rad: Extended): Extended;
begin
     Result := (200 * InvPI) * Rad;
end;

function Grad2Deg (const Grad: Extended): Extended;
begin
     Result := 0.9 * Grad;
end;

function Deg2Grad (const Deg: Extended): Extended;
begin
     Result := (100 / 90) * Deg;
end;

{--- Logarithm & Power Functions ---}

{------------------------------------------------------------------------------
PURPOSE:     Calculate 2 to the given floating point power.
AUTHOR:      Rory Daulton
DATE:        December 1998
PARAMETERS:  X: power to take of 2 [so the result is 2**X]
EXCEPTIONS:  EOverflow  when X > Log2(MaxExtended) = 16383.8868675082210346
        (if there was no other FPU error condition, such as
        underflow or denormal, before entry to this routine)
     EInvalidOp on some occasions when EOverflow would be expected, due
        to some other FPU error condition (such as underflow)
        before entry to this routine
NOTES:       1. The algorithm used is to scale the power of the fractional part
    of X, using FPU commands.
     2. Although the FPU (Floating Point Unit) is used, the answer is
    exact for integral X, since the FSCALE FPU command is.
------------------------------------------------------------------------------}

function Pow2 (const X: Extended): Extended;
asm
	   fld     X
// find Round(X)
	   fld     st
	   frndint
// find _Frac(X) [minimal fractional part of X, between -0.5 and 0.5]
	   fsub    st(1),st
	   fxch    st(1)
// Find 2**_Frac(X)
	   f2xm1
	   fld1
	   fadd
// Result := 2**_Frac(X) * 2**Round(X)
	   fscale
	   fstp    st(1)
	   fwait
end {Pow2};

function IntPow (const Base: Extended; const Exponent: LongWord): Extended;
{ Heart of Rory Daulton's IntPower: assumes valid parameters &
non-negative exponent }
asm
		fld1                      { Result := 1 }
		cmp     eax, 0            { eax := Exponent }
		jz      @@3
		fld     Base
		jmp     @@2
  @@1:    fmul    ST, ST            { X := Base * Base }
  @@2:    shr     eax,1
		jnc     @@1
		fmul    ST(1),ST          { Result := Result * X }
		jnz     @@1
		fstp    st                { pop X from FPU stack }
  @@3:
		fwait
end {P};

function ESBLog10 (const X: Extended): Extended;

     function FLog10 (X: Extended): Extended;
     asm
		fldlg2			// St(0) <- Log10 of 2
		fld		[X]		// St(0) <- X, St(1) <- Log10 of 2
		fyl2x			// St(0) <- log10 (2) * Log2 (X)
		fwait

     end;

     function AltFLog10 (X: Extended): Extended;
     asm
		fldlg2			// St(0) <- Log10 of 2
		fld		[X]		// St(0) <- X, St(1) <- Log10 of 2
		fyl2xp1			// St(0) <- log10 (2) * Log2 (X+1)
		fwait

     end;
begin
     if not FloatIsPositive (X) then // must be Positive
          raise EMathError.Create (rsValueGZero)
     else if abs (X - 1) < 0.1 then
          Result := AltFLog10 (X - 1)
     else
          Result := FLog10 (X);
end;

function ESBLog2 (const X: Extended): Extended;

     function FLog2 (X: Extended): Extended;
     asm
		fld1				// St(0) <- 1
		fld		[X]		// St(0) <- X, St(1) <-1
		fyl2x			// St(0) <- 1 * Log2 (X)
		fwait

     end;

     function AltFLog2 (X: Extended): Extended;
     asm
		fld1				// St(0) <- 1
		fld		[X]		// St(0) <- X, St(1) <-1
		fyl2xp1			// St(0) <- 1 * Log2 (X+1)
		fwait

     end;
begin
     if not FloatIsPositive (X) then // must be Positive
          raise EMathError.Create (rsValueGZero)
     else if abs (X - 1) < 0.1 then
          Result := AltFLog2 (X - 1)
     else
          Result := FLog2 (X);
end;

function ESBLogBase (const X, Base: Extended): Extended;
begin
     if not FloatIsPositive (X) then // must be Positive
          raise EMathError.Create (rsValueGZero)
     else if not FloatIsPositive (Base) then // must be Positive
          raise EMathError.Create (rsValueGZero)
     else
          Result := ESBLog2 (X) / ESBLog2 (Base);
end;

function ESBIntPower (const X: LongInt; const N: Byte): Int64; overload;
var
     I: LongWord;
begin
     if N > 62 then
          raise EMathError.Create (rsPowerInt64);

     if N = 0 then
          Result := 1
     else if (X = 0) or (X = 1) then
          Result := X
     else if X = -1 then
     begin
          if Odd (N) then
               Result := -1
          else
               Result := 1
     end
     else
     begin
          Result := X;
          for I := 2 to N do
               Result := Result * X;
     end;
end;

function ESBIntPower (const X: Extended; const N: LongInt): Extended; overload;
var
     P: LongWord;
begin
     if N = 0 then
          Result := 1
     else if (X = 0) then
     begin
          if N < 0 then
               raise EMathError.Create (rsZeroToNegPower)
          else
               Result := 0
     end
     else if (X = 1) then
          Result := 1
     else if X = -1 then
     begin
          if Odd (N) then
               Result := -1
          else
               Result := 1
     end
     else if N > 0 then
          Result := IntPow (X, N)
     else
     begin
          if N <> Low (LongInt) then
               P := abs (N)
          else
               P := LongWord (High (LongInt)) + 1;

          try
               Result := IntPow (X, P);
          except
               on EMathError do
               begin
                    Result := IntPow (1 / X, P); { try again with another method, }
                    Exit; {   perhaps less precise         }
               end {on};
          end {try};
          Result := 1 / Result;
     end;
end;

function ESBIntPower (const X: Extended; const N: LongWord): Extended; overload;
begin
     if N = 0 then
          Result := 1
     else if (X = 0) or (X = 1) then
          Result := X
     else if X = -1 then
     begin
          if Odd (N) then
               Result := -1
          else
               Result := 1
     end
     else
          Result := IntPow (X, N)
end;

function XtoY (const X, Y: Extended): Extended;

     function PowerAbs: Extended; // Routine developed by Rory Daulton
     var
          ExponentPow2: Extended; // equivalent exponent to power 2
     begin
          try
               ExponentPow2 := ESBLog2 (Abs (X)) * Y;
          except
               on EMathError do
                    // allow underflow, when ExponentPow2 would have been negative
                    if (Abs (X) > 1) <> (Y > 0) then
                    begin
                         Result := 0;
                         Exit;
                    end {if}
                    else
                         raise;
          end {try};
          Result := Pow2 (ExponentPow2);
     end;
begin
     if FloatIsZero (Y) then
          Result := 1
     else if FloatIsZero (X) then
     begin
          if Y < 0 then
               raise EMathError.Create (rsZeroToNegPower)
          else
               Result := 0
     end
     else if FloatIsZero (Frac (Y)) then
     begin
          if (Y >= Low (LongInt)) and (Y <= High (LongInt)) then
               Result := ESBIntPower (X, LongInt (Round (Y)))
          else
          begin
               if (X > 0) or FloatIsZero (Frac (Y / 2.0)) then
                    Result := PowerAbs
               else
                    Result := -PowerAbs
          end;
     end
     else if X > 0 then
          Result := PowerAbs
     else
          raise EMathError.Create (rsInvalidXtoY)
end;

function TenToY (const Y: Extended): Extended;
begin
     if FloatIsZero (Y) then
          Result := 1
     else if Y < -3.58E4931 then
          Result := 0
     else
          Result := Pow2 (Y * Log10Base2)
end;

function TwoToY (const Y: Extended): Extended;
begin
     if FloatIsZero (Y) then
          Result := 1
     else
          Result := Pow2 (Y)
end;

function ISqrt (const I: LongWord): LongWord;
const
     Estimates: array [0..31] of Word = (
          // for index i, constant := Trunc(Sqrt((Int64(2) shl i) - 1.0)), which is
          //   the largest possible ISqrt(n) for  2**i <= n < 2**(i+1)
          1, 1, 2, 3, 5, 7, 11, 15,
          22, 31, 45, 63, 90, 127, 181, 255,
          362, 511, 724, 1023, 1448, 2047, 2896, 4095,
          5792, 8191, 11585, 16383, 23170, 32767, 46340, 65535);
     // eax  // ebx  // ecx  // edx
asm  // entry:   // eax = I
  // calc the result quickly for zero or one (sqrt equals the argument)
	   cmp     eax, 1
	   jbe     @@end
  // save registers and the argument
	   push    ebx
	   mov     ebx, eax                // ebx = I
  // use the logarithm base 2 to load an initial estimate, which is greater
  //   than or equal to the actual value
	   bsr     eax, ebx        // eax = ILog2(I) (note upper WORD is now zero)
	   mov     ax, [word ptr Estimates + eax * 2]
						  // eax = X
  // repeat ...
@@repeat:
  // --  save the last estimate [ X ]
	   mov     ecx, eax                        // ecx = X
  // --  calc the new estimate [ (I/X + X) / 2 ; the Newton-Raphson formula ]
	   xor     edx, edx                                // edx = 0
	   mov     eax, ebx        // eax = I (so edx:eax = I)
	   div     ecx             // eax = I/X            // edx = I mod X
	   add     eax, ecx        // eax = I/X + X
	   shr     eax, 1          // eax = XNew = (I/X+X)/2
  // until the new estimate >= the last estimate
  //   [which can never happen in exact floating-point arithmetic, and can
  //   happen due to truncation only if the last estimate <= Sqrt(I) ]
	   cmp     eax, ecx
	   jb      @@repeat
  // use the next-to-last estimate as the result
	   mov     eax, ecx        // eax = X
  // restore registers
	   pop     ebx
@@end:              //exit:     // eax = Result
end {ISqrt};

function ILog2 (const I: LongWord): LongWord;

     procedure BadILog2;
     begin
          raise EMathError.Create (rsDivideByZero);
     end {BadILog2};
asm
    bsr     eax,eax
    jz      BadILog2
end {ILog2};

{--- Hyperbolic Functions ---}

function ESBArCosh (const X: Extended): Extended;
begin
     if X < 1 then
          raise EMathError.Create (rsNotDefinedForValue);

     Result := Ln (X + Sqrt (Sqr (X) - 1));
end;

function ESBArSinh (const X: Extended): Extended;
begin
     Result := Ln (X + Sqrt (Sqr (X) + 1));
end;

function ESBArTanh (const X: Extended): Extended;
var
     Y: Extended;
begin
     if Abs (X) >= 1 then
          raise EMathError.Create (rsNotDefinedForValue);

     Y := (1 + X) / (1 - X);
     Result := 0.5 * Ln (Y);
end;

function ESBArCoth (const X: Extended): Extended;
var
     Y: Extended;
begin
     if Abs (X) <= 1 then
          raise EMathError.Create (rsNotDefinedForValue);

     Y := (1 + X) / (X - 1);
     Result := 0.5 * Ln (Y);
end;

function ESBArSech (const X: Extended): Extended;
begin
     if FloatIsZero (X) then
          raise EMathError.Create (rsDivideByZero);

     Result := ESBArCosh (1 / X);
end;

function ESBArCosech (const X: Extended): Extended;
begin
     if FloatIsZero (X) then
          raise EMathError.Create (rsDivideByZero);

     Result := ESBArSinh (1 / X);
end;

function ESBCosh (const X: Extended): Extended;
var
     Z: Extended;
begin
     Z := Exp (X);
     Result := 0.5 * (Z + 1.0 / Z);
end;

function ESBSinh (const X: Extended): Extended;
var
     Z: Extended;
begin
     Z := Exp (X);
     Result := 0.5 * (Z - 1.0 / Z);
end;

function ESBTanh (const X: Extended): Extended;
var
     Y, Z, InvZ: Extended;
begin
     Z := Exp (X);
     InvZ := 1 / Z;
     Y := Z + InvZ;
     if FloatIsZero (Y) then // Should never happen
          raise EMathError.Create (rsNotDefinedForValue);

     Result := (Z - InvZ) / Y;
end;

function ESBCoth (const X: Extended): Extended;
var
     Y, Z, InvZ: Extended;
begin
     Z := Exp (X);
     InvZ := 1 / Z;
     Y := Z - InvZ;
     if FloatIsZero (Y) then
          raise EMathError.Create (rsNotDefinedForValue);

     Result := (Z + InvZ) / Y;
end;

function ESBCosech (const X: Extended): Extended;
begin
     if SameFloat (X, 0) then
          raise EMathError.Create (rsNotDefinedForValue);

     Result := 1 / ESBSinh (x);
end;

function ESBSech (const X: Extended): Extended;
begin
     Result := 1 / ESBCosh (x);
end;

{--- Increment/Decrements ---}

procedure IncLim (var B: Byte; const Limit: Byte);
begin
     if B < Limit then
          Inc (B);
end;

procedure IncLim (var B: ShortInt; const Limit: ShortInt);
begin
     if B < Limit then
          Inc (B);
end;

procedure IncLim (var B: SmallInt; const Limit: SmallInt);
begin
     if B < Limit then
          Inc (B);
end;

procedure IncLim (var B: Word; const Limit: Word);
begin
     if B < Limit then
          Inc (B);
end;

procedure IncLim (var B: LongInt; const Limit: LongInt);
begin
     if B < Limit then
          Inc (B);
end;

procedure IncLim (var B: LongWord; const Limit: LongWord);
begin
     if B < Limit then
          Inc (B);
end;

procedure IncLim (var B: Int64; const Limit: Int64);
begin
     if B < Limit then
          Inc (B);
end;

procedure DecLim (var B: Byte; const Limit: Byte);
begin
     if B > Limit then
          Dec (B);
end;

procedure DecLim (var B: ShortInt; const Limit: ShortInt);
begin
     if B > Limit then
          Dec (B);
end;

procedure DecLim (var B: SmallInt; const Limit: SmallInt);
begin
     if B > Limit then
          Dec (B);
end;

procedure DecLim (var B: Word; const Limit: Word);
begin
     if B > Limit then
          Dec (B);
end;

procedure DecLim (var B: LongInt; const Limit: LongInt);
begin
     if B > Limit then
          Dec (B);
end;

procedure DecLim (var B: LongWord; const Limit: LongWord);
begin
     if B > Limit then
          Dec (B);
end;

procedure DecLim (var B: Int64; const Limit: Int64);
begin
     if B > Limit then
          Dec (B);
end;

function MaxXY (const X, Y: Byte): Byte;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: ShortInt): ShortInt;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: Word): Word;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: SmallInt): SmallInt;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: LongWord): LongWord;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: LongInt): LongInt;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: Int64): Int64;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: Extended): Extended;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: Double): Double;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MaxXY (const X, Y: Single): Single;
begin
     if X > Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: Byte): Byte;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: ShortInt): ShortInt;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: Word): Word;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: SmallInt): SmallInt;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: LongWord): LongWord;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: LongInt): LongInt;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: Int64): Int64;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: Extended): Extended;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: Double): Double;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MinXY (const X, Y: Single): Single;
begin
     if X < Y then
          Result := X
     else
          Result := Y;
end;

function MaxXYZ (const X, Y, Z: Byte): Byte;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: ShortInt): ShortInt;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: Word): Word;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: SmallInt): SmallInt;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: LongWord): LongWord;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: LongInt): LongInt;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: Int64): Int64;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: Extended): Extended;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: Double): Double;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxXYZ (const X, Y, Z: Single): Single;
begin
     Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: Byte): Byte;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: ShortInt): ShortInt;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: Word): Word;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: SmallInt): SmallInt;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: LongWord): LongWord;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: LongInt): LongInt;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: Int64): Int64;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: Extended): Extended;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: Double): Double;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinXYZ (const X, Y, Z: Single): Single;
begin
     Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: Byte): Byte;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: ShortInt): ShortInt;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: Word): Word;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: SmallInt): SmallInt;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: LongWord): LongWord;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: LongInt): LongInt;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: Int64): Int64;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: Extended): Extended;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: Double): Double;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MaxWXYZ (const W, X, Y, Z: Single): Single;
begin
     Result := W;
     if X > Result then
          Result := X;
     if Y > Result then
          Result := Y;
     if Z > Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: Byte): Byte;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: ShortInt): ShortInt;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: Word): Word;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: SmallInt): SmallInt;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: LongWord): LongWord;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: LongInt): LongInt;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: Int64): Int64;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: Extended): Extended;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: Double): Double;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

function MinWXYZ (const W, X, Y, Z: Single): Single;
begin
     Result := W;
     if X < Result then
          Result := X;
     if Y < Result then
          Result := Y;
     if Z < Result then
          Result := Z;
end;

{--- Special Functions ---}

function InverseGamma (const X: Extended): Extended;
var
     C: array [1..26] of Extended;
     Z: Extended;
     XF: Extended;
     I: Integer;
begin
     C [1] := 1;
     C [2] := 0.5772156649015329;
     C [3] := -0.6558780715202538;
     C [4] := -0.0420026350340952;
     C [5] := 0.1665386113822915;
     C [6] := -0.0421977345555443;
     C [7] := -0.0096219715278770;
     C [8] := 0.0072189432466630;
     C [9] := -0.0011651675918591;
     C [10] := -0.0002152416741149;
     C [11] := 0.0001280502823882;
     C [12] := -0.0000201348547807;
     C [13] := -0.0000012504934821;
     C [14] := 0.0000011330272320;
     C [15] := -0.0000002056338417;
     C [16] := 0.0000000061160950;
     C [17] := 0.0000000050020075;
     C [18] := -0.0000000011812746;
     C [19] := 0.0000000001043427;
     C [20] := 0.0000000000077823;
     C [21] := -0.0000000000036968;
     C [22] := 0.0000000000005100;
     C [23] := -0.0000000000000206;
     C [24] := -0.0000000000000054;
     C [25] := 0.0000000000000014;
     C [26] := 0.0000000000000001;
     Result := 0;
     Z := 1;
     XF := Frac (X);
     if XF = 0 then
          XF := ESBSign (X);
     for I := 1 to 26 do
     begin
          Z := Z * XF;
          Result := Result + C [I] * Z;
     end;
     if X > 0 then
     begin
          while XF < X do
          begin
               Result := Result / XF;
               XF := XF + 1;
          end;
     end
     else if X < 0 then
     begin
          while XF > X do
          begin
               XF := XF - 1;
               Result := XF * Result;
          end;
     end
end;

function Gamma (const X: Extended): Extended;
var
     Z: Extended;
begin
     if FloatIsZero (X) or (FloatIsNegative (X) and SameFloat (X, Int (X))) then
          raise EMathError.Create (rsNotDefinedForValue);

     Z := InverseGamma (X);

     if FloatIsZero (Z) then
          raise EMathError.Create (rsNotDefinedForValue);

     Result := 1 / Z;
end;

{ Logarithm to base e of the gamma function.

  Accurate to about 1.e-14.
  Programmer: Alan Miller

  Latest revision of Fortran 77 version - 28 February 1988
}

function LnGamma (const X: Extended): Extended;
const
     A1 = -4.166666666554424E-02;
     A2 = 2.430554511376954E-03;
     A3 = -7.685928044064347E-04;
     A4 = 5.660478426014386E-04;
var
     Temp, Arg, Product: Extended;
     Reflect: Boolean;
begin
     //  lngamma is not defined if x = 0 or a negative integer.
     if FloatIsZero (X) or (FloatIsNegative (X) and SameFloat (X, Int (X))) then
          raise EMathError.Create (rsNotDefinedForValue);

     // If X < 0, use the reflection formula:
     //        gamma(x) * gamma(1-x) = pi * cosec(pi.x)

     Reflect := X < 0.0;
     if Reflect then
          Arg := 1.0 - X
     else
          Arg := X;

     // Increase the argument, if necessary, to make it > 10.

     Product := 1.0;
     while (Arg <= 10.0) do
     begin
          Product := Product * Arg;
          Arg := Arg + 1.0;
     end;

     // Use a polynomial approximation to Stirling's formula.
     // N.B. The real Stirling's formula is used here, not the simpler, but less
     //     accurate formula given by De Moivre in a letter to Stirling, which
     //     is the one usually quoted.

     Arg := Arg - 0.5;
     Temp := 1.0 / Sqr (Arg);
     Result := LnRt2Pi + Arg * (Ln (Arg) - 1.0 +
          (((A4 * Temp + A3) * Temp + A2) * Temp + A1) * Temp) - Ln (Product);

     if Reflect then
     begin
          Temp := Sin (ESBPi * X);
          Result := Ln (ESBPi / Temp) - Result;
     end;
end;

function Beta (const X, Y: Extended): Extended;
var
     R1, R2: Extended;
begin
     if FloatIsZero (X) or (FloatIsNegative (X) and SameFloat (X, Int (X))) then
          raise EMathError.Create (rsNotDefinedForValue);
     if FloatIsZero (Y) or (FloatIsNegative (Y) and SameFloat (Y, Int (Y))) then
          raise EMathError.Create (rsNotDefinedForValue);

     R1 := InverseGamma (X);
     R2 := InverseGamma (Y);

     if FloatIsZero (R1) or FloatIsZero (R2) then
          raise EMathError.Create (rsNotDefinedForValue);

     Result := InverseGamma (X + Y) / (R1 * R2);
end;

function LnBeta (const X, Y: Extended): Extended;
begin
     if not FloatIsPositive (X) or not FloatIsPositive (Y) then
          raise EMathError.Create (rsNotDefinedForValue);

     Result := LnGamma (X) + LnGamma (Y) - LnGamma (X + Y);
end;

function IncompleteBeta (X: Extended; P, Q: Extended): Extended;
{ Adapted From Collected Algorithms from CACM
 Algorithm 179 - Incomplete Beta Function Ratios
 Oliver G Ludwig
}
const
     Epsilon: Extended = 0.5E-18;
     MaxIterations = 1000;
var
     FinSum, InfSum, Temp, Temp1, Term, Term1, QRecur, Index: Extended;
     I: Integer;
     Alter: Boolean;
begin
     if not FloatIsPositive (P) or not FloatIsPositive (Q) or FloatIsNegative (X)
          or GreaterFloat (X, 1) then
     begin
          raise EMathError.Create (rsNotDefinedForValue)
     end;

     if (X = 0) or (X = 1) then
          Result := X
     else
     begin
          // Interchange arguments if necessary to get better convergence
          if X <= 0.5 then
               Alter := False
          else
          begin
               Alter := True;
               SwapXY (P, Q);
               X := 1 - X;
          end;

          // Recurs on the (effective) Q until the Power Series doesn't alternate
          FinSum := 0;
          Term := 1;
          Temp := 1 - X;
          QRecur := Q;
          Index := Q;
          repeat
               Index := Index - 1;
               if Index <= 0 then
                    Break;
               QRecur := Index;
               Term := Term * (QRecur + 1) / (Temp * (P + QRecur));
               FinSum := FinSum + Term;
          until False;

          // Sums a Power Series for non-integral effective Q and yields unity for integer Q
          InfSum := 1;
          Term := 1;
          for I := 1 to MaxIterations do
          begin
               if Term <= Epsilon then
                    Break;
               Index := I;
               Term := Term * X * (Index - QRecur) * (P + Index - 1) /
                    (Index * (P + Index));
               InfSum := InfSum + Term;
          end;

          // Evaluates Gammas
          Temp := Gamma (QRecur);
          Temp1 := Temp;
          Term := Gamma (QRecur + P);
          Term1 := Term;
          Index := QRecur;
          repeat
               Temp1 := Temp1 * Index;
               Term1 := Term1 * (Index + P);
               Index := Index + 1;
          until Index >= Q - 0.5;

          Temp := XtoY (X, P) * (InfSum * Term / (P * Temp) + FinSum * Term1
               * XtoY (1 - X, Q) / (Q * Temp1)) / Gamma (P);

          if Alter then
               Result := 1 - Temp
          else
               Result := Temp
     end;
end;

function Hermite (const X: Extended; const N: LongWord): Extended;
var
     I: LongWord;
     HNplus1, HN, HNminus1: Extended;
begin
     if N = 0 then // H0(x)=1
          Result := 1
     else if N = 1 then //H1(x)=2x
          Result := 2 * X
     else
     begin
          I := 1;
          HN := 2 * X;
          HNminus1 := 1;
          repeat
               Inc (I);
               HNplus1 := 2 * X * HN - 2 * (I - 1) * HNminus1;
               if I <> N then
               begin
                    HNminus1 := HN;
                    HN := HNplus1;
               end;
          until I = N;
          Result := HNPlus1;
     end;
end;

{--- Integer Routines ---}

function GCD (const X, Y: LongWord): LongWord;
asm
	jmp   @01      // We start with EAX <- X, EDX <- Y, and check to see if Y = 0
@00: mov   ecx, edx	// ECX <- EDX prepare for division
	xor   edx, edx // clear EDX for Division
	div   ecx		// EAX <- EDX:EAX div ECX, EDX <- EDX:EAX mod ECX
	mov   eax, ecx	// EAX <- ECX, and repeat if EDX <> 0
@01: and   edx, edx // test to see if EDX is zero, without changing EDX
	jnz   @00		// when EDX is zero EAX has the result
end;

function LCM (const X, Y: LongInt): Int64;
begin
     if (X = 0) or (Y = 0) then
          raise EMathError.Create (rsNotDefinedForValue);
     Result := (x div LongInt (GCD (Abs (X), Abs (Y)))) * Int64 (Y);
end;

function RelativePrime (const X, Y: LongWord): Boolean;
begin
     Result := GCD (X, Y) = 1;
end;

function SumOfSeries (EndValue: LongWord): Int64;
asm
	mov  ecx, eax
	mul  eax       //(edx:eax) := EndValue²
	add  eax, ecx  //(edx:eax) := EndValue² + EndValue
	adc  edx, 0
	shr  edx, 1    //Result := (edx:eax) shr 1
	rcr  eax, 1    //rcr ~ Rotate Carry Right
end;

function SumOfSeries (StartValue, EndValue: LongWord): Int64;
begin
     if EndValue = StartValue then
     begin
          Result := StartValue;
          Exit;
     end
     else if EndValue < StartValue then
     begin
          Result := 0;
          Exit;
     end;
     Result := SumOfSeries (EndValue) - SumOfSeries (StartValue) + StartValue;
end;

function SolveQuadratic (const A, B, C: Extended; out X1, X2: Extended): Byte;
var
     Det, Q: Extended;
begin
     Det := Sqr (B) - 4 * A * C;
     if FloatIsNegative (Det) then
     begin
          X1 := 0;
          X2 := 0;
          Result := 0;
     end
     else if FloatIsZero (A) then
     begin
          if FloatIsZero (B) then
               raise EMathError.Create (rsNotDefinedForValue);

          X1 := -C / B;
          X2 := X1;
          Result := 1;
     end
     else if FloatIsZero (det) then
     begin
          X1 := -0.5 * B / A;
          X2 := X1;
          Result := 1;
     end
     else
     begin
          Det := Sqrt (Det);
          {: Rather than the "traditional" algebraic solution,
           we use a method that improved accuracy, especially
           when either A or C or both are much closer to zero than B,
           then the discriminant is nearly equal to B and one of the
           calculations will involve the subtraction of two nearly equal
           quantities. Thanks to Rory Daulton for this improvement.
          }
          if B < 0 then
               Q := -0.5 * (B - Det)
          else
               Q := -0.5 * (B + Det);
          X1 := Q / A;
          X2 := C / Q;
          Result := 2;
     end;
end;

function ESBDistance (const X1, Y1, X2, Y2: Extended): Extended;
{ Rory Daulton suggested this more tolerant routine }
var
     X, Y: Extended;
begin
     X := Abs (X1 - X2);
     Y := Abs (Y1 - Y2);
     if X > Y then
          Result := X * Sqrt (1 + Sqr (Y / X))
     else if Y <> 0 then
          Result := Y * Sqrt (1 + Sqr (X / Y))
     else
          Result := 0
end;

function RoundDP (const X: Extended; const DecimalPlaces: ShortInt): Extended;
var
     Y: Extended;
begin
     Y := ESBIntPower (10, DecimalPlaces);
     Result := Round (X * Y) / Y;
end;

function AdjustAngleDegree (const X: Extended): Extended;
begin
     Result := X;
     if Result > 360 then
          Result := Result - Int (Result / 360) * 360;
     if Result < 0 then
          Result := Result + Int (abs (Result) / 360 + 1) * 360;
end;

function AdjustAngleRad (const X: Extended): Extended;
begin
     Result := X;
     if Result > TwoPi then
          Result := Result - Int (Result / TwoPi) * TwoPi;
     if Result < 0 then
          Result := Result + Int (abs (Result) / TwoPi + 1) * TwoPi;
end;

end.
