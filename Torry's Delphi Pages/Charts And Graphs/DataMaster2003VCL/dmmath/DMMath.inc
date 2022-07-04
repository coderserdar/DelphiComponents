///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

(*
Include this file into your Delphi project to use DMMath.dll procedures
*)

const
  LinearFitExNumTermsMask=$100; // if set, Fit is TBasis

type // callback prototypes
  Float=Double; // as in LeastSqr.pas
  TProgress=procedure; stdcall;  
  TFunc=function(X: TReal; A: TRealArray; NParam: Integer): TReal; stdcall;
  TDeriv=procedure(X: TReal; A, SigA: TRealArray; Nparam: Integer;
    var Deriv: TRealArray); stdcall;    // set Deriv[i]:=0 if SigA[i]<0!
  TBasis=function(X: Float; Term: integer): Float; stdcall; // used in LinearFitEx

(*
function Parse takes zero-terminated ascii character string as expression
to be evaluated and returns the result (TReal=extended). You must initialize
internal parser object before using Parse(): call AddParserXXX procedures to
add functions (standard or user-defined) and parameters.
*)
function Parse(Expression: PChar): TReal; stdcall; external 'dmmath.dll' name 'Parse';

(*
function NLSFParse makes the same job as Parse(), but uses NLSFParser object instead.
However, it takes 2 additional parameters for internal variables initialization.
*)
function NLSFParse(Expression: PChar; X: TReal; Parameters: TRealArray): TReal; stdcall; external 'dmmath.dll' name 'NLSFParse';

(*
procedure AddParserObject should be used to add function or parameter into the
namespace of the internal parser object. AnObj is the address of the parameter
or function, Name is the symbolic name of this parameter (asciiz string),
and AType is the type of parameter: 0 - function w/o parameters, 1 - function
with one byref "extended" parameter, 2 - function with two byref "extended"
parameters, 4 - extended value (parameter).
At initialization, a set of standard functions automatically added into the
namespace of the internal parser object.
*)
procedure AddParserObject(AnObj: pointer; Name: PChar; AType: integer); stdcall; external 'dmmath.dll' name 'AddParserObject';

(*
function LinearFit performs linear fitting. Parameters are: NumPoints - number
of points in the X, Y arrays; NumTerms - size of caller-allocated Solution
array, depends on the model basis (FitType parameter), which is one of:
0 - Polynomial, 1 - Fourier, 2 - Power, 3 - Exponential, 4 - Lorentzian,
5 - Sine. Output are StandardDeviation, Variance and the result code: 0 - ok,
2 - fitting error, 3 - invalid parameters. X, Y and Solution are pointers to
the arrays of "Float" values.
*)
function LinearFit(NumPoints, NumTerms, FitType: integer; X, Y, Solution: pointer;
  var StandardDeviation, Variance: Float): integer; stdcall; external 'dmmath.dll' name 'LinearFit';

(*
function LinearFitEx works like LinearFit but takes user-defined basis in the Fit parameter.
There are two possible cases:
a) Fit is PChar pointing to the set of expressions divided by CrLf delimiter; the number of
expressions additionally checked by NumTerms. Expression syntax allows only CX parameter,
NLSF Parser object used for basis terms evaluation.
b) Fit is TBasis (NumTerms and LinearFitExNumTermsMask=LinearFitExNumTermsMask).
In this case, LinearFitEx uses callback to evaluate basis functions.
*)
function LinearFitEx(NumPoints, NumTerms: integer; Fit: pointer; X, Y,
  Solution: pointer; var StandardDeviation, Variance: Float): integer; stdcall; external 'dmmath.dll' name 'LinearFitEx';

(*
function LMNLSF performs non-linear Levenberg-Marquardt fitting. Parameters are:
NumPoints - number of TReal in the X, Y and W arrays (X,Y - function, W - weight);
Parameters - initial values/values of fitting parameters; Sigma 0|-1 - parameter
fixation flags (-1 fixes appropriate parameter variation), on return this array
keeps parameter deviations; Expression - fitting function (asciiz); NIter - max
number of iterations, on return - actual number; Progress - callback procedure
called at each iteration; ParDel, ChiDel - parameters and ChiSqr convergence
thresholds; Deriv - used to numerically calculate derivative. Notice that
parameter names in Expression are 'p1'..'p23' and their number determined by
Expression itself; LMNLSF uses internal parser object to evaluate Expression.
*)
function LMNLSF(Expression: PChar; NumPoints, WeightType: integer; X, Y, W: pointer;
  var Parameters, Sigma: TRealArray; var NIter: integer; Progress: TProgress;
  var ChiSqr: TReal; ParDel, ChiDel, Deriv: TReal): integer; stdcall; external 'dmmath.dll' name 'LMNLSF';

(*
procedure AddNLSFParserObject works like AddParserObject, but may be used to add
custom functions to the namespace of the internal parser object, which is
initialized with standard set of functions and names of fitting parameters.
*)
procedure AddNLSFParserObject(AnObj: pointer; Name: PChar; AType: integer); stdcall; external 'dmmath.dll' name 'AddNLSFParserObject';

(*
function LMNLSFEx works as well as LMNLSF. However, it allows to use callback
functions instead of symbolic expression for calculations.
*)
function LMNLSFEx(NumPoints, NumParameters, WeightType: integer; X, Y, W: pointer;
  var Parameters, Sigma: TRealArray; var NIter: integer; Progress: TProgress; Func: TFunc; Deriv: TDeriv;
  var ChiSqr: TReal; ParDel, ChiDel: TReal): integer; stdcall; external 'dmmath.dll' name 'LMNLSFEx';

