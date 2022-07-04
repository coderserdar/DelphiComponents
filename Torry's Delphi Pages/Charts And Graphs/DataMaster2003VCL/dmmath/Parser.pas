{****************************************************************************}
{                      Adapted for Data Master 2003                          }
{             original parser code by G.W. van der Vegt (tfp_01)             }
{****************************************************************************}

unit Parser;

{$B-,F+}

interface

uses Windows{for GetTickCount}, SysUtils;

{$I DMData.inc}

const

  tfp_true  = 1.0;                      {----REAL value for BOOLEAN TRUE     }
  tfp_false = 0.0;                      {----REAL value for BOOLEAN FALSE    }
{$ifdef doublefloat}
  maxreal  = +1e300;
{$else}
  maxreal  = +1e4932;                   {----Internal maxreal                }
{$endif}
  maxparm  = 200;                       {----Maximum number of parameters    }

type
                                        {used internally by TMathParser:}
  tfp_fname = STRING[32];               {----String name                     }
  tfp_ftype = (tfp_noparm,              {----Function or Function()          }
               tfp_1real,               {----Function(VAR r)                 }
               tfp_2real,               {----Function(VAR r1,r2)             }
               tfp_nreal,               {----Function(VAR r;n  INTEGER)      }
               tfp_realvar,             {----Real VAR                        }
               tfp_intvar,              {----Integer VAR                     }
               tfp_boolvar,             {----Boolean VAR                     }
               tfp_realstr);            {----Real String VAR                 }
  fie      = RECORD
               fname : tfp_fname;       {----Name of function or var         }
               faddr : POINTER;         {----FAR POINTER to function or var  }
               ftype : tfp_ftype;       {----Type of entry                   }
             END;
  fieptr   = ARRAY[1..1] OF fie;        {----Will be used as [1..maxfie]     }
  real=TReal;    {redefine!}

  TMathParser=class(TObject)
  private                            {global variables moved from from tfp_01}
    fiearr: ^fieptr;                    {----Array of functions & vars       }
    maxfie: INTEGER;                    {----max no of functions & vars      }
    fiesiz: INTEGER;                    {----current no of functions & vars  }
    Line: STRING;                       {----Internal copy of string to Parse}
    Lp: INTEGER;                        {----Parsing Pointer into Line       }
    Nextchar: CHAR;                     {----Character at Lp Postion         }
    PROCEDURE Newchar;
    PROCEDURE Skip;
    FUNCTION Eval_number : REAL;
    FUNCTION Eval_factor : REAL;
    FUNCTION Eval_term : REAL;
    FUNCTION Eval_subterm : REAL;
    FUNCTION Eval_r_expr : REAL;
    FUNCTION Eval_b_expr : REAL;
  public
    function Parse(S: string): TReal;{evaluates string, may raise exceptions!}
    procedure Init(No: integer);     {allocates memory for params & functions}
    procedure AddObject(a: pointer; n: tfp_fname; t: tfp_ftype);
    procedure AddGonio;      {these procs add standard parameters & functions}
    procedure AddLogic;
    procedure AddMath;
    procedure AddMisc;
    procedure AddSpec;
    procedure AddStdParams(Pars:PRealArray);
    destructor Destroy; override;
  end;

  EMathParser=class(Exception)
    ErrorCode: integer;
    constructor CreateCode(Code: integer);
  end;

  procedure SetErrorCode(Code:byte); {must be called ONLY from user functions}

resourcestring
  errMathParser1='Incorrect numeric format encountered';
  errMathParser2='Undefined function or parameter';
  errMathParser3='Missed "(" or ")"';
  errMathParser4='Only integer power for negative base allowed';
  errMathParser5='TAN((2n+1)*PI/2) is infinite';
  errMathParser6='Unable to evaluate empty expression';
  errMathParser7='Argument of LN must be strictly positive';
  errMathParser8='Argument of SQRT must be positive';
  errMathParser9='Parser detects division by zero';
  errMathParser10='Too many functions and parameters';
  errMathParser11='Parser detects overflow/underflow';
  errMathParser12='Invalid characters in function name';
  errMathParser13='UNEXPECTED ERROR!!!';
  errMathParser14='Boolean expression error';
  errMathParser15='Error evaluating function parameters';

implementation

{---------------------------------------------------------}
{----Tricky stuff to call FUNCTIONS                       }
{---------------------------------------------------------}

VAR
  GluePtr : POINTER;
  Tfp_ernr: byte;

type
  TNoParam=function: real;
  T1Real=function(var R1): real;
  T2Real=function(var R1,R2): real;
  TNReal=function(var R1,N): real;

procedure SetErrorCode(Code:byte);
begin Tfp_ernr:=Code; end;

FUNCTION Call_noparm : REAL;
begin Call_noparm:=TNoParam(GluePtr); end;

FUNCTION Call_1real(VAR r) : REAL;
begin Call_1real:=T1Real(GluePtr)(r); end;

FUNCTION Call_2real(VAR r1,r2) : REAL;
begin Call_2real:=T2Real(GluePtr)(r1,r2); end;

FUNCTION Call_nreal(VAR r,n) : REAL;
begin Call_nreal:=TNReal(GluePtr)(r,n); end;

constructor EMathParser.CreateCode(Code: integer);   {same as parser's codes!}
begin
  ErrorCode:=Code;
  case Code of
    1: Message:=errMathParser1;
    2: Message:=errMathParser2;
    3: Message:=errMathParser3;
    4: Message:=errMathParser4;
    5: Message:=errMathParser5;
    6: Message:=errMathParser6;
    7: Message:=errMathParser7;
    8: Message:=errMathParser8;
    9: Message:=errMathParser9;
    10: Message:=errMathParser10;
    11: Message:=errMathParser11;
    12: Message:=errMathParser12;
    13: Message:=errMathParser13;
    14: Message:=errMathParser14;
    15: Message:=errMathParser15;
  end;
end;

{--- complex: ---}

type
  TComplex=record                                         {complex value type}
    X,Y: TReal;
  end;

function  StrC(Z: TComplex): string;                          {returns s=x+iy}
var S, Ss: string;
begin Str(Z.X, S); Str(Z.Y, Ss); StrC:=S+'+i'+Ss; end;

function  ModuleC(Z: TComplex): TReal;                           {returns |Z|}
begin ModuleC:=Sqrt(Sqr(Z.X)+Sqr(Z.Y)); end;

function  ArgC(Z: TComplex): TReal;                           {returns arg(Z)}
begin
  if Z.X<>0 then ArgC:=Arctan(Z.Y/Z.X) else         {else-value is imaginary!}
  begin
    if Z.Y<0 then ArgC:=-Pi/2;
    if Z.Y>0 then ArgC:=Pi/2; if Z.Y=0 then ArgC:=0;
  end;
end;

function MakeC(X,Y: TReal): TComplex;                                {Z:=X+iY}
begin Result.X:=X; Result.Y:=Y; end;

function CreateC(X,Y: TReal): TComplex;                          {Z:=Xexp(iY)}
begin Result.X:=X*cos(Y); Result.Y:=X*sin(Y); end;

function OneC: TComplex;                                       {returns  1+i0}
begin Result.X:=1; Result.Y:=0; end;

function AddC(X,Y: TComplex): TComplex;                               {Z:=X+Y}
begin Result.X:=X.X+Y.X; Result.Y:=X.Y+Y.Y; end;

function SubC(X,Y: TComplex): TComplex;                               {Z:=X-Y}
begin Result.X:=X.X-Y.X; Result.Y:=X.Y-Y.Y; end;

function MulC(X,Y: TComplex): TComplex;                               {Z:=X*Y}
begin Result.X:=X.X*Y.X-X.Y*Y.Y; Result.Y:=X.X*Y.Y+X.Y*Y.X; end;

function DivC(X,Y: TComplex): TComplex;                               {Z:=X/Y}
begin                                                    {Z1/Z2=Z1*_Z2/|Z2|^2}
{Result:=DivC(MulC(X,ConjC(Y)),MakeC(Sqr(Module(Y)),0)); may cause stack ovf!}
  Result.X:=(X.X*Y.X+X.Y*Y.Y)/(sqr(Y.X)+sqr(Y.Y));
  Result.Y:=(Y.X*X.Y-X.X*Y.Y)/(sqr(Y.X)+sqr(Y.Y));
end;

function ConjC(X: TComplex): TComplex;                        {Y:=conjugate X}
begin Result.X:=X.X; Result.Y:=-X.Y; end;

function SinC(X: TComplex): TComplex;                              {Y:=sin(X)}
begin
  Result.X:=sin(X.X)*(exp(X.Y)+exp(-X.Y))/2;
  Result.Y:=cos(X.X)*(exp(X.Y)-exp(-X.Y))/2;
end;

function CosC(X: TComplex): TComplex;                              {Y:=cos(X)}
begin
  Result.X:=cos(X.X)*(exp(X.Y)+exp(-X.Y))/2;
  Result.Y:=sin(X.X)*(exp(-X.Y)-exp(X.Y))/2;
end;

function TanC(X: TComplex): TComplex;                              {Y:=tan(X)}
begin Result:=DivC(SinC(X), CosC(X)); end;

function ExpC(X: TComplex): TComplex;                              {Y:=exp(X)}
begin Result.X:=exp(X.X)*cos(X.Y); Result.Y:=exp(X.X)*sin(X.Y); end;

function LnC(X: TComplex): TComplex;                                {Y:=Ln(X)}
begin Result.X:=ln(Abs(ModuleC(X))); Result.Y:=ArgC(X); end;

function SqrC(X: TComplex): TComplex;                              {Y:=Sqr(X)}
begin Result:=MulC(X,X); end;

function SqrtC(X: TComplex): TComplex;                            {Y:=sqrt(X)}
begin Result:=CreateC(Sqrt(ModuleC(X)), ArgC(X)/2); end;

function ShC(X: TComplex): TComplex;                                {Y:=sh(X)}
begin
  Result:=MulC(MakeC(0,-1),SinC(MulC(X,MakeC(0,1))));       {sh(X)=-i*sin(iX)}
end;

function ChC(X: TComplex): TComplex;                                {Y:=ch(X)}
begin Result:=CosC(MulC(X,MakeC(0,1))); end;                   {ch(X)=cos(iX)}

function ThC(X: TComplex): TComplex;                                {Y:=th(X)}
begin Result:=DivC(ShC(X),ChC(X)); end;

function InvC(X: TComplex): TComplex;                                 {Y:=1/X}
begin Result:=DivC(OneC,X); end;

{from PROCESS.PAS:}
{Interpolation, VDP and cable compensation routines: Gamma and Open/Short}
function LineInterpolate(X1, X2, Y1, Y2, X: TReal): TReal;
begin                 {linear interpolation: Y:=Yk+(X-Xk)/(Xk+1-Xk)*(Yk+1-Yk)}
  if X1=X2 then LineInterpolate:=Y1+Y2/2         {NOTE! process zero interval}
  else LineInterpolate:=Y1+(X-X1)/(X2-X1)*(Y2-Y1);
end;

function VDP(Ra,Rb: TReal): TReal;                     {return VDP formfactor}
  procedure Proc(r, y0: TReal; var d, f1:TReal);
  var v, rv, a: TReal;
  begin
    rv:=r*y0; v:=y0; v:=exp(v); rv:=exp(rv); a:=rv+1/rv-v;
    d:=r*(rv-1/rv)-v; f1:=a;
  end;
var r, y0, y1, d, f0, f1: TReal;
begin
  if ((Ra=0) or (Rb=0)) then begin Result:=1; Exit; end;  {superconducor: f=1}
  r:=(Ra-Rb)/(Ra+Rb); if r<0 then r:=-r; {<-- no need do ABS!!!} y0:=1;
  Proc(r, y0, d, f0);
  repeat
    y1:=y0-f0/d; Proc(r, y1, d, f1); y0:=y1; f0:=f1;
  until (abs(f1)<1.e-6);
  Result:=y1;
end;

function GCompensation(M,Fi,X,Y: TReal): TComplex;              {Gx,Gy->Zx,Zy}
var G,Z: TComplex;                                            {Gtrue=G*Gcable}
begin                                                       {Z/Zi=(1+G)/(1-G)}
  G:=MulC(CreateC(M,Fi),MakeC(X,Y)); Z:=DivC(AddC(OneC, G), SubC(OneC, G));
  Z.X:=50*Z.X; Z.Y:=50*Z.Y; Result:=Z;
end;

function OSCompensation(ZoX,ZoY,ZsX,ZsY,ZX,ZY: TReal): TComplex;
begin                               {O/S comp. routine: Ztrue=(Zs-Z)/(Z/Zo-1)}
  Result:=DivC(SubC(MakeC(ZsX, ZsY), MakeC(ZX, ZY)),
               SubC(DivC(MakeC(ZX, ZY),MakeC(ZoX, ZoY)), OneC));
end;

{--- TMathParser ---}
PROCEDURE TMathParser.AddStdParams(Pars:PRealArray);
var i: integer;
BEGIN
  for i:=low(TColIndex) to High(TColIndex) do
  AddObject(@pars^[i], char(byte('A')+I-low(TColIndex)), tfp_realvar);
END;

function xGCX(var R: TReal; var N: integer): TReal;                {Gx,Gy->Zx}
var Ar: TRealArray absolute R;                            {params: M,Fi,Gx,Gy}
begin             {note! after getting |M|,Fi of cable we need use 1/|M|, -Fi}
  if N=3 then Result:=GCompensation(Ar[1],Ar[2],Ar[3],Ar[4]).X
  else tfp_ernr:=15;
end;

function xGCY(var R: TReal; var N: integer): TReal;                {Gx,Gy->Zy}
var Ar: TRealArray absolute R;
begin
  if N=3 then Result:=GCompensation(Ar[1],Ar[2],Ar[3],Ar[4]).Y
  else tfp_ernr:=15;
end;

function xOSCX(var R: TReal; var N: integer): TReal;      {rets real part (X)}
var Ar: TRealArray absolute R;
begin                                          {params: ZoX,ZoY,ZsX,ZsY,ZX,ZY}
  if N=5 then Result:=OSCompensation(Ar[1],Ar[2],Ar[3],Ar[4],Ar[5],Ar[6]).X
  else tfp_ernr:=15;
end;

function xOSCY(var R: TReal; var N: integer): TReal;
var Ar: TRealArray absolute R;
begin                                          {params: ZoX,ZoY,ZsX,ZsY,ZX,ZY}
  if N=5 then Result:=OSCompensation(Ar[1],Ar[2],Ar[3],Ar[4],Ar[5],Ar[6]).Y
  else tfp_ernr:=15;
end;

function xInvCX(var X,Y: TReal): TReal;
begin Result:=InvC(MakeC(X,Y)).X; end;

function xInvCY(var X,Y: TReal): TReal;
begin Result:=InvC(MakeC(X,Y)).Y; end;

{other:}
function xTime: TReal;
begin Result:=GetTickCount/1e3; end;

{Van der Paw Ro calculation}
function xf_VDP(var Ra,Rb: TReal): TReal;               {returns formfactor f}
begin Result:=VDP(Ra,Rb); end;

function xRo_VDP(var R: TReal; var N: integer): TReal;     {Ra,Rb[Ohm]; d[cm]}
var Ar: TRealArray absolute R;
begin
{NOTE! parameters are enumerated 0..N!!! i.e., n+1}
  if N=2 then Result:=Pi*Ar[3]*(Ar[1]+Ar[2])/2/xf_VDP(Ar[1],Ar[2])
  else tfp_ernr:=15;
end;

{ADD routine:}
procedure TMathParser.AddSpec;
begin
  addobject(@xTime,'Time',tfp_noparm);
  addobject(@xf_VDP,'f_VDP',tfp_2real);
  addobject(@xRo_VDP,'Ro_VDP',tfp_nreal);
  addobject(@xGCX,'GCX',tfp_nreal);
  addobject(@xGCY,'GCY',tfp_nreal);
  addobject(@xOSCX,'OSCX',tfp_nreal);
  addobject(@xOSCY,'OSCY',tfp_nreal);
  addobject(@xInvCX,'InvCX',tfp_2real);
  addobject(@xInvCY,'InvCY',tfp_2real);
end;

{============================================================================}
{======= all the rest of unit copied directly from tfp_01 ===================}
{============================================================================}
{---------------------------------------------------------}
{----This routine skips one character                     }
{---------------------------------------------------------}

PROCEDURE TMathParser.Newchar;

BEGIN
  IF (lp<LENGTH(Line))
    THEN INC(Lp);
  Nextchar:=UPCASE(Line[Lp]);
END;

{---------------------------------------------------------}
{----This routine skips one character and                 }
{    all folowing spaces from an expression               }
{---------------------------------------------------------}

PROCEDURE TMathParser.Skip;

BEGIN
  REPEAT
    Newchar;
  UNTIL (Nextchar<>' ');
END;

{---------------------------------------------------------}
{  Number     = Real    (Bv 23.4E-5)                      }
{               Integer (Bv -45)                          }
{---------------------------------------------------------}

FUNCTION TMathParser.Eval_number : REAL;

VAR
  Temp  : STRING;
  Err   : INTEGER;
  value : REAL;

BEGIN
{----Correct .xx to 0.xx}
  IF (Nextchar='.')
    THEN Temp:='0'+Nextchar
    ELSE Temp:=Nextchar;

  Newchar;

{----Correct ñ.xx to ñ0.xx}
  IF (LENGTH(temp)=1) AND (Temp[1] IN ['+','-']) AND (Nextchar='.')
    THEN Temp:=Temp+'0';

  WHILE Nextchar IN ['0'..'9','.','E'] DO
    BEGIN
      Temp:=Temp+Nextchar;
      IF (Nextchar='E')
        THEN
          BEGIN
          {----Correct ñxxx.E to ñxxx.0E}
            IF (Temp[LENGTH(Temp)-1]='.')
              THEN INSERT('0',Temp,LENGTH(Temp));
            Newchar;
            IF (Nextchar IN ['+','-'])
              THEN
                BEGIN
                  Temp:=Temp+Nextchar;
                  Newchar;
                END;
          END
        ELSE Newchar;
    END;

{----Skip trailing spaces}
  IF (line[lp]=' ')
    THEN WHILE (Line[lp]=' ') DO INC(lp);
  nextchar:=line[lp];

{----Correct ñxx. to ñxx.0 but NOT ñxxEñyy.}
  IF (temp[LENGTH(temp)]='.') AND
     (POS('E',temp)=0)
    THEN Temp:=Temp+'0';

  VAL(Temp,value,Err);

  IF (Err<>0) THEN tfp_ernr:=1;

  IF (tfp_ernr=0)
    THEN Eval_number:=value
    ELSE Eval_number:=0;
END;

{---------------------------------------------------------}
{  Factor     = Number                                    }
{    (External) Function()                                }
{    (External) Function(Expr)                            }
{    (External) Function(Expr,Expr)                       }
{     External  Var Real                                  }
{     External  Var Integer                               }
{     External  Var Boolean                               }
{     External  Var realstring                            }
{               (R_Expr)                                  }
{---------------------------------------------------------}

FUNCTION TMathParser.Eval_factor : REAL;

VAR
  ferr    : BOOLEAN;
  param   : INTEGER;
  dummy   : ARRAY[0..maxparm] OF REAL;
  value,
  dummy1,
  dummy2  : REAL;
  temp    : tfp_fname;
  e,
  index   : INTEGER;
  temps   : STRING;

BEGIN
  CASE Nextchar OF
    '+'  : BEGIN
             Newchar;
             value:=+Eval_factor;
           END;
    '-'  : BEGIN
             Newchar;
             value:=-Eval_factor;
           END;

    '0'..'9',
    '.'  : value:=Eval_number;
    'A'..'Z'
         : BEGIN
             ferr:=TRUE;
             Temp:=Nextchar;
             Skip;
             WHILE Nextchar IN ['0'..'9','_','A'..'Z'] DO
               BEGIN
                 Temp:=Temp+Nextchar;
                 Skip;
               END;

           {----Seek function and CALL it}
             {$R-}
             FOR Index:=1 TO Fiesiz DO
               WITH fiearr^[index] DO
                 IF (fname=temp)
                   THEN
                     BEGIN
                       ferr:=FALSE;

                       CASE ftype OF

                       {----Function or Function()}
                         tfp_noparm  : IF (nextchar='(')
                                        THEN
                                          BEGIN
                                            Skip;

                                            IF (nextchar<>')')
                                              THEN tfp_ernr:=15;

                                            Skip;
                                          END;

                       {----Function(r)}
                         tfp_1real   : IF (nextchar='(')
                                         THEN
                                           BEGIN
                                             Skip;

                                             dummy1:=Eval_b_expr;

                                             IF (tfp_ernr=0) AND
                                                (nextchar<>')')
                                               THEN tfp_ernr:=15;

                                             Skip; {----Dump the ')'}
                                           END
                                         ELSE tfp_ernr:=15;

                       {----Function(r1,r2)}
                         tfp_2real   : IF (nextchar='(')
                                         THEN
                                           BEGIN
                                             Skip;

                                             dummy1:=Eval_b_expr;

                                             IF (tfp_ernr=0) AND
                                                (nextchar<>',')
                                               THEN tfp_ernr:=15;

                                             Skip; {----Dump the ','}
                                             dummy2:=Eval_b_expr;

                                              IF (tfp_ernr=0) AND
                                                 (nextchar<>')')
                                                THEN tfp_ernr:=15;

                                              Skip; {----Dump the ')'}
                                            END
                                          ELSE tfp_ernr:=15;

                       {----Function(r,n)}
                         tfp_nreal   : IF (nextchar='(')
                                         THEN
                                           BEGIN
                                             param:=0;

                                             Skip;
                                             dummy[param]:=Eval_b_expr;

                                             IF (tfp_ernr=0) AND
                                                (nextchar<>',')
                                               THEN tfp_ernr:=15
                                               ELSE
                                                 WHILE (tfp_ernr=0) AND
                                                       (nextchar=',') AND
                                                       (param<maxparm) DO
                                                   BEGIN
                                                     Skip; {----Dump the ','}
                                                     INC(param);
                                                     dummy[param]:=Eval_b_expr;
                                                   END;

                                             IF (tfp_ernr=0) AND
                                                (nextchar<>')')
                                               THEN tfp_ernr:=15;

                                             Skip; {----Dump the ')'}
                                           END
                                         ELSE tfp_ernr:=15;
                       {----Real Var}
                         tfp_realvar    : dummy1:=REAL(faddr^);

                       {----Integer Var}
                         tfp_intvar     : dummy1:=1.0*INTEGER(faddr^);

                       {----Boolean Var}
                         tfp_boolvar    : dummy1:=1.0*ORD(BOOLEAN(faddr^));

                       {----Real string Var}
                         tfp_realstr    : BEGIN
                                             temps:=STRING(faddr^);

                                           {----Delete Leading Spaces}
                                             WHILE (Length(temps)>0) AND
                                                   (temps[1]=' ') DO
                                               Delete(temps,1,1);

                                           {----Delete Trailing Spaces}
                                             WHILE (Length(temps)>0) AND
                                                   (temps[Length(temps)]=' ') Do
                                               Delete(temps,Length(temps),1);

                                          {----Correct .xx to 0.xx}
                                             IF (LENGTH(temps)>=1)  AND
                                                (LENGTH(temps)<255) AND
                                                (temps[1]='.')
                                               THEN Insert('0',temps,1);

                                           {----Correct ñ.xx to ñ0.xx}
                                             IF (LENGTH(temps)>=2) AND
                                                (LENGTH(temps)<255) AND
                                                (temps[1] IN ['+','-']) AND
                                                (temps[2]='.')
                                               THEN Insert('0',temps,2);

                                           {----Correct xx.Eyy to xx0.Exx}
                                             IF (Pos('.E',temps)>0) AND
                                                (Length(temps)<255)
                                               THEN Insert('0',temps,Pos('.E',temps));

                                           {----Correct xx.eyy to xx0.exx}
                                             IF (Pos('.e',temps)>0) AND
                                                (Length(temps)<255)
                                               THEN Insert('0',temps,Pos('.e',temps));
                                           {----Correct ñxx. to ñxx.0 but NOT ñ}
                                             IF (temps[LENGTH(temps)]='.') AND
                                                (POS('E',temps)=0) AND
                                                (POS('e',temps)=0) AND
                                                (Length(temps)<255)
                                               THEN Temps:=Temps+'0';

                                             VAL(temps,dummy1,e);
                                             IF (e<>0)
                                               THEN tfp_ernr:=1;
                                           END;
                       END;

                       IF (tfp_ernr=0)
                         THEN
                           BEGIN
                             glueptr:=faddr;

                             CASE ftype OF
                               tfp_noparm   : value:=call_noparm;
                               tfp_1real    : value:=call_1real(dummy1);
                               tfp_2real    : value:=call_2real(dummy1,dummy2);
                               tfp_nreal    : value:=call_nreal(dummy,param);
                               tfp_realvar,
                               tfp_intvar,
                               tfp_boolvar,
                               tfp_realstr  : value:=dummy1;
                             END;
                           END;
                     END;
             IF (ferr=TRUE)
               THEN tfp_ernr:=2;

             {$R+}
           END;

    '('  : BEGIN
             Skip;

             value:=Eval_b_expr;

             IF (tfp_ernr=0) AND (nextchar<>')') THEN tfp_ernr:=3;

             Skip; {----Dump the ')'}
           END;

    ELSE tfp_ernr:=2;
  END;

  IF (tfp_ernr=0)
    THEN Eval_factor:=value
    ELSE Eval_factor:=0;

END;

{---------------------------------------------------------}
{  Term       = Factor ^ Factor                           }
{---------------------------------------------------------}

FUNCTION TMathParser.Eval_term : REAL;

VAR
  value,
  Exponent,
  dummy,
  Base      : REAL;

BEGIN
  value:=Eval_factor;

  WHILE (tfp_ernr=0) AND (Nextchar='^') DO
    BEGIN
      Skip;

      Exponent:=Eval_factor;

      Base:=value;
      IF (tfp_ernr=0) AND (Base=0)
        THEN value:=0
        ELSE
          BEGIN

          {----Over/Underflow Protected}
            dummy:=Exponent*LN(ABS(Base));
            IF (dummy<=LN(MAXREAL))
               THEN value:=EXP(dummy)
               ELSE tfp_ernr:=11;
          END;

      IF (tfp_ernr=0) AND (Base<0)
        THEN
          BEGIN
          {----allow only whole number exponents}
            IF (INT(Exponent)<>Exponent) THEN tfp_ernr:=4;

            IF (tfp_ernr=0) AND ODD(ROUND(exponent)) THEN value:=-value;
          END;
    END;

  IF (tfp_ernr=0)
    THEN Eval_term:=value
    ELSE Eval_term:=0;
END;

{---------------------------------------------------------}
{----Subterm  = Term * Term                               }
{               Term / Term                               }
{---------------------------------------------------------}

FUNCTION TMathParser.Eval_subterm : REAL;

VAR
  value,
  dummy  : REAL;

BEGIN
  value:=Eval_term;

  WHILE (tfp_ernr=0) AND (Nextchar IN ['*','/']) DO
    CASE Nextchar OF

    {----Over/Underflow Protected}
      '*' : BEGIN
              Skip;

              dummy:=Eval_term;

              IF (tfp_ernr<>0) OR (value=0) OR (dummy=0)
                THEN value:=0
                ELSE IF (ABS( LN(ABS(value)) + LN(ABS(dummy)) )<LN(Maxreal))
                  THEN value:= value * dummy
                  ELSE tfp_ernr:=11;
            END;

    {----Over/Underflow Protected}
      '/' : BEGIN
              Skip;

              dummy:=Eval_term;

              IF (tfp_ernr=0)
                THEN
                  BEGIN

                  {----Division by ZERO Protected}
                    IF (dummy<>0)
                      THEN
                        BEGIN
                        {----Underflow Protected}
                          IF (value<>0)
                            THEN
                              IF (ABS( LN(ABS(value))-LN(ABS(dummy)) )
                                 <LN(Maxreal))
                                THEN value:=value/dummy
                                ELSE tfp_ernr:=11
                        END
                      ELSE tfp_ernr:=9;
                  END;
            END;
    END;

  IF (tfp_ernr=0)
    THEN Eval_subterm:=value
    ELSE Eval_subterm:=0;
END;

{---------------------------------------------------------}
{  Real Expr  = Subterm + Subterm                         }
{               Subterm - Subterm                         }
{---------------------------------------------------------}

FUNCTION TMathParser.Eval_r_expr : REAL;

VAR
  dummy,
  dummy2,
  value : REAL;

BEGIN
  value:=Eval_subterm;

  WHILE (tfp_ernr=0) AND (Nextchar IN ['+','-']) DO
    CASE Nextchar OF

      '+' : BEGIN
              Skip;

              dummy:=Eval_subterm;

              IF (tfp_ernr=0)
                THEN
                  BEGIN

                  {----Overflow Protected}
                    IF (ABS( (value/10)+(dummy/10) )<(Maxreal/10))
                      THEN value:=value+dummy
                      ELSE tfp_ernr:=11;
                  END;
            END;

      '-' : BEGIN
              Skip;
              dummy2:=value;

              dummy:=Eval_subterm;

              IF (tfp_ernr=0)
                THEN
                  BEGIN

                  {----Overflow Protected}
                    IF (ABS( (value/10)-(dummy/10) )<(Maxreal/10))
                      THEN value:=value-dummy
                      ELSE tfp_ernr:=11;

                  {----Underflow Protected}
                    IF (value=0) AND (dummy<>dummy2)
                      THEN tfp_ernr:=11;
                  END;

            END;
    END;

{----At this point the current char must be
        1. the EOLN marker or
        2. a right bracket
        3. start of a boolean operator }

  IF NOT (Nextchar IN [#00,')','>','<','=',','])
    THEN tfp_ernr:=2;

  IF (tfp_ernr=0)
    THEN Eval_r_expr:=value
    ELSE Eval_r_expr:=0;
END;

{---------------------------------------------------------}
{  Boolean Expr  = R_Expr <  R_Expr                       }
{                  R_Expr <= R_Expr                       }
{                  R_Expr <> R_Expr                       }
{                  R_Expr =  R_Expr                       }
{                  R_Expr >= R_Expr                       }
{                  R_Expr >  R_Expr                       }
{---------------------------------------------------------}

FUNCTION TMathParser.Eval_b_expr : REAL;

VAR
  value : REAL;

BEGIN
  value:=Eval_r_expr;

  IF (tfp_ernr=0) AND (Nextchar IN ['<','>','='])
    THEN
      CASE Nextchar OF

        '<' : BEGIN
                Skip;
                IF (Nextchar IN ['>','='])
                  THEN
                    CASE Nextchar OF
                      '>' : BEGIN
                              Skip;
                              IF (value<>Eval_r_expr)
                                THEN value:=tfp_true
                                ELSE value:=tfp_false;
                            END;
                      '=' : BEGIN
                              Skip;
                              IF (value<=Eval_r_expr)
                                THEN value:=tfp_true
                                ELSE value:=tfp_false;
                            END;
                    END
                  ELSE
                    BEGIN
                      IF (value<Eval_r_expr)
                        THEN value:=tfp_true
                        ELSE value:=tfp_false;
                    END;
              END;

        '>' : BEGIN
                Skip;
                IF (Nextchar='=')
                  THEN
                    BEGIN
                      Skip;
                      IF (value>=Eval_r_expr)
                        THEN value:=tfp_true
                        ELSE value:=tfp_false;
                    END
                  ELSE
                    BEGIN
                      IF (value>Eval_r_expr)
                        THEN value:=tfp_true
                        ELSE value:=tfp_false;
                    END;
              END;
        '=' : BEGIN
                Skip;
                IF (value=Eval_r_expr)
                  THEN value:=tfp_true
                  ELSE value:=tfp_false;
              END;
      END;

  IF (tfp_ernr=0)
    THEN Eval_b_expr:=value
    ELSE Eval_b_expr:=0.0;
END;

{---------------------------------------------------------}
{----Internal Functions                                   }
{---------------------------------------------------------}

{$F+}
FUNCTION xABS(VAR r : REAL) : REAL;

BEGIN
 xabs:=ABS(r);
END;

FUNCTION xAND(VAR r;VAR n : INTEGER) : REAL;

TYPE
  tmp   = ARRAY[0..0] OF REAL;

VAR
  x     : REAL;
  i     : INTEGER;

BEGIN
{$R-}
  FOR i:=0 TO n DO
    IF (tmp(r)[i]<>tfp_false) AND (tmp(r)[i]<>tfp_true)
      THEN
        BEGIN
          IF (tfp_ernr=0)
            THEN tfp_ernr:=14;
        END;
   IF (tfp_ernr=0) AND (n>0)
     THEN
       BEGIN
         x:=tfp_true*ORD(tmp(r)[0]=tfp_true);
         FOR i:=1 TO n DO
           x:=tfp_true*ORD((x=tfp_true) AND (tmp(r)[i]=tfp_true))
       END
     ELSE tfp_ernr:=15;
  IF tfp_ernr=0
    THEN xAND:=x
    ELSE xAND:=0.0;
{$R+}
END;

FUNCTION xARCTAN(VAR r : REAL) : REAL;

BEGIN
  xARCTAN:=ARCTAN(r);
END;

FUNCTION xCOS(VAR r : REAL) : REAL;

BEGIN
  xCOS:=COS(r);
END;

FUNCTION xDEG(VAR r : REAL) : REAL;

BEGIN
  xDEG:=(r/pi)*180;
END;

{FUNCTION xE : REAL;

BEGIN
  xE:=EXP(1);
END;}

FUNCTION xEXP(VAR r : REAL) : REAL;

BEGIN
  xEXP:=0;
  IF (ABS(r)<LN(MAXREAL))
    THEN xEXP:=EXP(r)
    ELSE tfp_ernr:=11;
END;

FUNCTION xFALSE : REAL;

BEGIN
  xFALSE:=tfp_false;
END;

FUNCTION xFRAC(VAR r : REAL) : REAL;

BEGIN
  xFRAC:=FRAC(r);
END;

FUNCTION xINT(VAR r : REAL) : REAL;

BEGIN
  xINT:=INT(r);
END;

FUNCTION xLN(VAR r : REAL) : REAL;

BEGIN
  xLN:=0;
  IF (r>0)
    THEN xLN:=LN(r)
    ELSE tfp_ernr:=7;
END;

FUNCTION xLOG(VAR r : REAL) : REAL;

BEGIN
  xLOG:=0;
  IF (r>0)
    THEN xLOG:=LN(r)/LN(10)
    ELSE tfp_ernr:=7;
END;

FUNCTION xMAX(VAR r;VAR n : INTEGER) : REAL;

TYPE
  tmp   = ARRAY[0..0] OF REAL;

VAR
  max   : REAL;
  i     : INTEGER;

BEGIN
{$R-}
  max:=tmp(r)[0];
  FOR i:=1 TO n DO
    IF (tmp(r)[i]>max)
      THEN max:=tmp(r)[i];
  xMAX:=max;
{$R+}
END;

FUNCTION xMIN(VAR r;VAR n : INTEGER) : REAL;

TYPE
  tmp   = ARRAY[0..0] OF REAL;

VAR
  min   : REAL;
  i     : INTEGER;

BEGIN
{$R-}
  min:=tmp(r)[0];
  FOR i:=1 TO n DO
    IF (tmp(r)[i]<min)
      THEN min:=tmp(r)[i];
  xMIN:=min;
{$R+}
END;
FUNCTION xIOR(VAR r;VAR n : INTEGER) : REAL;

TYPE
  tmp   = ARRAY[0..0] OF REAL;

VAR
  x     : REAL;
  i     : INTEGER;

BEGIN
{$R-}
  FOR i:=0 TO n DO
    IF (tmp(r)[i]<>tfp_false) AND (tmp(r)[i]<>tfp_true)
      THEN
        BEGIN
          IF (tfp_ernr=0)
            THEN tfp_ernr:=14;
        END;
   IF (tfp_ernr=0) AND (n>0)
     THEN
       BEGIN
         x:=tfp_true*ORD(tmp(r)[0]=tfp_true);
         FOR i:=1 TO n DO
           x:=tfp_true*ORD((x=tfp_true) OR (tmp(r)[i]=tfp_true))
       END
     ELSE tfp_ernr:=15;
  IF tfp_ernr=0
    THEN xIOR:=x
    ELSE xIOR:=0.0;
{$R+}
END;

FUNCTION xPI : REAL;

BEGIN
  xPI:=PI;
END;

function xRANDOM: real;           {added by RRR; rets 0..1}
begin xRANDOM:=RANDOM; end;

FUNCTION xRAD(VAR r : REAL) : REAL;

BEGIN
  xRAD:=(r/180)*pi;
END;

FUNCTION xROUND(VAR r : REAL) : REAL;

BEGIN
  xROUND:=ROUND(r);
END;

FUNCTION xSGN(VAR r : REAL) : REAL;

BEGIN
  IF (r>=0)
    THEN xSgn:=+1
    ELSE xSgn:=-1;
END;

FUNCTION xSIN(VAR r : REAL) : REAL;

BEGIN
  xSIN:=SIN(r);
END;

FUNCTION xSQR(VAR r : REAL) : REAL;

BEGIN
  xSQR:=0; if r=0 then Exit;
  IF ( ABS(2*LN(ABS(r))) )<LN(MAXREAL)
    THEN xSQR:=EXP( 2*LN(ABS(r)) )
    ELSE tfp_ernr:=11;
END;

FUNCTION xSQRT(VAR r : REAL) : REAL;

BEGIN
  xSQRT:=0;
  IF (r>=0)
    THEN xSQRT:=SQRT(r)
    ELSE tfp_ernr:=8;
END;

FUNCTION xTAN(VAR r : REAL) : REAL;

BEGIN
  xTAN:=0;
  IF (COS(r)=0)
    THEN tfp_ernr:=5
    ELSE xTAN:=SIN(r)/COS(r);
END;

FUNCTION xTRUE : REAL;

BEGIN
  xTRUE:=tfp_true;
END;

FUNCTION xXOR(VAR r1,r2 : REAL) : REAL;

BEGIN
 IF ((r1<>tfp_false) AND (r1<>tfp_true)) OR
    ((r2<>tfp_false) AND (r2<>tfp_true))
   THEN
     BEGIN
       IF (tfp_ernr=0)
         THEN tfp_ernr:=14;
     END
   ELSE xxor:=tfp_true*ORD((r1=tfp_true) XOR (r2=tfp_true));
END;

{===========================================================================}

PROCEDURE TMathParser.init(no : INTEGER);

BEGIN
  IF (maxfie>0)
    THEN FREEMEM(fiearr,maxfie*SIZEOF(fiearr^));

  GETMEM(fiearr,no*SIZEOF(fiearr^));

  maxfie:=no;
  fiesiz:=0;
END;

destructor TMathParser.Destroy;
begin
  if (maxfie>0) then FreeMem(fiearr,maxfie*SIZEOF(fiearr^));
  maxfie:=0;fiesiz:=0;
end;

{---------------------------------------------------------}

FUNCTION TMathParser.parse(s : string) : REAL;

VAR
  i,h     : INTEGER;
  value   : REAL;

BEGIN
  tfp_ernr:=0;

{----Test for match on numbers of ( and ) }
  h:=0;
  FOR i:=1 TO LENGTH(s) DO
    CASE s[i] OF
      '(' : INC(h);
      ')' : DEC(h);
    END;

  IF (h=0)
    THEN
      BEGIN

      {----Continue init}
        lp:=0;

      {----Add a CHR(0) as an EOLN marker}
        line:=S+#00;
        Skip;

      {----Try parsing if any characters left}
        IF (Line[Lp]<>#00)
          THEN value:=Eval_b_expr
          ELSE tfp_ernr:=6;
      END
    ELSE tfp_ernr:=3;

  IF (tfp_ernr<>0)
    THEN parse:=0.0
    ELSE parse:=value;
  if tfp_ernr<>0 then raise EMathParser.CreateCode(tfp_ernr);  {added by RRR!}
END;

PROCEDURE TMathParser.addobject(a : pointer;n : tfp_fname;t : tfp_ftype);

VAR
  i : INTEGER;

BEGIN
  {$R-}
  IF (fiesiz<maxfie)
    THEN
      BEGIN
        INC(fiesiz);
        WITH fiearr^[fiesiz] DO
          BEGIN
            faddr:=a;
            fname:=n;
            FOR i:=1 TO LENGTH(fname) DO
              IF (UPCASE(fname[i]) IN ['0'..'9','_','A'..'Z'])
                THEN fname[i]:=UPCASE(fname[i])
                ELSE tfp_ernr:=12;
              IF (LENGTH(fname)>0) AND
                 NOT (fname[1] IN ['A'..'Z'])
                THEN tfp_ernr:=12;
              ftype:=t;
          END
      END
    ELSE tfp_ernr:=10;
  {$R+}
  if tfp_ernr<>0 then raise EMathParser.CreateCode(tfp_ernr);  {added by RRR!}
END;

{------------------ Adding routines ----------------------}

PROCEDURE TMathParser.addgonio;
BEGIN
  AddObject(@xARCTAN,'ARCTAN',tfp_1real);
  AddObject(@xCOS   ,'COS'   ,tfp_1real);
  AddObject(@xDEG   ,'DEG'   ,tfp_1real);
  AddObject(@xPI    ,'PI'    ,tfp_noparm);
  AddObject(@xRAD   ,'RAD'   ,tfp_1real);
  AddObject(@xSIN   ,'SIN'   ,tfp_1real);
  AddObject(@xTAN   ,'TAN'   ,tfp_1real);
END;

PROCEDURE TMathParser.addlogic;
BEGIN
  AddObject(@xAND   ,'AND'   ,tfp_nreal);
  AddObject(@xFALSE ,'FALSE' ,tfp_noparm);
  AddObject(@xIOR   ,'OR'    ,tfp_nreal);
  AddObject(@xTRUE  ,'TRUE'  ,tfp_noparm);
  AddObject(@xXOR   ,'XOR'   ,tfp_2real);
END;

PROCEDURE TMathParser.addmath;
BEGIN
  AddObject(@xABS   ,'ABS'   ,tfp_1real);
  AddObject(@xEXP   ,'EXP'   ,tfp_1real);
{  AddObject(@xE     ,'E'     ,tfp_noparm);}
  AddObject(@xRANDOM,'RANDOM',tfp_noparm);
  AddObject(@xLN    ,'LN'    ,tfp_1real);
  AddObject(@xLOG   ,'LOG'   ,tfp_1real);
  AddObject(@xSQR   ,'SQR'   ,tfp_1real);
  AddObject(@xSQRT  ,'SQRT'  ,tfp_1real);
END;

PROCEDURE TMathParser.addmisc;
BEGIN
  AddObject(@xFRAC  ,'FRAC'  ,tfp_1real);
  AddObject(@xINT   ,'INT'   ,tfp_1real);
  AddObject(@xMAX   ,'MAX'   ,tfp_nreal);
  AddObject(@xMIN   ,'MIN'   ,tfp_nreal);
  AddObject(@xROUND ,'ROUND' ,tfp_1real);
  AddObject(@xSGN   ,'SGN'   ,tfp_1real);
END;

end.
