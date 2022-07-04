{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukeConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, TypInfo, uksyConsts, ukrMessages, ukeTypes;

const

{
--------------------------------------------------------------------------------
----------------------------- MessageSpy Constants -----------------------------
--------------------------------------------------------------------------------
}

	SPY_MSG_LOG_PATTERN = '%s.%-*s';
	SPY_MSG_LOG_PATTERN_EX = '%s.%-*s WParam $%8.8x; LParam $%8.8x; Result $%8.8x';

	SPY_MSG_EXT = '.msg';

	imeDefMaps = [imeWM, imeCM, imeCN];

	imeNames: array[imeWM..imeCN] of string[6] = (
		'imeWM', 'imeBM', 'imeLBN', 'imeLBM', 'imeCBN', 'imeCBM', 'imeEN',
		'imeEM', 'imeSBM', 'imeDM', 'imeSBC', 'imeSM2', 'imeCM', 'imeCN' );
  
	MsgEnumsMap: array[imeWM..imeCN] of TIdentMapEntry = (
		( Value: imeWM;   Name: 'imeWM' ),
		( Value: imeBM;   Name: 'imeBM' ),
		( Value: imeLBN;  Name: 'imeLBN' ),
		( Value: imeLBM;  Name: 'imeLBM' ),
		( Value: imeCBN;  Name: 'imeCBN' ),
		( Value: imeCBM;  Name: 'imeCBM' ),
		( Value: imeEN;   Name: 'imeEN' ),
		( Value: imeEM;   Name: 'imeEM' ),
		( Value: imeSBM;  Name: 'imeSBM' ),
		( Value: imeDM;   Name: 'imeDM' ),
		( Value: imeSBC;  Name: 'imeSBC' ),
		( Value: imeSM2;  Name: 'imeSM2' ),
		( Value: imeCM;   Name: 'imeCM' ),
		( Value: imeCN;   Name: 'imeCN' ) );

{----------------------------- Except Log Engine -------------------------------}

  MAX_HEXDUMP_DIGITS = 16;

{ Names for the StringsArray Strings name property }
	KSA_NAME_SECTIONS = 'Sections';
	KSA_NAME_MODADDRESSES ='ModAddresses';
	KSA_NAME_MODNAMES = 'ModNames';
	KSA_NAME_ADDRESSES = 'Addresses_%s';
	KSA_NAME_UNITNAME = 'ModName_%s';
	KSA_VALUE_NOTFOUND = '<not found>';

	KSA_INDEX_MAPCOMPATIBILITY = 0;
	KSA_INDEX_MODADDRESSES = 1;


{ This value was extracted from the map file format in the line numbers sections! }
	MAX_LINENUMBER_LENGTH = 11; {999999   11 because the <not found>}

{ Map file signature values }
	VALUES_ADDR_SECTION = '[Values]';
	MODULES_ADDR_SECTION = '[Module Addresses]';
	MAPCOMPATIBILY_SECTION = '[Map Compatibility]';

	PUBLICS_BYVALUES_SIGNATURE = 'Publics by Value';
	LINE_NUM_SIGNATURE = 'Line numbers for';

  DELPHI_MAPFILE_EXT = '.map';
	KNOWHOW_MAPFILE_EXT = '.kmp';
	KELE_REPORT_EXTENTION = '.ker';
  EXPT_LOGEXT = '.kel';

	METHODS_ADDRESSES_SEGMENT = '0001:';
	PUBLICS_VARIABLES_ADDRESSES_SEGMENT = '0002:';

	EX_FIXEDFLDNAME_EXPTADDR = 'Exception.ExceptAddr';
	KEX_DEF_MESSAGE_COUNT: Integer = 8;
	EXCEPTLOG_NOUSERCOMMENT = '<No user comment>';

	EXCEPT_RES_DELERRMSG = 900;
	EXCEPT_RES_KERRMSG   = 1000;

{----------------------------------- TKDCC32 -----------------------------------}

  COMPILER_GROUP_ALL  = COLLECTION_ITEM_GROUP_ALL;
  COMPILER_GROUP_NULL = COLLECTION_ITEM_GROUP_NULL;

	COMPILE_ITEMS_ATTRCOUNT = 4;
	COMPILE_ITEMS_COMPFNTYP = 'Compile File Type';

	DCC32_INI_DATETIME_FMT = 'dd/mm/yyyy hh:nn:ss';

  CURRENT_DCC32_VERSION = '1.00';

  DCC32_MAIN_INI_SEC_IDX = 0;

	DCC32_INI_COMMENT =
		'; * The compile item options, switches, and package environments are' + CH_CRLF +
		'; encoded as bitmasks (each posible value ORed as power of 2).' + CH_CRLF + CH_CRLF +
		'; * The values of LoadType field indicates a indicates a merge (0)' + CH_CRLF +
		'; or load (1 - default) operation.' + CH_CRLF + CH_CRLF;

	DCC32MIN_INI_FMT = 'DCC32-V%s=%s';

	DCC32MAIN_INI_SECTION =
		'[%s]' + CH_CRLF +
		'InitialDirectory=%s' + CH_CRLF +
		'Tag=%d' + CH_CRLF +
		'CompileItemCount=%d' + CH_CRLF +
		'LoadType=%d' + CH_CRLF +
		'KLibVersion=%s, %s' + CH_CRLF + CH_CRLF;

	DCC32MAIN_INI_MAXNAME_COUNT = 5;

	DCC32MAIN_INI_NAMES: array[0..DCC32MAIN_INI_MAXNAME_COUNT - 1] of string =
	(
		'InitialDirectory', 'Tag', 'CompileItemCount', 'LoadType', 'KLibVersion'
	);

	COMPILEITEM_INI_SECTION =
		'[%s=%d]'#13#10 +
		'CompileFileName=%s'#13#10 +
		'DCC32CompiledFilesType=%d'#13#10 +
		'DCC32EnumType=%d'#13#10 +
		'DCC32MapFile=%d'#13#10 +
		'DCC32Options=%d'#13#10 +
		'DCC32Switches=%d'#13#10 +
		'DCC32Version=%d'#13#10 +
		'DCUOutPutDir=%s'#13#10 +
		'Enabled=%d'#13#10 +
		'ErrorAction=%d'#13#10 +
		'EXEOutPutDir=%s'#13#10 +
		'GroupIndex=%d'#13#10 +
		'HasStatistics=%d'#13#10 +
		'ImageBaseAddr=%d'#13#10 +
		'IncludePaths=%s'#13#10 +
		'InitialDirectory=%s'#13#10 +
		'MaxStackSize=%d'#13#10 +
		'MinStackSize=%d'#13#10 +
		'ObjectPaths=%s'#13#10 +
		'PackageEnvironments=%d'#13#10 +
		'ResourcePaths=%s'#13#10 +
		'RunTimePackages=%s'#13#10 +
		'SymbolDefines=%s'#13#10 +
		'TargetExtension=%s'#13#10 +
		'UnitAliases=%s'#13#10 +
		'UnitPaths=%s'#13#10#13#10;

	COMPILEITEM_INI_MAXNAME_COUNT = 26;

	COMPILEITEM_INI_NAMES: array[0..COMPILEITEM_INI_MAXNAME_COUNT - 1] of string =
	(
		'CompileFileName', 'DCC32CompiledFilesType', 'DCC32EnumType', 'DCC32MapFile',
		'DCC32Options', 'DCC32Switches', 'DCC32Version', 'DCUOutPutDir', 'Enabled',
		'ErrorAction', 'EXEOutPutDir', 'GroupIndex', 'HasStatistics', 'ImageBaseAddr',
		'IncludePaths', 'InitialDirectory', 'MaxStackSize', 'MinStackSize', 'ObjectPaths',
		'PackageEnvironments', 'ResourcePaths', 'RunTimePackages', 'SymbolDefines',
		'TargetExtension', 'UnitAliases', 'UnitPaths'
	);



{----------------------------------- TKShell -----------------------------------}

	SHELL_EXITCODE = 0;

{##NI##}

{---------------------------------- TKDFMData ----------------------------------}

	KDFMDATA_SIGNATURE = '{9D2D8580-0474-11D3-AAF0-00C0DFE085FF}';

{-------------------------------- TKDFMResource --------------------------------}

	DEFAULT_DFMRES_FILENAME = 'dfmres_tmp';

	RES_AVI_EXT = '.avi';
	RES_WAV_EXT = '.wav';

{-------------------------------- TKCmdLineLexer -------------------------------}

  CMDLINE_APPNAME = 'AppName';	

{--------------------------- Math Parser/Lexer/Solver --------------------------}

	eiNumber = -1;
	eiIdent  = -2;
	OPEN_ARRAY_PARAMS = -1;
  DEFAULT_GROUP_ID  = -1;

  FIRST_GROUP_ID            = 0;
  TRIGONOMETRIC_GROUP_ID    = FIRST_GROUP_ID + 0;
  INVERSE_TRIGO_GROUP_ID    = FIRST_GROUP_ID + 1;
  HYPERBL_TRIGO_GROUP_ID    = FIRST_GROUP_ID + 2;
  INVERSE_HYTRIGO_GROUP_ID  = FIRST_GROUP_ID + 3;
  ANGLE_CONVS_GROUP_ID      = FIRST_GROUP_ID + 4;
  PREDEFINED_MATH_GROUP_ID  = FIRST_GROUP_ID + 5;
  MEMORY_ALIGNMENT_GROUP_ID = FIRST_GROUP_ID + 6;
  SPECIAL_MATH_GROUP_ID     = FIRST_GROUP_ID + 7;
  STATISTC_MATH_GROUP_ID    = FIRST_GROUP_ID + 8;
  SPECIAL_PROPEXPR_GROUP_ID = FIRST_GROUP_ID + 9;
  LAST_GROUP_ID             = FIRST_GROUP_ID + 9;

	REG_FUNC_INFOS: array[TKDefaultMathFunc] of TKRegFuncInfo =
		(
			( Name      : 'SIN';
        Comment   : 'Sine of a radian angle in radians';
        Formula   : 'Sin(rAngle)';
        ParamNames: 'rAngle';
        ParamCount: 1;
        GroupID   : TRIGONOMETRIC_GROUP_ID ),

			( Name      : 'COS';
        Comment   : 'Cosine of a radian angle in radians';
        Formula   : 'Cos(rAngle)';
        ParamNames: 'rAngle';
        ParamCount: 1;
        GroupID   : TRIGONOMETRIC_GROUP_ID ),

			( Name      : 'TAN';
        Comment   : 'Tangent of a radian angle in radians. Tan(x) = Sin(x)/Cos(x)';
        Formula   : 'Tan(rAngle)';
        ParamNames: 'rAngle';
        ParamCount: 1;
        GroupID   : TRIGONOMETRIC_GROUP_ID ),

			( Name      : 'COTAN';
        Comment   : 'CoTangent of a radian angle in radians. Angle could not be 0.' +
                    'Cotan(x)= 1/Tan(x)';
        Formula   : 'Cotan(rAngle)';
        ParamNames: 'rAngle';
        ParamCount: 1;
        GroupID   : TRIGONOMETRIC_GROUP_ID ),

			( Name      : 'ARCSIN';
        Comment   : 'Returns the inverse sine of X. X must be between -1 and 1.' +
                    'The return value will be in the range [-Pi/2..Pi/2], in radians.';
        Formula   : 'ArcSin(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : INVERSE_TRIGO_GROUP_ID ),

			( Name      : 'ARCCOS';
        Comment   : 'Returns the inverse cosine of X. X must be between -1 and 1.' +
                    'The return value will be in the range [0..Pi], in radians.';
        Formula   : 'ArcCos(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : INVERSE_TRIGO_GROUP_ID ),

			( Name      : 'ARCTAN';
        Comment   : 'Returns the inverse tangent of a number. Calculate other '  +
                    'trigonometric functions using Sin, Cos, and ArcTan in the ' +
                    'following expressions: Tan(x) = Sin(x) / Cos(x);'#13#10 +
                    'ArcSin(x) = ArcTan(x/sqrt(1-sqr(x)))'#13#10 +
                    'ArcCos(x) = ArcTan(sqrt(1-sqr(x))/x)';
        Formula   : 'ArcTan(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : INVERSE_TRIGO_GROUP_ID ),

			( Name      : 'ARCTAN2';
        Comment   : 'Returns the inverse tangent and quadrant of a number.' +
                    'Calculates ArcTan(Y/X), and returns an angle in the ' +
                    'correct quadrant. IN: |Y| < 2^64, |X| < 2^64, X <> 0 OUT: '  +
                    '[-PI..PI] radians';
        Formula   : 'ArcTan2(Y/X)';
        ParamNames: 'Y;X';
        ParamCount: 2;
        GroupID   : INVERSE_TRIGO_GROUP_ID ),

      ( Name      : 'SINH';
        Comment   : 'Hyperbolic sine of an angle';
        Formula   : 'SinH(Angle)';
        ParamNames: 'Angle';
        ParamCount: 1;
        GroupID   : HYPERBL_TRIGO_GROUP_ID ),

      ( Name      : 'COSH';
        Comment   : 'Hyperbolic cosine of an angle';
        Formula   : 'CosH(Angle)';
        ParamNames: 'Angle';
        ParamCount: 1;
        GroupID   : HYPERBL_TRIGO_GROUP_ID ),

      ( Name      : 'TANH';
        Comment   : 'Hyperbolic tangent of an angle';
        Formula   : 'TanH(Angle)';
        ParamNames: 'Angle';
        ParamCount: 1;
        GroupID   : HYPERBL_TRIGO_GROUP_ID ),

      ( Name      : 'ARCSINH';
        Comment   : 'Inverse hyperbolic sine of X';
        Formula   : 'ArcSinH(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : INVERSE_HYTRIGO_GROUP_ID ),

      ( Name      : 'ARCCOSH';
        Comment   : 'Inverse hyperbolic cosine of X (X >=1)';
        Formula   : 'ArcCosH(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : INVERSE_HYTRIGO_GROUP_ID ),

      ( Name      : 'ARCTANH';
        Comment   : 'Inverse hyperbolic tangent of X (X in [-1..1])';
        Formula   : 'ArcTanH(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : INVERSE_HYTRIGO_GROUP_ID ),

      ( Name      : 'DEGTORAD';
        Comment   : 'Convert degrees to radians. Radians = Degrees(pi/180)';
        Formula   : 'DegToRad(Degree)';
        ParamNames: 'Degree';
        ParamCount: 1;
        GroupID   : ANGLE_CONVS_GROUP_ID ),

      ( Name      : 'RADTODEG';
        Comment   : 'Convert radians to degrees. Radians = Degrees(pi/180)';
        Formula   : 'RadToDeg(Radian)';
        ParamNames: 'Radian';
        ParamCount: 1;
        GroupID   : ANGLE_CONVS_GROUP_ID ),

      ( Name      : 'GRADTORAD';
        Comment   : 'Convert grads to radians. Grads = Radians(200/pi)';
        Formula   : 'GradToRad(Grads)';
        ParamNames: 'Grads';
        ParamCount: 1;
        GroupID   : ANGLE_CONVS_GROUP_ID ),

      ( Name      : 'RADTOGRAD';
        Comment   : 'Convert radians to grads. Grads = Radians(200/pi)';
        Formula   : 'RadToGrad(Radian)';
        ParamNames: 'Radian';
        ParamCount: 1;
        GroupID   : ANGLE_CONVS_GROUP_ID ),

			( Name      : 'CYCLETORAD';
        Comment   : 'Convert cycles to radians. Cycles = Radians/(2pi)';
        Formula   : 'CycleToRad(Cycle)';
        ParamNames: 'Cycle';
        ParamCount: 1;
        GroupID   : ANGLE_CONVS_GROUP_ID ),

			( Name      : 'RADTOCYCLE';
        Comment   : 'Convert radians to cycles. Cycles = Radians/(2pi)';
        Formula   : 'RadToCycle(Radian)';
        ParamNames: 'Radian';
        ParamCount: 1;
        GroupID   : ANGLE_CONVS_GROUP_ID ),

			( Name      : 'ABS';
        Comment   : 'Returns absolute value of X';
        Formula   : 'Abs(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'INT';
        Comment   : 'Returns the integer part of X (i.e. X rounded toward zero)';
        Formula   : 'Int(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'FRAC';
        Comment   : 'Returns the fractional part of X (i.e. Frac(X) = X-Int(X))';
        Formula   : 'Int(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'TRUNC';
        Comment   : 'Returns the integer part of X typecasted to Longint';
        Formula   : 'Trunc(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'ROUND';
        Comment   : 'Returns a Longint value that is the value of X rounded to ' +
                    'the nearest whole number. If X is exactly halfway between ' +
                    'two whole numbers, the result is the number with the greatest ' +
                    'absolute magnitude';
        Formula   : 'Round(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'SQR';
        Comment   : 'Returns the square of a number X (i.e. X*X)';
        Formula   : 'Sqr(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'SQRT';
        Comment   : 'Returns the square root of a number X (i.e. X^(1/2))';
        Formula   : 'Sqrt(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'EXP';
        Comment   : 'Returns the value of "e" raised to the power of X, where "e" ' +
                    'is the base of the natural logarithms. You can use Exp to ' +
                    'calculate powers: Exp(Y*Ln(X)) = X^Y';
        Formula   : 'Exp(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'LN';
        Comment   :  'Returns the natural log of X. You can use Ln to calculate ' +
                     'powers: Exp(Y*Ln(X)) = X^Y. Ln(e) = 1.';
        Formula   : 'Ln(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'INC';
        Comment   : 'Increments X by N';
        Formula   : 'Inc(X, N)';
        ParamNames: 'X;N';
        ParamCount: 2;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

			( Name      : 'DEC';
        Comment   : 'Decrements X by N';
        Formula   : 'Dec(X, N)';
        ParamNames: 'X;N';
        ParamCount: 2;
        GroupID   : PREDEFINED_MATH_GROUP_ID ),

{$IFDEF DELPHI4}
			( Name      : 'HIDWORD';
        Comment   : 'Returns the high order DWORD of a Int64';
        Formula   : 'HiDWORD(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LODWORD';
        Comment   : 'Returns the low order DWORD of a Int64';
        Formula   : 'LoDWORD(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),


			( Name      : 'HIHIWORD';
        Comment   : 'Returns the fourth WORD of a Int64';
        Formula   : 'HiHiWORD(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'HILOWORD';
        Comment   : 'Returns the third WORD of a Int64';
        Formula   : 'HiLoWORD(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOHIWORD';
        Comment   : 'Returns the second WORD of a Int64';
        Formula   : 'LoHiWORD(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOLOWORD';
        Comment   : 'Returns the first WORD of a Int64';
        Formula   : 'LoLoWORD(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),


			( Name      : 'HIHIHIBYTE';
        Comment   : 'Returns the eighth BYTE of a Int64';
        Formula   : 'HiHiHiBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'HIHILOBYTE';
        Comment   : 'Returns the seventh BYTE of a Int64';
        Formula   : 'HiHiLoBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'HILOHIBYTE';
        Comment   : 'Returns the sixth BYTE of a Int64';
        Formula   : 'HiLoHiBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'HILOLOBYTE';
        Comment   : 'Returns the fifth BYTE of a Int64';
        Formula   : 'HiLoLoBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOHIHIBYTE';
        Comment   : 'Returns the fourth BYTE of a Int64';
        Formula   : 'LoHiHiBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOHILOBYTE';
        Comment   : 'Returns the third BYTE of a Int64';
        Formula   : 'LoHiLoBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOLOHIBYTE';
        Comment   : 'Returns the second BYTE of a Int64';
        Formula   : 'LoLoHiBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOLOLOBYTE';
        Comment   : 'Returns the first BYTE of a Int64';
        Formula   : 'LoLoLoBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

{$ENDIF}
			( Name      : 'HIWORD';
        Comment   : 'Returns the high order WORD of a Integer';
        Formula   : 'HiWORD(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOWORD';
        Comment   : 'Returns the low order WORD of a Integer';
        Formula   : 'LoWORD(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),


			( Name      : 'HIHIBYTE';
        Comment   : 'Returns the fourth BYTE of a Integer';
        Formula   : 'HiHiBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'HILOBYTE';
        Comment   : 'Returns the third BYTE of a Integer';
        Formula   : 'HiLoBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOHIBYTE';
        Comment   : 'Returns the second BYTE of a Integer';
        Formula   : 'LoHiBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'LOLOBYTE';
        Comment   : 'Returns the first BYTE of a Integer';
        Formula   : 'LoLoBYTE(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : MEMORY_ALIGNMENT_GROUP_ID ),

			( Name      : 'HYPOT';
        Comment   : 'Calculates the length of the hypotenuse of a triangle.'  +
                    'Specify the lengths of the sides adjacent to the right ' +
                    'angle in X and Y. Hypot uses the formula Sqrt(X^2 + Y^2)';
        Formula   : 'Hypot(X, Y)';
        ParamNames: 'X;Y';
        ParamCount: 2;
        GroupID   : SPECIAL_MATH_GROUP_ID ),

			( Name      : 'FPOWERN';
        Comment   : 'Calculates Power( Base, Expoent ) * N';
        Formula   : 'FPowerN(N, Base, Expoent)';
        ParamNames: 'N;Base;Expoent';
        ParamCount: 3;
        GroupID   : SPECIAL_MATH_GROUP_ID ),

			( Name      : 'LOG10';
        Comment   : 'Calculates Log base 10 of X';
        Formula   : 'Log10(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : SPECIAL_MATH_GROUP_ID ),

			( Name      : 'LOG2';
        Comment   : 'Calculates Log base 2 of X';
        Formula   : 'Log2(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : SPECIAL_MATH_GROUP_ID ),

			( Name      : 'LOGN';
        Comment   : 'Calculates Log base N of X';
        Formula   : 'LogN(N, X)';
        ParamNames: 'N;X';
        ParamCount: 2;
        GroupID   : SPECIAL_MATH_GROUP_ID ),

			( Name      : 'MIN';
        Comment   : 'Returns the smallest signed value in an open array';
        Formula   : 'Min([X,Y,...])';
        ParamNames: 'X;Y;...';
        ParamCount: OPEN_ARRAY_PARAMS;
        GroupID   : STATISTC_MATH_GROUP_ID ),

			( Name      : 'MAX';
        Comment   : 'Returns the largest signed value in an open array';
        Formula   : 'Min([X,Y,...])';
        ParamNames: 'X;Y;...';
        ParamCount: OPEN_ARRAY_PARAMS;
        GroupID   : STATISTC_MATH_GROUP_ID ),

			( Name      : 'MEAN';
        Comment   : 'Returns arithmetic average of all the values in an open array';
        Formula   : 'Mean([X,Y,...])';
        ParamNames: 'X;Y;...';
        ParamCount: OPEN_ARRAY_PARAMS;
        GroupID   : STATISTC_MATH_GROUP_ID ),

			( Name      : 'SUM';
        Comment   : 'Returns the sum of all the values in an open array';
        Formula   : 'Sum([X,Y,...])';
        ParamNames: 'X;Y;...';
        ParamCount: OPEN_ARRAY_PARAMS;
        GroupID   : STATISTC_MATH_GROUP_ID ),

			( Name      : 'SUMSQR';
        Comment   : 'Returns the sum of the squared of all the values in an open array';
        Formula   : 'SumSqr([X,Y,...])';
        ParamNames: 'X;Y;...';
        ParamCount: OPEN_ARRAY_PARAMS;
        GroupID   : STATISTC_MATH_GROUP_ID ),

			( Name      : 'STDDEV';
        Comment   : 'Returns the sample standard deviation (the square root of the ' +
                    'sample variance) of all the values in an open array';
        Formula   : 'Min([X,Y,...])';
        ParamNames: 'X;Y;...';
        ParamCount: OPEN_ARRAY_PARAMS;
        GroupID   : STATISTC_MATH_GROUP_ID ),

			( Name      : 'NORM';
        Comment   : 'Returns Euclidean ''''L-2'''' value of all the values in an open array.' +
                    'The ''''L-2'''' norm is the square root of the sum of squares';
        Formula   : 'Norm([X,Y,...])';
        ParamNames: 'X;Y;...';
        ParamCount: OPEN_ARRAY_PARAMS;
        GroupID   : STATISTC_MATH_GROUP_ID ),

  		( Name      : 'RANDG';
        Comment   : 'Generates random numbers with Gaussian distribution';
        Formula   : 'RandG(Mean, StdDev)';
        ParamNames: 'Mean;StdDev';
        ParamCount: 2;
        GroupID   : STATISTC_MATH_GROUP_ID ),

			( Name      : 'RANDOM';
        Comment   : 'Generates random numbers with a specified range.' +
                    '(i.e. X = Random(N); 0 <= X < N)';
        Formula   : 'Random(N)';
        ParamNames: 'N';
        ParamCount: 1;
        GroupID   : STATISTC_MATH_GROUP_ID ),

			( Name      : 'CEIL';
        Comment   : 'Rounds X up toward positive infinity (i.e. nearest high integer)'#13#10 +
                    'Ceil(-2.8) = -2; Ceil(2.8) = 3; Ceil(-1.0) = -1';
        Formula   : 'Ceil(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : SPECIAL_MATH_GROUP_ID ),

			( Name      : 'FLOOR';
        Comment   : 'Rounds X down toward negative infinity (i.e. nearest low integer)'#13#10 +
                    'Floor(-2.8) = -3; Floor(2.8) = 2; Floor(-1.0) = -1';
        Formula   : 'Floor(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : SPECIAL_MATH_GROUP_ID ),

			( Name      : 'SIGN';
        Comment   : 'Returns 1 if X is greater than or equal to 0, -1 otherwise';
        Formula   : 'Sign(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : SPECIAL_MATH_GROUP_ID ),

			( Name      : 'NOT';
        Comment   : 'Returns the binary complement of rounded X (i.e. (not Round(X))';
        Formula   : 'Not(X)';
        ParamNames: 'X';
        ParamCount: 1;
        GroupID   : SPECIAL_MATH_GROUP_ID )
		);

	REG_PROPEXPR_FUNC_INFOS: array[TKDefaultPropExprFunc] of TKRegFuncInfo =
		(
			( Name      : 'TEXTWIDTH';
        Comment   : 'Width of the given text in pixels relative to the current canvas.'#13#10 +
                    'Only component identifiers with string properties are allowed as a'#13#10 +
                    'parameter. If you try to use literal string constants or other kind'#13#10 +
                    'of values an exception will be raised.'#13#10 +
                    'Ex: Button1.Width = TextWidth( Buton1.Caption )+ 2       - Ok!'#13#10 +
                    'Ex: Button1.Width = TextWidth( Buton1.Caption + ''WA'' ) - Error!';
        Formula   : 'TextWidth(sText)';
        ParamNames: 'sText';
        ParamCount: 1;
        GroupID   : SPECIAL_PROPEXPR_GROUP_ID ),

      ( Name      : 'TEXTHEIGHT';
        Comment   : 'Height of the given text in pixels relative to the current canvas.'#13#10 +
                    'Only component identifiers with string properties are allowed as a'#13#10 +
                    'parameter. If you try to use literal string constants or other kind'#13#10 +
                    'of values an exception will be raised.'#13#10 +
                    'Ex: Button1.Height = TextHeight( Buton1.Caption )+ 2       - Ok!'#13#10 +
                    'Ex: Button1.Height = TextHeight( Buton1.Caption + ''gy'' ) - Error!';
        Formula   : 'TextHeight(sText)';
        ParamNames: 'sText';
        ParamCount: 1;
        GroupID   : SPECIAL_PROPEXPR_GROUP_ID )
        
    );

	REG_IDENT_INFOS: array[TKDefaultMathIdent] of TKRegIdentInfo =
		(
			( Name   : 'PI';
        Comment: 'Ratio of a circle''''s circumference to its diameter';
        Value  : PI;
        GroupID: TRIGONOMETRIC_GROUP_ID ),

			( Name   : 'E';
        Comment: 'Base of the natural logarithms';
        Value  : 2.7182818285;
        GroupID: PREDEFINED_MATH_GROUP_ID )
		);

  REG_GROUP_INFOS: array[FIRST_GROUP_ID..LAST_GROUP_ID] of TKRegGroupInfo =
    (
      ( Name   : 'Trigonometric';
        Comment: 'Trigonometric math functions (eg. Sin, Cos, Tan, etc)';
        GroupID: TRIGONOMETRIC_GROUP_ID ),

      ( Name   : 'Inverse Trigo.';
        Comment: 'Inverse trigonometric math functions (eg. ArcSin, ArcCos, ArcTan, etc)';
        GroupID: INVERSE_TRIGO_GROUP_ID ),

      ( Name   : 'Hyperbolic Trigo.';
        Comment: 'Hyperbolic trigonometric math functions (eg. SinH, CosH, TanH, etc)';
        GroupID: HYPERBL_TRIGO_GROUP_ID ),

      ( Name   : 'Inv. Hyperb. Trigo.';
        Comment: 'Inverse hyperbolic trigonometric math functions (eg. ArcSinH, ArcCosH, ' +
                 'ArcTanH, etc)';
        GroupID: INVERSE_HYTRIGO_GROUP_ID ),

      ( Name   : 'Angle Conversion';
        Comment: 'Angle conversion math functions. Convert Radians, Degrees, Cycles ' +
                 'Grads, etc';
        GroupID: ANGLE_CONVS_GROUP_ID ),

      ( Name   : 'Predefined Math';
        Comment: 'Predefined math functions used in pascal (eg. Trunc, Round, Frac, etc)';
        GroupID: PREDEFINED_MATH_GROUP_ID ),

      ( Name   : 'Memory Alignment';
        Comment: 'Memory alignment functions. Give you access to each BYTE, WORD or DWORD ' +
                 'for a Word, Integer or Int64';
        GroupID: MEMORY_ALIGNMENT_GROUP_ID ),

      ( Name   : 'Special Math';
        Comment: 'Special math functions like hypotenuse, logn, ceil, floor, etc';
        GroupID: SPECIAL_MATH_GROUP_ID ),

      ( Name   : 'Statistic Math';
        Comment: 'Statistic math functions like min, max, sum, stddev, mean, etc';
        GroupID: STATISTC_MATH_GROUP_ID ),

      ( Name   : 'Prop. Expr. Functions';
        Comment: 'Special property expression functions for canvas manipulation';
        GroupID: SPECIAL_PROPEXPR_GROUP_ID )

    );

  KM_PROPEXPR_DSGN_LOADED = KM_USER + 100; {?}  

  PROPEXPR_TYPEKIND_ORDSET = [tkInteger, tkChar, tkEnumeration, tkClass, tkSet,
    tkWChar, tkInterface];
  PROPEXPR_TYPEKIND_FLTSET = [tkFloat];
  PROPEXPR_TYPEKIND_SET = PROPEXPR_TYPEKIND_ORDSET + PROPEXPR_TYPEKIND_FLTSET +
    [tkString, tkLString, tkWString];

{----------- TKCustomPascalParser Classes -----------}

	PASCAL_PARSER_KEYWORDS =
		'absolute'#13#10'abstract'#13#10'and'#13#10'array'#13#10'as'#13#10'asm'#13#10 +
		'assembler'#13#10'at'#13#10'automated'#13#10'begin'#13#10'case'#13#10'cdecl'#13#10 +
		'class'#13#10'const'#13#10'constructor'#13#10'contains'#13#10'default'#13#10 +
		'destructor'#13#10'dispid'#13#10'dispinterface'#13#10'div'#13#10'do'#13#10 +
		'downto'#13#10'dynamic'#13#10'else'#13#10'end'#13#10'except'#13#10'exports'#13#10 +
		'external'#13#10'file'#13#10'finalization'#13#10'finally'#13#10'for'#13#10 +
		'forward'#13#10'function'#13#10'goto'#13#10'if'#13#10'implementation'#13#10 +
		'in'#13#10'index'#13#10'inherited'#13#10'initialization'#13#10'inline'#13#10 +
		'interface'#13#10'is'#13#10'label'#13#10'library'#13#10'message'#13#10'mod'#13#10 +
		'name'#13#10'nil'#13#10'nodefault'#13#10'not'#13#10'object'#13#10'of'#13#10 +
		'on'#13#10'or'#13#10'override'#13#10'packed'#13#10'pascal'#13#10'private'#13#10 +
		'procedure'#13#10'program'#13#10'property'#13#10'protected'#13#10'public'#13#10 +
		'published'#13#10'raise'#13#10'read'#13#10'record'#13#10'register'#13#10'repeat'#13#10 +
		'requires'#13#10'resident'#13#10'set'#13#10'shl'#13#10'shr'#13#10'stdcall'#13#10 +
		'stored'#13#10'string'#13#10'then'#13#10'threadvar'#13#10'to'#13#10'try'#13#10 +
		'type'#13#10'unit'#13#10'until'#13#10'uses'#13#10'var'#13#10'virtual'#13#10 +
		'while'#13#10'with'#13#10'write'#13#10'xor';

	DFM_PARSER_KEYWORDS =
		'end'#13#10'inherited'#13#10'object';

{-------------------------------- TKCustomDump ---------------------------------}

	SELDATA_COLOR_IDX  = 0;
	SELDATA_BACK_COLOR_IDX = 1;

	ANSICHAR_COLOR_IDX = 2;
	ADDR_COLOR_IDX     = 3;
	HEXDATA_COLOR_IDX  = 4;

	TEXT_COLOR_IDX     = 5;


{$IFDEF KLIB100}
	EXT_VER = '1.00';
	EXT_VER_INT = 100;
	EXT_VER_RELEASE_DATE = '25/07/1999 01:00:00';
{$ELSE}
	EXT_VER = '?.??';
	EXT_VER_INT = 0;
	EXT_VER_RELEASE_DATE = '01/01/1900 00:00:00';
{$ENDIF}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function GetExtRegistryInfo: LongInt;

{##NI##}

implementation

uses
	SysUtils, uksyTypes;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type
	TSignature	 = TUserName;
	TKey				 = TUserName;

	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

const

	KnowHowRegistryInfo: TKRegistryInfo =
	(
{$IFDEF KLIB100}
		Signature: '{09536FA0-BF69-11D2-B212-00C0DFE081C4}'; { do not resource/const }
{$ELSE}
    Signature: *MUST GENERATE AN ERROR!*;
{$ENDIF}
		Key:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		UserName:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		Company:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32;
	);

{---------------------------- Public Implementation ----------------------------}

function GetExtRegistryInfo: LongInt;
begin
	Result := LongInt( @KnowHowRegistryInfo ) - SizeOf( TKRegistryInfo );
end;

end.