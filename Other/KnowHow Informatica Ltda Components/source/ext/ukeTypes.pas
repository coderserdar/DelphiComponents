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

unit ukeTypes;

{$I s:\v100\include\iKLIB100.inc}

interface

type

	TKDefaultMathFunc =
		(
		{ Trigonometric Functions }

			{ Basic }
			dmfSin, dmfCos, dmfTan, dmfCoTan,
			{ Inverse }
			dmfArcSin, dmfArcCos, dmfArcTan, dmfArcTan2,
			{ Hyperbolic  }
			dmfSinh, dmfCosh, dmfTanh,
			{ Hyperbolic Inverse }
			dmfArcSinh, dmfArcCosh, dmfArcTanh,

		{ Angle Unit Conversion }
			dmfDegToRad, dmfRadToDeg,
			dmfGradToRad, dmfRadToGrad,
			dmfCycleToRad, dmfRadToCycle,

		{ Standard Pascal Predefined Functions }
			dmfAbs, dmfInt, dmfFrac, dmfTrunc, dmfRound, dmfSqr, dmfSqrt, dmfExp,
			dmfLn, dmfInc, dmfDec,

		{ Byte Alignment Predefined Functions (D4 has Int64) }

{$IFDEF DELPHI4}
			dmfHiDWord, dmfLoDWord, dmfHiHiWord, dmfHiLoWord, dmfLoHiWord, dmfLoLoWord,
			dmfHiHiHiByte, dmfHiHiLoByte, dmfHiLoHiByte, dmfHiLoLoByte, dmfLoHiHiByte,
			dmfLoHiLoByte, dmfLoLoHiByte, dmfLoLoLoByte,
{$ENDIF}
			dmfHiWord, dmfLoWord, dmfHiHiByte, dmfHiLoByte, dmfLoHiByte, dmfLoLoByte,


		{ Exponential and Logorithmic Functions }
			dmfHyPot, dmfFPowerN, dmfLog10, dmfLog2, dmfLogN,

		{ Statistical Functions }
			dmfMin, dmfMax, dmfMean, dmfSum, dmfSumSqr,
			dmfStdDev, dmfNorm,

		{ Random Number Generators }
			dmfRandG, dmfRandom,

		{ Generic Routines }
			dmfCeil, dmfFloor, dmfSign, dmfNot
		);

	TKDefaultMathIdent =
		( dmiPI, dmiE );

  TKDefaultPropExprFunc =
    (
      dpefTextWidth, dpefTextHeight 
    );
    
	TKRegFuncInfo = record
		Name: string;
    Comment: string;
    Formula: string;
    ParamNames: string;
		ParamCount: ShortInt;
    GroupID: ShortInt;
	end;

	TKRegIdentInfo = record
		Name: string;
    Comment: string;
		Value: Extended;
    GroupID: ShortInt;
	end;

  TKRegGroupInfo = record
    Name: string;
    Comment: string;
    GroupID: ShortInt;
  end;      

implementation

end.
