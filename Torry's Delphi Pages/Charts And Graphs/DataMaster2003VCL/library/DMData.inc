///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

const
  MaxCols=byte('z')-byte('a')+1;
  CRLF=#13#10; 
  LFCR=#10#13; 
  CR=#13; 
  LF=#10;         

type
  TColIndex=1..MaxCols; // TRealArray index type
{$ifdef doublefloat}
  TReal=double;
{$else}
  TReal=extended; // basic floating point number type
{$endif}
  TRealPoint=record  
    X,Y: TReal;
  end;
  PRealArray=^TRealArray; 
  TRealArray=array [TColIndex] of TReal;
  TFormat=packed record // floating point number format
    Width, Decimals: byte;
    FType: TFloatFormat;
  end;
  TFormatArray=array [TColIndex] of TFormat;
  PFormatArray=^TFormatArray;
