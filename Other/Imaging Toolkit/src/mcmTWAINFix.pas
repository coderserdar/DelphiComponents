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
// $Log:  15894: mcmTWAINFix.pas 
//
//    Rev 1.3    2014-03-28 17:52:54  mcm    Version: DT 4.1
// Added TWAIN 2.x support, and thereby support for Windows 7 & 8
//
//    Rev 1.2    2014-01-15 13:41:58  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.1    2013-12-04 23:16:12  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//   Rev 1.0    04-12-2001 16:49:08  mcm    Version: DT 2.0

unit mcmTWAINFix;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows,
     {$ELSE}
     Windows,
     {$ENDIF}
     twain;

function FloatToFIX32(floater : Real) : TW_FIX32;

function FIX32ToFloat(fix32 : TW_FIX32) : Real;

implementation


function FloatToFIX32(floater : Real) : TW_FIX32;
// FloatToFIX32 - Convert a floating point value into a FIX32.
var Fix32_value : TW_FIX32;
    Value       : TW_INT32;
begin
  Value := TW_INT32(Round(floater * 65536.0{ + 0.50}));
  Fix32_value.Whole := Trunc(floater); 
  Fix32_value.Frac  := LOWORD(value and $0000FFFF);
  FloatToFIX32      := Fix32_value;
end; { End FloatToFIX32.                                                       }


function FIX32ToFloat(fix32 : TW_FIX32) : Real;
// FIX32ToFloat - Convert a FIX32 value into a floating point value.
var floater : Real;
begin
  floater      := (1.0 * fix32.Frac / 65536.0);
  floater      := Round(10000.0 * (floater + 0.000005)) / 10000.0;
  floater      := floater + 1.0 * fix32.Whole;
  FIX32ToFloat := floater;
end; { End FIX32ToFloat.                                                       }



end.
