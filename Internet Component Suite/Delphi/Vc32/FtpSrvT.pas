{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Time functions.
EMail:        francois.piette@pophost.eunet.be    francois.piette@swing.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Creation:     Nov 24, 1999 from Bruce Christensen <bkc51831234@hotmail.com>
              code used with his permission. Thanks.
Version:      1.02
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1999-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Apr 02, 2000  V1.01 Added definition for TIME_ZONE_ID_STANDARD for BCB1 and BCB3
May 20, 2000  V1.01 Added definition for TIME_ZONE_ID_STANDARD for Delphi 3


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit FtpSrvT;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$IFNDEF VER80} { Not for Delphi 1                    }
    {$H+}       { Use long strings                    }
    {$J+}       { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF VER110} { C++ Builder V3.0                    }
    {$ObjExportAll On}
{$ENDIF}
{$IFDEF VER125} { C++ Builder V4.0                    }
    {$ObjExportAll On}
{$ENDIF}

const
    FtpSrvT_Unit       = 102;
    CopyRight : String = ' FtpSrvT  (c) 1999-2000 F. Piette V1.02 ';

function FileUtcStr(cFileName : String) : String;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
implementation
uses
    WinTypes, WinProcs, SysUtils;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function PadIntZero(nWord  : Word;
                    nWidth : Byte): String;
var
    cResult : String;
begin
    cResult := IntToStr(nWord);
    while Length(cResult) < nWidth do
        cResult := '0' + cResult;

    Result := cResult;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TimeDateStr(dDateTime : TDateTime) : String;
var
    nYear, nMonth, nDay, nHours, nMinutes, nSeconds, nMilliSecs : Word;
begin
    DecodeDate(dDateTime, nYear, nMonth, nDay);
    DecodeTime(dDateTime, nHours, nMinutes, nSeconds, nMilliSecs);

    Result := PadIntZero(nYear,  4) +
              PadIntZero(nMonth, 2) +
              PadIntZero(nDay,   2) +
              PadIntZero(nHours,   2) +
              PadIntZero(nMinutes, 2) +
              PadIntZero(nSeconds, 2) + '.' +
              PadIntZero(nMilliSecs, 3);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetLocalBiasUTC : LongInt;
{$IFDEF VER80}
{ Delphi 1 doesn't support GetTimeZoneInformation }
begin
    Result := 0;
end;
{$ELSE}
var
    tzInfo : TTimeZoneInformation;
{$IFDEF VER93} {BCB1}
const
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
{$ENDIF}
{$IFDEF VER110} {BCB3}
const
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
{$ENDIF}
{$IFDEF VER90} {D2}
const
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
{$ENDIF}
{$IFDEF VER100} {D3}
const
  TIME_ZONE_ID_STANDARD = 1;
  TIME_ZONE_ID_DAYLIGHT = 2;
{$ENDIF}
begin
    case GetTimeZoneInformation(tzInfo) of
    TIME_ZONE_ID_STANDARD: Result := tzInfo.Bias + tzInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Result := tzInfo.Bias + tzInfo.DaylightBias;
    else
        Result := tzInfo.Bias;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DateTimeToUTC(dtDT : TDateTime) : TDateTime;
begin
    Result := dtDT + GetLocalBiasUTC / (60.0 * 24.0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetFileAge(cFile : String) : Integer;
begin
    if cFile[Length(cFile)] in ['\', '/'] then
        cFile := cFile + '.';
    Result := FileAge(cFile);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FileUtcStr(cFileName : String) : String;
begin
    Result := TimeDateStr(
                  DateTimeToUTC(
                      FileDateToDateTime(GetFileAge(cFileName))));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

