{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Registration unit for TWSocket. If you build a runtime
              package, you shouldn't include this unit.
Creation:     Feb 24, 2002
Version:      6.02
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2002-2007 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be> 

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
Feb 24, 2002 V1.00 Wilfried Mestdagh <wilfried@mestdagh.biz> created a
             property editor for LineEnd property. I moved his code ti this
             new unit so that it is compatible with Delphi 6.
Jan 19, 2003 V5.00 First pre-release for ICS-SSL. New major version number.
             Skipped version numbers to mach wsocket.pas major version number.
May 31, 2004 V5.01 Used ICSDEFS.INC the same way as in other units
Mar 24, 2008 V6.02 Francois Piette made some changes to prepare code
                   for Unicode.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverByteIcsWSocketE;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF CLR}
    Borland.Vcl.Classes;
{$ENDIF}
{$IFDEF WIN32}
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    SysUtils, Classes,
{$IFDEF COMPILER6_UP}
  { Delphi 6/7: Add $(DELPHI)\Source\ToolsAPI to your library path }
  { and add designide.dcp to ICS package.                          }
  { BCB6 6: Add $(BCB)\Source\ToolsAPI to your library path        }
  { and add designide.bpi to ICS package.                          }
  DesignIntf, DesignEditors;
{$ELSE}
  DsgnIntf;
{$ENDIF}
{$ENDIF}

const
    WSocketEVersion          = 602;
    CopyRight : String       = ' WSocketE (c) 2002-2008 F. Piette V6.02 ';

{$IFDEF WIN32}
type
    TWSocketLineEndProperty = class(TStringProperty)
    public
        function  GetLineEnd(const Value: String): String;
        function  SetLineEnd(const Value: String): String;
        function  GetValue: String; override;
        procedure SetValue(const Value: String); override;
    end;
{$ENDIF}

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{                         LineEnd Property Editor                           }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function TWSocketLineEndProperty.SetLineEnd(const Value: String): String;
var
    Offset : Integer;
    C      : Char;
begin
    if Pos('#', Value) = 0 then
        raise Exception.Create('Invalid value');

    Offset := 1;
    Result := '';
    repeat
        if Value[Offset] <> '#' then
            break;

        Inc(Offset);
        C := #0;
        while (Offset <= Length(Value)) and
              ((Value[Offset] >= '0') and (Value[Offset] <= '9')) do begin
            C := Char(Ord(C) * 10 + Ord(Value[Offset]) - Ord('0'));
            Inc(Offset);
        end;

        Result := Result + C;
    until Offset > Length(Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketLineEndProperty.GetLineEnd(const Value: String): String;
var
    N: integer;
begin
    Result := '';
    for N := 1 to Length(Value) do
        Result := Result + '#' + IntToStr(Ord(Value[N]));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TWSocketLineEndProperty.GetValue: String;
begin
    Result := GetLineEnd(inherited GetValue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWSocketLineEndProperty.SetValue(const Value: String);
begin
    inherited SetValue(SetLineEnd(Value));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF}

end.
