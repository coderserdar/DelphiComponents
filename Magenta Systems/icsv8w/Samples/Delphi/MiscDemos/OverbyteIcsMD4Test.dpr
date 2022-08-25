{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       MD4 unit test.
Creation:     September 02, 2006
Version:      6.00
EMail:        francois.piette@overbyte.be   http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2006 by François PIETTE
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program OverbyteIcsMD4Test;

{$APPTYPE CONSOLE}
{$I OverbyteIcsDefs.inc}

uses
  SysUtils, OverbyteIcsMD4;

function MD4SelfTest : Boolean;
const
  Test1Out: TMD4Digest =
    ($a4,$48,$01,$7a,$af,$21,$d8,$52,$5f,$c1,$0a,$e8,$7a,$a6,$72,$9d);
  Test2Out: TMD4Digest =
    ($d7,$9e,$1c,$30,$8a,$a5,$bb,$cd,$ee,$a8,$ed,$63,$df,$41,$2d,$a9);
var
    MD4Context : TMD4Context;
    TestOut    : TMD4Digest;
    I          : Integer;
begin
    MD4Init(MD4Context);
    MD4UpdateStr(MD4Context, 'abc');
    MD4Final(MD4Context, TestOut);
    Result := TRUE;
    for I := Low(TestOut) to High(TestOut) do begin
        if TestOut[I] <> Test1Out[I] then begin
            Result := FALSE;
            break;
        end;
    end;
    if Result then begin
        MD4Init(MD4Context);
        MD4UpdateStr(MD4Context, 'abcdefghijklmnopqrstuvwxyz');
        MD4Final(MD4Context, TestOut);
        for I := Low(TestOut) to High(TestOut) do begin
            if TestOut[I] <> Test2Out[I] then begin
                Result := FALSE;
                break;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
    try
        if MD4SelfTest then
            WriteLn('MD4 Test passed.')
        else
            WriteLn('MD4 Test failed.');
    except
        on E:Exception do
            WriteLn(E.Classname, ': ', E.Message);
    end;
    WriteLn('Hit enter...');
    ReadLn;
end.
