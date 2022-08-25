{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE. Based on work given by Louis S. Berman from
              BrainTree Ltd, lsb@braintree.com
Description:  Compute MD5 signature for a file
Creation:     Dec 04, 2002
Version:      1.01
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2002-2010 by François PIETTE
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

Updates:
Sep 08, 2004 V1.01 Unit MD5 has been renamed to IcsMD5


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program OverbyteIcsMD5File;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  OverbyteIcsMD5;

const
    MD5FileVersion = 101;
    CopyRight      = ' MD5File (c) 2002-2010 by Francois PIETTE. V1.01';

var
    I : Integer;
begin
    if ParamCount <= 0 then begin
        WriteLn('Missing file name.');
        Exit;
    end;
    for I := 1 to ParamCount do begin
        if not FileExists(ParamStr(I)) then begin
            WriteLn('File ''' + ParamStr(I) + ''' not found.');
            Exit;
        end;
        WriteLn(FileMD5(ParamStr(I)), ' ', ParamStr(I));
    end;
end.
