{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
              Using code donated by Brad Choate <choate@delphiexchange.com>
Object:       UUEncode support routine
EMail:        francois.piette@pophost.eunet.be   francois.piette@rtfm.be
WebSite:      http://www.rtfm.be/fpiette
Creation:     February 14th, 1998
Version:      1.00
Support:      Use twsocket@rtfm.be mailing list. See website for details.
Legal issues: Copyright (C) 1997 by François PIETTE 

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

Updates:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit UUEncode;

interface

uses
    SysUtils;

const
  UUEncodeVersion    = 100;
  CopyRight : String = ' UUEncodeVersion Unit (c) 1998 F. Piette V1.00 ';


procedure InitUUEncode(var hFile: File; sFile: string);
procedure DoUUEncode(var hFile: File; var sLine: string; var More: boolean);
procedure EndUUEncode(var hFile: File);

implementation

type
  TLookup = array [0..64] of Char;

const
  Base64Out: TLookup =
    (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
    'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
    'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '='
   );

{$I+}   // Activate I/O check (EInOutError exception generated)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure InitUUEncode(var hFile: File; sFile: string);
begin
    AssignFile(hFile, sFile);
    Reset(hFile, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DoUUEncode(var hFile: File; var sLine: string; var More: boolean);
var
    Count     : integer;
    DataIn    : array [0..2] of byte;
    DataOut   : array [0..80] of byte;
    ByteCount : integer;
    i         : integer;
begin
    Count := 0;
{$I-}
    while not Eof(hFile) do begin
{$I+}
        BlockRead(hFile, DataIn, 3, ByteCount);
        DataOut[Count]     := (DataIn[0] and $FC) shr 2;
        DataOut[Count + 1] := (DataIn[0] and $03) shl 4;
        if ByteCount > 1 then begin
            DataOut[Count + 1] := DataOut[Count + 1] +
                                  (DataIn[1] and $F0) shr 4;
            DataOut[Count + 2] := (DataIn[1] and $0F) shl 2;
            if ByteCount > 2 then begin
                DataOut[Count + 2] := DataOut[Count + 2] +
                                      (DataIn[2] and $C0) shr 6;
                DataOut[Count + 3] := (DataIn[2] and $3F);
            end
            else begin
                DataOut[Count + 3] := $40;
            end;
        end
        else begin
            DataOut[Count + 2] := $40;
            DataOut[Count + 3] := $40;
        end;

        for i := 0 to 3 do
            DataOut[Count + i] := Byte(Base64Out[DataOut[Count + i]]);

        Count := Count + 4;

        if Count > 59 then
            break;
    end;

    DataOut[Count] := $0;
    sLine := StrPas(@DataOut[0]);

{$I-}
    More := not Eof(hFile);
{$I+}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure EndUUEncode(var hFile: File);
begin
    CloseFile(hFile);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

