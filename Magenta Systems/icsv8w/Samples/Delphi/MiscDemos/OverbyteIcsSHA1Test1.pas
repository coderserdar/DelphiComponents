{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE.
Description:  SHA1 self test routine for OverByteIcsSHA1 unit.
Creation:     Aug 01, 2007
Version:      6.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2007-2010 by François PIETTE
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
Jul 19, 2008 V6.00 F.Piette made some changes for Unicode
Dec 22, 2008 V6.01 F.Piette added a string cast in Button1Click to avoid
                   a warning with Delphi 2009.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSHA1Test1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsSha1, StdCtrls;

type
  TForm2 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
const TEST1   = 'abc';
      TEST2a  = 'abcdbcdecdefdefgefghfghighijhi';
      TEST2b  = 'jkijkljklmklmnlmnomnopnopq';
      TEST2   = TEST2a + TEST2b;
      TEST3   = 'a';
      TEST4a  = '01234567012345670123456701234567';
      TEST4b  = '01234567012345670123456701234567';
      TEST4   = TEST4a + TEST4b;
      testarray: array[0..3] of AnsiString = ( TEST1, TEST2, TEST3, TEST4 );
      repeatcount: array[0..3] of Integer = ( 1, 1, 1000000, 10 );
      resultarray: array [0..3] of AnsiString = (
             'A9 99 3E 36 47 06 81 6A BA 3E 25 71 78 50 C2 6C 9C D0 D8 9D',
             '84 98 3E 44 1C 3B D2 6E BA AE 4A A1 F9 51 29 E5 E5 46 70 F1',
             '34 AA 97 3C D4 C4 DA A4 F6 1E EB 2B DB AD 27 31 65 34 01 6F',
             'DE A3 56 A2 CD DD 90 C7 A7 EC ED C5 EB B5 63 93 4F 46 04 52' );
var   sha: SHA1Context;
      i, j, err: Integer;
      Message_Digest: SHA1Digest;
      s: String;
begin
    for j := 0 to 3 do begin
//        ListBox1.Items.Add( Format( 'Test %d: %d, "%s"',
//                            [ j+1, repeatcount[j], testarray[j] ] ) );
        ListBox1.Items.Add('Test ' + IntToStr(j+1) + ': ' +
                           IntToStr(repeatcount[j]) +
                           ', "' + String(testarray[j]) + '"');

        err := SHA1Reset(sha);
        if (err<>0) then begin
            ListBox1.Items.Add('SHA1Reset Error ' + IntToStr(err));
            break;    //* out of for j loop */
        end;

        for i := 0 to repeatcount[j]-1 do begin
            err := SHA1Input( sha,
                              {$IFNDEF CLR}PAnsiChar{$ENDIF}(testarray[j]),
                              length(testarray[j]) );
            if (err<>0) then begin
               ListBox1.Items.Add('SHA1Input Error ' + INtToStr(err));
               break;    //* out of for i loop */
            end;
        end;

        err := SHA1Result(sha, Message_Digest);
        if (err<>0) then begin
            ListBox1.Items.Add(
            'SHA1Result Error ' + IntToStr(Err) +
            ', could not compute message digest.');
        end else begin
              s := '';
              for i := 0 to 19 do begin
                  s := s + IntToHex(ord(Message_Digest[i]), 2) + ' ';
              end;
              ListBox1.Items.Add( 'Result: ' + s );
        end;

        ListBox1.Items.Add( 'Wanted: ' + Format('%s', [resultarray[j]] ) );
    end;
end;

end.
