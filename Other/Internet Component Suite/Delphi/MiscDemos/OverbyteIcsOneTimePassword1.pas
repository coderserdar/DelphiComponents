{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  One Time Password test routines for OverByteIcsOneTimePw unit.
Creation:     Nov 13, 2007
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2007 by François PIETTE
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

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsOneTimePassword1;

{$I OverbyteIcsDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsOneTimePw;

type
  TForm1 = class(TForm)
    Log: TMemo;
    doNewChallenge: TButton;
    PassMethod: TComboBox;
    Label1: TLabel;
    doNextChallenge: TButton;
    LabelSeed: TLabel;
    LabelSequence: TLabel;
    HexPassword: TCheckBox;
    LabelResposnse: TLabel;
    LabelChecked: TLabel;
    ChallengeResponse: TEdit;
    UserPassword: TEdit;
    Label2: TLabel;
    doPassResp: TButton;
    doAutoTest: TButton;
    doExit: TButton;
    doCheckOtp: TButton;
    procedure doNewChallengeClick(Sender: TObject);
    procedure doNextChallengeClick(Sender: TObject);
    procedure doPassRespClick(Sender: TObject);
    procedure doAutoTestClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure doCheckOtpClick(Sender: TObject);
  end;

Type
    TOtpTests = record
        Method: TOtpMethod;
        UserPw: string;
        Seed: string;
        Seq: integer;
        HexRes: string ;
        SixWRes: string ;
    end;

var
    Form1: TForm1;
    CurrOtpSeed: string ;
    CurrOtpSequence: integer ;
    CurrTest: integer = 1 ;
    CurrOtpPassword: string ;

{ test examples taken from RCF2289 document }
    OtpTests: array [1..13] of TOtpTests = (
     (Method: OtpKeyMd5; UserPW: 'A_Valid_Pass_Phrase'; Seed: 'AValidSeed';
        Seq: 99; HexRes: '85c43ee03857765b'; SixWRes: 'FOWL KID MASH DEAD DUAL OAF'),
     (Method: OtpKeyMd5; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 0; HexRes: '9E87 6134 D904 99DD'; SixWRes: 'INCH SEA ANNE LONG AHEM TOUR'),
     (Method: OtpKeyMd5; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 1; HexRes: '7965 E054 36F5 029F'; SixWRes: 'EASE OIL FUM CURE AWRY AVIS'),
     (Method: OtpKeyMd5; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 99; HexRes: '50FE 1962 C496 5880'; SixWRes: 'BAIL TUFT BITS GANG CHEF THY'),
     (Method: OtpKeyMd5; UserPW: 'AbCdEfGhIjK'; Seed: 'alpha1';
        Seq: 0; HexRes: '8706 6DD9 644B F206'; SixWRes: 'FULL PEW DOWN ONCE MORT ARC'),
     (Method: OtpKeyMd5; UserPW: 'AbCdEfGhIjK'; Seed: 'alpha1';
        Seq: 1; HexRes: '7CD3 4C10 40AD D14B'; SixWRes: 'FACT HOOF AT FIST SITE KENT'),
     (Method: OtpKeyMd5; UserPW: 'AbCdEfGhIjK'; Seed: 'alpha1';
        Seq: 99; HexRes: '5AA3 7A81 F212 146C'; SixWRes: 'BODE HOP JAKE STOW JUT RAP'),
     (Method: OtpKeyMd4; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 0; HexRes: 'D185 4218 EBBB 0B51'; SixWRes: 'ROME MUG FRED SCAN LIVE LACE'),
     (Method: OtpKeyMd4; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 1; HexRes: '6347 3EF0 1CD0 B444'; SixWRes: 'CARD SAD MINI RYE COL KIN'),
     (Method: OtpKeyMd4; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 99; HexRes: 'C5E6 1277 6E6C 237A'; SixWRes: 'NOTE OUT IBIS SINK NAVE MODE'),
     (Method: OtpKeySha1; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 0; HexRes: 'BB9E 6AE1 979D 8FF4'; SixWRes: 'MILT VARY MAST OK SEES WENT'),
     (Method: OtpKeySha1; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 1; HexRes: '63D9 3663 9734 385B'; SixWRes: 'CART OTTO HIVE ODE VAT NUT'),
     (Method: OtpKeySha1; UserPW: 'This is a test.'; Seed: 'TeSt';
        Seq: 99; HexRes: '87FE C776 8B73 CCF9'; SixWRes: 'GAFF WAIT SKID GIG SKY EYED')
      ) ;

implementation

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doNewChallengeClick(Sender: TObject);
begin
    CurrOtpSequence := -1;
    CurrOtpSeed := '';
    doNextChallengeClick (Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doNextChallengeClick(Sender: TObject);
var
    S: string ;
begin
    S := OtpCreateChallenge (TOtpMethod (PassMethod.ItemIndex),
                                                  CurrOtpSequence, CurrOtpSeed);
    ChallengeResponse.Text := S;
    LabelSequence.Caption := 'Sequence: ' + IntToStr (CurrOtpSequence);
    LabelSeed.Caption := 'Seed: ' + CurrOtpSeed;
    Log.Lines.Add ('Created OTP Challenge Response: ' + S) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doPassRespClick(Sender: TObject);
var
    OtpMethod: TOtpMethod;
    OtpSequence: integer;
    OtpSeed: string ;
begin
    LabelChecked.Caption := 'Checked:' ;
    LabelResposnse.Caption := 'Otp: ' ;
    if NOT OtpParseChallenge (ChallengeResponse.Text, OtpMethod,
                                                       OtpSequence, OtpSeed) then
    begin
        Log.Lines.Add ('Invalid OTP Challenge: ' + ChallengeResponse.Text);
        exit;
    end ;
    if NOT OtpProcessChallenge (ChallengeResponse.Text, UserPassword.Text,
                                             HexPassword.Checked, CurrOtpPassword) then
    begin
        Log.Lines.Add ('Failed to Create OTP Password Response for: ' +
                                                            ChallengeResponse.Text);
        exit;
    end ;
    LabelResposnse.Caption := 'Otp: ' + CurrOtpPassword ;
    Log.Lines.Add ('Created OTP Password Response: ' + CurrOtpPassword) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doAutoTestClick(Sender: TObject);
begin
    LabelChecked.Caption := 'Checked:' ;
    LabelResposnse.Caption := 'Otp: ' ;
    if (CurrTest <= 0) or (CurrTest > Length (OtpTests)) then CurrTest := 1;
    Log.Lines.Add ('Test ' + IntToStr (CurrTest)) ;
    with OtpTests [CurrTest] do
    begin
        PassMethod.ItemIndex := Ord (Method);
        CurrOtpSequence := Seq;
        CurrOtpSeed := Seed;
        UserPassword.Text := UserPw ;
        doNextChallengeClick (Self);
        if NOT OtpProcessChallenge (ChallengeResponse.Text, UserPassword.Text,
                                              HexPassword.Checked, CurrOtpPassword) then
            Log.Lines.Add ('Failed to Create OTP Password Response for: ' +
                                                                ChallengeResponse.Text)
        else
        begin
            LabelResposnse.Caption := 'Otp: ' + CurrOtpPassword ;
            Log.Lines.Add ('Created OTP Password Response: ' + CurrOtpPassword) ;
            if (OtpLowNoSpace (CurrOtpPassword) = OtpLowNoSpace (SixWRes)) OR
               (OtpLowNoSpace (CurrOtpPassword) = OtpLowNoSpace (HexRes)) then
            begin
                LabelChecked.Caption := 'Checked: Password Matched OK' ;
                Log.Lines.Add ('OTP Password Matched OK') ;
            end
            else
            begin
                LabelChecked.Caption := 'Checked: Password Failed' ;
                Log.Lines.Add ('Wrong OTP Password Response, Expected: ' +
                                                      SixWRes + ' or ' + HexRes) ;
            end ;
         end ;
      end ;
    inc (CurrTest) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doCheckOtpClick(Sender: TObject);
begin
    LabelChecked.Caption := 'Checked:' ;
    if CurrOtpPassword = '' then exit ;
    Log.Lines.Add ('Checking OTP Password: ' + CurrOtpPassword) ;
    if OtpTestPassword (CurrOtpPassword, UserPassword.Text,
                TOtpMethod (PassMethod.ItemIndex), CurrOtpSequence, CurrOtpSeed) then
    begin
        LabelChecked.Caption := 'Checked: Password Matched OK' ;
        Log.Lines.Add ('OTP Password Matched OK') ;
    end
    else
    begin
        LabelChecked.Caption := 'Checked: Password Failed' ;
        Log.Lines.Add ('OTP Password Failed') ;
    end ;
end;

end.
