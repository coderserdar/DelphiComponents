{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE.
Description:  MD5 self test routine for OverByteIcsMD5 unit.
Creation:     Aug 01, 2007
Version:      7.00
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
Dec 21, 2008 V1.01 F.Piette added a string cast in RunButtonClick to avoid
             a warning when compiling with Delphi 2009.
Apr 16, 2009 V7.00 Angus allow choose specific file, compare buffered and
             non-buffered MD5 and CRC32B


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMD5Test1;

{$I OverbyteIcsDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsMD5, OverbyteIcsCrc, OverbyteIcsFtpSrvT;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    RunButton: TButton;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    TestFileName: TEdit;
    dpFtpFileMd5: TButton;
    doFileMd5: TButton;
    doSelectFile: TButton;
    doFileCrcB: TButton;
    doFtpFileCrcB: TButton;
    procedure RunButtonClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure doSelectFileClick(Sender: TObject);
    procedure doFileMd5Click(Sender: TObject);
    procedure dpFtpFileMd5Click(Sender: TObject);
    procedure doFileCrcBClick(Sender: TObject);
    procedure doFtpFileCrcBClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

const
    // Strings to test MD5. Expected checksums are below.
    // If you change strings, be sure tochange also expected checksums.
    MD5TestStrings : array [0..6] of String = (
        '' ,
        'a' ,
        'abc' ,
        'message digest' ,
        'abcdefghijklmnopqrstuvwxyz' ,
        'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789' ,
        '12345678901234567890123456789012345678901234567890123456789012' +
        '345678901234567890');

    // Expected MD5 checksum for the above strings
    MD5TestResults : array [0..6] of String = (
        'D41D8CD98F00B204E9800998ECF8427E',
        '0CC175B9C0F1B6A831C399E269772661',
        '900150983CD24FB0D6963F7D28E17F72',
        'F96B697D7CB7938D525A2F31AAF161D0',
        'C3FCD3D76192E4007DFB496CCA67E13B',
        'D174AB98D277D9F5A5611C2C9F419D9F',
        '57EDF4A22BE3C955AC49DA2E2107B67A');


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MD5SelfTest(Verbose : Boolean) : Boolean;
var
    I      : Integer;
    MD5Sum : String;
    Buf    : String;
begin
    for I := 0 to 6 do begin
        if Verbose then
            Buf := 'MD5 test #' + IntToStr(I + 1) + ': ';
        MD5Sum := StrMD5(MD5TestStrings[I]);
        if not SameText(MD5Sum, MD5TestResults[I]) then begin
            if Verbose then
                Form1.Memo1.Lines.Add(Buf + 'failed');
            Result := TRUE;
            Exit;
        end;
        if Verbose then
            Form1.Memo1.Lines.Add(Buf + 'passed');
    end;
    Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.Button1Click(Sender: TObject);
var
    MD5Context : TMD5Context;
    I          : Integer;
begin
    FillChar(MD5Context, SizeOf(TMD5Context), #0);
    MD5Context.BufLong[0] := $12345678;
    for I := 0 to 3 do
        Memo1.Lines.Add(IntToHex(MD5GetBufChar(MD5Context, I), 2)
        {$IFNDEF SAFE}
                        + ' ' + IntToHex(MD5Context.BufChar[I], 2)
        {$ENDIF}
                        );
end;

procedure TForm1.Button2Click(Sender: TObject);
var
    MD5Context : TMD5Context;
    I          : Integer;
begin
    FillChar(MD5Context, SizeOf(TMD5Context), #0);
    MD5Context.BufLong[0] := $12345678;
    for I := 0 to 3 do begin
        MD5SetBufChar(MD5Context, I, MD5GetBufChar(MD5Context, I));
        Memo1.Lines.Add(IntToHex(MD5Context.BufLong[0], 8));
    end;


    MD5SetBufChar(MD5Context, 0, $78);
    MD5SetBufChar(MD5Context, 1, $56);
    MD5SetBufChar(MD5Context, 2, $34);
    MD5SetBufChar(MD5Context, 3, $12);
    Memo1.Lines.Add(IntToHex(MD5Context.BufLong[0], 8));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doFileCrcBClick(Sender: TObject);
var
    starttick: longword ;
    filesize: int64 ;
begin
    filesize :=  IcsGetFileSize (TestFileName.Text) ;
    if filesize < 0 then
    begin
        Form1.Memo1.Lines.Add('File not found: ' + TestFileName.Text) ;
        exit ;
    end;
    Form1.Memo1.Lines.Add ('Starting non-buffered CRC32B for: ' + TestFileName.Text +
                                                             ', Size ' + IntToKbyte (filesize)) ;
    starttick := IcsGetTickCountX ;
    Form1.Memo1.Lines.Add ('FileCRC32B: ' + FileCRC32B(TestFileName.Text)) ;
    Form1.Memo1.Lines.Add ('Took ' + IntToStr (IcsElapsedMSecs (starttick)) + 'ms') ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doFileMd5Click(Sender: TObject);
var
    starttick: longword ;
    filesize: int64 ;
begin
    filesize :=  IcsGetFileSize (TestFileName.Text) ;
    if filesize < 0 then
    begin
        Form1.Memo1.Lines.Add('File not found: ' + TestFileName.Text) ;
        exit ;
    end;
    Form1.Memo1.Lines.Add ('Starting non-buffered MD5 for: ' + TestFileName.Text +
                                                             ', Size ' + IntToKbyte (filesize)) ;
    starttick := IcsGetTickCountX ;
    Form1.Memo1.Lines.Add ('FileMD5: ' + String(FileMD5(TestFileName.Text))) ;
    Form1.Memo1.Lines.Add ('Took ' + IntToStr (IcsElapsedMSecs (starttick)) + 'ms') ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doFtpFileCrcBClick(Sender: TObject);
var
    starttick: longword ;
    filesize: int64 ;
begin
    filesize :=  IcsGetFileSize (TestFileName.Text) ;
    if filesize < 0 then
    begin
        Form1.Memo1.Lines.Add('File not found: ' + TestFileName.Text) ;
        exit ;
    end;
    Form1.Memo1.Lines.Add ('Starting buffered CRC32B for: ' + TestFileName.Text +
                                                             ', Size ' + IntToKbyte (filesize)) ;
    starttick := IcsGetTickCountX ;
    Form1.Memo1.Lines.Add ('FtpFileCRC32B: ' + FtpFileCRC32B(TestFileName.Text)) ;
    Form1.Memo1.Lines.Add ('Took ' + IntToStr (IcsElapsedMSecs (starttick)) + 'ms') ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.doSelectFileClick(Sender: TObject);
begin
    OpenDialog.FileName := TestFileName.Text ;
    if OpenDialog.Execute then
        TestFileName.Text := OpenDialog.FileName ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.dpFtpFileMd5Click(Sender: TObject);
var
    starttick: longword ;
    filesize: int64 ;
begin
    filesize :=  IcsGetFileSize (TestFileName.Text) ;
    if filesize < 0 then
    begin
        Form1.Memo1.Lines.Add('File not found: ' + TestFileName.Text) ;
        exit ;
    end;
    Form1.Memo1.Lines.Add ('Starting buffered MD5 for: ' + TestFileName.Text +
                                                             ', Size ' + IntToKbyte (filesize)) ;
    starttick := IcsGetTickCountX ;
    Form1.Memo1.Lines.Add ('FtpFileMD5: ' + FtpFileMD5(TestFileName.Text)) ;
    Form1.Memo1.Lines.Add ('Took ' + IntToStr (IcsElapsedMSecs (starttick)) + 'ms') ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TForm1.RunButtonClick(Sender: TObject);
var
    FileName : String;
    Stream   : TFileStream;
    I        : Integer;
begin
    Form1.Memo1.Clear;
{$IFDEF SAFE}
    Form1.Memo1.Lines.Add('SAFE version');
{$ENDIF}
    if MD5SelfTest(TRUE) then
        Form1.Memo1.Lines.Add('MD5 library failed')
    else
        Form1.Memo1.Lines.Add('MD5 library passed');

    FileName := 'Test.txt';
    Stream   := TFileStream.Create(FileName, fmCreate);
    for I := 1 to Length(MD5TestStrings[3]) do
        Stream.Write(MD5TestStrings[3][I], 1);
    Stream.Free;
    if not SameText(MD5TestResults[3],
                    String(FileMD5(FileName))) then
        Form1.Memo1.Lines.Add('FileMD5 failed')
    else
        Form1.Memo1.Lines.Add('FileMD5 passed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
