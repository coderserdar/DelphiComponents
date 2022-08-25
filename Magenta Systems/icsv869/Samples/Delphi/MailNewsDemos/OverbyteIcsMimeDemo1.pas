{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       This program is a demo for TMimeDecode component.
              TMimeDecode is a component whose job is to decode MIME encoded
              EMail messages (file attach). You can use it for example to
              decode messages received with a POP3 component.
              MIME is described in RFC-1521. headers are described if RFC-822.
Creation:     March 08, 1998
Version:      8.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2017 by François PIETTE
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
Updates:
Sep 13, 1998  V1.01 Added part and header end numbering
Feb 16/02/99  V1.02 In OnPartLine event handler, assemble line of text for
              display.
May 04, 2002  V1.03 Adapted InLineDecodeLine event to new Len argument.
              Added file store for UUEncoded files.
Nov 01, 2002  V1.04 Changed PChar arguments to Pointer to work around Delphi 7
              bug with PAnsiChar<->PChar (change has be done in component).
Oct 18, 2008  V7.15 Angus added MIME header encoding and decoding
              Added TMimeDecodeEx test button (uses no events)
              Fixed MimeDecode1InlineDecode events for D2009 (still Ansi)
Oct 23, 2008  V7.16 Arno added some test headers, demo uses TMimeDecodeW.
              Demonstrates how to display ANSI text parts properly.
              You need either Delphi 2009 or the TNT Unicode controls
              (Freeware: http://mh-nexus.de/en/tntunicodecontrols.php) to be
              able to view Unicode correctly. Define USE_TNT to use the TntMemo
              instead of TMemo.
Oct 25, 2008  V7.17 Added procedure Display buffer.
Dec 21, 2008  V7.18 F.Piette replaced StrPas() with String() to avoid a
              warning with Delphi 2009. Also added a few string cast for
              same reason.
Dec 21, 2008  V7.19 Arno reassigned MimeDecode1InlineDecodeBegin and
              MimeDecode1InlineDecodeEnd which were unassigned since rev. #198
Dec 22, 2008  V7.20 Arno - Added workaround for error "incompatible parameter list"
              in D2009. Added explicit string conversion in
              MimeDecode1InlineDecodeBegin to remove warning.
Oct 9, 2009   V7.21 Angus - more content headers shown
Feb 14, 2012  V7.22 Angus - test TMimeTypesList component
Oct 2, 2012   V8.00 Angus - restored missing buttons to form
                            DecodeFileExButton uses ContentTypeGetExtn to display Reg Extn
Apr 15, 2017  V8.01 FPiette - Removed compiler warning


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMimeDemo1;

{ Symbols defined in OverbyteIcsDefs.inc currently do not work with the     }
{ D2009 background parser/compiler! So this is a workaround.                }
{$IFDEF VER180} // 2006 and 2007
    {$DEFINE ANSI_COMPILER}
{$ENDIF}
{$IFDEF VER170}
    {$DEFINE ANSI_COMPILER}
{$ENDIF}
{$IFDEF VER150}
    {$DEFINE ANSI_COMPILER}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OverbyteIcsIniFiles,
{$IFDEF USE_TNT}
  TntStdCtrls,
{$ENDIF}
  OverbyteIcsTypes,
  OverbyteIcsMimeUtils, OverbyteIcsMimeDec,
  OverbyteIcsUtils, OverbyteIcsCharsetUtils;

const
  MimeDemoVersion    = 800;
  CopyRight : String = ' MimeDemo (c) 1998-2012 F. Piette V8.00 ';

type
{$IFDEF USE_TNT}
    TCurrentMemo = TTntMemo;
{$ELSE}
    TCurrentMemo = TMemo;
{$ENDIF}
  TMimeDecodeForm = class(TForm)
    Panel1: TPanel;
    FileEdit: TEdit;
    DecodeButton: TButton;
    MimeDecode1: TMimeDecodeW;
    Label1: TLabel;
    ClearButton: TButton;
    TextEdit: TEdit;
    Label2: TLabel;
    Decode64Button: TButton;
    Encode64Button: TButton;
    DecAutoHeaderButton: TButton;
    DecOneHeaderButton: TButton;
    EncodeOneHdrButton: TButton;
    DecodeFileExButton: TButton;
    MimeDecodeEx1: TMimeDecodeEx;
    IgnoreBlankParts: TCheckBox;
    doCreateMimeList: TButton;
    doMimefromReg: TButton;
    doMimeFromTypes: TButton;
    doMimeTestExtn: TButton;
    doMimeTestContent: TButton;
    MimeTypesList1: TMimeTypesList;
    procedure DecodeButtonClick(Sender: TObject);
    procedure MimeDecode1PartBegin(Sender: TObject);
    procedure MimeDecode1PartEnd(Sender: TObject);
    procedure MimeDecode1PartHeaderLine(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure MimeDecode1HeaderLine(Sender: TObject);
    procedure MimeDecode1PartLine(Sender: TObject; Data: Pointer;
      DataLen: Integer);
    procedure MimeDecode1HeaderBegin(Sender: TObject);
    procedure MimeDecode1HeaderEnd(Sender: TObject);
    procedure MimeDecode1PartHeaderBegin(Sender: TObject);
    procedure MimeDecode1PartHeaderEnd(Sender: TObject);
{$IFDEF ANSI_COMPILER}
    procedure MimeDecode1InlineDecodeBegin(Sender: TObject;
                                           Filename: String);
    procedure MimeDecode1InlineDecodeEnd(Sender: TObject;
                                         Filename: String);
{$ELSE}
    procedure MimeDecode1InlineDecodeBegin(Sender: TObject;
                                           Filename: AnsiString);
    procedure MimeDecode1InlineDecodeEnd(Sender: TObject;
                                         Filename: AnsiString);
{$ENDIF}
    procedure MimeDecode1InlineDecodeLine(Sender: TObject;
                                          Line: Pointer; Len : Integer);
    procedure Decode64ButtonClick(Sender: TObject);
    procedure Encode64ButtonClick(Sender: TObject);
    procedure DecAutoHeaderButtonClick(Sender: TObject);
    procedure DecOneHeaderButtonClick(Sender: TObject);
    procedure EncodeOneHdrButtonClick(Sender: TObject);
    procedure DecodeFileExButtonClick(Sender: TObject);
    procedure doCreateMimeListClick(Sender: TObject);
    procedure doMimefromRegClick(Sender: TObject);
    procedure doMimeFromTypesClick(Sender: TObject);
    procedure doMimeTestExtnClick(Sender: TObject);
    procedure doMimeTestContentClick(Sender: TObject);
  private
    Memo1          : TCurrentMemo;
    FInitialized   : Boolean;
    FIniFileName   : String;
    FLineBuf       : array [0..255] of AnsiChar;
    FCharCnt       : Integer;
    FFileStream    : TFileStream;
    FFileName      : String;
    procedure Display(Msg: UnicodeString);
    procedure DisplayBuffer;
    procedure ListMimeTypes;
  end;

var
  MimeDecodeForm: TMimeDecodeForm;

implementation

{$R *.DFM}
const
    SectionData   = 'Data';
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';
    KeyFile       = 'FileName';
    KeyText       = 'TextEdit';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.FormCreate(Sender: TObject);
begin
    Font              := Screen.IconFont;
    Memo1             := TCurrentMemo.Create(Self);
    Memo1.ParentFont  := TRUE;
    Memo1.Parent      := Self;
    Memo1.Align       := alClient;
    Memo1.ScrollBars  := ssBoth;
    Memo1.WordWrap    := FALSE;
    FIniFileName := GetIcsIniFileName;
    Memo1.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized        := TRUE;
        IniFile   := TIcsIniFile.Create(FIniFileName);
        Top       := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
        Left      := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
        Width     := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height    := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        FileEdit.Text := IniFile.ReadString(SectionData,  KeyFile,   'mime-demo1.txt');
        TextEdit.Text := IniFile.ReadString(SectionData,  KeyText,   'some text to encode');
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.WriteString(SectionData,    KeyFile,   FileEdit.Text);
    IniFile.WriteString(SectionData,    KeyText,   TextEdit.Text);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.DecodeButtonClick(Sender: TObject);
begin
    Memo1.Clear;
    Update;
    MimeDecode1.DecodeFile(FileEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.Display(Msg: UnicodeString);
begin
    Memo1.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.DisplayBuffer;
begin
    { If we want to display ANSI text written in a charset that  }
    { is not the default system charset we need to convert to    }
    { Unicode with the part code page.                           }
    if MimeDecode1.IsTextpart and
          ((MimeDecode1.PartCodePage = MimeDecode1.DefaultCodePage) or
           (IsValidCodePage(MimeDecode1.PartCodePage))) then
        Memo1.Lines.Add(AnsiToUnicode(FLineBuf, MimeDecode1.PartCodePage))
    else
        Memo1.Lines.Add(String(FLineBuf));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartBegin(Sender: TObject);
begin
    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' BEGIN ----------');
    FCharCnt := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartEnd(Sender: TObject);
begin
    if FCharCnt > 0 then begin
        FLineBuf[FCharCnt] := #0;
        DisplayBuffer;
        FCharCnt := 0;
    end;

    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' END ----------');
    { Close file, if any }
    if Assigned(FFileStream) then begin
        FFileStream.Destroy;
        FFileStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Decoded data arrives here. This routine suppose that we have text data    }
{ organized in lines.                                                       }
procedure TMimeDecodeForm.MimeDecode1PartLine(
    Sender  : TObject;
    Data    : Pointer;
    DataLen : Integer);
var
    I : Integer;
begin
    { Copy data to LineBuf until CR/LF }
    I := 0;
    while (I < DataLen) do begin
        if PAnsiChar(Data)[I] = #13 then   { Just ignre CR }
            Inc(I)
        else if PAnsiChar(Data)[I] = #10 then begin { LF is end of line }
            FLineBuf[FCharCnt] := #0;
            DisplayBuffer;
            FCharCnt := 0;
            Inc(I);
        end
        else begin
            FLineBuf[FCharCnt] := PAnsiChar(Data)[I];
            Inc(FCharCnt);
            Inc(I);
        end;
        if FCharCnt >= (High(FLineBuf) - 1) then begin
            { Buffer overflow, display data accumulated so far }
            FLineBuf[High(FLineBuf) - 1] := #0;
            DisplayBuffer;
            FCharCnt := 0;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartHeaderLine(Sender: TObject);
begin
    Display('Part header: ' + String(UnfoldHdrValue(MimeDecode1.CurrentData)));
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1HeaderLine(Sender: TObject);
begin
    Display('Msg header: ' + String(UnfoldHdrValue(MimeDecode1.CurrentData)));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1HeaderBegin(Sender: TObject);
begin
    Display('--------- HEADER BEGIN ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1HeaderEnd(Sender: TObject);
begin
    Display(#13#10'--- Some values MIME inline decoded to Unicode ---' + #13#10 +
            #9'FROM:'#9 + MimeDecode1.FromW + #13#10 +
            #9'TO:'#9 + MimeDecode1.DestW + #13#10 +
            #9'SUBJECT:'#9 + MimeDecode1.SubjectW + #13#10);
    Display('--------- HEADER END ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartHeaderBegin(Sender: TObject);
begin
    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' HEADER BEGIN ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1PartHeaderEnd(Sender: TObject);
begin
    Display('--------- PART ' +
            IntToStr(MimeDecode1.PartNumber) +
            ' HEADER END ----------');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1InlineDecodeBegin(
    Sender   : TObject;
    FileName : AnsiString);
begin
    FFileName := String(FileNAme);
    Display('--------- INLINE begin. Filename is ''' + FFileName + '''');
    Display('');
    if Assigned(FFileStream) then
        FFileStream.Destroy;        { Close previous file, if any }
    FFileStream := TFileStream.Create('MimeFile_' + FFileName, fmCreate);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1InlineDecodeEnd(
    Sender   : TObject;
    Filename : AnsiString);
begin
    Display('--------- INLINE end');
    { Close file, if any }
    if Assigned(FFileStream) then begin
        FFileStream.Destroy;
        FFileStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.MimeDecode1InlineDecodeLine(
    Sender : TObject;
    Line   : Pointer;
    Len    : Integer);
var
    LastLine : String;
    DataLine : String;
begin
    if (Line = nil) or (Len <= 0) then
        Exit;
    { If any file assigned, then write data to it }
    if Assigned(FFileStream) then
        FFileStream.Write(Line^, Len);

    SetLength(DataLine, Len);
    Move(Line^, DataLine[1], Len);
    if Memo1.Lines.Count < 1 then
        Memo1.Lines.Add(DataLine)
    else begin
        LastLine := Memo1.Lines.Strings[Memo1.Lines.Count - 2];
        Memo1.Lines.Delete(Memo1.Lines.Count - 1);
        Memo1.Lines.Delete(Memo1.Lines.Count - 1);
        LastLine := LastLine + DataLine;
        Memo1.Lines.Add(LastLine);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TMimeDecodeForm.Decode64ButtonClick(Sender: TObject);
var
    Buf : String;
    I   : Integer;
    Txt : String;
begin
    Buf := Base64Decode(TextEdit.Text);
    TextEdit.Text := Buf;
    Txt := '';
    for I := 1 to Length(Buf) do begin
        if (Buf[I] <= '!') or (Buf[I] > '~') then
            Txt := Txt + '$' + IntToHex(Ord(Buf[I]), 2)
        else
            Txt := Txt + Buf[I];
    end;
    Memo1.Lines.Add(Txt);
end;

procedure TMimeDecodeForm.Encode64ButtonClick(Sender: TObject);
var
    Buf : String;
begin
    Buf := Base64Encode(TextEdit.Text);
    TextEdit.Text := Buf;
    Memo1.Lines.Add(Buf);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{ few examples of encoded header lines (koi8-r is Cyrillic, GB2312 is Chinese,
8859-1 is Latin/Western European, 8859-5 is Latin/Cyrillic, 8859-6 is Latin/Arabic,
windows-1252 is a superset of 8859-1, see OverbyteIcsCharsetUtils for a full list  )  }

const
    TotHeaders = 34 ;
var
    TestHeaders: array [1..TotHeaders] of AnsiString = (
        'a'#1#1#1,
        'a'#1#1#1'b',
        '"test test test test test test test'#1#1#1#1#1#1'test test"',
        '=?ISO-8859-1?Q?a?='#1#1#1'=?ISO-8859-1?Q?b?=',
        '=?ISO-8859-1?Q?a?='#13#10#9'=?ISO-8859-1?Q?b?=',
        '=?ISO-8859-1?Q?a?='#13#10' =?ISO-8859-1?Q?b?=',
        '=?ISO-8859-1?Q?a?='#1#1#1'<foo@bar.com>',
        #10#10'=?ISO-8859-1?Q?a?= b'#1#1#1'=?ISO-8859-1?Q?c?=',
        '=?US-ASCII?Q?Keith_Moore?= <moore@cs.utk.edu>',
        '=?ISO-8859-1?Q?Andr=E9?= Pirard <PIRARD@vm1.ulg.ac.be>',
        '=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?=',
        '=?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?=',
        '=?ISO-8859-1?Q?a?= b=?ISO-8859-1?Q?a_b?=',
        '=?ISO-8859-1?Q?a?=  =?ISO-8859-1?Q?b?=',
        '=?iso-8859-1?q?Fred=20Mace?= <fredmace@yahoo.co.uk>',
        '=?ISO-8859-1?Q?Re=3A_=5Btwsocket=5D?= Encoded mail headers',
        '=?ISO-8859-1?B?aGk=?=',
        '=?GB2312?B?yczO8cOz0tc=?= <hk@163.com>',
        '=?GB2312?B?sOzA7c/juNvTosPAufq8yrmry77Qrbvh?=',
        '=?utf-8?B?TWljcm9zb2Z0IE91dGxvb2sgVGVzdCBNZXNzYWdl?=',
        '=?ISO-8859-1?b?UmU6S2V5cyB0byBzdGF5aW5nIHlvdW5n?=',
        '=?iso-8859-5?B?+szB1MEg4s/U18nOwQ==?= <dnevnik@liveinternet.ru>',
        '"eap2s@mtsu.eduruss.txt}" <dunman@magsys.co.uk>',
        '=?koi8-r?B?9C7yLiDs1cLRztPLycog?= <yujiko@barryland.com>',
        '=?koi8-r?B?NSDUy8EuIMTM0SDP0sfBzsnawdTP0s/XICDQ0sHaxM7Jy8/XIMkgzQ==?=',
        '=?windows-1252?Q?Don=92t_be_Blue!_Heat_up_October_with_these_Scorching_Deals_Only_at_Screwfix_?=',
        '=?iso-8859-1?Q?Integral_512MB_Pen_Drives_For_Just_=A31.29?=',
        '=?windows-1256?Q?=CF=E6=D1=C8=ED=E4_=E3=CF=C7=D1=C8=D3=CA=E5_=E6_=D3=C7=E4=CA=D1=C7=E1?=',
        '=?utf-8?B?QW5ndXMgZmFpdCBsYSBmw6p0ZSDDoCBGcmFuw6dvaXMgcXVhbmQgbCfDqXTDqSBhcnJpdmU=?=',
        '=?utf-8?Q?Angus_fait_la_f=C3=AAte_=C3=A0_Fran=C3=A7ois_quand_l''=C3=A9t=C3=A9_arrive?=',  // note escaped '' added
        '=?iso-8859-1?B?QW5ndXMgZmFpdCBsYSBm6nRlIOAgRnJhbudvaXMgcXVhbmQgbCfpdOkgYXJyaXZl?=',
        '=?iso-8859-1?Q?Angus_fait_la_f=EAte_=E0_Fran=E7ois_quand_l''=E9t=E9_arrive?=',            // note escaped '' added
        '=?utf-8?Q?Unicode_testing_-_Angus_fait_la_?==?utf-8?Q?f=C3=AAte_=C3=A0_Fran=C3=A7ois_q'+
           'uand_l''=C3=A9t=C3=A9_arri?==?utf-8?Q?ve_=28Greek:_=C5=B4=C6=9F=C6=B1=CE=A3=CE=A8=CE=A9=29?=',
        '=?utf-8?Q?Unicode_testing_-_Angus_fait_la_?='#13#10#9+
        '=?utf-8?Q?f=C3=AAte_=C3=A0_Fran=C3=A7ois_quand_l''=C3=A9t=C3=A9_arri?='#13#10#9+    // note escaped '' added
        '=?utf-8?Q?ve_=28Greek:_=C5=B4=C6=9F=C6=B1=CE=A3=CE=A8=CE=A9=29?=' );

procedure TMimeDecodeForm.DecAutoHeaderButtonClick(Sender: TObject);
var
    I: integer;
    CharSet, Unfolded: AnsiString;
    UniStr: UnicodeString ;
begin
    Display('Auto decoding test MIME Encoded Header Lines');
    for I := 1 to TotHeaders do begin
        Unfolded := UnfoldHdrValue (TestHeaders[I]);
        UniStr := DecodeMimeInlineValueEx (Unfolded, CharSet);
        Display('Raw Header: ' + String(TestHeaders[I]));
        Display('Unfolded Header: ' + String(Unfolded));
        Display('Unicode Header: ' + UniStr + ' [CharSet=' +
                                            Lowercase (String(CharSet)) + ']');
        Display('');
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.DecOneHeaderButtonClick(Sender: TObject);
var
    CharSet : AnsiString;
    UniStr  : UnicodeString ;
begin
    Display('Raw Header: ' + TextEdit.Text);
    UniStr := DecodeMimeInlineValueEx(AnsiString(TextEdit.Text), CharSet);
    Display('Unicode Header: ' + UniStr + ' [CharSet=' +
                                            String(CharSet) + ']');
    Display('');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.EncodeOneHdrButtonClick(Sender: TObject);
begin
    Display('Raw Text: ' + TextEdit.Text);
    Display('UTF-8 Text: ' + String(StringToUtf8 (TextEdit.Text)));
    Display('UTF-8 Binary: ' + String(HdrEncodeInLineEx(TextEdit.Text,
                                   SpecialsRFC822, 'B', CP_UTF8 , 72, false)));
    Display('UTF-8 Quoted: ' + String(HdrEncodeInLineEx(TextEdit.Text,
                                   SpecialsRFC822, 'Q', CP_UTF8 , 72, false)));
    Display('ISO-8859-1 Binary: ' + String(HdrEncodeInLineEx(TextEdit.Text,
                                      SpecialsRFC822, 'B', 28591, 72, false)));
    Display('ISO-8859-1 Quoted: ' + String(HdrEncodeInLineEx(TextEdit.Text,
                                      SpecialsRFC822, 'Q', 28591, 72, false)));
    Display('Current Binary: ' + String(HdrEncodeInLineEx(TextEdit.Text,
                                     SpecialsRFC822, 'B', CP_ACP, 72, false)));
    Display('Current Quoted: ' + String(HdrEncodeInLineEx(TextEdit.Text,
                                     SpecialsRFC822, 'Q', CP_ACP, 72, false)));
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.DecodeFileExButtonClick(Sender: TObject);
var
    I: integer;
    CLSID: string;
begin
    Display('MIME Decoding ' + FileEdit.Text);
    MimeDecodeEx1.MaxParts := 999 ;  // we want lots of parts
    MimeDecodeEx1.SkipBlankParts := IgnoreBlankParts.Checked ; // but not empty ones
    MimeDecodeEx1.DecodeFileEx(FileEdit.Text);   // decodes without using events, into arrays, see below
    Display('Total header lines found: ' + IntToStr (MimeDecodeEx1.TotHeaders) +
                             ', Length ' + IntToStr (MimeDecodeEx1.DecodeW.LengthHeader));
    Display('Wide Subject: ' + MimeDecodeEx1.DecodeW.SubjectW);
    if MimeDecodeEx1.TotHeaders = 0 then exit ;
    for I := 0 to Pred (MimeDecodeEx1.TotHeaders) do begin
   //       Display(MimeDecodeEx1.HeaderLines [I]);
   //       Display(DecodeMimeInlineValue (UnfoldHdrValue (MimeDecodeEx1.HeaderLines [I])));
         Display(MimeDecodeEx1.WideHeaders [I]);
    end;
    if MimeDecodeEx1.DecParts = 0 then
        Display('No parts found to decode')
    else begin
        for I := 0 to Pred (MimeDecodeEx1.DecParts) do begin
            with MimeDecodeEx1.PartInfos [I] do begin
                Display('Part '      + IntToStr (I) +
                      ', Content: '  + String(PContentType) +
                      ', Size: '     + IntToStr(PSize) +
                      ', Name: '     + PName +
                      ', FileName: ' + PFileName +
                      ', Encoding: ' + String(PEncoding) +
                      ', Charset: '  + String(PCharset) +
                      ', ApplType: ' + String(PApplType) +
                      ', Content Id: ' + String(PContentId) +
                      ', Reg Extn: ' + ContentTypeGetExtn (String(PContentType), CLSID)) ;    { V8.00 }
                 // the content of each part is in PartStream
                 // but we don't attempt to display it here, only the size
                 // you can save to file, ie PartStream.SaveToFile (fname)
                 // or load the stream into another component, ie HtmlViewer.LoadFromStream (PartStream, fname, ImgType)
            end;
        end;
        MimeDecodeEx1.Reset ;  // clear streams to free memory
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.ClearButtonClick(Sender: TObject);
begin
    Memo1.Clear;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.ListMimeTypes;
var
    TempList: TStringList;
begin
    Display ('Total Mime Extensions: ' + IntToStr (MimeTypesList1.CountExtn) +
                     ', Total Content Types: ' + IntToStr (MimeTypesList1.CountContent));
    TempList := TStringList.Create;
    try
        MimeTypesList1.GetContentTypes (TempList);
        TempList.Sort;
        Display (TempList.Text);
    finally
        TempList.Destroy;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure TMimeDecodeForm.doCreateMimeListClick(Sender: TObject);
begin
    MimeTypesList1.MimeTypeSrc := MTypeList;
    MimeTypesList1.LoadTypeList;
    Display ('Mime Types List Loaded from Defaults');
    ListMimeTypes;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.doMimefromRegClick(Sender: TObject);
var
    tickstart: longword;
begin
    tickstart := GetTickCount;
    MimeTypesList1.MimeTypeSrc := MTypeOS;
    if NOT MimeTypesList1.LoadTypeList then
        Display ('Failed to Load Mime Types from Windows Registry')
    else
    begin
        Display ('Mime Types List Loaded from Windows Registry, took ' +
                                                IntToStr (GetTickCount - tickstart) + 'ms');
        MimeTypesList1.AddContentType ('.zip', 'application/octet-stream');   // correct a Windows type we don't like
        ListMimeTypes;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.doMimeFromTypesClick(Sender: TObject);
var
    tickstart: longword;
begin
    tickstart := GetTickCount;
    MimeTypesList1.MimeTypesFile := 'mime.types';
    MimeTypesList1.MimeTypeSrc := MTypeMimeFile;
    if NOT MimeTypesList1.LoadTypeList then
        Display ('Failed to Load Mime Types from File: ' + MimeTypesList1.MimeTypesFile)
    else
    begin
        Display ('Mime Types List Loaded from  File: ' + MimeTypesList1.MimeTypesFile +
                            ', took ' + IntToStr (GetTickCount - tickstart) + 'ms');
        ListMimeTypes;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.doMimeTestContentClick(Sender: TObject);

    procedure DoTest (const Content: string);
    var
        S: string;
    begin
        S := MimeTypesList1.TypeGetExtn(Content);
        Display (Content + ' Look-up: ' + S);
    end;

begin
    DoTest ('text/html');
    DoTest ('image/jpeg');
    DoTest ('application/pdf');
    DoTest ('application/zip');
    DoTest ('application/x-msdownload');
    DoTest ('application/postscript');
    DoTest ('application/octet-stream');
    DoTest ('application/binary');
    Display ('');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecodeForm.doMimeTestExtnClick(Sender: TObject);
const
    tests = 500000;

    procedure DoTest (const Extn: string);
    var
        I: integer;
        tickstart: longword;
        S: string;
    begin
        tickstart := GetTickCount;
        for I := 1 to tests do S := MimeTypesList1.TypeFromExtn (Extn);
        Display (IntToStr (tests) + ' ' + Extn + ' Look-ups: ' + S +
                                 ', took ' + IntToStr (GetTickCount - tickstart) + 'ms');
    end;

begin
    DoTest ('.htm');
    DoTest ('.zip');
    DoTest ('.rtf');
    DoTest ('.jpeg');
    DoTest ('.iso');
    DoTest ('.eps');
    DoTest ('.xxx');
    Display ('');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

