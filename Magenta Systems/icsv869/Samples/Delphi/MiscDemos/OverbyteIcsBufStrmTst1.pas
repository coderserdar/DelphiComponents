{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Creation:     March 2009
Description:  Test of buffered stream classes, Delphi 7 and better.
Version:      8.00
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2009 by François PIETTE
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
                 to François PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:
Apr 18, 2009 Added various strange looking tests I used to debug.
Feb 23, 2016 V8.00 - Angus renamed TBufferedFileStream to TIcsBufferedFileStream
                     to avoid conflicts with other libraries

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsBufStrmTst1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  OverbyteIcsTypes,
  OverbyteIcsUtils,
  OverbyteIcsMD5,
  OverbyteIcsStreams;

type
  TMainForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditLine: TEdit;
    Memo1: TMemo;
    ButtonIcsStreamReader: TButton;
    ButtonIcsStreamWriter: TButton;
    ComboBoxCodePage: TComboBox;
    ComboBoxLineBreaks: TComboBox;
    EditLineCount: TEdit;
    CheckBoxDisplay: TCheckBox;
    ButtonClearMemo: TButton;
    ButtonStreamWriter: TButton;
    ButtonStreamReader: TButton;
    ButtonDetectLineBreakStyle: TButton;
    CheckBoxAppend: TCheckBox;
    Label5: TLabel;
    EditFileName: TEdit;
    Label6: TLabel;
    EditBufSize: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    ButtonBufferedFileStreamRead: TButton;
    ButtonBufferedFileStreamWrite: TButton;
    EditBlockSize: TEdit;
    EditLoops: TEdit;
    Label12: TLabel;
    GroupBox2: TGroupBox;
    Label13: TLabel;
    ButtonFileStreamRead: TButton;
    ButtonFileStreamWrite: TButton;
    StreamGroup: TRadioGroup;
    ButtonSeekTest: TButton;
    ButtonReadWriteTest: TButton;
    ButtonWriteTest: TButton;
    ButtonSeekAndReplace: TButton;
    ButtonRandomReadWrite: TButton;
    Panel1: TPanel;
    GroupBox3: TGroupBox;
    Label7: TLabel;
    ButtonBufferdStreamRead: TButton;
    ButtonBufferedStreamWrite: TButton;
    procedure ButtonClearMemoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditBlockSizeChange(Sender: TObject);
    procedure ButtonIcsStreamWriterClick(Sender: TObject);
    procedure ButtonIcsStreamReaderClick(Sender: TObject);
{$IF Rtlversion > 19} { Delphi 2009 only }
    procedure ButtonStreamWriterClick(Sender: TObject);
    procedure ButtonStreamReaderClick(Sender: TObject);
{$IFEND}
    procedure ButtonDetectLineBreakStyleClick(Sender: TObject);
    procedure ButtonSeekTestClick(Sender: TObject);
    procedure ButtonWriteTestClick(Sender: TObject);
    procedure ButtonSeekAndReplaceClick(Sender: TObject);
    procedure ButtonReadWriteTestClick(Sender: TObject);
    procedure ButtonRandomReadWriteClick(Sender: TObject);
    procedure ButtonBufferedFileStreamReadClick(Sender: TObject);
    procedure ButtonBufferedStreamReadClick(Sender: TObject);
    procedure ButtonBufferedFileStreamWriteClick(Sender: TObject);
    procedure ButtonBufferedStreamWriteClick(Sender: TObject);
    procedure ButtonFileStreamReadClick(Sender: TObject);
    procedure ButtonFileStreamWriteClick(Sender: TObject);
  private
    function  DetectLbStyle(const FileName: String): TIcsLineBreakStyle;
    procedure SeekTest(X: TStream);
    procedure DisplayStream(X: TStream);
    procedure Display(const Msg: String);
    function  CreateStream(ABufferSize: LongInt): TStream;
    procedure WriteTest(X: TStream);
    procedure ReadWriteTest(X: TStream);
    procedure SeekAndReplace(X: TStream);

  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormCreate(Sender: TObject);
begin
    Memo1.Clear;
    Constraints.MinWidth := Width;
{$IF Rtlversion < 20}
    ButtonStreamWriter.Enabled := FALSE;
    ButtonStreamReader.Enabled := FALSE;
{$ELSE}
    ButtonStreamWriter.OnClick := ButtonStreamWriterClick;
    ButtonStreamReader.OnClick := ButtonStreamReaderClick;
{$IFEND}
    EditBlockSizeChange(nil);
    Randomize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IF Rtlversion > 19}
procedure TMainForm.ButtonStreamReaderClick(Sender: TObject);
var
    X : TStreamReader;
    S : String;
    A, B, C : Int64;
begin
    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    {X := TStreamReader.Create(EditFileName.Text, TEncoding.Default, TRUE,
                              StrToIntDef(EditBufSize.Text, 4096)); // DetectBOM doesn't work QC #72913}
    X := TStreamReader.Create(EditFileName.Text, TRUE); // uses 1024 BufferSize performs slower with a larger buffer
    try
        try
            while not X.EndOfStream do
            begin
                S := X.ReadLine;
                if CheckBoxDisplay.Checked then
                    Memo1.Lines.Add(S);
            end;
        except
            raise;
        end;
    finally
        X.Free;
    end;

    QueryPerformanceCounter(C);
    Memo1.Lines.Add('Duration: ' + IntToStr((C - B) * 1000 div A));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonStreamWriterClick(Sender: TObject);
var
    X : TStreamWriter;
    Strm : TFileStream;
    I : Integer;
    A, B, C : Int64;
    Enc : TEncoding;
    BOM: TBytes;
    S : String;
    LB: String;
    LineBreakStyle : TIcsLineBreakStyle;
begin
    case ComboBoxCodePage.ItemIndex of
        1 : Enc := TEncoding.UTF8;
        2 : Enc := TEncoding.Unicode;
        3 : Enc := TEncoding.BigEndianUnicode;
        else
            Enc := TEncoding.Default;
    end;

    S := EditLine.Text;

    if CheckBoxAppend.Checked and FileExists(EditFileName.Text) then
        LineBreakStyle := DetectLbStyle(EditFileName.Text)
    else
        LineBreakStyle := TIcsLineBreakStyle(ComboBoxLineBreaks.ItemIndex);

    case LineBreakStyle of
        ilbsLF : LB := #10;
        ilbsCR : LB := #13;
        else
            LB := #13#10;
    end;

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    if CheckBoxAppend.Checked and FileExists(EditFileName.Text) then
    begin
        Strm := TFileStream.Create(EditFileName.Text, fmOpenReadWrite or fmShareDenyWrite);
        try
            SetLength(BOM, 3);
            FillChar(BOM[0], 3, #0);
            Strm.Read(BOM[0], 3);
            TEncoding.GetBufferEncoding(BOM, Enc);
            Strm.Seek(0, soEnd);
            X := TStreamWriter.Create(Strm, Enc, StrToIntDef(EditBufSize.Text, 4096));
            try
                X.NewLine := LB;
                for I := 1 to StrToIntDef(EditLineCount.Text, 0) do
                    X.WriteLine(S);
            finally
                X.Free;
            end;
        finally
            Strm.Free;
        end;
    end
    else begin
        X := TStreamWriter.Create(EditFileName.Text, False, Enc,
                                  StrToIntDef(EditBufSize.Text, 4096));
        try
            X.NewLine := LB;
            BOM := Enc.GetPreamble;
            X.BaseStream.Write(BOM[0], Length(BOM));
            for I := 1 to StrToIntDef(EditLineCount.Text, 0) do
                X.WriteLine(S);
        finally
            X.Free;
        end;
    end;
    QueryPerformanceCounter(C);
    Memo1.Lines.Add('Duration: ' + IntToStr((C - B) * 1000 div A));
end;
{$IFEND}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonIcsStreamReaderClick(Sender: TObject);
var
    X : TIcsStreamReader;
    S : String;
    A, B, C : Int64;
begin
    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);
    X := TIcsStreamReader.Create(EditFileName.Text, TRUE, CP_ACP,
                                 StrToIntDef(EditBufSize.Text, 4096));
    try
        while X.ReadLine(S) do
            if CheckBoxDisplay.Checked then
            begin
            {$IF Rtlversion > 19}
                Memo1.Lines.Add(S);
            {$ELSE}
                //if X.CurrentCodePage = CP_UTF8 then
                { S contains the utf8 string, let's convert, or     }
                { we could use the WideString overload of ReadLine  }
                { instead.                                          }
                //    Memo1.Lines.Add(Utf8ToStringA(S))
                //else
                    Memo1.Lines.Add(S);
            {$IFEND}
            end;
    finally
        X.Free;
    end;

    QueryPerformanceCounter(C);
    Memo1.Lines.Add('Duration: ' + IntToStr((C - B) * 1000 div A));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMainForm.DetectLbStyle(const FileName: String): TIcsLineBreakStyle;
var
    X : TIcsStreamReader;
begin
    if FileExists(FileName) then begin
        X := TIcsStreamReader.Create(FileName);
        try
            Result := X.DetectLineBreakStyle;
        finally
            X.Free;
        end;
    end
    else
        Result := ilbsCRLF;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonIcsStreamWriterClick(Sender: TObject);
var
    X : TIcsStreamWriter;
    I : Integer;
    A, B, C : Int64;
    CP : Cardinal;
    S : String;
    Append: Boolean;
    DetectCP: Boolean;
    LineBreakStyle : TIcsLineBreakStyle;
begin
    case ComboBoxCodePage.ItemIndex of
        1 : CP := CP_UTF8;
        2 : CP := 1200;
        3 : CP := 1201;
        else
            CP := CP_ACP;
    end;
    S := EditLine.Text;
    Append   := CheckBoxAppend.Checked;
    DetectCP := Append;

    if Append and FileExists(EditFileName.Text) then
        LineBreakStyle := DetectLbStyle(EditFileName.Text)
    else
        LineBreakStyle := TIcsLineBreakStyle(ComboBoxLineBreaks.ItemIndex);

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    X := TIcsStreamWriter.Create(EditFileName.Text, Append, DetectCP, CP,
                               StrToIntDef(EditBufSize.Text, 4096));
    try
        if (not Append) or (not FileExists(EditFileName.Text)) then
            X.WriteBOM;
        X.LineBreakStyle := LineBreakStyle;
        for I := 1 to StrToIntDef(EditLineCount.Text, 0) do
            X.WriteLine(S);
    finally
        X.Free;
    end;

    QueryPerformanceCounter(C);
    Memo1.Lines.Add('Duration: ' + IntToStr((C - B) * 1000 div A));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonDetectLineBreakStyleClick(Sender: TObject);
var
    X : TIcsStreamReader;
begin
    X := TIcsStreamReader.Create(EditFileName.Text);
    try
        Memo1.Lines.Add(ComboBoxLineBreaks.Items[(Ord(X.DetectLineBreakStyle))]);
    finally
        X.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonClearMemoClick(Sender: TObject);
begin
    Memo1.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.Display(const Msg: String);
begin
    Memo1.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.DisplayStream(X: TStream);
begin
    Display(X.ClassName + ' Size: ' + IntToStr(X.Size));
    if X is TIcsBufferedFileStream then
        Display(X.ClassName + ' FastSize: ' + IntToStr(TIcsBufferedFileStream(X).FastSize));
    Display(X.ClassName + ' Position: ' + IntToStr(X.Position) + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMainForm.CreateStream(ABufferSize: LongInt): TStream;
begin
    {
    TMemoryStream
    TFileStream
    TIcsBufferedFileStream
    TBufferedStream
    }
    case StreamGroup.ItemIndex of
        0 : Result := TFileStream.Create(EditFileName.Text, fmOpenReadWrite or fmShareDenyNone);
        1 : Result := TIcsBufferedFileStream.Create(EditFileName.Text, fmOpenReadWrite or fmShareDenyNone, ABufferSize);
        2 : Result := TIcsBufferedStream.Create(EditFileName.Text, fmOpenReadWrite or fmShareDenyNone, ABufferSize);
        else
            Result := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.SeekTest(X: TStream);
var
    I  : Integer;
begin
    Display('"Size := 128 and Seek(-64, soEnd)"');
    X.Size := 128;
    X.Seek(-64, soEnd);
    Display('SeekResult Seek(-100, soBeginning) =' + IntToStr(X.Seek(-100, soBeginning)));
    DisplayStream(X);

    Display('Size := 0');
    X.Size := 0;
    Display('SeekResult Seek(1000, soBeginning)=' + IntToStr(X.Seek(1000, soBeginning)));
    DisplayStream(X);

    Display('Size := ' + IntToStr(MIN_BUFSIZE + 1));
    X.Size := MIN_BUFSIZE + 1;
    DisplayStream(X);

    Display('"Size := -1"');
    if not (X is TMemoryStream) then
    begin
        X.Size := -1;
        DisplayStream(X);
    end;
    Display('"Size := 0"');
    X.Size := 0;
    DisplayStream(X);

    Display('"Position := 512 and seek 10 * one byte soCurrent"');
    X.Position := 512;
    for I := 1 to 10 do
    begin
        X.Seek(1, soCurrent);
        DisplayStream(X);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.SeekAndReplace(X: TStream);
const
    Ch : AnsiChar = 'Z';
begin
    { Replaces the "X" and "Y" in the file created by WriteTest }
    { or ReadWriteTest.                                         }

    X.Seek(-1, soEnd);
    X.Write(Ch, 1);

    X.Seek(0, soBeginning);
    X.Write(Ch, 1);

    X.Seek(MIN_BUFSIZE - 2, soCurrent);
    X.Write(Ch, 1);
    X.Write(Ch, 1);

    X.Seek(-MIN_BUFSIZE, soEnd);
    X.Write(Ch, 1);
    X.Seek(-2, soCurrent);
    X.Write(Ch, 1);

    X.Seek(-MIN_BUFSIZE * 2, soEnd);
    X.Write(Ch, 1);
    X.Seek(MIN_BUFSIZE * 2 -1, soBeginning);
    X.Write(Ch, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.WriteTest(X: TStream);
var
    I  : Integer;
    Buf: TBytes;
begin
    { Open resulting text file in a text editor                 }
    SetLength(Buf, MIN_BUFSIZE);
    FillChar(Buf[0], Length(Buf), '-');
    Buf[0] := Byte('X');
    Buf[MIN_BUFSIZE - 1] := Byte('Y');

    X.Size := 0;
    //DisplayStream(X);

    for I := 1 to 4 do
    begin
        X.Write(Buf[0], Length(Buf));
        //DisplayStream(X);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ReadWriteTest(X: TStream);
var
    I  : Integer;
    Buf: TBytes;
begin
    { Open resulting text file in a text editor                 }
    SetLength(Buf, MIN_BUFSIZE);
    FillChar(Buf[0], Length(Buf), '-');
    Buf[0] := Byte('X');
    Buf[MIN_BUFSIZE - 1] := Byte('Y');

    X.Size := 0;
    //DisplayStream(X);

    for I := 1 to 4 do
    begin
        X.Write(Buf[0], Length(Buf));
        X.Seek(-Length(Buf) div 2, soEnd);
        X.Read(Buf[Length(Buf) div 2], Length(Buf) div 2);
        //DisplayStream(X);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonSeekTestClick(Sender: TObject);
var
    Strm : TStream;
begin
    Strm := CreateStream(StrToIntDef(EditBufSize.Text, MIN_BUFSIZE));
    try
        SeekTest(Strm);
    finally
        Strm.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonSeekAndReplaceClick(Sender: TObject);
var
    Strm : TStream;
    S    : AnsiString;
begin
    Strm := CreateStream(MIN_BUFSIZE);
    try
        SeekAndReplace(Strm);
        Strm.Seek(0, soBeginning);
        SetLength(S, Strm.Size);
        Strm.ReadBuffer(PAnsiChar(S)^, Strm.Size);
        Memo1.Lines.Add(String(S));
    finally
        Strm.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonWriteTestClick(Sender: TObject);
var
    Strm : TStream;
    S    : AnsiString;
begin
    Strm := CreateStream(StrToIntDef(EditBufSize.Text, MIN_BUFSIZE));
    try
        WriteTest(Strm);
        Strm.Seek(0, soBeginning);
        SetLength(S, Strm.Size);
        Strm.ReadBuffer(PAnsiChar(S)^, Strm.Size);
        Memo1.Lines.Add(String(S));
    finally
        Strm.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonReadWriteTestClick(Sender: TObject);
var
    Strm : TStream;
    S    : AnsiString;
begin
    Strm := CreateStream(StrToIntDef(EditBufSize.Text, MIN_BUFSIZE));
    try
        ReadWriteTest(Strm);
        Strm.Seek(0, soBeginning);
        SetLength(S, Strm.Size);
        Strm.ReadBuffer(PAnsiChar(S)^, Strm.Size);
        Memo1.Lines.Add(String(S));
    finally
        Strm.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    TestFileSize = MAX_BUFSIZE * 15 + 13;


procedure TMainForm.ButtonRandomReadWriteClick(Sender: TObject);
var
    Strm      : TStream;
    OldMd5    : AnsiString;
    I         : Integer;
    Buf       : TBytes;
    Rnd       : Int64;
    MaxPos    : Integer;
    BytesRead : Integer;
    A, B, C : Int64;
begin
    { Creates a file, seeks, reads and writes, the MD5 sum may not change }

    Panel1.Caption := '..';
    Panel1.Update;

    { Create file with TFileStream }
    SetLength(Buf, TestFileSize);
    for I := 0 to TestFileSize -1 do
        Buf[I] := Random(255);
    Strm := TFileStream.Create(EditFileName.Text, fmCreate);
    try 
        Strm.WriteBuffer(Buf[0], TestFileSize);
    finally
        Strm.Free;
    end;

    { Get MD5 checksum }
    OldMd5 := FileMd5(EditFileName.Text);

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    Strm := CreateStream(StrToIntDef(EditBufSize.Text, MIN_BUFSIZE));
    try
        FillChar(Buf[0], Length(Buf), #0);
        MaxPos := TestFileSize - 1;
        for I := 1 to 50 do
        begin
            Rnd := Random(MaxPos);
            Strm.Seek(Rnd, soBeginning);
            BytesRead := Strm.Read(Buf[0], MaxPos - Rnd);
            Strm.Seek(-BytesRead, soCurrent);
            Strm.WriteBuffer(Buf[0], BytesRead);

            Rnd := Random(MaxPos);
            Strm.Seek(-Rnd, soEnd);
            BytesRead := Strm.Read(Buf[0], Rnd);
            Strm.Seek(-BytesRead div 2, soCurrent);
            Strm.WriteBuffer(Buf[BytesRead - BytesRead div 2], BytesRead div 2);

            Rnd := Random(MaxPos);
            Strm.Seek(-Rnd, soEnd);
            BytesRead := Strm.Read(Buf[0], Rnd);
            Strm.Seek(-BytesRead div 3, soCurrent);
            Strm.WriteBuffer(Buf[BytesRead - BytesRead div 3], BytesRead div 3);
        end;
        QueryPerformanceCounter(C);
        Memo1.Lines.Add(Strm.ClassName + ' Duration: ' + IntToStr((C - B) * 1000 div A));
    finally
        Strm.Free;
    end;    
    { Compare MD5 checksums }
    if FileMd5(EditFileName.Text) = OldMd5 then
        Panel1.Caption := 'MD5 OK'
    else
        Panel1.Caption := 'ERROR';

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    DefaultBlockSize   = 1024;
    DefaultBufferSize  = 4096;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonBufferedFileStreamReadClick(Sender: TObject);
var
    Strm : TIcsBufferedFileStream;
    BlockSize : Integer;
    A, B, C : Int64;
    Buf : TBytes;
begin
    Label10.Caption := '...';
    Label10.Update;
    BlockSize := StrToIntDef(EditBlockSize.Text, DefaultBlockSize);
    SetLength(Buf, BlockSize);

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    Strm := TIcsBufferedFileStream.Create(EditFileName.Text,
                                       fmOpenRead or fmShareDenyWrite,
                                       StrToIntDef(EditBufSize.Text,
                                       DefaultBufferSize));
    try
        while Strm.Read(Buf[0], BlockSize) = BlockSize do;
    finally
        Strm.Free;
    end;

    QueryPerformanceCounter(C);
    Label10.Caption := 'Duration: ' + IntToStr((C - B) * 1000 div A);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonFileStreamReadClick(Sender: TObject);
var
    Strm : TFileStream;
    BlockSize : Integer;
    A, B, C : Int64;
    Buf : TBytes;
begin
    Label13.Caption := '...';
    Label13.Update;
    BlockSize := StrToIntDef(EditBlockSize.Text, DefaultBlockSize);
    SetLength(Buf, BlockSize);

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    Strm := TFileStream.Create(EditFileName.Text, fmOpenRead or fmShareDenyWrite);
    try
        while Strm.Read(Buf[0], BlockSize) = BlockSize do;
    finally
        Strm.Free;
    end;

    QueryPerformanceCounter(C);
    Label13.Caption := 'Duration: ' + IntToStr((C - B) * 1000 div A);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonBufferedFileStreamWriteClick(Sender: TObject);
var
    Strm : TIcsBufferedFileStream;
    BlockSize : Integer;
    A, B, C : Int64;
    I : Integer;
    Buf : TBytes;
begin
    Label10.Caption := '...';
    Label10.Update;
    BlockSize := StrToIntDef(EditBlockSize.Text, DefaultBlockSize);
    SetLength(Buf, BlockSize);
    FillChar(Buf[0], BlockSize, 'A');

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    Strm := TIcsBufferedFileStream.Create(EditFileName.Text, fmCreate,
                                       StrToIntDef(EditBufSize.Text,
                                       DefaultBufferSize));
    try
        for I := 1 to StrToIntDef(EditLoops.Text, 0) do
            Strm.Write(Buf[0], BlockSize);
    finally
        Strm.Free;
    end;

    QueryPerformanceCounter(C);
    Label10.Caption := 'Duration: ' + IntToStr((C - B) * 1000 div A);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonFileStreamWriteClick(Sender: TObject);
var
    Strm : TFileStream;
    BlockSize : Integer;
    A, B, C : Int64;
    I : Integer;
    Buf : TBytes;
begin
    Label13.Caption := '...';
    Label13.Update;
    BlockSize := StrToIntDef(EditBlockSize.Text, DefaultBlockSize);
    SetLength(Buf, BlockSize);
    for I := 0 to BlockSize -1 do
      Buf[I] := Byte(Random(255));

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    Strm := TFileStream.Create(EditFileName.Text, fmCreate);
    try
        for I := 1 to StrToIntDef(EditLoops.Text, 0) do
            Strm.Write(Buf[0], BlockSize);
    finally
        Strm.Free;
    end;

    QueryPerformanceCounter(C);
    Label13.Caption := 'Duration: ' + IntToStr((C - B) * 1000 div A);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.EditBlockSizeChange(Sender: TObject);
var
    Size: Integer;
begin
    Size := StrToIntDef(EditBlockSize.Text, 0) * StrToIntDef(EditLoops.Text, 0);
    Label12.Caption := 'Write FileSize: ' + IntToStr(Size);
    Label12.Update;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonBufferedStreamWriteClick(Sender: TObject);
var
    Strm : TIcsBufferedStream;
    BlockSize : Integer;
    A, B, C : Int64;
    I : Integer;
    Buf : TBytes;
begin
    Label7.Caption := '...';
    Label7.Update;
    BlockSize := StrToIntDef(EditBlockSize.Text, DefaultBlockSize);
    SetLength(Buf, BlockSize);
    FillChar(Buf[0], BlockSize, 'A');

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    Strm := TIcsBufferedStream.Create(EditFileName.Text, fmCreate,
                                      StrToIntDef(EditBufSize.Text,
                                      DefaultBufferSize));
    try
        for I := 1 to StrToIntDef(EditLoops.Text, 0) do
            Strm.Write(Buf[0], BlockSize);
    finally
        Strm.Free;
    end;

    QueryPerformanceCounter(C);
    Label7.Caption := 'Duration: ' + IntToStr((C - B) * 1000 div A);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ButtonBufferedStreamReadClick(Sender: TObject);
var
    Strm : TIcsBufferedStream;
    BlockSize : Integer;
    A, B, C : Int64;
    Buf : TBytes;
begin
    Label7.Caption := '...';
    Label7.Update;
    BlockSize := StrToIntDef(EditBlockSize.Text, DefaultBlockSize);
    SetLength(Buf, BlockSize);

    QueryPerformanceFrequency(A);
    QueryPerformanceCounter(B);

    Strm := TIcsBufferedStream.Create(EditFileName.Text,
                                      fmOpenRead or fmShareDenyWrite,
                                      StrToIntDef(EditBufSize.Text,
                                      DefaultBufferSize));
    try
        while Strm.Read(Buf[0], BlockSize) = BlockSize do;
    finally
        Strm.Free;
    end;

    QueryPerformanceCounter(C);
    Label7.Caption := 'Duration: ' + IntToStr((C - B) * 1000 div A);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
