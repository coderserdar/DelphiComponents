{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jan 15, 2005
Description:
Version:      8.00
EMail:        francois.piette@overbyte.be    http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2005-2018 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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
Oct 03, 2009 V7.01 F.Piette added file upload demo. Fixed Unicode issue with
                   answer display.
Dec 19, 2009 V7.02 Arno fixed URL encoding.
Feb 4,  2011 V7.03 Angus added bandwidth throttling using TCustomThrottledWSocket
                    Demo shows file upload duration and speed
Apr 25, 2016 V8.00 Angus added more POST upload options


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpPost1;

{$I Include\OverbyteIcsDefs.inc}
{$IFNDEF DELPHI7_UP}
    Bomb('This sample requires Delphi 7 or later');
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls, Dialogs, Buttons,
  OverbyteIcsUrl, OverbyteIcsWndControl, OverbyteIcsHttpProt,
  OverByteIcsFtpSrvT, OverbyteIcsStreams, OverbyteIcsMimeUtils,
  OverbyteIcsFormDataDecoder, OverbyteIcsUtils;

type
  THttpPostForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    FirstNameEdit: TEdit;
    LastNameEdit: TEdit;
    Label3: TLabel;
    ActionURLEdit: TEdit;
    PostButton: TButton;
    Label4: TLabel;
    HttpCli1: THttpCli;
    Label5: TLabel;
    FileNameEdit: TEdit;
    UploadButton: TButton;
    Shape1: TShape;
    Label6: TLabel;
    UploadURLEdit: TEdit;
    Label10: TLabel;
    BandwidthLimitEdit: TEdit;
    UploadMethod: TRadioGroup;
    OpenDialog: TOpenDialog;
    SelectFile: TBitBtn;
    Label7: TLabel;
    FileDescr: TEdit;
    MimeTypesList1: TMimeTypesList;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure PostButtonClick(Sender: TObject);
    procedure HttpCli1RequestDone(Sender: TObject; RqType: THttpRequest;
                                  ErrCode: Word);
    procedure UploadButtonClick(Sender: TObject);
    procedure SelectFileClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  HttpPostForm: THttpPostForm;
  StartTime: Longword;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyFirstName       = 'FirstName';
    KeyLastName        = 'LastName';
    KeyActionURL       = 'ActionURL';
    KeyUploadURL       = 'UploadURL';
    KeyFilePath        = 'UploadFilePath';
    KeyBandwidthLimit  = 'BandwidthLimit';
    KeyUploadMethod    = 'UploadMethod';
    KeyFileDescr       = 'FileDescr';

    MethodPostBinPage  = 0;
    MethodPostBinArgs  = 1;
    MethodPutBinArgs   = 2;
    MethodPostMIME     = 3;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpPostForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpPostForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        FirstNameEdit.Text := IniFile.ReadString(SectionData, KeyFirstName,
                                                 'John');
        LastNameEdit.Text  := IniFile.ReadString(SectionData, KeyLastName,
                                                 'Doe');
        ActionURLEdit.Text := IniFile.ReadString(SectionData, KeyActionURL,
                                  'http://localhost/cgi-bin/FormHandler');
        UploadURLEdit.Text := IniFile.ReadString(SectionData, KeyUploadURL,
                                  'http://localhost/cgi-bin/FileUpload\unit1.pas');
        FileNameEdit.Text := IniFile.ReadString(SectionData, KeyFilePath,
                                  'c:\temp\unit1.pas');
        BandwidthLimitEdit.Text := IniFile.ReadString(SectionData, KeyBandwidthLimit, '1000000');
        UploadMethod.ItemIndex := IniFile.ReadInteger(SectionData, KeyUploadMethod, 0);
        FileDescr.Text := IniFile.ReadString(SectionData, KeyFileDescr, 'File upload description');
        IniFile.Free;
        DisplayMemo.Clear;
    end;
   Randomize;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpPostForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData, KeyFirstName, FirstNameEdit.Text);
    IniFile.WriteString(SectionData, KeyLastName,  LastNameEdit.Text);
    IniFile.WriteString(SectionData, KeyActionURL, ActionURLEdit.Text);
    IniFile.WriteString(SectionData, KeyUploadURL, UploadURLEdit.Text);
    IniFile.WriteString(SectionData, KeyFilePath,  FileNameEdit.Text);
    IniFile.WriteString(SectionData, KeyBandwidthLimit,  BandwidthLimitEdit.Text);
    IniFile.WriteInteger(SectionData, KeyUploadMethod,  UploadMethod.ItemIndex);
    IniFile.WriteString(SectionData, KeyFileDescr,  FileDescr.Text);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpPostForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpPostForm.PostButtonClick(Sender: TObject);
var
    Data : AnsiString;
begin
    Data := 'FirstName=' + UrlEncodeToA(Trim(FirstNameEdit.Text)) + '&' +
            'LastName='  + UrlEncodeToA(Trim(LastNameEdit.Text))  + '&' +
            'Submit=Submit';
    HttpCli1.SendStream := TMemoryStream.Create;
    HttpCli1.SendStream.Write(Data[1], Length(Data));
    HttpCli1.SendStream.Seek(0, 0);
    HttpCli1.RcvdStream      := TMemoryStream.Create;
    HttpCli1.URL             := Trim(ActionURLEdit.Text);
    HttpCli1.ContentTypePost := 'application/x-www-form-urlencoded';
    DisplayMemo.Lines.Add('POST data: ' + String(Data));
    DisplayMemo.Lines.Add('To URL: ' + HttpCli1.URL);
    DisplayMemo.Lines.Add('Content Type: ' + HttpCli1.ContentTypePost);
    StartTime := 0;
    HttpCli1.PostAsync;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpPostForm.SelectFileClick(Sender: TObject);
begin
    OpenDialog.InitialDir := ExtractFilePath(FilenameEdit.Text);
    OpenDialog.FileName := FilenameEdit.Text ;
    if OpenDialog.Execute then
        FilenameEdit.Text := OpenDialog.FileName;

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpPostForm.UploadButtonClick(Sender: TObject);
var
    FileName, Data, NameOnly: string;
    Header, Footer, RandBoundary: String;
    FileSize: int64;
begin
    if UploadMethod.ItemIndex = MethodPostBinPage then begin
        if Pos ('.', UploadURLEdit.Text) > 0  then begin
            DisplayMemo.Lines.Add('URL can not have a page name');
            Exit;
        end;
    end;
    FileName := Trim(FilenameEdit.Text);
    NameOnly := ExtractFileName(FileName);
    FileSize := IcsGetFileSize(FileName);   { V8.00 sanity check }
    if FileSize <= 0 then begin
        DisplayMemo.Lines.Add('File not found');
        Exit;
    end;
    HttpCli1.RcvdStream := TMemoryStream.Create;
    HttpCli1.URL := Trim(UploadURLEdit.Text);
{$IFDEF BUILTIN_THROTTLE}
    HttpCli1.BandwidthLimit := StrToIntDef(BandwidthLimitEdit.Text, 1000000);
    if HttpCli1.BandwidthLimit > 0 then
        HttpCli1.Options := HttpCli1.Options + [httpoBandwidthControl];
{$ENDIF}
    StartTime := GetTickCount;

  // there are several ways of uploading file to web servers, all of them require
  // scripting or custom code at the web server that understands the syntax
    DisplayMemo.Lines.Add('File uploading: ' + FileName + ', Size: ' + IntToKbyte(FileSize));

    if UploadMethod.ItemIndex = MethodPostBinPage then begin
        HttpCli1.SendStream := TIcsBufferedFileStream.Create(FileName,
                                          fmOpenRead, MAX_BUFSIZE);   { V8.00 buffered file stream }
        HttpCli1.ContentTypePost := 'application/binary';
        if HttpCli1.URL [Length (HttpCli1.URL)] <> '/' then
                                           HttpCli1.URL := HttpCli1.URL + '/';
        HttpCli1.URL := HttpCli1.URL + NameOnly;
        DisplayMemo.Lines.Add('To URL: ' + HttpCli1.URL);
        DisplayMemo.Lines.Add('Content Type: ' + HttpCli1.ContentTypePost);
        HttpCli1.PostAsync;
    end
    else if UploadMethod.ItemIndex = MethodPostBinArgs then begin
        HttpCli1.SendStream := TIcsBufferedFileStream.Create(FileName,
                                          fmOpenRead, MAX_BUFSIZE);   { V8.00 buffered file stream }
        HttpCli1.ContentTypePost := 'application/binary';
        Data := String('FileName=' + UrlEncodeToA(NameOnly) + '&' +
                     'FileTitle='  + UrlEncodeToA(Trim(FileDescr.Text)));
        HttpCli1.URL := HttpCli1.URL + '?' + Data;
        DisplayMemo.Lines.Add('To URL: ' + HttpCli1.URL);
        DisplayMemo.Lines.Add('Content Type: ' + HttpCli1.ContentTypePost);
        HttpCli1.PostAsync;
    end
    else if UploadMethod.ItemIndex = MethodPutBinArgs then begin
        HttpCli1.SendStream := TIcsBufferedFileStream.Create(FileName,
                                          fmOpenRead, MAX_BUFSIZE);   { V8.00 buffered file stream }
        HttpCli1.ContentTypePost := 'application/binary';
        Data := String('FileName=' + UrlEncodeToA(NameOnly) + '&' +
                     'FileTitle='  + UrlEncodeToA(Trim(FileDescr.Text)));
        HttpCli1.URL := HttpCli1.URL + '?' + Data;
        DisplayMemo.Lines.Add('To URL: ' + HttpCli1.URL);
        DisplayMemo.Lines.Add('Content Type: ' + HttpCli1.ContentTypePost);
        HttpCli1.PutAsync;
    end
    else if UploadMethod.ItemIndex = MethodPostMIME then begin
        RandBoundary := '-----------------------------' +
               IntToHex(Random(MaxInt), 8) + IntToHex(Random(MaxInt), 8);
        HttpCli1.ContentTypePost := 'multipart/form-data' +
                '; boundary=' + RandBoundary;
        Header := RandBoundary + #13#10 +
            'Content-Disposition: form-data; name="FileName"; FileName="'
               + TextToHtmlText(ExtractFileName(NameOnly)) + '"' + #13#10 +
                 'Content-Type: ' + MimeTypesList1.TypeFromFile(NameOnly)
                    + #13#10 + #13#10;
        Footer := RandBoundary + #13#10 +
            'Content-Disposition: form-data; name="FileTitle"' + #13#10 + #13#10
              + TextToHtmlText(FileDescr.Text) + #13#10 + RandBoundary + #13#10 +
                'Content-Disposition: form-data; name="Submit"' + #13#10 + #13#10
              + 'SubmitFile' + #13#10 + RandBoundary + '--' + #13#10;
        HttpCli1.SendStream := TMultiPartFileReader.Create (Filename, Header, Footer);
        HttpCli1.SendStream.Position := 0;
        DisplayMemo.Lines.Add('POST data: ' + Header + #13#10 + '(file)' + #13#10 + Footer);
        DisplayMemo.Lines.Add('To URL: ' + HttpCli1.URL);
        HttpCli1.PostAsync;
    end ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BuildLongString(Len : Integer) : String;
var
    I : Integer;
begin
    SetLength(Result, Len);
    for I := 1 to Len do
        Result[I] := Char(48 + (I mod 10));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpPostForm.HttpCli1RequestDone(
    Sender  : TObject;
    RqType  : THttpRequest;
    ErrCode : Word);
var
    Data : AnsiString;  // WebServ demo send AnsiString replies
    Duration, BytesSec, ByteCount: integer;
    Temp: string;
begin
    ByteCount := HttpCli1.SendStream.Size;
    HttpCli1.SendStream.Free;
    HttpCli1.SendStream := nil;

    if ErrCode <> 0 then begin
        Display('Post failed with error #' + IntToStr(ErrCode));
        HttpCli1.RcvdStream.Free;
        HttpCli1.RcvdStream := nil;
        Exit;
    end;
    if HttpCli1.StatusCode <> 200 then begin
        Display('Post failed with error: ' + IntToStr(HttpCli1.StatusCode) +
                ' ' + HttpCli1.ReasonPhrase);
        HttpCli1.RcvdStream.Free;
        HttpCli1.RcvdStream := nil;
        Exit;
    end;
    Display('Post was OK. Response was:');
    HttpCli1.RcvdStream.Seek(0, 0);
    SetLength(Data, HttpCli1.RcvdStream.Size);
    HttpCli1.RcvdStream.Read(Data[1], Length(Data));
    Display(String(Data));
    if StartTime <> 0 then begin
        Duration := GetTickCount - StartTime;
        Temp := 'Received ' + IntToStr(ByteCount) + ' bytes, ';
        if Duration < 5000 then
            Temp := Temp + IntToStr(Duration) + ' milliseconds'
        else
            Temp := Temp + IntToStr(Duration div 1000) + ' seconds';
        if ByteCount > 32767 then
            BytesSec := 1000 * (ByteCount div Duration)
        else
            BytesSec := (1000 * ByteCount) div Duration;
        Temp := Temp + ' (' + IntToKByte(BytesSec) + 'bytes/sec)';
        Display(temp);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
