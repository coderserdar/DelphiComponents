{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     November 23, 1997
Version:      1.04
Description:  Sample program to demonstrate some of the THttpCli features.
EMail:        francois.piette@pophost.eunet.be    
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

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
Jan 16, 1998  V1.00 Adapted for reviced HTTP component.
Apr 13, 1998  V1.01 Call HttpCli1DocEnd when to request failed, to close the
              document file in the case it is already opened.
Jul 09, 1998  V1.02 Adapted for Delphi 4
Sep 15, 1998  V1.03 Added some code to check for file creation errors (those
              errors mostly comes from document names not suitable as file
              names).
              Added code to save form size and position and to resize it
              correctly.
Sep 25, 1999  V1.04 Added display proxy setting


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit HttpTst1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, HttpProt, WSocket, StdCtrls, ExtCtrls, IniFiles;

const
  HttpTstVersion = 103;

type
  THttpTestForm = class(TForm)
    Panel1: TPanel;
    GetButton: TButton;
    HttpCli1: THttpCli;
    URLEdit: TEdit;
    DisplayMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    ProxyHostEdit: TEdit;
    ProxyPortEdit: TEdit;
    PostButton: TButton;
    Check64Button: TButton;
    DataEdit: TEdit;
    Label3: TLabel;
    DateTimeEdit: TEdit;
    DocumentMemo: TMemo;
    Label4: TLabel;
    HeadButton: TButton;
    AbortButton: TButton;
    Label5: TLabel;
    Label6: TLabel;
    ParseButton: TButton;
    procedure GetButtonClick(Sender: TObject);
    procedure HttpCli1Command(Sender: TObject; var s: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure HttpCli1DocBegin(Sender: TObject);
    procedure HttpCli1DocEnd(Sender: TObject);
    procedure PostButtonClick(Sender: TObject);
    procedure Check64ButtonClick(Sender: TObject);
    procedure HeadButtonClick(Sender: TObject);
    procedure HttpCli1RequestDone(Sender: TObject; RqType: THttpRequest;
      Error: Word);
    procedure AbortButtonClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure ParseButtonClick(Sender: TObject);
  private
    { Déclarations privées }
    Initialized : Boolean;
    DocFileName : String;
    procedure SetButtonState(State : Boolean);
  public
    { Déclarations publiques }
  end;

var
  HttpTestForm: THttpTestForm;

implementation

{$R *.DFM}

const
    IniFileName    = 'httptest';
    SectionWindow  = 'WindowMain';
    KeyTop         = 'Top';
    KeyLeft        = 'Left';
    KeyWidth       = 'Width';
    KeyHeight      = 'Height';
    SectionData    = 'Data';
    KeyUrl         = 'URL';
    KeyProxyHost   = 'ProxyHost';
    KeyProxyPort   = 'ProxyPort';
    KeyData        = 'Data';
    KeyDateTime    = 'DateTime';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not Initialized then begin
        Initialized  := TRUE;
        IniFile      := TIniFile.Create(IniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        URLEdit.Text       := IniFile.ReadString(SectionData, KeyURL,       '');
        ProxyHostEdit.Text := IniFile.ReadString(SectionData, KeyProxyHost, '');
        ProxyPortEdit.Text := IniFile.ReadString(SectionData, KeyProxyPort, '80');
        DataEdit.Text      := IniFile.ReadString(SectionData, KeyData,      '');
        DateTimeEdit.Text  := IniFile.ReadString(SectionData, KeyDateTime,  '');
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(IniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,       Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,      Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,     Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,    Height);
    IniFile.WriteString(SectionData,    KeyURL,       URLEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyHost, ProxyHostEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyPort, ProxyPortEdit.Text);
    IniFile.WriteString(SectionData,    KeyData,      DataEdit.Text);
    IniFile.WriteString(SectionData,    KeyDateTime,  DateTimeEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HeadButtonClick(Sender: TObject);
var
    I       : Integer;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        httpcli1.URL        := URLEdit.Text;
        httpcli1.Proxy      := ProxyHostEdit.Text;
        httpcli1.ProxyPort  := ProxyPortEdit.Text;
        httpcli1.RcvdStream := nil;
        if DateTimeEdit.Text <> '' then
            httpcli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
        else
            httpcli1.ModifiedSince := 0;

        if httpcli1.Proxy <> '' then
            DisplayMemo.Lines.Add('Using proxy ''' + httpcli1.Proxy + ':' +
                                  httpcli1.ProxyPort + '''')
        else
            DisplayMemo.Lines.Add('Not using proxy');

        try
            httpcli1.Head;
        except
            DisplayMemo.Lines.Add('HEAD Failed !');
            DisplayMemo.Lines.Add('StatusCode   = ' + IntToStr(httpcli1.StatusCode));
            DisplayMemo.Lines.Add('ReasonPhrase = ' + httpcli1.ReasonPhrase);
            Exit;
        end;

        DisplayMemo.Lines.Add('StatusCode = ' + IntToStr(httpcli1.StatusCode));

        for I := 0 to httpcli1.RcvdHeader.Count - 1 do
            DisplayMemo.Lines.Add('hdr>' + httpcli1.RcvdHeader.Strings[I]);
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.GetButtonClick(Sender: TObject);
var
    I       : Integer;
    DataIn  : TStream;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        httpcli1.URL        := URLEdit.Text;
        httpcli1.Proxy      := ProxyHostEdit.Text;
        httpcli1.ProxyPort  := ProxyPortEdit.Text;
        httpcli1.RcvdStream := nil;
        if DateTimeEdit.Text <> '' then
            httpcli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
        else
            httpcli1.ModifiedSince := 0;

        if httpcli1.Proxy <> '' then
            DisplayMemo.Lines.Add('Using proxy ''' + httpcli1.Proxy + ':' +
                                  httpcli1.ProxyPort + '''')
        else
            DisplayMemo.Lines.Add('Not using proxy');

        try
            httpcli1.Get;
        except
            DisplayMemo.Lines.Add('GET Failed !');
            DisplayMemo.Lines.Add('StatusCode   = ' + IntToStr(httpcli1.StatusCode));
            DisplayMemo.Lines.Add('ReasonPhrase = ' + httpcli1.ReasonPhrase);
            HttpCli1DocEnd(nil);  { This will close the file }
            Exit;
        end;

        DisplayMemo.Lines.Add('StatusCode = ' + IntToStr(httpcli1.StatusCode));

        for I := 0 to httpcli1.RcvdHeader.Count - 1 do
            DisplayMemo.Lines.Add('hdr>' + httpcli1.RcvdHeader.Strings[I]);

        DataIn := TFileStream.Create(DocFileName, fmOpenRead);
        if Copy(httpcli1.ContentType, 1, 5) = 'text/' then
            DocumentMemo.Lines.LoadFromStream(DataIn)
        else begin
            DocumentMemo.Lines.Add('Content type is ' + httpcli1.ContentType);
            DocumentMemo.Lines.Add('Document stored in ''' + DocFileName +
                                   ''' Size=' + IntToStr(DataIn.Size));
        end;
        DataIn.Free;
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.PostButtonClick(Sender: TObject);
var
    DataOut : TMemoryStream;
    DataIn  : TFileStream;
    Buf     : String;
    I       : Integer;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        DataOut := TMemoryStream.Create;
        Buf     := DataEdit.Text;
        if Length(Buf) > 0 then      { Check if some data to post }
            DataOut.Write(Buf[1], Length(Buf));
        DataOut.Seek(0, soFromBeginning);

        httpcli1.SendStream := DataOut;
        httpcli1.Proxy      := ProxyHostEdit.Text;
        httpcli1.ProxyPort  := ProxyPortEdit.Text;
        httpcli1.RcvdStream := nil;
        httpcli1.URL        := URLEdit.Text;

        if httpcli1.Proxy <> '' then
            DisplayMemo.Lines.Add('Using proxy ''' + httpcli1.Proxy + ':' +
                                  httpcli1.ProxyPort + '''')
        else
            DisplayMemo.Lines.Add('Not using proxy');

        try
            httpcli1.Post;
        except
            DataOut.Free;
            DisplayMemo.Lines.Add('POST Failed !');
            DisplayMemo.Lines.Add('StatusCode   = ' + IntToStr(httpcli1.StatusCode));
            DisplayMemo.Lines.Add('ReasonPhrase = ' + httpcli1.ReasonPhrase);
            Exit;
        end;
        DataOut.Free;

        DisplayMemo.Lines.Add('StatusCode = ' + IntToStr(httpcli1.StatusCode));

        for I := 0 to httpcli1.RcvdHeader.Count - 1 do
            DisplayMemo.Lines.Add('hdr>' + httpcli1.RcvdHeader.Strings[I]);

        DataIn := TFileStream.Create(httpcli1.DocName, fmOpenRead);
        DocumentMemo.Lines.LoadFromStream(DataIn);
        DataIn.Free;
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ WARNING: With DELPHI1, change "s: String" to "s: OpenString"              }
procedure THttpTestForm.HttpCli1Command(Sender: TObject; var s: String);
begin
    DisplayMemo.Lines.Add('cmd> ' + s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1DocBegin(Sender: TObject);
begin
    DisplayMemo.Lines.Add(HttpCli1.ContentType + ' => ' + httpcli1.DocName);
    DisplayMemo.Lines.Add('Document = ' + httpcli1.DocName);
    DocFileName := httpcli1.DocName;
    if DocFileName = '' then
        DocFileName := 'HttpTst.htm';
    try
        httpcli1.RcvdStream := TFileStream.Create(DocFileName, fmCreate);
    except
        on E:Exception do begin
            DisplayMemo.Lines.Add('Error opening file: ' + E.Message);
            DocFileName := 'HttpTst.htm';
            DisplayMemo.Lines.Add('Using default file name: ' + DocFileName);
            httpcli1.RcvdStream := TFileStream.Create(DocFileName, fmCreate);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1DocEnd(Sender: TObject);
begin
    if httpcli1.RcvdStream <> nil then begin
        httpcli1.RcvdStream.Free;
        httpcli1.RcvdStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.Check64ButtonClick(Sender: TObject);
const
    Inp : String = 'Aladdin:open sesame';
    Res : String = 'QWxhZGRpbjpvcGVuIHNlc2FtZQ==';
begin
    if EncodeLine(encBase64, @Inp[1], Length(Inp)) <> Res then
        DisplayMemo.Lines.Add('Base64 encoding do not work !')
    else
        DisplayMemo.Lines.Add('Base64 encoding works OK !');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.SetButtonState(State : Boolean);
begin
    GetButton.Enabled   := State;
    PostButton.Enabled  := State;
    HeadButton.Enabled  := State;
    AbortButton.Enabled := not State;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1RequestDone(Sender: TObject;
  RqType: THttpRequest; Error: Word);
begin
    SetButtonState(TRUE);
    DisplayMemo.Lines.Add('RequestDone Error = ' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.AbortButtonClick(Sender: TObject);
begin
    HttpCli1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.Panel1Resize(Sender: TObject);
begin
    GetButton.Left   := Panel1.Width - GetButton.Width - 8;
    PostButton.Left  := GetButton.Left;
    HeadButton.Left  := GetButton.Left;
    AbortButton.Left := GetButton.Left;
    URLEdit.Width    := GetButton.Left - URLEdit.Left - 8;
    DataEdit.Width   := URLEdit.Width;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure THttpTestForm.ParseButtonClick(Sender: TObject);
var
    Proto, User, Pass, Host, Port, Path : String;
begin
    ParseURL(URLEdit.Text, Proto, User, Pass, Host, Port, Path);
    DisplayMemo.Lines.Add('URL   = ''' + URLEdit.Text + '''');
    DisplayMemo.Lines.Add('Proto = ''' + Proto + '''');
    DisplayMemo.Lines.Add('Host  = ''' + Host  + '''');
    DisplayMemo.Lines.Add('Path  = ''' + Path  + '''');
    DisplayMemo.Lines.Add('Port  = ''' + Port  + '''');
    DisplayMemo.Lines.Add('User  = ''' + User  + '''');
    DisplayMemo.Lines.Add('Pass  = ''' + Pass  + '''');
end;

end.

