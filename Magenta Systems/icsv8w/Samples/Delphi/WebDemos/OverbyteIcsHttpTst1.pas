{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     November 23, 1997
Version:      8.57
Description:  Sample program to demonstrate some of the THttpCli features.
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2018 by François PIETTE
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
Aug 18, 2001  V1.05 Checked for document name before trying to display.
              Wrapped document file access to a try/finally.
              Checked ContentType to change document name extension according
              to some content type.
              Added display of copyright notice and version informations.
May 01, 2003  V1.06 Display Header checkbox added
May 09, 2003  V1.07 Implemented PUT
Jan 10, 2004  V1.08 Added code for HTTP 1.1 (Started months ago but forgot
              to add it in the history).
Feb 4,  2011  V7.00 Angus added bandwidth throttling using TCustomThrottledWSocket
Oct 6,  2011  V7.01 Angus added content encoding checkbox
Mar 19, 2012  V7.02 Angus added persistent cookie support
Apr 19, 2014  V8.00 Angus added PATCH method, thanks to RTT <pdfe@sapo.pt>
Jun  4, 2014  V8.01 Angus show StatusCode during relocation
Jul 16, 2014  V8.02 Angus added DELETE, OPTIONS and TRACE
              simplified code with GetHeadDelOptTrace
Nov 04, 2016  V8.37 report more error information
Sep 05, 2018  V8.57 Using OnSelectDns to show alternate IP addresses, changed
                      SocketFamily to sfAny so it finds both IPV4 and IPV6 addresses


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpTst1;

{$I Include\OverbyteIcsDefs.inc}
{$IFNDEF DELPHI7_UP}
    Bomb('This sample requires Delphi 7 or later');
{$ENDIF}
{$B-}                 { Enable partial boolean evaluation   }
{$T-}                 { Untyped pointers                    }
{$X+}                 { Enable extended syntax              }
{$I+}                 { Turn IO exceptions to on            }
{$H+}                 { Use long strings                    }
{$J+}                 { Allow typed constant to be modified }
{$WARN SYMBOL_PLATFORM   OFF}
{$WARN SYMBOL_LIBRARY    OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, OverbyteIcsIniFiles, OverbyteIcsUrl,
 {$IFDEF CLR}
  System.ComponentModel,
 {$ENDIF}
  OverbyteIcsWinsock,  OverbyteIcsWSocket,
  OverbyteIcsHttpProt, OverbyteIcsHttpCCodZLib, OverbyteIcsCookies,
  OverbyteIcsWndControl, OverbyteIcsLogger;

const
  HttpTstVersion         = 857;
  CopyRight : String     = 'HttpTst (c) 1997-2018 Francois Piette  V8.57 ';

type
  THttpTestForm = class(TForm)
    Panel1: TPanel;
    HttpCli1: THttpCli;
    URLEdit: TEdit;
    DisplayMemo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    ProxyHostEdit: TEdit;
    ProxyPortEdit: TEdit;
    DataEdit: TEdit;
    Label3: TLabel;
    DateTimeEdit: TEdit;
    DocumentMemo: TMemo;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    HttpVersionComboBox: TComboBox;
    Label8: TLabel;
    Panel2: TPanel;
    GetButton: TButton;
    HeadButton: TButton;
    PostButton: TButton;
    AbortButton: TButton;
    ParseButton: TButton;
    PutButton: TButton;
    CloseButton: TButton;
    ClearButton: TButton;
    DisplayHeaderCheckBox: TCheckBox;
    Label9: TLabel;
    Label10: TLabel;
    PostContentTypeEdit: TEdit;
    IcsLogger1: TIcsLogger;
    BandwidthLimitEdit: TEdit;
    Label11: TLabel;
    ContentEncodingCheckBox: TCheckBox;
    IcsCookies: TIcsCookies;
    PatchButton: TButton;
    TraceButton: TButton;
    OptionsButton: TButton;
    DeleteButton: TButton;
    procedure GetButtonClick(Sender: TObject);
    procedure HttpCli1Command(Sender: TObject; var S: String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure HttpCli1DocBegin(Sender: TObject);
    procedure HttpCli1DocEnd(Sender: TObject);
    procedure PostButtonClick(Sender: TObject);
    procedure HeadButtonClick(Sender: TObject);
    procedure HttpCli1RequestDone(Sender: TObject; RqType: THttpRequest;
      ErrCode: Word);
    procedure AbortButtonClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure ParseButtonClick(Sender: TObject);
    procedure HttpCli1HeaderData(Sender: TObject);
    procedure HttpCli1Cookie(Sender: TObject; const Data: String;
      var Accept: Boolean);
    procedure HttpCli1LocationChange(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure PutButtonClick(Sender: TObject);
    procedure IcsCookiesNewCookie(Sender: TObject; ACookie: TCookie; var Save: Boolean);
    procedure PatchButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure TraceButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure HttpCli1SelectDns(Sender: TObject; DnsList: TStrings;
      var NewDns: string);
  private
    Initialized  : Boolean;
    DocFileName  : String;
    FIniFileName : String;
    procedure SetButtonState(State : Boolean);
    procedure Display(const Msg : String);
    procedure PostPutOrPatch(Request: THttpRequest);
    procedure GetHeadDelOptTrace(Request: THttpRequest);
  end;

var
  HttpTestForm: THttpTestForm;

implementation

{$R *.DFM}

const
    SectionWindow   = 'WindowMain';
    KeyTop          = 'Top';
    KeyLeft         = 'Left';
    KeyWidth        = 'Width';
    KeyHeight       = 'Height';
    SectionData     = 'Data';
    KeyUrl          = 'URL';
    KeyProxyHost    = 'ProxyHost';
    KeyProxyPort    = 'ProxyPort';
    KeyData         = 'Data';
    KeyDateTime     = 'DateTime';
    KeyHttpVer      = 'HttpVer';
    KeyPostType     = 'PostContentType';
    KeyDisplayHdr   = 'DisplayHeader';
    KeyNTLMHost     = 'NTLMHost';
    KeyNTLMDomain   = 'NTLMDomain';
    KeyNTLMUsercode = 'NTLMUsercode';
    KeyNTLMPassword = 'NTLMPassword';
    KeyBandwidthLimit = 'BandwidthLimit';
    KeyContentEncoding = 'ContentEncoding' ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    wsi     : TWSADATA;
begin
    if not Initialized then begin
        Initialized  := TRUE;
        {$IFDEF DELPHI10_UP}
        {$IFNDEF CLR}
        // BDS2006 has built-in memory leak detection and display
        ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
        {$ENDIF}
        {$ENDIF}
        FIniFileName := GetIcsIniFileName;
        IniFile      := TIcsIniFile.Create(FIniFileName);
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
        PostContentTypeEdit.Text := IniFile.ReadString(SectionData, KeyPostType, 'text/plain');
        HttpVersionComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyHttpVer, 0);
        DisplayHeaderCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyDisplayHdr, 0));
        BandwidthLimitEdit.Text := IniFile.ReadString(SectionData, KeyBandwidthLimit, '1000000');
        ContentEncodingCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyContentEncoding, 0));
{$IFDEF UseNTLMAuthentication}
        HttpCli1.NTLMHost       := IniFile.ReadString(SectionData, KeyNTLMHost,     'PC');
        HttpCli1.NTLMDomain     := IniFile.ReadString(SectionData, KeyNTLMDomain,   'WORKGROUP');
        HttpCli1.NTLMUsercode   := IniFile.ReadString(SectionData, KeyNTLMUsercode, 'TESTUSER');
        HttpCli1.NTLMPassword   := IniFile.ReadString(SectionData, KeyNTLMPassword, 'TESTPASS');
{$ENDIF}
        IniFile.Free;
        Panel2.BevelOuter := bvNone;

        { 7.02 load persisent cookies, they will be saved automatically during close down to save file name }
        IcsCookies.LoadFromFile(ChangeFileExt(FIniFileName, '.cookies'));
        IcsCookies.AutoSave := true;

        { Display version info for program and used components }
        wsi := WinsockInfo;
        DisplayMemo.Clear;
        Display(CopyRight);
        Display('Using:');
        Display('   ' + OverbyteIcsWSocket.CopyRight);
        Display('   ' + OverbyteIcsHttpProt.CopyRight);
{$IFNDEF CLR}
        Display('    Winsock:');
        Display('        Version ' +
                Format('%d.%d', [WinsockInfo.wHighVersion shr 8,
                                 WinsockInfo.wHighVersion and 15]));
        Display('        ' + wsi.szDescription);
        Display('        ' + wsi.szSystemStatus);
        Display('        ' + wsi.lpVendorInfo);
{$ENDIF}
{$IFDEF UseNTLMAuthentication}
        Display('NTLMHost     = "' + HttpCli1.NTLMHost     + '"');
        Display('NTLMDomain   = "' + HttpCli1.NTLMDomain   + '"');
        Display('NTLMUsercode = "' + HttpCli1.NTLMUsercode + '"');
        Display('NTLMPassword = "' + HttpCli1.NTLMPassword + '"');
{$ENDIF}
{$IFDEF NO_ADVANCED_HTTP_CLIENT_FEATURES}
        Display('NO_ADVANCED_HTTP_CLIENT_FEATURES is defined');
{$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,       Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,      Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,     Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,    Height);
    IniFile.WriteString(SectionData,    KeyURL,       URLEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyHost, ProxyHostEdit.Text);
    IniFile.WriteString(SectionData,    KeyProxyPort, ProxyPortEdit.Text);
    IniFile.WriteString(SectionData,    KeyData,      DataEdit.Text);
    IniFile.WriteString(SectionData,    KeyDateTime,  DateTimeEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyHttpVer,   HttpVersionComboBox.ItemIndex);
    IniFile.WriteString(SectionData,    KeyPostType,  PostContentTypeEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyDisplayHdr, Ord(DisplayHeaderCheckBox.Checked));
    IniFile.WriteString(SectionData,    KeyBandwidthLimit,  BandwidthLimitEdit.Text);
    IniFile.WriteInteger(SectionData,   KeyContentEncoding, Ord(ContentEncodingCheckBox.Checked));
{$IFDEF UseNTLMAuthentication}
    IniFile.WriteString(SectionData, KeyNTLMHost,     HttpCli1.NTLMHost);
    IniFile.WriteString(SectionData, KeyNTLMDomain,   HttpCli1.NTLMDomain);
    IniFile.WriteString(SectionData, KeyNTLMUsercode, HttpCli1.NTLMUsercode);
    IniFile.WriteString(SectionData, KeyNTLMPassword, HttpCli1.NTLMPassword);
{$ENDIF}
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in display memo box, making sure we don't overflow it.  }

procedure THttpTestForm.Display(const Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            { We preserve only 200 lines }
            while DisplayMemo.Lines.Count > 200 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        { Makes last line visible }
        {$IFNDEF VER80}
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
procedure THttpTestForm.HeadButtonClick(Sender: TObject);
var
    I       : Integer;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        HttpCli1.URL        := URLEdit.Text;
        HttpCli1.Proxy      := ProxyHostEdit.Text;
        HttpCli1.ProxyPort  := ProxyPortEdit.Text;
        HttpCli1.Connection := 'Keep-Alive';
        HttpCli1.RequestVer := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);
        HttpCli1.RcvdStream := nil;
{$IFDEF BUILTIN_THROTTLE}
        HttpCli1.BandwidthLimit := StrToIntDef(BandwidthLimitEdit.Text, 1000000);
        if HttpCli1.BandwidthLimit > 0 then
             HttpCli1.Options := HttpCli1.Options + [httpoBandwidthControl];
{$ENDIF}
        if ContentEncodingCheckBox.Checked then
             HttpCli1.Options := HttpCli1.Options + [httpoEnableContentCoding]
        else
             HttpCli1.Options := HttpCli1.Options - [httpoEnableContentCoding];
        if DateTimeEdit.Text <> '' then
            HttpCli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
        else
            HttpCli1.ModifiedSince := 0;

        if HttpCli1.Proxy <> '' then
            Display('Using proxy ''' + HttpCli1.Proxy + ':' +
                    HttpCli1.ProxyPort + '''')
        else
            Display('Not using proxy');

        HttpCli1.Cookie := IcsCookies.GetCookies (HttpCli1.URL);  // 7.02 send cookies

        try
            HttpCli1.Head;
        except
            Display('HEAD Failed !');
            Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
            Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
            Exit;
        end;

        Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));

        if DisplayHeaderCheckBox.Checked then
            for I := 0 to HttpCli1.RcvdHeader.Count - 1 do
                Display('hdr>' + HttpCli1.RcvdHeader.Strings[I]);
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
        HttpCli1.URL            := URLEdit.Text;
        HttpCli1.Proxy          := ProxyHostEdit.Text;
        HttpCli1.ProxyPort      := ProxyPortEdit.Text;
        HttpCli1.AcceptLanguage := 'en, fr';
        HttpCli1.Connection     := 'Keep-Alive';
        HttpCli1.RequestVer     := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);
        HttpCli1.RcvdStream := nil;
{$IFDEF BUILTIN_THROTTLE}
        HttpCli1.BandwidthLimit := StrToIntDef(BandwidthLimitEdit.Text, 1000000);
        if HttpCli1.BandwidthLimit > 0 then
             HttpCli1.Options := HttpCli1.Options + [httpoBandwidthControl];
{$ENDIF}
        if ContentEncodingCheckBox.Checked then
             HttpCli1.Options := HttpCli1.Options + [httpoEnableContentCoding]
        else
             HttpCli1.Options := HttpCli1.Options - [httpoEnableContentCoding];
        if DateTimeEdit.Text <> '' then
            HttpCli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
        else
            HttpCli1.ModifiedSince := 0;

        if HttpCli1.Proxy <> '' then
            Display('Using proxy ''' + HttpCli1.Proxy + ':' +
                    HttpCli1.ProxyPort + '''')
        else
            Display('Not using proxy');

        HttpCli1.Cookie := IcsCookies.GetCookies (HttpCli1.URL);  // 7.02 send cookies

        try
            HttpCli1.Get;
        except
            Display('GET Failed !');
            Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
            Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
            HttpCli1DocEnd(nil);  { This will close the file }
            Exit;
        end;

        Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));

        if DisplayHeaderCheckBox.Checked then
            for I := 0 to HttpCli1.RcvdHeader.Count - 1 do
                Display('hdr>' + HttpCli1.RcvdHeader.Strings[I]);

        if Length(DocFileName) = 0 then begin
            DocumentMemo.Lines.Add('*** NO DOCUMENT FILE NAME ***');
        end
        else begin
            DataIn := TFileStream.Create(DocFileName, fmOpenRead);
            try
                if Copy(HttpCli1.ContentType, 1, 5) = 'text/' then
                    DocumentMemo.Lines.LoadFromStream(DataIn)
                else begin
                    DocumentMemo.Lines.Add('Content type is ' +
                                           HttpCli1.ContentType);
                    DocumentMemo.Lines.Add('Document stored in ''' +
                                           DocFileName +
                                           ''' Size=' + IntToStr(DataIn.Size));
                end;
            finally
                DataIn.Free;
            end;
        end;
    finally
        SetButtonState(TRUE);
    end;
end;
*)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure THttpTestForm.HeadButtonClick(Sender: TObject);
begin
    GetHeadDelOptTrace(httpHEAD);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.GetButtonClick(Sender: TObject);
begin
    GetHeadDelOptTrace(httpGET);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.GetHeadDelOptTrace(Request: THttpRequest);
var
    I       : Integer;
    DataIn  : TStream;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        HttpCli1.URL            := URLEdit.Text;
        HttpCli1.SocketFamily   := sfAny;  { V8.57 IP4 and IPV6 }
        HttpCli1.Proxy          := ProxyHostEdit.Text;
        HttpCli1.ProxyPort      := ProxyPortEdit.Text;
        HttpCli1.AcceptLanguage := 'en, fr';
        HttpCli1.Connection     := 'Keep-Alive';
        HttpCli1.RequestVer     := '1.' + IntToStr(HttpVersionComboBox.ItemIndex);
        HttpCli1.RcvdStream := nil;
{$IFDEF BUILTIN_THROTTLE}
        HttpCli1.BandwidthLimit := StrToIntDef(BandwidthLimitEdit.Text, 1000000);
        if HttpCli1.BandwidthLimit > 0 then
             HttpCli1.Options := HttpCli1.Options + [httpoBandwidthControl];
{$ENDIF}
        if ContentEncodingCheckBox.Checked then
             HttpCli1.Options := HttpCli1.Options + [httpoEnableContentCoding]
        else
             HttpCli1.Options := HttpCli1.Options - [httpoEnableContentCoding];
        if DateTimeEdit.Text <> '' then
            HttpCli1.ModifiedSince := StrToDateTime(DateTimeEdit.Text)
        else
            HttpCli1.ModifiedSince := 0;

        if HttpCli1.Proxy <> '' then
            Display('Using proxy ''' + HttpCli1.Proxy + ':' +
                    HttpCli1.ProxyPort + '''')
        else
            Display('Not using proxy');

        HttpCli1.Cookie := IcsCookies.GetCookies (HttpCli1.URL);  // 7.02 send cookies

        try
            if Request = httpGET then
                HttpCli1.Get
            else if Request = httpHEAD then
                HttpCli1.Head
            else if Request = httpDELETE then
                HttpCli1.Del
            else if Request = httpOPTIONS then
                HttpCli1.OptionsSync
            else if Request = httpTRACE then
                HttpCli1.Trace ;
        except
            Display('Request Failed !');
            Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
            Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
            HttpCli1DocEnd(nil);  { This will close the file }
            Exit;
        end;

        Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));

        if DisplayHeaderCheckBox.Checked then
            for I := 0 to HttpCli1.RcvdHeader.Count - 1 do
                Display('hdr>' + HttpCli1.RcvdHeader.Strings[I]);

        if (Request = httpHEAD) then Exit;

        if Length(DocFileName) = 0 then begin
            DocumentMemo.Lines.Add('*** NO DOCUMENT FILE NAME ***');
        end
        else begin
            DataIn := TFileStream.Create(DocFileName, fmOpenRead);
            try
                if Copy(HttpCli1.ContentType, 1, 5) = 'text/' then
                    DocumentMemo.Lines.LoadFromStream(DataIn)
                else begin
                    DocumentMemo.Lines.Add('Content type is ' +
                                           HttpCli1.ContentType);
                    DocumentMemo.Lines.Add('Document stored in ''' +
                                           DocFileName +
                                           ''' Size=' + IntToStr(DataIn.Size));
                end;
            finally
                DataIn.Free;
            end;
        end;
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.CloseButtonClick(Sender: TObject);
begin
    try
        HttpCli1.Close;
    except
        Display('CLOSE Failed !');
        Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
        Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
        Exit;
    end;
    Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.PutButtonClick(Sender: TObject);
begin
    PostPutOrPatch(httpPUT);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.PostButtonClick(Sender: TObject);
begin
    PostPutOrPatch(httpPOST);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.PostPutOrPatch(Request: THttpRequest);
var
    DataOut : TMemoryStream;
    DataIn  : TFileStream;
    Buf     : AnsiString;
    I       : Integer;
begin
    DisplayMemo.Clear;
    DocumentMemo.Clear;
    SetButtonState(FALSE);

    try
        DataOut := TMemoryStream.Create;
        Buf     := AnsiString(DataEdit.Text);
        if Length(Buf) > 0 then      { Check if some data to post }
            DataOut.Write(Buf[1], Length(Buf));
        DataOut.Seek(0, soBeginning);

        HttpCli1.SendStream      := DataOut;
        HttpCli1.Proxy           := ProxyHostEdit.Text;
        HttpCli1.ProxyPort       := ProxyPortEdit.Text;
        HttpCli1.Connection      := 'Keep-Alive';
        HttpCli1.RcvdStream      := nil;
        HttpCli1.ContentTypePost := PostContentTypeEdit.Text;
        HttpCli1.URL             := URLEdit.Text;
        HttpCli1.RequestVer      := '1.' +
                                    IntToStr(HttpVersionComboBox.ItemIndex);

{$IFDEF BUILTIN_THROTTLE}
        HttpCli1.BandwidthLimit := StrToIntDef(BandwidthLimitEdit.Text, 1000000);
        if HttpCli1.BandwidthLimit > 0 then
             HttpCli1.Options := HttpCli1.Options + [httpoBandwidthControl];
{$ENDIF}
        if ContentEncodingCheckBox.Checked then
             HttpCli1.Options := HttpCli1.Options + [httpoEnableContentCoding]
        else
             HttpCli1.Options := HttpCli1.Options - [httpoEnableContentCoding];
        if HttpCli1.Proxy <> '' then
            Display('Using proxy ''' + HttpCli1.Proxy + ':' +
                                  HttpCli1.ProxyPort + '''')
        else
            Display('Not using proxy');

        HttpCli1.Cookie := IcsCookies.GetCookies (HttpCli1.URL);  // 7.02 send cookies

        try
            if Request = httpPOST then
                HttpCli1.Post
            else if Request = httpPUT then
                HttpCli1.Put
            else if Request = httpPATCH then { V8.00 }
                HttpCli1.Patch;
        except
            DataOut.Free;
            Display('Request Failed !');
            Display('StatusCode   = ' + IntToStr(HttpCli1.StatusCode));
            Display('ReasonPhrase = ' + HttpCli1.ReasonPhrase);
            Exit;
        end;
        DataOut.Free;

        Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode) +
                ' (' + HttpCli1.ReasonPhrase + ')');

        if DisplayHeaderCheckBox.Checked then
            for I := 0 to HttpCli1.RcvdHeader.Count - 1 do
                Display('hdr>' + HttpCli1.RcvdHeader.Strings[I]);

        if HttpCli1.ContentLength = 0 then
            DocumentMemo.Lines.Add('No document received.')
        else begin
            DataIn := TFileStream.Create(HttpCli1.DocName, fmOpenRead);
            try
                if Copy(HttpCli1.ContentType, 1, 5) = 'text/' then
                    DocumentMemo.Lines.LoadFromStream(DataIn)
                else begin
                    DocumentMemo.Lines.Add('Content type is ' +
                                           HttpCli1.ContentType);
                    DocumentMemo.Lines.Add('Document stored in ''' +
                                           DocFileName +
                                           ''' Size=' + IntToStr(DataIn.Size));
                end;
            finally
                DataIn.Free;
            end;
        end;
    finally
        SetButtonState(TRUE);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1SelectDns(Sender: TObject; DnsList: TStrings;
  var NewDns: string);
begin
    Display('Looked-up DNS: ' + NewDns);
    if DnsList.Count > 1 then begin
        Display(IntToStr(DnsList.Count) + ' alternate addresses: ' + DnsList.CommaText);
      { we could select an alternate now, round robin or IPV4/IPV6 }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ WARNING: With DELPHI1, change "s: String" to "s: OpenString"              }
procedure THttpTestForm.HttpCli1Command(Sender: TObject; var S: String);
begin
    Display('cmd> ' + s);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1DocBegin(Sender: TObject);
begin
    Display(HttpCli1.ContentType + ' => ' + HttpCli1.DocName);
    Display('Location = ' + HttpCli1.Location);
    Display('URL = ' + HttpCli1.URL);
    Display('Document = ' + HttpCli1.DocName);

    DocFileName := HttpCli1.DocName;

    if HttpCli1.ContentType = 'image/gif' then
        ReplaceExt(DocFileName, 'gif')
    else if HttpCli1.ContentType = 'image/jpeg' then
        ReplaceExt(DocFileName, 'jpg')
    else if HttpCli1.ContentType = 'image/bmp' then
        ReplaceExt(DocFileName, 'bmp');

    if DocFileName = '' then
        DocFileName := 'HttpTst.htm';
    try
        HttpCli1.RcvdStream := TFileStream.Create(DocFileName, fmCreate);
    except
        on E:Exception do begin
            Display('Error opening file: ' + E.Message);
            DocFileName := 'HttpTst.htm';
            Display('Using default file name: ' + DocFileName);
            HttpCli1.RcvdStream := TFileStream.Create(DocFileName, fmCreate);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1DocEnd(Sender: TObject);
begin
    if HttpCli1.RcvdStream <> nil then begin
        HttpCli1.RcvdStream.Free;
        HttpCli1.RcvdStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.SetButtonState(State : Boolean);
begin
    GetButton.Enabled   := State;
    PostButton.Enabled  := State;
    HeadButton.Enabled  := State;
    PutButton.Enabled  := State;
    HeadButton.Enabled  := State;
    DeleteButton.Enabled  := State;
    OptionsButton.Enabled  := State;
    TraceButton.Enabled  := State;
    PatchButton.Enabled  := State;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1RequestDone(
    Sender  : TObject;
    RqType  : THttpRequest;
    ErrCode : Word);
begin
    SetButtonState(TRUE);
    if ErrCode <> 0 then
        Display('RequestDone Error = ' + IntToStr(ErrCode) + '. Status = ' +
                IntToStr(HttpCli1.StatusCode) +
                          ' - ' + HttpCli1.ReasonPhrase)  { V8.37 report more }
    else
        Display('RequestDone, no error. Status = ' +
                IntToStr(HttpCli1.StatusCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.AbortButtonClick(Sender: TObject);
begin
    HttpCli1.Abort;
    Display('StatusCode = ' + IntToStr(HttpCli1.StatusCode));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.Panel1Resize(Sender: TObject);
begin
    URLEdit.Width             := Panel1.Width - Panel2.Width - URLEdit.Left - 8;
    DataEdit.Width            := URLEdit.Width;
    PostContentTypeEdit.Width := Panel1.Width - Panel2.Width -
                                 PostContentTypeEdit.Left - 8
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.ParseButtonClick(Sender: TObject);
var
    Proto, User, Pass, Host, Port, Path : String;
begin
    ParseURL(URLEdit.Text, Proto, User, Pass, Host, Port, Path);
    Display('URL   = ''' + URLEdit.Text + '''');
    Display('Proto = ''' + Proto + '''');
    Display('Host  = ''' + Host  + '''');
    Display('Path  = ''' + Path  + '''');
    Display('Port  = ''' + Port  + '''');
    Display('User  = ''' + User  + '''');
    Display('Pass  = ''' + Pass  + '''');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1HeaderData(Sender: TObject);
begin
{    Display('Header: "' + HttpCli1.LastResponse + '"'); }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1Cookie(
    Sender     : TObject;
    const Data : String;
    var Accept : Boolean);
begin
    IcsCookies.SetCookie (Data, HttpCli1.Url);   // 7.02 save cookie
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.IcsCookiesNewCookie(Sender: TObject; ACookie: TCookie; var Save: Boolean);
var
    S: string;
begin
    with ACookie do begin
        S := 'NewCookie: ' + CName + '=' + CValue + ', Domain=' + CDomain + ', Path=' + CPath ;
        if CPersist then
            S := S + ', Expires=' + DateTimeToStr (CExpireDT)
        else
            S := S + ', Not Persisent';
        if CSecureOnly then S := S + ', SecureOnly';
        if CHttpOnly then S := S + ', HttpOnly';
        Display (S);               // 7.02 tell user what cookie we found, could also reject it
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.HttpCli1LocationChange(Sender: TObject);
begin
    Display('Status = ' + IntToStr(HttpCli1.StatusCode) +
                        ', Location changed to "' + HttpCli1.Location + '"');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.ClearButtonClick(Sender: TObject);
begin
    DocumentMemo.Clear;
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.PatchButtonClick(Sender: TObject);
begin
    PostPutOrPatch(httpPATCH);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.OptionsButtonClick(Sender: TObject);
begin
    GetHeadDelOptTrace(httpOPTIONS);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.TraceButtonClick(Sender: TObject);
begin
    GetHeadDelOptTrace(httpTRACE);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.DeleteButtonClick(Sender: TObject);
begin
    GetHeadDelOptTrace(httpDELETE);
end;

end.

