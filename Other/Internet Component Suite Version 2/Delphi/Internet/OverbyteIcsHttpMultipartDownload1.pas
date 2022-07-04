{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     March 2007
Version:      0.99 ALPHA CODE
Description:  Demo application for TMultipartHttpDownloader which is a
              component to download files using simultaneous connections to
              speedup download.
EMail:        francois.piette@overbyte.be         http://www.overbyte.be
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpMultipartDownload1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, OverbyteIcsIniFiles,
  OverbyteIcsHttpProt,
  OverbyteIcsUrl,
  OverbyteIcsMultiProgressBar,
  OverbyteIcsMultipartHttpDownloader,
  OverbyteIcsWndControl;

type
  TProxyMode = (httpProxyNone, httpProxyHttp, httpProxySocks);
  TMultipartHttpDownloadForm = class(TForm)
    DisplayMemo: TMemo;
    MPHttp: TMultipartHttpDownloader;
    TopPanel: TPanel;
    UrlLabel: TLabel;
    LocalFileLabel: TLabel;
    LocalFileEdit: TEdit;
    UrlEdit: TEdit;
    DownloadButton: TButton;
    AbortButton: TButton;
    PartCountEdit: TEdit;
    PartCountLabel: TLabel;
    PauseButton: TButton;
    ResumeButton: TButton;
    AuthGroupBox: TGroupBox;
    Label3: TLabel;
    UserCodeEdit: TEdit;
    Label4: TLabel;
    PasswordEdit: TEdit;
    ProxyGroupBox: TGroupBox;
    ProxyHostEdit: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    ProxyPortEdit: TEdit;
    BottomPanel: TPanel;
    MPBar: TMultiProgressBar;
    CountLabel: TLabel;
    ClearButton: TButton;
    AuthNoneRadioButton: TRadioButton;
    AuthBasicRadioButton: TRadioButton;
    AuthNtlmRadioButton: TRadioButton;
    ProxyNoneRadioButton: TRadioButton;
    ProxyHttpRadioButton: TRadioButton;
    ProxySocksRadioButton: TRadioButton;
    procedure FormShow(Sender: TObject);
    procedure DownloadButtonClick(Sender: TObject);
    procedure MPHttpDisplay(Sender: TObject;
      const Msg: String);
    procedure MPHttpRequestDone(Sender: TObject;
      ErrorCode: Integer; const Reason : String);
    procedure MPHttpProgressAddSegment(Sender: TObject;
      StartOffset, ASpan, InitPos: Int64);
    procedure MPHttpProgressSetPosition(Sender: TObject;
      Index: Integer; Position: Int64);
    procedure MPHttpShowStats(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
    procedure PauseButtonClick(Sender: TObject);
    procedure ResumeButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ClearButtonClick(Sender: TObject);
    procedure AuthNoneRadioButtonClick(Sender: TObject);
    procedure AuthBasicRadioButtonClick(Sender: TObject);
    procedure AuthNtlmRadioButtonClick(Sender: TObject);
    procedure ProxyNoneRadioButtonClick(Sender: TObject);
    procedure ProxyHttpRadioButtonClick(Sender: TObject);
    procedure ProxySocksRadioButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FServerAuth  : THttpAuthType;
    FProxyMode   : TProxyMode;
    procedure Display(const Msg : String);
  end;

var
  MultipartHttpDownloadForm: TMultipartHttpDownloadForm;

implementation

{$R *.dfm}

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyUrl             = 'Url';
    KeyPartCount       = 'PartCount';
    KeyUserCode        = 'UserCode';
    KeyPassword        = 'Password';
    KeyDoAuth          = 'DoAuth';
    KeyProxyHost       = 'ProxyHost';
    KeyProxyPort       = 'ProxyPort';
    KeyUseProxy        = 'UseProxy';
    KeyLocalFile       = 'LocalFile';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.FormShow(Sender: TObject);
var
    IniFile  : TIcsIniFile;
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
        UrlEdit.Text       := IniFile.ReadString(SectionData, KeyUrl,
                                       'http://www.overbyte.be/arch/ics.zip');
        LocalFileEdit.Text := IniFile.ReadString(SectionData,
                                                 KeyLocalFile, '');
        UserCodeEdit.Text  := IniFile.ReadString(SectionData,
                                                 KeyUserCode, '');
        PasswordEdit.Text  := IniFile.ReadString(SectionData,
                                                 KeyPassword, '');
        ProxyHostEdit.Text := IniFile.ReadString(SectionData,
                                                 KeyProxyHost, '');
        ProxyPortEdit.Text := IniFile.ReadString(SectionData,
                                                 KeyProxyPort, '');
        FServerAuth        := THttpAuthType(IniFile.ReadInteger(SectionData,
                                                 KeyDoAuth, Ord(httpAuthNone)));
        FProxyMode         := TProxyMode(IniFile.ReadInteger(SectionData,
                                                 KeyUseProxy, Ord(httpProxyNone)));
        PartCountEdit.Text := IniFile.ReadString(SectionData,
                                                 KeyPartCount, '10');
        IniFile.Free;
        DisplayMemo.Clear;
        CountLabel.Caption    := '';
        AuthNoneRadioButton.Checked   := FServerAuth =  httpAuthNone;
        AuthBasicRadioButton.Checked  := FServerAuth =  httpAuthBasic;
        AuthNtlmRadioButton.Checked   := FServerAuth =  httpAuthNtlm;
        UserCodeEdit.Enabled          := FServerAuth <> httpAuthNone;
        PasswordEdit.Enabled          := FServerAuth <> httpAuthNone;
        ProxyNoneRadioButton.Checked  := FProxyMode  =  httpProxyNone;
        ProxyHttpRadioButton.Checked  := FProxyMode  =  httpProxyHttp;
        ProxySocksRadioButton.Checked := FProxyMode  =  httpProxySocks;
        ProxyHostEdit.Enabled         := FProxyMode  <> httpProxyNone;
        ProxyPortEdit.Enabled         := FProxyMode  <> httpProxyNone;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
//  UrlEdit.Text := 'http://video.codegear.com/btpisos/Delphi_701_Arch_Inline.iso.zip';
//  UrlEdit.Text := 'http://video.codegear.com/btpisos/spacely.2627.5503.6.iso.zip';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData,  KeyUrl,       UrlEdit.Text);
    IniFile.WriteString(SectionData,  KeyLocalFile, LocalFileEdit.Text);
    IniFile.WriteString(SectionData,  KeyUserCode,  UserCodeEdit.Text);
    IniFile.WriteString(SectionData,  KeyPassword,  PasswordEdit.Text);
    IniFile.WriteString(SectionData,  KeyProxyHost, ProxyHostEdit.Text);
    IniFile.WriteString(SectionData,  KeyProxyPort, ProxyPortEdit.Text);
    IniFile.WriteInteger(SectionData, KeyDoAuth,    Ord(FServerAuth));
    IniFile.WriteInteger(SectionData, KeyUseProxy,  Ord(FProxyMode));
    IniFile.WriteString(SectionData,  KeyPartCount, Trim(PartCountEdit.Text));
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.Display(const Msg: String);
begin
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.MPHttpDisplay(
    Sender    : TObject;
    const Msg : String);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.DownloadButtonClick(Sender: TObject);
begin
    MPBar.Clear;
    MPHttp.URL           := Trim(UrlEdit.Text);
    MPHttp.StateFileName := Trim(LocalFileEdit.Text) + '.Status';
    MPHttp.PartCount     := StrToIntDef(Trim(PartCountEdit.Text), 0);
    MPHttp.FileStream    := TFileStream.Create(Trim(LocalFileEdit.Text), fmCreate);
    if FServerAuth <> httpAuthNone then begin
        MPHttp.Username   := UserCodeEdit.Text;
        MPHttp.Password   := PasswordEdit.Text;
        MPHttp.ServerAuth := httpAuthNtlm;
    end
    else begin
        MPHttp.Username   := '';
        MPHttp.Password   := '';
        MPHttp.ServerAuth := httpAuthNone;
    end;
    if FProxyMode = httpProxyNone then begin
        MPHttp.Proxy       := '';
        MPHttp.ProxyPort   := '';
        MPHttp.SocksServer := '';
        MPHttp.SocksPort   := '';
        MPHttp.SocksLevel  := '';
    end
    else if FProxyMode = httpProxyHttp then begin
        MPHttp.Proxy       := ProxyHostEdit.Text;
        MPHttp.ProxyPort   := ProxyPortEdit.Text;
        MPHttp.SocksServer := '';
        MPHttp.SocksPort   := '';
        MPHttp.SocksLevel  := '';
    end
    else begin
        MPHttp.Proxy       := '';
        MPHttp.ProxyPort   := '';
        MPHttp.SocksServer := ProxyHostEdit.Text;
        MPHttp.SocksPort   := ProxyPortEdit.Text;
        MPHttp.SocksLevel  := '5';
    end;
    try
        MPHttp.Start;
    except
        on E:Exception do begin
            MPHttp.FileStream.Free;
            MPHttp.FileStream := nil;
            Display('Unable to start download. ' +
                    E.ClassName + ': ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.MPHttpRequestDone(
    Sender       : TObject;
    ErrorCode    : Integer;
    const Reason : String);
begin
    MPHttp.FileStream.Free;
    MPHttp.FileStream := nil;
    if ErrorCode = 200 then
        Display('Finished')
    else
        Display('Finished, error #' + IntToStr(ErrorCode) + ' ' + Reason);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.MPHttpProgressAddSegment(
    Sender                      : TObject;
    StartOffset, ASpan, InitPos : Int64);
begin
    MPBar.AddSegment(StartOffset, ASpan, InitPos, clBlue);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.MPHttpProgressSetPosition(
    Sender   : TObject;
    Index    : Integer;
    Position : Int64);
begin
    MPBar.SetPosition(Index, Position);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.MPHttpShowStats(
    Sender: TObject);
begin
    CountLabel.Caption :=
            'Bytes: ' + IntToStr(MPHttp.TotalCount) +
             '   %: ' + Format('%3.0f', [MPHttp.PercentDone]) +
          '   Kbps: ' + Format('%6.2f', [MPHttp.CurSpeed]) +
        '  Elapsed: ' + FormatDateTime('hh:nn:ss', MPHttp.ElapsedTime);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.AbortButtonClick(Sender: TObject);
begin
    MPHttp.Abort;
    MPHttp.FileStream.Free;
    MPHttp.FileStream := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.PauseButtonClick(Sender: TObject);
begin
    MPHttp.Pause;
    MPHttp.FileStream.Free;
    MPHttp.FileStream := nil;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.ResumeButtonClick(Sender: TObject);
begin
    MPBar.Clear;
    MPHttp.StateFileName := Trim(LocalFileEdit.Text) + '.Status';
    MPHttp.FileStream    := TFileStream.Create(Trim(LocalFileEdit.Text), fmOpenWrite);
    if FServerAuth <> httpAuthNone then begin
        MPHttp.Username   := UserCodeEdit.Text;
        MPHttp.Password   := PasswordEdit.Text;
        MPHttp.ServerAuth := httpAuthNtlm;
    end
    else begin
        MPHttp.Username   := '';
        MPHttp.Password   := '';
        MPHttp.ServerAuth := httpAuthNone;
    end;
    if FProxyMode = httpProxyNone then begin
        MPHttp.Proxy       := '';
        MPHttp.ProxyPort   := '';
        MPHttp.SocksServer := '';
        MPHttp.SocksPort   := '';
        MPHttp.SocksLevel  := '';
    end
    else if FProxyMode = httpProxyHttp then begin
        MPHttp.Proxy       := ProxyHostEdit.Text;
        MPHttp.ProxyPort   := ProxyPortEdit.Text;
        MPHttp.SocksServer := '';
        MPHttp.SocksPort   := '';
        MPHttp.SocksLevel  := '';
    end
    else begin
        MPHttp.Proxy       := '';
        MPHttp.ProxyPort   := '';
        MPHttp.SocksServer := ProxyHostEdit.Text;
        MPHttp.SocksPort   := ProxyPortEdit.Text;
        MPHttp.SocksLevel  := '5';
    end;
    try
        MPHttp.Resume;
    except
        on E:Exception do begin
            MPHttp.FileStream.Free;
            MPHttp.FileStream := nil;
            Display('Unable to resume download. ' +
                    E.ClassName + ': ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.ClearButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.AuthNoneRadioButtonClick(Sender: TObject);
begin
    if AuthNoneRadioButton.Checked then
        FServerAuth := httpAuthNone;
    UserCodeEdit.Enabled := FServerAuth <> httpAuthNone;
    PasswordEdit.Enabled := FServerAuth <> httpAuthNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.AuthBasicRadioButtonClick(
  Sender: TObject);
begin
    if AuthBasicRadioButton.Checked then
        FServerAuth := httpAuthBasic;
    UserCodeEdit.Enabled := FServerAuth <> httpAuthNone;
    PasswordEdit.Enabled := FServerAuth <> httpAuthNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.AuthNtlmRadioButtonClick(Sender: TObject);
begin
    if AuthNtlmRadioButton.Checked then
        FServerAuth := httpAuthNtlm;
    UserCodeEdit.Enabled := FServerAuth <> httpAuthNone;
    PasswordEdit.Enabled := FServerAuth <> httpAuthNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.ProxyNoneRadioButtonClick(
  Sender: TObject);
begin
    if ProxyNoneRadioButton.Checked then
        FProxyMode := httpProxyNone;
     ProxyHostEdit.Enabled := FProxyMode <> httpProxyNone;
     ProxyPortEdit.Enabled := FProxyMode <> httpProxyNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.ProxyHttpRadioButtonClick(
  Sender: TObject);
begin
    if ProxyHttpRadioButton.Checked then
        FProxyMode := httpProxyHttp;
     ProxyHostEdit.Enabled := FProxyMode <> httpProxyNone;
     ProxyPortEdit.Enabled := FProxyMode <> httpProxyNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMultipartHttpDownloadForm.ProxySocksRadioButtonClick(
  Sender: TObject);
begin
    if ProxySocksRadioButton.Checked then
        FProxyMode := httpProxySocks;
     ProxyHostEdit.Enabled := FProxyMode <> httpProxyNone;
     ProxyPortEdit.Enabled := FProxyMode <> httpProxyNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
