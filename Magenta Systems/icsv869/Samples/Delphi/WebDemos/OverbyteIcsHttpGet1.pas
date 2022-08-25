{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     January 17, 1998
Version:      8.54
Description:  This sample program show how to get a document from a webserver
              and store it to a file. Also display some progress info.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2018 by François PIETTE
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

Updates:
Feb 4,  2011  V7.00 Angus added bandwidth throttling using TCustomThrottledWSocket
                    Demo shows file download duration and speed
May 21, 2018 V8.54 Added Utils instead of FtpSrvT

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpGet1;

{$I Include\OverbyteIcsDefs.inc}
{$IFNDEF DELPHI7_UP}
    Bomb('This sample requires Delphi 7 or later');
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsHttpProt, StdCtrls, OverbyteIcsIniFiles,
  OverbyteIcsWndControl, OverByteIcsUtils;

type
  THttpGetForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    URLEdit: TEdit;
    ProxyHostEdit: TEdit;
    ProxyPortEdit: TEdit;
    FileNameEdit: TEdit;
    Label5: TLabel;
    GetButton: TButton;
    AbortButton: TButton;
    InfoLabel: TLabel;
    HttpCli1: THttpCli;
    Label10: TLabel;
    BandwidthLimitEdit: TEdit;
    procedure GetButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure HttpCli1DocData(Sender: TObject; Buffer: Pointer;
      Len: Integer);
    procedure HttpCli1HeaderData(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
  private
    { Déclarations privées }
    FInitialized : Boolean;
    FIniFileName : String;
  public
    { Déclarations publiques }
  end;

var
  HttpGetForm: THttpGetForm;

implementation

{$R *.DFM}
const
    SectionData   = 'Data';
    KeyURL        = 'URL';
    KeyProxyHost  = 'ProxyHost';
    KeyProxyPort  = 'ProxyPort';
    KeyFileName   = 'FileName';
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';
    KeyBandwidthLimit = 'BandwidthLimit';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
    InfoLabel.Caption := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized       := TRUE;
        IniFile            := TIcsIniFile.Create(FIniFileName);
        try
            URLEdit.Text       := IniFile.ReadString(SectionData, KeyURL,
                                'http://www.rtfm.be/fpiette/images/overbyte.gif');
            ProxyHostEdit.Text := IniFile.ReadString(SectionData, KeyProxyHost,
                                  '');
            ProxyPortEdit.Text := IniFile.ReadString(SectionData, KeyProxyPort,
                                  '80');
            FileNameEdit.Text  := IniFile.ReadString(SectionData, KeyFileName,
                                  'test.tmp');
            Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
            Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
            Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            BandwidthLimitEdit.Text := IniFile.ReadString(SectionData, KeyBandwidthLimit, '1000000');
        finally
            IniFile.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteString(SectionData, KeyURL,       URLEdit.Text);
        IniFile.WriteString(SectionData, KeyProxyHost, ProxyHostEdit.Text);
        IniFile.WriteString(SectionData, KeyProxyPort, ProxyPortEdit.Text);
        IniFile.WriteString(SectionData, KeyFileName,  FileNameEdit.Text);
        IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
        IniFile.WriteString(SectionData, KeyBandwidthLimit,  BandwidthLimitEdit.Text);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.GetButtonClick(Sender: TObject);
var
    StartTime: Longword;
    Duration, BytesSec, ByteCount: integer;
    Temp: string;
begin
    HttpCli1.URL        := URLEdit.Text;
    HttpCli1.Proxy      := ProxyHostEdit.Text;
    HttpCli1.ProxyPort  := ProxyPortEdit.Text;
    HttpCli1.RcvdStream := TFileStream.Create(FileNameEdit.Text, fmCreate);
{$IFDEF BUILTIN_THROTTLE}
    HttpCli1.BandwidthLimit := StrToIntDef(BandwidthLimitEdit.Text, 1000000);
    if HttpCli1.BandwidthLimit > 0 then
        HttpCli1.Options := HttpCli1.Options + [httpoBandwidthControl];
{$ENDIF}
    GetButton.Enabled   := FALSE;
    AbortButton.Enabled := TRUE;
    InfoLabel.Caption   := 'Loading';
    try
        try
            StartTime := GetTickCount;
            HttpCli1.Get;
            Duration := GetTickCount - StartTime;
            ByteCount := HttpCli1.RcvdStream.Size;
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
            InfoLabel.Caption := Temp
        except
            on E: EHttpException do begin
                InfoLabel.Caption := 'Failed : ' +
                                     IntToStr(HttpCli1.StatusCode) + ' ' +
                                     HttpCli1.ReasonPhrase;;
            end
            else
                raise;
        end;
    finally
        GetButton.Enabled   := TRUE;
        AbortButton.Enabled := FALSE;
        HttpCli1.RcvdStream.Destroy;
        HttpCli1.RcvdStream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.HttpCli1DocData(Sender: TObject; Buffer: Pointer;
  Len: Integer);
begin
    InfoLabel.Caption := IntToStr(HttpCli1.RcvdCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.HttpCli1HeaderData(Sender: TObject);
begin
    InfoLabel.Caption := InfoLabel.Caption + '.';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.AbortButtonClick(Sender: TObject);
begin
    HttpCli1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

