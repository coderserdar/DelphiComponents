{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     January 17, 1998
Version:      1.00
Description:  This sample program show how to get a document from a webserver
              s store it to a file. Also display some progress info.
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

Updates:

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit HttpGet1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, HttpProt, StdCtrls, IniFiles;

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


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    InfoLabel.Caption := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized       := TRUE;
        IniFile            := TIniFile.Create(FIniFileName);
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
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyURL,       URLEdit.Text);
    IniFile.WriteString(SectionData, KeyProxyHost, ProxyHostEdit.Text);
    IniFile.WriteString(SectionData, KeyProxyPort, ProxyPortEdit.Text);
    IniFile.WriteString(SectionData, KeyFileName,  FileNameEdit.Text);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpGetForm.GetButtonClick(Sender: TObject);
begin
    HttpCli1.URL        := URLEdit.Text;
    HttpCli1.Proxy      := ProxyHostEdit.Text;
    HttpCli1.ProxyPort  := ProxyPortEdit.Text;
    HttpCli1.RcvdStream := TFileStream.Create(FileNameEdit.Text, fmCreate);
    GetButton.Enabled   := FALSE;
    AbortButton.Enabled := TRUE;
    InfoLabel.Caption   := 'Loading';
    try
        try
            HttpCli1.Get;
            InfoLabel.Caption := 'Received ' +
                                 IntToStr(HttpCli1.RcvdStream.Size) + ' bytes';
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

