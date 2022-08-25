{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Creation:     December 4, 1997
Version:      1.03
Description:  Sample program to demonstrate some of the THttpCli features.
              (POST a message to a CGI)
              (requested by Walter Daniel Leon Salas" <wdaniel@hotmail.com>)
              You can see what HttpPg does automatically using your browser
              and surfing to http://www.unired.net.pe/mensatel.html HttpPg
              does programmatically what you can do manually at this page using
              your browser.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
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
Dec 28, 1997  Added a TMemo to display the POST's result.
Jan 16, 1998  Added a Proxy edit box. Added ini file stuff.
              Better error handling. Added abort button.
Feb 10, 1999  Corrected Encode function (a test was done the wrong way).
              Thanks to Howie Hamlin <howie@hoot.com> for finding that one.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpPg1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  OverbyteIcsIniFiles, OverbyteIcsUtils, OverbyteIcsWndControl,
  OverbyteIcsWSocket, OverbyteIcsHttpProt;

const
    HttpPgVersion = 101;

type
  THttpTestForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    UserIDEdit: TEdit;
    EMailEdit: TEdit;
    MessageEdit: TEdit;
    SendButton: TButton;
    HttpCli1: THttpCli;
    ProxyEdit: TEdit;
    Label4: TLabel;
    AbortButton: TButton;
    procedure SendButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
  private
    { Déclarations privées }
    FIniFileName : String;
    FInitialized : Boolean;
  public
    { Déclarations publiques }
  end;

var
  HttpTestForm: THttpTestForm;

implementation

{$R *.DFM}
const
    SectionData   = 'Data';
    KeyUserID     = 'UserID';
    KeyUserName   = 'UserName';
    KeyEMail      = 'EMail';
    KeyMessage    = 'Message';
    KeyProxy      = 'Proxy';
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.FormCreate(Sender: TObject);
begin
    DisplayMemo.Clear;
    FIniFileName := GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Restore the form position and size, restore the datas for edit boxes.     }
procedure THttpTestForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        try
            UserIDEdit.Text  := IniFile.ReadString(SectionData, KeyUserID,
                                '27313');
            EMailEdit.Text   := IniFile.ReadString(SectionData, KeyEMail,
                                'francois.piette@overbyte.be');
            ProxyEdit.Text   := IniFile.ReadString(SectionData, KeyProxy,
                                '');
            MessageEdit.Text := IniFile.ReadString(SectionData, KeyMessage,
                                'Hello World ! (Message sent by HttpPg).');

            Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
            Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
            Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        finally
            IniFile.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Save the form position and size, save the datas for edit boxes.           }
procedure THttpTestForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteString(SectionData, KeyUserID,    UserIDEdit.Text);
        IniFile.WriteString(SectionData, KeyProxy,     ProxyEdit.Text);
        IniFile.WriteString(SectionData, KeyMessage,   MessageEdit.Text);
        IniFile.WriteString(SectionData, KeyEMail,     EMailEdit.Text);
        IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Encode the data to be sent to the CGI                                     }
function Encode(const msg : AnsiString) : AnsiString;
var
    I : Integer;
begin
    Result := '';
    for I := 1 to Length(msg) do begin
        if msg[I] = ' ' then
            Result := Result + '+'
        else if msg[I] in ['a'..'z', 'A'..'Z', '0'..'9'] then
            Result := Result + msg[I]
        else
            Result := Result + '%' + IcsIntToHexA(ord(msg[I]), 2);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Do the job !                                                              }
procedure THttpTestForm.SendButtonClick(Sender: TObject);
var
    DataIn  : TMemoryStream;
    DataOut : TMemoryStream;
    Buf     : AnsiString;
begin
    DisplayMemo.Clear;
    DataIn  := TMemoryStream.Create;  { For the response        }
    DataOut := TMemoryStream.Create;  { For the data to be sent }
    try
        { Build the data to be sent to the CGI. }
        Buf     := 'ID=' + Encode(AnsiString(UserIDEdit.Text)) +
                   '&REMITE=' + Encode(AnsiString(EMailEdit.Text)) +
                   '&MENSAJE=' + Encode(AnsiString(MessageEdit.Text));
        { Write the data to the stream which will be used to send }
        DataOut.Write(Buf[1], Length(Buf));
        { Position the stream at the beginning or nothing will be sent }
        DataOut.Seek(0, soFromBeginning);

        { Setup the HTTP component to transmit }
        httpcli1.SendStream := DataOut;
        httpcli1.RcvdStream := DataIn;
        httpcli1.Proxy      := ProxyEdit.Text;
        httpcli1.ProxyPort  := '80';
        HttpCli1.URL        := 'http://www.unired.net.pe/cgi-bin/a.out';

        SendButton.Enabled  := FALSE;
        AbortButton.Enabled := TRUE;
        try
            try
                httpcli1.Post;
                { Data sent, copy the webserver response to the DisplayMemo }
                DataIn.Seek(0, 0);
                DisplayMemo.Lines.LoadFromStream(DataIn);
            except
                { An error occured ! }
                DisplayMemo.Lines.Add('Failed : ' + HttpCli1.ReasonPhrase);
                raise
            end;
        finally
            SendButton.Enabled  := TRUE;
            AbortButton.Enabled := FALSE;
        end;
    finally
        DataOut.Free;
        DataIn.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.AbortButtonClick(Sender: TObject);
begin
    HttpCli1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

