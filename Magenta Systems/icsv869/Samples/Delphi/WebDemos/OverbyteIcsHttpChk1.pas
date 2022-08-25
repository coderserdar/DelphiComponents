{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     September 4, 1999
Version:      1.01
Description:  Use HTTP component to check for valid URL.
              See IsAddressValid function below. Of course you can remove
              display if all you need is a boolean status.
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpChk1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsHttpProt, OverbyteIcsWndControl;

type
  TCheckUrlForm = class(TForm)
    Label1: TLabel;
    URLEdit: TEdit;
    CheckButton: TButton;
    ResultLabel: TLabel;
    HttpCli1: THttpCli;
    Memo1: TMemo;
    procedure CheckButtonClick(Sender: TObject);
    procedure HttpCli1RequestDone(Sender: TObject; RqType: THttpRequest;
      Error: Word);
    procedure HttpCli1HeaderData(Sender: TObject);
  private
    FDoneFlag  : Boolean;
    FDoneError : Word;
  public
    { Public declarations }
    function IsAddressValid(URL : String) : Boolean;
  end;

var
  CheckUrlForm: TCheckUrlForm;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCheckUrlForm.CheckButtonClick(Sender: TObject);
begin
    Memo1.Clear;
    if IsAddressValid(UrlEdit.Text) then
        ResultLabel.Caption := 'Valid'
    else
        ResultLabel.Caption := 'Invalid';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check if an URL is valid. Use HTTP HEAD command to speed up thing. (It    }
{ doesn't retrieve document, just webserver answer header).                 }
{ Syntax of an URL: protocol://[user[:password]@]server[:port]/path         }
function TCheckUrlForm.IsAddressValid(URL : String) : Boolean;
var
    Timeout : longInt;
begin
    try
        FDoneFlag    := FALSE;
        FDoneError   := 0;
        HttpCli1.URL := URL;
        HttpCli1.HeadAsync;
        Timeout := GetTickCount + 30000;  { 30" timeout }
        while not FDoneFlag do begin
{$IFNDEF VER80}
            Sleep(0);
{$ENDIF}
            Application.ProcessMessages;
            if Timeout < Longint(GetTickCount) then begin
                HttpCli1.Abort;
                Result := FALSE;
                Exit;
            end;
        end;
        Result := (HttpCli1.StatusCode = 200);
        { You can suppress next line if you don't want display }
        Memo1.lines.Add('StatusCode = ' + IntToStr(HttpCli1.StatusCode));
    except
        Result := FALSE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TCheckUrlForm.HttpCli1RequestDone(Sender: TObject;
  RqType: THttpRequest; Error: Word);
begin
    FDoneFlag  := TRUE;
    FDoneError := Error;
    { You may suppress following two lines if you don't want any display }
    if Error <> 0 then
        Memo1.Lines.Add('Error #' + IntToStr(Error));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ You can remove this event handler if you don't want to see messages.      }
procedure TCheckUrlForm.HttpCli1HeaderData(Sender: TObject);
begin
    Memo1.Lines.Add(HttpCli1.LastResponse);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

