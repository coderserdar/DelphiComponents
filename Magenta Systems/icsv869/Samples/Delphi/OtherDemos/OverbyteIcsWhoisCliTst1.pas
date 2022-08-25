{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Whois is a Whois client
              Install the component OverbyteIcsWhoisCli.pas first.
Creation:     Aug 2002
Updated:      Jan 2019
Version:      8.60
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
Legal issues: Copyright (C) 2002-2019 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

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



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWhoisCliTst1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ComCtrls, ExtCtrls, OverbyteIcsWinSock,
  OverbyteIcsWSocket, OverbyteIcsUtils, OverbyteIcsWhoisCli;

const
  WM_APPSTARTUP = WM_USER + 1;

type
  TWhoisDemoForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    QueryButton: TButton;
    CancelButton: TButton;
    HostEdit: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    WhoisCli1: TIcsWhoisCli;
    CloseButton: TButton;
    WhoisMethod: TRadioGroup;
    QueryEdit: TComboBox;
    procedure QueryButtonClick(Sender: TObject);
    procedure WhoisCli1QueryDone(Sender: TObject; Error: Word);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private
      procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
  end;

var
  WhoisDemoForm: TWhoisDemoForm;


implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.CloseButtonClick(Sender: TObject);
begin
    Close;
end;

procedure TWhoisDemoForm.FormShow(Sender: TObject);
var
    I, J: Integer;
    S1, S2: String;
begin

  // build drop down list of Whois servers, not needed if only
  // automatic queries are being done
    if WhoisCli1.WhoisServers.Count > 0 then begin
        HostEdit.Items.Clear;
        for I := 0 to WhoisCli1.WhoisServers.Count - 1 do begin
            S1 := WhoisCli1.WhoisServers[I];
            J := Pos(IcsSpace, S1);
            if J = 0 then
                HostEdit.Items.Add(S1)
            else begin
                S2 := Copy(S1, 1, J);
                if S2 = '00 ' then
                    S2 := 'IP Addresses]'
                else if S2 = '?? ' then
                    S2 := 'Top Level Domains]'
                else
                    S2 := S2 + 'Domains]';
                HostEdit.Items.Add(Copy(S1, J + 1, 99) + ' [' + S2);
            end;
        end;
        HostEdit.ItemIndex := 0;
    end;

    { We use a custom message to initialize things once the form }
    { is visible                                                 }
    PostMessage(Handle, WM_APPSTARTUP, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.WMAppStartup(var msg: TMessage);
begin
    Update; { Let the window be visible completely }
    if ParamCount > 0 then begin
        QueryEdit.Text := ParamStr(1);
        QueryButtonClick(Self);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.QueryButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
    QueryButton.Enabled  := FALSE;
    CancelButton.Enabled := TRUE;
    WhoisCli1.Query      := QueryEdit.Text;
    WhoisCli1.Host       := HostEdit.Text ;
    if WhoisMethod.ItemIndex = 0 then
        WhoisCli1.StartAutoQuery
     else
        WhoisCli1.StartQuery;
    DisplayMemo.Lines.Add ('Query initiated for ' + WhoisCli1.Query);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.WhoisCli1QueryDone(Sender: TObject; Error: Word);
begin
    DisplayMemo.Lines.Add (WhoisCli1.WhoisResp);
    DisplayMemo.Lines.Add ('Query completed');
    QueryButton.Enabled  := TRUE;
    CancelButton.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.CancelButtonClick(Sender: TObject);
begin
    WhoisCli1.Abort;
    QueryButton.Enabled  := TRUE;
    CancelButton.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

