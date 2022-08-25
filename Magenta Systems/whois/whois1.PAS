{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Whois is a Whois client
              Install the components in WhoisCli.pas and WSocket.pas first.
Creation:     27 August 2002 by Angus Robertson, Magenta Systems Ltd
Version:      1.00
EMail:        http://www.overbyte.be       francois.piette@overbyte.be
              http://www.rtfm.be/fpiette   francois.piette@rtfm.be
              francois.piette@pophost.eunet.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2001 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be> <francois.piette@pophost.eunet.be>

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
unit Whois1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, WhoisCli, WinSock, WSocket;

const
  WhoisVersion             = 100;
  CopyRight    : String     = ' Whois (c) 2002 F. Piette V1.00 ';
  WM_APPSTARTUP = WM_USER + 1;

type
  TWhoisDemoForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    QueryEdit: TEdit;
    QueryButton: TButton;
    CancelButton: TButton;
    HostEdit: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    WhoisCli1: TWhoisCli;
    procedure QueryButtonClick(Sender: TObject);
    procedure WhoisCli1QueryDone(Sender: TObject; Error: Word);
    procedure WhoisCli1SessionConnected(Sender: TObject; Error: Word);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
      procedure WMAppStartup(var msg: TMessage); message WM_APPSTARTUP;
  end;

var
  WhoisDemoForm: TWhoisDemoForm;

const
{$IFDEF VER80}
    BufferSize = 255;     { Delphi 1 is limited to 255 bytes }
{$ELSE}
    BufferSize = 2048;
{$ENDIF}

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.FormShow(Sender: TObject);
begin
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
    WhoisCli1.StartQuery;
    DisplayMemo.Lines.Add ('Query started: ' + WhoisCli1.Query);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.WhoisCli1SessionConnected(Sender: TObject; Error: Word);
begin
    if Error = 0 then
        DisplayMemo.Lines.Add ('Connected to host: ' + WhoisCli1.Host);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.WhoisCli1QueryDone(Sender: TObject; Error: Word);
var
    I, J : Integer;
    TempList: TStringList ;
begin
    TempList := TStringList.Create ;
    try
        if Error <> 0 then begin
            if Error = WSAECONNREFUSED then
                DisplayMemo.Lines.Add ('No Whois service available.')
            else if Error = WSAETIMEDOUT then
               DisplayMemo.Lines.Add ('Host unreachable.')
            else
               DisplayMemo.Lines.Add ('Error #' + IntToStr(Error));
        end
        else
        begin
            TempList.Text := WhoisCli1.WhoisResp ;    // method converts LF only result
            DisplayMemo.Lines.AddStrings (TempList) ; // so it can be added properly

        // check if result includes redirection to another Whois server -
            for I := 0 to Pred (TempList.Count) do    // and searched more easily
            begin
                if Pos ('Whois Server:', TempList [I]) > 0 then
                begin
                    J := Pos (':', TempList [I]) ;
                    WhoisCli1.Host := Trim (Copy (TempList [I], Succ (J), 99)) ;
                    WhoisCli1.StartQuery;
                    DisplayMemo.Lines.Add ('Secondary Query: ' +
                                    WhoisCli1.Query + ' to ' + WhoisCli1.Host) ;
                    exit ;
                end ;
            end ;
        end ;
        QueryButton.Enabled  := TRUE;
        CancelButton.Enabled := FALSE;
       { If we started from command line, then close application }
        if ParamCount > 0 then
          Close;
    finally
        TempList.Free ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWhoisDemoForm.CancelButtonClick(Sender: TObject);
begin
    WhoisCli1.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

