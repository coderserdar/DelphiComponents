{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS HTTPS REST functions demo login form.
Creation:     May 2022
Updated:      May 2022
Version:      8.69
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2022 by Angus Robertson, Magenta Systems Ltd,
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

History:
May 16, 2022 - V8.69 Basline


Note, shared by multiple projects.  


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

 unit OverbyteIcsLogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TFormLogin = class(TForm)
    Label1: TLabel;
    LabelMethod: TLabel;
    AuthUsername: TEdit;
    Label2: TLabel;
    AuthPassword: TEdit;
    doOK: TBitBtn;
    doCancel: TBitBtn;
    ListMethods: TListBox;
    LabelPageURL: TLabel;
    procedure doOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLogin: TFormLogin;

implementation

{$R *.dfm}

procedure TFormLogin.doOKClick(Sender: TObject);
begin
    if (AuthUsername.Text = '') or
                  (AuthPassword.Text = '') then begin
        Beep;
        Exit;
    end;
    ModalResult := mrOk; 
end;

end.
