{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Creation:     December 30, 1997
Version:      6.00
Description:  Sample program to demonstrate some of the THttpCli features.
              (POST a message to a CGI)
              (requested by  "Coen" <supersys@power.co.za>)
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
Jul 16, 2008 V6.0 A.Garrels made it ICS V6 compatible. 

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpAsp1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsWSocket, OverbyteIcsHttpProt, ExtCtrls,
  OverbyteIcsWndControl;

const
    HttpAspVersion = 600;

type
  THttpTestForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    SupplierIDEdit: TEdit;
    PinEdit: TEdit;
    SendButton: TButton;
    HttpCli1: THttpCli;
    procedure FormCreate(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  HttpTestForm: THttpTestForm;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.FormCreate(Sender: TObject);
begin
    PinEdit.Text        := '';
    SupplierIDEdit.Text := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpTestForm.SendButtonClick(Sender: TObject);
var
    DataIn  : TMemoryStream;
    DataOut : TMemoryStream;
    Buf     : String;
begin
    DisplayMemo.Clear;
    DataIn  := TMemoryStream.Create;
    DataOut := TMemoryStream.Create;
    try
        Buf     := 'suplno=' + Trim(SupplierIDEdit.Text) +
                   '&PIN=' + Trim(PinEdit.Text) +
                   '&LOGIN=Login';
        DataOut.Write(Buf[1], Length(Buf));
        DataOut.Seek(0, soFromBeginning);

        httpcli1.SendStream := DataOut;
        httpcli1.RcvdStream := DataIn;
        httpcli1.Proxy      := 'intsrv02';
        httpcli1.ProxyPort  := '80';
        HttpCli1.Cookie     := 'ASPSESSIONID=OUYRWOSPOFGGPSSF';
        HttpCli1.URL        := 'http://www.transmed.co.za/webserv/menu.asp';

        SendButton.Enabled := FALSE;
        try
            httpcli1.Post;
        finally
            SendButton.Enabled := TRUE;
            DataIn.Seek(0, 0);
            DisplayMemo.Lines.LoadFromStream(DataIn);
        end;
    finally
        DataOut.Free;
        DataIn.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

