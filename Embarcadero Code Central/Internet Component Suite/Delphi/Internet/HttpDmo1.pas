{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Creation:     December 5, 1997
Version:      1.01
Description:  Sample program to demonstrate some of the THttpCli features.
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
Jan 16, 1998  V1.01 Added a proxy edit box.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit HttpDmo1;

interface

uses
  WinProcs, WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, HttpProt, ExtCtrls;

type
  THttpToMemoForm = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    URLEdit: TEdit;
    GoButton: TButton;
    HttpCli1: THttpCli;
    DisplayMemo: TMemo;
    Label2: TLabel;
    ProxyEdit: TEdit;
    procedure GoButtonClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  HttpToMemoForm: THttpToMemoForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadMemoFromMemoryStream(Memo : TMemo; Stream : TMemoryStream);
var
    p, q, r : PChar;
begin
    p := Stream.Memory;
    q := p + Stream.Size - 1;
    r := p;
    while (p <> nil) and (p < q) do begin
        while (p < q) and (p^ <> #13) and (p^ <> #10) do
            Inc(p);
        Memo.Lines.Add(Copy(StrPas(r), 1, p - r));
        if (p[0] = #13) and (p[1] = #10) then
           Inc(p, 2)
        else
           Inc(p);
        r := p;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpToMemoForm.GoButtonClick(Sender: TObject);
var
    Data : TMemoryStream;
begin
    DisplayMemo.Clear;

    GoButton.Enabled := FALSE;
    Data := TMemoryStream.Create;
    try
        httpcli1.URL        := URLEdit.Text;
        httpcli1.Proxy      := ProxyEdit.Text;
        httpcli1.ProxyPort  := '80';
        httpcli1.RcvdStream := Data;
        httpcli1.Get;
        LoadMemoFromMemoryStream(DisplayMemo, Data);
    finally
        Data.Free;
        GoButton.Enabled := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpToMemoForm.Panel1Resize(Sender: TObject);
begin
    GoButton.Left   := Panel1.Width - GoButton.Width - 8;
    URLEdit.Width   := Panel1.Width - URLEdit.Left - GoButton.Width - 16;
    ProxyEdit.Width := URLEdit.Width;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

