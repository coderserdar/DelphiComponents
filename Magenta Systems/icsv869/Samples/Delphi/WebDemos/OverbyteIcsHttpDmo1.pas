{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Creation:     December 5, 1997
Version:      1.02
Description:  Sample program to demonstrate some of the THttpCli features.
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
Jan 16, 1998  V1.01 Added a proxy edit box.
Aug 18, 2001  V1.02 Shay Horovitz <shay@imesh.com> corrected a small bug
              in LoadMemoFromMemoryStream.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpDmo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsHttpProt, ExtCtrls, OverbyteIcsWndControl,
  OverbyteIcsStreams;

const
    HttpDemoVersion  = 102;
    CopyRight        = ' HttpDemo (c) 1997-2010 by Francois PIETTE. V1.02';

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
    Reader : TIcsStreamReader;
    Line : String;
begin
    Stream.Position := 0;
    Reader := TIcsStreamReader.Create(Stream, TRUE, CP_ACP);
    try
        while Reader.ReadLine(Line) do
            Memo.Lines.Add(Line);
    finally
        Reader.Free;
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

