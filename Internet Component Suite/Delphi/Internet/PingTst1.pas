{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This demo show how to use the TPing object to ping any host.
Creation:     November 30, 1997
Version:      1.02
EMail:        francois.piette@pophost.eunet.be    francois.piette@pophost.eunet.be
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
Dec 13, 1997 V1.01 Use the new OnEchoRequest and OnEchoReply events.
Dec 26, 1998 V1.02 Changed event handler for new TPing version (1.10)

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit PingTst1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Ping, StdCtrls;

type
  TPingTstForm = class(TForm)
    Ping1: TPing;
    Label1: TLabel;
    HostEdit: TEdit;
    PingButton: TButton;
    DisplayMemo: TMemo;
    CancelButton: TButton;
    procedure PingButtonClick(Sender: TObject);
    procedure Ping1Display(Sender: TObject; Icmp: TObject; Msg: String);
    procedure Ping1DnsLookupDone(Sender: TObject; Error: Word);
    procedure CancelButtonClick(Sender: TObject);
    procedure Ping1EchoRequest(Sender: TObject; Icmp: TObject);
    procedure Ping1EchoReply(Sender: TObject; Icmp: TObject; Error: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  PingTstForm: TPingTstForm;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.PingButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
    DisplayMemo.Lines.Add('Resolving host ''' + HostEdit.Text + '''');
    PingButton.Enabled   := FALSE;
    CancelButton.Enabled := TRUE;
    Ping1.DnsLookup(HostEdit.Text);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.Ping1DnsLookupDone(Sender: TObject; Error: Word);
begin
    CancelButton.Enabled := FALSE;
    PingButton.Enabled   := TRUE;

    if Error <> 0 then begin
        DisplayMemo.Lines.Add('Unknown Host ''' + HostEdit.Text + '''');
        Exit;
    end;

    DisplayMemo.Lines.Add('Host ''' + HostEdit.Text + ''' is ' + Ping1.DnsResult);
    Ping1.Address := Ping1.DnsResult;
    Ping1.Ping;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.Ping1Display(Sender: TObject; Icmp: TObject; Msg: String);
begin
    DisplayMemo.Lines.Add(Msg);
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.CancelButtonClick(Sender: TObject);
begin
    Ping1.CancelDnsLookup;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.Ping1EchoRequest(Sender: TObject; Icmp: TObject);
begin
    DisplayMemo.Lines.Add('Sending ' + IntToStr(Ping1.Size) + ' bytes to ' +
                          Ping1.HostName + ' (' + Ping1.HostIP + ')');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TPingTstForm.Ping1EchoReply(Sender: TObject; Icmp: TObject; Error: Integer);
begin
    if Error = 0 then
        DisplayMemo.Lines.Add('Cannot ping host (' + Ping1.HostIP + ') : ' +
                              Ping1.ErrorString)
    else
        DisplayMemo.Lines.Add('Received ' + IntToStr(Ping1.Reply.DataSize) +
                              ' bytes from ' + Ping1.HostIP +
                              ' in ' + IntToStr(Ping1.Reply.RTT) + ' msecs');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

