{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This demo show how to use the TPing object to ping any host.
Creation:     November 30, 1997
Version:      6.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2007 by François PIETTE
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
Dec 13, 1997 V1.01 Use the new OnEchoRequest and OnEchoReply events.
Dec 26, 1998 V1.02 Changed event handler for new TPing version (1.10)
Nov 10, 2002 V1.03 Added Reply.Status in display when failed
                   Changed argument name from Error to Status in EchoReply
                   event (same change has in component).
Mar 26, 2006 V6.00 Created new version 6.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsPingTst1;

{$I OverbyteIcsDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, StdCtrls, Controls,
  OverbyteIcsWndControl, OverbyteIcsPing;

const
  PingTestVersion    = 600;
  CopyRight : String = ' PingTest (c) 1997-2007 Francois Piette  V6.00 ';

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
    procedure Ping1EchoReply(Sender: TObject; Icmp: TObject; Status: Integer);
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
procedure TPingTstForm.Ping1EchoReply(
    Sender : TObject;
    Icmp   : TObject;
    Status : Integer);
begin
    if Status <> 0 then
        { Success }
        DisplayMemo.Lines.Add('Received ' + IntToStr(Ping1.Reply.DataSize) +
                              ' bytes from ' + Ping1.HostIP +
                              ' in ' + IntToStr(Ping1.Reply.RTT) + ' msecs')
    else
        { Failure }
        DisplayMemo.Lines.Add('Cannot ping host (' + Ping1.HostIP + ') : ' +
                              Ping1.ErrorString +
                              '. Status = ' + IntToStr(Ping1.Reply.Status));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

