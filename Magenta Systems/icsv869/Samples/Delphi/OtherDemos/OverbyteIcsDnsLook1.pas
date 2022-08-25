{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demonstrate how to use TWSocket to asynchroneously resolve a
              host name to an IP address.
Creation:     October 29, 1997
Version:      8.00
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
Oct 30, 1997 V1.01 Made the code compatible with Delphi 1.
Oct 31, 1997 V1.02 Added a cancel button
jan 31, 1998 V1.03 Added a scroll bar. Display addresses count
Aug 03, 1998 V1.04 Added reverse DNS lookup
Mar 13, 1999 V1.05 Added a button to get local ip list
Dec 21, 2008 V1.06 F.Piette added a string cast in FormCreate to avoid a
             warning when compiling with Delphi 2009.


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDnsLook1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsWinSock, OverbyteIcsWndControl,
  OverbyteIcsWSocket;

const
  DNSLookVersion = 800;
  CopyRight : String = ' DnsLook (c) 1997-2012 F. Piette V8.00 ';

type
  TDnsLookupForm = class(TForm)
    WSocket1: TWSocket;
    HostEdit: TEdit;
    Label1: TLabel;
    IPLabel: TLabel;
    LookupButton: TButton;
    CancelButton: TButton;
    IPListMemo: TMemo;
    ReverseLookupButton: TButton;
    LocalIPButton: TButton;
    Label2: TLabel;
    SocketFamilyComboBox: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure LookupButtonClick(Sender: TObject);
    procedure WSocket1DnsLookupDone(Sender: TObject; Error: Word);
    procedure CancelButtonClick(Sender: TObject);
    procedure ReverseLookupButtonClick(Sender: TObject);
    procedure LocalIPButtonClick(Sender: TObject);
  end;

var
  DnsLookupForm: TDnsLookupForm;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsLookupForm.FormCreate(Sender: TObject);
begin
    IPLabel.Caption      := '';
    LookupButton.Enabled := TRUE;
    CancelButton.Enabled := FALSE;
    HostEdit.Text        := String(LocalHostName);
    IPListMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsLookupForm.LookupButtonClick(Sender: TObject);
begin
    LookupButton.Enabled := FALSE;     { Prevent recursive call            }
    ReverseLookupButton.Enabled := FALSE;
    CancelButton.Enabled := TRUE;      { Enable canceling                  }
    HostEdit.SelectAll;                { handy for the user                }
    ActiveControl   := HostEdit;       { he likes to have the cursor there }
    WSocket1.SocketFamily := TSocketFamily(SocketFamilyCombobox.ItemIndex);
    IPLabel.Caption := 'Waiting for DNS...';
    IPListMemo.Clear;
    WSocket1.DnsLookup(HostEdit.Text); { Start DnsLookup                   }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsLookupForm.ReverseLookupButtonClick(Sender: TObject);
begin
    LookupButton.Enabled := FALSE;     { Prevent recursive call            }
    ReverseLookupButton.Enabled := FALSE;
    CancelButton.Enabled := TRUE;      { Enable canceling                  }
    HostEdit.SelectAll;                { handy for the user                }
    ActiveControl   := HostEdit;       { he likes to have the cursor there }
    WSocket1.SocketFamily := TSocketFamily(SocketFamilyCombobox.ItemIndex);
    IPLabel.Caption := 'Waiting for DNS...';
    IPListMemo.Clear;
    WSocket1.ReverseDnsLookup(HostEdit.Text); { Start DnsLookup                   }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsLookupForm.WSocket1DnsLookupDone(Sender: TObject;
  Error: Word);
begin
    if Error = 0 then begin
        IPLabel.Caption  := WSocket1.DnsResult +
                            '  (' + IntToStr(WSocket1.DnsResultList.Count) +
                            ' addresses)';
        IPListMemo.Lines := WSocket1.DnsResultList;
    end
    else
        IPLabel.Caption  := 'NOT FOUND, ERROR #' + IntToStr(Error);
    LookupButton.Enabled := TRUE;
    ReverseLookupButton.Enabled := TRUE;
    CancelButton.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsLookupForm.CancelButtonClick(Sender: TObject);
begin
    IPLabel.Caption      := '';
    LookupButton.Enabled := TRUE;
    CancelButton.Enabled := FALSE;
    ActiveControl        := HostEdit;
    WSocket1.CancelDnsLookup;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsLookupForm.LocalIPButtonClick(Sender: TObject);
begin
     GetLocalIPList(IPListMemo.Lines, TSocketFamily(SocketFamilyCombobox.ItemIndex));
     if IPListMemo.Lines.Count > 0 then
         HostEdit.Text := IPListMemo.Lines[0];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

