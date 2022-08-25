{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demo program for TDnsQuery component.
              It shows how to query a list of domains without using loops but
              using pure event driven operation
Creation:     March 06, 2005
Version:      8.64
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1999-2020 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
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

History:
Dec 21, 2008 V1.01 F.Piette added astring casts in DnsQuery1RequestDone and
             an AnsiString cast in ResolveNext to avoid a warning when
             compiling with Delphi 2009.
Jul 4, 2012  V8.00 Angus changed to Goggle DNS 8.8.8.8
Mar 10, 2020 V8.64 Added support for International Domain Names for Applications
                     (IDNA), i.e. using accents and unicode characters in domain names.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverByteIcsDnsResolver1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsDnsQuery, OverbyteIcsWSocket, StdCtrls, ExtCtrls;

const
  WM_RESOLVE_NEXT = WM_USER + 1;

type
  TDnsResolverForm = class(TForm)
    DomainListBox: TListBox;
    ResultListBox: TListBox;
    QueryButton: TButton;
    DnsQuery1: TDnsQuery;
    Memo1: TMemo;
    Timer1: TTimer;
    DnsIPEdit: TEdit;
    Label1: TLabel;
    MXRecordRadioButton: TRadioButton;
    AddressRadioButton: TRadioButton;
    procedure QueryButtonClick(Sender: TObject);
    procedure DnsQuery1RequestDone(Sender: TObject; Error: Word);
    procedure Timer1Timer(Sender: TObject);
  private
    FCurrentItem  : Integer;
    FCurrentCount : Integer;
    FCurrentID    : Integer;
    procedure ResolveNext;
    procedure WMResolveNext(var Msg: TWMDropFiles); message WM_RESOLVE_NEXT;
  public
    { Public declarations }
  end;

var
  DnsResolverForm: TDnsResolverForm;

implementation

{$R *.dfm}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsResolverForm.QueryButtonClick(Sender: TObject);
begin
    DnsQuery1.Addr := DnsIPEdit.Text;
    ResultListBox.Clear;
    FCurrentCount   := 0;
    QueryButton.Enabled := FALSE;
    PostMessage(Handle, WM_RESOLVE_NEXT, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsResolverForm.WMResolveNext(var Msg: TWMDropFiles);
begin
    ResolveNext;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsResolverForm.ResolveNext;
begin
    if FCurrentItem >= DomainListBox.Items.Count then begin
        Memo1.Lines.Add('Finished');
        // Reset counter for next time
        FCurrentItem    := 0;
        QueryButton.Enabled := TRUE;
        Exit;
    end;
    Timer1.Interval := 2000;
    Timer1.Enabled  := TRUE;
    if MXRecordRadioButton.Checked then begin
        Memo1.Lines.Add('Resolving ' + DomainListBox.Items[FCurrentItem]);
        FCurrentID := DnsQuery1.MXLookup(DomainListBox.Items[FCurrentItem]);
    end
    else begin
        Memo1.Lines.Add('Resolving www.' + DomainListBox.Items[FCurrentItem]);
        FCurrentID := DnsQuery1.ALookup('www.' + DomainListBox.Items[FCurrentItem]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsResolverForm.DnsQuery1RequestDone(Sender: TObject; Error: Word);
var
    nIndex : Integer;
begin
    // Ignore old request
    if DnsQuery1.ResponseID <> FCurrentID then
        Exit;
    // Ignore empty reply
    if DnsQuery1.ResponseANCount < 1 then
        Exit;
    FCurrentCount  := 0;
    Timer1.Enabled := FALSE;
    nIndex         := DnsQuery1.AnswerTag[0];
    if nIndex < 0 then
        ResultListBox.Items.Add('*** not found ***')
    else begin
        if MXRecordRadioButton.Checked then
            ResultListBox.Items.Add(DnsQuery1.MXExchange[nIndex])
        else
            ResultListBox.Items.Add(
                String(WSocket_inet_ntoa(DnsQuery1.Address[nIndex])));
    end;
    Inc(FCurrentItem);
    PostMessage(Handle, WM_RESOLVE_NEXT, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDnsResolverForm.Timer1Timer(Sender: TObject);
begin
    Inc(FCurrentCount);
    Memo1.Lines.Add(DomainListBox.Items[FCurrentItem] + ' Timed out (' +
                    IntToStr(FCurrentCount) + ')');
    if FCurrentCount >= 3 then begin
        // Retries exhausted
        ResultListBox.Items.Add('*** timed out ***');
        Inc(FCurrentItem);
        FCurrentCount := 0;
    end;
    PostMessage(Handle, WM_RESOLVE_NEXT, 0, 0)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
