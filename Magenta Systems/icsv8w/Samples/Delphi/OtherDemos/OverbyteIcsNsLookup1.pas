{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      NsLookup
Description:  Demo for DnsQuery ICS component.
Author:       François Piette
Creation:     January 29, 1999
Version:      8.65
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1999-2019 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. F
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

History:
Feb 27, 1999 V1.01 Added PTR lookup (reverse DNS lookup)
Mar 07, 1999 V1.02 Adapted for Delphi 1
May 29, 2005 V1.03 Added TCP/UDP protocol selection. Added version infos.
Mar 26, 2006 V6.00 New version 6 started
Jul 19, 2008 V6.00 F.Piette made some changes for Unicode
Dec 22, 2008 V6.01 F.Piette added a few explicit casts to avoid warning when
                   compiling with D2009.
Jul 4, 2012  V8.00 Angus changed to Goggle DNS 8.8.8.8 and embarcadero.com
Apr 22 2019  V8.61 Angus major rewrite to support all important DNS queries
                     and also new Query All for seven most common queries.
                   Note new queries return results in single array rather
                     than multiple arrays, but those old arrays are still
                     available for backward compatibility.
                   Added list of public DNS servers.
May 04 2020 V8.64  Added support for International Domain Names for Applications (IDNA),
                   All Unicode queries are converted to Punycode ASCII, and responses
                     with ACE zn-- prefix are converted back to Unicode.
May 25 2020 V8.65 Handle NS and CNAME responses as host name with compressed
                     data correctly.
                   Dump question and buffer for all requests, note all requests
                      only shows last query. 


Note - OverbyteIcsHttpRest contains a derived component DnsQueryHttps which makes
DNS over HTTPS requests per RFC8484, illustrated in the OverbyteIcsHttpRest sample
which displays results in a more friendly grid.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsNsLookup1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsIniFiles, StdCtrls, ExtCtrls, Buttons,
  OverbyteIcsWinSock, OverbyteIcsWSocket, OverbyteIcsDnsQuery;

const
  NsLookVersion      = 865;
  CopyRight : String = ' NsLookup (c) 1999-2020 F. Piette V8.65 ';

type
  TNsLookupForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    DnsQuery1: TDnsQuery;
    ClearDisplayBitBtn: TBitBtn;
    LookupButton: TButton;
    TcpRadioButton: TRadioButton;
    UdpRadioButton: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    DnsEdit: TComboBox;
    Label3: TLabel;
    NameEdit: TComboBox;
    DnsQueryType: TComboBox;
    AllButton: TButton;
    Timer1: TTimer;
    AbortButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DnsQuery1RequestDone(Sender: TObject; Error: Word);
    procedure ClearDisplayBitBtnClick(Sender: TObject);
    procedure LookupButtonClick(Sender: TObject);
    procedure AllButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure AbortButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FRequestID   : Integer;
    procedure Display(Msg : String);
    procedure DumpDnsResponse;
  public
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  NsLookupForm: TNsLookupForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Window';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyName            = 'Name';
    KeyDns             = 'Dns';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.FormCreate(Sender: TObject);
var
    I: Integer;
begin
    FIniFileName := GetIcsIniFileName;

// fill Dns Query drop down
    DnsQueryType.Items.Clear;
    for I := Low(DnsReqTable) to High(DnsReqTable) do
         DnsQueryType.Items.Add (DnsReqTable[I].Asc +
                                         ' [' + DnsReqTable[I].Desc + ']');
    DnsQueryType.ItemIndex := 0;

 // fill DNS server drop down
    DnsEdit.Items.Clear;
    for I := Low(DnsPublicServerTable) to High(DnsPublicServerTable) do
         DnsEdit.Items.Add (DnsPublicServerTable[I]);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile       := TIcsIniFile.Create(FIniFileName);
        Width         := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height        := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top           := IniFile.ReadInteger(SectionWindow, KeyTop,
                                             (Screen.Height - Height) div 2);
        Left          := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                             (Screen.Width  - Width)  div 2);
        NameEdit.Text := IniFile.ReadString(SectionData, KeyName, 'embarcadero.com');
        DnsEdit.Text  := IniFile.ReadString(SectionData, KeyDns, DnsPublicServerTable[0]);
        DisplayMemo.Clear;
        Display(Trim(CopyRight));
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData, KeyName, NameEdit.Text);
    IniFile.WriteString(SectionData, KeyDns,  DnsEdit.Text);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.Display(Msg : String);
begin
    if DisplayMemo.Lines.Count > 200 then
        DisplayMemo.Lines.Delete(0);
    DisplayMemo.Lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.DumpDnsResponse;
var
    P      : PAnsiChar;
    I      : Integer;
    Len    : Integer;
    Buf    : String;
begin
    Display('Response dump (' + IntToStr(DnsQuery1.ResponseLen) + ' bytes):');
    P   := DnsQuery1.ResponseBuf;
    Len := DnsQuery1.ResponseLen;
    Buf := '';
    I   := 0;
    while I < Len do begin
        if P^ in [' '..'~'] then
            Buf := Buf + Char(P^)
        else
            Buf := Buf + '<' + IntToStr(Ord(P^)) + '>';
        Inc(I);
        Inc(P);
        if (I mod 16) = 0 then begin
            Display('  ' + Buf);
            Buf := '';
        end;
    end;
    if Length(Buf) > 0 then
        Display('  ' + Buf);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*  backward compatible response handling, only A, PTR and MX requests
procedure TNsLookupForm.DnsQuery1RequestDone(Sender: TObject; Error: Word);
var
    I      : Integer;
    nIndex : Integer;
begin
    if Error <> 0 then begin
        Display('Error #' + IntToStr(Error));
        Exit;
    end;
    Display('ID                 : ' + IntToStr(DnsQuery1.ResponseID));
    Display('ResponseCode       : ' + IntToStr(DnsQuery1.ResponseCode));
    Display('OpCode             : ' + IntToStr(DnsQuery1.ResponseOpCode));
    Display('Authoritative      : ' + IntToStr(Ord(DnsQuery1.ResponseAuthoritative)));
    Display('Truncation         : ' + IntToStr(Ord(DnsQuery1.ResponseTruncation)));
    Display('RecursionAvailable : ' + IntToStr(Ord(DnsQuery1.ResponseRecursionAvailable)));
    Display('QDCount            : ' + IntToStr(DnsQuery1.ResponseQDCount));
    Display('ANCount            : ' + IntToStr(DnsQuery1.ResponseANCount));
    Display('NSCount            : ' + IntToStr(DnsQuery1.ResponseNSCount));
    Display('ARCount            : ' + IntToStr(DnsQuery1.ResponseARCount));
    Display('ResponseLen        : ' + IntToStr(DnsQuery1.ResponseLen));
    Display('QuestionName       : ' + String(DnsQuery1.QuestionName));
    Display('QuestionType       : ' + IntToStr(DnsQuery1.QuestionType));
    Display('QuestionClass      : ' + IntToStr(DnsQuery1.QuestionClass));

    for I := 0 to DnsQuery1.ResponseANCount - 1 do begin
        Display('Answer #' + IntToStr(I + 1));
        Display('  AnswerName       : ' + String(DnsQuery1.AnswerName[I]));
        Display('  AnswerType       : ' + IntToStr(DnsQuery1.AnswerType[I]));
        Display('  AnswerClass      : ' + IntToStr(DnsQuery1.AnswerClass[I]));
        Display('  AnswerTTL        : ' + IntToStr(DnsQuery1.AnswerTTL[I]));
        nIndex := DnsQuery1.AnswerTag[I];
        if nIndex >= 0 then begin
            case DnsQuery1.AnswerType[I] of
            DnsQueryMX:
                begin
                    Display('  MXPreference     : ' + IntToStr(DnsQuery1.MXPreference[nIndex]));
                    Display('  MXExchange       : ' + String(DnsQuery1.MXExchange[nIndex]));
                end;
            DnsQueryA:
                begin
                    Display('  Address          : ' +
                            String(WSocket_inet_ntoa(DnsQuery1.Address[nIndex])));
                end;
            DnsQueryPTR:
                begin
                    Display('  Hostname         : ' +
                            String(DnsQuery1.Hostname[nIndex]));
                end;
            end;
        end;
    end;
    { Dump complete response }
    DumpDnsResponse;
end;   *)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.DnsQuery1RequestDone(Sender: TObject; Error: Word);
var
    I      : Integer;
begin
    Timer1.Enabled := False;
    AllButton.Enabled := True;
    LookupButton.Enabled := True;
    if Error <> 0 then begin
        Display('Error #' + IntToStr(Error));
        Exit;
    end;

  // only show question for a single query, not ALL
 //   if DnsQuery1.AnswerTotal = DnsQuery1.ResponseANCount then begin
        Display('ID                 : ' + IntToStr(DnsQuery1.ResponseID));
        Display('ResponseCode       : ' + IntToStr(DnsQuery1.ResponseCode) + ' ' +
                                                DnsRCodeTable[DnsQuery1.ResponseCode]);
        Display('OpCode             : ' + IntToStr(DnsQuery1.ResponseOpCode));
        Display('Authoritative      : ' + IntToStr(Ord(DnsQuery1.ResponseAuthoritative)));
        Display('Truncation         : ' + IntToStr(Ord(DnsQuery1.ResponseTruncation)));
        Display('RecursionAvailable : ' + IntToStr(Ord(DnsQuery1.ResponseRecursionAvailable)));
        Display('QDCount            : ' + IntToStr(DnsQuery1.ResponseQDCount));
        Display('ANCount            : ' + IntToStr(DnsQuery1.ResponseANCount));
        Display('NSCount            : ' + IntToStr(DnsQuery1.ResponseNSCount));
        Display('ARCount            : ' + IntToStr(DnsQuery1.ResponseARCount));
        Display('ResponseLen        : ' + IntToStr(DnsQuery1.ResponseLen));
        Display('QuestionName       : ' + String(DnsQuery1.QuestionName));
        Display('QuestionType       : ' + IntToStr(DnsQuery1.QuestionType) + ' ' +
                                             FindDnsReqTypeName(DnsQuery1.QuestionType));
        Display('QuestionClass      : ' + IntToStr(DnsQuery1.QuestionClass));
//    end;
    if DnsQuery1.AnswerTotal > 0 then begin
        for I := 0 to DnsQuery1.AnswerTotal - 1 do begin
            Display('Answer #' + IntToStr(I + 1));
            with  DnsQuery1.AnswerRecord[I] do begin
                Display('  AnswerName       : ' + AnswerName + '  (' + String(RRName) + ')');    { V8.64 }
                Display('  AnswerType       : ' + IntToStr(RRType) + ' ' + FindDnsReqTypeName(RRType));
                Display('  AnswerClass      : ' + IntToStr(RRClass));
                Display('  AnswerTTL        : ' + IntToStr(TTL));
                Display('  Result Length    : ' + IntToStr(RDLength));   { V8.64 }

                case RRType of
                DnsQueryMX:
                    begin
                        Display('  MXPreference     : ' + IntToStr(MxPref));
                        Display('  MXExchange       : ' + HostName);     { V8.64 }
                    end;
                DnsQueryA, DnsQueryAAAA:
                    begin
                        Display('  Address          : ' + String(RDData));
                    end;
                DnsQueryPTR, DnsQueryNS, DnsQueryCNAME:                 { V8.65 }
                    begin
                        Display('  Hostname         : ' + HostName);    { V8.64 }
                    end;
                else begin
                        Display('  Result           : ' + String(RDData));
                     end;
                end;
            end;
        end;
    end;

   { Dump complete response }
   DumpDnsResponse;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.AbortButtonClick(Sender: TObject);
begin
    if (NOT AllButton.Enabled) or (NOT AllButton.Enabled) then
        DnsQuery1.AbortQuery;
end;

procedure TNsLookupForm.AllButtonClick(Sender: TObject);
begin
    AllButton.Enabled := False;
    LookupButton.Enabled := False;
    if UdpRadioButton.Checked then
        DnsQuery1.Proto := 'udp'
    else
        DnsQuery1.Proto := 'tcp';
    DnsQuery1.Addr := DnsEdit.Text;
    Timer1.Enabled := True;
    FRequestID := DnsQuery1.QueryAll(Trim(NameEdit.Text));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.ClearDisplayBitBtnClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.LookupButtonClick(Sender: TObject);
begin
    AllButton.Enabled := False;
    LookupButton.Enabled := False;
    if UdpRadioButton.Checked then
        DnsQuery1.Proto := 'udp'
    else
        DnsQuery1.Proto := 'tcp';
    DnsQuery1.Addr := DnsEdit.Text;
    Timer1.Enabled := True;
    FRequestID := DnsQuery1.QueryAny(Trim(NameEdit.Text),
                                    DnsReqTable[DnsQueryType.ItemIndex].Num);
    Display('Request ID         : ' + IntToStr(FRequestID));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.Timer1Timer(Sender: TObject);
begin
    Timer1.Enabled := False;
    DnsQuery1.AbortQuery;
 //   DnsQuery1RequestDone(Self, 999);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
