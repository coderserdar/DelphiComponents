{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      NsLookup
Description:  Demo for DnsQuery ICS component.
Author:       François Piette
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
Creation:     January 29, 1999
Version:      1.02
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1999 by François PIETTE
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

History:
Feb 27, 1999 V1.01 Added PTR lookup (reverse DNS lookup)
Mar 07, 1999 V1.02 Adapted for Delphi 1

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit NsLook1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, WinSock, DnsQuery, StdCtrls, ExtCtrls, Buttons;

type
  TNsLookupForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    DnsEdit: TEdit;
    NameEdit: TEdit;
    MXLookupButton: TButton;
    DnsQuery1: TDnsQuery;
    ClearDisplayBitBtn: TBitBtn;
    ALookupButton: TButton;
    PTRLookupButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MXLookupButtonClick(Sender: TObject);
    procedure DnsQuery1RequestDone(Sender: TObject; Error: Word);
    procedure ClearDisplayBitBtnClick(Sender: TObject);
    procedure ALookupButtonClick(Sender: TObject);
    procedure PTRLookupButtonClick(Sender: TObject);
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
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile       := TIniFile.Create(FIniFileName);
        Width         := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height        := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top           := IniFile.ReadInteger(SectionWindow, KeyTop,
                                             (Screen.Height - Height) div 2);
        Left          := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                             (Screen.Width  - Width)  div 2);
        NameEdit.Text := IniFile.ReadString(SectionData, KeyName, 'inprise.com');
        DnsEdit.Text  := IniFile.ReadString(SectionData, KeyDns,  '193.121.171.135');
        DisplayMemo.Clear;
        IniFile.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData, KeyName, NameEdit.Text);
    IniFile.WriteString(SectionData, KeyDns,  DnsEdit.Text);
    IniFile.Destroy;
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
    P      : PChar;
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
            Buf := Buf + P^
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
    Display('QuestionName       : ' + DnsQuery1.QuestionName);
    Display('QuestionType       : ' + IntToStr(DnsQuery1.QuestionType));
    Display('QuestionClass      : ' + IntToStr(DnsQuery1.QuestionClass));

    for I := 0 to DnsQuery1.ResponseANCount - 1 do begin
        Display('Answer #' + IntToStr(I + 1));
        Display('  AnswerName       : ' + DnsQuery1.AnswerName[I]);
        Display('  AnswerType       : ' + IntToStr(DnsQuery1.AnswerType[I]));
        Display('  AnswerClass      : ' + IntToStr(DnsQuery1.AnswerClass[I]));
        Display('  AnswerTTL        : ' + IntToStr(DnsQuery1.AnswerTTL[I]));
        nIndex := DnsQuery1.AnswerTag[I];
        if nIndex >= 0 then begin
            case DnsQuery1.AnswerType[I] of
            DnsQueryMX:
                begin
                    Display('  MXPreference     : ' + IntToStr(DnsQuery1.MXPreference[nIndex]));
                    Display('  MXExchange       : ' + DnsQuery1.MXExchange[nIndex]);
                end;
            DnsQueryA:
                begin
                    Display('  Address          : ' + StrPas(inet_ntoa(DnsQuery1.Address[nIndex])));
                end;
            DnsQueryPTR:
                begin
                    Display('  Hostname         : ' + DnsQuery1.Hostname[nIndex]);
                end;
            end;
        end;
    end;
    { Dump complete response }
    DumpDnsResponse;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.ClearDisplayBitBtnClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.MXLookupButtonClick(Sender: TObject);
begin
    DnsQuery1.Addr := DnsEdit.Text;
    FRequestID     := DnsQuery1.MXLookup(NameEdit.Text);
    Display('Request ID         : ' + IntToStr(FRequestID));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.ALookupButtonClick(Sender: TObject);
begin
    DnsQuery1.Addr := DnsEdit.Text;
    FRequestID     := DnsQuery1.ALookup(NameEdit.Text);
    Display('Request ID         : ' + IntToStr(FRequestID));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNsLookupForm.PTRLookupButtonClick(Sender: TObject);
begin
    DnsQuery1.Addr := DnsEdit.Text;
    FRequestID     := DnsQuery1.PTRLookup(NameEdit.Text);
    Display('Request ID         : ' + IntToStr(FRequestID));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
