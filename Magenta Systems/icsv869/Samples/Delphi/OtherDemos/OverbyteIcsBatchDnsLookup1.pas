unit OverbyteIcsBatchDnsLookup1;
{
Program:      NsLookup
Description:  ICS batch async DNS lookup DnsLookup (IPv6 and IPv4)
Author:       François Piette
Creation:      ?
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

Mar  7, 2017  V8.43  Added Use Thread tick box so wsockets uses thread for
                    all DNS lookups instead of just IPv4
Apr 15, 2017  V8.44 FPiette removed compiler warnings for D10.2
Mar 10, 2020  V8.64 Added support for International Domain Names for Applications
                     (IDNA), i.e. using accents and unicode characters in domain names.
                    The IDN button encodes the IDN list into Punycode ASCII and
                      then back to Unicode again.  If built on a pre-Unicode compiler,
                      some domains will appear as ???.
                    When doing DNS Lookups, the actual Punycode ASCII domain looked-up
                      is shown.
                    A blank IDN list on startup loads default list.

}

interface

{$WARN SYMBOL_PLATFORM OFF}
{$I Include\OverbyteIcsDefs.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Contnrs,
{$IFDEF DELPHI24_UP}
  UITypes,
{$ENDIF}
  OverbyteIcsUtils,
  OverbyteIcsIniFiles,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket;

type
  TBatchDnsLookupForm = class(TForm)
    StartButton: TButton;
    DnsNamesMemo: TMemo;
    ResultMemo: TMemo;
    MinEdit: TEdit;
    Label1: TLabel;
    MaxEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    InstancesEdit: TEdit;
    SocketFamilyComboBox: TComboBox;
    Label5: TLabel;
    Label6: TLabel;
    UseThread: TCheckBox;
    IDNMemo: TMemo;
    doIDNEncode: TButton;
    DecodeMemo: TMemo;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure StartButtonClick(Sender: TObject);
    procedure WSocket1DnsLookupDone(Sender: TObject; ErrCode: Word);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SocketFamilyComboBoxChange(Sender: TObject);
    procedure doIDNEncodeClick(Sender: TObject);
    procedure DnsNamesMemoDblClick(Sender: TObject);
  private
    FDummyWSocket: TWSocket;
    FHostList: TStringList;
    FResultList: TStringList;
    FMin : Byte;
    FMax : Byte;
    FInstances : Byte;
    FIniFile : TIcsIniFile;
    FInitialized : Boolean;
    FWSocketList: TComponentList;
    procedure WmUser(var AMsg: TMessage); message WM_USER;
  public
    property IniFile: TIcsIniFile read FIniFile;
  end;

var
  BatchDnsLookupForm: TBatchDnsLookupForm;

implementation

{$R *.dfm}
uses
    Math;

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    //KeyWidth           = 'Width';
    //KeyHeight          = 'Height';
    SectionSetup       = 'Setup';
    KeyMin             = 'MinLookupThreads';
    KeyMax             = 'MaxLookupThreads';
    KeyInstances       = 'NumberOfInstances';
    SectionDnsNames    = 'DnsNames';
    KeyDnsName         = 'Item';
    KeyUseThread       = 'UseThread';

procedure TBatchDnsLookupForm.FormCreate(Sender: TObject);
begin
{$IF RTLVersion >= 18}
    { Built-in memory leak detection and display since BDS2006 }
    { This is useful for debugging, however a bit slower.      }
    ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$IFEND}
    IcsNameThreadForDebugging('Main');
    FHostList := TStringList.Create;
    FResultList := TStringList.Create;
    FWSocketList := TComponentList.Create(TRUE);
    ResultMemo.Clear;
    DnsNamesMemo.WordWrap := FALSE;
    FIniFile := TIcsIniFile.Create(OverbyteIcsIniFiles.GetIcsIniFileName);
    ActiveControl := StartButton;
end;

procedure TBatchDnsLookupForm.FormDestroy(Sender: TObject);
begin
    FWSocketList.Free;
    FHostList.Free;
    FResultList.Free;
    FIniFile.Free;
end;

procedure TBatchDnsLookupForm.FormShow(Sender: TObject);
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        //Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        //Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                               (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                               (Screen.Width  - Width)  div 2);
        FMin         := IniFile.ReadInteger(SectionSetup, KeyMin,  1);
        FMax         := IniFile.ReadInteger(SectionSetup, KeyMax,  4);
        FInstances   := IniFile.ReadInteger(SectionSetup, KeyInstances, 4);
        UseThread.Checked :=  IniFile.ReadBool(SectionSetup, KeyUseThread, False);
        IniFile.ReadStrings(SectionDnsNames, KeyDnsName, DnsNamesMemo.Lines);
        if DnsNamesMemo.Lines.Count = 0 then  { V8.64 }
        begin
            DnsNamesMemo.Text :=
            'www.overbyte.be'#13#10 +
            'svn.overbyte.be'#13#10 +
            'wiki.overbyte.be'#13#10 +
            'ipv4.magsys.co.uk'#13#10 +
            'ipv6.magsys.co.uk'#13#10 +
            'www.embarcardero.com'#13#10 +
            'edn.embarcardero.com'#13#10 +
            'nonexisting'#13#10 +
            'www.microsoft.com'#13#10 +
            'ipv6.google.com'#13#10 +
            'www.goggle.com'#13#10 +
            'sourceforge.net'#13#10 +
            'msdn.microsoft.com'#13#10 +
            'localhost'#13#10 +
            '127.0.0.1'#13#10 +
            '::1'#13#10 +
            'strøm.no'#13#10 +
            'www.mâgsÿstést.eu'#13#10 +
            'www.háčkyčárky.cz'#13#10 +         { needs Unicode }
            'stránky.háčkyčárky.cz'#13#10 +     { needs Unicode }
            'мособлеирц.рф'#13#10 +             { needs Unicode }
            '例子.测试'#13#10 +                 { needs Unicode, lookup fails }
            'scrúdú.mâgsÿstést.eu'#13#10 +
            'prøve.mâgsÿstést.eu'#13#10 +
            'тест.mâgsÿstést.eu'#13#10 +        { needs Unicode }
            'Ölçek.mâgsÿstést.eu'#13#10 +
            '测试.mâgsÿstést.eu'#13#10 +        { needs Unicode }
            'δοκιμή.mâgsÿstést.eu'#13#10 +      { needs Unicode }
            'тестовоезадание.mâgsÿstést.eu'#13#10 +   { needs Unicode }
            'próf.mâgsÿstést.eu'#13#10 +
            'テスト.mâgsÿstést.eu'#13#10 +      { needs Unicode }
            '테스트.mâgsÿstést.eu'#13#10 +      { needs Unicode }
            'www.bad(name).com'#13#10 +
            'www.-badname.com'#13#10;
        end;
        MinEdit.Text := IntToStr(FMin);
        MaxEdit.Text := IntToStr(FMax);
        InstancesEdit.Text := IntToStr(FInstances);

        { This instance is to keep a reference to a shared internal DnsLookup   }
        { object so that DNS lookup threads don't terminate when the last       }
        { TWSocket instance is destroyed.                                       }
        FDummyWSocket := TWSocket.Create(Self);
        { In order to get a reference we just set the minimum and maximum lookup}
        { threads shared by all TWsocket instances in this thread context.      }
        FDummyWSocket.SetMinMaxIcsAsyncDnsLookupThreads(FMin, FMax);
        { We can also call method AllocateIcsAsyncDnsLookup to get a reference. }
    end;
end;

procedure TBatchDnsLookupForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    try
        IniFile.WriteInteger(SectionWindow,   KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow,   KeyLeft,        Left);
        //IniFile.WriteInteger(SectionWindow,   KeyWidth,       Width);
        //IniFile.WriteInteger(SectionWindow,   KeyHeight,      Height);

        FMin        := StrToIntDef(MinEdit.Text, 0);
        FMax        := StrToIntDef(MaxEdit.Text, 2);
        FInstances  := StrToIntDef(InstancesEdit.Text, 3);

        IniFile.WriteInteger(SectionSetup,    KeyMin,         FMin);
        IniFile.WriteInteger(SectionSetup,    KeyMax,         FMax);
        IniFile.WriteInteger(SectionSetup,    KeyInstances,   FInstances);
        IniFile.WriteBool(SectionSetup,       KeyUseThread,   UseThread.Checked);
         IniFile.WriteStrings(SectionDnsNames, KeyDnsName,    DnsNamesMemo.Lines);
       IniFile.UpdateFile;
    except
        on E: Exception do
            MessageDlg(E.ClassName + ' ' + E.Message, mtError, [mbOK], 0);
    end;
end;

procedure TBatchDnsLookupForm.SocketFamilyComboBoxChange(Sender: TObject);
begin
    if TSocketFamily(SocketFamilyComboBox.ItemIndex) = sfIPv4 then
    begin
        MinEdit.Enabled := FALSE;
        MinEdit.Color   := clBtnFace;
        MaxEdit.Enabled := FALSE;
        MaxEdit.Color   := clBtnFace;
        Label1.Enabled  := FALSE;
        Label2.Enabled  := FALSE;
        Label3.Enabled  := FALSE;
        Label6.Enabled  := FALSE;
    end
    else begin
        MinEdit.Enabled := TRUE;
        MinEdit.Color   := clWindow;
        MaxEdit.Enabled := TRUE;
        MaxEdit.Color   := clWindow;
        Label1.Enabled  := TRUE;
        Label2.Enabled  := TRUE;
        Label3.Enabled  := TRUE;
        Label6.Enabled  := TRUE;
    end;
end;


procedure TBatchDnsLookupForm.WmUser(var AMsg: TMessage);
var
    WSocket : TWSocket;
begin
    WSocket := TWSocket(AMsg.WParam);
    if FHostList.Count > 0 then
    begin
        WSocket.Tag := FHostList.Count - 1;
        try
            WSocket.DnsLookup(FHostList[WSocket.Tag]);
        except
            FResultList[WSocket.Tag] := IcsGetExceptMess(ExceptObject);
        end;
        FHostList.Delete(WSocket.Tag);
    end
    else
        WSocket.Free;
end;

procedure TBatchDnsLookupForm.WSocket1DnsLookupDone(
    Sender  : TObject;
    ErrCode : Word);
var
    Row: Integer;
begin
    Row := TWSocket(Sender).Tag;
    if ErrCode = 0 then
        FResultList[Row] := TWSocket(Sender).DnsResult
    else
        FResultList[Row] := WSocketErrorDesc(ErrCode);
    ResultMemo.Lines.Assign(FResultList);
    ResultMemo.Update;
    IDNMemo.Lines[Row] := String(TWSocket(Sender).PunycodeHost);
    IDNMemo.Update;
    PostMessage(Handle, WM_USER, WPARAM(Sender), 0);
end;

procedure TBatchDnsLookupForm.StartButtonClick(Sender: TObject);
var
    I       : Integer;
    WSocket : TWSocket;
    LMin, LMax : Byte;
begin
    FWSocketList.Clear;
    IDNMemo.Clear;
    DecodeMemo.Clear;
    for I := 0 to DnsNamesMemo.Lines.Count - 1 do
        IDNMemo.Lines.Add('line');
    ResultMemo.Clear;
    ResultMemo.Update;
    IDNMemo.Update;

    LMin := StrToIntDef(MinEdit.Text, 0);
    LMax := StrToIntDef(MaxEdit.Text, 1);
    if (LMin <> FMin) or
       (LMax <> FMax) then
    begin
        FMin := LMin;
        FMax := LMax;
        FDummyWSocket.SetMinMaxIcsAsyncDnsLookupThreads(FMin, FMax);
    end;
    if StrToIntDef(InstancesEdit.Text, 1) <> FInstances then
        FInstances := StrToIntDef(InstancesEdit.Text, 1);

    FHostList.Assign(DnsNamesMemo.Lines);
    FResultList.Clear;
    for I := 0 to FHostList.Count -1 do
        FResultList.Add('Resolving..');

    for I := Min(FInstances, FHostList.Count - 1) downto 1 do begin
        WSocket := TWSocket.Create(nil);
        FWSocketList.Add(WSocket);
        WSocket.Tag := FHostList.Count -1;
        { Note DnsLookup uses the old API if SocketFamily is sfIPv4 unless wsoIcsDnsLookup is set  }
        { V8.43 see if using thread for IPv4 lookups }
        if UseThread.Checked then
            WSocket.ComponentOptions := WSocket.ComponentOptions + [wsoIcsDnsLookup];
        WSocket.SocketFamily := TSocketFamily(SocketFamilyComboBox.ItemIndex);
        WSocket.OnDnsLookupDone := WSocket1DnsLookupDone;
        try
            WSocket.DnsLookup(FHostList[WSocket.Tag]);
        except
            FResultList[WSocket.Tag] := IcsGetExceptMess(ExceptObject);
        end;
        FHostList.Delete(WSocket.Tag);
    end;
end;

procedure TBatchDnsLookupForm.DnsNamesMemoDblClick(Sender: TObject);
begin
    DnsNamesMemo.Lines.Clear;
end;

procedure TBatchDnsLookupForm.doIDNEncodeClick(Sender: TObject);
var
    I: Integer;
    PunyStr, UniStr: String;
    ErrFlag: Boolean;
begin
    IDNMemo.Clear;
    DecodeMemo.Clear;
    for I := 0 to DnsNamesMemo.Lines.Count -1 do begin
        UniStr := '';
        PunyStr := IcsIDNAToASCII(DnsNamesMemo.Lines[I], True, ErrFlag);
        if ErrFlag then
            PunyStr := 'IDNAToAscii Failed'
        else begin
            UniStr := IcsIDNAToUnicode(PunyStr, ErrFlag);
            if ErrFlag then
                UniStr := 'IDNAToUnicode Failed'
        end;
        IDNMemo.Lines.Add(PunyStr);
        DecodeMemo.Lines.Add(UniStr);
    end;
end;


end.
