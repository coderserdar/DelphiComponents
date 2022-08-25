unit IcsBatchDnsLookup1;
{
May 2012 - V8.00 - Arno converted demo for FireMonkey cross platform Mac
                   OS X support, now XE2 and later only uising FMX components
Dec 09, 2020 V8.65 Added DEFINE FMX.
                   Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
Apr 6, 2021  V8.67 Made Win64 compatible by correcting Integer(Pointer)
                         typecasts to W/LPARAM for PostMessage, thanks to Fr0sT.
Oct 14, 2021 V8.68 Fixed last fix.
}

interface

{$I Include\OverbyteIcsDefs.inc}
{$DEFINE FMX}
{$IF CompilerVersion < 23}
  {$MESSAGE FATAL 'This project requires Delphi or RAD Studio XE2 or better'};
{$IFEND}

{$WARN SYMBOL_PLATFORM OFF}

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
  WinApi.Messages,
{$ENDIF}
{$IFDEF POSIX}
  Ics.Posix.WinTypes,
  Ics.Posix.PXMessages,
{$ENDIF}
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Contnrs,
  System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Layouts, FMX.Memo, FMX.Edit, FMX.ListBox, FMX.TabControl,
  FMX.StdCtrls,
  OverbyteIcsUtils,
  OverbyteIcsIniFiles,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket, FMX.Memo.Types, FMX.ScrollBox, FMX.Controls.Presentation;

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
    procedure StartButtonClick(Sender: TObject);
    procedure WSocket1DnsLookupDone(Sender: TObject; ErrCode: Word);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SocketFamilyComboBoxChange(Sender: TObject);
  private
    FDummyWSocket: TWSocket;
    FHostList: TStringList;
    FResultList: TStringList;
    FMin : Byte;
    FMax : Byte;
    FNumOfInstances : Byte;
    FIniFile : TIcsIniFile;
    FInitialized : Boolean;
    FWSocketList: TComponentList;
{$IFDEF POSIX}
    { This is only required if we want to post/send custom messages as in Windows }
    FMessagePump               : TIcsMessagePump;
{$ENDIF}
    FNotifyWindow              : HWND;
    procedure WndProc(var AMsg: TMessage);

  public
    property IniFile: TIcsIniFile read FIniFile;
  end;

var
  BatchDnsLookupForm: TBatchDnsLookupForm;

implementation

{$R *.fmx}

uses
    Math,
    DemoUtils;

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    SectionSetup       = 'Setup';
    KeyMin             = 'MinLookupThreads';
    KeyMax             = 'MaxLookupThreads';
    KeyInstances       = 'NumberOfInstances';
    SectionDnsNames    = 'DnsNames';
    KeyDnsName         = 'Item';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBatchDnsLookupForm.WndProc(var AMsg: TMessage);
var
    WSocket : TWSocket;
begin
    try
        if AMsg.Msg = WM_USER then
        begin
            WSocket := TWSocket(AMsg.WParam);
            if FHostList.Count > 0 then
            begin
                WSocket.Tag := FHostList.Count - 1;
                WSocket.DnsLookup(FHostList[WSocket.Tag]);
                FHostList.Delete(WSocket.Tag);
            end
            else
                WSocket.Free;
        end
        else
            AMsg.Result := DefWindowProc(FNotifyWindow, AMsg.Msg, AMsg.WParam, AMsg.LParam);
    except
        Application.HandleException(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TBatchDnsLookupForm.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
    ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$ENDIF}
    IcsNameThreadForDebugging('Main');
{$IFDEF POSIX}
    FMessagePump  := TIcsMessagePump.Create;
{$ENDIF}
    FNotifyWindow := AllocateHWND(WndProc);
    FHostList := TStringList.Create;
    FResultList := TStringList.Create;
    FResultList.LineBreak := '#13#10';
    FWSocketList := TComponentList.Create(TRUE);
    ResultMemo.Lines.Clear;
    DnsNamesMemo.Lines.LineBreak := '#13#10';
    DnsNamesMemo.WordWrap := FALSE;
    FIniFile := TIcsIniFile.Create(OverbyteIcsIniFiles.GetIcsIniFileName);
    ActiveControl := StartButton;
    FormShow(Self);
end;

procedure TBatchDnsLookupForm.FormDestroy(Sender: TObject);
begin
    FWSocketList.Free;
    FHostList.Free;
    FResultList.Free;
    FIniFile.Free;
{$IFDEF POSIX}
    FMessagePump.Free;
{$ENDIF}
end;

procedure TBatchDnsLookupForm.FormShow(Sender: TObject);
begin
    if not FInitialized then begin
        FInitialized    := TRUE;
        Top             := IniFile.ReadInteger(SectionWindow, KeyTop,
                                              (ScreenHeight - Height) div 2);
        Left            := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                              (ScreenWidth  - Width)  div 2);
        FMin            := IniFile.ReadInteger(SectionSetup, KeyMin,  1);
        FMax            := IniFile.ReadInteger(SectionSetup, KeyMax,  4);
        FNumOfInstances := IniFile.ReadInteger(SectionSetup, KeyInstances, 4);
        if not IniFile.ReadStrings(SectionDnsNames, KeyDnsName, DnsNamesMemo.Lines) then
        begin
            DnsNamesMemo.Text :=
            'www.overbyte.be'#13#10 +
            'svn.overbyte.be'#13#10 +
            'wiki.overbyte.be'#13#10 +
            'www.embarcardero.com'#13#10 +
            'edn.embarcardero.com'#13#10 +
            'nonexisting'#13#10 +
            'www.microsoft.com'#13#10 +
            'ipv6.google.com'#13#10 +
            'www.goggle.com'#13#10 +
            'www.aol.com'#13#10 +
            'sourceforge.net'#13#10 +
            'www.borland.com'#13#10 +
            'msdn.microsoft.com'#13#10 +
            'localhost'#13#10 +
            '127.0.0.1'#13#10 +
            '::1'#13#10 +
            'www.sixxs.net';
        end;
        MinEdit.Text := IntToStr(FMin);
        MaxEdit.Text := IntToStr(FMax);
        InstancesEdit.Text := IntToStr(FNumOfInstances);

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

        FMin            := StrToIntDef(MinEdit.Text, 1);
        FMax            := StrToIntDef(MaxEdit.Text, 4);
        FNumOfInstances := StrToIntDef(InstancesEdit.Text, 3);

        IniFile.WriteInteger(SectionSetup,    KeyMin,         FMin);
        IniFile.WriteInteger(SectionSetup,    KeyMax,         FMax);
        IniFile.WriteInteger(SectionSetup,    KeyInstances,   FNumOfInstances);
        IniFile.WriteStrings(SectionDnsNames, KeyDnsName,     DnsNamesMemo.Lines);
        IniFile.UpdateFile;
    except
        on E: Exception do
            MessageDlg(E.ClassName + ' ' + E.Message, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end;
end;

procedure TBatchDnsLookupForm.SocketFamilyComboBoxChange(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    { Old API is only used in Windows }
    if TSocketFamily(SocketFamilyComboBox.ItemIndex) = sfIPv4 then
    begin
        MinEdit.Enabled := FALSE;
        MaxEdit.Enabled := FALSE;
        Label1.Enabled  := FALSE;
        Label2.Enabled  := FALSE;
        Label3.Enabled  := FALSE;
        Label6.Enabled  := FALSE;
    end
    else begin
        MinEdit.Enabled := TRUE;
        MaxEdit.Enabled := TRUE;
        Label1.Enabled  := TRUE;
        Label2.Enabled  := TRUE;
        Label3.Enabled  := TRUE;
        Label6.Enabled  := TRUE;
    end;
  {$ENDIF}
end;

procedure TBatchDnsLookupForm.WSocket1DnsLookupDone(
    Sender  : TObject;
    ErrCode : Word);
begin
    if ErrCode = 0 then
        FResultList[TWSocket(Sender).Tag] := TWSocket(Sender).DnsResult
    else
        FResultList[TWSocket(Sender).Tag] := WSocketErrorDesc(ErrCode);
    ResultMemo.Lines.Assign(FResultList);
    PostMessage(FNotifyWindow, WM_USER, WPARAM(Sender), 0);
end;

procedure TBatchDnsLookupForm.StartButtonClick(Sender: TObject);
var
    I       : Integer;
    WSocket : TWSocket;
    LMin, LMax : Byte;
begin
    FWSocketList.Clear;
    ResultMemo.Lines.Clear;

    LMin := StrToIntDef(MinEdit.Text, 0);
    LMax := StrToIntDef(MaxEdit.Text, 1);
    if (LMin <> FMin) or
       (LMax <> FMax) then
    begin
        FMin := LMin;
        FMax := LMax;
        FDummyWSocket.SetMinMaxIcsAsyncDnsLookupThreads(FMin, FMax);
    end;
    if StrToIntDef(InstancesEdit.Text, 1) <> FNumOfInstances then
        FNumOfInstances := StrToIntDef(InstancesEdit.Text, 1);

    FHostList.Assign(DnsNamesMemo.Lines);
    FResultList.Clear;
    for I := 0 to FHostList.Count -1 do
        FResultList.Add('Resolving..');

    for I := Min(FNumOfInstances, FHostList.Count - 1) downto 1 do begin
        WSocket := TWSocket.Create(nil);
        FWSocketList.Add(WSocket);
        WSocket.Tag := FHostList.Count -1;
        { Note DnsLookup uses the old API in Windows if SocketFamily is sfIPv4 }
        WSocket.SocketFamily := TSocketFamily(SocketFamilyComboBox.ItemIndex);
        WSocket.OnDnsLookupDone := WSocket1DnsLookupDone;
        WSocket.DnsLookup(FHostList[WSocket.Tag]);
        FHostList.Delete(WSocket.Tag);
    end;
end;

end.
