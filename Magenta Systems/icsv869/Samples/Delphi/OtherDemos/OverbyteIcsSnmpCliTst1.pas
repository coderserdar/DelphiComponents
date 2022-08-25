{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     May 11, 2011
Description:  Demo for SNMP (simple network management protocol) component 
Version:      1.00
EMail:        francois.piette@overbyte.be    francois.piette@rtfm.be
              http://www.overbyte.be
Support:      Unsupported code.
Legal issues: Copyright (C) 2011 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for
              any purpose, including commercial applications, and to alter
              it and redistribute it freely, subject to the following
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


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSnmpCliTst1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IniFiles, StdCtrls, ExtCtrls, StrUtils, Types,
  OverbyteIcsWinsock,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsUtils,
  OverbyteIcsAsn1Utils,
  OverbyteIcsSnmpCli,
  OverbyteIcsSnmpMsgs;

type
  TSnmpClientTestForm = class(TForm)
    ToolsPanel: TPanel;
    DisplayMemo: TMemo;
    HostEditLabel: TLabel;
    CommunityLabel: TLabel;
    HostEdit: TEdit;
    CommunityEdit: TEdit;
    Label4: TLabel;
    GetButton: TButton;
    TimeoutLabel1: TLabel;
    TimeoutEdit: TEdit;
    ClearButton: TButton;
    MaxRetriesEdit: TEdit;
    MaxRetriesLabel: TLabel;
    PortLabel: TLabel;
    PortEdit: TEdit;
    TimeoutLabel: TLabel;
    PortRangeLabel: TLabel;
    PortRangeEdit: TEdit;
    GetTableButton: TButton;
    OidComboBox: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GetButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure GetTableButtonClick(Sender: TObject);
  private
    FIniFileName    : String;
    FInitialized    : Boolean;
    FSnmpCli        : TSnmpCli;
    FStartTick      : Cardinal;
    FEndTick        : Cardinal;
    procedure SnmpCliRequestDone(Sender     : TObject;
                                 ReqType    : TSnmpCliReq;
                                 var Action : TSnmpCliAct;
                                 ErrCode    : Word);
  public
    procedure Display(Msg : String);
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  SnmpClientTestForm: TSnmpClientTestForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'WindowMain';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeySnmpServer      = 'SnmpServer';
    KeyOID             = 'OID';
    KeyCommunity       = 'Community';
    KeyTimeout         = 'Timeout';
    KeyMaxRetries      = 'MaxRetries';
    KeyPort            = 'Port';
    KeyPortRange       = 'PortRange';
    KeyOidIndex        = 'OidIndex';



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpClientTestForm.FormCreate(Sender: TObject);
begin
    Caption := 'SNMP Client Test - 1.00 - http://www.overbyte.be';
    FIniFileName  := ChangeFileExt(Application.ExeName, '.ini');
    DisplayMemo.Clear;
    TimeoutLabel.Caption      := '';
    FSnmpCli                  := TSnmpCli.Create(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpClientTestForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        HostEdit.Text         := IniFile.ReadString(SectionData, KeySnmpServer, '192.168.1.160');
        CommunityEdit.Text    := IniFile.ReadString(SectionData, KeyCommunity,  'public');
        TimeoutEdit.Text      := IniFile.ReadString(SectionData, KeyTimeout,    '10');
        MaxRetriesEdit.Text   := IniFile.ReadString(SectionData, KeyMaxRetries, '5');
        PortEdit.Text         := IniFile.ReadString(SectionData, KeyPort,       '5000');
        PortRangeEdit.Text    := IniFile.ReadString(SectionData, KeyPortRange,  '50');
        OidComboBox.Text      := IniFile.ReadString(SectionData, KeyOID,        '1.3.6.1.2.1.1.1.0');
        OidComboBox.ItemIndex := IniFile.ReadInteger(SectionData, KeyOidIndex, 0);
        if OidComboBox.Items.IndexOf(OidComboBox.Text) < 0 then
            OidComboBox.Items.Add(OidComboBox.Text);

        IniFile.Destroy;
        DisplayMemo.Clear;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpClientTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    try
        IniFile := TIniFile.Create(FIniFileName);
        try
            IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
            IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
            IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
            IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
            IniFile.WriteString(SectionData, KeySnmpServer, HostEdit.Text);
            IniFile.WriteString(SectionData, KeyCommunity,  CommunityEdit.Text);
            IniFile.WriteString(SectionData, KeyTimeout,    TimeoutEdit.Text);
            IniFile.WriteString(SectionData, KeyMaxRetries, MaxRetriesEdit.Text);
            IniFile.WriteString(SectionData, KeyPort,       PortEdit.Text);
            IniFile.WriteString(SectionData, KeyPortRange,  PortRangeEdit.Text);
        finally
            IniFile.Destroy;
        end;
    except
        ShowMessage('Can''t save configuration, check permissions');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpClientTestForm.ClearButtonClick(Sender: TObject);
begin
    TimeoutLabel.Caption := '';
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpClientTestForm.Display(Msg : String);
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 2000 then begin
            while DisplayMemo.Lines.Count > 2000 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpClientTestForm.GetButtonClick(Sender: TObject);
begin
    TimeoutLabel.Caption     := '';
    FSnmpCli.Port            := Trim(PortEdit.Text);
    FSnmpCli.Addr            := '0.0.0.0';
    FSnmpCli.HostIP          := HostEdit.Text;
    FSnmpCli.OID             := Trim(OidComboBox.Text);
    FSnmpCli.Version         := SNMP_V1;
    FSnmpCli.Community       := AnsiString(Trim(CommunityEdit.Text));
    FSnmpCli.Timeout         := Trunc(StrToFloatDef(TimeoutEdit.Text, 2) * 1000);
    FSnmpCli.OnRequestDone   := SnmpCliRequestDone;

    FStartTick := GetTickCount;
    if FSnmpCli.Get then
        GetButton.Enabled := FALSE
    else
        Display('Failed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpClientTestForm.SnmpCliRequestDone(
    Sender     : TObject;
    ReqType    : TSnmpCliReq;
    var Action : TSnmpCliAct;
    ErrCode    : Word);
var
    Msg     : String;
    Elapsed : Cardinal;
begin
    FEndTick := GetTickCount;
    Elapsed  := IcsCalcTickDiff(FStartTick, FEndTick);
    GetButton.Enabled := TRUE;
    if ErrCode <> 0 then begin
        if ErrCode = SNMP_TIMEOUT then
            Msg := 'Timeout error: ' + HostEdit.Text +
                   ' do not answer to SNMP request'
        else
            Msg := 'Error #' + IntToStr(ErrCode);
    end
    else if FSnmpCli.Reply.ErrorStatus <> 0 then
        Msg := '<ERROR STATUS=' +
               IntToStr(FSnmpCli.Reply.ErrorStatus) + ' "' +
               SnmpErrorToString(FSnmpCli.Reply.ErrorStatus) + '">'
    else begin
        if (ReqType = srqGet) or
           ((ReqType = srqGetNext) and (Action = sacContinue)) then
            Msg := FSnmpCli.ResponseValue
        else if (ReqType = srqGetNext) and (Action = sacNone) then
            Msg := '<END OF TABLE>'
        else
            Msg := '';
    end;

    if Msg <> '' then
        Display(FormatDateTime('YYYYMMDD|HHNNSS.ZZZ|', Now) +
                IntToStr(Elapsed) + '|' +
                FSnmpCli.ResponseOID + '|' +
                FSnmpCli.HostIP + '|' +
                Msg);

    if Action = sacNone then
        FSnmpCli.Abort;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSnmpClientTestForm.GetTableButtonClick(Sender: TObject);
begin
    TimeoutLabel.Caption      := '';
    FSnmpCli.Port             := Trim(PortEdit.Text);
    FSnmpCli.Addr             := '0.0.0.0';
    FSnmpCli.HostIP           := HostEdit.Text;
    FSnmpCli.OID              := Trim(OidComboBox.Text);
    FSnmpCli.Timeout          := Trunc(StrToFloatDef(TimeoutEdit.Text, 2) * 1000);
    FSnmpCli.Version          := SNMP_V1;
    FSnmpCli.Community        := AnsiString(Trim(CommunityEdit.Text));
    FSnmpCli.OnRequestDone    := SnmpCliRequestDone;

    FStartTick := GetTickCount;
    if FSnmpCli.GetNext then
        GetButton.Enabled := FALSE
    else
        Display('Failed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.
