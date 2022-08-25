{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS WMI Demos
Creation:     Mar 2020
Updated:      Apr 2020
Version:      8.64
Support:      Use the mailing list ics-ssl@elists.org
Legal issues: Copyright (C) 2020 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

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
21 Apr 2020 - 8.64 baseline
17 Aug 2020 - 8.65 - Check updating DNS actually worked.
                     Click on old DNS record to update it.
                     Update DNS now works, replaces all existing records.
                     


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsWmiTst1;

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TypInfo, Types, ComCtrls, ExtCtrls, Spin, Grids,
  OverbyteIcsWmi,
  OverbyteIcsUtils,
  OverbyteIcsIniFiles; 


const
    SectionMainWindow    = 'MainWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
//    KeyRestParams        = 'RestParams';

type
  TWmiDemoForm = class(TForm)
  // saved props
    WmiClass: TComboBox;
    WmiNamespace: TComboBox;
    WmiCommand: TComboBox;
    WmiOneProp: TComboBox;
    WmiLogOutput: TCheckBox;
    NetComputer: TEdit;
    NetUserName: TEdit;
    NetPassword: TEdit;
    DnsRecType: TComboBox;
    DnsRecName: TEdit;
    DnsRecData: TEdit;
    DnsRecZone: TEdit;

  // not saved props
    PageControl: TPageControl;
    TabSheetNetwork: TTabSheet;
    TabSheetWmi: TTabSheet;
    LogWin: TMemo;
    WmiListView: TListView;
    PanelWmi: TPanel;
    Label5: TLabel;
    Label2: TLabel;
    doGetClass: TButton;
    Panel2: TPanel;
    Label1: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    Label15: TLabel;
    doExit2: TButton;
    doRenameComp: TButton;
    doReboot: TButton;
    doCloseDown: TButton;
    doGetOneProp: TButton;
    LabelStatus: TLabel;
    Label6: TLabel;
    NewCompName: TEdit;
    PanelLogins: TPanel;
    PanelTop: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    GroupBoxAddr: TGroupBox;
    Label11: TLabel;
    doGetAdaptors: TButton;
    ListNetAdaptors: TListView;
    doGetAddresses: TButton;
    doSetAddresses: TButton;
    NetUseDhcp: TCheckBox;
    ListNetIPs: TStringGrid;
    ListNetGateways: TStringGrid;
    GroupBoxDns: TGroupBox;
    doListZones: TButton;
    doListRecs: TButton;
    ListZones: TListView;
    ListRecords: TListView;
    Label4: TLabel;
    GroupBoxNewRec: TGroupBox;
    Label7: TLabel;
    Label8: TLabel;
    Label10: TLabel;
    doDnsRecUpd: TButton;
    doDnsRecAdd: TButton;
    doDnsRecDel: TButton;
    LabelDnsZone: TLabel;
    Label13: TLabel;
    doCheckDnsSrv: TButton;
    LabelDnsStatus: TLabel;
    procedure doGetClassClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure doGetOnePropClick(Sender: TObject);
    procedure LogWinDblClick(Sender: TObject);
    procedure doCloseDownClick(Sender: TObject);
    procedure doRebootClick(Sender: TObject);
    procedure doRenameCompClick(Sender: TObject);
    procedure doListZonesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doGetAdaptorsClick(Sender: TObject);
    procedure doGetAddressesClick(Sender: TObject);
    procedure ListNetAdaptorsDblClick(Sender: TObject);
    procedure doSetAddressesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doListRecsClick(Sender: TObject);
    procedure ListZonesDblClick(Sender: TObject);
    procedure doDnsUpdateRec(Sender: TObject);
    procedure ListRecordsDblClick(Sender: TObject);
    procedure doCheckDnsSrvClick(Sender: TObject);
    procedure ListRecordsClick(Sender: TObject);
    procedure DnsRecDataDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddLog (const S: string) ;
    procedure ClearGrid(MyGrid: TStringGrid);
  end;

var
  WmiDemoForm: TWmiDemoForm;
  AdptNr: Integer = -1;
  AdptName: String;
  FIniFileName: String;

implementation

{$R *.dfm}

procedure TWmiDemoForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile: TIcsIniFile;
    section, temp: String;
begin
    FIniFileName := GetIcsIniFileName;
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do begin
    // form positions
        WriteInteger(SectionMainWindow, KeyTop, Top);
        WriteInteger(SectionMainWindow, KeyLeft, Left);
        WriteInteger(SectionMainWindow, KeyWidth, Width);
        WriteInteger(SectionMainWindow, KeyHeight, Height);
        section := SectionData;
        WriteString (section, 'WmiClass_Text', WmiClass.Text) ;
        WriteString (section, 'WmiNamespace_Text', WmiNamespace.Text) ;
        WriteString (section, 'WmiCommand_Text', WmiCommand.Text) ;
        WriteString (section, 'WmiOneProp_Text', WmiOneProp.Text) ;
        if WmiLogOutput.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'WmiLogOutput_Checked', temp) ;
        WriteString (section, 'NetComputer_Text', NetComputer.Text) ;
        WriteString (section, 'NetUserName_Text', NetUserName.Text) ;
        WriteString (section, 'NetPassword_Text', NetPassword.Text) ;
        WriteString (section, 'DnsRecType_Text', DnsRecType.Text) ;
        WriteString (section, 'DnsRecName_Text', DnsRecName.Text) ;
        WriteString (section, 'DnsRecData_Text', DnsRecData.Text) ;
        WriteString (section, 'DnsRecZone_Text', DnsRecZone.Text) ;
        UpdateFile;
    end;
    IniFile.Free;

end;

procedure TWmiDemoForm.FormCreate(Sender: TObject);
var
    IniFile: TIcsIniFile;
    section: String;
begin
    FIniFileName := GetIcsIniFileName;
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do begin
    // form positions
        Width := ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height := ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top := ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left := ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width)  div 2);
        section := SectionData;
        WmiClass.Text := ReadString (section, 'WmiClass_Text', WmiClass.Text) ;
        WmiNamespace.Text := ReadString (section, 'WmiNamespace_Text', WmiNamespace.Text) ;
        WmiCommand.Text := ReadString (section, 'WmiCommand_Text', WmiCommand.Text) ;
        WmiOneProp.Text := ReadString (section, 'WmiOneProp_Text', WmiOneProp.Text) ;
        if ReadString (section, 'WmiLogOutput_Checked', 'False') = 'True' then WmiLogOutput.Checked := true else WmiLogOutput.Checked := false ;
        NetComputer.Text := ReadString (section, 'NetComputer_Text', IcsGetCompName) ;
        NetUserName.Text := ReadString (section, 'NetUserName_Text', '') ;
        NetPassword.Text := ReadString (section, 'NetPassword_Text', '') ;
        DnsRecType.Text := ReadString (section, 'DnsRecType_Text', 'A') ;
        DnsRecName.Text := ReadString (section, 'DnsRecName_Text', '') ;
        DnsRecData.Text := ReadString (section, 'DnsRecData_Text', '') ;
        DnsRecZone.Text := ReadString (section, 'DnsRecZone_Text', '') ;
    end;
    IniFile.Free;
    NewCompName.Text := IcsGetCompName ;
    ListNetIPs.Cells [0, 0] := 'Static IP Address' ;
    ListNetIPs.Cells [1, 0] := 'Mask' ;
    ListNetGateways.Cells [0, 0] := 'Gateway IP Address' ;
    ListNetGateways.Cells [1, 0] := 'Cost Metric' ;
end;

procedure TWmiDemoForm.FormShow(Sender: TObject);
begin
// x
end;

procedure TWmiDemoForm.AddLog (const S: string) ;
begin
    if Pos (IcsLF,S) > 0 then
        LogWin.Lines.Text := LogWin.Lines.Text + IcsCRLF + S
    else
       LogWin.Lines.Add (S) ;
end;

procedure TWmiDemoForm.doExitClick(Sender: TObject);
begin
    Close;
end;


procedure TWmiDemoForm.doGetClassClick(Sender: TObject);
var
    rows, instances, cols, I, J: integer ;
    WmiResults: T2DimStrArray ;
    OldCursor: TCursor ;
    errstr: string ;
begin
    doGetClass.Enabled := false ;
    OldCursor := Screen.Cursor ;
    Screen.Cursor := crHourGlass ;
    try
        WmiListView.Items.Clear ;
        for I := 0 to WmiListView.Columns.Count - 1 do
            WmiListView.Columns [I].Width := 150 ;
        rows := IcsWmiGetInfoEx (NetComputer.Text, WmiNameSpace.Text, NetUserName.Text,
                           NetPassword.Text, WmiClass.Text, WmiResults, instances, errstr) ;

    // fill listview
        if rows > 0 then
        begin
            LabelStatus.Caption := WmiClass.Text + ', instance: ' +
                                IntToStr (instances) + ', Items: ' + IntToStr (rows) ;
            cols := instances ;
            if cols >= WmiListView.Columns.Count then
                cols := WmiListView.Columns.Count - 1 ;
            for J := 0 to cols do
                WmiListView.Columns.Items [J].Caption := WmiResults [J, 0] ;
            for I := 1 to rows do
            begin
                with WmiListView.Items.Add do
                begin
                    Caption := WmiResults [0, I] ;  // name
                    for J := 1 to cols do
                        SubItems.Add (WmiResults [J, I]) ;  // value
                end ;
            end ;
        end
        else if rows = -1 then
            LabelStatus.Caption := WmiClass.Text + ', Error: ' + errstr
        else
           LabelStatus.Caption := WmiClass.Text + ', instances: None' ;

     // log output as well
        if WmiLogOutput.Checked then
        begin
            AddLog (LabelStatus.Caption);
            if rows > 0 then
            begin
                for J := 1 to instances do
                begin
                    AddLog (WmiClass.Text + ', instance ' + IntToStr(J) +
                                                ' of ' + IntToStr(instances));
                    for I := 1 to rows do
                        AddLog (WmiResults [0, I]  + ' = ' + WmiResults [J, I]) ;
                    AddLog ('');
                end ;
            end;
        end;

    finally
        doGetClass.Enabled := true ;
        Screen.Cursor := OldCursor ;
        WmiResults := Nil ;
    end ;
end;

procedure TWmiDemoForm.doGetOnePropClick(Sender: TObject);
var
    res: string ;
    Errinfo: string ;
begin
    doGetOneProp.Enabled := false ;
    try
        if IcsWmiGetOneQ (WmiCommand.Text, WmiOneProp.Text, res, Errinfo) > 0 then
        begin
            AddLog ('Command: ' + WmiCommand.Text + ' ( ' + WmiOneProp.Text + ')');
            AddLog ('Result: ' + res) ;
        end
        else
            AddLog ('Command Failed: ' + Errinfo);
    finally
        doGetOneProp.Enabled := true ;
    end ;
end;

procedure TWmiDemoForm.doRebootClick(Sender: TObject);
var
    Errinfo: string ;
begin
    if Application.MessageBox (PChar ('Confirm Reboot PC ' +
                        NetComputer.Text + ' Now'),
                             'WMI - Reboot PC', MB_OKCANCEL) <> IDOK then exit  ;
    if IcsWmiCloseWin (NetComputer.Text, NetUserName.Text, NetPassword.Text, true, Errinfo) = 0 then
        Errinfo:= 'PC Reboot Accepted';
    AddLog (Errinfo) ;
end;

procedure TWmiDemoForm.doCloseDownClick(Sender: TObject);
var
    Errinfo: string ;
begin
    if Application.MessageBox (Pchar ('Confirm Power Down PC ' +
                        NetComputer.Text + ' Now'),
                             'WMI - Power Down PC', MB_OKCANCEL) <> IDOK then exit  ;
    if IcsWmiCloseWin (NetComputer.Text, NetUserName.Text, NetPassword.Text, false, Errinfo) = 0 then
        Errinfo := 'PC Power Down Accepted' ;
    AddLog (Errinfo) ;
end;

procedure TWmiDemoForm.doRenameCompClick(Sender: TObject);
var
    res: integer ;
    Errinfo: string ;
begin
    if NewCompName.Text = IcsGetCompName then exit ;
    if NewCompName.Text = '' then exit ;
    if Application.MessageBox (Pchar ('Confirm Rename This PC from ' +
                         IcsGetCompName + ' to ' + NewCompName.Text),
                             'WMI - Rename Computer', MB_OKCANCEL) <> IDOK then exit  ;
  // note, ignore domain controller login
    res := IcsWmiRenameComp (NetComputer.Text, NetUserName.Text, NetPassword.Text,
                                                    NewCompName.Text, '', '', Errinfo) ;
    if res = 0 then
        AddLog ('Rename Computer OK, Must Reboot Now')
    else
        AddLog ('Rename Computer Failed: ' + Errinfo) ;
end;


procedure TWmiDemoForm.LogWinDblClick(Sender: TObject);
begin
    LogWin.Lines.Clear;
end;


procedure TWmiDemoForm.doGetAdaptorsClick(Sender: TObject);
var
    AdapterList: T2DimStrArray;
    Tot, I: Integer;
    Errinfo: string ;
begin
    doGetAdaptors.Enabled := False ;
    ListNetAdaptors.Items.Clear;
    Tot := IcsWmiListAdaptors (NetComputer.Text, NetUserName.Text,
                        NetPassword.Text, AdapterList, Errinfo, False) ;  // enabled or all
    if Tot = 0 then
    begin
        AddLog ('Can Not Find Any Network Adapters: ' + Errinfo) ;
        doGetAdaptors.Enabled := True ;
        exit ;
    end ;
    for I := 0 to Tot - 1 do
    begin
        with ListNetAdaptors.Items.Add do
        begin
            Caption := AdapterList [I, 0];
            SubItems.Add (AdapterList [I, 1]) ;
            SubItems.Add (AdapterList [I, 2]) ;
        end;
    end;
    AddLog ('Listed ' + IntToStr (Tot) + ' Network Adapters') ;
    doGetAdaptors.Enabled := True ;
end;

procedure TWmiDemoForm.ClearGrid(MyGrid: TStringGrid);
var
    I, J: Integer;
begin
    with MyGrid do
    begin
        for I := 1 to RowCount - 1 do
        begin
            for J := 0 to ColCount  - 1 do
                Cells [J, I] := '';
        end;
    end;
end;

procedure TWmiDemoForm.DnsRecDataDblClick(Sender: TObject);
begin
    DnsRecData.Text := '';  
end;

procedure TWmiDemoForm.doGetAddressesClick(Sender: TObject);
var
    DhcpFlag: Boolean;
    WmiAddrs: TWmiAddrs;
    WmiGateways: TWmiGateways;
    Idx, TotAddr, TotGate, I: Integer;
    Errinfo: string ;
begin
    Idx := ListNetAdaptors.ItemIndex ;
    if Idx < 0 then
    begin
        AddLog ('Must Select a Network Adapter First') ;
        exit ;
    end ;
    AdptNr := atoi(ListNetAdaptors.Items[Idx].Caption) ;
    AdptName := ListNetAdaptors.Items[Idx].SubItems[0] ;
    ClearGrid (ListNetIPs);
    ClearGrid (ListNetGateways);
    TotAddr := IcsWmiGetAddresses (NetComputer.Text, NetUserName.Text,
                NetPassword.Text, AdptNr, DhcpFlag, WmiAddrs, WmiGateways, Errinfo) ;
    TotGate := Length(WmiGateways);
    NetUseDhcp.Checked := DhcpFlag;
    if DhcpFlag then
    begin
        ListNetIPs.Cells [0, 1] := 'DHCP' ;
        AddLog (AdptName + ': Set for DHCP') ;
        exit ;
    end ;
    if TotAddr <= 0 then
    begin
        AddLog (AdptName + ': Can Not Find Any IP Addresses: ' + Errinfo) ;
        exit ;
    end ;
    AddLog (AdptName + ': Found ' + IntToStr (TotAddr) + ' IP Addresses') ;
    ListNetIPs.RowCount := TotAddr + 7;
    for I := 0 to TotAddr - 1 do
    begin
        ListNetIPs.Cells [0, I + 1] := WmiAddrs [I].IpAddr;
        ListNetIPs.Cells [1, I + 1] := WmiAddrs [I].Mask ;
        AddLog ('Address: ' +  WmiAddrs [I].IpAddr + ', Mask: ' + WmiAddrs [I].Mask);
    end;
    if TotGate > 0 then
    begin
        ListNetGateways.RowCount := TotGate + 3;
        AddLog (AdptName + ': Found ' + IntToStr (TotGate) + ' Gateways') ;
        for I := 0 to TotGate - 1 do
        begin
            ListNetGateways.Cells [0, I + 1] := WmiGateways [I].IpAddr;
            ListNetGateways.Cells [1, I + 1] := WmiGateways [I].Cost ;
            AddLog ('Gateway: ' +  WmiGateways [I].IpAddr + ', Cost: ' + WmiGateways [I].Cost);
        end;
    end;
end;

procedure TWmiDemoForm.ListNetAdaptorsDblClick(Sender: TObject);
begin
    doGetAddressesClick(Self);
end;



procedure TWmiDemoForm.doSetAddressesClick(Sender: TObject);
var
    WmiAddrs: TWmiAddrs;
    WmiGateways: TWmiGateways;
    TotAddr, TotGate, I: Integer;
    Res: Int64;
    Errinfo: string ;
begin
   if (AdptNr < 0) or ((ListNetIPs.Cells [0, 1]  = '') and (NOT NetUseDhcp.Checked)) then
    begin
        AddLog ('Must Select a Network Adapter and IP Addresses First') ;
        exit ;
    end ;
    if Application.MessageBox (Pchar ('Confirm Update ' + AdptName + ' Network Adaptor IP Addresses'),
                             'WMI - Update RIP Addresses', MB_OKCANCEL) <> IDOK then exit  ;
    TotAddr := 0;
    TotGate := 0;
    doSetAddresses.Enabled := False;
    if NOT NetUseDhcp.Checked then
    begin
        SetLength (WmiAddrs, ListNetIPs.RowCount);
        SetLength (WmiGateways, ListNetGateways.RowCount);
        for I := 1 to ListNetIPs.RowCount - 1 do
        begin
            WmiAddrs [TotAddr].IpAddr := IcsTrim(ListNetIPs.Cells[0, I]);
            WmiAddrs [TotAddr].Mask := IcsTrim(ListNetIPs.Cells[1, I]);
            if (WmiAddrs [TotAddr].IpAddr <> '') and (WmiAddrs [TotAddr].Mask <> '') then
                TotAddr := TotAddr + 1 ;
        end;
        if ListNetGateways.RowCount > 0 then
        begin
            for I := 1 to ListNetGateways.RowCount - 1 do
            begin
                WmiGateways [TotGate].IpAddr := IcsTrim(ListNetGateways.Cells[0, I]);
                WmiGateways [TotGate].Cost := IcsTrim(ListNetGateways.Cells[1, I]);
                if (WmiGateways [TotGate].IpAddr <> '') and (WmiGateways [TotGate].Cost <> '') then
                    TotGate := TotGate + 1 ;
            end;
        end;
        if TotAddr = 0 then
        begin
            AddLog ('Must Specify IP Addresses and Masks First') ;
            doSetAddresses.Enabled := True;
            exit ;
        end;
    end;
    SetLength (WmiAddrs, TotAddr);
    SetLength (WmiGateways, TotGate);
    Res := IcsWmiSetAddresses (NetComputer.Text, NetUserName.Text, NetPassword.Text,
                            AdptNr, NetUseDhcp.Checked, WmiAddrs, WmiGateways, Errinfo) ;
    if Errinfo <> '' then
        AddLog (AdptName + ': Set IP Addressses Failed: ' + Errinfo)
    else if res > $80000000 then
        AddLog (AdptName + ': Set IP Addressses WMI Result: $' + IntToHex (res, 8))
    else
        AddLog (AdptName + ': Set IP Addressses WMI Result: ' + IntToStr (res)) ;
    doSetAddresses.Enabled := True;
end;

procedure TWmiDemoForm.doCheckDnsSrvClick(Sender: TObject);
var
    IpAddrs, Errinfo, SrvName: string;
begin
    doCheckDnsSrv.Enabled := False;
    SrvName := IcsWmiChkDnsSrv (NetComputer.Text, NetUserName.Text, NetPassword.Text, IpAddrs, Errinfo) ;
    doCheckDnsSrv.Enabled := True;
    if SrvName = '' then
        LabelDnsStatus.Caption := 'Can Not Find DNS Server: ' + Errinfo
    else
        LabelDnsStatus.Caption := 'DNS Server Name: ' + SrvName + ', IP Addresses: ' + IpAddrs;
    AddLog (LabelDnsStatus.Caption) ;
end;


procedure TWmiDemoForm.doListZonesClick(Sender: TObject);
var
    WmiDnsZones: TWmiDnsZones;
    Tot, I: Integer;
    ZType, ZRev, Errinfo: string ;
begin
    doDnsRecUpd.Enabled := False;
    doDnsRecAdd.Enabled := False;
    doDnsRecDel.Enabled := False;
    doListZones.Enabled := False;
    ListZones.Items.Clear;
    Tot := IcsWmiListDnsZones (NetComputer.Text, NetUserName.Text, NetPassword.Text, WmiDnsZones, Errinfo) ;
    if Tot = 0 then
    begin
        LabelDnsStatus.Caption := 'Can Not Find Any DNS Zones: ' + Errinfo ;
        AddLog (LabelDnsStatus.Caption) ;
        doListZones.Enabled := True ;
        exit ;
    end ;
    LabelDnsStatus.Caption := 'Total Domain Zones ' + IntToStr(Tot);
    AddLog (LabelDnsStatus.Caption) ;
    for I := 0 to Tot - 1 do
    begin
        ZType := ZoneTypeLits [WmiDnsZones [I].ZoneType];
        if WmiDnsZones [I].Reverse then
            ZRev := 'Reverse'
        else
            ZRev := 'Forward';
        with ListZones.Items.Add do
        begin
            Caption := WmiDnsZones [I].ZoneName;
            SubItems.Add (ZType) ;
            SubItems.Add (ZRev);
        end;
        AddLog ('Zone Name: ' +  WmiDnsZones [I].ZoneName + ', Type: ' + ZType + ': ' + ZRev);
    end;
    doListZones.Enabled := True ;
end;

procedure TWmiDemoForm.ListZonesDblClick(Sender: TObject);
begin
    doListRecsClick (Self);
end;

procedure TWmiDemoForm.doListRecsClick(Sender: TObject);
var
    WmiDnsRecs: TWmiDnsRecs;
    Tot, I: Integer;
    Errinfo: string ;
begin
    doDnsRecUpd.Enabled := False;
    doDnsRecAdd.Enabled := False;
    doDnsRecDel.Enabled := False;
    I := ListZones.ItemIndex;
    if (I < 0) and (DnsRecZone.Text = '') then
    begin
        LabelDnsStatus.Caption := 'Must Select a DNS Zone First' ;
        AddLog (LabelDnsStatus.Caption) ;
        exit ;
    end
    else if (I >= 0) then
        DnsRecZone.Text := ListZones.Items[I].Caption;
    doListRecs.Enabled := False;
    ListRecords.Items.Clear;
    Tot := IcsWmiListDnsRecs (NetComputer.Text, NetUserName.Text,
                        NetPassword.Text, DnsRecZone.Text, '', WmiDnsRecs, Errinfo) ;
    if Errinfo <> '' then
    begin
        LabelDnsStatus.Caption := 'Get DNS Records Failed: ' + Errinfo;
        AddLog (LabelDnsStatus.Caption) ;
        exit;
    end
    else if Tot = 0 then
    begin
        LabelDnsStatus.Caption := 'Can Not Find Any DNS Records for Zone: ' + DnsRecZone.Text ;
        AddLog (LabelDnsStatus.Caption) ;
        doListRecs.Enabled := True ;
        exit ;
    end ;
    LabelDnsStatus.Caption := 'Total Domain Zone Records ' + IntToStr(Tot);
    AddLog (LabelDnsStatus.Caption) ;
    for I := 0 to Tot - 1 do
    begin
        with ListRecords.Items.Add do
        begin
            Caption := WmiDnsRecs [I].HostName;
            SubItems.Add (WmiDnsRecs [I].RecType) ;
            SubItems.Add (WmiDnsRecs [I].RecData) ;
            SubItems.Add (IntToStr(WmiDnsRecs [I].Instance)) ;
            SubItems.Add (WmiDnsRecs [I].TextRep) ;
        end;
        AddLog ('Host Name: ' +  WmiDnsRecs [I].HostName + ', Type: ' +
               WmiDnsRecs [I].RecType + ', Data: ' + WmiDnsRecs [I].RecData +
                ', Instance ' + IntToStr(WmiDnsRecs [I].Instance));
    end;
    doListRecs.Enabled := True ;
    doDnsRecAdd.Enabled := True;
end;

procedure TWmiDemoForm.doDnsUpdateRec(Sender: TObject);
var
    WmiEdtFunc: TWmiEdtFunc;
    WmiDnsRec: TWmiDnsRec;
    Errinfo: string;
    Res: Integer;
    Flag: Boolean;
begin
    if Sender = doDnsRecUpd then WmiEdtFunc := EdtFuncUpd
    else  if Sender = doDnsRecAdd then WmiEdtFunc := EdtFuncAdd
    else if Sender = doDnsRecDel then WmiEdtFunc := EdtFuncDel
    else exit;
    if DnsRecZone.Text = '' then
    begin
        AddLog ('Must Select a DNS Zone First') ;
        exit ;
    end ;
    doDnsRecUpd.Enabled := False;
    doDnsRecAdd.Enabled := False;
    doDnsRecDel.Enabled := False;
    LabelDnsStatus.Caption := '(Updating DNS Server)';
    AddLog (LabelDnsStatus.Caption) ;
    WmiDnsRec.RecType := DnsRecType.Text;
    WmiDnsRec.HostName := DnsRecName.Text;
    WmiDnsRec.RecData := Trim(DnsRecData.Text);   { V8.65 }
    Res := IcsWmiUpdDnsRec (NetComputer.Text, NetUserName.Text, NetPassword.Text,
                                   DnsRecZone.Text, WmiDnsRec, WmiEdtFunc, Errinfo);
    if Res = 0 then
    begin
    { V8.65 check it worked }
         flag := IcsWmiChkDnsRec (NetComputer.Text, NetUserName.Text,
                              NetPassword.Text, DnsRecZone.Text, WmiDnsRec, Errinfo) ;
        if Flag then
            Errinfo := 'DNS Resource Record Updated OK: ' + DnsRecZone.Text + ': ' + WmiDnsRec.RecData
        else begin
            if WmiEdtFunc = EdtFuncDel then
                Errinfo := 'DNS Resource Record Deleted OK: ' + DnsRecZone.Text + ': ' + WmiDnsRec.HostName
             else
                Errinfo := 'DNS Resource Record Failed: Can Not Find New RR Records: ' + WmiDnsRec.RecData;
        end;
        doListRecsClick (Self);
    end
    else
        Errinfo := 'Set DNS Resource Record Failed: ' + Errinfo + ' for ' + DnsRecZone.Text;
    LabelDnsStatus.Caption := Errinfo;
    AddLog (Errinfo) ;
    doDnsRecUpd.Enabled := True;
    doDnsRecAdd.Enabled := True;
    doDnsRecDel.Enabled := True;
end;

procedure TWmiDemoForm.ListRecordsClick(Sender: TObject);
var
    I: Integer;
begin
    I := ListRecords.ItemIndex;
    if I < 0 then Exit;
    DnsRecName.Text := ListRecords.Items[I].Caption;
    DnsRecType.Text := ListRecords.Items[I].SubItems[0];
    DnsRecData.Text := ListRecords.Items[I].SubItems[1];
    doDnsRecUpd.Enabled := True;
    doDnsRecDel.Enabled := True;
end;

procedure TWmiDemoForm.ListRecordsDblClick(Sender: TObject);
var
    WmiDnsRec: TWmiDnsRec;
    Errinfo: string;
    I: Integer;
    Res: Integer;
begin
    I := ListRecords.ItemIndex;
    if I < 0 then Exit;
    WmiDnsRec.HostName := ListRecords.Items[I].Caption;
    WmiDnsRec.RecType := ListRecords.Items[I].SubItems[0];
    WmiDnsRec.RecData := ListRecords.Items[I].SubItems[1];
    if Application.MessageBox (PChar ('Confirm Delete DNS  ' + WmiDnsRec.RecType +
               ' record ' + WmiDnsRec.HostName), 'WMI - DNS Server', MB_OKCANCEL) <> IDOK then exit  ;
    Res := IcsWmiUpdDnsRec (NetComputer.Text, NetUserName.Text, NetPassword.Text,
                                   DnsRecZone.Text, WmiDnsRec, EdtFuncDel, Errinfo);
    if Res = 0 then
    begin
        doListRecsClick (Self);
        Errinfo := 'DNS Resource Record Deleted OK: ' + DnsRecZone.Text + ': ' + WmiDnsRec.HostName
    end
    else
        Errinfo := DnsRecZone.Text + ': Set DNS Resource Record Failed: ' + Errinfo;
    LabelDnsStatus.Caption := Errinfo;
    AddLog (Errinfo) ;
end;



end.

