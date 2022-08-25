unit wmimain;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{
Magenta Systems WMI and SMART Component demo application v5.6
Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright 2018, Magenta Systems Ltd

10th January 2004 - Release 4.93
12th July 2004 - Release 4.94 - added disk Smart failure info
14th October 2004 - Release 4.95 - added SCSI disk serial checking
9th January 2005  - Release 4.96 - added MagWmiCloseWin to close down windows
22nd October 2005 - Release 5.0  - separated from TMagRas
29th July 2008    - Release 5.1  - compability with unicode in Delphi 2009
5th March 2009    - Release 5.2  - fixed memory leaks with OleVariants
                                   better error handling
9th August 2010   - Release 5.3  - fixed some string cast warnings for Delphi 2009 and later
23 January 2013     Release 5.4  - added Map Drives button to list details of all physical disk drives
                                   added Map SCSI button to list details of all devices on SCSI adaptors,
                                      that may be ATA disks, DVD-ROMs, etc.
5 August 2013     - Release 5.5 - fix another WMI memory leak with OleVariants
26 November 2018  - Release 5.6 - added power related classes, ie Win32_Battery

}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Spin, ExtCtrls, magwmi, magsubs1 ;

type
  TForm1 = class(TForm)
    edtClass: TComboBox;
    edtComputer: TEdit;
    edtUser: TEdit;
    edtPass: TEdit;
    Label4: TLabel;
    Label7: TLabel;
    Label5: TLabel;
    Label8: TLabel;
    ListView: TListView;
    doGetClass: TButton;
    doExit: TButton;
    doMB: TButton;
    ResInfo: TEdit;
    doBIOS: TButton;
    Label1: TLabel;
    doDiskModel: TButton;
    DiskNum: TSpinEdit;
    doDiskSerial: TButton;
    doBootTime: TButton;
    doCommand: TButton;
    OneCommand: TComboBox;
    Label3: TLabel;
    OneProp: TComboBox;
    Panel1: TPanel;
    doIPAddr: TButton;
    SubNetMask: TEdit;
    Label6: TLabel;
    Label10: TLabel;
    IPAddress: TEdit;
    IPGateway: TEdit;
    Label9: TLabel;
    edtNamespace: TComboBox;
    doRenameComp: TButton;
    NewCompName: TEdit;
    Label11: TLabel;
    doSmart: TButton;
    doReboot: TButton;
    doCloseDown: TButton;
    doMemory: TButton;
    StatusBar: TStatusBar;
    doMapDrives: TButton;
    doMapSCSI: TButton;
    procedure doGetClassClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure doMBClick(Sender: TObject);
    procedure doBIOSClick(Sender: TObject);
    procedure doCommandClick(Sender: TObject);
    procedure doBootTimeClick(Sender: TObject);
    procedure doDiskSerialClick(Sender: TObject);
    procedure doDiskModelClick(Sender: TObject);
    procedure doIPAddrClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure doRenameCompClick(Sender: TObject);
    procedure doSmartClick(Sender: TObject);
    procedure doRebootClick(Sender: TObject);
    procedure doCloseDownClick(Sender: TObject);
    procedure doMemoryClick(Sender: TObject);
    procedure doMapDrivesClick(Sender: TObject);
    procedure doMapSCSIClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.doGetClassClick(Sender: TObject);
var
    rows, instances, I, J: integer ;
    WmiResults: T2DimStrArray ;
    OldCursor: TCursor ;
    errstr: string ;
begin
    doGetClass.Enabled := false ;
    OldCursor := Screen.Cursor ;
    Screen.Cursor := crHourGlass ;
    StatusBar.SimpleText := '' ;
    try
        ListView.Items.Clear ;
        for I := 0 to 12 do ListView.Columns [I].Width := 150 ;
        Application.ProcessMessages ;
        rows := MagWmiGetInfoEx (edtComputer.Text, edtNameSpace.Text, edtUser.Text,
                           edtPass.Text, edtClass.Text, WmiResults, instances, errstr) ;
        if rows > 0 then
        begin
            StatusBar.SimpleText := 'Instances: ' + IntToStr (instances) ;
            if instances >= ListView.Columns.Count then
                                        instances := ListView.Columns.Count - 1 ;
            for J := 0 to instances do
                            ListView.Columns.Items [J].Caption := WmiResults [J, 0] ;
            for I := 1 to rows do
            begin
                with ListView.Items.Add do
                begin
                    Caption := WmiResults [0, I] ;
                    for J := 1 to instances do
                        SubItems.Add (WmiResults [J, I]) ;
                end ;
            end ;
        end
        else if rows = -1 then
            StatusBar.SimpleText := 'Error: ' + errstr
        else
           StatusBar.SimpleText := 'Instances: None' ;
    finally
        doGetClass.Enabled := true ;
        Screen.Cursor := OldCursor ; 
        WmiResults := Nil ;
    end ;
end;

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;


procedure TForm1.doMBClick(Sender: TObject);
begin
    ResInfo.Text := MagWmiGetBaseBoard ;
end;

procedure TForm1.doBIOSClick(Sender: TObject);
begin
    ResInfo.Text := MagWmiGetSMBIOS ;
end;

procedure TForm1.doCommandClick(Sender: TObject);
var
    res: string ;
begin
    doCommand.Enabled := false ;
    ResInfo.Text := '' ;
    try
        if MagWmiGetOneQ (OneCommand.Text, OneProp.Text, res) > 0 then
                                                          ResInfo.Text := res ;
    finally
        doCommand.Enabled := true ;
    end ;
end;

procedure TForm1.doBootTimeClick(Sender: TObject);
var
    When: TDateTime ;
begin
    ResInfo.Text := '' ;
    When := MagWmiGetLastBootDT ;
    if When > 10 then ResInfo.Text := DateTimeToStr (When) ;
end;

procedure TForm1.doDiskSerialClick(Sender: TObject);
begin
    ResInfo.Text := MagWmiGetDiskSerial (DiskNum.Value) ;
end ;

procedure TForm1.doDiskModelClick(Sender: TObject);
begin
    ResInfo.Text := MagWmiGetDiskModel (DiskNum.Value) ;
end;

procedure TForm1.doIPAddrClick(Sender: TObject);
var
    adapter: string ;
    res, index: integer ;
    IPAddresses, SubnetMasks, IPGateways: StringArray;
    GatewayCosts: TIntegerArray ;
begin
    ResInfo.Text := '' ;
    adapter := '' ;
    SetLength (IPAddresses, 1) ;  // note, may be more than one address/mask
    SetLength (SubnetMasks, 1) ;
    SetLength (IPGateways, 1) ;
    SetLength (GatewayCosts, 1) ;
    IPAddresses [0] := IPAddress.Text ;
    SubnetMasks [0] := SubnetMask.Text ;
    IPGateways [0] := IPGateway.Text ;
    GatewayCosts [0] := 10 ;
    index := MagWmiFindAdaptor (adapter) ;  // looks for current Local Areas adaptor
    if index < 0 then
    begin
        ResInfo.Text := 'Can Not Find Single Adapter' ;
        exit ;
    end ;
    res := MagWmiNetSetIPAddr (index, IPAddresses, SubnetMasks) ;
    ResInfo.Text := adapter +  ' - Change IP Result: ' + IntToStr (res) ;
    if (res < 0) or (res > 1) then exit ;
    Res := MagWmiNetSetGateway (index, IPGateways, GatewayCosts) ;
    ResInfo.Text := adapter +  ' - Change IP Result: ' + IntToStr (res) ;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    NewCompName.Text := GetCompName ;
end;

procedure TForm1.doRenameCompClick(Sender: TObject);
var
    res: integer ;
begin
    if NewCompName.Text = GetCompName then exit ;
    if NewCompName.Text = '' then exit ;
    res := MagWmiRenameComp (NewCompName.Text, edtUser.Text, edtPass.Text) ;
    if res = 0 then
        ResInfo.Text := 'Rename Computer OK, Must Reboot Now'
    else
        ResInfo.Text := 'Rename Computer Failed: ' + IntToStr (res) ;
end;

function GetYN (value: boolean): string ;
begin
    result := 'No' ;
    if value then result := 'Yes' ;
end ;

procedure TForm1.doSmartClick(Sender: TObject);
var
    DriveResult: TDriveResult ;
    SmartResult: TSmartResult ;
    I: integer ;
begin
    ListView.Items.Clear ;
    StatusBar.SimpleText := '' ;
    if NOT MagWmiSmartDiskFail (DiskNum.Value, DriveResult, SmartResult) then
    begin
        ResInfo.Text := DriveResult.ErrInfo ;
        exit ;
    end ;
    with DriveResult do
    begin
        ResInfo.Text := 'IDE Model: ' + ModelNumber + ', Serial: ' + SerialNumber +
                                             ', Size = ' + IntToKbyte (CapacityNum, true) ;
    end;
    if SmartResult.TotalAttrs = 0 then
    begin
        ResInfo.Text := ResInfo.Text + ' No SMART Attributes Returned - ' + DriveResult.ErrInfo ;
        exit ;
    end ;
    StatusBar.SimpleText := 'SMART Attributes'  ;
    with ListView.Columns do
    begin
        Items [0].Caption := 'Attribute' ;
        Items [1].Caption := 'Name' ;
        Items [2].Caption := 'State' ;
        Items [3].Caption := 'Current' ;
        Items [4].Caption := 'Worst' ;
        Items [5].Caption := 'Threshold' ;
        Items [6].Caption := 'Raw Value' ;
        Items [7].Caption := 'Pre-Fail?' ;
        Items [8].Caption := 'Events?' ;
        Items [9].Caption := 'Error Rate?' ;
    end ;
    ListView.Columns [0].Width := 100 ;
    ListView.Columns [1].Width := 150 ;
    for I := 2 to 12 do ListView.Columns [I].Width := 75 ;
    with ListView.Items.Add do
    begin
        Caption := 'Temp' ;
        SubItems.Add (IntToStr (SmartResult.Temperature) + '°C') ;
    end;
//    ListView.Items.Add.Caption := 'Hours ' + IntToStr (SmartResult.HoursRunning) ;
    with ListView.Items.Add do
    begin
        Caption := 'Realloc Sec' ;
        SubItems.Add (IntToStr (SmartResult.ReallocSector)) ;
    end;
    with ListView.Items.Add do
    begin
        if SmartResult.SmartFailTot <> 0 then
        begin
            Caption := 'Result' ;
            SubItems.Add ('SMART Test Failed, Bad Attributes ' +
                                             IntToStr (SmartResult.SmartFailTot)) ;
        end
        else
        begin
            Caption := 'Result' ;
            SubItems.Add ('SMART Test Passed') ;
        end;
    end;
    for I := 0 to Pred (SmartResult.TotalAttrs) do
    begin
        with ListView.Items.Add do
        begin
            Caption := IntToStr (SmartResult.AttrNum [I]) ;
            SubItems.Add (SmartResult.AttrName [I]) ;
            SubItems.Add (SmartResult.AttrState [I]) ;
            SubItems.Add (IntToStr (SmartResult.AttrCurValue [I])) ;
            SubItems.Add (IntToStr (SmartResult.AttrWorstVal [I])) ;
            SubItems.Add (IntToStr (SmartResult.AttrThreshold [I])) ;
            SubItems.Add (IntToCStr (SmartResult.AttrRawValue [I])) ;
            SubItems.Add (GetYN (SmartResult.AttrPreFail [I])) ;
            SubItems.Add (GetYN (SmartResult.AttrEvents [I])) ;
            SubItems.Add (GetYN (SmartResult.AttrErrorRate [I])) ;
        end ;
    end ;
end;

procedure TForm1.doMapDrivesClick(Sender: TObject);
var
    totdrives, I: integer ;
    DriveResults: TDriveResults ;
begin
    ListView.Items.Clear ;
    with ListView.Columns do
    begin
        Items [0].Caption := 'Drive' ;
        Items [1].Caption := 'Device' ;
        Items [2].Caption := 'Bus' ;
        Items [3].Caption := 'Type' ;
        Items [4].Caption := 'Info' ;
        Items [5].Caption := 'SMART' ;
        Items [6].Caption := 'VendorId' ;
        Items [7].Caption := 'ProductId' ;
        Items [8].Caption := 'Serial' ;
        Items [9].Caption := 'Capacity' ;
        Items [10].Caption := 'ATA Version' ;
        Items [11].Caption := 'SATA Version' ;
        Items [12].Caption := 'Revision' ;
        Items [13].Caption := 'Removable' ;
        Items [14].Caption := 'Model' ;
        Items [15].Caption := 'Phys Sector' ;
    end;
    ListView.Columns [0].Width := 40 ;
    ListView.Columns [1].Width := 80 ;
    ListView.Columns [2].Width := 40 ;
    ListView.Columns [3].Width := 60 ;
    ListView.Columns [4].Width := 100 ;
    ListView.Columns [5].Width := 60 ;
    for I := 6 to 15 do ListView.Columns [I].Width := 100 ;
    StatusBar.SimpleText := '' ;
    totdrives := MagWmiMapDrives (12, DriveResults) ;
    if totdrives = 0 then
    begin
        ResInfo.Text := 'No Drives Found - ' + DriveResults [0].ErrInfo ;
        exit ;
    end;
    for I := 0 to totdrives - 1 do
    begin
        with DriveResults [I], ListView.Items.Add do
        begin
            Caption := IntToStr (DriveNum) ;
            SubItems.Add (DeviceId) ;
            SubItems.Add (BusTypeDisp) ;
            SubItems.Add (DevTypeDisp) ;
            SubItems.Add (ErrInfo) ;
            SubItems.Add (GetYN (SmartSupport)) ;
            SubItems.Add (VendorId) ;
            SubItems.Add (ProductId) ;
            SubItems.Add (SerialNumber) ;
            SubItems.Add (IntToKbyte (CapacityNum, true)) ;
            SubItems.Add (AtaVersion) ;
            SubItems.Add (SataVersion) ;
            SubItems.Add (ProductRev) ;
            SubItems.Add (GetYN (RemoveMedia)) ;
            SubItems.Add (ModelNumber) ;
            SubItems.Add (IntToStr (SecSizePhysical)) ;
        end;
    end;
end;

procedure TForm1.doMapSCSIClick(Sender: TObject);
var
    totdrives, I: integer ;
    DriveResults: TDriveResults ;
begin
    ListView.Items.Clear ;
    with ListView.Columns do
    begin
        Items [0].Caption := 'Drive' ;
        Items [1].Caption := 'Device' ;
        Items [2].Caption := 'Bus' ;
        Items [3].Caption := 'Type' ;
        Items [4].Caption := 'Info' ;
        Items [5].Caption := 'SMART' ;
        Items [6].Caption := 'VendorId' ;
        Items [7].Caption := 'ProductId' ;
        Items [8].Caption := 'Serial' ;
        Items [9].Caption := 'Capacity' ;
        Items [10].Caption := 'ATA Version' ;
        Items [11].Caption := 'SATA Version' ;
        Items [12].Caption := 'Revision' ;
        Items [13].Caption := 'Removable' ;
        Items [14].Caption := 'Model' ;
        Items [15].Caption := 'Phys Sector' ;
    end;
    ListView.Columns [0].Width := 40 ;
    ListView.Columns [1].Width := 80 ;
    ListView.Columns [2].Width := 40 ;
    ListView.Columns [3].Width := 60 ;
    ListView.Columns [4].Width := 100 ;
    ListView.Columns [5].Width := 60 ;
    for I := 6 to 15 do ListView.Columns [I].Width := 100 ;
    StatusBar.SimpleText := '' ;
    totdrives := MagWmiSmartScsiBus (12, DriveResults) ;
    if totdrives = 0 then
    begin
        ResInfo.Text := 'No Drives Found - ' + DriveResults [0].ErrInfo ;
        exit ;
    end;
    for I := 0 to totdrives - 1 do
    begin
        with DriveResults [I], ListView.Items.Add do
        begin
            Caption := IntToStr (DriveNum) ;
            SubItems.Add (DeviceId) ;
            SubItems.Add (BusTypeDisp) ;
            SubItems.Add (DevTypeDisp) ;
            SubItems.Add (ErrInfo) ;
            SubItems.Add (GetYN (SmartSupport)) ;
            SubItems.Add (VendorId) ;
            SubItems.Add (ProductId) ;
            SubItems.Add (SerialNumber) ;
            SubItems.Add (IntToKbyte (CapacityNum, true)) ;
            SubItems.Add (AtaVersion) ;
            SubItems.Add (SataVersion) ;
            SubItems.Add (ProductRev) ;
            SubItems.Add (GetYN (RemoveMedia)) ;
            SubItems.Add (ModelNumber) ;
            SubItems.Add (IntToStr (SecSizePhysical)) ;
        end;
    end;
end;

procedure TForm1.doRebootClick(Sender: TObject);
var
    S: string ;
begin
    ResInfo.Text := '' ;
    if edtComputer.Text = '.' then edtComputer.Text := GetCompName ;
    if Application.MessageBox (PChar ('Confirm Reboot PC ' + edtComputer.Text + ' Now'),
                             'WMI - Reboot PC', MB_OKCANCEL) <> IDOK then exit  ;
    if MagWmiCloseWin (edtComputer.Text, edtUser.Text, edtPass.Text,true, S) = 0 then
                                                                S := 'PC Reboot Accepted' ;
    ResInfo.Text := S ;
end;

procedure TForm1.doCloseDownClick(Sender: TObject);
var
    S: string ;
begin
    ResInfo.Text := '' ;
    if edtComputer.Text = '.' then edtComputer.Text := GetCompName ;
    if Application.MessageBox (Pchar ('Confirm Power Down PC ' + edtComputer.Text + ' Now'),
                             'WMI - Power Down PC', MB_OKCANCEL) <> IDOK then exit  ;
    if MagWmiCloseWin (edtComputer.Text, edtUser.Text, edtPass.Text,false, S) = 0 then
                                                         S := 'PC Power Down Accepted' ;
    ResInfo.Text := S ;
end;

procedure TForm1.doMemoryClick(Sender: TObject);
var
    WmiMemoryRec: TWmiMemoryRec ;
begin
    ResInfo.Text := '' ; 
    WmiMemoryRec := MagWmiGetMemory ;
    with WmiMemoryRec do
    begin
        ResInfo.Text := 'FreePhysicalMemory: ' + IntToKbyte (FreePhysicalMemory) +
            ', FreeSpaceInPagingFiles: ' + IntToKbyte (FreeSpaceInPagingFiles) +
            ', FreeVirtualMemory: ' + IntToKbyte (FreeVirtualMemory) ; 
      { SizeStoredInPagingFiles
        TotalSwapSpaceSize
        TotalVirtualMemorySize
        TotalVisibleMemorySize  }
    end ;
end;

end.
