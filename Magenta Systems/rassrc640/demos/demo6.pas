unit demo6;
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{

This application demos the TMagRasxxx components.  While they may be
installed in the component library and dropped on a form, this program
creates the components in code.

Please note this is not intended to be a fully functioning application
and may not be distributed as such.

Created by Angus Robertson, Magenta Systems Ltd, England
in early 1998, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright 1998-2015, Magenta Systems Ltd

Last updated: 6th June 2017 - Release 6.40


4.50 - removed StateChanged event handler and replaced it with StateEvent
which uses a message queue to buffer callback events that caused re-entrancy
and which fixes the long term cosmetic problem in this demo program where a
second row appeared incorrectly in the ListView

4.51 - added Encryption Type = Optional which is default for W2K
set SubEntry to 0 before dialling to dial all subentries with multilink

4.60 - added Ras Admin/Server component, on new RAS Server tab
added rasphone.pbk support to create connection entry list, 100 times faster than
    using RAS APIs on NT4/W2K, shown on new Entry List tab
now using phonebook path/name when getting and putting entries to make sure
  they go back where they came from (paths also shown when editing)
new entries on W2K can be specified to go into user or allusers phonebooks

4.61 - show network card MAC Address

4.62 - support for Windows XP RC1 build 2505
Online listView shows connection Flags and LUID (for multiple logons)
Full Properties has new XP tab showing new entry options
Added new Network tab showing some stuff from the IP Helper API including
    dynamic DNS addresses for the PC

4.70 - added GetNetworkAlive using IsDestinationReachable and
    IsNetworkAlive APIs, only work with MSIE5 and later, and then not always
Added support for some Internet Options, set auto dial and default connection
Tested on Windows XP final build 2600.

4.80 - Added connection desktop shortcut, minor changes for Ras Server

4.90 - Added ISDN link setting when connecting a call to determine whether all, one
       or both links are initially dialled.  A second link may be started later.
       Note that not all ISPs will allow a second link to be added later, and there
       are currently cosmetic problems in this demo monitoring the second link
Added Broadband phone book type for XP

4.91 - Show a RAS error if GetDeviceList fails
Tested briefly on Windows 2003 beta build 3604, no surprises except 260
  devices listed, mostly VPN, might disappear for final release as happened with W2K

4.94 - improved MSIE auto dial options, display multiple connections on XP

5.00 - IpHlp functions removed, they have their own demo app
       validate DialLink to avoid using SubEntry of -1

5.10 - Initial support for Windows Vista beta, not fully tested

5.20 - Finally tested on Windows Vista RTM and Longhorn (2008 Server) beta 3
Moved Windows version information to Network tab
Use IsProgAdmin to show whether application has admin rights and can therefore
  edit 'All User' phonebook entries, Vista apps don't normally have admin rights
Removed phonebook file path setting now done in the components
Quick New now allows phonebook location to be set
Create Entry and Edit Entry button now bring up dialogs in Vista
Added DUN Dialog and Dial Dialog to display windows dialogs

5.30 - Added 'Save logon for anyone who uses this computer' tickbox when
saving a logon, this may be needed as well as setting Phonebook Location to
all users on Vista.

6.00 - support RAS WideChar APIs and Unicode for Delphi 2009 with new MagRasxxxW units
       removed support for Win9x and NT4, both 10 years old now
       these units may be compiled under Delphi 7 to 2009, but only support
       Unicode properties under Delphi 2009 where String=UnicodeString

6.10 - No changes in demo

6.20 - fixed a cast warning for Delphi 2009 and later

6.30 - Set default dialling location if blank to avoid dialog appearing

6.31 - removed incoming call monitoring with TMagRasAdm, not supported since NT4
Recognise Windows 2012, MagRasOSVersion returns OS7 and OS8 but check >= OSVista
Clean up removing old Win9x and NT4 code

6.33 - added manifest so Windows 10 is detected correctly

6.40 - no longer need manifest for Windows 10 since GetOSInfo works better now
       increased window size since we all have massive screen nowadays
       Windows 10 version 1703 (March 2017) broke RAS modem dialling, using
         new function to fix registry, pops up dialog if reboot needed.
       Beware Microsoft could fix this bug in a different way so this change
         may need reviewing in a few months.



}

interface

uses
  Windows, Messages, SysUtils, Classes, {Graphics,} Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, ComObj, Mask, Spin,
{$IF CompilerVersion >= 24}
  System.UITypes,
{$IFEND}
 // RAS components and literals
  MagRasConW, MagRasPerW, MagRasEdtW, MagRasEntW, MagRasApiW, magsubs1, magsubs2 ;

type
  TMainForm = class(TForm)
    ConnList: TListBox;
    Label1: TLabel;
    Label4: TLabel;
    Status: TStatusBar;
    Timer: TTimer;
    doExit: TButton;
    Debug: TCheckBox;
    SaveDump: TSaveDialog;
    doConnect: TButton;
    doDisConn: TButton;
    doStat: TButton;
    OpenScript: TOpenDialog;
    OptSpeaker: TCheckBox;
    DialLink: TComboBox;
    Label60: TLabel;
    MainPages: TPageControl;
    TabSheet1: TTabSheet;
    ConInfoList: TListView;
    ConDevList: TListView;
    ConSpeedList: TListView;
    ConnLog: TMemo;
    TabSheet2: TTabSheet;
    LabelRasDevices: TLabel;
    LabelDefPhonebook: TLabel;
    LabelFileConns: TLabel;
    DeviceList: TListBox;
    GroupBox1: TGroupBox;
    doCreateConn: TButton;
    doEditConn: TButton;
    doDUNDialog: TButton;
    doDialDialog: TButton;
    GroupBox2: TGroupBox;
    doRenameConn: TButton;
    doCopyConn: TButton;
    doDeleteConn: TButton;
    doShortcut: TButton;
    TabSheet3: TTabSheet;
    Label41: TLabel;
    doLogonUpdate: TButton;
    doDumpEntSome: TButton;
    SBoxLogon: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label49: TLabel;
    ConnUser: TEdit;
    ConnPw: TEdit;
    SBoxDial: TGroupBox;
    LabelCan: TLabel;
    ConnPhone: TLabel;
    LabelCountry: TLabel;
    Label10: TLabel;
    ConnDialNum: TLabel;
    Label7: TLabel;
    LabelPhonebookPath: TLabel;
    ConnCanonical: TEdit;
    AltNumList: TMemo;
    SBoxDevice: TGroupBox;
    DeviceType: TLabel;
    DeviceName: TLabel;
    DevicePort: TLabel;
    LabelSubEnt: TLabel;
    LabelModemInfo: TLabel;
    LabelDialMode: TLabel;
    TabSheet4: TTabSheet;
    doPropNew: TButton;
    doPropLoad: TButton;
    doPropSave: TButton;
    FullPropsPages: TPageControl;
    TabDial: TTabSheet;
    Label5: TLabel;
    LocationBox: TGroupBox;
    Label6: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    LabelNumberDisp: TLabel;
    LabelNumberDial: TLabel;
    Label44: TLabel;
    entUseCountryandAreaCodes: TCheckBox;
    entCountryName: TComboBox;
    entAreaCode: TEdit;
    entLocalNumber: TEdit;
    entCanonNumber: TEdit;
    entAlternates: TMemo;
    entPromoteAlternates: TCheckBox;
    entCountryCode: TEdit;
    doPropDial: TButton;
    entCountryId: TEdit;
    entEntryName: TEdit;
    TabLogon: TTabSheet;
    DeviceBox: TGroupBox;
    Label15: TLabel;
    LabelPort: TLabel;
    Label17: TLabel;
    Label32: TLabel;
    entDeviceType: TEdit;
    entDevicePort: TEdit;
    entDeviceName: TComboBox;
    entIdleDisconnectSeconds: TSpinEdit;
    entIdleOption: TRadioGroup;
    LogonBox: TGroupBox;
    Label16: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    entUsername: TEdit;
    entPassword: TEdit;
    entDomain: TEdit;
    entCallBackNumber: TEdit;
    entDefaultCreds: TCheckBox;
    AutoDialBox: TGroupBox;
    Label21: TLabel;
    Label22: TLabel;
    entAutoDialDll: TEdit;
    entAutoDialFunc: TEdit;
    TabProtocol: TTabSheet;
    entFramingProtocol: TRadioGroup;
    ProtocolBox: TGroupBox;
    entNetTCPIP: TCheckBox;
    entNetIPX: TCheckBox;
    entNetBEUI: TCheckBox;
    TCPIPBox: TGroupBox;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    entSpecificIPAddress: TCheckBox;
    entSpecificNameServers: TCheckBox;
    entHeaderCompression: TCheckBox;
    entRemoteDefaultGateway: TCheckBox;
    entIPAddress: TMaskEdit;
    entDNSAddress: TMaskEdit;
    entDNSAddressAlt: TMaskEdit;
    entWINSAddress: TMaskEdit;
    entWINSAddressAlt: TMaskEdit;
    entSlipFrameSize: TRadioGroup;
    SpecialBox: TGroupBox;
    entNetworkLogon: TCheckBox;
    entDisableLCPExtensions: TCheckBox;
    entSoftwareCompression: TCheckBox;
    entTerminalAfterDial: TCheckBox;
    entTerminalBeforeDial: TCheckBox;
    entModemLights: TCheckBox;
    TabSecurity: TTabSheet;
    PasswordBox: TGroupBox;
    Label33: TLabel;
    entRequireEncryptedPassword: TCheckBox;
    entRequireMSEncryptedPassword: TCheckBox;
    entRequireDataEncryption: TCheckBox;
    entUseLogonCredentials: TCheckBox;
    entRequireEAP: TCheckBox;
    entRequirePAP: TCheckBox;
    entRequireSPAP: TCheckBox;
    entRequireCHAP: TCheckBox;
    entRequireMsCHAP: TCheckBox;
    entRequireMsCHAP2: TCheckBox;
    entRequireW95MSCHAP: TCheckBox;
    entCustom: TCheckBox;
    entEncryptionType: TRadioGroup;
    entCustomAuthKey: TSpinEdit;
    X25Box: TGroupBox;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    entX25PadType: TEdit;
    entX25Address: TEdit;
    entX25UserData: TEdit;
    entX25Facilities: TEdit;
    TabScript: TTabSheet;
    Label51: TLabel;
    LabelPhonebookPathFull: TLabel;
    ScriptBox: TGroupBox;
    Label27: TLabel;
    entScript: TEdit;
    doScriptOpen: TButton;
    doScriptView: TButton;
    ViewScript: TMemo;
    TabMultilink: TTabSheet;
    MultilinkBox: TGroupBox;
    Label34: TLabel;
    Label42: TLabel;
    entSubEntries: TEdit;
    entISDNChannels: TEdit;
    MultilinkList: TListView;
    BAPBox: TGroupBox;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    entDialMode: TRadioGroup;
    entHangUpExtraPercent: TSpinEdit;
    entDialExtraPercent: TSpinEdit;
    entDialExtraSampleSeconds: TSpinEdit;
    entHangUpExtraSampleSeconds: TSpinEdit;
    TabW2000: TTabSheet;
    BoxExtras: TGroupBox;
    Label39: TLabel;
    Label40: TLabel;
    entSecureLocalFiles: TCheckBox;
    entPreviewPhoneNumber: TCheckBox;
    entSharedPhoneNumbers: TCheckBox;
    entPreviewUserPw: TCheckBox;
    entPreviewDomain: TCheckBox;
    entShowDialingProgress: TCheckBox;
    entPType: TRadioGroup;
    entVpnStrategy: TRadioGroup;
    entguidId: TEdit;
    entCustomDialDll: TEdit;
    entPhoneBook: TRadioGroup;
    TabSheet8: TTabSheet;
    GroupBox6: TGroupBox;
    Label52: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    entSecureFileAndPrint: TCheckBox;
    entDontNegotiateMultilink: TCheckBox;
    entSecureClientForMSNet: TCheckBox;
    entDontUseRasCredentials: TCheckBox;
    entUsePreSharedKey: TCheckBox;
    entUseGlobalDeviceSettings: TCheckBox;
    entDisableNbtOverIP: TCheckBox;
    entInternet: TCheckBox;
    entReconnectIfDropped: TCheckBox;
    entSharePhoneNumbers: TCheckBox;
    entTcpWindowSize: TSpinEdit;
    entDnsSuffix: TEdit;
    entPrerequisitePbk: TEdit;
    entPrerequisiteEntry: TEdit;
    entRedialCount: TSpinEdit;
    entRedialPause: TSpinEdit;
    doPropDump: TButton;
    doPropCopy: TButton;
    TabSheet5: TTabSheet;
    Label46: TLabel;
    Label48: TLabel;
    GroupBox3: TGroupBox;
    Label43: TLabel;
    Label45: TLabel;
    Label9: TLabel;
    quickUserName: TEdit;
    quickPassword: TEdit;
    quickPhoneBook: TRadioGroup;
    quickDefaultCreds: TCheckBox;
    quickEntryName: TEdit;
    GroupBox4: TGroupBox;
    Label47: TLabel;
    quickDeviceName: TComboBox;
    GroupBox5: TGroupBox;
    Label53: TLabel;
    qLabelNumberDisp: TLabel;
    qLabelNumberDial: TLabel;
    quickCanonNumber: TEdit;
    doQuickClear: TButton;
    doQuickCreate: TButton;
    TabSheet7: TTabSheet;
    LabelEntryRes: TLabel;
    doRefreshEntries: TButton;
    EntriesList: TListView;
    EntryUseAPI: TCheckBox;
    TabSheet9: TTabSheet;
    GroupBox7: TGroupBox;
    LabelMACAddr: TLabel;
    LabelNetAlive: TLabel;
    LabelQOS: TLabel;
    GroupBox8: TGroupBox;
    Label59: TLabel;
    optMSIEDefConn: TComboBox;
    doMSIEUpdate: TButton;
    optMSIEAutDial: TRadioGroup;
    GroupBox9: TGroupBox;
    LabelRASVer: TLabel;
    LabelWinVer: TLabel;
    LabelVersion: TLabel;
    LabelAdmin: TLabel;
    procedure ConnListClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    Procedure StateEvent (Sender: TObject; CallState: TRasStateRec) ;
    procedure TimerTimer(Sender: TObject);
    procedure doConnectClick(Sender: TObject);
    procedure doCreateConnClick(Sender: TObject);
    procedure doDeleteConnClick(Sender: TObject);
    procedure doDisConnClick(Sender: TObject);
    procedure doEditConnClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure doLogonUpdateClick(Sender: TObject);
    procedure doRenameConnClick(Sender: TObject);
    procedure CanonNumberChange(Sender: TObject);
    procedure ConnCanonicalChange(Sender: TObject);
    procedure NumberChanged(Sender: TObject);
    procedure doCopyConnClick(Sender: TObject);
    procedure doDumpEntSomeClick(Sender: TObject);
    procedure doPropCopyClick(Sender: TObject);
    procedure doPropDialClick(Sender: TObject);
    procedure doPropDumpClick(Sender: TObject);
    procedure doPropLoadClick(Sender: TObject);
    procedure doPropNewClick(Sender: TObject);
    procedure doPropSaveClick(Sender: TObject);
    procedure doQuickClearClick(Sender: TObject);
    procedure doQuickCreateClick(Sender: TObject);
    procedure doScriptOpenClick(Sender: TObject);
    procedure doScriptViewClick(Sender: TObject);
    procedure doStatClick(Sender: TObject);
    procedure entCountryNameChange(Sender: TObject);
    procedure entDeviceNameChange(Sender: TObject);
    procedure entUseCountryandAreaCodesClick(Sender: TObject);
    procedure quickCanonNumberChange(Sender: TObject);
    procedure doRefreshEntriesClick(Sender: TObject);
    procedure optMSIEDefConnDropDown(Sender: TObject);
    procedure doMSIEUpdateClick(Sender: TObject);
    procedure doShortcutClick(Sender: TObject);
    procedure doDUNDialogClick(Sender: TObject);
    procedure doDialDialogClick(Sender: TObject);

  private
    { Private declarations }

    procedure ClearRows ;
    procedure RefreshConns ;
    procedure GetProperties ;
    procedure PutProperties ;
    function NewConn: boolean ;
    procedure GetDialProps ;
    procedure AddLog (info: string) ;
    function GetSelItem: integer ;
    procedure GetNetworkAlive ;


  public
    { Public declarations }
  end;

const
    LF = #10 ;
    CR = #13 ;
    CRLF: PChar = CR + LF ;
    DirScriptsW9x = 'c:\program files\accessories' ;
    DirScriptsNT  = 'c:\winnt\system32\ras' ;
var
    MainForm: TMainForm;
    MgRasCon: TMagRasCon ;      // the RAS Connections component
    MgRasPer: TMagRasPer ;      // the RAS Performance component
    MgRasEdt: TMagRasEdt ;      // the RAS Edit Properties component
    StopFlag: boolean ;         // if true, stop connection in progress
    heap: THeapStatus ;
    HandleList: array [0..MaxConnections-1] of HRasConn ; // keep track of display handles
    ParHandList: array [0..MaxConnections-1] of HRasConn ; // keep track of parent handles
    SubEntList: array [0..MaxConnections-1] of integer ;  // and matching subentries
    OldStatusList: array [0..MaxConnections-1] of integer ;   // to see if anything changed
    PerfNrList: array [0..MaxConnections-1] of integer ;   // where to get statistics from
    NewHandle: HRasConn ;         // when dialling a new call, cleared when connected
    NewConnName: string ;         // when dialling a new call
    PendSubFlag: boolean = false ;// while connecting, not got subentries yet
    NewDispNum: string ;          // when dialling a call, pseduo number to display
    NewDialNum: string ;          // when dialling a call, real number to dial
    NewDialTick: DWORD ;          // when dialling a call, the starting tick to calculate length
    ListTotRows: integer ;        // number of rows being displayed on ListViews
    DialProps: TDialProps;        // dialling properties
    DialLocation: TDialLocation;  // default dialling location
    DialCard: TDialCard ;         // default dialling calling card
    DialCountry: TDialCountry ;   // default country dialling info
    CurSelItem: integer ;         // current selected item for editing
    ProgAdmin: boolean ;        // 5.20 does program have administrator rights

implementation

{$R *.DFM}

// word wrap a caption allowing special file name and URL symbols for breaking

procedure WrapCaption (CapText: string; CapLabel: TLabel) ;
var
    maxcol: integer ;
begin
    if CapText <> '' then
    begin
        maxcol := (CapLabel.Width * Length (CapText)) div
                                        CapLabel.Canvas.TextWidth (CapText) ;
        CapLabel.Caption := WrapText (CapText, #13#10,
                                 [' ', '-', #9, '/', '\', '.', '?'], maxcol) ;
    end
    else
        CapLabel.Caption := CapText ;
end ;

procedure TMainForm.AddLog (info: string) ;
begin
    ConnLog.Lines.Add (FormatDateTime('hh:nn:ss:zzz', Time) + ' ' + info) ;
end ;

// get all TAPI dialling properties

procedure TMainForm.GetDialProps ;
begin
    MgRasCon.GetTransCaps (DialProps, DialLocation, DialCard, DialCountry) ;
end ;

procedure TMainForm.FormCreate(Sender: TObject);
var
    I, J, ret: integer ;
    info: string ;
//    tempcountries: TStringList ;
    MacAddresses: TStringList ;
    DialupBroken, RebootNeeded, Modemflag: boolean;
begin
    MainPages.ActivePage := TabSheet1 ;
    MgRasCon := TMagRasCon.Create (Self) ;      // create RAS Connection component
    MgRasPer := TMagRasPer.Create (Self) ;      // create RAS Performance component
    MgRasEdt := TMagRasEdt.Create (Self) ;      // create RAS Edit Properties component
    MgRasCon.OnStateEvent := StateEvent ;       // install event handler - old version
    LabelVersion.Caption := MgRasCon.Version ;
    LabelWinVer.Caption := 'Windows: ' + GetOSVersion ;
    LabelRASVer.Caption := 'No RAS' ;
    ProgAdmin := IsProgAdmin ; // 5.20 find if program has administrator access
    if ProgAdmin then
        LabelAdmin.Caption := 'Program has Administrator Rights'
    else
        LabelAdmin.Caption := 'Program does not have Administrator Rights' ;
    MacAddresses := TStringList.Create ;
    try
        GetMACAddresses ('', MacAddresses) ; // 4.61
        if MacAddresses.Count > 0 then
            LabelMACAddr.Caption := 'MAC Address: ' + MacAddresses [0] ;
    finally
        MacAddresses.Free ;
    end ;
    if MgRasCon.TestRAS then
    begin
        LabelRASVer.Caption := 'RAS: ' + MgRasCon.DUNInfo + ' (' +
                                          MgRasCon.DUNVersion + ')' ;
        RefreshConns ;       // build connection entry list
        for I := 0 to Pred (MaxConnections) do
        begin
            HandleList [I] := 0 ;
            SubEntList [I] := 0 ;
        end ;

    // get list of RAS capable modems
        Modemflag := false;
        ret := MgRasCon.GetDeviceList ;
        if ret = 0 then
        begin
            if MgRasCon.DeviceNameList.Count <> 0 then
            begin
                for I := 0 to MgRasCon.DeviceNameList.Count - 1 do
                begin
                    info := LowerCase (MgRasCon.DeviceTypeList [I]) ;
                    DeviceList.Items.Add (MgRasCon.DeviceNameList [I] +
                                                           ' (' + info + ')') ;
                    if info = 'modem' then Modemflag := true ;
                end ;
            end ;
            LabelRasDevices.Caption := 'RAS Capable Modems and ISDN Cards, Total ' +
                                            IntToStr (MgRasCon.DeviceNameList.Count) ;
        end
        else
            DeviceList.Items.Add ('Failed to Get RAS Devices - ' +
                                                    MgRasCon.GetErrorString(ret)) ;

    // 6.4 - Windows 10 version 1703 (March 2017) broke RAS modem dialling,
    // check registry for new RequiredPrivileges
        if Modemflag then
        begin
            info := MgRasCon.FixWin10Ras (DialupBroken, RebootNeeded) ;
            if info <> '' then
            begin
                 ShowMessage ('Windows 10 modem dialling was broken in version 1703' +
                    CRLF + CRLF + info) ;
            end;
        end;

    // get dialling properties, location, calling card, etc, then DUN version
        entCountryId.Text := '' ;   // stop numeric conversion errors ;
        GetDialProps ;

    // get NT4/W2K rasphone.pbk file
        LabelDefPhonebook.Caption := MagRasPhoneFiles [0] ;   // 4.60
        LabelFileConns.Caption := 'Default Phonebook (with ' +
                           IntToStr (MagRasGetPhoneBookFiles) + ' entries):' ;

    // default dialogs
        SaveDump.InitialDir := ExtractFileDir (Application.ExeName) ;
        try
            OpenScript.InitialDir := DirScriptsNT ;
        except
            OpenScript.InitialDir := '' ;
        end ;

    // get country list - for editing properties
        MgRasEdt.GetAllCountryInfo ;
     {   tempcountries := TStringList.Create ;   // save a country list, for documentation!
        with MgRasEdt do
        begin
            for I := 0 to CountryList.Count - 1 do
                    tempcountries.Add (CountryList [I] + ' ID=' +
                                            IntToStr (CountryIds [I])) ;
        end ;
        tempcountries.Sort ;
        tempcountries.SaveToFile ('countries.txt') ;
        tempcountries.Free ;   }

    // create ListView rows
        for I := 0 to MaxConnections - 1 do
        begin
            with ConInfoList.Items.Add do
            begin
                Caption := '' ;
                for J := 0 to 3 do SubItems.Add ('') ;
            end ;
            with ConDevList.Items.Add do
            begin
                Caption := '' ;
                for J := 0 to 10 do SubItems.Add ('') ;
            end ;
            with ConSpeedList.Items.Add do
            begin
                Caption := '' ;
                for J := 0 to 6 do SubItems.Add ('') ;
            end ;
        end ;
        ListTotRows := 0 ;
        NewDialTick := 0 ;

    // initial settings
        doQuickClearClick (self) ;  // default Quick New connection
        NewConn ;                   // default Full Props for new
        Timer.Enabled := true ;
    end
    else
    begin
        ConnList.Items.Add (MgRasCon.StatusStr) ;   // no RAS available
        Status.Panels[0].Text := MgRasCon.StatusStr ;
    end ;
    GetNetworkAlive ;
    if MSIEAutoDialOpt (I, false) then optMSIEAutDial.ItemIndex := I ;
    if MSIEDefConn (info, false) then
        optMSIEDefConn.Text := info
    else
        optMSIEDefConn.Text := '' ; 
end;

// clear ListView rows

procedure TMainForm.ClearRows ;
var
    I, J: integer ;
begin
    inc (ListTotRows) ;
    if ListTotRows > MaxConnections then ListTotRows := MaxConnections ;
    for I := 0 to ListTotRows - 1 do
    begin
        with ConInfoList.Items [I] do
        begin
            Caption := '' ;
            for J := 0 to 3 do SubItems [J] := '' ;
        end ;
        with ConDevList.Items [I] do
        begin
            Caption := '' ;
            for J := 0 to 10 do SubItems [J] := '' ;
        end ;
        with ConSpeedList.Items [I] do
        begin
            Caption := '' ;
            for J := 0 to 6 do SubItems [J] := '' ;
        end ;
    end ;
    ListTotRows := 0 ;
end ;

procedure TMainForm.ConnCanonicalChange(Sender: TObject);
begin
    MgRasCon.TranslateAddr (0, ConnCanonical.Text, NewDispNum, NewDialNum) ;
    ConnPhone.Caption := 'Display Number: ' + NewDispNum ;
    ConnDialNum.Caption := 'Dialable Number: ' + NewDialNum ;
end;

procedure TMainForm.doExitClick(Sender: TObject);
var
    key, I: integer ;
    closeflag: boolean ;
begin
    Timer.Enabled := false ;        // stop connection checks

// see if any connections still open
    closeflag := false ;
    for I := 0 to MaxConnections - 1 do
    begin
        if (HandleList [I] <> 0) then closeflag := true ;
    end ;
    if closeflag then
    begin
        key := MessageDlg ('Close Down Dial-Up Connection?',
                                 mtConfirmation, mbYesNoCancel, 0) ;
        if key = mrCancel then
        begin
            Timer.Enabled := true ;
            exit ;
        end ;
        if key = mrYes then
        begin
            for I := 0 to MaxConnections - 1 do
            begin                                    // no events
                if (HandleList [I] <> 0) then
                begin
                     AddLog ('Hanging-up Handle=' + IntToStr (HandleList [I])) ;
                     MgRasCon.DisconnectEx (HandleList [I], SubEntList [I], 3000, false) ;
                end ;
            end ;
        end ;
    end ;
    try
        MgRasCon.Free ;
        MgRasPer.Free ;
        MgRasEdt.Free ;
    except
    end ;
    Application.Terminate ;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    doExitClick (sender) ;
end;

// event handler called by TRAS when connection status changes
// this version runs from a queue so RAS is not suspended

procedure TMainform.StateEvent (Sender: TObject; CallState: TRasStateRec) ;
var
    info: string ;
    row, I, unique, duration: integer ;
    debugflag: boolean ;
begin
    debugflag := Debug.Checked ;
    if CallState.StateSubEntry <= 0 then CallState.StateSubEntry := 1 ;
    case CallState.StateEventSource of
        SourceDial: info := 'Dial' ;
        SourceStatus: info := 'Status' ;
        SourceHangup: info := 'Hangup' ;
    end ;

// check got a valid handle - major trouble!
    if CallState.StateRasConn = 0 then
    begin
        if debugflag then
            AddLog ('State Change, No Handle, ' + info + ' - ' + CallState.StatusStr) ;
        exit ;
    end ;

// ignore events after call finished
    if (CallState.ConnectState = ERROR_INVALID_PORT_HANDLE) then
    begin
        if debugflag then
                AddLog ('Ignored Event, ' + info + ' - ' + CallState.StatusStr) ;
        exit ;
    end ;

// find connection by checking handle, or parent and subentry during dialling
// check for subentry, then parent during dialling or subentry during status
    row := 0 ;
    if ListTotRows > 0 then
    begin
        for I := 0 to ListTotRows - 1 do
        begin
         {  if debugflag and (NewHandle <> 0) then   // testing when correct rows not found  !!!!
                    AddLog ('==Checking List ' + IntToStr (I) +
                     ' for statehandle=' + IntToStr (StateRasConn) +
                        ' parhandle=' + IntToStr (ParHandList [I]) +
                            ' subhandle=' + IntToStr (HandleList [I]) +
                                ', link=' + IntToStr (SubEntList [I])) ;  }
            if (CallState.StateSubEntry = SubEntList [I]) then
            begin
                if (CallState.StateRasConn = ParHandList [I]) or
                                    (CallState.StateRasConn = HandleList [I]) then
                begin
                    row := I + 1 ;
                    break ;
                end ;
            end ;
        end
    end
    else
    begin
      {  if debugflag then   // testing when correct rows not found  !!!!
                AddLog ('==No rows to check, ListTotRows=0') ;  }
   end ;

// see if need to do diag log
    if debugflag and (row > 0) then
    begin
        unique := Succ (CallState.ConnectState) * Succ (CallState.ConnectError)
                                 * Succ (Ord (CallState.StateEventSource)) ;
        if OldStatusList [row - 1] = unique then debugflag := false ;
        OldStatusList [row - 1] := unique ;
    end ;
    if debugflag then
    begin
        if NewDialTick <> 0 then
            duration := CallState.TickCount - NewDialTick
        else
            duration := 0 ;
        AddLog ('==' + info + ' Duration=' +   IntToStr (duration) +
            ' handle=' + IntToStr (CallState.StateRasConn) +
            ' link=' + IntToStr (CallState.StateSubEntry) + ' row=' +
            IntToStr (row) + ' state=' + IntToStr (CallState.ConnectState) +
            ' error=' + IntToStr (CallState.ConnectError) + ' ' + CallState.StatusStr) ;
    end ;

// on NT, a new call is not added to the connection list until connected
    if row = 0 then
    begin
        if NewHandle = 0 then
        begin
            if debugflag then
                AddLog ('Unable to Find Correct Connection' +
                                                    ' - ' + CallState.StatusStr) ;
            exit ;
        end ;

    // add connection and line being dialled to ListView yet
        HandleList [ListTotRows] := NewHandle ;
        ParHandList [ListTotRows] := NewHandle ;
        SubEntList [ListTotRows] := CallState.StateSubEntry ;
        ConInfoList.Items [ListTotRows].Caption := NewConnName ;
        inc (ListTotRows) ;
        row := ListTotRows ;   // correct one to display
        if debugflag then AddLog ('== Added Row ' +
            IntToStr (row) + ' for link ' + IntToStr (CallState.StateSubEntry)) ;
    end ;

// if dialling a call, see if connected or failed - must hang-up!!!
    if (NewHandle = ParHandList [row - 1]) then
    begin
        if CallState.ConnectState >= RASBase then
        begin
            doConnect.Enabled := true ;
            if (CallState.ConnectState <> RASCS_Connected) or StopFlag then
            begin
                AddLog ('Hanging-up Handle=' + IntToStr (NewHandle)) ;
                MgRasCon.DisconnectEx (NewHandle, 1, 3000, false) ;  // do not cause StateChanged
                Status.Panels[0].Text := 'Connection Terminated' ;
                beep ;
            end
            else
            begin
                Status.Panels[0].Text := 'Connection Opened OK' ;
            end ;
            NewHandle := 0 ;   // dialling completed, no more checks
        end ;
    end ;

// check able to display something
    if (row <= 0) or (row > ListTotRows) then
    begin
        AddLog ('ListView Bounds Error - ' + CallState.StatusStr) ;
        exit ;
    end ;

// update correct row in list view - check if same to avoid refresh
    try
        with ConInfoList.Items [row - 1] do
        begin
            if SubItems [0] <> CallState.StatusStr then
            begin
                AddLog (Caption + ' - ' + CallState.StatusStr) ;
                SubItems [0] := CallState.StatusStr ;
                SubItems [1] := IntToStr (CallState.StateSubEntry) ;
                Selected := true ;
            end ;
            if CallState.StateEventSource = SourceStatus then  // not for dial events
            begin
                if SubItems [2] <> CallState.ConnectPhoneNr then
                begin
                    SubItems [2] := CallState.ConnectPhoneNr ;
                    AddLog ('Connection Phone Nr: ' + CallState.ConnectPhoneNr) ;
                    AddLog ('Connection Device: ' + CallState.CurDevName +
                                               ' (' + CallState.CurDevType + ')') ;
                end ;
            end ;
        end ;
    except
        AddLog ('ListView Exception!!! - ' + CallState.StatusStr) ;
    end ;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
var
    I, J, K, L: integer ;
    subtot: DWORD ;
    curhandle: HRasConn ;
    ListSubHand: TSubHandList ;
begin
    Timer.Enabled := false ;
    try   // finally
    try   // except

// see if any connections are open
    MgRasCon.GetConnections ;               // check for active connections
    if MgRasCon.Connections.Count = 0 then  // no active connections
    begin
        if (ListTotRows <> 0) and (NewHandle = 0) then  // just gone offline
        begin
            if Debug.Checked then AddLog ('== No Connections') ;
            for I := 0 to MaxConnections - 1 do
            begin
                if HandleList [I] <> 0 then
                begin
                    AddLog ('Hanging-up Handle=' + IntToStr (HandleList [I])) ;
                    MgRasCon.IntDisconnectEx (HandleList [I]) ;  // clear handles
                end ;
            end ;
            beep ;
            AddLog ('') ;
            doDisConn.Enabled := false ;
            PendSubFlag := false ;
            Status.Panels[0].Text := '' ;
            MgRasPer.ResetPerfStats ;       // clear stats for next connection
            ClearRows ;
            NewDialTick := 0 ;
            NewConnName := '' ;
            for I := 0 to MaxConnections - 1 do
            begin
                HandleList [I] := 0 ;
                ParHandList [I] := 0 ;
                SubEntList [I] := 0 ;
                OldStatusList [I] := -1 ;
                PerfNrList [I] := 0 ;
            end ;
            GetNetworkAlive ;
        end
    end
    else
    begin

    // display info for all connections
        if NOT doDisConn.Enabled then doDisConn.Enabled := true ;
        if MgRasCon.ConnChangedFlag or PendSubFlag then
        begin
            if Debug.Checked then AddLog ('== Connection List Changed, TotConns=' +
                                                IntToStr (MgRasCon.Connections.Count)) ;
            PendSubFlag := false ;
            for I := 0 to MaxDevs do MgRasPer.PerfRasConn [I] := 0 ;
            for I := 0 to MaxConnections - 1 do
            begin
                HandleList [I] := 0 ;
                ParHandList [I] := 0 ;
                SubEntList [I] := 0 ;
                OldStatusList [I] := -1 ;
                PerfNrList [I] := 0 ;
            end ;
            ClearRows ;    // clear listview window, sets ListTotRows=0
            for I := 0 to MgRasCon.Connections.Count - 1 do
            begin
                MgRasCon.EntryName := MgRasCon.Connections.EntryName (I) ;
                if MgRasCon.GetEntryProps (false) <> 0 then // don't need subentries
                begin
                    AddLog ('Failed to Access ' +
                        MgRasCon.Connections.EntryName (I) + ' Properties') ;
                    continue ;
                end ;
                subtot := MgRasCon.TotSubEntries ;  // defined sub connections

            // dialling a single channel, don't attempt to show second channel yet
//                if (subtot > 1) and (MgRasCon.EntryName = NewConnName) and
//                                         (DialLink.ItemIndex >= 1) then subtot := 1 ;
                curhandle := MgRasCon.Connections.RasConn (I) ;
                ListSubHand [1] := 0 ;

        // see if any subhandles for multilink connections
                MgRasCon.GetSubHandles (curhandle, subtot, ListSubHand) ;
                subtot := ListSubHand [0] ;  // 4.90 keep actual channels connected
                if subtot < 1 then subtot := 1 ;                  // 4.90
                if (subtot <= 1) and (ListSubHand [1] = 0 ) then  // 4.90
                begin
                    subtot := 1 ;
                    ListSubHand [1] := curhandle ;
                end ;
                if Debug.Checked then AddLog
                         ('== ' + MgRasCon.Connections.EntryName (I) +
                               ' (' + IntToStr (subtot) + ' channel)') ;

            // keep handles and subentries, if more than one defined in phonebook
            // note they may not all have been dialled
             //   L := DialLink.ItemIndex ;  // see which link was dialled
             //   if L = 0 then L := 1 ;
             //   if L > subtot then L := subtot ;
                L := 1 ;
                for J := L to subtot do
                begin
                try
                    HandleList [ListTotRows] := ListSubHand [J] ;
                    ParHandList [ListTotRows] := curhandle ;
                    SubEntList [ListTotRows] := J ;
                    if Debug.Checked then AddLog
                         ('== Link= ' + IntToStr (J) +
                             ' ParHandle=' + IntToStr (curhandle) +
                                ' SubHandle=' + IntToStr (ListSubHand [J]) ) ;

                // show row ListView, one for each subentry
                    with ConInfoList.Items [ListTotRows]do
                    begin
                        Caption := MgRasCon.Connections.EntryName (I) ;
                        SubItems [0] := 'No Status Yet' ;
                        SubItems [1] := IntToStr (J) ;  // link
                    end ;

                // W2K keep handle and subentry for perf stats - base 1
                    MgRasPer.PerfRasConn [ListTotRows + 1] := curhandle ;
                    if subtot <> 1 then
                        MgRasPer.PerfSubEnt [ListTotRows + 1] := J
                    else
                        MgRasPer.PerfSubEnt [ListTotRows + 1] := 0 ;
                    PerfNrList [ListTotRows] := ListTotRows + 1 ;

                // show user what we found
                    with ConDevList.Items [ListTotRows] do
                    begin
                        if I < MgRasCon.Connections.Count then  // in case connection is closed already 4.90
                        begin
                            Caption := MgRasCon.Connections.EntryName (I) ;
                            SubItems [0] := MgRasCon.Connections.DeviceName (I) ;
                            SubItems [1] := MgRasCon.DevicePort ;
                            SubItems [2] := MgRasCon.Connections.DeviceType (I) ;
                            SubItems [3] := IntToStr (J) ;  // subentry
                            SubItems [4] := IntToStr (ListSubHand [J]) ;
                            SubItems [5] := IntToStr (curhandle) ;
                            SubItems [6] := MgRasCon.Connections.Phonebook (I) ;
                            SubItems [7] := GUIDToString
                                            (MgRasCon.Connections.guidEntry (I)) ;  //  W2K only
                            SubItems [8] := IntToStr (MgRasCon.Connections.Flags (I)) ;  // XP only
                            SubItems [9] := IntToCStr (MgRasCon.Connections.LogonSessId (I)) ; // XP only
                        end ;
                    end ;
                    ConSpeedList.Items [ListTotRows].Caption :=
                                        MgRasCon.Connections.EntryName (I) ;
                    inc (ListTotRows) ;   // base 0
                except
                    AddLog ('!!! Exception Adding New Row') ;
                end ;
                end ;
            end ;

            // now reshow any new connections
            if (NewHandle <> 0) then
            begin
                for I := 0 to ListTotRows - 1 do
                begin
                    if (HandleList [I] <> 0) and
                                        (ParHandList [I] = NewHandle) then
                    begin
                        if Debug.Checked then AddLog
                                ('== Started Status Request for ' +
                                                IntToStr (HandleList [I])) ;
                        MgRasCon.CurrentStatusEx (HandleList [I],
                                                            SubEntList [I]) ;
                    end ;
                end ;
            end ;  // loop for each connection
            if Debug.Checked then AddLog
                ('== Finished ListView Update, ' + IntToStr (ListTotRows) + ' rows') ;
        end ;

   // get performance info for all connections, but only display if connected
        MgRasPer.GetPerfStats ;

    // get status for each connection, then performance statistics
        for I := 0 to ListTotRows - 1 do
        begin
            if (HandleList [I] <> 0) and (ParHandList [I] <> NewHandle) then
            begin
                MgRasCon.CurrentStatusEx (HandleList [I], SubEntList [I]) ;

            // connected, show stats
                if (MgRasCon.ConnectState = RASCS_Connected) then
                begin

            // display performance statistics
                    K := PerfNrList [I] ;  // keep perf stats item number
                    if (K > MgRasPer.TotPorts) or (K < 0) then K := 0 ;
                    with ConSpeedList.Items [I] do
                    begin
                        Subitems [0] := IntToCStr (MgRasPer.PerfConnSpd [K]) ; // + ' bps' ;
                        Subitems [1] := IntToCStr (MgRasPer.PerfXmitCur [K]) ; // + ' chars' ;
                        Subitems [2] := IntToCStr (MgRasPer.PerfRecvCur [K]) ; // + ' chars' ;
                    // dynamic IP addresses and real device
                        if SubItems [3] = '' then
                        begin
                            if MgRasCon.GetProtocolEx (HandleList [I]) = 0 then
                            begin
                                Subitems [3] := MgRasCon.ConnProtocol ;
                                Subitems [4] := MgRasCon.ClientIP + ' > ' + MgRasCon.ServerIP ;
                                AddLog ('PPP Reply Message: ' + MgRasCon.PPPReplyMessage) ;
                            end ;
                        end ;
                    end ;
                    GetNetworkAlive ;
                end ;

            // disconnected, clear handles - might be one channel
                if (MgRasCon.LastError = ERROR_INVALID_PORT_HANDLE) or
                        (MgRasCon.ConnectState = RASCS_Disconnected) then
                begin
                    HandleList [I] := 0 ;  // stop further requests
                end ;
            end ;
        end ;
    end ;
    except
        AddLog ('Timer Exception!!!') ;
        Status.Panels[0].Text := 'Timer Exception!!!' ;
        beep ;
    end ;
    finally
        Timer.Enabled := true ;
    end ;
end;

// build and display main connection entry list for all phonebooks

procedure TMainForm.RefreshConns ;
var
    I: integer ;
begin
// build minimal entry list, name and phonebook path, Angus 4.60
    MagRasGetEntryList ('') ;
    ConnList.Items.Clear ;
    if MagRasNumEntryRec = 0 then exit ;
    for I := 0 to Pred (MagRasNumEntryRec) do
                    ConnList.Items.Add (MagRasEntryRecs [I].EntryName) ;
    if ConnList.Items.Count <> 0 then ConnList.ItemIndex := 0 ; // set first
    ConnListClick (self) ;                              // get connection info
    CurSelItem := -1 ;
end ;

// set selected entry, make sure it's legal, set component properties - 4.60

function TMainForm.GetSelItem: integer ;
begin
    result := -1 ;
    CurSelItem := ConnList.ItemIndex ;
    if CurSelItem = -1 then exit ;
    if CurSelItem >= MagRasNumEntryRec then
    begin
        CurSelItem := -1 ;
        beep ;
        Status.Panels[0].Text := 'Entry Not Found in List' ;
        exit ;
    end ;
    MgRasCon.EntryName := MagRasEntryRecs [CurSelItem].EntryName ;     // connection entry name, Angus 4.60
    MgRasCon.PBLocation := MagRasEntryRecs [CurSelItem].PBLocation ;   // phonebook location, Angus 5.20
    result := CurSelItem ;
end ;

// when a connection is clicked, get entry info

procedure TMainForm.ConnListClick(Sender: TObject);
var
    countryname: string ;
    countrycode: integer ;
begin
    Status.Panels[0].Text := '(Getting Entry Details)' ;
    ConnUser.Text := '' ;
    ConnPw.Text := '' ;
    LabelPhonebookPath.Caption := '' ;
    DeviceName.Caption := '' ;
    DeviceType.Caption := '' ;
    DevicePort.Caption := '' ;
    LabelSubEnt.Caption := '' ;
    LabelDialMode.Caption := '' ;
    ConnCanonical.Text := '' ;
    ConnPhone.Caption := '' ;
    ConnDialNum.Caption := '' ;
    LabelCountry.Caption := '' ;
    AltNumList.Lines.Clear ;

// get selected entry, sets MgRasCon properties
    if GetSelItem < 0 then exit ;

// get user connection parameters
    if MgRasCon.GetDialParams = 0 then
    begin
        ConnUser.Text := MgRasCon.UserName ;                    // display them
        ConnPw.Text := MgRasCon.Password ;
//        if NOT MgRasCon.PasswordFlag then Status.Panels[0].Text :=
//                                      'Warning - No Password Retrieved' ;
    end
    else
    begin
        beep ;
        Status.Panels[0].Text := MgRasCon.StatusStr ;
    end ;

// get device connection parameters
    if MgRasCon.GetEntryProperties = 0 then
    begin
        WrapCaption (MasRasPBLocationStr [MgRasCon.PBLocation] +
                            ' - ' + MgRasCon.PhoneBookPath, LabelPhonebookPath) ; // 5.20
        DeviceName.Caption := 'Device Name: ' + MgRasCon.DeviceName ;
        DeviceType.Caption := 'Device Type: ' + MgRasCon.DeviceType ;
        DevicePort.Caption := 'Device Port: ' + MgRasCon.DevicePort ;
        LabelSubEnt.Caption := 'Multiple Channels: ' +
                                        IntToStr (MgRasCon.TotSubEntries) ;
        LabelModemInfo.Caption := 'Modem Info: None' ;
        LabelDialMode.Caption := 'No Dial Mode' ;
        if MgRasCon.DialMode = RASEDM_DialAll then
                        LabelDialMode.Caption := 'Dial All Channels' ;
        if MgRasCon.DialMode = RASEDM_DialAsNeeded then
                            LabelDialMode.Caption := 'Dial As Needed' ;
        ConnCanonical.Text := MgRasCon.PhoneCanonical ;
        countryname := '' ;
        MgRasCon.GetOneCountryInfo (MgRasCon.CountryId, countrycode,
                                                            countryname) ;
        LabelCountry.Caption := 'Country: ' + countryname ;
        MgRasCon.TranslateAddr (0, ConnCanonical.Text,
                                                 NewDispNum, NewDialNum) ;
        ConnPhone.Caption := 'Phone Number: ' + NewDispNum ;
        ConnDialNum.Caption := 'Dial Number: ' + NewDialNum ;
        AltNumList.Lines.Assign (MgRasCon.AltPhoneNrList) ;
        Timer.Enabled := true ;  // not until RAS installed
        Status.Panels[0].Text := '' ;
    end
    else
    begin
        beep ;
        Status.Panels[0].Text := MgRasCon.StatusStr ;
    end ;
end;

// update entry, previously read by ConnListClick

procedure TMainForm.doLogonUpdateClick(Sender: TObject);
var
    longerr: integer ;
    oldPW: string ;
begin
    if CurSelItem < 0 then exit ;  // make sure entry is selected
    Status.Panels[0].Text := '(Saving Entry)' ;
    MgRasCon.EntryName := MagRasEntryRecs [CurSelItem].EntryName ;     // connection entry name, Angus 4.60
    MgRasCon.PBLocation := MagRasEntryRecs [CurSelItem].PBLocation ;   // phonebook location, Angus 5.20
    MgRasCon.UserName := ConnUser.Text ;
    MgRasCon.Password := ConnPw.Text ;
    oldPW := MgRasCon.Password ;
    MgRasCon.PhoneCanonical := ConnCanonical.Text ;
    longerr := MgRasCon.SetDialParams ;
    if longerr = 0 then longerr := MgRasCon.UpdatePhonebook ;
    if longerr = 0 then
        Status.Panels[0].Text := 'Connection Updated'
    else
        Status.Panels[0].Text := MgRasCon.StatusStr ;
end;

// this proc dials a connection, but does not wait

procedure TMainForm.doConnectClick(Sender: TObject);
var
    err: integer ;
    SpeakerMode: TSpeakerMode ;
begin
    MainPages.ActivePage := TabSheet1 ;
// get selected entry
    if GetSelItem < 0 then exit ;
    Status.Panels[0].Text := 'Starting Connection' ;

    AddLog (NewConnName + ' - ' + '*** Starting Connection to ' + NewDialNum) ;
    doConnect.Enabled := false ;

// ISDN multilink, might start all links, or just one, a second may be started later
    if DialLink.ItemIndex < 0 then DialLink.ItemIndex := 0 ; // Angus 5.00 sanity check
    MgRasCon.SubEntry := DialLink.ItemIndex ;  // which sub entry to dial for multilink
    NewHandle := 0 ;
    NewConnName := MgRasCon.EntryName ;
    MgRasCon.PhoneNumber := NewDialNum ;  // translated by TAPI
// NOTE - set PhoneNumber to blank to use the one in the phonebook
    StopFlag := false ;                         // set if Disconnect button is pressed
    NewDialTick := GetTickCount ;   // time call started
    if OptSpeaker.Checked then
        SpeakerMode := SpeakerOn
    else
        SpeakerMode := SpeakerOff ;

// get phone book, start connection
    err := MgRasCon.GetDialParams ;
    if err = 0 then
    begin
        err := MgRasCon.ConnectNT (NewHandle, false, SpeakerMode)
    end ;

// fails here is dialling did not even start
    if err <> 0 then
    begin
        Status.Panels[0].Text := 'Dial Failed - ' + MgRasCon.StatusStr ;
        AddLog ('Dial Failed, State=' + IntToStr (err) + ' - ' + MgRasCon.StatusStr) ;
        beep ;
    end ;

// dialling started OK
// dial connection or failure is checked in StateChanged event handler
   doDisConn.Enabled := true ;
end;

// hang-up a connection

procedure TMainForm.doDisConnClick(Sender: TObject);
var
    connr: integer ;
begin
    StopFlag := true ;
    if (ConInfoList.SelCount = 0) then exit ;
    connr := ConInfoList.Selected.Index ;  // find selected connection
    if connr < 0 then exit ;
    if HandleList [connr] = 0 then exit ;
    if ConInfoList.Items [connr].Caption = '' then exit ;  // no connection
    AddLog (ConInfoList.Items [connr].Caption + ' - ' +
              '*** Started Disconnect Handle=' + IntToStr (HandleList [connr])) ;

// disconnect, returns when done, calls StateChanged
    MgRasCon.DisconnectEx (HandleList [connr], SubEntList [connr], 3000, true) ;
    if NewHandle = HandleList [connr] then
    begin
        NewHandle := 0 ;   // dialling completed, no more checks
        doConnect.Enabled := true ;
    end ;
end;

// display Microsoft dialog to create a new entry in the phonebook

procedure TMainForm.doCreateConnClick(Sender: TObject);
begin
    Status.Panels[1].Text := '' ;
    if MgRasCon.CreatePhonebook (Application.Handle) <> 0 then
        Status.Panels[1].Text := MgRasCon.StatusStr
    else
        RefreshConns ;
end;

// display Microsoft dialog to edit an entry in the phonebook

procedure TMainForm.doEditConnClick(Sender: TObject);
begin
    Status.Panels[1].Text := '' ;
// get selected entry
    if GetSelItem < 0 then exit ;
    if MgRasCon.EditPhonebook (Application.Handle) <> 0 then        // display Dialog
                Status.Panels[1].Text := MgRasCon.StatusStr ;
    ConnListClick (self);
end;

procedure TMainForm.doDeleteConnClick(Sender: TObject);
begin
    Status.Panels[1].Text := '' ;
// get selected entry
    if GetSelItem < 0 then exit ;
    if MgRasCon.DeletePhonebook <> 0 then
        Status.Panels[1].Text := MgRasCon.StatusStr
    else
        RefreshConns ;
end;

procedure TMainForm.doRenameConnClick(Sender: TObject);
var
   oldname, newname: string ;
begin
    Status.Panels[1].Text := '' ;
// get selected entry
    if GetSelItem < 0 then exit ;
    oldname := MgRasCon.EntryName ;     // Connection name
    newname := oldname ;
    while newname = oldname do
    begin
       if NOT InputQuery ('Rename Connection', 'New Connection Name',
                                                    newname) then exit ;
       if MgRasCon.ValidateName (newname) <> 0 then
       begin
           Status.Panels[1].Text := MgRasCon.StatusStr ;
           beep ;
           newname := oldname ;
       end ;
    end ;
    Status.Panels[1].Text := '' ;
    MgRasCon.EntryName := oldname ;
    if MgRasCon.RenamePhonebook (newname) <> 0 then
        Status.Panels[1].Text := MgRasCon.StatusStr
    else
        RefreshConns ;
end;

procedure TMainForm.doCopyConnClick(Sender: TObject);
var
   oldname, newname: string ;
begin
    Status.Panels[1].Text := '' ;
// get selected entry
    if GetSelItem < 0 then exit ;
    oldname := MgRasCon.EntryName ;     // Connection name
    newname := oldname ;
    while newname = oldname do
    begin
       if NOT InputQuery ('Copy Connection', 'New Connection Name',
                                                    newname) then exit ;
       if MgRasCon.ValidateName (newname) <> 0 then
       begin
           Status.Panels[1].Text := MgRasCon.StatusStr ;
           beep ;
           newname := oldname ;
       end ;
    end ;
    Status.Panels[1].Text := '' ;
    MgRasCon.EntryName := oldname ;
    if MgRasCon.CopyPhonebook (newname) <> 0 then
            Status.Panels[1].Text := MgRasCon.StatusStr
    else
        RefreshConns ;
end;

// dump statistics for all connections to log file

procedure TMainForm.doStatClick(Sender: TObject);
var
    K: integer ;
begin
    MgRasPer.GetPerfStats ;       // get performance info
    for K := 0 to MgRasPer.TotPorts do
    with MgRasPer do
    begin
        AddLog ('Stats for device ' + IntToStr (K) +
                     ', Port=' + PerfPortNam [K] +
                        ', Xmit=' + IntToCStr (PerfXmitCur [K]) +
                            ', Recv=' + IntToCStr (PerfRecvCur [K]) +
                                ', Speed=' + IntToCStr (PerfConnSpd [K]));
    end ;
end;

// do a binary file dump of RASENTRY so it can be compared with other dumps

procedure TMainForm.doDumpEntSomeClick(Sender: TObject);
var
    dumpfn: file ;
    count, subent, len: integer ;
    info: string ; // 11 Aug 2010
begin
    SaveDump.FileName := SaveDump.InitialDir + '\' +
                                            MgRasCon.EntryName + '.bin' ;
    if SaveDump.Execute then
    begin
        try
            SaveDump.InitialDir := ExtractFileDir (SaveDump.Filename) ;
            AssignFile (dumpfn, SaveDump.Filename) ;
            Rewrite (dumpfn, 1) ;
            info := 'Conn-Entry-Info>' ;
            BlockWrite (dumpfn, info [1], Length (info), count) ;
            len := ((EntrySize + 15) div 16) * 16 ;  // round up to 16 bytes
            BlockWrite (dumpfn, EntryInfo, len, count) ;
            info := 'Conn-DeviceInfo>' ;
            BlockWrite (dumpfn, info [1], Length (info), count) ;
         //   len := ((DeviceSize + 15) div 16) * 16 ;  // round up to 16 bytes
         //   BlockWrite (dumpfn, DeviceInfo, len, count) ;
            if MgRasCon.TotSubEntries <> 0 then
            begin
                for subent := 1 to MgRasCon.TotSubEntries do
                begin
                    info := 'Link=' + IntToStr (subent) + '>' ;
                    BlockWrite (dumpfn, info [1], Length (info), count) ;
                    MgRasCon.GetSubEntryProps (subent) ;
                    BlockWrite (dumpfn, SubEntryInfo, SubEntrySize, count) ;
                end ;
            end ;
            Status.Panels[1].Text := 'Connection Dumped to ' +
                                               LowerCase (SaveDump.Filename) ;
            CloseFile (dumpfn) ;
        except
            Status.Panels[1].Text := 'Failed to Dump Entry' ;
            CloseFile (dumpfn) ;
        end ;
        beep ;
    end ;
end;

// set screen information from RAS component properties

procedure TMainForm.GetProperties ;
var
    I, J, K: integer ;
begin
    entCountryName.Items.Assign (MgRasEdt.CountryList);
    entCountryName.ItemIndex := -1 ;
    entDeviceName.Items.Assign (MgRasCon.DeviceNameList) ;
    MultilinkList.Items.Clear ;

    with MgRasEdt do
    begin

    // Location and phone number, including alternates
        entUseCountryAndAreaCodes.Checked := bUseCountryAndAreaCodes ;
        entCountryId.Text := IntToStr (CountryId) ;
        entCountryCode.Text := IntToStr (CountryCode) ;
        entAreaCode.Text := AreaCode ;
        entUseCountryandAreaCodesClick (self) ;  // enable/hide fields
        entLocalNumber.Text := LocalPhoneNumber ;
        entCanonNumber.Text := PhoneCanonical ;
        entAlternates.Lines.Assign (AltPhoneNrList) ;
        entPromoteAlternates.Checked := bPromoteAlternates ;

    // dial params
        entUserName.Text := UserName ;
        entPassword.Text := Password ;
        entDomain.Text := Domain ;
        entCallBackNumber.Text := CallBackNumber ;

    // device stuff
        entDeviceName.ItemIndex := MgRasCon.DeviceNameList.IndexOf (DeviceName) ;
        entDeviceType.Text := DeviceType ;
        entDevicePort.Text := DevicePort ;

    // Idle timeout - NT4 and W2K
        entIdleDisconnectSeconds.Value := 0 ;
        case IdleDisconnectSeconds of
            RASIDS_Disabled: entIdleOption.ItemIndex := 0 ;
            RASIDS_UseGlobalValue: entIdleOption.ItemIndex := 1 ;
            else
            begin
                entIdleOption.ItemIndex := 2 ;
                entIdleDisconnectSeconds.Value := IdleDisconnectSeconds ;
            end ;
        end ;

    // auto dial
        entAutoDialDll.Text := AutoDialDll ;
        entAutoDialFunc.Text := AutoDialFunc ;

    // Framing
        entFramingProtocol.ItemIndex := Ord (FramingProtocol) ;
        entNetIPX.Checked := bNetIPX ;
        entNetBEUI.Checked := bNetBEUI ;
        entNetTCPIP.Checked := bNetTCPIP ;
        entSlipFrameSize.ItemIndex := 0 ;
        if FrameSize > 1006 then entSlipFrameSize.ItemIndex := 1 ;

    // PPP/IP
        entSpecificIPAddress.Checked := bSpecificIPAddress ;
        entSpecificNameServers.Checked := bSpecificNameServers ;
        entIPAddress.Text := IPAddress ;
        entDNSAddress.Text := DNSAddress ;
        entDNSAddressAlt.Text := DNSAddressAlt ;
        entWINSAddress.Text := WINSAddress ;
        entWINSAddressAlt.Text := WINSAddressAlt ;
        entHeaderCompression.Checked := bHeaderCompression ;
        entRemoteDefaultGateway.Checked := bRemoteDefaultGateway ;

    // special stuff
        entNetworkLogon.Checked := bNetworkLogon ;
        entDisableLCPExtensions.Checked := bDisableLCPExtensions ;
        entSoftwareCompression.Checked := bSoftwareCompression ;
        entTerminalAfterDial.Checked := bTerminalAfterDial ;

    // security
        entRequireEncryptedPassword.Checked := bRequireEncryptedPassword ;
        entRequireMSEncryptedPassword.Checked := bRequireMSEncryptedPassword ;
        entRequireDataEncryption.Checked := bRequireDataEncryption ;
        entUseLogonCredentials.Checked := bUseLogonCredentials ;
        entRequireEAP.Checked := bRequireEAP ;
        entRequirePAP.Checked :=  bRequirePAP ;
        entRequireSPAP.Checked := bRequireSPAP ;
        entRequireCHAP.Checked := bRequireCHAP ;
        entRequireMsCHAP.Checked := bRequireMsCHAP ;
        entRequireMsCHAP2.Checked := bRequireMsCHAP2 ;
        entRequireW95MSCHAP.Checked := bRequireW95MSCHAP ;
        entCustom.Checked := bCustom ;
        entEncryptionType.ItemIndex := Ord (EncryptionType) ;  // W2K
        entCustomAuthKey.Value := CustomAuthKey ;

     // Script
        entScript.Text := Script ;
        entTerminalBeforeDial.Checked := bTerminalBeforeDial ;

    // X25
        entX25PadType.Text := X25PadType ;   // could read list
        entX25Address.Text := X25Address ;
        entX25Facilities.Text := X25Facilities ;
        entX25UserData.Text := X25UserData ;

    // multilink and BAP
        entISDNChannels.Text := IntToStr (ISDNChannels) ;  // probably unused
        entSubEntries.Text := IntToStr (SubEntries) ;   // read only
        entDialMode.ItemIndex := Ord (DialMode) ;
        entDialExtraPercent.Value := DialExtraPercent ;
        entDialExtraSampleSeconds.Value :=  DialExtraSampleSeconds ;
        entHangUpExtraPercent.Value := HangUpExtraPercent ;
        entHangUpExtraSampleSeconds.Value := HangUpExtraSampleSeconds ;

    // W2K stuff only
        entPType.ItemIndex := Ord (PType) ;  // read only, I think
        entguidId.Text := GUIDToString (guidId) ;
        entCustomDialDll.Text := CustomDialDll ;
        entVpnStrategy.ItemIndex := Ord (VpnStrategy) ;
        entModemLights.Checked := bModemLights ;
        entSecureLocalFiles.Checked := bSecureLocalFiles ;
        entPreviewPhoneNumber.Checked := bPreviewPhoneNumber ;
        entSharedPhoneNumbers.Checked := bSharedPhoneNumbers ;
        entPreviewUserPw.Checked := bPreviewUserPw ;
        entPreviewDomain.Checked := bPreviewDomain ;
        entShowDialingProgress.Checked := bShowDialingProgress ;

    // XP stuff only
        entSecureFileAndPrint.Checked := bSecureFileAndPrint ;
        entDontNegotiateMultilink.Checked := bDontNegotiateMultilink ;
        entSecureClientForMSNet.Checked := bSecureClientForMSNet ;
        entDontUseRasCredentials.Checked := bDontUseRasCredentials ;
        entUsePreSharedKey.Checked := bUsePreSharedKey ;
        entUseGlobalDeviceSettings.Checked := bUseGlobalDeviceSettings ;
        entDisableNbtOverIP.Checked := bDisableNbtOverIP ;
        entInternet.Checked := bInternet ;
        entReconnectIfDropped.Checked := bReconnectIfDropped ;
        entSharePhoneNumbers.Checked := bSharePhoneNumbers ;
        entDnsSuffix.Text := DnsSuffix ;
        entTcpWindowSize.Value := TcpWindowSize ;
        entPrerequisitePbk.Text := PrerequisitePbk ;
        entPrerequisiteEntry.Text := PrerequisiteEntry ;
        entRedialCount.Value := RedialCount ;
        entRedialPause.Value := RedialPause ;

// display multilink stuff - show all defined devices then which are being used
// Note - NT returns the default number and device as the first sub entry
        for I := 0 to MgRasCon.DeviceNameList.Count - 1  do
        begin
            with MultilinkList.Items.Add do
            begin
                Caption := MgRasCon.DeviceNameList [I] ;
                Checked := false ;
                SubItems.Add ('') ;
                SubItems.Add ('') ;
                SubItems.Add (MgRasCon.DeviceTypeList [I] ) ;
            end ;
        end ;
        if SubCurTotal <> 0 then
        begin
            for J := 1 to SubCurTotal do
            begin
                K := MgRasCon.DeviceNameList.IndexOf (SubDeviceName [J]) ;
                if K >= 0 then
                begin
                // silly special case where devices have the same name!
                    if (J > 1) and  (K < MgRasCon.DeviceNameList.Count - 1) then
                    begin
                        if (SubDeviceName [J] = SubDeviceName [J - 1]) then inc (K) ;
                    end ;
                    with MultilinkList.Items [K] do
                    begin
                        Checked := true ;
                        SubItems [0] := SubDevicePort [J] ;
                        SubItems [1] := SubLocalPhoneNumber [J] ;
                        SubItems [2] := SubDeviceType [J] ;
                    end ;
                end ;
            end ;
        end ;
    end ;
end ;

// set RAS component properties from stuff specified by user

procedure TMainForm.PutProperties ;
var
    I, newentries: integer ;
begin
    MgRasEdt.DefaultProps ;    // clear everything, in theory everything is set...
    with MgRasEdt do
    begin
    // phonebook
        PBLocation := entPhoneBook.ItemIndex ;  // 5.20 set phonebook file 

    // Location and phone number, including alternates
        bUseCountryAndAreaCodes := entUseCountryAndAreaCodes.Checked ;
        if entCountryName.ItemIndex >= 0 then
                    CountryId := CountryIds [entCountryName.ItemIndex] ;
        AreaCode := entAreaCode.Text ;
        try
            CountryCode := StrToInt (entCountryCode.Text) ;
        except
            CountryCode := CountryId ;
        end ;
        LocalPhoneNumber := entLocalNumber.Text ;
        PhoneCanonical := entCanonNumber.Text ;
        AltPhoneNrList.Assign (entAlternates.Lines) ;
        bPromoteAlternates := entPromoteAlternates.Checked ;

    // dial params
        UserName := entUserName.Text ;
        Password := entPassword.Text ;
        Domain := entDomain.Text ;
        CallBackNumber := entCallBackNumber.Text ;

    // device stuff
        DeviceName := entDeviceName.Items [entDeviceName.ItemIndex] ;
        DeviceType := entDeviceType.Text ;
    //  DevicePort   view only

    // Idle timeout - NT4 and W2K
        case entIdleOption.ItemIndex of
            0: IdleDisconnectSeconds := RASIDS_Disabled ;
            1: IdleDisconnectSeconds := RASIDS_UseGlobalValue ;
            2: IdleDisconnectSeconds := entIdleDisconnectSeconds.Value ;
        end ;

    // auto dial - did not allow these to be edited
        AutoDialDll := entAutoDialDll.Text ;
        AutoDialFunc := entAutoDialFunc.Text ;

    // Framing
        FramingProtocol := TFramingProtocol (entFramingProtocol.ItemIndex) ;
        bNetIPX := entNetIPX.Checked ;
        bNetBEUI := entNetBEUI.Checked ;
        bNetTCPIP := entNetTCPIP.Checked ;
        case entSlipFrameSize.ItemIndex of
            0: FrameSize := 1006 ;
            1: FrameSize := 1500 ;
        end ;

    // PPP/IP
        bSpecificIPAddress := entSpecificIPAddress.Checked ;
        bSpecificNameServers := entSpecificNameServers.Checked ;
        IPAddress := entIPAddress.Text ;
        DNSAddress := entDNSAddress.Text ;
        DNSAddressAlt := entDNSAddressAlt.Text ;
        WINSAddress := entWINSAddress.Text ;
        WINSAddressAlt := entWINSAddressAlt.Text ;
        bHeaderCompression := entHeaderCompression.Checked ;
        bRemoteDefaultGateway := entRemoteDefaultGateway.Checked ;

    // special stuff
        bNetworkLogon := entNetworkLogon.Checked ;
        bDisableLCPExtensions := entDisableLCPExtensions.Checked ;
        bSoftwareCompression := entSoftwareCompression.Checked ;
        bTerminalAfterDial := entTerminalAfterDial.Checked ;

    // security
        bRequireEncryptedPassword := entRequireEncryptedPassword.Checked ;
        bRequireMSEncryptedPassword := entRequireMSEncryptedPassword.Checked ;
        bRequireDataEncryption := entRequireDataEncryption.Checked ;
        bUseLogonCredentials := entUseLogonCredentials.Checked ;
        bRequireEAP := entRequireEAP.Checked ;
        bRequirePAP := entRequirePAP.Checked ;
        bRequireSPAP := entRequireSPAP.Checked ;
        bRequireCHAP := entRequireCHAP.Checked ;
        bRequireMsCHAP := entRequireMsCHAP.Checked ;
        bRequireMsCHAP2 := entRequireMsCHAP2.Checked ;
        bRequireW95MSCHAP := entRequireW95MSCHAP.Checked ;
        bCustom := entCustom.Checked ;
        EncryptionType := TEncryptionType (entEncryptionType.ItemIndex) ;
        CustomAuthKey := entCustomAuthKey.Value ;

     // Script
        Script := entScript.Text ;
        bTerminalBeforeDial := entTerminalBeforeDial.Checked ;

    // X25
        X25PadType := entX25PadType.Text ;
        X25Address := entX25Address.Text ;
        X25Facilities := entX25Facilities.Text ;
        X25UserData := entX25UserData.Text ;

    // multilink and BAP
    //      SubEntries   - read only
        DialMode := TDialMode (entDialMode.ItemIndex) ;
        DialExtraPercent := entDialExtraPercent.Value ;
        DialExtraSampleSeconds := entDialExtraSampleSeconds.Value ;
        HangUpExtraPercent := entHangUpExtraPercent.Value ;
        HangUpExtraSampleSeconds := entHangUpExtraSampleSeconds.Value ;

    // W2K stuff only
        PType := TPType (entPType.ItemIndex) ;
        guidId := StringToGUID (entguidId.Text) ;
        CustomDialDll := entCustomDialDll.Text ;
        VpnStrategy := TVpnStrategy (entVpnStrategy.ItemIndex) ;
        bModemLights := entModemLights.Checked ;
        bSecureLocalFiles := entSecureLocalFiles.Checked ;
        bPreviewPhoneNumber := entPreviewPhoneNumber.Checked ;
        bSharedPhoneNumbers := entSharedPhoneNumbers.Checked ;
        bPreviewUserPw := entPreviewUserPw.Checked ;
        bPreviewDomain := entPreviewDomain.Checked ;
        bShowDialingProgress := entShowDialingProgress.Checked ;

    // XP stuff only
        bSecureFileAndPrint := entSecureFileAndPrint.Checked ;
        bDontNegotiateMultilink := entDontNegotiateMultilink.Checked ;
        bSecureClientForMSNet := entSecureClientForMSNet.Checked ;
        bDontUseRasCredentials := entDontUseRasCredentials.Checked ;
        bUsePreSharedKey := entUsePreSharedKey.Checked ;
        bUseGlobalDeviceSettings := entUseGlobalDeviceSettings.Checked ;
        bDisableNbtOverIP := entDisableNbtOverIP.Checked ;
        bInternet := entInternet.Checked ;
        bReconnectIfDropped := entReconnectIfDropped.Checked ;
        bSharePhoneNumbers := entSharePhoneNumbers.Checked ;
        DnsSuffix := entDnsSuffix.Text ;
        TcpWindowSize := entTcpWindowSize.Value ;
        PrerequisitePbk := entPrerequisitePbk.Text ;
        PrerequisiteEntry := entPrerequisiteEntry.Text ;
        RedialCount := entRedialCount.Value ;
        RedialPause := entRedialPause.Value ;  

    // multilink - gets messy since may be adding or removing subentries
    // check how many devices are ticked and keep them
        newentries := 0 ;
        for I := 0 to MultilinkList.Items.Count - 1 do
        begin
           with MultilinkList.Items [I] do
           begin
                if Checked then
                begin
                    inc (newentries) ;
                    SubDeviceName [newentries] := Caption ;
                   // ignore port
            // same phone numbers for all subentries
                    SubLocalPhoneNumber [newentries] := entLocalNumber.Text ;
                    SubAltPhoneNrList [newentries].Assign (entAlternates.Lines) ;
                    SubDeviceType [newentries] := SubItems [2] ;
                end ;
            end ;
        end ;
        SubCurTotal := newentries ;
    end ;
end ;

procedure TMainForm.doPropLoadClick(Sender: TObject);
var
    oldname: string ;
    errcode, item: integer ;
begin
    Status.Panels[1].Text := '' ;
    entEntryName.ReadOnly := true ;
    item := ConnList.ItemIndex ;
    if item = -1 then exit ;
    if item >= MagRasNumEntryRec then
    begin
        beep ;
        Status.Panels[0].Text := 'Entry Not Found in List' ;
        exit ;
    end ;
    oldname := MagRasEntryRecs [item].EntryName ;     // connection entry name, Angus 4.60
    MgRasEdt.PBLocation := MagRasEntryRecs [item].PBLocation ;    // phonebook location, Angus 5.20
    WrapCaption (MgRasEdt.PhoneBookPath, LabelPhonebookPathFull) ; // 5.20
    entPhoneBook.ItemIndex := MgRasEdt.PBLocation ;  // phonebook location, Angus 4.60
    entPhoneBook.Enabled := false ;                  // but don't let user change it
    errcode := MgRasEdt.GetAllEntryProps (oldname) ;
    if errcode <> 0 then
    begin
        Status.Panels[1].Text := 'EntryProps: ' + MgRasEdt.StatusStr ;
        beep ;
    end
    else
    begin
        MgRasEdt.GetDialProps (oldname) ;
        if errcode <> 0 then
        begin
            Status.Panels[1].Text := 'DialProps: ' + MgRasEdt.StatusStr ;
            beep ;
        end
        else
        begin
            entEntryName.Text := oldname ;
            GetProperties ;
            Status.Panels[1].Text := 'Loaded Properties OK' ;
            beep ;
        end ;
    end ;
    FullPropsPages.ActivePage := TabDial ;
end;

function TMainForm.NewConn: boolean ;
begin
    MgRasEdt.PPPDefault ;
    result := true ;

// set dialling location defaults, country and area code
    with MgRasEdt do
    begin
        bUseCountryAndAreaCodes := true ;
        CountryId := DialLocation.CountryID ;
        CountryCode := DialLocation.CountryCode ;
        AreaCode := DialLocation.CityCode ;
    end ;
    WrapCaption (MgRasEdt.PhoneBookPath, LabelPhonebookPathFull) ; // 5.20
    entPhoneBook.ItemIndex := MgRasEdt.PBLocation ; // phonebook location, Angus 4.60
    entPhoneBook.Enabled := true ;
    entEntryName.Text := '' ;
    entEntryName.ReadOnly := false ;
    GetProperties ;
    FullPropsPages.ActivePage := TabDial ;
end;

procedure TMainForm.doPropNewClick(Sender: TObject);
begin
    if NewConn then Status.Panels[1].Text := 'Specify New Properties' ;
    beep ;
end;

procedure TMainForm.doPropCopyClick(Sender: TObject);
begin
    doPropLoadClick (self) ;
    entPhoneBook.Enabled := true ;
    entEntryName.Text := '' ;
    entEntryName.ReadOnly := false ;
end;

procedure TMainForm.doPropSaveClick(Sender: TObject);
var
    errcode: integer ;
    newname: string ;
    newflag: boolean ;
begin
    newflag := NOT entEntryName.ReadOnly ;
    newname := trim (entEntryName.Text) ;
    if newflag then
    begin
        if MgRasCon.ValidateName (newname) <> 0 then
        begin
            Status.Panels[1].Text := MgRasCon.StatusStr ;
            beep ;
            exit ;
        end ;
        entEntryName.ReadOnly := true ;
    end ;
    PutProperties ;
    errcode := MgRasEdt.PutAllEntryProps (newname) ;
    if errcode = 0 then MgRasEdt.PutDialProps (newname) ;
    if errcode <> 0 then
    begin
        Status.Panels[1].Text := MgRasEdt.StatusStr ;
        beep ;
        if newflag then entEntryName.ReadOnly := false ;
    end
    else
    begin
        if newflag then
        begin
            Status.Panels[1].Text := 'Created New Connection OK' ;
            RefreshConns ;
        end
        else
            Status.Panels[1].Text := 'Updated Properties OK' ;
        beep ;
    end
end;

procedure TMainForm.entUseCountryandAreaCodesClick(Sender: TObject);
var
    I, ID: integer ;
begin
    entCountryCode.Enabled := entUseCountryAndAreaCodes.Checked ;
    entAreaCode.Enabled := entUseCountryAndAreaCodes.Checked ;
    entCountryName.Enabled := entUseCountryAndAreaCodes.Checked ;
    if entCountryId.Text = '' then
                   entCountryId.Text := IntToStr (DialLocation.CountryID) ;
    if entUseCountryAndAreaCodes.Checked then
    begin
        ID := 0 ;
        if entCountryId.Text <> '' then
        try
            ID := StrToInt (entCountryId.Text) ;
        except
        end ;
        if ID = 0 then ID := DialLocation.CountryID ;
        for I := 0 to MgRasEdt.CountryList.Count - 1 do
        begin
            if ID = MgRasEdt.CountryIds [I] then
            begin
                entCountryName.ItemIndex := I ;
                entCountryCode.Text := IntToStr (MgRasEdt.CountryCodes [I]) ;
                break ;
            end ;
        end ;
    end
    else
        entCountryName.ItemIndex := -1 ;
    NumberChanged (self) ;
end;

procedure TMainForm.NumberChanged(Sender: TObject);
begin
    if entCountryCode.Text = '' then entCountryCode.Text := '0' ;
    entCanonNumber.Text := MagRasGetCanonical
         (entUseCountryAndAreaCodes.Checked, StrToInt (entCountryCode.Text),
                                       entAreaCode.Text, entLocalNumber.Text) ;
    CanonNumberChange (self) ;
end;

procedure TMainForm.entCountryNameChange(Sender: TObject);
var
    Id: integer ;
begin
    if entCountryName.ItemIndex >= 0 then
    begin
        Id := MgRasEdt.CountryIds [entCountryName.ItemIndex] ;
        entCountryId.Text := IntToStr (ID) ;
        entCountryCode.Text := IntToStr (MgRasEdt.CountryCodes
                                             [entCountryName.ItemIndex]) ;
        NumberChanged (self) ;
    end ;
end;

procedure TMainForm.doScriptOpenClick(Sender: TObject);
begin
    OpenScript.FileName := entScript.Text ;
    if entScript.Text <> '' then
                OpenScript.InitialDir := ExtractFileDir (entScript.Text) ;
    if OpenScript.Execute then entScript.Text := OpenScript.FileName ;
end;

procedure TMainForm.doScriptViewClick(Sender: TObject);
begin
    if entScript.Text = '' then exit ;
    try
        ViewScript.Lines.LoadFromFile (entScript.Text) ;
    except
    end ;
end;

// do a binary file dump of RASENTRY so it can be compared with other dumps

procedure TMainForm.doPropDumpClick(Sender: TObject);
var
    dumpfn: file ;
    count, subent, len: integer ;
    info: string ;
begin
    if entEntryName.Text = '' then exit ;
    SaveDump.FileName := SaveDump.InitialDir + '\' +
                                            entEntryName.Text + '.bin' ;
    if SaveDump.Execute then
    begin
        try
            SaveDump.InitialDir := ExtractFileDir (SaveDump.Filename) ;
            AssignFile (dumpfn, SaveDump.Filename) ;
            Rewrite (dumpfn, 1) ;
            info := 'EntryInfo>' ;
            BlockWrite (dumpfn, info [1], Length (info), count) ;
            len := ((EntryEdtSize + 15) div 16) * 16 ;  // round up to 16 bytes
            BlockWrite (dumpfn, EntryEdtInfo, len, count) ;
            info := 'DeviceInfo>' ;
            BlockWrite (dumpfn, info [1], Length (info), count) ;
         //   len := ((DeviceEdtSize + 15) div 16) * 16 ;  // round up to 16 bytes
         //   BlockWrite (dumpfn, DeviceEdtInfo, len, count) ;
            if MgRasEdt.SubCurTotal <> 0 then
            begin
                for subent := 1 to MgRasEdt.SubCurTotal do
                begin
                    info := 'Link=' + IntToStr (subent) + '>' ;
                    BlockWrite (dumpfn, info [1], Length (info), count) ;
                    MgRasEdt.GetSubEntryProps (entEntryName.Text, subent) ;
                    BlockWrite (dumpfn, SubEntryEdtInfo, SubEntryEdtSize, count) ;
                end ;
            end ;
            Status.Panels[1].Text := 'Connection Dumped to ' +
                                               LowerCase (SaveDump.Filename) ;
            CloseFile (dumpfn) ;
        except
            Status.Panels[1].Text := 'Failed to Dump Entry' ;
            CloseFile (dumpfn) ;
        end ;
        beep ;
    end ;
end;

procedure TMainForm.entDeviceNameChange(Sender: TObject);
begin
    entDeviceType.Text :=
                MgRasCon.DeviceTypeList [entDeviceName.ItemIndex] ;
end;

procedure TMainForm.CanonNumberChange(Sender: TObject);
var
    DispNum, DialNum: string ;
begin
    MgRasCon.TranslateAddr (0, entCanonNumber.Text, DispNum, DialNum) ;
    LabelNumberDisp.Caption := 'Display Number: ' + DispNum ;
    LabelNumberDial.Caption := 'Dialable Number: ' + DialNum ;
end;

// warning - this will only show the dialling properties for the first installed
// modem (devnr=0) - it needs a list of TAPI modems to work properly

procedure TMainForm.doPropDialClick(Sender: TObject);
begin
    MgRasCon.TranslateDialog (Handle, 0, '') ;
    GetDialProps ;  // in case things changed  
end;

procedure TMainForm.doQuickClearClick(Sender: TObject);
begin
    Status.Panels[1].Text := '' ;
    MgRasEdt.PPPDefault ;
    quickEntryName.Text := '' ;
    quickUserName.Text := '' ;
    quickPassword.Text := '' ;
    quickCanonNumber.Text := '' ;
    quickDeviceName.Items.Assign (MgRasCon.DeviceNameList) ;
    quickDeviceName.ItemIndex := 0 ;
    quickPhoneBook.ItemIndex := MgRasEdt.PBLocation ; // phonebook location, Angus 5.20
end;

procedure TMainForm.quickCanonNumberChange(Sender: TObject);
var
    DispNum, DialNum: string ;
begin
    MgRasCon.TranslateAddr (0, quickCanonNumber.Text, DispNum, DialNum) ;
    qLabelNumberDisp.Caption := 'Display Number: ' + DispNum ;
    qLabelNumberDial.Caption := 'Dialable Number: ' + DialNum ;
end;

// quick create a new phonebook entry

procedure TMainForm.doQuickCreateClick(Sender: TObject);
var
    errcode: integer ;
    newname: string ;
begin
    Status.Panels[1].Text := '(Creating Entry)' ;
    newname := trim (quickEntryName.Text) ;
    if MgRasCon.ValidateName (newname) <> 0 then
    begin
        Status.Panels[1].Text := MgRasCon.StatusStr ;
        beep ;
        exit ;
    end ;

// set properties
    MgRasEdt.PPPDefault ;
    with MgRasEdt do
    begin

    // phonebook location, Angus 5.20
        PBLocation := quickPhoneBook.ItemIndex ;

    // telephone numbers
        SetCanonical (quickCanonNumber.Text) ;

    // dial params
        UserName := quickUserName.Text ;
        Password := quickPassword.Text ;
        bDefaultCreds := quickDefaultCreds.Checked ; // 5.30

    // device stuff
        DeviceName := quickDeviceName.Items [quickDeviceName.ItemIndex] ;
        DeviceType := MgRasCon.DeviceTypeList [quickDeviceName.ItemIndex] ;
    end ;
    errcode := MgRasEdt.PutAllEntryProps (newname) ;
    if errcode = 0 then MgRasEdt.PutDialProps (newname) ;
    if errcode <> 0 then
    begin
        Status.Panels[1].Text := MgRasEdt.StatusStr ;
        beep ;
    end
    else
    begin
        Status.Panels[1].Text := 'Created New Connection OK' ;
        RefreshConns ;
        beep ;
    end

end;

// build both RAS Entry lists, ListView more detailed

procedure TMainForm.doRefreshEntriesClick(Sender: TObject);
var
    I, tot, errcode: integer ;
    StartTick: DWORD ;
    temp: string ;
begin
    doRefreshEntries.Enabled := false ;
    LabelEntryRes.Caption := '' ;
    try
    EntriesList.Items.Clear ;
    ConnList.Items.Clear ;
    StartTick := GetTickCount ;
    errcode := MagRasGetEntryRecs ('', EntryUseAPI.Checked) ;
    if errcode <> 0 then
    begin
        Status.Panels[1].Text := MgRasEdt.StatusStr ;
        beep ;
        exit ;
    end ;
    tot := MagRasNumEntryRec ;
    temp := FloatToStrF ((GetTickCount - StartTick) / 1000, ffFixed, 7, 3) ;
    LabelEntryRes.Caption := 'Refreshed ' + IntToStr (tot) +
                                        ' entries in ' + temp + ' secs' ;
    if tot = 0 then exit ;
    for I := 0 to Pred (tot) do
    begin
        ConnList.Items.Add (MagRasEntryRecs [I].EntryName) ;
        with EntriesList.Items.Add, MagRasEntryRecs [I] do
        begin
            Caption := EntryName ;
            SubItems.Add (CanonNum) ;
            SubItems.Add (DevName1) ;
            SubItems.Add (DevPort1) ;
            SubItems.Add (DevType1) ;
            SubItems.Add (DevName2) ;
            SubItems.Add (DevPort2) ;
            SubItems.Add (DevType2) ;
            if PBLocation > REN_AllUsers then PBLocation := 0 ;  // 5.20 make sure it's legal
            SubItems.Add (MasRasPBLocationStr [PBLocation]) ;    // 5.20 show literal
            SubItems.Add (Phonebook) ;
        end ;
    end ;
    if ConnList.Items.Count <> 0 then ConnList.ItemIndex := 0 ; // set first
    ConnListClick (self) ;                              // get connection info
    CurSelItem := -1 ;
    finally
        doRefreshEntries.Enabled := true ;
    end ;
end;

procedure TMainForm.GetNetworkAlive ;
var
    Flags: DWORD ;
    QocInfo: TQocInfo ;
begin
    LabelQOS.Caption := '' ;
    if NOT LoadSensapi then
    begin
        LabelNetAlive.Caption := 'These functions need MSIE5 or later' ;
        exit ;
    end ;
    if NOT IsNetAlive (Flags) then
    begin
        LabelNetAlive.Caption := 'No Network Connectivity' ;
        exit ;
    end ;
    LabelNetAlive.Caption := 'Network Connectivity: ' ;
    if Flags AND NETWORK_ALIVE_LAN = NETWORK_ALIVE_LAN then
           LabelNetAlive.Caption := LabelNetAlive.Caption + 'LAN ' ;
    if Flags AND NETWORK_ALIVE_WAN = NETWORK_ALIVE_WAN then
            LabelNetAlive.Caption := LabelNetAlive.Caption + 'RAS ' ;
    if Flags AND NETWORK_ALIVE_AOL = NETWORK_ALIVE_AOL then
            LabelNetAlive.Caption := LabelNetAlive.Caption + 'AOL ' ;

// pings an IP address, HTTP URL or UNC name
    IsDestReachable ('0.0.0.0', QocInfo) ;  // don't check result, no destination
    LabelQOS.Caption := 'Receive Speed: ' + IntToCStr (QocInfo.dwInSpeed) +
                 CRLF + 'Transmit Speed: ' + IntToCStr (QocInfo.dwOutSpeed) ;
end ;

procedure TMainForm.optMSIEDefConnDropDown(Sender: TObject);
begin
    optMSIEDefConn.Items.Assign (ConnList.Items) ;
end;

procedure TMainForm.doMSIEUpdateClick(Sender: TObject);
var
    res: boolean ;
    info: string ;
    I: integer ;
begin
    I := optMSIEAutDial.ItemIndex ;
    info := optMSIEDefConn.Text ;
    res := MSIEAutoDialOpt (I, true) ;
    if res then
        res := MSIEDefConn (info, true) ;
    if res then
        Status.Panels[0].Text := 'Internet Options Updated OK'
    else
        Status.Panels[0].Text := 'Failed to Update Internet Options' ;
end;

procedure TMainForm.doShortcutClick(Sender: TObject);
var
    err: string ;
begin
    Status.Panels[1].Text := '' ;
// get selected entry into MgRasCon.EntryName
    if GetSelItem < 0 then exit ;
    if NOT SCutSpecLink (CSIDL_CONNECTIONS, Nil, MgRasCon.EntryName, err) then
    begin
        Status.Panels[0].Text := 'Can Not Add Shortcut, Err=' + err ;
        exit ;
    end ;
    Status.Panels[0].Text := 'Added Shortcut Successfully' ;
end;

procedure TMainForm.doDUNDialogClick(Sender: TObject);
var
    PbdDlg: TRasPbdDlgW ;
begin
    Status.Panels[1].Text := '' ;
// get selected entry
    if GetSelItem < 0 then exit ;
    FillChar (PbdDlg, SizeOf (PbdDlg), #0);
    PbdDlg.dwSize := SizeOf (PbdDlg) ;
    PbdDlg.hwndOwner := Application.Handle ;
    PbdDlg.dwFlags := 0 ;
    if NOT RasPhonebookDlgW (PWideChar (MgRasCon.PhoneBookPathW),
                         PWideChar(MgRasCon.EntryNameW), PbdDlg) then
                  Status.Panels[1].Text := MgRasCon.GetErrorString (PbdDlg.dwError) ;
end;

procedure TMainForm.doDialDialogClick(Sender: TObject);
var
    DialDlg: TRasDialDlg ;
begin
    Status.Panels[1].Text := '' ;
// get selected entry
    if GetSelItem < 0 then exit ;
    FillChar (DialDlg, SizeOf (DialDlg), #0);
    DialDlg.dwSize := SizeOf (DialDlg) ;
    DialDlg.hwndOwner := Application.Handle ;
    DialDlg.dwFlags := 0 ;
    if NOT RasDialDlgW (PWideChar (MgRasCon.PhoneBookPathW),
                         PWideChar(MgRasCon.EntryNameW), Nil, DialDlg) then
                  Status.Panels[1].Text := MgRasCon.GetErrorString (DialDlg.dwError) ;
end;

end.

