unit exeditc0;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas Edit Entry Complex Example
==================================

Edit the properties of an entry in the RAS phonebook (ie a connection).
This complex version shows all the properties that can be edited.
The demo shows properties that are NT4 and/or Windows 2000 specific,
when ideally those edit boxes should be hidden if not supported. Likewise
several other options could be suppressed if not valid (like IP addresses).

Created by Angus Robertson, Magenta Systems Ltd, England
in 2000, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 20 Aug 2010

To load this example, the TMagRas components need to have been previously
installed on the component palette.

23 Jan 2001 - added Encryption Type = Optional which is default for W2K
24 May 2007 - set PBLocation before editing entry
            - use MagRasGetEntryList (in magrasent) instead of MagRasCon.GetPhoneBookEntries
19 Aug 2010 - Removed cast warning for Delphi 2009 and later,

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComObj, ComCtrls, Mask, Spin,
  magrascon, magrasapi, magrasedt, magrasent, magsubs1 ;

type
  TForm1 = class(TForm)
    MagRasCon: TMagRasCon;
    doExit: TButton;
    ConnList: TListBox;
    Label3: TLabel;
    MagRasEdt: TMagRasEdt;
    doLoad: TButton;
    doSave: TButton;
    Status: TStatusBar;
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
    OpenScript: TOpenDialog;
    LabelPBLocation: TLabel;
    procedure doExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doLoadClick(Sender: TObject);
    procedure doSaveClick(Sender: TObject);
    procedure entCountryNameChange(Sender: TObject);
    procedure entUseCountryandAreaCodesClick(Sender: TObject);
    procedure doPropDialClick(Sender: TObject);
	procedure entDeviceNameChange(Sender: TObject);
	procedure NumberChanged(Sender: TObject);
	procedure entCanonNumberChange(Sender: TObject);
	procedure doScriptOpenClick(Sender: TObject);
	procedure doScriptViewClick(Sender: TObject);
  private
    { Private declarations }
	procedure RefreshConnList ;
	procedure GetProperties ;
	procedure GetEntry ;
	procedure PutProperties ;
	procedure GetDialProps ;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  DialProps: TDialProps;        // dialling properties
  DialLocation: TDialLocation;  // default dialling location
  DialCard: TDialCard ;		 	// default dialling calling card
  DialCountry: TDialCountry ;	// default country dialling info
  OldName: string ;     		// entry being edited

implementation

{$R *.DFM}

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

procedure TForm1.RefreshConnList ;
var
    I: integer ;
begin
// get list of phonebook entries
//	MagRasCon.GetPhoneBookEntries ;   5.21 gone
//  MagRasCon.PhoneBookEntries.Sort ;  4.60 already sorted
//	ConnList.Items.Assign (MagRasCon.PhoneBookEntries) ;	 // display it  5.21 gone

// 5.21 get connection entries
    ConnList.Items.Clear;
    I := MagRasGetEntryList ('') ;
    if (I <> 0) then
        Status.SimpleText := 'Failed to Read Connection Entries - ' +
                                            MagRasCon.GetErrorString (I)
    else if (MagRasNumEntryRec = 0) then
        Status.SimpleText := 'No Connection Entries Found'
    else
    begin
        for I := 0 to Pred (MagRasNumEntryRec) do
            ConnList.Items.Add (String (MagRasEntryRecs [I].EntryName)) ;
    end ;
end ;

procedure TForm1.GetDialProps ;
begin
    MagRasCon.GetTransCaps (DialProps, DialLocation, DialCard, DialCountry) ;
end ;

procedure TForm1.FormCreate(Sender: TObject);
var
	I: integer ;
begin
// see if RAS has been installed
	if MagRasCon.TestRAS then
    begin
     // get list of phonebook entries
	    RefreshConnList ;

	// get list of RAS capable modems and devices - don't sort list!
    	MagRasCon.GetDeviceList ;
        if MagRasCon.DeviceNameList.Count <> 0 then
        begin
        	for I := 0 to MagRasCon.DeviceNameList.Count - 1 do
            begin
            	entDeviceName.Items.Add (MagRasCon.DeviceNameList [I] +
	                ' (' + LowerCase (MagRasCon.DeviceTypeList [I]) + ')') ;
			end ;
		end ;

   	// get CountryList, CountryIds and CountryCodes - don't sort list!
    	MagRasEdt.GetAllCountryInfo ;
        entCountryName.Items.Assign (MagRasEdt.CountryList) ;

	// get dialling properties, location, calling card, etc
		GetDialProps ;

	// clear fields
		FullPropsPages.ActivePage := TabDial ;
        entEntryName.Text := '' ;
       	MagRasEdt.DefaultProps ;    // clear everything, in theory everything is set...
        GetProperties ;    			// display blanks
	end
    else
    begin
	 	Status.SimpleText := 'RAS is not installed' ;
        beep ;
    end ;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	// could check if still online and close connection
    Application.Terminate ;
end;

// set screen information from RAS component properties

procedure TForm1.GetProperties ;
var
	I, J: integer ;
begin
//	entCountryName.Items.Assign (MgRasEdt.CountryList);
//    entCountryName.ItemIndex := -1 ;
//    entDeviceName.Items.Assign (MgRasCon.DeviceNameList) ;
//    MultilinkList.Items.Clear ;

	with MagRasEdt do
    begin

	// Location and phone number, including alternates
		entUseCountryAndAreaCodes.Checked := bUseCountryAndAreaCodes ;
        entCountryId.Text := IntToStr (CountryId) ;

	// find country name from country Id
		if MagRasEdt.CountryList.Count <> 0 then
        begin
			for I := 0 to Pred (MagRasEdt.CountryList.Count) do
		    begin
        		if CountryId = MagRasEdt.CountryIds [I] then
		        begin
	    		    entCountryName.ItemIndex := I ;
		            break ;
				end ;
			end ;
		end ;
        entCountryCode.Text := IntToStr (CountryCode) ;
		entAreaCode.Text := String (AreaCode) ;
        entUseCountryandAreaCodesClick (self) ;  // enable/hide fields
		entLocalNumber.Text := String (LocalPhoneNumber) ;
		entCanonNumber.Text := String (PhoneCanonical) ;
	    entAlternates.Lines.Assign (AltPhoneNrList) ;
		entPromoteAlternates.Checked := bPromoteAlternates ;

	// dial params
      	entUserName.Text := String (UserName) ;
    	entPassword.Text := String (Password) ;
    	entDomain.Text := String (Domain) ;
    	entCallBackNumber.Text := String (CallBackNumber) ;

    // 5.21 show location
        LabelPBLocation.Caption := 'Phonebook Location: ' + MasRasPBLocationStr [PBLocation] ;

	// device stuff
    	entDeviceName.ItemIndex := MagRasCon.DeviceNameList.IndexOf (String (DeviceName)) ;
    	entDeviceType.Text := String (DeviceType) ;
    	entDevicePort.Text := String (DevicePort) ;

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
  		entAutoDialDll.Text := String (AutoDialDll) ;
	    entAutoDialFunc.Text := String (AutoDialFunc) ;

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
 	   	entIPAddress.Text := String (IPAddress) ;
    	entDNSAddress.Text := String (DNSAddress) ;
    	entDNSAddressAlt.Text := String (DNSAddressAlt) ;
    	entWINSAddress.Text := String (WINSAddress) ;
    	entWINSAddressAlt.Text := String (WINSAddressAlt) ;
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
    	entScript.Text := String (Script) ;
        entTerminalBeforeDial.Checked := bTerminalBeforeDial ;

	// X25
	    entX25PadType.Text := String (X25PadType) ;   // could read list
	    entX25Address.Text := String (X25Address) ;
	    entX25Facilities.Text := String (X25Facilities) ;
	    entX25UserData.Text := String (X25UserData) ;

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
    	entCustomDialDll.Text := String (CustomDialDll) ;
    	entVpnStrategy.ItemIndex := Ord (VpnStrategy) ;
	   	entModemLights.Checked := bModemLights ;
 		entSecureLocalFiles.Checked := bSecureLocalFiles ;
		entPreviewPhoneNumber.Checked := bPreviewPhoneNumber ;
		entSharedPhoneNumbers.Checked := bSharedPhoneNumbers ;
		entPreviewUserPw.Checked := bPreviewUserPw ;
		entPreviewDomain.Checked := bPreviewDomain ;
		entShowDialingProgress.Checked := bShowDialingProgress ;

// display multilink stuff - show all defined devices then which are being used
// Note - NT returns the default number and device as the first sub entry
		if MagRasOSVersion >= OSNT4 then
	    begin
   	    	for I := 0 to MagRasCon.DeviceNameList.Count - 1  do
    	   	begin
		    	with MultilinkList.Items.Add do
	    	  	begin
   			    	Caption := MagRasCon.DeviceNameList [I] ;
            	    Checked := false ;
				   	SubItems.Add ('') ;
				   	SubItems.Add ('') ;
				   	SubItems.Add (MagRasCon.DeviceTypeList [I] ) ;
				end ;
	   		end ;
		    if SubCurTotal <> 0 then
            begin
		   	    for J := 1 to SubCurTotal do
    		   	begin
        		    I := MagRasCon.DeviceNameList.IndexOf (String (SubDeviceName [J])) ;
	        	    if I >= 0 then
    	        	begin
                    // silly special case where devices have the same name!
                    	if (J > 1) and
                        	 (I < MagRasCon.DeviceNameList.Count - 1) then
                        begin
 	                     	if (SubDeviceName [J] =
		                          		SubDeviceName [J - 1]) then inc (I) ;
                        end ;
				    	with MultilinkList.Items [I] do
			    	  	begin
    			    		Checked := true ;
					   		SubItems [0] := String (SubDevicePort [J]) ;
					   		SubItems [1] := String (SubLocalPhoneNumber [J]) ;
					   		SubItems [2] := String (SubDeviceType [J]) ;
						end ;
					end ;
				end ;
			end ;
	    end ;
	end ;
end ;

// set RAS component properties from stuff specified by user

procedure TForm1.PutProperties ;
var
	I, newentries: integer ;
begin
	MagRasEdt.DefaultProps ;    // clear everything, in theory everything is set...
	with MagRasEdt do
    begin

	// Location and phone number, including alternates
		bUseCountryAndAreaCodes := entUseCountryAndAreaCodes.Checked ;
        if entCountryName.ItemIndex >= 0 then
			        CountryId := CountryIds [entCountryName.ItemIndex] ;
		AreaCode := AnsiString (entAreaCode.Text) ;
        try
			CountryCode := StrToInt (entCountryCode.Text) ;
		except
        	CountryCode := CountryId ;
		end ;
		LocalPhoneNumber := AnsiString (entLocalNumber.Text) ;
		PhoneCanonical := AnsiString (entCanonNumber.Text) ;
	    AltPhoneNrList.Assign (entAlternates.Lines) ;
		bPromoteAlternates := entPromoteAlternates.Checked ;

	// dial params
      	UserName := AnsiString (entUserName.Text) ;
    	Password := AnsiString (entPassword.Text) ;
    	Domain := AnsiString (entDomain.Text) ;
    	CallBackNumber := AnsiString (entCallBackNumber.Text) ;

	// device stuff
    	DeviceName := AnsiString (MagRasCon.DeviceNameList [entDeviceName.ItemIndex]) ;
    	DeviceType := AnsiString (entDeviceType.Text) ;
    //	DevicePort   view only

 	// Idle timeout - NT4 and W2K
        case entIdleOption.ItemIndex of
	        0: IdleDisconnectSeconds := RASIDS_Disabled ;
 	        1: IdleDisconnectSeconds := RASIDS_UseGlobalValue ;
            2: IdleDisconnectSeconds := entIdleDisconnectSeconds.Value ;
		end ;

	// auto dial - did not allow these to be edited
  		AutoDialDll := AnsiString (entAutoDialDll.Text);
	    AutoDialFunc := AnsiString (entAutoDialFunc.Text);

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
 	   	IPAddress := AnsiString (entIPAddress.Text);
    	DNSAddress := AnsiString (entDNSAddress.Text);
    	DNSAddressAlt := AnsiString (entDNSAddressAlt.Text);
    	WINSAddress := AnsiString (entWINSAddress.Text);
    	WINSAddressAlt := AnsiString (entWINSAddressAlt.Text);
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
    	Script := AnsiString (entScript.Text);
        bTerminalBeforeDial := entTerminalBeforeDial.Checked ;

	// X25
	    X25PadType := AnsiString (entX25PadType.Text);
	    X25Address := AnsiString (entX25Address.Text);
	    X25Facilities := AnsiString (entX25Facilities.Text);
	    X25UserData := AnsiString (entX25UserData.Text);

 	// multilink and BAP
   	//    	SubEntries   - read only
		DialMode := TDialMode (entDialMode.ItemIndex) ;
		DialExtraPercent := entDialExtraPercent.Value ;
		DialExtraSampleSeconds := entDialExtraSampleSeconds.Value ;
		HangUpExtraPercent := entHangUpExtraPercent.Value ;
		HangUpExtraSampleSeconds := entHangUpExtraSampleSeconds.Value ;

  	// W2K stuff only
   		PType := TPType (entPType.ItemIndex) ;
    	guidId := StringToGUID (entguidId.Text) ;
    	CustomDialDll := AnsiString (entCustomDialDll.Text);
    	VpnStrategy := TVpnStrategy (entVpnStrategy.ItemIndex) ;
	   	bModemLights := entModemLights.Checked ;
 		bSecureLocalFiles := entSecureLocalFiles.Checked ;
		bPreviewPhoneNumber := entPreviewPhoneNumber.Checked ;
		bSharedPhoneNumbers := entSharedPhoneNumbers.Checked ;
		bPreviewUserPw := entPreviewUserPw.Checked ;
		bPreviewDomain := entPreviewDomain.Checked ;
		bShowDialingProgress := entShowDialingProgress.Checked ;

	// multilink - gets messy since may be adding or removing subentries
		if MagRasOSVersion >= OSNT4 then
	    begin

    	 // check how many devices are ticked and keep them
	    	newentries := 0 ;
   		    for I := 0 to MultilinkList.Items.Count - 1 do
    	   	begin
		       with MultilinkList.Items [I] do
        	   begin
           			if Checked then
		           	begin
		    	       	inc (newentries) ;
    	                SubDeviceName [newentries] := AnsiString (Caption);
                       // ignore port
                // same phone numbers for all subentries
        	            SubLocalPhoneNumber [newentries] := AnsiString (entLocalNumber.Text);
                        SubAltPhoneNrList [newentries].Assign (entAlternates.Lines) ;
					   	SubDeviceType [newentries] := AnsiString (SubItems [2]);
					end ;
				end ;
	   		end ;
    	    SubCurTotal := newentries ;
	   	end ;
    end ;
end ;

procedure TForm1.doLoadClick(Sender: TObject);
var
    I, Location: integer ;
begin
// see which entry user selected
	OldName := '' ;
	if ConnList.ItemIndex = -1 then exit ;
    I := ConnList.ItemIndex ;
    OldName := String (MagRasEntryRecs [I].EntryName) ;
    Location := MagRasEntryRecs [I].PBLocation ;  // 5.21 get phonebook location
    MagRasEdt.PBLocation := Location ;   // 5.21 keep location

// load it
	GetEntry ;
	if OldName <> '' then Status.SimpleText := 'Loaded Properties OK' ;
end ;

procedure TForm1.GetEntry ;
var
	errcode: integer ;
begin
// read entry and dial properties
    errcode := MagRasEdt.GetAllEntryProps (AnsiString (Oldname)) ;
    if errcode = 0 then MagRasEdt.GetDialProps (AnsiString (Oldname));
    if errcode <> 0 then
    begin
		OldName := '' ;
		Status.SimpleText := MagRasEdt.StatusStr ;
        beep ;
        exit ;
    end ;

// set-up edit boxes
	FullPropsPages.ActivePage := TabDial ;
    entEntryName.Text := OldName ;
	GetProperties ;
end;

procedure TForm1.entCountryNameChange(Sender: TObject);
begin
    entCountryCode.Text := IntToStr
    				(MagRasEdt.CountryCodes [entCountryName.ItemIndex]) ;
	entCountryId.Text := IntToStr
    				(MagRasEdt.CountryIds [entCountryName.ItemIndex]) ;
end;

procedure TForm1.entUseCountryandAreaCodesClick(Sender: TObject);
begin
   	entCountryName.Enabled := entUseCountryAndAreaCodes.Checked ;
   	entCountryCode.Enabled := entUseCountryAndAreaCodes.Checked ;
   	entAreaCode.Enabled := entUseCountryAndAreaCodes.Checked ;
   	entCanonNumberChange (self) ;
end;

procedure TForm1.NumberChanged(Sender: TObject);
begin
	if entCountryCode.Text = '' then entCountryCode.Text := '0' ;
	entCanonNumber.Text := String (MagRasEdt.GetCanonical
     	 (entUseCountryAndAreaCodes.Checked, StrToInt (entCountryCode.Text),
   			   AnsiString (entAreaCode.Text), AnsiString (entLocalNumber.Text))) ;
   	entCanonNumberChange (self) ;
end;

procedure TForm1.entDeviceNameChange(Sender: TObject);
begin
	entDeviceType.Text :=
    			MagRasCon.DeviceTypeList [entDeviceName.ItemIndex] ;
end;

procedure TForm1.entCanonNumberChange(Sender: TObject);
var
	DispNum, DialNum: AnsiString ;
begin
	MagRasCon.TranslateAddr (0, AnsiString (entCanonNumber.Text), DispNum, DialNum) ;
   	LabelNumberDisp.Caption := 'Display Number: ' + String (DispNum) ;
   	LabelNumberDial.Caption := 'Dialable Number: ' + String (DialNum) ;
end;

procedure TForm1.doScriptOpenClick(Sender: TObject);
begin
	OpenScript.FileName := entScript.Text ;
   	if entScript.Text <> '' then
    			OpenScript.InitialDir := ExtractFileDir (entScript.Text) ;
	if OpenScript.Execute then entScript.Text := OpenScript.FileName ;
end;

procedure TForm1.doScriptViewClick(Sender: TObject);
begin
	if entScript.Text = '' then exit ;
	try
	    ViewScript.Lines.LoadFromFile (entScript.Text) ;
	except
    end ;
end;

procedure TForm1.doSaveClick(Sender: TObject);
var
	errcode: integer ;
    newname: string ;
begin
//  make sure selected a device
	if entDeviceName.ItemIndex < 0 then
    begin
		Status.SimpleText := 'Must Select a Device' ;
        beep ;
        exit ;
   	end ;

// see if renaming entry
	if OldName = '' then exit ;
	newname := trim (entEntryName.Text) ;
	if newname <> OldName then
    begin
	   	if MagRasCon.ValidateName (AnsiString (newname)) <> 0 then
	   	begin
    	   	Status.SimpleText := MagRasCon.StatusStr ;
	       	beep ;
    	   	exit ;
	   	end ;
	 	MagRasCon.EntryName := AnsiString (OldName) ;
		if MagRasCon.RenamePhonebook (AnsiString (newname)) <> 0 then
        begin
			Status.SimpleText := MagRasCon.StatusStr ;
            beep ;
            exit ;
    	end ;
        OldName := newname ;
		RefreshConnList ;
	end ;

// set properties
	PutProperties ;
    errcode := MagRasEdt.PutAllEntryProps (AnsiString (OldName)) ;
    if errcode = 0 then MagRasEdt.PutDialProps (AnsiString (OldName)) ;
    if errcode <> 0 then
    begin
		Status.SimpleText := MagRasEdt.StatusStr ;
        beep ;
    end
	else
    begin
		Status.SimpleText := 'Saved Entry OK' ;
        beep ;
        GetEntry ;  // refresh by getting entry again
    end ;
end;

procedure TForm1.doPropDialClick(Sender: TObject);
begin
	MagRasCon.TranslateDialog (Handle, 0, '') ;
   	entCanonNumberChange (self) ;
	GetDialProps ;  // in case things changed
end;

end.
