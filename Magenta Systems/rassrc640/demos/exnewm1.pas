unit exnewm1;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas New Entry (modem) Example
=================================

Create a new entry in the RAS phonebook (ie a new connection).
It shows the minimal options required for a new entry, which
will be created with TCP/IP, PPP and dynamic IP addresses.
Takes the modem from the default connection, ignores strange
devices

Created by Angus Robertson, Magenta Systems Ltd, England
in 2000, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 20th August 2010 

To load this example, the TMagRas components need to have been previously
installed on the component palette.

24 May 2007 - set PBLocation when creating phonehook entry
20 Aug 2010 - support RAS WideChar APIs and Unicode for Delphi 2009 and later
			  with new MagRasxxxW units from TMagRas v6 and later
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Registry,
  magrasconw, magrasapiw, magrasedtw ;

type
  TForm1 = class(TForm)
    MagRasCon: TMagRasCon;
    doExit: TButton;
    ConnList: TListBox;
    Label3: TLabel;
    MagRasEdt: TMagRasEdt;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    entEntryName: TEdit;
    Label2: TLabel;
    entDeviceName: TComboBox;
    Label43: TLabel;
    entUserName: TEdit;
    Label45: TLabel;
    entPassword: TEdit;
    doClear: TButton;
    doCreate: TButton;
    entUseCountryandAreaCodes: TCheckBox;
    Label6: TLabel;
    entCountryName: TComboBox;
    Label44: TLabel;
    entCountryCode: TEdit;
    Label11: TLabel;
    entAreaCode: TEdit;
    Label13: TLabel;
    entLocalNumber: TEdit;
    Status: TStatusBar;
    Label4: TLabel;
    LabelDefConn: TLabel;
    entPhoneBook: TRadioGroup;
    procedure doExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doClearClick(Sender: TObject);
    procedure doCreateClick(Sender: TObject);
    procedure entCountryNameChange(Sender: TObject);
    procedure entUseCountryandAreaCodesClick(Sender: TObject);
  private
    { Private declarations }
    procedure RefreshConnList ;
    procedure GetDialProps ;
	function GetDefConn: string ;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  DialProps: TDialProps;        // dialling properties
  DialLocation: TDialLocation;  // default dialling location
  DialCard: TDialCard ;         // default dialling calling card
  DialCountry: TDialCountry ;   // default country dialling info

implementation

{$R *.DFM}

procedure TForm1.doExitClick(Sender: TObject);
begin
    Close ;
end;

// get default connection, as set-up in Internet Options/Properties

function TForm1.GetDefConn: string ;
var
	IniFile: TRegistry ;
const
	RemAcc = 'RemoteAccess' ;
    IntProf = 'InternetProfile' ;
begin
	result := '' ;
    IniFile := TRegistry.Create ;
	Try
    with IniFile do
    begin
    	try
		  	RootKey := HKEY_CURRENT_USER;
	      	if OpenKey (RemAcc, false) then result := ReadString (IntProf) ;
            CloseKey ;
        except
	   end ;
	end ;
	finally
      	if Assigned (IniFile) then IniFile.Free;
	end;
end ;

procedure TForm1.RefreshConnList ;
begin
    // get list of phonebook entries
    MagRasCon.GetPhoneBookEntries ;
 //   MagRasCon.PhoneBookEntries.Sort ;  4.60 already sorted 
    ConnList.Items.Assign (MagRasCon.PhoneBookEntries) ;     // display it
end ;

procedure TForm1.GetDialProps ;
begin
    MagRasCon.GetTransCaps (DialProps, DialLocation, DialCard, DialCountry) ;
end ;

procedure TForm1.FormCreate(Sender: TObject);
var
    I, errcode: integer ;
    S: string ;
begin
// see if RAS has been installed
    if MagRasCon.TestRAS then
    begin
     // get list of phonebook entries
        RefreshConnList ;

    // get list of RAS capable modems and isdn devices - ignore VNP, etc
    // entDeviceName has sorted property set
        MagRasCon.GetDeviceList ;
        if MagRasCon.DeviceNameList.Count <> 0 then
        begin
            for I := 0 to MagRasCon.DeviceNameList.Count - 1 do
            begin
            	S := LowerCase (MagRasCon.DeviceTypeList [I]) ;
                if (S = RASDT_Modem) or (S = RASDT_Isdn) then
	                entDeviceName.Items.Add (MagRasCon.DeviceNameList [I]) ;
            end ;

		// try and find device set for default connection
            S := GetDefConn ;
            LabelDefConn.Caption := S ;
            entDeviceName.ItemIndex := 0 ;   // set first
            if S <> '' then
            begin
			    errcode := MagRasEdt.GetAllEntryProps (S) ;
			    if errcode = 0 then
                begin
                	I := entDeviceName.Items.IndexOf (MagRasEdt.DeviceName) ;
                    if I >= 0 then entDeviceName.ItemIndex := I ;
                end ;
			end ;
        end ;

    // get CountryList, CountryIds and CountryCodes - don't sort list!
        MagRasEdt.GetAllCountryInfo ;
        entCountryName.Items.Assign (MagRasEdt.CountryList) ;

    // get dialling properties, location, calling card, etc
        GetDialProps ;                                     
        doClearClick (self) ;
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


procedure TForm1.doClearClick(Sender: TObject);
var
    I: integer ;
begin
    entEntryName.Text := '' ;
    entUserName.Text := '' ;
    entPassword.Text := '' ;
    entPhoneBook.ItemIndex := MagRasEdt.PBLocation ;  // 5.20 set phonebook file

// DialLocation hold the current location dialling properties
    entUseCountryandAreaCodes.Checked := true ;
    entCountryCode.Text := IntToStr (DialLocation.CountryCode) ;
    entAreaCode.Text := DialLocation.CityCode ;
    entLocalNumber.Text := '' ;

// find country name from country Id
    if MagRasEdt.CountryList.Count = 0 then exit ;
    for I := 0 to Pred (MagRasEdt.CountryList.Count) do
    begin
        if DialLocation.CountryID = cardinal (MagRasEdt.CountryIds [I]) then
        begin
            entCountryName.ItemIndex := I ;
            break ;
        end ;
    end ;
end;

procedure TForm1.entCountryNameChange(Sender: TObject);
begin
    entCountryCode.Text := IntToStr
                    (MagRasEdt.CountryCodes [entCountryName.ItemIndex]) ;
end;

procedure TForm1.entUseCountryandAreaCodesClick(Sender: TObject);
begin
    entCountryName.Enabled := entUseCountryAndAreaCodes.Checked ;
    entCountryCode.Enabled := entUseCountryAndAreaCodes.Checked ;
    entAreaCode.Enabled := entUseCountryAndAreaCodes.Checked ;
end;

procedure TForm1.doCreateClick(Sender: TObject);
var
    errcode, I: integer ;
    newname: string ;
begin
    newname := trim (entEntryName.Text) ;
    if MagRasCon.ValidateName (newname) <> 0 then
    begin
        Status.SimpleText := MagRasCon.StatusStr ;
        beep ;
        exit ;
    end ;

// default all properties for a PPP TCP/IP entry
    MagRasEdt.PPPDefault ;

// set specific properties
    with MagRasEdt do
    begin
    // phonebook
        PBLocation := entPhoneBook.ItemIndex ;  // 5.20 set phonebook file

    // telephone numbers - required
    // CountryId and CountryCode must not be zero if following is checked
        bUseCountryAndAreaCodes := entUseCountryAndAreaCodes.Checked ;
        if entCountryName.ItemIndex >= 0 then
                    CountryId := CountryIds [entCountryName.ItemIndex] ;
        try
            CountryCode := StrToInt (entCountryCode.Text) ;
        except
            CountryCode := 1 ;
        end ;
        AreaCode := entAreaCode.Text ;
        LocalPhoneNumber := entLocalNumber.Text ;

    // dial params - optional
        UserName := entUserName.Text ;
        Password := entPassword.Text ;

    // device stuff - required, must match precisely name and type from lists
    	I := MagRasCon.DeviceNameList.IndexOf
					        (entDeviceName.Items [entDeviceName.ItemIndex]) ;
		if I < 0 then
        begin
	        Status.SimpleText := 'Can Not Find Device' ;
    	    beep ;
        end ;
        DeviceName := MagRasCon.DeviceNameList [I] ;
        DeviceType := MagRasCon.DeviceTypeList [I] ;
    end ;
    errcode := MagRasEdt.PutAllEntryProps (newname) ;
    if errcode = 0 then MagRasEdt.PutDialProps (newname) ;
    if errcode <> 0 then
    begin
        Status.SimpleText := MagRasEdt.StatusStr ;
        beep ;
    end
    else
    begin
        Status.SimpleText := 'Created New Entry OK' ;
        beep ;
    end ;
    RefreshConnList ;
end;

end.
