unit exedits1;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
TMagRas Edit Entry Simple Example
=================================

Edit the properties of an entry in the RAS phonebook (ie a connection).
This simple version only shows the minimal options that are typically
edited by the user, a complex example (exeditc) shows all properties.

Created by Angus Robertson, Magenta Systems Ltd, England
in 2000, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Last updated: 20th August 2010 

To load this example, the TMagRas components need to have been previously
installed on the component palette.

24 May 2007 - set PBLocation before editing entry
            - use MagRasGetEntryList (in magrasent) instead of MagRasCon.GetPhoneBookEntries
20 Aug 2010 - support RAS WideChar APIs and Unicode for Delphi 2009 and later
			  with new MagRasxxxW units from TMagRas v6 and later

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls,
  magrasconw, magrasapiw, magrasentw, magrasedtw ;

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
    doLoad: TButton;
    doSave: TButton;
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
    LabelPBLocation: TLabel;
    procedure doExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doLoadClick(Sender: TObject);
    procedure doSaveClick(Sender: TObject);
    procedure entCountryNameChange(Sender: TObject);
    procedure entUseCountryandAreaCodesClick(Sender: TObject);
  private
    { Private declarations }
	procedure RefreshConnList ;
	procedure GetEntry ;
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
            ConnList.Items.Add (MagRasEntryRecs [I].EntryName) ;
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
        entEntryName.Text := '' ;
	    entUserName.Text := '' ;
	    entPassword.Text := '' ;
    	entUseCountryandAreaCodes.Checked := false ;
	    entCountryCode.Text := '' ;
    	entAreaCode.Text := '' ;
	    entLocalNumber.Text := '' ;
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


procedure TForm1.doLoadClick(Sender: TObject);
var
    I, Location: integer ;
begin
// see which entry user selected
	OldName := '' ;
	if ConnList.ItemIndex = -1 then exit ;
    I := ConnList.ItemIndex ;
    OldName := MagRasEntryRecs [I].EntryName ;
    Location := MagRasEntryRecs [I].PBLocation ;  // 5.21 get phonebook location
    MagRasEdt.PBLocation := Location ;   // 5.21 keep location

// load it
	GetEntry ;
	if OldName <> '' then Status.SimpleText := 'Loaded Properties OK' ;
end ;

procedure TForm1.GetEntry ;
var
	I, errcode: integer ;
begin
// read entry and dial properties
    errcode := MagRasEdt.GetAllEntryProps (Oldname) ;
    if errcode = 0 then MagRasEdt.GetDialProps (Oldname) ;
    if errcode <> 0 then
    begin
		OldName := '' ;
		Status.SimpleText := MagRasEdt.StatusStr ;
        beep ;
        exit ;
    end ;

// set-up edit boxes
    entEntryName.Text := OldName ;
	with MagRasEdt do
    begin

	// Location and phone number, including alternates
		entUseCountryAndAreaCodes.Checked := bUseCountryAndAreaCodes ;
        entCountryCode.Text := IntToStr (CountryCode) ;
		entAreaCode.Text := AreaCode ;
		entLocalNumber.Text := LocalPhoneNumber ;

	// dial params
      	entUserName.Text := UserName ;
    	entPassword.Text := Password ;

    // 5.21 show location
        LabelPBLocation.Caption := 'Phonebook Location: ' +
                           MasRasPBLocationStr [PBLocation] ;

	// device stuff - can not search listbox becausse it has (type) added
    	entDeviceName.ItemIndex := MagRasCon.DeviceNameList.IndexOf (DeviceName) ;

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
	end ;
	entUseCountryandAreaCodesClick (self) ;   // set a few fields
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
	   	if MagRasCon.ValidateName (newname) <> 0 then
	   	begin
    	   	Status.SimpleText := MagRasCon.StatusStr ;
	       	beep ;
    	   	exit ;
	   	end ;
	 	MagRasCon.EntryName := OldName ;
		if MagRasCon.RenamePhonebook (newname) <> 0 then
        begin
			Status.SimpleText := MagRasCon.StatusStr ;
            beep ;
            exit ;
    	end ;
        OldName := newname ;
		RefreshConnList ;
	end ;

// set properties
	with MagRasEdt do
    begin
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
    	DeviceName := MagRasCon.DeviceNameList [entDeviceName.ItemIndex] ;
    	DeviceType := MagRasCon.DeviceTypeList [entDeviceName.ItemIndex] ;
	end ;
    errcode := MagRasEdt.PutAllEntryProps (OldName) ;
    if errcode = 0 then MagRasEdt.PutDialProps (OldName) ;
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

end.
