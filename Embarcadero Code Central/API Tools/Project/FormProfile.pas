unit FormProfile;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Registry;

type
	szHwProfGuid= string[HW_PROFILE_GUIDLEN];
	szHwProfName= string[MAX_PROFILE_LEN];

	TfrProfile = class(TForm)
		lbxProfList: TListBox;
		GroupBox1: TGroupBox;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
		Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure lbxProfListClick(Sender: TObject);
	private
		fReg: TRegistry;
		fHwProfList: TStringList;
		fAliasable: BOOL;
		fCloned: BOOL;
		fPrefOrder: DWORD;
		fPristine: BOOL;

{ The HW_PROFILE_INFO structure contains information about a hardware profile.
	The GetCurrentHwProfile function uses this structure to retrieve the current
	hardware profile for the local computer. }
		fHwProfileInfo: THwProfileInfo;

{ A set of bit flags that indicate the docking state of the computer. This member can be a combination of the following values.

	Value										Meaning
	DOCKINFO_DOCKED					The computer is docked. This flag is always set for desktop systems
													that cannot be undocked.
	DOCKINFO_UNDOCKED				The computer is undocked. This flag is always set for desktop systems
													that cannot be undocked.
	DOCKINFO_USER_SUPPLIED	If this flag is set, GetCurrentHwProfile retrieved the current
													docking state from information provided by the user in the
													Hardware Profiles page of the System control panel application.
													Currently, Windows NT and Windows 95 are not able to detect the
													docking state. Consequently, this flag is always set.
	DOCKINFO_USER_DOCKED		The computer is docked, according to information provided by
													the user. This value is a combination of the DOCKINFO_USER_SUPPLIED
													and DOCKINFO_DOCKED flags.
	DOCKINFO_USER_UNDOCKED	The computer is undocked, according to information provided by
													the user. This value is a combination of the DOCKINFO_USER_SUPPLIED
													and DOCKINFO_UNDOCKED flags.
}
		fHwProfDocking: DWORD;

// 	A null-terminated string that contains the globally unique identifier (GUID)
// 	string for the current hardware profile. The string returned by GetCurrentHwProfile
//	encloses the GUID in curly braces {} and includes a null-terminator; for example:
//	"{12340001-4980-1920-6788-123456789012}"
//	You can use this string as a registry subkey under your application's configuration
//	settings key in HKEY_CURRENT_USER. This enables you to store settings for each
//	hardware profile.
		fHwProfGuid: szHwProfGuid;

//	A null-terminated string that contains the display name for the current hardware profile.
		fHwProfName: szHwProfName;

		function DockStateStr: string;
		procedure EnumHWProf;

	public
{ Current hardware profiles }
		property Aliasable: BOOL read fAliasable;
		property Cloned: BOOL read fCloned;
		property HwProfDocking: DWORD read fHwProfDocking;
		property HwProfGuid: szHwProfGuid read fHwProfGuid;
		property HwProfName: szHwProfName read fHwProfName;
		property PrefOrder: DWORD read fPrefOrder;
		property Pristine: BOOL read fPristine;

	end;

var
	frProfile: TfrProfile;

{	The system generates a GUID for each hardware profile and stores it as a string
	in the registry. You can use GetCurrentHwProfile to retrieve the GUID string
	to use as a registry subkey under your application's configuration settings key in
	HKEY_CURRENT_USER. This enables you to store each user's settings for each
	hardware profile. For example, the Colors control panel application could use
	the subkey to store each user's color preferences for different hardware profiles,
	such as profiles for the docked and undocked states.
}

// First define a constant for sub key
const
	HwProfKey= '\SYSTEM\CurrentControlSet\Control\IDConfigDB\Hardware Profiles';

implementation

{$R *.DFM}

// This function enumerates all available or user defined profiles
procedure TfrProfile.EnumHWProf;
begin
	with fReg do
	begin
		// Change root key to HKEY_LOCAL_MACHINE, cos HwProfKey hidden here!
		RootKey:= HKEY_LOCAL_MACHINE;
		// Try to open
		if not OpenKey(HwProfKey, False) then
		ShowMessage('Error opening key') else
		begin
			// Get all available and or user defined profile keys
			// system defines them as 0000, 0001, 0002, ...
			// First key is for a new profile, second is the current profile
			GetKeyNames(fHwProfList);
			// well as you can guess, we are just adding them in a string list so we can
			// use them for other purposes, hard job eh!
			with lbxProfList do
			begin
				Items:= fHwProfList;
				if Items.Count > 0 then
				ItemIndex:= 1 else
				ItemIndex:= 0;
			end;
		end;
	end;
end;

// DockStateStr returns the string representation of the docking state!!!
function TfrProfile.DockStateStr: string;
begin
	result:= 'Unknown';
	case HwProfDocking of
		0: result:= 'Undefined';		// I added this line to make things a little bit excited
		DOCKINFO_UNDOCKED: result:= 'Undocked';
		DOCKINFO_DOCKED: result:= 'Docked';
		DOCKINFO_USER_SUPPLIED: result:= 'User Defined';
		DOCKINFO_USER_UNDOCKED: result:= 'User Undocked';
		DOCKINFO_USER_DOCKED: result:= 'User Docked';
	end;
end;

procedure TfrProfile.FormCreate(Sender: TObject);
begin
	// Lets try to get values of the Current profile, shall we?
	if not GetCurrentHwProfile(fHwProfileInfo) then
	// Windows 95 and NT don't have this luxury, so raise an exception and leave
	// everything alone
	raise Exception.Create('Cannot get hardarwe profile');
	with fHwProfileInfo do
	begin
		fHwProfDocking:= dwDockInfo;
		fHwProfGuid:= UpperCase(szHwProfileGuid);
		fHwProfName:= szHwProfileName;
	end;
	fCloned:= True;
	// registry keys ready to read now
	fReg:= TRegistry.Create;
	// So, profile string list.
	fHwProfList:= TStringList.Create;
	EnumHWProf;	// Now put everything togather
end;

procedure TfrProfile.FormDestroy(Sender: TObject);
begin
	// Do not forget to destroy string list and registry
	fHwProfList.Free;
	fReg.Free;
end;

procedure TfrProfile.FormShow(Sender: TObject);
begin
	// Be nice and show available profile info, don's hassle the user like
	// Microsoft or McDonalds, by asking endless questions...
	Label1.Caption:= ' Profile name		: ' + HwProfName;
	Label2.Caption:= Format(' Docking Value		: %d', [HwProfDocking]);
	Label3.Caption:= ' Docking State		: ' + DockStateStr;
	Label4.Caption:= ' Profile GUID		: ' + HwProfGuid;
	if Aliasable then
	Label5.Caption:= ' Aliasable		: Yes' else
	Label5.Caption:= ' Aliasable		: No';
	if Cloned then
	Label6.Caption:= ' Cloned			: Yes' else
	Label6.Caption:= ' Cloned			: No';
	Label7.Caption:= Format(' Preference Order	: $%x (%0:d)', [PrefOrder]);
	if Pristine then
	Label8.Caption:= '  Original Profile		: Yes' else
	Label8.Caption:= '  Original Profile		: No';
	Label9.Caption:= HwProfKey;
end;

procedure TfrProfile.lbxProfListClick(Sender: TObject);
begin
	with fReg do
	if OpenKey(HwProfKey+'\'+lbxProfList.Items[lbxProfList.ItemIndex], False) then
	begin
		GetValueNames(fHwProfList);

		if ValueExists('FriendlyName') then
		fHwProfName:=  ReadString(fHwProfList.Strings[fHwProfList.IndexOf('FriendlyName')]) else
		fHwProfName:= 'Undefined';

		if ValueExists('DockState') then
		fHwProfDocking:= ReadInteger(fHwProfList.Strings[fHwProfList.IndexOf('DockState')]) else
		fHwProfDocking:= 0;

		if ValueExists('HwProfileGuid') then
		fHwProfGuid:= UpperCase(ReadString(fHwProfList.Strings[fHwProfList.IndexOf('HwProfileGuid')])) else
		fHwProfGuid:= 'Undefined';

		if ValueExists('Aliasable') then
		fAliasable:= ReadBool(fHwProfList.Strings[fHwProfList.IndexOf('Aliasable')]) else
		fAliasable:= False;

		if ValueExists('Cloned') then
		fCloned:= ReadBool(fHwProfList.Strings[fHwProfList.IndexOf('Cloned')]) else
		fCloned:= False;

		if ValueExists('Pristine') then
		fPristine:= ReadBool(fHwProfList.Strings[fHwProfList.IndexOf('Pristine')]) else
		fPristine:= False;

		if ValueExists('PreferenceOrder') then
		fPrefOrder:= ReadInteger(fHwProfList.Strings[fHwProfList.IndexOf('PreferenceOrder')]) else
		fPrefOrder:= 0;
		FormShow(Sender);
	end;
end;

end.
