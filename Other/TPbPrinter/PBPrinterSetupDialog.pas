//Author: Poul Bak
//Copyright © 2000 - 2005 : Bak-O-Soft (Poul Bak). All rights reserved.
//http://bak-o-soft.dk/
//Mailto:info@bak-o-soft.dk
{}
{Component Version: 6.20.00.00}
{}
{TPBPrinterSetupDialog is a TPrinterSetupDialog component with capability
of getting/setting printersetup-values (orientation, papersize etc) at
designtime and runtime. Settings can be saved so users don't have to setup
the printer every time they run your program.}
{Standard PrinterSetupDialog is used as propertyeditor.}
{At runtime you can switch between 4 setuptypes: stDefault (like standard
PrinterSetupDialog), stInitial (settings set at designtime), stSaved (user
defined settings saved) and stUser (settings set when the dialog has executed).}
{AutoSave and ForceInitialSetupValues.}
//
{How to use: First set the InitialSetupOptions (the values you want to change
from default).}
{Then either: Click the ellipse-button by InitialSetupValues (or double-click
the component on the form) to launch a PrinterSetupDialog, change the values
there and press 'Ok'.}
{Or: Double-click +InitialSetupValues to expand the sub-properties. Set the
values by picking from the lists or enter your own values.}
{That's enough - use the other properties if you like.}
//
{Note: When you have selected the PBPrinterSetupDialog component on the form,
the printersettings will follow the settings of the component until you deselect
the component again (or close your project). Pay attention if you print
from Delphi (deselect the component before you press 'Print' to restore the
original settings in Delphi.}

unit PBPrinterSetupDialog;

{$INCLUDE PBDefines.inc}

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Printers, WinSpool;

type
{The types for SetupType.}
	TSetupType = (stDefault, stInitial, stSaved, stUser);

{TInitialSetup is used as a set in TInitialSetupOptions.}
{Used by InitialSetupOptions and ThisPrinterOptions.}
	TInitialSetup = (isCollate, isColor, isCopies, isDefaultSource, isDitherType,
	isDuplex,	isFormName, isICMIntent, isICMMethod, isMediatype, isOrientation,
	isPaperLength, isPaperSize,	isPaperWidth, isPrinterName, isPrintQuality,
	isScale, isTTOption, isYResolution);
{See TInitialSetup.}
	TInitialSetupOptions = set of TInitialSetup;

{A 'dummy' type definition necessary so that the property editor
can show a list of values.}
	TSmallIntWithList = SmallInt;
{A 'dummy' type definition necessary so that the property editor
can show a list of values.}
	TCardinalWithList = Cardinal;
{A 'dummy' type definition necessary so that the property editor
can show a list of values.}
	TStringWithList = string;

{TInitialSetupValues is the class that contains the printersetup properties.}
{See InitialSetupValues and ThisPrinterValues.}
{See 'DEVMODE' in Delphi help.}
	TInitialSetupValues = class(TPersistent)
	private
		{ Private declarations }
		FCollate,	FColor : TSmallIntWithList;
		FComponent : TComponent;
		FCopies : SmallInt;
		FDefaultSource : TSmallIntWithList;
		FDitherType : TCardinalWithList;
		FDuplex : TSmallIntWithList;
		FFormName : TStringWithList;
		FICMIntent, FICMMethod, FMediatype : TCardinalWithList;
		FOrientation, FPaperSize : TSmallIntWithList;
		FPaperLength, FPaperWidth, FScale, FYResolution : SmallInt;
		FPrinterName : TStringWithList;
		FPrintQuality, FTTOption : TSmallIntWithList;
		function StoreFormName : Boolean;
		procedure SetCollate(Value : TSmallIntWithList);
		procedure SetColor(Value : TSmallIntWithList);
		procedure SetCopies(Value : SmallInt);
		procedure SetDefaultSource(Value : TSmallIntWithList);
		procedure SetDitherType(Value : TCardinalWithList);
		procedure SetDuplex(Value : TSmallIntWithList);
		procedure SetFormName(Value : TStringWithList);
		procedure SetICMIntent(Value : TCardinalWithList);
		procedure SetICMMethod(Value : TCardinalWithList);
		procedure SetMediaType(Value : TCardinalWithList);
		procedure SetOrientation(Value : TSmallIntWithList);
		procedure SetPaperLength(Value : SmallInt);
		procedure SetPaperSize(Value : TSmallIntWithList);
		procedure SetPaperWidth(Value : SmallInt);
		procedure SetPrinterName(Value : TStringWithList);
		procedure SetPrintQuality(Value : TSmallIntWithList);
		procedure SetScale(Value : SmallInt);
		procedure SetTTOption(Value : TSmallIntWithList);
		procedure SetYResolution(Value : SmallInt);
	published
{The way the printer handles several copies.}
{Note: Since DMCOLLATE_FALSE is equal to 0 it will show in InitialSetupValues
even though Collate is not included in InitialSetupOptions.}
		property Collate : TSmallIntWithList read FCollate write SetCollate default 0;
{Printing in color or monocrome.}
		property Color : TSmallIntWithList read FColor write SetColor default 0;
{The number of copies the printer produces.}
		property Copies : SmallInt read FCopies write SetCopies default 0;
{The bin the printer gets the paper from.}
		property DefaultSource : TSmallIntWithList read FDefaultSource write SetDefaultSource default 0;
{The way the printer handles dithering.}
		property DitherType : TCardinalWithList read FDitherType write SetDitherType default 0;
{The way the printer handles printing on both sides.}
		property Duplex : TSmallIntWithList read FDuplex write SetDuplex default 0;
{ FormName is the name of PaperSize.}
{Note: FormName is localized by Windows. Do not use if you plan to export your
application.}
{See PaperSize - PaperHeight - PaperWidth}
		property FormName : TStringWithList read FFormName write SetFormName stored StoreFormName;
{Image color matching (ICM) profile.}
		property ICMIntent : TCardinalWithList read FICMIntent write SetICMIntent default 0;
{Image color matching (ICM) method.}
		property ICMMethod : TCardinalWithList read FICMMethod write SetICMMethod default 0;
{The kind of paper being used.}
		property Mediatype : TCardinalWithList read FMediatype write SetMediatype default 0;
{Orientation is either portrait (default for most printers) or landscape.}
		property Orientation : TSmallIntWithList read FOrientation write SetOrientation default 0;
{See PaperSize.}
		property PaperLength : SmallInt read FPaperLength write SetPaperLength default 0;
{Set the papersize to one of the predefined values or use a custom value.}
{PaperSize works together with FormName and PaperHeight and PaperWidth.}
{Either set PaperSize or FormName or PaperHeight and PaperWidth.}
{If you specify more than one then the priority is:
{Priority: FormName has the lowest priority. Then PaperSize and finally
PaperHeight and PaperWidth.}
		property PaperSize : TSmallIntWithList read FPaperSize write SetPaperSize default 0;
{See PaperSize.}
		property PaperWidth : SmallInt read FPaperWidth write SetPaperWidth default 0;
{Set PrinterName property if you use a non-default printer to set up the
InitialSetupValues or if you want to use a particular printer at runtime.
Of course a printer of that name has to be installed on the users computer or
the default printer will be used.}
{If you specify a printer the name will be saved (even when not included in
InitialSetupOptions) to make designtime setup easier but it will only be used at
runtime, if you include the isPrinterName option.}
		property PrinterName : TStringWithList read FPrinterName write SetPrinterName;
{Either a predefined constant or a positive number indicating the Dots Per Inch
horizontally together with a positive number for YResolution indicating Dots Per
Inch vertically.}
		property PrintQuality : TSmallIntWithList read FPrintQuality write SetPrintQuality default 0;
{Scaling in percent of normal (default is 100).}
		property Scale : SmallInt read FScale write SetScale default 0;
{Indicates the way the printer handles True Type fonts.}
		property TTOption : TSmallIntWithList read FTTOption write SetTTOption default 0;
{See PrintQuality.}
		property YResolution : SmallInt read FYResolution write SetYResolution default 0;
	end;

{A record used internally.}
	TPrinterConfig = record
		PrinterName, Driver, Port : array[0..MAX_PATH] of Char;
		SizeOfDeviceMode : Cardinal;
	end;

{Component Version: 6.20.00.00}
{}
{TPBPrinterSetupDialog is a TPrinterSetupDialog component with capability
of getting/setting printersetup-values (orientation, papersize etc) at
designtime and runtime. Settings can be saved so users don't have to setup
the printer every time they run your program.}
{Standard PrinterSetupDialog is used as propertyeditor.}
{At runtime you can switch between 4 setuptypes: stDefault (like standard
PrinterSetupDialog), stInitial (settings set at designtime), stSaved (user
defined settings saved) and stUser (settings set when the dialog has exuted).}
{AutoSave and ForceInitialSetupValues.}
//
{How to use: First set the InitialSetupOptions (the values you want to change
from default).}
{Then either: Click the ellipse-button by InitialSetupValues (or double-click
the component on the form) to launch a PrinterSetupDialog, change the values
there and press 'Ok'.}
{Or: Double-click +InitialSetupValues to expand the sub-properties. Set the
values by picking from the lists or enter your own values.}
{That's enough - use the other properties if you like.}
//
{Note: When you have selected the PBPrinterSetupDialog component on the form,
the printersettings will follow the settings of the component until you deselect
the component again (or close your project). Pay attention if you print
from Delphi (deselect the component before you press 'Print' to restore the
original settings in Delphi.}

	TPBPrinterSetupDialog = class(TPrinterSetupDialog)
	private
		{ Private declarations }
		FFileName : TFileName;
		FAutoSave, FForceInitialSetupValues : Boolean;
		FConfigFile : file of TPrinterConfig;
		FDeviceModeFile : file of Char;
		FSetupType : TSetupType;
		FVersion, DefaultPrinter : string;
		FInitialSetupOptions, FThisPrinterOptions : TInitialSetupOptions;
		FInitialSetupValues, FThisPrinterValues : TInitialSetupValues;
		procedure Dummy(Value : string);
		procedure Dummy1(Value : TInitialSetupOptions);
		procedure Dummy2(Value : TInitialSetupValues);
		procedure SetSetupType(Value : TSetupType);
		procedure SetInitialSetupOptions(Value : TInitialSetupOptions);
		procedure SetInitialSetupValues;
		procedure SetThisPrinterValues;
		procedure SetFileName(Value : TFileName);
	protected
		{ protected declarations }
		DevModeHandle : THandle;
		FPrinterRecord : TPrinterConfig;
		PPrinterDevMode : PDevMode;
		function InheritedExecute : Boolean;
		function GetPrinterSetup: Boolean;
		function SetPrinterSetup: Boolean;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Loaded; override;
{Call Execute at runtime to let user setup his/her printer.}
{See AutoSave and ForceInitialSetupValues.}
		function Execute: Boolean; override;
{Loads a saved Setup from SetupFileName. Includes printername and all settings.}
{Same as setting SetupType to stSaved.}
		procedure LoadSetup;
{Sets printername to the standard printer and sets the printersetup to the default values.}
{Same as setting SetupType to stDefault.}
		procedure DefaultSetup;
{Saves the current printersetup to SetupFileName. Includes printername and all settings.}
{See SetupType.}
		procedure SaveSetup;
{Sets the printersetup to the values you set at designtime.}
{Same as setting SetupType to stInitial.}
		procedure InitialSetup;
	published
{If True then printersetup will be automatically saved when PBPrinterSetupDialog
has Executed, so users don't have to setup the printer again. If False you can
manually call SaveSetup to save.}
		property AutoSave : Boolean read FAutoSave write FAutoSave default True;
{If True then users can only change the non-important values (the values you
don't include in InitialSetupOptions) because every time PBPrinterSetupDialog
has executed it calls InitialSetup.}
		property ForceInitialSetupValues : Boolean read FForceInitialSetupValues
			write FForceInitialSetupValues default False;
{A set of options that decides which setup-values that must be set and which
values should be default. Only options included here will be changed when
calling InitialSetup. Other options won't be affected.}
{You can only set 'public' values of the devmode structure here.}
{Note: FormName is localized by Windows. Do not use if you plan to export your
application.}
{See InitialSetupValues, ThisPrinterOptions and ThisPrinterValues.}
		property InitialSetupOptions : TInitialSetupOptions
			read FInitialSetupOptions write SetInitialSetupOptions default [];
{The actual values that you want to set. You can change the values either by
clicking the ellipse-button which will execute a standard PrinterSetupDialog
(the easy way) or by setting the individual values manually.}
{If you want to change a setting that your own printer(s) doesn't support,
you must set the value manually either by picking a value from the list or type
the value.}
{Changing a value at designtime will automatically update the current
printersettings. If you change a value at runtime the printer will only be
updated if SetupType is stInitial or you can call InitialSetup or set SetupType
to stInitial to update the printersettings.}
{Note: FormName is localized by Windows. Do not use if you plan to export your
application.}
{See InitialSetupOptions, ThisPrinterOptions and ThisPrinterValues.}
{See DEVMODE in Dephi help file to learn more.}
		property InitialSetupValues : TInitialSetupValues
			read FInitialSetupValues write Dummy2;
{The (path and) filename where the setup is saved. Both the printername and all
settings (both 'public' and 'private' members of the devmode structure) are saved.}
{Because of that it is very effective at runtime, but the file should not be
distributed with your application, unless you know that the user has the same
printer as you (in a network for instance).}
{Changing the filename at runtime gives you the possibility of having more than
one setup-file to use in different parts of your application.}
		property SetupFileName : TFileName read FFileName write SetFileName;
{Can have 4 values: stDefault, stInitial, stSaved and stUser.}
{Read to determine the current SetupType, Set to change the SetupType.}
{stDefault: The default setup-values. This is the default printer with default setup
 (both 'private' and 'public' settings are set to default values).}
{stInitial: The values you set in the Object Inspector at designtime. Setting
SetupType to stInitial changes only the values you have included in
InitialSetupOptions.}
{stSaved: Changes all values (incl. printername, 'private' and 'public' members
of the devmode structure) to the values saved in SetupFileName. If SetupFileName
doesn't exist or doesn't fit to one of the connected printers, setting SetupType
to stSaved will have no effect.}
{stUser: A readonly value set when the user might have changed the setup (by
executing PBPrinterSetupDialog) and AutoSave is False.}
{Note: Switching between different SetupTypes does not change InitialSetupValues,
the values stay intact unless you change them individually.}
		property SetupType : TSetupType read FSetupType write SetSetupType;
{A readonly property for informational purpose. Here you can see at designtime,
which Setup values are initialized on the current printer. These values can
normally also be set. Values that have not been initialized might still be available
 though. It depends on the printerdriver.}
		property ThisPrinterOptions : TInitialSetupOptions
			read FThisPrinterOptions write Dummy1 stored False;
{A readonly property for informational purpose. Here you can see at designtime
(and check at runtime) the actual Setup values which have been initialized on
the current printer. These values can normally also be set.}
		property ThisPrinterValues : TInitialSetupValues
			read FThisPrinterValues write Dummy2 stored False;
		property Version : string read FVersion write Dummy stored False;
	end;

// -------------------- implementation -------------------------
implementation
{$I+}

const
	NUMBER_OF_FIELDS = 19;
	dmFieldConstants : array[0..NUMBER_OF_FIELDS - 1] of DWord = (DM_COLLATE,
		DM_COLOR, DM_COPIES, DM_DEFAULTSOURCE, DM_DITHERTYPE, DM_DUPLEX, DM_FORMNAME,
		DM_ICMINTENT, DM_ICMMETHOD, DM_MEDIATYPE, DM_ORIENTATION, DM_PAPERLENGTH,
		DM_PAPERSIZE,	DM_PAPERWIDTH, 0, DM_PRINTQUALITY, DM_SCALE, DM_TTOPTION,
		DM_YRESOLUTION);

constructor TPBPrinterSetupDialog.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FVersion := '6.20.00.00';
	FFileName := 'PrinterSetup.cfg';
	FAutoSave := True;
	FForceInitialSetupValues := False;
	FSetupType := stSaved;
	FInitialSetupOptions := [];
	FInitialSetupValues := TInitialSetupValues.Create;
	FInitialSetupValues.FComponent := Self;
	FThisPrinterValues := TInitialSetupValues.Create;
	FThisPrinterValues.FComponent := nil;
	DefaultSetup;
end;

procedure TPBPrinterSetupDialog.Loaded;
begin
	inherited Loaded;
	if (Printer.Printers.Count = 0) or (csDesigning in ComponentState) then Exit;
	if (FSetupType = stInitial) then InitialSetup
	else if (FSetupType >= stSaved) then
	begin
		if FileExists(FFileName) then LoadSetup
		else FSetupType := stInitial;
	end;
end;

destructor TPBPrinterSetupDialog.Destroy;
begin
	FInitialSetupValues.Free;
	FInitialSetupValues := nil;
	FThisPrinterValues.Free;
	FThisPrinterValues := nil;
	inherited Destroy;
end;

procedure TPBPrinterSetupDialog.Dummy(Value : string); begin end;
procedure TPBPrinterSetupDialog.Dummy1(Value : TInitialSetupOptions); begin end;
procedure TPBPrinterSetupDialog.Dummy2(Value : TInitialSetupValues); begin end;
function TInitialSetupValues.StoreFormName : Boolean;
begin
	Result := (FFormName <> '');
end;

procedure TPBPrinterSetupDialog.SaveSetup;
begin
	if Printer.Printers.Count = 0 then Exit;
	if GetPrinterSetup then
	begin
		AssignFile(FConfigFile, FFileName);
		ReWrite(FConfigFile);
		Write(FConfigFile, FPrinterRecord);
		CloseFile(FConfigFile);
		AssignFile(FDeviceModeFile, FFileName);
		Reset(FDeviceModeFile);
		Seek(FDeviceModeFile, FileSize(FDeviceModeFile));
		with FPrinterRecord do
		begin
			BlockWrite(FDeviceModeFile, PPrinterDevMode^, SizeOfDeviceMode);
		end;
		CloseFile(FDeviceModeFile);
		if not (csDesigning in ComponentState)then FSetupType := stSaved;
		GlobalUnLock(DevModeHandle);
	end;
end;

procedure TPBPrinterSetupDialog.LoadSetup;
var
	Index : integer;
	TempRecord : TPrinterConfig;
begin
	if Printer.Printers.Count = 0 then Exit;
	if FileExists(FFileName) then
	begin
		AssignFile(FConfigFile, FFileName);
		Reset(FConfigFile);
		Read(FConfigFile, TempRecord);
		CloseFile(FConfigFile);
		with TempRecord do
		begin
			Index := 0;
			while (Index < Printer.Printers.Count)
				and (AnsiUpperCase(Copy(Printer.Printers[Index], 1, StrLen(PrinterName)))
				<> AnsiUpperCase(PrinterName)) do Inc(Index);
		end;
		if Index < Printer.Printers.Count then
		begin
			FPrinterRecord := TempRecord;
			with FPrinterRecord do
			begin
				DevModeHandle:=GlobalAlloc(GHND, SizeOfDeviceMode);
				PPrinterDevMode := GlobalLock(DevModeHandle);
				AssignFile(FDeviceModeFile, FFileName);
				Reset(FDeviceModeFile);
				Seek(FDeviceModeFile, SizeOf(FPrinterRecord));
				BlockRead(FDeviceModeFile, PPrinterDevMode^, SizeOfDeviceMode);
				CloseFile(FDeviceModeFile);
				SetPrinterSetup;
				if not (csDesigning in ComponentState)then FSetupType := stSaved;
			end;
		end;
	end;
end;

function TPBPrinterSetupDialog.Execute : Boolean;
begin
	if Printer.Printers.Count = 0 then Result := False
	else
	begin
		Result := inherited Execute;
		if Result then
		begin
			if FForceInitialSetupValues then InitialSetup;
			if FAutoSave then SaveSetup
			else FSetupType := stUser;
			SetThisPrinterValues;
		end;
	end;
end;

procedure TPBPrinterSetupDialog.DefaultSetup;
begin
	if Printer.Printers.Count = 0 then Exit;
	Printer.PrinterIndex := -1;
	SetThisPrinterValues;
	FSetupType := stDefault;
end;

function TPBPrinterSetupDialog.GetPrinterSetup : Boolean;
begin
	if Printer.Printers.Count = 0 then Result := False
	else
	begin
		Result := True;
		ZeroMemory(@FPrinterRecord, SizeOf(FPrinterRecord));
		with FPrinterRecord do
		begin
			Printer.GetPrinter(PrinterName, Driver, Port, DevModeHandle);
			SizeOfDeviceMode := GlobalSize(DevModeHandle);
			if SizeOfDeviceMode = 0 then Result := False
			else
			begin
				PPrinterDevMode := GlobalLock(DevModeHandle);
				if PPrinterDevMode = nil then Result := False;
			end;
		end;
	end;
end;

function TPBPrinterSetupDialog.SetPrinterSetup : Boolean;
begin
	if Printer.Printers.Count = 0 then Result := False
	else
	begin
		with FPrinterRecord do Printer.SetPrinter(PrinterName, Driver, Port, DevModeHandle);
		GlobalUnLock(DevModeHandle);
		SetThisPrinterValues;
		Result := True;
	end;
end;

procedure TPBPrinterSetupDialog.SetSetupType(Value : TSetupType);
begin
	if FSetupType <> Value then FSetupType := Value;
	if (Printer.Printers.Count = 0) or (csDesigning in ComponentState)
		or (csLoading in ComponentState) then Exit;
	if Value = stDefault then DefaultSetup
	else if Value = stInitial then
	begin
		InitialSetup;
		FSetupType := stInitial;
	end
	else if Value = stSaved then LoadSetup;
end;

procedure TPBPrinterSetupDialog.SetFileName(Value : TFileName);
begin
	if FFileName <> Value then FFileName := Value;
end;

procedure TPBPrinterSetupDialog.InitialSetup;
var
	Index : integer;
	t : Byte;
begin
	if Printer.Printers.Count = 0 then Exit;
	if ((csDesigning in ComponentState) or (isPrinterName in FInitialSetupOptions))
		and (FInitialSetupValues.FPrinterName <> '') then
	begin
		Index := Printer.Printers.Count - 1;
		while (Index > -1)
			and (AnsiUpperCase(Copy(Printer.Printers[Index], 1,
			Length(FInitialSetupValues.FPrinterName)))
			<> AnsiUpperCase(FInitialSetupValues.FPrinterName)) do Dec(Index);
		Printer.PrinterIndex := Index;
	end;
	if GetPrinterSetup then with PPrinterDevMode^ do
	begin
		if isCollate in FInitialSetupOptions then dmCollate := FInitialSetupValues.FCollate;
		if isColor in FInitialSetupOptions then	dmColor := FInitialSetupValues.FColor;
		if isCopies in FInitialSetupOptions then dmCopies := FInitialSetupValues.FCopies;
		if isDefaultSource in FInitialSetupOptions then	dmDefaultSource := FInitialSetupValues.FDefaultSource;
		if isDitherType in FInitialSetupOptions then dmDitherType := FInitialSetupValues.FDitherType;
		if isDuplex in FInitialSetupOptions then dmDuplex := FInitialSetupValues.FDuplex;
		if isFormName in FInitialSetupOptions then StrPCopy(dmFormName, FInitialSetupValues.FFormName);
		if isICMIntent in FInitialSetupOptions then	dmICMIntent := FInitialSetupValues.FICMIntent;
		if isICMMethod in FInitialSetupOptions then	dmICMMethod := FInitialSetupValues.FICMMethod;
		if isMediatype in FInitialSetupOptions then dmMediatype := FInitialSetupValues.FMediatype;
		if isOrientation in FInitialSetupOptions then	dmOrientation := FInitialSetupValues.FOrientation;
		if isPaperLength in FInitialSetupOptions then	dmPaperLength := FInitialSetupValues.FPaperLength;
		if isPaperSize in FInitialSetupOptions then	dmPaperSize := FInitialSetupValues.FPaperSize;
		if isPaperWidth in FInitialSetupOptions then dmPaperWidth := FInitialSetupValues.FPaperWidth;
		if isPrintQuality in FInitialSetupOptions then dmPrintQuality := FInitialSetupValues.FPrintQuality;
		if isScale in FInitialSetupOptions then	dmScale := FInitialSetupValues.FScale;
		if isTTOption in FInitialSetupOptions then dmTTOption := FInitialSetupValues.FTTOption;
		if isYResolution in FInitialSetupOptions then dmYResolution := FInitialSetupValues.FYResolution;
		for t := 0 to NUMBER_OF_FIELDS - 1 do
		begin
			if TInitialSetup(t) in FInitialSetupOptions then dmFields := dmFields or dmFieldConstants[t];
		end;
		SetPrinterSetup;
	end;
end;

procedure TPBPrinterSetupDialog.SetInitialSetupValues;
begin
	if Printer.Printers.Count = 0 then Exit;
	if not GetPrinterSetup then Exit;
	if (isPrinterName in FInitialSetupOptions)
		or (FPrinterRecord.PrinterName <> DefaultPrinter)
		then FInitialSetupValues.PrinterName := FPrinterRecord.PrinterName
	else FInitialSetupValues.PrinterName := '';
	with PPrinterDevMode^ do
	begin
		if isCollate in FInitialSetupOptions then FInitialSetupValues.FCollate := dmCollate
		else FInitialSetupValues.FCollate := 0;
		if isColor in FInitialSetupOptions then FInitialSetupValues.FColor := dmColor
		else FInitialSetupValues.FColor := 0;
		if isCopies in FInitialSetupOptions then FInitialSetupValues.FCopies := dmCopies
		else FInitialSetupValues.FCopies := 0;
		if isDefaultSource in FInitialSetupOptions then FInitialSetupValues.FDefaultSource := dmDefaultSource
		else FInitialSetupValues.FDefaultSource := 0;
		if isDitherType in FInitialSetupOptions then FInitialSetupValues.FDitherType := dmDitherType
		else FInitialSetupValues.FDitherType := 0;
		if isDuplex in FInitialSetupOptions then FInitialSetupValues.FDuplex := dmDuplex
		else FInitialSetupValues.FDuplex := 0;
		if isFormName in FInitialSetupOptions then FInitialSetupValues.FFormName := dmFormName
		else FInitialSetupValues.FFormName := '';
		if isICMIntent in FInitialSetupOptions then FInitialSetupValues.FICMIntent := dmICMIntent
		else FInitialSetupValues.FICMIntent := 0;
		if isICMMethod in FInitialSetupOptions then FInitialSetupValues.FICMMethod := dmICMMethod
		else FInitialSetupValues.FICMMethod := 0;
		if isMediatype in FInitialSetupOptions then FInitialSetupValues.FMediatype := dmMediatype
		else FInitialSetupValues.FMediatype := 0;
		if isOrientation in FInitialSetupOptions then FInitialSetupValues.FOrientation := dmOrientation
		else FInitialSetupValues.FOrientation := 0;
		if isPaperLength in FInitialSetupOptions then FInitialSetupValues.FPaperLength := dmPaperLength
		else FInitialSetupValues.FPaperLength := 0;
		if isPaperSize in FInitialSetupOptions then FInitialSetupValues.FPaperSize := dmPaperSize
		else FInitialSetupValues.FPaperSize := 0;
		if isPaperWidth in FInitialSetupOptions then FInitialSetupValues.FPaperWidth := dmPaperWidth
		else FInitialSetupValues.FPaperWidth := 0;
		if isPrintQuality in FInitialSetupOptions then FInitialSetupValues.FPrintQuality := dmPrintQuality
		else FInitialSetupValues.FPrintQuality := 0;
		if isScale in FInitialSetupOptions then FInitialSetupValues.FScale := dmScale
		else FInitialSetupValues.FScale := 0;
		if isTTOption in FInitialSetupOptions then FInitialSetupValues.FTTOption := dmTTOption
		else FInitialSetupValues.FTTOption := 0;
		if isYResolution in FInitialSetupOptions then FInitialSetupValues.FYResolution := dmYResolution
		else FInitialSetupValues.FYResolution := 0;
	end;
	GlobalUnlock(DevModeHandle);
end;

procedure TPBPrinterSetupDialog.SetThisPrinterValues;
var
	t : Byte;
begin
	if Printer.Printers.Count = 0 then Exit;
	if not GetPrinterSetup then Exit;
	for t := 0 to NUMBER_OF_FIELDS - 1 do
	begin
		if (dmFieldConstants[t] and PPrinterDevMode^.dmFields) = dmFieldConstants[t]
			then FThisPrinterOptions := FThisPrinterOptions + [TInitialSetup(t)]
		else FThisPrinterOptions := FThisPrinterOptions - [TInitialSetup(t)];
	end;
	FThisPrinterValues.FPrinterName := FPrinterRecord.PrinterName;
	with PPrinterDevMode^ do
	begin
		FThisPrinterValues.FCollate := dmCollate;
		FThisPrinterValues.FColor := dmColor;
		FThisPrinterValues.FCopies := dmCopies;
		FThisPrinterValues.FDefaultSource := dmDefaultSource;
		FThisPrinterValues.FDitherType := dmDitherType;
		FThisPrinterValues.FDuplex := dmDuplex;
		FThisPrinterValues.FFormName := dmFormName;
		FThisPrinterValues.FICMIntent := dmICMIntent;
		FThisPrinterValues.FICMMethod := dmICMMethod;
		FThisPrinterValues.FMediatype := dmMediatype;
		FThisPrinterValues.FOrientation := dmOrientation;
		FThisPrinterValues.FPaperLength := dmPaperLength;
		FThisPrinterValues.FPaperSize := dmPaperSize;
		FThisPrinterValues.FPaperWidth := dmPaperWidth;
		FThisPrinterValues.FPrintQuality := dmPrintQuality;
		FThisPrinterValues.FScale := dmScale;
		FThisPrinterValues.FTTOption := dmTTOption;
		FThisPrinterValues.FYResolution := dmYResolution;
	end;
	GlobalUnlock(DevModeHandle);
end;

procedure TPBPrinterSetupDialog.SetInitialSetupOptions(Value : TInitialSetupOptions);
begin
	if FInitialSetupOptions <> Value then
	begin
		FInitialSetupOptions := Value;
		if not (csReading	in ComponentState) and (csDesigning in ComponentState)
			then with FInitialSetupValues do
		begin
			if not (isCollate in FInitialSetupOptions) then FCollate := 0
			else if FCollate = 0 then FCollate := FThisPrinterValues.FCollate;
			if not (isColor in FInitialSetupOptions) then FColor := 0
			else if FColor = 0 then FColor := FThisPrinterValues.FColor;
			if not (isCopies in FInitialSetupOptions) then FCopies := 0
			else if FCopies = 0 then FCopies := FThisPrinterValues.FCopies;
			if not (isDefaultSource in FInitialSetupOptions) then FDefaultSource := 0
			else if FDefaultSource = 0 then FDefaultSource := FThisPrinterValues.FDefaultSource;
			if not (isDitherType in FInitialSetupOptions) then FDitherType := 0
			else if FDitherType = 0 then FDitherType := FThisPrinterValues.FDitherType;
			if not (isDuplex in FInitialSetupOptions) then FDuplex := 0
			else if FDuplex = 0 then FDuplex := FThisPrinterValues.FDuplex;
			if not (isFormName in FInitialSetupOptions) then FFormName := ''
			else if FFormName = '' then FFormName := FThisPrinterValues.FFormName;
			if not (isICMIntent in FInitialSetupOptions) then FICMIntent := 0
			else if FICMIntent = 0 then FICMIntent := FThisPrinterValues.FICMIntent;
			if not (isICMMethod in FInitialSetupOptions) then FICMMethod := 0
			else if FICMMethod = 0 then FICMMethod := FThisPrinterValues.FICMMethod;
			if not (isMediaType in FInitialSetupOptions) then FMediaType := 0
			else if FMediaType = 0 then FMediaType := FThisPrinterValues.FMediatype;
			if not (isOrientation in FInitialSetupOptions) then FOrientation := 0
			else if FOrientation = 0 then FOrientation := FThisPrinterValues.FOrientation;
			if not (isPaperLength in FInitialSetupOptions) then FPaperLength := 0
			else if FPaperLength = 0 then FPaperLength := FThisPrinterValues.FPaperLength;
			if not (isPaperSize in FInitialSetupOptions) then FPaperSize := 0
			else if FPaperSize = 0 then FPaperSize := FThisPrinterValues.FPaperSize;
			if not (isPaperWidth in FInitialSetupOptions) then FPaperWidth := 0
			else if FPaperWidth = 0 then FPaperWidth := FThisPrinterValues.FPaperWidth;
			if not (isPrintQuality in FInitialSetupOptions) then FPrintQuality := 0
			else if FPrintQuality = 0 then FPrintQuality := FThisPrinterValues.FPrintQuality;
			if not (isScale in FInitialSetupOptions) then FScale := 0
			else if FScale = 0 then FScale := FThisPrinterValues.FScale;
			if not (isTTOption in FInitialSetupOptions) then FTTOption := 0
			else if FTTOption = 0 then FTTOption := FThisPrinterValues.FTTOption;
			if not (isYResolution in FInitialSetupOptions) then FYResolution := 0
			else if FYResolution = 0 then FYResolution := FThisPrinterValues.FYResolution;
		end;
		if not (csReading	in ComponentState) and ((csDesigning in ComponentState) or (FSetupType = stInitial)) then	InitialSetup;
	end;
end;

function TPBPrinterSetupDialog.InheritedExecute : Boolean;
begin
	InitialSetup;
	Result := inherited Execute;
	if Result then
	begin
		SetInitialSetupValues;
		SetThisPrinterValues;
	end;
end;

//  ------------------- TInitialSetupValues ----------------------
procedure TInitialSetupValues.SetCollate(Value : TSmallIntWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FCollate <> Value) and (isCollate in FInitialSetupOptions) then
		begin
			FCollate := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetColor(Value : TSmallIntWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FColor <> Value) and (isColor in FInitialSetupOptions) then
		begin
			FColor := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetCopies(Value : SmallInt);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FCopies <> Value) and (isCopies in FInitialSetupOptions) then
		begin
			FCopies := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetDefaultSource(Value : TSmallIntWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FDefaultSource <> Value) and (isDefaultSource in FInitialSetupOptions) then
		begin
			FDefaultSource := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetDitherType(Value : TCardinalWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FDitherType <> Value) and (isDitherType in FInitialSetupOptions) then
		begin
			FDitherType := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetDuplex(Value : TSmallIntWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FDuplex <> Value) and (isDuplex in FInitialSetupOptions) then
		begin
			FDuplex := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetFormName(Value : TStringWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FFormName <> Value) and (isFormName in FInitialSetupOptions) then
		begin
			FFormName := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetICMIntent(Value : TCardinalWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FICMIntent <> Value) and (isICMIntent in FInitialSetupOptions) then
		begin
			FICMIntent := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetICMMethod(Value : TCardinalWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FICMMethod <> Value) and (isICMMethod in FInitialSetupOptions) then
		begin
			FICMMethod := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetMediaType(Value : TCardinalWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FMediaType <> Value) and (isMediaType in FInitialSetupOptions) then
		begin
			FMediaType := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetOrientation(Value : TSmallIntWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FOrientation <> Value) and (isOrientation in FInitialSetupOptions) then
		begin
			FOrientation := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetPaperLength(Value : SmallInt);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FPaperLength <> Value) and (isPaperLength in FInitialSetupOptions) then
		begin
			FPaperLength := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetPaperSize(Value : TSmallIntWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FPaperSize <> Value) and (isPaperSize in FInitialSetupOptions) then
		begin
			FPaperSize := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetPaperWidth(Value : SmallInt);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FPaperWidth <> Value) and (isPaperWidth in FInitialSetupOptions) then
		begin
			FPaperWidth := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetPrinterName(Value : TStringWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FPrinterName <> Value) then
		begin
			FPrinterName := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetPrintQuality(Value : TSmallIntWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FPrintQuality <> Value) and (isPrintQuality in FInitialSetupOptions) then
		begin
			FPrintQuality := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetScale(Value : SmallInt);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FScale <> Value) and (isScale in FInitialSetupOptions) then
		begin
			FScale := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetTTOption(Value : TSmallIntWithList);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FTTOption <> Value) and (isTTOption in FInitialSetupOptions) then
		begin
			FTTOption := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

procedure TInitialSetupValues.SetYResolution(Value : SmallInt);
begin
	if FComponent is TPBPrinterSetupDialog then with (FComponent as TPBPrinterSetupDialog) do
	begin
		if (FYResolution <> Value) and (isYResolution in FInitialSetupOptions) then
		begin
			FYResolution := Value;
			if not (csReading	in ComponentState) and ((csDesigning in ComponentState)
				or (FSetupType = stInitial)) then	InitialSetup;
		end;
	end;
end;

end.

