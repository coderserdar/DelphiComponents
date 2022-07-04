//Author: Poul Bak
//Copyright © 2000 - 2005 : Bak-O-Soft (Poul Bak). All rights reserved.
//http://bak-o-soft.dk/
//Mailto:info@bak-o-soft.dk
{}
{Component Version: 6.20.00.00}

{Contains Register routines and component/property-editors.}
 
unit PBPrinterSetupDialogReg;

{$INCLUDE PBDefines.inc}

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	Printers, WinSpool, PBPrinterSetupDialog,
	{$IFDEF COMPILER_MAX_5} DsgnIntf
	{$ELSE}
		DesignIntf, DesignEditors, VCLEditors, RTLConsts
	{$ENDIF};

type
	TPBPrinterSetupdialogReg = class(TPBPrinterSetupdialog);

	TInitialSetupProperty = class(TClassProperty)
	public
		function GetAttributes : TPropertyAttributes; override;
		procedure Edit; override;
	end;

	TIntegerWithListProperty = class(TIntegerProperty)
	protected
		FValueList : TStringList;
	public
		destructor Destroy; override;
		procedure Initialize; override;
		function GetAttributes : TPropertyAttributes; override;
		function GetValue : string; override;
		procedure GetValues(Proc: TGetStrProc); override;
		procedure SetValue(const Value : string); override;
	end;

	TCollateSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TColorSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TDefaultSourceSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TDitherTypeSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TDuplexSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TStringWithListProperty = class(TStringProperty)
	protected
		FValueList : TStringList;
	public
		destructor Destroy; override;
		procedure Initialize; override;
		function GetAttributes : TPropertyAttributes; override;
		procedure GetValues(Proc: TGetStrProc); override;
	end;

	TFormNameSetupProperty = class(TStringWithListProperty)
	public
		procedure GetValues(Proc: TGetStrProc); override;
	end;

	TICMIntentSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TICMMethodSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TMediaTypeSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TOrientationSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TPaperSizeSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TPrinterNameProperty = class(TStringWithListProperty)
	public
		procedure Initialize; override;
	end;

	TPrintQualitySetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

	TTTOptionSetupProperty = class(TIntegerWithListProperty)
	public
		procedure Initialize; override;
	end;

// --------------------- ComponentEditor --------------------------
{$IFDEF COMPILER_MAX_3}
	IFormDesigner = TFormDesigner;
{$ENDIF}
	TPBPrinterSetupEditor = class(TDefaultEditor)
	public
		{$IFDEF COMPILER_MAX_5} constructor Create(AComponent: TComponent;
			ADesigner: IFormDesigner); override;
			procedure EditProperty(PropertyEditor : TPropertyEditor;
				var Continue, FreeEditor : Boolean); override;
		{$ELSE}
			constructor Create(AComponent: TComponent;
				ADesigner: IDesigner); override;
			procedure EditProperty(const PropertyEditor : IProperty;
				var Continue : Boolean); override;
		{$ENDIF}
		destructor Destroy; override;
		procedure ExecuteVerb(Index : Integer); override;
		function GetVerb(Index : Integer) : string; override;
		function GetVerbCount : Integer; override;
	end;

procedure Register;

implementation

// ------------- PropertyEditor for InitialSetupValues  ----------------------
function TInitialSetupProperty.GetAttributes : TPropertyAttributes;
begin
	Result := [paDialog, paSubProperties, paRevertable];
end;

procedure TInitialSetupProperty.Edit;
begin
	if Printer.Printers.Count = 0 then Exit;
	if GetName = 'InitialSetupValues' then
	begin
		if TPBPrinterSetupdialogReg(GetComponent(0)).InheritedExecute then Modified;
	end;
end;

// --------- Ancestor PropertyEditor for all IntegerWithLists  ------------
destructor TIntegerWithListProperty.Destroy;
begin
	FValueList.Free;
	FValueList := nil;
	inherited Destroy;
end;

function TIntegerWithListProperty.GetAttributes : TPropertyAttributes;
begin
	Result := [paValueList, paSortList, paRevertable];
end;

function TIntegerWithListProperty.GetValue : string;
var
	t : integer;
	Name : string;
begin
	Result := inherited GetValue;
	t := 0;
	while t < FValueList.Count do
	begin
		Name := FValueList.Names[t];
		if FValueList.Values[Name] = Result then
		begin
			Result := Name;
			Exit;
		end;
		Inc(t);
	end;
end;

procedure TIntegerWithListProperty.Initialize;
begin
	inherited Initialize;
	FValueList := TStringList.Create;
end;

procedure TIntegerWithListProperty.GetValues(Proc: TGetStrProc);
var
	t : integer;
begin
	t := 0;
	while t < FValueList.Count do
	begin
		Proc(FValueList.Names[t]);
		Inc(t);
	end;
end;

procedure TIntegerWithListProperty.SetValue(const Value : string);
begin
	if FValueList.Values[Value] <> '' then inherited SetValue(FValueList.Values[Value])
	else inherited SetValue(Value);
end;

// --- Specialized PropertyEditors (descends from TIntegerWithListProperty) ---
procedure TCollateSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMCOLLATE_TRUE=' + IntToStr(DMCOLLATE_TRUE));
	FValueList.Add('DMCOLLATE_FALSE=' + IntToStr(DMCOLLATE_FALSE));
end;

procedure TColorSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMCOLOR_COLOR=' + IntToStr(DMCOLOR_COLOR));
	FValueList.Add('DMCOLOR_MONOCHROME=' + IntToStr(DMCOLOR_MONOCHROME));
end;

procedure TDefaultSourceSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMBIN_UPPER=' + IntToStr(DMBIN_UPPER));
	FValueList.Add('DMBIN_LOWER=' + IntToStr(DMBIN_LOWER));
	FValueList.Add('DMBIN_MIDDLE=' + IntToStr(DMBIN_MIDDLE));
	FValueList.Add('DMBIN_MANUAL=' + IntToStr(DMBIN_MANUAL));
	FValueList.Add('DMBIN_ENVELOPE=' + IntToStr(DMBIN_ENVELOPE));
	FValueList.Add('DMBIN_ENVMANUAL=' + IntToStr(DMBIN_ENVMANUAL));
	FValueList.Add('DMBIN_TRACTOR=' + IntToStr(DMBIN_TRACTOR));
	FValueList.Add('DMBIN_SMALLFMT=' + IntToStr(DMBIN_SMALLFMT));
	FValueList.Add('DMBIN_LARGEFMT=' + IntToStr(DMBIN_LARGEFMT));
	FValueList.Add('DMBIN_LARGECAPACITY=' + IntToStr(DMBIN_LARGECAPACITY));
	FValueList.Add('DMBIN_FORMSOURCE=' + IntToStr(DMBIN_FORMSOURCE));
end;

procedure TDitherTypeSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMDITHER_NONE=' + IntToStr(DMDITHER_NONE));
	FValueList.Add('DMDITHER_COARSE=' + IntToStr(DMDITHER_COARSE));
	FValueList.Add('DMDITHER_FINE=' + IntToStr(DMDITHER_FINE));
	FValueList.Add('DMDITHER_LINEART=' + IntToStr(DMDITHER_LINEART));
	FValueList.Add('DMDITHER_GRAYSCALE=' + IntToStr(DMDITHER_GRAYSCALE));
end;

procedure TDuplexSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMDUP_SIMPLEX=' + IntToStr(DMDUP_SIMPLEX));
	FValueList.Add('DMDUP_HORIZONTAL=' + IntToStr(DMDUP_HORIZONTAL));
	FValueList.Add('DMDUP_VERTICAL=' + IntToStr(DMDUP_VERTICAL));
end;

procedure TICMIntentSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMICM_SATURATE=' + IntToStr(DMICM_SATURATE));
	FValueList.Add('DMICM_CONTRAST=' + IntToStr(DMICM_CONTRAST));
	FValueList.Add('DMICM_COLORMETRIC=' + IntToStr(DMICM_COLORMETRIC));
end;

procedure TICMMethodSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMICMMETHOD_NONE=' + IntToStr(DMICMMETHOD_NONE));
	FValueList.Add('DMICMMETHOD_SYSTEM=' + IntToStr(DMICMMETHOD_SYSTEM));
	FValueList.Add('DMICMMETHOD_DRIVER=' + IntToStr(DMICMMETHOD_DRIVER));
	FValueList.Add('DMICMMETHOD_DEVICE=' + IntToStr(DMICMMETHOD_DEVICE));
end;

procedure TMediaTypeSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMMEDIA_STANDARD=' + IntToStr(DMMEDIA_STANDARD));
	FValueList.Add('DMMEDIA_TRANSPARENCY=' + IntToStr(DMMEDIA_TRANSPARENCY));
	FValueList.Add('DMMEDIA_GLOSSY=' + IntToStr(DMMEDIA_GLOSSY));
end;

procedure TOrientationSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMORIENT_PORTRAIT=' + IntToStr(DMORIENT_PORTRAIT));
	FValueList.Add('DMORIENT_LANDSCAPE=' + IntToStr(DMORIENT_LANDSCAPE));
end;

procedure TPaperSizeSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMPAPER_LETTER=' + IntToStr(DMPAPER_LETTER));
	FValueList.Add('DMPAPER_LETTERSMALL=' + IntToStr(DMPAPER_LETTERSMALL));
	FValueList.Add('DMPAPER_TABLOID=' + IntToStr(DMPAPER_TABLOID));
	FValueList.Add('DMPAPER_LEDGER=' + IntToStr(DMPAPER_LEDGER));
	FValueList.Add('DMPAPER_LEGAL=' + IntToStr(DMPAPER_LEGAL));
	FValueList.Add('DMPAPER_STATEMENT=' + IntToStr(DMPAPER_STATEMENT));
	FValueList.Add('DMPAPER_EXECUTIVE=' + IntToStr(DMPAPER_EXECUTIVE));
	FValueList.Add('DMPAPER_A3=' + IntToStr(DMPAPER_A3));
	FValueList.Add('DMPAPER_A4=' + IntToStr(DMPAPER_A4));
	FValueList.Add('DMPAPER_A4SMALL=' + IntToStr(DMPAPER_A4SMALL));
	FValueList.Add('DMPAPER_A5=' + IntToStr(DMPAPER_A5));
	FValueList.Add('DMPAPER_B4=' + IntToStr(DMPAPER_B4));
	FValueList.Add('DMPAPER_B5=' + IntToStr(DMPAPER_B5));
	FValueList.Add('DMPAPER_FOLIO=' + IntToStr(DMPAPER_FOLIO));
	FValueList.Add('DMPAPER_QUARTO=' + IntToStr(DMPAPER_QUARTO));
	FValueList.Add('DMPAPER_10X14=' + IntToStr(DMPAPER_10X14));
	FValueList.Add('DMPAPER_11X17=' + IntToStr(DMPAPER_11X17));
	FValueList.Add('DMPAPER_NOTE=' + IntToStr(DMPAPER_NOTE));
	FValueList.Add('DMPAPER_ENV_9=' + IntToStr(DMPAPER_ENV_9));
	FValueList.Add('DMPAPER_ENV_10=' + IntToStr(DMPAPER_ENV_10));
	FValueList.Add('DMPAPER_ENV_11=' + IntToStr(DMPAPER_ENV_11));
	FValueList.Add('DMPAPER_ENV_12=' + IntToStr(DMPAPER_ENV_12));
	FValueList.Add('DMPAPER_ENV_14=' + IntToStr(DMPAPER_ENV_14));
	FValueList.Add('DMPAPER_CSHEET=' + IntToStr(DMPAPER_CSHEET));
	FValueList.Add('DMPAPER_DSHEET=' + IntToStr(DMPAPER_DSHEET));
	FValueList.Add('DMPAPER_ESHEET=' + IntToStr(DMPAPER_ESHEET));
	FValueList.Add('DMPAPER_ENV_DL=' + IntToStr(DMPAPER_ENV_DL));
	FValueList.Add('DMPAPER_ENV_C5=' + IntToStr(DMPAPER_ENV_C5));
	FValueList.Add('DMPAPER_ENV_C3=' + IntToStr(DMPAPER_ENV_C3));
	FValueList.Add('DMPAPER_ENV_C4=' + IntToStr(DMPAPER_ENV_C4));
	FValueList.Add('DMPAPER_ENV_C6=' + IntToStr(DMPAPER_ENV_C6));
	FValueList.Add('DMPAPER_ENV_C65=' + IntToStr(DMPAPER_ENV_C65));
	FValueList.Add('DMPAPER_ENV_B4=' + IntToStr(DMPAPER_ENV_B4));
	FValueList.Add('DMPAPER_ENV_B5=' + IntToStr(DMPAPER_ENV_B5));
	FValueList.Add('DMPAPER_ENV_B6=' + IntToStr(DMPAPER_ENV_B6));
	FValueList.Add('DMPAPER_ENV_ITALY=' + IntToStr(DMPAPER_ENV_ITALY));
	FValueList.Add('DMPAPER_ENV_MONARCH=' + IntToStr(DMPAPER_ENV_MONARCH));
	FValueList.Add('DMPAPER_ENV_PERSONAL=' + IntToStr(DMPAPER_ENV_PERSONAL));
	FValueList.Add('DMPAPER_FANFOLD_US=' + IntToStr(DMPAPER_FANFOLD_US));
	FValueList.Add('DMPAPER_FANFOLD_STD_GERMAN=' + IntToStr(DMPAPER_FANFOLD_STD_GERMAN));
	FValueList.Add('DMPAPER_FANFOLD_LGL_GERMAN=' + IntToStr(DMPAPER_FANFOLD_LGL_GERMAN));
	FValueList.Add('DMPAPER_ISO_B4=' + IntToStr(DMPAPER_ISO_B4));
	FValueList.Add('DMPAPER_JAPANESE_POSTCARD=' + IntToStr(DMPAPER_JAPANESE_POSTCARD));
	FValueList.Add('DMPAPER_9X11=' + IntToStr(DMPAPER_9X11));
	FValueList.Add('DMPAPER_10X11=' + IntToStr(DMPAPER_10X11));
	FValueList.Add('DMPAPER_15X11=' + IntToStr(DMPAPER_15X11));
	FValueList.Add('DMPAPER_ENV_INVITE=' + IntToStr(DMPAPER_ENV_INVITE));
	FValueList.Add('DMPAPER_LETTER_EXTRA=' + IntToStr(DMPAPER_LETTER_EXTRA));
	FValueList.Add('DMPAPER_LEGAL_EXTRA=' + IntToStr(DMPAPER_LEGAL_EXTRA));
	FValueList.Add('DMPAPER_TABLOID_EXTRA=' + IntToStr(DMPAPER_TABLOID_EXTRA));
	FValueList.Add('DMPAPER_A4_EXTRA=' + IntToStr(DMPAPER_A4_EXTRA));
	FValueList.Add('DMPAPER_LETTER_TRANSVERSE=' + IntToStr(DMPAPER_LETTER_TRANSVERSE));
	FValueList.Add('DMPAPER_A4_TRANSVERSE=' + IntToStr(DMPAPER_A4_TRANSVERSE));
	FValueList.Add('DMPAPER_LETTER_EXTRA_TRANSVERSE=' + IntToStr(DMPAPER_LETTER_EXTRA_TRANSVERSE));
	FValueList.Add('DMPAPER_A_PLUS=' + IntToStr(DMPAPER_A_PLUS));
	FValueList.Add('DMPAPER_B_PLUS=' + IntToStr(DMPAPER_B_PLUS));
	FValueList.Add('DMPAPER_LETTER_PLUS=' + IntToStr(DMPAPER_LETTER_PLUS));
	FValueList.Add('DMPAPER_A4_PLUS=' + IntToStr(DMPAPER_A4_PLUS));
	FValueList.Add('DMPAPER_A5_TRANSVERSE=' + IntToStr(DMPAPER_A5_TRANSVERSE));
	FValueList.Add('DMPAPER_B5_TRANSVERSE=' + IntToStr(DMPAPER_B5_TRANSVERSE));
	FValueList.Add('DMPAPER_A3_EXTRA=' + IntToStr(DMPAPER_A3_EXTRA));
	FValueList.Add('DMPAPER_A5_EXTRA=' + IntToStr(DMPAPER_A5_EXTRA));
	FValueList.Add('DMPAPER_B5_EXTRA=' + IntToStr(DMPAPER_B5_EXTRA));
	FValueList.Add('DMPAPER_A2=' + IntToStr(DMPAPER_A2));
	FValueList.Add('DMPAPER_A3_TRANSVERSE=' + IntToStr(DMPAPER_A3_TRANSVERSE));
	FValueList.Add('DMPAPER_A3_EXTRA_TRANSVERSE=' + IntToStr(DMPAPER_A3_EXTRA_TRANSVERSE));
	FValueList.Add('DMPAPER_USER=' + IntToStr(DMPAPER_USER));
end;

procedure TPrintQualitySetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMRES_DRAFT=' + IntToStr(DMRES_DRAFT));
	FValueList.Add('DMRES_LOW=' + IntToStr(DMRES_LOW));
	FValueList.Add('DMRES_MEDIUM=' + IntToStr(DMRES_MEDIUM));
	FValueList.Add('DMRES_HIGH=' + IntToStr(DMRES_HIGH));
end;

procedure TTTOptionSetupProperty.Initialize;
begin
	inherited Initialize;
	FValueList.Add('DMTT_BITMAP=' + IntToStr(DMTT_BITMAP));
	FValueList.Add('DMTT_DOWNLOAD=' + IntToStr(DMTT_DOWNLOAD));
	FValueList.Add('DMTT_SUBDEV=' + IntToStr(DMTT_SUBDEV));
	FValueList.Add('DMTT_DOWNLOAD_OUTLINE=' + IntToStr(DMTT_DOWNLOAD_OUTLINE));
end;

// --------------- Ancestor PropertyEditor StringWithList  --------------------
destructor TStringWithListProperty.Destroy;
begin
	FValueList.Free;
	FValueList := nil;
	inherited Destroy;
end;

function TStringWithListProperty.GetAttributes : TPropertyAttributes;
begin
	Result := [paValueList, paSortList, paRevertable];
end;

procedure TStringWithListProperty.Initialize;
begin
	inherited Initialize;
	FValueList := TStringList.Create;
end;

procedure TStringWithListProperty.GetValues(Proc: TGetStrProc);
var
	t : integer;
begin
	t := 0;
	while t < FValueList.Count do
	begin
		Proc(FValueList[t]);
		Inc(t);
	end;
end;

// --- Specialized PropertyEditors (descends from TStringWithListProperty) ---
procedure TFormNameSetupProperty.GetValues(Proc: TGetStrProc);
type
	TPaperNameArray = array [1..256, 0..63] of Char;
	PPapernameArray = ^TPaperNameArray;
var
	NumberOfPaperNames, t : integer;
	PPaperNames : PPapernameArray;
begin
	FValueList.Clear;
	with TPBPrinterSetupdialogReg(GetComponent(0)) do
	begin
		if GetPrinterSetup then with FPrinterRecord do
		begin
			NumberOfPaperNames := DeviceCapabilities(PrinterName, Port, DC_PAPERNAMES, nil, PPrinterDevMode);
			if NumberOfPaperNames > 0 then
			begin
				GetMem(PPaperNames, NumberOfPaperNames * 64);
				try
					DeviceCapabilities(PrinterName, Port, DC_PAPERNAMES, Pchar(PPaperNames), PPrinterDevMode);
					for t := 1 to NumberOfPaperNames do	FValueList.Add(PPaperNames^[t]);
				finally
					FreeMem(PPaperNames);
				end;
			end;
			GlobalUnlock(DevModeHandle);
		end;
	end;
	inherited GetValues(Proc);
end;

procedure TPrinterNameProperty.Initialize;
var
	t, p : integer;
begin
	inherited Initialize;
	if Printer.Printers.Count = 0 then Exit;
	for t := 0 to Printer.Printers.Count - 1 do
	begin
		p := Pos(' on ', Printer.Printers[t]);
		if p > 0 then FValueList.Add(Copy(Printer.Printers[t], 1, p - 1))
		else FValueList.Add(Printer.Printers[t]);
	end;
end;

// -------------------- ComponentEditor -------------------
{$IFDEF COMPILER_MAX_5} constructor TPBPrinterSetupEditor.Create(AComponent: TComponent;
	ADesigner: IFormDesigner);
{$ELSE}
	constructor TPBPrinterSetupEditor.Create(AComponent: TComponent;
		ADesigner: IDesigner);
{$ENDIF}
begin
	inherited Create(AComponent, ADesigner);
	with TPBPrinterSetupdialogReg(Component) do
	begin
		SaveSetup;
		InitialSetup;
	end;
end;

destructor TPBPrinterSetupEditor.Destroy;
begin
	TPBPrinterSetupdialogReg(Component).LoadSetup;
	inherited Destroy;
end;

{$IFDEF COMPILER_MAX_5} procedure TPBPrinterSetupEditor.EditProperty(PropertyEditor : TPropertyEditor;
	var Continue, FreeEditor : Boolean);
{$ELSE}
	procedure TPBPrinterSetupEditor.EditProperty(const PropertyEditor : IProperty;
		var Continue : Boolean);
{$ENDIF}
begin
	if CompareText(PropertyEditor.GetName, 'InitialSetupValues') = 0 then
	begin
		PropertyEditor.Edit;
		Continue := False;
	end;
end;

procedure TPBPrinterSetupEditor.ExecuteVerb(Index : Integer);
begin
	if Index = 0 then Edit;
end;

function TPBPrinterSetupEditor.GetVerb(Index : Integer) : string;
begin
	if Index = 0 then Result := 'Edit InitialSetupValues'
	else Result := '';
end;

function TPBPrinterSetupEditor.GetVerbCount : Integer;
begin
	Result :=  1;
end;

procedure Register;
begin
	RegisterComponents('PB', [TPBPrinterSetupDialog]);
	RegisterPropertyEditor(TypeInfo(TInitialSetupValues), TPBPrinterSetupDialog, '', TInitialSetupProperty);
	RegisterPropertyEditor(TypeInfo(TSmallIntWithList), TInitialSetupValues, 'Collate', TCollateSetupProperty);
	RegisterPropertyEditor(TypeInfo(TSmallIntWithList), TInitialSetupValues, 'Color', TColorSetupProperty);
	RegisterPropertyEditor(TypeInfo(TSmallIntWithList), TInitialSetupValues, 'DefaultSource', TDefaultSourceSetupProperty);
	RegisterPropertyEditor(TypeInfo(TCardinalWithList), TInitialSetupValues, 'DitherType', TDitherTypeSetupProperty);
	RegisterPropertyEditor(TypeInfo(TSmallIntWithList), TInitialSetupValues, 'Duplex', TDuplexSetupProperty);
	RegisterPropertyEditor(TypeInfo(TStringWithList), TInitialSetupValues, 'FormName', TFormNameSetupProperty);
	RegisterPropertyEditor(TypeInfo(TCardinalWithList), TInitialSetupValues, 'ICMIntent', TICMIntentSetupProperty);
	RegisterPropertyEditor(TypeInfo(TCardinalWithList), TInitialSetupValues, 'ICMMethod', TICMMethodSetupProperty);
	RegisterPropertyEditor(TypeInfo(TCardinalWithList), TInitialSetupValues, 'MediaType', TMediaTypeSetupProperty);
	RegisterPropertyEditor(TypeInfo(TSmallIntWithList), TInitialSetupValues, 'Orientation', TOrientationSetupProperty);
	RegisterPropertyEditor(TypeInfo(TSmallIntWithList), TInitialSetupValues, 'PaperSize', TPaperSizeSetupProperty);
	RegisterPropertyEditor(TypeInfo(TStringWithList), TInitialSetupValues, 'PrinterName', TPrinterNameProperty);
	RegisterPropertyEditor(TypeInfo(TSmallIntWithList), TInitialSetupValues, 'PrintQuality', TPrintQualitySetupProperty);
	RegisterPropertyEditor(TypeInfo(TSmallIntWithList), TInitialSetupValues, 'TTOption', TTTOptionSetupProperty);
	RegisterComponentEditor(TPBPrinterSetupDialog, TPBPrinterSetupEditor);
end;

end.
