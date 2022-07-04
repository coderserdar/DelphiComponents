{
=======================================================================

		KLIB v100
		Serious Software Made in Brazil


		home-page: www.knowhow-online.com.br (sorry, just portuguese)
		authors: Demian Lessa (demian@knowhow-online.com.br) and Leonardo Freitas

		Released under the Netscape Public License Version 1.0
	 (see license.txt)

		Unless otherwise noted, all materials provided in this release
		are copyright © 2001 by KnowHow Informatica Ltda.

=======================================================================
}

unit ukfdClasses;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Windows, Classes, TypInfo, ExptIntf, ToolIntf, DsgnIntf, ukfClasses;

type


{ TKFormCustomModule }

	TKFormCustomModule = class( TCustomModule )
		procedure ExecuteVerb( Index: Integer ); override;
		function GetVerb( Index: Integer ): String; override;
		function GetVerbCount: Integer; override;

	end;

{ TKFormExpert }

	TKFormExpert = class( TIExpert )
	private
		mniKForm,
		mniMultiLine,
		mniSeparator,
		mniNewHeight,
		mniChangeFont: TIMenuItemIntf;

	public
		constructor Create;
		destructor Destroy; override;
		function GetStyle: TExpertStyle; override;
		function GetName: String; override;
		function GetAuthor: String; override;
		function GetComment: String; override;
		function GetPage: String; override;
		function GetGlyph: HICON; override;
		function GetState: TExpertState; override;
		function GetIDString: String; override;
		function GetMenuText: String; override;
		procedure Execute; override;
		procedure mniKFormClick( Sender: TIMenuItemIntf );
		procedure mniMultiLineClick( Sender: TIMenuItemIntf );
		procedure mniNewHeightClick( Sender: TIMenuItemIntf );
		procedure mniChangeFontClick( Sender: TIMenuItemIntf );

	end;

{ TKSimpleMDIExpert }

	TKSimpleMDIExpert = class( TIExpert )
	private
		mniKSimpleMDI: TIMenuItemIntf;

	public
		constructor Create;
		destructor Destroy; override;
		function GetStyle: TExpertStyle; override;
		function GetName: String; override;
		function GetAuthor: String; override;
		function GetComment: String; override;
		function GetPage: String; override;
		function GetGlyph: HICON; override;
		function GetState: TExpertState; override;
		function GetIDString: String; override;
		function GetMenuText: String; override;
		procedure Execute; override;
		procedure mniKSimpleMDIClick( Sender: TIMenuItemIntf );

	end;

{ TKMDIFormExpert }

	TKMDIFormExpert = class( TIExpert )
	private
		mniKMDIForm: TIMenuItemIntf;

	public
		constructor Create;
		destructor Destroy; override;
		function GetStyle: TExpertStyle; override;
		function GetName: String; override;
		function GetAuthor: String; override;
		function GetComment: String; override;
		function GetPage: String; override;
		function GetGlyph: HICON; override;
		function GetState: TExpertState; override;
		function GetIDString: String; override;
		function GetMenuText: String; override;
		procedure Execute; override;
		procedure mniKMDIFormClick( Sender: TIMenuItemIntf );

	end;

{ TKChildExpert }

	TKChildExpert = class( TIExpert )
	private
		mniKChild: TIMenuItemIntf;

	public
		constructor Create;
		destructor Destroy; override;
		function GetStyle: TExpertStyle; override;
		function GetName: String; override;
		function GetAuthor: String; override;
		function GetComment: String; override;
		function GetPage: String; override;
		function GetGlyph: HICON; override;
		function GetState: TExpertState; override;
		function GetIDString: String; override;
		function GetMenuText: String; override;
		procedure Execute; override;
		procedure mniKChildClick( Sender: TIMenuItemIntf );

	end;


{ TKNavMDIFormExpert }

	TKNavMDIFormExpert = class( TIExpert )
	private
		mniKNavMDIForm: TIMenuItemIntf;

	public
		constructor Create;
		destructor Destroy; override;
		function GetStyle: TExpertStyle; override;
		function GetName: String; override;
		function GetAuthor: String; override;
		function GetComment: String; override;
		function GetPage: String; override;
		function GetGlyph: HICON; override;
		function GetState: TExpertState; override;
		function GetIDString: String; override;
		function GetMenuText: String; override;
		procedure Execute; override;
		procedure mniKNavMDIFormClick( Sender: TIMenuItemIntf );

	end;

{ TKDBChildExpert }

	TKDBChildExpert = class( TIExpert )
	private
		mniKDBChild: TIMenuItemIntf;

	public
		constructor Create;
		destructor Destroy; override;
		function GetStyle: TExpertStyle; override;
		function GetName: String; override;
		function GetAuthor: String; override;
		function GetComment: String; override;
		function GetPage: String; override;
		function GetGlyph: HICON; override;
		function GetState: TExpertState; override;
		function GetIDString: String; override;
		function GetMenuText: String; override;
		procedure Execute; override;
		procedure mniKDBChildClick( Sender: TIMenuItemIntf );

	end;


{ TKMDIFormStyleProperty }

	TKMDIFormStyleProperty = class( TStringProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: String; override;

	end;

{ TKChildFormStyleProperty }

	TKChildFormStyleProperty = class( TStringProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: String; override;

	end;

{ TKDefaultValueProperty }

	TKDefaultValueProperty = class( TStringProperty )
	private
		function  GetPropRef: PPropInfo;
		function  GetCompRef: TComponent;
		function  GetPersistenceItem: TKPersistenceItem;

	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues( Proc: TGetStrProc ); override;

		property PropRef: PPropInfo
						 read GetPropRef;
		property CompRef: TComponent
						 read GetCompRef;
		property kpItem: TKPersistenceItem
						 read GetPersistenceItem;

	end;

{ TKPropertyNameProperty }

	TKPropertyNameProperty = class( TStringProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{ TKComponentNameProperty }

	TKComponentNameProperty = class( TStringProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		procedure GetValues( Proc: TGetStrProc ); override;

	end;

{ TKMainMenuProperty }

	TKMainMenuProperty = class( TClassProperty );

{ TKPositionProperty }

	TKPositionProperty = class( TStringProperty )
	public
		function GetAttributes: TPropertyAttributes; override;
		function GetValue: String; override;

	end;

function FindMenuItemIntf( mi: TIMenuItemIntf; const smi: String ): TIMenuItemIntf;
procedure GenerateKnowHowMenu;
procedure GetIntfMenus;
procedure ReleaseIntfMenus;

var
	MainMenu: TIMainMenuIntf = nil;
	mniKnowHow: TIMenuItemIntf = nil;
	MainMenuItems: TIMenuItemIntf = nil;

implementation

uses
	Messages, SysUtils, Graphics, Controls, Forms, ComCtrls, Dialogs, Registry,
	VirtIntf, EditIntf, IStreams, uksyUtils, ukrUtils, ukfdConsts;

procedure GenerateKnowHowMenu;
var
	i: Integer;
	rg: TRegistry;
	tc: TTabControl;
	pi: TForm;
begin
	pi := nil;
  for i := 0 to Pred( Screen.FormCount ) do
		if ( Screen.Forms[i].ClassName = 'TPropertyInspector' ) then
		begin
			pi := Screen.Forms[i];
			Break;
		end;
	tc := Application.MainForm.FindComponent( 'TabControl' ) as TTabControl;
	rg := TRegistry.Create;
	try
		rg.OpenKey( sFormsRegKey, true );
		if rg.ValueExists( 'MultiLine' ) then
			tc.Multiline := rg.ReadBool( 'MultiLine' );
		if rg.ValueExists( 'Height' ) then
			tc.Height := rg.ReadInteger( 'Height' );
		if ( pi <> nil ) then
		begin
			if rg.ValueExists( 'FontName' ) then
				pi.Font.Name := rg.ReadString( 'FontName' );
			if rg.ValueExists( 'FontSize' ) then
				pi.Font.Size := rg.ReadInteger( 'FontSize' );
			if rg.ValueExists( 'FontBold' ) then
			begin
				if rg.ReadBool( 'FontBold' ) then
					pi.Font.Style := pi.Font.Style + [fsBold]
				else
					pi.Font.Style := pi.Font.Style - [fsBold];
			end;
			if rg.ValueExists( 'FontItalic' ) then
			begin
				if rg.ReadBool( 'FontItalic' ) then
					pi.Font.Style := pi.Font.Style + [fsItalic]
				else
					pi.Font.Style := pi.Font.Style - [fsItalic];
			end;
			if rg.ValueExists( 'FontUnderline' ) then
			begin
				if rg.ReadBool( 'FontUnderline' ) then
					pi.Font.Style := pi.Font.Style + [fsUnderline]
				else
					pi.Font.Style := pi.Font.Style - [fsUnderline];
			end;
		end;
	finally
		rg.Free;
	end;
	SendMessage( Application.MainForm.Handle, WM_SIZE, 0, 0 );
end;

procedure GetIntfMenus;
begin
	try
		if ( MainMenu = nil ) then
			MainMenu := ToolServices.GetMainMenu;
		if ( MainMenu <> nil ) and
			 ( MainMenuItems = nil ) then
			MainMenuItems := MainMenu.GetMenuItems;
	except
		ToolServices.RaiseException( ReleaseException );
	end;
end;

function FindMenuItemIntf( mi: TIMenuItemIntf; const smi: String ): TIMenuItemIntf;
var
  i: Integer;
begin
	Result := nil;
  if ( mi <> nil ) then
		 for i := 0 to mi.GetItemCount - 1 do
     begin
       Result := mi.GetItem( i );
       Dialogs.ShowMessage( Result.GetName );
			 if ( CompareText( Result.GetName, smi ) = 0 ) then
         Exit
       else
       begin
				 Result.Free;
				 Result := nil;
       end;
     end;
end;

procedure ReleaseIntfMenus;
begin
	try
		MainMenuItems.Free;
		MainMenu.Free;
		MainMenuItems := nil;
		MainMenu := nil;
	except
		ToolServices.RaiseException( ReleaseException );
	end;
end;

{************************************************************************}
{ TKFormCustomModule }
{************************************************************************}

procedure TKFormCustomModule.ExecuteVerb( Index: Integer );
begin
	case Index of
		0: ShowDialog( 'About KForms...',
									 'KForms are form templates built for improving ' +
									 'overall development productivity. Copyright (c) ' +
									 '1995,99 by KnowHow® Informatica Ltda.', nil, dsOK, boInformation );
	end;
end;

function TKFormCustomModule.GetVerb( Index: Integer ): string;
begin
	case Index of
		0: Result := 'About &KForms...';
	end;
end;

function TKFormCustomModule.GetVerbCount: Integer;
begin
	Result := 1;
end;

{************************************************************************}
{ TKFormExpert }
{************************************************************************}

constructor TKFormExpert.Create;
var
	ml: Boolean;
	rg: TRegistry;
begin
	inherited Create;
	mniKForm := nil;
	mniNewHeight := nil;
	mniMultiLine := nil;
	mniSeparator := nil;
	mniChangeFont := nil;
	rg := TRegistry.Create;
	try
		rg.OpenKey( sFormsRegKey, true );
		ml := rg.ValueExists( 'MultiLine' ) and rg.ReadBool( 'MultiLine' );
	finally
		rg.Free;
	end;
	try
		GetIntfMenus;
		mniKnowHow := MainMenuItems.InsertItem( 6, '&KnowHow', 'mniKnowHow', '',
			0, 0, 0, [mfVisible, mfEnabled], nil );
		mniKForm := mniKnowHow.InsertItem( 0, '&KForm', 'mniKForm', '',
			0, 0, 0, [mfVisible, mfEnabled], mniKFormClick );
		mniSeparator := mniKnowHow.InsertItem( 1, '-', 'mniSeparator', '',
			0, 0, 0, [mfVisible, mfEnabled], nil );
		mniChangeFont := mniKnowHow.InsertItem( 2, '&Inspector Font...', 'mniChangeFont', '',
			0, 0, 0, [mfVisible, mfEnabled], mniChangeFontClick );
		if ml then
		begin
			mniNewHeight := mniKnowHow.InsertItem( 3, '&Palette Height...', 'mniPaletteHeight', '',
				0, 0, 0, [mfVisible, mfEnabled], mniNewHeightClick );
			mniMultiLine := mniKnowHow.InsertItem( 4, 'Multi&Line Palette', 'mniMultiLine', '',
				0, 0, 0, [mfVisible, mfEnabled, mfChecked], mniMultiLineClick );
		end
		else
		begin
			mniNewHeight := mniKnowHow.InsertItem( 3, '&Palette Height...', 'mniPaletteHeight', '',
				0, 0, 0, [mfVisible], mniNewHeightClick );
			mniMultiLine := mniKnowHow.InsertItem( 4, 'Multi&Line Palette', 'mniMultiLine', '',
				0, 0, 0, [mfVisible, mfEnabled], mniMultiLineClick );
		end;
		ReleaseIntfMenus;
	except
		ToolServices.RaiseException( ReleaseException );
	end;
end;

destructor TKFormExpert.Destroy;
begin
	mniKForm.Free;
	mniSeparator.Free;
	mniMultiLine.Free;
	mniNewHeight.Free;
	mniChangeFont.Free;
	mniKnowHow.Free;
	inherited Destroy;
end;

function TKFormExpert.GetStyle: TExpertStyle;
begin
	Result := esForm;
end;

function TKFormExpert.GetName: String;
begin
	Result := 'Form Template'
end;

function TKFormExpert.GetAuthor: String;
begin
	Result := 'Demian Lessa - KnowHow Informatica';
end;

function TKFormExpert.GetComment: String;
begin
	Result := 'TKForm Wizard';
end;

function TKFormExpert.GetPage: String;
begin
	Result := 'KnowHow';
end;

function TKFormExpert.GetGlyph: HICON;
begin
	Result := 0;
end;

function TKFormExpert.GetState: TExpertState;
begin
	Result := [esEnabled];
end;

function TKFormExpert.GetIDString: String;
begin
	Result := 'KnowHow.KForm';
end;

function TKFormExpert.GetMenuText: String;
begin
	Result := '&Form Template';
end;

procedure TKFormExpert.Execute;
var
	mi: TIModuleInterface;
	ssForm, ssUnit: TStrings;
	sModule, sForm, sFile: String;
	imsForm, imsUnit: TIMemoryStream;
	msFormText, msForm, msUnit: TMemoryStream;
begin
	ToolServices.GetNewModuleAndClassName( 'KForm', sModule, sForm, sFile );

	ssForm := TStringList.Create;
	ssUnit := TStringList.Create;
	msForm := TMemoryStream.Create;
	msUnit := TMemoryStream.Create;
	msFormText := TMemoryStream.Create;

	try
		ssForm.Text := Format( DFM_KForm, [sForm, sForm, sForm] );
		ssUnit.Text := Format( UNIT_KForm, [sModule, sForm, 'KForm', sForm, sForm] );
		ssUnit.SaveToStream( msUnit );
		ssForm.SaveToStream( msFormText );
		msFormText.Position := 0;
		ObjectTextToResource( msFormText, msForm );
	finally
		ssUnit.Free;
		ssForm.Free;
		msFormText.Free;
	end;

	msForm.Position := 0;
	msUnit.Position := 0;
	imsForm := TIMemoryStream.Create( msForm );
	imsUnit := TIMemoryStream.Create( msUnit );

{$IFDEF DELPHI4}
	imsForm.StreamOwnership := soOwned;
	imsUnit.StreamOwnership := soOwned;
{$ELSE}
	imsForm.OwnStream := true;
	imsUnit.OwnStream := true;
{$ENDIF}

	mi := ToolServices.CreateModuleEx( sFile, sForm, 'KForm', '', imsUnit,
					imsForm, [cmAddToProject, cmShowSource, cmShowForm,	cmUnNamed,
					cmMarkModified] );
	mi.ShowSource;
	mi.ShowForm;
	mi.Release;
end;

procedure TKFormExpert.mniKFormClick( Sender: TIMenuItemIntf );
begin
	Execute;
end;

procedure TKFormExpert.mniNewHeightClick( Sender: TIMenuItemIntf );
var
	rg: TRegistry;
	tc: TTabControl;
	sHeight: String;
	iHeight, iError: Integer;
begin
	if not ( mfChecked in mniMultiLine.GetFlags ) then Exit;
	tc := Application.MainForm.FindComponent( 'TabControl' ) as TTabControl;
	sHeight := InputBox( 'Set New Height', 'Select the the Palette''s new height:', IntToStr( tc.Height ) );
	Val( sHeight, iHeight, iError );
	if ( iError = 0 ) then
		tc.Height := iHeight;
	rg := TRegistry.Create;
	try
		rg.OpenKey( sFormsRegKey, true );
		rg.WriteInteger( 'Height', tc.Height );
	finally
		rg.Free;
	end;
	SendMessage( Application.MainForm.Handle, WM_SIZE, 0, 0 );
end;

procedure TKFormExpert.mniMultiLineClick( Sender: TIMenuItemIntf );
var
	rg: TRegistry;
	tc: TTabControl;
	mfNH,	mfML: TIMenuFlags;
begin
	mfML := mniMultiLine.GetFlags;
	mfNH := mniNewHeight.GetFlags;
	if ( mfChecked in mfML ) then
	begin
		Exclude( mfML, mfChecked );
		Exclude( mfNH, mfEnabled );
	end
	else
	begin
		Include( mfML, mfChecked );
		Include( mfNH, mfEnabled );
	end;
	mniMultiLine.SetFlags( [mfChecked], mfML );
	mniNewHeight.SetFlags( [mfEnabled], mfNH );
	tc := Application.MainForm.FindComponent( 'TabControl' ) as TTabControl;
	tc.MultiLine := ( mfChecked in mniMultiLine.GetFlags );
	if tc.MultiLine then
		tc.Height := ( tc.Tabs.Count div 7 * 17 ) + 45
	else
		tc.Height := 59;
	SendMessage( Application.MainForm.Handle, WM_SIZE, 0, 0 );
	rg := TRegistry.Create;
	try
		rg.OpenKey( sFormsRegKey, true );
		with rg do
		begin
			WriteBool( 'MultiLine', tc.Multiline );
			WriteInteger( 'Height', tc.Height );
		end;
	finally
		rg.Free;
	end;
end;

procedure TKFormExpert.mniChangeFontClick( Sender: TIMenuItemIntf );
var
	i: Integer;
	rg: TRegistry;
	fd: TFontDialog;
begin
	for i := 0 to Pred( Screen.FormCount ) do
		if ( Screen.Forms[i].ClassName = 'TPropertyInspector' ) then
		begin
			fd := TFontDialog.Create( Application );
			try
				fd.Font := Screen.Forms[i].Font;
				if fd.Execute then
				begin
					Screen.Forms[i].Font := fd.Font;
					rg := TRegistry.Create;
					try
						rg.OpenKey( sFormsRegKey, true );
						with rg do
						begin
							WriteString( 'FontName', fd.Font.Name );
							WriteInteger( 'FontSize', fd.Font.Size );
							WriteBool( 'FontBold', fsBold in fd.Font.Style );
							WriteBool( 'FontItalic', fsItalic in fd.Font.Style );
							WriteBool( 'FontUnderline', fsUnderline in fd.Font.Style );
						end;
					finally
						rg.Free;
					end;
				end;
			finally
				fd.Free;
			end;
			Break;
		end;
end;

{************************************************************************}
{ TKSimpleMDIExpert }
{************************************************************************}

constructor TKSimpleMDIExpert.Create;
begin
	inherited Create;
	mniKSimpleMDI := nil;
	try
		GetIntfMenus;
		if ( mniKnowHow <> nil ) then
			mniKSimpleMDI := mniKnowHow.InsertItem( 1, '&Simple MDIForm',	'mniKSimpleMDI',
				'', 0, 0, 0, [mfVisible, mfEnabled], mniKSimpleMDIClick );
		ReleaseIntfMenus;
	except
		ToolServices.RaiseException( ReleaseException );
	end;
end;

destructor TKSimpleMDIExpert.Destroy;
begin
	mniKSimpleMDI.Free;
	inherited Destroy;
end;

function TKSimpleMDIExpert.GetStyle: TExpertStyle;
begin
	Result := esForm;
end;

function TKSimpleMDIExpert.GetName: String;
begin
	Result := 'Simple MDIForm'
end;

function TKSimpleMDIExpert.GetAuthor: String;
begin
	Result := 'Demian Lessa - KnowHow Informatica';
end;

function TKSimpleMDIExpert.GetComment: String;
begin
	Result := 'TKSimpleMDI Wizard';
end;

function TKSimpleMDIExpert.GetPage: String;
begin
	Result := 'KnowHow';
end;

function TKSimpleMDIExpert.GetGlyph: HICON;
begin
	Result := 0;
end;

function TKSimpleMDIExpert.GetState: TExpertState;
begin
	Result := [esEnabled];
end;

function TKSimpleMDIExpert.GetIDString: String;
begin
	Result := 'KnowHow.KSimpleMDI';
end;

function TKSimpleMDIExpert.GetMenuText: String;
begin
	Result := '&Simple MDIForm';
end;

procedure TKSimpleMDIExpert.Execute;
var
	mi: TIModuleInterface;
	ssForm, ssUnit: TStrings;
	sModule, sForm, sFile: String;
	imsForm, imsUnit: TIMemoryStream;
	msFormText, msForm, msUnit: TMemoryStream;
begin
	ToolServices.GetNewModuleAndClassName( 'KSimpleMDI', sModule, sForm, sFile );

	ssForm := TStringList.Create;
	ssUnit := TStringList.Create;
	msForm := TMemoryStream.Create;
	msUnit := TMemoryStream.Create;
	msFormText := TMemoryStream.Create;

	try
		ssForm.Text := Format( DFM_KSimpleMDI, [sForm, sForm, sForm] );
		ssUnit.Text := Format( UNIT_KForm, [sModule, sForm, 'KSimpleMDI', sForm, sForm] );
		ssUnit.SaveToStream( msUnit );
		ssForm.SaveToStream( msFormText );
		msFormText.Position := 0;
		ObjectTextToResource( msFormText, msForm );
	finally
		ssUnit.Free;
		ssForm.Free;
		msFormText.Free;
	end;

	msForm.Position := 0;
	msUnit.Position := 0;
	imsForm := TIMemoryStream.Create( msForm );
	imsUnit := TIMemoryStream.Create( msUnit );
{$IFDEF DELPHI4}
	imsForm.StreamOwnership := soOwned;
	imsUnit.StreamOwnership := soOwned;
{$ELSE}
	imsForm.OwnStream := true;
	imsUnit.OwnStream := true;
{$ENDIF}

	mi := ToolServices.CreateModuleEx( sFile, sForm, 'KSimpleMDI', '', imsUnit,
					imsForm, [cmAddToProject, cmShowSource, cmShowForm,	cmUnNamed,
					cmMarkModified] );
	mi.ShowSource;
	mi.ShowForm;
	mi.Release;
end;

procedure TKSimpleMDIExpert.mniKSimpleMDIClick( Sender: TIMenuItemIntf );
begin
	Execute;
end;

{************************************************************************}
{ TKMDIFormExpert }
{************************************************************************}

constructor TKMDIFormExpert.Create;
begin
	inherited Create;
	mniKMDIForm := nil;
	try
		GetIntfMenus;
		if ( mniKnowHow <> nil ) then
			mniKMDIForm := mniKnowHow.InsertItem( 2, 'MDIForm with &Menu',	'mniKMDIForm',
				'', 0, 0, 0, [mfVisible, mfEnabled], mniKMDIFormClick );
		ReleaseIntfMenus;
	except
		ToolServices.RaiseException( ReleaseException );
	end;
end;

destructor TKMDIFormExpert.Destroy;
begin
	mniKMDIForm.Free;
	inherited Destroy;
end;

function TKMDIFormExpert.GetStyle: TExpertStyle;
begin
	Result := esForm;
end;

function TKMDIFormExpert.GetName: String;
begin
	Result := 'MDIForm Template'
end;

function TKMDIFormExpert.GetAuthor: String;
begin
	Result := 'Demian Lessa - KnowHow Informatica';
end;

function TKMDIFormExpert.GetComment: String;
begin
	Result := 'TKMDIForm Wizard';
end;

function TKMDIFormExpert.GetPage: String;
begin
	Result := 'KnowHow';
end;

function TKMDIFormExpert.GetGlyph: HICON;
begin
	Result := 0;
end;

function TKMDIFormExpert.GetState: TExpertState;
begin
	Result := [esEnabled];
end;

function TKMDIFormExpert.GetIDString: String;
begin
	Result := 'KnowHow.KMDIForm';
end;

function TKMDIFormExpert.GetMenuText: String;
begin
	Result := 'MDIForm with &Menu';
end;

procedure TKMDIFormExpert.Execute;
var
	mi: TIModuleInterface;
	ssForm, ssUnit: TStrings;
	wYear, wMonth, wDay: Word;
	sModule, sForm, sFile: String;
	imsForm, imsUnit: TIMemoryStream;
	msFormText, msForm, msUnit: TMemoryStream;
begin
	ToolServices.GetNewModuleAndClassName( 'KMDIForm', sModule, sForm, sFile );

	ssForm := TStringList.Create;
	ssUnit := TStringList.Create;
	msForm := TMemoryStream.Create;
	msUnit := TMemoryStream.Create;
	msFormText := TMemoryStream.Create;
	DecodeDate( Now, wYear, wMonth, wDay );

	try
		ssForm.Text := Format( DFM_KMDIForm, [sForm, sForm, sForm] );
		ssUnit.Text := Format( UNIT_KMDIForm, [sModule, sForm, 'KMDIForm',
													 sForm, sForm, wDay, wMonth, wYear, sForm,
													 sForm, sForm, sForm, sForm, sForm, sForm] );
		ssUnit.SaveToStream( msUnit );
		ssForm.SaveToStream( msFormText );
		msFormText.Position := 0;
		ObjectTextToResource( msFormText, msForm );
	finally
		ssUnit.Free;
		ssForm.Free;
		msFormText.Free;
	end;

	msForm.Position := 0;
	msUnit.Position := 0;
	imsForm := TIMemoryStream.Create( msForm );
	imsUnit := TIMemoryStream.Create( msUnit );
{$IFDEF DELPHI4}
	imsForm.StreamOwnership := soOwned;
	imsUnit.StreamOwnership := soOwned;
{$ELSE}
	imsForm.OwnStream := true;
	imsUnit.OwnStream := true;
{$ENDIF}

	mi := ToolServices.CreateModuleEx( sFile, sForm, 'KMDIForm', '', imsUnit,
					imsForm, [cmAddToProject, cmShowSource, cmShowForm,	cmUnNamed,
					cmMarkModified] );
	mi.ShowSource;
	mi.ShowForm;
	mi.Release;
end;

procedure TKMDIFormExpert.mniKMDIFormClick( Sender: TIMenuItemIntf );
begin
	Execute;
end;

{************************************************************************}
{ TKChildExpert }
{************************************************************************}

constructor TKChildExpert.Create;
begin
	inherited Create;
	mniKChild := nil;
	try
		GetIntfMenus;
		if ( mniKnowHow <> nil ) then
			mniKChild := mniKnowHow.InsertItem( 4, 'MDI &Child',	'mniKChild',
				'', 0, 0, 0, [mfVisible, mfEnabled], mniKChildClick );
		ReleaseIntfMenus;
	except
		ToolServices.RaiseException( ReleaseException );
	end;
end;

destructor TKChildExpert.Destroy;
begin
	mniKChild.Free;
	inherited Destroy;
end;

function TKChildExpert.GetStyle: TExpertStyle;
begin
	Result := esForm;
end;

function TKChildExpert.GetName: String;
begin
	Result := 'Child Template'
end;

function TKChildExpert.GetAuthor: String;
begin
	Result := 'Demian Lessa - KnowHow Informatica';
end;

function TKChildExpert.GetComment: String;
begin
	Result := 'TKChild Wizard';
end;

function TKChildExpert.GetPage: String;
begin
	Result := 'KnowHow';
end;

function TKChildExpert.GetGlyph: HICON;
begin
	Result := 0;
end;

function TKChildExpert.GetState: TExpertState;
begin
	Result := [esEnabled];
end;

function TKChildExpert.GetIDString: String;
begin
	Result := 'KnowHow.KChild';
end;

function TKChildExpert.GetMenuText: String;
begin
	Result := 'MDI &Child';
end;

procedure TKChildExpert.Execute;
var
	mi: TIModuleInterface;
	ssForm, ssUnit: TStrings;
	sModule, sForm, sFile: String;
	imsForm, imsUnit: TIMemoryStream;
	msFormText, msForm, msUnit: TMemoryStream;
begin
	ToolServices.GetNewModuleAndClassName( 'KChild', sModule, sForm, sFile );

	ssForm := TStringList.Create;
	ssUnit := TStringList.Create;
	msForm := TMemoryStream.Create;
	msUnit := TMemoryStream.Create;
	msFormText := TMemoryStream.Create;

	try
		ssForm.Text := Format( DFM_KChild, [sForm, sForm, sForm] );
		ssUnit.Text := Format( UNIT_KForm, [sModule, sForm, 'KChild', sForm, sForm] );
		ssUnit.SaveToStream( msUnit );
		ssForm.SaveToStream( msFormText );
		msFormText.Position := 0;
		ObjectTextToResource( msFormText, msForm );
	finally
		ssUnit.Free;
		ssForm.Free;
		msFormText.Free;
	end;

	msForm.Position := 0;
	msUnit.Position := 0;
	imsForm := TIMemoryStream.Create( msForm );
	imsUnit := TIMemoryStream.Create( msUnit );
{$IFDEF DELPHI4}
	imsForm.StreamOwnership := soOwned;
	imsUnit.StreamOwnership := soOwned;
{$ELSE}
	imsForm.OwnStream := true;
	imsUnit.OwnStream := true;
{$ENDIF}

	mi := ToolServices.CreateModuleEx( sFile, sForm, 'KChild', '', imsUnit,
					imsForm, [cmAddToProject, cmShowSource, cmShowForm,	cmUnNamed,
					cmMarkModified] );
	mi.ShowSource;
	mi.ShowForm;
	mi.Release;
end;

procedure TKChildExpert.mniKChildClick( Sender: TIMenuItemIntf );
begin
	Execute;
end;

{************************************************************************}
{ TKNavMDIFormExpert }
{************************************************************************}

constructor TKNavMDIFormExpert.Create;
begin
	inherited Create;
	mniKNavMDIForm := nil;
	try
		GetIntfMenus;
		if ( mniKnowHow <> nil ) then
			mniKNavMDIForm := mniKnowHow.InsertItem( 3, 'MDIForm with &Navigator',
				'mniKNavMDIForm', '', 0, 0, 0, [mfVisible, mfEnabled], mniKNavMDIFormClick );
		ReleaseIntfMenus;
	except
		ToolServices.RaiseException( ReleaseException );
	end;
end;

destructor TKNavMDIFormExpert.Destroy;
begin
	mniKNavMDIForm.Free;
	inherited Destroy;
end;

function TKNavMDIFormExpert.GetStyle: TExpertStyle;
begin
	Result := esForm;
end;

function TKNavMDIFormExpert.GetName: String;
begin
	Result := 'NavMDIForm Template'
end;

function TKNavMDIFormExpert.GetAuthor: String;
begin
	Result := 'Demian Lessa - KnowHow Informatica';
end;

function TKNavMDIFormExpert.GetComment: String;
begin
	Result := 'TKNavMDIForm Wizard';
end;

function TKNavMDIFormExpert.GetPage: String;
begin
	Result := 'KnowHow';
end;

function TKNavMDIFormExpert.GetGlyph: HICON;
begin
	Result := 0;
end;

function TKNavMDIFormExpert.GetState: TExpertState;
begin
	Result := [esEnabled];
end;

function TKNavMDIFormExpert.GetIDString: String;
begin
	Result := 'KnowHow.KNavMDIForm';
end;

function TKNavMDIFormExpert.GetMenuText: String;
begin
	Result := 'MDIForm with &Navigator';
end;

procedure TKNavMDIFormExpert.Execute;
var
	mi: TIModuleInterface;
	ssForm, ssUnit: TStrings;
	wYear, wMonth, wDay: Word;
	sModule, sForm, sFile: String;
	imsForm, imsUnit: TIMemoryStream;
	msFormText, msForm, msUnit: TMemoryStream;
begin
	ToolServices.GetNewModuleAndClassName( 'KNavMDIForm', sModule, sForm, sFile );

	ssForm := TStringList.Create;
	ssUnit := TStringList.Create;
	msForm := TMemoryStream.Create;
	msUnit := TMemoryStream.Create;
	msFormText := TMemoryStream.Create;
	DecodeDate( Now, wYear, wMonth, wDay );

	try
		ssForm.Text := Format( DFM_KNavMDIForm, [sForm, sForm, sForm] );
		ssUnit.Text := Format( UNIT_KNavMDIForm, [sModule, sForm, 'KNavMDIForm',
													 sForm, sForm, wDay, wMonth, wYear, sForm,
													 sForm, sForm, sForm, sForm, sForm, sForm] );
		ssUnit.SaveToStream( msUnit );
		ssForm.SaveToStream( msFormText );
		msFormText.Position := 0;
		ObjectTextToResource( msFormText, msForm );
	finally
		ssUnit.Free;
		ssForm.Free;
		msFormText.Free;
	end;

	msForm.Position := 0;
	msUnit.Position := 0;
	imsForm := TIMemoryStream.Create( msForm );
	imsUnit := TIMemoryStream.Create( msUnit );
{$IFDEF DELPHI4}
	imsForm.StreamOwnership := soOwned;
	imsUnit.StreamOwnership := soOwned;
{$ELSE}
	imsForm.OwnStream := true;
	imsUnit.OwnStream := true;
{$ENDIF}

	mi := ToolServices.CreateModuleEx( sFile, sForm, 'KNavMDIForm', '', imsUnit,
					imsForm, [cmAddToProject, cmShowSource, cmShowForm,	cmUnNamed,
					cmMarkModified] );
	mi.ShowSource;
	mi.ShowForm;
	mi.Release;
end;

procedure TKNavMDIFormExpert.mniKNavMDIFormClick( Sender: TIMenuItemIntf );
begin
	Execute;
end;

{************************************************************************}
{ TKDBChildExpert }
{************************************************************************}

constructor TKDBChildExpert.Create;
begin
	inherited Create;
	mniKDBChild := nil;
	try
		GetIntfMenus;
		if ( mniKnowHow <> nil ) then
			mniKDBChild := mniKnowHow.InsertItem( 5, 'MDI &DBChild',	'mniKDBChild',
				'', 0, 0, 0, [mfVisible, mfEnabled], mniKDBChildClick );
		ReleaseIntfMenus;
	except
		ToolServices.RaiseException( ReleaseException );
	end;
end;

destructor TKDBChildExpert.Destroy;
begin
	mniKDBChild.Free;
	inherited Destroy;
end;

function TKDBChildExpert.GetStyle: TExpertStyle;
begin
	Result := esForm;
end;

function TKDBChildExpert.GetName: String;
begin
	Result := 'Child Template'
end;

function TKDBChildExpert.GetAuthor: String;
begin
	Result := 'Demian Lessa - KnowHow Informatica';
end;

function TKDBChildExpert.GetComment: String;
begin
	Result := 'TKDBChild Wizard';
end;

function TKDBChildExpert.GetPage: String;
begin
	Result := 'KnowHow';
end;

function TKDBChildExpert.GetGlyph: HICON;
begin
	Result := 0;
end;

function TKDBChildExpert.GetState: TExpertState;
begin
	Result := [esEnabled];
end;

function TKDBChildExpert.GetIDString: String;
begin
	Result := 'KnowHow.KDBChild';
end;

function TKDBChildExpert.GetMenuText: String;
begin
	Result := 'MDI &DBChild';
end;

procedure TKDBChildExpert.Execute;
var
	mi: TIModuleInterface;
	ssForm, ssUnit: TStrings;
	sModule, sForm, sFile: String;
	imsForm, imsUnit: TIMemoryStream;
	msFormText, msForm, msUnit: TMemoryStream;
begin
	ToolServices.GetNewModuleAndClassName( 'KDBChild', sModule, sForm, sFile );

	ssForm := TStringList.Create;
	ssUnit := TStringList.Create;
	msForm := TMemoryStream.Create;
	msUnit := TMemoryStream.Create;
	msFormText := TMemoryStream.Create;

	try
		ssForm.Text := Format( DFM_KChild, [sForm, sForm, sForm] );
		ssUnit.Text := Format( UNIT_KForm, [sModule, sForm, 'KDBChild', sForm, sForm] );
		ssUnit.SaveToStream( msUnit );
		ssForm.SaveToStream( msFormText );
		msFormText.Position := 0;
		ObjectTextToResource( msFormText, msForm );
	finally
		ssUnit.Free;
		ssForm.Free;
		msFormText.Free;
	end;

	msForm.Position := 0;
	msUnit.Position := 0;
	imsForm := TIMemoryStream.Create( msForm );
	imsUnit := TIMemoryStream.Create( msUnit );
{$IFDEF DELPHI4}
	imsForm.StreamOwnership := soOwned;
	imsUnit.StreamOwnership := soOwned;
{$ELSE}
	imsForm.OwnStream := true;
	imsUnit.OwnStream := true;
{$ENDIF}

	mi := ToolServices.CreateModuleEx( sFile, sForm, 'KDBChild', '', imsUnit,
					imsForm, [cmAddToProject, cmShowSource, cmShowForm,	cmUnNamed,
					cmMarkModified] );
	mi.ShowSource;
	mi.ShowForm;
	mi.Release;
end;

procedure TKDBChildExpert.mniKDBChildClick( Sender: TIMenuItemIntf );
begin
	Execute;
end;

function TKMDIFormStyleProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paReadOnly, paRevertable];
end;

function TKMDIFormStyleProperty.GetValue: String;
begin
	Result := 'fsMDIForm';
end;

function TKChildFormStyleProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paReadOnly, paRevertable];
end;

function TKChildFormStyleProperty.GetValue: String;
begin
	Result := 'fsMDIChild';
end;

function TKDefaultValueProperty.GetPropRef: PPropInfo;
begin
	Result := nil;
	if ( CompRef <> nil ) then
		Result := TypInfo.GetPropInfo( CompRef.ClassInfo, kpItem.PropertyName );
end;

function TKDefaultValueProperty.GetCompRef: TComponent;
begin
	with kpItem, Designer do
	begin
		if ( Form.FindComponent( ComponentName ) = nil ) and
			 ( CompareText( Form.Name, ComponentName ) <> 0 ) then
		begin
			Result := nil;
			Exit;
		end;
		if ( CompareText( Form.Name, ComponentName ) = 0 ) then
			Result := Form
		else
			Result := Form.FindComponent( ComponentName );
	end;
end;

function TKDefaultValueProperty.GetPersistenceItem: TKPersistenceItem;
begin
	Result := TKPersistenceItem( GetComponent( 0 ) );
end;

function TkDefaultValueProperty.GetAttributes: TPropertyAttributes;
begin
	if ( PropRef <> nil ) and ( PropRef^.PropType^.Kind = tkEnumeration ) then
		Result := [paValueList, paRevertable]
	else
		Result := [paRevertable];
end;

procedure TKDefaultValueProperty.GetValues( Proc: TGetStrProc );
var
	i: Integer;
	pd: PTypeData;
begin
	if ( PropRef = nil ) then Exit;
	if ( PropRef^.PropType^.Kind = tkEnumeration ) then
	begin
		pd := GetTypeData( PropRef^.PropType^ );
		for i := pd^.MinValue to pd^.MaxValue do
			Proc( GetEnumName( PropRef^.PropType^, i ) );
	end;
end;

function TKPropertyNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paValueList, paSortList];
end;

procedure TKPropertyNameProperty.GetValues( Proc: TGetStrProc );
var
	pi: PPropList;
	i, iCount: Integer;
	Component: TComponent;
begin
	if ( CompareText( Designer.Form.Name, TKPersistenceItem( GetComponent( 0 ) ).ComponentName ) <> 0 ) and
		 ( Designer.Form.FindComponent( TKPersistenceItem( GetComponent( 0 ) ).ComponentName ) = nil ) then
		Exit;
	if ( Designer.Form.Name = TKPersistenceItem( GetComponent( 0 ) ).ComponentName ) then
		Component := Designer.Form
	else
		Component := Designer.Form.FindComponent( TKPersistenceItem( GetComponent( 0 ) ).ComponentName );
	iCount := GetPropList( Component.ClassInfo, [tkInteger, tkChar, tkEnumeration, tkString, tkLString], nil );
	GetMem( pi, iCount * SizeOf( PPropInfo ) );
	try
		GetPropList( Component.ClassInfo, [tkInteger, tkChar, tkEnumeration, tkString, tkLString], pi );
		for i := 0 to Pred( iCount ) do
			Proc( pi[i]^.Name );
	finally
		FreeMem( pi, iCount * SizeOf( PPropInfo ) );
	end;
end;

function TKComponentNameProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paValueList, paSortList];
end;

procedure TKComponentNameProperty.GetValues( Proc: TGetStrProc );
var
	i: Integer;
	Component: TComponent;
begin
	Proc( Designer.Form.Name );
	for i := 0 to Pred( Designer.Form.ComponentCount ) do
	begin
		Component := Designer.Form.Components[i];
		if ( Component.Name <> '' ) and ( Component is TControl ) then
			Proc( Component.Name );
	end;
end;

function TKPositionProperty.GetAttributes: TPropertyAttributes;
begin
	Result := [paReadOnly, paRevertable];
end;

function TKPositionProperty.GetValue: String;
begin
	Result := 'poDefaultPosOnly';
end;


end.
