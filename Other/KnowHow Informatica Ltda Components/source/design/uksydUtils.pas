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

unit uksydUtils;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
	Classes, Graphics, DsgnIntf, ToolIntf, EditIntf, FileIntf, uksyUtils, uksyPackReg;

{
--------------------------------------------------------------------------------
----------------------- Generic Exception Architecture -------------------------
--------------------------------------------------------------------------------
}

type

{ Design-Time Package Main Exceptions }

  EKDSystem = class( EKSystem );
	EKDKernel = class( EKKernel );
	EKDStd = class( EKStd );
	EKDExt = class( EKExt );
	EKDWinAPI = class( EKWinAPI );
	EKDDBCtrls = class( EKDBCtrls );
	EKDDB = class( EKDB );
	EKDCOM = class( EKCOM );
	EKDExperts = class( EKExperts );
	EKDBarCode = class( EKBarCode );
	EKDCrypto = class( EKCrypto );
	EKDDialogs = class( EKDialogs );
	EKDDBDialogs = class( EKDBDialogs );
	EKDForms = class( EKForms );
	EKDCPL = class( EKCPL );

{
--------------------------------------------------------------------------------
----------------------- Generic Design-time Exceptions -------------------------
--------------------------------------------------------------------------------
}

	EKSYDUtils = class( EKDSystem );
	EKPropertyEditor = class( EKSYDUtils );
	EKOTA = class( EKSYDUtils );

	TIVirtualFileSystemClass = class of TIVirtualFileSystem;

{
--------------------------------------------------------------------------------
----------------------------- Generic Routines----------------------------------
--------------------------------------------------------------------------------
}

type

  TKFormDesigner = {$IFDEF DELPHI4}IFormDesigner{$ELSE}TFormDesigner{$ENDIF};

procedure GetProjectInfo( FileList, UnitList, FormList: TStrings );
function GetDesignerCanvas( Designer: TKFormDesigner ): TCanvas;
function GetRootCompDesigner( Root: TComponent ): TKFormDesigner;
function GetDesignerRootComp( Designer: TKFormDesigner ): TComponent;
function GetDesignerRootCompName( Designer: TKFormDesigner ): string;
procedure GetRepositoryPages( PageList: TStrings );
procedure AddCodeInsightTemplate( const ShortCut, Description, Code: string );
procedure GetRegisteredModules( ss: TStrings );
procedure GetRegisteredComponents( const ModuleName: string; ss: TStrings );
procedure GetAllRegisteredComponents( ss: TStrings );
procedure RegisterHiddenComponents( const Page: string;
	const Components: array of TComponentClass );

{
--------------------------------------------------------------------------------
------------------------- Generic Information Dialogs --------------------------
--------------------------------------------------------------------------------
}

function SelectDsgnForms( IsSorted: Boolean ): string;
function SelectDsgnUnits( IsSorted: Boolean ): string;
function SelectRegisteredComponents( IsSorted: Boolean ): string;

{
--------------------------------------------------------------------------------
------------------------ Design-Time RTTI Routines------------------------------
--------------------------------------------------------------------------------
}

function SetCompListPubProp( CompList: TComponentList; const PropName: String;
	const PropValue; ProcessChildren: Boolean ): Boolean;
function SetCompListPubProps( CompList: TComponentList; PropNameList: TStrings;
	const PropValue; ProcessChildren: Boolean ): Boolean;

{
--------------------------------------------------------------------------------
-------------------------- Open Tools API Routines------------------------------
--------------------------------------------------------------------------------
}

{ Menu Interface Routines }

type

	TKInsertAction = ( iaNone, iaBefore, iaAfter, iaChild );

function FindMenuItem( const Name, Caption: string ): TIMenuItemIntf;
function InsertMenuItem( Action: TKInsertAction; const TargetName, Caption,
	Name, Hint: string; ShortCut, Context, GroupIndex: Integer; Flags: TIMenuFlags;
	EventHandler: TIMenuClickEvent ): TIMenuItemIntf;

{ Editor Interface Routines }

type
	TKEditorExecuteMethod = procedure ( Module: TIModuleInterface;
		Editor: TIEditorInterface; Data: Pointer ) of object;

	TKEditorExecuteProc = procedure ( Module: TIModuleInterface;
		Editor: TIEditorInterface; Data: Pointer );

function CompareBlocks( Block1, Block2: TCharPos ): ShortInt;
procedure ReplaceSelection( Editor: TIEditorInterface; ViewNum: Integer;
	Text: string; MaintainBlockVisible: Boolean );

procedure ExecuteCurrentEditorInterfaceByMethod( Proc: TKEditorExecuteMethod;
	RaiseExceptions: Boolean; Data: Pointer );
procedure ExecuteCurrentEditorInterfaceByProc( Proc: TKEditorExecuteProc;
	RaiseExceptions: Boolean; Data: Pointer );
procedure ExecuteEditorInterfaceByMethod( const ModuleName: string;
	Proc: TKEditorExecuteMethod; RaiseExceptions: Boolean; Data: Pointer );
procedure ExecuteEditorInterfaceByProc( const ModuleName: string;
	Proc: TKEditorExecuteProc; RaiseExceptions: Boolean; Data: Pointer );

function SetModuleSyntaxHighlighter( const ModuleName: string;
	SyntaxHighlighter: TSyntaxHighlighter ): TSyntaxHighlighter;
function SetModuleBlockType( const ModuleName: string;
	BlockType: TBlockType ): TBlockType;

procedure InsertCode( Editor: TIEditorInterface; Position: Cardinal; const Text: string );
procedure ClearEditor( Editor: TIEditorInterface );
procedure SetBufferPosition( const FileName: string; LineNumber: Integer );

{ Module Interface Routines }

function SaveModuleInterface( const FileName: string; ForceSave,
	TryToMarkFormModified: Boolean ): Boolean;
function IsModIntfBufferModified( const FileName: string ): Boolean;

{ Form Interface Routines }

type
	TKFormExecuteMethod = procedure ( Module: TIModuleInterface;
		Form: TIFormInterface; Data: Pointer ) of object;

	TKFormExecuteProc = procedure ( Module: TIModuleInterface;
		Form: TIFormInterface; Data: Pointer );

function ModifyFormInterface( const FileName: string ): Boolean;

procedure ExecuteCurrentFormInterfaceByMethod( Proc: TKFormExecuteMethod;
	RaiseExceptions: Boolean; Data: Pointer );
procedure ExecuteCurrentFormInterfaceByProc( Proc: TKFormExecuteProc;
	RaiseExceptions: Boolean; Data: Pointer );
procedure ExecuteFormInterfaceByMethod( const ModuleName: string;
	Proc: TKFormExecuteMethod; RaiseExceptions: Boolean; Data: Pointer );
procedure ExecuteFormInterfaceByProc( const ModuleName: string;
	Proc: TKFormExecuteProc; RaiseExceptions: Boolean; Data: Pointer );

{ Resource Interface Routines }

procedure CopyResourceFromStream( Stream: TStream; ResEntry: TIResourceEntry );

{ Component Interface Routines }

procedure CreateComponent( const FormName: string; Comp: TComponent );
procedure CreateComponentEx( const ModuleName: string; Comp: TComponent );
procedure SetSubProperties( fi: TIFormInterface; ci: TIComponentInterface;
	Comp: TComponent );

{##NI##}

procedure InternalCreateComponent( mi: TIModuleInterface; Comp: TComponent );
procedure InternalCreateComponentEx( fi: TIFormInterface; Owner: TIComponentInterface;
	Comp: TComponent );

{##NI##}

{
--------------------------------------------------------------------------------
-------------------------- Strings Editor Routines------------------------------
--------------------------------------------------------------------------------
}

{##RI##}
function EditTStrings( Source: TPersistent; const PropName, VFSFileName: string;
	VFSClass: TIVirtualFileSystemClass; Designer: TKFormDesigner; ss: TStrings;
	sh: TSyntaxHighlighter; MultiSelect: Boolean ): Boolean;
{##RI##}
function EditString( Source: TPersistent; const PropName, VFSFileName: string;
	Designer: TKFormDesigner; var s: string; sh: TSyntaxHighlighter;
	MultiSelect: Boolean ): Boolean;

var
	StringsEditorFont: TFont = nil;

{##NI##}
{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDsgnShareWareVersion;       { PackageWide test }
procedure TestSystemDsgnShareWareVersion; { System Specific test }
{##NI##}

implementation

uses
	SysUtils, ExptIntf, TypInfo, Registry, Forms, Controls, StdCtrls, uksyResStr,
	uksyTypes, uksyConsts, uksyClasses, uksydClasses, uksydTypes, uksydConsts,
	uksydfStrEdit;

{
--------------------------------------------------------------------------------
----------------------------- Generic Routines----------------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type
	PProjectInfo = ^TProjectInfo;
	TProjectInfo = record
		FileList: TStrings;
		UnitList: TStrings;
		FormList: TStrings;
	end;

function EnumUnitsProc( Param: Pointer; const FileName, UnitName,
	FormName: string ): Boolean stdcall;
begin
	Result := True;
	with PProjectInfo( Param )^ do
	begin
		if CheckObject( FileList ) then
			FileList.Add( FileName );
		if CheckObject( UnitList ) then
			UnitList.Add( UnitName );
		if CheckObject( FormList ) then
			FormList.Add( FormName );
	end;
end;

{---------------------------- Public Implementation ----------------------------}

procedure GetProjectInfo( FileList, UnitList, FormList: TStrings );
var
	pi: TProjectInfo;
begin
	if ( not ( CheckObject( FileList ) or CheckObject( UnitList ) or
	  CheckObject( FormList ) ) ) then
		RaiseException( EKSYDUtils, sErrInvProjInfoParams );
	ZeroMemory( @pi, SizeOf( TProjectInfo ) );
	if CheckStrings( FileList ) then
		FileList.Clear;
	pi.FileList := FileList;
	if CheckStrings( UnitList ) then
		UnitList.Clear;
	pi.UnitList := UnitList;
	if CheckStrings( FormList ) then
		FormList.Clear;
	pi.FormList := FormList;
	if ( not ToolServices.EnumProjectUnits( EnumUnitsProc, @pi ) ) then
	  RaiseException( EKSYDUtils, sErrInvProjInfoEnum );
end;

function GetDesignerCanvas( Designer: TKFormDesigner ): TCanvas;
begin
	ForceDesigner( Designer );
	ForceObject( Designer.Form );
	Result := Designer.Form.Canvas;
end;

function GetRootCompDesigner( Root: TComponent ): TKFormDesigner;
var
	frm: TCustomForm;
begin
	ForceObject( Root );
	Result := nil;
	frm := nil;
	if CheckObjectClass( Root, TCustomForm ) then
		frm := ( Root as TCustomForm )
	else if ( CheckObject( Root.Owner ) and
		CheckObjectClass( Root.Owner, TCustomForm ) ) then
		frm := ( Root.Owner as TCustomForm );
	if ( CheckObject( frm ) and CheckDesignerClass( frm.Designer{$IFNDEF DELPHI4}, TFormDesigner {$ENDIF}) ) then
		Result := ( frm.Designer as TKFormDesigner );
end;

function GetDesignerRootCompName( Designer: TKFormDesigner ): string;
var
	c: TComponent;
begin
	Result := '';
	c := GetDesignerRootComp( Designer );
	if CheckObject( c ) then
		Result := c.Name;
	ForceTrimStr( Result );
end;

function GetDesignerRootComp( Designer: TKFormDesigner ): TComponent;
var
	i: Integer;
begin
	ForceDesigner( Designer );
	ForceObject( Designer.Form );
	Result := nil;
{ for more information see ukdsyConsts for theses constants... }
	with Designer.Form do
		if CheckStrEqual( ClassName, COMPONENT_DESINGER_CLASSNAME ) then
		begin
			for i := 0 to ComponentCount - 1 do
				if ( ( not CheckObjectClass( Components[i], TControl ) ) and
					 CheckObjectClass( Components[i], TComponent ) ) then
				begin
					{ The unique Component is the Root! The second test is just for safety }
					Result := Components[i];
					Break;
				end
		end
		else if CheckStrEqual( ClassName, WINCONTROL_DESIGNER_CLASSNAME ) then
		begin
			for i := 0 to ComponentCount - 1 do
				if CheckObjectClass( Components[i], TWinControl ) then
				begin
					{ The unique WinControl is the Root! }
					Result := Components[i];
					Break;
				end;
		end
		else
			Result := Designer.Form;
end;

procedure GetRepositoryPages( PageList: TStrings );
var
	fs: TFileStream;
begin
  ForceObject( PageList );
  fs := TFileStream.Create( CurrentDelphiRootDir + '\Bin\' + sDelphiDRO, fmOpenRead );
	try
    GetSectionFromStream( fs, REPOSITORY_PAGES_SECTION, PageList );
	finally
    fs.Free;
  end;
end;

procedure AddCodeInsightTemplate( const ShortCut, Description, Code: string );
var
	s: string;
	fs: TFileStream;
begin
	{$IFNDEF DELPHI3}
	RaiseException( EKSYDUtils, sErrInvCodeInsightAdd );
	{$ENDIF}
	ForceTrimStrs( [ShortCut, Description, Code] );
	s := CurrentDelphiRootDir + '\Bin\' + sDelphiDCI;
	ForceFile( s );
	fs := TFileStream.Create( s, fmOpenWrite );
	try
		s := Format( CODE_INSIGHT_TEMPLATE_PATTERN, [ShortCut, Description, Code] );
		fs.Position := fs.Size;
		fs.WriteBuffer( Pointer( s )^, Length( s ) );
	finally
		fs.Free;
	end;
end;

procedure GetRegisteredModules( ss: TStrings );
var
	i: Integer;
begin
	ForceObject( ss );
	ss.BeginUpdate;
	try
		ss.Clear;
		with ToolServices do
			for i := 0 to GetModuleCount - 1 do
				ss.Add( GetModuleName( i ) );
	finally
		ss.EndUpdate;
	end;
end;

procedure GetRegisteredComponents( const ModuleName: string; ss: TStrings );
var
	i,
	j: Integer;
begin
	ForceTrim( [ss, ModuleName] );
	ss.BeginUpdate;
	try
		with ToolServices do
			for i := 0 to GetModuleCount - 1 do
				if CheckStrEqual( ModuleName, GetModuleName( i ) ) then
				begin
					for j := 0 to GetComponentCount( i ) - 1 do
						ss.Add( GetComponentName( i, j ) );
					Exit;
				end;
	finally
		ss.EndUpdate;
	end;
end;

procedure GetAllRegisteredComponents( ss: TStrings );
var
  i: Integer;
	sMod: TStrings;
begin
	ForceObject( ss );
	ss.BeginUpdate;
	try
		ss.Clear;
		sMod := TStringList.Create;
		try
			GetRegisteredModules( sMod );
			ForceStrings( sMod );
			for i := 0 to sMod.Count - 1 do
			  GetRegisteredComponents( sMod[i], ss );			          
		finally
			sMod.Free;
		end;
	finally
		ss.EndUpdate;
	end;
end;

procedure RegisterHiddenComponents( const Page: string;
  const Components: array of TComponentClass );
var
	s: string;
	i: Integer;
	sl1,
	sl2: TStrings;
	Reg: TRegIniFile;
begin
	Reg := TRegIniFile.Create( ToolServices.GetBaseRegistryKey );
	try
		s := Reg.ReadString( DELPHI_REG_PALLETE_SECTION, Page + DELPHI_HIDDEN_PAGE_PATTERN, '' );
		for i := Low( Components ) to High( Components ) do
			s := s + Components[i].ClassName + CH_LIST_TOKEN;
		sl1 := TStringList.Create;
		try
			ExtractStrings( s, CH_LIST_TOKEN, sl1 );
			sl2 := TStringList.Create;
			try
				for i := 0 to sl1.Count - 1 do
					if ( sl2.IndexOf( sl1[i] ) = -1 ) then
						sl2.Add( sl1[i] );
				s := StringReplace( sl2.Text, CH_CRLF, CH_LIST_TOKEN, krfAll );
				if ( AnsiLastChar( s ) <> CH_LIST_TOKEN ) then
					s := s + CH_LIST_TOKEN;
				Reg.WriteString( DELPHI_REG_PALLETE_SECTION, Page + DELPHI_HIDDEN_PAGE_PATTERN, s );
			finally
			  sl2.Free;
			end;
		finally
			sl1.Free;
		end;
	finally
		Reg.Free;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------- Generic Information Dialogs --------------------------
--------------------------------------------------------------------------------
}

function SelectDsgnForms( IsSorted: Boolean ): string;
var
	i: Integer;
	ss: TStrings;
begin                               
	Result := '';
	ss := TStringList.Create;
	try
		TStringList( ss ).Sorted := IsSorted;
		GetProjectInfo( nil, nil, ss );
		i := InputListDialog( sSelForm, sSelFormName, 0, ss );
		if ( i <> -1 ) then
			Result := ss[i];
	finally
		ss.Free;
	end;
end;

function SelectDsgnUnits( IsSorted: Boolean ): string;
var
	i: Integer;
	ss: TStrings;
begin
	Result := '';
	ss := TStringList.Create;
	try
		TStringList( ss ).Sorted := IsSorted;
		GetProjectInfo( nil, ss, nil );
		i := InputListDialog( sSelUnit, sSelUnitName, 0, ss );
		if ( i <> -1 ) then
			Result := ss[i];
	finally
		ss.Free;
	end;
end;

function SelectRegisteredComponents( IsSorted: Boolean ): string;
var
	i: Integer;
	ss: TStrings;
begin
	Result := '';
	ss := TStringList.Create;
	try
		GetAllRegisteredComponents( ss );
		TStringList( ss ).Sorted := IsSorted;
		i := InputListDialog( sSelRegComp, sCompClass, 0, ss );
		if ( i <> -1 ) then
			Result := ss[i];
	finally
		ss.Free;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------ Design-Time RTTI Routines------------------------------
--------------------------------------------------------------------------------
}

function SetCompListPubProp( CompList: TComponentList; const PropName: String;
	const PropValue; ProcessChildren: Boolean ): Boolean;
var
	i: Integer;
begin
	ForceTrim( [CompList, PropName] ); { Cannot pass PropValue into Array ! }
	ForceReference( PropValue );
	Result := True;
	for i := 0 to CompList.Count-1 do
		Result := Result or SetPubProp( CompList.Items[i], PropName, PropValue,
			ProcessChildren );
end;

function SetCompListPubProps( CompList: TComponentList; PropNameList: TStrings;
	const PropValue; ProcessChildren: Boolean ): Boolean;
var
	i: Integer;
begin
	ForceTrim( [CompList, PropNameList] );
	ForceReference( PropValue );
	Result := True;
	for i := 0 to PropNameList.Count-1 do
		Result := Result and SetCompListPubProp( CompList, PropNameList.Strings[i],
			PropValue, ProcessChildren );
end;

{
--------------------------------------------------------------------------------
-------------------------- Open Tools API Routines------------------------------
--------------------------------------------------------------------------------
}

function FindMenuItem( const Name, Caption: string ): TIMenuItemIntf;
var
	MainMenu: TIMainMenuIntf;
begin
	MainMenu := ToolServices.GetMainMenu;
	if ( not CheckObject( MainMenu ) ) then
		RaiseException( EKOTA, sErrMainMenuIntfNotFound );
	try
		Result := MainMenu.FindMenuItem( Name );
		if ( not CheckObject( Result ) ) then
			RaiseExceptionFmt( EKOTA, sErrMenuIntfNotFound, [Name, Caption] );
	finally
		MainMenu.Free;
	end;
end;

function InsertMenuItem( Action: TKInsertAction; const TargetName, Caption,
	Name, Hint: string; ShortCut, Context, GroupIndex: Integer; Flags: TIMenuFlags;
	EventHandler: TIMenuClickEvent ): TIMenuItemIntf;
var
	TargetItem: TIMenuItemIntf;
	ParentItem: TIMenuItemIntf;
	Index: Integer;
begin
  Result := nil;
	{ Look up the target menu item. }
	TargetItem := FindMenuItem( TargetName, Caption );
	if CheckObject( TargetItem ) then
		try
			if ( Action = iaChild ) then
			begin
				{ Create a child of the target item. }
				ParentItem := TargetItem;
				ParentItem.AddRef; { avoid a double free }
				Index := 0;
			end
			else
			begin
				{ To create a sibling of the target, get the
					target’s parent. }
				ParentItem := TargetItem.GetParent;
				Index := TargetItem.GetIndex;
				if Action = iaAfter then
					Inc(Index);
			end;
			if ( not CheckObject( ParentItem ) ) then
				RaiseExceptionFmt( EKOTA, sErrMenuIntfNullParent, [TargetName] );
			try
				Result := ParentItem.InsertItem( Index, Caption, Name, Hint, ShortCut,
					Context, GroupIndex, Flags, EventHandler );
			finally
				ParentItem.Free;
			end;
		finally
			TargetItem.Free;
		end
	else
    ForceObject( TargetItem );
end;

function CompareBlocks( Block1, Block2: TCharPos ): ShortInt;
const
	Filter: array[Boolean, Boolean] of ShortInt = ( ( 1, 1 ), ( -1, -1 ) );
{
	B1.Line > B2.Line ?     B1.CharIndex > B2.CharIndex ?
			False                          False              »  1
			False                          True               »  1
			True                           False              » -1
			True                           True               » -1
}
begin
	if ( Block1.Line > Block2.Line ) then
		Result := -1
	else if ( Block2.Line > Block1.Line ) then
		Result := 1
	else
	begin
		if ( Block1.CharIndex > Block2.CharIndex ) then
			Result := -1
		else if ( Block2.CharIndex > Block1.CharIndex ) then
			Result := 1
		else
			Result := 0;
	end;
end;

procedure ReplaceSelection( Editor: TIEditorInterface;
	ViewNum: Integer; Text: string; MaintainBlockVisible: Boolean );
const
	BlockTypeName: array[TBlockType] of string[14] =
	(
		'btInclusive', 'btLine', 'btColumn', 'btNonInclusive', 'btUnknown'
	);
var
	Start,
	After: TCharPos;
	View: TIEditView;
	CursorPos: TEditPos;
	CharPos: TCharPos;

{ Replace the text between Start and After, inclusive, with Text. }
	procedure ReplaceInclusive;
	var
		Writer: TIEditWriter;
		StartPos, AfterPos: Integer;
	begin
		StartPos := View.CharPosToPos( Start );
		AfterPos := View.CharPosToPos( After );
		Writer := Editor.CreateUndoableWriter;
		if CheckObject( Writer ) then
			try
				Writer.CopyTo( StartPos );
				if ( AfterPos+1 > StartPos ) then
					Writer.DeleteTo( AfterPos + 1 );
				Writer.Insert( PChar( Text ) );
				Writer.CopyTo( High( LongInt ) );
			finally
				Writer.Free;
			end
		else
			RaiseException( EKOTA, sErrEdtIntfInvWriter );
	end;

begin
	ForceObject( Editor );
	Start := Editor.BlockStart;
	After := Editor.BlockAfter;
	View := Editor.GetView( ViewNum );
	if CheckObject( View ) then
		try
			case Editor.BlockType of
				{btColumn:}
				btInclusive:
					ReplaceInclusive;
				btNonInclusive:
					begin
						Dec( After.CharIndex );
						ReplaceInclusive;
					end;
				btLine:
					begin
						Start.CharIndex := 0;                { start of line }
						After.CharIndex := High( SmallInt ); { entire line }
						ReplaceInclusive;
					end;
			else
				RaiseExceptionFmt( EKOTA, sErrEdtIntfInvBlockType, [
					BlockTypeName[Editor.BlockType]] );
			end;

{ Set the cursor position to the start of the original
	block, which is the position where the replacement
	text starts. }
			if ( Editor.BlockType = btColumn ) then
			begin
				CursorPos := TEditPos( Start );
				View.ConvertPos( True, CursorPos, CharPos );
			end
			else
			begin
				CharPos := Start;
				View.ConvertPos( False, CursorPos, Start );
			end;
			View.CursorPos := CursorPos;

			if MaintainBlockVisible then
			begin
{ Select the newly inserted text. }
				Editor.BlockVisible := False;
				Editor.BlockType := btNonInclusive;
				Editor.BlockStart := CharPos;
				Inc( CharPos.CharIndex, Length( Text ) );
				Editor.BlockAfter := CharPos;
				Editor.BlockVisible := True;
			end;

{ Make sure the new cursor position is visible. }
			if ( CursorPos.Line < View.TopPos.Line ) or
				 ( CursorPos.Line >= View.TopPos.Line+View.ViewSize.CY ) then
			begin
				if ( CursorPos.Col < View.ViewSize.CX ) then
					CursorPos.Col := 1;
				View.TopPos := CursorPos;
			end;
		finally
			View.Free;
		end
	else
		RaiseExceptionFmt( EKOTA, sErrEdtIntfInvViewNum, [ViewNum] );
end;

procedure ExecuteCurrentEditorInterfaceByMethod( Proc: TKEditorExecuteMethod;
	RaiseExceptions: Boolean; Data: Pointer );
begin
	with ToolServices do
		ExecuteEditorInterfaceByMethod( GetCurrentFile, Proc, RaiseExceptions, Data );
end;

procedure ExecuteCurrentEditorInterfaceByProc( Proc: TKEditorExecuteProc;
	RaiseExceptions: Boolean; Data: Pointer );
begin
	with ToolServices do
		ExecuteEditorInterfaceByProc( GetCurrentFile, Proc, RaiseExceptions, Data );
end;

procedure ExecuteEditorInterfaceByMethod( const ModuleName: string; Proc: TKEditorExecuteMethod;
	RaiseExceptions: Boolean; Data: Pointer );
var
	Module: TIModuleInterface;
	Editor: TIEditorInterface;
begin
	ForceTrimStr( ModuleName );
	ForceReference( @Proc );
	with ToolServices do
		Module := GetModuleInterface( ModuleName );
	if CheckObject( Module ) then
		try
			Editor := Module.GetEditorInterface;
			if CheckObject( Editor ) then
				try
					Proc( Module, Editor, Data );
				finally
					Editor.Free;
				end
			else if RaiseExceptions then
				RaiseException( EKOTA, sErrEdtIntfFormSelected );
		finally
			Module.Free;
		end
	else if RaiseExceptions then
		RaiseExceptionFmt( EKOTA, sErrEdtIntfInvFileName, [ModuleName] );
end;

procedure ExecuteEditorInterfaceByProc( const ModuleName: string;
	Proc: TKEditorExecuteProc; RaiseExceptions: Boolean; Data: Pointer );
var
	Module: TIModuleInterface;
	Editor: TIEditorInterface;
begin
	ForceTrimStr( ModuleName );
	ForceReference( @Proc );
	with ToolServices do
		Module := GetModuleInterface( ModuleName );
	if CheckObject( Module ) then
		try
			Editor := Module.GetEditorInterface;
			if CheckObject( Editor ) then
				try
					Proc( Module, Editor, Data );
				finally
					Editor.Free;
				end
			else if RaiseExceptions then
				RaiseException( EKOTA, sErrEdtIntfFormSelected );
		finally
			Module.Free;
		end
	else if RaiseExceptions then
		RaiseExceptionFmt( EKOTA, sErrEdtIntfInvFileName, [ModuleName] );
end;

procedure ExecuteCurrentFormInterfaceByMethod( Proc: TKFormExecuteMethod;
	RaiseExceptions: Boolean; Data: Pointer );
begin
	with ToolServices do
		ExecuteFormInterfaceByMethod( GetCurrentFile, Proc, RaiseExceptions, Data );
end;

procedure ExecuteCurrentFormInterfaceByProc( Proc: TKFormExecuteProc;
	RaiseExceptions: Boolean; Data: Pointer );
begin
	with ToolServices do
		ExecuteFormInterfaceByProc( GetCurrentFile, Proc, RaiseExceptions, Data );
end;

procedure ExecuteFormInterfaceByMethod( const ModuleName: string; Proc: TKFormExecuteMethod;
	RaiseExceptions: Boolean; Data: Pointer );
var
	Module: TIModuleInterface;
	Form: TIFormInterface;
begin
	ForceTrimStr( ModuleName );
	ForceReference( @Proc );
	with ToolServices do
		Module := GetModuleInterface( ChangeFileExt( ModuleName, DELPHI_UNIT_EXT ) );
	if CheckObject( Module ) then
		try
			Form := Module.GetFormInterface;
			if CheckObject( Form ) then
				try
					Proc( Module, Form, Data );
				finally
					Form.Free;
				end
			else if RaiseExceptions then
				RaiseException( EKOTA, sErrEdtIntfEditorSelected );
		finally
			Module.Free;
		end
	else if RaiseExceptions then
		RaiseExceptionFmt( EKOTA, sErrEdtIntfInvFileName, [ModuleName] );
end;

procedure ExecuteFormInterfaceByProc( const ModuleName: string;
	Proc: TKFormExecuteProc; RaiseExceptions: Boolean; Data: Pointer );
var
	Module: TIModuleInterface;
	Form: TIFormInterface;
begin
	ForceTrimStr( ModuleName );
	ForceReference( @Proc );
	with ToolServices do
		Module := GetModuleInterface( ChangeFileExt( ModuleName, DELPHI_UNIT_EXT ) );
	if CheckObject( Module ) then
		try
			Form := Module.GetFormInterface;
			if CheckObject( Form ) then
				try
					Proc( Module, Form, Data );
				finally
					Form.Free;
				end
			else if RaiseExceptions then
				RaiseException( EKOTA, sErrEdtIntfEditorSelected );
		finally
			Module.Free;
		end
	else if RaiseExceptions then
		RaiseExceptionFmt( EKOTA, sErrEdtIntfInvFileName, [ModuleName] );
end;

procedure ChangeModuleSyntaxHighlighter( Module: TIModuleInterface;
	Editor: TIEditorInterface; Data: Pointer );
begin
	PSyntaxHighlighter( Data )^ := Editor.SetSyntaxHighlighter( PSyntaxHighlighter( Data )^ );
end;

function SetModuleSyntaxHighlighter( const ModuleName: string;
	SyntaxHighlighter: TSyntaxHighlighter ): TSyntaxHighlighter;
var
	psh: PSyntaxHighlighter;
begin
	ForceTrimStr( ModuleName );
	psh := New( PSyntaxHighlighter );
	try
		psh^ := SyntaxHighlighter;
		ExecuteEditorInterfaceByProc( ModuleName, ChangeModuleSyntaxHighlighter,
			False, psh );
		Result := psh^;
	finally
		Dispose( psh );
	end;
end;

procedure ChangeModuleBlockType( Module: TIModuleInterface;
	Editor: TIEditorInterface; Data: Pointer );
begin
	Editor.SetBlockType( PBlockType( Data )^ );
	PBlockType( Data )^ := Editor.GetBlockType;
end;

function SetModuleBlockType( const ModuleName: string;
	BlockType: TBlockType ): TBlockType;
var
	pbt: PBlockType;
begin
	ForceTrimStr( ModuleName );
	pbt := New( PBlockType );
	try
		pbt^ := BlockType;
		ExecuteEditorInterfaceByProc( ModuleName, ChangeModuleBlockType,
			False, pbt );
		Result := pbt^;
	finally
		Dispose( pbt );
	end;
end;

procedure InsertCode( Editor: TIEditorInterface; Position: Cardinal; const Text: string );
var
	Writer : TIEditWriter;
begin
	ForceTrim( [Editor, Text] );
	Writer := Editor.CreateUndoableWriter;
	if CheckObject( Writer ) then
		try
			if ( Position > 0 ) then
				Writer.CopyTo( Position );
			Writer.Insert( PChar( Text ) );
		finally
			Writer.Free;
		end
	else
		RaiseException( EKOTA, sErrEdtIntfInvWriter );
end;

procedure ClearEditor( Editor: TIEditorInterface );
var
  iSize: LongInt;
	Writer : TIEditWriter;
	Reader : TKIEditReaderStream;
begin
	ForceObject( Editor );
{ Cannot have a Edit Reader/Writer at the same time... }
	Reader := TKIEditReaderStream.Create( Editor );
	try
	  iSize := Reader.Size;
	finally
		Reader.Free;
	end;
	Writer := Editor.CreateWriter;
	if CheckObject( Writer ) then
		try
			if ( iSize > 0 ) then
				Writer.DeleteTo( iSize );
		finally
			Writer.Free;
		end
	else
		RaiseException( EKOTA, sErrEdtIntfInvWriter );
end;

procedure InternalSetBufPosProc( Module: TIModuleInterface; Editor: TIEditorInterface;
	Data: Pointer );
var
	edPos: TEditPos;
	evView: TIEditView;
begin
	Module.ShowSource;
	if ( Editor.GetViewCount > 1 ) then
		RaiseException( EKSYDUtils, sErrInvBufferViewCount );
	evView := Editor.GetView( 0 );
	try
		ForceObject( evView );
		edPos.Col := 1;
		edPos.Line := PInteger( Data )^;
		evView.CursorPos := edPos;
{ Put the line at center of the buffer }
		Dec( edPos.Line, ( evView.ViewSize.cy div 2 ) );
		edPos.Line := Max( 1, edPos.Line );
		evView.TopPos := edPos;
	finally
		evView.Free;
	end;
end;

procedure SetBufferPosition( const FileName: string; LineNumber: Integer );
begin
{
	If the file not exists or if exists and is a project file then raise
	If exists and not is a project file, if is a unit or a include or package file then
	If the file is not open and the file could not be opened then raise.
	Otherwise, we have a valid (existent non project) file opened in the IDE and we
	can process the buffer positioning.
}
	if ( ( not CheckFile( FileName ) ) or
			 CheckStrContains( DELPHI_PROJECT_EXT, FileName ) or
				 ( ( CheckStrContains( DELPHI_UNIT_EXT, FileName ) or
						 CheckStrContains( DELPHI_INCLUDE_EXT, FileName ) or
						 CheckStrContains( DELPHI_PACKAGESOURCE_EXT, FileName ) ) and
						 ( not ( ToolServices.IsFileOpen( FileName ) and
										 ToolServices.OpenFile( FileName ) ) ) ) ) then
			RaiseExceptionFmt( EKSYDUtils, sErrCouldNotOpenFileType, [FileName] );
{
	In some (very unusual) cases, the line number comes with negative or null values
	for unsucessful compilation (because of many reasons). The editor buffer line numbers
	are one-bases, so the less acceptable value is 1!
}
	LineNumber := Max( 1, LineNumber );
	ExecuteEditorInterfaceByProc( FileName, InternalSetBufPosProc, True, @LineNumber );
end;

function SaveModuleInterface( const FileName: string; ForceSave,
	TryToMarkFormModified: Boolean ): Boolean;
var
	mi: TIModuleInterface;
	fi: TIFormInterface;
begin
  ForceTrimStr( FileName );
	Result := False;
	mi := ToolServices.GetModuleInterface( FileName );
	if CheckObject( mi ) then
		try
			Result := true;
			if ( TryToMarkFormModified ) then
			begin
				fi := mi.GetFormInterface;
				if CheckObject( fi ) then
					try
						Result := fi.MarkModified;
					finally
						fi.Free;
					end;
			end;
			Result := ( Result and mi.Save( ForceSave ) );
			{if ( not Result ) and ( ForceSave ) then
				RaiseExceptionFmt( EKOTA, sErrModIntfSave, [FileName] );}
		finally
			mi.Free;
		end;
end;

procedure CheckModIntfBufferModified( Module: TIModuleInterface;
	Editor: TIEditorInterface; Data: Pointer );
begin
	PBoolean( Data )^ := Editor.BufferModified;
end;

function IsModIntfBufferModified( const FileName: string ): Boolean;
var
	pb: PBoolean;
begin
	pb := New( PBoolean );
	try
		pb^ := True;
		ExecuteEditorInterfaceByProc( FileName, CheckModIntfBufferModified, false,
			pb );
		Result := pb^;	
	finally
		Dispose( pb );
	end;
end;

function ModifyFormInterface( const FileName: string ): Boolean;
var
	mi: TIModuleInterface;
	fi: TIFormInterface;
	bOk,
	bFM: Boolean;
begin
	ForceTrimStr( FileName );
	bOk := false;
	bFM := false;
	Result := false;
	mi := ToolServices.GetModuleInterface( FileName );
	try
		Result := CheckObject( mi );
		if ( not Result ) then
			Exit;
		fi := mi.GetFormInterface;
		try
			bOk := CheckObject( fi );
			if ( not bOk ) then
				Exit;
			bFM := fi.MarkModified;
		finally
			if bOk then
				fi.Free;
		end;
	finally
		if Result then
		begin
			mi.Free;
			Result := ( bOk and bFM );
		end;
	end;
end;

procedure CopyResourceFromStream( Stream: TStream; ResEntry: TIResourceEntry );
var
	rs: TKIResEntryStream;
begin
	ForceObjects( [Stream, ResEntry] );
	rs := TKIResEntryStream.Create( ResEntry );
	try
		ResEntry.SetDataSize( Stream.Size );
		ForceStreamCopy( Stream, rs );
	finally
		rs.Free;
	end;
end;

procedure SetSubProperties( fi: TIFormInterface; ci: TIComponentInterface;
	Comp: TComponent );
var
	i,
	iCount: Integer;
	v: Variant;
	ppi: PPropInfo;
	PropValue: TKPropertyValue;
begin
	iCount := ci.GetPropCount;
	for i := 0 to iCount - 1 do
	begin
		ppi := GetPropInfo( Comp.ClassInfo, ci.GetPropName( i ) );
		if CheckPointer( ppi ) then
		begin
			ZeroMemory( @PropValue, SizeOf( TKPropertyValue ) );
			case ci.GetPropType( i ) of
				ptUnknown      : { ignore... };
				ptInteger, ptChar,
				ptEnumeration,
				ptSet, ptClass,
				ptWChar				 : PropValue.IntegerValue := GetOrdProp( Comp, ppi );
				ptFloat				 : PropValue.FloatValue   := GetFloatProp( Comp, ppi );
				ptString,
				ptLString,
				ptLWString		 : string( PropValue.PtrValue ) := GetStrProp( Comp, ppi );
				ptMethod			 : PropValue.MethodValue 			  := GetMethodProp( Comp, ppi );
				ptVariant      :
				begin
					v := GetVariantProp( Comp, ppi );
					Move( PVariant( @v )^, PByteArray( @PropValue.VariantValue[0] )^, SizeOf( Variant ) );
				end;
			end;
			ci.SetProp( i, PropValue );
		end;
	end;
end;

procedure InternalCreateComponentEx( fi: TIFormInterface; Owner: TIComponentInterface;
	Comp: TComponent );
var
  i: Integer;
	ci,
	co: TIComponentInterface;
begin
	ci := fi.CreateComponent( Owner, Comp.ClassName, LongRec( Comp.DesignInfo ).Lo,
		LongRec( Comp.DesignInfo ).Hi, -1, -1 );
	if CheckObject( ci ) then
		try
			SetSubProperties( fi, ci, Comp );
			for i := 0 to Comp.ComponentCount - 1 do
			begin
				co := fi.FindComponent( Comp.Name );
				try
				  ForceObject( co );
					InternalCreateComponentEx( fi, co, Comp.Components[i] );
				finally
					co.Free;
				end;
			end;
		finally
			ci.Free;
		end
	else
		RaiseExceptionFmt( EKSYDUtils, sErrInvCompIntf, [Comp.Name, Comp.ClassName] );
end;

procedure InternalCreateComponent( mi: TIModuleInterface; Comp: TComponent );
var
	fi: TIFormInterface;
	ci: TIComponentInterface;
begin
	fi := mi.GetFormInterface;
	if CheckObject( fi ) then
		try
			ci := nil;
			if CheckObject( Comp.Owner ) then
				ci := fi.FindComponent( Comp.Owner.Name );
			try
				InternalCreateComponentEx( fi, ci, Comp );
			finally
				ci.Free;
			end;
		finally
			fi.Free;
		end
	else
		RaiseException( EKSYDUtils, sErrInvFormIntf );
end;

procedure CreateComponent( const FormName: string; Comp: TComponent );
var
	mi: TIModuleInterface;
begin
	ForceTrim( [FormName, Comp] );
	mi := ToolServices.GetFormModuleInterface( FormName );
	if CheckObject( mi ) then
		try
			InternalCreateComponent( mi, Comp );
		finally
			mi.Free;
		end
	else
		RaiseExceptionFmt( EKSYDUtils, sErrInvFormName, [FormName] );
end;

procedure CreateComponentEx( const ModuleName: string; Comp: TComponent );
var
	mi: TIModuleInterface;
begin
	ForceTrim( [ModuleName, Comp] );
	mi := ToolServices.GetModuleInterface( ModuleName );
	if CheckObject( mi ) then
		try
			InternalCreateComponent( mi, Comp );
		finally
			mi.Free;
		end
	else
		RaiseExceptionFmt( EKSYDUtils, sErrInvModuleName, [ModuleName] );
end;

{
--------------------------------------------------------------------------------
-------------------------- Strings Editor Routines------------------------------
--------------------------------------------------------------------------------
}

function EditTStrings( Source: TPersistent; const PropName, VFSFileName: string;
	VFSClass: TIVirtualFileSystemClass; Designer: TKFormDesigner; ss: TStrings;
	sh: TSyntaxHighlighter; MultiSelect: Boolean ): Boolean;
var
	Form: TfrmStrEditDlg;
	ppi: PPropInfo;
	mi: TIModuleInterface;
begin
	ForceTrim( [Source, Designer, ss, PropName, VFSFileName] );
	ForceClassReference( VFSClass, TKIVirtualFileSystem );
	ppi := GetPropInfo( Source.ClassInfo, PropName );
	ForceVFSPropInfoKind( Source, ppi );
	Result := false;
{ if the module was already registered/opened, so show it and do not shows the editor }
	if ModuleRegisteredForVFS( TKIStringVFS, VFSFileName ) then
	begin
		mi := ToolServices.GetModuleInterface( VFSFileName );
		if CheckObject( mi ) then
			try
				mi.ShowSource;
				Exit;
			finally
				mi.Free;
			end;
	end;
	Form := TfrmStrEditDlg.Create( nil );
	try
		Form.Designer := Designer;
		Form.MultiSelect := MultiSelect;
		Form.VFSFileName := VFSFileName;
		Form.VFSClass := TKIVirtualFileSystemClass( VFSClass );
		Form.PropInfo := ppi;
		Form.Source := Source;
		Form.Memo.ScrollBars := ssVertical;
		Form.Memo.Lines.Assign( ss );
		Form.Memo.WordWrap := True;
		Form.UpdateStatus( nil );
		Form.Memo.Font.Name := 'Courier New';
		if CheckObject( StringsEditorFont ) then
  		Form.Memo.Font := StringsEditorFont;
		case Form.ShowModal of
			mrOk:
			begin
				Result := true;
				ss.Assign( Form.Memo.Lines );
			end;
			mrCodeEditor:
			begin
				if ( sh <> shNone ) then
					SetModuleSyntaxHighlighter( VFSFileName, sh );
				Form.ModuleIntf.ShowSource;
			end;
		end;
	finally
		Form.Free
	end;
end;

function EditString( Source: TPersistent; const PropName, VFSFileName: string;
	Designer: TKFormDesigner; var s: string; sh: TSyntaxHighlighter;
	MultiSelect: Boolean ): Boolean;
var
	sl: TStrings;
begin
	sl := TStringList.Create;
	try
		sl.Text := s;
		Result := EditTStrings( Source, PropName, VFSFileName, TKIStringVFS, Designer,
		  sl, sh, MultiSelect );
		if Result then
		begin
			if CheckStrings( sl ) then
			begin
				s := sl.Text;
				if ( Length( s ) > 1 ) then
					Delete( s, Length( s ) - 1, 2 ); { Cut the last CRLF! }
			end
			else
				s := '';
		end;
	finally
		sl.Free;
	end;
end;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

procedure TestDsgnShareWareVersion;
begin
	if ( AnyPackage_Shareware and CurrentDelphi32Running ) then
		RaiseExceptionFmt( EKSYDUtils, sErrShareWare, [FirstShareWare_PackageName] );
end;

procedure TestSystemDsgnShareWareVersion;
begin
	if ( IsSystem_Shareware and CurrentDelphi32Running ) then
		RaiseExceptionFmt( EKSYDUtils, sErrShareWare, [GetPackageName( pedSystem )] );
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
end;

procedure Done;
begin
end;

initialization
	Init;

finalization
	Done;

end.
