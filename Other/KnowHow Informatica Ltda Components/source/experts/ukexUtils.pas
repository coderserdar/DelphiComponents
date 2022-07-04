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

unit ukexUtils;

{$I s:\v100\include\iKLIB100.inc}
{.$DEFINE DYGA}

interface

uses
	Windows, Classes, ToolIntf, uksyUtils, uksydUtils, uksydClasses, ukexClasses;

type

	EKDExUtils = class( EKDExperts );

{
--------------------------------------------------------------------------------
----------------------------- Generic Routines ---------------------------------
--------------------------------------------------------------------------------
}

function CreateAddInExpert( AddInClass: TKAddInExpertClass; Action: TKInsertAction;
  const TargetName, Caption, Name, Hint: string; ShortCut, Context, GroupIndex: Integer;
	Flags: TIMenuFlags ): TKIAddInExpert;

procedure RegisterSpecialExperts;
procedure UnRegisterSpecialExperts;

{
--------------------------------------------------------------------------------
---------------------- Component Adjustment Routines ---------------------------
--------------------------------------------------------------------------------
}

procedure AdjustComponents( AOwner: TComponent; Origin: TPoint;
	AdjustRatio: Byte; InValidClasses: array of TComponentClass );

procedure CheckCmpAdjustAllowed;

function GetComponentBounds( Component: TComponent ): TRect;
procedure SetComponentBounds( Component: TComponent; Rect: TRect );

{##NI##}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function IsExpert_Shareware: Boolean;
procedure TestExpertDsgnShareWareVersion;

{##NI##}

{
--------------------------------------------------------------------------------
--------------------- Validating Registration Information ----------------------
--------------------------------------------------------------------------------
}

function PackageUserName: string;
function PackageCompanyName: string;
function PackageVersion: TKLibVersion;

implementation

uses
	SysUtils, Messages, Consts, ExptIntf, Graphics, Forms, Controls, Dialogs,
	Registry, Menus, ComCtrls, uksyConsts, uksyShortCuts, uksyResStr, uksyTypes,
	uksyPackReg, ukexConsts, ukexPackReg;

{
--------------------------------------------------------------------------------
----------------------------- Generic Routines ---------------------------------
--------------------------------------------------------------------------------
}

function CreateAddInExpert( AddInClass: TKAddInExpertClass; Action: TKInsertAction;
  const TargetName, Caption, Name, Hint: string; ShortCut, Context, GroupIndex: Integer;
	Flags: TIMenuFlags ): TKIAddInExpert;
begin
	ForceTrim( [AddInClass, TargetName, Caption, Name] );
	Result := AddInClass.Create;
	try
		Result.InstallExpert( Action, TargetName, Caption, Name, Hint, ShortCut,
			Context, GroupIndex, Flags );
	except
		on E: EKOTA do
		begin
			Result.Free;
			ToolServices.RaiseException( E.Message );
		end;
	end;
end;

procedure RegisterSpecialExperts;
{$IFDEF DYGA}
type
  TMenuMapHack = record
    Value: TShortCut;
    Name: string;
    Caption: string;
  end;
{$ENDIF}
const
	hiMI_sc = {$IFDEF DELPHI3}17{$ELSE}7{$ENDIF};
{$IFDEF DYGA}
  SHORT_CUT_MAP: array[0..hiMI_sc - 1] of TMenuMapHack =
	(
{$IFDEF DELPHI3}
	 ( Value: SC_ALT_F11; 					 Name: 'FileUseUnitItem';     Caption: 'Use the fock unit' ),
	 ( Value: SC_CTRL_F11; 					 Name: 'FileOpenItem';        Caption: 'Open It Guy!' ),
	 ( Value: SC_ALT_G; 						 Name: 'SearchGoToItem';      Caption: 'Which Line, Please...' ),
	 ( Value: SC_ALT_CTRL_F11; 			 Name: 'ViewPrjMgrItem';      Caption: 'Project Boss' ),
	 ( Value: SC_SHIFT_F11; 				 Name: 'ProjectAddItem';      Caption: 'Add Problem' ),
	 ( Value: SC_ALT_CTRL_S;  			 Name: 'ViewCallStackItem';   Caption: 'Call Heap' ),
	 ( Value: SC_ALT_CTRL_W;				 Name: 'ViewWatchItem';       Caption: 'Humm...Voyer' ),
	 ( Value: SC_ALT_CTRL_B; 	  		 Name: 'ViewBreakPointsItem'; Caption: 'BREAKIT NOW!' ),
	 ( Value: SC_ALT_CTRL_T; 	  		 Name: 'ViewThreadsItem';     Caption: 'I love this one' ),
	 ( Value: SC_ALT_CTRL_M; 	  		 Name: 'ViewModulesItem';     Caption: 'Project Peaceses' ),
{$ENDIF}
	 ( Value: SC_CTRL_SHIFT_F11; 		 Name: 'ProjectOptionsItem';  Caption: 'Focking Options' ),
	 ( Value: SC_ALT_CTRL_SHIFT_F11; Name: 'ToolsOptionsItem';    Caption: 'Hamers' ),
	 ( Value: SC_CTRL_F10; 					 Name: 'ProjectBuildItem';    Caption: 'Build ;)' ),
	 ( Value: SC_ALT_CTRL_A; 				 Name: 'ViewAlignItem';       Caption: ':):):):)' ),     
	 ( Value: SC_ALT_CTRL_R; 				 Name: 'ViewBrowserItem';     Caption: 'Browse objects!' ),
	 ( Value: SC_F2; 								 Name: 'FileSaveAllItem';     Caption: 'Save all shit' ),
	 ( Value: SC_CTRL_P; 						 Name: 'InstallPackagesItem'; Caption: 'Install Bags' )
	);

{$ELSE}

	SHORT_CUT_MAP: array[0..hiMI_sc - 1] of TIdentMapEntry =
	(
{$IFDEF DELPHI3}
	 ( Value: SC_ALT_F11; 					 Name: 'FileUseUnitItem' ),
	 ( Value: SC_CTRL_F11; 					 Name: 'FileOpenItem' ),
	 ( Value: SC_ALT_G; 						 Name: 'SearchGoToItem' ),
	 ( Value: SC_ALT_CTRL_F11; 			 Name: 'ViewPrjMgrItem' ),
	 ( Value: SC_SHIFT_F11; 				 Name: 'ProjectAddItem' ),
	 ( Value: SC_ALT_CTRL_S;  			 Name: 'ViewCallStackItem' ),
	 ( Value: SC_ALT_CTRL_W;				 Name: 'ViewWatchItem' ),
	 ( Value: SC_ALT_CTRL_B; 	  		 Name: 'ViewBreakPointsItem' ),
	 ( Value: SC_ALT_CTRL_T; 	  		 Name: 'ViewThreadsItem' ),
	 ( Value: SC_ALT_CTRL_M; 	  		 Name: 'ViewModulesItem' ),
{$ENDIF}
	 ( Value: SC_CTRL_SHIFT_F11; 		 Name: 'ProjectOptionsItem' ),
	 ( Value: SC_ALT_CTRL_SHIFT_F11; Name: 'ToolsOptionsItem' ),
	 ( Value: SC_CTRL_F10; 					 Name: 'ProjectBuildItem' ),
	 ( Value: SC_ALT_CTRL_A; 				 Name: 'ViewAlignItem' ),
	 ( Value: SC_ALT_CTRL_R; 				 Name: 'ViewBrowserItem' ),
	 ( Value: SC_F2; 								 Name: 'FileSaveAllItem' ),
	 ( Value: SC_CTRL_P; 						 Name: 'InstallPackagesItem' )
	);

{$ENDIF}
var
	i: Integer;
	pi: TForm;
	rg: TRegistry;
	tc: TTabControl;
	c: TComponent;
begin
{ Can call this expert only into packages... }
	if IsLibrary then
		Exit;
	pi := nil;
	{ set shortcuts for exceptionally important tools: Build All and Alignment Palette }
	with Application.MainForm do
		for i := 0 to hiMI_sc - 1 do
		begin
			c := FindComponent( SHORT_CUT_MAP[i].Name );
			if CheckObjectClass( c, TMenuItem ) then
      begin
        ( c as TMenuItem ).ShortCut := TShortCut( SHORT_CUT_MAP[i].Value );
      {$IFDEF DYGA}
        ( c as TMenuItem ).Caption := SHORT_CUT_MAP[i].Caption;
      {$ENDIF}
      end;
		end;
	for i := 0 to Pred( Screen.FormCount ) do
		if ( Screen.Forms[i].ClassName = 'TPropertyInspector' ) then
		begin
			pi := Screen.Forms[i];
			Break;
		end;
	tc := ( Application.MainForm.FindComponent( 'TabControl' ) as TTabControl );
	rg := TRegistry.Create;
	try
		rg.OpenKey( 'Software\KnowHow\DelphiEx', true );
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

procedure UnRegisterSpecialExperts;
var
	i: Integer;
	mi: TMenuItem;
begin
	{ set shortcuts for exceptionally important tools: Build All and Alignment Palette }
	with Application.MainForm do
		for i := 0 to ComponentCount - 1 do
			if ( Components[i] is TMenuItem ) then
				if ( CompareText( Components[i].Name, 'ProjectOptionsItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ToolsOptionsItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ProjectBuildItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ViewAlignItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ViewBrowserItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'FileSaveAllItem' ) = 0 ) or
				{$IFDEF DELPHI3}
					 ( CompareText( Components[i].Name, 'FileUseUnitItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'FileOpenItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'SearchGoToItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ViewPrjMgrItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ProjectAddItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ViewCallStackItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ViewWatchItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ViewBreakPointsItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ViewThreadsItem' ) = 0 ) or
					 ( CompareText( Components[i].Name, 'ViewModulesItem' ) = 0 ) or
				{$ENDIF}
					 ( CompareText( Components[i].Name, 'InstallPackagesItem' ) = 0 ) then
				begin
					mi := TMenuItem( Components[i] );
					mi.ShortCut := SC_NULL;
				end;
end;

{
--------------------------------------------------------------------------------
---------------------- Component Adjustment Routines ---------------------------
--------------------------------------------------------------------------------
}

procedure CheckCmpAdjustAllowed;
var
	Reg: TRegIniFile;
begin
	if ( not CheckObject( ToolServices ) ) then
		RaiseException( EKOTA, sErrInvToolServices );
	Reg := TRegIniFile.Create( ToolServices.GetBaseRegistryKey );
	try
		if Reg.ReadBool( DELPHI_REG_FORMDESIGN_SECTION, DELPHI_REG_COMPCAPTIONS_SECTION, True ) then
			RaiseException( EKOTA, sErrInvCapVisible );
	finally
		Reg.Free;
	end;
end;

function GetComponentBounds( Component: TComponent ): TRect;
begin
	ZeroMemory( @Result, SizeOf( TRect ) );
	if CheckObjectClass( Component, TDataModule ) then
		with ( Component as TDataModule ) do
		begin
			Result.Left := LongRec( DesignInfo ).Lo;
			Result.Top := LongRec( DesignInfo ).Hi;
			Result.Right := Result.Left + DesignSize.y;
			Result.Bottom := Result.Top + DesignSize.x;
		end
	else if CheckObjectClass( Component, TCustomForm ) then
		Result := ( Component as TCustomForm ).ClientRect
	else
	begin
		Result.Left := LongRec( Component.DesignInfo ).Lo;
		Result.Top := LongRec( Component.DesignInfo ).Hi;
	end;
end;

procedure SetComponentBounds( Component: TComponent; Rect: TRect );
var
	iDsgn : Integer;
begin
	iDsgn := 0;
	LongRec( iDsgn ).Lo := Rect.Left;
	LongRec( iDsgn ).Hi := Rect.Top;
	Component.DesignInfo := iDsgn;
	if CheckObjectClass( Component, TDataModule ) then
		( Component as TDataModule ).DesignSize := Point(
			Word( Rect.Right - Rect.Left + 5 ), Word( Rect.Bottom - Rect.Top + 5 ) )
	else if CheckObjectClass( Component, TCustomForm ) then
    ( Component as TCustomForm ).BoundsRect := Rect;
end;

procedure AdjustComponents( AOwner: TComponent; Origin: TPoint; AdjustRatio: Byte;
	InValidClasses: array of TComponentClass );

	function CheckClass( AComp: TComponent ): Boolean;
	var
		i: Integer;
	begin
		Result := True;
		for i := Low( InvalidClasses ) to High( InvalidClasses ) do
			if CheckObjectClass( AComp, InvalidClasses[i] ) or
			   CheckObjectClass( AComp, TControl ) then
			begin
				Result := False;
				Break;
			end;
	end;

const
	MAX_ADJUST_RATIO = 75;
	MIN_ADJUST_RATIO = 15;
  DEFAULT_HEIGHT_WIDTH = 28;

var
	i,
	j: Integer;
	Ending: TPoint;
begin
	CheckCmpAdjustAllowed;
	if ( AdjustRatio > MAX_ADJUST_RATIO ) then
		AdjustRatio := MAX_ADJUST_RATIO
	else if ( AdjustRatio < MIN_ADJUST_RATIO ) then
		AdjustRatio := MIN_ADJUST_RATIO;
  ZeroMemory( @Ending, SizeOf( TPoint ) );
	if CheckObjectClass( AOwner, TCustomForm ) then
		with ( AOwner as TCustomForm ) do
		begin
			if ( ClientWidth < Origin.x ) then
				Origin.x := ClientWidth;
			if ( ClientHeight < Origin.y ) then
				Origin.y := ClientHeight;
			Ending.x := ClientWidth;
			Ending.y := ClientHeight;
		end
	else
	begin
		if ( AOwner.DesignInfo > MakeLong( Word( Origin.x ), Word( Origin.Y ) ) ) then
		begin
			Origin.X := LongRec( ( AOwner as TComponent ).DesignInfo ).Lo;
			Origin.Y := LongRec( ( AOwner as TComponent ).DesignInfo ).Hi;
		end;
		if CheckObjectClass( AOwner, TDataModule ) then
			Ending := ( AOwner as TDataModule ).DesignSize;
	end;
	j := 0;
	Ending.x := MulDiv( Ending.x, AdjustRatio, 100 );
	for i := 0 to AOwner.ComponentCount - 1 do
		if CheckClass( AOwner.Components[i] ) then
		begin
			if ( ( Origin.x + ( j * DEFAULT_HEIGHT_WIDTH ) ) > Ending.x ) then
			begin
				Origin.y := Origin.y + DEFAULT_HEIGHT_WIDTH;
				j := 0;
			end;
			AOwner.Components[i].DesignInfo := MakeLong( Word( Origin.x + ( j * DEFAULT_HEIGHT_WIDTH ) + 1 ),
				Word( Origin.y + 1 ) );
			Inc( j );
		end;
end;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type

	TSignature	 = TUserName;
	TKey				 = TUserName;

{$IFNDEF INTERNAL_VERSION}

	PKInstallInfo = ^TKInstallInfo;
	TKInstallInfo = record
		Signature: TSignature;
		Key: TKey;
	end;

const

(* KLIB100_REGISTRY_SIGNATURE = '{09536FA0-BF69-11D2-B212-00C0DFE081C4}' *)

	KnowHowInstallInfo: TKInstallInfo =
	(
{$IFDEF KLIB100}
		Signature: '{09536FA0-BF69-11D2-B212-00C0DFE081C4}'; { do not resource/const }
{$ELSE}
		Signature: *MUST GENERATE AN ERROR!*;
{$ENDIF}
		Key:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
	);

{$ENDIF}

function IsExpert_Shareware: Boolean;
begin
{$IFDEF INTERNAL_VERSION}
	Result := false;
{$ELSE}
	Result := ( not CheckRegistryInfo( GetExpertRegistryInfo,
		LongInt( @KnowHowInstallInfo ) - SizeOf( TKInstallInfo ) ) );
{$ENDIF}
end;

procedure TestExpertDsgnShareWareVersion;
begin
	if ( IsExpert_Shareware and CurrentDelphi32Running ) then
		RaiseExceptionFmt( EKDExUtils, sErrShareWare, [GetPackageName( perExperts )] );
end;

procedure RegisterExpertsUnits;
begin
	RegisterRunningPackage( perExperts, $B71B8F53 ); { do not const... }
	RegisterRunningPackage( pedExperts, $F57821B0 );
end;

procedure UnregisterExpertsUnits;
begin
	UnregisterRunningPackage( perExperts, $B71B8F53 ); { do not const... }
	UnregisterRunningPackage( pedExperts, $F57821B0 );
end;                                    

{
--------------------------------------------------------------------------------
--------------------- Validating Registration Information ----------------------
--------------------------------------------------------------------------------
}

type

	PKRegistryInfo = ^TKRegistryInfo;
	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

function PackageUserName: string;
begin
	Result := Trim( PKRegistryInfo( GetExpertRegistryInfo + SizeOf( TKRegistryInfo ) ).UserName );
end;

function PackageCompanyName: string;
begin
	Result := Trim( PKRegistryInfo( GetExpertRegistryInfo + SizeOf( TKRegistryInfo ) ).Company );
end;

function PackageVersion: TKLibVersion;
begin
	ZeroMemory( @Result, SizeOf( TKLIBVersion ) );
	Result.Release := StrToDateTime( EXPERTS_VER_RELEASE_DATE );
	Result.Version := EXPERTS_VER;
	Result.Reserved := EXPERTS_VER_INT;
end;

{
--------------------------------------------------------------------------------
------------------- Initialization and Finalization Routines -------------------
--------------------------------------------------------------------------------
}

procedure Init;
begin
	RegisterExpertsUnits;
	TestExpertShareWareVersion;
	CreateRegCheckerThread( pedExperts );
end;

procedure Done;
begin
  UnregisterExpertsUnits;
	if ( not IsLibrary ) then
  	UnRegisterSpecialExperts;
end;

initialization
	Init;

finalization
	Done;

end.

