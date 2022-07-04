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

unit ukdgdClasses;

interface

uses
	Classes, uksydUtils, uksydClasses, ukrdClasses;

type

	EKDGDClasses = class( EKDDialogs );

{
--------------------------------------------------------------------------------
------------------------- Component Editor Support -----------------------------
--------------------------------------------------------------------------------
}


{ TKDialogEditor }

	TKDialogEditor = class( TKDefaultEditor )
	protected
		procedure DialogExecutionFailed; virtual;
		procedure DialogCheckParamsFailed; virtual;
		
	public
		function GetVerbCount : Integer; override;
		procedure ExecuteVerb( Index : Integer ); override;
		function GetVerb( Index : Integer ): string; override;
		
	end;

{
--------------------------------------------------------------------------------
-------------------------- Property Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{--------------------- TKSpeedDateDialogProperty Editor ------------------------}

	TKSpeedDateDialogProperty = class( TKDateProperty  )
	protected
		function GetDateTimeFormat: string; override;

	end;

{--------------------- TKSpeedTimeDialogProperty Editor ------------------------}

	TKSpeedTimeDialogProperty = class( TKTimeProperty )
	protected
		function GetDateTimeFormat: string; override;

	end;

{--------------------- TKSpeedDateTimeDialogProperty Editor ------------------------}

	TKSpeedDateTimeDialogProperty = class( TKDateTimeProperty )
	protected
		function GetDateTimeFormat: string; override;

	end;

{------------------- TKDualListDialogBitsProperty Editor -----------------------}

	TKDualListDialogBitsProperty = class( TKBitsProperty )
	protected
		procedure GetItemList( sl: TStrings ); override;

	end;

implementation

uses
	uksyUtils, ukdgUtils, ukdgClasses, ukdgdConsts;

{
--------------------------------------------------------------------------------
------------------------- Component Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{ TKDialogEditor }

type

	TKBaseDialogHack = class( TKBaseDialog );

procedure TKDialogEditor.DialogExecutionFailed;
begin
  { do nothing... }
end;

procedure TKDialogEditor.DialogCheckParamsFailed;
begin
	RaiseException( EKDGDClasses, sErrDlgInvCheckParams )
end;

procedure TKDialogEditor.ExecuteVerb( Index : Integer );
begin
	if ( Index = DIALOGEDITOR_VERBCOUNT - 1 ) then
		if ( not TKBaseDialogHack( ( Component as TKBaseDialog ) ).DoCheckParams ) then
			DialogCheckParamsFailed
		else if ( Component as TKBaseDialog ).Execute then
			Designer.Modified
		else
			DialogExecutionFailed;
end;

function TKDialogEditor.GetVerb( Index : Integer ): string;
begin
	if ( Index = DIALOGEDITOR_VERBCOUNT - 1 ) then
		Result := sDlgEdtExecute
	else
		Result := '';
end;

function TKDialogEditor.GetVerbCount : Integer;
begin
	Result := DIALOGEDITOR_VERBCOUNT;
end;

{
--------------------------------------------------------------------------------
-------------------------- Property Editor Support -----------------------------
--------------------------------------------------------------------------------
}

{--------------------- TKSpeedDateDialogProperty Editor ------------------------}

function TKSpeedDateDialogProperty.GetDateTimeFormat: string;
begin
	Result := GetFirstString( [( GetComponent( 0 ) as TKDateSpeedProperties ).DisplayFormat,
		( inherited GetDateTimeFormat )] );
end;

{--------------------- TKSpeedTimeDialogProperty Editor ------------------------}

function TKSpeedTimeDialogProperty.GetDateTimeFormat: string;
begin
	Result := GetFirstString( [( GetComponent( 0 ) as TKTimeSpeedProperties ).DisplayFormat,
		( inherited GetDateTimeFormat )] );
end;

{--------------------- TKSpeedDateTimeDialogProperty Editor ------------------------}

function TKSpeedDateTimeDialogProperty.GetDateTimeFormat: string;
begin
	Result := GetFirstString( [( GetComponent( 0 ) as TKDateTimeSpeedProperties ).DisplayFormat,
		( inherited GetDateTimeFormat )] );
end;

{------------------- TKDualListDialogBitsProperty Editor -----------------------}

procedure TKDualListDialogBitsProperty.GetItemList( sl: TStrings );
var
	sl1: TStrings;
begin
	sl1 := nil;
	if CheckStrContains( 'Source', GetName ) then
		sl1 := ( GetComponent( 0 ) as TKDualListDialog ).SourceList
	else if CheckStrContains( 'Dest', GetName ) then
		sl1 := ( GetComponent( 0 ) as TKDualListDialog ).DestList;
	if ( not CheckStrings( sl1 ) ) then
		RaiseException( EKDGDClasses, sErrDLDInvList );
	sl.Assign( sl1 );
end;

end.
