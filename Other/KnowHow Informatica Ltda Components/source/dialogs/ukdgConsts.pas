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

unit ukdgConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

uses
  Windows, Graphics, uksyClasses;

const

{
--------------------------------------------------------------------------------
----------------------------- Generic Constants --------------------------------
--------------------------------------------------------------------------------
}

	DIALOGBMP_LOGON    = 'DLG_LOGON';
	DIALOGBMP_PASSWORD = 'DLG_PASSWORD';
	DIALOGBMP_PRINT    = 'DLG_PRINT';
	DIALOGBMP_BACKUP	 = 'DLG_BACKUP';
	DIALOGBMP_CRYPTO	 = 'DLG_CRYPTO';

	DIALOG_DEFAULT_SEC_EXPIRATION = 15;

	DIALOG_DEFAULT_ACTIVE_FONT_COLOR = clCaptionText;
	DIALOG_DEFAULT_INACTIVE_FONT_COLOR = clYellow;
  
	DEFAULT_DIALOG_GRADIENT_STEPS = 64;
	DEFAULT_DIALOG_GRADIENT_STYLE = gsNone;
	DEFAULT_DIALOG_GRADIENT_CAPTION_STYLE = gsDoubleVertical;

	DEFAULT_DIALOG_FONT_NAME = 'Arial';
	DEFAULT_DIALOG_FONT_SIZE = 8;


	DEFAULT_DIALOG_FORM_SCALE: TPoint = ( X: 800; Y: 600 );

{$IFDEF KLIB100}
	DIALOGS_VER = '1.00';
	DIALOGS_VER_INT = 100;
	DIALOGS_VER_RELEASE_DATE = '25/07/1999 01:00:00';
{$ELSE}
	DIALOGS_VER = '?.??';
	DIALOGS_VER_INT = 0;
	DIALOGS_VER_RELEASE_DATE = '01/01/1900 00:00:00';
{$ENDIF}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function GetDialogsRegistryInfo: LongInt;

implementation

uses
	SysUtils, uksyTypes, uksyConsts;

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

{--------------------------- Internal Implementation ---------------------------}

type
	TSignature	 = TUserName;
	TKey				 = TUserName;

	TKRegistryInfo = record
		Signature: TSignature;
		Key: TKey;
		UserName: TUserName;
		Company: TCompanyName;
	end;

const

	KnowHowRegistryInfo: TKRegistryInfo =
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
		UserName:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32;
		Company:
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32#32 +
			#32#32#32#32#32#32#32#32#32#32;
	);

{---------------------------- Public Implementation ----------------------------}

function GetDialogsRegistryInfo: LongInt;
begin
	Result := LongInt( @KnowHowRegistryInfo ) - SizeOf( TKRegistryInfo );
end;

end.
