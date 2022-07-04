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

unit ukexConsts;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{------------------------------- ukexClasses.pas -------------------------------}

	sCECopyRight = #13#10'Commented by KnowHow® Comment Expert... '#13#10;

{ Comment Expert }
	sErrInvEditorIntFViewCount = 'Invalid editor interface view count (''%d'') of module interface file name (''%s'').';
	sErrInvEditorBlock = 'Invalid editor block. You must select a non column block first.';
	sErrInvViewIntf = 'Invalid edit view interface for editor interface of module interface file name (''%s'').';
	sErrCannotComment = 'Cannot comment this block. Block already commented';

	sCommentName = 'KnowHow Comment Wizard';
	sCommentIDString = 'KnowHow.CommentExpert';
	sCommentMenuText = 'Co&mment Block';
	sCommentMenuName = 'EditInsertCommentItem';
	sCommentTargetMenuPoint = 'EditDeleteItem';

{ Adjust Comps Expert }

  sErrInvFormComp = 'Invalid form component';
  sAdjustCompName = 'KnowHow Adjust Component Wizard';
	sAdjustCompIDString = 'KnowHow.AdjustCompExpert';
	sAdjustCompMenuText = '&Adjust Components';
	sAdjustCompMenuName = 'ComponentInstallAdjustComponent';
	sAdjustCompTargetMenuPoint = 'ComponentInstallCompositeItem';

{ Collection Expert }
	sColTempWichTemplate = 'Do you want help comments in the collection template?';
	sCollectionTemplateTitle = 'Collection Template Wizard';
	sColTempSelExptName = 'Select Exception Name';
  sColTempSelExptCls = 'Select Exception Class ancestor name';
	sColTempSelItemName = 'Select CollectionItem Name';
	sColTempSelColName = 'Select Collection Name';
	sColTempSelCompName = 'Select Component Name';
  sColTempSelPropName = 'Select Collection Property Name';
	sColTempSelAncestorClassName = 'Select Component Ancestor Class';

	sColExpertTitle = 'Collection Expert Informations';
	sColExpertColName = 'Choose a collection name';
	sColExpertColItemName = 'Choose a collection item name';
	sColExpertColCompName = 'Choose a collection component name';
	sColExpertComment = 'Create a default collection architeture ready for use';
	sColExpertName = 'Collection Expert';

{-------------------------------- ukexUtils.pas --------------------------------}

	sErrInvToolServices = 'Cannot Check Adjustment allowed at run time.';
	sErrInvCapVisible =	'Cannot adjust the position of components with visible captions.';

{$IFDEF DELPHI4}
	DELPHI_REG_FORMDESIGN_SECTION   = 'Form Design';
	DELPHI_REG_COMPCAPTIONS_SECTION = 'Show Component Captions';
{$ELSE}
	{$IFDEF DELPHI3}
		DELPHI_REG_FORMDESIGN_SECTION   = 'FormDesign';
		DELPHI_REG_COMPCAPTIONS_SECTION = 'ShowComponentCaptions';
	{$ENDIF}
{$ENDIF}

{##NI##}

const

{$IFDEF KLIB100}
	EXPERTS_VER = '1.00';
	EXPERTS_VER_INT = 100;
	EXPERTS_VER_RELEASE_DATE = '25/07/1999 01:00:00';
{$ELSE}
	EXPERTS_VER = '?.??';
	EXPERTS_VER_INT = 0;
	EXPERTS_VER_RELEASE_DATE = '01/01/1900 00:00:00';
{$ENDIF}

{
--------------------------------------------------------------------------------
------------------------- Registration Information -----------------------------
--------------------------------------------------------------------------------
}

function GetExpertRegistryInfo: LongInt;

{##NI##}

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

function GetExpertRegistryInfo: LongInt;
begin
	Result := LongInt( @KnowHowRegistryInfo ) - SizeOf( TKRegistryInfo );
end;

end.
