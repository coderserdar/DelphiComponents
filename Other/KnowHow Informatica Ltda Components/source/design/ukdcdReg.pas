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

unit ukdcdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation
										 
uses
	Classes, Controls, uksyUtils, uksydClasses, ukrDBCtrls, ukdcUtils, ukdcCtrls,
	ukdcdConsts, ukdcdMComments;

procedure Register;
begin

{ Components/Classes }

	RegisterComponents( sRegDbCtrls,
		[ TKDBNavigator, TKDBEdit, TKDBText, TKDBLabel3D, TKDBComboBox, TKDBListBox,
			TKDBCheckBox, TKDBBitwiseCheckBox, TKDBRadioGroup, TKDBMemo, TKDBRichEdit,
			TKDBImage, TKDBDateTimePicker, TKDBButton, TKDBBitBtn, TKDBSpeedButton,
			TKDBLookupComboBox, TKDBLookupListBox, { TKDBGrid, // not yet completed }
			TKDBGradientLabel, TKDBGradientText, TKDBSpeedText, TKDBSpeedFile, TKDBSpeedFolder,
			TKDBSpeedFloat, TKDBSpeedInteger, TKDBSpeedHexa, TKDBSpeedDateTime, TKDBSpeedDate,
			TKDBSpeedTime] );

{ Method Comments }

{------------------------------ TKDBGrid Comments ------------------------------}

{$IFNDEF EXCLUDED_CLASSES}

	RegisterDefaultMethod( TypeInfo( TKBlobCellDblClick ), TKDBGrid, 'OnBlobCellDblClick',
		sComBlobCellDblClick );
	RegisterDefaultMethod( TypeInfo( TKCheckBoxClick ), TKDBGrid, 'OnCheckBoxClick',
		sComCheckBoxClick );

{$ENDIF}

{--------------------------- DBCtrls Common Comments ---------------------------}

{
	TKDBLabel3D/TKDBCheckBox/TKDBBitwiseCheckBox/TKDBComboBox/TKDBListBox/
	TKDBRadioGroup/TKDBMemo/TKDBRichEdit/TKDBImage/TKDBDateTimePicker/
	TKDBSpeedInteger/TKDBSpeedHexa/TKDBSpeedFloat/TKDBSpeedDateTime/
	TKDBSpeedTime/TKDBSpeedDate/TKDBSpeedText/TKDBSpeedFile/TKDBSpeedFolder/
	TKDBButton/TKDBBitBtn/TKDBSpeedButton/TKDBLookUpListBox/TKDBLookUpComboBox
}

	RegisterDefaultMethod( TypeInfo( TKSetEnabledEvent ), TControl, 'OnSetEnabled',
		sComOnSetEnabled );

{----------------------------- TKDBLabel3D Comments ----------------------------}

	RegisterDefaultMethod( TypeInfo( TKGetTextEvent ), TKDBLabel3D, 'OnGetText',
		sComDBLbl3dGetTxt );

{---------------------------- TKDBRadioGroup Comments --------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDBRadioGroup, 'OnClick',
		sComDBRdGrpClick );

{-------------------------- TKDBDateTimePicker Comments ------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKDBDateTimePicker, 'OnChange',
		sComDBRdGrpClick );

end;

end.
