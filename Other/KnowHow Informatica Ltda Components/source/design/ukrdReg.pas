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

unit ukrdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	DsgnIntf, Classes, Menus, DB, DBCtrls, ComCtrls, Forms, uksydClasses,
	ukrConsts, ukrResStr, ukrClasses, ukrUtils, ukrLanguage, ukrDBCtrls,
	ukrCtrls, ukrdConsts, ukrdMComments, ukrdClasses, ukrdUtils;

procedure Register;
begin

{ property Editors }
	RegisterPropertyEditor( TypeInfo( string ), TKNavCollectionItem, 'FormName',
		TKDBNavEventFormNameProperty );

	RegisterPropertyEditor( TKStringsArray.ClassInfo, nil, sDefaultStringsArrayPropertyName,
		TKStringsArrayProperty );
	RegisterPropertyEditor( TypeInfo ( TLanguage ), nil, 'Language', TKLanguageProperty );
	RegisterPropertyEditor( TKPersistentCharSet.ClassInfo, nil, '', TKPersistentCharSetProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedFile, 'Value', TKFileNameProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedFolder, 'Value', TKDirectoryProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedText, 'InitialDir', TKDirectoryProperty );

	RegisterPropertyEditor( TypeInfo( LongInt ), TKFormattedSpeedControl, 'Increment', TKSpeedFloatProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKFormattedSpeedControl, 'MaxValue', TKSpeedFloatProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKFormattedSpeedControl, 'MinValue', TKSpeedFloatProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKFormattedSpeedControl, 'Value', TKSpeedFloatProperty );

	RegisterPropertyEditor( TypeInfo( LongInt ), TKCustomSpeedInteger, 'Increment', TKSpeedIntegerProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKCustomSpeedInteger, 'MaxValue', TKSpeedIntegerProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKCustomSpeedInteger, 'MinValue', TKSpeedIntegerProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKCustomSpeedInteger, 'Value', TKSpeedIntegerProperty );

	RegisterPropertyEditor( TypeInfo( LongInt ), TKCustomSpeedHexa, 'Increment', TKSpeedHexaProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKCustomSpeedHexa, 'MaxValue', TKSpeedHexaProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKCustomSpeedHexa, 'MinValue', TKSpeedHexaProperty );
	RegisterPropertyEditor( TypeInfo( LongInt ), TKCustomSpeedHexa, 'Value', TKSpeedHexaProperty );

	RegisterPropertyEditor( TypeInfo( TDateTime ), TKCustomSpeedDate, '', TKSpeedDateProperty );
	RegisterPropertyEditor( TypeInfo( TDateTime ), TKCustomSpeedTime, '', TKSpeedTimeProperty );
	RegisterPropertyEditor( TypeInfo( TDateTime ), TKCustomSpeedDateTime, '', TKSpeedDateTimeProperty );
	RegisterPropertyEditor( TypeInfo( TDateTime ), TKCustomSpeedDateTime, 'TimeIncrement', TKSpeedTimeProperty );
	
	RegisterPropertyEditor( TypeInfo( string ), TKFormattedSpeedControl, 'EditFormat', TKFloatFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKFormattedSpeedControl, 'DisplayFormat', TKFloatFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedInteger, 'EditFormat', TKIntegerFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedInteger, 'DisplayFormat', TKIntegerFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedDate, 'EditFormat', TKDateFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedDate, 'DisplayFormat', TKDateFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedTime, 'EditFormat', TKTimeFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedTime, 'DisplayFormat', TKTimeFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedDateTime, 'EditFormat', TKDateTimeFormatProperty );
	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedDateTime, 'DisplayFormat', TKDateTimeFormatProperty );

	RegisterPropertyEditor( TypeInfo( string ), TKCustomSpeedControl, 'ButtonHint', TKLongStringProperty );

{ Method Comments }

{------------------------------ Navigator Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKAfterActionEvent ), TKCustomDBNavigator, 'AfterAction',
		sComDBNavAfterAction );
	RegisterDefaultMethod( TypeInfo( TKBeforeActionEvent ), TKCustomDBNavigator, 'BeforeAction',
		sComDBNavBeforeAction );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomDBNavigator, 'OnPrint',
		sComDBNavOnPrint );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomDBNavigator, 'OnSearch',
		sComDBNavOnSearch );
	RegisterDefaultMethod( TypeInfo( TKFormNavEvent ), TKNavCollectionItem, 'OnPrint',
		sComDBNavItemOnPrint );
	RegisterDefaultMethod( TypeInfo( TKFormNavEvent ), TKNavCollectionItem, 'OnSearch',
		sComDBNavItemOnSearch );

{--------------------------- TKCaptionButton Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TKDrawCaptionButtonEvent ), TKCaptionButton, 'OnCustomButtonDraw',
		sComCapBtnCustomBtnDraw );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCaptionButton, 'OnClick',
		sComCapBtnClick );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCaptionButton, 'OnChange',
		sComCapBtnChange );

{---------------------------- TKFormPainter Comments ---------------------------}

	RegisterDefaultMethod( TypeInfo( TCloseQueryEvent ), TKFormPainter, 'OnCloseQuery',
		sComFrmPCloseQuery );

{------------------------- TKCustomSpeedButton Comments ------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomSpeedButton, 'OnDownClick',
		sComSpBtnDownClick );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomSpeedButton, 'OnUpClick',
		sComSpBtnUpClick );
	RegisterDefaultMethod( TypeInfo( TKDrawButtonEvent ), TKCustomSpeedButton, 'OnDrawButton',
		sComSpBtnDrawBtn );

{------------------------ TKCustomSpeedControl Comments ------------------------}

	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomSpeedControl, 'OnButtonClick',
		sComSpCtrlBtnClick );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomSpeedControl, 'OnDownClick',
		sComSpCtrlDownClick );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKCustomSpeedControl, 'OnUpClick',
		sComSpCtrlUpClick );
	RegisterDefaultMethod( TypeInfo( TKDrawButtonEvent ), TKCustomSpeedControl, 'OnDrawButton',
		sComSpCtrlDrawBtn );

{-------------------------- TKCustomSpeedText Comments -------------------------}

	RegisterDefaultMethod( TypeInfo( TKCheckTextEvent ), TKCustomSpeedText, 'OnCheckValue',
		sComSpTxtCheckValue );

{----------------------- TKFormattedSpeedControl Comments ----------------------}

	RegisterDefaultMethod( TypeInfo( TKFormatFloatTextEvent ), TKFormattedSpeedControl,
		'OnFormatDisplayText', sComSpFltFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatFloatTextEvent ), TKFormattedSpeedControl,
		'OnFormatEditText', sComSpFltFmtEdt );

{------------------------- TKCustomSpeedInteger Comments -----------------------}

	RegisterDefaultMethod( TypeInfo( TKFormatIntegerTextEvent ), TKCustomSpeedInteger,
		'OnFormatDisplayText', sComSpIntFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatIntegerTextEvent ), TKCustomSpeedInteger,
		'OnFormatEditText', sComSpIntFmtEdt );

{----- TKCustomSpeedDateTime/TKCustomSpeedDate/TKCustomSpeedTime Comments -----}

	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKCustomSpeedDate,
		'OnFormatDisplayText', sComSpDTFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKCustomSpeedDate,
		'OnFormatEditText', sComSpDTFmtEdt );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKCustomSpeedTime,
		'OnFormatDisplayText', sComSpDTFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKCustomSpeedTime,
		'OnFormatEditText', sComSpDTFmtEdt );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKCustomSpeedDateTime,
		'OnFormatDisplayText', sComSpDTFmtDisp );
	RegisterDefaultMethod( TypeInfo( TKFormatDateTimeTextEvent ), TKCustomSpeedDateTime,
		'OnFormatEditText', sComSpDTFmtEdt );

	if IsKernel_ShareWare then
	begin
		RegisterMethodShareWare( [
			TypeInfo( TDataSetNotifyEvent ), TypeInfo( TDataChangeEvent ), { from DB       }
			TypeInfo( TDataSetErrorEvent ), TypeInfo( TFieldNotifyEvent ),
			TypeInfo( ENavClick )] );                                      { from DBCtrls  }
	end;
	
end;

end.
