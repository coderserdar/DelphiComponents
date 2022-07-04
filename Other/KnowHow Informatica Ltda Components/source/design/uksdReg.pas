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

unit uksdReg;

{$I s:\v100\include\iKLIB100.inc}

interface

procedure Register;

implementation

uses
	Classes, Controls, Forms, Menus, DsgnIntf, uksyClasses, uksydClasses,
	ukrMessages, ukrdClasses, uksUtils, uksCtrls, uksdConsts, uksdUtils,
	uksdMComments, uksdClasses;

procedure Register;
begin

{ Components }
	RegisterComponents( sRegStandard,
		[TKLabel3D, TKBevel, TKLight, TKComboBox, TKHistoryList, TKFocusLabel,
		 TKGroupBox, TKRadioGroup, TKPathLabel, TKGradientControl, TKBox,
		 TKGradientLabel, TKGradientText, TKGradientPanel, TKWebLabel, TKMemo,
		 TKRichEdit, TKRollButton, TKRollBitBtn, TKRollSpeedButton, TKSpeedButton,
		 TKSpeedText, TKSpeedFile, TKSpeedFolder, TKSpeedFloat, TKSpeedInteger,
		 TKSpeedHexa, TKSpeedDateTime, TKSpeedDate, TKSpeedTime, TKSponsorPanel] );

{ * - There are no bitmaps associated in kdpStd100.dcr }

{ Property Editors }

  RegisterPropertyEditor( TKGradient.ClassInfo, nil, '', TKGradientProperty );

{ Method Comments }

{----------------------------- TKWebLabel Comments -----------------------------}

	RegisterDefaultMethod( TypeInfo( TKWebActionEvent ), TKWebLabel, 'BeforeWebAction',
		sComBeforeWebAction );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKWebLabel, 'AfterWebAction',
		sComAfterWebAction );
	RegisterDefaultMethod( TypeInfo( TNotifyEvent ), TKWebLabel, 'OnCustomWebAction',
		sComCustomWebAction );

end;

end.
