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

unit ukdcdMComments;

{$I s:\v100\include\iKLIB100.inc}

interface

resourcestring

{$IFNDEF EXCLUDED_CLASSES}

{------------------------------ TKDBGrid Comments ------------------------------}

{
	TKDBGrid.OnBlobCellDblClick: TBlobCellDblClick = procedure( Sender: TKDBGrid;
		BType: TKBlobType; BField: TField; ACol, ARow: Integer ) of object;
}
	sComBlobCellDblClick =
	'» Parameters: Sender = TKDBGrid; BType = the type of blob of the field that'#13#10 +
	'    received teh DblClick event; BField = the blob field that received the '#13#10 +
	'    DblClick event; ACol = the index of the column that received the event;'#13#10 +
	'    ARow = the index of the row that received the event.'#13#10 +
	'» Invoked after a grid cell associated with a TBlobField has been double-clicked.'#13#10 +
	'» Use this event to allow easier blob editing after a double-click has occurred.';

{
	TKDBGrid.OnCheckBoxClick: TKCheckBoxClick = procedure( Sender: TKDBGrid;
		Field: TField ) of object;
}
	sComCheckBoxClick =
	'» Parameters: Sender = TKDBGrid; Field = selected field in grid.'#13#10 +
	'» Invoked after a grid cell associated with a TBooleanField has been clicked.'#13#10 +
	'    It will occour only if CheckBoxes property are enabled.'#13#10 +
	'» Use this event to take any custom processing after a check box has been clicked.';

{$ENDIF}

{--------------------------- DBCtrls Common Comments ---------------------------}

{
	TKDBLabel3D/TKDBCheckBox/TKDBBitwiseCheckBox/TKDBComboBox/TKDBListBox/
	TKDBRadioGroup/TKDBMemo/TKDBRichEdit/TKDBImage/TKDBDateTimePicker/
	TKDBSpeedInteger/TKDBSpeedHexa/TKDBSpeedFloat/TKDBSpeedDateTime/
	TKDBSpeedTime/TKDBSpeedDate/TKDBSpeedText/TKDBSpeedFile/TKDBSpeedFolder/
	TKDBButton/TKDBBitBtn/TKDBSpeedButton/TKDBLookUpListBox/TKDBLookUpComboBox
}

{
	OnSetEnabled: TSetEnabledEvent = procedure( Sender: TControl; var IsEnabled:
		Boolean ) of object;
}

	sComOnSetEnabled =
	'» Parameters: Sender = TControl; var IsEnabled = control''s current enabled state;'#13#10 +
	'    set this property to the proper value according to the current dataset state.'#13#10 +
	'» Invoked after any dataset notification has taken place. The Sender''s enabled'#13#10 +
	'    property will be set to the appropriate value according to the EnabledStates'#13#10 +
	' 	  property.'#13#10 +
	'» Use this event to process dataset state synchronization.';

{----------------------------- TKDBLabel3D Comments ----------------------------}

{
	TKDBLabel3D.OnGetText: TKGetTextEvent = procedure ( Sender: TKCustomLabel3D;
		var AText: string ) of object;
}
	sComDBLbl3dGetTxt = '';

{---------------------------- TKDBRadioGroup Comments --------------------------}

{
	TKDBRadioGroup.OnClick: TNotifyEvent
}
	sComDBRdGrpClick = '';

{-------------------------- TKDBDateTimePicker Comments ------------------------}

{
	TKDBDateTimePicker.OnChange: TNotifyEvent
}
	sComDBDTPkChange = '';

implementation

end.
