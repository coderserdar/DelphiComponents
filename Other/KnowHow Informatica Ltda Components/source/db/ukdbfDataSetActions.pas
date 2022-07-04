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

unit ukdbfDataSetActions;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Buttons, ExtCtrls, ukdbEngines;

type
	TfrmKDataSetActions = class( TForm )
		GroupBox: TGroupBox;
		Panel: TPanel;
		BtOk: TBitBtn;
    BtCancel: TBitBtn;
    BtnAll: TBitBtn;
    BtNone: TBitBtn;
    cbOpen: TCheckBox;
    cbClose: TCheckBox;
		cbCancel: TCheckBox;
    cbPost: TCheckBox;
		cbEdit: TCheckBox;
    cbDelete: TCheckBox;
    cbCalcFields: TCheckBox;
    cbFilterRecord: TCheckBox;
    cbDeleteError: TCheckBox;
    cbEditError: TCheckBox;
    cbPostError: TCheckBox;
    cbScroll: TCheckBox;
    cbInsert: TCheckBox;
    cbNewRecord: TCheckBox;
    cbUpdateRecord: TCheckBox;
    cbUpdateError: TCheckBox;
		cbServerYield: TCheckBox;
		cbCommitCache: TCheckBox;
    cbExecSQL: TCheckBox;

		procedure BtAllNoneClick( Sender: TObject );

	private
		FLoSetValue: TKDataSetAction;
		FHiSetValue: TKDataSetAction;

		function GrpCount: Integer;
		function GetKDAtaSetActions: TKDataSetActions;
		function GetCheckBoxes( Index: Integer ): TCheckBox;
		procedure SetKDataSetActions( const Value: TKDataSetActions );

	public
		property CheckBoxes [Index: Integer]: TCheckBox
						 read GetCheckBoxes;
		property KDataSetActions: TKDataSetActions
						 read GetKDataSetActions write SetKDataSetActions;
		property LoSetValue: TKDataSetAction
						 read FLoSetValue;
		property HiSetValue: TKDataSetAction
						 read FHiSetValue;

	end;

{##NI##}

function KDataSetActionsEdit( var SetValue: TKDataSetActions; const AddCaption: string;
	LoValue, HiValue: TKDataSetAction ): Boolean;

implementation

uses
	uksyUtils, ukdbUtils, ukdbConsts;

{$R *.DFM}

const
	BTN_BASETAG = 100;

{---------------------------- Public Editor Function ---------------------------}

function KDataSetActionsEdit( var SetValue: TKDataSetActions; const AddCaption: string;
	LoValue, HiValue: TKDataSetAction ): Boolean;
var
	frmKDataSetActions: TfrmKDataSetActions;
begin
	frmKDataSetActions := TfrmKDataSetActions.Create( nil );
	try
		with frmKDataSetActions do
		begin
			fLoSetValue := LoValue;
			fHiSetValue := HiValue;
			KDataSetActions := SetValue;
			cbExecSQL.Visible := ( HiValue = dsaExecSQL );
			cbCommitCache.Visible := cbExecSQL.Visible;
			if CheckStr( AddCaption ) then
				Caption := Format( Caption, [AddCaption] )
			else
				Caption := Format( Caption, ['SetValue'] );
			Result := ( ShowModal = mrOk );
			if Result then
				SetValue := KDataSetActions;
		end;
	finally
		frmKDataSetActions.Free;
	end;
end;

{-------------------------- Form Editor Implementation -------------------------}

function TfrmKDataSetActions.GrpCount: Integer;
begin
	Result := GroupBox.ControlCount;
end;

function TfrmKDataSetActions.GetCheckBoxes( Index: Integer ): TCheckBox;
begin
	Result := ( GroupBox.Controls[Index] as TCheckBox );
end;

procedure TfrmKDataSetActions.SetKDataSetActions( const Value: TKDataSetActions );
var
	i: TKDataSetAction;
begin
	for i := LoSetValue to HiSetValue do
		CheckBoxes[Ord( i - 1 )].Checked := ( i in Value );
end;

function TFrmKDataSetActions.GetKDAtaSetActions: TKDataSetActions;
var
	i: Integer;
begin
	Result := [];
	for i := 0 to GrpCount - 1 do
		if CheckBoxes[i].Checked then
			Include( Result, TKDataSetAction( CheckBoxes[i].Tag ) )
		else
			Exclude( Result, TKDataSetAction( CheckBoxes[i].Tag ) );
end;

procedure TfrmKDataSetActions.BtAllNoneClick( Sender: TObject );
var
	i: Integer;
begin
	for i := 0 to GrpCount - 1 do
		if ( CheckBoxes[i].Enabled and CheckBoxes[i].Visible ) then
			CheckBoxes[i].Checked := Boolean( ( Sender as TBitBtn ).Tag - BTN_BASETAG );
end;

end.
