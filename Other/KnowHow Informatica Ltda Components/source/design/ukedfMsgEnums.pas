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

unit ukedfMsgEnums;

{$I s:\v100\include\iKLIB100.inc}

interface

{##NI##}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, Buttons, ExtCtrls, ukrMessages;

type

	TfrmMsgEnums = class(TForm)
		GroupBox: TGroupBox;
		Panel: TPanel;
		BtOk: TBitBtn;
    BtCancel: TBitBtn;
    BtnAll: TBitBtn;
    BtNone: TBitBtn;
    cbWM: TCheckBox;
		cbBM: TCheckBox;
		cbLBN: TCheckBox;
    cbLBM: TCheckBox;
    cbEN: TCheckBox;
    cbEM: TCheckBox;
    cbSBM: TCheckBox;
    cbDM: TCheckBox;
    cbCM: TCheckBox;
    cbCN: TCheckBox;
    cbCBN: TCheckBox;
    cbCBM: TCheckBox;
    cbSBC: TCheckBox;
    cbSM2: TCheckBox;
		procedure BtAllNoneClick(Sender: TObject);

	private
		FLoSetValue: TIdentMsgMapEntryEnum;
		FHiSetValue: TIdentMsgMapEntryEnum;

		function GetCheckBoxes(Index: Integer): TCheckBox;
		function GrpCount: Integer;
		function GetMsgEnums: TIdentMsgMapEntryEnums;
		procedure SetMsgEnums( const Value: TIdentMsgMapEntryEnums );
		procedure SetHints;
		
	public
		property CheckBoxes [Index: Integer]: TCheckBox
						 read GetCheckBoxes;
		property MsgEnums: TIdentMsgMapEntryEnums
						 read GetMsgEnums write SetMsgEnums;
		property LoSetValue: TIdentMsgMapEntryEnum
						 read FLoSetValue;
		property HiSetValue: TIdentMsgMapEntryEnum
						 read FHiSetValue;

	end;

{##NI##}

function MsgEnumsEdit( var SetValue: TIdentMsgMapEntryEnums; const AddCaption: string;
	LoValue, HiValue: TIdentMsgMapEntryEnum ): Boolean;

implementation

uses
	uksyUtils, ukeUtils, ukedConsts;

{$R *.DFM}

const
	BTN_BASETAG = 100;

function MsgEnumsEdit( var SetValue: TIdentMsgMapEntryEnums; const AddCaption: string;
	LoValue, HiValue: TIdentMsgMapEntryEnum ): Boolean;
var
	frmMsgEnums: TfrmMsgEnums;
begin
	frmMsgEnums := TfrmMsgEnums.Create( nil );
	try
		with frmMsgEnums do
		begin
			fLoSetValue := LoValue;
			fHiSetValue := HiValue;
      SetHints;
			MsgEnums := SetValue;
			if CheckStr( AddCaption ) then
				Caption := Format( Caption, [AddCaption] )
			else
				Caption := Format( Caption, ['SetValue'] );
			Result := ( ShowModal = mrOk );
			if Result then
				SetValue := MsgEnums;
		end;
	finally
		frmMsgEnums.Free;
	end;
end;

function TfrmMsgEnums.GrpCount: Integer;
begin
	Result := GroupBox.ControlCount;
end;

function TfrmMsgEnums.GetCheckBoxes(Index: Integer): TCheckBox;
begin
	Result := ( GroupBox.Controls[Index] as TCheckBox );
end;

procedure TfrmMsgEnums.SetHints;
var
  i: TIdentMsgMapEntryEnum;
begin
	for i := LoSetValue to HiSetValue do
		CheckBoxes[Ord( i )].Hint := imeDescription[i];
end;

procedure TfrmMsgEnums.SetMsgEnums( const Value: TIdentMsgMapEntryEnums );
var
	i: TIdentMsgMapEntryEnum;
begin
	for I:= LoSetValue to HiSetValue do
		CheckBoxes[Ord( i )].Checked := ( i in Value );
end;

function TfrmMsgEnums.GetMsgEnums: TIdentMsgMapEntryEnums;
var
  i: Integer;
begin
	Result := [];
	for i:= 0 to GrpCount - 1 do
		if CheckBoxes[I].Checked then
			Include( Result, TIdentMsgMapEntryEnum( CheckBoxes[I].Tag ) )
		else
			Exclude( Result, TIdentMsgMapEntryEnum( CheckBoxes[I].Tag ) );
end;

procedure TfrmMsgEnums.BtAllNoneClick(Sender: TObject);
var
	i: Integer;
begin
	for i:= 0 to GrpCount - 1 do
		if ( CheckBoxes[i].Enabled and CheckBoxes[i].Visible ) then
			CheckBoxes[I].Checked := Boolean( ( Sender as TBitBtn ).Tag - BTN_BASETAG );
end;

end.
