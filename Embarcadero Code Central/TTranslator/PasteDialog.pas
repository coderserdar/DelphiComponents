{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: PasteDialog.pas,v 1.18 2003/01/13 08:43:46 laa Exp $ }

unit PasteDialog;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls, Checklst,
{$else LINUX}
  QGraphics, QControls, QForms, QDialogs, QStdCtrls, QButtons, QExtCtrls, QCheckLst,
{$endif LINUX}
  RowList, SysUtils, Classes, DataEditorLib, DataType;

type
  TDialogType = (dtSelectFieldLists, dtSelectTables);

  TfrmPasteDialog = class(TForm)
    PanelBase: TPanel;
    GroupBoxFields: TGroupBox;
    PanelBottom: TPanel;
    PanelFields: TPanel;
    CheckListBoxFields: TCheckListBox;
    PanelSpace: TPanel;
    RadioGroupRules: TRadioGroup;
    PanelBottomRight: TPanel;
    BitBtnCancel: TBitBtn;
    BitBtnOK: TBitBtn;
    PanelLabel: TPanel;
    PanelLabelRight: TPanel;
    LabelRowsAffected: TLabel;
    procedure RadioGroupRulesClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FDialogType : TDialogType;
    FOtherNonKeys : Boolean;
    FTotalHeight : Integer;
    FHeightFields : Integer;
    FHeightRules : Integer;
    FTotalWidth : Integer;

    procedure AddFieldLists(AvailableLists: TStringList;
      OtherNonKeys: Boolean);
    procedure SetRowCount(ACount: Integer);
    procedure AddPasteRules(PasteRules: TPasteRulesSet);
    procedure MoveFieldListsToResult(Results: TStringList;
      var OtherNonKeys: Boolean);
    procedure MoveRulesToResult(var PasteRules: TPasteRulesSet);
  public
    function Run( AvailableLists, ResultLists : TStringList; var OtherNonKeys : Boolean;
                  var PasteRules : TPasteRulesSet; RowCount : Integer ) : Boolean;
{    function Run( AvailableLists, ResultLists : TStringList; var OtherNonKeys : Boolean;
                  PastingRows, PastingSingle, ForecastEmpty : Boolean;
                  var PasteRule : TPasteRules; RowCount : Integer ) : Boolean;}
    function RunTableSelect(EntryList : TValueList; RowLists : TList; var PasteRules : TPasteRulesSet) : Boolean;
  end;

var
  frmPasteDialog: TfrmPasteDialog;

const
  HEIGHT_CheckBox = 13;
  HEIGHT_RadioButton = 15;
  HEIGHT_FormBasic = 83;
  HEIGHT_GroupBasic = 30;
  WIDTH_FormBasic = 20;
  WIDTH_FieldsBasic = 135;
  WIDTH_Min = 184;
  LABEL_RowCountText = 'row(s) to paste';
  TEXT_OtherNonKeys = 'Other non keys';

implementation

{$R *.DFM}

uses
  Math, DataElements, DataClipBoard;

function TfrmPasteDialog.Run(AvailableLists, ResultLists : TStringList; var OtherNonKeys : Boolean;
         var PasteRules : TPasteRulesSet; RowCount : Integer ) : Boolean;
var
  ModalResult : Integer;
begin
  SetRowCount( RowCount );
  FDialogType := dtSelectFieldLists;
  AddFieldLists( AvailableLists, OtherNonKeys );
  AddPasteRules( PasteRules  );

  FTotalHeight := MaxIntValue( [FHeightFields, FHeightRules] ) + FTotalHeight;
  Self.Height := FTotalHeight;
  FTotalWidth := MaxIntValue( [FTotalWidth, WIDTH_Min] );

  ModalResult := ShowModal;
  Result := (ModalResult = mrOk);

  MoveFieldListsToResult( ResultLists, OtherNonKeys );
  MoveRulesToResult( PasteRules );
end;

procedure TfrmPasteDialog.SetRowCount( ACount : Integer );
begin
  PanelLabel.Visible := ACount >= 0;

  LabelRowsAffected.Caption := IntToStr( ACount ) + ' ' + LABEL_RowCountText;
  if PanelLabel.Visible then
    Inc( FTotalHeight, PanelLabel.Height );
end;

procedure TfrmPasteDialog.AddPasteRules( PasteRules : TPasteRulesSet );
var
  ARule : TPasteRules;
begin
  RadioGroupRules.Items.Clear;
  for ARule := Low( TPasteRules ) to High( TPasteRules ) do
  begin
    if ARule in PasteRules then
    begin
      RadioGroupRules.Items.AddObject( PasteRuleTexts[ARule], TObject(ARule) );
      Inc( FHeightRules, HEIGHT_RadioButton );
    end;
  end;

  if RadioGroupRules.Items.Count > 0 then
  begin
    Inc( FHeightRules, HEIGHT_GroupBasic );
    Inc( FTotalWidth, RadioGroupRules.Width );
  end
  else
    CheckListBoxFields.Visible := False;

  RadioGroupRules.ItemIndex := 0;
end;

procedure TfrmPasteDialog.MoveRulesToResult( var PasteRules : TPasteRulesSet );
begin
  PasteRules := [ TPasteRules( RadioGroupRules.Items.Objects[RadioGroupRules.ItemIndex] ) ];
end;

procedure TfrmPasteDialog.AddFieldLists( AvailableLists : TStringList; OtherNonKeys : Boolean);

  procedure AddList( ACaption : String; AList : TObject );
  begin
    CheckListBoxFields.Items.AddObject( ACaption, AList );
    Inc( FHeightFields, HEIGHT_CheckBox );
  end;

var
  i : Integer;
begin
  FOtherNonKeys := OtherNonKeys;
  CheckListBoxFields.Clear;

  for i := 0 to AvailableLists.Count -1 do
    AddList( AvailableLists[i], AvailableLists.Objects[i] );
  if OtherNonKeys then
    AddList( TEXT_OtherNonKeys, nil );

  if CheckListBoxFields.Items.Count > 0 then
  begin
    Inc( FHeightFields, HEIGHT_GroupBasic );
    Inc( FTotalWidth, WIDTH_FieldsBasic );
  end
  else
    CheckListBoxFields.Visible := False;

  for i := 0 to CheckListBoxFields.Items.Count -1 do
    CheckListBoxFields.Checked[i] := True;

  CheckListBoxFields.Enabled := True;
end;

procedure TfrmPasteDialog.MoveFieldListsToResult( Results : TStringList; var OtherNonKeys : Boolean );
var
  i : Integer;
  AList : TStrings;
begin
  OtherNonKeys := False;
  AList := CheckListBoxFields.Items;
  for i := 0 to AList.Count -1 do
    if AList.Objects[i] = nil then
      OtherNonKeys := CheckListBoxFields.Checked[i]
    else if CheckListBoxFields.Checked[i] then
      Results.AddObject(AList[i], AList.Objects[i]);
end;

function TfrmPasteDialog.RunTableSelect(EntryList : TValueList; RowLists : TList; var PasteRules : TPasteRulesSet) : Boolean;
var
  iTable : Integer;
  Table : TDataTable;
  CheckRequiredHeight, RadioRequiredHeight : Integer;
begin
  if EntryList.Count <> RowLists.Count then
    raise Exception.Create('TfrmPasteDialog.RunTableSelect: Internal Error: EntryList.Count <> RowLists.Count!');

  FDialogType := dtSelectTables;

  CheckListBoxFields.Clear;

  for iTable := 0 to EntryList.Count -1 do
  begin
    Table := TClipBoardEntry(EntryList.Objects[iTable]).DataTable;
    CheckListBoxFields.Items.Add(Table.TableName + ' ' + Table.Description + ': ' +
                                 IntToStr(TDataRowList(RowLists.Items[iTable]).Count) + ' row(s)');
  end;

  AddPasteRules(PasteRules);
  RadioGroupRules.ItemIndex := 2;   // Skip if row exists

  CheckRequiredHeight := HEIGHT_FormBasic + HEIGHT_GroupBasic + CheckListBoxFields.Items.Count * HEIGHT_CheckBox;
  RadioRequiredHeight := HEIGHT_FormBasic + HEIGHT_GroupBasic + RadioGroupRules.Items.Count * HEIGHT_RadioButton;
  Self.Height := MaxIntValue( [CheckRequiredHeight, RadioRequiredHeight] );

  for iTable := 0 to CheckListBoxFields.Items.Count -1 do
    CheckListBoxFields.Checked[iTable] := AsBoolean(EntryList.Values[iTable]);
    
  CheckListBoxFields.Enabled := True;
  GroupBoxFields.Caption := 'Tables';
  Self.Width := Self.Width + 150;
  CheckListBoxFields.Width := CheckListBoxFields.Width + 150;
  GroupBoxFields.Width := GroupBoxFields.Width + 150;

  ModalResult := ShowModal;
  Result := (ModalResult = mrOk);

  for iTable := 0 to CheckListBoxFields.Items.Count -1 do
    EntryList.Values[iTable] := ValueFromBoolean(CheckListBoxFields.Checked[iTable]);

  MoveRulesToResult( PasteRules );
end;

procedure TfrmPasteDialog.RadioGroupRulesClick(Sender: TObject);
begin
//  if FOtherNonKeys then
//    CheckListBoxFields.Enabled := not (RadioGroupRules.ItemIndex = RadioGroupRules.Items.Count -1);
end;

procedure TfrmPasteDialog.FormCreate(Sender: TObject);
begin
  FTotalHeight := HEIGHT_FormBasic;
  FTotalWidth := WIDTH_FormBasic;
end;

end.

