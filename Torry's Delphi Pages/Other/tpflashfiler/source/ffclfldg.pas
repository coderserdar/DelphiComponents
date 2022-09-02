{*********************************************************}
{* FlashFiler: Field Link Designer Dialog                *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.Inc}

unit ffclfldg;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  DB,
  ffdb,
  ffconst,
  ffdbbase,
  ffllbase;

type
  TfrmFieldLinkDesigner = class(TForm)
    pnlMain: TPanel;
    cboDetailIndexes: TComboBox;
    Label1: TLabel;
    lstDetailFields: TListBox;
    lstMasterFields: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    btnAdd: TButton;
    lstJoinedFields: TListBox;
    btnDelete: TButton;
    btnClear: TButton;
    btnOK: TButton;
    btnCancel: TButton;
    Label4: TLabel;
    procedure cboDetailIndexesClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure lstJoinedFieldsClick(Sender: TObject);
    procedure EnableAddButton(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    DetailTable: TffTable;
    procedure EnableOKButton;
    procedure RemoveJoinExpr(aIndex: Integer);
    procedure ReinsertField(aList: TStrings; aFieldName: TffShStr; aFieldNo: LongInt);
  public
  end;

function ShowFieldLinkDesigner(aMasterTable: TDataSet;
                               aDetailTable: TffTable;
                           var aDetailIndex,
                               aDetailFields,
                               aMasterFields: TffShStr): TModalResult;

implementation

{$R *.DFM}

const
  JoinSeparator= ' -> ';

type
  TffJoinedFieldNos = record
    MasterFieldNo: Word;  { MasterFieldNo must be stored before DetailFieldNo }
    DetailFieldNo: Word;  { to preserve numerical ordering when ReinsertField }
                          { is called by btnAddClick }
  end;

function ShowFieldLinkDesigner(aMasterTable: TDataset;
                               aDetailTable: TffTable;
                           var aDetailIndex,
                               aDetailFields,
                               aMasterFields: TffShStr): TModalResult;
var
  I, J, K: Integer;
  FieldName: TffShStr;
begin
  J := 0;
  with TfrmFieldLinkDesigner.Create(Application) do
    try
      DetailTable := aDetailTable;
      DetailTable.FieldDefs.Update;
      DetailTable.IndexDefs.Update;

      { Populate detail indexes }
      with cboDetailIndexes do begin
        DetailTable.GetIndexNames(Items);
        Items.Delete(0);    { remove the seq access index }
        ItemIndex := -1;
        if Items.Count <> 0 then
          ItemIndex := 0;
          if aDetailIndex = '' then
            if aDetailFields <> '' then
              try
                aDetailIndex := DetailTable.IndexDefs.FindIndexForFields(aDetailFields).Name
              except
                aDetailIndex := ''; {eat exceptions}
              end;
          if aDetailIndex <> '' then
            ItemIndex := Items.IndexOf(aDetailIndex);
      end;

      { Populate detail fields }
      cboDetailIndexesClick(nil);

      { Populate master fields; retain field's position within the record }
      with aMasterTable do begin
        FieldDefs.Update;
        for I := 0 to FieldDefs.Count - 1 do
          with FieldDefs[I] do
            lstMasterFields.Items.AddObject(Name, Pointer(FieldNo));
      end;

      { If an existing join is passed in, set it up }
      while aMasterFields <> '' do begin
        if aDetailIndex = '' then begin
          FFShStrSplit(aDetailFields, ';', FieldName, aDetailFields);
          if FieldName = '' then
            Break;
          J := lstDetailFields.Items.IndexOf(FieldName);
        end
        else
          J := 0;

        FFShStrSplit(aMasterFields, ';', FieldName, aMasterFields);
        K := lstMasterFields.Items.IndexOf(FieldName);

        if (J <> -1) and (K <> -1) then begin
          lstDetailFields.ItemIndex := J;
          lstMasterFields.ItemIndex := K;
          btnAddClick(nil);
        end;
      end;

      Result := ShowModal;
      aDetailIndex := '';
      aDetailFields := '';
      aMasterFields := '';

      if Result = mrOK then begin
        { If all detail fields used, return the index name }
        if lstDetailFields.Items.Count = 0 then begin
          aDetailIndex := cboDetailIndexes.Text;
          aDetailFields := '';
        end

        { otherwise return the detail fields used }
        else begin
          with lstJoinedFields.Items do
            for I := 0 to Count - 1 do begin
              FieldName := Copy(Strings[I], 1, Pos(JoinSeparator, Strings[I]) - 1);
              aDetailFields := aDetailFields + FieldName;
              if I < Count - 1 then
                aDetailFields := aDetailFields + ';';
            end;
        end;

        with lstJoinedFields.Items do
          for I := 0 to Count - 1 do begin
            FieldName := Copy(Strings[I], Pos(JoinSeparator, Strings[I]) + Length(JoinSeparator), 255);
            aMasterFields := aMasterFields + FieldName;
            if I < Count - 1 then
              aMasterFields := aMasterFields + ';';
          end;
      end;
    finally
      Free;
    end;
end;

procedure TfrmFieldLinkDesigner.cboDetailIndexesClick(Sender: TObject);
var
  FieldLst,
  OneField: TffShStr;
  P: Integer;
begin
  btnClearClick(Self);
  lstDetailFields.Clear;

  { Populate detail fields, retain the field's position within the index }
  with DetailTable do begin
    FieldLst := IndexDefs[cboDetailIndexes.ItemIndex + 1].Fields;
    P := 1;
    repeat
      FFShStrSplit(FieldLst, ';', OneField, FieldLst);
      lstDetailFields.Items.AddObject(OneField, Pointer(P));
      Inc(P);
    until FieldLst = '';
  end;
  EnableAddButton(Self);
end;

procedure TfrmFieldLinkDesigner.EnableAddButton(Sender: TObject);
begin
  btnAdd.Enabled := (lstDetailFields.ItemIndex <> -1) and
                    (lstMasterFields.ItemIndex <> -1);
end;

procedure TfrmFieldLinkDesigner.EnableOKButton;
begin
  btnOK.Enabled := lstJoinedFields.Items.Count <> 0;
end;

procedure TfrmFieldLinkDesigner.btnAddClick(Sender: TObject);
var
  DI, MI: Integer;
  JoinedFieldNos: TffJoinedFieldNos;
begin
  with lstDetailFields do begin
    DI := ItemIndex;
    JoinedFieldNos.DetailFieldNo := LongInt(Items.Objects[DI]);
  end;
  with lstMasterFields do begin
    MI := lstMasterFields.ItemIndex;
    JoinedFieldNos.MasterFieldNo := LongInt(Items.Objects[MI]);
  end;
  ReinsertField(lstJoinedFields.Items,
                lstDetailFields.Items[DI] + JoinSeparator + lstMasterFields.Items[MI],
                LongInt(JoinedFieldNos));
(*
  with lstJoinedFields.Items do begin
    AddObject(lstDetailFields.Items[DI] +
              JoinSeparator +
              lstMasterFields.Items[MI],
              Pointer(JoinedFieldNos));
  end;
*)
  lstDetailFields.Items.Delete(DI);
  lstMasterFields.Items.Delete(MI);

  btnClear.Enabled := True;
  EnableOKButton;
end;

procedure TfrmFieldLinkDesigner.lstJoinedFieldsClick(Sender: TObject);
begin
  btnDelete.Enabled := True;
end;

procedure TfrmFieldLinkDesigner.btnDeleteClick(Sender: TObject);
begin
  with lstJoinedFields do
    if ItemIndex <> -1 then
      RemoveJoinExpr(ItemIndex);
end;

procedure TfrmFieldLinkDesigner.btnClearClick(Sender: TObject);
begin
  with lstJoinedFields do
    while Items.Count <> 0 do
      RemoveJoinExpr(Items.Count - 1);
end;

procedure TfrmFieldLinkDesigner.RemoveJoinExpr(aIndex: Integer);
var
  P: Integer;
  JoinExpr: AnsiString;
  JoinedFieldNos: TffJoinedFieldNos;
begin
  with lstJoinedFields do begin
    JoinExpr := Items[aIndex];
    P := Pos(JoinSeparator, JoinExpr);
    JoinedFieldNos := TffJoinedFieldNos(Items.Objects[aIndex]);
    ReinsertField(lstDetailFields.Items,
                  Copy(JoinExpr, 1, P - 1),
                  JoinedFieldNos.DetailFieldNo);
    ReinsertField(lstMasterFields.Items,
                  Copy(JoinExpr, P + Length(JoinSeparator), 255),
                  JoinedFieldNos.MasterFieldNo);
    Items.Delete(aIndex);
    if Items.Count = 0 then begin
      btnDelete.Enabled := False;
      btnClear.Enabled := False;
    end;
  end;
  EnableOKButton;
end;

procedure TfrmFieldLinkDesigner.ReinsertField(aList: TStrings;
                                              aFieldName: TffShStr;
                                              aFieldNo: LongInt);
var
  I: Integer;
begin
  for I := 0 to aList.Count - 1 do
    if aFieldNo < LongInt(aList.Objects[I]) then begin
      aList.InsertObject(I, aFieldName, Pointer(aFieldNo));
      Exit;
    end;
  aList.AddObject(aFieldName, Pointer(aFieldNo));
end;

procedure TfrmFieldLinkDesigner.btnOKClick(Sender: TObject);
begin
  { Leading detail fields cannot be left unassigned.  Detail fields
    must be assigned from left to right in the index order }
  with lstDetailFields.Items do 
    if Count <> 0 then begin
      if LongInt(Objects[0]) < TffJoinedFieldNos(lstJoinedFields.Items.Objects[0]).DetailFieldNo then
        raise EffDatabaseError.CreateViaCodeFmt(ffccDesign_SLinkDesigner, [Strings[0]], False); {!!.06}
    end;
  ModalResult := mrOK;
end;

end.
