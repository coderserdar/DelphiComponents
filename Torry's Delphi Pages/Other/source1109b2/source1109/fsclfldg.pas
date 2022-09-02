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

{$I fsdefine.Inc}

Unit fsclfldg;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Stdctrls,
  ExtCtrls,
  DB,
  fsdb,
  fsconst,
  fsdbbase,
  fsllbase;

Type
  TfsFieldLinkDesigner = Class(TForm)
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
    Procedure cboDetailIndexesClick(Sender: TObject);
    Procedure btnAddClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnClearClick(Sender: TObject);
    Procedure lstJoinedFieldsClick(Sender: TObject);
    Procedure EnableAddButton(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
  Private
    DetailTable: TFSTable;
    Procedure EnableOKButton;
    Procedure RemoveJoinExpr(aIndex: Integer);
    Procedure ReinsertField(aList: TStrings; aFieldName: TffShStr; aFieldNo: Longint);
  Public
  End;

Function ShowFieldLinkDesigner(aMasterTable: TDataSet;
  aDetailTable: TFSTable;
  Var aDetailIndex,
  aDetailFields,
  aMasterFields: TffShStr): TModalResult;

Implementation

{$R *.DFM}

Const
  JoinSeparator = ' -> ';

Type
  TffJoinedFieldNos = Record
    MasterFieldNo: Word; { MasterFieldNo must be stored before DetailFieldNo }
    DetailFieldNo: Word; { to preserve numerical ordering when ReinsertField }
    { is called by btnAddClick }
  End;

Function ShowFieldLinkDesigner(aMasterTable: TDataset;
  aDetailTable: TFSTable;
  Var aDetailIndex,
  aDetailFields,
  aMasterFields: TffShStr): TModalResult;
Var
  I, J, K: Integer;
  FieldName: TffShStr;
Begin
  J := 0;
  With TfsFieldLinkDesigner.Create(Application) Do
    Try
      DetailTable := aDetailTable;
      DetailTable.FieldDefs.Update;
      DetailTable.IndexDefs.Update;

      { Populate detail indexes }
      With cboDetailIndexes Do
        Begin
          DetailTable.GetIndexNames(Items);
          Items.Delete(0); { remove the seq access index }
          ItemIndex := -1;
          If Items.Count <> 0 Then
            ItemIndex := 0;
          If aDetailIndex = '' Then
            If aDetailFields <> '' Then
              Try
                aDetailIndex := DetailTable.IndexDefs.FindIndexForFields(aDetailFields).Name
              Except
                aDetailIndex := ''; {eat exceptions}
              End;
          If aDetailIndex <> '' Then
            ItemIndex := Items.IndexOf(aDetailIndex);
        End;

      { Populate detail fields }
      cboDetailIndexesClick(Nil);

      { Populate master fields; retain field's position within the record }
      With aMasterTable Do
        Begin
          FieldDefs.Update;
          For I := 0 To FieldDefs.Count - 1 Do
            With FieldDefs[I] Do
              lstMasterFields.Items.AddObject(Name, Pointer(FieldNo));
        End;

      { If an existing join is passed in, set it up }
      While aMasterFields <> '' Do
        Begin
          If aDetailIndex = '' Then
            Begin
              FFShStrSplit(aDetailFields, ';', FieldName, aDetailFields);
              If FieldName = '' Then
                Break;
              J := lstDetailFields.Items.IndexOf(FieldName);
            End
          Else
            J := 0;

          FFShStrSplit(aMasterFields, ';', FieldName, aMasterFields);
          K := lstMasterFields.Items.IndexOf(FieldName);

          If (J <> -1) And (K <> -1) Then
            Begin
              lstDetailFields.ItemIndex := J;
              lstMasterFields.ItemIndex := K;
              btnAddClick(Nil);
            End;
        End;

      Result := ShowModal;
      aDetailIndex := '';
      aDetailFields := '';
      aMasterFields := '';

      If Result = mrOK Then
        Begin
          { If all detail fields used, return the index name }
          If lstDetailFields.Items.Count = 0 Then
            Begin
              aDetailIndex := cboDetailIndexes.Text;
              aDetailFields := '';
            End

              { otherwise return the detail fields used }
          Else
            Begin
              With lstJoinedFields.Items Do
                For I := 0 To Count - 1 Do
                  Begin
                    FieldName := Copy(Strings[I], 1, Pos(JoinSeparator, Strings[I]) - 1);
                    aDetailFields := aDetailFields + FieldName;
                    If I < Count - 1 Then
                      aDetailFields := aDetailFields + ';';
                  End;
            End;

          With lstJoinedFields.Items Do
            For I := 0 To Count - 1 Do
              Begin
                FieldName := Copy(Strings[I], Pos(JoinSeparator, Strings[I]) + Length(JoinSeparator), 255);
                aMasterFields := aMasterFields + FieldName;
                If I < Count - 1 Then
                  aMasterFields := aMasterFields + ';';
              End;
        End;
    Finally
      Free;
    End;
End;

Procedure TfsFieldLinkDesigner.cboDetailIndexesClick(Sender: TObject);
Var
  FieldLst,
    OneField: TffShStr;
  P: Integer;
Begin
  btnClearClick(Self);
  lstDetailFields.Clear;

  { Populate detail fields, retain the field's position within the index }
  With DetailTable Do
    Begin
      FieldLst := IndexDefs[cboDetailIndexes.ItemIndex + 1].Fields;
      P := 1;
      Repeat
        FFShStrSplit(FieldLst, ';', OneField, FieldLst);
        lstDetailFields.Items.AddObject(OneField, Pointer(P));
        Inc(P);
      Until FieldLst = '';
    End;
  EnableAddButton(Self);
End;

Procedure TfsFieldLinkDesigner.EnableAddButton(Sender: TObject);
Begin
  btnAdd.Enabled := (lstDetailFields.ItemIndex <> -1) And
    (lstMasterFields.ItemIndex <> -1);
End;

Procedure TfsFieldLinkDesigner.EnableOKButton;
Begin
  btnOK.Enabled := lstJoinedFields.Items.Count <> 0;
End;

Procedure TfsFieldLinkDesigner.btnAddClick(Sender: TObject);
Var
  DI, MI: Integer;
  JoinedFieldNos: TffJoinedFieldNos;
Begin
  With lstDetailFields Do
    Begin
      DI := ItemIndex;
      JoinedFieldNos.DetailFieldNo := Longint(Items.Objects[DI]);
    End;
  With lstMasterFields Do
    Begin
      MI := lstMasterFields.ItemIndex;
      JoinedFieldNos.MasterFieldNo := Longint(Items.Objects[MI]);
    End;
  ReinsertField(lstJoinedFields.Items,
    lstDetailFields.Items[DI] + JoinSeparator + lstMasterFields.Items[MI],
    Longint(JoinedFieldNos));
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
End;

Procedure TfsFieldLinkDesigner.lstJoinedFieldsClick(Sender: TObject);
Begin
  btnDelete.Enabled := True;
End;

Procedure TfsFieldLinkDesigner.btnDeleteClick(Sender: TObject);
Begin
  With lstJoinedFields Do
    If ItemIndex <> -1 Then
      RemoveJoinExpr(ItemIndex);
End;

Procedure TfsFieldLinkDesigner.btnClearClick(Sender: TObject);
Begin
  With lstJoinedFields Do
    While Items.Count <> 0 Do
      RemoveJoinExpr(Items.Count - 1);
End;

Procedure TfsFieldLinkDesigner.RemoveJoinExpr(aIndex: Integer);
Var
  P: Integer;
  JoinExpr: AnsiString;
  JoinedFieldNos: TffJoinedFieldNos;
Begin
  With lstJoinedFields Do
    Begin
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
      If Items.Count = 0 Then
        Begin
          btnDelete.Enabled := False;
          btnClear.Enabled := False;
        End;
    End;
  EnableOKButton;
End;

Procedure TfsFieldLinkDesigner.ReinsertField(aList: TStrings;
  aFieldName: TffShStr;
  aFieldNo: Longint);
Var
  I: Integer;
Begin
  For I := 0 To aList.Count - 1 Do
    If aFieldNo < Longint(aList.Objects[I]) Then
      Begin
        aList.InsertObject(I, aFieldName, Pointer(aFieldNo));
        Exit;
      End;
  aList.AddObject(aFieldName, Pointer(aFieldNo));
End;

Procedure TfsFieldLinkDesigner.btnOKClick(Sender: TObject);
Begin
  { Leading detail fields cannot be left unassigned.  Detail fields
    must be assigned from left to right in the index order }
  With lstDetailFields.Items Do
    If Count <> 0 Then
      Begin
        If Longint(Objects[0]) < TffJoinedFieldNos(lstJoinedFields.Items.Objects[0]).DetailFieldNo Then
          Raise EfsDatabaseError.CreateViaCodeFmt(fsccDesign_SLinkDesigner, [Strings[0]], False); {!!.06}
      End;
  ModalResult := mrOK;
End;

End.

