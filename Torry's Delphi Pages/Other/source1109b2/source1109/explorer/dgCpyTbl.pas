{$I fsdefine.inc}

Unit dgcpytbl;

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
  Buttons,
  ubase,
  uentity,
  fsdb,
  Spin;

Type
  TdlgCopyToTable = Class(TForm)
    lstTables: TListBox;
    lblImport: TLabel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    cbCopyBlobs: TCheckBox;
    btnNewTable: TButton;
    SpinEdit1: TSpinEdit;
    Label1: TLabel;
    Procedure btnOKClick(Sender: TObject);
    Procedure lstTablesDblClick(Sender: TObject);
    Procedure btnNewTableClick(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
  Private
  Public
    FDatabase: TffeDatabaseItem;
    FTableIndex: Longint;
    FSourceDataset: TfsDataSet;
    FExludeTableName: String;
  End;

Var
  dlgCopyToTable: TdlgCopyToTable;

Function ShowCopyTableDlg(aDatabase: TffeDatabaseItem;
  aExcludeTableIndex: Longint;
  aSourceDataset: TfsDataSet;
  Var aTableIndex: Longint;
  Var aCopyBlobs: Boolean;
  Var aTableItem: TffeTableItem;
  Var aCountPerTrans: Longint): TModalResult; {!!.11}

Implementation

{$R *.DFM}

Uses
  uconfig, {!!.11}
  fmmain; { to refresh tablelist if we create new }

Function ShowCopyTableDlg(aDatabase: TffeDatabaseItem;
  aExcludeTableIndex: Longint;
  aSourceDataset: TfsDataSet;
  Var aTableIndex: Longint;
  Var aCopyBlobs: Boolean;
  Var aTableItem: TffeTableItem;
  Var aCountPerTrans: Longint): TModalResult; {!!.11}
Var
  T: Longint;
  TableName: String; {!!.11}
  { we must save the tablename and use it to return the
    possibly changed TableItem. Creating new tables
    changes the tablelist structure and invalidates
    the passed-in aTableItem. }
Begin
  With TdlgCopyToTable.Create(Nil) Do
    Try
      TableName := aTableItem.TableName; {!!.11}
      FDatabase := aDatabase;
      FSourceDataset := aSourceDataset;
      FDatabase := aDatabase;
      lstTables.Clear;
      For T := 0 To pred(FDatabase.TableCount) Do
        With FDatabase.Tables[T] Do
          If T <> aExcludeTableIndex Then
            lstTables.Items.AddObject(TableName, Pointer(T))
          Else
            FExludeTableName := FDatabase.Tables[T].TableName;
      lstTables.ItemIndex := 0;
      Result := ShowModal;
      aTableIndex := -1;
      If Result = mrOK Then
        Begin
          aTableIndex := FTableIndex;
          aCopyBlobs := cbCopyBlobs.Checked;
          aCountPerTrans:= SpinEdit1.Value;
        End;
      { ensure we reset aTableName; it could have
        changed in the underlying structure }
      {Begin !!.11}
      If Assigned(aTableItem) Then
        For T := 0 To Pred(aDatabase.TableCount) Do
          If aDatabase.Tables[T].TableName = TableName Then
            Begin
              aTableItem := aDatabase.Tables[T];
              break;
            End;
      {End !!.11}
    Finally
      Free;
    End;
End;

Procedure TdlgCopyToTable.lstTablesDblClick(Sender: TObject);
Begin
  btnOk.Click;
End;

Procedure TdlgCopyToTable.btnOKClick(Sender: TObject);
Begin
  With lstTables Do
    FTableIndex := Longint(Items.Objects[ItemIndex]);
End;

Procedure TdlgCopyToTable.btnNewTableClick(Sender: TObject);
Var
  T: Integer;
  NewTableName: String;
Begin
  If InputQuery('New Table', 'Tablename:', NewTableName) Then
    Begin
      FDatabase.CreateTable(NewTableName, FSourceDataset.Dictionary);
      { refresh mainwindow treeview }
      frmMain.outServers.Selected := frmMain.GetEntityNode(etDatabase, FDatabase);
      frmMain.RefreshTables(Self);
      { refresh listbox }
      lstTables.Clear;
      For T := 0 To pred(FDatabase.TableCount) Do
        With FDatabase.Tables[T] Do
          If TableName <> FExludeTableName Then
            lstTables.Items.AddObject(TableName, Pointer(T));
      lstTables.ItemIndex := lstTables.Items.IndexOf(NewTableName);
      btnOk.SetFocus;
    End;
End;

{Begin !!.11}

Procedure TdlgCopyToTable.FormShow(Sender: TObject);
Var
  BaseSection: String;
Begin
  BaseSection := ClassName + '.' + Self.Caption;
  cbCopyBlobs.Checked := FFEConfigGetBoolean(BaseSection, 'Copy BLOBs', True);
End;

Procedure TdlgCopyToTable.FormClose(Sender: TObject; Var Action: TCloseAction);
Var
  BaseSection: String;
Begin
  BaseSection := ClassName + '.' + Self.Caption;
  FFEConfigSaveBoolean(BaseSection, 'Copy BLOBs', cbCopyBlobs.Checked);
End;
{End !!.11}

End.

