Unit BakSrvMn;

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
  FsDB,
  FsDBBase,
  Db,
  Stdctrls,
  Buttons,
  FsSRBDE {error codes},
  FsLLDict,
  FsClIntf,
  FsLLBase,
  fsBackup,
  ExtCtrls,
  Menus,
  fslleng,
  fssrintm,
  fsllcomp,
  fsllcomm,
  fslllgcy,
  fsserverremoteclass;

Type
  TFormBackup = Class(TForm)
    Panel1: TPanel;
    NameLabel: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    StatusLabel: TLabel;
    CopyBtn: TBitBtn;
    CompareBtn: TBitBtn;
    RestoreBtn: TBitBtn;
    BAKDirEdit: TEdit;
    BackupTable1: TFSBackupTable;
    ListBox1: TListBox;
    Button1: TButton;
    AliasEdit: TComboBox;
    PopupMenu1: TPopupMenu;
    Selectall1: TMenuItem;
    FSSession1: TFSSession;
    FSClient1: TFSClient;
    FSParamConnect1: TFSParamConnect;
    FSRemoteServer1: TFSRemoteServer;
    Procedure RestoreBtnClick(Sender: TObject);
    Procedure CopyBtnClick(Sender: TObject);
    Procedure BackupTable1TableCopyBegin(Sender: TObject; TableNo: Integer;
      TableName: String);
    Procedure CompareBtnClick(Sender: TObject);
    Procedure BackupTable1TableCompareBegin(Sender: TObject;
      TableNo: Integer; TableName: String);
    Procedure BackupTable1TableRestoreBegin(Sender: TObject;
      TableNo: Integer; TableName: String);
    Procedure BackupTable1Encrypt(Sender: TObject; TableNo: Integer;
      TableName: String; IsBLOB: Boolean; aBlock: Pointer;
      aBlockLen: TffWord32);
    Procedure FormShow(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Selectall1Click(Sender: TObject);
    Procedure AliasEditChange(Sender: TObject);
  Private
    { Private declarations }
    CallCount: Integer;
  Public
    { Public declarations }
  End;

Var
  FormBackup: TFormBackup;

Implementation

{$R *.DFM}

Procedure TFormBackup.CopyBtnClick(Sender: TObject);
Var
  AnAlias: String;
  i: Integer;
Begin
  AnAlias := AliasEdit.Text;
  With BackupTable1 Do
    Begin
      Active := False;
      DataBaseName := AnAlias;
      BackupDir := BAKDirEdit.Text;
    End;

  {GetTableName sets BackupTable1.DatbaseName to '' when it is called
  the SECOND time - 3rd + calls seem to work OK.
  This does not happen if the BackupTable is made Active before the call!!!

  It seems to be associated with the following code in CopyDatabase.
  Using exit; just before Active:=true stops it happening, but
  exit; just after, and it happens!!!  What does opening a table
  the first time do to the default Session to make this occur???

    Active:=false;
    TableName:=ATableName;
    IndexName:='';
    Active:=true;

  Placing the BackupTable setup code AFTER
  this call avoids the problem (still gets clobbered tho)
  - BUT why does it happen??????

  Also, using a separate TffSession (other than the default) does
  not exhibit this weird behaviour.

  As the code stands, you can press Copy, then Compare - the compare
  will fail because the DatbaseName is '' - un comment the ShowMessage
  to see this...
  I have left it like this so someone can provide the reason. HELP!

  This problem has been duplicated in TestBugBtn method.  Press it
  twice, and see the result!}
  BackupTable1.TableList.Clear;
  For i := 0 To ListBox1.Items.Count - 1 Do
    Begin
      If ListBox1.Selected[i] Then
        BackupTable1.TableList.Add(ListBox1.Items[i]);
    End;

  Screen.Cursor := crHourglass;
  Try
    inc(CallCount);
    {if CallCount<>2 then}
    If BackupTable1.CopyDatabase Then
      ShowMessage('SUCCESSFUL COPY')
    Else
      ShowMessage('FAILED COPY');
  Finally
    Screen.Cursor := crDefault;
  End;
End;

Procedure TFormBackup.BackupTable1TableCopyBegin(Sender: TObject;
  TableNo: Integer; TableName: String);
Begin
  StatusLabel.Caption := 'copying ' + TableName;
End;

Procedure TFormBackup.CompareBtnClick(Sender: TObject);
Var
  AnAlias: String;
  i: Integer;
Begin
  AnAlias := AliasEdit.Text;
  With BackupTable1 Do
    Begin
      Active := False;
      DataBaseName := AnAlias;
      BackupDir := BAKDirEdit.Text;
    End;
  BackupTable1.TableList.Clear;
  For i := 0 To ListBox1.Items.Count - 1 Do
    Begin
      If ListBox1.Selected[i] Then
        BackupTable1.TableList.Add(ListBox1.Items[i]);
    End;
  Screen.Cursor := crHourglass;
  Try
    If BackupTable1.CompareDatabase Then
      ShowMessage('SUCCESSFUL COMPARE')
    Else
      ShowMessage('FAILED COMPARE');
  Finally
    Screen.Cursor := crDefault;
  End;
End;

Procedure TFormBackup.BackupTable1TableCompareBegin(Sender: TObject;
  TableNo: Integer; TableName: String);
Begin
  StatusLabel.Caption := 'comparing ' + TableName;
End;

Procedure TFormBackup.RestoreBtnClick(Sender: TObject);
Var
  AnAlias: String;
Begin
  AnAlias := AliasEdit.Text;
  With BackupTable1 Do
    Begin
      Active := False;
      DataBaseName := AnAlias;
      BackupDir := BAKDirEdit.Text;
    End;
  Screen.Cursor := crHourglass;
  Try
    BackupTable1.RestoreDatabase;
  Finally
    Screen.Cursor := crDefault;
  End;
End;

Procedure TFormBackup.BackupTable1TableRestoreBegin(Sender: TObject;
  TableNo: Integer; TableName: String);
Begin
  StatusLabel.Caption := 'restoring ' + TableName;
End;

Procedure TFormBackup.BackupTable1Encrypt(Sender: TObject; TableNo: Integer;
  TableName: String; IsBLOB: Boolean; aBlock: Pointer;
  aBlockLen: TffWord32);
Var
  I: TffWord32;
  Buf: PByteArray Absolute aBlock;
Begin
  For I := 0 To aBlockLen - 1 Do
    Buf^[I] := Buf^[I] Xor ord('C');
End;

Procedure TFormBackup.FormShow(Sender: TObject);
Begin
  {for convenience}
  FSSession1.GetAliasNames(AliasEdit.Items);
  If AliasEdit.Items.Count > 0 Then
    Begin
      AliasEdit.ItemIndex := 0;
      Button1Click(Nil);
    End;
  BAKDirEdit.Text := 'c:\';
End;

Procedure TFormBackup.Button1Click(Sender: TObject);
Var
  AnAlias: String;
Begin
  AnAlias := AliasEdit.Text;
  With BackupTable1 Do
    Begin
      Active := False;
      DataBaseName := AnAlias;
      BackupDir := BAKDirEdit.Text;
    End;
  FSSession1.GetTableNames(AnAlias,
    '' {mask},
    False {extention in name},
    False {system-SQL},
    ListBox1.Items);
End;

Procedure TFormBackup.Selectall1Click(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To Listbox1.Items.Count - 1 Do
    Listbox1.Selected[i] := True;
End;

Procedure TFormBackup.AliasEditChange(Sender: TObject);
Begin
  Button1Click(Nil);
End;

End.

