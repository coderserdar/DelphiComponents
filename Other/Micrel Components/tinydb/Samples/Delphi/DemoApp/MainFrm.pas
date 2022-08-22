unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, Grids, DBGrids, StdCtrls, ExtCtrls, ComCtrls, TinyDB;

type
  TMainForm = class(TForm)
    PageControl: TPageControl;
    DatabaseTabSheet: TTabSheet;
    Label1: TLabel;
    StatePanel: TPanel;
    OpenDatabaseButton: TButton;
    CloseDatabaseButton: TButton;
    GroupBox5: TGroupBox;
    Label11: TLabel;
    Label14: TLabel;
    CompCheckBox: TCheckBox;
    CompAlgoComboBox: TComboBox;
    CompLevelComboBox: TComboBox;
    GroupBox6: TGroupBox;
    Label12: TLabel;
    Label13: TLabel;
    EncryptCheckBox: TCheckBox;
    EncAlgoComboBox: TComboBox;
    EncPwdEdit: TEdit;
    FileNameEdit: TEdit;
    CreateDatabaseButton: TButton;
    BrowseButton: TButton;
    RecordsTabSheet: TTabSheet;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    NameEdit: TEdit;
    AgeEdit: TEdit;
    SexComboBox: TComboBox;
    TelEdit: TEdit;
    AddrEdit: TEdit;
    NoteMemo: TMemo;
    AppendButton: TButton;
    PostButton: TButton;
    FirstButton: TButton;
    PrevButton: TButton;
    NextButton: TButton;
    LastButton: TButton;
    SearchTabSheet: TTabSheet;
    GroupBox2: TGroupBox;
    Label8: TLabel;
    GotoKeyNameEdit: TEdit;
    GotoKeyButton: TButton;
    GroupBox3: TGroupBox;
    Label9: TLabel;
    FindNameEdit: TEdit;
    FindFirstButton: TButton;
    FindNextButton: TButton;
    SearchDBGrid: TDBGrid;
    FilterTabSheet: TTabSheet;
    FilterDBGrid: TDBGrid;
    TinyDatabase1: TTinyDatabase;
    TinyTable1: TTinyTable;
    DataSource1: TDataSource;
    Panel1: TPanel;
    FilterButton: TButton;
    CancelFilterButton: TButton;
    FilterEdit: TEdit;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure CreateDatabaseButtonClick(Sender: TObject);
    procedure OpenDatabaseButtonClick(Sender: TObject);
    procedure CloseDatabaseButtonClick(Sender: TObject);
    procedure AppendButtonClick(Sender: TObject);
    procedure PostButtonClick(Sender: TObject);
    procedure FirstButtonClick(Sender: TObject);
    procedure PrevButtonClick(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure LastButtonClick(Sender: TObject);
    procedure GotoKeyButtonClick(Sender: TObject);
    procedure FindFirstButtonClick(Sender: TObject);
    procedure FindNextButtonClick(Sender: TObject);
    procedure FilterButtonClick(Sender: TObject);
    procedure CancelFilterButtonClick(Sender: TObject);
    procedure CompCheckBoxClick(Sender: TObject);
    procedure EncryptCheckBoxClick(Sender: TObject);
    procedure TinyTable1AfterOpen(DataSet: TDataSet);
    procedure TinyTable1AfterClose(DataSet: TDataSet);
    procedure TinyTable1AfterScroll(DataSet: TDataSet);
  private
    { Private declarations }
    procedure LoadFieldData;
    procedure SaveFieldData;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}

{ TMainForm }

procedure TMainForm.LoadFieldData;
begin
  NameEdit.Text := TinyTable1.FieldByName('Name').AsString;
  AgeEdit.Text := TinyTable1.FieldByName('Age').AsString;
  if TinyTable1.FieldByName('Sex').AsBoolean then
    SexComboBox.ItemIndex := 0
  else
    SexComboBox.ItemIndex := 1;
  TelEdit.Text := TinyTable1.FieldByName('Tel').AsString;
  AddrEdit.Text := TinyTable1.FieldByName('Addr').AsString;
  NoteMemo.Lines.Text := TinyTable1.FieldByName('Note').AsString;
end;

procedure TMainForm.SaveFieldData;
begin
  TinyTable1.FieldByName('Name').AsString := NameEdit.Text;
  TinyTable1.FieldByName('Age').AsInteger := StrToInt(AgeEdit.Text);
  if SexComboBox.ItemIndex = 0 then
    TinyTable1.FieldByName('Sex').AsBoolean := True
  else
    TinyTable1.FieldByName('Sex').AsBoolean := False;
  TinyTable1.FieldByName('Tel').AsString := TelEdit.Text;
  TinyTable1.FieldByName('Addr').AsString := AddrEdit.Text;
  TinyTable1.FieldByName('Note').AsString := NoteMemo.Lines.Text;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage := DatabaseTabSheet; 
  FileNameEdit.Text := ExtractFilePath(Application.ExeName) + 'Test.tdb';
  TTinyDatabase.GetCompressAlgoNames(CompAlgoComboBox.Items);
  TTinyDatabase.GetEncryptAlgoNames(EncAlgoComboBox.Items);
end;

procedure TMainForm.BrowseButtonClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
begin
  SaveDialog := TSaveDialog.Create(Self);

  SaveDialog.Filter := 'TinyDatabase Files(*.tdb)|*.tdb|Any Files(*.*)|*.*';
  SaveDialog.DefaultExt := '.tdb';
  SaveDialog.Options := SaveDialog.Options + [ofOverwritePrompt];
  if SaveDialog.Execute then
    FileNameEdit.Text := SaveDialog.FileName;
  SaveDialog.Free;
end;

procedure TMainForm.CreateDatabaseButtonClick(Sender: TObject);
var
  Ok: Boolean;
begin
  //Create database
  Ok := TinyDatabase1.CreateDatabase(
      FileNameEdit.Text,               //Database file name
      CompCheckBox.Checked,            //Compress or not
      TCompressLevel(CompLevelComboBox.ItemIndex),   //Compression level
      CompAlgoComboBox.Text,           //Compression algorithm
      EncryptCheckBox.Checked,         //Encrypt or not
      EncAlgoComboBox.Text,            //Encryption algorithm
      EncPwdEdit.Text,                 //Password
      False                            //CRC32 or not
      );

  //Create table
  TinyDatabase1.DatabaseName := FileNameEdit.Text;
  TinyDatabase1.Password := EncPwdEdit.Text;
  TinyDatabase1.CreateTable('TestTable', [
    FieldItem('Name', ftString, 32),
    FieldItem('Age', ftInteger),
    FieldItem('Sex', ftBoolean),
    FieldItem('Tel', ftString, 32),
    FieldItem('Addr', ftString, 100),
    FieldItem('Note', ftMemo)
    ] );

  //Create index
  TinyDatabase1.CreateIndex('TestTable', 'NameIndex', [], ['Name']);

  //Open database
  OpenDatabaseButtonClick(nil);

  //Append some records
  TinyTable1.AppendRecord(['Tom', 24, True, '', 'Shanghai', '']);
  TinyTable1.AppendRecord(['John', 22, False, '', 'Wuhan', '']);
  TinyTable1.AppendRecord(['Anna', 22, False, '', 'Nanjing', '']);

  if Ok then
    ShowMessage('Create database successfully.')
  else
    ShowMessage('Fail to create database.');
end;

procedure TMainForm.OpenDatabaseButtonClick(Sender: TObject);
begin
  TinyTable1.Close;
  TinyTable1.DatabaseName := FileNameEdit.Text;
  TinyTable1.Password := EncPwdEdit.Text;
  TinyTable1.TableName := 'TestTable';
  TinyTable1.IndexName := 'NameIndex';
  TinyTable1.Open;
end;

procedure TMainForm.CloseDatabaseButtonClick(Sender: TObject);
begin
  TinyTable1.Close;
end;

procedure TMainForm.AppendButtonClick(Sender: TObject);
begin
  TinyTable1.Append;

  NameEdit.Text := '';
  AgeEdit.Text := '';
  SexComboBox.ItemIndex := -1;
  TelEdit.Text := '';
  AddrEdit.Text := '';
  NoteMemo.Lines.Clear;
end;

procedure TMainForm.PostButtonClick(Sender: TObject);
begin
  SaveFieldData;
  TinyTable1.Post;
end;

procedure TMainForm.FirstButtonClick(Sender: TObject);
begin
  TinyTable1.First;
  LoadFieldData;
end;

procedure TMainForm.PrevButtonClick(Sender: TObject);
begin
  TinyTable1.Prior;
  LoadFieldData;
end;

procedure TMainForm.NextButtonClick(Sender: TObject);
begin
  TinyTable1.Next;
  LoadFieldData;
end;

procedure TMainForm.LastButtonClick(Sender: TObject);
begin
  TinyTable1.Last;
  LoadFieldData;
end;

procedure TMainForm.GotoKeyButtonClick(Sender: TObject);
var
  Found: Boolean;
begin
  TinyTable1.IndexName := 'NameIndex';
  TinyTable1.SetKey;
  TinyTable1.FieldByName('Name').AsString := GotoKeyNameEdit.Text;
  Found := TinyTable1.GotoKey;
  if not Found then ShowMessage('Record not found.');
end;

procedure TMainForm.FindFirstButtonClick(Sender: TObject);
begin
  TinyTable1.Filter := 'Name=''' + FindNameEdit.Text + '''';
  TinyTable1.FindFirst;
end;

procedure TMainForm.FindNextButtonClick(Sender: TObject);
begin
  TinyTable1.Filter := 'Name=''' + FindNameEdit.Text + '''';
  TinyTable1.FindNext;
end;

procedure TMainForm.FilterButtonClick(Sender: TObject);
begin
  TinyTable1.Filter := FilterEdit.Text;
  TinyTable1.Filtered := True;
end;

procedure TMainForm.CancelFilterButtonClick(Sender: TObject);
begin
  TinyTable1.Filtered := False;
end;

procedure TMainForm.CompCheckBoxClick(Sender: TObject);
begin
  CompAlgoComboBox.Enabled := CompCheckBox.Checked;
  CompLevelComboBox.Enabled := CompCheckBox.Checked;
  if CompCheckBox.Checked then
  begin
    CompAlgoComboBox.ItemIndex := 0;
    CompLevelComboBox.ItemIndex := Integer(clNormal);
  end;
end;

procedure TMainForm.EncryptCheckBoxClick(Sender: TObject);
begin
  EncAlgoComboBox.Enabled := EncryptCheckBox.Checked;
  EncPwdEdit.Enabled := EncryptCheckBox.Checked;
  if EncryptCheckBox.Checked then
    EncAlgoComboBox.ItemIndex := 0;
end;

procedure TMainForm.TinyTable1AfterOpen(DataSet: TDataSet);
begin
  StatePanel.Caption := 'Database State: Opened.';
end;

procedure TMainForm.TinyTable1AfterClose(DataSet: TDataSet);
begin
  StatePanel.Caption := 'Database State: Closed.';
end;

procedure TMainForm.TinyTable1AfterScroll(DataSet: TDataSet);
begin
  LoadFieldData;
end;

end.
