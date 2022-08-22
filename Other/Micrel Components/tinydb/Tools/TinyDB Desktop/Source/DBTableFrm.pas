unit DBTableFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, Db, TinyDB, DBCtrls,
  ComCtrls, StdCtrls, ExtCtrls, ToolWin, BaseFrm;

type
  TDBTableFormData = record
    TinyDatabase: TTinyDatabase;
    TableName: string;
  end;

  TDBTableForm = class(TBaseForm)
    ToolBar: TToolBar;
    IndexPanel: TPanel;
    IndexLabel: TLabel;
    IndexComboBox: TComboBox;
    ToolButton1: TToolButton;
    DBNavigator: TDBNavigator;
    DataSource: TDataSource;
    TinyTable: TTinyTable;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormActivate(Sender: TObject);
    procedure IndexComboBoxClick(Sender: TObject);
  private
    { Private declarations }
    FValue: TDBTableFormData;
    function GetTableName: string;
    procedure InitIndexComboBox;
  public
    { Public declarations }
    procedure TransLanguage; override;
    procedure SetData(Value: TDBTableFormData); virtual;
    property TableName: string read GetTableName;
  end;

var
  DBTableForm: TDBTableForm;

implementation

uses MainFrm, DBInfoFrm, LangMgr;

{$R *.DFM}

procedure TDBTableForm.TransLanguage;
begin
  inherited;
  DBNavigator.Hints[0] := AppLangMgr.Trans('First record');
  DBNavigator.Hints[1] := AppLangMgr.Trans('Prior record');
  DBNavigator.Hints[2] := AppLangMgr.Trans('Next record');
  DBNavigator.Hints[3] := AppLangMgr.Trans('Last record');
  DBNavigator.Hints[4] := AppLangMgr.Trans('Insert record');
  DBNavigator.Hints[5] := AppLangMgr.Trans('Delete record');
  DBNavigator.Hints[6] := AppLangMgr.Trans('Edit record');
  DBNavigator.Hints[7] := AppLangMgr.Trans('Post edit');
  DBNavigator.Hints[8] := AppLangMgr.Trans('Cancel edit');
  DBNavigator.Hints[9] := AppLangMgr.Trans('Refresh data');
  IndexComboBox.BoundsRect := Rect(IndexLabel.BoundsRect.Right + 4,
    IndexComboBox.Top, IndexComboBox.BoundsRect.Right, IndexComboBox.BoundsRect.Bottom);
  Caption := AppLangMgr.Trans('Table') + ' [' + FValue.TableName + ']';
end;

procedure TDBTableForm.SetData(Value: TDBTableFormData);
begin
  FValue := Value;
  Caption := AppLangMgr.Trans('Table') + ' [' + Value.TableName + ']';
  TinyTable.DatabaseName := Value.TinyDatabase.DatabaseName;
  TinyTable.TableName := Value.TableName;
  TinyTable.Open;
  InitIndexComboBox;
  MainForm.TableComboBox.Text := Value.TableName;
end;

function TDBTableForm.GetTableName: string;
begin
  Result := FValue.TableName;
end;

procedure TDBTableForm.InitIndexComboBox;
var
  I: Integer;
begin
  IndexComboBox.Items.Clear;
  IndexComboBox.Items.Add('(' + AppLangMgr.Trans('None') + ')');
  for I := 0 to TinyTable.IndexDefs.Count - 1 do
    IndexComboBox.Items.Add(TinyTable.IndexDefs[I].Name);
  IndexComboBox.ItemIndex := 0;
end;

procedure TDBTableForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if MainForm.MDIChildCount = 1 then
    MainForm.TableComboBox.ItemIndex := -1;
  TinyTable.Close;
  Action := caFree;
end;

procedure TDBTableForm.FormActivate(Sender: TObject);
begin
  if DBInfoForm <> nil then DBInfoForm.LocateNodeOfTableName(TableName);
  MainForm.TableComboBox.Text := TableName;
end;

procedure TDBTableForm.IndexComboBoxClick(Sender: TObject);
begin
  if IndexComboBox.ItemIndex = 0 then
    TinyTable.IndexName := ''
  else
    TinyTable.IndexName := IndexComboBox.Text;
end;

end.
