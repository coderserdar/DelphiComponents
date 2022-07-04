// This example demonstrate editing capabilities of TOCIQuery:
// - general capabilities. For example You can override
//   commands, such as, delete/insert/..., using TOCIUpdateSQL
// - cached updates.
// - client constraints/ defaults /calc. by expr. fields. See:
//   ENAME    - field contraint
//   HIREDATE - field default expression in natural language
//   Total    - calculated by expression field
//   OCIQuery1.Constraints - record level constraints

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOci, NCOciWrapper, NCOciDB, ExtCtrls, DBCtrls, Grids, DBGrids,
  NCOciUpdateSQL, StdCtrls, NCSQLMon, ComCtrls;

type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    OCIQuery1: TOCIQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    OCIUpdateSQL1: TOCIUpdateSQL;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    ComboBox2: TComboBox;
    CheckBox1: TCheckBox;
    Label3: TLabel;
    ComboBox3: TComboBox;
    NCSQLMonitorClient1: TNCSQLMonitorClient;
    CheckBox2: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    OCIQuery1ENAME: TStringField;
    OCIQuery1JOB: TStringField;
    OCIQuery1MGR: TFloatField;
    OCIQuery1HIREDATE: TDateTimeField;
    OCIQuery1SAL: TFloatField;
    OCIQuery1COMM: TFloatField;
    OCIQuery1DEPTNO: TFloatField;
    OCIQuery1Total: TFloatField;
    OCIQuery1EMPNO: TFloatField;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    StatusBar1: TStatusBar;
    OCIQuery1ROWID: TStringField;
    procedure ComboBox1Click(Sender: TObject);
    procedure ComboBox2Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure DataSource1DataChange(Sender: TObject; Field: TField);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.ComboBox1Click(Sender: TObject);
begin
    OCIUpdateSQL1.LockMode := TOCILockMode(ComboBox1.ItemIndex);
end;

procedure TForm1.ComboBox2Click(Sender: TObject);
begin
    OCIUpdateSQL1.LockPoint := TOCILockPoint(ComboBox2.ItemIndex);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
    OCIUpdateSQL1.UpdateChangedFields := CheckBox1.Checked;
end;

procedure TForm1.ComboBox3Click(Sender: TObject);
begin
    OCIUpdateSQL1.UpdateMode := TUpdateMode(ComboBox3.ItemIndex);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    ComboBox1.ItemIndex := Integer(OCIUpdateSQL1.LockMode);
    ComboBox2.ItemIndex := Integer(OCIUpdateSQL1.LockPoint);
    CheckBox1.Checked := OCIUpdateSQL1.UpdateChangedFields;
    ComboBox3.ItemIndex := Integer(OCIUpdateSQL1.UpdateMode);
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
    OCIQuery1.CachedUpdates := CheckBox2.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
    OCIQuery1.ApplyUpdates;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    OCIQuery1.CancelUpdates;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
    OCIDatabase1.StartTransaction;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
    OCIDatabase1.Commit;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
    OCIDatabase1.Rollback;
end;

procedure TForm1.DataSource1DataChange(Sender: TObject; Field: TField);
begin
    if OCIQuery1.EOF then
        StatusBar1.Panels[0].Text := 'EOF'
    else
        StatusBar1.Panels[0].Text := '';
    if OCIQuery1.BOF then
        StatusBar1.Panels[1].Text := 'BOF'
    else
        StatusBar1.Panels[1].Text := '';
    if OCIQuery1.UpdatesPending then
        StatusBar1.Panels[2].Text := '!UP!'
    else
        StatusBar1.Panels[2].Text := '';
    StatusBar1.Panels[3].Text := IntToStr(OCIQuery1.RecordCount);
end;

end.

