unit Unit1;

interface

{$I DTDBTree.Inc}

uses
{$IFNDEF TR_DELPHI5}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Provider, DBClient, DB, DBTables, Grids, DBGrids,
  VirtualTrees, DTTableTree, StdCtrls, DTDBTreeView, Mask, DBCtrls, ImgList;

type
  TForm1 = class(TForm)
    Table1: TTable;
    DataSource1: TDataSource;
    DTTableTree1: TDTTableTree;
    Table1ID: TAutoIncField;
    Table1PARENT: TIntegerField;
    Table1NAME: TStringField;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button1: TButton;
    DBGrid1: TDBGrid;
    ImageList1: TImageList;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses DTCommon, ShellAPI;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with Table1 do
  begin
    DatabaseName := ExtractFilePath(Application.ExeName);
    TableType := ttParadox;
    TableName := 'TreeTbl';
    Open;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
   Table1.Close;
end;

procedure TForm1.Label2Click(Sender: TObject);
var
  Addr: array[0..255] of char;
begin
  ShellExecute(Handle, 'open', StrPCopy(Addr, Label2.Caption), nil, nil, SW_SHOW);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  ParentValue: Variant;
begin
  with Table1, DTTableTree1.DBTreeFields do
  begin
    ParentValue := FieldValues[KeyFieldName];
    if ParentValue = NULL then
      ParentValue := ParentOfRootValue;
    Append;
    FieldValues[ParentFieldName] := ParentValue;
    Post;
    Edit;
    FieldValues[ListFieldName] :=
      'Node ' + IntToStr(FieldByName(KeyFieldName).AsInteger);
    Post;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ParentValue: Variant;
begin
  with Table1, DTTableTree1.DBTreeFields do
  begin
    ParentValue := FieldValues[ParentFieldName];
    Append;
    FieldValues[ParentFieldName] := ParentValue;
    Post;
    Edit;
    FieldValues[ListFieldName] := 'Node ' + IntToStr(FieldByName(KeyFieldName).AsInteger);
    Post;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if DTTableTree1.RecordHasChildren then
    Information('Selected node has children. Can not delete.')
  else
    Table1.Delete;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  DTTableTree1.GoTop;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  DTTableTree1.GoDown;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  DTTableTree1.Next;
end;

end.
