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
    Table1HasChildren: TBooleanField;
    Label3: TLabel;
    DBEdit1: TDBEdit;
    Label4: TLabel;
    Label5: TLabel;
    DBEdit2: TDBEdit;
    Label6: TLabel;
    DBEdit3: TDBEdit;
    Label7: TLabel;
    DBEdit4: TDBEdit;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    procedure UpdateParentHasChildrenValue(ParentKey: Variant);
  public
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
    DisableControls;
    ParentValue := FieldValues[KeyFieldName];
    Append;
    FieldValues[ParentFieldName] := ParentValue;
    FieldValues[HasChildrenFieldName] := False;    
    Post;
    Edit;
    FieldValues[ListFieldName] :=
      'Node ' + IntToStr(FieldByName(KeyFieldName).AsInteger);
    Post;
    UpdateParentHasChildrenValue(ParentValue);
    EnableControls;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ParentValue: Variant;
begin
  with Table1, DTTableTree1.DBTreeFields do
  begin
    DisableControls;
    ParentValue := FieldValues[ParentFieldName];
    Append;
    FieldValues[ParentFieldName] := ParentValue;
    FieldValues[HasChildrenFieldName] := False;    
    Post;
    Edit;
    FieldValues[ListFieldName] := 'Node ' + IntToStr(FieldByName(KeyFieldName).AsInteger);
    Post;
    EnableControls;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  ParentValue: Variant;
begin
  if DTTableTree1.RecordHasChildren then
    Information('Selected node has children. Can not delete.')
  else
    with Table1, DTTableTree1.DBTreeFields do
    begin
      DisableControls;
      ParentValue := FieldValues[ParentFieldName];
      Delete;
      UpdateParentHasChildrenValue(ParentValue);
      EnableControls;
    end;
end;

procedure TForm1.UpdateParentHasChildrenValue(ParentKey: Variant);
var
  HasChildren: Boolean;
  BMark: String;
begin
  with Table1, DTTableTree1.DBTreeFields do
  begin
    BMark := Bookmark;
    HasChildren := Locate(ParentFieldName, ParentKey, []);
    if Locate(KeyFieldName, ParentKey, []) then
    begin
      Edit;
      FieldValues[HasChildrenFieldName] := HasChildren;
      Post;
    end;
    Bookmark := BMark;
  end;
end;

end.
