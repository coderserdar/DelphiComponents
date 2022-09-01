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
    Label1: TLabel;
    Label2: TLabel;
    ImageList1: TImageList;
    Table1Key: TAutoIncField;
    Table1Parent: TIntegerField;
    Table1Name: TStringField;
    Table1IsContinent: TBooleanField;
    Table1IsCountry: TBooleanField;
    Table1IsCity: TBooleanField;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Label2Click(Sender: TObject);
    procedure DTTableTree1DBTreeCreateNode(Sender: TDTDBTreeView;
      Node: PVirtualNode; DataSet: TDataSet);
    procedure DTTableTree1PaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure DTTableTree1GetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer;
      var ImageList: TCustomImageList);
  private
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

procedure TForm1.DTTableTree1DBTreeCreateNode(Sender: TDTDBTreeView;
  Node: PVirtualNode; DataSet: TDataSet);
begin
  with DataSet do                
    Sender.ListNode[Node].Param :=
      VarArrayOf([FieldByName('IsContinent').AsBoolean,
      FieldByName('IsCountry').AsBoolean,
      FieldByName('IsCity').AsBoolean]);
end;

procedure TForm1.DTTableTree1PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  with (Sender as TDTDBTreeView).ListNode[Node] do
  begin
    if (vsSelected in Node.States) and (Column = 0) then
    begin
      TargetCanvas.Font.Color := clWhite;
      TargetCanvas.Brush.Color := clBlue;
    end
    else
    begin
      if Param[0] then
        TargetCanvas.Font.Color := clBlue
      else if Param[1] then
        TargetCanvas.Font.Color := clGreen
      else if Param[2] then
        TargetCanvas.Font.Color := clBlack;
    end;
  end;
end;

procedure TForm1.DTTableTree1GetImageIndexEx(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer;
  var ImageList: TCustomImageList);
begin
  if Column = 0 then
  begin
    ImageList := ImageList1;
    with (Sender as TDTDBTreeView).ListNode[Node] do
    begin
      if Param[0] then
        ImageIndex := 0
      else if Param[1] then
        ImageIndex := 1
      else if Param[2] then
        ImageIndex := 2;
    end
  end;
end;

end.
