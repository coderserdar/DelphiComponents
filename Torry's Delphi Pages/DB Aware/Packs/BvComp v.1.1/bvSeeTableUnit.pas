unit bvSeeTableUnit;

interface

{$ifdef LINUX}
  ERROR: not compatible with LINUX
{$endif}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, ExtCtrls, Grids, DBGrids, bvDBGrid, bvFormSaver, DBCtrls,
  StdCtrls,
  bvLocalization;

type
  TSeeTableForm = class(TForm)
    DataSource: TDataSource;
    Table: TTable;
    Panel2: TPanel;
    DBNavigator: TDBNavigator;
    PanelStatus: TPanel;
    LabData: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TableAfterRefresh(DataSet: TDataSet);
  private
    { Private declarations }
  public
    { Public declarations }
    function SetTable(TableName:string):boolean;
    constructor Create(AOwner:TComponent); override;
  end;

var
  SeeTableForm: TSeeTableForm;

implementation

uses bvBDE;

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

constructor TSeeTableForm.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if Application.MainForm.FormStyle=fsMDIForm
  then Self.FormStyle:=fsMDIChild
  else Self.FormStyle:=fsNormal;
end;

function TSeeTableForm.SetTable(TableName:string):boolean;
begin
  try
    Table.Active:=false;
    Table.TableName:=TableName;
    Table.Active:=true;
    PanelStatus.Caption:=TableName;
    Visible:=true;
    Application.ProcessMessages;
    Result:=true;
  except
    Result:=false;
  end;
end;

procedure TSeeTableForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:=caFree;
end;

procedure TSeeTableForm.FormDestroy(Sender: TObject);
begin
   bvFlush(Table);
   saveform(self);
end;

procedure TSeeTableForm.FormCreate(Sender: TObject);
var i:integer;
begin
  caption:=StrSeeTable;
  with DBNavigator.hints do
  begin
    clear;

    for i:=low(ArrNavigatorHints) to high(ArrNavigatorHints)
    do begin
      add(ArrNavigatorHints[i]);
    end;
  end;

  restoreform(self);
  with TbvDBGrid.create(self) do
  begin
     align:=alClient;
     DataSource:=self.datasource;
     parent:=self;
     loaded;
  end
end;

procedure TSeeTableForm.TableAfterRefresh(DataSet: TDataSet);
var RecN:integer;
    RecC:integer;
begin
   if table.state=dsEdit then LabData.Font.Color:=clGreen
   else if table.state=dsInsert then LabData.Font.color:=clRED
   else LabDATA.Font.Color:=PanelStatus.Font.Color;

   if table.state=dsInActive then LabData.Caption:='0:0'
   else begin
      RecN:=table.recno;
      RecC:=table.Recordcount;
      if recn<0 then RecN:=RecC+1;
      if table.state=dsInsert then labData.Caption:='*:'+inttostr(RecC)
      else labData.Caption:=inttostr(REcN)+':'+inttostr(RecC);
   end;
end;

end.
