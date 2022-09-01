unit bvDBGridDemoUnit;

interface

uses                 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, ExtCtrls, Grids, DBGrids, bvDBGrid, Menus, bvFormSaver,
  StdCtrls;

type
  TbvDBGridDemoForm = class(TForm)
    Panel1: TPanel;
    bvDBGrid1: TbvDBGrid;
    bvFormSaver1: TbvFormSaver;
    Table1: TTable;
    DataSource1: TDataSource;
    Panel2: TPanel;
    Label1: TLabel;
    Bevel1: TBevel;
    procedure bvDBGrid1GetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  bvDBGridDemoForm: TbvDBGridDemoForm;

implementation

{$R *.DFM}

procedure TbvDBGridDemoForm.bvDBGrid1GetCellParams(Sender: TObject;
  Field: TField; AFont: TFont; var Background: TColor; Highlight: Boolean);
begin
  if (Field=table1.fieldbyname('State/prov'))
      and
     (uppercase(field.asstring)='NY')
  then begin
     afont.color:=clRED;
     afont.style:=afont.style+[fsBold];
  end;
end;

procedure TbvDBGridDemoForm.FormCreate(Sender: TObject);
begin
  if not table1.active then begin
    table1.tablename:=extractfilepath(application.exename)+table1.tablename;
    table1.active:=true;
  end;
end;

procedure TbvDBGridDemoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree
end;

end.
