unit StandardGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, ExtCtrls, Grids, DBGrids, bvDBGrid, Menus, bvFormSaver,
  StdCtrls, {bvdbTablePrinter, }bvBookMark, bvFindUnit, bvdbTableSaver,
  bvdbGridSaver;

type
  TStandardGridDemoForm = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    bvFormSaver1: TbvFormSaver;
    Table1: TTable;
    DataSource1: TDataSource;
    Panel2: TPanel;
    Label1: TLabel;
    Bevel1: TBevel;
    DBGridSaver1: TDBGridSaver;
    DBTableSaver1: TDBTableSaver;
    bv_Find1: bv_Find;
    bvBookMark1: TbvBookMark;
    procedure DBGrid1GetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  StandardGridDemoForm: TStandardGridDemoForm;

implementation

{$R *.DFM}

procedure TStandardGridDemoForm.DBGrid1GetCellParams(Sender: TObject;
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

procedure TStandardGridDemoForm.FormCreate(Sender: TObject);
begin
  if not table1.active then begin
    table1.tablename:=extractfilepath(application.exename)+table1.tablename;
    table1.active:=true;
  end;
end;

procedure TStandardGridDemoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree
end;

end.
