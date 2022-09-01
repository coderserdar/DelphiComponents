unit StandardGrid;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons,
  bvdbTableSaver,
  Grids, DBGrids,
{$else}
  QExtCtrls,
  QControls, QStdCtrls, QButtons,
  QForms,QDialogs,
  Qt,
  QGraphics,
  QGrids, QDBGrids,
{$endif}
  SysUtils, Classes,

  bvDBGrid, bvFormSaver,
  {bvdbTablePrinter, }bvBookMark, bvFindUnit,
  DBClient,
  bvdbGridSaver, DB ;

{$ifndef LINUX}
type
  TStandardGridDemoForm = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    bvFormSaver1: TbvFormSaver;
    DataSource1: TDataSource;
    Panel2: TPanel;
    Label1: TLabel;
    Bevel1: TBevel;
    DBGridSaver1: TDBGridSaver;
    DBTableSaver1: TDBTableSaver;
    bv_Find1: bv_Find;
    bvBookMark1: TbvBookMark;
    ClientDataSet1: TClientDataSet;
    procedure DBGrid1GetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

{$else}

type
  TStandardGridDemoForm = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    bvFormSaver1: TbvFormSaver;
    DataSource1: TDataSource;
    Panel2: TPanel;
    Label1: TLabel;
    Bevel1: TBevel;
    DBGridSaver1: TDBGridSaver;
    bv_Find1: bv_Find;
    bvBookMark1: TbvBookMark;
    ClientDataSet1: TClientDataSet;
    procedure DBGrid1GetCellParams(Sender: TObject; Field: TField;
      AFont: TFont; var Background: TColor; Highlight: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

{$endif}

var
  StandardGridDemoForm: TStandardGridDemoForm;

implementation

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure TStandardGridDemoForm.DBGrid1GetCellParams(Sender: TObject;
  Field: TField; AFont: TFont; var Background: TColor; Highlight: Boolean);
begin
  if (Field=Clientdataset1.fieldbyname('State/prov'))
      and
     (uppercase(field.asstring)='NY')
  then begin
     afont.color:=clRED;
     afont.style:=afont.style+[fsBold];
  end;
end;

procedure TStandardGridDemoForm.FormCreate(Sender: TObject);
begin
  if not clientdataset1.active then begin
    clientdataset1.filename:=extractfilepath(application.exename)+'custoly.cds';
    clientdataset1.active:=true;
  end;
end;

procedure TStandardGridDemoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree
end;

end.
