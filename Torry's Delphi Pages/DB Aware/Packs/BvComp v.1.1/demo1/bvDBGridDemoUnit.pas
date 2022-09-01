unit bvDBGridDemoUnit;

interface

uses
{$ifndef LINUX}
  Windows, Messages, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, Buttons,
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
  bvFormSaver,bvCursorUnit, DB, DBClient, bvDBGrid;

type
  TbvDBGridDemoForm = class(TForm)
    Panel1: TPanel;
    bvDBGrid1: TbvDBGrid;
    bvFormSaver1: TbvFormSaver;
    DataSource1: TDataSource;
    Panel2: TPanel;
    Label1: TLabel;
    Bevel1: TBevel;
    ClientDataSet1: TClientDataSet;
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

{$ifndef LINUX}
{$R *.DFM}
{$else}
{$R *.xfm}
{$endif}

procedure TbvDBGridDemoForm.bvDBGrid1GetCellParams(Sender: TObject;
  Field: TField; AFont: TFont; var Background: TColor; Highlight: Boolean);
begin
  if (Field=ClientDataSet1.fieldbyname('State/prov'))
      and
     (uppercase(field.asstring)='NY')
  then begin
     afont.color:=clRED;
     afont.style:=afont.style+[fsBold];
  end;
end;

procedure TbvDBGridDemoForm.FormCreate(Sender: TObject);
begin
  if not ClientDataset1.active then begin
    ClientDataset1.Filename:=extractfilepath(application.exename)+'custoly.cds';
    ClientDataset1.active:=true;
  end;
end;

procedure TbvDBGridDemoForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree
end;

end.
