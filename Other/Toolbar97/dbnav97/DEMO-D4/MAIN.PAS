{
*********************************************************
DBNavigator97 Example
*********************************************************
Source modified and enhanced for use with the Toolbar97 components
from Jordan Russell (http://www.digicron.com/jordanr/).

DBNavigator97 forces the user to press Post or Cancel when dataset.state is in
dsInseret or dsEdit. The user is also not allowed to scroll and therefore
you have a minimal errorchecking and errorhandling on you dataset. The scroll disabling
solves only an errorhandling problem if you have a AfterInsert or afteredit event on your dataset,
but for normal use a scroll will cancel inserting or editing mode and set the dataset into
dmBrowse mode.

Try and put the dataset in editmode and then press the exitbutton, that all
the errorcheck and handling you need !

When putting it on a Toolbar97,the buttons will be separated from each
other when running the application,and the toolbar can be resized with
correct "button wrapping" instead of keeping one unseparated block.

I didn't test it with Delphi 1. If there are problems I suppose they have
to do with the database implementation.

© 1999 Jordan Russell}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, DBTables, TB97Tlbr, ExtCtrls, DBNav97, TB97Ctls, TB97, Grids, DBGrids,
  DBCtrls, StdCtrls;

type
  TTB97DBNaviForm = class(TForm)
    DBNaviDock97: TDock97;
    DBNaviToolbar97: TToolbar97;
    TBtnExit: TToolbarButton97;
    DBNavigator971: TDBNavigator97;
    Empty1: TToolbarSep97;
    Empty3: TToolbarSep97;
    DataSource1: TDataSource;
    Table1: TTable;
    Table1Name: TStringField;
    Table1Capital: TStringField;
    Table1Continent: TStringField;
    Table1Area: TFloatField;
    Table1Population: TFloatField;
    DBTB97DBNaviGrid: TDBGrid;
    DBNavigator1: TDBNavigator;
    lblStandard: TLabel;
    Empty2: TToolbarSep97;
    lblInfo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure TBtnExitClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TB97DBNaviForm: TTB97DBNaviForm;

implementation

{$R *.DFM}

procedure TTB97DBNaviForm.FormCreate(Sender: TObject);
begin
  table1.open;
  table1.First;
end;

procedure TTB97DBNaviForm.TBtnExitClick(Sender: TObject);
begin
  if table1.state <> dsbrowse then
  begin
    if messagedlg('You are inserting og editing a record'+#13+'Are you sure you want to quit?',mtwarning,[mbYes,mbNo],0) = mrYes then
    begin
       table1.close;
       Close;
    end;
  end else
  begin
   table1.close;
   Close;
  end;
end;

end.
