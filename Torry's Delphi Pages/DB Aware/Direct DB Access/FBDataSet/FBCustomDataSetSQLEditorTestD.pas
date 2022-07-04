unit FBCustomDataSetSQLEditorTestD;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Grids, DBGrids, DB, mydbUnit,
  FBCustomDataSet, SynEdit, ComCtrls;

type
  TFBCustomDataSetSQLEditorTestForm = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    DataSource1: TDataSource;
    FBDataSetTest: TFBDataSet;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    DBGrid1: TDBGrid;
    SynEdit1: TSynEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FBCustomDataSetSQLEditorTestForm: TFBCustomDataSetSQLEditorTestForm;

implementation

{$R *.dfm}

end.
