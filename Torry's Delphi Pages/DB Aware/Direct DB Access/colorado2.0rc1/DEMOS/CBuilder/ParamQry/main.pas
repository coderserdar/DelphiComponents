unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  colorado, Db, StdCtrls, DBCtrls, ExtCtrls, Grids, DBGrids;

type
  TMainForm = class(TForm)
    Connection1: TConnection;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    CQuery1: TCQuery;
    DataSource2: TDataSource;
    CQuery2: TCQuery;
    DBGrid2: TDBGrid;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}


end.
