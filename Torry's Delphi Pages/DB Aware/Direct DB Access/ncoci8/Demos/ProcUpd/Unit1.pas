// Thsi demo shows how to use TOCIUpdateSQL & Stored Procs
// to edit data. To run demo, you need to run script ProcUpd.sql

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DBCtrls, Grids, DBGrids, Db, NCOciDB, NCOciUpdateSQL, NCOci,
  NCOciWrapper, NCSQLMon;

type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    OCIQuery1: TOCIQuery;
    OCIUpdateSQL1: TOCIUpdateSQL;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
    NCSQLMonitorClient1: TNCSQLMonitorClient;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

end.
