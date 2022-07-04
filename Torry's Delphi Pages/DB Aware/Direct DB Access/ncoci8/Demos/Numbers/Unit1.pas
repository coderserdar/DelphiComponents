unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOci, NCOciWrapper, NCOciDB, ExtCtrls, DBCtrls, Grids, DBGrids;

type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    OCIQuery1: TOCIQuery;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    DBNavigator1: TDBNavigator;
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
