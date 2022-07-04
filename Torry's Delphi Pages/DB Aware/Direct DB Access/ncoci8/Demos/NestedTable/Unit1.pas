// This demo shows how to use TOCINestedDataSet in case of
// Oracle8 nested tables. To use this demo you must first run
// script NestedTable.sql. Current implementation does not
// support editing for any dataset: parent and nested. So,
// all possible errors like: 'fetch out of sequence',
// OCI_INVALID_HANDLE are due that limited support.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOci, NCOciWrapper, NCOciDB, Grids, DBGrids, NCSQLMon;

type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    OCIQuery1: TOCIQuery;
    OCINestedDataSet1: TOCINestedDataSet;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    OCINestedDataSet1NUM: TFloatField;
    OCINestedDataSet1STR: TStringField;
    OCINestedDataSet1DAT: TDateTimeField;
    OCIQuery1CODE: TFloatField;
    OCIQuery1ROWID: TStringField;
    OCIQuery1CRS: TDataSetField;
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
