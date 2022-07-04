// This demo shows how to work with "ref cursor" parameters
// of PL/SQL procedures. Here OCINestedDataSet1 is binded
// to "ref cursor" parameter of OCIStoredProc1:
//    OCINestedDataSet1.ParamDataSet -> OCIStoredProc1
//    OCINestedDataSet1.ParamName -> 'C'
// Also it is possible simply set
//    OCIStoredProc1.Active = True (before, you must disconnect
//                                  OCINestedDataSet1 from OCIStoredProc1)
// To use this demo you should run script RefCursor.sql

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DBCtrls, NCOciDB, NCOciUpdateSQL, Db, NCOci, NCOciWrapper,
  Grids, DBGrids;

type
  TForm1 = class(TForm)
    OCIDatabase1: TOCIDatabase;
    OCIStoredProc1: TOCIStoredProc;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    OCINestedDataSet1: TOCINestedDataSet;
    OCIUpdateSQL1: TOCIUpdateSQL;
    DBNavigator1: TDBNavigator;
    OCINestedDataSet1PRODUCT_ID: TFloatField;
    OCINestedDataSet1DESCRIPTION: TStringField;
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
