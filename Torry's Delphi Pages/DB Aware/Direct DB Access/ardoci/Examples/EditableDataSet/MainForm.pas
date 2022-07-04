{

   ************** PLEASE READ TEXT BELOW BEFORE FIRST RUN OF EXAMPLE!! *****************

   Example how to make editable DataSet using TOraSQL and TOraDB components.
   To run this example you need to cteate table Test in your Oracle database:
   CREATE TABLE Test {
    F1  VARCHAR2(10),
    F2  NUMBER(9,4),
    F3  VARCHAR(200)
    )
    and type DBLogin, DBPassword, DBServer in the appropriate properties of OraDB component
    for example this properties may look like this:
     DBLogin = test
     DBPassword = testpass
     DBServer = ORCL - the name of your Oracle instance

     After that you should succesfully compile and run this example.
     Enjoy!

     Good luck!
       Andrey Romanchenko     lasersquard@yahoo.com
}

unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ADataSet, OraDB, Grids, DBGrids, Db, VirtualDataSet, DataSetQuery, OraSQL,
  ExtCtrls, DBCtrls, AOraUpdateSQL;

type
  TFormMain = class(TForm)
    OraSQL: TOraSQL;
    DataSource: TDataSource;
    DBGrid: TDBGrid;
    OraDB: TOraDB;
    AOraUpdateSQL: TAOraUpdateSQL;
    Panel1: TPanel;
    DBNavigator: TDBNavigator;
    procedure FormShow(Sender: TObject);
    procedure OraSQLUpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.FormShow(Sender: TObject);
begin
 OraSQL.Open;
end;

// TOraSQL.UpdateRecord event calls once during Post method.
// This event allows users to make complex database updates (cascading updates or deletes)
// or make an update of a set of tables.
// In this example we use TOraSQL.UpdateRecord only for apply changes to server (OraSQL.ApplyUpdates).
procedure TFormMain.OraSQLUpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  OraDB.StartTransaction;

  // applies changes in DBGrid to the Oracle server i.e. executes sql queries from AOraUpdateSQL.
  OraSQL.ApplyUpdates;

  OraDB.CommitTransaction;

// By default UpdateAction=usFail. That does not allow TOraSQL to make Post.
// Set UpdateAction=uaApplied tells TOraSQL that all database updates applied succesfully.
  UpdateAction:=uaApplied;
end;

end.
