{
  This example represents a sampling of the way that you might
  approach trapping a number of database errors.

   ************** PLEASE READ TEXT BELOW BEFORE FIRST RUN OF EXAMPLE!! *****************

   To run this example you need to cteate several tables in your Oracle database server:

   CREATE TABLE CUSTOMER(
     CUSTNO     NUMBER        NOT NULL,
     COMPANY    VARCHAR2(30)  NOT NULL);

   CREATE TABLE ORDERS(
     ORDERNO     NUMBER       NOT NULL,
     CUSTNO      NUMBER       NOT NULL,
     SALEDATE    DATE         NOT NULL,
     SHIPDATE    DATE         NOT NULL,
     EMPNO       NUMBER(9)    NOT NULL);

   CREATE TABLE ITEMS(
     ORDERNO    NUMBER        NOT NULL,
     ITEMNO     NUMBER        NOT NULL,
     PARTNO     NUMBER        NOT NULL,
     QTY        NUMBER        NOT NULL,
     DISCOUNT   NUMBER(9)     NOT NULL);

    Type DBLogin, DBPassword, DBServer in the appropriate properties of OraDB component.
    These properties may look like this:
     DBLogin = test
     DBPassword = testpass
     DBServer = ORCL - the name of your Oracle instance

     After that you should succesfully compile and run this example.
     Enjoy!

     Good luck!
       Andrey Romanchenko     lasersquard@yahoo.com
}

unit DM1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DBTables, DB, ADataSet, OraDB, AOraUpdateSQL, VirtualDataSet,
  DataSetQuery, OraSQL;

type
  TDM = class(TDataModule)
    CustomerSource: TDataSource;
    OrdersSource: TDataSource;
    ItemsSource: TDataSource;
    OraCustomer: TOraSQL;
    OraOrders: TOraSQL;
    OraItems: TOraSQL;
    AOraUpdateCustomer: TAOraUpdateSQL;
    AOraUpdateOrders: TAOraUpdateSQL;
    AOraUpdateItems: TAOraUpdateSQL;
    OraDB: TOraDB;
    OraCustomerCUSTNO: TFloatField;
    OraCustomerCOMPANY: TStringField;
    OraOrdersORDERNO: TFloatField;
    OraOrdersCUSTNO: TFloatField;
    OraOrdersSALEDATE: TDateField;
    OraOrdersSHIPDATE: TDateField;
    OraOrdersEMPNO: TIntegerField;
    OraItemsORDERNO: TFloatField;
    OraItemsITEMNO: TFloatField;
    OraItemsPARTNO: TFloatField;
    OraItemsQTY: TFloatField;
    OraItemsDISCOUNT: TIntegerField;
    procedure OraCustomerDeleteError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure OraOrdersDeleteError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure OraOrdersPostError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure OraItemsPostError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure OraCustomerPostError(DataSet: TDataSet; E: EDatabaseError;
      var Action: TDataAction);
    procedure OraCustomerUpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure OraOrdersUpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
    procedure OraItemsUpdateRecord(DataSet: TDataSet;
      UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;


implementation

{$R *.DFM}

procedure TDM.OraCustomerDeleteError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  MessageDlg(E.Message, mtWarning, [mbOK], 0);
end;

procedure TDM.OraCustomerPostError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  MessageDlg(e.Message, mtWarning, [mbOK], 0);
end;

procedure TDM.OraItemsPostError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  MessageDlg(E.Message, mtWarning,[mbOK],0);
end;

procedure TDM.OraOrdersPostError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  MessageDlg(E.Message, mtWarning, [mbOK], 0);
  Action := daAbort;
end;

procedure TDM.OraOrdersDeleteError(DataSet: TDataSet; E: EDatabaseError;
  var Action: TDataAction);
begin
  MessageDlg(E.Message, mtConfirmation, [mbYes, mbNo], 0);
end;

procedure TDM.OraCustomerUpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  OraDB.StartTransaction;

  // applies changes in DBGrid to the Oracle server i.e. executes sql queries from AOraUpdateSQL.
  (DataSet as TOraSQL).ApplyUpdates;

  OraDB.CommitTransaction;

// By default UpdateAction=usFail. That does not allow TOraSQL to make Post.
// Set UpdateAction=uaApplied tells TOraSQL that all database updates applied succesfully.
  UpdateAction:=uaApplied;
end;

procedure TDM.OraOrdersUpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  OraDB.StartTransaction;

  (DataSet as TOraSQL).ApplyUpdates;

  OraDB.CommitTransaction;

  UpdateAction:=uaApplied;
end;

procedure TDM.OraItemsUpdateRecord(DataSet: TDataSet;
  UpdateKind: TUpdateKind; var UpdateAction: TUpdateAction);
begin
  OraDB.StartTransaction;

  (DataSet as TOraSQL).ApplyUpdates;

  OraDB.CommitTransaction;

  UpdateAction:=uaApplied;
end;

end.



