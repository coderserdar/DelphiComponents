unit Data;

interface

uses
  SysUtils, Classes, Provider, DBClient, DBLocal, DBLocalI, DB,
  IBCustomDataSet, IBQuery, IBDatabase, FxCommon, FxStore, FxDB, FMTBcd,
  DBXpress, SqlExpr, FxMap, FxExpr;

// Note: DecisionCube.MaxCells set to 200000 for demontration loading
//   Dimensions and Summaries with ActiveFlag=diAsNeeded.
//   Set MaxCells to bigger number to load all dimensions and summaries

type
  TDM = class(TDataModule)
    prvDict: TDataSetProvider;
    cust: TClientDataSet;
    prvSale: TDataSetProvider;
    vendor: TClientDataSet;
    Database: TIBDatabase;
    Transaction: TIBTransaction;
    qryDict: TIBQuery;
    qrySale: TIBQuery;
    sale: TClientDataSet;
    saleSALEDATE: TDateTimeField;
    saleCUSTNO: TFloatField;
    saleVENDORNO: TFloatField;
    saleCOUNTALL: TIntegerField;
    saleQTY: TIntegerField;
    saleSUBT: TFloatField;
    saleCITY: TStringField;
    DecisionCube: TFxCube;
    procedure DatabaseAfterConnect(Sender: TObject);
    procedure DecisionCubeAfterOpen(DataCube: TFxCustomStore);
    procedure DecisionCubeDimensionMap0Transform(var Value: Variant;
      Data: TDimensionItem);
    function DecisionCubeCall(const PID, Off: Integer;
      const Params: TExprs): Variant;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;

implementation

uses DbLogDlg, Forms, Variants;

{$R *.dfm}

// Open dictionaries for DecisionCube Dimensionsions.Kind=dkLookup

procedure TDM.DatabaseAfterConnect(Sender: TObject);
begin
//  cust.Open;
//  vendor.Open;
  // Not need now.  Decision Cube open Lookup DataSets
end;

// This save server,network and memory resources
// If you will have changed params for sales dataset, then
// clear FileName property or/and delete file

procedure TDM.DecisionCubeAfterOpen(DataCube: TFxCustomStore);
var
  FileName:string;
begin
  FileName:=ExtractFilePath(Application.ExeName)+'sale.cds';
  sale.SaveToFile(FileName);
  sale.Close;
  sale.FileName:=FileName;
end;

// Sample Transform Event
// Group by 1,10,20
procedure TDM.DecisionCubeDimensionMap0Transform(var Value: Variant;
  Data: TDimensionItem);
var
  Present:TDateTime;
  Y,M,D:Word;
begin
  Present:=Value;
  DecodeDate(Present,Y,M,D);
  if D<10 then
    D:=1
  else if D<20 then
    D:=10
  else
    D:=20;
  Value:=EncodeDate(Y,M,D);
end;

// Smaple implementation User defined function Test(Value)= return -Value;

function TDM.DecisionCubeCall(const PID,Off:Integer; const Params:TExprs):Variant;
begin
  Result:=-Params[0].Eval(Off);
end;

end.
