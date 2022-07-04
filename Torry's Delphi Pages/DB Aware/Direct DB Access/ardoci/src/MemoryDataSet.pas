unit MemoryDataSet;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, VirtualDataSet, DataSetQuery, DynamicArrays, ADataSet;

type
  TMemoryDataSet = class(TDataSetQuery)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
   constructor Create(AOwner:TComponent);override;
   procedure OpenWithData(Fields:THArrayPointer);
    // opens MemoryDataSet and fills it with data from parameter Fields
    // Fields - array of pointers on the set of TAField classes
    // each TAField contain a column with data 
    // Fields.Count - will be a count of fields in filling table
    // TAField(Fields[i]).Count - count of records in table (all fields in Fields count must be the same)
   function GetNormalCurrent : integer;
   function VPost(RecordNum:integer):TUpdateAction;override;
   function VInsert(RecordNum:integer):TUpdateAction;override;
   function VDeleteRecord (RecordNum:integer):TUpdateAction; override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Access', [TMemoryDataSet]);
end;

constructor TMemoryDataSet.Create(AOwner: TComponent);
begin
 inherited CreateSet(AOwner,qtMemory);
end;

function TMemoryDataSet.GetNormalCurrent : integer;
begin
  if Self.CurrentRecord < 0 then Result := 0
  else  if Self.CurrentRecord > Self.RecordCount-1 then Result := Self.RecordCount-1
  else Result := Self.CurrentRecord;
end;

procedure TMemoryDataSet.OpenWithData(Fields: THArrayPointer);
var i:integer;
begin
 Query.ClearFields;
 FieldDefs.Clear;
 for i:=0 to Fields.Count-1 do
  FieldDefs.Add(TAField(Fields[i]).Name,TypeAToDelphi(TAField(Fields[i]).FieldType),
  TAField(Fields[i]).FieldSize,TAField(Fields[i]).Required);

 Open;
 Query.Open(Fields);
 SyncBookm;
 Refresh;
end;

function TMemoryDataSet.VDeleteRecord(RecordNum: integer): TUpdateAction;
begin
 Result:=uaApplied;
 Query.DeleteRecord(RecordNum);
end;

function TMemoryDataSet.VInsert(RecordNum: integer): TUpdateAction;
begin
 Result:=uaApplied;
 Query.InsertRecord(RecordNum);
end;

function TMemoryDataSet.VPost(RecordNum: integer): TUpdateAction;
begin
 Result:=uaApplied;
end;

end.
