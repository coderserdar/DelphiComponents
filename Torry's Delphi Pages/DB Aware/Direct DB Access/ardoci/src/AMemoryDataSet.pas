unit AMemoryDataSet;

{
  Implements class which stores data in memory.

  You can use AMemoryDataSet when you need save and access to table oriented 
   data and you no need show this data in DBGrid.
  
  If you need store data in memory and show them in DBGrid you can use 
   MemoryDataSet - the wrapper around AMemoryDataSet and descendant of TDataSet.
 
  AMemoryDataSet more fast then MemoryDataSet.

}

interface

uses
  Windows, Classes, ADataSet, DBConsts;

type
  TAMemoryDataSet = class(TADataSet)
  public
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
//   procedure AddField(FieldName:string;FieldType:TAFieldType;FieldSize:word;Required:boolean); override;
   procedure Fetch;override;
   procedure ClearParams;override;
   procedure AddParam(ParamName:string;FieldType:TAFieldType;ParamType:TAParamType);override;
   procedure Prepare;override;
   procedure UnPrepare;override;
   procedure MoveRecord(FromPos,Count,Offset : integer);   // moving records
  published
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Access', [TAMemoryDataSet]);
end;

{ TAMemoryDataSet }

constructor TAMemoryDataSet.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FFetched:=True;
end;

destructor TAMemoryDataSet.Destroy;
begin
 inherited Destroy;
end;

procedure TAMemoryDataSet.MoveRecord(FromPos, Count, Offset: integer);
var
  i : integer;
begin
  if Offset=0 then exit;
  for i:=0 to FieldCount-1 do
  begin
   if Assigned(FieldByIndex[i].HArrayValues) then FieldByIndex[i].HArrayValues.MoveData(FromPos,Count,Offset);
   if Assigned(FieldByIndex[i].HArrayValuesNull) then FieldByIndex[i].HArrayValuesNull.MoveData(FromPos,Count,Offset);
  end;
end;

procedure TAMemoryDataSet.AddParam(ParamName: string;
  FieldType: TAFieldType; ParamType: TAParamType);
begin
end;

procedure TAMemoryDataSet.ClearParams;
begin
end;

procedure TAMemoryDataSet.Fetch;
begin
end;

procedure TAMemoryDataSet.Prepare;
begin
end;

procedure TAMemoryDataSet.UnPrepare;
begin
end;

end.
