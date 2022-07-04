unit AOraUpdateSQL;

{
  Class for storage statements for update data in tables.
  You can type INSERT, UPDATE and DELETE statements in properties
  InsertSQL, UpdateSQL and DeleteSQL.

  How to use:
   You must drop TAOraUpdateSQL on form and link it to the TOraSQL by property
   UpdateSQls. After you can type 3 query to update data or generate them in
   AOraUpdateSQL designer (double-click on component).

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB;

type
  TAOraUpdateSQL = class(TComponent)
  private
    FDataSet:TDataSet; // The OraSQL which belongs AOraUpdateSQL 
//    procedure SetDelText(const Value: TStrings);
//    procedure SetInsText(const Value: TStrings);
//    procedure SetModText(const Value: TStrings);
//    procedure AddDataSet(DataSet: TDataSet);
//    procedure RemoveDataSet(DataSet: TDataSet);
    procedure SetDataSet(const Value: TDataSet);
    function GetSQL(UpdateKind: TUpdateKind): TStrings;
    function GetSQLIndex(Index: Integer): TStrings;
    procedure SetSQL(UpdateKind: TUpdateKind; const Value: TStrings);
    procedure SetSQLIndex(const Index: Integer; const Value: TStrings);
  protected
   FSQLText: array[TUpdateKind] of TStrings;
//   DelText:TStrings;
//   ModText:TStrings;
//   InsText:TStrings;
  public
   constructor Create(AOwner:TComponent); override;
   destructor Destroy; override;
   property DataSet:TDataSet read FDataSet write SetDataSet;
   property SQL[UpdateKind: TUpdateKind]: TStrings read GetSQL write SetSQL;
  published
{   property DeleteSQL:TStrings read DelText write SetDelText;
   property InsertSQL:TStrings read InsText write SetInsText;
   property ModifySQL:TStrings read ModText write SetModText;}
   property ModifySQL: TStrings index 0 read GetSQLIndex write SetSQLIndex;
   property InsertSQL: TStrings index 1 read GetSQLIndex write SetSQLIndex;
   property DeleteSQL: TStrings index 2 read GetSQLIndex write SetSQLIndex;
  end;

procedure Register;

implementation

uses OraSQL, AOraUpdateSQLEd;

procedure Register;
begin
  RegisterComponents('Data Access', [TAOraUpdateSQL]);
end;

{ TAOraUpdateSQL }

constructor TAOraUpdateSQL.Create(AOwner: TComponent);
var UpdateKind:TUpdateKind;
begin
 inherited Create(AOwner);
 for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
  begin
    FSQLText[UpdateKind] := TStringList.Create;
//    TStringList(FSQLText[UpdateKind]).OnChange := SQLChanged;
  end;

// DelText:=TStringList.Create;
// InsText:=TStringList.Create;
// ModText:=TStringList.Create;
end;

destructor TAOraUpdateSQL.Destroy;
var UpdateKind:TUpdateKind;
begin
// DelText.Free;
// InsText.Free;
// ModText.Free;
  if Assigned(FDataSet) and (TOraSQL(FDataSet).UpdateSQLs = Self) then
    TOraSQL(FDataSet).UpdateSQLs := nil;
  for UpdateKind := Low(TUpdateKind) to High(TUpdateKind) do
    FSQLText[UpdateKind].Free;
 inherited Destroy;
end;

function TAOraUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
 Result := FSQLText[UpdateKind];
end;

function TAOraUpdateSQL.GetSQLIndex(Index: Integer): TStrings;
begin
 Result := FSQLText[TUpdateKind(Index)];
end;

procedure TAOraUpdateSQL.SetDataSet(const Value: TDataSet);
begin
 FDataSet := Value;
end;

{procedure TAOraUpdateSQL.SetDelText(const Value: TStrings);
begin
 DelText.Assign(Value);
end;

procedure TAOraUpdateSQL.SetInsText(const Value: TStrings);
begin
 InsText.Assign(Value);
end;

procedure TAOraUpdateSQL.SetModText(const Value: TStrings);
begin
 ModText.Assign(Value);
end;}

procedure TAOraUpdateSQL.SetSQL(UpdateKind: TUpdateKind;const Value: TStrings);
begin
  FSQLText[UpdateKind].Assign(Value);
end;

procedure TAOraUpdateSQL.SetSQLIndex(const Index: Integer;const Value: TStrings);
begin
  SetSQL(TUpdateKind(Index), Value);
end;

end.
