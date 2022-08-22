unit TinyQuery;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TinyDB, Db, xQuery;

type
  TxTinyQuery = class(TxQuery)
  private
    { Private declarations }
    FDatabase: TTinyDatabase;
    procedure SetDatabase(const Value: TTinyDatabase);
  protected
    { Protected declarations }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    function DataSetByName(const Name: string): TDataSet; //override;

  published
    { Published declarations }
    property Database: TTinyDatabase read FDatabase write SetDatabase;
  end;

procedure Register;

implementation

{$R xTinyQuery.dcr}

{  Register  }

procedure Register;
begin
  RegisterComponents('Data Access2', [TxTinyQuery]);
end;

{ TxTinyQuery }

constructor TxTinyQuery.Create(AOwner: TComponent);
begin
  inherited;
  //UseTemporal := true;
end;

function TxTinyQuery.DataSetByName(const Name: string): TDataSet;
var
  i: integer;
  t: TTinyTable;
begin
  Result := nil;
  if not FDatabase.Connected then FDatabase.Connected := True;
  for i := 0 to FDatabase.TableDefs.Count - 1 do
  begin
    if AnsiCompareText(FDatabase.TableDefs[i].Name, Name) = 0 then
    begin
      t := TTinyTable.Create(nil);
      t.DatabaseName := FDatabase.DatabaseName;
      t.SessionName := FDatabase.SessionName;
      t.TableName := Name;
      t.Open;
      Result := t;
      break;
    end;
  end;
end;

procedure TxTinyQuery.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDatabase) then
    FDatabase := nil;
end;

procedure TxTinyQuery.SetDatabase(const Value: TTinyDatabase);
begin
  FDatabase := Value;
  if Value <> nil then
  begin
    Value.FreeNotification(self);
  end;
end;

end.

