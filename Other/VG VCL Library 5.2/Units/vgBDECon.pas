{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         TBDEConverter, TBDEConvertItem                }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgBDECon;

interface
uses SysUtils, Classes, vgTools, DB, DBTables, vgDBConv;

type
  TBDEConverter = class;
  TConverterItemNotifyEvent = procedure (Converter: TBDEConverter; Item: TDBConvertItem) of object;

{ TBDEConvertItem }
  TBDEConvertItem = class(TDBConvertItem)
  public
    procedure DefaultInitDataSet(TableMode: TTableMode; TableName: String;
      TableDataSet: TDataSet; Dest: Boolean; var DataSet: TDataSet); override;
  end;

{ TBDEConverter }

  TBDEConverter = class(TDBConverter)
  private
    FDatabases: array [0..1] of TDatabase;
    procedure SetDatabase(Index: Integer; Value: TDatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DatabaseSource: TDatabase index 0 read FDatabases[0] write SetDatabase;
    property DatabaseDest: TDatabase index 1 read FDatabases[1] write SetDatabase;
  end;

implementation
uses vgBDEUtl;

const
  Registered: Boolean = False;

{ TBDEConvertItem }
procedure TBDEConvertItem.DefaultInitDataSet(TableMode: TTableMode; TableName: String;
  TableDataSet: TDataSet; Dest: Boolean; var DataSet: TDataSet);
begin
  if not Assigned(DataSet) then
  begin
    DataSet := TTable.Create(Self);
    try
      TTable(DataSet).TableName := TableName;
      if Dest then
        ChangeToDatabase(TDBDataSet(DataSet), (Converter as TBDEConverter).DatabaseDest) else
        ChangeToDatabase(TDBDataSet(DataSet), (Converter as TBDEConverter).DatabaseSource);
     TTable(DataSet).UpdateMode := upWhereKeyOnly;
    except
      TTable(DataSet).Free;
      raise;
    end;
  end;
end;

{ TBDEConverter }

constructor TBDEConverter.Create(AOwner: TComponent);
begin
  inherited;
  if not Registered then
  begin
    RegisterClasses([TBDEConvertItem]);
    Registered := True;
  end;
end;

destructor TBDEConverter.Destroy;
begin
  SetDatabase(0, nil);
  SetDatabase(1, nil);
  inherited;
end;

procedure TBDEConverter.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if FDatabases[0] = AComponent then SetDatabase(0, nil) else
    if FDatabases[1] = AComponent then SetDatabase(1, nil);
  end;
end;

procedure TBDEConverter.SetDatabase(Index: Integer; Value: TDatabase);
begin
  if (Value <> FDatabases[Index]) then
  begin
    if Assigned(Value) then FreeNotification(Value);
    FDatabases[Index] := Value;
  end;
end;

end.
