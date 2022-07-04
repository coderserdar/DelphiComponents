{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Report components for MS Word: TDataSet       }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}
{$I VG.INC }
{$D-,L-}

unit vgWPBDE;

interface
uses Classes, vgWP, vgWPDB, DB, DBTables;

type
  TUpdateParamEvent = procedure(Sender: TObject; Group: TWordBookmark;
    DataSet: TDataSet; Param: TParam) of object;

{ TvgBDEWordPrint }
  TvgBDEWordPrint = class(TvgDBWordPrint)
  private
    FDatabase: TDatabase;
    FOnUpdateParam: TUpdateParamEvent;
    procedure SetDatabase(Value: TDatabase);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DefaultCreateDataSet(Group: TWordBookmark; var DataSet: TDataSet); override;
    procedure DefaultUpdateParam(Group: TWordBookmark; DataSet: TDataSet;
      Param: TParam); virtual;
    procedure DefaultUpdateParams(Group: TWordBookmark; DataSet: TDataSet); override;
  published
    property Database: TDatabase read FDatabase write SetDatabase;
    property OnUpdateParam: TUpdateParamEvent read FOnUpdateParam write FOnUpdateParam;
  end;

implementation
uses SysUtils;

{ TvgBDEWordPrint }
constructor TvgBDEWordPrint.Create(AOwner: TComponent);
begin
  inherited;
  DataSetClass := TQuery;
end;

procedure TvgBDEWordPrint.DefaultCreateDataSet(Group: TWordBookmark; var DataSet: TDataSet);
begin
  inherited DefaultCreateDataSet(Group, DataSet);
  try
    if (DataSet is TDBDataSet) and Assigned(FDatabase) then
    with TDBDataSet(DataSet) do
    begin
      SessionName := FDatabase.SessionName;
      DatabaseName := FDatabase.DatabaseName;
    end;
    if (DataSet is TQuery) then
      TQuery(DataSet).SQL.Text := Group.BookmarkSQL.SQL;
  except
    DataSet.Free;
    raise;
  end;
end;

procedure TvgBDEWordPrint.DefaultUpdateParam(Group: TWordBookmark; DataSet: TDataSet; Param: TParam);
var
  MasterField: TField;
begin
  MasterField := FindMasterField(Param.Name, Group);
  if Assigned(MasterField) then 
    Param.AssignFieldValue(MasterField, MasterField.Value);
end;

procedure TvgBDEWordPrint.DefaultUpdateParams(Group: TWordBookmark; DataSet: TDataSet);
var
  I: Integer;
  Param: TParam;
begin
  if (DataSet is TQuery) then
  with TQuery(DataSet) do
  begin
    for I := 0 to ParamCount - 1 do
    begin
      Param := Params[I];
      if Assigned(FOnUpdateParam) then
        FOnUpdateParam(Self, Group, DataSet, Param) else
        DefaultUpdateParam(Group, DataSet, Param);
    end;
  end;
end;

procedure TvgBDEWordPrint.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FDatabase) then SetDataBase(nil);
end;

procedure TvgBDEWordPrint.SetDatabase(Value: TDatabase);
begin
  if (FDatabase <> Value) then
  begin
    if Assigned(Value) then FreeNotification(Value);
    FDatabase := Value;
  end;
end;

end.
