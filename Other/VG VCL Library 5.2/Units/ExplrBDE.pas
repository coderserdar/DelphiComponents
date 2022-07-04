{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Explorer library: BDE objects                 }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit ExplrBDE;

interface
uses Classes, Explorer, DB, DBTables;

type
{ TExplorerBDEItemNode }
  TExplorerBDEItemNode = class(TExplorerFolderNode)
  private
    FRunTime: Boolean;
  public
    function CanExpand(NodeTypes: TExplorerNodeTypes): Boolean; override;
    function IsStored: Boolean; override;
    function IsRunTime: Boolean; override;
    property RunTime: Boolean read FRunTime write FRunTime;
  end;

{ TExplorerSessionListNode }
  TExplorerSessionListNode = class(TExplorerBDEItemNode)
  protected
    procedure InternalExpand; override;
  end;

{ TExplorerSessionNode }
  TExplorerSessionNode = class(TExplorerBDEItemNode)
  private
    FSessionName: string;
    FConfigMode: TConfigMode;
    procedure SetSessionName(Value: string);
    procedure SetConfigMode(Value: TConfigMode);
  protected
    procedure InternalExpand; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property SessionName: string read FSessionName write SetSessionName;
    property ConfigMode: TConfigMode read FConfigMode write SetConfigMode default {$IFNDEF _D3_}cmPersistent{$ELSE}[cfmPersistent]{$ENDIF};
  end;

{ TExplorerDatabaseNode }
  TExplorerDatabaseNode = class(TExplorerBDEItemNode)
  private
    FDatabaseName: string;
    FSessionName: string;
    procedure SetDatabaseName(Value: string);
    procedure SetSessionName(Value: string);
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property SessionName: string read FSessionName write SetSessionName;
  end;

{ TExplorerDatabaseItemNode }
  TExplorerDatabaseItemNode = class(TExplorerBDEItemNode)
  private
    function GetDatabaseName: string;
    function GetSessionName: string;
  public
    property DatabaseName: string read GetDatabaseName;
    property SessionName: string read GetSessionName;
  end;

{ TExplorerTablesNode }
  TExplorerTablesNode = class(TExplorerDatabaseItemNode)
  private
    FSystemTables: Boolean;
  protected
    procedure InternalExpand; override;
  published
    property SystemTables: Boolean read FSystemTables;
  end;

{ TExplorerTableNode }
  TExplorerTableNode = class(TExplorerDatabaseItemNode)
  private
    FTableName: string;
  public
    property TableName: string read FTableName;
  end;

{ TExplorerProceduresNode }
  TExplorerProceduresNode = class(TExplorerDatabaseItemNode)
  protected
    procedure InternalExpand; override;
  end;

{ TExplorerProcedureNode }
  TExplorerProcedureNode = class(TExplorerDatabaseItemNode)
  private
    FStoredProcName: string;
  public
    property StoredProcName: string read FStoredProcName;
  end;

implementation
uses Controls, vgVCLUtl, vgBDEUtl;

{ TExplorerBDEItemNode }
function TExplorerBDEItemNode.CanExpand(NodeTypes: TExplorerNodeTypes): Boolean;
begin
  Result := ntFolder in NodeTypes;
end;

function TExplorerBDEItemNode.IsStored: Boolean;
begin
  Result := not FRunTime;
end;

function TExplorerBDEItemNode.IsRunTime: Boolean;
begin
  Result := FRunTime;
end;

{ TExplorerSessionListNode }
procedure TExplorerSessionListNode.InternalExpand;
var
  I: Integer;
begin
  for I := 0 to Sessions.Count - 1 do
  begin
    with TExplorerSessionNode.Create(Self) do
    try
      SessionName := Sessions[I].SessionName;
      FRunTime := True;
      Parent := Self;
    except
      Free;
      raise;
    end;
  end;
end;

constructor TExplorerSessionNode.Create(AOwner: TComponent);
begin
  inherited;
  FConfigMode := {$IFNDEF _D3_}cmPersistent{$ELSE}[cfmPersistent]{$ENDIF};
end;

procedure TExplorerSessionNode.InternalExpand;
var
  Node: TExplorerDatabaseNode;
  List: TStrings;
  I: Integer;
begin
  AppSetCursor(crSQLWait);
  try
    List := TStringList.Create;
    try
      TStringList(List).Sorted := True;
      GetAliasNames(FSessionName, ConfigMode, List);
      for I := 0 to List.Count - 1 do
      begin
        Node := TExplorerDatabaseNode.Create(Self);
        try
          Node.DatabaseName := List[I];
          Node.SessionName := FSessionName;
          Node.FRunTime := True;
          Node.Text := Node.DatabaseName;
          Node.Parent := Self;
        except
          Node.Free;
          raise;
        end;
      end;
    finally
      List.Free;
    end;
  finally
    AppRestoreCursor;
  end;
end;

procedure TExplorerSessionNode.SetConfigMode(Value: TConfigMode);
begin
  if (FConfigMode <> Value) then
  begin
    FConfigMode := Value;
    Clear;
  end;
end;

procedure TExplorerSessionNode.SetSessionName(Value: string);
begin
  if (FSessionName <> Value) then
  begin
    FSessionName := Value;
    Text := Value;
    Clear;
  end;
end;

{ TExplorerDatabaseNode }
procedure TExplorerDatabaseNode.SetDatabaseName(Value: string);
begin
  if (FDatabaseName <> Value) then
  begin
    FDatabaseName := Value;
    Clear;
  end;
end;

procedure TExplorerDatabaseNode.SetSessionName(Value: string);
begin
  if (FSessionName <> Value) then
  begin
    FSessionName := Value;
    Clear;
  end;
end;

{ TExplorerDatabaseItemNode }
function TExplorerDatabaseItemNode.GetDatabaseName: string;
begin
  Result := (Parent as TExplorerDatabaseNode).DatabaseName;
end;

function TExplorerDatabaseItemNode.GetSessionName: string;
begin
  Result := (Parent as TExplorerDatabaseNode).SessionName;
end;

{ TExplorerTablesNode }
procedure TExplorerTablesNode.InternalExpand;
var
  I: Integer;
  List: TStrings;
  Table: TExplorerTableNode;
  Session: TSession;
begin
  AppSetCursor(crSQLWait);
  try
    BeginExpand;
    try
      Clear;
      if Assigned(Parent) then
      begin
        Session := Sessions.FindSession(SessionName);
        List := TStringList.Create;
        try
          TStringList(List).Sorted := True;
          Session.GetTableNames(DatabaseName, '*.*', False, SystemTables, List);
          for I := 0 to List.Count - 1 do
          begin
            Table := TExplorerTableNode.Create(Self);
            try
              Table.FTableName := List[I];
              Table.FRunTime := True;
              Table.Text := List[I];
              Table.Parent := Self;
            except
              Table.Free;
              raise;
            end;
          end;
        finally
          List.Free;
        end;
      end;
    finally
      EndExpand;
    end;
  finally
    AppRestoreCursor;
  end;
end;

{ TExplorerProceduresNode }
procedure TExplorerProceduresNode.InternalExpand;
var
  I: Integer;
  List: TStrings;
  Proc: TExplorerProcedureNode;
  Session: TSession;
begin
  AppSetCursor(crSQLWait);
  try
    BeginExpand;
    try
      Clear;
      if Assigned(Parent) then
      begin
        Session := Sessions.FindSession(SessionName);
        List := TStringList.Create;
        try
          TStringList(List).Sorted := True;
          Session.GetStoredProcNames(DatabaseName, List);
          for I := 0 to List.Count - 1 do
          begin
            Proc := TExplorerProcedureNode.Create(Self);
            try
              Proc.FStoredProcName := List[I];
              Proc.FRunTime := True;
              Proc.Text := List[I];
              Proc.Parent := Self;
            except
              Proc.Free;
              raise;
            end;
          end;
        finally
          List.Free;
        end;
      end;
    finally
      EndExpand;
    end;
  finally
    AppRestoreCursor;
  end;
end;

end.



















