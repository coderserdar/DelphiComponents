unit ExtBackup;
(*-----------------------------------------------------------------------------
  DESCRIPTION

  This component is freeware. You may use it, distribute it and modify it, but
  you may not charge for it.
  In case of modifications you must mail me (mmm@imaginet.fr) a copy of
  the modifications. The reason are simple: Any changes that improve this free-
  ware component should be to benefit for everybody, not only you. That way you
  can be pretty sure, that this component has few errors and much functionality.
  In case of modifications, you will be on the credits list beneath:

  HISTORY

  Version 1.0 (02/06/98) by Morgan Martinet (France):

  TExtBackup is derived from TMyBackup and can close all active datasets,
  in order to backup them. I derived the component especially, because
  I need to use the DB... units and some of you may not want it, if they
  don't use the BDE in their applications !

  properties:
  -----------
    AutoCloseDatasets     if True, then close the datasets before the Backup/Restore
    AutoReopenDatasets    if True, then reopen the datasets after the Backup/Restore
    ClosedDatasetCount    number of datasets closed
    ClosedDatasets        list of closed datasets
    ASession              if empty, then we'll use the default session object,
                          or you can specify your session.

  methods:
  --------
    ClearClosedDatasetsList   clears the list of the closed datasets
    CloseSession              closes all Databases of the Session
    CloseDatabase             closes all Datasets of the Database
    CloseDataset              closes a Dataset and adds it to the list
    ReopenDatasets            reopen all datasets in the list
    GetAliasPath              finds the path associated with an alias

  Notes:
  -----
    You can use GetAliasPath if you want to find the pathname associated with an alias
    and store it in the property FilesPath.

    If you want to close all you tables, then use AutoCloseDatasets and AutoReopenDatasets.
    But if it's more complex (you have several databases, but you want to close only one,
    or you want to close some datasets only), then use the procedures ClearClosedDatasetsList,
    CloseDatabase or CloseDatasets before a Backup/Restore. Then use ReopenDatasets.

    This will work only if there's only one user connected to the datasets !
    Don't try to backup your tables if you run your application from Delphi,
    because Delphi has already opened your tables.
    You may use the function IsDirectoryInUse( aDirectory ), in order to check
    if the tables located in your directory are currently in use or not.

    When using a TArchiver (property UseArchiver, or using the Component alone),
    it seems that it can archive the opened tables without error.
    But I can't assume any warranty, because the BDE handles a cache, and I
    suppose that your archive wouldn't be up to date, unless you call a function
    to flush the cache.
*)

interface
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  CopyFile, myBackup, DB, DBTables;

type
  TExtBackup = class( TMyBackup )
  protected
    FClosedDatasets : TList;
    FAutoCloseDatasets : Boolean;
    FAutoReopenDatasets : Boolean;
    FSession : TSession;

    function GetClosedDatasetCount : Integer;
    function GetClosedDatasets( idx : Integer ) : TDataset;

  public
    constructor Create( AOwner : TComponent ); override;
    destructor  Destroy; override;

    procedure Start; override;
    procedure Finish; override;

    // Handle active datasets
    procedure ClearClosedDatasetsList;
    procedure CloseSession( sess : TSession );
    procedure CloseDatabase( aDatabase : TDatabase );
    procedure CloseDataset( aDataset : TDataset );
    procedure ReopenDatasets;
    function  GetAliasPath( const alias : String ) : String;

    // Public properties
    property ClosedDatasetCount : Integer read GetClosedDatasetCount;
    property ClosedDatasets[ idx : Integer ] : TDataset read GetClosedDatasets;

  published
    property AutoCloseDatasets : Boolean read FAutoCloseDatasets write FAutoCloseDatasets;
    property AutoReopenDatasets : Boolean read FAutoReopenDatasets write FAutoReopenDatasets;
    property ASession : TSession read FSession write FSession;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Backup Tools', [TExtBackup]);
end;

constructor TExtBackup.Create( AOwner : TComponent );
begin
  inherited;
  FAutoCloseDatasets  := True;
  FAutoReopenDatasets := True;
  FClosedDatasets     := TList.Create;
end;

destructor  TExtBackup.Destroy;
begin
  FClosedDatasets.Free;
  inherited;
end;

procedure TExtBackup.Start;
begin
  inherited;
  if (FStartCount = 1) and AutoCloseDatasets then
    begin
      ClearClosedDatasetsList;
      if Assigned(FSession) then
        CloseSession( FSession )
      else
        CloseSession( Session );
    end;
end;

procedure TExtBackup.Finish;
begin
  if (FStartCount = 1) and AutoReopenDatasets then
    ReopenDatasets;
  inherited;
end;

function TExtBackup.GetClosedDatasetCount : Integer;
begin
  Result := FClosedDatasets.Count;
end;

function TExtBackup.GetClosedDatasets( idx : Integer ) : TDataset;
begin
  Result := TDataset(FClosedDatasets.Items[idx]);
end;

procedure TExtBackup.ClearClosedDatasetsList;
begin
  FClosedDatasets.Clear;
end;

procedure TExtBackup.CloseSession( sess : TSession );
var
  i : Integer;
begin
  for i := sess.DatabaseCount - 1 downto 0 do
    CloseDatabase( sess.Databases[i] );
end;

procedure TExtBackup.CloseDatabase( aDatabase : TDatabase );
var
  i : Integer;
begin
  for i := aDatabase.DatasetCount - 1 downto 0 do
    CloseDataset( aDatabase.Datasets[i] );
end;

procedure TExtBackup.CloseDataset( aDataset : TDataset );
begin
  if aDataset.Active then
    begin
      FClosedDatasets.Add( aDataset );
      aDataset.Close;
    end;
end;

procedure TExtBackup.ReopenDatasets;
var
  i : Integer;
  ds : TDataset;
begin
  WriteFileName( '' );
  SetCaption( Messages.ReopeningTables );
  for i := ClosedDatasetCount - 1 downto 0 do
    begin
      SetProgress( 100 - ( (i * 100) div (ClosedDatasetCount - 1) ) );
      ds := ClosedDatasets[i];
      if ds is TDBDataset then
        with TDBDataset(ds) do
          if Assigned(Database) and (Database.Connected = False) then
             Database.Open;
      ds.Open;
    end;
end;

function TExtBackup.GetAliasPath( const alias : String ) : String;
var
  L : TStringList;
  sess : TSession;
begin
  L := TStringList.Create;
  try
    if Assigned(FSession) then
      sess := FSession
    else
      sess := Session;
    sess.GetAliasParams( Alias, L );
    Result := L.Values['PATH'];
  finally
    L.Free;
  end;
end;

end.
