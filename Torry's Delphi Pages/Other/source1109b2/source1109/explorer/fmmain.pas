{$I fsdefine.inc}

Unit fmmain;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Db,
  Menus,
  Stdctrls,
  DBGrids,
  DBCtrls,
  Grids,
  Outline,
  ExtCtrls,
  ComCtrls,
  fsdbbase,
  fsllbase,
  fsllprot,
  fssrbde,
  uconfig,
  uentity,
  fslllgcy,
  fsdb,
  fschangeport,
  fsfunInterp,
  fslllog
  {$IFDEF DCC4ORLATER}
  ,
  ImgList,
  ToolWin
  {$ENDIF}
  ;

Type
  TfrmMain = Class(TForm)
    mnuMain: TMainMenu;
    mnuServer: TMenuItem;
    N1: TMenuItem;
    mnuServerExit: TMenuItem;
    popmnuServer: TPopupMenu;
    popmnuServerAttach: TMenuItem;
    popmnuServerDetach: TMenuItem;
    popmnuAlias: TPopupMenu;
    popmnuTable: TPopupMenu;
    popmnuTableDefinition: TMenuItem;
    popmnuTableRedefine: TMenuItem;
    popmnuTableDelete: TMenuItem;
    popmnuTableRename: TMenuItem;
    popmnuTableNew: TMenuItem;
    popmnuDatabaseNew: TMenuItem;
    popmnuDatabaseDelete: TMenuItem;
    N3: TMenuItem;
    popmnuDatabaseRefresh: TMenuItem;
    popmnuServerNewDatabase: TMenuItem;
    N5: TMenuItem;
    popmnuDatabaseNewTable: TMenuItem;
    N6: TMenuItem;
    popmnuServerRefresh: TMenuItem;
    N7: TMenuItem;
    mnuOptions: TMenuItem;
    popmnuTableReindex: TMenuItem;
    mnuOptionsPrintSetup: TMenuItem;
    popmnuTableImportSchema: TMenuItem;
    mnuToolsFFComms: TMenuItem;
    popmnuDatabaseRename: TMenuItem;
    popmnuDatabaseImportSchema: TMenuItem;
    dlgPrinterSetup: TPrinterSetupDialog;
    mnuServerRegister: TMenuItem;
    popmnuServerRegister: TMenuItem;
    popmnuTableEmpty: TMenuItem;
    pnlLeft: TPanel;
    pnlLeftHeader: TPanel;
    lblFlashFilerServers: TLabel;
    mnuSetAutoInc: TMenuItem;
    mnuOptionsLiveDatasets: TMenuItem;
    logMain: TfsEventLog;
    outServers: TTreeView;
    imgMain: TImageList;
    mnuDatabaseSQL: TMenuItem;
    mnuViewTable: TMenuItem;
    N4: TMenuItem;
    mnuCloseAll: TMenuItem;
    popmnuTableSQL: TMenuItem;
    mnuSetAsAutomaticDefault: TMenuItem;
    N9: TMenuItem;
    mnuTools: TMenuItem;
    popmnuTableReindexAll: TMenuItem;
    popmnuServerStatistics: TMenuItem;
    mnuOptionsSetDefaultTimeout: TMenuItem;
    N14: TMenuItem;
    Addtrigertable1: TMenuItem;
    Addconstrainttable1: TMenuItem;
    Addprocedurestable1: TMenuItem;
    Changeclientport1: TMenuItem;
    Loadservers1: TMenuItem;
    bb1: TMenuItem;
    N8: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    Tableflags1: TMenuItem;
    N15: TMenuItem;
    A1: TMenuItem;
    N16: TMenuItem;
    S1: TMenuItem;
    C1: TMenuItem;
    R1: TMenuItem;
    N2: TMenuItem;
    S2: TMenuItem;
    S3: TMenuItem;
    Passwords1: TMenuItem;
    Setpasswordrestructure1: TMenuItem;
    OptimisticNoWait1: TMenuItem;
    OptimisticWait1: TMenuItem;
    PessimisticNoWait1: TMenuItem;
    PessimisticWait1: TMenuItem;
    N10: TMenuItem;
    Databaserecordlocking1: TMenuItem;
    N17: TMenuItem;
    PackReindexUndeleteAll1: TMenuItem;
    Recoveryonlydeletedrecords1: TMenuItem;
    N18: TMenuItem;
    About1: TMenuItem;
    mnuWindows: TMenuItem;
    mnuWindowsSplitter: TMenuItem;

    Procedure FormCreate(Sender: TObject);
    Procedure mnuServerExitClick(Sender: TObject);
    Procedure outServersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FormDestroy(Sender: TObject);
    Procedure mnuServerRefreshClick(Sender: TObject);
    Procedure popmnuTableDefinitionClick(Sender: TObject);
    Procedure popmnuTableNewClick(Sender: TObject);
    Procedure popmnuDatabaseNewTableClick(Sender: TObject);
    Procedure popmnuServerPopup(Sender: TObject);
    Procedure popmnuServerDetachClick(Sender: TObject);
    Procedure popmnuServerAttachClick(Sender: TObject);
    Procedure popmnuTableDeleteClick(Sender: TObject);
    Procedure popmnuTablePackClick(Sender: TObject);
    Procedure popmnuTableRedefineClick(Sender: TObject);
    Procedure popmnuTablePopup(Sender: TObject);
    Procedure outServersClick(Sender: TObject);
    Procedure ExitBtnClick(Sender: TObject);
    Procedure popmnuServerNewDatabaseClick(Sender: TObject);
    Procedure popmnuTableReindexClick(Sender: TObject);
    Procedure popmnuTableImportSchemaClick(Sender: TObject);
    Procedure popmnuDatabaseImportSchemaClick(Sender: TObject);
    Procedure popmnuDatabaseDeleteClick(Sender: TObject);
    Procedure mnuOptionsPrintSetupClick(Sender: TObject);
    Procedure popmnuDatabaseRenameClick(Sender: TObject);
    Procedure mnuServerRegisterClick(Sender: TObject);
    Procedure popmnuTableEmptyClick(Sender: TObject);
    Procedure mnuSetAutoIncClick(Sender: TObject);
    Procedure outServersDblClick(Sender: TObject);
    Procedure mnuOptionsLiveDatasetsClick(Sender: TObject);
    Procedure outServersExpanding(Sender: TObject; Node: TTreeNode;
      Var AllowExpansion: Boolean);
    Procedure outServersEditing(Sender: TObject; Node: TTreeNode;
      Var AllowEdit: Boolean);
    Procedure outServersEdited(Sender: TObject; Node: TTreeNode;
      Var S: String);
    Procedure outServersKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);

    Procedure RefreshDatabases(Sender: TObject);
    { Refresh a servers' list of databases. }

    Procedure RefreshTables(Sender: TObject);
    Procedure mnuDatabaseSQLClick(Sender: TObject);
    Procedure mnuViewTableClick(Sender: TObject);
    Procedure outServersCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; Var Compare: Integer);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure mnuCloseAllClick(Sender: TObject);
    Procedure mnuWindowsClick(Sender: TObject);
    Procedure mnuToolsFFCommsClick(Sender: TObject);
    Procedure mnuSetAsAutomaticDefaultClick(Sender: TObject);
    Procedure outServersChange(Sender: TObject; Node: TTreeNode);
    Procedure outServersContextPopup(Sender: TObject; MousePos: TPoint;
      Var Handled: Boolean);
    Procedure popmnuServerRefreshClick(Sender: TObject);
    Procedure popmnuServerStatisticsClick(Sender: TObject);
    Procedure mnuOptionsSetDefaultTimeoutClick(Sender: TObject);
    Procedure Addtrigertable1Click(Sender: TObject);
    Procedure Addconstrainttable1Click(Sender: TObject);
    Procedure Addprocedurestable1Click(Sender: TObject);
    Procedure Changeclientport1Click(Sender: TObject);
    Procedure Loadservers1Click(Sender: TObject);
    Procedure bb1Click(Sender: TObject);
    Procedure Tableflags1Click(Sender: TObject);
    Procedure S1Click(Sender: TObject);
    Procedure C1Click(Sender: TObject);
    Procedure R1Click(Sender: TObject);
    Procedure S2Click(Sender: TObject);
    Procedure S3Click(Sender: TObject);
    Procedure Passwords1Click(Sender: TObject);
    Procedure Setpasswordrestructure1Click(Sender: TObject);
    Procedure OptimisticNoWait1Click(Sender: TObject);
    Procedure PackReindexUndeleteAll1Click(Sender: TObject);
    Procedure Recoveryonlydeletedrecords1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    { Refresh a database's list of tables. }

  Private
    //    function mapProtocolClassToProtocol(const Protocol : TffCommsProtocolClass) : TffProtocolType;
    Procedure WindowsMenuItemClick(Sender: TObject); {!!.06}
    Procedure AppMessage(Var Msg: TMsg; Var Handled: Boolean); {!!.06}
    Procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    Procedure ShowServerStatistics(aServer: TffeServerItem); {!!.06}
  Protected
    Initialized: Boolean;
    {- True if the (DB) Session has been started }

    Function GetNewSelectedNode(aNode: TTreeNode): TTreeNode;
    {- Assuming aNode is going to be deleted, determines which node should be
       selected after the deletion. }

  Public
    Procedure LockDatabase(aDatabase: TfsDatabase);
    Function AddOutlineDatabase(aNode: TTreeNode;
      aDatabase: TffeDatabaseItem): TTreeNode;
    {- Adds a database entry to the outline.  Returns outline index of new entry}
    Procedure AddOutlineServer(aServer: TffeServerItem);
    {- Adds a server entry to the outline.  Returns outline index of new entry}
    Procedure AddOutlineTable(aNode: TTreeNode; aTable: TffeTableItem);
    {- Adds a table entry to the outline.  Returns outline index of new entry}
    Procedure DeleteNodeChildren(aNode: TTreeNode);
    {- Deletes all the children for a given outline entry }
    Function DoAttach(aNode: TTreeNode): TffResult; {!!.01}
    {- Attach to the given server }
    Procedure DoDetach;
    {- Detach from given server }
    Procedure EnableScreen(aSwitch: Boolean);
    {- Enables/Disables the main screen controls while a process runs; allows
       main form to be minimized}
    Function GetEntityNode(aEntityType: TffeEntityType;
      anEntity: TffeEntityItem): TTreeNode;
    {- Returns the node for the given entity }
    Function GetNodeEntity(aNode: TTreeNode): TffeEntityItem;
    {- Returns the entity associated with a given node. }
    Function GetNodeEntityType(aNode: TTreeNode): TffeEntityType;
    {- Returns the entity type associated with a given node. }
    Function GetSelectedEntity: TffeEntityItem;
    {- Returns the entity for the currently selected outline item. }
    Procedure Initialize;
    {- Initial setup of the session and the server list }
    Procedure LoadConfig;
    {- Read parameters out of persistent configuration storage }
    Procedure LoadOutlineServers;
    {- Refreshes the entire outline view }
    Procedure LoadOutlineDatabases(aNode: TTreeNode);
    {- Refreshes the outline view (databases, tables) for the given server}
    Procedure LoadOutlineTables(aNode: TTreeNode);
    {- For a given database entry in the outline, load all of its member
       tables into the outline.  aNode may point to a table or
       a database entry in the outline. }
    Procedure OutlineClear;
    {- Frees the TffeOutlineData instances attached to the TTreeNodes in
       outServers.  Clears the TTreeView. }
    Procedure SaveConfig;
    {- Writes the FFE configuration settings to persistent storage}
    Procedure ShowQueryWindow(aDatabaseIndex: Longint);
    {- Creates a modeless query window for a particular database. }
    Procedure ShowTableBrowser(aTable: TffeTableItem);
    {- Creates a modeless table browser for a particular table. }
    Procedure UncheckMenuGroupSiblings(aMenuItem: TMenuItem);
    {- Unchecks all the menu items in the same menu group as the given item
       (primary for compatibility with Delphi 1) }
    Procedure UpdateWindowsMenuItems; {!!.06}
    {- populates the Windows menu with the current
       table- and SQL-browser windows }
  End;

Var
  frmMain: TfrmMain;

Implementation

Uses
  {$IFDEF USETeDEBUG}
  jcldebug,
  {$ENDIF}
  fsclbase, {!!.07}
  fsllcomm, {!!.07}
  fsserverremoteclass,
  fsclcfg, {!!.07}
  fsutil,
  ufscomms, {!!.07}
  {$IFDEF DCC6OrLater}
  Types, {!!.07}
  {$ENDIF}
  ubase,
  uconsts,
  dgaddals,
  dgimport,
  dgregsrv,
  dgselidx,
  fslldict,
  fsllexcp, {!!.01}
  fmprog,
  fmstruct,
  dgautoin,
  fsrestriction,
  dgmrec,
  dgtable,
  fspasswd1,
  fspasswd,
  dgtabletrigger,
  dgtableconstraints,
  dgtableprocedures,
  dgquery,
  fssrbase,
  dgServSt,
  About;

{$R *.DFM}

Const
  { Outline levels for schema entities }
  lvServer = 1;
  lvDatabase = 2;
  lvTable = 3;

  {===TffeOutlineData==================================================}
Type
  { This is the data kept by each outline entry to refer it to
    the underlying data structure. }
  TffeOutlineData = Class
  Public
    EntityType: TffeEntityType;
    Entity: TffeEntityItem;
    Constructor Create(aEntityType: TffeEntityType; anEntity: TffeEntityItem);
  End;

Constructor TffeOutlineData.Create(aEntityType: TffeEntityType;
  anEntity: TffeEntityItem);
Begin
  Inherited Create;
  EntityType := aEntityType;
  Entity := anEntity;
End;
{====================================================================}

{===TfrmMain=========================================================}

Procedure TfrmMain.LockDatabase(aDatabase: TfsDatabase);
Begin
  //tlOptimisticNoWait, tlOptimisticWait,tlPessimisticNoWait, tlPessimisticWait
  If OptimisticNoWait1.Checked Then aDatabase.RecLocking := tlOptimisticNoWait;
  If OptimisticWait1.Checked Then aDatabase.RecLocking := tlOptimisticWait;
  If PessimisticNoWait1.Checked Then aDatabase.RecLocking := tlPessimisticNoWait;
  If PessimisticWait1.Checked Then aDatabase.RecLocking := tlPessimisticWait;
End;

Function TfrmMain.AddOutlineDatabase(aNode: TTreeNode;
  aDatabase: TffeDatabaseItem): TTreeNode;
Var
  OutlineData: TffeOutlineData;
Begin
  Result := Nil;
  OutlineData := TffeOutlineData.Create(etDatabase, aDatabase);
  With outServers Do
    With TffeOutlineData(aNode.Data) Do
      Case EntityType Of
        etServer:
          Result := Items.AddChildObject(aNode, aDatabase.DataBaseName,
            OutlineData);
        etDatabase:
          Result := Items.AddObject(aNode, aDatabase.DataBaseName,
            OutlineData);
      End;
  If assigned(Result) Then
    Begin
      Result.ImageIndex := pred(lvDatabase);
      Result.SelectedIndex := Result.ImageIndex;
      Result.HasChildren := True;
    End;
  outServers.AlphaSort;
End;
{--------}

Procedure TfrmMain.AddOutlineServer(aServer: TffeServerItem);
Var
  Node: TTreeNode;
  OutlineData: TffeOutlineData;
  aProtocol: TfsCommsProtocolClass;
  aProtocolName: TffShStr;

  {Begin !!.07}
  { removes leading zeroes in order to compare ip addresses
    like 192.000.001.001 against 192.0.1.1 - necessary because
    FFCOMMS might register addresses with extra 0's }

  Function StripLeadingZeros(servername: String): String;
  Var
    s: String;
  Begin
    Result := '';
    { while characters in string do }
    While (Length(servername) > 0) Do
      Begin
        { if first char not a number}
        If Not (servername[1] In ['0'..'9']) Then
          Begin
            { move char to result }
            Result := Result + servername[1];
            Delete(servername, 1, 1);
          End
        Else
          Begin
            s := '';
            { collect numbers up to next non-numerical char }
            While (Length(servername) > 0) And (servername[1] In ['0'..'9']) Do
              Begin
                s := s + servername[1];
                Delete(servername, 1, 1);
              End;
            { strip leading zeroes and add to Result }
            Result := Result + IntToStr(StrToInt(s));
          End;
      End;
  End;
  {End !!.07}

Begin
  OutlineData := TffeOutlineData.Create(etServer, aServer);
  With outServers Do
    Node := Items.AddObject(outServers.TopItem, aServer.ServerName, OutlineData);
  If assigned(Node) Then
    Begin
      {Begin !!.07}
      { check if the server is the default for the workstation
        and use a different glyph if so }
      FFClientConfigReadProtocol(aProtocol, aProtocolName);
      If (FsGetProtocolString(aServer.Protocol) = aProtocolName) And
        ((aServer.Protocol = ptSingleUser) Or
        (StripLeadingZeros(FFClientConfigReadServerName) = StripLeadingZeros(aServer.ServerName))) Then
        Begin
          Node.ImageIndex := 12;
        End
      Else
        {End !!.07}
        Node.ImageIndex := pred(lvServer);
      Node.SelectedIndex := Node.ImageIndex;
      Node.HasChildren := True;
    End;
  outServers.AlphaSort;
End;
{--------}

Procedure TfrmMain.AddOutlineTable(aNode: TTreeNode; aTable: TffeTableItem);
Var
  Node: TTreeNode;
  OutlineData: TffeOutlineData;
Begin
  Node := Nil;
  OutlineData := TffeOutlineData.Create(etTable, aTable);
  With outServers Do
    With TffeOutlineData(aNode.Data) Do
      Case EntityType Of
        etDatabase:
          Node := Items.AddChildObject(aNode, aTable.TableName, OutlineData);
        etTable:
          Node := Items.AddObject(aNode, aTable.TableName, OutlineData);
      End;
  If assigned(Node) Then
    Begin
      Node.ImageIndex := pred(lvTable);
      Node.SelectedIndex := Node.ImageIndex;
      Node.HasChildren := False;
    End;
  outServers.AlphaSort;
End;
{--------}

Procedure TfrmMain.DeleteNodeChildren(aNode: TTreeNode);
Var
  aChild: TTreeNode;
Begin
  With outServers Do
    Begin
      Items.BeginUpdate;
      Try
        With aNode Do
          Begin
            aChild := GetFirstChild;
            While assigned(aChild) Do
              Begin
                If assigned(aChild.Data) Then
                  Begin
                    DeleteNodeChildren(aChild);
                    TffeOutlineData(aChild.Data).free;
                  End;
                aChild := GetNextChild(aChild);
              End;
          End;
        aNode.DeleteChildren;
      Finally
        Items.EndUpdate;
      End;
    End;
End;
{--------}

Function TfrmMain.DoAttach(aNode: TTreeNode): TffResult; {!!.01}
Var
  aServer: TffeServerItem;
Begin
  aServer := TffeServerItem(TffeOutlineData(aNode.Data).Entity);
  Try
    Result := aServer.Attach(logMain); {!!.01}
    If Result = DBIERR_NONE Then
      Begin {!!.01}
        LoadOutlineDatabases(aNode); {!!.01}
        Config.LastServer := aServer.ServerName; {!!.01}
      End; {!!.01}
  Except
    On E: EfsDatabaseError Do
      Begin {!!.01}
        If E.ErrorCode = 11278 Then
          Raise EfsDatabaseError.CreateFmt('Unable to connect. "%S" is currently unavailable',
            [aServer.EntityName])
        Else
          Raise;
      End; {!!.01}
  End;
End;
{--------}

Procedure TfrmMain.DoDetach;
Var
  aServer: TffeServerItem;
Begin
  aServer := TffeServerItem(GetSelectedEntity);
  If assigned(aServer) Then
    Begin
      outServers.Selected.Collapse(True);
      DeleteNodeChildren(outServers.Selected);
      aServer.Detach;
      outServers.Selected.HasChildren := True;
    End;
End;
{--------}

Procedure TfrmMain.EnableScreen(aSwitch: Boolean);
Begin
  If aSwitch Then
    Application.ProcessMessages;
  mnuServer.Enabled := aSwitch;
  mnuOptions.Enabled := aSwitch;
End;
{--------}

Function TfrmMain.GetEntityNode(aEntityType: TffeEntityType;
  anEntity: TffeEntityItem): TTreeNode;
Var
  I: Longint;
Begin
  Result := Nil;
  With outServers Do
    For I := 0 To pred(Items.Count) Do
      With TffeOutlineData(Items[I].Data) Do
        If (EntityType = aEntityType) And
          (Entity = anEntity) Then
          Begin
            Result := Items[I];
            Break;
          End;
End;
{--------}

Function TfrmMain.GetNodeEntity(aNode: TTreeNode): TffeEntityItem;
Begin
  Result := TffeOutlineData(aNode.Data).Entity;
End;
{--------}

Function TfrmMain.GetNodeEntityType(aNode: TTreeNode): TffeEntityType;
Begin
  Result := TffeOutlineData(aNode.Data).EntityType;
End;
{--------}

Function TfrmMain.GetSelectedEntity: TffeEntityItem;
Begin
  Result := TffeOutlineData(outServers.Selected.Data).Entity;
End;
{--------}

Procedure TfrmMain.Initialize;
Begin
  { Try
     Initialized := False;
     { if not assigned(ServerList) then begin
        ServerList := TffeServerList.Create(logMain);
        ServerList.OnAttach := slServerAttach;
      end; }
     {LoadOutlineServers;
   Except
     On E: Exception Do
       ShowMessage(E.Message);
   End;    }
End;
{--------}

Procedure TfrmMain.LoadConfig;
Begin
  { Set window coordinates }
  WindowState := Config.WindowState;
  If (WindowState <> wsMaximized) And (Config.Window.Bottom <> 0) Then
    With Config Do
      Begin
        Left := Window.Left;
        Top := Window.Top;
        Width := Window.Right - Config.Window.Left;
        Height := Window.Bottom - Config.Window.Top;
      End;
  mnuOptionsLiveDataSets.Checked := coLiveDatasets In Config.Options;
  //  tbOptionsLiveDataSets.Down := mnuOptionsLiveDataSets.Checked; {!!.06}
End;
{--------}

Procedure TfrmMain.OutlineClear;
Var
  Index: Longint;
Begin
  { Free the TffeOutlineData structures associated with the nodes. }
  With outServers Do
    Begin
      For Index := 0 To pred(Items.Count) Do
        If assigned(Items[Index].Data) Then
          TffeOutlineData(Items[Index].Data).Free;
    End;
  outServers.Items.Clear;
End;
{--------}

Procedure TfrmMain.LoadOutlineServers;
Var
  aNode: TTreeNode;
  Server: TffeServerItem;
  S: Longint;
  DefaultServerName: TffNetAddress;
  OldCursor: TCursor;
Begin

  OutlineClear;

  { Load up the registered servers into the outline }
  mnuServer.Enabled := False;
  outServers.Enabled := False;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Try
    If assigned(ServerList) Then
      Begin
        ServerList.free;
        ServerList := Nil;
      End;
    If Not assigned(ServerList) Then
      Begin
        ServerList := TffeServerList.Create(logMain);
        //ServerList.OnAttach := slServerAttach;
      End;

    ServerList.Load;

    { Load up all the servers into the outline }
    For S := 0 To ServerList.Count - 1 Do
      AddOutlineServer(ServerList.Items[S]);

    { Find the default server }
    DefaultServerName := Config.LastServer;
    If DefaultServerName <> '' Then
      Begin
        S := ServerList.IndexOfName(DefaultServerName);
        If S <> -1 Then
          Begin
            Server := ServerList.Items[S];
            aNode := GetEntityNode(etServer, Server);
            {Begin !!.01}
                    { Attached to server? }
           // If DoAttach(aNode) = DBIERR_NONE Then
              { Expand the attached server.  If the server has only one
                database then expand the database too. }
            //  aNode.Expand(Server.DatabaseCount = 1);
            {End !!.01}
          End;
      End;
    outServers.AlphaSort;
  Finally
    Screen.Cursor := OldCursor;
    outServers.Invalidate;
    Screen.Cursor := OldCursor;
    mnuServer.Enabled := True;
    outServers.Enabled := True;
  End;
End;
{--------}

Procedure TfrmMain.LoadOutlineDatabases(aNode: TTreeNode);
{ For a given server entry in the outline, load all of its member
  databases into the outline }
Var
  D: Longint;
  Server: TffeServerItem;
Begin
  Server := TffeServerItem(TffeOutlineData(aNode.Data).Entity);
  If (Not Server.Attached) Then
    If DoAttach(aNode) <> DBIERR_NONE Then {!!.01}
      Exit; {!!.01}
  { Delete all the children of this server }
  DeleteNodeChildren(aNode);

  { Load the databases into the outline; we assume the server's database list &
    table list have already been populated. }
  For D := 0 To pred(Server.DatabaseCount) Do
    AddOutlineDatabase(aNode, Server.Databases[D]);

  outServers.AlphaSort;
End;
{--------}

Procedure TfrmMain.LoadOutlineTables(aNode: TTreeNode);
Var
  Database: TffeDatabaseItem;
  T: Longint;
Begin
  { If we're pointing to a table entry, kick up to the table's
    database entry }
  With TffeOutlineData(aNode.Data) Do
    If EntityType = etTable Then
      Begin
        aNode := aNode.Parent;
        outServers.Selected := aNode;
      End;
  Database := TffeDatabaseItem(TffeOutlineData(aNode.Data).Entity);

  outServers.Items.BeginUpdate;
  Try
    { Delete all the children of this database }
    DeleteNodeChildren(aNode);

    { Load the database's tables. }
    Database.LoadTables;

    { Load the database's tables into the outline }
    For T := 0 To pred(Database.TableCount) Do
      AddOutlineTable(aNode, Database.Tables[T]);
    outServers.AlphaSort;
  Finally
    outServers.Items.EndUpdate;
  End;

End;
{--------}

Procedure TfrmMain.SaveConfig;
Begin
  If Assigned(Config) Then
    Begin
      With Config Do
        Begin
          Window := Bounds(Left, Top, Width, Height);
          Options := [];
        End;
      Config.WindowState := WindowState;
      Config.Options := [];
      If mnuOptionsLiveDataSets.Checked Then
        Config.Options := [coLiveDataSets];

      Config.Save;
    End;
End;
{--------}

Procedure TfrmMain.ShowQueryWindow(aDatabaseIndex: Longint);
Var
  dummy: Boolean;
  Database: TffeDatabaseItem;
Begin
  { make sure tablelist is loaded; implicitly checks for valid directory }
  outServersExpanding(outServers, outServers.Selected, dummy); {!!.06}
  Database := TffeDatabaseItem(GetSelectedEntity);
  { implicitly check valid directory }
  outServersExpanding(outServers, outServers.Selected, dummy); {!!.07}
  With TdlgQuery.Create(Nil) Do
    Begin
      {Begin !!.07}
      { If we're pointing to a table entry, get the table's
        database entry from the parent }
      If TffeOutlineData(outServers.Selected.Data).EntityType = etTable Then
        Begin
          DatabaseItem := TffeDatabaseItem(TffeOutlineData(outServers.Selected.Parent.Data).Entity);
          DatabaseItem.database.Close;
          LockDatabase(DatabaseItem.database);
          DatabaseItem.database.Open;
          Strpasswd.Assign(TffeDatabaseItem(TffeOutlineData(outServers.Selected.Parent.Data).Entity).Database.Session.Passwords);
          ServerName := outServers.Selected.Parent.Parent.Text;
          DataBaseName := outServers.Selected.Parent.Text;
          Protocol := TffeDatabaseItem(TffeOutlineData(outServers.Selected.Parent.Data).Entity).Server.Protocol;
          InitialStatement := 'SELECT * FROM ' +
            '"' + TffeTableItem(TffeOutlineData(outServers.Selected.Data).Entity).TableName + '"';
          With TffexpSession(TffeDatabaseItem(TffeOutlineData(outServers.Selected.Parent.Data).Entity).Database.Session) Do
            Begin
              Password := ffePassword;
              UserName := ffeUserName;
            End;
        End
      Else
        Begin
          DatabaseItem := TffeDatabaseItem(GetSelectedEntity);
          ServerName := outServers.Selected.Parent.Text;
          DataBaseName := outServers.Selected.Text;
          Protocol := TffeDatabaseItem(GetSelectedEntity).Server.Protocol;
          DatabaseItem.database.Close;
          DatabaseItem.Database.RecLocking := tlPessimisticWait;
          //LockDatabase(DatabaseItem.database);
          DatabaseItem.database.Open;
          Strpasswd.Assign(TffeDatabaseItem(GetSelectedEntity).Database.Session.Passwords);
          With TffexpSession(TffeDatabaseItem(GetSelectedEntity).Database.Session) Do
            Begin
              Password := ffePassword;
              UserName := ffeUserName;
            End;
        End;
      {End !!.07}
      Log := LogMain; {!!.02}
      Show;

    End;
End;
{--------}

Procedure TfrmMain.ShowTableBrowser(aTable: TffeTableItem);
Var
  OldCursor: TCursor;
  aTableDlg: TdlgTable;
  aTableDlgTrigger: TdlgTableTrigger;
  aTableDlgProcedures: TdlgTableProcedures;
  aTableDlgConstraints: TdlgTableConstraints;
Begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Try
    If UpperCase(aTable.TableName) = 'SYS$TRIGGERS' Then
      Begin
        aTableDlgTrigger := TdlgTableTrigger.Create(Application); {!!.02}
        With aTableDlgTrigger Do
          Begin
            TableItem := aTable; {!!.10}
            TableItem.Table.RecLockedBeforeEdit := False;
            Protocol := aTable.Server.Protocol; {!!.07}
            ServerName := aTable.Server.ServerName;
            DataBaseName := aTable.Database.DataBaseName;
            TableName := aTable.TableName;
            UserName := TffexpSession(aTable.Table.Session).ffeUserName;
            Password := TffexpSession(aTable.Table.Session).ffePassword;
            ReadOnly := (Not mnuOptionsLiveDataSets.Checked);
            Log := LogMain; {!!.02}
            Show;
          End;
      End
    Else If UpperCase(aTable.TableName) = 'SYS$PROCEDURES' Then
      Begin
        aTableDlgProcedures := TdlgTableProcedures.Create(Application);
        With aTableDlgProcedures Do
          Begin
            TableItem := aTable;
            TableItem.Table.RecLockedBeforeEdit := False;
            Protocol := aTable.Server.Protocol;
            ServerName := aTable.Server.ServerName;
            DataBaseName := aTable.Database.DataBaseName;
            TableName := aTable.TableName;
            UserName := TffexpSession(aTable.Table.Session).ffeUserName;
            Password := TffexpSession(aTable.Table.Session).ffePassword;
            ReadOnly := (Not mnuOptionsLiveDataSets.Checked);
            Log := LogMain;
            Show;
          End;
      End
    Else If UpperCase(aTable.TableName) = 'SYS$CONSTRAINTS' Then
      Begin
        aTableDlgConstraints := TdlgTableConstraints.Create(Application);
        With aTableDlgConstraints Do
          Begin
            TableItem := aTable;
            TableItem.Table.RecLockedBeforeEdit := False;
            Protocol := aTable.Server.Protocol;
            ServerName := aTable.Server.ServerName;
            DataBaseName := aTable.Database.DataBaseName;
            TableName := aTable.TableName;
            UserName := TffexpSession(aTable.Table.Session).ffeUserName;
            Password := TffexpSession(aTable.Table.Session).ffePassword;
            ReadOnly := (Not mnuOptionsLiveDataSets.Checked);
            Log := LogMain;
            Show;
          End;
      End
    Else
      Begin
        aTableDlg := TdlgTable.Create(Application); {!!.02}
        With aTableDlg Do
          Begin
            TableItem := aTable; {!!.10}
            TableItem.Table.RecLockedBeforeEdit := False;
            TableItem.Table.FilterEval := fseServer;
            Protocol := aTable.Server.Protocol; {!!.07}
            ServerName := aTable.Server.ServerName;
            aTable.database.Close;
            LockDatabase(TableItem.database.Database);
            aTable.database.Open;
            DataBaseName := aTable.Database.DataBaseName;
            TableName := aTable.TableName;
            UserName := TffexpSession(aTable.Table.Session).ffeUserName;
            Password := TffexpSession(aTable.Table.Session).ffePassword;
            ReadOnly := (Not mnuOptionsLiveDataSets.Checked);
            Log := LogMain; {!!.02}
            Show;
          End;
      End;
  Finally
    Screen.Cursor := OldCursor;
  End;
End;
{--------}

{====================================================================}

{===Form-level event handlers========================================}

Procedure TfrmMain.ApplicationEvents1Exception(Sender: TObject; E: Exception);
{$IFDEF USETeDEBUG}
Var
  i: Integer;
  sl: TSTringList;
  {$ENDIF}
Begin
  {$IFDEF USETeDEBUG}
  sl := TSTringList.Create;
  Try
    sl.Add(E.Message);
    If JclLastExceptStackList <> Nil Then
      JclLastExceptStackList.AddToStrings(sl);
    For i := 0 To sl.Count - 1 Do
      logMain.WriteString(sl[i]);
    Application.ShowException(E);
  Finally
    sl.Free;
  End;
  {$ELSE}
  Application.ShowException(E);
  {$ENDIF}
End;

Procedure TfrmMain.FormCreate(Sender: TObject);
Begin
  { write log to app directory }
  logMain.FileName := Config.WorkingDirectory + ChangeFileExt(ExtractFileName(Application.ExeName), '.LG'); {!!.11}
  Application.OnException := ApplicationEvents1Exception;
  HelpContext := hcMainOutline;
  Initialized := False;

  If FileExists(ExtractFilePath(ParamStr(0)) + 'FFE.HLP') Then
    Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'FFE.HLP'
  Else
    Application.HelpFile := ExtractFilePath(ParamStr(0)) + '..\HELP\FFE.HLP';

  mnuOptionsLiveDataSets.Checked := True;
  // tbOptionsLiveDataSets.Down := mnuOptionsLiveDataSets.Checked; {!!.06}

  LoadConfig;

  Application.OnMessage := AppMessage;
  caption := caption + Format(' %5.3f', [fsVersionNumber / 1000.0]);
  If fsVersionBeta > 0 Then
    Caption := Caption + ' Beta: ' + IntToStr(fsVersionBeta);
End;
{Begin !!.02}
{--------}

Procedure TfrmMain.FormClose(Sender: TObject; Var Action: TCloseAction);
Var
  Idx: Integer;
Begin
  For Idx := 0 To Pred(Screen.FormCount) Do
    If (Screen.Forms[Idx] Is TdlgTable) Or
      (Screen.Forms[Idx] Is TdlgTableProcedures) Or
      (Screen.Forms[Idx] Is TdlgTableTrigger) Or
      (Screen.Forms[Idx] Is TdlgTableConstraints) Or
      (Screen.Forms[Idx] Is TdlgQuery) Or
      (Screen.Forms[Idx] Is TdlgServerStats) Then {!!.11}
      Screen.Forms[Idx].Close;
End;
{End !!.02}
{--------}

Procedure TfrmMain.FormDestroy(Sender: TObject);
Begin
  ClosingApp := True;
  outServers.onClick := Nil;
  ServerList.Free;
  SaveConfig;
  OutlineClear;
End;
{====================================================================}

{===Server menu event handlers=======================================}

Procedure TfrmMain.mnuServerRefreshClick(Sender: TObject);
Begin
  LoadOutlineServers;
End;
{--------}

Procedure TfrmMain.mnuServerRegisterClick(Sender: TObject);
Begin
  If outServers.Items.Count > 0 Then
    If ShowRegisteredServersDlg = mrOK Then
      Try
        LoadOutlineServers;
      Except
        On E: Exception Do
          ShowMessage(E.Message);
      End;

End;
{--------}

Procedure TfrmMain.mnuServerExitClick(Sender: TObject);
Begin
  Close;
End;

{ "Options" menu event-handlers }

Procedure TfrmMain.mnuOptionsPrintSetupClick(Sender: TObject);
Begin
  dlgPrinterSetup.Execute;
End;
{====================================================================}

{===Server outline event handlers====================================}

Procedure TfrmMain.outServersClick(Sender: TObject);
{ Set the popup menu depending on which level we are on }
Begin
  With outServers Do
    Begin
      If assigned(Selected) Then
        Case TffeOutlineData(Selected.Data).EntityType Of
          etServer:
            Begin
              PopupMenu := popmnuServer;
            End;
          etDatabase:
            Begin
              PopupMenu := popmnuAlias;
            End;
          etTable:
            Begin
              PopupMenu := popmnuTable;
            End;
        End;
    End;
End;
{--------}

Procedure TfrmMain.outServersCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; Var Compare: Integer);
Begin
  Compare := FFAnsiCompareText(Node1.Text, Node2.Text); {!!.07}
End;
{--------}

Procedure TfrmMain.outServersMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  aNode: TTreeNode;
Begin
  If Button = mbRight Then
    Begin
      aNode := outServers.GetNodeAt(X, Y);
      If assigned(aNode) And assigned(aNode.Data) Then
        Begin
          outServers.Selected := aNode;
          Case TffeOutlineData(aNode.Data).EntityType Of
            etServer: PopupMenu := popmnuServer;
            etDatabase: PopupMenu := popmnuAlias;
            etTable: PopupMenu := popmnuTable;
          End;
          PopupMenu.Popup(ClientToScreen(Point(X, Y)).X + 5,
            ClientToScreen(Point(X, Y)).Y + 5);
        End;
    End;
End;
{====================================================================}

{===Server outline context menus event handlers======================}

Procedure TfrmMain.popmnuServerPopup(Sender: TObject);
Var
  Entity: TffeEntityItem;
Begin
  Entity := TffeOutlineData(outServers.Selected.Data).Entity;
  popmnuServerAttach.Enabled := Not TffeServerItem(Entity).Attached;
  popmnuServerDetach.Enabled := Not popmnuServerAttach.Enabled;
  popmnuServerNewDatabase.Enabled := Not popmnuServerAttach.Enabled;
End;
{--------}

Procedure TfrmMain.popmnuServerAttachClick(Sender: TObject);
Var
  aNode: TTreeNode;
  Server: TffeServerItem;
Begin

  aNode := outServers.Selected;
  {Begin !!.01}
  If DoAttach(aNode) = DBIERR_NONE Then
    Begin
      Server := TffeServerItem(GetSelectedEntity);

      { Expand the attached server.  If it has only one database then expand
        the database too. }
      aNode.Expand(Server.DatabaseCount = 1);
    End;
  {End !!.01}
End;
{--------}

Procedure TfrmMain.popmnuServerDetachClick(Sender: TObject);
Begin
  DoDetach;
End;
{--------}

{--------}

Procedure TfrmMain.RefreshDatabases(Sender: TObject);
Var
  aNode: TTreeNode;
  OldCursor: TCursor;
  Server: TffeServerItem;
Begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  outServers.Items.BeginUpdate;
  Try
    { Get the server. }
    aNode := outServers.Selected;
    Server := TffeServerItem(GetNodeEntity(aNode));
    Server.LoadDatabases;
    LoadOutlineDatabases(aNode);
    aNode.Expand(False);
  Finally
    outServers.Items.EndUpdate;
    Screen.Cursor := OldCursor;
  End;
End;
{--------}

Procedure TfrmMain.RefreshTables(Sender: TObject);
Var
  aNode: TTreeNode;
  Database: TffeDatabaseItem;
  OldCursor: TCursor;
Begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  outServers.Items.BeginUpdate;
  Try
    { Get the database. }
    aNode := outServers.Selected;
    Database := TffeDatabaseItem(GetNodeEntity(aNode));
    Database.LoadTables;
    LoadOutlineTables(aNode);
    aNode.Expand(True);
  Finally
    outServers.Items.EndUpdate;
    Screen.Cursor := OldCursor;
  End;
End;
{--------}

Procedure TfrmMain.popmnuDatabaseNewTableClick(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  TableIndex: Longint;
  dummy: Boolean;
Begin
  { make sure tablelist is loaded; implicitly checks for valid directory }
  outServersExpanding(outServers, outServers.Selected, dummy); {!!.06}
  Database := TffeDatabaseItem(GetSelectedEntity);
  With outServers Do
    If ShowCreateTableDlg(Database, TableIndex, Nil) = mrOK Then
      Begin
        LoadOutlineTables(Selected);
        Selected.Expand(False);
      End;
End;
{--------}

Procedure TfrmMain.popmnuServerNewDatabaseClick(Sender: TObject);
Var
  aDatabase: TffeDatabaseItem;
  anEntity: TffeEntityItem;
  aNode: TTreeNode;
  Server: TffeServerItem;
Begin
  aDatabase := Nil;
  Server := Nil;
  aNode := outServers.Selected;
  anEntity := TffeOutlineData(aNode.Data).Entity;
  Case anEntity.EntityType Of
    etServer:
      Server := TffeServerItem(anEntity);
    etDatabase:
      Begin
        aNode := aNode.Parent;
        Server := TffeServerItem(TffeOutlineData(aNode.Data).Entity);
      End;
  End;

  With outServers Do
    Begin
      If ShowAddAliasDlg(Server, aDatabase) = mrOK Then
        LoadOutlineTables
          (AddOutlineDatabase(aNode, aDatabase));
      AlphaSort;
    End;
End;
{--------}

Function TfrmMain.GetNewSelectedNode(aNode: TTreeNode): TTreeNode;
Begin
  { Does the node have a previous sibling? }
  Result := aNode.Parent.GetPrevChild(aNode);
  If Not assigned(Result) Then
    Begin
      { No previous sibling.  See if has next sibling. }
      Result := aNode.Parent.GetNextChild(aNode);
      If Not assigned(Result) Then
        { No siblings.  Default to parent node. }
        Result := aNode.Parent;
    End;
End;
{--------}

Procedure TfrmMain.popmnuDatabaseDeleteClick(Sender: TObject);
Var
  aNode: TTreeNode;
  Database: TffeDatabaseItem;
Begin
  Database := TffeDatabaseItem(GetSelectedEntity);
  If MessageDlg('Delete ' + Database.DataBaseName + '?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Begin
      Screen.Cursor := crHourglass;
      Try

        { Delete the database from the server. }
        Database.Server.DropDatabase(Database.DataBaseName);

        { Delete database from outline }
        With outServers Do
          Begin
            aNode := Selected;
            If assigned(aNode.Data) Then
              TffeOutlineData(aNode.Data).free;
            Selected := GetNewSelectedNode(aNode);
            Items.Delete(aNode);
          End;
      Finally
        Screen.Cursor := crDefault;
      End;
    End;
End;
{--------}

Procedure TfrmMain.popmnuDatabaseRenameClick(Sender: TObject);
Begin
  outServers.Selected.EditText;
End;
{--------}

Procedure TfrmMain.popmnuDatabaseImportSchemaClick(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  TableIndex: Longint;
  dummy: Boolean;
Begin
  outServersExpanding(outServers, outServers.Selected, dummy);
  TableIndex := -1;
  Database := TffeDatabaseItem(GetSelectedEntity);
  With outServers Do
    Begin
      ShowImportDlg(Database, TableIndex);
      If TableIndex <> -1 Then {we have a new table}
        AddOutlineTable(Selected, Database.Tables[TableIndex]);
    End;
End;
{--------}

Procedure TfrmMain.popmnuTablePopup(Sender: TObject);
Var
  Table: TffeTableItem;
  I: Integer;
Begin
  Table := TffeTableItem(GetSelectedEntity);
  With Table Do
    With popmnuTable Do
      Begin
        If Rebuilding Then
          Begin
            For I := 0 To Items.Count - 1 Do
              Items[I].Enabled := False;
            popmnuTableNew.Enabled := True;
          End
        Else
          For I := 0 To Items.Count - 1 Do
            Items[I].Enabled := True;
      End;
  PackReindexUndeleteAll1.Enabled := (table.Table.TableVersion >= 1064);
  Recoveryonlydeletedrecords1.Enabled := PackReindexUndeleteAll1.Enabled;
End;
{--------}

Procedure TfrmMain.popmnuTableDefinitionClick(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  Table: TffeTableItem;
Begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  ShowViewTableStructureDlg(Database, Database.IndexOf(Table), vtViewFields);
End;
{--------}

Procedure TfrmMain.popmnuTableNewClick(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  Table: TffeTableItem;
  TableIndex: Longint;
Begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  TableIndex := Database.IndexOf(Table);
  With outServers Do
    If ShowCreateTableDlg(Database, TableIndex, Nil) = mrOK Then
      LoadOutlineTables(outServers.Selected);
  //      AddOutlineTable(Selected, Table);
End;
{--------}

Procedure TfrmMain.popmnuTableDeleteClick(Sender: TObject);
Var
  aNode: TTreeNode;
  Table: TffeTableItem;
Begin
  Table := TffeTableItem(GetSelectedEntity);
  If MessageDlg(Format('Delete table %s?', [Table.TableName]),
    mtConfirmation,
    [mbYes, mbNo],
    0) = mrYes Then
    Begin
      Screen.Cursor := crHourglass;
      Try
        Table.Database.DropTable(Table.Database.IndexOf(Table));

        { Remove table from tree view. }
        With outServers Do
          Begin
            aNode := Selected;
            If assigned(aNode.Data) Then
              TffeOutlineData(aNode.Data).free;
            Selected := GetNewSelectedNode(aNode);
            aNode.Delete;
          End;
      Finally
        Screen.Cursor := crDefault;
      End;
    End;
End;
{--------}

Procedure TfrmMain.popmnuTablePackClick(Sender: TObject);
Var
  aNode: TTreeNode;
  Status: TffRebuildStatus;
  RebuildDone: Boolean;
  Table: TffeTableItem;
  PromptMsg: String;
  StatusMsg: String;
Begin
  PromptMsg := 'Are you sure you want to defragmentation this table?';
  StatusMsg := 'Defragmentation';

  If MessageDlg(PromptMsg, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes Then
    Begin

      aNode := outServers.Selected;
      Table := TffeTableItem(GetNodeEntity(aNode));

      With Table Do
        Begin
          Pack(False, False);

          If Rebuilding Then
            Begin

              { Change the display in the outline; table will be unavailable
                until the rebuild is done. }
              aNode.Text := TableName + ' (Defragmentation)';
              Try
                Application.ProcessMessages;

                { Display the rebuild progress window }
                With TfrmRebuildStatus.Create(Nil) Do
                  Try
                    ShowProgress(StatusMsg, TableName); {!!.10}
                    Try
                      Repeat
                        CheckRebuildStatus(RebuildDone, Status);
                        If Not RebuildDone Then
                          Begin
                            UpdateProgress(RebuildDone, Status);
                            Sleep(250);
                          End;
                      Until RebuildDone;
                    Finally
                      Hide;
                    End;
                  Finally
                    Free;
                  End;
              Finally
                aNode.Text := TableName;
              End;
            End;
        End;
    End;
End;
{--------}

Procedure TfrmMain.popmnuTableReindexClick(Sender: TObject);
Var
  aNode: TTreeNode;
  IndexNum: Integer;
  RebuildDone: Boolean;
  Status: TffRebuildStatus;
  Table: TffeTableItem;
Begin
  Table := TffeTableItem(GetSelectedEntity);
  If SelectIndexDlg(Table, IndexNum) = mrOk Then
    Begin
      aNode := outServers.Selected;

      With Table Do
        Begin
          Reindex(IndexNum);

          { Change the display in the outline; table will be unavailable
            until the rebuild is done. }
          aNode.Text := TableName + ' (reindexing)';
          Try
            Application.ProcessMessages;

            { Display the rebuild progress window }
            With TfrmRebuildStatus.Create(Nil) Do
              Try
                ShowProgress('Reindexing', TableName);
                Try
                  Repeat
                    CheckRebuildStatus(RebuildDone, Status);
                    If Not RebuildDone Then
                      Begin
                        UpdateProgress(RebuildDone, Status);
                        Sleep(250);
                      End;
                  Until RebuildDone;
                Finally
                  Hide;
                End;
              Finally
                Free;
              End;
          Finally
            aNode.Text := TableName;
          End;
        End;
    End;
End;
{--------}

Procedure TfrmMain.popmnuTableRedefineClick(Sender: TObject);
Var
  aNode: TTreeNode;
  Status: TffRebuildStatus;
  RebuildDone: Boolean;
  Database: TffeDatabaseItem;
  Table: TffeTableItem;
  TableIndex: Longint;
  UnableToOpen: Boolean;
Begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;

  TableIndex := Database.IndexOf(Table);
  With outServers Do
    Begin
      Database.Server.Session.Active := False;
      Database.Server.Session.Active := True;
      Database.Open;
      If Table.Table.Active Then
        Table.Table.Close;
      Table.Table.Exclusive := True;
      Try
        Screen.Cursor := crHourGlass;
        Try
          Table.Table.Open;
          Table.Table.Close;
          UnableToOpen := False;
        Finally
          Table.Table.Exclusive := False;
          Screen.Cursor := crDefault;
        End;
      Except
        UnableToOpen := True;
      End;
      If UnableToOpen Then
        Begin
          MessageDlg('Unable to gain exclusive access to the table. Restructure operation '
            + #13 + #10 + 'cannot contiue.', mtInformation, [mbOK], 0);
          Exit;
        End;
      If ShowRestructureTableDlg(Database, TableIndex) = mrOK Then
        Begin
          aNode := outServers.Selected;

          With Table Do
            Begin
              If Rebuilding Then
                Begin

                  { Change the display in the outline; table will be unavailable
                    until the rebuild is done. }
                  aNode.Text := TableName + ' (restructuring)';
                  Try
                    Application.ProcessMessages;

                    { Display the rebuild progress window }
                    With TfrmRebuildStatus.Create(Nil) Do
                      Try
                        ShowProgress('Restructuring', TableName);
                        Try
                          Repeat
                            CheckRebuildStatus(RebuildDone, Status);
                            If Not RebuildDone Then
                              Begin
                                UpdateProgress(RebuildDone, Status);
                                Sleep(250);
                              End;
                          Until RebuildDone;
                        Finally
                          Hide;
                        End;
                        Check(Status.rsErrorCode);
                      Finally
                        Free;
                      End;
                  Finally
                    aNode.Text := TableName;
                  End;
                End;
            End;
        End;
    End;
  If Table.Table.Active Then {!!.06}
    Table.Table.Close {!!.06}
End;
{--------}

Procedure TfrmMain.popmnuTableImportSchemaClick(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  Table: TffeTableItem;
  TableIndex: Longint;
Begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  TableIndex := Database.IndexOf(Table);

  With outServers Do
    Begin
      ShowImportDlg(Database, TableIndex);
      If TableIndex <> -1 Then {we have a new table}
        AddOutlineTable(Selected, Table);
    End;
End;
{--------}

Procedure TfrmMain.popmnuTableEmptyClick(Sender: TObject);
Var
  aSavCursor: TCursor; {!!.01}
  aTable: TffeTableItem;
Begin
  aTable := TffeTableItem(GetSelectedEntity);
  With aTable Do
    Begin
      Table.DisableControls;
      Try
        With Table Do
          Begin
            Close;
            Exclusive := True;
            Open;
          End;

        If RecordCount = 0 Then
          ShowMessage('Table is already empty')
        Else
          Begin
            If MessageDlg('Delete all records in ' + TableName + '?',
              mtWarning, [mbYes, mbNo], 0) = mrYes Then
              Begin
                aSavCursor := Screen.Cursor; {!!.01}
                Screen.Cursor := crHourglass;
                Try
                  Table.EmptyTable;
                Finally
                  Screen.Cursor := aSavCursor;
                End;
              End;
          End;
      Finally
        Table.Close; {!!.01}
        Table.Exclusive := False; {!!.01}
        Table.EnableControls;
      End;
    End;
End;
{--------}

Procedure TfrmMain.ExitBtnClick(Sender: TObject);
Begin
  Close;
End;
{--------}

Procedure TfrmMain.UncheckMenuGroupSiblings(aMenuItem: TMenuItem);
Var
  I: Integer;
Begin
  With aMenuItem.Parent Do
    Begin
      For I := 0 To Count - 1 Do
        If (Items[I] <> aMenuItem) And (Items[I].GroupIndex = aMenuItem.GroupIndex) Then
          Items[I].Checked := False;
    End;
End;
{--------}

Procedure TfrmMain.mnuSetAutoIncClick(Sender: TObject);
Var
  aTable: TffeTableItem;
  Seed: Int64; {!!.10}
  Step: Longint;
Begin
  aTable := TffeTableItem(GetSelectedEntity);
  Seed := aTable.GetAutoInc(Step);
  With aTable Do
    Begin
      If ShowAutoIncDlg(TableName, Seed, Step) = mrOK Then
        SetAutoIncSeed(Seed, Step);
    End;
End;
{--------}

Procedure TfrmMain.outServersDblClick(Sender: TObject);
Var
  aTable: TffeTableItem;
  //  dummy : boolean;
Begin
  With outServers Do
    Begin
      If assigned(Selected) Then
        Case TffeOutlineData(Selected.Data).EntityType Of
          etServer:
            Begin
              PopupMenu := popmnuServer;
              //            outServersExpanding(outServers, outServers.Selected, dummy);
            End;
          etDatabase:
            Begin
              PopupMenu := popmnuAlias;
              //            outServersExpanding(outServers, outServers.Selected, dummy);
            End;
          etTable:
            Begin
              aTable := TffeTableItem(GetSelectedEntity);
              aTable.Table.RecLockedBeforeEdit := False;
              aTable.Table.FilterEval := fseServer;
              PopupMenu := popmnuTable;
              // show
              ShowTableBrowser(aTable);
            End;
        End;
    End;
End;
{--------}
{function TfrmMain.mapProtocolClassToProtocol(const Protocol : TffCommsProtocolClass) : TffProtocolType;
begin
  if (Protocol = TffTCPIPProtocol) then
    result := ptTCPIP
  else if (Protocol = TffIPXSPXProtocol) then
    result := ptIPXSPX
  else
    result := ptSingleUser;
end;}
{--------}

Procedure TfrmMain.mnuOptionsLiveDatasetsClick(Sender: TObject);
Var {!!.01}
  Idx: Integer; {!!.01}
Begin
  If outServers.Items.Count = 0 Then Exit;
  mnuOptionsLiveDataSets.Checked := Not mnuOptionsLiveDataSets.Checked;
  //  tbOptionsLiveDataSets.Down := mnuOptionsLiveDataSets.Checked; {!!.06}
  With Config Do
    If mnuOptionsLiveDataSets.Checked Then
      Options := Options + [coLiveDatasets]
    Else
      Options := Options - [coLiveDatasets];

  For Idx := 0 To Pred(Screen.FormCount) Do {BEGIN !!.01}
    If Screen.Forms[Idx] Is TdlgTable Then
      With TdlgTable(Screen.Forms[Idx]) Do
        Begin
          ReadOnly := Not mnuOptionsLiveDataSets.Checked;
          UpdateDisplay;
        End; {END !!.01}
End;
{--------}

Procedure TfrmMain.outServersExpanding(Sender: TObject; Node: TTreeNode;
  Var AllowExpansion: Boolean);
Var
  aData: TffeOutlineData;
Begin
  aData := TffeOutlineData(Node.Data);
  AllowExpansion := aData.EntityType In [etServer, etDatabase];

  { If we can expand and the node currently has no children, go grab the
    children. }
  If AllowExpansion And (Node.Count = 0) Then
    Begin
      Case aData.EntityType Of
        etServer:
          LoadOutlineDatabases(Node);
        etDatabase:
          LoadOutlineTables(Node);
      End; { case }
      {Begin !!.01}
      If Node.Expanded Then
        Begin
          Node.HasChildren := (Node.Count > 0);
          AllowExpansion := Node.HasChildren;
        End;
      {End !!.01}
    End;
End;
{--------}

Procedure TfrmMain.outServersEditing(Sender: TObject; Node: TTreeNode;
  Var AllowEdit: Boolean);
Begin
  AllowEdit := GetNodeEntityType(Node) In [etDatabase, etTable];
End;
{--------}

Procedure TfrmMain.outServersEdited(Sender: TObject; Node: TTreeNode;
  Var S: String);
Var
  OldCursor: TCursor;
Begin
  { Perform the rename. }
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Try
    Case GetNodeEntityType(Node) Of
      etDatabase:
        Begin
          TffeDatabaseItem(GetNodeEntity(Node)).Rename(S);
          Node.Text := S;
          LoadOutlineServers; {!!.01}
        End;
      etTable:
        Begin
          TffeTableItem(GetNodeEntity(Node)).Rename(S);
          Node.Text := S;
        End;
    End;
  Finally
    Screen.Cursor := OldCursor;
  End;
End;
{--------}
{$IFNDEF DCC6OrLater}

Function CenterPoint(Const Rect: TRect): TPoint;
Begin
  With Rect Do
    Begin
      Result.X := (Right - Left) Div 2 + Left;
      Result.Y := (Bottom - Top) Div 2 + Top;
    End;
End;
{$ENDIF}
{--------}

Procedure TfrmMain.outServersKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  aNode: TTreeNode;
Begin
  { If user presses F2 then edit current node. }
  If (Key = VK_F2) And assigned(outServers.Selected) Then
    outServers.Selected.EditText
  Else If Key = VK_RETURN Then
    outServersDblClick(Nil)
      {Begin !!.07}
{ support the windows keyboard context menu key }
  Else If (Key = VK_APPS) Or
    ((Shift = [ssShift]) And (Key = VK_F10)) Then
    Begin
      aNode := outServers.Selected;
      If assigned(aNode) And assigned(aNode.Data) Then
        Begin
          Case TffeOutlineData(aNode.Data).EntityType Of
            etServer: PopupMenu := popmnuServer;
            etDatabase: PopupMenu := popmnuAlias;
            etTable: PopupMenu := popmnuTable;
          End;
          PopupMenu.Popup(ClientToScreen(CenterPoint(aNode.DisplayRect(True))).X + 5,
            ClientToScreen(CenterPoint(aNode.DisplayRect(True))).Y + 5);
        End;
    End;
  {End !!.07}
End;
{--------}

Procedure TfrmMain.mnuViewTableClick(Sender: TObject);
Begin
  outServersDblClick(Nil);
End;
{--------}

Procedure TfrmMain.mnuDatabaseSQLClick(Sender: TObject);
Begin
  ShowQueryWindow(0);
End;
{Begin !!.06}
{--------}

Procedure TfrmMain.mnuCloseAllClick(Sender: TObject);
Var
  Idx: Integer;
Begin
  For Idx := 0 To Pred(Screen.FormCount) Do
    If (Screen.Forms[Idx] Is TdlgTable) Or
      (Screen.Forms[Idx] Is TdlgTableProcedures) Or
      (Screen.Forms[Idx] Is TdlgTableTrigger) Or
      (Screen.Forms[Idx] Is TdlgTableConstraints) Or
      (Screen.Forms[Idx] Is TdlgQuery) Then
      Screen.Forms[Idx].Close;
End;
{End !!.06}

{Begin !!.06}
{--------}

Procedure TfrmMain.UpdateWindowsMenuItems;
Var
  Count,
    Idx: Integer;
  NewItem: TMenuItem;
Begin
  { ensure windows are closed first }
  Application.ProcessMessages;
  { remove all items - requires that mnuWindowsSplitter is the last
    item in the menu at designtime! }
  While mnuWindows.Items[mnuWindows.Count - 1] <> mnuWindowsSplitter Do
    mnuWindows.Delete(mnuWindows.Count - 1);
  { add back existing forms }
  Count := 1;
  { note: it varies between Delphi versions wether new forms are added
    at the beginning or end of the Screen.Forms array. The code below
    assumes it is compiled with Delphi 6. The last opened window should
    appear at the bottom of the menu. If it appears at the top, switch
    the loop parameters around. }
  For Idx := Pred(Screen.FormCount) Downto 0 Do
    If (Screen.Forms[Idx] Is TdlgTable) Or
      (Screen.Forms[Idx] Is TdlgTableProcedures) Or
      (Screen.Forms[Idx] Is TdlgTableTrigger) Or
      (Screen.Forms[Idx] Is TdlgTableConstraints) Or
      (Screen.Forms[Idx] Is TdlgQuery) Or
      (Screen.Forms[Idx] Is TfrmTableStruct) Then
      Begin {!!.11}
        NewItem := TMenuItem.Create(Nil);
        NewItem.Caption := Screen.Forms[Idx].Caption;
        If Count <= 9 Then
          NewItem.Caption := '&' + IntToStr(Count) + ' ' + NewItem.Caption;
        Inc(Count);
        NewItem.OnClick := WindowsMenuItemClick;
        NewItem.Tag := Integer(Screen.Forms[Idx]);
        mnuWindows.Add(NewItem);
      End;
End;
{End !!.06}

{Begin !!.06}
{--------}

Procedure TfrmMain.WindowsMenuItemClick(Sender: TObject);
Begin
  If (Sender Is TMenuItem) And
    Assigned(Pointer(TMenuItem(Sender).Tag)) Then
    TForm(TMenuItem(Sender).Tag).BringToFront;
End;
{End !!.06}

{Begin !!.06}
{--------}

Procedure TfrmMain.mnuWindowsClick(Sender: TObject);
Begin
  { we only update the menu when the user actually clicks it. the update
    executes so fast that the user won't notice anyway. }
  UpdateWindowsMenuItems;
  mnuCloseAll.Enabled := Screen.FormCount > 1;
  //tbCloseAll.Enabled := mnuCloseAll.Enabled;
End;
{End !!.06}

{Begin !!.06}
{--------}

Procedure TfrmMain.AppMessage(Var Msg: TMsg; Var Handled: Boolean);
Var
  Idx: Integer;
Begin
  { trap ALT-F6 keypresses and make the next window in the
    window list active }
  If (Msg.message = WM_SYSKEYDOWN) And
    (Msg.wparam = VK_F6) Then
    Begin
      If (Screen.FormCount > 1) And
        (Screen.ActiveForm Is TfrmMain) Or
        (Screen.ActiveForm Is TdlgTable) Or
        (Screen.ActiveForm Is TdlgTableProcedures) Or
        (Screen.ActiveForm Is TdlgTableTrigger) Or
        (Screen.ActiveForm Is TdlgTableConstraints) Or
        (Screen.ActiveForm Is TdlgQuery) Or
        (Screen.ActiveForm Is TfrmTableStruct) Then
        Begin {!!.11}
          Idx := 0;
          { find index of active form }
          While (Idx < Screen.FormCount) And
            (Screen.ActiveForm <> Screen.Forms[Idx]) Do
            Inc(Idx);
          { note: it may be that the code below will fail, depending on what delphi
            version it is compiled with and how that delphi version updates the
            Screen.Forms array. the code below works with Delphi 6. }
          { if at start of array, wrap around, else pick previous in list }
          If Idx = 0 Then
            Screen.Forms[Pred(Screen.FormCount)].BringToFront
          Else
            Screen.Forms[Idx - 1].BringToFront;
        End;
      Handled := True;
    End;
  { for all other messages, Handled remains False }
  { so that other message handlers can respond }
End;
{End !!.06}
{Begin !!.06}
{--------}

{End !!.06}
{Begin !!.07}

Procedure TfrmMain.mnuToolsFFCommsClick(Sender: TObject);
Begin
  With ufscomms.TfrmFFCommsMain.Create(Self) Do
    Try
      Caption := 'Set Default Server';
      //    Label3.Visible := False;
      If ShowModal = mrOK Then
        Initialize;
    Finally
      Free;
    End;
End;
{End !!.07}
{Begin !!.07}

Procedure TfrmMain.mnuSetAsAutomaticDefaultClick(Sender: TObject);
Begin
  { leave servername alone if SUP, like FFCOMMS does }
  If TffeServerItem(GetSelectedEntity).Protocol = ptSingleUser Then
    FfClientConfigWriteProtocolName(fsc_SingleUser)
  Else
    Begin
      If TffeServerItem(GetSelectedEntity).Protocol = ptTCPIP Then
        FFClientConfigWriteProtocolName(fsc_TCPIP)
      Else If TffeServerItem(GetSelectedEntity).Protocol = ptIPXSPX Then
        FFClientConfigWriteProtocolName(fsc_IPXSPX);
      FFClientConfigWriteServerName(TffeServerItem(GetSelectedEntity).ServerName);
    End;
  Initialize;
End;
{End !!.07}

Procedure TfrmMain.outServersChange(Sender: TObject; Node: TTreeNode);
Begin
  outServersClick(Sender);
End;

Procedure TfrmMain.outServersContextPopup(Sender: TObject;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  {}
End;

{Begin !!.11}

Procedure TfrmMain.popmnuServerRefreshClick(Sender: TObject);
Begin
  RefreshDatabases(Sender);
End;

{--------}

Procedure TfrmMain.ShowServerStatistics(aServer: TffeServerItem);
Var
  OldCursor: TCursor;
  dlgServerStats: TdlgServerStats;
Begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  Try
    dlgServerStats := TdlgServerStats.Create(Application); {!!.02}
    With dlgServerStats Do
      Begin
        Log := LogMain;
        Protocol := aServer.Protocol;
        ServerName := aServer.ServerName;
        UserName := TffexpSession(aServer.Session).ffeUserName;
        Password := TffexpSession(aServer.Session).ffePassword;
        Show;
      End;
  Finally
    Screen.Cursor := OldCursor;
  End;
End;

Procedure TfrmMain.popmnuServerStatisticsClick(Sender: TObject);
Var
  anEntity: TffeEntityItem;
  Server: TffeServerItem;
Begin
  anEntity := TffeOutlineData(outServers.Selected.Data).Entity;
  Server := TffeServerItem(anEntity);
  ShowServerStatistics(Server);
End;

Procedure TfrmMain.mnuOptionsSetDefaultTimeoutClick(Sender: TObject);
Var
  sTimeout: String;
  res: Boolean;
  Idx: Integer;
Begin
  If outServers.Items.Count = 0 Then Exit;
  sTimeout := IntToStr(Config.DefaultTimeout);
  Repeat
    res := InputQuery('Default Timeout (ms)', 'Value:', sTimeout);
    If res Then
      Try
        Config.DefaultTimeout := StrToInt(sTimeout);
        If Config.DefaultTimeout < -1 Then
          Raise EConvertError.Create('');
        {Begin !!.11}
        { set default timeout on open servers, tables and queries }
        For idx := 0 To ServerList.Count - 1 Do
          If Assigned(ServerList.Items[idx].Client) Then
            ServerList.Items[idx].Client.TimeOut := Config.DefaultTimeout;
        For Idx := 0 To Pred(Screen.FormCount) Do
          If (Screen.Forms[Idx] Is TdlgTable) Then
            TdlgTable(Screen.Forms[Idx]).UpdateDefaultTimeout
          Else If (Screen.Forms[Idx] Is TdlgTableTrigger) Then
            TdlgTableTrigger(Screen.Forms[Idx]).UpdateDefaultTimeout
          Else If (Screen.Forms[Idx] Is TdlgTableConstraints) Then
            TdlgTableConstraints(Screen.Forms[Idx]).UpdateDefaultTimeout
          Else If (Screen.Forms[Idx] Is TdlgTableProcedures) Then
            TdlgTableProcedures(Screen.Forms[Idx]).UpdateDefaultTimeout
          Else If (Screen.Forms[Idx] Is TdlgQuery) Then
            TdlgQuery(Screen.Forms[Idx]).UpdateDefaultTimeout;
        {End !!.11}
        res := False;
      Except
        On EConvertError Do
          Begin
            MessageDlg('Value must be a number between -1 and ' + IntToStr(MaxInt), mtError, [mbOK], 0);
          End;
      End;
  Until Not res;
End;
{End !!.11}

Procedure TfrmMain.Addtrigertable1Click(Sender: TObject);
Var
  Dict: TFSInfoDict;
  FldArray: TffFieldList;
  FieldsAscDesc, FieldsNullTop: TffFieldList;
  FieldsCase, FieldsSize, FieldsFlags: TffFieldList;
  fiValCheck: TffVCheckDescriptor;
  IdxHlprs: TffFieldIHList;
  Database: TffeDatabaseItem;
  TableIndex: Longint;
  Ex: Boolean;
Begin
  { make sure tablelist is loaded; implicitly checks for valid directory }
  outServersExpanding(outServers, outServers.Selected, Ex); {!!.06}
  Database := TffeDatabaseItem(GetSelectedEntity);
  Database.Database.Connected := True;

  ex := Database.Database.TableExists('sys$triggers');
  If ex Then
    If MessageDlg('Replace sys$triggers?',
      mtConfirmation, [mbYes, mbNo], 0) = mrNo Then Exit;
  Dict := TFSInfoDict.Create(4096);
  Try
    With Dict Do
      Begin
        fiValCheck.vdHasDefVal := True;
        FSStringToVCheckVal('A', fstShortString, fiValCheck.vdDefVal);
        AddField('ACTIVE', '', fstShortString, 1, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('TRIGGERNAME', fstShortString, fiValCheck.vdDefVal);
        AddField('NAME', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('TABLENAME', fstShortString, fiValCheck.vdDefVal);
        AddField('TABLENAME', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('BEFOREINSERT', fstShortString, fiValCheck.vdDefVal);
        AddField('METODENAME', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('0', fstInt32, fiValCheck.vdDefVal);
        AddField('POSITION', '', fstInt32, 10, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        AddField('CODE', '', fstBlobMemo, sizeof(Longint), 0, False, Nil, blNone, '', rNone, False, duNormal);
        AddField('BLR', '', fstBlobMemo, sizeof(Longint), 0, False, Nil, blNone, '', rNone, False, duNormal);

        FSStringToVCheckVal('0', fstInt32, fiValCheck.vdDefVal);
        AddField('FLAGS', '', fstInt32, 10, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        AddField('DESCRIPTION', '', fstBlobMemo, sizeof(Longint), 0, False, Nil, blNone, '', rNone, False, duNormal);

        FSStringToVCheckVal('<NOW>', fstDateTime, fiValCheck.vdDefVal);
        AddField('DATECREATE', '', fstDateTime, 24, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        FldArray[0] := 0;
        FldArray[1] := 2;
        FldArray[2] := 3;
        FldArray[3] := 4;
        IdxHlprs[0] := '';
        IdxHlprs[1] := '';
        IdxHlprs[2] := '';
        IdxHlprs[3] := '';
        FieldsAscDesc[0] := 1;
        FieldsAscDesc[1] := 1;
        FieldsAscDesc[2] := 1;
        FieldsAscDesc[3] := 1;
        FieldsCase[0] := 1;
        FieldsCase[1] := 1;
        FieldsCase[2] := 1;
        FieldsCase[3] := 1;
        FieldsSize[0] := 0;
        FieldsSize[1] := 0;
        FieldsSize[2] := 0;
        FieldsSize[3] := 0;
        FieldsFlags[0] := 0;
        FieldsFlags[1] := 0;
        FieldsFlags[2] := 0;
        FieldsFlags[3] := 0;
        FieldsNullTop[0] := 1;
        FieldsNullTop[1] := 1;
        FieldsNullTop[2] := 1;
        FieldsNullTop[3] := 1;
        AddIndex('RELATION', '', 0, 4, FldArray, FieldsAscDesc, FieldsCase,
          FieldsSize, FieldsFlags, FieldsNullTop, IdxHlprs, True);

      End;
    Dict.IsEncrypted := True;
    Dict.TableType := ttSystem;
    Dict.FileDescriptor[0].fdDesc := 'Trigger table';
    If Database.Database.CreateTable(True, 'sys$triggers', Dict) <> 0 Then
      Raise Exception.Create('Error creating table')
    Else
      LoadOutlineTables(outServers.Selected);
  Finally
    Dict.Free;
  End;
End;

Procedure TfrmMain.Addconstrainttable1Click(Sender: TObject);
Var
  Dict: TFSInfoDict;
  FldArray, FieldsNullTop: TffFieldList;
  fiValCheck: TffVCheckDescriptor;
  IdxHlprs: TffFieldIHList;
  Database: TffeDatabaseItem;
  FieldsAscDesc, FieldsSize, FieldsFlags: TffFieldList;
  TableIndex: Longint;
  dummy, ex: Boolean;
  FieldsCase: TffFieldList;
Begin
  { make sure tablelist is loaded; implicitly checks for valid directory }
  outServersExpanding(outServers, outServers.Selected, dummy); {!!.06}
  Database := TffeDatabaseItem(GetSelectedEntity);
  Database.Database.Connected := True;

  ex := Database.Database.TableExists('sys$constraints');
  If ex Then
    If MessageDlg('Replace sys$constraints?',
      mtConfirmation, [mbYes, mbNo], 0) = mrNo Then Exit;

  Dict := TFSInfoDict.Create(4096);
  Try
    With Dict Do
      Begin
        fiValCheck.vdHasDefVal := True;
        FSStringToVCheckVal('A', fstShortString, fiValCheck.vdDefVal);
        AddField('ACTIVE', '', fstShortString, 1, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('CONSTRAINTNAME', fstShortString, fiValCheck.vdDefVal);
        AddField('NAME', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('CASCADEDELETE', fstShortString, fiValCheck.vdDefVal);
        AddField('METODENAME', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('0', fstInt32, fiValCheck.vdDefVal);
        AddField('POSITION', '', fstInt32, 10, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        FSStringToVCheckVal('SOURCE TABLE', fstShortString, fiValCheck.vdDefVal);
        AddField('SOURCE', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('SOURCE FIELD', fstShortString, fiValCheck.vdDefVal);
        AddField('SOURCEFIELD', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('DEST TABLE', fstShortString, fiValCheck.vdDefVal);
        AddField('DESTINATION', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('DEST FIELD', fstShortString, fiValCheck.vdDefVal);
        AddField('DESTINATIONFIELD', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        //FSStringToVCheckVal('DEST INDEX', fstShortString, fiValCheck.vdDefVal);
        AddField('DESTINATIONINDEX', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        AddField('EXPRESSION', '', fstNullString, 1024, 0, False, Nil, blNone, '', rNone, False, duNormal);

        FSStringToVCheckVal('0', fstInt32, fiValCheck.vdDefVal);
        AddField('FLAGS', '', fstInt32, 10, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        AddField('DESCRIPTION', '', fstBlobMemo, sizeof(Longint), 0, False, Nil, blNone, '', rNone, False, duNormal);

        FSStringToVCheckVal('<NOW>', fstDateTime, fiValCheck.vdDefVal);
        AddField('DATECREATE', '', fstDateTime, 24, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        FldArray[0] := 0;
        FldArray[1] := 4;
        FldArray[2] := 2;
        FldArray[3] := 3;
        IdxHlprs[0] := '';
        IdxHlprs[1] := '';
        IdxHlprs[2] := '';
        IdxHlprs[3] := '';
        FieldsAscDesc[0] := 1;
        FieldsAscDesc[1] := 1;
        FieldsAscDesc[2] := 1;
        FieldsAscDesc[3] := 1;
        FieldsCase[0] := 1;
        FieldsCase[1] := 1;
        FieldsCase[2] := 1;
        FieldsCase[3] := 1;
        FieldsSize[0] := 0;
        FieldsSize[1] := 0;
        FieldsSize[2] := 0;
        FieldsSize[3] := 0;
        FieldsFlags[0] := 0;
        FieldsFlags[1] := 0;
        FieldsFlags[2] := 0;
        FieldsFlags[3] := 0;
        FieldsNullTop[0] := 1;
        FieldsNullTop[1] := 1;
        FieldsNullTop[2] := 1;
        FieldsNullTop[3] := 1;
        AddIndex('RELATION', '', 0, 4, FldArray, FieldsAscDesc, FieldsCase,
          FieldsSize, FieldsFlags, FieldsNullTop, IdxHlprs, True);

      End;
    Dict.IsEncrypted := True;
    Dict.TableType := ttSystem;
    Dict.FileDescriptor[0].fdDesc := 'Contstraint table';
    If Database.Database.CreateTable(True, 'sys$constraints', Dict) <> 0 Then
      Raise Exception.Create('Error creating table')
    Else
      LoadOutlineTables(outServers.Selected);
  Finally
    Dict.Free;
  End;
End;

Procedure TfrmMain.Addprocedurestable1Click(Sender: TObject);
Var
  Dict: TFSInfoDict;
  FldArray, FieldsNullTop: TffFieldList;
  fiValCheck: TffVCheckDescriptor;
  IdxHlprs: TffFieldIHList;
  Database: TffeDatabaseItem;
  FieldsAscDesc, FieldsSize, FieldsFlags: TffFieldList;
  TableIndex: Longint;
  dummy, ex: Boolean;
  FieldsCase: TffFieldList;
Begin
  { make sure tablelist is loaded; implicitly checks for valid directory }
  outServersExpanding(outServers, outServers.Selected, dummy); {!!.06}
  Database := TffeDatabaseItem(GetSelectedEntity);
  Database.Database.Connected := True;

  ex := Database.Database.TableExists('sys$procedures');
  If ex Then
    If MessageDlg('Replace sys$procedures?',
      mtConfirmation, [mbYes, mbNo], 0) = mrNo Then Exit;

  Dict := TFSInfoDict.Create(4096);
  Try
    With Dict Do
      Begin
        fiValCheck.vdHasDefVal := True;
        FSStringToVCheckVal('A', fstShortString, fiValCheck.vdDefVal);
        AddField('ACTIVE', '', fstShortString, 1, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        FSStringToVCheckVal('NAME', fstShortString, fiValCheck.vdDefVal);
        AddField('NAME', '', fstShortString, 32, 0, True, @fiValCheck, blNone, '', rNone, True, duNormal);

        AddField('CODE', '', fstBlobMemo, sizeof(Longint), 0, False, Nil, blNone, '', rNone, False, duNormal);
        AddField('BLR', '', fstBlobMemo, sizeof(Longint), 0, False, Nil, blNone, '', rNone, False, duNormal);

        FSStringToVCheckVal('0', fstInt32, fiValCheck.vdDefVal);
        AddField('FLAGS', '', fstInt32, 10, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        AddField('DESCRIPTION', '', fstBlobMemo, sizeof(Longint), 0, False, Nil, blNone, '', rNone, False, duNormal);

        FSStringToVCheckVal('<NOW>', fstDateTime, fiValCheck.vdDefVal);
        AddField('DATECREATE', '', fstDateTime, 24, 0, True, @fiValCheck, blNone, '', rNone, False, duNormal);

        FldArray[0] := 0;
        FldArray[1] := 1;
        IdxHlprs[0] := '';
        IdxHlprs[1] := '';
        FieldsAscDesc[0] := 1;
        FieldsAscDesc[1] := 1;
        FieldsCase[0] := 1;
        FieldsCase[1] := 1;
        FieldsSize[0] := 0;
        FieldsSize[1] := 0;
        FieldsFlags[0] := 0;
        FieldsFlags[1] := 0;
        FieldsNullTop[0] := 1;
        FieldsNullTop[1] := 1;

        AddIndex('RELATION', '', 0, 2, FldArray, FieldsAscDesc, FieldsCase, FieldsSize,
          FieldsFlags, FieldsNullTop, IdxHlprs, True);

        FldArray[0] := 1;
        FieldsAscDesc[0] := 1;
        IdxHlprs[0] := '';
        FieldsCase[0] := 1;
        FieldsSize[0] := 0;
        FieldsFlags[0] := 0;
        FieldsNullTop[0] := 1;
        AddIndex('NAME', '', 0, 1, FldArray, FieldsAscDesc, FieldsCase, FieldsSize,
          FieldsFlags, FieldsNullTop, IdxHlprs, False);
      End;
    Dict.IsEncrypted := True;
    Dict.TableType := ttSystem;
    Dict.FileDescriptor[0].fdDesc := 'Procedure table';
    If Database.Database.CreateTable(True, 'sys$procedures', Dict) <> 0 Then
      Raise Exception.Create('Error creating table')
    Else
      LoadOutlineTables(outServers.Selected);
  Finally
    Dict.Free;
  End;
End;

Procedure TfrmMain.Changeclientport1Click(Sender: TObject);
Begin
  With fschangeport.TFSChangePortClient.Create(Self) Do
    Try
      If ShowModal = mrOK Then
        LoadOutlineServers;
    Finally
      Free;
    End;
End;

Procedure TfrmMain.Loadservers1Click(Sender: TObject);
Begin
  Try
    Initialized := False;
    Try
      LoadOutlineServers;
    Except
      LoadOutlineServers;
    End;
  Except
    On E: Exception Do
      ShowMessage(E.Message);
  End;
End;

Procedure TfrmMain.bb1Click(Sender: TObject);
Var
  aTable: TffeTableItem;
  Seed: Longint;
Begin
  aTable := TffeTableItem(GetSelectedEntity);
  Seed := aTable.GetMaxRecords;
  With aTable Do
    Begin
      If ShowMaxRecordsDlg(TableName, Seed) = mrOK Then
        SetMaxRecordsSeed(Seed);
    End;
End;

Procedure TfrmMain.Tableflags1Click(Sender: TObject);
Var
  aTable: TffeTableItem;
  r: Word;
Begin
  aTable := TffeTableItem(GetSelectedEntity);
  r := aTable.GetTableFlags;
  With aTable Do
    Begin
      If ShowTableFlagsDlg(TableName, r) = mrOK Then
        SetTableFlagsSeed(r);
    End;
End;

Procedure TfrmMain.S1Click(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  dummy: Boolean;
Begin
  outServersExpanding(outServers, outServers.Selected, dummy);
  Database := TffeDatabaseItem(GetSelectedEntity);
  If Not Database.database.Connected Then Database.database.Open;
  If Not Database.database.InTransaction Then
    Database.database.StartTransaction;
End;

Procedure TfrmMain.C1Click(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  dummy: Boolean;
Begin
  outServersExpanding(outServers, outServers.Selected, dummy);
  Database := TffeDatabaseItem(GetSelectedEntity);
  If Not Database.database.Connected Then Database.database.Open;
  If Database.database.InTransaction Then
    Database.database.Commit;
End;

Procedure TfrmMain.R1Click(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  dummy: Boolean;
Begin
  outServersExpanding(outServers, outServers.Selected, dummy);
  Database := TffeDatabaseItem(GetSelectedEntity);
  If Not Database.database.Connected Then Database.database.Open;
  If Database.database.InTransaction Then
    Database.database.Rollback;
End;

Procedure TfrmMain.S2Click(Sender: TObject);
Var
  aTable: TffeTableItem;
  Seed1, Seed2: String;
Begin
  aTable := TffeTableItem(GetSelectedEntity);
  With aTable Do
    Begin
      If ShowTablePasswd(Seed1, Seed2) = mrOK Then
        SetTablePassword(Seed1, Seed2);
    End;
End;

Procedure TfrmMain.S3Click(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  TableIndex: Longint;
  dummy: Boolean;
Begin
  { make sure tablelist is loaded; implicitly checks for valid directory }
  outServersExpanding(outServers, outServers.Selected, dummy); {!!.06}
  Database := TffeDatabaseItem(GetSelectedEntity);
  ShowTablesPasswd(Database.Server.Session.Passwords);
End;

Procedure TfrmMain.Passwords1Click(Sender: TObject);
Var
  Database: TffeDatabaseItem;
  Table: TffeTableItem;
Begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  ShowTablesPasswd(Database.Server.Session.Passwords);
End;

Procedure TfrmMain.Setpasswordrestructure1Click(Sender: TObject);
Var
  aTable: TffeTableItem;
  Seed1, Seed2: String;
Begin
  aTable := TffeTableItem(GetSelectedEntity);
  With aTable Do
    Begin
      If ShowTablePasswd(Seed1, Seed2) = mrOK Then
        SetTablePasswordRest(Seed1, Seed2);
    End;
End;

Procedure TfrmMain.OptimisticNoWait1Click(Sender: TObject);
Begin
  //tlOptimisticNoWait, tlOptimisticWait,tlPessimisticNoWait, tlPessimisticWait
  Case TMenuitem(Sender).tag Of
    1: OptimisticNoWait1.Checked := True;
    2: OptimisticWait1.Checked := True;
    3: PessimisticNoWait1.Checked := True;
    4: PessimisticWait1.Checked := True;
  End;
End;

Procedure TfrmMain.PackReindexUndeleteAll1Click(Sender: TObject);
Var
  aNode: TTreeNode;
  Status: TffRebuildStatus;
  RebuildDone: Boolean;
  Table: TffeTableItem;
  PromptMsg: String;
  StatusMsg: String;
Begin
  PromptMsg := 'Are you sure you want to recovery deleted records this table?'; {!!.10}
  StatusMsg := 'Recovery deleted records';

  aNode := outServers.Selected;
  Table := TffeTableItem(GetNodeEntity(aNode));
  Table.Table.Open;
  If Table.Table.Dictionary.EngineDeleteType = edtNotUndelete Then
    Begin
      Table.Table.Close;
      ShowMessage('Deleted records were not found!');
      Exit;
    End;

  Table.Table.Close;
  If MessageDlg(PromptMsg, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes Then
    Begin
      With Table Do
        Begin
          Pack(True, False);

          If Rebuilding Then
            Begin

              { Change the display in the outline; table will be unavailable
                until the rebuild is done. }
              aNode.Text := TableName + ' (' + StatusMsg + ')';
              Try
                Application.ProcessMessages;

                { Display the rebuild progress window }
                With TfrmRebuildStatus.Create(Nil) Do
                  Try
                    ShowProgress(StatusMsg, TableName); {!!.10}
                    Try
                      Repeat
                        CheckRebuildStatus(RebuildDone, Status);
                        If Not RebuildDone Then
                          Begin
                            UpdateProgress(RebuildDone, Status);
                            Sleep(250);
                          End;
                      Until RebuildDone;
                    Finally
                      Hide;
                    End;
                  Finally
                    Free;
                  End;
              Finally
                aNode.Text := TableName;
              End;
            End;
        End;
    End;
End;

Procedure TfrmMain.Recoveryonlydeletedrecords1Click(Sender: TObject);
Var
  aNode: TTreeNode;
  Status: TffRebuildStatus;
  RebuildDone: Boolean;
  Table: TffeTableItem;
  PromptMsg: String;
  StatusMsg: String;
Begin
  PromptMsg := 'Are you sure you want to recovery only deleted records this table?'; {!!.10}
  StatusMsg := 'Recovery only deleted records';

  aNode := outServers.Selected;
  Table := TffeTableItem(GetNodeEntity(aNode));
  Table.Table.Open;
  If Table.Table.Dictionary.EngineDeleteType = edtNotUndelete Then
    Begin
      Table.Table.Close;
      ShowMessage('Deleted records were not found!');
      Exit;
    End;

  Table.Table.Close;
  If MessageDlg(PromptMsg, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes Then
    Begin
      With Table Do
        Begin
          Pack(True, True);

          If Rebuilding Then
            Begin

              { Change the display in the outline; table will be unavailable
                until the rebuild is done. }
              aNode.Text := TableName + ' (' + StatusMsg + ')';
              Try
                Application.ProcessMessages;

                { Display the rebuild progress window }
                With TfrmRebuildStatus.Create(Nil) Do
                  Try
                    ShowProgress(StatusMsg, TableName); {!!.10}
                    Try
                      Repeat
                        CheckRebuildStatus(RebuildDone, Status);
                        If Not RebuildDone Then
                          Begin
                            UpdateProgress(RebuildDone, Status);
                            Sleep(250);
                          End;
                      Until RebuildDone;
                    Finally
                      Hide;
                    End;
                  Finally
                    Free;
                  End;
              Finally
                aNode.Text := TableName;
              End;
            End;
        End;
    End;
End;

procedure TfrmMain.About1Click(Sender: TObject);
Var
  ft: TfsAboutForm;
Begin
  Ft := TfsAboutForm.Create(Nil);
  Try
    Ft.ShowModal;
  Finally
    ft.free;
  End;
end;

End.

