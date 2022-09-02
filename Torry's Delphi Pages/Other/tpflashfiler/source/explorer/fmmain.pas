{*********************************************************}
{* FlashFiler Explorer Main Form                         *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit fmmain;

{$IFDEF SingleEXE}
!! Error: This application should not be compiled with SingleEXE mode enabled.
{$ENDIF}
interface

uses
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
  StdCtrls,
  DBGrids,
  DBCtrls,
  Grids,
  Outline,
  ExtCtrls,
  ComCtrls,
  ffdbbase,
  ffllbase,
  ffllprot,
  ffsrbde,
  uconfig,
  uentity,
  fflllgcy,
  ffdb,
  fflllog
{$IFDEF DCC4ORLATER}
  ,
  ImgList,
  ToolWin
{$ENDIF}
;

type
  TfrmMain = class(TForm)
    mnuMain: TMainMenu;
    mnuHelp: TMenuItem;
    mnuHelpAbout: TMenuItem;
    mnuServer: TMenuItem;
    N1: TMenuItem;
    mnuServerExit: TMenuItem;
    popmnuServer: TPopupMenu;
    popmnuServerAttach: TMenuItem;
    popmnuServerDetach: TMenuItem;
    popmnuAlias: TPopupMenu;
    popmnuTable: TPopupMenu;
    popmnuTableDefinition: TMenuItem;
    popmnuTableIndexes: TMenuItem;
    popmnuTableRedefine: TMenuItem;
    N2: TMenuItem;
    popmnuTableDelete: TMenuItem;
    popmnuTableRename: TMenuItem;
    popmnuTableNew: TMenuItem;
    popmnuDatabaseNew: TMenuItem;
    popmnuDatabaseDelete: TMenuItem;
    N3: TMenuItem;
    popmnuDatabaseRefresh: TMenuItem;
    pnlStatusContainer: TPanel;
    pnlStatusBarComment: TPanel;
    mnuServerRefresh: TMenuItem;
    popmnuServerNewDatabase: TMenuItem;
    N5: TMenuItem;
    popmnuDatabaseNewTable: TMenuItem;
    N6: TMenuItem;
    popmnuServerRefresh: TMenuItem;
    N7: TMenuItem;
    mnuOptions: TMenuItem;
    pnlBottomSpacer: TPanel;
    popmnuTableReindex: TMenuItem;
    mnuOptionsPrintSetup: TMenuItem;
    popmnuTableImportSchema: TMenuItem;
    mnuToolsFFComms: TMenuItem;
    popmnuDatabaseRename: TMenuItem;
    popmnuDatabaseImportSchema: TMenuItem;
    mnuHelpTopics: TMenuItem;
    N8: TMenuItem;
    mnuHelpWebSite: TMenuItem;
    mnuHelpEMail: TMenuItem;
    dlgPrinterSetup: TPrinterSetupDialog;
    mnuServerRegister: TMenuItem;
    popmnuServerRegister: TMenuItem;
    popmnuTableEmpty: TMenuItem;
    pnlLeft: TPanel;
    pnlLeftHeader: TPanel;
    lblFlashFilerServers: TLabel;
    mnuSetAutoInc: TMenuItem;
    mnuOptionsLiveDatasets: TMenuItem;
    logMain: TffEventLog;
    outServers: TTreeView;
    imgMain: TImageList;
    mnuDatabaseSQL: TMenuItem;
    mnuViewTable: TMenuItem;
    N4: TMenuItem;
    barToolBar: TToolBar;
    tbRefresh: TToolButton;
    tbServerRegister: TToolButton;
    N12: TToolButton;
    mnuWindows: TMenuItem;
    mnuCloseAll: TMenuItem;
    mnuWindowsSplitter: TMenuItem;
    tbOptionsLiveDataSets: TToolButton;
    tbOptionsPrintSetup: TToolButton;
    N11: TToolButton;
    tbCloseAll: TToolButton;
    N13: TToolButton;
    tbHelpTopics: TToolButton;
    tbHelpWebSite: TToolButton;
    tbHelpEMail: TToolButton;
    popmnuTableSQL: TMenuItem;
    mnuSetAsAutomaticDefault: TMenuItem;
    N9: TMenuItem;
    mnuTools: TMenuItem;
    tbFFComms: TToolButton;
    N10: TToolButton;
    popmnuTableReindexAll: TMenuItem;
    popmnuServerStatistics: TMenuItem;
    mnuOptionsSetDefaultTimeout: TMenuItem;
    N14: TMenuItem;

    procedure mnuHelpAboutClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mnuServerExitClick(Sender: TObject);
    procedure outServersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure mnuServerRefreshClick(Sender: TObject);
    procedure popmnuTableDefinitionClick(Sender: TObject);
    procedure popmnuTableNewClick(Sender: TObject);
    procedure popmnuDatabaseNewTableClick(Sender: TObject);
    procedure popmnuServerPopup(Sender: TObject);
    procedure popmnuServerDetachClick(Sender: TObject);
    procedure popmnuServerAttachClick(Sender: TObject);
    procedure popmnuTableDeleteClick(Sender: TObject);
    procedure popmnuTablePackClick(Sender: TObject);
    procedure popmnuTableRedefineClick(Sender: TObject);
    procedure popmnuTablePopup(Sender: TObject);
    procedure popmnuTableIndexesClick(Sender: TObject);
    procedure outServersClick(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure popmnuServerNewDatabaseClick(Sender: TObject);
    procedure popmnuTableReindexClick(Sender: TObject);
    procedure popmnuTableImportSchemaClick(Sender: TObject);
    procedure popmnuDatabaseImportSchemaClick(Sender: TObject);
    procedure mnuHelpWebSiteClick(Sender: TObject);
    procedure mnuHelpEMailClick(Sender: TObject);
    procedure popmnuDatabaseDeleteClick(Sender: TObject);
    procedure mnuOptionsPrintSetupClick(Sender: TObject);
    procedure popmnuDatabaseRenameClick(Sender: TObject);
    procedure mnuServerRegisterClick(Sender: TObject);
    procedure mnuHelpTopicsClick(Sender: TObject);
    procedure popmnuTableEmptyClick(Sender: TObject);
    procedure mnuSetAutoIncClick(Sender: TObject);
    procedure outServersDblClick(Sender: TObject);
    procedure mnuOptionsLiveDatasetsClick(Sender: TObject);
    procedure outServersExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure outServersEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure outServersEdited(Sender: TObject; Node: TTreeNode;
      var S: String);
    procedure outServersKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure RefreshServers(Sender : TObject);
      { Refresh the entire list of servers. }

    procedure RefreshDatabases(Sender : TObject);
      { Refresh a servers' list of databases. }

    procedure RefreshTables(Sender : TObject);
    procedure mnuDatabaseSQLClick(Sender: TObject);
    procedure mnuViewTableClick(Sender: TObject);
    procedure outServersCompare(Sender: TObject; Node1, Node2: TTreeNode;
      Data: Integer; var Compare: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mnuCloseAllClick(Sender: TObject);
    procedure mnuWindowsClick(Sender: TObject);
    procedure mnuToolsFFCommsClick(Sender: TObject);
    procedure mnuSetAsAutomaticDefaultClick(Sender: TObject);
    procedure outServersChange(Sender: TObject; Node: TTreeNode);
    procedure outServersContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure popmnuServerRefreshClick(Sender: TObject);
    procedure popmnuServerStatisticsClick(Sender: TObject);
    procedure mnuOptionsSetDefaultTimeoutClick(Sender: TObject);
      { Refresh a database's list of tables. }

  private
//    function mapProtocolClassToProtocol(const Protocol : TffCommsProtocolClass) : TffProtocolType;
    procedure WindowsMenuItemClick(Sender: TObject);                      {!!.06}
    procedure AppMessage(var Msg: TMsg; var Handled: Boolean);            {!!.06}
    procedure DoIdle(Sender: TObject; var Done: Boolean);
    procedure ApplicationEvents1Exception(Sender: TObject; E: Exception);
    procedure ShowServerStatistics(aServer: TffeServerItem);                 {!!.06}
  protected
    Initialized: Boolean;
    {- True if the (DB) Session has been started }

    function GetNewSelectedNode(aNode : TTreeNode) : TTreeNode;
    {- Assuming aNode is going to be deleted, determines which node should be
       selected after the deletion. }

  public
    function AddOutlineDatabase(aNode : TTreeNode;
                                aDatabase: TffeDatabaseItem) : TTreeNode;
    {- Adds a database entry to the outline.  Returns outline index of new entry}
    procedure AddOutlineServer(aServer : TffeServerItem);
    {- Adds a server entry to the outline.  Returns outline index of new entry}
    procedure AddOutlineTable(aNode : TTreeNode; aTable : TffeTableItem);
    {- Adds a table entry to the outline.  Returns outline index of new entry}
    procedure DeleteNodeChildren(aNode : TTreeNode);
    {- Deletes all the children for a given outline entry }
    function DoAttach(aNode : TTreeNode) : TffResult;                  {!!.01}
    {- Attach to the given server }
    procedure DoDetach;
    {- Detach from given server }
    procedure EnableScreen(aSwitch: Boolean);
    {- Enables/Disables the main screen controls while a process runs; allows
       main form to be minimized}
    function GetEntityNode(aEntityType : TffeEntityType;
                           anEntity : TffeEntityItem): TTreeNode;
    {- Returns the node for the given entity }
    function GetNodeEntity(aNode : TTreeNode) : TffeEntityItem;
    {- Returns the entity associated with a given node. }
    function GetNodeEntityType(aNode : TTreeNode) : TffeEntityType;
    {- Returns the entity type associated with a given node. }
    function GetSelectedEntity : TffeEntityItem;
    {- Returns the entity for the currently selected outline item. }
    procedure Initialize;
    {- Initial setup of the session and the server list }
    procedure LoadConfig;
    {- Read parameters out of persistent configuration storage }
    procedure LoadOutlineServers;
    {- Refreshes the entire outline view }
    procedure LoadOutlineDatabases(aNode : TTreeNode);
    {- Refreshes the outline view (databases, tables) for the given server}
    procedure LoadOutlineTables(aNode : TTreeNode);
    {- For a given database entry in the outline, load all of its member
       tables into the outline.  aNode may point to a table or
       a database entry in the outline. }
    procedure OutlineClear;
    {- Frees the TffeOutlineData instances attached to the TTreeNodes in
       outServers.  Clears the TTreeView. }
    procedure SaveConfig;
    {- Writes the FFE configuration settings to persistent storage}
    procedure ShowQueryWindow(aDatabaseIndex: LongInt);
    {- Creates a modeless query window for a particular database. }
    procedure ShowTableBrowser(aTable : TffeTableItem);
    {- Creates a modeless table browser for a particular table. }
    procedure slServerAttach(aServerIndex: LongInt);
    {- event-handler for server attaches}
    procedure StatusComment(aMsg: string);
    {- Displays a message in the status bar}
    procedure UncheckMenuGroupSiblings(aMenuItem: TMenuItem);
    {- Unchecks all the menu items in the same menu group as the given item
       (primary for compatibility with Delphi 1) }
    procedure UpdateWindowsMenuItems;                                 {!!.06}
    {- populates the Windows menu with the current
       table- and SQL-browser windows }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  {$IFDEF USETeDEBUG}
  jcldebug,
  {$ENDIF}
  ffclbase,                                                            {!!.07}
  ffllcomm,                                                            {!!.07}
  ffclreng,                                                            {!!.07}
  ffclcfg,                                                             {!!.07}
  ffutil,
  uFFComms,                                                            {!!.07}
  {$IFDEF DCC6OrLater}
  Types,                                                               {!!.07}
  {$ENDIF}
  ffabout,
  ubase,
  uconsts,
  dgaddals,
  dgimport,
  dgregsrv,
  dgselidx,
  ffllexcp,                                                            {!!.01}
  fmprog,
  fmstruct,
  dgautoin,
  dgtable,
  dgquery,
  dgServSt;                                                            {!!.11}

{$R *.DFM}

const
  { Outline levels for schema entities }
  lvServer   = 1;
  lvDatabase = 2;
  lvTable    = 3;

{===TffeOutlineData==================================================}
type
  { This is the data kept by each outline entry to refer it to
    the underlying data structure. }
  TffeOutlineData = class
    public
      EntityType: TffeEntityType;
      Entity : TffeEntityItem;
      constructor Create(aEntityType: TffeEntityType; anEntity : TffeEntityItem);
  end;


constructor TffeOutlineData.Create(aEntityType: TffeEntityType;
                                   anEntity : TffeEntityItem);
begin
  inherited Create;
  EntityType := aEntityType;
  Entity := anEntity;
end;
{====================================================================}

{===TfrmMain=========================================================}
function TfrmMain.AddOutlineDatabase(aNode : TTreeNode;
                                     aDatabase : TffeDatabaseItem) : TTreeNode;
var
  OutlineData: TffeOutlineData;
begin
  Result := nil;
  OutlineData := TffeOutlineData.Create(etDatabase, aDatabase);
  with outServers do
    with TffeOutlineData(aNode.Data) do
      case EntityType of
        etServer:
          Result := Items.AddChildObject(aNode, aDatabase.DatabaseName,
                                         OutlineData);
        etDatabase:
          Result := Items.AddObject(aNode, aDatabase.DatabaseName,
                                    OutlineData);
      end;
  if assigned(Result) then begin
    Result.ImageIndex := pred(lvDatabase);
    Result.SelectedIndex := Result.ImageIndex;
    Result.HasChildren := True;
  end;
  outServers.AlphaSort;
end;
{--------}
procedure TfrmMain.AddOutlineServer(aServer : TffeServerItem);
var
  Node : TTreeNode;
  OutlineData: TffeOutlineData;
  aProtocol : TffCommsProtocolClass;
  aProtocolName : TffShStr;


  {Begin !!.07}
  { removes leading zeroes in order to compare ip addresses
    like 192.000.001.001 against 192.0.1.1 - necessary because
    FFCOMMS might register addresses with extra 0's }
  function StripLeadingZeros(servername : String) : String;
  var
    s : String;
  begin
    Result := '';
    { while characters in string do }
    while (Length(servername)>0) do begin
      { if first char not a number}
      if NOT (servername[1] IN ['0'..'9']) then begin
        { move char to result }
        Result := Result + servername[1];
        Delete(servername, 1, 1);
      end
      else begin
        s := '';
        { collect numbers up to next non-numerical char }
        while (Length(servername)>0) and (servername[1] IN ['0'..'9']) do begin
          s := s + servername[1];
          Delete(servername, 1, 1);
        end;
        { strip leading zeroes and add to Result }
        Result := Result + IntToStr(StrToInt(s));
      end;
    end;
  end;
  {End !!.07}

begin
  OutlineData := TffeOutlineData.Create(etServer, aServer);
  with outServers do
    Node := Items.AddObject(outServers.TopItem, aServer.ServerName, OutlineData);
  if assigned(Node) then begin
    {Begin !!.07}
    { check if the server is the default for the workstation
      and use a different glyph if so }
    FFClientConfigReadProtocol(aProtocol, aProtocolName);
    if (FFGetProtocolString(aServer.Protocol)=aProtocolName) and
       ((aServer.Protocol=ptSingleUser) or
       (StripLeadingZeros(FFClientConfigReadServerName)=StripLeadingZeros(aServer.ServerName))) then begin
      Node.ImageIndex := 12;
    end
    else
    {End !!.07}
      Node.ImageIndex := pred(lvServer);
    Node.SelectedIndex := Node.ImageIndex;
    Node.HasChildren := True;
  end;
  outServers.AlphaSort;
end;
{--------}
procedure TfrmMain.AddOutlineTable(aNode : TTreeNode; aTable : TffeTableItem);
var
  Node : TTreeNode;
  OutlineData: TffeOutlineData;
begin
  Node := nil;
  OutlineData := TffeOutlineData.Create(etTable, aTable);
  with outServers do
    with TffeOutlineData(aNode.Data) do
      case EntityType of
        etDatabase:
          Node := Items.AddChildObject(aNode, aTable.TableName, OutlineData);
        etTable:
          Node := Items.AddObject(aNode, aTable.TableName, OutlineData);
      end;
  if assigned(Node) then begin
    Node.ImageIndex := pred(lvTable);
    Node.SelectedIndex := Node.ImageIndex;
    Node.HasChildren := False;
  end;
  outServers.AlphaSort;
end;
{--------}
procedure TfrmMain.DeleteNodeChildren(aNode : TTreeNode);
var
  aChild : TTreeNode;
begin
  with outServers do begin
    Items.BeginUpdate;
    try
      with aNode do begin
        aChild := GetFirstChild;
        while assigned(aChild) do begin
          if assigned(aChild.Data) then begin
            DeleteNodeChildren(aChild);
            TffeOutlineData(aChild.Data).free;
          end;
          aChild := GetNextChild(aChild);
        end;
      end;
      aNode.DeleteChildren;
    finally
      Items.EndUpdate;
    end;
  end;
end;
{--------}
function TfrmMain.DoAttach(aNode : TTreeNode) : TffResult;             {!!.01}
var
  aServer : TffeServerItem;
begin
  aServer := TffeServerItem(TffeOutlineData(aNode.Data).Entity);
  try
    Result := aServer.Attach(logMain);                                 {!!.01}
    if Result = DBIERR_NONE then begin                                 {!!.01}
      LoadOutlineDatabases(aNode);                                     {!!.01}
      Config.LastServer := aServer.ServerName;                         {!!.01}
    end;                                                               {!!.01}
  except
    on E: EffDatabaseError do begin                                    {!!.01}
      if E.ErrorCode = 11278 then
        raise EffDatabaseError.CreateFmt('Unable to connect. "%S" is currently unavailable',
                                         [aServer.EntityName])
      else
        raise;
    end;                                                               {!!.01}
  end;
end;
{--------}
procedure TfrmMain.DoDetach;
var
  aServer : TffeServerItem;
begin
  aServer := TffeServerItem(GetSelectedEntity);
  if assigned(aServer) then begin
    outServers.Selected.Collapse(True);
    DeleteNodeChildren(outServers.Selected);
    aServer.Detach;
    outServers.Selected.HasChildren := True;
  end;
end;
{--------}
procedure TfrmMain.EnableScreen(aSwitch: Boolean);
begin
  if aSwitch then Application.ProcessMessages;
  mnuServer.Enabled := aSwitch;
  mnuOptions.Enabled := aSwitch;
end;
{--------}
function TfrmMain.GetEntityNode(aEntityType: TffeEntityType;
                                anEntity: TffeEntityItem): TTreeNode;
var
  I : longInt;
begin
  Result := nil;
  with outServers do
    for I := 0 to pred(Items.Count) do
      with TffeOutlineData(Items[I].Data) do
        if (EntityType = aEntityType) and
           (Entity = anEntity) then begin
          Result := Items[I];
          Break;
        end;
end;
{--------}
function TfrmMain.GetNodeEntity(aNode : TTreeNode) : TffeEntityItem;
begin
  Result := TffeOutlineData(aNode.Data).Entity;
end;
{--------}
function TfrmMain.GetNodeEntityType(aNode : TTreeNode) : TffeEntityType;
begin
  Result := TffeOutlineData(aNode.Data).EntityType;
end;
{--------}
function TfrmMain.GetSelectedEntity : TffeEntityItem;
begin
  Result := TffeOutlineData(outServers.Selected.Data).Entity;
end;
{--------}
procedure TfrmMain.Initialize;
begin
  try
    Initialized := False;
    if not assigned(ServerList) then begin
      ServerList := TffeServerList.Create(logMain);
      ServerList.OnAttach := slServerAttach;
    end;
    LoadOutlineServers;
  except
    on E:Exception do
      showMessage(E.Message);
  end;
end;
{--------}
procedure TfrmMain.LoadConfig;
begin
  { Set window coordinates }
  WindowState := Config.WindowState;
  if (WindowState <> wsMaximized) and (Config.Window.Bottom <> 0) then
    with Config do begin
      Left   := Window.Left;
      Top    := Window.Top;
      Width  := Window.Right - Config.Window.Left;
      Height := Window.Bottom - Config.Window.Top;
    end;
  mnuOptionsLiveDataSets.Checked := coLiveDatasets in Config.Options;
  tbOptionsLiveDataSets.Down := mnuOptionsLiveDataSets.Checked;    {!!.06}
end;
{--------}
procedure TfrmMain.OutlineClear;
var
  Index : longInt;
begin
  { Free the TffeOutlineData structures associated with the nodes. }
  with outServers do begin
    for Index := 0 to pred(Items.Count) do
      if assigned(Items[Index].Data) then
        TffeOutlineData(Items[Index].Data).Free;
  end;
  outServers.Items.Clear;
end;
{--------}
procedure TfrmMain.LoadOutlineServers;
var
  aNode : TTreeNode;
  Server : TffeServerItem;
  S : LongInt;
  DefaultServerName: TffNetAddress;
  OldCursor: TCursor;
begin

  OutlineClear;

  { Load up the registered servers into the outline }
  StatusComment('Searching for active FlashFiler servers...');
  mnuServer.Enabled := False;
  outServers.Enabled := False;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    ServerList.Load;

    { Load up all the servers into the outline }
    for S := 0 to ServerList.Count - 1 do
      AddOutlineServer(ServerList.Items[S]);

    { Find the default server }
    DefaultServerName := Config.LastServer;
    if DefaultServerName <> '' then begin
      S := ServerList.IndexOfName(DefaultServerName);
      if S <> -1 then begin
        Server := ServerList.Items[S];
        aNode := GetEntityNode(etServer, Server);
{Begin !!.01}
        { Attached to server? }
        if DoAttach(aNode) = DBIERR_NONE then
          { Expand the attached server.  If the server has only one
            database then expand the database too. }
          aNode.Expand(Server.DatabaseCount = 1);
{End !!.01}
      end;
    end;
    outServers.AlphaSort;
  finally
    Screen.Cursor := OldCursor;
    outServers.Invalidate;
    StatusComment('');
    if outServers.Items.Count = 0 then
      StatusComment('No active FlashFiler servers found.');
    Screen.Cursor := OldCursor;
    mnuServer.Enabled := True;
    outServers.Enabled := True;
  end;
end;
{--------}
procedure TfrmMain.LoadOutlineDatabases(aNode : TTreeNode);
{ For a given server entry in the outline, load all of its member
  databases into the outline }
var
  D : longInt;
  Server : TffeServerItem;
begin

  Server := TffeServerItem(TffeOutlineData(aNode.Data).Entity);

  if (not Server.Attached) then
    if DoAttach(aNode) <> DBIERR_NONE then                             {!!.01}
      Exit;                                                            {!!.01}

  { Delete all the children of this server }
  DeleteNodeChildren(aNode);

  { Load the databases into the outline; we assume the server's database list &
    table list have already been populated. }
  for D := 0 to pred(Server.DatabaseCount) do
    AddOutlineDatabase(aNode, Server.Databases[D]);

  outServers.AlphaSort;
end;
{--------}
procedure TfrmMain.LoadOutlineTables(aNode : TTreeNode);
var
  Database : TffeDatabaseItem;
  T: LongInt;
begin
  { If we're pointing to a table entry, kick up to the table's
    database entry }
  with TffeOutlineData(aNode.Data) do
    if EntityType = etTable then begin
      aNode := aNode.Parent;
      outServers.Selected := aNode;
    end;
  Database := TffeDatabaseItem(TffeOutlineData(aNode.Data).Entity);

  outServers.Items.BeginUpdate;
  try
    { Delete all the children of this database }
    DeleteNodeChildren(aNode);

    { Load the database's tables. }
    Database.LoadTables;

    { Load the database's tables into the outline }
      for T := 0 to pred(Database.TableCount) do
        AddOutlineTable(aNode, Database.Tables[T]);
    outServers.AlphaSort;    
  finally
    outServers.Items.EndUpdate;
  end;

end;
{--------}
procedure TfrmMain.SaveConfig;
begin
  if Assigned(Config) then begin
    with Config do begin
      Window := Bounds(Left, Top, Width, Height);
      Options := [];
    end;
    Config.WindowState := WindowState;
    Config.Options := [];
    if mnuOptionsLiveDataSets.Checked then
      Config.Options := [coLiveDataSets];

    Config.Save;
  end;
end;
{--------}
procedure TfrmMain.ShowQueryWindow(aDatabaseIndex : LongInt);
var
  dummy: Boolean;
begin
  { implicitly check valid directory }
  outServersExpanding(outServers, outServers.Selected, dummy);                    {!!.07}
  with TdlgQuery.create(nil) do begin
    {Begin !!.07}
    { If we're pointing to a table entry, get the table's
      database entry from the parent }
    if TffeOutlineData(outServers.Selected.Data).EntityType = etTable then begin
      DatabaseItem := TffeDatabaseItem(TffeOutlineData(outServers.Selected.Parent.Data).Entity);
      ServerName := outServers.Selected.Parent.Parent.Text;
      DatabaseName := outServers.Selected.Parent.Text;
      Protocol := TffeDatabaseItem(TffeOutlineData(outServers.Selected.Parent.Data).Entity).Server.Protocol;
      InitialStatement := 'SELECT * FROM ' +
                          TffeTableItem(TffeOutlineData(outServers.Selected.Data).Entity).TableName;
      with TffexpSession(TffeDatabaseItem(TffeOutlineData(outServers.Selected.Parent.Data).Entity).Database.Session) do begin
        Password := ffePassword;
        UserName := ffeUserName;
      end;
    end
    else
    begin
      DatabaseItem := TffeDatabaseItem(GetSelectedEntity);
      ServerName := outServers.Selected.Parent.Text;
      DatabaseName := outServers.Selected.Text;
      Protocol := TffeDatabaseItem(GetSelectedEntity).Server.Protocol;
      with TffexpSession(TffeDatabaseItem(GetSelectedEntity).Database.Session) do begin
        Password := ffePassword;
        UserName := ffeUserName;
      end;
    end;
    {End !!.07}
    Log := LogMain;                                                    {!!.02}
    Show;
  end;
end;
{--------}
procedure TfrmMain.ShowTableBrowser(aTable : TffeTableItem);
var
  OldCursor: TCursor;
  aTableDlg : TdlgTable;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    aTableDlg := TdlgTable.Create(Application);                       {!!.02}
    with aTableDlg do begin
      TableItem := aTable;                                            {!!.10}
      Protocol := aTable.Server.Protocol;                             {!!.07}
      ServerName := aTable.Server.ServerName;
      DatabaseName := aTable.Database.DatabaseName;
      TableName := aTable.TableName;
      UserName := TffexpSession(aTable.Table.Session).ffeUserName;
      Password := TffexpSession(aTable.Table.Session).ffePassword;
      ReadOnly := (not mnuOptionsLiveDataSets.Checked);
      Log := LogMain;                                                  {!!.02}
      Show;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;
{--------}
procedure TfrmMain.slServerAttach(aServerIndex: LongInt);
begin
  StatusComment('');
end;
{--------}
procedure TfrmMain.StatusComment(aMsg: string);
begin
  pnlStatusBarComment.Caption := ' ' + aMsg;
  Application.ProcessMessages;
end;
{====================================================================}

{===Form-level event handlers========================================}
procedure TfrmMain.ApplicationEvents1Exception(Sender: TObject; E: Exception);
{$IFDEF USETeDEBUG}
var
  i : Integer;
  sl : TSTringList;
{$ENDIF}
begin
  {$IFDEF USETeDEBUG}
  sl := TSTringList.Create;
  try
    sl.Add(E.Message);
    if JclLastExceptStackList <> nil then
      JclLastExceptStackList.AddToStrings(sl);
    for i := 0 to sl.Count-1 do
      logMain.WriteString(sl[i]);
    Application.ShowException(E);
  finally
    sl.Free;
  end;
  {$ELSE}
  Application.ShowException(E);
  {$ENDIF}
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  { write log to app directory }
  logMain.FileName := Config.WorkingDirectory + ChangeFileExt(ExtractFileName(Application.ExeName), '.LOG');      {!!.11}
  Application.OnException := ApplicationEvents1Exception;
  HelpContext := hcMainOutline;
  Initialized := False;

  if FileExists(ExtractFilePath(ParamStr(0)) + 'FFE.HLP') then
    Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'FFE.HLP'
  else
    Application.HelpFile := ExtractFilePath(ParamStr(0)) + '..\HELP\FFE.HLP';

  mnuOptionsLiveDataSets.Checked := True;
  tbOptionsLiveDataSets.Down := mnuOptionsLiveDataSets.Checked;    {!!.06}

  LoadConfig;

  Application.OnMessage := AppMessage;
  Application.OnIdle := DoIdle;
end;
{Begin !!.02}
{--------}
procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  Idx : Integer;
begin
  for Idx := 0 to Pred(Screen.FormCount) do
    if (Screen.Forms[Idx] is TdlgTable) or
       (Screen.Forms[Idx] is TdlgQuery) or
       (Screen.Forms[Idx] is TdlgServerStats) then                 {!!.11}
    Screen.Forms[Idx].Close;
end;
{End !!.02}
{--------}
procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  ClosingApp := True;
  outServers.onClick := nil;
  ServerList.Free;
  SaveConfig;
  OutlineClear;
end;
{====================================================================}

{===Server menu event handlers=======================================}
procedure TfrmMain.mnuServerRefreshClick(Sender: TObject);
begin
  LoadOutlineServers;
end;
{--------}
procedure TfrmMain.mnuServerRegisterClick(Sender: TObject);
begin
  if ShowRegisteredServersDlg = mrOK then
    LoadOutlineServers;
end;
{--------}
procedure TfrmMain.mnuServerExitClick(Sender: TObject);
begin
  Close;
end;

{ "Options" menu event-handlers }

procedure TfrmMain.mnuOptionsPrintSetupClick(Sender: TObject);
begin
  dlgPrinterSetup.Execute;
end;
{====================================================================}

{===Help menu event handlers=========================================}
procedure TfrmMain.mnuHelpTopicsClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_FINDER, 0);
end;
{--------}
procedure TfrmMain.mnuHelpAboutClick(Sender: TObject);
var
  AboutBox : TFFAboutBox;
begin
  AboutBox := TFFAboutBox.Create(Application);
  try
    AboutBox.Caption := 'About FlashFiler Explorer';
    AboutBox.ProgramName.Caption := 'FlashFiler Explorer';
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;
{--------}
procedure TfrmMain.mnuHelpWebSiteClick(Sender: TObject);
begin
  ShellToWWW;
end;
{--------}
procedure TfrmMain.mnuHelpEMailClick(Sender: TObject);
begin
  ShellToEMail;
end;
{====================================================================}

{===Server outline event handlers====================================}
procedure TfrmMain.outServersClick(Sender: TObject);
{ Set the popup menu depending on which level we are on }
begin
  with outServers do begin
    if assigned(Selected) then
      case TffeOutlineData(Selected.Data).EntityType of
        etServer:
          begin
            PopupMenu := popmnuServer;
          end;
        etDatabase:
          begin
            PopupMenu := popmnuAlias;
          end;
        etTable:
          begin
            PopupMenu := popmnuTable;
          end;
      end;
  end;
end;
{--------}
procedure TfrmMain.outServersCompare(Sender: TObject; Node1,
  Node2: TTreeNode; Data: Integer; var Compare: Integer);
begin
  Compare := FFAnsiCompareText(Node1.Text, Node2.Text);               {!!.07}
end;
{--------}
procedure TfrmMain.outServersMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  aNode : TTreeNode;
begin
  if Button = mbRight then begin
    aNode := outServers.GetNodeAt(X,Y);
    if assigned(aNode) and assigned(aNode.Data) then begin
      outServers.Selected := aNode;
      case TffeOutlineData(aNode.Data).EntityType of
        etServer:   PopupMenu := popmnuServer;
        etDatabase: PopupMenu := popmnuAlias;
        etTable:    PopupMenu := popmnuTable;
      end;
      PopupMenu.Popup(ClientToScreen(Point(X, Y)).X + 5,
                      ClientToScreen(Point(X, Y)).Y + 5);
    end;
  end;
end;
{====================================================================}

{===Server outline context menus event handlers======================}
procedure TfrmMain.popmnuServerPopup(Sender: TObject);
var
  Entity : TffeEntityItem;
begin
  Entity := TffeOutlineData(outServers.Selected.Data).Entity;
  popmnuServerAttach.Enabled := not TffeServerItem(Entity).Attached;
  popmnuServerDetach.Enabled := not popmnuServerAttach.Enabled;
  popmnuServerNewDatabase.Enabled := not popmnuServerAttach.Enabled;
end;
{--------}
procedure TfrmMain.popmnuServerAttachClick(Sender: TObject);
var
  aNode : TTreeNode;
  Server : TffeServerItem;
begin

  aNode := outServers.Selected;
{Begin !!.01}
  if DoAttach(aNode) = DBIERR_NONE then begin
    Server := TffeServerItem(GetSelectedEntity);

    { Expand the attached server.  If it has only one database then expand
      the database too. }
    aNode.Expand(Server.DatabaseCount = 1);
  end;
{End !!.01}
end;
{--------}
procedure TfrmMain.popmnuServerDetachClick(Sender: TObject);
begin
  DoDetach;
end;
{--------}
procedure TfrmMain.RefreshServers(Sender: TObject);
begin
  LoadOutlineServers;
end;
{--------}
procedure TfrmMain.RefreshDatabases(Sender: TObject);
var
  aNode : TTreeNode;
  OldCursor : TCursor;
  Server : TffeServerItem;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  outServers.Items.BeginUpdate;
  try
    { Get the server. }
    aNode := outServers.Selected;
    Server := TffeServerItem(GetNodeEntity(aNode));
    Server.LoadDatabases;
    LoadOutlineDatabases(aNode);
    aNode.Expand(False);
  finally
    outServers.Items.EndUpdate;
    Screen.Cursor := OldCursor;
  end;
end;
{--------}
procedure TfrmMain.RefreshTables(Sender: TObject);
var
  aNode : TTreeNode;
  Database : TffeDatabaseItem;
  OldCursor : TCursor;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  outServers.Items.BeginUpdate;
  try
    { Get the database. }
    aNode := outServers.Selected;
    Database := TffeDatabaseItem(GetNodeEntity(aNode));
    Database.LoadTables;
    LoadOutlineTables(aNode);
    aNode.Expand(True);
  finally
    outServers.Items.EndUpdate;
    Screen.Cursor := OldCursor;
  end;
end;
{--------}
procedure TfrmMain.popmnuDatabaseNewTableClick(Sender: TObject);
var
  Database : TffeDatabaseItem;
  TableIndex: LongInt;
  dummy : Boolean;
begin
  { make sure tablelist is loaded; implicitly checks for valid directory }
  outServersExpanding(outServers, outServers.Selected, dummy);              {!!.06}
  Database := TffeDatabaseItem(GetSelectedEntity);
  with outServers do
    if ShowCreateTableDlg(Database, TableIndex, nil) = mrOK then begin
      LoadOutlineTables(Selected);
      Selected.Expand(False);
    end;
end;
{--------}
procedure TfrmMain.popmnuServerNewDatabaseClick(Sender: TObject);
var
  aDatabase : TffeDatabaseItem;
  anEntity : TffeEntityItem;
  aNode : TTreeNode;
  Server : TffeServerItem;
begin
  aDatabase := nil;
  Server := nil;
  aNode := outServers.Selected;
  anEntity := TffeOutlineData(aNode.Data).Entity;
  case anEntity.EntityType of
    etServer :
      Server := TffeServerItem(anEntity);
    etDatabase :
      begin
        aNode := aNode.Parent;
        Server := TffeServerItem(TffeOutlineData(aNode.Data).Entity);
      end;
  end;

  with outServers do begin
    if ShowAddAliasDlg(Server, aDatabase) = mrOK then
      LoadOutlineTables
        (AddOutlineDatabase(aNode, aDatabase));
    AlphaSort;
  end;
end;
{--------}
function TfrmMain.GetNewSelectedNode(aNode : TTreeNode) : TTreeNode;
begin
  { Does the node have a previous sibling? }
  Result := aNode.Parent.GetPrevChild(aNode);
  if not assigned(Result) then begin
    { No previous sibling.  See if has next sibling. }
    Result := aNode.Parent.GetNextChild(aNode);
    if not assigned(Result) then
      { No siblings.  Default to parent node. }
      Result := aNode.Parent;
  end;
end;
{--------}
procedure TfrmMain.popmnuDatabaseDeleteClick(Sender: TObject);
var
  aNode : TTreeNode;
  Database : TffeDatabaseItem;
begin
  Database := TffeDatabaseItem(GetSelectedEntity);
  if MessageDlg('Delete ' + Database.DatabaseName + '?',
                mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    Screen.Cursor := crHourglass;
    try

      { Delete the database from the server. }
      Database.Server.DropDatabase(Database.DatabaseName);

      { Delete database from outline }
      with outServers do begin
        aNode := Selected;
        if assigned(aNode.Data) then
          TffeOutlineData(aNode.Data).free;
        Selected := GetNewSelectedNode(aNode);
        Items.Delete(aNode);
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;
{--------}
procedure TfrmMain.popmnuDatabaseRenameClick(Sender: TObject);
begin
  outServers.Selected.EditText;
end;
{--------}
procedure TfrmMain.popmnuDatabaseImportSchemaClick(Sender: TObject);
var
  Database : TffeDatabaseItem;
  TableIndex: LongInt;
  dummy : Boolean;
begin
  outServersExpanding(outServers, outServers.Selected, dummy);
  TableIndex := -1;
  Database := TffeDatabaseItem(GetSelectedEntity);
  with outServers do begin
    ShowImportDlg(Database, TableIndex);
    if TableIndex <> -1 then {we have a new table}
      AddOutlineTable(Selected, Database.Tables[TableIndex]);
  end;
end;
{--------}
procedure TfrmMain.popmnuTablePopup(Sender: TObject);
var
  Table : TffeTableItem;
  I: Integer;
begin
  Table := TffeTableItem(GetSelectedEntity);
  with Table do
    with popmnuTable do begin
      if Rebuilding then begin
        for I := 0 to Items.Count - 1 do
          Items[I].Enabled := False;
        popmnuTableNew.Enabled := True;
      end
      else
        for I := 0 to Items.Count - 1 do
          Items[I].Enabled := True;
    end;
end;
{--------}
procedure TfrmMain.popmnuTableDefinitionClick(Sender: TObject);
var
  Database : TffeDatabaseItem;
  Table    : TffeTableItem;
begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  ShowViewTableStructureDlg(Database, Database.IndexOf(Table), vtViewFields);
end;
{--------}
procedure TfrmMain.popmnuTableIndexesClick(Sender: TObject);
var
  Database : TffeDatabaseItem;
  Table    : TffeTableItem;
begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  ShowViewTableStructureDlg(Database, Database.IndexOf(Table), vtViewIndexes);
end;
{--------}
procedure TfrmMain.popmnuTableNewClick(Sender: TObject);
var
  Database : TffeDatabaseItem;
  Table    : TffeTableItem;
  TableIndex : longInt;
begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  TableIndex := Database.IndexOf(Table);
  with outServers do
    if ShowCreateTableDlg(Database, TableIndex, nil) = mrOK then
      LoadOutlineTables(outServers.Selected);
//      AddOutlineTable(Selected, Table);
end;
{--------}
procedure TfrmMain.popmnuTableDeleteClick(Sender: TObject);
var
  aNode : TTreeNode;
  Table : TffeTableItem;
begin
  Table := TffeTableItem(GetSelectedEntity);
  if MessageDlg(Format('Delete table %s?', [Table.TableName]),
                mtConfirmation,
                [mbYes, mbNo],
                0) = mrYes then begin
    Screen.Cursor := crHourglass;
    try
      Table.Database.DropTable(Table.Database.IndexOf(Table));

      { Remove table from tree view. }
      with outServers do begin
        aNode := Selected;
        if assigned(aNode.Data) then
          TffeOutlineData(aNode.Data).free;
        Selected := GetNewSelectedNode(aNode);
        aNode.Delete;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;
{--------}
procedure TfrmMain.popmnuTablePackClick(Sender: TObject);
var
  aNode : TTreeNode;
  Status: TffRebuildStatus;
  RebuildDone: Boolean;
  Table : TffeTableItem;
  PromptMsg : string;                                                  {!!.10}
  StatusMsg : string;                                                  {!!.10}
begin
  PromptMsg := 'Are you sure you want to pack/reindex this table?';    {!!.10}
  StatusMsg := 'Packing';                                              {!!.10}

  if MessageDlg(PromptMsg, mtConfirmation,                             {!!.10}
                [mbYes, mbNo], 0) = mrYes then begin

    aNode := outServers.Selected;
    Table := TffeTableItem(GetNodeEntity(aNode));

    with Table do begin
      Pack;

      if Rebuilding then begin

        { Change the display in the outline; table will be unavailable
          until the rebuild is done. }
        aNode.Text := TableName + ' (packing)';
        try
          Application.ProcessMessages;

          { Display the rebuild progress window }
          with TfrmRebuildStatus.Create(nil) do
          try
            ShowProgress(StatusMsg, TableName);                        {!!.10}
            try
              repeat
                CheckRebuildStatus(RebuildDone, Status);
                if not RebuildDone then begin
                  UpdateProgress(RebuildDone, Status);
                  Sleep(250);
                end;
              until RebuildDone;
            finally
              Hide;
            end;
          finally
            Free;
          end;
        finally
          aNode.Text := TableName;
        end;
      end;
    end;
  end;
end;
{--------}
procedure TfrmMain.popmnuTableReindexClick(Sender: TObject);
var
  aNode : TTreeNode;
  IndexNum: Integer;
  RebuildDone: Boolean;
  Status: TffRebuildStatus;
  Table : TffeTableItem;
begin
  Table := TffeTableItem(GetSelectedEntity);
  if SelectIndexDlg(Table, IndexNum) = mrOk then begin
    aNode := outServers.Selected;

    with Table do begin
      Reindex(IndexNum);

      { Change the display in the outline; table will be unavailable
        until the rebuild is done. }
      aNode.Text := TableName + ' (reindexing)';
      try
        Application.ProcessMessages;

        { Display the rebuild progress window }
        with TfrmRebuildStatus.Create(nil) do
        try
          ShowProgress('Reindexing', TableName);
          try
            repeat
              CheckRebuildStatus(RebuildDone, Status);
              if not RebuildDone then begin
                UpdateProgress(RebuildDone, Status);
                Sleep(250);
              end;
            until RebuildDone;
          finally
            Hide;
          end;
        finally
          Free;
        end;
      finally
        aNode.Text := TableName;
      end;
    end;
  end;
end;
{--------}
procedure TfrmMain.popmnuTableRedefineClick(Sender: TObject);
var
  aNode : TTreeNode;
  Status: TffRebuildStatus;
  RebuildDone: Boolean;
  Database : TffeDatabaseItem;
  Table    : TffeTableItem;
  TableIndex : longInt;
  UnableToOpen : Boolean;
begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  TableIndex := Database.IndexOf(Table);
  with outServers do begin
    if Table.Table.Active then
      Table.Table.Close;
    Table.Table.Exclusive := True;
    try
      Screen.Cursor := crHourGlass;
      try
        Table.Table.Open;
        Table.Table.Close;
        UnableToOpen := False;
      finally
        Table.Table.Exclusive := False;
        Screen.Cursor := crDefault;
      end;
    except
      UnableToOpen := True;
    end;
    if UnableToOpen then begin
      MessageDlg('Unable to gain exclusive access to the table. Restructure operation '
                 + #13 + #10 + 'cannot contiue.', mtInformation, [mbOK], 0);
      Exit;
    end;
    if ShowRestructureTableDlg(Database, TableIndex) = mrOK then begin
      aNode := outServers.Selected;

      with Table do begin
        if Rebuilding then begin

          { Change the display in the outline; table will be unavailable
            until the rebuild is done. }
          aNode.Text := TableName + ' (restructuring)';
          try
            Application.ProcessMessages;

            { Display the rebuild progress window }
            with TfrmRebuildStatus.Create(nil) do
            try
              ShowProgress('Restructuring', TableName);
              try
                repeat
                  CheckRebuildStatus(RebuildDone, Status);
                  if not RebuildDone then begin
                    UpdateProgress(RebuildDone, Status);
                    Sleep(250);
                  end;
                until RebuildDone;
              finally
                Hide;
              end;
              Check(Status.rsErrorCode);
            finally
              Free;
            end;
          finally
            aNode.Text := TableName;
          end;
        end;
      end;
    end;
  end;
  if Table.Table.Active then                                                    {!!.06}
    Table.Table.Close                                                           {!!.06}
end;
{--------}
procedure TfrmMain.popmnuTableImportSchemaClick(Sender: TObject);
var
  Database : TffeDatabaseItem;
  Table    : TffeTableItem;
  TableIndex : longInt;
begin
  Table := TffeTableItem(GetSelectedEntity);
  Database := Table.Database;
  TableIndex := Database.IndexOf(Table);

  with outServers do begin
    ShowImportDlg(Database, TableIndex);
    if TableIndex <> -1 then {we have a new table}
      AddOutlineTable(Selected, Table);
  end;
end;
{--------}
procedure TfrmMain.popmnuTableEmptyClick(Sender: TObject);
var
  aSavCursor : TCursor;                                                {!!.01}
  aTable : TffeTableItem;
begin
  aTable := TffeTableItem(GetSelectedEntity);
  with aTable do begin
    Table.DisableControls;
    try
//      if not Table.Active or not Table.Exclusive then begin          {Deleted !!.01}
        with Table do begin
          Close;
          Exclusive := True;
          Open;
        end;
//      end;                                                           {Deleted !!.01}

      if RecordCount = 0 then
        ShowMessage('Table is already empty')
      else begin
        if MessageDlg('Delete all records in ' + TableName + '?',
                      mtWarning, [mbYes, mbNo], 0) = mrYes then begin
          aSavCursor := Screen.Cursor;                                 {!!.01}
          Screen.Cursor := crHourglass;
          try
            Table.EmptyTable;
          finally
//            Table.Close;                                             {Deleted !!.01}
//            Table.Exclusive := False;                                {Deleted !!.01}
            Screen.Cursor := aSavCursor;                               {!!.01}
          end;
        end;
      end;
    finally
      Table.Close;                                                     {!!.01}
      Table.Exclusive := False;                                        {!!.01}
      Table.EnableControls;
    end;
  end;
end;
{--------}
procedure TfrmMain.ExitBtnClick(Sender: TObject);
begin
  Close;
end;
{--------}
procedure TfrmMain.UncheckMenuGroupSiblings(aMenuItem: TMenuItem);
var
  I: Integer;
begin
  with aMenuItem.Parent do begin
    for I := 0 to Count - 1 do
      if (Items[I] <> aMenuItem) and (Items[I].GroupIndex = aMenuItem.GroupIndex) then
        Items[I].Checked := False;
  end;
end;
{--------}
procedure TfrmMain.mnuSetAutoIncClick(Sender: TObject);
var
  aTable : TffeTableItem;
  Seed : TffWord32;                                                {!!.10}
begin
  aTable := TffeTableItem(GetSelectedEntity);
  Seed := aTable.GetAutoInc;
  with aTable do begin
    if ShowAutoIncDlg(TableName, Seed) = mrOK then
      SetAutoIncSeed(Seed);
  end;
end;
{--------}
procedure TfrmMain.outServersDblClick(Sender: TObject);
var
  aTable : TffeTableItem;
//  dummy : boolean;
begin
  with outServers do begin
    if assigned(Selected) then
      case TffeOutlineData(Selected.Data).EntityType of
        etServer:
          begin
            PopupMenu := popmnuServer;
//            outServersExpanding(outServers, outServers.Selected, dummy);
          end;
        etDatabase:
          begin
            PopupMenu := popmnuAlias;
//            outServersExpanding(outServers, outServers.Selected, dummy);
          end;
        etTable:
          begin
            aTable := TffeTableItem(GetSelectedEntity);
            PopupMenu := popmnuTable;
            ShowTableBrowser(aTable);
          end;
      end;
  end;
end;
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
procedure TfrmMain.mnuOptionsLiveDatasetsClick(Sender: TObject);
var                                                                      {!!.01}
  Idx : Integer;                                                         {!!.01}
begin
  mnuOptionsLiveDataSets.Checked := not mnuOptionsLiveDataSets.Checked;
  tbOptionsLiveDataSets.Down := mnuOptionsLiveDataSets.Checked;          {!!.06}
  with Config do
    if mnuOptionsLiveDataSets.Checked then
      Options := Options + [coLiveDatasets]
    else
      Options := Options - [coLiveDatasets];

  for Idx := 0 to Pred(Screen.FormCount) do                        {BEGIN !!.01}
    if Screen.Forms[Idx] is TdlgTable then
      with TdlgTable(Screen.Forms[Idx]) do begin
        ReadOnly := not mnuOptionsLiveDataSets.Checked;
        UpdateDisplay;
      end;                                                           {END !!.01}
end;
{--------}
procedure TfrmMain.outServersExpanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  aData : TffeOutlineData;
begin
  aData := TffeOutlineData(Node.Data);
  AllowExpansion := aData.EntityType in [etServer, etDatabase];

  { If we can expand and the node currently has no children, go grab the
    children. }
  if AllowExpansion and (Node.Count = 0) then begin
    case aData.EntityType of
      etServer :
        LoadOutlineDatabases(Node);
      etDatabase :
        LoadOutlineTables(Node);
    end;  { case }
{Begin !!.01}
    if Node.Expanded then begin
      Node.HasChildren := (Node.Count > 0);
      AllowExpansion := Node.HasChildren;
    end;
{End !!.01}
  end;
end;
{--------}
procedure TfrmMain.outServersEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit := GetNodeEntityType(Node) in [etDatabase, etTable];
end;
{--------}
procedure TfrmMain.outServersEdited(Sender: TObject; Node: TTreeNode;
  var S: String);
var
  OldCursor : TCursor;
begin
  { Perform the rename. }
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    case GetNodeEntityType(Node) of
      etDatabase :
        begin
          TffeDatabaseItem(GetNodeEntity(Node)).Rename(S);
          Node.Text := S;
          LoadOutlineServers;                                            {!!.01}
        end;
      etTable :
        begin
          TffeTableItem(GetNodeEntity(Node)).Rename(S);
          Node.Text := S;
        end;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;
{--------}
{$IFNDEF DCC6OrLater}
function CenterPoint(const Rect: TRect): TPoint;
begin
  with Rect do
  begin
    Result.X := (Right - Left) div 2 + Left;
    Result.Y := (Bottom - Top) div 2 + Top;
  end;
end;
{$ENDIF}
{--------}
procedure TfrmMain.outServersKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  aNode : TTreeNode;
begin
  { If user presses F2 then edit current node. }
  if (Key = VK_F2) and assigned(outServers.Selected) then
    outServers.Selected.EditText
  else if Key = VK_RETURN then
    outServersDblClick(nil)
  {Begin !!.07}
  { support the windows keyboard context menu key }
  else if (Key = VK_APPS) or
          ((Shift = [ssShift]) and (Key = VK_F10)) then begin
    aNode := outServers.Selected;
    if assigned(aNode) and assigned(aNode.Data) then begin
      case TffeOutlineData(aNode.Data).EntityType of
        etServer:   PopupMenu := popmnuServer;
        etDatabase: PopupMenu := popmnuAlias;
        etTable:    PopupMenu := popmnuTable;
      end;
      PopupMenu.Popup(ClientToScreen(CenterPoint(aNode.DisplayRect(True))).X + 5,
                      ClientToScreen(CenterPoint(aNode.DisplayRect(True))).Y + 5);
    end;
  end;
  {End !!.07}
end;
{--------}
procedure TfrmMain.mnuViewTableClick(Sender: TObject);
begin
  outServersDblClick(nil);
end;
{--------}
procedure TfrmMain.mnuDatabaseSQLClick(Sender: TObject);
begin
  ShowQueryWindow(0);
end;
{Begin !!.06}
{--------}
procedure TfrmMain.mnuCloseAllClick(Sender: TObject);
var
  Idx : Integer;
begin
  for Idx := 0 to Pred(Screen.FormCount) do
    if (Screen.Forms[Idx] is TdlgTable) or
       (Screen.Forms[Idx] is TdlgQuery) then
    Screen.Forms[Idx].Close;
end;
{End !!.06}

{Begin !!.06}
{--------}
procedure TfrmMain.UpdateWindowsMenuItems;
var
  Count,
  Idx : Integer;
  NewItem : TMenuItem;
Begin
  { ensure windows are closed first }
  Application.ProcessMessages;
  { remove all items - requires that mnuWindowsSplitter is the last
    item in the menu at designtime! }
  while mnuWindows.Items[mnuWindows.Count-1]<>mnuWindowsSplitter do
    mnuWindows.Delete(mnuWindows.Count-1);
  { add back existing forms }
  Count := 1;
  { note: it varies between Delphi versions wether new forms are added
    at the beginning or end of the Screen.Forms array. The code below
    assumes it is compiled with Delphi 6. The last opened window should
    appear at the bottom of the menu. If it appears at the top, switch
    the loop parameters around. }
  for Idx := Pred(Screen.FormCount) downto 0 do
    if (Screen.Forms[Idx] is TdlgTable) or
       (Screen.Forms[Idx] is TdlgQuery) or
       (Screen.Forms[Idx] is TfrmTableStruct) then begin             {!!.11}
      NewItem := TMenuItem.Create(NIL);
      NewItem.Caption := Screen.Forms[Idx].Caption;
      if Count<=9 then
        NewItem.Caption := '&' + IntToStr(Count) + ' ' + NewItem.Caption;
      Inc(Count);
      NewItem.OnClick := WindowsMenuItemClick;
      NewItem.Tag := Integer(Screen.Forms[Idx]);
      mnuWindows.Add(NewItem);
    end;
end;
{End !!.06}

{Begin !!.06}
{--------}
procedure TfrmMain.WindowsMenuItemClick(Sender: TObject);
begin
  if (Sender IS TMenuItem) AND
      Assigned(Pointer(TMenuItem(Sender).Tag)) then
    TForm(TMenuItem(Sender).Tag).BringToFront;
end;
{End !!.06}

{Begin !!.06}
{--------}
procedure TfrmMain.mnuWindowsClick(Sender: TObject);
begin
  { we only update the menu when the user actually clicks it. the update
    executes so fast that the user won't notice anyway. }
  UpdateWindowsMenuItems;
  mnuCloseAll.Enabled := Screen.FormCount>1;
  tbCloseAll.Enabled := mnuCloseAll.Enabled;
end;
{End !!.06}

{Begin !!.06}
{--------}
procedure TfrmMain.AppMessage(var Msg: TMsg; var Handled: Boolean);
var
  Idx : Integer;
begin
  { trap ALT-F6 keypresses and make the next window in the
    window list active }
  if (Msg.message = WM_SYSKEYDOWN) and
     (Msg.wparam = VK_F6) then
  begin
    if (Screen.FormCount>1) and
       (Screen.ActiveForm is TfrmMain) or
       (Screen.ActiveForm is TdlgTable) or
       (Screen.ActiveForm is TdlgQuery) or
       (Screen.ActiveForm is TfrmTableStruct) then begin          {!!.11}
      Idx := 0;
      { find index of active form }
      while (Idx<Screen.FormCount) and
            (Screen.ActiveForm<>Screen.Forms[Idx]) do
        Inc(Idx);
      { note: it may be that the code below will fail, depending on what delphi
        version it is compiled with and how that delphi version updates the
        Screen.Forms array. the code below works with Delphi 6. }
      { if at start of array, wrap around, else pick previous in list }
      if Idx=0 then
        Screen.Forms[Pred(Screen.FormCount)].BringToFront
      else
        Screen.Forms[Idx-1].BringToFront;
    end;
    Handled := True;
  end;
  { for all other messages, Handled remains False }
  { so that other message handlers can respond }
end;
{End !!.06}
{Begin !!.06}
{--------}
procedure TfrmMain.DoIdle(Sender: TObject; var Done: Boolean);
var
  Idx : Integer;
begin
  { to ensure the toolbutton is correctly updated }
  for Idx := 0 to Pred(Screen.FormCount) do
    if (Screen.Forms[Idx] is TdlgTable) or
       (Screen.Forms[Idx] is TdlgQuery) then begin
      tbCloseAll.Enabled := True;
      Exit;
    end;
  tbCloseAll.Enabled := False;
end;
{End !!.06}
{Begin !!.07}
procedure TfrmMain.mnuToolsFFCommsClick(Sender: TObject);
begin
  with uFFComms.TfrmFFCommsMain.Create(Self) do
  try
    Caption := 'Set Default Server';
//    Label3.Visible := False;
    if ShowModal=mrOK then
      Initialize;
  finally
    Free;
  end;
end;
{End !!.07}
{Begin !!.07}
procedure TfrmMain.mnuSetAsAutomaticDefaultClick(Sender: TObject);
begin
  { leave servername alone if SUP, like FFCOMMS does }
  if TffeServerItem(GetSelectedEntity).Protocol=ptSingleUser then
    FFClientConfigWriteProtocolName(ffc_SingleUser)
  else begin
    if TffeServerItem(GetSelectedEntity).Protocol=ptTCPIP then
      FFClientConfigWriteProtocolName(ffc_TCPIP)
    else
    if TffeServerItem(GetSelectedEntity).Protocol=ptIPXSPX then
      FFClientConfigWriteProtocolName(ffc_IPXSPX);
    FFClientConfigWriteServerName(TffeServerItem(GetSelectedEntity).ServerName);
  end;
  Initialize;
end;
{End !!.07}
procedure TfrmMain.outServersChange(Sender: TObject; Node: TTreeNode);
begin
  outServersClick(Sender);
end;

procedure TfrmMain.outServersContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
{}
end;

{Begin !!.11}
procedure TfrmMain.popmnuServerRefreshClick(Sender: TObject);
begin
  RefreshDatabases(Sender);
end;

{--------}
procedure TfrmMain.ShowServerStatistics(aServer : TffeServerItem);
var
  OldCursor: TCursor;
  dlgServerStats : TdlgServerStats;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    dlgServerStats := TdlgServerStats.Create(Application);                       {!!.02}
    with dlgServerStats do begin
      Log := LogMain;
      Protocol := aServer.Protocol;
      ServerName := aServer.ServerName;
      UserName := TffexpSession(aServer.Session).ffeUserName;
      Password := TffexpSession(aServer.Session).ffePassword;
      Show;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;


procedure TfrmMain.popmnuServerStatisticsClick(Sender: TObject);
var
  anEntity : TffeEntityItem;
  Server : TffeServerItem;
begin
  anEntity := TffeOutlineData(outServers.Selected.Data).Entity;
  Server := TffeServerItem(anEntity);
  ShowServerStatistics(Server);
end;


procedure TfrmMain.mnuOptionsSetDefaultTimeoutClick(Sender: TObject);
var
  sTimeout : String;
  res : Boolean;
  Idx : Integer;
begin
  sTimeout := IntToStr(Config.DefaultTimeout);
  repeat
    res := InputQuery('Default Timeout (ms)', 'Value:', sTimeout);
    if res then
    try
      Config.DefaultTimeout := StrToInt(sTimeout);
      if Config.DefaultTimeout<-1 then
        raise EConvertError.Create('');
      {Begin !!.11}
      { set default timeout on open servers, tables and queries }
      for idx := 0 to ServerList.Count - 1 do
        if Assigned(ServerList.Items[idx].Client) then
          ServerList.Items[idx].Client.TimeOut := Config.DefaultTimeout;
      for Idx := 0 to Pred(Screen.FormCount) do
        if (Screen.Forms[Idx] is TdlgTable) then
           TdlgTable(Screen.Forms[Idx]).UpdateDefaultTimeout
        else
        if (Screen.Forms[Idx] is TdlgQuery) then
           TdlgQuery(Screen.Forms[Idx]).UpdateDefaultTimeout;
      {End !!.11}
      res := False;
    except
      on EConvertError do begin
        MessageDlg('Value must be a number between -1 and '+IntToStr(MaxInt), mtError, [mbOK], 0);
      end;
    end;
  until not res;
end;
{End !!.11}
end.

