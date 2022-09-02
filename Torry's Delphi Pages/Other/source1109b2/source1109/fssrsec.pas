Unit fssrsec;

Interface

Uses
  SysUtils,
  fslleng,
  fsllbase,
  fsserverclass,
  fssrbase,
  fssrbde;

Type
  TFSSecurityExtender = Class(TFSBaseEngineExtender)
  Public
    Constructor Create(aOwner: TFSBaseEngineMonitor); Override;
    Function Notify(aServerObject: TFSSpecObject;
      aAction: TffEngineAction): TffResult; Override;
  Protected
  End;

  TFSMonitor = Class(TFSBaseEngineMonitor)
  Public
    Function Interested(aServerObject: TFSSpecObject): TFSBaseEngineExtender; Override;
  Protected
    Procedure bemSetServerEngine(anEngine: TFSBaseServerEngine); Override;
  End;

Implementation

Uses
  fssrcvex;

{===TFSMonitor===============================================}

Function TFSMonitor.Interested(aServerObject: TFSSpecObject): TFSBaseEngineExtender;
{Rewritten !!.11}
Begin
  { This should always be a TfsSrBaseCursor, TfsSrcClient or TffDatabase,
    but we need to check to be sure. }
  Result := Nil;
  If (aServerObject Is TfsSrBaseCursor) Then
    Begin
      { Do not create extenders for temporary files. Temporary files are
        created by the SQL engine when it needs to build a result table. }
      If Not (fffaTemporary In
        TfsSrBaseCursor(aServerObject).Table.Files[0].fiAttributes) Then
        Result := TFSSecurityExtender.Create(Self);
    End
  Else If (aServerObject Is TfsSrcDatabase) Or
    (aServerObject Is TfsSrcClient) Then
    Result := TFSSecurityExtender.Create(Self);
End;
{--------}

Procedure TFSMonitor.bemSetServerEngine(anEngine: TFSBaseServerEngine);
Begin
  Inherited bemSetServerEngine(anEngine);
  AddInterest(TfsSrcClient);
  AddInterest(TfsSrcDatabase);
  AddInterest(TfsSrBaseCursor);
End;
{====================================================================}

{===TFSSecurityExtender==============================================}

Constructor TFSSecurityExtender.Create(aOwner: TFSBaseEngineMonitor);
Begin
  Inherited Create(aOwner);
  FActions := [ffeaBeforeAddInx,
    ffeaBeforeBLOBCreate,
    ffeaBeforeBLOBDelete,
    ffeaBeforeBLOBGetLength,
    ffeaBeforeBLOBRead,
    ffeaBeforeBLOBTruncate,
    ffeaBeforeBLOBWrite,
    ffeaBeforeChgAliasPath,
    ffeaBeforeDBDelete,
    ffeaBeforeDBInsert,
    ffeaBeforeDBRead,
    ffeaBeforeDBUpdate,
    ffeaBeforeFileBLOBAdd,
    ffeaBeforeRebuildInx,
    ffeaBeforeRecDelete,
    ffeaBeforeRecInsert,
    ffeaBeforeRecRead,
    ffeaBeforeRecUpdate,
    ffeaBeforeTabDelete,
    ffeaBeforeTabInsert,
    ffeaBeforeTableLock,
    ffeaBeforeTabPack,
    ffeaBeforeTabRead,
    ffeaBeforeTabRestruct,
    ffeaBeforeTabUpdate,
    //
  ffeaBeforeViewRead,
    ffeaBeforeViewInsert,
    ffeaBeforeViewUpdate,
    ffeaBeforeViewDelete,

  ffeaBeforeProcRead,
    ffeaBeforeProcInsert,
    ffeaBeforeProcUpdate,
    ffeaBeforeProcDelete,

  ffeaBeforeTrigRead,
    ffeaBeforeTrigInsert,
    ffeaBeforeTrigUpdate,
    ffeaBeforeTrigDelete,

  ffeaBeforeReferRead,
    ffeaBeforeReferInsert,
    ffeaBeforeReferUpdate,
    ffeaBeforeReferDelete,

  ffeaBeforeGenerRead,
    ffeaBeforeGenerInsert,
    ffeaBeforeGenerUpdate,
    ffeaBeforeGenerDelete,

  ffeaBeforeExceptRead,
    ffeaBeforeExceptInsert,
    ffeaBeforeExceptUpdate,
    ffeaBeforeExceptDelete,

  ffeaBeforeOpenData,
    ffeaBeforeOpenUser,
    ffeaBeforeOpenSystem,
    ffeaBeforeCopyTable,
    ffeaBeforeTabEmpty,
    ffeaBeforeTabSetInc,
    ffeaBeforeProtectRow];
End;

Function TFSSecurityExtender.Notify(aServerObject: TFSSpecObject;
  aAction: TffEngineAction): TffResult;
Var
  ReqRights: TffUserRights;
Begin
  Try
    Result := DBIERR_NONE;
    ReqRights := [];

    {don't check rights if this isn't a secure server}
    If Not TFSServer(FParent.ServerEngine).Configuration.GeneralInfo^.giIsSecure Then
      Exit;
    { Ignore if this is not the right kind of server object. }
    If (Not (aServerObject Is TfsSrBaseCursor)) And {!!.01}
    (Not (aServerObject Is TfsSrcDatabase)) And {!!.01}
    (Not (aServerObject Is TfsSrcClient)) Then
      Exit;
    {find what rights are needed for aAction}
    Case aAction Of
      {grouped by unique subsets of TffUserRights}
      {record actions include actions for BLOBs - for example,
       reading a BLOB would require the client to have the same
       privileges as reading a record}
      {reading a record, BLOB, table, or database requires read privileges}
      ffeaBeforeCopyTable: ReqRights := [arDelete, arInsert, arRead, arUpdate,
        arReadBlob, arUpdateBlob, arDeleteBlob, arInsertBlob, arReadBase,
          arReadTable, arUpdateBase, arUpdateTable, arCopyTable];
      ffeaBeforeDBRead: ReqRights := [arReadBase];
      ffeaBeforeBLOBRead,
        ffeaBeforeBLOBGetLength: ReqRights := [arReadBase, arReadTable, arRead, arReadBlob];
      ffeaBeforeRecRead: ReqRights := [arReadBase, arReadTable, arRead];
      ffeaBeforeTabRead: ReqRights := [arReadBase, arReadTable];

      {inserting a record, BLOB, table, or database requires insert privileges}
      ffeaBeforeDBInsert: ReqRights := [arInsertBase];
      ffeaBeforeBLOBCreate,
        ffeaBeforeFileBLOBAdd: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable, arInsertBlob];
      ffeaBeforeRecInsert: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable, arInsert];
      ffeaBeforeTabInsert: ReqRights := [arReadBase, arUpdateBase, arInsertTable];

      ffeaBeforeTabEmpty: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable, arTabEmpty];
      ffeaBeforeTabSetInc: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable, arTabSetInc];
      {updating a table, writing to a BLOB, or truncating a BLOB requires
       updates privileges}
      ffeaBeforeBLOBWrite: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable, arRead, arReadBlob, arUpdateBlob];
      ffeaBeforeBLOBTruncate: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable, arRead, arReadBlob, arUpdateBlob];
      ffeaBeforeTabUpdate: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable];

      {updating a record requires read and update privileges}
      ffeaBeforeRecUpdate: ReqRights := [arRead, arUpdate, arReadBase, arReadTable, arUpdateBase, arUpdateTable];
      ffeaBeforeProtectRow: ReqRights := [arRead, arUpdate, arReadBase, arReadTable, arUpdateBase, arUpdateTable, arProtectRow];

      {deleting a record, BLOB, table, or database requires delete privileges}
      ffeaBeforeDBDelete: ReqRights := [arReadBase, arDeleteBase];
      ffeaBeforeDBUpdate: ReqRights := [arReadBase, arUpdateBase];
      ffeaBeforeBLOBDelete: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable, arRead, arReadBlob, arDeleteBlob];
      ffeaBeforeRecDelete: ReqRights := [arReadBase, arReadTable, arUpdateBase, arUpdateTable, arRead, arDelete];
      ffeaBeforeTabDelete: ReqRights := [arReadBase, arUpdateBase, arDeleteTable];

      {restructuring a table requires the client to have delete,
       insert, read, and update privileges}
      ffeaBeforeTabRestruct: ReqRights := [arDelete, arInsert, arRead, arUpdate,
        arReadBlob, arUpdateBlob, arDeleteBlob,
          arInsertBlob, arRestruct, arReadTable, arUpdateTable, arReadBase, arUpdateBase];

      {packing a table requires delete and update privileges}
      ffeaBeforeTabPack: ReqRights := [arDelete, arInsert, arRead, arUpdate,
        arUpdateBlob, arDeleteBlob, arInsertBlob, arReadBlob, arDefrag,
          arReadTable, arUpdateTable, arReadBase, arUpdateBase];

      {adding or rebuilding an index requires insert, read, and
       update privileges}
      ffeaBeforeAddInx: ReqRights := [arInsert, arRead, arUpdate, arRestruct, arReadTable,
        arUpdateTable, arReadBase, arUpdateBase];
      ffeaBeforeRebuildInx: ReqRights := [arInsert, arRead, arUpdate, arDefrag,
        arReadTable, arUpdateTable, arReadBase, arUpdateBase];

      {locking a table or changing an alias requires read and
       update privileges}
      ffeaBeforeChgAliasPath: ReqRights := [arReadBase, arUpdateBase];
      ffeaBeforeTableLock: ReqRights := [arRead, arUpdate, arReadTable,
        arUpdateTable, arReadBase, arUpdateBase];
    End; {case}

    { If no rights required then exit. }
    If ReqRights = [] Then
      Exit;

    { Now that we know what rights are required, lets see if the
      client has sufficient rights. }
    If aServerObject Is TfsSrcClient Then
      Begin
        If (TfsSrcClient(aServerObject).Rights * ReqRights) <> ReqRights Then
          Result := DBIERR_NOTSUFFTABLERIGHTS;
      End
    Else If ((TfsSrcClient(TfsServerObject(aServerObject).Client).Rights) *
      ReqRights) <> ReqRights Then
      Result := DBIERR_NOTSUFFTABLERIGHTS;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerException(E, Nil);
      End;
  End; {try..except}
End;
{====================================================================}

End.

