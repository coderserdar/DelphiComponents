{*********************************************************}
{* FlashFiler: Server Configuration Information          *}
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

unit ffsrcfg;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffllbase,
  ffllunc,
  ffsrmgr,
  ffllexcp,
  ffllprot,
  ffsrintf,
  ffsrbase;

type
  PffGeneralInfo = ^TffGeneralInfo;
  TffGeneralInfo = packed record
{Begin !!.06}
{$IFDEF IsDelphi}
    giServerName      : TffNetName;
{$ELSE}
    giServerName      : TffNetNameShr;
{$ENDIF}
{End !!.06}
    giMaxRAM          : Longint;
    giSingleUser      : Boolean;
    giIPXSPX          : Boolean;
    giIPXSPXLFB       : Boolean;
    giTCPIP           : Boolean;
    giTCPIPLFB        : Boolean;
    giTCPPort         : Longint;
    giUDPPortSr       : Longint;
    giUDPPortCl       : Longint;
    giIPXSocketSr     : Longint;
    giIPXSocketCl     : Longint;
    giSPXSocket       : Longint;
    giIsSecure        : Boolean;
    giAutoUp          : Boolean;
    giAutoMini        : Boolean;
    giDebugLog        : Boolean;
    giAllowEncrypt    : Boolean;
    giReadOnly        : Boolean;
    giLastMsgInterval : Longint;
    giKAInterval      : Longint;
    giKARetries       : Longint;
    giPriority        : Longint;
    giTCPInterface    : Longint;
    giNoAutoSaveCfg   : Boolean;
    giTempStoreSize   : Integer;
    giCollectEnabled  : Boolean;
    giCollectFreq     : Longint;
  end;

type
  TffAliasItem = class(TffUCStrListItem)
    protected {private}
      FCheckDisk : Boolean;                                            {!!.11}
      FPath      : PffShStr;
    protected
      function GetAlias : string;                                      {!!.10}
      function GetPath : string;                                       {!!.10}
    public
      constructor Create(const aAlias     : TffName;
                         const aPath      : TffPath;
                               aCheckDisk : Boolean);                  {!!.11}
      destructor Destroy; override;

      property Alias : string read GetAlias;                           {!!.10}
      property Path : string read GetPath;                             {!!.10}
      property CheckSpace : Boolean                                    {!!.11}
        read FCheckDisk;                                               {!!.11}
  end;

  TffAliasList = class(TffThreadList)
    protected
      function GetAliasItem(aInx : Integer) : TffAliasItem;
      function GetAliasPath(const aAlias : TffName) : TffPath;
    public
      procedure AddAlias(aAliasItem : TffAliasItem);
      function AliasExists(const aAlias : TffName) : Boolean;
      function AliasIndex(const aAlias : TffName) : Integer;
      function CheckDiskSpace(const aAlias : TffName) : Boolean;       {!!.11}
      procedure DeleteAlias(const aAlias : TffName);

      property AliasItem[aInx : Integer] : TffAliasItem
        read GetAliasItem; default;
      property Path[const aAlias : TffName] : TffPath
        read GetAliasPath;
  end;

  TffUserItem = class(TffUCStrListItem)
    protected {private}
      FFirst   : PffShStr;
      FLast    : PffShStr;
      FPwdHash : TffWord32;
      FRights  : TffUserRights;
    protected
      function GetFirstName : string;                                  {!!.10}
      function GetLastName : string;                                   {!!.10}
      function GetUserID : string;                                     {!!.10}
    public
      constructor Create(const aUserID    : TffName;
                         const aLastName  : TffName;
                         const aFirstName : TffName;
                               aPwdHash   : TffWord32;
                               aRights    : TffUserRights);
      destructor Destroy; override;

      property FirstName : string read GetFirstName;                   {!!.10}
      property LastName : string read GetLastName;                     {!!.10}
      property PasswordHash : TffWord32 read FPwdHash;
      property Rights : TffUserRights read FRights;
      property UserID : string read GetUserID;                         {!!.10}
  end;

  TffUserList = class(TffObject)
    protected {private}
      FUserList : TffList;
    protected
      function GetUserItem(aInx : integer) : TffUserItem;
      function GetUserPwdHash(const aUserID : TffName) : TffWord32;
      function GetUserRights(const aUserID : TffName) : TffUserRights;
    public
      constructor Create;
      destructor Destroy; override;

      procedure AddUser(aUserItem : TffUserItem);
      function UserExists(const aUserID : TffName) : Boolean;
      function UserIndex(const aUserID : TffName) : Integer;
      function Count : integer;
      procedure DeleteUser(const aUserID : TffName);
      procedure Empty;

      property UserItem[aInx : integer] : TffUserItem read GetUserItem; default;
      property PasswordHash[const aUserID : TffName] : TffWord32 read GetUserPwdHash;
      property UserRights[const aUserID : TffName] : TffUserRights read GetUserRights;
  end;

  TffKeyProcItem = class(TffUCStrListItem)
    protected {private}
      FIndexID     : Integer;
      FDLLName     : PffShStr;
      FBuildKey    : TffKeyBuildFunc;
      FBuildName   : PffShStr;
      FCompareKey  : TffKeyCompareFunc;
      FCompareName : PffShStr;
      FPath        : PffShStr;
      FTable       : PffShStr;

      kpiLibHandle    : THandle;
    protected
      function GetBuildKeyName : string;                               {!!.10}
      function GetCompareKeyName : string;                             {!!.10}
      function GetDLLName : string;                                    {!!.10}
      function GetPath : string;                                       {!!.10}
      function GetTable : string;                                      {!!.10}
      function GetTableDataFileName : string;                          {!!.10}
    public
      constructor Create(const aPath        : TffPath;
                         const aTable       : TffTableName;
                               aIndexID     : Integer;
                         const aDLLName     : TffFullFileName;
                         const aBuildName   : TffName;
                         const aCompareName : TffName);
      destructor Destroy; override;
      function Link : boolean;
      procedure Unlink;

      property DLLName : string
        read GetDLLName;                                               {!!.10}
      property IndexID : Integer
        read FIndexID;
      property BuildKey : TffKeyBuildFunc
        read FBuildKey;
      property BuildKeyName : string
        read GetBuildKeyName;                                          {!!.10}
      property CompareKey : TffKeyCompareFunc
        read FCompareKey;
      property CompareKeyName : string
        read GetCompareKeyName;                                        {!!.10}
      property Path : string
        read GetPath;                                                  {!!.10}
      property Table : string
        read GetTable;                                                 {!!.10}
      property TableDataFileName : string
        read GetTableDataFileName;                                     {!!.10}
  end;

  TffKeyProcList = class(TffObject)
    protected {private}
      FKPList : TffList;
    protected
      function GetKPItem(aInx : integer) : TffKeyProcItem;
    public
      constructor Create;
      destructor Destroy; override;

      procedure AddKeyProc(aKeyProcItem : TffKeyProcItem);
      function KeyProcExists(const aPath    : TffPath;
                             const aTable   : TffTableName;
                                   aIndexID : Integer)
                                            : Boolean;
      function KeyProcIndex(const aPath    : TffPath;
                            const aTable   : TffTableName;
                                  aIndexID : Integer)
                                           : Integer;
      function Count : integer;
      procedure DeleteKeyProc(const aPath    : TffPath;
                              const aTable   : TffTableName;
                                    aIndexID : Integer);
      procedure Empty;

      property KeyProcItem[aInx : Integer] : TffKeyProcItem
        read GetKPItem; default;
  end;

  TffServerConfiguration = class(TffObject)
    protected {private}
      FAliasList   : TffAliasList;
      FGeneralInfo : TffGeneralInfo;
      FKeyProcList : TffKeyProcList;
      FUserList    : TffUserList;
      scPadLock    : TffPadLock;
    protected
      function GetGeneralInfo : PffGeneralInfo;
      function GetServerName : string;                                 {!!.10}
    public
      constructor Create;
      destructor Destroy; override;

      procedure AddAlias(const aAlias     : TffName;
                         const aPath      : TffPath;
                               aCheckDisk : Boolean);                  {!!.11}
      procedure AddKeyProc(const aPath        : TffPath;
                           const aTable       : TffTableName;
                                 aIndexID     : Integer;
                           const aDLLName     : TffFullFileName;
                           const aBuildName   : TffName;
                           const aCompareName : TffName);
      procedure AddUser(const aUserID    : TffName;
                        const aLastName  : TffName;
                        const aFirstName : TffName;
                              aPwdHash   : TffWord32;
                              aRights    : TffUserRights);
      procedure Lock;
      procedure PerformDynamicLink;
      procedure Unlock;

      property AliasList : TffAliasList
        read FAliasList;
      property GeneralInfo : PffGeneralInfo
        read GetGeneralInfo;
      property KeyProcList : TffKeyProcList
        read FKeyProcList;
      property ServerName : string
        read GetServerName;                                            {!!.10}
      property UserList : TffUserList
        read FUserList;
  end;

{---Internal helper routines---}
function srcfgCalcKPKey(const aTable : TffFullFileName;
                              aIndex : Integer) : TffShStr;
function srcfgAliasFromKPKey(const BKK : TffShStr) : TffName;
function srcfgTableFromKPKey(const BKK : TffShStr) : TffTableName;
function srcfgIndexFromKPKey(const BKK : TffShStr) : Integer;

implementation

{== Helper routines ==================================================}
function srcfgCalcKPKey(const aTable : TffFullFileName;
                              aIndex : Integer)
                                     : TffShStr;
var
  S : string[9];
begin
  Str(aIndex, S);
  Result := aTable;
  FFShStrAddChar(Result, '|');
  FFShStrConcat(Result, S);
end;
{--------}
function srcfgAliasFromKPKey(const BKK : TffShStr) : TffName;
var
  PosSlash : integer;
begin
  PosSlash := Pos('/', BKK);
  if (PosSlash > 0) then
    Result := Copy(BKK, 1, pred(PosSlash))
  else
    Result := '';
end;
{--------}
function srcfgTableFromKPKey(const BKK : TffShStr) : TffTableName;
var
  PosColon : Integer;
  PosSlash : Integer;
begin
  PosSlash := Pos('/', BKK);
  PosColon := Pos(':', BKK);
  if (PosSlash > 0) and (PosColon > 0) then
    Result := Copy(BKK, succ(PosSlash), pred(PosColon - PosSlash))
  else
    Result := '';
end;
{--------}
function srcfgIndexFromKPKey(const BKK : TffShStr) : Integer;
var
  PosColon : Integer;
  ec       : Integer;
  InxAsStr : string[9];
begin
  PosColon := Pos(':', BKK);
  if (PosColon > 0) then begin
    InxAsStr := Copy(BKK, succ(PosColon), 255);
    Val(InxAsStr, Result, ec);
    if (ec <> 0) then
      Result := 0;
  end
  else
    Result := 0;
end;
{=====================================================================}

{== TffAliasItem =====================================================}
constructor TffAliasItem.Create(const aAlias     : TffName;
                                const aPath      : TffPath;
                                      aCheckDisk : Boolean);           {!!.11}
begin
  inherited Create(aAlias);
  FPath := FFShStrAlloc(FFExpandUNCFileName(aPath));
  FCheckDisk := aCheckDisk;                                            {!!.11}
end;
{--------}
destructor TffAliasItem.Destroy;
begin
  FFShStrFree(FPath);
  inherited Destroy;
end;
{--------}
function TffAliasItem.GetAlias : string;                               {!!.10}
begin
  Result := KeyAsStr;
end;
{--------}
function TffAliasItem.GetPath : string;                                {!!.10}
begin
  Result := FPath^;
end;
{====================================================================}


{===TffAliasList=====================================================}
procedure TffAliasList.AddAlias(aAliasItem : TffAliasItem);
begin
  Insert(aAliasItem);
end;
{--------}
function TffAliasList.AliasExists(const aAlias : TffName) : Boolean;
begin
  Result := Exists(aAlias);
end;
{--------}
function TffAliasList.AliasIndex(const aAlias : TffName) : Integer;
begin
  Result := Index(aAlias);
end;
{--------}
{!!.11 - New}
function TffAliasList.CheckDiskSpace(const aAlias : TffName)
                                                  : Boolean;
var
  Position : Integer;
begin
  Result := False;
  Position := Index(aAlias);
  if (Position > -1) then
    Result := AliasItem[Position].CheckSpace;
end;
{--------}
procedure TffAliasList.DeleteAlias(const aAlias : TffName);
begin
  Delete(aAlias);
end;
{--------}
function TffAliasList.GetAliasItem(aInx : Integer) : TffAliasItem;
begin
  Result := TffAliasItem(fflList[aInx]);
end;
{--------}
function TffAliasList.GetAliasPath(const aAlias : TffName) : TffPath;
var
  Inx : Integer;
begin
  Inx := Index(aAlias);
  if (Inx = -1) then
    Result := ''
  else
    Result := TffAliasItem(fflList[Inx]).Path;
end;
{====================================================================}


{===TffUserItem======================================================}
constructor TffUserItem.Create(const aUserID    : TffName;
                               const aLastName  : TffName;
                               const aFirstName : TffName;
                                     aPwdHash   : TffWord32;
                                     aRights    : TffUserRights);
begin
  inherited Create(aUserID);
  FFirst := FFShStrAlloc(aFirstName);
  FLast := FFShStrAlloc(aLastName);
  FPwdHash := aPwdHash;
  FRights := aRights;
end;
{--------}
destructor TffUserItem.Destroy;
begin
  FFShStrFree(FFirst);
  FFShStrFree(FLast);
  inherited Destroy;
end;
{--------}
function TffUserItem.GetFirstName : string;                            {!!.10}
begin
  Result := FFirst^;
end;
{--------}
function TffUserItem.GetLastName : string;                             {!!.10}
begin
  Result := FLast^;
end;
{--------}
function TffUserItem.GetUserID : string;                               {!!.10}
begin
  Result := KeyAsStr;
end;
{====================================================================}


{===TffUserList======================================================}
constructor TffUserList.Create;
begin
  inherited Create;
  FUserList := TffList.Create;
end;
{--------}
destructor TffUserList.Destroy;
begin
  FUserList.Free;
  inherited Destroy;
end;
{--------}
procedure TffUserList.AddUser(aUserItem : TffUserItem);
begin
  FUserList.Insert(aUserItem);
end;
{--------}
function TffUserList.UserExists(const aUserID : TffName) : Boolean;
begin
  Result := FUserList.Exists(aUserID);
end;
{--------}
function TffUserList.UserIndex(const aUserID : TffName) : Integer;
begin
  Result := FUserList.Index(aUserID);
end;
{--------}
function TffUserList.Count : Integer;
begin
  Result := FUserList.Count;
end;
{--------}
procedure TffUserList.DeleteUser(const aUserID : TffName);
begin
  FUserList.Delete(aUserID);
end;
{--------}
procedure TffUserList.Empty;
begin
  FUserList.Empty;
end;
{--------}
function TffUserList.GetUserItem(aInx : Integer) : TffUserItem;
begin
  Result := TffUserItem(FUserList[aInx]);
end;
{--------}
function TffUserList.GetUserPwdHash(const aUserID : TffName) : TffWord32;
var
  Inx : integer;
begin
  if (aUserID = '') then
    Result := 0
  else begin
    Inx := FUserList.Index(aUserID);
    if (Inx = -1) then
      Result := $FFFFFFFF
    else
      Result := TffUserItem(FUserList[Inx]).PasswordHash;
  end;
end;
{--------}
function TffUserList.GetUserRights(const aUserID : TffName) : TffUserRights;
var
  Inx : integer;
begin
  Inx := FUserList.Index(aUserID);
  if (Inx = -1) then
    Result := []
  else
    Result := TffUserItem(FUserList[Inx]).Rights;
end;
{====================================================================}



{===TffKeyProcItem===================================================}
constructor TffKeyProcItem.Create(const aPath        : TffPath;
                                  const aTable       : TffTableName;
                                        aIndexID     : integer;
                                  const aDLLName     : TffFullFileName;
                                  const aBuildName   : TffName;
                                  const aCompareName : TffName);
var
  FFN     : TffFullFileName;
  UNCPath : TffPath;
begin
  UNCPath := FFExpandUNCFileName(aPath);
  FFN := FFMakeFullFileName(UNCPath, FFMakeFileNameExt(aTable, ffc_ExtForData));
  inherited Create(srcfgCalcKPKey(FFN, aIndexID));
  FIndexID := aIndexID;
  FPath := FFShStrAlloc(UNCPath);
  FTable := FFShStrAlloc(aTable);
  FDLLName := FFShStrAlloc(aDLLName);
  FBuildName := FFShStrAlloc(aBuildName);
  FCompareName := FFShStrAlloc(aCompareName);
end;
{--------}
destructor TffKeyProcItem.Destroy;
begin
  Unlink;
  FFShStrFree(FCompareName);
  FFShStrFree(FBuildName);
  FFShStrFree(FDLLName);
  FFShStrFree(FTable);
  FFShStrFree(FPath);
  inherited Destroy;
end;
{--------}
function TffKeyProcItem.GetDLLName : string;                           {!!.10}
begin
  Result := FDLLName^;
end;
{--------}
function TffKeyProcItem.GetBuildKeyName : string;                      {!!.10}         
begin
  Result := FBuildName^;
end;
{--------}
function TffKeyProcItem.GetCompareKeyName : string;                    {!!.10}
begin
  Result := FCompareName^;
end;
{--------}
function TffKeyProcItem.GetPath : string;                              {!!.10}
begin
  Result := FPath^;
end;
{--------}
function TffKeyProcItem.GetTable : string;                             {!!.10}       
begin
  Result := FTable^;
end;
{--------}
function TffKeyProcItem.GetTableDataFileName : string;                 {!!.10}
begin
  Result := FFMakeFullFileName(Path, FFMakeFileNameExt(Table, ffc_ExtForData));
end;
{--------}
function TffKeyProcItem.Link : Boolean;
var
  DLLPathZ  : TffStringZ;
  ProcNameZ : TffStringZ;
begin
  Result := false;
  Unlink;
  kpiLibHandle := LoadLibrary(FFStrPCopy(DLLPathZ, DLLName));
  if (kpiLibHandle <> 0) then begin
    @FBuildKey := GetProcAddress(kpiLibHandle, FFStrPCopy(ProcNameZ, BuildKeyName));
    if Assigned(FBuildKey) then begin
      @FCompareKey := GetProcAddress(kpiLibHandle, FFStrPCopy(ProcNameZ, CompareKeyName));
      if Assigned(FCompareKey) then
        Result := true
      else
        Unlink;
    end
    else
      Unlink;
  end;
end;
{--------}
procedure TffKeyProcItem.Unlink;
begin
  if (kpiLibHandle <> 0) then
    FreeLibrary(kpiLibHandle);
  kpiLibHandle := 0;
  FBuildKey := nil;
  FCompareKey := nil;
end;
{====================================================================}


{===TffKeyProcList===================================================}
constructor TffKeyProcList.Create;
begin
  inherited Create;
  FKPList := TffList.Create;
end;
{--------}
destructor TffKeyProcList.Destroy;
begin
  FKPList.Free;
  inherited Destroy;
end;
{--------}
procedure TffKeyProcList.AddKeyProc(aKeyProcItem : TffKeyProcItem);
begin
  FKPList.Insert(aKeyProcItem);
end;
{--------}
function TffKeyProcList.Count : Integer;
begin
  Result := FKPList.Count;
end;
{--------}
procedure TffKeyProcList.DeleteKeyProc(const aPath    : TffPath;
                                       const aTable   : TffTableName;
                                             aIndexID : integer);
var
  FFN   : TffFullFileName;
  KPKey : TffShStr;
begin
  FFN := FFMakeFullFileName(
            FFExpandUNCFileName(aPath),
            FFMakeFileNameExt(aTable, ffc_ExtForData));
  KPKey := srcfgCalcKPKey(FFN, aIndexID);
  FKPList.Delete(KPKey);
end;
{--------}
procedure TffKeyProcList.Empty;
begin
  FKPList.Empty;
end;
{--------}
function TffKeyProcList.GetKPItem(aInx : Integer) : TffKeyProcItem;
begin
  Result := TffKeyProcItem(FKPList[aInx]);
end;
{--------}
function TffKeyProcList.KeyProcExists(const aPath    : TffPath;
                                      const aTable   : TffTableName;
                                            aIndexID : Integer) : Boolean;
var
  FFN   : TffFullFileName;
  KPKey : TffShStr;
begin
  FFN := FFMakeFullFileName(
            FFExpandUNCFileName(aPath),
            FFMakeFileNameExt(aTable, ffc_ExtForData));
  KPKey := srcfgCalcKPKey(FFN, aIndexID);
  Result := (FKPList.Index(KPKey) <> -1);
end;
{--------}
function TffKeyProcList.KeyProcIndex(const aPath    : TffPath;
                                     const aTable   : TffTableName;
                                           aIndexID : Integer) : Integer;
var
  FFN   : TffFullFileName;
  KPKey : TffShStr;
begin
  FFN := FFMakeFullFileName(
            FFExpandUNCFileName(aPath),
            FFMakeFileNameExt(aTable, ffc_ExtForData));
  KPKey := srcfgCalcKPKey(FFN, aIndexID);
  Result := FKPList.Index(KPKey);
end;
{=====================================================================}


{== TffServerConfiguration ===========================================}
constructor TffServerConfiguration.Create;
begin
  inherited Create;                                                    {!!.01}

  {set up the default general info}
  with FGeneralInfo do begin
    giServerName := '';
    giMaxRAM := 10;
    giSingleUser := True;
    giIPXSPX := False;
    giIPXSPXLFB := True;
    giTCPIP := False;
    giTCPIPLFB := True;
    giIsSecure := False;
    giAutoUp := False;
    giAutoMini := False;
    giDebugLog := False;
    {$IFDEF SecureServer}
    giAllowEncrypt := True;
    {$ELSE}
    giAllowEncrypt := False;
    {$ENDIF}
    giReadOnly := False;
    giNoAutoSaveCfg := False;
    giLastMsgInterval := ffc_LastMsgInterval;
    giKAInterval := ffc_KeepAliveInterval;
    giKARetries := ffc_KeepAliveRetries;
    giPriority := 2; {THREAD_PRIORITY_HIGHEST}                      
    giTCPPort := FFGetTCPPort;
    giUDPPortSr := FFGetUDPPortServer;
    giUDPPortCl := FFGetUDPPortClient;
    giIPXSocketSr := FFGetIPXSocketServer;
    giIPXSocketCl := FFGetIPXSocketClient;
    giSPXSocket := FFGetSPXSocket;
    giTempStoreSize := ffcl_TempStorageSize;
    giCollectEnabled := False;
    giCollectFreq := ffcl_CollectionFrequency;
  end;

  {create internal items}
  scPadLock := TffPadLock.Create;
  FAliasList := TffAliasList.Create;
  FKeyProcList := TffKeyProcList.Create;
  FUserList := TffUserList.Create;
end;
{--------}
destructor TffServerConfiguration.Destroy;
begin
  FUserList.Free;
  FKeyProcList.Free;
  FAliasList.Free;
  scPadLock.Free;
  inherited Destroy;                                                   {!!.01}
end;
{--------}
procedure TffServerConfiguration.AddAlias(const aAlias     : TffName;
                                          const aPath      : TffPath;
                                                aCheckDisk : Boolean); {!!.11}
var
  NewAlias : TffAliasItem;
begin
  { Assumption: Thread-safeness enforced at a higher level. }
  NewAlias := TffAliasItem.Create(aAlias, aPath, aCheckDisk);          {!!.11}
  try
    FAliasList.AddAlias(NewAlias)
  except
    NewAlias.Free;
    raise;
  end;{try..except}
end;
{--------}
procedure TffServerConfiguration.AddKeyProc(const aPath        : TffPath;
                                            const aTable       : TffTableName;
                                                  aIndexID     : Integer;
                                            const aDLLName     : TffFullFileName;
                                            const aBuildName   : TffName;
                                            const aCompareName : TffName);
var
  NewKeyProc : TffKeyProcItem;
begin
  { Assumption: Thread-safeness enforced at a higher level. }
  NewKeyProc := TffKeyProcItem.Create(aPath, aTable, aIndexID,
                                      aDLLName, aBuildName, aCompareName);
  try
    FKeyProcList.AddKeyProc(NewKeyProc)
  except
    NewKeyProc.Free;
    raise;
  end;{try..except}
end;
{--------}
procedure TffServerConfiguration.AddUser(const aUserID    : TffName;
                                         const aLastName  : TffName;
                                         const aFirstName : TffName;
                                               aPwdHash   : TffWord32;
                                               aRights    : TffUserRights);
var
  NewUser : TffUserItem;
begin
  { Assumption: Thread-safeness enforced at a higher level. }
  NewUser := TffUserItem.Create(aUserID, aLastName, aFirstName, aPwdHash, aRights);
  try
    FUserList.AddUser(NewUser)
  except
    NewUser.Free;
    raise;
  end;{try..except}
end;
{--------}
function TffServerConfiguration.GetGeneralInfo : PffGeneralInfo;
begin
  Result := @FGeneralInfo;
end;
{--------}
function TffServerConfiguration.GetServerName : string;                {!!.10}   
begin
  Result := FGeneralInfo.giServerName;
end;
{--------}
procedure TffServerConfiguration.Lock;
begin
  scPadLock.Lock;
end;
{--------}
procedure TffServerConfiguration.PerformDynamicLink;
var
  i      : integer;
  lTable : TffFullFileName;
  lDLL   : TffFullFileName;
  lBKName: TffName;
  lCKName: TffName;
begin
  Lock;
  try
    for i := 0 to pred(KeyProcList.Count) do begin
      with KeyProcList[i] do begin
        if not Link then begin
          lTable := TableDataFileName;
          lDLL := DLLName;
          lBKName := BuildKeyName;
          lCKName := CompareKeyName;
          FFRaiseException(EffServerException, ffStrResServer, fferrDynamicLink,
                           [lTable, IndexID, lDLL, lBKName, lCKName])
        end;
      end;
    end;
  finally
    Unlock;
  end;{try..finally}
end;
{--------}
procedure TffServerConfiguration.UnLock;
begin
  scPadLock.Unlock;
end;
{====================================================================}
end.
