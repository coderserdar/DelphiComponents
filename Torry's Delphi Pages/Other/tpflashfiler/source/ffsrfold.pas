{*********************************************************}
{* FlashFiler: Folder and folder list objects for Server *}
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

unit ffsrfold;

interface

uses
  Windows,
  Messages,
  SysUtils,
  ffllbase,
  ffllunc,
  ffhash,
  ffsrbase,
  ffsrlock,
  ffsrtran;

type
  TffSrFolder = class(TffSelfListItem)
    protected {private}
{Begin !!.11}
      FExistingTableVersion : Longint;
        { !!! WARNING: This is intended for testing & utility purposes only. !!!
          Holds the version # to be assigned to existing tables.
          Default value is zero. If zero then the version number already in the
          table is used. }
{End !!.11}
      FHash     : TffWord32;
      FLockMgr  : TffLockManager;
{Begin !!.11}
      FNewTableVersion : Longint;
        { !!! WARNING: This is intended for testing & utility purposes only. !!!
          Holds the version # of tables that are created in this folder.
          Default value is constant FFVersionNumber declared in unit FFLLBASE. }
      FPackSrcTableVersion : Longint;
        { !!! WARNING: This is intended for testing & utility purposes only. !!!
          Holds the version # of source tables that are being opened in this
          folder for a pack operation.
          Default value is zero. If zero then the version number already in the
          table is used. }
{End !!.11}
      FPath     : PffShStr;
      FRefCount : integer;
      FTranMgr  : TffSrTransactionMgr;
    protected
      function GetPath : TffPath;

      function CanDelete : boolean;

{Begin !!.11}
      procedure SetExistingTableVersion(const Version : Longint);
      procedure SetNewTableVersion(const Version : Longint);
      procedure SetPackSrcTableVersion(const Version : Longint);
{End !!.11}
    public
      constructor Create(const aPath : TffPath;
                         const isReadOnly : boolean;
                               aBufMgr : TffBufferManager);
      destructor Destroy; override;

      procedure DecRefCount;
      procedure IncRefCount;

{Begin !!.11}
      property ExistingTableVersion : Longint
        read FExistingTableVersion
        write SetExistingTableVersion;
        { !!! WARNING: This property is intended for testing & utility
          purposes only. !!!
          The version number to be assigned to existing tables opened in this
          folder. Default value is zero which causes the version number in the
          table to be used. }
{End !!.11}
      property FolderID : longint read KeyAsInt;
      property FolderReferences : integer read FRefCount;
      property LockMgr : TffLockManager read FLockMgr;
{Begin !!.11}
      property NewTableVersion : Longint
        read FNewTableVersion
        write SetNewTableVersion;
        { !!! WARNING: This property is intended for testing & utility
          purposes only. !!!
          The version number to be assigned to new tables created in this
          folder. Default value is constant FFVersionNumber declared in unit
          FFLLBASE. }
      property PackSrcTableVersion : Longint
        read FPackSrcTableVersion
        write SetPackSrcTableVersion;
        { !!! WARNING: This property is intended for testing & utility
          purposes only. !!!
          The version number to be assigned to source tables opened by the
          pack operation. Default value is zero which causes the version number
          in the table to be used. }
{End !!.11}
      property Path : TffPath read GetPath;
      property PathHash : TffWord32 read FHash;
      property RefCount : integer read FRefCount;
      property TransactionMgr : TffSrTransactionMgr read FTranMgr;
  end;

  TffSrFolderList = class(TffObject)                                  
    protected {private}
      FList : TffThreadList;
    protected
      function GetFolderItem(Find : TffListFindType; Value : longint) : TffSrFolder;
      function plIndexOf(const aPath : TffPath) : integer;
    public
      constructor Create;
      destructor Destroy; override;

      function AddFolder(const aPath : TffPath;
                         const isReadOnly : boolean;
                               aBufMgr : TffBufferManager) : TffSrFolder;
        { If a folder for the specified path does not exist in the folder list
          then this method creates and adds a new folder to the folder list.
          Otherwise it increments the reference count on the existing folder. }
          
      function BeginRead : TffSrFolderList;
        {-A thread must call this method to gain read access to the list.
          Returns the instance of this object as a convenience. }

      function BeginWrite : TffSrFolderList;
        {-A thread must call this method to gain write access to the list.
          Returns the instance of this object as a convenience.}

      procedure DeleteFolder(const aPath : TffPath);
      procedure DeleteFolderByID(aFolderID : longint);

      procedure EndRead;
        {-A thread must call this method when it no longer needs read access
          to the list.  If it does not call this method, all writers will
          be perpetually blocked. }

      procedure EndWrite;
        {-A thread must call this method when it no longer needs write access
          to the list.  If it does not call this method, all readers and writers
          will be perpetualy blocked. }

      function ExistsPath(const aPath : TffPath) : boolean;
      procedure RemoveUnusedFolders;
  
      property Folder[Find : TffListFindType; Value : longint] : TffSrFolder
         read GetFolderItem;
  end;

implementation

{===TffSrPath========================================================}
constructor TffSrFolder.Create(const aPath : TffPath;
                               const isReadOnly : boolean;
                                     aBufMgr : TffBufferManager);
var
  UNC : TffShStr;
begin
  inherited Create;
  UNC := FFShStrUpper(FFExpandUNCFileName(aPath));
  FPath := FFShStrAlloc(UNC);
  FHash := FFCalcShStrELFHash(FPath^);
  FLockMgr := TffLockManager.Create;
  FExistingTableVersion := 0;                                         {!!.11}
  FNewTableVersion := FFVersionNumber;                                {!!.11}
  FPackSrcTableVersion := 0;                                          {!!.11}
  FTranMgr := TffSrTransactionMgr.Create(aBufMgr, FLockMgr, FPath^, isReadOnly);
end;
{--------}
destructor TffSrFolder.Destroy;
begin
  FFShStrFree(FPath);
  FLockMgr.Free;
  FLockMgr := nil;
  FTranMgr.Free;
  FTranMgr := nil;
  inherited Destroy;
end;
{--------}
function TffSrFolder.CanDelete : boolean;
begin
  dec(FRefCount);
  Result := (FRefCount = 0);
end;
{--------}
procedure TffSrFolder.DecRefCount;
begin
  if FRefCount > 0 then
    dec(FRefCount);
end;
{--------}
function TffSrFolder.GetPath : TffPath;
begin
  Result := FPath^;
end;
{--------}
procedure TffSrFolder.IncRefCount;
begin
  inc(FRefCount);
end;
{Begin !!.11}
{--------}
procedure TffSrFolder.SetExistingTableVersion(const Version : Longint);
begin
  if Version <> FExistingTableVersion then
    FExistingTableVersion := Version;
end;
{--------}
procedure TffSrFolder.SetNewTableVersion(const Version : Longint);
begin
  if Version <> FNewTableVersion then
    FNewTableVersion := Version;
end;
{--------}
procedure TffSrFolder.SetPackSrcTableVersion(const Version : Longint);
begin
  if Version <> FPackSrcTableVersion then
    FPackSrcTableVersion := Version;
end;
{End !!.11}
{====================================================================}


{===TffSrFolderList====================================================}
constructor TffSrFolderList.Create;
begin
  inherited Create;
  FList := TffThreadList.Create;
end;
{--------}
destructor TffSrFolderList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
{--------}
function TffSrFolderList.AddFolder(const aPath : TffPath;
                                   const isReadOnly : boolean;
                                         aBufMgr : TffBufferManager) : TffSrFolder;
var
  Inx     : integer;
begin
  Inx := plIndexOf(aPath);
  if (Inx <> -1) then begin
    Result := TffSrFolder(FList[Inx]);
    Result.IncRefCount;
  end
  else begin
    Result := TffSrFolder.Create(aPath, isReadOnly, aBufMgr);
    try
      FList.Insert(Result);
      Result.IncRefCount;
    except
      Result.Free;
      raise;
    end;
  end;
end;
{--------}
function TffSrFolderList.BeginRead : TffSrFolderList;
begin
  FList.BeginRead;
  Result := Self;
end;
{--------}
function TffSrFolderList.BeginWrite : TffSrFolderList;
begin
  FList.BeginWrite;
  Result := Self;
end;
{--------}
procedure TffSrFolderList.DeleteFolder(const aPath : TffPath);
var
  Inx  : integer;
  Item : TffSrFolder;
begin
  Inx := plIndexOf(aPath);
  if (Inx <> -1) then begin
    Item := TffSrFolder(FList[Inx]);
    if Item.CanDelete then
      FList.DeleteAt(Inx);
  end;
end;
{--------}
procedure TffSrFolderList.DeleteFolderByID(aFolderID : longint);
var
  Inx  : integer;
  Item : TffSrFolder;
begin
  Inx := FList.Index(aFolderID);
  if (Inx <> -1) then begin
    Item := TffSrFolder(FList[Inx]);
    if Item.CanDelete then
      FList.DeleteAt(Inx);
  end;
end;
{--------}
procedure TffSrFolderList.EndRead;
begin
  FList.EndRead;
end;
{--------}
procedure TffSrFolderList.EndWrite;
begin
  FList.EndWrite;
end;
{--------}
function TffSrFolderList.ExistsPath(const aPath : TffPath) : boolean;
begin
  Result := plIndexOf(aPath) <> -1;
end;
{--------}
function TffSrFolderList.GetFolderItem(Find : TffListFindType; Value : longint) : TffSrFolder;
var
  Inx : integer;
begin
  Result := nil;
  if (Find = ftFromID) then begin
    Inx := FList.Index(Value);
    if (Inx <> -1) then
      Result := TffSrFolder(FList[Inx]);
  end
  else {Find = ftFromIndex}
    if (0 <= Value) and (Value < FList.Count) then
      Result := TffSrFolder(FList[Value]);
end;
{--------}
function TffSrFolderList.plIndexOf(const aPath : TffPath) : integer;
var
  i    : integer;
  Hash : TffWord32;
  Path : TffSrFolder;
  UNC  : TffShStr;
begin
  UNC := FFShStrUpper(FFExpandUNCFileName(aPath));
  Hash := FFCalcShStrELFHash(UNC);
  for i := 0 to pred(FList.Count) do begin
    Path := TffSrFolder(FList[i]);
    if (Path.PathHash = Hash) then
      if (FFCmpShStr(Path.Path, UNC, 255) = 0) then begin
        Result := i;
        Exit;
      end;
  end;
  Result := -1;
end;
{--------}                                                           
procedure TffSrFolderList.RemoveUnusedFolders;
var
  Inx : integer;
  Item : TffSrFolder;
begin
  FList.BeginWrite;
  try
    for Inx := pred(FList.Count) downto 0 do begin
      Item := TffSrFolder(FList[Inx]);
      if Item.RefCount = 0 then
        FList.DeleteAt(Inx);
    end;
  finally
    FList.EndWrite;
  end;
end;

{====================================================================}

end.
