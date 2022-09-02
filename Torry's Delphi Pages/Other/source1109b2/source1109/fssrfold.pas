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

{$I fsdefine.inc}

Unit fssrfold;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  fsllbase,
  fsllunc,
  fshash,
  fssrbase,
  fssrlock,
  fssrtran;

Type
  TfsSrcFolder = Class(TfsSelfListItem)
  Protected {private}
    {Begin !!.11}
    FExistingTableVersion: Longint;
    { !!! WARNING: This is intended for testing & utility purposes only. !!!
      Holds the version # to be assigned to existing tables.
      Default value is zero. If zero then the version number already in the
      table is used. }
{End !!.11}
    FHash: TffWord32;
    FLockMgr: TfsLockManager;
    {Begin !!.11}
    FNewTableVersion: Longint;
    { !!! WARNING: This is intended for testing & utility purposes only. !!!
      Holds the version # of tables that are created in this folder.
      Default value is constant fsVersionNumber declared in unit FFLLBASE. }
    FPackSrcTableVersion: Longint;
    { !!! WARNING: This is intended for testing & utility purposes only. !!!
      Holds the version # of source tables that are being opened in this
      folder for a pack operation.
      Default value is zero. If zero then the version number already in the
      table is used. }
{End !!.11}
    FPath: PffShStr;
    FRefCount: Integer;
    FTranMgr: TfsSrcTransactionMgr;
  Protected
    Function GetPath: TffPath;

    Function CanDelete: boolean;

    {Begin !!.11}
    Procedure SetExistingTableVersion(Const Version: Longint);
    Procedure SetNewTableVersion(Const Version: Longint);
    Procedure SetPackSrcTableVersion(Const Version: Longint);
    {End !!.11}
  Public
    Constructor Create(Const aPath: TffPath;
      Const isReadOnly: boolean;
      aBufMgr: TfsBufferManager);
    Destructor Destroy; Override;

    Procedure DecRefCount;
    Procedure IncRefCount;

    {Begin !!.11}
    Property ExistingTableVersion: Longint
      Read FExistingTableVersion
      Write SetExistingTableVersion;
    { !!! WARNING: This property is intended for testing & utility
      purposes only. !!!
      The version number to be assigned to existing tables opened in this
      folder. Default value is zero which causes the version number in the
      table to be used. }
{End !!.11}
    Property FolderID: Longint Read KeyAsInt;
    Property FolderReferences: Integer Read FRefCount;
    Property LockMgr: TfsLockManager Read FLockMgr;
    {Begin !!.11}
    Property NewTableVersion: Longint
      Read FNewTableVersion
      Write SetNewTableVersion;
    { !!! WARNING: This property is intended for testing & utility
      purposes only. !!!
      The version number to be assigned to new tables created in this
      folder. Default value is constant fsVersionNumber declared in unit
      FFLLBASE. }
    Property PackSrcTableVersion: Longint
      Read FPackSrcTableVersion
      Write SetPackSrcTableVersion;
    { !!! WARNING: This property is intended for testing & utility
      purposes only. !!!
      The version number to be assigned to source tables opened by the
      pack operation. Default value is zero which causes the version number
      in the table to be used. }
{End !!.11}
    Property Path: TffPath Read GetPath;
    Property PathHash: TffWord32 Read FHash;
    Property RefCount: Integer Read FRefCount;
    Property TransactionMgr: TfsSrcTransactionMgr Read FTranMgr;
  End;

  TfsSrcFolderList = Class(TFSSpecObject)
  Protected {private}
    FList: TFSSpecThreadList;
  Protected
    Function GetFolderItem(Find: TfsListFindType; Value: Longint): TfsSrcFolder;
    Function plIndexOf(Const aPath: TffPath): Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function AddFolder(Const aPath: TffPath;
      Const isReadOnly: boolean;
      aBufMgr: TfsBufferManager): TfsSrcFolder;
    { If a folder for the specified path does not exist in the folder list
      then this method creates and adds a new folder to the folder list.
      Otherwise it increments the reference count on the existing folder. }

    Function BeginRead: TfsSrcFolderList;
    {-A thread must call this method to gain read access to the list.
      Returns the instance of this object as a convenience. }

    Function BeginWrite: TfsSrcFolderList;
    {-A thread must call this method to gain write access to the list.
      Returns the instance of this object as a convenience.}

    Procedure DeleteFolder(Const aPath: TffPath);
    Procedure DeleteFolderByID(aFolderID: Longint);

    Procedure EndRead;
    {-A thread must call this method when it no longer needs read access
      to the list.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    {-A thread must call this method when it no longer needs write access
      to the list.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }

    Function ExistsPath(Const aPath: TffPath): boolean;
    Procedure RemoveUnusedFolders;

    Property Folder[Find: TfsListFindType;
    Value: Longint]: TfsSrcFolder
    Read GetFolderItem;
  End;

Implementation

{===TffSrPath========================================================}

Constructor TfsSrcFolder.Create(Const aPath: TffPath;
  Const isReadOnly: boolean;
  aBufMgr: TfsBufferManager);
Var
  UNC: TffShStr;
Begin
  Inherited Create;
  UNC := FFShStrUpper(FFExpandUNCFileName(aPath));
  FPath := FFShStrAlloc(UNC);
  FHash := FSCalcShStrELFHash(FPath^);
  FLockMgr := TfsLockManager.Create;
  FExistingTableVersion := 0; {!!.11}
  FNewTableVersion := fsVersionNumber; {!!.11}
  FPackSrcTableVersion := 0; {!!.11}
  FTranMgr := TfsSrcTransactionMgr.Create(aBufMgr, FLockMgr, FPath^, isReadOnly);
End;
{--------}

Destructor TfsSrcFolder.Destroy;
Begin
  FFShStrFree(FPath);
  FLockMgr.Free;
  FLockMgr := Nil;
  FTranMgr.Free;
  FTranMgr := Nil;
  Inherited Destroy;
End;
{--------}

Function TfsSrcFolder.CanDelete: boolean;
Begin
  dec(FRefCount);
  Result := (FRefCount = 0);
End;
{--------}

Procedure TfsSrcFolder.DecRefCount;
Begin
  If FRefCount > 0 Then
    dec(FRefCount);
End;
{--------}

Function TfsSrcFolder.GetPath: TffPath;
Begin
  Result := FPath^;
End;
{--------}

Procedure TfsSrcFolder.IncRefCount;
Begin
  inc(FRefCount);
End;
{Begin !!.11}
{--------}

Procedure TfsSrcFolder.SetExistingTableVersion(Const Version: Longint);
Begin
  If Version <> FExistingTableVersion Then
    FExistingTableVersion := Version;
End;
{--------}

Procedure TfsSrcFolder.SetNewTableVersion(Const Version: Longint);
Begin
  If Version <> FNewTableVersion Then
    FNewTableVersion := Version;
End;
{--------}

Procedure TfsSrcFolder.SetPackSrcTableVersion(Const Version: Longint);
Begin
  If Version <> FPackSrcTableVersion Then
    FPackSrcTableVersion := Version;
End;
{End !!.11}
{====================================================================}

{===TfsSrcFolderList====================================================}

Constructor TfsSrcFolderList.Create;
Begin
  Inherited Create;
  FList := TFSSpecThreadList.Create;
End;
{--------}

Destructor TfsSrcFolderList.Destroy;
Begin
  FList.Free;
  Inherited Destroy;
End;
{--------}

Function TfsSrcFolderList.AddFolder(Const aPath: TffPath;
  Const isReadOnly: boolean;
  aBufMgr: TfsBufferManager): TfsSrcFolder;
Var
  Inx: Integer;
Begin
  Inx := plIndexOf(aPath);
  If (Inx <> -1) Then
    Begin
      Result := TfsSrcFolder(FList[Inx]);
      Result.IncRefCount;
    End
  Else
    Begin
      Result := TfsSrcFolder.Create(aPath, isReadOnly, aBufMgr);
      Try
        FList.Insert(Result);
        Result.IncRefCount;
      Except
        Result.Free;
        Raise;
      End;
    End;
End;
{--------}

Function TfsSrcFolderList.BeginRead: TfsSrcFolderList;
Begin
  FList.BeginRead;
  Result := Self;
End;
{--------}

Function TfsSrcFolderList.BeginWrite: TfsSrcFolderList;
Begin
  FList.BeginWrite;
  Result := Self;
End;
{--------}

Procedure TfsSrcFolderList.DeleteFolder(Const aPath: TffPath);
Var
  Inx: Integer;
  Item: TfsSrcFolder;
Begin
  Inx := plIndexOf(aPath);
  If (Inx <> -1) Then
    Begin
      Item := TfsSrcFolder(FList[Inx]);
      If Item.CanDelete Then
        FList.DeleteAt(Inx);
    End;
End;
{--------}

Procedure TfsSrcFolderList.DeleteFolderByID(aFolderID: Longint);
Var
  Inx: Integer;
  Item: TfsSrcFolder;
Begin
  Inx := FList.Index(aFolderID);
  If (Inx <> -1) Then
    Begin
      Item := TfsSrcFolder(FList[Inx]);
      If Item.CanDelete Then
        FList.DeleteAt(Inx);
    End;
End;
{--------}

Procedure TfsSrcFolderList.EndRead;
Begin
  FList.EndRead;
End;
{--------}

Procedure TfsSrcFolderList.EndWrite;
Begin
  FList.EndWrite;
End;
{--------}

Function TfsSrcFolderList.ExistsPath(Const aPath: TffPath): boolean;
Begin
  Result := plIndexOf(aPath) <> -1;
End;
{--------}

Function TfsSrcFolderList.GetFolderItem(Find: TfsListFindType; Value: Longint): TfsSrcFolder;
Var
  Inx: Integer;
Begin
  Result := Nil;
  If (Find = ftFromID) Then
    Begin
      Inx := FList.Index(Value);
      If (Inx <> -1) Then
        Result := TfsSrcFolder(FList[Inx]);
    End
  Else {Find = ftFromIndex} If (0 <= Value) And (Value < FList.Count) Then
      Result := TfsSrcFolder(FList[Value]);
End;
{--------}

Function TfsSrcFolderList.plIndexOf(Const aPath: TffPath): Integer;
Var
  i: Integer;
  Hash: TffWord32;
  Path: TfsSrcFolder;
  UNC: TffShStr;
Begin
  UNC := FFShStrUpper(FFExpandUNCFileName(aPath));
  Hash := FSCalcShStrELFHash(UNC);
  For i := 0 To pred(FList.Count) Do
    Begin
      Path := TfsSrcFolder(FList[i]);
      If (Path.PathHash = Hash) Then
        If (FFCmpShStr(Path.Path, UNC, 255) = 0) Then
          Begin
            Result := i;
            Exit;
          End;
    End;
  Result := -1;
End;
{--------}

Procedure TfsSrcFolderList.RemoveUnusedFolders;
Var
  Inx: Integer;
  Item: TfsSrcFolder;
Begin
  FList.BeginWrite;
  Try
    For Inx := pred(FList.Count) Downto 0 Do
      Begin
        Item := TfsSrcFolder(FList[Inx]);
        If Item.RefCount = 0 Then
          FList.DeleteAt(Inx);
      End;
  Finally
    FList.EndWrite;
  End;
End;

{====================================================================}

End.

