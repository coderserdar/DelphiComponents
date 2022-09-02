{********************************************************************}
{* BTLCKMGR.INC - lock manager                                      *}
{********************************************************************}

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
 * The Original Code is TurboPower B-Tree Filer
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

{Notes:
  the lock manager interfaces between B-Tree Filer high level routines
                   and the operating system physical locks. The lock
                   manager performs the following actions:
                   - tracks all locks acquired and released for all
                     files,
                   - reference counts locks so that you can place the
                     same lock several times,
                   - enables all locks for a file handle to be
                     released.
                   - provides a way to determine whether a lock is
                     required or not, and if so returns the sub-locks
                     that need to be placed (this helps for the new
                     requirement that regions must be locked before
                     they are read from/written to).
                   The lock manager is not an object/class (the reason
                   is that BTF can be used by compilers that have
                   different object models) but is instead a global
                   variable (btfLocks) and a set of routines that act
                   on that variable).
  btfLockMgrAcqLock is a high level routine that gets another lock for
                    a file handle; if the lock already exists as a
                    lock item, the count is incremented; if not a new
                    lock is physically placed and a new lock item is
                    added to the list.
  btfLockMgrRelLock is a high level routine that releases a lock for
                    a file handle; the count for the lock item is
                    decremented and if it reaches zero the lock is
                    physically released and the lock item is removed
                    from the list.
  btfLockMgrLockExists is a high level routine that returns true if
                       the lock exists in the list
  btfLockMgrRelAllLocks is a high level routine that releases all
                        locks for a file handle
  btfLockMgrIsLockReq is a high level routine that determines whether
                      a lock is required or not; if a lock is required
                      then it returns true with the parameters of up
                      to two sub-locks required; if not it returns
                      false (please see extra notes below).
  btfLockMgrCreate is a high level routine that sets up the
                   btfFileLocks list.
  btfLockMgrDestroy is a high level routine that frees the
                    btfFileLocks list; no locks are physically
                    released, this routine is designed to be called at
                    program termination to release the list memory
                    back to the heap manager and it is assumed that
                    all locks will have already been released.
  Errors. The lock manager can return 5 error codes in variable
          btfLockMgrError:
            btferrLockMgrCannotAllocList -
                  not enough memory to allocate a new file or lock
                  list, or the maximum number of files or locks per
                  file has been exceeded
            btferrLockMgrLockLenMismatch -
                  an attempt was made to allocate a lock that
                  partially overlaps an existing one
            btferrLockMgrLockNotFound -
                  an attempt was made to release a lock that doesn't
                  appear in the lock list
            btferrLockMgrCannotLock -
                  the call to IsamLockRecord failed
            btferrLockMgrCannotUnlock -
                  the call to IsamUnlockRecord failed
 }

const
  btfcMaxFiles = 4000;  {max. number of file handles that can be tracked}
  btfcMaxLocks = 4000;  {max. number of locks per file that can be tracked}
  btfcLockDelta = 50;
  btfcFileDelta = 25;

  btferrLockMgrSuccess         = 0;
  btferrLockMgrCannotAllocList = 1;
  btferrLockMgrLockLenMismatch = 2;
  btferrLockMgrLockNotFound    = 3;
  btferrLockMgrCannotLock      = 4;
  btferrLockMgrCannotUnlock    = 5;

type
  PbtfLockItem = ^TbtfLockItem;
  TbtfLockItem = record
    bliOffset : longint;         {offset in file}
    bliLength : longint;         {length of lock}
    bliCount  : integer;         {count of locks}
  end;
  PbtfLockArray = ^TbtfLockArray;
  TbtfLockArray = record
    blaMaxCount : integer;       {maximum count of locks, 0..btfcMaxLocks}
    blaCount    : integer;       {current count of locks}
    blaLocks    : array [0..pred(btfcMaxLocks)] of TbtfLockItem;
  end;
  PbtfFileItem = ^TbtfFileItem;
  TbtfFileItem = record
    bfiHandle : integer;         {file handle}
    bfiLocks  : PbtfLockArray;   {array of locks for file}
  end;
  PbtfFileArray = ^TbtfFileArray;
  TbtfFileArray = record
    bfaMaxCount : integer;       {maximum count of files, 0..btfcMaxFiles}
    bfaCount    : integer;       {current count of files}
    bfaFiles    : array [0..pred(btfcMaxFiles)] of TbtfFileItem;
  end;

var
  btfFileLocks    : PbtfFileArray;
  btfLockMgrError : integer;

{===Lock Manager Routines============================================}
function btfLockMgrCalcFileArraySize(aFileCount : integer) : longint;
 {-calculate the memory size for a file array}
begin
  btfLockMgrCalcFileArraySize :=
     (2 * sizeof(integer)) + (longint(aFileCount) * sizeof(TbtfFileItem));
end;
{--------}
function btfLockMgrCalcLockArraySize(aLockCount : integer) : longint;
 {-calculate the memory size for a lock array}
begin
  btfLockMgrCalcLockArraySize :=
     (2 * sizeof(integer)) + (longint(aLockCount) * sizeof(TbtfLockItem));
end;
{--------}
function btfLockMgrResizeFileArray(var aFileArray : PbtfFileArray;
                                       aFileCount : integer) : Boolean;
 {-resize a file array to hold more files}
var
  OldSize      : longint;
  NewFileArray : PbtfFileArray;
begin
  btfLockMgrResizeFileArray := false;
  if (aFileArray = nil) then begin
    if (aFileCount <= btfcFileDelta) then
      aFileCount := btfcFileDelta;
  end
  else begin
    if (aFileCount <= aFileArray^.bfaMaxCount) then
      aFileCount := aFileArray^.bfaMaxCount + btfcFileDelta;
  end;
  if (aFileCount > btfcMaxFiles) or
     (not IsamGetMem(NewFileArray, btfLockMgrCalcFileArraySize(aFileCount))) then begin
    btfLockMgrError := btferrLockMgrCannotAllocList;
    Exit;
  end;
  if (aFileArray <> nil) then {a previous file array exists} begin
    OldSize := btfLockMgrCalcFileArraySize(aFileArray^.bfaMaxCount);
    Move(aFileArray^, NewFileArray^, OldSize);
    FreeMem(aFileArray, OldSize);
  end
  else {no previous file array} begin
    NewFileArray^.bfaCount := 0;
  end;
  NewFileArray^.bfaMaxCount := aFileCount;
  aFileArray := NewFileArray;
  btfLockMgrResizeFileArray := true;
end;
{--------}
function btfLockMgrResizeLockArray(var aLockArray : PbtfLockArray;
                                       aLockCount : integer) : Boolean;
 {-resize a lock array to hold more locks}
var
  OldSize      : longint;
  NewLockArray : PbtfLockArray;
begin
  btfLockMgrResizeLockArray := false;
  if (aLockArray = nil) then begin
    if (aLockCount <= btfcLockDelta) then
      aLockCount := btfcLockDelta;
  end
  else begin
    if (aLockCount <= aLockArray^.blaMaxCount) then
      aLockCount := aLockArray^.blaMaxCount + btfcLockDelta;
  end;
  if (aLockCount > btfcMaxLocks) or
     (not IsamGetMem(NewLockArray, btfLockMgrCalcLockArraySize(aLockCount))) then begin
    btfLockMgrError := btferrLockMgrCannotAllocList;
    Exit;
  end;
  if (aLockArray <> nil) then {a previous lock array exists} begin
    OldSize := btfLockMgrCalcLockArraySize(aLockArray^.blaMaxCount);
    Move(aLockArray^, NewLockArray^, OldSize);
    FreeMem(aLockArray, OldSize);
  end
  else {no previous lock array} begin
    NewLockArray^.blaCount := 0;
  end;
  NewLockArray^.blaMaxCount := aLockCount;
  aLockArray := NewLockArray;
  btfLockMgrResizeLockArray := true;
end;
{--------}
function btfLockMgrFindFile(aFileArray : PbtfFileArray;
                            aHandle    : integer;
                        var aInx       : integer) : boolean;
 {-find a file handle in a file array}
var
  L, R, M    : integer;
  FileHandle : integer;
begin
  {simple case - nothing there}
  if (aFileArray = nil) or (aFileArray^.bfaCount = 0) then begin
    btfLockMgrFindFile := false;
    aInx := 0;
    Exit;
  end;
  {binary search}
  L := 0;
  R := pred(aFileArray^.bfaCount);
  repeat
    M := (L + R) div 2;
    FileHandle := aFileArray^.bfaFiles[M].bfiHandle;
    if (aHandle < FileHandle) then
      R := pred(M)
    else if (aHandle > FileHandle) then
      L := succ(M)
    else begin
      btfLockMgrFindFile := true;
      aInx := M;
      Exit;
    end;
  until (L > R);
  aInx := L;
  btfLockMgrFindFile := false;
end;
{--------}
function btfLockMgrFindLock(aLockArray : PbtfLockArray;
                            aOffset    : longint;
                        var aInx       : integer) : boolean;
 {-find a lock (defined by its offset) in a lock array}
var
  L, R, M : integer;
  LockOfs : longint;
begin
  {simple case - no locks}
  if (aLockArray = nil) or (aLockArray^.blaCount = 0) then begin
    btfLockMgrFindLock := false;
    aInx := 0;
    Exit;
  end;
  {binary search}
  L := 0;
  R := pred(aLockArray^.blaCount);
  repeat
    M := (L + R) div 2;
    LockOfs := aLockArray^.blaLocks[M].bliOffset;
    if (aOffset < LockOfs) then
      R := pred(M)
    else if (aOffset > LockOfs) then
      L := succ(M)
    else begin
      btfLockMgrFindLock := true;
      aInx := M;
      Exit;
    end;
  until (L > R);
  aInx := L;
  btfLockMgrFindLock := false;
end;
{--------}
procedure btfLockMgrInsertFileItem(aFileArray : PbtfFileArray;
                                   aInx       : integer;
                                   aHandle    : longint);
 {-insert a new file item in a file array}
begin
  with aFileArray^ do begin
    if (aInx < bfaCount) then begin
      Move(bfaFiles[aInx],
           bfaFiles[aInx+1],
           (bfaCount - aInx) * sizeof(TbtfFileItem));
    end;
    with bfaFiles[aInx] do begin
      bfiHandle := aHandle;
      bfiLocks := nil;
      btfLockMgrResizeLockArray(bfiLocks, btfcLockDelta);
    end;
    inc(bfaCount);
  end;
end;
{--------}
procedure btfLockMgrInsertLockItem(aLockArray : PbtfLockArray;
                                   aInx       : integer;
                                   aOffset    : longint;
                                   aLength    : longint);
 {-insert a new lock item in a lock array}
begin
  with aLockArray^ do begin
    if (aInx < blaCount) then begin
      Move(blaLocks[aInx],
           blaLocks[aInx+1],
           (blaCount - aInx) * sizeof(TbtfLockItem));
    end;
    with blaLocks[aInx] do begin
      bliOffset := aOffset;
      bliLength := aLength;
      bliCount  := 1;
    end;
    inc(blaCount);
  end;
end;
{--------}
procedure btfLockMgrRemoveFileItem(aFileArray : PbtfFileArray;
                                   aInx       : integer);
 {-remove a file item from a file array}
begin
  with aFileArray^ do begin
    {free the file's lock array, if required}
    with bfaFiles[aInx] do begin
      if (bfiLocks <> nil) then
        FreeMem(bfiLocks,
                btfLockMgrCalcLockArraySize(bfiLocks^.blaMaxCount));
    end;
    {close up the hole held by the file}
    dec(bfaCount);
    if (aInx < bfaCount) then begin
      Move(bfaFiles[aInx+1],
           bfaFiles[aInx],
           (bfaCount - aInx) * sizeof(TbtfFileItem));
    end;
  end;
end;
{--------}
procedure btfLockMgrRemoveLockItem(aLockArray : PbtfLockArray;
                                   aInx       : integer);
 {-remove a lock item from a lock array}
begin
  with aLockArray^ do begin
    dec(blaCount);
    if (aInx < blaCount) then begin
      Move(blaLocks[aInx+1],
           blaLocks[aInx],
           (blaCount - aInx) * sizeof(TbtfLockItem));
    end;
  end;
end;
{--------}
function btfLockMgrAcqLock(aHandle     : integer;
                           aOffset     : longint;
                           aLength     : longint;
                           aTimeOut    : longint;
                           aTotalDelay : longint) : Boolean;
var
  LockInx   : integer;
  FileInx   : integer;
  LockArray : PbtfLockArray;
  FoundIt   : boolean;
begin
  {Assumption: if a lock already exists the length of the lock that is
               being acquired must equal the length of the lock in the
               lock list, otherwise an error is generated}

  btfLockMgrError := btferrLockMgrSuccess;
  btfLockMgrAcqLock := false;
  {create the file array, if required; add the first item}
  if (btfFileLocks = nil) then begin
    if not btfLockMgrResizeFileArray(btfFileLocks, btfcFileDelta) then
      Exit;
    btfLockMgrInsertFileItem(btfFileLocks, 0, aHandle);
    FileInx := 0;
  end
  {otherwise, find the index of the file item for the given handle;
   create it, if required (remember to check for resizing the file
   array)}
  else begin
    if not btfLockMgrFindFile(btfFileLocks, aHandle, FileInx) then begin
      with btfFileLocks^ do begin
        if (bfaCount = bfaMaxCount) then begin
          if not btfLockMgrResizeFileArray(btfFileLocks,
                                           bfaMaxCount + btfcFileDelta) then
            Exit;
        end;
      end;
      btfLockMgrInsertFileItem(btfFileLocks, FileInx, aHandle);
    end;
  end;

  {create the lock array for this file item, if required}
  LockArray := btfFileLocks^.bfaFiles[FileInx].bfiLocks;
  with LockArray^ do begin
    {check whether there's room in the lock array}
    if (blaCount = blaMaxCount) then begin
      if not btfLockMgrResizeLockArray(btfFileLocks^.bfaFiles[FileInx].bfiLocks,
                                       blaMaxCount + btfcLockDelta) then
        Exit;
      LockArray := btfFileLocks^.bfaFiles[FileInx].bfiLocks;
    end;

    {get the nearest lock (as defined by offset); if the lock is there
     return true and the element number, if not return false and the
     element number where it should be inserted}
    FoundIt := btfLockMgrFindLock(LockArray, aOffset, LockInx);

    {if it already exists, increment the lock count}
    if FoundIt then begin
      if (aLength <> blaLocks[LockInx].bliLength) then begin
        btfLockMgrError := btferrLockMgrLockLenMismatch;
        Exit;
      end;
      inc(blaLocks[LockInx].bliCount);
    end
    {if the lock doesn't exist, add it}
    else begin
      if IsamLockRecord(aOffset, aLength, aHandle, aTimeOut, aTotalDelay) then
        btfLockMgrInsertLockItem(LockArray, LockInx, aOffset, aLength)
      else begin
        btfLockMgrError := btferrLockMgrCannotLock;
        Exit;
      end;
    end;
  end;
  btfLockMgrAcqLock := true;
end;
{--------}
function btfLockMgrRelLock(aHandle : integer;
                           aOffset : longint;
                           aLength : longint) : Boolean;
var
  LockInx   : integer;
  FileInx   : integer;
  LockArray : PbtfLockArray;
  FoundIt   : boolean;
begin
  {Assumption: if a lock already exists the length of the lock that is
               being released must equal the length of the lock in the
               lock list, otherwise an error is generated}
  btfLockMgrError := btferrLockMgrSuccess;
  btfLockMgrRelLock := false;
  FoundIt := btfLockMgrFindFile(btfFileLocks, aHandle, FileInx);
  {if the file doesn't exist, report lock not found error}
  if not FoundIt then begin
    btfLockMgrError := btferrLockMgrLockNotFound;
    Exit;
  end;
  {the file was found}
  LockArray := btfFileLocks^.bfaFiles[FileInx].bfiLocks;
  with LockArray^ do begin
    {get the nearest lock (as defined by handle/offset); if the lock
     is there return true and the element number, if not return false}
    FoundIt := btfLockMgrFindLock(LockArray, aOffset, LockInx);

    {if it already exists, decrement the lock count; if the lock count
     goes to zero, remove the lock and the lock item}
    if FoundIt then begin
      if (aLength <> blaLocks[LockInx].bliLength) then begin
        btfLockMgrError := btferrLockMgrLockLenMismatch;
        Exit;
      end;
      if (blaLocks[LockInx].bliCount = 1) then begin
        if IsamUnlockRecord(aOffset, aLength, aHandle) then
          btfLockMgrRemoveLockItem(LockArray, LockInx)
        else begin
          btfLockMgrError := btferrLockMgrCannotUnlock;
          Exit;
        end;
      end
      else begin
        dec(blaLocks[LockInx].bliCount);
      end;
    end
    {if the lock doesn't exist, report error}
    else begin
      btfLockMgrError := btferrLockMgrLockNotFound;
      Exit;
    end;
  end;
  btfLockMgrRelLock := true;
end;
{--------}
function btfLockMgrLockExists(aHandle : integer;
                              aOffset : longint;
                              aLength : longint) : boolean;
var
  LockInx   : integer;
  FileInx   : integer;
  LockArray : PbtfLockArray;
  FoundIt   : boolean;
begin
  {Assumption: if a lock already exists the length of the lock that is
               being queried must equal the length of the lock in the
               lock list, otherwise an error is generated}
  btfLockMgrError := btferrLockMgrSuccess;
  FoundIt := btfLockMgrFindFile(btfFileLocks, aHandle, FileInx);
  if FoundIt then begin
    LockArray := btfFileLocks^.bfaFiles[FileInx].bfiLocks;
    FoundIt := btfLockMgrFindLock(LockArray, aOffset, LockInx);
    if FoundIt then begin
      if (aLength <> LockArray^.blaLocks[LockInx].bliLength) then begin
        btfLockMgrError := btferrLockMgrLockLenMismatch;
        FoundIt := false; {ie, it doesn't count if wrong length}
      end;
    end;
  end;
  btfLockMgrLockExists := FoundIt;
end;
{--------}
function btfLockMgrRelAllLocks(aHandle : integer) : Boolean;
var
  LockInx   : integer;
  FileInx   : integer;
  LockArray : PbtfLockArray;
begin
  btfLockMgrError := btferrLockMgrSuccess;
  btfLockMgrRelAllLocks := false;
  if btfLockMgrFindFile(btfFileLocks, aHandle, FileInx) then begin
    LockArray := btfFileLocks^.bfaFiles[FileInx].bfiLocks;
    {release all locks for this handle, by continually removing the
     lock at the end of the list until there are no more; by doing
     this from the end ensures a more efficient routine}
    with LockArray^ do begin
      for LockInx := pred(blaCount) downto 0 do begin
        with blaLocks[LockInx] do begin
          if IsamUnlockRecord(bliOffset, bliLength, aHandle) then
            btfLockMgrRemoveLockItem(LockArray, LockInx)
          else begin
            btfLockMgrError := btferrLockMgrCannotUnlock;
            Exit;
          end;
        end;
      end;
    end;
    btfLockMgrRemoveFileItem(btfFileLocks, FileInx);
  end;
  btfLockMgrRelAllLocks := true;
end;
{--------}
function btfLockMgrIsLockReq(aHandle    : integer;
                             aReqOffset : longint;
                             aReqLength : longint;
                         var aOffset1   : longint;
                         var aLength1   : longint;
                         var aOffset2   : longint;
                         var aLength2   : longint) : boolean;
  {------}
  function ReqLockIntersects(LockArray : PbtfLockArray;
                             aLockInx  : integer) : boolean;
  begin
    with LockArray^.blaLocks[aLockInx] do
      ReqLockIntersects :=
         ((bliOffset <= aReqOffset) and (aReqOffset < (bliOffset + bliLength))) or
         ((aReqOffset <= bliOffset) and (bliOffset < (aReqOffset + aReqLength)));
  end;
  {------}
var
  LockInx     : integer;
  FileInx     : integer;
  Inx         : integer;
  NextLockInx : integer;
  LockCount   : integer;
  CheckOffset : longint;
  SubLockToUse: integer;
  LockArray   : PbtfLockArray;
  FoundIt     : boolean;
  CurrentLockIntersects : boolean;
begin
  {Notes: the reason for this routine is to determine whether a lock
          is required and is used by the safe read/write routines
          (those which only read/write from a locked region, to combat
          the latest Win32 Microsoft caching problems). The routine
          attempts to see whether (1) the full lock has already been
          placed (in which case no lock is required) or (2) at least
          part of the lock region is already locked. In this second
          case, analysis of the standard B-Tree Filer locking method-
          ology has shown that this already-locked part is either at
          the front of the required region, at the back or in the
          middle. Hence only one or at most two sub-locks need to be
          placed to ensure the entire region is locked, and this
          routine will return those one or two sub-locks required.

          If the routine returns false, no lock(s) is/are required.

          If the routine returns true, at least one lock is required.
          and maybe two. The first lock is given by aOffset1/aLength1.
          If aLength2 is zero, no second sub-lock is required, other-
          wise aOffset2/aLength2 determines the second sub-lock.}

  {assume no sublocks are required}
  btfLockMgrIsLockReq := false;
  aOffset1 := 0;
  aLength1 := 0;
  aOffset2 := 0;
  aLength2 := 0;

  {check to see whether the file lock list is empty, if it is then
   obviously the lock will be required}
  if (btfFileLocks = nil) or (btfFileLocks^.bfaCount = 0) then begin
    btfLockMgrIsLockReq := true;
    aOffset1 := aReqOffset;
    aLength1 := aReqLength;
    Exit;
  end;

  {get the file item for this handle, if not found or if the file's
   lock list is empty then the lock is required}
  FoundIt := btfLockMgrFindFile(btfFileLocks, aHandle, FileInx);
  if (not FoundIt) or
     (btfFileLocks^.bfaFiles[FileInx].bfiLocks^.blaCount = 0) then begin
    btfLockMgrIsLockReq := true;
    aOffset1 := aReqOffset;
    aLength1 := aReqLength;
    Exit;
  end;

  LockArray := btfFileLocks^.bfaFiles[FileInx].bfiLocks;
  with LockArray^ do begin
    {get the nearest lock (as defined by handle/offset); if the lock
     is there return true and the element number, if not return false}
    FoundIt := btfLockMgrFindLock(LockArray, aReqOffset, LockInx);

    {while the previous lock in the list is for the same handle and
     intersects our required region, decrement the lock index: ie,
     find the first lock in the series that intersects our region}
    if FoundIt then begin
      CurrentLockIntersects := true;
    end
    else begin
      if (LockInx = blaCount) then
        CurrentLockIntersects := false
      else
        CurrentLockIntersects := ReqLockIntersects(LockArray, LockInx);
      while (LockInx > 0) and
            ReqLockIntersects(LockArray, pred(LockInx)) do begin
        dec(LockInx);
        CurrentLockIntersects := true;
      end;
    end;

    {if the current lock (as defined by LockInx) intersects our
     region, count the succeeding locks in the list that are for the
     same handle and that intersect our required region}
    if CurrentLockIntersects then begin
      NextLockInx := succ(LockInx);
      while (NextLockInx < blaCount) and
            ReqLockIntersects(LockArray, NextLockInx) do
        inc(NextLockInx);
      LockCount := NextLockInx - LockInx;
    end
    {the current lock doesn't intersect our region, therefore none of
     the locks in the list will}
    else begin
      btfLockMgrIsLockReq := true;
      aOffset1 := aReqOffset;
      aLength1 := aReqLength;
      Exit;
    end;

    {if we reach this point we know that at least one existing lock
     intersects our region, and we know the total number that do;
     identify at most two sub-locks required (again, to repeat,
     analysis of B-Tree Filer's locking behaviour has shown that at
     most two sub-locks would be required)}
    CheckOffset := aReqOffset;
    SubLockToUse := 1;
    for Inx := LockInx to pred(LockInx + LockCount) do begin
      with blaLocks[Inx] do begin
        if (CheckOffset < bliOffset) then begin
          btfLockMgrIsLockReq := true;
          if (SubLockToUse = 1) then begin
            aOffset1 := CheckOffset;
            aLength1 := bliOffset - CheckOffset;
          end
          else begin
            aOffset2 := CheckOffset;
            aLength2 := bliOffset - CheckOffset;
          end;
          inc(SubLockToUse);
          if (SubLockToUse > 2) then
            Exit;
        end;
        CheckOffset := bliOffset + bliLength;
      end;
    end;
    if (CheckOffset < (aReqOffset + aReqLength)) then begin
      btfLockMgrIsLockReq := true;
      with blaLocks[pred(LockInx + LockCount)] do begin
        if (SubLockToUse = 1) then begin
          aOffset1 := CheckOffset;
          aLength1 := aReqOffset + aReqLength - CheckOffset;
        end
        else begin
          aOffset2 := CheckOffset;
          aLength2 := aReqOffset + aReqLength - CheckOffset;
        end;
      end;
    end;
  end;
end;
{--------}
procedure btfLockMgrCreate;
begin
  btfLockMgrError := btferrLockMgrSuccess;
  btfFileLocks := nil;
end;
{--------}
procedure btfLockMgrDestroy;
var
  FileInx : integer;
begin
  btfLockMgrError := btferrLockMgrSuccess;
  if (btfFileLocks <> nil) then begin
    with btfFileLocks^ do begin
      for FileInx := pred(bfaCount) downto 0 do
        btfLockMgrRemoveFileItem(btfFileLocks, FileInx);
    end;
    FreeMem(btfFileLocks,
            btfLockMgrCalcFileArraySize(btfFileLocks^.bfaMaxCount));
    btfFileLocks := nil;
  end;
end;
{====================================================================}

