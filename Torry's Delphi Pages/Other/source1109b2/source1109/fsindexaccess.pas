{*********************************************************}
{* FlashFiler: Table b-tree index access                 *}
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

Unit fsindexaccess;

Interface

Uses
  SysUtils,
  fsconst,
  fsllbase,
  fssrmgr,
  fsllexcp,
  fssrintf,
  fssrbase,
  fsfile,
  fssrlock,
  fstablehelper,
  fsdictserveraccess;

{$IFDEF FF_DEBUG}
Var
  FFDEBUG_IndexCounter: Record
    Splits,
      RotateLeftNode,
      RotateRightNode,
      RotateLeftLeaf,
      RotateRightLeaf,
      Merge,
      SwapNext,
      SwapPrev: Integer;
  End;
  {$ENDIF}

Type
  PffKeyIndexData = ^TffKeyIndexData;
  TffKeyIndexData = Record {Data record for key routines}
    {must be supplied}
    kidFI: PffFileInfo; {..index file}
    kidIndex: Integer; {..index number}
    kidCompare: TffKeyCompareFunc; {..compare routine}
    kidCompareData: PffCompareData; {..compare data}
    {calculated internally}
    kidFileHeader: PffBlockHeaderFile; {..pointer to the index file header}
    kidIndexHeader: PffIndexHeader; {..pointer to the index header}
    {used elsewhere}
    kidIndexType: TfsIndexType; {..index type: composite or user-defined}
  End;

Type
  {Note: an key path is a structure which defines a particular
         key in a B-Tree. The path defines the page numbers and the
         element numbers into the key arrays in the pages to get to
         a particular key. If the position is OnKey then the keypath
         points exactly to that key. If the position is OnCrack then
         the keypath points to the key that would be retrieved by a
         NextKey or a PrevKey operation.
         An invalid keypath has an path element count of zero and a
         position of Unknown}
  TffKeyPathPosition = ({Possible positions of a keypath..}
    kppUnknown, {..unknown}
    kppBOF, {..before all keys}
    kppOnCrackBefore, {..in between two keys}
    kppOnCrackAfter, {..in between two keys}
    kppOnKey, {..on a key}
    kppEOF); {..after all keys}
  TffKeyPathElement = Record {An element of a key path}
    kpePage: TffWord32; {..the page number}
    kpeItem: Integer; {..the element number of the key}
  End;
  PffKeyPath = ^TffKeyPath;
  TffKeyPath = Record {The key path type}
    kpCount: Integer; {..number of active elements in the path}
    kpPath: Array[0..31] Of TffKeyPathElement; {..the path}
    kpPos: TffKeyPathPosition; {..it's position}
    kpLSN: TffWord32; {...LSN of the index map page at the time
    we positioned to this record.  If the LSN
    has changed then our key path is no longer
    valid. }
  End;
  {Note: 32 elements is *ample*, 32 levels in a sparsely populated order 3
         B-Tree would hold 4 billion keys: anyone who creates such a B-Tree
         (ie 1Kb keys using a 4Kb block) deserve what they get if they even
         could.}

{---Key path related routines---}
Procedure FFInitKeyPath(Var aKeyPath: TffKeyPath);
Procedure FFSetKeyPathToBOF(Var aKeyPath: TffKeyPath);
Procedure FFSetKeyPathToEOF(Var aKeyPath: TffKeyPath);

{---Index related routines--- (NOT THREAD-SAFE)}
Procedure FFTblAddIndex(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aIndex: Integer;
  aMaxKeyLen: Integer;
  aAllowDups: boolean;
  aKeysAreRefs: boolean);
{-Add an index to a file}
Procedure FFTblDeleteIndex(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aIndex: Integer);
{-Delete an index from a file}
Procedure FFTblPrepareIndexes(aFI: PffFileInfo;
  aTI: PffTransInfo);
{-Prepare a file to contain indexes}

{---Key access related routines--- (NOT THREAD-SAFE)}
Procedure FFTblDeleteAllKeys(aTI: PffTransInfo; Var aIndex: TffKeyIndexData);
{-Delete all keys from an index.
  Note: cannot be used inside a transaction--it implements a
        low level file update}
Function FFTblDeleteKey(Const aTI: PffTransInfo;
  Const aKey: PffByteArray;
  Const aRefNr: TffInt64;
  Var aIndex: TffKeyIndexData;
  Var aBTreeChanged: Boolean): Boolean; {!!.05}
{-Delete a key/ref from an index}
Function FFTblFindKey(Var aIndex: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aAction: TffSearchKeyAction): boolean;
{-Find the given key/ref (or nearby one) in the index, set up the
  keypath and the key/ref found, return true; if key/ref not found,
  return false and an invalid keypath. Note that if the index allows
  dups and the refnr is zero and the key matches, the first matching
  key/ref is returned. Note also the keypath is positioned on the
  crack for the key/ref in question.}
Function FFTblGetApproxPos(Var aIndex: TffKeyIndexData;
  Var aPos: Integer;
  aTI: PffTransInfo;
  Const aKeyPath: TffKeyPath): boolean;
{-Given a valid keypath to key/ref, calculate the approx position of
  that key/ref in the b-tree as percentage.}
Function FFTblInsertKey(Var aIndex: TffKeyIndexData;
  Const aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray): boolean;
{-Insert a key/ref into an index}
Function FFTblKeyExists(Var aIndex: TffKeyIndexData;
  Const aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray): boolean;
{-Return true if key/ref exists in index.  If the lock duration is
  ffldShort then index locks are released once this method has finished
  using the index pages. }
Function FFTblNextKey(Var aIndex: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): boolean;
{-Given a keypath, find the next key/ref in the index, set up the
  keypath and key/ref to it, return true; if no next key, return
  false and set keypath to EOF}
Function FFTblPrevKey(Var aIndex: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): boolean;
{-Given a keypath, find the previous key/ref in the index, set up
  the keypath and key/ref to it, return true; if no previous key,
  return false and set keypath to BOF}
Function FFTblSetApproxPos(Var aIndex: TffKeyIndexData;
  aPos: Integer;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): boolean;
{-Set the keypath to the approximate position given by aPos (a percentage),
  return true and the key/ref if able to, return false if not. The returned
  keypath will have length 2, unless the b-tree only consists of the root
  page, in which case it will be length 1.

  Note: All index pages accessed by this method are Share locked for duration
        ffldCommit.}

Implementation

{Notes: to optimize disk space, there are four types of btree pages:
          A: standard keys, node page
          B: standard keys, leaf page
          C: record reference number keys, node page
          D: record reference number keys, leaf page
        They have a dynamic (ie not static) format that depends on the
        block size of the file and the length of the keys. Ignoring
        the block header, in hand-waving terms the format of each
        block is as follows:
          A: an array of page numbers (32 bits, 4 bytes each),
             followed by an array of reference numbers (64 bits, 8
             bytes each), followed by an array of keys (variable
             length). All arrays have the same number of elements. The
             "page before all keys" number is stored in the block
             header.
          B: an array of reference numbers (64 bits, 8 bytes each),
             followed by an array of keys (variable length). Both
             arrays have the same number of elements.
          C: an array of page numbers (32 bits, 4 bytes each),
             followed by an array of reference numbers (64 bits, 8
             bytes each). Both arrays have the same number of
             elements. The "page before all keys" number is stored in
             the block header. The reference numbers are the keys.
          D: an array of reference numbers (64 bits, 8 bytes each).
             The reference numbers are the keys.
        The number of elements in the arrays MUST be odd because of
        the btree algorithm used. To calculate the number of elements
        in each array (the example values shown refer to a 4KB block
        with key length 20):
          A: take the block size, subtract the size of the header,
             divide by [sizeof(page number) + sizeof(ref number) + key
             length]. If not odd, subtract 1.
             Example: (4096 - 32) / (4 + 8 + 20) = 127
          B: take the block size, subtract the size of the header,
             divide by [sizeof(ref number) + key length]. If not odd,
             subtract 1.
             Example: (4096 - 32) / (8 + 20) = 145
          C: take the block size, subtract the size of the header,
             divide by [sizeof(page number) + sizeof(ref number)]. If
             not odd, subtract 1.
             Example: (4096 - 32) / (4 + 8) = 338; minus 1 = 337
          D: take the block size, subtract the size of the header,
             divide by sizeof(ref number). If not odd, subtract 1.
             Example: (4096 - 32) / 8 = 508; minus 1 = 507

        References:
          File Structures, Zoellick & Folk, Addison Wesley
          Introduction to Algorithms, Cormen, etc., McGraw-Hill
          Data Structures, Algorithms, and Performance, Wood, Addison Wesley

        Although the algorithm used here can be found in the above
        references, the data structures used are original.}

Type
  PRef = ^TRef;
  TRef = TffInt64;
  PPageNum = ^TpageNum;
  TPageNum = TffWord32;

Const
  SizeOfRef = sizeof(TRef);
  SizeOfPageNum = sizeof(TPageNum);

Type
  PRefBlock = ^TRefBlock;
  TRefBlock = Array[0..($FFFFFFF Div SizeOfRef) - 1] Of TRef;

  PPageNumBlock = ^TPageNumBlock;
  TPageNumBlock = Array[0..($FFFFFFF Div SizeOfPageNum) - 1] Of TPageNum;

  {===Helper routines==================================================}

Function GetNewInxHeaderBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
{-Return a new index header block, pre-mark as dirty}
Var
  InxBlockHdr: PffBlockHeaderIndex Absolute Result;
  InxHeader: PffIndexHeader;
Begin
  Result := FFTblHlpGetNewBlock(aFI, aTI, aReleaseMethod, fsoNone);
  With InxBlockHdr^ Do
    Begin
      bhiSignature := fsc_SigIndexBlock;
      bhiNextBlock := fsc_W32NoValue;
      bhiLSN := 0;
      bhiBlockType := fsc_InxBlockTypeHeader;
      bhiIsLeafPage := False; {not used in header}
      bhiNodeLevel := 0; {not used in header}
      bhiKeysAreRefs := False; {not used in header}
      bhiIndexNum := $FFFF; {not used in header}
      bhiKeyLength := 0; {not used in header}
      bhiKeyCount := 0; {not used in header}
      bhiMaxKeyCount := 0; {not used in header}
      bhiPrevPageRef := fsc_W32NoValue; {not used in header}
    End;
  InxHeader := PffIndexHeader(@Result^[fsc_BlockHeaderSizeIndex]);
  FillChar(InxHeader^, sizeof(TffIndexHeader), 0);
End;
{--------}

Function GetNewInxBtreeBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aIndexHeader: PffIndexHeader;
  aIndex: Integer;
  aIsLeaf: boolean;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
{-Return a new index btree node/leaf block, pre-mark as dirty}
Var
  InxBlockHdr: PffBlockHeaderIndex Absolute Result;
Begin
  Result := FFTblHlpGetNewBlock(aFI, aTI, aReleaseMethod,fsoNone);
  With InxBlockHdr^, aIndexHeader^ Do
    Begin
      bhiSignature := fsc_SigIndexBlock;
      bhiNextBlock := fsc_W32NoValue;
      bhiBlockType := fsc_InxBlockTypeBtreePage;
      bhiIsLeafPage := aIsLeaf;
      If aIsLeaf Then
        bhiNodeLevel := 1 {leaves are at level 1}
      Else
        bhiNodeLevel := 0; {ie haven't a clue at present}
      bhiKeysAreRefs := (bihIndexFlags[aIndex] And fsc_InxFlagKeysAreRefs) <> 0;
      bhiIndexNum := aIndex;
      bhiKeyLength := bihIndexKeyLen[aIndex];
      bhiKeyCount := 0;
      If aIsLeaf Then
        If bhiKeysAreRefs Then
          bhiMaxKeyCount :=
            (aFI^.fiBlockSize - fsc_BlockHeaderSizeIndex)
            Div (SizeOfRef)
        Else
          bhiMaxKeyCount :=
            (aFI^.fiBlockSize - fsc_BlockHeaderSizeIndex)
            Div (bhiKeyLength + SizeOfRef)
      Else {it's a node}  If bhiKeysAreRefs Then
        bhiMaxKeyCount :=
          (aFI^.fiBlockSize - fsc_BlockHeaderSizeIndex)
          Div (SizeOfPageNum + SizeOfRef)
      Else
        bhiMaxKeyCount :=
          (aFI^.fiBlockSize - fsc_BlockHeaderSizeIndex)
          Div (bhiKeyLength + SizeOfPageNum + SizeOfRef);
      If Not Odd(bhiMaxKeyCount) Then
        dec(bhiMaxKeyCount);
      bhiPrevPageRef := fsc_W32NoValue;
      inc(bihIndexPageCount[aIndex]);
    End;
End;
{--------}

Function ReadVfyInxBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aFileHeader: PffBlockHeaderFile;
  Const aMarkDirty: boolean;
  Const aBlockType: Integer;
  Const aBlockNumber: TffWord32;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
Var
  InxBlockHdr: PffBlockHeaderIndex Absolute Result;
Begin
  With aFileHeader^ Do
    Begin
      {verify the block number}
      If (aBlockNumber <= 0) Or (aBlockNumber >= bhfUsedBlocks) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadBlockNr,
          [aFI^.fiName^, aBlockNumber]);
      {now get the record block; note: mark header block as fixed}
      Result := FFBMGetBlock(aFI, aTI, aBlockNumber, aMarkDirty, aReleaseMethod,fsoNone);
      {verify that it's an index block}
      If (InxBlockHdr^.bhiSignature <> fsc_SigIndexBlock) Or
        (InxBlockHdr^.bhiThisBlock <> aBlockNumber) Or
        (InxBlockHdr^.bhiBlockType <> aBlockType) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadInxBlock,
          [aFI^.fiName^, aBlockNumber]);
    End;
End;
{====================================================================}

{===Key rotation routines============================================}

Procedure RotateLeftLeaf(aParentPage: PffBlock;
  aSeparator: Longint;
  aChildLeft: PffBlock;
  aChildRight: PffBlock);
{-Rotate keys from right leaf child to left leaf child through key in
  parent given by separator index. Equalise number of keys}
Var
  ParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  ChildLeftHdr: PffBlockHeaderIndex Absolute aChildLeft;
  ChildRightHdr: PffBlockHeaderIndex Absolute aChildRight;
  KeysToMove: Longint;
  OffsetL: Longint;
  OffsetR: Longint;
  OffsetP: Longint;
  BytesToMove: Longint;
Begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.RotateLeftLeaf);
  {$ENDIF}
  {calculate the number of keys to move, this means that the right child
   will *lose* this number of keys and the left child will *gain* this
   number}
  KeysToMove := (ChildRightHdr^.bhiKeyCount - ChildLeftHdr^.bhiKeyCount) Div 2;
  If (KeysToMove = 0) Then
    inc(KeysToMove);
  {move the first pred(KeysToMove) keys from the right child to the last
   pred(KeysToMove) places of the left child, the last key of all comes
   from/goes to the parent}
  With ChildLeftHdr^ Do
    Begin
      {move the reference numbers}
      OffsetL := fsc_BlockHeaderSizeIndex +
        (bhiKeyCount * SizeOfRef);
      OffsetR := fsc_BlockHeaderSizeIndex;
      OffsetP := fsc_BlockHeaderSizeIndex +
        (ParentPageHdr^.bhiMaxKeyCount * SizeOfPageNum) +
        (aSeparator * SizeOfRef);
      {..move parent ref}
      PRef(@aChildLeft^[OffsetL])^ := PRef(@aParentPage^[OffsetP])^;
      {..move first set of refs}
      BytesToMove := pred(KeysToMove) * SizeOfRef;
      Move(aChildRight^[OffsetR],
        aChildLeft^[OffsetL + SizeOfRef],
        BytesToMove);
      {..set parent ref}
      PRef(@aParentPage^[OffsetP])^ :=
        PRef(@aChildRight^[OffsetR + BytesToMove])^;
      {..close up the gap}
      BytesToMove := (ChildRightHdr^.bhiKeyCount - KeysToMove) * SizeOfRef;
      Move(aChildRight^[OffsetR + (KeysToMove * SizeOfRef)],
        aChildRight^[OffsetR],
        BytesToMove);
      {if keys are separate entities, move the keys}
      If Not bhiKeysAreRefs Then
        Begin
          {move the keys}
          OffsetL := fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * SizeOfRef) +
            (bhiKeyCount * bhiKeyLength);
          OffsetR := fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * SizeOfRef);
          OffsetP := fsc_BlockHeaderSizeIndex +
            (ParentPageHdr^.bhiMaxKeyCount *
            (SizeOfPageNum + SizeOfRef)) +
            (aSeparator * bhiKeyLength);
          {..move parent key}
          Move(aParentPage^[OffsetP],
            aChildLeft^[OffsetL],
            bhiKeyLength);
          {..move first set of keys}
          BytesToMove := pred(KeysToMove) * bhiKeyLength;
          Move(aChildRight^[OffsetR],
            aChildLeft^[OffsetL + bhiKeyLength],
            BytesToMove);
          {..set parent key}
          Move(aChildRight^[OffsetR + BytesToMove],
            aParentPage^[OffsetP],
            bhiKeyLength);
          {..close up the gap}
          BytesToMove := (ChildRightHdr^.bhiKeyCount - KeysToMove) * bhiKeyLength;
          Move(aChildRight^[OffsetR + (KeysToMove * bhiKeyLength)],
            aChildRight^[OffsetR],
            BytesToMove);
        End;
    End;
  {Update the key counts}
  inc(ChildLeftHdr^.bhiKeyCount, KeysToMove);
  dec(ChildRightHdr^.bhiKeyCount, KeysToMove);
End;
{--------}

Procedure RotateLeftNode(aParentPage: PffBlock;
  aSeparator: Longint;
  aChildLeft: PffBlock;
  aChildRight: PffBlock);
{-Rotate keys from right node child to left node child through key in
  parent given by separator index. Equalise number of keys}
Var
  ParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  ChildLeftHdr: PffBlockHeaderIndex Absolute aChildLeft;
  ChildRightHdr: PffBlockHeaderIndex Absolute aChildRight;
  KeysToMove: Longint;
  OffsetL: Longint;
  OffsetR: Longint;
  OffsetP: Longint;
  BytesToMove: Longint;
Begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.RotateLeftNode);
  {$ENDIF}
  {calculate the number of keys to move, this means that the right child
   will *lose* this number of keys and the left child will *gain* this
   number}
  KeysToMove := (ChildRightHdr^.bhiKeyCount - ChildLeftHdr^.bhiKeyCount) Div 2;
  If (KeysToMove = 0) Then
    inc(KeysToMove);
  {move the first pred(KeysToMove) keys from the right child to the last
   pred(KeysToMove) places of the left child, the last key of all comes
   from/goes to the parent}
  With ChildLeftHdr^ Do
    Begin
      {move the page numbers}
      OffsetL := fsc_BlockHeaderSizeIndex +
        (bhiKeyCount * SizeOfPageNum);
      OffsetR := fsc_BlockHeaderSizeIndex;
      {..move set of page numbers}
      BytesToMove := KeysToMove * SizeOfPageNum;
      Move(aChildRight^[OffsetR - SizeOfPageNum],
        aChildLeft^[OffsetL],
        BytesToMove);
      {..close up the gap}
      BytesToMove := succ(ChildRightHdr^.bhiKeyCount - KeysToMove) * SizeOfPageNum;
      Move(aChildRight^[OffsetR + (pred(KeysToMove) * SizeOfPageNum)],
        aChildRight^[OffsetR - SizeOfPageNum],
        BytesToMove);
      {move the data reference numbers}
      OffsetL := fsc_BlockHeaderSizeIndex +
        ((bhiMaxKeyCount * SizeOfPageNum) + (bhiKeyCount * SizeOfRef));
      OffsetR := fsc_BlockHeaderSizeIndex +
        (bhiMaxKeyCount * SizeOfPageNum);
      OffsetP := fsc_BlockHeaderSizeIndex +
        (ParentPageHdr^.bhiMaxKeyCount * SizeOfPageNum) + (aSeparator * SizeOfRef);
      {..move parent ref}
      PRef(@aChildLeft^[OffsetL])^ := PRef(@aParentPage^[OffsetP])^;
      {..move first set of refs}
      BytesToMove := pred(KeysToMove) * SizeOfRef;
      Move(aChildRight^[OffsetR],
        aChildLeft^[OffsetL + SizeOfRef],
        BytesToMove);
      {..set parent ref}
      PRef(@aParentPage^[OffsetP])^ :=
        PRef(@aChildRight^[OffsetR + BytesToMove])^;
      {..close up the gap}
      BytesToMove := (ChildRightHdr^.bhiKeyCount - KeysToMove) * SizeOfRef;
      Move(aChildRight^[OffsetR + (KeysToMove * SizeOfRef)],
        aChildRight^[OffsetR],
        BytesToMove);
      {if keys are separate entities, move the keys}
      If Not bhiKeysAreRefs Then
        Begin
          {move the keys}
          OffsetL := fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef) +
            (bhiKeyCount * bhiKeyLength));
          OffsetR := fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef));
          OffsetP := fsc_BlockHeaderSizeIndex +
            (ParentPageHdr^.bhiMaxKeyCount * (SizeOfRef + SizeOfPageNum)) +
            (aSeparator * bhiKeyLength);
          {..move parent key}
          Move(aParentPage^[OffsetP],
            aChildLeft^[OffsetL],
            bhiKeyLength);
          {..move first set of keys}
          BytesToMove := pred(KeysToMove) * bhiKeyLength;
          Move(aChildRight^[OffsetR],
            aChildLeft^[OffsetL + bhiKeyLength],
            BytesToMove);
          {..set parent key}
          Move(aChildRight^[OffsetR + BytesToMove],
            aParentPage^[OffsetP],
            bhiKeyLength);
          {..close up the gap}
          BytesToMove := (ChildRightHdr^.bhiKeyCount - KeysToMove) * bhiKeyLength;
          Move(aChildRight^[OffsetR + (KeysToMove * bhiKeyLength)],
            aChildRight^[OffsetR],
            BytesToMove);
        End;
    End;
  {Update the key counts}
  inc(ChildLeftHdr^.bhiKeyCount, KeysToMove);
  dec(ChildRightHdr^.bhiKeyCount, KeysToMove);
End;
{--------}

Procedure RotateRightLeaf(aParentPage: PffBlock;
  aSeparator: Longint;
  aChildLeft: PffBlock;
  aChildRight: PffBlock);
{-Rotate keys from left leaf child to right leaf child through key in
  parent given by separator index. Equalise number of keys}
Var
  ParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  ChildLeftHdr: PffBlockHeaderIndex Absolute aChildLeft;
  ChildRightHdr: PffBlockHeaderIndex Absolute aChildRight;
  KeysToMove: Longint;
  OffsetL: Longint;
  OffsetR: Longint;
  OffsetP: Longint;
  BytesToMove: Longint;
Begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.RotateRightLeaf);
  {$ENDIF}
  {calculate the number of keys to move, this means that the left child
   will *lose* this number of keys and the right child will *gain* this
   number}
  KeysToMove := (ChildLeftHdr^.bhiKeyCount - ChildRightHdr^.bhiKeyCount) Div 2;
  If (KeysToMove = 0) Then
    inc(KeysToMove);
  {open up enough room in the right child for these keys, and move
   the last pred(KeysToMove) keys from the left child to the first
   pred(KeysToMove) places, the last key of all comes from/goes to
   the parent}
  With ChildRightHdr^ Do
    Begin
      {move the reference numbers}
      OffsetR := fsc_BlockHeaderSizeIndex;
      OffsetL := fsc_BlockHeaderSizeIndex +
        (ChildLeftHdr^.bhiKeyCount - KeysToMove) * SizeOfRef;
      OffsetP := fsc_BlockHeaderSizeIndex +
        (ParentPageHdr^.bhiMaxKeyCount * SizeOfPageNum) +
        (aSeparator * SizeOfRef);
      {..open up space}
      BytesToMove := bhiKeyCount * SizeOfRef;
      Move(aChildRight^[OffsetR],
        aChildRight^[OffsetR + (KeysToMove * SizeOfRef)],
        BytesToMove);
      {..move last set of refs}
      BytesToMove := pred(KeysToMove) * SizeOfRef;
      Move(aChildLeft^[OffsetL + SizeOfRef],
        aChildRight^[OffsetR],
        BytesToMove);
      {..move parent ref}
      PRef(@aChildRight^[OffsetR + BytesToMove])^ :=
        PRef(@aParentPage^[OffsetP])^;
      {..move to parent ref}
      PRef(@aParentPage^[OffsetP])^ := PRef(@aChildLeft^[OffsetL])^;
      {if keys are separate entities, move the keys}
      If Not bhiKeysAreRefs Then
        Begin
          {move the keys}
          OffsetR := fsc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfRef);
          OffsetL := fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * SizeOfRef) +
            (ChildLeftHdr^.bhiKeyCount - KeysToMove) * bhiKeyLength;
          OffsetP := fsc_BlockHeaderSizeIndex +
            (ParentPageHdr^.bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef)) +
            (aSeparator * bhiKeyLength);
          {..open up space}
          BytesToMove := bhiKeyCount * bhiKeyLength;
          Move(aChildRight^[OffsetR],
            aChildRight^[OffsetR + (KeysToMove * bhiKeyLength)],
            BytesToMove);
          {..move last set of keys}
          BytesToMove := pred(KeysToMove) * bhiKeyLength;
          {Start !!.01}
          //      if BytesToMove > 0 then begin
          Move(aChildLeft^[OffsetL + bhiKeyLength],
            aChildRight^[OffsetR],
            BytesToMove);
          {..move parent key}
          Move(aParentPage^[OffsetP],
            aChildRight^[OffsetR + BytesToMove],
            bhiKeyLength);
          //      end;
                                                                    {End !!.01}
                {..move to parent key}
          Move(aChildLeft^[OffsetL],
            aParentPage^[OffsetP],
            bhiKeyLength);
        End;
    End;
  {Update the key counts}
  dec(ChildLeftHdr^.bhiKeyCount, KeysToMove);
  inc(ChildRightHdr^.bhiKeyCount, KeysToMove);
End;
{--------}

Procedure RotateRightNode(aParentPage: PffBlock;
  aSeparator: Longint;
  aChildLeft: PffBlock;
  aChildRight: PffBlock);
{-Rotate keys from left node child to right node child through key in
  parent given by separator index. Equalise number of keys}
Var
  ParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  ChildLeftHdr: PffBlockHeaderIndex Absolute aChildLeft;
  ChildRightHdr: PffBlockHeaderIndex Absolute aChildRight;
  KeysToMove: Longint;
  OffsetL: Longint;
  OffsetR: Longint;
  OffsetP: Longint;
  BytesToMove: Longint;
Begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.RotateRightNode);
  {$ENDIF}
  {calculate the number of keys to move, this means that the left child
   will *lose* this number of keys and the right child will *gain* this
   number}
  KeysToMove := (ChildLeftHdr^.bhiKeyCount - ChildRightHdr^.bhiKeyCount) Div 2;
  If (KeysToMove = 0) Then
    inc(KeysToMove);
  {open up enough room in the right child for these keys, and move
   the last pred(KeysToMove) keys from the left child to the first
   pred(KeysToMove) places, the last key of all comes from/goes to
   the parent}
  With ChildRightHdr^ Do
    Begin
      {move the page numbers}
      OffsetR := fsc_BlockHeaderSizeIndex;
      OffsetL := fsc_BlockHeaderSizeIndex +
        (ChildLeftHdr^.bhiKeyCount - KeysToMove) * SizeOfPageNum;
      {..open up space}
      BytesToMove := succ(bhiKeyCount) * SizeOfPageNum;
      Move(aChildRight^[OffsetR - SizeOfPageNum],
        aChildRight^[OffsetR + (pred(KeysToMove) * SizeOfPageNum)],
        BytesToMove);
      {..move set of page numbers}
      BytesToMove := KeysToMove * SizeOfPageNum;
      Move(aChildLeft^[OffsetL],
        aChildRight^[OffsetR - SizeOfPageNum],
        BytesToMove);

      {move the data reference numbers}
      OffsetR := fsc_BlockHeaderSizeIndex +
        (bhiMaxKeyCount * SizeOfPageNum);
      OffsetL := fsc_BlockHeaderSizeIndex +
        (bhiMaxKeyCount * SizeOfPageNum) +
        (ChildLeftHdr^.bhiKeyCount - KeysToMove) * SizeOfRef;
      OffsetP := fsc_BlockHeaderSizeIndex +
        (ParentPageHdr^.bhiMaxKeyCount * SizeOfPageNum) +
        (aSeparator * SizeOfRef);
      {..open up space}
      BytesToMove := bhiKeyCount * SizeOfRef;
      Move(aChildRight^[OffsetR],
        aChildRight^[OffsetR + (KeysToMove * SizeOfRef)],
        BytesToMove);
      {..move last set of refs}
      BytesToMove := pred(KeysToMove) * SizeOfRef;
      Move(aChildLeft^[OffsetL + SizeOfRef],
        aChildRight^[OffsetR],
        BytesToMove);
      {..move parent ref}
      PRef(@aChildRight^[OffsetR + BytesToMove])^ :=
        PRef(@aParentPage^[OffsetP])^;
      {..move to parent ref}
      PRef(@aParentPage^[OffsetP])^ := PRef(@aChildLeft^[OffsetL])^;
      {if keys are separate entities, move the keys}
      If Not bhiKeysAreRefs Then
        Begin
          {move the keys}
          OffsetR := fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef));
          OffsetL := fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef)) +
            (ChildLeftHdr^.bhiKeyCount - KeysToMove) * bhiKeyLength;
          OffsetP := fsc_BlockHeaderSizeIndex +
            (ParentPageHdr^.bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef)) +
            (aSeparator * bhiKeyLength);
          {..open up space}
          BytesToMove := bhiKeyCount * bhiKeyLength;
          Move(aChildRight^[OffsetR],
            aChildRight^[OffsetR + (KeysToMove * bhiKeyLength)],
            BytesToMove);
          {..move last set of keys}
          BytesToMove := pred(KeysToMove) * bhiKeyLength;
          Move(aChildLeft^[OffsetL + bhiKeyLength],
            aChildRight^[OffsetR],
            BytesToMove);
          {..move parent key}
          Move(aParentPage^[OffsetP],
            aChildRight^[OffsetR + BytesToMove],
            bhiKeyLength);
          {..move to parent key}
          Move(aChildLeft^[OffsetL],
            aParentPage^[OffsetP],
            bhiKeyLength);
        End;
    End;
  {Update the key counts}
  dec(ChildLeftHdr^.bhiKeyCount, KeysToMove);
  inc(ChildRightHdr^.bhiKeyCount, KeysToMove);
End;
{====================================================================}

{===Key insertion into/deletion from/swapping pages==================}

Procedure InsertKeyInLeafPage(aLeaf: PffBlock;
  aElement: Longint;
  aKey: PffByteArray;
  Const aRefNr: TffInt64);
Var
  LeafHeader: PffBlockHeaderIndex Absolute aLeaf;
  RefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  Offset: Integer;
Begin
  {Assumptions: aLeaf has been marked dirty}
  With LeafHeader^ Do
    Begin
      {get the address of the reference block}
      RefBlock := PRefBlock(@aLeaf^[fsc_BlockHeaderSizeIndex]);
      {open up room to insert the new reference}
      Move(RefBlock^[aElement], RefBlock^[succ(aElement)],
        SizeOfRef * (bhiKeyCount - aElement));
      {insert the new reference}
      RefBlock^[aElement] := aRefNr;
      {if keys are separate entities, insert into key block}
      If Not LeafHeader^.bhiKeysAreRefs Then
        Begin
          {get the address of the keyblock}
          KeyBlock :=
            PffByteArray(@aLeaf^[fsc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfRef)]);
          {open up room to insert the new key}
          Offset := aElement * bhiKeyLength;
          Move(KeyBlock^[Offset], KeyBlock^[Offset + bhiKeyLength],
            bhiKeyLength * (bhiKeyCount - aElement));
          {insert the new key}
          Move(aKey^, KeyBlock^[Offset], bhiKeyLength);
        End;
      {increment the number of keys}
      inc(bhiKeyCount);
    End;
End;
{--------}

Procedure InsertKeyInNodePage(aNode: PffBlock;
  aElement: Longint;
  aKey: PffByteArray;
  Const aRefNr: TffInt64;
  aChild: TffWord32);
Var
  NodeHeader: PffBlockHeaderIndex Absolute aNode;
  PageBlock: PPageNumBlock;
  RefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  Offset: Integer;
Begin
  {Assumptions: aNode has been marked dirty}
  With NodeHeader^ Do
    Begin
      {get the address of the page number block}
      PageBlock := PPageNumBlock(@aNode^[fsc_BlockHeaderSizeIndex]);
      {open up room to insert the new reference}
      Move(PageBlock^[aElement], PageBlock^[succ(aElement)],
        SizeOfPageNum * (bhiKeyCount - aElement));
      {insert the new page number}
      PageBlock^[aElement] := aChild;
      {get the address of the data reference block}
      RefBlock :=
        PRefBlock(@aNode^[fsc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum)]);
      {open up room to insert the new reference}
      Move(RefBlock^[aElement], RefBlock^[succ(aElement)],
        SizeOfRef * (bhiKeyCount - aElement));
      {insert the new reference}
      RefBlock^[aElement] := aRefNr;
      {if keys are separate entities, insert into key block}
      If Not bhiKeysAreRefs Then
        Begin
          {get the address of the keyblock}
          KeyBlock :=
            PffByteArray(@aNode^[fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
          {open up room to insert the new key}
          Offset := aElement * bhiKeyLength;
          Move(KeyBlock^[Offset], KeyBlock^[Offset + bhiKeyLength],
            bhiKeyLength * (bhiKeyCount - aElement));
          {insert the new key}
          Move(aKey^, KeyBlock^[Offset], bhiKeyLength);
        End;
      {increment the number of keys}
      inc(bhiKeyCount);
    End;
End;
{--------}

Procedure RemoveKeyFromLeafPage(aLeaf: PffBlock;
  aElement: Longint);
Var
  LeafHeader: PffBlockHeaderIndex Absolute aLeaf;
  RefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  Offset: Integer;
Begin
  { Assumption: We have Exclusively locked aLeaf. }
  With LeafHeader^ Do
    Begin
      {decrement the key count}
      dec(bhiKeyCount);
      {get the address of the data reference block}
      RefBlock := PRefBlock(@aLeaf^[fsc_BlockHeaderSizeIndex]);
      {close up to delete the reference}
      Move(RefBlock^[succ(aElement)], RefBlock^[aElement],
        SizeOfRef * (bhiKeyCount - aElement));
      {if keys are separate entities, delete from key block}
      If Not LeafHeader^.bhiKeysAreRefs Then
        Begin
          {get the address of the key block}
          KeyBlock :=
            PffByteArray(@aLeaf^[fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * SizeOfRef)]);
          {close up to delete the key}
          Offset := aElement * bhiKeyLength;
          Move(KeyBlock^[Offset + bhiKeyLength], KeyBlock^[Offset],
            bhiKeyLength * (bhiKeyCount - aElement));
        End;
    End;
End;
{--------}

Procedure RemoveKeyFromNodePage(aNode: PffBlock;
  aElement: Longint);
Var
  NodeHeader: PffBlockHeaderIndex Absolute aNode;
  PageBlock: PPageNumBlock;
  RefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  Offset: Integer;
Begin
  {Assumptions: aNode has been marked dirty}
  With NodeHeader^ Do
    Begin
      {decrement the key count}
      dec(bhiKeyCount);
      {get the address of the page number block}
      PageBlock := PPageNumBlock(@aNode^[fsc_BlockHeaderSizeIndex]);
      {close up to delete the page number}
      Move(PageBlock^[succ(aElement)], PageBlock^[aElement],
        SizeOfPageNum * (bhiKeyCount - aElement));
      {get the address of the data reference block}
      RefBlock :=
        PRefBlock(@aNode^[fsc_BlockHeaderSizeIndex +
        (bhiMaxKeyCount * SizeOfPageNum)]);
      {close up to delete the reference}
      Move(RefBlock^[succ(aElement)], RefBlock^[aElement],
        SizeOfRef * (bhiKeyCount - aElement));
      {if keys are separate entities, delete from key block}
      If Not bhiKeysAreRefs Then
        Begin
          {get the address of the key block}
          KeyBlock :=
            PffByteArray(@aNode^[fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
          {close up to delete the key}
          Offset := aElement * bhiKeyLength;
          Move(KeyBlock^[Offset + bhiKeyLength], KeyBlock^[Offset],
            bhiKeyLength * (bhiKeyCount - aElement));
        End;
    End;
End;
{--------}

Procedure SwapKeys(aNode: PffBlock;
  aNElement: Longint;
  aLeaf: PffBlock;
  aLElement: Longint;
  aKey: PffByteArray);
{-Swap the key at aNElement in aNode with that at aLElement in aLeaf}
Var
  NodeHdr: PffBlockHeaderIndex Absolute aNode;
  LeafHdr: PffBlockHeaderIndex Absolute aLeaf;
  OffsetN: Longint;
  OffsetL: Longint;
  Temp: TffInt64;
Begin
  {Assumptions: aNode, aLeaf have been marked dirty; the key at
                aNElement in aNode compares equal to aKey}
  With NodeHdr^ Do
    Begin
      {swap references}
      OffsetN := fsc_BlockHeaderSizeIndex +
        (bhiMaxKeyCount * SizeOfPageNum) +
        (aNElement * SizeOfRef);
      OffsetL := fsc_BlockHeaderSizeIndex +
        (aLElement * SizeOfRef);
      Temp := PRef(@aNode^[OffsetN])^;
      PRef(@aNode^[OffsetN])^ := PRef(@aLeaf^[OffsetL])^;
      PRef(@aLeaf^[OffsetL])^ := Temp;
      {if keys are separate entities, swap keys}
      If Not bhiKeysAreRefs Then
        Begin
          OffsetN := fsc_BlockHeaderSizeIndex +
            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef)) +
            (aNElement * NodeHdr^.bhiKeyLength);
          OffsetL := fsc_BlockHeaderSizeIndex +
            (LeafHdr^.bhiMaxKeyCount * SizeOfRef) +
            (aLElement * LeafHdr^.bhiKeyLength);
          Move(aLeaf^[OffsetL], aNode^[OffsetN], bhiKeyLength);
          Move(aKey^, aLeaf^[OffsetL], bhiKeyLength);
        End;
    End;
End;
{====================================================================}

{===Page splitting/merging routines==================================}

Procedure MergeChildren(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aParentPage: PffBlock;
  aSeparator: Longint;
  aChildLeft: PffBlock;
  aChildRight: PffBlock);
{-Merge the right child into the left child, separated by the given
  key from the parent. The right child is deleted; the parent loses
  one key.}
Var
  ParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  ChildLeftHdr: PffBlockHeaderIndex Absolute aChildLeft;
  ChildRightHdr: PffBlockHeaderIndex Absolute aChildRight;
  OffsetL: Longint;
  OffsetR: Longint;
  OffsetP: Longint;
Begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.Merge);
  {$ENDIF}
  With ChildLeftHdr^ Do
    Begin
      {Note: this routine is *only* called if both children have
       (bhiMaxKeyCount div 2) keys--the absolute minimum}
      If (bhiKeyCount <> (bhiMaxKeyCount Div 2)) Or
        (bhiKeyCount <> ChildRightHdr^.bhiKeyCount) Then
        FSRaiseException(EfsServerException, fsStrResServer, fserrBadMergeCall,
          [aIndexData.kidFI^.fiName^, bhiThisBlock, ChildRightHdr^.bhiThisBlock]);
      {the merge process is different for nodes and leaves}
      If (Not bhiIsLeafPage) Then
        Begin
          {copy over the page numbers}
          OffsetR := fsc_BlockHeaderSizeIndex;
          OffsetL := fsc_BlockHeaderSizeIndex + (bhiKeyCount * SizeOfPageNum);
          Move(aChildRight^[OffsetR - SizeOfPageNum],
            aChildLeft^[OffsetL],
            (succ(bhiKeyCount) * SizeOfPageNum));
          {set up offsets for data references}
          OffsetL := fsc_BlockHeaderSizeIndex +
            ((bhiMaxKeyCount * SizeOfPageNum) + (bhiKeyCount * SizeOfRef));
          OffsetR := fsc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum);
        End
      Else {it's a leaf}
        Begin
          {set up offsets for data references}
          OffsetL := fsc_BlockHeaderSizeIndex +
            (bhiKeyCount * SizeOfRef);
          OffsetR := fsc_BlockHeaderSizeIndex;
        End;
      {copy over parent data reference}
      OffsetP := fsc_BlockHeaderSizeIndex +
        (ParentPageHdr^.bhiMaxKeyCount * SizeOfPageNum) +
        (aSeparator * SizeOfRef);
      PRef(@aChildLeft^[OffsetL])^ := PRef(@aParentPage^[OffsetP])^;
      {copy over other data references}
      Move(aChildRight^[OffsetR],
        aChildLeft^[OffsetL + SizeOfRef],
        (bhiKeyCount * SizeOfRef));
      {if keys are separate entities, move the keys}
      If Not bhiKeysAreRefs Then
        Begin
          {set up offsets for keys}
          inc(OffsetL, ((bhiMaxKeyCount - bhiKeyCount) * SizeOfRef) +
            (bhiKeyCount * bhiKeyLength));
          inc(OffsetR, (bhiMaxKeyCount * SizeOfRef));
          OffsetP := fsc_BlockHeaderSizeIndex +
            (ParentPageHdr^.bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef)) +
            (aSeparator * bhiKeyLength);
          {copy over the parent key}
          Move(aParentPage^[OffsetP], aChildLeft^[OffsetL], bhiKeyLength);
          {copy over all the other keys}
          Move(aChildRight^[OffsetR],
            aChildLeft^[OffsetL + bhiKeyLength],
            (bhiKeyCount * bhiKeyLength));
        End;
      {delete the parent key since it now points to an invalid page}
      RemoveKeyFromNodePage(aParentPage, aSeparator);

      {patch up the left child's key count}
      bhiKeyCount := bhiMaxKeyCount;
    End;
  {delete the right child, it is no longer referenced}
  With aIndexData Do
    Begin
      FFTblHlpDeleteBlock(kidFI, kidFileHeader, aChildRight);
      dec(kidIndexHeader^.bihIndexPageCount[kidIndex]);
    End;
End;
{--------}

Procedure BtreeSplitChild(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aParentPage: PffBlock;
  aChildIndex: Integer;
  aChildPage: PffBlock);
{-Split the given child into two children. If the number of keys in
  the child is 2N+1, each child will end up with N keys, the parent
  gains one key at aChildIndex.}
Var
  aParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  aChildPageHdr: PffBlockHeaderIndex Absolute aChildPage;
  NewChildPage: PffBlock;
  NewChildPageHdr: PffBlockHeaderIndex Absolute NewChildPage;
  NewChild: Longint;
  NewOffset: Integer;
  OldOffset: Integer;
  MedianRef: TRef;
  aRelMethod: TffReleaseMethod;
Begin
  { Assumptions: aParentPage and aChildPage have been marked dirty. }
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.Splits);
  {$ENDIF}
  aRelMethod := Nil;
  With aChildPageHdr^ Do
    Begin
      {create a new child page}
      With aIndexData Do
        NewChildPage :=
          GetNewInxBtreeBlock(kidFI, aTI, kidIndexHeader, kidIndex,
          bhiIsLeafPage, aRelMethod);
      Try
        NewChild := NewChildPageHdr^.bhiThisBlock;
        NewChildPageHdr^.bhiNodeLevel := bhiNodeLevel;
        {transfer the second half of the old child to the first half of the new one}
        {note this depends on whether the page is an internal node or a leaf}
        If (Not bhiIsLeafPage) Then
          Begin
            {move the page numbers}
            {note: we must transfer into the prev page number field of the header}
            NewOffset := fsc_BlockHeaderSizeIndex - SizeOfPageNum;
            OldOffset := NewOffset + (succ(bhiMaxKeyCount Div 2) * SizeOfPageNum);
            Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
              succ(bhiMaxKeyCount Div 2) * SizeOfPageNum);
            {move the data references}
            NewOffset := fsc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum);
            OldOffset := NewOffset + (succ(bhiMaxKeyCount Div 2) * SizeOfRef);
            Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
              (bhiMaxKeyCount Div 2) * SizeOfRef);
            MedianRef := PRef(@aChildPage^[OldOffset - SizeOfRef])^;
            {if keys are separate entities, move the keys}
            If Not bhiKeysAreRefs Then
              Begin
                NewOffset := fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef));
                OldOffset := NewOffset + (succ(bhiMaxKeyCount Div 2) * bhiKeyLength);
                Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
                  (bhiMaxKeyCount Div 2) * bhiKeyLength);
              End;
          End
        Else {it's a leaf}
          Begin
            {move the data references}
            NewOffset := fsc_BlockHeaderSizeIndex;
            OldOffset := NewOffset + (succ(bhiMaxKeyCount Div 2) * SizeOfRef);
            Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
              (bhiMaxKeyCount Div 2) * SizeOfRef);
            MedianRef := PRef(@aChildPage^[OldOffset - SizeOfRef])^;
            {if keys are separate entities, move the keys}
            If Not bhiKeysAreRefs Then
              Begin
                NewOffset := fsc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfRef);
                OldOffset := NewOffset + (succ(bhiMaxKeyCount Div 2) * bhiKeyLength);
                Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
                  (bhiMaxKeyCount Div 2) * bhiKeyLength);
              End;
          End;
        {insert the median key into the parent}
        InsertKeyInNodePage(aParentPage, aChildIndex,
          PffByteArray(@aChildPage^[OldOffset - bhiKeyLength]),
          MedianRef, NewChild);
        {set the number of keys in each child}
        bhiKeyCount := bhiMaxKeyCount Div 2;
        NewChildPageHdr^.bhiKeyCount := bhiMaxKeyCount Div 2;
      Finally
        aRelMethod(NewChildPage);
      End;
    End;
End;
{====================================================================}

{===Key insertion helper routines====================================}

Function BtreeInsertRedistribute(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aParentPage: PffBlock;
  aChildIndex: Integer;
  aChildPage: PffBlock): boolean;
Var
  aParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  SiblingPage: PffBlock;
  SiblingPageHdr: PffBlockHeaderIndex Absolute SiblingPage;
  Sibling: Longint;
  PageBlock: PPageNumBlock;
  aRelMethod: TffReleaseMethod;
Begin
  { Assumption: aParentPage and aChildPage have been marked dirty. }
  Result := False;

  { Try the child's successor sibling page. }
  If (aChildIndex < aParentPageHdr^.bhiKeyCount) Then
    Begin
      PageBlock := PPageNumBlock(@aParentPage^[fsc_BlockHeaderSizeIndex]);
      Sibling := PageBlock^[aChildIndex];
      With aIndexData Do
        SiblingPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_MarkDirty,
          fsc_InxBlockTypeBtreePage, Sibling,
          aRelMethod);

      Try
        { Are there at least two spare key slots? }
        If (SiblingPageHdr^.bhiKeyCount < pred(SiblingPageHdr^.bhiMaxKeyCount)) Then
          Begin
            { Yes. Redistribute the keys. }
            If (Not SiblingPageHdr^.bhiIsLeafPage) Then
              RotateRightNode(aParentPage, aChildIndex, aChildPage, SiblingPage)
            Else
              RotateRightLeaf(aParentPage, aChildIndex, aChildPage, SiblingPage);
            Result := True;
          End;
      Finally
        aRelMethod(SiblingPage);
      End;
    End;

  { If not done it yet, try the child's predecessor sibling page. }
  If (Not Result) And (aChildIndex > 0) Then
    Begin
      If (aChildIndex = 1) Then
        Sibling := aParentPageHdr^.bhiPrevPageRef
      Else
        Begin
          PageBlock := PPageNumBlock(@aParentPage^[fsc_BlockHeaderSizeIndex]);
          Sibling := PageBlock^[aChildIndex - 2];
        End;
      With aIndexData Do
        SiblingPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_MarkDirty,
          fsc_InxBlockTypeBtreePage, Sibling,
          aRelMethod);
      Try
        { Are there at least two spare key slots? }
        If (SiblingPageHdr^.bhiKeyCount < pred(SiblingPageHdr^.bhiMaxKeyCount)) Then
          Begin
            { Yes. Redistribute the keys. }
            If (Not SiblingPageHdr^.bhiIsLeafPage) Then
              RotateLeftNode(aParentPage, aChildIndex - 1, SiblingPage, aChildPage)
            Else
              RotateLeftLeaf(aParentPage, aChildIndex - 1, SiblingPage, aChildPage);
            Result := True;
          End;
      Finally
        aRelMethod(SiblingPage);
      End;
    End;
End;
{--------}

Function BtreeInsertNonFull(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  Var aPage: PffBlock;
  Var aRelMethod: TffReleaseMethod;
  aKey: PffByteArray;
  Const aRefNr: TffInt64): boolean;
Var
  PageHdr: PffBlockHeaderIndex Absolute aPage;
  PageNumBlock: PPageNumBlock;
  DataRefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  L, R, M: Integer;
  CompResult: Integer;
  Child: Longint;
  ChildPage: PffBlock;
  ChildPageHdr: PffBlockHeaderIndex Absolute ChildPage;
  Compare: TffKeyCompareFunc;
  AllowDups: boolean;
  DoneRecursing: boolean;
  aChildRelMethod: TffReleaseMethod;
Begin
  { Assumptions: aPage could be dirty or clean.  Caller has incremented
      aPage's ref count. }
  Result := False;
  { Learn whether dup keys are allowed, get compare function. }
  With aIndexData Do
    Begin
      AllowDups := (kidIndexHeader^.bihIndexFlags[kidIndex] And
        fsc_InxFlagAllowDups) <> 0;
      Compare := kidCompare;
    End;
  {simulate recursion (ie unwind it}
  DoneRecursing := False;
  Repeat
    With PageHdr^ Do
      Begin
        {get the addresses of the reference block and key string block,
         this is different for leaf and node pages}
        If bhiIsLeafPage Then
          Begin
            PageNumBlock := Nil;
            DataRefBlock := PRefBlock(@aPage^[fsc_BlockHeaderSizeIndex]);
            If bhiKeysAreRefs Then
              KeyBlock := PffByteArray(DataRefBlock)
            Else
              KeyBlock :=
                PffByteArray(@aPage^[fsc_BlockHeaderSizeIndex +
                (bhiMaxKeyCount * SizeOfRef)]);
          End
        Else {its a node page}
          Begin
            PageNumBlock := PPageNumBlock(@aPage^[fsc_BlockHeaderSizeIndex]);
            DataRefBlock :=
              PRefBlock(@aPage^[fsc_BlockHeaderSizeIndex +
              (bhiMaxKeyCount * SizeOfPageNum)]);
            If bhiKeysAreRefs Then
              KeyBlock := PffByteArray(DataRefBlock)
            Else
              KeyBlock :=
                PffByteArray(@aPage^[fsc_BlockHeaderSizeIndex +
                (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
          End;
        {binary search to find insertion point}
        L := 0;
        R := pred(bhiKeyCount);
        Repeat
          M := (L + R) Div 2;
          CompResult := Compare(aKey^, KeyBlock^[M * bhiKeyLength],
            aIndexData.kidCompareData);
          If (CompResult < 0) Then
            R := pred(M)
          Else If (CompResult > 0) Then
            L := succ(M)
          Else {CompResult = 0}  If AllowDups Then
            Begin
              CompResult := FFCmpI64(aRefNr, DataRefBlock^[M]);
              If (CompResult < 0) Then
                R := pred(M)
              Else If (CompResult > 0) Then
                L := succ(M)
              Else {it's a duplicate key+refnr combo}
                Exit;
            End
          Else {it's a duplicate key}
            Exit;
        Until (L > R);
        If bhiIsLeafPage Then
          Begin
            { It's a leaf page.  Mark it as dirty since we are about to modify it. }
            FFBMDirtyBlock(aIndexData.kidFI, bhiThisBlock, aTI, aPage);

            { The key+refnr combo doesn't exist, insert at L. }
            InsertKeyInLeafPage(aPage, L, aKey, aRefNr);
            Result := True;
            DoneRecursing := True;
          End
        Else {it's a node page}
          Begin
            { The child we need to traverse to is given by (L - 1). }
            If (L = 0) Then
              Child := bhiPrevPageRef
            Else
              Child := PageNumBlock^[pred(L)];
            { Read the page.  For now, we need a Share lock. }
            With aIndexData Do
              ChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                fsc_InxBlockTypeBtreePage, Child,
                aChildRelMethod);
            { If this child is full, split it or redistribute now. }
            With ChildPageHdr^ Do
              If (bhiKeyCount = bhiMaxKeyCount) Then
                Begin
                  { Splitting a child/redistribution will update the parent
                    page as well, so mark both pages as dirty. }
                  FFBMDirtyBlock(aIndexData.kidFI, PageHdr^.bhiThisBlock, aTI, aPage);
                  FFBMDirtyBlock(aIndexData.kidFI, Child, aTI, ChildPage);

                  { Try redistribution else split the child. }
                  If Not BtreeInsertRedistribute(aIndexData, aTI, aPage, L, ChildPage) Then
                    BtreeSplitChild(aIndexData, aTI, aPage, L, ChildPage);
                  aChildRelMethod(ChildPage);
                  { We've just rearranged the keys in this page, recurse this page. }
                End
              Else
                Begin
                  { Insert the key into the child's subtree, ie recurse with child. }
                  aRelMethod(aPage);
                  aRelMethod := aChildRelMethod;
                  aPage := ChildPage;
                End;
          End;
      End;
  Until DoneRecursing;
End;
{--------}

Function BtreeInsert(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aRoot: TffWord32;
  aKey: PffByteArray;
  Const aRefNr: TffInt64): boolean;
Var
  RootPage: PffBlock;
  RootPageHdr: PffBlockHeaderIndex Absolute RootPage;
  NewRootPage: PffBlock;
  NewRootPageHdr: PffBlockHeaderIndex Absolute NewRootPage;
  aNewRelMethod,
    aRelMethod: TffReleaseMethod;
Begin
  { Get the root page. }
  With aIndexData Do
    RootPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_MarkDirty,
      fsc_InxBlockTypeBtreePage, aRoot,
      aRelMethod);
  { If the root is full, we split it now. }
  With RootPageHdr^ Do
    Try
      If (bhiKeyCount = bhiMaxKeyCount) Then
        Begin
          { Since we're about to update it, mark the block as dirty. }
          FFBMDirtyBlock(aIndexData.kidFI, aRoot, aTI, RootPage);

          { Create a new root. }
          With aIndexData Do
            NewRootPage :=
              GetNewInxBtreeBlock(kidFI, aTI, kidIndexHeader, kidIndex, False,
              aNewRelMethod);
          Try
            NewRootPageHdr^.bhiNodeLevel := succ(bhiNodeLevel);

            { Patch it so that the previous page is the old root. }
            NewRootPageHdr^.bhiPrevPageRef := aRoot;

            { Split the old root. }
            BtreeSplitChild(aIndexData, aTI, NewRootPage, 0, RootPage);

            { Update the index header to point to the new root. }
            With aIndexData Do
              kidIndexHeader^.bihIndexRoot[kidIndex] := NewRootPageHdr^.bhiThisBlock;
            {now insert the key into the tree starting at the new root}
            Result :=
              BtreeInsertNonFull(aIndexData, aTI, NewRootPage, aNewRelMethod,
              aKey, aRefNr);
          Finally
            aNewRelMethod(NewRootPage);
          End;

        End
      Else {insert the key into the tree starting at the root}
        Result :=
          BtreeInsertNonFull(aIndexData, aTI, RootPage, aRelMethod, aKey,
          aRefNr);
    Finally
      aRelMethod(RootPage);
    End;
End;
{====================================================================}

{===Key deletion helper routines=====================================}

Procedure BtreeDeleteIndexPage(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aParent: TffWord32);
Var
  ParentPage: PffBlock;
  ParentPageHdr: PffBlockHeaderIndex Absolute ParentPage;
  PageBlock: PPageNumBlock;
  Child: Integer;
  aRelMethod: TffReleaseMethod;
Begin
  {WARNING: this is a recursive routine with an absolute maximum of 32
            levels of recursion}
  {read the parent index page, mark dirty}
  With aIndexData Do
    ParentPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_MarkDirty,
      fsc_InxBlockTypeBtreePage, aParent,
      aRelMethod);
  Try
    {get the page number block}
    PageBlock := PPageNumBlock(@ParentPage^[fsc_BlockHeaderSizeIndex]);
    With ParentPageHdr^ Do
      {if there are children recurse through them all}
      If (bhiNodeLevel > 1) Then
        Begin
          BtreeDeleteIndexPage(aIndexData, aTI, bhiPrevPageRef);
          For Child := 0 To pred(bhiKeyCount) Do
            BtreeDeleteIndexPage(aIndexData, aTI, PageBlock^[Child]);
        End;
    {delete this page}
    With aIndexData Do
      Begin
        FFTblHlpDeleteBlock(kidFI, kidFileHeader, ParentPage);
      End;
  Finally
    aRelMethod(ParentPage);
  End;
End;
{--------}

Function BtreeDeleteSwapKey(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aParentPage: PffBlock;
  aKeyIndex: Integer;
  aKey: PffByteArray;
  Var aRelMethod: TffReleaseMethod): PffBlock;
Var
  ParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  ChildPage: PffBlock;
  ChildPageHdr: PffBlockHeaderIndex Absolute ChildPage;
  Child: TffWord32;
  LChildPage: PffBlock;
  LChildPageHdr: PffBlockHeaderIndex Absolute LChildPage;
  LChild: TffWord32;
  RChildPage: PffBlock;
  RChildPageHdr: PffBlockHeaderIndex Absolute RChildPage;
  RChild: TffWord32;
  ResultPageNum: TffWord32;
  MergeThem: boolean;
  aChildRelMeth,
    aLCRelMethod,
    aRCRelMethod: TffReleaseMethod;
Begin
  {Assumptions: aParentPage has already been marked dirty.
                The key at aKeyIndex in the parent equals aKey}
  { Assume that we shall have to do a merge. }
  LChild := 0;
  Result := Nil;
  MergeThem := True;
  With ParentPageHdr^ Do
    Begin
      {we shall first search for the successor key}
      RChild :=
        PPageNum(@aParentPage^[fsc_BlockHeaderSizeIndex + (aKeyIndex * SizeOfPageNum)])^;
      With aIndexData Do
        RChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
          fsc_InxBlockTypeBtreePage, RChild,
          aRCRelMethod);
      { Does this child page have enough keys?. }
      If (RChildPageHdr^.bhiKeyCount > (RChildPageHdr^.bhiMaxKeyCount Div 2)) Then
        Begin
          {$IFDEF FF_DEBUG}
          inc(FFDEBUG_IndexCounter.SwapNext);
          {$ENDIF}
          {save the right child page as Result}
          aRelMethod := aRCRelMethod;
          Result := RChildPage;
          ResultPageNum := RChild;
          {prepare to walk down the btree}
          Child := RChild;
          ChildPage := RChildPage;
          aChildRelMeth := aRCRelMethod;
          {continue walking down the btree going left all the time until
           we hit a leaf, locking pages as we go}
          While (Not ChildPageHdr^.bhiIsLeafPage) Do
            Begin
              Child := ChildPageHdr^.bhiPrevPageRef;
              If ChildPage <> Result Then {!!.01}
                aChildRelMeth(ChildPage); {!!.01}
              With aIndexData Do
                ChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                  fsc_InxBlockTypeBtreePage, Child,
                  aChildRelMeth);
            End;
          { Mark the final leaf child since we shall be updating it.
            Note: If we walked down only 1 level then we need to make sure Result
            contains a pointer to the modified block put into ChildPage. }
          FFBMDirtyBlock(aIndexData.kidFI, Child, aTI, ChildPage);
          //      if ResultPageNum = Child then begin                            {Deleted !!.01}
          //      aRelMethod := aChildRelMeth;                                   {Deleted !!.01}
          //      Result := ChildPage;                                           {Deleted !!.01}
          //      end;                                                           {Deleted !!.01}

                { Swap the child's smallest key & ref with this key in the parent. }
          SwapKeys(aParentPage, aKeyIndex, ChildPage, 0, aKey);
          {Begin !!.01}
                { Did we modify the right child? }
          If Child = ResultPageNum Then
            { Yes. Make sure we return the child page's modified block to the
              calling method. }
            Result := ChildPage
          Else
            { No. Release the child page that we modified. }
            aChildRelMeth(ChildPage);
          {End !!.01}
          MergeThem := False;
        End;

      { If we couldn't use the successor, try the predecessor. }
      If MergeThem Then
        Begin
          {find the left child}
          If (aKeyIndex = 0) Then
            LChild := bhiPrevPageRef
          Else
            LChild := PPageNum(@aParentPage^[fsc_BlockHeaderSizeIndex +
              ((aKeyIndex - 1) * SizeOfPageNum)])^;
          With aIndexData Do
            LChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
              fsc_InxBlockTypeBtreePage, LChild,
              aLCRelMethod);
          { Does this child page have enough keys? }
          If (LChildPageHdr^.bhiKeyCount > (LChildPageHdr^.bhiMaxKeyCount Div 2)) Then
            Begin
              {$IFDEF FF_DEBUG}
              inc(FFDEBUG_IndexCounter.SwapPrev);
              {$ENDIF}
              {save the left child page as result}
              aRelMethod := aLCRelMethod;
              Result := LChildPage;
              ResultPageNum := LChild;
              {prepare to walk down the btree}
              Child := LChild;
              ChildPage := LChildPage;
              aChildRelMeth := aLCRelMethod;
              {continue walking down the btree going right all the time until
               we hit a leaf, locking as we go}
              While (Not ChildPageHdr^.bhiIsLeafPage) Do
                Begin
                  Child :=
                    PPageNum(@ChildPage^[fsc_BlockHeaderSizeIndex +
                    ((ChildPageHdr^.bhiKeyCount - 1) * SizeOfPageNum)])^;
                  If ChildPage <> LChildPage Then {!!.01}
                    aChildRelMeth(ChildPage); {!!.01}
                  With aIndexData Do
                    ChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                      fsc_InxBlockTypeBtreePage, Child,
                      aChildRelMeth);
                End;
              { Mark the leaf child dirty because we shall be updating it. }
              FFBMDirtyBlock(aIndexData.kidFI, Child, aTI, ChildPage);
              //        if ResultPageNum = Child then begin                          {Deleted !!.01}
              //        aRelMethod := aChildRelMeth;                                 {Deleted !!.01}
              //        Result := ChildPage;                                         {Deleted !!.01}
              //        end;                                                         {Deleted !!.01}

                      { Swap the child's largest key & ref with this key in the parent. }
              SwapKeys(aParentPage, aKeyIndex,
                ChildPage, pred(ChildPageHdr^.bhiKeyCount),
                aKey);
              {Begin !!.01}
                    { Did we modify the left child? }
              If Child = ResultPageNum Then
                { Yes. Make sure we return the child page's modified block to the
                  calling method. }
                Result := ChildPage
              Else
                { No. Release the child page that we modified. }
                aChildRelMeth(ChildPage);
              {End !!.01}
              MergeThem := False;
            End;
        End;

      {if we've failed to find the predecessor/successor, merge the two children}
      If MergeThem Then
        Begin
          { Obtain an Exclusive lock on the children. }
          {***Delphi32***: the compiler tags LChild as possibly being
                           "used before definition". Not true.}
          FFBMDirtyBlock(aIndexData.kidFI, LChild, aTI, LChildPage);
          FFBMDirtyBlock(aIndexData.kidFI, RChild, aTI, RChildPage);

          { Merge them. }
          MergeChildren(aIndexData, aTI, aParentPage, aKeyIndex, LChildPage,
            RChildPage);
          aRelMethod := aLCRelMethod;
          Result := LChildPage;
          aRCRelMethod(RChildPage);
        End;
    End;
  {***Delphi32***: the compiler tags the return value of this function
                   as being "possibly undefined". Not true.}
End;
{--------}

Procedure BtreeDeleteRedistributeOrMerge(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aParentPage: PffBlock;
  aChildIndex: Integer;
  aChildPage: PffBlock);
Var
  aParentPageHdr: PffBlockHeaderIndex Absolute aParentPage;
  SiblingPage: PffBlock;
  SiblingPageHdr: PffBlockHeaderIndex Absolute SiblingPage;
  Sibling: Longint;
  PageBlock: PPageNumBlock;
  DoneIt: boolean;
  IsRightSibling: boolean;
  aRelList: TfsPointerList;
  aRelMethod: TffReleaseMethod;
Begin
  {Assumptions: aParentPage and aChildPage have both been marked dirty.
                aChildPage has the minimum number of keys. }
  aRelList := TfsPointerList.Create;
  Sibling := 0;
  IsRightSibling := False;
  SiblingPage := Nil;
  Try
    {assume we shall fail all the way}
    DoneIt := False;
    {read the child's successor sibling page}
    If (aChildIndex < aParentPageHdr^.bhiKeyCount) Then
      Begin
        PageBlock := PPageNumBlock(@aParentPage^[fsc_BlockHeaderSizeIndex]);
        Sibling := PageBlock^[aChildIndex];
        IsRightSibling := True;
        With aIndexData Do
          SiblingPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
            fsc_InxBlockTypeBtreePage, Sibling,
            aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(SiblingPage, TffInt64(aRelMethod)));
        {check for at least one spare key}
        If (SiblingPageHdr^.bhiKeyCount > (SiblingPageHdr^.bhiMaxKeyCount Div 2)) Then
          Begin
            { Mark the sibling as dirty. }
            FFBMDirtyBlock(aIndexData.kidFI, Sibling, aTI, SiblingPage);

            { Redistribute the keys. }
            If (Not SiblingPageHdr^.bhiIsLeafPage) Then
              RotateLeftNode(aParentPage, aChildIndex, aChildPage, SiblingPage)
            Else
              RotateLeftLeaf(aParentPage, aChildIndex, aChildPage, SiblingPage);
            DoneIt := True;
          End;
      End;

    { Read the child's predecessor sibling page. }
    If (Not DoneIt) And (aChildIndex > 0) Then
      Begin
        If (aChildIndex = 1) Then
          Sibling := aParentPageHdr^.bhiPrevPageRef
        Else
          Begin
            PageBlock := PPageNumBlock(@aParentPage^[fsc_BlockHeaderSizeIndex]);
            Sibling := PageBlock^[aChildIndex - 2];
          End;
        IsRightSibling := False;
        With aIndexData Do
          SiblingPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
            fsc_InxBlockTypeBtreePage, Sibling,
            aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(SiblingPage, TffInt64(aRelMethod)));
        { Check for at least one spare key. }
        If (SiblingPageHdr^.bhiKeyCount > (SiblingPageHdr^.bhiMaxKeyCount Div 2)) Then
          Begin
            { Obtain an Exclusive lock on the sibling. }
            FFBMDirtyBlock(aIndexData.kidFI, Sibling, aTI, SiblingPage);

            { Redistribute the keys. }
            If (Not SiblingPageHdr^.bhiIsLeafPage) Then
              RotateRightNode(aParentPage, aChildIndex - 1, SiblingPage, aChildPage)
            Else
              RotateRightLeaf(aParentPage, aChildIndex - 1, SiblingPage, aChildPage);
            DoneIt := True;
          End;
      End;

    {***Delphi32***: The compiler tags both Sibling and IsRightSibling as
                     possibly being "used before definition". A corollary
                     of the definition of a B-Tree insists that every child
                     page has at least one sibling and so in practice both
                     variables will be set by this point.}
    If (Not DoneIt) Then
      Begin
        { Mark the sibling as dirty. }
        FFBMDirtyBlock(aIndexData.kidFI, Sibling, aTI, SiblingPage);
        { Merge with our sibling. }
        If IsRightSibling Then
          MergeChildren(aIndexData, aTI, aParentPage, aChildIndex, aChildPage,
            SiblingPage)
        Else
          MergeChildren(aIndexData, aTI, aParentPage, aChildIndex - 1,
            SiblingPage, aChildPage)
      End;
  Finally
    For Sibling := 0 To pred(aRelList.Count) Do
      FFDeallocReleaseInfo(aRelList[Sibling]);
    aRelList.Free;
  End;
End;
{--------}

Function BtreeDeleteAmplePage(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aPage: PffBlock;
  aKey: PffByteArray;
  Const aRefNr: TffInt64;
  Var aBTreeChanged: Boolean) {!!.05}
: Boolean;
{-Routine to delete a key from a page; only called for
  pages that have succ(minimum keys) present, or for the root}
Var
  Page: PffBlock;
  PageHdr: PffBlockHeaderIndex Absolute Page;
  PageNumBlock: PPageNumBlock;
  DataRefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  L, R, M: Integer;
  CompResult: Integer;
  Child: Longint;
  ChildPage: PffBlock;
  ChildPageHdr: PffBlockHeaderIndex Absolute ChildPage;
  Compare: TffKeyCompareFunc;
  AllowDups: boolean;
  KeyFound: boolean;
  DoneRecursing: boolean;
  aRelMethod: TffReleaseMethod;
  aRelList: TfsPointerList;
Begin
  {$IFDEF DefeatWarnings}
  M := 0;
  Result := False;
  {$ENDIF}

  Page := aPage;
  aRelMethod := Nil;

  { We use the following list to track the RAM pages we've accessed and
    the release method associated with each RAM page. At the end of this
    routine, we will call the release method for each RAM page. }
  aRelList := TfsPointerList.Create;

  Try
    { Assumption: Page has not been marked dirty. }
    { Learn whether dup keys are allowed, get compare function. }
    With aIndexData Do
      Begin
        AllowDups := (kidIndexHeader^.bihIndexFlags[kidIndex] And
          fsc_InxFlagAllowDups) <> 0;
        Compare := kidCompare;
      End;
    {simulate recursion (ie unwind it)}
    DoneRecursing := False;
    Repeat
      With PageHdr^ Do
        Begin
          {get the addresses of the reference block and key string block}
          If bhiIsLeafPage Then
            Begin
              PageNumBlock := Nil;
              DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
              If bhiKeysAreRefs Then
                KeyBlock := PffByteArray(DataRefBlock)
              Else
                KeyBlock :=
                  PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * SizeOfRef)]);
            End
          Else
            Begin
              PageNumBlock :=
                PPageNumBlock(@Page^[fsc_BlockHeaderSizeIndex]);
              DataRefBlock :=
                PRefBlock(@Page^[fsc_BlockHeaderSizeIndex +
                (bhiMaxKeyCount * SizeOfPageNum)]);
              If bhiKeysAreRefs Then
                KeyBlock := PffByteArray(DataRefBlock)
              Else
                KeyBlock :=
                  PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
            End;
          {binary search to find out if key is present}
          L := 0;
          R := pred(bhiKeyCount);
          KeyFound := False;
          {note: it is possible for this routine to be called for an empty root}
          If (R >= 0) Then
            Repeat
              M := (L + R) Div 2;
              CompResult := Compare(aKey^, KeyBlock^[M * bhiKeyLength],
                aIndexData.kidCompareData);
              If (CompResult < 0) Then
                R := pred(M)
              Else If (CompResult > 0) Then
                L := succ(M)
              Else {CompResult = 0}  If AllowDups Then
                Begin
                  CompResult := FFCmpI64(aRefNr, DataRefBlock^[M]);
                  If (CompResult < 0) Then
                    R := pred(M)
                  Else If (CompResult > 0) Then
                    L := succ(M)
                  Else {key+refnr have been found}
                    Begin
                      KeyFound := True;
                      Break; {out of the repeat..until loop}
                    End
                End
              Else {key has been found}
                Begin
                  KeyFound := True;
                  Break; {out of the repeat..until loop}
                End
            Until (L > R);
          {if the page is a leaf...}
          If bhiIsLeafPage Then
            Begin
              {if the key was found delete it from the page, return true}
              If KeyFound Then
                Begin
                  { Mark the block as dirty. }
                  FFBMDirtyBlock(aIndexData.kidFI, bhiThisBlock, aTI, Page);
                  {***Delphi32***: the compiler flags M as "possibly used
                                   before definition". Not true.}
                  RemoveKeyFromLeafPage(Page, M);
                  Result := True;
                End
                  {otherwise return false}
              Else
                Result := False;
              DoneRecursing := True;
            End
              { Otherwise the page is an internal node... }
          Else
            If KeyFound Then
              Begin
                {we need to swap this key with its predecessor/successor
                 (this is guaranteed to be on a leaf) then delete the key
                 in the leaf}
                { Mark the block as dirty. }
                FFBMDirtyBlock(aIndexData.kidFI, bhiThisBlock, aTI, Page);
                { Swap the key with a key on a leaf, or merge children,
                  then recursively delete from returned child. }
                Page := BtreeDeleteSwapKey(aIndexData, aTI, Page, M, aKey, aRelMethod);
                aBtreeChanged := True; {!!.05}
                aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
              End
                {otherwise the key was not found...}
            Else
              Begin
                {the key, if anywhere, is in the child subtree at L-1}
                If (L = 0) Then
                  Child := bhiPrevPageRef
                Else
                  Child := PageNumBlock^[pred(L)];
                {read the child's page}
                With aIndexData Do
                  ChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                    fsc_InxBlockTypeBtreePage, Child,
                    aRelMethod);
                aRelList.Append(FFAllocReleaseInfo(ChildPage, TffInt64(aRelMethod)));
                {check whether the child has enough keys, if so recurse}
                If (ChildPageHdr^.bhiKeyCount > (ChildPageHdr^.bhiMaxKeyCount Div 2)) Then
                  Page := ChildPage
                    {otherwise try and make it full enough}
                Else {not enough keys in child}
                  Begin
                    { Mark this page and the child as dirty. }
                    FFBMDirtyBlock(aIndexData.kidFI, bhiThisBlock, aTI, Page);
                    FFBMDirtyBlock(aIndexData.kidFI, Child, aTI, ChildPage);

                    { Redistribute the keys among siblings, or merge. }
                    BtreeDeleteRedistributeOrMerge(aIndexData, aTI,
                      Page, L, ChildPage);
                    aBTreeChanged := True; {!!.05}
                    {recurse ourselves}
                    {Note: it could be that we now have only the minimum number
                           of keys, but it doesn't matter since we'll immediately
                           recurse into one of our children}
                  End;
              End;
        End;
    Until DoneRecursing;
    {***Delphi32***: the compiler tags the return value of this function
                     as being "possibly undefined". Not true, DoneRecursing
                     is only set true once Result has been set true/false}
  Finally
    For Child := 0 To pred(aRelList.Count) Do
      FFDeallocReleaseInfo(aRelList[Child]);
    aRelList.Free;
  End;
End;
{--------}

Function BtreeDelete(Const aRoot: Longint;
  Const aKey: PffByteArray;
  Const aTI: PffTransInfo;
  Const aIndexData: TffKeyIndexData;
  Const aRefNr: TffInt64;
  Var aBTreeChanged: Boolean) {!!.05}
: Boolean;
Var
  RootPage, RootPageClone: PffBlock;
  RootPageHdr: PffBlockHeaderIndex Absolute RootPage;
  RootPageCloneHdr: PffBlockHeaderIndex Absolute RootPageClone;
  aCloneRelMethod,
    aRelMethod: TffReleaseMethod;
Begin
  { Obtain the root page. }
  With aIndexData Do
    RootPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
      fsc_InxBlockTypeBtreePage, aRoot,
      aRelMethod);
  Try
    { Delete the key from this page. }
    Result := BtreeDeleteAmplePage(aIndexData, aTI, RootPage, aKey,
      aRefNr, aBTreeChanged); {!!.05}

    { If the root page is empty then replace it with its first child &
      delete the root page. }
    If Result Then
      Begin
        { Get the root page as it may have been modified. We may be looking at
          the read-only block right now and we need the modified block. }
        RootPageClone := FFBMGetBlock(aIndexData.kidFI, aTI, aRoot, fsc_ReadOnly,
          aCloneRelMethod,fsoNone);
        If (RootPageCloneHdr^.bhiKeyCount = 0) Then
          { Assumption: The root page has been exclusively locked somewhere
                        in the delete process. }
          With aIndexData Do
            Begin
              kidIndexHeader^.bihIndexRoot[kidIndex] := RootPageCloneHdr^.bhiPrevPageRef;
              FFTblHlpDeleteBlock(kidFI, kidFileHeader, RootPageClone);
              dec(kidIndexHeader^.bihIndexPageCount[kidIndex]);
            End;
        aCloneRelMethod(RootPageClone);
      End;
  Finally
    aRelMethod(RootPage);
  End;
End;
{====================================================================}

{===Key reading helper routines======================================}

Function BtreeExistsKey(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aRoot: TffWord32;
  aKey: PffByteArray;
  aRefNr: TffInt64): boolean;
Var
  Page: PffBlock;
  PageHdr: PffBlockHeaderIndex Absolute Page;
  PageNumBlock: PPageNumBlock;
  DataRefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  L, R, M: Integer;
  CompResult: Integer;
  Child: TffWord32;
  Compare: TffKeyCompareFunc;
  CheckDups: boolean;
  KeyFound: boolean;
  DoneRecursing: boolean;
  aRelMethod: TffReleaseMethod;
Begin
  {$IFDEF DefeatWarnings}
  Result := False;
  {$ENDIF}
  {get the root page}
  With aIndexData Do
    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
      fsc_InxBlockTypeBtreePage, aRoot, aRelMethod);
  Try
    {set up the invariants}
    With aIndexData Do
      Begin
        CheckDups := ((kidIndexHeader^.bihIndexFlags[kidIndex] And
          fsc_InxFlagAllowDups) <> 0) And (aRefNr.iLow <> 0) And
          (aRefNr.iHigh <> 0);
        Compare := kidCompare;
      End;
    {simulate recursion (ie unwind it)}
    DoneRecursing := False;
    Repeat
      With PageHdr^ Do
        Begin
          {get the addresses of the reference block and key string block}
          If bhiIsLeafPage Then
            Begin
              PageNumBlock := Nil;
              DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
              If bhiKeysAreRefs Then
                KeyBlock := PffByteArray(DataRefBlock)
              Else
                KeyBlock :=
                  PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * SizeOfRef)]);
            End
          Else
            Begin
              PageNumBlock :=
                PPageNumBlock(@Page^[fsc_BlockHeaderSizeIndex]);
              DataRefBlock :=
                PRefBlock(@Page^[fsc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum)]);
              If bhiKeysAreRefs Then
                KeyBlock := PffByteArray(DataRefBlock)
              Else
                KeyBlock :=
                  PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
            End;
          {binary search to find out if key is present}
          L := 0;
          R := pred(bhiKeyCount);
          KeyFound := False;
          Repeat
            M := (L + R) Div 2;
            CompResult := Compare(aKey^, KeyBlock^[M * bhiKeyLength], aIndexData.kidCompareData);
            If (CompResult < 0) Then
              R := pred(M)
            Else If (CompResult > 0) Then
              L := succ(M)
            Else {CompResult = 0}  If CheckDups Then
              Begin
                CompResult := FFCmpI64(aRefNr, DataRefBlock^[M]);
                If (CompResult < 0) Then
                  R := pred(M)
                Else If (CompResult > 0) Then
                  L := succ(M)
                Else {key+refnr have been found}
                  Begin
                    KeyFound := True;
                    Break; {out of the repeat..until loop}
                  End
              End
            Else {key has been found}
              Begin
                KeyFound := True;
                Break; {out of the repeat..until loop}
              End;
          Until (L > R);
          If KeyFound Then
            Begin
              Result := True;
              DoneRecursing := True;
            End
          Else
            If bhiIsLeafPage Then
              Begin
                {the key was not found at all}
                Result := False;
                DoneRecursing := True;
              End
                {otherwise the page is an internal node...}
            Else
              Begin
                {the key, if anywhere, is in the child subtree at L-1}
                If (L = 0) Then
                  Child := bhiPrevPageRef
                Else
                  Child := PageNumBlock^[pred(L)];
                {read the child's page}
                aRelMethod(Page);
                With aIndexData Do
                  Begin
                    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                      fsc_InxBlockTypeBtreePage, Child,
                      aRelMethod);
                  End;

                {and recurse it}
              End;
        End;
    Until DoneRecursing;

    {***Delphi32***: the compiler tags the return value of this function
                     as being "possibly undefined". Not true, DoneRecursing
                     is only set true once Result has been set true/false}
  Finally
    aRelMethod(Page);
  End;
End;
{--------}

Function BtreeNextKey(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aRefNr: TffInt64;
  Var aKeyPath: TffKeyPath): boolean;
Var
  aInx: Longint;
  Page: PffBlock;
  PageHdr: PffBlockHeaderIndex Absolute Page;
  PageNumBlock: PPageNumBlock;
  DataRefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  PageNum: TffWord32;
  aRelList: TfsPointerList;
  aRelMethod: TffReleaseMethod;
Begin
  { Assumption: the btree has at least one key. }
  aRelList := TfsPointerList.Create;
  Try
    With aKeyPath Do
      Begin
        {patch the path for BOF}
        If (kpPos = kppBOF) Then
          Begin
            kpPath[0].kpePage := aIndexData.kidIndexHeader^.bihIndexRoot[aIndexData.kidIndex];
            kpPath[0].kpeItem := -1;
            kpCount := 1;
          End;
        {get the last page on the key path}
        With aIndexData Do
          Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
            fsc_InxBlockTypeBtreePage,
            kpPath[pred(kpCount)].kpePage, aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
        {if we're on a crack, just return the key pointed to by the path}
        If (kpPos = kppOnCrackBefore) And {!!.03 - Start}
        (kpPath[pred(kpCount)].kpeItem <= pred(PageHdr^.bhiKeyCount)) Then {!!.03 - End}
          With kpPath[pred(kpCount)], PageHdr^ Do
            Begin
              If bhiIsLeafPage Then
                DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex])
              Else
                DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * SizeOfPageNum)]);
              aRefNr := DataRefBlock^[kpeItem];
              If bhiKeysAreRefs Then
                KeyBlock := PffByteArray(DataRefBlock)
              Else If bhiIsLeafPage Then
                KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * SizeOfRef)])
              Else
                KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
              Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
              Result := True;
              Exit;
            End;
        {if the current page is a node, we need to travel down the btree,
         going left all the time, until we hit a leaf}
        If Not PageHdr^.bhiIsLeafPage Then
          Begin
            {read the first child}
            PageNumBlock := PPageNumBlock(@Page^[fsc_BlockHeaderSizeIndex]);
            If (kpPath[pred(kpCount)].kpeItem = -1) Then
              PageNum := PageHdr^.bhiPrevPageRef
            Else
              PageNum := PageNumBlock^[kpPath[pred(kpCount)].kpeItem];
            With aIndexData Do
              Begin
                Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                  fsc_InxBlockTypeBtreePage, PageNum,
                  aRelMethod);
                aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
              End;
            While Not PageHdr^.bhiIsLeafPage Do
              Begin
                With kpPath[kpCount] Do
                  Begin
                    kpePage := PageHdr^.bhiThisBlock;
                    kpeItem := -1;
                  End;
                inc(kpCount);
                With aIndexData Do
                  Begin
                    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                      fsc_InxBlockTypeBtreePage,
                      PageHdr^.bhiPrevPageRef, aRelMethod);
                    aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
                  End;
              End;
            With kpPath[kpCount], PageHdr^ Do
              Begin
                kpePage := PageHdr^.bhiThisBlock;
                kpeItem := 0;
                inc(kpCount);
                DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
                aRefNr := DataRefBlock^[0];
                If bhiKeysAreRefs Then
                  KeyBlock := PffByteArray(DataRefBlock)
                Else
                  KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                    (bhiMaxKeyCount * SizeOfRef)]);
                Move(KeyBlock^[0], aKey^, bhiKeyLength);
              End;
            Result := True;
          End
            {otherwise the current page is a leaf}
            {if the current item is not the final key, just return the next}
        Else If (kpPath[pred(kpCount)].kpeItem < pred(PageHdr^.bhiKeyCount)) Then
          Begin
            With kpPath[pred(kpCount)], PageHdr^ Do
              Begin
                inc(kpeItem);
                DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
                aRefNr := DataRefBlock^[kpeItem];
                If bhiKeysAreRefs Then
                  KeyBlock := PffByteArray(DataRefBlock)
                Else
                  KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                    (bhiMaxKeyCount * SizeOfRef)]);
                Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
                Result := True;
              End;
          End
            {otherwise the current item is the last key on the page, we need to
             travel up the btree returning along the path, until we get to a node
             where the current item is less than the number of keys; if we can't
             find one, return false--there is no next key}
        Else
          Begin
            {read the first parent, assume we won't find a next key}
            dec(kpCount);
            Result := False;
            {while there are still items in the key path}
            While (kpCount > 0) Do
              Begin
                {read the current page}
                With aIndexData Do
                  Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                    fsc_InxBlockTypeBtreePage,
                    kpPath[pred(kpCount)].kpePage,
                    aRelMethod);
                aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
                {if the current item is not the final key, just return the next}
                If (kpPath[pred(kpCount)].kpeItem < pred(PageHdr^.bhiKeyCount)) Then
                  Begin
                    With kpPath[pred(kpCount)], PageHdr^ Do
                      Begin
                        inc(kpeItem);
                        DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex +
                          (bhiMaxKeyCount * SizeOfPageNum)]);
                        aRefNr := DataRefBlock^[kpeItem];
                        If bhiKeysAreRefs Then
                          KeyBlock := PffByteArray(DataRefBlock)
                        Else
                          KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
                        Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
                      End;
                    Result := True;
                    Break; {out of dowhile loop}
                  End;
                {otherwise go back one step}
                dec(kpCount);
              End;
          End; { if }
      End; { with }
  Finally
    For aInx := 0 To pred(aRelList.Count) Do
      FFDeallocReleaseInfo(aRelList[aInx]);
    aRelList.Free;
  End;
End;
{--------}

Function BtreePrevKey(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aRefNr: TffInt64;
  Var aKeyPath: TffKeyPath): boolean;
Var
  aInx: Longint;
  Page: PffBlock;
  PageHdr: PffBlockHeaderIndex Absolute Page;
  PageNumBlock: PPageNumBlock;
  DataRefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  PageNum: TffWord32;
  aRelList: TfsPointerList;
  aRelMethod: TffReleaseMethod;
Begin
  {Assumption: the btree has at least one key
               if the path is pointing to EOF the root page can be
               found at aKeyPath.kpPath[0].kpePage}
  aRelList := TfsPointerList.Create;
  Try
    With aKeyPath Do
      Begin
        {if the keypath points to EOF, then read the root page and set
         the item number of the first path element to the count of keys
         ready for the walk down the btree}
        If (kpPos = kppEOF) Then
          Begin
            With kpPath[0], aIndexData Do
              Begin
                kpePage := kidIndexHeader^.bihIndexRoot[kidIndex];
                Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                  fsc_InxBlockTypeBtreePage, kpePage,
                  aRelMethod);
                aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
                kpeItem := PageHdr^.bhiKeyCount;
              End;
            kpCount := 1;
          End
        Else
          Begin
            {get the last page on the key path}
            With aIndexData Do
              Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                fsc_InxBlockTypeBtreePage,
                kpPath[pred(kpCount)].kpePage, aRelMethod);
            aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
            {if we're on a crack, just return the key pointed to by the path}
            If (kpPos = kppOnCrackAfter) Then
              With kpPath[pred(kpCount)], PageHdr^ Do
                Begin
                  If bhiIsLeafPage Then
                    DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex])
                  Else
                    DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex +
                      (bhiMaxKeyCount * SizeOfPageNum)]);
                  aRefNr := DataRefBlock^[kpeItem];
                  If bhiKeysAreRefs Then
                    KeyBlock := PffByteArray(DataRefBlock)
                  Else If bhiIsLeafPage Then
                    KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                      (bhiMaxKeyCount * SizeOfRef)])
                  Else
                    KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                      (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
                  Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
                  Result := True;
                  Exit;
                End; { with }
          End; { if }

        {if the current page is a node, we need to travel down the btree,
         going right all the time after the first child, until we hit a leaf}
        If Not PageHdr^.bhiIsLeafPage Then
          Begin
            {read the first (ie previous) child}
            dec(kpPath[pred(kpCount)].kpeItem);
            PageNumBlock := PPageNumBlock(@Page^[fsc_BlockHeaderSizeIndex]);
            If (kpPath[pred(kpCount)].kpeItem < 0) Then
              PageNum := PageHdr^.bhiPrevPageRef
            Else
              PageNum := PageNumBlock^[kpPath[pred(kpCount)].kpeItem];
            With aIndexData Do
              Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                fsc_InxBlockTypeBtreePage, PageNum,
                aRelMethod);
            aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
            While Not PageHdr^.bhiIsLeafPage Do
              Begin
                With kpPath[kpCount], PageHdr^ Do
                  Begin
                    kpePage := bhiThisBlock;
                    kpeItem := pred(bhiKeyCount);
                  End;
                inc(kpCount);
                PageNumBlock := PPageNumBlock(@Page^[fsc_BlockHeaderSizeIndex]);
                With aIndexData Do
                  Begin
                    PageNum := PageNumBlock^[pred(PageHdr^.bhiKeyCount)];
                    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                      fsc_InxBlockTypeBtreePage,
                      PageNum, aRelMethod);
                    aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
                  End;
              End;
            With kpPath[kpCount], PageHdr^ Do
              Begin
                kpePage := bhiThisBlock;
                kpeItem := pred(bhiKeyCount);
                inc(kpCount);
                DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
                aRefNr := DataRefBlock^[kpeItem];
                If bhiKeysAreRefs Then
                  KeyBlock := PffByteArray(DataRefBlock)
                Else
                  KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                    (bhiMaxKeyCount * SizeOfRef)]);
                Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
              End;
            Result := True;
          End
            {otherwise the current page is a leaf}
            {if the current item is not the first key, just return the previous}
        Else If (kpPath[pred(kpCount)].kpeItem > 0) Then
          Begin
            With kpPath[pred(kpCount)], PageHdr^ Do
              Begin
                dec(kpeItem);
                DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
                aRefNr := DataRefBlock^[kpeItem];
                If bhiKeysAreRefs Then
                  KeyBlock := PffByteArray(DataRefBlock)
                Else
                  KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                    (bhiMaxKeyCount * SizeOfRef)]);
                Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
                Result := True;
              End;
          End
            {otherwise the current item is the first key on the page, we need to
             travel up the btree returning along the path, until we get to a node
             where the current item is not the first key on the page; if we can't
             find one, return false--there is no previous key}
        Else
          Begin
            {read the first parent, assume we won't find a previous key}
            dec(kpCount);
            Result := False;
            {while there are still items in the key path}
            While (kpCount > 0) Do
              Begin
                {read the current page}
                With aIndexData Do
                  Begin
                    PageNum := kpPath[pred(kpCount)].kpePage;
                    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                      fsc_InxBlockTypeBtreePage, PageNum,
                      aRelMethod);
                    aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
                  End;
                {if the current item is not -1, just return it}
                If (kpPath[pred(kpCount)].kpeItem >= 0) Then
                  Begin
                    With kpPath[pred(kpCount)], PageHdr^ Do
                      Begin
                        DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex +
                          (bhiMaxKeyCount * SizeOfPageNum)]);
                        aRefNr := DataRefBlock^[kpeItem];
                        If bhiKeysAreRefs Then
                          KeyBlock := PffByteArray(DataRefBlock)
                        Else
                          KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
                        Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
                      End;
                    Result := True;
                    Break; {out of dowhile loop}
                  End;
                {otherwise go back one step}
                dec(kpCount);
              End;
          End;
      End; { with }
  Finally
    For aInx := 0 To pred(aRelList.Count) Do
      FFDeallocReleaseInfo(aRelList[aInx]);
    aRelList.Free;
  End;
End;
{--------}

Function BtreeFindKey(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aRoot: TffWord32;
  aKey: PffByteArray;
  Var aRefNr: TffInt64;
  Var aKeyPath: TffKeyPath;
  aAction: TffSearchKeyAction): boolean;
Var
  Page: PffBlock;
  PageHdr: PffBlockHeaderIndex Absolute Page;
  PageNumBlock: PPageNumBlock;
  DataRefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  OurKey: PffByteArray;
  KeyLen: Integer;
  L, R, M: Integer;
  KeyCompResult: Integer;
  RefCompResult: Integer;
  Child: TffWord32;
  Compare: TffKeyCompareFunc;
  CheckDups: boolean;
  KeyFound: boolean;
  DoneRecursing: boolean;
  HasDups: boolean;
  aRelMethod: TffReleaseMethod;
Begin
  { Get the root page. }
  With aIndexData Do
    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
      fsc_InxBlockTypeBtreePage, aRoot, aRelMethod);

  Try
    { Set up the invariants. }
    With aIndexData Do
      Begin
        {HasDups means that there might be duplicate keys here}
        HasDups := (kidIndexHeader^.bihIndexFlags[kidIndex] And
          fsc_InxFlagAllowDups) <> 0;
        {CheckDups means that we're trying to find an exact key/refnr
         combination}
        CheckDups := HasDups And ((aRefNr.iLow <> 0) Or (aRefNr.iHigh <> 0));
        Compare := kidCompare;
      End;

    { Prepare the key path. }
    FFInitKeyPath(aKeyPath);
    {simulate recursion (ie unwind it)}
    DoneRecursing := False;
    Repeat
      With PageHdr^, aKeyPath Do
        Begin
          {get the addresses of the reference block and key string block}
          If bhiIsLeafPage Then
            Begin
              PageNumBlock := Nil;
              DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
              If bhiKeysAreRefs Then
                KeyBlock := PffByteArray(DataRefBlock)
              Else
                KeyBlock :=
                  PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * SizeOfRef)]);
            End
          Else
            Begin
              PageNumBlock :=
                PPageNumBlock(@Page^[fsc_BlockHeaderSizeIndex]);
              DataRefBlock :=
                PRefBlock(@Page^[fsc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum)]);
              If bhiKeysAreRefs Then
                KeyBlock := PffByteArray(DataRefBlock)
              Else
                KeyBlock :=
                  PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
            End;
          {binary search to find out if key is present}
          L := 0;
          R := pred(bhiKeyCount);
          KeyFound := False;
          Repeat
            M := (L + R) Div 2;
            KeyCompResult := Compare(aKey^, KeyBlock^[M * bhiKeyLength], aIndexData.kidCompareData);
            If (KeyCompResult < 0) Then
              R := pred(M)
            Else If (KeyCompResult > 0) Then
              L := succ(M)
            Else {KeyCompResult = 0}  If CheckDups Then
              Begin
                RefCompResult := FFCmpI64(aRefNr, DataRefBlock^[M]);
                If (RefCompResult < 0) Then
                  R := pred(M)
                Else If (RefCompResult > 0) Then
                  L := succ(M)
                Else {key+refnr have been found}
                  Begin
                    KeyFound := True;
                    Break; {out of the repeat..until loop}
                  End
              End
            Else {key has been found}
              Begin
                KeyFound := True;
                Break; {out of the repeat..until loop}
              End;
          Until (L > R);
          {if the key/ref was found then save the final keypath element}
          If KeyFound Then
            Begin
              DoneRecursing := True;
              With kpPath[kpCount] Do
                Begin
                  kpePage := bhiThisBlock;
                  kpeItem := M;
                End;
              inc(kpCount);
              aRefNr := DataRefBlock^[M];
              Move(KeyBlock^[M * bhiKeyLength], aKey^, bhiKeyLength);
              kpPos := kppOnKey;
            End
          Else
            If bhiIsLeafPage Then
              Begin
                {if the index allows dups, the key has been matched and the
                 passed refnr is zero, return the first refnr in the index
                 for the key}
                If CheckDups And (KeyCompResult = 0) And
                  (aRefNr.iLow = 0) And (aRefNr.iHigh = 0) Then
                  Begin
                    KeyFound := True;
                    DoneRecursing := True;
                    With kpPath[kpCount] Do
                      Begin
                        kpePage := bhiThisBlock;
                        kpeItem := L;
                      End;
                    inc(kpCount);
                    aRefNr := DataRefBlock^[L];
                    kpPos := kppOnCrackBefore;
                  End
                Else
                  Begin
                    {the key/ref was not present at all, patch the final
                     keypath node according to the aAction parameter}
                    DoneRecursing := True;
                    With kpPath[kpCount] Do
                      Begin
                        kpePage := bhiThisBlock;
                        Case aAction Of
                          skaEqual: FFInitKeyPath(aKeyPath);
                          skaEqualCrack,
                            skaGreater,
                            skaGreaterEqual:
                            Begin
                              If (L < bhiKeyCount) Then
                                Begin
                                  kpeItem := L;
                                  kpPos := kppOnCrackBefore;
                                End
                              Else
                                Begin
                                  kpeItem := pred(L);
                                  kpPos := kppOnCrackAfter;
                                End;
                            End;
                        End; {case}
                      End;
                    inc(kpCount);
                  End;
              End
                {otherwise the page is an internal node...}
            Else
              Begin
                {the key, if anywhere, is in the child subtree at L-1}
                With kpPath[kpCount] Do
                  Begin
                    kpePage := bhiThisBlock;
                    kpeItem := pred(L);
                  End;
                inc(kpCount);
                If (L = 0) Then
                  Child := bhiPrevPageRef
                Else
                  Child := PageNumBlock^[pred(L)];
                {read the child's page}
                aRelMethod(Page);
                With aIndexData Do
                  Begin
                    { Crab down to child block and unlock parent block. }
                    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
                      fsc_InxBlockTypeBtreePage, Child,
                      aRelMethod);
                  End;
                {and recurse it}
              End;
        End;
    Until DoneRecursing;

    {if the key wasn't found...}
    If (Not KeyFound) Then
      Begin
        { If we don't mind the missing key and can accept being positioned on
          a crack before the next key, our search is over. }
        If (aAction = skaEqualCrack) Then
          KeyFound := True
            {if we can return the next greater key, do so; always return true
         (the caller will be patching the keypath)}
        Else If (aAction <> skaEqual) Then
          Begin
            If BtreeNextKey(aIndexData, aTI, aKey, aRefNr, aKeyPath) Then
              aKeyPath.kpPos := kppOnKey
            Else
              {we hit the end of the index; this is OK, just set the keypath
               to EOF}
              FFSetKeyPathToEOF(aKeyPath);
            KeyFound := True;
          End;
      End
        {otherwise the key was found...}
    Else {KeyFound is true}
      Begin
        {if we actually wanted the next greater key, continue doing next
         key operations until the key returned compares unequal to the one
         we have, or we hit EOF; always return true}
        If (aAction = skaGreater) Then
          Begin
            KeyLen := aIndexData.kidCompareData^.cdKeyLen;
            FFGetMem(OurKey, KeyLen);
            Try
              Move(aKey^, OurKey^, KeyLen);
              Repeat
                KeyFound := BtreeNextKey(aIndexData, aTI, aKey, aRefNr, aKeyPath);
                If KeyFound Then
                  Begin
                    aKeyPath.kpPos := kppOnKey;
                    KeyFound := Compare(aKey^, OurKey^, aIndexData.kidCompareData) = 0
                  End
                Else
                  FFSetKeyPathToEOF(aKeyPath);
              Until (Not KeyFound);
            Finally
              FFFreeMem(OurKey, KeyLen);
            End;
          End
            {otherwise we wanted an equal key}
        Else {aAction <> skaGreater}
          Begin
            {if we were making an exact full key match on an index with
             unique keys, we're done now; otherwise we have to position the
             keypath at the first of possibly many equal partial keys, or
             equal duplicate keys. Note that if the index has dup keys, but
             we've matched exactly on the refnr as well, then we've found
             the exact key}
            If (HasDups And Not CheckDups) Or
              (aIndexData.kidCompareData^.cdFldCnt <> 0) Or
              (aIndexData.kidCompareData^.cdPartLen <> 0) Then
              Begin
                KeyLen := aIndexData.kidCompareData^.cdKeyLen;
                FFGetMem(OurKey, KeyLen);
                Try
                  Move(aKey^, OurKey^, KeyLen);
                  Repeat
                    KeyFound := BtreePrevKey(aIndexData, aTI, aKey, aRefNr, aKeyPath);
                    If KeyFound Then
                      KeyFound := Compare(aKey^, OurKey^, aIndexData.kidCompareData) = 0
                    Else
                      FFSetKeyPathToBOF(aKeyPath);
                  Until (Not KeyFound);
                  BtreeNextKey(aIndexData, aTI, aKey, aRefNr, aKeyPath);
                  aKeyPath.kpPos := kppOnKey;
                Finally
                  FFFreeMem(OurKey, KeyLen);
                End;
              End;
          End;
        {make sure that KeyFound is still true, we may have altered it}
        KeyFound := True;
      End;
    Result := KeyFound;
  Finally
    aRelMethod(Page);
  End;
End;
{--------}

Procedure BtreeFindApprox(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aRoot: TffWord32;
  aKey: PffByteArray;
  Var aRefNr: TffInt64;
  Var aKeyPath: TffKeyPath;
  aPos: Integer);
Var
  Page: PffBlock;
  PageHdr: PffBlockHeaderIndex Absolute Page;
  PageNumBlock: PPageNumBlock;
  DataRefBlock: PRefBlock;
  KeyBlock: PffByteArray;
  Child: TffWord32;
  ChildPos: Integer;
  aRelMethod: TffReleaseMethod;
Begin
  {get the root page}
  With aIndexData Do
    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
      fsc_InxBlockTypeBtreePage, aRoot, aRelMethod);

  Try
    {if the root is a leaf, just do a simple calculation to find
     the key at the approx position}
    If PageHdr^.bhiIsLeafPage Then
      With aKeyPath, PageHdr^ Do
        Begin
          kpCount := 1;
          With kpPath[0] Do
            Begin
              kpePage := bhiThisBlock;
              kpeItem := (aPos * bhiKeyCount) Div 101;
              DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
              aRefNr := DataRefBlock^[kpeItem];
              If bhiKeysAreRefs Then
                KeyBlock := PffByteArray(DataRefBlock)
              Else
                KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                  (bhiMaxKeyCount * SizeOfRef)]);
              Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
            End;
        End
          {otherwise the root is a node, ie has children}
    Else
      With aKeyPath Do
        Begin
          {there will be two levels in the keypath}
          kpCount := 2;
          {set up the first entry in the keypath, calc values for the child}
          With PageHdr^, kpPath[0] Do
            Begin
              kpePage := bhiThisBlock;
              kpeItem := ((aPos * succ(bhiKeyCount)) Div 101) - 1;
              PageNumBlock := PPageNumBlock(@Page^[fsc_BlockHeaderSizeIndex]);
              If (kpeItem = -1) Then
                Child := bhiPrevPageRef
              Else
                Child := PageNumBlock^[kpeItem];
              ChildPos := ((aPos * 100) Div (101 Div succ(bhiKeyCount))) -
                (succ(kpeItem) * 100);
            End;
          {get the child page}
          aRelMethod(Page);
          With aIndexData Do
            Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
              fsc_InxBlockTypeBtreePage, Child, aRelMethod);
          {set up the second entry in the keypath}
          With PageHdr^, kpPath[1] Do
            Begin
              kpePage := bhiThisBlock;
              kpeItem := ((ChildPos * bhiKeyCount) Div 101);
              If bhiIsLeafPage Then
                Begin
                  DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex]);
                  If bhiKeysAreRefs Then
                    KeyBlock := PffByteArray(DataRefBlock)
                  Else
                    KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                      (bhiMaxKeyCount * SizeOfRef)]);
                End
              Else
                Begin
                  DataRefBlock := PRefBlock(@Page^[fsc_BlockHeaderSizeIndex +
                    (bhiMaxKeyCount * SizeOfPageNum)]);
                  If bhiKeysAreRefs Then
                    KeyBlock := PffByteArray(DataRefBlock)
                  Else
                    KeyBlock := PffByteArray(@Page^[fsc_BlockHeaderSizeIndex +
                      (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
                End;
              aRefNr := DataRefBlock^[kpeItem];
              Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
            End;
        End;
  Finally
    aRelMethod(Page);
  End;
End;
{--------}

Procedure BtreeCalcApprox(Const aIndexData: TffKeyIndexData;
  aTI: PffTransInfo;
  aRoot: Longint;
  Const aKeyPath: TffKeyPath;
  Var aPos: Integer);
Var
  Page: PffBlock;
  PageHdr: PffBlockHeaderIndex Absolute Page;
  RootKeyCount: Integer;
  aRelMethod: TffReleaseMethod;
Begin
  {get the root page}
  With aIndexData Do
    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
      fsc_InxBlockTypeBtreePage, aRoot, aRelMethod);

  Try
    {if the root is a leaf, just do a simple calculation to find
     approx position of the key}
    If PageHdr^.bhiIsLeafPage Then
      With aKeyPath.kpPath[0], PageHdr^ Do
        aPos := (kpeItem * 100) Div bhiKeyCount
          {otherwise the root is a node, ie has children}
    Else
      With aKeyPath Do
        Begin
          {there will be two levels to check in the keypath}
          RootKeyCount := PageHdr^.bhiKeyCount;
          {get the relevant child page}
          aRelMethod(Page);
          With aIndexData Do
            Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
              fsc_InxBlockTypeBtreePage, kpPath[1].kpePage,
              aRelMethod);

          {calculate the position}
          If PageHdr^.bhiIsLeafPage Then
            aPos := ((100 + (kpPath[1].kpeItem * 100) Div PageHdr^.bhiKeyCount) *
              succ(kpPath[0].kpeItem)) Div
              succ(RootKeyCount)
          Else
            aPos := ((100 + (succ(kpPath[1].kpeItem) * 100) Div succ(PageHdr^.bhiKeyCount)) *
              succ(kpPath[0].kpeItem)) Div
              succ(RootKeyCount);
        End;
  Finally
    aRelMethod(Page);
  End;
End;
{====================================================================}

{===Key access related routines======================================}

Procedure FFTblDeleteAllKeys(aTI: PffTransInfo; Var aIndex: TffKeyIndexData);
Var
  InxBlock: PffBlock;
  InxBlockHdr: PffBlockHeaderIndex Absolute InxBlock;
  Root: TffWord32;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  {Note: this routine can only be run in a 'subset' transaction.
         Essentially all the index pages are going to be changed: if
         they were all in a normal transaction, we could quite easily
         run out of memory trying to hold all the dirty pages in
         memory. A corollary is that FFTblDeleteAllKeys cannot be
         rolled back. Another is that if this crashes, the index file
         is pretty well hosed and should be rebuilt.}
  With aIndex Do
    Begin
      { Obtain a Share lock on the file header.  We need the file header
        but we don't need to modify it. }
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_ReadOnly, aRelMethod,fsoNone));

      Try
        { Get an Exclusive lock on the index header. }
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_MarkDirty,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader,
          aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          {get the root page}
          Root := kidIndexHeader^.bihIndexRoot[kidIndex];
          {patch the index header}
          With kidIndexHeader^ Do
            Begin
              bihIndexRoot[kidIndex] := fsc_W32NoValue;
              bihIndexPageCount[kidIndex] := 0;
            End;
          {special case: if the root page does not exist, just return
           (ie there are no keys)}
          If (Root = fsc_W32NoValue) Then
            Exit;
          { Otherwise go delete all the index pages. }
          BtreeDeleteIndexPage(aIndex, aTI, Root);
          { Allow the caller to do the final commit. }
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(PffBlock(kidFileHeader));
      End;
    End; { with }
End;
{--------}

Function FFTblDeleteKey(Const aTI: PffTransInfo;
  Const aKey: PffByteArray;
  Const aRefNr: TffInt64;
  Var aIndex: TffKeyIndexData;
  Var aBTreeChanged: Boolean): Boolean; {!!.05}
Var
  InxBlock: PffBlock;
  InxBlockHdr: PffBlockHeaderIndex Absolute InxBlock;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  With aIndex Do
    Begin
      { Mark the file header as dirty. }
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_MarkDirty, aRelMethod,fsoNone));
      Try
        { Obtain an Exclusive lock on the index header. }
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_MarkDirty,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader,
          aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          {special case: if the root page does not exist, return false}
          If (kidIndexHeader^.bihIndexRoot[kidIndex] = fsc_W32NoValue) Then
            Result := False
              {otherwise go delete from the b-tree}
          Else
            Result := BtreeDelete(kidIndexHeader^.bihIndexRoot[kidIndex],
              aKey, aTI, aIndex, aRefNr,
              aBTreeChanged); {!!.05}
          {decrement the number of keys}
          If Result Then
            dec(kidIndexHeader^.bihIndexKeyCount[kidIndex]);
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(PffBlock(kidFileHeader));
      End;
    End;
End;
{--------}

Function FFTblFindKey(Var aIndex: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath;
  aAction: TffSearchKeyAction): boolean;
Var
  InxBlock: PffBlock;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  With aIndex Do
    Begin
      {get the file header, block 0}
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_ReadOnly, aRelMethod,fsoNone));
      Try
        {get the index header}
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader, aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          {special case: if the root page does not exist}
          If (kidIndexHeader^.bihIndexRoot[kidIndex] = fsc_W32NoValue) Then
            Begin
              If (aAction = skaEqual) Then
                Result := False
              Else
                Begin
                  Result := True;
                  FFSetKeyPathToEOF(aKeyPath);
                End;
            End
              {otherwise go read the b-tree}
          Else
            Result := BtreeFindKey(aIndex, aTI,
              kidIndexHeader^.bihIndexRoot[kidIndex],
              aKey, aRefNr,
              aKeyPath,
              aAction);
          aKeyPath.kpLSN := kidFI^.fiBufMgr.GetRAMPageLSN2 {!!.06}
          (kidFI, kidFileHeader^.bhfIndexHeader); {!!.06}
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(PffBlock(kidFileHeader));
      End;
    End; { with }

  {if the key was not found, ensure the path is invalidated}
  If (Not Result) And (aAction = skaEqual) Then
    FFInitKeyPath(aKeyPath);
End;
{--------}

Function FFTblGetApproxPos(Var aIndex: TffKeyIndexData;
  Var aPos: Integer;
  aTI: PffTransInfo;
  Const aKeyPath: TffKeyPath): boolean;
Var
  InxBlock: PffBlock;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  With aIndex Do
    Begin
      {get the file header, block 0}
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_ReadOnly,
        aRelMethod,fsoNone));
      Try
        {get the index header}
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader, aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          {special case: if the root page does not exist, return false}
          If (kidIndexHeader^.bihIndexRoot[kidIndex] = fsc_W32NoValue) Then
            Result := False
              {otherwise go read the b-tree}
          Else
            Begin
              Result := True;
              BtreeCalcApprox(aIndex, aTI, kidIndexHeader^.bihIndexRoot[kidIndex],
                aKeyPath, aPos);
            End;
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(PffBlock(kidFileHeader));
      End;
    End; { with }
End;
{--------}

Function FFTblInsertKey(Var aIndex: TffKeyIndexData;
  Const aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray): boolean;
Var
  InxBlock, {!!.11}
  InxNewBlock: PffBlock; {!!.11}
  InxBlockHdr: PffBlockHeaderIndex; {!!.11}
  aInxRelMeth,
    aInxRelMethNewBlock, {!!.11}
  aRelMethod: TffReleaseMethod;
Begin
  With aIndex Do
    Begin
      { Obtain an Exclusive lock on the file header. }
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_MarkDirty, aRelMethod,fsoNone));
      Try
        { Dirty the index header. }
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_MarkDirty,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader, aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          { Special case: if the root page does not yet exist, create a new one
            and add the key to it. }
          If (kidIndexHeader^.bihIndexRoot[kidIndex] = fsc_W32NoValue) Then
            Begin
              {Begin !!.11}
              //          aInxRelMeth(InxBlock);
              InxNewBlock := GetNewInxBtreeBlock(kidFI, aTI, kidIndexHeader,
                kidIndex, True,
                aInxRelMethNewBlock);
              Try
                InxBlockHdr := PffBlockHeaderIndex(InxNewBlock);
                kidIndexHeader^.bihIndexRoot[kidIndex] := InxBlockHdr^.bhiThisBlock;
                InsertKeyInLeafPage(InxNewBlock, 0, aKey, aRefNr);
                Result := True;
              Finally
                aInxRelMethNewBlock(InxNewBlock);
              End;
              {End !!.11}
            End
              {otherwise insert the key in the relevant leaf page}
          Else
            Result := BtreeInsert(aIndex, aTI,
              kidIndexHeader^.bihIndexRoot[kidIndex],
              aKey, aRefNr);
          {increment the number of keys if key was added}
          If Result Then
            inc(kidIndexHeader^.bihIndexKeyCount[kidIndex]);
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(Pffblock(kidFileHeader));
      End;
    End; { with }
End;
{--------}

Function FFTblKeyExists(Var aIndex: TffKeyIndexData;
  Const aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray): boolean;
Var
  InxBlock: PffBlock;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  { If the lock duration is ffldShort then this method will free the locks
    after it has finished searching for the key. }
  With aIndex Do
    Begin
      {get the file header, block 0}
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_ReadOnly, aRelMethod,fsoNone));
      Try
        {get the index header}
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader, aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          {special case: if the root page does not exist, return false}
          If (kidIndexHeader^.bihIndexRoot[kidIndex] = fsc_W32NoValue) Then
            Result := False
              {otherwise go read the b-tree}
          Else
            Result := BtreeExistsKey(aIndex, aTI,
              kidIndexHeader^.bihIndexRoot[kidIndex],
              aKey, aRefNr);
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(PffBlock(kidFileHeader));
      End;
    End; { with }
End;
{--------}

Function FFTblNextKey(Var aIndex: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): boolean;
Var
  //  IndexMap : TffbmRAMPage;                                           {Deleted !!.06}
  IndexMapLSN: TffWord32; {!!.06}
  InxBlock: PffBlock;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  {if the keypath is valid and it's at EOF, there is no next key}
  If (aKeyPath.kpPos = kppEOF) Then
    Begin
      Result := False;
      Exit;
    End;
  {otherwise do some work}
  With aIndex Do
    Begin
      {get the file header, block 0}
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_ReadOnly, aRelMethod,fsoNone));
      Try
        {get the index header}
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader, aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          {special case: if the root page does not exist, return false}
          If (kidIndexHeader^.bihIndexRoot[kidIndex] = fsc_W32NoValue) Then
            Result := False
              {otherwise go read the b-tree}
          Else
            Begin
              IndexMapLSN := kidFI^.fiBufMgr.GetRAMPageLSN2 {!!.06}
              (kidFI, kidFileHeader^.bhfIndexHeader);
              If (aKeyPath.kpPos = kppUnknown) Then
                FFSetKeyPathToBOF(aKeyPath)
                  {Begin !!.06}
              Else
                Begin
                  { Has the index map changed since our last visit? }
                  If (((aKeyPath.kpLSN > 0) And
                    (IndexMapLSN > aKeyPath.kpLSN)) Or {!!.06}
                    (aKeyPath.kpPos = kppUnknown)) Then
                    Begin {!!.05}
                      { Yes.  Reposition. }
                      Result := BtreeFindKey(aIndex, aTI,
                        KidIndexHeader^.bihIndexRoot[kidIndex],
                        aKey, aRefNr, aKeyPath, skaEqualCrack);
                      If Not Result Then
                        Exit;
                    End;
                End;
              Result := BtreeNextKey(aIndex, aTI, aKey, aRefNr, aKeyPath);
              aKeyPath.kpLSN := IndexMapLSN; {!!.06}
            End;
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(PffBlock(kidFileHeader));
      End;
    End; { with }
  {if a key was found, ensure the path points to a key}
  If Result Then
    aKeyPath.kpPos := kppOnKey
      {if no next key found, ensure the path points to EOF}
  Else
    FFSetKeyPathToEOF(aKeyPath);
End;
{--------}

Function FFTblPrevKey(Var aIndex: TffKeyIndexData;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): boolean;
Var
  //  IndexMap : TffbmRAMPage;                                           {Deleted !!.06}
  IndexMapLSN: TffWord32; {!!.06}
  InxBlock: PffBlock;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  {if the keypath is valid and it's at BOF, there is no prev key}
  If (aKeyPath.kpPos = kppBOF) Then
    Begin
      Result := False;
      Exit;
    End;
  {otherwise do some work}
  With aIndex Do
    Begin
      {get the file header, block 0}
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_ReadOnly, aRelMethod,fsoNone));
      Try
        {get the index header}
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader, aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          {special case: if the root page does not exist, return false}
          If (kidIndexHeader^.bihIndexRoot[kidIndex] = fsc_W32NoValue) Then
            Result := False
              {otherwise go read the b-tree}
          Else
            Begin
              IndexMapLSN := kidFI^.fiBufMgr.GetRAMPageLSN2 {!!.06}
              (kidFI, kidFileHeader^.bhfIndexHeader); {!!.06}
              If (aKeyPath.kpPos = kppUnknown) Then
                FFSetKeyPathToEOF(aKeyPath);
              { Has the index map changed since our last visit? }
              If (((aKeyPath.kpLSN > 0) And
                (IndexMapLSN > aKeyPath.kpLSN)) Or {!!.06}
                (aKeyPath.kpPos = kppUnknown)) Then
                Begin {!!.05}
                  { Yes.  Reposition. }
                  Result := BtreeFindKey(aIndex, aTI,
                    KidIndexHeader^.bihIndexRoot[kidIndex],
                    aKey, aRefNr, aKeyPath, skaEqualCrack);
                  If Not Result Then
                    Exit;
                End;
              Result := BtreePrevKey(aIndex, aTI, aKey, aRefNr, aKeyPath);
              aKeyPath.kpLSN := IndexMapLSN; {!!.06}
            End;
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(PffBlock(kidFileHeader));
      End;
    End; { with }

  {if a key was found, ensure the path points to a key}
  If Result Then
    aKeyPath.kpPos := kppOnKey
      {if no previous key found, ensure the path points to BOF}
  Else
    FFSetKeyPathToBOF(aKeyPath);
End;
{--------}

Function FFTblSetApproxPos(Var aIndex: TffKeyIndexData;
  aPos: Integer;
  Var aRefNr: TffInt64;
  aTI: PffTransInfo;
  aKey: PffByteArray;
  Var aKeyPath: TffKeyPath): boolean;
Var
  InxBlock: PffBlock;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  {validate the position to be 0..100}
  If (aPos < 0) Or (aPos > 100) Then
    With aIndex Do
      FSRaiseException(EfsServerException, fsStrResServer, fserrBadApproxPos,
        [kidFI^.fiName^, kidIndex, aPos]);

  With aIndex Do
    Begin
      {get the file header, block 0}
      kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
        fsc_ReadOnly, aRelMethod,fsoNone));
      Try
        {get the index header}
        InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, fsc_ReadOnly,
          fsc_InxBlockTypeHeader,
          kidFileHeader^.bhfIndexHeader, aInxRelMeth);
        Try
          kidIndexHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
          {special case: if the root page does not exist, return false
           and an invalid keypath}
          If (kidIndexHeader^.bihIndexRoot[kidIndex] = fsc_W32NoValue) Then
            Begin
              Result := False;
              FFInitKeyPath(aKeyPath);
            End
              {otherwise go read the b-tree}
          Else
            Begin
              Result := True;
              BtreeFindApprox(aIndex, aTI,
                kidIndexHeader^.bihIndexRoot[kidIndex],
                aKey, aRefNr, aKeyPath, aPos);
            End;
          aKeyPath.kpLSN := kidFI^.fiBufMgr.GetRAMPageLSN2 {!!.06}
          (kidFI, kidFileHeader^.bhfIndexHeader); {!!.06}
        Finally
          aInxRelMeth(InxBlock);
        End;
      Finally
        aRelMethod(PffBlock(kidFileHeader));
      End;
    End; { with }
End;
{====================================================================}

{===Index related routines===========================================}

Procedure FFTblAddIndex(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aIndex: Integer;
  aMaxKeyLen: Integer;
  aAllowDups: boolean;
  aKeysAreRefs: boolean);
Var
  FileHeader: PffBlockHeaderFile;
  InxBlock: PffBlock;
  InxHeader: PffIndexHeader;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  { First get an Exclusive lock on the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod,fsoNone));
  Try
    { Second get an Exclusive lock on the index header. }
    InxBlock := ReadVfyInxBlock(aFI, aTI, FileHeader, fsc_MarkDirty,
      fsc_InxBlockTypeHeader,
      FileHeader^.bhfIndexHeader, aInxRelMeth);
    InxHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);

    { Set up the index data. }
    With InxHeader^, FileHeader^ Do
      Begin
        {note that there is only *one* index that uses references as
         keys: index 0}
        If aKeysAreRefs Then
          Begin
            bihIndexFlags[aIndex] := fsc_InxFlagKeysAreRefs; {ie no dups!}
            bihIndexKeyLen[aIndex] := SizeOfRef;
            bhfHasSeqIndex := 1;
          End
        Else
          Begin
            bihIndexKeyLen[aIndex] := aMaxKeyLen;
            If aAllowDups Then
              bihIndexFlags[aIndex] := fsc_InxFlagAllowDups
            Else
              bihIndexFlags[aIndex] := 0;
          End;
        bihIndexRoot[aIndex] := fsc_W32NoValue;
      End;
    aInxRelMeth(InxBlock);
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;
{--------}

Procedure FFTblDeleteIndex(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aIndex: Integer);
Var
  FileHeader: PffBlockHeaderFile;
  InxBlock: PffBlock;
  InxHeader: PffIndexHeader;
  Elements: Integer;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  { First get an Exclusive lock on the file header, block 0.}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod,fsoNone));
  Try
    { Second get an Exclusive lock on the index header. }
    InxBlock := ReadVfyInxBlock(aFI, aTI, FileHeader, fsc_MarkDirty,
      fsc_InxBlockTypeHeader,
      FileHeader^.bhfIndexHeader, aInxRelMeth);
    InxHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);

    { Remove the index data. }
    With InxHeader^ Do
      Begin
        If (aIndex < pred(fscl_MaxIndexes)) Then
          Begin
            Elements := pred(fscl_MaxIndexes - aIndex);
            Move(bihIndexKeyLen[succ(aIndex)], bihIndexKeyLen[aIndex], Elements * sizeof(Word));
            Move(bihIndexFlags[succ(aIndex)], bihIndexFlags[aIndex], Elements * sizeof(Byte));
            Move(bihIndexRoot[succ(aIndex)], bihIndexRoot[aIndex], Elements * sizeof(Longint));
            Move(bihIndexPageCount[succ(aIndex)], bihIndexPageCount[aIndex], Elements * sizeof(Longint));
          End;
        bihIndexKeyLen[pred(fscl_MaxIndexes)] := 0;
        bihIndexFlags[pred(fscl_MaxIndexes)] := 0;
        bihIndexRoot[pred(fscl_MaxIndexes)] := fsc_W32NoValue;
        bihIndexPageCount[pred(fscl_MaxIndexes)] := 0;
      End;
    aInxRelMeth(InxBlock);
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;
{--------}

Procedure FFTblPrepareIndexes(aFI: PffFileInfo;
  aTI: PffTransInfo);
Var
  FileHeader: PffBlockHeaderFile;
  InxBlock: PffBlock;
  InxBlockHdr: PffBlockHeaderIndex Absolute InxBlock;
  InxHeader: PffIndexHeader;
  aInxRelMeth,
    aRelMethod: TffReleaseMethod;
Begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
    aRelMethod,fsoNone));
  Try
    {create the index header block}
    InxBlock := GetNewInxHeaderBlock(aFI, aTI, aInxRelMeth);
    With FileHeader^ Do
      Begin
        bhfIndexHeader := InxBlockHdr^.bhiThisBlock;
        InxHeader := PffIndexHeader(@InxBlock^[fsc_BlockHeaderSizeIndex]);
        With InxHeader^ Do
          Begin
            {set up the internal fields}
            FillChar(bihIndexKeyLen, sizeof(bihIndexKeyLen), 0);
            FillChar(bihIndexFlags, sizeof(bihIndexFlags), 0);
            FillChar(bihIndexRoot, sizeof(bihIndexRoot), fsc_W32NoValue);
            FillChar(bihIndexPageCount, sizeof(bihIndexPageCount), 0);
          End;
      End;
    aInxRelMeth(InxBlock);
  Finally
    aRelMethod(PffBlock(FileHeader));
  End;
End;
{====================================================================}

{===Keypath routines=================================================}

Procedure FFInitKeyPath(Var aKeyPath: TffKeyPath);
Begin
  FillChar(aKeyPath, sizeof(aKeyPath), 0);
End;
{--------}

Procedure FFSetKeyPathToBOF(Var aKeyPath: TffKeyPath);
Begin
  FillChar(aKeyPath, sizeof(aKeyPath), 0);
  aKeyPath.kpPos := kppBOF;
End;
{--------}

Procedure FFSetKeyPathToEOF(Var aKeyPath: TffKeyPath);
Begin
  FillChar(aKeyPath, sizeof(aKeyPath), 0);
  aKeyPath.kpPos := kppEOF;
End;
{====================================================================}

End.

