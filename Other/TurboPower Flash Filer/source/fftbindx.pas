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

{$I ffdefine.inc}

unit fftbindx;

interface

uses
  SysUtils,
  ffconst,
  ffllbase,
  ffsrmgr,
  ffllexcp,
  ffsrintf,
  ffsrbase,
  fffile,
  ffsrlock,
  fftbbase,
  fftbdict;

{$IFDEF FF_DEBUG}
var
  FFDEBUG_IndexCounter : record
    Splits,
    RotateLeftNode,
    RotateRightNode,
    RotateLeftLeaf,
    RotateRightLeaf,
    Merge,
    SwapNext,
    SwapPrev : integer;
  end;
{$ENDIF}

type
  PffKeyIndexData = ^TffKeyIndexData;
  TffKeyIndexData = record              {Data record for key routines}
    {must be supplied}
    kidFI         : PffFileInfo;        {..index file}
    kidIndex      : integer;            {..index number}
    kidCompare    : TffKeyCompareFunc;  {..compare routine}
    kidCompareData: PffCompareData;     {..compare data}
    {calculated internally}
    kidFileHeader : PffBlockHeaderFile; {..pointer to the index file header}
    kidIndexHeader: PffIndexHeader;     {..pointer to the index header}
    {used elsewhere}
    kidIndexType  : TffIndexType;       {..index type: composite or user-defined}
  end;

type
  {Note: an key path is a structure which defines a particular
         key in a B-Tree. The path defines the page numbers and the
         element numbers into the key arrays in the pages to get to
         a particular key. If the position is OnKey then the keypath
         points exactly to that key. If the position is OnCrack then
         the keypath points to the key that would be retrieved by a
         NextKey or a PrevKey operation.
         An invalid keypath has an path element count of zero and a
         position of Unknown}
  TffKeyPathPosition = (       {Possible positions of a keypath..}
    kppUnknown,                {..unknown}
    kppBOF,                    {..before all keys}
    kppOnCrackBefore,          {..in between two keys}
    kppOnCrackAfter,           {..in between two keys}
    kppOnKey,                  {..on a key}
    kppEOF);                   {..after all keys}
  TffKeyPathElement = record   {An element of a key path}
    kpePage : TffWord32;       {..the page number}
    kpeItem : integer;         {..the element number of the key}
  end;
  PffKeyPath = ^TffKeyPath;
  TffKeyPath = record             {The key path type}
    kpCount : integer;            {..number of active elements in the path}
    kpPath  : array [0..31] of TffKeyPathElement; {..the path}
    kpPos   : TffKeyPathPosition; {..it's position}
    kpLSN   : TffWord32;          {...LSN of the index map page at the time
                                      we positioned to this record.  If the LSN
                                      has changed then our key path is no longer
                                      valid. }
  end;
  {Note: 32 elements is *ample*, 32 levels in a sparsely populated order 3
         B-Tree would hold 4 billion keys: anyone who creates such a B-Tree
         (ie 1Kb keys using a 4Kb block) deserve what they get if they even
         could.}

{---Key path related routines---}
procedure FFInitKeyPath(var aKeyPath : TffKeyPath);
procedure FFSetKeyPathToBOF(var aKeyPath : TffKeyPath);
procedure FFSetKeyPathToEOF(var aKeyPath : TffKeyPath);


{---Index related routines--- (NOT THREAD-SAFE)}
procedure FFTblAddIndex(aFI          : PffFileInfo;
                        aTI          : PffTransInfo;
                        aIndex       : integer;
                        aMaxKeyLen   : integer;
                        aAllowDups   : boolean;
                        aKeysAreRefs : boolean);
  {-Add an index to a file}
procedure FFTblDeleteIndex(aFI    : PffFileInfo;
                           aTI    : PffTransInfo;
                           aIndex : integer);
  {-Delete an index from a file}
procedure FFTblPrepareIndexes(aFI : PffFileInfo;
                              aTI : PffTransInfo);
  {-Prepare a file to contain indexes}


{---Key access related routines--- (NOT THREAD-SAFE)}
procedure FFTblDeleteAllKeys(aTI : PffTransInfo; var aIndex : TffKeyIndexData);
  {-Delete all keys from an index.
    Note: cannot be used inside a transaction--it implements a
          low level file update}
function FFTblDeleteKey(const aTI           : PffTransInfo;
                        const aKey          : PffByteArray;
                        const aRefNr        : TffInt64;
                          var aIndex        : TffKeyIndexData;
                          var aBTreeChanged : Boolean) : Boolean;      {!!.05}
  {-Delete a key/ref from an index}
function FFTblFindKey(var aIndex   : TffKeyIndexData;
                      var aRefNr   : TffInt64;
                          aTI      : PffTransInfo;
                          aKey     : PffByteArray;
                      var aKeyPath : TffKeyPath;
                          aAction  : TffSearchKeyAction) : boolean;
  {-Find the given key/ref (or nearby one) in the index, set up the
    keypath and the key/ref found, return true; if key/ref not found,
    return false and an invalid keypath. Note that if the index allows
    dups and the refnr is zero and the key matches, the first matching
    key/ref is returned. Note also the keypath is positioned on the
    crack for the key/ref in question.}
function FFTblGetApproxPos(var aIndex   : TffKeyIndexData;
                           var aPos     : integer;
                               aTI      : PffTransInfo;
                         const aKeyPath : TffKeyPath) : boolean;
  {-Given a valid keypath to key/ref, calculate the approx position of
    that key/ref in the b-tree as percentage.}
function FFTblInsertKey(var aIndex : TffKeyIndexData;
                      const aRefNr : TffInt64;
                            aTI    : PffTransInfo;
                            aKey   : PffByteArray) : boolean;
  {-Insert a key/ref into an index}
function FFTblKeyExists(var aIndex : TffKeyIndexData;
                      const aRefNr : TffInt64;
                            aTI    : PffTransInfo;
                            aKey   : PffByteArray) : boolean;
  {-Return true if key/ref exists in index.  If the lock duration is
    ffldShort then index locks are released once this method has finished
    using the index pages. }
function FFTblNextKey(var aIndex   : TffKeyIndexData;
                      var aRefNr   : TffInt64;
                          aTI      : PffTransInfo;
                          aKey     : PffByteArray;
                      var aKeyPath : TffKeyPath) : boolean;
  {-Given a keypath, find the next key/ref in the index, set up the
    keypath and key/ref to it, return true; if no next key, return
    false and set keypath to EOF}
function FFTblPrevKey(var aIndex   : TffKeyIndexData;
                      var aRefNr   : TffInt64;
                          aTI      : PffTransInfo;
                          aKey     : PffByteArray;
                      var aKeyPath : TffKeyPath) : boolean;
  {-Given a keypath, find the previous key/ref in the index, set up
    the keypath and key/ref to it, return true; if no previous key,
    return false and set keypath to BOF}
function FFTblSetApproxPos(var aIndex   : TffKeyIndexData;
                               aPos     : integer;
                           var aRefNr   : TffInt64;
                               aTI      : PffTransInfo;
                               aKey     : PffByteArray;
                           var aKeyPath : TffKeyPath) : boolean;
  {-Set the keypath to the approximate position given by aPos (a percentage),
    return true and the key/ref if able to, return false if not. The returned
    keypath will have length 2, unless the b-tree only consists of the root
    page, in which case it will be length 1.

    Note: All index pages accessed by this method are Share locked for duration
          ffldCommit.}

implementation

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

type
  PRef = ^TRef;
  TRef = TffInt64;
  PPageNum = ^TpageNum;
  TPageNum = TffWord32;

const
  SizeOfRef = sizeof(TRef);
  SizeOfPageNum = sizeof(TPageNum);

type
  PRefBlock = ^TRefBlock;
  TRefBlock = array [0..($FFFFFFF div SizeOfRef)-1] of TRef;

  PPageNumBlock = ^TPageNumBlock;
  TPageNumBlock = array [0..($FFFFFFF div SizeOfPageNum)-1] of TPageNum;

{===Helper routines==================================================}
function GetNewInxHeaderBlock(aFI          : PffFileInfo;
                              aTI          : PffTransInfo;
                          var aReleaseMethod : TffReleaseMethod  ) : PffBlock;
  {-Return a new index header block, pre-mark as dirty}
var
  InxBlockHdr : PffBlockHeaderIndex absolute Result;
  InxHeader   : PffIndexHeader;
begin
  Result := FFTblHlpGetNewBlock(aFI, aTI, aReleaseMethod);
  with InxBlockHdr^ do begin
    bhiSignature := ffc_SigIndexBlock;
    bhiNextBlock := ffc_W32NoValue;
    bhiLSN := 0;
    bhiBlockType := ffc_InxBlockTypeHeader;
    bhiIsLeafPage := false;           {not used in header}
    bhiNodeLevel := 0;                {not used in header}
    bhiKeysAreRefs := false;          {not used in header}
    bhiIndexNum := $FFFF;             {not used in header}
    bhiKeyLength := 0;                {not used in header}
    bhiKeyCount := 0;                 {not used in header}
    bhiMaxKeyCount := 0;              {not used in header}
    bhiPrevPageRef := ffc_W32NoValue; {not used in header}
  end;
  InxHeader := PffIndexHeader(@Result^[ffc_BlockHeaderSizeIndex]);
  FillChar(InxHeader^, sizeof(TffIndexHeader), 0);
end;
{--------}
function GetNewInxBtreeBlock(aFI          : PffFileInfo;
                             aTI          : PffTransInfo;
                             aIndexHeader : PffIndexHeader;
                             aIndex       : integer;
                             aIsLeaf      : boolean;
                         var aReleaseMethod : TffReleaseMethod) : PffBlock;
  {-Return a new index btree node/leaf block, pre-mark as dirty}
var
  InxBlockHdr : PffBlockHeaderIndex absolute Result;
begin
  Result := FFTblHlpGetNewBlock(aFI, aTI, aReleaseMethod);
  with InxBlockHdr^, aIndexHeader^ do begin
    bhiSignature := ffc_SigIndexBlock;
    bhiNextBlock := ffc_W32NoValue;
    bhiBlockType := ffc_InxBlockTypeBtreePage;
    bhiIsLeafPage := aIsLeaf;
    if aIsLeaf then
      bhiNodeLevel := 1  {leaves are at level 1}
    else
      bhiNodeLevel := 0; {ie haven't a clue at present}
    bhiKeysAreRefs := (bihIndexFlags[aIndex] and ffc_InxFlagKeysAreRefs) <> 0;
    bhiIndexNum := aIndex;
    bhiKeyLength := bihIndexKeyLen[aIndex];
    bhiKeyCount := 0;
    if aIsLeaf then
      if bhiKeysAreRefs then
        bhiMaxKeyCount :=
           (aFI^.fiBlockSize - ffc_BlockHeaderSizeIndex)
              div (SizeOfRef)
      else
        bhiMaxKeyCount :=
           (aFI^.fiBlockSize - ffc_BlockHeaderSizeIndex)
              div (bhiKeyLength + SizeOfRef)
    else {it's a node}
      if bhiKeysAreRefs then
        bhiMaxKeyCount :=
           (aFI^.fiBlockSize - ffc_BlockHeaderSizeIndex)
              div (SizeOfPageNum + SizeOfRef)
      else
        bhiMaxKeyCount :=
           (aFI^.fiBlockSize - ffc_BlockHeaderSizeIndex)
              div (bhiKeyLength + SizeOfPageNum + SizeOfRef);
    if not Odd(bhiMaxKeyCount) then
      dec(bhiMaxKeyCount);
    bhiPrevPageRef  := ffc_W32NoValue;
    inc(bihIndexPageCount[aIndex]);
  end;
end;
{--------}
function ReadVfyInxBlock(aFI          : PffFileInfo;
                         aTI          : PffTransInfo;
                         aFileHeader  : PffBlockHeaderFile;
                   const aMarkDirty   : boolean;
                   const aBlockType   : integer;
                   const aBlockNumber : TffWord32;
                     var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  InxBlockHdr : PffBlockHeaderIndex absolute Result;
begin
  with aFileHeader^ do begin
    {verify the block number}
    if (aBlockNumber <= 0) or (aBlockNumber >= bhfUsedBlocks) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadBlockNr,
                       [aFI^.fiName^, aBlockNumber]);
    {now get the record block; note: mark header block as fixed}
    Result := FFBMGetBlock(aFI, aTI, aBlockNumber, aMarkDirty, aReleaseMethod);
    {verify that it's an index block}
    if (InxBlockHdr^.bhiSignature <> ffc_SigIndexBlock) or
       (InxBlockHdr^.bhiThisBlock <> aBlockNumber) or
       (InxBlockHdr^.bhiBlockType <> aBlockType) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadInxBlock,
                       [aFI^.fiName^, aBlockNumber]);
  end;
end;
{====================================================================}


{===Key rotation routines============================================}
procedure RotateLeftLeaf(aParentPage : PffBlock;
                         aSeparator  : Longint;
                         aChildLeft  : PffBlock;
                         aChildRight : PffBlock);
  {-Rotate keys from right leaf child to left leaf child through key in
    parent given by separator index. Equalise number of keys}
var
  ParentPageHdr : PffBlockHeaderIndex absolute aParentPage;
  ChildLeftHdr  : PffBlockHeaderIndex absolute aChildLeft;
  ChildRightHdr : PffBlockHeaderIndex absolute aChildRight;
  KeysToMove    : Longint;
  OffsetL       : Longint;
  OffsetR       : Longint;
  OffsetP       : Longint;
  BytesToMove   : Longint;
begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.RotateLeftLeaf);
  {$ENDIF}
  {calculate the number of keys to move, this means that the right child
   will *lose* this number of keys and the left child will *gain* this
   number}
  KeysToMove := (ChildRightHdr^.bhiKeyCount - ChildLeftHdr^.bhiKeyCount) div 2;
  if (KeysToMove = 0) then
    inc(KeysToMove);
  {move the first pred(KeysToMove) keys from the right child to the last
   pred(KeysToMove) places of the left child, the last key of all comes
   from/goes to the parent}
  with ChildLeftHdr^ do begin
    {move the reference numbers}
    OffsetL := ffc_BlockHeaderSizeIndex +
               (bhiKeyCount * SizeOfRef);
    OffsetR := ffc_BlockHeaderSizeIndex;
    OffsetP := ffc_BlockHeaderSizeIndex +
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
    if not bhiKeysAreRefs then begin
      {move the keys}
      OffsetL := ffc_BlockHeaderSizeIndex +
                 (bhiMaxKeyCount * SizeOfRef) +
                 (bhiKeyCount * bhiKeyLength);
      OffsetR := ffc_BlockHeaderSizeIndex +
                 (bhiMaxKeyCount * SizeOfRef);
      OffsetP := ffc_BlockHeaderSizeIndex +
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
    end;
  end;
  {Update the key counts}
  inc(ChildLeftHdr^.bhiKeyCount, KeysToMove);
  dec(ChildRightHdr^.bhiKeyCount, KeysToMove);
end;
{--------}
procedure RotateLeftNode(aParentPage : PffBlock;
                         aSeparator  : Longint;
                         aChildLeft  : PffBlock;
                         aChildRight : PffBlock);
  {-Rotate keys from right node child to left node child through key in
    parent given by separator index. Equalise number of keys}
var
  ParentPageHdr : PffBlockHeaderIndex absolute aParentPage;
  ChildLeftHdr  : PffBlockHeaderIndex absolute aChildLeft;
  ChildRightHdr : PffBlockHeaderIndex absolute aChildRight;
  KeysToMove    : Longint;
  OffsetL       : Longint;
  OffsetR       : Longint;
  OffsetP       : Longint;
  BytesToMove   : Longint;
begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.RotateLeftNode);
  {$ENDIF}
  {calculate the number of keys to move, this means that the right child
   will *lose* this number of keys and the left child will *gain* this
   number}
  KeysToMove := (ChildRightHdr^.bhiKeyCount - ChildLeftHdr^.bhiKeyCount) div 2;
  if (KeysToMove = 0) then
    inc(KeysToMove);
  {move the first pred(KeysToMove) keys from the right child to the last
   pred(KeysToMove) places of the left child, the last key of all comes
   from/goes to the parent}
  with ChildLeftHdr^ do begin
    {move the page numbers}
    OffsetL := ffc_BlockHeaderSizeIndex +
               (bhiKeyCount * SizeOfPageNum);
    OffsetR := ffc_BlockHeaderSizeIndex;
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
    OffsetL := ffc_BlockHeaderSizeIndex +
               ((bhiMaxKeyCount * SizeOfPageNum) + (bhiKeyCount * SizeOfRef));
    OffsetR := ffc_BlockHeaderSizeIndex +
               (bhiMaxKeyCount * SizeOfPageNum);
    OffsetP := ffc_BlockHeaderSizeIndex +
               (ParentPageHdr^.bhiMaxKeyCount * SizeOfPageNum)  + (aSeparator * SizeOfRef);
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
    if not bhiKeysAreRefs then begin
      {move the keys}
      OffsetL := ffc_BlockHeaderSizeIndex +
                 (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef) +
                 (bhiKeyCount * bhiKeyLength));
      OffsetR := ffc_BlockHeaderSizeIndex +
                 (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef));
      OffsetP := ffc_BlockHeaderSizeIndex +
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
    end;
  end;
  {Update the key counts}
  inc(ChildLeftHdr^.bhiKeyCount, KeysToMove);
  dec(ChildRightHdr^.bhiKeyCount, KeysToMove);
end;
{--------}
procedure RotateRightLeaf(aParentPage : PffBlock;
                          aSeparator  : Longint;
                          aChildLeft  : PffBlock;
                          aChildRight : PffBlock);
  {-Rotate keys from left leaf child to right leaf child through key in
    parent given by separator index. Equalise number of keys}
var
  ParentPageHdr : PffBlockHeaderIndex absolute aParentPage;
  ChildLeftHdr  : PffBlockHeaderIndex absolute aChildLeft;
  ChildRightHdr : PffBlockHeaderIndex absolute aChildRight;
  KeysToMove    : Longint;
  OffsetL       : Longint;
  OffsetR       : Longint;
  OffsetP       : Longint;
  BytesToMove   : Longint;
begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.RotateRightLeaf);
  {$ENDIF}
  {calculate the number of keys to move, this means that the left child
   will *lose* this number of keys and the right child will *gain* this
   number}
  KeysToMove := (ChildLeftHdr^.bhiKeyCount - ChildRightHdr^.bhiKeyCount) div 2;
  if (KeysToMove = 0) then
    inc(KeysToMove);
  {open up enough room in the right child for these keys, and move
   the last pred(KeysToMove) keys from the left child to the first
   pred(KeysToMove) places, the last key of all comes from/goes to
   the parent}
  with ChildRightHdr^ do begin
    {move the reference numbers}
    OffsetR := ffc_BlockHeaderSizeIndex;
    OffsetL := ffc_BlockHeaderSizeIndex +
               (ChildLeftHdr^.bhiKeyCount - KeysToMove) * SizeOfRef;
    OffsetP := ffc_BlockHeaderSizeIndex +
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
    if not bhiKeysAreRefs then begin
      {move the keys}
      OffsetR := ffc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfRef);
      OffsetL := ffc_BlockHeaderSizeIndex +
                 (bhiMaxKeyCount * SizeOfRef) +
                 (ChildLeftHdr^.bhiKeyCount - KeysToMove) * bhiKeyLength;
      OffsetP := ffc_BlockHeaderSizeIndex +
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
    end;
  end;
  {Update the key counts}
  dec(ChildLeftHdr^.bhiKeyCount, KeysToMove);
  inc(ChildRightHdr^.bhiKeyCount, KeysToMove);
end;
{--------}
procedure RotateRightNode(aParentPage : PffBlock;
                          aSeparator  : Longint;
                          aChildLeft  : PffBlock;
                          aChildRight : PffBlock);
  {-Rotate keys from left node child to right node child through key in
    parent given by separator index. Equalise number of keys}
var
  ParentPageHdr : PffBlockHeaderIndex absolute aParentPage;
  ChildLeftHdr  : PffBlockHeaderIndex absolute aChildLeft;
  ChildRightHdr : PffBlockHeaderIndex absolute aChildRight;
  KeysToMove    : Longint;
  OffsetL       : Longint;
  OffsetR       : Longint;
  OffsetP       : Longint;
  BytesToMove   : Longint;
begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.RotateRightNode);
  {$ENDIF}
  {calculate the number of keys to move, this means that the left child
   will *lose* this number of keys and the right child will *gain* this
   number}
  KeysToMove := (ChildLeftHdr^.bhiKeyCount - ChildRightHdr^.bhiKeyCount) div 2;
  if (KeysToMove = 0) then
    inc(KeysToMove);
  {open up enough room in the right child for these keys, and move
   the last pred(KeysToMove) keys from the left child to the first
   pred(KeysToMove) places, the last key of all comes from/goes to
   the parent}
  with ChildRightHdr^ do begin
    {move the page numbers}
    OffsetR := ffc_BlockHeaderSizeIndex;
    OffsetL := ffc_BlockHeaderSizeIndex +
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
    OffsetR := ffc_BlockHeaderSizeIndex +
               (bhiMaxKeyCount * SizeOfPageNum);
    OffsetL := ffc_BlockHeaderSizeIndex +
               (bhiMaxKeyCount * SizeOfPageNum) +
               (ChildLeftHdr^.bhiKeyCount - KeysToMove) * SizeOfRef;
    OffsetP := ffc_BlockHeaderSizeIndex +
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
    if not bhiKeysAreRefs then begin
      {move the keys}
      OffsetR := ffc_BlockHeaderSizeIndex +
                 (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef));
      OffsetL := ffc_BlockHeaderSizeIndex +
                 (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef)) +
                 (ChildLeftHdr^.bhiKeyCount - KeysToMove) * bhiKeyLength;
      OffsetP := ffc_BlockHeaderSizeIndex +
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
    end;
  end;
  {Update the key counts}
  dec(ChildLeftHdr^.bhiKeyCount, KeysToMove);
  inc(ChildRightHdr^.bhiKeyCount, KeysToMove);
end;
{====================================================================}


{===Key insertion into/deletion from/swapping pages==================}
procedure InsertKeyInLeafPage(aLeaf    : PffBlock;
                              aElement : Longint;
                              aKey     : PffByteArray;
                        const aRefNr   : TffInt64);
var
  LeafHeader: PffBlockHeaderIndex absolute aLeaf;
  RefBlock  : PRefBlock;
  KeyBlock  : PffByteArray;
  Offset    : integer;
begin
  {Assumptions: aLeaf has been marked dirty}
  with LeafHeader^ do begin
    {get the address of the reference block}
    RefBlock := PRefBlock(@aLeaf^[ffc_BlockHeaderSizeIndex]);
    {open up room to insert the new reference}
    Move(RefBlock^[aElement], RefBlock^[succ(aElement)],
         SizeOfRef * (bhiKeyCount - aElement));
    {insert the new reference}
    RefBlock^[aElement] := aRefNr;
    {if keys are separate entities, insert into key block}
    if not LeafHeader^.bhiKeysAreRefs then begin
      {get the address of the keyblock}
      KeyBlock :=
         PffByteArray(@aLeaf^[ffc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfRef)]);
      {open up room to insert the new key}
      Offset := aElement * bhiKeyLength;
      Move(KeyBlock^[Offset], KeyBlock^[Offset + bhiKeyLength],
           bhiKeyLength * (bhiKeyCount - aElement));
      {insert the new key}
      Move(aKey^, KeyBlock^[Offset], bhiKeyLength);
    end;
    {increment the number of keys}
    inc(bhiKeyCount);
  end;
end;
{--------}
procedure InsertKeyInNodePage(aNode    : PffBlock;
                              aElement : Longint;
                              aKey     : PffByteArray;
                        const aRefNr   : TffInt64;
                              aChild   : TffWord32);
var
  NodeHeader: PffBlockHeaderIndex absolute aNode;
  PageBlock : PPageNumBlock;
  RefBlock  : PRefBlock;
  KeyBlock  : PffByteArray;
  Offset    : integer;
begin
  {Assumptions: aNode has been marked dirty}
  with NodeHeader^ do begin
    {get the address of the page number block}
    PageBlock := PPageNumBlock(@aNode^[ffc_BlockHeaderSizeIndex]);
    {open up room to insert the new reference}
    Move(PageBlock^[aElement], PageBlock^[succ(aElement)],
         SizeOfPageNum * (bhiKeyCount - aElement));
    {insert the new page number}
    PageBlock^[aElement] := aChild;
    {get the address of the data reference block}
    RefBlock :=
      PRefBlock(@aNode^[ffc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum)]);
    {open up room to insert the new reference}
    Move(RefBlock^[aElement], RefBlock^[succ(aElement)],
         SizeOfRef * (bhiKeyCount - aElement));
    {insert the new reference}
    RefBlock^[aElement] := aRefNr;
    {if keys are separate entities, insert into key block}
    if not bhiKeysAreRefs then begin
      {get the address of the keyblock}
      KeyBlock :=
         PffByteArray(@aNode^[ffc_BlockHeaderSizeIndex +
                              (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
      {open up room to insert the new key}
      Offset := aElement * bhiKeyLength;
      Move(KeyBlock^[Offset], KeyBlock^[Offset + bhiKeyLength],
           bhiKeyLength * (bhiKeyCount - aElement));
      {insert the new key}
      Move(aKey^, KeyBlock^[Offset], bhiKeyLength);
    end;
    {increment the number of keys}
    inc(bhiKeyCount);
  end;
end;
{--------}
procedure RemoveKeyFromLeafPage(aLeaf    : PffBlock;
                                aElement : Longint);
var
  LeafHeader: PffBlockHeaderIndex absolute aLeaf;
  RefBlock  : PRefBlock;
  KeyBlock  : PffByteArray;
  Offset    : integer;
begin
  { Assumption: We have Exclusively locked aLeaf. }
  with LeafHeader^ do begin
    {decrement the key count}
    dec(bhiKeyCount);
    {get the address of the data reference block}
    RefBlock := PRefBlock(@aLeaf^[ffc_BlockHeaderSizeIndex]);
    {close up to delete the reference}
    Move(RefBlock^[succ(aElement)], RefBlock^[aElement],
         SizeOfRef * (bhiKeyCount - aElement));
    {if keys are separate entities, delete from key block}
    if not LeafHeader^.bhiKeysAreRefs then begin
      {get the address of the key block}
      KeyBlock :=
         PffByteArray(@aLeaf^[ffc_BlockHeaderSizeIndex +
                                (bhiMaxKeyCount * SizeOfRef)]);
      {close up to delete the key}
      Offset := aElement * bhiKeyLength;
      Move(KeyBlock^[Offset+bhiKeyLength], KeyBlock^[Offset],
           bhiKeyLength * (bhiKeyCount - aElement));
    end;
  end;
end;
{--------}
procedure RemoveKeyFromNodePage(aNode    : PffBlock;
                                aElement : Longint);
var
  NodeHeader: PffBlockHeaderIndex absolute aNode;
  PageBlock : PPageNumBlock;
  RefBlock  : PRefBlock;
  KeyBlock  : PffByteArray;
  Offset    : integer;
begin
  {Assumptions: aNode has been marked dirty}
  with NodeHeader^ do begin
    {decrement the key count}
    dec(bhiKeyCount);
    {get the address of the page number block}
    PageBlock := PPageNumBlock(@aNode^[ffc_BlockHeaderSizeIndex]);
    {close up to delete the page number}
    Move(PageBlock^[succ(aElement)], PageBlock^[aElement],
         SizeOfPageNum * (bhiKeyCount - aElement));
    {get the address of the data reference block}
    RefBlock :=
       PRefBlock(@aNode^[ffc_BlockHeaderSizeIndex +
                         (bhiMaxKeyCount * SizeOfPageNum)]);
    {close up to delete the reference}
    Move(RefBlock^[succ(aElement)], RefBlock^[aElement],
         SizeOfRef * (bhiKeyCount - aElement));
    {if keys are separate entities, delete from key block}
    if not bhiKeysAreRefs then begin
      {get the address of the key block}
      KeyBlock :=
         PffByteArray(@aNode^[ffc_BlockHeaderSizeIndex +
                            (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
      {close up to delete the key}
      Offset := aElement * bhiKeyLength;
      Move(KeyBlock^[Offset+bhiKeyLength], KeyBlock^[Offset],
           bhiKeyLength * (bhiKeyCount - aElement));
    end;
  end;
end;
{--------}
procedure SwapKeys(aNode     : PffBlock;
                   aNElement : Longint;
                   aLeaf     : PffBlock;
                   aLElement : Longint;
                   aKey      : PffByteArray);
  {-Swap the key at aNElement in aNode with that at aLElement in aLeaf}
var
  NodeHdr : PffBlockHeaderIndex absolute aNode;
  LeafHdr : PffBlockHeaderIndex absolute aLeaf;
  OffsetN : Longint;
  OffsetL : Longint;
  Temp    : TffInt64;
begin
  {Assumptions: aNode, aLeaf have been marked dirty; the key at
                aNElement in aNode compares equal to aKey}
  with NodeHdr^ do begin
    {swap references}
    OffsetN := ffc_BlockHeaderSizeIndex +
               (bhiMaxKeyCount * SizeOfPageNum) +
               (aNElement * SizeOfRef);
    OffsetL := ffc_BlockHeaderSizeIndex +
               (aLElement * SizeOfRef);
    Temp := PRef(@aNode^[OffsetN])^;
    PRef(@aNode^[OffsetN])^ := PRef(@aLeaf^[OffsetL])^;
    PRef(@aLeaf^[OffsetL])^ := Temp;
    {if keys are separate entities, swap keys}
    if not bhiKeysAreRefs then begin
      OffsetN := ffc_BlockHeaderSizeIndex +
                 (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef)) +
                 (aNElement * NodeHdr^.bhiKeyLength);
      OffsetL := ffc_BlockHeaderSizeIndex +
                 (LeafHdr^.bhiMaxKeyCount * SizeOfRef) +
                 (aLElement * LeafHdr^.bhiKeyLength);
      Move(aLeaf^[OffsetL], aNode^[OffsetN], bhiKeyLength);
      Move(aKey^, aLeaf^[OffsetL], bhiKeyLength);
    end;
  end;
end;
{====================================================================}


{===Page splitting/merging routines==================================}
procedure MergeChildren(const aIndexData  : TffKeyIndexData;
                              aTI         : PffTransInfo;
                              aParentPage : PffBlock;
                              aSeparator  : Longint;
                              aChildLeft  : PffBlock;
                              aChildRight : PffBlock);
  {-Merge the right child into the left child, separated by the given
    key from the parent. The right child is deleted; the parent loses
    one key.}
var
  ParentPageHdr : PffBlockHeaderIndex absolute aParentPage;
  ChildLeftHdr  : PffBlockHeaderIndex absolute aChildLeft;
  ChildRightHdr : PffBlockHeaderIndex absolute aChildRight;
  OffsetL : Longint;
  OffsetR : Longint;
  OffsetP : Longint;
begin
  {Assumptions: all relevant pages have been marked dirty}
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.Merge);
  {$ENDIF}
  with ChildLeftHdr^ do begin
    {Note: this routine is *only* called if both children have
     (bhiMaxKeyCount div 2) keys--the absolute minimum}
    if (bhiKeyCount <> (bhiMaxKeyCount div 2)) or
       (bhiKeyCount <> ChildRightHdr^.bhiKeyCount) then
      FFRaiseException(EffServerException, ffStrResServer, fferrBadMergeCall,
                       [aIndexData.kidFI^.fiName^, bhiThisBlock, ChildRightHdr^.bhiThisBlock]);
    {the merge process is different for nodes and leaves}
    if (not bhiIsLeafPage) then begin
      {copy over the page numbers}
      OffsetR := ffc_BlockHeaderSizeIndex;
      OffsetL := ffc_BlockHeaderSizeIndex + (bhiKeyCount * SizeOfPageNum);
      Move(aChildRight^[OffsetR - SizeOfPageNum],
           aChildLeft^[OffsetL],
           (succ(bhiKeyCount) * SizeOfPageNum));
      {set up offsets for data references}
      OffsetL := ffc_BlockHeaderSizeIndex +
                 ((bhiMaxKeyCount * SizeOfPageNum) +  (bhiKeyCount * SizeOfRef));
      OffsetR := ffc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum);
    end
    else {it's a leaf} begin
      {set up offsets for data references}
      OffsetL := ffc_BlockHeaderSizeIndex +
                 (bhiKeyCount * SizeOfRef);
      OffsetR := ffc_BlockHeaderSizeIndex;
    end;
    {copy over parent data reference}
    OffsetP := ffc_BlockHeaderSizeIndex +
               (ParentPageHdr^.bhiMaxKeyCount * SizeOfPageNum) +
               (aSeparator * SizeOfRef);
    PRef(@aChildLeft^[OffsetL])^ := PRef(@aParentPage^[OffsetP])^;
    {copy over other data references}
    Move(aChildRight^[OffsetR],
         aChildLeft^[OffsetL + SizeOfRef],
         (bhiKeyCount * SizeOfRef));
    {if keys are separate entities, move the keys}
    if not bhiKeysAreRefs then begin
      {set up offsets for keys}
      inc(OffsetL, ((bhiMaxKeyCount-bhiKeyCount) * SizeOfRef) +
                    (bhiKeyCount * bhiKeyLength));
      inc(OffsetR, (bhiMaxKeyCount * SizeOfRef));
      OffsetP := ffc_BlockHeaderSizeIndex +
                 (ParentPageHdr^.bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef)) +
                 (aSeparator * bhiKeyLength);
      {copy over the parent key}
      Move(aParentPage^[OffsetP], aChildLeft^[OffsetL], bhiKeyLength);
      {copy over all the other keys}
      Move(aChildRight^[OffsetR],
           aChildLeft^[OffsetL + bhiKeyLength],
           (bhiKeyCount * bhiKeyLength));
    end;
    {delete the parent key since it now points to an invalid page}
    RemoveKeyFromNodePage(aParentPage, aSeparator);

    {patch up the left child's key count}
    bhiKeyCount := bhiMaxKeyCount;
  end;
  {delete the right child, it is no longer referenced}
  with aIndexData do begin
    FFTblHlpDeleteBlock(kidFI, kidFileHeader, aChildRight);
    dec(kidIndexHeader^.bihIndexPageCount[kidIndex]);
  end;
end;
{--------}
procedure BtreeSplitChild(const aIndexData  : TffKeyIndexData;
                                aTI         : PffTransInfo;
                                aParentPage : PffBlock;
                                aChildIndex : integer;
                                aChildPage  : PffBlock);
  {-Split the given child into two children. If the number of keys in
    the child is 2N+1, each child will end up with N keys, the parent
    gains one key at aChildIndex.}
var
  aParentPageHdr  : PffBlockHeaderIndex absolute aParentPage;
  aChildPageHdr   : PffBlockHeaderIndex absolute aChildPage;
  NewChildPage    : PffBlock;
  NewChildPageHdr : PffBlockHeaderIndex absolute NewChildPage;
  NewChild        : Longint;
  NewOffset       : integer;
  OldOffset       : integer;
  MedianRef       : TRef;
  aRelMethod      : TffReleaseMethod;
begin
  { Assumptions: aParentPage and aChildPage have been marked dirty. }
  {$IFDEF FF_DEBUG}
  inc(FFDEBUG_IndexCounter.Splits);
  {$ENDIF}
  aRelMethod := nil;
  with aChildPageHdr^ do begin
    {create a new child page}
    with aIndexData do
      NewChildPage :=
         GetNewInxBtreeBlock(kidFI, aTI, kidIndexHeader, kidIndex,
                             bhiIsLeafPage, aRelMethod);
    try
      NewChild := NewChildPageHdr^.bhiThisBlock;
      NewChildPageHdr^.bhiNodeLevel := bhiNodeLevel;
      {transfer the second half of the old child to the first half of the new one}
      {note this depends on whether the page is an internal node or a leaf}
      if (not bhiIsLeafPage) then begin
        {move the page numbers}
        {note: we must transfer into the prev page number field of the header}
        NewOffset := ffc_BlockHeaderSizeIndex - SizeOfPageNum;
        OldOffset := NewOffset + (succ(bhiMaxKeyCount div 2) * SizeOfPageNum);
        Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
             succ(bhiMaxKeyCount div 2) * SizeOfPageNum);
        {move the data references}
        NewOffset := ffc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum) ;
        OldOffset := NewOffset + (succ(bhiMaxKeyCount div 2) * SizeOfRef);
        Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
             (bhiMaxKeyCount div 2) * SizeOfRef);
        MedianRef := PRef(@aChildPage^[OldOffset-SizeOfRef])^;
        {if keys are separate entities, move the keys}
        if not bhiKeysAreRefs then begin
          NewOffset := ffc_BlockHeaderSizeIndex +
                       (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef));
          OldOffset := NewOffset + (succ(bhiMaxKeyCount div 2) * bhiKeyLength);
          Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
               (bhiMaxKeyCount div 2) * bhiKeyLength);
        end;
      end
      else {it's a leaf} begin
        {move the data references}
        NewOffset := ffc_BlockHeaderSizeIndex;
        OldOffset := NewOffset + (succ(bhiMaxKeyCount div 2) * SizeOfRef);
        Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
             (bhiMaxKeyCount div 2) * SizeOfRef);
        MedianRef := PRef(@aChildPage^[OldOffset-SizeOfRef])^;
        {if keys are separate entities, move the keys}
        if not bhiKeysAreRefs then begin
          NewOffset := ffc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfRef);
          OldOffset := NewOffset + (succ(bhiMaxKeyCount div 2) * bhiKeyLength);
          Move(aChildPage^[OldOffset], NewChildPage^[NewOffset],
               (bhiMaxKeyCount div 2) * bhiKeyLength);
        end;
      end;
      {insert the median key into the parent}
      InsertKeyInNodePage(aParentPage, aChildIndex,
                          PffByteArray(@aChildPage^[OldOffset-bhiKeyLength]),
                          MedianRef, NewChild);
      {set the number of keys in each child}
      bhiKeyCount := bhiMaxKeyCount div 2;
      NewChildPageHdr^.bhiKeyCount := bhiMaxKeyCount div 2;
    finally
      aRelMethod(NewChildPage);
    end;
  end;
end;
{====================================================================}


{===Key insertion helper routines====================================}
function BtreeInsertRedistribute(const aIndexData  : TffKeyIndexData;
                                       aTI         : PffTransInfo;
                                       aParentPage : PffBlock;
                                       aChildIndex : integer;
                                       aChildPage  : PffBlock) : boolean;
var
  aParentPageHdr : PffBlockHeaderIndex absolute aParentPage;
  SiblingPage    : PffBlock;
  SiblingPageHdr : PffBlockHeaderIndex absolute SiblingPage;
  Sibling        : Longint;
  PageBlock      : PPageNumBlock;
  aRelMethod     : TffReleaseMethod;
begin
  { Assumption: aParentPage and aChildPage have been marked dirty. }
  Result := false;

  { Try the child's successor sibling page. }
  if (aChildIndex < aParentPageHdr^.bhiKeyCount) then begin
    PageBlock := PPageNumBlock(@aParentPage^[ffc_BlockHeaderSizeIndex]);
    Sibling := PageBlock^[aChildIndex];
    with aIndexData do
      SiblingPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_MarkDirty,
                                     ffc_InxBlockTypeBtreePage, Sibling,
                                     aRelMethod);

    try
      { Are there at least two spare key slots? }
      if (SiblingPageHdr^.bhiKeyCount < pred(SiblingPageHdr^.bhiMaxKeyCount)) then begin
        { Yes. Redistribute the keys. }
        if (not SiblingPageHdr^.bhiIsLeafPage) then
             RotateRightNode(aParentPage, aChildIndex, aChildPage, SiblingPage)
        else RotateRightLeaf(aParentPage, aChildIndex, aChildPage, SiblingPage);
        Result := true;
      end;
    finally
      aRelMethod(SiblingPage);
    end;
  end;

  { If not done it yet, try the child's predecessor sibling page. }
  if (not Result) and (aChildIndex > 0) then begin
    if (aChildIndex = 1) then
      Sibling := aParentPageHdr^.bhiPrevPageRef
    else begin
      PageBlock := PPageNumBlock(@aParentPage^[ffc_BlockHeaderSizeIndex]);
      Sibling := PageBlock^[aChildIndex - 2];
    end;
    with aIndexData do
      SiblingPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_MarkDirty,
                                     ffc_InxBlockTypeBtreePage, Sibling,
                                     aRelMethod);
    try
      { Are there at least two spare key slots? }
      if (SiblingPageHdr^.bhiKeyCount < pred(SiblingPageHdr^.bhiMaxKeyCount)) then begin
        { Yes. Redistribute the keys. }
        if (not SiblingPageHdr^.bhiIsLeafPage) then
             RotateLeftNode(aParentPage, aChildIndex-1, SiblingPage, aChildPage)
        else RotateLeftLeaf(aParentPage, aChildIndex-1, SiblingPage, aChildPage);
        Result := true;
      end;
    finally
      aRelMethod(SiblingPage);
    end;
  end;
end;
{--------}
function BtreeInsertNonFull(const aIndexData  : TffKeyIndexData;
                                  aTI         : PffTransInfo;
                              var aPage       : PffBlock;
                              var aRelMethod  : TffReleaseMethod;
                                  aKey        : PffByteArray;
                            const aRefNr      : TffInt64) : boolean;
var
  PageHdr      : PffBlockHeaderIndex absolute aPage;
  PageNumBlock : PPageNumBlock;
  DataRefBlock : PRefBlock;
  KeyBlock     : PffByteArray;
  L, R, M      : integer;
  CompResult   : integer;
  Child        : Longint;
  ChildPage    : PffBlock;
  ChildPageHdr : PffBlockHeaderIndex absolute ChildPage;
  Compare      : TffKeyCompareFunc;
  AllowDups    : boolean;
  DoneRecursing: boolean;
  aChildRelMethod : TffReleaseMethod;
begin
  { Assumptions: aPage could be dirty or clean.  Caller has incremented
      aPage's ref count. }
  Result := false;
  { Learn whether dup keys are allowed, get compare function. }
  with aIndexData do begin
    AllowDups := (kidIndexHeader^.bihIndexFlags[kidIndex] and
                  ffc_InxFlagAllowDups) <> 0;
    Compare := kidCompare;
  end;
  {simulate recursion (ie unwind it}
  DoneRecursing := false;
  repeat
    with PageHdr^ do begin
      {get the addresses of the reference block and key string block,
       this is different for leaf and node pages}
      if bhiIsLeafPage then begin
        PageNumBlock := nil;
        DataRefBlock := PRefBlock(@aPage^[ffc_BlockHeaderSizeIndex]);
        if bhiKeysAreRefs then
          KeyBlock := PffByteArray(DataRefBlock)
        else
          KeyBlock :=
             PffByteArray(@aPage^[ffc_BlockHeaderSizeIndex +
                                (bhiMaxKeyCount * SizeOfRef)]);
      end
      else {its a node page} begin
        PageNumBlock := PPageNumBlock(@aPage^[ffc_BlockHeaderSizeIndex]);
        DataRefBlock :=
           PRefBlock(@aPage^[ffc_BlockHeaderSizeIndex +
                             (bhiMaxKeyCount*SizeOfPageNum)]);
        if bhiKeysAreRefs then
          KeyBlock := PffByteArray(DataRefBlock)
        else
          KeyBlock :=
             PffByteArray(@aPage^[ffc_BlockHeaderSizeIndex +
                                (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
      end;
      {binary search to find insertion point}
      L := 0;
      R := pred(bhiKeyCount);
      repeat
        M := (L + R) div 2;
        CompResult := Compare(aKey^, KeyBlock^[M * bhiKeyLength],
                              aIndexData.kidCompareData);
        if (CompResult < 0) then
          R := pred(M)
        else if (CompResult > 0) then
          L := succ(M)
        else {CompResult = 0}
          if AllowDups then begin
            CompResult := FFCmpI64(aRefNr, DataRefBlock^[M]);
            if (CompResult < 0) then
              R := pred(M)
            else if (CompResult > 0) then
              L := succ(M)
            else {it's a duplicate key+refnr combo}
              Exit;
          end
          else {it's a duplicate key}
            Exit;
      until (L > R);
      if bhiIsLeafPage then begin
        { It's a leaf page.  Mark it as dirty since we are about to modify it. }
        FFBMDirtyBlock(aIndexData.kidFI, bhiThisBlock, aTI, aPage);

        { The key+refnr combo doesn't exist, insert at L. }
        InsertKeyInLeafPage(aPage, L, aKey, aRefNr);
        Result := true;
        DoneRecursing := true;
      end
      else {it's a node page} begin
        { The child we need to traverse to is given by (L - 1). }
        if (L = 0) then Child := bhiPrevPageRef
        else            Child := PageNumBlock^[pred(L)];
        { Read the page.  For now, we need a Share lock. }
        with aIndexData do
          ChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                       ffc_InxBlockTypeBtreePage, Child,
                                       aChildRelMethod);
        { If this child is full, split it or redistribute now. }
        with ChildPageHdr^ do
          if (bhiKeyCount = bhiMaxKeyCount) then begin
            { Splitting a child/redistribution will update the parent
              page as well, so mark both pages as dirty. }
            FFBMDirtyBlock(aIndexData.kidFI, PageHdr^.bhiThisBlock, aTI, aPage);
            FFBMDirtyBlock(aIndexData.kidFI, Child, aTI, ChildPage);

            { Try redistribution else split the child. }
            if not BtreeInsertRedistribute(aIndexData, aTI, aPage, L, ChildPage) then
              BtreeSplitChild(aIndexData, aTI, aPage, L, ChildPage);
            aChildRelMethod(ChildPage);
            { We've just rearranged the keys in this page, recurse this page. }
          end
          else begin
            { Insert the key into the child's subtree, ie recurse with child. }
            aRelMethod(aPage);
            aRelMethod := aChildRelMethod;
            aPage := ChildPage;
          end;
      end;
    end;
  until DoneRecursing;
end;
{--------}
function BtreeInsert(const aIndexData : TffKeyIndexData;
                           aTI        : PffTransInfo;
                           aRoot      : TffWord32;
                           aKey       : PffByteArray;
                     const aRefNr     : TffInt64) : boolean;
var
  RootPage       : PffBlock;
  RootPageHdr    : PffBlockHeaderIndex absolute RootPage;
  NewRootPage    : PffBlock;
  NewRootPageHdr : PffBlockHeaderIndex absolute NewRootPage;
  aNewRelMethod,
  aRelMethod     : TffReleaseMethod;
begin
  { Get the root page. }
  with aIndexData do
    RootPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_MarkDirty,
                                ffc_InxBlockTypeBtreePage, aRoot,
                                aRelMethod);
  { If the root is full, we split it now. }
  with RootPageHdr^ do
    try
      if (bhiKeyCount = bhiMaxKeyCount) then begin
        { Since we're about to update it, mark the block as dirty. }
        FFBMDirtyBlock(aIndexData.kidFI, aRoot, aTI, RootPage);

        { Create a new root. }
        with aIndexData do
          NewRootPage :=
             GetNewInxBtreeBlock(kidFI, aTI, kidIndexHeader, kidIndex, false,
                                 aNewRelMethod);
        try
          NewRootPageHdr^.bhiNodeLevel := succ(bhiNodeLevel);

          { Patch it so that the previous page is the old root. }
          NewRootPageHdr^.bhiPrevPageRef := aRoot;

          { Split the old root. }
          BtreeSplitChild(aIndexData, aTI, NewRootPage, 0, RootPage);

          { Update the index header to point to the new root. }
          with aIndexData do
            kidIndexHeader^.bihIndexRoot[kidIndex] := NewRootPageHdr^.bhiThisBlock;
          {now insert the key into the tree starting at the new root}
          Result :=
            BtreeInsertNonFull(aIndexData, aTI, NewRootPage, aNewRelMethod,
                               aKey, aRefNr);
        finally
          aNewRelMethod(NewRootPage);
        end;

      end
      else {insert the key into the tree starting at the root}
        Result :=
          BtreeInsertNonFull(aIndexData, aTI, RootPage, aRelMethod, aKey,
                             aRefNr);
    finally
      aRelMethod(RootPage);
    end;
end;
{====================================================================}


{===Key deletion helper routines=====================================}
procedure BtreeDeleteIndexPage(const aIndexData : TffKeyIndexData;
                                     aTI        : PffTransInfo;
                                     aParent    : TffWord32);
var
  ParentPage    : PffBlock;
  ParentPageHdr : PffBlockHeaderIndex absolute ParentPage;
  PageBlock     : PPageNumBlock;
  Child         : integer;
  aRelMethod    : TffReleaseMethod;
begin
  {WARNING: this is a recursive routine with an absolute maximum of 32
            levels of recursion}
  {read the parent index page, mark dirty}
  with aIndexData do
    ParentPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_MarkDirty,
                                  ffc_InxBlockTypeBtreePage, aParent,
                                  aRelMethod);
  try
    {get the page number block}
    PageBlock := PPageNumBlock(@ParentPage^[ffc_BlockHeaderSizeIndex]);
    with ParentPageHdr^ do
      {if there are children recurse through them all}
      if (bhiNodeLevel > 1) then begin
        BtreeDeleteIndexPage(aIndexData, aTI, bhiPrevPageRef);
        for Child := 0 to pred(bhiKeyCount) do
          BtreeDeleteIndexPage(aIndexData, aTI, PageBlock^[Child]);
      end;
    {delete this page}
    with aIndexData do begin
      FFTblHlpDeleteBlock(kidFI, kidFileHeader, ParentPage);
    end;
  finally
    aRelMethod(ParentPage);
  end;
end;
{--------}
function BtreeDeleteSwapKey(const aIndexData  : TffKeyIndexData;
                                  aTI         : PffTransInfo;
                                  aParentPage : PffBlock;
                                  aKeyIndex   : integer;
                                  aKey        : PffByteArray;
                              var aRelMethod  : TffReleaseMethod) : PffBlock;
var
  ParentPageHdr  : PffBlockHeaderIndex absolute aParentPage;
  ChildPage      : PffBlock;
  ChildPageHdr   : PffBlockHeaderIndex absolute ChildPage;
  Child          : TffWord32;
  LChildPage     : PffBlock;
  LChildPageHdr  : PffBlockHeaderIndex absolute LChildPage;
  LChild         : TffWord32;
  RChildPage     : PffBlock;
  RChildPageHdr  : PffBlockHeaderIndex absolute RChildPage;
  RChild         : TffWord32;
  ResultPageNum  : TffWord32;                                          
  MergeThem      : boolean;
  aChildRelMeth,
  aLCRelMethod,
  aRCRelMethod   : TffReleaseMethod;
begin
  {Assumptions: aParentPage has already been marked dirty.
                The key at aKeyIndex in the parent equals aKey}
  { Assume that we shall have to do a merge. }
  LChild := 0;
  Result := nil;
  MergeThem := true;
  with ParentPageHdr^ do begin
    {we shall first search for the successor key}
    RChild :=
       PPageNum(@aParentPage^[ffc_BlockHeaderSizeIndex + (aKeyIndex * SizeOfPageNum)])^;
    with aIndexData do
      RChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                    ffc_InxBlockTypeBtreePage, RChild,
                                    aRCRelMethod);
    { Does this child page have enough keys?. }
    if (RChildPageHdr^.bhiKeyCount > (RChildPageHdr^.bhiMaxKeyCount div 2)) then begin
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
      while (not ChildPageHdr^.bhiIsLeafPage) do begin
        Child := ChildPageHdr^.bhiPrevPageRef;
        if ChildPage <> Result then                                    {!!.01}
          aChildRelMeth(ChildPage);                                    {!!.01}
        with aIndexData do
          ChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                       ffc_InxBlockTypeBtreePage, Child,
                                       aChildRelMeth);
      end;
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
      if Child = ResultPageNum then
        { Yes. Make sure we return the child page's modified block to the
          calling method. }
        Result := ChildPage
      else
        { No. Release the child page that we modified. }
        aChildRelMeth(ChildPage);
{End !!.01}
      MergeThem := false;
    end;

    { If we couldn't use the successor, try the predecessor. }
    if MergeThem then begin
      {find the left child}
      if (aKeyIndex = 0) then
        LChild := bhiPrevPageRef
      else
        LChild := PPageNum(@aParentPage^[ffc_BlockHeaderSizeIndex +
                                     ((aKeyIndex-1) * SizeOfPageNum)])^;
      with aIndexData do
        LChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                      ffc_InxBlockTypeBtreePage, LChild,
                                      aLCRelMethod);
      { Does this child page have enough keys? }
      if (LChildPageHdr^.bhiKeyCount > (LChildPageHdr^.bhiMaxKeyCount div 2)) then begin
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
        while (not ChildPageHdr^.bhiIsLeafPage) do begin
          Child :=
             PPageNum(@ChildPage^[ffc_BlockHeaderSizeIndex +
                              ((ChildPageHdr^.bhiKeyCount-1) * SizeOfPageNum)])^;
          if ChildPage <> LChildPage then                              {!!.01}
            aChildRelMeth(ChildPage);                                  {!!.01}
          with aIndexData do
            ChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                         ffc_InxBlockTypeBtreePage, Child,
                                         aChildRelMeth);
        end;
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
      if Child = ResultPageNum then
        { Yes. Make sure we return the child page's modified block to the
          calling method. }
        Result := ChildPage
      else
        { No. Release the child page that we modified. }
        aChildRelMeth(ChildPage);
{End !!.01}
        MergeThem := false;
      end;
    end;

    {if we've failed to find the predecessor/successor, merge the two children}
    if MergeThem then
      begin
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
      end;
  end;
  {***Delphi32***: the compiler tags the return value of this function
                   as being "possibly undefined". Not true.}
end;
{--------}
procedure BtreeDeleteRedistributeOrMerge(const aIndexData  : TffKeyIndexData;
                                               aTI         : PffTransInfo;
                                               aParentPage : PffBlock;
                                               aChildIndex : integer;
                                               aChildPage  : PffBlock);
var
  aParentPageHdr : PffBlockHeaderIndex absolute aParentPage;
  SiblingPage    : PffBlock;
  SiblingPageHdr : PffBlockHeaderIndex absolute SiblingPage;
  Sibling        : Longint;
  PageBlock      : PPageNumBlock;
  DoneIt         : boolean;
  IsRightSibling : boolean;
  aRelList       : TffPointerList;
  aRelMethod     : TffReleaseMethod;
begin
  {Assumptions: aParentPage and aChildPage have both been marked dirty.
                aChildPage has the minimum number of keys. }
  aRelList := TffPointerList.Create;
  Sibling := 0;
  IsRightSibling := false;
  SiblingPage := nil;
  try
    {assume we shall fail all the way}
    DoneIt := false;
    {read the child's successor sibling page}
    if (aChildIndex < aParentPageHdr^.bhiKeyCount) then begin
      PageBlock := PPageNumBlock(@aParentPage^[ffc_BlockHeaderSizeIndex]);
      Sibling := PageBlock^[aChildIndex];
      IsRightSibling := true;
      with aIndexData do
        SiblingPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                       ffc_InxBlockTypeBtreePage, Sibling,
                                       aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(SiblingPage, TffInt64(aRelMethod)));
      {check for at least one spare key}
      if (SiblingPageHdr^.bhiKeyCount > (SiblingPageHdr^.bhiMaxKeyCount div 2)) then begin
        { Mark the sibling as dirty. }
        FFBMDirtyBlock(aIndexData.kidFI, Sibling, aTI, SiblingPage);

        { Redistribute the keys. }
        if (not SiblingPageHdr^.bhiIsLeafPage) then
             RotateLeftNode(aParentPage, aChildIndex, aChildPage, SiblingPage)
        else RotateLeftLeaf(aParentPage, aChildIndex, aChildPage, SiblingPage);
        DoneIt := true;
      end;
    end;

    { Read the child's predecessor sibling page. }
    if (not DoneIt) and (aChildIndex > 0) then begin
      if (aChildIndex = 1) then
        Sibling := aParentPageHdr^.bhiPrevPageRef
      else begin
        PageBlock := PPageNumBlock(@aParentPage^[ffc_BlockHeaderSizeIndex]);
        Sibling := PageBlock^[aChildIndex-2];
      end;
      IsRightSibling := false;
      with aIndexData do
        SiblingPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                       ffc_InxBlockTypeBtreePage, Sibling,
                                       aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(SiblingPage, TffInt64(aRelMethod)));
      { Check for at least one spare key. }
      if (SiblingPageHdr^.bhiKeyCount > (SiblingPageHdr^.bhiMaxKeyCount div 2)) then begin
        { Obtain an Exclusive lock on the sibling. }
        FFBMDirtyBlock(aIndexData.kidFI, Sibling, aTI, SiblingPage);

        { Redistribute the keys. }
        if (not SiblingPageHdr^.bhiIsLeafPage) then
             RotateRightNode(aParentPage, aChildIndex-1, SiblingPage, aChildPage)
        else RotateRightLeaf(aParentPage, aChildIndex-1, SiblingPage, aChildPage);
        DoneIt := true;
      end;
    end;

    {***Delphi32***: The compiler tags both Sibling and IsRightSibling as
                     possibly being "used before definition". A corollary
                     of the definition of a B-Tree insists that every child
                     page has at least one sibling and so in practice both
                     variables will be set by this point.}
    if (not DoneIt) then begin
      { Mark the sibling as dirty. }
      FFBMDirtyBlock(aIndexData.kidFI, Sibling, aTI, SiblingPage);
      { Merge with our sibling. }
      if IsRightSibling then
           MergeChildren(aIndexData, aTI, aParentPage, aChildIndex, aChildPage,
                         SiblingPage)
      else MergeChildren(aIndexData, aTI, aParentPage, aChildIndex-1,
                         SiblingPage, aChildPage)
    end;
  finally
    for Sibling := 0 to pred(aRelList.Count) do
      FFDeallocReleaseInfo(aRelList[Sibling]);
    aRelList.Free;
  end;
end;
{--------}
function BtreeDeleteAmplePage(const aIndexData    : TffKeyIndexData;
                                    aTI           : PffTransInfo;
                                    aPage         : PffBlock;
                                    aKey          : PffByteArray;
                              const aRefNr        : TffInt64;
                                var aBTreeChanged : Boolean)           {!!.05}
                                                  : Boolean;
  {-Routine to delete a key from a page; only called for
    pages that have succ(minimum keys) present, or for the root}
var
  Page         : PffBlock;
  PageHdr      : PffBlockHeaderIndex absolute Page;
  PageNumBlock : PPageNumBlock;
  DataRefBlock : PRefBlock;
  KeyBlock     : PffByteArray;
  L, R, M      : integer;
  CompResult   : integer;
  Child        : Longint;
  ChildPage    : PffBlock;
  ChildPageHdr : PffBlockHeaderIndex absolute ChildPage;
  Compare      : TffKeyCompareFunc;
  AllowDups    : boolean;
  KeyFound     : boolean;
  DoneRecursing: boolean;
  aRelMethod   : TffReleaseMethod;
  aRelList     : TffPointerList;
begin
  {$IFDEF DefeatWarnings}
  M := 0;
  Result := false;
  {$ENDIF}

  Page := aPage;
  aRelMethod := nil;

  { We use the following list to track the RAM pages we've accessed and
    the release method associated with each RAM page. At the end of this
    routine, we will call the release method for each RAM page. }
  aRelList := TffPointerList.Create;

  try
    { Assumption: Page has not been marked dirty. }
    { Learn whether dup keys are allowed, get compare function. }
    with aIndexData do begin
      AllowDups := (kidIndexHeader^.bihIndexFlags[kidIndex] and
                    ffc_InxFlagAllowDups) <> 0;
      Compare := kidCompare;
    end;
    {simulate recursion (ie unwind it)}
    DoneRecursing := false;
    repeat
      with PageHdr^ do begin
        {get the addresses of the reference block and key string block}
        if bhiIsLeafPage then begin
          PageNumBlock := nil;
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock :=
              PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                  (bhiMaxKeyCount * SizeOfRef)]);
        end
        else begin
          PageNumBlock :=
            PPageNumBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          DataRefBlock :=
            PRefBlock(@Page^[ffc_BlockHeaderSizeIndex +
                             (bhiMaxKeyCount * SizeOfPageNum)]);
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock :=
              PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                   (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
        end;
        {binary search to find out if key is present}
        L := 0;
        R := pred(bhiKeyCount);
        KeyFound := false;
        {note: it is possible for this routine to be called for an empty root}
        if (R >= 0) then
          repeat
            M := (L + R) div 2;
            CompResult := Compare(aKey^, KeyBlock^[M * bhiKeyLength],
                                  aIndexData.kidCompareData);
            if (CompResult < 0) then
              R := pred(M)
            else if (CompResult > 0) then
              L := succ(M)
            else {CompResult = 0}
              if AllowDups then begin
                CompResult := FFCmpI64(aRefNr, DataRefBlock^[M]);
                if (CompResult < 0) then
                  R := pred(M)
                else if (CompResult > 0) then
                  L := succ(M)
                else {key+refnr have been found}
                  begin
                    KeyFound := true;
                    Break;{out of the repeat..until loop}
                  end
              end
              else {key has been found} begin
                KeyFound := true;
                Break;{out of the repeat..until loop}
              end
          until (L > R);
        {if the page is a leaf...}
        if bhiIsLeafPage then begin
          {if the key was found delete it from the page, return true}
          if KeyFound then begin
            { Mark the block as dirty. }
            FFBMDirtyBlock(aIndexData.kidFI, bhiThisBlock, aTI, Page);
            {***Delphi32***: the compiler flags M as "possibly used
                             before definition". Not true.}
            RemoveKeyFromLeafPage(Page, M);
            Result := true;
          end
          {otherwise return false}
          else
            Result := false;
          DoneRecursing := true;
        end
        { Otherwise the page is an internal node... }
        else
          { If the key was found in the node... }
          if KeyFound then begin
            {we need to swap this key with its predecessor/successor
             (this is guaranteed to be on a leaf) then delete the key
             in the leaf}
            { Mark the block as dirty. }
            FFBMDirtyBlock(aIndexData.kidFI, bhiThisBlock, aTI, Page);
            { Swap the key with a key on a leaf, or merge children,
              then recursively delete from returned child. }
            Page := BtreeDeleteSwapKey(aIndexData, aTI, Page, M, aKey, aRelMethod);
            aBtreeChanged := True;                                     {!!.05}
            aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
          end
          {otherwise the key was not found...}
          else begin
            {the key, if anywhere, is in the child subtree at L-1}
            if (L = 0) then
                 Child := bhiPrevPageRef
            else Child := PageNumBlock^[pred(L)];
            {read the child's page}
            with aIndexData do
              ChildPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                           ffc_InxBlockTypeBtreePage, Child,
                                           aRelMethod);
            aRelList.Append(FFAllocReleaseInfo(ChildPage, TffInt64(aRelMethod)));
            {check whether the child has enough keys, if so recurse}
            if (ChildPageHdr^.bhiKeyCount > (ChildPageHdr^.bhiMaxKeyCount div 2)) then
              Page := ChildPage
            {otherwise try and make it full enough}
            else {not enough keys in child} begin
              { Mark this page and the child as dirty. }
              FFBMDirtyBlock(aIndexData.kidFI, bhiThisBlock, aTI, Page);
              FFBMDirtyBlock(aIndexData.kidFI, Child, aTI, ChildPage);

              { Redistribute the keys among siblings, or merge. }
              BtreeDeleteRedistributeOrMerge(aIndexData, aTI,
                                             Page, L, ChildPage);
              aBTreeChanged := True;                                   {!!.05}
              {recurse ourselves}
              {Note: it could be that we now have only the minimum number
                     of keys, but it doesn't matter since we'll immediately
                     recurse into one of our children}
            end;
          end;
      end;
    until DoneRecursing;
    {***Delphi32***: the compiler tags the return value of this function
                     as being "possibly undefined". Not true, DoneRecursing
                     is only set true once Result has been set true/false}
  finally
    for Child := 0 to pred(aRelList.Count) do
      FFDeallocReleaseInfo(aRelList[Child]);
    aRelList.Free;
  end;
end;
{--------}
function BtreeDelete(const aRoot         : Longint;
                     const aKey          : PffByteArray;
                     const aTI           : PffTransInfo;
                     const aIndexData    : TffKeyIndexData;
                     const aRefNr        : TffInt64;
                       var aBTreeChanged : Boolean)                    {!!.05}
                                         : Boolean;
var
  RootPage, RootPageClone : PffBlock;
  RootPageHdr : PffBlockHeaderIndex absolute RootPage;
  RootPageCloneHdr : PffBlockHeaderIndex absolute RootPageClone;
  aCloneRelMethod,
  aRelMethod  : TffReleaseMethod;
begin
  { Obtain the root page. }
  with aIndexData do
    RootPage := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                ffc_InxBlockTypeBtreePage, aRoot,
                                aRelMethod);
  try
    { Delete the key from this page. }
    Result := BtreeDeleteAmplePage(aIndexData, aTI, RootPage, aKey,
                                   aRefNr, aBTreeChanged);             {!!.05}

    { If the root page is empty then replace it with its first child &
      delete the root page. }
    if Result then begin
      { Get the root page as it may have been modified. We may be looking at
        the read-only block right now and we need the modified block. }
      RootPageClone := FFBMGetBlock(aIndexData.kidFI, aTI, aRoot, ffc_ReadOnly,
                                    aCloneRelMethod);
      if (RootPageCloneHdr^.bhiKeyCount = 0) then
        { Assumption: The root page has been exclusively locked somewhere
                      in the delete process. }
        with aIndexData do begin
          kidIndexHeader^.bihIndexRoot[kidIndex] := RootPageCloneHdr^.bhiPrevPageRef;
          FFTblHlpDeleteBlock(kidFI, kidFileHeader, RootPageClone);
          dec(kidIndexHeader^.bihIndexPageCount[kidIndex]);
        end;
      aCloneRelMethod(RootPageClone);
    end;
  finally
    aRelMethod(RootPage);
  end;
end;
{====================================================================}


{===Key reading helper routines======================================}
function BtreeExistsKey(const aIndexData : TffKeyIndexData;
                              aTI        : PffTransInfo;
                              aRoot      : TffWord32;
                              aKey       : PffByteArray;
                              aRefNr     : TffInt64) : boolean;
var
  Page         : PffBlock;
  PageHdr      : PffBlockHeaderIndex absolute Page;
  PageNumBlock : PPageNumBlock;
  DataRefBlock : PRefBlock;
  KeyBlock     : PffByteArray;
  L, R, M      : integer;
  CompResult   : integer;
  Child        : TffWord32;
  Compare      : TffKeyCompareFunc;
  CheckDups    : boolean;
  KeyFound     : boolean;
  DoneRecursing: boolean;
  aRelMethod   : TffReleaseMethod;
begin
  {$IFDEF DefeatWarnings}
  Result := false;
  {$ENDIF}
  {get the root page}
  with aIndexData do
    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                            ffc_InxBlockTypeBtreePage, aRoot, aRelMethod);
  try
    {set up the invariants}
    with aIndexData do begin
      CheckDups := ((kidIndexHeader^.bihIndexFlags[kidIndex] and
                     ffc_InxFlagAllowDups) <> 0) and (aRefNr.iLow <> 0) and
                     (aRefNr.iHigh <> 0);
      Compare := kidCompare;
    end;
    {simulate recursion (ie unwind it)}
    DoneRecursing := false;
    repeat
      with PageHdr^ do begin
        {get the addresses of the reference block and key string block}
        if bhiIsLeafPage then begin
          PageNumBlock := nil;
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock :=
              PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                   (bhiMaxKeyCount * SizeOfRef)]);
        end
        else begin
          PageNumBlock :=
            PPageNumBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          DataRefBlock :=
            PRefBlock(@Page^[ffc_BlockHeaderSizeIndex + (bhiMaxKeyCount*SizeOfPageNum)]);
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock :=
              PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                   (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
        end;
        {binary search to find out if key is present}
        L := 0;
        R := pred(bhiKeyCount);
        KeyFound := false;
        repeat
          M := (L + R) div 2;
          CompResult := Compare(aKey^, KeyBlock^[M * bhiKeyLength], aIndexData.kidCompareData);
          if (CompResult < 0) then
            R := pred(M)
          else if (CompResult > 0) then
            L := succ(M)
          else {CompResult = 0}
            if CheckDups then begin
              CompResult := FFCmpI64(aRefNr, DataRefBlock^[M]);
              if (CompResult < 0) then
                R := pred(M)
              else if (CompResult > 0) then
                L := succ(M)
              else {key+refnr have been found}
                begin
                  KeyFound := true;
                  Break;{out of the repeat..until loop}
                end
            end
            else {key has been found} begin
              KeyFound := true;
              Break;{out of the repeat..until loop}
            end;
        until (L > R);
        if KeyFound then begin
          Result := true;
          DoneRecursing := true;
        end
        else
          {if the page is a leaf...}
          if bhiIsLeafPage then begin
            {the key was not found at all}
            Result := false;
            DoneRecursing := true;
          end
          {otherwise the page is an internal node...}
          else begin
            {the key, if anywhere, is in the child subtree at L-1}
            if (L = 0) then
                 Child := bhiPrevPageRef
            else Child := PageNumBlock^[pred(L)];
            {read the child's page}
            aRelMethod(Page);
            with aIndexData do begin
              Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                      ffc_InxBlockTypeBtreePage, Child,
                                      aRelMethod);
            end;

            {and recurse it}
          end;
      end;
    until DoneRecursing;

    {***Delphi32***: the compiler tags the return value of this function
                     as being "possibly undefined". Not true, DoneRecursing
                     is only set true once Result has been set true/false}
  finally
    aRelMethod(Page);
  end;
end;
{--------}
function BtreeNextKey(const aIndexData : TffKeyIndexData;
                            aTI        : PffTransInfo;
                            aKey       : PffByteArray;
                        var aRefNr     : TffInt64;
                        var aKeyPath   : TffKeyPath) : boolean;
var
  aInx         : Longint;
  Page         : PffBlock;
  PageHdr      : PffBlockHeaderIndex absolute Page;
  PageNumBlock : PPageNumBlock;
  DataRefBlock : PRefBlock;
  KeyBlock     : PffByteArray;
  PageNum      : TffWord32;
  aRelList     : TffPointerList;
  aRelMethod   : TffReleaseMethod;
begin
  { Assumption: the btree has at least one key. }
  aRelList := TffPointerList.Create;
  try
    with aKeyPath do begin
      {patch the path for BOF}
      if (kpPos = kppBOF) then begin
        kpPath[0].kpePage := aIndexData.kidIndexHeader^.bihIndexRoot[aIndexData.kidIndex];
        kpPath[0].kpeItem := -1;
        kpCount := 1;
      end;
      {get the last page on the key path}
      with aIndexData do
        Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                ffc_InxBlockTypeBtreePage,
                                kpPath[pred(kpCount)].kpePage, aRelMethod);
      aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
      {if we're on a crack, just return the key pointed to by the path}
      if (kpPos = kppOnCrackBefore) and                                {!!.03 - Start}
         (kpPath[pred(kpCount)].kpeItem <= pred(PageHdr^.bhiKeyCount)) then {!!.03 - End}
        with kpPath[pred(kpCount)], PageHdr^ do begin
          if bhiIsLeafPage then
            DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex])
          else
            DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex +
                                             (bhiMaxKeyCount * SizeOfPageNum)]);
          aRefNr := DataRefBlock^[kpeItem];
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            if bhiIsLeafPage then
              KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                              (bhiMaxKeyCount*SizeOfRef)])
            else
              KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                              (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
          Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
          Result := true;
          Exit;
        end;
      {if the current page is a node, we need to travel down the btree,
       going left all the time, until we hit a leaf}
      if not PageHdr^.bhiIsLeafPage then begin
        {read the first child}
        PageNumBlock := PPageNumBlock(@Page^[ffc_BlockHeaderSizeIndex]);
        if (kpPath[pred(kpCount)].kpeItem = -1) then
             PageNum := PageHdr^.bhiPrevPageRef
        else PageNum := PageNumBlock^[kpPath[pred(kpCount)].kpeItem];
        with aIndexData do begin
          Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeBtreePage, PageNum,
                                  aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
        end;
        while not PageHdr^.bhiIsLeafPage do begin
          with kpPath[kpCount] do begin
            kpePage := PageHdr^.bhiThisBlock;
            kpeItem := -1;
          end;
          inc(kpCount);
          with aIndexData do begin
            Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                    ffc_InxBlockTypeBtreePage,
                                    PageHdr^.bhiPrevPageRef, aRelMethod);
            aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
          end;
        end;
        with kpPath[kpCount], PageHdr^ do begin
          kpePage := PageHdr^.bhiThisBlock;
          kpeItem := 0;
          inc(kpCount);
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          aRefNr := DataRefBlock^[0];
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                             (bhiMaxKeyCount * SizeOfRef)]);
          Move(KeyBlock^[0], aKey^, bhiKeyLength);
        end;
        Result := true;
      end
      {otherwise the current page is a leaf}
      {if the current item is not the final key, just return the next}
      else if (kpPath[pred(kpCount)].kpeItem < pred(PageHdr^.bhiKeyCount)) then begin
        with kpPath[pred(kpCount)], PageHdr^ do begin
          inc(kpeItem);
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          aRefNr := DataRefBlock^[kpeItem];
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                             (bhiMaxKeyCount * SizeOfRef)]);
          Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
          Result := true;
        end;
      end
      {otherwise the current item is the last key on the page, we need to
       travel up the btree returning along the path, until we get to a node
       where the current item is less than the number of keys; if we can't
       find one, return false--there is no next key}
      else begin
        {read the first parent, assume we won't find a next key}
        dec(kpCount);
        Result := false;
        {while there are still items in the key path}
        while (kpCount > 0) do begin
          {read the current page}
          with aIndexData do
            Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                    ffc_InxBlockTypeBtreePage,
                                    kpPath[pred(kpCount)].kpePage,
                                    aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
          {if the current item is not the final key, just return the next}
          if (kpPath[pred(kpCount)].kpeItem < pred(PageHdr^.bhiKeyCount)) then begin
            with kpPath[pred(kpCount)], PageHdr^ do begin
              inc(kpeItem);
              DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex +
                                               (bhiMaxKeyCount * SizeOfPageNum)]);
              aRefNr := DataRefBlock^[kpeItem];
              if bhiKeysAreRefs then
                KeyBlock := PffByteArray(DataRefBlock)
              else
                KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                                 (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
              Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
            end;
            Result := true;
            Break;{out of dowhile loop}
          end;
          {otherwise go back one step}
          dec(kpCount);
        end;
      end;  { if }
    end;  { with }
  finally
    for aInx := 0 to pred(aRelList.Count) do
      FFDeallocReleaseInfo(aRelList[aInx]);
    aRelList.Free;
  end;
end;
{--------}
function BtreePrevKey(const aIndexData : TffKeyIndexData;
                            aTI        : PffTransInfo;
                            aKey       : PffByteArray;
                        var aRefNr     : TffInt64;
                        var aKeyPath   : TffKeyPath) : boolean;
var
  aInx         : Longint;
  Page         : PffBlock;
  PageHdr      : PffBlockHeaderIndex absolute Page;
  PageNumBlock : PPageNumBlock;
  DataRefBlock : PRefBlock;
  KeyBlock     : PffByteArray;
  PageNum      : TffWord32;
  aRelList     : TffPointerList;
  aRelMethod   : TffReleaseMethod;
begin
  {Assumption: the btree has at least one key
               if the path is pointing to EOF the root page can be
               found at aKeyPath.kpPath[0].kpePage}
  aRelList := TffPointerList.Create;
  try
    with aKeyPath do begin
      {if the keypath points to EOF, then read the root page and set
       the item number of the first path element to the count of keys
       ready for the walk down the btree}
      if (kpPos = kppEOF) then begin
        with kpPath[0], aIndexData do begin
          kpePage := kidIndexHeader^.bihIndexRoot[kidIndex];
          Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeBtreePage, kpePage,
                                  aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
          kpeItem := PageHdr^.bhiKeyCount;
        end;
        kpCount := 1;
      end
      else begin
        {get the last page on the key path}
        with aIndexData do
          Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeBtreePage,
                                  kpPath[pred(kpCount)].kpePage, aRelMethod);
          aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
        {if we're on a crack, just return the key pointed to by the path}
        if (kpPos = kppOnCrackAfter) then
          with kpPath[pred(kpCount)], PageHdr^ do begin
            if bhiIsLeafPage then
              DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex])
            else
              DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex +
                                               (bhiMaxKeyCount * SizeOfPageNum)]);
            aRefNr := DataRefBlock^[kpeItem];
            if bhiKeysAreRefs then
              KeyBlock := PffByteArray(DataRefBlock)
            else
              if bhiIsLeafPage then
                KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                                (bhiMaxKeyCount * SizeOfRef)])
              else
                KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                                (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
            Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
            Result := true;
            Exit;
          end;  { with }
      end;  { if }

      {if the current page is a node, we need to travel down the btree,
       going right all the time after the first child, until we hit a leaf}
      if not PageHdr^.bhiIsLeafPage then begin
        {read the first (ie previous) child}
        dec(kpPath[pred(kpCount)].kpeItem);
        PageNumBlock := PPageNumBlock(@Page^[ffc_BlockHeaderSizeIndex]);
        if (kpPath[pred(kpCount)].kpeItem < 0) then
             PageNum := PageHdr^.bhiPrevPageRef
        else PageNum := PageNumBlock^[kpPath[pred(kpCount)].kpeItem];
        with aIndexData do
          Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeBtreePage, PageNum,
                                  aRelMethod);
        aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
        while not PageHdr^.bhiIsLeafPage do begin
          with kpPath[kpCount], PageHdr^ do begin
            kpePage := bhiThisBlock;
            kpeItem := pred(bhiKeyCount);
          end;
          inc(kpCount);
          PageNumBlock := PPageNumBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          with aIndexData do begin
            PageNum := PageNumBlock^[pred(PageHdr^.bhiKeyCount)];
            Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                    ffc_InxBlockTypeBtreePage,
                                    PageNum, aRelMethod);
            aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
          end;
        end;
        with kpPath[kpCount], PageHdr^ do begin
          kpePage := bhiThisBlock;
          kpeItem := pred(bhiKeyCount);
          inc(kpCount);
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          aRefNr := DataRefBlock^[kpeItem];
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                             (bhiMaxKeyCount * SizeOfRef)]);
          Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
        end;
        Result := true;
      end
      {otherwise the current page is a leaf}
      {if the current item is not the first key, just return the previous}
      else if (kpPath[pred(kpCount)].kpeItem > 0) then begin
        with kpPath[pred(kpCount)], PageHdr^ do begin
          dec(kpeItem);
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          aRefNr := DataRefBlock^[kpeItem];
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                            (bhiMaxKeyCount * SizeOfRef)]);
          Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
          Result := true;
        end;
      end
      {otherwise the current item is the first key on the page, we need to
       travel up the btree returning along the path, until we get to a node
       where the current item is not the first key on the page; if we can't
       find one, return false--there is no previous key}
      else begin
        {read the first parent, assume we won't find a previous key}
        dec(kpCount);
        Result := false;
        {while there are still items in the key path}
        while (kpCount > 0) do begin
          {read the current page}
          with aIndexData do begin
            PageNum := kpPath[pred(kpCount)].kpePage;
            Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                    ffc_InxBlockTypeBtreePage, PageNum,
                                    aRelMethod);
            aRelList.Append(FFAllocReleaseInfo(Page, TffInt64(aRelMethod)));
          end;
          {if the current item is not -1, just return it}
          if (kpPath[pred(kpCount)].kpeItem >= 0) then begin
            with kpPath[pred(kpCount)], PageHdr^ do begin
              DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex +
                                               (bhiMaxKeyCount * SizeOfPageNum)]);
              aRefNr := DataRefBlock^[kpeItem];
              if bhiKeysAreRefs then
                KeyBlock := PffByteArray(DataRefBlock)
              else
                KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                                 (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
              Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
            end;
            Result := true;
            Break;{out of dowhile loop}
          end;
          {otherwise go back one step}
          dec(kpCount);
        end;
      end;
    end;  { with }
  finally
    for aInx := 0 to pred(aRelList.Count) do
      FFDeallocReleaseInfo(aRelList[aInx]);
    aRelList.Free;
  end;
end;
{--------}
function BtreeFindKey(const aIndexData : TffKeyIndexData;
                            aTI        : PffTransInfo;
                            aRoot      : TffWord32;
                            aKey       : PffByteArray;
                        var aRefNr     : TffInt64;
                        var aKeyPath   : TffKeyPath;
                            aAction    : TffSearchKeyAction) : boolean;
var
  Page         : PffBlock;
  PageHdr      : PffBlockHeaderIndex absolute Page;
  PageNumBlock : PPageNumBlock;
  DataRefBlock : PRefBlock;
  KeyBlock     : PffByteArray;
  OurKey       : PffByteArray;
  KeyLen       : integer;
  L, R, M      : integer;
  KeyCompResult: integer;
  RefCompResult: integer;
  Child        : TffWord32;
  Compare      : TffKeyCompareFunc;
  CheckDups    : boolean;
  KeyFound     : boolean;
  DoneRecursing: boolean;
  HasDups      : boolean;
  aRelMethod   : TffReleaseMethod;
begin
  { Get the root page. }
  with aIndexData do
    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                            ffc_InxBlockTypeBtreePage, aRoot, aRelMethod);

  try
    { Set up the invariants. }
    with aIndexData do begin
      {HasDups means that there might be duplicate keys here}
      HasDups := (kidIndexHeader^.bihIndexFlags[kidIndex] and
                     ffc_InxFlagAllowDups) <> 0;
      {CheckDups means that we're trying to find an exact key/refnr
       combination}
      CheckDups := HasDups and ((aRefNr.iLow <> 0) or (aRefNr.iHigh <> 0));
      Compare := kidCompare;
    end;

    { Prepare the key path. }
    FFInitKeyPath(aKeyPath);
    {simulate recursion (ie unwind it)}
    DoneRecursing := false;
    repeat
      with PageHdr^, aKeyPath do begin
        {get the addresses of the reference block and key string block}
        if bhiIsLeafPage then begin
          PageNumBlock := nil;
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock :=
               PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                    (bhiMaxKeyCount * SizeOfRef)]);
        end
        else begin
          PageNumBlock :=
            PPageNumBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          DataRefBlock :=
            PRefBlock(@Page^[ffc_BlockHeaderSizeIndex + (bhiMaxKeyCount * SizeOfPageNum)]);
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock :=
               PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                    (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
        end;
        {binary search to find out if key is present}
        L := 0;
        R := pred(bhiKeyCount);
        KeyFound := false;
        repeat
          M := (L + R) div 2;
          KeyCompResult := Compare(aKey^, KeyBlock^[M * bhiKeyLength], aIndexData.kidCompareData);
          if (KeyCompResult < 0) then
            R := pred(M)
          else if (KeyCompResult > 0) then
            L := succ(M)
          else {KeyCompResult = 0}
            if CheckDups then begin
              RefCompResult := FFCmpI64(aRefNr, DataRefBlock^[M]);
              if (RefCompResult < 0) then
                R := pred(M)
              else if (RefCompResult > 0) then
                L := succ(M)
              else {key+refnr have been found} begin
                KeyFound := true;
                Break;{out of the repeat..until loop}
              end
            end
            else {key has been found} begin
              KeyFound := true;
              Break;{out of the repeat..until loop}
            end;
        until (L > R);
        {if the key/ref was found then save the final keypath element}
        if KeyFound then begin
          DoneRecursing := true;
          with kpPath[kpCount] do begin
            kpePage := bhiThisBlock;
            kpeItem := M;
          end;
          inc(kpCount);
          aRefNr := DataRefBlock^[M];
          Move(KeyBlock^[M * bhiKeyLength], aKey^, bhiKeyLength);
          kpPos := kppOnKey;
        end
        else
          {if the page is a leaf...}
          if bhiIsLeafPage then begin
            {if the index allows dups, the key has been matched and the
             passed refnr is zero, return the first refnr in the index
             for the key}
            if CheckDups and (KeyCompResult = 0) and
               (aRefNr.iLow = 0) and (aRefNr.iHigh = 0) then begin
              KeyFound := true;
              DoneRecursing := true;
              with kpPath[kpCount] do begin
                kpePage := bhiThisBlock;
                kpeItem := L;
              end;
              inc(kpCount);
              aRefNr := DataRefBlock^[L];
              kpPos := kppOnCrackBefore;
            end
            else begin
              {the key/ref was not present at all, patch the final
               keypath node according to the aAction parameter}
              DoneRecursing := true;
              with kpPath[kpCount] do begin
                kpePage := bhiThisBlock;
                case aAction of
                  skaEqual        : FFInitKeyPath(aKeyPath);
                  skaEqualCrack,
                  skaGreater,
                  skaGreaterEqual : begin
                                      if (L < bhiKeyCount) then begin
                                        kpeItem := L;
                                        kpPos := kppOnCrackBefore;
                                      end
                                      else begin
                                        kpeItem := pred(L);
                                        kpPos := kppOnCrackAfter;
                                      end;
                                    end;
                end;{case}
              end;
              inc(kpCount);
            end;
          end
          {otherwise the page is an internal node...}
          else begin
            {the key, if anywhere, is in the child subtree at L-1}
            with kpPath[kpCount] do begin
              kpePage := bhiThisBlock;
              kpeItem := pred(L);
            end;
            inc(kpCount);
            if (L = 0) then
                 Child := bhiPrevPageRef
            else Child := PageNumBlock^[pred(L)];
            {read the child's page}
            aRelMethod(Page);
            with aIndexData do begin
              { Crab down to child block and unlock parent block. }
              Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                      ffc_InxBlockTypeBtreePage, Child,
                                      aRelMethod);
            end;
            {and recurse it}
          end;
      end;
    until DoneRecursing;

    {if the key wasn't found...}
    if (not KeyFound) then begin
      { If we don't mind the missing key and can accept being positioned on
        a crack before the next key, our search is over. }
      if (aAction = skaEqualCrack) then
        KeyFound := true
      {if we can return the next greater key, do so; always return true
       (the caller will be patching the keypath)}
      else if (aAction <> skaEqual) then begin
        if BtreeNextKey(aIndexData, aTI, aKey, aRefNr, aKeyPath) then
          aKeyPath.kpPos := kppOnKey
        else
          {we hit the end of the index; this is OK, just set the keypath
           to EOF}
          FFSetKeyPathToEOF(aKeyPath);
        KeyFound := true;
      end;
    end
    {otherwise the key was found...}
    else {KeyFound is true} begin
      {if we actually wanted the next greater key, continue doing next
       key operations until the key returned compares unequal to the one
       we have, or we hit EOF; always return true}
      if (aAction = skaGreater) then begin
        KeyLen := aIndexData.kidCompareData^.cdKeyLen;
        FFGetMem(OurKey, KeyLen);
        try
          Move(aKey^, OurKey^, KeyLen);
          repeat
            KeyFound := BtreeNextKey(aIndexData, aTI, aKey, aRefNr, aKeyPath);
            if KeyFound then begin
              aKeyPath.kpPos := kppOnKey;
              KeyFound := Compare(aKey^, OurKey^, aIndexData.kidCompareData) = 0
            end
            else
              FFSetKeyPathToEOF(aKeyPath);
          until (not KeyFound);
        finally
          FFFreeMem(OurKey, KeyLen);
        end;
      end
      {otherwise we wanted an equal key}
      else {aAction <> skaGreater} begin
        {if we were making an exact full key match on an index with
         unique keys, we're done now; otherwise we have to position the
         keypath at the first of possibly many equal partial keys, or
         equal duplicate keys. Note that if the index has dup keys, but
         we've matched exactly on the refnr as well, then we've found
         the exact key}
        if (HasDups and not CheckDups) or
           (aIndexData.kidCompareData^.cdFldCnt <> 0) or
           (aIndexData.kidCompareData^.cdPartLen <> 0) then begin
          KeyLen := aIndexData.kidCompareData^.cdKeyLen;
          FFGetMem(OurKey, KeyLen);
          try
            Move(aKey^, OurKey^, KeyLen);
            repeat
              KeyFound := BtreePrevKey(aIndexData, aTI, aKey, aRefNr, aKeyPath);
              if KeyFound then
                KeyFound := Compare(aKey^, OurKey^, aIndexData.kidCompareData) = 0
              else
                FFSetKeyPathToBOF(aKeyPath);
            until (not KeyFound);
            BtreeNextKey(aIndexData, aTI, aKey, aRefNr, aKeyPath);
            aKeyPath.kpPos := kppOnKey;
          finally
            FFFreeMem(OurKey, KeyLen);
          end;
        end;
      end;
      {make sure that KeyFound is still true, we may have altered it}
      KeyFound := true;
    end;
    Result := KeyFound;
  finally
    aRelMethod(Page);
  end;
end;
{--------}
procedure BtreeFindApprox(const aIndexData : TffKeyIndexData;
                                aTI        : PffTransInfo;
                                aRoot      : TffWord32;
                                aKey       : PffByteArray;
                            var aRefNr     : TffInt64;
                            var aKeyPath   : TffKeyPath;
                                aPos       : integer);
var
  Page         : PffBlock;
  PageHdr      : PffBlockHeaderIndex absolute Page;
  PageNumBlock : PPageNumBlock;
  DataRefBlock : PRefBlock;
  KeyBlock     : PffByteArray;
  Child        : TffWord32;
  ChildPos     : integer;
  aRelMethod   : TffReleaseMethod;
begin
  {get the root page}
  with aIndexData do
    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                            ffc_InxBlockTypeBtreePage, aRoot, aRelMethod);

  try
    {if the root is a leaf, just do a simple calculation to find
     the key at the approx position}
    if PageHdr^.bhiIsLeafPage then
      with aKeyPath, PageHdr^ do begin
        kpCount := 1;
        with kpPath[0] do begin
          kpePage := bhiThisBlock;
          kpeItem := (aPos * bhiKeyCount) div 101;
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          aRefNr := DataRefBlock^[kpeItem];
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                             (bhiMaxKeyCount * SizeOfRef)]);
          Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
        end;
      end
    {otherwise the root is a node, ie has children}
    else with aKeyPath do begin
      {there will be two levels in the keypath}
      kpCount := 2;
      {set up the first entry in the keypath, calc values for the child}
      with PageHdr^, kpPath[0] do
        begin
          kpePage := bhiThisBlock;
          kpeItem := ((aPos * succ(bhiKeyCount)) div 101) - 1;
          PageNumBlock := PPageNumBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          if (kpeItem = -1) then
            Child := bhiPrevPageRef
          else
            Child := PageNumBlock^[kpeItem];
          ChildPos := ((aPos * 100) div (101 div succ(bhiKeyCount))) -
                      (succ(kpeItem) * 100);
        end;
      {get the child page}
      aRelMethod(Page);
      with aIndexData do
        Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                ffc_InxBlockTypeBtreePage, Child, aRelMethod);
      {set up the second entry in the keypath}
      with PageHdr^, kpPath[1] do begin
        kpePage := bhiThisBlock;
        kpeItem := ((ChildPos * bhiKeyCount) div 101);
        if bhiIsLeafPage then begin
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex]);
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                             (bhiMaxKeyCount*SizeOfRef)]);
        end
        else begin
          DataRefBlock := PRefBlock(@Page^[ffc_BlockHeaderSizeIndex +
                                           (bhiMaxKeyCount*SizeOfPageNum)]);
          if bhiKeysAreRefs then
            KeyBlock := PffByteArray(DataRefBlock)
          else
            KeyBlock := PffByteArray(@Page^[ffc_BlockHeaderSizeIndex +
                                             (bhiMaxKeyCount * (SizeOfPageNum + SizeOfRef))]);
        end;
        aRefNr := DataRefBlock^[kpeItem];
        Move(KeyBlock^[kpeItem * bhiKeyLength], aKey^, bhiKeyLength);
      end;
    end;
  finally
    aRelMethod(Page);
  end;
end;
{--------}
procedure BtreeCalcApprox(const aIndexData : TffKeyIndexData;
                                aTI        : PffTransInfo;
                                aRoot      : Longint;
                          const aKeyPath   : TffKeyPath;
                            var aPos       : integer);
var
  Page         : PffBlock;
  PageHdr      : PffBlockHeaderIndex absolute Page;
  RootKeyCount : integer;
  aRelMethod   : TffReleaseMethod;
begin
  {get the root page}
  with aIndexData do
    Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                            ffc_InxBlockTypeBtreePage, aRoot, aRelMethod);

  try
    {if the root is a leaf, just do a simple calculation to find
     approx position of the key}
    if PageHdr^.bhiIsLeafPage then
      with aKeyPath.kpPath[0], PageHdr^ do
        aPos := (kpeItem * 100) div bhiKeyCount
    {otherwise the root is a node, ie has children}
    else with aKeyPath do begin
      {there will be two levels to check in the keypath}
      RootKeyCount := PageHdr^.bhiKeyCount;
      {get the relevant child page}
      aRelMethod(Page);
      with aIndexData do
        Page := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                ffc_InxBlockTypeBtreePage, kpPath[1].kpePage,
                                aRelMethod);

      {calculate the position}
      if PageHdr^.bhiIsLeafPage then
        aPos := ((100 + (kpPath[1].kpeItem * 100) div PageHdr^.bhiKeyCount) *
                 succ(kpPath[0].kpeItem)) div
                succ(RootKeyCount)
      else
        aPos := ((100 + (succ(kpPath[1].kpeItem) * 100) div succ(PageHdr^.bhiKeyCount)) *
                 succ(kpPath[0].kpeItem)) div
                succ(RootKeyCount);
    end;
  finally
    aRelMethod(Page);
  end;
end;
{====================================================================}


{===Key access related routines======================================}
procedure FFTblDeleteAllKeys(aTI : PffTransInfo; var aIndex : TffKeyIndexData);
var
  InxBlock   : PffBlock;
  InxBlockHdr: PffBlockHeaderIndex absolute InxBlock;
  Root       : TffWord32;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  {Note: this routine can only be run in a 'subset' transaction.
         Essentially all the index pages are going to be changed: if
         they were all in a normal transaction, we could quite easily
         run out of memory trying to hold all the dirty pages in
         memory. A corollary is that FFTblDeleteAllKeys cannot be
         rolled back. Another is that if this crashes, the index file
         is pretty well hosed and should be rebuilt.}
  with aIndex do begin
    { Obtain a Share lock on the file header.  We need the file header
      but we don't need to modify it. }
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                                     ffc_ReadOnly, aRelMethod));

    try
      { Get an Exclusive lock on the index header. }
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_MarkDirty,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader,
                                  aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        {get the root page}
        Root := kidIndexHeader^.bihIndexRoot[kidIndex];
        {patch the index header}
        with kidIndexHeader^ do begin
          bihIndexRoot[kidIndex] := ffc_W32NoValue;
          bihIndexPageCount[kidIndex] := 0;
        end;
        {special case: if the root page does not exist, just return
         (ie there are no keys)}
        if (Root = ffc_W32NoValue) then
          Exit;
        { Otherwise go delete all the index pages. }
        BtreeDeleteIndexPage(aIndex, aTI, Root);
        { Allow the caller to do the final commit. }
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(PffBlock(kidFileHeader));
    end;
  end;  { with }
end;
{--------}
function FFTblDeleteKey(const aTI           : PffTransInfo;
                        const aKey          : PffByteArray;
                        const aRefNr        : TffInt64;
                          var aIndex        : TffKeyIndexData;
                          var aBTreeChanged : Boolean) : Boolean;      {!!.05}
var
  InxBlock   : PffBlock;
  InxBlockHdr: PffBlockHeaderIndex absolute InxBlock;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  with aIndex do begin
    { Mark the file header as dirty. }
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                        ffc_MarkDirty, aRelMethod));
    try
      { Obtain an Exclusive lock on the index header. }
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_MarkDirty,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader,
                                  aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        {special case: if the root page does not exist, return false}
        if (kidIndexHeader^.bihIndexRoot[kidIndex] = ffc_W32NoValue) then
          Result := false
        {otherwise go delete from the b-tree}
        else
          Result := BtreeDelete(kidIndexHeader^.bihIndexRoot[kidIndex],
                                aKey, aTI, aIndex, aRefNr,
                                aBTreeChanged);                        {!!.05}
        {decrement the number of keys}
        if Result then
          dec(kidIndexHeader^.bihIndexKeyCount[kidIndex]);
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(PffBlock(kidFileHeader));
    end;
  end;
end;
{--------}
function FFTblFindKey(var aIndex   : TffKeyIndexData;
                      var aRefNr   : TffInt64;
                          aTI      : PffTransInfo;
                          aKey     : PffByteArray;
                      var aKeyPath : TffKeyPath;
                          aAction  : TffSearchKeyAction) : boolean;
var
  InxBlock   : PffBlock;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  with aIndex do begin
    {get the file header, block 0}
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                                     ffc_ReadOnly, aRelMethod));
    try
      {get the index header}
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader, aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        {special case: if the root page does not exist}
        if (kidIndexHeader^.bihIndexRoot[kidIndex] = ffc_W32NoValue) then begin
          if (aAction = skaEqual) then
            Result := false
          else begin
            Result:= true;
            FFSetKeyPathToEOF(aKeyPath);
          end;
        end
        {otherwise go read the b-tree}
        else
          Result := BtreeFindKey(aIndex, aTI,
                                 kidIndexHeader^.bihIndexRoot[kidIndex],
                                 aKey, aRefNr,
                                 aKeyPath,
                                 aAction);
        aKeyPath.kpLSN := kidFI^.fiBufMgr.GetRAMPageLSN2               {!!.06}
                            (kidFI, kidFileHeader^.bhfIndexHeader);    {!!.06}
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(PffBlock(kidFileHeader));
    end;
  end;  { with }

  {if the key was not found, ensure the path is invalidated}
  if (not Result) and (aAction = skaEqual) then
    FFInitKeyPath(aKeyPath);
end;
{--------}
function FFTblGetApproxPos(var aIndex   : TffKeyIndexData;
                           var aPos     : integer;
                               aTI      : PffTransInfo;
                         const aKeyPath : TffKeyPath) : boolean;
var
  InxBlock   : PffBlock;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  with aIndex do begin
    {get the file header, block 0}
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                                     ffc_ReadOnly,
                                                     aRelMethod));
    try
      {get the index header}
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader, aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        {special case: if the root page does not exist, return false}
        if (kidIndexHeader^.bihIndexRoot[kidIndex] = ffc_W32NoValue) then
          Result := false
        {otherwise go read the b-tree}
        else begin
          Result := true;
          BtreeCalcApprox(aIndex, aTI, kidIndexHeader^.bihIndexRoot[kidIndex],
                          aKeyPath, aPos);
        end;
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(PffBlock(kidFileHeader));
    end;
  end;  { with }
end;
{--------}
function FFTblInsertKey(var aIndex : TffKeyIndexData;
                      const aRefNr : TffInt64;
                            aTI    : PffTransInfo;
                            aKey   : PffByteArray) : boolean;
var
  InxBlock,                                                            {!!.11}
  InxNewBlock   : PffBlock;                                            {!!.11}
  InxBlockHdr: PffBlockHeaderIndex;                                    {!!.11}
  aInxRelMeth,
  aInxRelMethNewBlock,                                                 {!!.11}
  aRelMethod : TffReleaseMethod;
begin
  with aIndex do begin
    { Obtain an Exclusive lock on the file header. }
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                                     ffc_MarkDirty, aRelMethod));
    try
      { Dirty the index header. }
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_MarkDirty,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader, aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        { Special case: if the root page does not yet exist, create a new one
          and add the key to it. }
        if (kidIndexHeader^.bihIndexRoot[kidIndex] = ffc_W32NoValue) then begin
{Begin !!.11}
//          aInxRelMeth(InxBlock);
          InxNewBlock := GetNewInxBtreeBlock(kidFI, aTI, kidIndexHeader,
                                             kidIndex, True,
                                             aInxRelMethNewBlock);
          try
            InxBlockHdr := PffBlockHeaderIndex(InxNewBlock);
            kidIndexHeader^.bihIndexRoot[kidIndex] := InxBlockHdr^.bhiThisBlock;
            InsertKeyInLeafPage(InxNewBlock, 0, aKey, aRefNr);
            Result := true;
          finally
            aInxRelMethNewBlock(InxNewBlock);                        
          end;
{End !!.11}
        end
        {otherwise insert the key in the relevant leaf page}
        else
          Result := BtreeInsert(aIndex, aTI,
                                kidIndexHeader^.bihIndexRoot[kidIndex],
                                aKey, aRefNr);
        {increment the number of keys if key was added}
        if Result then
          inc(kidIndexHeader^.bihIndexKeyCount[kidIndex]);
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(Pffblock(kidFileHeader));
    end;
  end; { with }
end;
{--------}
function FFTblKeyExists(var aIndex : TffKeyIndexData;
                      const aRefNr : TffInt64;
                            aTI    : PffTransInfo;
                            aKey   : PffByteArray ) : boolean;
var
  InxBlock   : PffBlock;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  { If the lock duration is ffldShort then this method will free the locks
    after it has finished searching for the key. }
  with aIndex do begin
    {get the file header, block 0}
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                                     ffc_ReadOnly, aRelMethod));
    try
      {get the index header}
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader, aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        {special case: if the root page does not exist, return false}
        if (kidIndexHeader^.bihIndexRoot[kidIndex] = ffc_W32NoValue) then
          Result := false
        {otherwise go read the b-tree}
        else
          Result := BtreeExistsKey(aIndex, aTI,
                                   kidIndexHeader^.bihIndexRoot[kidIndex],
                                   aKey, aRefNr);
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(PffBlock(kidFileHeader));
    end;
  end;  { with }
end;
{--------}
function FFTblNextKey(var aIndex   : TffKeyIndexData;
                      var aRefNr   : TffInt64;
                          aTI      : PffTransInfo;
                          aKey     : PffByteArray;
                      var aKeyPath : TffKeyPath) : boolean;
var
//  IndexMap : TffbmRAMPage;                                           {Deleted !!.06}
  IndexMapLSN : TffWord32;                                             {!!.06}
  InxBlock   : PffBlock;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  {if the keypath is valid and it's at EOF, there is no next key}
  if (aKeyPath.kpPos = kppEOF) then begin
    Result := false;
    Exit;
  end;
  {otherwise do some work}
  with aIndex do begin
    {get the file header, block 0}
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                                     ffc_ReadOnly, aRelMethod));
    try
      {get the index header}
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader, aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        {special case: if the root page does not exist, return false}
        if (kidIndexHeader^.bihIndexRoot[kidIndex] = ffc_W32NoValue) then
          Result := false
        {otherwise go read the b-tree}
        else begin
          IndexMapLSN := kidFI^.fiBufMgr.GetRAMPageLSN2                {!!.06}
                        (kidFI, kidFileHeader^.bhfIndexHeader);
          if (aKeyPath.kpPos = kppUnknown) then
            FFSetKeyPathToBOF(aKeyPath)
{Begin !!.06}
          else begin
            { Has the index map changed since our last visit? }
            if (((aKeyPath.kpLSN > 0) and
                 (IndexMapLSN > aKeyPath.kpLSN)) or                    {!!.06}
                (aKeyPath.kpPos = kppUnknown)) then begin              {!!.05}
              { Yes.  Reposition. }
              Result := BtreeFindKey(aIndex, aTI,
                                     KidIndexHeader^.bihIndexRoot[kidIndex],
                                     aKey, aRefNr, aKeyPath, skaEqualCrack);
              if not Result then
                Exit;
            end;
          end;
          Result := BtreeNextKey(aIndex, aTI, aKey, aRefNr, aKeyPath);
          aKeyPath.kpLSN := IndexMapLSN;                               {!!.06}
        end;
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(PffBlock(kidFileHeader));
    end;
  end;  { with }
  {if a key was found, ensure the path points to a key}
  if Result then
    aKeyPath.kpPos := kppOnKey
  {if no next key found, ensure the path points to EOF}
  else
    FFSetKeyPathToEOF(aKeyPath);
end;
{--------}
function FFTblPrevKey(var aIndex   : TffKeyIndexData;
                      var aRefNr   : TffInt64;
                          aTI      : PffTransInfo;
                          aKey     : PffByteArray;
                      var aKeyPath : TffKeyPath) : boolean;
var
//  IndexMap : TffbmRAMPage;                                           {Deleted !!.06}
  IndexMapLSN : TffWord32;                                             {!!.06}
  InxBlock    : PffBlock;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  {if the keypath is valid and it's at BOF, there is no prev key}
  if (aKeyPath.kpPos = kppBOF) then begin
    Result := false;
    Exit;
  end;
  {otherwise do some work}
  with aIndex do begin
    {get the file header, block 0}
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                                     ffc_ReadOnly, aRelMethod));
    try
      {get the index header}
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader, aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        {special case: if the root page does not exist, return false}
        if (kidIndexHeader^.bihIndexRoot[kidIndex] = ffc_W32NoValue) then
          Result := false
        {otherwise go read the b-tree}
        else begin
          IndexMapLSN := kidFI^.fiBufMgr.GetRAMPageLSN2                {!!.06}
                          (kidFI, kidFileHeader^.bhfIndexHeader);      {!!.06}
          if (aKeyPath.kpPos = kppUnknown) then
            FFSetKeyPathToEOF(aKeyPath);
            { Has the index map changed since our last visit? }
            if (((aKeyPath.kpLSN > 0) and
                 (IndexMapLSN > aKeyPath.kpLSN)) or                    {!!.06}
                (aKeyPath.kpPos = kppUnknown)) then begin              {!!.05}
              { Yes.  Reposition. }
              Result := BtreeFindKey(aIndex, aTI,
                                     KidIndexHeader^.bihIndexRoot[kidIndex],
                                     aKey, aRefNr, aKeyPath, skaEqualCrack);
              if not Result then
                Exit;
            end;
          Result := BtreePrevKey(aIndex, aTI, aKey, aRefNr, aKeyPath);
          aKeyPath.kpLSN := IndexMapLSN;                               {!!.06}
        end;
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(PffBlock(kidFileHeader));
    end;
  end;  { with }

  {if a key was found, ensure the path points to a key}
  if Result then
    aKeyPath.kpPos := kppOnKey
  {if no previous key found, ensure the path points to BOF}
  else
    FFSetKeyPathToBOF(aKeyPath);
end;
{--------}
function FFTblSetApproxPos(var aIndex   : TffKeyIndexData;
                               aPos     : integer;
                           var aRefNr   : TffInt64;
                               aTI      : PffTransInfo;
                               aKey     : PffByteArray;
                           var aKeyPath : TffKeyPath) : boolean;
var
  InxBlock   : PffBlock;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  {validate the position to be 0..100}
  if (aPos < 0) or (aPos > 100) then
    with aIndex do
      FFRaiseException(EffServerException, ffStrResServer, fferrBadApproxPos,
                       [kidFI^.fiName^, kidIndex, aPos]);

  with aIndex do begin
    {get the file header, block 0}
    kidFileHeader := PffBlockHeaderFile(FFBMGetBlock(kidFI, aTI, 0,
                                                     ffc_ReadOnly, aRelMethod));
    try
      {get the index header}
      InxBlock := ReadVfyInxBlock(kidFI, aTI, kidFileHeader, ffc_ReadOnly,
                                  ffc_InxBlockTypeHeader,
                                  kidFileHeader^.bhfIndexHeader, aInxRelMeth);
      try
        kidIndexHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
        {special case: if the root page does not exist, return false
         and an invalid keypath}
        if (kidIndexHeader^.bihIndexRoot[kidIndex] = ffc_W32NoValue) then begin
          Result := false;
          FFInitKeyPath(aKeyPath);
        end
        {otherwise go read the b-tree}
        else begin
          Result := true;
          BtreeFindApprox(aIndex, aTI,
                          kidIndexHeader^.bihIndexRoot[kidIndex],
                          aKey, aRefNr, aKeyPath, aPos);
        end;
        aKeyPath.kpLSN := kidFI^.fiBufMgr.GetRAMPageLSN2               {!!.06}
                            (kidFI, kidFileHeader^.bhfIndexHeader);    {!!.06}
      finally
        aInxRelMeth(InxBlock);
      end;
    finally
      aRelMethod(PffBlock(kidFileHeader));
    end;
  end;  { with }
end;
{====================================================================}


{===Index related routines===========================================}
procedure FFTblAddIndex(aFI          : PffFileInfo;
                        aTI          : PffTransInfo;
                        aIndex       : integer;
                        aMaxKeyLen   : integer;
                        aAllowDups   : boolean;
                        aKeysAreRefs : boolean);
var
  FileHeader : PffBlockHeaderFile;
  InxBlock   : PffBlock;
  InxHeader  : PffIndexHeader;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  { First get an Exclusive lock on the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aRelMethod));
  try
    { Second get an Exclusive lock on the index header. }
    InxBlock := ReadVfyInxBlock(aFI, aTI, FileHeader, ffc_MarkDirty,
                                ffc_InxBlockTypeHeader,
                                FileHeader^.bhfIndexHeader, aInxRelMeth);
    InxHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);

    { Set up the index data. }
    with InxHeader^, FileHeader^ do begin
      {note that there is only *one* index that uses references as
       keys: index 0}
      if aKeysAreRefs then begin
        bihIndexFlags[aIndex] := ffc_InxFlagKeysAreRefs; {ie no dups!}
        bihIndexKeyLen[aIndex] := SizeOfRef;
        bhfHasSeqIndex := 1;
      end
      else begin
        bihIndexKeyLen[aIndex] := aMaxKeyLen;
        if aAllowDups then
          bihIndexFlags[aIndex] := ffc_InxFlagAllowDups
        else
          bihIndexFlags[aIndex] := 0;
      end;
      bihIndexRoot[aIndex] := ffc_W32NoValue;
    end;
    aInxRelMeth(InxBlock);
  finally
    aRelMethod(PffBlock(FileHeader));
  end;
end;
{--------}
procedure FFTblDeleteIndex(aFI    : PffFileInfo;
                           aTI    : PffTransInfo;
                           aIndex : integer);
var
  FileHeader : PffBlockHeaderFile;
  InxBlock   : PffBlock;
  InxHeader  : PffIndexHeader;
  Elements   : integer;
  aInxRelMeth,
  aRelMethod : TffReleaseMethod;
begin
  { First get an Exclusive lock on the file header, block 0.}
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aRelMethod));
  try
    { Second get an Exclusive lock on the index header. }
    InxBlock := ReadVfyInxBlock(aFI, aTI, FileHeader, ffc_MarkDirty,
                                ffc_InxBlockTypeHeader,
                                FileHeader^.bhfIndexHeader, aInxRelMeth);
    InxHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);

    { Remove the index data. }
    with InxHeader^ do begin
      if (aIndex < pred(ffcl_MaxIndexes)) then begin
        Elements := pred(ffcl_MaxIndexes - aIndex);
        Move(bihIndexKeyLen[succ(aIndex)], bihIndexKeyLen[aIndex], Elements * sizeof(word));
        Move(bihIndexFlags[succ(aIndex)], bihIndexFlags[aIndex], Elements * sizeof(byte));
        Move(bihIndexRoot[succ(aIndex)], bihIndexRoot[aIndex], Elements * sizeof(Longint));
        Move(bihIndexPageCount[succ(aIndex)], bihIndexPageCount[aIndex], Elements * sizeof(Longint));
      end;
      bihIndexKeyLen[pred(ffcl_MaxIndexes)] := 0;
      bihIndexFlags[pred(ffcl_MaxIndexes)] := 0;
      bihIndexRoot[pred(ffcl_MaxIndexes)] := ffc_W32NoValue;
      bihIndexPageCount[pred(ffcl_MaxIndexes)] := 0;
    end;
    aInxRelMeth(InxBlock);
  finally
    aRelMethod(PffBlock(FileHeader));
  end;
end;
{--------}
procedure FFTblPrepareIndexes(aFI : PffFileInfo;
                              aTI      : PffTransInfo);
var
  FileHeader  : PffBlockHeaderFile;
  InxBlock    : PffBlock;
  InxBlockHdr : PffBlockHeaderIndex absolute InxBlock;
  InxHeader   : PffIndexHeader;
  aInxRelMeth,
  aRelMethod  : TffReleaseMethod;
begin
  { First get the file header, block 0. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                aRelMethod));
  try
    {create the index header block}
    InxBlock := GetNewInxHeaderBlock(aFI, aTI, aInxRelMeth);
    with FileHeader^ do begin
      bhfIndexHeader := InxBlockHdr^.bhiThisBlock;
      InxHeader := PffIndexHeader(@InxBlock^[ffc_BlockHeaderSizeIndex]);
      with InxHeader^ do begin
        {set up the internal fields}
        FillChar(bihIndexKeyLen, sizeof(bihIndexKeyLen), 0);
        FillChar(bihIndexFlags, sizeof(bihIndexFlags), 0);
        FillChar(bihIndexRoot, sizeof(bihIndexRoot), ffc_W32NoValue);
        FillChar(bihIndexPageCount, sizeof(bihIndexPageCount), 0);
      end;
    end;
    aInxRelMeth(InxBlock);
  finally
    aRelMethod(PffBlock(FileHeader));
  end;
end;
{====================================================================}


{===Keypath routines=================================================}
procedure FFInitKeyPath(var aKeyPath : TffKeyPath);
begin
  FillChar(aKeyPath, sizeof(aKeyPath), 0);
end;
{--------}
procedure FFSetKeyPathToBOF(var aKeyPath : TffKeyPath);
begin
  FillChar(aKeyPath, sizeof(aKeyPath), 0);
  aKeyPath.kpPos := kppBOF;
end;
{--------}
procedure FFSetKeyPathToEOF(var aKeyPath : TffKeyPath);
begin
  FillChar(aKeyPath, sizeof(aKeyPath), 0);
  aKeyPath.kpPos := kppEOF;
end;
{====================================================================}

end.
