{*********************************************************}
{* FlashFiler: FF 2 file repair constants                *}
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

unit ffrepcnst;

interface

type
  TffRepairAction = (raDecide, raSelfRepair, raPack, raUnsalvageable);
    { This enumerated type represents the different types of repair actions
      that may be taken. Values:

      raDecide - The parent repair logic must decide what action to take based
        upon the current context. For example, if the data dictionary block
        reported that it had an unknown block type then the repair logic could
        decide the table is unsalvageable. But if it were an index or data
        block that did not know its block type then the repair logic could
        decide the table needed to be reindexed or restructured.

      raSelfRepair - Allow the block to repair itself.

      raPack - Restructure the table.

      raUnsalvageable - The data is so badly damaged that nothing can be
        done with the table.

    }

const

  { Verify error codes. }
  rciUnknownBlockType          = 1;
  rciInvalidBlockRefNext       = 2;
  rciInvalidBlockRefDict       = 3;
  rciInvalidThisBlock          = 4;
  rciInvalidBlockSize          = 5;
  rciNoDictBlock               = 6;
  rciInvalidInt64              = 7;
  rciNoDataBlockForRecs        = 8;
  rciInvalidBlockRefFirstData  = 9;
  rciInvalidBlockRefFirstFree  = 10;
  rciInvalidSeqIndexFlag       = 11;
  rciInvalidBlockRefIndexHead  = 12;
  rciNoLastDataBlockForRecs    = 13;
  rciInvalidBlockRefLastData   = 14;
  rciInvalidLog2BlockSize      = 15;
  rciInvalidUsedBlocks         = 16;
  rciInxHeaderInvalidRowCount  = 17;
  rciInxHeaderInvalidKeyLen    = 18;
  rciInxHeaderInvalidKeyCount  = 19;
  rciInxHeaderNoRootPage       = 20;
  rciInxHeaderInvalidRootPage  = 21;
  rciInxHeaderNoRefsFlag       = 22;
  rciInxHeaderNoDupsFlag       = 23;
  rciInvalidInxPrefPageRef     = 24;
  rciInxInvalidBlockRef        = 25;
  rciInvalidLeafKeyBlockRef    = 26;
  rciInvalidLeafKeyRefNum      = 27;
  rciInvalidIntrnalKeyBlockRef = 28;
  rciInvalidIntrnalKeyRefNum   = 29;
  rciInvalidDataBlockRecCount  = 30;
  rciInvalidDataBlockRecLen    = 31;
  rciInvalidNextDataBlock      = 32;
  rciInvalidPrevDataBlock      = 33;
  rciBLOBDeleted               = 34;
  rciBLOBContentBlockSignature = 35;
  rciBLOBContentSegSignature   = 36;
  rciBLOBInvalidRefNr          = 37;
  rciBLOBInvalidLookupRefNr    = 38;
  rciBLOBInvalidContentRefNr   = 39;
  rciBLOBHeaderSignature       = 40;
  rciPackFailure               = 41;
  rciOrphanedUsedDataBlocks    = 42;
  rciSplitUsedDataBlocks       = 43;

  rciNumErrCodes               = 43;

  { Verify error strings per error. }
  rcErrStr : array[1..rciNumErrCodes] of string =
    (
{1}   'Unknown block type: %d.',
{2}   'Invalid block reference, Next Block points to block %d.',
{3}   'Invalid block reference, DataDict points to block %d.',
{4}   'Invalid internal block number. Should be %d but is set to %d.',
{5}   'Invalid block size: %d.',
{6}   'File header DataDictBlockNum does not point to a data dictionary.',
{7}   'Invalid %s, value: %d:%d.',
{8}   'Record count is %d but FirstDataBlock does not point to a data block.',
{9}   'Invalid block reference, FirstDataBlock points to non-data block %d.',
{10}  'Invalid block reference, FirstFreeBlock points to active block %d.',
{11}  'Invalid sequential access index flag in file header, value: %d.',
{12}  'Invalid block reference, IndexHeaderBlockNum points to non-index block %d.',
{13}  'Record count is %d but LastDataBlock does not point to a data block.',
{14}  'Invalid block reference, LastDataBlock points to non-data block %d',
{15}  'Invalid Log2 block size. For block size %d, expected %d but actual value is %d.',
{16}  'Invalid Used Blocks count. Calculated as %d but actual value is %d.',
{17}  'Index header contains %d rows but there are %d indices in the dictionary.',
{18}  'Index header row %d specifies key length of %d but dictionary specifies key length of %d',
{19}  'Index header row %d specifies the index contains %d keys but there are %d records in the table.',
{20}  'No root page specified for row %d of index header',
{21}  'Root page reference in row %d of index header does not point to an index block',
{22}  'Row 0 of index header does not have "keys are reference numbers" flag set',
{23}  'Dictionary indicates index %d allows duplicate keys but the row %d in the index header does not have this flag set',
{24}  'Index block previous page reference points to non-index block %d',
{25}  'Key %d of leaf index block %d (index %d) references block %d',
{26}  'Key %d of leaf index block %d (index %d) points to data block %d but that block is not a data block.  The refNum for that key is %d:%d. %s',
{27}  'Key %d of leaf index block %d (index %d) points to data block %d. The RefNum (%d:%d) is invalid for that data block.',
{28}  'Key %d of internal index block %d (index %d) points to index block %d but that block is not an index block.  The refNum for that key is %d:%d. %s',
{29}  'Key %d of internal index block %d (index %d) points to data block %d. The RefNum (%d:%d) is invalid for that data block.',
{30}  'Header of data block %d says record count is %d but it is listed as %d records per block in the file header',
{31}  'Header of data block %d says record length is %d but it is listed as %d in the data dictionary',
{32}  'Header of data block %d points to next data block %d but that block is not a data block',
{33}  'Header of data block %d points to previous data block %d but that block is not a data block',
{34}  'The BLOB is marked as deleted (BLOB field "%s", BLOB refnum %d:%d, key fields: %s, record %d of data block %d)',
{35}  'A content block has an invalid signature (BLOB field "%s", BLOB refnum %d:%d, key fields: [%s], record %d of data block %d)',
{36}  'A content segment has an invalid signature (BLOB field "%s", BLOB refnum %d:%d, key fields: [%s], record %d of data block %d)',
{37}  'Invalid BLOB reference number (BLOB field "%s", BLOB refnum %d:%d, key fields: [%s], record %d (base 0) of data block %d)',
{38}  'Invalid BLOB lookup segment reference number (BLOB field "%s", BLOB refnum %d:%d, key fields: [%s], record %d (base 0) of data block %d)',
{39}  'Invalid BLOB content segment reference number (BLOB field "%s", BLOB refnum %d:%d, key fields: [%s], record %d (base 0) of data block %d)',
{40}  'Invalid BLOB header signature (BLOB field "%s", BLOB refnum %d:%d, key fields: [%s], record %d (base 0) of data block %d)',
{41}  'Could not pack table: %s',
{42}  'There are data blocks that are not part of the used data block chain',
{43}  'There are breaks in the chain of used data blocks'
    );

  { Recommended actions per error. }
  rcAction : array[1..rciNumErrCodes] of TffRepairAction =
    (
      raSelfRepair,         {rciUnknownBlockType}
      raDecide,             {rciInvalidBlockRefNext}
      raDecide,             {rciInvalidBlockRefDict}
      raSelfRepair,         {rciInvalidThisBlock}
      raDecide,             {rciInvalidBlockSize}
      raDecide,             {rciNoDictBlock}
      raPack,               {rciInvalidInt64}
      raPack,               {rciNoDataBlockForRecs}
      raPack,               {rciInvalidBlockRefFirstData}
      raPack,               {rciInvalidBlockRefFirstFree}
      raSelfRepair,         {rciInvalidSeqIndexFlag}
      raPack,               {rciInvalidBlockRefIndexHead}
      raPack,               {rciNoLastDataBlockForRecs}
      raPack,               {rciInvalidBlockRefLastData}
      raSelfRepair,         {rciInvalidLog2BlockSize}
      raSelfRepair,         {rciInvalidUsedBlocks}
      raPack,               {rciInxHeaderInvalidRowCount}
      raPack,               {rciInxHeaderInvalidKeyLen}
      raPack,               {rciInxHeaderInvalidKeyCount}
      raPack,               {rciInxHeaderNoRootPage}
      raPack,               {rciInxHeaderInvalidRootPage}
      raPack,               {rciInxHeaderNoRefsFlag}
      raPack,               {rciInxHeaderNoDupsFlag}
      raPack,               {rciInvalidInxPrefPageRef}
      raPack,               {rciInxInvalidPageRef}
      raPack,               {rciInvalidLeafKeyBlockRef}
      raPack,               {rciInvalidLeafKeyRefNum}
      raPack,               {rciInalidIntrnalKeyBlockRef}
      raPack,               {rciInvalidIntrnalKeyRefNum}
      raSelfRepair,         {rciInvalidDataBlockRecCount}
      raSelfRepair,         {rciInvalidDataBlockRecLen}
      raPack,               {rciInvalidNextDataBlock}
      raPack,               {rciInvalidPrevDataBlock}
      raPack,               {rciBLOBDeleted}
      raPack,               {rciBLOBContentBlockSignature}
      raPack,               {rciBLOBContentSegSignature}
      raPack,               {rciBLOBInvalidRefNr}
      raPack,               {rciBLOBInvalidLookupRefNr}
      raPack,               {rciBLOBInvalidContentRefNr}
      raPack,               {rciBLOBHeaderSignature}
      raUnsalvageable,      {rciPackFailure}
      raSelfRepair,         {rciOrphanedUsedDataBlocks}
      raSelfRepair          {rciSplitUsedDataBlocks}
    );

  { How the problem was repaired. Specify values only for those problems that
    can be self-repaired. }

  csBLOBRefSetToNull = 'BLOB reference set to null (field "%s", key fields: [%s], record %d of data block %d).';

  rcFixStr : array[1..rciNumErrCodes] of string =
    (
      'Block %d marked as a free block',                   {rciUnknownBlockType}
      'NextBlock set to value %d.',                        {rciInvalidBlockRefNext}
      '',                                                  {rciInvalidBlockRefDict}
      'ThisBlock set to value %d.',                        {rciInvalidThisBlock}
      '',                                                  {rciInvalidBlockSize}
      '',                                                  {rciNoDictBlock}
      '',                                                  {rciInvalidInt64}
      '',                                                  {rciNoDataBlockForRecs}
      '',                                                  {rciInvalidBlockRefFirstData}
      '',                                                  {rciInvalidBlockRefFirstFree}
      'Sequential index flag set to value %d.',            {rciInvalidSeqIndexFlag}
      '',                                                  {rciInvalidBlockRefIndexHead}
      '',                                                  {rciNoLastDataBlockForRecs}
      'Last Data Block set to value %d.',                  {rciInvalidBlockRefLastData}
      'Log 2 block size set to value %d.',                 {rciInvalidLog2BlockSize}
      'Used block count set to value %d.',                 {rciInvalidUsedBlocks}
      '',                                                  {rciInxHeaderInvalidRowCount}
      '',                                                  {rciInxHeaderInvalidKeyLen}
      '',                                                  {rciInxHeaderInvalidKeyCount}
      '',                                                  {rciInxHeaderNoRootPage}
      '',                                                  {rciInxHeaderInvalidRootPage}
      '',                                                  {rciInxHeaderNoRefsFlag}
      '',                                                  {rciInxHeaderNoDupsFlag}
      '',                                                  {rciInvalidInxPrefPageRef}
      '',                                                  {rciInxInvalidPageRef}
      '',                                                  {rciInvalidLeafKeyBlockRef}
      '',                                                  {rciInvalidLeafKeyRefNum}
      '',                                                  {rciInvalidIntrnalKeyBlockRef}
      '',                                                  {rciInvalidIntrnalKeyRefNum}
      'Record count in data block %d set to %d.',          {rciInvalidDataBlockRecCount}
      'Record length in data block %d set to %d.',         {rciInvalidDataBlockRecLen}
      '',                                                  {rciInvalidNextDataBlock}
      '',                                                  {rciInvalidPrevDataBlock}
      csBLOBRefSetToNull,                                  {rciBLOBDeleted}
      csBLOBRefSetToNull,                                  {rciBLOBContentBlockSignature}
      csBLOBRefSetToNull,                                  {rciBLOBContentSegSignature}
      csBLOBRefSetToNull,                                  {rciBLOBInvalidRefNr}
      csBLOBRefSetToNull,                                  {rciBLOBInvalidLookupRefNr}
      csBLOBRefSetToNull,                                  {rciBLOBInvalidContentRefNr}
      csBLOBRefSetToNull,                                  {rciBLOBHeaderSignature}
      '',                                                  {rciPackFailure}
      'Orphaned data blocks added to used block chain.',   {rciOrphanedUsedDataBlocks}
      'Used data block chain repaired.'                    {rciSplitUsedDataBlocks}
    );


implementation

end.
