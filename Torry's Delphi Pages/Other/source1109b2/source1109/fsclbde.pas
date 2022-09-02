{*********************************************************}
{* FlashFiler: BDE consts and types for client           *}
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

{Note: The following definitions are copied from BDE.PAS. The client
       cannot have BDE in its uses list since that unit has an
       initialization section which, when run, would pull in far too
       much for the FF client. This also removes any requirements for
       the BDE package when using runtime packages}

{BDE.PAS source file and error codes are
 (c) Copyright Borland International Inc, 1997}

{$I fsdefine.inc}

{$Z+}

Unit fsclbde;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fssrbde,
  fsconst,
  fsllbase;

{-----------------------------------------------------------------------}
{     DBI types                                                         }
{-----------------------------------------------------------------------}
Const
  { Constants }

  DBIMAXNAMELEN = 31; { Name limit (table, field etc) }
  DBIMAXTBLNAMELEN = 260; { Max table name length }
  DBIMAXFLDSINKEY = 16; { Max fields in a key }
  DBIMAXKEYEXPLEN = 220; { Max Key expression length }
  DBIMAXVCHKLEN = 255; { Max val check len }
  DBIMAXPICTLEN = 175; { Max picture len }
  DBIMAXPATHLEN = 260; { Max path+file name len (excluding zero termination) }

  {============================================================================}
  {                             G e n e r a l                                  }
  {============================================================================}

Type
  TIME = Longint;

  { Handle Types }
Type
  _hDBIObj = Record
  End; { Dummy structure to create "typed" handles }
  hDBIFilter = ^_hDBIObj; { Filter handle }

  { typedefs for buffers of various common sizes: }
  DBINAME = Packed Array[0..DBIMAXNAMELEN] Of Char; { holds a name }
  DBITBLNAME = Packed Array[0..DBIMAXTBLNAMELEN] Of Char; { holds a table name }
  DBIKEY = Packed Array[0..DBIMAXFLDSINKEY - 1] Of Word; { holds list of fields in a key }
  DBIKEYEXP = Packed Array[0..DBIMAXKEYEXPLEN] Of Char; { holds a key expression }
  DBIVCHK = Packed Array[0..DBIMAXVCHKLEN] Of Byte; { holds a validity check }
  DBIPICT = Packed Array[0..DBIMAXPICTLEN] Of Char; { holds a picture (Pdox) }
  DBIPATH = Packed Array[0..DBIMAXPATHLEN] Of Char; { holds a DOS path }

  {============================================================================}
  {                    Cursor properties                                       }
  {============================================================================}
Type
  DBIShareMode = ({ Database/Table Share type }
    dbiOPENSHARED, { Open shared  (Default) }
    dbiOPENEXCL { Open exclusive }
    );

  DBIOpenMode = ({ Database/Table Access type }
    dbiREADWRITE, { Read + Write   (Default) }
    dbiREADONLY { Read only }
    );

  FFXLTMode = ({ Field translate mode }
    xltNONE, { No translation  (Physical Types) }
    xltRECORD, { Record level translation (not supported) }
    xltFIELD { Field level translation (Logical types) }
    );

  { Linear exression tree}
  {----------------------}
Type
  pFILTERInfo = ^FILTERInfo;
  FILTERInfo = Packed Record
    iFilterId: Word; { Id for filter }
    hFilter: hDBIFilter; { Filter handle }
    iClientData: Longint; { Client supplied data }
    iPriority: Word; { 1..N with 1 being highest }
    bCanAbort: WordBool; { TRUE : pfFilter can return ABORT }
    pfFilter: pfGENFilter; { Client filter function }
    pCanExpr: Pointer; { Supplied expression }
    bActive: WordBool; { TRUE : filter is active }
  End;

  {pfGENFilter returns TRUE, FALSE or ABORT }
Const
  ABORT = -2;

  {============================================================================}
  {                    Field descriptor                                        }
  {============================================================================}
Type
  FLDVchk = ({ Field Val Check type }
    fldvNOCHECKS, { Does not have explicit val checks }
    fldvHASCHECKS, { One or more val checks on the field }
    fldvUNKNOWN { Dont know at this time }
    );

Type
  FLDRights = ({ Field Rights }
    fldrREADWRITE, { Field can be Read/Written }
    fldrREADONLY, { Field is Read only }
    fldrNONE, { No Rights on this field }
    fldrUNKNOWN { Dont know at this time }
    );

Type
  pFLDDesc = ^FLDDesc;
  FLDDesc = Packed Record { Field Descriptor }
    iFldNum: Word; { Field number (1..n) }
    szName: DBINAME; { Field name }
    iFldType: Word; { Field type }
    iSubType: Word; { Field subtype (if applicable) }
    iUnits1: Smallint; { Number of Chars, digits etc }
    iUnits2: Smallint; { Decimal places etc. }
    iOffset: Longint; { Offset in the record (computed) }
    iLen: Longint; { Length in bytes (computed) }
    iNullOffset: Longint; { For Null bits (computed) }
    efldvVchk: FLDVchk; { Field Has vcheck (computed) }
    efldrRights: FLDRights; { Field Rights (computed) }
    bCalcField: WordBool; { Is Calculated field (computed) }
    iUnUsed: Packed Array[0..1] Of Word;
  End;

  {============================================================================}
  {                   Record Properties                                        }
  {============================================================================}

Type
  pRECProps = ^RECProps;
  RECProps = Packed Record { Record properties }
    iSeqNum: Longint; { When Seq# supported only }
    iPhyRecNum: Longint; { When Phy Rec#s supported only }
    iRecStatus: Word; { Delayed Updates Record Status }
    bSeqNumChanged: WordBool; { Not used }
    bDeleteFlag: WordBool; { When soft delete supported only }
  End;

  {============================================================================}
  {                    Index descriptor                                        }
  {============================================================================}

Type
  pIDXDesc = ^IDXDesc;
  IDXDesc = Packed Record { Index description }
    szName: DBITBLNAME; { Index name }
    iIndexId: Word; { Index number }
    szTagName: DBINAME; { Tag name (for dBASE) }
    szFormat: DBINAME; { Optional format (BTREE, HASH etc) }
    bPrimary: WordBool; { True, if primary index }
    bUnique: WordBool; { True, if unique keys (TRI-STATE for dBASE) }
    bDescending: WordBool; { True, for descending index }
    bMaintained: WordBool; { True, if maintained index }
    bSubset: WordBool; { True, if subset index }
    bExpIdx: WordBool; { True, if expression index }
    iCost: Word; { Not used }
    iFldsInKey: Word; { Fields in the key (1 for Exp) }
    iKeyLen: Word; { Phy Key length in bytes (Key only) }
    bOutofDate: WordBool; { True, if index out of date }
    iKeyExpType: Word; { Key type of Expression }
    aiKeyFld: DBIKEY; { Array of field numbers in key }
    szKeyExp: DBIKEYEXP; { Key expression }
    szKeyCond: DBIKEYEXP; { Subset condition }
    bCaseInsensitive: WordBool; { True, if case insensitive index }
    iBlockSize: Word; { Block size in bytes }
    iRestrNum: Word; { Restructure number }
    abDescending: Packed Array[0..DBIMAXFLDSINKEY - 1] Of WordBool; { TRUE }
    iUnUsed: Packed Array[0..15] Of Word;
  End;

  {============================================================================}
  {             Validity check, Referential integrity descriptors              }
  {============================================================================}

  { Subtypes for Lookup }

  LKUPType = ({ Paradox Lookup type }
    lkupNONE, { Has no lookup }
    lkupPRIVATE, { Just Current Field + Private }
    lkupALLCORRESP, { All Corresponding + No Help }
    lkupHELP, { Just Current Fld + Help and Fill }
    lkupALLCORRESPHELP { All Corresponging + Help }
    );

Type
  pVCHKDesc = ^VCHKDesc;
  VCHKDesc = Packed Record { Val Check structure }
    iFldNum: Word; { Field number }
    bRequired: WordBool; { If True, value is required }
    bHasMinVal: WordBool; { If True, has min value }
    bHasMaxVal: WordBool; { If True, has max value }
    bHasDefVal: WordBool; { If True, has default value }
    aMinVal: DBIVCHK; { Min Value }
    aMaxVal: DBIVCHK; { Max Value }
    aDefVal: DBIVCHK; { Default value }
    szPict: DBIPICT; { Picture string }
    elkupType: LKUPType; { Lookup/Fill type }
    szLkupTblName: DBIPATH; { Lookup Table name }
  End;

  {============================================================================}
  {                             Key searches                                   }
  {============================================================================}

Type
  DBISearchCond = ({ Search condition for keys }
    keySEARCHEQ, { = }
    keySEARCHGT, { > }
    keySEARCHGEQ { >= }
    );

  {============================================================================}
  {                      Date, Time, Number Formats                            }
  {============================================================================}

Type
  pFMTBcd = ^FMTBcd;
  FMTBcd = Packed Record
    iPrecision: Byte; { 1..64 considered valid }
    iSignSpecialPlaces: Byte; { sign:1, special:1, places:6 }
    iFraction: Packed Array[0..31] Of Byte; { bcd nibbles, 00..99 per byte, high nibble 1st }
  End;

  {============================================================================}
  {                    Security descriptor                                     }
  {============================================================================}
Const
  prvUNKNOWN = $FF; { Unknown }

  {============================================================================}
  {                            Error Categories                                }
  {============================================================================}
Function ErrCat(rslt: Word): Word;

Implementation

Function ErrCat(rslt: Word): Word;
Begin
  ErrCat := rslt Shr 8;
End;

End.

