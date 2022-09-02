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

{$I ffdefine.inc}

{$Z+}

unit ffclbde;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffsrbde,
  ffconst,
  ffllbase;

{-----------------------------------------------------------------------}
{     DBI types                                                         }
{-----------------------------------------------------------------------}
const
{ Constants }

  DBIMAXNAMELEN      = 31;              { Name limit (table, field etc) }
  DBIMAXTBLNAMELEN   = 260;             { Max table name length }
  DBIMAXFLDSINKEY    = 16;              { Max fields in a key }
  DBIMAXKEYEXPLEN    = 220;             { Max Key expression length }
  DBIMAXVCHKLEN      = 255;             { Max val check len }
  DBIMAXPICTLEN      = 175;             { Max picture len }
  DBIMAXPATHLEN      = 260;             { Max path+file name len (excluding zero termination) }

{============================================================================}
{                             G e n e r a l                                  }
{============================================================================}

type
  TIME               = Longint;


{ Handle Types }
type
  _hDBIObj           = record end;      { Dummy structure to create "typed" handles }
  hDBIFilter         = ^_hDBIObj;       { Filter handle }


{ typedefs for buffers of various common sizes: }
  DBINAME            = packed array [0..DBIMAXNAMELEN] of Char; { holds a name }
  DBITBLNAME         = packed array [0..DBIMAXTBLNAMELEN] of Char; { holds a table name }
  DBIKEY             = packed array [0..DBIMAXFLDSINKEY-1] of Word; { holds list of fields in a key }
  DBIKEYEXP          = packed array [0..DBIMAXKEYEXPLEN] of Char; { holds a key expression }
  DBIVCHK            = packed array [0..DBIMAXVCHKLEN] of Byte; { holds a validity check }
  DBIPICT            = packed array [0..DBIMAXPICTLEN] of Char; { holds a picture (Pdox) }
  DBIPATH            = packed array [0..DBIMAXPATHLEN] of Char; { holds a DOS path }

{============================================================================}
{                    Cursor properties                                       }
{============================================================================}
type
  DBIShareMode = (                      { Database/Table Share type }
    dbiOPENSHARED,                      { Open shared  (Default) }
    dbiOPENEXCL                         { Open exclusive }
  );

  DBIOpenMode = (                       { Database/Table Access type }
    dbiREADWRITE,                       { Read + Write   (Default) }
    dbiREADONLY                         { Read only }
  );

  FFXLTMode = (                         { Field translate mode }
    xltNONE,                            { No translation  (Physical Types) }
    xltRECORD,                          { Record level translation (not supported) }
    xltFIELD                            { Field level translation (Logical types) }
  );

{ Linear exression tree}
{----------------------}
type
  pFILTERInfo = ^FILTERInfo;
  FILTERInfo = packed record
    iFilterId       : Word;             { Id for filter }
    hFilter         : hDBIFilter;       { Filter handle }
    iClientData     : Longint;          { Client supplied data }
    iPriority       : Word;             { 1..N with 1 being highest }
    bCanAbort       : WordBool;         { TRUE : pfFilter can return ABORT }
    pfFilter        : pfGENFilter;      { Client filter function }
    pCanExpr        : Pointer;          { Supplied expression }
    bActive         : WordBool;         { TRUE : filter is active }
  end;

{pfGENFilter returns TRUE, FALSE or ABORT }
const
  ABORT              = -2;


{============================================================================}
{                    Field descriptor                                        }
{============================================================================}
type
  FLDVchk = (                           { Field Val Check type }
    fldvNOCHECKS,                       { Does not have explicit val checks }
    fldvHASCHECKS,                      { One or more val checks on the field }
    fldvUNKNOWN                         { Dont know at this time }
  );

type
  FLDRights = (                         { Field Rights }
    fldrREADWRITE,                      { Field can be Read/Written }
    fldrREADONLY,                       { Field is Read only }
    fldrNONE,                           { No Rights on this field }
    fldrUNKNOWN                         { Dont know at this time }
  );

type
  pFLDDesc = ^FLDDesc;
  FLDDesc = packed record               { Field Descriptor }
    iFldNum         : Word;             { Field number (1..n) }
    szName          : DBINAME;          { Field name }
    iFldType        : Word;             { Field type }
    iSubType        : Word;             { Field subtype (if applicable) }
    iUnits1         : SmallInt;         { Number of Chars, digits etc }
    iUnits2         : SmallInt;         { Decimal places etc. }
    iOffset         : Word;             { Offset in the record (computed) }
    iLen            : Word;             { Length in bytes (computed) }
    iNullOffset     : Word;             { For Null bits (computed) }
    efldvVchk       : FLDVchk;          { Field Has vcheck (computed) }
    efldrRights     : FLDRights;        { Field Rights (computed) }
    bCalcField      : WordBool;         { Is Calculated field (computed) }
    iUnUsed         : packed array [0..1] of Word;
  end;

{============================================================================}
{                   Record Properties                                        }
{============================================================================}

type
  pRECProps = ^RECProps;
  RECProps = packed record              { Record properties }
    iSeqNum         : Longint;          { When Seq# supported only }
    iPhyRecNum      : Longint;          { When Phy Rec#s supported only }
    iRecStatus      : Word;             { Delayed Updates Record Status }
    bSeqNumChanged  : WordBool;         { Not used }
    bDeleteFlag     : WordBool;         { When soft delete supported only }
  end;

{============================================================================}
{                    Index descriptor                                        }
{============================================================================}

type
  pIDXDesc = ^IDXDesc;
  IDXDesc = packed record               { Index description }
    szName          : DBITBLNAME;       { Index name }
    iIndexId        : Word;             { Index number }
    szTagName       : DBINAME;          { Tag name (for dBASE) }
    szFormat        : DBINAME;          { Optional format (BTREE, HASH etc) }
    bPrimary        : WordBool;         { True, if primary index }
    bUnique         : WordBool;         { True, if unique keys (TRI-STATE for dBASE) }
    bDescending     : WordBool;         { True, for descending index }
    bMaintained     : WordBool;         { True, if maintained index }
    bSubset         : WordBool;         { True, if subset index }
    bExpIdx         : WordBool;         { True, if expression index }
    iCost           : Word;             { Not used }
    iFldsInKey      : Word;             { Fields in the key (1 for Exp) }
    iKeyLen         : Word;             { Phy Key length in bytes (Key only) }
    bOutofDate      : WordBool;         { True, if index out of date }
    iKeyExpType     : Word;             { Key type of Expression }
    aiKeyFld        : DBIKEY;           { Array of field numbers in key }
    szKeyExp        : DBIKEYEXP;        { Key expression }
    szKeyCond       : DBIKEYEXP;        { Subset condition }
    bCaseInsensitive : WordBool;        { True, if case insensitive index }
    iBlockSize      : Word;             { Block size in bytes }
    iRestrNum       : Word;             { Restructure number }
    abDescending    : packed array [0..DBIMAXFLDSINKEY-1] of WordBool; { TRUE }
    iUnUsed         : packed array [0..15] of Word;
  end;

{============================================================================}
{             Validity check, Referential integrity descriptors              }
{============================================================================}

{ Subtypes for Lookup }

  LKUPType = (                          { Paradox Lookup type }
    lkupNONE,                           { Has no lookup }
    lkupPRIVATE,                        { Just Current Field + Private }
    lkupALLCORRESP,                     { All Corresponding + No Help }
    lkupHELP,                           { Just Current Fld + Help and Fill }
    lkupALLCORRESPHELP                  { All Corresponging + Help }
  );

type
  pVCHKDesc = ^VCHKDesc;
  VCHKDesc = packed record              { Val Check structure }
    iFldNum         : Word;             { Field number }
    bRequired       : WordBool;         { If True, value is required }
    bHasMinVal      : WordBool;         { If True, has min value }
    bHasMaxVal      : WordBool;         { If True, has max value }
    bHasDefVal      : WordBool;         { If True, has default value }
    aMinVal         : DBIVCHK;          { Min Value }
    aMaxVal         : DBIVCHK;          { Max Value }
    aDefVal         : DBIVCHK;          { Default value }
    szPict          : DBIPICT;          { Picture string }
    elkupType       : LKUPType;         { Lookup/Fill type }
    szLkupTblName   : DBIPATH;          { Lookup Table name }
  end;

{============================================================================}
{                             Key searches                                   }
{============================================================================}

type
  DBISearchCond = (                     { Search condition for keys }
    keySEARCHEQ,                        { = }
    keySEARCHGT,                        { > }
    keySEARCHGEQ                        { >= }
  );

{============================================================================}
{                      Date, Time, Number Formats                            }
{============================================================================}

type
  pFMTBcd = ^FMTBcd;
  FMTBcd  = packed record
    iPrecision      : Byte;             { 1..64 considered valid }
    iSignSpecialPlaces : Byte;          { sign:1, special:1, places:6 }
    iFraction       : packed array [0..31] of Byte;    { bcd nibbles, 00..99 per byte, high nibble 1st }
  end;

{============================================================================}
{                    Security descriptor                                     }
{============================================================================}
const
  prvUNKNOWN   = $FF;                 { Unknown }

{============================================================================}
{                            Error Categories                                }
{============================================================================}
function ErrCat(rslt: Word): Word;

implementation

function ErrCat(rslt: Word): Word;
begin
  ErrCat := rslt shr 8;
end;


end.
