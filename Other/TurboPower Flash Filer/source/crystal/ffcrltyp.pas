{*********************************************************}
(* Datatypes specific to this physical database.         *)
(* These types are extracted from the PHYSDB.CPP source  *)
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

{$I ffcrdefn.inc}

unit ffcrltyp;

interface

uses
  ffllbase,
  SysUtils,
  ffclbde,
  ffsrbde,
  ffdb,
  ffdbbase,
  ffcrtype;

const
  BLOB_INFO_SIZE = 8;

type
  TDbiDate = ffsrbde.DBIDATE;
  TDbiTime = ffclbde.TIME;
  TDbiTimestamp = ffsrbde.TIMESTAMP;

  PDbiDate = ^TDbiDate;
  PDbiTime = ^TDbiTime;
  PDbiTimestamp = TDbiTimestamp;

  PPhysDbReadFieldInfo = ^TPhysDbReadFieldInfo;
  TPhysDbReadFieldInfo = packed record
    ReadFieldNo    : TcrInt16u;
    FieldNo        : TcrInt16u;
    OffsetInRecord : TcrInt16u;
    FieldLength    : TcrInt16u;
    FieldType      : TFieldValueType;

    NativeFieldType         : TcrInt16u;
    NBytesInNativeField     : TcrInt16u;
    NDecPlacesInNativeField : TcrInt16u;
    NativeFieldOffset       : TcrInt16u;

    OffsetInStopKeyBuf : TcrInt16u;  { offset of each range field }
    StopInclusive      : TcrBoolean; { only used in stopping the range search }
  end;
  TPhysDbReadFieldInfoArray = array[0..32767 div SizeOf(TPhysDbReadFieldInfo)] of TPhysDbReadFieldInfo;
  PPhysDbReadFieldInfoArray = ^TPhysDbReadFieldInfoArray;

  PPhysDbReadInfo = ^TPhysDbReadInfo;
  TPhysDbReadInfo = packed record
    NBytesInPhysRecord : TcrInt16u;
    PhysRecordBuf      : PffByteArray;

    CurrentRecord : TcrInt32u;
    KeyBuf        : array[0..1023] of Char;

    NBytesInReadRecord  : TcrInt16u;
    NFieldsInReadRecord : TcrInt16u;
    FieldInfo           : PPhysDbReadFieldInfoArray;

    NBytesInIndexRecord  : TcrInt16u;
    NFieldsInIndexRecord : TcrInt16u;
    IndexFieldInfo       : PPhysDbReadFieldInfoArray;

    ValuesUnique       : TcrBoolean;   { Always T for primary, F for secondary }
    IndexCaseSensitive : TcrBoolean;   { If the index in use is case sensitive }
    AscendingIndex     : TcrBoolean;

    NFieldsInIndexDefn : TcrInt16u;           { Save field types, etc. }
    IndexDefnInfo      : PPhysDbReadFieldInfoArray;

    NumRanges      : TcrInt16u;
    RangeFieldInfo : PPhysDbReadFieldInfoArray;
    NStopKeyRanges : TcrInt16u;
    StopKeyBuf     : array[0..254] of Char; { the upper limit for range search }
    StopKeyLen     : TcrInt16u;             { generic integer type }

    NFieldsInLookupValue : TcrInt16u;       { Always <= NFieldsInIndexDefn }
    LookupValueLen       : TcrInt16u;
    LastLookupFieldLen   : TcrInt16u;       { Only for partial lookup }
    LastLookupFieldIsSubstr : TcrBoolean;   { T = CLOSEST lookup }
  end;

  TPhysDbFileHandle = packed record
    DatabaseID      : TffWord32; { database handle from IDAPI }
    CursorID        : TffWord32; { Cursor handle from IDAPI }
    PathAndFileName : PChar;     { save data file path and name }
    IndexFilename   : PChar;     { save the index file path and name }
    TagName         : PChar;     { save the tag name in the index }
    MainFile        : Boolean;   { for sorting and range }
    RangeLimit      : Boolean;
    ReadInfo        : PPhysDbReadInfo;
    NotXlateDOSString : Boolean;
    NotXlateDOSMemo   : Boolean;
  end;

  TPhysDbServerHandle = packed record
  end;

implementation

end.
