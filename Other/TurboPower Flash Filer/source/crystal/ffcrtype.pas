{*********************************************************}
{* Low-level datatypes.                                  *)
(* Direct port of original TYPES.HPP and DBTYPES.HPP     *)
(* source files                                          *)
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

unit ffcrtype;

interface

{ The following types are derived from the original DBTYPES.HPP source file }
type
  TDBFieldFileType = (ftFlatFile, ftRecurringFile, ftStoredProcedure);

  TDBFieldAlignment = (alLeftAlignedChars, alRightAlignedChars);

  TDBLinkLookupType = (luLookupParallel, luLookupProduct, luLookupSeries);

  TDBLinkJoinType = (
    jtUnused1,
    jtUnused2,
    jtUnused3,
    jtLookupEqual,
    jtLookupLeftOuter,
    jtLookupRightOuter,
    jtLookupOuter,
    jtLookupGreaterThan,
    jtLookupLessThan,
    jtLookupGreaterOrEqual,
    jtLookupLessOrEqual,
    jtLookupNotEqual
  );

{ The following types are derived from the original TYPES.HPP source file.
  These are generally datatypes shared between the driver and the CRW application. }
type
  DWORD       = LongInt;
  TcrInt8u    = Byte;
  TcrInt8s    = ShortInt;
  TcrInt16u   = Word;
  PcrInt16u   = ^TcrInt16u;
  TcrInt16s   = SmallInt;
  PcrInt16s   = ^TcrInt16s;
  TcrInt32u   = DWORD;
  TcrInt32s   = LongInt;
  PcrInt32s   = ^TcrInt32s;
  TcrBoolean  = WordBool;
  PcrBoolean  = ^TcrBoolean;
  TcrNumber   = Double;
  PcrNumber   = ^TcrNumber;
  TcrCurrency = Double;
  PcrCurrency = ^TcrCurrency;
  TcrDate     = LongInt;
  PcrDate     = ^TcrDate;
  TcrTime     = TcrInt32u;
  PcrTime     = ^TcrTime;

  TcrBooleanArray = array[0..32767 div SizeOf(TcrBoolean)] of TcrBoolean;
  PcrBooleanArray = ^TcrBooleanArray;

  PSmallInt = ^SmallInt;
  PDateTime = ^TDateTime;
  PDouble   = ^Double;

  TFieldValueType = (ftInt8sField,
                     ftInt8uField,
                     ftInt16sField,
                     ftInt16uField,
                     ftInt32sField,
                     ftInt32uField,
                     ftNumberField,
                     ftCurrencyField,
                     ftBooleanField,
                     ftDateField,
                     ftTimeField,
                     ftStringField,
                     ftTransientMemoField,
                     ftPersistentMemoField,
                     ftBlobField,
                     ftUnknownField);

const
  NULL_BRAHMA_DATE : TcrDate = -1;
  NULL_BRAHMA_TIME : TcrTime = -1;

  NUMBER_SCALING_FACTOR : TcrNumber = 100.0;

  SIZEOF_DATETIME_FIELD_STRING = 22; { YYYY/MM/DD HH:MM:SS.mm }

  { temporary, for debugging purposes }
  FieldValueTypes: array[TFieldValueType] of string[20] =
                   ('Int8sField',
                    'Int8uField',
                    'Int16sField',
                    'Int16uField',
                    'Int32sField',
                    'Int32uField',
                    'NumberField',
                    'CurrencyField',
                    'BooleanField',
                    'DateField',
                    'TimeField',
                    'StringField',
                    'TransientMemoField',
                    'PersistentMemoField',
                    'BlobField',
                    'UnknownField');

implementation

end.
