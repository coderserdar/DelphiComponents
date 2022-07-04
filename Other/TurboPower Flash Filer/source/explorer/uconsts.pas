{*********************************************************}
{* Explorer Constants                                    *}
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

unit uconsts;

interface

uses
  messages,
  ffllbase;

const
  FileTypes        : array[TffFileType] of string[5] = ( //was string[20]
                       'Base',
                       'Index',
                       'BLOB');

  IndexTypes       : array[TffIndexType] of string[4] = ( //was string[20]
                       'Comp',
                       'User');

const
  ffeNetTimeout      = 4000;
  ffeRegistrySubKey  = '\Explorer';

const
  ffm_Close = WM_USER + $200;
    { Used to close a form when a failure occurs during FormShow. }

const
  oeFFEBaseError           = 1;
  oeInvalidFieldName       = oeFFEBaseError + 0;
  oeDuplicateFieldName     = oeFFEBaseError + 1;
  oeMissingFieldName       = oeFFEBaseError + 2;
  oeInvalidIndexName       = oeFFEBaseError + 3;
  oeDuplicateIndexName     = oeFFEBaseError + 4;
  oeMissingIndexName       = oeFFEBaseError + 5;
  oeDuplicateFileExtension = oeFFEBaseError + 6;
  oeInvalidFileExtension   = oeFFEBaseError + 7;
  oeInvalidFieldUnits      = oeFFEBaseError + 8;
  oeInvalidIndexKeyLength  = oeFFEBaseError + 9;
  oeMaximumIndexKeyLength  = oeFFEBaseError + 10;

{ Help contexts }
const
  hcMainOutline            = 110;
  hcAddDatabaseDlg         = 200;
  hcDefineNewTableDlg      = 210;
  hcRegisteredServersDlg   = 220;
  hcRedefineTableDlg       = 230;
  hcViewTableDlg           = 240;
  hcImportDataDlg          = 250;

implementation

end.

