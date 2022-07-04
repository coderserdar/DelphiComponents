{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: DataTypes.pas,v 1.48 2003/02/25 10:04:00 mvj Exp $}

unit DataTypes;

interface
{$i common.inc}

type

  TDisplayValues = (dvKeyOnly, dvKeyAndText, dvTextOnly, dvDefault);

  TSortOrder = (soAscending, soDescending);
  TRowStatus = (rsUnchanged, rsChanged, rsDeleted, rsNew, rsUnaccepted, rsExternControlled);
  TSetAction = (saDontOverwriteKeys, saOverwriteOnKeyChange);
  TSetResult = (srOk, srInvalidValue, srKeyConflict, srKeyOverwrited, srReadOnly);
  TGetAction = (gaReference, gaCopy, gaCut, gaDelete);
  TPutAction = (paDontOverwriteKeys, paOverwriteOnKeyChange, paOverwriteOnDifferingField, paAddExcept,
                paAddReplaceNone, paAddReplaceBlank, paAddReplaceNonAggregable, paInternal);
  TPutResult = (prOk, prValuesMissing, prKeyConflict, prKeyOverwrited, prIllegalKeyValue,
                prRowsAdded, prCannotAdd);
  TDistributeAction = (daFirst, daSecond, daBoth, daNone);
  TGranularity      = (tgOne, tgThousand, tgMillion);
  TRefillType = (rtNever, rtAlways, rtDependentChange);
  TConflictAction = (caCurrent, caOverwrite, caUnion, caIntersection);

  TCompareResult = (crEqual, crSeparate, crOverlapping, crSubset, crSuperset);

const
  IllegalPutResults : set of TPutResult = [prValuesMissing, prKeyConflict, prIllegalKeyValue, prCannotAdd];
  IllegalSetResults : set of TSetResult = [srInvalidValue, srKeyConflict, srReadOnly];

  CommonCompareResults : set of TCompareResult = [crEqual, crOverlapping, crSubset, crSuperset];

  PutResultText : array[TPutResult] of String =
                ('Ok', 'Values missing', 'Key conflict', 'Key overwrited', 'Illegal key value',
                 'Rows added', 'Cannot add');
  DisplayValueText : array[TDisplayValues] of string =
                 ('KeyOnly', 'KeyAndText', 'TextOnly', 'Default');

{  GranularityTexts :  array[TGranularity] of string = ('One', 'Thousand', 'Million');
  GranularityShortTexts :  array[TGranularity] of string = ('', 'K', 'M'); }
  GranularityIdentifiers :  array[TGranularity] of string = ('ONE', 'KILO', 'MEGA');
  GranularityDivisors : array[TGranularity] of cardinal = (1,1000,1000000);

type
  TAddAction = (aaExceptOnDifferentValues, aaReplaceNone, aaReplaceBlank,
                aaReplaceNonAggregable, aaReplaceAllValues);
  TChangeType = (kcNewRow, kcKeyUpdate, kcDeletedRow, kcVisibility);

  TIntervalIndex = (iiLow, iiHigh);

  TLogType = (ltInformation, ltError, ltWarning );

  TOperationType = (otNone, otLoad, otCalcOperation, otClear, otSave, otOther);

implementation

end.










