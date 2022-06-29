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

{ $Id: DataEditorLib.pas,v 1.42 2002/12/30 09:32:10 laa Exp $}

unit DataEditorLib;

interface
{$i common.inc}

uses
{$ifndef LINUX}
  Windows,
{$else LINUX}
  Types,
{$endif LINUX}
  DataTypes, DataType, DataElements;

type
  TPasteRules = (sprAdd, sprReplace, sprSkipIfExist, sprEnterAsNewRow, sprDeleteAll);
  TPasteRulesSet = set of TPasteRules;
  TUpdateType = (utCell, utRow, utCol, utRowCol, utAll);
  TEditTypes = (etAddRow, etDuplicateRow, etDeleteRow);
  TEditState = set of TEditTypes;

  {/** Rearrange the coordinates in ARect so they go from smaller to bigger */}
  function ArrangeRect(ARect : TRect) : TRect;
  {/** Set the value of aField in aDataRow to Value, or reset if Reset = True */}
  function SetOrResetFieldValue(aField : TDataField; aDataRow : TAbstractRow;
                  Value : TValue; SetAction : TSetAction; Reset : Boolean) : TSetResult;
  {/** Get the value of aField in aDataRow and put it in Value */}
(*  procedure GetFieldValue(aField : TDataField; aDataRow : TAbstractRow; var Value : TValue;
                  KeyFormatting : TDisplayValues);*)
  {Compare value in TCellContent with field}
  function CompareFieldValue(DataRow : TAbstractRow; Field : TDataField;
        DataType : TDataType; Value : TValue; KeyFormatting : TDisplayValues) : Boolean;
  {Is DataField the level this SubTotalRow}
//  function IsHierarchyField(SubTotalRow : TSubTotalRow; DataField : TDataField) : Boolean;
  {Does this AbstractRow contain DataField}
  function RowHasField(ARow : TAbstractRow; DataField : TDataField) : Boolean;
  {Does this SubTotalRow contain DataField as a Key}
//  function SubtotalRowHasKey(SubTotalRow : TSubTotalRow; DataField : TDataField) : Boolean;
  {Does this AbstractRow contain DataField as a Key}
  function RowHasKey(ARow : TAbstractRow; DataField : TDataField) : Boolean;

var
  { MsgArea is the fifth field and can be 12 charactrs long.
  These are automaticly added to the checklistbox}
  PasteRuleTexts : Array[Low(TPasteRules)..High(TPasteRules)] of string =
  ( 'Add values',                         //    sprAdd
    'Replace values',                     //    sprReplace
    'Skip if row exists',                 //    sprSkipIfExist
    'Enter as new row',                    //    sprEnterAsNewRow
    'Delete before paste');               //    sprDeleteAll

implementation

uses
  Math, SysUtils,
  CalcField;

const
  MSGE_NoSuchType = 'No such type!';

function ArrangeRect(ARect : TRect) : TRect;
begin
  if (ARect.Bottom = -1) or
     (ARect.Top = -1) or
     (ARect.Left = -1) or
     (ARect.Right = -1) then
    Result := ARect
  else
  begin
    Result.Left := MinIntValue([ARect.Left, ARect.Right]);
    Result.Right := MaxIntValue([ARect.Left, ARect.Right]);
    Result.Top := MinIntValue([ARect.Bottom, ARect.Top]);
    Result.Bottom := MaxIntValue([ARect.Bottom, ARect.Top]);
  end;
end;

function SetOrResetFieldValue(aField : TDataField; aDataRow : TAbstractRow;
                    Value : TValue; SetAction : TSetAction; Reset : Boolean) : TSetResult;
begin
  Assert(aDataRow <> nil, 'SetFieldValue: You have to supply a DataRow!');

  if Reset then
    Value := aField.DataType.DefaultValue;

  Result := aDataRow.SetFieldValue(aField, Value, SetAction);
end;

procedure GetFieldValue(aField : TDataField; aDataRow : TAbstractRow;
                var Value : TValue; KeyFormatting : TDisplayValues);
var
  IsStringType, IsDateOrBooleanType, IsDateOrBooleanSpecialFormatting, MABKludge : Boolean;
begin
  IsStringType := (aField.DataType is TStringType);
  IsDateOrBooleanType := (aField.DataType is TDateTimeType) or (aField.DataType is TBooleanType);
  IsDateOrBooleanSpecialFormatting := IsDateOrBooleanType and (KeyFormatting <> dvKeyOnly);
  MABKludge := (aField.DataType is TObjectType) and (aField.DisplayValues = dvTextOnly);

  if ( IsStringType or IsDateOrBooleanSpecialFormatting or MABKludge ) then
    Value := ValueFromString( Trim(aDataRow.DisplayString[aField, KeyFormatting]) )
  else
    Value := aDataRow.GetFieldValue(aField);
end;

function CompareFieldValue(DataRow : TAbstractRow; Field : TDataField;
      DataType : TDataType; Value : TValue; KeyFormatting : TDisplayValues) : Boolean;
var
  AValue : TValue;
begin
  try
    GetFieldValue(Field, DataRow, AValue, KeyFormatting);
    Result := ( DataType.Compare(Value, AValue) = 0 );
  except
    Result := False;
  end;
end;

function RowHasField(ARow : TAbstractRow; DataField : TDataField) : Boolean;

  function ClosedFieldComponentsAvailable( AClosedField : TClosedField ) : Boolean;
  var
    iField : Integer;
  begin
    Result := True;
    for iField := 0 to AClosedField.FieldCount -1 do
      Result := Result and RowHasField( ARow, AClosedField.Field[iField] );
  end;

begin
  Result := False;
  if DataField.IsAggregable then
    Result := True
  else if ARow.DataTable.TableHasNonKey(DataField) then
    Result := True
  else if {IsHierarchyField(ARow, DataField) or} RowHasKey(ARow, DataField) then
    Result := True
  else if (DataField is TClosedField) and
          ClosedFieldComponentsAvailable( TClosedField(DataField) ) then
    Result := True
  else if Assigned(DataField.LookupField) and
         (DataField.LookupField <> DataField) then
    Result := RowHasField(ARow, DataField.LookupField);

  if not Result and
     Assigned(DataField.DirectAuxTableField) and
     (DataField.DirectAuxTableField <> DataField) then
    Result := RowHasField(ARow, DataField.DirectAuxTableField);

  if not Result then
    Result := (DataField is TCalcField);
end;

{function SubtotalRowHasKey(ARow : TARow; DataField : TDataField) : Boolean;
var
  iLevel, thisLevel : Integer;
begin
  Result := False;
  thisLevel := ARow.SubTotalKey.TreeKeyIndex;
  for iLevel := thisLevel -1 downto 0 do
  begin
    Result := ARow.Storage.TreeKey[iLevel].TreeKey = DataField;
    if Result then
      Break;
  end;
end;
}
function RowHasKey(ARow : TAbstractRow; DataField : TDataField) : Boolean;
var
  iLevel, thisLevel : Integer;
begin
  try
    if (ARow is TDataRow) and
       (TDataRow(ARow).Status = rsExternControlled) then
    begin
      Result := ARow.DataTable.TableHasKey( DataField );
      Exit;
    end
    else
      Result := False;

    thisLevel := ARow.ValidKeyCount;

    for iLevel := thisLevel -1 downto 0 do
    begin
      Result := ARow.Storage.TreeKey[iLevel].TreeKey = DataField;
      if Result then
        Break;
    end;
  except
    Result := False;
  end;
end;

end.


