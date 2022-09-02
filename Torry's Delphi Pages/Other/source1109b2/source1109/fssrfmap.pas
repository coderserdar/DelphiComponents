{*********************************************************}
{* FlashFiler: Server Restructure Field Map              *}
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

Unit fssrfmap;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fsllbase,
  fslldict,
  fssrbase,
  fssrbde,
  fsdictserveraccess;

Type
  TfsSrcRestructField = Record
    Name: TffDictItemName;
    Number: Integer;
    Offset: Integer;
    FieldLength: Integer;
    FieldType: TfsFieldType;
  End;

  TfsSrcFieldMapListItem = Class(TfsSelfListItem)
  Protected {private}
    fmPadlock: TfsPadlock;
    fmSource: TfsSrcRestructField;
    fmTarget: TfsSrcRestructField;
  Protected
  Public
    Constructor Create(aSourceField, aTargetField: TfsSrcRestructField);
    Destructor Destroy; Override;
  End;

  TfsSrcFieldMapList = Class(TFSSpecObject)
  Protected {private}
    FList: TFSNormalList;
    fmlPadlock: TfsPadlock;
    fmlSourceDict: TFSInfoServerDict;
    fmlTargetDict: TFSInfoServerDict;
  Protected
    Function GetCount: Integer;
    Function GetSourceField(aIndex: Integer): TfsSrcRestructField;
    Function GetTargetField(aIndex: Integer): TfsSrcRestructField;
  Public
    Constructor Create(aSourceDict, aTargetDict: TFSInfoDict);
    Destructor Destroy; Override;

    Function Add(aSourceFieldName, aTargetFieldName: TffShStr): TffResult;
    {-insert single source-to-target field mapping into list;
      return true on success}
    Function AddStringList(aFieldMap: TFSSpecStringList): TffResult;
    {-given a string list containing "destfield=sourcefield" entries,
      populates the field map with structured field entries}

    Property Count: Integer
      {-the number of items in the list}
    Read GetCount;

    Property SourceDict: TFSInfoServerDict
      {-provides access to the source table's dictionary}
    Read fmlSourceDict;

    Property SourceField[aIndex: Integer]: TfsSrcRestructField
    {-returns the field info for a source field}
    Read GetSourceField;

    Property TargetDict: TFSInfoServerDict
      {-provides access to the target table's dictionary}
    Read fmlTargetDict;

    Property TargetField[aIndex: Integer]: TfsSrcRestructField
    {-returns the field info for a target field}
    Read GetTargetField;
  End;

Function FSBuildFieldMapEntry(aFieldName: TffDictItemName;
  aDictionary: TFSInfoServerDict;
  Var aFieldEntry: TfsSrcRestructField): Boolean;

Implementation

{===TfsSrcFieldMapListItem============================================}

Constructor TfsSrcFieldMapListItem.Create(aSourceField,
  aTargetField: TfsSrcRestructField);
Begin
  Inherited Create;
  fmPadlock := TfsPadlock.Create;
  fmSource := aSourceField;
  fmTarget := aTargetField;
End;
{--------}

Destructor TfsSrcFieldMapListItem.Destroy;
Begin
  fmPadlock.Free;
  Inherited Destroy;
End;
{====================================================================}

{===TfsSrcFieldMapLList===============================================}

Constructor TfsSrcFieldMapList.Create(aSourceDict, aTargetDict: TFSInfoDict);
Begin
  Inherited Create;
  FList := TFSNormalList.Create;
  fmlPadlock := TfsPadlock.Create;
  fmlSourceDict := TFSInfoServerDict.Create(4096);
  fmlTargetDict := TFSInfoServerDict.Create(4096);

  fmlSourceDict.Assign(aSourceDict);
  fmlTargetDict.Assign(aTargetDict);
End;
{--------}

Destructor TfsSrcFieldMapList.Destroy;
Begin
  fmlSourceDict.Free;
  fmlTargetDict.Free;
  fmlPadlock.Free;
  FList.Free;
  Inherited Destroy;
End;
{--------}

Function TfsSrcFieldMapList.Add(aSourceFieldName,
  aTargetFieldName: TffShStr): TffResult;
Var
  Item: TfsSrcFieldMapListItem;
  SourceField, TargetField: TfsSrcRestructField;
Begin
  Result := DBIERR_NONE;
  fmlPadlock.Lock;
  Try
    If aSourceFieldName = '' Then
      aSourceFieldName := aTargetFieldName;

    { Build packet of info about the source field }
    With fmlSourceDict Do
      Begin
        SourceField.Name := aSourceFieldName;
        SourceField.Number := GetFieldFromName(aSourceFieldName);
        If SourceField.Number = -1 Then
          Begin
            Result := DBIERR_INVALIDFIELDNAME;
            Exit;
          End;

        SourceField.Offset := FieldOffset[SourceField.Number];
        SourceField.FieldLength := FieldLength[SourceField.Number];
        SourceField.FieldType := FieldType[SourceField.Number];
      End;

    { Build packet of info about the target field }
    With fmlTargetDict Do
      Begin
        TargetField.Name := aTargetFieldName;
        TargetField.Number := GetFieldFromName(aTargetFieldName);
        If TargetField.Number = -1 Then
          Begin
            Result := DBIERR_INVALIDFIELDNAME;
            Exit;
          End;

        TargetField.Offset := FieldOffset[TargetField.Number];
        TargetField.FieldLength := FieldLength[TargetField.Number];
        TargetField.FieldType := FieldType[TargetField.Number];
      End;

    Item := TfsSrcFieldMapListItem.Create(SourceField, TargetField);
    Try
      If Not FList.Insert(Item) Then
        Result := -1 {!! DBIERR_????};
    Except
      Item.Free;
      Raise;
    End; {try..except}
  Finally
    fmlPadlock.Unlock;
  End; {try..finally}
End;
{--------}

Function TfsSrcFieldMapList.AddStringList(aFieldMap: TFSSpecStringList): TffResult;
Var
  I: Integer;
  SourceName, TargetName: TffShStr;
Begin
  Result := DBIERR_NONE;
  For I := 0 To aFieldMap.Count - 1 Do
    Begin
      FFShStrSplit(aFieldMap.Strings[I], '=', TargetName, SourceName);

      Add(SourceName, TargetName); {!! check for errors }
    End;
End;
{--------}

Function TfsSrcFieldMapList.GetCount: Integer;
Begin
  Result := FList.Count;
End;
{--------}

Function TfsSrcFieldMapList.GetSourceField(aIndex: Integer): TfsSrcRestructField;
Begin
  Result := TfsSrcFieldMapListItem(FList.Items[aIndex]).fmSource;
End;
{--------}

Function TfsSrcFieldMapList.GetTargetField(aIndex: Integer): TfsSrcRestructField;
Begin
  Result := TfsSrcFieldMapListItem(FList.Items[aIndex]).fmTarget;
End;
{====================================================================}

Function FSBuildFieldMapEntry(aFieldName: TffDictItemName;
  aDictionary: TFSInfoServerDict;
  Var aFieldEntry: TfsSrcRestructField): Boolean;
Begin
  Result := True;
  With aDictionary Do
    Begin
      aFieldEntry.Number := GetFieldFromName(aFieldName);
      If aFieldEntry.Number = -1 Then
        Begin
          Result := False;
          Exit;
        End;

      aFieldEntry.Offset := FieldOffset[aFieldEntry.Number];
      aFieldEntry.FieldLength := FieldLength[aFieldEntry.Number];
      aFieldEntry.FieldType := FieldType[aFieldEntry.Number];
    End;
End;

End.

