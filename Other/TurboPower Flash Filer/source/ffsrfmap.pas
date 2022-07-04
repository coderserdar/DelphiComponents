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

{$I ffdefine.inc}

unit ffsrfmap;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffllbase,
  fflldict,
  ffsrbase,
  ffsrbde,
  fftbdict;

type
  TffSrRestructField = record
    Name: TffDictItemName;
    Number: Integer;
    Offset: Integer;
    FieldLength: Integer;
    FieldType: TffFieldType;
  end;

  TffSrFieldMapListItem = class(TffSelfListItem)
    protected {private}
      fmPadlock : TffPadlock;
      fmSource : TffSrRestructField;
      fmTarget : TffSrRestructField;
    protected
    public
      constructor Create(aSourceField, aTargetField: TffSrRestructField);
      destructor Destroy; override;
  end;

  TffSrFieldMapList = class(TffObject)                                
    protected {private}
      FList : TffList;
      fmlPadlock : TffPadlock;
      fmlSourceDict: TffServerDataDict;
      fmlTargetDict: TffServerDataDict;
    protected
      function GetCount: Integer;
      function GetSourceField(aIndex: Integer): TffSrRestructField;
      function GetTargetField(aIndex: Integer): TffSrRestructField;
    public
      constructor Create(aSourceDict, aTargetDict: TffDataDictionary);
      destructor Destroy; override;

      function Add(aSourceFieldName, aTargetFieldName: TffShStr): TffResult;
        {-insert single source-to-target field mapping into list;
          return true on success}
      function AddStringList(aFieldMap: TffStringList): TffResult;
        {-given a string list containing "destfield=sourcefield" entries,
          populates the field map with structured field entries}

      property Count: Integer
        {-the number of items in the list}
         read GetCount;

      property SourceDict: TffServerDataDict
        {-provides access to the source table's dictionary}
        read fmlSourceDict;

      property SourceField[aIndex: Integer]: TffSrRestructField
        {-returns the field info for a source field}
        read GetSourceField;

      property TargetDict: TffServerDataDict
        {-provides access to the target table's dictionary}
        read fmlTargetDict;

      property TargetField[aIndex: Integer]: TffSrRestructField
        {-returns the field info for a target field}
        read GetTargetField;
  end;

  function FFBuildFieldMapEntry(aFieldName: TffDictItemName;
                                aDictionary: TffServerDataDict;
                            var aFieldEntry: TffSrRestructField): Boolean;

implementation

{===TffSrFieldMapListItem============================================}
constructor TffSrFieldMapListItem.Create(aSourceField,
                                         aTargetField: TffSrRestructField);
begin
  inherited Create;
  fmPadlock := TffPadlock.Create;
  fmSource := aSourceField;
  fmTarget := aTargetField;
end;
{--------}
destructor TffSrFieldMapListItem.Destroy;
begin
  fmPadlock.Free;
  inherited Destroy;
end;
{====================================================================}


{===TffSrFieldMapLList===============================================}
constructor TffSrFieldMapList.Create(aSourceDict, aTargetDict: TffDataDictionary);
begin
  inherited Create;
  FList := TffList.Create;
  fmlPadlock := TffPadlock.Create;
  fmlSourceDict := TffServerDataDict.Create(4096);
  fmlTargetDict := TffServerDataDict.Create(4096);

  fmlSourceDict.Assign(aSourceDict);
  fmlTargetDict.Assign(aTargetDict);
end;
{--------}
destructor TffSrFieldMapList.Destroy;
begin
  fmlSourceDict.Free;
  fmlTargetDict.Free;
  fmlPadlock.Free;
  FList.Free;
  inherited Destroy;
end;
{--------}
function TffSrFieldMapList.Add(aSourceFieldName,
                               aTargetFieldName: TffShStr): TffResult;
var
  Item: TffSrFieldMapListItem;
  SourceField, TargetField: TffSrRestructField;
begin
  Result := DBIERR_NONE;
  fmlPadlock.Lock;
  try
    if aSourceFieldName = '' then
      aSourceFieldName := aTargetFieldName;

    { Build packet of info about the source field }
    with fmlSourceDict do begin
      SourceField.Name := aSourceFieldName;
      SourceField.Number := GetFieldFromName(aSourceFieldName);
      if SourceField.Number = -1 then begin
        Result := DBIERR_INVALIDFIELDNAME;
        Exit;
      end;

      SourceField.Offset := FieldOffset[SourceField.Number];
      SourceField.FieldLength := FieldLength[SourceField.Number];
      SourceField.FieldType := FieldType[SourceField.Number];
    end;

    { Build packet of info about the target field }
    with fmlTargetDict do begin
      TargetField.Name := aTargetFieldName;
      TargetField.Number := GetFieldFromName(aTargetFieldName);
      if TargetField.Number = -1 then begin
        Result := DBIERR_INVALIDFIELDNAME;
        Exit;
      end;

      TargetField.Offset := FieldOffset[TargetField.Number];
      TargetField.FieldLength := FieldLength[TargetField.Number];
      TargetField.FieldType := FieldType[TargetField.Number];
    end;

    Item := TffSrFieldMapListItem.Create(SourceField, TargetField);
    try
      if not FList.Insert(Item) then
        Result := -1 {!! DBIERR_????};
    except
      Item.Free;
      raise;
    end;{try..except}
  finally
    fmlPadlock.Unlock;
  end;{try..finally}
end;
{--------}
function TffSrFieldMapList.AddStringList(aFieldMap: TffStringList): TffResult;
var
  I: Integer;
  SourceName, TargetName: TffShStr;
begin
  Result := DBIERR_NONE;
  for I := 0 to aFieldMap.Count - 1 do begin
    FFShStrSplit(aFieldMap.Strings[I], '=', TargetName, SourceName);

    Add(SourceName, TargetName); {!! check for errors }
  end;
end;
{--------}
function TffSrFieldMapList.GetCount: Integer;
begin
  Result := FList.Count;
end;
{--------}
function TffSrFieldMapList.GetSourceField(aIndex: Integer): TffSrRestructField;
begin
  Result := TffSrFieldMapListItem(FList.Items[aIndex]).fmSource;
end;
{--------}
function TffSrFieldMapList.GetTargetField(aIndex: Integer): TffSrRestructField;
begin
  Result := TffSrFieldMapListItem(FList.Items[aIndex]).fmTarget;
end;
{====================================================================}



function FFBuildFieldMapEntry(aFieldName: TffDictItemName;
                              aDictionary: TffServerDataDict;
                          var aFieldEntry: TffSrRestructField): Boolean;
begin
  Result := True;
  with aDictionary do begin
    aFieldEntry.Number := GetFieldFromName(aFieldName);
    if aFieldEntry.Number = -1 then begin
      Result := False;
      Exit;
    end;

    aFieldEntry.Offset := FieldOffset[aFieldEntry.Number];
    aFieldEntry.FieldLength := FieldLength[aFieldEntry.Number];
    aFieldEntry.FieldType := FieldType[aFieldEntry.Number];
  end;
end;

end.
