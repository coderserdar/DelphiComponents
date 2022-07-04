{*********************************************************}
{* Classes for table field/index lists                   *}
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

unit uelement;

interface

uses
  ffllbase,
  fflldict,
  ubase,
  Classes,
  SysUtils;

type
  TffeScratchDict = class(TffDataDictionary)
  public
    function CreateFieldDesc(const aIdent    : TffDictItemName;
                             const aDesc     : TffDictItemDesc;
                                   aType     : TffFieldType;
                                   aUnits    : Integer;
                                   aDecPl    : Integer;
                                   aReqFld   : Boolean;
                             const aValCheck : TffVCheckDescriptor) : PffFieldDescriptor;
  end;

  TffeBaseListItem = class(TffListItem)
    protected
    public
      Name: TffDictItemName;
      {$IFDEF DefeatWarnings}
      function Compare(aKey : Pointer): Integer; override;
      function Key: Pointer; override;
      {$ENDIF}
  end;

  TffeBaseList = class(TffList)
    protected
      function GetItem(aIndex: LongInt): TffeBaseListItem;
    public
      constructor Create;
      procedure Exchange(aIndex1, aIndex2: LongInt);
      function IndexOf(aElementName: TffDictItemName): LongInt;
      function InsertAt(aIndex: LongInt; aItem: TffeBaseListItem): Boolean;

      property Items[aIndex: LongInt]: TffeBaseListItem
        read GetItem;
  end;

  TffeFieldListItem = class(TffeBaseListItem)
    protected { private }
    protected
      function GetFieldType: TffFieldType;
    public
      fiDataTypeIndex : Integer;
      fiUnits         : Word;
      fiDecPlaces     : Word;
      fiRequired      : Boolean;
      fiDescription   : TffDictItemDesc;
      fiSize          : Word;
      fiValCheck      : TffVCheckDescriptor;

      constructor Create;
      procedure CalcActualValues;
      {- Use the DataDictionary to compute actual Units, Dec Pl, and Size }
      property FieldType: TffFieldType
        read GetFieldType;
  end;

  TffeFieldList = class(TffeBaseList)
    private
    protected
      function GetItem(aIndex: LongInt): TffeFieldListItem;
    public
      function AddEmpty: Boolean;
      function Insert(aName     : TffDictItemName;
                      aType     : Integer;
                      aUnits    : Word;
                      aDecPl    : Word;
                      aRequired : Boolean;
                      aDesc     : TffShStr;
                      aValCheck : PffVCheckDescriptor): Boolean;
      function InsertEmpty(aIndex: LongInt): Boolean;
      property Items[aIndex: LongInt]: TffeFieldListItem
        read GetItem;
  end;

  TffeIndexListItem = class(TffeBaseListItem)
    protected {private}
      FFields: TStringList;  { List of field names comprising this key }
    protected
      function GetBlockSize: Integer;
      function GetFieldCount: Integer;
      function GetFieldName(aIndex: Integer): TffDictItemName;
      procedure SetFieldName(aIndex: Integer; const Value: TffDictItemName);
    public
      iiKeyTypeIndex: Integer; {-1 = Undefined, 0 = Composite, 1 = User-Defined}
      iiKeyLen: SmallInt;
      iiUnique: Boolean;
      iiAscending: Boolean;
      iiCaseSensitive: Boolean;
      iiExtension: TffExtension;
      iiBlockSizeIndex: Integer; {-1 = Undefined, 0,1,2,3 = 4096, 8148, 16384, 32768}
      iiDescription: TffDictItemDesc;

      constructor Create;
      destructor Destroy; override;
      procedure AddField(aFieldName: TffDictItemName);
      procedure DeleteField(aFieldName: TffDictItemName);
      procedure ExchangeFields(aFieldName1, aFieldName2: TffDictItemName);

      property BlockSize: Integer
        read GetBlockSize;
      property FieldCount: Integer
        read GetFieldCount;
      property FieldName[aIndex: Integer]: TffDictItemName
        read GetFieldName
        write SetFieldName;
  end;

  TffeIndexList = class(TffeBaseList)
    private
    protected
      function GetItem(aIndex: LongInt): TffeIndexListItem;
    public
      function AddEmpty: Boolean;
      function FieldInUse(aFieldName: TffDictItemName): Integer;
      function Insert(aName: TffDictItemName;
                      aKeyTypeIndex: Integer;
                      aKeyLen: Integer;
                      aUnique: Boolean;
                      aAscending: Boolean;
                      aCaseSensitive: Boolean;
                      aExt: TffExtension;
                      aBlockSize: Integer;
                      aDesc: TffShStr): Boolean;
      function InsertEmpty(aIndex: LongInt): Boolean;
      procedure LoadFromDict(aDictionary: TffDataDictionary);
      property Items[aIndex: LongInt]: TffeIndexListItem
        read GetItem;
  end;

const
  ktComposite = 0;
  ktUserDefined = 1;

implementation

var
  ScratchDict: TffeScratchDict;

{=====TffeScratchDict methods=====}

function TffeScratchDict.CreateFieldDesc(const aIdent    : TffDictItemName;
                                         const aDesc     : TffDictItemDesc;
                                               aType     : TffFieldType;
                                               aUnits    : Integer;
                                               aDecPl    : Integer;
                                               aReqFld   : Boolean;
                                         const aValCheck : TffVCheckDescriptor) : PffFieldDescriptor;
begin
  { This was necessary to expose the protected method }
  Result := inherited CreateFieldDesc(aIdent, aDesc, aType, aUnits, aDecPl, aReqFld, PffVCheckDescriptor(@aValCheck));
end;

{=====TffeBaseListItem methods=====}

{$IFDEF DefeatWarnings}
function TffeBaseListItem.Compare(aKey : Pointer): Integer;
begin
  Result := 0;
end;

function TffeBaseListItem.Key: Pointer;
begin
  Result := nil;
end;
{$ENDIF}


{=====TffeBaseList methods=====}

constructor TffeBaseList.Create;
begin
  inherited Create;
  Sorted := False;
end;

procedure TffeBaseList.Exchange(aIndex1, aIndex2: LongInt);
var
  Temp: Pointer;
begin
  if not Sorted then begin
    Temp := fflList[aIndex1];
    fflList[aIndex1] := fflList[aIndex2];
    fflList[aIndex2] := Temp;
  end;
end;

function TffeBaseList.GetItem(aIndex: LongInt): TffeBaseListItem;
begin
  Result := TffeBaseListItem(inherited Items[aIndex]);
end;

function TffeBaseList.IndexOf(aElementName: TffDictItemName): LongInt;
var
  I: LongInt;
begin
  Result := -1;
  aElementName := ANSIUppercase(aElementName);
  for I := 0 to Count - 1 do
    if ANSIUppercase(Items[I].Name) = aElementName then begin
      Result := I;
      Exit;
    end;
end;

function TffeBaseList.InsertAt(aIndex: LongInt; aItem: TffeBaseListItem): Boolean;
begin
  Result := False;
  if not Sorted then begin
    if aIndex < Count then begin
      Result := Insert(aItem);
      Move(fflList^[aIndex],
           fflList^[aIndex + 1],
           SizeOf(fflList^[0]) * ((Count - 1) - aIndex));           {!!.55}
      fflList[aIndex] := aItem;
    end;
  end
end;

{=====TffeFieldListItem methods=====}

constructor TffeFieldListItem.Create;
begin
  inherited Create;
  Name := '';
  fiDataTypeIndex := -1;
  fiUnits := 0;
  fiDecPlaces := 0;
  fiRequired := False;
  fiDescription := '';
end;

procedure TffeFieldListItem.CalcActualValues;
var
  FldCheck : TffVCheckDescriptor;
  FldDesc  : PffFieldDescriptor;
begin
  FldCheck.vdHasDefVal := False;

  { Compute the actual size, units, and dec pl for this field type }
  FldDesc := ScratchDict.CreateFieldDesc(Name, fiDescription, FieldType,
                                         fiUnits, fiDecPlaces, fiRequired, FldCheck);
  try
    fiSize := FldDesc^.fdLength;
    fiUnits := FldDesc^.fdUnits;
    fiDecPlaces := FldDesc^.fdDecPl;
  finally
    FFFreeMem(FldDesc, SizeOf(TffFieldDescriptor));
  end;
end;

function TffeFieldListItem.GetFieldType: TffFieldType;
begin
  Result := fftBoolean;
  if fiDataTypeIndex <> -1 then
    Result := FFEIndexToFieldType(fiDataTypeIndex);
end;

{=====TffeFieldList methods=====}

function TffeFieldList.AddEmpty: Boolean;
begin
  Result := inherited Insert(TffeFieldListItem.Create);
end;

function TffeFieldList.Insert(aName     : TffDictItemName;
                              aType     : Integer;
                              aUnits    : Word;
                              aDecPl    : Word;
                              aRequired : Boolean;
                              aDesc     : TffShStr;
                              aValCheck : PffVCheckDescriptor): Boolean;
var
  Item: TffeFieldListItem;
begin
  Item := TffeFieldListItem.Create;
  with Item do begin
    Name := aName;
    fiDataTypeIndex := aType;
    fiUnits := aUnits;
    fiDecPlaces := aDecPl;
    fiRequired := aRequired;
    fiDescription := aDesc;
    if Assigned(aValCheck) then
      fiValCheck := aValCheck^;
    CalcActualValues;
  end;

  Result := inherited Insert(Item);
end;

function TffeFieldList.InsertEmpty(aIndex: LongInt): Boolean;
begin
  Result := InsertAt(aIndex, TffeFieldListItem.Create);
end;

function TffeFieldList.GetItem(aIndex: LongInt): TffeFieldListItem;
begin
  Result := nil;
  if aIndex < Count then
    Result := TffeFieldListItem(inherited Items[aIndex]);
end;

{=====TffeIndexListItem methods=====}

constructor TffeIndexListItem.Create;
begin
  inherited Create;
  Name := '';
  iiKeyTypeIndex := ktComposite;
  iiKeyLen := 0;
  iiUnique := False;
  iiAscending := True;
  iiCaseSensitive := False;
  iiExtension := '';
  iiBlockSizeIndex := -1;
  iiDescription := '';
  FFields := TStringList.Create;
end;

destructor TffeIndexListItem.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

procedure TffeIndexListItem.AddField(aFieldName : TffDictItemName);
begin
  if (Name <> '') then begin
    if (FieldCount >= ffcl_MaxIndexFlds) then                          {!!.05}
      raise Exception.CreateFmt('Maximum of %d fields per composite index',
                                [ffcl_MaxIndexFlds]);

    FFields.Add(aFieldName);
  end;
end;

procedure TffeIndexListItem.DeleteField(aFieldName: TffDictItemName);
var
  I: LongInt;
begin
  I := FFields.IndexOf(aFieldName);
  if I <> -1 then
    FFields.Delete(I);
end;

procedure TffeIndexListItem.ExchangeFields(aFieldName1,
                                           aFieldName2 : TffDictItemName);
begin
  with FFields do
    Exchange(IndexOf(aFieldName1),IndexOf(aFieldName2));
end;

function TffeIndexListItem.GetBlockSize: Integer;
begin
  Result := 0;
  if iiBlockSizeIndex > -1 then
    Result := (1 shl iiBlockSizeIndex) shl 12;
end;

function TffeIndexListItem.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TffeIndexListItem.GetFieldName(aIndex: Integer): TffDictItemName;
begin
  Result := '';
  if aIndex < FFields.Count then
    Result := FFields[aIndex];
end;

procedure TffeIndexListItem.SetFieldName(aIndex: Integer;
  const Value: TffDictItemName);
begin
  FFields.Delete(aIndex);
  FFields.Insert(aIndex, Value);
end;

{=====TffeIndexList methods=====}

function TffeIndexList.AddEmpty: Boolean;
begin
  Result := inherited Insert(TffeIndexListItem.Create);
end;

function TffeIndexList.FieldInUse(aFieldName: TffDictItemName): Integer;
var
  F: Integer;
begin
  for Result := 0 to Count - 1 do
    with Items[Result] do
      for F := 0 to FieldCount do
        if FFCmpShStr(FieldName[F], aFieldName, 255) = 0 then
          Exit;
  Result := -1;
end;

function TffeIndexList.Insert(aName: TffDictItemName;
                              aKeyTypeIndex: Integer;
                              aKeyLen: Integer;
                              aUnique: Boolean;
                              aAscending: Boolean;
                              aCaseSensitive: Boolean;
                              aExt: TffExtension;
                              aBlockSize: Integer;
                              aDesc: TffShStr): Boolean;
var
  Item: TffeIndexListItem;
begin
  Item := TffeIndexListItem.Create;
  with Item do begin
    Name := aName;
    iiKeyTypeIndex := aKeyTypeIndex;
    iiKeyLen := aKeyLen;
    iiUnique := aUnique;
    iiAscending := aAscending;
    iiCaseSensitive := aCaseSensitive;
    iiExtension := aExt;
    iiBlockSizeIndex := FFEBlockSizeIndex(aBlockSize);
    iiDescription := aDesc;
  end;
  Result := inherited Insert(Item);
end;

function TffeIndexList.InsertEmpty(aIndex: LongInt): Boolean;
begin
  Result := InsertAt(aIndex, TffeIndexListItem.Create);
end;

function TffeIndexList.GetItem(aIndex: LongInt): TffeIndexListItem;
begin
  Result := nil;
  if aIndex < Count then
    Result := TffeIndexListItem(inherited Items[aIndex]);
end;

procedure TffeIndexList.LoadFromDict(aDictionary: TffDataDictionary);
var
  I, J: Integer;
  KeyTypeIndex: Integer;
  FileExtension: TffExtension;
  FileBlock: Integer;
begin
  with aDictionary do begin
    Empty;
    for I := 0 to IndexCount - 1 do begin
      with IndexDescriptor[I]^ do begin
        if idCount = -1 then
          KeyTypeIndex := ktUserDefined
        else
          KeyTypeIndex := ktComposite;

        FileExtension := FileExt[idFile];
        FileBlock := FileBlockSize[idFile];
        if idFile = 0 then begin
          FileExtension := '';
          FileBlock := -1;
        end;

        Insert(idName,
               KeyTypeIndex,
               idKeyLen,
               not idDups,
               idAscend,
               not idNoCase,
               FileExtension,
               FileBlock,
               idDesc);

        case KeyTypeIndex of
          ktComposite: { Get the fields, in order, that make up this index }
             for J := 0 to idCount - 1 do
               Items[IndexOf(idName)].AddField(FieldName[idFields[J]]);
        end;
      end;
    end;
  end;
end;

end.

