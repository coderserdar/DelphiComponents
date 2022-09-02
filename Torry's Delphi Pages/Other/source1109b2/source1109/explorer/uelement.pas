{$I fsdefine.inc}

Unit uelement;

Interface

Uses
  fsllbase,
  fslldict,
  ubase,
  Classes,
  SysUtils,
  fsfunInterp;

Type
  TffeScratchDict = Class(TFSInfoDict)
  Public
    Function CreateFieldDesc(Const aIdent: TffDictItemName;
      Const aDesc: TffDictItemDesc;
      aType: TfsFieldType;
      aUnits: Integer;
      aDecPl: Integer;
      aReqFld: Boolean;
      Const aValCheck: TffVCheckDescriptor;
      aBlobLevelComp: TDataCompLevel;
      aDescription: TffDictDescription;
      aRound: TRound;
      aEmptyAsNull: boolean;
      aDefaultUpdate: TDefaultUpdate): PffFieldDescriptor;
  End;

  TffeBaseListItem = Class(TFSSpecListItem)
  Protected
  Public
    Name: TffDictItemName;
    {$IFDEF DefeatWarnings}
    Function Compare(aKey: Pointer): Integer; Override;
    Function Key: Pointer; Override;
    {$ENDIF}
  End;

  TffeBaseList = Class(TFSNormalList)
  Protected
    Function GetItem(aIndex: LongInt): TffeBaseListItem;
  Public
    Constructor Create;
    Procedure Exchange(aIndex1, aIndex2: LongInt);
    Function IndexOf(aElementName: TffDictItemName): LongInt;
    Function InsertAt(aIndex: LongInt; aItem: TffeBaseListItem): Boolean;

    Property Items[aIndex: LongInt]: TffeBaseListItem
    Read GetItem;
  End;

  TffeFieldListItem = Class(TffeBaseListItem)
  Protected { private }
  Protected
  Public
    fiDataType: TfsFieldType;
    fiUnits: integer;
    fiDecPlaces: integer;
    fiRequired: Boolean;
    fiDisplay: TffDictItemDesc;
    fiSize: integer;
    fiValCheck: TffVCheckDescriptor;
    fiBlobLevelComp: TDataCompLevel;
    fiRound: TRound;
    fiDescription: TffDictDescription;
    fiEmptyAsNull: Boolean;
    fiDefaultUpdate: TDefaultUpdate;
    Constructor Create;
    Procedure CalcActualValues;
    {- Use the DataDictionary to compute actual Units, Dec Pl, and Size }
    Property FieldType: TfsFieldType
      Read fiDataType;

  End;

  TffeFieldList = Class(TffeBaseList)
  Private
  Protected
    Function GetItem(aIndex: LongInt): TffeFieldListItem;
  Public
    Function AddEmpty: Boolean;
    Function Insert(aName: TffDictItemName;
      aType: TfsFieldType;
      aUnits: Integer;
      aDecPl: Integer;
      aRequired: Boolean;
      aDesc: TffShStr;
      aValCheck: PffVCheckDescriptor;
      aBlobLevelComp: TDataCompLevel;
      aDescription: TffDictDescription;
      aRound: TRound; aEmptyAsNull: boolean; aDefaultUpdate: TDefaultUpdate): Boolean;
    Function InsertEmpty(aIndex: LongInt): Boolean;
    Property Items[aIndex: LongInt]: TffeFieldListItem
    Read GetItem;
  End;

  TffeIndexListItem = Class(TffeBaseListItem)
  Protected {private}
    FFields: TStringList; { List of field names comprising this key }
    FFieldsCase: TStringList;
    fFieldsSize: TStringList;
    fFieldsNullTop: TStringList;
  Protected
    Function GetBlockSize: Integer;
    Function GetFieldCount: Integer;
    Function GetFieldName(aIndex: Integer): TffDictItemName;
    Procedure SetFieldName(aIndex: Integer; Const Value: TffDictItemName);
  Public
    iiKeyTypeIndex: Integer; {-1 = Undefined, 0 = Composite, 1 = User-Defined}
    iiKeyLen: SmallInt;
    iiUnique: Boolean;
    iiExtension: TffExtension;
    iiBlockSizeIndex: Integer; {-1 = Undefined, 0,1,2,3 = 4096, 8148, 16384, 32768}
    iiDescription: TffDictItemDesc;

    Constructor Create;
    Destructor Destroy; Override;
    Procedure AddField(aFieldName: TffDictItemName; ISort, ICase, iSize, iNullTop: Longint);
    Procedure DeleteField(aFieldName: TffDictItemName);
    Procedure ExchangeFields(aFieldName1, aFieldName2: TffDictItemName);

    Property BlockSize: Integer
      Read GetBlockSize;
    Property FieldCount: Integer
      Read GetFieldCount;
    Property FieldName[aIndex: Integer]: TffDictItemName
    Read GetFieldName
      Write SetFieldName;
    Property Fields: TStringList Read FFields Write FFields;
    Property FieldsCase: TStringList Read FFieldsCase Write FFieldsCase;
    Property FieldsSize: TStringList Read FFieldsSize Write FFieldsSize;
    Property FieldsNullTop: TStringList Read FFieldsNullTop Write FFieldsNullTop;
  End;

  TffeIndexList = Class(TffeBaseList)
  Private
  Protected
    Function GetItem(aIndex: LongInt): TffeIndexListItem;
  Public
    Function AddEmpty: Boolean;
    Function FieldInUse(aFieldName: TffDictItemName): Integer;
    Function Insert(aName: TffDictItemName;
      aKeyTypeIndex: Integer;
      aKeyLen: Integer;
      aUnique: Boolean;
      aExt: TffExtension;
      aBlockSize: Integer;
      aDesc: TffShStr): Boolean;
    Function InsertEmpty(aIndex: LongInt): Boolean;
    Procedure LoadFromDict(aDictionary: TFSInfoDict);
    Property Items[aIndex: LongInt]: TffeIndexListItem
    Read GetItem;
  End;

Const
  ktComposite = 0;
  ktUserDefined = 1;

Implementation

Var
  ScratchDict: TffeScratchDict;

  {=====TffeScratchDict methods=====}

Function TffeScratchDict.CreateFieldDesc(Const aIdent: TffDictItemName;
  Const aDesc: TffDictItemDesc;
  aType: TfsFieldType;
  aUnits: Integer;
  aDecPl: Integer;
  aReqFld: Boolean;
  Const aValCheck: TffVCheckDescriptor;
  aBlobLevelComp: TDataCompLevel;
  aDescription: TffDictDescription;
  aRound: TRound;
  aEmptyAsNull: boolean;
  aDefaultUpdate: TDefaultUpdate): PffFieldDescriptor;
Begin
  { This was necessary to expose the protected method }
  Result := Inherited CreateFieldDesc(aIdent, aDesc, aType, aUnits, aDecPl, aReqFld,
    PffVCheckDescriptor(@aValCheck), aBlobLevelComp, aDescription, aRound, aEmptyAsNull, aDefaultUpdate);
End;

{=====TffeBaseListItem methods=====}

{$IFDEF DefeatWarnings}

Function TffeBaseListItem.Compare(aKey: Pointer): Integer;
Begin
  Result := 0;
End;

Function TffeBaseListItem.Key: Pointer;
Begin
  Result := Nil;
End;
{$ENDIF}

{=====TffeBaseList methods=====}

Constructor TffeBaseList.Create;
Begin
  Inherited Create;
  Sorted := False;
End;

Procedure TffeBaseList.Exchange(aIndex1, aIndex2: LongInt);
Var
  Temp: Pointer;
Begin
  If Not Sorted Then
    Begin
      Temp := fflList[aIndex1];
      fflList[aIndex1] := fflList[aIndex2];
      fflList[aIndex2] := Temp;
    End;
End;

Function TffeBaseList.GetItem(aIndex: LongInt): TffeBaseListItem;
Begin
  Result := TffeBaseListItem(Inherited Items[aIndex]);
End;

Function TffeBaseList.IndexOf(aElementName: TffDictItemName): LongInt;
Var
  I: LongInt;
Begin
  Result := -1;
  aElementName := ANSIUppercase(aElementName);
  For I := 0 To Count - 1 Do
    If ANSIUppercase(Items[I].Name) = aElementName Then
      Begin
        Result := I;
        Exit;
      End;
End;

Function TffeBaseList.InsertAt(aIndex: LongInt; aItem: TffeBaseListItem): Boolean;
Begin
  Result := False;
  If Not Sorted Then
    Begin
      If aIndex < Count Then
        Begin
          Result := Insert(aItem);
          Move(fflList^[aIndex],
            fflList^[aIndex + 1],
            SizeOf(fflList^[0]) * ((Count - 1) - aIndex)); {!!.55}
          fflList[aIndex] := aItem;
        End;
    End
End;

{=====TffeFieldListItem methods=====}

Constructor TffeFieldListItem.Create;
Begin
  Inherited Create;
  Name := '';
  fiDataType := fstBoolean;
  fiUnits := 0;
  fiDecPlaces := 0;
  fiRequired := False;
  fiEmptyAsNull := False;
  fiDescription := '';
  fiDisplay := '';
  fiBlobLevelComp := blNone;
  fiRound := rNone;
  fiDefaultUpdate := duNormal;
End;

Procedure TffeFieldListItem.CalcActualValues;
Var
  FldCheck: TffVCheckDescriptor;
  FldDesc: PffFieldDescriptor;
Begin
  FldCheck.vdHasDefVal := False;

  { Compute the actual size, units, and dec pl for this field type }
  FldDesc := ScratchDict.CreateFieldDesc(Name, fiDisplay, fiDataType,
    fiUnits, fiDecPlaces, fiRequired, FldCheck, fiBlobLevelComp, fiDescription, fiRound, fiEmptyAsNull, fiDefaultUpdate);
  Try
    fiSize := FldDesc^.fdLength;
    fiUnits := FldDesc^.fdUnits;
    fiDecPlaces := FldDesc^.fdDecPl;
  Finally
    FFFreeMem(FldDesc, SizeOf(TffFieldDescriptor));
  End;
End;


{=====TffeFieldList methods=====}

Function TffeFieldList.AddEmpty: Boolean;
Begin
  Result := Inherited Insert(TffeFieldListItem.Create);
End;

Function TffeFieldList.Insert(aName: TffDictItemName;
  aType: TfsFieldType;
  aUnits: integer;
  aDecPl: integer;
  aRequired: Boolean;
  aDesc: TffShStr;
  aValCheck: PffVCheckDescriptor;
  aBlobLevelComp: TDataCompLevel;
  aDescription: TffDictDescription;
  aRound: TRound;
  aEmptyAsNull: boolean;
  aDefaultUpdate: TDefaultUpdate): Boolean;
Var
  Item: TffeFieldListItem;
Begin
  Item := TffeFieldListItem.Create;
  With Item Do
    Begin
      Name := aName;
      fiDataType := aType;
      fiUnits := aUnits;
      fiDecPlaces := aDecPl;
      fiRequired := aRequired;
      fiDisplay := aDesc;
      fiDescription := aDescription;
      fiBlobLevelComp := aBlobLevelComp;
      fiRound := aRound;
      fiEmptyAsNull := aEmptyAsNull;
      fiDefaultUpdate := aDefaultUpdate;
      If Assigned(aValCheck) Then
        fiValCheck := aValCheck^;
      CalcActualValues;
    End;

  Result := Inherited Insert(Item);
End;

Function TffeFieldList.InsertEmpty(aIndex: LongInt): Boolean;
Begin
  Result := InsertAt(aIndex, TffeFieldListItem.Create);
End;

Function TffeFieldList.GetItem(aIndex: LongInt): TffeFieldListItem;
Begin
  Result := Nil;
  If aIndex < Count Then
    Result := TffeFieldListItem(Inherited Items[aIndex]);
End;

{=====TffeIndexListItem methods=====}

Constructor TffeIndexListItem.Create;
Begin
  Inherited Create;
  Name := '';
  iiKeyTypeIndex := ktComposite;
  iiKeyLen := 0;
  iiUnique := False;
  iiExtension := '';
  iiBlockSizeIndex := -1;
  iiDescription := '';
  FFields := TStringList.Create;
  FFieldsCase := TStringList.Create;
  fFieldsSize := TStringList.Create;
  fFieldsNullTop := TStringList.Create;
End;

Destructor TffeIndexListItem.Destroy;
Begin
  FFields.Free;
  FFieldsCase.free;
  fFieldsSize.free;
  fFieldsNullTop.free;
  Inherited Destroy;
End;

Procedure TffeIndexListItem.AddField(aFieldName: TffDictItemName; ISort, ICase, iSize, iNullTop: Longint);
Begin
  If (Name <> '') Then
    Begin
      If (FieldCount >= fscl_MaxIndexFlds) Then
        Raise Exception.CreateFmt('Maximum of %d fields per composite index',
          [fscl_MaxIndexFlds]);

      FFields.AddObject(aFieldName, TObject(ISort));
      FFieldsCase.AddObject(aFieldName, TObject(ICase));
      fFieldsSize.AddObject(aFieldName, TObject(ISize));
      fFieldsNullTop.AddObject(aFieldName, TObject(iNullTop));
    End;
End;

Procedure TffeIndexListItem.DeleteField(aFieldName: TffDictItemName);
Var
  I: LongInt;
Begin
  I := FFields.IndexOf(aFieldName);
  If I <> -1 Then
    Begin
      FFields.Delete(I);
      FFieldsCase.Delete(I);
      fFieldsSize.Delete(I);
      fFieldsNullTop.Delete(I);
    End;
End;

Procedure TffeIndexListItem.ExchangeFields(aFieldName1,
  aFieldName2: TffDictItemName);
Begin
  FFields.Exchange(FFields.IndexOf(aFieldName1), FFields.IndexOf(aFieldName2));
  FFieldsCase.Exchange(FFieldsCase.IndexOf(aFieldName1), FFieldsCase.IndexOf(aFieldName2));
  fFieldsSize.Exchange(fFieldsSize.IndexOf(aFieldName1), fFieldsSize.IndexOf(aFieldName2));
  fFieldsNullTop.Exchange(fFieldsNullTop.IndexOf(aFieldName1), fFieldsNullTop.IndexOf(aFieldName2));
End;

Function TffeIndexListItem.GetBlockSize: Integer;
Begin
  Result := 0;
  If iiBlockSizeIndex > -1 Then
    Result := (1 Shl iiBlockSizeIndex) Shl 12;
End;

Function TffeIndexListItem.GetFieldCount: Integer;
Begin
  Result := FFields.Count;
End;

Function TffeIndexListItem.GetFieldName(aIndex: Integer): TffDictItemName;
Begin
  Result := '';
  If aIndex < FFields.Count Then
    Result := FFields[aIndex];
End;

Procedure TffeIndexListItem.SetFieldName(aIndex: Integer;
  Const Value: TffDictItemName);
Var
  i: integer;
Begin
  i := integer(FFields.Objects[aIndex]);
  FFields.Delete(aIndex);
  FFields.InsertObject(aIndex, Value, TObject(i));

  i := integer(FFieldsCase.Objects[aIndex]);
  FFieldsCase.Delete(aIndex);
  FFieldsCase.InsertObject(aIndex, Value, TObject(i));

  i := integer(fFieldsSize.Objects[aIndex]);
  fFieldsSize.Delete(aIndex);
  FFieldsSize.InsertObject(aIndex, Value, TObject(i));

  i := integer(fFieldsNullTop.Objects[aIndex]);
  fFieldsNullTop.Delete(aIndex);
  fFieldsNullTop.InsertObject(aIndex, Value, TObject(i));
End;

{=====TffeIndexList methods=====}

Function TffeIndexList.AddEmpty: Boolean;
Var
  Item: TffeIndexListItem;
Begin
  Item := TffeIndexListItem.Create;
  Result := Inherited Insert(ITEM);
End;

Function TffeIndexList.FieldInUse(aFieldName: TffDictItemName): Integer;
Var
  F: Integer;
Begin
  For Result := 0 To Count - 1 Do
    With Items[Result] Do
      For F := 0 To FieldCount Do
        If FFCmpShStr(FieldName[F], aFieldName, 255) = 0 Then
          Exit;
  Result := -1;
End;

Function TffeIndexList.Insert(aName: TffDictItemName;
  aKeyTypeIndex: Integer;
  aKeyLen: Integer;
  aUnique: Boolean;
  aExt: TffExtension;
  aBlockSize: Integer;
  aDesc: TffShStr): Boolean;
Var
  Item: TffeIndexListItem;
Begin
  Item := TffeIndexListItem.Create;
  With Item Do
    Begin
      Name := aName;
      iiKeyTypeIndex := aKeyTypeIndex;
      iiKeyLen := aKeyLen;
      iiUnique := aUnique;
      iiExtension := aExt;
      iiBlockSizeIndex := FFEBlockSizeIndex(aBlockSize);
      iiDescription := aDesc;
    End;
  Result := Inherited Insert(Item);
End;

Function TffeIndexList.InsertEmpty(aIndex: LongInt): Boolean;
Begin
  Result := InsertAt(aIndex, TffeIndexListItem.Create);
End;

Function TffeIndexList.GetItem(aIndex: LongInt): TffeIndexListItem;
Begin
  Result := Nil;
  If aIndex < Count Then
    Result := TffeIndexListItem(Inherited Items[aIndex]);
End;

Procedure TffeIndexList.LoadFromDict(aDictionary: TFSInfoDict);
Var
  I, J: Integer;
  KeyTypeIndex: Integer;
  FileExtension: TffExtension;
  FileBlock, isize: Integer;
  b, b1, b2: boolean;
Begin
  With aDictionary Do
    Begin
      Empty;
      For I := 0 To IndexCount - 1 Do
        Begin
          With IndexDescriptor[I]^ Do
            Begin
              If idCount = -1 Then
                KeyTypeIndex := ktUserDefined
              Else
                KeyTypeIndex := ktComposite;

              FileExtension := FileExt[idFile];
              FileBlock := FileBlockSize[idFile];
              If idFile = 0 Then
                Begin
                  FileExtension := '';
                  FileBlock := -1;
                End;
              Insert(idName,
                KeyTypeIndex,
                idKeyLen,
                Not idDups,
                FileExtension,
                FileBlock,
                idDesc);

              Case KeyTypeIndex Of
                ktComposite: { Get the fields, in order, that make up this index }
                  For J := 0 To idCount - 1 Do
                    Begin
                      b := boolean(idFieldsAscDesc[J]);
                      b1 := Not boolean(idFieldsCase[J]);
                      b2 := boolean(idFieldsNullTop[J]);
                      isize := idFieldsSize[J];
                      Items[IndexOf(idName)].AddField(FieldName[idFields[J]], byte(b), byte(b1), isize, byte(b2));
                    End;
              End;
            End;
        End;
    End;
End;

End.

