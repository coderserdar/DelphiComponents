unit SnapObjectDataset;

interface

uses
  Classes,
  SysUtils,
  Windows,
  DB,
  SnapBaseDataset;

Type
  TSnapFieldData = class(TCollectionItem)
  private
    fMaxValue: cardinal;
    fDataSize: integer;
    fMinValue: integer;
    fFieldName: string;
    fFieldType: TFieldType;
    //fPropInfo: PPropInfo;
    fFieldSize: integer;
    fBufferOffset: integer;
  public
    property FieldName: string read fFieldName;
    property FieldType: TFieldType read fFieldType;
    property FieldSize: integer read fFieldSize;
    property DataSize: integer read fDataSize;
    property MinValue: integer read fMinValue;
    property MaxValue: cardinal read fMaxValue;
    property BufferOffset: integer read fBufferOffset;
  end;

  TSnapFieldsData = class(TCollection)
  private
    function GetItem(const Index: integer): TSnapFieldData;
  public
    constructor Create;
    function Add: TSnapFieldData; virtual;
    function AddFieldDef(AFieldName: string;
                         AFieldType: TFieldType;
                         ADataSize: integer;
                         AFieldSize: integer = 0;
                         AMinValue: integer = 0;
                         AMaxValue: cardinal = 0): TSnapFieldData;
    function RecordDataSize: integer;
    property Items[const Index: integer]: TSnapFieldData read GetItem; default;
  end;

  TSnapOnObjectCreateEvent = procedure (Sender: TObject; var NewInstance: TObject) of Object;

  TSnapCustomObjectDataset=class(TSnapCustonIndexedDataset)
  private
    fObjectInstance: TObject;
    fObjectClass: TClass;
    fObjectClassName: string;
    fOnObjectCreate: TSnapOnObjectCreateEvent;
    fTempObject: TObject;
    fStringWidth: Integer;
    fObjectFields: TSnapFieldsData;
    procedure SetObjectClassName(const Value: string);
    procedure CheckObjectClass;
    function GetObjectInstance: TObject;
    procedure SetObjectInstance(const Value: TObject);
    procedure SetInternalObjectInstance(const Value: TObject);
    function GetObjectItem(AIndex: integer): TObject;
  protected
    function DoObjectCreate: TObject; virtual;
    function DoOpen: Boolean; override;
    procedure DoClose; override;
    procedure DoInsertRecord; override;
    procedure DoDeleteRecord; override;
    procedure DoCancelRecord; override;
    procedure DoCreateFieldDefs; override;
    procedure DoSetMasterField(Field: TField); override;
    function GetFieldValue(Field: TField): Variant; override;
    procedure SetFieldValue(Field: TField; Value: Variant); override;
    procedure GetBlobField(Field: TBlobField; Stream: TStream); override;
    procedure SetBlobField(Field: TBlobField; Stream: TStream); override;
  protected
    {Overriden datatset methods}
    function GetCanModify: Boolean; override;
    function GetRecordCount: Integer; override;
  protected
    {protected methods for this dataset}
    function GetActiveItem: TObject;
    property ActiveItem: TObject read GetActiveItem;
  public
    property ObjectInstance: TObject read GetObjectInstance write SetObjectInstance;
    property ObjectClass: TClass read fObjectClass;
    property ObjectClassName: string read fObjectClassName write SetObjectClassName;
    property OnObjectCreate: TSnapOnObjectCreateEvent read FOnObjectCreate write FOnObjectCreate;

    property StringWidth: Integer read FStringWidth write FStringWidth;

    function SupportedFieldType(AType: TFieldType): Boolean; override;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

  TSnapObjectDataset = class(TSnapCustomObjectDataset)
  published
    property ObjectClassName;
    property StringWidth;
    property MasterSource;
    property MasterAttribute;
    property Active;

    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;

    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property OnNewRecord;
  end;

const
  ftSupportedType = [ftString,
                     ftBoolean,
                     ftSmallInt,
                     ftWord,
                     ftInteger,
                     ftLargeInt,
                     ftDate,
                     ftTime,
                     ftDateTime,
                     ftFloat,
                     ftMemo,
                     ftGraphic,
                     ftDataset];
  
implementation

uses
  typinfo,
  Contnrs,
  Graphics,
  DBConsts;

{ TSnapFieldsData }

function TSnapFieldsData.Add: TSnapFieldData;
begin
  Result := TSnapFieldData(inherited Add);
end;

function TSnapFieldsData.AddFieldDef(AFieldName: string;
  AFieldType: TFieldType; ADataSize, AFieldSize, AMinValue: integer;
  AMaxValue: cardinal): TSnapFieldData;
begin
  Result := Add;
  with Result do
  begin
    fFieldName := AFieldName;
    fDataSize := ADataSize;
    fFieldSize := AFieldSize;
    fMinValue := AMinValue;
    fMaxValue := AMaxValue;
  end;
end;

constructor TSnapFieldsData.Create;
begin
  inherited Create(TSnapFieldData);
end;

function TSnapFieldsData.GetItem(const Index: integer): TSnapFieldData;
begin
  Result  := TSnapFieldData(inherited Items[Index]);
end;

function TSnapFieldsData.RecordDataSize: integer;
var
  i: integer;
begin
  Result := 0;
  for i:=0 to Pred(Count) do
    Result := Result + Items[i].DataSize;
end;

{ TCollectionDataset }

procedure TSnapCustomObjectDataset.AfterConstruction;
begin
  inherited;
  FStringWidth := 255;
  fTempObject := nil;
  fObjectFields := TSnapFieldsData.Create;
end;

procedure TSnapCustomObjectDataset.BeforeDestruction;
begin
  fObjectFields.Free;
  if Assigned(fTempObject) then
    FreeAndNil(fTempObject);
  inherited;
end;

procedure TSnapCustomObjectDataset.DoClose;
begin

end;

procedure TSnapCustomObjectDataset.DoCreateFieldDefs;
(*
var PropList: PPropList;
PropInfo:PPropInfo;
Data,Data2: PTypeData;
j: Integer;
oClass: TClass;

Begin
  FieldDefs.Clear;
  Data:=GetTypeData(FCollection.ItemClass.ClassInfo);
  GetMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
  Try
    GetPropInfos(FCollection.ItemClass.ClassInfo,PropList);
    for j:=0 to Data^.propCount-1 do
      Begin
      PropInfo:=PropList^[j];
      Case PropInfo^.PropType^.Kind of
        tkLString,tkString: FieldDefs.Add(PropInfo^.Name,ftString,StringWidth,False);
        tkInteger:  FieldDefs.Add(PropInfo^.Name,ftInteger,0,False);
        tkEnumeration,tkSet:
          Begin
          if CompareText(PropInfo^.PropType^.Name,'Boolean')=0 then FieldDefs.Add(PropInfo^.Name,ftBoolean,0,False)
          else FieldDefs.Add(PropInfo^.Name,ftInteger,0,False);
          End;
        tkClass:
          Begin
          Data2:=GetTypeData(PropInfo^.PropType^);
          oClass:=Data2.ClassType;
          if oClass=TStrings then
            FieldDefs.Add(PropInfo^.Name,ftMemo,0,False)
          else if oClass=TPicture then
            FieldDefs.Add(PropInfo^.Name,ftGraphic,0,False)
          else Raise Exception.Create(PropInfo^.Name+' is can not be made into a valid TField');
          End;
        else Raise Exception.Create(PropInfo^.Name+' is can not be made into a valid TField');
      End;
      End;
  finally
  FreeMem(PropList,Data^.PropCount*Sizeof(PPropInfo));
  End;
*)
  procedure InValidFieldMapping(PropInfo: PPropInfo);
  begin
    //raise Exception.CreateFmt('%s.%s (%s) could not be mapped into a dataset field',
    //  [ObjectClass.ClassName, PropInfo^.Name, PropInfo^.PropType^.Name]);
  end;

  procedure AddFieldDef(APropInfo: PPropInfo; AFieldType: TFieldType; ADataSize: integer;
    AFieldSize: integer = 0; AMinValue: integer = 0; AMaxValue: cardinal = 0);
  //Create internal field
  var
    oObjectField: TSnapFieldData;
  begin
    oObjectField := FObjectFields.Add;
    //oObjectField.PropInfo := APropInfo;
    oObjectField.fDataSize := ADataSize;
    oObjectField.fMinValue := AMinValue;
    oObjectField.fMaxValue := AMaxValue;
    oObjectField.fFieldSize := AFieldSize;
    oObjectField.fFieldType := AFieldType;
    oObjectField.fFieldName := APropInfo^.Name;
    //oObjectField.fBufferOffset := FRecFieldsSize;

    //Extend the buffer and reserve space for optional #0 terminator
    //Inc(fRecFieldsSize, ADataSize + integer(AFieldType=ftString));
    FieldDefs.Add(APropInfo^.Name, AFieldType, AFieldSize, False);
  end;

var
  TypeData, PropData: PTypeData;
  PropList: PPropList;
  PropInfo: PPropInfo;
  oClass: TClass;
  i: Integer;
begin
  FieldDefs.Clear;
  fObjectFields.Clear;
  //FRecFieldsSize := 0;
  if Assigned(ObjectClass) then
  begin
    PropData := GetTypeData(ObjectClass.ClassInfo);
    GetMem(PropList, PropData^.PropCount * SizeOf(PPropInfo));
    try
      GetPropInfos(ObjectClass.ClassInfo, PropList);
      for i := 0 to PropData^.propCount - 1 do
      begin
        PropInfo := PropList^[i];
        if Assigned(PropInfo^.GetProc) then
        begin
          TypeData := GetTypeData(PropInfo^.PropType^);
          case PropInfo^.PropType^.Kind of
            tkChar: AddFieldDef(PropInfo, ftString, 1,1);
            tkLString,
            tkWString: AddFieldDef(PropInfo, ftString, StringWidth, StringWidth);
            tkString: AddFieldDef(PropInfo, ftString, TypeData^.MaxLength, TypeData^.MaxLength);

            tkInteger,
            tkEnumeration:
            begin
              if CompareText(PropInfo^.PropType^.Name, 'Boolean') = 0 then
                AddFieldDef(PropInfo, ftBoolean, Sizeof(WordBool))
              else if CompareText(PropInfo^.PropType^.Name, 'ByteBool') = 0 then
                AddFieldDef(PropInfo, ftBoolean, Sizeof(WordBool))
              else if CompareText(PropInfo^.PropType^.Name, 'WordBool') = 0 then
                AddFieldDef(PropInfo, ftBoolean, Sizeof(WordBool))
              else if CompareText(PropInfo^.PropType^.Name, 'LongBool') = 0 then
                AddFieldDef(PropInfo, ftBoolean, Sizeof(WordBool))
              else
                case TypeData^.OrdType of
                  otSByte: AddFieldDef(PropInfo, ftSmallInt, Sizeof(Smallint),0,-128,127);
                  otUByte: AddFieldDef(PropInfo, ftWord, Sizeof(Word),0,0,255);
                  otSWord: AddFieldDef(PropInfo, ftSmallInt, Sizeof(SmallInt) {ok});
                  otUWord: AddFieldDef(PropInfo, ftWord, Sizeof(Word) {ok});
                  otSLong: AddFieldDef(PropInfo, ftInteger, Sizeof(Integer) {ok});
                  otULong: AddFieldDef(PropInfo, ftLargeInt, Sizeof(Int64),0,0,4294967295) //=Cardinal
                else
                  InValidFieldMapping(PropInfo);
                end;
            end;
            tkInt64: AddFieldDef(PropInfo, ftLargeInt, Sizeof(Int64){ok});
            tkSet: AddFieldDef(PropInfo, ftInteger, Sizeof(Integer));
            tkFloat:
            begin
              if CompareText(PropInfo^.PropType^.Name, 'TDate') = 0 then
                AddFieldDef(PropInfo, ftDate, Sizeof(Integer))
              else if CompareText(PropInfo^.PropType^.Name, 'TTime') = 0 then
                AddFieldDef(PropInfo, ftTime, Sizeof(Integer))
              else if CompareText(PropInfo^.PropType^.Name, 'TDateTime') = 0 then
                AddFieldDef(PropInfo, ftDateTime, Sizeof(Double))
              else
                case TypeData^.FloatType of
                  ftExtended: AddFieldDef(PropInfo, ftFloat, Sizeof(Double));
                  ftCurr: AddFieldDef(PropInfo, ftFloat, Sizeof(Double));
                  ftDouble:AddFieldDef(PropInfo, ftFloat, Sizeof(Double));//No real Db-type!
                  ftComp:AddFieldDef(PropInfo, ftFloat, Sizeof(Double));//No real Db-type!
                  ftSingle: AddFieldDef(PropInfo, ftFloat, Sizeof(Double));
                else
                  InValidFieldMapping(PropInfo);
                end;
            end;
            tkClass:
            begin
              oClass := TypeData.ClassType;
              if oClass.InheritsFrom(TStrings) then
                AddFieldDef(PropInfo, ftMemo, Sizeof(Pointer))
              else if oClass.InheritsFrom(TPicture) then
                AddFieldDef(PropInfo, ftGraphic, Sizeof(Pointer))
              else if oClass.InheritsFrom(TList) then
                AddFieldDef(PropInfo, ftDataset, Sizeof(TObject))
              else if oClass.InheritsFrom(TCollection) then
                AddFieldDef(PropInfo, ftDataset, Sizeof(TObject))
              else if oClass.InheritsFrom(TObject) then
                AddFieldDef(PropInfo, ftDataset, Sizeof(TObject));
            end;
            tkMethod:; //Ignore since we don't support it.
            else
              InValidFieldMapping(PropInfo);
          end;
        end;
      end;
    finally
      FreeMem(PropList, PropData^.PropCount * SizeOf(PPropInfo));
    end;
  end;

end;

procedure TSnapCustomObjectDataset.DoDeleteRecord;
begin
  if ActiveItem<>nil then
    ActiveItem.Free;
end;

function TSnapCustomObjectDataset.DoOpen: Boolean;
begin
  Result := False;
  CheckObjectClass;
  if inherited DoOpen then
  begin
    Result := (fObjectInstance<>nil);
    if (fObjectInstance=nil) then
      Raise Exception.Create('No ObjectInstance has been assigned to dataset');
  end;
end;

function TSnapCustomObjectDataset.GetActiveItem: TObject;
var
  n: Integer;
begin
  Result := nil;
  {
  if (FCurrent=-1) and (fObjectInstance.Count>0) then
    Result:=FCollection.Items[0];
  if (FCurrent>=0) and (FCurrent<=FCollection.Count-1) then
    Result:=FCollection.Items[FCurrent];
  }
  n := GetRecordCount;
  if (FCurrent=-1) and (n>0) then
    Result := GetObjectItem(0);
  if (FCurrent>=0) and (FCurrent<=n-1) then
    Result:=GetObjectItem(FCurrent);
end;

procedure TSnapCustomObjectDataset.GetBlobField(Field: TBlobField; Stream: TStream);
var Obj: TObject;
begin
  if ActiveItem=nil then
    exit;
  Obj := TObject(GetOrdProp(ActiveItem,Field.FieldName));
  if (Obj is TStrings) then
    TStrings(Obj).SaveToStream(Stream);
  if (Obj is TPicture) then
    if TPicture(Obj).Graphic<>nil then
      TPicture(Obj).Graphic.SaveToStream(Stream);
end;

function TSnapCustomObjectDataset.GetFieldValue(Field: TField): Variant;
var
  PropInfo: PPropInfo;
  pDest: pChar;
  Obj: TObject;
begin
  Obj := ActiveItem;
  if Obj<>nil then
  begin
    PropInfo := GetPropInfo(Obj, Field.FieldName);
    case Field.DataType of
      ftString:
      begin
        if PropInfo^.PropType^.Kind = tkChar then
          Result := Chr(GetOrdProp(Obj, Field.FieldName))
        else
          Result := GetStrProp(Obj, Field.FieldName);
        if length(Result) > Field.Size then
          Result :=Copy(Result, 1, Field.Size);
      end;
      ftInteger:
      begin
        Result := GetOrdProp(Obj, Field.FieldName);
      end;
      ftSmallInt:
      begin
        Result := GetOrdProp(Obj, Field.FieldName);
      end;
      ftWord:
      begin
        Result := GetOrdProp(Obj, Field.FieldName);
      end;
      ftLargeInt:
      begin
        //Problem:Cardinal is also edited as int64
        if PropInfo^.PropType^.Kind = tkInt64 then
          Result := GetInt64Prop(Obj, Field.FieldName)
        else
          Result := GetOrdProp(Obj, Field.FieldName);
      end;
      ftBoolean:
      begin
        Result := WordBool(GetOrdProp(Obj, Field.FieldName));
      end;
      ftFloat,
      ftCurrency:
      begin
        Result := GetFloatProp(Obj, Field.FieldName);
      end;
      ftBCD,
      ftDateTime,
      ftDate,
      ftTime,
      ftTimeStamp:
      begin
        Result := GetFloatProp(Obj, Field.FieldName);
      end;
      ftDataset:
      begin
        TVarData(Result).VType := varByRef;
        TVarData(Result).VPointer := GetObjectProp(Obj, Field.FieldName);
      end;
      else
        DatabaseErrorFmt('Unable to fill Buffer from Object for field %s',[Field.FieldName]);
    end;
  end;
end;

procedure TSnapCustomObjectDataset.SetFieldValue(Field: TField; Value: Variant);
var
  PropInfo: PPropInfo;
  s: string;
  TempDouble: Double;
  Obj: TObject;
begin
  Obj := ActiveItem;
  if Obj<>nil then
  begin
    PropInfo := GetPropInfo(obj, Field.FieldName);
    if Assigned(PropInfo^.SetProc) then
    begin
      case Field.DataType of
        ftString:
        begin
          TVarData(Value).VType := varString;
          s := TVarData(Value).VOleStr;
          if PropInfo^.PropType^.Kind = tkChar then
          begin
            if s=''then
              s := chr(32);
            SetOrdProp(obj, Field.FieldName, Ord(s[1]));
          end
          else
            SetStrProp(obj, Field.FieldName, Value);
        end;
        ftInteger:
        begin
          SetOrdProp(obj, Field.FieldName, Value);
        end;
        ftSmallInt:
        begin
          SetOrdProp(obj, Field.FieldName, Value);
        end;
        ftWord:
        begin
          SetOrdProp(obj, Field.FieldName, Value);
        end;
        ftBoolean:
        begin
          SetOrdProp(obj, Field.FieldName, Value);
        end;
        ftFloat,
        ftCurrency:
        begin
          SetFloatProp(obj, Field.FieldName, Value);
        end;
        ftLargeInt:
        begin //Variants do not support int64
          //Problem:Cardinal is also edited as int64
          if PropInfo^.PropType^.Kind = tkInt64 then
            SetInt64Prop(obj, Field.FieldName, TVarData(Value).VInt64)
          else
            SetOrdProp(obj, Field.FieldName, Value);
        end;
        ftBCD,
        ftDateTime,
        ftDate,
        ftTime,
        ftTimeStamp:
        begin
          SetFloatProp(obj, Field.FieldName, Value);
        end;
        ftDataset:
        begin
          SetObjectProp(obj, Field.FieldName, TObject(TVarData(Value).VPointer));
        end;
        else
          DatabaseErrorFmt('Unable to fill Object from buffer for field %s',[Field.FieldName]);
      end;
    end;
  end;
end;

procedure TSnapCustomObjectDataset.SetBlobField(Field: TBlobField; Stream: TStream);
var Obj: TObject;
begin
  if ActiveItem=nil then exit;
  Obj:=TObject(GetOrdProp(ActiveItem,Field.FieldName));
  if (Obj is TStrings) then
    TStrings(Obj).LoadFromStream(Stream);
  if (Obj is TPicture) then
    if TPicture(Obj).Graphic<>nil then
      TPicture(Obj).Graphic.LoadFromStream(Stream);
end;

function TSnapCustomObjectDataset.GetCanModify: Boolean;
begin
  Result := True;
end;

function TSnapCustomObjectDataset.GetRecordCount: Integer;
begin
  Result := 0;
  if fObjectInstance<>nil then
  begin
    if fObjectInstance.InheritsFrom(TObjectList) then
      Result := TObjectList(fObjectInstance).Count
    else if fObjectInstance.InheritsFrom(TCollection) then
      Result := TCollection(fObjectInstance).Count
    else if (fObjectInstance<>nil) then
      Result := 1;
  end;
end;

procedure TSnapCustomObjectDataset.DoInsertRecord;
var
  Item: TCollectionItem;
begin
  if fObjectInstance.InheritsFrom(TCollection) then
  begin
    if fCurrent=-1 then
      Item := TCollection(fObjectInstance).Add
    else
      Item := TCollection(fObjectInstance).Insert(fCurrent);
    fCurrent := Item.Index;
  end
  else if fObjectInstance.InheritsFrom(TObjectList) then
  begin
    fTempObject := DoObjectCreate;
    if fCurrent=-1 then
      fCurrent := TObjectList(fObjectInstance).Add(fTempObject)
    else
      TObjectList(fObjectInstance).Insert(fCurrent, fTempObject);
    fTempObject := nil;
  end;
end;

procedure TSnapCustomObjectDataset.DoCancelRecord;
begin
  if (State=dsInsert) and (fCurrent<>-1) then
  begin
    if(fObjectInstance.InheritsFrom(TCollection)) then
      TCollection(fObjectInstance).Delete(fCurrent)
    else if(fObjectInstance.InheritsFrom(TObjectList)) then
      TObjectList(fObjectInstance).Delete(fCurrent)
  end;
end;

procedure TSnapCustomObjectDataset.DoSetMasterField(Field: TField);
var
  MyBuffer: pChar;
  obj: TObject;
begin
  obj := nil;
  GetMem(MyBuffer, SizeOf(TObject));
  try
    FillChar(MyBuffer^, SizeOf(TObject), 0);
    Field.GetData(MyBuffer);
    Move(MyBuffer^, Obj, Sizeof(TObject))
  finally
    FreeMem(MyBuffer, SizeOf(TObject));
  end;
  SetInternalObjectInstance(obj);
  fCurrent := -1;
end;

procedure TSnapCustomObjectDataset.SetObjectClassName(const Value: string);
begin
  if IsOpen then
    DatabaseError(SDataSetOpen);
  fObjectClassName := Value;
end;

procedure TSnapCustomObjectDataset.CheckObjectClass;
begin
  if (ObjectClass=nil) and (Trim(ObjectClassName) <> '') then
    fObjectClass := GetClass(ObjectClassName);
  if (ObjectClass=nil) and (Trim(ObjectClassName) = '') then
    DatabaseErrorFmt('%s.ObjectClassname is empty',[Name]);
end;


function TSnapCustomObjectDataset.GetObjectInstance: TObject;
begin
  Result := fObjectInstance;
end;

procedure TSnapCustomObjectDataset.SetObjectInstance(const Value: TObject);
begin
  if IsOpen then
    DatabaseError(SDataSetOpen);
  SetInternalObjectInstance(Value);
end;

function TSnapCustomObjectDataset.GetObjectItem(AIndex: integer): TObject;
begin
  if fObjectInstance.InheritsFrom(TObjectList) then
    Result := TObjectList(fObjectInstance).Items[AIndex]
  else if fObjectInstance.InheritsFrom(TCollection) then
    Result := TCollection(fObjectInstance).Items[AIndex]
  else
    Result := fObjectInstance;
end;

procedure TSnapCustomObjectDataset.SetInternalObjectInstance(
  const Value: TObject);
begin
  fObjectInstance := Value;
  if fObjectInstance<>nil then
  begin
    if fObjectInstance.InheritsFrom(TCollection) then
      fObjectClass := TCollection(fObjectInstance).ItemClass
    else if not(fObjectInstance.InheritsFrom(TObjectList)) then
      fObjectClass := fObjectInstance.ClassType;
  end;
end;

function TSnapCustomObjectDataset.DoObjectCreate: TObject;
begin
  if Assigned(fOnObjectCreate) then
    fOnObjectCreate(Self, Result)
  else
    Result := fObjectClass.Create;
end;

function TSnapCustomObjectDataset.SupportedFieldType(
  AType: TFieldType): Boolean;
begin
  Result := AType in ftSupportedType;
end;

end.
