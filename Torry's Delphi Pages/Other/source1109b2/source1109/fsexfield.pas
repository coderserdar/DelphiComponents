
Unit fsexfield;

{$R-,T-,H+,X+}
{$I fsdefine.inc}

Interface

Uses windows,
  SysUtils,
  Classes,
  fsllBase,
  fsfunInterp,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  db,
  Graphics;
Const
  dsFSMaxStringSize = 32767;
  dsFSMaxArraySize = 65531;
  dsFSMaxWordArraySize = 32767;
  dsFSMaxIntArraySize = 16383;
  dsFSMaxDoubleArraySize = 8191;
Type

  TFSNumericField = Class(TField)
  Private
    fRound: TRound;
    ffsDataType: TfsFieldType;
    FDisplayFormat: String;
    FEditFormat: String;
  Protected
    Procedure RangeError(Value, Min, Max: Extended);
    Procedure SetDisplayFormat(Const Value: String);
    Procedure SetEditFormat(Const Value: String);
  Public
    Constructor Create(AOwner: TComponent); Override;
    Procedure CheckInactive;
    Property FsDataType: TfsFieldType Read ffsDataType Write ffsDataType;
  Published
    Property Alignment Default taRightJustify;
    Property DisplayFormat: String Read FDisplayFormat Write SetDisplayFormat;
    Property EditFormat: String Read FEditFormat Write SetEditFormat;
    Property VRound: TRound Read fRound Write fRound;
  End;

  { TFSExtendedField }
  TFSExtendedField = Class(TFSNumericField)
  Private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FPrecision: Integer;
    FMinValue: Extended;
    FMaxValue: Extended;
    Procedure SetCurrency(Value: Boolean);
    Procedure SetMaxValue(Value: Extended);
    Procedure SetMinValue(Value: Extended);
    Procedure SetPrecision(Value: Integer);
    Procedure UpdateCheckRange;
  Protected
    Class Procedure CheckTypeSize(Value: Integer); Override;
    Procedure CopyData(Source, Dest: Pointer); Override;
    Function GetAsFloat: Double; Override;
    Function GetAsExtended: Extended;
    Function GetAsCurrency: Currency; Override;
    Function GetAsInteger: Longint; Override;
    Function GetAsInt64: Int64;
    Function GetAsString: String; Override;
    Function GetAsVariant: Variant; Override;
    Function GetDataSize: Integer; Override;
    Procedure GetText(Var Text: String; DisplayText: Boolean); Override;
    Procedure SetAsFloat(Value: Double); Override;
    Procedure SetAsCurrency(Value: Currency); Override;
    Procedure SetAsExtended(Value: Extended); Virtual;
    Procedure SetAsInteger(Value: Longint); Override;
    Procedure SetAsInt64(Value: Int64);
    Procedure SetAsString(Const Value: String); Override;
    Procedure SetVarValue(Const Value: Variant); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property Value: Extended Read GetAsExtended Write SetAsExtended;
    Property AsExtended: Extended Read GetAsExtended Write SetAsExtended;
  Published
    Property Currency: Boolean Read FCurrency Write SetCurrency Default False;
    Property MaxValue: Extended Read FMaxValue Write SetMaxValue;
    Property MinValue: Extended Read FMinValue Write SetMinValue;
    Property Precision: Integer Read FPrecision Write SetPrecision;
  End;

  TFSCurrencyField = Class(TFSNumericField)
  Private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FMinValue: Currency;
    FMaxValue: Currency;
    FPrecision: Integer;
    Procedure SetCurrency(Value: Boolean);
    Procedure SetMaxValue(Value: Currency);
    Procedure SetMinValue(Value: Currency);
    Procedure SetPrecision(Value: Integer);
    Procedure UpdateCheckRange;
  Protected
    Class Procedure CheckTypeSize(Value: Integer); Override;
    Procedure CopyData(Source, Dest: Pointer); Override;
    Function GetAsCurrency: Currency; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsExtended: Extended;
    Function GetAsInteger: Longint; Override;
    Function GetAsInt64: Int64;
    Function GetAsString: String; Override;
    Function GetAsVariant: Variant; Override;
    Function GetDataSize: Integer; Override;
    Function GetDefaultWidth: Integer; Override;
    Procedure GetText(Var Text: String; DisplayText: Boolean); Override;
    Function GetValue(Var Value: Currency): Boolean;
    Procedure SetAsCurrency(Value: Currency); Override;
    Procedure SetAsFloat(Value: Double); Override;
    Procedure SetAsExtended(Value: Extended); Virtual;
    Procedure SetAsInteger(Value: Longint); Override;
    Procedure SetAsInt64(Value: Int64);
    Procedure SetAsString(Const Value: String); Override;
    Procedure SetVarValue(Const Value: Variant); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property Value: Currency Read GetAsCurrency Write SetAsCurrency;
  Published
    { Lowercase to avoid name clash with C++ Currency type }
    Property Currency: Boolean Read FCurrency Write SetCurrency Default False;
    Property MaxValue: Currency Read FMaxValue Write SetMaxValue;
    Property MinValue: Currency Read FMinValue Write SetMinValue;
    Property Precision: Integer Read FPrecision Write SetPrecision Default 0;
    Property Size Default 4;
  End;

  TFSBCDField = Class(TFSNumericField)
  Private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FMinValue: Currency;
    FMaxValue: Currency;
    FPrecision: Integer;
    Procedure SetCurrency(Value: Boolean);
    Procedure SetMaxValue(Value: Currency);
    Procedure SetMinValue(Value: Currency);
    Procedure SetPrecision(Value: Integer);
    Procedure UpdateCheckRange;
  Protected
    Class Procedure CheckTypeSize(Value: Integer); Override;
    Procedure CopyData(Source, Dest: Pointer); Override;
    Function GetAsCurrency: Currency; Override;
    Function GetAsFloat: Double; Override;
    Function GetAsExtended: Extended;
    Function GetAsInteger: Longint; Override;
    Function GetAsInt64: Int64;
    Function GetAsString: String; Override;
    Function GetAsVariant: Variant; Override;
    Function GetDataSize: Integer; Override;
    Function GetDefaultWidth: Integer; Override;
    Procedure GetText(Var Text: String; DisplayText: Boolean); Override;
    Function GetValue(Var Value: Currency): Boolean;
    Procedure SetAsCurrency(Value: Currency); Override;
    Procedure SetAsFloat(Value: Double); Override;
    Procedure SetAsExtended(Value: Extended); Virtual;
    Procedure SetAsInteger(Value: Longint); Override;
    Procedure SetAsInt64(Value: Int64);
    Procedure SetAsString(Const Value: String); Override;
    Procedure SetVarValue(Const Value: Variant); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property Value: Currency Read GetAsCurrency Write SetAsCurrency;
  Published
    { Lowercase to avoid name clash with C++ Currency type }
    Property Currency: Boolean Read FCurrency Write SetCurrency Default False;
    Property MaxValue: Currency Read FMaxValue Write SetMaxValue;
    Property MinValue: Currency Read FMinValue Write SetMinValue;
    Property Precision: Integer Read FPrecision Write SetPrecision Default 0;
    Property Size Default 4;
  End;

  TFSStringField = Class(TStringField)
  Protected
    Class Procedure CheckTypeSize(Value: Integer); Override;
    Function GetValue(Var Value: String): Boolean;
    Procedure SetAsString(Const Value: String); Override;
    Function GetAsString: String; Override;
    Function GetAsVariant: Variant; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
  End;

  TfsIntegerField = Class(TfsNumericField)
  Private
    FMinRange: Longint;
    FMaxRange: Longword;
    FMinValue: Longint;
    FMaxValue: Longword;
    Procedure CheckRange(Value, Min: Longint; Max: Longword);
    Procedure CheckRangeL(Value: Longword; Min: Longint; Max: Longword);
    Procedure SetMaxValue(Value: Longword);
    Procedure SetMinValue(Value: Longint);
  Protected
    Function GetAsFloat: Double; Override;
    Function GetAsInteger: Longint; Override;
    Function GetAsLongword: Longword;
    Function GetAsString: String; Override;
    Function GetAsVariant: Variant; Override;
    Function GetDataSize: Integer; Override;
    Procedure GetText(Var Text: String; DisplayText: Boolean); Override;
    Function GetValue(Var Value: Longint): Boolean;
    Function GetValueL(Var Value: Longword): Boolean;
    Procedure SetAsFloat(Value: Double); Override;
    Procedure SetAsInteger(Value: Longint); Override;
    Procedure SetAsLongWord(Value: Longword);
    Procedure SetAsString(Const Value: String); Override;
    Procedure SetVarValue(Const Value: Variant); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property Value: Longint Read GetAsInteger Write SetAsInteger;
    Property ValueLong: Longword Read GetAsLongword Write SetAsLongword;
  Published
    Property MaxValue: Longword Read FMaxValue Write SetMaxValue;
    Property MinValue: Longint Read FMinValue Write SetMinValue;
  End;

  TfsBinaryField = Class(TField)
  Private
    ffsDataType: TfsFieldType;
  Protected
    Class Procedure CheckTypeSize(Value: Integer); Override;
    Procedure CopyData(Source, Dest: Pointer); Override;
    Function GetAsString: String; Override;
    Procedure GetText(Var Text: String; DisplayText: Boolean); Override;
    Function GetAsVariant: Variant; Override;
    Procedure SetAsString(Const Value: String); Override;
    Procedure SetText(Const Value: String); Override;
    Procedure SetVarValue(Const Value: Variant); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property FsDataType: TfsFieldType Read ffsDataType Write ffsDataType;
  Published
    Property Size Default 16;
  End;

  { TBytesField }

  TfsArrayField = Class(TfsBinaryField)
  Private
    fShowArray: Boolean;
    FDecimal: Byte;
    fRound: TRound;
    Procedure SetDecimal(Value: Byte);
    Procedure SetShowArray(Value: Boolean);
    Function GetAValue(Index: Integer): Variant;
    Procedure SetAValue(Index: Integer; Value: Variant);
  Protected
    Class Procedure CheckTypeSize(Value: Integer); Override;
    Procedure CopyData(Source, Dest: Pointer); Override;
    Function GetValue(Var Value: String): Boolean;
    Procedure SetAsString(Const Value: String); Override;
    Function GetAsString: String; Override;
    Function GetAsVariant: Variant; Override;
    Procedure GetText(Var Text: String; DisplayText: Boolean); Override;
    Function GetDataSize: Integer; Override;
    Procedure SetText(Const Value: String); Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Property Value[Index: Integer]: Variant Read GetAValue Write SetAValue;
    Property VRound: TRound Read fRound Write fRound;
  Published
    Property ShowArray: boolean Read fShowArray Write SetShowArray;
    Property Decimal: Byte Read FDecimal Write SetDecimal;
  End;

  TFSBlobField = Class(TBlobField)
  Protected
    Function GetIsNull: Boolean; Override;
    Function GetClassDesc: String; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
  End;

  TFSGraphicField = Class(TBlobField)
  Protected
    Function GetIsNull: Boolean; Override;
    Function GetClassDesc: String; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
  End;

  TFSMemoField = Class(TBlobField)
  Protected
    Function GetIsNull: Boolean; Override;
    Function GetClassDesc: String; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
  End;

  TFSFmtMemoField = Class(TBlobField)
  Protected
    Function GetIsNull: Boolean; Override;
    Function GetClassDesc: String; Override;
  Public
    Constructor Create(AOwner: TComponent); Override;
  End;

  { TParam }

  TBlobData = String;

  TfsParamType = (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
  TfsParamTypes = Set Of TfsParamType;

  TfsParams = Class;

  TfsParam = Class(TCollectionItem)
  Private
    FParamRef: TfsParam;
    FNativeStr: String;
    FData: Variant;
    FNull: Boolean;
    FName: String;
    FDataType: TFieldType;
    FBound: Boolean;
    FParamType: TfsParamType;
    Function ParamRef: TfsParam;
    Function GetDataSet: TDataSet;
    Function IsParamStored: Boolean;
    Function GetDataType: TFieldType;
    Function GeTfsParamType: TfsParamType;
    Procedure SeTfsParamType(Value: TfsParamType);
  Protected
    Procedure AssignParam(Param: TfsParam);
    Procedure AssignTo(Dest: TPersistent); Override;
    Function GetAsBCD: Currency;
    Function GetAsBoolean: Boolean;
    Function GetAsDateTime: TDateTime;
    Function GetAsCurrency: Currency;
    Function GetAsFloat: Double;
    Function GetAsInteger: Longint;
    Function GetAsMemo: String;
    Function GetAsString: String;
    Function GetAsVariant: Variant;
    Function GetIsNull: Boolean;
    Function IsEqual(Value: TfsParam): Boolean;
    Procedure SetAsBCD(Const Value: Currency);
    Procedure SetAsBlob(Const Value: TBlobData);
    Procedure SetAsBoolean(Value: Boolean);
    Procedure SetAsCurrency(Const Value: Currency);
    Procedure SetAsDate(Const Value: TDateTime);
    Procedure SetAsDateTime(Const Value: TDateTime);
    Procedure SetAsFloat(Const Value: Double);
    Procedure SetAsInteger(Value: Longint);
    Procedure SetAsMemo(Const Value: String);
    Procedure SetAsString(Const Value: String);
    Procedure SetAsSmallInt(Value: Longint);
    Procedure SetAsTime(Const Value: TDateTime);
    Procedure SetAsVariant(Const Value: Variant);
    Procedure SetAsWord(Value: Longint);
    Procedure SetDataType(Value: TFieldType);
    Procedure SetText(Const Value: String);
    Function GetDisplayName: String; Override;
    Property DataSet: TDataSet Read GetDataSet;
  Public
    Constructor Create(Collection: TCollection); Overload; Override;
    Constructor Create(AParams: TfsParams; AParamType: TfsParamType); Reintroduce; Overload;
    Procedure Assign(Source: TPersistent); Override;
    Procedure AssignField(Field: TField);
    Procedure AssignFieldValue(Field: TField; Const Value: Variant);
    Procedure Clear;
    Procedure GetData(Buffer: Pointer);
    Function GetDataSize: Integer;
    Procedure LoadFromFile(Const FileName: String; BlobType: TBlobType);
    Procedure LoadFromStream(Stream: TStream; BlobType: TBlobType);
    Procedure SetBlobData(Buffer: Pointer; Size: Integer);
    Procedure SetData(Buffer: Pointer);
    Property AsBcd: Currency Read GetAsBCD Write SetAsBCD;
    Property AsBlob: TBlobData Read GetAsString Write SetAsBlob;
    Property AsBoolean: Boolean Read GetAsBoolean Write SetAsBoolean;
    Property AsCurrency: Currency Read GetAsCurrency Write SetAsCurrency;
    Property AsDate: TDateTime Read GetAsDateTime Write SetAsDate;
    Property AsDateTime: TDateTime Read GetAsDateTime Write SetAsDateTime;
    Property AsFloat: Double Read GetAsFloat Write SetAsFloat;
    Property AsInteger: Longint Read GetAsInteger Write SetAsInteger;
    Property AsSmallInt: Longint Read GetAsInteger Write SetAsSmallInt;
    Property AsMemo: String Read GetAsMemo Write SetAsMemo;
    Property AsString: String Read GetAsString Write SetAsString;
    Property AsTime: TDateTime Read GetAsDateTime Write SetAsTime;
    Property AsWord: Longint Read GetAsInteger Write SetAsWord;
    Property Bound: Boolean Read FBound Write FBound;
    Property IsNull: Boolean Read GetIsNull;
    Property NativeStr: String Read FNativeStr Write FNativeStr;
    Property Text: String Read GetAsString Write SetText;
  Published
    Property DataType: TFieldType Read GetDataType Write SetDataType;
    Property Name: String Read FName Write FName;
    Property ParamType: TfsParamType Read GeTfsParamType Write SeTfsParamType;
    Property Value: Variant Read GetAsVariant Write SetAsVariant Stored IsParamStored;
  End;

  { TfsParams }

  TfsParams = Class(TCollection)
  Private
    FOwner: TPersistent;
    Function GeTfsParamValue(Const ParamName: String): Variant;
    Procedure ReadBinaryData(Stream: TStream);
    Procedure SeTfsParamValue(Const ParamName: String;
      Const Value: Variant);
    Function GetItem(Index: Integer): TfsParam;
    Procedure SetItem(Index: Integer; Value: TfsParam);
  Protected
    Procedure AssignTo(Dest: TPersistent); Override;
    Procedure DefineProperties(Filer: TFiler); Override;
    Function GetDataSet: TDataSet;
    Function GetOwner: TPersistent; Override;
    Procedure Update(Item: TCollectionItem); Override;
  Public
    Constructor Create(Owner: TPersistent); Overload;
    Procedure AssignValues(Value: TfsParams);
    { Create, AddParam, RemoveParam and CreateParam are in for backward compatibility }
    Constructor Create; Overload;
    Procedure AddParam(Value: TfsParam);
    Procedure RemoveParam(Value: TfsParam);
    Function CreateParam(FldType: TFieldType; Const ParamName: String;
      ParamType: TfsParamType): TfsParam;
    Procedure GeTfsParamList(List: TList; Const ParamNames: String);
    Function IsEqual(Value: TfsParams): Boolean;
    Function ParseSQL(SQL: String; DoCreate: Boolean): String;
    Function ParamByName(Const Value: String): TfsParam;
    Function FindParam(Const Value: String): TfsParam;
    Property Items[Index: Integer]: TfsParam Read GetItem Write SetItem; Default;
    Property ParamValues[Const ParamName: String]: Variant Read GeTfsParamValue Write SeTfsParamValue;
  End;

Implementation

Uses dbConsts,
  consts,
  sysConst,
  Mask,
  fsutil;

Constructor TFSNumericField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  Alignment := taRightJustify;
  fRound := rNone;
  fFsDataType := fstInterval;
End;

Procedure TFSNumericField.RangeError(Value, Min, Max: Extended);
Begin
  DatabaseErrorFmt(SFieldRangeError, [Value, DisplayName, Min, Max]);
End;

Procedure TFSNumericField.SetDisplayFormat(Const Value: String);
Begin
  If FDisplayFormat <> Value Then
    Begin
      FDisplayFormat := Value;
      PropertyChanged(False);
    End;
End;

Procedure TFSNumericField.SetEditFormat(Const Value: String);
Begin
  If FEditFormat <> Value Then
    Begin
      FEditFormat := Value;
      PropertyChanged(False);
    End;
End;

Procedure TFSNumericField.CheckInactive;
Begin
  If Dataset.Active Then
    If ([csUpdating, csDesigning] * ComponentState) <> [] Then
      Dataset.Close
    Else
      DatabaseError(SDataSetOpen, Dataset);
End;

Constructor TFSStringField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
End;

Class Procedure TFSStringField.CheckTypeSize(Value: Integer);
Begin
  If (Value < 0) Or (Value > dsfsMaxStringSize) Then
    DatabaseError(SInvalidFieldSize);
End;

Function TFSStringField.GetValue(Var Value: String): Boolean;
Var
  Buffer: Array[0..dsfsMaxStringSize] Of Char;
Begin
  Result := GetData(@Buffer);
  If Result Then
    Begin
      Value := Buffer;
      If Transliterate And (Value <> '') Then
        DataSet.Translate(PChar(Value), PChar(Value), False);
    End;
End;

Function TFSStringField.GetAsString: String;
Begin
  If Not GetValue(Result) Then Result := '';
End;

Function TFSStringField.GetAsVariant: Variant;
Var
  S: String;
Begin
  If GetValue(S) Then
    Result := S
  Else
    Result := Null;
End;

Procedure TFSStringField.SetAsString(Const Value: String);
Var
  Buffer: Array[0..dsfsMaxStringSize] Of Char;
Begin
  StrLCopy(Buffer, PChar(Value), Size);
  If Transliterate Then
    DataSet.Translate(Buffer, Buffer, True);
  SetData(@Buffer);
End;

Constructor TFSBlobField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftBlob);
End;

Function TFSBlobField.GetIsNull: Boolean;
Begin
  If Modified Then
    Begin
      With DataSet.CreateBlobStream(Self, bmRead) Do
        Try
          Result := (Size = 0);
        Finally
          Free;
        End;
    End
  Else
    Begin
      If (csDesigning In ComponentState) Then
        Result := True
      Else
        Begin
          Result := Inherited GetIsNull;
          // alternative
          {TFSDataSet(Dataset).GetActiveRecBuf(bsRecBuf);
          If bsRecBuf <> Nil then
            Begin
              BSize := bsGetBlobSize;
              Result := BSize <= 0;
            End ; }
        End;
    End;
End;

Function TFSBlobField.GetClassDesc: String;
Begin
  Result := Format('(%s)', [FieldtypeNames[DataType]]);
  If Not IsNull Then
    Result := AnsiUpperCase(Result);
End;

Constructor TFSMemoField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftMemo);
End;

Function TFSMemoField.GetIsNull: Boolean;
Begin
  If Modified Then
    Begin
      With DataSet.CreateBlobStream(Self, bmRead) Do
        Try
          Result := (Size = 0);
        Finally
          Free;
        End;
    End
  Else
    Begin
      If (csDesigning In ComponentState) Then
        Result := True
      Else
        Begin
          Result := Inherited GetIsNull;
          // alternative
          {TFSDataSet(Dataset).GetActiveRecBuf(bsRecBuf);
          If bsRecBuf <> Nil then
            Begin
              BSize := bsGetBlobSize;
              Result := BSize <= 0;
            End ; }
        End;
    End;
End;

Function TFSMemoField.GetClassDesc: String;
Begin
  Result := Format('(%s)', [FieldtypeNames[DataType]]);
  If Not IsNull Then
    Result := AnsiUpperCase(Result);
End;

Constructor TFSFmtMemoField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftFmtMemo);
End;

Function TFSFmtMemoField.GetIsNull: Boolean;
Begin
  If Modified Then
    Begin
      With DataSet.CreateBlobStream(Self, bmRead) Do
        Try
          Result := (Size = 0);
        Finally
          Free;
        End;
    End
  Else
    Begin
      If (csDesigning In ComponentState) Then
        Result := True
      Else
        Begin
          Result := Inherited GetIsNull;
          // alternative
          {TFSDataSet(Dataset).GetActiveRecBuf(bsRecBuf);
          If bsRecBuf <> Nil then
            Begin
              BSize := bsGetBlobSize;
              Result := BSize <= 0;
            End ; }
        End;
    End;
End;

Function TFSFmtMemoField.GetClassDesc: String;
Begin
  Result := Format('(%s)', [FieldtypeNames[DataType]]);
  If Not IsNull Then
    Result := AnsiUpperCase(Result);
End;

Constructor TFSGraphicField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftGraphic);
End;

Function TFSGraphicField.GetIsNull: Boolean;
Begin
  If Modified Then
    Begin
      With DataSet.CreateBlobStream(Self, bmRead) Do
        Try
          Result := (Size = 0);
        Finally
          Free;
        End;
    End
  Else
    Begin
      If (csDesigning In ComponentState) Then
        Result := True
      Else
        Begin
          Result := Inherited GetIsNull;
        End;
    End;
End;

Function TFSGraphicField.GetClassDesc: String;
Begin
  Result := Format('(%s)', [FieldtypeNames[DataType]]);
  If Not IsNull Then
    Result := AnsiUpperCase(Result);
End;

{ TFSExtendedField }

Constructor TFSExtendedField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftFloat);
  FPrecision := 18;
  ValidChars := [DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
End;

Procedure TFSExtendedField.CopyData(Source, Dest: Pointer);
Begin
  If fsDataType = fstExtended Then
    System.Extended(Dest^) := System.Extended(Source^)
  Else
    System.Double(Dest^) := System.Double(Source^);
End;

Function TFSExtendedField.GetAsFloat: Double;
Var
  E: Extended;
Begin
  If fsDataType = fstExtended Then
    Begin
      If Not GetData(@E) Then
        Result := 0
      Else
        Result := E;
    End
  Else
    Begin
      If Not GetData(@Result) Then
        Result := 0;
    End;
End;

Function TFSExtendedField.GetAsExtended: Extended;
Var
  D: Double;
Begin
  If fsDataType = fstExtended Then
    Begin
      If Not GetData(@Result) Then
        Result := 0;
    End
  Else
    Begin
      If Not GetData(@D) Then
        Result := 0
      Else
        Result := D;
    End;
End;

Function TFSExtendedField.GetAsInteger: Longint;
Begin
  Result := Longint(Round(GetAsExtended));
End;

Function TFSExtendedField.GetAsInt64: Int64;
Begin
  Result := Round(GetAsExtended);
End;

Function TFSExtendedField.GetAsString: String;
Var
  F: Extended;
  D: Double;
Begin
  If fsDataType = fstExtended Then
    Begin
      If GetData(@F) Then
        Result := fsFloatToStr(F)
      Else
        Result := '';
    End
  Else If GetData(@D) Then
    Result := fsFloatToStr(D)
  Else
    Result := '';
End;

Function TFSExtendedField.GetAsVariant: Variant;
Var
  F: Extended;
  D: Double;
Begin
  If fsDataType = fstExtended Then
    Begin
      If GetData(@F) Then
        Result := F
      Else
        Result := Null;
    End
  Else If GetData(@D) Then
    Result := D
  Else
    Result := Null;
End;

Function TFSExtendedField.GetDataSize: Integer;
Begin
  If fsDataType = fstExtended Then
    Result := 10
  Else
    Result := 8;
End;

Procedure TFSExtendedField.GetText(Var Text: String; DisplayText: Boolean);
Var
  Format: TFloatFormat;
  FmtStr: String;
  Digits: Integer;
  F: Extended;
  D: Double;
Begin
  If fsDataType = fstExtended Then
    Begin
      If GetData(@F) Then
        Begin
          If DisplayText Or (EditFormat = '') Then
            FmtStr := DisplayFormat
          Else
            FmtStr := EditFormat;
          If FmtStr = '' Then
            Begin
              If Currency Then
                Begin
                  If DisplayText Then
                    Format := ffCurrency
                  Else
                    Format := ffFixed;
                  Digits := CurrencyDecimals;
                End
              Else
                Begin
                  If size > 0 Then
                    Begin
                      Format := ffFixed;
                      Digits := size;
                    End
                  Else
                    Begin
                      Format := ffGeneral;
                      Digits := 0;
                    End;
                End;
              Text := fsFloatToStrF(F, Format, Precision, Digits);
            End
          Else
            Text := FormatFloat(FmtStr, F);
        End
      Else
        Text := '';
    End
  Else
    Begin
      If GetData(@D) Then
        Begin
          If DisplayText Or (EditFormat = '') Then
            FmtStr := DisplayFormat
          Else
            FmtStr := EditFormat;
          If FmtStr = '' Then
            Begin
              If Currency Then
                Begin
                  If DisplayText Then
                    Format := ffCurrency
                  Else
                    Format := ffFixed;
                  Digits := CurrencyDecimals;
                End
              Else
                Begin
                  If size > 0 Then
                    Begin
                      Format := ffFixed;
                      Digits := size;
                    End
                  Else
                    Begin
                      Format := ffGeneral;
                      Digits := 0;
                    End;
                End;
              Text := fsFloatToStrF(D, Format, Precision, Digits);
            End
          Else
            Text := FormatFloat(FmtStr, D);
        End;
    End;
End;

Class Procedure TFSExtendedField.CheckTypeSize(Value: Integer);
Begin
  If Value > 20 Then DatabaseError(SInvalidFieldSize);
End;

Procedure TFSExtendedField.SetAsFloat(Value: Double);
Begin
  SetAsExtended(Value);
End;

Procedure TFSExtendedField.SetAsExtended(Value: Extended);
Var
  D: Double;
Begin
  If FCheckRange And ((Value < FMinValue) Or (Value > FMaxValue)) Then
    RangeError(Value, FMinValue, FMaxValue);
  If fsDataType = fstExtended Then
    SetData(@Value)
  Else
    Begin
      D := Value;
      SetData(@D);
    End;
End;

Procedure TFSExtendedField.SetAsInteger(Value: Longint);
Begin
  SetAsExtended(Value);
End;

Procedure TFSExtendedField.SetAsInt64(Value: Int64);
Begin
  SetAsExtended(Value);
End;

Procedure TFSExtendedField.SetAsString(Const Value: String);
Var
  F: Extended;
Begin
  If Value = '' Then
    Clear
  Else
    Begin
      If Not TextToFloat(PChar(Value), F, fvExtended) Then
        DatabaseErrorFmt(SInvalidFloatValue, [Value, DisplayName]);
      SetAsExtended(F);
    End;
End;

Procedure TFSExtendedField.SetCurrency(Value: Boolean);
Begin
  If FCurrency <> Value Then
    Begin
      FCurrency := Value;
      PropertyChanged(False);
    End;
End;

Procedure TFSExtendedField.SetAsCurrency(Value: Currency);
Var
  F: Extended;
Begin
  If FCheckRange And ((Value < FMinValue) Or (Value > FMaxValue)) Then
    RangeError(Value, FMinValue, FMaxValue);
  F := Value;
  SetAsExtended(F);
End;

Function TFSExtendedField.GetAsCurrency: Currency;
Begin
  If Not GetData(@Result) Then
    Result := 0;
End;

Procedure TFSExtendedField.SetMaxValue(Value: Extended);
Begin
  FMaxValue := Value;
  UpdateCheckRange;
End;

Procedure TFSExtendedField.SetMinValue(Value: Extended);
Begin
  FMinValue := Value;
  UpdateCheckRange;
End;

Procedure TFSExtendedField.SetPrecision(Value: Integer);
Begin
  If Value < 1 Then
    Value := 1;
  If FPrecision <> Value Then
    Begin
      FPrecision := Value;
      PropertyChanged(False);
    End;
End;

Procedure TFSExtendedField.SetVarValue(Const Value: Variant);
Begin
  SetAsFloat(Value);
End;

Procedure TFSExtendedField.UpdateCheckRange;
Begin
  FCheckRange := (FMinValue <> 0) Or (FMaxValue <> 0);
End;

Constructor TFSCurrencyField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftCurrency);
  FPrecision := 19;
  ValidChars := [DecimalSeparator, '+', '-', '0'..'9'];
End;

Class Procedure TFSCurrencyField.CheckTypeSize(Value: Integer);
Begin
  If Value > 32 Then DatabaseError(SInvalidFieldSize);
End;

Function TFSCurrencyField.GetAsCurrency: Currency;
Begin
  If Not GetValue(Result) Then Result := 0;
End;

Function TFSCurrencyField.GetAsFloat: Double;
Begin
  Result := GetAsCurrency;
End;

Function TFSCurrencyField.GetAsExtended: Extended;
Begin
  Result := GetAsCurrency;
End;

Function TFSCurrencyField.GetAsInteger: Longint;
Begin
  Result := Longint(Round(GetAsCurrency));
End;

Function TFSCurrencyField.GetAsInt64: Int64;
Begin
  Result := Round(GetAsCurrency);
End;

Function TFSCurrencyField.GetAsString: String;
Var
  C: System.Currency;
Begin
  If GetValue(C) Then
    Result := CurrToStr(C)
  Else
    Result := '';
End;

Function TFSCurrencyField.GetAsVariant: Variant;
Var
  C: System.Currency;
Begin
  If GetValue(C) Then
    Result := C
  Else
    Result := Null;
End;

Function TFSCurrencyField.GetDataSize: Integer;
Begin
  Result := 8; //SizeOf(TBcd);
End;

Function TFSCurrencyField.GetDefaultWidth: Integer;
Begin
  If FPrecision > 0 Then
    Result := FPrecision + 1
  Else
    Result := Inherited GetDefaultWidth;
End;

Procedure TFSCurrencyField.GetText(Var Text: String; DisplayText: Boolean);
Var
  Format: TFloatFormat;
  Digits: Integer;
  FmtStr: String;
  C: System.Currency;
Begin
  Try
    If GetData(@C, False) Then
      Begin
        If DisplayText Or (EditFormat = '') Then
          FmtStr := DisplayFormat
        Else
          FmtStr := EditFormat;
        If FmtStr = '' Then
          Begin
            If FCurrency Then
              Begin
                If DisplayText Then
                  Format := ffCurrency
                Else
                  Format := ffFixed;
                Digits := CurrencyDecimals;
              End
            Else
              Begin
                If size > 0 Then
                  Begin
                    Format := ffFixed;
                    Digits := size;
                  End
                Else
                  Begin
                    Format := ffGeneral;
                    Digits := 0;
                  End;
              End;
            Text := CurrToStrF(C, Format, Digits);
          End
        Else
          Text := FormatCurr(FmtStr, C);
      End
    Else
      Text := '';
  Except
    On E: Exception Do
      Text := SBCDOverflow;
  End;
End;

Function TFSCurrencyField.GetValue(Var Value: Currency): Boolean;
Begin
  Result := GetData(@Value, False);
End;

Procedure TFSCurrencyField.SetAsCurrency(Value: Currency);
Begin
  If FCheckRange And ((Value < FMinValue) Or (Value > FMaxValue)) Then
    RangeError(Value, FMinValue, FMaxValue);
  SetData(@Value, False);
End;

Procedure TFSCurrencyField.SetAsFloat(Value: Double);
Begin
  SetAsCurrency(Value);
End;

Procedure TFSCurrencyField.SetAsExtended(Value: Extended);
Begin
  SetAsCurrency(Value);
End;

Procedure TFSCurrencyField.SetAsInt64(Value: Int64);
Begin
  SetAsExtended(Value);
End;

Procedure TFSCurrencyField.SetAsInteger(Value: Longint);
Begin
  SetAsCurrency(Value);
End;

Procedure TFSCurrencyField.SetAsString(Const Value: String);
Var
  C: System.Currency;
Begin
  If Value = '' Then
    Clear
  Else
    Begin
      If Not TextToFloat(PChar(Value), C, fvCurrency) Then
        DatabaseErrorFmt(SInvalidFloatValue, [Value, DisplayName]);
      SetAsCurrency(C);
    End;
End;

Procedure TFSCurrencyField.SetCurrency(Value: Boolean);
Begin
  If FCurrency <> Value Then
    Begin
      FCurrency := Value;
      PropertyChanged(False);
    End;
End;

Procedure TFSCurrencyField.SetMaxValue(Value: Currency);
Begin
  FMaxValue := Value;
  UpdateCheckRange;
End;

Procedure TFSCurrencyField.SetMinValue(Value: Currency);
Begin
  FMinValue := Value;
  UpdateCheckRange;
End;

Procedure TFSCurrencyField.SetPrecision(Value: Integer);
Begin
  If (DataSet <> Nil) Then
    CheckInactive;
  If Value < 0 Then Value := 0;
  If Value > 20 Then Value := 20;
  If FPrecision <> Value Then
    Begin
      FPrecision := Value;
      PropertyChanged(False);
    End;
End;

Procedure TFSCurrencyField.SetVarValue(Const Value: Variant);
Begin
  SetAsCurrency(Value);
End;

Procedure TFSCurrencyField.UpdateCheckRange;
Begin
  FCheckRange := (FMinValue <> 0) Or (FMaxValue <> 0);
End;

Procedure TFSCurrencyField.CopyData(Source, Dest: Pointer);
Begin
  System.Currency(Dest^) := System.Currency(Source^);
End;

Constructor TFSBCDField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftBCD);
  Size := 4;
  ValidChars := [DecimalSeparator, '+', '-', '0'..'9'];
End;

Class Procedure TFSBCDField.CheckTypeSize(Value: Integer);
Begin
  If Value > 32 Then DatabaseError(SInvalidFieldSize);
End;

Function TFSBCDField.GetAsCurrency: Currency;
Begin
  If Not GetValue(Result) Then Result := 0;
End;

Function TFSBCDField.GetAsFloat: Double;
Begin
  Result := GetAsCurrency;
End;

Function TFSBCDField.GetAsExtended: Extended;
Begin
  Result := GetAsCurrency;
End;

Function TFSBCDField.GetAsInteger: Longint;
Begin
  Result := Longint(Round(GetAsCurrency));
End;

Function TFSBCDField.GetAsInt64: Int64;
Begin
  Result := Round(GetAsCurrency);
End;

Function TFSBCDField.GetAsString: String;
Var
  C: System.Currency;
Begin
  If GetValue(C) Then
    Result := CurrToStr(C)
  Else
    Result := '';
End;

Function TFSBCDField.GetAsVariant: Variant;
Var
  C: System.Currency;
Begin
  If GetValue(C) Then
    Result := C
  Else
    Result := Null;
End;

Function TFSBCDField.GetDataSize: Integer;
Begin
  Result := 8; //SizeOf(TBcd);
End;

Function TFSBCDField.GetDefaultWidth: Integer;
Begin
  If FPrecision > 0 Then
    Result := FPrecision + 1
  Else
    Result := Inherited GetDefaultWidth;
End;

Procedure TFSBCDField.GetText(Var Text: String; DisplayText: Boolean);
Var
  Format: TFloatFormat;
  Digits: Integer;
  FmtStr: String;
  C: System.Currency;
Begin
  Try
    If GetData(@C, False) Then
      Begin
        If DisplayText Or (EditFormat = '') Then
          FmtStr := DisplayFormat
        Else
          FmtStr := EditFormat;
        If FmtStr = '' Then
          Begin
            If FCurrency Then
              Begin
                If DisplayText Then
                  Format := ffCurrency
                Else
                  Format := ffFixed;
                Digits := CurrencyDecimals;
              End
            Else
              Begin
                Format := ffGeneral;
                Digits := 0;
              End;
            Text := CurrToStrF(C, Format, Digits);
          End
        Else
          Text := FormatCurr(FmtStr, C);
      End
    Else
      Text := '';
  Except
    On E: Exception Do
      Text := SBCDOverflow;
  End;
End;

Function TFSBCDField.GetValue(Var Value: Currency): Boolean;
Begin
  Result := GetData(@Value, False);
End;

Procedure TFSBCDField.SetAsCurrency(Value: Currency);
Begin
  If FCheckRange And ((Value < FMinValue) Or (Value > FMaxValue)) Then
    RangeError(Value, FMinValue, FMaxValue);
  SetData(@Value, False);
End;

Procedure TFSBCDField.SetAsFloat(Value: Double);
Begin
  SetAsCurrency(Value);
End;

Procedure TFSBCDField.SetAsExtended(Value: Extended);
Begin
  SetAsCurrency(Value);
End;

Procedure TFSBCDField.SetAsInt64(Value: Int64);
Begin
  SetAsExtended(Value);
End;

Procedure TFSBCDField.SetAsInteger(Value: Longint);
Begin
  SetAsCurrency(Value);
End;

Procedure TFSBCDField.SetAsString(Const Value: String);
Var
  C: System.Currency;
Begin
  If Value = '' Then
    Clear
  Else
    Begin
      If Not TextToFloat(PChar(Value), C, fvCurrency) Then
        DatabaseErrorFmt(SInvalidFloatValue, [Value, DisplayName]);
      SetAsCurrency(C);
    End;
End;

Procedure TFSBCDField.SetCurrency(Value: Boolean);
Begin
  If FCurrency <> Value Then
    Begin
      FCurrency := Value;
      PropertyChanged(False);
    End;
End;

Procedure TFSBCDField.SetMaxValue(Value: Currency);
Begin
  FMaxValue := Value;
  UpdateCheckRange;
End;

Procedure TFSBCDField.SetMinValue(Value: Currency);
Begin
  FMinValue := Value;
  UpdateCheckRange;
End;

Procedure TFSBCDField.SetPrecision(Value: Integer);
Begin
  If (DataSet <> Nil) Then
    CheckInactive;
  If Value < 0 Then Value := 0;
  If Value > 32 Then Value := 32;
  If FPrecision <> Value Then
    Begin
      FPrecision := Value;
      PropertyChanged(False);
    End;
End;

Procedure TFSBCDField.SetVarValue(Const Value: Variant);
Begin
  SetAsCurrency(Value);
End;

Procedure TFSBCDField.UpdateCheckRange;
Begin
  FCheckRange := (FMinValue <> 0) Or (FMaxValue <> 0);
End;

Procedure TFSBCDField.CopyData(Source, Dest: Pointer);
Begin
  System.Currency(Dest^) := System.Currency(Source^);
End;

Constructor TfsIntegerField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftInteger);
  FMinRange := Low(Longint);
  FMaxRange := High(Longword);
  FMinValue := 0;
  FMaxValue := 0;
  ValidChars := ['+', '-', '0'..'9'];
End;

Procedure TfsIntegerField.CheckRange(Value, Min: Longint; Max: Longword);
Begin
  If (Value < Min) Or (Value > Max) Then RangeError(Value, Min, Max);
End;

Procedure TfsIntegerField.CheckRangeL(Value: Longword; Min: Longint; Max: Longword);
Begin
  If (Value < Min) Or (Value > Max) Then RangeError(Value, Min, Max);
End;

Function TfsIntegerField.GetAsFloat: Double;
Begin
  If fsDataType = fstUInt32 Then
    Result := GetAsLongword
  Else
    Result := GetAsInteger;
End;

Function TfsIntegerField.GetAsInteger: Longint;
Begin
  If Not GetValue(Result) Then Result := 0;
End;

Function TfsIntegerField.GetAsLongword: Longword;
Begin
  If Not GetValueL(Result) Then Result := 0;
End;

Function TfsIntegerField.GetAsString: String;
Var
  L: Longint;
  W: Longword;
Begin
  If fsDataType = fstUInt32 Then
    Begin
      If GetValueL(W) Then
        Str(W, Result)
      Else
        Result := '';
    End
  Else
    Begin
      If GetValue(L) Then
        Str(L, Result)
      Else
        Result := '';
    End;
End;

Function TfsIntegerField.GetAsVariant: Variant;
Var
  L: Longint;
  W: Longword;
Begin
  If fsDataType = fstUInt32 Then
    Begin
      If GetValueL(W) Then
        Result := W
      Else
        Result := Null;
    End
  Else
    Begin
      If GetValue(L) Then
        Result := L
      Else
        Result := Null;
    End;
End;

Function TfsIntegerField.GetDataSize: Integer;
Begin
  Result := SizeOf(Integer);
End;

Procedure TfsIntegerField.GetText(Var Text: String; DisplayText: Boolean);
Var
  L: Longint;
  W: Longword;
  FmtStr: String;
Begin
  If fsDataType = fstUInt32 Then
    Begin
      If GetValueL(W) Then
        Begin
          If DisplayText Or (FEditFormat = '') Then
            FmtStr := FDisplayFormat
          Else
            FmtStr := FEditFormat;
          If FmtStr = '' Then
            Str(W, Text)
          Else
            Text := FormatFloat(FmtStr, W);
        End
      Else
        Text := '';
    End
  Else
    Begin
      If GetValue(L) Then
        Begin
          If DisplayText Or (FEditFormat = '') Then
            FmtStr := FDisplayFormat
          Else
            FmtStr := FEditFormat;
          If FmtStr = '' Then
            Str(L, Text)
          Else
            Text := FormatFloat(FmtStr, L);
        End
      Else
        Text := '';
    End;
End;

Function TfsIntegerField.GetValue(Var Value: Longint): Boolean;
Var
  Data: Record
    Case Integer Of
      0: (I: Smallint);
      1: (W: Word);
      2: (L: Longint);
  End;
Begin
  Data.L := 0;
  Result := GetData(@Data);
  If Result Then
    Case DataType Of
      ftSmallint: Value := Data.I;
      ftWord: Value := Data.W;
      Else
        Value := Data.L;
    End;
End;

Function TfsIntegerField.GetValueL(Var Value: Longword): Boolean;
Begin
  Result := GetData(@Value);
End;

Procedure TfsIntegerField.SetAsFloat(Value: Double);
Begin
  If fsDataType = fstUInt32 Then
    Begin
      If Value < 0 Then DatabaseErrorFmt(SInvalidIntegerValue, [Value, DisplayName]);
      SetAsLongword(Longword(Round(Value)));
    End
  Else
    SetAsInteger(Integer(Round(Value)));
End;

Procedure TfsIntegerField.SetAsInteger(Value: Longint);
Begin
  If fsDataType = fstUInt32 Then
    Begin
      If Value < 0 Then DatabaseErrorFmt(SInvalidIntegerValue, [Value, DisplayName]);
      SetAsLongword(Value);
    End
  Else
    Begin
      If (FMinValue <> 0) Or (FMaxValue <> 0) Then
        CheckRange(Value, FMinValue, FMaxValue)
      Else
        CheckRange(Value, FMinRange, FMaxRange);
      SetData(@Value);
    End;
End;

Procedure TfsIntegerField.SetAsLongword(Value: Longword);
Begin
  If (FMinValue <> 0) Or (FMaxValue <> 0) Then
    CheckRangeL(Value, FMinValue, FMaxValue)
  Else
    CheckRangeL(Value, FMinRange, FMaxRange);
  SetData(@Value);
End;

Procedure TfsIntegerField.SetAsString(Const Value: String);
Var
  E: Integer;
  L: Longint;
  W: Int64;
Begin
  If Value = '' Then
    Clear
  Else
    Begin
      If fsDataType = fstUInt32 Then
        Begin
          Val(Value, W, E);
          If (w < 0) Or (w > maxLongword) Then DatabaseErrorFmt(SInvalidIntegerValue, [Value, DisplayName]);
          If E <> 0 Then DatabaseErrorFmt(SInvalidIntegerValue, [Value, DisplayName]);
          SetAsLongword(W);
        End
      Else
        Begin
          Val(Value, L, E);
          If E <> 0 Then DatabaseErrorFmt(SInvalidIntegerValue, [Value, DisplayName]);
          SetAsInteger(L);
        End;
    End;
End;

Procedure TfsIntegerField.SetMaxValue(Value: Longword);
Begin
  CheckRange(Value, FMinRange, FMaxRange);
  FMaxValue := Value;
End;

Procedure TfsIntegerField.SetMinValue(Value: Longint);
Begin
  CheckRange(Value, FMinRange, FMaxRange);
  FMinValue := Value;
End;

Procedure TfsIntegerField.SetVarValue(Const Value: Variant);
Begin
  If fsDataType = fstUInt32 Then
    SetAsLongword(Value)
  Else
    SetAsInteger(Value);
End;

{ TfsBinaryField }

Constructor TfsBinaryField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  fFsDataType := fstInterval;
End;

Procedure TfsBinaryField.CopyData(Source, Dest: Pointer);
Begin
  POleVariant(Dest)^ := POleVariant(Source)^;
End;

Class Procedure TfsBinaryField.CheckTypeSize(Value: Integer);
Begin
  If (Value = 0) Then DatabaseError(SInvalidFieldSize);
End;

Function TfsBinaryField.GetAsString: String;
Var
  Len: Integer;
  Data: Variant;
  PData: Pointer;
Begin
  Data := GetAsByteArray;
  If VarIsNull(Data) Then
    Result := ''
  Else
    Begin
      Len := VarArrayHighBound(Data, 1) + 1;
      PData := VarArrayLock(Data);
      Try
        SetLength(Result, Len);
        Move(PData^, Pointer(Result)^, Len);
      Finally
        VarArrayUnlock(Data);
      End;
    End;
End;

Procedure TfsBinaryField.SetAsString(Const Value: String);
Var
  Len: Integer;
  Data: Variant;
  PData: Pointer;
Begin
  If Value = '' Then
    Clear
  Else
    Begin
      Len := Length(Value);
      If Len > Size Then Len := Size;
      Data := VarArrayCreate([0, Len - 1], varByte);
      PData := VarArrayLock(Data);
      Try
        Move(Pointer(Value)^, PData^, Len);
      Finally
        VarArrayUnlock(Data);
      End;
      SetAsByteArray(Data);
    End;
End;

Function TfsBinaryField.GetAsVariant: Variant;
Begin
  Result := GetAsByteArray;
End;

Procedure TfsBinaryField.SetVarValue(Const Value: Variant);
Begin
  SetAsByteArray(Value);
End;

Procedure TfsBinaryField.GetText(Var Text: String; DisplayText: Boolean);
Begin
  Text := Inherited GetAsString;
End;

Procedure TfsBinaryField.SetText(Const Value: String);
Begin
  Raise AccessError('Text');
End;

{ TfsArrayField }

Constructor TfsArrayField.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  SetDataType(ftBytes);
  Size := 16;
  fShowArray := False;
  fDecimal := 0;
  fRound := rNone;
End;

Procedure TfsArrayField.SetShowArray(Value: Boolean);
Begin
  If FShowArray <> Value Then
    Begin
      FShowArray := Value;
      PropertyChanged(False);
    End;
End;

Procedure TfsArrayField.SetDecimal(Value: Byte);
Begin
  If Value < 0 Then
    Value := 0;
  If FDecimal <> Value Then
    Begin
      FDecimal := Value;
      PropertyChanged(False);
    End;
End;

Procedure TfsArrayField.CopyData(Source, Dest: Pointer);
Begin
  Move(Source^, Dest^, Size);
End;

Function TfsArrayField.GetDataSize: Integer;
Begin
  If fsDataType = fstArrayUInt8 Then
    Result := Size
  Else If fsDataType = fstArrayUInt16 Then
    Result := Size * 2
  Else If fsDataType = fstArrayInt32 Then
    Result := Size * 4
  Else If fsDataType = fstArrayDouble Then
    Result := Size * 8
  Else
    Result := Size;
End;

Procedure TfsArrayField.GetText(Var Text: String; DisplayText: Boolean);
Var
  Buffer: Array[0..dsfsMaxArraySize] Of Byte;
  wBuffer: Array[0..dsfsMaxWordArraySize] Of Word;
  iBuffer: Array[0..dsfsMaxIntArraySize] Of Longint;
  dBuffer: Array[0..dsFSMaxDoubleArraySize] Of Double;

  SResult: Boolean;
Begin
  If ShowArray Then
    Text := GetAsString
  Else
    Begin
      If fsDataType = fstArrayUInt8 Then
        Begin
          SResult := GetData(@Buffer);
          If SResult Then
            Text := '(ARRAYUINT8)'
          Else
            Text := '(ArrayUInt8)';
        End
      Else If fsDataType = fstArrayUInt16 Then
        Begin
          SResult := GetData(@iBuffer);
          If SResult Then
            Text := '(ARRAYUINT16)'
          Else
            Text := '(ArrayUInt16)';
        End
      Else If fsDataType = fstArrayInt32 Then
        Begin
          SResult := GetData(@wBuffer);
          If SResult Then
            Text := '(ARRAYINT32)'
          Else
            Text := '(ArrayInt32)';
        End
      Else If fsDataType = fstArrayDouble Then
        Begin
          SResult := GetData(@dBuffer);
          If SResult Then
            Text := '(ARRAYDOUBLE)'
          Else
            Text := '(ArrayDouble)';
        End
      Else
        Text := '[ErrorArray]';
    End;
End;

Procedure TfsArrayField.SetText(Const Value: String);
Begin
  SetAsString(Value);
End;

Class Procedure TfsArrayField.CheckTypeSize(Value: Integer);
Begin
  If (Value < 1) Or (Value > dsFSMaxArraySize) Then
    DatabaseError(SInvalidFieldSize);
End;

Function TfsArrayField.GetValue(Var Value: String): Boolean;
Var
  Buffer: Array[0..dsfsMaxArraySize] Of Byte;
  wBuffer: Array[0..dsfsMaxWordArraySize] Of Word;
  iBuffer: Array[0..dsfsMaxIntArraySize] Of Longint;
  dBuffer: Array[0..dsFSMaxDoubleArraySize] Of Double;
  r: TRound;

  Function ByteArrayToString(ByteArray: Pointer; ArrayLength: Integer): String;
  Var
    idx: Integer;
    BArr: PffByteArray Absolute ByteArray;
  Begin
    Result := '';
    Result := IntToStr(BArr[0]);
    For idx := 1 To ArrayLength - 1 Do
      Result := Result + ',' + IntToStr(BArr[idx]);
  End;

  Function WordArrayToString(WordArray: Pointer; ArrayLength: Integer): String;
  Var
    idx: Integer;
    BArr: PffWordArray Absolute WordArray;
  Begin
    Result := '';
    Result := IntToStr(BArr[0]);
    For idx := 1 To ArrayLength - 1 Do
      Result := Result + ',' + IntToStr(BArr[idx]);
  End;

  Function IntegerArrayToString(IntArray: Pointer; ArrayLength: Integer): String;
  Var
    idx: Integer;
    BArr: PffIntArray Absolute intArray;
  Begin
    Result := '';
    Result := IntToStr(BArr[0]);
    For idx := 1 To ArrayLength - 1 Do
      Result := Result + ',' + IntToStr(BArr[idx]);
  End;

  Function DoubleArrayToString(DoubleArray: Pointer; ArrayLength: Integer): String;
  Var
    idx: Integer;
    BArr: PffDoubleArray Absolute DoubleArray;
    S: String;
    D: Extended;
    c: Char;
  Begin
    Result := '';
    D := BArr[0];
    c := DecimalSeparator;
    DecimalSeparator := '.';
    r := fround;
    If r = rNone Then
      r := rMathematical;
    Try
      If decimal > 0 Then
        Begin
          d := RoundExtended(d, decimal, r);
          S := fsFloatToStrF(D, ffFixed, 20, decimal);
        End
      Else
        S := fsFloatToStr(D);
      Result := S;
      For idx := 1 To ArrayLength - 1 Do
        Begin
          D := BArr[idx];
          If decimal > 0 Then
            Begin
              d := RoundExtended(d, decimal, r);
              S := fsFloatToStrF(D, ffFixed, 20, decimal);
            End
          Else
            S := fsFloatToStr(D);
          Result := Result + ',' + S;
        End;
    Finally
      DecimalSeparator := c;
    End;
  End;

Begin
  If fsDataType = fstArrayUInt8 Then
    Begin
      Result := GetData(@Buffer);
      If Result Then
        Value := ByteArrayToString(@Buffer, Size);
    End
  Else If fsDataType = fstArrayUInt16 Then
    Begin
      Result := GetData(@wBuffer);
      If Result Then
        Value := WordArrayToString(@wBuffer, Size);
    End
  Else If fsDataType = fstArrayInt32 Then
    Begin
      Result := GetData(@iBuffer);
      If Result Then
        Value := IntegerArrayToString(@iBuffer, Size);
    End
  Else If fsDataType = fstArrayDouble Then
    Begin
      Result := GetData(@dBuffer);
      If Result Then
        Value := DoubleArrayToString(@dBuffer, Size);
    End
  Else
    Result := False;
  If Not Result Then
    Value := '';
End;

Function TfsArrayField.GetAsString: String;
Begin
  If Not GetValue(Result) Then Result := '';
End;

Function TfsArrayField.GetAsVariant: Variant;
Var
  Buffer: Array[0..dsfsMaxArraySize] Of Byte;
  wBuffer: Array[0..dsfsMaxWordArraySize] Of Word;
  iBuffer: Array[0..dsfsMaxIntArraySize] Of Longint;
  dBuffer: Array[0..dsFSMaxDoubleArraySize] Of Double;

  V: OleVariant;

  Procedure BufferToByteArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);
  Var
    PVarData: Pointer;
  Begin
    VarArray := VarArrayCreate([0, DataSize - 1], varByte);
    PVarData := VarArrayLock(VarArray);
    Try
      Move(Data^, PVarData^, DataSize);
    Finally
      VarArrayUnlock(VarArray);
    End;
  End;

  Procedure BufferToWordArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);
  Var
    PVarData: Pointer;

    Procedure WordArrayToVariant(WordArray: Pointer; ArrayLength: Integer);
    Var
      idx: Integer;
      BArr: PffWordArray Absolute WordArray;
    Begin
      For idx := 0 To ArrayLength - 1 Do
        Result[idx] := BArr[idx];
    End;
  Begin
    VarArray := VarArrayCreate([0, DataSize - 1], varInteger);
    Result := VarArray;
    PVarData := VarArrayLock(VarArray);
    Try
      WordArrayToVariant(Data, DataSize);
    Finally
      VarArrayUnlock(VarArray);
    End;
  End;

  Procedure BufferToIntArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);
  Var
    PVarData: Pointer;
  Begin
    VarArray := VarArrayCreate([0, DataSize - 1], varInteger);
    PVarData := VarArrayLock(VarArray);
    Try
      Move(Data^, PVarData^, DataSize);
    Finally
      VarArrayUnlock(VarArray);
    End;
  End;

  Procedure BufferToDoubleArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);
  Var
    PVarData: Pointer;

    Procedure DoubleArrayToVariant(DoubleArray: Pointer; ArrayLength: Integer);
    Var
      idx: Integer;
      BArr: PffDoubleArray Absolute DoubleArray;
      r: TRound;
    Begin
      r := fround;
      If r = rNone Then
        r := rMathematical;
      For idx := 0 To ArrayLength - 1 Do
        Begin
          If decimal > 0 Then
            Result[idx] := RoundExtended(BArr[idx], decimal, r)
          Else
            Result[idx] := BArr[idx];
        End;
    End;
  Begin
    VarArray := VarArrayCreate([0, DataSize - 1], varDouble);
    Result := VarArray;
    PVarData := VarArrayLock(VarArray);
    Try
      DoubleArrayToVariant(Data, DataSize);
    Finally
      VarArrayUnlock(VarArray);
    End;
  End;

Begin
  If fsDataType = fstArrayUInt8 Then
    Begin
      Result := GetData(@Buffer);
      If Result Then
        Begin
          BufferToByteArray(@Buffer, Size, V);
          Result := V;
        End
      Else
        Result := Null;
    End
  Else If fsDataType = fstArrayUInt16 Then
    Begin
      Result := GetData(@wBuffer);
      If Result Then
        BufferToWordArray(@wBuffer, Size, V)
      Else
        Result := Null;
    End
  Else If fsDataType = fstArrayInt32 Then
    Begin
      Result := GetData(@iBuffer);
      If Result Then
        Begin
          BufferToIntArray(@iBuffer, Size, V);
          Result := V;
        End
      Else
        Result := Null;
    End
  Else If fsDataType = fstArrayDouble Then
    Begin
      Result := GetData(@dBuffer);
      If Result Then
        BufferToDoubleArray(@dBuffer, Size, V)
      Else
        Result := Null;
    End
  Else
    Result := Null;
End;

Function TfsArrayField.GetAValue(Index: Integer): Variant;
Var
  Buffer: Array[0..dsfsMaxArraySize] Of Byte;
  wBuffer: Array[0..dsfsMaxWordArraySize] Of Word;
  iBuffer: Array[0..dsfsMaxIntArraySize] Of Longint;
  dBuffer: Array[0..dsFSMaxDoubleArraySize] Of Double;
  Rt: Boolean;

  Procedure vIntArray(IntArray: Pointer; ArrayLength: Integer; Var V: Variant);
  Var
    BArr: PffIntArray Absolute IntArray;
  Begin
    If (Index >= 0) And (Index <= ArrayLength - 1) Then
      V := BArr[Index]
    Else
      DatabaseError('Invalid Array Index');
  End;

  Procedure vDoubleArray(DoubleArray: Pointer; ArrayLength: Integer; Var V: Variant);
  Var
    BArr: PffDoubleArray Absolute DoubleArray;
    r: TRound;
  Begin
    r := fround;
    If r = rNone Then
      r := rMathematical;
    If (Index >= 0) And (Index <= ArrayLength - 1) Then
      Begin
        If decimal > 0 Then
          V := RoundExtended(BArr[Index], decimal, r)
        Else
          V := BArr[Index];
      End
    Else
      DatabaseError('Invalid Array Index');
  End;

  Procedure vWordArray(WordArray: Pointer; ArrayLength: Integer; Var V: Variant);
  Var
    BArr: PffWordArray Absolute WordArray;
  Begin
    If (Index >= 0) And (Index <= ArrayLength - 1) Then
      V := BArr[Index]
    Else
      DatabaseError('Invalid Array Index');
  End;

  Procedure vByteArray(ByteArray: Pointer; ArrayLength: Integer; Var V: Variant);
  Var
    BArr: PffByteArray Absolute ByteArray;
  Begin
    If (Index >= 0) And (Index <= ArrayLength - 1) Then
      V := BArr[Index]
    Else
      DatabaseError('Invalid Array Index');
  End;
Begin
  If fsDataType = fstArrayUInt8 Then
    Begin
      rt := GetData(@Buffer);
      If Not Rt Then
        Result := null
      Else
        vByteArray(@Buffer, Size, Result);
    End
  Else If fsDataType = fstArrayUInt16 Then
    Begin
      rt := GetData(@wBuffer);
      If Not Rt Then
        Result := null
      Else
        vWordArray(@wBuffer, Size, Result);
    End
  Else If fsDataType = fstArrayInt32 Then
    Begin
      rt := GetData(@iBuffer);
      If Not Rt Then
        Result := null
      Else
        vIntArray(@iBuffer, Size, Result);
    End
  Else If fsDataType = fstArrayDouble Then
    Begin
      rt := GetData(@dBuffer);
      If Not Rt Then
        Result := null
      Else
        vDoubleArray(@dBuffer, Size, Result);
    End;
End;

Procedure TfsArrayField.SetAValue(Index: Integer; Value: Variant);
Var
  Buffer: Array[0..dsfsMaxArraySize] Of Byte;
  wBuffer: Array[0..dsfsMaxWordArraySize] Of Word;
  iBuffer: Array[0..dsfsMaxIntArraySize] Of Longint;
  dBuffer: Array[0..dsFSMaxDoubleArraySize] Of Double;
  Rt: Boolean;

  Procedure ToIntArray(IntArray: Pointer; ArrayLength: Integer; Reset: boolean);
  Var
    BArr: PffIntArray Absolute IntArray;
    idx: Integer;
  Begin
    If Value = null Then
      Exit;
    If Reset Then
      For idx := 0 To ArrayLength - 1 Do
        BArr[idx] := 0;
    If (Index >= 0) And (Index <= ArrayLength - 1) Then
      Begin
        BArr[Index] := Value;
        SetData(@iBuffer);
      End
    Else
      DatabaseError('Invalid Array Index');
  End;

  Procedure ToDoubleArray(DoubleArray: Pointer; ArrayLength: Integer; Reset: boolean);
  Var
    BArr: PffDoubleArray Absolute DoubleArray;
    idx: Integer;
    r: TRound;
    e: Extended;
  Begin
    r := fround;
    If r = rNone Then
      r := rMathematical;
    If Value = null Then
      Exit;
    If Reset Then
      For idx := 0 To ArrayLength - 1 Do
        BArr[idx] := 0;
    If (Index >= 0) And (Index <= ArrayLength - 1) Then
      Begin
        If Decimal > 0 Then
          Begin
            e := RoundExtended(Value, decimal, r);
            BArr[Index] := e;
          End
        Else
          BArr[Index] := Value;
        SetData(@dBuffer);
      End
    Else
      DatabaseError('Invalid Array Index');
  End;

  Procedure ToWordArray(WordArray: Pointer; ArrayLength: Integer; Reset: boolean);
  Var
    BArr: PffWordArray Absolute WordArray;
    idx: Integer;
  Begin
    If Value = null Then
      Exit;
    If Reset Then
      For idx := 0 To ArrayLength - 1 Do
        BArr[idx] := 0;
    If (Index >= 0) And (Index <= ArrayLength - 1) Then
      Begin
        BArr[Index] := Value;
        SetData(@wBuffer);
      End
    Else
      DatabaseError('Invalid Array Index');
  End;

  Procedure ToByteArray(ByteArray: Pointer; ArrayLength: Integer; Reset: boolean);
  Var
    BArr: PffByteArray Absolute ByteArray;
    idx: Integer;
  Begin
    If Value = null Then
      Exit;
    If Reset Then
      For idx := 0 To ArrayLength - 1 Do
        BArr[idx] := 0;
    If (Index >= 0) And (Index <= ArrayLength - 1) Then
      Begin
        BArr[Index] := Value;
        SetData(@Buffer);
      End
    Else
      DatabaseError('Invalid Array Index');
  End;
Begin
  If fsDataType = fstArrayUInt8 Then
    Begin
      rt := Not GetData(@Buffer);
      ToByteArray(@Buffer, Size, rt);
    End
  Else If fsDataType = fstArrayUInt16 Then
    Begin
      rt := Not GetData(@wBuffer);
      ToWordArray(@wBuffer, Size, rt);
    End
  Else If fsDataType = fstArrayInt32 Then
    Begin
      rt := Not GetData(@iBuffer);
      ToIntArray(@iBuffer, Size, rt);
    End
  Else If fsDataType = fstArrayDouble Then
    Begin
      rt := Not GetData(@dBuffer);
      ToDoubleArray(@dBuffer, Size, rt);
    End;
End;

Procedure TfsArrayField.SetAsString(Const Value: String);
Var
  Buffer: Array[0..dsfsMaxArraySize] Of Byte;
  wBuffer: Array[0..dsfsMaxWordArraySize] Of Word;
  iBuffer: Array[0..dsfsMaxIntArraySize] Of Longint;
  dBuffer: Array[0..dsFSMaxDoubleArraySize] Of Double;

  Procedure StringToIntArray(IntArray: Pointer; ArrayLength: Integer; S: String);
  Var
    idx: Integer;
    BArr: PffIntArray Absolute IntArray;

    Function ExtractArray(Const aStrinArray: String): String;
    Var
      iPos: Longint;

      Function EArray(Const aArray: String; Var aPos: Longint): String;
      Var
        I: Longint;
      Begin
        I := aPos;
        While (I <= Length(aArray)) And (aArray[I] <> ',') Do
          Inc(I);
        Result := Trim(Copy(aArray, aPos, I - aPos));
        If (I <= Length(aArray)) And (aArray[I] = ',') Then
          Inc(I);
        aPos := I;
      End;
    Begin
      Result := '';
      iPos := 1;
      idx := 0;
      While iPos <= Length(aStrinArray) Do
        Begin
          If idx <= ArrayLength - 1 Then
            BArr[idx] := StrToInt(EArray(aStrinArray, iPos))
          Else
            System.Break;
          inc(idx);
        End;
    End;
  Begin
    If Length(Trim(S)) = 0 Then
      Begin
        SetData(Nil);
        Exit;
      End;
    For idx := 0 To ArrayLength - 1 Do
      BArr[idx] := 0;
    ExtractArray(Trim(S));
    SetData(@iBuffer);
  End;

  Procedure StringToDoubleArray(DoubleArray: Pointer; ArrayLength: Integer; S: String);
  Var
    idx, ICode: Integer;
    BArr: PffDoubleArray Absolute DoubleArray;
    D: Extended;
    r: TRound;

    Function ExtractArray(Const aStrinArray: String): String;
    Var
      iPos: Longint;

      Function EArray(Const aArray: String; Var aPos: Longint): String;
      Var
        I: Longint;
      Begin
        I := aPos;
        While (I <= Length(aArray)) And (aArray[I] <> ',') Do
          Inc(I);
        Result := Trim(Copy(aArray, aPos, I - aPos));
        If (I <= Length(aArray)) And (aArray[I] = ',') Then
          Inc(I);
        aPos := I;
      End;
    Begin
      Result := '';
      iPos := 1;
      idx := 0;
      r := fround;
      If r = rNone Then
        r := rMathematical;
      While iPos <= Length(aStrinArray) Do
        Begin
          If idx <= ArrayLength - 1 Then
            Begin
              fsStringToExtended(EArray(aStrinArray, iPos), D, ICode);
              If iCode = 0 Then
                Begin
                  If decimal > 0 Then
                    Begin
                      D := RoundExtended(D, decimal, r);
                      BArr[idx] := D;
                    End
                  Else
                    BArr[idx] := D;
                End
              Else
                DatabaseError(SInvalidFloatValue);
            End
          Else
            System.Break;
          inc(idx);
        End;
    End;
  Begin
    If Length(Trim(S)) = 0 Then
      Begin
        SetData(Nil);
        Exit;
      End;
    For idx := 0 To ArrayLength - 1 Do
      BArr[idx] := 0;
    ExtractArray(Trim(S));
    SetData(@dBuffer);
  End;

  Procedure StringToWordArray(WordArray: Pointer; ArrayLength: Integer; S: String);
  Var
    idx: Integer;
    BArr: PffWordArray Absolute WordArray;

    Function ExtractArray(Const aStrinArray: String): String;
    Var
      iPos: Longint;

      Function EArray(Const aArray: String; Var aPos: Longint): String;
      Var
        I: Longint;
      Begin
        I := aPos;
        While (I <= Length(aArray)) And (aArray[I] <> ',') Do
          Inc(I);
        Result := Trim(Copy(aArray, aPos, I - aPos));
        If (I <= Length(aArray)) And (aArray[I] = ',') Then
          Inc(I);
        aPos := I;
      End;
    Begin
      Result := '';
      iPos := 1;
      idx := 0;
      While iPos <= Length(aStrinArray) Do
        Begin
          If idx <= ArrayLength - 1 Then
            BArr[idx] := StrToInt(EArray(aStrinArray, iPos))
          Else
            System.Break;
          inc(idx);
        End;
    End;
  Begin
    If Length(Trim(S)) = 0 Then
      Begin
        SetData(Nil);
        Exit;
      End;
    For idx := 0 To ArrayLength - 1 Do
      BArr[idx] := 0;
    ExtractArray(Trim(S));
    SetData(@wBuffer);
  End;

  Procedure StringToByteArray(ByteArray: Pointer; ArrayLength: Integer; S: String);
  Var
    idx: Integer;
    BArr: PffByteArray Absolute ByteArray;

    Function ExtractArray(Const aStrinArray: String): String;
    Var
      iPos: Longint;

      Function EArray(Const aArray: String; Var aPos: Longint): String;
      Var
        I: Longint;
      Begin
        I := aPos;
        While (I <= Length(aArray)) And (aArray[I] <> ',') Do
          Inc(I);
        Result := Trim(Copy(aArray, aPos, I - aPos));
        If (I <= Length(aArray)) And (aArray[I] = ',') Then
          Inc(I);
        aPos := I;
      End;
    Begin
      Result := '';
      iPos := 1;
      idx := 0;
      While iPos <= Length(aStrinArray) Do
        Begin
          If idx <= ArrayLength - 1 Then
            BArr[idx] := StrToInt(EArray(aStrinArray, iPos))
          Else
            System.Break;
          inc(idx);
        End;
    End;
  Begin
    If Length(Trim(S)) = 0 Then
      Begin
        SetData(Nil);
        Exit;
      End;
    For idx := 0 To ArrayLength - 1 Do
      BArr[idx] := 0;
    ExtractArray(Trim(S));
    SetData(@Buffer);
  End;
Begin
  If fsDataType = fstArrayUInt8 Then
    StringToByteArray(@Buffer, Size, Value)
  Else If fsDataType = fstArrayUInt16 Then
    StringToWordArray(@wBuffer, Size, Value)
  Else If fsDataType = fstArrayInt32 Then
    StringToIntArray(@iBuffer, Size, Value)
  Else If fsDataType = fstArrayDouble Then
    StringToDoubleArray(@dBuffer, Size, Value);
End;

// TfsParams
{ TfsParams }

Constructor TfsParams.Create;
Begin
  FOwner := Nil;
  Inherited Create(TfsParam);
End;

Constructor TfsParams.Create(Owner: TPersistent);
Begin
  FOwner := Owner;
  Inherited Create(TfsParam);
End;

Procedure TfsParams.Update(Item: TCollectionItem);
Var
  i: Integer;
Begin
  For i := 0 To Count - 1 Do
    Items[i].FParamRef := Nil;
  Inherited Update(Item);
End;

Function TfsParams.GetItem(Index: Integer): TfsParam;
Begin
  Result := TfsParam(Inherited Items[Index]);
  Result := Result.ParamRef;
End;

Procedure TfsParams.SetItem(Index: Integer; Value: TfsParam);
Begin
  Inherited SetItem(Index, TCollectionItem(Value));
End;

Function TfsParams.GetOwner: TPersistent;
Begin
  Result := FOwner;
End;

Function TfsParams.GetDataSet: TDataSet;
Begin
  If FOwner Is TDataSet Then
    Result := TDataSet(FOwner)
  Else
    Result := Nil;
End;

Procedure TfsParams.AssignTo(Dest: TPersistent);
Begin
  If Dest Is TfsParams Then
    TfsParams(Dest).Assign(Self)
  Else
    Inherited AssignTo(Dest);
End;

Procedure TfsParams.AssignValues(Value: TfsParams);
Var
  I: Integer;
  P: TfsParam;
Begin
  For I := 0 To Value.Count - 1 Do
    Begin
      P := FindParam(Value[I].Name);
      If P <> Nil Then
        P.Assign(Value[I]);
    End;
End;

Procedure TfsParams.AddParam(Value: TfsParam);
Begin
  Value.Collection := Self;
End;

Procedure TfsParams.RemoveParam(Value: TfsParam);
Begin
  Value.Collection := Nil;
End;

Function TfsParams.CreateParam(FldType: TFieldType; Const ParamName: String;
  ParamType: TfsParamType): TfsParam;
Begin
  Result := Add As TfsParam;
  Result.ParamType := ParamType;
  Result.Name := ParamName;
  Result.DataType := FldType;
End;

Function TfsParams.IsEqual(Value: TfsParams): Boolean;
Var
  I: Integer;
Begin
  Result := Count = Value.Count;
  If Result Then
    For I := 0 To Count - 1 Do
      Begin
        Result := Items[I].IsEqual(Value.Items[I]);
        If Not Result Then Break;
      End
End;

Function TfsParams.ParamByName(Const Value: String): TfsParam;
Begin
  Result := FindParam(Value);
  If Result = Nil Then
    DatabaseErrorFmt(SParameterNotFound, [Value], GetDataSet);
End;

Function TfsParams.FindParam(Const Value: String): TfsParam;
Var
  I: Integer;
Begin
  For I := 0 To Count - 1 Do
    Begin
      Result := TfsParam(Inherited Items[I]);
      If AnsiCompareText(Result.Name, Value) = 0 Then Exit;
    End;
  Result := Nil;
End;

Procedure TfsParams.DefineProperties(Filer: TFiler);
Begin
  Inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadBinaryData, Nil, False);
End;

Procedure TfsParams.ReadBinaryData(Stream: TStream);
Var
  I, Temp, NumItems: Integer;
  Buffer: Array[0..2047] Of Char;
  TempStr: String;
  Version: Word;
  Bool: Boolean;
Begin
  Clear;
  With Stream Do
    Begin
      ReadBuffer(Version, SizeOf(Version));
      If Version > 2 Then DatabaseError(SInvalidVersion);
      NumItems := 0;
      If Version = 2 Then
        ReadBuffer(NumItems, SizeOf(NumItems))
      Else
        ReadBuffer(NumItems, 2);
      For I := 0 To NumItems - 1 Do
        With TfsParam(Add) Do
          Begin
            Temp := 0;
            If Version = 2 Then
              ReadBuffer(Temp, SizeOf(Temp))
            Else
              ReadBuffer(Temp, 1);
            SetLength(TempStr, Temp);
            ReadBuffer(PChar(TempStr)^, Temp);
            Name := TempStr;
            ReadBuffer(FParamType, SizeOf(FParamType));
            ReadBuffer(FDataType, SizeOf(FDataType));
            If DataType <> ftUnknown Then
              Begin
                Temp := 0;
                If Version = 2 Then
                  ReadBuffer(Temp, SizeOf(Temp))
                Else
                  ReadBuffer(Temp, 2);
                ReadBuffer(Buffer, Temp);
                If DataType In [ftBlob, ftGraphic..ftTypedBinary, ftOraBlob, ftOraClob] Then
                  SetBlobData(@Buffer, Temp)
                Else
                  SetData(@Buffer);
              End;
            ReadBuffer(Bool, SizeOf(Bool));
            If Bool Then FData := NULL;
            ReadBuffer(FBound, SizeOf(FBound));
          End;
    End;
End;

Function TfsParams.GeTfsParamValue(Const ParamName: String): Variant;
Var
  I: Integer;
  Params: TList;
Begin
  If Pos(';', ParamName) <> 0 Then
    Begin
      Params := TList.Create;
      Try
        GeTfsParamList(Params, ParamName);
        Result := VarArrayCreate([0, Params.Count - 1], varVariant);
        For I := 0 To Params.Count - 1 Do
          Result[I] := TfsParam(Params[I]).Value;
      Finally
        Params.Free;
      End;
    End
  Else
    Result := ParamByName(ParamName).Value
End;

Procedure TfsParams.SeTfsParamValue(Const ParamName: String;
  Const Value: Variant);
Var
  I: Integer;
  Params: TList;
Begin
  If Pos(';', ParamName) <> 0 Then
    Begin
      Params := TList.Create;
      Try
        GeTfsParamList(Params, ParamName);
        For I := 0 To Params.Count - 1 Do
          TfsParam(Params[I]).Value := Value[I];
      Finally
        Params.Free;
      End;
    End
  Else
    ParamByName(ParamName).Value := Value;
End;

Procedure TfsParams.GeTfsParamList(List: TList; Const ParamNames: String);
Var
  Pos: Integer;
Begin
  Pos := 1;
  While Pos <= Length(ParamNames) Do
    List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
End;

Function TfsParams.ParseSQL(SQL: String; DoCreate: Boolean): String;
Const
  Literals = ['''', '"', '`'];
Var
  Value, CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: String;

  Function NameDelimiter: Boolean;
  Begin
    Result := CurChar In [' ', ',', ';', ')', #13, #10];
  End;

  Function IsLiteral: Boolean;
  Begin
    Result := CurChar In Literals;
  End;

  Function StripLiterals(Buffer: PChar): String;
  Var
    Len: Word;
    TempBuf: PChar;

    Procedure StripChar;
    Begin
      If TempBuf^ In Literals Then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      If TempBuf[StrLen(TempBuf) - 1] In Literals Then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    End;

  Begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    Result := '';
    Try
      StrCopy(TempBuf, Buffer);
      StripChar;
      Result := StrPas(TempBuf);
    Finally
      FreeMem(TempBuf, Len);
    End;
  End;

Begin
  Result := SQL;
  Value := PChar(Result);
  If DoCreate Then Clear;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  Repeat
    While (CurPos^ In LeadBytes) Do
      Inc(CurPos, 2);
    CurChar := CurPos^;
    If (CurChar = ':') And Not Literal And ((CurPos + 1)^ <> ':') Then
      Begin
        StartPos := CurPos;
        While (CurChar <> #0) And (Literal Or Not NameDelimiter) Do
          Begin
            Inc(CurPos);
            While (CurPos^ In LeadBytes) Do
              Inc(CurPos, 2);
            CurChar := CurPos^;
            If IsLiteral Then
              Begin
                Literal := Literal Xor True;
                If CurPos = StartPos + 1 Then EmbeddedLiteral := True;
              End;
          End;
        CurPos^ := #0;
        If EmbeddedLiteral Then
          Begin
            Name := StripLiterals(StartPos + 1);
            EmbeddedLiteral := False;
          End
        Else
          Name := StrPas(StartPos + 1);
        If DoCreate Then
          TfsParam(Add).Name := Name;
        CurPos^ := CurChar;
        StartPos^ := '?';
        Inc(StartPos);
        StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
        CurPos := StartPos;
      End
    Else If (CurChar = ':') And Not Literal And ((CurPos + 1)^ = ':') Then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    Else If IsLiteral Then
      Literal := Literal Xor True;
    Inc(CurPos);
  Until CurChar = #0;
End;

{ TfsParam }

Constructor TfsParam.Create(Collection: TCollection);
Begin
  Inherited Create(Collection);
  ParamType := ptUnknown;
  DataType := ftUnknown;
  FData := Unassigned;
  FBound := False;
  FNull := True;
  FNativeStr := '';
End;

Constructor TfsParam.Create(AParams: TfsParams; AParamType: TfsParamType);
Begin
  Create(AParams);
  ParamType := ParamType;
End;

Function TfsParam.IsEqual(Value: TfsParam): Boolean;
Begin
  Result := (VarType(FData) = VarType(Value.FData)) And
    (VarIsEmpty(FData) Or (FData = Value.FData)) And
    (Name = Value.Name) And (DataType = Value.DataType) And
    (IsNull = Value.IsNull) And (Bound = Value.Bound) And
    (ParamType = Value.ParamType);
End;

Function TfsParam.IsParamStored: Boolean;
Begin
  Result := Bound;
End;

Function TfsParam.ParamRef: TfsParam;
Begin
  If Not Assigned(FParamRef) Then
    If Assigned(Collection) And (Name <> '') Then
      FParamRef := TfsParams(Collection).ParamByName(Name)
    Else
      FParamRef := Self;
  Result := FParamRef;
End;

Function TfsParam.GetIsNull: Boolean;
Begin
  Result := FNull Or VarIsNull(FData) Or VarIsEmpty(FData);
End;

Function TfsParam.GeTfsParamType: TfsParamType;
Begin
  Result := ParamRef.FParamType;
End;

Procedure TfsParam.SeTfsParamType(Value: TfsParamType);
Begin
  ParamRef.FParamType := Value;
End;

Function TfsParam.GetDataType: TFieldType;
Begin
  Result := ParamRef.FDataType;
End;

Procedure TfsParam.SetDataType(Value: TFieldType);
Const
  {$IFDEF IsVerDataset1} //d5
  VarTypeMap: Array[TFieldType] Of Integer = (varError, varOleStr, varSmallint,
    varInteger, varSmallint, varBoolean, varDouble, varCurrency, varCurrency,
    varDate, varDate, varDate, varOleStr, varOleStr, varInteger, varOleStr,
    varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varError,
    varOleStr, varOleStr, varError, varError, varError, varError, varError,
    varOleStr, varOleStr, varVariant, varUnknown, varDispatch, varOleStr);
  {$ENDIF}
  {$IFDEF IsVerDataset2} //d6-2005
  VarTypeMap: Array[TFieldType] Of Integer = (varError, varOleStr, varSmallint,
    varInteger, varSmallint, varBoolean, varDouble, varCurrency, varCurrency,
    varDate, varDate, varDate, varOleStr, varOleStr, varInteger, varOleStr,
    varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varError,
    varOleStr, varOleStr, varError, varError, varError, varError, varError,
    varOleStr, varOleStr, varVariant, varUnknown, varDispatch, varOleStr,
    varDate, varVariant);
  {$ENDIF}
  {$IFDEF IsVerDataset3} // d2006
  VarTypeMap: Array[TFieldType] Of Integer = (varError, varOleStr, varSmallint,
    varInteger, varSmallint, varBoolean, varDouble, varCurrency, varCurrency,
    varDate, varDate, varDate, varOleStr, varOleStr, varInteger, varOleStr,
    varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varOleStr, varError,
    varOleStr, varOleStr, varError, varError, varError, varError, varError,
    varOleStr, varOleStr, varVariant, varUnknown, varDispatch, varOleStr,
    varDate, varVariant,
    varSmallint, varOleStr, varDate, varUnknown);
  {$ENDIF}
Var
  vType: Integer;
Begin
  ParamRef.FDataType := Value;
  If Assigned(DataSet) And (csDesigning In DataSet.ComponentState) And
    (Not ParamRef.IsNull) Then
    Begin
      vType := VarTypeMap[Value];
      If vType <> varError Then
        Try
          VarCast(ParamRef.FData, ParamRef.FData, vType);
        Except
          ParamRef.Clear;
        End
      Else
        ParamRef.Clear;
    End
  Else
    ParamRef.Clear;
End;

Function TfsParam.GetDataSize: Integer;
Begin
  Result := 0;
  Case DataType Of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
    ftString, ftFixedChar, ftMemo: Result := Length(VarToStr(FData)) + 1;
    ftBoolean: Result := SizeOf(WordBool);
    ftBCD: Result := 8; //SizeOf(TBcd);
    ftDateTime,
      ftCurrency,
      ftFloat: Result := SizeOf(Double);
    ftTime,
      ftDate,
      ftAutoInc,
      ftInteger: Result := SizeOf(Integer);
    ftSmallint: Result := SizeOf(Smallint);
    ftWord: Result := SizeOf(Word);
    ftBytes, ftVarBytes:
      If VarIsArray(FData) Then
        Result := VarArrayHighBound(FData, 1) + 1
      Else
        Result := 0;
    ftBlob, ftGraphic..ftTypedBinary, ftOraClob, ftOraBlob: Result := Length(VarToStr(FData));
    ftADT, ftArray, ftDataSet,
      ftReference, ftCursor: Result := 0;
    Else
      DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  End;
End;

Procedure TfsParam.GetData(Buffer: Pointer);
Var
  P: Pointer;
Begin
  Case DataType Of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
    ftString, ftFixedChar, ftMemo:
      StrMove(Buffer, PChar(GetAsString), Length(GetAsString) + 1);
    ftSmallint: Smallint(Buffer^) := GetAsInteger;
    ftWord: Word(Buffer^) := GetAsInteger;
    ftAutoInc,
      ftInteger: Integer(Buffer^) := GetAsInteger;
    ftTime: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Time;
    ftDate: Integer(Buffer^) := DateTimeToTimeStamp(AsDateTime).Date;
    ftDateTime: Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(AsDateTime));
    ftBCD: Double(Buffer^) := GetAsFloat; //CurrToBCD(AsBcd, TBcd(Buffer^));
    ftCurrency,
      ftFloat: Double(Buffer^) := GetAsFloat;
    ftBoolean: Word(Buffer^) := Ord(GetAsBoolean);
    ftBytes, ftVarBytes:
      Begin
        If VarIsArray(FData) Then
          Begin
            P := VarArrayLock(FData);
            Try
              Move(P^, Buffer^, VarArrayHighBound(FData, 1) + 1);
            Finally
              VarArrayUnlock(FData);
            End;
          End;
      End;
    ftBlob, ftGraphic..ftTypedBinary, ftOraBlob, ftOraClob:
      Move(PChar(GetAsString)^, Buffer^, Length(GetAsString));
    ftADT, ftArray, ftDataSet,
      ftReference, ftCursor: {Nothing};
    Else
      DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  End;
End;

Procedure TfsParam.SetBlobData(Buffer: Pointer; Size: Integer);
Var
  DataStr: String;
Begin
  SetLength(DataStr, Size);
  Move(Buffer^, PChar(DataStr)^, Size);
  AsBlob := DataStr;
End;

Procedure TfsParam.SetData(Buffer: Pointer);
Var
  TimeStamp: TTimeStamp;
Begin
  Case DataType Of
    ftUnknown: DatabaseErrorFmt(SUnknownFieldType, [Name], DataSet);
    ftString, ftFixedChar: AsString := StrPas(Buffer);
    ftWord: AsWord := Word(Buffer^);
    ftSmallint: AsSmallInt := Smallint(Buffer^);
    ftInteger, ftAutoInc: AsInteger := Integer(Buffer^);
    ftTime:
      Begin
        TimeStamp.Time := Longint(Buffer^);
        TimeStamp.Date := DateDelta;
        AsTime := TimeStampToDateTime(TimeStamp);
      End;
    ftDate:
      Begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
        AsDate := TimeStampToDateTime(TimeStamp);
      End;
    ftDateTime:
      Begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
        AsDateTime := TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)));
      End;
    {ftBCD:
      If BCDToCurr(TBcd(Buffer^), Value) then
        AsBcd := Value
      Else
        AsBcd := 0;  }
    ftCurrency: AsCurrency := Double(Buffer^);
    ftFloat: AsFloat := Double(Buffer^);
    ftBoolean: AsBoolean := WordBool(Buffer^);
    ftMemo: AsMemo := StrPas(Buffer);
    ftCursor: FData := 0;
    Else
      DatabaseErrorFmt(SBadFieldType, [Name], DataSet);
  End;
End;

Procedure TfsParam.SetText(Const Value: String);
Begin
  Self.Value := Value;
End;

Procedure TfsParam.Assign(Source: TPersistent);

  Procedure LoadFromBitmap(Bitmap: TBitmap);
  Var
    MS: TMemoryStream;
  Begin
    MS := TMemoryStream.Create;
    Try
      Bitmap.SaveToStream(MS);
      LoadFromStream(MS, ftGraphic);
    Finally
      MS.Free;
    End;
  End;

  Procedure LoadFromStrings(Source: TSTrings);
  Begin
    AsMemo := Source.Text;
  End;

Begin
  If Source Is TfsParam Then
    AssignParam(TfsParam(Source))
  Else If Source Is TField Then
    AssignField(TField(Source))
  Else If Source Is TStrings Then
    LoadFromStrings(TStrings(Source))
  Else If Source Is TBitmap Then
    LoadFromBitmap(TBitmap(Source))
  Else If (Source Is TPicture) And (TPicture(Source).Graphic Is TBitmap) Then
    LoadFromBitmap(TBitmap(TPicture(Source).Graphic))
  Else
    Inherited Assign(Source);
End;

Procedure TfsParam.AssignTo(Dest: TPersistent);
Begin
  If Dest Is TField Then
    TField(Dest).Value := FData
  Else
    Inherited AssignTo(Dest);
End;

Procedure TfsParam.AssignParam(Param: TfsParam);
Begin
  If Param <> Nil Then
    Begin
      FDataType := Param.DataType;
      If Param.IsNull Then
        Clear
      Else
        Value := Param.FData;
      FBound := Param.Bound;
      Name := Param.Name;
      If ParamType = ptUnknown Then ParamType := Param.ParamType;
    End;
End;

Procedure TfsParam.AssignFieldValue(Field: TField; Const Value: Variant);
Begin
  If Field <> Nil Then
    Begin
      If (Field.DataType = ftString) And TStringField(Field).FixedChar Then
        DataType := ftFixedChar
      Else If (Field.DataType = ftMemo) And (Field.Size > 255) Then
        DataType := ftString
      Else
        DataType := Field.DataType;
      If VarIsNull(Value) Then
        Clear
      Else
        Self.Value := Value;
      FBound := True;
    End;
End;

Procedure TfsParam.AssignField(Field: TField);
Begin
  If Field <> Nil Then
    Begin
      AssignFieldValue(Field, Field.Value);
      Name := Field.FieldName;
    End;
End;

Procedure TfsParam.Clear;
Begin
  FNull := True;
  FData := Unassigned;
End;

Function TfsParam.GetDataSet: TDataSet;
Begin
  If Not Assigned(Collection) Then
    Result := Nil
  Else
    Result := TfsParams(Collection).GetDataSet;
End;

Function TfsParam.GetDisplayName: String;
Begin
  If FName = '' Then
    Result := Inherited GetDisplayName
  Else
    Result := FName;
End;

Procedure TfsParam.SetAsBoolean(Value: Boolean);
Begin
  FDataType := ftBoolean;
  Self.Value := Value;
End;

Function TfsParam.GetAsBoolean: Boolean;
Begin
  If IsNull Then
    Result := False
  Else
    Result := FData;
End;

Procedure TfsParam.SetAsFloat(Const Value: Double);
Begin
  FDataType := ftFloat;
  Self.Value := Value;
End;

Function TfsParam.GetAsFloat: Double;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := FData;
End;

Procedure TfsParam.SetAsCurrency(Const Value: Currency);
Begin
  FDataType := ftCurrency;
  Self.Value := Value;
End;

Function TfsParam.GetAsCurrency: Currency;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := FData;
End;

Procedure TfsParam.SetAsBCD(Const Value: Currency);
Begin
  FDataType := ftBCD;
  Self.Value := Value;
End;

Function TfsParam.GetAsBCD: Currency;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := FData;
End;

Procedure TfsParam.SetAsInteger(Value: Longint);
Begin
  FDataType := ftInteger;
  Self.Value := Value;
End;

Function TfsParam.GetAsInteger: Longint;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := FData;
End;

Procedure TfsParam.SetAsWord(Value: Longint);
Begin
  FDataType := ftWord;
  Self.Value := Value;
End;

Procedure TfsParam.SetAsSmallInt(Value: Longint);
Begin
  FDataType := ftSmallint;
  Self.Value := Value;
End;

Procedure TfsParam.SetAsString(Const Value: String);
Begin
  FDataType := ftString;
  Self.Value := Value;
End;

Function TfsParam.GetAsString: String;
Begin
  If IsNull Then
    Result := ''
  Else If DataType = ftBoolean Then
    Begin
      If FData Then
        Result := STextTrue
      Else
        Result := STextFalse;
    End
  Else
    Result := FData;
End;

Procedure TfsParam.SetAsDate(Const Value: TDateTime);
Begin
  FDataType := ftDate;
  Self.Value := Value;
End;

Procedure TfsParam.SetAsTime(Const Value: TDateTime);
Begin
  FDataType := ftTime;
  Self.Value := Value
End;

Procedure TfsParam.SetAsDateTime(Const Value: TDateTime);
Begin
  FDataType := ftDateTime;
  Self.Value := Value
End;

Function TfsParam.GetAsDateTime: TDateTime;
Begin
  If IsNull Then
    Result := 0
  Else
    Result := VarToDateTime(FData);
End;

Procedure TfsParam.SetAsVariant(Const Value: Variant);
Begin
  If ParamRef = Self Then
    Begin
      FBound := Not VarIsEmpty(Value);
      FNull := VarIsEmpty(Value) Or VarIsNull(Value);
      If FDataType = ftUnknown Then
        Case VarType(Value) Of
          varSmallint, varByte: FDataType := ftSmallint;
          varInteger: FDataType := ftInteger;
          varCurrency: FDataType := ftBCD;
          varSingle, varDouble: FDataType := ftFloat;
          varDate: FDataType := ftDateTime;
          varBoolean: FDataType := ftBoolean;
          varString, varOleStr: FDataType := ftString;
          Else
            FDataType := ftUnknown;
        End;
      FData := Value;
    End
  Else
    ParamRef.SetAsVariant(Value);
End;

Function TfsParam.GetAsVariant: Variant;
Begin
  Result := ParamRef.FData;
End;

Procedure TfsParam.SetAsMemo(Const Value: String);
Begin
  FDataType := ftMemo;
  Self.Value := Value;
End;

Function TfsParam.GetAsMemo: String;
Begin
  If IsNull Then
    Result := ''
  Else
    Result := FData;
End;

Procedure TfsParam.SetAsBlob(Const Value: TBlobData);
Begin
  FDataType := ftBlob;
  Self.Value := Value;
End;

Procedure TfsParam.LoadFromFile(Const FileName: String; BlobType: TBlobType);
Var
  Stream: TStream;
Begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  Try
    LoadFromStream(Stream, BlobType);
  Finally
    Stream.Free;
  End;
End;

Procedure TfsParam.LoadFromStream(Stream: TStream; BlobType: TBlobType);
Var
  DataStr: String;
  Len: Integer;
Begin
  With Stream Do
    Begin
      FDataType := BlobType;
      Position := 0;
      Len := Size;
      SetLength(DataStr, Len);
      ReadBuffer(Pointer(DataStr)^, Len);
      Self.Value := DataStr;
    End;
End;

End.

