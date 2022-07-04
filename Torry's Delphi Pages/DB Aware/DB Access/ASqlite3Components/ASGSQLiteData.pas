{$I asqlite_def.inc}

unit ASGSQLiteData;

interface

uses
 ActiveX,
 Variants,
 DB,
 Windows,
 sysutils,
 FMTBcd;

procedure BufferToVar(FieldType: TFieldType; Buffer: Pointer; var Data: OleVariant);
procedure VarToBuffer(Field: TField; Data: OleVariant; Buffer: Pointer);
procedure UnpackBuffer(Buffer: PAnsiChar; FieldType: TFieldType; SQLiteDateFormat: boolean; var Data: OleVariant);
//function  FieldNoToIndex(ADataset: TDataSet; AFieldNo: integer): integer;

function GetCalcFieldsCount(ADataset: TDataset): integer;

implementation

uses
{$IFDEF ASQLite_D2006PLUS}
   WideStrUtils,
{$ENDIF}
  ASGRout3,
  ASGSQLite3;

function GetCalcFieldsCount(ADataset: TDataset): integer;
var i: integer;
begin
  result:= 0;
  for I := 0 to ADataset.FieldCount - 1 do
    if ADataset.Fields[i].FieldKind <> fkData then
      inc(result);
    

end;
//function  FieldNoToIndex(ADataset: TDataSet; AFieldNo: integer): integer;
//var Field: TField;
//begin
//  Assert(Assigned(ADataset));
//  Assert(AFieldNo > 0);
//
//
//  // if fieldcount is 0 then no fields defined yet so return field no -1
//  if ADataset.FieldCount = 0 then
//    result:= AFieldNo - 1
//  else
//  begin
//  // otherwise find the matching field and return it's index
//
//  Field:= ADataset.Fields.FieldByNumber(AFieldNo);
//  if assigned(Field) then
//    result:= Field.Index
//  else
//    result:= -1;  // if no matching field found, return -1
//  end;
//end;

procedure BufferToVar(FieldType: TFieldType; Buffer: Pointer; var Data: OleVariant);
begin
  case FieldType of
    ftString, ftFixedChar, ftGuid:
      Data := string(PChar(Buffer));
    ftWideString {$IFDEF ASQLite_D2006PLUS} ,ftFixedWideChar{$ENDIF}:
      Data := WideString(PWideChar(Buffer));
    ftAutoInc, ftInteger:
      Data := LongInt(Buffer^);
    ftSmallInt:
      Data := SmallInt(Buffer^);
    ftWord:
      Data := Word(Buffer^);
    ftBoolean:
      Data := WordBool(Buffer^);
    ftFloat, ftCurrency:
      Data := Double(Buffer^);
    ftBlob, ftMemo, ftGraphic, ftVariant {$IFDEF ASQLite_D2006PLUS} ,ftWideMemo{$ENDIF}:
      Data := Variant(Buffer^);
    ftInterface:
      Data := IUnknown(Buffer^);
    ftIDispatch:
      Data := IDispatch(Buffer^);
    ftDate, ftTime, ftDateTime:
        Data := TDateTime(Buffer^);
    ftBCD:
        Data := Currency(Buffer^);
    ftTimestamp:
        Data := Variant(Buffer^);
    ftBytes, ftVarBytes:
//        if NativeFormat then
//          DataConvert(Field, Buffer, @Data, False) else
        Data := OleVariant(Buffer^);
    ftLargeInt:
      Data := LargeInt(Buffer^);
    else
      DatabaseErrorFmt('Unsupported field type "%s"', [FieldTypeNames[FieldType], '']);
  end;
end;


function VarDataSize(const Value: OleVariant): Integer;
begin
  if VarIsNull(Value) then
    Result := -1
  else if VarIsArray(Value) then
    Result := VarArrayHighBound(Value, 1) + 1
  else if TVarData(Value).VType = varOleStr then
  begin
    Result := Length(PWideString(@TVarData(Value).VOleStr)^);
    if Result = 0 then
      Result := -1;
  end
  else
    Result := SizeOf(OleVariant);
end;

function WStrLCopy(Dest, Source: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Count: Cardinal;
begin
  // copies a specified maximum number of characters from Source to Dest
  Result := Dest;
  Count := 0;
  While (Count < MaxLen) and (Source^ <> #0) do begin
    Dest^ := Source^;
    Inc(Source);
    Inc(Dest);
    Inc(Count);
  end;
  Dest^ := #0;
end;

function WStrCopy(Dest, Source: PWideChar): PWideChar;
begin
  Result := WStrLCopy(Dest, Source, MaxInt);
end;

procedure BCDConvert(Source, Dest: Pointer);
begin
  TBcd(Dest^) := TBcd(Source^);
end;

procedure VarToBuffer(Field: TField; Data: OleVariant; Buffer: Pointer);
  procedure CurrToBuffer(const C: Currency);
  begin
    BCDConvert(@C, Buffer);
  end;
begin
  with tagVariant(Data) do
    case Field.DataType of
      ftGuid, ftFixedChar, ftString:
        begin
          PChar(Buffer)[Field.Size] := #0;
          WideCharToMultiByte(0, 0, bStrVal, SysStringLen(bStrVal)+1,
            Buffer, Field.Size, nil, nil);
        end;
     {$IFDEF ASQLite_D2006PLUS} ftFixedWideChar, {$ENDIF}
      ftWideString:
        WStrCopy(Buffer, bstrVal);
      ftSmallint:
        if vt = VT_UI1 then
          SmallInt(Buffer^) := Byte(cVal) else
          SmallInt(Buffer^) := iVal;
      ftWord:
        if vt = VT_UI1 then
          Word(Buffer^) := bVal else
          Word(Buffer^) := uiVal;
      ftAutoInc, ftInteger:
        Integer(Buffer^) := Data;
      ftFloat, ftCurrency:
        if vt = VT_R8 then
          Double(Buffer^) := dblVal else
          Double(Buffer^) := Data;
      ftBCD:
        if vt = VT_CY then
          CurrToBuffer(cyVal) else
          CurrToBuffer(Data);
      ftBoolean:
        WordBool(Buffer^) := vbool;
      ftDate, ftTime, ftDateTime:
//          DataConvert(Field, @date, Buffer, True);
          TOleDate(Buffer^) := date;
      ftBytes, ftVarBytes:
//          DataConvert(Field, @Data, Buffer, True);
            OleVariant(Buffer^) := Data;
      ftInterface: IUnknown(Buffer^) := Data;
      ftIDispatch: IDispatch(Buffer^) := Data;
      ftLargeInt:
        if Decimal(Data).sign > 0 then
          LargeInt(Buffer^):=-1*Decimal(Data).Lo64
        else
          LargeInt(Buffer^):=Decimal(Data).Lo64;
      ftBlob..ftTypedBinary, ftVariant
     {$IFDEF ASQLite_D2006PLUS}, ftWideMemo {$ENDIF} :
         OleVariant(Buffer^) := Data;
    else
      DatabaseErrorFmt('Unsupported field type in "%s"', [FieldTypeNames[Field.DataType],
        Field.DisplayName]);
    end;
end;

procedure UnpackBuffer(Buffer: PAnsiChar; FieldType: TFieldType; SQLiteDateFormat: boolean; var Data: OleVariant);
var
  TempInt           : integer;
  TempDouble        : double;
  TempBool          : wordbool;
  TempT             : TDateTime;
  SaveDateFormat    : string;
begin
{$IFDEF DEBUG_VERY_LOUD}
  DebugEnter('TASQLite3BaseQuery.UnpackBuffer: ' + Buffer);
{$ENDIF}
  case FieldType of
    ftString:
      begin
{$IFDEF DEBUG_VERY_LOUD}
        DebugLeave('TASQLite3BaseQuery.UnpackBuffer');
{$ENDIF}
        exit;
      end;
    ftInteger, ftSmallInt:
      begin
        TempInt := StrToIntX(string(Buffer));
        Data:= TempInt;
      end;
    ftTime:
      begin
        if SQLiteDateFormat then begin
           shorttimeformat := 'hh":"nn":"ss"."zzz';
           TempT := StrToTimeX(Buffer)                 // aducom 2006
        end else begin
           shorttimeformat := 'hh":"nn":"ss"';
           TempT := StrToTimeX(string(Buffer));                 // aducom 2006
        end;
        Data:= TempT;
      end;
    ftDate:
      begin
        if SQLiteDateFormat then begin
           savedateformat := shortdateformat;
           shortdateformat := 'yyyy-mm-dd';
           TempT := (StrToDateX(Buffer));                // aducom 2006
           shortdateformat := savedateformat;
        end else
           TempT := (StrToDateX(Buffer));                // aducom 2006
        Data:= TempT;
      end;
    ftDateTime:
      begin
        if SQLiteDateFormat then       // aducom
          TempT := YYYYMMDDParser(Buffer) // jpierce
        else
          TempT := StrToDateTimeX(Buffer);
        Data:= TempT;
      end;
    ftFloat, ftBCD, ftCurrency:
      begin
        TempDouble := StrToFloatX(FloatParser(Buffer));
        Data:= TempDouble;
      end;

{$IFDEF ASQLITE_D6PLUS}
    ftBoolean:
      begin
        TempBool := StrToBool(Buffer);
        Data:= TempBool;
      end;
{$ENDIF}
    ftMemo, ftGraphic, ftBlob, ftFMTMemo: // pointer to stream
      begin
        TempInt := StrToInt(Buffer);
        Data:= TempInt;
      end;
  end;
{$IFDEF DEBUG_VERY_LOUD}
  DebugLeave('TASQLite3BaseQuery.UnpackBuffer');
{$ENDIF}
end;

end.
