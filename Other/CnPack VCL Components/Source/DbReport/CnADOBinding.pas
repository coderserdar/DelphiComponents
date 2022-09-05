{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnADOBinding;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ���ѯ�������ؼ��� VC++ �����ݰ󶨵�Ԫ
* ��Ԫ���ߣ�penal
*           ������ (appleak46@yahoo.com.cn)
* ��    ע��Delphi ����汾
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.11.24 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses 
  Windows, Classes, SysUtils, ActiveX
{$IFDEF COMPILER6_UP}
  , Variants
{$ENDIF}
  ;

//VC++
//˵��������ͻ���ȡһ�����ݲ�ֱ�����������ݳ�Ա�Ľӿڡ��ͻ�������Ҫ�������а�������Ŀ����ָ�� Recordset Field ����������ݳ�Ա֮��Ĺ�����
const
// enum ADOFieldStatusEnum  ö��ADO Status ����
  adFldOK = 0;                  // һ����NULL��ֵ����
  adFldBadAccessor = 1;         // ����Ч.
  adFldCantConvertValue = 2;    // ָʾ�ֶο����ڲ���ʧ�κ����ݵ�����±���ȡ�ʹ���
  adFldNull = 3;                // ������һ��NULLֵ
  adFldTruncated = 4;           // ָʾ������Դ��ȡ����ʱ���ԵĿɱ䳤ֵ����
  adFldSignMismatch = 5;        //ָʾͨ�������ṩ����[provider]���ص���ֵ���������ţ�����ADO�ֶ�ֵ����ֵ���Ͳ�����������
  adFldDataOverFlow = 6;        // ָʾ�Ӽ����ṩ����[provider]���ص���ֵ����ֶε���ֵ����
  adFldCantCreate = 7;      //���ڼ����ṩ����[provider]��������Ҫ�����Բ���������ֶ�
  adFldUnavailable = 8;     //��������Դ��ȡ����ʱ�������ṩ����[provider]���ܾ�����ֵ
  adFldPermissionDenied = 9; //ָʾ�ֶβ��ܱ����ģ���Ϊ������Ϊֻ��
  adFldIntegrityViolation = 10; //ָʾ�ֶβ��ܱ����ģ���Ϊ��ʳһ���ѱ���������ѱ�������ʵ��
  adFldSchemaViolation = 11; //ָʾֵΥ��������Դ�ƻ�[data source schema]���ֶε�����
  adFldBadStatus = 12;  //ָʾһ����ADO���͵�OLEDB�ṩ�ߵ���Ч״ֵ̬
  adFldDefault = 13;  //����Ĭ��ʹ�õ��ֶ�ֵ ................
   //..........................
type
  PADO_BINDING_ENTRY = ^ADO_BINDING_ENTRY;
  ADO_BINDING_ENTRY = record
    ulOrdinal: UINT;
    wDataType: Word;
    bPrecision: Byte;
    bScale: Byte;
    ulSize: UINT;
    ulBufferOffset: UINT;
    ulStatusOffset: UINT;
    ulLengthOffset: UINT;
    ulADORecordBindingOffset: UINT;
    fModify: BOOL;
  end;
  TADOBindingEntry = ADO_BINDING_ENTRY;
  PADOBindingEntry = ^TADOBindingEntry;
  //IADORecordBinding �ӿھ���ʹ Recordset �ֶ��� C/C++ ����������������к�ִ�и��µķ�����������������������ʹָ��ָ������ CADORecordBinding ���࣬�� CADORecordBinding ����ÿ���ֶκͱ���֮��İ󶨡�
  IADORecordBinding = interface
    ['{00000544-0000-0010-8000-00AA006D2EA4}']
    function BindToRecordset(BindInfo: Pointer): HResult; stdcall; //���ø÷����ɹ����������ֶ�
    function AddNew(BindInfo: Pointer): HResult; stdcall; //���ø÷�����ֱ�ӵ��� ADO AddNew ����
    function Update(BindInfo: Pointer): HResult; stdcall;//���ø÷�����ֱ�ӵ��� ADO Update ������
  end;

  EADOBindingException = class(Exception);

  PColumnRawData = ^TColumnRawData;
  TColumnRawData = record
    DataLength: LongWord;
    Status: LongWord;
    RawData: array[0..0] of Byte;
  end;

  TADOBinding = class
  private
    FEntries: array of TADOBindingEntry;
    FBindingData: Pointer;
    FDataSize: Integer;
    FPtrList: array of Pointer;
    function CalcDataSize(data_type: Word; data_size: Integer): LongWord;//�����ֶ����ͳ���
    procedure CheckIndex(Index: Integer);
    procedure CheckStatus(Index: Integer);
    procedure Error(status: UINT); overload;
    procedure Error(const msg: string); overload;
    function GetAsInteger(Index: Integer): Integer;
    function GetAsString(Index: Integer): string;
    function GetLen(Index: Integer): Integer;
    function GetStatus(Index: Integer): Integer;
    function GetAsDouble(Index: Integer): Double;
    function GetIsNull(Index: Integer): Boolean;
    function GetAsRawData(Index: Integer): PColumnRawData;
    function GetAsSingle(Index: Integer): Single;
  public
    constructor Create(bindings: array of TADOBindingEntry); overload;
    constructor Create; overload;
    destructor Destroy; override;

    // �󶨶�������
    procedure AddBinding(ordinal: UINT; data_type: Word;
                        modify: Boolean); overload;
    // �󶨱䳤����adVarChar��
    procedure AddBinding(ordinal: UINT; data_type: Word;
                        data_size: UINT; modify: Boolean); overload;
    // ����ֵ����adNumeric��
    procedure AddBinding(ordinal: UINT; data_type: Word;
                        precision, scale: Byte; modify: Boolean); overload;

    function GetADOBindingData: Pointer;
    procedure ClearBuffer;
    property AsString[Index: Integer]: string read GetAsString;
    property AsInteger[Index: Integer]: Integer read GetAsInteger;
    property AsDouble[Index: Integer]: Double read GetAsDouble;
    property AsSingle[Index: Integer]: Single read GetAsSingle;
    property AsRawData[Index: Integer]: PColumnRawData read GetAsRawData;
    property Status[Index: Integer]: Integer read GetStatus;
    property Len[Index: Integer]: Integer read GetLen;
    property IsNull[Index: Integer]: Boolean read GetIsNull;
  end;

  TDefaultBindingInfo = class
  private
    FEntries: PADOBindingEntry;
  public
    function GetADOBindingEntries: PADOBindingEntry; virtual; stdcall;
    constructor Create(bindings: PADOBindingEntry);
  end;

implementation

uses
  ADOInt, OleDB;

function TDefaultBindingInfo.GetADOBindingEntries: PADOBindingEntry;
begin
  Result := FEntries;
end;

constructor TDefaultBindingInfo.Create(bindings: PADOBindingEntry);
begin
  FEntries := bindings;
end;

{ TADOBinding }

procedure TADOBinding.AddBinding(ordinal: UINT; data_type: Word;
  modify: Boolean);
var
  I: Integer;
begin
  SetLength(FEntries, Length(FEntries) + 1);
  I := Length(FEntries) - 2;
  FEntries[I].ulOrdinal := ordinal;
  FEntries[I].wDataType := data_type;
  FEntries[I].fModify := modify;
end;

procedure TADOBinding.AddBinding(ordinal: UINT; data_type: Word;
  data_size: UINT; modify: Boolean);
var
  I: Integer;
begin
  SetLength(FEntries, Length(FEntries) + 1);
  I := Length(FEntries) - 2;
  FEntries[I].ulOrdinal := ordinal;
  FEntries[I].wDataType := data_type;
  FEntries[I].ulSize := data_size;
  FEntries[I].fModify := modify;
end;

procedure TADOBinding.AddBinding(ordinal: UINT; data_type: Word; precision,
  scale: Byte; modify: Boolean);
var
  I: Integer;
begin
  SetLength(FEntries, Length(FEntries) + 1);
  I := Length(FEntries) - 2;
  FEntries[I].ulOrdinal := ordinal;
  FEntries[I].wDataType := data_type;
  FEntries[I].bPrecision := precision;
  FEntries[I].bScale := scale;
  FEntries[I].fModify := modify;
end;

function TADOBinding.CalcDataSize(data_type: Word; data_size: Integer): LongWord;
begin
  Result := 0;
  case data_type of
    adTinyInt: Result := 1;
    adSmallInt: Result := 2;
    adInteger: Result := 4;
    adBigInt: Result := 8;
    adUnsignedTinyInt: Result := 1;
    adUnsignedSmallInt: Result := 2;
    adUnsignedInt: Result := 4;
    adUnsignedBigInt: Result := 8;
    adSingle: Result := 4;
    adDouble: Result := 8;
    adCurrency: Result := 8;
    adDecimal: Result := SizeOf(DECIMAL);
    adNumeric: Result := SizeOf(DB_NUMERIC);
    adBoolean: Result := SizeOf(BOOL);
    adError: Result := SizeOf(Integer);
    adVariant: Result := SizeOf(Variant);
    adIDispatch,
    adIUnknown: Result := SizeOf(Pointer);
    adGUID: Result := SizeOf(TGuid);
    adDate: Result := SizeOf(TDateTime);
    adDBDate: Result := SizeOf(DBDATE);
    adDBTime: Result := SizeOf(DBTIME);
    adDBTimeStamp: Result := SizeOf(DBTIMESTAMP);
    adBSTR: Result := SizeOf(Pointer);
    adChar,
    adVarChar,
    adLongVarChar: Result := data_size + 1;
    adWChar,
    adVarWChar,
    adLongVarWChar: Result := data_size + 2;

    adBinary,
    adVarBinary,
    adLongVarBinary: Result := data_size;

    adFileTime: Result := sizeof(FILETIME);
  else
    Error('Unsupported data type.');
// unsupported data type:
//  adChapter = $00000088;
//  adDBFileTime = $00000089;
//  adPropVariant = $0000008A;
//  adVarNumeric = $0000008B;
//  adUserDefined = $00000084;

  end;
end;

procedure TADOBinding.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Length(FPtrList)) then
    Error('CheckIndex: Index out of bound.');
end;

procedure TADOBinding.CheckStatus(Index: Integer);
var
  stat: UINT;
begin
  stat := Self.GetStatus(Index);
  if not (stat in [adFldOK, adFldNull, adFldTruncated, adFldDefault]) then
    Error(stat);
end;

constructor TADOBinding.Create(bindings: array of TADOBindingEntry);
var
  I: Integer;
begin
  SetLength(FEntries, Length(bindings) + 1);  //��ʼ��
  for I := 0 to Length(bindings)-1 do
    FEntries[I] := bindings[I];
end;

procedure TADOBinding.ClearBuffer;
begin
  FillChar(FBindingData^, FDataSize, 0);
end;

constructor TADOBinding.Create;
begin
  SetLength(FEntries, 1);
end;

destructor TADOBinding.Destroy;
begin
  if FBindingData <> nil then
    FreeMem(FBindingData);
  FBindingData := nil;
  inherited;
end;

procedure TADOBinding.Error(status: UINT);
const
  stat_err_msg: array[1..13] of string = (
        'Binding invalid.',       // adFldBadAccessor = 1
        'Value Can not Be Convert', // adFldCantConvertValue = 2
        'Value Is NULL', // adFldNull = 3; not error
        'Variable-length data or numeric digits were truncated.', // adFldTruncated = 4
        'Value is signed and variable data type is unsigned.', // adFldSignMismatch = 5
        'Value is larger than could be stored in the variable data type.', // adFldDataOverFlow = 6
        'Unknown column type and field already open.', // adFldCantCreate = 7;
        'Field value could not be determined.', // adFldUnavailable = 8;
        'When updating, no permission to write data.', // adFldPermissionDenied = 9;
        'When updating, field value would violate column integrity.', // adFldIntegrityViolation = 10;
        'When updating, field value would violate column schema.', // adFldSchemaViolation = 11;
        'When updating, invalid status parameter.', // adFldBadStatus = 12;
        'When updating, a default value was used.'  // adFldDefault = 13; not error
      );
begin
  if (status > 0) and (status <= adFldDefault) then
    raise EADOBindingException.Create(stat_err_msg[status])
  else
    raise EADOBindingException.Create('');
end;

procedure TADOBinding.Error(const msg: string);
begin
  raise EADOBindingException.Create(msg);
end;

function TADOBinding.GetADOBindingData: Pointer;
type
  // TDefaultBindingInfo ����ṹ.
  PBindClassRec = ^BindClassRec;
  BindClassRec = record
    VMT: Pointer;
    Entries: Pointer;
  end;

var
  I: Integer;
  data_size: LongWord;
begin
  if FBindingData <> nil then
  begin
    Result := FBindingData;
    Exit;
  end;

  SetLength(FPtrList, Length(FEntries)-1);
  data_size := SizeOf(BindClassRec);
  data_size := (data_size + 7) and (not 7);
  for I := 0 to Length(FEntries)-2 do
  begin
    FEntries[I].ulLengthOffset := data_size;
    FEntries[I].ulStatusOffset := data_size + 4;
    FEntries[I].ulBufferOffset := data_size + 8;

    Inc(data_size, 8); // data length, data status
    FPtrList[I] := Pointer(data_size);
    Inc(data_size, CalcDataSize(FEntries[I].wDataType, FEntries[I].ulSize));
    data_size := (data_size + 7) and (not 7);
  end;
  FDataSize := data_size;
  FBindingData := AllocMem(data_size);
  PBindClassRec(FBindingData)^.VMT := Pointer(TDefaultBindingInfo); //PPointer(FClass)^;
  PBindClassRec(FBindingData)^.Entries := @FEntries[0];
  for I := 0 to Length(FEntries)-2 do
  begin
    FPtrList[I] := Pointer(Cardinal(FPtrList[I]) + Cardinal(FBindingData)); 
  end;

  Result := FBindingData;
end;

function TADOBinding.GetAsDouble(Index: Integer): Double;
begin
//  CheckIndex(Index);
  CheckStatus(Index);
  Result := PDouble(FPtrList[Index])^;
end;

function TADOBinding.GetAsInteger(Index: Integer): Integer;
begin
//  CheckIndex(Index);
  CheckStatus(Index);
  Result := PInteger(FPtrList[Index])^;
end;

function TADOBinding.GetAsRawData(Index: Integer): PColumnRawData;
begin
  CheckIndex(Index);
  Result := PColumnRawData(Cardinal(FPtrList[Index]) - 8);
end;

function TADOBinding.GetAsSingle(Index: Integer): Single;
begin
  CheckStatus(Index);
  Result := PSingle(FPtrList[Index])^;
end;

function TADOBinding.GetAsString(Index: Integer): string;
var
  len: Integer;
begin
  CheckStatus(Index);
  len := GetLen(Index);
  SetString(Result, PChar(FPtrList[Index]), len);
end;

function TADOBinding.GetIsNull(Index: Integer): Boolean;
begin
  Result := GetStatus(Index) = adFldNull;
end;

function TADOBinding.GetLen(Index: Integer): Integer;
var
  len: PLongWord;
begin
  CheckIndex(Index);
  len := PLongWord(Cardinal(FPtrList[Index]) - 8);
  Result := len^;
end;

function TADOBinding.GetStatus(Index: Integer): Integer;
var
  stat: PLongWord;
begin
  CheckIndex(Index);
  stat := PLongWord(Cardinal(FPtrList[Index]) - 4);
  Result := stat^;
end;

end.
