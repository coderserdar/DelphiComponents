{
    Firesoft - ExportSuite
    Copyright (C) 1997-2006 Federico Firenze

    This library is free software; you can redistribute it and/or modify it
    under the terms of the GNU Library General Public License as published
    by the Free Software Foundation; either version 2 of the License,
    or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    Federico Firenze,
    Buenos Aires, Argentina
    webmaster@delphi.com.ar

}

unit DataToDbf;

{$I DELPHI.VER}
{$DEFINE FC} // Full Compatible - Compatible con las DBF's mas antiguas

interface

uses
  Db, Classes, DataExport;

type
  {$IFDEF LESS110}
  LongWord = Cardinal;
  {$ENDIF}

  TDBFDate = record
    Year: Byte;
    Month: Byte;
    Day: Byte;
  end;
  PDBFDate = ^TDBFDate;

  TDbfFieldType = Char;
  TFieldName = array[1..11] of Char;

  PDBFHeader = ^TDBFHeader;
  TDBFHeader = record
    Version: Byte;                  { Should be 3 or $83                       }
                                    { $3  - FoxBase+/dBase III Plus            }
                                    {     - FoxPro/dBase IV                    }
                                    { $83 - FoxBase+/dBase III Plus            }
                                    { $F5 - FoxPro                             }
                                    { $8B - dBase IV                           }
    Date: TDBFDate;                 { Date of last update                      }
    RecordCount: Longword;          { Number of records in the file            }
    HeaderSize: Word;               { Length of the header                     }
    RecordSize: Word;               { Length of individual records             }
    Reserved1: array[1..2] of Byte; { not used                                 }
    Transaction: Byte;              { begin-end transaction                    }
                                    { 00 - no transaction protected            }
                                    { 01 - transaction protected               }
    Encrypt: Byte;                  { coded fields                             }
                                    { 00 - uncrypted                           }
                                    { 01 - encrypted                           }
    MultiUse: Integer;              { 16-19                                    }
    LastUserID: Integer;            { 20-23                                    }
    Dummy2: array[24..27] of Byte;  {                                          }
    MDXFlag: Byte;                  { 28                                       }
    Language: Byte;                 { language driver /fox/                    }
                                    { 001 - code page 437                      }
                                    { 002 - code page 850                      }
                                    { 100 - code page 852                      }
                                    { 102 - code page 865                      }
                                    { 101 - code page 866                      }
                                    { 104 - code page 895                      }
                                    { 200 - code page 1250                     }
                                    { 201 - code page 1251                     }
                                    { 003 - code page 1252                     }
    Labeled: Word;                  { 30-31                                    }
  end;

  PDbfFieldHeader = ^TDbfFieldHeader;
  TDbfFieldHeader = record
    FieldName: TFieldName;         { Name of the field                     11 }
    FieldType: TDbfFieldType;      { Type of data in this field             1 }
    Address: LongWord;             { only applicable to foxpro databases    4 }
    FieldLength: Byte;             { Length of the field                    1 }
    Decimals: Byte;                { Number of decimal places               1 }
    Dummy2: array[18..31] of Byte; { Reserved: Word;
                                     WorkArea: Byte;
                                     Reserved2: Word;
                                     SetFields: Byte;
                                     Reserved3: Array[1..7] Of Byte;
                                     IndexFlag: Byte;
                                   }
  end;

  TDataType = (dtChar, dtDate, dtNumber, dtLogic
               {$IFNDEF FC}, dtTime, dtFloat, dtMemo{$ENDIF});
  TFieldSize = 0..High(Byte);

  TDBFField = class(TExportField)
  private
    FRequired: Boolean;
    FSize: TFieldSize;
    FPrecision: Byte;
    FDataType: TDataType;
    procedure SetDataType(const Value: TDataType);
    procedure SetPrecision(const Value: Byte);
    procedure SetSize(const Value: TFieldSize);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function DataSize: LongWord;
    //function GetData: string;
    procedure GetData(var ABuffer: array of char; ABuffSize: Cardinal);
  published
    property DataType: TDataType read FDataType write SetDataType default dtChar;
    property Size: TFieldSize read FSize write SetSize default 1;
    property Precision: Byte read FPrecision write SetPrecision default 0;
    property Required: Boolean read FRequired  write FRequired default False;
  end;

  TDBFFields = class(TExportFields)
  private
    function GetItem(Index: Integer): TDBFField;
    procedure SetItem(Index: Integer; const Value: TDBFField);
  protected
  public
    function Add: TDBFField;
    function RecordSize: Word;
    property Items[Index: Integer]: TDBFField read GetItem write SetItem; default;
  end;

  TDataToDbf = class(TDataExport)
  private
    FFields: TDBFFields;
  protected
    procedure OpenFile; override;
    procedure CloseFile; override;
    procedure WriteRecord; override;
    procedure WriteHeader;
  public
    function GetFields: TExportFields; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property SaveIfEmpty;
    property DataSet;
    property OnBeginExport;
    property OnEndExport;
    property BeforeWriteRecord;
    property AfterWriteRecord;
    property Fields: TDBFFields read FFields write FFields;
  end;

implementation

uses
  SysUtils, Windows, Consts, DBGrids, Math
  {$IFNDEF LESS110}, SysConst{$ENDIF}
  {$IFNDEF LESS140}, RTLConsts{$ENDIF};

const
  { Data Type }
  DDT_CHAR = #$43;    {'C' - characters, all ascii}
  DDT_DATE = #$44;    {'D' - date ddmmyyyy, fix size 8}
  DDT_NUMBER = #$4E;  {'N' - numeric, -,.,0..9}
  DDT_LOGIC = #$4C;   {'L' - logical, ?,Y,y,N,n}
  {$IFNDEF FC}
  DDT_TIME = #$54;    {'T' - time hhmmss, fix size 6}
  DDT_FLOAT = #$46;   {'F' - float point, -,.,0..9}
  DDT_MEMO = #$4D;    {'M' - memo, as numeric, fix size 10}
  {$ENDIF}

  { Begin-End Transaction }
  DTR_NOPROTECTED = $00; { No transaction Protected }
  DTR_PROTECTED = $01;   { Transaction Protected }

  { Coded Fields }
  DEN_UNCRYPTED = $00; { Uncrypted }
  DEN_ENCRYPTED = $01; { Encrypted }

  DBF_DECIMAL_SEPARATOR = '.';

{$IFDEF LESS100}
const
{$ELSE}
resourcestring
{$ENDIF}
  SDataTypeUnImplemented = 'Tipo de dato no implementado';

{$IFDEF LESS110}
function Max(const A, B: LongWord): LongWord;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Min(const A, B: LongWord): LongWord;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;
{$ENDIF}

function DBFDate(ADateTime: TDateTime): TDBFDate;
var
 wYear,
 wMonth,
 wDay: Word;
begin
  DecodeDate(ADateTime, wYear, wMonth, wDay);

  with Result do
  begin
    Year := wYear mod 255;
    Month := wMonth;
    Day := wDay;
  end;
end;

function DbfFieldType(ADataType: TDataType): TDbfFieldType;
begin
  case ADataType of
    dtChar:    Result := DDT_CHAR;
    dtDate:    Result := DDT_DATE;
    dtNumber:  Result := DDT_NUMBER;
    dtLogic:   Result := DDT_LOGIC;
    {$IFNDEF FC}
    dtTime:    Result := DDT_TIME;
    dtFloat:   Result := DDT_FLOAT;
    dtMemo:    Result := DDT_MEMO;
    {$ENDIF}
  else
    {$IFDEF LESS100}
    raise Exception.CreateRes(SInvalidPropertyValue);
    {$ELSE}
    raise Exception.Create(SInvalidPropertyValue);
    {$ENDIF}
  end;
end;

function FieldTypeToDataType(AValue: TFieldType): TDataType;
begin
  case AValue of
    ftString, ftBytes, ftVarBytes{$IFNDEF LESS110}, ftFixedChar, ftWideString{$ENDIF}:
      Result := dtChar;
    ftDate, ftDateTime:
      Result := dtDate;
    ftSmallint, ftInteger, ftWord, ftAutoInc{$IFNDEF LESS110}, ftLargeint{$ENDIF}
    {$IFDEF FC},ftFloat, ftCurrency, ftBCD{$ENDIF}:
      Result := dtNumber;
    ftBoolean:
      Result := dtLogic;
    {$IFDEF FC}
    ftTime, {$IFNDEF VER100}{$IFNDEF LESS130}ftOraBlob, ftOraClob,{$ENDIF}{$ENDIF} ftBlob, ftMemo, ftGraphic, ftFmtMemo:
      Result := dtChar;
    {$ELSE}
    ftTime:
      Result := dtTime;
    ftFloat, ftCurrency:
      Result := dtFloat;
    ftOraBlob, ftOraClob, ftBlob, ftMemo, ftGraphic, ftFmtMemo:
      Result := dtMemo;
    {$ENDIF}
  else
    {$IFDEF LESS100}
    raise Exception.CreateRes(SInvalidPropertyValue);
    {$ELSE}
    raise Exception.Create(SInvalidPropertyValue);
    {$ENDIF}
  end;
end;

procedure GetFieldIntegerValue(AValue: Integer; const AWidth: Integer;
  const ADest: PChar); {overload;}
var
  nSign,
  nPos: Byte;
begin
  FillMemory(ADest, AWidth, Ord('0'));   { LLena la memoria con ceros          }
  if AValue < 0 then begin               { Número negativo                     }
    ADest[0] := '-';                     { Pone el signo en el primer caracter }
    nSign := 1;
  end else
    nSign := 0;

  AValue := Abs(AValue);
  { Si el largo del valor no entra en el ancho pasado, produce un error }
  if AValue > StrToInt(StringOfChar('9', AWidth-nSign)) then
    {$IFDEF LESS100}
    raise Exception.CreateRes(65426);
    {$ELSE}
    raise Exception.Create(SIntOverflow);
    {$ENDIF}

  nPos := 1;
  repeat
    ADest[AWidth - nPos] := Chr((AValue mod 10) + Ord('0'));
    AValue := AValue div 10;
    Inc(nPos);
  until nPos > (AWidth - nSign);
end;

procedure GetFieldDateValue(AValue: TDateTime; const ADest: PChar;
  AYearLength: Byte); {overload;}
var
  wYear, wMonth, wDay: Word;
begin
  DecodeDate(AValue, wYear, wMonth, wDay);

  { Formato yyyymmdd }
  GetFieldIntegerValue(wYear, AYearLength, PChar(ADest));
  GetFieldIntegerValue(wMonth, 2, PChar(ADest) + AYearLength);
  GetFieldIntegerValue(wDay, 2, PChar(ADest) + AYearLength + 2);
end;

{$IFNDEF FC}
procedure GetFieldTimeValue(AValue: TDateTime; const ADest: PChar); {overload;}
var
  wHour, wMin, wSec, wMSec: Word;
begin
  DecodeTime(AValue, wHour, wMin, wSec, wMSec);

  { Formato hhmmss }
  GetFieldValue(wHour, 2, PChar(ADest));
  GetFieldValue(wMin, 2, PChar(ADest) + 2);
  GetFieldValue(wSec, 2, PChar(ADest) + 4);
end;
{$ENDIF}

procedure GetFieldFloatValue(AValue: Extended; const AWidth, APrecision: Integer;
  const ADest: PChar); {overload;}
var
  PFindChar: PChar;
begin
  FloatToText(ADest, AValue, fvExtended, ffFixed, AWidth, APrecision);
  if DecimalSeparator <> DBF_DECIMAL_SEPARATOR then
  begin
    PFindChar := StrScan(ADest, DecimalSeparator);
    if PFindChar <> nil then
      PFindChar^ := DBF_DECIMAL_SEPARATOR;
  end;
end;

{ TDBFField }

procedure TDBFField.SetDataType(const Value: TDataType);
begin
  if FDataType <> Value then
  begin
    FDataType := Value;
    case FDataType of
      dtChar:
        FPrecision := 0;
      dtDate:
      begin
        FSize := 8;
        FPrecision := 0;
      end;
      dtLogic:
      begin
        FSize := 1;
        FPrecision := 0;
      end;
      {$IFNDEF FC}
      dtTime:
      begin
        FSize := 6;
        FPrecision := 0;
      end;
      dtMemo:
        FPrecision := 0;
      {$ENDIF}
    end;
  end;
end;

procedure TDBFField.SetPrecision(const Value: Byte);
begin
  if (FPrecision <> Value) and (Value <= FSize) and
      (not (FDataType in [dtChar, dtDate, dtLogic
            {$IFNDEF FC}, dtTime, dtMemo{$ENDIF}])) then
    FPrecision := Value;
end;

procedure TDBFField.SetSize(const Value: TFieldSize);
begin
  if (FSize <> Value) and (not (FDataType in
    [dtDate, dtLogic {$IFNDEF FC}, dtTime{$ENDIF}])) then
    FSize := Value;
end;

procedure TDBFField.Assign(Source: TPersistent);
var
  FField: TField;
begin
  inherited;
  if Source is TDBFField then
    with Source as TDBFField do
    begin
      FDataType := DataType;
      FSize := Size;
      FPrecision := Precision;
      FRequired := Required;
    end
  else
  if Source is TField then
  begin
    FField := TField(Source);

    FDataType  := FieldTypeToDataType(FField.DataType);

    if FField is TMemoField then
      SetSize(High(Byte))
    else if FField.DataSize = 0 then
      SetSize(1)
    else
      SetSize(FField.DataSize);

    FRequired := FField.Required;

    if FField is TFloatField then
      SetPrecision(TFloatField(FField).Precision)
    else
      SetPrecision(0);

  end else
  if Source is TColumn then
    if Assigned(TColumn(Source).Field) then
      Assign(TColumn(Source).Field); { Asigna todas las propiedades propias del Campo }
end;

constructor TDBFField.Create(Collection: TCollection);
begin
  inherited;

  FDataType := dtChar;
  FSize := 1;
  FPrecision := 0;
  FRequired := False;
end;

function TDBFField.DataSize: LongWord;
begin
  case DataType of
    dtNumber,
    dtChar:  Result := Max(Size, 1);
    dtDate:  Result := 8;
    dtLogic: Result := 1;
    {$IFNDEF FC}
    dtFloat: Result := Size;
    dtTime:  Result := 6;
    dtMemo:  Result := 10;
    {$ENDIF}
  else
    raise Exception.Create(SInvalidPropertyValue);
  end;
end;

procedure TDBFField.GetData(var ABuffer: array of char; ABuffSize: Cardinal);
var
  iLen: Integer;
begin
  if ABuffSize < DataSize then
    iLen := ABuffSize
  else
    iLen := DataSize;

  ZeroMemory(@ABuffer[0], ABuffSize);
  case FDataType of
    dtChar:
    begin
      FillChar(ABuffer[0], iLen, 32);
      Move(PChar(Field.AsString)^, ABuffer[0], Min(iLen, Length(Field.AsString)));
    end;
    dtNumber{$IFNDEF FC}, dtFloat{$ENDIF}:
      GetFieldFloatValue(Field.AsFloat, DataSize, FPrecision, @ABuffer[0]);
    dtDate:
      GetFieldDateValue(Field.AsDateTime, @ABuffer[0], 4);
    {$IFNDEF FC}
    dtTime:
      GetFieldTimeValue(Field.AsDateTime, @ABuffer[0]);
    {$ENDIF}
    dtLogic:
    begin
      if Field.AsBoolean then
        ABuffer[0] := 'Y'
      else
        ABuffer[0] := 'N';
    end;
    else
      raise Exception.Create(SDataTypeUnImplemented);
  end;
end;

{ TDBFFields }

function TDBFFields.Add: TDBFField;
begin
  Result := inherited Add as TDBFField;
end;

function TDBFFields.GetItem(Index: Integer): TDBFField;
begin
  Result := inherited GetItem(Index) as TDBFField;
end;

function TDBFFields.RecordSize: Word;
var
  i: Integer;
begin
  Result := 1; { Es un Byte que marca si el registro esta dado de baja }
  for i := 0 to Count - 1 do
    Result := Result + Items[i].DataSize;
end;

procedure TDBFFields.SetItem(Index: Integer; const Value: TDBFField);
begin
  SetItem(Index, Value);
end;

{ TDataToDbf }

procedure TDataToDbf.CloseFile;
begin
  WriteChar(#$1A); { end of File #26 }
  inherited;

 {FreeMem(RecordBuffer, FRecordSize);}
end;

constructor TDataToDbf.Create(AOwner: TComponent);
begin
  inherited;
  FFields := TDBFFields.Create(Self, TDBFField);
end;

destructor TDataToDbf.Destroy;
begin
  FFields.Free ;
  inherited;
end;

function TDataToDbf.GetFields: TExportFields;
begin
  Result := FFields;
end;

procedure TDataToDbf.OpenFile;
begin
  inherited;
  WriteHeader;
  WriteChar(#$0D); { end of header }

  {GetMem(RecordBuffer, FRecordSize);}
end;

procedure TDataToDbf.WriteHeader;
var
  Header: PDBFHeader;
  FieldHdr: PDbfFieldHeader;
  iField: Integer;
begin
  GetMem(Header, SizeOf(TDBFHeader));
  try
    ZeroMemory(Header, SizeOf(TDBFHeader));
    with Header^ do
    begin
      Version := $03; //$8B;
      Date := DBFDate(Now);
      RecordCount := DataSet.RecordCount;
      HeaderSize := SizeOf(TDBFHeader) + (SizeOf(TDbfFieldHeader) * FFields.Count) + 1; { Largo total del Header }
      RecordSize := FFields.RecordSize;
      Transaction := $00;
      Encrypt := $00;
    end;
    Write(Header^, SizeOf(TDBFHeader));
  finally
    FreeMem(Header);
  end;

  { - - [ Guarda los Campos ] - - - - - - - - - - - - - - - - - - - - - - - - }
  GetMem(FieldHdr, SizeOf(TDbfFieldHeader));
  try
    for iField := 0 to FFields.Count -1 do
    begin
      ZeroMemory(FieldHdr, SizeOf(TDbfFieldHeader));
      with FieldHdr^ do
      begin
        StrPLCopy(@FieldName[1], FFields[iField].DataField, SizeOf(FieldName));
        FieldType := DbfFieldType(FFields[iField].DataType);
        FieldLength := FFields[iField].Size;
        Decimals := FFields[iField].Precision;
      end;
      Write(FieldHdr^, SizeOf(TDbfFieldHeader));
    end;
  finally
    FreeMem(FieldHdr);
  end;
end;

procedure TDataToDbf.WriteRecord;
var
  iField: Integer;
  ABuffer: array[0..High(Byte)] of Char;
begin
  WriteChar(#32);
  for iField := 0 to FFields.Count -1 do
    with FFields[iField] do
    begin
      GetData(ABuffer, High(Byte));
      {if} Write(ABuffer, DataSize); {<> DataSize then
        raise Exception.CreateFmt(SIniFileWriteError, [ClassName]);}
    end;
end;

end.
