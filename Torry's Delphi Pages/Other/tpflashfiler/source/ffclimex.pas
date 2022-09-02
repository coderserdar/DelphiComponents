{*********************************************************}
{* FlashFiler: Import/Export unit                        *}
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

unit ffclimex;

interface

uses
  Windows,
  DB,
  DBConsts,
  Forms,
  SysUtils,
  Classes,
  IniFiles,
  TypInfo,
  ffsrbde,
  ffdbbase,
  ffdb,
  ffstdate,
  ffconst,
  ffclbase,
  fflldate,
  ffllexcp,
  ffconvff,
  ffclintf,
  ffllbase,
  fflldict;

const
  DefDateMask       = 'MM/DD/YYYY';
  DefDblDelims      = False;
  DefDelimitor      = '"';
  DefError          = 'ERROR';
  DefExt            = '.SCH';
  DefMaxLineLength  = 8*1024; { Max line length assumed by ASCII import routines }
  DefSeparator      = ',';
  DefEpoch          : Integer = 1969;                                  {!!.05}
  DefYieldInterval  = 1;

type
  TffieFileType = (ftCSV, ftASCII, ftBINARY, ftBTF, ftVARBTF);

  TffieNativeFieldType = (nftUnknown,
                          nftChar,
                          nftASCIIFloat,
                          nftASCIINumber,
                          nftASCIIBool,
                          nftASCIILongInt,
                          nftASCIIAutoInc,                            
                          nftASCIIDate,
                          nftASCIITime,
                          nftASCIITimestamp,
                          nftInt8,
                          nftInt16,
                          nftInt32,
                          nftUInt8,
                          nftUInt16,
                          nftUInt32,
                          nftAutoInc8,
                          nftAutoInc16,                               
                          nftAutoInc32,                               
                          nftReal,
                          nftSingle,
                          nftDouble,
                          nftExtended,
                          nftComp,
                          nftCurrency,
                          nftBoolean,
                          nftDateTime1,
                          nftDateTime2,
                          nftStDate,
                          nftStTime,
                          nftLString,
                          nftZString,
                          nftUnicode,
                          nftBinary);

  {===== Schema File Classes =====}

  TffieFieldItem = class
    fiTargetFieldNo: SmallInt;
    fiFieldName: TffDictItemName;
    fiNativeTypeDesc: string[20];
    fiNativeType: TffieNativeFieldType;
    fiNativeSize: SmallInt;
    fiNativeDecPl: SmallInt;
    fiNativeOffset: SmallInt;
    fiDateMask: string[25];
  end;

  TffSchemaFieldList = class(TffObject)
  private
    FList : TList;
    function GetCount: Integer;
  protected
    function GetFieldItem(aIndex: Integer): TffieFieldItem;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(aFieldItem : TffieFieldItem);
    property Count : Integer read GetCount;
    property Items[aIndex: Integer]: TffieFieldItem read GetFieldItem;
  end;

  TffSchemaFile = class(TIniFile)
  protected {private}
    FFilename: TFileName;
    FFields: TffSchemaFieldList;
    FMainSection: string;
    FRecLength: LongInt;
    FBTFDelFlag: Boolean;
    function GetDateMask: string;
    function GetDblDelims: Boolean;
    function GetDelimiter: AnsiChar;
    function GetFileType: TffieFileType;
    function GetSeparator: AnsiChar;
    procedure LoadFields;
    procedure SetDateMask(aValue: string);
    procedure SetDblDelims(aValue: Boolean);
    procedure SetDelimiter(aValue: AnsiChar);
    procedure SetFileType(aValue: TffieFileType);
    procedure SetRecLength(aValue: LongInt);
    procedure SetSeparator(aValue: AnsiChar);
  public
    constructor Create(aFileName: string);
    destructor Destroy; override;
    procedure BindDictionary(aDictionary: TffDataDictionary);
    function GetSourceFieldPtr(aBufPtr: Pointer; aFieldNo: Integer): Pointer;
    procedure MakeIntoDictionary(aDictionary: TffDataDictionary);
    property BTFDelFlag: Boolean read FBTFDelFlag;
    property DateMask: string read GetDateMask write SetDateMask;
    property DblDelims: Boolean read GetDblDelims write SetDblDelims;
    property Delimiter: AnsiChar read GetDelimiter write SetDelimiter;
    property Fields: TffSchemaFieldList read FFields;
    property FileType: TffieFileType read GetFileType write SetFileType;
    property RecordLength: LongInt read FRecLength write SetRecLength;
    property Section: string read FMainSection;
    property Separator: AnsiChar read GetSeparator write SetSeparator;
  end;

  {===== Stream Classes for File I/O =====}

  TffFileStream = class(TFileStream)
  protected
  protected
    function GetNumRecords: LongInt; virtual; abstract;
    function GetPercentCompleted: Word; virtual;
    function GetRecordLength: LongInt; virtual; abstract;
  public
    function Read(var Buffer; Count: LongInt): LongInt; override;
    function ReadRec(var Rec): Boolean; virtual; abstract;
    property NumRecords: LongInt read GetNumRecords;
    property PercentCompleted: Word read GetPercentCompleted;
    property RecordLength: LongInt read GetRecordLength;
  end;

  TffFixedFileStream = class(TffFileStream)
  protected {private}
    FRecLength: LongInt;
    FNumRecs: LongInt;
  protected
    function GetNumRecords: LongInt; override;
    function GetRecordLength: LongInt; override;
  public
    constructor Create(const aFileName: string; aMode: Word; aRecLength: LongInt);
    function ReadRec(var Rec): Boolean; override;
  end;

  TffFixedASCIIStream = class(TffFixedFileStream)
  protected {private}
  protected
    CRLF: Boolean;
  public
    function ReadRec(var Rec): Boolean; override;
  end;

  TffFixedBTFStream = class(TffFixedFileStream)
  protected {private}
    FNumSkipped: LongInt;
    DelFieldAvail: Boolean;
  protected
  public
    constructor Create(const aFileName: string; aMode: Word; aDelFlag: Boolean);
    function ReadRec(var Rec): Boolean; override;
    property NumSkipped: LongInt read FNumSkipped;
  end;

  TffVaryingFileStream = class(TffFileStream)
  protected
  public
    function ReadRec(var Rec): Boolean; override;
  end;

  {===== Field Conversion Classes to Parse Records =====}

  TffFieldConverter = class
  protected { private }
    FBuffer: Pointer;
    FBufLen: LongInt;
    FSchema: TffSchemaFile;
    FDict: TffDataDictionary;
  public
    procedure Init(aFieldBuf: Pointer;
                   aBufLen: LongInt;
                   aSchema: TffSchemaFile;
                   aDictionary: TffDataDictionary);
    procedure AdjustMaskAndValue(aMask, aValue: TffShStr;
                             var aDateMask, aDateValue,
                                 aTimeMask, aTimeValue: TffShStr);
    { Translates a FF date/time mask into one suitable for SysTools conversion
      routines (expands token characters out to the correct number of digitis
      for each element) }
    function ConvertField(aSourcePtr: Pointer;
                          aSourceType: TffieNativeFieldType;
                          aSourceSize: Integer;
                          aTargetFFType: TffFieldType;
                          aTargetSize: Integer;
                          aDateMask: TffShStr): TffResult;
  end;

  {===== Engine Classes =====}

  TffieProgressPacket = record
    ppNumRecs: DWORD;
    ppTotalRecs: DWORD;
  end;

  TffieYieldEvent = procedure(aProgressPacket: TffieProgressPacket) of object;

  TffInOutEngine = class
  protected {private}
    FDataFile: TffFullFileName;
    FLogFile: TextFile;
    FLogFilename: TFileName;
    FLogCount: LongInt;
    FSchema: TffSchemaFile;
    FStream: TffFileStream;
    FTerminated: Boolean;
    FYieldInterval: Word;
    FImportFilename: TFileName;
    FOnYield: TffieYieldEvent;
  protected
  public
    constructor Create(const aFileName: TffFullFileName;
                             aMode: Word);
    destructor Destroy; override;
    procedure PostLog(S: string);
    procedure Terminate;

    property LogFilename: TFilename read FLogFilename;
    property LogCount: LongInt read FLogCount;
    property Schema: TffSchemaFile read FSchema;
    property Stream: TffFileStream read FStream;
    property Terminated: Boolean read FTerminated;
    property YieldInterval: Word read FYieldInterval write FYieldInterval;
    property OnYield: TffieYieldEvent
      read FOnYield write FOnYield;
  end;

  TffExportEngine = class(TffInOutEngine)
  protected
  public
  end;

  TffImportEngine = class(TffInOutEngine)
  protected
    FieldConverter: TffFieldConverter;
  public
    constructor Create(const aFileName: TffFullFileName);
      { Creates the import engine.  aFilename is the full path and
        filename for the file to import. }
    destructor Destroy; override;

    procedure Import(aTable: TffTable; aBlockInserts: Word);
      { Loads the import file into the given table. Importing only works with
        an existing table. If the import is aborted, the partially loaded
        table remains. }
  end;

implementation

function StripQuotes(S: TffShStr): TffShStr;
begin
  S := FFShStrTrim(S);
  if Copy(S, 1, 1) = '"' then
    Delete(S, 1, 1);
  if COpy(S, Length(S), 1) = '"' then
    Delete(S, Length(S), 1);
  Result := S;
end;


{ TffSchemaFieldList }

procedure TffSchemaFieldList.Add(aFieldItem: TffieFieldItem);
begin
  FList.Add(aFieldItem);
end;

constructor TffSchemaFieldList.Create;
begin
  FList := TList.Create;
end;

destructor TffSchemaFieldList.Destroy;
begin
  FList.Free;
end;

function TffSchemaFieldList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TffSchemaFieldList.GetFieldItem(aIndex: Integer): TffieFieldItem;
begin
  Result := TffieFieldItem(FList.Items[aIndex]);
end;

{ TffSchemaFile }

constructor TffSchemaFile.Create(aFileName: string);
var
  Dir: string;
  FCB: TextFile;                                                      
  Rec: TffShStr;                                                      
begin
  if not FileExists(aFileName) then
    FFRaiseException(EffClientException, ffStrResClient, ffccImport_NoSchemaFile, [aFilename]);

  { TIniFile will look in the WINDOWS directory if no path is given }
  if ExtractFilePath(aFileName) = '' then begin
    GetDir(0, Dir);
    aFileName := Dir + '\' + aFileName;
  end;
  FFileName := aFileName;

  inherited Create(FFileName);

         
  {FMainSection := ChangeFileExt(ExtractFileName(aFileName), '');}
  { Get section header }
  FMainSection := '';
  AssignFile(FCB, FFileName);
  Reset(FCB);
  try
    repeat
      ReadLn(FCB, Rec);
      Rec := FFShStrTrim(Rec);
    until Rec <> '';
    if (Length(Rec) > 2) and (Rec[1] = '[') and (Rec[Length(Rec)] = ']') then
      FMainSection := Copy(Rec, 2, Length(Rec) - 2)
    else
      FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadSchemaHeader, [Rec]);
  finally
    CloseFile(FCB);
  end;
         

  FFields := TffSchemaFieldList.Create;
  LoadFields;

  { Check to see if the first field of a BTF file is the delete flag }
  with Fields.Items[0] do
    FBTFDelFlag := (FileType in [ftBTF, ftVARBTF]) and
                   (Uppercase(fiFieldName) = 'DELFLAG') and
                   (fiNativeType = nftInt32);

  { Get the record length of a fixed ASCII file }
  FRecLength := 0;
  if FileType in [ftASCII, ftBINARY] then begin
    FRecLength := ReadInteger(FMainSection, 'RECLENGTH', 0);
    if FRecLength = 0 then begin

      { reclength required for typed binary files }
      if FileType = ftBinary then
        FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccImport_RECLENGTHRequired);

      { For fixed ASCII, reclength defined by size and position of
        last field with an assumed CRLF }
      with FFields.Items[FFields.Count - 1] do
        FRecLength := fiNativeOffset + fiNativeSize + 2;
    end;
  end;
end;

destructor TffSchemaFile.Destroy;
var
  I: Integer;
begin
  if Assigned(FFields) then
    for I := 0 to FFields.Count - 1 do
      FFields.Items[I].Free;

  FFields.Free;
  inherited Destroy;
end;

procedure TffSchemaFile.BindDictionary(aDictionary: TffDataDictionary);
var
  I: Integer;
  NoMatches: Boolean;
begin
  NoMatches := True;
  for I := 0 to FFields.Count - 1 do
    if not ((I = 0) and BTFDelFlag) then
      with FFields.Items[I] do begin
        fiTargetFieldNo := aDictionary.GetFieldFromName(fiFieldName);
        if fiTargetFieldNo <> -1 then NoMatches := False;
      end;
  if NoMatches then
    FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccImport_NoMatchingFields);
end;

function TffSchemaFile.GetDateMask: string;
begin
  Result := ReadString(FMainSection, 'DATEMASK', DefDateMask);
end;

function TffSchemaFile.GetDblDelims: Boolean;
begin
  Result := ReadBool(FMainSection, 'DBLDELIMS', DefDblDelims);
end;

function TffSchemaFile.GetDelimiter: AnsiChar;
begin
  Result := ReadString(FMainSection, 'DELIMITER', DefDelimitor)[1];
end;

function TffSchemaFile.GetFileType: TffieFileType;
var
  S: string;
begin
  S := ReadString(FMainSection, 'FILETYPE', '');
  if S = '' then
    FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccImport_FILETYPEMissing);
  Result := TffieFileType(GetEnumValue(TypeInfo(TffieFileType), 'ft' + S));
  if Ord(Result) = -1 then
    FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccImport_FILETYPEInvalid);
end;

function TffSchemaFile.GetSeparator: AnsiChar;
begin
  Result := ReadString(FMainSection, 'SEPARATOR', DefSeparator)[1];
end;

procedure TffSchemaFile.LoadFields;

  function BuildField(FieldEntry: TffShStr): TffieFieldItem;
  var
    FieldID: TffShStr;
    Temp: TffShStr;
  begin

    { Parse the FIELD string from the schema file }
    Result := TffieFieldItem.Create;
    with Result do begin
      fiTargetFieldNo := -1;

      { Field ID }
      FFShStrSplit(FieldEntry, '=', Temp, FieldEntry);
      FieldID := FFShStrTrim(Temp);

      { Field name }
      FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
      fiFieldName := FFShStrTrim(Temp);
      if fiFieldName = '' then
        FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadFieldName, [FieldID, fiFieldName]);

      { Import datatype }
      FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
      fiNativeTypeDesc := Uppercase(FFShStrTrim(Temp));

      { Import field size }
      FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
      try
        fiNativeSize := StrToInt(FFShStrTrim(Temp));
      except
        FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadSize, [FieldID, Temp]);
      end;

      { Import decimal places }
      FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
      try
        fiNativeDecPl := StrToInt(FFShStrTrim(Temp));
      except
        FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadDecPl, [FieldID, Temp]);
      end;

      { Import offset }
      FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
      try
        fiNativeOffset := StrToInt(FFShStrTrim(Temp));
      except
        FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadOffset, [FieldID, Temp]);
      end;

      fiDateMask := '';

      { The following tokens are valid for any import filetype }
      if fiNativeTypeDesc = 'CHAR' then
        fiNativeType := nftChar
      else if fiNativeTypeDesc = 'DATE' then begin
        fiNativeType := nftASCIIDate;
        fiDateMask := StripQuotes(FieldEntry);
      end
      else if fiNativeTypeDesc = 'TIME' then begin
        fiNativeType := nftASCIITime;
        fiDateMask := StripQuotes(FieldEntry);
      end
      else if fiNativeTypeDesc = 'TIMESTAMP' then begin
        fiNativeType := nftASCIITimeStamp;
        fiDateMask := StripQuotes(FieldEntry);
      end

      { The following tokens are valid only for ASCII import files }
      else if FileType in [ftASCII, ftCSV] then begin
        if fiNativeTypeDesc = 'BOOL' then
          fiNativeType := nftASCIIBool
        else if fiNativeTypeDesc = 'FLOAT' then
          fiNativeType := nftASCIIFloat
        else if fiNativeTypeDesc = 'NUMBER' then
          fiNativeType := nftASCIINumber
        else if fiNativeTypeDesc = 'LONGINT' then
          fiNativeType := nftASCIILongInt
        else if fiNativeTypeDesc = 'AUTOINC' then
          fiNativeType := nftASCIIAutoInc
        else
          FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadFieldtype, [FieldID, fiNativeTypeDesc]);
      end

      { The following datatype tokens only apply to Binary and BTF files }
      else if FileType in [ftBINARY, ftBTF, ftVARBTF] then begin
        if fiNativeTypeDesc = 'BOOL' then
          fiNativeType := nftBoolean
        else if fiNativeTypeDesc = 'FLOAT' then begin
          case fiNativeSize of
             4: fiNativeType := nftSingle;
             6: fiNativeType := nftReal;
             8: fiNativeType := nftDouble;
            10: fiNativeType := nftExtended;
            else
              FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadFloatSize, [FieldID]);
          end;
        end
        else if fiNativeTypeDesc = 'INTEGER' then begin
          case fiNativeSize of
            1: fiNativeType := nftInt8;
            2: fiNativeType := nftInt16;
            4: fiNativeType := nftInt32;
            8: fiNativeType := nftComp;
            else
              FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadIntegerSize, [FieldID]);
          end;
        end
        else if fiNativeTypeDesc = 'UINTEGER' then begin
          case fiNativeSize of
            1: fiNativeType := nftUInt8;
            2: fiNativeType := nftUInt16;
            4: fiNativeType := nftUInt32;
            else
              FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadUIntegerSize, [FieldID]);
          end;
        end
        else if fiNativeTypeDesc = 'AUTOINC' then begin
          case fiNativeSize of
            1: fiNativeType := nftAutoInc8;
            2: fiNativeType := nftAutoInc16;
            4: fiNativeType := nftAutoInc32;
            else
              FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadAutoIncSize, [FieldID]);
          end;
        end
        else if fiNativeTypeDesc = 'STRING' then
          fiNativeType := nftLString
        else if fiNativeTypeDesc = 'ASCIIZ' then
          fiNativeType := nftZString
        else if fiNativeTypeDesc = 'UNICODE' then
          fiNativeType := nftUnicode
        else if fiNativeTypeDesc = 'CURRENCY' then
          fiNativeType := nftCurrency
        else if fiNativeTypeDesc = 'DATETIME1' then
          fiNativeType := nftDateTime1
        else if fiNativeTypeDesc = 'DATETIME2' then
          fiNativeType := nftDateTime2
        else if fiNativeTypeDesc = 'STDATE' then
          fiNativeType := nftStDate
        else if fiNativeTypeDesc = 'STTIME' then
          fiNativeType := nftStTime
        else if fiNativeTypeDesc = 'BINARY' then
          fiNativeType := nftBinary
        else
          FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadFieldtype, [FieldID, fiNativeTypeDesc]);
      end
      else
        FFRaiseException(EffClientException, ffStrResClient, ffccImport_BadFieldtype, [FieldID, fiNativeTypeDesc]);
    end;
  end;
var
  SchemaFields: TStringList;
  I: Integer;
begin
  SchemaFields := TStringList.Create;
  try

    { Get all the field descriptors into a stringlist }
    SchemaFields.LoadFromFile(FFileName);

    { Traverse the stringlist and grab all the field descriptors in order }
    for I := 0 to SchemaFields.Count - 1 do
      if FFCmpShStrUC(FFShStrTrim(SchemaFields[I]), 'FIELD', 5) = 0 then
        Fields.Add(BuildField(SchemaFields[I]));
  finally
    SchemaFields.Free;
  end;

  if Fields.Count = 0 then
    FFRaiseExceptionNoData(EffClientException, ffStrResClient, ffccImport_NoFields);
end;

function TffSchemaFile.GetSourceFieldPtr(aBufPtr: Pointer; aFieldNo: Integer): Pointer;
begin
  Result := nil;
  case FileType of
    ftASCII, ftBINARY, ftBTF:
      Result := PChar(aBufPtr) + Fields.Items[aFieldNo].fiNativeOffset;
    ftCSV: ;
    ftVARBTF: ;
  end;
end;

procedure TffSchemaFile.MakeIntoDictionary(aDictionary : TffDataDictionary);
var
  I            : Integer;
  FieldType    : TffFieldType;
  Units, DecPl : Integer;
begin
  for I := 0 to Fields.Count - 1 do
    if not ((I = 0) and BTFDelFlag) then begin
      with Fields.Items[I] do begin
        Units := 0;
        DecPl := 0;
        case fiNativeType of
          nftChar:
            begin
              if fiNativeSize = 1 then begin
                FieldType := fftChar;
                Units := 1;
              end
              else begin
                FieldType := fftShortString;
                Units := fiNativeSize;
              end;
            end;
          nftASCIIFloat:
            begin
              FieldType := fftDouble;
              DecPl := fiNativeDecPl;
            end;
          nftASCIINumber:
            FieldType := fftInt16;
          nftASCIIBool:
            FieldType := fftBoolean;
          nftASCIILongInt:
            FieldType := fftInt32;
          nftASCIIAutoInc:
            FieldType := fftAutoInc;
          nftASCIIDate:
            FieldType := fftDateTime;
          nftASCIITime:
            FieldType := fftDateTime;
          nftASCIITimestamp:
            FieldType := fftDateTime;
          nftInt8:
            FieldType := fftInt8;
          nftInt16:
            FieldType := fftInt16;
          nftInt32:
            FieldType := fftInt32;
          nftAutoInc8,
          nftAutoInc16,
          nftAutoInc32:
            FieldType := fftAutoInc;
          nftUInt8:
            FieldType := fftByte;
          nftUInt16:
            FieldType := fftWord16;
          nftUInt32:
            FieldType := fftWord32;
          nftReal:
            begin
              FieldType := fftDouble;
              DecPl := fiNativeDecPl;
            end;
          nftSingle:
            begin
              FieldType := fftSingle;
              DecPl := fiNativeDecPl;
            end;
          nftDouble:
            begin
              FieldType := fftDouble;
              DecPl := fiNativeDecPl;
            end;
          nftExtended:
            begin
              FieldType := fftExtended;
              DecPl := fiNativeDecPl;
            end;
          nftComp:
            begin
              FieldType := fftComp;
              DecPl := fiNativeDecPl;
            end;
          nftCurrency:
            begin
              FieldType := fftCurrency;
              DecPl := fiNativeDecPl;
            end;
          nftBoolean:
            FieldType := fftBoolean;
          nftDateTime1,
          nftDateTime2:
            FieldType := fftDateTime;
          nftLString:
            begin
              if fiNativeSize = 2 then
                FieldType := fftChar
              else if fiNativeSize <= 256 then
                FieldType := fftShortString
              else FieldType := fftNullString;
              Units := fiNativeSize - 1;
            end;
          nftZString:
            begin
              FieldType := fftNullString;
              Units := fiNativeSize - 1;
            end;
          nftUnicode:
            if fiNativeSize = 2 then
              FieldType := fftWideChar
            else begin
              FieldType := fftWideString;
              Units := (fiNativeSize - 2) div 2;
            end;
          nftStDate:
            FieldType := fftStDate;
          nftStTime:
            FieldType := fftStTime;
          else
            FieldType :=fftByteArray;
            Units := fiNativeSize;
        end;

        aDictionary.AddField(fiFieldName, '', FieldType, Units, DecPl, False, nil);
      end;
    end;
end;

procedure TffSchemaFile.SetDateMask(aValue: string);
begin
  WriteString(FMainSection, 'DATEMASK', aValue);
end;

procedure TffSchemaFile.SetDblDelims(aValue: Boolean);
begin
  WriteBool(FMainSection, 'DBLDELIMS', aValue);
end;

procedure TffSchemaFile.SetDelimiter(aValue: AnsiChar);
begin
  WriteString(FMainSection, 'DELIMITER', aValue);
end;

procedure TffSchemaFile.SetFileType(aValue: TffieFileType);
var
  S: string;
begin
  S := GetEnumName(TypeInfo(TffieFileType), Integer(aValue));
  Delete(S, 1, 2);
  WriteString(FMainSection, 'FILETYPE', S);
end;

procedure TffSchemaFile.SetRecLength(aValue: LongInt);
begin
  FRecLength := aValue;
end;

procedure TffSchemaFile.SetSeparator(aValue: AnsiChar);
begin
  WriteString(FMainSection, 'SEPARATOR', aValue);
end;

{ TffFileStream }

function TffFileStream.GetPercentCompleted: Word;
begin
  Result := Round(Position * 100.0 / Size);
end;

function TffFileStream.Read(var Buffer; Count: LongInt): LongInt;
begin
  if (Position = Size - 1) then begin
    Result := inherited Read(Buffer, 1);
    if Byte(Buffer) = $1A {EOF} then
      Result := 0;
  end
  else
    Result := inherited Read(Buffer, Count);
end;

{ TffFixedFileStream }

constructor TffFixedFileStream.Create(const aFileName: string;
                                            aMode: Word;
                                            aRecLength: LongInt);
begin
  inherited Create(aFileName, aMode);

  if aRecLength > 0 then begin
    FRecLength := aRecLength;
    FNumRecs := Size div RecordLength;
  end;
end;

function TffFixedFileStream.GetNumRecords: LongInt;
begin
  Result := FNumRecs;
end;

function TffFixedFileStream.GetRecordLength: LongInt;
begin
  Result := FRecLength;
end;

function TffFixedFileStream.ReadRec(var Rec): Boolean;
begin
  Result := Read(Rec, RecordLength) <> 0;
end;

{ TffFixedASCIIStream }

function TffFixedASCIIStream.ReadRec(var Rec): Boolean;
var
  Buffer: Word;
begin
  { Determine if we need to account for a CR+LF at the end of each record }
  if Position = 0 then begin
    Result := Read(Rec, RecordLength - 2) <> 0;
    Read(Buffer, 2);
    CRLF := Buffer = $0A0D;
  end
  else begin
    if CRLF then begin
      Result := Read(Rec, RecordLength - 2) <> 0;
      Position := Position + 2;
    end
    else
      Result := Read(Rec, RecordLength) <> 0;
  end;
end;

{ TffFixedBTFStream }

constructor TffFixedBTFStream.Create(const aFileName: string;
                                           aMode: Word;
                                           aDelFlag: Boolean);
begin
  inherited Create(aFileName, aMode, 0);

  DelFieldAvail := aDelFlag;

  { Absorb the BTF header record }
  Position := 8;
  Read(FNumRecs, SizeOf(FNumRecs));
  Read(FRecLength, SizeOf(FRecLength));
  Position := FRecLength;
end;

function TffFixedBTFStream.ReadRec(var Rec): Boolean;
begin
  repeat
    Inc(FNumSkipped);
    Result := inherited ReadRec(Rec);
                      { Skip deleted records}
  until not Result or (not DelFieldAvail or (LongInt(Rec) = 0));
  Dec(FNumSkipped);
end;

{ TffVaryingFileStream }

function TffVaryingFileStream.ReadRec(var Rec): Boolean;
begin
  Result := False;
end;

{ TffFieldConverter }

procedure TffFieldConverter.Init(aFieldBuf: Pointer;
                                 aBufLen: LongInt;
                                 aSchema: TffSchemaFile;
                                 aDictionary: TffDataDictionary);
begin
  FBuffer := aFieldBuf;
  FBufLen := aBufLen;
  FSchema := aSchema;
  FDict := aDictionary;
end;

procedure TffFieldConverter.AdjustMaskAndValue(aMask, aValue: TffShStr;
                                           var aDateMask, aDateValue,
                                               aTimeMask, aTimeValue: TffShStr);
{ Translates a FF date/time mask into one suitable for SysTools conversion
routines (expands token characters out to the correct number of digitis
for each element) }
var
  I, J, K, N: Integer;
  ValueIdx: Integer;
  LastDateCharAt,
  LastTimeCharAt,
  FirstDateCharAt,
  FirstTimeCharAt: SmallInt;
  MaskStart,
  ValueStart: Integer;
  NewMask: string;
  Found: Boolean;
  NoDelimitersFound: Boolean;
begin
  aDateMask := '';
  aDateValue := '';
  aTimeMask := '';
  aTimevalue := '';
  NewMask := '';

  { Match number of digits in the mask with number of
    digits in the data }
  MaskStart := 1;
  ValueStart := 1;
  I := 1;
  NoDelimitersFound := True;
  while I <= Length(aMask) do begin
    { look for the next delimiter in the mask }
    if Pos(aMask[I], 'DMYhmst') = 0 then begin
      NoDelimitersFound := False;
      if I - MaskStart = 0 then begin
        {Error}
        Exit;
      end;

      { aMask[I] is our delimiter; find the position of this delimiter
        in the value  }
      ValueIdx := ValueStart;
      Found := (aValue[ValueIdx] = aMask[I]);
      while not Found and (ValueIdx < Length(aValue)) do begin
        Inc(ValueIdx);
        Found := aValue[ValueIdx] = aMask[I];
      end;

      { Count the digits in this element of the value }
      N := ValueIdx - ValueStart;
      if not Found or (N = 0) then begin
        {error}
        Exit;
      end;

      NewMask := NewMask + FFShStrRepChar(aMask[I - 1], N) + aMask[I];
      MaskStart := I + 1;
      ValueStart := ValueIdx + 1;
    end;
    Inc(I);
  end;

  if NoDelimitersFound then
    NewMask := aMask
  else begin
    { Handle end-of-mask case }
    N := Length(aValue) - ValueStart + 1;
    NewMask := NewMask + FFShStrRepChar(aMask[Length(aMask)], N);
  end;

  {-- Special handling for "seconds" token; truncate fractional seconds --}
  for I := 1 to Length(NewMask) do
    { find start of "seconds" mask }
    if NewMask[I] = 's' then begin
      { Find the end of the "seconds" mask }
      J := I + 1;
      while (NewMask[J] = 's') and (J <= Length(NewMask)) do Inc(J);

      { Find first nondigit character in the "seconds" data }
      K := I;
      while (K < J) and (Pos(aValue[K], '0123456789') <> 0) do Inc(K);

      if K <> J then begin
        { Truncate mask and data }
        Delete(NewMask, K, J - K);
        Delete(aValue, K, J - K);
      end;
      Break;
    end;

  {-- Break up the date and time components --}
  LastDateCharAt := 0;
  LastTimeCharAt := 0;
  FirstDateCharAt := 0;
  FirstTimeCharAt := 0;

  { Find the bounds of each component in the mask }
  for I := 1 to Length(NewMask) do begin
    if Pos(NewMask[I], 'DMY') <> 0 then
      LastDateCharAt := I;
    if Pos(NewMask[I], 'hmst') <> 0 then
      LastTimeCharAt := I;

    J := Length(NewMask) - I + 1;
    if Pos(NewMask[J], 'DMY') <> 0 then
      FirstDateCharAt := J;
    if Pos(NewMask[J], 'hmst') <> 0 then
      FirstTimeCharAt := J;
  end;

  { Return date components }
  if FirstDateCharAt <> 0 then begin
    aDateMask := Copy(NewMask, FirstDateCharAt, LastDateCharAt - FirstDateCharAt + 1);
    aDateValue := Copy(aValue, FirstDateCharAt, LastDateCharAt - FirstDateCharAt + 1);
  end;

  { Return time components }
  if FirstTimeCharAt <> 0 then begin
    aTimeMask := Copy(NewMask, FirstTimeCharAt, LastTimeCharAt - FirstTimeCharAt + 1);
    aTimeValue := Copy(aValue, FirstTimeCharAt, LastTimeCharAt - FirstTimeCharAt + 1);
  end;
end;

function TffFieldConverter.ConvertField(aSourcePtr: Pointer;
                                        aSourceType: TffieNativeFieldType;
                                        aSourceSize: Integer;
                                        aTargetFFType: TffFieldType;
                                        aTargetSize: Integer;
                                        aDateMask: TffShStr): TffResult;
var
  I: Integer;
  MinUnits: Integer;
  SourceFFType: TffFieldType;
  vFloat: Extended;
  vDouble: Double;
  vSmallInt: SmallInt;
  vLongInt: LongInt;
  vDateValue,
  vTimeValue: TffShStr;
  vDateMask,
  vTimeMask: TffShStr;
  Da, Mo, Yr: Integer;
  Hr, Mn, Sc: Integer;
  IsBlank: Boolean;

  function ExtractAsciiField(aPtr: PChar; aSize: SmallInt): TffShStr;
  var
    HoldChar: Char;
  begin
    HoldChar := aPtr[aSize];
    aPtr[aSize] := #0;
    Result := FFStrPasLimit(aPtr, aSize);
    aPtr[aSize] := HoldChar;
  end;

begin
  FillChar(FBuffer^, FBufLen, #0);
  Result := 0;

  { ASCII import fields that are totally blank are treated as nulls }
  if FSchema.FileType = ftASCII then begin
    IsBlank := True;
    for I := 0 to aSourceSize - 1 do begin
      IsBlank := FFCmpB(PByte(LongInt(aSourcePtr) + I)^, $20) = 0;
      if not IsBlank then Break;
    end;

    if IsBlank then begin
      Result := DBIERR_FIELDISBLANK;
      Exit;
    end;
  end;

  case aSourceType of
    nftChar:
      begin
        MinUnits := FFMinI(aSourceSize, aTargetSize);
        case aTargetFFType of
          fftChar:
            Char(FBuffer^) := Char(aSourcePtr^);
          fftShortString, fftShortAnsiStr:
            TffShStr(FBuffer^) := FFShStrTrimR(ExtractAsciiField(aSourcePtr, MinUnits));
          fftNullString, fftNullAnsiStr:
            Move(aSourcePtr^, FBuffer^, MinUnits);
          fftWideChar:
            WideChar(FBuffer^) := FFCharToWideChar(Char(aSourcePtr^));
          fftWideString:
            begin
              { Note: the length of a "wide" field is the number of bytes
                it occupies, not the number of wide chars it will hold. }
              MinUnits := FFMinI(aSourceSize - 1, (aTargetSize div SizeOf(WideChar)) - 1);
              FFShStrLToWideStr(FFShStrTrimR(TffShStr(aSourcePtr^)), FBuffer, MinUnits);
            end;
          else Result := DBIERR_INVALIDFLDXFORM;
        end;
      end;

    nftASCIIFloat:
      begin
        vFloat :=                                                        {!!.02}
          StrToFloat(Trim(ExtractAsciiField(aSourcePtr, aSourceSize)));  {!!.02}
        case aTargetFFType of
          fftSingle:
            Single(FBuffer^) := vFloat;
          fftDouble:
            Double(FBuffer^) := vFloat;
          fftExtended:
            Extended(FBuffer^) := vFloat;
          fftCurrency: Comp(FBuffer^) := vFloat * 10000.0;               {!!.03}
          else Result := DBIERR_INVALIDFLDXFORM;
        end;
      end;

    nftASCIINumber:
      begin
        vSmallInt :=
          StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize)));  {!!.02}
        case aTargetFFType of
          fftByte, fftInt8:
            Byte(FBuffer^) := vSmallInt;
          fftWord16, fftInt16:
            TffWord16(FBuffer^) := vSmallInt;
          fftWord32, fftInt32:
            TffWord32(FBuffer^) :=
              StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
          fftComp:
            Comp(FBuffer^) :=
              StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
          fftCurrency: begin
            Comp(FBuffer^) :=
              StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
            Comp(FBuffer^) := Comp(FBuffer^) * 10000.0;
          end;
          fftAutoInc:
            TffWord32(FBuffer^) := vSmallInt;
          else Result := DBIERR_INVALIDFLDXFORM;
        end;
      end;

    nftASCIIBool:
      if aTargetFFType = fftBoolean then
        Boolean(FBuffer^) := (Char(aSourcePtr^) in ['T', 't', 'Y', 'y', '1'])
      else
        Result := DBIERR_INVALIDFLDXFORM;

    nftASCIILongInt,
    nftASCIIAutoInc:                                                  
      begin
        vLongInt :=
          StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize)));  {!!.02}      
        case aTargetFFType of
          fftWord32, fftInt32:
            TffWord32(FBuffer^) := vLongInt;
          fftComp:
            Comp(FBuffer^) := vLongInt;
          fftCurrency: begin
            Comp(FBuffer^) := vLongInt;
            Comp(FBuffer^) := Comp(FBuffer^) * 10000.0;
          end;
          fftAutoInc:
            TffWord32(FBuffer^) := vLongInt;
          else Result := DBIERR_INVALIDFLDXFORM;
        end;
      end;

    nftASCIIDate:
      begin
        AdjustMaskAndValue(aDateMask, ExtractAsciiField(aSourcePtr, aSourceSize),
                           vDateMask, vDateValue,
                           vTimeMask, vTimeValue);
        DateStringToDMY(vDateMask, vDateValue, Da, Mo, Yr, DefEpoch);
        if (Yr = 0) and (Mo = 0) and (Da = 0) then begin              
          Result := DBIERR_FIELDISBLANK;                              
          Exit;                                                       
        end;                                                          
        {if Yr < 100 then Yr := Yr + DefEpoch;}                        {!!.05 - Deleted}
        Yr := ResolveEpoch(Yr, DefEpoch);                              {!!.05 - Added}
        case aTargetFFType of
          fftDateTime:
            { TDateTime values are stored in the buffer as Delphi 1 dates }
            TDateTime(FBuffer^) := EncodeDate(Yr, Mo, Da) + 693594.0;
          fftStDate:
            TStDate(FBuffer^) := DMYToStDate(Da, Mo, Yr, DefEpoch);
          else Result := DBIERR_INVALIDFLDXFORM;
        end;
      end;

    nftASCIITime:
      begin
        AdjustMaskAndValue(aDateMask, ExtractAsciiField(aSourcePtr, aSourceSize),        
                           vDateMask, vDateValue,
                           vTimeMask, vTimeValue);
        TimeStringToHMS(vTimeMask, vTimeValue, Hr, Mn, Sc);
        case aTargetFFType of
          fftDateTime:
            TDateTime(FBuffer^) := EncodeTime(Hr, Mn, Sc, 0);
          fftStTime:
            TStTime(FBuffer^) := HMSToStTime(Hr, Mn, Sc);
          else Result := DBIERR_INVALIDFLDXFORM;
        end;
      end;

    nftASCIITimestamp:
      begin
        AdjustMaskAndValue(aDateMask, ExtractAsciiField(aSourcePtr, aSourceSize),         
                           vDateMask, vDateValue,
                           vTimeMask, vTimeValue);
        DateStringToDMY(vDateMask, vDateValue, Da, Mo, Yr, DefEpoch);
        if (Yr = 0) and (Mo = 0) and (Da = 0) then begin              
          Result := DBIERR_FIELDISBLANK;                              
          Exit;                                                       
        end;
        {if Yr < 100 then Yr := Yr + DefEpoch;}                        {!!.05 - Deleted}
        Yr := ResolveEpoch(Yr, DefEpoch);                              {!!.05 - Added}
        TimeStringToHMS(vTimeMask, vTimeValue, Hr, Mn, Sc);
        if Hr < 0 then Hr := 0;
        if Mn < 0 then Mn := 0;
        if Sc < 0 then Sc := 0;
        case aTargetFFType of
          fftDateTime:
            { TDateTime values are stored in the buffer as Delphi 1 dates }
            TDateTime(FBuffer^) := EncodeDate(Yr, Mo, Da) + 693594.0 +
                                   EncodeTime(Hr, Mn, Sc, 0);
          fftStDate:
            TStDate(FBuffer^) := DMYToStDate(Da, Mo, Yr, DefEpoch);
          fftStTime:
            TStTime(FBuffer^) := HMSToStTime(Hr, Mn, Sc);
          else Result := DBIERR_INVALIDFLDXFORM;
        end;
      end;

    nftReal:
      begin
        vDouble := Real(aSourcePtr^);
        case aTargetFFType of
          fftSingle:
            Single(FBuffer^) := vDouble;
          fftDouble:
            Double(FBuffer^) := vDouble;
          fftExtended:
            Extended(FBuffer^) := vDouble;
          fftCurrency: begin
            Comp(FBuffer^) := vDouble;
            Comp(FBuffer^) := Comp(FBuffer^) * 10000.0;
          end;
          else Result := DBIERR_INVALIDFLDXFORM;
        end;
      end;

    else begin

      { All remaining datatypes are native to FlashFiler.  Map datatypes and
        use the FF restructure conversion routine. }

      case aSourceType of
        nftInt8:      SourceFFType := fftInt8;
        nftInt16:     SourceFFType := fftInt16;
        nftInt32:     SourceFFType := fftInt32;
        nftUInt8:     SourceFFType := fftByte;
        nftUInt16:    SourceFFType := fftWord16;
        nftUInt32:    SourceFFType := fftWord32;
        nftAutoInc8,
        nftAutoInc16,
        nftAutoInc32: SourceFFType := fftAutoInc;
        nftSingle:    SourceFFType := fftSingle;
        nftDouble:    SourceFFType := fftDouble;
        nftExtended:  SourceFFType := fftExtended;
        nftComp:      SourceFFType := fftComp;
        nftCurrency:  SourceFFType := fftCurrency;
        nftBoolean:   SourceFFType := fftBoolean;
        nftDateTime1: SourceFFType := fftDateTime;
        nftDateTime2:
          begin
            SourceFFType := fftDateTime;
            { TDateTime values must be written to the record buffer as
              Delphi 1 values }
            TDateTime(aSourcePtr^) := TDateTime(aSourcePtr^) + 693594.0;
          end;
        nftLString:   SourceFFType := fftShortString;
        nftZString:   SourceFFType := fftNullString;
        nftUnicode:
          if aSourceSize = 2 then SourceFFType := fftWideChar
          else SourceFFType := fftWideString;
        nftStDate:    SourceFFType := fftStDate;
        nftStTime:    SourceFFType := fftStTime;
      else
        SourceFFType := fftByteArray;
      end;

      Result := FFConvertSingleField(aSourcePtr,
                                     FBuffer,
                                     SourceFFType,
                                     aTargetFFType,
                                     aSourceSize,
                                     aTargetSize);
    end;
  end;
end;

{ TffInOutEngine }

constructor TffInOutEngine.Create(const aFileName: TffFullFileName;
                                        aMode: Word);
begin
  FLogFilename := ChangeFileExt(aFilename, '.LOG');                   
  DeleteFile(FLogFilename);
  FLogCount := 0;
  FTerminated := False;

  FYieldInterval := DefYieldInterval;
  FImportFilename := aFileName;
  FSchema := TffSchemaFile.Create(ChangeFileExt(aFileName, DefExt));
  case FSchema.FileType of
    ftASCII:
      FStream := TffFixedASCIIStream.Create(aFileName, aMode, FSchema.RecordLength);
    ftBINARY:
      FStream := TffFixedFileStream.Create(aFilename, aMode, FSchema.RecordLength);
    ftBTF:
      begin
        FStream := TffFixedBTFStream.Create(aFileName, aMode, FSchema.BTFDelFlag);
        FSchema.RecordLength := FStream.RecordLength;
      end;
    ftCSV: ;
    ftVARBTF: ;
  end;
end;

destructor TffInOutEngine.Destroy;
begin
  if FLogCount <> 0 then
    CloseFile(FLogFile);

  FStream.Free;
  FSchema.Free;
  inherited Destroy;
end;

procedure TffInOutEngine.PostLog(S: string);
begin
  if LogCount = 0 then begin
    AssignFile(FLogFile, FLogFilename);
    Rewrite(FLogFile);
  end;
  WriteLn(FLogFile, S);
  Inc(FLogCount);
end;

procedure TffInOutEngine.Terminate;
begin
  FTerminated := True;
end;

{ TffImportEngine }

constructor TffImportEngine.Create(const aFileName: TffFullFileName);
begin
  inherited Create(aFileName, fmOpenRead);
  FieldConverter := TffFieldConverter.Create;
end;

destructor TffImportEngine.Destroy;
begin
  FieldConverter.Free;
  inherited Destroy;
end;

procedure TffImportEngine.Import(aTable: TffTable; aBlockInserts: Word);
var
  RecBuffer: PByteArray;
  FldBuffer: Pointer;
  FldBufLen: LongInt;
  FFTable: TffTable;
  F: Integer;
  DateMask: TffShStr;
  ProgressPacket: TffieProgressPacket;
  Status: TffResult;
  IsNull: Boolean;
  DoExplicitTrans: Boolean;
  InTransaction: Boolean;
  AutoIncField: Integer;
  AutoIncHighValue: TffWord32;
begin
  if aTable.CursorID = 0 then
    DatabaseError(SDataSetClosed);

  if not aTable.Active then
    DatabaseError(SDataSetClosed);

  { If we only have one insert per transaction, then let the server
    do implicit transactions; it'll be faster }
  if aBlockInserts = 0 then aBlockInserts := 1;
  DoExplicitTrans := (aBlockInserts > 1);

  FFTable := aTable;
  Schema.BindDictionary(FFTable.Dictionary);

  { See if we'll need to deal with an autoinc field }
  AutoIncHighValue := 0;
  if not FFTable.Dictionary.HasAutoIncField(AutoIncField) then
    AutoIncField := -1;

  { Find the largest target field }
  FldBufLen := 0;
  for F := 0 to Schema.Fields.Count - 1 do
    with Schema.Fields.Items[F] do
      if fiTargetFieldNo <> -1 then
        FldBufLen := FFMaxDW(FFTable.Dictionary.FieldLength[fiTargetFieldNo], FldBufLen);

  { Allocate field buffer }
  FFGetMem(FldBuffer, FldBufLen);
  try

    { Bind the field converter }
    FieldConverter.Init(FldBuffer, FldBufLen, Schema, FFTable.Dictionary);

    { Allocate record buffer }
    FFGetMem(RecBuffer, FStream.RecordLength);
    try
      with ProgressPacket do begin
        ppTotalRecs := Stream.NumRecords;
        ppNumRecs := 0;
      end;

      InTransaction := False;
      try

        { For each record in the import file... }
        while FStream.ReadRec(RecBuffer^) do begin
          Inc(ProgressPacket.ppNumRecs);

          { Check to see if we need to send the progress status }
          if (ProgressPacket.ppNumRecs mod YieldInterval) = 0 then
            if Assigned(FOnYield) then begin
              FOnYield(ProgressPacket);
              Application.ProcessMessages;

              { Check for user termination }
              if Terminated then begin
                if InTransaction then
                  aTable.Database.Rollback;
                Exit;
              end;
            end;

          { Blocks inserts within a transaction }
          if DoExplicitTrans and not InTransaction then begin
            aTable.Database.StartTransaction;
            InTransaction := True;
          end;

          aTable.Insert;

          { Set all fields to default (null) values }
          aTable.ClearFields;

          { Find all fields in the import file }
          for F := 0 to Schema.Fields.Count - 1 do begin
            with Schema.Fields.Items[F], FFTable.Dictionary do begin
              if fiTargetFieldNo <> - 1 then begin

                { If we have an ASCII date/time field, fetch the mask }
                DateMask := '';
                if fiNativeType in [nftASCIIDate,
                                    nftASCIITime,
                                    nftASCIITimestamp] then begin
                  DateMask := fiDateMask;
                  if DateMask = '' then DateMask := Schema.DateMask;
                end;

                { Convert the field into FF datatype }
                Status := FieldConverter.ConvertField(Schema.GetSourceFieldPtr(RecBuffer, F),
                                                      fiNativeType,
                                                      fiNativeSize,
                                                      FieldType[fiTargetFieldNo],
                                                      FieldLength[fiTargetFieldNo],
                                                      DateMask);
                with FFTable.Dictionary do begin
                  if Status = 0 then begin

                    { All's well, save the field data to the record buffer }
                    SetRecordField(fiTargetFieldNo,
                                   Pointer(aTable.ActiveBuffer),
                                   FldBuffer);

                    { Check for AutoInc field and retain largest value observed }
                    if fiTargetFieldNo = AutoIncField then begin
                      if FFCmpDW(PffWord32(FldBuffer)^, AutoIncHighValue) > 0 then
                        AutoIncHighValue := PffWord32(FldBuffer)^;
                    end;
                  end
                  else begin

                    { Assign null for this field }
                    SetRecordField(fiTargetFieldNo,
                                   Pointer(aTable.ActiveBuffer),
                                   nil);
                    case Status of
                      DBIERR_INVALIDFLDXFORM:
                        if ProgressPacket.ppNumRecs = 1 then
                          PostLog(Format('Field %s datatype %s is incompatible ' +
                                         'with target field datatype %s',
                                         [fiFieldName,
                                         fiNativeTypeDesc,
                                         GetEnumName(TypeInfo(TffFieldType), Ord(FieldType[fiTargetFieldNo]))
                                         ]));
                    end;
                  end;
                end;
              end;
            end;
          end;

          { Clean up "required" fields that are null; assign binary zero value }
          FillChar(FldBuffer^, FldBufLen, #0);
          with FFTable.Dictionary do begin
            for F := 0 to FieldCount - 1 do begin
              GetRecordField(F, Pointer(aTable.ActiveBuffer), IsNull, nil);
              if IsNull and FieldRequired[F] then
                if not (FieldType[F] in [fftBLOB..ffcLastBLOBType]) then
                  { set nonBLOB fields to zeros }
                  SetRecordField(F, Pointer(aTable.ActiveBuffer), FldBuffer);
                { Required BLOB fields are going to fail if not loaded
                  by the import }
            end;
          end;

          { Post the changes }
          aTable.Post;
          if AutoIncField <> -1 then
            Check(aTable.SetTableAutoIncValue(AutoIncHighValue));

          { See if it's time to commit the transaction }
          if InTransaction and ((ProgressPacket.ppNumRecs mod aBlockInserts) = 0) then begin
            aTable.Database.Commit;
            InTransaction := False;
          end;
        end;

        { Residual inserts need to be posted? }
        if InTransaction then
          aTable.Database.Commit;
      except
        on E:Exception do begin
          if InTransaction then
            aTable.Database.Rollback;
          raise;
        end;
      end;

      { Check to see if we need to send the final progress status }
      if (ProgressPacket.ppNumRecs mod YieldInterval) <> 0 then
        if Assigned(FOnYield) then begin
          FOnYield(ProgressPacket);
          Application.ProcessMessages;
        end;
    finally
      FFFreeMem(RecBuffer, FStream.RecordLength);
    end;
  finally
    FFFreeMem(FldBuffer, FldBufLen);
  end;
end;

end.
