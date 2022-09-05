{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcMySqlUtils;

interface

{$I ZDbc.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZPlainMySqlDriver, ZPlainMySqlConstants, ZDbcLogging,
  ZCompatibility, ZDbcResultSetMetadata, ZVariant, ZDbcMySql;

const
  MAXBUF = 65535;

type
  {** Silent exception }
  EZMySQLSilentException = class(EAbort);

{**
  Converts a MySQL native types into ZDBC SQL types.
  @param PlainDriver a native MySQL plain driver.
  @param FieldHandle a handler to field description structure.
  @param FieldFlags field flags.
  @return a SQL undepended type.
}
function ConvertMySQLHandleToSQLType(FieldHandle: PZMySQLField;
  CtrlsCPType: TZControlsCodePage; MySQL_FieldType_Bit_1_IsBoolean: Boolean): TZSQLType;

{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckMySQLError(const PlainDriver: IZMySQLPlainDriver;
  Handle: PMySQL; LogCategory: TZLoggingCategory;
  const LogMessage: RawByteString; ConSettings: PZConSettings);
procedure CheckMySQLPrepStmtError(const PlainDriver: IZMySQLPlainDriver;
  Handle: PMySQL; LogCategory: TZLoggingCategory;
  const LogMessage: RawByteString; ConSettings: PZConSettings;
  ErrorIsIgnored: PBoolean = nil; IgnoreErrorCode: Integer = 0);

procedure EnterSilentMySQLError;
procedure LeaveSilentMySQLError;

{**
  Decodes a MySQL Version Value encoded with format:
   (major_version * 10,000) + (minor_version * 100) + sub_version
  into separated major, minor and subversion values
  @param MySQLVersion an integer containing the MySQL Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
procedure DecodeMySQLVersioning(const MySQLVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);

{**
  Encodes major, minor and subversion (revision) values in MySQL format:
   (major_version * 10,000) + (minor_version * 100) + sub_version
  For example, 4.1.12 is returned as 40112.
  @param MajorVersion an integer containing the Major Version.
  @param MinorVersion an integer containing the Minor Version.
  @param SubVersion an integer containing the Sub Version (revision).
  @return an integer containing the full version.
}
function EncodeMySQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;

{**
  Decodes a MySQL Version Value and Encodes it to a Zeos SQL Version format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param MySQLVersion an integer containing the Full Version to decode.
  @return Encoded Zeos SQL Version Value.
}
function ConvertMySQLVersionToSQLVersion( const MySQLVersion: Integer ): Integer;

function getMySQLFieldSize (field_type: TMysqlFieldTypes; field_size: LongWord): LongWord;

{**
  Returns a valid TZColumnInfo from a FieldHandle
  @param PlainDriver the MySQL PlainDriver interface
  @param FieldHandle the handle of the fetched field
  @returns a new TZColumnInfo
}
function GetMySQLColumnInfoFromFieldHandle(FieldHandle: PZMySQLField;
  ConSettings: PZConSettings; MySQL_FieldType_Bit_1_IsBoolean: boolean): TZColumnInfo;

procedure ConvertMySQLColumnInfoFromString(var TypeName: RawByteString;
  ConSettings: PZConSettings; out TypeInfoSecond: RawByteString;
  out FieldType: TZSQLType; out ColumnSize: Integer; out Scale: Integer;
  MySQL_FieldType_Bit_1_IsBoolean: Boolean);

function MySQLPrepareAnsiSQLParam(const Connection: IZMySQLConnection;
  const Value: TZVariant; const DefaultValue: String;
  const ClientVarManager: IZClientVariantManager;
  InParamType: TZSQLType; UseDefaults: Boolean): RawByteString;

function ReverseWordBytes(Src: Pointer): Word;
function ReverseLongWordBytes(Src: Pointer; Len: Byte): LongWord;
function ReverseQuadWordBytes(Src: Pointer; Len: Byte): UInt64;

implementation

uses {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} Math,
  ZMessages, ZDbcUtils, ZFastCode, ZEncoding;

threadvar
  SilentMySQLError: Integer;

procedure EnterSilentMySQLError;
begin
  Inc(SilentMySQLError);
end;

procedure LeaveSilentMySQLError;
begin
  Dec(SilentMySQLError);
end;

{**
  Converts a MySQL native types into ZDBC SQL types.
  @param PlainDriver a native MySQL plain driver.
  @param FieldHandle a handler to field description structure.
  @param FieldFlags a field flags.
  @return a SQL undepended type.
}
function ConvertMySQLHandleToSQLType(FieldHandle: PZMySQLField;
  CtrlsCPType: TZControlsCodePage; MySQL_FieldType_Bit_1_IsBoolean: Boolean): TZSQLType;
begin
    case PMYSQL_FIELD(FieldHandle)^._type of
    FIELD_TYPE_TINY:
      if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0
      then Result := stShort
      else Result := stByte;
    FIELD_TYPE_YEAR:
      Result := stWord;
    FIELD_TYPE_SHORT:
      if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0
      then Result := stSmall
      else Result := stWord;
    FIELD_TYPE_INT24, FIELD_TYPE_LONG:
      if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0
      then Result := stInteger
      else Result := stLongWord;
    FIELD_TYPE_LONGLONG:
      if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0
      then Result := stLong
      else Result := stULong;
    FIELD_TYPE_FLOAT:
      Result := stDouble;//stFloat;
    FIELD_TYPE_DECIMAL, FIELD_TYPE_NEWDECIMAL: {ADDED FIELD_TYPE_NEWDECIMAL by fduenas 20-06-2006}
      if PMYSQL_FIELD(FieldHandle)^.decimals = 0 then
        if PMYSQL_FIELD(FieldHandle)^.length < 11 then
          if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0 then
            Result := stInteger
          else
            Result := stLongWord
        else
          if PMYSQL_FIELD(FieldHandle)^.flags and UNSIGNED_FLAG = 0 then
             Result := stLong
          else
            Result := stULong
      else
        Result := stDouble;
    FIELD_TYPE_DOUBLE:
      Result := stDouble;
    FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
      Result := stDate;
    FIELD_TYPE_TIME:
      Result := stTime;
    FIELD_TYPE_DATETIME, FIELD_TYPE_TIMESTAMP:
      Result := stTimestamp;
    FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB,
    FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB:
      if //((PMYSQL_FIELD(FieldHandle).flags and BINARY_FLAG) = 0)
         (PMYSQL_FIELD(FieldHandle)^.charsetnr <> 63{binary}) then
        If ( CtrlsCPType = cCP_UTF16) then
          Result := stUnicodeStream
        else
          Result := stAsciiStream
      else
        Result := stBinaryStream;
    FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.1/en/bit-type.html
      case PMYSQL_FIELD(FieldHandle)^.length of
        1: if MySQL_FieldType_Bit_1_IsBoolean
           then Result := stBoolean
           else result := stByte;
        2..8: Result := stByte;
        9..16: Result := stWord;
        17..32: Result := stLongWord;
        else Result := stULong;
      end;
    FIELD_TYPE_VARCHAR,
    FIELD_TYPE_VAR_STRING,
    FIELD_TYPE_STRING:
      if //((PMYSQL_FIELD(FieldHandle)^.flags and BINARY_FLAG) = 0)
         (PMYSQL_FIELD(FieldHandle)^.charsetnr <> 63{binary}) then
        if ( CtrlsCPType = cCP_UTF16)
        then Result := stUnicodeString
        else Result := stString
      else Result := stBytes;
    FIELD_TYPE_ENUM:
      Result := stString;
    FIELD_TYPE_SET:
      Result := stString;
    FIELD_TYPE_NULL:
      // Example: SELECT NULL FROM DUAL
      Result := stString;
   FIELD_TYPE_GEOMETRY:
      // Todo: Would be nice to show as WKT.
      Result := stBinaryStream;
   else
      raise Exception.Create('Unknown MySQL data type!');
   end;
end;

{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckMySQLError(const PlainDriver: IZMySQLPlainDriver;
  Handle: PMySQL; LogCategory: TZLoggingCategory;
  const LogMessage: RawByteString; ConSettings: PZConSettings);
var
  ErrorMessage: RawByteString;
  ErrorCode: Integer;
begin
  ErrorMessage := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}Trim(PlainDriver.GetLastError(Handle));
  ErrorCode := PlainDriver.GetLastErrorCode(Handle);
  if (ErrorCode <> 0) and (ErrorMessage <> '') then
  begin
    if SilentMySQLError > 0 then
      raise EZMySQLSilentException.CreateFmt(SSQLError1, [ErrorMessage]);

    DriverManager.LogError(LogCategory, ConSettings.Protocol, LogMessage,
      ErrorCode, ErrorMessage);
    raise EZSQLException.CreateWithCode(ErrorCode,
      Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(
        ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]));
  end;
end;

procedure CheckMySQLPrepStmtError(const PlainDriver: IZMySQLPlainDriver;
  Handle: PMySQL; LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  ConSettings: PZConSettings; ErrorIsIgnored: PBoolean = nil; IgnoreErrorCode: Integer = 0);
var
  ErrorMessage: RawByteString;
  ErrorCode: Integer;
begin
  ErrorCode := PlainDriver.stmt_errno(Handle);
  if Assigned(ErrorIsIgnored) then
    if (IgnoreErrorCode = ErrorCode) then
    begin
      ErrorIsIgnored^ := True;
      Exit;
    end
    else
      ErrorIsIgnored^ := False;
  ErrorMessage := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}Trim(PlainDriver.stmt_error(Handle));
  if (ErrorCode <> 0) and (ErrorMessage <> '') then
  begin
    if SilentMySQLError > 0 then
      raise EZMySQLSilentException.CreateFmt(SSQLError1, [ErrorMessage]);

    DriverManager.LogError(LogCategory, ConSettings^.Protocol, LogMessage,
      ErrorCode, ErrorMessage);
    raise EZSQLException.CreateWithCode(ErrorCode,
      Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(
        ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]));
  end;
end;

{**
  Decodes a MySQL Version Value encoded with format:
   (major_version * 10,000) + (minor_version * 100) + sub_version
  into separated major, minor and subversion values
  @param MySQLVersion an integer containing the MySQL Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
procedure DecodeMySQLVersioning(const MySQLVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);
begin
  MajorVersion := MySQLVersion div 10000;
  MinorVersion := (MySQLVersion - (MajorVersion * 10000)) div 100;
  SubVersion   := MySQLVersion-(MajorVersion*10000)-(MinorVersion*100);
end;

{**
  Encodes major, minor and subversion (revision) values in MySQL format:
   (major_version * 10,000) + (minor_version * 100) + sub_version
  For example, 4.1.12 is returned as 40112.
  @param MajorVersion an integer containing the Major Version.
  @param MinorVersion an integer containing the Minor Version.
  @param SubVersion an integer containing the Sub Version (revision).
  @return an integer containing the full version.
}
function EncodeMySQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;
begin
 Result := (MajorVersion * 10000) + (MinorVersion * 100) + SubVersion;
end;

{**
  Decodes a MySQL Version Value and Encodes it to a Zeos SQL Version format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  So it transforms a version in format XYYZZ to XYYYZZZ where:
   X = major_version
   Y = minor_version
   Z = sub version
  @param MySQLVersion an integer containing the Full MySQL Version to decode.
  @return Encoded Zeos SQL Version Value.
}
function ConvertMySQLVersionToSQLVersion( const MySQLVersion: Integer ): integer;
var
   MajorVersion, MinorVersion, SubVersion: Integer;
begin
 DecodeMySQLVersioning(MySQLVersion,MajorVersion,MinorVersion,SubVersion);
 Result := EncodeSQLVersioning(MajorVersion,MinorVersion,SubVersion);
end;

function getMySQLFieldSize(field_type: TMysqlFieldTypes; field_size: LongWord): LongWord;
begin
  case field_type of
    FIELD_TYPE_ENUM:        Result := 1;
    FIELD_TYPE_TINY:        Result := 1;
    FIELD_TYPE_SHORT:       Result := 2;
    FIELD_TYPE_LONG:        Result := 4;
    FIELD_TYPE_LONGLONG:    Result := 8;
    FIELD_TYPE_FLOAT:       Result := 4;
    FIELD_TYPE_DOUBLE:      Result := 8;
    FIELD_TYPE_DATE:        Result := sizeOf(TMYSQL_TIME);
    FIELD_TYPE_TIME:        Result := sizeOf(TMYSQL_TIME);
    FIELD_TYPE_DATETIME:    Result := sizeOf(TMYSQL_TIME);
    FIELD_TYPE_TINY_BLOB:   Result := field_size; //stBytes
    FIELD_TYPE_BLOB:        Result := field_size;
    FIELD_TYPE_STRING:      Result := field_size;
  else
    Result := 255;  {unknown ??}
  end;
end;

{**
  Returns a valid TZColumnInfo from a FieldHandle
  @param PlainDriver the MySQL PlainDriver interface
  @param FieldHandle the handle of the fetched field
  @returns a new TZColumnInfo
}
function GetMySQLColumnInfoFromFieldHandle(FieldHandle: PZMySQLField;
  ConSettings: PZConSettings; MySQL_FieldType_Bit_1_IsBoolean:boolean): TZColumnInfo;
var
  FieldLength: ULong;
  function ValueToString(Buf: PAnsiChar; Len: Cardinal): String;
  {$IFNDEF UNICODE}
  var tmp: ZWideString;
  {$ENDIF}
  begin
    {$IFDEF UNICODE}
    Result := PRawToUnicode(Buf, Len, ConSettings^.ClientCodePage^.CP);
    {$ELSE}
    if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)
    then System.SetString(Result, Buf, Len)
    else begin
      tmp := PRawToUnicode(Buf, len, ConSettings^.ClientCodePage^.CP);
      Result := ZUnicodeToString(tmp, ConSettings^.CTRL_CP);
    end;
    {$ENDIF}
  end;
begin
  if Assigned(FieldHandle) then
  begin
    Result := TZColumnInfo.Create;
    Result.ColumnLabel := ValueToString(PMYSQL_FIELD(FieldHandle)^.name,
      PMYSQL_FIELD(FieldHandle)^.name_length);
    Result.TableName := ValueToString(PMYSQL_FIELD(FieldHandle)^.org_table,
      PMYSQL_FIELD(FieldHandle)^.org_table_length);
    if Result.TableName <> '' then begin
      Result.ColumnName := ValueToString(PMYSQL_FIELD(FieldHandle)^.org_name,
        PMYSQL_FIELD(FieldHandle)^.org_name_length);
      {JDBC maps the MySQL MYSQK_FIELD.db to Catalog:
       see: https://stackoverflow.com/questions/7942520/relationship-between-catalog-schema-user-and-database-instance}
      Result.CatalogName := ValueToString(PMYSQL_FIELD(FieldHandle)^.db,
        PMYSQL_FIELD(FieldHandle)^.db_length);
    end;
    Result.ReadOnly := (PMYSQL_FIELD(FieldHandle)^.org_table = nil) or (PMYSQL_FIELD(FieldHandle)^.org_name = nil);
    Result.Writable := not Result.ReadOnly;
    Result.ColumnType := ConvertMySQLHandleToSQLType(FieldHandle, ConSettings.CPType, MySQL_FieldType_Bit_1_IsBoolean);
    FieldLength := PMYSQL_FIELD(FieldHandle)^.length;
    //EgonHugeist: arrange the MBCS field DisplayWidth to a proper count of Chars

    if Result.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
      Result.ColumnCodePage := ConSettings^.ClientCodePage^.CP
    else
      Result.ColumnCodePage := High(Word);

    if Result.ColumnType in [stString, stUnicodeString] then begin
       Result.CharOctedLength := FieldLength;
       case PMYSQL_FIELD(FieldHandle)^.charsetnr of
        1, 84, {Big5}
        95, 96, {cp932 japanese}
        19, 85, {euckr}
        24, 86, {gb2312}
        38, 87, {gbk}
        13, 88, {sjis}
        35, 90, 128..151:  {ucs2}
          begin
            Result.ColumnDisplaySize := (FieldLength div 4);
            Result.Precision := Result.ColumnDisplaySize;
            if Result.ColumnType = stString then
              Result.CharOctedLength := FieldLength
            else
              Result.CharOctedLength := FieldLength shr 1;
          end;
        33, 83, 192..215, { utf8 }
        97, 98, { eucjpms}
        12, 91: {ujis}
          begin
            Result.ColumnDisplaySize := (FieldLength div 3);
            Result.Precision := Result.ColumnDisplaySize;
            if Result.ColumnType = stString then
              Result.CharOctedLength := FieldLength
            else
              Result.CharOctedLength := Result.ColumnDisplaySize shl 1;
          end;
        54, 55, 101..124, {utf16}
        56, 62, {utf16le}
        60, 61, 160..183, {utf32}
        45, 46, 224..247: {utf8mb4}
          begin
            Result.ColumnDisplaySize := (FieldLength div 4);
            Result.Precision := Result.ColumnDisplaySize;
            if Result.ColumnType = stString then
              Result.CharOctedLength := FieldLength
            else
              Result.CharOctedLength := FieldLength shr 1;
          end;
        else //1-Byte charsets
        begin
          Result.ColumnDisplaySize := FieldLength;
          Result.Precision := FieldLength;
          if Result.ColumnType = stString then
            Result.CharOctedLength := FieldLength
          else
            Result.CharOctedLength := FieldLength shl 1;
        end;
      end
    end else
      Result.Precision := Integer(FieldLength)*Ord(not (PMYSQL_FIELD(FieldHandle)^._type in
        [FIELD_TYPE_BLOB, FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB]));
    Result.Scale := PMYSQL_FIELD(FieldHandle)^.decimals;
    Result.AutoIncrement := (AUTO_INCREMENT_FLAG and PMYSQL_FIELD(FieldHandle)^.flags <> 0);// or
      //(TIMESTAMP_FLAG and PMYSQL_FIELD(FieldHandle)^.flags <> 0);
    Result.Signed := (UNSIGNED_FLAG and PMYSQL_FIELD(FieldHandle)^.flags) = 0;
    if NOT_NULL_FLAG and PMYSQL_FIELD(FieldHandle)^.flags <> 0 then
      Result.Nullable := ntNoNulls
    else
      Result.Nullable := ntNullable;
    // Properties not set via query results here will be fetched from table metadata.
  end
  else
    Result := nil;
end;

procedure ConvertMySQLColumnInfoFromString(var TypeName: RawByteString;
  ConSettings: PZConSettings; out TypeInfoSecond: RawByteString;
  out FieldType: TZSQLType; out ColumnSize: Integer; out Scale: Integer;
  MySQL_FieldType_Bit_1_IsBoolean: Boolean);
const
  GeoTypes: array[0..7] of RawByteString = (
   'point','linestring','polygon','geometry',
   'multipoint','multilinestring','multipolygon','geometrycollection'
  );
var
  TempPos: Integer;
  pB, pC: Integer;
  Signed: Boolean;
label SetLobSize, lByte, lWord, lLong, lLongLong;
begin
  TypeInfoSecond := '';
  Scale := 0;
  ColumnSize := 0;

  TypeName := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}LowerCase(TypeName);
  Signed := (not (ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('unsigned'), TypeName) > 0));
  pB := ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('('), TypeName);
  if pB > 0 then begin
    pC := ZFastCode.PosEx({$IFDEF UNICODE}RawByteString{$ENDIF}(')'), TypeName, pB);
    TypeInfoSecond := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}UpperCase(Copy(TypeName, pB+1, pc-pB-1));
    TypeName := Copy(TypeName, 1, pB-1);
  end;

  { the column type is ENUM}
  if TypeName = 'enum' then begin
    FieldType := stString;
    if not MySQL_FieldType_Bit_1_IsBoolean and ((TypeInfoSecond = '''Y'',''N''') or (TypeInfoSecond = '''N'',''Y''')) then
      FieldType := stBoolean
    else begin
      TempPos := 1;
      while true do begin
        pC := PosEx({$IFDEF UNICODE}RawByteString{$ENDIF}(','), TypeInfoSecond, TempPos);
        if pC > 0 then begin
          TypeInfoSecond[pc] := #0;
          ColumnSize := Max(ColumnSize, ZFastCode.StrLen(@TypeInfoSecond[TempPos])-2);
          //TypeInfoSecond[pc] := ',';
          TempPos := pc+1;
        end else begin
          ColumnSize := Max(ColumnSize, ZFastCode.StrLen(@TypeInfoSecond[TempPos])-2);
          Break;
        end;
      end;
    end
  end else if TypeName = 'set' then begin
    ColumnSize := 255;
    FieldType := stString;
  end else if ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}('int'), TypeName) > 0 then begin
    if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('tiny')) then begin
lByte:
      FieldType := TZSQLType(Ord(stByte)+Ord(Signed));  //0 - 255 or -128 - 127
      ColumnSize := 3+Ord(Signed);
    end else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('small')) then begin
lWord:
      FieldType := TZSQLType(Ord(stWord)+Ord(Signed));  //0 - 65535 or -32768 - 32767
      ColumnSize := 5+Ord(Signed);
    end else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('medium')) or
                EndsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('24')) then begin
      FieldType := TZSQLType(Ord(stLongWord)+Ord(Signed)); //0 - 16777215 or -8388608 - 8388607
      ColumnSize := 8;
    end else if StartsWith(TypeName, {$IFDEF UNICODE}RawByteString{$ENDIF}('big')) then begin
lLongLong:
      FieldType := TZSQLType(Ord(stULong)+Ord(Signed)); //0 - 18446744073709551615 or -9223372036854775808 - 922337203685477580
      ColumnSize := 20;
    end else begin//includes INTEGER
lLong:
      FieldType := TZSQLType(Ord(stLongWord)+Ord(Signed));  //0 - 4294967295 or -2147483648 - 2147483647
      ColumnSize := 10+Ord(Signed);
    end;
  end else if TypeName = 'year' then begin
    FieldType := stWord;  //1901 to 2155, and 0000 in the 4 year format and 1970-2069 if you use the 2 digit format (70-69).
    ColumnSize := 4;
  end else if TypeName = 'real' then begin
    FieldType := stFloat
  end else if {(TypeName = 'float') or }(TypeName = 'decimal') {or StartsWith(TypeName, RawByteString('double'))} then begin
    //read careful! http://dev.mysql.com/doc/refman/5.7/en/floating-point-types.html
    if TypeInfoSecond = '' then begin
      FieldType := stDouble;
      ColumnSize := 12;
    end else begin
      pC := ZFastCode.Pos({$IFDEF UNICODE}RawByteString{$ENDIF}(','), TypeInfoSecond);
      if pC > 0 then begin
        TypeInfoSecond[pC] := #0;
        ColumnSize := RawToIntDef(@TypeInfoSecond[1], 0);
        Scale := RawToIntDef(@TypeInfoSecond[pC+1], 0);
        TypeInfoSecond[pC] := ',';
      end;
      if Scale = 0 then
        if ColumnSize < 10 then
          goto lLong
        else
          goto lLongLong
      else {if ColumnSize < 25 then begin
        FieldType := stFloat;
        ColumnSize := 12;
      end else} begin
        FieldType := stDouble;
        ColumnSize := 22;
      end;
    end
  end else if (TypeName = 'float') or StartsWith(TypeName, RawByteString('double')) then begin
    FieldType := stDouble;
    ColumnSize := 22;
  end else if EndsWith(TypeName, RawByteString('char')) then begin //includes 'VARCHAR'
    FieldType := stString;
    ColumnSize := RawToIntDef(TypeInfoSecond, 0);
  end else if EndsWith(TypeName, RawByteString('binary')) then begin //includes 'VARBINARY'
    FieldType := stBytes;
    ColumnSize := RawToIntDef(TypeInfoSecond, 0);
  end else if TypeName = 'date' then begin
    FieldType := stDate;
    ColumnSize := 10;
  end else if TypeName = 'time' then begin
    FieldType := stTime;
    ColumnSize := 8;
  end else if (TypeName = 'timestamp') or (TypeName = 'datetime') then begin
    FieldType := stTimestamp;
    ColumnSize := 19;
  end else if EndsWith(TypeName, RawByteString('blob')) then begin //includes 'TINYBLOB', 'MEDIUMBLOB', 'LONGBLOB'
    FieldType := stBinaryStream;
SetLobSize:
    if StartsWith(TypeName, RawByteString('tiny')) then
      ColumnSize := 255
    else if StartsWith(TypeName, RawByteString('medium')) then
      ColumnSize := 16277215//may be 65535
    else if StartsWith(TypeName, RawByteString('long')) then
      ColumnSize := High(Integer)//2147483657//may be 65535
    else ColumnSize := MAXBUF;
  end else if EndsWith(TypeName, RawByteString('text')) then begin //includes 'TINYTEXT', 'MEDIUMTEXT', 'LONGTEXT'
    FieldType := stAsciiStream;
    goto SetLobSize;
  end else if TypeName = 'bit' then begin //see: http://dev.mysql.com/doc/refman/5.1/en/bit-type.html
    ColumnSize := RawToIntDef(TypeInfoSecond, 1);
    Signed := False;
    case ColumnSize of
      1: if MySQL_FieldType_Bit_1_IsBoolean
         then FieldType := stBoolean
         else goto lByte;
      2..8: goto lByte;
      9..16: goto lWord;
      17..32: goto lLong;
      else goto lLongLong;
    end;
  end else if TypeName = 'json' then
    FieldType := stAsciiStream
  else
    for pC := 0 to High(GeoTypes) do
       if GeoTypes[pC] = TypeName then begin
          FieldType := stBinaryStream;
          Break;
       end;

  case FieldType of
    stString: if ( ConSettings^.CPType = cCP_UTF16) then FieldType := stUnicodeString;
    stAsciiStream: if ( ConSettings^.CPType = cCP_UTF16) then FieldType := stUnicodeStream;
    stUnknown: raise Exception.Create('Unknown MySQL data type!'+String(TypeName));
  end;
end;

function MySQLPrepareAnsiSQLParam(const Connection: IZMySQLConnection;
  const Value: TZVariant; const DefaultValue: String;
  const ClientVarManager: IZClientVariantManager;
  InParamType: TZSQLType; UseDefaults: Boolean): RawByteString;
var
  TempBytes: TBytes;
  TempBlob: IZBlob;
  CharRec: TZCharRec;
  ConSettings: PZConSettings;
  TempVar: TZVariant;
label SetDefaultVal;
begin
  ConSettings := Connection.GetConSettings;
  if ClientVarManager.IsNull(Value) then
SetDefaultVal:
    if UseDefaults and (DefaultValue <> '') then
      Result := ConSettings^.ConvFuncs.ZStringToRaw(DefaultValue,
        ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)
    else
      Result := 'NULL'
  else
  begin
    case InParamType of
      stBoolean:
        if Connection.MySQL_FieldType_Bit_1_IsBoolean
        then Result := ZSysUtils.BoolStrIntsRaw[ClientVarManager.GetAsBoolean(Value)]
        else if ClientVarManager.GetAsBoolean(Value)
          then Result := '''Y'''
          else Result := '''N''';
      stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
      stFloat, stDouble, stCurrency, stBigDecimal:
        Result := ClientVarManager.GetAsRawByteString(Value);
      stBytes:
        begin
          TempBytes := ClientVarManager.GetAsBytes(Value);
          Result := GetSQLHexAnsiString(PAnsiChar(TempBytes), Length(TempBytes));
        end;
      stString, stUnicodeString: begin
          ClientVarManager.Assign(Value, TempVar);
          CharRec := ClientVarManager.GetAsCharRec(TempVar, Connection.GetConSettings^.ClientCodePage^.CP);
          Result := Connection.EscapeString(CharRec.P, CharRec.Len, True);
        end;
      stDate:
        Result := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
      stTime:
        Result := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
      stTimestamp:
        Result := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
      stAsciiStream, stUnicodeStream, stBinaryStream:
        begin
          TempBlob := ClientVarManager.GetAsInterface(Value) as IZBlob;
          if not TempBlob.IsEmpty then
            if InParamType  = stBinaryStream
            then Result := GetSQLHexAnsiString(PAnsichar(TempBlob.GetBuffer), TempBlob.Length)
            else if TempBlob.IsClob then begin
              CharRec.P := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
              Result := Connection.EscapeString(CharRec.P, TempBlob.Length, True);
            end else
              Result := Connection.EscapeString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                TempBlob.Length, ConSettings))
          else
            goto SetDefaultVal;
        end;
      else
        RaiseUnsupportedParameterTypeException(InParamType);
    end;
  end;
end;

procedure ReverseBytes(const Src, Dest: Pointer; Len: Byte);
var b: Byte;
  P: PAnsiChar;
begin
  Len := Len -1;
  P := PAnsiChar(Src)+Len;
  for b := Len downto 0 do
    (PAnsiChar(Dest)+B)^ := (P-B)^;
end;

function ReverseWordBytes(Src: Pointer): Word;
begin
  (PAnsiChar(@Result)+1)^ := PAnsiChar(Src)^;
  PAnsiChar(@Result)^ := (PAnsiChar(Src)+1)^;
end;

function ReverseLongWordBytes(Src: Pointer; Len: Byte): LongWord;
begin
  Result := 0;
  ReverseBytes(Src, @Result, Len);
end;

function ReverseQuadWordBytes(Src: Pointer; Len: Byte): UInt64;
begin
  Result := 0;
  ReverseBytes(Src, @Result, Len);
end;

end.
