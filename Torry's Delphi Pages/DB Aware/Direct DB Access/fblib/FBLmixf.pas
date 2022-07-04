{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLmixf.pas
   Copyright (c) 2002-2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.
}
{$I fbl.inc}
unit FBLmixf;

interface


uses SysUtils, ibase_h
     {$IFDEF D6M}
    , Math
     {$ELSE}
    , DateUtils
        {$IFDEF FPC}
        , Math
        {$ENDIF}
     {$ENDIF}
     ;

{$IFDEF D6MFPC}
type 
  TRoundToRange = -37..37;
  {$ENDIF}
  //memory manager routines
procedure FBLCalloc(var APointer; ASize: integer);
procedure FBLMalloc(var APointer; ASize: integer);
procedure FBLFree(var APointer);
  //------------------------------------------------------------------------------
function RandomString: string;
function BlobSize(ABlobHandle: PISC_BLOB_HANDLE): Long;
function SQLTypeDesc(AObj: TXSQLVAR): string;
function DateToSql(ADateTime: TDateTime): string;
function TimeToSql(ADateTime: TDateTime): string;
function DateTimeToSql(ADateTime: TDateTime): string;
function DecodeDB_KEY(const AValue: string): string;
{$IFDEF D6M}
function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute,
  ASecond, AMilliSecond: word): TDateTime;
procedure DecodeDateTime(const AValue: TDateTime;
  out AYear, AMonth, ADay, AHour, AMinute, ASecond, AMilliSecond: word);
{$ENDIF}
{$IFDEF D6MFPC}
function RoundTo(const AValue: double; const ADigit: TRoundToRange): double;
{$ENDIF}

implementation

uses
  FBLExcept;

// memory allocation routines
procedure FBLCalloc(var APointer; ASize: integer);
var
  i: integer;
begin
  ReallocMem(Pointer(APointer), ASize);
  for i := 0 to ASize - 1 do
    PChar(APointer)[i] := #0;
end;

procedure FBLMalloc(var APointer; ASize: integer);
begin
  ReallocMem(Pointer(APointer), ASize);
end;

procedure FBLFree(var APointer);
begin
  ReallocMem(Pointer(APointer), 0);
end;

//------------------------------------------------------------------------------

function RandomString: string;
var
  i: integer;
begin
  SetLength(Result, 15);
  for i := 1 to 15 do
    Result[i] := Chr(Random(25) + 65);
end;

//------------------------------------------------------------------------------

{$IFDEF D6M}
procedure DecodeDateTime(const AValue: TDateTime; out AYear, AMonth, ADay,
  AHour, AMinute, ASecond, AMilliSecond: word);
begin
  DecodeDate(AValue, AYear, AMonth, ADay);
  DecodeTime(AValue, AHour, AMinute, ASecond, AMilliSecond);
end;

 //-----------------------------------------------------------------------------

function EncodeDateTime(const AYear, AMonth, ADay, AHour, AMinute, ASecond,
  AMilliSecond: word): TDateTime;
var
  tmpTime: TDateTime;
begin
  tmpTime := EncodeTime(AHour, AMinute, ASecond, AMillisecond);
  Result := EncodeDate(AYear, AMonth, ADay) + tmpTime;
end;
{$ENDIF}
 //-----------------------------------------------------------------------------
{$IFDEF D6MFPC}
function RoundTo(const AValue: double; const ADigit: TRoundToRange): double;
var
  LFactor: extended;
begin
  LFactor := IntPower(10, ADigit);
  Result := Round(AValue / LFactor) * LFactor;
end;

{$ENDIF}
//------------------------------------------------------------------------------

function BlobSize(ABlobHandle: PISC_BLOB_HANDLE): Long;
var
  Item: char;
  Buffer: array[0..99] of char;
  Status_vector: ISC_STATUS_VECTOR;
  Len: integer;
begin
  Item := char(isc_info_blob_total_length);
  if isc_blob_info(@Status_Vector, ABlobHandle, SizeOf(Item), @Item,
    SizeOf(Buffer), Buffer) > 0 then
    FBLShowError(@Status_vector);
  Len := isc_vax_integer(@buffer[1], 2);
  Result := isc_vax_integer(@buffer[3], Short(Len));
end;

//------------------------------------------------------------------------------

function SQLTypeDesc(AObj: TXSQLVAR): string;
begin
  case AObj.sqltype and (not 1) of
    SQL_VARYING: Result := Format('VARCHAR(%d)', [AObj.sqllen]);
    SQL_TEXT: Result := Format('CHAR(%d)', [AObj.sqllen]);
    SQL_DOUBLE:
      begin
        if AObj.sqlscale = 0 then
          Result := 'DOUBLE PRECISION'
        else
          Result := format('NUMERIC(N,%d)', [Abs(AObj.sqlscale)]);
      end;
    SQL_D_FLOAT,
    SQL_FLOAT: Result := 'FLOAT';
    SQL_LONG:
      begin
        if AObj.sqlscale = 0 then
          Result := 'INTEGER'
        else if AObj.sqlsubtype = 1 then
          Result := format('NUMERIC(N,%d)', [abs(AObj.sqlscale)])
        else if AObj.sqlsubtype = 2 then
          Result := format('DECIMAL(N,%d)', [abs(AObj.sqlscale)]);
      end;
    SQL_SHORT:
      begin
        if AObj.sqlscale = 0 then
          Result := 'SMALLINT'
        else if AObj.sqlsubtype = 1 then
          Result := format('NUMERIC(N,%d)', [abs(AObj.sqlscale)])
        else if AObj.sqlsubtype = 2 then
          Result := format('DECIMAL(N,%d)', [abs(AObj.sqlscale)]);
      end;
    SQL_TIMESTAMP: Result := 'TIMESTAMP';
    SQL_BLOB:
      begin
        Result := 'BLOB';
        case AObj.sqlsubtype of
          1: Result := Result + ' SUB_TYPE TEXT';
          2: Result := Result + ' SUB_TYPE 2';
          3: Result := Result + ' SUB_TYPE 3';
          4: Result := Result + ' SUB_TYPE 4';
          5: Result := Result + ' SUB_TYPE 5';
          6: Result := Result + ' SUB_TYPE 6';
          7: Result := Result + ' SUB_TYPE 7';
          8: Result := Result + ' SUB_TYPE 8';
        end;
      end;

    SQL_ARRAY: Result := 'ARRAY';
    SQL_QUAD: Result := 'QUAD';
    SQL_TYPE_TIME: Result := 'TIME';
    SQL_TYPE_DATE: Result := 'DATE';
    SQL_INT64:
      begin
        if AObj.sqlscale = 0 then
          Result := 'INT64'
        else if AObj.sqlsubtype = 1 then
          Result := format('NUMERIC(N,%d)', [Abs(AObj.sqlscale)])
        else if AObj.sqlsubtype = 2 then
          Result := format('DECIMAL(N,%d)', [Abs(AObj.sqlscale)]);
      end;
  end;
end;

//------------------------------------------------------------------------------

function DateToSql(ADateTime: TDateTime): string;
var
  Y, M, D: word;
begin
  Y := 0;
  M := 0;
  D := 0;
  DecodeDate(ADateTime, Y, M, D);
  Result := Format('%d-%d-%d', [Y, M, D]);
end;

//------------------------------------------------------------------------------

function TimeToSql(ADateTime: TDateTime): string;
var
  H, M, S, MS: word;
begin
  H := 0;
  M := 0;
  S := 0;
  MS := 0;
  DecodeTime(ADateTime, H, M, S, MS);
  Result := Format('%d:%d.%d.%d', [H, M, S, MS]);
end;

//------------------------------------------------------------------------------

function DateTimeToSql(ADateTime: TDateTime): string;
var
  Y, M, D, H, MI, S, MS: word;
begin
  DecodeDateTime(ADateTime, Y, M, D, H, MI, S, MS);
  Result := Format('%d-%d-%d %d:%d.%d.%d', [Y, M, D, H, MI, S, MS]);
end;

//------------------------------------------------------------------------------

function DecodeDB_KEY(const AValue: string): string;
var
  i, l: integer;
begin
  Result := '';
  l := Length(AValue);
  for i := 1 to l do
    Result := Result + IntToHex(integer(AValue[i]), 2);
end;

//------------------------------------------------------------------------------
end.
