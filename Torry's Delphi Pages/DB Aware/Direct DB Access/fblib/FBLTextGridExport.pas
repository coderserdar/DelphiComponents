{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLTextGridExport.pas
   Copyright (c) 2003-2004 Alessandro Batisti
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
unit FBLTextGridExport;

interface

uses
  SysUtils, Classes, FBLDsql, ibase_h;

type
  TFBLTextGridExport = class(TObject)
  private
    FQuery: TFBLDsql;
    FHeader, FSeparator, FCurrentLine: string;
    FBorder: boolean;
    FHeaderCreated: boolean;            // true if Head is created
    FPrepared: boolean;                 // True if prepared
    FEOF: boolean;                      // end of  lines
    FFieldLen: array of integer;        // Len of each Fields
    FLineNumber: integer;
    procedure Prepare;
  public
    constructor Create(AQuery: TFBLDsql; ABorder: boolean = True);
    destructor Destroy; override;
    procedure NextLine;
    procedure Reset;
    property EOF: boolean read FEOF write FEOF;
    property CurrentLine: string read FCurrentLine;
    property LineNumber: integer read FLineNumber;
  end;

procedure TextGrid(AQuery: TFBLDsql; AOut: TStrings);

implementation

uses
  FBLmixf;

procedure TextGrid(AQuery: TFBLDsql; AOut: TStrings);
var
  ExOut: TFBLTextGridExport;
begin
  ExOut := TFBLTextGridExport.Create(Aquery, True);
  try
    AOut.Clear;
    while not ExOut.EOF do
    begin
      ExOut.NextLine;
      AOut.Add(ExOut.CurrentLine);
    end;
  finally
    exOut.Free;
  end;
end;


constructor TFBLTextGridExport.Create(AQuery: TFBLDsql; ABorder: boolean);
begin
  FQuery := AQuery;
  FBorder := ABorder;
  FHeaderCreated := False;
  FPrepared := False;
end;

//------------------------------------------------------------------------------

destructor TFBLTextGridExport.Destroy;
begin
  {$IFNDEF FPC}
  if FFieldLen <> nil then
    FFieldLen := nil;
  {$ENDIF}
  inherited;
end;

//------------------------------------------------------------------------------

procedure TFBLTextGridExport.Prepare;
var
  i: integer;
  FormatString, FieldName: string;
begin
  FFieldLen := nil;
  SetLength(FFieldlen, FQuery.FieldCount);
  if FBorder then
  begin
    FHeader := '|';
    FSeparator := '+';
  end
  else
  begin
    FHeader := '';
    FSeparator := '';
  end;
  for i := 0 to FQuery.FieldCount - 1 do
  begin
    case FQuery.FieldType(i) of
      SQL_VARYING, SQL_TEXT:
        begin
          if FQuery.FieldSize(i) < Length(FQuery.FieldName(I)) then
            FFieldLen[i] := Length(FQuery.FieldName(i))
          else
            FFieldLen[i] := FQuery.FieldSize(i);
          if (FFieldLen[i] < 6) and FQuery.FieldIsNullable(i) then FFieldLen[i] := 6;
          if FQuery.FieldRealName(i) = 'DB_KEY' then FFieldLen[i] := FFieldLen[i] * 2;
        end;
      SQL_DOUBLE, SQL_FLOAT,
      SQL_D_FLOAT, SQL_INT64:
        if Length(FQuery.FieldName(i)) < 20 then
          FFieldLen[i] := 20
        else
          FFieldLen[i] := Length(FQuery.FieldName(i));
        SQL_LONG:
        if Length(FQuery.FieldName(i)) < 12 then
          FFieldLen[i] := 12
        else
          FFieldLen[i] := Length(FQuery.FieldName(i));
        SQL_SHORT:
        if Length(FQuery.FieldName(i)) < 7 then
          FFieldLen[i] := 7
        else
          FFieldLen[i] := Length(FQuery.FieldName(i));
        SQL_BLOB,
        SQL_ARRAY,
        SQL_QUAD:
        if Length(FQuery.FieldName(i)) < 6 then
          FFieldLen[i] := 6
        else
          FFieldLen[i] := Length(FQuery.FieldName(i));
        SQL_TYPE_TIME:
        if Length(FQuery.FieldName(i)) < Length(TimeToStr(now)) then
          FFieldLen[i] := Length(TimeToStr(now))
        else
          FFieldLen[i] := Length(FQuery.FieldName(i));
        SQL_TYPE_DATE:
        if Length(FQuery.FieldName(i)) < Length(DateToStr(now)) then
          FFieldLen[i] := Length(DateToStr(now))
        else
          FFieldLen[i] := Length(FQuery.FieldName(i));
        SQL_TIMESTAMP:
        if Length(FQuery.FieldName(i)) < Length(DateTimeToStr(now)) then
          FFieldLen[i] := Length(DateTimeToStr(now))
        else
          FFieldLen[i] := Length(FQuery.FieldName(i));
    end;
    FormatString := '%-' + IntToStr(FFieldlen[i]) + 's';
    FieldName := Format(FormatString, [FQuery.FieldName(i)]);
    if FBorder then
    begin
      FHeader := FHeader + FieldName + '|';
      FSeparator := FSeparator + StringOfChar('-', FFieldLen[i]) + '+';
    end
    else
    begin
      FHeader := FHeader + FieldName + ' ';
      FSeparator := FSeparator + StringOfChar('-', FFieldLen[i]) + ' ';
    end;
  end;
  FPrepared := True;
  FLineNumber := 0;
end;

//------------------------------------------------------------------------------

procedure TFBLTextGridExport.NextLine;
var
  i: integer;
  Row, FormatString, Field: string;
begin
  if not FPrepared then
    Prepare;

  if not FHeaderCreated then
  begin
    if FLineNumber = 0 then
    begin
      if FBorder then
        FCurrentLine := FSeparator
      else
        FCurrentLine := FHeader;
      Inc(FLineNumber);
      Exit;
    end
    else if FLineNumber = 1 then
    begin
      if FBorder then
        FCurrentLine := FHeader
      else
      begin
        FCurrentLine := FSeparator;
        FHeaderCreated := True;
      end;
      Inc(FLineNumber);
      Exit;
    end
    else if FLineNumber = 2 then
    begin
      FCurrentLine := FSeparator;
      FHeaderCreated := True;
      Inc(FLineNumber);
      Exit;
    end;
  end;

  if FBorder then
    Row := '|'
  else
    Row := '';

  if not FQuery.EOF then
  begin
    for i := 0 to FQuery.FieldCount - 1 do
    begin
      FormatString := '%-' + IntToStr(FFieldLen[i]) + 's';
      if FQuery.FieldIsNull(i) then
        Field := Format(FormatString, ['(null)'])
      else
        case FQuery.FieldType(i) of
          SQL_BLOB:
            if FQuery.FieldSubType(i) = 1 then
              Field := Format(FormatString, ['(Memo)'])
            else
              Field := Format(FormatString, ['(Blob)']);
            SQL_ARRAY:
            Field := Format(FormatString, ['(Array)']);
          SQL_FLOAT, SQL_D_FLOAT:
            begin
              FormatString := '%-' + IntToStr(FFieldLen[i]) + 'g';
              Field := Format(FormatString, [FQuery.FieldAsFloat(i)]);
            end;
          SQL_DOUBLE:
            begin
              if FQuery.FieldScale(i) = 0 then
              begin
                FormatString := '%-' + IntToStr(FFieldLen[i]) + 'g';
                Field := Format(FormatString, [FQuery.FieldAsDouble(i)]);
              end
              else
              begin
                FormatString := '%' + IntToStr(FFieldLen[i]) + '.' +
                  IntToStr(Abs(FQuery.FieldScale(i))) + 'f';
                Field := Format(FormatString, [FQuery.FieldAsDouble(i)]);
              end;
            end;
          SQL_INT64,
          SQL_LONG, SQL_SHORT:
            if FQuery.FieldScale(i) = 0 then
            begin
              FormatString := '%' + IntToStr(FFieldLen[i]) + 's';
              Field := Format(FormatString, [FQuery.FieldAsString(i)]);
            end
            else
              begin
                FormatString := '%' + IntToStr(FFieldLen[i]) + '.' +
                  IntToStr(abs(FQuery.FieldScale(i))) + 'f';
                Field := Format(FormatString, [FQuery.FieldAsDouble(i)]);
              end;
            else
              if FQuery.FieldRealName(i) = 'DB_KEY' then
                Field := Format(FormatString, [DecodeDB_Key(FQuery.FieldAsString(i))])
              else
                Field := Format(FormatString, [FQuery.FieldAsString(i)]);
        end;
      if FBorder then
        Row := Row + Field + '|'
      else
        Row := Row + Field + ' ';
    end;
    FCurrentLine := Row;
    Inc(FLineNumber);
    FQuery.Next;
  end
  else
  begin
    if not FEOF then
    begin
      if FBorder then
      begin
        FCurrentLine := FSeparator;
        Inc(FLineNumber);
        FEOF := True;
      end
      else
        EOF := True;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TFBLTextGridExport.Reset;
begin
  FPrepared := False;
  FHeaderCreated := False;
  FLineNumber := 0;
  FCurrentLine := '';
end;

//------------------------------------------------------------------------------

end.
