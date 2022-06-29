{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: DerivedDataType.pas,v 1.10 2003/02/21 10:08:19 laa Exp $}

{----------------------------------------------------------------------------
  DerivedDataType    ???

  What:

  Company:           Polycon Ab
  Authors:           ???
----------------------------------------------------------------------------}

unit DerivedDataType;

interface
{$i common.inc}

uses

{$ifndef LINUX}
  Graphics,
{$else}
  QGraphics,
{$endif LINUX}
  Classes,
  DataType;

type
  TIntegerStringType = class(TStringType)
  protected
    constructor Create;

    function GetAlignment : TAlignment; override;
  end;

  TPictureType = class(TObjectType)
  protected
    function GetAsPicture(AValue : TValue) : TPicture; virtual;
  public
    function Value(Val : TPicture) : TValue;

    function FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue; override;
    function ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean; override;
  end;

  TStringDateTimeType = class(TDateTimeType)
  protected
    function GetAsString(Value : TValue) : String; override;
    function GetSQLDataTypeString : String; override;
  public
    function ConvertFromString(StrVal : String) : TDateTime; override;

  end;

function AsStringDateTime(Value : TValue) : TDateTime;
function ValueFromStringDateTime(Val : TDateTime) : TValue;
function AsPicture(Value : TValue) : TPicture;
function ValueFromPicture(Val : TPicture) : TValue;

var
  IntegerStringType : TIntegerStringType;
  PictureType : TPictureType;
  StringDateTimeType : TStringDateTimeType;

implementation

uses SysUtils;

{ TIntegerStringType }

constructor TIntegerStringType.Create;
begin
  Inherited Create(Length(IntToStr(MaxInt)), False);
end;

function TIntegerStringType.GetAlignment: TAlignment;
begin
  Result := taRightJustify;
end;

{ TPictureType }

function TPictureType.GetAsPicture(AValue : TValue) : TPicture;
var
  AObject : TObject;
begin
  AObject := AsObject(AValue);
  if (AObject <> nil) and
     not (AObject is TPicture) then
    raise Exception.Create(Self.ClassName + '.GetAsPicture: You can not get Pictures from this DataType!')
  else
    Result := TPicture(AObject);
end;

function TPictureType.FormattedValue(AValue : TValue; DataFormatter : TDataFormatter) : TValue;
begin
  Result := Inherited FormattedValue(AValue, DataFormatter);
end;

function TPictureType.ParseText(AString : String; var ParsedValue : TValue; DataFormatter : TDataFormatter) : Boolean;
begin
  Result := Inherited ParseText(AString, ParsedValue, DataFormatter);
end;

function TPictureType.Value(Val : TPicture) : TValue;
begin
  Result := Inherited Value(Val);
end;

function AsPicture(Value : TValue) : TPicture;
begin
  Result := PictureType.GetAsPicture(Value);
end;

function ValueFromPicture(Val : TPicture) : TValue;
begin
  Result := PictureType.Value(Val);
end;

{ TStringDateTimeType }

const
  FormatSpecifier = 'yyyy-mm-dd hh/nn/ss';

function ExtractDate(S : String) : TDateTime;
var
  sLength : Integer;
  Year, Month, Day : Word;
begin
  sLength := Length(S);
  if sLength <> Length(FormatSpecifier) then
    raise Exception.Create('ExtractDate: The length of the length of the date/time value ''' +
            S + ''' doesn''t equal the length of the format string ''' + FormatSpecifier + '''!' )
  else
  begin
    Year :=  StrToInt(Copy(S,1,4));
    Month := StrToInt(Copy(S,6,2));
    Day :=   StrToInt(Copy(S,9,2));

    Result := EncodeDate(Year, Month, Day);
  end;
end;

function ExtractTime(S : String) : TDateTime;
var
  sLength : Integer;
  Hour, Min, Sec : Word;
begin
  sLength := Length(S);
  if sLength <> Length(FormatSpecifier) then
    raise Exception.Create('ExtractTime: The length of the length of the date/time value ''' +
            S + ''' doesn''t equal the length of the format string ''' + FormatSpecifier + '''!' )
  else
  begin
    Hour :=  StrToInt(Copy(S,12,2));
    Min :=   StrToInt(Copy(S,15,2));
    Sec :=   StrToInt(Copy(S,18,2));

    Result := EncodeTime(Hour, Min, Sec, 0);
  end;
end;

function AsStringDateTime(Value : TValue) : TDateTime;
begin
  Result := StringDateTimeType.GetAsDateTime(Value);
end;

function ValueFromStringDateTime(Val : TDateTime) : TValue;
begin
  Result := StringDateTimeType.Value(Val);
end;

{ TStringDateTimeType }





function TStringDateTimeType.ConvertFromString(StrVal: String): TDateTime;
begin
  if StrVal = '' then
    Result := 0
  else
    Result := ExtractDate(StrVal) + ExtractTime(StrVal);
end;

function TStringDateTimeType.GetAsString(Value: TValue): String;
begin
  if Value.DataType <> Self then
    Result := AsString(Value)
  else
    Result := FormatDateTime(FormatSpecifier, AsDateTime(Value));
end;

function TStringDateTimeType.GetSQLDataTypeString: String;
begin
  Result := StringType(Length(FormatSpecifier), False).SQLDataTypeString;
end;

initialization
  IntegerStringType := TIntegerStringType.Create;
  PictureType := TPictureType.Create;
  StringDateTimeType := TStringDateTimeType.Create;

finalization
  IntegerStringType.Free;
  PictureType.Free;
  StringDateTimeType.Free;

end.

