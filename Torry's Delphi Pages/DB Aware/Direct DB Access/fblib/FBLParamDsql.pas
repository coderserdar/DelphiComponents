{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   Copyright (c) 2002-2004 Alessandro Batisti
   fblib@altervista.org
   http://fblib.altervista.org

   --- Extension ----

   File:FBLParamDsql.pas
   Version 1.0, 2005-09-06
   Copyright (c) 2005 Uwe Willmann
   uwe@willmann-weltweit.de

   This file adds a DSQL descendant to Firebird Library
   to support named parameters as known in IBX and many other components.

   Differences to DSQL in detail:

   Named parameters have the form :PARAM_NAME,
   where PARAM_NAME consists of consecutive characters [A-Z][a-z][0-9]_
   without whitespaces.
   Values can be set
   - using ParamByNameAs... procedures, adressing parameters by name,
   - or ParamAs... procedures, adressing parameters by index.
   The general rule is:
     all parameters with the same name always have the same value.

   This behaviour is achieved by a silent "compilation" of the SQL property into
   a version containing ?-placeholders for every parameter
   whenever SQL property is getting changed, and by supplying overwritten
	 parameter-setting procedures to ensure the above rule.
   For instance, when using ParamAs... for referencing a parameter by index,
   silently all other parameters with the same name as the referenced get also
   set to the new value.
   The result of the compilation is available via RawSQL property.

   "?" placeholders may also appear, though it is not advised.
   They receive a generated unique name in order to keep the mapping
   between parameter names and parameter indices consistent.
   Be aware that this way the SQL property gets changed during compilation.

   The new stringlist property ParamNames contains all unique parameter names.

   If an error should be reported during preparing or executing a statement,
   it is tried to parse error line and column from the exceptions' error message.
   This position originally refers to the (automatically generated) text
   in RawSQL property.
   It gets adjusted to point to the equivalent position in SQL property.
   The error message is changed to contain the adjusted position.
   Error position information is available separate via ErrorLine, ErrorCol and
   ErrorPos properties.
   (ErrorPos is the value needed for TMemo.StartSel)

   ---

   A general note of use regarding timing constraints:

   ParamAs... and ParamByName.. procedures can only be used
   after the SQL statement is prepared and
   before the statement is executed via ExecSQL

   The property ParamNames is available after changing of the SQL-property,
   prepare is not necessary to get this info.



   ---

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

{
This unit adds a DSQL descendant to Firebird Library
to support named parameters as known in IBX and many other components.
}
unit FBLParamDsql;

interface

uses
  SysUtils, Classes, FBLDsql;

type
  {@exclude}
  TFBLParamScanner = class
  private
    FText: string;
    FPos: integer;
  public
    constructor Create;
    procedure Start(ASQLText: string);
    function GetNextParameterPosition: integer;
  end;

type
  {This class adds a TFBLDdsql descendant to Firebird Library
   to support named parameters as known in IBX and many other components}
  TFBLParamDsql = class(TFBLDsql)
  private
    {Lists the parameter names,
    where position #i corresponds to DSqls parameter #i}
    FParamMap: TStringList;
    {The query text with named parameters in ":PARAMETERNAME" style
    on assigning a query text, sql gets the same text
    but all ":parameters" get replaced by "?" placeholders}
    FParamSQL: TStrings;
    {Lists all parameters sorted without duplicates }
    FParamNames: TStrings;
    {Last Error position info}
    FErrorCol: integer;
    FErrorLine: integer;
    FErrorPos: integer;
    FScanner: TFBLParamScanner;
    procedure ProcessParams;
    procedure AdjustPosition(var Column, Line, Position: integer);
    function ParseErrorPosInfo(ErrorMessage: string): string;
    procedure CheckParamName(const AParamName: string);
    procedure SetParamSQL(Value: TStrings);
    function GetRawSQL: TStrings;
    procedure InternalSQLChange(Sender: TObject);
  public
    {Lists all parameters sorted without duplicates }
    property ParamNames: TStrings read FParamNames;
    property RawSQL: TStrings read GetRawSQL;
    {
    Info about Error position
    Positional information in SQL Exception messages
    are getting parsed. Due to parameter name replacement
    they are adjusted to fit the sql property
    ErrorCol/ErrorLine gives line (starting at 1) and column,
    ErrorPos gives Position of SQL.Text, as needed for TMemo.StartSel
    }
    property ErrorCol: integer read FErrorCol;
    property ErrorLine: integer read FErrorLine;
    property ErrorPos: integer read FErrorPos;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {Returns the name of a param
    @param(AParamIdx index of the input param)}
    function ParamName(AParamIdx: integer): string;
    {
      Overwritten param functions
      These functions work like the inherited ones,
      but automatically spread the parameter value to every other
      parameter with the same name
      in order to enforce the new parameter semantic
    }

    (* // don't know if/how to handle these regarding multiple occurences
    {Return True if Param can accept null value
    @param(AParamIdx index of the input param)}
    function ParamIsNullable(const AParamIdx: integer): boolean;
    {Return an integer value that identify parameter type
    valid value are constant in ibase_h.pas
    @param(AParamIdx index of the input param)
    @html(<br>) see also @link(FieldType)}
    function ParamType(const AParamIdx: integer): smallint;
    {Return an integer value that identify parameter subtype
    valid value are constant in ibase_h.pas
    @param(AParamIdx index of the input param)}
    function ParamSubType(const AParamIdx: integer): smallint;
    {Return scale for numeric/decimal param
     @param(AParamIdx index of the input param)}
    function ParamScale(const AParamIdx: integer): smallint;
    {Return size in bytes of param
     @param(AParamIdx index of the input param)}
    function ParamSize(const AParamIdx: integer): smallint;
    {Return extended sql description  of param
     @param(AParamIdx index of the input param)}
    function ParamSQLTypeDesc(const AParamIdx: integer): string;
    *)

    {Insert null value in param
    @param(AParamIdx index of the input param)}
    procedure ParamAsNull(const AParamIdx: integer);
    {Insert Short value (smallint) in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsShort(const AParamIdx: integer; AValue: smallint);
    {Insert Long value (Integer) in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsLong(const AParamIdx: integer; AValue: longint);
    {Insert Int64 value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsInt64(const AParamIdx: integer; AValue: int64);
    {Insert string value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsString(const AParamIdx: integer; const AValue: string);
    {Insert Double value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsDouble(const AParamIdx: integer; AValue: double);
    {Insert Float (single) value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsFloat(const AParamIdx: integer; AValue: single);
    {Insert TDateTime value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamAsDateTime(const AParamIdx: integer; AValue: TDateTime);
    {Insert string value in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure BlobParamAsString(const AParamIdx: integer; const AValue: string);
    {Insert Value as TStream in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue TStream source data)}
    procedure BlobParamLoadFromStream(const AParamIdx: integer; AStream: TStream);
    {Copy the content of fileneme in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue TStream source data)}
    procedure BlobParamLoadFromFile(const AParamIdx: integer; const AFileName: string);
    {Return pointer to TFBLDatabase Handle see PISC_DB_HANDLE in ibase_hp.as}

    {
      New ParamByNameAs... procedures
      each corresponds to the equivalent ParamAs... procedure
      but allows parameter adressing by name
    }

    {Insert null value in param
    @param(AParamIdx index of the input param)}
    procedure ParamByNameAsNull(const AParamName: string);
    {Insert Short value (smallint) in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamByNameAsShort(const AParamName: string; AValue: smallint);
    {Insert Long value (Integer) in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamByNameAsLong(const AParamName: string; AValue: longint);
    {Insert Int64 value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamByNameAsInt64(const AParamName: string; AValue: int64);
    {Insert string value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamByNameAsString(const AParamName: string; const AValue: string);
    {Insert Double value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamByNameAsDouble(const AParamName: string; AValue: double);
    {Insert Float (single) value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamByNameAsFloat(const AParamName: string; AValue: single);
    {Insert TDateTime value in param
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure ParamByNameAsDateTime(const AParamName: string; AValue: TDateTime);
    {Insert string value in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue value of param)}
    procedure BlobParamByNameAsString(const AParamName: string; const AValue: string);
    {Insert Value as TStream in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue TStream source data)}
    procedure BlobParamByNameLoadFromStream(const AParamName: string; AStream: TStream);
    {Copy the content of fileneme in BlobParam
    @param(AParamIdx index of the input param)
    @param(AValue TStream source data)}
    procedure BlobParamByNameLoadFromFile(const AParamName: string; const AFileName: string);

    {Works like TParam.AssignValues in BDE
    @param(AParamList list of lines ParamName=ParamValue.
    Every line gets evaluated to ParamAsByName(ParamName, ParamValue)
    Not existing parameter names are ignored.
    Note that it is not possible to assign NULL or Blob values this way}
    procedure AssignValues(AParamList: TStringList);

    {Sets all parameters to NULL, convenient when working with AssignValues}
    procedure ClearParams;

    procedure ExecSQL;
    procedure Prepare;

  published


    {a query statement which may contain colon-prefixed named parameters.
    The statement gets transformed into a query with parameter placeholders
    and assigned to DSQL's SQL-property, preserving the name info internally.
    Named Parameters are set using ParamByName... procedures.}
    property SQL: TStrings read FParamSQL write SetParamSQL;

  end;


implementation

uses
  FBLExcept;

const

  {eventually occuring '?'-parameters get an unique name using this template}
  ParamNameTemplate = 'PARAM_%d';

  { positional information in exception messages follows after this keywords: }
  ErrorLineSymbol = 'LINE ';
  ErrorColSymbol1 = 'COLUMN ';
  ErrorColSymbol2 = 'CHAR ';

  {$IFDEF UNIX}
    NewLineSequence = #10;
  {$ELSE}
    NewLineSequence = #13#10;
  {$ENDIF}

{
procedure Register;
begin
  RegisterComponents('FBLib', [TFBLParamDsql]);
end;
}
{ TFBLParamScanner }


constructor TFBLParamScanner.Create;
begin
  inherited;
	FText := '';
  FPos := 0;
end;

function TFBLParamScanner.GetNextParameterPosition: integer;
var
  State: integer;
  { States of this scanner:
    0: out of quotation, test for quote->1  or '/'->3
    1: within quotation, test for quote, yes -> 2, else ->1
    2: quote within quotation encountered, test for quote ->1, test for '/'->3, else ->0
    3: '/' encountered while not in quotation, test for '*'->4
    4: '*' after '/' encountered - within comment, test for '*'->5, else ->4
    5: '*' after '/*' encountered, test for '/'->0, else ->4
  }
begin
  State := 0;
  while (FPos <= Length(FText)) do
  begin
    case FText[FPos] of
      '''':
        case state of
          0:
            State := 1; // enter quotation
          1:
            State := 2; // quote sign within quotation encountered,
                        // test next character
          2:
            State := 1; // positive test for double quote
                        // stay in quotation
          3:
            State := 0; // negative test for '*' after '/'
          5:
            State := 4; // negative test for '/' after '*'
          // else ignore
        end;
      ':', '?':
        case state of
          0, 2:
            Break;
          3:
            State := 0; // negative test for '*' after '/'
          5:
            State := 4; // negative test for '/' after '*'
          // else ignore
        end;
      '/':
        case state of
          0, 2:
            State := 3; // test for '*'
          3, 5:
            state := 0; // negative test for '*' after '/'
                        // positive test for '/' after '*'
          // else ignore
        end;
      '*':
        case state of
          2:
            State := 0; // negative test for quote
          3:
            State := 4; // positive test for '*' after '/', test for '*/'
          4:
            State := 5; // positive test for '*' after '/*',
                        // wait for '/', else ignore
          5:
            State := 4; // negative test for '/' after '*'
          // else ignore
        end;

    else
      case state of
        2:
          State := 0; // negative test for quote
        3:
          State := 0; // negative test for '*' after '/'
        5:
          State := 4; // negative test for '/' after '*'
        // else ignore
      end;
    end;
    inc(FPos);
  end;
  if FPos > Length(FText) then
    Result := 0
  else
    Result := FPos;
  FPos := FPos + 1;
end;

procedure TFBLParamScanner.Start(ASQLText: string);
begin
	FText := ASQLText;
  FPos := 1;
end;

{ TFBLParamDsql }

constructor TFBLParamDsql.Create(AOwner: TComponent);
begin
  inherited;
  FParamSQL := TStringList.Create;
  FParamMap := TStringList.Create;
  FParamMap.Sorted := False;
  FParamNames := TStringList.Create;
  with FParamNames as TStringList do
  begin
  	Sorted := True;
  	Duplicates := dupIgnore;
	end;
  (FParamSQL as TStringList).OnChange := InternalSQLChange;
  FScanner := TFBLParamScanner.Create;
end;

destructor TFBLParamDsql.Destroy;
begin
  FParamSQL.Destroy;
  FParamSQL := nil;
  FParamMap.Destroy;
  FParamMap := nil;
  FParamNames.Destroy;
  FParamNames := nil;
  FScanner.Destroy;
  FScanner := nil;
  inherited;
end;

procedure TFBLParamDsql.ProcessParams;
var
  j, p, d: integer;
  Line: string;
  ParamName: string;
  ParamID: integer;

begin
  FErrorCol := 0;
  FErrorLine := 0;
  FErrorPos := 0;
  FParamMap.Clear;
  FParamNames.Clear;
  Line := FParamSQL.Text;
  FScanner.Start(Line);
  d := 0;
  p := FScanner.GetNextParameterPosition;
  while p > 0 do
  begin
  	p := p - d;
    case Line[p] of
    '?':
			// Processing of empty names is deferred - see below
      ParamName := '?';
    ':':
      begin
        j := p + 1;
        while (j <= Length(Line)) and (
          Pos(
            Line[j],
            '012345689ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_'
          ) > 0
        ) do
          inc(j);
        ParamName := UpperCase(Copy(Line, p + 1, j - p - 1));
        Line[p] := '?';
        Delete(Line, p + 1, j - p - 1);
        d := d + j - p - 1;
      end
    end;
    FParamMap.Add(ParamName);
    if ParamName <> '?' then
    	FParamNames.Add(ParamName);
    p := FScanner.GetNextParameterPosition;
  end;

	inherited SQL.Text := Line;

  // Process empty names
  // deferred to avoid conflicts with given names
  Line := FParamSQL.Text;
	for j := 0 to FParamMap.Count - 1 do
  	if FParamMap[j] = '?' then
    begin
    	{
        Construct unique parameter name.
        By deferring, even if user-given parameter names accidentally
        (or intentionally...) match the template we get unique names
				Example:

            select * from anytable
            where field1 = ? and field2 in (:param_0, :Param_2)

        not deferring would cause a conflict when choosing
        PARAM_0 for "?".

        But now, the result of this processing is

  			    select * from anytable
            where field1 = :PARAM_1 and field2 in (:PARAM_0, :PARAM_2)
      }

  		ParamID := 0;
      ParamName := format(ParamNameTemplate, [ParamId]);
      {the following loop may seem to go potentially forever
    	but it is unlikely if impossible
      to have more unnamed parameters than high(integer)}
      while FParamMap.IndexOf(ParamName) >= 0 do
      begin
        inc(ParamID);
      	ParamName := format(ParamNameTemplate, [ParamId]);
      end;
			FParamMap[j] := ParamName;
      FParamNames.Add(ParamName);

      {for consistency, replace "?" by the auto-named parameter
      in the SQL property text}
      // search first '?' parameter in original query text
      FScanner.Start(Line);
      d := 0;
      repeat
        p := FScanner.GetNextParameterPosition;
      until (p = 0) or (Line[p + d] = '?');
      if p > 0 then
      begin
      	p := p + d;
      	Line[p] := ':';
        Insert(ParamName, Line, p + 1);
        d := d + Length(ParamName);
      end;

      // The following Code does not work if there are '?' characters
      // within quotes or comments
      // before the actual '?' Parameter
      // FParamSQL.Text := StringReplace(FParamSQL.Text, '?', ParamName, []);
    end;
		FParamSQL.Text := Line;

end;

procedure TFBLParamDsql.SetParamSQL(Value: TStrings);
begin
  FParamSQL.Assign(Value);
end;

procedure TFBLParamDsql.BlobParamByNameAsString(const AParamName,
  AValue: string);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited BlobParamAsString(i, AValue);
end;

procedure TFBLParamDsql.BlobParamByNameLoadFromFile(const AParamName,
  AFileName: string);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited BlobParamLoadFromFile(i, AFileName);
end;

procedure TFBLParamDsql.BlobParamByNameLoadFromStream(const AParamName: string;
  AStream: TStream);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited BlobParamLoadFromStream(i, AStream);
end;

procedure TFBLParamDsql.CheckParamName(const AParamName: string);
begin
  if FParamMap.IndexOf(AParamName) < 0 then
  	EFBLError.Create(format('Unknown parameter %s', [AParamName]));
end;

procedure TFBLParamDsql.ParamByNameAsDateTime(const AParamName: string;
  AValue: TDateTime);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited ParamAsDateTime(i, AValue);
end;

procedure TFBLParamDsql.ParamByNameAsDouble(const AParamName: string;
  AValue: double);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited ParamAsDouble(i, AValue);
end;

procedure TFBLParamDsql.ParamByNameAsFloat(const AParamName: string;
  AValue: single);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited ParamAsFloat(i, AValue);
end;

procedure TFBLParamDsql.ParamByNameAsInt64(const AParamName: string;
  AValue: int64);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited ParamAsInt64(i, AValue);
end;

procedure TFBLParamDsql.ParamByNameAsLong(const AParamName: string;
  AValue: Integer);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited ParamAsLong(i, AValue);
end;

procedure TFBLParamDsql.ParamByNameAsShort(const AParamName: string;
  AValue: smallint);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited ParamAsShort(i, AValue);
end;

procedure TFBLParamDsql.ParamByNameAsString(const AParamName, AValue: string);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited ParamAsString(i, AValue);
end;

procedure TFBLParamDsql.ParamByNameAsNull(const AParamName: string);
var
  i: integer;
  ParamName: string;
begin
  ParamName := UpperCase(AParamName);
  CheckParamName(ParamName);
  with FParamMap do
    for i := 0 to Count - 1 do
      if Strings[i] = ParamName then
        inherited ParamAsNull(i);
end;

function TFBLParamDsql.ParamName(AParamIdx: integer): string;
begin
  if AParamIdx < FParamMap.Count then
    Result := FParamMap[AParamIdx];
end;

procedure TFBLParamDsql.AssignValues(AParamList: TStringList);
var
  i: integer;
begin
	for i := 0 to AParamList.Count - 1 do
  	if fParamNames.Indexof(Uppercase(AParamList.Names[i])) >= 0 then
  		ParamByNameAsString(AParamList.Names[i], AParamList.Values[AParamList.Names[i]]);
end;

procedure TFBLParamDsql.ClearParams;
var
  i: integer;
begin
	for i := 0 to ParamCount - 1 do
  	inherited ParamAsNull(i);
end;

procedure TFBLParamDsql.BlobParamAsString(const AParamIdx: integer;
  const AValue: string);
begin
	inherited BlobParamAsString(AParamIdx, AValue);
end;

procedure TFBLParamDsql.BlobParamLoadFromFile(const AParamIdx: integer;
  const AFileName: string);
begin
	BlobParamByNameLoadFromFile(FParamMap[AParamIdx], AFileName);
end;

procedure TFBLParamDsql.BlobParamLoadFromStream(const AParamIdx: integer;
  AStream: TStream);
begin
	BlobParamByNameLoadFromStream(FParamMap[AParamIdx], AStream);
end;

procedure TFBLParamDsql.ParamAsDateTime(const AParamIdx: integer;
  AValue: TDateTime);
begin
  ParamByNameAsDateTime(FParamMap[AParamIdx], AValue);
end;

procedure TFBLParamDsql.ParamAsDouble(const AParamIdx: integer;
  AValue: double);
begin
	ParamByNameAsDouble(FParamMap[AParamIdx], AValue);
end;

procedure TFBLParamDsql.ParamAsFloat(const AParamIdx: integer;
  AValue: single);
begin
	ParamByNameAsFloat(FParamMap[AParamIdx], AValue);
end;

procedure TFBLParamDsql.ParamAsInt64(const AParamIdx: integer;
  AValue: int64);
begin
  ParamByNameAsInt64(FParamMap[AParamIdx], AValue);
end;

procedure TFBLParamDsql.ParamAsLong(const AParamIdx: integer;
  AValue: Integer);
begin
  ParamByNameAsLong(FParamMap[AParamIdx], AValue);
end;

procedure TFBLParamDsql.ParamAsNull(const AParamIdx: integer);
begin
  ParamByNameAsNull(FParamMap[AParamIdx]);
end;

procedure TFBLParamDsql.ParamAsShort(const AParamIdx: integer;
  AValue: smallint);
begin
  ParamByNameAsShort(FParamMap[AParamIdx], AValue);
end;

procedure TFBLParamDsql.ParamAsString(const AParamIdx: integer;
  const AValue: string);
begin
  ParamByNameAsString(FParamMap[AParamIdx], AValue);
end;

function TFBLParamDsql.GetRawSQL: TStrings;
begin
	Result := inherited SQL;
end;

procedure TFBLParamDsql.InternalSQLChange(Sender: TObject);
var
	OldChange: TNotifyEvent;
begin
	OldChange := (FParamSQL as TStringList).OnChange;
  (FParamSQL as TStringList).OnChange := nil;
	ProcessParams;
  (FParamSQL as TStringList).OnChange := OldChange;
end;

function TFBLParamDsql.ParseErrorPosInfo(ErrorMessage: string): string;
var
	i: integer;
  ColPos: integer;
  ColLength: integer;
  j: integer;
begin

  FErrorline := 0;
  FErrorCol := 0;
  FErrorPos := 0;
  j := 0;
  Result := ErrorMessage;

  // parse Line number
  i := Pos(ErrorLineSymbol, UpperCase(ErrorMessage));
  if i > 0 then
  begin
    i := i + Length(ErrorLineSymbol);
    while
    	(i <= Length(ErrorMessage))
    	and (Pos(ErrorMessage[i], '0123456789') > 0)
    do
    begin
      FErrorline := FErrorline * 10 + ord(ErrorMessage[i]) - ord('0');
      inc(i);
    end;
    j := i;
    ErrorMessage := Copy(ErrorMessage, i, Length(ErrorMessage) - i);
  end;

  if FErrorline > 0 then
  begin
    // parse column number
    i := Pos(ErrorColSymbol1, UpperCase(ErrorMessage));
    if i > 0 then
      i := i + Length(ErrorColSymbol1)
    else
    begin
      i := Pos(ErrorColSymbol2, Uppercase(ErrorMessage));
      if i > 0 then
        i := i + Length(ErrorColSymbol2);
    end;
    if i > 0 then
    begin
      ColPos := i;
      while
        (i <= Length(ErrorMessage))
        and (Pos(ErrorMessage[i], '0123456789') > 0)
      do
      begin
        FErrorcol := FErrorcol * 10 + ord(ErrorMessage[i]) - ord('0');
        inc(i);
      end;
      ColLength := i - ColPos;
      ColPos := ColPos + j - 1;
      FErrorPos := FErrorCol;
      if FErrorPos > 0 then
      begin
        for i := FErrorLine - 2 downto 0 do
          FErrorPos :=
          	FErrorPos + Length(RawSQL.Strings[i]) + Length(NewLineSequence);
        FErrorPos := FErrorPos;
      end;
      AdjustPosition(FErrorCol, FErrorLine, FErrorPos);
      Delete(Result, ColPos, ColLength);
      Insert(format('%d', [FErrorCol]), Result, ColPos);
    end;

  end;

end;

procedure TFBLParamDsql.AdjustPosition(var Column, Line,
  Position: integer);
var
  i, d, p: integer;
begin

  { Adjust position to fit SQL property }
  if FParamMap.Count > 0 then
  begin
  	// Adjust Position
    FScanner.Start(RawSQL.Text);
    d := 0;
    p := FScanner.GetNextParameterPosition;
  	i := 0;
    while (p > 0) and (p <= FErrorPos) do
    begin
    	d := d + Length(fParamMap[i]);
    	p := FScanner.GetNextParameterPosition;
    	inc(i);
    end;
    Position := Position + d;

    // Adjust Column
    i := 0;
    d := 0;
    while (i < FParamSQL.Count - 1)
    	and ((d + Length(FParamSQL.Strings[i])) < FErrorPos)
    do
    begin
      d := d + Length(FParamsQL.Strings[i]) + Length(NewLineSequence);
      inc(i);
    end;
    Column := Position - d;
  end;

end;

procedure TFBLParamDsql.ExecSQL;
begin
	FErrorCol := 0;
  FErrorLine := 0;
  FErrorPos := 0;
	try
	  inherited ExecSQL;
	except
  	on e: exception do
    begin
    	e.Message := ParseErrorPosInfo(e.Message);
      raise;
    end;
  end;
end;

procedure TFBLParamDsql.Prepare;
begin
	FErrorCol := 0;
  FErrorLine := 0;
  FErrorPos := 0;
	try
	  inherited Prepare;
	except
  	on e: exception do
    begin
    	e.Message := ParseErrorPosInfo(e.Message);
      raise;
    end;
  end;
end;

end.
