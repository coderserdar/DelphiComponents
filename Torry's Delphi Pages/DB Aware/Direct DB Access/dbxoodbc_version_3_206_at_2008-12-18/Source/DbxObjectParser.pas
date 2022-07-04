{
  Part of Kylix / Delphi open source DbExpress driver for ODBC
  Version 3.205 2008-11-27

  Copyright (c) 2002-2009 by Vadim V.Lopushansky

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public License
  as published by the Free Software Foundation; either version 2.1
  of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the GNU Lesser General Public License for more details.
}

unit DbxObjectParser;

{$I DbxOpenOdbc.inc}
{$B-}

{.$DEFINE _DEBUG_}
{.$UNDEF _RegExpr_DumpMatchPos_}
{.$DEFINE _RegExpr_DumpMatchPos_}

{$IFDEF _DEBUG_}
  // - Debugger options
  {$O-,D+,L+}
  {$UNDEF _TRACE_CALLS_}
  {.$DEFINE _TRACE_CALLS_}// logging of calls
{$ELSE}
  // - Release options:
  {$O+}
  {$UNDEF _TRACE_CALLS_}
{$ENDIF}

{
 - Some warnings:
    -  Diferent quotes support only in "Fixed Templates":
        Examples of different quotes:
                   [catalog].[schema].(table)
                   <c:catalog>.<s:schema>.<t:table>
    - "Fixed Templates" do not allowed change Quote at runtime.
       "Fixed tamplate" marked as:
         ...
         QuoteTemplate :#0
         ...

 - Breakpoints:
   #01: DumpMatchPos
   #02: DecodeObjectFullName
}

interface

uses
  SysUtils, Classes,
  RegExpr, // Regular Expression Library.
           // Author: Andrey V. Sorokin,  St-Petersburg,  Russia
           // home pages: http://anso.da.ru ,  http://anso.virtualave.net
           // e-mail: anso@mail.ru, anso@usa.net
{$IFDEF _TRACE_CALLS_}
  DbxOpenOdbcTrace,
{$ENDIF _TRACE_CALLS_}
  DbxOpenOdbcInterface;

type

  TArrayOfInteger = array of integer;
  PArrayOfInteger = ^TArrayOfInteger;
  TArrayOf3Boolean = array[1..3] of Boolean;

  PObjectNameTemplateInfo = ^TObjectNameTemplateInfo;
  TObjectNameTemplateInfo = record
    sName: string;
    sRegExpr: AnsiString;
    QuoteTemplate: AnsiString; // if Quoted then replace template quote in regexp to
                                // driver current quote string(char)
    RegexpMathesIndexes: PArrayOfInteger; // result position of catalog,schema,table. If index<0 or
                                         // mathes[index] not found then result:=value[nextIndex] else result:=''
    FullNameTemplate: AnsiString; // full name format without internal dividers. Only external dividers
    CatalogTemplate: AnsiString;
    SchemaTemplate: AnsiString;
    ObjectTemplate: AnsiString;
    RequiredParts: TArrayOf3Boolean;
    OneDelimiter: Boolean;

    NextTemplate: PObjectNameTemplateInfo;

   //Empty parts of templates
    fEmptyCatalogTemplate: AnsiString;
    fEmptySchemaTemplate: AnsiString;
    fEmptyObjectTemplate: AnsiString;
  end;
  TArrayOfTObjectNameTemplateInfo = array of TObjectNameTemplateInfo;

  TArrayOfTRegExpr = array of TRegExpr;

  TObjectNameParser = class
  private
    fObjectNameTemplateInfo: TArrayOfTObjectNameTemplateInfo;
    fRegExpr: TArrayOfTRegExpr;
  public
    constructor Create(AObjectNameTemplateInfo: PObjectNameTemplateInfo;
      const DbQuote: AnsiString; const sRegExpNew: AnsiString = '');
    destructor Destroy; override;
    // DecodeObjectFullName:
    //   Fix Delphi 2009 compiler bug: Invalid Pointer operation (for AnsiString RefCounter)
    //   Changed: remove const from DecodeObjectFullName
    function DecodeObjectFullName({$IFNDEF _D11UP_}const{$ENDIF} ObjectFullName: AnsiString;
      var sCatalogName, sSchemaName, sObjectName: AnsiString): PObjectNameTemplateInfo;
    function EncodeObjectFullName(const CatalogName, SchemaName, ObjectName: AnsiString;
      pTemplateInfo: PObjectNameTemplateInfo = nil): AnsiString;
    function GetQuotedObjectName(const ObjectName: AnsiString): AnsiString;
  end;

const

// Indexes to accessing to TObjectNameTemplateInfo.RegexpMathesIndexes and
// TObjectNameTemplateInfo.RequiredParts

  idxLength = 0;
  idxCatalog = 1;
  idxSchema = 2;
  idxObject = 3;

//=================================================================
// DEFAULT DATABASE OBJECT NAME FORMAT: "catalog"."schema"."object"
//=================================================================

  DefaultMathesIndexes: array[0..9] of integer = (
    10, // array length
    4, 6, 8, // position info for diferent parts. For accessing usage indexes: idxCatalog, idxSchema, idxObject
    6, 7,    // array of catalog name matches indexes
    11, 12,  // array of schema  name matches indexes
    14, 13   // array of object  name matches indexes
    );
  //
  // for build and debug need use RegExprParser.zip (TestObjectParser.exe)
  //   - run TestObjectParser.exe
  //   - select parser. enter you 'regexp new' if it needed.
  //   - (A)
  //   - enter 'parsing object name'
  //   - press decode
  //   - look dump file 'debug-dump-match-pos.txt'
  //   - (B)
  //   - loop (A)..(B)for different 'parsing object name'. like for MSSQL catalog regexp index dumping:
  //                                                              [catalog].schema.table
  //                                                              "catalog".schema.table
  //                                                              catalog.schema.table
  //   - result for one logic (catalog/schema/object) is set of digits.
  //     on crossing the miscellaneous result ('debug-dump-match-pos.txt') must be empty
  //

  DefaultObjectNameTemplateInfo: TObjectNameTemplateInfo = (
    sName: 'DefaultObjectNameTemplateInfo';
    sRegExpr: '^((((("(.*?)")|([^\."]*?))\.))?((("(.*?)")|([^\."]*?))\.))?("(.*)"|.*)$';
  //           |                             ||                          ||            |
  //            \                           /  \                        /  \          /
  //              -----------   -----------      ----------   ---------      ---  ---
  //                          Y                             Y                   Y
  //                          |                             |                   |
  //                       catalog                        schema              object
  //
    QuoteTemplate: '"'; // Quote can be redefined at runtime ...
    RegexpMathesIndexes: @DefaultMathesIndexes; // result position of catalog, schema, table
  {
   the next chars is reserved:
   #1 - catalog name
   #2 - schema name
   #3 - object name
  }
    FullNameTemplate: '"'#1'"."'#2'"."'#3'"';
    CatalogTemplate: '"'#1'".';
    SchemaTemplate: '"'#2'".';
    ObjectTemplate: '"'#3'"';
    RequiredParts: (True, True, True); // if format is <"catalog"."schema"."table">
    // then call TObjectNameParser.EncodeObjectFullName( catalog='catalog', schema='', object='table')
    // returned empty schema name: <"catalog'.""."table">. But if RequiredParts[idxSchema] will
    // equal False then it returned error result: <"catalog'."table">
    OneDelimiter: True;
    NextTemplate: nil;
    fEmptyCatalogTemplate: '';
    fEmptySchemaTemplate: '';
    fEmptyObjectTemplate: '';
  );

//=================================================================
// SQLSERVER DATABASE OBJECT NAME FORMAT: [catalog].[schema].[object]
//=================================================================

  SQLServerMathesIndexes: array[0..12] of integer = (
    13, // array length
    4, 7, 10,   // position info for diferent parts. For accessing usage indexes: idxCatalog, idxSchema, idxObject
    8, 6, 4,    // array of catalog name matches indexes
    13, 15, 16, // array of schema name matches indexes
    19, 21, 22  // array of object name matches indexes
    );

  SQLServerObjectNameTemplateInfo: TObjectNameTemplateInfo = (
    sName: 'SQLServerObjectNameTemplateInfo';
    sRegExpr: '^(((((\[(.*?)\])|("(.*?)")|([^\."]*?))\.))?(((\[(.*?)\])|("(.*?)")|([^\."]*?))\.))?((\[(.*)\])|("(.*)")|(.*))$';
  //           |                                         ||                                     ||                             |
  //            \                                       /  \                                   /  \                           /
  //              -----------------   -----------------      ---------------   ---------------      -----------   -----------
  //                                Y                                        Y                                  Y
  //                                |                                        |                                  |
  //                             catalog                                  schema                              object
  //
    QuoteTemplate: #0; // "Fixed Templates" do not allowed change Quote at runtime.
    RegexpMathesIndexes: @SQLServerMathesIndexes; // result position of catalog, schema, table
  {
   the next chars is reserved:
   #1 - catalog name
   #2 - schema name
   #3 - object name
  }
    FullNameTemplate: '"'#1'"."'#2'"."'#3'"';
    CatalogTemplate: '"'#1'".';
    SchemaTemplate: '"'#2'".';
    ObjectTemplate: '"'#3'"';
    RequiredParts: (False, False, True); // if format is <"catalog"."schema"."table">
    // then call TObjectNameParser.EncodeObjectFullName( catalog='catalog', schema='', object='table')
    // returned empty schema name: <"catalog'.""."table">. But if RequiredParts[idxSchema] will
    // equal False then it returned error result: <"catalog'."table">
    OneDelimiter: True;
    NextTemplate: nil;
    //NextTemplate: @DefaultObjectNameTemplateInfo;
    fEmptyCatalogTemplate: '';
    fEmptySchemaTemplate: '';
    fEmptyObjectTemplate: '';
  );

//========================================================================
// ORACLE DATABASE OBJECT NAME FORMAT: "catalog"."schema"."package.object"
//========================================================================
// hides quoting for SQLExpr.pas
// user must by itself indicate quoting

  OracleMathesIndexes: array[0..6] of integer = (
    7, // array length
    4, 5, 6, // position info for diferent parts. For accessing usage indexes: idxCatalog, idxSchema, idxObject
    2,       // array of catalog name matches indexes
    4,       // array of schema  name matches indexes
    5        // array of object  name matches indexes
    );
  OracleObjectNameTemplateInfo: TObjectNameTemplateInfo = (
    sName: 'OracleObjectNameTemplateInfo';
    sRegExpr: '^(([^\.]*?)\.)?(([^\.]*?)\.)?(.*)$';
  //           |            ||            ||    |
  //            \           /\           /  \  /
  //              ----  ---    ----   ---    Y
  //                  Y             Y        |
  //                  |             |        |
  //               catalog       schema    object
  //
    QuoteTemplate: #0; // "Fixed Templates" do not allowed change Quote at runtime.
    RegexpMathesIndexes: @OracleMathesIndexes; // result position of catalog, schema, table
  {
   the next chars is reserved:
   #1 - catalog name
   #2 - schema name
   #3 - object name
  }
    FullNameTemplate: #1'.'#2'.'#3;
    CatalogTemplate: #1'.';
    SchemaTemplate: #2'.';
    ObjectTemplate: #3;
    RequiredParts: (True, True, True); // if format is <"catalog"."schema"."table">
    // then call TObjectNameParser.EncodeObjectFullName( catalog='catalog', schema='', object='table')
    // returned empty schema name: <"catalog'.""."table">. But if RequiredParts[idxSchema] will
    // equal False then it returned error result: <"catalog'."table">
    OneDelimiter: True;
    NextTemplate: nil;
    fEmptyCatalogTemplate: '';
    fEmptySchemaTemplate: '';
    fEmptyObjectTemplate: '';
  );

//=================================================================
// INFORMIX DATABASE OBJECT NAME FORMAT: "catalog":"schema"."object"
//=================================================================

  InformixMathesIndexes: array[0..6] of integer = (
    7, // array length
    4, 5, 6, // position info for diferent parts. For accessing usage indexes: idxCatalog, idxSchema, idxObject
    2,       // array of catalog name matches indexes
    5,       // array of schema  name matches indexes
    6        // array of object  name matches indexes
    );

  InformixObjectNameTemplateInfo: TObjectNameTemplateInfo = (
    sName: 'InformixObjectNameTemplateInfo';
    sRegExpr: '^(([^\:\.]*?)(\:){1,2})?(([^\.]*?)\.)?(.*)$';
  //           |                      ||            ||    |
  //            \                    /  \          /  \  /
  //              --------   -------      --   ---     Y
  //                       Y                 Y         |
  //                       |                 |         |
  //                    catalog            schema    object
  //
    QuoteTemplate: #0; // "Fixed Templates" do not allowed change Quote at runtime.
      // identify informix do not supported of quote. Or identify fixed template (do not changed in runtime).
    RegexpMathesIndexes: @InformixMathesIndexes; // result position of catalog, schema, table
  //OLD:
  {
  FullNameTemplate    :#1'::'#2'.'#3; // Two characters ":" it is necessary for the taking into account of logic
  CatalogTemplate     :#1'::';        // of work of the parser of parameters "db.pas:TPasrams.ParseSQL".
  }
  //NEW: Fixed: You should use module "SqlExprFix.pas".
    FullNameTemplate: #1':'#2'.'#3;
    CatalogTemplate: #1':';
    SchemaTemplate: #2'.';
    ObjectTemplate: #3;
    RequiredParts: (False, False, False); // for informix format <catalog:schema.table>
    // call TObjectNameParser.EncodeObjectFullName( catalog='catalog', schema='', object='table')
    // returned: <catalog:table>. Informix format allows to not indicate a name of the scheme.
    // In classic version it would be impossible.
    OneDelimiter: False;
    NextTemplate: nil;
    fEmptyCatalogTemplate: '';
    fEmptySchemaTemplate: '';
    fEmptyObjectTemplate: '';
  );

//=================================================================
// TEXT(CSV) DATABASE OBJECT NAME FORMAT: "FileNameWithExtension"
//=================================================================

  TextMathesIndexes: array[0..5] of integer = (
    6, // array length
    0, 0, 4, // position info for diferent parts. For accessing usage indexes: idxCatalog, idxSchema, idxObject
    2, 1     // array of object  name matches indexes
    );

  TextObjectNameTemplateInfo: TObjectNameTemplateInfo = (
    sName: 'TextObjectNameTemplateInfo';
    sRegExpr: '^("(.*)"|.*)$';
  //           |           |
  //            \         /
  //              --- ---
  //                 Y
  //                 |
  //               object = file name with extension
  //
    QuoteTemplate: '"'; // Quote can be redefined at runtime ...
    RegexpMathesIndexes: @TextMathesIndexes; // result position of catalog, schema, table
    FullNameTemplate: '"'#3'"';
    CatalogTemplate: '';
    SchemaTemplate: '';
    ObjectTemplate: '"'#3'"';
    RequiredParts: (False, False, True);
    OneDelimiter: True;
    NextTemplate: nil;
    fEmptyCatalogTemplate: '';
    fEmptySchemaTemplate: '';
    fEmptyObjectTemplate: '';
  );

//==================================
// DBMS ARRAY ObjectNameTemplateInfo
//==================================

  DbmsObjectNameTemplateInfo: array[TDbmsType] of PObjectNameTemplateInfo = (
    nil, //eDbmsTypeUnspecified ( "nil" is equal to "@DefaultObjectNameTemplateInfo" )
    nil, //eDbmsTypeGupta
    @SQLServerObjectNameTemplateInfo, //eDbmsTypeMsSqlServer
    @SQLServerObjectNameTemplateInfo, //eDbmsTypeMsSqlServer2005Up
    nil, //eDbmsTypeIbmDb2
    nil, //eDbmsTypeIbmDb2AS400
    nil, //eDbmsTypeMySql
    nil, //eDbmsTypeMySqlMax
    nil, //eDbmsTypeMsAccess
    @TextObjectNameTemplateInfo, //eDbmsTypeExcel
    @TextObjectNameTemplateInfo, //eDbmsTypeText
    @TextObjectNameTemplateInfo, //eDbmsTypeDBase
    @TextObjectNameTemplateInfo, //eDbmsTypeParadox
    @OracleObjectNameTemplateInfo, //eDbmsTypeOracle
    nil, //eDbmsTypeInterbase
    @InformixObjectNameTemplateInfo, //eDbmsTypeInformix
    nil, //eDbmsTypeSybase
    nil, //eDbmsTypeSQLLite
    nil, //eDbmsTypeThinkSQL
    nil, //eDbmsTypeSapDb
    nil, //eDbmsTypePervasiveSQL
    nil, //eDbmsTypeFlashFiler
    nil, //eDbmsTypePostgreSQL
    nil, //eDbmsTypeInterSystemCache
    @TextObjectNameTemplateInfo, //eDbmsTypeFoxPro
    nil, //eDbmsTypeClipper
    nil, //eDbmsTypeBtrieve
    nil, //eDbmsTypeOpenIngres
    nil, //eDbmsTypeProgress
    nil //eDbmsTypeOterroRBase
  );

function GetDbmsObjectNameTemplateInfo(DbmsType: TDbmsType): PObjectNameTemplateInfo;

implementation

uses
{$IFDEF _D12UP_}
  AnsiStrings,
{$ENDIF}
  DbxOpenOdbcFuncs;

function GetDbmsObjectNameTemplateInfo(DbmsType: TDbmsType): PObjectNameTemplateInfo;
begin
  Result := DbmsObjectNameTemplateInfo[DbmsType];
  if Result = nil then
    Result := @DefaultObjectNameTemplateInfo;
end;

{ TObjectNameParser }

constructor TObjectNameParser.Create(
  AObjectNameTemplateInfo: PObjectNameTemplateInfo;
  const DbQuote, sRegExpNew: AnsiString
  );
var
  vRegexpQuote, vQuoteTemplate: AnsiString;
  iTemplate, i: integer;
  vObjectNameTemplateInfo: PObjectNameTemplateInfo;
  vTemplateList: TList;
  vRegExpr: TRegExpr;
const
  cRegExprSpesialSymbols: AnsiString = '[]\^.$*+?{},&()/|:=!';
begin
  if AObjectNameTemplateInfo = nil then
    AObjectNameTemplateInfo := @DefaultObjectNameTemplateInfo;
{$IFDEF _TRACE_CALLS_}vRegExpr := nil; LogEnterProc(ClassName + '.' + 'Create', 'DbQuote=<' + DbQuote + '>; Template: ' + AObjectNameTemplateInfo.sName);
  try try
{$ENDIF _TRACE_CALLS_}
  vTemplateList := TList.Create;
  try
    vObjectNameTemplateInfo := AObjectNameTemplateInfo;
    while Assigned(vObjectNameTemplateInfo) do
    begin
      vTemplateList.Add(vObjectNameTemplateInfo);
      vObjectNameTemplateInfo := vObjectNameTemplateInfo.NextTemplate;
    end;
    SetLength(fObjectNameTemplateInfo, vTemplateList.Count);
    SetLength(fRegExpr, vTemplateList.Count);

    for iTemplate := 0 to vTemplateList.Count - 1 do
    begin
      fObjectNameTemplateInfo[iTemplate] := PObjectNameTemplateInfo(vTemplateList[iTemplate])^;
      vObjectNameTemplateInfo := @fObjectNameTemplateInfo[iTemplate];
      if (iTemplate = 0) and (sRegExpNew <> '')  then
        vObjectNameTemplateInfo.sRegExpr := sRegExpNew;


      if // #0 identify template unsupported correction in runtime
        (vObjectNameTemplateInfo.QuoteTemplate <> #0) and
        (DbQuote <> #0) and
        (vObjectNameTemplateInfo.QuoteTemplate <> DbQuote) then
      begin

        vQuoteTemplate := vObjectNameTemplateInfo.QuoteTemplate;
        vObjectNameTemplateInfo.QuoteTemplate := DbQuote;

        if Length(vObjectNameTemplateInfo.QuoteTemplate) > 0 then
        begin
      // Replace RegExp spesial symbols
          vRegexpQuote := '';

          for i := 1 to Length(DbQuote) do
          begin
            if (AnsiPos(DbQuote[i], cRegExprSpesialSymbols) > 0) then
              vRegexpQuote := vRegexpQuote + '\' + DbQuote[i]
            else
              vRegexpQuote := vRegexpQuote + DbQuote[i];
          end;

          vObjectNameTemplateInfo.sRegExpr :=
            StringReplace(
            vObjectNameTemplateInfo.sRegExpr,
            vQuoteTemplate,
            vRegexpQuote,
            [rfReplaceAll, rfIgnoreCase]
            );
        end;

    // Updating of templates accordingly to new QuoteTemplate
        vObjectNameTemplateInfo.FullNameTemplate :=
          StringReplace(
          vObjectNameTemplateInfo.FullNameTemplate,
          vQuoteTemplate,
          DbQuote,
          [rfReplaceAll, rfIgnoreCase]
          );

        vObjectNameTemplateInfo.CatalogTemplate :=
          StringReplace(
          vObjectNameTemplateInfo.CatalogTemplate,
          vQuoteTemplate,
          DbQuote,
          [rfReplaceAll, rfIgnoreCase]
          );

        vObjectNameTemplateInfo.SchemaTemplate :=
          StringReplace(
          vObjectNameTemplateInfo.SchemaTemplate,
          vQuoteTemplate,
          DbQuote,
          [rfReplaceAll, rfIgnoreCase]
          );

        vObjectNameTemplateInfo.ObjectTemplate :=
          StringReplace(
          vObjectNameTemplateInfo.ObjectTemplate,
          vQuoteTemplate,
          DbQuote,
          [rfReplaceAll, rfIgnoreCase]
          );
      end;

      fRegExpr[iTemplate] := TRegExpr.Create;
      vRegExpr := fRegExpr[iTemplate];
      vRegExpr.Expression := vObjectNameTemplateInfo.sRegExpr;
      if (iTemplate = 0) and (sRegExpNew <> '')  then
      begin
        try
          vRegExpr.Exec('test');
        except
          on e: Exception do
          begin
            e.Message := 'RegExpr Expression Error !'
              + #13'Expression: ' + string(sRegExpNew)
              + #13'Detail: ' + e.Message;
            raise;
          end;
        end;
      end;

  // Empty parts of templates
    // catalog:
      vObjectNameTemplateInfo.fEmptyCatalogTemplate :=
        StringReplace(vObjectNameTemplateInfo.CatalogTemplate,
          AnsiChar(#1), AnsiString(''), [rfIgnoreCase]);
    // schema:
      vObjectNameTemplateInfo.fEmptySchemaTemplate :=
        StringReplace(vObjectNameTemplateInfo.SchemaTemplate,
          AnsiChar(#2), AnsiString(''), [rfIgnoreCase]);
    // object:
      vObjectNameTemplateInfo.fEmptyObjectTemplate :=
        StringReplace(vObjectNameTemplateInfo.ObjectTemplate,
          AnsiChar(#3), AnsiString(''), [rfIgnoreCase]);

    end; //of: for iTemplate
  finally
    vTemplateList.Free;
  end;
{$IFDEF _TRACE_CALLS_}
    except on e: exception do
      begin LogExceptProc(ClassName + '.' + 'Create', e);
        raise;
      end;
    end;
  finally if Assigned(vRegExpr) then LogExitProc(ClassName + '.' + 'Create. LasError=' + IntToStr(vRegExpr.LastError));
  end;
{$ENDIF _TRACE_CALLS_}
end;

destructor TObjectNameParser.Destroy;
var
  iTemplate: Integer;
begin
{$IFDEF _TRACE_CALLS_}LogEnterProc(ClassName + '.' + 'Destroy');
  try try
{$ENDIF _TRACE_CALLS_}
  for iTemplate := 0 to High(fRegExpr) do
  begin
    fRegExpr[iTemplate].Free;
    fRegExpr[iTemplate] := nil;
  end;
  inherited;
{$IFDEF _TRACE_CALLS_}
    except on e: exception do
      begin LogExceptProc(ClassName + '.' + 'Destroy', e);
        raise;
      end;
    end;
  finally LogExitProc(ClassName + '.' + 'Destroy');
  end;
{$ENDIF _TRACE_CALLS_}
end;

function TObjectNameParser.DecodeObjectFullName;//(
//  const ObjectFullName: AnsiString;
//  var sCatalogName, sSchemaName, sObjectName: AnsiString
//  ): PObjectNameTemplateInfo;
var
  iTemplate, i, ifRegExprHihg: integer;
  MathesIndexes: PArrayOfInteger;
  vRegExpr: TRegExpr;
  vObjectNameTemplateInfo: PObjectNameTemplateInfo;
  CatalogName, SchemaName, ObjectName: AnsiString;
  IsMathPos: Boolean;

  {$IFDEF _RegExpr_DumpMatchPos_}
  procedure DumpMatchPos;
  var
    i: Integer;
  begin
    with TStringList.Create do
    try
      Add('Template: ' + string(vObjectNameTemplateInfo.sName));
      Add('RegExpr: ' + string(vObjectNameTemplateInfo.sRegExpr));
      Add('');
      for i := 0 to NSUBEXP - 1 do
      begin
        Add(IntToStr(i) + ' '#9' ' + string(vRegExpr.Match[i]));
      end;
      SaveToFile(ExtractFilePath(ParamStr(0)) + 'debug-dump-match-pos.txt');
    finally
      Free;
    end;
  end;
  {$ENDIF}

begin
  { #02 }
  Result := nil;
  sCatalogName := '';
  sSchemaName := '';
  sObjectName := '';
{$IFDEF _TRACE_CALLS_}LogEnterProc(ClassName + '.' + 'DecodeObjectFullName', 'ObjectFullName=' + ObjectFullName);
  try try
{$ENDIF _TRACE_CALLS_}
  ifRegExprHihg := High(fRegExpr);
  for iTemplate := 0 to ifRegExprHihg do
  begin
    vRegExpr := fRegExpr[iTemplate];
  // Parse FullName:
      if vRegExpr.Exec(ObjectFullName) then
      begin
        vObjectNameTemplateInfo := @fObjectNameTemplateInfo[iTemplate];
        MathesIndexes := PArrayOfInteger(@vObjectNameTemplateInfo.RegexpMathesIndexes);
  // Build Results:

        { #01 }
        {$IFDEF _RegExpr_DumpMatchPos_}
        DumpMatchPos; // : debug for quick calculation MathesIndexes array (like DefaultMathesIndexes). For RegExprParser.zip
        {$ENDIF}

    // OBJECT
        i := MathesIndexes^[idxObject];
        ObjectName := '';
        IsMathPos := False;
        if i > 0 then
        begin
          for i := i to (MathesIndexes^[idxLength]) - 1 do
          begin
            if (MathesIndexes^[i] > 0) and
              (MathesIndexes^[i] <= vRegExpr.SubExprMatchCount) and
              (vRegExpr.MatchPos[MathesIndexes^[i]] > 0) then
            begin
              IsMathPos := True;
              ObjectName := vRegExpr.Match[MathesIndexes^[i]];
              Break;
            end;
          end;
        end;
        if {(ifRegExprHihg > 0) and} (not IsMathPos) then
          Continue; // go to next Template
    // CATALOG
        i := MathesIndexes^[idxCatalog];
        CatalogName := '';
        if i > 0 then
        begin
          for i := i to MathesIndexes^[idxSchema] - 1 do
          begin
            if (MathesIndexes^[i] > 0) and
              (MathesIndexes^[i] <= vRegExpr.SubExprMatchCount) and
              (vRegExpr.MatchPos[MathesIndexes^[i]] > 0) then
            begin
              CatalogName := vRegExpr.Match[MathesIndexes^[i]];
              Break;
            end;
          end;
        end;
    // SCHEMA
        i := MathesIndexes^[idxSchema];
        SchemaName := '';
        if i > 0 then
        begin
          for i := i to MathesIndexes^[idxObject] - 1 do
          begin
            if (MathesIndexes^[i] > 0) and
              (MathesIndexes^[i] <= vRegExpr.SubExprMatchCount) and
              (vRegExpr.MatchPos[MathesIndexes^[i]] > 0) then
            begin
              SchemaName := vRegExpr.Match[MathesIndexes^[i]];
              Break;
            end;
          end;
        end;
    // RESULT:
        Result := vObjectNameTemplateInfo;
        // make result:
        vObjectNameTemplateInfo := @fObjectNameTemplateInfo[0];
        if CatalogName <> vObjectNameTemplateInfo.fEmptyCatalogTemplate then
          sCatalogName := CatalogName;
        if SchemaName <> vObjectNameTemplateInfo.fEmptySchemaTemplate then
          sSchemaName := SchemaName;
        if ObjectFullName <> vObjectNameTemplateInfo.fEmptyObjectTemplate then
          sObjectName := ObjectName;

        if vObjectNameTemplateInfo.OneDelimiter then
        begin
          if (sObjectName = '') then
          begin
            sObjectName := sSchemaName;
            sSchemaName := '';
          end;
          if (sSchemaName = '') then
          begin
            sSchemaName := sCatalogName;
            sCatalogName := '';
          end;
        end;

        Exit;
      end; // of: if vRegExpr.Exec
  end; //of: for iTemplate
  vObjectNameTemplateInfo := @fObjectNameTemplateInfo[0];
  if ObjectFullName <> vObjectNameTemplateInfo.fEmptyObjectTemplate then
    sObjectName := ObjectFullName;
{$IFDEF _TRACE_CALLS_}
    except on e: exception do
      begin LogExceptProc(ClassName + '.' + 'DecodeObjectFullName', e);
        raise;
      end;
    end;
  finally LogExitProc(ClassName + '.' + 'DecodeObjectFullName', 'CatalogName, SchemaName, ObjectName=' + sCatalogName +
    ', ' + sSchemaName + ', ' + sObjectName);
  end;
{$ENDIF _TRACE_CALLS_}
end;

function TObjectNameParser.EncodeObjectFullName(const CatalogName,
  SchemaName, ObjectName: AnsiString;
  pTemplateInfo: PObjectNameTemplateInfo = nil
  ): AnsiString;
var
  S: AnsiString;
  i: integer;
  vObjectNameTemplateInfo: PObjectNameTemplateInfo;

  procedure LDoCalculateTemplate;
  //var
  //  vCatalogName, vSchemaName, vObjectName: string;
  begin
    vObjectNameTemplateInfo := @fObjectNameTemplateInfo[0];
    {
    if vObjectNameTemplateInfo.NextTemplate = nil then
      Exit;
    if ObjectName <> '' then
    begin
      vObjectNameTemplateInfo := DecodeObjectFullName(ObjectName, vCatalogName, vSchemaName, vObjectName);
      if (vObjectNameTemplateInfo <> nil) then
        Exit;
    end;
    if (SchemaName <> '') then
    begin
      vObjectNameTemplateInfo := DecodeObjectFullName(SchemaName, vCatalogName, vSchemaName, vObjectName);
      if (vObjectNameTemplateInfo <> nil) then
        Exit;
    end;
    if (CatalogName <> '') then
    begin
      vObjectNameTemplateInfo := DecodeObjectFullName(CatalogName, vCatalogName, vSchemaName, vObjectName);
      if (vObjectNameTemplateInfo <> nil) then
        Exit;
    end;
    vObjectNameTemplateInfo := @fObjectNameTemplateInfo[0];
    {}
  end;

begin
{$IFDEF _TRACE_CALLS_}LogEnterProc(ClassName + '.' + 'EncodeObjectFullName', 'CatalogName, SchemaName, ObjectName=' +
  CatalogName + ', ' + SchemaName + ', ' + ObjectName);
  try try
{$ENDIF _TRACE_CALLS_}
    if pTemplateInfo= nil then
      LDoCalculateTemplate
    else
      vObjectNameTemplateInfo := pTemplateInfo;

  // Aggregation of a parts name into a full name of dbms object according to a template

      Result := vObjectNameTemplateInfo.FullNameTemplate;

  // CATALOG:
      if Length(CatalogName) > 0 then
        S := StringReplace(vObjectNameTemplateInfo.CatalogTemplate, AnsiChar(#1), CatalogName, [rfIgnoreCase])
      else if vObjectNameTemplateInfo.RequiredParts[idxCatalog] then
        S := vObjectNameTemplateInfo.fEmptyCatalogTemplate
      else
        S := '';
      Result := StringReplace(Result, vObjectNameTemplateInfo.CatalogTemplate, S, [rfIgnoreCase]);

  // SCHEMA:
      if Length(SchemaName) > 0 then
        S := StringReplace(vObjectNameTemplateInfo.SchemaTemplate, AnsiChar(#2), SchemaName, [rfIgnoreCase])
      else if vObjectNameTemplateInfo.RequiredParts[idxSchema] then
        S := vObjectNameTemplateInfo.fEmptySchemaTemplate
      else
        S := '';
      Result := StringReplace(Result, vObjectNameTemplateInfo.SchemaTemplate, S, [rfIgnoreCase]);

  // OBJECT:
      if Length(ObjectName) > 0 then
        S := StringReplace(vObjectNameTemplateInfo.ObjectTemplate, AnsiChar(#3), ObjectName, [rfIgnoreCase])
      else if vObjectNameTemplateInfo.RequiredParts[idxObject] then
        S := vObjectNameTemplateInfo.fEmptyObjectTemplate
      else
        S := '';

  // Formation of a name on a template:
      Result := StringReplace(Result, vObjectNameTemplateInfo.ObjectTemplate, S, [rfIgnoreCase]);

  // Prepare result - remove "leading" empty parts:
      for i := 0 to 2 do
      begin
        if pos(vObjectNameTemplateInfo.fEmptyCatalogTemplate, Result) = 1 then
          Result := Copy(Result,
            Length(vObjectNameTemplateInfo.fEmptyCatalogTemplate) + 1,
            Length(Result) - Length(vObjectNameTemplateInfo.fEmptyCatalogTemplate)
            );
        if pos(vObjectNameTemplateInfo.fEmptySchemaTemplate, Result) = 1 then
          Result := Copy(Result,
            Length(vObjectNameTemplateInfo.fEmptySchemaTemplate) + 1,
            Length(Result) - Length(vObjectNameTemplateInfo.fEmptySchemaTemplate)
            );
        if pos(vObjectNameTemplateInfo.fEmptyObjectTemplate, Result) = 1 then
          Result := Copy(Result,
            Length(vObjectNameTemplateInfo.fEmptyObjectTemplate) + 1,
            Length(Result) - Length(vObjectNameTemplateInfo.fEmptyObjectTemplate)
            );
      end;
{$IFDEF _TRACE_CALLS_}
    except on e: exception do
      begin LogExceptProc(ClassName + '.' + 'EncodeObjectFullName', e);
        raise;
      end;
    end;
  finally LogExitProc(ClassName + '.' + 'EncodeObjectFullName', 'Result=' + Result);
  end;
{$ENDIF _TRACE_CALLS_}
end;

function TObjectNameParser.GetQuotedObjectName(const ObjectName: AnsiString): AnsiString;
var
  vCatalogName, vSchemaName, vObjectName: AnsiString;
  vObjectNameTemplateInfo: PObjectNameTemplateInfo;
begin
{$IFDEF _TRACE_CALLS_}LogEnterProc(ClassName + '.' + 'GetQuotedObjectName', 'ObjectName=' + ObjectName);
  try try{$ENDIF _TRACE_CALLS_}
  // Extract of parts name from full name
      vObjectNameTemplateInfo := DecodeObjectFullName(ObjectName, vCatalogName, vSchemaName, vObjectName);
  // Agregate of parts name into full dbms name ...
      Result := EncodeObjectFullName(vCatalogName, vSchemaName, vObjectName, vObjectNameTemplateInfo);
{$IFDEF _TRACE_CALLS_}
    except on e: exception do
      begin LogExceptProc(ClassName + '.' + 'GetQuotedObjectName', e);
        raise;
      end;
    end;
  finally LogExitProc(ClassName + '.' + 'GetQuotedObjectName', 'Result=' + Result);
  end;
{$ENDIF _TRACE_CALLS_}
end;

end.
