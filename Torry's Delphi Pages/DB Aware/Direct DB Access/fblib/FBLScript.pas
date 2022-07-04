{
   Firebird Library
   Open Source Library No Data Aware for direct access to Firebird
   Relational Database from Borland Delphi / Kylix and Freepascal

   File:FBLScript.pas
   Copyright (c) 2002 Alessandro Batisti
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

{last modify 2005-11-25 alessandro batisti}

{$I fbl.inc}

{
@abstract(Simple script sql)
@author(Alessandro Batisti <fblib@altervista.org>)
FbLib - Firebird Library @html(<br>)
FBLScript.pas unit provides exctract simple sql script
}


unit FBLScript;

interface

uses
  {$IFDEF UNIX}
  Libc,
  {$ELSE}
  windows,
  {$ENDIF}
  Classes,
  SysUtils;

type
  TStatementType = (stUnknow, stSetTerm, stSetGenerator, stSelect, stInsert, stUpdate,
    stDelete, stDDL, stCommit, stRollback, stExecProc,
    stSec, stCreateDatabase);

  {@exclude}
  TStmInfo = class
  private
    FStmType:    TStatementType;
    FLineNumber: Integer;
  public
    constructor Create(AStmType:TStatementType; ALineNumber: Integer);
    property StmType:  TStatementType  read  FStmType write FStmType;
    property LineNumber: Integer read  FLineNumber write  FLineNumber;
  end;

  {@abstract(encapsulates the properties and methods for exctract single sql statement from sql script)}
  TFBLScript = class(TComponent)
  private
    FScript: TStrings;
    FTerm: string;
    FOpen,FEof: boolean;
    FStatementCount, FStatementIdx: Integer;
    FLineNumber, FCurrentLineNumber: Integer;
    FStatementType: TStatementType;
    FStatements: TStringList;
    FNewLineCount: Integer;
    function CStrNextChar(const AStr: PChar): PChar;
    function GetStatement: string;
    function GetTerminator: string;
    function ParseStatement(const AStm: string): TStatementType;
    function GetStatementCount: Integer;
    procedure SetScript(Value: TStrings);
    procedure Init;
    procedure Parse;
    procedure CheckTerminator;
    procedure ClearStatements;
    procedure OnScriptChange(Sender: TObject);
  public
    {Create an instance  of a TFBLScript}
    constructor Create(AOwner: TComponent); override;
    {Free up  all resources associated with this instance}
    destructor Destroy; override;
    {Reset pseudo cursor to begin of the script}
    procedure Reset;
    {Exctract single sql statement and position pseudo cursor on next sql statement
    @html(<br>) set @link(EOF) := @True  if cursor is at end of the script
    @longcode(#
    //Example
    // myScript  is a instance of TFBLScript
    // stm is a string value
    // set sql script
    myScript.SQLScript.Add('Insert into MyTable (nome) values (''pluto'');
    myScript.SQLScript.Add('Insert into MyTable (nome) values (''topolino'');
    //..
    stm := myScript.Statement;
    //stm is 'Insert into MyTable (nome) values (''pluto'')';
    //mySCript.StatementType is stInsert
    //myScript.EOF is False
    stm := myScript.Statement;
    //stm is 'Insert into MyTable (nome) values (''topolino'')';
    //mySCript.StatementType is stInsert
    //myScript.EOF is true - end  of script #)
    see also @link(EOF) @link(StatementType)
    }
    property  Statement: string read GetStatement ;
    {Return @True at the end of script}
    property EOF: Boolean read FEOF;
    {Return current Sql statement type see @link(TStatementType)
    @html(<br>) see also @link(Statement)}
    property StatementType: TStatementType read FStatementType;
    {Return script line number  where begin current statement}
    property CurrentLineNumber: Integer read FCurrentLineNumber;
    {Return number of statements parsed in script}
    property StatementCount: Integer read GetStatementCount;
  published
    {Sql Script source code}
    property SQLScript: TStrings read FScript write SetScript;
    {Current value of terminator default := ';'}
    property Terminator: string read GetTerminator;
  end;


implementation

uses
  FBLConst, FBLExcept;

constructor TStmInfo.Create(AStmType:TStatementType; ALineNumber: Integer);
begin
  FStmType := AStmType;
  FLineNumber := ALineNumber;
end;

//------------------------------------------------------------------------------

constructor TFBLScript.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FScript := TStringList.Create;
  FStatements := TStringList.Create;
  FOpen := False;
  FTerm := ';';
  FStatementIdx := 0;
  TStringList(FScript).OnChange :=  OnScriptChange;
end;

//------------------------------------------------------------------------------

destructor TFBLScript.Destroy;
begin
  TStringList(FSCript).OnChange := nil;
  FScript.Free;
  ClearStatements;
  FStatements.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TFBLScript.OnScriptChange(Sender: TObject);
begin
  if FOpen then FOpen := False ;
  if FEOF then FEOF := False;
end;

//------------------------------------------------------------------------------

function TFBLScript.CStrNextChar(const AStr: PChar): PChar;
{$IFDEF UNIX}
var
  CharLen: Integer;
{$ENDIF}
begin
  {$IFDEF UNIX}
  CharLen := Libc.mblen(AStr, MB_CUR_MAX);
  if (CharLen = -1) 
    then CharLen := 1;
  Result := AStr + CharLen;
  {$ELSE}
  Result := CharNext(AStr);
  {$ENDIF}
end;


//------------------------------------------------------------------------------

function TFBLScript.GetStatementCount: Integer;
begin
  if not FOpen then
    Init;
  Result := FStatementCount;
end;

//------------------------------------------------------------------------------

procedure  TFBLScript.ClearStatements;
var
  i: integer;
begin
  for i := 0 to  FStatements.Count -1 do
    if Assigned(TStmInfo(FStatements.Objects[i])) then
      TStmInfo(FStatements.Objects[i]).Free;
   FStatements.Clear;
end;

//------------------------------------------------------------------------------

procedure TFBLScript.SetScript(Value: TStrings);
begin
  FScript.Assign(Value);
end;

//------------------------------------------------------------------------------

function TFBLScript.GetTerminator;
begin
  Result := FTerm;
end;

//------------------------------------------------------------------------------

procedure TFBLScript.Reset;
begin
  FStatementIdx := 0;
  FEOF := False;
  FOpen := False;
end;

//------------------------------------------------------------------------------

procedure TFBLScript.CheckTerminator;
var
  TermLen: Integer;
begin
  TermLen := Length((FTerm));
  if (TermLen < 1) or (TermLen  > 2) then
     FBLError(E_SC_INVALID_LEN_SET_TERM);
  if TermLen = 1 then
     if FTerm[1] in ['0'..'9','a'..'z','A'..'Z'] then
        FBLError(E_SC_INVALID_CHAR_IN_SET_TERM,[FTerm]);
  if TermLen = 2 then
     if FTerm[2] in ['0'..'9','a'..'z','A'..'Z'] then
        FBLError(E_SC_INVALID_CHAR_IN_SET_TERM,[FTerm]);
end;

//------------------------------------------------------------------------------

procedure TFBLScript.Init;
begin
  ClearStatements;
  FLineNumber := 0;
  FTerm := ';';
  Parse;
  FEOF := FStatementCount = 0;
  FStatementIdx := 0;
  FOpen := True;
end;

//------------------------------------------------------------------------------

procedure TFBLScript.Parse;
var
  //lterm: Integer;
  PScript, PHead, PTail, PPrev: PChar;
  EOS, InQuote,InComment,Separator2Char: Boolean;
  QuoteChar: Char;
  Item: string;
  Separators: TSysCharSet;
  SChar1,SChar2 : Char;
  CurrentStmType : TStatementType;
begin
  FStatementCount := 0;
  PScript := PChar(FScript.Text);
  if (PScript = nil) or (PScript^=#0) then Exit;
  PTail := PScript;
  InQuote := False;
  InComment := False;
  QuoteChar := #0;
  SChar1 := #0;

  repeat
    Separator2Char := False;
    SChar2  := #0;
    Separators := [];
    if Length(FTerm) = 1 then
    begin
      SChar1 := FTerm[1];
      Separators := [SChar1];
    end
    else if Length(FTerm) = 2 then
    begin
      SChar1 := FTerm[1];
      SChar2 := FTerm[2];
      Separators := [SChar1,SChar2];
    end;

    //delete "comment" "new line" "space" at begin of script
    while (PTail^ in [#32, #13, #10, '/', '*']) or InComment do
    begin
      if PTail^ = #10 then  Inc(FLineNumber);

      if PTail^ in ['/','*'] then
      begin
        if PTail^  = '/'  then
        begin
          PPrev := PTail;
          PTail := CStrNextChar(PTail);
          if PTail^ = '*' then
          begin
            if not InComment then
              InComment := True;
            end
            else
              PTail := PPrev;
        end
        else
        begin
          PPrev := PTail;
          PTail := CStrNextChar(PTail);
          if PTail^ = '/' then
          begin
            if InComment then InComment := False;
          end
          else
            PTail := PPrev;
        end;
      end;
      PTail := CStrNextChar(PTail);
    end;
    // end delete "comment"

    PHead := PTail;

    while True do
    begin
      while (InQuote and not (PTail^ in [QuoteChar, #0])) or not (PTail^ in Separators + [#0, '''', '"']) do
      begin
        If  PTail^ = #10 then
          Inc(FLineNumber);
        PTail := CStrNextChar(PTail);
      end;

      if (PTail^= SChar1) and (SChar2 <> #0) then
      begin
        PPrev := PTail;
        PTail :=  CStrNextChar(PTail);
        if PTail^ = SChar2 then
        begin
          PTail := PPrev;
          Separator2Char := True;
          Break;
        end;
      end;

      if PTail^ in ['''', '"'] then
      begin
        if (QuoteChar <> #0) and (QuoteChar = PTail^) then
          QuoteChar := #0
        else if QuoteChar = #0 then
          QuoteChar := PTail^;
        InQuote := QuoteChar <> #0;
        PTail := CStrNextChar(PTail);
      end
      else
        Break;

    end;

    EOS := PTail^ = #0;

    if (PHead <> PTail) and (PHead^ <> #0) then
    begin
      SetString(Item, PHead, PTail - PHead);
      CurrentStmType :=  ParseStatement(Item);
      FStatements.AddObject(Item,TStmInfo.Create(CurrentStmType,
        ((FLineNumber + 1) - FNewLineCount)));
      Inc(FStatementCount);
    end;
    PTail := CStrNextChar(PTail);
    if Separator2Char then PTail := CStrNextChar(PTail);
  until EOS;
end;

//------------------------------------------------------------------------------

function TFBLScript.GetStatement: string;
begin
  Result := '';
  if not FOpen then Init;
  if FStatementIdx <  FStatementCount then
  begin
    Result := FStatements.Strings[FStatementIdx];
    FStatementType :=  TStmInfo(FStatements.Objects[FStatementIdx]).StmType;
    FCurrentLineNumber := TStmInfo(FStatements.Objects[FStatementIdx]).LineNumber;
    Inc(FStatementIdx);
  end;   
  if FStatementIdx = FStatementCount then FEOF := True;
end;

//------------------------------------------------------------------------------

function  TFBLScript.ParseStatement(const AStm: string): TStatementType ;
var
  Tokens :TStringList;
  PTail, PHead : PChar;
  EOS : Boolean;
  CurrentToken: string;
  TokenCount : Integer;
begin
  Tokens := TStringList.Create;
  PTail := PChar(AStm);
  Result :=  stUnknow;
  FNewLineCount := 0;
  try
    repeat
      while (PTail^ in [#9, #13, #10]) do
      begin
        if PTail^ = #10 then  Inc(FNewLineCount);
        PTail := CStrNextChar(PTail);
      end;

      PHead := PTail;

      while (PTail^ <> #0) and (not (PTail^ in [#32, #13, #10])) do
        PTail := CStrNextChar(PTail);

      if PTail^ = #10 then Inc(FNewLineCount);

      EOS := PTail^ = #0;

      if (PHead <> PTail) and (PHead^ <> #0) then
      begin
          SetString(CurrentToken, PHead, PTail - PHead);
          Tokens.Add(AnsiUpperCase(CurrentToken));
      end;
      PTail := CStrNextChar(PTail);

    until  EOS;
      
   TokenCount :=  Tokens.Count;

   if TokenCount > 0 then
    begin

      if Tokens.Strings[0] = 'SET' then
      begin
        if TokenCount > 1 then

          if Tokens.Strings[1] = 'TERM' then
          begin
            if TokenCount > 2 then
            begin
              FTerm := Tokens.Strings[2];
              CheckTerminator;
              Result := stSetTerm;
            end
            else
               FBLError(E_SC_INVALID_LEN_SET_TERM);
          end
          else if Tokens.Strings[1] = 'GENERATOR' then
              Result := stSetGenerator;
      end;

      if Tokens.Strings[0] = 'SELECT' then
        Result := stSelect;
      if Tokens.Strings[0] = 'UPDATE' then
        Result := stUpdate;
      if Tokens.Strings[0] = 'INSERT' then
        Result := stInsert;
      if Tokens.Strings[0] = 'DELETE' then
        Result := stDelete;
      if Tokens.Strings[0] = 'EXECUTE' then
        Result := stExecProc;
      if (Tokens.Strings[0] = 'CREATE') or (Tokens.Strings[0] = 'RECREATE') or
        (Tokens.Strings[0] = 'DECLARE') or
        (Tokens.Strings[0] = 'ALTER') or (Tokens.Strings[0] = 'DROP') then
        Result := stDDL;
      if Tokens.Strings[0] = 'COMMIT' then
        Result := stCommit;
      if Tokens.Strings[0] = 'ROLLBACK' then
        Result := stRollBack;
      if (Tokens.Strings[0] = 'GRANT') or (Tokens.Strings[0] = 'REVOKE') then
        Result := stSec;
      if (Tokens.Strings[0] = 'CREATE') then
        if TokenCount > 1 then
          if (Tokens.Strings[1] = 'DATABASE') then
            Result := stCreateDatabase;
    end;
  finally
    Tokens.Free;
  end;
end;

end.
