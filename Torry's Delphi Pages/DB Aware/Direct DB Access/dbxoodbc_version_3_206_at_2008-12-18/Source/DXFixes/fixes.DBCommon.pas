[...]
// QC: 6317
// added detected SQL line comments by two hyphen ("--"):
// it comment is supported in mssql, oracle, informix, ...
function NextSQLToken(var p: PChar; out Token: string; CurSection: TSQLToken): TSQLToken;
[...]
      '/':
      begin
        StartToken;
        Inc(p);
        if p^ in ['/','*'] then
        begin
          if p^ = '*' then
          begin
            repeat Inc(p) until (p = #0) or ((p^ = '*') and (p[1] = '/'));
          end else
            while not (p^ in [#0, #10, #13]) do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stComment;
          Exit;
        end;
      end;
      {+} // ADDED NEW:
      '-':
      begin
        StartToken;
        Inc(p);
        if (p^ ='-') and (Length(Token) = 0) then
        begin
          while not (p^ in [#0, #10, #13]) do Inc(p);
          SetString(Token, TokenStart, p - TokenStart);
          Result := stComment;
          Exit;
        end;
      end;
      {+.}
[...]
{+}
// QC: 6318.
// fixes: for IsMultiTableQuery:
// added check allowed words after from: ( where, union, ... ).
// added support "select from select"

// IsMultiTableQuery and GetTableNameFromSQL work by a similar principle but have a separate code.
// Now they have the common logic code.

// Now TClientDataSet can correctly establish indexes. It can make apply updates and use of unique
// indexes for updates.

function IsMultiTableQueryEx(const SQL: string; var TableName: string; DefaultResult: Boolean): Boolean;
const
  SInnerJoin = 'inner join ';       { do not localize }
  SOuterJoin = 'outer join ';       { do not localize }
var
  Start{+}, StartTmp{+.}: PChar;
  SResult, Token: string;
  SQLToken, CurSection{+}, SQLTokenTmp{+.}: TSQLToken;
begin
  SResult := '';
  Start := PChar(SQL);
  CurSection := stUnknown;
  Result := DefaultResult;
  repeat
    SQLToken := NextSQLToken(Start, Token, CurSection);
    if SQLToken in SQLSections then CurSection := SQLToken;
  until SQLToken in [stEnd, stFrom];
  if SQLToken = stFrom then
  begin
    repeat
      SQLToken := NextSQLToken(Start, Token, CurSection);
      {+}
      if SQLToken = stSelect then
      begin // select from select:
        Result := IsMultiTableQueryEx( Start, TableName, Result );
        exit;
      end
      else
      {+.}
      if SQLToken in SQLSections then
        CurSection := SQLToken else
      // stValue is returned if TableNames contain quote chars.
      if (SQLToken = stTableName) or (SQLToken = stValue) then
      begin
        SResult := Token;
        while (Start[0] = '.') and not (SQLToken in [stEnd]) do
        begin
          SQLToken := NextSqlToken(Start, Token, CurSection);
          SResult := SResult + '.' + Token;
        end;
        if (Start[0] = ',') or (Start[1] = ',') then
        begin
          Result := False;
          exit;
        end;
        {+}
        TableName := SResult;
        StartTmp := Start;
        SQLTokenTmp := NextSqlToken(Start, Token, CurSection);
        if SQLTokenTmp = stTableName then
        begin
          // finding delimiter ',' for support "select from select"
          // example:
          // select * from (select * from table1) t
          while not (StartTmp^ in [',', #0 ]) do
            inc(StartTmp);
          if StartTmp^=',' then
          begin
            Result := True;
            exit;
          end
          else
          if StartTmp^=#0 then
          begin
            Result := False;
            exit;
            //SQLTokenTmp := stEnd;
            //Start := StartTmp;
          end;
        end;
        {+.}
        if Assigned(AnsiStrPos(Start, PChar(SInnerJoin))) or
           Assigned(AnsiStrPos(Start, PChar(SOuterJoin))) then
        begin
          Result := False;
          Exit;
        end;
        {+}
        if not ( SQLTokenTmp in [stWhere, stGroupBy, stHaving, stPlan, stOrderBy, stForUpdate,
          stEnd] ) then
        begin
          SQLToken := NextSqlToken(Start, Token, CurSection);
          if SQLToken = stTableName then
          begin
            Result := True;
            Exit;
          end;
        end;
        // scan next select for multitables
        //  (example: select * from table1 UNION select * from table2). "UNION" is multiselect,
        // since unique indexes can be duplicated.
        if (SQLTokenTmp <> stEnd) then
        begin
          SResult := #1;
          Result := IsMultiTableQueryEx( Start, SResult, False ) or (SResult<>#1);
          //if (not Result) and (SResult<>TableName) then
          //  Result := True;
        end
        else
          Result := False;
        {+.}
        Exit;
      end;
    until (CurSection <> stFrom) or (SQLToken in [stEnd, stTableName]);
  end;
  {+}
  Result := True; // default result
  {+.}
end;

function GetTableNameFromSQL(const SQL: string): string;
begin
  if IsMultiTableQueryEx(SQL, Result, True) then
    Result := '';
end;

function IsMultiTableQuery(const SQL: string): Boolean;
var
  TableName: string;
begin
  TableName := '';
  Result := IsMultiTableQueryEx(SQL, TableName, True);
end;
{+.}
