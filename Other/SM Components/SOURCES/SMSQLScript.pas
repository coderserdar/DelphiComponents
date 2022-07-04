{ Copyright (C) 1998-2004, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
  WEB: http://www.scalabium.com

  This components allow to parse the SQL-script on parts (SQL-statement).

  TODO:
  - to skip comments (/* */ and all after ';' up to end of line)
  - to skip a Term within string values
}
unit SMSQLScript;

interface

uses Classes;

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

const
  DefaultTermChar = ';';

type
  TSMSQLScript = class(TComponent)
  private
    FSQL: TStrings;
    FSeparators: TList;
    FTerm: Char;

    FParsed: Boolean;

    function GetSQL: TStrings;
    procedure SetSQL(Value: TStrings);
  protected
    procedure ParseSQL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetStatementCount: Integer;
    function GetStatement(Index: Integer): string;
  published
    property SQL: TStrings read GetSQL write SetSQL;
    property Term: Char read FTerm write FTerm default DefaultTermChar;
  end;

procedure Register;

implementation

uses SysUtils;

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMSQLScript]);
end;

constructor TSMSQLScript.Create(AOwner: TComponent);
begin
  inherited;

  FSQL := TStringList.Create;
  FSeparators := TList.Create;

  FTerm := DefaultTermChar;
end;

destructor TSMSQLScript.Destroy;
begin
  FSeparators.Free;
  FSQL.Free;

  inherited;
end;

function TSMSQLScript.GetSQL: TStrings;
begin
  Result := FSQL;
end;

procedure TSMSQLScript.SetSQL(Value: TStrings);
begin
  FSQL.BeginUpdate;
  try
    FSQL.Assign(Value);

    FSeparators.Clear;
    FParsed := False;
  finally
    FSQL.EndUpdate;
  end;
end;

function TSMSQLScript.GetStatementCount: Integer;
begin
  if not FParsed then
    ParseSQL;

  Result := FSeparators.Count
end;

function TSMSQLScript.GetStatement(Index: Integer): string;

  function RemoveCRLF(const Value: string): string;
  var
    intStart, intEnd, intLen: Integer;
  begin
    Result := '';

    {remove chars in begining of string}
    intStart := 1;
    intLen := Length(Value);
    while (intStart < intLen) and
          {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Value[intStart] {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}[#13, #10, ' ']) do
      Inc(intStart);

    {remove chars in end of string}
    intEnd := intLen;
    while (intEnd > 0) and
          {$IFDEF SMForDelphi2009}CharInSet{$ENDIF}(Value[intEnd] {$IFDEF SMForDelphi2009},{$ELSE} in {$ENDIF}[#13, #10, ' ']) do
      Dec(intEnd);
    Result := Copy(Value, intStart, intEnd-intStart+1);
  end;

var
  intStart, intEnd: Integer;
begin
  if not FParsed then
    ParseSQL;

  Result := '';
  if (Index > -1) and (Index < FSeparators.Count) then
  begin
    intStart := LongInt(FSeparators[Index]);
    if (Index+1 < FSeparators.Count) then
      intEnd := LongInt(FSeparators[Index+1])
    else
      intEnd := Length(SQL.Text);
    Result := Copy(SQL.Text, intStart, intEnd-intStart);

    Result := RemoveCRLF(Result);
  end;
end;

procedure TSMSQLScript.ParseSQL;
var
  s: string;
  i: Integer;
begin
  FSeparators.Clear;

  s := SQL.Text;
  if (s <> '') then
  begin
    FSeparators.Add(TObject(1));

    for i := 1 to Length(s) do
    begin
      if (s[i] = Term) then
        FSeparators.Add(TObject(i+1));
    end;
  end;
  FParsed := True
end;

end.