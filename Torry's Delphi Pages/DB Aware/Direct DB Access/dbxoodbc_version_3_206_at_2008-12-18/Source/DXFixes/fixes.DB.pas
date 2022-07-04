[...]
// QC: 6319.
// fixes: allow only param name classic name. Also allow problem informix fullname parsing.
function TParams.ParseSQL(SQL: String; DoCreate: Boolean): String;
const
  Literals = ['''', '"', '`'];
var
  Value, CurPos, StartPos: PChar;
  CurChar: Char;
  Literal: Boolean;
  EmbeddedLiteral: Boolean;
  Name: string;

  function NameDelimiter: Boolean;
  begin
    {+}
    //OLD:
    //Result := CurChar in [' ', ',', ';', ')', #13, #10];
    //NEW:
    Result := CurChar in [' ', ',', ';', ')', #13, #10
        ,#1..#9, '-', '+', '*', '/', '\', '!', '~', '''', '"', '&',
        '%', '`', '|', ']', '[', '>', '<', '=', '?',  '(', '^'
    ];
    {+.}
  end;

  function IsLiteral: Boolean;
  begin
    Result := CurChar in Literals;
  end;

  function StripLiterals(Buffer: PChar): string;
  var
    Len: Word;
    TempBuf: PChar;

    procedure StripChar;
    begin
      if TempBuf^ in Literals then
        StrMove(TempBuf, TempBuf + 1, Len - 1);
      if TempBuf[StrLen(TempBuf) - 1] in Literals then
        TempBuf[StrLen(TempBuf) - 1] := #0;
    end;

  begin
    Len := StrLen(Buffer) + 1;
    TempBuf := AllocMem(Len);
    try
      StrCopy(TempBuf, Buffer);
      StripChar;
      Result := StrPas(TempBuf);
    finally
      FreeMem(TempBuf, Len);
    end;
  end;

  {+} // ADDED NEW:
  function isDigit:Boolean;
   var LCurPos:PChar;
  begin
      Result := (CurPos-1)^ in ['0'..'9'];
      if Result and (CurPos-1>Value) then begin
          LCurPos := CurPos-1;
          while LCurPos>=Value do begin
              dec(LCurPos);
              if not (LCurPos^ in [#9, #10, #13, ' ', '0'..'9'])
              then begin
                  Result := False;
                  exit;
              end;
          end;
      end;
  end;
  {+.}

begin
  Result := SQL;
  Value := PChar(Result);
  if DoCreate then Clear;
  CurPos := Value;
  Literal := False;
  EmbeddedLiteral := False;
  repeat
    while (CurPos^ in LeadBytes) do Inc(CurPos, 2);
    CurChar := CurPos^;
    {+}
    //OLD:
    //if (CurChar = ':') and not Literal and ((CurPos + 1)^ <> ':') then
    //NEW:
    if (CurChar = ':') and not Literal and ( not ((CurPos + 1)^ in [':',#0,#9,#10,#13,#32]) )
       and( Value <> CurPos )
       and( isDigit or (not ( UpCase((CurPos - 1)^) in ['A'..'Z','_','0'..'9']  ))  ) then
    {+.}
    begin
      StartPos := CurPos;
      while (CurChar <> #0) and (Literal or not NameDelimiter) do
      begin
        Inc(CurPos);
        while (CurPos^ in LeadBytes) do Inc(CurPos, 2);
        CurChar := CurPos^;
        if IsLiteral then
        begin
          Literal := Literal xor True;
          if CurPos = StartPos + 1 then EmbeddedLiteral := True;
        end;
      end;
      CurPos^ := #0;
      if EmbeddedLiteral then
      begin
        Name := StripLiterals(StartPos + 1);
        EmbeddedLiteral := False;
      end
      else Name := StrPas(StartPos + 1);
      if DoCreate then
        TParam(Add).Name := Name;
      CurPos^ := CurChar;
      StartPos^ := '?';
      Inc(StartPos);
      StrMove(StartPos, CurPos, StrLen(CurPos) + 1);
      CurPos := StartPos;
    end
    else if (CurChar = ':') and not Literal and ((CurPos + 1)^ = ':') then
      StrMove(CurPos, CurPos + 1, StrLen(CurPos) + 1)
    else if IsLiteral then Literal := Literal xor True;
    Inc(CurPos);
  until CurChar = #0;
end;
