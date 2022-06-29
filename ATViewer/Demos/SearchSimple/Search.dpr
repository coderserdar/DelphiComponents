//Simple demo, which can use ATStreamSEarch.
//Run it in console only (cmd.exe)!

{$apptype console}
uses
  Windows, SysUtils, ATStreamSearch, ATxSProc, ATxCodepages;

var
  FN, Text: string;
  S: TATStreamSearch;
  Count: Integer;
begin
  if ParamCount < 2 then
    begin
    Writeln('Usage: Search.exe <FileName> <Text>');
    Writeln;
    Writeln('Notes:');
    Writeln('  <Text> may contain zeroes (#0) in the form "\0".');
    Writeln('  Up to 5 search results are shown.');
    Exit
    end;

  FN := ParamStr(1);
  Text := ParamStr(2);
  Count := 0;
  SReplaceAll(Text, '\0', #0);
  Writeln(Format('Searching "%s" in file "%s":', [Text, FN]));

  S := TATStreamSearch.Create(nil);
  try
    S.FileName := FN;

    if S.FindFirst(Text, 0, vEncANSI, []) then
    repeat
      Writeln(Format('Result:  Pos: %d, Length: %d', [S.FoundStart, S.FoundLength]));
      if not S.FindNext then Break;
      Inc(Count);
      if Count >= 5 then Break;
    until False
    else
      Writeln('Search failed');

  finally
    S.FileName := '';
    S.Free;
  end;

  Writeln;
  Writeln('Press <Enter>');
  Readln;
end.
