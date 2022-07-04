program OverbyteIcsRtlTest;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, OverbyteIcsLibrary;

var
    I,  Cnt : Integer;
    N : Integer;
    Start, Stop: Cardinal;
    S1, S2, S3, S4 : AnsiString;
begin
    S1 := '          abcdHjndmopGKHJNBdfggABCDhJNDMOPGKHJNBdfgg        ';
    S2 := '';
    S3 := 'ABCDhJNDMOPGKHJNBdfgg'#9;
    S4 := 'abcdHjndmopGKHJNBdfgg'#1;
    Randomize;
    Cnt := 0;
    while true do
    begin
      Inc(Cnt);
      N  := Random(MAXINT);
      if Odd(N) then N := N * -1;

      WriteLn('Loop # ' + IntToStr(Cnt));

      Start := GetTickCount;
      for I := 1 to 400000 do
        IntToStr(N);
      Stop := GetTickCount;
      WriteLn('400000x IntToStr     "' + IntToStr(N) + '": ' + IntToStr(Stop - Start));
      Sleep(1000);
      Start := GetTickCount;
      for I := 1 to 400000 do
          IcsIntToStrA(N);
      Stop := GetTickCount;
      WriteLn('400000x IcsIntToStrA "' + IcsIntToStrA(N) + '": ' + IntToStr(Stop - Start));
      Sleep(1000);
      WriteLn('');

      Start := GetTickCount;
      for I := 1 to 400000 do
        IntToHex(N, 8);
      Stop := GetTickCount;
      WriteLn('400000x IntToHex     "' + IntToHex(N, 8) + '": ' + IntToStr(Stop - Start));
      Sleep(1000);
      Start := GetTickCount;
      for I := 1 to 400000 do
        IcsIntToHexA(N, 8);
      Stop := GetTickCount;
      WriteLn('400000x IcsIntToHexA "' + IcsIntToHexA(N, 8) + '": ' + IntToStr(Stop - Start));
      Sleep(1000);
      WriteLn('');

      Start := GetTickCount;
      for I := 1 to 400000 do
        CompareText(S3, S4);
      Stop := GetTickCount;
      WriteLn('400000x  CompareText Result: ' + IntToStr(CompareText(S3, S4)) + ' : ' + IntToStr(Stop - Start));
      Sleep(1000);
      Start := GetTickCount;
      for I := 1 to 400000 do
        _CompareText(S3, S4);
      Stop := GetTickCount;
      WriteLn('400000x _CompareText Result: ' + IntToStr(_CompareText(S3, S4)) + ' : ' + IntToStr(Stop - Start));
      Sleep(1000);
      WriteLn('');

      Start := GetTickCount;
      for I := 1 to 400000 do
        S2 := Trim(S1);
      Stop := GetTickCount;
      WriteLn('400000x  Trim Result: "' + S2 + '" : ' + IntToStr(Stop - Start));
      Sleep(1000);
      Start := GetTickCount;
      for I := 1 to 400000 do
        S2 := _Trim(S1);
      Stop := GetTickCount;
      WriteLn('400000x _Trim Result: "' + S2 + '" : ' + IntToStr(Stop - Start));
      Sleep(1000);
      WriteLn('');

      Start := GetTickCount;
      for I := 1 to 400000 do
        S1 := UpperCase(S2);
      Stop := GetTickCount;
      WriteLn('400000x  UpperCase Result: "' + S1 + '" : ' + IntToStr(Stop - Start));
      Sleep(1000);
      Start := GetTickCount;
      for I := 1 to 400000 do
        S1 := _UpperCase(S2);
      Stop := GetTickCount;
      WriteLn('400000x _UpperCase Result: "' + S1 + '" : ' + IntToStr(Stop - Start));
      Sleep(1000);
      WriteLn('');

      Start := GetTickCount;
      for I := 1 to 400000 do
        S1 := LowerCase(S2);
      Stop := GetTickCount;
      WriteLn('400000x  LowerCase Result: "' + S1 + '" : ' + IntToStr(Stop - Start));
      Sleep(1000);
      Start := GetTickCount;
      for I := 1 to 400000 do
        S1 := _LowerCase(S2);
      Stop := GetTickCount;
      WriteLn('400000x _LowerCase Result: "' + S1 + '" : ' + IntToStr(Stop - Start));
      Sleep(1000);
      WriteLn('');

    end;
    
    WriteLn('End');
    ReadLn;
end.
