unit ZRStrUtl;

interface

{$I ZRDefine.inc}

type
  TCharSet = Set of Char;

function MakeStr(C: Char; N: Integer): String;
function Space(N: Integer): String;
function Replicate(S: String; N: Integer): String;

function PadLeftChar(const S: String; N: Integer; C: Char): String;
function PadRightChar(const S: String; N: Integer; C: Char): String;
function PadCenterChar(const S: String; N: Integer; C: Char): String;

function PadLeft(const S: String; N: Integer): String;
function PadRight(const S: String; N: Integer): String;
function PadCenter(const S: String; N: Integer): String;

function WrapText(const Source: String; MaxCol: Integer): String;

function MatchMask(const Mask, Text: String): Boolean;

implementation

Uses Windows, SysUtils, Math;

function MakeStr(C: Char; N: Integer): String;
begin
  if N < 1 then
    Result := ''
  else begin
    SetLength(Result, N);
    FillChar(Result[1], Length(Result), C);
  end;
end;

function Space(N: Integer): String;
begin
  Result:= MakeStr(' ', N);
end;

function Replicate(S: String; N: Integer): String;
var
  i: Integer;
begin
  Result:= '';
  for i:= 1 to N do Result:= Result + S;
end;

function PadLeftChar(const S: String; N: Integer; C: Char): String;
begin
  if Length(S) < N then
    Result := MakeStr(C, N - Length(S)) + S
  else
    Result := S;
end;
function PadRightChar(const S: String; N: Integer; C: Char): String;
begin
  if Length(S) < N then
    Result := S + MakeStr(C, N - Length(S))
  else
    Result := S;
end;
function PadCenterChar(const S: String; N: Integer; C: Char): String;
begin
  if Length(S) < N then begin
    Result := MakeStr(C, (N - Length(S)) div 2) + S;
    Result := Result + MakeStr(C, N - Length(Result));
  end else
    Result := S;
end;

function PadLeft(const S: String; N: Integer): String;
begin
  Result:= PadLeftChar(S, N, ' ');
end;
function PadRight(const S: String; N: Integer): String;
begin
  Result:= PadRightChar(S, N, ' ');
end;
function PadCenter(const S: String; N: Integer): String;
begin
  Result:= PadCenterChar(S, N, ' ');
end;

function WrapText(const Source: String; MaxCol: Integer): String;
var
  SrcLen,
  SrcPos,
  Start,
  Finish: Integer;
begin
  if MaxCol <= 0 then
    Result := Source
  else begin
    SrcLen:= Length(Source);
    Result:= '';
    SrcPos:= 1;
    while SrcPos <= SrcLen do begin
      Start  := SrcPos;
      while (Start <= SrcLen) and (Source[Start] = ' ') do Inc(Start);
      Finish:= Min(Start + MaxCol - 1, SrcLen);
      if (Finish < SrcLen) and (Source[Finish+1] <> ' ') then
        while (Finish > Start) and (Source[Finish] <> ' ') do Dec(Finish);
      if (Finish = Start) then
        Finish:= Min(Start + MaxCol - 1, SrcLen)
      else
        while (Finish > Start) and (Source[Finish] = ' ') do Dec(Finish);
      Result:= Result + copy(Source, Start, Finish - Start + 1) + #13#10;
      SrcPos:= Finish + 1;
    end;
  end;
end;

function MatchMask(const Mask, Text: String): Boolean;
  
  function Match(Mask, Text: PChar; MaskLen, TextLen: Integer): Boolean;
  var
    pt, pm: PChar;
    lt, lm: Integer;
    m , t : Char;
  begin
    Result:= False;
    pm:= Mask;
    pt:= Text;
    lm:= MaskLen;
    lt:= TextLen;
    
    while (lt >= 0) do
      begin
        if (lt = 0) and (lm = 0) then
          begin
            Result:= True;
            Exit;
          end;
        if (lm = 0) then Exit;

        m:= pm^;
        Inc(pm);
        Dec(lm);
     
        if (m = '*') then
          while (lt >= 0) do
            begin
              Result:= Match(pm, pt, lm, lt);
              if Result then Exit;
              Inc(pt);
              Dec(lt);
            end
        else
          begin
            if (lt = 0) then Exit;
            t:= pt^;
            Inc(pt);
            Dec(lt);
            if (m = '?') then Continue;
            if (m <> t) then Exit;
          end;

      end;
  end;
  
begin
  Result:= Match(PChar(Mask),PChar(Text),Length(Mask),Length(Text));
end;

end.

