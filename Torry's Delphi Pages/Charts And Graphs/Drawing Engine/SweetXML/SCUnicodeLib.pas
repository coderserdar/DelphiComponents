{*******************************************************}
{                                                       }
{                  CA SweetXML Library                  }
{                                                       }
{  Copyright (c) 1996,2004 CodeAccelerate Corporation   }
{                                                       }
{*******************************************************}

unit SCUnicodeLib;

interface

uses
  Windows, Messages, SysUtils, Classes, SCUnicodeLibRes;

const
  UniWidePathSeparator = WideChar('\');
  UniWideNull = WideChar(#0);
  UniWideTab = WideChar(#9);
  UniSpace = WideChar(' ');
  UniWideSpace = WideChar(#32);
  UniWideExclaim = WideChar('!');
  UniWideQuestion = WideChar('?');
  UniWideCR = WideChar(#13);
  UniWideLF = WideChar(#10);
  UniLineFeed = UniWideLF;
  UniCarriageReturn = UniWideCR;
  UniLineSeparator = WideChar($2028);
  UniWideEscapeStart = WideChar('&');
  UniWideEscapeEnd = WideChar(';');
  UniWideQuatMark = WideChar('''');
  UniWideDoubleQuatMark = WideChar('"');
  UniWideEqual = WideChar('=');
  UniWideDot = WideChar('.');
  UniBOM = WideChar($FEFF);
  UniBigEndianBOM = WideChar($FFFE);
  Utf8BOM_1: WideChar = WideChar($BBEF);  // EF BB BF
  Utf8BOM_2: Char = Char($BF);
  UniWideCRLF: WideString = #13#10;
  UniWideLFCR: WideString = #10#13;
  UniParagraphSeparator = WideChar($2029);
  UniVerticalTab = WideChar($B);
  UniFormFeed = WideChar($C);

type
  PSCUniStringItem = ^TSCUniStringItem;
  TSCUniStringItem = record
    FString: WideString;
    FObject: TObject;
  end;

  TSCUniStringList = class(TPersistent)
  private
    FList: TList;
    FUpdateCount: Integer;
    FSorted: Boolean;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure SetSorted(Value: Boolean);
    procedure SetText(const Value: WideString);
    procedure InsertItem(Index: Integer; const S: WideString);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function  Get(Index: Integer): WideString; virtual;
    function  GetCount: Integer; virtual;
    function  GetObject(Index: Integer): TObject; virtual;
    procedure Put(Index: Integer; const S: WideString); virtual;
    procedure PutObject(Index: Integer; AObject: TObject); virtual;
    procedure SetUpdateState(Updating: Boolean); virtual;
    function  GetTextStr: WideString; virtual;
    function  GetText: WideString; virtual;
    procedure Error(Msg: string);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    function  Add(const S: WideString): Integer; virtual;
    function  AddObject(const S: WideString; AObject: TObject): Integer; virtual;
    procedure Append(const S: WideString);
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function  Find(const S: WideString; var Index: Integer; CaseSensitive: Boolean = True): Boolean; virtual;
    function  IndexOfObject(AObject: TObject): Integer;
    function  IndexOf(const S: WideString; CaseSensitive: Boolean = True): Integer; virtual;
    procedure InsertObject(Index: Integer; const S: WideString; AObject: TObject);
    procedure Insert(Index: Integer; const S: WideString); virtual;

    property Count: Integer read GetCount;
    property Objects[Index: Integer]: TObject read GetObject write PutObject;
    property Strings[Index: Integer]: WideString read Get write Put; default;
    property Text: WideString read GetText write SetText;
    property Sorted: Boolean read FSorted write SetSorted;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
  end;


function  SC_WideCharIsSpace(AChar: WideChar): boolean;
function  SC_WideCharIsControl(AChar: WideChar): boolean;
function  SC_WideTrimLeft(const S: WideString): WideString;
function  SC_WideTrimRight(const S: WideString): WideString;
function  SC_WideTrim(const S: WideString): WideString;
function  SC_WidePretyTrim(const S: WideString): WideString;
function  SC_WideInvertStr(const S: WideString): WideString;
procedure SC_WideSwapByteOrder(var S: WideString);
function  SC_WideTabToSpace(const WideLine: WideString; TabWidth: Integer): WideString;
function  SC_WideSpaceToTab(const WideLine: WideString; TabWidth: Integer): WideString;
function  SC_IsCharWhiteSpace(const Chr: WideChar): boolean;
function  SC_IsStrWhiteSpace(const Str: WideString): boolean;
function  SC_IsSameStr(const S1, S2: WideString): boolean;
function  SC_IsSameText(const S1, S2: WideString): boolean;
function  SC_GetUniMem(Size: Cardinal): PWideChar;
function  SC_CloneStr(const S: WideString): WideString;
function  SC_WideCompareStr(const S1, S2: WideString): Integer;
function  SC_WideStringReplace(const S, OldPattern, NewPattern: WideString;
  ReplaceAll: Boolean): WideString;
function  SC_WideScan(SubStr, S: WideString; After: LongInt): LongInt;
function  SC_WideStrUntil(SubStr, S: WideString; After: LongInt): WideString;
function  SC_UTF8ToWideString(const S: String): WideString;
function  SC_WideStringToUtf8(const S: WideString): String;
function  SC_UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal;
  Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
function  SC_Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal;
  Source: Pointer; SourceBytes: Cardinal): Cardinal; overload;

implementation

function SC_WideCharIsSpace(AChar: WideChar): boolean;
begin
  result := AChar in [UniWideSpace, UniWideTab];
end;

function SC_WideCharIsControl(AChar: WideChar): boolean;
begin
  result := AChar in [UniWideCR, UniWideLF];
end;

function SC_WideTrimLeft(const S: WideString): WideString;
var
  i, ln: LongInt;
begin
  result := S;

  ln := Length(S);
  if S = '' then exit;

  i := 1;
  while i <= ln do
  begin
    if not (SC_WideCharIsSpace(result[i]) or
      SC_WideCharIsControl(result[i])) then break;
    Inc(i);
  end;
  Delete(result, 1, i-1);
end;

function SC_WideTrimRight(const S: WideString): WideString;
var
  i, ln: LongInt;
begin
  result := S;

  ln := Length(S);
  if S = '' then exit;

  i := ln;
  while i > 0 do
  begin
    if not (SC_WideCharIsSpace(result[i]) or
      SC_WideCharIsControl(result[i])) then break;
    Dec(i);
  end;
  Delete(result, i+1, ln-i);
end;

function SC_WideTrim(const S: WideString): WideString;
begin
  result := SC_WideTrimRight(SC_WideTrimLeft(S));
end;

function  SC_WidePretyTrim(const S: WideString): WideString;
begin
  result := S;
  if SC_IsStrWhiteSpace(S) then result := SC_WideTrim(result);
end;

function SC_WideInvertStr(const S: WideString): WideString;
var
  i, ln: LongInt;
begin
  ln := Length(S);
  SetLength(result, ln);

  i := ln;
  while i > 0 do
  begin
    result[ln-i+1] := S[i];
    Dec(i);
  end;
end;

procedure SC_WideSwapByteOrder(var S: WideString);
var
  i, ln: LongInt;
  SW: Word;
begin
  ln := Length(S);
  i := 1;

  while i <= ln do
  begin
    SW := Word(S[i]);
    S[i] := WideChar(MakeWord(HiByte(SW), LoByte(SW)));
    Inc(i);
  end;
end;

function  SC_WideTabToSpace(const WideLine: WideString; TabWidth: Integer): WideString;
var
  Cnt, i, Ln, SpaceCnt: Integer;
  Space: WideString;
begin
  result := WideLine;
  Ln := Length(WideLine);

  if Ln = 0 then exit;

  if TabWidth = 0 then TabWidth := 1;

  i := 1;
  Cnt := 0;
  while i <= Ln do
  begin
    if result[i] in [UniWideCR, UniWideLF] then
    begin
      Cnt := 0;
    end else
      if result[i] = UniWideTab then
      begin
        SpaceCnt := TabWidth - (Cnt mod TabWidth);
        if SpaceCnt = 0 then SpaceCnt := 1;

        Delete(result, i, 1);
        Space := StringOfChar(' ', SpaceCnt);

        Insert(Space, result, i);

        Inc(Cnt,  SpaceCnt - 1);
        Inc(Ln, SpaceCnt - 1);
        Inc(i, SpaceCnt - 1);
      end;

    Inc(Cnt);
    Inc(i);
  end;
end;

function  SC_WideSpaceToTab(const WideLine: WideString; TabWidth: Integer): WideString;
var
  Cnt, i, Ln: Integer;
  Space: WideString;
begin
  result := WideLine;
  Ln := Length(WideLine);

  if (TabWidth = 0) or (Ln = 0) then exit;

  Space := StringOfChar(' ', TabWidth);

  i := 1;
  Cnt := 0;
  while i <= Ln do
  begin
    if result[i] = UniWideSpace then
    begin
      Inc(Cnt);

      if Cnt = TabWidth then
      begin
        Dec(i, TabWidth - 1);
        Dec(Ln, TabWidth - 1);

        Delete(result, i, TabWidth);
        Insert(UniWideTab, result, i-1);

        Cnt := 0;
      end;
    end;

    Inc(i);
  end;
end;

function  SC_IsCharWhiteSpace(const Chr: WideChar): boolean;
begin
  result := SC_WideCharIsSpace(Chr) or SC_WideCharIsControl(Chr);
end;

function  SC_IsStrWhiteSpace(const Str: WideString): boolean;
var
  I, Ln: LongInt;
begin
  Result := True;

  I := 1;
  Ln := Length(Str);
  while I <= Ln do
  begin
    if not SC_IsCharWhiteSpace(Str[I]) then
    begin
      Result := False;
      Exit;
    end;

    Inc(I);
  end;
end;

function SC_IsSameStr(const S1, S2: WideString): boolean;
var
  Ln, I: Integer;
begin
  Result := False;

  Ln := Length(S1);
  if Ln <> Length(S2) then
    Exit;

  for I := 1 to Ln do
    if S1[I] <> S2[I] then
      Exit;

  Result := True;
end;

function SC_IsSameText(const S1, S2: WideString): boolean;
begin
  Result := S1 = S2;
end;

function SC_GetUniMem(Size: Cardinal): PWideChar;
begin
  Size := SizeOf(WideChar) * Size + SizeOf(Cardinal);
  GetMem(result, Size);
  FillChar(result^, Size, 0);
  Cardinal(Pointer(result)^) := Size;
  Inc(result, SizeOf(Cardinal) div SizeOf(WideChar));
end;

function SC_CloneStr(const S: WideString): WideString;
var
  Ln: LongInt;
begin
  Ln := Length(S);

  SetLength(Result, Ln);
  if Ln = 0 then Exit;

  Result := SC_GetUniMem(Ln)^;
  System.Move(Pointer(S)^, Result, 2*Ln);
end;

function SC_WideCompareStr(const S1, S2: WideString): Integer;
var
  I, Ln1, Ln2, CheckTill: LongInt;
begin
  result := 0;

  Ln1 := Length(S1);
  Ln2 := Length(S2);

  if Ln1 = Ln2 then CheckTill := Ln1
  else if Ln1 < Ln2 then CheckTill := Ln1
  else CheckTill := Ln2;

  I := 1;
  while I <= CheckTill do
  begin
    result := Word(S1[I]) - Word(S2[I]);
    if result <> 0 then Break;
    Inc(I);
  end;

  if (result = 0) and (CheckTill > 0) then result := Ln1 - Ln2;
end;

function  SC_WideStringReplace(const S, OldPattern, NewPattern: WideString;
  ReplaceAll: Boolean): WideString;
var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
begin
  SearchStr := S;
  Patt := OldPattern;

  NewStr := S;
  Result := '';

  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;

    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);

    if not ReplaceAll then
    begin
      Result := Result + NewStr;
      Break;
    end;

    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

function SC_WideScan(SubStr, S: WideString; After: LongInt): LongInt;
var
  PC: PWideChar;
begin
  Result := 0;

  Inc(After);

  if (After > 0) and (After <= Length(S)) then
  begin
    PC := @S[After];
    Result := Pos(SubStr, PC);
    if Result > 0 then Inc(Result, After-1);
  end;
end;

function SC_WideStrUntil(SubStr, S: WideString; After: LongInt): WideString;
var
  Indx: LongInt;
begin
  SetLength(Result, 0);

  Indx := SC_WideScan(SubStr, S, After);
  if Indx > After then
  begin
    Inc(After);
    Result := Copy(S, After, Indx - After);
  end;
end;

function SC_UnicodeToUtf8(Dest: PChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := Char(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := Char($E0 or (c shr 12));
        Dest[count+1] := Char($80 or ((c shr 6) and $3F));
        Dest[count+2] := Char($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := Char($C0 or (c shr 6));
        Dest[count+1] := Char($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

function SC_Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal;
  Source: Pointer; SourceBytes: Cardinal): Cardinal; overload;
var
  c: Byte;
  S: PChar;
  i, count, wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;

  S := PChar(Source);

  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(S[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(S[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(S[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(S[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(S[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(S[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
end;

function SC_UTF8ToWideString(const S: String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := SC_Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1,
    Pointer(S), Length(S));
    
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

function SC_WideStringToUtf8(const S: WideString): String;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := SC_Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1,
    Pointer(S), Length(S));
    
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

{ TSCUniStringList }

function TSCUniStringList.Add(const S: WideString): Integer;
begin
  {if not Sorted then Result := Count
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: ; // Error(@SDuplicateString, 0);
      end;}
  Result := Count;    
  InsertItem(Result, S);
end;

function TSCUniStringList.AddObject(const S: WideString; AObject: TObject): Integer;
begin
  result := Add(S);
  if (AObject <> nil) and (result > -1) then
    PSCUniStringItem(FList[result])^.FObject := AObject;
end;

procedure TSCUniStringList.Append(const S: WideString);
begin
  Add(S);
end;

procedure TSCUniStringList.BeginUpdate;
begin
  if FUpdateCount = 0 then SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TSCUniStringList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then FOnChange(Self);
end;

procedure TSCUniStringList.Changing;
begin
  if (FUpdateCount = 0) and Assigned(FOnChanging) then FOnChanging(Self);
end;

procedure TSCUniStringList.Clear;
var
  ARecord: PSCUniStringItem;
  i: integer;
begin
  if Count = 0 then Exit;

  try
    Changing;

    for i := Count-1 downto 0 do
    begin
      ARecord := FList[i];
      Dispose(ARecord);
      FList.Delete(i);
    end;
  finally
    Changed;
  end;
end;

constructor TSCUniStringList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

procedure TSCUniStringList.Delete(Index: Integer);
var
  ARecord: PSCUniStringItem;
begin
  if (Index < 0) or (Index >= Count) then Error(Format(SCRes_UniListIndexError, [Index]));

  try
    Changing;
    ARecord := FList.Items[Index];
    Dispose(ARecord);
  finally
    FList.Delete(Index);
    Changed;
  end;
end;

destructor TSCUniStringList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TSCUniStringList.EndUpdate;
begin
  Dec(FUpdateCount);

  if FUpdateCount <= 0 then
  begin
    FUpdateCount := 0;
    SetUpdateState(False);
  end;
end;

procedure TSCUniStringList.Error(Msg: string);
begin
  raise Exception.Create(Msg);
end;

procedure TSCUniStringList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TSCUniStringList.Find(const S: WideString; var Index: Integer;
  CaseSensitive: Boolean): Boolean;
begin
  Index := IndexOf(S, CaseSensitive);
  result := Index > -1;
end;

function TSCUniStringList.Get(Index: Integer): WideString;
begin
  if (Index < 0) or (Index >= Count) then Error(Format(SCRes_UniListIndexError, [Index]));
  Result := PSCUniStringItem(FList[Index])^.FString;
end;

function TSCUniStringList.GetCount: Integer;
begin
  result := FList.Count;
end;

function TSCUniStringList.GetObject(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= Count) then Error(Format(SCRes_UniListIndexError, [Index]));
  Result := PSCUniStringItem(FList[Index])^.FObject;
end;

function TSCUniStringList.GetText: WideString;
begin
  Result := SC_CloneStr(GetTextStr);
end;

function TSCUniStringList.GetTextStr: WideString;
var
  I, L,
  Size, Count: Integer;
  P: PWideChar;
  S: WideString;
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + 2);
  SetLength(Result, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, 2*L);
      Inc(P, L);
    end;
    P^ := UniWideCR;
    Inc(P);
    P^ := UniWideLF;
    Inc(P);
  end;
end;

function TSCUniStringList.IndexOf(const S: WideString; CaseSensitive: Boolean): Integer;
var
  i: Integer;
  Matched: Boolean;
  ARecord: PSCUniStringItem;
begin
  result := -1;

  for i := 0 to Count-1 do
  begin
    ARecord := FList.Items[i];

    if CaseSensitive then
      Matched := SC_IsSameStr(S, ARecord^.FString)
    else
      Matched := SC_IsSameText(S, ARecord^.FString);

    if Matched then
    begin
      result := i;
      Exit;
    end;
  end;
end;

function TSCUniStringList.IndexOfObject(AObject: TObject): Integer;
var
  ARecord: PSCUniStringItem;
  i: Integer;
begin
  result := -1;

  for i := 0 to Count-1 do
  begin
    ARecord := FList.Items[i];

    if AObject = ARecord^.FObject then
    begin
      result := i;
      Exit;
    end;
  end;
end;

procedure TSCUniStringList.Insert(Index: Integer; const S: WideString);
begin
  if Sorted then Error(SCRes_UniSortedListError);
  if (Index < 0) or (Index > Count) then Error(Format(SCRes_UniListIndexError, [Index]));

  InsertItem(Index, S);
end;

procedure TSCUniStringList.InsertItem(Index: Integer; const S: WideString);
var
  ARecord: PSCUniStringItem;
begin
  Changing;
  New(ARecord);
  FList.Insert(Index, ARecord);

  with ARecord^ do
  begin
    Pointer(FString) := nil;
    FObject := nil;
    FString := S;
  end;
  Changed;
end;

procedure TSCUniStringList.InsertObject(Index: Integer; const S: WideString; AObject: TObject);
begin
  Insert(Index, S);
  PutObject(Index, AObject);
end;

procedure TSCUniStringList.Put(Index: Integer; const S: WideString);
begin
  if Sorted then Error(SCRes_UniSortedListError);
  if (Index < 0) or (Index >= Count) then Error(Format(SCRes_UniListIndexError, [Index]));
  Changing;
  PSCUniStringItem(FList[Index])^.FString := S;
  Changed;
end;

procedure TSCUniStringList.PutObject(Index: Integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= Count) then Error(Format(SCRes_UniListIndexError, [Index]));
  Changing;
  PSCUniStringItem(FList[Index])^.FObject := AObject;
  Changed;
end;

procedure TSCUniStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    if FSorted then ; // SortList;
  end;
end;

procedure TSCUniStringList.SetText(const Value: WideString);
var
  S: WideString;
  Head, Tail: PWideChar;
begin
  BeginUpdate;
  try
    Clear;
    Head := PWideChar(Value);

    while Head^ <> UniWideNull do
    begin
      Tail := Head;
      while not (Tail^ in [UniWideNull, UniLineFeed, UniCarriageReturn,
        UniVerticalTab, UniFormFeed]) and (Tail^ <> UniLineSeparator) and
        (Tail^ <> UniParagraphSeparator) do
        Inc(Tail);

      SetString(S, Head, Tail - Head);
      Add(S);

      Head := Tail;
      if Head^ <> UniWideNull then
      begin
        Inc(Head);
        if (Tail^ = UniCarriageReturn) and (Head^ = UniLineFeed) then
          Inc(Head);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TSCUniStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

end.
