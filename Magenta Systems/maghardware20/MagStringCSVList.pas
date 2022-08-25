unit MagStringCSVList;

// 8th Sept 2000 - baseline Magenta Systems

// reads comma text but not treating spaces as a field delimiter
// reads text with a specific delimiter (ie pipe, :, tab etc)
// writes comma text always with delimiters
// LoadFromFieldStream and SaveToFieldStream using FS (hex1C) instead of CRLF
// Aug 2000 - added FieldsText property (using FS).
// 9 Oct 2000 - removed FORMS
// 3 July 2002 - ignore spaces between fields
// 4 Sept 2006 - added DelimitedText for compatibility with TstringList D6 and later
// 13 June 2014 - saved as MagStingCSVList so we can find it again
// 18 Sept 2019 - added function DelimTextToLines


interface

uses
  Windows, Messages, SysUtils, Classes ;

type
  TStringCSVList = class(TStringList)
  private
    { Private declarations }
    function GetCSVText: string;
    procedure SetCSVText(const Value: string);

  protected
    { Protected declarations }
    function GetTextFields: string;
    procedure SetTextFields(const Value: string);
    procedure SetDelimitedText(const Value: string);
    function GetDelimitedText: string;
  public
    { Public declarations }
    procedure LoadFromFieldStream(Stream: TStream);
    procedure SaveToFieldStream(Stream: TStream);
    procedure SetDelimText(const Value: string; const delim: char);
  published
    { Published declarations }
     property CSVText: string read GetCSVText write SetCSVText;
     property FieldsText: string read GetTextFields write SetTextFields ;
     property DelimitedText: string read GetDelimitedText write SetDelimitedText;
  end;

function DelimTextToLines (const Value: string; const Sepch, Quotech: Char): String;


implementation

function TStringCSVList.GetTextFields: string;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S: string;
begin
  Count := GetCount;
  Size := 0;
  for I := 0 to Count - 1 do Inc(Size, Length(Get(I)) + 1);
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count - 1 do
  begin
    S := Get(I);
    L := Length(S);
    if L <> 0 then
    begin
      System.Move(Pointer(S)^, P^, L);
      Inc(P, L);
    end;
    P^ := #28;
    Inc(P);
  end;
end;

function TStringCSVList.GetCSVText: string;
var
  S: string;
  I, Count: Integer;
begin
  Count := GetCount;
  if (Count = 1) and (Get(0) = '') then
    Result := '""'
  else
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      S := AnsiQuotedStr(Get(I), '"');
      Result := Result + S + ',';
    end;
    System.Delete(Result, Length(Result), 1);
  end;
end;

procedure TStringCSVList.LoadFromFieldStream(Stream: TStream);
var
  Size: Integer;
  S: string;
begin
  BeginUpdate;
  try
    Size := Stream.Size - Stream.Position;
    SetString(S, nil, Size);
    Stream.Read(Pointer(S)^, Size);
    SetTextFields(S);
  finally
    EndUpdate;
  end;
end;


procedure TStringCSVList.SetCSVText(const Value: string);
begin
  SetDelimText (value, ',') ;
end;

procedure TStringCSVList.SetDelimText(const Value: string; const delim: char);
var
  P, P1: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := PChar(Value);
    while (P^ in [#1..#31]) and (P^ <> delim) do P := CharNext(P);
    while P^ <> #0 do
    begin
        if P^ = '"' then
            S := AnsiExtractQuotedStr (P, '"')
        else
        begin
            P1 := P;
            while (P^ >= ' ') and (P^ <> delim) do P := CharNext(P);
            SetString (S, P1, P - P1) ;
        end;
        Add (S);
         // 3 July 2002 - changed #31 to #32 (space)
        while (P^ in [#1..#31]) and (P^ <> delim) do P := CharNext(P);
        if P^ = delim then
        repeat
            P := CharNext(P);
        until not ((P^ in [#1..#31]) and (P^ <> delim)) ;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TStringCSVList.SaveToFieldStream(Stream: TStream);
var
  S: string;
begin
  S := GetTextFields;
  Stream.WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TStringCSVList.SetTextFields(const Value: string);
var
  P, Start: PChar;
  S: string;
begin
  BeginUpdate;
  try
    Clear;
    P := Pointer(Value);
    if P <> nil then
      while P^ <> #0 do
      begin
        Start := P;
        while not (P^ in [#0, #10, #13, #28]) do Inc(P);
        SetString(S, Start, P - Start);
        Add(S);
        if P^ = #28 then Inc(P);
        if P^ = #13 then Inc(P);
        if P^ = #10 then Inc(P);
      end;
  finally
    EndUpdate;
  end;
end;

procedure TStringCSVList.SetDelimitedText(const Value : string);
begin
        SetDelimText(Value, Delimiter);
end;

function TStringCSVList.GetDelimitedText : string;
begin
    result := inherited DelimitedText;
end;

// Sept 2019 convert delimited text to string with CRLFs

function DelimTextToLines (const Value: string; const Sepch, Quotech: Char): String;
var
  P, P1: PChar;
  S: string;
begin
    Result := '';
    P := PChar(Value);
    while (P^ in [#1..#31]) and (P^ <> Sepch) do P := CharNext(P);
    while P^ <> #0 do
    begin
        if P^ = Quotech then  // quoted fields, including double quotes
            S := AnsiExtractQuotedStr (P, Quotech)
        else
        begin
            P1 := P;
            while (P^ >= ' ') and (P^ <> Sepch) do P := CharNext(P);
            SetString (S, P1, P - P1) ;
        end;
        Result := Result + S + #13#10 ;
        while (P^ in [#1..#31]) and (P^ <> Sepch) do P := CharNext(P);
        if P^ = Sepch then
        repeat
            P := CharNext(P);
        until not ((P^ in [#1..#31]) and (P^ <> Sepch)) ;
    end;
end;

end.
