// THTMLParser v1.17 (w) 1996-2003 D.Spreen (dennis@spreendigital.de)
// This unit is freeware. Just drop me a line if this unit is useful for you.

// Updated by Angus Robertson, Magenta Systems Ltd, England, 18th November 2008
// delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
// Copyright Magenta Systems Ltd
//
// revision history
//   1.00-1.04 added several optimaizations
//   1.05 fixed a memory leak in THTMLParser.destroy
//   1.06 changed layout in prof.component
//   1.07 added ' as a valid parameter key bracket
//   1.08 fixed <!--> and //--> comments
//   1.09 added reverse entity support - HTMLText.ELine (on request by Eiríkur Haraldsson)
//   1.10 fixed quotes in comment tags
//   1.11 fixed javascript operators bug in comment tags with new "live comment detector"
//   1.12 fixed closing tag before quotes
//   1.13 fixed key values with spaces around the equal sign (done by Frédéric Leneuf-Magaud)
//        fixed problem with mixed quotes (done by Frédéric Leneuf-Magaud)
//   1.14 fixed quotes in comments, optimized entity support (thanks to Oleg Zarichniy)
//   1.15 fixed #255 line marker (thanks to Oleg Zarichniy)
//   1.16 fixed unix crlf #10
//   1.17 added Clear(), Feed(s: string) and FeedStop() - see sample.dpr
//   2.0 Angus added M+ TYPEINFO ON to stop D2007 warning
//
// You may find new versions of THTMLParser and other delphi components
// at http://www.spreendigital.de/delphi/

unit MagentaHtmlPars;

{$M+}

interface

uses
  Windows, Messages, SysUtils, Classes ;

type
  THTMLParam = class
  private
    fRaw: string;
    fKey: string;
    fValue: string;
    procedure SetKey(Key: string);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Key: string read fKey write SetKey;
    property Value: string read fValue;
    property Raw: string read fRaw;
  end;

type
  THTMLTag = class
  private
    fName: string;
    fRaw: string;
    procedure SetName(Name: string);
  public
    Params: TList;
    constructor Create;
    destructor Destroy; override;
  published
    property Name: string read fName write SetName; // uppercased TAG (without <>)
    property Raw: string read fRaw;
    // raw TAG (parameters included) as read from input file (without<>)
  end;

type
  THTMLText = class
  private
    fLine: string;
    fRawLine: string;
    procedure SetLine(Line: string);
    function GetLine: string; // v1.09
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Line: string read fLine write SetLine;
    // HTML3.2 Entities and Western Latin-1 Font converted Text
    property Raw: string read fRawLine; // raw text line as read from input file
    property ELine: string read GetLine; // v1.09
  end;

type
  THTMLParser = class(TObject)
  private
    Text: string;
    Tag: string;
    isTag: boolean;
    isComment: boolean; // v1.11
    FirstQuoteChar: char;
    isQuote: boolean;
    fClearOnExecute: boolean; // v1.17
    procedure AddText;
    procedure AddTag;
    procedure FeedStart; // v1.17
  public
    parsed: TList;
    Lines: TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    procedure Feed(s: string);      // v1.17
    procedure FeedStop;             // v1.17
    procedure Clear;                // v1.17
  published
    property ClearOnExecute: boolean read fClearOnExecute write fClearOnExecute; // v1.17
  end;

implementation



constructor THTMLParser.Create;
begin
  inherited Create;
  Lines := TStringList.Create;
  Parsed := TList.Create;
  FeedStart;
  fClearOnExecute := True;
end;


procedure THTMLParser.Clear;  // v1.17
var
  i: integer;
  obj: TObject;
begin
  if Parsed.Count = 0 then exit ;  // angus
  // memory leak fixed in v1.05
  for i := Parsed.Count downto 1 do
  begin
    obj := parsed[i - 1];
    if obj.classtype = THTMLTag then THTMLTag(obj).Free
    else if obj.classtype = THTMLText then THTMLText(obj).Free
    else
      obj.Free;
    Parsed.Delete(i - 1);
  end;
end;


destructor THTMLParser.Destroy;
begin
  Lines.Free;
  Clear;
  Parsed.Free;
  inherited Destroy;
end;



procedure THTMLParser.AddText;
var
  HTMLText: THTMLText;
begin
  if not isTag then
    if Text <> '' then
    begin
      HTMLText := THTMLText.Create;
      HTMLText.Line := Text;
      Text := '';
      parsed.Add(HTMLText);
    end;
end;


procedure THTMLParser.AddTag;
var
  HTMLTag: THTMLTag;
begin
  isTag := False;
  isComment := False;
  HTMLTag := THTMLTag.Create;
  HTMLTag.Name := Tag;
  Tag := '';
  parsed.Add(HTMLTag);
end;


procedure THTMLParser.FeedStart; // v1.17
begin
  Text := '';
  Tag := '';
  isTag := False;
  isQuote := False;  // v1.10
  FirstQuoteChar := ' '; // v1.10
  isComment := False; //v1.11
end;


procedure THTMLParser.Feed(s: string);  // v1.17
var
  i: integer;
begin
  for i := 1 to length(s) do
  begin
    if isTag then //v1.12
      if (s[i] = '"') or (s[i] = '''') then // v1.10
      begin
        if not isComment then // v1.14
          if not isQuote then
          begin
            isQuote := True;
            FirstQuoteChar := s[i];
          end
          else if s[i] = FirstQuoteChar then isQuote := False;
      end;

    if (s[i] = '<') and (not isQuote) then
    begin
      AddText;
      isTag := True;
    end //v1.12
    else if (s[i] = '>') and (not isQuote) and
      (isTag) and //v1.14
      ((not (isComment)) or           //v1.11
      ((isComment) and (length(Tag) > 1) and (copy(tag, length(tag) - 1,2) = '--'))
      //v1.11
      ) then AddTag
    else if isTag then
    begin
      Tag := Tag + s[i];
      if tag = '!--' then isComment := True; //v1.11
    end
    else
      Text := Text + s[i];

  end;
end;


procedure THTMLParser.FeedStop; // v1.17
begin
  if (isTag) and (Tag <> '') then AddTag;
  if (not isTag) and (Text <> '') then AddText;
  FeedStart;
end;



procedure THTMLParser.Execute; // changed v1.17
var
  i: integer;
begin
  if fClearOnExecute then Clear;
  FeedStart;
  for i := 1 to Lines.Count do
  begin
    if i <> 1 then
      if isTag then Tag := Tag + #$0d#$0a
      else
        Text := Text + #$0d#$0a;

    Feed(Lines[i - 1]);
  end;
  FeedStop;
end;




constructor THTMLTag.Create;
begin
  inherited Create;
  Params := TList.Create;
end;


destructor THTMLTag.Destroy;
var
  i: integer;
begin
  for i := Params.Count downto 1 do
  begin
    THTMLparam(Params[i - 1]).Free;
    Params.Delete(i - 1);
  end;
  Params.Free;
  inherited Destroy;
end;



procedure THTMLTag.SetName(Name: string);
var
  Tag: string;
  param: string;
  HTMLParam: THTMLParam;
  isQuote: char;
  hasSpace,        // v1.13
  hasEqual,        // v1.13
  isValue: boolean; // v1.13
  p: integer;
begin
  fRaw := Name;
  Params.Clear;

  //v1.10 - replace new lines with space
  while pos(#$0d#$0a, Name) <> 0 do
  begin
    p := pos(#$0d#$0a, Name);
    Delete(Name, p, 1);
    Name[p] := ' ';
  end;

{ //v1.08 change "<!--comment" to "<!-- comment"
 //uncomment if you want to "auto-correct" comments

  if (Length(Name)>3) and (copy(Name,1,3)='!--') then
   if (Name[4]<>' ') then insert(' ',Name,4) else else
  if (Length(Name)>2) and (copy(Name,1,2)='!-') then
   if (Name[3]<>' ') then insert(' ',Name,3);
}


  while (Length(Name) > 0) and (Name[1] <> ' ') do
  begin
    Tag := Tag + Name[1];
    Delete(Name, 1,1);
  end;

  fName := uppercase(Tag);

  while (Length(Name) > 0) do
  begin
    param := '';
    { // v1.13 - Beginning of changes }
    isQuote := #0;
    hasSpace := False;
    hasEqual := False;
    isValue := False;
    while (Length(Name) > 0) do
    begin
      if isQuote <> #0 then
      begin
        param := param + Name[1];
        if (Name[1] = isQuote) then
        begin
          Delete(Name, 1,1);
          break;
        end;
        Delete(Name, 1,1);
      end
      else
      begin
        if (Name[1] = ' ') then
        begin
          Delete(Name, 1,1);
          if isValue then
            break;
          hasSpace := True;
        end
        else
        begin
          if (Name[1] = '"') or (Name[1] = #$27) then
          begin
            if (not hasEqual) and hasSpace then
              break
            else
              IsQuote := Name[1];
          end
          else
          begin
            if Name[1] = '=' then
              hasEqual := True
            else if hasEqual then
              isValue := True
            else if hasSpace then
              break;
          end;
          param := param + Name[1];
          Delete(Name, 1,1);
        end;
      end;
    end;
    { // v1.13 - End of changes }

    // v1.13  if (Length(Name)>0) and (Name[1]=' ') then Delete(Name,1,1);
    if param <> '' then
    begin
      //v1.08 fix comment blocks
      if {(fName='!-') or }(fname = '!--') then
      begin
        if (param = '//--') then param := ''
        else {if (length(param)>0) and (param[length(param)]='-') then delete(param,length(param),1);
      if (length(param)>0) and (param[length(param)]='-') then delete(param,length(param),1);
       if you uncomment these then delete the line below!}
        if (length(param) > 1) and (copy(param, length(param) - 2,2) = '--') then
          Delete(param, length(param) - 1,2);
      end;

      if param <> '' then
      begin
        HTMLParam := THTMLParam.Create;
        HTMLParam.key := param;
        params.add(HTMLParam);
      end;
    end;
  end;
end;


{$i magentalatin1.pas}

procedure THTMLText.SetLine(Line: string);
var
  j, i: integer;
  isEntity: boolean;
  Entity: string;
  EnLen, EnPos: integer;
  p, d, c: integer;
begin
  fRawLine := Line;

  // v1.15 - delete all new lines at text begin
  while (pos(#$0d#$0a, Line) = 1) do
    delete(line,1,2);

  //v1.15 - replace all other new lines with space
  while pos(#$0d#$0a, Line) <> 0 do
  begin
    p := pos(#$0d#$0a, Line);
    Delete(Line, p, 1);
    line[p] := #32;
  end;

  //v1.16 - replace unix lf's with space
  for i := 1 to length(line) do
   if line[i]=#$0a then line[i] := #32;

  // delete all double spaces
  while pos('  ', Line) > 0 do Delete(Line, pos('  ', Line), 1);


  i := 1;
  isEntity := False;
  EnPos := 0;
  while (i <= Length(Line)) do
  begin
    if Line[i] = '&' then
    begin
      EnPos := i;
      isEntity := True;
      Entity := '';
    end;
    if (isEntity) and (Line[i] <> ' ') then Entity := Entity + Line[i]; // 1.14
    if isEntity then
      if (Line[i] = ';') or (Line[i] = ' ') or (i = Length(line)) then
      begin  // 1.14
        EnLen := Length(Entity);

        // charset encoded entity
        if (EnLen > 2) and (Entity[2] = '#') then
        begin
          Delete(Entity, EnLen, 1); //delete the ;
          Delete(Entity, 1,2); // delete the &#
          if uppercase(Entity[1]) = 'X' then Entity[1] := '$';
          // it's hex (but not supported!!!)
          if (Length(Entity) <= 3) then
          // we cant convert e.g. cyrillic/chinise capitals
          begin
            val(Entity, d, c);
            if c = 0 then // conversion successful
            begin
              Delete(Line, EnPos, EnLen);
              insert(Charset[d], Line, EnPos);
              i := EnPos; // set new start
            end;
          end;
        end
        else
        begin // its an entity


          // 1.14
          // correct HTML 4.0 Specification (W3C Recomendation 18-Dec-1997)
          // Character entity references part 5.3

          if (length(Entity) > 0) and
            (Entity[Length(Entity)] <> ';') then
            Entity := Entity + ';';

          // 1.14
          j := Low(Entities);
          while (j <= High(Entities)) do
          begin
            if Entity = (Entities[j, 1]) then
            begin
              Delete(Line, EnPos, EnLen);
              insert(Entities[j, 2], Line, Enpos);
              j := High(Entities) + 2; // stop searching
            end;
            j := j + 1;
          end;

          // reset Line, an reparse entity
          if j = High(Entities) + 3 then
            i := EnPos - 1
          else
            i := EnPos;
        end;

        IsEntity := False;
      end;
    i := i + 1;
  end;

  fLine := Line;
end;


function THTMLText.GetLine: string; // v1.09
var
  i, j, c, d: integer;
  Entity, adde: string;
begin
  //v1.09
  Result := '';
  for i := 1 to length(Line) do
  begin
    adde := Line[i];
    for j := 1 to 100 do
    begin
      Entity := entities[j, 2];
      Delete(entity, 1,2);
      val(entity, d, c);
      if line[i] = chr(d) then adde := Entities[j, 1]
    end;
    Result := Result + adde;
  end;
end;


procedure THTMLParam.SetKey(Key: string);
begin
  fValue := '';
  fRaw := Key;
  if pos('=', key) <> 0 then
  begin
    fValue := Key;
    Delete(fValue, 1,pos('=', key));
    key := copy(Key, 1,pos('=', key) - 1);

    if Length(fValue) > 1 then
      if ((fValue[1] = '"') and (fValue[Length(fValue)] = '"')) or
        ((fValue[1] = #$27) and (fValue[Length(fValue)] = #$27)) then
      begin
        Delete(fValue, 1,1);
        Delete(fValue, Length(fValue), 1);
      end;
  end;
  fKey := uppercase(key);
end;

constructor THTMLParam.Create;
begin
  inherited Create;
end;

destructor THTMLParam.Destroy;
begin
  inherited Destroy;
end;

constructor THTMLText.Create;
begin
  inherited Create;
end;

destructor THTMLText.Destroy;
begin
  inherited Destroy;
end;

end.
