{ ************************************************************************************** }
{ }
{ Tiny XML parser of OpenOffice (.OXT) dictionaries, for Delphi 2007+ }
{ Version 1.0.0 (2010-07-26) }
{ }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 }
{ (the "License"); you may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at http://www.mozilla.org/MPL/ }
{ }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT }
{ WARRANTY OF ANY KIND, either express or implied. See the License for the specific }
{ language governing rights and limitations under the License. }
{ }
{ Original file name: NHunXml.pas. }
{ }
{ Copyright 2010 (C) Alexander Halser (http://www.ec-software.com) }
{ }
{ ************************************************************************************** }

{
  This is a lightweight XML parser for the XML files found in OXT dictionary files.
  Neither is it very fast, nor does it support basic XML structures like
  Unicode attribute names or CDATA sections. This stuff, however, doesn't occur
  in OXT files anyway and the XML data to parse is just a few bytes.

  Although all current OXT dictionary files seem to be UTF-8 encoded, I have added
  codepage encoding, just in case.

  Alexander Halser, July 2010
}

unit NHunXml;

interface

uses
  Windows, Messages, Classes, SysUtils, StrUtils, Widestrings;

type
{$IFNDEF UNICODE}
  /// define RawByteString, as it does exist in Delphi 2009/2010
  // - to be used for byte storage into an AnsiString
  RawByteString = AnsiString;
  UnicodeString = WideString;
{$ENDIF}
  TOxtParser = class;

  TOxtNode = class
  private
    FReader: TOxtParser;
    FName, FValue, FAttr: AnsiString; // raw byte string
    FItems: TList;
    FParent: TOxtNode;
    FLevel: Integer;
    function GetName: WideString;
    function GetValue: WideString;
    function GetAttr: WideString;
    function Decode(const AValue: AnsiString): WideString;
  public
    constructor Create(AReader: TOxtParser);
    destructor Destroy; override;
    function ParentName: WideString;
    function NodeByName(const AName: WideString): TOxtNode;
    procedure NodesByName(const AName: WideString; var AList: TList);
    function Attrib(const AName: AnsiString): WideString;
    property Parent: TOxtNode read FParent;
    property Name: WideString read GetName;
    property Value: WideString read GetValue;
    property Attr: WideString read GetAttr;
  end;

  TOxtParser = class
  private
    FRoot: TOxtNode;
    FUTF8Encoded: boolean;
    FCodepage: Word;
    procedure ParseString(const Source: AnsiString);
  public
    constructor Create(const Source: AnsiString);
    destructor Destroy; override;

    procedure Clear;
    function GetFirst: TOxtNode;
    function GetNext(Node: TOxtNode): TOxtNode;
    function AsText: string;
    property Root: TOxtNode read FRoot write FRoot;
  end;

implementation

constructor TOxtNode.Create(AReader: TOxtParser);
begin
  inherited Create;

  FReader := AReader;
  FItems := TList.Create;
  FName := '';
  FValue := '';
  FAttr := '';
  FParent := nil;
  FLevel := -1;
end;

destructor TOxtNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    TOxtNode(FItems[i]).Free;
  FreeAndNil(FItems);

  inherited Destroy;
end;

function TOxtNode.ParentName: WideString;
begin
  Result := '';
  if self.Parent <> nil then
    Result := self.Parent.Name;
end;

function TOxtNode.NodeByName(const AName: WideString): TOxtNode;
var
  intItem: Integer;
begin
  Result := nil;
  for intItem := 0 to FItems.Count - 1 do
  begin
    if AName = TOxtNode(FItems[intItem]).Name then
      Result := TOxtNode(FItems[intItem])
    else
      Result := TOxtNode(FItems[intItem]).NodeByName(AName);
    if assigned(Result) then
      break;
  end;
end;

procedure TOxtNode.NodesByName(const AName: WideString; var AList: TList);
var
  intItem: Integer;
begin
  for intItem := 0 to FItems.Count - 1 do
  begin
    if AName = TOxtNode(FItems[intItem]).Name then
      AList.Add(FItems[intItem]);
    TOxtNode(FItems[intItem]).NodesByName(AName, AList);
  end;
end;

function TOxtNode.Decode(const AValue: AnsiString): WideString;

  function WideStringReplace(const S, OldPattern, NewPattern: WideString;
    Flags: TReplaceFlags): WideString;
  var
    SearchStr, Patt, NewStr: WideString;
    Offset: Integer;
  begin
    if rfIgnoreCase in Flags then
    begin
      SearchStr := WideUpperCase(S);
      Patt := WideUpperCase(OldPattern);
    end
    else
    begin
      SearchStr := S;
      Patt := OldPattern;
    end;
    NewStr := S;
    Result := '';
    while SearchStr <> '' do
    begin
      Offset := Pos(Patt, SearchStr);
      if Offset = 0 then
      begin
        Result := Result + NewStr;
        break;
      end;
      Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
      NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
      if not(rfReplaceAll in Flags) then
      begin
        Result := Result + NewStr;
        break;
      end;
      SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
    end;
  end;

const
  escapes: array [0 .. 4, 0 .. 1] of WideString = (('&', '&amp;'),
    ('<', '&lt;'), ('>', '&gt;'), ('''', '&apos;'), ('"', '&quot;'));
var
  l: Integer;
begin
  Result := '';
  if FReader.FUTF8Encoded then
    Result := UTF8Decode(AValue)
  else
  begin
    l := MultiByteToWideChar(FReader.FCodepage, 0, PAnsiChar(AValue), -1, Nil,
      0);
    if l > 1 then
    begin
      SetLength(Result, l - 1);
      MultiByteToWideChar(FReader.FCodepage, 0, PAnsiChar(AValue), -1,
        PWideChar(Result), l - 1);
    end;
  end;

  for l := 0 to 4 do
    Result := WideStringReplace(Result, escapes[l][1], escapes[l][0],
      [rfReplaceAll, rfIgnoreCase]);
end;

function TOxtNode.GetName: WideString;
begin
  Result := Decode(FName);
end;

function TOxtNode.GetValue: WideString;
begin
  Result := Decode(FValue);
end;

function TOxtNode.GetAttr: WideString;
begin
  Result := Decode(FAttr);
end;

function TOxtNode.Attrib(const AName: AnsiString): WideString;
var
  tmpStr: TStringlist;
begin
  tmpStr := TStringlist.Create;
  try
    tmpStr.Text := FAttr;                   { implcit conversion }
    Result := Decode(tmpStr.Values[AName]); { implcit conversion }
  finally
    FreeAndNil(tmpStr);
  end;
end;

{ ---- TOxtParser ---- }

constructor TOxtParser.Create(const Source: AnsiString);
begin
  Root := TOxtNode.Create(self);
  FCodepage := GetACP;
  FUTF8Encoded := true;
  ParseString(Source);
end;

destructor TOxtParser.Destroy;
begin
  Root.Free;
  inherited Destroy;
end;

procedure TOxtParser.Clear;
begin
  Root.Free;
  Root := TOxtNode.Create(self);
end;

procedure TOxtParser.ParseString(const Source: AnsiString);
const
  InNone = 0;
  InName = 1;
  InAttr = 2;
  InValue = 3;
var
  Node, NewNode: TOxtNode;
  c, cmax: Integer;
  u: Word;
  InStr: boolean;
  StrDel: Ansichar;
  tmpEncoding: AnsiString;

  procedure AddChar(chr: Ansichar);
  begin
    case u of
      InName:
        Node.FName := Node.FName + chr;
      InAttr:
        Node.FAttr := Node.FAttr + chr;
      InValue:
        Node.FValue := Node.FValue + chr;
    end;
  end;

begin
  Clear;
  Node := FRoot;
  c := 0;
  cmax := Length(Source);
  u := InNone;
  InStr := false;
  StrDel := #0;

  while c < cmax do
  begin
    inc(c);
    case Source[c] of
      '<':
        if InStr then
          AddChar(Source[c])
        else
        begin
          case u of
            InAttr:
              AddChar(Source[c]);
            InValue, InNone:
              begin
                if (c = cmax) or (Source[c + 1] <> '/') then
                begin
                  NewNode := TOxtNode.Create(self);
                  NewNode.FLevel := Node.FLevel + 1;
                  NewNode.FParent := Node;
                  Node.FItems.Add(NewNode);
                  Node := NewNode;
                  u := InName;
                end
                else
                  u := InNone;
              end;
          end;
        end;
      '>':
        if InStr then
          AddChar(Source[c])
        else
        begin
          case u of
            InName, InAttr:
              begin
                u := InValue;
              end;
          end;
        end;
      #13, #10, #9, #32:
        if InStr then
          AddChar(#32)
        else
        begin
          case u of
            InName:
              u := InAttr;
            InValue:
              AddChar(#32);
          end;
        end;
      #34, #39:
        if InStr then
        begin
          if Source[c] <> StrDel then
            AddChar(Source[c])
          else
          begin
            InStr := false;
            StrDel := #0;
            if (u = InAttr) and (Node.FAttr <> '') then
              Node.FAttr := Node.FAttr + #13#10;
          end;
        end
        else
        begin
          case u of
            InName:
              u := InAttr;
            InValue:
              AddChar(Source[c]);
          end;
          if u <> InValue then
          begin
            InStr := true;
            StrDel := Source[c];
          end;
        end;
      '/':
        if InStr then
          AddChar(Source[c])
        else
        begin
          case u of
            InValue:
              AddChar(Source[c]);
            InName, InAttr, InNone:
              begin
                Node := Node.Parent;
                if Node = nil then
                  Node := Root;
                u := InNone;
              end;
          end;
        end;
    else
      AddChar(Source[c]);
    end;
  end;

  { Detect encoding }
  if (Root.FItems.Count > 0) then
  begin
    tmpEncoding := LowerCase(TOxtNode(Root.FItems[0]).Attrib('encoding')); { implcit conversion }
    if Copy(tmpEncoding, 1, 3) = 'iso' then
    begin
      if (tmpEncoding = 'iso-8859-1') or (tmpEncoding = 'iso8859-1') then
        FCodepage := 28591
      else if (tmpEncoding = 'iso-8859-2') or (tmpEncoding = 'iso8859-2') then
        FCodepage := 28592
      else if (tmpEncoding = 'iso-8859-3') or (tmpEncoding = 'iso8859-3') then
        FCodepage := 28593
      else if (tmpEncoding = 'iso-8859-4') or (tmpEncoding = 'iso8859-4') then
        FCodepage := 28594
      else if (tmpEncoding = 'iso-8859-5') or (tmpEncoding = 'iso8859-5') then
        FCodepage := 28595
      else if (tmpEncoding = 'iso-8859-6') or (tmpEncoding = 'iso8859-6') then
        FCodepage := 28596
      else if (tmpEncoding = 'iso-8859-7') or (tmpEncoding = 'iso8859-7') then
        FCodepage := 28597
      else if (tmpEncoding = 'iso-8859-8') or (tmpEncoding = 'iso8859-8') then
        FCodepage := 28598
      else if (tmpEncoding = 'iso-8859-9') or (tmpEncoding = 'iso8859-9') then
        FCodepage := 28599
      else if (tmpEncoding = 'iso-8859-13') or (tmpEncoding = 'iso8859-13') then
        FCodepage := 28603
      else if (tmpEncoding = 'iso-8859-15') or (tmpEncoding = 'iso8859-15') then
        FCodepage := 28605;
    end
    else if (tmpEncoding = 'koi8-r') or (tmpEncoding = 'koi8-u') then
      FCodepage := 20866
    else if (Copy(tmpEncoding, 1, 7) = 'windows') or
      (Copy(tmpEncoding, 1, 9) = 'microsoft') then
      FCodepage := StrToIntDef(Copy(tmpEncoding, 4, Length(tmpEncoding) - 3), FCodepage);  { implcit conversion }
  end;

end;

function TOxtParser.GetFirst: TOxtNode;
begin
  Result := nil;
  if (Root.FItems.Count > 0) then
    Result := TOxtNode(Root.FItems[0]);
end;

function TOxtParser.GetNext(Node: TOxtNode): TOxtNode;
var
  i: Integer;
begin
  Result := nil;
  if Node = nil then
    exit;

  if (Node.FItems.Count > 0) then
    Result := TOxtNode(Node.FItems[0])
  else
    while (Result = nil) and (Node <> nil) and (Node.FParent <> nil) do
    begin
      i := TOxtNode(Node.FParent).FItems.Indexof(Node);
      if i < TOxtNode(Node.FParent).FItems.Count - 1 then
        Result := TOxtNode(Node.FParent).FItems[i + 1]
      else
        Node := Node.FParent;
    end;
end;

function TOxtParser.AsText: string;
var
  Node: TOxtNode;
  str: TStringlist;
  i: Integer;
begin
  Result := '';
  Node := GetFirst;
  while Node <> nil do
  begin
    Result := Result + Copy('                                ', 1,
      Node.FLevel * 4) + 'NODE: ' + Node.Name + ' VALUE: ' + Node.Value + #13#10;
    str := TStringlist.Create;
    str.Text := Node.FAttr;
    for i := 0 to str.Count - 1 do
      Result := Result + Copy('                                ', 1,
        Node.FLevel * 6) + 'ATTR: ' + str[i] + #13#10;

    str.Free;
    Node := GetNext(Node);
  end;
end;

end.
