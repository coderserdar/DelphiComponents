unit unitCodeSnippets;

interface

uses Windows, Classes, SysUtils;

type
TCodeSnippet = class
private
  fSnippetName: string;
  fCode: string;
public
  constructor Create (const ASnippetName, ACode : string);
  property SnippetName : string read fSnippetName;
  property Code : string read fCode;
end;

function GetSnippet (const snippetName : string) : string;

implementation

uses contnrs;

var
  SnippetsList : TObjectList;

procedure LoadSnippets;
var
  resInstance : THandle;
  res : THandle;
  codeResource : THandle;
  size, i, ps : Integer;
  l : TStringList;
  p, buffer : PChar;
  snippet : TStringList;
  snippetName : string;
  s : string;

begin
  ResInstance := FindResourceHInstance(HInstance);
  res := FindResource(ResInstance, 'SNIPETS', RT_RCDATA);
  if res > 0 then
  begin
    size := SizeOfResource (resInstance, res);
    codeResource := LoadResource (ResInstance, res);
    p := LockResource (codeResource);
    buffer := StrAlloc (size + 1);
    Move (p^, buffer^, size);
    l := TStringList.Create;
    try
      l.Text := buffer;
      snippet := Nil;
      for i := 0 to l.Count - 1 do
      begin
        s := l.Strings [i];
        if Copy (s, 1, 1) = '-' then
          if Assigned (snippet) then
          begin
            SnippetsList.Add (TCodeSnippet.Create (snippetName, snippet.Text));
            FreeAndNil (snippet)
          end
          else
          begin
            ps := Pos (':', s);
            if ps > 0 then
            begin
              snippet := TStringList.Create;
              snippetName := Trim (Copy (s, ps + 1, MaxInt));
            end
          end
        else
          if Assigned (snippet) then
            snippet.Add (s)
      end;

      if Assigned (snippet) then
      begin
        SnippetsList.Add (TCodeSnippet.Create (snippetName, snippet.Text));
        FreeAndNil (snippet)
      end
    finally
      l.Free
    end
  end
end;

function GetSnippet (const snippetName : string) : string;
var
  i : Integer;
begin
  result := '';
  for i := 0 to SnippetsList.Count - 1 do
    if CompareText (snippetName, TCodeSnippet (SnippetsList.Items [i]).SnippetName) = 0 then
    begin
      result := TCodeSnippet (SnippetsList.Items [i]).Code;
      break
    end;

  if result = '' then
    raise Exception.Create ('Code Snippet ' + snippetName + ' not found')
end;

{ TCodeSnippet }

constructor TCodeSnippet.Create(const ASnippetName, ACode: string);
begin
  fSnippetName := ASnippetName;
  fCode := ACode;
end;

initialization
  SnippetsList := TObjectList.Create;
  LoadSnippets;
finalization
  SnippetsList.Free
end.
