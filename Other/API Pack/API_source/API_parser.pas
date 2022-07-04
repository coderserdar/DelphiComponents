unit API_parser;

//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
//
// r1.02, 29062009, ari pikivirta
//  * added internal text property to do all job on the component
//  * changed to use api_Strings.getnexttoken instead of local function here
//
// r1.01, 08012008, ari pikivirta
//  * taken parser back into normal component set
//  * rewrote parsing of individual lines (also available without class)

interface

uses
  SysUtils, Classes, API_base, Dialogs;

type
  TAPI_parser = class(TAPI_Custom_Component)
  private
    fdelimiters: Ansistring;
    ftext: string;
    fwordlist: tstringlist;
    procedure setdelimiters(const s: ansistring);
    procedure settext(const s: string);
    procedure setROlist(const s: tstringlist); // readonly
  protected
  public
    constructor Create(aowner: tcomponent); override;
    destructor Destroy; override;
    function Parse(Const InputString, Delimiters: string; Wordlist: tstrings): boolean;
  published
    property Delimiters: Ansistring read fdelimiters write setdelimiters;
    property Text: string read ftext write settext stored FALSE;
    property WordList: tstringlist read fwordlist write setROlist stored FALSE;
  end;

function  ParseText(Const InputString: String; Const Delimiters: AnsiString; Wordlist: tstrings): boolean;
procedure Register;

implementation

{$r *.res}

uses
  api_strings;

const
  versioninfostring: string = 'r1.02/ari.pikivirta[at]kolumbus.fi';

//------------------------------------------------------------------------------
constructor TAPI_parser.Create(aowner: tcomponent);
begin
  inherited create(aowner);
  version:=       versioninfostring;
  fdelimiters:=   ' :;.,!?/\()[]{}<>&"'+#13#10;    // 15 most common delimiters + enter key
  ftext:=         '';
  fwordlist:=     tstringlist.create;
end;

//------------------------------------------------------------------------------
destructor TAPI_parser.Destroy;
begin
  fwordlist.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function  ParseText(Const InputString: String; Const Delimiters: AnsiString; Wordlist: tstrings): boolean;
var
  temp: String;
  //
  // locate next delimiter (of all defined)
  function GetDelimiterPos (const ts: ansistring): integer;
  var
    p, i: integer;
  begin
    result:= -1;                          // assume delimiter will not be found
    for p:=1 to length(ts) do
      for i:=1 to length(delimiters) do   // go trought delimiters
        if ts[p]=delimiters[i] then       // if one was found
        begin
          result:= p;                     // return current position as result
          exit;                           // break loop to exit function
        end;
  end;
  //
var
  s_in: String;
begin
  result:= FALSE;                         // assume we'll failt
  //
  wordlist.clear;                         // clear result list
  s_in:= InputString;                     // get input string
  if s_in='' then exit;                   // exit if input string is empty
  if delimiters='' then exit;             // exit if no delimiters is defined
  //
  s_in:= s_in + string(delimiters[1]);    // append with some delimiter
  repeat
    api_strings.GetNextToken(s_in, temp, delimiters);
    if temp<>'' then wordlist.add(temp);
  until (temp='');
  //
  result:= TRUE;                          // ok done.. result true
end;

//------------------------------------------------------------------------------
function TAPI_parser.Parse(Const InputString, Delimiters: string; Wordlist: tstrings): boolean;
var
  s1: String;
  s2: ansistring;
begin
  // call also exported function internally here..
  s1:= inputstring;
  s2:= ansistring(delimiters);
  result:= api_parser.ParseText(s1, s2, wordlist);
end;

//------------------------------------------------------------------------------
procedure TAPI_parser.setdelimiters(Const s: ansistring);
begin
  if (s<>fdelimiters) then
  begin
    fdelimiters:= s;
    api_parser.ParseText(ftext, fdelimiters, fwordlist);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_parser.settext(const s: string);
begin
  if (s<>ftext) then
  begin
    ftext:= s;
    api_parser.ParseText(ftext, fdelimiters, fwordlist);
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_parser.setROlist(const s: tstringlist);
begin
  // does absolutely nothing!
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_parser]);
end;

end.
