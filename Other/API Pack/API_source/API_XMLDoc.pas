unit API_XMLDoc;

//------------------------------------------------------------------------------
// XMLDoc components for parsing xml documents more easily.
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
// 06102008, r1.03, ari pikivirta
//  * rewrote all functions totally again, not tested yet though, but updated anyway =)
//
// 22032006, r1.02, ari pikivirta
//  * added write function
//
// 21032006, r1.01, ari pikivirta
//  * added load and save functions for the document

interface

uses
  SysUtils, Classes, Dialogs, SyncObjs;

type
  TAPI_XMLDoc = class(TComponent)
  private
    { Private declarations }
    fversion: string;
    flock: tcriticalsection;
    fxmldoc: string;
    fmodified: boolean;

    procedure dummys(s: string);
    procedure dummyb(b: boolean);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SplitPath(APath: string): tstringlist;
    function RemoveBeginning(AXMLDoc, APath: String; var APosition: integer): string; overload;
    function RemoveBeginning(AXMLDoc, APath: String): string; overload;
    function RemoveEnd(AXMLDoc, APath: String; var APosition: integer): string; overload;
    function RemoveEnd(AXMLDoc, APath: String): string; overload;

    function Nodes(AXMLDoc, APath: string; IncludeSubNodes: Boolean = FALSE): tstringlist; overload;
    function Nodes(APath: string; IncludeSubNodes: Boolean = FALSE): tstringlist; overload;
    function GetValue (AXMLDoc, APath: string): string; overload;
    function GetValue (APath: string): string; overload;
    function SetValue (var AXMLDoc: string; APath, NewValue: string): boolean; overload;
    function SetValue (APath, NewValue: string): boolean; overload;
    function Search (AXMLDoc, ValueToSearch: string): string; overload;
    function Search (ValueToSearch: string ): string; overload;

    function Load( Filename: string ): boolean;
    function Save( Filename: string ): boolean;

  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
    property XMLDoc: string read fxmldoc write fxmldoc;
    property Modified: boolean read fmodified write dummyb stored false;
  end;

procedure Register;

implementation

{$include '..\API_source\inc\CompilerVersions.INC'}
{$R *.RES}

uses
  api_strings;

//------------------------------------------------------------------------------
procedure TAPI_XMLDoc.dummys(s: string);
begin
end;

//------------------------------------------------------------------------------
procedure TAPI_XMLDoc.dummyb(b: boolean);
begin
end;

//------------------------------------------------------------------------------
constructor TAPI_XMLDoc.create(aowner: tcomponent);
begin
  inherited create(aowner);
  fversion:= 'r1.03/ari.pikivirta@kolumbus.fi';
  fxmldoc:= '';
  fmodified:= false;
  flock:= tcriticalsection.create;
end;

//------------------------------------------------------------------------------
destructor TAPI_XMLDoc.destroy;
begin
  flock.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
// SplitPath functions splits the path to pieces and stores
// everything on their own line on resulting list
function TAPI_XMLDoc.SplitPath(APath: string): tstringlist;
var
  temppath: Ansistring;
  i: integer;
begin
  temppath:= ansistring(apath);

  // create and init result list
  result:= tstringlist.create;
  result.clear;                                     // make sure result is cleared

  // remove double slashes
  stringreplace(temppath, '\\', '\');               // remove double slashed

  // check first and last character for slash
  if (temppath<>'') then                            // if temppath was defined
    if (temppath[1] = '\') then                     // if first character is slash
      delete(temppath,1,1);                         // delete first character

  // split path items into result list
  result.Delimiter:= '\';
  result.DelimitedText:= string(temppath);

  // remove empty lines
  i:=0;
  while i<result.count do
    if result[i]='' then result.delete(i)
      else i:= i + 1;
end;

//------------------------------------------------------------------------------
// remove everything from space to end
function RemoveSpaces(AString: string): string;
var
  SpacePosition: integer;
begin
  spaceposition:= pos(' ', ansistring(AString));
  if SpacePosition>0 then
    result:= string(copy(ansistring(AString), 1, SpacePosition))
    else result:= AString;
end;

//------------------------------------------------------------------------------
// remove beginning of document to the beginning of value of APath
function TAPI_XMLDoc.RemoveBeginning(AXMLDoc, APath: String; var APosition: integer): string;
var
  s: string;
  nodes: tstringlist;
  i, p, start: integer;
begin
  result:= '';
  aposition:= 0;

  if Apath='' then
  begin
    result:= AXMLDoc;
    exit;
  end;

  nodes:= splitpath(APath);                           // create nodes list
  try
    if nodes.Count<1 then exit;                       // if no path nodes were defined exit

    // locate nodes..
    start:= 0;
    for i:=0 to nodes.count-1 do                      // go trough all splitted node names
    begin
      s:= '<'+nodes[i];                               // set next node name
      RemoveSpaces(s);                                // remove everything after space (params)
      p:= pos(ansistring(s), ansistring(AXMLDoc), false, start+1);            // find next sub node
      if (p>0) then start:= p                         // set next starting point
        else exit;                                    // error: couldn't locate node!
    end;

    p:= pos('>', ansistring(AXMLDoc), false, start+1);            // find end of above node item
    if (p<1) then exit;                               // no end marking found

    result:= string(copy(ansistring(AXMLDoc), p, length(ansistring(AXMLDoc))));       // resulting document
    aposition:= p;                                    // return position
  finally
    nodes.free;                                       // free nodes list
  end;
end;

function TAPI_XMLDoc.RemoveBeginning(AXMLDoc, APath: String): string;
var
  temp: integer;
begin
  result:= RemoveBeginning(AXMLDoc, APath, Temp);
end;

//------------------------------------------------------------------------------
// remove rest of document from value of Apath node (note, that
// result does include all subnodes if there's any with the value)
function TAPI_XMLDoc.RemoveEnd(AXMLDoc, APath: string; var APosition: integer): string;
var
  nodes: tstringlist;
  s: string;
  p: integer;
begin
  aposition:= 0;

  if Apath='' then
  begin
    result:= AXMLDoc;
    exit;
  end else
    result:= '';

  nodes:= splitpath(APath);
  try
    if nodes.count<1 then exit;
    s:= '</'+RemoveSpaces(nodes[nodes.count-1]);      // last path item's ending
    p:= pos(ansistring(s), ansistring(AXMLDoc));                              // locate start of end :)
    if (p>0) then
    begin
      result:= copy(AXMLDoc, 1, p-1);                 // we've winner here
      aposition:= p-1;                                // where it all ended up to
    end;
  finally
    nodes.free;
  end;
end;

function TAPI_XMLDoc.RemoveEnd(AXMLDoc, APath: string): string;
var
  temp: integer;
begin
  result:= RemoveEnd(AXMLDoc, APath, temp);
end;

//------------------------------------------------------------------------------
// lists all nodes on a specified APath. In case APath is empty
// all nodes will be listed
function TAPI_XMLDoc.Nodes(AXMLDoc, APath: string; IncludeSubNodes: Boolean = FALSE): tstringlist;
var
  tempXML: string;
  s: string;
  p, start, temp: integer;
  items: tstringlist;
begin
  result:= tstringlist.create;                        // create stringlist
  result.clear;                                       // make sure list is clear

  tempXML:= removebeginning(AXMLDoc, APath);
  tempXML:= removeend(tempXML, Apath);

  items:= tstringlist.create;
  try
    items.Delimiter:= '\';                            // make full path!
    items.Text:= '';
    start:= 0;                                        // start position is zero
    repeat
      p:= pos('<', ansistring(tempXML), false, start+1);          // look for "<" tag to start
      if (p>0) then
      begin
        temp:= p;                                     // temp = start position
        start:= p + 1;                                // set new starting position
        p:= pos('>', ansistring(tempXML), false, start);          // find end position
        if (p>temp+1) then                            // if end ">" was found with any content
        begin
          s:= copy(tempXML, temp+1, p-temp);          // get node name content
          if (s[1]<>'/') then                         // if end node
          begin
            {$ifdef DELPHI2009UP}
            if (not (charinset(s[1], ['?', '!']))) then        // and not anything weird nor end
            {$else}
            if (not (s[1] in ['?', '!'])) then        // and not anything weird nor end
            {$endif}
            begin
              items.Add(s);                           // add sub node to path list
              result.add(items.DelimitedText);        // add full path
            end;
          end else
          begin
            items.Delete(items.count-1);              // delete last
            if (items.count>1) and (not IncludeSubNodes) then // if not including subs
              result.Delete(result.count-1);          // delete last item
          end;
        end;
      end;
    until (p<1);
  finally
    items.free;
  end;
end;

function TAPI_XMLDoc.Nodes(APath: string; IncludeSubNodes: Boolean = FALSE): tstringlist;
begin
  flock.acquire;
  try
    result:= nodes(fXMLdoc, apath, includesubnodes);
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_XMLDoc.GetValue (AXMLDoc, APath: string): string;
var
  tempXML: string;
  p: integer;
begin
  tempXML:= removebeginning(AXMLDoc, APath);
  tempXML:= removeend(tempXML, APath);

  p:= pos('<', ansistring(tempXML));                            // find next item's start
  if (p>0) then result:= string(copy(ansistring(tempXML), 1, p-1))      // we found special result
    else result:= string(tempXML);                          // remaining doc is the result
end;

//------------------------------------------------------------------------------
function TAPI_XMLDoc.GetValue (APath: string): string;
begin
  flock.acquire;
  try
    result:= GetValue(fxmldoc, Apath);
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_XMLDoc.SetValue (var AXMLDoc: string; APath, NewValue: string): boolean;
var
  tempXML: string;
  p, p1, p2: integer;
begin
  tempXML:= AXMLDoc;

  // DUH! At the moment, if you add value
  // you should also be adding the nodes
  // needed, meaning.. if you add value "1" to
  // some node that doesn't exist new value should be:
  // <newvalue>1</newvalue>

  tempXML:= removebeginning(AXMLDoc, APath, p1);
  tempXML:= removeend(tempXML, APath, p2);
  p2:= p2 + p1;

  // find the next item to the value
  // to get the copy positions correct
  p:= pos('<', ansistring(tempXML));                            // find next item's start
  if (p>0) then p2:= p1 + p;                        // some node lies between!

  // make result
  tempXML:=                                         // set param variable
    copy( AXMLDoc, 1, p1 ) +                        // beginning of document
    NewValue +                                      // New Value (with line change)
    copy( AXMLDoc, p2, length(AXMLDoc) );           // end of the document

  AXMLDoc:= tempXML;                                // store resulting document back
  fModified:= true;                                 // set modified time
  result:= true;
end;

//------------------------------------------------------------------------------
function TAPI_XMLDoc.SetValue (APath, NewValue: string): boolean;
begin
  flock.acquire;
  try
    result:= SetValue( fxmldoc, Apath, NewValue );
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_XMLDoc.Search (AXMLDoc, ValueToSearch: string): string;
var
  sl: tstringlist;
  i: integer;
  value: string;
begin
  result:= ''; // path to texttosearch
  sl:= nodes(axmldoc, '', true);                    // get list of all nodes
  try
    for i:=0 to sl.count-1 do
    begin
      value:= getvalue(axmldoc, sl[i]);
      if pos(ansistring(valuetosearch), ansistring(value), true)>0 then
      begin
        result:= sl[i];
        break;
      end;
    end;
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_XMLDoc.Search (ValueToSearch: string): string;
begin
  flock.acquire;
  try
    result:= Search(fxmldoc, valuetosearch);
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_XMLDoc.Load( Filename: string ): boolean;
var
  sl: tstringlist;
begin
  result:= false;

  if Filename<>'' then
  begin
    sl:= tstringlist.create;
    flock.Acquire;
    try
      try
        sl.Clear;
        sl.LoadFromFile( Filename );
        fxmldoc:= sl.Text;
        result:= true;
        fmodified:= false;
      except
        // just ignore
      end;
    finally
      flock.Release;
      sl.free;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_XMLDoc.Save( Filename: string ): boolean;
var
  sl: tstringlist;
begin
  result:= false;
  
  if Filename<>'' then
  begin
    sl:= tstringlist.create;
    flock.acquire;
    try
      sl.Clear;
      sl.text:= fxmldoc;
      try
        sl.SaveToFile( Filename );
        result:= true;
        fmodified:= false;
      except
      end;
    finally
      sl.free;
      flock.release;
    end;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_XMLDoc]);
end;

end.
