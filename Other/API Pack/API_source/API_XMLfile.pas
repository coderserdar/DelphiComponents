unit API_XMLfile;

//------------------------------------------------------------------------------
// XMLfile
// component to work with xml files with similar interface than ini files
// and registry. in some cases, using xml file formatting might be much
// better idea than just playing around with ini files (or at least to play
// with some fileformat of your own).
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// r1.00, 08022007, ari pikivirta
//  * created
//------------------------------------------------------------------------------

interface

uses
  SysUtils, Classes,
  Windows,
  XmlIntf, XMLDoc;

type
  TAPI_XMLfile = class(TComponent)
  private
    { Private declarations }
    fversion: string;
    ffilename: string;
    fxml: txmldocument;
    fchanged: boolean;
    procedure dummys(s: string);
    procedure Save;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const GroupName, Filename: string); overload;
    constructor Create(AOwner: TComponent; const Filename: string); overload;
    destructor Destroy; override;
    function ReadString(const Section, Key, Default: string): string;
    procedure WriteString(const Section, Key, Value: string);
    function Readinteger(const Section, Key: string; const Default: integer): integer;
    procedure WriteInteger(const Section, Key: string; const Value: integer);
    function ReadFloat(const Section, Key: string; const Default: double): double;
    procedure WriteFloat(const Section, Key: string; const Value: double);
    function ReadBool(const Section, Key: string; const Default: boolean): boolean;
    procedure WriteBool(const Section, Key: string; const Value: boolean);
  published
    { Published declarations }
    property Version: string read fversion write dummys stored false;
  end;

procedure Register;

implementation

{$R *.RES}

//------------------------------------------------------------------------------
constructor TAPI_XMLfile.Create(AOwner: tcomponent);
begin
  inherited create(AOwner);
  fversion:= 'r1.00/ari.pikivirta[at]kolumbus.fi';
  fxml:= TXMLDocument.Create(self);
  fxml.options:= [doNodeAutoIndent];
end;

//------------------------------------------------------------------------------
constructor TAPI_XMLfile.Create(AOwner: TComponent; const Filename: string);
begin
  self.create(AOwner);
  ffilename:= filename;
  if fileexists(filename) then
  begin
    fxml.LoadFromFile(ffilename);
    fchanged:= false;
  end else
  begin
    fxml.Active:= true;
    fchanged:= true;
  end;
end;

//------------------------------------------------------------------------------
constructor TAPI_XMLfile.Create(AOwner: TComponent; const GroupName, Filename: string);
begin
  self.create(AOwner);
  ffilename:= filename;
  if fileexists(filename) then
  begin
    fxml.LoadFromFile(ffilename);
    fchanged:= false;
  end else
  begin
    fxml.Active:= true;
    fxml.AddChild(GroupName);
    fchanged:= true;
  end;
end;

//------------------------------------------------------------------------------
destructor TAPI_XMLfile.Destroy;
begin
  save;
  fxml.Destroy;
  fxml.free;
  fxml:= nil;
  inherited destroy;
end;

//------------------------------------------------------------------------------
procedure TAPI_XMLFile.dummys(s: string);
begin
  // does absolutely nothing :)
end;

//------------------------------------------------------------------------------
function TAPI_XMLfile.ReadString(const Section, Key, Default: string): string;
var
  node: IXMLNode;
begin
  Node:= fxml.DocumentElement.ChildNodes.FindNode(Section);
  if Assigned(Node) and Node.HasAttribute(Key) then result:= Node.Attributes[Key]
    else result:= default;
end;

//------------------------------------------------------------------------------
procedure TAPI_XMLfile.WriteString(const Section, Key, Value: string);
var
  Node: IXMLNode;
begin
  if (readstring(Section, key, '')=Value) then exit;
  Node:= fxml.DocumentElement.ChildNodes.FindNode(Section);
  if not Assigned(Node) then Node:= fxml.DocumentElement.AddChild(Section);
  Node.Attributes[Key]:= Value;
  fchanged:= true;
end;

//------------------------------------------------------------------------------
function TAPI_XMLfile.Readinteger(const Section, Key: string; const Default: integer): integer;
begin
  try
    result:= strtoint(readstring(section, key, inttostr(default)));
  except
    result:= default;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_XMLfile.WriteInteger(const Section, Key: string; const Value: integer);
begin
  writestring(section, key, inttostr(value));
end;

//------------------------------------------------------------------------------
function TAPI_XMLfile.ReadFloat(const Section, Key: string; const Default: double): double;
begin
  try
    result:= strtofloat(readstring(section, key, floattostr(default)));
  except
    result:= default;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_XMLfile.WriteFloat(const Section, Key: string; const Value: double);
begin
  writestring(section, key, floattostr(value));
end;

//------------------------------------------------------------------------------
function TAPI_XMLfile.ReadBool(const Section, Key: string; const Default: boolean): boolean;
begin
  try
    result:= strtobool(readstring(section, key, booltostr(default)));
  except
    result:= default;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_XMLfile.WriteBool(const Section, Key: string; const Value: boolean);
begin
  writestring(section, key, booltostr(value));
end;

//------------------------------------------------------------------------------
procedure TAPI_XMLfile.Save;
begin
  if not fchanged then exit;
  if fileexists(ffilename) then copyfile(pchar(ffilename), pchar(ffilename+'.bak'), false);
  fxml.savetofile(ffilename);
  fchanged:= false;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_XMLfile]);
end;

end.
