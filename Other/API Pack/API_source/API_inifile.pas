unit API_inifile;

//------------------------------------------------------------------------------
// component to handle inifiles with. many handy features added to just
// writing and reading inifile values.
//------------------------------------------------------------------------------
// component free to use and modify, subject to following restinctions:
// 1. do not mispresent the origin
// 2. altered revisions must be clearly marked as modified from the original
// 3. do not remove this notice from the source code
// 4. send email about bugs, features needed and features you would like
// * if you like this very much, feel free to donate and support supporting
// and developing the package at www.paypal.com - ari pikivirta@kolumbus.fi
//------------------------------------------------------------------------------
// component revision history
//
// r1.08, 04082007, ari pikivirta
//  * added tryreadxxx functions
//
// r1.07, 01082007, ari pikivirta
//  * removed overloading of read/write functions
//
// r1.06, ari pikivirta
//  * restored overloading of read and write for easy usage
//  * removed unnecessary try..except from reading
//  * removed onerror event, because reason is always clear for false result
//  * added fileexists function
//  * changed double reading and writing to really write and read floats
//
// r1.02, ari pikivirta
// * added empty filename checks
// * added try..except into all write and read functions
// * added noolean write and read functions
// r1.03, ari pikivirta
// * added more properties
// * removed unneeded procedures
// r1.04, ari pikivirta
// * added section and item exists function
// * added onerror event
// r1.05, ari pikivirta
// * overloaded write and read functions
//

interface

uses
  Windows, Messages, SysUtils, Classes, API_base;

type
  TAPI_inifile = class(TAPI_Custom_Component)
  private
    ffilename: string;
    fsection: string;

  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;

    function FileExists: boolean;
    function SectionExists: boolean;
    function ItemExists(fitem: string): boolean;
    function DeleteItem(fsection, fitem: string): boolean;

    // inifile writing
    function WriteStr(fitem: string; fvalue: string): boolean;
    function WriteInt(fitem: string; fvalue: integer): boolean;
    function WriteFloat(fitem: string; fvalue: double): boolean;
    function WriteBool(fitem: string; fvalue: boolean): boolean;

    // inifile reading
    function ReadStr(fitem: string; var fvalue: string): boolean;
    function ReadInt(fitem: string; var fvalue: integer): boolean;
    function ReadFloat(fitem: string; var fvalue: double): boolean;
    function ReadBool(fitem: string; var fvalue: boolean): boolean;
    function TryReadStr(fitem, default: string): string;
    function TryReadInt(fitem: string; default: integer): integer;
    function TryReadFloat(fitem: string; default: double): double;
    function TryReadBool(fitem: string; default: boolean): boolean;

  published
    property Filename: string read ffilename write ffilename;
    property Section: string read fsection write fsection;

  end;

procedure Register;

implementation

{$R *.RES}

uses
  inifiles;

const
  versioninfo = 'r1.08/ari.pikivirta@kolumbus.fi';

//----------------------------------------------------------
constructor tAPI_inifile.create(aowner:tcomponent);
begin
  inherited create(aowner);
  version:=versioninfo;
  ffilename:='';
  fsection:='section';
end;

//----------------------------------------------------------
destructor tAPI_inifile.destroy;
begin
  inherited destroy;
end;

//----------------------------------------------------------
function TAPI_inifile.FileExists: boolean;
begin
  result:= sysutils.fileexists(ffilename);
end;

//----------------------------------------------------------
function TAPI_inifile.SectionExists: boolean;
var
  ini: tinifile;
begin
  result:=False;
  if ffilename<>'' then
  begin
    ini:=tinifile.Create(ffilename);
    try
      result:=ini.SectionExists(section);
    finally
      ini.Free;
    end;
  end;
end;

//----------------------------------------------------------
function TAPI_inifile.ItemExists(fitem: string): boolean;
var
  ini: tinifile;
begin
  result:=False;
  if ffilename='' then exit;
  ini:=tinifile.Create(ffilename);
  try
    result:=ini.ValueExists(fsection, fitem);
  finally
    ini.Free;
  end;
end;

//----------------------------------------------------------
function tAPI_inifile.WriteStr(fitem, fvalue:string): boolean;
var
  ini : Tinifile;
begin
  result:=false;
  if ffilename='' then exit;
  ini:=tinifile.create(ffilename);
  try
    ini.WriteString(fsection,fitem,fvalue);
    result:= true;
  finally
    ini.free;
  end;
end;

//----------------------------------------------------------
function tAPI_inifile.WriteInt(fitem: string; fvalue:integer): boolean;
var
  ini: Tinifile;
begin
  result:=false;
  if ffilename='' then exit;
  ini:=tinifile.create(ffilename);
  try
    ini.WriteInteger(fsection, fitem, fvalue);
    result:=true;
  finally
    ini.Free;
  end;
end;

//----------------------------------------------------------
function tAPI_inifile.WriteFloat(fitem: string; fvalue:double): boolean;
var
  ini : Tinifile;
begin
  result:=false;
  if ffilename='' then exit;
  ini:=tinifile.create(ffilename);
  try
    ini.WriteFloat(fsection,fitem,fvalue);
    result:=true;
  finally
    ini.Free;
  end;
end;

//----------------------------------------------------------
function tAPI_inifile.WriteBool(fitem: string; fvalue: boolean): boolean;
var
  ini: tinifile;
begin
  result:=false;
  if ffilename='' then exit;
  ini:=tinifile.create(ffilename);
  try
    ini.WriteBool(fsection, fitem, fvalue);
    result:=true;
  finally
    ini.free;
  end;
end;

//----------------------------------------------------------
function tAPI_inifile.ReadStr(fitem: string; var fvalue:string): boolean;
var
  ini : Tinifile;
begin
  result:=false;
  if ffilename='' then exit;
  ini:=tinifile.create(ffilename);
  try
    fvalue:= ini.ReadString(fsection,fitem,fvalue);
    result:=true;
  finally
    ini.Free;
  end;
end;

//----------------------------------------------------------
function tAPI_inifile.ReadInt(fitem: string; var fvalue:integer): boolean;
var
  ini: Tinifile;
begin
  result:=false;
  if ffilename='' then exit;
  ini:=tinifile.create(ffilename);
  try
    fvalue:=ini.ReadInteger(fsection,fitem,fvalue);
    result:=true;
  finally
    ini.Free;
  end;
end;

//----------------------------------------------------------
function tAPI_inifile.ReadFloat(fitem: string; var fvalue:double): boolean;
var
  ini : Tinifile;
begin
  result:=false;
  if ffilename='' then
    exit;
  ini:=tinifile.create(ffilename);
  try
    fvalue:=ini.ReadFloat(fsection,fitem,fvalue);
    result:=true;
  finally
    ini.Free;
  end;
end;

//----------------------------------------------------------
function tAPI_inifile.ReadBool(fitem: string; var fvalue: boolean): boolean;
var
  ini: tinifile;
begin
  result:=false;
  if ffilename='' then
    exit;
  ini:=tinifile.create(ffilename);
  try
    fvalue:=ini.Readbool(fsection, fitem, fvalue);
    result:=true;
  finally
    ini.free;
  end;
end;

//----------------------------------------------------------

function tAPI_inifile.deleteitem(fsection,fitem:string): boolean;
var
  ini : Tinifile;
begin
  result:=FalsE;
  if ffilename='' then exit;
  ini:=tinifile.create(ffilename);
  try
    if ini.ValueExists(fsection, fitem) then
    begin
      ini.DeleteKey(fsection,fitem);
      result:=true;
    end;
  finally
    ini.Free;
  end;
end;

//----------------------------------------------------------

function TAPI_inifile.TryReadStr(fitem, default: string): string;
var
  value: string;
begin
  value:= default;
  readstr(fitem, value);
  result:= value;
end;

function TAPI_inifile.TryReadInt(fitem: string; default: integer): integer;
var
  value: integer;
begin
  value:= default;
  readint(fitem, value);
  result:= value;
end;

function TAPI_inifile.TryReadFloat(fitem: string; default: double): double;
var
  value: double;
begin
  value:= default;
  readfloat(fitem, value);
  result:= value;
end;

function TAPI_inifile.TryReadBool(fitem: string; default: boolean): boolean;
var
  value: boolean;
begin
  value:= default;
  readbool(fitem, value);
  result:= value;
end;

//----------------------------------------------------------

procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_inifile]);
end;

end.
