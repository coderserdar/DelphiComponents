unit API_registry;

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
// r1.10/06022008, ari pikivirta
//  * added miscellaneous functions
//
// r1.09/10082006, ari pikivirta
//  * fixed run on windows startup remove finally
//  * added check function for run on windows startup (for example checkboxes)
//
// r1.08, ari pikivirta
//  * pushed read and write functions back overloaded to keep usage as easy as possible
//  * fixed problem with remove run on windows startup
//  * removed create key when reading or deleting key entries
//
// r1.07, ari pikivirta
// * removed overloaded items for easier understanding
//
// r1.06, ari pikivirta
// * added set and remove uninstaller functions (miscellous)
// * added set and remove runonwindowstartup functions (miscellous)
//
// r1.05, ari pikivirta
// * overloaded write and read functions
//
// r1.04, ari pikivirta
// * added all root keys
// * changed names of accestypes, just "_" removed from the beginning
// * removed unneeded procedures
// * added "key" as property
// * added onerror event
//
// r1.03, ari pikivirta
// * added boolean read write functions
// * added try..exception functions to prevent app hangs
// * fixed the read value is cleared when error occurs during the read
//

interface

uses
  Windows, Messages, SysUtils, Classes, API_base;

const
  uninstallerkey = 'Software\Microsoft\Windows\CurrentVersion\Uninstall';
  runonstartupkey = 'Software\Microsoft\Windows\CurrentVersion\Run';
  runonceonstartupkey = 'Software\Microsoft\Windows\CurrentVersion\RunOnce';

type
  taccesstype = (
    as_all,
    as_write,
    as_read,
    as_query);

  troottype = (
    _HKEY_CLASSES_ROOT,
    _HKEY_CURRENT_USER,
    _HKEY_LOCAL_MACHINE,
    _HKEY_USERS,
    _HKEY_CURRENT_CONFIG);

  TAPI_registry = class(TAPI_Custom_Component)
  private
    froot: troottype;
    fkey: string;
    faccess: taccesstype;
    function getaccess: longword;
    function getroot: longword;
    procedure setkey(s: string);

  protected
  public
    // public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;

    // general functions
    function KeyExists: boolean;
    function DeleteItem(fitem:string): boolean;
    function DeleteKey: boolean;

    // registry write
    function WriteString(fitem: string; fvalue: string): boolean;
    function WriteInt(fitem: string; fvalue: integer): boolean;
    function WriteFloat(fitem: string; fvalue: double): boolean;
    function WriteBool(fitem: string; fvalue: boolean): boolean;

    // registry read
    function ReadString(fitem: string; var fvalue: string): boolean;
    function ReadInt(fitem: string; var fvalue: integer): boolean;
    function ReadFloat(fitem: string; var fvalue: double): boolean;
    function ReadBool(fitem: string; var fvalue: boolean): boolean;

    // miscellous important registry keys..
    function UninstallerSet(displayname, uninstaller: string): boolean;
    function UninstallerDelete(displayname: string): boolean;
    function RunOnWinStartupSet(displayname, filename: string): boolean;
    function RunOnWinStartupDelete(displayname: string): boolean;
    function RunOnWinStartupExists(displayname: string): boolean;
    function GetExeByExtension(sExt: string) : string;
    procedure RegisterFileType(ExtName:String; AppName:String);
    function RegisterFileTypeCommand(fileExtension, menuItemText, target: string): boolean;
    function UnRegisterFileTypeCommand(fileExtension, menuItemText: string): boolean;
    procedure EnableCTRLALTDEL(YesNo: boolean);

  published
    property Root: troottype read froot write froot;
    property Key: string read fkey write setkey;
    property Access: taccesstype read faccess write faccess;

  end;

procedure Register;

implementation

uses
  registry, shlobj (*for shellnotify*);

const
  versioninfo = 'r1.10/ari.pikivirta@kolumbus.fi';

{$R *.RES}

//-----------------------------------------------------------
constructor tAPI_registry.create(aowner:tcomponent);
begin
  inherited create(aowner);
  version:= versioninfo;
  froot:=_HKEY_CURRENT_USER;
  faccess:= as_all;
end;

//-----------------------------------------------------------
destructor tAPI_registry.destroy;
begin
  inherited destroy;
end;

//-----------------------------------------------------------
function TAPI_registry.UninstallerSet(displayname, uninstaller: string): boolean;
var
  reg: tregistry;
begin
  result:=false;
  if not fileexists(uninstaller) then exit;
  reg:=tregistry.create(KEY_WRITE);
  try
    reg.RootKey:=HKEY_LOCAL_MACHINE;
    if reg.openkey(uninstallerkey, true) then
    try
      reg.WriteString(displayname, uninstaller);
    finally
      reg.CloseKey;
      result:=True;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function TAPI_registry.UninstallerDelete(displayname: string): boolean;
var
  reg: tregistry;
begin
  result:=false;
  reg:=tregistry.create(KEY_ALL_ACCESS);
  try
    reg.RootKey:=HKEY_LOCAL_MACHINE;
    if reg.openkey(uninstallerkey, false) then
    try
      result:=reg.DeleteKey(displayname);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function TAPI_registry.RunOnWinStartupSet(displayname, filename: string): boolean;
var
  reg: tregistry;
begin
  result:=false;
  if not fileexists(filename) then exit;
  reg:=tregistry.create(KEY_WRITE);
  try
    reg.RootKey := HKEY_LOCAL_MACHINE;
    if reg.OpenKey(runonstartupkey, true) then
    try
      reg.WriteString(displayname, filename);
    finally
      reg.CloseKey;
      result:=true;
    end;
  finally
    reg.free;
  end;
end;

//-----------------------------------------------------------
function TAPI_registry.RunOnWinStartupDelete(displayname: string): boolean;
var
  reg: tregistry;
begin
  result:=false;
  reg:=tregistry.create(KEY_ALL_ACCESS);
  try
    reg.RootKey:= HKEY_LOCAL_MACHINE;
    if reg.OpenKey(runonstartupkey,false) then
    try
      result:=reg.DeleteValue(displayname);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function TAPI_registry.RunOnWinStartupExists(displayname: string): boolean;
var
  reg: tregistry;
begin
  result:= false;
  reg:= tregistry.create(KEY_READ);
  try
    reg.RootKey:= HKEY_LOCAL_MACHINE;
    if reg.OpenKey(runonstartupkey, false) then
    try
      result:= reg.ValueExists(displayname);
    finally
      reg.closekey;
    end;
  finally
    reg.free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_registry.GetExeByExtension(sExt : string) : string;
var
   sExtDesc:string;
begin
   with TRegistry.Create do
   begin
     try
       RootKey:=HKEY_CLASSES_ROOT;
       if OpenKeyReadOnly(sExt) then
       begin
         sExtDesc:=ReadString('') ;
         CloseKey;
       end;
       if sExtDesc <>'' then
       begin
         if OpenKeyReadOnly(sExtDesc + '\Shell\Open\Command') then
         begin
           Result:= ReadString('') ;
         end
       end;
     finally
       Free;
     end;
   end;

   if Result <> '' then
   begin
     if Result[1] = '"' then
     begin
       Result:=Copy(Result,2,-1 + Pos('"',Copy(Result,2,MaxINt))) ;
     end
   end;
end;

//------------------------------------------------------------------------------
procedure TAPI_registry.RegisterFileType(ExtName:String; AppName:String) ;
var
  reg:TRegistry;
begin
  reg := TRegistry.Create;
  try
   reg.RootKey:=HKEY_CLASSES_ROOT;
   reg.OpenKey('.' + ExtName, True) ;
   reg.WriteString('', ExtName + 'file') ;
   reg.CloseKey;
   reg.CreateKey(ExtName + 'file') ;
   reg.OpenKey(ExtName + 'file\DefaultIcon', True) ;
   reg.WriteString('', AppName + ',0') ;
   reg.CloseKey;
   reg.OpenKey(ExtName + 'file\shell\open\command', True) ;
   reg.WriteString('',AppName+' "%1"') ;
   reg.CloseKey;
  finally
   reg.Free;
  end;
  // below needs SHLobjs
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil) ;
end;

//------------------------------------------------------------------------------
function TAPI_registry.RegisterFileTypeCommand(fileExtension, menuItemText, target: string): boolean;
var
  reg: TRegistry;
  fileType: string;
begin
  result:= false;
  reg:= TRegistry.Create;
  with reg do
  try
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKey('.' + fileExtension, True) then
    begin
      fileType := ReadString('') ;
      if fileType = '' then
      begin
        fileType := fileExtension + 'file';
        WriteString('', fileType) ;
      end;
      CloseKey;
      if OpenKey(fileType + '\shell\' + menuItemText + '\command', True) then
      begin
        WriteString('', target + ' "%1"') ;
        CloseKey;
        result := true;
      end;
    end;
  finally
    Free;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_registry.UnRegisterFileTypeCommand(fileExtension, menuItemText: string): boolean;
var
  reg: TRegistry;
  fileType: string;
begin
  result := false;
  reg := TRegistry.Create;
  with reg do
  try
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKey('.' + fileExtension, True) then
    begin
      fileType := ReadString('') ;
      CloseKey;
    end;
    if OpenKey(fileType + '\shell', True) then
    begin
      DeleteKey(menuItemText) ;
      CloseKey;
      result := true;
    end;
  finally
    Free;
  end;
end;

//-----------------------------------------------------------
procedure TAPI_registry.EnableCTRLALTDEL(YesNo : boolean);
const
sRegPolicies = '\Software\Microsoft\Windows\CurrentVersion\Policies';
begin
   with TRegistry.Create do
   try
     RootKey:=HKEY_CURRENT_USER;
     if OpenKey(sRegPolicies+'\System\',True) then
     begin
       case YesNo of
         False:
           begin
             WriteInteger('DisableTaskMgr',1) ;
           end;
         True:
           begin
             WriteInteger('DisableTaskMgr',0) ;
           end;
       end;
     end;
     CloseKey;
     if OpenKey(sRegPolicies+'\Explorer\',True) then
     begin
       case YesNo of
         False:
           begin
             WriteInteger('NoChangeStartMenu',1) ;
             WriteInteger('NoClose',1) ;
             WriteInteger('NoLogOff',1) ;
           end;
         True:
           begin
             WriteInteger('NoChangeStartMenu',0) ;
             WriteInteger('NoClose',0) ;
             WriteInteger('NoLogOff',0) ;
           end;
       end;
     end;
     CloseKey;
   finally
     Free;
   end;
end;

//-----------------------------------------------------------
procedure TAPI_registry.setkey(s: string);
begin
  if s<>fkey then fkey:=s;
end;

//-----------------------------------------------------------
function tAPI_registry.getaccess: longword;
begin
  case faccess of
    as_all: result:=KEY_ALL_ACCESS;
    as_write: result:=KEY_WRITE;
    as_read: result:=KEY_READ;
    as_query: result:=KEY_QUERY_VALUE;
  else result:=KEY_ALL_ACCESS;
  end;
end;

//-----------------------------------------------------------
function TAPI_registry.getroot: longword;
begin
  case froot of
    _HKEY_CLASSES_ROOT: result:=HKEY_CLASSES_ROOT;
    _HKEY_CURRENT_USER: result:=HKEY_CURRENT_USER;
    _HKEY_LOCAL_MACHINE: result:=HKEY_LOCAL_MACHINE;
    _HKEY_USERS: result:=HKEY_USERS;
    _HKEY_CURRENT_CONFIG: result:=HKEY_CURRENT_CONFIG;
  else result:=HKEY_CURRENT_USER;
  end;
end;

//-----------------------------------------------------------
function TAPI_registry.KeyExists: boolean;
var
  reg: tregistry;
begin
  reg:=Tregistry.Create(getaccess);
  try
    reg.RootKey:=getroot;
    result:=reg.KeyExists(fkey);
  finally
    reg.free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.WriteString(fitem,fvalue:string): boolean;
var
  reg : Tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.RootKey:=getroot;
    if reg.openkey(fkey,true) then
    try
      reg.WriteString(fitem, fValue);
      result:=True;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.WriteInt(fitem:string; fvalue:integer): boolean;
var
  reg : Tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.rootkey:=getroot;
    if reg.openkey(fkey, true) then
    try
      reg.WriteInteger(fitem, fValue);
      result:=True;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.WriteFloat(fitem:string; fvalue:double): boolean;
var
  reg : Tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.rootkey:=getroot;
    if reg.openkey(fkey, true) then
    try
      reg.WriteFloat(fitem, fValue);
      result:=True;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.WriteBool(fitem: string; fvalue: boolean): boolean;
var
  reg : Tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.rootkey:=getroot;
    if reg.openkey(fkey, true) then
    try
      reg.WriteBool(fitem, fValue);
      result:=True;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.ReadString(fitem:string; var fvalue:string): boolean;
var
  reg: tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.rootkey:=getroot;
    if reg.openkey(fkey, false) then
    try
      fvalue:=reg.ReadString(fItem);
      result:=true;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.ReadInt(fitem:string; var fvalue:integer): boolean;
var
  reg: tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.RootKey:=getroot;
    if reg.openkey(fkey, false) then
    try
      fvalue:=reg.ReadInteger(fItem);
      result:=true;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.ReadFloat(fitem:string; var fvalue:double): boolean;
var
  reg: tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.RootKey:=getroot;
    if reg.openkey(fkey,false) then
    try
      fvalue:= reg.ReadFloat(fItem);
      result:=true;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.ReadBool(fitem: string; var fvalue: boolean): boolean;
var
  reg : Tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.rootkey:=getroot;
    if reg.openkey(fkey, false) then
    try
      fvalue:= reg.ReadBool(fitem);
      result:=True;
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.DeleteItem(fitem:string): boolean;
var
  reg : Tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.rootkey:=getroot;
    if reg.openkey(fkey, false) then
    try
      result:=reg.DeleteValue(fItem);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
function tAPI_registry.DeleteKey: boolean;
var
  reg : Tregistry;
begin
  result:=false;
  reg:=tregistry.create(getaccess);
  try
    reg.RootKey:=getroot;
    if reg.openkey(fkey, false) then
    try
      result:=reg.DeleteKey(fkey);
    finally
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

//-----------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Comm', [TAPI_registry]);
end;

end.
