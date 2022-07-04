unit API_usermanager;

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
// TODO:
//
// 07102008, r1.04, ari pikivirta
//  * files are now encrypted and also encoded to 64 format
//
// 09012008, r1.03, ari pikivirta
//  * whole component rewrite
//
// 18102004, r1.02, ari pikivirta
// * added properties for storing current user name and password
// * added overloaded functions for above properties for authorizing
//
// r1.01, ari pikivirta
// * added version information property (string)
// * removed admin part - for now on new component must be inserted
//   to use more userlist..


interface

uses
  Windows, Messages, SysUtils, Classes, SyncObjs, API_base, DIALOGS;

type
  TUserDataRecord = record
    S: String;
    I: Integer;
    F: double;
    P: Pointer;
  end;

  // user information record that can
  // be extracted anytime threadsafe
  TAPI_User = record
    Username: string;               // user name
    Password: string;               // password
    Level: Integer;                 // access level
    Data: TUserDataRecord;
  end;

  // events; user content can be changed
  // on the event, always called in lock
  TAPI_OnNewUser = procedure(Sender: TObject; var User: TAPI_User) of object;
  TAPI_OnDeleteUser = procedure(Sender: TObject; var User: TAPI_User) of object;
  TAPI_OnModifyUser = procedure(Sender: TObject; OldUser: TAPI_User; var NewUser: TAPI_User) of object;

  // component
  TAPI_usermanager = class(TAPI_Custom_Component)
  private
    flock: tcriticalsection;        // critical section
    fusers: integer;                // amount of users listed
    fuser: array of TAPI_User;      // user items array
    fcurusername: string;           // current username
    fcurpassword: string;           // current password
    fuserlimit: integer;            // maximum amount of users
    fonnewuser: TAPI_OnNewUSer;     // on new user event
    fondeleteuser: TAPI_OnDeleteUser; // on delete user event
    fonmodifyUser: TAPI_OnModifyUser; // on user changed
    function Int_IndexOf(Username, Password: string): integer;
    function  getuserlimit: integer;
    procedure setuserlimit (i: integer);
    function  getcurusername: string;
    procedure setcurusername(s: string);
    function  getcurpassword: string;
    procedure setcurpassword(s: string);

  protected
  public
    constructor Create (aowner: tcomponent); override;
    destructor Destroy; override;

    procedure Clear;
    function Count: integer;
    function IndexOf(Username, Password: string): integer;
    function UsernameExists(Username: string): boolean;
    function Level(Username, Password: string): integer; overload;
    procedure Level(Username, Password: string; Level: integer); overload;
    function Level: integer; overload;
    function Data(Username, Password: string): TUserDataRecord; overload;
    function Data: TUserDataRecord; overload;
    procedure Data(Username, Password: string; Data: TUserDataRecord); overload;
    procedure ListUsernames(List: TStrings);
    function UserRecord(Index: integer): TAPI_User;
    function Add(Username, Password: String; Level: integer): boolean; overload;
    function Add(User: TAPI_User): boolean; overload;
    function Modify(OldUser, NewUser: TAPI_User): boolean;
    function Delete(Username, Password: string): boolean; overload;
    function Delete(User: TAPI_User): boolean; overload;

    function LoadFromFile(Filename: string): boolean;
    function SaveToFile(Filename: string): boolean;

  published
    property MaxUserCount: integer read getuserlimit write setuserlimit;
    property Username: string read getcurusername write setcurusername;
    property Password: string read getcurpassword write setcurpassword;
    property OnNewUser: TAPI_OnNewUSer read fonnewuser write fonnewuser;
    property OnDeleteUser: TAPI_OnDeleteUser read fondeleteuser write fondeleteuser;
    property OnModifyUser: TAPI_OnModifyUser read fonmodifyUser write fonmodifyUser;

  end;

procedure Register;

implementation

uses
  api_strings;

const
  revisioninformation = 'r1.04/ari.pikivirta@kolumbus.fi';
  encryption_key_0 = 21;
  encryption_key_1 = 9;
  encryption_key_2 = 77;

{$R *.RES}

//------------------------------------------------------------------------------
constructor TAPI_usermanager.create (aowner: tcomponent);
begin
  inherited create(aowner);
  version:= revisioninformation;
  flock:= Tcriticalsection.create;
  fcurusername:= 'Anonymous';
  fcurpassword:= '';
  fuserlimit:= 0;
  fusers:= 0;
  setlength(fuser, fusers);
end;

//------------------------------------------------------------------------------
destructor TAPI_usermanager.destroy;
begin
  Clear; // free all data(s) assigned
  flock.free;
  inherited destroy;
end;

//------------------------------------------------------------------------------
function TAPI_usermanager.Count: integer;
begin
  flock.acquire;
  try
    result:= fusers;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Usermanager.Int_indexof(username, password: string): integer;
var
  i: integer;
begin
  result:= -1;
  for i:=0 to fusers-1 do
    if (username<>'') and (password<>'') and (fuser[i].Username=username) and (fuser[i].Password=password) then
    begin
      result:= i;
      break;
    end;
end;

function TAPI_Usermanager.IndexOf(Username, Password: string): integer;
begin
  flock.Acquire;
  try
    result:= Int_IndexOf(Username, password);
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function  TAPI_Usermanager.getuserlimit: integer;
begin
  flock.acquire;
  try
    result:= fuserlimit;
  finally
    flock.release;
  end;
end;

procedure TAPI_Usermanager.setuserlimit (i: integer);
begin
  flock.acquire;
  try
    if i>-1 then fuserlimit:= i;
  finally
    flock.release;
  end;
end;

function  TAPI_Usermanager.getcurusername: string;
begin
  flock.Acquire;
  try
    result:= fcurusername;
  finally
    flock.release;
  end;
end;

procedure TAPI_Usermanager.setcurusername(s: string);
begin
  flock.acquire;
  try
    fcurusername:= s;
  finally
    flock.Release;
  end;
end;

function  TAPI_Usermanager.getcurpassword: string;
begin
  flock.Acquire;
  try
    result:= fcurpassword;
  finally
    flock.Release;
  end;
end;

procedure TAPI_Usermanager.setcurpassword(s: string);
begin
  flock.acquire;
  try
    fcurpassword:= s;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Usermanager.UsernameExists(Username: string): boolean;
var
  i: integer;
begin
  flock.Acquire;
  try
    result:= false;
    for i:=0 to fusers-1 do
      if fuser[i].Username=username then
      begin
        result:= true;
        break;
      end;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_usermanager.UserRecord(Index: integer): TAPI_User;
begin
  flock.acquire;
  try
    if (index>-1) and (index<fusers) then
      result:= fuser[index];
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_usermanager.Level(Username, Password: string): integer;
var
  index: integer;
begin
  flock.Acquire;
  try
    index:= int_indexof(username, password);
    if index>-1 then
    begin
      result:= fuser[index].Level;
    end else
      result:= 0;
  finally
    flock.release;
  end;
end;

procedure TAPI_usermanager.Level(Username, Password: string; Level: integer);
var
  index: integer;
begin
  flock.acquire;
  try
    index:= int_indexof(username, password);
    if index>-1 then
      fuser[index].level:= level;
  finally
    flock.release;
  end;
end;

function TAPI_usermanager.Level: integer;
begin
  result:= level(Username, Password);
end;

//------------------------------------------------------------------------------
function TAPI_Usermanager.Data(Username, Password: string): TUserDataRecord;
var
  index: integer;
begin
  flock.acquire;
  try
    index:= int_indexof(username, password);
    if index>-1 then result:= fuser[index].Data
      else begin
        result.S:= '';
        result.I:= 0;
        result.F:= 0;
        result.P:= nil;
      end;
  finally
    flock.release;
  end;
end;

procedure TAPI_usermanager.Data(Username, Password: string; Data: Tuserdatarecord);
var
  index: integer;
begin
  flock.acquire;
  try
    index:= int_indexof(username, password);
    if index>-1 then
      fuser[index].data:= data;
  finally
    flock.release;
  end;
end;

function TAPI_usermanager.Data: TUserDataRecord;
begin
  result:= Data(username, password);
end;

//------------------------------------------------------------------------------
procedure TAPI_usermanager.ListUsernames(List: Tstrings);
var
  i: integer;
begin
  flock.Acquire;
  try
    List.clear;
    for i:=0 to fusers-1 do
      List.add(fuser[i].Username);
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
procedure TAPI_usermanager.Clear;
var
  i: integer;
begin
  flock.Acquire;
  try
    for i:=0 to fusers-1 do
      if assigned(fondeleteuser) then
        fondeleteuser(self, fuser[i]);      // delete event on all
    fusers:= 0;
    setlength(fuser, fusers);
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_Usermanager.Add(User: TAPI_User): boolean;
var
  index: integer;
begin
  flock.acquire;
  try
    index:= int_indexof(user.username, user.password);
    if (index<0) and ((fuserlimit=0) or (fusers<fuserlimit)) then
    begin
      fusers:= fusers+1;
      setlength(fuser, fusers);
      if assigned(fonnewuser) then fonnewuser(self, user);
      fuser[fusers-1]:= user;
      result:= true;
    end else
      result:= false;
  finally
    flock.release;
  end;
end;

function TAPI_usermanager.Add(Username, Password: String; Level: integer): boolean;
var
  user: TAPI_user;
begin
  user.Username:= username;
  user.Password:= password;
  user.Level:= level;
  user.Data.S:= '';
  user.data.I:= 0;
  user.data.F:= 0;
  user.data.p:= nil;
  result:= add(user);
end;

//------------------------------------------------------------------------------
function TAPI_usermanager.Modify(OldUser, NewUser: TAPI_User): boolean;
var
  index: integer;
begin
  flock.acquire;
  try
    index:= int_indexof(olduser.Username, olduser.Password);
    if index>-1 then
    begin
      if assigned(fonmodifyuser) then
        fonmodifyuser(self, olduser, newuser);
      fuser[index]:= newuser;
      result:= true;
    end else
      result:= false;
  finally
    flock.release;
  end;
end;

//------------------------------------------------------------------------------
function TAPI_usermanager.Delete(User: TAPI_User): boolean;
var
  index, i: integer;
begin
  flock.acquire;
  try
    index:= int_indexof(user.Username, user.Password);
    if index>-1 then
    begin
      if assigned(fondeleteuser) then
        fondeleteuser(self, user);
      for i:=index to fusers-2 do
        fuser[i]:= fuser[i+1];
      fusers:= fusers-1;
      setlength(fuser, fusers);
      result:= true;
    end else
      result:= false;
  finally
    flock.release;
  end;
end;

function TAPI_usermanager.Delete(Username, Password: string): boolean;
var
  User: TAPI_User;
begin
  user.Username:= username;
  user.Password:= password;
  result:= delete(user);
end;

//------------------------------------------------------------------------------
const
  USERMANAGER_KEY1 = 21;
  USERMANAGER_KEY2 = 74;
  USERMANAGER_KEY3 = 89;
  USERMANAGER_HEAD = 'USERMANAGERFILE001';

function TAPI_usermanager.LoadFromFile(Filename: string): boolean;
var
  sl: tstringlist;
  i,c: integer;
  User: TAPI_user;
begin
  result:= false;
  sl:= tstringlist.create;
  try
    Clear; // to free all data assigned
    sl.clear;
    try
      sl.LoadFromFile(filename);
      sl.Text:= string(api_strings.Decrypt(ansistring(sl.text), USERMANAGER_KEY1, USERMANAGER_KEY2, USERMANAGER_KEY3));
      sl.text:= string(api_strings.Decode64(ansistring(sl.text)));
    except
      exit;
    end;
    // check header
    if (sl.count>0) and (sl.values['HEADER']=USERMANAGER_HEAD) then
    begin
      // go trough all users
      trystrtoint(sl.Values['Users'], c);
      for i:=0 to c-1 do
      begin
        // read basic data
        user.Username:= sl.Values['User'+inttostr(i)];
        user.Password:= sl.values['Pass'+inttostr(i)];
        trystrtoint(sl.values['Levl'+inttostr(i)], user.level);
        user.data.s:= sl.values['DataS'+inttostr(i)];
        trystrtoint(sl.values['DataI'+inttostr(i)], user.data.I);
        trystrtofloat(sl.values['DataF'+inttostr(i)], user.Data.F);
        user.data.P:= nil;
        add(user);
      end;
      // file read ok
      result:= true;
    end;
  finally
    sl.free;
  end;
end;

function TAPI_usermanager.SaveToFile(Filename: string): boolean;
var
  sl: tstringlist;
  i: integer;
  User: TAPI_user;
begin
  result:= false;
  sl:= tstringlist.create;
  try
    sl.clear;
    // go trough all users
    sl.add('HEADER='+USERMANAGER_HEAD);
    sl.add('Users='+inttostr(fusers));
    for i:=0 to fusers-1 do
    begin
      user:= UserRecord(i);
      // store basic data
      sl.add('User'+inttostr(i)+'='+user.Username);
      sl.add('Pass'+inttostr(i)+'='+user.Password);
      sl.add('Levl'+inttostr(i)+'='+inttostr(user.level));
      sl.add('DataS'+inttostr(i)+'='+user.data.s);
      sl.add('DataI'+inttostr(i)+'='+inttostr(user.data.I));
      sl.add('DataF'+inttostr(i)+'='+floattostr(user.Data.F));
    end;
    // encrypt and save to stream
    try
      sl.text:= string(api_Strings.Encode64(ansistring(sl.text)));
      sl.Text:= string(api_strings.Encrypt(ansistring(sl.text), USERMANAGER_KEY1, USERMANAGER_KEY2, USERMANAGER_KEY3));
      sl.SaveToFile(filename);
      result:= true;
    except
      // failed to save file
    end;
  finally
    sl.free;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_usermanager]);
end;

end.
