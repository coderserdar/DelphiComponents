unit API_valueholder;

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
// 1.05, 07102008, ari pikivirta
//  * added use of try..finally to load and save functions
//  * added encoding & decoding to 64 for loading and saving
//
// 1.03, ari pikivirta
// * added get and set value functions

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  TAPI_ValueHolder = class(tcomponent)
  private
    fversion: string;
    fholder: tstringlist;
    fgroupindex: integer;
    fmodified: boolean;
    fitemindex: integer;
    procedure dummys(s: string);
    procedure dummyi(i: integer);
    procedure dummyb(b: boolean);

  protected
  public
    constructor Create(aowner:tcomponent); override;
    destructor Destroy; override;

    // general
    procedure Clear;
    function Save(fname:string): boolean;
    function Load(fname:string): boolean;

    // Items
    procedure Itemlist(Group:string; var list: tstringlist);
    function Item: string;
    function ItemName: string;
    function ItemValue: string;
    function Item_Count(Group: string): integer;
    function Item_Locate(Group, Name:string): boolean;
    function Item_Delete(Group, Name:string): boolean;
    function Item_First(Group:string): boolean;
    function Item_Next(Group:string): boolean;
    function Item_Previous(Group:string): boolean;
    function Item_Last(Group:string): boolean;
    function Item_Eof(Group:string): boolean;

    // Item values
    function Item_Add(Group, Value:string): boolean; overload;
    function Item_Add(Group, Name:string; value:boolean): boolean; overload;
    function Item_Add(Group, Name:string; value:string): boolean; overload;
    function Item_Add(Group, Name:string; value:integer): boolean; overload;
    function Item_Add(Group, Name:string; value:double): boolean; overload;
    function Item_Get(Group, Name:string; var value:boolean): Boolean; overload;
    function Item_Get(Group, Name:string; var value:string): boolean; overload;
    function Item_Get(Group, Name:string; var value:integer): boolean; overload;
    function Item_Get(Group, Name:string; var value:double): boolean; overload;
    function Item_Set(Group, Name:string; value:boolean): boolean; overload;
    function Item_Set(Group, Name:string; value:string): boolean; overload;
    function Item_Set(Group, Name:string; value:integer): boolean; overload;
    function Item_Set(Group, Name:string; value:double): boolean; overload;

    // Groups (Item)
    procedure Grouplist(List: tstrings);
    function Group: string;
    function Group_Count: integer;
    function Group_Locate(GroupName:string): boolean;
    function Group_Add(GroupName:string): boolean;
    function Group_Delete(GroupName:string): boolean;
    function Group_First: boolean;
    function Group_Next: boolean;
    function Group_Previous: boolean;
    function Group_Last: boolean;
    function Group_Eof: boolean;

  published
    property Version: string read fversion write dummys stored false;
    property Holder: tstringlist read fholder write fholder;
    property ItemIndex: integer read fitemindex write dummyi stored false;
    property GroupIndex: integer read fgroupindex write dummyi stored false;
    property Modified: boolean read fmodified write dummyb stored false;

  end;

procedure Register;

implementation

{$R *.RES}

uses
  api_strings;

const
  versioninfo = 'r1.05/ari.pikivirta@kolumbus.fi';

procedure TAPI_valueholder.dummys(s: string); begin end;
procedure TAPI_valueholder.dummyi(i: integer); begin end;
procedure TAPI_valueholder.dummyb(b: boolean); begin end;

// -----------------------------------------------------------------------------
constructor tAPI_valueholder.create(aowner:tcomponent);
begin
  inherited create(aowner);
  fversion:=versioninfo;
  fholder:=tstringlist.Create;
  fmodified:=false;
  fholder.Clear;
  fholder.sorted:=true;
  fgroupindex:=-1;
  fitemindex:=-1;
end;

// -----------------------------------------------------------------------------
destructor tAPI_valueholder.destroy;
var
  i: integer;
begin
  for i:=0 to fholder.Count-1 do
    fholder.Objects[i].Free;
  fholder.Free;
  inherited destroy;
end;

// -----------------------------------------------------------------------------
procedure tAPI_valueholder.Clear;
var
  i: integer;
begin
  for i:=0 to fholder.Count-1 do
    fholder.Objects[i].Free;
  fholder.clear;
  fitemindex:=-1;
  fgroupindex:=-1;
end;

// -----------------------------------------------------------------------------
function taPI_valueholder.Save(fname:string): boolean;
var
  f: textfile;
  i: integer;
  j: integer;
begin
  result:= false;
  {$i-}
  assignfile(f, fname);
  rewrite(f);
  {$i+}
  if ioresult=0 then
  try
    writeln(f, fholder.count);
    for i:=0 to fholder.count-1 do
    begin
      writeln(f, string(encode64(ansistring(fholder.strings[i]))) );
      writeln(f, tstringlist(fholder.Objects[i]).count);
      for j:=0 to tstringlist(fholder.objects[i]).count-1 do
        writeln(f, string(encode64(ansistring(tstringlist(fholder.objects[i]).strings[j]))) );
    end;
    result:=true;
    fmodified:=false;
  finally
    closefile(f);
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Load(fname:string): boolean;
var
  f: textfile;
  i: integer;
  grpcount: integer;
  j: integer;
  itmcount: integer;
  s: string;
begin
  result:=falsE;
  {$i-}
  assignfile(f,fname);
  reset(f);
  {$i+}
  if ioresult=0 then
  try
    clear;
    readln(f,s);
    grpcount:= strtoint(s);
    for i:=0 to grpcount-1 do
    begin
      readln(f, s);
      if group_add( string(decode64(ansistring(s))) ) then
      begin
        readln(f, s);
        itmcount:= strtoint(s);
        for j:=0 to itmcount-1 do
        begin
          readln(f, s);
          item_add(group, string(decode64(ansistring(s))) );
        end;
      end;
    end;
    result:=true;
    fmodified:=false;
  finally
    closefile(f);
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_Add(GroupName:string): boolean;
var
  index: integer;
begin
  result:=false;
  // check if group already exists
  if group_locate(groupname) then exit;
  // create group and stringlist object
  fholder.Add(Groupname);
  index:=fholder.Count-1;
  fholder.Objects[index]:=tstringlist.Create;
  tstringlist(fholder.Objects[index]).Clear;
  tstringlist(fholder.Objects[index]).Sorted:=true;
  fgroupindex:=index; // update current position
  fitemindex:=-1; // is empty
  fmodified:=true;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_Locate(GroupName:string): boolean;
var
  index: integer;
  i: integer;
begin
  result:=false;
  index:=-1;
  for i:=0 to fholder.Count-1 do
    if fholder[i]=groupname then
    begin
      index:=i;
      break;
    end;
  if index<0 then exit;
  fgroupindex:=index;
  if fitemindex>tstringlist(fholder.objects[index]).Count-1 then
    fitemindex:=tstringlist(fholder.objects[index]).count-1;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_Count: integer;
begin
  result:=fholder.count;
end;

// -----------------------------------------------------------------------------
procedure TAPI_valueholder.Grouplist(List: tstrings);
var
  index: integer;
begin
  list.Clear;
  for index:=0 to fholder.Count-1 do
    list.add(fholder.strings[index]);
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group: string;
begin
  result:='';
  if fgroupindex<0 then exit;
  if fgroupindex>fholder.count-1 then exit;
  result:=fholder[fgroupindex];
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_Delete(GroupName:string): boolean;
var
  index: integer;
begin
  result:=false;

  if group_locate(groupname) then
  begin
    index:=fgroupindex;
    if assigned(fholder.objects[index]) then    // if sublist is assigned
      fholder.Objects[index].Free;              // free it
    fholder.Delete(index);                      // remove this group

    // check if current groupindex is the one deleted
    if fgroupindex>-1 then
    begin
      if fitemindex>tstringlist(fholder.objects[fgroupindex]).Count-1 then
        fitemindex:=tstringlist(fholder.objects[fgroupindex]).count-1;
    end else
      fitemindex:=-1;

    fmodified:=true;
    result:=true;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_First: boolean;
begin
  result:=false;
  if fholder.count<1 then exit;
  fgroupindex:=0;
  if fitemindex>fholder.count-1
    then fitemindex:=fholder.Count-1;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_Next: boolean;
begin
  result:=false;
  if fgroupindex+1>fholder.count-1 then exit;
  if fholder.count<1 then exit;
  fgroupindex:=fgroupindex+1;
  if tstringlist(fholder.Objects[fgroupindex]).count-1>fitemindex
    then fitemindeX:=tstringlist(fholder.objects[fgroupindex]).Count-1;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_Previous: boolean;
begin
  result:=false;
  if fholder.count<1 then exit;
  if fgroupindex-1<0 then exit;
  fgroupindex:=fgroupindex-1;
  if tstringlist(fholder.Objects[fgroupindex]).count-1>fitemindex
    then fitemindeX:=tstringlist(fholder.objects[fgroupindex]).Count-1;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_Last: boolean;
begin
  result:=false;
  if fholder.count<0 then exit;
  fgroupindex:=fholder.Count-1;
  if tstringlist(fholder.Objects[fgroupindex]).count-1>fitemindex
    then fitemindex:=tstringlist(fholder.objects[fgroupindex]).Count-1;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Group_Eof: boolean;
begin
  result:=false;
  if fgroupindex>=fholder.Count-1 then
    result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Add(Group, Value:string): boolean;
begin
  result:=false;
  if not group_locate(group) then  exit;
  if item_locate(group, value) then exit;
  tstringlist(fholder.objects[fgroupindex]).add(value);
  fitemindex:=tstringlist(fholder.objects[fgroupindex]).Count-1;
  fmodified:=true;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Locate(Group,Name:string): boolean;
var
  i: integer;
begin
  result:=false;
  if not group_locate( group ) then exit;
  if tstringlist(fholder.objects[fgroupindex]).count<1 then exit;
  for i:=0 to tstringlist(fholder.objects[fgroupindex]).Count-1 do
    if (name=tstringlist(fholder.objects[fgroupindex]).names[i]) or
      (name=tstringlist(fholder.objects[fgroupindex])[i]) then
    begin
      fitemindex:=i;
      result:=true;
      break;
    end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Count(Group: string): integer;
begin
  result:=-1;
  if not Group_locate(group) then exit;
  result:=tstringlist(fholder.Objects[fgroupindex]).Count;
end;

// -----------------------------------------------------------------------------
procedure tAPI_valueholder.Itemlist(Group:string; var list: tstringlist);
var
  index: integer;
begin
  list.Clear;
  if not group_locate( group ) then  exit;
  for index:=0 to tstringlist(fholder.objects[fgroupindex]).Count-1  do
    list.add(tstringlist(fholder.objects[fgroupindex]).Strings[index]);
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item: string;
begin
  result:='';
  if not group_locate(Group) then exit;
  if fitemindex<0 then exit;
  if fitemindex>tstringlist(fholder.Objects[fgroupindex]).count-1 then exit;
  result:=tstringlist(fholder.Objects[fgroupindex]).Strings[fitemindex];
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.ItemName: string;
begin
  result:='';
  if fholder.count<1 then exit;
  if not group_locate( group )then exit;
  if fitemindex<0 then exit;
  if fitemindex>tstringlist(fholder.Objects[fgroupindex]).count-1 then exit;
  result:=tstringlist(fholder.Objects[fgroupindex]).Names[fitemindex];
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.ItemValue: string;
var
  _name: string;
begin
  result:='';
  if fholder.count<1 then exit;
  if not group_locate( group ) then exit;
  if fitemindex<0 then exit;
  if fitemindex>tstringlist(fholder.Objects[fgroupindex]).count-1 then exit;
  _name:=tstringlist(fholder.Objects[fgroupindex]).Names[fitemindex];
  result:=tstringlist(fholder.Objects[fgroupindex]).values[_name];
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Delete(Group, Name:string): boolean;
var
  index: integer;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    index:=fitemindex;
    tstringlist(fholder.Objects[fgroupindex]).Delete(index);
    if fitemindex>tstringlist(fholder.objects[fgroupindex]).Count-1 then
      fitemindex:=tstringlist(fholder.objects[fgroupindex]).count-1;
    fmodified:=true;
    result:=true;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_First(Group:string): boolean;
begin
  result:=false;
  if not group_locate( group ) then exit;
  if tstringlist(fholder.Objects[fgroupindex]).Count<1 then
  begin
    fitemindex:=-1;
    exit;
  end;
  fitemindex:=0;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Next(Group:string): boolean;
begin
  result:=false;
  if not group_locate( group ) then exit;
  if fitemindex+1>tstringlist(fholder.objects[fgroupindex]).count-1 then exit;
  fitemindeX:=fitemindex+1;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Previous(Group:string): boolean;
begin
  result:=false;
  if not group_locate( group ) then exit;
  if fitemindex<1 then exit;
  fitemindex:=fitemindex-1;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Last(Group:string): boolean;
begin
  result:=false;
  if not group_locate ( group ) then exit;
  if tstringlist(fholder.objects[fgroupindex]).count<1 then exit;
  fitemindex:=tstringlist(fholder.objects[fgroupindex]).count-1;
  result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Eof(Group:string): boolean;
begin
  result:=false;
  if not group_locate( group ) then exit;
  if fitemindex>=tstringlist(fholder.Objects[fgroupindex]).Count-1 then
    result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Get(Group, Name:string; var value:boolean): Boolean;
var
  s: string;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    try
      s:=tstringlist(fholder.Objects[fgroupindex]).Values[name];
      value:=strtobool(s);
      result:=true;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Get(Group, Name:string; var value:double): boolean;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    try
      value:=strtofloat( tstringlist(fholder.objects[fgroupindex]).values[name] );
      result:=true;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Get(Group, Name:string; var value:string): boolean;
var
  s: string;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    try
      s:=tstringlist(fholder.objects[fgroupindex]).values[name];
      value:=s;
      result:=true;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Get(Group, Name:string; var value:integer): boolean;
var
  s: string;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    try
      s:=tstringlist(fholder.objects[fgroupindex]).values[name];
      value:=strtoint(s);
      result:=true;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Set(Group, Name:string; value:boolean): boolean;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    try
      tstringlist(fholder.objects[fgroupindex]).values[name]:=booltostr(value);
      fmodified:=true;
      result:=true;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Set(Group, Name:string; value:string): boolean;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    try
      tstringlist(fholder.objects[fgroupindex]).values[name]:=value;
      fmodified:=true;
      result:=true;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Set(Group, Name:string; value:double): boolean;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    try
      tstringlist(fholder.objects[fgroupindex]).values[name]:= floattostr(value);
      fmodified:=true;
      result:=true;
    except
    end;
  end;
end;


// -----------------------------------------------------------------------------

function tAPI_valueholder.Item_Set(Group, Name:string; value:integer): boolean;
begin
  result:=false;
  if item_locate(group, name) then
  begin
    try
      tstringlist(fholder.objects[fgroupindex]).values[name]:=inttostr(value);
      fmodified:=True;
      result:=true;
    except
    end;
  end;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Add(Group, Name:string; value:boolean): boolean;
begin
  result:=false;
  if item_add(group,name+'='+booltostr(value)) then
    result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Add(Group, Name:string; value:string): boolean;
begin
  result:=false;
  if item_add(group,name+'='+value) then
    result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Add(Group, Name:string; value:double): boolean;
begin
  result:=false;
  if item_add(group,name+'='+floattostr(value)) then
    result:=true;
end;

// -----------------------------------------------------------------------------
function tAPI_valueholder.Item_Add(Group, Name:string; value:integer): boolean;
begin
  result:=false;
  if item_add(group,name+'='+inttostr(value)) then
    result:=true;
end;

// -----------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_valueholder]);
end;

end.

