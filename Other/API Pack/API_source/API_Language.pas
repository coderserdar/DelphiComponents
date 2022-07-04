unit API_Language;

//------------------------------------------------------------------------------
// API_Lanugage
//------------------------------------------------------------------------------
// r1.01, 15102009, ari pikivirta
//  * will check also sub components when retrieving texts
//------------------------------------------------------------------------------

interface

uses
  Windows, SysUtils, Forms, Classes, Controls, typInfo, API_Base;

type
  TAPI_Language = class(TAPI_Custom_Component)
  private
    fList: tstringlist;
    procedure RetrieveSubComponents(Const c: TComponent);
    procedure ManageSubComponents(var C: TComponent);
  protected
  public
    constructor Create(AOWner: Tcomponent); override;
    destructor Destroy; override;
    function OpenLanguage(const Filename: string): boolean;
    function ApplyTexts: boolean;
    function RetrieveTexts: boolean;
    function SaveLanguage(const Filename: string): boolean;
  published
  end;

function getProp(comp: TComponent; prop: string): string;
procedure setProp(comp: TComponent; prop, value: string);
function HasProperty(comp: TComponent; prop: string): boolean;
procedure Register;

implementation

{$include '..\API_source\inc\CompilerVersions.INC'}

//------------------------------------------------------------------------------
constructor TAPI_Language.Create(AOWner: Tcomponent);
begin
  inherited Create(AOwner);
  Version:= 'r1.01/ari.pikivirta-at-kolumbus.fi';
  fList:= Tstringlist.create;
end;

//------------------------------------------------------------------------------
destructor TAPI_Language.Destroy;
begin
  fList.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------
function TAPI_Language.OpenLanguage(const Filename: string): boolean;
begin
  try
    flist.clear;
    flist.LoadFromFile(filename);
    result:= true;
  except
    result:= false;
  end;
end;

//------------------------------------------------------------------------------
// Backs up the prop property value of the comp component
function getProp(comp: TComponent; prop: string): string;
var
  ppi: PPropInfo;
begin
	ppi:= getPropInfo(comp.classInfo,prop);
  if ppi<>nil then result:= getStrProp(comp,ppi)
    else	result:='';
end;

//------------------------------------------------------------------------------
// Assign the value value to prop property of comp component
procedure setProp(comp: TComponent; prop, value: string);
var
	ppi: PPropInfo;
begin
	if value<>'' then
  begin
   	ppi:= getPropInfo(comp.classInfo,prop);
    if ppi<>nil then setStrProp(comp,ppi,value);
  end;
end;

//------------------------------------------------------------------------------
// True if prop property exists for comp component
function HasProperty(comp: TComponent; prop: string): boolean;
begin
	result:= (getPropInfo(comp.classInfo,prop)<>nil) and (comp.name<>'');
end;

//------------------------------------------------------------------------------
function IsNumeric(s: ansistring): boolean;
var
  p: integer;
begin
  result:= true;
  for p:=1 to length(s) do
    {$ifdef DELPHI2009UP}
    if not (charinset(s[p], ['0'..'9'])) then
    {$else}
    if not (s[p] in ['0'..'9']) then
    {$endif}
    begin
      result:= false;
      break;
    end;
end;

//------------------------------------------------------------------------------
procedure TAPI_Language.RetrieveSubComponents(Const c: TComponent);
var
  j: integer;
  cc: tcomponent;
  n: string;
begin
  // look for sub components
  for j:= 0 to (c.componentCount-1) do
  begin
    cc:= c.Components[j];
    n:= cc.name;
    // get components "Title"
    if HasProperty(cc,'Title')
      and (n<>getProp(cc,'Title'))
      and (getProp(cc, 'Title')<>'-')
      and (not IsNumeric(ansistring(getProp(cc,'Title'))))
      and (getProp(cc, 'Title')<>'') then
      flist.add(c.Name+'.'+cc.Name+'.Title='+getProp(cc,'Title'));
    // get components "caption"
    if HasProperty(cc,'Caption')
      and (n<>getProp(cc,'Caption'))
      and (getProp(cc, 'Caption')<>'-')
      and (not IsNumeric(ansistring(getProp(cc,'Caption'))))
      and (getProp(cc, 'Caption')<>'') then
      flist.add(c.Name+'.'+cc.Name+'.Caption='+getProp(cc,'Caption'));
    // get components "text"
    if HasProperty(cc,'Text')
      and (n<>getProp(cc,'Text'))
      and (getProp(cc, 'Text')<>'-')
      and (not IsNumeric(ansistring(getProp(cc,'Text'))))
      and (getProp(cc, 'Text')<>'') then
      flist.add(c.Name+'.'+cc.Name+'.Text='+getProp(cc,'Text'));
    // get component's hint text
    if HasProperty(cc,'Hint')
      and (getProp(cc,'Hint')<>'')
      and (getProp(cc,'Hint')<>'-') then
      flist.add(c.name+'.'+cc.Name+'.Hint='+getProp(cc,'Hint'));
    // look for more sub components
    if (cc.ComponentCount>0) then
    begin
      RetrieveSubComponents(cc);
    end;
  end; // sub components
end;

//------------------------------------------------------------------------------
function TAPI_Language.RetrieveTexts: boolean;
var
  i: integer;
  c: tcomponent;
begin
  flist.Clear;
  for i:= 0 to (application.ComponentCount-1) do
  begin
    c:= application.Components[i];
    RetrieveSubComponents(c);
  end; // application's components
  result:= TRUE;
end;

//------------------------------------------------------------------------------
procedure TAPI_Language.ManageSubComponents(var C: TComponent);
var
  j, i2: integer;
  cc: tcomponent;
begin
  // look for sub components
  for j:=0 to (c.componentCount-1) do
  begin
    cc:= c.Components[j];
    // set components "Title"
    i2:= flist.indexofname(c.Name+'.'+cc.Name+'.Title');
    if HasProperty(cc,'Title') and (i2>-1) then setProp(cc, 'Title', flist.valuefromindex[i2]);
    // set components "caption"
    i2:= flist.indexofname(c.Name+'.'+cc.Name+'.Caption');
    if HasProperty(cc,'Caption') and (i2>-1) then setProp(cc, 'Caption', flist.valuefromindex[i2]);
    // set components "text"
    i2:= flist.indexofname(c.Name+'.'+cc.Name+'.Text');
    if HasProperty(cc,'Text') and (i2>-1) then setProp(cc, 'Text', flist.valuefromindex[i2]);
    // set component's hint text
    i2:= flist.indexofname(c.name+'.'+cc.Name+'.Hint');
    if HasProperty(cc,'Hint') and (i2>-1) then setProp(cc, 'Hint', flist.valuefromindex[i2]);
    // see if there's subs on this
    if cc.componentcount>0 then ManageSubComponents(cc);
  end; // sub components
end;

//------------------------------------------------------------------------------
function TAPI_Language.ApplyTexts: boolean;
var
  i: integer;
  c: tcomponent;
begin
  // go trough application components
  for i:=0 to (application.ComponentCount-1) do
  begin
    c:= application.Components[i];
    ManageSubComponents(c);
  end; // application's components
  result:= TRUE;
end;

//------------------------------------------------------------------------------
function TAPI_Language.SaveLanguage(const Filename: string): boolean;
begin
  try
    flist.SaveToFile(filename);
    result:= true;
  except
    result:= false;
  end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponents('API Misc', [TAPI_Language]);
end;

end.
