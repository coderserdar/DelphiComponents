///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMUserActionEditor;

{$B-}

interface

procedure Register;

implementation

uses
  Classes{TGetStrProc}, DesignIntf, DesignEditors, DMUserAction, Registry;

type
  TUserActionProviderProperty=class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  TUserActionIDProperty=class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TUserActionProvider),
    TDMUserAction, 'Provider', TUserActionProviderProperty);
  RegisterPropertyEditor(TypeInfo(TUserActionID),
    TDMUserAction, 'ID', TUserActionIDProperty);
end;

{ TUserActionProviderProperty }

function TUserActionProviderProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList];
end;

{Converts GUID remembered in property to the readable provider name}
function TUserActionProviderProperty.GetValue: string;
var
  I: TProviders;
  S: string;
begin
  Result:='';
  S:=inherited GetValue;
  for I:=low(TProviders) to high(TProviders) do
  if Providers[I].GUID=S then
  begin
    Result:=Providers[I].Name;
    Break;
  end;
  if Result='' then Result:=S;
end;

{Makes the list of provider names}
procedure TUserActionProviderProperty.GetValues(Proc: TGetStrProc);
var
  I: TProviders;
begin
  for I:=low(TProviders) to high(TProviders) do
  if Providers[I].Name<>'' // NOTICE: nonzero Name means non-empty!!!
  then Proc(Providers[I].Name);
end;

{Forces TDMUserAction always remember GUID in DFM}
procedure TUserActionProviderProperty.SetValue(const Value: string);
var
  I: integer;
  S: string;
begin
  S:='';
  for I:=low(TProviders) to high(TProviders) do
  if Providers[I].Name=Value then
  begin
    S:=Providers[I].GUID;
    Break;
  end;
  if S='' then S:=Value; 
  inherited SetValue(S);
end;

{ TUserActionIDProperty }

function TUserActionIDProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList];
end;

procedure TUserActionIDProperty.GetValues(Proc: TGetStrProc);
var
  I, PrvIdx, ActCount: integer;
  S: shortstring;
  SS: WideString;
begin
  // search provider
  PrvIdx:=-1;
  S:=(GetComponent(0) as TDMUserAction).Provider;
  if S='' then Exit; // empty provider!
  for I:=low(TProviders) to high(TProviders) do
  if (S=Providers[I].Name) or (S=Providers[I].GUID) then
  begin
    PrvIdx:=I;
    Break;
  end;
  if PrvIdx=-1 then Exit; // nothing found!
  // build action list}
  Assert(Providers[PrvIdx].Provider.ActionCount(ActCount)=S_OK);
  for I:=1 to ActCount do
  begin
    Assert(Providers[PrvIdx].Provider.GetActionID(I, SS)=S_OK);
    Proc(SS);
  end;
end;

// enable provider and action lists at design time
const
  LibraryRegKey='Software\RRR\DM2003\Library';
  InitializeProviders='InitializeProviders';

initialization
  with TRegistry.Create do
  try
    if OpenKeyReadOnly(LibraryRegKey) and ValueExists(InitializeProviders)
    then ProvidersInitialized:=ReadBOOL(InitializeProviders);
  finally
    Free;
  end;
  if ProvidersInitialized
  then InitProviders;
finalization
  if ProvidersInitialized
  then DoneProviders;
end.
