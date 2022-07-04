///////////////////////////////////////
//        Data Master 2003           //
//   Copyright (c) 1993-2003 RRR     //
///////////////////////////////////////

unit DMUserAction;

interface

uses
  SysUtils, Classes, ActnList;

{$I ActPrv.inc}

type
  TUserActionID=string[64];
  TUserActionProvider=string[48];
  TProviders=1..16;
  TProviderEntry=record
    Name: shortstring; // user-friendly name
    GUID: shortstring;
    Provider: IDMActionProvider;
  end;

  TDMUserAction = class(TCustomAction)
  private
    { Private declarations }
    FID: TUserActionID;
    FProvider: TUserActionProvider;
    procedure SetProvider(Prv: TUserActionProvider);
    procedure SetID(UAI: TUserActionID);
    procedure LoadProps;
  protected
    { Protected declarations }
    procedure Loaded; override;
  public
    { Public declarations }
    function HandlesTarget(Target: TObject): Boolean; override;
    procedure UpdateTarget(Target: TObject); override;
    procedure ExecuteTarget(Target: TObject); override;
  published
    { Published declarations }
    property ImageIndex;
    property ShortCut;
    property Enabled;
    property Checked;
    property Visible;
    property ID: TUserActionID read FID write SetID;
    property Provider: TUserActionProvider read FProvider write SetProvider;
  end;

var
  Providers: array[TProviders] of TProviderEntry;
  ProvidersInitialized: boolean=true;

procedure Register;
procedure InitProviders;
procedure DoneProviders;

resourcestring
  strProviderNotFoundCaption='Invalid';
  strProviderNotFoundHint='Error|Action provider not found!';
  strActionNotFoundCaption='Unavailable';
  strActionNotFoundHint='Error|Action properties unavailable';
  strProviderNotRegistered='Provider %s not registered!';
  
implementation

uses ComObj{CreateComObject}, ActiveX{Ole(Un)Initialize}, Dialogs{ShowMessage};

procedure Register;
begin
  RegisterActions('DM2003', [TDMUserAction], nil);
end;

procedure InitProviders;
var
  PI: TProviders;
  punk: IUnknown;
  pci: ICatInformation;
  peg: IEnumGUID; 
  Fetched: Cardinal;
  G: TGUID;
  N: POleStr;
  S: string;
begin
  OleInitialize(nil);
  punk:=CreateComObject(CLSID_StdComponentCategoryMgr);
  pci:=punk as ICatInformation; // implicit QueryInterface
  OleCheck(pci.EnumClassesOfCategories(1, @CATID_DMActionProvider, 0, nil, peg));
  PI:=Low(TProviders);
  while peg.Next(1, G, Fetched)=S_OK do
  begin
    if PI>High(TProviders) then Break;
    Providers[PI].GUID:=GuidToString(G);
    OleCheck(OleRegGetUserType(G, USERCLASSTYPE_FULL, N));
    S:=N; // implicit conversion widestring -> string
    Providers[PI].Name:=S;
    Providers[PI].Provider:=CreateComObject(G) as IDMActionProvider;
    PI:=Succ(PI);
  end;
end;

procedure DoneProviders;
var
  PI: TProviders;
begin
  for PI:=low(TProviders) to high(TProviders) do
  begin
    if Assigned(Providers[PI].Provider)
    then Providers[PI].Provider.CloseProvider; 
    Finalize(Providers[PI].Provider);
    Providers[PI].GUID:='';
    Providers[PI].Name:='';
  end;
  OleUninitialize;
end;

{ TDMUserAction }

procedure TDMUserAction.SetProvider(Prv: TUserActionProvider);
var
  I: TProviders;
  Found: boolean;
begin
  if FProvider<>Prv then
  begin
    FProvider:=Prv;
    if csDesigning in ComponentState then  // check provider registration
    begin
      Found:=false;
      for I:=low(TProviders) to high(TProviders) do
      if (FProvider=Providers[I].GUID) or (FProvider=Providers[I].Name) then
      begin
        Found:=true;
        Break;
      end;
      if (not Found) and ProvidersInitialized
      then MessageDlg(Format(strProviderNotRegistered, 
        [FProvider]), mtError, [mbCancel], 0);
    end;
  end;
end;

procedure TDMUserAction.SetID(UAI: TUserActionID);
begin
  if FID<>UAI then
  begin
    FID:=UAI;
    Caption:=strProviderNotFoundCaption;
    Hint:=strProviderNotFoundHint;
    HelpKeyword:='';
    if not (csReading in ComponentState) 
    then LoadProps;
  end;
end;

{Since TDMUserAction is universal, all target objects are always valid}
function TDMUserAction.HandlesTarget(Target: TObject): Boolean;
begin
  Result:=true; 
end; 

{Notice: provider checked by both Name (user friendly) and GUID!}
procedure TDMUserAction.ExecuteTarget(Target: TObject);
var
 I: TProviders;
begin
  if FProvider<>'' then
  for I:=low(TProviders) to high(TProviders) do
  if (FProvider=Providers[I].GUID) or (FProvider=Providers[I].Name) then
  begin
    OleCheck(Providers[I].Provider.ExecuteAction(FID)); 
    Break;
  end;
end;

{Also search Provider by Name and GUID. If no provider found, action 
is disabled, unchecked and visible. If provider returns dmuaUndefined,
use default Delphi behavior}
procedure TDMUserAction.UpdateTarget(Target: TObject);
var
 I, S: integer;
 E, C, V: boolean;
begin
  E:=false;
  C:=false;
  V:=true;
  if FProvider<>'' then
  for I:=low(TProviders) to high(TProviders) do
  if (FProvider=Providers[I].GUID) or (FProvider=Providers[I].Name) then
  begin
    S:=0;
    if Providers[I].Provider.UpdateAction(FID, S)=S_OK then
    begin
      if S=dmuaUndefined
      then Exit;
      E:=S and dmuaEnabled<>0;
      C:=S and dmuaChecked<>0;
      V:=S and dmuaVisible<>0;
    end;
    Break;
  end;
  Enabled:=E;
  Checked:=C;
  Visible:=V;
end;

{Caption, Hint and HelpKeyword must be loaded after reading FProvider and FID}
procedure TDMUserAction.Loaded;
begin
  inherited;
  LoadProps;
end;

{load Caption/Hint/HelpKeyword from the provider}
procedure TDMUserAction.LoadProps;
var
  I: TProviders;
  S1, S2, S3: WideString;
begin
  if FProvider='' then Exit; 
  for I:=low(TProviders) to high(TProviders) do
  if (FProvider=Providers[I].GUID) or (FProvider=Providers[I].Name) then
  begin
    if Providers[I].Provider.GetActionProperties(FID, S1, S2, S3)=S_OK then
    begin
      Caption:=S1;
      Hint:=S2;
      HelpKeyword:=S3;
    end else
    begin
      Caption:=strActionNotFoundCaption;
      Hint:=strActionNotFoundHint;
      HelpKeyword:='';
    end;
    Break;
  end;
end;

end.
