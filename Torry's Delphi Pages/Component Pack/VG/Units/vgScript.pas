{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Scripting                                     }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{         TObjectProxy, TEnumeratorObject idea and      }
{         portions copyright (c) 1999 Tolik Tentser     }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgScript;

interface
uses Windows, ActiveX, SysUtils, TypInfo, Classes, vgSystem, vgTools, Variants;

type
{ IQueryObject }
  IQueryObject = interface
  ['{C3AD7A80-AF9D-11D3-B6B3-4854E828D2EB}']
    function GetObject: TObject;
  end;

  PPVariantList = ^TPVariantList;
  TPVariantList = array[0..0] of PVariant;

  TInvokeFlag = (flMethod, flPropGet, flPropPut, flPropPutRef);
  TInvokeFlags = set of TInvokeFlag;

{ TInvokeArguments }
  TInvokeArguments = class(TObject)
  private
    FParams: PDispParams;
    FFlags: TInvokeFlags;
    function GetArgCount: Integer;
    function GetArg(Index: Integer): Variant;
    procedure SetArg(Index: Integer; const Value: Variant);
  public
    constructor Create(AParams: PDispParams; AFlags: TInvokeFlags);
    property Count: Integer read GetArgCount;
    property Args[Index: Integer]: Variant read GetArg write SetArg; default;
    property Flags: TInvokeFlags read FFlags;
  end;

  TObjectProxy = class;
  TObjectProxyMethod = class;

  TObjectProxyMethodEvent = function(AObjectProxy: TObjectProxy;
    Args: TInvokeArguments;  VarResult: POleVariant): HResult of object;

{ TObjectProxyServer }
  TObjectProxyServer = class(TObject)
  private
    FMethods: TStrings;
    FProxies, FProxyMethods: TList;
    FEnumClasses, FComponentClasses: TClassList;
  protected
    procedure InsertMethod(AMethod: TObjectProxyMethod);
    procedure RemoveMethod(AMethod: TObjectProxyMethod; RemoveName: Boolean);
    procedure InsertProxy(AObjectProxy: TObjectProxy);
    procedure RemoveProxy(AObjectProxy: TObjectProxy);
  public
    constructor Create;
    destructor Destroy; override;
    { Methods routines }
    procedure RegisterMethods; virtual;
    procedure RegisterMethod(AClass: TClass; const AMethodName: string;
      ADispID: TDispID; AMethod: TObjectProxyMethodEvent);
    procedure UnregisterMethod(AClass: TClass; const AMethodName: string);
    function FindMethodByDispID(AClass: TClass; DispID: TDispID): TObjectProxyMethod;
    function FindMethodByName(AClass: TClass; const AMethodName: string): TObjectProxyMethod;
    { Proxy interface }
    function CreateObjectProxy(AObject: TObject): TObjectProxy; virtual;
    function GetObjectProxy(AObject: TObject; var AObjectProxy: OleVariant): HResult;
    function CreateEnumerator(AObjectProxy: TObjectProxy): TEnumeratorObject; virtual;
    function GetIDOfMethod(AObjectProxy: TObjectProxy; const AMethodName: string;
      var DispID: TDispID): Boolean; virtual;
    function GetIDOfProperty(AObjectProxy: TObjectProxy; const APropName: string;
      var DispID: TDispID): Boolean; virtual;
    function GetProperty(AObjectProxy: TObjectProxy; PropInfo: PPropInfo; Args: TInvokeArguments;
      VarResult: POleVariant): HResult; virtual;
    function SetProperty(AObjectProxy: TObjectProxy; PropInfo: PPropInfo;
      Args: TInvokeArguments): HResult; virtual;
    { Enumerator classes }
    property EnumClasses: TClassList read FEnumClasses;
    { Components available for creation }
    property ComponentClasses: TClassList read FComponentClasses;
  end;

  TObjectProxyServerClass = class of TObjectProxyServer;

{ TObjectProxyMethod }
  TObjectProxyMethod = class
  private
    FName: string;
    FMethod: TObjectProxyMethodEvent;
    FDispID: TDispID;
    FProxyServer: TObjectProxyServer;
  public
    constructor Create(AProxyServer: TObjectProxyServer; const AName: string;
      ADispID: TDispID; AMethod: TObjectProxyMethodEvent);
    destructor Destroy; override;
    function Invoke(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    property DispID: TDispID read FDispID;
    property Name: string read FName;
    property Method: TObjectProxyMethodEvent read FMethod;
    property ProxyServer: TObjectProxyServer read FProxyServer;
  end;

{ TObjectProxyEnumerator }
  TObjectProxyEnumerator = class(TEnumeratorObject)
  private
    FObjectProxy: TObjectProxy;
  public
    constructor Create(AObjectProxy: TObjectProxy);
    destructor Destroy; override;
    function CreateEnumerator: TEnumeratorObject; override;
    property ObjectProxy: TObjectProxy read FObjectProxy;
  end;

  TObjectProxyEnumeratorClass = class of TObjectProxyEnumerator;

{ TStringsProxyEnumerator }
  TStringsProxyEnumerator = class(TObjectProxyEnumerator)
  public
    function Fetch(Index: LongWord; var VarResult: OleVariant): HResult; override;
    function GetCount: LongWord; override;
  end;

{ TCollectionProxyEnumerator }
  TCollectionProxyEnumerator = class(TObjectProxyEnumerator)
  public
    function Fetch(Index: LongWord; var VarResult: OleVariant): HResult; override;
    function GetCount: LongWord; override;
  end;

{ TComponentProxyEnumerator }
  TComponentProxyEnumerator = class(TObjectProxyEnumerator)
  public
    function Fetch(Index: LongWord; var VarResult: OleVariant): HResult; override;
    function GetCount: LongWord; override;
  end;

{ TObjectProxy }
  TObjectProxy = class(TInterfacedObject, IDispatch, IQueryObject)
  private
    FObject: TObject;
    FProxyServer: TObjectProxyServer;
    FEnumerators: TList;
    procedure InsertEnumerator(AEnumerator: TObjectProxyEnumerator);
    procedure RemoveEnumerator(AEnumerator: TObjectProxyEnumerator);
  protected
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    { Helper methods }
    function InvokeDispID(DispID: TDispID; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    function GetIDOfName(const AName: string; var DispID: TDispID): Boolean;
  public
    constructor Create(AObject: TObject; AProxyServer: TObjectProxyServer);
    destructor Destroy; override;
    { IQueryObject}
    function GetObject: TObject;
    { PropInfos routines }
    function GetPropInfo(const PropName: string): PPropInfo;
    function GetProperty(PropInfo: PPropInfo; Args: TInvokeArguments; VarResult: POleVariant): HResult;
    function SetProperty(PropInfo: PPropInfo; Args: TInvokeArguments): HResult;
    { Other methods }
    function GetObjectProxy(AObject: TObject; var AObjectProxy: OleVariant): HResult;
    function CreateEnumerator: TEnumeratorObject; virtual;
    property ProxyServer: TObjectProxyServer read FProxyServer;
  end;

{ CoTObjectProxyMethods }
  CoTObjectProxyMethods = class(TObject)
  public
    class procedure RegisterMethods(AProxyServer: TObjectProxyServer); virtual;
    { TObject methods }
    class function TObjectClassName(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TObjectIsClass(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TObjectHasMethod(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TObjectHasProperty(AObjectProxy: TObjectProxy;
      Args: TInvokeArguments; VarResult: POleVariant): HResult;
    class function TObjectIsEqualTo(AObjectProxy: TObjectProxy;
      Args: TInvokeArguments; VarResult: POleVariant): HResult;
    class function TObjectFree(AObjectProxy: TObjectProxy;
      Args: TInvokeArguments; VarResult: POleVariant): HResult;
  end;

{ CoTPersistentProxyMethods }
  CoTPersistentProxyMethods = class(CoTObjectProxyMethods)
  public
    class procedure RegisterMethods(AProxyServer: TObjectProxyServer); override;
    { TPersistent methods }
    class function TPersistentAssign(AObjectProxy: TObjectProxy;
      Args: TInvokeArguments; VarResult: POleVariant): HResult;
  end;

{ CoTStringsProxyMethods }
  CoTStringsProxyMethods = class(CoTPersistentProxyMethods)
  public
    class procedure RegisterMethods(AProxyServer: TObjectProxyServer); override;
    { TStrings }
    class function TStringsCount(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TStringsInsert(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TStringsAdd(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TStringsDelete(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TStringsIndexOf(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TStringsText(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TStringsCommaText(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TStringsStrings(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
  end;

{ CoTCollectionProxyMethods }
  CoTCollectionProxyMethods = class(CoTPersistentProxyMethods)
  public
    class procedure RegisterMethods(AProxyServer: TObjectProxyServer); override;
    { TCollection }
    class function TCollectionCount(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TCollectionItems(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TCollectionInsert(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TCollectionAdd(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TCollectionClear(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
  end;

{ CoTComponentProxyMethods }
  CoTComponentProxyMethods = class(CoTPersistentProxyMethods)
  public
    class procedure RegisterMethods(AProxyServer: TObjectProxyServer); override;
    { TComponent }
    class function TComponentComponentName(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TComponentComponentCount(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TComponentComponents(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TComponentFindComponent(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TComponentOwner(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TComponentCreate(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
  end;

{ TEventThunk }
  TEventThunk = class(TObject)
  public
    function GetMethod: TMethod; virtual;
  end;

  TNotifyEventThunkEvent = procedure(EventThunk: TEventThunk; Sender: TObject) of object;

{ TNotifyEventThunk }
  TNotifyEventThunk = class(TEventThunk)
  private
    FOnEvent: TNotifyEventThunkEvent;
  protected
    procedure DoEvent(Sender: TObject); virtual;
  public
    function GetMethod: TMethod; override;
    property OnEvent: TNotifyEventThunkEvent read FOnEvent write FOnEvent;
  end;

{ EScriptError }
  EScriptError = class(Exception)
  private
    FLine, FColumn: Integer;
  public
    constructor Create(const Msg: string; ALine, AColumn: Integer);
    property Line: Integer read FLine;
    property Column: Integer read FColumn;
  end;

{ TCustomScript }
  TCustomScript = class(TComponent)
  private
    FLines: TStrings;
    procedure SetLines(Value: TStrings);
  protected
    procedure LinesChanged(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Properties }
    property Lines: TStrings read FLines write SetLines;
  end;

  TDispatchMethod = class;

  TDispatchMethodInvokeEvent =  procedure (Method: TDispatchMethod; AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant; var ErrorCode: HResult) of object;

{ TDispatchMethods }
  TDispatchMethods = class(TNamedItemsCollection)
  private
    function GetItem(Index: Integer): TDispatchMethod;
  public
    function ProxyServer: TObjectProxyServer; virtual;
    procedure Update(Item: TCollectionItem); override;
    property Items[Index: Integer]: TDispatchMethod read GetItem; default;
  end;

{ TDispatchMethod }
  TDispatchMethod = class(TNamedCollectionItem)
  private
    FActive: Boolean;
    FDispID: TDispID;
    FOnInvoke: TDispatchMethodInvokeEvent;
    FClassNameSupport: string;
    function StoreClassNameSupport: Boolean;
    procedure SetActive(Value: Boolean);
    procedure SetDispID(Value: TDispID);
  protected
    function InternalInvoke(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult; virtual;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function Invoke(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    function ProxyServer: TObjectProxyServer;
  published
    property Active: Boolean read FActive write SetActive default True;
    property ClassNameSupport: string read FClassNameSupport write FClassNameSupport stored StoreClassNameSupport;
    property DispID: TDispID read FDispID write SetDispID;
    property OnInvoke: TDispatchMethodInvokeEvent read FOnInvoke write FOnInvoke;
  end;

{ TScriptDispatchMethods }
  TScriptDispatchMethods = class(TDispatchMethods)
  public
    function ProxyServer: TObjectProxyServer; override;
  end;

{ TCustomDispatchScript }
  TCustomDispatchScript = class(TCustomScript)
  private
    FMethods: TDispatchMethods;
    FProxyServer: TObjectProxyServer;
    function GetProxyServer: TObjectProxyServer;
    procedure SetMethods(Value: TDispatchMethods);
    function StoreMetods: Boolean;
  protected
    function CreateProxyServer: TObjectProxyServer; virtual;
    property ProxyServer: TObjectProxyServer read GetProxyServer;
    property Methods: TDispatchMethods read FMethods write SetMethods stored StoreMetods;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ Default proxy manager }
function ProxyServer: TObjectProxyServer;

{ Enumerators support }
procedure RegisterEnumerators(AProxyServer: TObjectProxyServer);

{ TObjectProxy utility routines }
procedure SetVarResult(VarResult: POleVariant; const Value: OleVariant);
function GetArgObject(const Value: Variant; var AObject: TObject): Boolean;

function PackMethod(Method: TMethod; TypeInfo: Pointer): OleVariant;
procedure UnpackMethod(const Value: OleVariant; var Method: TMethod; var TypeInfo: Pointer);

var
  ProxyServerClass: TObjectProxyServerClass = TObjectProxyServer;

const
{ Object proxy }
  DISPID_OBJECTPROXYMIN                = DISPID_VALUE;
  DISPID_OBJECTPROXYSYS                = 10000 - 1;
  DISPID_OBJECTPROXYMAX                = 20000 - 1;
  DISPID_OBJECTPROXUSER                = DISPID_OBJECTPROXYMAX + 1;

{ Method DispIDs }

{ TObject }
  DISPID_TObjectMin                    = DISPID_OBJECTPROXYMIN;
  DISPID_TObjectClassName              = DISPID_TObjectMin + 1;
  DISPID_TObjectIsClass                = DISPID_TObjectMin + 2;
  DISPID_TObjectHasMethod              = DISPID_TObjectMin + 3;
  DISPID_TObjectHasProperty            = DISPID_TObjectMin + 4;
  DISPID_TObjectIsEqualTo              = DISPID_TObjectMin + 5;
  DISPID_TObjectFree                   = DISPID_TObjectMin + 6;

{ TPersistent }
  DISPID_TPersistentMin                = DISPID_TObjectMin + 20;
  DISPID_TPersistentAssign             = DISPID_TPersistentMin + 1;

{ TStrings }
  DISPID_TStringsMin                   = DISPID_TPersistentMin + 20;
  DISPID_TStringsCount                 = DISPID_TStringsMin + 1;
  DISPID_TStringsInsert                = DISPID_TStringsMin + 2;
  DISPID_TStringsAdd                   = DISPID_TStringsMin + 3;
  DISPID_TStringsDelete                = DISPID_TStringsMin + 4;
  DISPID_TStringsIndexOf               = DISPID_TStringsMin + 5;
  DISPID_TStringsText                  = DISPID_TStringsMin + 6;
  DISPID_TStringsCommaText             = DISPID_TStringsMin + 7;
  DISPID_TStringsStrings               = DISPID_TStringsMin + 8;

{ TCollection }
  DISPID_TCollectionMin                = DISPID_TStringsMin + 20;
  DISPID_TCollectionCount              = DISPID_TCollectionMin + 1;
  DISPID_TCollectionItems              = DISPID_TCollectionMin + 2;
  DISPID_TCollectionInsert             = DISPID_TCollectionMin + 3;
  DISPID_TCollectionAdd                = DISPID_TCollectionMin + 4;
  DISPID_TCollectionClear              = DISPID_TCollectionMin + 5;

{ TComponent }
  DISPID_TComponentMin                 = DISPID_TCollectionMin + 20;
  DISPID_TComponentComponentName       = DISPID_TComponentMin + 1;
  DISPID_TComponentComponentCount      = DISPID_TComponentMin + 2;
  DISPID_TComponentComponents          = DISPID_TComponentMin + 3;
  DISPID_TComponentFindComponent       = DISPID_TComponentMin + 4;
  DISPID_TComponentOwner               = DISPID_TComponentMin + 5;
  DISPID_TComponentCreate              = DISPID_TComponentMin + 6;

{ Method names }

{ TObject }
  STObjectClassName                    = 'ClassName';
  STObjectIsClass                      = 'IsClass';
  STObjectHasMethod                    = 'HasMethod';
  STObjectHasProperty                  = 'HasProperty';
  STObjectIsEqualTo                    = 'IsEqualTo';
  STObjectFree                         = 'Free';

{TPersistent }
  STPersistentAssign                   = 'Assign';

{ TStrings }
  STStringsCount                       = 'Count';
  STStringsInsert                      = 'Insert';
  STStringsAdd                         = 'Add';
  STStringsDelete                      = 'Delete';
  STStringsIndexOf                     = 'IndexOf';
  STStringsText                        = 'Text';
  STStringsCommaText                   = 'CommaText';
  STStringsStrings                     = 'Strings';

{ TCollection }
  STCollectionCount                    = 'Count';
  STCollectionItems                    = 'Items';
  STCollectionInsert                   = 'Insert';
  STCollectionAdd                      = 'Add';
  STCollectionClear                    = 'Clear';

{ TComponent }
  STComponentComponentName             = 'ComponentName';
  STComponentComponentCount            = 'ComponentCount';
  STComponentComponents                = 'Components';
  STComponentFindComponent             = 'FindComponent';
  STComponentOwner                     = 'Owner';
  STComponentCreate                    = 'Create';

implementation
uses vgUtils;

var
  FProxyServer: TObject = nil;

function ProxyServer: TObjectProxyServer;
begin
  if not Assigned(FProxyServer) then
    FProxyServer := ProxyServerClass.Create;
  Result := TObjectProxyServer(FProxyServer);
end;

procedure RegisterEnumerators(AProxyServer: TObjectProxyServer);
begin
  AProxyServer.EnumClasses.RegisterClass(TStrings, TStringsProxyEnumerator, '', True);
  AProxyServer.EnumClasses.RegisterClass(TCollection, TCollectionProxyEnumerator, '', True);
  AProxyServer.EnumClasses.RegisterClass(TComponent, TComponentProxyEnumerator, '', True);
end;

procedure SetVarResult(VarResult: POleVariant; const Value: OleVariant);
begin
  if Assigned(VarResult) then
    VarResult^ := Value;
end;

function GetArgObject(const Value: Variant; var AObject: TObject): Boolean;
var
  Arg: Variant;
  QueryObject: IQueryObject;
begin
  if TVarData(Value).VType = (varVariant or varByRef) then
    Arg := PVariant(TVarData(Value).VPointer)^ else
    Arg := Value;
  if TVarData(Arg).VType in [varEmpty, varNull] then
  begin
    AObject := nil;
    Result := True;
  end else if TVarData(Arg).VType = varDispatch then
  begin
    if Assigned(IDispatch(Arg)) then
    begin
      QueryObject := IDispatch(Arg) as IQueryObject;
      AObject := QueryObject.GetObject;
    end else
      AObject := nil;
    Result := True;
  end else
    Result := False;
end;

function PackMethod(Method: TMethod; TypeInfo: Pointer): OleVariant;
begin
  if Assigned(Method.Code) then
    Result := VarArrayOf([Integer(Method.Code), Integer(Method.Data), Integer(TypeInfo)]) else
    Result := Unassigned;
end;

procedure UnpackMethod(const Value: OleVariant; var Method: TMethod; var TypeInfo: Pointer);
begin
  if not VarIsEmpty(Value) then
  begin
    Method.Code := TVarData(Value[0]).VPointer;
    Method.Data := TVarData(Value[1]).VPointer;
    TypeInfo := TVarData(Value[2]).VPointer;
  end else begin
    Method.Code := nil;
    Method.Data := nil;
    TypeInfo := nil;
  end;
end;

{ TInvokeArguments }
constructor TInvokeArguments.Create(AParams: PDispParams; AFlags: TInvokeFlags);
begin
  FParams := AParams;
  FFlags := AFlags;
end;

function TInvokeArguments.GetArgCount: Integer;
begin
  Result := FParams^.cArgs;
end;

function TInvokeArguments.GetArg(Index: Integer): Variant;
begin
  with FParams^ do
  begin
    if (Index < 0) or (Index >= cArgs) then
      ListError(Index);
    Result := Variant(rgvarg^[cArgs - 1 - Index])
  end;
end;

procedure TInvokeArguments.SetArg(Index: Integer; const Value: Variant);
begin
  with FParams^ do
  begin
    if (Index < 0) or (Index >= cArgs) then
      ListError(Index);
    Variant(rgvarg^[cArgs - 1 - Index]) := Value;
  end;
end;

{ TObjectProxyServer }
constructor TObjectProxyServer.Create;
begin
  inherited;
  FMethods := TStringList.Create;
  FEnumClasses := TClassList.Create;
  FComponentClasses := TClassList.Create;
  RegisterMethods;
end;

destructor TObjectProxyServer.Destroy;
begin
  while ListCount(FProxies) > 0 do
    RemoveProxy(FProxies.Last);
  ListDestroyAll(FProxyMethods);
  FEnumClasses.Free;
  FComponentClasses.Free;
  FMethods.Free;
  inherited;
end;

function TObjectProxyServer.CreateObjectProxy(AObject: TObject): TObjectProxy;
begin
  if Assigned(AObject) then
    Result := TObjectProxy.Create(AObject, Self) else
    Result := nil;
end;

function TObjectProxyServer.CreateEnumerator(AObjectProxy: TObjectProxy): TEnumeratorObject;
var
  AObject: TObject;
  AClass: TClass;
  I: Integer;
begin
  AObject := AObjectProxy.GetObject;
  I := FEnumClasses.IndexOfClass(AObject.ClassType, True);
  if I >= 0 then
  begin
    AClass := FEnumClasses.Items[I].Data;
    Result := TObjectProxyEnumeratorClass(AClass).Create(AObjectProxy);
  end else
    Result := nil;
end;

function TObjectProxyServer.GetObjectProxy(AObject: TObject; var AObjectProxy: OleVariant): HResult;
var
  ObjectProxy: TObjectProxy;
begin
  ObjectProxy := CreateObjectProxy(AObject);
  if Assigned(ObjectProxy) then
  begin
    AObjectProxy := ObjectProxy as IDispatch;
    Result := S_OK;
  end else
    Result := E_NOTIMPL;
end;

function TObjectProxyServer.GetIDOfMethod(AObjectProxy: TObjectProxy;
  const AMethodName: string; var DispID: TDispID): Boolean;
var
  Method: TObjectProxyMethod;
begin
  Method := FindMethodByName(AObjectProxy.GetObject.ClassType, AMethodName);
  if Assigned(Method) then
  begin
    DispID := Method.DispID;
    Result := True;
  end else
    Result := False;
end;

function TObjectProxyServer.GetIDOfProperty(AObjectProxy: TObjectProxy;
  const APropName: string; var DispID: TDispID): Boolean;
var
  PropInfo: PPropInfo;
begin
  PropInfo := AObjectProxy.GetPropInfo(APropName);
  if Assigned(PropInfo) then
  begin
    DispID := TDispID(PropInfo);
    Result := True;
  end else
    Result := False;
end;

procedure TObjectProxyServer.RegisterMethod(AClass: TClass; const AMethodName: string;
  ADispID: TDispID; AMethod: TObjectProxyMethodEvent);
var
  I, J: Integer;
  NewMethod, OldMethod: TObjectProxyMethod;
begin
  I := FMethods.IndexOf(AMethodName);
  if I < 0 then
    I := FMethods.AddObject(AMethodName, TClassList.Create);
  NewMethod := TObjectProxyMethod.Create(Self, AMethodName, ADispID, AMethod);
  with TClassList(FMethods.Objects[I]) do
  begin
    J := IndexOfClass(AClass, False);
    if J >= 0 then
    begin
      with Items[J] do
      begin
        OldMethod := Data;
        Data := NewMethod;
        RemoveMethod(OldMethod, False);
        OldMethod.Free;
      end;
    end else
      RegisterClass(AClass, NewMethod, '', True);
  end;
end;

procedure TObjectProxyServer.UnregisterMethod(AClass: TClass; const AMethodName: string);
var
  I, J: Integer;
begin
  I := FMethods.IndexOf(AMethodName);
  if I >= 0 then
    with TClassList(FMethods.Objects[I]) do
    begin
      J := IndexOfClass(AClass, False);
      if J >= 0 then
        TObject(Items[J].Data).Free;
    end;
end;

procedure TObjectProxyServer.InsertMethod(AMethod: TObjectProxyMethod);
begin
  ListAdd(FProxyMethods, AMethod);
  AMethod.FProxyServer := Self;
end;

procedure TObjectProxyServer.RemoveMethod(AMethod: TObjectProxyMethod; RemoveName: Boolean);
var
  I, J: Integer;
begin
  ListRemove(FProxyMethods, AMethod);
  AMethod.FProxyServer := nil;
  
  if RemoveName then
    for I := 0 to FMethods.Count - 1 do
      with TClassList(FMethods.Objects[I]) do
        for J := 0 to Count - 1 do
          if Items[J].Data = AMethod then
          begin
            UnregisterClass(Items[J].GetClassType);
            if Count = 0 then
            begin
              Free;
              FMethods.Delete(I);
            end;
            Exit;
          end;
end;

procedure TObjectProxyServer.InsertProxy(AObjectProxy: TObjectProxy);
begin
  ListAdd(FProxies, AObjectProxy);
  AObjectProxy.FProxyServer := Self;
end;

procedure TObjectProxyServer.RemoveProxy(AObjectProxy: TObjectProxy);
begin
  ListRemove(FProxies, AObjectProxy);
  AObjectProxy.FProxyServer := nil;
end;

function TObjectProxyServer.FindMethodByDispID(AClass: TClass; DispID: TDispID): TObjectProxyMethod;
var
  I, J: Integer;
  MethodClass: TClass;
  ClassList: TClassList;
begin
  for I := 0 to FMethods.Count - 1 do
  begin
    ClassList := TClassList(FMethods.Objects[I]);
    for J := ClassList.Count - 1 downto 0 do
    begin
      MethodClass := ClassList.Items[J].GetClassType;
      if IsClass(AClass, MethodClass) then
      begin
        Result := ClassList.Items[J].Data;
        if Result.DispID = DispID then Exit;
      end;
    end;
  end;
  Result := nil;
end;

function TObjectProxyServer.FindMethodByName(AClass: TClass; const AMethodName: string): TObjectProxyMethod;
var
  I, J: Integer;
  ClassList: TClassList;
begin
  I := FMethods.IndexOf(AMethodName);
  if I >= 0 then
  begin
    ClassList := TClassList(FMethods.Objects[I]);
    J := ClassList.IndexOfClass(AClass, True);
    if J >= 0 then
      Result := ClassList.Items[J].Data else
      Result := nil;
  end else
    Result := nil;
end;

procedure TObjectProxyServer.RegisterMethods;
begin
  { TObject }
  CoTObjectProxyMethods.RegisterMethods(Self);
  { TPersistent }
  CoTPersistentProxyMethods.RegisterMethods(Self);
  { TStrings }
  CoTStringsProxyMethods.RegisterMethods(Self);
  { TCollection }
  CoTCollectionProxyMethods.RegisterMethods(Self);
  { TComponent }
  CoTComponentProxyMethods.RegisterMethods(Self);
  { Enumerators }
  RegisterEnumerators(Self);
end;

function TObjectProxyServer.GetProperty(AObjectProxy: TObjectProxy; PropInfo: PPropInfo; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  I, J: Integer;
  S: string;
  OrdVal: Integer;
  DateTime: TDateTime;
  Method: TMethod;
  ProxyMethod: TObjectProxyMethod;
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
  AObject: TObject;
begin
  AObject := AObjectProxy.GetObject;
  case PropInfo^.PropType^.Kind of
    tkInteger:
      if Args.Count = 0 then
      begin
        SetVarResult(VarResult, GetOrdProp(AObject, PropInfo));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkChar:
      if Args.Count = 0 then
      begin
        SetVarResult(VarResult, Char(GetOrdProp(AObject, PropInfo)));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkEnumeration:
      if Args.Count = 0 then
      begin
        if PropInfo^.PropType^ = System.TypeInfo(Boolean) then
        begin
          SetVarResult(VarResult, Boolean(GetOrdProp(AObject, PropInfo)));
        end else begin
          OrdVal := GetOrdProp(AObject, PropInfo);
          SetVarResult(VarResult, GetEnumName(PropInfo^.PropType^, OrdVal));
        end;
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkString, tkLString, tkWChar, tkWString:
      if Args.Count = 0 then
      begin
        SetVarResult(VarResult, GetStrProp(AObject, PropInfo));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkFloat:
      if Args.Count = 0 then
      begin
        if PropInfo^.PropType^ = System.TypeInfo(TDateTime) then
        begin
          DateTime := GetFloatProp(AObject, PropInfo);
          SetVarResult(VarResult, DateTime);
        end else
          SetVarResult(VarResult, GetFloatProp(AObject, PropInfo));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkSet:
      if Args.Count = 0 then
      begin
        OrdVal := GetOrdProp(AObject, PropInfo);
        TypeData := GetTypeData(PropInfo^.PropType^);
        TypeInfo := TypeData^.CompType^;
        S := '';
        if OrdVal <> 0 then
        begin
          for I := 0 to 31 do
          begin
            J := 1 shl I;
            if (J and OrdVal) = J then
              AddDelimeted(S, GetEnumName(TypeInfo, I), ',');
          end;
        end;
        SetVarResult(VarResult, S);
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkClass:
      begin
        OrdVal := GetOrdProp(AObject, PropInfo);
        if Args.Count = 0 then
        begin
          if OrdVal <> 0 then
          begin
            Result := GetObjectProxy(TObject(OrdVal), VarResult^)
          end else begin
            SetVarResult(VarResult, Unassigned);
            Result := S_OK;
          end;
        end else begin
          ProxyMethod := FindMethodByDispID(TObject(OrdVal).ClassType, DISPID_VALUE);
          if Assigned(ProxyMethod) then
            Result := ProxyMethod.Invoke(CreateObjectProxy(TObject(OrdVal)), Args, VarResult) else
            Result := DISP_E_MEMBERNOTFOUND;
        end;
      end;

    tkMethod:
      begin
        Method := GetMethodProp(AObject, PropInfo);
        SetVarResult(VarResult, PackMethod(Method, PropInfo^.PropType^));
        Result := S_OK;
      end;

    tkVariant:
      if Args.Count = 0 then
      begin
        SetVarResult(VarResult, GetVariantProp(AObject, PropInfo));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

{$IFDEF _D5_}
    tkInt64:
      if Args.Count = 0 then
      begin
        SetVarResult(VarResult, Integer(GetInt64Prop(AObject, PropInfo)));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;
{$ENDIF}
  else
    Result := DISP_E_MEMBERNOTFOUND;
  end;
end;

function TObjectProxyServer.SetProperty(AObjectProxy: TObjectProxy;
  PropInfo: PPropInfo; Args: TInvokeArguments): HResult;
var
  S, EnumName: string;
  C: Char;
  OrdVal, EnumValue: Integer;
  CommaPos: Integer;
  DateTime: TDateTime;
  Method: TMethod;
  ProxyMethod: TObjectProxyMethod;
  TypeData: PTypeData;
  TypeInfo: PTypeInfo;
  AObject: TObject;
begin
  AObject := AObjectProxy.GetObject;
  case PropInfo^.PropType^.Kind of
    tkInteger:
      if Args.Count = 1 then
      begin
        SetOrdProp(AObject, PropInfo, Args[0]);
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkChar:
      if Args.Count = 1 then
      begin
        S := Args[0];
        if Length(S) > 0 then C := S[1] else C := #0;
        SetOrdProp(AObject, PropInfo, ord(C));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkEnumeration:
      if Args.Count = 1 then
      begin
        S := Trim(Args[0]);
        OrdVal := GetEnumValue(PropInfo^.PropType^, S);
        if (OrdVal < 0) and (PropInfo^.PropType^ = System.TypeInfo(Boolean)) then
        begin
          if S = '0' then OrdVal := 0 else
          if S = '1' then OrdVal := 1 else
          if S = '-1' then OrdVal := 1;
        end;

        if OrdVal >= 0 then
        begin
          SetOrdProp(AObject, PropInfo, OrdVal);
          Result := S_OK;
        end else
          Result := DISP_E_BADVARTYPE;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkString, tkLString, tkWChar, tkWString:
      if Args.Count = 1 then
      begin
        SetStrProp(AObject, PropInfo, string(Args[0]));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkFloat:
      if Args.Count = 1 then
      begin
        if (PropInfo^.PropType^ = System.TypeInfo(TDateTime)) then
        begin
          if TVarData(Args[0]).VType = varDate then
          begin
            DateTime := Args[0];
            SetFloatProp(AObject, PropInfo, DateTime);
            Result := S_OK;
          end else if TVarData(Args[0]).VType = varOleStr then
          begin
            DateTime := StrToDate(Args[0]);
            SetFloatProp(AObject, PropInfo, DateTime);
            Result := S_OK;
          end else
            Result := DISP_E_BADVARTYPE;
        end else begin
          SetFloatProp(AObject, PropInfo, Args[0]);
          Result := S_OK;
        end;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkSet:
      if Args.Count = 1 then
      begin
        if TVarData(Args[0]).VType = varOleStr then
        begin
          TypeData := GetTypeData(PropInfo^.PropType^);
          TypeInfo := TypeData^.CompType^;
          S := Trim(Args[0]);

          OrdVal := 0;
          while Length(S) > 0 do
          begin
            CommaPos := Pos(',', S);
            if CommaPos = 0 then
              CommaPos := Length(S) + 1;
            EnumName := Trim(Copy(S, 1, CommaPos - 1));
            System.Delete(S, 1, CommaPos);
            if Length(EnumName) > 0 then
            begin
              EnumValue := GetEnumValue(TypeInfo, EnumName);
              if EnumValue >= 0 then
              begin
                OrdVal := OrdVal or 1 shl EnumValue;
              end else begin
                Result := DISP_E_BADVARTYPE;
                Exit;
              end;
            end;
          end;
          SetOrdProp(AObject, PropInfo, OrdVal);
          Result := S_OK;
        end else
          Result := DISP_E_BADVARTYPE;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkClass:
      if Args.Count = 1 then
      begin
        if GetArgObject(Args[0], TObject(OrdVal)) then
        begin
          SetOrdProp(AObject, PropInfo, OrdVal);
          Result := S_OK;
        end else
          Result := DISP_E_BADVARTYPE;
      end else begin
        AObject := AObjectProxy.GetObject;
        OrdVal := GetOrdProp(AObject, PropInfo);
        ProxyMethod := FindMethodByDispID(TObject(OrdVal).ClassType, DISPID_VALUE);
        if Assigned(ProxyMethod) then                                      
          Result := ProxyMethod.Invoke(CreateObjectProxy(TObject(OrdVal)), Args, nil) else
          Result := DISP_E_MEMBERNOTFOUND;
      end;

    tkMethod:
      if Args.Count = 1 then
      begin
        UnpackMethod(Args[0], Method, Pointer(TypeInfo));
        if (TypeInfo = nil) or (TypeInfo = PropInfo^.PropType^) then
        begin
          SetMethodProp(AObject, PropInfo, Method);
          Result := S_OK;
        end else
          Result := DISP_E_BADVARTYPE;
      end else
        Result := DISP_E_BADPARAMCOUNT;

    tkVariant:
      if Args.Count = 1 then
      begin
        SetVariantProp(AObject, PropInfo, Args[0]);
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;
        
{$IFDEF _D5_}
    tkInt64:
      if Args.Count = 1 then
      begin
        SetInt64Prop(AObject, PropInfo, Integer(Args[0]));
        Result := S_OK;
      end else
        Result := DISP_E_BADPARAMCOUNT;
{$ENDIF}
  else
    Result := DISP_E_MEMBERNOTFOUND;
  end;
end;

{ TObjectProxyMethod }
constructor TObjectProxyMethod.Create(AProxyServer: TObjectProxyServer; const AName: string;
  ADispID: TDispID; AMethod: TObjectProxyMethodEvent);
begin
  FName := AName;
  FMethod := AMethod;
  FDispID := ADispID;
  if Assigned(AProxyServer) then AProxyServer.InsertMethod(Self);
end;

destructor TObjectProxyMethod.Destroy;
begin
  if Assigned(FProxyServer) then
    FProxyServer.RemoveMethod(Self, True);
  inherited;
end;

function TObjectProxyMethod.Invoke(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Assigned(Method) then
    Result := Method(AObjectProxy, Args, VarResult) else
    Result := S_OK;
end;

{ TObjectProxyEnumerator }
constructor TObjectProxyEnumerator.Create(AObjectProxy: TObjectProxy);
begin
  inherited Create;
  AObjectProxy.InsertEnumerator(Self);
end;

destructor TObjectProxyEnumerator.Destroy;
begin
  if Assigned(FObjectProxy) then
    FObjectProxy.RemoveEnumerator(Self);
  inherited;
end;

function TObjectProxyEnumerator.CreateEnumerator: TEnumeratorObject;
begin
  Result := TObjectProxyEnumerator(Self.ClassType).Create(ObjectProxy);
end;

{ TStringsProxyEnumerator }
function TStringsProxyEnumerator.Fetch(Index: LongWord; var VarResult: OleVariant): HResult;
begin
  if Assigned(ObjectProxy) then
  begin
    VarResult := (ObjectProxy.GetObject as TStrings).Strings[Index];
    Result := S_OK;
  end else
    Result := S_FALSE;
end;

function TStringsProxyEnumerator.GetCount: LongWord;
begin
  if Assigned(ObjectProxy) then
    Result := (ObjectProxy.GetObject as TStrings).Count else
    Result := 0;
end;

{ TCollectionProxyEnumerator }
function TCollectionProxyEnumerator.Fetch(Index: LongWord; var VarResult: OleVariant): HResult;
begin
  if Assigned(ObjectProxy) then
    Result := ObjectProxy.GetObjectProxy((ObjectProxy.GetObject as TCollection).Items[Index], VarResult) else
    Result := S_FALSE;
end;

function TCollectionProxyEnumerator.GetCount: LongWord;
begin
  if Assigned(ObjectProxy) then
    Result := (ObjectProxy.GetObject as TCollection).Count else
    Result := 0;
end;

{ TComponentProxyEnumerator }
function TComponentProxyEnumerator.Fetch(Index: LongWord; var VarResult: OleVariant): HResult;
begin
  if Assigned(ObjectProxy) then
    Result := ObjectProxy.GetObjectProxy((ObjectProxy.GetObject as TComponent).Components[Index], VarResult) else
    Result := S_FALSE;
end;

function TComponentProxyEnumerator.GetCount: LongWord;
begin
  if Assigned(ObjectProxy) then
    Result := (ObjectProxy.GetObject as TComponent).ComponentCount else
    Result := 0;
end;

{ TObjectProxy }
constructor TObjectProxy.Create(AObject: TObject; AProxyServer: TObjectProxyServer);
begin
  inherited Create;
  if AProxyServer = nil then
    AProxyServer := vgScript.ProxyServer;
  FProxyServer := AProxyServer;
  FProxyServer.InsertProxy(Self);
  FObject := AObject;
end;

destructor TObjectProxy.Destroy;
begin
  if Assigned(FProxyServer) then
    FProxyServer.RemoveProxy(Self);
  while ListCount(FEnumerators) > 0 do
    RemoveEnumerator(FEnumerators.Last);
  inherited;
end;

procedure TObjectProxy.InsertEnumerator(AEnumerator: TObjectProxyEnumerator);
begin
  ListAdd(FEnumerators, AEnumerator);
  AEnumerator.FObjectProxy := Self;
end;

procedure TObjectProxy.RemoveEnumerator(AEnumerator: TObjectProxyEnumerator);
begin
  ListRemove(FEnumerators, AEnumerator);
  AEnumerator.FObjectProxy := nil;
end;

{ TObjectProxy.IDispatch }
function TObjectProxy.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := E_NOTIMPL;
end;

function TObjectProxy.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TObjectProxy.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
var
  DispID: TDispID;
begin
  if GetIDOfName(PNamesArray(Names)[0], DispID) then
  begin
    PDispIDs(DispIDs)[0] := DispID;
    Result := S_OK;
  end else
    Result := DISP_E_UNKNOWNNAME;
end;

function TObjectProxy.InvokeDispID(DispID: TDispID; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  Enumerator: TEnumeratorObject;
  Method: TObjectProxyMethod;
begin
  case DispID of
    DISPID_NEWENUM:
      begin
        Enumerator := CreateEnumerator;
        if Assigned(Enumerator) then
        begin
          SetVarResult(VarResult, Enumerator as IEnumVariant);
          Result := S_OK;
        end else
          Result := E_NOTIMPL;
      end;
    DISPID_VALUE:
      if Args.Count = 0 then
      begin
        SetVarResult(VarResult, Self as IDispatch);
        Result := S_OK;
      end else begin
        Method := ProxyServer.FindMethodByDispID(GetObject.ClassType, DispID);
        if Assigned(Method) then
          Result := Method.Invoke(Self, Args, VarResult) else
          Result := DISP_E_MEMBERNOTFOUND;
      end;
  else
    Method := ProxyServer.FindMethodByDispID(GetObject.ClassType, DispID);
    if Assigned(Method) then
      Result := Method.Invoke(Self, Args, VarResult)
    else begin
      if Args.Flags * [flPropPut, flPropPutRef] <> [] then
        Result := SetProperty(PPropInfo(DispID), Args) else
        Result := GetProperty(PPropInfo(DispID), Args, VarResult);
    end;
  end;
end;

function TObjectProxy.GetProperty(PropInfo: PPropInfo; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Assigned(ProxyServer) then
    Result := ProxyServer.GetProperty(Self, PropInfo, Args, VarResult) else
    Result := E_NOTIMPL;
end;

function TObjectProxy.SetProperty(PropInfo: PPropInfo; Args: TInvokeArguments): HResult;
begin
  if Assigned(ProxyServer) then
    Result := ProxyServer.SetProperty(Self, PropInfo, Args) else
    Result := E_NOTIMPL;
end;

function TObjectProxy.Invoke(DispID: TDispID; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
var
  Args: TInvokeArguments;
  WideS: WideString;
begin
  try
    Args := TInvokeArguments.Create(@Params, TInvokeFlags(Byte(Flags)));
    try
      Result := InvokeDispID(DispID, Args, POleVariant(VarResult));
    finally
      Args.Free;
    end;
  except
    on E: Exception do
    begin
      if Assigned(ExcepInfo) then
      begin
        ZeroMem(ExcepInfo, SizeOf(TExcepInfo));
        TExcepInfo(ExcepInfo^).wCode := 1001;
        if GetObject <> nil then
        begin
          WideS := GetObject.ClassName;
          TExcepInfo(ExcepInfo^).bstrSource := SysAllocString(PWideChar(WideS));
        end;
        WideS := E.Message;
        TExcepInfo(ExcepInfo^).bstrDescription := SysAllocString(PWideChar(WideS));
      end;
      Result := DISP_E_EXCEPTION;
    end;
  end;
end;

{ TObjectProxy.IQueryObject}
function TObjectProxy.GetObject: TObject;
begin
  Result := FObject;
end;

function TObjectProxy.GetPropInfo(const PropName: string): PPropInfo;
var
  AObject: TObject;
begin
  AObject := GetObject;
  if Assigned(AObject) then
    Result := TypInfo.GetPropInfo(AObject.ClassInfo, PropName) else
    Result := nil;
end;

function TObjectProxy.GetIDOfName(const AName: string; var DispID: TDispID): Boolean;
begin
  if Assigned(ProxyServer) then
  begin
    Result :=
      ProxyServer.GetIDOfProperty(Self, AName, DispID) or
      ProxyServer.GetIDOfMethod(Self, AName, DispID);
  end else
    Result := False;
end;

function TObjectProxy.GetObjectProxy(AObject: TObject; var AObjectProxy: OleVariant): HResult;
var
  ObjectProxy: TObjectProxy;
begin
  if Assigned(ProxyServer) then
    ObjectProxy := ProxyServer.CreateObjectProxy(AObject) else
    ObjectProxy := nil;
  if Assigned(ObjectProxy) then
  begin
    AObjectProxy := ObjectProxy as IDispatch;
    Result := S_OK;
  end else
    Result := E_NOTIMPL;
end;

function TObjectProxy.CreateEnumerator: TEnumeratorObject;
begin
  if Assigned(ProxyServer) then
    Result := ProxyServer.CreateEnumerator(Self) else
    Result := nil;
end;

{ TEventThunk }
function TEventThunk.GetMethod: TMethod;
begin
  Result.Code := nil;
  Result.Data := nil;
end;

{ TNotifyEventThunk }
function TNotifyEventThunk.GetMethod: TMethod;
begin
  Result.Code := @TNotifyEventThunk.DoEvent;
  Result.Data := Self;
end;

procedure TNotifyEventThunk.DoEvent(Sender: TObject);
begin
  if Assigned(FOnEvent) then FOnEvent(Self, Sender);
end;

{ EScriptError }
constructor EScriptError.Create(const Msg: string; ALine, AColumn: Integer);
begin
  inherited Create(Msg);
  FLine := ALine;
  FColumn := AColumn;
end;

{ TCustomScript }
constructor TCustomScript.Create(AOwner: TComponent);
begin
  inherited;
  FLines := TStringList.Create;
  TStringList(FLines).OnChange := LinesChanged;
end;

destructor TCustomScript.Destroy;
begin
  FLines.Free;
  inherited;
end;

procedure TCustomScript.SetLines(Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TCustomScript.LinesChanged(Sender: TObject);
begin
end;

{ TDispatchMethods }
function TDispatchMethods.GetItem(Index: Integer): TDispatchMethod;
begin
  Result := TDispatchMethod(inherited Items[Index]);
end;

function TDispatchMethods.ProxyServer: TObjectProxyServer;
begin
  Result := nil
end;

procedure TDispatchMethods.Update(Item: TCollectionItem);
var
  I: Integer;
  Method: TDispatchMethod;
begin
  for I := 0 to Count - 1 do
  begin
    Method := Items[I];
    with Self.ProxyServer, Method do
      if Active then
        RegisterMethod(nil, Name, DispID, Invoke);
  end;
end;

{ TDispatchMethod }
constructor TDispatchMethod.Create(Collection: TCollection);
begin
  inherited;
  FActive := True;
  FClassNameSupport := TObject.ClassName;
  FDispID := DISPID_OBJECTPROXUSER + ID;
end;

destructor TDispatchMethod.Destroy;
begin
  if Active then
    ProxyServer.UnregisterMethod(nil, Name);
  inherited;
end;

procedure TDispatchMethod.SetDisplayName(const Value: string);
var
  OldName: string;
begin
  OldName := Name;
  inherited;
  if Active and (AnsiCompareText(Name, OldName) <> 0) then
    ProxyServer.UnregisterMethod(nil, OldName);
end;

function TDispatchMethod.ProxyServer: TObjectProxyServer;
begin
  Result := TDispatchMethods(Collection).ProxyServer;
end;

function TDispatchMethod.InternalInvoke(AObjectProxy: TObjectProxy;
  Args: TInvokeArguments; VarResult: POleVariant): HResult;
begin
  Result := E_NOTIMPL;
  if Assigned(FOnInvoke) then
    FOnInvoke(Self, AObjectProxy, Args, VarResult, Result);
end;

function TDispatchMethod.Invoke(AObjectProxy: TObjectProxy;
  Args: TInvokeArguments; VarResult: POleVariant): HResult;
var
  IsClass: Boolean;
  AClass: TClass;
begin
  AClass := AObjectProxy.GetObject.ClassType;
  repeat
    IsClass := CompareText(AClass.ClassName, FClassNameSupport) = 0;
    if IsClass then Break;
    AClass := AClass.ClassParent;
  until AClass = nil;

  if IsClass then
    Result := InternalInvoke(AObjectProxy, Args, VarResult) else
    Result := E_NOTIMPL;
end;

function TDispatchMethod.StoreClassNameSupport: Boolean;
begin
  Result := CompareText(FClassNameSupport, TObject.ClassName) <> 0;
end;

procedure TDispatchMethod.Assign(Source: TPersistent);
begin
  if Source is TDispatchMethod then
    with TDispatchMethod(Source) do
    begin
      Self.Active := Active;
      Self.DispID := DispID;
      Self.FOnInvoke := OnInvoke;
      Self.FClassNameSupport := ClassNameSupport;
    end;
  inherited;
end;

procedure TDispatchMethod.SetActive(Value: Boolean);
begin
  if (FActive <> Value) then
  begin
    if FActive then
      ProxyServer.UnregisterMethod(nil, Name);
    FActive := Value;
    Changed(False);
  end;
end;

procedure TDispatchMethod.SetDispID(Value: TDispID);
begin
  if FDispID <> Value then
  begin
    if FActive then
      ProxyServer.UnregisterMethod(nil, Name);
    FDispID := Value;
    Changed(False);
  end;
end;

{ TScriptDispatchMethods }
function TScriptDispatchMethods.ProxyServer: TObjectProxyServer;
begin
  Result := TCustomDispatchScript(GetOwner).ProxyServer;
end;

{ TCustomDispatchScript }
destructor TCustomDispatchScript.Destroy;
begin
  FProxyServer.Free;
  inherited;
end;

function TCustomDispatchScript.CreateProxyServer: TObjectProxyServer;
begin
  Result := nil;
end;

constructor TCustomDispatchScript.Create(AOwner: TComponent);
begin
  inherited;
  FMethods := TScriptDispatchMethods.Create({$IFDEF _D4_}Self,{$ENDIF}TDispatchMethod);
end;

function TCustomDispatchScript.GetProxyServer: TObjectProxyServer;
begin
  if not Assigned(FProxyServer) then
    FProxyServer := CreateProxyServer;
  Result := FProxyServer;
end;

procedure TCustomDispatchScript.SetMethods(Value: TDispatchMethods);
begin
  FMethods.Assign(Value);
end;

function TCustomDispatchScript.StoreMetods: Boolean;
begin
  Result := FMethods.Count > 0;
end;

{ CoTObjectProxyMethods }
class procedure CoTObjectProxyMethods.RegisterMethods(AProxyServer: TObjectProxyServer);
begin
  with AProxyServer do
  begin
    RegisterMethod(TObject, STObjectClassName, DISPID_TObjectClassName, TObjectClassName);
    RegisterMethod(TObject, STObjectIsClass, DISPID_TObjectIsClass, TObjectIsClass);
    RegisterMethod(TObject, STObjectHasMethod, DISPID_TObjectHasMethod, TObjectHasMethod);
    RegisterMethod(TObject, STObjectHasProperty, DISPID_TObjectHasProperty, TObjectHasProperty);
    RegisterMethod(TObject, STObjectIsEqualTo, DISPID_TObjectIsEqualTo, TObjectIsEqualTo);
    RegisterMethod(TObject, STObjectFree, DISPID_TObjectFree, TObjectFree);
  end;
end;

class function CoTObjectProxyMethods.TObjectClassName(AObjectProxy: TObjectProxy;
  Args: TInvokeArguments; VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    SetVarResult(VarResult, AObjectProxy.GetObject.ClassName);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTObjectProxyMethods.TObjectIsClass(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  AClass: TClass;
  ClassName: string;
  IsClass: Boolean;
begin
  if Args.Count = 1 then
  begin
    ClassName := Args[0];
    AClass := AObjectProxy.GetObject.ClassType;
    repeat
      IsClass := CompareText(AClass.ClassName, ClassName) = 0;
      if IsClass then Break;
      AClass := AClass.ClassParent;
    until AClass = nil;
    SetVarResult(VarResult, IsClass);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTObjectProxyMethods.TObjectHasMethod(AObjectProxy: TObjectProxy;
  Args: TInvokeArguments; VarResult: POleVariant): HResult;
var
  MethodName: string;
begin
  if Args.Count = 1 then
  begin
    MethodName := Args[0];
    SetVarResult(VarResult, AObjectProxy.ProxyServer.FindMethodByName(AObjectProxy.GetObject.ClassType, MethodName) <> nil);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTObjectProxyMethods.TObjectHasProperty(AObjectProxy: TObjectProxy;
  Args: TInvokeArguments; VarResult: POleVariant): HResult;
var
  PropName: string;
begin
  if Args.Count = 1 then
  begin
    PropName := Args[0];
    SetVarResult(VarResult, AObjectProxy.GetPropInfo(PropName) <> nil);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTObjectProxyMethods.TObjectFree(AObjectProxy: TObjectProxy;
  Args: TInvokeArguments; VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    AObjectProxy.GetObject.Free;
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTObjectProxyMethods.TObjectIsEqualTo(AObjectProxy: TObjectProxy;
  Args: TInvokeArguments; VarResult: POleVariant): HResult;
var
  AObject: TObject;
begin
  if Args.Count = 1 then
  begin
    if GetArgObject(Args[0], AObject) then
    begin
      SetVarResult(VarResult, AObjectProxy.GetObject = AObject);
      Result := S_OK;
    end else
      Result := DISP_E_BADVARTYPE;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

{ CoTPersistentProxyMethods }
class procedure CoTPersistentProxyMethods.RegisterMethods(AProxyServer: TObjectProxyServer);
begin
  inherited;
  with AProxyServer do
  begin
    RegisterMethod(TPersistent, STPersistentAssign, DISPID_TPersistentAssign, TPersistentAssign);
  end;
end;

{ TPersistent }
class function CoTPersistentProxyMethods.TPersistentAssign(AObjectProxy: TObjectProxy;
  Args: TInvokeArguments; VarResult: POleVariant): HResult;
var
  AObject: TObject;
begin
  if Args.Count = 1 then
  begin
    if GetArgObject(Args[0], AObject) and Assigned(AObject) then
    begin
      (AObjectProxy.GetObject as TPersistent).Assign(AObject as TPersistent);
      Result := S_OK;
    end else
      Result := DISP_E_BADVARTYPE;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

{ CoTStringsProxyMethods }
class procedure CoTStringsProxyMethods.RegisterMethods(AProxyServer: TObjectProxyServer);
begin
  inherited;
  with AProxyServer do
  begin
    RegisterMethod(TStrings, STStringsCount, DISPID_TStringsCount, TStringsCount);
    RegisterMethod(TStrings, STStringsInsert, DISPID_TStringsInsert, TStringsInsert);
    RegisterMethod(TStrings, STStringsAdd, DISPID_TStringsAdd, TStringsAdd);
    RegisterMethod(TStrings, STStringsDelete, DISPID_TStringsDelete, TStringsDelete);
    RegisterMethod(TStrings, STStringsIndexOf, DISPID_TStringsIndexOf, TStringsIndexOf);
    RegisterMethod(TStrings, STStringsText, DISPID_TStringsText, TStringsText);
    RegisterMethod(TStrings, STStringsCommaText, DISPID_TStringsCommaText, TStringsCommaText);
    { Default property }
    RegisterMethod(TStrings, STStringsStrings, DISPID_VALUE, TStringsStrings);
  end;
end;

{ TStrings }
class function CoTStringsProxyMethods.TStringsCount(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TStrings).Count);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTStringsProxyMethods.TStringsInsert(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 2 then
  begin
    (AObjectProxy.GetObject as TStrings).Insert(Args[0], Args[1]);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTStringsProxyMethods.TStringsAdd(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 1 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TStrings).Add(Args[0]));
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTStringsProxyMethods.TStringsDelete(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 1 then
  begin
    (AObjectProxy.GetObject as TStrings).Delete(Args[0]);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTStringsProxyMethods.TStringsIndexOf(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 1 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TStrings).IndexOf(Args[0]));
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTStringsProxyMethods.TStringsText(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TStrings).Text);
    Result := S_OK;
  end else if Args.Count = 1 then
  begin
    (AObjectProxy.GetObject as TStrings).Text := Args[0];
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTStringsProxyMethods.TStringsCommaText(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TStrings).CommaText);
    Result := S_OK;
  end else if Args.Count = 1 then
  begin
    (AObjectProxy.GetObject as TStrings).CommaText := Args[0];
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTStringsProxyMethods.TStringsStrings(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 1 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TStrings).Strings[Args[0]]);
    Result := S_OK;
  end else if Args.Count = 2 then
  begin
    (AObjectProxy.GetObject as TStrings).Strings[Args[0]] := Args[1];
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

{ CoTCollectionProxyMethods }
class procedure CoTCollectionProxyMethods.RegisterMethods(AProxyServer: TObjectProxyServer);
begin
  inherited;
  with AProxyServer do
  begin
    RegisterMethod(TCollection, STCollectionCount, DISPID_TCollectionCount, TCollectionCount);
    RegisterMethod(TCollection, STCollectionInsert, DISPID_TCollectionInsert, TCollectionInsert);
    RegisterMethod(TCollection, STCollectionAdd, DISPID_TCollectionAdd, TCollectionAdd);
    RegisterMethod(TCollection, STCollectionClear, DISPID_TCollectionClear, TCollectionClear);
    { Default property }
    RegisterMethod(TCollection, STCollectionItems, DISPID_VALUE, TCollectionItems);
  end;
end;

{ TCollection }
class function CoTCollectionProxyMethods.TCollectionCount(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TCollection).Count);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTCollectionProxyMethods.TCollectionItems(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  Item: TCollectionItem;
begin
  if Args.Count = 1 then
  begin
    Item := (AObjectProxy.GetObject as TCollection).Items[Args[0]];
    Result := AObjectProxy.ProxyServer.GetObjectProxy(Item, VarResult^);
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTCollectionProxyMethods.TCollectionInsert(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  Item: TCollectionItem;
begin
  if Args.Count = 1 then
  begin
{$IFDEF _D4_}
    Item := (AObjectProxy.GetObject as TCollection).Insert(Args[0]);
{$ELSE}
    Item := (AObjectProxy.GetObject as TCollection).Add;
{$ENDIF}
    try
{$IFNDEF _D4_}
      Item.Index := Args[0];
{$ENDIF}
      Result := AObjectProxy.ProxyServer.GetObjectProxy(Item, VarResult^);
    except
      Item.Free;
      raise;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTCollectionProxyMethods.TCollectionAdd(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  Item: TCollectionItem;
begin
  if Args.Count = 0 then
  begin
    Item := (AObjectProxy.GetObject as TCollection).Add;
    try
      Result := AObjectProxy.ProxyServer.GetObjectProxy(Item, VarResult^);
    except
      Item.Free;
      raise;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTCollectionProxyMethods.TCollectionClear(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    (AObjectProxy.GetObject as TCollection).Clear;
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

{ CoTComponentProxyMethods }
class procedure CoTComponentProxyMethods.RegisterMethods(AProxyServer: TObjectProxyServer);
begin
  inherited;
  with AProxyServer do
  begin
    RegisterMethod(TComponent, STComponentComponentName, DISPID_TComponentComponentName, TComponentComponentName);
    RegisterMethod(TComponent, STComponentComponentCount, DISPID_TComponentComponentCount, TComponentComponentCount);
    RegisterMethod(TComponent, STComponentComponents, DISPID_TComponentComponents, TComponentComponents);
    RegisterMethod(TComponent, STComponentOwner, DISPID_TComponentOwner, TComponentOwner);
    RegisterMethod(TComponent, STComponentCreate, DISPID_TComponentCreate, TComponentCreate);
    { Default property }
    RegisterMethod(TComponent, STComponentFindComponent, DISPID_VALUE, TComponentFindComponent);
  end;
end;

{ TComponent }
class function CoTComponentProxyMethods.TComponentComponentName(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TComponent).Name);
    Result := S_OK;
  end else if Args.Count = 1 then
  begin
    (AObjectProxy.GetObject as TComponent).Name := Args[0];
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTComponentProxyMethods.TComponentComponentCount(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TComponent).ComponentCount);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTComponentProxyMethods.TComponentComponents(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 1 then
  begin
    Result := AObjectProxy.ProxyServer.GetObjectProxy((AObjectProxy.GetObject as TComponent).Components[Args[0]], VarResult^);
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTComponentProxyMethods.TComponentFindComponent(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 1 then
  begin
    Result := AObjectProxy.ProxyServer.GetObjectProxy((AObjectProxy.GetObject as TComponent).FindComponent(Args[0]), VarResult^);
    if Result = E_NOTIMPL then
    begin
      SetVarResult(VarResult, Unassigned);
      Result := S_OK;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTComponentProxyMethods.TComponentOwner(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    Result := AObjectProxy.ProxyServer.GetObjectProxy((AObjectProxy.GetObject as TComponent).Owner, VarResult^);
    if Result = E_NOTIMPL then
    begin
      SetVarResult(VarResult, Unassigned);
      Result := S_OK;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTComponentProxyMethods.TComponentCreate(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  AClassName: string;
  AOwner: TObject;
  AComponent: TComponent;
  CompClass: TComponentClass;
  ClassItem: TClassItem;
begin
  if (Args.Count > 0) and (Args.Count <= 3) then
  begin
    AClassName := Args[0];
    AOwner := nil;
    if (Args.Count > 1) and not ((GetArgObject(Args[Min(1, Args.Count - 1)], AOwner) and
      ((AOwner = nil) or (AOwner is TComponent)))) then
    begin
      Result := DISP_E_BADVARTYPE;
      Exit;
    end else begin
      ClassItem := AObjectProxy.ProxyServer.ComponentClasses.ClassItemByName(AClassName);
      CompClass := TComponentClass(ClassItem.GetClassType);
      AComponent := CompClass.Create(TComponent(AOwner));
      try
        if Args.Count = 3 then
          AComponent.Name := Args[2];
        Result := AObjectProxy.ProxyServer.GetObjectProxy(AComponent, VarResult^);
        if Result <> S_OK then
          AComponent.Free;
      except
        AComponent.Free;
        raise;
      end;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

initialization

finalization
   FProxyServer.Free;

end.
