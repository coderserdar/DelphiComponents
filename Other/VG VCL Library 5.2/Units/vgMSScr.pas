{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Scripting: MS Script control                  }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgMSScr;

interface
uses Windows, TypInfo, Classes, vgSystem, vgScript, MSScript;

type
{ TWinControlProxyEnumerator }
  TWinControlProxyEnumerator = class(TObjectProxyEnumerator)
  public
    function Fetch(Index: LongWord; var VarResult: OleVariant): HResult; override;
    function GetCount: LongWord; override;
  end;

{ TVCLProxyServer }
  TVCLProxyServer = class(TObjectProxyServer)
  public
    procedure RegisterMethods; override;
  end;

{ CoTControlProxyMethods }
  CoTControlProxyMethods = class(CoTComponentProxyMethods)
  public
    class procedure RegisterMethods(AProxyServer: TObjectProxyServer); override;
    { TControl }
    class function TControlParent(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
  end;

{ CoTWinControlProxyMethods }
  CoTWinControlProxyMethods = class(CoTControlProxyMethods)
  public
    class procedure RegisterMethods(AProxyServer: TObjectProxyServer); override;
    { TWinControl }
    class function TWinControlControlCount(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
    class function TWinControlControls(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
      VarResult: POleVariant): HResult;
  end;

  TCustomMSSCScript = class;

{ TMSSCScriptProxyServer }
  TMSSCScriptProxyServer = class(TVCLProxyServer)
  private
    FScript: TCustomMSSCScript;
  public
    constructor Create(AScript: TCustomMSSCScript);
    { Event support }
    function GetProperty(AObjectProxy: TObjectProxy; PropInfo: PPropInfo; Args: TInvokeArguments;
      VarResult: POleVariant): HResult; override;
    function SetProperty(AObjectProxy: TObjectProxy; PropInfo: PPropInfo;
      Args: TInvokeArguments): HResult; override;
    { properties }
    property Script: TCustomMSSCScript read FScript;
  end;

{ TScriptHandlerThunk }
  TScriptHandlerThunk = class(TNotifyEventThunk)
  private
    FInstance: TComponent;
    FPropName, FProcName: string;
  public
    constructor Create(AComponent: TComponent; const APropName, AProcName: string;
      AHandler: TNotifyEventThunkEvent);
    property Instance: TComponent read FInstance;
    property ProcName: string read FProcName;
    property PropName: string read FPropName;
  end;

  TCreateObjectEvent = procedure(Sender: TObject;
    ProxyServer: TObjectProxyServer;  var Obj: OleVariant) of object;

{ TCustomMSSCScript }
  TCustomMSSCScript = class(TCustomDispatchScript)
  private
    FInExecute: Boolean;
    FResetNeeded: Boolean;
    FAllowUI: Boolean;
    FTimeout: Integer;
    FLanguage: string;
    FMainProc, FObjectName: string;
    FScriptControl: TScriptControl;
    FEventThunks: TList;
    FOnCreateObject: TCreateObjectEvent;
    function IsMainProcStored: Boolean;
    function IsObjectNameStored: Boolean;
    procedure SetLanguage(Value: string);
    procedure SetMainProc(Value: string);
    procedure SetObjectName(Value: string);
    function StoreLanguage: Boolean;
    { TNotifyEvent thunks }
    function FindEventThunk(AInstance: TComponent; const APropName: string): TScriptHandlerThunk;
    procedure NotifyEventHandler(EventThunk: TEventThunk; Sender: TObject); virtual;
    procedure SetNotifyEventHandler(AInstance: TComponent; const PropName: string;
      EventThunk: TScriptHandlerThunk);
  protected
    function CreateObject: OleVariant; virtual;
    function CreateProxyServer: TObjectProxyServer; override;
    procedure CreateScriptControl;
    procedure HandleError(const ErrorObject: IScriptError);
    procedure LinesChanged(Sender: TObject); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetEventHandler(AObjectProxy: TObjectProxy; const PropName: string): string;
    procedure SetEventHandler(AObjectProxy: TObjectProxy; const PropName, ProcName: string);
    property ScriptControl: TScriptControl read FScriptControl;
    property AllowUI: Boolean read FAllowUI write FAllowUI default False;
    property Timeout: Integer read FTimeout write FTimeout default -1;
    property Lines;
    property Language: string read FLanguage write SetLanguage stored StoreLanguage;
    property MainProc: string read FMainProc write SetMainProc stored IsMainProcStored;
    property ObjectName: string read FObjectName write SetObjectName stored IsObjectNameStored;
    property OnCreateObject: TCreateObjectEvent read FOnCreateObject write FOnCreateObject;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Reset;
    { Execution }
    function Evalute(const Expression: string): OleVariant;
    function Execute(const Parameters: OleVariant): OleVariant;
    procedure ExecuteStatement(const Statement: string);
    function Run(const ProcedureName: string; const Parameters: OleVariant): OleVariant;
  end;

{ TMSSCScript }
  TMSSCScript = class(TCustomMSSCScript)
  public
    property ProxyServer;
    property ScriptControl;
  published
    property AllowUI;
    property Lines;
    property Language;
    property MainProc;
    property Methods;
    property ObjectName;
    property OnCreateObject;
  end;

const
  DefaultMainProc   = 'Main';
  DefaultObjectName = 'Self';

  LangVBScript      = 'VBScript';
  LangJavaScript    = 'JScript';

{ TControl }
  DISPID_TControlMin                   = DISPID_TObjectMin + 1000;
  DISPID_TControlParent                = DISPID_TControlMin + 1;

{ TWinControl }
  DISPID_TWinControlMin                = DISPID_TControlMin + 20;
  DISPID_TWinControlControlCount       = DISPID_TWinControlMin + 1;
  DISPID_TWinControlControls           = DISPID_TWinControlMin + 2;

{ Method names }

{ TControl }
  STControlParent                    = 'Parent';

{ TWinControl }
  STWinControlControlCount           = 'ControlCount';
  STWinControlControls               = 'Controls';

{ Enumerators support }
procedure RegisterVCLEnumerators(AProxyServer: TObjectProxyServer);

implementation
uses ActiveX, Consts, SysUtils, vgOleUtl, vgUtils, Controls;

procedure RegisterVCLEnumerators(AProxyServer: TObjectProxyServer);
begin
  AProxyServer.EnumClasses.RegisterClass(TWinControl, TWinControlProxyEnumerator, '', True);
end;

procedure PropertyNotFound;
begin
  raise EInvalidOp.Create(SUnknownProperty);
end;

{ TWinControlProxyEnumerator }
function TWinControlProxyEnumerator.Fetch(Index: LongWord; var VarResult: OleVariant): HResult;
begin
  if Assigned(ObjectProxy) then
    Result := ObjectProxy.GetObjectProxy((ObjectProxy.GetObject as TWinControl).Controls[Index], VarResult) else
    Result := S_FALSE;
end;

function TWinControlProxyEnumerator.GetCount: LongWord;
begin
  if Assigned(ObjectProxy) then
    Result := (ObjectProxy.GetObject as TWinControl).ControlCount else
    Result := 0;
end;

{ TVCLProxyServer }
procedure TVCLProxyServer.RegisterMethods;
begin
  inherited;
  { TControl }
  CoTControlProxyMethods.RegisterMethods(Self);
  { TWinControl }
  CoTWinControlProxyMethods.RegisterMethods(Self);
  { Enumerators }
  RegisterVCLEnumerators(Self);
end;

{ CoTControlProxyMethods }
class procedure CoTControlProxyMethods.RegisterMethods(AProxyServer: TObjectProxyServer);
begin
  inherited;
  with AProxyServer do
  begin
    RegisterMethod(TControl, STControlParent, DISPID_TControlParent, TControlParent);
  end;
end;

{ TControl }
class function CoTControlProxyMethods.TControlParent(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  AParent: TObject;
begin
  if Args.Count = 0 then
  begin
    Result := AObjectProxy.ProxyServer.GetObjectProxy((AObjectProxy.GetObject as TControl).Parent, VarResult^);
    if Result = E_NOTIMPL then
    begin
      SetVarResult(VarResult, Unassigned);
      Result := S_OK;
    end;
  end else if Args.Count = 1 then
  begin
    if (GetArgObject(Args[0], AParent) and (AParent is TWinControl)) then
    begin
      (AObjectProxy.GetObject as TControl).Parent := TWinControl(AParent);
      Result := S_OK;
    end else
      Result := DISP_E_BADVARTYPE;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

{ CoTWinControlProxyMethods }
class procedure CoTWinControlProxyMethods.RegisterMethods(AProxyServer: TObjectProxyServer);
begin
  inherited;
  with AProxyServer do
  begin
    RegisterMethod(TWinControl, STWinControlControlCount, DISPID_TWinControlControlCount, TWinControlControlCount);
    RegisterMethod(TWinControl, STWinControlControls, DISPID_TWinControlControls, TWinControlControls);
  end;
end;

{ TWinControl }
class function CoTWinControlProxyMethods.TWinControlControlCount(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 0 then
  begin
    SetVarResult(VarResult, (AObjectProxy.GetObject as TWinControl).ControlCount);
    Result := S_OK;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

class function CoTWinControlProxyMethods.TWinControlControls(AObjectProxy: TObjectProxy; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
begin
  if Args.Count = 1 then
  begin
    Result := AObjectProxy.ProxyServer.GetObjectProxy((AObjectProxy.GetObject as TWinControl).Controls[Args[0]], VarResult^);
    if Result = E_NOTIMPL then
    begin
      SetVarResult(VarResult, Unassigned);
      Result := S_OK;
    end;
  end else
    Result := DISP_E_BADPARAMCOUNT;
end;

{ TMSSCScriptProxyServer }
constructor TMSSCScriptProxyServer.Create(AScript: TCustomMSSCScript);
begin
  inherited Create;
  FScript := AScript;
end;

function TMSSCScriptProxyServer.GetProperty(AObjectProxy: TObjectProxy; PropInfo: PPropInfo; Args: TInvokeArguments;
  VarResult: POleVariant): HResult;
var
  AObject: TObject;
  ProcName: string;
begin
  AObject := AObjectProxy.GetObject;
  case PropInfo^.PropType^.Kind of
    tkMethod:
      begin
        if (PropInfo^.PropType^ = System.TypeInfo(TNotifyEvent)) and (AObject is TComponent) then
        begin
          ProcName := Script.GetEventHandler(AObjectProxy, PropInfo^.Name);
          if ProcName <> '' then
          begin
            SetVarResult(VarResult, ProcName);
            Result := S_OK;
            Exit;
          end;
        end;
        Result := inherited GetProperty(AObjectProxy, PropInfo, Args, VarResult);
      end
  else
    Result := inherited GetProperty(AObjectProxy, PropInfo, Args, VarResult);
  end;
end;

function TMSSCScriptProxyServer.SetProperty(AObjectProxy: TObjectProxy; PropInfo: PPropInfo;
  Args: TInvokeArguments): HResult;
var
  AObject: TObject;
  OldProcName: string;
begin
  AObject := AObjectProxy.GetObject;
  case PropInfo^.PropType^.Kind of
    tkMethod:
      begin
        if (PropInfo^.PropType^ = System.TypeInfo(TNotifyEvent)) and (AObject is TComponent) then
        begin
          OldProcName := Script.GetEventHandler(AObjectProxy, PropInfo^.Name);
          if Args.Count = 1 then
          begin
            if VarIsArray(Args[0]) then
            begin
              if OldProcName <> '' then
                Script.SetEventHandler(AObjectProxy, PropInfo^.Name, '');
              Result := inherited SetProperty(AObjectProxy, PropInfo, Args)
            end else begin
              Script.SetEventHandler(AObjectProxy, PropInfo^.Name, Args[0]);
              Result := S_OK;
            end;
          end else
            Result := DISP_E_BADPARAMCOUNT;
          Exit;
        end else
          Result := inherited SetProperty(AObjectProxy, PropInfo, Args);
      end;
  else
    Result := inherited SetProperty(AObjectProxy, PropInfo, Args);
  end;
end;

{ TScriptHandlerThunk }
constructor TScriptHandlerThunk.Create(AComponent: TComponent; const APropName, AProcName: string;
  AHandler: TNotifyEventThunkEvent);
begin
  FInstance := AComponent;
  FPropName := APropName;
  FProcName := AProcName;
  OnEvent := AHandler;
end;

{ TCustomMSSCScript }
constructor TCustomMSSCScript.Create(AOwner: TComponent);
begin
  inherited;
  FLanguage := LangVBScript;
  FMainProc := DefaultMainProc;
  FObjectName := DefaultObjectName;
  FResetNeeded := True;
  FTimeout := -1;
end;

destructor TCustomMSSCScript.Destroy;
begin
  Reset;
  inherited;
end;

procedure TCustomMSSCScript.HandleError(const ErrorObject: IScriptError);
var
  ErrorMsg: string;
begin
  with ErrorObject do
  begin
    ErrorMsg := Description;
    if ErrorMsg <> '' then
      raise EScriptError.Create(Format('%s'#13#10'%s (%d, %d)',
        [Source, ErrorMsg, Line, Column]), Line, Column) else
      raise EScriptError.Create(Exception(ExceptObject).Message, -1, -1);
  end;
end;

procedure TCustomMSSCScript.LinesChanged(Sender: TObject);
begin
  inherited LinesChanged(Sender);
  FResetNeeded := True;
end;

function TCustomMSSCScript.CreateObject: OleVariant;
begin
  Result := Unassigned;
  if Assigned(FOnCreateObject) then
    FOnCreateObject(Self, ProxyServer, Result);
end;

function TCustomMSSCScript.CreateProxyServer: TObjectProxyServer;
begin
  Result := TMSSCScriptProxyServer.Create(Self);
end;

procedure TCustomMSSCScript.CreateScriptControl;
var
  I: Integer;
  Obj, Tmp: OleVariant;
begin
  if FInExecute then Exit;

  if not Assigned(FScriptControl) then
    FScriptControl := TScriptControl.Create(nil);

  with ScriptControl do
  begin
    AllowUI := FAllowUI;
    Timeout := FTimeout;
    UseSafeSubset := True;
    if FResetNeeded then
    begin
      Language := FLanguage;
      Reset;
      Obj := CreateObject;
      if not VarIsNull(Obj) then
        if VarIsArray(Obj) then
        begin
          for I := VarArrayLowBound(Obj, 1) to VarArrayHighBound(Obj, 1) do
          begin
            Tmp := Obj[I];
            AddObject(Tmp[0], VarToInterface(Tmp[1]), True);
          end;
        end else
          AddObject(FObjectName, VarToInterface(Obj), True);
      AddCode(Lines.Text);
      FResetNeeded := False;
    end;
  end;
end;

procedure TCustomMSSCScript.NotifyEventHandler(EventThunk: TEventThunk; Sender: TObject);
var
  Obj: OleVariant;
begin
  Obj := TObjectProxy.Create(Sender, ProxyServer) as IDispatch;
  Run(TScriptHandlerThunk(EventThunk).ProcName, Obj);
end;

function TCustomMSSCScript.IsMainProcStored: Boolean;
begin
  Result := FMainProc <> DefaultMainProc;
end;

function TCustomMSSCScript.IsObjectNameStored: Boolean;
begin
  Result := FObjectName <> DefaultObjectName;
end;

procedure TCustomMSSCScript.SetLanguage(Value: string);
begin
  if FLanguage <> Value then
  begin
    FLanguage := Value;
    FResetNeeded := True;
  end;
end;

procedure TCustomMSSCScript.SetMainProc(Value: string);
begin
  if FMainProc <> Value then
    FMainProc := Value;
end;

procedure TCustomMSSCScript.SetObjectName(Value: string);
begin
  if FObjectName <> Value then
  begin
    FObjectName := Value;
    FResetNeeded := True;
  end;
end;

function TCustomMSSCScript.Evalute(const Expression: string): OleVariant;
var
  SaveExecute: Boolean;
begin
  CreateScriptControl;
  SaveExecute := FInExecute;
  try
    FInExecute := True;
    try
      Result := ScriptControl.Eval(Expression);
    except
      HandleError(ScriptControl.ErrorObject);
    end;
  finally
    FInExecute := SaveExecute;
  end;
end;

function TCustomMSSCScript.Execute(const Parameters: OleVariant): OleVariant;
begin
  Result := Run(MainProc, Parameters);
end;

procedure TCustomMSSCScript.ExecuteStatement(const Statement: string);
var
  SaveExecute: Boolean;
begin
  CreateScriptControl;
  SaveExecute := FInExecute;
  try
    FInExecute := True;
    try
      ScriptControl.ExecuteStatement(Statement);
    except
      HandleError(ScriptControl.ErrorObject);
    end;
  finally
    FInExecute := SaveExecute;
  end;
end;

function TCustomMSSCScript.Run(const ProcedureName: string; const Parameters: OleVariant): OleVariant;
var
  P: PSafeArray;
  Tmp: Variant;
  SaveExecute: Boolean;
begin
  CreateScriptControl;

  SaveExecute := FInExecute;
  try
    FInExecute := True;

    if VarIsEmpty(Parameters) then
      Tmp := VarArrayCreate([1, 0], varVariant)
    else if not VarIsArray(Parameters) then
      Tmp := VarArrayOf([Parameters])
    else
      Tmp := Parameters;

    try
      IsMultithread := True;
      P := PSafeArray(TVarData(Tmp).VArray);
      Result := ScriptControl.Run(ProcedureName, P);
    except
      HandleError(ScriptControl.ErrorObject);
    end;
  finally
    FInExecute := SaveExecute;
  end;
end;

procedure TCustomMSSCScript.Reset;
var
  I: Integer;
  EventThunk: TScriptHandlerThunk;
begin
  FreeObject(FScriptControl);
  for I := ListCount(FEventThunks) - 1 downto 0 do
  begin
    EventThunk := FEventThunks[I];
    SetNotifyEventHandler(EventThunk.Instance, EventThunk.PropName, nil);
  end;
end;

procedure TCustomMSSCScript.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
  EventThunk: TScriptHandlerThunk;
begin
  inherited;
  if (Operation = opRemove) then
  begin
    for I := ListCount(FEventThunks) - 1 downto 0 do
    begin
      EventThunk := FEventThunks[I];
      if EventThunk.Instance = AComponent then
        SetNotifyEventHandler(AComponent, EventThunk.PropName, nil);
    end;
  end;
end;

function TCustomMSSCScript.FindEventThunk(AInstance: TComponent; const APropName: string): TScriptHandlerThunk;
var
  I: Integer;
begin
  for I := 0 to ListCount(FEventThunks) - 1 do
  begin
    Result := FEventThunks[I];
    if (Result.Instance = AInstance) and
      (AnsiCompareText(Result.PropName, APropName) = 0) then Exit;
  end;
  Result := nil;
end;

procedure TCustomMSSCScript.SetNotifyEventHandler(AInstance: TComponent;
  const PropName: string; EventThunk: TScriptHandlerThunk);
var
  Method: TMethod;
  TmpEventThunk: TScriptHandlerThunk;
begin
  if Assigned(EventThunk) then
    Method := EventThunk.GetMethod
  else begin
    Method.Code := nil;
    Method.Data := nil;
  end;

  SetMethodProp(AInstance, GetPropInfo(AInstance.ClassInfo, PropName), Method);

  TmpEventThunk := FindEventThunk(AInstance, PropName);
  if Assigned(TmpEventThunk) then
  begin
    ListRemove(FEventThunks, TmpEventThunk);
    TmpEventThunk.Free;
  end;

  if Assigned(EventThunk) then
  begin
    FreeNotification(AInstance);
    ListAdd(FEventThunks, EventThunk);
  end;
end;

function TCustomMSSCScript.GetEventHandler(AObjectProxy: TObjectProxy;
  const PropName: string): string;
var
  AInstance: TComponent;
  PropInfo: PPropInfo;
  EventThunk: TScriptHandlerThunk;
begin
  PropInfo := AObjectProxy.GetPropInfo(PropName);
  if Assigned(PropInfo) and (PropInfo^.PropType^ = System.TypeInfo(TNotifyEvent)) then
  begin
    AInstance := AObjectProxy.GetObject as TComponent;
    EventThunk := FindEventThunk(AInstance, PropName);
    if Assigned(EventThunk) then
      Result := EventThunk.ProcName else
      Result := '';
  end else
    PropertyNotFound;
end;

procedure TCustomMSSCScript.SetEventHandler(AObjectProxy: TObjectProxy;
  const PropName, ProcName: string);
var
  AObject: TComponent;
  PropInfo: PPropInfo;
  EventThunk: TScriptHandlerThunk;
begin
  PropInfo := AObjectProxy.GetPropInfo(PropName);
  if Assigned(PropInfo) and (PropInfo^.PropType^ = System.TypeInfo(TNotifyEvent)) then
  begin
    AObject := AObjectProxy.GetObject as TComponent;
    if ProcName <> '' then
      EventThunk := TScriptHandlerThunk.Create(AObject, PropName, ProcName, NotifyEventHandler) else
      EventThunk := nil;
    try
      SetNotifyEventHandler(AObject, PropName, EventThunk)
    except
      EventThunk.Free;
      raise;
    end;
  end else
    PropertyNotFound;
end;

function TCustomMSSCScript.StoreLanguage: Boolean;
begin
  Result := CompareText(FLanguage, LangVBScript) <> 0
end;

initialization
  ProxyServerClass := TVCLProxyServer;

end.


