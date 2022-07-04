{ Copyright (C) 1998-2008, written by Mike Shkolnik, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com

  The TSMScript is a wrapper for MS Script Control which allow to
  execute any script for VBScript, JavaScript languages (and any other registred language)

MS ScriptControl:
http://download.microsoft.com/download/winscript56/Install/1.0/W982KMeXP/EN-US/sct10en.exe
must be installed on Win 98, ME and NT 4.0 (in Win 2000 is installed by default)
}
unit SMScript;

interface

{$IFDEF VER100}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER110}
  {$DEFINE SMForDelphi3}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER125}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER185}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
{$ENDIF}

{$IFDEF VER190}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
{$ENDIF}

{$IFDEF VER200}
  {$DEFINE SMForDelphi3}
  {$DEFINE SMForDelphi4}
  {$DEFINE SMForDelphi5}
  {$DEFINE SMForDelphi6}
  {$DEFINE SMForDelphi7}
  {$DEFINE SMForDelphi2005}
  {$DEFINE SMForDelphi2006}
  {$IFDEF BCB}
    {$DEFINE SMForBCB2006}
    {$DEFINE SMForBCB2007}
    {$DEFINE SMForBCB2009}
  {$ENDIF}
  {$DEFINE SMForDelphi2007}
  {$DEFINE SMForRADStudio2007}
  {$DEFINE SMForDelphi2009}
{$ENDIF}

uses Classes, ActiveX, Windows, SysUtils, TypInfo;

type
  TSMScriptExecutor = class;

  TSMSEError = class
  private
    { Private declarations }
    FScriptExecutor: TSMScriptExecutor;

    FNumber: string;
    FSource: string;
    FDescription: string;
    FText: string;

    FLine: Integer;
    FColumn: Integer;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AScriptExecutor: TSMScriptExecutor);

    procedure Clear;
  published
    { Published declarations }
    property Number: string read FNumber write FNumber;
    property Source: string read FSource write FSource;

    property Description: string read FDescription write FDescription;
    property Text: string read FText write FText;

    property Line: Integer read FLine write FLine;
    property Column: Integer read FColumn write FColumn;
  end;

  TSMSEModule = class;

  {type of procedure: HasReturnValue?}
  TSMSEProcedureType = (ptProcedure, ptFunction);

  {procedure in module}
  TSMSEProcedure = class(TCollectionItem)
  private
    { Private declarations }
    FProcName: string;
    FNumArg: Integer;
    FProcedureType: TSMSEProcedureType;
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property ProcName: string read FProcName write FProcName;
    property NumArg: Integer read FNumArg write FNumArg default 0;
    property ProcedureType: TSMSEProcedureType read FProcedureType write FProcedureType default ptProcedure;
  end;

  {collection of procedures in module}
  TSMSEProcedures = class(TCollection)
  private
    FModule: TSMSEModule;

    function GetProcedure(Index: Integer): TSMSEProcedure;
    procedure SetProcedure(Index: Integer; Value: TSMSEProcedure);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AModule: TSMSEModule);

    property Items[Index: Integer]: TSMSEProcedure read GetProcedure write SetProcedure; default;
  end;

  {module in script}
  TSMSEModule = class(TCollectionItem)
  private
    { Private declarations }
    FModuleName: string;
    FProcedures: TSMSEProcedures;

    function GetProcedures: TSMSEProcedures;
    procedure SetProcedures(Value: TSMSEProcedures);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure AddCode(const Code: string);
    function Eval(const Expression: string): Variant;
    procedure ExecuteStatement(const Statement: string);
    function Run(const ProcedureName: string; Parameters: OLEVariant): OLEVariant;
  published
    { Published declarations }
    property ModuleName: string read FModuleName write FModuleName;
    property Procedures: TSMSEProcedures read GetProcedures write SetProcedures;
  end;

  {module collection}
  TSMSEModules = class(TCollection)
  private
    FScriptExecutor: TSMScriptExecutor;

    function GetModule(Index: Integer): TSMSEModule;
    procedure SetModule(Index: Integer; Value: TSMSEModule);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AScriptExecutor: TSMScriptExecutor);
    function GetModuleByName(const Value: string): TSMSEModule;

    property Items[Index: Integer]: TSMSEModule read GetModule write SetModule; default;
  end;

  LongWord = dWORD;
  PLongWord = PDWORD;

  EInvalidParamCount = class(Exception)
  end;

  EInvalidParamType = class(Exception)
  end;

  IQueryPersistent = interface
  ['{26F5B6E1-9DA5-11D3-BCAD-00902759A497}']
    function GetPersistent: TPersistent;
  end;

  IEnumVariant = interface(IUnknown)
    ['{00020404-0000-0000-C000-000000000046}']
    function Next(celt: LongWord; var rgvar : OleVariant;
      pceltFetched: PLongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  end;

  TVCLProvider = class(TInterfacedObject, IDispatch, IQueryPersistent)
  private
    FOwner: TPersistent;
    FScriptControl: TSMScriptExecutor;
    procedure DoCreateControl(AName, AClassName: WideString; WithEvents: Boolean);
    function SetVCLProperty(PropInfo: PPropInfo; Argument: TVariantArg): HRESULT;
    function GetVCLProperty(PropInfo: PPropInfo; dps: TDispParams;
      PDispIds: PDispIdList; var Value: OleVariant): HRESULT;
    { IDispatch }
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
    { IQueryPersistent }
    function GetPersistent: TPersistent;
  protected
    function DoInvoke (DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var dps : TDispParams; pDispIds : PDispIdList;
      VarResult, ExcepInfo, ArgErr: Pointer): HResult; virtual;
  public
    constructor Create(AOwner: TPersistent; ScriptControl: TSMScriptExecutor);
    destructor Destroy; override;
  end;

  TVCLEnumerator = class(TInterfacedObject, IEnumVariant)
  private
    FEnumPosition: Integer;
    FOwner: TPersistent;
    FScriptControl: TSMScriptExecutor;
    { IEnumVariant }
    function Next(celt: LongWord; var rgvar : OleVariant;
      pceltFetched: PLongWord): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumVariant): HResult; stdcall;
  public
    constructor Create(AOwner: TPersistent; AScriptControl: TSMScriptExecutor);
  end;

  TSMScriptLanguage = (slCustom, slVBScript, slJavaScript);
  TSMScriptExecutor = class(TComponent)
  private
    FAllowUI: Boolean;

    FLanguage: TSMScriptLanguage;
    FLanguageStr: string;

    FTimeOut: Integer;

    FScriptPrepared: Boolean;

    FScriptBody: TStrings;
    FUseSafeSubset: Boolean;

    FLastError: TSMSEError;

    FModules: TSMSEModules;

    {for VCL objects}
    FProviderList: TList;

    procedure SetLanguage(Value: TSMScriptLanguage);
    procedure SetLanguageStr(Value: string);

    procedure SetScriptBody(Value: TStrings);
    function GetLastError: TSMSEError;

    function GetModules: TSMSEModules;
    procedure SetModules(Value: TSMSEModules);
  protected
    {for VCL objects}
    function GetProvider(AOwner: TPersistent): IDispatch;
    procedure ProviderDestroyed(Address: Pointer);
  public
    ScriptControl: Variant;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadScriptFunctions(const ScriptLib: string; list: TStrings; ClearList: Boolean);

    procedure LoadMacro(const FileName: string);

    procedure Prepare;
    procedure ParseModules;

    procedure Reset;
    procedure AddCode(const Code: string);
    function Eval(const Expression: string): Variant;
    procedure ExecuteStatement(const Statement: string);
    function Run(const ProcedureName: string; Parameters: OLEVariant): OLEVariant;
    procedure AddObject(const Name: WideString; const Obj: IDispatch; AddMembers: WordBool);

    {for VCL objects}
    procedure RegisterClass(const Name: String; AClass: TPersistent);

    property LastError: TSMSEError read GetLastError;
  published
    {for VCL objects}
    procedure OnChangeHandler(Sender: TObject);
    procedure OnClickHandler(Sender: TObject);
    procedure OnEnterHandler(Sender: TObject);
    procedure OnExitHandler(Sender: TObject);
    procedure OnTimerHandler(Sender: TObject);

    property AllowUI: Boolean read FAllowUI write FAllowUI default True;

    property Language: TSMScriptLanguage read FLanguage write SetLanguage default slVBScript;
    property LanguageStr: string read FLanguageStr write SetLanguageStr;

    property Modules: TSMSEModules read GetModules write SetModules;
    property TimeOut: Integer read FTimeOut write FTimeOut default -1;
    property UseSafeSubset: Boolean read FUseSafeSubset write FUseSafeSubset default True;
    property ScriptBody: TStrings read FScriptBody write SetScriptBody;
  end;

procedure Register;

implementation
uses ComObj, Controls, forms, Graphics
     {$IFDEF SMForDelphi6} , Variants {$ENDIF}
     ;

{$R SMScript.DCR}

const
  DefaultModule = 'Global';

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMScriptExecutor]);
end;


{ TSMSEError }
constructor TSMSEError.Create(AScriptExecutor: TSMScriptExecutor);
begin
  inherited Create;

  FScriptExecutor := AScriptExecutor
end;

procedure TSMSEError.Clear;
begin
  FScriptExecutor.ScriptControl.Error.Clear
end;

{ TSMSEProcedure }
procedure TSMSEProcedure.Assign(Source: TPersistent); 
begin
  if Source is TSMSEProcedure then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      NumArg := TSMSEProcedure(Source).NumArg;
      ProcedureType := TSMSEProcedure(Source).ProcedureType;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;


{ TSMSEProcedures }
constructor TSMSEProcedures.Create(AModule: TSMSEModule);
begin
  inherited Create(TSMSEProcedure);

  FModule := AModule;
end;

function TSMSEProcedures.GetOwner: TPersistent; 
begin
  Result := FModule;
end;

function TSMSEProcedures.GetProcedure(Index: Integer): TSMSEProcedure;
begin
  Result := TSMSEProcedure(inherited Items[Index]);
end;

procedure TSMSEProcedures.SetProcedure(Index: Integer; Value: TSMSEProcedure);
begin
  Items[Index].Assign(Value);
end;


{ TSMSEModule }
constructor TSMSEModule.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FProcedures := TSMSEProcedures.Create(Self);
end;

destructor TSMSEModule.Destroy;
begin
  FProcedures.Free;

  inherited Destroy
end;

procedure TSMSEModule.Assign(Source: TPersistent);
begin
  if Source is TSMSEModule then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      Procedures := TSMSEModule(Source).Procedures;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TSMSEModule.GetProcedures: TSMSEProcedures;
begin
  Result := FProcedures;
end;

procedure TSMSEModule.SetProcedures(Value: TSMSEProcedures);
begin
  FProcedures.Assign(Value);
end;

procedure TSMSEModule.AddCode(const Code: string);
begin
end;

function TSMSEModule.Eval(const Expression: string): Variant;
begin
end;

procedure TSMSEModule.ExecuteStatement(const Statement: string);
begin
end;

function TSMSEModule.Run(const ProcedureName: string; Parameters: OLEVariant): OLEVariant;
begin
end;


{ TSMSEModules }
constructor TSMSEModules.Create(AScriptExecutor: TSMScriptExecutor);
begin
  inherited Create(TSMSEModule);

  FScriptExecutor := AScriptExecutor
end;

function TSMSEModules.GetOwner: TPersistent;
begin
  Result := FScriptExecutor
end;

function TSMSEModules.GetModule(Index: Integer): TSMSEModule;
begin
  Result := TSMSEModule(inherited Items[Index]);
end;

procedure TSMSEModules.SetModule(Index: Integer; Value: TSMSEModule);
begin
  Items[Index].Assign(Value);
end;

function TSMSEModules.GetModuleByName(const Value: string): TSMSEModule;
var
  i: Integer;
begin
  i := FScriptExecutor.ScriptControl.Modules[Value].Index;
  Result := Items[i]
end;


{ TSMScriptExecutor }
constructor TSMScriptExecutor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FProviderList := TList.Create;
  FModules := TSMSEModules.Create(Self);

  FScriptBody := TStringList.Create;
  FScriptPrepared := True;

  Language := slVBScript;

  AllowUI := True;
  UseSafeSubset := True;
end;

destructor TSMScriptExecutor.Destroy;
begin
//  ScriptControl.Reset;
  ScriptControl := UnAssigned;

  FScriptBody.Free;
  FModules.Free;

  if Assigned(FLastError) then
    FLastError.Free;

  FProviderList.Free;
  FProviderList := nil;

  inherited Destroy;
end;

const
  arrScriptLanguage: array [TSMScriptLanguage] of string =
       ('', 'VBSCRIPT', 'JAVASCRIPT');

procedure TSMScriptExecutor.SetLanguage(Value: TSMScriptLanguage);
begin
  if (FLanguage <> Value) then
  begin
    FLanguage := Value;
    FLanguageStr := arrScriptLanguage[Value]
  end;
end;

procedure TSMScriptExecutor.SetLanguageStr(Value: string);
var
  strUpperValue: string;
begin
  if (FLanguageStr <> Value) then
  begin
    FLanguageStr := Value;
    strUpperValue := UpperCase(Value);
    if (strUpperValue = arrScriptLanguage[slVBScript]) then
      FLanguage := slVBScript
    else
    if (strUpperValue = arrScriptLanguage[slJavaScript]) then
      FLanguage := slJavaScript
    else
      FLanguage := slCustom;
  end;
end;

procedure TSMScriptExecutor.SetScriptBody(Value: TStrings);
begin
  FScriptBody.Assign(Value);
  FScriptPrepared := False;
end;

procedure TSMScriptExecutor.LoadMacro(const FileName: string);
begin
  ScriptBody.LoadFromFile(FileName)
end;

function TSMScriptExecutor.GetModules: TSMSEModules;
begin
  Result := FModules;
end;

procedure TSMScriptExecutor.SetModules(Value: TSMSEModules);
begin
  FModules.Assign(Value);
end;

function TSMScriptExecutor.GetLastError: TSMSEError;
begin
  if not Assigned(FLastError) then
    FLastError := TSMSEError.Create(Self);

  FLastError.Number := '';
  FLastError.Source := '';
  FLastError.Description := '';
  FLastError.Text := '';
  FLastError.Line := -1;
  FLastError.Column := -1;
  try
    if not VarIsEmpty(ScriptControl) and not VarIsEmpty(ScriptControl.Error) then
    begin
      FLastError.Number := ScriptControl.Error.Number;
      FLastError.Source := ScriptControl.Error.Source;
      FLastError.Description := ScriptControl.Error.Description;
      FLastError.Text := ScriptControl.Error.Text;
      FLastError.Line := ScriptControl.Error.Line;
      FLastError.Column := ScriptControl.Error.Column;
    end;
  except
  end;
  Result := FLastError;
end;

procedure TSMScriptExecutor.Prepare;
begin
  if VarIsNull(ScriptControl) or
     VarIsEmpty(ScriptControl) then
  begin
    ScriptControl := CreateOleObject('MSScriptControl.ScriptControl');
  end;

  if (FLanguage = slCustom) then
    ScriptControl.Language := FLanguageStr
  else
    ScriptControl.Language := arrScriptLanguage[FLanguage];
  ScriptControl.AllowUI := AllowUI;
  ScriptControl.UseSafeSubset := UseSafeSubset;
  {set the maximum run time allowed
   PS: -1 stands for unlimited }
  ScriptControl.TimeOut := FTimeOut;

  Reset;

  FScriptPrepared := True;
end;

procedure TSMScriptExecutor.ParseModules;
var
  i, j: Integer;
begin
  {build module and parameter lists}
//  if (ScriptBody.Count > 0) then
//    AddCode(ScriptBody.Text);

  {clear current list of modules}
  Modules.Clear;

  {load a list of modules in current script}
  for i := 1 to ScriptControl.Modules.Count do
  begin
    with (Modules.Add as TSMSEModule) do
    begin
      ModuleName := ScriptControl.Modules.Item[i].Name;

      for j := 1 to ScriptControl.Modules.Item[i].Procedures.Count do
      begin
        with (Procedures.Add as TSMSEProcedure) do
        begin
          { name of subroutine }
          ProcName := ScriptControl.Modules.Item[i].Procedures.Item[j].Name;

          { total number of argument, if any }
          NumArg := ScriptControl.Modules.Item[i].Procedures.Item[j].NumArgs;

          { type of routine }
          if ScriptControl.Modules.Item[i].Procedures.Item[j].HasReturnValue then
            ProcedureType := ptFunction
          else
            ProcedureType := ptProcedure
        end;
      end;
    end;
  end;
end;

{clear a cache from previos executed script}
procedure TSMScriptExecutor.Reset;
begin
  ScriptControl.Reset;
end;

{load a script body}
procedure TSMScriptExecutor.AddCode(const Code: string);
begin
  Prepare;

  ScriptControl.Modules.Item[DefaultModule].AddCode(Code{ScriptBody.Text});
//  ParseModules;
end;

{execute a statement within the context of the global module

Sample:
ScriptBody.Text := 'Sub HelloWorld(str)'+
                   '  MsgBox str & " - , Hello from the large world of scripts"'+
                   'End Sub'
SMScriptExecutor.ExecuteStatement('Call Hello("Scalabium TSMScriptExecutor")');
or define a variable:
SMScriptExecutor.ExecuteStatement('str = "Scalabium TSMScriptExecutor"');
}
procedure TSMScriptExecutor.ExecuteStatement(const Statement: string);
begin
  Prepare;

  {Execute it as a direct statement}
  ScriptControl.ExecuteStatement(Statement);
end;

{evaluate an expression within the context of the global module

Sample:
1. check a value of variable:
bool := SMScriptExecutor.Eval('x = 50');

2. return a variable value:
int := SMScriptExecutor.Eval('x');
}
function TSMScriptExecutor.Eval(const Expression: string): Variant;
begin
  Prepare;

  Result := ScriptControl.Eval(Expression);
end;

{run a procedure defined in the global module}
function TSMScriptExecutor.Run(const ProcedureName: string; Parameters: OLEVariant): OLEVariant;
begin
//  if not FScriptPrepared then
//    Prepare;
//    ParseModules;

  if VarIsEmpty(Parameters) {or VarIsNull(Parameters) }then
//    ScriptControl.Run(ProcedureName)
    ScriptControl.Modules.Item[DefaultModule].Run(ProcedureName)
  else
    ScriptControl.Modules.Item[DefaultModule].Run(ProcedureName, Parameters);
end;

{add to script engine some COM-object which will be "visible" from script}
procedure TSMScriptExecutor.AddObject(const Name: WideString; const Obj: IDispatch; AddMembers: WordBool);
begin
  ScriptControl.AddObject(Name, Obj, AddMembers);
//  ScriptControl.Modules.Item[DefaultModule].AddObject(Name, Obj, AddMembers);
end;

procedure TSMScriptExecutor.LoadScriptFunctions(const ScriptLib: string; list: TStrings; ClearList: Boolean);

  procedure LoadInterface(TypeInfo: ITypeInfo; TypeAttr: PTypeAttr);
  var
    AName: WideString;
    ADocString: WideString;
    AHelpContext: LongInt;
    FuncDesc: PFuncDesc;
    i, j: Integer;
    Names: PBStrList;
    cNames: Integer;

    strItem: string;
  begin
    TypeInfo.GetDocumentation(-1, @AName, @ADocString, @AHelpContext, nil);

    New(Names);
    try
      // load functions
      for i := 0 to TypeAttr.cFuncs - 1 do
      begin
        TypeInfo.GetFuncDesc(i, FuncDesc);
        try
          TypeInfo.GetDocumentation(FuncDesc.memid, @AName, @ADocString, @AHelpContext, nil);

          strItem := AName;
          if FuncDesc.cParams > 0 then
          begin
            strItem := strItem + '(';
            // load parameters
            TypeInfo.GetNames(FuncDesc.memid, Names, SizeOf(TBStrList), cNames);

            // Skip Names[0] - it's the function name
            for j := 1 to FuncDesc.cParams do
              if j < 2 then
                strItem := strItem + Names[j]
              else
                strItem := strItem + ', ' + Names[j];
            strItem := strItem + ')';
          end;
          if (ADocString <> '') then
            strItem := strItem + #9 + ADocString;
          list.Add(strItem);
        finally
          TypeInfo.ReleaseFuncDesc(FuncDesc);
        end;
      end;
    finally
      Dispose(Names);
    end;
  end;

var
  FLib: ITypeLib;
  TypeInfo: ITypeInfo;
  TypeAttr: PTypeAttr;
  i: Integer;
begin
  if ClearList then
    list.Clear;

  {load a library}
  OleCheck(LoadTypeLib(PWideChar(WideString(ScriptLib)), FLib));

  for i := 0 to FLib.GetTypeInfoCount - 1 do
  begin
    FLib.GetTypeInfo(i, TypeInfo);
    OleCheck(TypeInfo.GetTypeAttr(TypeAttr));
    try
      if (TypeAttr.typeKind in [TKIND_DISPATCH, TKIND_INTERFACE]) then
        LoadInterface(TypeInfo, TypeAttr);
    finally
      TypeInfo.ReleaseTypeAttr(TypeAttr);
    end;
  end;
end;

function ValidType(Argument: TVariantArg; TypeId: Integer;
  RaiseException: Boolean): Boolean;
begin
 Result := Argument.vt = TypeId;
 if RaiseException and (not Result) then
   raise EInvalidParamType.Create('');
end;

function CheckArgCount(Count: Integer; Accepted: array of Integer;
  RaiseException: Boolean): Boolean;
var
  I: Integer;
begin
  Result := FALSE;
  for I := Low(Accepted) to High(Accepted) do
  begin
    Result := Accepted[I] = Count;
    if Result then
      Break;
  end;
  if RaiseException and (not Result) then
    raise EInvalidParamCount.Create('');
end;

function IntValue(Argument: TVariantArg): Integer;
var
 VT: Word;
 ByRef: Boolean;
begin
 VT := Argument.vt;
 ByRef := (VT and VT_BYREF) = VT_BYREF;
 if ByRef then
 begin
   VT := VT and (not VT_BYREF);
   case VT of
     VT_I1: Result := Argument.pbVal^;
     VT_I2: Result := Argument.piVal^;
     VT_VARIANT: Result := Argument.pvarVal^;
   else
     Result := Argument.plVal^;
   end;
 end
 else
 case VT of
   VT_I1: Result := Argument.bVal;
   VT_I2: Result := Argument.iVal;
 else
   Result := Argument.lVal;
 end;
end;

type
  PNamesArray = ^TNamesArray;
  TNamesArray = array[0..0] of PWideChar;
  PDispIdsArray = ^TDispIdsArray;
  TDispIdsArray = array[0..0] of Integer;
  TVariantList = array [0..0] of OleVariant;

const
  DISPID_CONTROLS    = 1;
  DISPID_COUNT       = 2;
  DISPID_ADD         = 3;
  DISPID_HASPROPERTY = 4;

{ TVCLProvider }
constructor TVCLProvider.Create(AOwner: TPersistent; ScriptControl: TSMScriptExecutor);
begin
  inherited Create;

  FOwner := AOwner;
  FScriptControl := ScriptControl;
end;

function TVCLProvider.Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
  Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult;
var
  dps : TDispParams absolute Params;
  HasParams : boolean;
  pDispIds : PDispIdList;
  iDispIdsSize : integer;
  WS: WideString;
  I: Integer;
begin
  pDispIds := nil;
  iDispIdsSize := 0;
  HasParams := (dps.cArgs > 0);
  if HasParams then
  begin
    iDispIdsSize := dps.cArgs * SizeOf(TDispId);
    GetMem(pDispIds, iDispIdsSize);
  end;
  try
    if HasParams then
      for I := 0 to dps.cArgs - 1 do
        pDispIds^[I] := dps.cArgs - 1 - I;
    try
      Result := DoInvoke(DispId, IID, LocaleID, Flags, dps, pDispIds, VarResult, ExcepInfo, ArgErr);
    except
      on E: EInvalidParamCount do Result := DISP_E_BADPARAMCOUNT;
      on E: EInvalidParamType do  Result := DISP_E_BADVARTYPE;
      on E: Exception do
      begin
        if Assigned(ExcepInfo) then
        begin
          FillChar(ExcepInfo^, SizeOf(TExcepInfo), 0);
          TExcepInfo(ExcepInfo^).wCode := 1001;
          TExcepInfo(ExcepInfo^).BStrSource := SysAllocString('TVCLProvider');
          WS := E.Message;
          TExcepInfo(ExcepInfo^).bstrDescription := SysAllocString(PWideChar(WS));
        end;
        Result := DISP_E_EXCEPTION;
      end;
    end;
  finally
    if HasParams then
      FreeMem(pDispIds, iDispIdsSize);
  end;
end;

function TVCLProvider.GetIDsOfNames(const IID: TGUID; Names: Pointer; NameCount,
  LocaleID: Integer; DispIDs: Pointer): HResult;
var
  S: string;
  Info: PPropInfo;
begin
  Result := S_OK;

  { get the method/property name }
  S := PNamesArray(Names)[0];
  { check if such property exists }
  Info := GetPropInfo(FOwner.ClassInfo, S);
  if Assigned(Info) then
  begin
    { if property exists, return the address for PropInfo as DispId }
    PDispIdsArray(DispIds)[0] := Integer(Info);
  end
  else
  { if property not exists, check the predefined custom functions}
  if CompareText(S, 'CONTROLS') = 0 then
  begin
    if (FOwner is TWinControl) then
      PDispIdsArray(DispIds)[0] := DISPID_CONTROLS
    else
      Result := DISP_E_UNKNOWNNAME;
  end
  else
  if CompareText(S, 'COUNT') = 0 then
  begin
    Result := S_OK;
    {call for TCollection and TStrings only}
    if (FOwner is TCollection) or (FOwner is TStrings)
       or (FOwner is TWinControl) then
      PDispIdsArray(DispIds)[0] := DISPID_COUNT
    else
      Result := DISP_E_UNKNOWNNAME;
  end
  else
  if CompareText(S, 'ADD') = 0 then
  begin
    Result := S_OK;
    {call for TCollection and TStrings only}
    if (FOwner is TCollection) or (FOwner is TStrings) or
       (FOwner is TWinControl) then
      PDispIdsArray(DispIds)[0] := DISPID_ADD
    else
      Result := DISP_E_UNKNOWNNAME;
  end
  else
  if CompareText(S, 'HASPROPERTY') = 0 then
    PDispIdsArray(DispIds)[0] := DISPID_HASPROPERTY
  else
    Result := DISP_E_UNKNOWNNAME;
end;

function TVCLProvider.DoInvoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var dps: TDispParams;
  pDispIds: PDispIdList; VarResult, ExcepInfo, ArgErr: Pointer): HResult;

 {check if parameter with Index have a same type}
 function _ValidType(Index, TypeId: Integer; RaiseException: Boolean): Boolean;
 begin
   Result := ValidType(dps.rgvarg^[pDispIds^[Index]], TypeId, RaiseException);
 end;

 {get the integer value for parameter by Index}
 function _IntValue(Index: Integer): Integer;
 begin
   Result := IntValue(dps.rgvarg^[pDispIds^[Index]]);
 end;

var
  S: string;
  Put: Boolean;
  I: Integer;
  P: TPersistent;
  B: Boolean;
  OutValue: OleVariant;
begin
  Result := S_OK;
  case DispId of
   DISPID_NEWENUM: begin
        {get the IEnumVariant interface for object (for ForEach)
         and create the class with realization}
        OleVariant(VarResult^) := TVCLEnumerator.Create(FOwner, FScriptControl) as IEnumVariant;
      end;
   DISPID_CONTROLS: begin
        {Controls function called}
        with FOwner as TWinControl do
        begin
          {check parameter}
          CheckArgCount(dps.cArgs, [1], TRUE);
          P := nil;
          if _ValidType(0, VT_BSTR, FALSE) then
          begin
            {if this is string parameter, must find the child control with such name}
            S := dps.rgvarg^[pDispIds^[0]].bstrVal;
            for I := 0 to Pred(ControlCount) do
              if CompareText(S, Controls[I].Name) = 0 then
              begin
                P := Controls[I];
                Break;
              end;
          end
          else
          begin
            {else find the child control by Index}
            I := _IntValue(0);
            P := Controls[I];
          end;
          {control not found}
          if not Assigned(P) then
            raise EInvalidParamType.Create('');
          {return the IDispatch for component}
          OleVariant(VarResult^) := FScriptControl.GetProvider(P);
        end;
      end;
   DISPID_COUNT: begin
        {check if Count function called without parameters}
        CheckArgCount(dps.cArgs, [0], TRUE);
        {return the count for child controls}
        if FOwner is TWinControl then
          OleVariant(VarResult^) := (FOwner as TWinControl).ControlCount;

        {return the collection count}
        if FOwner is TCollection then
          OleVariant(VarResult^) := TCollection(FOwner).Count
        else
        {return the TStrings count}
        if FOwner is TStrings then
          OleVariant(VarResult^) := TStrings(FOwner).Count;
      end;
   DISPID_ADD: begin
        {ADD function called}
        if FOwner is TWinControl then
        begin
          {check parameter count}
          CheckArgCount(dps.cArgs, [2,3], TRUE);
          {check types for required parameters}
          _ValidType(0, VT_BSTR, TRUE);
          _ValidType(1, VT_BSTR, TRUE);
          {third parameter is not required
          False by default}
          if (dps.cArgs = 3) and _ValidType(2, VT_BOOL, TRUE) then
            B := dps.rgvarg^[pDispIds^[0]].vbool
          else
            B := False;
          {call the method where we'll create a component}
          DoCreateControl(dps.rgvarg^[pDispIds^[0]].bstrVal,
            dps.rgvarg^[pDispIds^[1]].bstrVal, B)
        end
        else
        if FOwner is TCollection then
        begin
          {add component}
          P := TCollection(FOwner).Add;
          {return the IDispatch}
          OleVariant(varResult^) := FScriptControl.GetProvider(P);
        end
        else
        if FOwner is TStrings then
        begin
          {check if parameters defined}
          CheckArgCount(dps.cArgs, [1,2], TRUE);
          {check if there is the string for first parameter}
          _ValidType(0, VT_BSTR, TRUE);
          if dps.cArgs = 2 then
            {position in string list}
            I := _IntValue(1)
          else
            {if not defined, add to end of list}
            I := TStrings(FOwner).Count;
          TStrings(FOwner).Insert(I, dps.rgvarg^[pDispIds^[0]].bstrVal);
        end;
      end;
   DISPID_HASPROPERTY: begin
        {check if parameters defined for HasProperty}
        CheckArgCount(dps.cArgs, [1], TRUE);
        {check the parameter type}
        _ValidType(0, VT_BSTR, TRUE);
        S := dps.rgvarg^[pDispIds^[0]].bstrVal;
        {return True if property exists}
        OleVariant(varResult^) := Assigned(GetPropInfo(FOwner.ClassInfo, S));
      end;
  else
    {this is not our function but this is property
    Check the Flags (to get or to set a property value)}
    Put := (Flags and DISPATCH_PROPERTYPUT) <> 0;
    if Put then
    begin
      {set property
      Check the parameter and apply a value}
      CheckArgCount(dps.cArgs, [1], True);
      Result := SetVCLProperty(PPropInfo(DispId), dps.rgvarg^[pDispIds^[0]])
    end
    else
    begin
      {get property}
      if DispId = 0 then
      begin
        {default property - return the IDispatch}
        OleVariant(VarResult^) := Self as IDispatch;
        Result := S_OK;
      end
      else
      begin
        {return the current value}
        Result := GetVCLProperty(PPropInfo(DispId), dps, pDispIds, OutValue);
        if Result = S_OK then
          OleVariant(VarResult^) := OutValue;
      end
    end;
  end;
end;

function TVCLProvider.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
  Pointer(TypeInfo) := NIL;
  Result := E_NOTIMPL;
end;

function TVCLProvider.GetTypeInfoCount(out Count: Integer): HResult;
begin
  Count := 0;
  Result := E_NOTIMPL;
end;

procedure TVCLProvider.DoCreateControl(AName, AClassName: WideString; WithEvents: Boolean);

  procedure SetHandler(Control: TPersistent; Owner:TObject; Name: String);
  // To set the handler for method of form with Name property as
  // Name + 'Handler'
  var
    Method: TMethod;
    PropInfo: PPropInfo;
  begin
    // to get RTTI information
    PropInfo := GetPropInfo(Control.ClassInfo, Name);
    if Assigned(PropInfo) then
    begin
      {get the address for handler}
      Method.Code := FScriptControl.MethodAddress(Name + 'Handler');
      if Assigned(Method.Code) then
      begin
        {event exists}
        Method.Data := FScriptControl;
        {to set a handler}
        SetMethodProp(Control, PropInfo, Method);
      end;
    end;
  end;

var
  ThisClass: TControlClass;
  C: TComponent;
  NewOwner: TCustomForm;
begin
  {change the Owner as form}
  if not (FOwner is TCustomForm) then
    NewOwner := GetParentForm(FOwner as TControl)
  else
    NewOwner := FOwner as TCustomForm;
  {get a class for new component}
  ThisClass := TControlClass(GetClass(AClassName));
  {create component}
  C := ThisClass.Create(NewOwner);
  {set the Name and Parent property}
  C.Name := AName;
  if C is TControl then
    TControl(C).Parent := FOwner as TWinControl;
  if WithEvents then
  begin
    {set our custom events}
    SetHandler(C, NewOwner, 'OnClick');
    SetHandler(C, NewOwner, 'OnChange');
    SetHandler(C, NewOwner, 'OnEnter');
    SetHandler(C, NewOwner, 'OnExit');
    SetHandler(C, NewOwner, 'OnTimer');
  end;
  // create the class that realize the IDispatch and add this as object for TScriptControl
  FScriptControl.RegisterClass(AName, C);
end;

function TVCLProvider.SetVCLProperty(PropInfo: PPropInfo;
  Argument: TVariantArg): HResult;
var
  I, J, K, CommaPos: Integer;
  GoodToken: Boolean;
  S, S1: string;
  DT: TDateTime;
  ST: TSystemTime;
  IP: IQueryPersistent;
  Data, TypeData: PTypeData;
  TypeInfo: PTypeInfo;
begin
  Result := S_OK;
  case PropInfo^.PropType^.Kind of
    tkChar, tkString, tkLString, tkWChar, tkWString: // Символьная строка
      begin
        {check the parameter type and set a value}
        ValidType(Argument, VT_BSTR, True);
        SetStrProp(FOwner, PropInfo, Argument.bstrVal);
      end;
    tkInteger:
      begin
        // Проверяем тип свойства на TCursor, TColor
        // если он совпадает и передано символьное значение
        // пытаемся получить его идентификатор
        if (UpperCase(PropInfo^.PropType^.Name) = 'TCURSOR') and
          (Argument.vt = VT_BSTR) then
        begin
          if not IdentToCursor(Argument.bstrVal, I) then
          begin
            Result := DISP_E_BADVARTYPE;
            Exit;
          end;
        end
        else
        if (UpperCase(PropInfo^.PropType^.Name) = 'TCOLOR') and
          (Argument.vt = VT_BSTR) then
        begin
          if not IdentToColor(Argument.bstrVal, I) then
          begin
            Result := DISP_E_BADVARTYPE;
            Exit;
          end;
        end
        else
          I := IntValue(Argument);
        SetOrdProp(FOwner, PropInfo, I);
      end;
    tkEnumeration:
      begin
        // Проверяем на тип Boolean - для него в VBScript есть
        // отдельный тип данных
        if CompareText(PropInfo^.PropType^.Name, 'BOOLEAN') = 0 then
        begin
          // Проверяем тип данных аргумента
          ValidType(Argument, VT_BOOL, TRUE);
          // Это свойство Boolean - получаем значение и устанавливаем его
          SetOrdProp(FOwner, PropInfo, Integer(Argument.vBool));
        end
        else
        begin
          // Иначе - перечислимый тип передается в виде символьной строки
          // Проверяем тип данных аргумента
          ValidType(Argument, VT_BSTR, TRUE);
          // Получаем значение
          S := Trim(Argument.bstrVal);
          // Переводим в Integer
          I := GetEnumValue(PropInfo^.PropType^, S);
          // Если успешно - устанавливаем свойство
          if I >= 0 then
            SetOrdProp(FOwner, PropInfo, I)
          else
            raise EInvalidParamType.Create('');
        end;
      end;
    tkClass:
      begin
        // check the type - must be IDispatch interfcae}
        ValidType(Argument, VT_DISPATCH, TRUE);
        if Assigned(Argument.dispVal) then
        begin
          // Передано непустое значение
          // Получаем интерфейс IQueryPersistent
          IP := IDispatch(Argument.dispVal) as IQueryPersistent;
          // Получаем ссылку на класс, представителем которого
          // является интерфейс
          I := Integer(IP.GetPersistent);
        end
        else
          // Иначе - очищаем свойство
          I := 0;
        // Устанавливаем значение
        SetOrdProp(FOwner, PropInfo, I);
      end;
   tkFloat:
      // Число с плавающей точкой
      begin
        if (PropInfo^.PropType^ = System.TypeInfo(TDateTime)) or
           (PropInfo^.PropType^ = System.TypeInfo(TDate)) then
        begin
          // Проверяем тип данных аргумента
          if Argument.vt = VT_BSTR then
          begin
            DT := StrToDate(Argument.bstrVal);
          end
          else
          begin
            ValidType(Argument, VT_DATE, TRUE);
            if VariantTimeToSystemTime(Argument.date, ST) <> 0 then
              DT := SystemTimeToDateTime(ST)
            else
            begin
              Result := DISP_E_BADVARTYPE;
              Exit;
            end;
          end;
          SetFloatProp(FOwner, PropInfo, DT);
        end
        else
        begin
          // Проверяем тип данных аргумента
          ValidType(Argument, VT_R8, TRUE);
          // Устанавливаем значение}
          SetFloatProp(FOwner, PropInfo, Argument.dblVal);
        end;
      end;
   tkSet:
      // Набор
      begin
        // Проверяем тип данных, должна быть символьная строка
        ValidType(Argument, VT_BSTR, TRUE);
        // Получаем данные
        S := Trim(Argument.bstrVal);
        // Получаем информацию RTTI
        Data := GetTypeData(PropInfo^.PropType^);
        TypeInfo := Data^.CompType^;
        TypeData := GetTypeData(TypeInfo);
        I := 0;
        while Length(S) > 0 do
        begin
          // Проходим по строке, выбирая разделенные запятыми
          // значения идентификаторов
          CommaPos := Pos(',', S);
          if CommaPos = 0 then
            CommaPos := Length(S) + 1;
          S1 := Trim(System.Copy(S, 1, CommaPos - 1));
          System.Delete(S, 1, CommaPos);
          if Length(S1) > 0 then
          begin
            // Поверяем, какому из допустимых значений соответствует
            // полученный идентификатор
            K := 1;
            GoodToken := FALSE;
            for J := TypeData^.MinValue to TypeData^.MaxValue do
            begin
              if CompareText(S1, GetEnumName(TypeInfo , J)) = 0 then
              begin
                // identificator is found, add to mask
                I := I or K;
                GoodToken := TRUE;
              end;
              K := K shl 1;
            end;
            if not GoodToken then
            begin
              // identificator is not found
              Result := DISP_E_BADVARTYPE;
              Exit;
            end;
          end;
        end;
        // to set a value
        SetOrdProp(FOwner, PropInfo, I);
      end;
    tkVariant:
      begin
        // check the data type
        ValidType(Argument, VT_VARIANT, TRUE);
        // to set a value
        SetVariantProp(FOwner, PropInfo, Argument.pvarVal^);
      end;
   else
     // another data types are not supported by OLE
     Result := DISP_E_MEMBERNOTFOUND;
  end;
end;

function TVCLProvider.GetVCLProperty(PropInfo: PPropInfo; dps: TDispParams;
  PDispIds: PDispIdList; var Value: OleVariant): HResult;
var
  I, J, K: Integer;
  S: String;
  P, P1: TPersistent;
  Data: PTypeData;
  DT: TDateTime;
  TypeInfo: PTypeInfo;
begin
  Result := S_OK;
  case PropInfo^.PropType^.Kind of
    tkString, tkLString, tkWChar, tkWString:
      // Символьная строка
      Value := GetStrProp(FOwner, PropInfo);
    tkChar, tkInteger:
      // Целое число
      Value := GetOrdProp(FOwner, PropInfo);
    tkEnumeration:
      // Перечислимый тип
      begin
        // Проверяем, не Boolean ли это
        if CompareText(PropInfo^.PropType^.Name, 'BOOLEAN') = 0 then
        begin
          // Передаем как Boolean
          Value := Boolean(GetOrdProp(FOwner, PropInfo));
        end
        else
        begin
          // Остальные - передаем как строку
          I := GetOrdProp(FOwner, PropInfo);
          Value := GetEnumName(PropInfo^.PropType^, I);
        end;
      end;
    tkClass:
      // Класс
      begin
        // Получаем значение свойства
        P := TPersistent(GetOrdProp(FOwner, PropInfo));
        if Assigned(P) and (P is TCollection) and (dps.cArgs = 1) then
        begin
          // Запрошен элемент коллекции с индексом (есть параметр)
          if ValidType(dps.rgvarg^[pDispIds^[0]], VT_BSTR, FALSE) then
          begin
            // Параметр строковый, ищем элемент по свойству
            // DisplayName
            S := dps.rgvarg^[pDispIds^[0]].bstrVal;
            P1 := nil;
            for I := 0 to Pred(TCollection(P).Count) do
              if CompareText(S, TCollection(P).Items[I].DisplayName) = 0 then
              begin
                P1 := TCollection(P).Items[I];
                Break;
              end;
            if Assigned(P1) then
              // Found - return the IDispatch interface
              Value := FScriptControl.GetProvider(P1)
               //IDispatch(TVCLProvider.Create(P1, FScriptControl))
            else
              // not found
              Result := DISP_E_MEMBERNOTFOUND;
          end
          else
          begin
            // an integer parameter, return the element by index
            I := IntValue(dps.rgvarg^[pDispIds^[0]]);
            if (I >= 0) and (I < TCollection(P).Count) then
            begin
              P := TCollection(P).Items[I];
              Value := FScriptControl.GetProvider(P);
                //IDispatch(TVCLProvider.Create(P, FScriptControl));
            end
            else
              Result := DISP_E_MEMBERNOTFOUND;
          end;
        end
        else
        if Assigned(P) and (P is TStrings) and (dps.cArgs = 1) then
        begin
          // Запрошен элемент из Strings с индексом (есть параметр)
          if ValidType(dps.rgvarg^[pDispIds^[0]], VT_BSTR, FALSE) then
          begin
            // Параметр строковый - возвращаем значение свойства Values
            S := dps.rgvarg^[pDispIds^[0]].bstrVal;
            Value := TStrings(P).Values[S];
          end
          else
          begin
            // Параметр целый, возвращаем строку по индексу
            I := IntValue(dps.rgvarg^[pDispIds^[0]]);
            if (I >= 0) and (I < TStrings(P).Count) then
              Value := TStrings(P)[I]
            else
              Result := DISP_E_MEMBERNOTFOUND;
          end;
        end
        else
          // Общий случай, возвращаем интерфейс IDispatch свойства
          if Assigned(P) then
            Value := FScriptControl.GetProvider(P)
              //IDispatch(TVCLProvider.Create(P, FScriptControl))
          else
            // Или Unassigned, если P = NIL
            Value := Unassigned;
      end;
    tkFloat:
      begin
        if (PropInfo^.PropType^ = System.TypeInfo(TDateTime)) or
           (PropInfo^.PropType^ = System.TypeInfo(TDate)) then
        begin
          DT := GetFloatProp(FOwner, PropInfo);
          Value := DT;
        end
        else
          Value := GetFloatProp(FOwner, PropInfo);
      end;
    tkSet:
      begin
        // Получаем значение свойства (битовая маска)
        I := GetOrdProp(FOwner, PropInfo);
        // Получаем информацию RTTI
        Data := GetTypeData(PropInfo^.PropType^);
        TypeInfo := Data^.CompType^;
        // Формируем строку с набором значений
        S := '';
        if I <> 0 then
        begin
          for K := 0 to 31 do
          begin
            J := 1 shl K;
            if (J and I) = J then
              S := S + GetEnumName(TypeInfo, K) + ',';
          end;
          System.Delete(S, Length(S), 1);
        end;
        Value := S;
      end;
    tkVariant:
      // Вариант
      Value := GetVariantProp(FOwner, PropInfo);
  else
    // Остальные типы не поддерживаются
    Result := DISP_E_MEMBERNOTFOUND;
  end;
end;

function TVCLProvider.GetPersistent: TPersistent;
begin
  // Реализация IQueryPersistent
  // Возвращаем ссылку на класс
  Result := FOwner;
end;

destructor TVCLProvider.Destroy;
begin
  // must delete self from object list TVCLProvider
  FScriptControl.ProviderDestroyed(Self);

  inherited;
end;

{ TVCLEnumerator }

constructor TVCLEnumerator.Create(AOwner: TPersistent; AScriptControl: TSMScriptExecutor);
begin
  inherited Create;

  FOwner := AOwner;
  FScriptControl := AScriptControl;
  FEnumPosition := 0;
end;

type
  CVCLEnumerator = class of TVCLEnumerator;

function TVCLEnumerator.Clone(out Enum: IEnumVariant): HResult;
var
  EnumeratorClass: CVCLEnumerator;
  NewEnum: TVCLEnumerator;
begin
  EnumeratorClass := CVCLENumerator(Self.ClassType);
  NewEnum := EnumeratorClass.Create(FOwner, FScriptControl);
  NewEnum.FEnumPosition := FEnumPosition;
  Enum := NewEnum as IEnumVariant;
  Result := S_OK;
end;

function TVCLEnumerator.Next(celt: LongWord; var rgvar: OleVariant;
  pceltFetched: PLongWord): HResult;
var
  I: Cardinal;
begin
  Result := S_OK;
  I := 0;
  if FOwner is TWinControl then
  begin
    with TWinControl(FOwner) do
    begin
      while (FEnumPosition < ControlCount) and (I < celt) do
      begin
        TVariantList(rgvar)[I] :=
          FScriptControl.GetProvider(Controls[FEnumPosition]);
//          TVCLProvider.Create(Controls[FEnumPosition], FScriptControl) as IDispatch;
        Inc(I);
        Inc(FEnumPosition);
      end;
    end;
  end
  else
  if FOwner is TCollection then
  begin
    with TCollection(FOwner) do
    begin
      while (FEnumPosition < Count) and (I < celt) do
      begin
        TVariantList(rgvar)[I] :=
          FScriptControl.GetProvider(Items[FEnumPosition]);
//          TVCLProvider.Create(Items[FEnumPosition], FScriptControl) as IDispatch;
        Inc(I);
        Inc(FEnumPosition);
      end;
    end;
  end
  else
  if FOwner is TStrings then
  begin
    with TStrings(FOwner) do
    begin
      while (FEnumPosition < Count) and (I < celt) do
      begin
        TVariantList(rgvar)[I] := TStrings(FOwner)[FEnumPosition];
        Inc(I);
        Inc(FEnumPosition);
      end;
    end;
  end
  else
    Result := S_FALSE;
  if I <> celt then
    Result := S_FALSE;
  if Assigned(pceltFetched) then
    pceltFetched^ := I;
end;

function TVCLEnumerator.Reset: HResult;
begin
  FEnumPosition := 0;
  Result := S_OK;
end;

function TVCLEnumerator.Skip(celt: LongWord): HResult;
var
  Total: Integer;
begin
  Result := S_FALSE;
  if FOwner is TWinControl then
    Total := TWinControl(FOwner).ControlCount
  else
  if FOwner is TCollection then
    Total := TCollection(FOwner).Count
  else
  if FOwner is TStrings then
    Total := TStrings(FOwner).Count
  else
    Exit;
  {$WARNINGS OFF}
  if FEnumPosition + celt <= Total then
  begin
  {$WARNINGS ON}
    Result := S_OK;
    Inc(FEnumPosition, celt)
  end;
end;

function TSMScriptExecutor.GetProvider(AOwner: TPersistent): IDispatch;
var
  I: Integer;
  QP: IQueryPersistent;
begin
  Result := nil;
  with FProviderList do
    for I := 0 to Pred(Count) do
    begin
      if TObject(FProviderList[I]).GetInterface(IQueryPersistent, QP) and
         (QP.GetPersistent = AOwner) then
      begin
        Result := TVCLProvider(FProviderList[I]) as IDispatch;
        Exit;
      end;
    end;
  I := FProviderList.Add(TVCLProvider.Create(AOwner, Self));
  Result := TVCLProvider(FProviderList[I]) as IDispatch;
end;

procedure TSMScriptExecutor.ProviderDestroyed(Address: Pointer);
begin
  if Assigned(FProviderList) then
    with FProviderList do
      Delete(IndexOf(Address));
end;

procedure TSMScriptExecutor.RegisterClass(const Name: String;
  AClass: TPersistent);
begin
  AddObject(Name, GetProvider(AClass), False);
end;

procedure TSMScriptExecutor.OnChangeHandler(Sender: TObject);
begin
  Run((Sender as TComponent).Name + '_' + 'OnChange', UnAssigned);
end;

procedure TSMScriptExecutor.OnClickHandler(Sender: TObject);
begin
  Run((Sender as TComponent).Name + '_' + 'OnClick', UnAssigned);
end;

procedure TSMScriptExecutor.OnEnterHandler(Sender: TObject);
begin
  Run((Sender as TComponent).Name + '_' + 'OnEnter', UnAssigned);
end;

procedure TSMScriptExecutor.OnExitHandler(Sender: TObject);
begin
  Run((Sender as TComponent).Name + '_' + 'OnExit', UnAssigned);
end;

procedure TSMScriptExecutor.OnTimerHandler(Sender: TObject);
begin
  Run((Sender as TComponent).Name + '_' + 'OnTimer', UnAssigned);
end;

end.
