{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         Scripting: MS Script control                  }
{                                                       }
{         Source is auto-generated                      }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit MSScript;

{ This file contains pascal declarations imported from a type library.
  This file will be written during each import or refresh of the type
  library editor.  Changes to this file will be discarded during the
  refresh process. }

{ Microsoft Script Control 1.0 }
{ Version 1.0 }

{ Conversion log:
  Warning: 'Procedure' is a reserved word. Procedure changed to Procedure_
  Warning: ScriptControlConstants: Modules are not supported.
  Warning: 'Object' is a reserved word. Parameter 'Object' in IScriptModuleCollection.Add changed to 'Object_'
  Warning: 'Object' is a reserved word. Parameter 'Object' in IScriptControl.AddObject changed to 'Object_'
  Warning: IScriptModule.Run parameter Parameters of type PSafeArray was written as OleVariant.
  Warning: IScriptControl.Run parameter Parameters of type PSafeArray was written as OleVariant.
 }

interface

uses Windows, ActiveX, Classes, OleCtrls;

const
  LIBID_MSScriptControl: TGUID = '{0E59F1D2-1FBE-11D0-8FF2-00A0D10038BC}';

const

{ States into which the scripting engine can be placed }

{ ScriptControlStates }

  Initialized = 0;
  Connected = 1;

const

{ Component class GUIDs }
  Class_Procedure_: TGUID = '{0E59F1DA-1FBE-11D0-8FF2-00A0D10038BC}';
  Class_Procedures: TGUID = '{0E59F1DB-1FBE-11D0-8FF2-00A0D10038BC}';
  Class_Module: TGUID = '{0E59F1DC-1FBE-11D0-8FF2-00A0D10038BC}';
  Class_Modules: TGUID = '{0E59F1DD-1FBE-11D0-8FF2-00A0D10038BC}';
  Class_Error: TGUID = '{0E59F1DE-1FBE-11D0-8FF2-00A0D10038BC}';
  Class_ScriptControl: TGUID = '{0E59F1D5-1FBE-11D0-8FF2-00A0D10038BC}';

type

{ Forward declarations: Interfaces }
  IScriptProcedure = interface;
  IScriptProcedureDisp = dispinterface;
  IScriptProcedureCollection = interface;
  IScriptProcedureCollectionDisp = dispinterface;
  IScriptModule = interface;
  IScriptModuleDisp = dispinterface;
  IScriptModuleCollection = interface;
  IScriptModuleCollectionDisp = dispinterface;
  IScriptError = interface;
  IScriptErrorDisp = dispinterface;
  IScriptControl = interface;
  IScriptControlDisp = dispinterface;
  DScriptControlSource = dispinterface;

{ Forward declarations: CoClasses }
  ScriptControl = IScriptControl;

{ Forward declarations: Enums }
  ScriptControlStates = TOleEnum;

{ Describes a procedure }

  IScriptProcedure = interface(IDispatch)
    ['{70841C73-067D-11D0-95D8-00A02463AB28}']
    function Get_Name: WideString; safecall;
    function Get_NumArgs: Integer; safecall;
    function Get_HasReturnValue: WordBool; safecall;
    property Name: WideString read Get_Name;
    property NumArgs: Integer read Get_NumArgs;
    property HasReturnValue: WordBool read Get_HasReturnValue;
  end;

{ DispInterface declaration for Dual Interface IScriptProcedure }

  IScriptProcedureDisp = dispinterface
    ['{70841C73-067D-11D0-95D8-00A02463AB28}']
    property Name: WideString readonly dispid 0;
    property NumArgs: Integer readonly dispid 100;
    property HasReturnValue: WordBool readonly dispid 101;
  end;

{ Collection of procedures }

  IScriptProcedureCollection = interface(IDispatch)
    ['{70841C71-067D-11D0-95D8-00A02463AB28}']
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(Index: OleVariant): IScriptProcedure; safecall;
    function Get_Count: Integer; safecall;
    property NewEnum: IUnknown read Get__NewEnum;
    property Item[Index: OleVariant]: IScriptProcedure read Get_Item; default;
    property Count: Integer read Get_Count;
  end;

{ DispInterface declaration for Dual Interface IScriptProcedureCollection }

  IScriptProcedureCollectionDisp = dispinterface
    ['{70841C71-067D-11D0-95D8-00A02463AB28}']
    property NewEnum: IUnknown readonly dispid -4;
    property Item[Index: OleVariant]: IScriptProcedure readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
  end;

{ Context in which functions can be defined and expressions can be evaluated }

  IScriptModule = interface(IDispatch)
    ['{70841C70-067D-11D0-95D8-00A02463AB28}']
    function Get_Name: WideString; safecall;
    function Get_CodeObject: IDispatch; safecall;
    function Get_Procedures: IScriptProcedureCollection; safecall;
    procedure AddCode(const Code: WideString); safecall;
    function Eval(const Expression: WideString): OleVariant; safecall;
    procedure ExecuteStatement(const Statement: WideString); safecall;
    function Run(const ProcedureName: WideString; var Parameters: PSafeArray): OleVariant; safecall;
    property Name: WideString read Get_Name;
    property CodeObject: IDispatch read Get_CodeObject;
    property Procedures: IScriptProcedureCollection read Get_Procedures;
  end;

{ DispInterface declaration for Dual Interface IScriptModule }

  IScriptModuleDisp = dispinterface
    ['{70841C70-067D-11D0-95D8-00A02463AB28}']
    property Name: WideString readonly dispid 0;
    property CodeObject: IDispatch readonly dispid 1000;
    property Procedures: IScriptProcedureCollection readonly dispid 1001;
    procedure AddCode(const Code: WideString); dispid 2000;
    function Eval(const Expression: WideString): OleVariant; dispid 2001;
    procedure ExecuteStatement(const Statement: WideString); dispid 2002;
    function Run(const ProcedureName: WideString; var Parameters: OleVariant): OleVariant; dispid 2003;
  end;

{ Collection of modules }

  IScriptModuleCollection = interface(IDispatch)
    ['{70841C6F-067D-11D0-95D8-00A02463AB28}']
    function Get__NewEnum: IUnknown; safecall;
    function Get_Item(Index: OleVariant): IScriptModule; safecall;
    function Get_Count: Integer; safecall;
    function Add(const Name: WideString; var Object_: OleVariant): IScriptModule; safecall;
    property NewEnum: IUnknown read Get__NewEnum;
    property Item[Index: OleVariant]: IScriptModule read Get_Item; default;
    property Count: Integer read Get_Count;
  end;

{ DispInterface declaration for Dual Interface IScriptModuleCollection }

  IScriptModuleCollectionDisp = dispinterface
    ['{70841C6F-067D-11D0-95D8-00A02463AB28}']
    property NewEnum: IUnknown readonly dispid -4;
    property Item[Index: OleVariant]: IScriptModule readonly dispid 0; default;
    property Count: Integer readonly dispid 1;
    function Add(const Name: WideString; var Object_: OleVariant): IScriptModule; dispid 2;
  end;

{ Provides access to scripting error information }

  IScriptError = interface(IDispatch)
    ['{70841C78-067D-11D0-95D8-00A02463AB28}']
    function Get_Number: Integer; safecall;
    function Get_Source: WideString; safecall;
    function Get_Description: WideString; safecall;
    function Get_HelpFile: WideString; safecall;
    function Get_HelpContext: Integer; safecall;
    function Get_Text: WideString; safecall;
    function Get_Line: Integer; safecall;
    function Get_Column: Integer; safecall;
    procedure Clear; safecall;
    property Number: Integer read Get_Number;
    property Source: WideString read Get_Source;
    property Description: WideString read Get_Description;
    property HelpFile: WideString read Get_HelpFile;
    property HelpContext: Integer read Get_HelpContext;
    property Text: WideString read Get_Text;
    property Line: Integer read Get_Line;
    property Column: Integer read Get_Column;
  end;

{ DispInterface declaration for Dual Interface IScriptError }

  IScriptErrorDisp = dispinterface
    ['{70841C78-067D-11D0-95D8-00A02463AB28}']
    property Number: Integer readonly dispid 201;
    property Source: WideString readonly dispid 202;
    property Description: WideString readonly dispid 203;
    property HelpFile: WideString readonly dispid 204;
    property HelpContext: Integer readonly dispid 205;
    property Text: WideString readonly dispid -517;
    property Line: Integer readonly dispid 206;
    property Column: Integer readonly dispid 207;
    procedure Clear; dispid 208;
  end;

{ Control to host scripting engines that understand the ActiveX Scripting interface }

  IScriptControl = interface(IDispatch)
    ['{0E59F1D3-1FBE-11D0-8FF2-00A0D10038BC}']
    function Get_Language: WideString; safecall;
    procedure Set_Language(const Value: WideString); safecall;
    function Get_State: ScriptControlStates; safecall;
    procedure Set_State(Value: ScriptControlStates); safecall;
    procedure Set_SitehWnd(Value: Integer); safecall;
    function Get_SitehWnd: Integer; safecall;
    function Get_Timeout: Integer; safecall;
    procedure Set_Timeout(Value: Integer); safecall;
    function Get_AllowUI: WordBool; safecall;
    procedure Set_AllowUI(Value: WordBool); safecall;
    function Get_UseSafeSubset: WordBool; safecall;
    procedure Set_UseSafeSubset(Value: WordBool); safecall;
    function Get_Modules: IScriptModuleCollection; safecall;
    function Get_Error: IScriptError; safecall;
    function Get_CodeObject: IDispatch; safecall;
    function Get_Procedures: IScriptProcedureCollection; safecall;
    procedure AboutBox; safecall;
    procedure AddObject(const Name: WideString; Object_: IDispatch; AddMembers: WordBool); safecall;
    procedure Reset; safecall;
    procedure AddCode(const Code: WideString); safecall;
    function Eval(const Expression: WideString): OleVariant; safecall;
    procedure ExecuteStatement(const Statement: WideString); safecall;
    function Run(const ProcedureName: WideString; var Parameters: PSafeArray): OleVariant; safecall;
    property Language: WideString read Get_Language write Set_Language;
    property State: ScriptControlStates read Get_State write Set_State;
    property SitehWnd: Integer read Get_SitehWnd write Set_SitehWnd;
    property Timeout: Integer read Get_Timeout write Set_Timeout;
    property AllowUI: WordBool read Get_AllowUI write Set_AllowUI;
    property UseSafeSubset: WordBool read Get_UseSafeSubset write Set_UseSafeSubset;
    property Modules: IScriptModuleCollection read Get_Modules;
    property Error: IScriptError read Get_Error;
    property CodeObject: IDispatch read Get_CodeObject;
    property Procedures: IScriptProcedureCollection read Get_Procedures;
  end;

{ DispInterface declaration for Dual Interface IScriptControl }

  IScriptControlDisp = dispinterface
    ['{0E59F1D3-1FBE-11D0-8FF2-00A0D10038BC}']
    property Language: WideString dispid 1500;
    property State: ScriptControlStates dispid 1501;
    property SitehWnd: Integer dispid 1502;
    property Timeout: Integer dispid 1503;
    property AllowUI: WordBool dispid 1504;
    property UseSafeSubset: WordBool dispid 1505;
    property Modules: IScriptModuleCollection readonly dispid 1506;
    property Error: IScriptError readonly dispid 1507;
    property CodeObject: IDispatch readonly dispid 1000;
    property Procedures: IScriptProcedureCollection readonly dispid 1001;
    procedure AboutBox; dispid -552;
    procedure AddObject(const Name: WideString; Object_: IDispatch; AddMembers: WordBool); dispid 2500;
    procedure Reset; dispid 2501;
    procedure AddCode(const Code: WideString); dispid 2000;
    function Eval(const Expression: WideString): OleVariant; dispid 2001;
    procedure ExecuteStatement(const Statement: WideString); dispid 2002;
    function Run(const ProcedureName: WideString; var Parameters: OleVariant): OleVariant; dispid 2003;
  end;

  DScriptControlSource = dispinterface
    ['{8B167D60-8605-11D0-ABCB-00A0C90FFFC0}']
    procedure Error; dispid 3000;
    procedure Timeout; dispid 3001;
  end;

{ Control to host scripting engines that understand the ActiveX Scripting interface }

  TScriptControl = class(TOleControl)
  private
    FOnError: TNotifyEvent;
    FOnTimeout: TNotifyEvent;
    FIntf: IScriptControl;
    function GetControlInterface: IScriptControl;
    function Get_Modules: IScriptModuleCollection;
    function Get_Error: IScriptError;
    function Get_Procedures: IScriptProcedureCollection;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function GetTOleEnumProp(Index: Integer): TOleEnum;
    procedure SetTOleEnumProp(Index: Integer; Value: TOleEnum);
  public
    procedure AboutBox;
    procedure AddObject(const Name: WideString; AObject: IDispatch; AddMembers: WordBool);
    procedure Reset;
    procedure AddCode(const Code: WideString);
    function Eval(const Expression: WideString): OleVariant;
    procedure ExecuteStatement(const Statement: WideString);
    function Run(const ProcedureName: WideString; var Parameters: PSafeArray): OleVariant;
    property ControlInterface: IScriptControl read GetControlInterface;
    property Modules: IScriptModuleCollection read Get_Modules;
    property ErrorObject: IScriptError read Get_Error;
    property CodeObject: IDispatch index 1000 read GetIDispatchProp;
    property Procedures: IScriptProcedureCollection read Get_Procedures;
  published
    property Language: WideString index 1500 read GetWideStringProp write SetWideStringProp stored False;
    property State: ScriptControlStates index 1501 read GetTOleEnumProp write SetTOleEnumProp stored False;
    property SitehWnd: Integer index 1502 read GetIntegerProp write SetIntegerProp stored False;
    property Timeout: Integer index 1503 read GetIntegerProp write SetIntegerProp stored False;
    property AllowUI: WordBool index 1504 read GetWordBoolProp write SetWordBoolProp stored False;
    property UseSafeSubset: WordBool index 1505 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnTimeout: TNotifyEvent read FOnTimeout write FOnTimeout;
  end;

implementation

uses ComObj;

procedure TScriptControl.InitControlData;
const
  CEventDispIDs: array[0..1] of Integer = (
    $00000BB8, $00000BB9);
  CControlData: TControlData = (
    ClassID: '{0E59F1D5-1FBE-11D0-8FF2-00A0D10038BC}';
    EventIID: '{8B167D60-8605-11D0-ABCB-00A0C90FFFC0}';
    EventCount: 2;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil;
    Flags: $00000000;
    Version: 300);
begin
  ControlData := @CControlData;
end;

procedure TScriptControl.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as IScriptControl;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TScriptControl.GetControlInterface: IScriptControl;
begin
  CreateControl;
  Result := FIntf;
end;

function TScriptControl.GetTOleEnumProp(Index: Integer): TOleEnum;
begin
  Result := GetIntegerProp(Index);
end;

procedure TScriptControl.SetTOleEnumProp(Index: Integer; Value: TOleEnum);
begin
  SetIntegerProp(Index, Value);
end;

procedure TScriptControl.AboutBox;
begin
  CreateControl;
  FIntf.AboutBox;
end;

procedure TScriptControl.AddObject(const Name: WideString; AObject: IDispatch; AddMembers: WordBool);
begin
  CreateControl;
  FIntf.AddObject(Name, AObject, AddMembers);
end;

procedure TScriptControl.Reset;
begin
  CreateControl;
  FIntf.Reset;
end;

procedure TScriptControl.AddCode(const Code: WideString);
begin
  CreateControl;
  FIntf.AddCode(Code);
end;

function TScriptControl.Eval(const Expression: WideString): OleVariant;
begin
  CreateControl;
  Result := FIntf.Eval(Expression);
end;

procedure TScriptControl.ExecuteStatement(const Statement: WideString);
begin
  CreateControl;
  FIntf.ExecuteStatement(Statement);
end;

function TScriptControl.Run(const ProcedureName: WideString; var Parameters: PSafeArray): OleVariant;
begin
  CreateControl;
  Result := FIntf.Run(ProcedureName, Parameters);
end;

function TScriptControl.Get_Modules: IScriptModuleCollection;
begin
  CreateControl;
  Result := FIntf.Modules;
end;

function TScriptControl.Get_Error: IScriptError;
begin
  CreateControl;
  Result := FIntf.Error;
end;

function TScriptControl.Get_Procedures: IScriptProcedureCollection;
begin
  CreateControl;
  Result := FIntf.Procedures;
end;

end.
