
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit radreg;

interface

uses Classes, DsgnIntf, Forms, Controls,
     radconlist, radcommon, radp2p, rade2p, rade2m, radvar;

type
  TradClassTypeNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TradComponentProperty = class(TComponentProperty)
  private
    FCheckProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TradPropertyInfoProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetRADClassInfo: TradClassInfo;
    function IsPropertyVisible(Prop: TradPropertyInfo): Boolean;
  end;

  TradPropertyIndexesProperty = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure Edit; override;
    function GetPropertyInfo: TradPropertyInfo;
  end;

  TradEventInfoProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetRADClassInfo: TradClassInfo;
  end;

  TradEventParamProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetEventInfo: TradEventInfo;
  end;

  TradMethodInfoProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TradMethodParamProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;

  TradMethodParamsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  TradConnectionListEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TradVariableEditor = class(TComponentEditor)
  public
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

procedure Register;

implementation

{$R radconnectors.dcr}

uses SysUtils, Consts, TypInfo, Dialogs, radcsdlg, radlinksedit, radpropindex, radabout;

//------------------------------------------------------------------------------
// TradClassTypeNameProperty implementation
//------------------------------------------------------------------------------
function TradClassTypeNameProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paSortList, paRevertable];
end;

//------------------------------------------------------------------------------
procedure TradClassTypeNameProperty.GetValues(Proc: TGetStrProc);
begin
    TradClassRegistry.Iterate(Proc);
end;

//------------------------------------------------------------------------------
// TradComponentProperty implementation
//------------------------------------------------------------------------------
procedure TradComponentProperty.CheckComponent(const Value: string);
var
    Comp: TComponent;
begin
    Comp := Designer.GetComponent(Value);
    if Assigned(TradClassRegistry.FindClass(Comp.ClassName)) then
        FCheckProc(Value);
end;

//------------------------------------------------------------------------------
procedure TradComponentProperty.GetValues(Proc: TGetStrProc);
begin
    FCheckProc := Proc;
    inherited GetValues(CheckComponent);
end;

//------------------------------------------------------------------------------
// TradPropertyInfoProperty implementation
//------------------------------------------------------------------------------
function TradPropertyInfoProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paSortList, paRevertable];
end;

//------------------------------------------------------------------------------
procedure TradPropertyInfoProperty.GetValues(Proc: TGetStrProc);
var
    i: Integer;
    CompInfo: TradClassInfo;
begin
    if Assigned(GetComponent(0)) then begin
        CompInfo := GetRADClassInfo;
        if Assigned(CompInfo) then begin
            for i := 0 to CompInfo.PropertyCount - 1 do begin
                if (CompInfo.Properties[i].Visibility <> pvProtected) and IsPropertyVisible(CompInfo.Properties[i]) then
                    Proc(CompInfo.Properties[i].Name);
            end;
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure TradPropertyInfoProperty.SetValue(const Value: string);
var
    CompInfo: TradClassInfo;
begin
    CompInfo := GetRADClassInfo;
    if (Value = '') or not Assigned(CompInfo) then
        inherited SetValue('')
    else if not Assigned(CompInfo.FindProperty(Value)) then
        raise EPropertyError.Create('Unknown property name!')
    else
        inherited SetValue(Value);
end;

//------------------------------------------------------------------------------
function TradPropertyInfoProperty.GetRADClassInfo: TradClassInfo;
begin
    if GetName = 'SourceProperty' then
        Result := TradConnection(GetComponent(0)).SourceInfo
    else if GetName = 'TargetProperty' then
        Result := TradConnection(GetComponent(0)).TargetInfo
    else if GetName = 'ValueComponentProperty' then
        Result := TradE2PConnection(GetComponent(0)).ValueComponentInfo
    else if GetName = 'ComponentProperty' then
        Result := TradParameterConnection(GetComponent(0)).ComponentInfo
    else
        raise EPropertyError.Create('Unknown property!');
end;

//------------------------------------------------------------------------------
function TradPropertyInfoProperty.IsPropertyVisible(Prop: TradPropertyInfo): Boolean;
begin
    if GetName = 'SourceProperty' then
        Result := psReadable in Prop.Style
    else if GetName = 'TargetProperty' then
        Result := psWritable in Prop.Style
    else if GetName = 'ValueComponentProperty' then
        Result := psReadable in Prop.Style
    else if GetName = 'ComponentProperty' then
        Result := psReadable in Prop.Style
    else
        raise EPropertyError.Create('Unknown property!');
end;

//------------------------------------------------------------------------------
// TradPropertyIndexesProperty implementation
//------------------------------------------------------------------------------
function TradPropertyIndexesProperty.GetAttributes: TPropertyAttributes;
var
    PropInfo: TradPropertyInfo;
begin
    PropInfo := GetPropertyInfo;
    Result := [paReadOnly];
    if Assigned(PropInfo) and (PropInfo.IndexCount > 0) then
        Include(Result, paDialog);
end;

//------------------------------------------------------------------------------
function TradPropertyIndexesProperty.GetValue: string;
var
    i: Integer;
    PropInfo: TradPropertyInfo;
    Value: String;
begin
    Value := 'None';
    if Assigned(GetComponent(0)) then begin
        PropInfo := GetPropertyInfo;
        if Assigned(PropInfo) and (PropInfo.IndexCount > 0) then begin
            Value := '[';
            for i := 0 to PropInfo.IndexCount - 1 do begin
                Value := Value + PropInfo.Indexes[i].Name;
                if i < PropInfo.IndexCount - 1 then
                    Value := Value + ', ';
            end;
            Value := Value + ']';
        end;
    end;
    Result := Value;
end;

//------------------------------------------------------------------------------
procedure TradPropertyIndexesProperty.Edit;
var
    PropInfo: TradPropertyInfo;
    Indexes: Variant;
    i: Integer;
begin
    PropInfo := GetPropertyInfo;
    if Assigned(PropInfo) and (PropInfo.IndexCount > 0) then begin
        radShowPropertyIndexEditor(Designer, GetComponent(0), GetName, PropInfo);
        Indexes := GetVarValue;
        for i := 0 to PropInfo.IndexCount - 1 do
            Indexes[i] := PropInfo.Indexes[i].Value;
        SetVarValue(Indexes);
        Modified;
    end;
end;

//------------------------------------------------------------------------------
function TradPropertyIndexesProperty.GetPropertyInfo: TradPropertyInfo;
begin
    if GetName = 'SourcePropertyIndex' then
        Result := TradP2PConnection(GetComponent(0)).SourcePropertyInfo
    else if GetName = 'TargetPropertyIndex' then begin
        if GetComponent(0) is TradP2PConnection then
            Result := TradP2PConnection(GetComponent(0)).TargetPropertyInfo
        else if GetComponent(0) is TradE2PConnection then
            Result := TradE2PConnection(GetComponent(0)).TargetPropertyInfo
        else
            raise EPropertyError.Create('Unknown property!');
    end else if GetName = 'ValueComponentPropertyIndex' then
        Result := TradE2PConnection(GetComponent(0)).ValueComponentPropertyInfo
    else if GetName = 'ComponentPropertyIndex' then
        Result := TradParameterConnection(GetComponent(0)).ComponentPropertyInfo
    else
        raise EPropertyError.Create('Unknown property!');
end;

//------------------------------------------------------------------------------
// TradEventInfoProperty implementation
//------------------------------------------------------------------------------
function TradEventInfoProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paSortList, paRevertable];
end;

//------------------------------------------------------------------------------
procedure TradEventInfoProperty.GetValues(Proc: TGetStrProc);
var
    i: Integer;
    CompInfo: TradClassInfo;
begin
    if Assigned(GetComponent(0)) then begin
        CompInfo := GetRADClassInfo;
        if Assigned(CompInfo) then begin
            for i := 0 to CompInfo.EventCount - 1 do begin
                if (CompInfo.Events[i].Visibility <> pvProtected) then
                    Proc(CompInfo.Events[i].Name);
            end;
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure TradEventInfoProperty.SetValue(const Value: string);
var
    CompInfo: TradClassInfo;
begin
    CompInfo := GetRADClassInfo;
    if (Value = '') or not Assigned(CompInfo) then
        inherited SetValue('')
    else if not Assigned(CompInfo.FindEvent(Value)) then
        raise EPropertyError.Create('Unknown property name!')
    else
        inherited SetValue(Value)
end;

//------------------------------------------------------------------------------
function TradEventInfoProperty.GetRADClassInfo: TradClassInfo;
begin
    if GetName = 'SourceEvent' then
        Result := TradConnection(GetComponent(0)).SourceInfo
    else if GetName = 'TargetEvent' then
        Result := TradConnection(GetComponent(0)).TargetInfo
    else
        raise EPropertyError.Create('Unknown property!');
end;

//------------------------------------------------------------------------------
// TradEventParamProperty implementation
//------------------------------------------------------------------------------
function TradEventParamProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paRevertable];
end;

//------------------------------------------------------------------------------
procedure TradEventParamProperty.GetValues(Proc: TGetStrProc);
var
    i: Integer;
    EventInfo: TradEventInfo;
begin
    if Assigned(GetComponent(0)) then begin
        EventInfo := GetEventInfo;
        if Assigned(EventInfo) then begin
            for i := 0 to EventInfo.ArgCount - 1 do
                Proc(EventInfo.Args[i].Name);
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure TradEventParamProperty.SetValue(const Value: string);
var
    EventInfo: TradEventInfo;
begin
    EventInfo := GetEventInfo;
    if (Value = '') or not Assigned(EventInfo) then
        inherited SetValue('')
    else if not Assigned(EventInfo.Find(Value)) then
        raise EPropertyError.Create('Unknown property name!')
    else
        inherited SetValue(Value)
end;

//------------------------------------------------------------------------------
function TradEventParamProperty.GetEventInfo: TradEventInfo;
begin
    if GetComponent(0) is TradE2PConnection then
        Result := TradE2PConnection(GetComponent(0)).SourceEventInfo
    else if GetComponent(0) is TradParameterConnection then
        Result := TradParameterConnection(GetComponent(0)).Connection.SourceEventInfo
    else
        raise EPropertyError.Create('Unknown property!');
end;

//------------------------------------------------------------------------------
// TradMethodInfoProperty implementation
//------------------------------------------------------------------------------
function TradMethodInfoProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paValueList, paSortList, paRevertable];
end;

//------------------------------------------------------------------------------
procedure TradMethodInfoProperty.GetValues(Proc: TGetStrProc);
var
    i: Integer;
    CompInfo: TradClassInfo;
begin
    if Assigned(GetComponent(0)) then begin
        if GetName = 'TargetMethod' then
            CompInfo := TradConnection(GetComponent(0)).TargetInfo
        else
            raise EPropertyError.Create('Unknown property!');

        if Assigned(CompInfo) then begin
            for i := 0 to CompInfo.MethodCount - 1 do
                Proc(CompInfo.Methods[i].Name);
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure TradMethodInfoProperty.SetValue(const Value: string);
var
    CompInfo: TradClassInfo;
begin
    if GetName = 'TargetMethod' then
        CompInfo := TradConnection(GetComponent(0)).TargetInfo
    else
        raise EPropertyError.Create('Unknown property!');

    if (Value = '') or not Assigned(CompInfo) then
        inherited SetValue('')
    else if not Assigned(CompInfo.FindMethod(Value)) then
        raise EPropertyError.Create('Unknown property name!')
    else
        inherited SetValue(Value)
end;

//------------------------------------------------------------------------------
// TradMethodParamProperty implementation
//------------------------------------------------------------------------------
function TradMethodParamProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paReadOnly];
end;

//------------------------------------------------------------------------------
// TradMethodParamsProperty implementation
//------------------------------------------------------------------------------
function TradMethodParamsProperty.GetAttributes: TPropertyAttributes;
begin
    Result := [paDialog, paReadOnly];
end;

//------------------------------------------------------------------------------
procedure TradMethodParamsProperty.Edit;
begin
    radShowMethodParameterEditor(Designer, TradParameterConnections(GetOrdValue()).Connection);
end;

//------------------------------------------------------------------------------
// TradConnectionListEditor implementation
//------------------------------------------------------------------------------
function TradConnectionListEditor.GetVerb(Index: Integer): string;
begin
    case Index of
        0: Result := 'TrueRAD Suite 1.0';
        1: Result := 'Copyright (C) 2000 TrueRAD Soft';
        2: Result := '-';
        3: Result := 'About...';
        4: Result := 'Edit connections...';
    end;
end;

//------------------------------------------------------------------------------
function TradConnectionListEditor.GetVerbCount: Integer;
begin
    Result := 5;
end;

//------------------------------------------------------------------------------
procedure TradConnectionListEditor.Edit;
begin
    radShowConnectionsEditor(Designer, Component);
end;

//------------------------------------------------------------------------------
procedure TradConnectionListEditor.ExecuteVerb(Index: Integer);
var
    AboutFrm: TradAboutForm;
begin
    case Index of
        3: begin
            AboutFrm := TradAboutForm.Create(Application);
            try
                AboutFrm.ShowModal;
            finally
                AboutFrm.Free;
            end;
        end;
        4: Edit;
    end;
end;

//------------------------------------------------------------------------------
// TradVariableEditor implementation
//------------------------------------------------------------------------------
function TradVariableEditor.GetVerb(Index: Integer): string;
begin
    case Index of
        0: Result := 'TrueRAD Suite 1.0';
        1: Result := 'Copyright (C) 2000 TrueRAD Soft';
        2: Result := '-';
        3: Result := 'About...';
    end;
end;

//------------------------------------------------------------------------------
function TradVariableEditor.GetVerbCount: Integer;
begin
    Result := 4;
end;

//------------------------------------------------------------------------------
procedure TradVariableEditor.ExecuteVerb(Index: Integer);
var
    AboutFrm: TradAboutForm;
begin
    case Index of
        3: begin
            AboutFrm := TradAboutForm.Create(Application);
            try
                AboutFrm.ShowModal;
            finally
                AboutFrm.Free;
            end;
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure Register;
begin
    { Source, Target, etc. objects and components }
    RegisterPropertyEditor(TypeInfo(TComponent), TradConnection, '', TradComponentProperty);

    { TradP2PConnection }
    RegisterPropertyEditor(TypeInfo(String), TradP2PConnection, 'SourceProperty', TradPropertyInfoProperty);
    RegisterPropertyEditor(TypeInfo(Variant), TradP2PConnection, 'SourcePropertyIndex', TradPropertyIndexesProperty);
    RegisterPropertyEditor(TypeInfo(String), TradP2PConnection, 'SourceEvent', TradEventInfoProperty);
    RegisterPropertyEditor(TypeInfo(String), TradP2PConnection, 'TargetProperty', TradPropertyInfoProperty);
    RegisterPropertyEditor(TypeInfo(Variant), TradP2PConnection, 'TargetPropertyIndex', TradPropertyIndexesProperty);
    RegisterPropertyEditor(TypeInfo(String), TradP2PConnection, 'TargetEvent', TradEventInfoProperty);

    { TradE2PConnection }
    RegisterPropertyEditor(TypeInfo(String), TradE2PConnection, 'SourceEvent', TradEventInfoProperty);
    RegisterPropertyEditor(TypeInfo(String), TradE2PConnection, 'TargetProperty', TradPropertyInfoProperty);
    RegisterPropertyEditor(TypeInfo(Variant), TradE2PConnection, 'TargetPropertyIndex', TradPropertyIndexesProperty);
    RegisterPropertyEditor(TypeInfo(String), TradE2PConnection, 'ValueComponentProperty', TradPropertyInfoProperty);
    RegisterPropertyEditor(TypeInfo(Variant), TradE2PConnection, 'ValueComponentPropertyIndex', TradPropertyIndexesProperty);
    RegisterPropertyEditor(TypeInfo(String), TradE2PConnection, 'ValueEventParameter', TradEventParamProperty);

    { TradE2MConnection }
    RegisterPropertyEditor(TypeInfo(String), TradE2MConnection, 'SourceEvent', TradEventInfoProperty);
    RegisterPropertyEditor(TypeInfo(String), TradE2MConnection, 'TargetMethod', TradMethodInfoProperty);
    RegisterPropertyEditor(TypeInfo(TradParameterConnections), TradE2MConnection, 'TargetMethodParameters', TradMethodParamsProperty);

    { TradParameterConnection }
    RegisterPropertyEditor(TypeInfo(TComponent), TradParameterConnection, 'Component', TradComponentProperty);
    RegisterPropertyEditor(TypeInfo(String), TradParameterConnection, 'ParameterName', TradMethodParamProperty);
    RegisterPropertyEditor(TypeInfo(String), TradParameterConnection, 'ComponentProperty', TradPropertyInfoProperty);
    RegisterPropertyEditor(TypeInfo(Variant), TradParameterConnection, 'ComponentPropertyIndex', TradPropertyIndexesProperty);
    RegisterPropertyEditor(TypeInfo(String), TradParameterConnection, 'EventParameter', TradEventParamProperty);

    { TradVariable }
    RegisterPropertyEditor(TypeInfo(String), TradVariable, 'ClassTypeName', TradClassTypeNameProperty);

    { Component Editors }
    RegisterComponentEditor(TradConnectionList, TradConnectionListEditor);
    RegisterComponentEditor(TradVariable, TradVariableEditor);

    { Component registration }
    RegisterNoIcon([TradP2PConnection, TradE2PConnection, TradE2MConnection]);
    RegisterComponents('TrueRAD', [TradVariable, TradConnectionList]);
end;

initialization
    RegisterClasses([TradConnectionList, TradConnection, TradP2PConnection, TradE2PConnection, TradE2MConnection]);
end.
