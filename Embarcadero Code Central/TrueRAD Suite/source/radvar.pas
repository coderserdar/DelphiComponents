
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit radvar;

interface

uses Classes, radcommon;

type
  TradVariableClassInfo = class(TradClassInfo)
  private
    FSelf: TObject;
    FSelfClassInfo: TradClassInfo;
  protected
    procedure DefineRADProperties(AProperties: TList); override;
    procedure DefineRADMethods(AMethods: TList); override;
    procedure DefineRADEvents(AEvents: TList); override;
    procedure Update(ClassTypeName: String);
  public
    constructor Create(ClassTypeName: String);
    destructor Destroy; override;
    function GetProperty(Prop: TradPropertyInfo): Variant; override;
    procedure SetProperty(Prop: TradPropertyInfo; Value: Variant); override;
    procedure InvokeMethod(Method: TradMethodInfo); override;
  end;

  TradVariable = class(TComponent, IradObject)
  private
    FClassTypeName: String;
    FClassInfo: TradVariableClassInfo;
    procedure SetClassTypeName(Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetRADClassInfo: TradClassInfo;
  published
    property ClassTypeName: String read FClassTypeName write SetClassTypeName;
  end;

implementation

uses SysUtils;

const
  SRADVariableProperties: String =
    'Override::Self::psReadable..psWritable::pvPublished::0';

  SRADVariableEvents: String =
    'Define::OnInstanceChange::pvPublished::1::Instance';

//------------------------------------------------------------------------------
// TradVariableClassInfo implementation
//------------------------------------------------------------------------------
constructor TradVariableClassInfo.Create(ClassTypeName: String);
begin
    inherited Create;
    Update(ClassTypeName);
end;

//------------------------------------------------------------------------------
destructor TradVariableClassInfo.Destroy;
begin
    FSelfClassInfo.Free;
    inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TradVariableClassInfo.Update(ClassTypeName: String);
var
    CompInfoClass: TradClassInfoClass;
begin
    if Assigned(FSelfClassInfo) then FSelfClassInfo.Free;
    CompInfoClass := TradClassRegistry.FindClass(ClassTypeName);
    FSelfClassInfo := TradClassInfo(CompInfoClass.NewInstance);
    FSelfClassInfo.Create;
    FSelfClassInfo.InitMetaData;
end;

//------------------------------------------------------------------------------
procedure TradVariableClassInfo.DefineRADProperties(AProperties: TList);
var
    i: Integer;
begin
    for i := 0 to FSelfClassInfo.PropertyCount - 1 do
        AProperties.Add(FSelfClassInfo.Properties[i]);
    ReadRADProperties(SRADVariableProperties);
end;

//------------------------------------------------------------------------------
procedure TradVariableClassInfo.DefineRADMethods(AMethods: TList);
var
    i: Integer;
begin
    for i := 0 to FSelfClassInfo.MethodCount - 1 do
        AMethods.Add(FSelfClassInfo.Methods[i]);
end;

//------------------------------------------------------------------------------
procedure TradVariableClassInfo.DefineRADEvents(AEvents: TList);
var
    i: Integer;
begin
    for i := 0 to FSelfClassInfo.EventCount - 1 do
        AEvents.Add(FSelfClassInfo.Events[i]);
    ReadRADEvents(SRADVariableEvents);
end;

//------------------------------------------------------------------------------
function TradVariableClassInfo.GetProperty(Prop: TradPropertyInfo): Variant;
begin
    if Prop.Name = 'Self' then
        Result := Longint(FSelf)
    else
        Result := FSelfClassInfo.GetProperty(Prop);
end;

//------------------------------------------------------------------------------
procedure TradVariableClassInfo.SetProperty(Prop: TradPropertyInfo; Value: Variant);
var
    E: TradEventInfo;
begin
    if Prop.Name = 'Self' then begin
        FSelf := TObject(Longint(Value));
        FSelfClassInfo.Owner := FSelf;
        E := FindEvent('OnInstanceChange');
        E.Args[0].Value := Longint(FSelf);
        FireEvent(E);
    end else
        FSelfClassInfo.SetProperty(Prop, Value);
end;

//------------------------------------------------------------------------------
procedure TradVariableClassInfo.InvokeMethod(Method: TradMethodInfo);
begin
    FSelfClassInfo.InvokeMethod(Method);
end;

//------------------------------------------------------------------------------
// TradVariable implementation
//------------------------------------------------------------------------------
constructor TradVariable.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FClassTypeName := 'TObject';
    FClassInfo := TradVariableClassInfo.Create(FClassTypeName);
    FClassInfo.InitMetaData;
end;

//------------------------------------------------------------------------------
destructor TradVariable.Destroy;
begin
    FClassInfo.Free;
    inherited Destroy;
end;

//------------------------------------------------------------------------------
function TradVariable.GetRADClassInfo: TradClassInfo;
begin
    Result := FClassInfo;
end;

//------------------------------------------------------------------------------
procedure TradVariable.SetClassTypeName(Value: String);
var
    CompInfoClass: TradClassInfoClass;
begin
    CompInfoClass := TradClassRegistry.FindClass(Value);
    if not Assigned(CompInfoClass) then raise Exception.Create('Unknown class!');
    if Value <> FClassTypeName then begin
        FClassTypeName := Value;
        FClassInfo.Update(FClassTypeName);
        FClassInfo.InitMetaData;
    end;
end;

initialization
    TradClassRegistry.RegisterClass('TradVariable', TradVariableClassInfo);
end.
