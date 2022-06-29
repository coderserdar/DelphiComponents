
{*******************************************************}
{                                                       }
{       TrueRAD Suite                                   }
{       Copyright (c) 2000 TrueRAD Soft                 }
{       Created by Basil Tunegov                        }
{       Version 1.0                                     }
{                                                       }
{*******************************************************}

unit radcommon;

interface

uses Classes;

type
  TradClassInfo = class;
  TradMethodInfo = class;

  TradFeature = class
  private
    FSource: TradClassInfo;
    FName: String;
  protected
    constructor Create(ASource: TradClassInfo; AName: String);
  public
    property Source: TradClassInfo read FSource;
    property Name: String read FName;
  end;

  TradParameterInfo = class(TradFeature)
  private
    FValue: Variant;
  public
    constructor Create(ASource: TradClassInfo; AName: String);
    property Value: Variant read FValue write FValue;
  end;

  TradArguments = array of TradParameterInfo;
  TradPropertyStyle = set of (psReadable, psWritable, psIndexed);
  TradPropertyVisibility = (pvProtected, pvPublic, pvPublished);

  TradPropertyInfo = class(TradFeature)
  private
    FStyle: TradPropertyStyle;
    FVisibility: TradPropertyVisibility;
    FIndexes: TradArguments;
    function GetIndexCount: Integer;
    function GetIndexes(Index: Integer): TradParameterInfo;
  public
    constructor Create(ASource: TradClassInfo; AName: String; AStyle: TradPropertyStyle; AVisibility: TradPropertyVisibility; AIndexes: TradArguments);
    property Style: TradPropertyStyle read FStyle write FStyle;
    property Visibility: TradPropertyVisibility read FVisibility write FVisibility;
    property Indexes[Index: Integer]: TradParameterInfo read GetIndexes;
    property IndexCount: Integer read GetIndexCount;
  end;

  TradMethodStyle = (msFunction, msProcedure);

  TradMethodInfo = class(TradFeature)
  private
    FStyle: TradMethodStyle;
    FArgs: TradArguments;
    FResult: Variant;
    function GetArgCount: Integer;
    function GetArgs(Index: Integer): TradParameterInfo;
  public
    constructor Create(ASource: TradClassInfo; AName: String; Arguments: TradArguments; AStyle: TradMethodStyle);
    function Find(Name: String): TradParameterInfo;
    property Style: TradMethodStyle read FStyle write FStyle;
    property Args[Index: Integer]: TradParameterInfo read GetArgs;
    property ArgCount: Integer read GetArgCount;
    property Result: Variant read FResult write FResult;
  end;

  TradConstructorInfo = class(TradFeature)
  private
    FArgs: TradArguments;
    FCreatedObject: TObject;
    function GetArgCount: Integer;
    function GetArgs(Index: Integer): TradParameterInfo;
  public
    constructor Create(ASource: TradClassInfo; AName: String; Arguments: TradArguments);
    function Find(Name: String): TradParameterInfo;
    property CreatedObject: TObject read FCreatedObject write FCreatedObject;
    property Args[Index: Integer]: TradParameterInfo read GetArgs;
    property ArgCount: Integer read GetArgCount;
  end;

  TradEventInfo = class(TradFeature)
  private
    FArgs: TradArguments;
    FVisibility: TradPropertyVisibility;
    function GetArgCount: Integer;
    function GetArgs(Index: Integer): TradParameterInfo;
  public
    constructor Create(ASource: TradClassInfo; AName: String; AArgs: TradArguments; AVisibility: TradPropertyVisibility);
    function Find(Name: String): TradParameterInfo;
    property Args[Index: Integer]: TradParameterInfo read GetArgs;
    property ArgCount: Integer read GetArgCount;
    property Visibility: TradPropertyVisibility read FVisibility write FVisibility;
  end;

  TNotifyRADEvent = procedure (Sender: TObject; Event: TradEventInfo) of object;

  TradListener = class
  private
    FEventName: String;
    FOnFiring: TNotifyRADEvent;
  public
    property EventName: String read FEventName write FEventName;
    property OnFiring: TNotifyRADEvent read FOnFiring write FOnFiring;
  end;

  TradClassInfo = class
  private
    FOwner: TObject;
    FProperties: TList;
    FMethods: TList;
    FConstructors: TList;
    FEvents: TList;
    FListeners: TList;
    procedure SetOwner(Value: TObject);
    function GetPropertyCount: Integer;
    function GetProperties(Index: Integer): TradPropertyInfo;
    function GetMethodCount: Integer;
    function GetMethods(Index: Integer): TradMethodInfo;
    function GetConstructorCount: Integer;
    function GetConstructors(Index: Integer): TradConstructorInfo;
    function GetEventCount: Integer;
    function GetEvents(Index: Integer): TradEventInfo;
  protected
    procedure ConnectEventHandlers; virtual;
    procedure UnConnectEventHandlers; virtual;
    procedure DefineRADProperties(AProperties: TList); virtual;
    procedure DefineRADMethods(AMethods: TList); virtual;
    procedure DefineRADConstructors(AConstructors: TList); virtual;
    procedure DefineRADEvents(AEvents: TList); virtual;
    procedure ReadRADProperties(Data: String);
    procedure ReadRADMethods(Data: String);
    procedure ReadRADConstructors(Data: String);
    procedure ReadRADEvents(Data: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitMetaData;
    function FindProperty(Name: String): TradPropertyInfo;
    function FindEvent(Name: String): TradEventInfo;
    function FindMethod(Name: String): TradMethodInfo;
    function FindConstructor(Name: String): TradConstructorInfo;
    function GetProperty(Prop: TradPropertyInfo): Variant; virtual;
    procedure SetProperty(Prop: TradPropertyInfo; Value: Variant); virtual;
    procedure InvokeMethod(Method: TradMethodInfo); virtual;
    procedure InvokeConstructor(Constr: TradConstructorInfo); virtual;
    procedure AddListener(Listener: TradListener);
    procedure RemoveListener(Listener: TradListener);
    procedure FireEvent(E: TradEventInfo);
    property Owner: TObject read FOwner write SetOwner;
    property PropertyCount: Integer read GetPropertyCount;
    property Properties[Index: Integer]: TradPropertyInfo read GetProperties;
    property MethodCount: Integer read GetMethodCount;
    property Methods[Index: Integer]: TradMethodInfo read GetMethods;
    property ConstructorCount: Integer read GetConstructorCount;
    property Constructors[Index: Integer]: TradConstructorInfo read GetConstructors;
    property EventCount: Integer read GetEventCount;
    property Events[Index: Integer]: TradEventInfo read GetEvents;
  end;

  TradClassInfoClass = class of TradClassInfo;

  IradObject = interface(IUnknown)
    ['{A268AC01-268D-11D4-827F-008048A98473}']
    function GetRADClassInfo: TradClassInfo;
  end;

  TradClassRegistry = class
  public
    class procedure RegisterClass(ClassName: String; InfoClass: TradClassInfoClass);
    class procedure Iterate(Proc: TGetStrProc);
    class function FindClass(ClassName: String): TradClassInfoClass;
    class function GetInfo(Obj: TObject): TradClassInfo;
  end;

implementation

uses SysUtils, StdCtrls;

type
  TStringTokenizer = class
  private
    FData: String;
    FDelim: String;
    FPos: Integer;
  public
    constructor Create(AData, ADelim: String);
    function NextToken: String;
    function IsEnd: Boolean;
  end;

//------------------------------------------------------------------------------
// TStringTokenizer implementation
//------------------------------------------------------------------------------
constructor TStringTokenizer.Create(AData, ADelim: String);
begin
    FData := AData;
    FDelim := ADelim;
    FPos := 1;
end;

//------------------------------------------------------------------------------
function TStringTokenizer.NextToken: String;
var
    Res: String;
begin
    while (FPos <= Length(FData)) do begin
        if FData[FPos] <> FDelim[1] then
            Res := Res + FData[FPos]
        else if FPos + 1 > Length(FData) then begin
            Res := Res + FData[FPos];
            FPos := FPos + 1;
            Break;
        end else if FData[FPos + 1] = FDelim[2] then begin
            FPos := FPos + 2;
            Break;
        end;
        FPos := FPos + 1;
    end;
    Result := Res;
end;

//------------------------------------------------------------------------------
function TStringTokenizer.IsEnd: Boolean;
begin
    Result := FPos >= Length(FData);
end;

//------------------------------------------------------------------------------
// TradFeature implementation
//------------------------------------------------------------------------------
constructor TradFeature.Create(ASource: TradClassInfo; AName: String);
begin
    inherited Create;
    FSource := ASource;
    FName := AName;
end;

//------------------------------------------------------------------------------
// TradParameterInfo implementation
//------------------------------------------------------------------------------
constructor TradParameterInfo.Create(ASource: TradClassInfo; AName: String);
begin
    inherited Create(ASource, AName);
end;

//------------------------------------------------------------------------------
// TradPropertyInfo implementation
//------------------------------------------------------------------------------
constructor TradPropertyInfo.Create(ASource: TradClassInfo; AName: String; AStyle: TradPropertyStyle; AVisibility: TradPropertyVisibility; AIndexes: TradArguments);
var
    i: Integer;
begin
    inherited Create(ASource, AName);
    FStyle := AStyle;
    FVisibility := AVisibility;
    SetLength(FIndexes, Length(AIndexes));
    for i := 0 to Length(AIndexes) - 1 do
        FIndexes[i] := AIndexes[i];
end;

//------------------------------------------------------------------------------
function TradPropertyInfo.GetIndexCount: Integer;
begin
    Result := Length(FIndexes);
end;

//------------------------------------------------------------------------------
function TradPropertyInfo.GetIndexes(Index: Integer): TradParameterInfo;
begin
    Result := FIndexes[Index];
end;

//------------------------------------------------------------------------------
// TradMethodInfo implementation
//------------------------------------------------------------------------------
constructor TradMethodInfo.Create(ASource: TradClassInfo; AName: String; Arguments: TradArguments; AStyle: TradMethodStyle);
var
    i: Integer;
begin
    inherited Create(ASource, AName);
    FStyle := AStyle;
    SetLength(FArgs, Length(Arguments));
    for i := 0 to Length(FArgs) - 1 do
        FArgs[i] := Arguments[i];
end;

//------------------------------------------------------------------------------
function TradMethodInfo.Find(Name: String): TradParameterInfo;
var
    i: Integer;
begin
    for i := 0 to Length(FArgs) - 1 do begin
        if FArgs[i].Name = Name then begin
            Result := FArgs[i];
            Exit;
        end;
    end;
    Result := nil;
end;

//------------------------------------------------------------------------------
function TradMethodInfo.GetArgCount: Integer;
begin
    Result := Length(FArgs);
end;

//------------------------------------------------------------------------------
function TradMethodInfo.GetArgs(Index: Integer): TradParameterInfo;
begin
    Result := FArgs[Index];
end;

//------------------------------------------------------------------------------
// TradConstructorInfo implementation
//------------------------------------------------------------------------------
constructor TradConstructorInfo.Create(ASource: TradClassInfo; AName: String; Arguments: TradArguments);
var
    i: Integer;
begin
    inherited Create(ASource, AName);
    SetLength(FArgs, Length(Arguments));
    for i := 0 to Length(FArgs) - 1 do
        FArgs[i] := Arguments[i];
end;

//------------------------------------------------------------------------------
function TradConstructorInfo.Find(Name: String): TradParameterInfo;
var
    i: Integer;
begin
    for i := 0 to Length(FArgs) - 1 do begin
        if FArgs[i].Name = Name then begin
            Result := FArgs[i];
            Exit;
        end;
    end;
    Result := nil;
end;

//------------------------------------------------------------------------------
function TradConstructorInfo.GetArgCount: Integer;
begin
    Result := Length(FArgs);
end;

//------------------------------------------------------------------------------
function TradConstructorInfo.GetArgs(Index: Integer): TradParameterInfo;
begin
    Result := FArgs[Index];
end;

//------------------------------------------------------------------------------
// TradEventInfo implementation
//------------------------------------------------------------------------------
constructor TradEventInfo.Create(ASource: TradClassInfo; AName: String; AArgs: TradArguments; AVisibility: TradPropertyVisibility);
var
    i: Integer;
begin
    inherited Create(ASource, AName);

    SetLength(FArgs, Length(AArgs));
    for i := 0 to Length(FArgs) - 1 do
        FArgs[i] := AArgs[i];

    FVisibility := AVisibility;
end;

//------------------------------------------------------------------------------
function TradEventInfo.Find(Name: String): TradParameterInfo;
var
    i: Integer;
begin
    for i := 0 to Length(FArgs) - 1 do begin
        if FArgs[i].Name = Name then begin
            Result := FArgs[i];
            Exit;
        end;
    end;
    Result := nil;
end;

//------------------------------------------------------------------------------
function TradEventInfo.GetArgCount: Integer;
begin
    Result := Length(FArgs);
end;

//------------------------------------------------------------------------------
function TradEventInfo.GetArgs(Index: Integer): TradParameterInfo;
begin
    Result := FArgs[Index];
end;

//------------------------------------------------------------------------------
// TradClassInfo implementation
//------------------------------------------------------------------------------
constructor TradClassInfo.Create;
begin
    inherited Create;
    FProperties := TList.Create;
    FMethods := TList.Create;
    FConstructors := TList.Create;
    FEvents := TList.Create;
    FListeners := TList.Create;
end;

//------------------------------------------------------------------------------
destructor TradClassInfo.Destroy;
var
    i: Integer;
begin
{    for i := 0 to FProperties.Count - 1 do
        TradPropertyInfo(FProperties[0]).Free;}
    FProperties.Free;
{    for i := 0 to FMethods.Count - 1 do
        TradMethodInfo(FMethods[0]).Free;}
    FMethods.Free;
{    for i := 0 to FConstructors.Count - 1 do
        TradConstructorInfo(FConstructors[0]).Free;}
    FConstructors.Free;
{    for i := 0 to FEvents.Count - 1 do
        TradEventInfo(FEvents[0]).Free;}
    FEvents.Free;
    FListeners.Free;
    inherited Destroy;
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.InitMetaData;
begin
    FProperties.Clear;
    FMethods.Clear;
    FConstructors.Clear;
    FEvents.Clear;
    DefineRADProperties(FProperties);
    DefineRADMethods(FMethods);
    DefineRADConstructors(FConstructors);
    DefineRADEvents(FEvents);
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.SetOwner(Value: TObject);
begin
    if Assigned(FOwner) then UnConnectEventHandlers;
    FOwner := Value;
    if Assigned(Value) then ConnectEventHandlers;
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.ConnectEventHandlers;
begin
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.UnConnectEventHandlers;
begin
end;

//------------------------------------------------------------------------------
function TradClassInfo.FindProperty(Name: String): TradPropertyInfo;
var
    i: Integer;
begin
    for i := 0 to PropertyCount - 1 do begin
        if Properties[i].Name = Name then begin
            Result := Properties[i];
            Exit;
        end;
    end;
    Result := nil;
end;

//------------------------------------------------------------------------------
function TradClassInfo.FindEvent(Name: String): TradEventInfo;
var
    i: Integer;
begin
    for i := 0 to EventCount - 1 do begin
        if Events[i].Name = Name then begin
            Result := Events[i];
            Exit;
        end;
    end;
    Result := nil;
end;

//------------------------------------------------------------------------------
function TradClassInfo.FindMethod(Name: String): TradMethodInfo;
var
    i: Integer;
begin
    for i := 0 to MethodCount - 1 do begin
        if Methods[i].Name = Name then begin
            Result := Methods[i];
            Exit;
        end;
    end;
    Result := nil;
end;

//------------------------------------------------------------------------------
function TradClassInfo.FindConstructor(Name: String): TradConstructorInfo;
var
    i: Integer;
begin
    for i := 0 to ConstructorCount - 1 do begin
        if Constructors[i].Name = Name then begin
            Result := Constructors[i];
            Exit;
        end;
    end;
    Result := nil;
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetProperty(Prop: TradPropertyInfo): Variant;
begin
    raise Exception.Create('Property ' + Prop.Name + ' not found!');
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.SetProperty(Prop: TradPropertyInfo; Value: Variant);
begin
    raise Exception.Create('Property ' + Prop.Name + ' not found!');
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.InvokeMethod(Method: TradMethodInfo);
begin
    raise Exception.Create('Method ' + Method.Name + ' not found!');
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.InvokeConstructor(Constr: TradConstructorInfo);
begin
    raise Exception.Create('Constructor ' + Constr.Name + ' not found!');
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.AddListener(Listener: TradListener);
begin
    if Assigned(Listener) then
        FListeners.Add(Listener);
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.RemoveListener(Listener: TradListener);
begin
    if Assigned(Listener) then
        FListeners.Remove(Listener);
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.DefineRADProperties(AProperties: TList);
begin
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.DefineRADMethods(AMethods: TList);
begin
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.DefineRADConstructors(AConstructors: TList);
begin
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.DefineRADEvents(AEvents: TList);
begin
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.FireEvent(E: TradEventInfo);
var
    i: Integer;
begin
    for i := 0 to FListeners.Count - 1 do begin
        if (TradListener(FListeners.Items[i]).EventName = E.Name) and
           Assigned(TradListener(FListeners.Items[i]).OnFiring) then
        begin
            TradListener(FListeners.Items[i]).OnFiring(FOwner, E);
        end;
    end;
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.ReadRADProperties(Data: String);
    function ReadStyle(Token: String): TradPropertyStyle;
    var
        Tokenizer: TStringTokenizer;
        S: String;
        Res: TradPropertyStyle;
    begin
        Res := [];
        Tokenizer := TStringTokenizer.Create(Token, '..');
        try
            while Tokenizer.IsEnd = false do begin
                S := Tokenizer.NextToken;
                if S = 'psReadable' then
                    Include(Res, psReadable)
                else if S = 'psWritable' then
                    Include(Res, psWritable)
                else if S = 'psIndexed' then
                    Include(Res, psIndexed)
                else
                    raise Exception.Create(Token + ' Invalid property style property!');
            end;
        finally
            Tokenizer.Free;
        end;
        Result := Res;
    end;

    function ReadVisibility(Token: String): TradPropertyVisibility;
    begin
        if Token = 'pvProtected' then
            Result := pvProtected
        else if Token = 'pvPublic' then
            Result := pvPublic
        else if Token = 'pvPublished' then
            Result := pvPublished
        else
            raise Exception.Create(Token + ' Invalid property visibility property!');
    end;
var
    Tokenizer: TStringTokenizer;
    Define: String;
    Name: String;
    Style: TradPropertyStyle;
    Visibility: TradPropertyVisibility;
    Indexes: TradArguments;
    i: Integer;
    Prop: TradPropertyInfo;
begin
    Tokenizer := TStringTokenizer.Create(Data, '::');
    try
        while not Tokenizer.IsEnd do begin
            Define := Tokenizer.NextToken;
            if Define = 'Define' then begin
                Name := Tokenizer.NextToken;
                Style := ReadStyle(Tokenizer.NextToken);
                Visibility := ReadVisibility(Tokenizer.NextToken);
                SetLength(Indexes, StrToInt(Tokenizer.NextToken));
                for i := 0 to Length(Indexes) - 1 do
                    Indexes[i] := TradParameterInfo.Create(Self, Tokenizer.NextToken);
                Prop := TradPropertyInfo.Create(Self, Name, Style, Visibility, Indexes);
                FProperties.Add(Prop);
            end else if Define = 'Override' then begin
                Name := Tokenizer.NextToken;
                Style := ReadStyle(Tokenizer.NextToken);
                Visibility := ReadVisibility(Tokenizer.NextToken);
                Prop := FindProperty(Name);
                if Style <> [] then
                    Prop.Style := Style;
                Prop.Visibility := Visibility;
            end else
                raise Exception.Create('Invalid property define token!');
        end;
    finally
        Tokenizer.Free;
    end;
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.ReadRADMethods(Data: String);
    function ReadStyle(Token: String): TradMethodStyle;
    begin
        if Token = 'msFunction' then
            Result := msFunction
        else if Token = 'msProcedure' then
            Result := msProcedure
        else
            raise Exception.Create('Invalid method style property!');
    end;
var
    Tokenizer: TStringTokenizer;
    Name: String;
    Style: TradMethodStyle;
    ArgCount: Integer;
    Args: TradArguments;
    ParamName: String;
    Method: TradMethodInfo;
    i: Integer;
begin
    Tokenizer := TStringTokenizer.Create(Data, '::');
    try
        while not Tokenizer.IsEnd do begin
            Name := Tokenizer.NextToken;
            Style := ReadStyle(Tokenizer.NextToken);
            ArgCount := StrToInt(Tokenizer.NextToken);
            SetLength(Args, ArgCount);
            for i := 0 to ArgCount - 1 do begin
                ParamName := Tokenizer.NextToken;
                Args[i] := TradParameterInfo.Create(Self, ParamName);
            end;
            Method := TradMethodInfo.Create(Self, Name, Args, Style);
            FMethods.Add(Method);
        end;
    finally
        Tokenizer.Free;
    end;
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.ReadRADConstructors(Data: String);
var
    Tokenizer: TStringTokenizer;
    Name: String;
    ArgCount: Integer;
    Args: TradArguments;
    ParamName: String;
    Constr: TradConstructorInfo;
    i: Integer;
begin
    Tokenizer := TStringTokenizer.Create(Data, '::');
    try
        while not Tokenizer.IsEnd do begin
            Name := Tokenizer.NextToken;
            ArgCount := StrToInt(Tokenizer.NextToken);
            SetLength(Args, ArgCount);
            for i := 0 to ArgCount - 1 do begin
                ParamName := Tokenizer.NextToken;
                Args[i] := TradParameterInfo.Create(Self, ParamName);
            end;
            Constr := TradConstructorInfo.Create(Self, Name, Args);
            FConstructors.Add(Constr);
        end;
    finally
        Tokenizer.Free;
    end;
end;

//------------------------------------------------------------------------------
procedure TradClassInfo.ReadRADEvents(Data: String);
    function ReadVisibility(Token: String): TradPropertyVisibility;
    begin
        if Token = 'pvProtected' then
            Result := pvProtected
        else if Token = 'pvPublic' then
            Result := pvPublic
        else if Token = 'pvPublished' then
            Result := pvPublished
        else
            raise Exception.Create(Token + ' Invalid event property visibility property!');
    end;
var
    Tokenizer: TStringTokenizer;
    Define: String;
    Name: String;
    ArgCount: Integer;
    Args: TradArguments;
    ParamName: String;
    Visibility: TradPropertyVisibility;
    Event: TradEventInfo;
    i: Integer;
begin
    Tokenizer := TStringTokenizer.Create(Data, '::');
    try
        while not Tokenizer.IsEnd do begin
            Define := Tokenizer.NextToken;
            if Define = 'Define' then begin
                Name := Tokenizer.NextToken;
                Visibility := ReadVisibility(Tokenizer.NextToken);
                ArgCount := StrToInt(Tokenizer.NextToken);
                SetLength(Args, ArgCount);
                for i := 0 to ArgCount - 1 do begin
                    ParamName := Tokenizer.NextToken;
                    Args[i] := TradParameterInfo.Create(Self, ParamName);
                end;
                Event := TradEventInfo.Create(Self, Name, Args, Visibility);
                FEvents.Add(Event);
            end else if Define = 'Override' then begin
                Name := Tokenizer.NextToken;
                Visibility := ReadVisibility(Tokenizer.NextToken);
                Event := FindEvent(Name);
                Event.Visibility := Visibility;
            end else
                raise Exception.Create('Invalid property define token!');
            end;
    finally
        Tokenizer.Free;
    end;
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetPropertyCount: Integer;
begin
    Result := FProperties.Count;
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetProperties(Index: Integer): TradPropertyInfo;
begin
    Result := TradPropertyInfo(FProperties.Items[Index]);
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetMethodCount: Integer;
begin
    Result := FMethods.Count;
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetMethods(Index: Integer): TradMethodInfo;
begin
    Result := TradMethodInfo(FMethods.Items[Index]);
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetConstructorCount: Integer;
begin
    Result := FConstructors.Count;
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetConstructors(Index: Integer): TradConstructorInfo;
begin
    Result := TradConstructorInfo(FConstructors.Items[Index]);
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetEventCount: Integer;
begin
    Result := FEvents.Count;
end;

//------------------------------------------------------------------------------
function TradClassInfo.GetEvents(Index: Integer): TradEventInfo;
begin
    Result := TradEventInfo(FEvents.Items[Index]);
end;

type
  TradNotifier = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;

  PradClassInfoRec = ^TradClassInfoRec;
  TradClassInfoRec = record
    ClassName: String;
    InfoClass: TradClassInfoClass;
  end;

var
  ClassInfoClassList: TList;
  ClassInfoList: TList;
  Notifier: TradNotifier;

//------------------------------------------------------------------------------
// TradNotifier implemetation
//------------------------------------------------------------------------------
procedure TradNotifier.Notification(AComponent: TComponent; Operation: TOperation);
var
    i: Integer;
begin
    if Operation = opRemove then begin
        for i := 0 to ClassInfoList.Count - 1 do begin
            if TradClassInfo(ClassInfoList.Items[i]).Owner = AComponent then begin
                TradClassInfo(ClassInfoList.Items[i]).Free;
                ClassInfoList.Items[i] := nil;
            end;
        end;
        ClassInfoList.Pack;
    end;
    inherited Notification(AComponent, Operation);
end;

//------------------------------------------------------------------------------
// TradClassRegistry implemetation
//------------------------------------------------------------------------------
class procedure TradClassRegistry.RegisterClass(ClassName: String; InfoClass: TradClassInfoClass);
var
    InfoRec: PRADClassInfoRec;
begin
    if not Assigned(FindClass(ClassName)) then begin
        New(InfoRec);
        InfoRec^.ClassName := ClassName;
        InfoRec^.InfoClass := InfoClass;
        ClassInfoClassList.Add(InfoRec);
    end;
end;

//------------------------------------------------------------------------------
class procedure TradClassRegistry.Iterate(Proc: TGetStrProc);
var
    i: Integer;
begin
    for i := 0 to ClassInfoClassList.Count - 1 do
        Proc(PradClassInfoRec(ClassInfoClassList.Items[i])^.ClassName);
end;

//------------------------------------------------------------------------------
class function TradClassRegistry.FindClass(ClassName: String): TradClassInfoClass;
var
    i: Integer;
begin
    for i := 0 to ClassInfoClassList.Count - 1 do begin
        if AnsiCompareText(PradClassInfoRec(ClassInfoClassList.Items[i])^.ClassName, ClassName) = 0 then begin
            Result := PradClassInfoRec(ClassInfoClassList.Items[i])^.InfoClass;
            Exit;
        end;
    end;
    Result := nil;
end;

//------------------------------------------------------------------------------
class function TradClassRegistry.GetInfo(Obj: TObject): TradClassInfo;
var
    i: Integer;
    CompInfoClass: TradClassInfoClass;
    NewCompInfo: TradClassInfo;
    radObject: IradObject;
begin
    if Obj.GetInterface(IradObject, radObject) then begin
        Result := radObject.GetRADClassInfo;
        Exit;
    end;

    for i := 0 to ClassInfoList.Count - 1 do begin
        if TradClassInfo(ClassInfoList.Items[i]).Owner = Obj then begin
            Result := TradClassInfo(ClassInfoList.Items[i]);
            Exit;
        end;
    end;

    CompInfoClass := Self.FindClass(Obj.ClassName);
    NewCompInfo := TradClassInfo(CompInfoClass.NewInstance);
    NewCompInfo.Create;
    NewCompInfo.InitMetaData;
    NewCompInfo.Owner := Obj;
    ClassInfoList.Add(NewCompInfo);
    if Obj is TComponent then
        Notifier.FreeNotification(Obj as TComponent);
    Result := NewCompInfo;
end;

initialization
    ClassInfoClassList := TList.Create;
    ClassInfoList := TList.Create;
    Notifier := TradNotifier.Create(nil);

finalization
    {while ClassInfoList.Count > 0 do
        Dispose(ClassInfoList.Items[0]);}
    ClassInfoClassList.Free;
    ClassInfoList.Free;
//    Notifier.Free;
end.
