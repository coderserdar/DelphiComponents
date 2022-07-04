
(********************************************************)
(*                                                      *)
(*  Codebot Class Library @ www.codebot.org/delphi      *)
(*                                                      *)
(*  1.00.01 Open Source Released 2006                   *)
(*                                                      *)
(********************************************************)

unit Rules;

interface

{$I STD.INC}

uses
  ActiveX, Classes, DB, ADOdb, ComObj, SysUtils, StrTools;

{ General array datatypes }

type
  TIntegerArray = array of Integer;
  PIntegerArray = ^TIntegerArray;

  TStringArray = array of string;
  PStringArray = ^TStringArray;

{ Aliases }

  TVariantTypes = TIntegerArray;

{ Array datatype construction routines }

function IntegerArray(const Items: array of Integer): TIntegerArray;
function StringArray(const Items: array of string): TStringArray;

{ The Contains functions checks if the Container holds a copy of Item }

function Contains(const Container: TIntegerArray; Item: Integer): Boolean; overload;
function Contains(const Container, Item: TIntegerArray): Boolean; overload;
function Contains(const Container: TStringArray; Item: string): Boolean; overload;
function Contains(const Container, Item: TStringArray): Boolean; overload;

{ The Combine functions merges multiple arrays into a single array }

procedure Combine(var Dest: TIntegerArray; const Values: TIntegerArray); overload;
procedure Combine(var Dest: TIntegerArray; const Values: array of Integer); overload;
{ function Combine(const Values: array of TStringArray): TStringArray; overload; }

{ General range datatypes }

type
  TIntegerRange = record
    Lo: Integer;
    Hi: Integer;
  end;

  TDateRange = record
    Lo: TDateTime;
    Hi: TDateTime;
  end;

{ Range datatype construction routines }

function IntegerRange(const Lo, Hi: Integer): TIntegerRange;
function DateRange(const Lo, Hi: TDateTime): TDateRange;

{ Range datatype testing routing }

function IsRangeEmpty(Range: TIntegerRange): Boolean; overload;
function IsRangeEmpty(Range: TDateRange): Boolean; overload;

{ TParamItem datatype }

type
  TParamItem = record
    Name: string;
    Value: Variant;
    DataType: TFieldType;
  end;

{ TParamItem construction routines }

function StrParam(const Name: string; const Value: string): TParamItem;
function IntParam(const Name: string; Value: Integer): TParamItem;
function DateTimeParam(const Name: string; const Value: TDateTime): TParamItem;
function DateParam(const Name: string; const Value: TDateTime): TParamItem;
function FloatParam(const Name: string; const Value: Double): TParamItem;

{ Default string formats }

const
  DefaultCurrencyFormat: string = '%.2m';
  DefaultDateFormat: string = 'm/dd/yyyy';
  DefaultTimeFormat: string = 'h:mm:ss';
  DefaultDateTimeFormat: string = 'm/dd/yyyy h:mm:ssAM/PM';

const
  vtIntegers: TVariantTypes = nil;
  vtStrings: TVariantTypes = nil;

{ TRule class

  Serves as the base class for database business rules objects. This class
  take care of database connections, provides place holders for storage and
  retrieval of data. Since the TRules class derives from TPersistent, it is
  compatible with derived classes of the same type.

  The following are methods that can be overridden to change the behavior of
  derived classes:

    Locate
      Searches the database for a record that matches the Key parameter. Key is
      passed as a constant Variant which cna be checked for type using the
      VarType function from the System unit. If a derived class finds a match
      it should call ReadValues and return True, otherwise it should call set
      the Result to the inherited Locate value.

    Initialize
      Resets all fields within the class to zero, and sets the ID property to
      negative 1. A call to Clear is the same as calling Initialize directly.
      If the ID of your object is negative 1 and Revert is called, TRules will
      call Initialize for you.

    ReadValues
      Abstract method that must be overriden in derived classes. Typically you
      will override Locate and call ReadValues within it, at which point you can
      populate fields of your class from the database.

    WriteValues
      Abstract method that must be overriden in derived classes. Very similar to
      the ReadValues method, except it is invoked when Save has been called
      while the object is modified. The return value of the WriteValues function
      must be the new or exsisting ID of the object. }

type
  TChildRule = class;
  TChildRuleClass = class of TChildRule;

{ TRule }

  TRule = class(TPersistent)
  private
    FDestroying: Boolean;
    FID: Integer;
    FList: TList;
    FOnChange: TNotifyEvent;
    function GetQuery: TADOQuery;
    procedure SetID(Value: Integer);
  protected
    FModified: Boolean;
    FOnLocate: TNotifyEvent;
    function AddChild(ChildClass: TChildRuleClass): TChildRule;
    procedure Changed; virtual;
    function DefaultLocate(const VarTypes: TVariantTypes; const Key: Variant): Boolean;
    procedure Initialize; virtual;
    procedure ReadValues; virtual; abstract;
    procedure UpdateIdentity(Identity: Integer);
    function WriteValues: Integer; virtual; abstract;
    property Destroying: Boolean read FDestroying;
    property Query: TADOQuery read GetQuery;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    procedure Delete; virtual;
    function Locate(const Key: Variant): Boolean; virtual;
    procedure Revert;
    procedure Save;
    property ID: Integer read FID write SetID;
    property Modified: Boolean read FModified;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnLocate: TNotifyEvent read FOnLocate write FOnLocate;
  end;

{ TChildRule }

  TChildRule = class (TRule)
  private
    FParent: TRule;
  protected
    procedure Changed; override;
    property Parent: TRule read FParent;
  public
    destructor Destroy; override;
  end;

{ TRulesConnection class }

type
  TConnectionKind = (ckComponent, ckString);

  EConnectionError = class(Exception);

  TRulesConnection = class (TObject)
  private
    FConnection: TADOConnection;
    FConnectionCount: Integer;
    FConnectionString: String;
    FKind: TConnectionKind;
    FQuery: TADOQuery;
    FTransRef: Integer;
    FUser: Integer;
    procedure CheckConnection;
    procedure SetConnection(Value: TADOConnection);
    procedure SetConnectionString(const Value: String);
  protected
    procedure Connect(Rule: TRule);
    procedure Disconnect(Rule: TRule);
    property Query: TADOQuery read FQuery;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginTrans;
    procedure EndTrans;
    procedure RollbackTrans;
    property Connection: TADOConnection read FConnection write SetConnection;
    property ConnectionCount: Integer read FConnectionCount;
    property ConnectionString: string read FConnectionString write SetConnectionString;
    property Kind: TConnectionKind read FKind;
    property User: Integer read FUser write FUser;
  end;

{ The RulesConnection function returns an instance of the connection manager
  for all rule objects. You should set the Connection or ConnectionString
  property of the object before you create an instance of a rule object. }

function RulesConnection: TRulesConnection;

{ IServerConnection interface }

type
  IServerConnection = interface(IUnknown)
    function GetQuery: TADOQuery;
    procedure Connect(const ServerParams: TServerParams);
    property Query: TADOQuery read GetQuery;
  end;

{ TConnectableObject class }

  TConnectableObject = class(TAutoIntfObject)
  private
    FConnection: IServerConnection;
    function GetQuery: TADOQuery;
  public
    constructor Create(Connection: IServerConnection; const TypeLib: ITypeLib;
      const DispIntf: TGUID); virtual;
    property Query: TADOQuery read GetQuery;
  end;

  TConnectableClass = class of TConnectableObject;

{ TIdentityObject class }

  TIdentityObject = class(TConnectableObject)
  private
    FID: Integer;
  public
    property ID: Integer read FID write FID;
  end;

{ ConnectDataset procedure }

procedure ConnectDataset(Dataset: TCustomADODataset);

{ XML dataset translations }

function DatasetToText(Dataset: TCustomADODataSet): string;
function TextToDataset(const S: string): TCustomADODataSet;

{ The PurifyStatement procedure removes excess information from query statements }

procedure PurifyStatement(var Statement: string; const Param: string;
 const Value: string); overload;
procedure PurifyStatement(var Statement: string; const Param: string;
  const Value: Integer); overload;

{ Quick database access routines }

procedure ExecuteStatement(const Statement: string); overload;
procedure ExecuteStatement(const Statement: string;
  const Params: array of TParamItem); overload;

function OpenStatement(const Statement: string): TDataset; overload;
function OpenStatement(const Statement: string;
  const Params: array of TParamItem): TDataSet; overload;

procedure SafeSetParam(Query: TADOQuery; const Name: string;
  const Value: Variant);

procedure SetStrParam(Parameters: TParameters; const Name: string; const Value: string);
procedure SetDateParam(Parameters: TParameters; const Name: string; const Value: TDateTime);
procedure SetDateTimeParam(Parameters: TParameters; const Name: string; const Value: TDateTime);


implementation

uses
  StrConst;

type
  PInteger = ^Integer;

procedure ClearChildList(List: TList);
var
  I: Integer;
begin
  if List <> nil then
    for I := 0 to List.Count - 1 do
      TObject(List).Free;
end;

function IntegerArray(const Items: array of Integer): TIntegerArray;
var
  I: Integer;
begin
  SetLength(Result, High(Items) - Low(Items) + 1);
  for I := 0 to Length(Result) - 1 do
    Result[I] := Items[Low(Items) + I];
end;

function StringArray(const Items: array of string): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, High(Items) - Low(Items) + 1);
  for I := 0 to Length(Result) - 1 do
    Result[I] := Items[Low(Items) + I];
end;

function Contains(const Container: TIntegerArray; Item: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Container) to High(Container) do
    if Container[I] = Item then
    begin
      Result := True;
      Break;
    end;
end;

function Contains(const Container, Item: TIntegerArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Item) to High(Item) do
    if Contains(Container, Item[I]) then
    begin
      Result := True;
      Break;
    end;
end;

function Contains(const Container: TStringArray; Item: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Container) to High(Container) do
    if Container[I] = Item then
    begin
      Result := True;
      Break;
    end;
end;

function Contains(const Container, Item: TStringArray): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := Low(Item) to High(Item) do
    if Contains(Container, Item[I]) then
    begin
      Result := True;
      Break;
    end;
end;

procedure Combine(var Dest: TIntegerArray; const Values: TIntegerArray);
var
  Len: Integer;
  I: Integer;
begin
  Len := Length(Dest);
  I := Len;
  Inc(Len, Length(Values));
  if I = Len then Exit;
  SetLength(Dest, Len);
  Move(PInteger(@Values[0])^, PInteger(@Dest[I])^, Length(Values) *
    SizeOf(Integer));
end;

procedure Combine(var Dest: TIntegerArray; const Values: array of Integer);
var
  Len: Integer;
  I, J: Integer;
begin
  Len := Length(Dest);
  I := High(Values) - Low(Values) + 1;
  if I = 0 then Exit;
  SetLength(Dest, Len + I);
  J := 0;
  for I := Len to Length(Dest) - 1 do
  begin
    Dest[I] := Values[Low(Values) + J];
    Inc(J);
  end;
end;

function IntegerRange(const Lo, Hi: Integer): TIntegerRange;
begin
  Result.Lo := Lo;
  Result.Hi := Hi;
end;

function DateRange(const Lo, Hi: TDateTime): TDateRange;
begin
  Result.Lo := Lo;
  Result.Hi := Hi;
end;

function IsRangeEmpty(Range: TIntegerRange): Boolean;
begin
  with Range do
    Result := (Lo = 0) and (Hi = 0);
end;

function IsRangeEmpty(Range: TDateRange): Boolean;
begin
  with Range do
    Result := (Lo = 0) and (Hi = 0);
end;

function StrParam(const Name: string; const Value: string): TParamItem;
begin
  Result.Name := Name;
  if Value = '' then
    Result.Value := NULL
  else
    Result.Value := Value;
  Result.DataType := ftString;
end;

function IntParam(const Name: string; Value: Integer): TParamItem;
begin
  Result.Name := Name;
  Result.Value := Value;
  Result.DataType := ftInteger;
end;

function DateTimeParam(const Name: string; const Value: TDateTime): TParamItem;
begin
  Result.Name := Name;
  if Value = 0 then
    Result.Value := NULL
  else
    Result.Value := Value;
  Result.DataType := ftDateTime;
end;

function DateParam(const Name: string; const Value: TDateTime): TParamItem;
var
  Date: TDateTime;
begin
  Date := Trunc(Value);
  Result.Name := Name;
  if Value = 0 then
    Result.Value := NULL
  else
    Result.Value := Date;
  Result.DataType := ftDate;
end;

function FloatParam(const Name: string; const Value: Double): TParamItem;
begin
  Result.Name := Name;
  Result.Value := Value;
  Result.DataType := ftFloat;
end;

{ TChildRule }

procedure TChildRule.Changed;
begin
  inherited Changed;
  if FParent <> nil then
    FParent.Changed;
end;

destructor TChildRule.Destroy;
begin
  if (FParent <> nil) and (not FParent.Destroying) then
    FParent.FList.Remove(Self);
  inherited Destroy;
end;

{ TRule }

function TRule.AddChild(ChildClass: TChildRuleClass): TChildRule;
begin
  Result := ChildClass.Create;
  Result.FParent := Self;
  FList.Add(Result);
end;

procedure TRule.Assign(Source: TPersistent);
begin
  if Source.ClassType = ClassType then
    ID := (Source as TRule).ID
  else
    inherited Assign(Source);
end;

procedure TRule.Changed;
begin
  FModified := True;
end;

procedure TRule.Clear;
begin
  Initialize;
end;

constructor TRule.Create;
begin
  inherited Create;
  RulesConnection.Connect(Self);
  FList := TList.Create;
  Initialize;
end;

function TRule.DefaultLocate(const VarTypes: TVariantTypes; const Key: Variant): Boolean;
begin
  Result := Contains(VarTypes, VarType(Key));
  if Result then
  begin
    Query.Close;
    Query.SQL.Clear;
  end;
end;

procedure TRule.Delete;
begin
  Clear;
end;

destructor TRule.Destroy;
begin
  FDestroying := True;
  RulesConnection.Disconnect(Self);
  ClearChildList(FList);
  FList.Free;
  inherited Destroy;
end;

function TRule.GetQuery: TADOQuery;
begin
  Result := RulesConnection.Query;
end;

procedure TRule.Initialize;
begin
  FModified := False;
  FID := -1;
end;

function TRule.Locate(const Key: Variant): Boolean;
begin
  Result := False;
  if Result and Assigned(FOnLocate) then
    FOnLocate(Self);
end;

procedure TRule.Revert;
begin
  if FModified then
  begin
    if (FID > -1) and Locate(FID) then
      ReadValues
    else
      Initialize;
    FModified := False;
  end;
end;

procedure TRule.Save;
var
  I: Integer;
begin
  if FModified then
  begin
    FID := WriteValues;
    FModified := False;
  end;
  for I := 0 to FList.Count - 1 do
    TRule(FList[I]).Save;
end;

procedure TRule.SetID(Value: Integer);
begin
  if Locate(Value) then
  begin
    FID := Value;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TRule.UpdateIdentity(Identity: Integer);
begin
  FID := Identity;
end;

{ TRulesConnection }

procedure TRulesConnection.BeginTrans;
begin
  if FConnection <> nil then
  begin
    if FTransRef = 0 then
      FConnection.BeginTrans;
    Inc(FTransRef);
  end;
end;

procedure TRulesConnection.CheckConnection;
begin
  if ((FKind = ckComponent) and (FConnection = nil)) or
    ((FKind = ckString) and (FConnectionString = '')) then
    raise EConnectionError.Create(SNoConnection);
end;

procedure TRulesConnection.Connect(Rule: TRule);
begin
  CheckConnection;
  Inc(FConnectionCount);
end;

constructor TRulesConnection.Create;
begin
  inherited Create;
  FQuery := TADOQuery.Create(nil);
end;

destructor TRulesConnection.Destroy;
begin
  FQuery.Free;
  inherited Destroy;
end;

procedure TRulesConnection.Disconnect(Rule: TRule);
begin
  Dec(FConnectionCount);
end;

procedure TRulesConnection.EndTrans;
begin
  if (FConnection <> nil) and (FTransRef > 0) then
  begin
    Dec(FTransRef);
    if FTransRef = 0 then
      FConnection.CommitTrans;
  end;
end;

procedure TRulesConnection.RollbackTrans;
begin
  if (FConnection <> nil) and (FTransRef > 0) then
  begin
    FConnection.RollbackTrans;
    FTransRef := 0;
  end;
end;

procedure TRulesConnection.SetConnection(Value: TADOConnection);
begin
  if Value <> FConnection then
  begin
    FConnection := Value;
    if (FKind = ckComponent) or (FConnectionString = '') then
    begin
      FKind := ckComponent;
      FQuery.Close;
      FQuery.Connection := FConnection;
    end;
  end;
end;

procedure TRulesConnection.SetConnectionString(const Value: String);
begin
  if Value <> FConnectionString then
  begin
    FConnectionString := Value;
    if (FKind = ckString) or (FConnection = nil) then
    begin
      FKind := ckString;
      FQuery.Close;
      FQuery.ConnectionString := FConnectionString;
    end;
  end;
end;

var
  InternalRulesConnection: TObject;

function RulesConnection: TRulesConnection;
begin
  if InternalRulesConnection = nil then
    InternalRulesConnection := TRulesConnection.Create;
  Result := InternalRulesConnection as TRulesConnection;
end;

{ TConnectableObject }

constructor TConnectableObject.Create(Connection: IServerConnection; const TypeLib: ITypeLib;
  const DispIntf: TGUID);
begin
  inherited Create(TypeLib, DispIntf);
  FConnection := Connection;
  if FConnection = nil then
    raise ERuleViolation.Create('No server information passed');
end;

function TConnectableObject.GetQuery: TADOQuery;
begin
  Result := FConnection.GetQuery;
end;

procedure ConnectDataset(Dataset: TCustomADODataset);
begin
  with RulesConnection do
    if Kind = ckComponent then
      Dataset.Connection := Connection
    else
      Dataset.ConnectionString := ConnectionString;
end;

type
  TStreamableDataset = class(TCustomADODataSet);

function DatasetToText(DataSet: TCustomADODataSet): string;
var
  Recordset: OleVariant;
  Stream: OleVariant;
begin
  Recordset := TStreamableDataset(Dataset).Recordset as IDispatch;
  Stream := CreateOleObject('ADODB.Stream');
  Recordset.Save(Stream, 1);
  Stream.Position := 0;
  Result := Stream.ReadText;
end;

function TextToDataset(const S: string): TCustomADODataSet;
var
  Stream: OleVariant;
  Recordset: OleVariant;
  Dispatch: IDispatch;
begin
  Stream := CreateOleObject('ADODB.Stream');
  Stream.Open;
  Stream.WriteText(S);
  Stream.Position := 0;
  RecordSet := CreateOleObject('ADODB.Recordset');
  Recordset.Open(Stream);
  Dispatch := Recordset;
  Result := TADODataSet.Create(nil);
  try
    TStreamableDataset(Result).Recordset := Dispatch as _Recordset;
  except
    Result.Free;
    raise;
  end;
end;

procedure ExecuteStatement(const Statement: string);
begin
  ExecuteStatement(Statement, []);
end;

procedure InternalPurifyStatement(var Statement: string; const Param: string;
  Cleaned: Boolean);

  procedure Strip(const S: string);
  var
    P: PChar;
    I: Integer;
  begin
    P := PChar(Statement);
    I := Pos('[' + S, Statement);
    if I > 0 then
    begin
      Inc(P, I - 1);
      if Cleaned then
        while not (P^ in [#0, ']']) do
        begin
          P^ := ' ';
          Inc(P);
        end
      else
      begin
        P^ := ' ';
        while not (P^ in [#0, ']']) do
          Inc(P);
      end;
      if P^ = ']' then P^ := ' ';
    end;
  end;

begin
  Strip(Param);
  Strip(':' + Param);
end;

procedure PurifyStatement(var Statement: string; const Param: string;
  const Value: string); overload;
begin
  InternalPurifyStatement(Statement, Param, Trim(Value) = '');
end;

procedure PurifyStatement(var Statement: string; const Param: string;
  const Value: Integer); overload;
begin
  InternalPurifyStatement(Statement, Param, Value = 0);
end;

procedure ExecuteStatement(const Statement: string;
  const Params: array of TParamItem);
const
  GO = #13#10'GO'#13#10;
var
  Query: TADOQuery;
  Parameter: TParameter;
  Start, Stop: PChar;
  Found: Boolean;
  S: string;
  I: Integer;
begin
  Query := TADOQuery.Create(nil);
  try
    RulesConnection.BeginTrans;
    try
      ConnectDataset(Query);
      S := '';
      Start := PChar(Statement);
      Stop := Start;
      repeat
        Found := SearchToken(Stop, GO);
        if Start = Stop then Break;
        if Found then
          SetString(S, Start, Integer(Stop - Start) - Length(GO))
        else
          SetString(S, Start, Integer(Stop - Start));
        Query.SQL.Text := S;
        for I := Low(Params) to High(Params) do
        begin
          Parameter := Query.Parameters.ParamByName(Params[I].Name);
          Parameter.Value := Params[I].Value;
          Parameter.DataType := Params[I].DataType;
        end;
        Query.ExecSQL;
        Start := Stop;
      until not Found;
      RulesConnection.EndTrans;
    except
      RulesConnection.RollbackTrans;
      raise;
    end;
  finally
    Query.Free;
  end;
end;

function OpenStatement(const Statement: string): TDataSet;
begin
  Result := OpenStatement(Statement, []);
end;

function OpenStatement(const Statement: string;
  const Params: array of TParamItem): TDataSet;
var
  Query: TADOQuery absolute Result;
  Parameter: TParameter;
  I: Integer;
begin
  Query := TADOQuery.Create(nil);
  try
    RulesConnection.BeginTrans;
    ConnectDataset(Query);
    Query.SQL.Text := Statement;
    for I := Low(Params) to High(Params) do
    begin
      Parameter := Query.Parameters.ParamByName(Params[I].Name);
      Parameter.Value := Params[I].Value;
      Parameter.DataType := Params[I].DataType;
    end;
    Query.Open;
    RulesConnection.EndTrans;
  except
    Query.Free;
    RulesConnection.RollbackTrans;
    raise;
  end;
end;

procedure SafeSetParam(Query: TADOQuery; const Name: string;
  const Value: Variant);
var
  Param: TParameter;
  DateValue: TDateTime;
  StringValue: string;
begin
  Param := Query.Parameters.FindParam(Name);
  if Param <> nil then
    case VarType(Value) of
      varDate:
        begin
          DateValue := Value;
          SetDateParam(Query.Parameters, Name, DateValue);
        end;
      varOleStr, varString:
        begin
          StringValue := Value;
          SetStrParam(Query.Parameters, Name, StringValue);
        end;
     else
       Param.Value := Value;
     end;
end;

procedure SetStrParam(Parameters: TParameters; const Name: string;
  const Value: string);
var
  Param: TParameter;
begin
  Param := Parameters.ParamByName(Name);
  if Value = '' then
    Param.Value := NULL
  else
    Param.Value := Value;
  Param.DataType := ftString;
end;

procedure SetDateParam(Parameters: TParameters; const Name: string;
  const Value: TDateTime);
var
  Param: TParameter;
begin
  Param := Parameters.ParamByName(Name);
  if Value = 0 then
    Param.Value := NULL
  else
    Param.Value := Value;
  Param.DataType := ftDate;
end;

procedure SetDateTimeParam(Parameters: TParameters; const Name: string;
  const Value: TDateTime);
var
  Param: TParameter;
begin
  Param := Parameters.ParamByName(Name);
  if Value = 0 then
    Param.Value := NULL
  else
    Param.Value := Value;
  Param.DataType := ftDateTime;
end;

initialization
  InternalRulesConnection := nil;
  Combine(vtIntegers, [varByte, varSmallInt, varInteger]);
  Combine(vtStrings, [varString, varOleStr]);
finalization
  InternalRulesConnection.Free;
end.
