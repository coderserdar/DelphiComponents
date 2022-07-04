unit jsonparser;

// ***********************************************************************
//
//   JSON Parser VCL Component
//
//   pawel.glowacki@embarcadero.com
//   July 2010
//
// ***********************************************************************


interface

uses
  Classes, DBXJSON, jsondoc;

type
  TJSONTokenKind = (
    jsNumber,
    jsString,
    jsTrue,
    jsFalse,
    jsNull,
    jsObjectStart,
    jsObjectEnd,
    jsArrayStart,
    jsArrayEnd,
    jsPairStart,
    jsPairEnd
  );

  TJSONTokenList = class
  private
    FItems: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function Add(AKind: TJSONTokenKind; AContent: string): integer;
    function GetItemKind(i: integer): TJSONTokenKind;
    function GetItemContent(i: integer): string;
  end;

  TJSONTokenEvent = procedure(
    ATokenKind: TJSONTokenKind; AContent: string) of object;

  TJSONTokenProc = reference to procedure(ATokenKind: TJSONTokenKind;
    AContent: string);

  TJSONParser = class(TJSONDocument)
  private
    FOnToken: TJSONTokenEvent;
    FTokenList: TJSONTokenList;
    procedure DoOnAddToTokenListEvent(ATokenKind: TJSONTokenKind;
      AContent: string);
    procedure DoOnFireTokenEvent(ATokenKind: TJSONTokenKind; AContent: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FireTokenEvents;
    procedure BuildTokenList;
    procedure DoProcess(val: TJSONValue; aTokenProc: TJSONTokenProc);
    property TokenList: TJSONTokenList read FTokenList;
  published
    property OnToken: TJSONTokenEvent read FOnToken write FOnToken;
  end;

implementation

type
  TJSONTokenWrapper = class
    Kind: TJSONTokenKind;
    Content: string;
    constructor Create(AKind: TJSONTokenKind; AContent: string);
  end;

{ TJSONTokenWrapper }

constructor TJSONTokenWrapper.Create(AKind: TJSONTokenKind; AContent: string);
begin
  Kind := AKind;
  Content := AContent;
end;

{ TJSONTokenList }

constructor TJSONTokenList.Create;
begin
  FItems := TList.Create;
end;

destructor TJSONTokenList.Destroy;
begin
  Clear;
  FItems.Free;
end;

function TJSONTokenList.Add(AKind: TJSONTokenKind; AContent: string): integer;
begin
  Result := FItems.Add(TJSONTokenWrapper.Create(AKind, AContent));
end;

procedure TJSONTokenList.Clear;
var i: integer; w: TJSONTokenWrapper;
begin
  for i := 0 to Count-1 do
  begin
    w := FItems[i];
    w.Free;
  end;
  FItems.Clear;
end;

function TJSONTokenList.Count: integer;
begin
  Result := FItems.Count;
end;

function TJSONTokenList.GetItemKind(i: integer): TJSONTokenKind;
var w: TJSONTokenWrapper;
begin
  w := FItems[i];
  Result := w.Kind;
end;

function TJSONTokenList.GetItemContent(i: integer): string;
var w: TJSONTokenWrapper;
begin
  w := FItems[i];
  Result := w.Content;
end;

{ TJSONParser }

constructor TJSONParser.Create(AOwner: TComponent);
begin
  inherited;
  FTokenList := TJSONTokenList.Create;
end;

destructor TJSONParser.Destroy;
begin
  FTokenList.Free;
  inherited;
end;

procedure TJSONParser.DoOnFireTokenEvent(ATokenKind: TJSONTokenKind;
  AContent: string);
begin
  if Assigned(FOnToken) then
    FOnToken(ATokenKind, AContent);
end;

procedure TJSONParser.DoOnAddToTokenListEvent(ATokenKind: TJSONTokenKind;
  AContent: string);
begin
  FTokenList.Add(ATokenKind, AContent);
end;

procedure TJSONParser.BuildTokenList;
begin
  if RootValue <> nil then
    DoProcess(RootValue, DoOnAddToTokenListEvent);
end;

procedure TJSONParser.FireTokenEvents;
begin
  if RootValue <> nil then
    DoProcess(RootValue, DoOnFireTokenEvent);
end;

procedure TJSONParser.DoProcess(val: TJSONValue; aTokenProc: TJSONTokenProc);
var i: integer;
begin
  if val is TJSONNumber then
    aTokenProc(jsNumber, TJSONNumber(val).Value)

  else if val is TJSONString then
    aTokenProc(jsString, TJSONString(val).Value)

  else if val is TJSONTrue then
    aTokenProc(jsTrue, 'true')

  else if val is TJSONFalse then
    aTokenProc(jsFalse, 'false')

  else if val is TJSONNull then
    aTokenProc(jsNull, 'null')

  else if val is TJSONArray then
  begin
    aTokenProc(jsArrayStart, '');
    with val as TJSONArray do
      for i := 0 to Size - 1 do
        DoProcess(Get(i), aTokenProc);
    aTokenProc(jsArrayEnd, '');
  end

  else if val is TJSONObject then
  begin
    aTokenProc(jsObjectStart, '');
    with val as TJSONObject do
     for i := 0 to Size - 1 do
       begin
         aTokenProc(jsPairStart, Get(i).JsonString.ToString);
         DoProcess(Get(i).JsonValue, aTokenProc);
         aTokenProc(jsPairEnd, '');
       end;
    aTokenProc(jsObjectEnd, '');
  end

  else
    raise EUnknownJsonValueDescendant.Create;
end;

end.
