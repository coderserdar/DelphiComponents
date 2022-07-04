unit jsondoc;

// ***********************************************************************
//
//   JSON Document VCL Component
//
//   pawel.glowacki@embarcadero.com
//   July 2010
//
// ***********************************************************************

interface

uses
  Classes, SysUtils, DBXJSON;

type
  EJSONDocument = class(Exception);

  EUnknownJsonValueDescendant = class(EJSONDocument)
    constructor Create;
  end;

  TJSONDocument = class(TComponent)
  private
    FRootValue: TJSONValue;
    FJsonText: string;
    FOnChange: TNotifyEvent;
    procedure SetJsonText(const Value: string);
    procedure SetRootValue(const Value: TJSONValue);
  protected
    procedure FreeRootValue;
    procedure DoOnChange; virtual;
  public
    class function IsSimpleJsonValue(v: TJSONValue): boolean; inline;
    class function UnQuote(s: string): string; inline;
    class function StripNonJson(s: string): string; inline;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ProcessJsonText: boolean;
    function IsActive: boolean;
    function EstimatedByteSize: integer;
    property RootValue: TJSONValue read FRootValue write SetRootValue;
  published
    property JsonText: string read FJsonText write SetJsonText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

uses
  Character;

{ TJSONDocument }

constructor TJSONDocument.Create(AOwner: TComponent);
begin
  inherited;
  FRootValue := nil;
  FJsonText := '';
end;

destructor TJSONDocument.Destroy;
begin
  FreeRootValue;
  inherited;
end;

procedure TJSONDocument.FreeRootValue;
begin
  if Assigned(FRootValue) then
    FreeAndNil(FRootValue);
end;

procedure TJSONDocument.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

function TJSONDocument.EstimatedByteSize: integer;
begin
  if IsActive then
    Result := FRootValue.EstimatedByteSize
  else
    Result := 0;
end;

function TJSONDocument.IsActive: boolean;
begin
  Result := RootValue <> nil;
end;

function TJSONDocument.ProcessJsonText: boolean;
var s: string;
begin
  FreeRootValue;
  s := StripNonJson(JsonText);
  FRootValue := TJSONObject.ParseJSONValue(BytesOf(s),0);
  Result := IsActive;
  DoOnChange;
end;

procedure TJSONDocument.SetJsonText(const Value: string);
begin
  if FJsonText <> Value then
  begin
    FreeRootValue;
    FJsonText := Value;
    if FJsonText <> '' then
      ProcessJsonText
  end;
end;

procedure TJSONDocument.SetRootValue(const Value: TJSONValue);
begin
  if FRootValue <> Value then
  begin
    if FRootValue <> nil then
      FreeRootValue;

    FRootValue := Value;

    if FRootValue <> nil then
      FJsonText := FRootValue.ToString;

    DoOnChange;
  end;
end;

class function TJSONDocument.StripNonJson(s: string): string;
var ch: char; inString: boolean;
begin
  Result := '';
  inString := false;
  for ch in s do
  begin
    if ch = '"' then
      inString := not inString;

    if TCharacter.IsWhiteSpace(ch) and not inString then
      continue;

    Result := Result + ch;
  end;
end;

class function TJSONDocument.UnQuote(s: string): string;
begin
  Result := Copy(s,2,Length(s)-2);
end;

class function TJSONDocument.IsSimpleJsonValue(v: TJSONValue): boolean;
begin
  Result := (v is TJSONNumber)
    or (v is TJSONString)
    or (v is TJSONTrue)
    or (v is TJSONFalse)
    or (v is TJSONNull);
end;

{ EUnknownJsonValueDescendant }

resourcestring
  StrUnknownTJSONValueDescendant = 'Unknown TJSONValue descendant';

constructor EUnknownJsonValueDescendant.Create;
begin
  inherited Create(StrUnknownTJSONValueDescendant);
end;

end.
