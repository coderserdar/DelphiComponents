{$I fb_define.inc}
unit fbparams;

interface
uses Classes, SysUtils;

type
  TFBParam = class(TCollectionItem)
  private
    FValue: string;
    FName: string;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignParam(Param: TFBParam);
  published
    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
  end;

  TFBParams = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TFBParam;
    function GetParamValue(const ParamName: string): string;
    procedure SetItem(Index: Integer; const Value: TFBParam);
    procedure SetParamValue(const ParamName, AValue: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TPersistent); overload;
    procedure AssignValues(AValue: TFBParams);
    function ParamByName(const AValue: string): TFBParam;
    function FindParam(const AValue: string): TFBParam;
    function CreateParam(const ParamName: string): TFBParam;
    property Items[Index: Integer]: TFBParam read GetItem write SetItem; default;
    property ParamValues[const ParamName: string]: string read GetParamValue write SetParamValue;
    procedure GetParamList(List: TList; const ParamNames: string);
  end;

implementation
uses fbmisc
{$IFNDEF FPC}
  ,DB
{$ENDIF};
{ TFBParam }

procedure TFBParam.Assign(Source: TPersistent);
begin
  if (Source is TFBParam) then AssignParam(TFBParam(Source))
  else
    inherited Assign(Source);
end;

procedure TFBParam.AssignParam(Param: TFBParam);
begin
  FValue:=Param.FValue;
  FName:=Param.FName;
end;

constructor TFBParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FValue:='';
  FName:='';
end;

function TFBParam.GetDisplayName: string;
begin
  if FName='' then Result:=inherited GetDisplayName
  else
    Result:=FName + ' -> '+FValue;
end;

{ TFBParams }

procedure TFBParams.AssignTo(Dest: TPersistent);
begin
  if Dest is TFBParams then TFBParams(Dest).Assign(Self)
  else inherited AssignTo(Dest);
end;

procedure TFBParams.AssignValues(AValue: TFBParams);
var
  I: Integer;
  P: TFBParam;
begin
  for I := 0 to AValue.Count - 1 do
  begin
    P := FindParam(AValue[I].Name);
    if P <> nil then
      P.Assign(AValue[I]);
  end;
end;

constructor TFBParams.Create(Owner: TPersistent);
begin
  FOwner := Owner;
  inherited Create(TFBParam);
end;

function TFBParams.CreateParam(const ParamName: string): TFBParam;
begin
  Result := Add as TFBParam;
  Result.Name := ParamName;
end;

function TFBParams.FindParam(const AValue: string): TFBParam;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TFBParam(inherited Items[I]);
    if AnsiCompareText(Result.Name, AValue) = 0 then Exit;
  end;
  Result := nil;
end;

function TFBParams.GetItem(Index: Integer): TFBParam;
begin
  Result := TFBParam(inherited Items[Index]);
end;

procedure TFBParams.GetParamList(List: TList; const ParamNames: string);
var
  Pos: Integer;
begin
  Pos := 1;
  while Pos <= Length(ParamNames) do
    List.Add(ParamByName(ExtractFieldName(ParamNames, Pos)));
end;

function TFBParams.GetParamValue(const ParamName: string): string;
begin
  Result := ParamByName(ParamName).Value
end;

function TFBParams.ParamByName(const AValue: string): TFBParam;
begin
  Result := FindParam(AValue);
  if Result = nil then
    FBError(fbeParameterNotFound, [AValue]);
end;

procedure TFBParams.SetItem(Index: Integer; const Value: TFBParam);
begin
  inherited SetItem(Index, TCollectionItem(Value));
end;

procedure TFBParams.SetParamValue(const ParamName, AValue: string);
begin
  ParamByName(ParamName).Value := AValue;
end;

end.
