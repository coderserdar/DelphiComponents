unit SnapRtti;


interface

uses
  Classes, SysUtils, typinfo;

resourcestring

  IncompatiblePropertyMsg = 'incompatible types: %s and %s';


type

  EPropertyValueError = Exception;

  { TPropertiesInfoList }

  TPropertiesInfoList = class(TObject)
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    FClassName: string;
    function Get(Index: Integer): PPropInfo;
    function GetInfo(const Index: string): PPropInfo;
  public
    constructor Create(AObjectClass: TClass; Filter: TTypeKinds);
    destructor Destroy; override;
    function Find(const AName: string): PPropInfo;
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get;
    property PropInfos[const Index: string]: PPropInfo read GetInfo; default;
    property EntityClassName: string read FClassName;
  end;

implementation

{ TPropertiesInfoList }

function TPropertiesInfoList.Get(Index: Integer): PPropInfo;
begin
  Result := FList^[Index];
end;

function TPropertiesInfoList.GetInfo(const Index: string): PPropInfo;
begin
  Result := Find(Index);
end;

constructor TPropertiesInfoList.Create(AObjectClass: TClass; Filter: TTypeKinds);
begin
  if AObjectClass <> nil then
  begin
    FCount := GetPropList(AObjectClass.ClassInfo, Filter, nil);
    FSize := FCount * SizeOf(Pointer);
    GetMem(FList, FSize);
    GetPropList(AObjectClass.ClassInfo, Filter, FList);
    FClassName := AObjectClass.ClassName;
  end
  else
  begin
    FCount := 0;
    FList := nil;
  end;
end;

destructor TPropertiesInfoList.Destroy;
begin
  inherited Destroy;
  if FList <> nil then
    FreeMem(FList, FSize);
end;

function TPropertiesInfoList.Find(const AName: string): PPropInfo;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    with FList^[i]^ do
    if Length(Name) = Length(AName) then
      if CompareText(Name, AName) = 0 then
      begin
        Result := FList^[i];
        Exit;
      end;
  Result := nil;
end;

end.

