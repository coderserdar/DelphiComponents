{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       StringStore                     }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_StringStore;
interface
uses
   Classes
  ,Contnrs
  ;

type
{------------------------------------------------------------------------------}
  TgtStoreStringList = class(TStringList)
  private
    FGroupID: Integer;
    FName   : string;
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Name    : string  read FName    write FName;
    property GroupID : Integer read FGroupID write FGroupID;
  end;
{------------------------------------------------------------------------------}
  TgtStringStore = class(TComponent)
  private
    { Private declarations }
    FStringObjectList : TObjectList;
    function GetStringStoreList(Index: Integer): TgtStoreStringList;
    function GetStringStoreListByName(Name: string): TgtStoreStringList;
    function GetItemsCount: Integer;
  protected
    { Protected declarations }
    function GetStringStoreListIndex(Name : string):Integer;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent);override;
    destructor  Destroy;override;
  public
    procedure AddNew(Name  : string;GroupId:Integer = -1);overload;
    procedure AddNew(StoreStringList : TgtStoreStringList);overload;
    procedure Delete(Index : Integer);overload;
    procedure Delete(Name  : string);overload;
    procedure Clear;
  public
    property Items     [Index: Integer]: TgtStoreStringList read GetStringStoreList;default;
    property ItemByName[Name : string] : TgtStoreStringList read GetStringStoreListByName;
    property Count                     : Integer            read GetItemsCount;
  published
    { Published declarations}
  end;
{------------------------------------------------------------------------------}

implementation
uses
  SysUtils
  ;

{ TSgttringStore }

{------------------------------------------------------------------------------}
constructor TgtStringStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStringObjectList := TObjectList.Create;
  FStringObjectList.OwnsObjects := True;
end;
{------------------------------------------------------------------------------}
destructor TgtStringStore.Destroy;
begin
  FStringObjectList.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
function TgtStringStore.GetStringStoreListIndex(Name: string): Integer;
var
  i : integer;
begin
  Result := -1;
  for i:= 0 to Pred(FStringObjectList.Count) do
  begin
    if SameText(Items[i].Name,Name) then
    begin
      Result := i;
      Break;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtStringStore.AddNew(Name: string;GroupId:Integer = -1);
var
  STList : TgtStoreStringList;
begin
  STList         := TgtStoreStringList.Create;
  STList.Name    := Name;
  STList.GroupID := GroupId;
  FStringObjectList.Add(STList);
end;
{------------------------------------------------------------------------------}
procedure TgtStringStore.AddNew(StoreStringList: TgtStoreStringList);
begin
  if Assigned(StoreStringList) then
  begin
   FStringObjectList.Add(StoreStringList);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtStringStore.Clear;
begin
  FStringObjectList.Clear;
end;
{------------------------------------------------------------------------------}
procedure TgtStringStore.Delete(Index: Integer);
begin
  FStringObjectList.Delete(Index);
end;
{------------------------------------------------------------------------------}
procedure TgtStringStore.Delete(Name: string);
begin
  FStringObjectList.Delete(GetStringStoreListIndex(Name));
end;
{------------------------------------------------------------------------------}
function TgtStringStore.GetStringStoreList(Index: Integer): TgtStoreStringList;
begin
  Result := TgtStoreStringList(FStringObjectList.Items[Index]);
end;
{------------------------------------------------------------------------------}
function TgtStringStore.GetStringStoreListByName(Name: string): TgtStoreStringList;
begin
  Result := TgtStoreStringList(FStringObjectList.Items[GetStringStoreListIndex(Name)]);
end;
{------------------------------------------------------------------------------}
function TgtStringStore.GetItemsCount: Integer;
begin
  Result := FStringObjectList.Count;
end;
{------------------------------------------------------------------------------}

end.

