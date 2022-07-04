unit CHStringList;

{ ##############################################################################
  TCHStringList

  Version   		:   1.0.0
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 07.10.2005    - First Release



  ############################################################################ }

interface

uses
  Forms, SysUtils, Classes, extctrls;


type
  TOnAdd = procedure(Sender: TObject; S: String) of object;
  TOnDelete = procedure(Sender: TObject; Index: Integer) of object;
  TOnInsert = procedure(Sender: TObject; Index: Integer; S: String) of object;


  TCHStringList = class(TComponent)
  private
    FOnDelete: TOnDelete;
    FOnClear: TNotifyEvent;
    FOnAdd: TOnAdd;
    FOnExchange: TNotifyEvent;
    FOnSort: TNotifyEvent;
    FOnInsert: TOnInsert;
  public
    FStrings : TStringList;
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    function Add(const S: string): Integer;
    function AddObject(const S: string; AObject: TObject): Integer;
    procedure Clear;
    procedure CustomSort(Compare: TStringListSortCompare);
    procedure Delete(Index: Integer);
    procedure Exchange(Index1: Integer; Index2: Integer);
    function Find(const S: string; var Index: Integer): Boolean;
    function IndexOf(const S: string): Integer;
    procedure Insert(Index: Integer; const S: string);
    procedure InsertObject(Index: Integer; const S: string; AObject: TObject);
    procedure Sort;
  published
    property OnAdd : TOnAdd read FOnAdd write FOnAdd;
    property OnDelete : TOnDelete read FOnDelete write FOnDelete;
    property OnClear : TNotifyEvent read FOnClear write FOnClear;
    property OnSort : TNotifyEvent read FOnSort write FOnSort;
    property OnExchange : TNotifyEvent read FOnExchange write FOnExchange;
    property OnInsert : TOnInsert read FOnInsert write FOnInsert;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHStringList]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHStringList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStrings := TStringList.Create;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHStringList.Destroy;
begin
  FStrings.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringList.Add(const S: string): Integer;
begin
  Result := FStrings.Add(S);
  if Assigned(FOnAdd) then
    FOnAdd(Self, S);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringList.AddObject(const S: string; AObject: TObject): Integer;
begin
  Result := FStrings.AddObject(S, AObject);
  if Assigned(FOnAdd) then
    FOnAdd(Self, S);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringList.Clear;
begin
  FStrings.Clear;
  if Assigned(FOnClear) then
    FOnClear(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringList.CustomSort(Compare: TStringListSortCompare);
begin
  FStrings.CustomSort(Compare);
  if Assigned(FOnSort) then
    FOnSort(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringList.Delete(Index: Integer);
begin
  FStrings.Delete(Index);
  if Assigned(FOnDelete) then
    FOnDelete(Self, Index);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringList.Exchange(Index1, Index2: Integer);
begin
  FStrings.Exchange(Index1, Index2);
  if Assigned(FOnExchange) then
    FOnExchange(Self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringList.Find(const S: string; var Index: Integer): Boolean;
begin
  Result := FStrings.Find(S, Index);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHStringList.IndexOf(const S: string): Integer;
begin
  Result := FStrings.IndexOf(S);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringList.Insert(Index: Integer; const S: string);
begin
  FStrings.Insert(Index, S);
  if Assigned(FOnInsert) then
    FOnInsert(Self, Index, S);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringList.InsertObject(Index: Integer; const S: string; AObject: TObject);
begin
  FStrings.InsertObject(Index, S, AObject);
  if Assigned(FOnInsert) then
    FOnInsert(Self, Index, S);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHStringList.Sort;
begin
  FStrings.Sort;
  if Assigned(FOnSort) then
    FOnSort(Self);
end;

end.
