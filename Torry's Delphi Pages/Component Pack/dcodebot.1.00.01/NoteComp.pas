unit NoteComp;

interface

uses
  SysUtils, Classes;

type
  TNoteItem = class(TCollectionItem)
  private
    FDisplayName: string;
    FNote: TStrings;
    procedure SetNote(Value: TStrings);
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property DisplayName;
    property Note: TStrings read FNote write SetNote;
  end;

  TNoteItems = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TNoteItem;
    procedure SetItem(Index: Integer; Value: TNoteItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TNoteItem;
    procedure Assign(Source: TPersistent); override;
    function Find(const DisplayName: string): TNoteItem;
    property Items[Index: Integer]: TNoteItem read GetItem write SetItem; default;
  end;

  TNoteContainer = class(TComponent)
  private
    FNotes: TNoteItems;
    procedure SetNotes(Value: TNoteItems);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Find(const DisplayName: string): TStrings;
  published
    property Notes: TNoteItems read FNotes write SetNotes;
  end;

implementation

constructor TNoteItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FNote := TStringList.Create;
end;

destructor TNoteItem.Destroy;
begin
  FNote.Free;
  inherited Destroy;
end;

procedure TNoteItem.Assign(Source: TPersistent);
var
  NoteItem: TNoteItem absolute Source;
begin
  if Source is TNoteItem then
  begin
    FNote.Assign(NoteItem.Note);
    FDisplayName := NoteItem.DisplayName;
  end
  else
    inherited Assign(Source);
end;

function TNoteItem.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

procedure TNoteItem.SetDisplayName(const Value: string);
begin
  FDisplayName := Value;
end;

procedure TNoteItem.SetNote(Value: TStrings);
begin
  FNote.Assign(Value);
end;

constructor TNoteItems.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TNoteItem);
end;

function TNoteItems.Add: TNoteItem;
begin
  Result:= TNoteItem(inherited Add);
end;

procedure TNoteItems.Assign(Source: TPersistent);
var
  NoteItems: TNoteItems absolute Source;
  I: Integer;
begin
  if Source is TNoteItems then
  begin
    Clear;
    for I := 0 to NoteItems.Count - 1 do
      Add.Assign(NoteItems[I]);
  end
  else
    inherited Assign(Source);
end;

function TNoteItems.Find(const DisplayName: string): TNoteItem;
var
  S: string;
  I: Integer;
begin
  Result := nil;
  S := UpperCase(DisplayName);
  for I := 0 to Count - 1 do
    if UpperCase(Items[I].DisplayName) = S then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TNoteItems.GetItem(Index: Integer): TNoteItem;
begin
  Result := TNoteItem(inherited Items[Index]);
end;

procedure TNoteItems.SetItem(Index: Integer; Value: TNoteItem);
begin
  GetItem(Index).Assign(Value);
end;

constructor TNoteContainer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNotes := TNoteItems.Create(Self);
end;

destructor TNoteContainer.Destroy;
begin
  FNotes.Free;
  inherited Destroy;
end;

function TNoteContainer.Find(const DisplayName: string): TStrings;
var
  NoteItem: TNoteItem;
begin
  NoteItem := FNotes.Find(DisplayName);
  if NoteItem <> nil then Result := NoteItem.Note else Result := nil;
end;

procedure TNoteContainer.SetNotes(Value: TNoteItems);
begin
  FNotes.Assign(Value);
end;

end.
 