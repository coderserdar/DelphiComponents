unit unTransObj;

interface
uses Classes;

{
  These classes let us represent which component's strings need
  to be translated, and which Id they're associated with.
}

type
  TXXProperty = class
  public
    Name : String;
    Id : Integer;
  end;

  TXXComponent = class
  protected
    FList : TList;

    function GetItemCount : Integer;
    function GetItems( idx : Integer ) : TXXProperty;

  public
    Name : String;

    constructor Create;
    destructor Destroy; override;

    procedure Add( item : TXXProperty );
    function Find( const name : String ) : TXXProperty;

    property ItemCount : Integer read GetItemCount;
    property Items[ idx : Integer ] : TXXProperty read GetItems;
  end;

  TXXForm = class
  protected
    FList : TList;

    function GetItemCount : Integer;
    function GetItems( idx : Integer ) : TXXComponent;

  public
    Name : String;
    This : TXXComponent;

    constructor Create;
    destructor Destroy; override;

    procedure Add( item : TXXComponent );
    function Find( const name : String ) : TXXComponent;

    property ItemCount : Integer read GetItemCount;
    property Items[ idx : Integer ] : TXXComponent read GetItems;
  end;

  TXXForms = class
  protected
    FList : TList;

    function GetItemCount : Integer;
    function GetItems( idx : Integer ) : TXXForm;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add( item : TXXForm );
    function Find( const name : String ) : TXXForm;

    property ItemCount : Integer read GetItemCount;
    property Items[ idx : Integer ] : TXXForm read GetItems;
  end;

implementation

//---------------  TXXComponent  -----------------

function TXXComponent.GetItemCount : Integer;
begin
  Result := FList.Count;
end;

function TXXComponent.GetItems( idx : Integer ) : TXXProperty;
begin
  Result := TXXProperty( FList.Items[idx] );
end;

constructor TXXComponent.Create;
begin
  FList := TList.Create;
end;

destructor TXXComponent.Destroy;
var
  i : Integer;
begin
  for i := 0 to ItemCount - 1 do
    Items[i].Free;
  FList.Free;
  inherited;
end;

procedure TXXComponent.Add( item : TXXProperty );
begin
  FList.Add( item );
end;

function TXXComponent.Find( const name : String ) : TXXProperty;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to ItemCount - 1 do
    if Items[i].Name = name then
      begin
        Result := Items[i];
        Exit;
      end;
end;

//---------------  TXXForm  ----------------------

function TXXForm.GetItemCount : Integer;
begin
  Result := FList.Count;
end;

function TXXForm.GetItems( idx : Integer ) : TXXComponent;
begin
  Result := TXXComponent( FList.Items[idx] );
end;

constructor TXXForm.Create;
begin
  FList := TList.Create;
end;

destructor TXXForm.Destroy;
var
  i : Integer;
begin
  for i := 0 to ItemCount - 1 do
    Items[i].Free;
  FList.Free;
  inherited;
end;

procedure TXXForm.Add( item : TXXComponent );
begin
  FList.Add( item );
end;

function TXXForm.Find( const name : String ) : TXXComponent;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to ItemCount - 1 do
    if Items[i].Name = name then
      begin
        Result := Items[i];
        Exit;
      end;
end;

//--------------  TXXForms  ----------------------

function TXXForms.GetItemCount : Integer;
begin
  Result := FList.Count;
end;

function TXXForms.GetItems( idx : Integer ) : TXXForm;
begin
  Result := TXXForm( FList.Items[idx] );
end;

constructor TXXForms.Create;
begin
  FList := TList.Create;
end;

destructor TXXForms.Destroy;
var
  i : Integer;
begin
  for i := 0 to ItemCount - 1 do
    Items[i].Free;
  FList.Free;
  inherited;
end;

procedure TXXForms.Add( item : TXXForm );
begin
  FList.Add( item );
end;

function TXXForms.Find( const name : String ) : TXXForm;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to ItemCount - 1 do
    if Items[i].Name = name then
      begin
        Result := Items[i];
        Exit;
      end;
end;

end.
