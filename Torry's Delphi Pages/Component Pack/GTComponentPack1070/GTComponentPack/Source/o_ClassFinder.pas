{*****************************************}
{   GT Delphi Components                  }
{   TgtRegisteredClasses                  }
{                                         }
{   Copyright (c) GT Delphi Components    }
{   http://www.gtdelphicomponents.gr      }
{                                         }
{                                         }
{*****************************************}

unit o_ClassFinder;

interface
  uses
    Classes
    ;
type
  TgtRegisteredClasses = class(TComponent)
  private
    { Private declarations }
    FRegisteredClasses : TStrings;
    FClassFinder       : TClassFinder;
    procedure GetClassCallBack(AClass: TPersistentClass);
    function GetRegisteredClasses: TStrings;
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    function IsRegisteredClass(const AClassName : string):Boolean;
    function InheritsFrom(Instance : TComponent; AClassName :string):Boolean;overload;
    function InheritsFrom(Instance : TPersistentClass; AClassName :string):Boolean;overload;
  public
    property ClassList : TStrings  read GetRegisteredClasses;
  published
    { Published declarations}
  end;

function RegisteredClasses:TgtRegisteredClasses;

implementation
uses
  SysUtils
  ;

var
  _RegisteredClasses : TgtRegisteredClasses;

function RegisteredClasses:TgtRegisteredClasses;
begin
   if not Assigned(_RegisteredClasses) then
    _RegisteredClasses := TgtRegisteredClasses.Create(nil);
  Result := _RegisteredClasses;
end;

{ TRegisteredClasses }
{--------------------------------------------------}
constructor TgtRegisteredClasses.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRegisteredClasses := TStringList.Create;
  FClassFinder       := TClassFinder.Create();
end;
{--------------------------------------------------}
destructor TgtRegisteredClasses.Destroy;
begin
  FreeAndNil(FRegisteredClasses);
  FClassFinder.Free;
  inherited;
end;
{--------------------------------------------------}
procedure TgtRegisteredClasses.GetClassCallBack(AClass: TPersistentClass);
begin
  FRegisteredClasses.AddObject(AClass.ClassName,TObject(AClass));
end;
{--------------------------------------------------}
function TgtRegisteredClasses.GetRegisteredClasses: TStrings;
begin
  FRegisteredClasses.Clear;
  FClassFinder.GetClasses(GetClassCallBack);
  Result := FRegisteredClasses;
end;
{--------------------------------------------------}
function TgtRegisteredClasses.IsRegisteredClass(const AClassName: string): Boolean;
begin
  Result := ClassList.IndexOf(AClassName) <> -1;
end;
{--------------------------------------------------}
function TgtRegisteredClasses.InheritsFrom(Instance: TPersistentClass;AClassName: string): Boolean;
var
  AClass : TClass;
begin
  Result := False;
  if Assigned(Instance) then
  begin
    AClass := Instance.ClassParent;
    while AClass <> nil do
    begin
      if SameText(AClass.ClassName,AClassName) then
      begin
        Result := True;
        Break;
      end;
      AClass := AClass.ClassParent;
    end;
  end;
end;
{--------------------------------------------------}
function TgtRegisteredClasses.InheritsFrom(Instance : TComponent; AClassName :string):Boolean;
var
  AClass : TClass;
begin
  Result := False;
  if Assigned(Instance) then
  begin
    AClass := Instance.ClassParent;
    while AClass <> nil do
    begin
      if SameText(AClass.ClassName,AClassName) then
      begin
        Result := True;
        Break;
      end;
      AClass := AClass.ClassParent;
    end;
  end;
end;
{--------------------------------------------------}

initialization

finalization
  if Assigned(_RegisteredClasses) then
    FreeAndNil(_RegisteredClasses);

end.

