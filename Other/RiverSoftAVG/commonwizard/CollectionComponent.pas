unit CollectionComponent;

interface

uses
  Windows, Messages, SysUtils, Classes;

type
  TItemDescendant = TCollectionItem;
  TITEMCOLLECTIONDESCENDANT = TOwnedCollection;
  TPropertyType = String;
  TITEMCOLLECTIONOWNER = TComponent;
  TITEMNAME = class(TITEMDESCENDANT)
  { Purpose: }
  private
    { Private declarations }
    FITEMPROPERTY: TPROPERTYTYPE;
    procedure SetITEMPROPERTY(const Value: TPROPERTYTYPE);
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure Assign(Source: TPersistent); override;
  published
    { Published declarations }
    property ITEMPROPERTY: TPROPERTYTYPE read FITEMPROPERTY write SetITEMPROPERTY;
  end; { TITEMNAME }

  TITEMCOLLECTION = class(TITEMCOLLECTIONDESCENDANT)
  { Purpose: }
  private
    { Private declarations }
    function GetItem(Index: Integer): TITEMNAME;
    procedure SetItem(Index: Integer; const Value: TITEMNAME);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TITEMCOLLECTIONOWNER);
    function Add: TITEMNAME;
    function FindItemID(ID: Integer): TITEMNAME;
    function Insert(Index: Integer): TITEMNAME;
    property Items[Index: Integer]: TITEMNAME read GetItem write SetItem; default;
    function Owner: TITEMCOLLECTIONOWNER; reintroduce;
  published
    { Published declarations }
  end; { TITEMCOLLECTION }

  TCollectionComponent = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Samples', [TCollectionComponent]);
end;

{ TITEMNAME }

procedure TITEMNAME.Assign(Source: TPersistent);
begin
     if Source is TITEMNAME then
     begin
          FITEMPROPERTY := TITEMNAME(Source).ITEMPROPERTY;
          Changed(False);
     end
     else
         inherited Assign(Source);
end;

procedure TITEMNAME.SetITEMPROPERTY(const Value: TPROPERTYTYPE);
begin
     if Value <> ITEMPROPERTY then
     begin
          FITEMPROPERTY := Value;
          Changed(False);
     end;
end;

{ TITEMCOLLECTION }

function TITEMCOLLECTION.Add: TITEMNAME;
begin
     result := TITEMNAME(inherited Add);
end;

function TITEMCOLLECTION.FindItemID(ID: Integer): TITEMNAME;
begin
     result := TITEMNAME(inherited FindItemID(ID));
end;

function TITEMCOLLECTION.GetItem(Index: Integer): TITEMNAME;
begin
     result := TITEMNAME(inherited Items[Index]);
end;

function TITEMCOLLECTION.Owner: TITEMCOLLECTIONOWNER;
var
   AOwner: TPersistent;
begin
     AOwner := inherited Owner;
     if AOwner is TITEMCOLLECTIONOWNER then
        result := TITEMCOLLECTIONOWNER(AOwner)
     else
         result := nil;
end;

function TITEMCOLLECTION.Insert(Index: Integer): TITEMNAME;
begin
     result := TITEMNAME(inherited Insert(Index));
end;

procedure TITEMCOLLECTION.SetItem(Index: Integer; const Value: TITEMNAME);
begin
     inherited Items[Index] := Value;
end;

constructor TITEMCOLLECTION.Create(AOwner: TITEMCOLLECTIONOWNER);
begin
     inherited Create(AOwner, TITEMNAME);
end;

end.
 