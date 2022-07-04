{*******************************************************}
{                                                       }
{       Thinx S.A                                       }
{       THWindowsCPL                                    }
{                                                       }
{       Copyright (c) Thinx S.A                         }
{                                                       }
{                                                       }
{       Creation Date : 2007/11/13                      }
{                                                       }
{*******************************************************}

unit o_GTWindowsCPL;

interface

uses
   SysUtils
  ,Classes
  ;

type
{------------------------------------------------------------------------------}
  TgtWindowsCPL = class;
{------------------------------------------------------------------------------}
  TgtCPLItem=class(TCollectionItem)
  private
    FFileName   : TFilename;
    FCPLName    : String;
    procedure SetCPLName(Value:String);
  published
    property FileName   :TFileName read FFileName    write FFileName;
    property CPLName    :String    read FCPLName     write SetCPLName;
  end;
{------------------------------------------------------------------------------}
  TgtCPLItemCollection=class(TCollection)
  private
    FWindowsCPL : TgtWindowsCPL;
    function  GetItem(Index:Integer):TgtCPLItem;
    procedure SetItem(Index: Integer; const Value: TgtCPLItem);
  protected
    function GetOwner:TPersistent; override;
  public
    constructor Create(AOwner:TgtWindowsCPL);
    function Add:TgtCPLItem;
    function IndexOf(Name:String):Integer;
    property Items[Index:Integer]:TgtCPLItem read GetItem write SetItem; default;
  end;
{------------------------------------------------------------------------------}
  TgtWindowsCPL = class(TComponent)
  private
    { Private declarations }
    FItems : TgtCPLItemCollection;
    procedure SetItems(Value:TgtCPLItemCollection);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor  Destroy;override;
    procedure   OpenByIndex (ItemIndex:Integer);
    procedure   OpenByName  (ItemName:String);
  published
    { Published declarations }
    property Items:TgtCPLItemCollection read FItems write SetItems;
  end;
{------------------------------------------------------------------------------}


implementation
uses
   ShellApi
  ,Windows;



{ TgtCPLItem }
{------------------------------------------------------------------------------}
procedure TgtCPLItem.SetCPLName(Value: String);
begin
  FCPLName := Value;
end;
{------------------------------------------------------------------------------}

{ TgtCPLItemCollection }
{------------------------------------------------------------------------------}
function TgtCPLItemCollection.Add: TgtCPLItem;
begin
  try
    Result :=  TgtCPLItem(inherited Add);
  except
    on E:Exception do
      raise Exception.Create(E.Message);
  end;
end;
{------------------------------------------------------------------------------}
constructor TgtCPLItemCollection.Create(AOwner: TgtWindowsCPL);
begin
  inherited Create(TgtCPLItem);
  FWindowsCPL := AOwner;
end;
{------------------------------------------------------------------------------}
function TgtCPLItemCollection.GetItem(Index: Integer): TgtCPLItem;
begin
  Result:=TgtCPLItem(inherited GetItem(Index));
end;
{------------------------------------------------------------------------------}
function TgtCPLItemCollection.GetOwner: TPersistent;
begin
  Result := FWindowsCPL;
end;
{------------------------------------------------------------------------------}
function TgtCPLItemCollection.IndexOf(Name: String): Integer;
var i:integer;
begin
  Result := -1;
  for i  := 0 to Pred(Count) do
    begin
      if Items[i].CPLName = Name then
        begin
          Result:=i;
          Break;
        end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TgtCPLItemCollection.SetItem(Index: Integer;const Value: TgtCPLItem);
begin
  inherited SetItem(Index,Value);
end;
{------------------------------------------------------------------------------}

{ TgtWindowsCPL }
{------------------------------------------------------------------------------}
constructor TgtWindowsCPL.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TgtCPLItemCollection.Create(Self);
end;
{------------------------------------------------------------------------------}
destructor TgtWindowsCPL.Destroy;
begin
  FItems.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtWindowsCPL.OpenByIndex(ItemIndex: Integer);
const
  WinDir ='C:\Windows\System32\';
begin
  if ItemIndex<>-1 then
    ShellExecute(HWND(nil),'open',WinDir+'control.exe',PChar(Items[ItemIndex].FileName),'',0);
end;
{------------------------------------------------------------------------------}
procedure TgtWindowsCPL.OpenByName(ItemName: String);
begin
  OpenByIndex(Items.IndexOf(LowerCase(ItemName)));
end;
{------------------------------------------------------------------------------}
procedure TgtWindowsCPL.SetItems(Value: TgtCPLItemCollection);
begin
  FItems.Assign(Value);
end;
{------------------------------------------------------------------------------}


end.
 