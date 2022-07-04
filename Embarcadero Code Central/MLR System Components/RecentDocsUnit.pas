unit RecentDocsUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus;

type
  TItemClickEvent = procedure (Sender: TObject; Item: TMenuItem) of object;
  TMLRRecentDocs = class(TComponent)
  private
    FAutoRebuild: Boolean;
    FOnItemClick: TItemClickEvent;
    FSeparator: TMenuItem;
    FHint: string;
    FShowShort: Boolean;
    FWidth: Integer;
    FAddToShell: Boolean;
    FLimit: Integer;
    procedure SetSeparator(const Value: TMenuItem);
    { Private declarations }
  protected
    { Protected declarations }
    FRecentDocs: TStrings;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnClick(Sender: TObject);
    procedure OnDrawItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);
    procedure OnMeasureItem(Sender: TObject; ACanvas: TCanvas; var Width, Height: Integer);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  TextForItem(Item: TMenuItem): string;
    procedure Add(const FileName: string);
    procedure Remove(const FileName: string);
    procedure Build;
    procedure Clear;
    procedure Rebuild;
    procedure ClearSystemDocs;
    property RecentDocs: TStrings read FRecentDocs;
  published
    { Published declarations }
    property AutoRebuild: Boolean read FAutoRebuild write FAutoRebuild default True;
    property AddToShell: Boolean read FAddToShell write FAddToShell default True;
    property Limit: Integer read FLimit write FLimit default 0;
    property ShowShort: Boolean read FShowShort write FShowShort default True;
    property Separator: TMenuItem read FSeparator write SetSeparator;
    property OnItemClick: TItemClickEvent read FOnItemClick write FOnItemClick;
    property Hint: string read FHint write FHint;
    property Width: Integer read FWidth write FWidth default 144;
  end;

procedure Register;

implementation

uses
  ShlObj, ShellAPI;

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TMLRRecentDocs]);
end;

{ TMLRRecentDocs }

procedure TMLRRecentDocs.Add(const FileName: string);
var
  i: Integer;
begin
  i := FRecentDocs.IndexOf(FileName);
  if i = -1 then
    FRecentDocs.Insert(0, FileName)
  else
    FRecentDocs.Exchange(0, i);
  if FAutoRebuild then Rebuild;
  if FAddToShell then
    SHAddToRecentDocs(SHARD_PATH, PChar(FileName));
end;

procedure TMLRRecentDocs.Build;
var
  i: Integer;
  Item: TMenuItem;
  StartIndex: Integer;
  ParentItem: TMenuItem;
begin
  if FRecentDocs.Count = 0 then
    FSeparator.Visible := False
  else begin
    FSeparator.Visible := True;
    ParentItem := Separator.Parent;
    StartIndex := ParentItem.IndexOf(FSeparator) + 1;
    for i := 0 to FRecentDocs.Count - 1 do begin
      Item := TMenuItem.Create(FSeparator);
      Item.Caption := '&' + IntToHex(i + 1, 1) + '. ' + FRecentDocs[i];
      Item.Hint := FHint;
      if FShowShort then begin
        Item.OnDrawItem := OnDrawItem;
        Item.OnMeasureItem := OnMeasureItem;
      end;
      Item.OnClick := OnClick;
      ParentItem.Insert(StartIndex, Item);
      Inc(StartIndex);
    end;
  end;
end;

procedure TMLRRecentDocs.Clear;
var
  i: Integer;
  ParentItem: TMenuItem;
  Item: TMenuItem;
begin
  ParentItem := FSeparator.Parent;
  i := ParentItem.IndexOf(FSeparator) + 1;
  while ParentItem.Items[i].Caption <> '-' do begin
    Item := ParentItem.Items[i];
    ParentItem.Remove(Item);
    Item.Free;
  end;
end;

procedure TMLRRecentDocs.ClearSystemDocs;
begin
  SHAddToRecentDocs(SHARD_PIDL, nil);
end;

constructor TMLRRecentDocs.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRecentDocs := TStringList.Create;
  FAutoRebuild := True;
  FShowShort := True;
  FWidth := 144;
  FAddToShell := True;
end;

destructor TMLRRecentDocs.Destroy;
begin
  FRecentDocs.Free;
  inherited Destroy;
end;

procedure TMLRRecentDocs.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation <> opRemove then exit;
  if AComponent = FSeparator then FSeparator := nil;
end;

procedure TMLRRecentDocs.OnClick(Sender: TObject);
begin
  if Assigned(FOnItemClick) then
    FOnItemClick(Self, Sender as TMenuItem);
end;

procedure TMLRRecentDocs.OnDrawItem(Sender: TObject; ACanvas: TCanvas;
  ARect: TRect; Selected: Boolean);
var
  X, Y: Integer;
  Item: TMenuItem;
  Menu: TMenu;
  NewCaption: string;
begin
  ACanvas.Pen.Style := psClear;
  ACanvas.Rectangle(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  X := ARect.Left + 2;
  Y := ARect.Top + 2;
  Item := Sender as TMenuItem;
  Menu := Item.GetParentMenu;
  NewCaption := Item.Caption;
  UniqueString(NewCaption);
  if Assigned(Menu.Images) then
    Inc(X, Menu.Images.Width + 2 + 2);
  ARect.Left := X;
  Dec(ARect.Right, 2);
  DrawText(ACanvas.Handle, PChar(NewCaption), Length(NewCaption), ARect,
    DT_CALCRECT or DT_PATH_ELLIPSIS or DT_MODIFYSTRING);
  ARect := Rect(X, Y, 2000, 2000);
  DrawText(ACanvas.Handle, PChar(NewCaption), StrLen(PChar(NewCaption)),
    ARect, DT_LEFT or DT_NOCLIP or DT_SINGLELINE);
end;

procedure TMLRRecentDocs.OnMeasureItem(Sender: TObject; ACanvas: TCanvas;
  var Width, Height: Integer);
var
  Item: TMenuItem;
  Menu: TMenu;
begin
  Item := Sender as TMenuItem;
  Menu := Item.GetParentMenu;
  Width := FWidth + 2;
  if Assigned(Menu.Images) then
    Inc(Width, Menu.Images.Width + 2 + 2);
end;

procedure TMLRRecentDocs.Rebuild;
begin
  Clear;
  Build;
end;

procedure TMLRRecentDocs.Remove(const FileName: string);
var i: Integer;
begin
  i := FRecentDocs.IndexOf(FileName);
  if i <> -1 then FRecentDocs.Delete(i);
  if FAutoRebuild then Rebuild;
end;

procedure TMLRRecentDocs.SetSeparator(const Value: TMenuItem);
begin
  FSeparator := Value;
  if Assigned(FSeparator) then
    FSeparator.FreeNotification(Self);
end;

function TMLRRecentDocs.TextForItem(Item: TMenuItem): string;
var i: Integer; s: string;
begin
  i := 0;
  s := Item.Caption;
  while s[i] <> ' ' do Inc(i);
  Result := Copy(s, i+1, 5000);
end;

end.

