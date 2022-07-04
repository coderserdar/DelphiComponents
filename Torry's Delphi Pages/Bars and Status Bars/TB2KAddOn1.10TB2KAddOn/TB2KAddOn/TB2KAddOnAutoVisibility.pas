unit TB2KAddOnAutoVisibility;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  TB2Toolbar, TB2Item, TB2ExtItems;

type
  TTB2KToolbarListRefreshList = procedure(ToolbarName: string; Cancel:
    Boolean) of object;

  TTB2KToolbarList = class(TComponent)
  private
    FContainer: TTBVisibilityToggleItem;
    FOnRefreshList: TTB2KToolbarListRefreshList;
  protected
    property Container: TTBVisibilityToggleItem read FContainer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshList;
  published
    property OnRefreshList: TTB2KToolbarListRefreshList read FOnRefreshList
      write FOnRefreshList;
  end;

  ETB2KToolbarList = class(Exception);

  TTBAVListItem = class(TTBCustomItem)
  private
    FToolbarList: TTB2KToolbarList;
    procedure SetToolbarList(Value: TTB2KToolbarList);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ToolbarList: TTB2KToolbarList read FToolbarList write
      SetToolbarList;
  end;

implementation

{ TTB2KToolbarList }

resourcestring
  // Error messages
  sErrFormRequired = 'TB2KToolbarList components must be placed on a form';
  sErrSingleInstance = 'Only one TB2KToolbarList component is permitted on a '
    + 'form: %s is already present on %s';

procedure TTB2KToolbarList.RefreshList;
var
  item: TTBVisibilityToggleItem;
  cancel: Boolean;
  i: Integer;
begin
  FContainer.Clear;
  for i := 0 to Pred(Owner.ComponentCount) do
  begin
    if Owner.Components[i].ClassNameIs('TTBToolbar') then
    begin
      cancel := not ((TTBToolbar(Owner.Components[i]).CloseButton) or
        (TTBToolbar(Owner.Components[i]).CloseButtonWhenDocked));

      if Assigned(FOnRefreshList) then
        FOnRefreshList(Owner.Components[i].Name, cancel);

      if not cancel then
      begin
        item := TTBVisibilityToggleItem.Create(FContainer);
        item.Caption := TTBToolbar(Owner.Components[i]).Caption;
        item.Control := TControl(Owner.Components[i]);
        FContainer.Add(item);
      end;
    end;
  end;
end;

constructor TTB2KToolbarList.Create(AOwner: TComponent);
var
  i: Integer;
begin
  if not Assigned(AOwner) or not (AOwner is TForm) then
    raise ETB2KToolbarList.Create(sErrFormRequired);
  // Ensure there is only one TTB2KToolbarList component on a form
  for i := 0 to Pred(AOwner.ComponentCount) do
    if AOwner.Components[i] is TTB2KToolbarList then
      raise ETB2KToolbarList.CreateFmt(sErrSingleInstance,
        [AOwner.Components[i].Name, AOwner.Name]);

  inherited Create(AOwner);
  FContainer := TTBVisibilityToggleItem.Create(nil);
end;

destructor TTB2KToolbarList.Destroy;
begin
  FContainer.Free;
  inherited;
end;

{ TTBAVListItem }

constructor TTBAVListItem.Create(AOwner: TComponent);
begin
  inherited;
  ItemStyle := ItemStyle + [tbisEmbeddedGroup];
  Caption := '(AVList)';
end;

procedure TTBAVListItem.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FToolbarList) and (Operation = opRemove) then
    SetToolbarList(nil);
end;

procedure TTBAVListItem.SetToolbarList(Value: TTB2KToolbarList);
begin
  if FToolbarList <> Value then
  begin
    FToolbarList := Value;
    if Assigned(FToolbarList) then
    begin
      Value.FreeNotification(Self);
      FToolbarList.RefreshList;
      LinkSubitems := FToolbarList.FContainer;
    end
    else
      LinkSubitems := nil;
  end;
end;

end.

