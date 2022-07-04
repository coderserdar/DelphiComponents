unit ColorDBGrid;

interface

uses
  SysUtils, Classes, Controls, ColorPresets, Grids, DBGrids,
  {$IFDEF VER130} //D5
  Windows,
  {$ENDIF}
  {$IFDEF VER140} //D6
  Types,
  {$ENDIF}
  {$IFDEF VER150} //D7
  Types,
  {$ENDIF}


  Graphics, Messages;

type
  TOnMouseWeelEvent = procedure(Sender: TComponent; WeelDelta: Integer) of object;
  TColorColumn = class(TColumn)
  private
    fColorPresets: TColorPresets;
    function GetColorPresets: TColorPresets;
    procedure SetColorPresets(const Value: TColorPresets);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Soruce: TPersistent); override; 
  published
    property ColorPresets: TColorPresets read GetColorPresets write SetColorPresets;
  end;

  TColorDBGridColumns = class(TDBGridColumns)
  private
    function GetItems(Index: integer): TColorColumn;
    procedure SetItems(Index: integer; const Value: TColorColumn);
  public
    property Items[Index: integer]: TColorColumn read GetItems write SetItems; default;
  end;

  TColorDBGrid = class(TDBGrid)
  private
    fColorPresets: TColorPresets;
    fRepaintOnMouseWeel: boolean;
    fOnMouseWeel: TOnMouseWeelEvent;
    fOnResize: TNotifyEvent;
    function GetColorPresets: TColorPresets;
    procedure SetColorPresets(const Value: TColorPresets);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMMouseWheel(var Message: TWMMouseWheel);  message WM_MOUSEWHEEL;
  protected
    function CreateColumns : TDBGridColumns; override;
    procedure Resize; override;
  public
    procedure DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    destructor Destroy; override;

  published
    property ColorPresets: TColorPresets read GetColorPresets write SetColorPresets;
    property RepaintOnMouseWeel: boolean read fRepaintOnMouseWeel write fRepaintOnMouseWeel;
    property OnMouseWeel: TOnMouseWeelEvent read fOnMouseWeel write fOnMouseWeel;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
  end;


implementation



{ TColorDBGrid }

procedure TColorDBGrid.CMEnter(var Message: TCMEnter);
begin
  Invalidate;
  inherited;

end;

procedure TColorDBGrid.CMExit(var Message: TCMExit);
begin

  Invalidate;
  inherited;

end;

function TColorDBGrid.CreateColumns: TDBGridColumns;
begin
  Result := TColorDBGridColumns.Create(Self, TColorColumn);
end;

destructor TColorDBGrid.Destroy;
begin
  if Assigned(ColorPresets) then ColorPresets.DeleteOwner(Self);
  inherited;
end;

procedure TColorDBGrid.DrawColumnCell(const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
var ColorPresetsLink: TColorPresets;
begin
  try
    if not Assigned(DataSource) or (not Assigned(ColorPresets) and not Assigned(TColorColumn(Column).ColorPresets)) then exit;
    if Assigned(TColorColumn(Column).ColorPresets) then ColorPresetsLink := TColorColumn(Column).ColorPresets else
   ColorPresetsLink := ColorPresets;

    if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible) then
    begin
      if assigned(InplaceEditor) then
      InplaceEditor.Brush.Color := ColorPresetsLink.ColorInplaceEditor;


      if DataSource.DataSet.RecNo mod 2 = 0 then
        Canvas.Brush.Color := ColorPresetsLink.ColorActive1 else
        Canvas.Brush.Color := ColorPresetsLink.ColorActive2;


      if SelectedRows.CurrentRowSelected and (State <> [gdSelected, gdFocused]) then
        if DataSource.DataSet.RecNo mod 2 = 0 then
          Canvas.Brush.Color := ColorPresetsLink.ColorMultiSelectedActive1 else
          Canvas.Brush.Color := ColorPresetsLink.ColorMultiSelectedActive2;

      if State = [gdSelected, gdFocused] then Canvas.Brush.Color := ColorPresetsLink.ColorSelectedActive;
    end else
    begin
      if DataSource.DataSet.RecNo mod 2 = 0 then
        Canvas.Brush.Color := ColorPresetsLink.ColorInactive1 else
        Canvas.Brush.Color := ColorPresetsLink.ColorInactive2;

      if SelectedRows.CurrentRowSelected and (State <> [gdSelected])  then
        if DataSource.DataSet.RecNo mod 2 = 0 then
          Canvas.Brush.Color := ColorPresetsLink.ColorMultiSelectedInactive1 else
          Canvas.Brush.Color := ColorPresetsLink.ColorMultiSelectedInactive2;

      if State = [gdSelected] then Canvas.Brush.Color := ColorPresetsLink.ColorSelectedInactive;
    end;

    if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible)then
    begin
      if DataSource.DataSet.RecNo mod 2 = 0 then
        Canvas.Font.Assign(ColorPresetsLink.FontActive1) else
        Canvas.Font.Assign(ColorPresetsLink.FontActive2);


      if SelectedRows.CurrentRowSelected and (State <> [gdSelected, gdFocused]) then
        if DataSource.DataSet.RecNo mod 2 = 0 then
          Canvas.Font.Assign(ColorPresetsLink.FontMultiSelectedActive1) else
          Canvas.Font.Assign(ColorPresetsLink.FontMultiSelectedActive2);

      if State = [gdSelected, gdFocused] then Canvas.Font.Assign(ColorPresetsLink.FontSelectedActive);
    end else
    begin
      if DataSource.DataSet.RecNo mod 2 = 0 then
        Canvas.Font.Assign(ColorPresetsLink.FontInactive1) else
        Canvas.Font.Assign(ColorPresetsLink.FontInactive2);

      if SelectedRows.CurrentRowSelected and (State <> [gdSelected])  then
        if DataSource.DataSet.RecNo mod 2 = 0 then
          Canvas.Font.Assign(ColorPresetsLink.FontMultiSelectedInactive1) else
          Canvas.Font.Assign(ColorPresetsLink.FontMultiSelectedInactive2);

      if State = [gdSelected] then Canvas.Font.Assign(ColorPresetsLink.FontSelectedInactive);
    end;

    DefaultDrawColumnCell(Rect, DataCol, Column, State);
    if ColorPresetsLink.FrameOverride then
    begin
      if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible) then
        Canvas.Pen.Assign(ColorPresetsLink.FramePenActive) else
        Canvas.Pen.Assign(ColorPresetsLink.FramePenInactive);
      Canvas.Pen.Style := psSolid;
      Canvas.Brush.Style := bsClear;
      Canvas.Rectangle(rect.Left - 1, rect.Top - 1, rect.Right + 1, rect.Bottom + 1);
    end;
  finally
    inherited;
  end;
end;

function TColorDBGrid.GetColorPresets: TColorPresets;
begin
  Result := fColorPresets;
end;

procedure TColorDBGrid.Notification(AComponent: TComponent; Operation: TOperation);
var i: integer;
begin
  inherited;
  if (AComponent = fColorPresets) and (Operation = opRemove) then
  begin
    for i := 0 to Columns.Count - 1 do
      if Assigned(TColorColumn(Columns).ColorPresets) and (TColorColumn(Columns).ColorPresets = ColorPresets) then
        TColorColumn(Columns).ColorPresets := nil;
      fColorPresets := nil;
  end;
end;

procedure TColorDBGrid.Resize;
begin
  inherited;
  Invalidate;
  if Assigned(fOnResize) then OnResize(Self);
end;

procedure TColorDBGrid.SetColorPresets(const Value: TColorPresets);
begin
  fColorPresets := value;
  if Value <> nil then
  begin
    fColorPresets.FreeNotification(Self);
    ColorPresets.AddOwner(Self);
  end;
   Invalidate;
end;

procedure TColorDBGrid.WMMouseWheel(var Message: TWMMouseWheel);
begin
  if RepaintOnMouseWeel then Invalidate;
  if Assigned(fOnMouseWeel) then fOnMouseWeel(Self, Message.WheelDelta);
  inherited;
end;

{ TColorColumn }

procedure TColorColumn.Assign(Soruce: TPersistent);
begin
  inherited;
  if Soruce is TColorColumn then ColorPresets := TColorColumn(Soruce).ColorPresets;
end;

constructor TColorColumn.Create(Collection: TCollection);
begin
  inherited;
end;

destructor TColorColumn.Destroy;
begin
  if Assigned(ColorPresets) then ColorPresets.DeleteOwner(Self);
  inherited;
end;

function TColorColumn.GetColorPresets: TColorPresets;
begin
  Result := fColorPresets;
end;


procedure TColorColumn.SetColorPresets(const Value: TColorPresets);
begin
  if Value = nil then if Assigned(fColorPresets) then fColorPresets.DeleteOwner(Self);
  fColorPresets := value;
  if Value <> nil then ColorPresets.AddOwner(Self);
end;

{ TColorDBGridColumns }

function TColorDBGridColumns.GetItems(Index: integer): TColorColumn;
begin
 result := TColorColumn(inherited Items[Index]);
end;

procedure TColorDBGridColumns.SetItems(Index: integer;
  const Value: TColorColumn);
begin
  inherited Items[Index] := Value;
end;

end.

