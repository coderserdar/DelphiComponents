unit ColorDrawGrid;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Grids, ColorPresets, Graphics;

type
  TColorDrawGrid = class(TDrawGrid)
    fColorPresets: TColorPresets;
    function GetColorPresets: TColorPresets;
    procedure SetColorPresets(const Value: TColorPresets);
    function CellSelected(ARow, ACol: Longint; SelectedArea: TGridRect): boolean;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function CreateEditor: TInplaceEdit; override;
  public
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;
    destructor Destroy; override;
  published
    property ColorPresets: TColorPresets read GetColorPresets write SetColorPresets;
  end;

implementation
{ TColorDrawGrid }

function TColorDrawGrid.CellSelected(ARow, ACol: Integer;
  SelectedArea: TGridRect): boolean;
begin
  result := (ARow >= SelectedArea.Top) and (ARow <= SelectedArea.Bottom) and
    (ACol >= SelectedArea.Left) and (ACol <= SelectedArea.Right);
end;

procedure TColorDrawGrid.CMEnter(var Message: TCMEnter);
begin
  inherited;
  Invalidate;
end;

procedure TColorDrawGrid.CMExit(var Message: TCMExit);
begin
  inherited;
  Invalidate;
end;

function TColorDrawGrid.CreateEditor: TInplaceEdit;
begin
  Result := TInplaceEdit.Create(Self);
  if Assigned(ColorPresets) then
    Result.Brush.Color := ColorPresets.ColorInplaceEditor;
end;

destructor TColorDrawGrid.Destroy;
begin
  if Assigned(ColorPresets) then ColorPresets.DeleteOwner(self);
  inherited;
end;

procedure TColorDrawGrid.DrawCell(ACol, ARow: Integer; ARect: TRect;
  AState: TGridDrawState);
var ColorPresetsLink: TColorPresets;
begin
  try
    if not Assigned(ColorPresets) then exit;
    if ACol < FixedCols then exit;
    if ARow < FixedRows then exit;
    ColorPresetsLink := ColorPresets;
    if assigned(InplaceEditor) then
      InplaceEditor.Brush.Color := ColorPresetsLink.ColorInplaceEditor;
    if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible) then
    begin
      if assigned(InplaceEditor) then
        InplaceEditor.Brush.Color := ColorPresetsLink.ColorInplaceEditor;
      if ARow mod 2 = 0 then
        Canvas.Brush.Color := ColorPresetsLink.ColorActive1 else
        Canvas.Brush.Color := ColorPresetsLink.ColorActive2;

      if CellSelected(ARow, ACol, Selection) and (aState <> [gdSelected, gdFocused]) then
        if ARow mod 2 = 0 then
          Canvas.Brush.Color := ColorPresetsLink.ColorMultiSelectedActive1 else
          Canvas.Brush.Color := ColorPresetsLink.ColorMultiSelectedActive2;

      if aState = [gdSelected, gdFocused] then Canvas.Brush.Color := ColorPresetsLink.ColorSelectedActive;
    end else
    begin
      if ARow mod 2 = 0 then
        Canvas.Brush.Color := ColorPresetsLink.ColorInactive1 else
        Canvas.Brush.Color := ColorPresetsLink.ColorInactive2;

      if CellSelected(ARow, ACol, Selection) {and (aState <> [gdSelected])  } then
        if ARow mod 2 = 0 then
          Canvas.Brush.Color := ColorPresetsLink.ColorMultiSelectedInactive1 else
          Canvas.Brush.Color := ColorPresetsLink.ColorMultiSelectedInactive2;

      if (aState = [gdSelected]) and (ACol = col) and (ARow = row) then Canvas.Brush.Color := ColorPresetsLink.ColorSelectedInactive;
    end;

    if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible) then
    begin
      if ARow mod 2 = 0 then
        Canvas.Font.Assign(ColorPresetsLink.FontActive1) else
        Canvas.Font.Assign(ColorPresetsLink.FontActive2);


      if CellSelected(ARow, ACol, Selection) and (AState <> [gdSelected, gdFocused]) then
        if ARow mod 2 = 0 then
          Canvas.Font.Assign(ColorPresetsLink.FontMultiSelectedActive1) else
          Canvas.Font.Assign(ColorPresetsLink.FontMultiSelectedActive2);

      if AState = [gdSelected, gdFocused] then Canvas.Font.Assign(ColorPresetsLink.FontSelectedActive);
    end else
    begin
      if ARow mod 2 = 0 then
        Canvas.Font.Assign(ColorPresetsLink.FontInactive1) else
        Canvas.Font.Assign(ColorPresetsLink.FontInactive2);

      if CellSelected(ARow, ACol, Selection) then
        if ARow mod 2 = 0 then
          Canvas.Font.Assign(ColorPresetsLink.FontMultiSelectedInactive1) else
          Canvas.Font.Assign(ColorPresetsLink.FontMultiSelectedInactive2);

      if (aState = [gdSelected]) and (ACol = col) and (ARow = row) then Canvas.Font.Assign(ColorPresetsLink.FontSelectedInactive);
    end;

    Canvas.FillRect(arect);
  finally
    inherited;
  end;
  if not Assigned(ColorPresets) then exit;
  if ACol < FixedCols then exit;
  if ARow < FixedRows then exit;

  if ColorPresetsLink.FrameOverride then
  begin
    if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible) then
      Canvas.Pen.Assign(ColorPresetsLink.FramePenActive) else
      Canvas.Pen.Assign(ColorPresetsLink.FramePenInactive);
    Canvas.MoveTo(ARect.Right, ARect.Top);
    Canvas.LineTo(ARect.Right, ARect.Bottom);
    Canvas.MoveTo(ARect.Left, ARect.Bottom);
    Canvas.LineTo(ARect.Right, ARect.Bottom);
    Canvas.Pen.Style := psSolid;
  end;
end;

function TColorDrawGrid.GetColorPresets: TColorPresets;
begin
  Result := fColorPresets;
end;

procedure TColorDrawGrid.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = fColorPresets) and (Operation = opRemove) then
    fColorPresets := nil;
end;

procedure TColorDrawGrid.SetColorPresets(const Value: TColorPresets);
begin
  if value = nil then
    if Assigned(ColorPresets) then ColorPresets.DeleteOwner(self);
  fColorPresets := value;
  if Value <> nil then
  begin
    ColorPresets.FreeNotification(Self);
    ColorPresets.AddOwner(self);
  end;
  Invalidate;
end;

end.

