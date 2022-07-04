unit ColorValueListEditor;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Grids, ValEdit, ColorPresets, Graphics;

type
  TColorValueListEditor = class(TValueListEditor)
  private
    fColorPresets_Key: TColorPresets;
    fColorPresets_Value: TColorPresets;
    function GetColorPresets_Key: TColorPresets;
    function GetColorPresets_Value: TColorPresets;
    procedure SetColorPresets_Key(const Value: TColorPresets);
    procedure SetColorPresets_Value(const Value: TColorPresets);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  protected
    procedure DrawCell(ACol, ARow: Longint; ARect: TRect; AState: TGridDrawState); override;

  public
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property ColorPresets_Key: TColorPresets read GetColorPresets_Key write SetColorPresets_Key;
    property ColorPresets_Value: TColorPresets read GetColorPresets_Value write SetColorPresets_Value;
  end;


implementation



{ TColorValueListEditor }

procedure TColorValueListEditor.CMEnter(var Message: TCMEnter);
begin
  inherited;
  InvalidateGrid;
end;

procedure TColorValueListEditor.CMExit(var Message: TCMExit);
begin
  inherited;
  InvalidateGrid;
end;

destructor TColorValueListEditor.Destroy;
begin
  if Assigned(fColorPresets_Key) then fColorPresets_Key.DeleteOwner(Self);
  if Assigned(fColorPresets_Value) then fColorPresets_Value.DeleteOwner(Self);
  inherited;
end;



procedure TColorValueListEditor.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var ColorPresetsLink: TColorPresets;
begin
  try
    ColorPresetsLink := nil;
    if Assigned(ColorPresets_Key) and (ACol = 0) then ColorPresetsLink := ColorPresets_Key;
    if Assigned(ColorPresets_Value) and (ACol = 1) then ColorPresetsLink := ColorPresets_Value;
    if ColorPresetsLink = nil then exit;
    if ARow < FixedRows then exit;

    if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible) then
    begin
     if assigned(InplaceEditor) then
       InplaceEditor.Brush.Color := ColorPresetsLink.ColorInplaceEditor;

     if ARow mod 2 = 0 then
        Canvas.Brush.Color := ColorPresetsLink.ColorActive1 else
        Canvas.Brush.Color := ColorPresetsLink.ColorActive2;

     if AState = [gdSelected, gdFocused] then Canvas.Brush.Color := ColorPresetsLink.ColorSelectedActive;
    end else
    begin
     if ARow mod 2 = 0 then
        Canvas.Brush.Color := ColorPresetsLink.ColorInactive1 else
        Canvas.Brush.Color := ColorPresetsLink.ColorInactive2;

     if AState = [gdSelected] then Canvas.Brush.Color := ColorPresetsLink.ColorSelectedInactive;
    end;

    if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible) then
    begin
      if ARow mod 2 = 0 then
        Canvas.Font.Assign(ColorPresetsLink.FontActive1) else
        Canvas.Font.Assign(ColorPresetsLink.FontActive2);

      if AState = [gdSelected, gdFocused] then Canvas.Font.Assign(ColorPresetsLink.FontSelectedActive);
    end else
    begin
      if ARow mod 2 = 0 then
        Canvas.Font.Assign(ColorPresetsLink.FontInactive1) else
        Canvas.Font.Assign(ColorPresetsLink.FontInactive2);

      if AState = [gdSelected] then Canvas.Font.Assign(ColorPresetsLink.FontSelectedInactive);
    end;

     if  ColorPresetsLink.FrameOverride then
     begin
       if Focused or (assigned(InplaceEditor) and InplaceEditor.Visible) then
          Canvas.Pen.Assign(ColorPresetsLink.FramePenActive) else
          Canvas.Pen.Assign(ColorPresetsLink.FramePenInactive);
       Canvas.Pen.Style := psSolid;
     end;
    //  Canvas.Brush.Style := bsClear;
  finally
    inherited;
  end;
end;

function TColorValueListEditor.GetColorPresets_Key: TColorPresets;
begin
  Result := fColorPresets_Key;
end;

function TColorValueListEditor.GetColorPresets_Value: TColorPresets;
begin
  Result := fColorPresets_Value;
end;

procedure TColorValueListEditor.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = ColorPresets_Key) and (Operation = opRemove) then
  begin
    fColorPresets_Key := nil;
    InvalidateGrid;
  end;

  if (AComponent = ColorPresets_Value) and (Operation = opRemove) then
  begin
    fColorPresets_Value := nil;
    InvalidateGrid;
  end;
end;

procedure TColorValueListEditor.SetColorPresets_Key(const Value: TColorPresets);
begin
  if Value = nil then if Assigned(fColorPresets_Key) then fColorPresets_Key.DeleteOwner(Self);
  fColorPresets_Key := Value;
  if Value <> nil then
  begin
    fColorPresets_Key.FreeNotification(Self);
    fColorPresets_Key.AddOwner(Self);
  end;
  InvalidateGrid;
end;

procedure TColorValueListEditor.SetColorPresets_Value(const Value: TColorPresets);
begin
  if Value = nil then if Assigned(fColorPresets_Value) then fColorPresets_Value.DeleteOwner(Self);
  fColorPresets_Value := Value;
  if Value <> nil then
  begin
    fColorPresets_Value.FreeNotification(Self);
    fColorPresets_Value.AddOwner(Self);
  end;
  InvalidateGrid;
end;

end.
