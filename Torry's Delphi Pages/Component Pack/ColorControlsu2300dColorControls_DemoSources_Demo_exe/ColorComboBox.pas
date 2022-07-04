unit ColorComboBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ColorPresets, Graphics;

type
  TColor_ComboBox = class(TComboBox)
  private
    fColorPresets: TColorPresets;
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    function GetColorPresets: TColorPresets;
    procedure SetColorPresets(const Value: TColorPresets);
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property ColorPresets: TColorPresets read GetColorPresets write SetColorPresets;
  end;


implementation


{ TColorComboBox }

procedure TColor_ComboBox.CMEnter(var Message: TCMEnter);
begin
  Invalidate;  
  inherited;
end;

procedure TColor_ComboBox.CMExit(var Message: TCMExit);
begin
  Invalidate;  
  inherited;
end;

destructor TColor_ComboBox.Destroy;
begin
  if Assigned(ColorPresets) then ColorPresets.DeleteOwner(self);
  inherited;
end;

procedure TColor_ComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  try
    if not Assigned(ColorPresets) then exit;
    // Задаем цвет фона
    if State <> [odComboBoxEdit] then
    begin
      if Index mod 2 = 0 then
        Canvas.Brush.Color := ColorPresets.ColorActive1 else
        Canvas.Brush.Color := ColorPresets.ColorActive2;

      if (State = [odSelected, odFocused, odComboBoxEdit]) or
        (State = [odSelected, odFocused]) then
        Canvas.Brush.Color := ColorPresets.ColorSelectedActive;
    end else
    begin
      if Index mod 2 = 0 then
        Canvas.Brush.Color := ColorPresets.ColorInactive1 else
        Canvas.Brush.Color := ColorPresets.ColorInactive2;

      if (State = [odComboBoxEdit]) and (Index = ItemIndex) then
        Canvas.Brush.Color := ColorPresets.ColorSelectedInactive;
    end;

    // Задаем шрифт фона
    if State <> [odComboBoxEdit] then
    begin
      if Index mod 2 = 0 then
        Canvas.Font.Assign(ColorPresets.FontActive1) else
        Canvas.Font.Assign(ColorPresets.FontActive2);

      if (State = [odSelected, odFocused, odComboBoxEdit]) or
        (State = [odSelected, odFocused]) then
        Canvas.Font.Assign(ColorPresets.FontSelectedActive);
    end else
    begin
      if Index mod 2 = 0 then
        Canvas.Font.Assign(ColorPresets.FontInactive1) else
        Canvas.Font.Assign(ColorPresets.FontInactive2);

      if (State = [odComboBoxEdit]) and (Index = ItemIndex) then
        Canvas.Font.Assign(ColorPresets.FontSelectedInactive);
    end;

  finally
    inherited;
  end;

  if ColorPresets.FrameOverride then
  begin
    if Focused then Canvas.Pen.Assign(ColorPresets.FramePenActive) else
      Canvas.Pen.Assign(ColorPresets.FramePenInactive);
    Canvas.Brush.Style := bsClear;
    Rect.Bottom := rect.Bottom + 1;
    Canvas.Rectangle(rect);
  end;
end;

function TColor_ComboBox.GetColorPresets: TColorPresets;
begin
   Result := fColorPresets;
end;

procedure TColor_ComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if not((AComponent = fColorPresets) and (Operation = opRemove)) then exit;
  ColorPresets := nil;
  Invalidate;
end;

procedure TColor_ComboBox.SetColorPresets(const Value: TColorPresets);
begin
 if Value = nil then if Assigned(fColorPresets) then fColorPresets.DeleteOwner(Self);
  fColorPresets := value;
  if Value <> nil then
  begin
    fColorPresets.FreeNotification(self);
    fColorPresets.AddOwner(Self);
    Style := csOwnerDrawFixed;
  end;
  Invalidate;
end;

end.
