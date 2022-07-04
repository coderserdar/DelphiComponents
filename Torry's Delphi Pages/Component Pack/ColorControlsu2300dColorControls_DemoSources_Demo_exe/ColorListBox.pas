unit ColorListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, StdCtrls, ColorPresets, Graphics;

type

  TColorListBox = class(TListBox)
  private
    fColorPresets: TColorPresets;
    fOnResize: TNotifyEvent;
    function GetColorPresets: TColorPresets;
    procedure SetColorPresets(const Value: TColorPresets);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
  public
    destructor Destroy; override;  
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Resize; override;
  published
    property ColorPresets: TColorPresets read GetColorPresets write SetColorPresets;
    property OnResize: TNotifyEvent read fOnResize write fOnResize;
  end;

implementation
{$IFDEF VER140}
uses Types;
{$ENDIF}

{ TColorListBox }

procedure TColorListBox.CMEnter(var Message: TCMEnter);
begin
  Invalidate;  
  inherited;
end;

procedure TColorListBox.CMExit(var Message: TCMExit);
begin
  Invalidate;
  inherited;
end;

destructor TColorListBox.Destroy;
begin
  if Assigned(ColorPresets) then ColorPresets.DeleteOwner(self);
  inherited;
end;

procedure TColorListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  try
    if not Assigned(ColorPresets) then exit;

    // Задаем цвет фона
    if Focused then
    begin
      if Index mod 2 = 0 then
        Canvas.Brush.Color := ColorPresets.ColorActive1 else
        Canvas.Brush.Color := ColorPresets.ColorActive2;

      if State = [odSelected] then
        if Index mod 2 = 0 then
          Canvas.Brush.Color := ColorPresets.ColorMultiSelectedActive1 else
          Canvas.Brush.Color := ColorPresets.ColorMultiSelectedActive2;

      if State = [odSelected, odFocused] then
        Canvas.Brush.Color := ColorPresets.ColorSelectedActive;
    end else
    begin
      if Index mod 2 = 0 then
        Canvas.Brush.Color := ColorPresets.ColorInactive1 else
        Canvas.Brush.Color := ColorPresets.ColorInactive2;

      if State = [odSelected] then
        if Index mod 2 = 0 then
          Canvas.Brush.Color := ColorPresets.ColorMultiSelectedInactive1 else
          Canvas.Brush.Color := ColorPresets.ColorMultiSelectedInactive2;

      if (State = [odSelected]) and (Index = ItemIndex) then
        Canvas.Brush.Color := ColorPresets.ColorSelectedInactive;
    end;

    // Задаем шрифт фона
    if Focused then
    begin
      if Index mod 2 = 0 then
        Canvas.Font.Assign(ColorPresets.FontActive1) else
        Canvas.Font.Assign(ColorPresets.FontActive2);

      if State = [odSelected] then
        if Index mod 2 = 0 then
          Canvas.Font.Assign(ColorPresets.FontMultiSelectedActive1) else
          Canvas.Font.Assign(ColorPresets.FontMultiSelectedActive2);

      if State = [odSelected, odFocused] then
        Canvas.Font.Assign(ColorPresets.FontSelectedActive);
    end else
    begin
      if Index mod 2 = 0 then
        Canvas.Font.Assign(ColorPresets.FontInactive1) else
        Canvas.Font.Assign(ColorPresets.FontInactive2);

      if State = [odSelected] then
        if Index mod 2 = 0 then
          Canvas.Font.Assign(ColorPresets.FontMultiSelectedInactive1) else
          Canvas.Font.Assign(ColorPresets.FontMultiSelectedInactive2);

      if (State = [odSelected]) and (Index = ItemIndex) then
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

function TColorListBox.GetColorPresets: TColorPresets;
begin
  Result := fColorPresets;
end;

procedure TColorListBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if not((AComponent = fColorPresets) and (Operation = opRemove)) then exit;
  ColorPresets := nil;
  Invalidate;
end;

procedure TColorListBox.Resize;
begin
  inherited;
  if Assigned(ColorPresets) then Invalidate;
  if Assigned(fOnResize) then OnResize(Self);
end;

procedure TColorListBox.SetColorPresets(const Value: TColorPresets);
begin
  if Value = nil then if Assigned(fColorPresets) then fColorPresets.DeleteOwner(Self);
  fColorPresets := value;
  if Value <> nil then
  begin
    fColorPresets.FreeNotification(self);
    Style := lbOwnerDrawFixed;
    fColorPresets.AddOwner(Self);
  end;
  Invalidate;
end;

end.
