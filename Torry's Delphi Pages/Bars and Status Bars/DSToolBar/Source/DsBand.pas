unit DsBand;
//------------------------------------------------------------------------------
interface
//------------------------------------------------------------------------------
uses
  Windows, Controls;
//------------------------------------------------------------------------------
type
  // Represents a band with instruments. Keep the control inside
  TDsBand = class
  private
    fPrev: TDsBand;
    fNext: TDsBand;
    fControl: TControl;
    fInsetRight: Integer;
    fInsetBottom: Integer;
    fInsetTop: Integer;
    fInsetLeft: Integer;
    fPosChanging: Boolean;
  protected
    function getX: Integer;
    function getY: Integer;
    function getHeight: Integer;
    function getWidth: Integer;
  public
    // Правоъгълника, който заема Band-a
    function BandRect: TRect;
    function BoundsRect: TRect;
    // Задава координатите на Band-a
    procedure setPos(aX, aY: Integer);
    // Връща дали позицията е в грабъра
    function PosInGrabber(aPos: Integer): Boolean;
    // Контролата, която обгражда Band-a
    property Control: TControl read fControl write fControl;
    // Позицията, къдато се намира Band-a
    property X: Integer read getX;
    property Y: Integer read getY;
    // Width of the Band
    property Width: Integer read getWidth;
    // Height of the Band
    property Height: Integer read getHeight;
    // Insets to the control inside
    property InsetLeft: Integer read fInsetLeft write fInsetLeft;
    property InsetRight: Integer read fInsetRight write fInsetRight;
    property InsetTop: Integer read fInsetTop write fInsetTop;
    property InsetBottom: Integer read fInsetBottom write fInsetBottom;
    // Previous band on the row. Nil means it's first band on the row.
    property Prev: TDsBand read fPrev write fNext;
    // Next band on the row. Nil means it's last band on the row.
    property Next: TDsBand read fNext write fNext;
    //
    property PosChanging: Boolean read fPosChanging;
  end;//TDsBand
//------------------------------------------------------------------------------
implementation
//------------------------------------------------------------------------------
uses
  Classes, Forms;

//------------------------------------------------------------------------------
// TDsBand
//------------------------------------------------------------------------------
function TDsBand.BandRect: TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + Width - 1;
  Result.Bottom := Y + Height - 1;
end;//BandRect
//------------------------------------------------------------------------------
function TDsBand.BoundsRect: TRect;
begin
  Result.Left := X;
  Result.Top := Y;
  Result.Right := X + Width;
  Result.Bottom := Y + Height;
end;//BoundsRect
//------------------------------------------------------------------------------
function TDsBand.getHeight: Integer;
begin
  Result := InsetTop + Control.Height + InsetBottom;
end;//getHeight
//------------------------------------------------------------------------------
function TDsBand.getWidth: Integer;
begin
  Result := InsetLeft + Control.Width + InsetRight;
end;//getWidth
//------------------------------------------------------------------------------
function TDsBand.getX: Integer;
begin
  Result := Control.Left - InsetLeft;
end;//getX
//------------------------------------------------------------------------------
function TDsBand.getY: Integer;
begin
  Result := Control.Top - InsetTop;
end;//getY
//------------------------------------------------------------------------------
function TDsBand.PosInGrabber(aPos: Integer): Boolean;
begin
  Result := (aPos >= X) and (aPos < X + InsetLeft); //TODO
end;//PosInGrabber
//------------------------------------------------------------------------------
procedure TDsBand.setPos(aX, aY: Integer);
var
  vForm: TCustomForm;
begin
  fPosChanging := True;
  try
    if csDesigning in Control.ComponentState then
    begin
      vForm := GetParentForm(Control);
      if (vForm <> nil) and (vForm.Designer <> nil) then
        vForm.Designer.Modified;
    end;
    Control.Left := aX + InsetLeft;
    Control.Top := aY + InsetTop;
    if csDesigning in Control.ComponentState then
    begin
      vForm := GetParentForm(Control);
      if (vForm <> nil) and (vForm.Designer <> nil) then
        vForm.Designer.Modified;
   end;
  finally
    fPosChanging := False;
  end;
end;//setPos
//------------------------------------------------------------------------------

end.
