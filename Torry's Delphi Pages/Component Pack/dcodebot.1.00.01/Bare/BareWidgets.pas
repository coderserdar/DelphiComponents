unit BareWidgets;

interface

type
  TWidget = class
  protected
    procedure DoKeyPress(var Key: Char); virtual;
    procedure DoMouseDown(X, Y: Integer; Buttons: TMouseButtons); virtual
    procedure DoMouseWheel(Scroll: Integer); virtual
    procedure MouseEnter; virtual;
    procedure MouseLeave; virtual;
    procedure FocusEnter
    procedure Render; virtual; abstract;
  public
    procedure Capture;
    procedure Focus;
    property Focused
    property Captured;
    property Text
    property Enabled
    property Visible
    property OnKeyPress
    propery MouseDown(X, Y: Integer; Buttons: TMouseButtons); virtual
  end;

  TWidgetContainer = class
  end;

  TWidgetCollection = class



implementation

end.
