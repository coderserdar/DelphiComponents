{*******************************************************}
{                                                       }
{       Lista paneli typu TPanel                        }
{                                                       }
{       Copyright (c) 2002, 2003 Tomasz Bojara          }
{                                                       }
{*******************************************************}

unit tbPanelList;

interface

uses SysUtils, Windows, Messages, Classes, Controls, Forms,
  Graphics, Menus;

type

  TFramePanel = class(TFrame)
  private
    FCanvas: TCanvas;
    FSelected: Boolean;
    FIndex: Integer;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetSelected(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Selected: Boolean read FSelected write SetSelected;
    property Index: Integer read FIndex;
  end;

  TFrameClass = class of TFrame;


  TPanelList = class(TWinControl)
  private
    FSelectedColor: TColor;
    FFrame: TFrameClass;
    function FSelColorChanged: Boolean;
    procedure SetSelectedColor(const Value: TColor);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Append;
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BevelWidth;
    property BorderWidth;
    property Ctl3D;
    property Color;
    property Constraints;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Frame: TFrameClass read FFrame write FFrame;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property TabOrder;
    property TabStop default True;
    property SelectedColor: TColor read FSelectedColor write SetSelectedColor
      stored FSelColorChanged default clWindow;
    property ShowHint;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses DBConsts, VDBConsts, TypInfo, ComObj;

procedure Register;
begin
  RegisterComponents('TOM', [TPanelList]);
end;

destructor TPanelList.Destroy;
begin
  inherited;
end;

procedure TPanelList.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_CLIPCHILDREN;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TPanelList.CreateWnd;
begin
  inherited CreateWnd;
end;


constructor TPanelList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csOpaque, csDoubleClicks];
  TabStop := True;
end;

function TPanelList.FSelColorChanged: Boolean;
begin

end;

{ TFramePanel }

constructor TFramePanel.Create(AOwner: TComponent);
begin
  inherited;
  FCanvas:= TControlCanvas.Create;
  TControlCanvas(FCanvas).Control :=  Self;
end;

destructor TFramePanel.Destroy;
begin
  FCanvas.Free;
  inherited;
end;

procedure TFramePanel.SetSelected(const Value: Boolean);
begin
  if FSelected <> Value then
  begin
    FSelected := Value;
    Invalidate;
  end;
end;

procedure TFramePanel.WMPaint(var Message: TWMPaint);
begin
  inherited;
  if FSelected then
  begin
    FCanvas.Pen.Width:= 2;
    FCanvas.MoveTo(1, 1);
    FCanvas.LineTo(Width - 3, 1);
    FCanvas.LineTo(Width - 3, Height - 3);
    FCanvas.LineTo(7, Height - 3);
    FCanvas.LineTo(1, Height - 9);
    FCanvas.LineTo(1, 1);
  end;
end;

procedure TPanelList.SetSelectedColor(const Value: TColor);
begin
  FSelectedColor := Value;
end;

procedure TPanelList.Append;
var F: TFrame;
    FP: TFrame;
begin
  if FFrame <> nil then
  begin
    F:= FFrame.Create(Self);
//    F.Align:= alTop;

    FP:= TFrame.Create(nil);
    FP.Align:= alTop;
    FP.Assign(F);

    F.Free;

    InsertControl(FP);
  end;
end;

end.

