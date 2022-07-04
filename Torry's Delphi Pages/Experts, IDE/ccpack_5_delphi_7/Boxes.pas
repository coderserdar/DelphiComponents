{*******************************************************}
{                                                       }
{       Delphi Visual Component Library                 }
{       Custom Containers Pack (CCPack)                 }
{                                                       }
{       Copyright (c) 1997-99, Sergey Orlik             }
{                                                       }
{     Written by:                                       }
{       Sergey Orlik                                    }
{       product manager                                 }
{       Russia, C.I.S. and Baltic States (former USSR)  }
{       Inprise Moscow office                           }
{       e-mail:  sorlik@inprise.ru                      }
{       WWW: http://www.inprise.ru                      }
{                                                       }
{       Personal Home Page:                             }
{       www.geocities.com/SiliconValley/Way/9006/       }
{                                                       }
{*******************************************************}
{$I CCPDEF.INC}

{$IFDEF VER_CB}
  {$ObjExportAll On}
{$ENDIF}

unit Boxes;

{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, ExtCtrls, StdCtrls, Dialogs, ActnList;

type
  TBox = class(TCustomPanel)
  private
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Paint; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    property Caption;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property Font;
    property Locked;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;  

  TControlGroupBox = class(TCustomGroupBox)
  private
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  TControlScrollBox = class(TScrollBox)
  private
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

implementation

resourcestring
  sResNotFound = 'Resource for %s is not found.';

{ TBox }

constructor TBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (ClassType <> TBox) and not (csDesignInstance in ComponentState) then
  begin
    if not InitInheritedComponent(Self, ClassType.ClassParent) then
      raise EResNotFound.CreateFmt(SResNotFound, [ClassName]);
    if Assigned(FOnCreate) then
    try
      FOnCreate(Self);
    except
      Application.HandleException(Self);
    end;
  end
  else
  begin
    Width := 320;
    Height := 240;
  end;
end;

destructor TBox.Destroy;
begin
  if Assigned(FOnDestroy) then
  try
    FOnDestroy(Self);
  except
    Application.HandleException(Self);
  end;
  inherited Destroy;
end;

procedure TBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Parent = nil then
    Params.WndParent := Application.Handle;
end;

procedure TBox.SetParent(AParent: TWinControl);
begin
  if (Parent = nil) and HandleAllocated then
    DestroyHandle;
  inherited;
end;

procedure TBox.Paint;
var
  Rect: TRect;
  TopColor, BottomColor: TColor;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := clBtnHighlight;
    if Bevel = bvLowered then TopColor := clBtnShadow;
    BottomColor := clBtnShadow;
    if Bevel = bvLowered then BottomColor := clBtnHighlight;
  end;

begin
  Rect := GetClientRect;
  if BevelOuter <> bvNone then
  begin
    AdjustColors(BevelOuter);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  Frame3D(Canvas, Rect, Color, Color, BorderWidth);
  if BevelInner <> bvNone then
  begin
    AdjustColors(BevelInner);
    Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
  end;
  with Canvas do
  begin
    Brush.Color := Color;
    FillRect(Rect);
    Brush.Style := bsClear;
    Font := Self.Font;
  end;
end;

procedure TBox.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
end;

procedure TBox.SetChildOrder(Child: TComponent; Order: Integer);
var
  I, J: Integer;
begin
  if Child is TControl then
    inherited SetChildOrder(Child, Order)
  else
  begin
    Dec(Order, ControlCount);
    J := -1;
    for I := 0 to ComponentCount - 1 do
      if not Components[I].HasParent then
      begin
        Inc(J);
        if J = Order then
        begin
          Child.ComponentIndex := I;
          Exit;
        end;
      end;
  end;
end;

{ TControlGroupBox }

constructor TControlGroupBox.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if (ClassType <> TControlGroupBox) and not (csDesignInstance in ComponentState) then
  begin
    if not InitInheritedComponent(Self, ClassType.ClassParent) then
      raise EResNotFound.CreateFmt(sResNotFound, [ClassName]);
    if Assigned(FOnCreate) then
    try
      FOnCreate(Self);
    except
      Application.HandleException(Self);
    end;
  end
  else
  begin
    Width := 320;
    Height := 240;
  end;
end;

destructor TControlGroupBox.Destroy;
begin
  if Assigned(FOnDestroy) then
  try
    FOnDestroy(Self);
  except
    Application.HandleException(Self);
  end;
  inherited Destroy;
end;

procedure TControlGroupBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Parent = nil then
    Params.WndParent := Application.Handle;
end;

procedure TControlGroupBox.SetParent(AParent: TWinControl);
begin
  if (Parent = nil) and HandleAllocated then
    DestroyHandle;
  inherited;
end;

procedure TControlGroupBox.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
end;

procedure TControlGroupBox.SetChildOrder(Child: TComponent; Order: Integer);
var
  I, J: Integer;
begin
  if Child is TControl then
    inherited SetChildOrder(Child, Order)
  else
  begin
    Dec(Order, ControlCount);
    J := -1;
    for I := 0 to ComponentCount - 1 do
      if not Components[I].HasParent then
      begin
        Inc(J);
        if J = Order then
        begin
          Child.ComponentIndex := I;
          Exit;
        end;
      end;
  end;
end;

{ TControlScrollBox }

constructor TControlScrollBox.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if (ClassType <> TControlScrollBox) and not (csDesignInstance in ComponentState) then
  begin
    if not InitInheritedComponent(Self, ClassType.ClassParent) then
      raise EResNotFound.CreateFmt(sResNotFound, [ClassName]);
    if Assigned(FOnCreate) then
    try
      FOnCreate(Self);
    except
      Application.HandleException(Self);
    end;
  end
  else
  begin
    Width := 320;
    Height := 240;
  end;
end;

destructor TControlScrollBox.Destroy;
begin
  if Assigned(FOnDestroy) then
  try
    FOnDestroy(Self);
  except
    Application.HandleException(Self);
  end;
  inherited Destroy;
end;

procedure TControlScrollBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if Parent = nil then
    Params.WndParent := Application.Handle;
end;

procedure TControlScrollBox.SetParent(AParent: TWinControl);
begin
  if (Parent = nil) and HandleAllocated then
    DestroyHandle;
  inherited;
end;

procedure TControlScrollBox.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  OwnedComponent: TComponent;
begin
  inherited GetChildren(Proc, Root);
  if Root = Self then
    for I := 0 to ComponentCount - 1 do
    begin
      OwnedComponent := Components[I];
      if not OwnedComponent.HasParent then Proc(OwnedComponent);
    end;
end;

procedure TControlScrollBox.SetChildOrder(Child: TComponent; Order: Integer);
var
  I, J: Integer;
begin
  if Child is TControl then
    inherited SetChildOrder(Child, Order)
  else
  begin
    Dec(Order, ControlCount);
    J := -1;
    for I := 0 to ComponentCount - 1 do
      if not Components[I].HasParent then
      begin
        Inc(J);
        if J = Order then
        begin
          Child.ComponentIndex := I;
          Exit;
        end;
      end;
  end;
end;

end.




