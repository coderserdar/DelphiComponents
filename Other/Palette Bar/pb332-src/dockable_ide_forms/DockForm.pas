unit DockForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DeskForm, IDEMessages, IniFiles, ActnList, Menus;

type
  TIDEDockType = (dtJoin, dtTab);

  TDockableForm = class(TDesktopForm)
    DockActionList: TActionList;
    DockableCmd: TAction;
    StayOnTopCmd: TAction;
    ZoomWindowCmd: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormGetSiteInfo(Sender: TObject; DockClient: TControl;
      var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
    procedure DockableCmdExecute(Sender: TObject);
    procedure DockableCmdUpdate(Sender: TObject);
    procedure StayOnTopCmdExecute(Sender: TObject);
    procedure StayOnTopCmdUpdate(Sender: TObject);
    procedure ZoomWindowCmdExecute(Sender: TObject);
  private
    FIDEDockType: TIDEDockType;
    FDockEdge: TAlign;
    FAboutToDestroy: Boolean;
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    procedure WMMouseActivate(var Message: TWMMouseActivate); message WM_MOUSEACTIVATE;
  protected
    FDockable: Boolean;
    function CreateDockParent(var Message: TCMDockClient): Boolean; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DockOver(Source: TDragDockObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure SetDockable(const Value: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ForceShow; virtual;
    procedure GetTabDockRect(var ARect: TRect);
    function ToggleDockable: Boolean;
    procedure SaveWindowState(Desktop: TMemIniFile; isProject: Boolean); override;
    procedure LoadWindowState(Desktop: TMemIniFile); override;
    property AboutToDestroy: Boolean read FAboutToDestroy write FAboutToDestroy;
    property Dockable: Boolean read FDockable write SetDockable;
  end;

implementation

{$R *.DFM}

type
  TControlAccess = class(TControl);

constructor TDockableForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDockable := True;
end;

procedure TDockableForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Parent = nil then
    with Params do Style := Style or WS_POPUP;
end;

function TDockableForm.CreateDockParent(var Message: TCMDockClient): Boolean;
begin
end;

procedure TDockableForm.CMDockClient(var Message: TCMDockClient);
begin
  inherited;
end;

procedure TDockableForm.GetTabDockRect(var ARect: TRect);
begin
end;

procedure TDockableForm.DockOver(Source: TDragDockObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  {}
end;

procedure TDockableForm.FormCreate(Sender: TObject);
begin
  inherited;
  {}
end;

procedure TDockableForm.ForceShow;
begin
end;

procedure TDockableForm.FormGetSiteInfo(Sender: TObject;
  DockClient: TControl; var InfluenceRect: TRect; MousePos: TPoint;
  var CanDock: Boolean);
begin
  inherited;
  {}
end;

function TDockableForm.ToggleDockable: Boolean;
begin
  Dockable := not Dockable;
  Result := Dockable;
end;

procedure TDockableForm.SetDockable(const Value: Boolean);
begin
  if FDockable <> Value then
  begin
    FDockable := Value;
    if not Value then
    begin
      if HostDockSite <> nil then ManualDock(nil, nil, alTop);
      DockSite := False;
      DragMode := dmManual;
      DragKind := dkDrag;
    end
    else begin
      DockSite := True;
      DragMode := dmAutomatic;
      DragKind := dkDock;
    end;
  end;
end;

procedure TDockableForm.SaveWindowState(Desktop: TMemIniFile; isProject: Boolean);
begin
  inherited SaveWindowState(Desktop, isProject);
  if SaveStateNecessary and (Desktop <> nil) then
  begin
    Desktop.WriteInteger(DeskSection, ivTBDockHeight, TBDockHeight);
    Desktop.WriteInteger(DeskSection, ivLRDockWidth, LRDockWidth);
    Desktop.WriteBool(DeskSection, ivDockable, Dockable);
  end;
end;

procedure TDockableForm.LoadWindowState(Desktop: TMemIniFile);
begin
  if Desktop <> nil then
  begin
    Dockable := Desktop.ReadBool(DeskSection, ivDockable, True);
    TBDockHeight := Desktop.ReadInteger(DeskSection, ivTBDockHeight, Height);
    LRDockWidth := Desktop.ReadInteger(DeskSection, ivLRDockWidth, Width);
  end;
  inherited LoadWindowState(Desktop);
end;

procedure TDockableForm.DockableCmdExecute(Sender: TObject);
begin
  Dockable := not Dockable;
end;

procedure TDockableForm.DockableCmdUpdate(Sender: TObject);
begin
  (Sender as TAction).Checked := Dockable;
end;

procedure TDockableForm.StayOnTopCmdExecute(Sender: TObject);
begin
  inherited;
  StayOnTopCmd.Checked := not StayOnTopCmd.Checked;
  if StayOnTopCmd.Checked then FormStyle := fsStayOnTop
  else FormStyle := fsNormal;
  if HostDockSite is TBaseDockHostForm then
    TBaseDockHostForm(HostDockSite).ResetStayOnTop
  else if (HostDockSite is TPageControl) and
    (TPageControl(HostDockSite).Owner is TTabDockHostForm) then
    TTabDockHostForm(HostDockSite.Owner).ResetStayOnTop;
end;

procedure TDockableForm.StayOnTopCmdUpdate(Sender: TObject);
begin
  inherited;
  (Sender as TAction).Checked := FormStyle = fsStayOnTop;
end;

procedure TDockableForm.WMMouseActivate(var Message: TWMMouseActivate);
begin
  inherited;
  if (HostDockSite is TJoinDockForm) and ((Message.Result = MA_ACTIVATE) or
    (Message.Result = MA_ACTIVATEANDEAT)) then
  begin
    Perform(CM_ACTIVATE, 0, 0);
    HostDockSite.Perform(UM_NEWCLIENTFOCUSED, 0, Integer(Self));
  end;
end;

procedure TDockableForm.ZoomWindowCmdExecute(Sender: TObject);
begin
  inherited;
  ZoomWindow;
end;

end.
