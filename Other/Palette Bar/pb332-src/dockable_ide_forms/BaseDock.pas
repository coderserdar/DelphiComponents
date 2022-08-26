unit BaseDock;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DockForm, IDEMessages, ActnList;

type
  TBaseDockHostForm = class(TDockableForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FLastFocusedClient: TDockableForm;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    function GetVisibleClientCount: Integer;
    procedure UMFinalUndock(var Message: TMessage); message UM_FINALUNDOCK;
    procedure UMNewClientFocused(var Message: TMessage); message UM_NEWCLIENTFOCUSED;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  protected
    function CreateDockParent(var Message: TCMDockClient): Boolean; override;
    procedure DoAddDockClient(Client: TControl; const ARect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    function DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean; override;
    function GetDialogCharParentClass: TWinControlClass; virtual; abstract;
    function GetDockSiteControl: TWinControl; virtual;
    procedure SetDockable(const Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ManualDockClient(Client: TControl; DropControl: TControl;
      ControlSide: TAlign; Replace: Boolean); virtual;
    procedure ResetCaption;
    procedure ResetStayOnTop;
    property VisibleClientCount: Integer read GetVisibleClientCount;
  end;

  TBaseDockHostFormClass = class of TBaseDockHostForm;

var
  HostDockFormList: TList;

implementation

{$R *.DFM}

{ TBaseDockHostForm }

constructor TBaseDockHostForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBaseDockHostForm.Destroy;
begin
  inherited Destroy;
end;

procedure TBaseDockHostForm.CMDialogChar(var Message: TCMDialogChar);
begin
end;

function TBaseDockHostForm.CreateDockParent(var Message: TCMDockClient): Boolean;
begin
  Result := False;
end;

function TBaseDockHostForm.DoUnDock(NewTarget: TWinControl; Client: TControl): Boolean;
begin
  Result := False;
end;

procedure TBaseDockHostForm.UMFinalUndock(var Message: TMessage);
begin
end;

procedure TBaseDockHostForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  {}
end;

procedure TBaseDockHostForm.ManualDockClient(Client: TControl;
  DropControl: TControl; ControlSide: TAlign; Replace: Boolean);
begin
  {}
end;

procedure TBaseDockHostForm.ResetCaption;
begin
end;

procedure TBaseDockHostForm.DoAddDockClient(Client: TControl;
  const ARect: TRect);
begin
  inherited DoAddDockClient(Client, ARect);
end;

procedure TBaseDockHostForm.DoRemoveDockClient(Client: TControl);
begin
  inherited DoRemoveDockClient(Client);
end;

procedure TBaseDockHostForm.SetDockable(const Value: Boolean);
begin
  // Do nothing; don't allow docking toggle
end;

function TBaseDockHostForm.GetDockSiteControl: TWinControl;
begin
  Result := Self;
end;

procedure TBaseDockHostForm.ResetStayOnTop;
begin
end;

function TBaseDockHostForm.GetVisibleClientCount: Integer;
begin
end;

procedure TBaseDockHostForm.UMNewClientFocused(var Message: TMessage);
begin
end;

procedure TBaseDockHostForm.WMActivate(var Message: TWMActivate);
begin
end;

initialization
  RegisterDesktopFormClass(TBaseDockHostForm, isDockSite, '');
  HostDockFormList := TList.Create;
finalization
  HostDockFormList.Free;
end.
