unit DsToolEditor;
//------------------------------------------------------------------------------
interface
//------------------------------------------------------------------------------
uses
  DesignEditors, DsDockSite;
//------------------------------------------------------------------------------
type
//------------------------------------------------------------------------------
  TDSToolBarEditor = class(TDefaultEditor)
  protected
    procedure NewButton;
    procedure NewSeparator;
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;//TDSToolBarEditor
//------------------------------------------------------------------------------
  TDSDockSiteEditor = class(TComponentEditor)
  protected
    procedure NewToolBar;
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;//TDSToolBarEditor
//------------------------------------------------------------------------------
  procedure Register;
//------------------------------------------------------------------------------
implementation
uses
  DesignIntf, Controls, DsToolBar, Classes;
//------------------------------------------------------------------------------
procedure Register;
begin
  RegisterComponentEditor(TDSToolBar   , TDSToolBarEditor);
  RegisterComponentEditor(TDSToolButton, TDSToolBarEditor);
  
  RegisterComponentEditor(TDSDockSite  , TDSDockSiteEditor);
end;
//------------------------------------------------------------------------------
{ TDSToolBarEditor }
//------------------------------------------------------------------------------
function TDSToolBarEditor.GetVerb(Index: Integer): string;
begin
  case Index of
  0: Result := 'New B&utton';
  1: Result := 'New Se&parator';
  end;
end;
//------------------------------------------------------------------------------
function TDSToolBarEditor.getVerbCount: Integer;
begin
  Result := 2;
end;
//------------------------------------------------------------------------------
procedure TDSToolBarEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
  0: NewButton;
  1: NewSeparator;
  end;
end;
//------------------------------------------------------------------------------
procedure TDSToolBarEditor.NewButton;
var
  vParent: TWinControl;
begin
  if Component is TDSToolBar then
    vParent := TWinControl(Component)
  else
    vParent := TWinControl(Component).Parent;
  Designer.CreateComponent(TDSToolButton, vParent, 1, 1, 23, 23);
end;//NewButton
//------------------------------------------------------------------------------
procedure TDSToolBarEditor.NewSeparator;
begin

end;//NewSeparator
//------------------------------------------------------------------------------
procedure TDSToolBarEditor.Edit;
begin
  if (Component is TDSToolButton) then
    inherited;
end;//Edit
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
{ TDSDockSiteEditor }
//------------------------------------------------------------------------------
procedure TDSDockSiteEditor.Edit;
begin
  inherited;
end;//Edit
//------------------------------------------------------------------------------
procedure TDSDockSiteEditor.ExecuteVerb(Index: Integer);
begin
  if Index = 0 then
    NewToolBar;
end;
//------------------------------------------------------------------------------
function TDSDockSiteEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then
    Result := 'New Tool Bar';
end;
//------------------------------------------------------------------------------
function TDSDockSiteEditor.getVerbCount: Integer;
begin
  Result := 1;
end;
//------------------------------------------------------------------------------
procedure TDSDockSiteEditor.NewToolBar;
begin
  Designer.CreateComponent(TDSToolBar, Component, 0, 0, 129, 23);
end;
//------------------------------------------------------------------------------
end.
