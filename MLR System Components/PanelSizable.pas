unit PanelSizable;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TPanelSizable = class(TPanel)
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
  published
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TPanelSizable]);
end;

{ TPanelSizable }

constructor TPanelSizable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := '';
  BevelInner := bvLowered;
  BorderWidth := 2;
end;

procedure TPanelSizable.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if csDesigning in ComponentState then exit;
  Params.Style := Params.Style or WS_THICKFRAME;
end;

end.
