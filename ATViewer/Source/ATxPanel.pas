unit ATxPanel;

interface

uses
  Classes, Controls, StdCtrls, ExtCtrls;

type
  TTextPanel = class(TPanel)
  private
    lab: TLabel;
    procedure SetClick(Value: TNotifyEvent);
    procedure SetCaption(Value: string);
  public
    constructor Create(Owner: TComponent); override;
    property OnLabClick: TNotifyEvent write SetClick;
    property Caption: string write SetCaption;
  protected
    procedure Paint; override;
  end;

implementation

uses Graphics;

procedure TTextPanel.Paint;
begin
  Canvas.Brush.Color := clInfoBk;
  Canvas.FillRect(ClientRect);
end;

constructor TTextPanel.Create;
const
  Msg = 'Format not known'#13'Click here to show binary dump';
begin
  inherited;
  lab := TLabel.Create(Self);
  with lab do
  begin
    Parent := Self;
    Align := alTop;
    Alignment := taCenter;
    Font.Color := clWindowText;
    SetCaption(Msg);
  end;
end;

procedure TTextPanel.SetClick;
begin
  lab.OnClick := Value;
end;

procedure TTextPanel.SetCaption(Value: string);
begin
  lab.Caption := Value;
  Height := lab.Height + 4;
end;

end.
