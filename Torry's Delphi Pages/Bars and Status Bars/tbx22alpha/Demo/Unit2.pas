unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

var
  N: Integer = 1;

{ TForm2 }

constructor TForm2.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  Caption := 'MDI Child Window ' + IntToStr(N);
  Inc(N);
end;

procedure TForm2.WMEraseBkgnd(var Message: TMessage);
begin
  Message.Result := 1;
end;

end.
 