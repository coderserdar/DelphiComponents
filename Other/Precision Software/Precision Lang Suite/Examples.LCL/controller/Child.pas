unit Child;

interface

uses
  LCLIntf, LCLType, lresources, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, plsController;

type
  TfrmChild = class(TForm)
    lbTest: TLabel;
    sbClose: TButton;
    plsController1: TplsController;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure sbCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmChild: TfrmChild;

implementation

procedure TfrmChild.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrmChild.sbCloseClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I Child.lrs}

end.
