unit Child;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, plsController;

type
  TfrmChild = class(TForm)
    lbTest: TLabel;
    sbClose: TButton;
    plsController1: TplsController;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sbCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmChild: TfrmChild;

implementation

{$R *.dfm}

procedure TfrmChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmChild.sbCloseClick(Sender: TObject);
begin
  Close;
end;

end.
