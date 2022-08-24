unit Child;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.plsController
  {$IF CompilerVersion >= 25.0 }
  , FMX.StdCtrls
  {$IFEND}
  ;

type
  TfrmChild = class(TForm)
    lbTest: TLabel;
    sbClose: TButton;
    plsController1: TplsController;
    procedure sbCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmChild: TfrmChild;

implementation

{$R *.fmx}

procedure TfrmChild.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TfrmChild.sbCloseClick(Sender: TObject);
begin
  Close;
end;

end.
