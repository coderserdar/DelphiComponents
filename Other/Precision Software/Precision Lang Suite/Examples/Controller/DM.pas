unit DM;

interface

uses
  SysUtils, Classes, Dialogs, plsController;

type
  TfrmDM = class(TDataModule)
    dlgOpen: TOpenDialog;
    plsController1: TplsController;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDM: TfrmDM;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
