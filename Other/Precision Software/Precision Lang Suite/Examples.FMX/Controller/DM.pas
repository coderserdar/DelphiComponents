unit DM;

interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Dialogs, FMX.plsController;

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

{%CLASSGROUP 'FMX.Types.TFmxObject'}

{$R *.dfm}

end.
