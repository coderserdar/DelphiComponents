unit DM;

interface

uses
  lresources, SysUtils, Classes, Dialogs, plsController;

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

initialization
  {$I DM.lrs}

end.
