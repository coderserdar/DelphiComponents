unit CPFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmCP = class(TForm)
    txtName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    txtTag: TEdit;
    lblStatus: TLabel;
    btnCancel: TButton;
    btnOk: TButton;
    btnDelete: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCP: TfrmCP;

implementation

{$R *.DFM}

end.
