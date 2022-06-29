unit frmBasico_Rep;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  users_basic, users_cs, DBCtrls, ComCtrls, ExtCtrls;

type
  TfrmBasico_Rep = class(TForm)
    UsersCSReg1: TUsersCSReg;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBasico_Rep1: TfrmBasico_Rep;

implementation

{$R *.DFM}

end.
