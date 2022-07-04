unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, users_basic, users_cs;

type
  TForm2 = class(TForm)
    MainMenu1: TMainMenu;
    Orders1: TMenuItem;
    New1: TMenuItem;
    List1: TMenuItem;
    N1: TMenuItem;
    Print1: TMenuItem;
    UsersCSReg1: TUsersCSReg;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses main;

{$R *.DFM}

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action:=caFree;
  Form2:=NIL;
end;

end.
