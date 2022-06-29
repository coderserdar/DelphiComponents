unit unit_form1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, users_basic, users_cs;

type
  TFormDLL = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    UsersCSReg1: TUsersCSReg;
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ExecForm1(const TheSecurityComponent: TUsersCS);

var
  Form1: TFormDLL;

implementation

uses main_form;

{$R *.DFM}

procedure ExecForm1(const TheSecurityComponent: TUsersCS);
begin
  Form1:=TFormDLL.Create(NIL);
  Form1.UsersCSReg1.SecurityComponent:=TheSecurityComponent;
  Form1.UsersCSReg1.SetDatabaseConnection(TheSecurityComponent.GetDatabase);
  Form1.UsersCSReg1.ApplySecurity;
  Form1.ShowModal;
  Form1.Release;
end;

end.
