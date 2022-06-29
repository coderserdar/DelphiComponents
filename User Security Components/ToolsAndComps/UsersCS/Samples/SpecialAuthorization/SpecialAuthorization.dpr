program SpecialAuthorization;

uses
  Forms,
  main in 'main.pas' {Form1},
  discount_auth in 'discount_auth.pas' {frmDiscountAuth};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
