program ExOrders;

uses
  Forms,
  ExOrderu in 'ExOrderu.pas' {Form1};

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
