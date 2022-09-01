program edit_color;

uses
  Forms,
  edit_color_main in 'edit_color_main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
