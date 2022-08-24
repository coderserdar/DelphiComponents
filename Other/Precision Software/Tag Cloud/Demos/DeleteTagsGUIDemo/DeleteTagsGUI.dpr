program DeleteTagsGUI;

uses
  Forms,
  Main in 'Main.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delete Tags GUI Demo';
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
