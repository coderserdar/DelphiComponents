program demo;

uses
  Forms,
  main in 'main.pas' {mainform},
  editor in 'editor.pas' {editorForm},
  dbform in 'dbform.pas' {adbform};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tmainform, mainform);
  Application.Run;
end.
