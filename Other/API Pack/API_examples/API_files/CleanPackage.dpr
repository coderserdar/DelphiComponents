program CleanPackage;

uses
  Forms,
  u_cleanpackage in 'u_cleanpackage.pas' {f_cleanpackage};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Clean Unnecessary Files';
  Application.CreateForm(Tf_cleanpackage, f_cleanpackage);
  Application.Run;
end.
