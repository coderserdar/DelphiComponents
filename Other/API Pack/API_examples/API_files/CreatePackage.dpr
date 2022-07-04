program CreatePackage;

uses
  Forms,
  u_createpackage in 'u_createpackage.pas' {f_createpackage};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Create Zip Package of Folder';
  Application.CreateForm(Tf_createpackage, f_createpackage);
  Application.Run;
end.
