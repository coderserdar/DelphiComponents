program djsonview;

uses
  Forms,
  uFormJsonViewer in 'uFormJsonViewer.pas' {FormJsonViewer};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'Delphi 2010 JSON Viewer';
  Application.CreateForm(TFormJsonViewer, FormJsonViewer);
  Application.Run;
end.
