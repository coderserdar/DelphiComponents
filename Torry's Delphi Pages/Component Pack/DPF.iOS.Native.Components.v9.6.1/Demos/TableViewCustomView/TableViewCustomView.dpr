program TableViewCustomView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {FTableViewCustomView};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFTableViewCustomView, FTableViewCustomView);
  Application.Run;
end.
