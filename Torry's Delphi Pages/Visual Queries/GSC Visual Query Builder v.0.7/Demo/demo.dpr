{*******************************************************************************
* GSC Query Builder v0.5 Demo application                                      *
*------------------------------------------------------------------------------*
* Please read the top of GSCQBWorkArea.pas!!!                                  * 
********************************************************************************}
program demo;

uses
  Forms,
  main in 'main.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
