program sql3man;

uses
  Forms,
  fmmain in 'fmmain.pas' {fmain},
  udm in 'udm.pas' {dm: TDataModule},
  fmconnect in 'fmconnect.pas' {fconnect},
  fmsql in 'fmsql.pas' {fsql},
  fmblob in 'fmblob.pas' {fblob},
  fmext in 'fmext.pas' {fext},
  fmabout in 'fmabout.pas' {fAbout},
  fparmsfill in 'fparmsfill.pas' {manFillParms},
  fmfilter in 'fmfilter.pas' {ffilter},
  fmlocate in 'fmlocate.pas' {flocate};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tdm, dm);
  Application.CreateForm(Tfmain, fmain);
  Application.Run;
end.
