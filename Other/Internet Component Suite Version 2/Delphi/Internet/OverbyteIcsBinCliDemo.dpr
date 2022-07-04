program OverbyteIcsBinCliDemo;

uses
  Forms,
  OverbyteIcsBinCliDemo1 in 'OverbyteIcsBinCliDemo1.pas' {BinClientForm};

{$R *.RES}

begin
  Application.CreateForm(TBinClientForm, BinClientForm);
  Application.Run;
end.
