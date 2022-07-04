program AstaNCOCI8Server;

uses
  Forms,
  dialogs,
  astautil,
  Main in 'Main.pas' {frmMain},
  AstaDataModule in 'AstaDataModule.pas' {AstaOracleDataModule: TDataModule};

{$R *.RES}

begin
  if paramcheck('?')>0 then begin
   ShowMessage('Asta NCOCI8 Server'+#13+
               'Command Line Switches'+#13+
               'port=NNNN where NNNN is a port number'+#13+#13+
               'Asta Technology Group LLC - www.astatech.com'+#13+
               'Dmitry Arefiev - www.da-soft.com');
   halt;
  end;
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
