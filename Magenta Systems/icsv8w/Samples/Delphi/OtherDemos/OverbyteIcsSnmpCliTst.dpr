program OverbyteIcsSnmpCliTst;

uses
  Forms,
  OverbyteIcsIniFiles in '..\..\OverbyteIcsIniFiles.pas',
  OverbyteIcsSnmpCliTst1 in 'OverbyteIcsSnmpCliTst1.pas' {SnmpClientTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSnmpClientTestForm, SnmpClientTestForm);
  Application.Run;
end.
