program SimpleTest;

uses
  Forms,
  CryptoUtils in '../../../HashLib!/CryptoUtils.pas',
  MD in '../../../HashLib!/MD.pas',
  SHA in '../../../HashLib!/SHA.pas',
  CRC in '../../../HashLib!/CRC.pas',
  Tiger in '../../../HashLib!/Tiger.pas',
  Adler in '../../../HashLib!/Adler.pas',
  Haval in '../../../HashLib!/Haval.pas',
  Gost in '../../../HashLib!/Gost.pas',
  RIPEMD in '../../../HashLib!/RIPEMD.pas',
  CryptoAPI in '../../../HashLib!/CryptoAPI.pas',
  HashTests in '../../../HashLib!/HashTests.pas',
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
