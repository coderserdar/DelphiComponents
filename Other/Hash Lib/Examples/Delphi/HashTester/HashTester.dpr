program HashTester;
{(C) Coban (alex@ritlabs.com)}

uses
  Windows,
  Messages,
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
  Main in 'Main.pas';

{$R myres.res}

begin
  StartApp;
end.
