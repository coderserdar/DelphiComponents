{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit pls_lazarus;

interface

uses
  plsLangMan, plsController, PLSReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('PLSReg', @PLSReg.Register);
end;

initialization
  RegisterPackage('pls_lazarus', @Register);
end.
