{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit GpSQLBuilderPkg;

interface

uses
  GpSQLBuilder.AST, GpSQLBuilder, GpSQLBuilder.Serialize, 
  System.Generics.Collections, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('GpSQLBuilderPkg', @Register);
end.
