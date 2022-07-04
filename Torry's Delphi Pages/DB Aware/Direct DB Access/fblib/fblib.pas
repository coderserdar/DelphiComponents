{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit fblib; 

interface

uses
  FBLDatabase, FBLTransaction, FBLDsql, FBLMetadata, FBLScript, FBLService, 
    FBLEvents, FBLParamDsql, FBLReg, FBLConst, FBLExcept, FBLHtmlExport, 
    FBLmixf, FBLTableToSqlScriptExport, FBLTextGridExport, ibase_h, iberror_h, 
    LazarusPackageIntf; 

implementation

procedure Register; 
begin
  RegisterUnit('FBLReg', @FBLReg.Register); 
end; 

initialization
  RegisterPackage('fblib', @Register); 
end.
