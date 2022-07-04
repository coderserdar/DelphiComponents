unit FBDataSetRegister;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils
{$ifdef FPC}
  , LazarusPackageIntf
{$endif}
  ;

procedure Register;
implementation
uses JvUIBStoredProc, FBCustomDataSet;

procedure RegisterJvUIBStoredProc;
begin
  RegisterComponents('Jv UIB',[TJvUIBStoredProc]);
end;

procedure RegisterFBCustomDataSet;
begin
  RegisterComponents('Jv UIB', [TFBDataSet]);
  RegisterClass(TFBAnsiMemoField);
end;

procedure Register;
begin
{$ifdef FPC}
  RegisterUnit('JvUIBStoredProc', @RegisterJvUIBStoredProc);
  RegisterUnit('FBCustomDataSet', @RegisterFBCustomDataSet);
{$else}
  RegisterJvUIBStoredProc;
  RegisterFBCustomDataSet;
{$endif}
end;

end.

