unit cefreg;

{$I ..\src\cef.inc}

interface

procedure Register;

implementation
uses
  Classes, cefvcl
{$ifdef DELPHI16_UP}
  ,ceffmx
{$endif}
  ;

procedure Register;
begin
  RegisterComponents('Chromium', [
    TChromium, TChromiumOSR
{$ifdef DELPHI16_UP}
    ,TChromiumFMX, TChromiumFMXOSR
{$endif}
    ]);
end;

end.
