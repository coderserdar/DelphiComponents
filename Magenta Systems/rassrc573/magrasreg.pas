unit magrasreg;

interface

//Include the resource with the bitmap. Note bitmap
//must have same name as the class
{$R MAGRASCON.RES}
{$R MAGRASEDT.RES}
{$R MAGRASPER.RES}
{$R MAGTAPI.RES}

uses
  SysUtils, Classes,
  magrascon, magrasper, magrasedt, magrasent, magrasadm2, magtapi ;

procedure Register;

implementation

(*
uses
{xIFDEF WIN32}
  {$IFDEF COMPILER10_UP}
    Windows,
    ToolsApi,
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
    DesignIntf, DesignEditors;
  {$ELSE}
    DsgnIntf;
  {$ENDIF}
{xENDIF}  *)

procedure Register;
begin
    RegisterComponents('Magenta Systems',
     [TMagRasCon, TMagRasEdt, TMagRasPer, TMagRasAdm, TMagTAPI] );
end;

end.
