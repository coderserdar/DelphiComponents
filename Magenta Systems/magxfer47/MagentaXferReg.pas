unit MagentaXferReg;

interface

uses
  SysUtils, Classes,
  MagentaCopy, MagentaFtp3, MagentaHttp ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Magenta Systems', [TMagFileCopy, TMagHttp, TMagFtp]);
end;

end.
