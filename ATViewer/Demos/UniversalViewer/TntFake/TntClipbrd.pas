unit TntClipbrd;

interface

uses
  Windows, Clipbrd, SysUtils;

type
  TTntClipboard = class(TClipboard)
  public
    function AsWideText: WideString;
  end;

var
  TntClipboard: TTntClipboard;


implementation

function TTntClipboard.AsWideText;
begin
  Result := AsText;
end;

initialization
  
  TntClipboard := TTntClipboard.Create;

finalization

  FreeAndNil(TntClipboard);

end.
