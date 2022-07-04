unit TntFileCtrl;

interface

function WideSelectDirectory(const Caption: WideString; const Root: WideString; var Directory: WideString): Boolean;

implementation

uses FileCtrl;

function WideSelectDirectory;
var
  Dir: string;
begin
  Result := SelectDirectory(Caption, Root, Dir);
  Directory := Dir;
end;


end.