unit regs;

interface

procedure Register;

implementation

uses
  Classes, jsondoc, jsonparser, jsontreeview;

procedure Register;
begin
  RegisterComponents('JSON', [TJSONDocument, TJSONParser, TJSONTreeView]);
end;

end.
