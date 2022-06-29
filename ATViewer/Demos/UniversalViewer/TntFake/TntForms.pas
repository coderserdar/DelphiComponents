unit TntForms;

interface

uses
  Forms;

function TntApplication: TApplication;

type
  TTntForm = TForm;


implementation

function TntApplication: TApplication;
begin
  Result := Application;
end;


end.
