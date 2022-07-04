unit jsonregs;

// ***********************************************************************
//
//   JSON components desigh-time support
//
//   pawel.glowacki@embarcadero.com
//   July 2010
//
// ***********************************************************************


interface

{$R jsondoc.dcr}

procedure Register;

implementation

uses
  Classes, jsondoc, jsonparser, jsontreeview;

procedure Register;
begin
  RegisterComponents('JSON', [TJSONDocument, TJSONParser, TJSONTreeView]);
end;

end.
