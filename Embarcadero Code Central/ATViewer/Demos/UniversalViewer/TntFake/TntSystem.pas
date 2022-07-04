unit TntSystem;

interface

function WideParamCount: Integer;
function WideParamStr(N: Integer): WideString;


implementation

function WideParamCount: Integer;
begin
  Result := ParamCount;
end;

function WideParamStr(N: Integer): WideString;
begin
  Result := ParamStr(N);
end;


end.
