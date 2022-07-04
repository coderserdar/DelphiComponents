unit RVMisc;

interface
uses RVScroll, RichView, RVEdit, Dialogs;
function GetRVSearchOptions(fo: TFindOptions): TRVSearchOptions;
function GetRVESearchOptions(fo: TFindOptions): TRVESearchOptions;
{-----------------------------------------------------------------------}
implementation
{-----------------------------------------------------------------------}
function GetRVSearchOptions(fo: TFindOptions): TRVSearchOptions;
begin
  Result := [];
  if frMatchCase in fo then Result := [rvsroMatchCase];
  if frDown in fo then Result := Result+[rvsroDown];
end;
{-----------------------------------------------------------------------}
function GetRVESearchOptions(fo: TFindOptions): TRVESearchOptions;
begin
  Result := [];
  if frMatchCase in fo then Result := [rvseoMatchCase];
  if frDown in fo then Result := Result+[rvseoDown];
end;
end.
