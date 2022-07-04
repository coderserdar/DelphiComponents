//
// "dbxoodbc_nextrs.pas": version: 2008-09-26.
//
// TSQLStoredProcEx:
//   - fixed: NextRecordSet
//   - added: midas(TClientDataSet) support
//

unit dbxoodbc_nextrs;

{$B-,O-,D+,L+}

interface

uses
  Windows, SysUtils, Classes, DB, SqlExpr,
  DBClient, Provider;

type

  TSQLStoredProcBase = TSQLStoredProc;
  TSQLStoredProcEx = class(TSQLStoredProcBase)
  protected
    {$IF CompilerVersion > 18.00}
    FNextRecordSetMode: Boolean;
    {$IFEND}
    procedure PSSetParams(AParams: TParams); override; // Midas & NextRecordSet: midas close cursor by set parameters
    procedure CloseCursor; override; // fix base NextRecordSet close cursor after call NextDataSet
    {$IF CompilerVersion >= 19.00}
    procedure InternalClose; override;
    {$IFEND}
    {$IF CompilerVersion >= 19.00}
    procedure SetCommandText(const Value: UnicodeString); override;
    {$ELSE}
    {$IF CompilerVersion > 17.00}
    procedure SetCommandText(const Value: WideString); override;
    {$ELSE}
    procedure SetCommandText(const Value: string); override;
    {$IFEND}
    {$IFEND}
    procedure ClearCachedSchema;
  public
    function NextRecordSet: TCustomSQLDataSet; reintroduce;
  end;

//
// ClientDataSet_NextRecordSet:
//   - Only for TSQLStoredProcEx.
//
procedure ClientDataSet_NextRecordSet(ACDS: TClientDataset; AProvider: TDataSetProvider);

implementation

procedure ClientDataSet_NextRecordSet(ACDS: TClientDataset; AProvider: TDataSetProvider);
var
  p, d: OleVariant;
  o: TProviderOptions;
begin
  if (ACDS = nil) or (not ACDS.Active) or (AProvider = nil) then
    Exit;
  ACDS.Close;
  o := AProvider.Options;
  try
    AProvider.Options := o + [poAllowCommandText];
    try
      AProvider.Execute(#0'NextRecordSet', p, d);
    except
      on e: EAbort do;
    end;
  finally
    AProvider.Options := o;
  end;
  ACDS.Open;
end;

{ TSQLStoredProcEx }

procedure TSQLStoredProcEx.ClearCachedSchema;
begin
  //
  // Clear cached schema:
  //
  if Assigned(FieldDefs) then
    FieldDefs.Updated := False;
  ClearIndexDefs;
  DestroyFields;
end;

procedure TSQLStoredProcEx.CloseCursor;
begin
  {$IF CompilerVersion > 18.00}
  if not (FNextRecordSetMode and IsCursorOpen) then
  {$IFEND}
  inherited;
  ClearCachedSchema;
end;

{$IF CompilerVersion >= 19.00}
procedure TSQLStoredProcEx.InternalClose;
begin
  {$IF CompilerVersion > 18.00}
  if not (FNextRecordSetMode and IsCursorOpen) then
  {$IFEND}
  inherited;
end;

type
  TDataSetPro = class(TDataSet);
  TObjectProc = procedure of object;
{$IFEND}

function TSQLStoredProcEx.NextRecordSet: TCustomSQLDataSet;
{$IF CompilerVersion >= 19.00}
var
   vMethod: TMethod;
{$IFEND}
begin
  {$IF CompilerVersion > 18.00}
  {$IF CompilerVersion >= 19.00}
  vMethod.Code := @TDataSetPro.CloseCursor;
  vMethod.Data := Self;
  {$ELSE}
  SetState(dsInactive);
  CloseCursor;
  {$IFEND}
  //Tag := 1; // for debug point condition
  FNextRecordSetMode := True;
  try
    {$IF CompilerVersion >= 19.00}
    SetState(dsInactive);
    TObjectProc(vMethod); // call TDataSet.CloseCursor
    ClearCachedSchema;
    {$IFEND}

    Result := inherited NextRecordSet;
  finally
    FNextRecordSetMode := False;
    //Tag := 0;
  end;
  {$ELSE}
  Result := inherited NextRecordSet;
  {$IFEND}
end;

procedure TSQLStoredProcEx.PSSetParams(AParams: TParams);
begin
  if not Active then // it is not correctly, but base class not allow accees to fields ...
    inherited;
end;

procedure TSQLStoredProcEx.SetCommandText;//(const Value: UnicodeString);
var
  sCommand: string;
begin
  if (Value <> '') and (Value[1] = #0) then
  begin
    // Command Extensions:
    sCommand := Copy(Value, 2, Length(Value)-1);
    if SameText(sCommand, 'NextRecordSet')  then
    begin
      if NextRecordSet = nil then
        Close;
    end;
    Abort;
  end
  else
    inherited;
end;

end.
