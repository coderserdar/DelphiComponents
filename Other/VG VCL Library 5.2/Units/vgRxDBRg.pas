{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         RX library extensions registration            }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L-}

unit vgRXDBRg;

interface

procedure Register;

implementation
uses SysUtils, Classes, Forms, TypInfo, vgUtils, DsgnIntf, DB, DBTables, vgDBUtl,
  vgRxDBEx, rxQuery, vgVCLRes, vgQuery, vgRXDBCt, vgTreeCm, vgDBRg
  , {$IFNDEF _D4_}UpdSQLE {$ELSE} UpdSQLEd {$ENDIF}
  {$IFDEF _D3_}, DBConsts, BDEConst {$ENDIF}
  {$IFNDEF _D3_}, DBConsts {$ENDIF}
  ;

{$R vgDRXDB.dcr}

type
{ TvgUpdateScriptEditor }
  TvgUpdateScriptEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

procedure TvgUpdateScriptEditor.ExecuteVerb(Index: Integer);
var
  I: TUpdateKind;
  UpdateSQL: TUpdateSQL;
begin
  case Index of
    0: with TvgUpdateScript(Component) do
       begin
         UpdateSQL := TUpdateSQL.Create(Component);
         try
           UpdateSQL.DataSet := DataSet;
           for I := Low(TUpdateKind) to High(TUpdateKind) do
             UpdateSQL.SQL[I] := SQL[I];

           if EditUpdateSQL(UpdateSQL) then
           begin
             for I := Low(TUpdateKind) to High(TUpdateKind) do
               SQL[I] := UpdateSQL.SQL[I];
             Designer.Modified;
           end;
         finally
           UpdateSQL.Free;
         end;
       end;
  end;
end;

function TvgUpdateScriptEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := ResStr(SUpdateSQLEditor);
  end;
end;

function TvgUpdateScriptEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure Register;
begin
  RegisterComponents(LoadStr(SRegDataControls), [TvgDBTreeCombo, TvgDBLookupCombo,
    TDBHistoryComboBox, TvgDBGrid, TrxQuickSearch]);
  RegisterComponents(LoadStr(SRegDataAccess), [TvgQuery, TvgSQLScript, TvgUpdateScript]);

  RegisterPropertyEditor(TypeInfo(String), TvgDBTreeCombo, 'DataFieldID', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgDBTreeCombo, 'DataFieldParentID', TDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(String), TvgDBTreeCombo, 'DataFieldText', TDataFieldProperty);

  RegisterComponentEditor(TvgUpdateScript, TvgUpdateScriptEditor);
end;

initialization
  InitFieldAutoSizers([TrxQuery, TvgQuery]);

finalization
  DoneFieldAutoSizers([TrxQuery, TvgQuery]);

end.

