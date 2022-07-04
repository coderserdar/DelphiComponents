{*******************************************************}
{                                                       }
{         Vladimir Gaitanoff Delphi VCL Library         }
{         RX lib extensions                             }
{         TrxQuickSearch                                }
{                                                       }
{         Copyright (c) 1997, 2000                      }
{                                                       }
{*******************************************************}

{$I VG.INC }
{$D-,L- }

unit vgRxDBEx;

interface
uses Classes, DB, vgDBCtrl, DBUtils;

type
  TrxQuickSearch = class(TvgQuickSearch)
  private
    FLocate: {$IFDEF RX240}TLocateObject{$ELSE}TDBLocate{$ENDIF};
  protected
    function LocateDataSet(DataSet: TDataSet; const KeyFields: string;
      const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation
uses Windows, vgDBUtl, {$IFDEF _D3_}DBTables{$ELSE}vgBDE{$ENDIF};

{ TrxQuickSearch }
constructor TrxQuickSearch.Create(AOwner: TComponent);
begin
  inherited;
  FLocate := {$IFDEF RX240}TLocateObject{$ELSE}TDBLocate{$ENDIF}.Create;
end;

destructor TrxQuickSearch.Destroy;
begin
  FLocate.Free;
  inherited;
end;

function TrxQuickSearch.LocateDataSet(DataSet: TDataSet; const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  DataSet.DisableControls;
  try
    FLocate.DataSet := DataSet as TBDEDataSet;
    try
      Result := FLocate.Locate(KeyFields, KeyValues,
        not (loPartialKey in Options), not (loCaseInsensitive in Options));
    finally
      FLocate.DataSet := nil;
    end;
  finally
    DataSet.EnableControls;
  end;
end;

end.
