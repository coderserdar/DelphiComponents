{*******************************************************}
{File:      NCOciBuff.PAS                               }
{Revision:  0.03.02 / 24.08.2000                        }
{Comment:   NC OCI8 VCL: Oracle8 dictionary data module }
{Copyright: (c) 1999-2000, Dmitry Arefiev               }
{Author:    Dmitry Arefiev, darefiev@da-soft.com        }
{*******************************************************}
{$I NCOciDef.inc}

unit NCOciDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, NCOciDB, NCOci, NCOciWrapper;

type
  TOCIDM = class(TDataModule)
    qrSelObjs: TOCIQuery;
    qrPacks: TOCIQuery;
    qrPackProcs: TOCIQuery;
    qrProcs: TOCIQuery;
    qrSeqs: TOCIQuery;
    qrTabDefaults: TOCIQuery;
    qrTabCons: TOCIQuery;
  private
    { Private declarations }
    class procedure GetDictionaryInfo(const ADatabaseName: String; AList: TStrings;
        AIncludeSystem: Boolean; AQuery: TOCIQuery);
  public
    { Public declarations }
    class procedure GetSelectables(const ADatabaseName: String; AList: TStrings;
        AIncludeSystem: Boolean);
    class procedure GetPackages(const ADatabaseName: String; AList: TStrings;
        AIncludeSystem: Boolean);
    class procedure GetProcs(const ADatabaseName: String; AList: TStrings;
        AIncludeSystem: Boolean);
    class procedure GetPackProcs(const ADatabaseName: String; const APackName: String;
        AList: TStrings);
    class procedure GetSeqs(const ADatabaseName: String; AList: TStrings;
        AIncludeSystem: Boolean);
    class function GetTabDefaults(const ADatabaseName, AOwner, ATableName: String): TDataSet;
    class function GetTabConstraints(const ADatabaseName, AOwner, ATableName: String): TDataSet;
  end;

var
  OCIDM: TOCIDM;

implementation

{$R *.DFM}

Uses NCOciUtil;

class procedure TOCIDM.GetDictionaryInfo(const ADatabaseName: String; AList: TStrings;
    AIncludeSystem: Boolean; AQuery: TOCIQuery);
const
    sExcludeSys: String = ' and owner not in (''SYS'', ''SYSTEM'')';
begin
    with OCIDM, AQuery do begin
        DatabaseName := ADatabaseName;
        if AIncludeSystem then
            MacroByName('system').Value := ''
        else
            MacroByName('system').Value := sExcludeSys;
        if not Active then begin
            Prepare;
            Open;
        end
        else
            First;
        AList.BeginUpdate;
        try
            AList.Clear;
            while not EOF do begin
                AList.Add(Fields[0].AsString);
                Next;
            end;
        finally
            AList.EndUpdate;
        end;
    end;
end;

class procedure TOCIDM.GetSelectables(const ADatabaseName: String; AList: TStrings;
    AIncludeSystem: Boolean);
begin
    if OCIDM = nil then
        OCIDM := TOCIDM.Create(nil);
    GetDictionaryInfo(ADatabaseName, AList, AIncludeSystem, OCIDM.qrSelObjs);
end;

class procedure TOCIDM.GetPackages(const ADatabaseName: String; AList: TStrings;
    AIncludeSystem: Boolean);
begin
    if OCIDM = nil then
        OCIDM := TOCIDM.Create(nil);
    GetDictionaryInfo(ADatabaseName, AList, AIncludeSystem, OCIDM.qrPacks);
end;

class procedure TOCIDM.GetProcs(const ADatabaseName: String; AList: TStrings;
    AIncludeSystem: Boolean);
begin
    if OCIDM = nil then
        OCIDM := TOCIDM.Create(nil);
    GetDictionaryInfo(ADatabaseName, AList, AIncludeSystem, OCIDM.qrProcs);
end;

class procedure TOCIDM.GetSeqs(const ADatabaseName: String; AList: TStrings;
    AIncludeSystem: Boolean);
begin
    if OCIDM = nil then
        OCIDM := TOCIDM.Create(nil);
    GetDictionaryInfo(ADatabaseName, AList, AIncludeSystem, OCIDM.qrSeqs);
end;

class procedure TOCIDM.GetPackProcs(const ADatabaseName, APackName: String;
    AList: TStrings);
var
    i: Integer;
    p, o: String;
begin
    if OCIDM = nil then
        OCIDM := TOCIDM.Create(nil);
    with OCIDM, qrPackProcs do begin
        DatabaseName := ADatabaseName;
        i := Pos('.', APackName);
        if i <> 0 then begin
            o := UCOraName(Copy(APackName, 1, i - 1));
            p := UCOraName(Copy(APackName, i + 1, Length(APackName)));
        end
        else begin
            o := '';
            p := UCOraName(APackName);
        end;
        with ParamByName('O') do
            if AsString <> o then begin
                Close;
                AsString := o;
            end;
        with ParamByName('P') do
            if AsString <> p then begin
                Close;
                AsString := p;
            end;
        if not Active then begin
            Prepare;
            Open;
        end
        else
            First;
        AList.BeginUpdate;
        try
            AList.Clear;
            while not EOF do begin
                AList.Add(Fields[0].AsString);
                Next;
            end;
        finally
            AList.EndUpdate;
        end;
    end;
end;

class function TOCIDM.GetTabConstraints(const ADatabaseName, AOwner,
  ATableName: String): TDataSet;
begin
    if OCIDM = nil then
        OCIDM := TOCIDM.Create(nil);
    with OCIDM, qrTabCons do begin
        DatabaseName := ADatabaseName;
        if AOwner = '' then
            ParamByName('OWNER').Clear
        else
            ParamByName('OWNER').AsString := UCOraName(AOwner);
        ParamByName('TABNAME').AsString := UCOraName(ATableName);
        Open;
        Result := qrTabCons;
    end;
end;

class function TOCIDM.GetTabDefaults(const ADatabaseName, AOwner,
  ATableName: String): TDataSet;
begin
    if OCIDM = nil then
        OCIDM := TOCIDM.Create(nil);
    with OCIDM, qrTabDefaults do begin
        DatabaseName := ADatabaseName;
        if AOwner = '' then
            ParamByName('OWNER').Clear
        else
            ParamByName('OWNER').AsString := UCOraName(AOwner);
        ParamByName('TABNAME').AsString := UCOraName(ATableName);
        Open;
        Result := qrTabDefaults;
    end;
end;

initialization
    OCIDM := nil;

finalization
    OCIDM.Free;
    OCIDM := nil;

end.
