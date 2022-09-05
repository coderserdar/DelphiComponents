{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDHibernateQueryAdv; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��չ�ؼ���
* ��Ԫ���ƣ��߼�Query�ؼ���Ԫ
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 2009
* ���ݲ��ԣ�Win2000/XP/Vista/2008 + Delphi 2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.23 V1.8
*               ��ֲ�� Delphi2009
*           2006.09.04 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$M+}
{$WARNINGS OFF}

{$IFDEF SUPPORT_ADO}

uses
  SysUtils, Classes, DB, ADODB, CnDHibernateMemData, CnDHibernateClasses,
  CnDHibernatePodoList, CnDHibernateBase, TypInfo, CnDHibernateConsts, StrUtils, DBClient;

type
  TCnDHibernateQueryAdvance = class(TClientDataSet)
  private
    FDHQuery: TCnDHibernateQuery;
    FRowsPerPage: integer;
    FCurrentPage: integer;
    FConnection: TAdoConnection;
    FActive: boolean;
    FTableName: string;
    FuseFormula: boolean;
    FPKName: string;
    FAbout: string;
    procedure SetRowsPerPage(const Value: integer);
    procedure SetCurrentPage(const Value: integer);
    function GetPagecount: integer;
    function GetSQL: TStrings;
    procedure SetSQL(const Value: TStrings);
    procedure SetConnection(const Value: TAdoConnection);
    procedure SetActive(const Value: boolean);
  protected
    procedure GetFields; 
    { ��ȡ formula �ֶ� }
    procedure GetFormulaFields;
  public
    constructor Create(Aowner: TComponent); override;
    destructor Destroy; override;
    procedure PriorPage;
    procedure NextPage;
    procedure FirstPage;
    procedure LastPage;
    procedure Open;
    procedure Close;
    procedure Refresh;
    procedure find(hql: string; param: ICnMap);
    function get(TableName: string; pkName: string; pkValue: Variant): TObject; 
    { save }
    function saveData(TableName: string; data: TObject): Boolean; 
    { update }
    function updateData(TableName: string; data: TObject; pkName: string): Boolean; 
    { delete }
    function deleteData(TableName: string; data: TObject): Boolean; 
    { save or update }
    function saveOrUpdateData(TableName: string; data: TObject; pkName: string): Boolean; 
    { save all }
    function saveAllData(TableName: string; dataList: TCnPodoList): Boolean; 
    { update all }
    function updateAllData(TableName: string; dataList: TCnPodoList; pkName: string): Boolean; 
    { delete all }
    function deleteAllData(TableName: string; dataList: TCnPodoList): Boolean; 
    { save or update all }
    function saveOrUpdateAllData(TableName: string; dataList: TCnPodoList; pkName: string): Boolean;
  published
    property About: string read FAbout write FAbout;
    property Active: boolean read FActive write SetActive;
    property Connection: TAdoConnection read FConnection write SetConnection;
    property SQL: TStrings read GetSQL write SetSQL;
    property PageCount: integer read GetPagecount;
    property CurrentPage: integer read FCurrentPage write SetCurrentPage;
    property RowsPerPage: integer read FRowsPerPage write SetRowsPerPage default 10; 
    { table name for formula }
    property TableName: string read FTableName write FTableName; 
    { whether use formula or not? }
    property useFormula: boolean read FuseFormula write FuseFormula; 
    { pk name }
    property PKName: string read FPKName write FPKName;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateQueryAdvance }

procedure TCnDHibernateQueryAdvance.Close;
begin
  inherited Close;
  FDHQuery.Close;
  FActive := false;
end;

constructor TCnDHibernateQueryAdvance.Create(Aowner: TComponent);
begin
  inherited Create(Aowner);
  FConnection := nil;
  FActive := false;
  FDHQuery := TCnDHibernateQuery.Create(nil);
  RowsPerPage := 10;
end;

function TCnDHibernateQueryAdvance.deleteAllData(TableName: string; dataList: TCnPodoList): Boolean;
begin
  Result := FDHQuery.deleteAllData(TableName, dataList);
end;

function TCnDHibernateQueryAdvance.deleteData(TableName: string; data: TObject): Boolean;
begin
  Result := FDHQuery.deleteData(TableName, data);
end;

destructor TCnDHibernateQueryAdvance.Destroy;
begin
  FDHQuery.Connection := nil;
  FDHQuery.Free;
  FConnection := nil;
  inherited Destroy;
end;

procedure TCnDHibernateQueryAdvance.find(hql: string; param: ICnMap);
var
  i: Integer;
begin
  Self.Close;
  FDHQuery.SQL.Text := hql;
  for i := 0 to param.size - 1 do
  begin
    FDHQuery.Parameters.ParamValues[param.getTable(i)^.hashName] := param.getTable(i)^.hashValue;
  end;
  Self.Open;
end;

procedure TCnDHibernateQueryAdvance.FirstPage;
begin
  if not FActive then
    Exit;
  CurrentPage := 1;
end;

function TCnDHibernateQueryAdvance.get(TableName, pkName: string; pkValue: Variant): TObject;
begin
  Result := FDHQuery.get(TableName, pkName, pkValue);
end;

procedure TCnDHibernateQueryAdvance.GetFields;
var
  i: integer;
  ft: TFieldType;
begin
  FieldDefs.Clear;
  for i := 0 to FDHQuery.FieldCount - 1 do
  begin
    with FDHQuery.Fields[i] do
    begin
      ft := DataType;
      if ft = ftWideString then
        ft := ftString; 
      {$IFDEF DELPHI2006_UP}
      if ft = ftWideMemo then
        ft := ftMemo; 
      {$ENDIF}
      FieldDefs.Add(FieldName, ft, Size, Required);
    end;
  end;
  GetFormulaFields;
  CreateDataSet;
  FActive := True;
end;

procedure TCnDHibernateQueryAdvance.GetFormulaFields;
var
  clazz: TClass;
  Pplst: PPropList;
  Classtypeinfo: PTypeInfo;
  classDataInfo: PTypeData;
  i: Integer;
  tk: TTypeKind;
  ppName: string;
begin
  // todo : get formula fields
  clazz := FindClass(Format(DH_CLASS_NAME, [TableName]));
  Classtypeinfo := clazz.ClassInfo;
  classDataInfo := GetTypeData(Classtypeinfo);
  if classDataInfo.PropCount <> 0 then
  begin
    GetMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
    try
      GetPropInfos(Classtypeinfo, Pplst);
      for i := 0 to classDataInfo.PropCount - 1 do
      begin
        ppName := pplst[i]^.Name; 
        // check whether formula attr
        if RightStr(ppName, 8) = '_FORMULA' then
        begin
          tk := Pplst[i]^.PropType^.Kind;
          if tk <> tkMethod then
          begin
            if (tk = tkString) or (tk = tkLString) or (tk = tkWString) {$IFDEF UNICODE_STRING} or (tk = tkUString) {$ENDIF} then
              FieldDefs.Add(ppName, ftString, 255, False);
            if tk = tkInteger then
              FieldDefs.Add(ppName, ftInteger, 8, False);
            if tk = tkFloat then
              FieldDefs.Add(ppName, ftFloat, 32, False);
            if tk = tkVariant then
              FieldDefs.Add(ppName, ftVariant, 255, False);
          end;
        end;
      end;
    finally
      FreeMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
    end;
  end;
end;

function TCnDHibernateQueryAdvance.GetPagecount: integer;
begin
  Result := 0;
  if not FActive then
    Exit;
  Result := Trunc(FDHQuery.RecordCount / FRowsPerPage);
  if Result * FRowsPerPage <> FDHQuery.RecordCount then
    Result := Result + 1;
end;

function TCnDHibernateQueryAdvance.GetSQL: TStrings;
begin
  Result := TStrings(FDHQuery.SQL);
end;

procedure TCnDHibernateQueryAdvance.LastPage;
begin
  if not FActive then
    Exit;
  CurrentPage := PageCount;
end;

procedure TCnDHibernateQueryAdvance.NextPage;
begin
  if not FActive then
    Exit;
  if CurrentPage < PageCount then
    CurrentPage := CurrentPage + 1;
end;

procedure TCnDHibernateQueryAdvance.Open;
begin
  try
    FDHQuery.Open;
  except
    FActive := false;
    Exit;
  end;
  FActive := True;
  GetFields;
  inherited Open;
  CurrentPage := 1;
  First;
end;

procedure TCnDHibernateQueryAdvance.PriorPage;
begin
  if not FActive then
    Exit;
  if CurrentPage > 1 then
    CurrentPage := CurrentPage - 1;
end;

procedure TCnDHibernateQueryAdvance.Refresh;
var
  pg: Integer;
begin
  pg := CurrentPage;
  Close;
  Open;
  CurrentPage := pg;
end;

function TCnDHibernateQueryAdvance.saveAllData(TableName: string; dataList: TCnPodoList): Boolean;
begin
  Result := FDHQuery.saveAllData(TableName, dataList);
end;

function TCnDHibernateQueryAdvance.saveData(TableName: string; data: TObject): Boolean;
begin
  Result := FDHQuery.saveData(TableName, data);
end;

function TCnDHibernateQueryAdvance.saveOrUpdateAllData(TableName: string; dataList: TCnPodoList; pkName: string): Boolean;
begin
  Result := FDHQuery.saveOrUpdateAllData(TableName, dataList, pkName);
end;

function TCnDHibernateQueryAdvance.saveOrUpdateData(TableName: string; data: TObject; pkName: string): Boolean;
begin
  Result := FDHQuery.saveOrUpdateData(TableName, data, pkName);
end;

procedure TCnDHibernateQueryAdvance.SetActive(const Value: boolean);
begin
  if (FConnection = nil) or (FDHQuery.SQL.Text = '') then
    Exit;
  FActive := Value;
  if FActive then
    Open
  else
    Close;
end;

procedure TCnDHibernateQueryAdvance.SetConnection(const Value: TAdoConnection);
begin
  Close;
  FConnection := Value;
  FDHQuery.Connection := FConnection;
end;

procedure TCnDHibernateQueryAdvance.SetCurrentPage(const Value: integer);
var
  i, j, k: integer;
  start: integer;
  obj: TObject;
  clazz: TClass;
  Pplst: PPropList;
  Classtypeinfo: PTypeInfo;
  classDataInfo: PTypeData;
  ppName: string;
  tk: TTypeKind;
begin
  if not FActive then
    Exit;
  if (Value <= 0) or (Value > PageCount) then
    Exit;
  FCurrentPage := Value; 
  // ��ռ�¼
  for i := 1 to RecordCount do
    Delete; 
  // ȡ��¼
  start := (FCurrentPage - 1) * FRowsPerPage + 1;
  for i := start to start + FRowsPerPage - 1 do
  begin
    FDHQuery.RecNo := i;
    if FDHQuery.RecNo <> i then
      Break;
    clazz := FindClass(Format(DH_CLASS_NAME, [TableName]));
    obj := FDHQuery.get(TableName, PKName, FDHQuery.FieldByName(PKName).Value);
    Append; 
    // �����¼
    for j := 0 to FDHQuery.FieldCount - 1 do
      Fields[j].Value := FDHQuery.Fields[j].Value; 
    // formula ��¼
    if useFormula then
    begin
      // todo : add formula records
      Classtypeinfo := clazz.ClassInfo;
      classDataInfo := GetTypeData(Classtypeinfo);
      if classDataInfo.PropCount <> 0 then
      begin
        GetMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
        try
          GetPropInfos(Classtypeinfo, Pplst);
          for k := 0 to classDataInfo.PropCount - 1 do
          begin
            ppName := pplst[k]^.Name; 
            // check whether formula attr
            if RightStr(ppName, 8) = '_FORMULA' then
            begin
              tk := Pplst[k]^.PropType^.Kind;
              if tk <> tkMethod then
              begin
                if (tk = tkString) or (tk = tkLString) or (tk = tkWString) {$IFDEF UNICODE_STRING} or (tk = tkUString) {$ENDIF} then
                  FieldByName(ppName).AsString := GetStrProp((obj as clazz), ppName);
                if tk = tkInteger then
                  FieldByName(ppName).AsInteger := GetInt64Prop((obj as clazz), ppName);
                if tk = tkFloat then
                  FieldByName(ppName).AsFloat := GetFloatProp((obj as clazz), ppName);
                if tk = tkVariant then
                  FieldByName(ppName).AsVariant := GetVariantProp((obj as clazz), ppName);
              end;
            end;
          end;
        finally
          FreeMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
        end;
      end;
    end;
    Post;
  end;
end;

procedure TCnDHibernateQueryAdvance.SetRowsPerPage(const Value: integer);
begin
  Close;
  if Value <= 0 then
    Exit;
  FRowsPerPage := Value;
end;

procedure TCnDHibernateQueryAdvance.SetSQL(const Value: TStrings);
begin
  Close;
  FDHQuery.SQL.Assign(Value);
end;

function TCnDHibernateQueryAdvance.updateAllData(TableName: string; dataList: TCnPodoList; pkName: string): Boolean;
begin
  Result := FDHQuery.updateAllData(TableName, dataList, pkName);
end;

function TCnDHibernateQueryAdvance.updateData(TableName: string; data: TObject; pkName: string): Boolean;
begin
  Result := FDHQuery.updateData(TableName, data, pkName);
end; 

{$ENDIF SUPPORT_ADO}
end.
