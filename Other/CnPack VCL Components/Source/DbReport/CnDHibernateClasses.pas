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

unit CnDHibernateClasses; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼�ؼ���
* ��Ԫ���ƣ������ؼ���/���൥Ԫ
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

{$IFDEF SUPPORT_ADO}

uses
  Classes, SysUtils, DB, ADODB, CnDHibernateConsts, CnDHibernateBase,
  CnDHibernatePodoList, TypInfo, StrUtils, Dialogs;

type

  { Id generator }

  TCnIdGenerator = class(TADOQuery)
  private
    FTableName: string;
    FIdSeed: string;
    FCodeLength: integer;
    FIdFieldName: string;
    FSeedFieldName: string;
  protected
    function getId(idStr: string): string;
  public
    function getNextId: string;
  public
    { table's name }
    property TableName: string read FTableName write FTableName; 
    { identifier of the id }
    property IdSeed: string read FIdSeed write FIdSeed; 
    { identifier seed column name }
    property SeedFieldName: string read FSeedFieldName write FSeedFieldName; 
    { identifier column name }
    property IdFieldName: string read FIdFieldName write FIdFieldName; 
    { the code's length, expected the prefix }
    property CodeLength: integer read FCodeLength write FCodeLength;
  end;

  TCnDHibernateIdGenerator = class(TCnIdGenerator)
  private
    FAbout: string;
  published
    property About: string read FAbout write FAbout;
    property Connection;
    property TableName;
    property IdSeed;
    property SeedFieldName;
    property IdFieldName;
    property CodeLength;
  end; 

  { TCnDHibernateQuery }

  TCnDHibernateQuery = class(TADOQuery)
  private
    FIdGenerator: TCnIdGenerator;
    FAbout: string;
  protected
    procedure setFormulaProperties(TableName: string; pkName: string; pkValue: string; out dataObj: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { search data by stringbuffer and map }
    procedure find(hql: string; param: ICnMap); 
    { get one data object }
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
    property IdGenerator: TCnIdGenerator read FIdGenerator write FIdGenerator;
    property About: string read FAbout write FAbout;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnIdGenerator }

function TCnIdGenerator.getId(idStr: string): string;
var
  iint: Integer;
  i: Integer;
  ret: string;
begin
  iint := StrToInt(idStr);
  Inc(iint);
  ret := IntToStr(iint);
  for i := Length(ret)to CodeLength - 1 do
    ret := '0' + ret;
  Result := ret;
end;

function TCnIdGenerator.getNextId: string;
var
  id: string;
  code: string;
begin
  // generate the id
  // format of year-month-day-code
  Close;
  SQL.Text := Format(DH_ID_GENERATOR, [TableName, SeedFieldName, IdSeed]);
  Open;
  if recordcount = 0 then
  begin
    id := FormatDateTime('yyyymmdd', Now) + getId('0');
    Append;
  end
  else
  begin
    id := FieldByName(IdFieldName).AsString; 
    // ���
    code := Copy(id, Length(id) - CodeLength + 1, CodeLength); 
    // �ж��������赱��
    if FormatDateTime('yyyymmdd', Now) <> Copy(id, 1, Length(id) - codelength) then
      id := FormatDateTime('yyyymmdd', Now) + getId('0')
    else
      id := FormatDateTime('yyyymmdd', Now) + getId(code);
    Edit;
  end; 
  // ���� id
  FieldByName(SeedFieldName).AsString := IdSeed;
  FieldByName(IdFieldName).AsString := id;
  Post;
  Result := id;
end; 

{ TCnDHibernateQuery }

constructor TCnDHibernateQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IdGenerator := TCnIdGenerator.Create(nil);
  IdGenerator.Connection := self.Connection;
end;

function TCnDHibernateQuery.deleteAllData(TableName: string; dataList: TCnPodoList): Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to dataList.Count - 1 do
  begin
    if not deleteData(TableName, dataList.Objects[i]) then
      Result := False;
  end;
end;

function TCnDHibernateQuery.deleteData(TableName: string; data: TObject): Boolean;
var
  map: ICnMap;
  hql: string;
begin
  map := getPodoProperties(TableName, data);
  hql := Format(DH_DELETE_RECORD, [TableName]) + getSearchEvent(map);
  with TADOQuery.Create(nil) do
  begin
    Connection := Self.Connection;
    SQL.Text := hql;
    try
      ExecSQL;
      Result := True;
    except
      on E: Exception do
      begin
        raise Exception.Create(E.Message);
      end;
    end;
    Free;
  end;
end;

destructor TCnDHibernateQuery.Destroy;
begin
  IdGenerator.Free;
  inherited Destroy;
end;

procedure TCnDHibernateQuery.find(hql: string; param: ICnMap);
var
  i: Integer;
begin
  self.Close;
  Self.SQL.Text := hql;
  for i := 0 to param.size - 1 do
  begin
    Self.Parameters.ParamValues[param.getTable(i)^.hashName] := param.getTable(i)^.hashValue;
  end;
  self.Open;
end;

function TCnDHibernateQuery.get(TableName: string; pkName: string; pkValue: Variant): TObject;
var
  clazz: TClass;
  obj: TObject;
  Pplst: PPropList;
  Classtypeinfo: PTypeInfo;
  classDataInfo: PTypeData;
  tempQuery: TADOQuery;
  i: Integer;
  tk: TTypeKind;
begin
  clazz := FindClass(Format(DH_CLASS_NAME, [TableName]));
  obj := clazz.NewInstance;
  tempQuery := TADOQuery.Create(nil);
  with tempQuery do
  begin
    Connection := Self.Connection;
    SQL.Text := Format(DH_GET_RECORD, [TableName, pkName, pkName]);
    Parameters.ParamValues[pkName] := pkValue;
    Open;
    Classtypeinfo := clazz.ClassInfo;
    classDataInfo := GetTypeData(Classtypeinfo);
    if classDataInfo.PropCount <> 0 then
    begin
      GetMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
      try
        GetPropInfos(Classtypeinfo, Pplst);
        for i := 0 to classDataInfo.PropCount - 1 do
        begin
          if (RightStr(Pplst[i]^.Name, 8) = '_FORMULA') or (RightStr(pplst[i]^.Name, 4) = '_SQL') then
            Continue;
          tk := Pplst[i]^.PropType^.Kind;
          if tk <> tkMethod then
          begin
            // set the string properties
            if (tk = tkString) or (tk = tkLString) or (tk = tkWString) {$IFDEF UNICODE_STRING} or (tk = tkUString) {$ENDIF} then
            begin
              SetStrProp((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), FieldByName({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name)).AsString);
            end; 
            // set the integer properties
            if tk = tkInteger then
            begin
              try
                SetInt64Prop((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), FieldByName({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name)).AsInteger);
              except
                SetInt64Prop((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), 0);
              end;
            end; 
            // set the float properties
            if tk = tkFloat then
            begin
              try
                SetFloatProp((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), FieldByName({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name)).AsFloat);
              except
                SetFloatProp((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), 0);
              end;
            end; 
            // set the variant properties
            if tk = tkVariant then
            begin
              SetVariantProp((obj as clazz), {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name), FieldByName({$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name)).Value);
            end;
          end;
        end;
      finally
        FreeMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
      end;
    end;
    Close;
    Free;
  end;
  setFormulaProperties(TableName, pkName, pkValue, obj);
  Result := obj;
end;

function TCnDHibernateQuery.saveAllData(TableName: string; dataList: TCnPodoList): Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to dataList.Count - 1 do
  begin
    if not saveData(TableName, dataList.Objects[i]) then
      Result := False;
  end;
end;

function TCnDHibernateQuery.saveData(TableName: string; data: TObject): Boolean;
var
  map: ICnMap;
  i: Integer;
  hql: string;
begin
  map := getPodoProperties(TableName, data);
  hql := Format(DH_SELECT, [TableName]);
  with TADOQuery.Create(nil) do
  begin
    Connection := self.Connection;
    SQL.Text := hql;
    Open;
    Append; 
    // д������
    for i := 0 to map.size - 1 do
    begin
      FieldByName(map.getTable(i).hashName).Value := map.getTable(i).hashValue;
    end;
    try
      Post;
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

function TCnDHibernateQuery.saveOrUpdateAllData(TableName: string; dataList: TCnPodoList; pkName: string): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to dataList.Count - 1 do
  begin
    if not saveOrUpdateData(TableName, dataList.Objects[i], pkName) then
      Result := False;
  end;
end;

function TCnDHibernateQuery.saveOrUpdateData(TableName: string; data: TObject; pkName: string): Boolean;
var
  map: ICnMap;
  i: Integer;
  hql: string;
begin
  map := getPodoProperties(TableName, data);
  hql := Format(DH_ID_GENERATOR, [TableName, pkName, map.get(pkName)]);
  with TADOQuery.Create(nil) do
  begin
    Connection := Self.Connection;
    SQL.Text := hql;
    Open;
    if recordcount = 0 then
      Append
    else
      Edit;
    for i := 0 to map.size - 1 do
    begin
      // ShowMessage(map.getTable(i).hashName+':'+string(map.getTable(i).hashValue));
      FieldByName(map.getTable(i).hashName).Value := map.getTable(i).hashValue;
    end;
    try
      Post;
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end;

procedure TCnDHibernateQuery.setFormulaProperties(TableName, pkName, pkValue: string; out dataObj: TObject);
var
  clazz: TClass;
  obj: TObject;
  Pplst: PPropList;
  Classtypeinfo: PTypeInfo;
  classDataInfo: PTypeData;
  i: Integer;
  tk: TTypeKind;
  ppName: string;
  fsql: string;
begin
  clazz := FindClass(Format(DH_CLASS_NAME, [TableName]));
  obj := dataObj;
  Classtypeinfo := clazz.ClassInfo;
  classDataInfo := GetTypeData(Classtypeinfo);
  if classDataInfo.PropCount <> 0 then
  begin
    GetMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
    try
      GetPropInfos(Classtypeinfo, Pplst);
      for i := 0 to classDataInfo.PropCount - 1 do
      begin
        ppName := {$IFDEF UNICODE}String{$ENDIF}(pplst[i]^.Name);
        // check whether formula attr
        if RightStr(ppName, 4) = '_SQL' then
        begin
          with TADOQuery.Create(nil) do
          begin
            Connection := self.Connection;
            fsql := GetStrProp((obj as clazz), ppName);
            SQL.Text := fsql;
            Open;
            tk := Pplst[i]^.PropType^.Kind;
            if tk <> tkMethod then
            begin
              // set the string properties
              if (tk = tkString) or (tk = tkLString) or (tk = tkWString) {$IFDEF UNICODE_STRING} or (tk = tkUString) {$ENDIF} then
              begin
                SetStrProp((obj as clazz), LeftStr(ppName, Length(ppName) - 4) + '_FORMULA', Fields[0].AsString);
              end; 
              // set the integer properties
              if tk = tkInteger then
              begin
                try
                  SetInt64Prop((obj as clazz), LeftStr(ppName, Length(ppName) - 4) + '_FORMULA', Fields[0].AsInteger);
                except
                  SetInt64Prop((obj as clazz), LeftStr(ppName, Length(ppName) - 4) + '_FORMULA', 0);
                end;
              end; 
              // set the float properties
              if tk = tkFloat then
              begin
                try
                  SetFloatProp((obj as clazz), LeftStr(ppName, Length(ppName) - 4) + '_FORMULA', Fields[0].AsFloat);
                except
                  SetFloatProp((obj as clazz), LeftStr(ppName, Length(ppName) - 4) + '_FORMULA', 0);
                end;
              end; 
              // set the variant properties
              if tk = tkVariant then
              begin
                SetVariantProp((obj as clazz), LeftStr(ppName, Length(ppName) - 4) + '_FORMULA', Fields[0].Value);
              end;
            end;
            Close;
            Free;
          end;
        end;
      end;
    finally
      FreeMem(Pplst, sizeof(PpropInfo) * classDataInfo.PropCount);
    end;
  end;
end;

function TCnDHibernateQuery.updateAllData(TableName: string; dataList: TCnPodoList; pkName: string): Boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to dataList.Count - 1 do
  begin
    if not updateData(TableName, dataList.Objects[i], pkName) then
      Result := False;
  end;
end;

function TCnDHibernateQuery.updateData(TableName: string; data: TObject; pkName: string): Boolean;
var
  map: ICnMap;
  hql: string;
  i: integer;
begin
  map := getPodoProperties(TableName, data);
  hql := Format(DH_ID_GENERATOR, [TableName, pkName, map.get(pkName)]);
  with TADOQuery.Create(nil) do
  begin
    Connection := Self.Connection;
    SQL.Text := hql;
    Open;
    if recordcount = 0 then
    begin
      Result := False;
      Free;
      Exit;
    end;
    Edit;
    for i := 0 to map.size - 1 do
    begin
      FieldByName(map.getTable(i).hashName).Value := map.getTable(i).hashValue;
    end;
    try
      Post;
      Result := True;
    except
      Result := False;
    end;
    Free;
  end;
end; 

{$ENDIF SUPPORT_ADO}
end.
