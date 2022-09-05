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

unit CnDHibernateSubQuery;
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼�ؼ���
* ��Ԫ���ƣ����ӱ��Ӳ�ѯ�ؼ���Ԫ
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
  Windows, Messages, SysUtils, Classes, DB, ADODB, CnDHibernateClasses,
  CnDHibernateBase, CnDHibernateSet, CnDHibernatePodoList, CnDHibernateConsts;

type
  TCnDHibernateSubQuery = class(TCnDHibernateQuery)
  private
    FMainTableName: string;
    FMainTablePK: string;
    FMainTablePKValue: Variant;
    FSubTableRefField: string;
    FSubTableName: string;
    FSubTablePKName: string;
    FAbout: string;
    procedure SetMainTablePKValue(const Value: Variant);
    procedure SetSubTableRefField(const Value: string);
  protected
    procedure findSubDetail;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function saveDetail(items: ICnSet): Boolean;
    function updateDetail(items: ICnSet): Boolean;
    function deleteDetail(items: ICnSet): Boolean;
    function saveOrUpdateDetail(items: ICnSet): Boolean;
  published
    property About: string read FAbout write FAbout;
    property MainTableName: string read FMainTableName write FMainTableName;
    property MainTablePK: string read FMainTablePK write FMainTablePK;
    property MainTablePKValue: Variant read FMainTablePKValue write
      SetMainTablePKValue;
    property SubTableRefField: string read FSubTableRefField write
      SetSubTableRefField;
    property SubTableName: string read FSubTableName write FSubTableName;
    property SubTablePKName: string read FSubTablePKName write FSubTablePKName;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateSubQuery }

constructor TCnDHibernateSubQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

function TCnDHibernateSubQuery.deleteDetail(items: ICnSet): Boolean;
var
  i                 : Integer;
  podos             : TCnPodoList;
begin
  Result := True;
  podos := items.toArray;
  for i := 0 to podos.Count - 1 do
  begin
    if not deleteData(FSubTableName, podos.Objects[i]) then
      Result := False;
  end;
end;

destructor TCnDHibernateSubQuery.Destroy;
begin

  inherited Destroy;
end;

procedure TCnDHibernateSubQuery.findSubDetail;
var
  hql               : TCnStringBuffer;
  param             : ICnMap;
begin
  if (FMainTableName = EmptyStr) or (FMainTablePK = EmptyStr) or
    (FMainTablePKValue = EmptyStr) or (FSubTableRefField = EmptyStr)
    or (FSubTableName = EmptyStr) then
    Exit;
  hql := TCnStringBuffer.Create(Format(DH_GET_RECORD, [FSubTableName,
    FSubTableRefField, FSubTableRefField]));
  param := TCnDHHashMap.Create;
  param.put(FSubTableRefField, FMainTablePKValue);
  Self.find(hql.toString, param);
end;

function TCnDHibernateSubQuery.saveDetail(items: ICnSet): Boolean;
var
  i                 : Integer;
  podos             : TCnPodoList;
begin
  Result := True;
  podos := items.toArray;
  for i := 0 to podos.Count - 1 do
  begin
    if not saveData(FSubTableName, podos.Objects[i]) then
      Result := False;
  end;
end;

function TCnDHibernateSubQuery.saveOrUpdateDetail(items: ICnSet): Boolean;
var
  i                 : Integer;
  podos             : TCnPodoList;
begin
  Result := True;
  podos := items.toArray;
  for i := 0 to podos.Count - 1 do
  begin
    if not saveOrUpdateData(FSubTableName, podos.Objects[i], FSubTablePKName)
      then
      Result := False;
  end;
end;

procedure TCnDHibernateSubQuery.SetMainTablePKValue(const Value: Variant);
begin
  // check whether main table has name and primary key
  if (FMainTableName = EmptyStr) or (FMainTablePK = EmptyStr) then
  begin
    FMainTablePKValue := EmptyStr;
    Exit;
  end;
  FMainTablePKValue := Value;
  if FSubTableRefField <> EmptyStr then
    findSubDetail;
end;

procedure TCnDHibernateSubQuery.SetSubTableRefField(const Value: string);
begin
  FSubTableRefField := Value;
  if (FMainTableName <> EmptyStr) and (FMainTablePK <> EmptyStr) and
    (FMainTablePKValue <> EmptyStr) then
    findSubDetail;
end;

function TCnDHibernateSubQuery.updateDetail(items: ICnSet): Boolean;
var
  i                 : integer;
  podos             : TCnPodoList;
begin
  Result := True;
  podos := items.toArray;
  for i := 0 to podos.Count - 1 do
  begin
    if not updateData(FSubTableName, podos.Objects[i], FSubTablePKName) then
      Result := False;
  end;
end;

{$ENDIF SUPPORT_ADO}
end.
