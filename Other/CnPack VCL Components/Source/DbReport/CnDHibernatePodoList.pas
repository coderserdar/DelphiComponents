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

unit CnDHibernatePodoList;
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼���
* ��Ԫ���ƣ�PODO ��/���൥Ԫ
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
*
*      ʹ��ʾ����
*
*      objList := TRaObjList.Create;
*      obj := 'obj';
*      obj.add('id', TObject(obj));
*      showMessage(string(obj.getObjectByName('id')));
*
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
  Classes, SysUtils;

const
  // �쳣�ַ���
  OUT_OF_OBJECT_INDEX_EXCEPTION_STRING = '���������б��±�';
  NAME_NOT_FOUND_EXCEPTION_STRING = '���� id ������';

  (******************************************************************************)

type
  // �쳣����
  // ���쳣���ж� index ʱʹ��
  // ��� index ��Χ���������׳����쳣
  TCnOutOfObjectIndexException = Exception;

  // ���쳣���ж϶��� id ʱʹ��
  // �������û�� id�����׳����쳣
  TCnNameNotFoundException = Exception;

  (******************************************************************************)

    // �����¼����
    // �˼�¼�� hash ���һ�����
    // ����ͨ�� ���� ���� ����
    // ���ƿ��Բ�Ψһ
    // ���Ǹ������Ʋ��Ҷ���ʱ
    // ����������ظ�����ôֻ��ȡ����һ��ƥ���
    // �������Ϊ�գ�ֻ��ͨ�� index ȡ������
  TCnObjectRec = record
    // ����
    id: string;
    // ����
    obj: TObject;
  end;                                   { TCnObjectRec }

  (******************************************************************************)

    // �����б�������
    // �ö������ڱ�������б�
    // ���ҿ���ȡ������

  TCnPodoList = class
  private

    // �����б�
    // ���б�ʹ�� TCnObjectRec ��¼����
    // �γ� hash ��Ľṹ
    // ʵ�ʲ���ʱ��ʹ�õ�����ֵ��
    objList: array of TCnObjectRec;

    // ��ȡָ����ŵĶ���
    // �󶨵� Objects ����
    function GetObject(index: integer): TObject;
    procedure SetObject(index: integer; obj: TObject);
    // ��ȡ id �б�
    // �󶨵� IdList ����
    function getIdList: TStringList;
    function GetCount: integer;
  protected
  public
    // ����б�
    procedure Clear;
    // ��Ӷ���
    procedure Add(obj: TObject); overload;
    // ��Ӷ��󣬴��� id
    procedure Add(Id: string; obj: TObject); overload;
    // �� index ���������
    procedure Insert(index: Integer; obj: TObject); overload;
    // �� index ��������󣬴��� id
    procedure Insert(index: Integer; id: string; obj: TObject); overload;
    // �Ƴ� index ���Ķ���
    procedure Remove(index: Integer); overload;
    // �Ƴ� obj �� ����
    function Remove(obj: TObject): boolean; overload;
    // ��ȡ��һ����������
    function IndexOf(obj: TObject): integer;
    // ��ȡ����������
    function LastIndexOf(obj: TObject): integer;
    // �������ƻ�ȡ����
    function getObjectByName(ObjName: string): TObject;
    // �ƶ�
    procedure move(oldIndex, newIndex: integer);
  public
    // ��������
    property Objects[index: integer]: TObject read GetObject write SetObject;
    // �����б�
    property IdList: TStringList read getIdList;
    // ������
    property Count: integer read GetCount;
  end;                                   

  (******************************************************************************)

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnPodoList }

/// <summary>
/// ��Ӷ����б�
/// </summary>
/// <param name="obj">����</param>
procedure TCnPodoList.Add(obj: TObject);
var
  id: string;
begin
  // �� id Ϊ��
  id := EmptyStr;
  // ���ô� 2 ����������ӷ���
  Add(id, obj);
end;                                     { Add }

/// <summary>
/// ��Ӷ����б����� id
/// </summary>
/// <param name="Id">��ʶ</param>
/// <param name="obj">����</param>

procedure TCnPodoList.Add(Id: string; obj: TObject);
var
  Len: Integer;
begin
  // ��ȡ�б���
  Len := Length(objList);
  // ���ó��� + 1
  SetLength(objList, Len + 1);
  // Ϊ hash ��ֵ
  objList[Len].id := Id;
  objList[Len].obj := obj;
end;                                     { Add }

(******************************************************************************)

/// <summary>
/// ��ն����б�
/// </summary>

procedure TCnPodoList.Clear;
begin
  // �����еĶ����ÿ�
  // �ڲ���������
  SetLength(objList, 0);
end;                                     { Clear }

function TCnPodoList.GetCount: integer;
begin
  Result := Length(objList);
end;                                     { GetCount }

(******************************************************************************)

/// <summary>
/// ��ȡ id �б�
/// </summary>
/// <returns>�б�</returns>

function TCnPodoList.getIdList: TStringList;
var
  i: Integer;
begin
  // ���� id �б�ʵ��
  Result := TStringList.Create;
  // �������б��еĶ��� id ������ӵ� id �б���
  for i := 0 to Length(objList) - 1 do
    Result.Add(objlist[i].id);
end;                                     { getIdList }

(******************************************************************************)

/// <summary>
/// �������ƻ�ȡ����
/// </summary>
/// <param name="ObjName">��������</param>
/// <returns>����</returns>

function TCnPodoList.getObjectByName(ObjName: string): TObject;
var
  i: Integer;
begin
  // �ж������Ƿ����
  if ObjName = EmptyStr then
  begin
    // ������������׳��쳣
    raise TCnNameNotFoundException.Create(NAME_NOT_FOUND_EXCEPTION_STRING);
    Exit;
  end;                                   { if }

  // ��ʼ�����ض���
  Result := nil;
  for i := 0 to Length(objList) - 1 do
  begin
    // �������� id �봫���������ͬ
    if objList[i].id = ObjName then
    begin
      // ���ش˶��󲢽���ѭ��
      Result := objlist[i].obj;
      Break;
    end;                                 { if }
  end;                                   { for }
end;                                     { getObjectByName }

(******************************************************************************)

/// <summary>
/// ��ȡָ����ŵĶ���
/// </summary>
/// <param name="index">���</param>
/// <returns>����</returns>

function TCnPodoList.GetObject(index: integer): TObject;
begin
  // �ж�����Ƿ񳬳��±�
  if (index < 0) or (index > Length(objList)) then
  begin
    // �����������׳��쳣
    raise TCnOutOfObjectIndexException.Create(OUT_OF_OBJECT_INDEX_EXCEPTION_STRING);
    Exit;
  end;                                   { if }
  // ���ض���
  Result := objlist[index].obj;
end;
                                  { GetObject }
(******************************************************************************)

/// <summary>
/// ����ָ����ŵĶ���
/// </summary>
/// <param name="index">���</param>
/// <param name="obj">����</param>
// ���� SetObject by ���� 1.6 ��

procedure TCnPodoList.SetObject(index: integer; obj: TObject);
begin
  // �ж�����Ƿ񳬳��±�
  if (index < 0) or (index > Length(objList)) then
  begin
    // �����������׳��쳣
    raise TCnOutOfObjectIndexException.Create(OUT_OF_OBJECT_INDEX_EXCEPTION_STRING);
    Exit;
  end;                                   { if }
  // ���ض���
  objlist[index].obj := obj;
end;
                                  { GetObject }
(******************************************************************************)

/// <summary>
/// ��ȡָ���Ķ������
/// </summary>
/// <param name="obj">����</param>
/// <returns>���</returns>

function TCnPodoList.IndexOf(obj: TObject): integer;
var
  i: Integer;
begin
  // ��ʼ������ֵΪ -1
  // �˷���ֵ��ʾδ�ҵ���صĶ���
  Result := -1;
  for i := 0 to Length(objList) - 1 do
  begin
    // ��������б����д˶���
    if objList[i].obj = obj then
    begin
      // ���ض������Ų�����ѭ��
      Result := i;
      Break;
    end;                                 { if }
  end;                                   { for }
end;                                     { IndexOf }


(******************************************************************************)

/// <summary>
/// ��ȡ���һ���������
/// </summary>
/// <param name="obj">����</param>
/// <returns>���</returns>
// ���� LastIndexOf  by ���� 1.6 ��

function TCnPodoList.LastIndexOf(obj: TObject): integer;
var
  i: Integer;
begin
  // ��ʼ������ֵΪ -1
  // �˷���ֵ��ʾδ�ҵ���صĶ���
  Result := -1;
  for i := Length(objList) - 1 downto 0 do
  begin
    // ��������б����д˶���
    if objList[i].obj = obj then
    begin
      // ���ض������Ų�����ѭ��
      Result := i;
      Break;
    end;                                 { if }
  end;                                   { for }
end;

(******************************************************************************)

/// <summary>
/// �� index ��������󣬴��� id
/// </summary>
/// <param name="index">���</param>
/// <param name="id">��ʶ</param>
/// <param name="obj">����</param>
procedure TCnPodoList.Insert(index: Integer; id: string; obj: TObject);
var
  i: Integer;
  len: Integer;
begin
  // ������С���㣬�����㴦����
  if index < 0 then
    index := 0;

  // �����Ŵ����б���
  // �����б�������
  if index > Length(objList) - 1 then
  begin
    Add(id, obj);
    Exit;
  end;                                   { if }

  // ���г��� + 1
  len := Length(objList);
  SetLength(objList, len + 1);

  // �������˳��
  for i := len downto index + 1 do
  begin
    objList[i].id := objList[i - 1].id;
    objList[i].obj := objList[i - 1].obj;
  end;                                   { for }

  // ��������뵽 index ��
  objList[index].id := id;
  objList[index].obj := obj;
end;                                     { Insert }

/// <summary>
/// ���������Ŵ�
/// </summary>
/// <param name="index">���</param>
/// <param name="obj">����</param>

procedure TCnPodoList.Insert(index: Integer; obj: TObject);
var
  id: string;
begin
  // �� id �ÿ�
  id := EmptyStr;
  // ���ô� 3 �������Ĳ��뷽��
  Insert(index, id, obj);
end;

(******************************************************************************)

/// <summary>
/// �������Ƴ�����
/// </summary>
/// <param name="index">���</param>
procedure TCnPodoList.Remove(index: Integer);
var
  i: Integer;
begin
  // �ж�����Ƿ񳬳��±�
  if (index < 0) or (index > Length(objList)) then
  begin
    // �����������׳��쳣
    raise TCnOutOfObjectIndexException.Create(OUT_OF_OBJECT_INDEX_EXCEPTION_STRING);
    Exit;
  end;                                   { if }

  // �Ƴ�����
  // ����Ķ�������������
  for i := index to Length(objList) - 2 do
  begin
    objList[i].id := objlist[i + 1].id;
    objList[i].obj := objlist[i + 1].obj;
  end;                                   { for }

  // �����һ������ȥ��
  SetLength(objList, Length(objList) - 1);
end;

(******************************************************************************)

/// <summary>
/// ��objcet�Ƴ�����
/// </summary>
/// <param name="index">����</param>
// ���� Remove by ���� 1.6 ��
function TCnPodoList.Remove(obj: TObject): boolean;
var
  index: Integer;
begin
  index := indexof(obj);
  if index < 0 then
    result := false
  else
  begin
    Remove(index);
    result := true;
  end;
end;

procedure TCnPodoList.move(oldIndex, newIndex: integer);
var
  obj: TObject;
begin
  // get object at old index
  obj := Objects[oldIndex];
  // remove old obj
  Remove(oldIndex);
  // insert the object at the new index
  Insert(newIndex, obj);
end;

{$ENDIF SUPPORT_ADO}
end.
