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

unit CnDHibernateArrayList; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼���
* ��Ԫ���ƣ������б��൥Ԫ
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
  Classes, SysUtils, CnDHibernatePodoList;

const
  // �쳣�ַ���
  OUT_OF_ARRAYLIST_INDEX_EXCEPTION_STRING = '���������б��±�';

type

  // �쳣����
  // ���쳣���ж� index ʱʹ��
  // ��� index ��Χ���������׳����쳣
  TCnOutOfArrayListIndexException = Exception;

  ICnList = interface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure trimToSize; stdcall;
    procedure ensureCapacity(minCapacity: integer); stdcall;
    function size: Integer; stdcall;
    function isEmpty: Boolean; stdcall;
    function contains(elem: TObject): Boolean; stdcall;
    function indexOf(elem: TObject): integer; stdcall;
    function lastIndexOf(elem: TObject): integer; stdcall;
    // function clone: TObject; stdcall;
    function toArray: TCnPodoList; overload; stdcall;
    // function toArray(a: TPodoList): TPodoList; overload; stdcall;
    function _get(index: integer): TObject; stdcall;
    function _set(index: integer; element: TObject): TObject; stdcall;
    procedure add(o: TObject); overload; stdcall;
    procedure add(index: integer; element: TObject); overload; stdcall;
    function remove(index: integer): TObject; overload; stdcall;
    function remove(o: TObject): Boolean; overload; stdcall;
    procedure fastRemove(index: integer); stdcall;
    procedure clear; stdcall;
    procedure addAll(c: TCollection); overload; stdcall;
    procedure addAll(index: integer; c: TCollection); overload; stdcall;
  end;

  TCnArrayList = class(TObject, ICnList)
  private
    FSize: integer;
    FElementData: TCnPodoList;
    procedure fastRemove(index: integer); stdcall;
  protected
    procedure RangeCheck(index: integer);
  public
    constructor Create;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    procedure trimToSize; stdcall;
    procedure ensureCapacity(minCapacity: integer); stdcall;
    function size: Integer; stdcall;
    function isEmpty: Boolean; stdcall;
    function contains(elem: TObject): Boolean; stdcall;
    function indexOf(elem: TObject): integer; stdcall;
    function lastIndexOf(elem: TObject): integer; stdcall;
    // function clone: TObject; stdcall;
    function toArray: TCnPodoList; stdcall;
    function _get(index: integer): TObject; stdcall;
    function _set(index: integer; element: TObject): TObject; stdcall;
    procedure add(o: TObject); overload; stdcall;
    procedure add(index: integer; element: TObject); overload; stdcall;
    function remove(index: integer): TObject; overload; stdcall;
    function remove(o: TObject): Boolean; overload; stdcall;
    procedure clear; stdcall;
    procedure addAll(c: TCollection); overload; stdcall;
    procedure addAll(index: integer; c: TCollection); overload; stdcall;
    //procedure TCnArrayList.removeRange(fromIndex:integer;toIndex:integer);
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

constructor TCnArrayList.Create;
begin

end;

destructor TCnArrayList.Destroy;
begin
  inherited Destroy;
end;

function TCnArrayList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCnArrayList._AddRef: Integer;
begin
  Result := -1;
end;

function TCnArrayList._Release: Integer;
begin
  Result := -1;
end;

(******************************************************************************)
/// .registed
/// <summary>
///  ����Ԫ������
/// </summary>
procedure TCnArrayList.trimToSize;
var
  oldCapacity: integer;
  // oldData           : TPodoList;
  i: integer;
begin
  // modCount := modCount + 1;
  oldCapacity := FElementData.Count;
  if (Fsize < oldCapacity) then
  begin
    if Fsize <> 0 then
    begin
      for i := 0 to(oldCapacity - Fsize - 1) do
        FElementData.Remove(oldCapacity - i);
    end
    else
      FElementData.Clear;
  end
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ??
/// </summary>
/// <param name="minCapacity">??</param>
procedure TCnArrayList.ensureCapacity(minCapacity: integer);
var
  oldCapacity: integer;
  newCapacity: integer;
  i: integer;
begin
  // modCount := modCount + 1;
  oldCapacity := FElementData.Count;
  if minCapacity > oldCapacity then
  begin
    newCapacity := (oldCapacity * 3) div 2 + 1;
    if newCapacity < minCapacity then
      newCapacity := minCapacity;
    for i := 0 to(newCapacity - oldCapacity - 1) do
    begin
      FElementData.Add(nil);
    end;
  end;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ����list������
/// </summary>
/// <returns>����</returns>
function TCnArrayList.size: Integer;
begin
  result := Fsize;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// �Ƿ�Ϊ��
/// </summary>
/// <returns>boolean</returns>
function TCnArrayList.isEmpty: Boolean;
begin
  result := FSize = 0;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ���Ԫ���Ƿ���list��
/// </summary>
/// <param name="elem">Ԫ��</param>
/// <returns>boolean</returns>
function TCnArrayList.contains(elem: TObject): Boolean;
begin
  result := indexOf(elem) > 0;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ����Ԫ����list�����,����һ�γ���Ϊ׼
/// </summary>
/// <param name="elem">Ԫ��</param>
/// <returns>���</returns>
function TCnArrayList.indexOf(elem: TObject): integer;
begin
  Result := -1;
  if elem <> nil then
  begin
    result := FElementData.IndexOf(elem);
  end;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ����Ԫ����list�����,�Ӻ���ǰ��
/// </summary>
/// <param name="elem">Ԫ��</param>
/// <returns>���</returns>
function TCnArrayList.lastIndexOf(elem: TObject): integer;
begin
  Result := -1;
  if elem <> nil then
  begin
    result := FElementData.lastIndexOf(elem);
  end;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ����list
/// </summary>
/// <returns>Ԫ���б�</returns>
function TCnArrayList.toArray: TCnPodoList;
begin
  result := FElementData;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ��ȡָ����Ԫ��
/// </summary>
/// <param name="index">���</param>
/// <returns>Ԫ��</returns>
function TCnArrayList._get(index: integer): TObject;
begin
  RangeCheck(index);
  result := FElementData.Objects[index];
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ����ָ��Ԫ��
/// </summary>
/// <param name="index">���</param>
/// <param name="element">Ԫ��</param>
/// <returns>oldԪ��</returns>
function TCnArrayList._set(index: integer; element: TObject): TObject;
var
  oldValue: TObject;
begin
  RangeCheck(index);
  oldValue := FElementData.Objects[index];
  FElementData.Objects[index] := element;
  result := oldValue;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ����Ԫ��
/// </summary>
/// <param name="o">Ԫ��</param>
/// <returns>oldԪ��</returns>
procedure TCnArrayList.add(o: TObject);
begin
  FElementData.Add(o);
  Fsize := Fsize + 1;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ����Ԫ��
/// </summary>
/// <param name="index">���</param>
/// <param name="element">Ԫ��</param>
procedure TCnArrayList.add(index: integer; element: TObject);
begin
  FElementData.Insert(index, element);
  Fsize := Fsize + 1;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ɾ��Ԫ��
/// </summary>
/// <param name="index">���</param>
/// <returns>oldԪ��</returns>
function TCnArrayList.remove(index: integer): TObject;
var
  oldValue: TObject;
begin
  RangeCheck(index);
  oldValue := FElementData.Objects[index];
  FElementData.Remove(index);
  Fsize := Fsize - 1;
  // modCount := modCount + 1;
  result := oldValue;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ɾ��Ԫ��
/// </summary>
/// <param name="index">���</param>
/// <returns>oldԪ��</returns>
function TCnArrayList.remove(o: TObject): boolean;
begin
  result := FElementData.Remove(o);
  Fsize := Fsize - 1;
  // modCount := modCount + 1;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ɾ��Ԫ��,�޷���ֵ
/// </summary>
/// <param name="index">���</param>
procedure TCnArrayList.fastRemove(index: integer);
begin
  FElementData.Remove(index);
  Fsize := Fsize - 1;
  // modCount := modCount + 1;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// ���
/// </summary>
procedure TCnArrayList.clear;
begin
  FElementData.Clear;
  Fsize := 0;
  // modCount := modCount + 1;
end;

(******************************************************************************)
procedure TCnArrayList.addAll(c: TCollection);
var
  i: integer;
begin
  for i := 0 to c.Count - 1 do
  begin
    FElementData.Add(TObject(c.Items[i]));
  end;
end;

procedure TCnArrayList.addAll(index: integer; c: TCollection);
var
  i: integer;
begin
  for i := 0 to c.Count - 1 do
  begin
    FElementData.Insert(index, TObject(c.Items[i]));
  end;
end;

(******************************************************************************)
/// .registed
/// <summary>
/// �����ŵ���Ч��Χ
/// </summary>
/// <param name="index">���</param>
procedure TCnArrayList.RangeCheck(index: integer);
begin
  if (index < 0) or (index > FElementData.Count - 1) then
  begin
    raise TCnOutOfArrayListIndexException.Create(OUT_OF_ARRAYLIST_INDEX_EXCEPTION_STRING);
    exit;
  end;
end; 

{$ENDIF SUPPORT_ADO}
end.
