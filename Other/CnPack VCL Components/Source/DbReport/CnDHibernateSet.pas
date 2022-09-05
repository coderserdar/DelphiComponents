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

unit CnDHibernateSet; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼���
* ��Ԫ���ƣ�Set�൥Ԫ
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

type
  ICnSet = interface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function add(item: TObject): Boolean; stdcall;
    function addAll(items: TCnPodoList): Boolean; stdcall;
    function remove(item: TObject): Boolean; stdcall;
    function removeAll(items: TCnPodoList): Boolean; stdcall;
    procedure clear; stdcall;
    function size: Integer; stdcall;
    function toArray: TCnPodoList; stdcall;
  end;

  TCnHashSet = class(TObject, ICnSet)
  private
    FObjectList: array of TObject;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function add(item: TObject): Boolean; stdcall;
    function addAll(items: TCnPodoList): Boolean; stdcall;
    function remove(item: TObject): Boolean; stdcall;
    function removeAll(items: TCnPodoList): Boolean; stdcall;
    procedure clear; stdcall;
    function size: Integer; stdcall;
    function toArray: TCnPodoList; stdcall;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnHashSet }

function TCnHashSet._AddRef: Integer;
begin
  Result := -1;
end;

function TCnHashSet._Release: Integer;
begin
  Result := -1;
end;

function TCnHashSet.add(item: TObject): Boolean;
var
  i: Integer;
  len: Integer;
begin
  len := Length(FObjectList);
  for i := 0 to len - 1 do
  begin
    if FObjectList[i] = item then
    begin
      // already exists
      Result := False;
      Exit;
    end;
  end; 
  // add
  SetLength(FObjectList, len + 1);
  FObjectList[len] := item;
  Result := True;
end;

function TCnHashSet.addAll(items: TCnPodoList): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to items.Count - 1 do
  begin
    if not add(items.Objects[i]) then
      Result := False;
  end;
end;

procedure TCnHashSet.clear;
begin
  SetLength(FObjectList, 0);
end;

function TCnHashSet.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TCnHashSet.remove(item: TObject): Boolean;
var
  i, j: Integer;
begin
  Result := False;
  for i := 0 to Length(FObjectList) - 1 do
  begin
    if FObjectList[i] = item then
    begin
      for j := i to Length(FObjectList) - 2 do
        FObjectList[j] := FObjectList[j + 1];
      SetLength(FObjectList, Length(FObjectList) - 1);
      Result := True;
      Break;
    end;
  end;
end;

function TCnHashSet.removeAll(items: TCnPodoList): Boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 0 to items.Count - 1 do
  begin
    if not remove(items.Objects[i]) then
      Result := False;
  end;
end;

function TCnHashSet.size: Integer;
begin
  Result := Length(FObjectList);
end;

function TCnHashSet.toArray: TCnPodoList;
var
  i: Integer;
begin
  Result := TCnPodoList.Create;
  for i := 0 to Length(FObjectList) - 1 do
    Result.Add(FObjectList[i]);
end;

constructor TCnHashSet.Create;
begin
  SetLength(FObjectList, 0);
end;

destructor TCnHashSet.Destroy;
begin
  SetLength(FObjectList, 0);
  inherited Destroy;
end; 

{$ENDIF SUPPORT_ADO}
end.
