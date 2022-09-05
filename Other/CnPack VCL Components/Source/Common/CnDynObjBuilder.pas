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

unit CnDynObjBuilder;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�ͨ���ַ������ݣ���̬�Ľ�������
* ��Ԫ���ߣ�Eric Wang Email: Eric@SimpleDataAccess.net
* ��    ע��
* ����ƽ̨��
* ���ݲ��ԣ���δ����
* �� �� �����õ�Ԫ���豾�ػ�����
* �޸ļ�¼��2010.02.04 V1.1
*               ��ֲ�� CnVCL
*           2008.03.17 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

uses Classes, TypInfo;

type
  TCollectionClass = class of TCollection;

  //��̬���󹹽���
  TCnDynamicBuilder = class
  public
    class function BuildCollection(ClassName: string; CollectionItemClass:
      TCollectionItemClass): TCollection;
    class function BuildCollectionItem(ClassName: string; Collection:
      TCollection): TCollectionItem;
    class function BuildComponent(const ClassName: string; const AOwner:
      TComponent = nil): TComponent;
    class function BuildPersistent(ClassName: string): TPersistent;
  end;

implementation

{ TCnDynamicBuilder }

class function TCnDynamicBuilder.BuildCollection(ClassName: string;
  CollectionItemClass: TCollectionItemClass): TCollection;
begin
  Result := TCollectionClass(FindClass(ClassName)).Create(CollectionItemClass);
end;

class function TCnDynamicBuilder.BuildCollectionItem(ClassName: string;
  Collection: TCollection): TCollectionItem;
begin
  Result := TCollectionItemClass(FindClass(ClassName)).Create(Collection);
end;

class function TCnDynamicBuilder.BuildComponent(const ClassName: string; const
  AOwner: TComponent = nil): TComponent;
begin
  Result := TComponentClass(FindClass(ClassName)).Create(AOwner);
end;

class function TCnDynamicBuilder.BuildPersistent(ClassName: string): TPersistent;
begin
  Result := TPersistentClass(FindClass(ClassName)).Create;
end;

end.
