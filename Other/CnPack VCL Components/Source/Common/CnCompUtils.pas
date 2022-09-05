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

unit CnCompUtils;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ�������ߵ�Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ����ƽ̨��PWinXP SP2 + Delphi 5.01
* ���ݲ��ԣ�
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��    ע��
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Controls;

procedure AddComponentFreeNotify(var AComponent: TComponent; OnFree: TNotifyEvent);
{* ����һ������ͷ�֪ͨ����� OnFree Ϊ nil������ͷ�ʱ�Ὣ AComponent ������Ϊ nil }
procedure RemoveComponentFreeNotify(var AComponent: TComponent);
{* ɾ��һ������ͷ�֪ͨ��}

implementation

type
  PCnCompFreeNotifyRec = ^TCnCompFreeNotifyRec;
  TCnCompFreeNotifyRec = record
    Comp: TComponent;
    PComp: ^TComponent;
    OnFree: TNotifyEvent;
  end;
  
  TCnCompFreeNotifyObj = class(TComponent)
  private
    FList: TList;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Add(var AComponent: TComponent; OnFree: TNotifyEvent): Integer;
    procedure Remove(var AComponent: TComponent);
  end;

{ TCnCompFreeNotifyObj }

function TCnCompFreeNotifyObj.Add(var AComponent: TComponent;
  OnFree: TNotifyEvent): Integer;
var
  P: PCnCompFreeNotifyRec;
begin
  Result := -1;
  if AComponent <> nil then
  begin
    New(P);
    P^.Comp := AComponent;
    P^.PComp := @AComponent;
    P^.OnFree := OnFree;
    Result := FList.Add(P);
    AComponent.FreeNotification(Self);
  end;
end;

constructor TCnCompFreeNotifyObj.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
end;

destructor TCnCompFreeNotifyObj.Destroy;
var
  P: PCnCompFreeNotifyRec;
begin
  while FList.Count > 0 do
  begin
    P := FList.Extract(FList[0]);
    Dispose(P);
  end;
  FList.Free;  
  inherited;
end;

procedure TCnCompFreeNotifyObj.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  i: Integer;
  P: PCnCompFreeNotifyRec;
begin
  inherited;
  if Operation = opRemove then
  begin
    for i := FList.Count - 1 downto 0 do
    begin
      P := FList[i];
      if P^.Comp = AComponent then
      begin
        P^.PComp^ := nil;
        if Assigned(P^.OnFree) then
          P^.OnFree(AComponent);
        Dispose(P);
        FList.Delete(i);
      end;  
    end;
  end;    
end;

procedure TCnCompFreeNotifyObj.Remove(var AComponent: TComponent);
var
  i: Integer;
  P: PCnCompFreeNotifyRec;
begin
  for i := FList.Count - 1 downto 0 do
  begin
    P := FList[i];
    if P^.PComp = @AComponent then
    begin
      Dispose(P);
      FList.Delete(i);
    end;
  end;
end;

var
  FCnCompFreeNotifyObj: TCnCompFreeNotifyObj;

// ����һ������ͷ�֪ͨ����� OnFree Ϊ nil������ͷ�ʱ�Ὣ AComponent ������Ϊ nil
procedure AddComponentFreeNotify(var AComponent: TComponent; OnFree: TNotifyEvent);
begin
  if FCnCompFreeNotifyObj = nil then
    FCnCompFreeNotifyObj := TCnCompFreeNotifyObj.Create(nil);
  FCnCompFreeNotifyObj.Add(AComponent, OnFree);
end;

// ɾ��һ������ͷ�֪ͨ��
procedure RemoveComponentFreeNotify(var AComponent: TComponent);
begin
  if FCnCompFreeNotifyObj <> nil then
    FCnCompFreeNotifyObj.Remove(AComponent);
end;

initialization

finalization
  if FCnCompFreeNotifyObj <> nil then
    FreeAndNil(FCnCompFreeNotifyObj);

end.
