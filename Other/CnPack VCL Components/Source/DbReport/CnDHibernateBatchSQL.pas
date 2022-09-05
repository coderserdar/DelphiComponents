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

unit CnDHibernateBatchSQL; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼�ؼ���
* ��Ԫ���ƣ�����ִ��SQL�ؼ���Ԫ
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
  Classes, SysUtils, DB, ADODB;

type
  TCnOnException = procedure(Sender: TObject; E: Exception) of object;

  TCnOnFinishOne = procedure(Sender: TObject; FinishedSQL: string) of object;

  TCnDHibernateBatchSQL = class(TComponent)
  private
    FConnection: TADOConnection;
    FBatchSQL: TStringList;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    FOnException: TCnOnException;
    FOnFinishOne: TCnOnFinishOne;
    FAbout: string;
    function GetBatchSQL: TStrings;
    procedure SetBatchSQL(const Value: TStrings);
  protected
    FSQLList: TStringList;
    procedure ExtractBatchSQLToSQLList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute;
  published
    property About: string read FAbout write FAbout;
    property Connection: TADOConnection read FConnection write FConnection;
    property BatchSQL: TStrings read GetBatchSQL write SetBatchSQL;
    property BeforeExecute: TNotifyEvent read FBeforeExecute write FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute write FAfterExecute;
    property OnException: TCnOnException read FOnException write FOnException;
    property OnFinishOne: TCnOnFinishOne read FOnFinishOne write FOnFinishOne;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateBatchSQL }

constructor TCnDHibernateBatchSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FBatchSQL := TStringList.Create;
  FSQLList := TStringList.Create;
end;

destructor TCnDHibernateBatchSQL.Destroy;
begin
  FConnection := nil;
  FBatchSQL.Free;
  FSQLList.Free;
  inherited Destroy;
end;

procedure TCnDHibernateBatchSQL.Execute;
var
  i: Integer;
  hql: string;
begin
  if FConnection = nil then
    raise Exception.Create('No ADOConnection found!');
  if not FConnection.Connected then
    FConnection.Open();
  if Assigned(BeforeExecute) then
    BeforeExecute(Self);
  ExtractBatchSQLToSQLList; 
  //
  with TADOQuery.Create(nil) do
  begin
    // todo: exceute the sql
    for i := 0 to FSQLList.Count - 1 do
    begin
      hql := FSQLList[i]; 
      // check whether select, insert, update, delete included.
      if Pos('select', hql) > 0 then
      begin
        // do select
        Close;
        SQL.Text := hql;
        try
          Open;
          if Assigned(OnFinishOne) then
            OnFinishOne(self, hql);
        except
          on E: Exception do
            if Assigned(OnException) then
              OnException(Self, E);
        end;
      end
      else if (Pos('insert', hql) > 0) or (pos('update', hql) > 0) or (Pos('delete', hql) > 0) then
      begin
          // do insert, update, delete
        Close;
        sql.Text := hql;
        try
          ExecSQL;
          if Assigned(OnFinishOne) then
            OnFinishOne(self, hql);
        except
          on E: Exception do
            if Assigned(OnException) then
              OnException(Self, E);
        end;
      end
      else
      begin
          // not matched! exception throws.
        if Assigned(OnException) then
          OnException(self, exception.Create('SQL not contains select, insert, update or delete.'));
      end;
    end;
    close;
    Free;
  end;
  if Assigned(AfterExecute) then
    AfterExecute(Self);
end;

procedure TCnDHibernateBatchSQL.ExtractBatchSQLToSQLList;
var
  str: string;
begin
  FSQLList.Clear;
  str := FBatchSQL.Text; 
  // remove the CR_LF
  str := StringReplace(str, #13#10, EmptyStr, [rfReplaceAll, rfIgnoreCase]); 
  // split the string to list
  FSQLList.Delimiter := '|';
  FSQLList.DelimitedText := str;
end;

function TCnDHibernateBatchSQL.GetBatchSQL: TStrings;
begin
  Result := FBatchSQL;
end;

procedure TCnDHibernateBatchSQL.SetBatchSQL(const Value: TStrings);
begin
  FBatchSQL.Assign(Value);
end; 

{$ENDIF SUPPORT_ADO}
end.
