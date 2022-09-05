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

unit CnDHibernateExport; 
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼�ؼ���
* ��Ԫ���ƣ����ݵ����ؼ���Ԫ
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
  Windows, Messages, SysUtils, Classes, DB, ADODB, ComObj, CnDHibernateBase,
  Variants;

type
  TCnOnExport = procedure of object;

  TCnDHibernateExport = class(tcomponent)
  private
    FSQL: string;
    FConnection: TAdoConnection;
    FExportColumn: boolean;
    FFileName: string;
    FSheetName: string;
    FAdoQuery: TADOQuery;
    FOnExport: TCnOnExport;
    FAbout: string; 
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Export: Integer;
  published
    property About: string read FAbout write FAbout;
    property Connection: TAdoConnection read FConnection write FConnection;
    property SQL: string read FSQL write FSQL; 
    { whether export column name or not }
    property ExportColumn: boolean read FExportColumn write FExportColumn default True;
    property FileName: string read FFileName write FFileName;
    property SheetName: string read FSheetName write FSheetName;
    property OnExport: TCnOnExport read FOnExport write FOnExport;
  end; 

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateExport }

constructor TCnDHibernateExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FExportColumn := True;
end;

destructor TCnDHibernateExport.Destroy;
begin
  FConnection := nil;
  inherited Destroy;
end;

function TCnDHibernateExport.Export: Integer;
var
  i: Integer;
  excel: OleVariant;
  currentRow: Integer;
begin
  Result := 0; 
  // check event
  if FFileName = EmptyStr then
    raise TCnNoFileException.Create('file not found!');
  if FSheetName = EmptyStr then
    raise TCnNoSheetNameException.Create('sheet not found!');
  if FConnection = nil then
    raise TCnNoConnectionException.Create('data connection not found!');
  if FSQL = EmptyStr then
    raise TCnNoSQLException.Create('T-SQL not found!');
  try
    excel := CreateOleObject('excel.application');
    excel.WorkBooks.Open(fFileName);
  except
    raise TCnNoExcelException.Create('Excel not installed!');
    Exit;
  end;
  excel.WorkSheets[FSheetName].Activate; 
  
  // create ds
  FAdoQuery := TADOQuery.Create(nil);
  FAdoQuery.Connection := FConnection;
  FAdoQuery.SQL.Text := FSQL;
  FAdoQuery.Open;
  currentRow := 1;
  if FExportColumn then
  begin
    // export column name
    for i := 0 to FAdoQuery.FieldCount - 1 do
    begin
      excel.Cells[currentRow, i + 1].Value := FAdoQuery.Fields[i].FieldName;
    end;
    Inc(currentRow);
  end; 
  // export data
  FAdoQuery.First;
  while not FAdoQuery.Eof do
  begin
    for i := 0 to FAdoQuery.FieldCount - 1 do
    begin
      excel.cells[currentRow, i + 1].value := FAdoQuery.Fields[i].Value;
    end;
    Inc(Result);
    if Assigned(OnExport) then
      OnExport;
    Inc(currentRow);
    FAdoQuery.Next;
  end;
  FAdoQuery.Close;
  FAdoQuery.Free; 
  // excel.WorkBooks.Close(SaveChanges:=True);
  excel.save;
  excel.quit;
  excel := Unassigned;
end; 

{$ENDIF SUPPORT_ADO}
end.
