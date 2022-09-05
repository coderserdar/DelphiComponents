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

unit CnDHibernateImport;
{* |<PRE>
================================================================================
* ������ƣ�CnDHibernate��׼�ؼ���
* ��Ԫ���ƣ����ݵ���ؼ���Ԫ
* ��Ԫ���ߣ�Rarnu (rarnu@cnpack.org)
* ��    ע��
*
*             ���ݵ���ӳ��˵��
* һ����ѧ���ʽ
*	  ֱ��д����ѧ���ʽ���ɣ��������ֶ������㣺
*   ���磺[Area] + 1
* �����ַ������ʽ
*	  �õ����������ַ������ɣ����磺'abc'
* ����GUID ���ʽ
*	  GUID �ж�д�������ԣ������״����ɵ� GUID��
*   ʹ��д���ʽ�����磺GUID(w1)
*	  ���� w1 ������д�����Ϊ 1 �� GUID��������
*   GUID ʱ��ʹ�ö����ʽ�����磺
*   GUID(r1)����ʱϵͳ��� GUID(w1)��ȡ��ֵ��
*   ǰ�������ǣ�GUID(w1)������ڣ�����ʹ��
*   GUID(r1)������ж�� GUID������д��
*   GUID(w2)��GUID(r2)��
* �ġ��ϲ����ʽ
*	  �ɽ� xml �� excel �е��ַ�ֱ�����㣬Ȼ���
*   ���Ľ���������ݿ⡣�ϲ����ʽ������� xml
*   �� excel �е��ֶ�������д����Ϊ�����ż� X-
*   ���ֶ��������磺[X-CityNo]+[X-ProvinceNo]��
* �塢�������ʽ
*	  �������ʽ�����û�ʹ�� if ����������жϣ�
*   �﷨���£�
*	  If([X-No]='Y'):1;
*	  If([X-No]='N'):0;
*	  �������÷ֺŽ�β�����ĳ�����������㣬ϵ
*   ͳ�Զ����� NULL
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
  Windows, Messages, SysUtils, Classes, DB, ADODB, ComObj, CnDHibernateConsts,
  Variants, CnDHibernateBase, CnDHibernateUtils, StrUtils;

type
  TCnOnImport = procedure of object;

  TCnDHibernateImport = class(tcomponent)
  private
    FColumnLine: integer;
    FSkipHead: integer;
    FFileName: string;
    FSheetName: string;
    FMap: TStringList;
    FTableName: string;
    FConnection: TADOConnection;
    FOnImport: TCnOnImport;
    FADOTable: TADOTable;
    FAbout: string;
    function GetMap: TStrings;
    procedure SetMap(const Value: TStrings);
    { Private declarations }
  protected
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Import: Integer;            // ���ص�����������
  published
    property About: string read FAbout write FAbout;
    { excel file name }
    property FileName: string read FFileName write FFileName;
    { excel sheet name }
    property SheetName: string read FSheetName write FSheetName;
    { ����ͷ���ļ��У� }
    property SkipHead: integer read FSkipHead write FSkipHead default 1;
    { �����ڵڼ��У� }
    property ColumnLine: integer read FColumnLine write FColumnLine default 1;
    { excel �������ݱ��ֶε�ӳ�� }
    property Map: TStrings read GetMap write SetMap;
    { connection }
    property Connection: TADOConnection read FConnection write FConnection;
    { table name }
    property TableName: string read FTableName write FTableName;
    { on import }
    property OnImport: TCnOnImport read FOnImport write FOnImport;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnDHibernateImport }

constructor TCnDHibernateImport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnection := nil;
  FMap := TStringList.Create;
  FSkipHead := 1;
  FColumnLine := 1;
end;

destructor TCnDHibernateImport.Destroy;
begin
  FConnection := nil;
  FMap.Free;
  inherited Destroy;
end;

function TCnDHibernateImport.GetMap: TStrings;
begin
  Result := FMap;
end;

function TCnDHibernateImport.Import: Integer;
var
  excel: OleVariant;
  rowCnt: integer;
  i, j: Integer;
  HMap: ICnMap;
  n, v: string;
  cell: string;
  guid, g: string;
  iv: string;
begin
  Result := 0;
  // import data
  if FMap.Count = 0 then
    raise TCnNoMappingException.Create('No mapping data found!');
  if FConnection = nil then
    raise TCnNoConnectionException.Create('No connection found!');
  if FTableName = EmptyStr then
    raise TCnNoTableException.Create('No table name found!');
  if FFileName = EmptyStr then
    raise TCnNoFileException.Create('No excel file found!');
  try
    excel := CreateOleObject('excel.application');
    excel.WorkBooks.Open(fFileName);
  except
    raise TCnNoExcelException.Create('Excel not installed!');
    Exit;
  end;
  // get row count
  if FSheetName = Emptystr then
    raise TCnNoSheetNameException.Create('No sheet name found!');
  rowCnt := excel.WorkSheets[FSheetName].UsedRange.Rows.Count;
  excel.WorkSheets[FSheetName].Activate;

  // create the adotable instance
  FADOTable := TADOTable.Create(nil);
  FADOTable.Connection := FConnection;
  FADOTable.TableName := FTableName;
  FADOTable.Open;
  // formatter of FMap is
  // FieldName=ColumnNumber
  // e.g.
  // CountryName=1
  // others:
  // FieldName=Expression
  // e.g.
  // InDate=GetDate()
  HMap := StringMapToHashMap(TStringList(FMap));
  for i := FSkipHead + 1 to rowCnt do
  begin
    FADOTable.Append;
    for j := 0 to HMap.size - 1 do
    begin
      n := HMap.gettable(j).hashName;
      v := HMap.getTable(j).hashValue;
      if StrToIntDef(v, -1) = -1 then
      begin
        // expression
        if v = 'GetDate()' then
        begin
          // ȡ����
          FADOTable.FieldByName(n).Value := Now;
        end
        else if Pos('GUID', v) > 0 then
        begin
          guid := GenerateGUID;
          g := GuidRW(v, guid, i);
          if g = 'w' then
            FADOTable.FieldByName(n).Value := guid
          else
            FADOTable.FieldByName(n).Value := g;
            // writeLog(table.TableName + ':' + guid + '-' + g);
        end
        else if Pos('select', v) > 0 then
        begin
              // formula �ֶ�����
          FADOTable.FieldByName(n).Value := GetFormulaValue(v, FADOTable);
        end
        else if (Pos('X-', v) > 0) and (Pos('if', v) <= 0) then
        begin
                // todo: �����ֶκϲ�����
          FADOTable.FieldByName(n).Value := ExcelConvert(v, excel, i, HMap, FADOTable);
        end
        else if pos('if', v) > 0 then
        begin
                  // todo: ������������
          FADOTable.FieldByName(n).Value := ExcelEventExpressions(v, excel, i, HMap, FADOTable);
        end
        else
        begin
                  // ȡ���ʽ
          try
            FADOTable.FieldByName(n).Value := GetExpressValue(v, FADOTable);
          except
                    // whether number?
            if (LeftStr(v, 1) = '(') and (RightStr(v, 1) = ')') then
            begin
              iv := Copy(v, 2, Length(v) - 2);
              try
                StrToInt(iv);
                FADOTable.FieldByName(n).Value := StrToInt(iv);
              except
                        // whether string
                if LeftStr(v, 2) = '(''' then
                  v := RightStr(v, Length(v) - 2);
                if RightStr(v, 2) = ''')' then
                  v := LeftStr(v, Length(v) - 2);
                FADOTable.FieldByName(n).Value := v;
              end;
            end;
          end;
        end;
      end
      else
      begin
        // field
        cell := excel.Cells[i, strToInt(v)].Value;
        FADOTable.FieldByName(n).Value := Variant(cell);
      end;
    end;
    try
      FADOTable.Post;
      Inc(Result);
      if Assigned(OnImport) then
        OnImport;
    except

    end;
  end;
  FADOTable.Close;
  FADOTable.Free;
  excel.quit;
  excel := Unassigned;
end;

procedure TCnDHibernateImport.SetMap(const Value: TStrings);
begin
  FMap.Assign(Value);
end;

{$ENDIF SUPPORT_ADO}
end.
