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

unit CnSQLAnalyzer;
{* |<PRE>
================================================================================
* ������ƣ�CnPack �����
* ��Ԫ���ƣ���ѯ���������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ������� (appleak46@yahoo.com.cn)
* ��    ע��
* ����ƽ̨��PWinXP + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.11.24 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF SUPPORT_ADO}

uses
  Windows,SysUtils, Classes,Controls, ExtCtrls, DB, ADODb, Graphics, StdCtrls,
  ComCtrls, CnRunSqlFrame, CnDBConsts, CnRunSqlUnit;

type
  TCnSQLAnalyzer = class(TCustomPanel)
  private
    FrameRunSql: TCnFrameRunSql;
    FUseCustomGrid: boolean;
    FOnRunSqlEnd: TRunSqlEvent;
    FSqlLines: TStrings;
    procedure SetConnection(const Value: TADOConnection);
    function GetConnection: TADOConnection;
    procedure SetConnectionString(const Value: string);
    function GetConnectionString: string;
    procedure SetSqlText(const Value: string);
    function GetSqlText: string;
    procedure SetEditorFont(const Value: TFont);
    function GetEditorFont: TFont;
    function GetGridFont: TFont;
    procedure SetGridFont(const Value: TFont);
    function GetMsgFont: TFont;
    procedure SetMsgFont(const Value: TFont);
    procedure SetShowGridPage(const Value: boolean);
    function GetShowGridPage: boolean;
    procedure SetShowToolBar(const Value: boolean);
    function GetShowToolBar: boolean;
    function GetSourceList: TList;
    procedure SetUseCustomGrid(const Value: boolean);
    function GetMsgList: TStrings;
    function GetRunSucced: boolean;
    procedure SetOnRunSqlEnd(const Value: TRunSqlEvent);
    procedure SetSqlLines(const Value: TStrings);
    { Private declarations }
  protected
    { Protected declarations }
    procedure  Loaded; override;
  public
    { Public declarations }
    Constructor Create(AOwner: TComponent);override;
    Destructor  Destroy;override;
    property    SqlText: string read GetSqlText write SetSqlText;
    procedure   LoadFromFile(FileName: string);
    procedure   SaveToFile(FileName: string);
    procedure   RunSql;
    procedure   ParaseSql;
    procedure   StopRun;
    property    FDataSourceList: TList read GetSourceList;
    property    FMsgList: TStrings read GetMsgList;
    property    RunSucced: boolean read GetRunSucced;
  published
    { Published declarations }
    property Connection: TADOConnection read GetConnection write SetConnection;
    property ConnectionString: string read GetConnectionString write SetConnectionString;
    property EditorFont: TFont read GetEditorFont write SetEditorFont;
    property GridFont: TFont Read GetGridFont Write SetGridFont;
    property MsgFont: TFont Read GetMsgFont Write SetMsgFont;
    property ShowGridPage: boolean read GetShowGridPage write SetShowGridPage;
    property UseCustomGrid: boolean read FUseCustomGrid write SetUseCustomGrid;
    property SqlLines: TStrings read FSqlLines write SetSqlLines;
    property ShowToolBar: boolean read GetShowToolBar write SetShowToolBar;
    property Align;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderStyle;
    property OnRunSqlEnd: TRunSqlEvent read FOnRunSqlEnd Write SetOnRunSqlEnd;
  end;

  TCnSqlRunner = Class(TComponent)
  private
    FConnectionString: string;
    FConnection: TADOConnection;
    FRunThread: TRunThread;
    FMsgHandle: THandle;
    FRunSucc: Boolean;
    FOnRunEnd: TRunSqlEvent;
    FDataSourceList: TList;
    FMsgList: TStringList;
    procedure SetConnection(const Value: TADOConnection);
    procedure SetConnectionString(const Value: string);
    procedure RunEnd(Sender: Tobject);
    procedure ClearRecords;
    procedure SetMsgHandle(const Value: THandle);
  public
    constructor CreateEx(AOwner: TComponent; AMsgHandle: THandle);
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure RunSql(SqlText: string; OnlyParse: Boolean = False);
    procedure RunStop;
    property  MsgHandle: THandle read FMsgHandle write SetMsgHandle;
  published
    property Connection: TADOConnection read FConnection write SetConnection;
    property ConnectionString: string read FConnectionString write SetConnectionString;
    property RunSucc: boolean read FRunSucc;
    property OnRunEnd: TRunSqlEvent read FOnRunEnd write FOnRunEnd;
  end;

{$ENDIF SUPPORT_ADO}

implementation

{$IFDEF SUPPORT_ADO}

{ TCnSQLAnalyzer }

constructor TCnSQLAnalyzer.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);
   Height := 296;
   Width := 444;
   BevelOuter := bvLowered;
   FrameRunSql := TCnFrameRunSql.Create(self);
   FrameRunSql.InitRunFrame;
   FrameRunSql.Parent := self;
   FrameRunSql.CnSQLAnalyzer := self;
   FSqlLines := TStringList.Create;
end;

destructor TCnSQLAnalyzer.Destroy;
begin
  FSqlLines.Free;
  FrameRunSql.FreeRes;
  FrameRunSql.Free;
  inherited;
end;

function TCnSQLAnalyzer.GetConnection: TADOConnection;
begin
  Result := FrameRunSql.Connection;
end;

function TCnSQLAnalyzer.GetConnectionString: string;
begin
   Result := FrameRunSql.Constr;
end;

function TCnSQLAnalyzer.GetEditorFont: TFont;
begin
  Result := FrameRunSql.CodeEdit.Font;
end;

function TCnSQLAnalyzer.GetGridFont: TFont;
begin
  Result := FrameRunSql.DataGridFont;
end;


function TCnSQLAnalyzer.GetMsgFont: TFont;
begin
  Result := FrameRunSql.Panel1.Font;
end;

function TCnSQLAnalyzer.GetMsgList: TStrings;
begin
  Result := FrameRunSql.MsgEdit.Lines;
end;

function TCnSQLAnalyzer.GetRunSucced: boolean;
begin
  Result := FrameRunSql.RunSucc;
end;

function TCnSQLAnalyzer.GetShowGridPage: boolean;
begin
   Result := FrameRunSql.BtnShowGrid.Down;
end;

function TCnSQLAnalyzer.GetShowToolBar: boolean;
begin
  Result := FrameRunSql.ToolBar1.Visible;
end;

function TCnSQLAnalyzer.GetSourceList: TList;
begin
   Result := FrameRunSql.DataSourceList;
end;

function TCnSQLAnalyzer.GetSqlText: string;
begin
  Result := FrameRunSql.CodeEdit.Text;
end;


procedure TCnSQLAnalyzer.Loaded;
begin
  inherited;
  FrameRunSql.CodeEdit.Lines.Assign(FSqlLines);
end;

procedure TCnSQLAnalyzer.LoadFromFile(FileName: string);
begin
  FrameRunSql.CodeEdit.Lines.LoadFromFile(FileName);
end;

procedure TCnSQLAnalyzer.ParaseSql;
begin
  FrameRunSql.BtnParaseClick(nil);
end;

procedure TCnSQLAnalyzer.RunSql;
begin
  FrameRunSql.ActRunExecute(nil);
end;

procedure TCnSQLAnalyzer.SaveToFile(FileName: string);
begin
  FrameRunSql.CodeEdit.Lines.SaveToFile(FileName);
end;

procedure TCnSQLAnalyzer.SetConnection(const Value: TADOConnection);
begin
  if FrameRunSql.Connection <> Value then
    FrameRunSql.Connection := Value;
end;

procedure TCnSQLAnalyzer.SetConnectionString(const Value: string);
begin
   FrameRUnSql.Constr := Value;
end;

procedure TCnSQLAnalyzer.SetEditorFont(const Value: TFont);
begin
  FrameRunSql.CodeEdit.Font.Assign(Value);
end;

procedure TCnSQLAnalyzer.SetGridFont(const Value: TFont);
begin
  FrameRunSql.DataGridFont.Assign(Value);
end;


procedure TCnSQLAnalyzer.SetMsgFont(const Value: TFont);
begin
  FrameRunsql.Panel1.Font.Assign(Value);
end;

procedure TCnSQLAnalyzer.SetOnRunSqlEnd(const Value: TRunSqlEvent);
begin
  FOnRunSqlEnd := Value;
  FrameRunSql.RunEndProc := Value;
end;

procedure TCnSQLAnalyzer.SetShowGridPage(const Value: boolean);
begin
  if not FUseCustomGrid then
    FrameRunSql.ShowGridPage := Value;
end;

procedure TCnSQLAnalyzer.SetShowToolBar(const Value: boolean);
begin
  FrameRunSql.ToolBar1.Visible := Value;
end;

procedure TCnSQLAnalyzer.SetSqlLines(const Value: TStrings);
begin
  FSqlLines.Assign(Value);
  FrameRunSql.CodeEdit.Lines.Assign(FSqlLines);
end;

procedure TCnSQLAnalyzer.SetSqlText(const Value: string);
begin
  FrameRunSql.CodeEdit.Text := Value;
end;

procedure TCnSQLAnalyzer.SetUseCustomGrid(const Value: boolean);
begin
  FUseCustomGrid := Value;
  FrameRunSql.CustomShowGrid := Value;
end;

procedure TCnSQLAnalyzer.StopRun;
begin
  FrameRunSql.BtnStopClick(nil);
end;

{ TCnSqlRunner }

procedure TCnSqlRunner.ClearRecords;
var
  Ds: TDataSource;
begin
  while FDataSourceList.Count > 0 do
  begin
    Ds := TDataSource(FDataSourceList.Items[FDataSourceList.Count - 1]);
    Ds.DataSet.Free;
    Ds.Free;
    FDataSourceList.Delete(FDataSourceList.Count - 1);
  end;
  FMsgList.Clear;
end;

constructor TCnSqlRunner.CreateEx(AOwner: TComponent; AMsgHandle: THandle);
begin
  Create(AOwner);
  FMsgHandle := AMsgHandle;
end;

constructor TCnSqlRunner.Create(AOwner: TComponent);
begin
  inherited;
  FDataSourceList := TList.Create;
  FMsgList := TStringList.Create;
end;

destructor TCnSqlRunner.Destroy;
begin
  ClearRecords;
  FMsgList.Free;
  FDataSourceList.Free;
  inherited;
end;

procedure TCnSqlRunner.RunEnd(Sender: Tobject);
var
  Thread: TRunThread;
  i: integer;
  tempstr: pchar;
  Ds: TDataSource;
begin
  Thread := TRunThread(Sender);
  ClearRecords;

  for i := 0 to Thread.RecordList.Count - 1 do
  begin
    Ds := Thread.RecordList.Items[i];
    FDataSourceList.Add(ds);
  end;

  for i := 0 to Thread.MsgList.Count - 1 do
  begin
     tempstr := Thread.MsgList.Items[i];
     FMsgList.Add(string(tempstr));
  end;
  FRunSucc := Thread.RunSucced;

  if Assigned(FOnRunEnd) then
    FOnRunEnd(Self, FDataSourceList, FMsgList);
end;

procedure TCnSqlRunner.RunSql(SqlText: string; OnlyParse: Boolean);
var
  Con: TADOConnection;
begin
  FRunThread := TRunThread.Create(true,FMsgHandle);
  if Connection = nil then
  begin
    if Trim(FConnectionString) <> '' then
    begin
      Con := TADOConnection.Create(nil);
      Con.LoginPrompt := False;
      Con.ConnectionString := FConnectionString;
      //Con.ConnectOptions := coAsyncConnect;//ʹ���첽��ʽ��ѯ
      FRunThread.Connection := con;
      FRunThread.DBProvider := Con.Provider;
    end
    else
    begin
      FRunThread.MsgList.Add(strNew(pchar(SCnUnUseConstr)));
      FRunThread.Terminate;
      raise Exception.Create(SCnUnUseConstr);
    end;
  end
  else
  begin
    //Connection.ConnectOptions := coAsyncConnect;//ʹ���첽��ʽ��ѯ
    FRunThread.Connection := Connection;
    FRunThread.DBProvider := COnnection.Provider;
  end;
  FRunThread.OnTerminate := RunEnd;
  FRunThread.IsParse := OnlyParse;
  FRunThread.FreeOnTerminate := True;
  FRunThread.Sql := SqlText;
  FRunThread.Resume;
end;

procedure TCnSqlRunner.RunStop;
begin
  if not FRunThread.IsStop then
  begin
    FRunThread.IsStop := True;
    FRunThread.Connection.Cancel;
    FRunThread.MsgList.Add(strNew(pchar(SCnOperateCancel)));
    FRunThread.Terminate;
  end;
end;

procedure TCnSqlRunner.SetConnection(const Value: TADOConnection);
begin
  FConnection := Value;
end;

procedure TCnSqlRunner.SetConnectionString(const Value: string);
begin
  FConnectionString := Value;
end;

procedure TCnSqlRunner.SetMsgHandle(const Value: THandle);
begin
  FMsgHandle := Value;
end;

{$ENDIF SUPPORT_ADO}
end.

