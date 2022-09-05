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

unit CnIISCtrl;

{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�ʵ��IIS���ù��ܵ�Ԫ
* ��Ԫ���ߣ�rarnu(rarnu@cnpack.org)
* ��    ע��
* ����ƽ̨��Windows2003 Server + Delphi2007 up2
* ���ݲ��ԣ�Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.08.14 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, ComObj, Windows;

type
  TCnIISCtrl = class(TComponent)
  private
    FOnDeleteVirtualDirApp: TNotifyEvent;
    FOnDeleteVirtualDir: TNotifyEvent;
    FOnCreateVirtualDir: TNotifyEvent;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    {* ����Ƿ���� .NET FrameWork }
    function CheckDotNetFramework: Boolean;
    {* ɾ������Ŀ¼Ӧ�ó����� }
    function DeleteVirtualDirApp(strVirtualDir: string): Boolean;
    {* ɾ������Ŀ¼ }
    function DeleteVirtualDir(strVirtualDir: string): Boolean;
    {* ����Ƿ�������Ŀ¼ }
    function CheckVirtualDir(const strVirtualDir: string): Boolean;
    {* ��������Ŀ¼ }
    function CreateVirtualDir(const strVirtualDir, strDir, strAppName: string): Boolean;
  published
    {* ɾ������Ŀ¼Ӧ�ó�����ʱ�����¼� }
    property OnDeleteVirtualDirApp: TNotifyEvent read FOnDeleteVirtualDirApp write FOnDeleteVirtualDirApp;
    {* ɾ������Ŀ¼ʱ�����¼� }
    property OnDeleteVirtualDir: TNotifyEvent read FOnDeleteVirtualDir write FOnDeleteVirtualDir;
    {* ��������Ŀ¼ʱ�����¼� }
    property OnCreateVirtualDir: TNotifyEvent read FOnCreateVirtualDir write FOnCreateVirtualDir;
  end;

implementation

{ TCnIISCtrl }

function TCnIISCtrl.CheckDotNetFramework: Boolean;
var
  SysDir: pchar;
begin
  GetMem(SysDir, 250);
  GetSystemDirectory(SysDir, 250);
  if not FileExists(SysDir + '\MSCOREE.DLL') then
    Result := False
  else
    Result := True;
  FreeMem(SysDir);
end;

function TCnIISCtrl.CheckVirtualDir(const strVirtualDir: string): Boolean;
var
  WebSite, WebServer, WebRoot: Variant;
begin
  Result := True;
  try
    WebSite := CreateOleObject('IISNamespace');
    WebSite := WebSite.GetObject('IIsWebService', 'localhost/w3svc');
    WebServer := WebSite.GetObject('IIsWebServer', '1');
    WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
    WebRoot.GetObject('IIsWebVirtualDir', strVirtualDir);
  except
    Result := False;
  end;
end;

constructor TCnIISCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TCnIISCtrl.CreateVirtualDir(const strVirtualDir, strDir,
  strAppName: string): Boolean;
var
  WebSite, WebServer, WebRoot, vdir: Variant;
begin
  Result := True;
  try
    WebSite := CreateOleObject('IISNamespace');
    WebSite := WebSite.GetObject('IIsWebService', 'localhost/w3svc');
    WebServer := WebSite.GetObject('IIsWebServer', '1');
    WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
    vdir := WebRoot.Create('IIsWebVirtualDir', strVirtualDir);
    vdir.AccessRead := True;                    // �����ȡ
    vdir.AccessScript := True;                  // ִ�����Ϊ���ű�
    vdir.DefaultDoc := 'index.aspx,index.asp';  // Ĭ���ĵ�
    vdir.EnableDirBrowsing := False;            // �������Ŀ¼
    vdir.AppFriendlyName := strAppName;         // Ӧ�ó�����
    vdir.Path := strDir;                        // ����Ŀ¼��ʵ·��
    vdir.AppCreate(True);                       // ����Ŀ¼�Զ�����Ӧ�ó�����
    vdir.SetInfo;
  except
    Result := False;
  end;

  (************************************************************)
  (*       IIS ������������б��ڴˣ��ɸ�����Ҫ�޸�           *)
  (*                                                          *)
  (*  vdir.AccessWrite   := True;   // ����д��               *)
  (*  vdir.AccessSource  := True;   // ����ű���Դ����       *)
  (*  vdir.AccessExecute := True;   // �����ִ���ļ�         *)
  (************************************************************)

  if Assigned(FOnCreateVirtualDir) then
    FOnCreateVirtualDir(Self);
end;

function TCnIISCtrl.DeleteVirtualDir(strVirtualDir: string): Boolean;
var
  WebSite, WebServer, WebRoot: Variant;
begin
  Result := True;
  try
    WebSite := CreateOleObject('IISNamespace');
    WebSite := WebSite.GetObject('IIsWebService', 'localhost/w3svc');
    WebServer := WebSite.GetObject('IIsWebServer', '1');
    WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
    WebRoot.Delete('IIsWebVirtualDir', strVirtualDir);
  except
    Result := False;
  end;
  if Assigned(FOnDeleteVirtualDir) then
    FOnDeleteVirtualDir(Self);
end;

function TCnIISCtrl.DeleteVirtualDirApp(strVirtualDir: string): Boolean;
var
  WebSite, WebServer, WebRoot, vdir: Variant;
begin
  Result := True;
  try
    WebSite := CreateOleObject('IISNamespace');
    WebSite := WebSite.GetObject('IIsWebService', 'localhost/w3svc');
    WebServer := WebSite.GetObject('IIsWebServer', '1');
    WebRoot := WebServer.GetObject('IIsWebVirtualDir', 'Root');
    vdir := WebRoot.GetObject('IIsWebVirtualDir', strVirtualDir);
    vdir.AppDelete;
    vdir.SetInfo;
  except
    Result := False;
  end;
  if Assigned(FOnDeleteVirtualDirApp) then
    FOnDeleteVirtualDirApp(Self);
end;

end.
