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

{*******************************************************}
{                                                       }
{       ��������Delphi��ͣ�����                        }
{       CnDelphiDockStyle ��Ԫ                          }
{                                                       }
{       ��Ȩ (C) 2002,2003 ³С��                       }
{                                                       }
{*******************************************************}

unit CnDelphiDockStyle;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ���������Delphi��ͣ�����ĵ�Ԫ 
* ��Ԫ���ߣ�CnPack������ ���沨��³С�ࣩ
* ��    ע������Ԫ��ԭ������ȨCnPack��������ֲ���ѱ���ԭ���߰�Ȩ��Ϣ
* ����ƽ̨��
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.07.13 V1.0
*                ��ֲ��Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses 
  Windows, Classes, Controls, Math, Messages, Graphics,
  CnDockFormControl, CnDockSupportControl, CnDockTree, CnConsts, CnCompConsts;

type

  TCnDelphiDockStyle = class(TCnBasicDockStyle)
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;  
    procedure FormDockDrop(DockClient: TCnDockClient;
      Source: TCnDragDockObject; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
//    class function GetControlName: string; override;
    function GetControlName: string; override;
  published
    property ConjoinServerOption;
    property TabServerOption;
  end;

  TCnDelphiDockSplitter = class(TCnDockSplitter);

  TCnDelphiDockPanel = class(TCnDockPanel);

  TCnDelphiConjoinPanel = class(TCnConjoinPanel);

  TCnDelphiTabPageControl = class(TCnTabPageControl)
  protected
    procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
  end;

  TCnDelphiDockZone = class(TCnDockZone);

  TCnDelphiDockTree = class(TCnDockTree);

  TCnDelphiDragDockObject = class(TCnDragDockObject);
  
implementation

uses
  Forms, SysUtils, CnDockSupportProc, CnDockGlobal;

{ TCnDelphiDockStyle }

constructor TCnDelphiDockStyle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CnDockPanelClass := TCnDelphiDockPanel;
  CnDockSplitterClass := TCnDelphiDockSplitter;
  CnConjoinPanelClass := TCnDelphiConjoinPanel;
  CnTabDockClass := TCnDelphiTabPageControl;
  CnDockPanelZoneClass := TCnDelphiDockZone;
  CnDockPanelTreeClass := TCnDelphiDockTree;
  CnConjoinPanelZoneClass := TCnDelphiDockZone;
  CnConjoinPanelTreeClass := TCnDelphiDockTree;
end;

procedure TCnDelphiDockStyle.FormDockDrop(DockClient: TCnDockClient;
  Source: TCnDragDockObject; X, Y: Integer);
var
  ARect,DRect: TRect;
  DockType: TAlign;
  Host: TForm;
  APanelDock: TWinControl;
  ADockClient: TCnDockClient;
begin
  if IsDockable(DockClient.ParentForm, Source.Control, Source.DropOnControl, Source.DropAlign) then
  begin
    // ����ComputeDockingRect����֪��ͣ��������
    Host := nil;
    { ��סWindows���� }
    if not IsLoading then
      Cn_LockWindow(nil);
    try
      with DockClient do
      begin
        DockType := ComputeDockingRect(DockClient.ParentForm, ARect, Point(X, Y));
        if (ParentForm.HostDockSite is TCnDockPanel) then
        begin
          // ���ͣ����������TDockPanel����ͣ����TDockServer��DockPanel�ϡ�
          if DockType = alClient then
          begin
            // ���ͣ��������alClient
            if Source.Control is TCnTabDockHostForm then
            begin
              // ���ͣ���ͻ���TCnTabDockHostForm��
              // ���Ȱ�Parentͣ����TCnTabDockHostForm�У�
              // �ٰ�TCnTabDockHostFormͣ����TCnDockPanel�С�
              APanelDock := ParentForm.HostDockSite;
              ARect := ParentForm.BoundsRect;
              ParentForm.ManualDock(TCnTabDockHostForm(Source.Control).PageControl, nil, alClient);
              TCnTabDockHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
              Source.Control.BoundsRect := ARect;
              Source.Control.ManualDock(APanelDock, nil, alClient);
              if ParentForm.FormStyle = fsStayOnTop then
                TForm(Source.Control).FormStyle := fsStayOnTop;
            end else
            begin
              // ����ʹ���TCnTabDockHostForm��
              // �Ѱ�Parentͣ����TCnTabDockHostForm�У�
              // �ٰ�TCnTabDockHostFormͣ����TCnDockPanel�С�
              APanelDock := ParentForm.HostDockSite;
              DRect.TopLeft := ParentForm.HostDockSite.ClientToScreen(Point(0, 0));
              Host := CreateTabHostAndDockControl(ParentForm, Source.Control);
              SetDockSite(ParentForm, False);
              SetDockSite(TWinControl(Source.Control), False);
              Host.Top := DRect.Top;
              Host.Left := DRect.Left;
              Host.ManualDock(APanelDock, nil, alClient);
              Host.Visible := True;
            end;
          end
          else
          begin
            // ���ͣ�����Ͳ���alClient,
            // �Ͱ�ͣ������ͣ����TCnDockPanel.
            DRect := ParentForm.HostDockSite.BoundsRect;
            Source.Control.ManualDock(ParentForm.HostDockSite, nil, DockType);
            ParentForm.HostDockSite.BoundsRect := DRect;
          end;
          Exit;
        end;

        // ������ҳ�ķ�����
        if DockType = alClient then
        begin
          if Source.Control is TCnTabDockHostForm then
          begin
            ARect := DockClient.ParentForm.BoundsRect;
            DockClient.ParentForm.ManualDock(TCnTabDockHostForm(Source.Control).PageControl, nil, alClient);
            TCnTabDockHostForm(Source.Control).PageControl.ActivePage.PageIndex := 0;
            Source.Control.BoundsRect := ARect;
            if DockClient.ParentForm.FormStyle = fsStayOnTop then
              TCnTabDockHostForm(Source.Control).FormStyle := fsStayOnTop;
            Exit;
          end else
          begin
            Host := DockClient.CreateTabHostAndDockControl(DockClient.ParentForm, Source.Control);
            Host.Visible := True;
          end;
        end
        // ����ƽ�̵ķ�����
        else if DockType <> alNone then
        begin
          Host := CreateConjoinHostAndDockControl(ParentForm, Source.Control, DockType);
          ADockClient := FindDockClient(Host);
          if ADockClient <> nil then
            ADockClient.EnableDock := False;
          SetDockSite(ParentForm, False);
          SetDockSite(TWinControl(Source.Control), False);
          Host.Visible := True;
        end;

        if Host <> nil then
        begin
          Host.LRDockWidth := Source.Control.LRDockWidth;
          Host.TBDockHeight := Source.Control.TBDockHeight;
        end;
      end;
    finally
      { ����Windows���� }
      if not IsLoading then
        Cn_UnLockWindow;
    end;
  end;
end;

procedure TCnDelphiDockStyle.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnDelphiDockStyleName;
  Author := SCnPack_LuXiaoban;
  Email := SCnPack_LuXiaobanEmail;
  Comment := SCnDelphiDockStyleComment;
end;

function TCnDelphiDockStyle.GetControlName: string;
begin
  Result := Format(gs_LikeDelphiStyle, [inherited GetControlName]);
end;

{ TCnDelphiTabPageControl }

procedure TCnDelphiTabPageControl.CMDockClient(var Message: TCMDockClient);
var i: Integer;
  AControl: TControl;
  APageCount: Integer;
begin
  if Message.DockSource.Control is TCnTabDockHostForm then
  begin
    with TCnTabDockHostForm(Message.DockSource.Control) do
    begin
      APageCount := Self.PageCount;
      for i := PageControl.DockClientCount - 1 downto 0 do
      begin
        AControl := PageControl.DockClients[i];
        DoFloat(PageControl, AControl);
        AControl.ManualDock(Self, nil, alClient);
        Self.ActivePage.PageIndex := APageCount;
      end;
    end;
  end else
    inherited;
end;

end.
