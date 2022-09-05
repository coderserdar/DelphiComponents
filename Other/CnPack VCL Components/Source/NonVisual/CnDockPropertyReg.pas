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

unit CnDockPropertyReg;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������ͣ����Ԫ
* ��Ԫ���ƣ�ͣ���������������༭����Ԫ 
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
  Classes, SysUtils, Dialogs,
  {$IFDEF COMPILER6_UP}
  DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  Dsgnintf,
  {$ENDIF}
  CnDockFormControl, CnVIDDockStyle;


type
  TCnDockControlEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TCnDockStyleEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TCnVIDTabPageControlEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

uses
  CnDockGlobal;

{ TCnDockControlEditor }

procedure TCnDockControlEditor.ExecuteVerb(Index: Integer);
var ABoutStr: string;
  ProductStr: string;
begin
  inherited;
  case Index of
    0:
    begin
      if Component is TCnDockServer then
        ProductStr := gs_CnDcokServerName
      else if Component is TCnDockClient then
        ProductStr := gs_CnDcokClientName
      else Exit;
      ABoutStr := Format(gs_CnDockManagerAbout,
        [ProductStr,
        gs_CnDockManagerVersion,
        gs_CnDockManagerCopyRightBegin,
        gs_CnDockManagerCopyRightEnd,
        gs_CnAuthorName,
        gs_CnComparyName,
        gs_CnHomePage,
        gs_CnEmail]);
      ShowMessage(ABoutStr);
    end;
  end;
end;

function TCnDockControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
    begin
      if Component is TCnDockServer then
        Result := Format('%s %s', [gs_CnAbout, gs_CnDcokServerName])
      else if Component is TCnDockClient then
        Result := Format('%s %s', [gs_CnAbout, gs_CnDcokClientName])
      else Exit;
    end;
  end;
end;

function TCnDockControlEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TCnBaseDockStyleEditor }

procedure TCnDockStyleEditor.ExecuteVerb(Index: Integer);
var ABoutStr: string;
begin
  inherited;
  case Index of
    0:
    begin
      ABoutStr := Format(gs_CnDockManagerAbout,
        [TCnBasicDockStyle(Component).GetControlName,
        gs_CnDockStyleVersion,
        gs_CnDockStyleCopyRightBegin,
        gs_CnDockStyleCopyRightEnd,
        gs_CnAuthorName,
        gs_CnComparyName,
        gs_CnHomePage,
        gs_CnEmail]);
      ShowMessage(ABoutStr);
    end;
  end;
end;

function TCnDockStyleEditor.GetVerb(Index: Integer): string;
begin
  Result := Format('%s %s', [gs_CnAbout, TCnBasicDockStyle(Component).GetControlName]);
end;

function TCnDockStyleEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TCnVIDTabPageControlEditor }

procedure TCnVIDTabPageControlEditor.ExecuteVerb(Index: Integer);
var Sheet: TCnVIDDockTabSheet;
  Page: TCnVIDTabPageControl;
begin
  inherited ExecuteVerb(Index);
  if Component is TCnVIDTabPageControl then
    Page := Component as TCnVIDTabPageControl
  else Page := TCnVIDDockTabSheet(Component).Parent as TCnVIDTabPageControl;
  case Index of
    0:
    begin
{$IFDEF COMPILER6_UP}
      Sheet := TCnVIDDockTabSheet.Create(Designer.Root);
{$ELSE}
      Sheet := TCnVIDDockTabSheet.Create(Designer.Form);
{$ENDIF}

      Sheet.PageControl := Page;
      Sheet.Name := Designer.UniqueName(TCnVIDDockTabSheet.ClassName);
      Sheet.Caption := Sheet.Name;
      Page.ActivePage := Sheet;
      Page.Panel.Invalidate;
    end;
    1:
    begin
      if Page.PageCount >= 0 then
      begin
        if Page.ActivePageIndex = Page.PageCount - 1 then
          Page.ActivePageIndex := 0
        else Page.ActivePageIndex := Page.ActivePageIndex + 1;
      end;
    end;
    2:
    begin
      if Page.PageCount >= 0 then
      begin
        if Page.ActivePageIndex = 0 then
          Page.ActivePageIndex := Page.PageCount - 1
        else Page.ActivePageIndex := Page.ActivePageIndex - 1;
      end;
    end;
    3:
    begin
      if Page.PageCount >= 0 then
        Page.ActivePage.Free;
    end;
  end;
end;

function TCnVIDTabPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Ne&w Page';
    1: Result := 'Ne&xt Page';
    2: Result := '&Pravious Page';
    3: Result := '&Delete Page';
  end;
end;

function TCnVIDTabPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

end.
