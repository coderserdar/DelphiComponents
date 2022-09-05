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

unit CnASHostServices;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�ActiveScript Host ����Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע���õ�Ԫ�����˹� ActiveScript ʹ�õ� Host ��������
* ����ƽ̨��PWin2K SP3 + Delphi 7
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2003.10.31
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF COMPILER6_UP}
  'Error: This unit can used only for Delphi / C++Builder 6 or up.'
{$ENDIF COMPILER6_UP}

uses
  Windows, SysUtils, Classes, TypInfo, ComObj, CnASIDispatchProxy, CnCommon;

type
  
//==============================================================================
// ActiveScript �������������
//==============================================================================

{ ICnASService }

  ICnASService = interface(IActiveScriptInvokable)
  ['{5640170F-9C03-400E-9E46-549A80F3ABDD}']
  end;

{ TCnASService }

  TCnASService = class(TInterfacedObject, IActiveScriptInvokable, ICnASService)
  {* ActiveScript ������������࣬�����ṩ������ű�ʹ�ã�
     �ű��ɵ��� HostServices.CreateObject('ServiceName', CreateParam) ��ʹ�á�}
  public
    constructor Create(const CreateParam: OleVariant); virtual;
    {* �๹������CreateParam Ϊ�ű�����Ĳ���}
  end;

  TCnASServiceClass = class of TCnASService;

//==============================================================================
// ActiveScript Host ������
//==============================================================================

{ ICnASHostServices }

  ICnASHostServices = interface(IActiveScriptInvokable)
  ['{E5B6A915-69AE-4CB2-BFBE-9411B75F8E49}']
    // �������
    function CreateObject(const ServiceName: string; CreateParam: OleVariant): IUnknown; stdcall;
    {* ���첢����һ��ָ����ʶ���ƵĶ���ӿڣ�ServiceName Ϊ�������ƣ�
       CreateParam Ϊ��صĲ���}

    // �Ի������
    function MessageBox(const Text, Caption: string; Flags: Integer): Integer; stdcall;
    {* ��Ϣ�Ի���}
    procedure InfoDlg(const Text: string; const Caption: string = ''); stdcall;
    {* ��ʾ��ʾ����}
    function InfoOk(Text: string; Caption: string = ''): Boolean; stdcall;
    {* ��ʾ��ʾȷ�ϴ���}
    procedure ErrorDlg(Text: string; Caption: string = ''); stdcall;
    {* ��ʾ���󴰿�}
    procedure WarningDlg(Text: string; Caption: string = ''); stdcall;
    {* ��ʾ���洰��}
    function QueryDlg(Text: string; DefaultNo: Boolean = False;
      Caption: string = ''): Boolean; stdcall;
    {* ��ʾѯ�ʶԻ���}
    function InputQuery(const Caption, Prompt: string): string; stdcall;
    {* ��ʾһ�����봰�ڣ����ȡ�������ؿ��ַ���}

    { TODO : �������� Host ���������ܡ� }
  end;

function GetCnASHostServices: IDispatch;
{* ����һ��֧�� ICnASHostServices �� IDispatch �Ľӿڡ�
   �ɹ� ActiveScriptSite.AddNamedItem ʹ�á�}

//==============================================================================
// Host �������б���ع���
//==============================================================================

procedure RegisterCnASService(const ServiceName: string;
  const AClass: TCnASServiceClass; IntfTypeInfo: PTypeInfo);
{* ע��һ�� TCnASService ���������ã�ÿ��������ʵ��Ӧ�ڸõ�Ԫ�� initialization
   �ڵ��øù���ע����ط����� }

implementation

//==============================================================================
// Host �������б���ع���
//==============================================================================

var
  CnASServiceClassList: TStrings = nil; // Host �����������б�
  CnASServiceIntfTypeInfoList: TList = nil; // ʵ�ֵĽӿ���Ϣ�б�

// ע��һ�� TCnASServiceClass ����������
procedure RegisterCnASService(const ServiceName: string;
  const AClass: TCnASServiceClass; IntfTypeInfo: PTypeInfo);
begin
  if CnASServiceClassList.IndexOf(ServiceName) < 0 then
  begin
    CnASServiceClassList.AddObject(ServiceName, TObject(AClass));
    CnASServiceIntfTypeInfoList.Add(IntfTypeInfo);
  end;
end;

// ���ݷ�������ȡָ���ķ���������
function GetCnASServiceClass(const ServiceName: string; var IntfTypeInfo:
  PTypeInfo): TCnASServiceClass;
var
  Idx: Integer;
begin
  Idx := CnASServiceClassList.IndexOf(ServiceName);
  if Idx >= 0 then
  begin
    Result := TCnASServiceClass(CnASServiceClassList.Objects[Idx]);
    IntfTypeInfo := CnASServiceIntfTypeInfoList[Idx];
  end
  else
  begin
    Result := nil;
    IntfTypeInfo := nil;
  end;
end;

//==============================================================================
// ActiveScript �������������
//==============================================================================

{ TCnASService }

constructor TCnASService.Create(const CreateParam: OleVariant);
begin
  inherited Create;
end;

//==============================================================================
// ActiveScript Host ������
//==============================================================================

{ ICnASHostServices }

type
  TCnASHostServices = class(TInterfacedObject, ICnASHostServices)
  public
    // �������
    function CreateObject(const ServiceName: string; CreateParam: OleVariant): IUnknown; stdcall;

    // �Ի������
    function MessageBox(const Text, Caption: string; Flags: Integer): Integer; stdcall;
    procedure InfoDlg(const Text: string; const Caption: string = ''); stdcall;
    function InfoOk(Text: string; Caption: string = ''): Boolean; stdcall;
    procedure ErrorDlg(Text: string; Caption: string = ''); stdcall;
    procedure WarningDlg(Text: string; Caption: string = ''); stdcall;
    function QueryDlg(Text: string; DefaultNo: Boolean = False;
      Caption: string = ''): Boolean; stdcall;
    function InputQuery(const Caption, Prompt: string): string; stdcall;
  end;

// ����һ��֧�� ICnASHostServices �� IDispatch �Ľӿڡ�
function GetCnASHostServices: IDispatch;
begin
  Result := GetIDispatchProxy(TCnASHostServices.Create, TypeInfo(ICnASHostServices));
end;

{ TCnASHostServices }

function TCnASHostServices.CreateObject(const ServiceName: string;
  CreateParam: OleVariant): IUnknown;
var
  IntfTypeInfo: PTypeInfo;
  ServiceClass: TCnASServiceClass;
begin
  ServiceClass := GetCnASServiceClass(ServiceName, IntfTypeInfo);
  if Assigned(ServiceClass) then
    Result := GetIDispatchProxy(ServiceClass.Create(CreateParam), IntfTypeInfo)
  else
    Result := nil;
end;

procedure TCnASHostServices.ErrorDlg(Text, Caption: string);
begin
  CnCommon.ErrorDlg(Text, Caption);
end;

procedure TCnASHostServices.InfoDlg(const Text, Caption: string);
begin
  CnCommon.InfoDlg(Text, Caption);
end;

function TCnASHostServices.InfoOk(Text, Caption: string): Boolean;
begin
  Result := CnCommon.InfoOk(Text, Caption);
end;

function TCnASHostServices.InputQuery(const Caption,
  Prompt: string): string;
begin
  CnCommon.CnInputQuery(Caption, Prompt, Result);
end;

function TCnASHostServices.MessageBox(const Text, Caption: string;
  Flags: Integer): Integer;
begin
  Result := Windows.MessageBox(0, PChar(Text), PChar(Caption), Flags);
end;

function TCnASHostServices.QueryDlg(Text: string; DefaultNo: Boolean;
  Caption: string): Boolean;
begin
  Result := CnCommon.QueryDlg(Text, DefaultNo, Caption);
end;

procedure TCnASHostServices.WarningDlg(Text, Caption: string);
begin
  CnCommon.WarningDlg(Text, Caption);
end;

initialization
  CnASServiceClassList := TStringList.Create;
  CnASServiceIntfTypeInfoList := TList.Create;

finalization
  FreeAndNil(CnASServiceClassList);
  FreeAndNil(CnASServiceIntfTypeInfoList);

end.
