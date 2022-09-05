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

unit CnASIDispatchProxy;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�ActiveScript Host ���� IDispatch ����ӿڵ�Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע��
* ����ƽ̨��PWin2K SP3 + Delphi 7
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2003.07.11
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF COMPILER6_UP}
  'Error: This unit can used only for Delphi / C++Builder 6 or up.'
{$ENDIF COMPILER6_UP}

uses
  Windows, Classes, TypInfo;

type

{$M+}
  IActiveScriptInvokable = interface(IUnknown)
  end;
{$M-}

function GetIDispatchProxy(AItemObject: TObject; IntfTypeInfo: PTypeInfo): IDispatch;

implementation

uses
  Sysutils, ActiveX, Variants, CnASInvoker;

type

{ TIDispatchProxy }

  TIDispatchProxy = class(TInterfacedObject, IDispatch)
  protected
    FObject: TObject;
    FIntf: IUnknown;
    FIntfMD: TIntfMetaData;
    function GetTypeInfoCount(out Count: Integer): hResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): hResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): hResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): hResult;
      stdcall;
  public
    constructor Create(AItemObject: TObject; IntfTypeInfo: PTypeInfo);
    destructor Destroy; override;
    property ItemObject: TObject read FObject write FObject;
  end;

function GetIDispatchProxy(AItemObject: TObject; IntfTypeInfo: PTypeInfo): IDispatch;
begin
  Result := TIDispatchProxy.Create(AItemObject, IntfTypeInfo) as IDispatch;
end;

constructor TIDispatchProxy.Create(AItemObject: TObject; IntfTypeInfo: PTypeInfo);
resourcestring
  SNoInterfaceGUID = 'Class %s does not implement interface GUID %s';
begin
  Assert(Assigned(AItemObject));
  Assert(Assigned(IntfTypeInfo));
  inherited Create;
  FObject := AItemObject;
  GetIntfMetaData(IntfTypeInfo, FIntfMD, True);
  // ����һ���ӿ�����
  if not FObject.GetInterface(FIntfMD.IID, FIntf) then
    raise Exception.CreateFmt(SNoInterfaceGUID,
      [FObject.ClassName, GuidToString(FIntfMD.IID)]);
end;

destructor TIDispatchProxy.Destroy;
begin
  FIntf := nil;
  inherited;
end;

function TIDispatchProxy.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): hResult;
type
  TDispIDsArray = array[0..0] of TDispID;
  PDispIDsArray = ^TDispIDsArray;
var
  IDs: PDispIDsArray absolute DispIDs;
  i: Integer;
  Name: WideString;
  Id: Integer;
begin
  if NameCount > 1 then
    Result := DISP_E_UNKNOWNNAME
  else if NameCount < 1 then
    Result := E_INVALIDARG
  else
    Result := S_OK;
    
  for i := 0 to NameCount - 1 do
    IDs[i] := DISPID_UNKNOWN;
    
  if NameCount = 1 then
  begin
    Name := PWideChar(Names^);
    //Name := UpperCase(Name);
    Id := GetMethNum(FIntfMD, Name);
    if Id <> 0 then
    begin
      IDs[0] := Id;
    end
    else
    begin
      Result := DISP_E_UNKNOWNNAME;
    end;
  end;
end;

function TIDispatchProxy.GetTypeInfo(Index, LocaleID: Integer; out TypeInfo):
  hResult;
begin
  Pointer(TypeInfo) := nil;
  Result := E_NOTIMPL;
end;

function TIDispatchProxy.GetTypeInfoCount(out Count: Integer): hResult;
begin
  Count := 0;
  Result := S_OK;
end;

function TIDispatchProxy.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): hResult;
var
  DispParams: TDispParams;
  MethEntry: TIntfMethEntry;
  V: OleVariant;
  i: Integer;
  Context: TInvContext;
begin
  if (DispID >= Low(FIntfMD.MDA)) and (DispID <= High(FIntfMD.MDA)) then
  begin
    try
      MethEntry := FIntfMD.MDA[DispID];
      DispParams := TDispParams(Params);
      // ������漸������ʹ��Ĭ��ֵ
      if DispParams.cArgs <= MethEntry.ParamCount then
      begin
        // Var �� Out ���͵Ĳ�������ʡ��
        for i := DispParams.cArgs to MethEntry.ParamCount - 1 do
          if [pfVar, pfOut] * MethEntry.Params[i].Flags <> [] then
          begin
            Result := DISP_E_BADPARAMCOUNT;
            Exit;
          end;

        Context := TInvContext.Create;
        try
          Context.SetMethodInfo(MethEntry);
          Context.AllocServerData(MethEntry);
          
          // ���÷���ǰת�� OleVariant ����Ϊ��������
          for i := 0 to MethEntry.ParamCount - 1 do
          begin
            // �������ת�������˴��������Ĳ������
            PInteger(ArgErr)^ := i;
            // �������Ĳ����Ͷ����˳���෴
            if i < DispParams.cArgs then
              V := OleVariant(DispParams.rgvarg^[DispParams.cArgs - 1 - i])
            else
              V := Null;
            TypeTranslator.CastVariantToNative(MethEntry.Params[i].Info,
              V, Context.GetParamPointer(i));
          end;

          // ���ýӿڷ���
          InterfaceInvoker.Invoke(FObject, FIntfMD, DispID, Context);

          // ������ɺ�ת�� var �� out ����Ϊ OleVariant ����
          { TODO : JScript �� VBScript �ƺ���֧�ֱ��������� }
          for i := 0 to DispParams.cArgs - 1 do
            if [pfVar, pfOut] * MethEntry.Params[i].Flags <> [] then
            begin
              // �������ת�������˴��������Ĳ������
              PInteger(ArgErr)^ := i;
              // �������Ĳ����Ͷ����˳���෴
              TypeTranslator.CastNativeToVariant(MethEntry.Params[i].Info,
                V, Context.GetParamPointer(i));
              OleVariant(DispParams.rgvarg^[DispParams.cArgs - 1 - i]) := V;
            end;

          PInteger(ArgErr)^ := 0;
          
          // ���ط���ִ�н��
          if Assigned(MethEntry.ResultInfo) and Assigned(VarResult) then
          begin
            TypeTranslator.CastNativeToVariant(MethEntry.ResultInfo,
              V, Context.GetResultPointer);
            OleVariant(VarResult^) := V;
          end
          else if Assigned(VarResult) then
            OleVariant(VarResult^) := Null;
        finally
          Context.Free;
        end;

        Result := S_OK;
      end
      else
        Result := DISP_E_BADPARAMCOUNT;
    except
      on E: Exception do
      begin
        if E is ETypeTransException then
          Result := DISP_E_TYPEMISMATCH
        else if E is EInvalidCast then
          Result := DISP_E_TYPEMISMATCH
        else if E is EConvertError then
          Result := DISP_E_TYPEMISMATCH
        else if E is EOverflow then
          Result := DISP_E_OVERFLOW
        else
        begin
          Result := DISP_E_EXCEPTION;
          { TODO -oyygw : �����쳣ʱ�Ĵ�����Ϣ }
        end;
      end;
    end;
  end
  else
    Result := DISP_E_MEMBERNOTFOUND;
end;

end.

