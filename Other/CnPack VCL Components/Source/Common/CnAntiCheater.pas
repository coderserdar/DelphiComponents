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

unit CnAntiCheater;
{* |<PRE>
================================================================================
* ������ƣ��������������
* ��Ԫ���ƣ������� published �� Integer Get/Set �����ṩ���ݱ����Ļ�����ʵ�ֵ�Ԫ
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
* ��    ע��TCnAntiCheater ���ڴ������������� published �Ĵ� Get �� Set ����
            �� Integer ���Ե�ʱ���ҽ��������������ڶ�д��Щ���ԵĹ����в���һ
            �Զ���任���̣��Ӷ��ﵽ�ڴ�������ֵ�Ͷ�����ʾ��һ�µ�Ч���Զ㿪��Ϸ
            �޸�����׷�١�

            ǰ��������
            Get �� Set �������뱣��һ�����Ӷ��Ա�֤��������볤�� 5 �ֽڲ��ܹҽ�
            ����Ƽ��ڱ���ѡ���С��ر��Ż��������ɺ�����ջ������������֤���볤�ȣ�
            �����ڴ��Ż�������£�Get �� Set �����а����������������д��

            function TCnAntiCheater.GetData: Integer;
            begin
              if FData <> 0 then
                Result := FData
              else
                Result := 0;
            end;

            procedure TCnAntiCheater.SetData(const Value: Integer);
            begin
              if FData <> Value then
                FData := Value;
            end;

* ����ƽ̨��PWinXP + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2006.11.22 V1.0
*               ������Ԫ��ʵ�ֹ���
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, Windows, SysUtils, TypInfo, CnMethodHook;

type
  EHookException = class(Exception)
  end;

  TCnAntiCheater = class(TPersistent)
  {* �������ṩ published �� Integer �������ݱ����Ļ�����}
  private
    FData: Integer;
    function GetData: Integer;
    procedure SetData(const Value: Integer);
  protected
    function GetConvertCardinal(const Value: Cardinal): Cardinal; virtual;
    function SetConvertCardinal(const Value: Cardinal): Cardinal; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    // �ҽӺ�ʵ��ִ�е������������ĸ���Ʒ�������ɷ����ĵ�ַ�ѱ�����
    function MyGetCardinalData: Integer;
    procedure MySetCardinalData(const Value: Integer);

    class procedure HookSelfClass;
    class procedure UnHookSelfClass;

  published
    property Data: Integer read GetData write SetData;
  end;

  TCnClassHookItem = class(TObject)
  {* ÿһʵ��������ĳ��Ĺҽ���Ϣ}
  private
    FHookEnabled: Boolean;
    FHookers: TList;
    FHookProcs: TList;
    FHookClassName: string;
    function GetHookItems(Index: Integer): TCnMethodHook;
    procedure SetHookEnabled(const Value: Boolean);
    function GetHookCount: Integer;
    function GetHookProcs(Index: Integer): Pointer;

  public
    constructor Create;
    destructor Destroy; override;
    function AddHooker(AMethodHook: TCnMethodHook; AProc: Pointer): Integer;
    
    property HookEnabled: Boolean read FHookEnabled write SetHookEnabled;
    property HookClassName: string read FHookClassName write FHookClassName;
    property HookItems[Index: Integer]: TCnMethodHook read GetHookItems;
    property HookProcs[Index: Integer]: Pointer read GetHookProcs;
    property HookCount: Integer read GetHookCount;
  end;

implementation

const
  THUNK_SIZE = 4096; // x86 ҳ��С��ĿǰֻŪһ��ҳ��

  CALL_SIZE = 57; // GetCall �� SetCall �ṹ�ߴ��еĽϴ��

type
  TCnGetCall = packed record
    Code1: array[1..10] of Byte;
    HookInst1: Pointer;            // CnMethodHook ��ʵ����ַ
    Code2: array[1..9] of Byte;
    AddrGet: Pointer;              // ԭ�е� Get ��������Ե�ַ
    Code3: array[1..9] of Byte;
    OffSetConvert: Byte;           // Get ����õ� Convert ������ VMT �еĵ�ַƫ��
    Code4: array[1..4] of Byte;
    HookInst2: Pointer;            // CnMethodHook ��ʵ����ַ
    Code5: array[1..4] of Byte;
  end;
  PCnGetCall = ^TCnGetCall;

const
  SCnGetCall: array[1..57] of Byte = ($55, $8B, $EC, $83, $C4, $F8, $89, $45,
    $FC, $B8, $00, $00, $00, $00, $8B, $10, $FF, $52, $04, $8B, $45, $FC, $E8,
    $59, $FD, $FF, $FF, $8B, $D0, $8B, $45, $FC, $8B, $08, $FF, $51, $0C, $89,
    $45, $F8, $B8, $00, $00, $00, $00, $8B, $10, $FF, $52, $00, $8B, $45, $F8,
    $59, $59, $5D, $C3);

{
function TCnAntiCheater.MyGetCardinalData: Integer;
begin
  TCnMethodHook($33333333).UnHookMethod;
  Result := GetConvertCardinal(GetData);
  TCnMethodHook($33333333).HookMethod;
end;

CnAntiCheater.pas.377: begin
000C 55               push ebp
000D 8BEC             mov ebp,esp
000F 83C4F8           add esp,-$08
0012 8945FC           mov [ebp-$04],eax
CnAntiCheater.pas.378: TCnMethodHook($33333333).UnHookMethod;
0015 B833333333       mov eax,$33333333                          !
001A 8B10             mov edx,[eax]
001C FF5204           call dword ptr [edx+$04]
CnAntiCheater.pas.379: Result := GetConvertCardinal(GetData);
001F 8B45FC           mov eax,[ebp-$04]
0022 E859FDFFFF       call TCnAntiCheater.GetData                !
0027 8BD0             mov edx,eax
0029 8B45FC           mov eax,[ebp-$04]
002C 8B08             mov ecx,[eax]
002E FF510C           call dword ptr [ecx+$0c]                   !
0031 8945F8           mov [ebp-$08],eax
CnAntiCheater.pas.380: TCnMethodHook($33333333).HookMethod;
0034 B833333333       mov eax,$33333333                          !
0039 8B10             mov edx,[eax]
003B FF5200           call dword ptr [edx]
003E 8B45F8           mov eax,[ebp-$08]
CnAntiCheater.pas.381: end;
0041 59               pop ecx
0042 59               pop ecx
0043 5D               pop ebp
0044 C3               ret
}

type
  TCnSetCall = packed record
    Code1: array[1..13] of Byte;
    HookInst1: Pointer;            // CnMethodHook ��ʵ����ַ
    Code2: array[1..15] of Byte;
    OffSetConvert: Byte;           // Set ǰ���õ� Convert ������ VMT ƫ��
    Code3: array[1..6] of Byte;
    AddrSet: Pointer;              // ԭ�е� Set �����ĵ�ַ
    Code4: array[1..1] of Byte;
    HookInst2: Pointer;            // CnMethodHook ��ʵ����ַ
    Code5: array[1..9] of Byte;
  end;
  PCnSetCall = ^TCnSetCall;

const
  SCnSetCall: array[1..57] of Byte = ($55, $8B, $EC, $83, $C4, $F8, $89, $55,
    $F8, $89, $45, $FC, $B8, $00, $00, $00, $00, $8B, $10, $FF, $52, $04, $8B,
    $55, $F8, $8B, $45, $FC, $8B, $08, $FF, $51, $00, $8B, $D0, $8B, $45, $FC,
    $E8, $00, $00, $00, $00, $B8, $00, $00, $00, $00, $8B, $10, $FF, $52, $00,
    $59, $59, $5D, $C3);

{
procedure TCnAntiCheater.MySetCardinalData(const Value: Integer);
begin
  TCnWizMethodHook($33333333).UnHookMethod;
  SetData(SetConvertCardinal(Value));
  TCnMethodHook($33333333).HookMethod;
end;

CnAntiCheater.pas.384: begin
0048 55               push ebp
0049 8BEC             mov ebp,esp
004B 83C4F8           add esp,-$08
004E 8955F8           mov [ebp-$08],edx
0051 8945FC           mov [ebp-$04],eax
CnAntiCheater.pas.385: TCnMethodHook($33333333).UnHookMethod;
0054 B833333333       mov eax,$33333333                          !
0059 8B10             mov edx,[eax]
005B FF5204           call dword ptr [edx+$04]
CnAntiCheater.pas.386: SetData(SetConvertCardinal(Value));
005E 8B55F8           mov edx,[ebp-$08]
0061 8B45FC           mov eax,[ebp-$04]
0064 8B08             mov ecx,[eax]
0066 FF5110           call dword ptr [ecx+$10]                   !
0069 8BD0             mov edx,eax
006B 8B45FC           mov eax,[ebp-$04]
006E E829FDFFFF       call TCnAntiCheater.SetData                !
CnAntiCheater.pas.387: TCnMethodHook($33333333).HookMethod;
0073 B833333333       mov eax,$33333333                          !
0078 8B10             mov edx,[eax]
007A FF5200           call dword ptr [edx]
CnAntiCheater.pas.388: end;
007D 59               pop ecx
007E 59               pop ecx
007F 5D               pop ebp
0080 C3               ret
}

var
  ClassList: TStringList = nil;
  {* �洢��Ҫ Hook �� Class���� Objects �洢һ TCnClassHookItem}

  FHookPool: Pointer = nil;

  FEmptyPtr: Integer = 0;

  FEnableProtect: Boolean = False;

procedure InitHookPool;
begin
  FHookPool := VirtualAlloc(nil, THUNK_SIZE, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  if FHookPool = nil then
    raise EHookException.Create('Callback Pool Init Error!');
  FEmptyPtr := 0;
end;

// �����Ƿ��� Hook �б���
function SelfClassHooked(AClass: TClass): Boolean;
begin
  Result := (ClassList <> nil) and (ClassList.IndexOf(AClass.ClassName) >= 0);
end;

// �����Ƿ��Ѿ� Hooked
function SelfClassHookEnabled(AClass: TClass): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ClassList <> nil then
  begin
    I := ClassList.IndexOf(AClass.ClassName);
    if I >= 0 then
      Result := TCnClassHookItem(ClassList.Objects[I]).HookEnabled;
  end;
end;

// Ϊĳ�����һ HookItem ���б���
function AddHookItem(AClass: TClass): TCnClassHookItem;
var
  I: Integer;
  Item: TCnClassHookItem;
begin
  Result := nil;
  if ClassList = nil then
  begin
    ClassList := TStringList.Create;
    ClassList.Duplicates := dupIgnore;
  end;

  if ClassList.IndexOf(AClass.ClassName) >= 0 then Exit;
  I := ClassList.Add(AClass.ClassName);

  Item := TCnClassHookItem.Create;
  Item.HookClassName := AClass.ClassName;
  ClassList.Objects[I] := Item;
  Result := Item;
end;

// ����ĳ��� HookItem
function GetHookItemFromClassList(AClass: TClass): TCnClassHookItem;
var
  I: Integer;
begin
  Result := nil;
  if ClassList = nil then
  begin
    ClassList := TStringList.Create;
    ClassList.Duplicates := dupIgnore;
  end;

  I := ClassList.IndexOf(AClass.ClassName);
  if I >= 0 then
    Result := TCnClassHookItem(ClassList.Objects[I]);
end;

// ����ĳ�����Ƿ� Hook ��ϵı�־
procedure SetSelfClassHooked(AClass: TClass; Hooked: Boolean);
var
  I: Integer;
  Item: TCnClassHookItem;
begin
  if ClassList = nil then
  begin
    ClassList := TStringList.Create;
    ClassList.Duplicates := dupIgnore;
  end;

  I := ClassList.IndexOf(AClass.ClassName);
  if I >= 0 then
  begin
    Item := TCnClassHookItem(ClassList.Objects[I]);
  end
  else
  begin
    Item := AddHookItem(AClass);
  end;

  Item.HookEnabled := Hooked;
end;

// �ж�һ�����Ƿ��ѱ� Hook
function AProcHooked(AProc: Pointer): Boolean;
var
  I, J: Integer;
  HookItem: TCnClassHookItem;
begin
  Result := False;
  if ClassList <> nil then
  begin
    for I := 0 to ClassList.Count - 1 do
    begin
      HookItem := TCnClassHookItem(ClassList.Objects[I]);
      if HookItem <> nil then
      begin
        for J := 0 to HookItem.HookCount - 1 do
        begin
          if HookItem.HookProcs[J] = AProc then
          begin
            Result := True;
            Exit;
          end;
        end;
      end;
    end;
  end;
end;

{ TCnAntiCheater }

constructor TCnAntiCheater.Create;
begin
  if not SelfClassHooked(Self.ClassType) then
    HookSelfClass;
end;

destructor TCnAntiCheater.Destroy;
begin
  inherited;

end;

function TCnAntiCheater.GetConvertCardinal(const Value: Cardinal): Cardinal;
begin
  // Get ��ʱ����е�ת�������������
  Result := not Value;
end;

function TCnAntiCheater.SetConvertCardinal(const Value: Cardinal): Cardinal;
begin
  // Set ��ʱ����е�ת�������������
  Result := not Value;
end;

function TCnAntiCheater.GetData: Integer;
begin
  if FData <> 0 then
    Result := FData
  else
    Result := 0;
end;

procedure TCnAntiCheater.SetData(const Value: Integer);
begin
  if FData <> Value then
    FData := Value;
end;

class procedure TCnAntiCheater.HookSelfClass;
var
  HookItem: TCnClassHookItem;
  AGet: PCnGetCall;
  ASet: PCnSetCall;
  I, APropCount: Integer;
  PropInfo: PPropInfo;
  PropListPtr: PPropList;
  AHooker: TCnMethodHook;
begin
  if not FEnableProtect or SelfClassHookEnabled(Self) then Exit;
  HookItem := AddHookItem(Self);

  if FHookPool = nil then
    InitHookPool;

  APropCount := GetTypeData(PTypeInfo(Self.ClassInfo))^.PropCount;
  GetMem(PropListPtr, APropCount * SizeOf(Pointer));
  GetPropList(PTypeInfo(Self.ClassInfo), tkAny, PropListPtr);

  try
    for I := 0 to APropCount - 1 do
    begin
      PropInfo := PropListPtr^[I];
      if PropInfo^.PropType^^.Kind in [tkInteger] then // ��ʱֻ�ҽ� Integer ������
      begin
        // ע��Ԥ���Բ�ͬ�����ͬһ���������ظ��ҽ�
        if PropInfo^.GetProc <> nil then // �ҽӴ��� Get ����
        begin
          if not AProcHooked(PropInfo^.GetProc) then
          begin
            if FEmptyPtr = (THUNK_SIZE div CALL_SIZE) then
              raise EHookException.Create('Hook Pool Overflow!');
    
            AGet := PCnGetCall(Integer(FHookPool) + FEmptyPtr * CALL_SIZE);
            Inc(FEmptyPtr);
    
            Move(SCnGetCall, AGet^.Code1, SizeOf(SCnGetCall));
            AGet^.AddrGet := Pointer(Integer(PropInfo^.GetProc) - Integer(AGet) - 27);
            // 27 Ϊ�����תָ�������AGetͷ����ƫ�ƣ�������תָ���
            AGet^.OffSetConvert := $0C; // GetConvert �����ڱ��� VMT �е�ƫ�� $0c
    
            AHooker := TCnMethodHook.Create(PropInfo^.GetProc, AGet);
            AGet^.HookInst1 := AHooker;
            AGet^.HookInst2 := AHooker;
            HookItem.AddHooker(AHooker, PropInfo^.GetProc);
          end;
        end;
    
        if PropInfo^.SetProc <> nil then // �ҽӴ��� Set ����
        begin
          if not AProcHooked(PropInfo^.SetProc) then
          begin
            if FEmptyPtr = (THUNK_SIZE div CALL_SIZE) then
              raise EHookException.Create('Hook Pool Overflow!');
    
            ASet := PCnSetCall(Integer(FHookPool) + FEmptyPtr * CALL_SIZE);
            Inc(FEmptyPtr);
    
            Move(SCnSetCall, ASet^.Code1, SizeOf(SCnSetCall));
            ASet^.AddrSet := Pointer(Integer(PropInfo^.SetProc) - Integer(ASet) - 43);
            // 43 Ϊ�����תָ�������ASetͷ����ƫ�ƣ�������תָ���
            ASet^.OffSetConvert := $10; // SetConvert �����ڱ��� VMT �е�ƫ�� $10
    
            AHooker := TCnMethodHook.Create(PropInfo^.SetProc, ASet);
            ASet^.HookInst1 := AHooker;
            ASet^.HookInst2 := AHooker;
            HookItem.AddHooker(AHooker, PropInfo^.SetProc);
          end;
        end;
      end;
    end;
  finally
    FreeMem(PropListPtr);
  end;

  SetSelfClassHooked(Self, True);
end;

class procedure TCnAntiCheater.UnHookSelfClass;
var
  HookItem: TCnClassHookItem;
  I: Integer;
begin
  if not SelfClassHookEnabled(Self) then Exit;
  HookItem := GetHookItemFromClassList(Self);
  for I := 0 to HookItem.HookCount - 1 do
    HookItem.HookItems[I].UnhookMethod;

  SetSelfClassHooked(Self, False);
end;

// �˺���������ȡ�����Ļ�����ģ��
function TCnAntiCheater.MyGetCardinalData: Integer;
begin
  TCnMethodHook($33333333).UnHookMethod;
  Result := GetConvertCardinal(GetData);
  TCnMethodHook($33333333).HookMethod;
end;

// �˺���������ȡ�����Ļ�����ģ��
procedure TCnAntiCheater.MySetCardinalData(const Value: Integer);
begin
  TCnMethodHook($33333333).UnHookMethod;
  SetData(SetConvertCardinal(Value));
  TCnMethodHook($33333333).HookMethod;
end;

{ TCnClassHookItem }

function TCnClassHookItem.AddHooker(AMethodHook: TCnMethodHook; AProc: Pointer): Integer;
begin
  Result := FHookers.Add(AMethodHook);
  FHookProcs.Add(AProc);
end;

constructor TCnClassHookItem.Create;
begin
  FHookers := TList.Create;
  FHookProcs := TList.Create;
end;

destructor TCnClassHookItem.Destroy;
var
  I: Integer;
begin
  for I := 0 to FHookers.Count - 1 do
    TCnMethodHook(FHookers.Items[I]).Free;
  FHookers.Free;
  FHookProcs.Free;
  inherited;
end;

function TCnClassHookItem.GetHookCount: Integer;
begin
  Result := FHookers.Count;
end;

function TCnClassHookItem.GetHookItems(Index: Integer): TCnMethodHook;
begin
  Result := TCnMethodHook(FHookers.Items[Index]);
end;

function TCnClassHookItem.GetHookProcs(Index: Integer): Pointer;
begin
  Result := FHookProcs.Items[Index];
end;

procedure TCnClassHookItem.SetHookEnabled(const Value: Boolean);
var
  I: Integer;
begin
  if FHookEnabled <> Value then
  begin
    FHookEnabled := Value;
    if not FHookEnabled then
      for I := 0 to FHookers.Count - 1 do
        TCnMethodHook(FHookers.Items[I]).UnhookMethod;
  end;
end;

procedure ClearClassList;
var
  I: Integer;
begin
  if ClassList <> nil then
  begin
    for I := 0 to ClassList.Count - 1 do
      TCnClassHookItem(ClassList.Objects[I]).Free;
    FreeAndNil(ClassList);
  end;
end;

initialization
  FEnableProtect := True;

finalization
  ClearClassList;
  if FHookPool <> nil then
    VirtualFree(FHookPool, 0, MEM_RELEASE);

end.
