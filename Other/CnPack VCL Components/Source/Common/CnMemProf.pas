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

unit CnMemProf;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��ڴ������Ԫ
* ��Ԫ���ߣ�Chinbo(Shenloqi@hotmail.com)
* ��    ע��ʹ������ʱ��Ҫ�����ŵ�Project�ļ���Uses�ĵ�һ������Ȼ������󱨡�
*           Ȼ���ڹ����м���
*             - mmPopupMsgDlg := True;
*               ������ڴ�й©���͵����Ի���
*             - mmShowObjectInfo := True;
*               ���ڴ�й©������RTTI���ͻᱨ����������
*             - ������ó���������ٶ����������趨
*               mmUseObjectList := False;
*               ���ܹ�������ϸ���ڴ�й©�ĵ�ַ�Լ�������Ϣ����ʹ�趨��
*               mmShowObjectInfo�������������ٶȸ�Delphi�Դ����ٶ����
*             - �������Ҫ�ڴ��鱨�棬�����趨
*               mmSaveToLogFile := False;
*             - ���Ҫ�Զ����¼�ļ��������趨
*               mmErrLogFile := '��ļ�¼�ļ���';
*               Ĭ���ļ���Ϊexe�ļ���Ŀ¼�µ�memory.log
*             - ����ʹ��SnapToFile����ץȡ�ڴ�����״̬��ָ���ļ���
*               �ڳ�����ֹʱ��OutputDebugString���ڴ�ʹ��״����
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ������ϱ��ػ�����ʽ
* �޸ļ�¼��
*           2004.09.18 V1.3
*               ��¼�滻�ڴ������֮ǰ��AllocMemCount
*               ʵ�ֱ��ػ�
*           2004.03.29 V1.2
*               Ϊ�ܿ�D6D7��TypInfo���µ��󱨣�ʹ�ñ���ָ����� RTTI ��Ϣ��¼��
*               �򿪱��뿪�� LOGRTTI ��������󱨣����� uses �� DB ��Ԫ��ʱ��
*           2003.09.21 V1.1
*               ����ʾ������Ϣ�����ڴ�й©ʱ�����һ�����з���鿴
*               �������趨mmErrLogFile����һ���ڴ�й©�ļ���
*               ԭ�������ڴ��������Ч֮ǰ��mmErrLogFileָ��������ü���Ϊ0��
*             ���������ڴ��������Ч֮���趨��������������ڴ�����������˿ռ䣬
*             ����Ϊ��ȫ�ֵ�mmErrLogFile���ø������������ü���ʼ�մ���0���ַ�
*             �����������ڴ�������ƽ�֮ǰ��ʱ�ͷţ����������ڴ�й©�ļ���
*           2002.08.06 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

// Ĭ�ϲ���¼ RTTI ��Ϣ������D67�µ� TypInfo ��Ԫ�������
// {$DEFINE LOGRTTI}  

var
  GetMemCount: Integer = 0;
  FreeMemCount: Integer = 0;
  ReallocMemCount: Integer = 0;

  mmPopupMsgDlg: Boolean = False;
  mmShowObjectInfo: Boolean = False;
  mmUseObjectList: Boolean = True;
  mmSaveToLogFile: Boolean = True;
  mmErrLogFile: string[255] = '';

procedure SnapToFile(Filename: string);

implementation

uses
  Windows, SysUtils, CnCommon, CnConsts{$IFDEF LOGRTTI}, TypInfo{$ENDIF};

const
  MaxCount = High(Word);

type
{$IFDEF SUPPORT_32_AND_64}
  TCnMemProofInteger = NativeInt;
{$ELSE}
  TCnMemProofInteger = Integer;
{$ENDIF}

var
  OldMemMgr: TMemoryManager;
  ObjList: array[0..MaxCount] of Pointer;
  FreeInList: Integer = 0;
  StartTime: DWORD;
  OldAllocMemCount: Integer;

{-----------------------------------------------------------------------------
  Procedure: AddToList
  Author:    Chinbo(Chinbo)
  Date:      06-����-2002
  Arguments: P: Pointer
  Result:    None
  ���ָ��
-----------------------------------------------------------------------------}

procedure AddToList(P: Pointer);
begin
  if FreeInList > High(ObjList) then
  begin
    MessageBox(0, SMemMgrOverflow, SCnPackMemMgr, mb_ok + mb_iconError);
    Exit;
  end;
  ObjList[FreeInList] := P;
  Inc(FreeInList);
end;

{-----------------------------------------------------------------------------
  Procedure: RemoveFromList
  Author:    Chinbo(Chinbo)
  Date:      06-����-2002
  Arguments: P: Pointer
  Result:    None
  �Ƴ�ָ��
-----------------------------------------------------------------------------}

procedure RemoveFromList(P: Pointer);
var
  I: Integer;
begin
  for I := Pred(FreeInList) downto 0 do
    if ObjList[I] = P then
    begin
      Dec(FreeInList);
      Move(ObjList[I + 1], ObjList[I], (FreeInList - I) * SizeOf(Pointer));
      Exit;
    end;
end;

{-----------------------------------------------------------------------------
  Procedure: SnapToFile
  Author:    Chinbo(Chinbo)
  Date:      06-����-2002
  Arguments: Filename: string
  Result:    None
  Modify:    �ܾ��� (zjy@cnpack.org) 2002.08.06
             Ϊ���㱾�ػ�����������һЩ����
             ����ɶ��Ա�ԭ���½� :-(
  ץȡ����
-----------------------------------------------------------------------------}

procedure SnapToFile(Filename: string);
var
  OutFile: TextFile;
  I, CurrFree, BlockSize: Integer;
  HeapStatus: THeapStatus;
  NowTime: DWORD;

  {$IFDEF LOGRTTI}
  Item: TObject;
  ptd: PTypeData;
  ppi: PPropInfo;
  {$ENDIF}

{-----------------------------------------------------------------------------
  Procedure: MSELToTime
  Author:    Chinbo(Chinbo)
  Date:      06-����-2002
  Arguments: const MSEL: DWORD
  Result:    string
  ת��ʱ��
-----------------------------------------------------------------------------}

  function MSELToTime(const MSEL: DWORD): string;
  begin
    Result := Format(SMemMgrRunTime,
      [MSEL div 3600000, MSEL div 60000, MSEL div 1000]);
  end;

begin
  AssignFile(OutFile, Filename);
  try
    if FileExists(Filename) then
      Append(OutFile)
    else
      Rewrite(OutFile);
    NowTime := GetTickCount - StartTime;
    HeapStatus := GetHeapStatus;
    with HeapStatus do
    begin
      Writeln(OutFile, ':::::::::::::::::::::::::::::::::::::::::::::::::::::');
      Writeln(OutFile, DateTimeToStr(Now));
      Writeln(OutFile);
      Writeln(OutFile, SAppRunTime + MSELToTime(NowTime));
      Writeln(OutFile, Format(SOldAllocMemCount, [OldAllocMemCount]));
      Writeln(OutFile, Format(SMemSpaceCanUse, [TotalAddrSpace div 1024]));
      Writeln(OutFile, Format(SUncommittedSpace, [TotalUncommitted div 1024]));
      Writeln(OutFile, Format(SCommittedSpace, [TotalCommitted div 1024]));
      Writeln(OutFile, Format(SFreeSpace, [TotalFree div 1024]));
      Writeln(OutFile, Format(SAllocatedSpace, [TotalAllocated div 1024]));
      if (TotalAllocated > 0) and (TotalAddrSpace > 0) then
        Writeln(OutFile, Format(SAllocatedSpacePercent, [TotalAllocated div (TotalAddrSpace div 100)]))
      else
        Writeln(OutFile, Format(SAllocatedSpacePercent, [0]));
      Writeln(OutFile, Format(SFreeSmallSpace, [FreeSmall div 1024]));
      Writeln(OutFile, Format(SFreeBigSpace, [FreeBig div 1024]));
      Writeln(OutFile, Format(SUnusedSpace, [Unused div 1024]));
      Writeln(OutFile, Format(SOverheadSpace, [Overhead div 1024]));
    end; //end with HeapStatus
    CurrFree := FreeInList;
    Writeln(OutFile);
    Write(OutFile, SObjectCountInMemory);
    if mmUseObjectList then
    begin
      Write(OutFile, CurrFree);
      if not mmShowObjectInfo then
        Writeln(OutFile);
    end
    else
    begin
      Write(OutFile, GetMemCount - FreeMemCount);
      if GetMemCount = FreeMemCount then
        Write(OutFile, SCommaString + SNoMemLeak)
      else
        Write(OutFile, SPeriodString);
      Writeln(OutFile);
    end; //end if mmUseObjectList
    if mmUseObjectList and mmShowObjectInfo then
    begin
      if CurrFree = 0 then
      begin
        Write(OutFile, SCommaString + SNoMemLeak);
        Writeln(OutFile);
      end
      else
      begin
        Writeln(OutFile);
        for I := 0 to CurrFree - 1 do
        begin
          BlockSize := PDWORD(DWORD(ObjList[I]) - 4)^;
          Write(OutFile, Format('%4d) %s - %4d', [I + 1,
            IntToHex(Cardinal(ObjList[I]), 16), BlockSize]));
          Write(OutFile, Format('($%s)%s - ', [IntToHex(BlockSize, 4), SByte]));

          {$IFDEF LOGRTTI}
          try
            Item := TObject(ObjList[I]);
            //Use RTTI, in IDE may raise exception, But not problems
            if PTypeInfo(Item.ClassInfo).Kind <> tkClass then
              Write(OutFile, SNotAnObject)
            else
            begin
              ptd := GetTypeData(PTypeInfo(Item.ClassInfo));
              //�Ƿ��������
              ppi := GetPropInfo(PTypeInfo(Item.ClassInfo), 'Name');
              if ppi <> nil then
              begin
                Write(OutFile, GetStrProp(Item, ppi));
                Write(OutFile, ' : ');
              end
              else
                Write(OutFile, SNoName + ': ');
              Write(OutFile, PTypeInfo(Item.ClassInfo).Name);
              Write(OutFile, Format(' (%d %s) - In %s.pas',
                [ptd.ClassType.InstanceSize, SByte, ptd.UnitName]));
            end; //end if GET RTTI
          except
            on Exception do
              Write(OutFile, SNotAnObject);
          end; //end try
          {$ENDIF}

          Writeln(OutFile);
        end;
      end; //end if CurrFree
    end; //end if mmUseObjectList and mmShowObjectInfo
  finally
    CloseFile(OutFile);
  end; //end try
end;

{-----------------------------------------------------------------------------
  Procedure: NewGetMem
  Author:    Chinbo(Chinbo)
  Date:      06-����-2002
  Arguments: Size: Integer
  Result:    Pointer
  �����ڴ�
-----------------------------------------------------------------------------}

function NewGetMem(Size: TCnMemProofInteger): Pointer;
begin
  Inc(GetMemCount);
  Result := OldMemMgr.GetMem(Size);
  if mmUseObjectList then
    AddToList(Result);
end;

{-----------------------------------------------------------------------------
  Procedure: NewFreeMem
  Author:    Chinbo(Chinbo)
  Date:      06-����-2002
  Arguments: P: Pointer
  Result:    Integer
  �ͷ��ڴ�
-----------------------------------------------------------------------------}

function NewFreeMem(P: Pointer): Integer;
begin
  Inc(FreeMemCount);
  Result := OldMemMgr.FreeMem(P);
  if mmUseObjectList then
    RemoveFromList(P);
end;

{-----------------------------------------------------------------------------
  Procedure: NewReallocMem
  Author:    Chinbo(Chinbo)
  Date:      06-����-2002
  Arguments: P: Pointer; Size: Integer
  Result:    Pointer
  ���·���
-----------------------------------------------------------------------------}

function NewReallocMem(P: Pointer; Size: TCnMemProofInteger): Pointer;
begin
  Inc(ReallocMemCount);
  Result := OldMemMgr.ReallocMem(P, Size);
  if mmUseObjectList then
  begin
    RemoveFromList(P);
    AddToList(Result);
  end;
end;

const
  NewMemMgr: TMemoryManager = (
    GetMem: NewGetMem;
    FreeMem: NewFreeMem;
    ReallocMem: NewReallocMem);

initialization
  StartTime := GetTickCount;
  OldAllocMemCount := AllocMemCount;
  GetMemoryManager(OldMemMgr);
  SetMemoryManager(NewMemMgr);

finalization
  SetMemoryManager(OldMemMgr);
  if (GetMemCount - FreeMemCount) <> 0 then
  begin
    if mmPopupMsgDlg then
      MessageBox(0, PChar(Format(SMemLeakDlgReport,
        [GetMemCount - FreeMemCount, OldAllocMemCount])), SCnPackMemMgr, MB_OK)
    else
      OutputDebugString(PChar(Format(SMemLeakDlgReport,
        [GetMemCount - FreeMemCount, OldAllocMemCount])));
  end;
  OutputDebugString(PChar(Format(SMemMgrODSReport,
    [GetMemCount, FreeMemCount, ReallocMemCount])));
  if mmErrLogFile = '' then
    mmErrLogFile := _CnExtractFilePath(ParamStr(0)) + 'Memory.Log';
  if mmSaveToLogFile then
    SnapToFile(mmErrLogFile);

end.

