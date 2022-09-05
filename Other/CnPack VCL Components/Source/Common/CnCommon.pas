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

unit CnCommon;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ��������л����ⵥԪ
* ��Ԫ���ߣ�CnPack������
* ��    ע���õ�Ԫ������������Ļ������
* ����ƽ̨��PWin98SE + Delphi 5.0
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.02.25 by LiuXiao
*               ���Ӽ���й���½ 18 λ���֤���Ƿ���ȷ�ĺ���
*           2012.01.19 by LiuXiao
*               ����һ����ֲ�����ţ�˵Ŀ��ٿ����ŵ����ĺ���
*           2011.11.02 by LiuXiao
*               ���������� ccrun �İѳ��򶤵� Win7 �������ĺ���
*           2011.10.03 by LiuXiao
*               ����һ���ַ�װ�� Control �����ĺ��������ԶԸ� FMX ���
*           2007.01.31 by LiuXiao
*               ���ӻ�ȡһ�������������б�ĺ���
*           2006.11.29 by shenloqi
*               �޸���ShortNameToLongName������ʹ��֧��Win95/NT����֧��Linux��
*           2005.08.02 by shenloqi
*               ������SameCharCounts��CharCounts ��RelativePath��������д��
*               GetRelativePath����
*           2005.07.08 by shenloqi
*               �޸��� GetRelativePath �������޸��� FileMatchesExts ������������
*             һϵ��ͨ���֧�ֵĺ�����FileNameMatch��MatchExt��MatchFileName��
*             FileExtsToStrings��FileMasksToStrings��FileMatchesMasks
*           2005.05.03 by hubdog
*               ���� ExploreFile ����
*           2004.09.18 by Shenloqi
*               ΪDelphi5������ BoolToStr ����
*           2004.05.21 by Icebird
*               �޸��˺��� GetLine, IsInt, IsFloat, CnDateToStr, MyDateToStr
*           2003.10.29 by Shenloqi
*               �����ĸ�����CheckWinXP,DllGetVersion,GetSelText,UnQuotedStr
*           2002.08.12 V1.1
*               ����һ������ CheckAppRunning by �ܾ���
*           2002.04.09 V1.0
*               ����Ԫ������汾��
*           2002.03.17 V0.02
*               �������ֺ������������޸�
*           2002.01.30 V0.01
*               ������Ԫ�����������
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Math, Menus, PsAPI, Registry, ComObj, CnNativeDecl,
{$IFDEF COMPILER6_UP}
  StrUtils, Variants, Types,
{$ENDIF}
  FileCtrl, ShellAPI, CommDlg, MMSystem, StdCtrls, TLHelp32, ActiveX, ShlObj,
  CnConsts, CnIni, CnIniStrUtils, CheckLst, IniFiles, MultiMon, TypInfo;

//------------------------------------------------------------------------------
// �������Ͷ���
//------------------------------------------------------------------------------

type
  PRGBColor = ^TRGBColor;
  TRGBColor = packed record
    b, g, r: Byte;
  end;

  PRGBArray = ^TRGBArray;
  TRGBArray = array[0..65535] of TRGBColor;

  ENotImplementedException = class(Exception);

{$IFDEF COMPILER5}
  UTF8String = type string; // Delphi 5 has no UTF8String definition
{$ENDIF}

const
{$IFNDEF COMPILER6_UP}
  sLineBreak = {$IFDEF LINUX} #10 {$ENDIF} {$IFDEF MSWINDOWS} #13#10 {$ENDIF};
{$ENDIF}

  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];

  SCN_UTF16_ANSI_WIDE_CHAR_SEP = $900;

//==============================================================================
// Ansi �ַ�������
//==============================================================================

function AnsiTrim(const S: AnsiString): AnsiString;

function TrimBom(const S: AnsiString): AnsiString;

//------------------------------------------------------------------------------
// ��չ���ļ�Ŀ¼��������
//------------------------------------------------------------------------------

procedure ExploreDir(const APath: string; ShowDir: Boolean = True);
{* ����Դ�������д�ָ��Ŀ¼ }

procedure ExploreFile(const AFile: string; ShowDir: Boolean = True);
{* ����Դ�������д�ָ���ļ� }

function ForceDirectories(Dir: string): Boolean;
{* �ݹ鴴���༶��Ŀ¼}

function MoveFile(const sName, dName: string): Boolean;
{* �ƶ��ļ���Ŀ¼������ΪԴ��Ŀ����}

function DeleteToRecycleBin(const FileName: string): Boolean;
{* ɾ���ļ�������վ}

procedure FileProperties(const FName: string);
{* ���ļ����Դ���}

function OpenDialog(var FileName: string; const Title: string; const Filter: string;
  const Ext: string): Boolean;
{* ���ļ���}

function GetDirectory(const Caption: string; var Dir: string;
  ShowNewButton: Boolean = True): Boolean;
{* ��ʾѡ���ļ��жԻ���֧������Ĭ���ļ���}

function SelectDirectoryW(hOwn: HWND; var Path: WideString; const Caption,
  Root: WideString; uFlag: DWORD = $25): Boolean;
{* ���� SelectDirectory ������֧�� Unicode ��ѡ���ļ���}

function SelectDirectoryEx(hOwn: HWND; const Caption, Root: WideString): WideString;
{* �� SelectDirectoryW �ķ�װ}

function FormatPath(const APath: string; Width: Integer): string;
{* ������ʾ���µĳ�·����}

procedure DrawCompactPath(Hdc: HDC; Rect: TRect; const Str: string);
{* ͨ�� DrawText ��������·��}

procedure DrawMatchText(Canvas: TCanvas; const MatchStr, Text: string;
  X, Y: Integer; HighlightColor: TColor; MatchedIndexes: TList = nil;
  StartOffset: Integer = 1);
{* ��ָ�� Canvas �ϻ���ƥ����ַ�����ƥ�䲿�ָ�����ʾ}

function SameCharCounts(s1, s2: string): Integer;
{* �����ַ�����ǰ�����ͬ�ַ���}
function CharCounts(Str: PChar; Chr: Char): Integer;
{* ���ַ�����ĳ�ַ����ֵĴ���}
function GetRelativePath(ATo, AFrom: string;
  const PathStr: string = '\'; const ParentStr: string = '..';
  const CurrentStr: string = '.'; const UseCurrentDir: Boolean = False): string;
{* ȡ����Ŀ¼�����·��}

function CombineCommonPath(InFiles, OutFiles: TStrings): Boolean;
{* Ѱ��һ���ļ��Ĺ�ͬ��·��������ȥ����ͬ��·�����ļ������� OutFiles �У������Ƿ�ȥ���ɹ�}

{$IFNDEF BCB}
function PathRelativePathToA(pszPath: PAnsiChar; pszFrom: PAnsiChar; dwAttrFrom: DWORD;
  pszTo: PAnsiChar; dwAttrTo: DWORD): BOOL; stdcall;
function PathRelativePathToW(pszPath: PWideChar; pszFrom: PWideChar; dwAttrFrom: DWORD;
  pszTo: PWideChar; dwAttrTo: DWORD): BOOL; stdcall;
function PathRelativePathTo(pszPath: PChar; pszFrom: PChar; dwAttrFrom: DWORD;
  pszTo: PChar; dwAttrTo: DWORD): BOOL; stdcall;

function RelativePath(const AFrom, ATo: string; FromIsDir, ToIsDir: Boolean): string;
{* ʹ�� Windows API ȡ����Ŀ¼�����·��}
{$ENDIF}

function LinkPath(const Head, Tail: string): string;
{* ��������·����
   Head - ��·���������� C:\Test��\\Test\C\Abc��http://www.abc.com/dir/ �ȸ�ʽ
   Tail - β·���������� ..\Test��Abc\Temp��\Test��/web/lib �ȸ�ʽ����Ե�ַ��ʽ }

procedure RunFile(const FName: string; Handle: THandle = 0;
  const Param: string = '');
{* ����һ���ļ�}

procedure OpenUrl(const Url: string; UseCmd: Boolean = False);
{* ��һ������}

procedure MailTo(const Addr: string; const Subject: string = ''; UseCmd: Boolean = False);
{* �����ʼ�}

function WinExecute(const FileName: string; Visibility: Integer = SW_NORMAL): Boolean;
{* ����һ���ļ����������� }

function WinExecAndWait32(const FileName: string; Visibility: Integer = SW_NORMAL;
  ProcessMsg: Boolean = False): Integer;
{* ����һ���ļ����ȴ������}

function WinExecWithPipe(const CmdLine, Dir: string; slOutput: TStrings;
  var dwExitCode: Cardinal): Boolean; overload;
{* �ùܵ���ʽ�� Dir Ŀ¼ִ�� CmdLine��Output ���������Ϣ����Ϊʵʱ������ȴ�ִ�����
  ����� slOutput ��½������ֵ��dwExitCode �����˳��롣����ɹ����� True }

function WinExecWithPipe(const CmdLine, Dir: string; var Output: string;
  var dwExitCode: Cardinal): Boolean; overload;
{* �ùܵ���ʽ�� Dir Ŀ¼ִ�� CmdLine��Output ���������Ϣ���ȴ�ִ����Ϻ��ٷ��ؽ��ֵ
   dwExitCode �����˳��롣����ɹ����� True }

function CreateGuidString: string;
{* ���� GUID �ַ���}

function AppPath: string;
{* Ӧ�ó���·��}

function ModulePath: string;
{* ��ǰִ��ģ�����ڵ�·�� }

function GetProgramFilesDir: string;
{* ȡ Program Files Ŀ¼}

function GetWindowsDir: string;
{* ȡ Windows Ŀ¼}

function GetWindowsTempPath: string;
{* ȡ��ʱ�ļ�·��}

function CnGetTempFileName(const Ext: string): string;
{* ����һ����ʱ�ļ��� }

function GetSystemDir: string;
{* ȡϵͳĿ¼}

function GetMyDocumentsDir: string;
{* ȡ�ҵ��ĵ�Ŀ¼}

function ShortNameToLongName(const FileName: string): string;
{* ���ļ���ת���ļ���}

function LongNameToShortName(const FileName: string): string;
{* ���ļ���ת���ļ���}

function GetTrueFileName(const FileName: string): string;
{* ȡ����ʵ���ļ�����������Сд}

function FindExecFile(const AName: string; var AFullName: string): Boolean;
{* ���ҿ�ִ���ļ�������·�� }

function GetSpecialFolderLocation(const Folder: Integer): string;
{* ȡ��ϵͳ�����ļ���λ�ã�Folder ʹ���� ShlObj �ж���ı�ʶ���� CSIDL_DESKTOP }

function AddDirSuffix(const Dir: string): string;
{* Ŀ¼β�� '\' ����}

function MakePath(const Dir: string): string;
{* Ŀ¼β�� '\' ����}

function MakeDir(const Path: string): string;
{* ·��βȥ�� '\'}

function GetUnixPath(const Path: string): string;
{* ·���е� '\' ת�� '/'}

function GetWinPath(const Path: string): string;
{* ·���е� '/' ת�� '\'}

function FileNameMatch(Pattern, FileName: PAnsiChar): Integer;
{* �ļ����Ƿ���ͨ���ƥ�䣬����ֵΪ 0 ��ʾƥ�䣬����Ϊ��ƥ��}

function MatchExt(const S, Ext: string): Boolean;
{* �ļ����Ƿ�����չ��ͨ���ƥ��}

function MatchFileName(const S, FN: string): Boolean;
{* �ļ����Ƿ���ͨ���ƥ��}

procedure FileExtsToStrings(const FileExts: string; ExtList: TStrings; CaseSensitive: Boolean);
{* ת����չ��ͨ����ַ���Ϊͨ����б�}

function FileMatchesExts(const FileName, FileExts: string; CaseSensitive: Boolean = False): Boolean; overload;
function FileMatchesExts(const FileName: string; ExtList: TStrings): Boolean; overload;
{* �ļ����Ƿ�ƥ����չ��ͨ�����FileExts ���� '.pas;.dfm;.inc' �������ַ���}

procedure FileMasksToStrings(const FileMasks: string; MaskList: TStrings; CaseSensitive: Boolean);
{* ת���ļ�ͨ����ַ���Ϊͨ����б�}

function FileMatchesMasks(const FileName, FileMasks: string; CaseSensitive: Boolean): Boolean; overload;
function FileMatchesMasks(const FileName: string; MaskList: TStrings): Boolean; overload;
{* �ļ����Ƿ�ƥ��ͨ���}

function IsFileInUse(const FName: string): Boolean;
{* �ж��ļ��Ƿ�����ʹ��}

function IsAscii(const FileName: string): Boolean;
{* �ж��ļ��Ƿ�Ϊ Ascii �ļ�}

function IsValidFileName(const Name: string): Boolean;
{* �ж��ļ��Ƿ�����Ч���ļ���}

function GetValidFileName(const Name: string): string;
{* ������Ч���ļ��� }

function SetFileDate(const FileName: string; CreationTime, LastWriteTime, LastAccessTime:
  TFileTime): Boolean;
{* �����ļ�ʱ��}

function GetFileDate(const FileName: string; var CreationTime, LastWriteTime, LastAccessTime:
  TFileTime): Boolean;
{* ȡ�ļ�ʱ��}

function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
{* �ļ�ʱ��ת��������ʱ��}

function DateTimeToFileTime(const DateTime: TDateTime): TFileTime;
{* ��������ʱ��ת�ļ�ʱ��}

function GetFileIcon(const FileName: string; var Icon: TIcon): Boolean;
{* ȡ�����ļ���ص�ͼ�꣬�ɹ��򷵻�True}

function CreateBakFile(const FileName, Ext: string): Boolean;
{* ���������ļ�}

function FileTimeToLocalSystemTime(FTime: TFileTime): TSystemTime;
{* �ļ�ʱ��ת����ʱ��}

function LocalSystemTimeToFileTime(STime: TSystemTime): TFileTime;
{* ����ʱ��ת�ļ�ʱ��}

function DateTimeToLocalDateTime(DateTime: TDateTime): TDateTime;
{* UTC ʱ��ת����ʱ��}
function LocalDateTimeToDateTime(DateTime: TDateTime): TDateTime;
{* ����ʱ��ת UTC ʱ��}

procedure PinAppToWin7Taskbar(const Path, App: string);
{* �ѳ��򶤵� Windows 7 ������������Ϊ����·�����ļ���}

{$IFDEF COMPILER5}
type
  TValueRelationship = -1..1;

function CompareValue(const A, B: Int64): TValueRelationship;

function AnsiStartsText(const ASubText, AText: string): Boolean;
{* AText �Ƿ��� ASubText ��ͷ }

function AnsiReplaceText(const AText, AFromText, AToText: string): string;
{$ENDIF}

{$IFNDEF COMPILER7_UP}
function AnsiContainsText(const AText, ASubText: string): Boolean;
{* AText �Ƿ���� ASubText }
{$ENDIF}

function AnsiCompareTextPos(const ASubText, AText1, AText2: string): TValueRelationship;
function CompareTextPos(const ASubText, AText1, AText2: string): TValueRelationship;
{* �Ƚ� SubText �������ַ����г��ֵ�λ�õĴ�С����������Ƚ��ַ����������Դ�Сд }

function CompareTextWithPos(const ASubText, AText1, AText2: string;
  Reverse: Boolean): TValueRelationship;
{* �Ƚ� SubText �������ַ����г��ֵ�λ�õĴ�С��λ�ò�����
   �����Ƚ��ַ�����������ɷ��򣬺��Դ�Сд }

function StringReplaceNonAnsi(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
{* �� Ansi ��ʽ���ַ����滻}

function Deltree(const Dir: string; DelRoot: Boolean = True;
  DelEmptyDirOnly: Boolean = False): Boolean;
{* ɾ������Ŀ¼, DelRoot ��ʾ�Ƿ�ɾ��Ŀ¼����}

procedure DelEmptyTree(const Dir: string; DelRoot: Boolean = True);
{* ɾ������Ŀ¼�еĿ�Ŀ¼, DelRoot ��ʾ�Ƿ�ɾ��Ŀ¼����}

function GetDirFiles(const Dir: string; FileNames: TStrings = nil): Integer;
{* ȡ�ļ����µ�ֱϵ�ļ��б���������Ŀ¼�������ļ���}

type
  TFindCallBack = procedure(const FileName: string; const Info: TSearchRec;
    var Abort: Boolean) of object;
{* ����ָ��Ŀ¼���ļ��Ļص�����}

  TDirCallBack = procedure(const SubDir: string) of object;
{* ����ָ��Ŀ¼ʱ������Ŀ¼�ص�����}

function FindFile(const Path: string; const FileName: string = '*.*';
  Proc: TFindCallBack = nil; DirProc: TDirCallBack = nil; bSub: Boolean = True;
  bMsg: Boolean = True): Boolean;
{* ����ָ��Ŀ¼���ļ��������Ƿ��ж� }

function CnSearchFile(const FileName: string; const Ext: string = '.exe'): string;
{* ���� SearchFile �ڵ�ǰĿ¼��ϵͳĿ¼�� PATH �����в���ָ����չ�����ļ���
   FileName Ϊ������չ�����ļ�����Ext Ϊ��չ�����ɹ�����ȫ·���ļ�����ʧ�ܷ��ؿա�}

function OpenWith(const FileName: string): Integer;
{* ��ʾ�ļ��򿪷�ʽ�Ի���}

function CheckAppRunning(const FileName: string; var Running: Boolean): Boolean;
{* ���ָ����Ӧ�ó����Ƿ���������
 |<PRE>
   const FileName: string   - Ӧ�ó����ļ���������·�������������չ����
                              Ĭ��Ϊ".EXE"����Сд����ν��
                              �� Notepad.EXE
   var Running: Boolean     - ���ظ�Ӧ�ó����Ƿ����У�����Ϊ True
   Result: Boolean          - ������ҳɹ�����Ϊ True������Ϊ False
 |</PRE>}

type
  TVersionNumber = packed record
  {* �ļ��汾��}
    Major: Word;
    Minor: Word;
    Release: Word;
    Build: Word;
  end;

function GetFileVersionNumber(const FileName: string): TVersionNumber;
{* ȡ�ļ��汾��}

function GetFileVersionStr(const FileName: string): string;
{* ȡ�ļ��汾�ַ���}

function GetFileInfo(const FileName: string; var FileSize: Int64;
  var FileTime: TDateTime): Boolean;
{* ȡ�ļ���Ϣ}

function GetFileSize(const FileName: string): Int64;
{* ȡ�ļ�����}

function GetFileDateTime(const FileName: string): TDateTime;
{* ȡ�ļ�Delphi��ʽ����ʱ��}

function LoadStringFromFile(const FileName: string): string;
{* ���ļ���Ϊ�ַ���}

function SaveStringToFile(const S, FileName: string): Boolean;
{* �����ַ�����Ϊ�ļ�}

procedure QuickSortStringList(List: TStringList; L, R: Integer; SCompare: TStringListSortCompare);
{* StringList ���ţ��ֲ� D5��6 �����򲻿��ŵľ���}

//------------------------------------------------------------------------------
// �����������
//------------------------------------------------------------------------------

function DelEnvironmentVar(const Name: string): Boolean;
{* ɾ����ǰ�����еĻ������� }

function ExpandEnvironmentVar(var Value: string): Boolean;
{* ��չ��ǰ�����еĻ������� }

function GetEnvironmentVar(const Name: string; var Value: string;
  Expand: Boolean): Boolean;
{* ���ص�ǰ�����еĻ������� }

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
{* ���ص�ǰ�����еĻ��������б� }

function SetEnvironmentVar(const Name, Value: string): Boolean;
{* ���õ�ǰ�����еĻ������� }

//------------------------------------------------------------------------------
// ��չ���ַ�����������
//------------------------------------------------------------------------------

type
  TAnsiCharSet = set of AnsiChar;

  TCharSet = set of AnsiChar;

function CharInSet(C: Char; CharSet: TAnsiCharSet): Boolean;
{* �ж��ַ��Ƿ��ڼ�����}

function InStr(const sShort: string; const sLong: string): Boolean;
{* �ж� sShort �Ƿ������ sLong �У������ִ�Сд}

function IntToStrEx(Value: Integer; Len: Integer; FillChar: Char = '0'): string;
{* ��չ����ת�ַ�������}

function IntToStrSp(Value: Integer; SpLen: Integer = 3; Sp: Char = ',';
  ShowPlus: Boolean = False): string;
{* ���ָ������������ַ�ת��}

function IsFloat(const s: String): Boolean;
{* �ж��ַ����Ƿ��ת���ɸ�����}

function IsInt(const s: String): Boolean;
{* �ж��ַ����Ƿ��ת��������}

function IsDateTime(const s: string): Boolean;
{* �ж��ַ����Ƿ��ת���� DateTime }

function IsValidEmail(const s: string): Boolean;
{* �ж��Ƿ���Ч���ʼ���ַ }

function AverageNoOverflow(A, B: Integer): Integer;
{* �Բ�����ķ�ʽ�����������͵�����ƽ����}

function StrSpToInt(const Value: string; Sp: Char = ','): Int64;
{* ȥ���ַ����еķָ������ַ�ת��}

function ByteToBin(Value: Byte): string;
{* �ֽ�ת�����ƴ�}

function WordToBin(Value: Word): string;
{* ˫�ֽ�ת�����ƴ�}

function DWordToBin(Value: DWORD): string;
{* ���ֽ�ת�����ƴ�}

function StrRight(const Str: string; Len: Integer): string;
{* �����ַ����ұߵ��ַ�}

function StrLeft(const Str: string; Len: Integer): string;
{* �����ַ�����ߵ��ַ�}

function GetLine(C: Char; Len: Integer): string;
{* �����ַ�����}

function GetTextFileLineCount(const FileName: String): Integer;
{* �����ı��ļ�������}

function Spc(Len: Integer): string;
{* ���ؿո�}

procedure SwapStr(var s1, s2: string);
{* �����ִ�}

procedure SeparateStrAndNum(const AInStr: string; var AOutStr: string;
  var AOutNum: Integer);
{* �ָ�"������+����"��ʽ���ַ����еķ����ֺ�����}

function UnQuotedStr(const str: string; const ch: Char;
  const sep: string = ''): string;
{* ȥ�������õ��ַ���������}

function CharPosWithCounter(const Sub: Char; const AStr: String;
  Counter: Integer = 1): Integer;
{* �����ַ����г��ֵĵ� Counter �ε��ַ���λ�� }

function CountCharInStr(const Sub: Char; const AStr: string): Integer;
{* �����ַ������ַ��ĳ��ִ���}

function IsValidIdentChar(C: Char; First: Boolean = False): Boolean;
{* �ж��ַ��Ƿ���Ч��ʶ���ַ���First ��ʾ�Ƿ�Ϊ���ַ�}

function IsValidIdentW(const Ident: string): Boolean;
{* �ж��ַ����Ƿ�����Ч�� Unicode ��ʶ����ֻ�� Unicode �µ���}

function IsValidIdentWide(const Ident: WideString): Boolean;
{* �жϿ��ַ����Ƿ�����Ч�� Unicode ��ʶ����ֻ�� BDS ���ϵ���}

{$IFDEF COMPILER5}
function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
{* Delphi5û��ʵ�ֲ�����ת��Ϊ�ַ�����������Delphi6,7��ʵ��}
{$ENDIF COMPILER5}

function LinesToStr(const Lines: string): string;
{* �����ı�ת���У����з�ת'\n'��}

function StrToLines(const Str: string): string;
{* �����ı�ת���У�'\n'ת���з���}

function WideStrToLines(const Str: WideString): WideString;
{* ���п��ı�ת���У�'\n'ת���з���}

function MyDateToStr(Date: TDate): string;
{* ����ת�ַ�����ʹ�� yyyy.mm.dd ��ʽ}

function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
{* ȡע����ֵ}

function GetKeysInRegistryKey(const Key: string; List: TStrings): Boolean;
{* ȡע���ĳ�����Ӽ��б�}

procedure ReadStringsFromIni(Ini: TCustomIniFile; const Section: string; Strings: TStrings);
{* �� INI �ж�ȡ�ַ����б�}

procedure WriteStringsToIni(Ini: TCustomIniFile; const Section: string; Strings: TStrings);
{* д�ַ����б� INI �ļ���}

function VersionToStr(Version: DWORD): string;
{* �汾��ת���ַ������� $01020000 --> '1.2.0.0' }

function StrToVersion(const S: string): DWORD;
{* �ַ���ת�ɰ汾�ţ��� '1.2.0.0' --> $01020000�������ʽ����ȷ������ $01000000 }

function CnDateToStr(Date: TDateTime): string;
{* ת������Ϊ yyyy.mm.dd ��ʽ�ַ��� }

function CnStrToDate(const S: string): TDateTime;
{* �� yyyy.mm.dd ��ʽ�ַ���ת��Ϊ���� }

function GetDatePart(DateTime: TDateTime): TDate;
{* ȡ����ʱ������ڲ��֣�������}

function GetTimePart(DateTime: TDateTime): TTime;
{* ȡ����ʱ���ʱ�䲿�֣�С����}

function DateTimeToFlatStr(const DateTime: TDateTime): string;
{* ����ʱ��ת '20030203132345' ʽ���� 14 λ�����ַ���}

function FlatStrToDateTime(const Section: string; var DateTime: TDateTime): Boolean;
{* '20030203132345' ʽ���� 14 λ�����ַ���ת����ʱ��}

function RMBFloatToChinese(ARMBCash: Real): string;
{* ����ת��д���}

function EvalSimpleExpression(const Value: string): Double;
{* ��������������˷��ı��ʽֵ}

function FastInverseSqrt(X: Single): Single;
{* ���ټ��㿪���ŵĵ���}

function FastSqrt(N: LongWord): LongWord;
{* ��λȷ�������ټ���������ƽ��������������}

function FastSqrt64(N: Int64): Int64;
{* ��λȷ�������ټ���������ƽ��������������}

function StrToRegRoot(const s: string): HKEY;
{* �ַ���תע��������֧�� 'HKEY_CURRENT_USER' 'HKCR' �������ָ�ʽ}

function RegRootToStr(Key: HKEY; ShortFormat: Boolean = True): string;
{* ע������ת�ַ�������ѡ 'HKEY_CURRENT_USER' 'HKCR' �������ָ�ʽ}

function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TSysCharSet): string;
{* ���ַ����и���ָ���ķָ���������Ӵ�
 |<PRE>
   const S: string           - Դ�ַ���
   var Pos: Integer          - ������ҵ���ʼλ�ã����������ɵĽ���λ��
   const Delims: TSysCharSet - �ָ�������
   Result: string            - �����Ӵ�
 |</PRE>}

function WildcardCompare(const FileWildcard, FileName: string; const IgnoreCase:
  Boolean = True): Boolean;
{* �ļ���ͨ����Ƚ�}

function ScanCodeToAscii(Code: Word): AnsiChar;
{* ���ݵ�ǰ���̲��ֽ�����ɨ����ת���� ASCII �ַ������� WM_KEYDOWN �ȴ�ʹ��
   ���ڲ����� ToAscii���ʿ�֧��ʹ�� Accent Character �ļ��̲��� }

function IsDeadKey(Key: Word): Boolean;
{* ����һ��������Ƿ� Dead key}

function VirtualKeyToAscii(Key: Word): AnsiChar;
{* ���ݵ�ǰ����״̬�������ת���� ASCII �ַ������� WM_KEYDOWN �ȴ�ʹ��
   ���ܻᵼ�� Accent Character ����ȷ}

function VK_ScanCodeToAscii(VKey: Word; Code: Word): AnsiChar;
{* ���ݵ�ǰ�ļ��̲��ֽ��������ɨ����ת���� ASCII �ַ���ͨ�������������С���̣�
   ɨ���봦�����̣�֧�� Accent Character �ļ��̲��� }

function GetShiftState: TShiftState;
{* ���ص�ǰ�İ���״̬���ݲ�֧�� ssDouble ״̬ }

function IsShiftDown: Boolean;
{* �жϵ�ǰ Shift �Ƿ��� }

function IsAltDown: Boolean;
{* �жϵ�ǰ Alt �Ƿ��� }

function IsCtrlDown: Boolean;
{* �жϵ�ǰ Ctrl �Ƿ��� }

function IsInsertDown: Boolean;
{* �жϵ�ǰ Insert �Ƿ��� }

function IsCapsLockDown: Boolean;
{* �жϵ�ǰ Caps Lock �Ƿ��� }

function IsNumLockDown: Boolean;
{* �жϵ�ǰ NumLock �Ƿ��� }

function IsScrollLockDown: Boolean;
{* �жϵ�ǰ Scroll Lock �Ƿ��� }

function RemoveClassPrefix(const ClassName: string): string;
{* ɾ������ǰ׺ T}

function CnAuthorEmailToStr(Author, Email: string): string;
{* �÷ֺŷָ������ߡ������ַ���ת��Ϊ�����ʽ�����磺
 |<PRE>
   Author  = 'Tom;Jack;Bill'
   Email   = 'tom@email.com;jack@email.com;Bill@email.net'
   Result  = 'Tom(tom@email.com)' + #13#10 +
             'Jack(jack@email.com)' + #13#10 +
             'Bill(bill@email.net)
 |</PRE>}

//------------------------------------------------------------------------------
// ��չ�ĶԻ�����
//------------------------------------------------------------------------------

procedure InfoDlg(const Mess: string; Caption: string = ''; Flags: Integer
  = MB_OK + MB_ICONINFORMATION);
{* ��ʾ��ʾ����}

function InfoOk(const Mess: string; Caption: string = ''): Boolean;
{* ��ʾ��ʾȷ�ϴ���}

procedure ErrorDlg(const Mess: string; Caption: string = '');
{* ��ʾ���󴰿�}

procedure WarningDlg(const Mess: string; Caption: string = '');
{* ��ʾ���洰��}

function QueryDlg(const Mess: string; DefaultNo: Boolean = False;
  Caption: string = ''): Boolean;
{* ��ʾ��ѯ�Ƿ񴰿�}

const
  csDefComboBoxSection = 'History';

function CnInputQuery(const ACaption, APrompt: string;
  var Value: string; Ini: TCustomIniFile = nil;
  const Section: string = csDefComboBoxSection; APassword: Boolean = False): Boolean;
{* ����Ի���}

function CnInputBox(const ACaption, APrompt, ADefault: string;
   Ini: TCustomIniFile = nil; const Section: string = csDefComboBoxSection): string;
{* ����Ի���}

//------------------------------------------------------------------------------
// ��չ����ʱ���������
//------------------------------------------------------------------------------

function GetYear(Date: TDate): Integer;
{* ȡ������ݷ���}
function GetMonth(Date: TDate): Integer;
{* ȡ�����·ݷ���}
function GetDay(Date: TDate): Integer;
{* ȡ������������}
function GetHour(Time: TTime): Integer;
{* ȡʱ��Сʱ����}
function GetMinute(Time: TTime): Integer;
{* ȡʱ����ӷ���}
function GetSecond(Time: TTime): Integer;
{* ȡʱ�������}
function GetMSecond(Time: TTime): Integer;
{* ȡʱ��������}

//------------------------------------------------------------------------------
// λ��������
//------------------------------------------------------------------------------

type
  TByteBit = 0..7;
  {* Byte����λ����Χ}
  TWordBit = 0..15;
  {* Word����λ����Χ}
  TDWordBit = 0..31;
  {* DWord����λ����Χ}

procedure SetBit(var Value: Byte; Bit: TByteBit; IsSet: Boolean); overload;
{* ���ö�����λ}
procedure SetBit(var Value: WORD; Bit: TWordBit; IsSet: Boolean); overload;
{* ���ö�����λ}
procedure SetBit(var Value: DWORD; Bit: TDWordBit; IsSet: Boolean); overload;
{* ���ö�����λ}

function GetBit(Value: Byte; Bit: TByteBit): Boolean; overload;
{* ȡ������λ}
function GetBit(Value: WORD; Bit: TWordBit): Boolean; overload;
{* ȡ������λ}
function GetBit(Value: DWORD; Bit: TDWordBit): Boolean; overload;
{* ȡ������λ}

function CountSetBits(const Value: Cardinal): Integer;
{* ��������������ж���λ����Ϊ 1}

//------------------------------------------------------------------------------
// ϵͳ���ܺ���
//------------------------------------------------------------------------------

type
  PDLLVERSIONINFO = ^TDLLVERSIONINFO;
  TDLLVERSIONINFO = packed record
    cbSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
  end;
  PDLLVERSIONINFO2 = ^TDLLVERSIONINFO2;
  TDLLVERSIONINFO2 = packed record
    info1: TDLLVERSIONINFO;
    dwFlags: DWORD;
    ullVersion: ULARGE_INTEGER;
  end;

procedure MoveMouseIntoControl(AWinControl: TControl);
{* �ƶ���굽�ؼ�}

procedure AddComboBoxTextToItems(ComboBox: TComboBox; MaxItemsCount: Integer = 10);
{* �� ComboBox ���ı��������ӵ������б���}

function DynamicResolution(x, y: WORD): Boolean;
{* ��̬���÷ֱ���}

procedure StayOnTop(Handle: HWND; OnTop: Boolean);
{* �������Ϸ���ʾ}

procedure SetHidden(Hide: Boolean);
{* ���ó����Ƿ������������}

procedure SetTaskBarVisible(Visible: Boolean);
{* �����������Ƿ�ɼ�}

procedure SetDesktopVisible(Visible: Boolean);
{* ���������Ƿ�ɼ�}

function CnSetWindowAlphaBlend(Hwnd: THandle; Alpha: Byte): Boolean;
{* ���ô��� Alpha ͸��ֵ}

function ForceForegroundWindow(HWND: HWND): Boolean;
{* ǿ����һ��������ʾ��ǰ̨}

function GetWorkRect(const Form: TCustomForm = nil): TRect;
{* ȡ��������}

procedure BeginWait;
{* ��ʾ�ȴ����}

procedure EndWait;
{* �����ȴ����}

function CheckWindows9598: Boolean;
{* ����Ƿ�Win95/98ƽ̨}

function CheckWinXP: Boolean;
{* ����Ƿ�WinXP����ƽ̨}

function CheckWinVista: Boolean;
{* ����Ƿ� Vista/Win7 ����ϵͳ }

function CheckWow64: Boolean;
{* ����Ƿ� 64bit ϵͳ }

function CheckXPManifest(var OSSupport, AppValid: Boolean): Boolean;
{* ���ϵͳ�͵�ǰ�����Ƿ�֧�� XP Manifest
   OSSupport: ���ز���ϵͳ�Ƿ�֧�� XP Manifest
   AppValid: ���ص�ǰ�����Ƿ������� XP Manifest
   Result: ����Ƿ�ɹ� }

function DllGetVersion(const dllname: string;
  var DVI: TDLLVERSIONINFO2): Boolean;
{* ���Dll�İ汾��Ϣ}

function GetOSString: string;
{* ���ز���ϵͳ��ʶ��}

function GetComputeNameStr : string;
{* �õ�������}

function GetLocalUserName: string;
{* �õ������û���}

function GetRegisteredCompany: string;
{* �õ���˾��}

function GetRegisteredOwner: string;
{* �õ�ע���û���}

//------------------------------------------------------------------------------
// ���ÿؼ���������
//------------------------------------------------------------------------------

procedure ListViewDeleteSelected(ListView: TListView);
{* ɾ�� ListView ��ǰѡ���� }

procedure ListViewMoveDownSelected(ListView: TListView);
{* ListView ��ǰѡ���������ƶ� }

procedure ListViewMoveUpSelected(ListView: TListView);
{* ListView ��ǰѡ���������ƶ� }

procedure ListboxHorizontalScrollbar(Listbox: TCustomListBox);
{* Ϊ Listbox ����ˮƽ������}

procedure CloneMenuItem(Source, Dest: TMenuItem);
{* ���Ʋ˵����������}

//------------------------------------------------------------------------------
// ��������
//------------------------------------------------------------------------------

function GetControlBitmap(AControl: TControl; Bmp: TBitmap; ResetSize: Boolean = False): Boolean;
{* ��ȡ Control �����λͼ��ResetSize Ϊ True ��ʾʹ�� Control �ߴ�����λͼ�ߴ�}

function GetMultiMonitorDesktopRect: TRect;
{* ��ö���ʾ������£������������������ʾ��ԭ�������}

function TrimInt(Value, Min, Max: Integer): Integer;
{* ���������Min..Max֮��}

function CompareInt(V1, V2: Integer; Desc: Boolean = False): Integer;
{* �Ƚ�����������V1 > V2 ���� 1��V1 < V2 ���� -1��V1 = V2 ���� 0
   ��� Desc Ϊ True�����ؽ������ }

function IntToByte(Value: Integer): Byte;
{* ���������0..255֮��}

function InBound(Value: Integer; V1, V2: Integer): Boolean;
{* �ж�����Value�Ƿ���V1��V2֮��}

function SameMethod(Method1, Method2: TMethod): Boolean;
{* �Ƚ�����������ַ�Ƿ����}

function HalfFind(List: TList; P: Pointer; SCompare: TListSortCompare): Integer;
{* ���ַ��������б��в���}

function CheckChineseIDCardNumber(const IDNumber: string): Boolean;
{* ����й���½�� 18 λ���֤�Ƿ�Ϸ�}

type
  TFindRange = record
    tgFirst: Integer;
    tgLast: Integer;
  end;

function HalfFindEx(List: TList; P: Pointer; SCompare: TListSortCompare): TFindRange;
{* ���ַ��������б��в��ң�֧���ظ���¼������һ����Χֵ}

procedure CnSwap(var A, B: Byte); overload;
{* ����������}
procedure CnSwap(var A, B: Integer); overload;
{* ����������}
procedure CnSwap(var A, B: Single); overload;
{* ����������}
procedure CnSwap(var A, B: Double); overload;
{* ����������}

function RectEqu(Rect1, Rect2: TRect): Boolean;
{* �Ƚ�����Rect�Ƿ����}

procedure DeRect(Rect: TRect; var x, y, Width, Height: Integer);
{* �ֽ�һ��TRectΪ���Ͻ�����x, y�Ϳ��Width���߶�Height}

function EnSize(cx, cy: Integer): TSize;
{* ����һ��TSize����}

function RectWidth(Rect: TRect): Integer;
{* ����TRect�Ŀ��}

function RectHeight(Rect: TRect): Integer;
{* ����TRect�ĸ߶�}

procedure Delay(const uDelay: DWORD);
{* ��ʱ}

procedure SetClipboardContent(Format: Word; var Buffer; Size: Integer);
{* ��ָ���ڴ�������ָ����ʽ�����������}

{$IFNDEF WIN64}
procedure BeepEx(const Freq: WORD = 1200; const Delay: WORD = 1);
{* ��Win9X�������ȷ���}
{$ENDIF}

function GetLastErrorMsg(IncludeErrorCode: Boolean = False): string;
{* ȡ�����һ�δ�����Ϣ}

procedure ShowLastError;
{* ��ʾWin32 Api���н����Ϣ}

function GetHzPy(const AHzStr: AnsiString): AnsiString;
{* ȡ���ֵ�ƴ��}

{$IFDEF UNICODE}
function GetHzPyW(const AHzStr: string): string;
{* ȡ���ֵ�ƴ��������Ϊ Utf16}
{$ENDIF}

function TextFullWidthToHalfWidth(const Text: string): string;
{* ȫ���ַ�ת��Ϊ����ַ������о��"��"תΪ"."���ٺ�"��"תΪ","}

function TextHalfWidthToFullWidth(const Text: string): string;
{* ����ַ�ת��Ϊȫ���ַ�}

function GetSelText(edt: TCustomEdit): string;
{* ���CustomEditѡ�е��ַ���������ȷ����ʹ����XP��ʽ�ĳ���}

function SoundCardExist: Boolean;
{* �����Ƿ����}

function FindFormByClass(AClass: TClass): TForm;
{* ����ָ���������Ҵ���}

function ModalFormExists: Boolean;
{* ��ǰ�Ƿ���ģ̬���ڴ���}

function InheritsFromClassName(ASrc: TClass; const AClass: string): Boolean; overload;
{* �ж� ASrc �Ƿ�����������Ϊ AClass ���� }

function InheritsFromClassName(AObject: TObject; const AClass: string): Boolean; overload;
{* �ж� AObject �Ƿ�����������Ϊ AClass ���� }

function AdjustDebugPrivilege(Enable: Boolean): Boolean;
{* ��������Ȩ�޵� SeDebug ��ȡ����Ȩ��}

{$IFNDEF WIN64}

function PEBIsDebugged: Boolean;
{* ���� PEB �ṹ�������ж��������Ƿ񱻵�����}

{$ENDIF}

function NtIsDeugged: Boolean;
{* ʹ�� NtQueryInformationProcess ���ж��������Ƿ񱻵�����}

procedure KillProcessByFileName(const FileName: String);
{* �����ļ����������̣�������·��}

function IndexStr(const AText: string; AValues: array of string; IgCase: Boolean = True): Integer;
{* �����ַ����ڶ�̬�����е�����������string����ʹ��Case���}

function IndexInt(ANum: Integer; AValues: array of Integer): Integer;
{* �������α����ڶ�̬�����е����������ڱ���ʹ��Case���}

procedure TrimStrings(AList: TStrings);
{* ɾ�����к�ÿһ�е�����β�ո� }

//==============================================================================
// �������Բ�����غ��� by LiuXiao
//==============================================================================

function GetPropInfoIncludeSub(Instance: TObject; const PropName: string;
  AKinds: TTypeKinds = []): PPropInfo;
{* ��ü���������Ϣ}

function GetPropValueIncludeSub(Instance: TObject; PropName: string;
    PreferStrings: Boolean = True): Variant;
{* ��ü�������ֵ}

function SetPropValueIncludeSub(Instance: TObject; const PropName: string;
  const Value: Variant; AOwner: TComponent = nil): Boolean;
{* ���ü�������ֵ}

procedure DoSetPropValueIncludeSub(Instance: TObject; const PropName: string;
  Value: Variant; AOwner: TComponent = nil);
{* ���ü�������ֵ���������쳣}

function StrToSetValue(const Value: string; PInfo: PTypeInfo): Integer;
{* �ַ���ת����ֵ }

function PropInfoName(PropInfo: PPropInfo): string;
{* ȡ�� PropInfo �� Name }

function TypeInfoName(TypeInfo: PTypeInfo): string;
{* ȡ�� TypeInfo �� Name }

procedure GetAllPropNames(AComp: TObject; PropNames: TStrings;
  const BaseName: string = ''; IncludeType: Boolean = False);
{* ���ĳ������������Ե��ַ���ֵ�����������Ե�����
   IncludeType Ϊ True ʱ����ʽΪ Name=TypeName��Object�з��� PropType }

//==============================================================================
// ��������� by LiuXiao
//==============================================================================

type
  TCnFontControl = class(TControl)
  public
    property ParentFont;
    property Font;
  end;

function IsParentFont(AControl: TControl): Boolean;
{* �ж�ĳ Control �� ParentFont �����Ƿ�Ϊ True������ Parent �򷵻� False }

function GetParentFont(AControl: TComponent): TFont;
{* ȡĳ Control �� Parent �� Font ���ԣ����û�з��� nil }

const
  InvalidFileNameChar: set of AnsiChar = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];

function _CnPChar(const S: string): {$IFDEF UNICODE_STRING} PAnsiChar; inline {$ELSE} PChar {$ENDIF};
{* ��װ�� PChar ת���������� D2009 ������ǰ�汾 IDE ��ͬʱʹ��}

function _CnExtractFileExt(const FileName: string): string;
{* ��ExtractFileExt�ķ�װ��Delphi XE3��ExtractFileExt��Ϊ������TStringHelper.LastDelimiter����ExtractFileExt('.dpr')���ٷ���'.dpr'���Ƿ��ؿ�ֵ��}

function _CnExtractFileName(const FileName: string): string;
{* ��ExtractFileName�ķ�װ����ֹDelphi XE3��TStringHelper.LastDelimiter����Ĳ�����}

function _CnExtractFileDir(const FileName: string): string;
{* ��ExtractFileDir�ķ�װ����ֹDelphi XE3��TStringHelper.LastDelimiter����Ĳ�����}

function _CnExtractFilePath(const FileName: string): string;
{* ��ExtractFilePath�ķ�װ����ֹDelphi XE3��TStringHelper.LastDelimiter����Ĳ�����}

function _CnChangeFileExt(const FileName, Extension: string): string;
{* ��ChangeFileExt�ķ�װ����ֹDelphi XE3��TStringHelper.LastDelimiter����Ĳ�����}

function CnUtf8ToAnsi(const Text: AnsiString): AnsiString;
function CnUtf8ToAnsi2(const Text: string): string;
{* Ansi ���ת�� Utf8 �� Ansi �ַ������Խ�� D2009 �� Utf8ToAnsi �� UString ������ }

function CnAnsiToUtf8(const Text: AnsiString): AnsiString;
function CnAnsiToUtf82(const Text: string): string;
{* Ansi ���ת�� Ansi �� Utf8 �ַ������Խ�� D2009 �� AnsiToUtf8 �� UString ������ }

{$IFNDEF UNICODE}

function CnUtf8EncodeWideString(const S: WideString): AnsiString;
{* �� WideString ���� Utf8 ����õ� AnsiString������ Ansi ת�����ⶪ�ַ�}

function CnUtf8DecodeToWideString(const S: AnsiString): WideString;
{* �� AnsiString �� Utf8 ����õ� WideString������ Ansi ת�����ⶪ�ַ�}

{$ENDIF}

function WideStringReplace(const S, OldPattern, NewPattern: Widestring): Widestring;
{* WideString ��ȫ���滻ʵ�֣����ִ�Сд}

function DoubleEqual(const D1, D2: Double): Boolean;
{* �жϸ������Ƿ���ȣ����㹻С��}

function ExtendedEqual(const E1, E2: Extended): Boolean;
{* �жϸ������Ƿ���ȣ����㹻С��}

function SingleEqual(const S1, S2: Single): Boolean;
{* �жϸ������Ƿ���ȣ����㹻С��}

function CodePageOnlySupportsEnglish: Boolean;
{* �жϵ�ǰƽ̨�Ƿ�ֻ֧��Ӣ�ģ����ڴ����ֹ Unicode -> Ansi ʱ���ַ�������}

function WideCharIsWideLength(const AWChar: WideChar): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* �����ж�һ�� Unicode ���ַ��Ƿ�ռ�����ַ����}

function CalcAnsiLengthFromWideString(Text: PWideChar; VisualMode: Boolean = True): Integer;
{* ���� Unicode ���ַ����� Ansi ���ȣ�����ת Ansi ��� Length��������ת Ansi���Է�ֹ��Ӣ��ƽ̨�¶��ַ�
   VisualMode Ϊ True ʱ�Դ����ַ�����жϣ�Ϊ False ʱ�Դ������ $FF �жϡ�}

function CalcAnsiLengthFromWideStringOffset(Text: PWideChar; WideOffset: Integer; VisualMode: Boolean = True): Integer;
{* ���� Unicode ���ַ����� 1 �� WideOffset ���Ӵ��� Ansi ���ȣ�WideOffset �� 1 ��ʼ��
   ���� Copy(1, WideOffset) ����Ӵ�ת Ansi ȡ Length��������ʵ��ת Ansi���Է�ֹ��Ӣ��ƽ̨�¶��ַ�
   VisualMode Ϊ True ʱ�Դ����ַ�����жϣ�Ϊ False ʱ�Դ������ $FF �жϡ�}

function CalcWideStringLengthFromAnsiOffset(Text: PWideChar; AnsiOffset: Integer;
  VisualMode: Boolean = True; AllowExceedEnd: Boolean = False): Integer;
{* ���� Unicode ���ַ���ָ�� Ansi �Ӵ����ȶ�Ӧ�� Unicode �Ӵ����ȣ�AnsiOffset �� 1 ��ʼ��
   ����ת Ansi ��� Copy(1, AnsiOffset) ��ת���� Unicode ��ȡ Length�������� Ansi/Unicode ��ת���Է�ֹ��Ӣ��ƽ̨�¶��ַ�
   ע�� Ansi ��� Copy ���ܻ����˫�ֽ��ַ���
   AllowExceedEnd Ϊ False ʱ�����㵽 #0 �����ֹ�������� #0��Ϊ True ʱ���Բ��ո�ʽ����
   VisualMode Ϊ True ʱ�Դ����ַ�����жϣ�Ϊ False ʱ�Դ������ $FF �жϡ�}

function CalcUtf8StringLengthFromWideOffset(Utf8Text: PAnsiChar; WideOffset: Integer): Integer;
{* ���� Utf8 �ַ���ת���� WideSting ��ָ�� Wide �Ӵ����ȶ�Ӧ�� Utf8 �ַ������ȣ�WideOffset �� 1 ��ʼ��
   ����ת WideString �� Copy(1, WideOffset) ��ת�� Utf8 ��ȡ Length�������� Utf8/WideString ��ת���Ա������ı�������}

function CalcUtf8LengthFromWideChar(AChar: WideChar): Integer; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
{* ����һ�� WideChar ת���� Utf8 ����ַ�����}

function CalcUtf8LengthFromWideString(Text: PWideChar): Integer;
{* ������ַ����� Utf8 ���ȣ����� Utf8Encode ��ȡ Length������ʵ��ת��}

function CalcUtf8LengthFromUtf8HeadChar(AChar: AnsiChar): Integer;
{* ����һ�� Utf8 ǰ���ַ���������ַ�����}

function ConvertUtf16ToAlterAnsi(WideText: PWideChar; AlterChar: AnsiChar = ' '): AnsiString;
{* �ֶ������ַ���ת���� Ansi�������еĿ��ַ����滻������ AlterChar�����ڴ�Ӣ�Ļ����µ��ַ���ȼ���}

function ConvertUtf8ToAlterAnsi(Utf8Text: PAnsiChar; AlterChar: AnsiChar = ' '): AnsiString;
{* �ֶ��� Utf8 �ַ���ת���� Ansi�������еĿ��ַ����滻������ AlterChar�����ڴ�Ӣ�Ļ����µ��ַ���ȼ���}

implementation

const
  MINOR_DOUBLE = 1E-8;
  MINOR_EXTENDED = 1E-10;
  MINOR_SINGLE = 1E-6;
  // ��ͬ���͸������ж����ʱʹ�õĲ�ֵ�������峡�϶������в���׼ȷ��

type
  TNtQueryInformationProcess = function(ProcessHandle: THANDLE; ProcessInformationClass: DWORD;
    ProcessInformation: Pointer; ProcessInformationLength: ULONG; ReturnLength: PULONG): LongInt; stdcall;
var
  NtQueryInformationProcess: TNtQueryInformationProcess = nil;
  NtDllHandle: THandle = 0;

function DoubleEqual(const D1, D2: Double): Boolean;
begin
  Result := Abs(D1 - D2) < MINOR_DOUBLE;
end;

function ExtendedEqual(const E1, E2: Extended): Boolean;
begin
  Result := Abs(E1 - E2) < MINOR_EXTENDED;
end;

function SingleEqual(const S1, S2: Single): Boolean;
begin
  Result := Abs(S1 - S2) < MINOR_SINGLE;
end;

// �жϵ�ǰƽ̨�Ƿ�ֻ֧��Ӣ�ģ����ڴ����ֹ Unicode -> Ansi ʱ���ַ�������
function CodePageOnlySupportsEnglish: Boolean;
begin
  Result := (GetACP = 1252); // ANSI LATIN
end;

// �����ж�һ�� Unicode ���ַ��Ƿ�ռ�����ַ����
function WideCharIsWideLength(const AWChar: WideChar): Boolean; {$IFDEF SUPPORT_INLINE} inline; {$ENDIF}
begin
  Result := Ord(AWChar) > SCN_UTF16_ANSI_WIDE_CHAR_SEP; // ������Ϊ�� $900 ��� Utf16 �ַ���ռ���ֽ�
end;

// ���� Unicode ���ַ����� Ansi ���ȣ�����ת Ansi ��� Length��������ת Ansi���Է�ֹ��Ӣ��ƽ̨�¶��ַ�
function CalcAnsiLengthFromWideString(Text: PWideChar; VisualMode: Boolean): Integer;
begin
  Result := 0;
  if Text <> nil then
  begin
    if VisualMode then
    begin
      while Text^ <> #0 do
      begin
        if WideCharIsWideLength(Text^) then
          Inc(Result, SizeOf(WideChar))
        else
          Inc(Result, SizeOf(AnsiChar));
        Inc(Text);
      end;
    end
    else
    begin
      while Text^ <> #0 do
      begin
        if Ord(Text^) > $FF then
          Inc(Result, SizeOf(WideChar))
        else
          Inc(Result, SizeOf(AnsiChar));
        Inc(Text);
      end;
    end;
  end;
end;

// ���� Unicode ���ַ����� 1 �� WideOffset ���Ӵ��� Ansi ���ȣ�WideOffset �� 1 ��ʼ��
function CalcAnsiLengthFromWideStringOffset(Text: PWideChar; WideOffset: Integer;
  VisualMode: Boolean): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text <> nil) and (WideOffset > 0) then
  begin
    Idx := 0;
    if VisualMode then
    begin
      while (Text^ <> #0) and (Idx < WideOffset) do
      begin
        if WideCharIsWideLength(Text^) then
          Inc(Result, SizeOf(WideChar))
        else
          Inc(Result, SizeOf(AnsiChar));
        Inc(Text);
        Inc(Idx);
      end;
    end
    else
    begin
      while (Text^ <> #0) and (Idx < WideOffset) do
      begin
        if Ord(Text^) > $FF then
          Inc(Result, SizeOf(WideChar))
        else
          Inc(Result, SizeOf(AnsiChar));
        Inc(Text);
        Inc(Idx);
      end
    end;
  end;
end;

// ���� Unicode ���ַ���ָ�� Ansi �Ӵ����ȶ�Ӧ�� Unicode �Ӵ����ȣ�AnsiOffset �� 1 ��ʼ��
function CalcWideStringLengthFromAnsiOffset(Text: PWideChar; AnsiOffset: Integer;
  VisualMode: Boolean; AllowExceedEnd: Boolean): Integer;
var
  Idx: Integer;
begin
  Result := 0;
  if (Text <> nil) and (AnsiOffset > 0) then
  begin
    Idx := 0;
    if VisualMode then
    begin
      while (Text^ <> #0) and (Idx < AnsiOffset) do
      begin
        if WideCharIsWideLength(Text^) then
          Inc(Idx, SizeOf(WideChar))
        else
          Inc(Idx, SizeOf(AnsiChar));
        Inc(Text);
        Inc(Result);
      end;
    end
    else
    begin
      while (Text^ <> #0) and (Idx < AnsiOffset) do
      begin
        if Ord(Text^) > $FF then
          Inc(Idx, SizeOf(WideChar))
        else
          Inc(Idx, SizeOf(AnsiChar));
        Inc(Text);
        Inc(Result);
      end;
    end;

    if AllowExceedEnd and (Text^ = #0) and (Idx < AnsiOffset) then
      Inc(Result, AnsiOffset - Idx);
  end;
end;

// ���� Utf8 �ַ���ת���� WideSting ��ָ�� Wide �Ӵ����ȶ�Ӧ�� Utf8 �ַ������ȣ�WideOffset �� 1 ��ʼ��
// ����ת WideString �� Copy(1, WideOffset) ��ת�� Utf8 ��ȡ Length�������� Utf8/WideString ��ת���Ա������ı�������
function CalcUtf8StringLengthFromWideOffset(Utf8Text: PAnsiChar;
  WideOffset: Integer): Integer;
var
  Utf8Len, WideIdx: Integer;
begin
  Result := 0;
  if (Utf8Text = nil) or (WideOffset <= 0) then
    Exit;

  WideIdx := 0;
  while (Utf8Text^ <> #0) and (WideIdx < WideOffset) do
  begin
    Utf8Len := CalcUtf8LengthFromUtf8HeadChar(Utf8Text^);
    Inc(Result, Utf8Len);

    case Utf8Len of
      1:
        begin
          Inc(WideIdx);
          Inc(Utf8Text);
        end;
      2:
        begin
          Inc(WideIdx);
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
        end;
      3:
        begin
          Inc(WideIdx);
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
          if Utf8Text^ = #0 then
            Exit;
          Inc(Utf8Text);
        end;
      else
        Exit;
    end;
  end;
end;

// ����һ�� WideChar ת���� Utf8 ����ַ�����
function CalcUtf8LengthFromWideChar(AChar: WideChar): Integer;
var
  V: Cardinal;
begin
  V := Ord(AChar);
  if V <= $7F then
    Result := 1
  else if V <= $7FF then
    Result := 2
  else if V <= $FFFF then
    Result := 3
  else if V <= $10FFFF then
    Result := 4
  else
    Result := 0;
end;

// ������ַ����� Utf8 ���ȣ����� Utf8Encode ��ȡ Length������ʵ��ת��
function CalcUtf8LengthFromWideString(Text: PWideChar): Integer;
begin
  Result := 0;
  if Text = nil then
    Exit;

  while Text^ <> #0 do
  begin
    Inc(Result, CalcUtf8LengthFromWideChar(Text^));
    Inc(Text);
  end;
end;

// ����һ�� Utf8 ǰ���ַ���������ַ�����
function CalcUtf8LengthFromUtf8HeadChar(AChar: AnsiChar): Integer;
var
  B: Byte;
begin
  B := Ord(AChar);
  if B and $80 = 0 then  // 0xxx xxxx
    Result := 1
  else if B and $E0 = $C0 then // 110x xxxx 10xxxxxx
    Result := 2
  else if B and $F0 = $E0 then // 1110 xxxx 10xxxxxx 10xxxxxx
    Result := 3
  else
    raise Exception.Create('More than UTF16 NOT Support.');
end;

// �ֶ������ַ���ת���� Ansi�������еĿ��ַ����滻������ AlterChar�����ڴ�Ӣ�Ļ����µ��ַ���ȼ���
function ConvertUtf16ToAlterAnsi(WideText: PWideChar; AlterChar: AnsiChar = ' '): AnsiString;
var
  Len: Integer;
begin
  if WideText = nil then
  begin
    Result := '';
    Exit;
  end;

{$IFDEF UNICODE}
  Len := StrLen(WideText);
{$ELSE}
  Len := Length(WideString(WideText));
{$ENDIF}

  if Len = 0 then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Len * SizeOf(WideChar));
  Len := 0;
  while WideText^ <> #0 do
  begin
    if WideCharIsWideLength(WideText^) then
    begin
      Inc(Len);
      Result[Len] := AlterChar;
      Inc(Len);
      Result[Len] := AlterChar;
    end
    else
    begin
      Inc(Len);
      if Ord(WideText^) <= 255 then // Absolutely 'Single' Char
        Result[Len] := AnsiChar(WideText^)
      else                          // Extended 'Single' Char, Replace
        Result[Len] := AlterChar;
    end;
    Inc(WideText);
  end;
  SetLength(Result, Len);
end;

// �ֶ��� Utf8 �ַ���ת���� Ansi�������еĿ��ַ����滻������ AlterChar�����ڴ�Ӣ�Ļ����µ��ַ���ȼ���
function ConvertUtf8ToAlterAnsi(Utf8Text: PAnsiChar; AlterChar: AnsiChar = ' '): AnsiString;
var
  I, J, Len, ByteCount: Integer;
  C: AnsiChar;
  W: Word;
  B, B1, B2: Byte;
begin
  Result := '';
  if Utf8Text = nil then
    Exit;

  Len := StrLen(Utf8Text);
  if Len = 0 then
    Exit;

  SetLength(Result, Len);
  I := 0;
  J := 1;
  while I < Len do
  begin
    C := Utf8Text[I];
    B := Ord(C);
    W := 0;

    // ���� B ��ֵ�ó�����ַ�ռ����λ
    if B and $80 = 0 then  // 0xxx xxxx
      ByteCount := 1
    else if B and $E0 = $C0 then // 110x xxxx 10xxxxxx
      ByteCount := 2
    else if B and $F0 = $E0 then // 1110 xxxx 10xxxxxx 10xxxxxx
      ByteCount := 3
    else
      raise Exception.Create('More than UTF16 NOT Support.');

    // �ټ������Ӧ�Ŀ��ֽ��ַ�
    case ByteCount of
      1:
      begin
        W := B and $7F;
      end;
      2:
      begin
        B1 := Ord(Utf8Text[I + 1]);
        W := ((B and $1F) shl 6) or (B1 and $3F);
      end;
      3:
      begin
        B1 := Ord(Utf8Text[I + 1]);
        B2 := Ord(Utf8Text[I + 2]);
        W := ((B and $0F) shl 12) or ((B1 and $3F) shl 6) or (B2 and $3F);
      end;
    end;

    if WideCharIsWideLength(WideChar(W)) then
    begin
      Result[J] := AlterChar;
      Inc(J);
      Result[J] := AlterChar;
      Inc(J);
    end
    else
    begin
      if W <= 255 then
        Result[J] := AnsiChar(W)
      else
        Result[J] := AlterChar;
      Inc(J);
    end;
    Inc(I, ByteCount);
  end;

  SetLength(Result, J);
end;

// ��װ�� PChar ת���������� D2009 ������ǰ�汾 IDE ��ͬʱʹ��
// ���⣬D2009 �����ϰ汾������� inline��
// �������ʵ��ת��ʱ����ֵָ��ֲ����ͷŵ�����
function _CnPChar(const S: string): {$IFDEF UNICODE_STRING} PAnsiChar; inline {$ELSE} PChar {$ENDIF};
begin
{$IFDEF UNICODE_STRING}
  Result := PAnsiChar(AnsiString(S));
{$ELSE}
  Result := PChar(S);
{$ENDIF}
end;

// ��ExtractFileExt�ķ�װ��Delphi XE3��ExtractFileExt��Ϊ������
// TStringHelper.LastDelimiter��0��������ExtractFileExt('.dpr')���ٷ���'.dpr'��
// ���Ƿ��ؿ�ֵ�ˣ� XE3��SysUtils.LastDelimiter������XE2���ݵ�
function _CnExtractFileExt(const FileName: string): string;
{$IFDEF DELPHIXE3_UP}
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim, FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;
{$ELSE}
begin
  Result := ExtractFileExt(FileName);
end;
{$ENDIF}

// ��ExtractFileName�ķ�װ����ֹDelphi XE3��
// TStringHelper.LastDelimiter����Ĳ�����
function _CnExtractFileName(const FileName: string): string;
{$IFDEF DELPHIXE3_UP}
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;
{$ELSE}
begin
  Result := ExtractFileName(FileName);
end;
{$ENDIF}

// ��ExtractFileDir�ķ�װ����ֹDelphi XE3��TStringHelper.LastDelimiter
// ����Ĳ����ݣ�XE3��ExtractFileDir('C:\1.dpr')���ص���'C:'������'C:\'
function _CnExtractFileDir(const FileName: string): string;
{$IFDEF DELPHIXE3_UP}
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, Filename);
  if (I > 1) and (FileName[I] = PathDelim) and
    (not IsDelimiter( PathDelim + DriveDelim, FileName, I-1)) then Dec(I);
  Result := Copy(FileName, 1, I);
end;
{$ELSE}
begin
  Result := ExtractFileDir(FileName);
end;
{$ENDIF}

// ��ExtractFilePath�ķ�װ����ֹDelphi XE3��
// TStringHelper.LastDelimiter����Ĳ�����
function _CnExtractFilePath(const FileName: string): string;
{$IFDEF DELPHIXE3_UP}
var
  I: Integer;
begin
  I := LastDelimiter(PathDelim + DriveDelim, FileName);
  Result := Copy(FileName, 1, I);
end;
{$ELSE}
begin
  Result := ExtractFilePath(FileName);
end;
{$ENDIF}

// ��ChangeFileExt�ķ�װ����ֹDelphi XE3��
// TStringHelper.LastDelimiter����Ĳ�����
function _CnChangeFileExt(const FileName, Extension: string): string;
{$IFDEF DELPHIXE3_UP}
var
  I: Integer;
begin
  I := LastDelimiter('.' + PathDelim + DriveDelim,Filename);
  if (I = 0) or (FileName[I] <> '.') then I := MaxInt;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;
{$ELSE}
begin
  Result := ChangeFileExt(FileName, Extension);
end;
{$ENDIF}

//==============================================================================
// Ansi �ַ�������
//==============================================================================

{$IFNDEF UNICODE}

// D5 ��û������ UTF8/Ansi ת������

function InternalUnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal;
  Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  I, Cnt: Cardinal;
  C: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  Cnt := 0;
  I := 0;
  if Dest <> nil then
  begin
    while (I < SourceChars) and (Cnt < MaxDestBytes) do
    begin
      C := Cardinal(Source[I]);
      Inc(I);
      if C <= $7F then
      begin
        Dest[Cnt] := Char(C);
        Inc(Cnt);
      end
      else if C > $7FF then
      begin
        if Cnt + 3 > MaxDestBytes then
          break;
        Dest[Cnt] := Char($E0 or (C shr 12));
        Dest[Cnt + 1] := Char($80 or ((C shr 6) and $3F));
        Dest[Cnt + 2] := Char($80 or (C and $3F));
        Inc(Cnt, 3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if Cnt + 2 > MaxDestBytes then
          break;
        Dest[Cnt] := Char($C0 or (C shr 6));
        Dest[Cnt + 1] := Char($80 or (C and $3F));
        Inc(Cnt,2);
      end;
    end;
    if Cnt >= MaxDestBytes then Cnt := MaxDestBytes - 1;
    Dest[Cnt] := #0;
  end
  else
  begin
    while I < SourceChars do
    begin
      C := Integer(Source[I]);
      Inc(I);
      if C > $7F then
      begin
        if C > $7FF then
          Inc(Cnt);
        Inc(Cnt);
      end;
      Inc(Cnt);
    end;
  end;
  Result := Cnt + 1;  // convert zero based index to byte count
end;

function InternalUtf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal;
  Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  I, Cnt: Cardinal;
  C: Byte;
  WC: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  Cnt := 0;
  I := 0;
  if Dest <> nil then
  begin
    while (I < SourceBytes) and (Cnt < MaxDestChars) do
    begin
      WC := Cardinal(Source[I]);
      Inc(I);
      if (WC and $80) <> 0 then
      begin
        if I >= SourceBytes then Exit;          // incomplete multibyte char
        WC := WC and $3F;
        if (WC and $20) <> 0 then
        begin
          C := Byte(Source[I]);
          Inc(I);
          if (C and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if I >= SourceBytes then Exit;        // incomplete multibyte char
          WC := (WC shl 6) or (C and $3F);
        end;
        C := Byte(Source[I]);
        Inc(I);
        if (C and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[Cnt] := WideChar((WC shl 6) or (C and $3F));
      end
      else
        Dest[Cnt] := WideChar(WC);
      Inc(Cnt);
    end;
    if Cnt >= MaxDestChars then Cnt := MaxDestChars-1;
    Dest[Cnt] := #0;
  end
  else
  begin
    while (I < SourceBytes) do
    begin
      C := Byte(Source[I]);
      Inc(I);
      if (C and $80) <> 0 then
      begin
        if I >= SourceBytes then Exit;          // incomplete multibyte char
        C := C and $3F;
        if (C and $20) <> 0 then
        begin
          C := Byte(Source[I]);
          Inc(I);
          if (C and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if I >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        C := Byte(Source[I]);
        Inc(I);
        if (C and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(Cnt);
    end;
  end;
  Result := Cnt + 1;
end;

// �� WideString ���� Utf8 ����õ� AnsiString������ Ansi ת�����ⶪ�ַ�
function CnUtf8EncodeWideString(const S: WideString): AnsiString;
var
  L: Integer;
  Temp: AnsiString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S) * 3); // SetLength includes space for null terminator

  L := InternalUnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := '';
  Result := Temp;
end;

// �� AnsiString �� Utf8 ����õ� WideString������ Ansi ת�����ⶪ�ַ�
function CnUtf8DecodeToWideString(const S: AnsiString): WideString;
var
  L: Integer;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Result, Length(S));

  L := InternalUtf8ToUnicode(PWideChar(Result), Length(Result) + 1, PAnsiChar(S), Length(S));
  if L > 0 then
    SetLength(Result, L - 1)
  else
    Result := '';
end;

{$ENDIF}

// WideString ��ȫ���滻ʵ�֣����ִ�Сд
function WideStringReplace(const S, OldPattern, NewPattern: WideString): WideString;
var
  SearchStr, Patt, NewStr: WideString;
  Offset: Integer;
begin
  SearchStr := S;
  Patt := OldPattern;

  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);

    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

// Ansi ���ת�� Utf8 �� Ansi �ַ������Խ�� D2009 �� Utf8ToAnsi �� UString ������
function CnUtf8ToAnsi(const Text: AnsiString): AnsiString;
begin
{$IFDEF UNICODE_STRING}
  Result := AnsiString(UTF8ToUnicodeString(PAnsiChar(Text)));
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := Utf8ToAnsi(Text);
  {$ELSE}
  Result := AnsiString(CnUtf8DecodeToWideString(Text));
  {$ENDIF}
{$ENDIF}
end;

function CnUtf8ToAnsi2(const Text: string): string;
begin
{$IFDEF UNICODE_STRING}
  Result := UTF8ToUnicodeString(PAnsiChar(AnsiString(Text)));
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := Utf8ToAnsi(Text);
  {$ELSE}
  Result := AnsiString(CnUtf8DecodeToWideString(Text));
  {$ENDIF}
{$ENDIF}
end;

function CnAnsiToUtf8(const Text: AnsiString): AnsiString;
begin
{$IFDEF UNICODE_STRING}
  Result := AnsiString(Utf8Encode(Text)); // ����ֵ���ɸ�Ϊ UTF8String ���ͣ�����˴�ת����Ч
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := AnsiToUtf8(Text);
  {$ELSE}
  Result := CnUtf8EncodeWideString(WideString(Text));
  {$ENDIF}
{$ENDIF}
end;

function CnAnsiToUtf82(const Text: string): string;
begin
{$IFDEF UNICODE_STRING}
  Result := string(Utf8Encode(Text));
{$ELSE}
  {$IFDEF COMPILER6_UP}
  Result := AnsiToUtf8(Text);
  {$ELSE}
  Result := CnUtf8EncodeWideString(WideString(Text));
  {$ENDIF}
{$ENDIF}
end;

function AnsiTrim(const S: AnsiString): AnsiString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;

function TrimBom(const S: AnsiString): AnsiString;
const
  UTF8_BOM = #$EF#$BB#$BF;
begin
  if Length(S) < Length(UTF8_BOM) then
    Result := S
  else if StrLComp(PAnsiChar(UTF8_BOM), PAnsiChar(S), Length(UTF8_BOM)) = 0 then
    Result := Copy(S, Length(UTF8_BOM) + 1, MaxInt)
  else
    Result := S;
end;

//------------------------------------------------------------------------------
// ��չ���ļ�Ŀ¼��������
//------------------------------------------------------------------------------

// ����Դ�������д�ָ��Ŀ¼
procedure ExploreDir(const APath: string; ShowDir: Boolean);
var
  strExecute: AnsiString;
begin
  if not ShowDir then
    strExecute := AnsiString(Format('EXPLORER.EXE "%s"', [APath]))
  else
    strExecute := AnsiString(Format('EXPLORER.EXE /e, "%s"', [APath]));
  WinExec(PAnsiChar(strExecute), SW_SHOWNORMAL);
end;

// ����Դ�������д�ָ���ļ�
procedure ExploreFile(const AFile: string; ShowDir: Boolean);
var
  strExecute: AnsiString;
begin
  if not ShowDir then
    strExecute := AnsiString(Format('EXPLORER.EXE /select, "%s"', [AFile]))
  else
    strExecute := AnsiString(Format('EXPLORER.EXE /e, /select, "%s"', [AFile]));
  WinExec(PAnsiChar(strExecute), SW_SHOWNORMAL);
end;

// �ݹ鴴���༶��Ŀ¼
function ForceDirectories(Dir: string): Boolean;
begin
  Result := True;

  if Length(Dir) = 0 then
  begin
    Result := False;
    Exit;
  end;
  Dir := ExcludeTrailingBackslash(Dir);
  if (Length(Dir) < 3) or DirectoryExists(Dir)
    or (_CnExtractFilePath(Dir) = Dir) then
    Exit;                                // avoid 'xyz:\' problem.
  Result := ForceDirectories(_CnExtractFilePath(Dir)) and CreateDir(Dir);
end;

// �ƶ��ļ���Ŀ¼
function MoveFile(const sName, dName: string): Boolean;
var
  s1, s2: string;
  lpFileOp: TSHFileOpStruct;
begin
  s1 := PChar(sName) + #0#0;
  s2 := PChar(dName) + #0#0;
  with lpFileOp do
  begin
    Wnd := Application.Handle;
    wFunc := FO_MOVE;
    pFrom := PChar(s1);
    pTo := PChar(s2);
    fFlags := FOF_ALLOWUNDO;
    hNameMappings := nil;
    lpszProgressTitle := nil;
    fAnyOperationsAborted := True;
  end;

  try
    Result := SHFileOperation(lpFileOp) = 0;
  except
    Result := False;
  end;
end;

// ɾ���ļ�������վ
function DeleteToRecycleBin(const FileName: string): Boolean;
var
  s: string;
  lpFileOp: TSHFileOpStruct;
begin
  s := PChar(FileName) + #0#0;
  with lpFileOp do
  begin
    Wnd := Application.Handle;
    wFunc := FO_DELETE;
    pFrom := PChar(s);
    pTo := nil;
    fFlags := FOF_ALLOWUNDO or FOF_SILENT or FOF_NOCONFIRMATION;
    hNameMappings := nil;
    lpszProgressTitle := nil;
    fAnyOperationsAborted := True;
  end;

  try
    Result := SHFileOperation(lpFileOp) = 0;
  except
    Result := False;
  end;
end;

// ���ļ����Դ���
procedure FileProperties(const FName: string);
var
  SEI: SHELLEXECUTEINFO;
begin
  with SEI do
  begin
    cbSize := SizeOf(SEI);
    fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_INVOKEIDLIST or
      SEE_MASK_FLAG_NO_UI;
    Wnd := Application.Handle;
    lpVerb := 'properties';
    lpFile := PChar(FName);
    lpParameters := nil;
    lpDirectory := nil;
    nShow := 0;
    hInstApp := 0;
    lpIDList := nil;
  end;
  ShellExecuteEx(@SEI);
end;

// ������ʾ���µĳ�·����
function FormatPath(const APath: string; Width: Integer): string;
var
  SLen: Integer;
  i, j: Integer;
  TString: string;
begin
  SLen := Length(APath);
  if (SLen <= Width) or (Width <= 6) then
  begin
    Result := APath;
    Exit
  end
  else
  begin
    i := SLen;
    TString := APath;
    for j := 1 to 2 do
    begin
      while (TString[i] <> '\') and (SLen - i < Width - 8) do
        i := i - 1;
      i := i - 1;
    end;
    for j := SLen - i - 1 downto 0 do
      TString[Width - j] := TString[SLen - j];
    for j := SLen - i to SLen - i + 2 do
      TString[Width - j] := '.';
    Delete(TString, Width + 1, 255);
    Result := TString;
  end;
end;

// ͨ�� DrawText ��������·��
procedure DrawCompactPath(Hdc: HDC; Rect: TRect; const Str: string);
begin
  DrawText(Hdc, PChar(Str), Length(Str), Rect, DT_PATH_ELLIPSIS);
end;

// ��ָ�� Canvas �ϻ���ƥ����ַ�����ƥ�䲿�ָ�����ʾ
procedure DrawMatchText(Canvas: TCanvas; const MatchStr, Text: string;
  X, Y: Integer; HighlightColor: TColor; MatchedIndexes: TList; StartOffset: Integer);
var
  MatchIdx, I, W, L: Integer;
  HdrStr, AMatchStr, TailStr, PaintStr: string;
  OldColor, OldBrushColor: TColor;
  OldStyle: TBrushStyle;
  ASize: TSize;
  C: Char;
begin
  OldStyle := Canvas.Brush.Style;
  OldBrushColor := Canvas.Brush.Color;
  Canvas.Brush.Style := bsClear;

  // �������־����� bsClear ģʽ����
  if (MatchedIndexes = nil) or (MatchedIndexes.Count = 0) then
  begin
    if MatchStr = '' then
      MatchIdx := 0
    else if StartOffset > 1 then
    begin
      TailStr := Copy(Text, StartOffset, MaxInt);
      MatchIdx := Pos(UpperCase(Trim(MatchStr)), UpperCase(TailStr));
      Inc(MatchIdx, StartOffset - 1);
    end
    else
      MatchIdx := Pos(UpperCase(Trim(MatchStr)), UpperCase(Text));

    if MatchIdx > 0 then
    begin
      HdrStr := Copy(Text, 1, MatchIdx - 1);
      AMatchStr := Copy(Text, MatchIdx, Length(Trim(MatchStr)));
      TailStr := Copy(Text, MatchIdx + Length(Trim(MatchStr)), MaxInt);

      Canvas.TextOut(X, Y, HdrStr);
      Inc(X, Canvas.TextWidth(HdrStr));
      OldColor := Canvas.Font.Color;
      Canvas.Font.Color := HighlightColor;
      Canvas.TextOut(X, Y, AMatchStr);
      Canvas.Font.Color := OldColor;
      Inc(X, Canvas.TextWidth(AMatchStr));
      Canvas.TextOut(X, Y, TailStr);
    end
    else
      Canvas.TextOut(X, Y, Text);

    Canvas.Brush.Style := OldStyle;
  end
  else
  begin
    Canvas.TextOut(X, Y, Text);
    SetLength(PaintStr, Length(Text));
    StrCopy(PChar(PaintStr), PChar(Text));
    OldColor := Canvas.Font.Color;
    Canvas.Font.Color := HighlightColor;

    for I := MatchedIndexes.Count - 1 downto 0 do
    begin
      L := Integer(MatchedIndexes[I]);
      if (L <= 0) or (L > Length(PaintStr)) then
        Continue;

      if L < Length(PaintStr) then
        PaintStr[L + 1] := #0;
      C := PaintStr[L];
      PaintStr[L] := #0;

      ASize.cx := 0;
      ASize.cy := 0;
      if L = 1 then
        W := 0
      else
      begin
        Windows.GetTextExtentPoint32(Canvas.Handle, PChar(@(PaintStr[1])), L - 1, ASize);
        W := ASize.cx; // ����������ַ�ǰ�Ŀ��
      end;
      PaintStr[L] := C;
      Windows.TextOut(Canvas.Handle, X + W, Y, PChar(@(PaintStr[L])), 1);
    end;
    SetLength(PaintStr, 0);
    Canvas.Font.Color := OldColor;
  end;
  Canvas.Brush.Style := OldStyle;
  Canvas.Brush.Color := OldBrushColor;
end;

// ���ļ���
function OpenDialog(var FileName: string; const Title: string; const Filter: string;
  const Ext: string): Boolean;
var
  OpenName: TOPENFILENAME;
  TempFilename, ReturnFile: string;
begin
  with OpenName do
  begin
    lStructSize := SizeOf(OpenName);
    hWndOwner := GetModuleHandle('');
    Hinstance := SysInit.Hinstance;
    lpstrFilter := PChar(Filter + #0 + Ext + #0#0);
    lpstrCustomFilter := '';
    nMaxCustFilter := 0;
    nFilterIndex := 1;
    nMaxFile := MAX_PATH;
    SetLength(TempFilename, nMaxFile + 2);
    lpstrFile := PChar(TempFilename);
    FillChar(lpstrFile^, MAX_PATH, 0);
    SetLength(TempFilename, nMaxFile + 2);
    nMaxFileTitle := MAX_PATH;
    SetLength(ReturnFile, MAX_PATH + 2);
    lpstrFileTitle := PChar(ReturnFile);
    FillChar(lpstrFile^, MAX_PATH, 0);
    lpstrInitialDir := '.';
    lpstrTitle := PChar(Title);
    Flags := OFN_HIDEREADONLY + OFN_ENABLESIZING;
    nFileOffset := 0;
    nFileExtension := 0;
    lpstrDefExt := PChar(Ext);
    lCustData := 0;
    lpfnHook := nil;
    lpTemplateName := '';
  end;
  Result := GetOpenFileName(OpenName);
  if Result then
    FileName := ReturnFile
  else
    FileName := '';
end;

function SelectDirCB(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
begin
  if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), lpdata);
  Result := 0;
end;

function CnSelectDirectory(const Caption: string; const Root: WideString;
  var Directory: string; Owner: HWND; ShowNewButton: Boolean = True): Boolean;
var
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;
begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      SHGetDesktopFolder(IDesktopFolder);
      if Root = '' then
        RootItemIDList := nil
      else
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      with BrowseInfo do
      begin
        hwndOwner := Owner;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        if ShowNewButton then
          ulFlags := ulFlags or $0040;
        lpfn := SelectDirCB;
        lparam := Integer(PChar(Directory));
      end;
      ItemIDList := SHBrowseForFolder(BrowseInfo);
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

function GetDirectory(const Caption: string; var Dir: string;
  ShowNewButton: Boolean): Boolean;
var
  OldErrorMode: UINT;
  BrowseRoot: WideString;
  OwnerHandle: HWND;
begin
  OldErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    BrowseRoot := '';
    if Screen.ActiveCustomForm <> nil then
      OwnerHandle := Screen.ActiveCustomForm.Handle
    else
      OwnerHandle := Application.Handle;
    Result := CnSelectDirectory(Caption, BrowseRoot, Dir, OwnerHandle,
      ShowNewButton);
  finally
    SetErrorMode(OldErrorMode);
  end;
end;

{*****************************************************}
{ SelectDirectoryW ���� ���� SelectDirectory ����     }
{ ������Զ����ʼĿ¼ ,������֧�� UniCode �ַ�       }
{ author: Arthur(��ʤ), yzdbs@msn.com, 08.06.16       }
{ hOwn: �����ھ��                                    }
{ Path: Ŀ¼�����������, ����ʱΪ��ʼĿ¼            }
{ Caption: ��ʾ�ı�                                   }
{ Root: ��Ŀ¼                                        }
{ uFlag: ������ʽ�����������ʹ��,��                  }
{       BIF_RETURNONLYFSDIRS or BIF_VALIDATE          }
{       ����� Win32SDK �鿴������ϸ                  }
{*****************************************************} 
function SelectDirectoryW(hOwn: HWND; var Path: WideString; const Caption,
  Root: WideString; uFlag: DWORD = $25): Boolean;
const
  BIF_NEWDIALOGSTYLE = $0040;
{$IFDEF ver185}
{$REGION ' �Ի�����ʽ����'}
(*
{ Browsing for directory. }

  {$EXTERNALSYM BIF_RETURNONLYFSDIRS}
  BIF_RETURNONLYFSDIRS   = $0001;  { For finding a folder to start document searching }
  {$EXTERNALSYM BIF_DONTGOBELOWDOMAIN}
  BIF_DONTGOBELOWDOMAIN  = $0002;  { For starting the Find Computer }
  {$EXTERNALSYM BIF_STATUSTEXT}
  BIF_STATUSTEXT         = $0004;
  {$EXTERNALSYM BIF_RETURNFSANCESTORS}
  BIF_RETURNFSANCESTORS  = $0008;
  {$EXTERNALSYM BIF_EDITBOX}
  BIF_EDITBOX            = $0010;
  {$EXTERNALSYM BIF_VALIDATE}
  BIF_VALIDATE           = $0020;  { insist on valid result (or CANCEL) }
  {$EXTERNALSYM BIF_NEWDIALOGSTYLE}
  BIF_NEWDIALOGSTYLE     = $0040;
  {$EXTERNALSYM BIF_USENEWUI}
  BIF_USENEWUI = BIF_NEWDIALOGSTYLE or BIF_EDITBOX;

  {$EXTERNALSYM BIF_BROWSEINCLUDEURLS}
  BIF_BROWSEINCLUDEURLS  = $0080;
  {$EXTERNALSYM BIF_UAHINT}
  BIF_UAHINT = $100;   // Add a UA hint to the dialog, in place of the edit box. May not be combined with BIF_EDITBOX
  {$EXTERNALSYM BIF_NONEWFOLDERBUTTON}
  BIF_NONEWFOLDERBUTTON = $200;   // Do not add the "New Folder" button to the dialog.  Only applicable with BIF_NEWDIALOGSTYLE.
  {$EXTERNALSYM BIF_NOTRANSLATETARGETS}
  BIF_NOTRANSLATETARGETS = $400;   // don't traverse target as shortcut

  {$EXTERNALSYM BIF_BROWSEFORCOMPUTER}
  BIF_BROWSEFORCOMPUTER  = $1000;  { Browsing for Computers. }
  {$EXTERNALSYM BIF_BROWSEFORPRINTER}
  BIF_BROWSEFORPRINTER   = $2000;  { Browsing for Printers }
  {$EXTERNALSYM BIF_BROWSEINCLUDEFILES}
  BIF_BROWSEINCLUDEFILES = $4000;  { Browsing for Everything }
  {$EXTERNALSYM BIF_SHAREABLE}
  BIF_SHAREABLE          = $8000;
*)
{$ENDREGION}
{$ENDIF}
var
  BrowseInfo: TBrowseInfoW;
  Buffer: PWideChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Dummy: LongWord;
  function BrowseCallbackProc(hwnd: HWND; uMsg: UINT; lParam: Cardinal;
    lpData: Cardinal): integer; stdcall;
  var
    PathName: array[0..MAX_PATH] of WideChar;
  begin
    case uMsg of
      BFFM_INITIALIZED:
        SendMessage(Hwnd, BFFM_SETSELECTION, Ord(True), Integer(lpData));
      BFFM_SELCHANGED:
        begin
          SHGetPathFromIDListW(PItemIDList(lParam), @PathName);
          SendMessage(hwnd, BFFM_SETSTATUSTEXTW, 0,
            LongInt(PWideChar(WideString(
            @PathName))));
        end;
    end;
    Result := 0;
  end;
begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(hOwn, nil, POleStr(WideString(Root)),
          Dummy, RootItemIDList, Dummy);
      end;
      with BrowseInfo do
      begin
        hwndOwner := hOwn;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PWideChar(Caption);
        ulFlags := uFlag;
        lpfn := @BrowseCallbackProc;
        lParam := Integer(PWideChar(Path));
      end;
      ItemIDList := SHBrowseForFolderW(BrowseInfo);
      Result := ItemIDList <> nil;
      if Result then
      begin
        SHGetPathFromIDListW(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Path := WideString(Buffer);
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

// �� SelectDirectoryW �ķ�װ
function SelectDirectoryEx(hOwn: HWND; const Caption, Root: WideString): WideString;
begin
  SelectDirectoryW(hOwn, Result, Caption, Root);
end;

// �����ַ�����ǰ�����ͬ�ַ���
function SameCharCounts(s1, s2: string): Integer;
var
  Str1, Str2: PChar;
begin
  Result := 1;
  s1 := s1 + #0;
  s2 := s2 + #0;
  Str1 := PChar(s1);
  Str2 := PChar(s2);

  while (s1[Result] = s2[Result]) and (s1[Result] <> #0) do
  begin
    Inc(Result);
  end;
  Dec(Result);
{$IFDEF MSWINDOWS}
  if (StrByteType(Str1, Result - 1) = mbLeadByte) or
    (StrByteType(Str2, Result - 1) = mbLeadByte) then
    Dec(Result);
{$ENDIF}
{$IFDEF LINUX}
  if (StrByteType(Str1, Result - 1) <> mbSingleByte) or
    (StrByteType(Str2, Result - 1) <> mbSingleByte) then
    Dec(Result);
{$ENDIF}
end;

// ���ַ�����ĳ�ַ����ֵĴ���
function CharCounts(Str: PChar; Chr: Char): Integer;
var
  p: PChar;
begin
  Result := 0;
  p := StrScan(Str, Chr);
  while p <> nil do
  begin
{$IFDEF MSWINDOWS}
    case StrByteType(Str, Integer(p - Str)) of
      mbSingleByte: begin
        Inc(Result);
        Inc(p);
      end;
      mbLeadByte: Inc(p);
    end;
{$ENDIF}
{$IFDEF LINUX}
    if StrByteType(Str, Integer(p - Str)) = mbSingleByte then begin
      Inc(Result);
      Inc(p);
    end;
{$ENDIF}
    Inc(p);
    p := StrScan(p, Chr);
  end;
end;

// ȡ����Ŀ¼�����·��
function GetRelativePath(ATo, AFrom: string;
  const PathStr: string = '\'; const ParentStr: string = '..';
  const CurrentStr: string = '.'; const UseCurrentDir: Boolean = False): string;
var
  i, HeadNum: Integer;
begin
  ATo := StringReplace(ATo, '/', '\', [rfReplaceAll]);
  AFrom := StringReplace(AFrom, '/', '\', [rfReplaceAll]);
  while AnsiPos('\\', ATo) > 0 do
    ATo := StringReplace(ATo, '\\', '\', [rfReplaceAll]);
  while AnsiPos('\\', AFrom) > 0 do
    AFrom := StringReplace(AFrom, '\\', '\', [rfReplaceAll]);
  if StrRight(ATo, 1) = ':' then
    ATo := ATo + '\';
  if StrRight(AFrom, 1) = ':' then
    AFrom := AFrom + '\';

  HeadNum := SameCharCounts(AnsiUpperCase(_CnExtractFilePath(ATo)),
    AnsiUpperCase(_CnExtractFilePath(AFrom)));

  // HeadNum ��ʾ��Ŀ¼��ǰ����ͬ�Ĳ��֣�ע����������ͬ�Ĳ�����ǰ׺��ͬ�Ĳ�ͬĿ¼����
  // ��ˣ����ĩβ������\������Ҫ�����ҵ����� \ ��λ��
  while HeadNum > 0 do
  begin
    if (ATo[HeadNum] = '\') and (AFrom[HeadNum] = '\') then
      Break;
    Dec(HeadNum);
  end;

  if HeadNum > 0 then
  begin
    ATo := StringReplace(Copy(ATo, HeadNum + 1, MaxInt), '\', PathStr, [rfReplaceAll]);
    AFrom := Copy(AFrom, HeadNum + 1, MaxInt);

    Result := '';
    HeadNum := CharCounts(PChar(AFrom), '\');
    for i := 1 to HeadNum do
      Result := Result + ParentStr + PathStr;
    if (Result = '') and UseCurrentDir then
      Result := CurrentStr + PathStr;
    Result := Result + ATo;
  end
  else
    Result := ATo;
end;

// Ѱ��һ���ļ��Ĺ�ͬ��·��������ȥ����ͬ��·�����ļ������� OutFiles �У������Ƿ�ȥ���ɹ�
function CombineCommonPath(InFiles, OutFiles: TStrings): Boolean;
var
  I, SC: Integer;
  Root, S: string;
begin
  Result := False;
  if (InFiles <> nil) and (OutFiles <> nil) and (InFiles.Count > 1) then
  begin
    Root := InFiles[0];
    for I := 1 to InFiles.Count - 1 do
    begin
      SC := SameCharCounts(Root, InFiles[I]);
      if SC <= 0 then
        Exit;

      if SC < Length(Root) then
      begin
        Root := Copy(Root, 1, SC);
        SC := LastDelimiter('\', Root);
        if SC > 0 then
          Root := Copy(Root, 1, SC);  // Root �ǹ���·��
      end;

      if Root = '' then
        Break;
    end;

    SC := Length(Root);
    Result := SC > 0;
    if Result then
    begin
      OutFiles.Clear;
      for I := 0 to InFiles.Count - 1 do
      begin
        S := InFiles[I];
        Delete(S, 1, SC);
        OutFiles.Add(S);
      end;
    end;
  end;
end;

{$IFNDEF BCB}
const
  shlwapi32 = 'shlwapi.dll';

function PathRelativePathToA; external shlwapi32 name 'PathRelativePathToA';
function PathRelativePathToW; external shlwapi32 name 'PathRelativePathToW';
function PathRelativePathTo; external shlwapi32 name 'PathRelativePathToA';

// ʹ�� Windows API ȡ����Ŀ¼�����·��
function RelativePath(const AFrom, ATo: string; FromIsDir, ToIsDir: Boolean): string;
  function GetAttr(IsDir: Boolean): DWORD;
  begin
    if IsDir then
      Result := FILE_ATTRIBUTE_DIRECTORY
    else
      Result := FILE_ATTRIBUTE_NORMAL;
  end;
var
  p: array[0..MAX_PATH] of Char;
begin
  PathRelativePathTo(p, PChar(AFrom), GetAttr(FromIsDir), PChar(ATo), GetAttr(ToIsDir));
  Result := StrPas(p);
end;
{$ENDIF}

// ��������·����
// Head - ��·���������� C:\Test��\\Test\C\Abc��http://www.abc.com/dir/ �ȸ�ʽ
// Tail - β·���������� ..\Test��Abc\Temp��\Test��/web/lib �ȸ�ʽ����Ե�ַ��ʽ
function LinkPath(const Head, Tail: string): string;
var
  HeadIsUrl: Boolean;
  TailHasRoot: Boolean;
  TailIsRel: Boolean;
  AHead, ATail, S: string;
  UrlPos, i: Integer;
begin
  if Head = '' then
  begin
    Result := Tail;
    Exit;
  end;

  if Tail = '' then
  begin
    Result := Head;
    Exit;
  end;

  TailHasRoot := (AnsiPos(':\', Tail) = 2) or // C:\Test
                 (AnsiPos('\\', Tail) = 1) or // \\Name\C\Test
                 (AnsiPos('://', Tail) > 0);  // ftp://ftp.abc.com
  if TailHasRoot then
  begin
    Result := Tail;
    Exit;
  end;

  UrlPos := AnsiPos('://', Head);
  HeadIsUrl := UrlPos > 0;
  AHead := StringReplace(Head, '/', '\', [rfReplaceAll]);
  ATail := StringReplace(Tail, '/', '\', [rfReplaceAll]);

  TailIsRel := ATail[1] = '\'; // β·�������·��
  if TailIsRel then
  begin
    if AnsiPos(':\', AHead) = 2 then
      Result := AHead[1] + ':' + ATail
    else if AnsiPos('\\', AHead) = 1 then
    begin
      S := Copy(AHead, 3, MaxInt);
      i := AnsiPos('\', S);
      if i > 0 then
        Result := Copy(AHead, 1, i + 1) + ATail
      else
        Result := AHead + ATail;
    end else if HeadIsUrl then
    begin
      S := Copy(AHead, UrlPos + 3, MaxInt);
      i := AnsiPos('\', S);
      if i > 0 then
        Result := Copy(AHead, 1, i + UrlPos + 1) + ATail
      else
        Result := AHead + ATail;
    end
    else
    begin
      Result := Tail;
      Exit;
    end;
  end
  else
  begin
    if Copy(ATail, 1, 2) = '.\' then
      Delete(ATail, 1, 2);
    AHead := MakeDir(AHead);
    i := Pos('..\', ATail);
    while i > 0 do
    begin
      AHead := _CnExtractFileDir(AHead);
      Delete(ATail, 1, 3);
      i := Pos('..\', ATail);
    end;
    Result := MakePath(AHead) + ATail;
  end;

  if HeadIsUrl then
    Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
end;

// ����һ���ļ�
procedure RunFile(const FName: string; Handle: THandle;
  const Param: string);
begin
  ShellExecute(Handle, nil, PChar(FName), PChar(Param), nil, SW_SHOWNORMAL);
end;

// ��һ������
procedure OpenUrl(const Url: string; UseCmd: Boolean);
begin
  // Do not check protocal prefix.
  if CheckWindows9598 or not UseCmd then
    RunFile(Url)
  else
    ShellExecute(0, 'open', 'cmd.exe', PChar('/c start ' + Url), '', SW_HIDE);
end;

// �����ʼ�
procedure MailTo(const Addr: string; const Subject: string; UseCmd: Boolean);
const
  csPrefix = 'mailto:';
  csSubject = '?Subject=';
var
  Url: string;
begin
  if Pos(csPrefix, Addr) < 1 then
    Url := csPrefix + Addr
  else
    Url := Addr;
  if Subject <> '' then
    Url := Url + csSubject + Subject;

  if CheckWindows9598 or not UseCmd then
    RunFile(Url)
  else
    ShellExecute(0, 'open', 'cmd.exe', PChar('/c start ' + Url), '', SW_HIDE);
end;

// ����һ���ļ�����������
function WinExecute(const FileName: string; Visibility: Integer = SW_NORMAL): Boolean;
var
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
  CmdLines: array[0..512] of Char;
begin
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  StrPLCopy(@CmdLines[0], FileName, SizeOf(CmdLines) - 1);
  Result := CreateProcess(nil, PChar(@CmdLines[0]), nil, nil, False,
    CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS, nil, nil, StartupInfo,
    ProcessInfo);
end;

// ����һ���ļ����ȴ������
function WinExecAndWait32(const FileName: string; Visibility: Integer;
  ProcessMsg: Boolean): Integer;
var
  zAppName: array[0..512] of Char;
  zCurDir: array[0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPLCopy(zAppName, FileName, SizeOf(zAppName) - 1);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
    zAppName,                           { pointer to command line string }
    nil,                                { pointer to process security attributes }
    nil,                                { pointer to thread security attributes }
    False,                              { handle inheritance flag }
    CREATE_NEW_CONSOLE or               { creation flags }
    NORMAL_PRIORITY_CLASS,
    nil,                                { pointer to new environment block }
    nil,                                { pointer to current directory name }
    StartupInfo,                        { pointer to STARTUPINFO }
    ProcessInfo) then
    Result := -1                        { pointer to PROCESS_INF }
  else
  begin
    if ProcessMsg then
    begin
      repeat
        Application.ProcessMessages;
        GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
      until (Result <> STILL_ACTIVE) or Application.Terminated;
    end
    else
    begin
      WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
      GetExitCodeProcess(ProcessInfo.hProcess, Cardinal(Result));
    end;
  end;
end;

// �ùܵ���ʽ�� Dir Ŀ¼ִ�� CmdLine��Output ���������Ϣ��
// dwExitCode �����˳��롣����ɹ����� True
function WinExecWithPipe(const CmdLine, Dir: string; slOutput: TStrings;
  var dwExitCode: Cardinal): Boolean;
var
  HOutRead, HOutWrite: THandle;
  StartInfo: TStartupInfo;
  ProceInfo: TProcessInformation;
  sa: TSecurityAttributes;
  InStream: THandleStream;
  strTemp: string;
  PDir: PChar;
  CmdLines: array[0..512] of Char;

  procedure ReadLinesFromPipe(IsEnd: Boolean);
  var
    s: AnsiString;
    ls: TStringList;
    i: Integer;
  begin
    if InStream.Position < InStream.Size then
    begin
      SetLength(s, InStream.Size - InStream.Position);
      InStream.Read(PAnsiChar(s)^, InStream.Size - InStream.Position);
      strTemp := strTemp + string(s);
      ls := TStringList.Create;
      try
        ls.Text := strTemp;
        for i := 0 to ls.Count - 2 do
          slOutput.Add(ls[i]);
        strTemp := ls[ls.Count - 1];
      finally
        ls.Free;
      end;
    end;

    if IsEnd and (strTemp <> '') then
    begin
      slOutput.Add(strTemp);
      strTemp := '';
    end;
  end;
begin
  dwExitCode := 0;
  Result := False;
  try
    FillChar(sa, sizeof(sa), 0);
    sa.nLength := sizeof(sa);
    sa.bInheritHandle := True;
    sa.lpSecurityDescriptor := nil;
    InStream := nil;
    strTemp := '';
    HOutRead := INVALID_HANDLE_VALUE;
    HOutWrite := INVALID_HANDLE_VALUE;
    try
      Win32Check(CreatePipe(HOutRead, HOutWrite, @sa, 0));

      FillChar(StartInfo, SizeOf(StartInfo), 0);
      StartInfo.cb := SizeOf(StartInfo);
      StartInfo.wShowWindow := SW_HIDE;
      StartInfo.dwFlags := STARTF_USESTDHANDLES + STARTF_USESHOWWINDOW;
      StartInfo.hStdError := HOutWrite;
      StartInfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
      StartInfo.hStdOutput := HOutWrite;

      InStream := THandleStream.Create(HOutRead);

      if Dir <> '' then
        PDir := PChar(Dir)
      else
        PDir := nil;

      StrPLCopy(@CmdLines[0], CmdLine, SizeOf(CmdLines) - 1);
      Win32Check(CreateProcess(nil, //lpApplicationName: PChar
        PChar(@CmdLines[0]), //lpCommandLine: PChar
        nil, //lpProcessAttributes: PSecurityAttributes
        nil, //lpThreadAttributes: PSecurityAttributes
        True, //bInheritHandles: BOOL
        NORMAL_PRIORITY_CLASS, //CREATE_NEW_CONSOLE,
        nil,
        PDir,
        StartInfo,
        ProceInfo));

      while WaitForSingleObject(ProceInfo.hProcess, 100) = WAIT_TIMEOUT do
      begin
        ReadLinesFromPipe(False);
        Application.ProcessMessages;
        //if Application.Terminated then break;
      end;
      ReadLinesFromPipe(True);

      GetExitCodeProcess(ProceInfo.hProcess, dwExitCode);

      CloseHandle(ProceInfo.hProcess);
      CloseHandle(ProceInfo.hThread);

      Result := True;
    finally
      if InStream <> nil then InStream.Free;
      if HOutRead <> INVALID_HANDLE_VALUE then CloseHandle(HOutRead);
      if HOutWrite <> INVALID_HANDLE_VALUE then CloseHandle(HOutWrite);
    end;
  except
    ;
  end;
end;

function WinExecWithPipe(const CmdLine, Dir: string; var Output: string;
  var dwExitCode: Cardinal): Boolean;
var
  slOutput: TStringList;
begin
  slOutput := TStringList.Create;
  try
    Result := WinExecWithPipe(CmdLine, Dir, slOutput, dwExitCode);
    Output := slOutput.Text;
  finally
    slOutput.Free;
  end;
end;

// ����GUID�ַ���
function CreateGuidString: string;
var
  P: PWideChar;
  GUID: TGUID;
begin
  CoCreateGuid(GUID);
  if not Succeeded(StringFromCLSID(GUID, P)) then
    Result := ''
  else
    Result := P;
  CoTaskMemFree(P);
end;
  
// Ӧ�ó���·��
function AppPath: string;
begin
  Result := _CnExtractFilePath(Application.ExeName);
end;

// ��ǰִ��ģ�����ڵ�·��
function ModulePath: string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, GetModuleFileName(HInstance, ModName, SizeOf(ModName)));
  Result := _CnExtractFilePath(Result);
end;

const
  HKLM_CURRENT_VERSION_WINDOWS = 'Software\Microsoft\Windows\CurrentVersion';
  HKLM_CURRENT_VERSION_NT      = 'Software\Microsoft\Windows NT\CurrentVersion';

function RelativeKey(const Key: string): PChar;
begin
  Result := PChar(Key);
  if (Key <> '') and (Key[1] = '\') then
    Inc(Result);
end;

function RegReadStringDef(const RootKey: HKEY; const Key, Name, Def: string): string;
var
  RegKey: HKEY;
  Size: DWORD;
  StrVal: string;
  RegKind: DWORD;
begin
  Result := Def;
  if RegOpenKeyEx(RootKey, RelativeKey(Key), 0, KEY_READ, RegKey) = ERROR_SUCCESS then
  begin
    RegKind := 0;
    Size := 0;
    if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, nil, @Size) = ERROR_SUCCESS then
      if RegKind in [REG_SZ, REG_EXPAND_SZ] then
      begin
        SetLength(StrVal, Size);
        if RegQueryValueEx(RegKey, PChar(Name), nil, @RegKind, PByte(StrVal), @Size) = ERROR_SUCCESS then
        begin
          SetLength(StrVal, StrLen(PChar(StrVal)));
          Result := StrVal;
        end;
      end;
    RegCloseKey(RegKey);
  end;
end;

function GetKeysInRegistryKey(const Key: string; List: TStrings): Boolean;
var
  Reg: TRegistry;
begin
  Result := False;
  Reg := TRegistry.Create(KEY_READ);
  try
    if Reg.OpenKey(Key, False) then
    begin
      Reg.GetKeyNames(List);
      Result := True;
    end;
  finally
    Reg.Free;
  end;
end;

procedure StrResetLength(var S: string);
begin
  SetLength(S, StrLen(PChar(S)));
end;

// ȡProgram FilesĿ¼
function GetProgramFilesDir: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, HKLM_CURRENT_VERSION_WINDOWS, 'ProgramFilesDir', '');
end;

// ȡWindowsĿ¼
function GetWindowsDir: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetWindowsDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetWindowsDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;

// ȡ��ʱ�ļ�·��
function GetWindowsTempPath: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetTempPath(0, nil);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetTempPath(Required, PChar(Result));
    StrResetLength(Result);
  end;
end;

// ����һ����ʱ�ļ���
function CnGetTempFileName(const Ext: string): string;
var
  Path: string;
begin
  Path := MakePath(GetWindowsTempPath);
  repeat
    Result := Path + IntToStr(Random(MaxInt)) + Ext;
  until not FileExists(Result);
end;

// ȡϵͳĿ¼
function GetSystemDir: string;
var
  Required: Cardinal;
begin
  Result := '';
  Required := GetSystemDirectory(nil, 0);
  if Required <> 0 then
  begin
    SetLength(Result, Required);
    GetSystemDirectory(PChar(Result), Required);
    StrResetLength(Result);
  end;
end;

function ShellGetFolder(const Name: string): string;
const
  RegPath = '\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
var
  Reg: TRegistry;
  Folder: string;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey(RegPath, False) then
      Folder := Reg.ReadString(Name);
  finally
    Reg.Free;
  end;
  Result := Folder;
end;

// ȡ�ҵ��ĵ�Ŀ¼
function GetMyDocumentsDir: string;
begin
  Result := ShellGetFolder('Personal');
end;

var
  _Kernel32Handle: HMODULE = HMODULE(0);
  _GetLongPathName: function (lpszShortPath: PChar; lpszLongPath: PChar;
    cchBuffer: DWORD): DWORD; stdcall;

function Kernel32Handle: HMODULE;
begin
  if _Kernel32Handle = HMODULE(0) then
    _Kernel32Handle := LoadLibrary(kernel32);
  Result := _Kernel32Handle;
end;


function ShellGetLongPathName(const Path: string): string;
var
  PIDL: PItemIDList;
  Desktop: IShellFolder;
  AnsiName: string;
  WideName: array [0..MAX_PATH] of WideChar;
  Eaten, Attr: ULONG;
begin
  Result := Path;
  if Path <> '' then
  begin
    if Succeeded(SHGetDesktopFolder(Desktop)) then
    begin
      MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, _CnPChar(Path), -1, WideName, MAX_PATH);
      if Succeeded(Desktop.ParseDisplayName(0, nil, WideName, Eaten, PIDL, Attr)) then
      try
        SetLength(AnsiName, MAX_PATH);
        if SHGetPathFromIDList(PIDL, PChar(AnsiName)) then
          StrResetLength(AnsiName);
        Result := AnsiName;
      finally
        CoTaskMemFree(PIDL);
      end;
    end;
  end;
end;

// ���ļ���ת���ļ���
function ShortNameToLongName(const FileName: string): string;
const
{$IFDEF UNICODE_STRING}
  SCnGetLongPathName = 'GetLongPathNameW';
{$ELSE}
  SCnGetLongPathName = 'GetLongPathNameA';
{$ENDIF}
begin
  Result := FileName;
  if not Assigned(_GetLongPathName) then
    _GetLongPathName := GetProcAddress(Kernel32Handle, SCnGetLongPathName);
  if Assigned(_GetLongPathName) then
  begin
    SetLength(Result, MAX_PATH);
    SetLength(Result, _GetLongPathName(PChar(FileName), PChar(Result), MAX_PATH));
  end
  else
  begin
    Result := ShellGetLongPathName(FileName);
  end;
end;

// ���ļ���ת���ļ���
function LongNameToShortName(const FileName: string): string;
var
  Buf: PChar;
  BufSize: Integer;
begin
  BufSize := GetShortPathName(PChar(FileName), nil, 0) + 1;
  GetMem(Buf, BufSize * SizeOf(Char));
  try
    GetShortPathName(PChar(FileName), Buf, BufSize);
    Result := Buf;
  finally
    FreeMem(Buf);
  end;
end;

// ȡ����ʵ���ļ�����������Сд
function GetTrueFileName(const FileName: string): string;
var
  AName: string;
  FindName: string;

  function DoFindFile(const FName: string): string;
  var
    F: TSearchRec;
  begin
    if SysUtils.FindFirst(FName, faAnyFile, F) = 0 then
      Result := F.Name
    else
      Result := _CnExtractFileName(FName);
    SysUtils.FindClose(F);
  end;
begin
  AName := MakeDir(FileName);
  if (Length(AName) > 3) and (AName[2] = ':') then
  begin
    Result := '';
    while Length(AName) > 3 do
    begin
      FindName := DoFindFile(AName);

      if FindName = '' then
      begin
        Result := AName;
        Exit;
      end;

      if Result = '' then
        Result := FindName
      else
        Result := FindName + '\' + Result;

      AName := _CnExtractFileDir(AName);
    end;

    Result := UpperCase(AName) + Result;
  end
  else
    Result := AName;
end;

// ���ҿ�ִ���ļ�������·��
function FindExecFile(const AName: string; var AFullName: string): Boolean;
var
  fn: array[0..MAX_PATH] of Char;
  pc: PChar;
begin
  if (0 = SearchPath(nil, PChar(AName), '.exe', Length(fn), fn, pc)) and
     (0 = SearchPath(nil, PChar(AName), '.com', Length(fn), fn, pc)) and
     (0 = SearchPath(nil, PChar(AName), '.bat', Length(fn), fn, pc)) then
  begin
    Result := False;
  end
  else
  begin
    Result := True;
    AFullName := fn;
  end;
end;

function PidlFree(var IdList: PItemIdList): Boolean;
var
  Malloc: IMalloc;
begin
  Result := False;
  if IdList = nil then
    Result := True
  else
  begin
    if Succeeded(SHGetMalloc(Malloc)) and (Malloc.DidAlloc(IdList) > 0) then
    begin
      Malloc.Free(IdList);
      IdList := nil;
      Result := True;
    end;
  end;
end;

function PidlToPath(IdList: PItemIdList): string;
begin
  SetLength(Result, MAX_PATH);
  if SHGetPathFromIdList(IdList, PChar(Result)) then
    StrResetLength(Result)
  else
    Result := '';
end;

// ȡ��ϵͳ�����ļ���λ�ã�Folder ʹ���� ShlObj �ж���ı�ʶ���� CSIDL_DESKTOP
function GetSpecialFolderLocation(const Folder: Integer): string;
var
  FolderPidl: PItemIdList;
begin
  if Succeeded(SHGetSpecialFolderLocation(0, Folder, FolderPidl)) then
  begin
    Result := PidlToPath(FolderPidl);
    PidlFree(FolderPidl);
  end
  else
    Result := '';
end;

// Ŀ¼β��'\'����
function AddDirSuffix(const Dir: string): string;
begin
  Result := Trim(Dir);
  if Result = '' then Exit;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF};
end;

// Ŀ¼β��'\'����
function MakePath(const Dir: string): string;
begin
  Result := AddDirSuffix(Dir);
end;

// ·��βȥ�� '\'
function MakeDir(const Path: string): string;
begin
  Result := Trim(Path);
  if Result = '' then Exit;
  if CharInSet(Result[Length(Result)], ['/', '\']) then
    Delete(Result, Length(Result), 1);
end;

// ·���е� '\' ת�� '/'
function GetUnixPath(const Path: string): string;
begin
  Result := StringReplace(Path, '\', '/', [rfReplaceAll]);
end;

// ·���е� '/' ת�� '\'
function GetWinPath(const Path: string): string;
begin
  Result := StringReplace(Path, '/', '\', [rfReplaceAll]);
end;

function PointerXX(var X: PAnsiChar): PAnsiChar;
{$IFDEF PUREPASCAL}
begin
  Result := X;
  Inc(X);
end;
{$ELSE}
asm
  {
  EAX = X
  }
  MOV EDX, [EAX]
  INC dword ptr [EAX]
  MOV EAX, EDX
end;
{$ENDIF}

function Evaluate(var X: AnsiChar; const Value: AnsiChar): AnsiChar;
{$IFDEF PUREPASCAL}
begin
  X := Value;
  Result := X;
end;
{$ELSE}
asm
  {
  EAX = X
  EDX = Value (DL)
  }
  MOV [EAX], DL
  MOV AL, [EAX]
end;
{$ENDIF}

// �ļ����Ƿ���ͨ���ƥ�䣬����ֵΪ0��ʾƥ��
function FileNameMatch(Pattern, FileName: PAnsiChar): Integer;
var
  p, n: PAnsiChar;
  c: AnsiChar;
begin
  p := Pattern;
  n := FileName;

  while Evaluate(c, PointerXX(p)^) <> #0 do
  begin
	  case c of
		  '?': begin
          if n^ = '.' then
          begin
            while (p^ <> '.') and (p^ <> #0) do
            begin
              if (p^ <> '?') and (p^ <> '*') then
              begin
                Result := -1;
                Exit;
              end;
              Inc(p);
            end;
          end
          else
          begin
            if n^ <> #0 then
              Inc(n);
          end;
        end;

      '>': begin
          if n^ = '.' then
          begin
            if ((n + 1)^ = #0) and (FileNameMatch(p, n+1) = 0) then
            begin
              Result := 0;
              Exit;
            end;
            if FileNameMatch(p, n) = 0 then
            begin
              Result := 0;
              Exit;
            end;
            Result := -1;
            Exit;
          end;
          if n^ = #0 then
          begin
            Result := FileNameMatch(p, n);
            Exit;
          end;
          Inc(n);
        end;

      '*': begin
          while n^ <> #0 do
          begin
            if FileNameMatch(p, n) = 0 then
            begin
              Result := 0;
              Exit;
            end;
            Inc(n);
          end;
        end;

      '<': begin
          while n^ <> #0 do
          begin
				    if FileNameMatch(p, n) = 0 then
            begin
              Result := 0;
              Exit;
            end;
            if (n^ = '.') and (StrScan(n + 1, '.') = nil) then
            begin
              Inc(n);
              Break;
            end;
            Inc(n);
          end;
        end;

      '"': begin
          if (n^ = #0) and (FileNameMatch(p, n) = 0) then
          begin
            Result := 0;
            Exit;
          end;
          if n^ <> '.' then
          begin
            Result := -1;
            Exit;
          end;
          Inc(n);
        end;
    else
      if (c = '.') and (n^ = #0) then
      begin
        while p^ <> #0 do
        begin
          if (p^ = '*') and ((p + 1)^ = #0) then
          begin
            Result := 0;
            Exit;
          end;
          if p^ <> '?' then
          begin
            Result := -1;
            Exit;
          end;
          Inc(p);
        end;
        Result := 0;
        Exit;
			end;
      if c <> n^ then
      begin
        Result := -1;
        Exit;
      end;
      Inc(n);
    end;
  end;

  if n^ = #0 then
  begin
    Result := 0;
    Exit;
  end;

  Result := -1;
end;

// �ļ����Ƿ�����չ��ͨ���ƥ��
function MatchExt(const S, Ext: string): Boolean;
begin
  if S = '.*' then
  begin
    Result := True;
    Exit;
  end;

  Result := FileNameMatch(_CnPChar(S), _CnPChar(Ext)) = 0;
end;

// �ļ����Ƿ���ͨ���ƥ��
function MatchFileName(const S, FN: string): Boolean;
begin
  if S = '*.*' then
  begin
    Result := True;
    Exit;
  end;

  Result := FileNameMatch(_CnPChar(S), _CnPChar(FN)) = 0;
end;

// �õ���Сд�Ƿ����е��ַ���
function _CaseSensitive(const CaseSensitive: Boolean; const S: string): string;
begin
  if CaseSensitive then
    Result := S
  else
    Result := AnsiUpperCase(S);
end;

// ת����չ��ͨ����ַ���Ϊͨ����б�
procedure FileExtsToStrings(const FileExts: string; ExtList: TStrings; CaseSensitive: Boolean);
var
  Exts: string;
  i: Integer;
begin
  Exts := StringReplace(FileExts, ';', ',', [rfReplaceAll]);
  ExtList.CommaText := Exts;

  for i := 0 to ExtList.Count - 1 do
  begin
    if StrScan(PChar(ExtList[i]), '.') <> nil then
    begin
      ExtList[i] := _CaseSensitive(CaseSensitive, _CnExtractFileExt(ExtList[i]));
    end
    else
    begin
      ExtList[i] := '.' + _CaseSensitive(CaseSensitive, ExtList[i]);
    end;
    if ExtList[i] = '.*' then
    begin
      if i > 0 then
        ExtList.Exchange(0, i);
      Exit;
    end;
  end;
end;

// �ļ����Ƿ�ƥ����չ��ͨ���
function FileMatchesExts(const FileName, FileExts: string; CaseSensitive: Boolean): Boolean;
var
  ExtList: TStrings;
  FExt: string;
  i: Integer;
begin
  ExtList := TStringList.Create;
  try
    FileExtsToStrings(FileExts, ExtList, CaseSensitive);

    FExt := _CaseSensitive(CaseSensitive, _CnExtractFileExt(FileName));
    Result := False;
    for i := 0 to ExtList.Count - 1 do
    begin
      if MatchExt(ExtList[i], FExt) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    ExtList.Free;
  end;
end;

// �ļ����Ƿ�ƥ����չ��ͨ���
function FileMatchesExts(const FileName: string; ExtList: TStrings): Boolean;
var
  FExt: string;
  i: Integer;
begin
  FExt := _CaseSensitive(False, _CnExtractFileExt(FileName));

  Result := False;
  for i := 0 to ExtList.Count - 1 do
  begin
    if MatchExt(ExtList[i], FExt) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// ת���ļ�ͨ����ַ���Ϊͨ����б�
procedure FileMasksToStrings(const FileMasks: string; MaskList: TStrings; CaseSensitive: Boolean);
var
  Exts: string;
  i: Integer;
begin
  Exts := StringReplace(FileMasks, ';', ',', [rfReplaceAll]);
  MaskList.CommaText := Exts;

  for i := 0 to MaskList.Count - 1 do
  begin
    if StrScan(PChar(MaskList[i]), '.') <> nil then
    begin
      if MaskList[i][1] = '.' then
        MaskList[i] := '*' + _CaseSensitive(CaseSensitive, MaskList[i])
      else
        MaskList[i] := _CaseSensitive(CaseSensitive, MaskList[i]);
    end
    else
    begin
      MaskList[i] := '*.' + _CaseSensitive(CaseSensitive, MaskList[i]);
    end;
    if MaskList[i] = '*.*' then
    begin
      if i > 0 then
        MaskList.Exchange(0, i);
      Exit;
    end;
  end;
end;

// �ļ����Ƿ�ƥ��ͨ���
function FileMatchesMasks(const FileName, FileMasks: string; CaseSensitive: Boolean): Boolean;
var
  MaskList: TStrings;
  FFileName: string;
  i: Integer;
begin
  MaskList := TStringList.Create;
  try
    FileMasksToStrings(FileMasks, MaskList, CaseSensitive);

    FFileName := _CaseSensitive(CaseSensitive, _CnExtractFileName(FileName));
    Result := False;
    for i := 0 to MaskList.Count - 1 do
    begin
      if MatchFileName(MaskList[i], FFileName) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    MaskList.Free;
  end;
end;

// �ļ����Ƿ�ƥ��ͨ���
function FileMatchesMasks(const FileName: string; MaskList: TStrings): Boolean;
var
  FFileName: string;
  i: Integer;
begin
  FFileName := _CaseSensitive(False, _CnExtractFileName(FileName));

  Result := False;
  for i := 0 to MaskList.Count - 1 do
  begin
    if MatchFileName(_CaseSensitive(False, MaskList[i]), FFileName) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

// �ж��ļ��Ƿ�����ʹ��
function IsFileInUse(const FName: string): Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  if not FileExists(FName) then
    Exit;
  HFileRes := CreateFile(PChar(FName), GENERIC_READ or GENERIC_WRITE, 0,
    nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;

// �ж��ļ��Ƿ�Ϊ Ascii �ļ�
function IsAscii(const FileName: string): Boolean;
const
  Sett=2048;
var
  I: Integer;
  AFile: File;
  Bool: Boolean;
  TotSize, IncSize, ReadSize: Integer;
  C: array[0..Sett] of Byte;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    {$I-}
    AssignFile(AFile, FileName);
    Reset(AFile, 1);
    TotSize := FileSize(AFile);
    IncSize := 0;
    Bool := True;
    while (IncSize < TotSize) and Bool do
    begin
      ReadSize := Sett;
      if IncSize + ReadSize > TotSize then
        ReadSize := TotSize - IncSize;
      IncSize := IncSize + ReadSize;
      BlockRead(AFile, C, ReadSize);
      for I := 0 to ReadSize-1 do // Iterate
        if (C[I] < 32) and (not(C[I] in [9, 10, 13, 26])) then Bool := False;
    end; // while
    CloseFile(AFile);
    {$I+}
    if IOResult <> 0 then
      Result := False
    else
      Result := Bool;
  end;
end;

// �ж��ļ��Ƿ�����Ч���ļ���
function IsValidFileName(const Name: string): Boolean;
var
  i: Integer;
begin
  Result := False;

  if (Name = '') or (Length(Name) > MAX_PATH) then
    Exit;

  for i := 1 to Length(Name) do
  begin
    if CharInSet(Name[i], InvalidFileNameChar) then
      Exit;
  end;
  Result := True;
end;

// ������Ч���ļ���
function GetValidFileName(const Name: string): string;
var
  i: Integer;
begin
  Result := Name;
  for i := Length(Result) downto 1 do
  begin
    if CharInSet(Result[i], InvalidFileNameChar) then
      Delete(Result, i, 1);
  end;
  if Length(Result) > MAX_PATH - 1 then
    Result := Copy(Result, 1, MAX_PATH - 1);
end;

// �����ļ�ʱ��
function SetFileDate(const FileName: string; CreationTime, LastWriteTime, LastAccessTime:
  TFileTime): Boolean;
var
  FileHandle: Integer;
begin
  FileHandle := FileOpen(FileName, fmOpenWrite or fmShareDenyNone);
  if FileHandle > 0 then
  begin
    SetFileTime(FileHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
    FileClose(FileHandle);
    Result := True;
  end
  else
    Result := False;
end;

// ȡ�ļ�ʱ��
function GetFileDate(const FileName: string; var CreationTime, LastWriteTime, LastAccessTime:
  TFileTime): Boolean;
var
  FileHandle: Integer;
begin
  FileHandle := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if FileHandle > 0 then
  begin
    GetFileTime(FileHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
    FileClose(FileHandle);
    Result := True;
  end
  else
    Result := False;
end;

// ȡ�����ļ���ص�ͼ��
// FileName: e.g. "e:\hao\a.txt"
// �ɹ��򷵻�True
function GetFileIcon(const FileName: string; var Icon: TIcon): Boolean;
var
  SHFileInfo: TSHFileInfo;
  h: HWND;
begin
  if not Assigned(Icon) then
    Icon := TIcon.Create;
  h := SHGetFileInfo(PChar(FileName),
    0,
    SHFileInfo,
    SizeOf(SHFileInfo),
    SHGFI_ICON or SHGFI_SYSICONINDEX);
  Icon.Handle := SHFileInfo.hIcon;
  Result := (h <> 0);
end;

// �ļ�ʱ��ת��������ʱ��
function FileTimeToDateTime(const FileTime: TFileTime): TDateTime;
var
  SystemTime: TSystemTime;
begin
  SystemTime := FileTimeToLocalSystemTime(FileTime);
  with SystemTime do
    Result := EncodeDate(wYear, wMonth, wDay) + EncodeTime(wHour, wMinute,
      wSecond, wMilliseconds);
end;

// ��������ʱ��ת�ļ�ʱ��
function DateTimeToFileTime(const DateTime: TDateTime): TFileTime;
var
  SystemTime: TSystemTime;
begin
  with SystemTime do
  begin
    DecodeDate(DateTime, wYear, wMonth, wDay);
    DecodeTime(DateTime, wHour, wMinute, wSecond, wMilliseconds);
  end;
  Result := LocalSystemTimeToFileTime(SystemTime);
end;

// �ļ�ʱ��ת����ʱ��
function FileTimeToLocalSystemTime(FTime: TFileTime): TSystemTime;
var
  STime: TSystemTime;
begin
  FileTimeToLocalFileTime(FTime, FTime);
  FileTimeToSystemTime(FTime, STime);
  Result := STime;
end;

// ����ʱ��ת�ļ�ʱ��
function LocalSystemTimeToFileTime(STime: TSystemTime): TFileTime;
var
  FTime: TFileTime;
begin
  SystemTimeToFileTime(STime, FTime);
  LocalFileTimeToFileTime(FTime, FTime);
  Result := FTime;
end;

const
  MinutesPerDay     = 60 * 24;
  SecondsPerDay     = MinutesPerDay * 60;

// UTC ʱ��ת����ʱ��
function DateTimeToLocalDateTime(DateTime: TDateTime): TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  if GetTimeZoneInformation(TimeZoneInfo) = TIME_ZONE_ID_DAYLIGHT then
    Result := DateTime - ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / MinutesPerDay)
  else
    Result := DateTime - (TimeZoneInfo.Bias / MinutesPerDay);
end;

// ����ʱ��ת UTC ʱ��
function LocalDateTimeToDateTime(DateTime: TDateTime): TDateTime;
var
  TimeZoneInfo: TTimeZoneInformation;
begin
  FillChar(TimeZoneInfo, SizeOf(TimeZoneInfo), #0);
  if GetTimeZoneInformation(TimeZoneInfo) = TIME_ZONE_ID_DAYLIGHT then
    Result := DateTime + ((TimeZoneInfo.Bias + TimeZoneInfo.DaylightBias) / MinutesPerDay)
  else
    Result := DateTime + (TimeZoneInfo.Bias / MinutesPerDay);
end;

// �ѳ��򶤵�Windows7������������Ϊ����·�����ļ���
procedure PinAppToWin7Taskbar(const Path, App: string);
var
  Shell, Folder, FolderItem, ItemVerbs: Variant;
  vPath, vApp: Variant;
  I: Integer;
  Str: String;
  H: HINST;
  PinName: array[0..255] of Char;
begin
  Shell := CreateOleObject('Shell.Application');
  vPath := Path;
  Folder := Shell.NameSpace(vPath);
  vApp := App;
  FolderItem := Folder.ParseName(vApp);
  ItemVerbs := FolderItem.Verbs;

  H := LoadLibrary('Shell32.dll');
  LoadString(H, 5386, PinName, 256);
  FreeLibrary(H);

  for I := 1 to ItemVerbs.Count do
  begin
    Str := ItemVerbs.Item(I).Name;
    if SameText(Str, PinName) then
      ItemVerbs.Item(I).DoIt;
  end;
end;

{$IFDEF COMPILER5}
const
  LessThanValue = Low(TValueRelationship);
  EqualsValue = 0;
  GreaterThanValue = High(TValueRelationship);

function CompareValue(const A, B: Int64): TValueRelationship;
begin
  if A = B then
    Result := EqualsValue
  else if A < B then
    Result := LessThanValue
  else
    Result := GreaterThanValue;
end;

// AText �Ƿ��� ASubText ��ͷ
function AnsiStartsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiPos(AnsiUpperCase(ASubText), AnsiUpperCase(AText)) = 1;
end;

function AnsiReplaceText(const AText, AFromText, AToText: string): string;
begin
  Result := StringReplace(AText, AFromText, AToText, [rfReplaceAll, rfIgnoreCase]);
end;
{$ENDIF}

{$IFNDEF COMPILER7_UP}
// AText �Ƿ���� ASubText
function AnsiContainsText(const AText, ASubText: string): Boolean;
begin
  Result := AnsiPos(AnsiUpperCase(ASubText), AnsiUpperCase(AText)) > 0;
end;
{$ENDIF}

// �Ƚ� SubText �������ַ����г��ֵ�λ�õĴ�С����������Ƚ��ַ����������Դ�Сд
function AnsiCompareTextPos(const ASubText, AText1, AText2: string): TValueRelationship;
begin
  Result := 0;
  if ASubText <> '' then
    Result := CompareValue(AnsiPos(AnsiUpperCase(ASubText), AnsiUpperCase(AText1)),
      AnsiPos(AnsiUpperCase(ASubText), AnsiUpperCase(AText2)));
  if Result = 0 then
    Result := AnsiCompareText(AText1, AText2);
end;

function CompareTextPos(const ASubText, AText1, AText2: string): TValueRelationship;
begin
  Result := 0;
  if ASubText <> '' then
    Result := CompareValue(Pos(UpperCase(ASubText), UpperCase(AText1)),
      Pos(UpperCase(ASubText), UpperCase(AText2)));
  if Result = 0 then
    Result := CompareText(AText1, AText2);
end;

// �Ƚ� SubText �������ַ����г��ֵ�λ�õĴ�С��λ�ò����������Ƚ��ַ�����������ɷ��򣬺��Դ�Сд
function CompareTextWithPos(const ASubText, AText1, AText2: string;
  Reverse: Boolean): TValueRelationship;
var
  P1, P2: Integer;
begin
  Result := 0;
  if ASubText <> '' then
  begin
    P1 := Pos(UpperCase(ASubText), UpperCase(AText1));
    P2 := Pos(UpperCase(ASubText), UpperCase(AText2));

    if P1 = P2 then // �Ӵ�λ����ͬ��û�У��Ƚ��ַ��������ɷ���
    begin
      Result := CompareStr(AText1, AText2);
      if Reverse then
        Result := -Result;
    end
    else if (P1 = 0) or (P2 = 0) then // һ����һ��û�У����跴���еı�Ȼ��ǰ
    begin
      if P1 = 0 then
        Result := 1
      else if P2 = 0 then
        Result := -1;
    end
    else // ���е���ͬ���Ƚ��Ӵ�λ�ã����跴��
    begin
      Result := P1 - P2;
    end;
  end
  else // �Ӵ�Ϊ�գ����Ƚ��ַ������ɷ���
  begin
    Result := CompareStr(AText1, AText2);
    if Reverse then
      Result := -Result;
  end;
end;

// �� Ansi ��ʽ���ַ����滻
function StringReplaceNonAnsi(const S, OldPattern, NewPattern: string;
  Flags: TReplaceFlags): string;
var
  SearchStr, Patt, NewStr: string;
  Offset: Integer;
begin
  if rfIgnoreCase in Flags then
  begin
    SearchStr := UpperCase(S);
    Patt := UpperCase(OldPattern);
  end else
  begin
    SearchStr := S;
    Patt := OldPattern;
  end;
  NewStr := S;
  Result := '';
  while SearchStr <> '' do
  begin
    Offset := Pos(Patt, SearchStr);
    if Offset = 0 then
    begin
      Result := Result + NewStr;
      Break;
    end;
    Result := Result + Copy(NewStr, 1, Offset - 1) + NewPattern;
    NewStr := Copy(NewStr, Offset + Length(OldPattern), MaxInt);
    if not (rfReplaceAll in Flags) then
    begin
      Result := Result + NewStr;
      Break;
    end;
    SearchStr := Copy(SearchStr, Offset + Length(Patt), MaxInt);
  end;
end;

// ���������ļ�
function CreateBakFile(const FileName, Ext: string): Boolean;
var
  BakFileName: string;
  AExt: string;
begin
  if (Ext <> '') and (Ext[1] = '.') then
    AExt := Ext
  else
    AExt := '.' + Ext;
  BakFileName := FileName + AExt;
  Result := CopyFile(PChar(FileName), PChar(BakFileName), False);
end;

// ɾ������Ŀ¼
function Deltree(const Dir: string; DelRoot: Boolean; DelEmptyDirOnly: Boolean): Boolean;
var
  sr: TSearchRec;
  fr: Integer;
begin
  Result := True;
  if not DirectoryExists(Dir) then
    Exit;
  fr := FindFirst(AddDirSuffix(Dir) + '*.*', faAnyFile, sr);
  try
    while fr = 0 do
    begin
      if (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        SetFileAttributes(PChar(AddDirSuffix(Dir) + sr.Name), FILE_ATTRIBUTE_NORMAL);
        if sr.Attr and faDirectory = faDirectory then
          Result := Deltree(AddDirSuffix(Dir) + sr.Name, True, DelEmptyDirOnly)
        else if not DelEmptyDirOnly then
          Result := DeleteFile(AddDirSuffix(Dir) + sr.Name);
      end;
      fr := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;

  if DelRoot then
    Result := RemoveDir(Dir);
end;

// ɾ������Ŀ¼�еĿ�Ŀ¼, DelRoot ��ʾ�Ƿ�ɾ��Ŀ¼����
procedure DelEmptyTree(const Dir: string; DelRoot: Boolean = True);
var
  sr: TSearchRec;
  fr: Integer;
begin
  fr := FindFirst(AddDirSuffix(Dir) + '*.*', faDirectory, sr);
  try
    while fr = 0 do
    begin
      if (sr.Name <> '.') and (sr.Name <> '..') and (sr.Attr and faDirectory
        = faDirectory) then
      begin
        SetFileAttributes(PChar(AddDirSuffix(Dir) + sr.Name), FILE_ATTRIBUTE_NORMAL);
        DelEmptyTree(AddDirSuffix(Dir) + sr.Name, True);
      end;
      fr := FindNext(sr);
    end;
  finally
    FindClose(sr);
  end;

  if DelRoot then
    RemoveDir(Dir);
end;

// ȡ�ļ����µ�ֱϵ�ļ��б���������Ŀ¼�������ļ���
function GetDirFiles(const Dir: string; FileNames: TStrings): Integer;
var
  Sr: TSearchRec;
  Fr: Integer;
begin
  Result := 0;
  if FileNames <> nil then
    FileNames.Clear;

  Fr := FindFirst(AddDirSuffix(Dir) + '*.*', faAnyFile, Sr);
  while Fr = 0 do
  begin
    if (Sr.Name <> '.') and (Sr.Name <> '..') and // ����Ŀ¼
      (FILE_ATTRIBUTE_DIRECTORY and Sr.Attr = 0) then
    begin
      Inc(Result);
      if FileNames <> nil then
        FileNames.Add(Sr.Name);
    end;
    Fr := FindNext(Sr);
  end;
  FindClose(Sr);
end;

// ����ָ���������Ҵ���
function FindFormByClass(AClass: TClass): TForm;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Screen.FormCount - 1 do
  begin
    if Screen.Forms[I] is AClass then
    begin
      Result := Screen.Forms[I];
      Exit;
    end;
  end;
end;

// ��ǰ�Ƿ���ģ̬���ڴ���
function ModalFormExists: Boolean;
var
  I: Integer;
begin
  for I := 0 to Screen.CustomFormCount - 1 do
  begin
    if fsModal in Screen.CustomForms[I].FormState then
    begin
      Result := True;
      Exit;
    end
  end;
  Result := False;
end;

var
  FindAbort: Boolean;

// ����ָ��Ŀ¼���ļ�
function FindFile(const Path: string; const FileName: string = '*.*';
  Proc: TFindCallBack = nil; DirProc: TDirCallBack = nil; bSub: Boolean = True;
  bMsg: Boolean = True): Boolean;

  procedure DoFindFile(const Path, SubPath: string; const FileName: string;
    Proc: TFindCallBack; DirProc: TDirCallBack; bSub: Boolean;
    bMsg: Boolean);
  var
    APath: string;
    Info: TSearchRec;
    Succ: Integer;
  begin
    FindAbort := False;
    APath := MakePath(MakePath(Path) + SubPath);
    Succ := FindFirst(APath + FileName, faAnyFile - faVolumeID, Info);
    try
      while Succ = 0 do
      begin
        if (Info.Name <> '.') and (Info.Name <> '..') then
        begin
          if (Info.Attr and faDirectory) <> faDirectory then
          begin
            if Assigned(Proc) then
              Proc(APath + Info.FindData.cFileName, Info, FindAbort);
          end
        end;
        if bMsg then
          Application.ProcessMessages;
        if FindAbort then
          Exit;
        Succ := FindNext(Info);
      end;
    finally
      FindClose(Info);
    end;

    if bSub then
    begin
      Succ := FindFirst(APath + '*.*', faAnyFile - faVolumeID, Info);
      try
        while Succ = 0 do
        begin
          if (Info.Name <> '.') and (Info.Name <> '..') and
            (Info.Attr and faDirectory = faDirectory) then
          begin
            if Assigned(DirProc) then
              DirProc(MakePath(SubPath) + Info.Name);
            DoFindFile(Path, MakePath(SubPath) + Info.Name, FileName, Proc,
              DirProc, bSub, bMsg);
            if FindAbort then
              Exit;
          end;
          Succ := FindNext(Info);
        end;
      finally
        FindClose(Info);
      end;
    end;
  end;

begin
  DoFindFile(Path, '', FileName, Proc, DirProc, bSub, bMsg);
  Result := not FindAbort;
end;

// ���� SearchFile �ڵ�ǰĿ¼��ϵͳĿ¼�� PATH �����в���ָ����չ�����ļ���
// FileName Ϊ������չ�����ļ�����Ext Ϊ��չ�����ɹ�����ȫ·���ļ�����ʧ�ܷ��ؿա�
function CnSearchFile(const FileName: string; const Ext: string = '.exe'): string;
var
  FN: array[0..MAX_PATH] of Char;
  PC: PChar;
begin
  Result := '';
  if SearchPath(nil, PChar(FileName), PChar(Ext), Length(FN), FN, PC) <> 0 then
    Result := FN;
end;

// �ļ��򿪷�ʽ
function OpenWith(const FileName: string): Integer;
begin
  Result := ShellExecute(Application.Handle, 'open', 'rundll32.exe',
    PChar('shell32.dll,OpenAs_RunDLL ' + FileName), '', SW_SHOW);
end;

// ���ָ����Ӧ�ó����Ƿ���������
// ���ߣ��ܾ��� 2002.08.12
function CheckAppRunning(const FileName: string; var Running: Boolean): Boolean;
var
  hSnap: THandle;
  ppe: TProcessEntry32;
  AName: string;
begin
  Result := False;
  AName := Trim(FileName);
  if AName = '' then Exit;              // ���Ϊ��ֱ���˳�
  if _CnExtractFileExt(FileName) = '' then // Ĭ����չ��Ϊ EXE
    AName := AName + '.EXE';
  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0); // ������ǰ���̿���
  if hSnap <> INVALID_HANDLE_VALUE then
  try
    ppe.dwSize := SizeOf(TProcessEntry32);
    if Process32First(hSnap, ppe) then  // ȡ��һ��������Ϣ
      repeat
        if AnsiCompareText(_CnExtractFileName(ppe.szExeFile), AName) = 0 then
        begin                           // �Ƚ�Ӧ�ó�����
          Running := True;
          Result := True;
          Exit;
        end;
      until not Process32Next(hSnap, ppe); // ȡ��һ��������Ϣ
    Result := GetLastError = ERROR_NO_MORE_FILES; // �жϲ����Ƿ���������
  finally
    CloseHandle(hSnap);                 // �رվ��
  end;
end;

// ȡ�ļ��汾��
function GetFileVersionNumber(const FileName: string): TVersionNumber;
var
  VersionInfoBufferSize: DWORD;
  dummyHandle: DWORD;
  VersionInfoBuffer: Pointer;
  FixedFileInfoPtr: PVSFixedFileInfo;
  VersionValueLength: UINT;
begin
  FillChar(Result, SizeOf(Result), 0);
  if not FileExists(FileName) then
    Exit;

  VersionInfoBufferSize := GetFileVersionInfoSize(PChar(FileName), dummyHandle);
  if VersionInfoBufferSize = 0 then
    Exit;

  GetMem(VersionInfoBuffer, VersionInfoBufferSize);
  try
    try
      Win32Check(GetFileVersionInfo(PChar(FileName), dummyHandle,
        VersionInfoBufferSize, VersionInfoBuffer));
      Win32Check(VerQueryValue(VersionInfoBuffer, '\',
        Pointer(FixedFileInfoPtr), VersionValueLength));
    except
      Exit;
    end;
    Result.Major := FixedFileInfoPtr^.dwFileVersionMS shr 16;
    Result.Minor := FixedFileInfoPtr^.dwFileVersionMS;
    Result.Release := FixedFileInfoPtr^.dwFileVersionLS shr 16;
    Result.Build := FixedFileInfoPtr^.dwFileVersionLS;
  finally
    FreeMem(VersionInfoBuffer);
  end;
end;

// ȡ�ļ��汾�ַ���
function GetFileVersionStr(const FileName: string): string;
begin
  with GetFileVersionNumber(FileName) do
    Result := Format('%d.%d.%d.%d', [Major, Minor, Release, Build]);
end;

// ȡ�ļ���Ϣ
function GetFileInfo(const FileName: string; var FileSize: Int64;
  var FileTime: TDateTime): Boolean;
var
  Handle: THandle;
  FindData: TWin32FindData;
begin
  Result := False;
  Handle := FindFirstFile(PChar(FileName), FindData);
  if Handle <> INVALID_HANDLE_VALUE then
  begin
    Windows.FindClose(Handle);
    if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
      Int64Rec(FileSize).Lo := FindData.nFileSizeLow;
      Int64Rec(FileSize).Hi := FindData.nFileSizeHigh;
      FileTime := FileTimeToDateTime(FindData.ftLastWriteTime);
      Result := True;
    end;
  end;
end;

// ȡ�ļ�����
function GetFileSize(const FileName: string): Int64;
var
  FileTime: TDateTime;
begin
  Result := -1;
  GetFileInfo(FileName, Result, FileTime);
end;

// ȡ�ļ�Delphi��ʽ����ʱ��
function GetFileDateTime(const FileName: string): TDateTime;
var
  Size: Int64;
begin
  Result := 0;
  GetFileInfo(FileName, Size, Result);
end;

// ���ļ���Ϊ�ַ���
function LoadStringFromFile(const FileName: string): string;
begin
  try
    with TStringList.Create do
    try
      LoadFromFile(FileName);
      Result := Text;
    finally
      Free;
    end;
  except
    Result := '';
  end;
end;

// �����ַ�����Ϊ�ļ�
function SaveStringToFile(const S, FileName: string): Boolean;
begin
  try
    with TStringList.Create do
    try
      Text := S;
      SaveToFile(FileName);
      Result := True;
    finally
      Free;
    end;
  except
    Result := False;
  end;
end;

// StringList ���ţ��ֲ� D5��6 �����򲻿��ŵľ���
procedure QuickSortStringList(List: TStringList; L, R: Integer; SCompare: TStringListSortCompare);
var
  I, J, P: Integer;

  procedure ExchangeItems(Index1, Index2: Integer);
  var
    TempS: string;
    TempObj: TObject;
  begin
    TempS := List[Index1];
    List[Index1] := List[Index2];
    List[Index2] := TempS;

    TempObj := List.Objects[Index1];
    List.Objects[Index1] := List.Objects[Index2];
    List.Objects[Index2] := TempObj;
  end;

begin
  if (List = nil) or (List.Count = 0) then
    Exit;

  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(List, I, P) < 0 do Inc(I);
      while SCompare(List, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSortStringList(List, L, J, SCompare);
    L := I;
  until I >= R;
end;

//------------------------------------------------------------------------------
// �����������
//------------------------------------------------------------------------------

procedure MultiSzToStrings(const Dest: TStrings; const Source: PChar);
var
  P: PChar;
begin
  Assert(Dest <> nil);
  Dest.Clear;
  if Source <> nil then
  begin
    P := Source;
    while P^ <> #0 do
    begin
      Dest.Add(P);
      P := StrEnd(P);
      Inc(P);
    end;
  end;
end;

function DelEnvironmentVar(const Name: string): Boolean;
begin
  Result := SetEnvironmentVariable(PChar(Name), nil);
end;

function ExpandEnvironmentVar(var Value: string): Boolean;
var
  R: Integer;
  Expanded: string;
begin
  SetLength(Expanded, 1);
  R := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), 0);
  SetLength(Expanded, R);
  Result := ExpandEnvironmentStrings(PChar(Value), PChar(Expanded), R) <> 0;
  if Result then
  begin
    StrResetLength(Expanded);
    Value := Expanded;
  end;
end;

function GetEnvironmentVar(const Name: string; var Value: string; Expand: Boolean): Boolean;
var
  R: DWORD;
begin
  R := GetEnvironmentVariable(PChar(Name), nil, 0);
  SetLength(Value, R);
  R := GetEnvironmentVariable(PChar(Name), PChar(Value), R);
  Result := R <> 0;
  if not Result then
    Value := ''
  else
  begin
    SetLength(Value, R);
    if Expand then
      ExpandEnvironmentVar(Value);
  end;
end;

function GetEnvironmentVars(const Vars: TStrings; Expand: Boolean): Boolean;
var
  Raw: PChar;
  Expanded: string;
  I: Integer;
begin
  Vars.Clear;
  Raw := GetEnvironmentStrings;
  try
    MultiSzToStrings(Vars, Raw);
    Result := True;
  finally
    FreeEnvironmentStrings(Raw);
  end;
  if Expand then
  begin
    for I := 0 to Vars.Count - 1 do
    begin
      Expanded := Vars[I];
      if ExpandEnvironmentVar(Expanded) then
        Vars[I] := Expanded;
    end;
  end;
end;

function SetEnvironmentVar(const Name, Value: string): Boolean;
begin
  Result := SetEnvironmentVariable(PChar(Name), PChar(Value));
end;

//------------------------------------------------------------------------------
// ��չ���ַ�����������
//------------------------------------------------------------------------------

// �ж��ַ����Ƿ��ת���ɸ�����
function IsFloat(const s: String): Boolean;
var
  I: Real;
  E: Integer;
begin
  Val(s, I, E);
  Result := E = 0;
  E := Trunc( I );
end;

// �ж��ַ����Ƿ��ת��������
function IsInt(const s: String): Boolean;
var
  I: Integer;
  E: Integer;
begin
  Val(s, I, E);
  Result := E = 0;
  E := Trunc( I );
end;

// �ж��ַ����Ƿ��ת���� DateTime
function IsDateTime(const s: string): Boolean;
begin
  try
    StrToDateTime(s);
    Result := True;
  except
    Result := False;
  end;
end;

// �ж��Ƿ���Ч���ʼ���ַ
function IsValidEmail(const s: string): Boolean;
var
  i: Integer;
  AtCount: Integer;
begin
  Result := False;
  if s = '' then Exit;
  AtCount := 0;
  for i := 1 to Length(s) do
  begin
    if s[i] = '@' then
    begin
      Inc(AtCount);
      if AtCount > 1 then
        Exit;
    end
    else if not CharInSet(s[i], ['0'..'9', 'a'..'z', 'A'..'Z', '_', '.', '-']) then
      Exit;
  end;
  Result := AtCount = 1;
end;

// �Բ�����ķ�ʽ�����������͵�����ƽ����
function AverageNoOverflow(A, B: Integer): Integer;
begin
  Result := (A and B) + ((A xor B) shr 1);
end;

// �ж��ַ��Ƿ��ڼ�����
function CharInSet(C: Char; CharSet: TAnsiCharSet): Boolean;
begin
{$IFDEF COMPILER12_UP}
  if Ord(C) <= $FF then
    Result := AnsiChar(C) in CharSet
  else
    Result := False;
{$ELSE}
  Result := C in CharSet;
{$ENDIF}
end;

// �ж�s1�Ƿ������s2��
function InStr(const sShort: string; const sLong: string): Boolean;
var
  s1, s2: string;
begin
  s1 := LowerCase(sShort);
  s2 := LowerCase(sLong);
  Result := Pos(s1, s2) > 0;
end;

// ��չ����ת�ַ��������������ֱ�ΪĿ���������ȡ�����ַ���Ĭ��Ϊ����
function IntToStrEx(Value: Integer; Len: Integer; FillChar: Char = '0'): string;
begin
  Result := IntToStr(Value);
  while Length(Result) < Len do
    Result := FillChar + Result;
end;

// ���ָ������������ַ�ת��
function IntToStrSp(Value: Integer; SpLen: Integer; Sp: Char; ShowPlus: Boolean): string;
var
  s: string;
  i, j: Integer;
begin
  s := IntToStr(Value);
  if ShowPlus and (Value > 0) then
    s := '+' + s;
  Result := '';
  j := 0;
  for i := Length(s) downto 1 do
  begin
    Result := s[i] + Result;
    Inc(j);
    if ((j mod SpLen) = 0) and (i <> 1) and not CharInSet(s[i - 1], ['+', '-']) then
      Result := Sp + Result;
  end;
end;

function StrSpToInt(const Value: string; Sp: Char = ','): Int64;
begin
  Result := StrToInt64(AnsiReplaceText(Value, Sp, ''));
end;

// �����ַ����ұߵ��ַ�
function StrRight(const Str: string; Len: Integer): string;
begin
  if Len >= Length(Str) then
    Result := Str
  else
    Result := Copy(Str, Length(Str) - Len + 1, Len);
end;

// �����ַ�����ߵ��ַ�
function StrLeft(const Str: string; Len: Integer): string;
begin
  if Len > Length(Str) then
    Result := Str
  else
    Result := Copy(Str, 1, Len);
end;

// �ֽ�ת�����ƴ�
function ByteToBin(Value: Byte): string;
const
  V: Byte = 1;
var
  I: Integer;
begin
  Result := '';
  for I := 7 downto 0 do
    if (V shl I) and Value <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
end;

// ˫�ֽ�ת�����ƴ�
function WordToBin(Value: Word): string;
const
  V: Word = 1;
var
  I: Integer;
begin
  Result := '';
  for I := 15 downto 0 do
    if (V shl I) and Value <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
end;

// ���ֽ�ת�����ƴ�
function DWordToBin(Value: DWORD): string;
const
  V: DWORD = 1;
var
  I: Integer;
begin
  Result := '';
  for I := 31 downto 0 do
    if (V shl I) and Value <> 0 then
      Result := Result + '1'
    else
      Result := Result + '0';
end;

// �����ַ�����
function GetLine(C: Char; Len: Integer): string;
begin
  Result := StringOfChar(C, Len);
end;

// �����ı��ļ�������
function GetTextFileLineCount(const FileName: string): Integer;
var
  Lines: TStringList;
begin
  Result := 0;
  Lines := TStringList.Create;
  try
    if FileExists(FileName) then
    begin
      Lines.LoadFromFile(FileName);
      Result := Result + Lines.Count;
    end;
  finally
    Lines.Free;
  end;
end;

// ���ؿո�
function Spc(Len: Integer): string;
begin
  Result := StringOfChar(' ', Len);
end;

// �����ִ�
procedure SwapStr(var s1, s2: string);
var
  tempstr: string;
begin
  tempstr := s1;
  s1 := s2;
  s2 := tempstr;
end;

// �ָ�"������+����"��ʽ���ַ����еķ����ֺ�����
procedure SeparateStrAndNum(const AInStr: string; var AOutStr: string;
  var AOutNum: Integer);
var
  iLen: Integer;
begin
  iLen := Length(AInStr);
  while (iLen > 0) and CharInSet(AInStr[iLen], ['0'..'9']) do Dec(iLen);
  AOutStr := Copy(AInStr, iLen + 1, MaxInt);
  if AOutStr = '' then
    AOutNum := -1
  else
    AOutNum := StrToInt(AOutStr);
  AOutStr := Copy(AInStr, 1, iLen);
end;

// ȥ�������õ��ַ���������
function UnQuotedStr(const str: string; const ch: Char;
  const sep: string = ''): string;
var
  s: string;
  ps: PChar;
begin
  Result := '';
  s := str;
  ps := PChar(s);
  while ps <> nil do
  begin
    ps := AnsiStrScan(ps, ch);
    s := AnsiExtractQuotedStr(ps, ch);
    if (Result = '') or (s = '') then
      Result := Result + s
    else
      Result := Result + sep + s;
  end;
end;

// �����ַ����г��ֵĵ� Counter �ε��ַ���λ��
function CharPosWithCounter(const Sub: Char; const AStr: string;
  Counter: Integer = 1): Integer;
var
  I, J: Integer;
begin
  Result := 0;
  if Counter <= 0 then Exit;
  if AStr <> '' then
  begin
    J := 0;
    for I := 1 to Length(AStr) do
    begin
      if AStr[I] = Sub then
        Inc(J);
      if J = Counter then
      begin
        Result := I;
        Exit;
      end;
    end;
  end;
end;

function CountCharInStr(const Sub: Char; const AStr: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if AStr = '' then Exit;
  for I := 1 to Length(AStr) do
    if AStr[I] = Sub then
      Inc(Result);
end;

// �ж��ַ��Ƿ���Ч��ʶ���ַ���First ��ʾ�Ƿ�Ϊ���ַ�
function IsValidIdentChar(C: Char; First: Boolean): Boolean;
begin
  if First then
    Result := CharInSet(C, Alpha)
  else
    Result := CharInSet(C, AlphaNumeric);
end;

// �ж��ַ����Ƿ�����Ч�� Unicode ��ʶ����ֻ�� Unicode �µ���
function IsValidIdentW(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not ((AnsiChar(Ident[1]) in Alpha) or (Ord(Ident[1]) > 127)) then
    Exit;
  for I := 2 to Length(Ident) do
    if not ((AnsiChar(Ident[I]) in AlphaNumeric) or (Ord(Ident[I]) > 127)) then
      Exit;
  Result := True;
end;

// �жϿ��ַ����Ƿ�����Ч�� Unicode ��ʶ����ֻ�� BDS ���ϵ���
function IsValidIdentWide(const Ident: WideString): Boolean;
{$IFDEF BDS}
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
{$ENDIF}
begin
  Result := False;
{$IFDEF BDS}
  if (Length(Ident) = 0) or not ((AnsiChar(Ident[1]) in Alpha) or (Ord(Ident[1]) > 127)) then
    Exit;
  for I := 2 to Length(Ident) do
    if not ((AnsiChar(Ident[I]) in AlphaNumeric) or (Ord(Ident[I]) > 127)) then
      Exit;
  Result := True;
{$ENDIF}
end;

const
  csLinesCR = #13#10;
  csStrCR = '\n';

// �����ı�ת���У����з�ת'\n'��
{$IFDEF COMPILER5}
function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    if B then
      Result := 'True'
    else
      Result := 'False';
  end
  else
    Result := cSimpleBoolStrs[B];
end;
{$ENDIF COMPILER5}

function LinesToStr(const Lines: string): string;
begin
  Result := StringReplace(Lines, csLinesCR, csStrCR, [rfReplaceAll]);
end;

// �����ı�ת���У�'\n'ת���з���
function StrToLines(const Str: string): string;
begin
  Result := StringReplace(Str, csStrCR, csLinesCR, [rfReplaceAll]);
end;

// ���п��ı�ת���У�'\n'ת���з���
function WideStrToLines(const Str: WideString): WideString;
begin
  Result := WideStringReplace(Str, csStrCR, csLinesCR);
end;

// ����ת�ַ�����ʹ�� yyyy.mm.dd ��ʽ
function MyDateToStr(Date: TDate): string;
begin
  Result := CnDateToStr(Date);
end;

const
  csCount = 'Count';
  csItem = 'Item';

procedure ReadStringsFromIni(Ini: TCustomIniFile; const Section: string; Strings: TStrings);
var
  Count, i: Integer;
begin
  Strings.Clear;
  Count := Ini.ReadInteger(Section, csCount, 0);
  for i := 0 to Count - 1 do
    if Ini.ValueExists(Section, csItem + IntToStr(i)) then
      Strings.Add(Ini.ReadString(Section, csItem + IntToStr(i), ''));
end;

procedure WriteStringsToIni(Ini: TCustomIniFile; const Section: string; Strings: TStrings);
var
  i: Integer;
begin
  Ini.WriteInteger(Section, csCount, Strings.Count);
  for i := 0 to Strings.Count - 1 do
    Ini.WriteString(Section, csItem + IntToStr(i), Strings[i]);
end;

// �汾��ת���ַ������� $01020000 --> '1.2.0.0'
function VersionToStr(Version: DWORD): string;
begin
  Result := Format('%d.%d.%d.%d', [Version div $1000000, version mod $1000000
    div $10000, version mod $10000 div $100, version mod $100]);
end;

// �ַ���ת�ɰ汾�ţ��� '1.2.0.0' --> $01020000�������ʽ����ȷ������ $01000000
function StrToVersion(const S: string): DWORD;
var
  Strs: TStrings;
begin
  try
    Strs := TStringList.Create;
    try
      Strs.Text := StringReplace(S, '.', #13#10, [rfReplaceAll]);
      if Strs.Count = 4 then
        Result := StrToInt(Strs[0]) * $1000000 + StrToInt(Strs[1]) * $10000 +
          StrToInt(Strs[2]) * $100 + StrToInt(Strs[3])
      else
        Result := $01000000;
    finally
      Strs.Free;
    end;
  except
    Result := $01000000;
  end;
end;

// ת������Ϊ yyyy.mm.dd ��ʽ�ַ���
function CnDateToStr(Date: TDateTime): string;
begin
  Result := FormatDateTime('yyyy.mm.dd', Date);
end;

// �� yyyy.mm.dd ��ʽ�ַ���ת��Ϊ����
function CnStrToDate(const S: string): TDateTime;
var
  i: Integer;
  Year, Month, Day: string;
begin
  try
    i := 1;
    Year := ExtractSubstr(S, i, ['.', '/', '-']);
    Month := ExtractSubstr(S, i, ['.', '/', '-']);
    Day := ExtractSubstr(S, i, ['.', '/', '-']);
    Result := EncodeDate(StrToInt(Year), StrToInt(Month), StrToInt(Day));
  except
    Result := 0;
  end;
end;

// ȡ����ʱ������ڲ��֣�������
function GetDatePart(DateTime: TDateTime): TDate;
begin
  Result := Trunc(DateTime);
end;  

// ȡ����ʱ���ʱ�䲿�֣�С����
function GetTimePart(DateTime: TDateTime): TTime;
begin
  Result := Frac(DateTime);
end;  

// ����ʱ��ת '20030203132345' ʽ���� 14 λ�����ַ���
function DateTimeToFlatStr(const DateTime: TDateTime): string;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Min, Sec, MSec);
  Result := IntToStrEx(Year, 4) + IntToStrEx(Month, 2) + IntToStrEx(Day, 2) +
    IntToStrEx(Hour, 2) + IntToStrEx(Min, 2) + IntToStrEx(Sec, 2);
end;

// '20030203132345' ʽ���� 14 λ�����ַ���ת����ʱ��
function FlatStrToDateTime(const Section: string; var DateTime: TDateTime): Boolean;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  try
    Result := False;
    if Length(Section) <> 14 then Exit;
    Year := StrToInt(Copy(Section, 1, 4));
    Month := StrToInt(Copy(Section, 5, 2));
    Day := StrToInt(Copy(Section, 7, 2));
    Hour := StrToInt(Copy(Section, 9, 2));
    Min := StrToInt(Copy(Section, 11, 2));
    Sec := StrToInt(Copy(Section, 13, 2));
    MSec := 0;
    DateTime := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Min, Sec, MSec);
    Result := True;
  except
    Result := False;
  end;
end;

// ����ת��д���
function RMBFloatToChinese(ARMBCash: Real): string;
var
  tmp1, rr: string;
  l, i, j, k: integer;
const
  n1: array[0..9] of string = ('��', 'Ҽ', '��', '��', '��', '��', '½', '��', '��', '��');
  n2: array[0..3] of string = ('', 'ʰ', '��', 'Ǫ');
  n3: array[0..2] of string = ('Ԫ', '��', '��');
begin
  tmp1 := FormatFloat('#.00', ARMBCash);
  l := Length(tmp1);
  rr := '';
  if StrToInt(tmp1[l]) <> 0 then
  begin
    rr := '��';
    rr := n1[StrToInt(tmp1[l])] + rr;
  end;

  if StrToInt(tmp1[l - 1]) <> 0 then
  begin
    rr := '��' + rr;
    rr := n1[StrToInt(tmp1[l - 1])] + rr;
  end;
  i := l - 3;
  j := 0; k := 0;
  while i > 0 do
  begin
    if j mod 4 = 0 then
    begin
      rr := n3[k] + rr;
      inc(k); if k > 2 then k := 1;
      j := 0;
    end;
    if StrToInt(tmp1[i]) <> 0 then
      rr := n2[j] + rr;
    rr := n1[StrToInt(tmp1[i])] + rr;
    inc(j);
    Dec(i);
  end;
  while Pos('����', rr) > 0 do
    rr := stringreplace(rr, '����', '��', [rfReplaceAll]);
  rr := stringreplace(rr, '����', '����', [rfReplaceAll]);
  while Pos('����', rr) > 0 do
    rr := stringreplace(rr, '����', '��', [rfReplaceAll]);
  rr := stringreplace(rr, '����', '����', [rfReplaceAll]);
  while Pos('����', rr) > 0 do
    rr := stringreplace(rr, '����', '��', [rfReplaceAll]);
  rr := stringreplace(rr, '��Ԫ', 'Ԫ��', [rfReplaceAll]);
  while Pos('����', rr) > 0 do
    rr := stringreplace(rr, '����', '��', [rfReplaceAll]);
  rr := stringreplace(rr, '����', '��', [rfReplaceAll]);
  if Copy(rr, Length(rr) - 1, 2) = '��' then
    rr := Copy(rr, 1, Length(rr) - 2);
  if rr='' then
    rr:='��Ԫ';
  Result := rr;
end;

// ��������������˷��ı��ʽֵ�����ߣ��͹�ķ��
function EvalSimpleExpression(const Value: string): Double;
var
  Code, Temp: string;
  Loop, APos: Integer;
  Opers, Consts: TStrings; // ������ // ������
  AFlag: Boolean; // ��־��һ�����õ��ַ��Ƿ��ǲ�����
begin
  Result:= 0;
  AFlag:= True;
  Opers:= TStringList.Create;
  Consts:= TStringList.Create;

  try
    Code:= UpperCase(Trim(Value)); // ȡ��ʽ

    while Trim(Code) <> '' do
      case Code[1] of
        '+', '-', '*', '/', '^': // ����ǲ�����
          begin
            if not AFlag then
            begin
              Opers.Add(Code[1]);
              Delete(Code, 1, 1);
              Temp:= '';
              AFlag:= True; // ����˲������Ժ��ñ�־ΪTrue
            end
            else
            begin
              Temp:= Code[1];
              Delete(Code, 1, 1);
              AFlag:= False; // �����ñ�־ΪFalse
            end;
          end;

        '0'..'9', '.': // ����ǲ�����
          begin
            while Trim(Code) <> '' do
              if CharInSet(Code[1], ['0'..'9', '.']) then
              begin
                Temp:= Temp + Code[1];
                Delete(Code, 1, 1);
              end
              else
                Break;
                
            Consts.Add(Temp);
            AFlag:= False; // ����˲������Ժ��ñ�־ΪFalse
          end;

        '(':       // ���������
          begin
            Delete(Code, 1, 1);  // ɾ����һ��������
            APos:= 1;            // ���������������Ϊ�ҵ��������ű������Ŷ�
            Temp:= '';
            while Trim(Code) <> '' do
              if (Pos(')', Code) > -1) and (APos > 0) then
              begin
                if Code[1] = '(' then // ����ҵ������������������һ
                  Inc(APos)
                else if Code[1] = ')' then // ����ҵ��������������һ
                  Dec(APos);

                Temp:= Temp + Code[1];
                Delete(Code, 1, 1);
              end
              else
                Break;

            Temp:= Copy(Temp, 1, Length(Temp) - 1); // ɾ�����һ��������
            Consts.Add(FloatToStr(EvalSimpleExpression(Temp))); // �ݹ���ú����������ȼ��������ڵ�ֵ
            Temp:= '';
            AFlag:= False; // ��������Ժ��ñ�־ΪFalse
          end;

        else // ���������ַ�
          Delete(Code, 1, 1);
      end;

    if Opers.Count = 0 then // ���û�в�����
    begin
      if Consts.Count > 0 then // ����в�����
        Result:= StrToFloat(Consts.Strings[0]);
      Exit;
    end
    else if Consts.Count = 0 then // ���û�в�����
      Exit;

    Loop:= 0;
    while Opers.Count > 0 do
    begin
      if Opers.Strings[Loop] = '^' then // ����������ǳ˷�
      begin
        Consts.Strings[Loop]:= FloatToStr(Power(StrToFloat(Consts.Strings[Loop]), StrToFloat(Consts.Strings[Loop + 1])));
        Consts.Delete(Loop + 1);
        Opers.Delete(Loop);
        Loop:= 0;
      end
      else if Opers.IndexOf('^') > -1 then // ������Ǵη����ǻ��м���η�������
      begin
        Inc(Loop);
        Continue;
      end
      else if CharInSet(Opers.Strings[Loop][1], ['*', '/']) then // ����ǳ�/����
        case Opers.Strings[Loop][1] of
          '*':
            begin
              Consts.Strings[Loop]:= FloatToStr(StrToFloat(Consts.Strings[Loop]) * StrToFloat(Consts.Strings[Loop + 1]));
              Consts.Delete(Loop + 1);
              Opers.Delete(Loop);
              Loop:= 0;
            end;

          '/':
            begin
              Consts.Strings[Loop]:= FloatToStr(StrToFloat(Consts.Strings[Loop]) / StrToFloat(Consts.Strings[Loop + 1]));
              Consts.Delete(Loop + 1);
              Opers.Delete(Loop);
              Loop:= 0;
            end;
        end
      else if (Opers.IndexOf('*') > -1) or (Opers.IndexOf('/') > -1) then
      begin
        Inc(Loop);
        Continue;
      end
      else if CharInSet(Opers.Strings[Loop][1], ['+', '-']) then
        case Opers.Strings[Loop][1] of
          '+':
            begin
              Consts.Strings[Loop]:= FloatToStr(StrToFloat(Consts.Strings[Loop])
                + StrToFloat(Consts.Strings[Loop + 1]));
              Consts.Delete(Loop + 1);
              Opers.Delete(Loop);
              Loop:= 0;
            end;

          '-':
            begin
              Consts.Strings[Loop]:= FloatToStr(StrToFloat(Consts.Strings[Loop])
                - StrToFloat(Consts.Strings[Loop + 1]));
              Consts.Delete(Loop + 1);
              Opers.Delete(Loop);
              Loop:= 0;
            end;
        end
      else
        Inc(Loop);
    end;

    Result:= StrToFloat(Consts.Strings[0]);
  finally
    FreeAndNil(Consts);
    FreeAndNil(Opers);
  end;
end;

// ���ټ��㿪���ŵĵ���
function FastInverseSqrt(X: Single): Single;
var
  xHalf: Single;
  I: Integer;
begin
  xHalf := 0.5 * X;
  I := (PInteger(@X))^;
  I := $5f375a86 - (I shr 1);
  X := (PSingle(@I))^;
  X := X *(1.5 - xHalf * X * X);
  X := X *(1.5 - xHalf * X * X);
  Result := X;
end;

// ��λȷ�������ټ���������ƽ��������������
function FastSqrt(N: LongWord): LongWord;
var
  T, B: LongWord;
  Sft: LongWord;
begin
  Result := 0;
  B := $8000;
  Sft := 15;
  repeat
    T := ((Result shl 1)+ B) shl Sft;
    Dec(Sft);
    if N >= T then
    begin
      Result := Result + B;
      N := N - T;
    end;
    B := B shr 1;
  until B = 0;
end;

// ��λȷ�������ټ���������ƽ��������������
function FastSqrt64(N: Int64): Int64;
var
  T, B: Int64;
  Sft: Int64;
begin
  Result := 0;
  B := $80000000;
  Sft := 31;
  repeat
    T := ((Result shl 1)+ B) shl Sft;
    Dec(Sft);
    if N >= T then
    begin
      Result := Result + B;
      N := N - T;
    end;
    B := B shr 1;
  until B = 0;
end;

// �ַ���תע��������֧�� 'HKEY_CURRENT_USER' 'HKCR' �������ָ�ʽ
function StrToRegRoot(const s: string): HKEY;
begin
  if SameText(s, 'HKEY_CLASSES_ROOT') or SameText(s, 'HKCR') then
    Result := HKEY_CLASSES_ROOT
  else if SameText(s, 'HKEY_CURRENT_USER') or SameText(s, 'HKCU') then
    Result := HKEY_CURRENT_USER
  else if SameText(s, 'HKEY_LOCAL_MACHINE') or SameText(s, 'HKLM') then
    Result := HKEY_LOCAL_MACHINE
  else if SameText(s, 'HKEY_USERS') or SameText(s, 'HKU') then
    Result := HKEY_USERS
  else if SameText(s, 'HKEY_PERFORMANCE_DATA') or SameText(s, 'HKPD') then
    Result := HKEY_PERFORMANCE_DATA
  else if SameText(s, 'HKEY_CURRENT_CONFIG') or SameText(s, 'HKCC') then
    Result := HKEY_CURRENT_CONFIG
  else if SameText(s, 'HKEY_DYN_DATA') or SameText(s, 'HKDD') then
    Result := HKEY_DYN_DATA
  else
    Result := HKEY_CURRENT_USER;
end;

// ע������ת�ַ�������ѡ 'HKEY_CURRENT_USER' 'HKCR' �������ָ�ʽ
function RegRootToStr(Key: HKEY; ShortFormat: Boolean): string;
begin
  if Key = HKEY_CLASSES_ROOT then
    if ShortFormat then
      Result := 'HKCR'
    else
      Result := 'HKEY_CLASSES_ROOT'
  else if Key = HKEY_CURRENT_USER then
    if ShortFormat then
      Result := 'HKCU'
    else
      Result := 'HKEY_CURRENT_USER'
  else if Key = HKEY_LOCAL_MACHINE then
    if ShortFormat then
      Result := 'HKLM'
    else
      Result := 'HKEY_LOCAL_MACHINE'
  else if Key = HKEY_USERS then
    if ShortFormat then
      Result := 'HKU'
    else
      Result := 'HKEY_USERS'
  else if Key = HKEY_PERFORMANCE_DATA then
    if ShortFormat then
      Result := 'HKPD'
    else
      Result := 'HKEY_PERFORMANCE_DATA'
  else if Key = HKEY_CURRENT_CONFIG then
    if ShortFormat then
      Result := 'HKCC'
    else
      Result := 'HKEY_CURRENT_CONFIG'
  else if Key = HKEY_DYN_DATA then
    if ShortFormat then
      Result := 'HKDD'
    else
      Result := 'HKEY_DYN_DATA'
  else
    Result := ''
end;

// ���ַ����з�����Ӵ�
function ExtractSubstr(const S: string; var Pos: Integer;
  const Delims: TSysCharSet): string;
var
  i: Integer;
begin
  i := Pos;
  while (i <= Length(S)) and not CharInSet(S[i], Delims) do Inc(i);
  Result := Copy(S, Pos, i - Pos);
  if (i <= Length(S)) and CharInSet(S[i], Delims) then Inc(i);
  Pos := i;
end;

// �ļ���ͨ����Ƚ�
function WildcardCompare(const FileWildcard, FileName: string; const IgnoreCase:
  Boolean): Boolean;

  function WildCompare(var WildS, IstS: string): Boolean;
  var
    WildPos, FilePos, l, p: Integer;
  begin
    // Start at the first wildcard/filename character
    WildPos := 1; // Wildcard position.
    FilePos := 1; // FileName position.
    while (WildPos <= Length(WildS)) do
    begin
      // '*' matches any sequence of characters.
      if WildS[WildPos] = '*' then
      begin
        // We've reached the end of the wildcard string with a * and are done.
        if WildPos = Length(WildS) then
        begin
          Result := True;
          Exit;
        end
        else
        begin
          l := WildPos + 1;
          // Anything after a * in the wildcard must match literally.
          while (l < Length(WildS)) and (WildS[l + 1] <> '*') do
            Inc(l);
          // Check for the literal match immediately after the current position.
          p := Pos(Copy(WildS, WildPos + 1, l - WildPos), IstS);
          if p > 0 then
            FilePos := p - 1
          else
          begin
            Result := False;
            Exit;
          end;
        end;
      end
      // '?' matches any character - other characters must literally match.
      else if (WildS[WildPos] <> '?') and ((Length(IstS) < WildPos) or
        (WildS[WildPos] <> IstS[FilePos])) then
      begin
        Result := False;
        Exit;
      end;
      // Match is OK so far - check the next character.
      Inc(WildPos);
      Inc(FilePos);
    end;
    Result := (FilePos > Length(IstS));
  end;

  function LastCharPos(const S: string; C: Char): Integer;
  var
    i: Integer;
  begin
    i := Length(S);
    while (i > 0) and (S[i] <> C) do
      Dec(i);
    Result := i;
  end;

var
  NameWild, NameFile, ExtWild, ExtFile: string;
  DotPos: Integer;
begin
  // Parse to find the extension and name base of filename and wildcard.
  DotPos := LastCharPos(FileWildcard, '.');
  if DotPos = 0 then
  begin
    // Assume .* if an extension is missing
    NameWild := FileWildcard;
    ExtWild := '*';
  end
  else
  begin
    NameWild := Copy(FileWildcard, 1, DotPos - 1);
    ExtWild := Copy(FileWildcard, DotPos + 1, Length(FileWildcard));
  end;

  // We could probably modify this to use _CnExtractFileExt, etc.
  DotPos := LastCharPos(FileName, '.');
  if DotPos = 0 then
    DotPos := Length(FileName) + 1;

  NameFile := Copy(FileName, 1, DotPos - 1);
  ExtFile := Copy(FileName, DotPos + 1, Length(FileName));
  // Case insensitive check
  if IgnoreCase then
  begin
    NameWild := AnsiUpperCase(NameWild);
    NameFile := AnsiUpperCase(NameFile);
    ExtWild := AnsiUpperCase(ExtWild);
    ExtFile := AnsiUpperCase(ExtFile);
  end;
  // Both the extension and the filename must match
  Result := WildCompare(NameWild, NameFile) and WildCompare(ExtWild, ExtFile);
end;

// ���ݵ�ǰ���̲��ֽ�����ɨ����ת���� ASCII �ַ������� WM_KEYDOWN �ȴ�ʹ��
// ���ڲ����� ToAscii���ʿ�֧��ʹ�� Accent Character �ļ��̲���
function ScanCodeToAscii(Code: Word): AnsiChar;
var
  i: Byte;
  C: Cardinal;
begin
  C := Code;
  if GetKeyState(VK_SHIFT) < 0 then
    C := C or $10000;
  if GetKeyState(VK_CONTROL) < 0 then
    C := C or $20000;
  if GetKeyState(VK_MENU) < 0 then
    C := C or $40000;
  for i := Low(Byte) to High(Byte) do
    if OemKeyScan(i) = C then
    begin
      Result := AnsiChar(i);
      Exit;
    end;
  Result := #0;
end;

// ����һ��������Ƿ� Dead key
function IsDeadKey(Key: Word): Boolean;
begin
  Result := MapVirtualKey(Key, 2) and $80000000 <> 0;
end;

// ���ݵ�ǰ����״̬�������ת���� ASCII �ַ������� WM_KEYDOWN �ȴ�ʹ��
// ���ܻᵼ�� Accent Character ����ȷ
function VirtualKeyToAscii(Key: Word): AnsiChar;
var
  KeyState: TKeyboardState;
  ScanCode: Word;
  Buff: array[0..1] of AnsiChar;
begin
  Result := #0;
  if not IsDeadKey(Key) then
  begin
    case Key of
      VK_SHIFT, VK_CONTROL, VK_MENU:
        ;
    else
      begin
        ScanCode := MapVirtualKey(Key, 0);
        GetKeyboardState(KeyState);
        if ToAscii(Key, ScanCode, KeyState, @Buff, 0) = 1 then
          Result := Buff[0];
      end;
    end;
  end;
end;

// ���ݵ�ǰ�ļ��̲��ֽ��������ɨ����ת���� ASCII �ַ���ͨ�������������С���̣�
// ɨ���봦�����̣�֧�� Accent Character �ļ��̲���
function VK_ScanCodeToAscii(VKey: Word; Code: Word): AnsiChar;
begin
  if (VKey >= VK_NUMPAD0) and (VKey <= VK_DIVIDE) then
  begin
    case VKey of
      VK_NUMPAD0..VK_NUMPAD9:
        if IsNumLockDown then
          Result := AnsiChar(Ord('0') + VKey - VK_NUMPAD0)
        else
          Result := #0;
      VK_MULTIPLY: Result := '*';
      VK_ADD: Result := '+';
      VK_SEPARATOR: Result := #13;
      VK_SUBTRACT: Result := '-';
      VK_DECIMAL: Result := '.';
      VK_DIVIDE: Result := '/';
    else
      Result := #0;
    end;
  end
  else
  begin
    Result := ScanCodeToAscii(Code);
  end;    
end;

// ���ص�ǰ�İ���״̬���ݲ�֧�� ssDouble ״̬
function GetShiftState: TShiftState;
var
  KeyState: TKeyboardState;

  function IsDown(Key: Byte): Boolean;
  begin
    Result := (Key and $80) = $80;
  end;
begin
  Result := [];
  GetKeyboardState(KeyState);
  if IsDown(KeyState[VK_LSHIFT]) or IsDown(KeyState[VK_RSHIFT]) then
    Include(Result, ssShift);
  if IsDown(KeyState[VK_LMENU]) or IsDown(KeyState[VK_RMENU]) then
    Include(Result, ssAlt);
  if IsDown(KeyState[VK_LCONTROL]) or IsDown(KeyState[VK_RCONTROL]) then
    Include(Result, ssCtrl);
  if IsDown(KeyState[VK_LBUTTON]) then
    Include(Result, ssLeft);
  if IsDown(KeyState[VK_RBUTTON]) then
    Include(Result, ssRight);
  if IsDown(KeyState[VK_MBUTTON]) then
    Include(Result, ssMiddle);
end;

// �жϵ�ǰ Shift �Ƿ���
function IsShiftDown: Boolean;
begin
  Result := ssShift in GetShiftState;
end;

// �жϵ�ǰ Alt �Ƿ���
function IsAltDown: Boolean;
begin
  Result := ssAlt in GetShiftState;
end;

// �жϵ�ǰ Ctrl �Ƿ���
function IsCtrlDown: Boolean;
begin
  Result := ssCtrl in GetShiftState;
end;

// �жϵ�ǰ Insert �Ƿ���
function IsInsertDown: Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := Odd(KeyState[VK_INSERT]);
end;

// �жϵ�ǰ Caps Lock �Ƿ���
function IsCapsLockDown: Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := Odd(KeyState[VK_CAPITAL]);
end;

// �жϵ�ǰ NumLock �Ƿ���
function IsNumLockDown: Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := Odd(KeyState[VK_NUMLOCK]);
end;

// �жϵ�ǰ Scroll Lock �Ƿ���
function IsScrollLockDown: Boolean;
var
  KeyState: TKeyboardState;
begin
  GetKeyboardState(KeyState);
  Result := Odd(KeyState[VK_SCROLL]);
end;

// ɾ������ǰ׺ T
function RemoveClassPrefix(const ClassName: string): string;
begin
  Result := ClassName;
  if (Result <> '') and (UpperCase(Result[1]) = 'T') then
    Delete(Result, 1, 1);
end;

// �÷ֺŷָ������ߡ������ַ���ת��Ϊ�����ʽ
function CnAuthorEmailToStr(Author, Email: string): string;
var
  s1, s2: string;

  function GetLeftStr(var s: string; Sep: string): string;
  var
    i: Integer;
  begin
    Result := '';
    i := AnsiPos(Sep, s);
    if i > 0 then
    begin
      Result := Trim(Copy(s, 1, i - 1));
      Delete(s, 1, i);
    end
    else begin
      Result := s;
      s := '';
    end;
  end;

begin
  Result := '';
  s1 := GetLeftStr(Author, ';');
  s2 := GetLeftStr(Email, ';');
  while s1 <> '' do
  begin
    if Result <> '' then Result := Result + #13#10;
    Result := Result + s1;
    if s2 <> '' then Result := Result + ' (' + s2 + ')';
    s1 := GetLeftStr(Author, ';');
    s2 := GetLeftStr(Email, ';');
  end;
end;

//------------------------------------------------------------------------------
// ��չ�ĶԻ�����
//------------------------------------------------------------------------------

// ��ʾ��ʾ����
procedure InfoDlg(const Mess: string; Caption: string; Flags: Integer);
begin
  if Caption = '' then
    Caption := SCnInformation;
  Application.MessageBox(PChar(Mess), PChar(Caption), Flags);
end;

// ��ʾ��ʾȷ�ϴ���
function InfoOk(const Mess: string; Caption: string): Boolean;
begin
  if Caption = '' then
    Caption := SCnInformation;
  Result := Application.MessageBox(PChar(Mess), PChar(Caption),
    MB_OKCANCEL + MB_ICONINFORMATION) = IDOK;
end;

// ��ʾ���󴰿�
procedure ErrorDlg(const Mess: string; Caption: string);
begin
  if Caption = '' then
    Caption := SCnError;
  Application.MessageBox(PChar(Mess), PChar(Caption), MB_OK + MB_ICONSTOP);
end;

// ��ʾ���洰��
procedure WarningDlg(const Mess: string; Caption: string);
begin
  if Caption = '' then
    Caption := SCnWarning;
  Application.MessageBox(PChar(Mess), PChar(Caption), MB_OK + MB_ICONWARNING);
end;

// ��ʾ��ѯ�Ƿ񴰿�
function QueryDlg(const Mess: string; DefaultNo: Boolean; Caption: string): Boolean;
const
  Defaults: array[Boolean] of DWORD = (0, MB_DEFBUTTON2);
begin
  if Caption = '' then
    Caption := SCnInformation;
  Result := Application.MessageBox(PChar(Mess), PChar(Caption),
    MB_YESNO + MB_ICONQUESTION + Defaults[DefaultNo]) = IDYES;
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;

// ����Ի���
function CnInputQuery(const ACaption, APrompt: string;
  var Value: string; Ini: TCustomIniFile; const Section: string;
  APassword: Boolean): Boolean;
var
  Form: TForm;
  Prompt: TLabel;
  Edit: TEdit;
  ComboBox: TComboBox;
  DialogUnits: TPoint;
  ButtonTop, ButtonWidth, ButtonHeight: Integer;
{$IFDEF CREATE_PARAMS_BUG}
  OldLong: Longint;
  AHandle: THandle;
  NeedChange: Boolean;
{$ENDIF}
begin
  Result := False;
  Edit := nil;
  ComboBox := nil;

{$IFDEF CREATE_PARAMS_BUG}
  NeedChange := False;
  OldLong := 0;
  AHandle := Application.ActiveFormHandle;
{$ENDIF}

  Form := TForm.Create(Application);
  with Form do
    try
      Scaled := False;
      Font.Handle := GetStockObject(DEFAULT_GUI_FONT);
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;

      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := APrompt;
      end;

      if Assigned(Ini) then
      begin
        ComboBox := TComboBox.Create(Form);
        with ComboBox do
        begin
          Parent := Form;
          Left := Prompt.Left;
          Top := MulDiv(19, DialogUnits.Y, 8);
          Width := MulDiv(164, DialogUnits.X, 4);
          // MaxLength := 1024;
          ReadStringsFromIni(Ini, Section, ComboBox.Items);
          if (Value = '') and (ComboBox.Items.Count > 0) then
            Text := ComboBox.Items[0]
          else
            Text := Value;
          SelectAll;
        end;
      end
      else
      begin
        Edit := TEdit.Create(Form);
        with Edit do
        begin
          Parent := Form;
          Left := Prompt.Left;
          Top := MulDiv(19, DialogUnits.Y, 8);
          Width := MulDiv(164, DialogUnits.X, 4);
          // MaxLength := 1024;
          if APassword then
            PasswordChar := '*';
          Text := Value;
          SelectAll;
        end;
      end;

      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);

      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SCnMsgDlgOK;
        ModalResult := mrOk;
        Default := True;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;

      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SCnMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;

{$IFDEF CREATE_PARAMS_BUG}
      if AHandle <> 0 then
      begin
        OldLong := GetWindowLong(AHandle, GWL_EXSTYLE);
        NeedChange := OldLong and WS_EX_TOOLWINDOW = WS_EX_TOOLWINDOW;
        if NeedChange then
          SetWindowLong(AHandle, GWL_EXSTYLE, OldLong and not WS_EX_TOOLWINDOW);
      end;
{$ENDIF}

      if ShowModal = mrOk then
      begin
        if Assigned(ComboBox) then
        begin
          Value := ComboBox.Text;
          AddComboBoxTextToItems(ComboBox);
          WriteStringsToIni(Ini, Section, ComboBox.Items);
        end
        else
          Value := Edit.Text;
        Result := True;
      end;
    finally
{$IFDEF CREATE_PARAMS_BUG}
      if NeedChange and (OldLong <> 0) then
        SetWindowLong(AHandle, GWL_EXSTYLE, OldLong);
{$ENDIF}
      Form.Free;
    end;
end;

// ����Ի���
function CnInputBox(const ACaption, APrompt, ADefault: string;
  Ini: TCustomIniFile; const Section: string): string;
begin
  Result := ADefault;
  CnInputQuery(ACaption, APrompt, Result, Ini, Section);
end;

//------------------------------------------------------------------------------
// λ��չ����ʱ���������
//------------------------------------------------------------------------------

function GetYear(Date: TDate): Integer;
var
  y, m, d: WORD;
begin
  DecodeDate(Date, y, m, d);
  Result := y;
end;

function GetMonth(Date: TDate): Integer;
var
  y, m, d: WORD;
begin
  DecodeDate(Date, y, m, d);
  Result := m;
end;

function GetDay(Date: TDate): Integer;
var
  y, m, d: WORD;
begin
  DecodeDate(Date, y, m, d);
  Result := d;
end;

function GetHour(Time: TTime): Integer;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(Time, h, m, s, ms);
  Result := h;
end;

function GetMinute(Time: TTime): Integer;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(Time, h, m, s, ms);
  Result := m;
end;

function GetSecond(Time: TTime): Integer;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(Time, h, m, s, ms);
  Result := s;
end;

function GetMSecond(Time: TTime): Integer;
var
  h, m, s, ms: WORD;
begin
  DecodeTime(Time, h, m, s, ms);
  Result := ms;
end;

//------------------------------------------------------------------------------
// λ��������
//------------------------------------------------------------------------------

// ����λ
procedure SetBit(var Value: Byte; Bit: TByteBit; IsSet: Boolean);
begin
  if IsSet then
    Value := Value or (1 shl Bit)
  else
    Value := Value and not (1 shl Bit);
end;

procedure SetBit(var Value: WORD; Bit: TWordBit; IsSet: Boolean);
begin
  if IsSet then
    Value := Value or (1 shl Bit)
  else
    Value := Value and not (1 shl Bit);
end;

procedure SetBit(var Value: DWORD; Bit: TDWordBit; IsSet: Boolean);
begin
  if IsSet then
    Value := Value or (1 shl Bit)
  else
    Value := Value and not (1 shl Bit);
end;

// ȡλ
function GetBit(Value: Byte; Bit: TByteBit): Boolean;
begin
  Result := Value and (1 shl Bit) <> 0;
end;

function GetBit(Value: WORD; Bit: TWordBit): Boolean;
begin
  Result := Value and (1 shl Bit) <> 0;
end;

function GetBit(Value: DWORD; Bit: TDWordBit): Boolean;
begin
  Result := Value and (1 shl Bit) <> 0;
end;

function CountSetBits(const Value: Cardinal): Integer;
var
  LS: DWORD;
  BT: Int64;
  I: DWORD;
begin
  LS := SizeOf(Cardinal) * 8 - 1;
  Result := 0;
  BT := 1 shl LS;

  for I := 0 to LS do
  begin
    if (Value and BT) <> 0 then
      Inc(Result);
    BT := BT shr 1;
  end;
end;

//------------------------------------------------------------------------------
// ϵͳ���ܺ���
//------------------------------------------------------------------------------

// �ƶ���굽�ؼ�
procedure MoveMouseIntoControl(AWinControl: TControl);
var
  rtControl: TRect;
begin
  rtControl := AWinControl.BoundsRect;
  MapWindowPoints(AWinControl.Parent.Handle, 0, rtControl, 2);
  SetCursorPos(rtControl.Left + (rtControl.Right - rtControl.Left) div 2,
    rtControl.Top + (rtControl.Bottom - rtControl.Top) div 2);
end;

// �� ComboBox ���ı��������ӵ������б���
procedure AddComboBoxTextToItems(ComboBox: TComboBox; MaxItemsCount: Integer = 10);
var
  Text: string;
begin
  if ComboBox.Text <> '' then
  begin
    Text := ComboBox.Text;
    if ComboBox.Items.IndexOf(ComboBox.Text) < 0 then
      ComboBox.Items.Insert(0, ComboBox.Text)
    else
      ComboBox.Items.Move(ComboBox.Items.IndexOf(ComboBox.Text), 0);
    while (MaxItemsCount > 1) and (ComboBox.Items.Count > MaxItemsCount) do
      ComboBox.Items.Delete(ComboBox.Items.Count - 1);
    ComboBox.Text := Text;
  end;
end;

// ��̬���÷ֱ���
function DynamicResolution(x, y: WORD): Boolean;
var
  lpDevMode: TDeviceMode;
begin
  Result := EnumDisplaySettings(nil, 0, lpDevMode);
  if Result then
  begin
    lpDevMode.dmFields := DM_PELSWIDTH or DM_PELSHEIGHT;
    lpDevMode.dmPelsWidth := x;
    lpDevMode.dmPelsHeight := y;
    Result := ChangeDisplaySettings(lpDevMode, 0) = DISP_CHANGE_SUCCESSFUL;
  end;
end;

// �������Ϸ���ʾ
procedure StayOnTop(Handle: HWND; OnTop: Boolean);
const
  csOnTop: array[Boolean] of HWND = (HWND_NOTOPMOST, HWND_TOPMOST);
begin
  SetWindowPos(Handle, csOnTop[OnTop], 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or
    SWP_NOACTIVATE);
end;

var
  WndLong: Integer;

// ���ó����Ƿ������������
procedure SetHidden(Hide: Boolean);
begin
  ShowWindow(Application.Handle, SW_HIDE);
  if Hide then
    SetWindowLong(Application.Handle, GWL_EXSTYLE,
      WndLong or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW or WS_EX_TOPMOST)
  else
    SetWindowLong(Application.Handle, GWL_EXSTYLE, WndLong);
  ShowWindow(Application.Handle, SW_SHOW);
end;

const
  csWndShowFlag: array[Boolean] of DWORD = (SW_HIDE, SW_RESTORE);

// �����������Ƿ�ɼ�
procedure SetTaskBarVisible(Visible: Boolean);
var
  wndHandle: THandle;
begin
  wndHandle := FindWindow('Shell_TrayWnd', nil);
  ShowWindow(wndHandle, csWndShowFlag[Visible]);
end;

// ���������Ƿ�ɼ�
procedure SetDesktopVisible(Visible: Boolean);
var
  hDesktop: THandle;
begin
  hDesktop := FindWindow('Progman', nil);
  ShowWindow(hDesktop, csWndShowFlag[Visible]);
end;

type
  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF;
    bAlpha: Byte; dwFlags: DWORD): BOOL; stdcall;

var
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes;

procedure InitSetLayeredWindowAttributesFunc;
const
  sUser32 = 'User32.dll';
var
  ModH: HMODULE;
begin
  ModH := GetModuleHandle(sUser32);
  if ModH <> 0 then
     @SetLayeredWindowAttributes := GetProcAddress(ModH, 'SetLayeredWindowAttributes');
end;

// ���ô��� Alpha ͸��ֵ
function CnSetWindowAlphaBlend(Hwnd: THandle; Alpha: Byte): Boolean;
const
  WS_EX_LAYERED = $00080000;
  LWA_ALPHA = $00000002;
var
  AStyle: Integer;
begin
  if Assigned(SetLayeredWindowAttributes) then
  begin
    AStyle := GetWindowLong(Hwnd, GWL_EXSTYLE);
    if (AStyle and WS_EX_LAYERED) = 0 then
      SetWindowLong(Hwnd, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
    Result := SetLayeredWindowAttributes(Hwnd, 0, Alpha, LWA_ALPHA);
  end
  else
    Result := False;
end;

// ǿ����һ��������ʾ��ǰ̨
function ForceForegroundWindow(HWND: HWND): Boolean;
var
  ThreadID1, ThreadID2: DWORD;
begin
  if HWND = GetForegroundWindow then
    Result := True
  else
  begin
    ThreadID1 := GetWindowThreadProcessId(GetForegroundWindow, nil);
    ThreadID2 := GetWindowThreadProcessId(HWND, nil);
    if ThreadID1 <> ThreadID2 then
    begin
      AttachThreadInput(ThreadID1, ThreadID2, True);
      Result := SetForegroundWindow(HWND);
      AttachThreadInput(ThreadID1, ThreadID2, False);
    end
    else
      Result := SetForegroundWindow(HWND);
    if IsIconic(HWND) then
      ShowWindow(HWND, SW_RESTORE)
    else
      ShowWindow(HWND, SW_SHOW);
  end;
end;

// ȡ��������
function GetWorkRect(const Form: TCustomForm = nil): TRect;
var
  Monitor: TMonitor;
  MonInfo: TMonitorInfo;
begin
  Result.Top := 0;
  Result.Left := 0;
  Result.Right := Screen.Width;
  Result.Bottom := Screen.Height;
  if Assigned(Form) then
  begin
    Monitor := Form.Monitor;
    if Assigned(Monitor) then
    begin
      MonInfo.cbSize := SizeOf(MonInfo);
      GetMonitorInfo(Monitor.Handle, @MonInfo);
      Result := MonInfo.rcWork;
    end;
  end
  else
    SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
end;

// ��ʾ�ȴ����
procedure BeginWait;
begin
  Screen.Cursor := crHourGlass;
end;

// �����ȴ����
procedure EndWait;
begin
  Screen.Cursor := crDefault;
end;

// ����Ƿ�Win95/98ƽ̨
function CheckWindows9598: Boolean;
var
  V: TOSVersionInfo;
begin
  V.dwOSVersionInfoSize := SizeOf(V);
  Result := False;
  if not GetVersionEx(V) then Exit;
  if V.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
    Result := True;
end;

// ����Ƿ�WinXP����ƽ̨
function CheckWinXP: Boolean;
begin
  Result := (Win32MajorVersion > 5) or
    ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1));
end;

// ����Ƿ� Vista/Win7 ����ϵͳ
function CheckWinVista: Boolean;
begin
  Result := Win32MajorVersion >= 6;
end;

// ����Ƿ� 64bit ϵͳ
function CheckWow64: Boolean;
type
  TIsWow64ProcessProc = function(Handle: THandle; var IsWow64: LongBool): LongBool stdcall;
var
  proc: TIsWow64ProcessProc;
  IsWow64: LongBool;
begin
  Result := False;
  proc := TIsWow64ProcessProc(GetProcAddress(GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(proc) then
  begin
    if proc(GetCurrentProcess, IsWow64) then
      Result := IsWow64;
  end;
end;

// ���ϵͳ�͵�ǰ�����Ƿ�֧�� XP Manifest
function CheckXPManifest(var OSSupport, AppValid: Boolean): Boolean;
var
  hProc: THandle;
  hMods: array[0..1023] of HMODULE;
  Name: array[0..1023] of AnsiChar;
  cbNeeded: DWORD;
  i: Integer;
  Ver: TVersionNumber;
begin
  Result := False;
  OSSupport := False;
  AppValid := True;
  hProc := GetCurrentProcess;
  EnumProcessModules(hProc, @hMods, SizeOf(hMods), cbNeeded);
  for i := 0 to cbNeeded div SizeOf(HMODULE) - 1 do
  begin
    if GetModuleFileNameExA(hProc, hMods[i], Name, SizeOf(Name)) > 0 then
    begin
      if Pos(UpperCase(comctl32), UpperCase(string(Name))) > 0 then
      begin
        Ver := GetFileVersionNumber(string(Name));
        if Ver.Major <= 5 then
        begin
          AppValid := False;
        end
        else if Ver.Major >= 6 then
        begin
          OSSupport := True;
        end;
      end;
      Result := True;
    end;
  end;
  if not OSSupport then
    AppValid := False;
end;  

// ���Dll�İ汾��Ϣ
function DllGetVersion(const dllname: string;
  var DVI: TDLLVERSIONINFO2): Boolean;
type
  _DllGetVersion = function (var DVI: TDLLVERSIONINFO2): DWORD; stdcall;
var
  hMod:THandle;
  pfDllVersion: _DllGetVersion;
begin
  Result := False;
  hMod := LoadLibrary(PChar(dllname));
  if hMod <> 0 then
  try
    @pfDllVersion := GetProcAddress(hMod, 'DllGetVersion');
    if @pfDllVersion = nil then
      Exit;
    FillChar(DVI, SizeOf(TDLLVERSIONINFO2), 0);
    DVI.info1.cbSize := SizeOf(TDLLVERSIONINFO2);
    Result := pfDllVersion(DVI) and $80000000 = 0;
  finally
    FreeLibrary(hMod);
  end;
end;

// ���ز���ϵͳ��ʶ��
function GetOSString: string;
var
  OSPlatform: string;
  BuildNumber: Integer;
begin
  Result := 'Unknown Windows Version';
  OSPlatform := 'Windows';
  BuildNumber := 0;

  case Win32Platform of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        BuildNumber := Win32BuildNumber and $0000FFFF;
        case Win32MinorVersion of
          0..9:
            begin
              if Trim(Win32CSDVersion) = 'B' then
                OSPlatform := 'Windows 95 OSR2'
              else
                OSPlatform := 'Windows 95';
            end;
          10..89:
            begin
              if Trim(Win32CSDVersion) = 'A' then
                OSPlatform := 'Windows 98'
              else
                OSPlatform := 'Windows 98 SE';
            end;
          90:
            OSPlatform := 'Windows Millennium';
        end;
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        if Win32MajorVersion in [3, 4] then
          OSPlatform := 'Windows NT'
        else if Win32MajorVersion = 5 then
        begin
          case Win32MinorVersion of
            0: OSPlatform := 'Windows 2000';
            1: OSPlatform := 'Windows XP';
          end;
        end;
        BuildNumber := Win32BuildNumber;
      end;
    VER_PLATFORM_WIN32s:
      begin
        OSPlatform := 'Win32s';
        BuildNumber := Win32BuildNumber;
      end;
  end;
  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) or
    (Win32Platform = VER_PLATFORM_WIN32_NT) then
  begin
    if Trim(Win32CSDVersion) = '' then
      Result := Format('%s %d.%d (Build %d)', [OSPlatform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber])
    else
      Result := Format('%s %d.%d (Build %d: %s)', [OSPlatform, Win32MajorVersion,
        Win32MinorVersion, BuildNumber, Win32CSDVersion]);
  end
  else
    Result := Format('%s %d.%d', [OSPlatform, Win32MajorVersion, Win32MinorVersion])
end;

// �õ�������
function GetComputeNameStr : string;
var
  dwBuff : DWORD;
  aryCmpName : array [0..255] of Char;
begin
  Result := '';
  dwBuff := 256;
  FillChar(aryCmpName, SizeOf(aryCmpName), 0);
  if GetComputerName(aryCmpName, dwBuff) then
    Result := StrPas(aryCmpName);
end;

// �õ������û���
function GetLocalUserName: string;
var
  Count: DWORD;
begin
  Count := 256 + 1; // UNLEN + 1
  // set buffer size to 256 + 2 characters
  SetLength(Result, Count);
  if GetUserName(PChar(Result), Count) then
    StrResetLength(Result)
  else
    Result := '';
end;

function REG_CURRENT_VERSION: string;
begin
  if CheckWindows9598 then
    Result := HKLM_CURRENT_VERSION_WINDOWS
  else
    Result := HKLM_CURRENT_VERSION_NT;
end;

function GetRegisteredCompany: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOrganization', '');
end;

function GetRegisteredOwner: string;
begin
  Result := RegReadStringDef(HKEY_LOCAL_MACHINE, REG_CURRENT_VERSION, 'RegisteredOwner', '');
end;

//------------------------------------------------------------------------------
// ���ÿؼ���������
//------------------------------------------------------------------------------

procedure ListViewSwapItem(ListView: TListView; Idx1, Idx2: Integer);
var
  S: string;
  Data: Pointer;
  Sel: Boolean;
begin
  S := ListView.Items[Idx1].SubItems.Text;
  Data := ListView.Items[Idx1].Data;
  Sel := ListView.Items[Idx1].Selected;
  ListView.Items[Idx1].SubItems.Assign(ListView.Items[Idx2].SubItems);
  ListView.Items[Idx1].Data := ListView.Items[Idx2].Data;
  ListView.Items[Idx1].Selected := ListView.Items[Idx2].Selected;
  ListView.Items[Idx2].SubItems.Text := S;
  ListView.Items[Idx2].Data := Data;
  ListView.Items[Idx2].Selected := Sel;
end;

procedure ListViewDeleteSelected(ListView: TListView);
var
  i: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for i := ListView.Items.Count - 1 downto 0 do
      if ListView.Items[i].Selected then
        ListView.Items.Delete(i);
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure ListViewMoveDownSelected(ListView: TListView);
var
  i: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for i := ListView.Items.Count - 2 downto 0 do
      if ListView.Items[i].Selected and not ListView.Items[i + 1].Selected then
        ListViewSwapItem(ListView, i, i + 1);
  finally
    ListView.Items.EndUpdate;
  end;
end;

procedure ListViewMoveUpSelected(ListView: TListView);
var
  i: Integer;
begin
  ListView.Items.BeginUpdate;
  try
    for i := 1 to ListView.Items.Count - 1 do
      if ListView.Items[i].Selected and not ListView.Items[i - 1].Selected then
        ListViewSwapItem(ListView, i, i - 1);
  finally
    ListView.Items.EndUpdate;
  end;
end;

// Ϊ Listbox ����ˮƽ������
procedure ListboxHorizontalScrollbar(Listbox: TCustomListBox);
var
  i: Integer;
  Width, MaxWidth: Integer;
begin
  Assert(Assigned(Listbox));
  MaxWidth := 0;
  for i := 0 to Listbox.Items.Count - 1 do
  begin
    Width := Listbox.Canvas.TextWidth(Listbox.Items[i]) + 4;
    if Width > MaxWidth then
      MaxWidth := Width;
  end;
  if ListBox is TCheckListBox then
    Inc(MaxWidth, GetSystemMetrics(SM_CXMENUCHECK) + 2);
  SendMessage(Listbox.Handle, LB_SETHORIZONTALEXTENT, MaxWidth, 0);
end;

// ���Ʋ˵����������
procedure CloneMenuItem(Source, Dest: TMenuItem);
var
  Item, AItem: TMenuItem;
  I: Integer;
begin
  if (Source <> nil) and (Dest <> nil) then
  begin
    Dest.Clear;

    for I := 0 to Source.Count - 1 do
    begin
      Item := TMenuItem.Create(Dest.Owner);
      AItem := Source.Items[I];

      if AItem.Action <> nil then
        Item.Action := AItem.Action
      else
      begin
        Item.Caption := AItem.Caption;
        Item.Hint := AItem.Hint;
        Item.Tag := AItem.Tag;
        Item.ShortCut := AItem.ShortCut;
        Item.OnClick := AItem.OnClick;

        Item.Checked := AItem.Checked;
        Item.ImageIndex := AItem.ImageIndex;
      end;

      Dest.Add(Item);

      CloneMenuItem(Source.Items[I], Item);
    end;
  end;
end;

//------------------------------------------------------------------------------
// ��������
//------------------------------------------------------------------------------

type
  TWinControlAccess = class(TWinControl);
  TControlAccess = class(TControl);

// ��ȡ Control �����λͼ��ResetSize Ϊ True ��ʾʹ�� Control �ߴ�����λͼ�ߴ�
function GetControlBitmap(AControl: TControl; Bmp: TBitmap;
  ResetSize: Boolean): Boolean;
begin
  Result := False;
  if (AControl = nil) or (Bmp = nil) then
    Exit;

  if ResetSize then
  begin
    Bmp.PixelFormat := pf24Bit;
    Bmp.Canvas.Brush.Color := TControlAccess(AControl).Color;
    Bmp.Width := AControl.Width;
    Bmp.Height := AControl.Height;
  end;

  if AControl is TWinControl then
    TWinControlAccess(AControl).PaintWindow(Bmp.Canvas.Handle)
  else
    AControl.Perform(WM_PAINT, Bmp.Canvas.Handle, 0);
  Result := True;
end;

// ��ö���ʾ������£������������������ʾ��ԭ�������
function GetMultiMonitorDesktopRect: TRect;
var
  I: Integer;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Bottom := Screen.DesktopHeight;
  Result.Right := Screen.DesktopWidth;

  for I := 0 to Screen.MonitorCount - 1 do
  begin
    if Screen.Monitors[I].Left < Result.Left then
      Result.Left := Screen.Monitors[I].Left;
    if Screen.Monitors[I].Top < Result.Top then
      Result.Top := Screen.Monitors[I].Top;
    if Screen.Monitors[I].Height + Screen.Monitors[I].Top > Result.Bottom then
      Result.Bottom := Screen.Monitors[I].Height + Screen.Monitors[I].Top;
    if Screen.Monitors[I].Width + Screen.Monitors[I].Left > Result.Right then
      Result.Right := Screen.Monitors[I].Width + Screen.Monitors[I].Left;
  end;
end;

// ���������Min..Max֮��
function TrimInt(Value, Min, Max: Integer): Integer; overload;
begin
  if Value > Max then
    Result := Max
  else if Value < Min then
    Result := Min
  else
    Result := Value;
end;

// �Ƚ�����������V1 > V2 ���� 1��V1 < V2 ���� -1��V1 = V2 ���� 0
// ��� Desc Ϊ True�����ؽ������
function CompareInt(V1, V2: Integer; Desc: Boolean = False): Integer;
begin
  if V1 > V2 then
    Result := 1
  else if V1 < V2 then
    Result := -1
  else // V1 = V2
    Result := 0;
  if Desc then
    Result := -Result;
end;

// ���������0..255֮��
function IntToByte(Value: Integer): Byte; overload;
asm
        OR     EAX, EAX
        JNS    @@Positive
        XOR    EAX, EAX
        RET

@@Positive:
        CMP    EAX, 255
        JBE    @@OK
        MOV    EAX, 255
@@OK:
end;

// ��TRect��������ꡢ���
procedure DeRect(Rect: TRect; var x, y, Width, Height: Integer);
begin
  x := Rect.Left;
  y := Rect.Top;
  Width := Rect.Right - Rect.Left;
  Height := Rect.Bottom - Rect.Top;
end;

// �Ƚ�����Rect
function RectEqu(Rect1, Rect2: TRect): Boolean;
begin
  Result := (Rect1.Left = Rect2.Left) and (Rect1.Top = Rect2.Top) and
    (Rect1.Right = Rect2.Right) and (Rect1.Bottom = Rect2.Bottom);
end;

// ����TSize����
function EnSize(cx, cy: Integer): TSize;
begin
  Result.cx := cx;
  Result.cy := cy;
end;

// ����Rect�Ŀ��
function RectWidth(Rect: TRect): Integer;
begin
  Result := Rect.Right - Rect.Left;
end;

// ����Rect�ĸ߶�
function RectHeight(Rect: TRect): Integer;
begin
  Result := Rect.Bottom - Rect.Top;
end;

// �жϷ�Χ
function InBound(Value: Integer; V1, V2: Integer): Boolean;
begin
  Result := (Value >= Min(V1, V2)) and (Value <= Max(V1, V2));
end;

// �Ƚ�����������ַ�Ƿ����
function SameMethod(Method1, Method2: TMethod): Boolean;
begin
  Result := CompareMem(@Method1, @Method2, SizeOf(TMethod));
end;

// ���ַ����б��в���
function HalfFind(List: TList; P: Pointer; SCompare: TListSortCompare): Integer;
var
  L, R, M: Integer;
  Res: Integer;
begin
  Result := -1;
  L := 0;
  R := List.Count - 1;
  if R < L then Exit;
  if SCompare(P, List[L]) < 0 then Exit;
  if SCompare(P, List[R]) > 0 then Exit;
  while True do
  begin
    M := (L + R) shr 1;
    Res := SCompare(P, List[M]);
    if Res > 0 then
      L := M
    else if Res < 0 then
      R := M
    else
    begin
      Result := M;
      Exit;
    end;
    if L = R then
      Exit
    else if R - L = 1 then
    begin
      if SCompare(P, List[L]) = 0 then
        Result := L
      else if SCompare(P, List[R]) = 0 then
        Result := R;
      Exit;
    end;
  end;
end;

// ���ַ��������б��в��ң�֧���ظ���¼������һ����Χֵ
function HalfFindEx(List: TList; P: Pointer; SCompare: TListSortCompare): TFindRange;
var
  i, Idx: Integer;
begin
  Idx := HalfFind(List, P, SCompare);
  Result.tgFirst := Idx;
  for i := Idx - 1 downto 0 do
    if SCompare(P, List[i]) = 0 then
      Result.tgFirst := i
    else
      Break;
  Result.tgLast := Idx;
  for i := Idx + 1 to List.Count - 1 do
    if SCompare(P, List[i]) = 0 then
      Result.tgLast := i
    else
      Break;
end;

// ����й���½�� 18 λ���֤�Ƿ�Ϸ�
function CheckChineseIDCardNumber(const IDNumber: string): Boolean;
const
  IDFactors: array[1..17] of Integer = (7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2);
  Remains: array[0..10] of Char = ('1', '0', 'X', '9', '8', '7', '6', '5', '4', '3', '2');
var
  I, Sum: Integer;
begin
  Result := False;
  if Length(IDNumber) <> 18 then
    Exit;

  Sum := 0;
  for I := 1 to 17 do
  begin
    if not (IDNumber[I] in ['0'..'9']) then
      Exit;

    Sum := Sum + (Ord(IDNumber[I]) - 48) * IDFactors[I];
  end;
  Sum := Sum mod 11;
  if Remains[Sum] = UpperCase(IDNumber[18]) then
    Result := True;
end;

// ����������
procedure CnSwap(var A, B: Byte); overload;
var
  Tmp: Byte;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure CnSwap(var A, B: Integer); overload;
var
  Tmp: Integer;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure CnSwap(var A, B: Single); overload;
var
  Tmp: Single;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

procedure CnSwap(var A, B: Double); overload;
var
  Tmp: Double;
begin
  Tmp := A;
  A := B;
  B := Tmp;
end;

// ��ʱ
procedure Delay(const uDelay: DWORD);
var
  n: DWORD;
begin
  n := GetTickCount;
  while GetTickCount - n <= uDelay do
    Application.ProcessMessages;
end;

// ��ָ���ڴ�������ָ����ʽ�����������
procedure SetClipboardContent(Format: Word; var Buffer; Size: Integer);
var
  Data: THandle;
  DataPtr: Pointer;
begin
  OpenClipboard(0);
  try
    Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Size);
    try
      DataPtr := GlobalLock(Data);
      try
        Move(Buffer, DataPtr^, Size);
        EmptyClipboard;
        SetClipboardData(Format, Data);
      finally
        GlobalUnlock(Data);
      end;
    except
      GlobalFree(Data);
      raise;
    end;
  finally
    CloseClipboard;
  end;
end;

{$IFNDEF WIN64}

// ��Win9X�������ȷ���
procedure BeepEx(const Freq: WORD = 1200; const Delay: WORD = 1);
const
  FREQ_SCALE = $1193180;
var
  Temp: WORD;
begin
  Temp := FREQ_SCALE div Freq;
  asm
    in al,61h;
    or al,3;
    out 61h,al;
    mov al,$b6;
    out 43h,al;
    mov ax,temp;
    out 42h,al;
    mov al,ah;
    out 42h,al;
  end;
  Sleep(Delay);
  asm
    in al,$61;
    and al,$fc;
    out $61,al;
  end;
end;

{$ENDIF}

function GetLastErrorMsg(IncludeErrorCode: Boolean): string;
var
  ErrNo: Integer;
  Buf: array[0..255] of Char;
begin
  ErrNo := GetLastError;
  FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrNo, $400, Buf, 255, nil);
  if Buf = '' then StrCopy(@Buf, PChar(SUnknowError));
  Result := Buf;
  if IncludeErrorCode then
    Result := Result + #10#13 + SErrorCode + IntToStr(ErrNo);
end;

// ��ʾWin32 Api���н����Ϣ
procedure ShowLastError;
begin
  MessageBox(Application.Handle, PChar(GetLastErrorMsg),
    PChar(SCnInformation), MB_OK + MB_ICONINFORMATION);
end;


{$IFDEF UNICODE}
// ȡ���ֵ�ƴ��������Ϊ Utf16
function GetHzPyW(const AHzStr: string): string;
begin
  Result := string(GetHzPy(AnsiString(AHzStr)));
end;
{$ENDIF}

// ȡ���ֵ�ƴ��
function GetHzPy(const AHzStr: AnsiString): AnsiString;
const
  ChinaCode: array[0..25, 0..1] of Integer = ((1601, 1636), (1637, 1832), (1833, 2077),
    (2078, 2273), (2274, 2301), (2302, 2432), (2433, 2593), (2594, 2786), (9999, 0000),
    (2787, 3105), (3106, 3211), (3212, 3471), (3472, 3634), (3635, 3722), (3723, 3729),
    (3730, 3857), (3858, 4026), (4027, 4085), (4086, 4389), (4390, 4557), (9999, 0000),
    (9999, 0000), (4558, 4683), (4684, 4924), (4925, 5248), (5249, 5589));
var
  i, j, HzOrd: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(AHzStr) do
  begin
    if (AHzStr[i] >= #160) and (AHzStr[i + 1] >= #160) then
    begin
      HzOrd := (Ord(AHzStr[i]) - 160) * 100 + Ord(AHzStr[i + 1]) - 160;
      for j := 0 to 25 do
      begin
        if (HzOrd >= ChinaCode[j][0]) and (HzOrd <= ChinaCode[j][1]) then
        begin
          Result := Result + AnsiChar(Byte('A') + j);
          Break;
        end;
      end;
      Inc(i);
    end else Result := Result + AHzStr[i];
    Inc(i);
  end;
end;

// ȫ���ַ�ת��Ϊ����ַ������о��"��"תΪ"."���ٺ�"��"תΪ","
function TextFullWidthToHalfWidth(const Text: string): string;
var
  s: string;
  s1, s2: WideString;
  l: Integer;
begin
  // ���ľ�źͶٺŲ����Զ��滻Ϊ . �ţ���Ҫ���д���
  s := StringReplace(Text, '��', '.', [rfReplaceAll]);
  s := StringReplace(s, '��', ',', [rfReplaceAll]);
  s1 := s;
  l := Length(s1);
  SetLength(s2, l);
  LCMapStringW(GetThreadLocale, LCMAP_HALFWIDTH, PWideChar(s1), l, PWideChar(s2), l);
  Result := s2;
end;

// ����ַ�ת��Ϊȫ���ַ�
function TextHalfWidthToFullWidth(const Text: string): string;
var
  s1, s2: WideString;
  l: Integer;
begin
  s1 := Text;
  l := Length(s1);
  SetLength(s2, l);
  LCMapStringW(GetThreadLocale, LCMAP_FULLWIDTH, PWideChar(s1), l, PWideChar(s2), l);
  Result := s2;
end;

// ���CustomEditѡ�е��ַ��������Դ���XP���ϵ�ϵͳ
function GetSelText(edt: TCustomEdit): string;
var
  Ver: TDLLVERSIONINFO2;
  iSelStart, Len: Integer;
  i, j, itemp: Integer;
  stext: string;
begin
  Assert(Assigned(edt));
  Result := edt.SelText;
  if not DllGetVersion('comctl32.dll', Ver) then
    Exit;
  if Ver.info1.dwMajorVersion <= 5 then
    Exit;
  with edt do
  begin
    Result := '';
    if SelLength <= 0 then
      Exit;

    stext := edt.Text;
    iSelStart := 0;
    i := 0;
    j := 1;
    itemp := SelStart;
    while i < itemp do
    begin
      if ByteType(stext, j) <> mbLeadByte then
        Inc(i);
      Inc(iSelStart);
      Inc(j);
    end;
    Len := SelLength;
    i := 0;
    j := 1;
    while i < Len do
    begin
      Result := Result + stext[iSelStart + j];
      if ByteType(stext, iSelStart + j) <> mbLeadByte then
        Inc(i);
      Inc(j);
    end;
  end;
end;

// ɾ�����к�ÿһ�е�����β�ո�
procedure TrimStrings(AList: TStrings);
var
  i: Integer;
begin
  for i := AList.Count - 1 downto 0 do
  begin
    AList[i] := Trim(AList[i]);
    if AList[i] = '' then
      AList.Delete(i);
  end;
end;

// �����Ƿ����
function SoundCardExist: Boolean;
begin
  Result := WaveOutGetNumDevs > 0;
end;

// �ж� ASrc �Ƿ�����������Ϊ AClass ����
function InheritsFromClassName(ASrc: TClass; const AClass: string): Boolean;
begin
  Result := False;
  while ASrc <> nil do
  begin
    if ASrc.ClassNameIs(AClass) then
    begin
      Result := True;
      Exit;
    end;
    ASrc := ASrc.ClassParent;
  end;
end;

// �ж� AObject �Ƿ�����������Ϊ AClass ����
function InheritsFromClassName(AObject: TObject; const AClass: string): Boolean;
begin
  Result := InheritsFromClassName(AObject.ClassType, AClass);
end;  

// ��������Ȩ�޵�SeDebug��ȡ����Ȩ��
function AdjustDebugPrivilege(Enable: Boolean): Boolean;
var
  Token: THandle;
  
  function InternalEnablePrivilege(Token: Cardinal; PrivName: string; Enable: Boolean): Boolean;
  var
    TP: TOKEN_PRIVILEGES;
    Dummy: Cardinal;
  begin
    TP.PrivilegeCount := 1;
    LookupPrivilegeValue(nil, PChar(PrivName), TP.Privileges[0].Luid);
    if Enable then
      TP.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED
    else
      TP.Privileges[0].Attributes := 0;

    AdjustTokenPrivileges(Token, False, TP, SizeOf(TP), nil, Dummy);
    Result := GetLastError = ERROR_SUCCESS;
  end;

begin
  OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, Token);
  Result := InternalEnablePrivilege(Token, 'SeDebugPrivilege', Enable);
  CloseHandle(Token);
end;

{$IFNDEF WIN64}

// ���� PEB �ṹ�������ж��������Ƿ񱻵�����
function PEBIsDebugged: Boolean;
asm
        MOV     EAX, FS:[$30];
        MOV     AL, BYTE PTR DS:[EAX + 02];
end;

{$ENDIF}

// ʹ�� NtQueryInformationProcess ���ж��������Ƿ񱻵�����
function NtIsDeugged: Boolean;
var
  DebugPort: DWORD;
begin
  Result := False;
  if not Assigned(NtQueryInformationProcess) then
  begin
    if (Win32Platform = VER_PLATFORM_WIN32_NT)
      and (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1) then
    begin
      if NtDllHandle = 0 then
      begin
        NtDllHandle := LoadLibrary('NTDLL.DLL');
        if NtDllHandle = 0 then
          raise Exception.Create('Can NOT Load NTDLL.');
      end;

      NtQueryInformationProcess := GetProcAddress(NtDllHandle, 'NtQueryInformationProcess');
    end
    else
      raise Exception.Create('Current Windows NOT Support.');

    if not Assigned(NtQueryInformationProcess) then
      raise Exception.Create('NtQueryInformationProcess NOT Found in NTDLL.');

    if 0 <> NtQueryInformationProcess(GetCurrentProcess, 7, @DebugPort, SizeOf(DebugPort), nil) then
      raise Exception.Create('Call NtQueryInformationProcess Failed.');

    Result := DebugPort <> 0;
  end;
end;

// �����ļ����������̣�������·��
procedure KillProcessByFileName(const FileName: String);
var
  ID:DWORD;
  S, Tmp: string;
  Ret: Boolean;
  SnapshotHandle: THandle;
  PE32: TProcessEntry32;
  hh: HWND;
begin
  S := LowerCase(FileName);
  SnapshotHandle := CreateToolHelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  PE32.dwSize := SizeOf(PE32);
  Ret := Process32First(SnapshotHandle, PE32);
  while Integer(Ret) <> 0 do
  begin
    Tmp := LowerCase(PE32.szExeFile);
    if Pos(S, Tmp) > 0 then
    begin
      Id := PE32.th32ProcessID;
      hh := OpenProcess(PROCESS_ALL_ACCESS, True,Id);
      TerminateProcess(hh, 0);
    end;
    Ret := Process32Next(SnapshotHandle,PE32);
  end;
end;

// ��ü���������Ϣ
function GetPropInfoIncludeSub(Instance: TObject; const PropName: string;
  AKinds: TTypeKinds): PPropInfo;
var
  AObject: TObject;
  Dot: Integer;
  RestProp: String;
begin
  Dot := Pos('.', PropName);
  if Dot = 0 then
  begin
    Result := GetPropInfo(Instance, PropName, AKinds);
  end
  else
  begin
    if GetPropInfo(Instance, Copy(PropName, 1, Dot - 1)) <> nil then
    begin
      AObject := GetObjectProp(Instance, Copy(PropName, 1, Dot - 1));
      if AObject = nil then
        Result :=  nil
      else
      begin
        RestProp := Copy(PropName, Dot + 1, Length(PropName) - Dot);
        Result := GetPropInfoIncludeSub(AObject, RestProp, AKinds);
      end;
    end
    else
      Result := nil;
  end;
end;

// ��ü�������ֵ
function GetPropValueIncludeSub(Instance: TObject; PropName: string;
  PreferStrings: Boolean = True): Variant;
const
  SCnControlFont = '!Font';
var
  AObject: TObject;
  Dot: Integer;
  RestProp: String;
  IntToId: TIntToIdent;
  IdValue: String;
  PropInfo: PPropInfo;
begin
  Result := Null;
  if Instance = nil then Exit;

  Dot := Pos('.', PropName);
  if Dot = 0 then
  begin
    if (Instance is TStrings) and (PropName = 'Text') then
    begin
      Result := (Instance as TStrings).Text;
      Exit;
    end
    else if (Instance is TListItem) and (PropName = 'Caption') then
    begin
      Result := (Instance as TListItem).Caption;
      Exit;
    end
    else if (Instance is TTreeNode) and (PropName = 'Text') then
    begin
      Result := (Instance as TTreeNode).Text;
      Exit;
    end
    else if PropName = SCnControlFont then // �ڴ��ڲ����� !Font �����
    begin
      PropName := 'Font';
      PropInfo := GetPropInfo(Instance, PropName);
      if PropInfo = nil then
        Exit;

      if PropInfo^.PropType^.Kind = tkClass then
      begin
        try
          Result := FontToString(TFont(GetObjectProp(Instance, PropName)));
        except
          ;
        end;
        Exit;
      end;
    end;

    PropInfo := GetPropInfo(Instance, PropName);
    if PropInfo = nil then
      Exit;

    if PropInfo^.PropType^.Kind = tkClass then
    begin
      AObject := GetObjectProp(Instance, PropName);
      // PreferStrings ʱ���ȷ��������
      if PreferStrings and (AObject <> nil) and (AObject is TComponent) then
        Result := (AObject as TComponent).Name
      else
        Result := Integer(AObject);
      Exit;
    end;

    Result := GetPropValue(Instance, PropName, PreferStrings);
    if (Result <> Null) and IsInt(Result) then   // ����������������Խ���ת���ɳ�����
    begin
      if PropInfo^.PropType^.Kind = tkInteger then
      begin
        IntToId := FindIntToIdent(PPropInfo(PropInfo)^.PropType^);
        if Assigned(IntToId) and IntToId(Result, IdValue) then
          Result := IdValue;
      end
    end
  end
  else
  begin
    // �ݹ�Ѱ��
    AObject := nil;
    if GetPropInfo(Instance, Copy(PropName, 1, Dot - 1)) <> nil then
      AObject := GetObjectProp(Instance, Copy(PropName, 1, Dot - 1));

    if AObject = nil then
      Result :=  Null
    else
    begin
      RestProp := Copy(PropName, Dot + 1, Length(PropName) - Dot);
      Result := GetPropValueIncludeSub(AObject, RestProp, PreferStrings);
    end;
  end;
end;

// ���ü�������ֵ���������쳣
procedure DoSetPropValueIncludeSub(Instance: TObject; const PropName: string;
  Value: Variant; AOwner: TComponent = nil);
var
  AObject: TObject;
  Dot, IntValue: Integer;
  RestProp: string;
  PropInfo: PPropInfo;
  IdToInt: TIdentToInt;
  S, AName: string;
  AComp: TComponent;
begin
  Dot := Pos('.', PropName);
  if Dot = 0 then
  begin
    PropInfo := GetPropInfo(Instance, PropName);
    if PropInfo^.PropType^.Kind = tkInteger then
    begin
      IdToInt := FindIdentToInt(PPropInfo(PropInfo)^.PropType^);
      if Assigned(IdToInt) and IdToInt(Value, IntValue) then
        SetPropValue(Instance, PropName, IntValue)
      else
      begin
        S := VarToStr(Value);
        if (S <> '') and (Length(S) > 1) and (S[1] = '$') then // ��ʮ������
        begin
          if IsInt(S) then
          begin
            SetPropValue(Instance, PropName, StrToInt(S));
            Exit;
          end;
        end;
        SetPropValue(Instance, PropName, Value)
      end;
    end
    else
    begin
      if (PropInfo^.PropType^.Kind in [tkSet, tkEnumeration]) and
        (VarType(Value) <> varInteger) then
      begin
        Value := Trim(Value);
        SetPropValue(Instance, PropName, Value);
      end
      else if PropInfo^.PropType^.Kind = tkClass then
      begin
        AName := VarToStr(Value);
        AComp := nil;
        if (AOwner <> nil) and (AName <> '') then
          AComp := AOwner.FindComponent(AName);
        SetObjectProp(Instance, PropName, AComp);
      end
      else
        SetPropValue(Instance, PropName, Value);
    end
  end
  else
  begin
    // �ݹ�����
    AObject := GetObjectProp(Instance, Copy(PropName, 1, Dot - 1));
    RestProp := Copy(PropName, Dot + 1, Length(PropName) - Dot);
    DoSetPropValueIncludeSub(AObject, RestProp, Value);
  end;
end;

// ���ü�������ֵ
function SetPropValueIncludeSub(Instance: TObject; const PropName: string;
  const Value: Variant; AOwner: TComponent = nil): Boolean;
begin
  try
    DoSetPropValueIncludeSub(Instance, PropName, Value, AOwner);
    Result := True;
  except
    Result := False;
  end;
end;

// �ַ���ת����ֵ
function StrToSetValue(const Value: string; PInfo: PTypeInfo): Integer;
var
  EnumInfo: PTypeInfo;
  EnumValue: 0..SizeOf(Integer) * 8 - 1;
  S: string;
  Strings: TStrings;
  i: Integer;
begin
  Result := 0;
  S := Trim(Value);
  if S = '' then Exit;
  if S[1] = '[' then
    Delete(S, 1, 1);
  if S = '' then Exit;
  if S[Length(S)] = ']' then
    Delete(S, Length(S), 1);
  EnumInfo := GetTypeData(PInfo).CompType^;
  Strings := TStringList.Create;
  try
    Strings.CommaText := S;
    for i := 0 to Strings.Count - 1 do
    begin
      EnumValue := GetEnumValue(EnumInfo, Trim(Strings[i]));
      if (EnumValue < GetTypeData(EnumInfo)^.MinValue) or
        (EnumValue > GetTypeData(EnumInfo)^.MaxValue) then
        Exit;                       // ������Ч��ö��ֵ
      Include(TIntegerSet(Result), EnumValue);
    end;
  finally
    Strings.Free;
  end;
end;

function PropInfoName(PropInfo: PPropInfo): string;
begin
  Result := string(PropInfo^.Name);
end;

function TypeInfoName(TypeInfo: PTypeInfo): string;
begin
  Result := string(TypeInfo^.Name);
end;

// ���ĳ������������Ե��ַ���ֵ�����������Ե�����
// IncludeType Ϊ True ʱ����ʽΪ Name=TypeName��Object�з��� PropType
procedure GetAllPropNames(AComp: TObject; PropNames: TStrings;
  const BaseName: string; IncludeType: Boolean);
var
  I, APropCount: Integer;
  PropListPtr: PPropList;
  PropInfo: PPropInfo;
  AObj: TObject;
begin
  if PropNames = nil then
    Exit;
    
  APropCount := GetTypeData(PTypeInfo(AComp.ClassInfo))^.PropCount;
  if APropCount > 0 then
  begin
    GetMem(PropListPtr, APropCount * SizeOf(Pointer));
    GetPropList(PTypeInfo(AComp.ClassInfo), tkAny, PropListPtr);
    
    try
      for I := 0 to APropCount - 1 do
      begin
        PropInfo := PropListPtr^[I];
        if PropInfo^.Name = '' then
          Continue;
      
        if PropInfo^.PropType^^.Kind in (tkProperties + tkMethods) then
        begin
          if BaseName = '' then
          begin
            if not IncludeType then
              PropNames.Add(PropInfoName(PropInfo))
            else
              PropNames.AddObject(PropInfoName(PropInfo) + '=' +
                TypeInfoName(PropInfo^.PropType^), TObject(PropInfo^.PropType^^.Kind))
          end
          else
          begin
            if not IncludeType then
              PropNames.Add(BaseName + '.' + PropInfoName(PropInfo))
            else
              PropNames.AddObject(BaseName + '.' + PropInfoName(PropInfo) + '=' +
                TypeInfoName(PropInfo^.PropType^), TObject(PropInfo^.PropType^^.Kind))
          end;

          if PropInfo^.PropType^^.Kind = tkClass then
          begin
            AObj := GetObjectProp(AComp, PropInfo);
            if (AObj <> nil) then // ��ֻ�������������ԡ��������Լ����������
              if not (AObj is TComponent) or ((AObj as TComponent).Owner = AComp) then
                GetAllPropNames(AObj, PropNames, PropInfoName(PropInfo), IncludeType);
          end;
        end;
      end;
    finally
      FreeMem(PropListPtr);
    end;
  end;
end;

// �ж�ĳ Control �� ParentFont �����Ƿ�Ϊ True������ Parent �򷵻� False
function IsParentFont(AControl: TControl): Boolean;
begin
  try
    Result := not (AControl.Parent = nil);
    if Result then
      Result := TCnFontControl(AControl).ParentFont;
  except
    Result := False;
  end;
end;

// ȡĳ Control �� Parent �� Font ���ԣ����û�з��� nil
function GetParentFont(AControl: TComponent): TFont;
begin
  Result := nil;
  try
    if AControl <> nil then
    begin
      if AControl is TControl then
      begin
        if TControl(AControl).Parent <> nil then
          Result := TCnFontControl(TControl(AControl).Parent).Font;
      end
      else if AControl is TComponent then
      begin
        if (AControl.Owner <> nil) and (AControl.Owner is TControl) then
          Result := TCnFontControl(AControl.Owner).Font;
      end;
    end;
  except
    ;
  end;
end;

//�����ַ����ڶ�̬�����е�����������string����ʹ��Case���
function IndexStr(const AText: string; AValues: array of string; IgCase: Boolean = True): Integer;
type
  TSameFunc = function(const S1, S2: string): Boolean;
var
  Index: Integer;
  SameFunc: TSameFunc;
begin
  Result := -1;
  if IgCase then
    SameFunc := AnsiSameText
  else
    SameFunc := AnsiSameStr;

  for Index := Low(AValues) to High(AValues) do
    if SameFunc(AValues[Index], AText) then
    begin
      Result := Index;
      Exit;
    end;
end;

// �������α����ڶ�̬�����е����������ڱ���ʹ��Case���
function IndexInt(ANum: Integer; AValues: array of Integer): Integer;
var
  Index: Integer;
begin
  Result := -1;
  for Index := Low(AValues) to High(AValues) do
    if ANum = AValues[Index] then
    begin
      Result := Index;
      Exit;
    end;
end;

initialization
  WndLong := GetWindowLong(Application.Handle, GWL_EXSTYLE);
  InitSetLayeredWindowAttributesFunc;

finalization
  if NtDllHandle <> 0 then
    FreeLibrary(NtDllHandle);

end.


