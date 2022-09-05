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

unit CnWinampCtrl;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ�Winamp���������TCnWinampCtrl��Ԫ
* ��Ԫ���ߣ�С�� (kendling@21cn.com)
* ��    ע��- ���Կ���Winamp��һ��СС�ؼ���
*           - �����ñ��ؼ�дһ�������Ͽ�����ȫ����Winamp�������
*           - �����ñ��ؼ�����һ����ʱ༭����
* ����ƽ̨��PWin2000 + Delphi 6.0 Update Pack 2
* ���ݲ��ԣ�PWin2000 + Delphi 6.0 Update Pack 2
* �� �� �����õ�Ԫ��û���ַ�����Դ
* �޸ļ�¼��
*           2005.03.08 v1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface
    
{$I CnPack.inc}

uses
//------------------------------------------------------------------------------
// System
//------------------------------------------------------------------------------
  SysUtils, Classes, Messages, Windows, ShellAPI,
//------------------------------------------------------------------------------
// CnPack
//------------------------------------------------------------------------------
  CnCommon, CnClasses, CnConsts, CnCompConsts;

const
  WinampClassName = 'Winamp v1.x';

//------------------------------------------------------------------------------
// WA IPC
//------------------------------------------------------------------------------
  WM_WA_IPC             = WM_USER;
  IPC_GETVERSION        = 0;    
  IPC_PLAYFILE          = 100;    
  IPC_ENQUEUEFILE       = 100;
  IPC_DELETE            = 101;
  IPC_CHDIR             = 103;
  IPC_ISPLAYING         = 104;
  IPC_GETOUTPUTTIME     = 105; 
  IPC_JUMPTOTIME        = 106;
  IPC_WRITEPLAYLIST     = 120;
  IPC_SETPLAYLISTPOS    = 121;
  IPC_SETVOLUME         = 122; 
  IPC_SETPANNING        = 123; 
  IPC_GETLISTLENGTH     = 124;
  IPC_GETLISTPOS        = 125; 
  IPC_GETINFO           = 126;  
  IPC_GETEQDATA         = 127;
  IPC_SETEQDATA         = 128;   
  IPC_RESTARTWINAMP     = 135;
  IPC_CHANGECURRENTFILE = 245;
  IPC_GET_SHUFFLE       = 250;
  IPC_GET_REPEAT        = 251;
  IPC_SET_SHUFFLE       = 252;
  IPC_SET_REPEAT        = 253; 
  IPC_ENABLEDISABLE_ALL_WINDOWS = 259;
  IPC_GETWND            = 260;
    IPC_GETWND_EQ       = 0;
    IPC_GETWND_PE       = 1;
    IPC_GETWND_MB       = 2;
    IPC_GETWND_VIDEO    = 3;

  WINAMP_FILE_QUIT      = 40001;
  WINAMP_OPTIONS_EQ     = 40036;
  WINAMP_OPTIONS_PLEDIT = 40040;
  WINAMP_BUTTON1        = 40044;
  WINAMP_BUTTON2        = 40045;
  WINAMP_BUTTON3        = 40046;
  WINAMP_BUTTON4        = 40047;
  WINAMP_BUTTON5        = 40048;
  WINAMP_VOLUMEUP       = 40058;
  WINAMP_VOLUMEDOWN     = 40059;
  WINAMP_FFWD5S         = 40060;
  WINAMP_REW5S          = 40061;
  WINAMP_BUTTON1_SHIFT  = 40144;
  WINAMP_BUTTON2_SHIFT  = 40145;
  WINAMP_BUTTON3_SHIFT  = 40146;
  WINAMP_BUTTON4_SHIFT  = 40147;
  WINAMP_BUTTON5_SHIFT  = 40148;
  WINAMP_BUTTON1_CTRL   = 40154;
  WINAMP_BUTTON2_CTRL   = 40155;
  WINAMP_BUTTON3_CTRL   = 40156;
  WINAMP_BUTTON4_CTRL   = 40157;
  WINAMP_BUTTON5_CTRL   = 40158;
  IDC_SORT_FILENAME     = 40166;
  IDC_SORT_FILETITLE    = 40167;
  IDC_SORT_ENTIREFILENAME = 40168;     
  WINAMP_JUMP10FWD      = 40195;
  WINAMP_JUMP10BACK     = 40197; 
  WINAMP_MAIN_WINDOW    = 40258; 
  WINAMP_MINIMIZE       = 40334;
  
//------------------------------------------------------------------------------
// WA PE
//------------------------------------------------------------------------------   

type
  TEQDataSelect = (EQ60hz, EQ170hz, EQ310hz, EQ600hz, EQ1k, EQ3k,EQ6k, EQ12k,
    EQ14k, EQ16k, EQPreAmp, EQEnabled, EQAutoLoad);

//==============================================================================
// Winamp����������
//==============================================================================

{ TWinampControl }

  TCnWinampCtrl = class(TCnComponent)
  {* Winamp���������}
  private
    FAutoFind: Boolean;
    FAutoWritePlayList: Boolean;
    FStartDelay: Integer;
    FWAPath: string;
    FWndWinamp: HWND;     
    function GetEQData(const Index: TEQDataSelect): Byte;
    function GetIsFound: Boolean;
    function GetPlayListPos: Integer;
    function GetVolume: Byte;
    function GetVolBalance: Integer;
    function GetWACurrentTime: Integer;
    function GetWARepeat: Boolean;
    function GetWAShufle: Boolean;
    function GetWAState: Integer;
    function SendMessageToWinamp(Msg: Cardinal; wParam: WPARAM; lParam:
            LPARAM): Integer;
    procedure SetEnabledWAWindow(const Value: Boolean);
    procedure SetEQData(const Index: TEQDataSelect; const Value: Byte);
    procedure SetPlayListPos(const Value: Integer);
    procedure SetVolume(const Value: Byte);
    procedure SetVolBalance(const Value: Integer);
    procedure SetWACurrentTime(const Value: Integer);
    procedure SetWARepeat(const Value: Boolean);
    procedure SetWAShufle(const Value: Boolean);
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    {* �๹����}
    destructor Destroy; override;
    {* ��������}
    procedure AddDir(const strPath: string);
    {* ���Ŀ¼�������б����}
    procedure AddFile(const strPath: string);
    {* ����ļ��������б����}
    procedure ClearPlayList;
    {* ��������б�}
    procedure CloseWinamp;
    {* �ر�Winamp}
    function FindWinamp: Boolean;
    {* ����Winamp���ھ��}
    procedure FFW_5sec;
    {* ��ǰ5��}
    function GetInfo(const iMode: Integer): Integer;
    {* ��ȡ��ǰ������Ϣ  iModeΪ 0:������ 1:������ 2:ͨ�� 3:��Ƶ LOWORD=w HIWORD=h 4:> 65536, string (��Ƶ����)}
    function GetPlayList: string;
    {* ��ȡWinamp��ǰ�����б�  ���ظ�ʽΪTString.CommaText}
    procedure GetPlayListCount;
    {* ��ȡ������Ŀ����}
    function GetTimeLength: Integer;
    {* ��ȡ��ǰ������ʱ��}
    function GetVersion: string;
    {* ��ȡWinamp�汾}
    procedure JUMP10BACK;
    {* ���10��}
    procedure JUMP10FWD;
    {* ��ǰ10��}
    procedure NextTack;
    {* ��һ�׸���}
    procedure Pause;
    {* ��ͣ����}
    procedure Play;
    {* ��ʼ����}
    procedure PlayIndex(const Index: Integer);
    {* �����б��е�һ�׸�}
    procedure PlayListSortInFileName;
    {* �����б����ļ�������}
    procedure PlayListSortInFilePath;
    {* �����б���·�����ļ�������}
    procedure PlayListSortInTitle;
    {* �����б��Ա�������}
    procedure PrevTrack;
    {* ��һ�׸���}
    procedure RestartWinamp;
    {* ��������Winamp}
    procedure REW_5sec;
    {* ���5��}
    function StartWinamp(const strWAPath: string=''): Boolean;
    {* ����Winamp}
    procedure Stop;
    {* ֹͣ����}
    procedure VolumeDown;
    {* ����һ������ ������2.95���ã�5.08������}
    procedure VolumeUp;
    {* ����һ������ ������2.95���ã�5.08������}
    procedure WritePlayList;
    {* ���浱ǰ�����б�Winamp�����Ŀ¼}
    property EnabledWAWindow: Boolean write SetEnabledWAWindow;
    {* ����/����Winamp���д��ڣ����������Ч��}
    property EQData[const Index: TEQDataSelect]: Byte read GetEQData write
        SetEQData; default;
    {* EQ���� IndexΪ: EQPreAmp, EQ60hz, EQ170hz, EQ310hz, EQ600hz, EQ1k, EQ3k,
    EQ6k, EQ12k, EQ14k, EQ16k, EQEnabled, EQAutoLoad  EQPreAmp..EQ16k:0-63
    EQEnabled/EQAutoLoadΪ0ʱ�رգ���0ʱ������}
    property IsFound: Boolean read GetIsFound default False;
    {* ��ǰ�Ƿ��Ѿ��ҵ�Winamp���ھ��}
    property PlayListPos: Integer read GetPlayListPos write SetPlayListPos;
    {* �����б���ѡ��λ��}    
    property Volume: Byte read GetVolume write SetVolume;
    {* Winamp���� 0 - 255}
    property VolBalance: Integer read GetVolBalance write SetVolBalance;
    {* ����ƽ�� -127 - 127}
    property WACurrentTime: Integer read GetWACurrentTime write SetWACurrentTime;
    {* �������ŵĵ�ǰʱ�� ��λms} 
    property WARepeat: Boolean read GetWARepeat write SetWARepeat;
    {* ��ȡ/����ѭ������}
    property WAShufle: Boolean read GetWAShufle write SetWAShufle;
    {* ��ȡ/�����������}
    property WAState: Integer read GetWAState;
    {* ��ȡWinamp��ǰ״̬  ����ֵ��0 Ϊֹͣ  1 Ϊ���ڲ���  3 Ϊ��ͣ}
  published
    property AutoFind: Boolean read FAutoFind write FAutoFind default False;
    {* �Զ�����Winamp���ھ��}
    property AutoWritePlayList: Boolean read FAutoWritePlayList write
        FAutoWritePlayList default False;
    {* �Զ����沥���б�}
    property StartDelay: Integer read FStartDelay write FStartDelay default 3000;
    {* �ȴ�Winamp��������ʱ}
    property WAPath: string read FWAPath write FWAPath;
    {* Winamp����·�� ��: C:\Program Files\Winamp\Winamp.exe}
  end;

implementation
          
//==============================================================================
// Winamp����������
//==============================================================================

constructor TCnWinampCtrl.Create(AOwner: TComponent);
begin
  inherited;
  FStartDelay := 3000;
  if FAutoFind then FindWinamp;
end;

destructor TCnWinampCtrl.Destroy;
begin
  inherited;
end;

procedure TCnWinampCtrl.AddDir(const strPath: string);
var
  PPath: PChar;
  cds : COPYDATASTRUCT;
begin                
  PPath := PChar(strPath);
  cds.dwData := IPC_CHDIR;
  cds.lpData := PPath;
  cds.cbData := SysUtils.StrLen(PAnsiChar(cds.lpData))+1; // include space for null char
  SendMessageToWinamp(WM_COPYDATA, WPARAM(0), LPARAM(@cds));
  if FAutoFind then WritePlayList;
end;

procedure TCnWinampCtrl.AddFile(const strPath: string);
var
  PPath: PChar;
  cds : COPYDATASTRUCT;
begin
  PPath := PChar(strPath);
  cds.dwData := IPC_PLAYFILE;
  cds.lpData := PPath;
  cds.cbData := SysUtils.StrLen(PAnsiChar(cds.lpData))+1; // include space for null char
  SendMessageToWinamp(WM_COPYDATA, WPARAM(0), LPARAM(@cds));
  if FAutoFind then WritePlayList;
end;

procedure TCnWinampCtrl.ClearPlayList;
begin
  SendMessageToWinamp(WM_WA_IPC, 0, IPC_DELETE);
  if FAutoFind then WritePlayList;
end;

procedure TCnWinampCtrl.CloseWinamp;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_FILE_QUIT ,0);
  FWndWinamp := 0;
end;

function TCnWinampCtrl.FindWinamp: Boolean;
begin
  FWndWinamp := FindWindow(WinampClassName, nil);
  Result := IsFound;
end;

procedure TCnWinampCtrl.FFW_5sec;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_FFWD5S, 0);
end;

procedure TCnWinampCtrl.GetComponentInfo(var AName, Author, Email, Comment:
    string);
begin
  AName := SCnWinampCtrlName;
  Author := SCnPack_Kendling;
  Email := SCnPack_KendlingEmail;
  Comment := SCnWinampCtrlComment;
end;

function TCnWinampCtrl.GetEQData(const Index: TEQDataSelect): Byte;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, Ord(Index), IPC_GETEQDATA);
end;

function TCnWinampCtrl.GetInfo(const iMode: Integer): Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, iMode, IPC_GETINFO);
end;

function TCnWinampCtrl.GetPlayList: string;
var
  slPlayList, slCPlayList: TStrings;
  i, j: Integer;
begin
  {����Ƿ�������Winamp·��}
  Result := '';
  if not FileExists(FWAPath) then Exit;
  if FAutoFind then WritePlayList;
  {��ȡ��ǰ�����б�}
  slPlayList := TStringList.Create;
  slCPlayList := TStringList.Create;
  slPlayList.LoadFromFile(_CnChangeFileExt(FWAPath, '.m3u'));
  if UpperCase(slPlayList[0]) = '#EXTM3U' then
  begin
    i := 1;
    while i < slPlayList.Count do
    begin
      if UpperCase(Copy(slPlayList[i], 1, 7)) = '#EXTINF' then
      begin
        j := AnsiPos(',', slPlayList[i]);
        slCPlayList.Add(Copy(slPlayList[i], j+1, 256));
        Inc(i);
      end else
      begin
        slCPlayList.Add(_CnExtractFileName(slPlayList[i]));
      end;
      Inc(i);
    end;
  {��������б�}
    Result := slCPlayList.CommaText;
  end;
  slPlayList.Free;
  slCPlayList.Free;
end;

function TCnWinampCtrl.GetIsFound: Boolean;
begin
  Result := FWndWinamp <> 0; //INVALID_HANDLE_VALUE [DWord(-1)]; 2005.3.7 QQCAT
end;

procedure TCnWinampCtrl.GetPlayListCount;
begin
  SendMessageToWinamp(WM_WA_IPC, 0, IPC_GETLISTLENGTH);
end;

function TCnWinampCtrl.GetPlayListPos: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_GETLISTPOS);
end;

function TCnWinampCtrl.GetTimeLength: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 1, IPC_GETOUTPUTTIME);
end;

function TCnWinampCtrl.GetVersion: string;
begin
  Result := IntToHex(SendMessageToWinamp(WM_WA_IPC, 0, IPC_GETVERSION), 2);
  if Result = '00' then
  begin
    Result := '0';
    Exit;
  end;
  if Result[1] = '2' then Result[3] := Result[2];
  if Result[1] = '1' then Result[3] := Result[2];
    Result[2] := '.';
end;

function TCnWinampCtrl.GetVolume: Byte;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, WPARAM(-666), IPC_SETVOLUME);
end;

function TCnWinampCtrl.GetVolBalance: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, WPARAM(-666), IPC_SETPANNING);
end;

function TCnWinampCtrl.GetWACurrentTime: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_GETOUTPUTTIME);
end;

function TCnWinampCtrl.GetWARepeat: Boolean;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_GET_REPEAT)>0;
end;

function TCnWinampCtrl.GetWAShufle: Boolean;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_GET_SHUFFLE)>0;
end;

function TCnWinampCtrl.GetWAState: Integer;
begin
  Result := SendMessageToWinamp(WM_WA_IPC, 0, IPC_ISPLAYING);
end;

procedure TCnWinampCtrl.JUMP10BACK;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_JUMP10BACK, 0);
end;

procedure TCnWinampCtrl.JUMP10FWD;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_JUMP10FWD, 0);
end;

procedure TCnWinampCtrl.NextTack;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON5, 0);
end;

procedure TCnWinampCtrl.Pause;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON3, 0);
end;

procedure TCnWinampCtrl.Play;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON2, 0);
end;

procedure TCnWinampCtrl.PlayIndex(const Index: Integer);
begin
  SendMessageToWinamp(WM_WA_IPC, WPARAM(Index), IPC_CHANGECURRENTFILE);
end;

procedure TCnWinampCtrl.PlayListSortInFileName;
begin
  SendMessageToWinamp(WM_COMMAND, IDC_SORT_FILENAME, 0);
end;

procedure TCnWinampCtrl.PlayListSortInFilePath;
begin
  SendMessageToWinamp(WM_COMMAND, IDC_SORT_ENTIREFILENAME, 0);
end;

procedure TCnWinampCtrl.PlayListSortInTitle;
begin
  SendMessageToWinamp(WM_COMMAND, IDC_SORT_FILETITLE, 0);
end;

procedure TCnWinampCtrl.PrevTrack;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON1, 0);
end;

procedure TCnWinampCtrl.RestartWinamp;
begin
  SendMessageToWinamp(WM_WA_IPC, 0, IPC_RESTARTWINAMP);
end;

procedure TCnWinampCtrl.REW_5sec;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_REW5S, 0);
end;

function TCnWinampCtrl.SendMessageToWinamp(Msg: Cardinal; wParam: WPARAM;
        lParam: LPARAM): Integer;
begin
  Result := 0;
  if not IsFound then Exit;
  Result := SendMessage(FWndWinamp, Msg, wParam, lParam);
end;

procedure TCnWinampCtrl.SetEnabledWAWindow(const Value: Boolean);
begin
  if Value then
    SendMessageToWinamp(WM_WA_IPC, 0, IPC_ENABLEDISABLE_ALL_WINDOWS)
  else
    SendMessageToWinamp(WM_WA_IPC, WPARAM($DEADBEEF), IPC_ENABLEDISABLE_ALL_WINDOWS);
end;

procedure TCnWinampCtrl.SetEQData(const Index: TEQDataSelect; const Value:
    Byte);
begin
  SendMessageToWinamp(WM_WA_IPC, Ord(Index), IPC_GETEQDATA);
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_SETEQDATA);
end;

procedure TCnWinampCtrl.SetPlayListPos(const Value: Integer);
begin
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_SETPLAYLISTPOS)
end;

procedure TCnWinampCtrl.SetVolume(const Value: Byte);
begin
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_SETVOLUME);
end;

procedure TCnWinampCtrl.SetVolBalance(const Value: Integer);
begin
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_SETPANNING);
end;

procedure TCnWinampCtrl.SetWACurrentTime(const Value: Integer);
begin
  SendMessageToWinamp(WM_WA_IPC, Value, IPC_JUMPTOTIME);
end;

procedure TCnWinampCtrl.SetWARepeat(const Value: Boolean);
begin
  SendMessageToWinamp(WM_WA_IPC, Integer(Value), IPC_SET_REPEAT);
end;

procedure TCnWinampCtrl.SetWAShufle(const Value: Boolean);
begin
  SendMessageToWinamp(WM_WA_IPC, Integer(Value), IPC_SET_SHUFFLE);
end;

function TCnWinampCtrl.StartWinamp(const strWAPath: string=''): Boolean;
begin
  Result := False;
  if strWAPath <> '' then FWAPath := strWAPath;
  if not FileExists(FWAPath) then Exit;
  if ShellExecute(0, '', PChar(FWAPath), PChar('/CLASS="'+WinAmpClassName+'"'),
    PChar(_CnExtractFilePath(FWAPath)), SW_MINIMIZE) > 32 then
    Result := True;
  if FAutoFind then
  begin
    Sleep(FStartDelay);
    FindWinamp;
  end;
end;

procedure TCnWinampCtrl.Stop;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_BUTTON4, 0);
end;

procedure TCnWinampCtrl.VolumeDown;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_VOLUMEDOWN, 0);
end;

procedure TCnWinampCtrl.VolumeUp;
begin
  SendMessageToWinamp(WM_COMMAND, WINAMP_VOLUMEUP, 0);
end;

procedure TCnWinampCtrl.WritePlayList;
begin
  SendMessageToWinamp(WM_WA_IPC, 0, IPC_WRITEPLAYLIST);
end;

end.
