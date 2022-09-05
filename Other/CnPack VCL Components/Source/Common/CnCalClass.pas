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

unit CnCalClass;
{* |<PRE>
================================================================================
* ������ƣ�������������
* ��Ԫ���ƣ������������
* ��Ԫ���ߣ���Х (liuxiao@cnpack.org)
* ��    ע��
* ����ƽ̨��PWinXP SP2 + Delphi 5
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2007.11.15 V1.2
*               ���Ӷ�ʮ�����յ�����
*           2006.09.15 V1.0
*               ����ũ�����µ�����
*           2006.08.13 V1.0
*               ������Ԫ
================================================================================
|</PRE>}

interface

uses
  SysUtils, Classes, Windows, Math, CnCalendar;

type

 { Calendar Interfaces }

  ICnYearIntf = interface
    ['{7910FC7C-8B79-46B6-BAFE-558EE338FAB1}']
    function GetShengXiao: Integer;
    function GetYear: Integer;
    function GetYearGan: Integer;
    function GetYearZhi: Integer;
    function GetYearGanZhi: Integer;
    function GetRuMeiMonth: Integer;
    function GetRuMeiDay: Integer;
    function GetChuMeiMonth: Integer;
    function GetChuMeiDay: Integer;
    procedure SetYear(const Value: Integer);
    procedure SetDateTime(const ADateTime: TDateTime);

    property Year: Integer read GetYear write SetYear;
    {* ������� }
    property YearGan: Integer read GetYearGan;
    {* ����ɣ�0-9 ��Ӧ �׵��� }
    property YearZhi: Integer read GetYearZhi;
    {* ���֧��0-11 ��Ӧ �ӵ��� }
    property YearGanZhi: Integer read GetYearGanZhi;
    {* ����ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ }
    property ShengXiao: Integer read GetShengXiao;
    {* ��Ф��0-11 ��Ӧ ���� }
    property RuMeiMonth: Integer read GetRuMeiMonth;
    {* ��÷�յ��·� }
    property RuMeiDay: Integer read GetRuMeiDay;
    {* ��÷�յ����� }
    property ChuMeiMonth: Integer read GetChuMeiMonth;
    {* ��÷�յ��·� }
    property ChuMeiDay: Integer read GetChuMeiDay;
    {* ��÷�յ����� }
  end;

  ICnMonthIntf = interface(ICnYearIntf)
    ['{18547CBA-0751-4524-BC4B-FF733F10FAB2}']
    function GetMonth: Integer;
    function GetMonthGan: Integer;
    function GetMonthGanZhi: Integer;
    function GetMonthZhi: Integer;
    procedure SetMonth(const Value: Integer);

    procedure SetYearMonth(const AYear, AMonth: Integer);
    {* һ�������ù�������}
    property Month: Integer read GetMonth write SetMonth;
    {* �����·� }
    property MonthGan: Integer read GetMonthGan;
    {* ����ɣ�0-9 ��Ӧ �׵��� }
    property MonthZhi: Integer read GetMonthZhi;
    {* �µ�֧��0-11 ��Ӧ �ӵ��� }
    property MonthGanZhi: Integer read GetMonthGanZhi;
    {* ����ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ }
  end;

  ICnDayIntf = interface(ICnMonthIntf)
    ['{85DBCE15-7281-4A58-BF19-5D29A5F318D6}']
    function GetDay: Integer;
    function GetWeek: Integer;
    function GetDayGan: Integer;
    function GetDayGanZhi: Integer;
    function GetDayZhi: Integer;
    function GetXingZuo: Integer;
    function GetDay28Xiu: Integer;
    function GetIsInJiu: Boolean;
    function GetJieQi: Integer;
    function GetJiu: Integer;
    function GetJiuDay: Integer;
    function GetIsInFu: Boolean;
    function GetFu: Integer;
    function GetFuDay: Integer;
    function GetLunarMonth: Integer;
    function GetLunarDay: Integer;
    function GetIsLeapMonth: Boolean;
    procedure SetDay(const Value: Integer);

    procedure SetYearMonthDay(const AYear, AMonth, ADay: Integer);
    {* һ�������ù���������}
    property Day: Integer read GetDay write SetDay;
    {* �������� }
    property Week: Integer read GetWeek;
    {* ���ڣ�0-6 ��Ӧ �յ��� }
    property DayGan: Integer read GetDayGan;
    {* ����ɣ�0-9 ��Ӧ �׵��� }
    property DayZhi: Integer read GetDayZhi;
    {* �յ�֧��0-11 ��Ӧ �ӵ��� }
    property DayGanZhi: Integer read GetDayGanZhi;
    {* ����ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ }
    property XingZuo: Integer read GetXingZuo;
    {* ������0-11 ��Ӧ ����˫�� }
    property Day28Xiu: Integer read GetDay28Xiu;
    {* �ն�ʮ����, 0-27 ��Ӧ �ǵ���}
    property JieQi: Integer read GetJieQi;
    {* �����Ǳ����ʲô������0-23����Ӧ�������󺮣�����Ϊ -1}
    property IsInJiu: Boolean read GetIsInJiu;
    {* �����Ƿ����������� }
    property Jiu: Integer read GetJiu;
    {* �������еĵڼ��ţ�1~9 ��Ӧһ�ŵ��žţ���������������Ϊ -1 }
    property JiuDay: Integer read GetJiuDay;
    {* �������иþŵĵڼ��գ�1~9��Ӧһ�ŵ��žţ���������������Ϊ -1 }
    property IsInFu: Boolean read GetIsInFu;
    {* �����Ƿ����������� }
    property Fu: Integer read GetFu;
    {* �������еĵڼ�����1~3 ��Ӧ������ĩ������������������Ϊ -1 }
    property FuDay: Integer read GetFuDay;
    {* �������з��еĵڼ��գ�1~10 ��Ӧ���ڵ�һ�յ����ڵ�ʮ�գ���������������Ϊ -1 }
    property LunarMonth: Integer read GetLunarMonth;
    {* ũ����}
    property LunarDay: Integer read GetLunarDay;
    {* ũ����}
    property IsLeapMonth: Boolean read GetIsLeapMonth;
    {* ũ�����Ƿ�����}
  end;

  ICnHourIntf = interface(ICnMonthIntf)
    ['{113BBE61-71B3-407A-948D-62699D15E2BA}']
    function GetHour: Integer;
    function GetHourGan: Integer;
    function GetHourZhi: Integer;
    function GetHourGanZhi: Integer;
    procedure SetHour(const Value: Integer);

    procedure SetYearMonthDayHour(const AYear, AMonth, ADay, AHour: Integer);
    {* һ�������ù���������ʱ}
    property Hour: Integer read GetHour write SetHour;
    {* Сʱ��24 ʱ�� }
    property HourGan: Integer read GetHourGan;
    {* ʱ��ɣ�0-9 ��Ӧ �׵��� }
    property HourZhi: Integer read GetHourZhi;
    {* ʱ��֧��Ҳ��ʱ����0-11 ��Ӧ �ӵ��� }
    property HourGanZhi: Integer read GetHourGanZhi;
    {* ʱ��ɵ�֧��0-59 ��Ӧ ���ӵ��ﺥ }
  end;

  { Calendar Classes }

{$M+}
  TCnYearObj = class(TInterfacedObject, ICnYearIntf)
  private
    FYear: Integer;
    FYearGan: Integer;
    FYearZhi: Integer;
    FRuMeiMonth: Integer;
    FRuMeiDay: Integer;
    FChuMeiMonth: Integer;
    FChuMeiDay: Integer;
  protected
    procedure Update; virtual;
  public
    constructor Create; virtual;

    function GetShengXiao: Integer;
    function GetYear: Integer;
    function GetYearGan: Integer;
    function GetYearZhi: Integer;
    function GetYearGanZhi: Integer;
    function GetRuMeiMonth: Integer;
    function GetRuMeiDay: Integer;
    function GetChuMeiMonth: Integer;
    function GetChuMeiDay: Integer;
    procedure SetYear(const Value: Integer);
    procedure SetDateTime(const ADateTime: TDateTime); virtual;
  published
    property Year: Integer read GetYear write SetYear;
    property YearGan: Integer read GetYearGan;
    property YearZhi: Integer read GetYearZhi;
    property YearGanZhi: Integer read GetYearGanZhi;
    property ShengXiao: Integer read GetShengXiao;
    property RuMeiMonth: Integer read GetRuMeiMonth;
    property RuMeiDay: Integer read GetRuMeiDay;
    property ChuMeiMonth: Integer read GetChuMeiMonth;
    property ChuMeiDay: Integer read GetChuMeiDay;    
  end;
{$M-}

  TCnMonthObj = class(TCnYearObj, ICnMonthIntf)
  private
    FMonth: Integer;
    FMonthGan: Integer;
    FMonthZhi: Integer;
  protected
    procedure Update; override;
  public
    constructor Create; override;

    function GetMonth: Integer;
    function GetMonthGan: Integer;
    function GetMonthGanZhi: Integer;
    function GetMonthZhi: Integer;
    procedure SetMonth(const Value: Integer);
    procedure SetYearMonth(const AYear, AMonth: Integer);
    procedure SetDateTime(const ADateTime: TDateTime); override;
  published
    property Month: Integer read GetMonth write SetMonth;
    property MonthGan: Integer read GetMonthGan;
    property MonthZhi: Integer read GetMonthZhi;
    property MonthGanZhi: Integer read GetMonthGanZhi;
  end;

  TCnDayObj = class(TCnMonthObj, ICnDayIntf)
  private
    FDay: Integer;
    FWeek: Integer;
    FDayGan: Integer;
    FDayZhi: Integer;
    FXingZuo: Integer;
    FDay28Xiu: Integer;
    FIsInJiu: Boolean;
    FJiu: Integer;
    FJiuDay: Integer;
    FIsInFu: Boolean;
    FFu: Integer;
    FFuDay: Integer;
    FLunarYear: Integer;
    FLunarMonth: Integer;
    FLunarDay: Integer;
    FIsLeapMonth: Boolean;
    function GetLunarYear: Integer;
  protected
    procedure Update; override;
  public
    constructor Create; override;

    function GetDay: Integer;
    function GetWeek: Integer;
    function GetDayGan: Integer;
    function GetDayGanZhi: Integer;
    function GetDayZhi: Integer;
    function GetXingZuo: Integer;
    function GetDay28Xiu: Integer;
    function GetJieQi: Integer;
    function GetIsInJiu: Boolean;
    function GetJiu: Integer;
    function GetJiuDay: Integer;
    function GetIsInFu: Boolean;
    function GetFu: Integer;
    function GetFuDay: Integer;
    function GetLunarMonth: Integer;
    function GetLunarDay: Integer;
    function GetIsLeapMonth: Boolean;
    procedure SetDay(const Value: Integer);
    procedure SetYearMonthDay(const AYear, AMonth, ADay: Integer);
    procedure SetDateTime(const ADateTime: TDateTime); override;
  published
    property Day: Integer read GetDay write SetDay;
    property Week: Integer read GetWeek;
    property DayGan: Integer read GetDayGan;
    property DayZhi: Integer read GetDayZhi;
    property DayGanZhi: Integer read GetDayGanZhi;
    property XingZuo: Integer read GetXingZuo;
    property Day28Xiu: Integer read GetDay28Xiu;
    property JieQi: Integer read GetJieQi;
    property IsInJiu: Boolean read GetIsInJiu;
    property Jiu: Integer read GetJiu;
    property JiuDay: Integer read GetJiuDay;
    property IsInFu: Boolean read GetIsInFu;
    property Fu: Integer read GetFu;
    property FuDay: Integer read GetFuDay;
    property LunarYear: Integer read GetLunarYear;
    property LunarMonth: Integer read GetLunarMonth;
    property LunarDay: Integer read GetLunarDay;
    property IsLeapMonth: Boolean read GetIsLeapMonth;
  end;

  TCnHourObj = class(TCnDayObj, ICnHourIntf)
  private
    FHour: Integer;
    FHourGan: Integer;
    FHourZhi: Integer;
  protected
    procedure Update; override;    
  public
    constructor Create; override;

    function GetHour: Integer;
    function GetHourGan: Integer;
    function GetHourZhi: Integer;
    function GetHourGanZhi: Integer;
    procedure SetHour(const Value: Integer);
    procedure SetYearMonthDayHour(const AYear, AMonth, ADay, AHour: Integer);    
    procedure SetDateTime(const ADateTime: TDateTime); override;
  published
    property Hour: Integer read GetHour write SetHour;
    property HourGan: Integer read GetHourGan;
    property HourZhi: Integer read GetHourZhi;
    property HourGanZhi: Integer read GetHourGanZhi;
  end;


implementation

{ TCnYearObj }

constructor TCnYearObj.Create;
begin
  SetDateTime(Now);
end;

function TCnYearObj.GetChuMeiDay: Integer;
begin
  Result := FChuMeiDay;
end;

function TCnYearObj.GetChuMeiMonth: Integer;
begin
  Result := FChuMeiMonth;
end;

function TCnYearObj.GetRuMeiDay: Integer;
begin
  Result := FRuMeiDay;
end;

function TCnYearObj.GetRuMeiMonth: Integer;
begin
  Result := FRuMeiMonth;
end;

function TCnYearObj.GetShengXiao: Integer;
begin
  Result := FYearZhi;
end;

function TCnYearObj.GetYear: Integer;
begin
  Result := FYear;
end;

function TCnYearObj.GetYearGan: Integer;
begin
  Result := FYearGan;
end;

function TCnYearObj.GetYearGanZhi: Integer;
begin
  Result := CombineGanZhi(FYearGan, FYearZhi);;
end;

function TCnYearObj.GetYearZhi: Integer;
begin
  Result := FYearZhi;
end;

procedure TCnYearObj.SetYear(const Value: Integer);
begin
  ValidDate(Value, 1, 1);
  FYear := Value;
  Update;
end;

procedure TCnYearObj.SetDateTime(const ADateTime: TDateTime);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADateTime, AYear, AMonth, ADay);
  SetYear(AYear);
end;

procedure TCnYearObj.Update;
begin
  ExtractGanZhi(GetGanZhiFromYear(FYear), FYearGan, FYearZhi);
  CnCalendar.GetRuMeiDay(FYear, FRuMeiMonth, FRuMeiDay);
  CnCalendar.GetChuMeiDay(FYear, FChuMeiMonth, FChuMeiDay);
end;

{ TCnMonthObj }

constructor TCnMonthObj.Create;
begin
  inherited;
end;

function TCnMonthObj.GetMonth: Integer;
begin
  Result := FMonth;
end;

function TCnMonthObj.GetMonthGan: Integer;
begin
  Result := FMonthGan;
end;

function TCnMonthObj.GetMonthGanZhi: Integer;
begin
  Result := CombineGanZhi(FMonthGan, FMonthZhi);
end;

function TCnMonthObj.GetMonthZhi: Integer;
begin
  Result := FMonthZhi;
end;

procedure TCnMonthObj.SetMonth(const Value: Integer);
begin
  ValidDate(Year, Value, 1);
  FMonth := Value;
  Update;
end;

procedure TCnMonthObj.SetYearMonth(const AYear, AMonth: Integer);
begin
  ValidDate(AYear, AMonth, 1);
  FYear := AYear;
  FMonth := AMonth;
  Update;
end;

procedure TCnMonthObj.SetDateTime(const ADateTime: TDateTime);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADateTime, AYear, AMonth, ADay);
  SetYearMonth(AYear, AMonth);
end;

procedure TCnMonthObj.Update;
begin
  inherited;
  // MonthObj ��������Ϣ������ 15 ��Ϊ׼����ȡ��֧��������ﵽ�����ڡ�
  ExtractGanZhi(GetGanZhiFromMonth(Year, Month, 15), FMonthGan, FMonthZhi);
end;

{ TCnDayObj }

constructor TCnDayObj.Create;
begin
  inherited;
end;

function TCnDayObj.GetDay: Integer;
begin
  Result := FDay;
end;

function TCnDayObj.GetDay28Xiu: Integer;
begin
  Result := FDay28Xiu;
end;

function TCnDayObj.GetDayGan: Integer;
begin
  Result := FDayGan;
end;

function TCnDayObj.GetDayGanZhi: Integer;
begin
  Result := CombineGanZhi(FDayGan, FDayZhi);
end;

function TCnDayObj.GetDayZhi: Integer;
begin
  Result := FDayZhi;
end;

function TCnDayObj.GetFu: Integer;
begin
  Result := FFu;
end;

function TCnDayObj.GetFuDay: Integer;
begin
  Result := FFuDay;
end;

function TCnDayObj.GetIsInFu: Boolean;
begin
  Result := FIsInFu;
end;

function TCnDayObj.GetIsInJiu: Boolean;
begin
  Result := FIsInJiu;
end;

function TCnDayObj.GetIsLeapMonth: Boolean;
begin
  Result := FIsLeapMonth;
end;

function TCnDayObj.GetJieQi: Integer;
begin
  Result := GetJieQiFromDay(Year, Month, Day);
end;

function TCnDayObj.GetJiu: Integer;
begin
  Result := FJiu;
end;

function TCnDayObj.GetJiuDay: Integer;
begin
  Result := FJiuDay;
end;

function TCnDayObj.GetLunarDay: Integer;
begin
  Result := FLunarDay;
end;

function TCnDayObj.GetLunarMonth: Integer;
begin
  Result := FLunarMonth;
end;

function TCnDayObj.GetLunarYear: Integer;
begin
  Result := FLunarYear;
end;

function TCnDayObj.GetWeek: Integer;
begin
  Result := FWeek;
end;

function TCnDayObj.GetXingZuo: Integer;
begin
  Result := FXingZuo;
end;

procedure TCnDayObj.SetDay(const Value: Integer);
begin
  ValidDate(Year, Month, Value);
  FDay := Value;
  Update;
end;

procedure TCnDayObj.SetYearMonthDay(const AYear, AMonth, ADay: Integer);
begin
  ValidDate(AYear, AMonth, ADay);
  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;
  Update;
end;

procedure TCnDayObj.SetDateTime(const ADateTime: TDateTime);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADateTime, AYear, AMonth, ADay);
  SetYearMonthDay(AYear, AMonth, ADay);
end;

procedure TCnDayObj.Update;
begin
  inherited;
  FWeek := CnCalendar.GetWeek(Year, Month, Day);
  // �����ڣ�����������Ϊ�����һ�����¸�֧
  ExtractGanZhi(GetGanZhiFromYear(Year, Month, Day), FYearGan, FYearZhi);
  ExtractGanZhi(GetGanZhiFromMonth(Year, Month, Day), FMonthGan, FMonthZhi);

  ExtractGanZhi(GetGanZhiFromDay(Year, Month, Day), FDayGan, FDayZhi);
  FXingZuo := GetXingZuoFromMonthDay(Month, Day);
  FDay28Xiu := Get28XiuFromDay(Year, Month, Day);
  FIsInJiu := GetShu9Day(Year, Month, Day, FJiu, FJiuDay);
  FIsInFu := Get3FuDay(Year, Month, Day, FFu, FFuDay);

  GetLunarFromDay(Year, Month, Day, FLunarYear, FLunarMonth, FLunarDay, FIsLeapMonth);
end;

{ TCnHourObj }

constructor TCnHourObj.Create;
begin
  inherited;
end;

function TCnHourObj.GetHour: Integer;
begin
  Result := FHour;
end;

function TCnHourObj.GetHourGan: Integer;
begin
  Result := FHourGan;
end;

function TCnHourObj.GetHourGanZhi: Integer;
begin
  Result := CombineGanZhi(FHourGan, FHourZhi);
end;

function TCnHourObj.GetHourZhi: Integer;
begin
  Result := FHourZhi;
end;

procedure TCnHourObj.SetDateTime(const ADateTime: TDateTime);
var
  AYear, AMonth, ADay: Word;
  AHour, AMin, ASec, AMSec: Word;
begin
  DecodeDate(ADateTime, AYear, AMonth, ADay);
  DecodeTime(Now, AHour, AMin, ASec, AMSec);
  SetYearMonthDayHour(AYear, AMonth, ADay, AHour);
end;

procedure TCnHourObj.SetHour(const Value: Integer);
begin
  ValidTime(Value, 1, 1);
  FHour := Value;
  Update;
end;

procedure TCnHourObj.SetYearMonthDayHour(const AYear, AMonth, ADay, AHour: Integer);
begin
  ValidDate(AYear, AMonth, ADay);
  ValidTime(AHour, 1, 1);
  FYear := AYear;
  FMonth := AMonth;
  FDay := ADay;
  FHour := AHour;  
  Update;
end;

procedure TCnHourObj.Update;
begin
  inherited;
  ExtractGanZhi(GetGanZhiFromHour(Year, Month, Day, Hour), FHourGan, FHourZhi);
end;

end.
