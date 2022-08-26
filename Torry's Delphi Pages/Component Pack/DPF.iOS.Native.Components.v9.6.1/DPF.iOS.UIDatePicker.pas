// ------------------------------------------------------------------------------
// DPF.iOS.UIDatePicker Component
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
// Email #3: bayaghoobi@gmail.com
//
// ------------------------------------------------------------------------------
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ------------------------------------------------------------------------------
unit DPF.iOS.UIDatePicker;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  System.TypInfo,
  DPF.iOS.BaseControl,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  FMX.Platform.iOS,
  DPF.iOS.Common,
{$ELSE}
  // Added by Fenistil
  DPF.iOS.DesignTime,
{$ENDIF}
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

const
  TDPFLocaleTypeStr: array [0 .. 532] of string = ( 'en_IE', 'ro_MD', 'br', 'en_GY', 'es_GT', 'shi_Tfng_MA', 'mr', 'bs', 'en_AS', 'ksf', 'sr_Latn_ME', 'ms', 'ms_MY', 'mt', 'ha', 'nb_NO', 'en_BZ', 'pt_BR', 'or_IN', 'is_IS', 'mn_Cyrl_MN', 'ar_IQ', 'he', 'zh_Hans_CN', 'my', 'bas', 'mer', 'en_JM', 'dz_BT', 'teo_KE', 'cy_GB', 'sg', 'it_CH', 'de_LU', 'en_US',
    'hi', 'hu_HU', 'uz_Latn_UZ', 'af_NA', 'si', 'fr_BI', 'ga_IE', 'mfe', 'en_CA', 'ne_IN', 'rwk_TZ', 'en_AU', 'sk', 'teo', 'sl', 'tk_Latn_TM', 'tzm_Latn', 'ee_GH', 'kde', 'sn', 'dyo_SN', 'mas_TZ', 'en_SG', 'so', 'nyn_UG', 'br_FR', 'fr_BJ', 'pt_MZ', 'hr', 'az_Latn', 'sq', 'sr', 'sw_KE', 'ca', 'hu', 'et_EE', 'lag_TZ', 'nb', 'bn_IN', 'sv', 'th_TH', 'ml_IN',
    'sr_Cyrl_RS', 'sw', 'nd', 'ta_IN', 'fr_MQ', 'hy', 'ne', 'es_AR', 'pt_AO', 'ne_NP', 'ar_BH', 'hi_IN', 'bo_IN', 'seh', 'de_DE', 'fr_BL', 'fr_MR', 'fa_IR', 'nl', 'es_PR', 'en_PW', 'rn_BI', 'nn', 'kk_Cyrl', 'sl_SI', 'dua', 'kea', 'ig_NG', 'kln', 'yo', 'sv_FI', 'ru_MD', 'en_ZW', 'brx_IN', 'fil_PH', 'cs', 'pt_GW', 'bn_BD', 'de_AT', 'luo', 'sk_SK', 'ar_001',
    'es_US', 'ta', 'mk_MK', 'om_KE', 'da_DK', 'ko_KR', 'shi_Latn', 'ff_SN', 'id', 'sr_Cyrl_ME', 'kde_TZ', 'cy', 'mgh', 'te', 'fr_GN', 'fo_FO', 'ig', 'it_IT', 'uk_UA', 'tg', 'vai', 'bm_ML', 'en_SL', 'ii', 'ses', 'th', 'ti', 'ru_KZ', 'te_IN', 'tk', 'cs_CZ', 'ar_AE', 'brx', 'haw', 'tzm_Latn_MA', 'so_DJ', 'uz_Cyrl_UZ', 'to', 'ewo_CM', 'nl_AW', 'ar_MR', 'sn_ZW',
    'en_IN', 'en_TT', 'tr', 'is', 'fr_GP', 'luy', 'es_NI', 'it', 'da', 'kln_KE', 'tk_Latn', 'en_BB', 'ar_DZ', 'ar_SY', 'ha_Latn', 'en_MH', 'mr_IN', 'en_GB', 'de', 'fr_GQ', 'ky_KG', 'pt_PT', 'fr_RW', 'nus_SD', 'asa', 'zh', 'ha_Latn_GH', 'bo_CN', 'kam_KE', 'dua_CM', 'khq_ML', 'ur_IN', 'ro_RO', 'om', 'ksb_TZ', 'gu_IN', 'fr_TD', 'jmc', 'ja_JP', 'so_ET', 'nl_NL',
    'es_ES', 'or', 'yo_NG', 'es_PY', 'mua_CM', 'fa_AF', 'en_HK', 'luo_KE', 'ja', 'twq', 'en_BE', 'es_UY', 'dje_NE', 'luy_KE', 'naq', 'si_LK', 'zu', 'bs_BA', 'zh_Hans_MO', 'fr_KM', 'zh_Hant_HK', 'dz', 'swc', 'asa_TZ', 'az_Cyrl', 'ewo', 'gv_GB', 'ti_ER', 'be_BY', 'uk', 'nyn', 'cgg_UG', 'de_CH', 'fr_TG', 'jmc_TZ', 'ta_LK', 'so_SO', 'es_DO', 'fr_LU',
    'shi_Latn_MA', 'swc_CD', 'kn_IN', 'hy_AM', 'fil', 'bas_CM', 'ar_TD', 'ur', 'bez_TZ', 'haw_US', 'tg_Cyrl', 'pa', 'ee_TG', 'ti_ET', 'sr_Latn_BA', 'ee', 'sv_SE', 'ki_KE', 'zh_Hans', 'bem', 'uz', 'ar_YE', 'seh_MZ', 'ru_UA', 'fr_SC', 'ar_KM', 'en_ZA', 'nn_NO', 'mas_KE', 'ar_EG', 'el', 'pl', 'nl_BE', 'en', 'uz_Latn', 'eo', 'shi', 'kok', 'mas', 'fr_FR', 'rof',
    'en_MP', 'de_BE', 'ar_EH', 'es_CL', 'en_VI', 'es', 'ps', 'et', 'vai_Latn', 'pt', 'eu', 'ka', 'fr_NE', 'eu_ES', 'mgh_MZ', 'zu_ZA', 'ar_SA', 'chr_US', 'cgg', 'lag', 'az_Latn_AZ', 'es_VE', 'el_GR', 'el_CY', 'mfe_MU', 'ki', 'vi', 'rwk', 'bez', 'kk', 'kl', 'zh_Hant', 'fr_CA', 'km', 'es_HN', 'agq_CM', 'kn', 'ii_CN', 'mn_Cyrl', 'en_BM', 'ko', 'ln_CD', 'en_GM',
    'es_CO', 'guz_KE', 'es_PA', 'twq_NE', 'en_NZ', 'fa', 'en_US_POSIX', 'dav_KE', 'lt_LT', 'en_SZ', 'ar_SD', 'rof_TZ', 'uz_Arab_AF', 'vi_VN', 'en_MT', 'kw', 'yav_CM', 'ta_MY', 'ru_KG', 'kab', 'ky', 'ff', 'en_PG', 'to_TO', 'ar_LY', 'af_ZA', 'de_LI', 'sr_Cyrl_BA', 'fi', 'ksf_CM', 'khq', 'gsw', 'es_SV', 'fr_DJ', 'en_MU', 'sr_Latn', 'pl_PL', 'kea_CV', 'pa_Arab',
    'fr_MC', 'en_PH', 'saq', 'ar_PS', 'fr_CD', 'bem_ZM', 'ru_RU', 'uz_Cyrl', 'pa_Guru', 'vai_Vaii', 'fo', 'so_KE', 'ln_CG', 'ar_OM', 'pt_ST', 'kl_GL', 'fr', 'es_CR', 'ses_ML', 'tzm', 'mer_KE', 'xog', 'xog_UG', 'nl_SX', 'en_FJ', 'en_MW', 'ar_MA', 'kam', 'am_ET', 'af', 'ar_TN', 'es_PE', 'sbp_TZ', 'fr_CF', 'vun_TZ', 'lg', 'ar_JO', 'ebu', 'fr_RE', 'ha_Latn_NG',
    'lv_LV', 'ak', 'chr', 'az_Cyrl_AZ', 'dav', 'es_419', 'ebu_KE', 'fr_MF', 'am', 'en_PK', 'fr_CG', 'dje', 'dyo', 'pa_Guru_IN', 'ln', 'ak_GH', 'ar_DJ', 'en_BS', 'lo', 'zh_Hant_TW', 'lg_UG', 'ar_KW', 'ar', 'fr_MG', 'ca_ES', 'as', 'he_IL', 'sbp', 'fr_GA', 'mg_MG', 'my_MM', 'ps_AF', 'fr_CH', 'vun', 'lt', 'kk_Cyrl_KZ', 'ga', 'en_FM', 'lu', 'nmg', 'es_BO', 'lv',
    'fr_YT', 'km_KH', 'teo_UG', 'fr_SN', 'om_ET', 'ms_BN', 'ar_ER', 'gsw_CH', 'az', 'fi_FI', 'tr_TR', 'fr_CI', 'en_UM', 'sr_Cyrl', 'ur_PK', 'hr_HR', 'nl_CW', 'nmg_CM', 'en_GU', 'es_EC', 'gl_ES', 'zh_Hant_MO', 'gl', 'mt_MT', 'ha_Latn_NE', 'en_NA', 'rm', 'kw_GB', 'zh_Hans_SG', 'rn', 'ro', 'rm_CH', 'saq_KE', 'vai_Vaii_LR', 'ka_GE', 'es_GQ', 'sr_Latn_RS',
    'zh_Hans_HK', 'agq', 'gu', 'lo_LA', 'ru', 'en_SB', 'gv', 'en_BW', 'yav', 'ta_SG', 'fr_BE', 'bg_BG', 'es_MX', 'rw', 'be', 'nd_ZW', 'mua', 'kab_DZ', 'bg', 'tg_Cyrl_TJ', 'mg', 'sg_CF', 'pa_Arab_PK', 'sw_TZ', 'en_SC', 'nus', 'shi_Tfng', 'ar_QA', 'naq_NA', 'fr_BF', 'rw_RW', 'as_IN', 'guz', 'ksb', 'fr_ML', 'mk', 'kok_IN', 'sq_AL', 'ml', 'fr_GF', 'bm', 'lu_CD',
    'fr_CM', 'bn', 'ar_LB', 'id_ID', 'uz_Arab', 'mn', 'bo', 'vai_Latn_LR' );

  TDPFCalendarTypeStr: array [0 .. 10] of string = ( 'NSGregorianCalendar', 'NSPersianCalendar', 'NSBuddhistCalendar', 'NSChineseCalendar', 'NSHebrewCalendar', 'NSIslamicCalendar', 'NSIslamicCivilCalendar', 'NSJapaneseCalendar', 'NSRepublicOfChinaCalendar', 'NSIndianCalendar', 'NSISO8601Calendar' );

type

  TDPFDatePicker = class;

  TDPFDatePickerMode = ( pmTime = 0, pmDate = 1, pmDateAndTime = 2, pmCountDownTimer = 3 );
  TDPFTimeZoneType   = ( tztDefault, tztSystem, tztLocal );

  TDPFLocaleType = ( en_IE_, ro_MD_, br_, en_GY_, es_GT_, shi_Tfng_MA_, mr_, bs_, en_AS_, ksf_, sr_Latn_ME_, ms_, ms_MY_, mt_, ha_, nb_NO_, en_BZ_, pt_BR_, or_IN_, is_IS_, mn_Cyrl_MN_, ar_IQ_, he_, zh_Hans_CN_, my_, bas_, mer_, en_JM_, dz_BT_, teo_KE_, cy_GB_, sg_, it_CH_, de_LU_, en_US_, hi_, hu_HU_, uz_Latn_UZ_, af_NA_, si_, fr_BI_, ga_IE_, mfe_, en_CA_,
    ne_IN_, rwk_TZ_, en_AU_, sk_, teo_, sl_, tk_Latn_TM_, tzm_Latn_, ee_GH_, kde_, sn_, dyo_SN_, mas_TZ_, en_SG_, so_, nyn_UG_, br_FR_, fr_BJ_, pt_MZ_, hr_, az_Latn_, sq_, sr_, sw_KE_, ca_, hu_, et_EE_, lag_TZ_, nb_, bn_IN_, sv_, th_TH_, ml_IN_, sr_Cyrl_RS_, sw_, nd_, ta_IN_, fr_MQ_, hy_, ne_, es_AR_, pt_AO_, ne_NP_, ar_BH_, hi_IN_, bo_IN_, seh_, de_DE_,
    fr_BL_, fr_MR_, fa_IR_, nl_, es_PR_, en_PW_, rn_BI_, nn_, kk_Cyrl_, sl_SI_, dua_, kea_, ig_NG_, kln_, yo_, sv_FI_, ru_MD_, en_ZW_, brx_IN_, fil_PH_, cs_, pt_GW_, bn_BD_, de_AT_, luo_, sk_SK_, ar_001_, es_US_, ta_, mk_MK_, om_KE_, da_DK_, ko_KR_, shi_Latn_, ff_SN_, id_, sr_Cyrl_ME_, kde_TZ_, cy_, mgh_, te_, fr_GN_, fo_FO_, ig_, it_IT_, uk_UA_, tg_, vai_,
    bm_ML_, en_SL_, ii_, ses_, th_, ti_, ru_KZ_, te_IN_, tk_, cs_CZ_, ar_AE_, brx_, haw_, tzm_Latn_MA_, so_DJ_, uz_Cyrl_UZ_, to_, ewo_CM_, nl_AW_, ar_MR_, sn_ZW_, en_IN_, en_TT_, tr_, is_, fr_GP_, luy_, es_NI_, it_, da_, kln_KE_, tk_Latn_, en_BB_, ar_DZ_, ar_SY_, ha_Latn_, en_MH_, mr_IN_, en_GB_, de_, fr_GQ_, ky_KG_, pt_PT_, fr_RW_, nus_SD_, asa_, zh_,
    ha_Latn_GH_, bo_CN_, kam_KE_, dua_CM_, khq_ML_, ur_IN_, ro_RO_, om_, ksb_TZ_, gu_IN_, fr_TD_, jmc_, ja_JP_, so_ET_, nl_NL_, es_ES_, or_, yo_NG_, es_PY_, mua_CM_, fa_AF_, en_HK_, luo_KE_, ja_, twq_, en_BE_, es_UY_, dje_NE_, luy_KE_, naq_, si_LK_, zu_, bs_BA_, zh_Hans_MO_, fr_KM_, zh_Hant_HK_, dz_, swc_, asa_TZ_, az_Cyrl_, ewo_, gv_GB_, ti_ER_, be_BY_,
    uk_, nyn_, cgg_UG_, de_CH_, fr_TG_, jmc_TZ_, ta_LK_, so_SO_, es_DO_, fr_LU_, shi_Latn_MA_, swc_CD_, kn_IN_, hy_AM_, fil_, bas_CM_, ar_TD_, ur_, bez_TZ_, haw_US_, tg_Cyrl_, pa_, ee_TG_, ti_ET_, sr_Latn_BA_, ee_, sv_SE_, ki_KE_, zh_Hans_, bem_, uz_, ar_YE_, seh_MZ_, ru_UA_, fr_SC_, ar_KM_, en_ZA_, nn_NO_, mas_KE_, ar_EG_, el_, pl_, nl_BE_, en_, uz_Latn_,
    eo_, shi_, kok_, mas_, fr_FR_, rof_, en_MP_, de_BE_, ar_EH_, es_CL_, en_VI_, es_, ps_, et_, vai_Latn_, pt_, eu_, ka_, fr_NE_, eu_ES_, mgh_MZ_, zu_ZA_, ar_SA_, chr_US_, cgg_, lag_, az_Latn_AZ_, es_VE_, el_GR_, el_CY_, mfe_MU_, ki_, vi_, rwk_, bez_, kk_, kl_, zh_Hant_, fr_CA_, km_, es_HN_, agq_CM_, kn_, ii_CN_, mn_Cyrl_, en_BM_, ko_, ln_CD_, en_GM_,
    es_CO_, guz_KE_, es_PA_, twq_NE_, en_NZ_, fa_, en_US_POSIX_, dav_KE_, lt_LT_, en_SZ_, ar_SD_, rof_TZ_, uz_Arab_AF_, vi_VN_, en_MT_, kw_, yav_CM_, ta_MY_, ru_KG_, kab_, ky_, ff_, en_PG_, to_TO_, ar_LY_, af_ZA_, de_LI_, sr_Cyrl_BA_, fi_, ksf_CM_, khq_, gsw_, es_SV_, fr_DJ_, en_MU_, sr_Latn_, pl_PL_, kea_CV_, pa_Arab_, fr_MC_, en_PH_, saq_, ar_PS_, fr_CD_,
    bem_ZM_, ru_RU_, uz_Cyrl_, pa_Guru_, vai_Vaii_, fo_, so_KE_, ln_CG_, ar_OM_, pt_ST_, kl_GL_, fr_, es_CR_, ses_ML_, tzm_, mer_KE_, xog_, xog_UG_, nl_SX_, en_FJ_, en_MW_, ar_MA_, kam_, am_ET_, af_, ar_TN_, es_PE_, sbp_TZ_, fr_CF_, vun_TZ_, lg_, ar_JO_, ebu_, fr_RE_, ha_Latn_NG_, lv_LV_, ak_, chr_, az_Cyrl_AZ_, dav_, es_419_, ebu_KE_, fr_MF_, am_, en_PK_,
    fr_CG_, dje_, dyo_, pa_Guru_IN_, ln_, ak_GH_, ar_DJ_, en_BS_, lo_, zh_Hant_TW_, lg_UG_, ar_KW_, ar_, fr_MG_, ca_ES_, as_, he_IL_, sbp_, fr_GA_, mg_MG_, my_MM_, ps_AF_, fr_CH_, vun_, lt_, kk_Cyrl_KZ_, ga_, en_FM_, lu_, nmg_, es_BO_, lv_, fr_YT_, km_KH_, teo_UG_, fr_SN_, om_ET_, ms_BN_, ar_ER_, gsw_CH_, az_, fi_FI_, tr_TR_, fr_CI_, en_UM_, sr_Cyrl_,
    ur_PK_, hr_HR_, nl_CW_, nmg_CM_, en_GU_, es_EC_, gl_ES_, zh_Hant_MO_, gl_, mt_MT_, ha_Latn_NE_, en_NA_, rm_, kw_GB_, zh_Hans_SG_, rn_, ro_, rm_CH_, saq_KE_, vai_Vaii_LR_, ka_GE_, es_GQ_, sr_Latn_RS_, zh_Hans_HK_, agq_, gu_, lo_LA_, ru_, en_SB_, gv_, en_BW_, yav_, ta_SG_, fr_BE_, bg_BG_, es_MX_, rw_, be_, nd_ZW_, mua_, kab_DZ_, bg_, tg_Cyrl_TJ_, mg_,
    sg_CF_, pa_Arab_PK_, sw_TZ_, en_SC_, nus_, shi_Tfng_, ar_QA_, naq_NA_, fr_BF_, rw_RW_, as_IN_, guz_, ksb_, fr_ML_, mk_, kok_IN_, sq_AL_, ml_, fr_GF_, bm_, lu_CD_, fr_CM_, bn_, ar_LB_, id_ID_, uz_Arab_, mn_, bo_, vai_Latn_LR_ );

  TDPFCalendarType = ( Gregorian, Persian, Buddhist, Chinese, Hebrew, Islamic, IslamicCivil, Japanese, RepublicOfChina, Indian, ISO8601 );
  // ------------------------------------------------------------------------------
{$IFDEF IOS}

  IDPFDatePickerDelegate = interface( IObjectiveC )
    ['{A443FBAB-2C9A-4197-8216-0953E73C9E96}']
    procedure didChanged( datePicker: UIDatePicker ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFDatePickerDelegate = class( TOCLocal, IDPFDatePickerDelegate )
  private
    FDPFDatePicker: TDPFDatePicker;
  public
    constructor Create( AOwner: TDPFDatePicker );
    procedure didChanged( datePicker: UIDatePicker ); cdecl;
  end;
{$ENDIF}
  // ------------------------------------------------------------------------------

  TMinuteInterval       = 1 .. 30;
  TDPFDatePickerOnChane = procedure( Sender: TObject ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFDatePicker = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FDPFDatePicker        : UIDatePicker;
    FDPFDatePickerDelegate: TDPFDatePickerDelegate;
{$ENDIF}
    FOnChange      : TNotifyEvent;
    FPickerMode    : TDPFDatePickerMode;
    FTimeZoneType  : TDPFTimeZoneType;
    FLocaleType    : TDPFLocaleType;
    FCalendarType  : TDPFCalendarType;
    FMaximumDate   : TDateTime;
    FMinimumDate   : TDateTime;
    FEnabled       : Boolean;
    FMinuteInterval: TMinuteInterval;
    procedure SetPickerDate( const Value: TDateTime );
    function GetPickerDate: TDateTime;
    procedure SetPickerMode( const Value: TDPFDatePickerMode );
    procedure SetCalendarType( const Value: TDPFCalendarType );
    procedure SetLocaleType( const Value: TDPFLocaleType );
    procedure SetTimeZoneType( const Value: TDPFTimeZoneType );
    procedure SetEnabled( const Value: Boolean );
    procedure SetMinuteInterval( const Value: TMinuteInterval );

  protected
    procedure Resize; override;
    procedure Move; override;
  public
{$IFDEF IOS}
    procedure Loaded; override;
{$ELSE}
    procedure Paint; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property PickerDate    : TDateTime read GetPickerDate write SetPickerDate;
    property PickerMode    : TDPFDatePickerMode read FPickerMode write SetPickerMode default TDPFDatePickerMode.pmDateAndTime;
    property TimeZoneType  : TDPFTimeZoneType read FTimeZoneType write SetTimeZoneType default TDPFTimeZoneType.tztDefault;
    property LocaleType    : TDPFLocaleType read FLocaleType write SetLocaleType default TDPFLocaleType.en_;
    property CalendarType  : TDPFCalendarType read FCalendarType write SetCalendarType default TDPFCalendarType.Gregorian;
    property OnChange      : TNotifyEvent read FOnChange write FOnChange;
    property MinimumDate   : TDateTime read FMinimumDate write FMinimumDate;
    property MaximumDate   : TDateTime read FMaximumDate write FMaximumDate;
    property MinuteInterval: TMinuteInterval read FMinuteInterval write SetMinuteInterval default 1;

    property Enabled: Boolean read FEnabled write SetEnabled default True;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFDatePicker }
// ------------------------------------------------------------------------------
constructor TDPFDatePicker.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption := 'DatePicker';
  FTimeZoneType  := TDPFTimeZoneType.tztDefault;
  FLocaleType    := TDPFLocaleType.en_;
  FPickerMode    := TDPFDatePickerMode.pmDateAndTime;
  FCalendarType  := TDPFCalendarType.Gregorian;
  FMinimumDate   := 0;
  FMaximumDate   := 0;
  FEnabled       := true;
  MinuteInterval := 1;

{$IFDEF IOS}
  FDPFDatePickerDelegate := TDPFDatePickerDelegate.Create( Self );
  FDPFDatePicker         := TUIDatePicker.Wrap( TUIDatePicker.Alloc.initWithFrame( CGRectMake( 0, 0, 320, 216 ) ) );
  FUIControl             := FDPFDatePicker;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFDatePicker.Destroy;
begin
{$IFDEF IOS}
  FDPFDatePicker.removeTarget( FDPFDatePickerDelegate.GetObjectID, // target
    Sel_getUid( 'didChanged:' ), // action
    UIControlEventValueChanged ); // event
  FDPFDatePickerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFDatePicker.GetPickerDate: TDateTime;
begin
  result := Now;
{$IFDEF IOS}
  if FDPFDatePicker <> nil then
    result := NSDateToDateTime( FDPFDatePicker.date );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFDatePicker.Loaded;
const
  FIXED_PICKER_HEIGHT = 216;
var
  dX, dY, targetHeight, scaleFactor: CGFloat;
begin
  // -------------------------------------
  // Set Picker Height
  targetHeight := FDPFDatePicker.frame.size.height;
  scaleFactor  := targetHeight / FIXED_PICKER_HEIGHT;
  if scaleFactor <> 1 then
  begin
    FDPFDatePicker.setFrame( CGRectMake( Position.X, Position.Y, Width, FIXED_PICKER_HEIGHT ) );
    FDPFDatePicker.setTransform( CGAffineTransformIdentity );
    TUIView.Wrap( FDPFDatePicker.superview ).setFrame( FDPFDatePicker.frame );
    // FDPFDatePicker.setFrame( CGRectMake( Position.X, Position.Y, Width, targetHeight ) );
    dX := FDPFDatePicker.bounds.size.Width / 2;
    dY := FDPFDatePicker.bounds.size.Height / 2;
    FDPFDatePicker.setTransform( CGAffineTransformTranslate( CGAffineTransformScale( CGAffineTransformMakeTranslation( -dX, -dY ), 1, scaleFactor ), dX, dY ) );
  end;
  { else
    FDPFDatePicker.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) ); }

  // -------------------------------------

  setEnabled( FEnabled );

  SetMinuteInterval( FMinuteInterval );
  FDPFDatePicker.setHidden( not Visible );

  FDPFDatePicker.addTarget( FDPFDatePickerDelegate.GetObjectID, // target
    Sel_getUid( 'didChanged:' ), // action
    UIControlEventValueChanged ); // event

  SetTimeZoneType( FTimeZoneType );
  SetCalendarType( FCalendarType );
  SetLocaleType( FLocaleType );
  SetPickerMode( FPickerMode );

  FDPFDatePicker.setDate( DateTimeToNSDate( Now ) );

  if FMinimumDate > 0 then
    FDPFDatePicker.setMinimumDate( DateTimeToNSDate( FMinimumDate ) );

  if FMaximumDate > 0 then
    FDPFDatePicker.setMaximumDate( DateTimeToNSDate( FMaximumDate ) );

  AddSubView( Self, ParentControl );

  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.Resize;
begin
  inherited;
{$IFDEF IOS}
  if FDPFDatePicker <> nil then
  begin
    FDPFDatePicker.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
  end;
{$ELSE} // Added by Fenistil
  Width  := iOS_GUI_Bitmaps.DatePicker.CountDown.Width;
  Height := iOS_GUI_Bitmaps.DatePicker.CountDown.Height;
  Invalidate;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.Move;
begin
  Resize;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFDatePicker.Paint;
begin
  // Added by Fenistil
  Canvas.BeginScene;
  case PickerMode of
    pmTime:
      BitmapAsBackground( Self, iOS_GUI_Bitmaps.DatePicker.Time );
    pmDate:
      BitmapAsBackground( Self, iOS_GUI_Bitmaps.DatePicker.Date );
    pmDateAndTime:
      BitmapAsBackground( Self, iOS_GUI_Bitmaps.DatePicker.DateAndTime );
    pmCountDownTimer:
      BitmapAsBackground( Self, iOS_GUI_Bitmaps.DatePicker.CountDown );
  end;
  Canvas.EndScene;
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.SetCalendarType( const Value: TDPFCalendarType );
{$IFDEF IOS}
var
  NSC: NSCalendar;
{$ENDIF}
begin
  FCalendarType := Value;
{$IFDEF IOS}
  if FDPFDatePicker <> nil then
  begin
    NSC := TNSCalendar.Wrap( TNSCalendar.Alloc.initWithCalendarIdentifier( CocoaNSStringConst( '/System/Library/Frameworks/Foundation.framework/Foundation', TDPFCalendarTypeStr[Integer( CalendarType )] ) ) );
    FDPFDatePicker.setCalendar( NSC );
    NSC.Release;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.SetEnabled( const Value: Boolean );
begin
  FEnabled := Value;
{$IFDEF IOS}
  if Assigned( FDPFDatePicker ) then
    FDPFDatePicker.setEnabled( FEnabled );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.SetPickerDate( const Value: TDateTime );
begin
{$IFDEF IOS}
  if FDPFDatePicker <> nil then
    FDPFDatePicker.setDate( DateTimeToNSDate( Value ) );
  if Assigned( OnChange ) then
    OnChange( Self );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.SetLocaleType( const Value: TDPFLocaleType );
{$IFDEF IOS}
var
  locale: NSLocale;
{$ENDIF}
begin
  FLocaleType := Value;
{$IFDEF IOS}
  if FDPFDatePicker <> nil then
  begin
    locale := TNSLocale.Wrap( TNSLocale.Alloc.initWithLocaleIdentifier( NSStr( TDPFLocaleTypeStr[Integer( FLocaleType )] ) ) );
    FDPFDatePicker.setLocale( locale );
    locale.release;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.SetMinuteInterval( const Value: TMinuteInterval );
begin
  FMinuteInterval := Value;
{$IFDEF IOS}
  if FDPFDatePicker <> nil then
    FDPFDatePicker.setMinuteInterval( Integer( Value ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.SetPickerMode( const Value: TDPFDatePickerMode );
begin
  FPickerMode := Value;
{$IFDEF IOS}
  if FDPFDatePicker <> nil then
    FDPFDatePicker.setDatePickerMode( Integer( FPickerMode ) );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFDatePicker.SetTimeZoneType( const Value: TDPFTimeZoneType );
{$IFDEF IOS}
var
  NSZ: NSTimeZone;
{$ENDIF}
begin
  FTimeZoneType := Value;
{$IFDEF IOS}
  if FDPFDatePicker <> nil then
    if FTimeZoneType = tztDefault then
      NSZ := TNSTimeZone.Wrap( TNSTimeZone.OCClass.defaultTimeZone )
    else if FTimeZoneType = tztSystem then
      NSZ := TNSTimeZone.Wrap( TNSTimeZone.OCClass.systemTimeZone )
    else if FTimeZoneType = tztLocal then
      NSZ := TNSTimeZone.Wrap( TNSTimeZone.OCClass.localTimeZone );

  FDPFDatePicker.setTimeZone( NSZ );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

{ TDPFDatePickerDelegate }
constructor TDPFDatePickerDelegate.Create( AOwner: TDPFDatePicker );
begin
  inherited Create;
  FDPFDatePicker := AOwner;
end;

// ------------------------------------------------------------------------------
procedure TDPFDatePickerDelegate.didChanged( datePicker: UIDatePicker );
begin
  if Assigned( FDPFDatePicker.FOnChange ) then
    FDPFDatePicker.FOnChange( FDPFDatePicker );

end;
{$ENDIF}

// ------------------------------------------------------------------------------
end.
