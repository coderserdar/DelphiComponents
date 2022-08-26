// ------------------------------------------------------------------------------
// DPF.iOS.RateApp Component
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
unit DPF.iOS.RateApp;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.IniFiles,
  System.TypInfo,
  System.Math,
  System.DateUtils,

  DPF.iOS.BaseControl,
  DPF.iOS.UIAlertView,
  DPF.iOS.StoreKit,
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
{$ENDIF}
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls;

type
  TDPFRateApp = class;

  // ------------------------------------------------------------------------------
{$IFDEF IOS}
{$ENDIF}
  TDPFRateAppState = ( raUnknown, raRated, raNoThanks, raRemindMeLater );

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFRateApp = class( TComponent )
  private
{$IFDEF IOS}
    FIniFileName: string;
{$ENDIF}
    FRated                : Boolean;
    FAppLastCheckedDate   : TDateTime;
    FAppRunCount          : Integer;
    FRemindMeLater        : Boolean;
    FNoThanks             : Boolean;
    FEveryNDay            : Integer;
    FEveryNCountRun       : Integer;
    FRateMessage          : string;
    FRateTitle            : string;
    FAppID                : int64;
    FNoThanksButtonCaption: string;
    FRateItButtonCaption  : string;
    FRemindMeButtonCaption: string;
    inRatingMod           : Boolean;

    FDPFInAppPurchase: TDPFInAppPurchase;
{$IFDEF IOS}
    FAppStoreShowTime: TDateTime;
    FSectionName     : string;
{$ENDIF}
  protected

{$IFDEF IOS}
    procedure LoadData;
    procedure OnCloseAppStoreRate( sender: TObject );
{$ENDIF}
  public
{$IFDEF IOS}
    procedure Loaded; override;
    function CheckRateApp( const ForceToRate: Boolean = false ): TDPFRateAppState;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    property Rated: Boolean read FRated;
    property AppRunCount: Integer read FAppRunCount;
    property RemindMeLater: Boolean read FRemindMeLater;
    property NoThanks: Boolean read FNoThanks;
    property AppLastCheckedDate: TDateTime read FAppLastCheckedDate;
  published
    property AppID         : int64 read FAppID write FAppID;
    property EveryNDay     : Integer read FEveryNDay write FEveryNDay default 8;
    property EveryNCountRun: Integer read FEveryNCountRun write FEveryNCountRun default 10;
    property RateTitle     : string read FRateTitle write FRateTitle;
    property RateMessage   : string read FRateMessage write FRateMessage;

    property RateItButtonCaption  : string read FRateItButtonCaption write FRateItButtonCaption;
    property RemindMeButtonCaption: string read FRemindMeButtonCaption write FRemindMeButtonCaption;
    property NoThanksButtonCaption: string read FNoThanksButtonCaption write FNoThanksButtonCaption;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFRateApp }
// ------------------------------------------------------------------------------
constructor TDPFRateApp.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  FEveryNDay             := 8;
  FEveryNCountRun        := 10;
  FRateTitle             := 'Rate DPF Components';
  FRateMessage           := 'If you enjoy this app, would you mind taking a moment to rate it? It won''t take more than a minute. ' + #10#13 + 'Thanks for your support!';
  FRateItButtonCaption   := 'Rate It Now';
  FRemindMeButtonCaption := 'Remind Me Later';
  FNoThanksButtonCaption := 'No, Thanks';
  inRatingMod            := false;

{$IFDEF IOS}
  FSectionName := 'RateApp' + GetAppVersion;
  FIniFileName := GetPreferencesFolder + 'DPFRateAppConfig.ini';
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFRateApp.Destroy;
begin
  inherited;
  if not Assigned( FDPFInAppPurchase ) then
    FDPFInAppPurchase.DisposeOf;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFRateApp.LoadData;
var
  ini: TIniFile;
begin
  ini                 := TIniFile.Create( FIniFileName );
  FRated              := ini.ReadBool( FSectionName, 'Rated', false );
  FNoThanks           := ini.ReadBool( FSectionName, 'NoThanks', False );
  FRemindMeLater      := ini.ReadBool( FSectionName, 'RemindMeLater', true );
  FAppRunCount        := ini.ReadInteger( FSectionName, 'AppRunCount', 0 );
  FAppLastCheckedDate := ini.ReadDateTime( FSectionName, 'AppLastCheckedDate', Now );
  ini.Free;
end;

// ------------------------------------------------------------------------------

procedure TDPFRateApp.Loaded;
var
  ini: TIniFile;
begin
  ini                 := TIniFile.Create( FIniFileName );
  FRated              := ini.ReadBool( FSectionName, 'Rated', false );
  FNoThanks           := ini.ReadBool( FSectionName, 'NoThanks', False );
  FRemindMeLater      := ini.ReadBool( FSectionName, 'RemindMeLater', true );
  FAppRunCount        := ini.ReadInteger( FSectionName, 'AppRunCount', -1 );
  FAppLastCheckedDate := ini.ReadDateTime( FSectionName, 'AppLastCheckedDate', now );

  if FAppRunCount = -1 then
  begin
    ini.WriteDateTime( FSectionName, 'AppLastCheckedDate', Now );
    FAppRunCount := 0;
  end;
  ini.WriteInteger( FSectionName, 'AppRunCount', FAppRunCount + 1 );
  ini.Free;
  inherited;
end;

// ------------------------------------------------------------------------------
function TDPFRateApp.CheckRateApp( const ForceToRate: Boolean = false ): TDPFRateAppState;
var
  idx         : Integer;
  DPFAlertView: TDPFAlertView;
  ini         : TIniFile;
begin
  result := raUnknown;
  if inRatingMod then
    exit;

  inRatingMod := true;
  try
    LoadData;
    if ForceToRate or ( not FRated and not FNoThanks and ( ( FAppRunCount >= EveryNCountRun ) or ( DaysBetween( Now, FAppLastCheckedDate ) >= EveryNDay ) ) ) then
    begin
      DPFAlertView := TDPFAlertView.Create( nil );
      try
        idx := DPFAlertView.ShowMessageDialog( RateTitle, RateMessage, [FRateItButtonCaption, FRemindMeButtonCaption, FNoThanksButtonCaption], 0, [], [] );
        if idx = 0 then
        begin
          if not Assigned( FDPFInAppPurchase ) then
            FDPFInAppPurchase := TDPFInAppPurchase.Create( nil );

          FAppStoreShowTime := Now;
          FDPFInAppPurchase.ShowProductInAppStore( FAppID, OnCloseAppStoreRate );
        end;
        ini       := TIniFile.Create( FIniFileName );
        FNoThanks := idx = 2;
        ini.WriteBool( FSectionName, 'NoThanks', FNoThanks );

        FRemindMeLater := idx = 1;
        ini.WriteBool( FSectionName, 'RemindMeLater', FRemindMeLater );
        if FRemindMeLater then
        begin
          ini.WriteDateTime( FSectionName, 'AppLastCheckedDate', Now );
          ini.WriteInteger( FSectionName, 'AppRunCount', 0 );
        end;
        ini.Free;

      finally
        DPFAlertView.DisposeOf;
      end;
    end;
    result := raUnknown;
    if FNoThanks then
      result := raNoThanks
    else if FRated then
      result := raRated
    else if FRemindMeLater then
      result := raRemindMeLater;
  finally
    inRatingMod := false;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFRateApp.OnCloseAppStoreRate( sender: TObject );
var
  ini: TIniFile;
begin
  ini    := TIniFile.Create( FIniFileName );
  FRated := true;
  ini.WriteBool( FSectionName, 'Rated', true );
  ini.WriteDateTime( FSectionName, 'AppLastCheckedDate', Now );
  ini.Free;
end;
{$ENDIF}
// ------------------------------------------------------------------------------

end.
