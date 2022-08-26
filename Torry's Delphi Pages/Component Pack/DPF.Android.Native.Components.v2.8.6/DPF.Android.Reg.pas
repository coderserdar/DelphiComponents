// ------------------------------------------------------------------------------
// DPF.Android.Reg Java Classes
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Babak Yaghoobi
//
// Email #1: yaghoobi@dpfaragir.com
// Email #2: b_yaghobi@yahoo.com
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
unit DPF.Android.Reg;

interface

{$I DPF.Android.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.TypInfo,

{$IFDEF WIN32}
  Windows, DesignEditors, DesignIntf, ToolsAPI,
{$ENDIF}
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls;

{$IFNDEF ANDROID}

type
  // ----------------------------------------------------------------------------
  IFormDesigner = IDesigner;
  TFormDesigner = IFormDesigner;

  // ----------------------------------------------------------------------------
{$ENDIF}
{$R DPF.ANDROID.DesignTime.Splash.res}
  // ------------------------------------------------------------------------------
procedure Register;

implementation

uses

{$IFDEF WIN32}
  DPF.Android.JTabHost.DesignTime,
{$ENDIF}
  DPF.Android.ApplicationManager,
  DPF.Android.JTextView,
  DPF.Android.JRelativeLayout,
  DPF.Android.JLinearLayout,
  DPF.Android.JEditText,
  DPF.Android.JButton,
  DPF.Android.JAlertDialog,
  DPF.Android.JAnalogClock,
  DPF.Android.JCheckBox,
  DPF.Android.JCheckedTextView,
  DPF.Android.JChronometer,
  DPF.Android.JDatePicker,
  DPF.Android.JImageView,
  DPF.Android.JListView,
  DPF.Android.JToast,
  DPF.Android.JProgressBar,
  DPF.Android.JProgressDialog,
  DPF.Android.JTimePickerDialog,
  DPF.Android.JDatePickerDialog,
  DPF.Android.JNumberPicker,
  DPF.Android.JRadioButton,
  DPF.Android.JRadioGroup,
  DPF.Android.JScrollView,
  DPF.Android.JSeekBar,
  DPF.Android.JSpinner,
  DPF.Android.JTabHost,
  DPF.Android.JTextSwitcher,
  DPF.Android.JWebView,
  DPF.Android.JTextClock,
  DPF.Android.JVideoView,
  DPF.Android.JHTTP,
  DPF.Android.JAnimation,
  DPF.Android.JMapView ;

// ------------------------------------------------------------------------------
procedure Register;
var
  Bmp: HBITMAP;
begin
  RegisterComponents( 'D.P.F Android Components', [TDPFAndroidApplicationManager, TDPFJRelativeLayout, TDPFJLinearLayout, TDPFJTextView, TDPFJEditText, TDPFJButton, TDPFJAlertDialog, TDPFJAnalogClock, TDPFJCheckBox, TDPFJChronometer, TDPFJDatePicker, TDPFJImageView, TDPFJListView, TDPFJToast, TDPFJProgressBar, TDPFJProgressDialog, TDPFJTimePickerDialog, TDPFJDatePickerDialog,
    TDPFJNumberPicker, TDPFJRadioButton, TDPFJRadioGroup, TDPFJScrollView, TDPFJSeekBar, TDPFJSpinner, TDPFJTabHost, TDPFJTextSwitcher, TDPFJWebView, TDPFJTextClock, TDPFJVideoView, TDPFJHTTP, TDPFJAnimation, TDPFJMapView] );

{$IFDEF WIN32}
  RegisterComponentEditor( TDPFJTabHost, TAndroidTabBarControllerEditor );
  RegisterComponentEditor( TDPFAndroidTabBarItem, TAndroidTabBarControllerEditor );
  System.Classes.RegisterClass( TDPFAndroidTabBarItem );
  System.Classes.RegisterClass( TDPFAndroidTabBar );
  RegisterPropertyEditor( TypeInfo( Integer ), TDPFJTabHost, 'ActivePageIndex', TDPFJTabHostActivePageIndexProperty );

  // RegisterComponentEditor( TDPFNavigationController, TNavigationControllerEditor );
  // RegisterComponentEditor( TDPFNavigationControllerPage, TNavigationControllerEditor );
  // System.Classes.RegisterClass( TDPFNavigationControllerPage );
  // System.Classes.RegisterClass( TDPFNavigationNavBar );
  // System.Classes.RegisterClass( TDPFNavigationToolBar );
  // RegisterPropertyEditor( TypeInfo( string ), TDPFNavigationController, 'ActivePage', TDPFNavigationControllerActivePageProperty );

  if Assigned( SplashScreenServices ) then
  begin
    Bmp := LoadBitmap( FindResourceHInstance( HInstance ), 'DPF_Android_LOGO' );
    try
      SplashScreenServices.AddPluginBitmap( 'D.P.F Delphi Android Native Components ', Bmp, False, 'Apache License' );
    finally
      DeleteObject( Bmp );
    end;
  end;

{$ENDIF}
end;

end.
