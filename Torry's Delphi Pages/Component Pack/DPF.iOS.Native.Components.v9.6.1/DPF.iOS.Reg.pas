// ------------------------------------------------------------------------------
// DPF.iOS.Reg Component
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
unit DPF.iOS.Reg;

interface

{$I DPF.iOS.Defs.inc}

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

{$IFNDEF IOS}

type
  // ----------------------------------------------------------------------------
  IFormDesigner = IDesigner;
  TFormDesigner = IFormDesigner;

  // ----------------------------------------------------------------------------
{$ENDIF}
{$R DPF.iOS.DesignTime.Splash.res}
  // ------------------------------------------------------------------------------
procedure Register;

implementation

uses
  DPF.iOS.UIView,
  DPF.iOS.UIViewController,
  DPF.iOS.UITabBarController,
  DPF.iOS.UITabBarController.DesignTime,
  DPF.iOS.ABPeoplePicker,
  DPF.iOS.EKCalendar,
  DPF.iOS.HTTP,
  DPF.iOS.MFMailCompose,
  DPF.iOS.MFMessageCompose,
  DPF.iOS.MKMapView,
  DPF.iOS.MPMoviePlayerViewController,
  DPF.iOS.MPVolume,
  DPF.iOS.UIImageView,
  DPF.iOS.UIButton,
  DPF.iOS.UILabel,
  DPF.iOS.UIPageControl,
  DPF.iOS.UIScrollView,
  DPF.iOS.UISegmentedControl,
  DPF.iOS.UISlider,
  DPF.iOS.UIStepper,
  DPF.iOS.UISwitch,
  DPF.iOS.UITableView,
  DPF.iOS.UITextField,
  DPF.iOS.UITextView,
  DPF.iOS.UIToolbar,
  DPF.iOS.UIWebView,
  DPF.iOS.UINavigationController,
  DPF.iOS.UINavigationController.DesignTime,
  DPF.iOS.UIPickerView,
  DPF.iOS.UIAlertView,
  DPF.iOS.UIDatePicker,
  DPF.iOS.UIPageViewController,
  DPF.iOS.UIActionSheet,
  DPF.iOS.UIImagePickerController,
  DPF.iOS.UIActivityViewController,
  DPF.iOS.SLComposeViewController,
  DPF.iOS.AVPlayer,
  DPF.iOS.UIProgressView,
  DPF.iOS.NSOperationQueue,
  DPF.iOS.UIActivityIndicatorView,
  DPF.iOS.UIPopoverController,
  DPF.iOS.QLPreviewController,
  DPF.iOS.UISearchBar,
  DPF.iOS.CheckBox,
  DPF.iOS.ADBanner,
  DPF.iOS.UIComboBox,
  DPF.iOS.UIDateTimeComboBox,
  DPF.iOS.ApplicationManager,
  DPF.iOS.SlideDialog,
  DPF.iOS.NSTimer,
  DPF.iOS.StoreKit,
  DPF.iOS.GLKView,
  DPF.iOS.GameCenterManager,
  DPF.iOS.Media,
  DPF.iOS.Keyboard,
  DPF.iOS.RateApp,
  DPF.iOS.NSUserDefaults,
  DPF.iOS.iCloud,
  DPF.iOS.iCloudDoc,
  DPF.iOS.UICollectionView,
  DPF.iOS.BarcodeReader,
  DPF.iOS.UIPrintPageRenderer,
  DPF.iOS.AVAudioRecorder,
  DPF.iOS.LocalAuthentication;

// ------------------------------------------------------------------------------
procedure AddSplash;
var
  Bmp: HBITMAP;
begin
  if Assigned( SplashScreenServices ) then
  begin
    Bmp := LoadBitmap( FindResourceHInstance( HInstance ), 'DPF_LOGO' );
    try
      SplashScreenServices.AddPluginBitmap( 'D.P.F Delphi iOS Native Components ', Bmp, False, 'Apache License' );
    finally
      DeleteObject( Bmp );
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents( 'D.P.F Components iOS', [TDPFUIView, TDPFToolbar, TDPFTextView, TDPFTextField, TDPFUITableView, TDPFSwitch, TDPFStepper, TDPFSlider, TDPFSegmentedControl, TDPFUIScrollView, TDPFUIPageControl, TDPFLabel, TDPFImageView, TDPFButton, TDPFMPVolume, TDPFMPMoviePlayerViewController, TDPFMapView, TDPFMessageCompose, TDPFMailCompose, TDPFHttp,
    TDPFEKCalendar, TDPFAddressBook, TDPFTabBarController, TDPFWeb, TDPFNavigationController, TDPFPickerView, TDPFAlertView, TDPFDatePicker, TDPFUIViewController, TDPFUIPageViewController, TDPFUIActionSheet, TDPFUIImagePickerController, TDPFActivityViewController, TDPFSLComposeViewController, TDPFAVPlayer, TDPFProgress, TDPFNSOperationQueue,
    TDPFActivityIndicatorView, TDPFPopover, TDPFQLPreviewController, TDPFSearchBar, TDPFCheckBox, TDPFADBanner, TDPFComboBox, TDPFDateTimeComboBox, TDPFApplicationManager, TDPFSlideDialog, TDPFNSTimer, TDPFInAppPurchase, TDPFGLKView, TDPFGameCenterManager, TDPFCamera, TDPFKeyboard, TDPFRateApp, TDPFUserDefaults, TDPFiCloud, TDPFiCloudDoc,
    TDPFUICollectionView, TDPFQRCodeScanner, TDPFUIPrintPageRenderer, TDPFAVAudioRecorder, TDPFLocalAuthentication] );

{$IFDEF WIN32}
  RegisterComponentEditor( TDPFTabBarController, TTabBarControllerEditor );
  RegisterComponentEditor( TDPFTabBarItem, TTabBarControllerEditor );
  System.Classes.RegisterClass( TDPFTabBarItem );
  System.Classes.RegisterClass( TDPFTabBar );
  RegisterPropertyEditor( TypeInfo( Integer ), TDPFTabBarController, 'ActivePageIndex', TDPFTabBarControllerActivePageIndexProperty );

  RegisterComponentEditor( TDPFNavigationController, TNavigationControllerEditor );
  RegisterComponentEditor( TDPFNavigationControllerPage, TNavigationControllerEditor );
  System.Classes.RegisterClass( TDPFNavigationControllerPage );
  System.Classes.RegisterClass( TDPFNavigationNavBar );
  System.Classes.RegisterClass( TDPFNavigationToolBar );
  RegisterPropertyEditor( TypeInfo( string ), TDPFNavigationController, 'ActivePage', TDPFNavigationControllerActivePageProperty );

{$ENDIF}
end;

initialization

AddSplash;

end.
