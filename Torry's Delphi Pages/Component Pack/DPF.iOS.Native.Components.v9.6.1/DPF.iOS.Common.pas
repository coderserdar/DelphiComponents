// ------------------------------------------------------------------------------
// DPF.iOS.Common Tools & Classes
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
unit DPF.iOS.Common;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Math,
  System.Classes,
  System.NetEncoding,
  FMX.Consts,
  FMX.Types,
  FMX.Types3D,
  FMX.Platform,
{$IFDEF DELPHIXE5}
  FMX.Graphics,
{$ENDIF}
  DateUtils,
{$IFDEF IOS}
{$IFDEF DELPHIXE6}
  Macapi.Helpers,
{$ENDIF}
  Soap.EncdDecd,
  Macapi.ObjCRuntime,
  Macapi.ObjectiveC,
  Posix.Wchar,

  Macapi.CoreFoundation,
  IOSapi.CoreLocation,
  IOSApi.Foundation,
  IOSApi.UIKit,
  IOSApi.QuartzCore,
  IOSApi.CocoaTypes,
  IOSApi.CoreGraphics,
  IOSApi.CoreText,
  FMX.Platform.iOS,
{$ELSE}
{$IFDEF MACOS}
  Macapi.ObjectiveC,
  Macapi.Foundation
{$ENDIF MACOS}
{$ENDIF IOS}
    FMX.Forms;

const
  libAudioToolbox = '/System/Library/Frameworks/AudioToolbox.framework/AudioToolbox';
  libKit          = '/System/Library/Frameworks/UIKit.framework/UIKit';
  libc            = '/usr/lib/libc.dylib';

  kSystemSoundID_vibrate = $FFF;

  MERCATOR_OFFSET = 268435456;
  MERCATOR_RADIUS = 85445659.44705395;

  {$IFNDEF IOS}
type
    {$IFDEF CPU64}
      CGFloat = Double;
    {$ELSE}
      CGFloat = Single;
    {$ENDIF}
  {$ENDIF}

{$IFDEF IOS}
  // ------------------------------------------------------------------------------

function NSSTR( const Str: string ): NSString;

// ------------------------------------------------------------------------------
function sysctl( name: PInteger; namelen: cardinal; oldp: Pointer; oldlen: Psize_t; newp: Pointer; newlen: size_t ): Integer; cdecl; external libc name _PU + 'sysctl';
function sysctlbyname( Name: MarshaledAString; oldp: Pointer; oldlen: Psize_t; newp: Pointer; newlen: size_t ): Integer; cdecl; external libc name _PU + 'sysctlbyname';
function sysctlnametomib( name: MarshaledAString; mibp: PInteger; sizep: Psize_t ): Integer; cdecl; external libc name _PU + 'sysctlnametomib';

// ------------------------------------------------------------------------------
procedure AudioServicesPlaySystemSound( inSystemSoundID: UInt32 ); cdecl; external libAudioToolbox name _PU + 'AudioServicesPlaySystemSound';
procedure UIGraphicsBeginImageContext( size: CGSize ); cdecl; external libUIKit name _PU + 'UIGraphicsBeginImageContext';
procedure UIGraphicsEndImageContext; cdecl; external libUIKit name _PU + 'UIGraphicsEndImageContext';

procedure UIGraphicsPushContext( context: CGContextRef ); cdecl; external libUIKit name _PU + 'UIGraphicsPushContext';
procedure UIGraphicsPopContext; cdecl; external libUIKit name _PU + 'UIGraphicsPopContext';

// function UIEdgeInsetsMake( top: CGFloat; left: CGFloat; bottom: CGFloat; right: CGFloat ): UIEdgeInsets; cdecl; external libUIKit name _PU + 'UIEdgeInsetsMake';
// {$EXTERNALSYM UIEdgeInsetsMake}

function GetSharedApplication: UIApplication;

function ScreenForceRotate( IfOrientationNotIn: TScreenOrientation ): Boolean;

procedure HideKeyBoard;

procedure ShowAlert( AMessage: string; ATitle: string = ''; CloseButton: string = 'Ok' );
function NSStrToStr( const ASource: NSString ): string;
function PNSStr( const AStr: string ): PNSString;
function GetAppVersion: string;
procedure VibrateDevice;
function GetFileSize( const FileName: string ): Int64;

function GetAppFolder: string;
function GetDocumentsFolder: string;
function GetTempDirectory: string;
function GetPreferencesFolder: string;

function TColorToUIColor( const Color: TAlphaColor; Alpha: Single ): UIColor; overload;
function TColorToUIColor( const Color: TAlphaColor ): UIColor; overload;
function TColorToUIColorPtr( const Color: TAlphaColor; Alpha: Single = 1 ): Pointer;
function AddFontFromFile( const FileName: string ): Boolean;

function NSDateToDateTime( const ADateTime: NSDate ): TDateTime;
function DateTimeToNSDate( const ADateTime: TDateTime ): NSDate;
function GetTimeZone: integer;
function GetGMTDateTime( const ADateTime: TDateTime ): TDateTime; overload;
function GetGMTDateTime( const ADateTime: TDateTime; TimeZone: integer ): TDateTime; overload;
function GetLocalDateTime( const ADateTime: TDateTime ): TDateTime; overload;

function longitudeToPixelSpaceX( longitude: double ): double;
function latitudeToPixelSpaceY( latitude: double ): double;
function pixelSpaceXToLongitude( pixelX: double ): double;
function pixelSpaceYToLatitude( pixelY: double ): double;

function GetURLContent( const HttpURL: string ): string;

function BitmapToUIImage( const Bitmap: TBitmap ): UIImage;
function UIImageToBitmap( const AImage: UIImage; ARotate: Single ): TBitmap;
function UIImageToBase64( const AImage: UIImage ): string;
function MemoryStreamToUIImage( const memStream: TMemoryStream ): UIImage;

procedure SaveBitmapInAlbum( const BitMap: TBitmap );
procedure SaveImageInAlbum( const Image: UIImage );

function IsIPad: Boolean;

function GetFileMIMEType( const AFileName: string ): string;

function CocoaNSStringConst( Fwk: string; ConstStr: string ): NSString;
function CocoaNSNumberConst( Fwk: string; ConstStr: string ): NSNumber;
function FNV1aHash( const S: string ): Cardinal;
function GetHashFileName( URL: string; CreateDirectory: Boolean ): string;

function GetHash( const Str: string ): int64;

procedure DPFNSLog( const AMessage: string ); overload;
procedure DPFNSLog( const AMessage: string; Rect: CGRect ); overload;

function PolyDecode( const encoded: string ): TArray<CLLocationCoordinate2D>;

procedure PlaySystemSound( SoundID: UInt32 = $450 );

function IsNumeric( const AString: string ): Boolean; overload;
function IsNumeric( const AChar: Char ): Boolean; inline; overload;

function convertImageToGrayScale( image: UIImage ): UIImage;

procedure SetAppFullScreen;

function GetImageOrientation( const AImage: UIImage ): Single;

procedure RemoveFile( FileName: string ); overload;
procedure RemoveFile( FileURL: NSURL ); overload;

function MakeScreenshot( view: UIView ): UIImage;

function AddLeadingZeroes( const aNumber, Length: integer ): string;

function isURLExists( const URL: string; const timeout: Single = 5.0 ): Integer;
function GetUUID( deleteExtra: Boolean = true ): string;
function GetDeviceID: string;
function GenerateBoundary: string;

function GetPlatform: string;
function GetSysInfoByName( typeSpecifier: string ): string;

function GetNSURL( URL: string ): NSURL;

function RenameFile( oldFileName: string; newFileName: string ): boolean;

{$ENDIF}
procedure DisposeOfAndNil( var Obj ); // SZ added

implementation

{$IFDEF IOS}

uses DPF.iOS.Classes;

var
  MIMETableList: TStringList;
  // DeviceTimeZone: NSInteger = $FFFF;

  // ------------------------------------------------------------------------------
procedure DPFNSLog( const AMessage: string );
begin
{$IFDEF DEBUG}
  try
    NSLog( ( NSSTR( AMessage ) as ILocalObject ).GetObjectID );
  except
  end;
{$ENDIF}
end;

procedure DPFNSLog( const AMessage: string; Rect: CGRect );
begin
  {$IFDEF DEBUG}
  DPFNSLog(AMessage + ' ' + Format('%.1n, %.1n, %.1n, %.1n', [Rect.origin.x, Rect.origin.y, Rect.size.width, rect.size.height]));
  {$ENDIF}
end;

// ------------------------------------------------------------------------------
function NSSTR( const Str: string ): NSString;
begin
{$IFDEF DELPHIXE6}
  result := Macapi.Helpers.StrToNSStr( Str );
{$ELSE}
  result := iOSapi.Foundation.NSSTR( Str );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function CocoaPointerConst( Fwk: string; ConstStr: string ): Pointer;
var
  FwkMod: HMODULE;
begin
  Result := nil;
  FwkMod := LoadLibrary( PWideChar( Fwk ) );
  if FwkMod <> 0 then
  begin
    Result := GetProcAddress( FwkMod, PWideChar( ConstStr ) );
    FreeLibrary( FwkMod );
  end;
end;

// ------------------------------------------------------------------------------
// const FoundationFwk: string = '/System/Library/Frameworks/Foundation.framework/Foundation';
//
// Result := CocoaNSStringConst(FoundationFwk, 'NSLocaleLanguageCode');
//
// ------------------------------------------------------------------------------
function CocoaNSStringConst( Fwk: string; ConstStr: string ): NSString;
var
  Obj: Pointer;
begin
  Obj := Pointer( CocoaPointerConst( Fwk, ConstStr )^ );
  if Obj <> nil then
    Result := TNSString.Wrap( Obj )
  else
    Result := nil;
end;

// ------------------------------------------------------------------------------
function CocoaNSNumberConst( Fwk: string; ConstStr: string ): NSNumber;
var
  Obj: Pointer;
begin
  Obj := Pointer( CocoaPointerConst( Fwk, ConstStr )^ );
  if Obj <> nil then
    Result := TNSNumber.Wrap( Obj )
  else
    Result := nil;
end;

// ------------------------------------------------------------------------------
function GetSysInfoByName( typeSpecifier: string ): string;
var
  Size   : Integer;
  AResult: TArray<Byte>;
begin
  sysctlbyname( MarshaledAString( TMarshal.AsAnsi( typeSpecifier ) ), nil, @Size, nil, 0 );
  SetLength( AResult, Size );
  sysctlbyname( MarshaledAString( TMarshal.AsAnsi( typeSpecifier ) ), MarshaledAString( AResult ), @Size, nil, 0 );
  Result := TEncoding.UTF8.GetString( AResult );
end;

// ------------------------------------------------------------------------------
(*
  iPhone1,1 ->  iPhone 1G
  iPhone1,2 ->  iPhone 3G
  iPhone2,1 ->  iPhone 3GS
  iPhone3,1 ->  iPhone 4/AT&T
  iPhone3,2 ->  iPhone 4/Other
  iPhone3,3 ->  iPhone 4/Verizon
  iPhone4,1 ->  (iPhone 4S/GSM)
  iPhone4,2 ->  (iPhone 4S/CDMA)
  iPhone4,3 ->  (iPhone 4S)
  iPhone5,1 ->  iPhone Next Gen
  iPhone5,1 ->  iPhone Next Gen
  iPhone5,1 ->  iPhone Next Gen

  iPod1,1   ->  iPod touch 1G
  iPod2,1   ->  iPod touch 2G
  iPod2,2   ->  Unknown
  iPod3,1   ->  iPod touch 3G
  iPod4,1   ->  iPod touch 4G

  iPad1,1   ->  iPad 1G, WiFi and 3G
  iPad2,1   ->  iPad 2G, WiFi
  iPad2,2   ->  iPad 2G, GSM 3G
  iPad2,3   ->  iPad 2G, CDMA 3G
  iPad3,1   ->  (iPad 3G, WiFi)
  iPad3,2   ->  (iPad 3G, GSM)
  iPad3,3   ->  (iPad 3G, CDMA)
  iPad4,1   ->  (iPad 4G, WiFi)
  iPad4,2   ->  (iPad 4G, GSM)
  iPad4,3   ->  (iPad 4G, CDMA)

  AppleTV2,1 -> AppleTV 2
  AppleTV3,1 -> AppleTV 3

  i386, x86_64 -> iPhone Simulator
*)
function GetPlatform: string;
begin
  result := GetSysInfoByName( 'hw.machine' );
end;

// ------------------------------------------------------------------------------
function IsIPad: Boolean;
begin
  Result := TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).userInterfaceIdiom = UIUserInterfaceIdiomPad;
end;

// ------------------------------------------------------------------------------
procedure SetAppFullScreen;
begin
  GetSharedApplication.setStatusBarHidden( True );
  GetSharedApplication.keyWindow.RootViewController.setWantsFullScreenLayout( True );
  GetSharedApplication.keyWindow.RootViewController.view.setFrame( TUIScreen.Wrap( TUIScreen.OCClass.mainScreen ).bounds );
end;

// ------------------------------------------------------------------------------
function ScreenForceRotate( IfOrientationNotIn: TScreenOrientation ): boolean;
const
  ConvOri: array [TScreenOrientation] of NativeUInt = ( UIDeviceOrientationPortrait, UIDeviceOrientationLandscapeRight, UIDeviceOrientationPortraitUpsideDown, UIDeviceOrientationLandscapeLeft );
var
  w: UIWindow;
  v: UIView;
  // ScreenService : IFMXScreenService;
  MainRroot   : UIViewController;
  Orientations: TFormOrientations;
  // CurOrientation: TScreenOrientation;
begin
  Orientations := Application.FormFactor.Orientations;
  try
    (*
      if TPlatformServices.Current.SupportsPlatformService( IFMXScreenService, IInterface( ScreenService ) ) then
      CurOrientation := ScreenService.GetScreenOrientation
      else
      CurOrientation := TScreenOrientation.soPortrait;

      Result           := CurOrientation <> IfOrientationNotIn;
    *)
    Result := true; // CurOrientation <> IfOrientationNotIn;

    if Result then
    begin

      MainRroot := GetSharedApplication.keyWindow.rootViewController;

      Application.FormFactor.Orientations := [TScreenOrientation.Portrait, TScreenOrientation.Landscape, TScreenOrientation.InvertedPortrait, TScreenOrientation.InvertedLandscape];
      GetSharedApplication.keyWindow.setRootViewController( nil );
      // application.ProcessMessages;

      GetSharedApplication.setStatusBarOrientation( ConvOri[IfOrientationNotIn] );
      TUIDevice.Wrap( TUIDevice.OCClass.currentDevice ).performSelector( sel_getUid( 'setOrientation:' ), ( TNSNumber.Wrap( TNSNumber.OCClass.numberWithUnsignedInt( ConvOri[IfOrientationNotIn] ) ) as ILocalObject ).GetObjectID, 0 );

      w := GetSharedApplication.keyWindow;
      if w.subviews.count > 0 then
      begin
        v := TUIView.Wrap( w.subviews.objectAtIndex( 0 ) );
        v.removeFromSuperview;
        w.insertSubview( v, 0 );
      end;
      GetSharedApplication.keyWindow.setRootViewController( MainRroot );
      GetSharedApplication.keyWindow.rootViewController.didRotateFromInterfaceOrientation( ConvOri[IfOrientationNotIn] );

    end;
  finally
    Application.FormFactor.Orientations := Orientations;
  end;
end;

// ------------------------------------------------------------------------------
function GetURLContent( const HttpURL: string ): string;
var
  url    : NSURL;
  content: NSString;
begin
  url     := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( HttpURL ) ) );
  content := TNSString.Wrap( TNSString.OCClass.stringWithContentsOfURL( url, NSUTF8StringEncoding, nil ) );
  Result  := UTF8ToString( content.UTF8String );
end;

// ------------------------------------------------------------------------------
function PNSStr( const AStr: string ): PNSString;
begin
  Result := ( NSStr( AStr ) as ILocalObject ).GetObjectID
end;

// ------------------------------------------------------------------------------
function GetSharedApplication: UIApplication;
begin
  Result := TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication );
end;

// ------------------------------------------------------------------------------
procedure HideKeyBoard;
begin
  GetSharedApplication.keyWindow.endEditing( true );
end;

// ------------------------------------------------------------------------------
function GetAppVersion: string;
var
  NSB  : NSBundle;
  NSDic: NSDictionary;
  NS   : NSString;
begin
  NSB    := TNSBundle.Wrap( TNSBundle.OCClass.mainBundle );
  NSDic  := NSB.InfoDictionary;
  NS     := TNSString.Wrap( NSDic.valueForKey( NSStr( 'CFBundleVersion' ) ) );
  Result := UTF8ToString( NS.UTF8String );
end;

// ------------------------------------------------------------------------------
// Note the following points:
// 1) Vibration work on iPhone and On the iPod touch, does nothing.
// 2) In the iPhone settings->Sounds->Vibrate on Ring set to ON
procedure VibrateDevice;
begin
  AudioServicesPlaySystemSound( kSystemSoundID_vibrate );
end;

// ------------------------------------------------------------------------------
function GetFileSize( const FileName: string ): Int64;
var
  FileManager   : NSFileManager;
  fileAttributes: NSDictionary;
  url           : NSURL;
begin
  url         := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( ParamStr( 0 ) ) ) );
  FileManager := TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager );

  fileAttributes := FileManager.attributesOfItemAtPath( NSStr( FileName ), nil );
  Result         := fileAttributes.fileSize;
  FileManager.release;
  FileManager := nil;
end;

// ------------------------------------------------------------------------------
function GetAppFolder: string;
begin
  Result := GetHomePath + PathDelim + Application.Title + '.app' + PathDelim;
end;

// ------------------------------------------------------------------------------
function GetDocumentsFolder: string;
var
  searchPaths : NSArray;
  documentPath: NSString;
  UR          : NSURL;
begin
  searchPaths := TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager ).URLsForDirectory( NSDocumentDirectory, NSUserDomainMask );
  UR          := TNSURL.Wrap( searchPaths.lastObject );
  result      := UTF8ToString( UR.path.UTF8String );
  result      := IncludeTrailingBackslash( result );
  TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager ).createDirectoryAtPath( UR.path, true, nil, nil );
  exit;

  searchPaths := TNSArray.Wrap( NSSearchPathForDirectoriesInDomains( NSDocumentDirectory, NSUserDomainMask, true ) );
  if assigned( searchPaths ) and ( searchPaths.count > 0 ) then
    documentPath := TNSString.Wrap( searchPaths.objectAtIndex( 0 ) );
  if assigned( documentPath ) then
    result := UTF8ToString( documentPath.UTF8String );
  result   := IncludeTrailingBackslash( result );
end;

// ------------------------------------------------------------------------------
function GetPreferencesFolder: string;
var
  searchPaths: NSArray;
  UR         : NSURL;
begin
  searchPaths := TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager ).URLsForDirectory( NSLibraryDirectory, NSUserDomainMask );
  UR          := TNSURL.Wrap( searchPaths.lastObject );
  result      := UTF8ToString( UR.path.UTF8String );
  result      := IncludeTrailingBackslash( result ) + 'Preferences/';
  TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager ).createDirectoryAtPath( UR.path, true, nil, nil );
end;

// ------------------------------------------------------------------------------
function GetTempDirectory: string;
begin
  Result := IncludeTrailingPathDelimiter( UTF8ToString( TNSString.Wrap( NSTemporaryDirectory( ) ).stringByExpandingTildeInPath.UTF8String ) );
end;

// ------------------------------------------------------------------------------
function TColorToUIColor( const Color: TAlphaColor; Alpha: Single ): UIColor; overload;
begin
  if Color = TAlphaColors.Null then
    Result := TUIColor.Wrap( TUIColor.OCClass.clearColor )
  else
    Result := TUIColor.Wrap( TUIColor.OCClass.colorWithRed( TAlphaColorRec( Color ).R / 255, TAlphaColorRec( Color ).G / 255, TAlphaColorRec( Color ).B / 255, Alpha ) );
end;

// ------------------------------------------------------------------------------
function TColorToUIColor( const Color: TAlphaColor ): UIColor; overload;
begin
  if Color = TAlphaColors.Null then
    Result := TUIColor.Wrap( TUIColor.OCClass.clearColor )
  else
    Result := TUIColor.Wrap( TUIColor.OCClass.colorWithRed( TAlphaColorRec( Color ).R / 255, TAlphaColorRec( Color ).G / 255, TAlphaColorRec( Color ).B / 255, TAlphaColorRec( Color ).A / 255 ) );
end;

// ------------------------------------------------------------------------------
function TColorToUIColorPtr( const Color: TAlphaColor; Alpha: Single = 1 ): Pointer;
begin
  Result := TUIColor.OCClass.colorWithRed( TAlphaColorRec( Color ).R / 255, TAlphaColorRec( Color ).G / 255, TAlphaColorRec( Color ).B / 255, Alpha );
end;

// ------------------------------------------------------------------------------
function AddFontFromFile( const FileName: string ): Boolean;
var
  FontData: NSData;
  error   : CFErrorRef;
  provider: CGDataProviderRef;
  font    : CGFontRef;
begin
  FontData := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfFile( NSStr( FileName ) ) );
  provider := CGDataProviderCreateWithCFData( CFDataRef( @FontData ) );
  font     := CGFontCreateWithDataProvider( provider );
  Result   := CTFontManagerRegisterGraphicsFont( font, @error ) > 0;
  CFRelease( font );
  CFRelease( provider );
end;

// ------------------------------------------------------------------------------
function GetTimeZone: integer;
begin
  result := TNSTimeZone.Wrap( TNSTimeZone.OCClass.defaultTimeZone ).secondsFromGMT;
end;

// ------------------------------------------------------------------------------
// From FMX.Notification.iOS
{ function NSDateToDateTime( const ADateTime: NSDate ): TDateTime;
  begin
  Result := ( ADateTime.TimeIntervalSince1970 + GetTimeZone ) / SecsPerDay + EncodeDate( 1970, 1, 1 );
  end; }

function NSDateToDateTime( const ADateTime: NSDate ): TDateTime;
var
  t: double;
begin
  t := ADateTime.timeIntervalSince1970;
  if t < -EncodeDate( 1970, 1, 1 ) * SecsPerDay then
    t    := t - SecsPerDay;
  Result := ( t + GetTimeZone ) / SecsPerDay + EncodeDate( 1970, 1, 1 );
end;

// ------------------------------------------------------------------------------
function DateTimeToNSDate( const ADateTime: TDateTime ): NSDate;
begin
  result := TNSDate.Wrap( TNSDate.OCClass.dateWithTimeIntervalSince1970( DateTimeToUnix( ADateTime ) ) );
end;

// ------------------------------------------------------------------------------
// From FMX.Notification.iOS
function GetGMTDateTime( const ADateTime: TDateTime ): TDateTime; overload;
begin
  Result := IncSecond( ADateTime, ( -1 ) * GetTimeZone );
end;

// ------------------------------------------------------------------------------
function GetLocalDateTime( const ADateTime: TDateTime ): TDateTime; overload;
begin
  Result := IncSecond( ADateTime, GetTimeZone );
end;

// ------------------------------------------------------------------------------
function GetGMTDateTime( const ADateTime: TDateTime; TimeZone: integer ): TDateTime; overload;
begin
  if GetTimeZone > 0 then
    Result := ADateTime - EncodeTime( TimeZone, 0, 0, 0 )
  else
    Result := ADateTime + EncodeTime( Abs( TimeZone ), 0, 0, 0 );
end;

// ------------------------------------------------------------------------------
function NSStrToStr( const ASource: NSString ): string;
begin
  if ASource = nil then // SZ: do it the same way as Delphi NSStrToStr (ASource could be nil if a string is empty)
    Exit( '' )
  else
    Result := UTF8ToString( ASource.UTF8String );
end;

// ------------------------------------------------------------------------------
function longitudeToPixelSpaceX( longitude: double ): double;
begin
  if longitude > 90.0 then
    longitude := 90;
  if longitude < -90.0 then
    longitude := -90;

  Result := MERCATOR_OFFSET;
  if not IsNan( longitude ) then
    Result := Round( MERCATOR_OFFSET + MERCATOR_RADIUS * longitude * PI / 180.0 );
end;

// ------------------------------------------------------------------------------
function latitudeToPixelSpaceY( latitude: double ): double;
begin
  if latitude > 180.0 then
    latitude := 180.0;
  if latitude < -180.0 then
    latitude := -180.0;

  Result := MERCATOR_OFFSET;
  if not IsNan( latitude ) then
    Result := Round( MERCATOR_OFFSET - MERCATOR_RADIUS * ln( ( 1 + sin( latitude * PI / 180.0 ) ) / ( 1 - sin( latitude * PI / 180.0 ) ) ) / 2.0 );
end;

// ------------------------------------------------------------------------------
function pixelSpaceXToLongitude( pixelX: double ): double;
begin
  Result := ( ( Round( pixelX ) - MERCATOR_OFFSET ) / MERCATOR_RADIUS ) * 180.0 / PI;
end;

// ------------------------------------------------------------------------------
function pixelSpaceYToLatitude( pixelY: double ): double;
begin
  Result := ( PI / 2.0 - 2.0 * tan( exp( ( Round( pixelY ) - MERCATOR_OFFSET ) / MERCATOR_RADIUS ) ) ) * 180.0 / PI;
end;

// ------------------------------------------------------------------------------
function UIImageToBase64( const AImage: UIImage ): string;
var
  BMP    : TBitMap;
  InputS : TMemoryStream;
  OutputS: TStringStream;
begin
  BMP     := UIImageToBitmap( AImage, 0 );
  InputS  := TMemoryStream.Create;
  OutputS := TStringStream.Create;
  BMP.SaveToStream( InputS );
  BMP.Free;
  InputS.Position := 0;
  EncodeStream( InputS, OutputS );
  Result := OutputS.DataString;
  InputS.Free;
  OutputS.Free;
end;

// ------------------------------------------------------------------------------
function UIImageToBitmap( const AImage: UIImage; ARotate: Single ): TBitmap;

  function ReduceImageSize( const AOriginalSize: TSize ): TSize;
  var
    ScaleCoef      : Single;
    LMaxTextureSize: Integer;
  begin
{$IFDEF DELPHIXE7}
    LMaxTextureSize := TContextManager.DefaultContextClass.MaxTextureSize;
{$ELSE}
    LMaxTextureSize := TContext3D.MaxTextureSize;
{$ENDIF}
    Result := AOriginalSize;
    if Max( AOriginalSize.cx, AOriginalSize.cy ) div LMaxTextureSize > 0 then
    begin
      ScaleCoef := Max( AOriginalSize.cx, AOriginalSize.cy ) / LMaxTextureSize;
      Result    := TSize.Create( Round( AOriginalSize.cx / ScaleCoef ), Round( AOriginalSize.cy / ScaleCoef ) );
    end;
  end;

var
  ImageRef  : CGImageRef;
  Bitmap    : TBitmap;
  CtxRef    : CGContextRef;
  ColorSpace: CGColorSpaceRef;
  Data      : TBitmapData;
  BitmapSize: TSize;
begin
  ImageRef := AImage.CGImage;
  if Assigned( ImageRef ) then
  begin
    BitmapSize := ReduceImageSize( TSize.Create( CGImageGetWidth( ImageRef ), CGImageGetHeight( ImageRef ) ) );
    Bitmap     := TBitmap.Create( BitmapSize.cx, BitmapSize.cy );
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      if Bitmap.Map( TMapAccess.Write, Data ) then
      begin
        CtxRef := CGBitmapContextCreate( Data.Data, Bitmap.Width, Bitmap.Height, 8, 4 * Bitmap.Width, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big );
        try
          CGContextDrawImage( CtxRef, CGRectMake( 0, 0, Bitmap.Width, Bitmap.Height ), ImageRef );
        finally
          CGContextRelease( CtxRef );
        end;
      end;
    finally
      CGColorSpaceRelease( ColorSpace );
    end;
    Bitmap.Rotate( ARotate );
    Result := Bitmap;
  end
  else
    Result := nil;
end;

// ------------------------------------------------------------------------------
function BitmapToUIImage( const Bitmap: TBitmap ): UIImage;
var
  ImageRef  : CGImageRef;
  CtxRef    : CGContextRef;
  ColorSpace: CGColorSpaceRef;
  BitmapData: TBitmapData;
begin
  if Bitmap.IsEmpty then
    Result := TUIImage.Create
  else
  begin
    ColorSpace := CGColorSpaceCreateDeviceRGB;
    try
      if Bitmap.Map( TMapAccess.Read, BitmapData ) then
      begin
        CtxRef := CGBitmapContextCreate( BitmapData.Data, Bitmap.Width, Bitmap.Height, 8, 4 * Bitmap.Width, ColorSpace, kCGImageAlphaPremultipliedLast or kCGBitmapByteOrder32Big );
        try
          ImageRef := CGBitmapContextCreateImage( CtxRef );
          try
            Result := TUIImage.Wrap( TUIImage.Alloc.initWithCGImage( ImageRef ) );

          finally
            CGImageRelease( ImageRef );
          end;
        finally
          CGContextRelease( CtxRef );
        end;
      end;
    finally
      CGColorSpaceRelease( ColorSpace );
    end;
  end;
end;

// ------------------------------------------------------------------------------
function MemoryStreamToUIImage( const memStream: TMemoryStream ): UIImage;
var
  data: NSData;
begin
  Result             := nil;
  memStream.Position := 0;
  data               := TNSData.Wrap( TNSData.alloc.initWithBytesNoCopy( memStream.Memory, memStream.Size, False ) );
  try
    if data.length > 0 then
      Result := TUIImage.Wrap( TUIImage.alloc.initWithData( data ) );
  finally
    data.release;
  end;
end;

// ------------------------------------------------------------------------------
function GetFileMIMEType( const AFileName: string ): string;
var
  LExt: string;
begin
  LExt := LowerCase( ExtractFileExt( AFileName ) );

  Result := MIMETableList.Values[LExt];
  if Result = '' then
    Result := 'application/octet-stream'
end;

// ------------------------------------------------------------------------------
procedure SaveBitmapInAlbum( const BitMap: TBitmap );
var
  Image: UIImage;
begin
  if not Assigned( Bitmap ) then
    exit;
  Image := BitmapToUIImage( BitMap );
  UIImageWriteToSavedPhotosAlbum( ( Image as ILocalObject ).GetObjectID, nil, nil, nil );
end;

// ------------------------------------------------------------------------------
procedure SaveImageInAlbum( const Image: UIImage );
begin
  if Assigned( image ) and ( image.size.width > 0.99 ) then
    UIImageWriteToSavedPhotosAlbum( ( Image as ILocalObject ).GetObjectID, nil, nil, nil );
end;

// ------------------------------------------------------------------------------
// Extracted From Indy.
procedure FillMimeTable;
begin
  if not Assigned( MIMETableList ) or ( MIMETableList.Count > 0 ) then
    Exit;

  // Animation
  MIMETableList.Add( '.nml=animation/narrative' );

  // Audio
  MIMETableList.Add( '.aac=audio/mp4' );
  MIMETableList.Add( '.aif=audio/x-aiff' );
  MIMETableList.Add( '.aifc=audio/x-aiff' );
  MIMETableList.Add( '.aiff=audio/x-aiff' );

  MIMETableList.Add( '.au=audio/basic' );
  MIMETableList.Add( '.gsm=audio/x-gsm' );
  MIMETableList.Add( '.kar=audio/midi' );
  MIMETableList.Add( '.m3u=audio/mpegurl' );
  MIMETableList.Add( '.m4a=audio/x-mpg' );
  MIMETableList.Add( '.mid=audio/midi' );
  MIMETableList.Add( '.midi=audio/midi' );
  MIMETableList.Add( '.mpega=audio/x-mpg' );
  MIMETableList.Add( '.mp2=audio/x-mpg' );
  MIMETableList.Add( '.mp3=audio/x-mpg' );
  MIMETableList.Add( '.mpga=audio/x-mpg' );
  MIMETableList.Add( '.m3u=audio/x-mpegurl' );
  MIMETableList.Add( '.pls=audio/x-scpls' );
  MIMETableList.Add( '.qcp=audio/vnd.qcelp' );
  MIMETableList.Add( '.ra=audio/x-realaudio' );
  MIMETableList.Add( '.ram=audio/x-pn-realaudio' );
  MIMETableList.Add( '.rm=audio/x-pn-realaudio' );
  MIMETableList.Add( '.sd2=audio/x-sd2' );
  MIMETableList.Add( '.sid=audio/prs.sid' );
  MIMETableList.Add( '.snd=audio/basic' );
  MIMETableList.Add( '.wav=audio/x-wav' );
  MIMETableList.Add( '.wax=audio/x-ms-wax' );
  MIMETableList.Add( '.wma=audio/x-ms-wma' );

  MIMETableList.Add( '.mjf=audio/x-vnd.AudioExplosion.MjuiceMediaFile' );

  // Image
  MIMETableList.Add( '.art=image/x-jg' );
  MIMETableList.Add( '.bmp=image/bmp' );
  MIMETableList.Add( '.cdr=image/x-coreldraw' );
  MIMETableList.Add( '.cdt=image/x-coreldrawtemplate' );
  MIMETableList.Add( '.cpt=image/x-corelphotopaint' );
  MIMETableList.Add( '.djv=image/vnd.djvu' );
  MIMETableList.Add( '.djvu=image/vnd.djvu' );
  MIMETableList.Add( '.gif=image/gif' );
  MIMETableList.Add( '.ief=image/ief' );
  MIMETableList.Add( '.ico=image/x-icon' );
  MIMETableList.Add( '.jng=image/x-jng' );
  MIMETableList.Add( '.jpg=image/jpeg' );
  MIMETableList.Add( '.jpeg=image/jpeg' );
  MIMETableList.Add( '.jpe=image/jpeg' );
  MIMETableList.Add( '.pat=image/x-coreldrawpattern' );
  MIMETableList.Add( '.pcx=image/pcx' );
  MIMETableList.Add( '.pbm=image/x-portable-bitmap' );
  MIMETableList.Add( '.pgm=image/x-portable-graymap' );
  MIMETableList.Add( '.pict=image/x-pict' );
  MIMETableList.Add( '.png=image/x-png' );
  MIMETableList.Add( '.pnm=image/x-portable-anymap' );
  MIMETableList.Add( '.pntg=image/x-macpaint' );
  MIMETableList.Add( '.ppm=image/x-portable-pixmap' );
  MIMETableList.Add( '.psd=image/x-psd' );
  MIMETableList.Add( '.qtif=image/x-quicktime' );
  MIMETableList.Add( '.ras=image/x-cmu-raster' );
  MIMETableList.Add( '.rf=image/vnd.rn-realflash' );
  MIMETableList.Add( '.rgb=image/x-rgb' );
  MIMETableList.Add( '.rp=image/vnd.rn-realpix' );
  MIMETableList.Add( '.sgi=image/x-sgi' );
  MIMETableList.Add( '.svg=image/svg-xml' );
  MIMETableList.Add( '.svgz=image/svg-xml' );
  MIMETableList.Add( '.targa=image/x-targa' );
  MIMETableList.Add( '.tif=image/x-tiff' );
  MIMETableList.Add( '.wbmp=image/vnd.wap.wbmp' );
  MIMETableList.Add( '.webp=image/webp' );
  MIMETableList.Add( '.xbm=image/xbm' );
  MIMETableList.Add( '.xbm=image/x-xbitmap' );
  MIMETableList.Add( '.xpm=image/x-xpixmap' );
  MIMETableList.Add( '.xwd=image/x-xwindowdump' );

  // Text
  MIMETableList.Add( '.323=text/h323' );
  MIMETableList.Add( '.xml=text/xml' );
  MIMETableList.Add( '.uls=text/iuls' );
  MIMETableList.Add( '.txt=text/plain' );
  MIMETableList.Add( '.rtx=text/richtext' );
  MIMETableList.Add( '.wsc=text/scriptlet' );
  MIMETableList.Add( '.rt=text/vnd.rn-realtext' );
  MIMETableList.Add( '.htt=text/webviewhtml' );
  MIMETableList.Add( '.htc=text/x-component' );
  MIMETableList.Add( '.vcf=text/x-vcard' );

  // Video
  MIMETableList.Add( '.asf=video/x-ms-asf' );
  MIMETableList.Add( '.asx=video/x-ms-asf' );
  MIMETableList.Add( '.avi=video/x-msvideo' );
  MIMETableList.Add( '.dl=video/dl' );
  MIMETableList.Add( '.dv=video/dv' );
  MIMETableList.Add( '.flc=video/flc' );
  MIMETableList.Add( '.fli=video/fli' );
  MIMETableList.Add( '.gl=video/gl' );
  MIMETableList.Add( '.lsf=video/x-la-asf' );
  MIMETableList.Add( '.lsx=video/x-la-asf' );
  MIMETableList.Add( '.mng=video/x-mng' );

  MIMETableList.Add( '.mp2=video/mpeg' );
  MIMETableList.Add( '.mp3=video/mpeg' );
  MIMETableList.Add( '.mp4=video/mp4' );
  MIMETableList.Add( '.mpeg=video/x-mpeg2a' );
  MIMETableList.Add( '.mpa=video/mpeg' );
  MIMETableList.Add( '.mpe=video/mpeg' );
  MIMETableList.Add( '.mpg=video/mpeg' );
  MIMETableList.Add( '.ogv=video/ogg' );
  MIMETableList.Add( '.moov=video/quicktime' );
  MIMETableList.Add( '.mov=video/quicktime' );
  MIMETableList.Add( '.mxu=video/vnd.mpegurl' );
  MIMETableList.Add( '.qt=video/quicktime' );
  MIMETableList.Add( '.qtc=video/x-qtc' );
  MIMETableList.Add( '.rv=video/vnd.rn-realvideo' );
  MIMETableList.Add( '.ivf=video/x-ivf' );
  MIMETableList.Add( '.webm=video/webm' );
  MIMETableList.Add( '.wm=video/x-ms-wm' );
  MIMETableList.Add( '.wmp=video/x-ms-wmp' );
  MIMETableList.Add( '.wmv=video/x-ms-wmv' );
  MIMETableList.Add( '.wmx=video/x-ms-wmx' );
  MIMETableList.Add( '.wvx=video/x-ms-wvx' );
  MIMETableList.Add( '.rms=video/vnd.rn-realvideo-secure' );
  MIMETableList.Add( '.asx=video/x-ms-asf-plugin' );
  MIMETableList.Add( '.movie=video/x-sgi-movie' );

  // Application
  MIMETableList.Add( '.7z=application/x-7z-compressed' );
  MIMETableList.Add( '.a=application/x-archive' );
  MIMETableList.Add( '.aab=application/x-authorware-bin' );
  MIMETableList.Add( '.aam=application/x-authorware-map' );
  MIMETableList.Add( '.aas=application/x-authorware-seg' );
  MIMETableList.Add( '.abw=application/x-abiword' );
  MIMETableList.Add( '.ace=application/x-ace-compressed' );
  MIMETableList.Add( '.ai=application/postscript' );
  MIMETableList.Add( '.alz=application/x-alz-compressed' );
  MIMETableList.Add( '.ani=application/x-navi-animation' );
  MIMETableList.Add( '.arj=application/x-arj' );
  MIMETableList.Add( '.asf=application/vnd.ms-asf' );
  MIMETableList.Add( '.bat=application/x-msdos-program' );
  MIMETableList.Add( '.bcpio=application/x-bcpio' );
  MIMETableList.Add( '.boz=application/x-bzip2' );
  MIMETableList.Add( '.bz=application/x-bzip' );
  MIMETableList.Add( '.bz2=application/x-bzip2' );
  MIMETableList.Add( '.cab=application/vnd.ms-cab-compressed' );
  MIMETableList.Add( '.cat=application/vnd.ms-pki.seccat' );
  MIMETableList.Add( '.ccn=application/x-cnc' );
  MIMETableList.Add( '.cco=application/x-cocoa' );
  MIMETableList.Add( '.cdf=application/x-cdf' );
  MIMETableList.Add( '.cer=application/x-x509-ca-cert' );
  MIMETableList.Add( '.chm=application/vnd.ms-htmlhelp' );
  MIMETableList.Add( '.chrt=application/vnd.kde.kchart' );
  MIMETableList.Add( '.cil=application/vnd.ms-artgalry' );
  MIMETableList.Add( '.class=application/java-vm' );
  MIMETableList.Add( '.com=application/x-msdos-program' );
  MIMETableList.Add( '.clp=application/x-msclip' );
  MIMETableList.Add( '.cpio=application/x-cpio' );
  MIMETableList.Add( '.cpt=application/mac-compactpro' );
  MIMETableList.Add( '.cqk=application/x-calquick' );
  MIMETableList.Add( '.crd=application/x-mscardfile' );
  MIMETableList.Add( '.crl=application/pkix-crl' );
  MIMETableList.Add( '.csh=application/x-csh' );
  MIMETableList.Add( '.dar=application/x-dar' );
  MIMETableList.Add( '.dbf=application/x-dbase' );
  MIMETableList.Add( '.dcr=application/x-director' );
  MIMETableList.Add( '.deb=application/x-debian-package' );
  MIMETableList.Add( '.dir=application/x-director' );
  MIMETableList.Add( '.dist=vnd.apple.installer+xml' );
  MIMETableList.Add( '.distz=vnd.apple.installer+xml' );
  MIMETableList.Add( '.dll=application/x-msdos-program' );
  MIMETableList.Add( '.dmg=application/x-apple-diskimage' );
  MIMETableList.Add( '.doc=application/msword' );
  MIMETableList.Add( '.dot=application/msword' );
  MIMETableList.Add( '.dvi=application/x-dvi' );
  MIMETableList.Add( '.dxr=application/x-director' );
  MIMETableList.Add( '.ebk=application/x-expandedbook' );
  MIMETableList.Add( '.eps=application/postscript' );
  MIMETableList.Add( '.evy=application/envoy' );
  MIMETableList.Add( '.exe=application/x-msdos-program' );
  MIMETableList.Add( '.fdf=application/vnd.fdf' );
  MIMETableList.Add( '.fif=application/fractals' );
  MIMETableList.Add( '.flm=application/vnd.kde.kivio' );
  MIMETableList.Add( '.fml=application/x-file-mirror-list' );
  MIMETableList.Add( '.gzip=application/x-gzip' );
  MIMETableList.Add( '.gnumeric=application/x-gnumeric' );
  MIMETableList.Add( '.gtar=application/x-gtar' );
  MIMETableList.Add( '.gz=application/x-gzip' );
  MIMETableList.Add( '.hdf=application/x-hdf' );
  MIMETableList.Add( '.hlp=application/winhlp' );
  MIMETableList.Add( '.hpf=application/x-icq-hpf' );
  MIMETableList.Add( '.hqx=application/mac-binhex40' );
  MIMETableList.Add( '.hta=application/hta' );
  MIMETableList.Add( '.ims=application/vnd.ms-ims' );
  MIMETableList.Add( '.ins=application/x-internet-signup' );
  MIMETableList.Add( '.iii=application/x-iphone' );
  MIMETableList.Add( '.iso=application/x-iso9660-image' );
  MIMETableList.Add( '.jar=application/java-archive' );
  MIMETableList.Add( '.karbon=application/vnd.kde.karbon' );
  MIMETableList.Add( '.kfo=application/vnd.kde.kformula' );
  MIMETableList.Add( '.kon=application/vnd.kde.kontour' );
  MIMETableList.Add( '.kpr=application/vnd.kde.kpresenter' );
  MIMETableList.Add( '.kpt=application/vnd.kde.kpresenter' );
  MIMETableList.Add( '.kwd=application/vnd.kde.kword' );
  MIMETableList.Add( '.kwt=application/vnd.kde.kword' );
  MIMETableList.Add( '.latex=application/x-latex' );
  MIMETableList.Add( '.lha=application/x-lzh' );
  MIMETableList.Add( '.lcc=application/fastman' );
  MIMETableList.Add( '.lrm=application/vnd.ms-lrm' );
  MIMETableList.Add( '.lz=application/x-lzip' );
  MIMETableList.Add( '.lzh=application/x-lzh' );
  MIMETableList.Add( '.lzma=application/x-lzma' );
  MIMETableList.Add( '.lzo=application/x-lzop' );
  MIMETableList.Add( '.lzx=application/x-lzx' );
  MIMETableList.Add( '.m13=application/x-msmediaview' );
  MIMETableList.Add( '.m14=application/x-msmediaview' );
  MIMETableList.Add( '.mpp=application/vnd.ms-project' );
  MIMETableList.Add( '.mvb=application/x-msmediaview' );
  MIMETableList.Add( '.man=application/x-troff-man' );
  MIMETableList.Add( '.mdb=application/x-msaccess' );
  MIMETableList.Add( '.me=application/x-troff-me' );
  MIMETableList.Add( '.ms=application/x-troff-ms' );
  MIMETableList.Add( '.msi=application/x-msi' );
  MIMETableList.Add( '.mpkg=vnd.apple.installer+xml' );
  MIMETableList.Add( '.mny=application/x-msmoney' );
  MIMETableList.Add( '.nix=application/x-mix-transfer' );
  MIMETableList.Add( '.o=application/x-object' );
  MIMETableList.Add( '.oda=application/oda' );
  MIMETableList.Add( '.odb=application/vnd.oasis.opendocument.database' );
  MIMETableList.Add( '.odc=application/vnd.oasis.opendocument.chart' );
  MIMETableList.Add( '.odf=application/vnd.oasis.opendocument.formula' );
  MIMETableList.Add( '.odg=application/vnd.oasis.opendocument.graphics' );
  MIMETableList.Add( '.odi=application/vnd.oasis.opendocument.image' );
  MIMETableList.Add( '.odm=application/vnd.oasis.opendocument.text-master' );
  MIMETableList.Add( '.odp=application/vnd.oasis.opendocument.presentation' );
  MIMETableList.Add( '.ods=application/vnd.oasis.opendocument.spreadsheet' );
  MIMETableList.Add( '.ogg=application/ogg' );
  MIMETableList.Add( '.odt=application/vnd.oasis.opendocument.text' );
  MIMETableList.Add( '.otg=application/vnd.oasis.opendocument.graphics-template' );
  MIMETableList.Add( '.oth=application/vnd.oasis.opendocument.text-web' );
  MIMETableList.Add( '.otp=application/vnd.oasis.opendocument.presentation-template' );
  MIMETableList.Add( '.ots=application/vnd.oasis.opendocument.spreadsheet-template' );
  MIMETableList.Add( '.ott=application/vnd.oasis.opendocument.text-template' );
  MIMETableList.Add( '.p10=application/pkcs10' );
  MIMETableList.Add( '.p12=application/x-pkcs12' );
  MIMETableList.Add( '.p7b=application/x-pkcs7-certificates' );
  MIMETableList.Add( '.p7m=application/pkcs7-mime' );
  MIMETableList.Add( '.p7r=application/x-pkcs7-certreqresp' );
  MIMETableList.Add( '.p7s=application/pkcs7-signature' );
  MIMETableList.Add( '.package=application/vnd.autopackage' );
  MIMETableList.Add( '.pfr=application/font-tdpfr' );
  MIMETableList.Add( '.pkg=vnd.apple.installer+xml' );
  MIMETableList.Add( '.pdf=application/pdf' );
  MIMETableList.Add( '.pko=application/vnd.ms-pki.pko' );
  MIMETableList.Add( '.pl=application/x-perl' );
  MIMETableList.Add( '.pnq=application/x-icq-pnq' );
  MIMETableList.Add( '.pot=application/mspowerpoint' );
  MIMETableList.Add( '.pps=application/mspowerpoint' );
  MIMETableList.Add( '.ppt=application/mspowerpoint' );
  MIMETableList.Add( '.ppz=application/mspowerpoint' );
  MIMETableList.Add( '.ps=application/postscript' );
  MIMETableList.Add( '.pub=application/x-mspublisher' );
  MIMETableList.Add( '.qpw=application/x-quattropro' );
  MIMETableList.Add( '.qtl=application/x-quicktimeplayer' );
  MIMETableList.Add( '.rar=application/rar' );
  MIMETableList.Add( '.rdf=application/rdf+xml' );
  MIMETableList.Add( '.rjs=application/vnd.rn-realsystem-rjs' );
  MIMETableList.Add( '.rm=application/vnd.rn-realmedia' );
  MIMETableList.Add( '.rmf=application/vnd.rmf' );
  MIMETableList.Add( '.rmp=application/vnd.rn-rn_music_package' );
  MIMETableList.Add( '.rmx=application/vnd.rn-realsystem-rmx' );
  MIMETableList.Add( '.rnx=application/vnd.rn-realplayer' );
  MIMETableList.Add( '.rpm=application/x-redhat-package-manager' );
  MIMETableList.Add( '.rsml=application/vnd.rn-rsml' );
  MIMETableList.Add( '.rtsp=application/x-rtsp' );
  MIMETableList.Add( '.rss=application/rss+xml' );
  MIMETableList.Add( '.scm=application/x-icq-scm' );
  MIMETableList.Add( '.ser=application/java-serialized-object' );
  MIMETableList.Add( '.scd=application/x-msschedule' );
  MIMETableList.Add( '.sda=application/vnd.stardivision.draw' );
  MIMETableList.Add( '.sdc=application/vnd.stardivision.calc' );
  MIMETableList.Add( '.sdd=application/vnd.stardivision.impress' );
  MIMETableList.Add( '.sdp=application/x-sdp' );
  MIMETableList.Add( '.setpay=application/set-payment-initiation' );
  MIMETableList.Add( '.setreg=application/set-registration-initiation' );
  MIMETableList.Add( '.sh=application/x-sh' );
  MIMETableList.Add( '.shar=application/x-shar' );
  MIMETableList.Add( '.shw=application/presentations' );
  MIMETableList.Add( '.sit=application/x-stuffit' );
  MIMETableList.Add( '.sitx=application/x-stuffitx' );
  MIMETableList.Add( '.skd=application/x-koan' );
  MIMETableList.Add( '.skm=application/x-koan' );
  MIMETableList.Add( '.skp=application/x-koan' );
  MIMETableList.Add( '.skt=application/x-koan' );
  MIMETableList.Add( '.smf=application/vnd.stardivision.math' );
  MIMETableList.Add( '.smi=application/smil' );
  MIMETableList.Add( '.smil=application/smil' );
  MIMETableList.Add( '.spl=application/futuresplash' );
  MIMETableList.Add( '.ssm=application/streamingmedia' );
  MIMETableList.Add( '.sst=application/vnd.ms-pki.certstore' );
  MIMETableList.Add( '.stc=application/vnd.sun.xml.calc.template' );
  MIMETableList.Add( '.std=application/vnd.sun.xml.draw.template' );
  MIMETableList.Add( '.sti=application/vnd.sun.xml.impress.template' );
  MIMETableList.Add( '.stl=application/vnd.ms-pki.stl' );
  MIMETableList.Add( '.stw=application/vnd.sun.xml.writer.template' );
  MIMETableList.Add( '.svi=application/softvision' );
  MIMETableList.Add( '.sv4cpio=application/x-sv4cpio' );
  MIMETableList.Add( '.sv4crc=application/x-sv4crc' );
  MIMETableList.Add( '.swf=application/x-shockwave-flash' );
  MIMETableList.Add( '.swf1=application/x-shockwave-flash' );
  MIMETableList.Add( '.sxc=application/vnd.sun.xml.calc' );
  MIMETableList.Add( '.sxi=application/vnd.sun.xml.impress' );
  MIMETableList.Add( '.sxm=application/vnd.sun.xml.math' );
  MIMETableList.Add( '.sxw=application/vnd.sun.xml.writer' );
  MIMETableList.Add( '.sxg=application/vnd.sun.xml.writer.global' );
  MIMETableList.Add( '.t=application/x-troff' );
  MIMETableList.Add( '.tar=application/x-tar' );
  MIMETableList.Add( '.tcl=application/x-tcl' );
  MIMETableList.Add( '.tex=application/x-tex' );
  MIMETableList.Add( '.texi=application/x-texinfo' );
  MIMETableList.Add( '.texinfo=application/x-texinfo' );
  MIMETableList.Add( '.tbz=application/x-bzip-compressed-tar' );
  MIMETableList.Add( '.tbz2=application/x-bzip-compressed-tar' );
  MIMETableList.Add( '.tgz=application/x-compressed-tar' );
  MIMETableList.Add( '.tlz=application/x-lzma-compressed-tar' );
  MIMETableList.Add( '.tr=application/x-troff' );
  MIMETableList.Add( '.trm=application/x-msterminal' );
  MIMETableList.Add( '.troff=application/x-troff' );
  MIMETableList.Add( '.tsp=application/dsptype' );
  MIMETableList.Add( '.torrent=application/x-bittorrent' );
  MIMETableList.Add( '.ttz=application/t-time' );
  MIMETableList.Add( '.txz=application/x-xz-compressed-tar' );
  MIMETableList.Add( '.udeb=application/x-debian-package' );

  MIMETableList.Add( '.uin=application/x-icq' );
  MIMETableList.Add( '.urls=application/x-url-list' );
  MIMETableList.Add( '.ustar=application/x-ustar' );
  MIMETableList.Add( '.vcd=application/x-cdlink' );
  MIMETableList.Add( '.vor=application/vnd.stardivision.writer' );
  MIMETableList.Add( '.vsl=application/x-cnet-vsl' );
  MIMETableList.Add( '.wcm=application/vnd.ms-works' );
  MIMETableList.Add( '.wb1=application/x-quattropro' );
  MIMETableList.Add( '.wb2=application/x-quattropro' );
  MIMETableList.Add( '.wb3=application/x-quattropro' );
  MIMETableList.Add( '.wdb=application/vnd.ms-works' );
  MIMETableList.Add( '.wks=application/vnd.ms-works' );
  MIMETableList.Add( '.wmd=application/x-ms-wmd' );
  MIMETableList.Add( '.wms=application/x-ms-wms' );
  MIMETableList.Add( '.wmz=application/x-ms-wmz' );
  MIMETableList.Add( '.wp5=application/wordperfect5.1' );
  MIMETableList.Add( '.wpd=application/wordperfect' );
  MIMETableList.Add( '.wpl=application/vnd.ms-wpl' );
  MIMETableList.Add( '.wps=application/vnd.ms-works' );
  MIMETableList.Add( '.wri=application/x-mswrite' );
  MIMETableList.Add( '.xfdf=application/vnd.adobe.xfdf' );
  MIMETableList.Add( '.xls=application/x-msexcel' );
  MIMETableList.Add( '.xlb=application/x-msexcel' );
  MIMETableList.Add( '.xpi=application/x-xpinstall' );
  MIMETableList.Add( '.xps=application/vnd.ms-xpsdocument' );
  MIMETableList.Add( '.xsd=application/vnd.sun.xml.draw' );
  MIMETableList.Add( '.xul=application/vnd.mozilla.xul+xml' );
  MIMETableList.Add( '.z=application/x-compress' );
  MIMETableList.Add( '.zoo=application/x-zoo' );
  MIMETableList.Add( '.zip=application/x-zip-compressed' );

  // WAP
  MIMETableList.Add( '.wbmp=image/vnd.wap.wbmp' );
  MIMETableList.Add( '.wml=text/vnd.wap.wml' );
  MIMETableList.Add( '.wmlc=application/vnd.wap.wmlc' );
  MIMETableList.Add( '.wmls=text/vnd.wap.wmlscript' );
  MIMETableList.Add( '.wmlsc=application/vnd.wap.wmlscriptc' );

  // Non-web text
  MIMETableList.Add( '.asm=text/x-asm' );
  MIMETableList.Add( '.p=text/x-pascal' );
  MIMETableList.Add( '.pas=text/x-pascal' );

  MIMETableList.Add( '.cs=text/x-csharp' );

  MIMETableList.Add( '.c=text/x-csrc' );
  MIMETableList.Add( '.c++=text/x-c++src' );
  MIMETableList.Add( '.cpp=text/x-c++src' );
  MIMETableList.Add( '.cxx=text/x-c++src' );
  MIMETableList.Add( '.cc=text/x-c++src' );
  MIMETableList.Add( '.h=text/x-chdr' );
  MIMETableList.Add( '.h++=text/x-c++hdr' );
  MIMETableList.Add( '.hpp=text/x-c++hdr' );
  MIMETableList.Add( '.hxx=text/x-c++hdr' );
  MIMETableList.Add( '.hh=text/x-c++hdr' );
  MIMETableList.Add( '.java=text/x-java' );

  // WEB
  MIMETableList.Add( '.css=text/css' );
  MIMETableList.Add( '.js=text/javascript' );
  MIMETableList.Add( '.htm=text/html' );
  MIMETableList.Add( '.html=text/html' );
  MIMETableList.Add( '.xhtml=application/xhtml+xml' );
  MIMETableList.Add( '.xht=application/xhtml+xml' );
  MIMETableList.Add( '.rdf=application/rdf+xml' );
  MIMETableList.Add( '.rss=application/rss+xml' );

  MIMETableList.Add( '.ls=text/javascript' );
  MIMETableList.Add( '.mocha=text/javascript' );
  MIMETableList.Add( '.shtml=server-parsed-html' );
  MIMETableList.Add( '.xml=text/xml' );
  MIMETableList.Add( '.sgm=text/sgml' );
  MIMETableList.Add( '.sgml=text/sgml' );

end;

// ------------------------------------------------------------------------------
function String2Hex( const Buffer: string ): string;
var
  n: Integer;
begin
  Result   := '';
  for n    := 1 to Length( Buffer ) - 1 do
    Result := Result + IntToHex( Ord( Buffer[n] ), 2 );
  Result   := LowerCase( Result );
end;

// ------------------------------------------------------------------------------
// AP hash Function
{$OVERFLOWCHECKS OFF}
{$R-}

function GetHash( const Str: string ): int64;
var
  i: Cardinal;
begin
  Result := $AAAAAAAA;
  for i  := 1 to Length( Str ) do
  begin
    if ( ( i - 1 ) and 1 ) = 0 then
      Result := Result xor ( ( Result shl 7 ) xor Ord( Str[i] ) * ( Result shr 3 ) )
    else
      Result := Result xor ( not( ( Result shl 11 ) + Ord( Str[i] ) xor ( Result shr 5 ) ) );
  end;
end;
{$R+}
{$OVERFLOWCHECKS ON}

// ------------------------------------------------------------------------------
function GetHashFileName( URL: string; CreateDirectory: Boolean ): string;
var
  FileExt : string;
  HashFile: Cardinal;
begin
  FileExt  := ExtractFileExt( URL );
  HashFile := ABS( FNV1aHash( URL ) );
  Result   := IncludeTrailingPathDelimiter( GetTempDirectory ) + 'cache' + PathDelim;
  if CreateDirectory and not DirectoryExists( Result ) then
    ForceDirectories( Result );
  Result := Result + IntToStr( HashFile ) + IntToSTr( GetHash( URL ) );
end;

// ------------------------------------------------------------------------------
function FNV1aHash( const S: string ): Cardinal;
var
  LResult: UInt32;
  I      : Integer;
  Temp   : string;
begin
  // Used HashNameMBCS function
  Temp := S.ToUpper; // ToUpperInvariant ????

  LResult := 0;
  for I   := 0 to Temp.Length - 1 do
  begin
    LResult := ( LResult shl 5 ) or ( LResult shr 27 ); // ROL Result, 5
    LResult := LResult xor UInt32( Temp[I] );
  end;
  Result := LResult;
end;

// ------------------------------------------------------------------------------
// Decode Google Map Encoded polyline
// By: Babak yaghoobi
// ------------------------------------------------------------------------------
function PolyDecode( const encoded: string ): TArray<CLLocationCoordinate2D>;
var
  lat, lng                     : integer;
  index, len                   : Integer;
  dlng, dlat, b, shift, _result: Integer;
begin
  index := 0;
  len   := encoded.Length;
  lat   := 0;
  lng   := 0;

  while ( index < len ) do
  begin
    shift   := 0;
    _result := 0;
    repeat
      b := integer( encoded.Chars[index] ) - 63;
      inc( index );
      _result := _result or ( ( b and $1F ) shl shift );
      shift   := shift + 5;
    until not( b >= $20 );
    if ( _result and 1 ) <> 0 then
      dlat := not( _result shr 1 )
    else
      dlat := ( _result shr 1 );
    lat    := lat + dlat;

    shift   := 0;
    _result := 0;
    repeat
      b := integer( encoded.Chars[index] ) - 63;
      inc( index );
      _result := _result or ( ( b and $1F ) shl shift );
      shift   := shift + 5;
    until not( b >= $20 );
    if ( _result and 1 ) <> 0 then
      dlng := not( _result shr 1 )
    else
      dlng := ( _result shr 1 );
    lng    := lng + dlng;
    Setlength( result, Length( result ) + 1 );
    result[high( result )].latitude  := ( lat / 1E5 );
    result[high( result )].longitude := ( lng / 1E5 );
  end;
end;

// ------------------------------------------------------------------------------
function IsNumeric( const AString: string ): Boolean; overload;
var
  I: Integer;
begin
  Result := False;
  for I  := 0 to AString.Length - 1 do
  begin
    if not IsNumeric( AString[i] ) then
    begin
      Exit;
    end;
  end;
  Result := True;
end;

// ------------------------------------------------------------------------------
function IsNumeric( const AChar: Char ): Boolean; inline; overload;
begin
  Result := ( AChar >= '0' ) and ( AChar <= '9' );
end;

// ------------------------------------------------------------------------------
procedure PlaySystemSound( SoundID: UInt32 = $450 );
begin
  AudioServicesPlaySystemSound( SoundID );
end;

// ------------------------------------------------------------------------------
function convertImageToGrayScale( image: UIImage ): UIImage;
var
  imageRect : CGRect;
  colorSpace: CGColorSpaceRef;
  context   : CGContextRef;
  imageRef  : CGImageRef;
begin
  // Create image rectangle with current image width/height
  imageRect := CGRectMake( 0, 0, image.size.width, image.size.height );

  // Grayscale color space
  colorSpace := CGColorSpaceCreateDeviceGray( );

  // Create bitmap content with current image size and grayscale colorspace
  context := CGBitmapContextCreate( nil, round( image.size.width ), round( image.size.height ), 8, 0, colorSpace, kCGImageAlphaNone );

  // Draw image into current context, with specified rectangle
  // using previously defined context (with grayscale colorspace)
  CGContextDrawImage( context, imageRect, image.CGImage );

  // Create bitmap image info from pixel data in current context
  imageRef := CGBitmapContextCreateImage( context );

  // Create a new UIImage object
  result := TUIImage.Wrap( TUIImage.OCClass.imageWithCGImage( imageRef ) );

  // Release colorspace, context and bitmap information
  CGColorSpaceRelease( colorSpace );
  CGContextRelease( context );
  CFRelease( imageRef );

  // Return the new grayscale image
end;

// ------------------------------------------------------------------------------
function GetImageOrientation( const AImage: UIImage ): Single;
begin
  case AImage.imageOrientation of
    UIImageOrientationDown, UIImageOrientationDownMirrored:
      Result := 180;
    UIImageOrientationLeft, UIImageOrientationLeftMirrored:
      Result := -90;
    UIImageOrientationRight, UIImageOrientationRightMirrored:
      Result := 90;
    UIImageOrientationUp, UIImageOrientationUpMirrored:
      Result := 0;
  else
    Result := 0;
  end;
end;

// ------------------------------------------------------------------------------
procedure RemoveFile( FileName: string );
var
  FURL: NSURL;
begin
  if FileName = '' then
    exit;

  FURL := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( FileName ) ) );
  RemoveFile( FURL );
end;

// ------------------------------------------------------------------------------
function MakeScreenshot( view: UIView ): UIImage;
begin
  UIGraphicsBeginImageContextWithOptions( view.bounds.size, false, 0.0 );
  view.layer.renderInContext( UIGraphicsGetCurrentContext( ) );
  result := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
  UIGraphicsEndImageContext( );

  // NSData *imageData = UIImageJPEGRepresentation(imageView, 1.0 ); //you can use PNG too
  // [imageData writeToFile:@"image1.jpeg" atomically:YES];

end;

// ------------------------------------------------------------------------------
procedure RemoveFile( FileURL: NSURL );
var
  fm: NSFileManager;
begin
  fm := TNSFileManager.Wrap( TNSFileManager.Alloc.init );
  try
    if fm.fileExistsAtPath( FileURL.path ) then
    begin
      fm.removeItemAtURL( FileURL, nil );
    end;
  finally
    fm.release;
  end;
  fm := nil;
end;

// ------------------------------------------------------------------------------
function AddLeadingZeroes( const aNumber, Length: integer ): string;
begin
  result := Format( '%.*d', [Length, aNumber] );
end;

// ------------------------------------------------------------------------------
function isURLExists( const URL: string; const timeout: Single = 5.0 ): Integer;
var
  FURLRequest: NSMutableURLRequest;
  FURL       : NSURL;
  response   : PPointer; // NSHTTPURLResponse;
  error      : PPointer;
  data       : NSData;
begin
  Result := -2;
  if URL = '' then
    exit;
  if URL.Contains( 'http://' ) or URL.Contains( 'https://' ) or URL.Contains( 'ftp://' ) then
  begin
    FURL        := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( URL ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding ) ) );
    FURLRequest := TNSMutableURLRequest.Wrap( TNSMutableURLRequest.OCClass.requestWithURL( FURL, NSURLRequestUseProtocolCachePolicy, timeout ) );
    FURLRequest.setHTTPMethod( NSStr( 'HEAD' ) );
    new( response );
    new( error );
    response^ := nil;
    error^    := nil;
    data      := DPF.iOS.Classes.TNSURLConnection.OCClass.sendSynchronousRequest( FURLRequest, response, error );
    if assigned( response ) and assigned( response^ ) then
      Result := TNSHTTPURLResponse.Wrap( response^ ).statusCode// TNSHTTPURLResponse.Wrap( response^ ).statusCode
    else if assigned( error^ ) then
      Result := TNSError.Wrap( error^ ).code;
    Dispose( response );
    Dispose( error );
  end
  else
  begin
    if TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager ).fileExistsAtPath( NSStr( URL ) ) then
      result := 200
    else
      result := 404
  end;
end;

// ------------------------------------------------------------------------------
function GetDeviceID: string;
begin
  result := UTF8ToString( DPF.iOS.Classes.TUIDevice.Wrap( DPF.iOS.Classes.TUIDevice.OCClass.currentDevice ).identifierForVendor.UUIDString.UTF8String );
end;

// ------------------------------------------------------------------------------
function GetUUID( deleteExtra: Boolean = true ): string;
var
  u: NSUUID;
begin
  u      := TNSUUID.Wrap( TNSUUID.OCClass.UUID );
  result := UTF8ToString( u.UUIDString.UTF8String );
  if deleteExtra then
  begin
    result := StringReplace( result, '-', '', [rfReplaceAll] );
    result := StringReplace( result, '{', '', [rfReplaceAll] );
    result := StringReplace( result, '}', '', [rfReplaceAll] );
  end;
end;

// ------------------------------------------------------------------------------
function GenerateRandomChar: Char;
var
  LOrd  : integer;
  LFloat: Double;
begin
  if RandSeed = 0 then
  begin
    Randomize;
  end;
  LFloat := ( Random * 61 ) + 1.5;
  LOrd   := Trunc( LFloat ) + 47;
  if LOrd > 83 then
  begin
    LOrd := LOrd + 13;
  end
  else if LOrd > 57 then
  begin
    Inc( LOrd, 7 );
  end;
  Result := Chr( LOrd );
end;

// ------------------------------------------------------------------------------
function GenerateBoundary: string;
const
  BoundaryLength = 34;
var
  LN    : Integer;
  LFloat: Double;
  EQ    : Integer;
begin
  Result := '';
  repeat
    LFloat := ( Random * ( BoundaryLength - 2 ) ) + 1.5;
    EQ     := Trunc( LFloat );
  until EQ >= BoundaryLength - 5;
  LN := 1;
  while LN < BoundaryLength do
  begin
    if EQ = LN then
    begin
      Result := Result + '=_';
      inc( LN );
    end
    else
      Result := Result + GenerateRandomChar;
    Inc( LN );
  end;

end;

// ------------------------------------------------------------------------------
function GetNSURL( URL: string ): NSURL;
var
  err: PPointer;
begin
  if URL.Contains( 'http://' ) or URL.Contains( 'https://' ) or URL.Contains( 'file://' ) then
    Result := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( URL ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding ) ) )
  else
  begin
    Result := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( URL ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding ) ) );
    new( err );
    if not Result.checkResourceIsReachableAndReturnError( nil ) then
      Result := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( URL ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding ) ) );
    dispose( err );
  end;
end;

// ------------------------------------------------------------------------------
procedure ShowAlert( AMessage: string; ATitle: string = ''; CloseButton: string = 'Ok' );
var
  AlertView: UIAlertView;
begin
  AlertView := TUIAlertView.Wrap( TUIAlertView.Alloc.initWithTitle( NSStr( ATitle ), NSStr( AMessage ), nil, NSSTR( CloseButton ), nil ) );
  AlertView.show;
  AlertView.release;
end;

// ------------------------------------------------------------------------------
function RenameFile( oldFileName: string; newFileName: string ): boolean;
var
  fm: NSFileManager;
begin
  fm     := TNSFileManager.Wrap( TNSFileManager.Alloc.init );
  result := fm.moveItemAtPath( NSStr( oldFileName ), NSStr( newFileName ), nil );
  fm.release;
end;

// ------------------------------------------------------------------------------

{$ENDIF}

procedure DisposeOfAndNil( var Obj ); // SZ added
// careful: DisposeOf does not set Obj to nil, which means that then owning objects will not be freed until Obj goes completely out of scope
{$IFDEF AUTOREFCOUNT}
var
  a: TObject;
begin
  pointer( a )   := pointer( Obj );
  pointer( Obj ) := nil;
  a.DisposeOf;
{$ELSE}
begin
  System.SysUtils.FreeAndNil( Obj );
{$ENDIF}
end;

{$IFDEF IOS}

initialization

MIMETableList := TStringList.Create;
FillMimeTable;

finalization

MIMETableList.Free;
{$ENDIF}

end.
