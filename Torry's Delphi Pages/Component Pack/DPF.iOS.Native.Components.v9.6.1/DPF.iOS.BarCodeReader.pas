// ------------------------------------------------------------------------------
// DPF.iOS.BarCodeReader Class
//
// Dadeh Pardazane Faragir ( DPF ) Co.
//
// Web: http://www.dpfaragir.com
//
// Developed By: Sebastian Zierer
//
// Email #1:
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
unit DPF.iOS.BarCodeReader;

interface

{$I DPF.iOS.Defs.inc}

uses
  Classes,
  TypInfo,
  FMX.Types,
{$IFDEF IOS}
  iOSApi.Foundation,
  iosAPi.CocoaTypes,
  Macapi.ObjectiveC,
  Macapi.Dispatch,
  iOSApi.AVFoundation,
  iOSApi.UIKit,
  DPF.iOS.Dispatch,
  iOSApi.QuartzCore,
{$ENDIF}
  DPF.iOS.ApplicationManager,
  DPF.iOS.BaseControl;

{$RTTI INHERIT METHODS(DefaultMethodRttiVisibility) PROPERTIES(DefaultPropertyRttiVisibility) FIELDS(DefaultFieldRttiVisibility)}

type
{$IFDEF IOS}
  AVCaptureMetadataOutputClass = interface( AVCaptureOutputClass )
    ['{383ABF7F-4315-4443-BBC6-B93526C11C18}']
  end;

  AVCaptureMetadataOutput = interface( AVCaptureOutput )
    ['{094528E2-3B89-4B6B-92E8-37C962DDEB78}']
    procedure setMetadataObjectTypes( values: NSArray ); cdecl;
    procedure setMetadataObjectsDelegate( objectsDelegate: Pointer; queue: dispatch_queue_t ); cdecl;
    function availableMetadataObjectTypes: NSArray; cdecl;
  end;

  TAVCaptureMetadataOutput = class( TOCGenericImport<AVCaptureMetadataOutputClass, AVCaptureMetadataOutput> )
  end;

  // ------------------------------------------------------------------------------
  ICaptureDelegate = interface( IObjectiveC )
    ['{7332A394-BA9F-4E95-8B00-5526F9198A82}']
    procedure captureOutput( captureOutput: AVCaptureOutput; didOutputMetadataObjects: NSArray; fromConnection: AVCaptureConnection ); cdecl;
  end;

  AVMetadataObjectClass = interface( NSObjectClass )
    ['{AE57FFB3-DA25-4884-8C5B-B0499A373F4A}']
  end;

  AVMetadataObject = interface( NSObject )
    ['{3EE159DA-B4CF-4956-AF8F-5C0667C2756A}']
    function &type: NSString; cdecl;
  end;

  // ------------------------------------------------------------------------------
  TAVMetadataObject = class( TOCGenericImport<AVMetadataObjectClass, AVMetadataObject> )
  end;

  AVMetadataMachineReadableCodeObjectClass = interface( AVMetadataObjectClass )
    ['{E9F9C797-87A7-4C5B-B445-4096EA169301}']
  end;

  AVMetadataMachineReadableCodeObject = interface( AVMetadataObject )
    ['{A343EC90-7BB0-4E2C-9B06-B2ED7355D72A}']
    function corners: NSArray; cdecl;
    function stringValue: NSString; cdecl;
  end;

  TAVMetadataMachineReadableCodeObject = class( TOCGenericImport<AVMetadataMachineReadableCodeObjectClass, AVMetadataMachineReadableCodeObject> )
  end;

  // ------------------------------------------------------------------------------
  AVCaptureVideoPreviewLayerClass = interface( CALayerClass )
    ['{0F570D38-C338-4EAB-926C-8036ABC0E991}']
    { class } function layerWithSession( session: AVCaptureSession ): Pointer; cdecl;
  end;

  AVCaptureVideoPreviewLayer = interface( CALayer )
    ['{ADD0E96E-F021-4C6A-B77F-EFE51BFF777F}']
    function automaticallyAdjustsMirroring: Boolean; cdecl;
    function initWithSession( session: AVCaptureSession ): Pointer; cdecl;
    function isMirrored: Boolean; cdecl;
    function isMirroringSupported: Boolean; cdecl;
    function isOrientationSupported: Boolean; cdecl;
    function orientation: AVCaptureVideoOrientation; cdecl;
    function session: AVCaptureSession; cdecl;
    procedure setAutomaticallyAdjustsMirroring( automaticallyAdjustsMirroring: Boolean ); cdecl;
    procedure setMirrored( mirrored: Boolean ); cdecl;
    procedure setOrientation( orientation: AVCaptureVideoOrientation ); cdecl;
    procedure setSession( session: AVCaptureSession ); cdecl;
    procedure setVideoGravity( videoGravity: NSString ); cdecl;
    function videoGravity: NSString; cdecl;
    function connection: AVCaptureConnection; cdecl; // @@@ SZ added
  end;

  TAVCaptureVideoPreviewLayer = class( TOCGenericImport<AVCaptureVideoPreviewLayerClass, AVCaptureVideoPreviewLayer> )
  end;

  // ------------------------------------------------------------------------------
  TDPFQRCodeScanner = class;

  // ------------------------------------------------------------------------------
  TCaptureDelegate = class( TOCLocal, ICaptureDelegate )
  private
    FOwner: TDPFQRCodeScanner;
  protected
    procedure DoOnScan( AText: string );
  public
    constructor Create( AOwner: TDPFQRCodeScanner );
    procedure captureOutput( captureOutput: AVCaptureOutput; didOutputMetadataObjects: NSArray; fromConnection: AVCaptureConnection ); cdecl;
  end;

{$ENDIF}

  // ------------------------------------------------------------------------------
  TQRCodeEvent             = procedure( Sender: TObject; AText: string ) of object;
  TQRCodeScannerErrorEvent = procedure( Sender: TObject; _NSError: Pointer ) of object;

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFQRCodeScanner = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FDelegate     : ICaptureDelegate;
    FPreview      : AVCaptureVideoPreviewLayer;
    FSession      : AVCaptureSession;
    FNativeControl: UIView;
{$ENDIF}
    FApplicationManager  : TDPFApplicationManager;
    FViewControllerEvents: TDPFViewControllerEvents;
    FOnScan              : TQRCodeEvent;
    FOnError             : TQRCodeScannerErrorEvent;
    procedure OnRotate( Sender: TObject );
    procedure ViewControllerViewDidAppear( Sender: TObject; Animated: Boolean );
    procedure SetOnScan( const Value: TQRCodeEvent );
    function IsCameraAvailable: Boolean;
    procedure SetOnError( const Value: TQRCodeScannerErrorEvent );
  protected
    procedure CreateUIControl; override;
    procedure DestroyUIControl; override;
    procedure UpdateUIControlPosition; override;
    procedure SetParent( const Value: TFmxObject ); override;
    procedure DoScan( AText: string ); virtual;
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure UpdateOrientation;
  published
    property OnScan : TQRCodeEvent read FOnScan write SetOnScan;
    property OnError: TQRCodeScannerErrorEvent read FOnError write SetOnError;
  end;

implementation

uses
{$IFDEF iOS}
  Macapi.Helpers,
  DPF.iOS.AVFoundationConsts,
  iOSApi.CoreGraphics,
{$ENDIF}
  FMX.Platform,
  Sysutils;

// http://stackoverflow.com/questions/5902705/qr-code-reader-for-iphone
// http://www.ama-dev.com/iphone-qr-code-library-ios-7/

// function dispatch_get_main_queue: dispatch_queue_t; external libdispatch name '_dispatch_main_q';

{$IFDEF IOS}
// function CocoaPointerConst(Fwk: string; ConstStr: string): Pointer;
// var
// FwkMod: HMODULE;
// begin
// Result := nil;
// FwkMod := LoadLibrary(PWideChar(Fwk));
// if FwkMod <> 0 then
// begin
// Result := GetProcAddress(FwkMod, PWideChar(ConstStr));
/// /    FreeLibrary(FwkMod);
// end;
// end;

// ------------------------------------------------------------------------------
// iOS 7.0 and later
function AVMetadataObjectTypeUPCECode: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeUPCECode' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeCode39Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeCode39Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeCode39Mod43Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeCode39Mod43Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeEAN13Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeEAN13Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeEAN8Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeEAN8Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeCode93Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeCode93Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeCode128Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeCode128Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypePDF417Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypePDF417Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeQRCode: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeQRCode' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeAztecCode: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeAztecCode' );
end;

// ------------------------------------------------------------------------------
// iOS 8.0 and later
function AVMetadataObjectTypeInterleaved2of5Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeInterleaved2of5Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeITF14Code: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeITF14Code' );
end;

// ------------------------------------------------------------------------------
function AVMetadataObjectTypeDataMatrixCode: NSString;
begin
  Result := CocoaNSStringConst( libAVFoundation, 'AVMetadataObjectTypeDataMatrixCode' );
end;
{$ENDIF}
{ TDPFQRCodeScanner }

// ------------------------------------------------------------------------------
constructor TDPFQRCodeScanner.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
{$IFDEF IOS}
  FDelegate := TCaptureDelegate.Create( Self );
{$ENDIF}
  FApplicationManager                    := TDPFApplicationManager.Create( Self );
  FApplicationManager.OnInterfaceRotated := OnRotate;
  FViewControllerEvents                  := TDPFViewControllerEvents.Create( Self );
  FViewControllerEvents.OnViewDidAppear  := ViewControllerViewDidAppear;
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.CreateUIControl;
{$IFDEF IOS}
var
  device  : AVCaptureDevice;
  error   : Pointer;
  input   : AVCaptureDeviceInput;
  LOutput : AVCaptureMetadataOutput;
  ObjTypes: NSMutableArray;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FNativeControl ) then
    Exit;

  FNativeControl := TUIView.Create;
  FUIControl     := FNativeControl;

  if IsCameraAvailable then
  begin
    FSession := TAVCaptureSession.Create;
    device   := TAVCaptureDevice.Wrap( TAVCaptureDevice.OCClass.defaultDeviceWithMediaType( AVMediaTypeVideo ) );

    error := nil;
    input := TAVCaptureDeviceInput.Wrap( TAVCaptureDeviceInput.OCClass.deviceInputWithDevice( device, @error ) );
    if Assigned( input ) then
      FSession.addInput( input )
    else
    begin
      if Assigned( FOnError ) and Assigned( error ) then
        FOnError( Self, error );
    end;

    LOutput := TAVCaptureMetadataOutput.Wrap( TAVCaptureMetadataOutput.Wrap( TAVCaptureMetadataOutput.OCClass.alloc ).init );

    FSession.addOutput( LOutput ); // important: must be done before setting metadata object types!

    ObjTypes := TNSMutableArray.Create; // TNSMutableArray.Wrap(TNSMutableArray.OCClass.arrayWithObject((AVMetadataObjectTypeQRCode as ILocalObject).GetObjectID));
    ObjTypes.addObject( ( AVMetadataObjectTypeQRCode as ILocalObject ).GetObjectID );
    // ObjTypes.addObject((AVMetadataObjectTypeCode39Code as ILocalObject).GetObjectID);
    // ObjTypes.addObject((AVMetadataObjectTypeCode39Mod43Code as ILocalObject).GetObjectID);
    // ObjTypes.addObject((AVMetadataObjectTypeCode93Code as ILocalObject).GetObjectID);
    ObjTypes.addObject( ( AVMetadataObjectTypeCode128Code as ILocalObject ).GetObjectID );
    LOutput.setMetadataObjectTypes( ObjTypes );
    LOutput.setMetadataObjectsDelegate( ( FDelegate as ILocalObject ).GetObjectID, dispatch_get_main_queue( ) );

    LOutput.release;
    LOutput := nil;

    Fpreview := TAVCaptureVideoPreviewLayer.Wrap( TAVCaptureVideoPreviewLayer.OCClass.layerWithSession( FSession ) );
    Fpreview.setVideoGravity( AVLayerVideoGravityResizeAspectFill );
    Fpreview.setframe( CGRectMake( 0, 0, FNativeControl.frame.size.width, FNativeControl.frame.size.height ) );

    UpdateOrientation;

    FNativeControl.layer.insertSublayer( FPreview, 0 );
  end;
{$ENDIF}
  inherited CreateUIControl;
end;

// ------------------------------------------------------------------------------
destructor TDPFQRCodeScanner.Destroy;
begin
{$IFDEF IOS}
  FDelegate := nil;
{$ENDIF}
  FApplicationManager.Free;
  FViewControllerEvents.Free;
  inherited;
end;

procedure TDPFQRCodeScanner.DestroyUIControl;
begin
{$IFDEF IOS}
  if Assigned( FSession ) then
  begin
    FSession.release;
    FSession := nil;
  end;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.DoScan( AText: string );
begin
  if Assigned( FOnScan ) then
    FOnScan( Self, AText );
end;

// ------------------------------------------------------------------------------
function TDPFQRCodeScanner.IsCameraAvailable: Boolean;
{$IFDEF IOS}
var
  videoDevices: NSArray;
{$ENDIF}
begin
{$IFDEF IOS}
  videoDevices := TAVCaptureDevice.OCClass.devicesWithMediaType( AVMediaTypeVideo );
  Result       := videoDevices.count > 0;
{$ELSE}
  Result := False;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.OnRotate( Sender: TObject );
begin
  UpdateOrientation;
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.SetOnError( const Value: TQRCodeScannerErrorEvent );
begin
  FOnError := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.SetOnScan( const Value: TQRCodeEvent );
begin
  FOnScan := Value;
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.SetParent( const Value: TFmxObject );
begin
  inherited;

end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.Start;
begin
{$IFDEF IOS}
  if Assigned( FSession ) then
  begin
    FSession.startRunning;
    UpdateUIControlPosition;
    UpdateOrientation;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.Stop;
begin
{$IFDEF IOS}
  if Assigned( FSession ) then
    FSession.stopRunning;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.UpdateOrientation;
var // fmx to video orientation
  ScreenService: IFMXScreenService;
begin
{$IFDEF IOS}
  if TPlatformServices.Current.SupportsPlatformService( IFMXScreenService, ScreenService ) then
  begin
    case ScreenService.GetScreenOrientation of
      TScreenOrientation.InvertedLandscape:
        FPreview.connection.setVideoOrientation( AVCaptureVideoOrientationLandscapeRight );
      TScreenOrientation.Landscape:
        FPreview.connection.setVideoOrientation( AVCaptureVideoOrientationLandscapeLeft );
      TScreenOrientation.Portrait:
        FPreview.connection.setVideoOrientation( AVCaptureVideoOrientationPortrait );
      TScreenOrientation.InvertedPortrait:
        FPreview.connection.setVideoOrientation( AVCaptureVideoOrientationPortraitUpsideDown );
    end;
  end;
  {$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFQRCodeScanner.UpdateUIControlPosition;
begin
  inherited;

{$IFDEF IOS}
  if Assigned( FPreview ) and Assigned( FNativeControl ) then
  begin
    FNativeControl.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );
    Fpreview.setframe( CGRectMake( 0, 0, FNativeControl.frame.size.width, FNativeControl.frame.size.height ) ); // self.view.frame.size.width, self.view.frame.size.height));
    UpdateOrientation;
  end;
{$ENDIF}
end;

procedure TDPFQRCodeScanner.ViewControllerViewDidAppear( Sender: TObject; Animated: Boolean );
begin
  // UpdateUIControlPosition;
end;

{$IFDEF IOS}
{ TCaptureDelegate }

// ------------------------------------------------------------------------------
procedure TCaptureDelegate.captureOutput( captureOutput: AVCaptureOutput; didOutputMetadataObjects: NSArray; fromConnection: AVCaptureConnection );
var
  QRCode  : NSString;
  metadata: AVMetadataObject;
  I       : Integer;
begin
  for I := 0 to didOutputMetadataObjects.count - 1 do
  begin
    metadata := TAVMetadataObject.Wrap( didOutputMetadataObjects.objectAtIndex( I ) );
    if metadata.&type.isEqualToString( AVMetadataObjectTypeQRCode ) then
    begin
      QRCode := TAVMetadataMachineReadableCodeObject.Wrap( ( metadata as ILocalObject ).GetObjectID ).stringValue;
      DoOnScan( NSStrToStr( QRCode ) );
      break;
    end;
  end;

  // NSLog(@"QR Code: %@", QRCode);
end;

// ------------------------------------------------------------------------------
constructor TCaptureDelegate.Create( AOwner: TDPFQRCodeScanner );
begin
  FOwner := AOwner;
  inherited Create;
end;

// ------------------------------------------------------------------------------
procedure TCaptureDelegate.DoOnScan( AText: string );
begin
  if Assigned( FOwner ) then
    FOwner.DoScan( AText );
end;
{$ENDIF}

end.
