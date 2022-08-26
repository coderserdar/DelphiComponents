// ------------------------------------------------------------------------------
// DPF.iOS.UIImageView Component
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
unit DPF.iOS.UIImageView;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,
  System.DateUtils,
  System.Math,
  System.TypInfo,

  DPF.iOS.BaseControl,
  DPF.iOS.HTTP,
  DPF.iOS.UILabel,
  DPF.iOS.UITextView,
  DPF.iOS.NSOperationQueue,
  DPF.iOS.UIActivityIndicatorView,
  DPF.iOS.CacheManager,
  DPF.iOS.Dispatch,

{$IFDEF IOS}
  Posix.Unistd,
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.CocoaTypes,
  IOSapi.Foundation,
  IOSapi.Uikit,
  IOSapi.CoreGraphics,
  IOSapi.CoreImage,
  IOSapi.QuartzCore,
  DPF.iOS.Common,
  FMX.Platform.iOS,
  DPF.iOS.Classes,
{$ENDIF}
  FMX.Layouts,
  // FMX.Memo,
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

type

  TDPFImageView = class;

{$IFDEF IOS}

  // ------------------------------------------------------------------------------
  DPFUIImageView = interface( UIImageView )
    ['{8EAB6645-704C-45A4-BA80-F90B7A925012}']

    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
  end;

  TDPFUIImageView = class( TOCLocal )
  private
  protected
    FDPFImageView: TDPFImageView;
  public
    constructor Create( ADPFImageView: TDPFImageView );
    function GetObjectiveCClass: PTypeInfo; override;

    procedure singleTap( Sender: pointer ); cdecl;
    procedure doubleTap( Sender: pointer ); cdecl;

    procedure touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
    procedure touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;

  end;

{$ENDIF}

  TDPFOnBeforeAsyncImageLoaded = procedure( Sender: TObject ) of object;
  TDPFOnAsyncImageLoadError    = procedure( Sender: TObject; Error: string ) of object;
  TDPFImageOnProgress          = procedure( Sender: TObject; const DownloadSize: Int64; const DownloadedSize: Int64 ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFImageView = class( TDPFiOSBaseControl )
  private
{$IFDEF IOS}
    FDPFUIImageView         : TDPFUIImageView;
    FUIImageView            : UIImageView;
    FUIActivityIndicatorView: UIActivityIndicatorView;
    ImageLodinggQueue       : dispatch_queue_t;
    ImageSettingQueue       : dispatch_queue_t;

    FOverImage              : string;
    FOverImageAtPointX      : single;
    FOverImageAtPointY      : single;
    FOverImageFileOutSideApp: Boolean;
{$ENDIF}
    FBackgroundColor     : TAlphaColor;
    FImageList           : TStrings;
    FAnimationDuration   : Double;
    FAnimationRepeatCount: Integer;
    FBitmap              : TBitMap;
    FLoadFromURL         : Boolean;
    FLoadAsync           : Boolean;
    FShowIndicator       : Boolean;
    FIndicatorStyle      : TDPFActivityIndicatorViewStyle;

    FOnClick                 : TDPFOnClicked;
    FCacheImage              : Boolean;
    FOperationQueue          : TDPFNSOperationQueue;
    FRetryCount              : Integer;
    FRetryLoading            : Integer;
    FOnAsyncImageLoaded      : TNotifyEvent;
    FOnAsyncImageLoadError   : TDPFOnAsyncImageLoadError;
    FOnBeforeAsyncImageLoaded: TDPFOnBeforeAsyncImageLoaded;
    FOnProgress              : TDPFImageOnProgress;
    FGrayScale               : Boolean;
    FFileOutSideApp          : Boolean;
    FIndicatorAlpha          : Single;
    FIndicatorBackgroundColor: TAlphaColor;
    FIndicatorColor          : TAlphaColor;

    FOnTouchesBegan: TDPFTouchesBegan;
    FOnTouchesEnded: TDPFTouchesEnded;
    FOnTouchesMoved: TDPFTouchesMoved;
    FOnDoubleClick : TDPFOnDoubleClicked;
    FDoubleTapDelay: integer;
    FTapDelay      : integer;
    procedure SetBackgroundColor( const Value: TAlphaColor );
    procedure SetImageList( const Value: TStrings );
    procedure SetBitmap( const Value: TBitMap );
    function GetIsLoadingImage: Boolean;
    function GetImageIsLoaded: Boolean;
{$IFDEF IOS}
    procedure LoadImageAsyncURL( URL: string );
    procedure ShowIndicatorView;
    procedure HideIndicatorView;
    function CheckCachedData( HTTPURL: string ): Boolean;
    function addImageOverImage( DestImage: UIImage; OverImage: string; atPointX: single = -1.0; atPointY: single = -1.0; FileOutSideApp: Boolean = false ): UIImage;

{$ENDIF}
  protected
    DPFHttp: TDPFHttp;
    procedure DoBitmapChanged( Sender: TObject );
    procedure Resize; override;
    procedure Move; override;
{$IFNDEF IOS}
    procedure Paint; override;
    procedure Click; override;
{$ELSE}
    procedure OnFinishAsyncLoading( Sender: TObject; Data: NSData; var isFree: Boolean );
    procedure OnErrorAsyncLoading( Sender: TObject; Error: string; var isFree: Boolean );
{$ENDIF}
  public
{$IFDEF IOS}
    function GetUIImage: UIImage;
    procedure Loaded; override;
    procedure ResizeImage( newWidth: Single; newHeight: Single );
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure StopAnimating;
    procedure StartAnimating;
    procedure ClearImage;
    procedure UpdateBitmap;
{$IFDEF IOS}
    procedure ReloadImage;
    function GetColoredImage( Image: UIImage; Color: TAlphaColors ): UIImage;
    // function SetImageBlur( blur: Single ): UIImage;

    procedure SetImage( AImageView: TDPFImageView ); overload;
    procedure SetImage( Image: UIImage; AndRelease: Boolean = false ); overload;
    procedure SetImage( Image: TBitMap ); overload;
    procedure LoadFromFile( FileName: string );
    procedure LoadFromMemoryStream( MemStream: TMemoryStream );
    procedure SaveToFile( FileName: string );
    procedure CancelLoading;
    procedure SetOverImage( OverImage: string; atPointX: single = -1.0; atPointY: single = -1.0; FileOutSideApp: Boolean = false );
{$ENDIF}
    property isLoadingImage: Boolean read GetIsLoadingImage;
    property isImageLoaded: Boolean read GetImageIsLoaded;
  published
    property BackgroundColor     : TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property ImageList           : TStrings read FImageList write SetImageList;
    property AnimationDuration   : Double read FAnimationDuration write FAnimationDuration;
    property AnimationRepeatCount: Integer read FAnimationRepeatCount write FAnimationRepeatCount default 0;
    property Bitmap              : TBitMap read FBitmap write SetBitmap;
    property LoadFromURL         : Boolean read FLoadFromURL write FLoadFromURL default false;
    property CacheImage          : Boolean read FCacheImage write FCacheImage default false;
    property LoadAsync           : Boolean read FLoadAsync write FLoadAsync default false;
    property OperationQueue      : TDPFNSOperationQueue read FOperationQueue write FOperationQueue;
    property RetryLoading        : Integer read FRetryLoading write FRetryLoading default 1;

    property ShowIndicator           : Boolean read FShowIndicator write FShowIndicator default false;
    property IndicatorStyle          : TDPFActivityIndicatorViewStyle read FIndicatorStyle write FIndicatorStyle default aisWhite;
    property IndicatorAlpha          : Single read FIndicatorAlpha write FIndicatorAlpha;
    property IndicatorColor          : TAlphaColor read FIndicatorColor write FIndicatorColor default TAlphaColors.White;
    property IndicatorBackgroundColor: TAlphaColor read FIndicatorBackgroundColor write FIndicatorBackgroundColor default TAlphaColors.Black;

    property GrayScale     : Boolean read FGrayScale write FGrayScale default false;
    property FileOutSideApp: Boolean read FFileOutSideApp write FFileOutSideApp default false;

    property OnAsyncImageLoaded      : TNotifyEvent read FOnAsyncImageLoaded write FOnAsyncImageLoaded;
    property OnBeforeAsyncImageLoaded: TDPFOnBeforeAsyncImageLoaded read FOnBeforeAsyncImageLoaded write FOnBeforeAsyncImageLoaded;
    property OnAsyncImageLoadError   : TDPFOnAsyncImageLoadError read FOnAsyncImageLoadError write FOnAsyncImageLoadError;
    property OnProgress              : TDPFImageOnProgress read FOnProgress write FOnProgress;

    property OnTouchesBegan: TDPFTouchesBegan read FOnTouchesBegan write FOnTouchesBegan;
    property OnTouchesEnded: TDPFTouchesEnded read FOnTouchesEnded write FOnTouchesEnded;
    property OnTouchesMoved: TDPFTouchesMoved read FOnTouchesMoved write FOnTouchesMoved;

    property OnClick      : TDPFOnClicked read FOnClick write FOnClick;
    property OnDoubleClick: TDPFOnDoubleClicked read FOnDoubleClick write FOnDoubleClick;

    property TapDelay      : integer read FTapDelay write FTapDelay default 1;
    property DoubleTapDelay: integer read FDoubleTapDelay write FDoubleTapDelay default 2;

    property AutoresizingMask;
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
{ TDPFImageView }
// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFImageView.Click; // SZ
begin
  inherited;
  if Assigned( FOnClick ) then
    FOnClick( Self );
end;
{$ENDIF}

constructor TDPFImageView.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  FGrayScale                := false;
  FFileOutSideApp           := false;
  FOperationQueue           := nil;
  FImageList                := TStringList.Create;
  FBitmap                   := TBitMap.Create( 0, 0 );
  FBitmap.OnChange          := DoBitmapChanged;
  ControlCaption            := 'ImageView';
  FBackgroundColor          := TAlphaColors.Null;
  FAnimationDuration        := 1;
  FAnimationRepeatCount     := 0;
  FLoadFromURL              := false;
  FLoadAsync                := false;
  FShowIndicator            := false;
  FIndicatorStyle           := aisWhite;
  FIndicatorAlpha           := 0.6;
  FIndicatorBackgroundColor := TAlphaColors.Black;
  FIndicatorColor           := TAlphaColors.White;
  FCacheImage               := false;
  FRetryLoading             := 1;
  FTapDelay                 := 1;
  FDoubleTapDelay           := 2;

{$IFDEF IOS}
  ImageLodinggQueue := dispatch_queue_create( 'ImageView Loding Queue', 0 );
  ImageSettingQueue := dispatch_queue_create( 'ImageView Setting Queue', 0 );
  FDPFUIImageView   := TDPFUIImageView.Create( self );
  FUIImageView      := TUIImageView.Wrap( FDPFUIImageView.Super.init );
  FUIControl        := FUIImageView;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFImageView.Destroy;
begin
  FBitmap.DisposeOf;
  FImageList.DisposeOf;

{$IFDEF IOS}
  if Assigned( FUIActivityIndicatorView ) then
  begin
    FUIActivityIndicatorView.release;
    FUIActivityIndicatorView := nil;
  end;

  CancelLoading;

  if ImageLodinggQueue > 0 then
    dispatch_release( ImageLodinggQueue );

  if ImageSettingQueue > 0 then
    dispatch_release( ImageSettingQueue );

  FDPFUIImageView.Free;

{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.DoBitmapChanged( Sender: TObject );
{$IFDEF IOS}
var
  img: UIImage;
{$ENDIF}
begin
{$IFDEF IOS}
  if Assigned( FUIImageView ) then
  begin
    dispatch_sync( ImageSettingQueue,
      procedure
      begin
        img := BitmapToUIImage( FBitmap );
        if assigned( img ) then
        begin
          FUIImageView.SetImage( img );
          img.release;
        end;
      end );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFImageView.GetImageIsLoaded: Boolean;
begin
{$IFDEF IOS}
  result := Assigned( FUIImageView.image ) and ( FUIImageView.image.size.width > 0.99 );
{$ELSE}
  result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFImageView.GetIsLoadingImage: Boolean;
begin
{$IFDEF IOS}
  result := Assigned( FUIActivityIndicatorView ) and FUIActivityIndicatorView.isAnimating;
{$ELSE}
  result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFImageView.OnErrorAsyncLoading( Sender: TObject; Error: string; var isFree: Boolean );
begin
  CancelLoading;
  isFree := true;

  if FRetryCount < FRetryLoading then
  begin
    inc( FRetryCount );
    LoadImageAsyncURL( FImageList[0] );
  end
  else
  begin
    HideIndicatorView;
  end;
  if ( FRetryCount < FRetryLoading ) and Assigned( OnAsyncImageLoadError ) then
    OnAsyncImageLoadError( Self, Error );
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.OnFinishAsyncLoading( Sender: TObject; Data: NSData; var isFree: Boolean );
var
  ErrStr  : string;
  Img     : UIImage;
  NSResult: NSString;
begin
  HideIndicatorView;
  isFree := true;
  dispatch_sync( ImageLodinggQueue,
    procedure
    var
      isFree: boolean;
    begin
      Img := TUIImage.Wrap( TUIImage.OCClass.imageWithData( Data ) );
      if Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 ) then
      begin
        SetImage( Img );
        if assigned( OnAsyncImageLoaded ) then
          OnAsyncImageLoaded( self );
        CancelLoading;
      end
      else
      begin
        ErrStr := '';
        NSResult := nil;
        if Data.length < 10 * 1024 then
        begin
          NSResult := TNSString.Wrap( TNSString.Alloc.initWithData( Data, NSUTF8StringEncoding ) );
          ErrStr := UTF8ToString( NSResult.UTF8String );
        end;
        // ShowMessage(ErrStr);
        DPFNSLog( 'TDPFImageView.OnFinishAsyncLoading: ' + ErrStr );
        OnErrorAsyncLoading( Self, 'Bad image data : ' + ErrStr, isFree );
        if Assigned( NSResult ) then
          NSResult.release;
      end;
    end );
end;

// ------------------------------------------------------------------------------
function TDPFImageView.GetUIImage: UIImage;
begin
  Result := FUIImageView.image;
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.ResizeImage( newWidth: Single; newHeight: Single );
var
  size                                  : CGSize;
  rectX                                 : CGRect;
  w, h, widthRatio, heightRatio, divisor: Single;
  smallImage                            : PUIImage;
begin

  size.width  := newWidth;
  size.height := newHeight;
  UIGraphicsBeginImageContext( size );
  rectX := CGRectMake( 0, 0, newWidth, newHeight );

  widthRatio  := FUIImageView.image.size.width / newwidth;
  heightRatio := FUIImageView.image.size.height / newheight;
  divisor     := heightRatio;
  if widthRatio > heightRatio then
    divisor := widthRatio;

  w := FUIImageView.image.size.width / divisor;
  h := FUIImageView.image.size.height / divisor;

  rectX.size.width  := w;
  rectX.size.height := h;

  if h < w then
    rectX.origin.y := h / 3;

  FUIImageView.image.drawInRect( rectX );

  smallImage := UIGraphicsGetImageFromCurrentImageContext( );

  UIGraphicsEndImageContext( );
  FUIImageView.setImage( TUIImage.Wrap( smallImage ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.HideIndicatorView;
begin
  if Assigned( FUIActivityIndicatorView ) then
  begin
    FUIActivityIndicatorView.stopAnimating;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.ShowIndicatorView;
begin
  if not FShowIndicator then
    exit;

  if not Assigned( FUIActivityIndicatorView ) then
  begin
    FUIActivityIndicatorView := TUIActivityIndicatorView.Create;
    FUIImageView.addSubview( FUIActivityIndicatorView );
    FUIImageView.bringSubviewToFront( FUIActivityIndicatorView );
    FUIActivityIndicatorView.setHidesWhenStopped( true );
    FUIActivityIndicatorView.setFrame( CGRectMake( 0, 0, Width * Scale.X, Height * Scale.Y ) );
    FUIActivityIndicatorView.setAlpha( FIndicatorAlpha );
    FUIActivityIndicatorView.setActivityIndicatorViewStyle( Integer( IndicatorStyle ) );
    if FIndicatorBackgroundColor <> TAlphaColors.Null then
      FUIActivityIndicatorView.setBackgroundColor( TColorToUIColor( FIndicatorBackgroundColor ) )
    else
      FUIActivityIndicatorView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
    if FIndicatorColor <> TAlphaColors.Null then
      FUIActivityIndicatorView.setColor( TColorToUIColor( FIndicatorColor ) )
    else
      FUIActivityIndicatorView.SetColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
  FUIActivityIndicatorView.startAnimating;
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.LoadImageAsyncURL( URL: string );
begin
  ShowIndicatorView;
  if Assigned( OnBeforeAsyncImageLoaded ) then
    OnBeforeAsyncImageLoaded( Self );

  if not Assigned( DPFHttp ) then
  begin
    DPFHttp                   := TDPFHttp.Create( nil );
    DPFHttp.OnReceiveDataBin  := OnFinishAsyncLoading;
    DPFHttp.OnError           := OnErrorAsyncLoading;
    DPFHttp.OperationQueue    := FOperationQueue;
    DPFHttp.OnReceiveProgress := FOnProgress;
    if not FCacheImage then
      DPFHttp.URLRequestCache := rcReloadIgnoringLocalCacheData;
  end;
  // DPFHttp.SetImageAsync( URL, FUIImageView );
  DPFHttp.GetUrlContentData( URL, [], [], [], True );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFImageView.ClearImage;
begin
{$IFDEF IOS}
  if Assigned( DPFHttp ) then
    DPFHttp.ConnectionCancel;

  dispatch_sync( ImageSettingQueue,
    procedure
    begin
      FUIImageView.setImage( nil );
    end );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFImageView.Loaded;
begin
  DPFNSLog( 'TDPFImageView.Loaded: ' + name );

  FUIImageView.setFrame( CGRectMake( Position.X, Position.Y, Width * Scale.X, Height * Scale.Y ) );
  FUIImageView.setHidden( not Visible );

  if not FBitmap.IsEmpty then
    DoBitmapChanged( Self )
  else
    SetImageList( nil );

  SetBackgroundColor( FBackgroundColor );

  addSubview( Self, ParentControl );
  // ----------------------------
  // Important
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.SaveToFile( FileName: string );
var
  Bmp: TBitMap;
begin
  Bmp := UIImageToBitmap( FUIImageView.Image, 0 );
  if Bmp <> nil then
  begin
    Bmp.SaveToFile( FileName );
    Bmp.DisposeOf;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.SetOverImage( OverImage: string; atPointX: single = -1.0; atPointY: single = -1.0; FileOutSideApp: Boolean = false );
begin
  FOverImage               := OverImage;
  FOverImageAtPointX       := AtPointX;
  FOverImageAtPointY       := AtPointY;
  FOverImageFileOutSideApp := FileOutSideApp;
end;

// ------------------------------------------------------------------------------
function TDPFImageView.addImageOverImage( DestImage: UIImage; OverImage: string; atPointX: single = -1.0; atPointY: single = -1.0; FileOutSideApp: Boolean = false ): UIImage;
var
  r  : NSRect;
  Img: UIImage;
begin
  FOverImage := OverImage;
  try
    if ( DestImage = nil ) and ( FUIImageView <> nil ) then
      DestImage := FUIImageView.image;

    if ( FOverImage = '' ) or not Assigned( DestImage ) then
      exit;

    if FFileOutSideApp then
      Img := TUIImage.Wrap( TUIImage.OCClass.imageWithContentsOfFile( NSStr( FOverImage ) ) )
    else
      Img := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FOverImage ) ) );

    if Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 ) then
    begin
      r := CGRectMake( 0, 0, DestImage.size.width, DestImage.size.height );
      UIGraphicsBeginImageContextWithOptions( DestImage.size, false, 0 );

      DestImage.drawInRect( r );
      if ( atPointX = -1 ) or ( atPointY = -1 ) then
        Img.drawInRect( r )
      else
        Img.drawAtPoint( CGPointMake( atPointX, atPointY ) );

      Result := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
      UIGraphicsEndImageContext( );
    end;
  except
    Result := nil;
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.SetImage( AImageView: TDPFImageView );
begin
  SetImage( AImageView.FUIImageView.image );
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.SetImage( Image: UIImage; AndRelease: Boolean = false );
var
  img: UIIMage;
begin
  dispatch_sync( ImageSettingQueue,
    procedure
    begin

      if FGrayScale and assigned( Image ) then
        Image := convertImageToGrayScale( Image );
      if FOverImage <> '' then
      begin
        img := addImageOverImage( Image, FOverImage, FOverImageAtPointX, FOverImageAtPointY, FOverImageFileOutSideApp );
        if assigned( img ) then
          FUIImageView.setImage( img );
      end
      else
      begin
        FUIImageView.setImage( Image );
      end;
      if AndRelease and assigned( Image ) then
        Image.release;
    end );
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.SetImage( Image: TBitMap );
begin
  SetImage( BitmapToUIImage( Image ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.LoadFromMemoryStream( MemStream: TMemoryStream );
begin
  SetImage( MemoryStreamToUIImage( MemStream ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.LoadFromFile( FileName: string );
var
  Bmp: TBitMap;
begin
  if not FileExists( FileName ) then
    exit;
  Bmp := TBitMap.CreateFromFile( FileName );
  if Bmp <> nil then
    SetImage( Bmp );
end;

// ------------------------------------------------------------------------------
(* function TDPFImageView.SetImageBlur( blur: Single ): UIImage;
  var
  inputImage : CIImage;
  filter    : CIFilter;
  NSA        : NSMutableArray;
  outputImage: CIImage;
  outImage   : CGImageRef;
  context    : CIContext;
  begin

  inputImage := TCIImage.Wrap( TCIImage.OCClass.imageWithCGImage( FUIImageView.image.CGImage ) );

  NSA := TNSMutableArray.Create;

  NSA.addObject( ( kCIInputImageKey as ILocalObject ).GetObjectID );
  NSA.addObject( ( inputImage as ILocalObject ).GetObjectID );

  NSA.addObject( ( NSStr( 'inputRadius' ) as ILocalObject ).GetObjectID );
  NSA.addObject( ( TNSNumber.Wrap( TNSNumber.OCClass.numberWithDouble( blur ) ) as ILocalObject ).GetObjectID );

  filter := TCIFilter.Wrap( TCIFilter.OCClass.filterWithName( NSStr( 'CIGaussianBlur' ), ( NSA as ILocalObject ).GetObjectID  ));

  outputImage := filter.outputImage;
  context     := TCIContext.Wrap( TCIContext.OCClass.contextWithOptions( nil ) );

  outImage := context.createCGImage( outputImage, outputImage.extent );

  result := TUIImage.Wrap( TUIImage.OCClass.imageWithCGImage( outImage ) );

  end; *)

// ------------------------------------------------------------------------------
function TDPFImageView.GetColoredImage( Image: UIImage; Color: TAlphaColors ): UIImage;
var
  context: CGContextRef;
  clr    : UIColor;
  rect   : CGRect;
begin

  // begin a new image context, to draw our colored image onto
  UIGraphicsBeginImageContext( Image.size );

  // get a reference to that context we created
  context := UIGraphicsGetCurrentContext( );

  // set the fill color
  clr := TColorToUIColor( FBackgroundColor );
  clr.setFill;

  // translate/flip the graphics context (for transforming from CG* coords to UI* coords
  CGContextTranslateCTM( context, 0, Image.size.height );
  CGContextScaleCTM( context, 1.0, -1.0 );

  // set the blend mode to color burn, and the original image
  CGContextSetBlendMode( context, kCGBlendModeColorBurn );
  rect := CGRectMake( 0, 0, Image.size.width, Image.size.height );
  CGContextDrawImage( context, rect, Image.CGImage );

  // set a mask that matches the shape of the image, then draw (color burn) a colored rectangle
  CGContextClipToMask( context, rect, Image.CGImage );
  CGContextAddRect( context, rect );
  CGContextDrawPath( context, kCGPathFill );

  // generate a new UIImage from the graphics context we drew onto
  result := TUIImage.Wrap( UIGraphicsGetImageFromCurrentImageContext( ) );
  UIGraphicsEndImageContext( );
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.CancelLoading;
begin
  if Assigned( DPFHttp ) then
  begin
    DPFHttp.OnReceiveDataBin  := nil;
    DPFHttp.OnError           := nil;
    DPFHttp.OnReceiveProgress := nil;
    DPFHttp.DisposeOf;
    DPFHttp := nil;
  end;
  HideIndicatorView;
end;

{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFImageView.Resize;
begin
  inherited;
{$IFDEF IOS}
  if Assigned( FUIImageView ) then
  begin
    if assigned( FUIActivityIndicatorView ) then
      FUIActivityIndicatorView.setFrame( CGRectMake( 0, 0, Width * Scale.X, Height * Scale.Y ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.Move;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFNDEF IOS}

procedure TDPFImageView.Paint;
begin
  InternalPaint( '', TAlphaColors.Null, TDPFTextAlignment.taCenter, FBackgroundColor );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFImageView.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if Assigned( FUIImageView ) then
  begin
    if FBackgroundColor <> TAlphaColors.Null then
      FUIImageView.SetBackgroundColor( TColorToUIColor( FBackgroundColor ) )
    else
      FUIImageView.SetBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.UpdateBitmap;
begin
{$IFDEF IOS}
  FBitmap := UIImageToBitmap( FUIImageView.Image, 0 );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.SetBitmap( const Value: TBitMap );
begin
  FBitmap.Assign( Value );
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
function TDPFImageView.CheckCachedData( HTTPURL: string ): Boolean;
var
  NSD   : NSData;
  Img   : UIImage;
  isFree: Boolean;
  // ln    : NativeUInt;
begin
  NSD    := GetCachedData( HTTPURL );
  result := ( NSD <> nil ) and ( NSD.length > 40 );
  if result then
  begin
    dispatch_sync( ImageLodinggQueue,
      procedure
      begin
        Img := TUIImage.Wrap( TUIImage.OCClass.imageWithData( NSD ) );
        if Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 ) then
        begin
          SetImage( Img );
          Img := nil;
          if assigned( OnAsyncImageLoaded ) then
            OnAsyncImageLoaded( self );
        end
        else
        begin
          OnErrorAsyncLoading( Self, '', isFree );
        end
      end );
  end
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.ReloadImage;
begin
  CancelLoading;
  SetImageList( nil );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFImageView.SetImageList( const Value: TStrings );
{$IFDEF IOS}
var
  Img     : UIImage;
  ImgArray: NSMutableArray;
  NData   : NSData;
  isFree  : Boolean;
  I       : Integer;
{$ENDIF}
begin

  FRetryCount := 0;
  if Assigned( Value ) then
    FImageList.Assign( Value );
{$IFDEF IOS}
  if FImageList.Count > 0 then
  begin
    if FImageList.Count = 1 then
    begin
      if FCacheImage then
        if checkCachedData( FImageList[0] ) then
          exit;

      if FLoadFromURL and FLoadAsync then
      begin
        LoadImageAsyncURL( FImageList[0] );
      end
      else if not FLoadFromURL and FLoadAsync then
      begin
        dispatch_sync( ImageLodinggQueue,
          procedure
          begin
            try
              if FFileOutSideApp then
                Img := TUIImage.Wrap( TUIImage.OCClass.imageWithContentsOfFile( NSStr( FImageList[0] ) ) )
              else
                Img := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FImageList[0] ) ) );

              if Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 ) then
              begin
                setImage( Img );
                Img := nil;
              end;
            except
            end;
          end );
      end
      else if not FLoadAsync then
      begin
        if not FLoadFromURL then
        begin

          dispatch_sync( ImageLodinggQueue,
            procedure
            begin
              if FFileOutSideApp then
                Img := TUIImage.Wrap( TUIImage.OCClass.imageWithContentsOfFile( NSStr( FImageList[0] ) ) )
              else
                Img := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FImageList[0] ) ) );

              if Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 ) then
              begin
                SetImage( Img );
                Img := nil;
              end
            end );

        end
        else if FLoadFromURL then
        begin
          NData  := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfURL( TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( FImageList[0] ) ) ) ) );
          Img    := TUIImage.Wrap( TUIImage.OCClass.imageWithData( NData ) );
          isFree := false;
          OnFinishAsyncLoading( Self, NData, isFree );
        end;

      end;
    end
    else if FImageList.Count > 1 then
    begin
      FUIImageView.StopAnimating;
      ImgArray := TNSMutableArray.Create;

      for I := 0 to FImageList.Count - 1 do
      begin
        Img := TUIImage.Wrap( TUIImage.OCClass.imageNamed( NSStr( FImageList[I] ) ) );
        if Assigned( Img ) and ( Assigned( Img.CGImage ) or Assigned( Img.CIImage ) ) and ( Img.size.width > 0.99 ) and ( Img.size.height > 0.99 ) then
          ImgArray.addObject( ( Img as ILocalObject ).GetObjectID );
      end;

      FUIImageView.setAnimationImages( ImgArray );
      FUIImageView.SetAnimationDuration( FAnimationDuration );
      FUIImageView.SetAnimationRepeatCount( FAnimationRepeatCount );
      FUIImageView.StartAnimating;

      ImgArray.release;
    end;
  end;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.StartAnimating;
begin
{$IFDEF IOS}
  FUIImageView.StartAnimating;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFImageView.StopAnimating;
begin
{$IFDEF IOS}
  FUIImageView.StopAnimating;
{$ENDIF}
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFUIImageView }

constructor TDPFUIImageView.Create( ADPFImageView: TDPFImageView );
var
  V: Pointer;
begin
  inherited Create;
  FDPFImageView := ADPFImageView;
  V             := UIImageView( Super ).initWithFrame( CGRectMake( 0, 0, 100, 100 ) );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TDPFUIImageView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFUIImageView );
end;

// ------------------------------------------------------------------------------
procedure TDPFUIImageView.touchesBegan( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIImageView( Super ).touchesBegan( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  if Assigned( FDPFImageView.FOnTouchesBegan ) then
  begin
    Touch := TUITouch.Wrap( touches.anyObject );
    P     := Touch.locationInView( FDPFImageView.FUIImageView );
    PrevP := Touch.previousLocationInView( FDPFImageView.FUIImageView );
    FDPFImageView.FOnTouchesBegan( FDPFImageView, DPFNSPoint( P ), DPFNSPoint( PrevP ) );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIImageView.touchesMoved( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIImageView( Super ).touchesMoved( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFImageView.FUIImageView );
  PrevP := Touch.previousLocationInView( FDPFImageView.FUIImageView );
  if Assigned( FDPFImageView.FOnTouchesMoved ) then
    FDPFImageView.FOnTouchesMoved( FDPFImageView, DPFNSPoint( P ), DPFNSPoint( PrevP ) );
end;

// ------------------------------------------------------------------------------
procedure TDPFUIImageView.touchesEnded( touches: NSSet; withEvent: UIEvent ); cdecl;
var
  Touch   : UITouch;
  P, PrevP: NSPoint;
begin
  UIImageView( Super ).touchesEnded( touches, iOSapi.UIKit.UIEvent( withEvent ) );

  Touch := TUITouch.Wrap( touches.anyObject );
  P     := Touch.locationInView( FDPFImageView.FUIImageView );
  PrevP := Touch.previousLocationInView( FDPFImageView.FUIImageView );
  if Assigned( FDPFImageView.FOnTouchesEnded ) then
    FDPFImageView.FOnTouchesEnded( FDPFImageView, Touch.tapCount, DPFNSPoint( P ), DPFNSPoint( PrevP ) );

  if Touch.tapCount = 1 then
    NSObject( self.Super ).performSelector( sel_getUid( 'singleTap:' ), nil, FDPFImageView.TapDelay / 1000 )
  else if Touch.tapCount > 1 then
  begin
    iOSapi.{$IFDEF DELPHIXE7}Foundation{$ELSE}CocoaTypes{$ENDIF}.TNSObject.OCClass.cancelPreviousPerformRequestsWithTarget( Self.GetObjectID );
    NSObject( self.Super ).performSelector( sel_getUid( 'doubleTap:' ), nil, FDPFImageView.DoubleTapDelay / 1000 );
  end;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIImageView.singleTap( Sender: pointer ); cdecl;
begin
  if Assigned( FDPFImageView.FOnClick ) then
    FDPFImageView.FOnClick( FDPFImageView );
end;

// ------------------------------------------------------------------------------
procedure TDPFUIImageView.DoubleTap( Sender: pointer ); cdecl;
begin
  if Assigned( FDPFImageView.FOnDoubleClick ) then
    FDPFImageView.FOnDoubleClick( FDPFImageView );
end;

{$ENDIF}

initialization

end.
