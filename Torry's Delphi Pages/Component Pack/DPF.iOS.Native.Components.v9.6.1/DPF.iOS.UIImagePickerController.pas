// ------------------------------------------------------------------------------
// DPF.iOS.UIImagePickerController Component
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
unit DPF.iOS.UIImagePickerController;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,

  DPF.iOS.Classes,
  DPF.iOS.BaseControl,
  DPF.iOS.UIView,
  DPF.iOS.UIViewController,
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.ObjCRuntime,
  IOSapi.Foundation,
  IOSapi.Uikit,
  iOSapi.CocoaTypes,
  IOSapi.CoreGraphics,
  DPF.iOS.Common,
  DPF.iOS.MobileCoreServices,
{$ENDIF}
{$IFDEF DELPHIXE5} FMX.Graphics, {$ENDIF}
  FMX.Types,
  System.TypInfo;

type

  TDPFUIImagePickerController = class;

  TDPFCameraKind   = ( ckBackCamera = 0, ckFrontCamera = 1, ckDefault = 2 );
  FDPFVideoQuality = ( vcqPhotoQuality, vcqHighQuality, vcqMediumQuality, vcqLowQuality, vcq352x288Quality, vcq640x480Quality, vcq1280x720Quality, vcq1920x1080Quality, vcqiFrame960x540Quality, vcqiFrame1280x720Quality, vcqInputPriorityQuality );

  // UIImagePickerControllerMediaType
  // UIImagePickerControllerOriginalImage
  // UIImagePickerControllerEditedImage
  // UIImagePickerControllerCropRect
  // UIImagePickerControllerMediaURL
  // UIImagePickerControllerReferenceURL;
  // UIImagePickerControllerMediaMetadata
  TDPFEditingType = ( etMediaType, etOriginalImage, etEditedImage );

  TDPFSaveImageType = ( itJPG, itPNG );

  TDPFOndidFinishPickingImage         = procedure( Sender: TObject; BitMap: TBitMap ) of object;
  TDPFOndidFinishPickingSavedImage    = procedure( Sender: TObject; FileName: string ) of object;
  TDPFOndidFinishPickingMovie         = procedure( Sender: TObject; const FileSize: Int64; var FileSavedPath: string ) of object;
  TDPFOndidCancel                     = procedure( Sender: TObject ) of object;
  TDPFOndidFinishPickingOnResizeImage = procedure( Sender: TObject; var MaxWidth: Single; var MaxHeight: Single ) of object;

{$IFDEF IOS}
  TDPFOndidFinishPickingUIImage = procedure( Sender: TObject; Image: UIImage ) of object;

  // ------------------------------------------------------------------------------
  UIImagePickerControllerDelegate = interface( IObjectiveC )
    ['{C84AA3F1-67C4-4A7C-B214-7AECAA6F237D}']
    procedure imagePickerController( picker: UIImagePickerController; didFinishPickingMediaWithInfo: NSDictionary ); cdecl;
    procedure imagePickerControllerDidCancel( picker: UIImagePickerController ); cdecl;
  end;

  TDPFImagePickerControllerDelegate = class( TOCLocal, UIImagePickerControllerDelegate )
  private
    FDPFUIImagePickerController: TDPFUIImagePickerController;
  public
    constructor Create( ADPFUIImagePickerController: TDPFUIImagePickerController );
    procedure HidePicker;

    procedure imagePickerController( picker: UIImagePickerController; didFinishPickingMediaWithInfo: NSDictionary ); cdecl;
    procedure imagePickerControllerDidCancel( picker: UIImagePickerController ); cdecl;
  end;

  // ------------------------------------------------------------------------------
{$ENDIF}
  (* DPFSaveToAlbum = interface( NSObject )
    ['{833795B0-1181-490E-8996-95B01E49207A}']
    procedure didFinishSavingWithError( videoPath: NSString; error: NSError; contextInfo: Pointer ); cdecl;
    end;

    // ------------------------------------------------------------------------------
    TDPFSaveToAlbum = class( TOCLocal )
    public
    function GetObjectiveCClass: PTypeInfo; override;
    procedure didFinishSavingWithError( videoPath: NSString; error: NSError; contextInfo: Pointer ); cdecl;
    end; *)

  // ------------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFUIImagePickerController = class( TComponent )
  private
    FCameraKind                  : TDPFCameraKind;
    FOndidFinishPickingImage     : TDPFOndidFinishPickingImage;
    FAllowsEditing               : Boolean;
    FShowsCameraControls         : Boolean;
    FEditingType                 : TDPFEditingType;
    FOndidFinishPickingMovie     : TDPFOndidFinishPickingMovie;
    FVideoQuality                : FDPFVideoQuality;
    FSaveImageType               : TDPFSaveImageType;
    FOndidFinishPickingSavedImage: TDPFOndidFinishPickingSavedImage;
    FAnimateModal                : Boolean;
    FOnDidCancel                 : TDPFOndidCancel;
    FOnResizeImage               : TDPFOndidFinishPickingOnResizeImage;
    FSaveToAlbum                 : Boolean;
{$IFDEF IOS}
    FOndidFinishPickingUIImage       : TDPFOndidFinishPickingUIImage;
    FDPFImagePickerController        : UIImagePickerController;
    FDPFImagePickerControllerDelegate: TDPFImagePickerControllerDelegate;
    FDPFPopover                      : UIPopoverController;
{$ENDIF}
  protected
    procedure ViewControllerCompletion;
{$IFDEF IOS}
    function TakeImage( VC: TDPFUIViewController; AControl: TDPFiOSBaseControl; const ASourceType: UIImagePickerControllerSourceType ): Boolean;
    class function IsAvailableSourceType( const ASourceType: UIImagePickerControllerSourceType ): Boolean;
{$ENDIF}
  public
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

    function TakeImageFromLibrary( VC: TDPFUIViewController = nil; Control: TDPFiOSBaseControl = nil ): Boolean;
    function TakeImageFromCamera( VC: TDPFUIViewController = nil; Control: TDPFiOSBaseControl = nil ): Boolean;
  published
{$IFDEF IOS}
    property OndidFinishPickingUIImage: TDPFOndidFinishPickingUIImage read FOndidFinishPickingUIImage write FOndidFinishPickingUIImage;
{$ENDIF}
    property OnDidFinishPickingSavedImage: TDPFOndidFinishPickingSavedImage read FOndidFinishPickingSavedImage write FOndidFinishPickingSavedImage;
    property OnDidFinishPickingImage     : TDPFOndidFinishPickingImage read FOndidFinishPickingImage write FOndidFinishPickingImage;
    property OnDidFinishPickingMovie     : TDPFOndidFinishPickingMovie read FOndidFinishPickingMovie write FOndidFinishPickingMovie;
    property OnDidCancel                 : TDPFOndidCancel read FOnDidCancel write FOnDidCancel;
    property OnResizeImage               : TDPFOndidFinishPickingOnResizeImage read FOnResizeImage write FOnResizeImage;

    property SaveImageType: TDPFSaveImageType read FSaveImageType write FSaveImageType default TDPFSaveImageType.itJPG;
    property VideoQuality : FDPFVideoQuality read FVideoQuality write FVideoQuality default FDPFVideoQuality.vcqHighQuality;

    property AnimateModal: Boolean read FAnimateModal write FAnimateModal default true;
    property SaveToAlbum : Boolean read FSaveToAlbum write FSaveToAlbum default true;

    property AllowsEditing      : Boolean read FAllowsEditing write FAllowsEditing default true;
    property ShowsCameraControls: Boolean read FShowsCameraControls write FShowsCameraControls default true;

    property CameraKind : TDPFCameraKind read FCameraKind write FCameraKind default TDPFCameraKind.ckDefault;
    property EditingType: TDPFEditingType read FEditingType write FEditingType default TDPFEditingType.etOriginalImage;
  end;

  // ------------------------------------------------------------------------------

implementation

// ------------------------------------------------------------------------------
{ TDPFUIImagePickerController }
// ------------------------------------------------------------------------------
constructor TDPFUIImagePickerController.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  FSaveToAlbum         := true;
  FAnimateModal        := true;
  FAllowsEditing       := true;
  FCameraKind          := TDPFCameraKind.ckDefault;
  FEditingType         := TDPFEditingType.etOriginalImage;
  FShowsCameraControls := true;
  FVideoQuality        := FDPFVideoQuality.vcqHighQuality;
  FSaveImageType       := TDPFSaveImageType.itJPG;

{$IFDEF IOS}
  FDPFImagePickerControllerDelegate := TDPFImagePickerControllerDelegate.Create( Self );
  FDPFImagePickerController         := TUIImagePickerController.Create;
  FDPFImagePickerController.setDelegate( FDPFImagePickerControllerDelegate.GetObjectID );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFUIImagePickerController.Destroy;
begin
{$IFDEF IOS}
  if Assigned( FDPFPopover ) then
    FDPFPopover.release;
  FDPFImagePickerController.release;
  FDPFImagePickerController := nil;
  FDPFImagePickerControllerDelegate.DisposeOf;
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFUIImagePickerController.ViewControllerCompletion;
begin

end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
function TDPFUIImagePickerController.TakeImage( VC: TDPFUIViewController; AControl: TDPFiOSBaseControl; const ASourceType: UIImagePickerControllerSourceType ): Boolean;
var
  frame: CGRect;
  Win  : UIWindow;
  MT   : NSMutableArray;
begin
  FDPFImagePickerController.setAllowsEditing( FAllowsEditing );
  FDPFImagePickerController.setAllowsImageEditing( FAllowsEditing );

  Result := IsAvailableSourceType( ASourceType );
  if not Result then
    Exit;

  FDPFImagePickerController.setSourceType( ASourceType );
  FDPFImagePickerController.setVideoQuality( integer( FVideoQuality ) );
  MT := TNSMutableArray.Create;
  if FEditingType = etMediaType then
  begin
    MT.addObject( ( kUTTypeMovie as ILocalObject ).GetObjectID );
    MT.addObject( ( kUTTypeVideo as ILocalObject ).GetObjectID );
    MT.addObject( ( kUTTypeAudio as ILocalObject ).GetObjectID );
    MT.addObject( ( kUTTypeMPEG4 as ILocalObject ).GetObjectID );
  end
  else
  begin
    MT.addObject( ( kUTTypeImage as ILocalObject ).GetObjectID );
    MT.addObject( ( kUTTypeJPEG as ILocalObject ).GetObjectID );
    MT.addObject( ( kUTTypePNG as ILocalObject ).GetObjectID );
    MT.addObject( ( kUTTypeQuickTimeImage as ILocalObject ).GetObjectID );
    MT.addObject( ( kUTTypeBMP as ILocalObject ).GetObjectID );
  end;
  FDPFImagePickerController.setMediaTypes( MT );
  MT.release;
  if IsAvailableSourceType( UIImagePickerControllerSourceTypeCamera ) and ( ASourceType = UIImagePickerControllerSourceTypeCamera ) then
  begin
    FDPFImagePickerController.setShowsCameraControls( FShowsCameraControls );
    if FCameraKind <> TDPFCameraKind.ckDefault then
      FDPFImagePickerController.setCameraDevice( Integer( FCameraKind ) );
  end;

  Win := GetSharedApplication.keyWindow;

  if not IsIPad or ( ASourceType = UIImagePickerControllerSourceTypeCamera ) then
  begin
    if Assigned( VC ) then
      VC.FUIViewController.presentModalViewController( FDPFImagePickerController, AnimateModal { , ViewControllerCompletion } )
    else
      Win.rootViewController.presentModalViewController( FDPFImagePickerController, AnimateModal { , ViewControllerCompletion } )
  end
  else
  begin
    if Assigned( FDPFPopover ) then
    begin
      FDPFPopover.release;
      FDPFPopover := nil;
    end;
    FDPFPopover := TUIPopoverController.Wrap( TUIPopoverController.Alloc.initWithContentViewController( FDPFImagePickerController ) );

    if Assigned( AControl ) then
      frame := UIView( AControl.UIControl ).bounds
    else
      frame := CGRectMake( 0, 0, 0, 0 );

    if Assigned( VC ) then
      FDPFPopover.presentPopoverFromRect( frame, VC.FUIViewController.View, UIPopoverArrowDirectionAny, AnimateModal )
    else
      FDPFPopover.presentPopoverFromRect( frame, Win.rootViewController.View, UIPopoverArrowDirectionAny, AnimateModal );
  end;

end;

// ------------------------------------------------------------------------------
class function TDPFUIImagePickerController.IsAvailableSourceType( const ASourceType: UIImagePickerControllerSourceType ): Boolean;
begin
  Result := TUIImagePickerController.OCClass.isSourceTypeAvailable( ASourceType );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
function TDPFUIImagePickerController.TakeImageFromCamera( VC: TDPFUIViewController = nil; Control: TDPFiOSBaseControl = nil ): Boolean;
begin
{$IFDEF IOS}
  Result := TakeImage( VC, Control, UIImagePickerControllerSourceTypeCamera );
{$ELSE}
  Result := false;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
function TDPFUIImagePickerController.TakeImageFromLibrary( VC: TDPFUIViewController = nil; Control: TDPFiOSBaseControl = nil ): Boolean;
begin
{$IFDEF IOS}
  if IsAvailableSourceType( UIImagePickerControllerSourceTypeCamera ) then
    Result := TakeImage( VC, Control, UIImagePickerControllerSourceTypePhotoLibrary )
  else
    Result := TakeImage( VC, Control, UIImagePickerControllerSourceTypeSavedPhotosAlbum );
{$ELSE}
  Result := false;
{$ENDIF}
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TDPFImagePickerControllerDelegate }

constructor TDPFImagePickerControllerDelegate.Create( ADPFUIImagePickerController: TDPFUIImagePickerController );
begin
  inherited create;
  FDPFUIImagePickerController := ADPFUIImagePickerController;
end;

// ------------------------------------------------------------------------------
procedure TDPFImagePickerControllerDelegate.imagePickerController( picker: UIImagePickerController; didFinishPickingMediaWithInfo: NSDictionary );
const
  EType: array [TDPFEditingType] of string = ( 'UIImagePickerControllerMediaType', 'UIImagePickerControllerOriginalImage', 'UIImagePickerControllerEditedImage' );
var
  img                : UIImage;
  Bitmap             : TBitmap;
  Angle              : SIngle;
  mtype              : NSString;
  videoURL           : NSURL;
  videoData          : NSData;
  FileSavedPath      : string;
  isMovie            : Boolean;
  P                  : Pointer;
  newWidth, newHeight: Single;
  scaleFactor        : Single;
begin
  HidePicker;

  mtype   := TNSString.Wrap( didFinishPickingMediaWithInfo.objectForKey( ( CocoaNSStringConst( libKit, 'UIImagePickerControllerMediaType' ) as ILocalObject ).GetObjectID ) );
  isMovie := mtype.isEqualToString( kUTTypeVideo ) or mtype.isEqualToString( kUTTypeMovie ) or mtype.isEqualToString( kUTTypeVideo );

  // movie != video
  if Assigned( FDPFUIImagePickerController.FOndidFinishPickingMovie ) and isMovie then
  begin
    videoURL  := TNSURL.Wrap( didFinishPickingMediaWithInfo.objectForKey( ( CocoaNSStringConst( libKit, 'UIImagePickerControllerMediaURL' ) as ILocalObject ).GetObjectID ) );
    videoData := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfURL( videoUrl ) );

    // FileSavedPath := UTF8ToString( videoURL.absoluteString.UTF8String );
    FileSavedPath := UTF8ToString( videoURL.path.UTF8String );

    if FDPFUIImagePickerController.FSaveToAlbum then
      if UIVideoAtPathIsCompatibleWithSavedPhotosAlbum( PNSStr( FileSavedPath ) ) then
        UISaveVideoAtPathToSavedPhotosAlbum( PNSStr( FileSavedPath ), nil, nil, nil );

    FDPFUIImagePickerController.FOndidFinishPickingMovie( FDPFUIImagePickerController, videoData.length, FileSavedPath );
    if FileSavedPath <> UTF8ToString( videoURL.absoluteString.UTF8String ) then
      videoData.writeToFile( NSStr( FileSavedPath ), true );
  end
  else if Assigned( FDPFUIImagePickerController.FOndidFinishPickingImage ) and not isMovie then
  begin
    img    := TUIImage.Wrap( didFinishPickingMediaWithInfo.objectForKey( ( CocoaNSStringConst( libKit, EType[FDPFUIImagePickerController.FEditingType] ) as ILocalObject ).GetObjectID ) );
    Angle  := GetImageOrientation( img );
    Bitmap := UIImageToBitmap( img, Angle );
    try
      FDPFUIImagePickerController.FOndidFinishPickingImage( FDPFUIImagePickerController, BitMap );
    finally
      Bitmap.DisposeOf;
    end;
  end
  else if Assigned( FDPFUIImagePickerController.FOndidFinishPickingUIImage ) and not isMovie then
  begin
    img := TUIImage.Wrap( didFinishPickingMediaWithInfo.objectForKey( ( CocoaNSStringConst( libKit, EType[FDPFUIImagePickerController.FEditingType] ) as ILocalObject ).GetObjectID ) );
    FDPFUIImagePickerController.FOndidFinishPickingUIImage( FDPFUIImagePickerController, img );
  end
  else if Assigned( FDPFUIImagePickerController.FOndidFinishPickingSavedImage ) and not isMovie then
  begin
    P := didFinishPickingMediaWithInfo.objectForKey( ( CocoaNSStringConst( libKit, EType[FDPFUIImagePickerController.FEditingType] ) as ILocalObject ).GetObjectID );

    if Assigned( FDPFUIImagePickerController.FOnResizeImage ) then
    begin
      img       := TUIImage.Wrap( P );
      newWidth  := img.size.width;
      newHeight := img.size.height;
      FDPFUIImagePickerController.FOnResizeImage( FDPFUIImagePickerController, newWidth, newHeight );

      if img.size.width > img.size.height then
      begin
        scaleFactor := img.size.width / img.size.height;
        newHeight   := newHeight / scaleFactor;
      end
      else
      begin
        scaleFactor := img.size.height / img.size.width;
        newWidth    := newWidth / scaleFactor;
      end;

      UIGraphicsBeginImageContext( CGSizeMake( newWidth, newHeight ) { , false, 0.0 } );
      img.drawInRect( CGRectMake( 0, 0, newWidth, newHeight ) );
      P := UIGraphicsGetImageFromCurrentImageContext( );
      UIGraphicsEndImageContext( );
    end;

    if FDPFUIImagePickerController.FSaveImageType = itPNG then
    begin
      P             := UIImagePNGRepresentation( P );
      FileSavedPath := '.png';
    end
    else
    begin
      P             := UIImageJPEGRepresentation( P, 1.0 );
      FileSavedPath := '.jpg';
    end;

    FileSavedPath := GetTempDirectory + GetUUID + FileSavedPath;
    if Assigned( P ) then
      if not TNSData.Wrap( P ).writeToFile( NSStr( FileSavedPath ), true ) then
        FileSavedPath := '';
    FDPFUIImagePickerController.FOndidFinishPickingSavedImage( FDPFUIImagePickerController, FileSavedPath );
  end;

  if not isMovie and ( FDPFUIImagePickerController.FDPFImagePickerController.sourceType = UIImagePickerControllerSourceTypeCamera ) and FDPFUIImagePickerController.FSaveToAlbum then
  begin
    p := didFinishPickingMediaWithInfo.objectForKey( ( CocoaNSStringConst( libKit, EType[FDPFUIImagePickerController.FEditingType] ) as ILocalObject ).GetObjectID );
    if FDPFUIImagePickerController.FSaveToAlbum then
      UIImageWriteToSavedPhotosAlbum( P, nil, nil, nil );
  end;

end;

// ------------------------------------------------------------------------------
procedure TDPFImagePickerControllerDelegate.imagePickerControllerDidCancel( picker: UIImagePickerController );
begin
  HidePicker;
  if Assigned( FDPFUIImagePickerController.FOnDidCancel ) then
    FDPFUIImagePickerController.FOnDidCancel( FDPFUIImagePickerController );
end;

// ------------------------------------------------------------------------------
procedure TDPFImagePickerControllerDelegate.HidePicker;
begin
  if not IsIPad or ( FDPFUIImagePickerController.FDPFImagePickerController.sourceType = UIImagePickerControllerSourceTypeCamera ) then
    FDPFUIImagePickerController.FDPFImagePickerController.dismissModalViewControllerAnimated( FDPFUIImagePickerController.AnimateModal { , FDPFUIImagePickerController.ViewControllerCompletion } )
  else
  begin
    FDPFUIImagePickerController.FDPFPopover.dismissPopoverAnimated( FDPFUIImagePickerController.AnimateModal );
  end;
end;

// ------------------------------------------------------------------------------
{$ENDIF}

end.
