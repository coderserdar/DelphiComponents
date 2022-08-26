// ------------------------------------------------------------------------------
// DPF.iOS.QLPreviewController Component
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
unit DPF.iOS.QLPreviewController;

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
  DPF.iOS.UIActivityIndicatorView,
  DPF.iOS.UINavigationController,
  DPF.iOS.UIViewController,
  DPF.iOS.UIView,
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
  FMX.Layouts, FMX.Memo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls;

const
  libQuickLook = '/System/Library/Frameworks/QuickLook.framework/QuickLook';

type

  TDPFQLPreviewController = class;

{$IFDEF IOS}

  QLPreviewItem = interface( NSObject )
    ['{D9C6242A-B4AB-49E7-9AB6-2EC94E22490A}']
    function previewItemURL: NSURL; cdecl;
    function previewItemTitle: NSString; cdecl;
  end;

  QLPreviewControllerClass = interface( UIViewControllerClass )
    ['{4402B308-B7A0-4928-BB5F-A55FB21DF41E}']
  end;

  QLPreviewController = interface( UIViewController )
    ['{B5DF1CA2-9FEF-4BF1-AB54-935C752554A7}']
    function dataSource: Pointer; cdecl;
    procedure setDataSource( dataSource: Pointer ); cdecl;

    function delegate: Pointer; cdecl;
    procedure setDelegate( delegate: Pointer ); cdecl;

    function canPreviewItem( item: QLPreviewItem ): boolean; cdecl;
    function currentPreviewItem: QLPreviewItem; cdecl;
    function currentPreviewItemIndex: NSInteger; cdecl;
    procedure refreshCurrentPreviewItem; cdecl;
    procedure reloadData; cdecl;
  end;

  TQLPreviewController = class( TOCGenericImport<QLPreviewControllerClass, QLPreviewController> )
  end;

  QLPreviewControllerDataSource = interface( IObjectiveC )
    ['{6B6482E1-6269-4489-B92C-F4A5490BAA1F}']
    function numberOfPreviewItemsInPreviewController( previewController: QLPreviewController ): NSInteger; cdecl;
    function previewController( previewController: QLPreviewController; previewItemAtIndex: NSInteger ): QLPreviewItem; cdecl;
  end;

  TQLPreviewControllerDataSource = class( TOCLocal, QLPreviewControllerDataSource )
  private
    FDPFQLPreviewController: TDPFQLPreviewController;
  public
    constructor Create( AParent: TDPFQLPreviewController );

    function numberOfPreviewItemsInPreviewController( previewController: QLPreviewController ): NSInteger; cdecl;
    function previewController( previewController: QLPreviewController; previewItemAtIndex: NSInteger ): QLPreviewItem; cdecl;
  end;

  // ----------------------------------------------------------------------------
  QLPreviewControllerDelegate = interface( IObjectiveC )
    ['{9EB6DB9C-E99F-4199-B985-7C265D423B14}']
    function previewController( previewController: QLPreviewController; frameForPreviewItem: QLPreviewItem; inSourceView: UIView ): CGRect; cdecl; overload;
    function previewController( previewController: QLPreviewController; transitionImageForPreviewItem: QLPreviewItem; contentRect: CGRect ): UIImage; cdecl; overload;
    function previewController( previewController: QLPreviewController; shouldOpenURL: NSURL; forPreviewItem: QLPreviewItem ): boolean; cdecl; overload;

    procedure previewControllerDidDismiss( controller: QLPreviewController ); cdecl;
    procedure previewControllerWillDismiss( controller: QLPreviewController ); cdecl;
    procedure viewDidAppear( viewDidAppear: QLPreviewController; animated: Boolean ); cdecl;
  end;

  TQLPreviewControllerDelegate = class( TOCLocal, QLPreviewControllerDelegate )
  private
    FDPFQLPreviewController: TDPFQLPreviewController;
  public
    constructor Create( AParent: TDPFQLPreviewController );

    function previewController( previewController: QLPreviewController; frameForPreviewItem: QLPreviewItem; inSourceView: UIView ): CGRect; overload; cdecl;
    function previewController( previewController: QLPreviewController; transitionImageForPreviewItem: QLPreviewItem; contentRect: CGRect ): UIImage; overload; cdecl;
    function previewController( previewController: QLPreviewController; shouldOpenURL: NSURL; forPreviewItem: QLPreviewItem ): boolean; overload; cdecl;

    procedure previewControllerDidDismiss( controller: QLPreviewController ); cdecl;
    procedure previewControllerWillDismiss( controller: QLPreviewController ); cdecl;
    procedure viewDidAppear( viewDidAppear: QLPreviewController; animated: Boolean ); cdecl;
  end;

{$ENDIF}

  TDPFShouldOpenURL = procedure( Sender: TObject; URL: string; var ShouldOpenURL: Boolean ) of object;

  // ----------------------------------------------------------------------------
  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFQLPreviewController = class( TDPFiOSBaseControl )
  private
    FIsURL                   : Boolean;
    FSourceFile              : string;
    FBackgroundColor         : TAlphaColor;
    DPFActivityIndicatorView1: TDPFActivityIndicatorView;
    FOnShouldOpenURL         : TDPFShouldOpenURL;
    procedure SetBackgroundColor( const Value: TAlphaColor );

  protected
{$IFNDEF IOS}
    procedure Paint; override;
{$ENDIF}
    procedure Resize; override;
    procedure ViewControllerCompletion;
  public
{$IFDEF IOS}
    FQLPreviewController          : QLPreviewController;
    FQLPreviewControllerDelegate  : QLPreviewControllerDelegate;
    FQLPreviewControllerDataSource: QLPreviewControllerDataSource;
    procedure Loaded; override;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
    procedure ShowDoc( SourceFile: string; IsURL: Boolean; rootViewController: TDPFUIViewController = nil );
    // procedure Push( SourceFile: string; IsURL: Boolean; Navigation: TDPFNavigationController );
  published
    property BackgroundColor: TAlphaColor read FBackgroundColor write SetBackgroundColor default TAlphaColors.Null;
    property OnShouldOpenURL: TDPFShouldOpenURL read FOnShouldOpenURL write FOnShouldOpenURL;

    property UserInteraction;
    property ContentMode;
    property Alpha;
    property Align;
    property Position;
    property Width;
    property Height;
    property Visible;
  end;

implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  QuickLookModule: THandle;
{$ENDIF}
{$ENDIF}

  // ------------------------------------------------------------------------------
constructor TDPFQLPreviewController.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );
  ControlCaption   := 'QuickLook View Control';
  FBackgroundColor := TAlphaColors.Null;
  AddThisToSubView := false;

{$IFDEF IOS}
  FQLPreviewController := TQLPreviewController.Wrap( TQLPreviewController.Alloc.init );

  FQLPreviewControllerDelegate   := TQLPreviewControllerDelegate.Create( Self );
  FQLPreviewControllerDataSource := TQLPreviewControllerDataSource.Create( Self );

  FQLPreviewController.setHidesBottomBarWhenPushed( false );
  FQLPreviewController.setWantsFullScreenLayout( true );
  FUIControl := FQLPreviewController;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFQLPreviewController.Destroy;
begin
{$IFDEF IOS}
  FQLPreviewControllerDelegate := nil;
  FQLPreviewControllerDataSource := nil;
{$ENDIF}
  if Assigned( DPFActivityIndicatorView1 ) then
    DPFActivityIndicatorView1.DisposeOf;
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFQLPreviewController.ShowDoc( SourceFile: string; IsURL: Boolean; rootViewController: TDPFUIViewController = nil );
{$IFDEF IOS}
var
  FMainWindow: UIWindow;
{$ENDIF}
begin
  FIsURL      := IsURL;
  FSourceFile := SourceFile;
{$IFDEF IOS}
  if not Assigned( DPFActivityIndicatorView1 ) then
    DPFActivityIndicatorView1               := TDPFActivityIndicatorView.Create( Parent );
  DPFActivityIndicatorView1.Parent          := Parent;
  DPFActivityIndicatorView1.Align           := TAlignLayout.Center;
  DPFActivityIndicatorView1.BackgroundColor := TAlphaColors.Black;
  DPFActivityIndicatorView1.CornerRadius    := 10;
  DPFActivityIndicatorView1.Height          := 100;
  DPFActivityIndicatorView1.Width           := 100;
  DPFActivityIndicatorView1.Loaded;

  if Assigned( rootViewController ) then
    rootViewController.FUIViewController.presentModalViewController( FQLPreviewController, false { true, ViewControllerCompletion } )
  else
  begin
    FMainWindow := GetSharedApplication.keyWindow;
    if Assigned( FMainWindow ) and Assigned( FMainWindow.rootViewController ) then
      FMainWindow.rootViewController.presentModalViewController( FQLPreviewController, false { true, ViewControllerCompletion } );
  end;
  FQLPreviewController.reloadData;
{$ENDIF}
end;

// ------------------------------------------------------------------------------
procedure TDPFQLPreviewController.ViewControllerCompletion;
begin

end;

// ------------------------------------------------------------------------------
(* procedure TDPFQLPreviewController.Push( SourceFile: string; IsURL: Boolean; Navigation: TDPFNavigationController );
  {$IFDEF IOS}
  var
  FMainWindow: UIWindow;
  {$ENDIF}
  begin
  {$IFDEF IOS}
  if not Assigned( DPFActivityIndicatorView1 ) then
  DPFActivityIndicatorView1               := TDPFActivityIndicatorView.Create( Parent );
  DPFActivityIndicatorView1.Parent          := Parent;
  DPFActivityIndicatorView1.Align           := TAlignLayout.Center;
  DPFActivityIndicatorView1.BackgroundColor := TAlphaColors.Black;
  DPFActivityIndicatorView1.CornerRadius    := 10;
  DPFActivityIndicatorView1.Height          := 100;
  DPFActivityIndicatorView1.Width           := 100;
  DPFActivityIndicatorView1.Loaded;

  FIsURL      := IsURL;
  FSourceFile := SourceFile;

  Navigation.GetNavigationController.pushViewController( FQLPreviewController, true );
  FQLPreviewController.reloadData;
  FQLPreviewController.view.setFrame(CGRectMake(0, 0, width, height));
  {$ENDIF}
  end; *)
// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFQLPreviewController.Loaded;
begin

  FQLPreviewController.view.setHidden( not Visible );
  FQLPreviewController.view.setFrame( CGRectMake( Position.X, Position.Y, Width, Height ) );

  FQLPreviewController.setDelegate( (FQLPreviewControllerDelegate as ILocalObject).GetObjectID );
  FQLPreviewController.setDataSource( (FQLPreviewControllerDataSource as ILocalObject).GetObjectID );
  SetBackgroundColor( FBackgroundColor );

  addSubview( Self, ParentControl, FQLPreviewController.view );
  // ----------------------------
  // Important
  inherited;
end;
{$ENDIF}
{$IFNDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFQLPreviewController.Paint;
var
  C: TAlphaColor;
begin
  C := BackgroundColor;
  if C = TAlphaColors.Null then
    C := TAlphaColors.White;
  InternalPaint( '', TAlphaColors.Black, TDPFTextAlignment.taCenter, C );
end;
{$ENDIF}

// ------------------------------------------------------------------------------
procedure TDPFQLPreviewController.Resize;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFQLPreviewController.SetBackgroundColor( const Value: TAlphaColor );
begin
  FBackgroundColor := Value;
{$IFDEF IOS}
  if FBackgroundColor <> TAlphaColors.Null then
  begin
    FQLPreviewController.view.setBackgroundColor( TColorToUIColor( FBackgroundColor ) );
    //FQLPreviewController.view.setTintColor( TColorToUIColor( FBackgroundColor ) );
  end
  else
  begin
    FQLPreviewController.view.setBackgroundColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
    //FQLPreviewController.view.setTintColor( TUIColor.Wrap( TUIColor.OCClass.clearColor ) );
  end;
{$ELSE}
  Invalidate;
{$ENDIF}
end;

{$IFDEF IOS}
// ------------------------------------------------------------------------------
{ TQLPreviewControllerDataSource }

constructor TQLPreviewControllerDataSource.Create( AParent: TDPFQLPreviewController );
begin
  inherited create;
  FDPFQLPreviewController := AParent;
end;

// ------------------------------------------------------------------------------
function TQLPreviewControllerDataSource.numberOfPreviewItemsInPreviewController( previewController: QLPreviewController ): NSInteger;
begin
  result := 1;
end;

// ------------------------------------------------------------------------------
function TQLPreviewControllerDataSource.previewController( previewController: QLPreviewController; previewItemAtIndex: NSInteger ): QLPreviewItem;
var
  fileURL : NSURL;
  D       : NSData;
  S       : string;
  HashName: string;
begin
  S        := FDPFQLPreviewController.FSourceFile;
  HashName := IntToStr( GetHash( FDPFQLPreviewController.FSourceFile ) ) + ExtractFileExt( S );
  if FDPFQLPreviewController.FIsURL and not FileExists( GetTempDirectory + 'Temp\' + HashName ) then
  begin
    if Assigned(FDPFQLPreviewController.DPFActivityIndicatorView1) then
      FDPFQLPreviewController.DPFActivityIndicatorView1.StartAnimating;
    fileURL := TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( S ) ) );
    D       := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfURL( fileURL ) );
    ForceDirectories( GetTempDirectory + 'Temp' );
    DeleteFile( GetTempDirectory + 'Temp\' + HashName );
    D.writeToFile( NSStr( GetTempDirectory + 'Temp\' + HashName ), true );
    S := GetTempDirectory + 'Temp\' + HashName;
  end
  else if FDPFQLPreviewController.FIsURL then
    S := GetTempDirectory + 'Temp\' + HashName;

  fileURL := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( NSStr( S ) ) );
  if Assigned(FDPFQLPreviewController.DPFActivityIndicatorView1) then
    FDPFQLPreviewController.DPFActivityIndicatorView1.StopAnimating;

  Result := QLPreviewItem( fileURL );
end;

// ------------------------------------------------------------------------------
{ TQLPreviewControllerDelegate }

constructor TQLPreviewControllerDelegate.Create( AParent: TDPFQLPreviewController );
begin
  inherited create;
  FDPFQLPreviewController := AParent;
end;

// ------------------------------------------------------------------------------
function TQLPreviewControllerDelegate.previewController( previewController: QLPreviewController; shouldOpenURL: NSURL; forPreviewItem: QLPreviewItem ): boolean;
begin
  result := true;
  if Assigned( FDPFQLPreviewController.OnShouldOpenURL ) then
    FDPFQLPreviewController.OnShouldOpenURL( FDPFQLPreviewController, UTF8ToString( shouldOpenURL.absoluteString.UTF8String ), Result );
end;

// ------------------------------------------------------------------------------
function TQLPreviewControllerDelegate.previewController( previewController: QLPreviewController; transitionImageForPreviewItem: QLPreviewItem; contentRect: CGRect ): UIImage;
begin

end;

// ------------------------------------------------------------------------------
function TQLPreviewControllerDelegate.previewController( previewController: QLPreviewController; frameForPreviewItem: QLPreviewItem; inSourceView: UIView ): CGRect;
begin

end;

// ------------------------------------------------------------------------------
procedure TQLPreviewControllerDelegate.previewControllerDidDismiss( controller: QLPreviewController );
begin

end;

// ------------------------------------------------------------------------------
procedure TQLPreviewControllerDelegate.previewControllerWillDismiss( controller: QLPreviewController );
begin

end;

// ------------------------------------------------------------------------------
procedure TQLPreviewControllerDelegate.viewDidAppear( viewDidAppear: QLPreviewController; animated: Boolean );
begin
end;
{$ENDIF}
// ------------------------------------------------------------------------------
{$IFDEF IOS}
{$IF defined(CPUARM)}
procedure LibQuickLookLoader; cdecl; external libQuickLook;
{$ELSE}

initialization

QuickLookModule := dlopen( MarshaledAString( libQuickLook ), RTLD_LAZY );

finalization

dlclose( QuickLookModule );
{$ENDIF}
{$ENDIF}

end.
