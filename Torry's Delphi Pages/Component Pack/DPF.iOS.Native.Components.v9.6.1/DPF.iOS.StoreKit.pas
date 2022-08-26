// ------------------------------------------------------------------------------
// DPF.iOS.StoreKit Class
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

unit DPF.iOS.StoreKit;

interface

{$I DPF.iOS.Defs.inc}

uses
{$IFDEF IOS}
  Macapi.ObjectiveC,
  Macapi.CoreFoundation,
  Macapi.ObjCRuntime,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  DPF.iOS.Common,
  iOSapi.UIKit,
{$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Variants,

  FMX.Dialogs,

  System.TypInfo,
  System.Math;

// ------------------------------------------------------------------------------

{$IFDEF IOS}

const
  SKDownloadStateWaiting   = 0;
  SKDownloadStateActive    = 1;
  SKDownloadStatePaused    = 2;
  SKDownloadStateFinished  = 3;
  SKDownloadStateFailed    = 4;
  SKDownloadStateCancelled = 5;

  SKPaymentTransactionStatePurchasing = 0;
  SKPaymentTransactionStatePurchased  = 1;
  SKPaymentTransactionStateFailed     = 2;
  SKPaymentTransactionStateRestored   = 3;

  // ----------------------------------------
  // Store Kit Errors
  SKErrorUnknown           = 0;
  SKErrorClientInvalid     = 1;
  SKErrorPaymentCancelled  = 2;
  SKErrorPaymentInvalid    = 3;
  SKErrorPaymentNotAllowed = 4;

  storeKitFwk = '/System/Library/Frameworks/StoreKit.framework/StoreKit';
{$ENDIF}

type

  TDPFInAppPurchase = class;

  TProductInfo = record
    localizedDescription: string;
    localizedTitle: string;
    price: double;
    productIdentifier: string;
    downloadable: Boolean;
    isInvalid: Boolean;
  end;

{$IFDEF IOS}

  id = pointer;

  // ------------------------------------------------------------------------------
  TDPFLoadProductCompletionBlock = procedure( result: pointer; error: Pointer ) of object;

  SKStoreProductViewControllerClass = interface( UIViewControllerClass )
    ['{A93AE215-1949-4EA1-9022-FA20B91E3C21}']
  end;

  SKStoreProductViewController = interface( UIViewController )
    ['{301333E3-FEF4-4BE1-9D4F-C87A1E6F867A}']

    function delegate: pointer; cdecl;
    procedure setDelegate( delegate: pointer ); cdecl;

    procedure loadProductWithParameters( parameters: NSDictionary; completionBlock: TDPFLoadProductCompletionBlock ); cdecl;

  end;

  TSKStoreProductViewController = class( TOCGenericImport<SKStoreProductViewControllerClass, SKStoreProductViewController> )
  end;

  SKStoreProductViewControllerDelegate = interface( IObjectiveC )
    ['{3CEF5E51-1EBF-4AED-9363-5E9D477BB646}']

    procedure productViewControllerDidFinish( viewController: SKStoreProductViewController ); cdecl;
  end;

  TSKStoreProductViewControllerDelegate = class( TOCLocal, SKStoreProductViewControllerDelegate )
  private
    FDPFInAppPurchase: TDPFInAppPurchase;
  public
    constructor Create( ADPFInAppPurchase: TDPFInAppPurchase );

    procedure productViewControllerDidFinish( viewController: SKStoreProductViewController ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  SKProductClass = interface( NSObjectClass )
    ['{AB017809-CB1F-433B-9979-236A29542F1F}']
  end;

  SKProduct = interface( NSObject )
    ['{71779515-9A0C-45AB-8723-194C5CC16030}']

    function localizedDescription: NSString; cdecl;
    function localizedTitle: NSString; cdecl;
    function price: NSDecimalNumber; cdecl;
    function priceLocale: NSLocale; cdecl;
    function productIdentifier: NSString; cdecl;
    function isDownloadable: Boolean; cdecl;
  end;

  TSKProduct = class( TOCGenericImport<SKProductClass, SKProduct> )
  end;

  // ------------------------------------------------------------------------------
  SKPaymentClass = interface( NSObjectClass )
    ['{04725FC9-A9E8-4314-AE07-8652CA36A219}']

    function paymentWithProduct( product: SKProduct ): pointer; cdecl;
  end;

  SKPayment = interface( NSObject )
    ['{5C0C3117-B821-491E-9120-7AB55F4A4BBB}']

    function productIdentifier: NSString; cdecl;
    function quantity: NSInteger; cdecl;
    function requestData: NSData; cdecl;
    function applicationUsername: NSData; cdecl;
  end;

  TSKPayment = class( TOCGenericImport<SKPaymentClass, SKPayment> )
  end;

  // ------------------------------------------------------------------------------
  SKPaymentTransactionClass = interface( NSObjectClass )
    ['{39DB7613-DD9C-4890-86FE-B907D0C1196F}']

  end;

  SKPaymentTransaction = interface( NSObject )
    ['{1F761A63-4962-4C81-A888-2D0D4265A9DB}']

    function error: NSError; cdecl;
    function originalTransaction: SKPaymentTransaction; cdecl;
    function payment: SKPayment; cdecl;
    function transactionDate: NSDate; cdecl;
    function transactionIdentifier: NSString; cdecl;
    function transactionState: NativeUInt; cdecl;
    function downloads: NSArray; cdecl;
  end;

  TSKPaymentTransaction = class( TOCGenericImport<SKPaymentTransactionClass, SKPaymentTransaction> )
  end;

  // ------------------------------------------------------------------------------
  SKPaymentQueueClass = interface( NSObjectClass )
    ['{715F102F-E4C6-4CF5-A747-4F54CA268501}']

    function canMakePayments: Boolean; cdecl;
    function defaultQueue: pointer; cdecl;
  end;

  SKPaymentQueue = interface( NSObject )
    ['{57994D6A-AAD2-46DD-A553-9BEA6215E3ED}']

    procedure addPayment( payment: SKPayment ); cdecl;
    procedure addTransactionObserver( observer: pointer ); cdecl;
    procedure cancelDownloads( downloads: NSArray ); cdecl;
    procedure finishTransaction( transaction: SKPaymentTransaction ); cdecl;
    procedure pauseDownloads( downloads: NSArray ); cdecl;
    procedure removeTransactionObserver( observer: pointer ); cdecl;
    procedure restoreCompletedTransactions; cdecl;
    procedure restoreCompletedTransactionsWithApplicationUsername( username: NSString ); cdecl;
    procedure resumeDownloads( downloads: NSArray ); cdecl;
    procedure startDownloads( downloads: NSArray ); cdecl;
  end;

  TSKPaymentQueue = class( TOCGenericImport<SKPaymentQueueClass, SKPaymentQueue> )
  end;

  // ------------------------------------------------------------------------------
  SKMutablePaymentClass = interface( SKPaymentClass )
    ['{B0F7C258-2975-4D53-A7F9-BF6582311EF8}']

    function paymentWithProduct( product: SKProduct ): pointer; cdecl;
  end;

  SKMutablePayment = interface( SKPayment )
    ['{21BD7C12-5123-49D8-8A2E-03B97E192CF4}']

    function productIdentifier: NSString; cdecl;
    function quantity: NSInteger; cdecl;
    procedure setQuantity( quantity: NSInteger ); cdecl;
    function requestData: NSData; cdecl;
  end;

  TSKMutablePayment = class( TOCGenericImport<SKMutablePaymentClass, SKMutablePayment> )
  end;

  // ------------------------------------------------------------------------------
  SKRequestClass = interface( NSObjectClass )
    ['{C81028C7-59A4-400A-83C8-9ADA044A3CA6}']
  end;

  SKRequest = interface( NSObject )
    ['{70E8955B-1D22-42B7-824C-E910FD3611D5}']

    procedure cancel; cdecl;
    procedure start; cdecl;
    procedure setDelegate( delegate: id ); cdecl;
  end;

  TSKRequest = class( TOCGenericImport<SKRequestClass, SKRequest> )
  end;

  // ------------------------------------------------------------------------------
  // iOS 7 and later
  SKReceiptRefreshRequestClass = interface( SKRequestClass )
    ['{8284760C-A116-4C87-9BAE-524A1018C01A}']
  end;

  SKReceiptRefreshRequest = interface( SKRequest )
    ['{694E3FEB-32C0-44D6-BD6D-FF72E22EFCA0}']

    function initWithReceiptProperties( properties: NSDictionary ): pointer; cdecl; // Available in iOS 7.0 and later
    function receiptProperties: NSDictionary; cdecl; // Available in iOS 7.0 and later
  end;

  TSKReceiptRefreshRequest = class( TOCGenericImport<SKReceiptRefreshRequestClass, SKReceiptRefreshRequest> )
  end;

  // ----------------------------------------------------------------------------
  SKProductsRequestClass = interface( SKRequestClass )
    ['{A7713708-F864-48C3-8D2F-7EF8F6674D9C}']
  end;

  SKProductsRequest = interface( SKRequest )
    ['{68CAD932-2B76-4EBF-8F8D-1AC14152B859}']

    function initWithProductIdentifiers( productIdentifiers: NSSet ): pointer; cdecl;
    function delegate: id; cdecl;
    procedure setDelegate( delegate: id ); cdecl;
  end;

  TSKProductsRequest = class( TOCGenericImport<SKProductsRequestClass, SKProductsRequest> )
  end;

  // ----------------------------------------------------------------------------
  SKProductsResponseClass = interface( NSObjectClass )
    ['{A6144986-B64E-4E0C-A42E-88FB0B6ABC98}']
  end;

  SKProductsResponse = interface( NSObject )
    ['{E77F5A47-D3E2-4543-96DF-F6DEC643165B}']

    function products: NSArray; cdecl;
    function invalidProductIdentifiers: NSArray; cdecl;
  end;

  TSKProductsResponse = class( TOCGenericImport<SKProductsResponseClass, SKProductsResponse> )
  end;

  // ----------------------------------------------------------------------------
  SKDownloadClass = interface( NSObjectClass )
    ['{3624886A-D996-4E08-A331-133DB7BC67EC}']
    function contentURLForProductID( productID: NSSTring ): NSURL; cdecl;
    procedure deleteContentForProductID( productID: NSString ); cdecl;
  end;

  SKDownload = interface( NSObject )
    ['{CBB35B26-F396-4DE5-B3C1-CC9B76E73F2D}']

    function contentIdentifier: NSString; cdecl;
    function contentLength: NativeUInt; cdecl;
    function contentURL: NSUrl; cdecl;
    function contentVersion: NSString; cdecl;
    function error: NSError; cdecl;
    function progress: Single; cdecl;
    function state: NativeUInt; cdecl;
    function timeRemaining: NSTimeInterval; cdecl;
  end;

  TSKDownload = class( TOCGenericImport<SKDownloadClass, SKDownload> )
  end;

  NSArray1 = interface( NSArray )
  end;

  NSArray2 = interface( NSArray )
  end;

  // ----------------------------------------------------------------------------
  SKPaymentTransactionObserver = interface( IObjectiveC )
    ['{6D1A4216-7A28-44B6-A704-E4199E76DBFB}']
    procedure paymentQueue( queue: SKPaymentQueue; updatedTransactions: NSArray ); cdecl; overload;
    procedure paymentQueue( queue: SKPaymentQueue; removedTransactions: NSArray1 ); cdecl; overload;
    procedure paymentQueue( queue: SKPaymentQueue; updatedDownloads: NSArray2 ); cdecl; overload;

    procedure paymentQueue( queue: SKPaymentQueue; restoreCompletedTransactionsFailedWithError: NSError ); cdecl; overload;
    procedure paymentQueueRestoreCompletedTransactionsFinished( queue: SKPaymentQueue ); cdecl;

  end;

  TSKPaymentTransactionObserver = class( TOCLocal, SKPaymentTransactionObserver )
  private
    FDPFInAppPurchase: TDPFInAppPurchase;
  public
    constructor Create( ADPFInAppPurchase: TDPFInAppPurchase );

    procedure paymentQueue( queue: SKPaymentQueue; updatedTransactions: NSArray ); overload; cdecl;
    procedure paymentQueue( queue: SKPaymentQueue; removedTransactions: NSArray1 ); overload; cdecl;
    procedure paymentQueue( queue: SKPaymentQueue; updatedDownloads: NSArray2 ); overload; cdecl;

    procedure paymentQueue( queue: SKPaymentQueue; restoreCompletedTransactionsFailedWithError: NSError ); overload; cdecl;
    procedure paymentQueueRestoreCompletedTransactionsFinished( queue: SKPaymentQueue ); cdecl;
  end;


  // ----------------------------------------------------------------------------

  SKRequestDelegate = interface( IObjectiveC )
    ['{FCDD751D-4BD2-46C6-987E-4B1C3C568B5F}']
    procedure request( request: SKRequest; didFailWithError: NSError ); cdecl;
    procedure requestDidFinish( request: SKRequest ); cdecl;
  end;

  // ----------------------------------------------------------------------------

  SKProductsRequestDelegate = interface( SKRequestDelegate )
    ['{DE7F8C36-6BA3-440B-9475-BAA02635B124}']

    procedure productsRequest( request: SKProductsRequest; didReceiveResponse: SKProductsResponse ); cdecl;
  end;

  TSKProductsRequestDelegate = class( TOCLocal, SKProductsRequestDelegate )
  private
    FDPFInAppPurchase: TDPFInAppPurchase;
  public
    constructor Create( ADPFInAppPurchase: TDPFInAppPurchase );

    procedure request( request: SKRequest; didFailWithError: NSError ); cdecl;
    procedure requestDidFinish( request: SKRequest ); cdecl;
    procedure productsRequest( request: SKProductsRequest; didReceiveResponse: SKProductsResponse ); cdecl;
  end;

  // ----------------------------------------------------------------------------
{$ENDIF}

  TDPFOnCloseAppStoreProduct = procedure( sender: TObject ) of object;

  TDPFOnProductsRequest                      = procedure( sender: TObject; Products: array of TProductInfo; InvalidProducts: array of string ) of object;
  TDPFOnProductsRequestFinished              = procedure( sender: TObject ) of object;
  TDPFOnRestoreCompletedTransactionsFinished = procedure( sender: TObject ) of object;
  TDPFOnCompletedTransactionsFailedWithError = procedure( sender: TObject; Error: string ) of object;
  TDPFOnProductsRequestError                 = procedure( sender: TObject; Error: string ) of object;
  TDPFOnUpdatedTransactions                  = procedure( Sender: TObject; productIdentifier: string; State: Longword; var isFinishTransaction: Boolean; ErrorStr: string; ErrorCode: Integer ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFInAppPurchase = class( TComponent )
  private
    FOnProductsRequest                     : TDPFOnProductsRequest;
    FOnProductsRequestError                : TDPFOnProductsRequestError;
    FOnProductsRequestFinished             : TDPFOnProductsRequestFinished;
    FOnUpdatedTransactions                 : TDPFOnUpdatedTransactions;
    FOnRestoreCompletedTransactionsFinished: TDPFOnRestoreCompletedTransactionsFinished;
    FOnCompletedTransactionsFailedWithError: TDPFOnCompletedTransactionsFailedWithError;
{$IFDEF IOS}
    FProductsRequest             : SKProductsRequest;
    FSKProductsRequestDelegate   : TSKProductsRequestDelegate;
    FSKPaymentTransactionObserver: TSKPaymentTransactionObserver;

    storeViewController                  : SKStoreProductViewController;
    FSKStoreProductViewControllerDelegate: TSKStoreProductViewControllerDelegate;

    FOnCloseAppStoreRate: TDPFOnCloseAppStoreProduct;

    FInAppPurchaseList: NSArray;
    procedure LoadProductCompletionBlock( result: pointer; error: Pointer );
{$ENDIF}
  public
    { Public declarations }
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;

{$IFDEF IOS}
    procedure FetchAvailableProducts( ProductsID: array of string );
    function PurchaseProduct( ProductID: string ): Boolean;
    procedure RestoreCompletedTransactions;
    procedure ShowProductInAppStore( AppID: NativeInt; OnClose: TDPFOnCloseAppStoreProduct );
{$ENDIF}
  published
    property OnProductsRequest                     : TDPFOnProductsRequest read FOnProductsRequest write FOnProductsRequest;
    property OnProductsRequestFinished             : TDPFOnProductsRequestFinished read FOnProductsRequestFinished write FOnProductsRequestFinished;
    property OnRestoreCompletedTransactionsFinished: TDPFOnRestoreCompletedTransactionsFinished read FOnRestoreCompletedTransactionsFinished write FOnRestoreCompletedTransactionsFinished;
    property OnCompletedTransactionsFailedWithError: TDPFOnCompletedTransactionsFailedWithError read FOnCompletedTransactionsFailedWithError write FOnCompletedTransactionsFailedWithError;
    property OnProductsRequestError                : TDPFOnProductsRequestError read FOnProductsRequestError write FOnProductsRequestError;
    property OnUpdatedTransactions                 : TDPFOnUpdatedTransactions read FOnUpdatedTransactions write FOnUpdatedTransactions;
  end;

{$IFDEF IOS}

function SKStoreProductParameterITunesItemIdentifier: NSString;
{$ENDIF}

// ----------------------------------------------------------------------------
implementation

{$IFDEF IOS}
{$IF Not defined(CPUARM)}

uses Posix.Dlfcn;

var
  storeKitModule: THandle;

  // ------------------------------------------------------------------------------

{$ENDIF}

  // ------------------------------------------------------------------------------
function SKStoreProductParameterITunesItemIdentifier: NSString;
begin
  result := CocoaNSStringConst( storeKitFwk, 'SKStoreProductParameterITunesItemIdentifier' );
end;

// ------------------------------------------------------------------------------
constructor TSKStoreProductViewControllerDelegate.Create( ADPFInAppPurchase: TDPFInAppPurchase );
begin
  inherited create;
  FDPFInAppPurchase := ADPFInAppPurchase;
end;

// ------------------------------------------------------------------------------
procedure TSKStoreProductViewControllerDelegate.productViewControllerDidFinish( viewController: SKStoreProductViewController ); cdecl;
begin
  if Assigned( viewController ) then
    viewController.dismissModalViewControllerAnimated( true );
  if Assigned( FDPFInAppPurchase.FOnCloseAppStoreRate ) then
    FDPFInAppPurchase.FOnCloseAppStoreRate( FDPFInAppPurchase );
end;

// ------------------------------------------------------------------------------
constructor TSKProductsRequestDelegate.Create( ADPFInAppPurchase: TDPFInAppPurchase );
begin
  inherited Create;
  FDPFInAppPurchase := ADPFInAppPurchase;
end;

// ------------------------------------------------------------------------------
procedure TSKProductsRequestDelegate.request( request: SKRequest; didFailWithError: NSError ); cdecl;
begin
  if Assigned( FDPFInAppPurchase.OnProductsRequestError ) then
    FDPFInAppPurchase.OnProductsRequestError( FDPFInAppPurchase, NSStrToStr( didFailWithError.localizedDescription ) );
end;

// ------------------------------------------------------------------------------
procedure TSKProductsRequestDelegate.requestDidFinish( request: SKRequest ); cdecl;
begin
  if Assigned( FDPFInAppPurchase.OnProductsRequestFinished ) then
    FDPFInAppPurchase.OnProductsRequestFinished( FDPFInAppPurchase );
end;

// ------------------------------------------------------------------------------
procedure TSKProductsRequestDelegate.ProductsRequest( request: SKProductsRequest; didReceiveResponse: SKProductsResponse ); cdecl;
var
  validProduct       : SKProduct;
  InValidProduct     : NSString;
  count              : NSUInteger;
  ProductsInfo       : array of TProductInfo;
  InvalidProductsInfo: array of string;
  I                  : Integer;
begin

  if assigned( FDPFInAppPurchase.FInAppPurchaseList ) then
    FDPFInAppPurchase.FInAppPurchaseList.release;

  FDPFInAppPurchase.FInAppPurchaseList := nil;
  count                                := didReceiveResponse.invalidProductIdentifiers.count;
  SetLength( InvalidProductsInfo, Count );
  for i := 0 to Count - 1 do
  begin
    InValidProduct         := TNSString.Wrap( didReceiveResponse.invalidProductIdentifiers.objectAtIndex( i ) );
    InvalidProductsInfo[I] := UTF8ToString( InValidProduct.UTF8String );
  end;

  count := didReceiveResponse.products.count;
  if count > 0 then
  begin
    FDPFInAppPurchase.FInAppPurchaseList := TNSArray.Wrap( TNSArray.OCClass.arrayWithArray( didReceiveResponse.products ) );
    FDPFInAppPurchase.FInAppPurchaseList.retain;
    SetLength( ProductsInfo, count );
  end;

  for i := 0 to Count - 1 do
  begin
    validProduct                         := TSKProduct.Wrap( didReceiveResponse.products.objectAtIndex( i ) );
    ProductsInfo[i].localizedDescription := NSStrToStr( validProduct.localizedDescription );
    ProductsInfo[i].localizedTitle       := NSStrToStr( validProduct.localizedTitle );
    ProductsInfo[i].productIdentifier    := NSStrToStr( validProduct.productIdentifier );
    ProductsInfo[i].price                := validProduct.price.doubleValue;
    ProductsInfo[i].downloadable         := validProduct.isDownloadable;
  end;

  if Assigned( FDPFInAppPurchase.FOnProductsRequest ) then
    FDPFInAppPurchase.FOnProductsRequest( FDPFInAppPurchase, ProductsInfo, InvalidProductsInfo );
end;

// ------------------------------------------------------------------------------
// TSKPaymentTransactionObserver
constructor TSKPaymentTransactionObserver.Create( ADPFInAppPurchase: TDPFInAppPurchase );
begin
  inherited create;
  FDPFInAppPurchase := ADPFInAppPurchase;
end;

// ------------------------------------------------------------------------------
procedure TSKPaymentTransactionObserver.paymentQueue( queue: SKPaymentQueue; updatedTransactions: NSArray ); cdecl;
var
  i                  : Integer;
  transaction        : SKPaymentTransaction;
  isFinishTransaction: Boolean;
  productIdentifier  : string;
  ErrStr             : string;
  ecode              : Integer;
begin

  for i := 0 to updatedTransactions.count - 1 do
  begin
    transaction := TSKPaymentTransaction.Wrap( updatedTransactions.objectAtIndex( i ) );

    isFinishTransaction := true;
    ErrStr              := '';
    ecode               := 0;
    if transaction.transactionState = SKPaymentTransactionStateFailed then
      if Assigned( transaction.error ) then
      begin
        ErrStr := UTF8ToString( transaction.error.localizedDescription.UTF8String );
        ecode  := transaction.error.code;
      end;

    productIdentifier := '';
    if assigned( transaction.payment ) and assigned( transaction.payment.productIdentifier ) then
      productIdentifier := UTF8ToString( transaction.payment.productIdentifier.UTF8String );

    if Assigned( FDPFInAppPurchase.FOnUpdatedTransactions ) then
      FDPFInAppPurchase.FOnUpdatedTransactions( FDPFInAppPurchase, productIdentifier, transaction.transactionState, isFinishTransaction, ErrStr, ecode );

    if isFinishTransaction and ( transaction.transactionState <> SKPaymentTransactionStatePurchasing ) then
      TSKPaymentQueue.Wrap( TSKPaymentQueue.OCClass.defaultQueue ).finishTransaction( transaction );
  end;
end;

// ------------------------------------------------------------------------------
procedure TSKPaymentTransactionObserver.paymentQueue( queue: SKPaymentQueue; removedTransactions: NSArray1 ); cdecl;
begin
end;

// ------------------------------------------------------------------------------
procedure TSKPaymentTransactionObserver.paymentQueue( queue: SKPaymentQueue; updatedDownloads: NSArray2 ); cdecl;
begin
end;

// ------------------------------------------------------------------------------
procedure TSKPaymentTransactionObserver.paymentQueue( queue: SKPaymentQueue; restoreCompletedTransactionsFailedWithError: NSError ); cdecl;
var
  Error: string;
begin
  Error := '';
  if Assigned( restoreCompletedTransactionsFailedWithError ) then
    Error := UTF8ToString( restoreCompletedTransactionsFailedWithError.localizedDescription.UTF8String );
  if assigned( FDPFInAppPurchase.FOnCompletedTransactionsFailedWithError ) then
    FDPFInAppPurchase.FOnCompletedTransactionsFailedWithError( FDPFInAppPurchase, Error );
end;

// ------------------------------------------------------------------------------
procedure TSKPaymentTransactionObserver.paymentQueueRestoreCompletedTransactionsFinished( queue: SKPaymentQueue ); cdecl;
begin
  if assigned( FDPFInAppPurchase.FOnRestoreCompletedTransactionsFinished ) then
    FDPFInAppPurchase.FOnRestoreCompletedTransactionsFinished( FDPFInAppPurchase );
end;

{$ENDIF}

// ------------------------------------------------------------------------------
{ TDPFInAppPurchase }
constructor TDPFInAppPurchase.Create( AOwner: TComponent );
begin
  inherited;
{$IFDEF IOS}
  FSKPaymentTransactionObserver := TSKPaymentTransactionObserver.Create( Self );
  TSKPaymentQueue.Wrap( TSKPaymentQueue.OCClass.defaultQueue ).addTransactionObserver( FSKPaymentTransactionObserver.GetObjectID );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFInAppPurchase.Destroy;
begin
{$IFDEF IOS}
  TSKPaymentQueue.Wrap( TSKPaymentQueue.OCClass.defaultQueue ).removeTransactionObserver( FSKPaymentTransactionObserver.GetObjectID );
  if Assigned( FSKProductsRequestDelegate ) then
    FSKProductsRequestDelegate.DisposeOf;

  if Assigned( FSKPaymentTransactionObserver ) then
    FSKPaymentTransactionObserver.DisposeOf;

  if assigned( FProductsRequest ) then
    FProductsRequest.release;

  if assigned( FSKStoreProductViewControllerDelegate ) then
    FSKStoreProductViewControllerDelegate.DisposeOf;

{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

procedure TDPFInAppPurchase.FetchAvailableProducts( ProductsID: array of string );
var
  productIdentifiers: NSSet;
  NS                : NSMutableArray;
  S                 : string;
  I                 : Integer;
begin
  NS    := TNSMutableArray.Create;
  for I := 0 to high( ProductsID ) do
  begin
    S := ProductsID[I];
    NS.addObject( ( NSStr( S ) as ILocalObject ).GetObjectID );
  end;

  productIdentifiers := TNSSet.Wrap( TNSSet.OCClass.setWithArray( NS ) );

  if assigned( FProductsRequest ) then
    FProductsRequest.release;

  if not Assigned( FSKProductsRequestDelegate ) then
    FSKProductsRequestDelegate := TSKProductsRequestDelegate.Create( self );

  FProductsRequest := TSKProductsRequest.Wrap( TSKProductsRequest.alloc.initWithProductIdentifiers( productIdentifiers ) );
  FProductsRequest.setDelegate( FSKProductsRequestDelegate.GetObjectID );
  FProductsRequest.start;
  NS.release;
end;

// ------------------------------------------------------------------------------
procedure TDPFInAppPurchase.LoadProductCompletionBlock( result: pointer; error: Pointer );
begin
  if Integer( result ) = 1 then
    GetSharedApplication.keyWindow.rootViewController.presentModalViewController( storeViewController, true );
end;

// ------------------------------------------------------------------------------
procedure TDPFInAppPurchase.ShowProductInAppStore( AppID: NativeInt; OnClose: TDPFOnCloseAppStoreProduct );
var
  parameters: NSDictionary;
begin
  if TOSVersion.Major > 6 then
  begin
    // TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication ).openURL( TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( 'http://phobos.apple.com/WebObjects/MZStore.woa/wa/viewSoftware?id=' + AppID.ToString + '&mt=8' ) ) ) );
    TUIApplication.Wrap( TUIApplication.OCClass.sharedApplication ).openURL( TNSURL.Wrap( TNSURL.OCClass.URLWithString( NSStr( 'http://itunes.apple.com/WebObjects/MZStore.woa/wa/viewContentsUserReviews?id=' + AppID.ToString + '&pageNumber=0&sortOrdering=1&type=Purple+Software' ) ) ) );
    TThread.Queue( nil,
      procedure( )
      begin
        Sleep( 6000 );
        if assigned( OnClose ) then
          OnClose( self );
      end );
    exit;
  end;

  FOnCloseAppStoreRate := OnClose;

  if Assigned( storeViewController ) then
    storeViewController.release;
  storeViewController := TSKStoreProductViewController.Wrap( TSKStoreProductViewController.alloc.init );

  if not assigned( FSKStoreProductViewControllerDelegate ) then
    FSKStoreProductViewControllerDelegate := TSKStoreProductViewControllerDelegate.Create( self );

  storeViewController.setDelegate( FSKStoreProductViewControllerDelegate.GetObjectID );

  parameters := TNSMutableDictionary.Wrap( TNSMutableDictionary.OCClass.dictionaryWithObject( TNSNumber.OCClass.numberWithInteger( AppID ), ( SKStoreProductParameterITunesItemIdentifier as ILocalObject ).GetObjectID ) );
  storeViewController.loadProductWithParameters( parameters, LoadProductCompletionBlock );
end;

// ------------------------------------------------------------------------------
function TDPFInAppPurchase.PurchaseProduct( ProductID: string ): Boolean;
var
  i      : Integer;
  product: SKProduct;
  payment: SKPayment;
begin
  result := false;
  if not Assigned( FInAppPurchaseList ) then
    exit;

  for i := 0 to FInAppPurchaseList.count - 1 do
  begin
    product := TSKProduct.Wrap( FInAppPurchaseList.objectAtIndex( i ) );
    if product.productIdentifier.isEqualToString( NSStr( ProductID ) ) then
      break;
    product := nil;
  end;

  if product = nil then
    exit;

  payment := TSKPayment.Wrap( TSKPayment.OCClass.paymentWithProduct( product ) );
  TSKPaymentQueue.Wrap( TSKPaymentQueue.OCClass.defaultQueue ).addPayment( payment );
end;

// ------------------------------------------------------------------------------
procedure TDPFInAppPurchase.RestoreCompletedTransactions;
begin
  TSKPaymentQueue.Wrap( TSKPaymentQueue.OCClass.defaultQueue ).restoreCompletedTransactions;
end;

{$ENDIF}
// ------------------------------------------------------------------------------
{$IFDEF IOS}
{$IF defined(CPUARM)}
procedure iAdLoaderLoader; cdecl; external storeKitFwk;
{$ELSE}

initialization

storeKitModule := dlopen( MarshaledAString( storeKitFwk ), RTLD_LAZY );

finalization

dlclose( storeKitModule );
{$ENDIF}
{$ENDIF}

end.
