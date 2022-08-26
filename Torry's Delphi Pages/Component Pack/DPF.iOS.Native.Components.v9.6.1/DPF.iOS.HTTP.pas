// ------------------------------------------------------------------------------
// DPF.iOS.HTTP Component
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

// ------------------------------------------------------------------------------
// HTML 4.0 POST data sample:
// ------------------------------------------------------------------------------
(*
  Content-Type: multipart/form-data; boundary=AaB03x

  --AaB03x
  Content-Disposition: form-data; name="submit-name"

  Larry
  --AaB03x
  Content-Disposition: form-data; name="files"
  Content-Type: multipart/mixed; boundary=BbC04y

  --BbC04y
  Content-Disposition: file; filename="file1.txt"
  Content-Type: text/plain

  ... contents of file1.txt ...
  --BbC04y
  Content-Disposition: file; filename="file2.gif"
  Content-Type: image/gif
  Content-Transfer-Encoding: binary

  ...contents of file2.gif...
  --BbC04y--
  --AaB03x--
*)
// ------------------------------------------------------------------------------

unit DPF.iOS.HTTP;

interface

{$I DPF.iOS.Defs.inc}

uses
  System.Classes,
  System.SysUtils,
  System.TypInfo,
  DPF.iOS.BaseControl,
  DPF.iOS.NSOperationQueue,
{$IFDEF IOS}
  Macapi.ObjCRuntime,
  DPF.iOS.Common,
  DPF.iOS.Classes,

  iOSapi.UIKit,
  Macapi.ObjectiveC,
  iOSapi.CocoaTypes,
  iOSapi.CoreGraphics,
  iOSapi.Foundation,
{$ENDIF}
  FMX.Forms,
  FMX.Dialogs;

type
  TDPFHttp = class;
{$IFDEF IOS}
  // ----------------------------------------------------------------------------

  NSURLAuthenticationChallenge1 = interface( NSURLAuthenticationChallenge )
  end;

  NSURLAuthenticationChallengeSenderClass = interface( NSObjectClass )
    ['{09D6CF3B-864E-4C62-A8DF-C4426CDEE8FB}']
  end;

  NSURLAuthenticationChallengeSender = interface( NSObject )
    ['{4503CADC-8784-4F4F-9B3F-1A615F53E3A8}']
    procedure cancelAuthenticationChallenge( challenge: NSURLAuthenticationChallenge ); cdecl;
    procedure continueWithoutCredentialForAuthenticationChallenge( challenge: NSURLAuthenticationChallenge ); cdecl;
    procedure performDefaultHandlingForAuthenticationChallenge( challenge: NSURLAuthenticationChallenge ); cdecl;
    procedure rejectProtectionSpaceAndContinueWithChallenge( challenge: NSURLAuthenticationChallenge ); cdecl;
    procedure useCredential( credential: NSURLCredential; forAuthenticationChallenge: NSURLAuthenticationChallenge ); cdecl;
  end;

  TNSURLAuthenticationChallengeSender = class( TOCGenericImport<NSURLAuthenticationChallengeSenderClass, NSURLAuthenticationChallengeSender> )
  end;

  NSURLConnectionDataDelegate = interface( IObjectiveC )
    ['{EAC573C7-A086-4411-954F-847383C18924}']
    procedure connectionDidFinishLoading( connection: NSURLConnection ); cdecl;
    procedure connection( connection: NSURLConnection; didReceiveData: NSData ); cdecl; overload;
    procedure connection( connection: NSURLConnection; didReceiveResponse: NSURLResponse ); cdecl; overload;
    procedure connection( connection: NSURLConnection; didFailWithError: NSError ); cdecl; overload; // Sometime Raise Error on Deploying Device
    procedure connection( connection: NSURLConnection; didSendBodyData: NSInteger; { bytesWritten }
      totalBytesWritten: NSInteger; { totalBytesWritten }
      totalBytesExpectedToWrite: NSInteger { totalBytesExpectedToWrite } ); cdecl; overload;

    procedure connection( connection: NSURLConnection; willSendRequestForAuthenticationChallenge: NSURLAuthenticationChallenge ); cdecl; overload;
    function connection( connection: NSURLConnection; canAuthenticateAgainstProtectionSpace: NSURLProtectionSpace ): Boolean; cdecl; overload;

    // Note: This method is not called if the delegate implements the connection:willSendRequestForAuthenticationChallenge: method.
    // procedure connection( connection: NSURLConnection; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge1 ); cdecl; overload;

  end;

  TNSURLConnectionDataDelegate = class( TOCLocal, NSURLConnectionDataDelegate )
  private
    FDPFHttp: TDPFHttp;
    Buffer  : NSMutableData;
  public
    constructor Create( AOwner: TDPFHttp );
    destructor Destroy; override;

    // -------------------------------------------------------------------------
    procedure connection( connection: NSURLConnection; didFailWithError: NSError ); overload; cdecl;
    procedure connection( connection: NSURLConnection; didSendBodyData: NSInteger; { bytesWritten } totalBytesWritten: NSInteger; { totalBytesWritten } totalBytesExpectedToWrite: NSInteger { totalBytesExpectedToWrite } ); overload; cdecl;
    procedure connection( connection: NSURLConnection; didReceiveData: NSData ); overload; cdecl;
    procedure connection( connection: NSURLConnection; didReceiveResponse: NSURLResponse ); overload; cdecl;
    procedure connectionDidFinishLoading( connection: NSURLConnection ); cdecl;

    procedure connection( connection: NSURLConnection; willSendRequestForAuthenticationChallenge: NSURLAuthenticationChallenge ); overload; cdecl;
    function connection( connection: NSURLConnection; canAuthenticateAgainstProtectionSpace: NSURLProtectionSpace ): Boolean; overload; cdecl;

    // Note: This method is not called if the delegate implements the connection:willSendRequestForAuthenticationChallenge: method.
    // procedure connection( connection: NSURLConnection; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge1 ); overload; cdecl;
    // -------------------------------------------------------------------------
  end;

  // ------------------------------------------------------------------------------
  THTTPHeader = record
    Field: string;
    Value: string;
  end;

  THTTPFormField = record
    Field: string;
    Value: string;
  end;

  THTTPFormData = record
    Name: string;
    FileName: string;
    Data: NSData;
    ContentType: string;
  end;

  THTTPFormImage = record
    Name: string;
    FileName: string;
    Image: UIImage;
    ContentType: string;
  end;

  THTTPFormFiles = record
    Name: string;
    DestFileName: string;
    SourceFileName: string;
    ContentType: string;
  end;

  // ----------------------------------------------------------------------------
{$ENDIF}

  TOnReceiveDataStream      = procedure( Sender: TObject; Data: TStream; var isFree: Boolean ) of object;
  TOnReceiveData            = procedure( Sender: TObject; Data: string; var isFree: Boolean ) of object;
  TDPFHttpOnError           = procedure( Sender: TObject; Error: string; var isFree: Boolean ) of object;
  TDPFHttpOnReceiveProgress = procedure( Sender: TObject; const DownloadSize: Int64; const DownloadedSize: Int64 ) of object;
  TDPFHttpOnSendProgress    = procedure( Sender: TObject; const SendSize: Int64; const SendededSize: Int64 ) of object;
  TDPFHttpOnFinishLoading   = procedure( Sender: TObject ) of object;
  TDPFHttpOnCanAuthenticate = procedure( Sender: TObject; var canAuthenticate: Boolean ) of object;

{$IFDEF IOS}
  TOnReceiveDataBin = procedure( Sender: TObject; Data: NSData; var isFree: Boolean ) of object;
{$ENDIF}
  TDPFNSURLRequestCache = ( rcUseProtocolCachePolicy = 0, rcReloadIgnoringLocalCacheData = 1, rcReturnCacheDataElseLoad = 2, rcReturnCacheDataDontLoad = 3, rcReloadIgnoringLocalAndRemoteCacheData = 4, rcReloadRevalidatingCacheData = 5 );

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFHttp = class( TComponent )
  private
    FOnError            : TDPFHttpOnError;
    FOnReceiveData      : TOnReceiveData;
    FConnectionTimeout  : Integer;
    FBusy               : Boolean;
    FWaitForFree        : Boolean;
    FLastURL            : string;
    FLastStatusCode     : Integer;
    FOperationQueue     : TDPFNSOperationQueue;
    FUserPassword       : string;
    FUserName           : string;
    FAuthFailureCount   : Integer;
    FOnReceiveProgress  : TDPFHttpOnReceiveProgress;
    FURLRequestCache    : TDPFNSURLRequestCache;
    FOnReceiveDataStream: TOnReceiveDataStream;
    FOnSendProgress     : TDPFHttpOnSendProgress;
    FOnFinishLoading    : TDPFHttpOnFinishLoading;
    FOnCanAuthenticate  : TDPFHttpOnCanAuthenticate;
{$IFDEF IOS}
    FDownloadSize          : Int64;
    FDownloadedSize        : Int64;
    FOnReceiveDataBin      : TOnReceiveDataBin;
    FConnectionDataDelegate: TNSURLConnectionDataDelegate;
    FConnection            : NSURLConnection;
    // _networkQueue          : NSOperationQueue;
{$ENDIF}
  protected
  public
{$IFDEF IOS}
    property OnReceiveDataBin: TOnReceiveDataBin read FOnReceiveDataBin write FOnReceiveDataBin;
    function GetUrlContentData( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; FormDatas: array of THTTPFormData; FormFields: array of THTTPFormField; ASync: Boolean = false; HTTPMethod: string = 'GET'; FHttpBody: string = '' ): NSData;
    function GetUrlContentString( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; ASync: Boolean = false; HTTPMethod: string = 'GET'; FHttpBody: string = '' ): string;
    function GetUrlContentImage( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; ASync: Boolean = false; HTTPMethod: string = 'GET' ): UIImage;
    function PutData( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; Datas: array of THTTPFormData; FormFields: array of THTTPFormField; ASync: Boolean = false ): string;
    function PostData( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; Datas: array of NSData; FormData: array of THTTPFormData; ASync: Boolean = false ): string;
    function PostImages( const HTTPURL: string; Images: array of THTTPFormImage; Fields: array of THTTPFormField; ASync: Boolean = false ): string;
    function PostFiles( const HTTPURL: string; Files: array of THTTPFormFiles; Fields: array of THTTPFormField; ASync: Boolean = false ): string;
    procedure ConnectionCancel;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published
    property ConnectionTimeout  : Integer read FConnectionTimeout write FConnectionTimeout default 20; // 20 Second
    property OnReceiveData      : TOnReceiveData read FOnReceiveData write FOnReceiveData;
    property OnReceiveDataStream: TOnReceiveDataStream read FOnReceiveDataStream write FOnReceiveDataStream;
    property OnError            : TDPFHttpOnError read FOnError write FOnError;
    property OnReceiveProgress  : TDPFHttpOnReceiveProgress read FOnReceiveProgress write FOnReceiveProgress;
    property OnSendProgress     : TDPFHttpOnSendProgress read FOnSendProgress write FOnSendProgress;
    property OnFinishLoading    : TDPFHttpOnFinishLoading read FOnFinishLoading write FOnFinishLoading;
    property OnCanAuthenticate  : TDPFHttpOnCanAuthenticate read FOnCanAuthenticate write FOnCanAuthenticate;

    property Busy            : Boolean read FBusy;
    property LastURL         : string read FLastURL;
    property OperationQueue  : TDPFNSOperationQueue read FOperationQueue write FOperationQueue;
    property UserName        : string read FUserName write FUserName;
    property UserPassword    : string read FUserPassword write FUserPassword;
    property AuthFailureCount: Integer read FAuthFailureCount write FAuthFailureCount default 1;

    property URLRequestCache: TDPFNSURLRequestCache read FURLRequestCache write FURLRequestCache default rcUseProtocolCachePolicy;
  end;
  // ----------------------------------------------------------------------------

{$IFDEF IOS}

function BuildHeaderRecord( Field: string; Value: string ): THTTPHeader;
function BuildFormFieldRecord( Field: string; Value: string ): THTTPFormField;
function BuildFormDataRecord( Name: string; FileName: string; Data: NSData; ContentType: string ): THTTPFormData;
function BuildFormImageRecord( Name: string; FileName: string; Image: UIImage; ContentType: string ): THTTPFormImage;
function BuildFormFilesRecord( Name: string; DestFileName: string; SourceFileName: string; ContentType: string ): THTTPFormFiles;
{$ENDIF}

implementation

{$IFDEF IOS}

const
  _CR = #13#10;

  // ------------------------------------------------------------------------------
function BuildHeaderRecord( Field: string; Value: string ): THTTPHeader;
begin
  Result.Field := Field;
  Result.Value := Value;
end;

// ------------------------------------------------------------------------------
function BuildFormFieldRecord( Field: string; Value: string ): THTTPFormField;
begin
  Result.Field := Field;
  Result.Value := Value;
end;

// ------------------------------------------------------------------------------
function BuildFormDataRecord( Name: string; FileName: string; Data: NSData; ContentType: string ): THTTPFormData;
begin
  Result.Name        := name;
  Result.FileName    := FileName;
  Result.Data        := Data;
  Result.ContentType := ContentType;
end;

// ------------------------------------------------------------------------------
function BuildFormImageRecord( Name: string; FileName: string; Image: UIImage; ContentType: string ): THTTPFormImage;
begin
  Result.Name        := name;
  Result.FileName    := FileName;
  Result.Image       := Image;
  Result.ContentType := ContentType;
end;

// ------------------------------------------------------------------------------
function BuildFormFilesRecord( Name: string; DestFileName: string; SourceFileName: string; ContentType: string ): THTTPFormFiles;
begin
  Result.Name           := name;
  Result.DestFileName   := DestFileName;
  Result.SourceFileName := SourceFileName;
  Result.ContentType    := ContentType;
end;
{$ENDIF}

// ----------------------------------------------------------------------------
constructor TDPFHttp.Create( AOwner: TComponent );
begin
{$IFDEF IOS}
  DPFNSLog( 'TDPFHttp.Create: ' + name );
{$ENDIF}
  inherited Create( AOwner );
  FConnectionTimeout := 20;
  FWaitForFree       := False;
  FLastStatusCode    := 0;
  FAuthFailureCount  := 1;
  FURLRequestCache   := rcUseProtocolCachePolicy;
{$IFDEF IOS}
{$ENDIF}
end;

// ----------------------------------------------------------------------------
destructor TDPFHttp.Destroy;
begin
{$IFDEF IOS}
  DPFNSLog( 'TDPFHttp.Destroy' );
  try
    { _networkQueue.cancelAllOperations;
      _networkQueue.release; }
    ConnectionCancel;
  except
    DPFNSLog( 'TDPFHttp.Destroy except' );
    FWaitForFree := True;
    exit;
  end;
  if Assigned( FConnectionDataDelegate ) then
  begin
    FConnectionDataDelegate.DisposeOf;
    FConnectionDataDelegate := nil;
  end;
{$ENDIF}
  inherited;
end;

{$IFDEF IOS}

// ----------------------------------------------------------------------------
procedure TDPFHttp.ConnectionCancel;
begin
  if Assigned( FConnection ) then
  begin
    FConnection.cancel;
    // FConnection.release;
    FConnection := nil;
    FBusy       := False;
  end;
  if Assigned( FConnectionDataDelegate ) then
  begin
    FConnectionDataDelegate.DisposeOf;
    FConnectionDataDelegate := nil;
  end;
  if FWaitForFree then
    Destroy;
end;

// ----------------------------------------------------------------------------
// HTTPURL: String ; //Sample:  http://www.dpfaragir.com
// HTTPHeader
function TDPFHttp.GetUrlContentData( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; FormDatas: array of THTTPFormData; FormFields: array of THTTPFormField; ASync: Boolean = false; HTTPMethod: string = 'GET'; FHttpBody: string = '' ): NSData;
var
  Error        : PPointer;
  I            : Integer;
  FURLRequest  : NSMutableURLRequest;
  FResponsePtr : PPointer;
  postBody     : NSMutableData;
  FURL         : NSURL;
  isFree       : Boolean;
  boundary     : string;
  Innerboundary: string;
begin
  isFree := false;
  Result := nil;
  if FBusy or FWaitForFree then
    exit;
  DPFNSLog( 'TDPFHttp.GetUrlContentData' );

  FLastURL := HTTPURL;

  FBusy := True;
  FURL  := TNSURL.Wrap( TNSURL.OCClass.URLWithString( DPF.iOS.Common.NSStr( HTTPURL ).stringByAddingPercentEscapesUsingEncoding( NSUTF8StringEncoding ) ) );

  FURLRequest := TNSMutableURLRequest.Wrap( TNSMutableURLRequest.OCClass.requestWithURL( FURL, Integer( FURLRequestCache ), FConnectionTimeout ) );

  FURLRequest.setHTTPMethod( DPF.iOS.Common.NSStr( HTTPMethod ) );
  for I := low( HTTPHeader ) to high( HTTPHeader ) do
  begin
    FURLRequest.setValue( DPF.iOS.Common.NSStr( HTTPHeader[I].Value ), DPF.iOS.Common.NSStr( HTTPHeader[I].Field ) );
  end;

  // FURLRequest.set

  if length( FormFields ) + length( FormDatas ) > 0 then
  begin
    postBody := TNSMutableData.Wrap( TNSMutableData.OCClass.data );
    boundary := GenerateBoundary;
    FURLRequest.setValue( DPF.iOS.Common.NSStr( 'multipart/form-data; boundary=' + boundary ), DPF.iOS.Common.NSStr( 'Content-Type' ) );
    postBody.appendData( DPF.iOS.Common.NSStr( _CR + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
  end;

  for I := low( FormFields ) to high( FormFields ) do
  begin
    postBody.appendData( DPF.iOS.Common.NSStr( '--' + boundary + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
    postBody.appendData( DPF.iOS.Common.NSStr( 'Content-Disposition: form-data; name="' + FormFields[I].Field + '"' + _CR + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
    postBody.appendData( DPF.iOS.Common.NSStr( FormFields[I].Value + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );

  end;

  if length( FormDatas ) > 100 then
  begin
    Innerboundary := GenerateBoundary;
    postBody.appendData( DPF.iOS.Common.NSStr( '--' + boundary + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
    postBody.appendData( DPF.iOS.Common.NSStr( 'Content-Disposition: form-data; name="' + FormDatas[0].Name + '"' + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
    postBody.appendData( DPF.iOS.Common.NSStr( 'Content-Type: multipart/mixed; boundary=' + Innerboundary + _CR + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
  end
  else
    Innerboundary := boundary;

  for I := low( FormDatas ) to high( FormDatas ) do
  begin
    postBody.appendData( DPF.iOS.Common.NSStr( '--' + Innerboundary + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
    if length( FormDatas ) > 100 then
      postBody.appendData( DPF.iOS.Common.NSStr( 'Content-Disposition: file; filename="' + FormDatas[I].FileName + '"' + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) )
    else
      postBody.appendData( DPF.iOS.Common.NSStr( 'Content-Disposition: form-data; name="' + FormDatas[I].Name + '"; filename="' + FormDatas[I].FileName + '"' + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
    postBody.appendData( DPF.iOS.Common.NSStr( 'Content-Type: ' + FormDatas[I].ContentType + _CR + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
    postBody.appendData( FormDatas[I].Data );
    postBody.appendData( DPF.iOS.Common.NSStr( _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
  end;

  if length( FormDatas ) > 100 then
    postBody.appendData( DPF.iOS.Common.NSStr( '--' + Innerboundary + '--' + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );

  if length( FormFields ) + length( FormDatas ) = 0 then
    FURLRequest.setHTTPBody( DPF.iOS.Common.NSSTR( FHttpBody ).dataUsingEncoding( NSUTF8StringEncoding ) )
  else
  begin
    postBody.appendData( DPF.iOS.Common.NSStr( '--' + boundary + '--' + _CR ).dataUsingEncoding( NSUTF8StringEncoding ) );
    FURLRequest.setHTTPBody( postBody );
    FURLRequest.setValue( DPF.iOS.Common.NSStr( intToStr( postBody.length ) ), DPF.iOS.Common.NSStr( 'Content-Length' ) );
  end;

  if ASync then
  begin
    FConnectionDataDelegate := TNSURLConnectionDataDelegate.Create( Self );
    FConnection             := TNSURLConnection.Wrap( TNSURLConnection.OCClass.connectionWithRequest( FURLRequest, ( FConnectionDataDelegate as ILocalObject ).GetObjectID ) );
    // FConnection := TNSURLConnection.Wrap( TNSURLConnection.Alloc.initWithRequest( FURLRequest, ( FConnectionDataDelegate as ILocalObject ).GetObjectID, false ) );
    if Assigned( FConnection ) then
    begin
      { _networkQueue := TNSOperationQueue.Wrap( TNSOperationQueue.OCClass.currentQueue );
        _networkQueue.setMaxConcurrentOperationCount( 1 );
        FConnection.setDelegateQueue( _networkQueue ); }

      if Assigned( OperationQueue ) then
        FConnection.setDelegateQueue( iOSapi.Foundation.NSOperationQueue( OperationQueue.FNSOperationQueue ) );

      FConnection.start;
    end;
  end
  else
  begin
    new( FResponsePtr );
    new( error );
    error^ := nil;
    try
      FResponsePtr^ := nil;
      Result        := DPF.iOS.Classes.TNSURLConnection.OCClass.sendSynchronousRequest( FURLRequest, FResponsePtr, Error );
      if ( Result = nil ) and ( Error^ <> nil ) and Assigned( FOnError ) then
        FOnError( Self, UTF8ToString( TNSError.Wrap( Error^ ).localizedDescription.UTF8String ), isFree );
      FBusy := false;
    finally
      Dispose( error );
      Dispose( FResponsePtr );
    end;
  end;
  if isFree then
    exit;
end;

// ----------------------------------------------------------------------------
// HTTPURL: String ; //Sample:  http://www.dpfaragir.com
// HTTPHeader
function TDPFHttp.GetUrlContentString( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; ASync: Boolean = false; HTTPMethod: string = 'GET'; FHttpBody: string = '' ): string;
var
  NSD  : NSData;
  NSStr: NSString;
begin
  Result := '';
  NSD    := GetUrlContentData( HTTPURL, HTTPHeader, [], [], ASync, HTTPMethod, FHttpBody );
  if NSD <> nil then
  begin
    NSStr  := TNSString.Wrap( TNSString.Alloc.initWithData( NSD, NSUTF8StringEncoding ) );
    Result := UTF8ToString( NSStr.UTF8String );
    NSStr.release;
  end;
end;

// ----------------------------------------------------------------------------
// HTTPURL: String ; //Sample:  http://www.dpfaragir.com
// HTTPHeader
function TDPFHttp.PostData( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; Datas: array of NSData; FormData: array of THTTPFormData; ASync: Boolean = false ): string;
var
  NSD  : NSData;
  NSStr: NSString;
begin
  Result := '';
  NSD    := GetUrlContentData( HTTPURL, HTTPHeader, [], [], ASync, 'POST', '' );
  if NSD <> nil then
  begin
    NSStr  := TNSString.Wrap( TNSString.Alloc.initWithData( NSD, NSUTF8StringEncoding ) );
    Result := UTF8ToString( NSStr.UTF8String );
    NSStr.release;
  end;
end;

// ----------------------------------------------------------------------------
// HTTPURL: String ; //Sample:  http://www.dpfaragir.com
// HTTPHeader
function TDPFHttp.PutData( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; Datas: array of THTTPFormData; FormFields: array of THTTPFormField; ASync: Boolean = false ): string;
var
  NSD  : NSData;
  NSStr: NSString;
begin
  Result := '';
  NSD    := GetUrlContentData( HTTPURL, HTTPHeader, Datas, FormFields, ASync, 'PUT', '' );
  if NSD <> nil then
  begin
    NSStr  := TNSString.Wrap( TNSString.Alloc.initWithData( NSD, NSUTF8StringEncoding ) );
    Result := UTF8ToString( NSStr.UTF8String );
    NSStr.release;
  end;
end;

// ----------------------------------------------------------------------------
// HTTPURL: String ; //Sample:  http://www.dpfaragir.com
// HTTPHeader
function TDPFHttp.PostImages( const HTTPURL: string; Images: array of THTTPFormImage; Fields: array of THTTPFormField; ASync: Boolean = false ): string;
var
  NSD  : NSData;
  NSStr: NSString;
  Arr  : array of THTTPFormData;
  I    : Integer;
begin
  Result := '';
  SetLength( Arr, Length( Images ) );
  for I := low( Images ) to high( Arr ) do
  begin
    Arr[I].Name        := Images[I].Name;
    Arr[I].FileName    := Images[I].FileName;
    Arr[I].Data        := TNSData.Wrap( UIImageJPEGRepresentation( ( Images[I].Image as ILocalObject ).GetObjectID, 1 ) );
    Arr[I].ContentType := Images[I].ContentType;
  end;

  NSD := GetUrlContentData( HTTPURL, [], Arr, [], ASync, 'POST', '' );
  if NSD <> nil then
  begin
    NSStr  := TNSString.Wrap( TNSString.Alloc.initWithData( NSD, NSUTF8StringEncoding ) );
    Result := UTF8ToString( NSStr.UTF8String );
    NSStr.release;
  end;
end;

// ----------------------------------------------------------------------------
// Source File Names Must Be have Full Path (dont use relative paths):
// example: GetAppFolder + 'Images/IMG_0042.JPG'
function TDPFHttp.PostFiles( const HTTPURL: string; Files: array of THTTPFormFiles; Fields: array of THTTPFormField; ASync: Boolean = false ): string;

var
  NSD : NSData;
  NStr: NSString;
  Arr : array of THTTPFormData;
  I   : Integer;

  UR: NSURL;
begin
  Result := '';
  SetLength( Arr, Length( Files ) );
  for I := low( Files ) to high( Arr ) do
  begin
    if Files[I].SourceFileName.Contains( 'file://' ) then
      UR := TNSURL.Wrap( TNSURL.OCClass.URLWithString( DPF.iOS.Common.NSStr( Files[I].SourceFileName ) ) )
    else
      UR := TNSURL.Wrap( TNSURL.OCClass.fileURLWithPath( DPF.iOS.Common.NSStr( Files[I].SourceFileName ) ) );

    Arr[I].Name     := Files[I].Name;
    Arr[I].FileName := Files[I].DestFileName;
    Arr[I].Data     := TNSData.Wrap( TNSData.OCClass.dataWithContentsOfURL( UR ) );

    Arr[I].ContentType := Files[I].ContentType;
  end;

  NSD := GetUrlContentData( HTTPURL, [], Arr, Fields, ASync, 'POST', '' );
  if NSD <> nil then
  begin
    NStr   := TNSString.Wrap( TNSString.Alloc.initWithData( NSD, NSUTF8StringEncoding ) );
    Result := UTF8ToString( NStr.UTF8String );
    NStr.release;
  end;
end;

// ----------------------------------------------------------------------------
// HTTPURL: String ; //Sample:  http://www.dpfaragir.com
// HTTPHeader
function TDPFHttp.GetUrlContentImage( const HTTPURL: string; const HTTPHeader: array of THTTPHeader; ASync: Boolean = false; HTTPMethod: string = 'GET' ): UIImage;
var
  NSD: NSData;
begin
  NSD    := GetUrlContentData( HTTPURL, HTTPHeader, [], [], ASync, HTTPMethod );
  Result := nil;
  if NSD <> nil then
    Result := TUIImage.Wrap( TUIImage.OCClass.imageWithData( NSD ) );
end;

// ----------------------------------------------------------------------------
{ TNSURLConnectionDataDelegate }

// ----------------------------------------------------------------------------
procedure TNSURLConnectionDataDelegate.connection( connection: NSURLConnection; didReceiveResponse: NSURLResponse );
var
  FNSHTTPURLResponse: NSHTTPURLResponse;
begin
  FDPFHttp.FBusy           := True;
  FDPFHttp.FDownloadSize   := didReceiveResponse.expectedContentLength;
  FDPFHttp.FDownloadedSize := 0;
  if didReceiveResponse.isKindOfClass( objc_getClass( 'NSHTTPURLResponse' ) ) then
  begin
    FNSHTTPURLResponse       := TNSHTTPURLResponse.Wrap( ( didReceiveResponse as ILocalObject ).GetObjectID );
    FDPFHttp.FLastStatusCode := FNSHTTPURLResponse.statusCode;
    if FDPFHttp.FDownloadSize = -1 then
      FDPFHttp.FDownloadSize := TNSNumber.Wrap( FNSHTTPURLResponse.allHeaderFields.objectForKey( ( DPF.iOS.Common.NSStr( 'Content-Length' ) as ILocalObject ).GetObjectID ) ).longLongValue;
  end;
  Buffer.setLength( 0 );
end;

// ----------------------------------------------------------------------------
procedure TNSURLConnectionDataDelegate.connection( connection: NSURLConnection; didSendBodyData: NSInteger; { bytesWritten }
  totalBytesWritten: NSInteger; { totalBytesWritten }
  totalBytesExpectedToWrite: NSInteger { totalBytesExpectedToWrite } ); cdecl;
begin
  if Assigned( FDPFHttp.FOnSendProgress ) then
    FDPFHttp.FOnSendProgress( FDPFHttp, totalBytesExpectedToWrite, totalBytesWritten );
end;

// ----------------------------------------------------------------------------
procedure TNSURLConnectionDataDelegate.connection( connection: NSURLConnection; didReceiveData: NSData );
begin
  FDPFHttp.FBusy := True;
  Buffer.appendData( didReceiveData );
  FDPFHttp.FDownloadedSize := FDPFHttp.FDownloadedSize + didReceiveData.length;
  if Assigned( FDPFHttp.FOnReceiveProgress ) then
    FDPFHttp.FOnReceiveProgress( FDPFHttp, FDPFHttp.FDownloadSize, FDPFHttp.FDownloadedSize );
end;

// ----------------------------------------------------------------------------
procedure TNSURLConnectionDataDelegate.connection( connection: NSURLConnection; didFailWithError: NSError );
var
  isFree: Boolean;
begin
  isFree := false;

  // FDPFHttp.FConnection.release;
  FDPFHttp.FConnection := nil;
  DPFNSLog( 'TNSURLConnectionDataDelegate.connection' );
  if FDPFHttp.FWaitForFree then
  begin
    FDPFHttp.FBusy := false;
    FDPFHttp.Destroy;
    exit;
  end;

  if Assigned( FDPFHttp.FOnError ) then
  begin
    FDPFHttp.FOnError( FDPFHttp, UTF8ToString( didFailWithError.localizedDescription.UTF8String ), isFree );
    if isFree then
      exit;
  end;
  FDPFHttp.FBusy := false;
end;

// ----------------------------------------------------------------------------
// UserName & Password Authentication
// &
// SSL for an untrusted cert
procedure TNSURLConnectionDataDelegate.connection( connection: NSURLConnection; willSendRequestForAuthenticationChallenge: NSURLAuthenticationChallenge );
var
  credential     : NSURLCredential;
  ChallengeSender: NSURLAuthenticationChallengeSender;
begin
  if willSendRequestForAuthenticationChallenge.previousFailureCount > FDPFHttp.FAuthFailureCount then
    connection.cancel
  else
  begin
    credential      := TNSURLCredential.Wrap( TNSURLCredential.OCClass.credentialWithUser( DPF.iOS.Common.NSStr( FDPFHttp.UserName ), DPF.iOS.Common.NSStr( FDPFHttp.UserPassword ), NSURLCredentialPersistenceForSession ) );
    ChallengeSender := TNSURLAuthenticationChallengeSender.Wrap( willSendRequestForAuthenticationChallenge.sender );
    ChallengeSender.useCredential( credential, willSendRequestForAuthenticationChallenge );
  end;
end;

// ----------------------------------------------------------------------------
// SSL for an untrusted cert
function TNSURLConnectionDataDelegate.connection( connection: NSURLConnection; canAuthenticateAgainstProtectionSpace: NSURLProtectionSpace ): Boolean; cdecl;
begin
  result := canAuthenticateAgainstProtectionSpace.authenticationMethod.isEqualToString( NSURLAuthenticationMethodServerTrust );
  if Assigned( FDPFHttp.FOnCanAuthenticate ) then
    FDPFHttp.FOnCanAuthenticate( FDPFHttp, result );
end;

// ----------------------------------------------------------------------------
// SSL for an untrusted cert
(* procedure TNSURLConnectionDataDelegate.connection( connection: NSURLConnection; didReceiveAuthenticationChallenge: NSURLAuthenticationChallenge1 ); cdecl;
  begin
  TNSURLAuthenticationChallengeSender.Wrap( didReceiveAuthenticationChallenge.sender ).useCredential( TNSURLCredential.Wrap( TNSURLCredential.OCClass.credentialForTrust( didReceiveAuthenticationChallenge.protectionSpace.serverTrust ) ), didReceiveAuthenticationChallenge );
  TNSURLAuthenticationChallengeSender.Wrap( didReceiveAuthenticationChallenge.sender ).continueWithoutCredentialForAuthenticationChallenge( didReceiveAuthenticationChallenge );
  end; *)

// ----------------------------------------------------------------------------
procedure TNSURLConnectionDataDelegate.connectionDidFinishLoading( connection: NSURLConnection );
var
  NSResult : NSString;
  StrResult: string;
  isFree   : Boolean;
  MS       : TMemoryStream;
begin
  isFree := false;

  // FDPFHttp.FConnection.release;
  FDPFHttp.FConnection := nil;

  DPFNSLog( 'Start: TNSURLConnectionDataDelegate.connectionDidFinishLoading' );
  FDPFHttp.FBusy := false;
  StrResult      := '';
  if FDPFHttp.FWaitForFree then
  begin
    DPFNSLog( 'FDPFHttp.FWaitForFree' );
    FDPFHttp.FBusy := false;
    FDPFHttp.Destroy;
    exit;
  end;

  if Assigned( FDPFHttp.FOnFinishLoading ) then
    FDPFHttp.FOnFinishLoading( FDPFHttp );

  if Assigned( FDPFHttp.FOnReceiveDataBin ) then
  begin
    FDPFHttp.FOnReceiveDataBin( FDPFHttp, Buffer, isFree );
    if isFree then
      exit;
  end;

  if Assigned( FDPFHttp.FOnReceiveDataStream ) then
  begin
    MS := TMemoryStream.Create;
    try
      MS.WriteBuffer( Buffer.bytes, Buffer.length );
      MS.Position := 0;
      FDPFHttp.FOnReceiveDataStream( FDPFHttp, MS, isFree );
    finally
      MS.Free;
    end;
    if isFree then
      exit;
  end;

  if Assigned( FDPFHttp.FOnReceiveData ) then
  begin
    NSResult  := TNSString.Wrap( TNSString.Alloc.initWithData( Buffer, NSUTF8StringEncoding ) );
    StrResult := string( NSResult.UTF8String );
    FDPFHttp.FOnReceiveData( FDPFHttp, StrResult, isFree );
    NSResult.release;
    if isFree then
      exit;
  end;
  DPFNSLog( 'End: TNSURLConnectionDataDelegate.connectionDidFinishLoading' );
end;

// ----------------------------------------------------------------------------
constructor TNSURLConnectionDataDelegate.Create( AOwner: TDPFHttp );
begin
  inherited Create;
  FDPFHttp := AOwner;
  Buffer   := TNSMutableData.Create;
end;

// ----------------------------------------------------------------------------
destructor TNSURLConnectionDataDelegate.Destroy;
begin
  if Buffer.retainCount > 0 then
  begin
    Buffer.setLength( 0 );
    Buffer.release;
  end;
  inherited Destroy;
end;

{$ENDIF}

// ----------------------------------------------------------------------------
end.
