// ------------------------------------------------------------------------------
// DPF.iOS.iCloudDoc Component
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
unit DPF.iOS.iCloudDoc;

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
  DPF.iOS.Dispatch,
  DPF.iOS.Classes,
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
  TDPFiCloudDoc = class;

  // ------------------------------------------------------------------------------
{$IFDEF IOS}
  TDocOpenCompletionHandler = procedure( success: pointer ) of object; cdecl;
  TDocSaveCompletionHandler = procedure( success: pointer ) of object; cdecl;

  UIDocumentClass = interface( NSObjectClass )
    ['{2458359E-2FD2-452A-83B4-58B638F41B2A}']
  end;

  UIDocument = interface( NSObject )
    ['{04FD2C04-B1C0-48E6-8359-7E0054E3F7BB}']
    function changeCountTokenForSaveOperation( saveOperation: UIDocumentSaveOperation ): Pointer; cdecl;
    function contentsForType( typeName: NSString; error: NSError ): Pointer; cdecl;
    procedure disableEditing; cdecl;
    function documentState: UIDocumentState; cdecl;
    procedure enableEditing; cdecl;
    function fileAttributesToWriteToURL( url: NSURL; forSaveOperation: UIDocumentSaveOperation; error: NSError ): NSDictionary; cdecl;
    function fileModificationDate: NSDate; cdecl;
    function fileNameExtensionForType( typeName: NSString; saveOperation: UIDocumentSaveOperation ): NSString; cdecl;
    function fileType: NSString; cdecl;
    function fileURL: NSURL; cdecl;
    procedure finishedHandlingError( error: NSError; recovered: Boolean ); cdecl;
    procedure handleError( error: NSError; userInteractionPermitted: Boolean ); cdecl;
    function hasUnsavedChanges: Boolean; cdecl;
    function initWithFileURL( url: NSURL ): Pointer; cdecl;
    function loadFromContents( contents: Pointer; ofType: NSString; error: NSError ): Boolean; cdecl;
    function localizedName: NSString; cdecl;
    function readFromURL( url: NSURL; error: NSError ): Boolean; cdecl;
    function savingFileType: NSString; cdecl;
    procedure setFileModificationDate( fileModificationDate: NSDate ); cdecl;
    procedure setUndoManager( undoManager: NSUndoManager ); cdecl;
    function undoManager: NSUndoManager; cdecl;
    procedure updateChangeCount( change: UIDocumentChangeKind ); cdecl;
    procedure updateChangeCountWithToken( changeCountToken: Pointer; forSaveOperation: UIDocumentSaveOperation ); cdecl;
    procedure userInteractionNoLongerPermittedForError( error: NSError ); cdecl;
    function writeContents( contents: Pointer; andAttributes: NSDictionary; safelyToURL: NSURL; forSaveOperation: UIDocumentSaveOperation; error: NSError ): Boolean; cdecl; overload;
    function writeContents( contents: Pointer; toURL: NSURL; forSaveOperation: UIDocumentSaveOperation; originalContentsURL: NSURL; error: NSError ): Boolean; cdecl; overload;

    procedure openWithCompletionHandler( completionHandler: TDocOpenCompletionHandler ); cdecl;
    procedure saveToURL( url: NSURL; saveOperation: UIDocumentSaveOperation; completionHandler: pointer ); cdecl;
  end;

  TUIDocument = class( TOCGenericImport<UIDocumentClass, UIDocument> )
  end;

  // ------------------------------------------------------------------------------
  IDPFiCloudDocHandler = interface( NSObject )
    ['{C2051A89-B864-4E8D-B348-2CCA4455B27E}']

    procedure queryDidFinishGathering( notification: NSNotification ); cdecl;
  end;

  // ------------------------------------------------------------------------------
  TDPFiCloudDocHandler = class( TOCLocal )
  private
    FDPFiCloudDoc: TDPFiCloudDoc;

  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    constructor create( ADPFiCloudDoc: TDPFiCloudDoc );

    procedure queryDidFinishGathering( notification: NSNotification ); cdecl;
  end;

  // ----------------------------------------------------------------------------
  NSFileManagerClass = interface( NSObjectClass )
    ['{5B3CDB25-4214-4EE3-9801-EB252FF3E144}']
    function defaultManager: Pointer; cdecl;
  end;

  NSFileManager = interface( NSObject )
    ['{F8C9DA56-9DBC-4A28-BB5D-D03DB3F187C3}']

    function URLForUbiquityContainerIdentifier( containerIdentifier: NSString ): NSURL; cdecl;
    function ubiquityIdentityToken: pointer; cdecl; // iOS 6.0 and later
  end;

  TNSFileManager = class( TOCGenericImport<NSFileManagerClass, NSFileManager> )
  end;

  // ----------------------------------------------------------------------------
  DPFDocument = interface( UIDocument )
    ['{5E815CB6-5056-40EE-B409-74E9E41A8412}']

    function contentsForType( typeName: NSString; error: NSError ): Pointer; cdecl;
    function loadFromContents( contents: Pointer; ofType: NSString; error: NSError ): Boolean; cdecl;
  end;

  TDPFDocument = class( TOCLocal )
  private
  protected
    FDPFiCloudDoc: TDPFiCloudDoc;
  public
    constructor Create( ADPFiCloudDoc: TDPFiCloudDoc; url: NSURL );
    function GetObjectiveCClass: PTypeInfo; override;

    function contentsForType( typeName: NSString; error: NSError ): Pointer; cdecl;
    function loadFromContents( contents: Pointer; ofType: NSString; error: NSError ): Boolean; cdecl;

    procedure DocSaveWithCompletionHandler( success: pointer ); cdecl;
  end;

{$ENDIF}

  // ----------------------------------------------------------------------------
  TDPFiCloudDocOnKeyChanged = procedure( sender: TObject ) of object;

  [ComponentPlatformsAttribute( PidWin32 or pidiOSDevice32 or pidiOSDevice64 or PidiOSSimulator )]
  TDPFiCloudDoc = class( TComponent )
  private
    FOnKeyChanged: TDPFiCloudDocOnKeyChanged;

{$IFDEF IOS}
    FDPFDocument        : TDPFDocument;
    FDPFiCloudDocHandler: TDPFiCloudDocHandler;
    DocQuery            : NSMetadataQuery;

    DocLodinggQueue: dispatch_queue_t;
    DocSavingQueue : dispatch_queue_t;
    FDocContent    : string;
{$ENDIF}
    FDocFileName: string;
  protected
{$IFDEF IOS}
    procedure LoadData( query: NSMetadataQuery );
    // procedure DocOpenWithCompletionHandler( success: pointer ); cdecl;
    // procedure DocSaveWithCompletionHandler( success: pointer ); cdecl;
{$ENDIF}
  public
{$IFDEF IOS}
    procedure Loaded; override;
    function iCloudAvailable: boolean;

    procedure Synchronize;
    procedure LoadDocument;
{$ENDIF}
    constructor Create( AOwner: TComponent ); override;
    destructor Destroy; override;
  published

    property OnKeyChanged: TDPFiCloudDocOnKeyChanged read FOnKeyChanged write FOnKeyChanged;
    property DocFileName : string read FDocFileName write FDocFileName;
  end;

  // ------------------------------------------------------------------------------
implementation

// ------------------------------------------------------------------------------
{ TDPFiCloudDoc }
// ------------------------------------------------------------------------------
constructor TDPFiCloudDoc.Create( AOwner: TComponent );
begin
  inherited Create( AOwner );

  FDocFileName := 'DPFDocument.dox';

{$IFDEF IOS}
  FDPFiCloudDocHandler := TDPFiCloudDocHandler.create( self );
  DocLodinggQueue      := dispatch_queue_create( 'Doc Loding Queue', 0 );
  DocSavingQueue       := dispatch_queue_create( 'Doc Setting Queue', 0 );
{$ENDIF}
end;

// ------------------------------------------------------------------------------
destructor TDPFiCloudDoc.Destroy;
begin
{$IFDEF IOS}
  FDPFiCloudDocHandler.DisposeOf;
  if DocLodinggQueue > 0 then
    dispatch_release( DocLodinggQueue );

  if DocSavingQueue > 0 then
    dispatch_release( DocSavingQueue );
{$ENDIF}
  inherited;
end;

// ------------------------------------------------------------------------------
{$IFDEF IOS}

// ------------------------------------------------------------------------------
procedure TDPFiCloudDoc.Loaded;
begin
  inherited;
end;

// ------------------------------------------------------------------------------
procedure TDPFiCloudDoc.LoadDocument;
var
  pred : NSPredicate;
  nsArr: NSMutableArray;
begin
  DocQuery := TNSMetadataQuery.Wrap( TNSMetadataQuery.Alloc.init );
  DocQuery.setSearchScopes( TNSArray.Wrap( TNSArray.OCClass.arrayWithObject( ( NSMetadataQueryUbiquitousDocumentsScope as ILocalObject ).GetObjectID ) ) );

  nsArr := TNSMutableArray.Create;
  nsArr.addObject( ( NSMetadataItemFSNameKey as ILocalObject ).GetObjectID );
  nsArr.addObject( ( NSStr( FDocFileName ) as ILocalObject ).GetObjectID );
  pred := TNSPredicate.Wrap( TNSPredicate.OCClass.predicateWithFormat( NSStr( '%K == %@' ), nsArr ) );
  DocQuery.setPredicate( pred );
  nsArr.release;

  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).addObserver( FDPFiCloudDocHandler.GetObjectID, sel_getUid( 'queryDidFinishGathering:' ), ( NSStr( 'NSMetadataQueryDidFinishGatheringNotification' ) as ILocalObject ).GetObjectID, ( DocQuery as ILocalObject ).GetObjectID );
  DocQuery.startQuery;
end;

// ------------------------------------------------------------------------------
function TDPFiCloudDoc.iCloudAvailable: boolean;
begin
  result := TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager ).ubiquityIdentityToken <> nil;
end;

// ------------------------------------------------------------------------------
procedure TDPFiCloudDoc.Synchronize;
var
  keyStore: NSUbiquitousKeyValueStore;
begin
  keyStore := TNSUbiquitousKeyValueStore.Wrap( TNSUbiquitousKeyValueStore.OCClass.defaultStore );
  keyStore.synchronize;
end;

// ------------------------------------------------------------------------------
{ procedure TDPFiCloudDoc.DocOpenWithCompletionHandler( success: pointer );
  begin
  if INteger( success ) > 0 then
  DPFNSLog( 'iCloud document opened' )
  else
  DPFNSLog( 'failed opening document from iCloud' );
  end; }

// ------------------------------------------------------------------------------
procedure TDPFDocument.DocSaveWithCompletionHandler( success: pointer );
begin
  if INteger( success ) > 0 then
    DPFNSLog( 'new document opened from iCloud' )
  else
    DPFNSLog( 'failed create newdocument in iCloud' );
end;

// ------------------------------------------------------------------------------
// NSMetadataItemURLKey
// NSMetadataItemFSNameKey
// NSMetadataItemDisplayNameKey
// NSMetadataItemIsUbiquitousKey
// NSMetadataUbiquitousItemHasUnresolvedConflictsKey
// NSMetadataUbiquitousItemIsDownloadedKey
// NSMetadataUbiquitousItemIsDownloadingKey
// NSMetadataUbiquitousItemIsUploadedKey
// NSMetadataUbiquitousItemIsUploadingKey
// NSMetadataUbiquitousItemPercentDownloadedKey
// NSMetadataUbiquitousItemPercentUploadedKey
procedure TDPFiCloudDoc.LoadData( query: NSMetadataQuery );
var
  item             : NSMetadataItem;
  ubiq             : NSURL;
  ubiquitousPackage: NSURL;
  fileURL          : NSURL;
  error            : NSError;
begin
  if query.resultCount = 1 then
  begin
    item := TNSMetadataItem.Wrap( query.resultAtIndex( 0 ) );

    ubiq         := TNSURL.Wrap( item.valueForAttribute( NSMetadataItemURLKey ) );
    FDPFDocument := TDPFDocument.Create( self, ubiq );
    // UIDocument( FDPFDocument.Super ).openWithCompletionHandler( DocOpenWithCompletionHandler );

  end
  else
  begin
    ubiq              := TNSFileManager.Wrap( TNSFileManager.OCClass.defaultManager ).URLForUbiquityContainerIdentifier( nil );
    ubiquitousPackage := ubiq.URLByAppendingPathComponent( NSStr( 'Documents' ) ).URLByAppendingPathComponent( NSStr( FDocFileName ) );

    FDPFDocument := TDPFDocument.Create( self, ubiquitousPackage );
    fileURL      := UIDocument( FDPFDocument.Super ).fileURL;
    dispatch_sync( DocSavingQueue,
      procedure
      begin
        UIDocument( FDPFDocument.Super ).writeContents( ( NSStr( 'aaaaaa' ) as ILocalObject ).GetObjectID, fileURL, UIDocumentSaveForCreating, ubiquitousPackage, error );
        // UIDocument( FDPFDocument.Super ).saveToURL( fileURL, UIDocumentSaveForCreating, FDPFDocument.DocSaveWithCompletionHandler );
      end );
  end;
end;

// ------------------------------------------------------------------------------
{ TDPFiCloudDocHandler }
constructor TDPFiCloudDocHandler.create( ADPFiCloudDoc: TDPFiCloudDoc );
begin
  inherited create;
  FDPFiCloudDoc := ADPFiCloudDoc;
end;

// ------------------------------------------------------------------------------
function TDPFiCloudDocHandler.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( IDPFiCloudDocHandler );
end;

// ------------------------------------------------------------------------------
procedure TDPFiCloudDocHandler.queryDidFinishGathering( notification: NSNotification );
var
  query: NSMetadataQuery;
begin

  // query := TNSMetadataQuery.Wrap( ( notification as ILocalObject ).GetObjectID );
  FDPFiCloudDoc.DocQuery.disableUpdates;
  FDPFiCloudDoc.DocQuery.stopQuery;

  TNSNotificationCenter.Wrap( TNSNotificationCenter.OCClass.defaultCenter ).removeObserver( FDPFiCloudDoc.FDPFiCloudDocHandler.GetObjectID );
  FDPFiCloudDoc.LoadData( FDPFiCloudDoc.DocQuery );
end;

// ------------------------------------------------------------------------------
{ TDPFDocument }
constructor TDPFDocument.Create( ADPFiCloudDoc: TDPFiCloudDoc; url: NSURL );
var
  V: Pointer;
begin
  inherited Create;
  FDPFiCloudDoc := ADPFiCloudDoc;
  V             := UIDocument( Super ).initWithFileURL( url );
  if GetObjectID <> V then
    UpdateObjectID( V );
end;

// ------------------------------------------------------------------------------
function TDPFDocument.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo( DPFDocument );
end;

// ------------------------------------------------------------------------------
function TDPFDocument.contentsForType( typeName: NSString; error: NSError ): Pointer;
begin
  if FDPFiCloudDoc.FDocContent = '' then
    FDPFiCloudDoc.FDocContent := 'Empty'
  else
    Result := TNSData.OCClass.dataWithBytes( NSStr( FDPFiCloudDoc.FDocContent ).UTF8String, Length( FDPFiCloudDoc.FDocContent ) );
end;

// ------------------------------------------------------------------------------
function TDPFDocument.loadFromContents( contents: Pointer; ofType: NSString; error: NSError ): Boolean;
begin
  if ( TNSString.Wrap( contents ).length > 0 ) then
  begin
    FDPFiCloudDoc.FDocContent := UTF8ToString( TNSString.Wrap( contents ).UTF8String );
  end
  else
  begin
    FDPFiCloudDoc.FDocContent := 'Empty';
  end;

  result := true;
end;

// ------------------------------------------------------------------------------
{$ENDIF}

end.
