// $HDR$
//----------------------------------------------------------------------------//
// MCM DESIGN                                                                 //  
//                                                                            //  
// For further information / comments, visit our WEB site at                  //  
//   www.mcm-design.com                                                       //  
// or e-mail to                                                               //  
//   CustomerCare@mcm-design.dk                                               //  
//----------------------------------------------------------------------------//
//
// $Log:  25962: mcmDragDrop.pas 
//
//    Rev 1.8    2014-02-02 21:09:54  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.7    26-08-2009 23:26:46  mcm    Version: IMG 3.2
// Delphi 2009 support
//
//    Rev 1.6    20-08-2007 20:28:24  mcm
// Added support for Delphi 2007
//
//   Rev 1.5    27-09-2005 18:52:02  mcm    Version: IMG 2.9
// Corrected Unicode problem under Windows 98.

//
//   Rev 1.4    22-09-2005 20:32:28  mcm
// Code clean-up.

//
//   Rev 1.3    04-08-2005 18:44:20  mcm    Version: IMG 2.9
// Moved public properties Copy, Link and Move to published section. Minor
// clean-up.

//
//   Rev 1.2    31-07-2005 21:01:26  mcm
// Added SetAsFileName and IDHandle, Extended Drop events, TranslateKeyState.

//
//   Rev 1.1    30/07/2005 12:19:58  mcm
// Changed how drag operation is detected (Cancelled use of Windows.DragDetect).

//
//   Rev 1.0    24-07-2005 18:52:06  mcm    Version: IMG 2.9
// Drag & Drop controls.

unit mcmDragDrop;

interface

{$Include 'mcmDefines.pas'}
                      
uses {$IFNDEF GE_DXE2}
      Windows, Messages, Classes, Controls, Graphics, ComCtrls, ExtCtrls, ActiveX,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.ComCtrls, Vcl.ExtCtrls, WinApi.ActiveX,
     {$ENDIF}
     mcmImage, mcmHandleStream;

const
  NoDragFormats = 127;

type
  //----------------------------------------------------------------------------
  // IUnknown base classes

  TmcmInterfacedObject = class (TInterfacedObject)
  private
  {$IFDEF DCB3}
    // Versions of Delphi and C++Builder latter then 3 already has the IUnknown
    // interface build in.

    // IUnknown methods.
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
  {$ENDIF}
  protected
  public
    function  AddRef : integer;
    function  Release : integer;
  end;

  TmcmInterfacedComponent = class (TComponent, IUnknown)
  private
  {$IFDEF DCB3}
    // Versions of Delphi and C++Builder latter then 3 already has the IUnknown
    // interface build in.

    // IUnknown methods.
    function  _AddRef: Integer; stdcall;
    function  _Release: Integer; stdcall;
  {$ENDIF}
  protected
    FRefCount : integer;
  {$IFDEF DCB3}
    // Versions of Delphi and C++Builder latter then 3 already has the IUnknown
    // interface build in.

    // IUnknown - Required to enable Drag from other application and Drop onto this app.
    function  QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  {$ENDIF}
  public
    function  AddRef : integer;
    function  Release : integer;
    procedure BeforeDestruction; {$IFNDEF DCB3} override; {$ENDIF}
    property  RefCount : integer
      read    FRefCount;
  published
  end;

  //----------------------------------------------------------------------------
  // Components for TmcmBaseDataSource.

  TmcmFormatEtcItem = class(TCollectionItem)
  private
    FName      : string;
    FFormatEtc : TFormatEtc;
  protected
    function  GetAspect : longint;
    function  GetFormat : TClipFormat;
    function  GetFormatEtc : TFormatEtc;
    function  GetFormatIndex : integer;
    function  GetMedium : longint;
    procedure SetAspect(const Value : longint);
    procedure SetFormat(const Value : TClipFormat);
    procedure SetFormatIndex(const Value : integer);
    procedure SetMedium(const Value : longint);
    procedure SetName(const Value : string);
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function  GetDisplayName : string; override;
  published
    property Aspect : longint
      read   GetAspect
      write  SetAspect default DVASPECT_CONTENT;
    property Format : TClipFormat
      read   GetFormat
      write  SetFormat stored False;
    property Medium : longint
      read   GetMedium
      write  SetMedium default TYMED_HGLOBAL;
    property Name : string
      read   FName
      write  SetName;
    property FormatEtc : TFormatEtc
      read   GetFormatEtc;
    property FormatIndex : integer
      read   GetFormatIndex
      write  SetFormatIndex;
  end;


  //----------------------------------------------------------------------------
  //
  TmcmFormatEtcList = class (TCollection)
  private
    function  GetItem(Index : integer) : TmcmFormatEtcItem;
    procedure SetItem(Index : integer; Value : TmcmFormatEtcItem);
  protected
  public
    constructor Create(AOwner : TComponent);
    function Add : TmcmFormatEtcItem;
    property Items[Index: integer] : TmcmFormatEtcItem
      read   GetItem
      write  SetItem; default;
  end;

  //----------------------------------------------------------------------------
  TmcmIntfEnumFormatEtc = class(TmcmInterfacedComponent, IEnumFORMATETC)
  private
    FIndex : integer;          
    FList  : TmcmFormatEtcList;
  protected
    // IEnumFORMATETC methods
    function Next(celt : longint; out elt; pceltFetched : PLongint) : HResult; stdcall;
    function Skip(celt : longint) : HResult; stdcall;
    function Reset : HResult; stdcall;
    function Clone(out Enum : IEnumFormatEtc) : HResult; stdcall;
  public
    constructor Create(List : TmcmFormatEtcList; Index : integer); {$IFNDEF DCB3} reintroduce; {$ENDIF}
    destructor Destroy; override;
  published
  end;

  //----------------------------------------------------------------------------
  // TmcmIntfDataSource.
  TmcmDataHandles = array[0..64] of THandle;

  TmcmIntfDataSource = class(TmcmInterfacedObject, IDataObject)
  private
    FHandles       : TmcmDataHandles;
    FFormatEtcList : TmcmFormatEtcList;
  protected
    // IDataObject methods
    function GetData(const formatetcIn : TFormatEtc; out medium : TStgMedium) : HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium) : HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc) : HResult; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc) : HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL) : HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc_: IEnumFormatEtc) : HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint; const advSink: IAdviseSink; out dwConnection: Longint) : HResult; stdcall;
    function DUnadvise(dwConnection: longint) : HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData) : HResult; stdcall;
  public
    constructor Create(Formats : TmcmFormatEtcList; Handles : TmcmDataHandles);
    destructor Destroy; override;
  end;

  //----------------------------------------------------------------------------
  // TmcmCustomDataSource.

  TmcmCustomDataSource = class(TComponent)
  private
    FComponent    : TPersistent; // Object being dragged.
    FDropEffect   : integer;
    FEnableDesc   : boolean; // Enable Descriptor
    FEnableEffect : boolean; // Enable drop effect.
    FIDHandle     : THandle;
  protected
    function  GetData(Format : TClipFormat; Medium : longint; Aspect : dword; AIndex : integer) : THandle; virtual;
    function  GetFormats : TmcmFormatEtcList; virtual;
    function  GetDataObject : IDataObject; virtual;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;

    function  SetAs(var Data; Size : integer) : hGlobal;
    function  SetAsImage(AFormat : word; Image : TmcmImage) : hGlobal;
    function  SetAsBitmap(Bitmap : TBitmap) : HBITMAP;
    function  SetAsFilename(StrList : TStringList) : hGlobal;
    function  SetAsHDrop(StrList : TStringList) : hGlobal;
    function  SetAsObjectDesc(OD : TObjectDescriptor; Src : string) : hGlobal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyToClipboard;
    procedure CutToClipboard;

    property DropEffect : integer
      read   FDropEffect
      write  FDropEffect;
    property DataObject : IDataObject
      read   GetDataObject;
    property IDHandle : THandle
      read   FIDHandle
      write  FIDHandle;
    property LinkControl : TPersistent
      read   FComponent
      write  FComponent;
  published
  end;


  //----------------------------------------------------------------------------
  // TmcmDropSource.

  TOnPreStartDrag = procedure(Sender : TObject; X, Y: Integer; var Component : TPersistent) of object;

  TmcmDropSource = class(TmcmInterfacedComponent, IDropSource)
  private
    FDragMode       : TDragMode;
    FCopy           : boolean;
    FLink           : boolean;
    FMove           : boolean;
    FDragging       : boolean;
    FEffect         : integer;
    FDataSource     : TmcmCustomDataSource;

    FDragInverval   : integer;
    FDragThreshold  : integer;
    FDragTimer      : TTimer;
    FMousePos       : TPoint;

    FOldDefWndProc  : TWndMethod; // Default window procedure of the assigned FControl.

    FControl        : TControl;    // Control being dragged.
    FComponent      : TPersistent; // Persistent Object being dragged.

    FEndDragEvent   : TEndDragEvent;   // = procedure(Sender, Target: TObject; X, Y: Integer) of object;
    FOnPreStartDrag : TOnPreStartDrag;
    FStartDragEvent : TStartDragEvent; //  = procedure (Sender: TObject; var DragObject: TDragObject) of object;
  protected
    // IDropSource methods
    function  GiveFeedback(dwEffect : longint) : HResult; stdcall;
    function  QueryContinueDrag(fEscapePressed : BOOL; grfKeyState : longint) : HResult; stdcall;

    // Local methods.
    procedure Connect;
    procedure Disconnect;
    procedure NewDefWndProc(var Msg : TMessage);
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure SetDragMode(Value : TDragMode);
    procedure SetControl(Value : TControl);
    procedure OnDragTimer(Sender : TObject);
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    function  BeginDrag : boolean;

    property Control : TControl
      read   FControl
      write  SetControl;
    property DataSource : TmcmCustomDataSource
      read   FDataSource
      write  FDataSource;
    property Dragging : boolean
      read   FDragging;
  published
    property DragMode : TDragMode
      read   FDragMode
      write  SetDragMode;
    property DragInverval : integer
      read   FDragInverval
      write  FDragInverval;
    property DragThreshold  : integer
      read   FDragThreshold
      write  FDragThreshold;
    property EnableCopy : boolean
      read   FCopy
      write  FCopy default True;
    property EnableLink : boolean
      read   FLink
      write  FLink default False;
    property EnableMove : boolean
      read   FMove
      write  FMove default False;  
    property OnEndDrag : TEndDragEvent
      read   FEndDragEvent
      write  FEndDragEvent;
    property OnPreStartDrag : TOnPreStartDrag
      read   FOnPreStartDrag
      write  FOnPreStartDrag;
    property OnStartDrag : TStartDragEvent
      read   FStartDragEvent
      write  FStartDragEvent;
  end;

  //----------------------------------------------------------------------------
  // TmcmDropEnum.

  TDragFormat  = word;
  TDragFormats = array[0..NoDragFormats] of TDragFormat;

  TmcmDropEnum = class
  private
    FDataObject  : IDataObject;
    FFormatEtc   : TFormatEtc;
    FDataFormats : TDragFormats;
    FNoFormats   : integer;
  protected
    procedure InitFormatEtc(ClipFormat : TClipFormat; TyMed : longint);
    procedure GetFormats;
  public
    constructor Create(DataObject : IDataObject);
    procedure Clear;
    procedure GetAsImage(Image : TmcmImage);
    procedure GetAsBitmap(Bitmap : TBitmap);
    procedure GetAsDIB(Bitmap : TBitmap);
    procedure GetAsHDrop(FileList : TStrings);
    procedure GetAsIDHandle(var IDHandle : THandle);
    procedure GetAsWMF(MetaFile : TMetaFile);
    procedure GetAsEMF(MetaFile : TMetafile);
    procedure GetAsPalette(Bitmap : TBitmap);
    procedure GetAsRTF(var RichText : TRichEdit);
    procedure GetAsSimpleRTF(var Str : string);
    procedure GetAsText(var Str : string);
    procedure GetAsFileName(var Str : string);
    procedure GetAsShellIDList(IDList : TStrings);
    procedure GetDescriptor(StgMedium : TStgMedium; List : TStrings);
    procedure GetAsObjectDescriptor(ObjDescList : TStrings);
    (*
    procedure GetAsLinkSrcDescriptor(LinkSrcDescList : TStrings);
    *)
    function GetIDataObject : IDataObject;
    function  HasClipFormat(ClipFormat : TClipFormat) : boolean;
  end;

  //----------------------------------------------------------------------------
  // TmcmDropTarget.
  // Copied from TControl
  //   TDragState = (dsDragEnter, dsDragLeave, dsDragMove);

  TExDragDropEvent = procedure(Sender, Source : TObject; Shift : TShiftState; X, Y : integer) of object;
  TExDragOverEvent = procedure(Sender, Source : TObject; Shift : TShiftState; X, Y : integer; State : TDragState; var Accept : boolean; var Copy : boolean; var Move : boolean) of object;

  TmcmDropTarget = class (TmcmInterfacedComponent, IDropTarget)
  private
    FParentHandle    : THandle;
    FOldDefWndProc   : pointer;
    FNewDefWndProc   : pointer;
    FActive          : boolean;

    FDropObject      : TmcmDropEnum; // Drop object.
    FAcceptDragObj   : boolean;      // Do we accept this object?
    FCopyObj         : boolean;
    FMoveObj         : boolean;
    FdwEffect        : longint;
    FgrfKeyState     : longint;

    FPoint           : TPoint;
    FDragDropEvent   : TDragDropEvent; // = procedure(Sender, Source: TObject; X, Y: Integer) of object;
    FDragOverEvent   : TDragOverEvent; // = procedure(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean) of object;
    FExDragOverEvent : TExDragOverEvent;
    FExDragDropEvent : TExDragDropEvent;

    procedure NewDefWndProc(var Msg : TMessage);
  protected
    // IDropTarget methods
    function DragEnter(const DataObj     : IDataObject;
                             grfKeyState : longint;
                             Point       : TPoint;
                       var   dwEffect    : longint) : HResult; stdcall;
    function DragOver (      grfKeyState : longint;
                             Point       : TPoint;
                       var   dwEffect    : longint) : HResult; {$IFNDEF DCB3} reintroduce; {$ENDIF} stdcall;
    function DragLeave : HResult; stdcall;
    function Drop     (const DataObj     : IDataObject;
                             grfKeyState : longint;
                             Point       : TPoint;
                       var   dwEffect    : longint) : HResult; stdcall;

    // Local methods.
    function DoDragEvent(    State       : TDragState;
                             grfKeyState : longint;
                             Point       : TPoint;
                         var dwEffect    : longint) : HResult;
    procedure SetActive(Value : boolean);
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Active : boolean
      read   FActive
      write  SetActive;

    property OnDragDrop : TDragDropEvent
      read   FDragDropEvent
      write  FDragDropEvent;
    property OnDragOver : TDragOverEvent
      read   FDragOverEvent
      write  FDragOverEvent;
    property OnExDragDrop : TExDragDropEvent
      read   FExDragDropEvent
      write  FExDragDropEvent;
    property OnExDragOver : TExDragOverEvent
      read   FExDragOverEvent
      write  FExDragOverEvent;
  end;

var CF_ICON                : TClipFormat;
    CF_RTF                 : TClipFormat;
    CF_FILENAME            : TClipFormat;
    CF_IDLIST              : TClipFormat;
    CF_OBJECTDESCRIPTOR    : TClipFormat;
    CF_LINKSRCDESCRIPTOR   : TClipFormat;
    CF_PREFERREDDROPEFFECT : TClipFormat;
    CF_MCMSOURCEID         : TClipFormat;

implementation

uses {$IFNDEF GE_DXE2}
      SysUtils, Forms, ComObj, ShellAPI, ShlObj, RichEdit;
     {$ELSE}
      System.SysUtils, Vcl.Forms, System.Win.ComObj, WinApi.ShellAPI, WinApi.ShlObj;
     {$ENDIF}


const
  CF_NULL = 0;
{$IFDEF DCB3}
  CFSTR_FILENAMEA           = 'FileName';                // CF_FILENAMEA
  CFSTR_SHELLIDLIST         = 'Shell IDList Array';      // CF_IDLIST
{$ENDIF}
  CFSTR_ICON                = 'Icon';                    // CF_ICON
  CFSTR_RTF                 = 'Rich Text Format';        // CF_RTF
  CFSTR_OBJECTDESCRIPTOR    = 'Object Descriptor';       //
  CFSTR_LINKSRCDESCRIPTOR   = 'Link Source Descriptor';  //

  CFSTR_MCMSOURCEID         = 'MCM Imaging Source ID Descriptor';
                                                         // CF_MCMSOURCEID
                                                         // Used to compare Source and Target Window.
                                                         // Returns the TmcmSourceDrop parent handle.

function TranslateKeyState(grfKeyState : longint) : TShiftState;
var Shift : TShiftState;
{$IFDEF DCB3_5}
const MK_ALT = $20;
{$ENDIF}
begin
  // Translate key state to Delphi style.
  Shift := [];
  if ((grfKeyState and MK_SHIFT) = MK_SHIFT)
  then Shift := Shift + [ssShift];
  if ((grfKeyState and MK_CONTROL) = MK_CONTROL)
  then Shift := Shift + [ssCtrl];
  if ((grfKeyState and MK_ALT) = MK_ALT)
  then Shift := Shift + [ssAlt];
  Result := Shift;
end; // TranslateKeyState.


//------------------------------------------------------------------------------
// IUnknown base class
//------------------------------------------------------------------------------

{$IFDEF DCB3}

// The following three methods enables drag and drop from other applications
// when compiled using Delphi 3 and CBuilder 3.
// These methods should NOT be implemented when using later editions of Delphi
// and CBuilder.

function TmcmInterfacedObject._AddRef : integer;
begin
 Result := -1;
 {
  if (VCLComObject = Nil)
  then Result := -1 // Reference counting is not used.
  else Result := IVCLComObject(VCLComObject)._AddRef;
  }
end; // TmcmInterfacedObject._AddRef.


function TmcmInterfacedObject._Release : integer;
begin
Result := -1;
{
  if (VCLComObject = Nil)
  then Result := -1 // Reference counting is not used.
  else Result := IVCLComObject(VCLComObject)._Release;
  }
end; // TmcmInterfacedObject._Release.

{$ENDIF}

function TmcmInterfacedObject.AddRef : integer;
begin
  Result := _AddRef;
end; // TmcmInterfacedObject.AddRef.


function TmcmInterfacedObject.Release : integer;
begin
  Result := _Release;
end; // TmcmInterfacedObject.Release.


{$IFDEF DCB3}

// The following three methods enables drag and drop from other applications
// when compiled using Delphi 3 and CBuilder 3.
// These methods should NOT be implemented when using later editions of Delphi
// and CBuilder.

function TmcmInterfacedComponent.QueryInterface(const IID: TGUID; out Obj) : HResult;
begin
  if (VCLComObject = Nil)
  then begin
       if GetInterface(IID, Obj)
       then Result := S_OK
       else Result := E_NOINTERFACE;
  end
  else Result := IVCLComObject(VCLComObject).QueryInterface(IID, Obj);
end; // TmcmInterfacedComponent.QueryInterface.


function TmcmInterfacedComponent._AddRef : integer;
begin
  if (VCLComObject = Nil)
  then Result := -1 // Reference counting is not used.
  else Result := IVCLComObject(VCLComObject)._AddRef;
end; // TmcmInterfacedComponent._AddRef.


function TmcmInterfacedComponent._Release : integer;
begin
  if (VCLComObject = Nil)
  then Result := -1 // Reference counting is not used.
  else Result := IVCLComObject(VCLComObject)._Release;
end; // TmcmInterfacedComponent._Release.

{$ENDIF}

function TmcmInterfacedComponent.AddRef : integer;
begin
  Result := _AddRef;
end; // TmcmInterfacedComponent.AddRef.


function TmcmInterfacedComponent.Release : integer;
begin
  Result := _Release;
end; // TmcmInterfacedComponent.Release.


procedure TmcmInterfacedComponent.BeforeDestruction;
begin
  {$IFNDEF DCB3}
  Inherited BeforeDestruction;
  {$ENDIF}
end; // TmcmInterfacedComponent.BeforeDestruction.


//------------------------------------------------------------------------------
// Drop base class
//------------------------------------------------------------------------------

constructor TmcmDropEnum.Create(DataObject : IDataObject);
begin
  Inherited Create;
  FDataObject := DataObject;
  GetFormats;
end; // TmcmDropEnum.Create.


procedure TmcmDropEnum.Clear;
begin
  FNoFormats := 0;
  FillChar(FDataFormats, SizeOf(FDataFormats), 0);
end; // TmcmDropEnum.Clear.


procedure TmcmDropEnum.GetFormats;

  procedure GetFormat(ClipFormat : TClipFormat; TyMed : longint);
  begin
    InitFormatEtc(ClipFormat, TyMed);
    if (FDataObject.QueryGetData(FFormatEtc) = S_OK)
    then begin
         FDataFormats[FNoFormats] := ClipFormat;
         //Include(FDataFormats,? Format);
         inc(FNoFormats);
    end;
  end; // GetFormat.

begin
  Clear;
  GetFormat(CF_MCMSOURCEID,       TYMED_HGLOBAL);
  GetFormat(CF_BITMAP,            TYMED_GDI);
  GetFormat(CF_DIB,               TYMED_HGLOBAL);
  GetFormat(CF_HDROP,             TYMED_HGLOBAL);
  GetFormat(CF_METAFILEPICT,      TYMED_MFPICT);
  GetFormat(CF_ENHMETAFILE,       TYMED_ENHMF);
  GetFormat(CF_PALETTE,           TYMED_GDI);
  GetFormat(CF_TEXT,              TYMED_HGLOBAL);
  GetFormat(CF_RTF,               TYMED_HGLOBAL);
  GetFormat(CF_FILENAME,          TYMED_HGLOBAL);
  GetFormat(CF_IDLIST,            TYMED_HGLOBAL);
  GetFormat(CF_OBJECTDESCRIPTOR,  TYMED_HGLOBAL);
  GetFormat(CF_LINKSRCDESCRIPTOR, TYMED_HGLOBAL);
end; // TmcmDropEnum.GetFormats.


function TmcmDropEnum.HasClipFormat(ClipFormat : TClipFormat) : boolean;
var Index : integer;
begin
  Result := False;
  Index := 0;
  while (Index < NoDragFormats) and (Result = False)
  do begin
     if (FDataFormats[Index] = ClipFormat)
     then Result := True;
     inc(Index);
  end;
end; // TmcmDropEnum.HasFormat.


procedure TmcmDropEnum.InitFormatEtc(ClipFormat : TClipFormat; TyMed : longint);
begin
  FFormatEtc.cfFormat := ClipFormat;
  FFormatEtc.tymed    := TyMed;
  FFormatEtc.ptd      := Nil;
  FFormatEtc.dwAspect := DVASPECT_CONTENT;
  FFormatEtc.lindex   := -1;
end; // TmcmDropEnum.InitFormatEtc.


function TmcmDropEnum.GetIDataObject : IDataObject;
begin
  Result := FDataObject;
end; // TmcmDropEnum.GetIDataObject.


procedure TmcmDropEnum.GetAsImage(Image : TmcmImage);
var StgMedium : TStgMedium;
begin
  if Not(Assigned(Image))
  then Exit;

  if HasClipFormat(CF_DIB)
  then InitFormatEtc(CF_DIB, TYMED_HGLOBAL)
  else if HasClipFormat(CF_BITMAP)
       then InitFormatEtc(CF_BITMAP, TYMED_GDI)
       else Exit;

  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    if HasClipFormat(CF_DIB)
    then Image.LoadFromClipboardFormat(FFormatEtc.cfFormat, StgMedium.hGlobal)
    else Image.LoadFromClipboardFormat(FFormatEtc.cfFormat, StgMedium.hBitmap);
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsImage.


procedure TmcmDropEnum.GetAsBitmap(Bitmap : TBitmap);
var StgMedium : TStgMedium;
begin
  if Not(Assigned(Bitmap))
  then Exit;

  InitFormatEtc(CF_BITMAP, TYMED_GDI);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    Bitmap.LoadFromClipboardFormat(CF_BITMAP, StgMedium.hBitmap, 0);
    if HasClipFormat(CF_PALETTE)
    then GetAsPalette(Bitmap);
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsBitmap.


procedure TmcmDropEnum.GetAsDIB(Bitmap : TBitmap);
var StgMedium : TStgMedium;
    Stream    : TMemoryStream;
    pDib      : Pointer;
    DibSize   : DWord;
    Bmfh      : TBitmapFileHeader;
begin
  if Not(Assigned(Bitmap))
  then Exit;
  InitFormatEtc(CF_DIB, TYMED_HGLOBAL);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    DIBSize := GlobalSize(StgMedium.hGlobal);
    pDib := GlobalLock(StgMedium.hGlobal);
    try
      Stream := TMemoryStream.Create;
      try
        // Write a bitmap file header record.
        FillChar(Bmfh, sizeof(Bmfh), 0);
        Bmfh.bfType := $4D42;
        Bmfh.bfSize := SizeOf(Bmfh) + DibSize;
        Stream.Write(Bmfh, SizeOf(Bmfh));

        // Write the DIB structure.
        Stream.Write(pDib^, DibSize);
        Stream.Position := 0;

        // Load the finished DIB into a TBitmap
        Bitmap.LoadFromStream(Stream)
      finally
        Stream.Free;
      end
    finally
      GlobalUnlock(StgMedium.hGlobal);
    end;
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsDIB.


procedure TmcmDropEnum.GetAsHDrop(FileList : TStrings);
var StgMedium : TStgMedium;
    Count     : integer;
    Loop      : integer;
    Buffer    : array[0..1023] of char;
begin
  if Not(Assigned(FileList))
  then Exit;

  InitFormatEtc(CF_HDROP, TYMED_HGLOBAL);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    // How many files were dragged ?
    Count := DragQueryFile(StgMedium.hGlobal, Cardinal(-1), Nil, 0);
    FileList.BeginUpdate;
    try
      FileList.Clear;
      // Loop through all files.
      for Loop := 0 to Pred(Count)
      do begin
         // Get file name
         DragQueryFile(StgMedium.hGlobal, Loop, Buffer, SizeOf(Buffer));
         FileList.Add(Buffer);
      end
    finally
      FileList.EndUpdate;
    end
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsHDrop.


procedure TmcmDropEnum.GetAsIDHandle(var IDHandle : THandle);
var StgMedium : TStgMedium;
    pHandle   : ^THandle;
begin
  InitFormatEtc(CF_MCMSOURCEID, TYMED_HGLOBAL);
  try
    OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
    try
      pHandle := GlobalLock(StgMedium.hGlobal);
      IDHandle := pHandle^;
      GlobalUnlock(StgMedium.hGlobal);
    finally
      ReleaseStgMedium(StgMedium);
    end;
  except
  end;
end; // TmcmDropEnum.GetAsIDHandle.


procedure TmcmDropEnum.GetAsWMF(MetaFile : TMetaFile);
var StgMedium  : TStgMedium;
    pMfp       : PMetaFilePict;
    MfpBufSize : dword;
    MfpBuf     : pointer;
begin
  if Not(Assigned(MetaFile))
  then Exit;

  InitFormatEtc(CF_METAFILEPICT, TYMED_MFPICT);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    // Get access to the TMetaFilePict record.
    pMfp := GlobalLock(StgMedium.hMetaFilePict);
    try
      // How big is the meta file?
      MfpBufSize := GetMetaFileBitsEx(pMfp^.hMF, 0, Nil);
      //Allocate sufficient buffer space
      GetMem(MfpBuf, MfpBufSize);
      try
        // Copy Meta file to buffer.
        if (GetMetaFileBitsEx(pMfp^.hMF, MfpBufSize, MfpBuf) <> 0)
        then begin
             // Generate enhanced meta file from buffer.
             MetaFile.Handle := SetWinMetaFileBits(MfpBufSize, MfpBuf, 0, pMfp^);
        end
        else MetaFile.Handle := 0;
      finally
        // Free buffer.
        FreeMem(MfpBuf);
      end
    finally
      // Unlock memory handle.
      GlobalUnlock(StgMedium.hMetaFilePict);
    end;
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsWMF.


procedure TmcmDropEnum.GetAsEMF(MetaFile : TMetafile);
var StgMedium : TStgMedium;
begin
  if Not(Assigned(MetaFile))
  then Exit;

  InitFormatEtc(CF_ENHMETAFILE, TYMED_ENHMF);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    MetaFile.Handle := CopyEnhMetafile(StgMedium.hEnhMetaFile, Nil);
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsEMF.


procedure TmcmDropEnum.GetAsPalette(Bitmap : TBitmap);
var StgMedium : TStgMedium;
begin
  if Not(Assigned(Bitmap))
  then Exit;

  InitFormatEtc(CF_PALETTE, TYMED_GDI);
  try
    OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
    try
      Bitmap.Palette := CopyPalette(StgMedium.hBitmap);
    finally
      ReleaseStgMedium(StgMedium);
    end;
  except
  end;
end; // TmcmDropEnum.GetAsPalette.


procedure TmcmDropEnum.GetAsText(var Str : String);
var StgMedium : TStgMedium;
    CharStr   : PChar;
begin
  InitFormatEtc(CF_TEXT, TYMED_HGLOBAL);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    CharStr := GlobalLock(StgMedium.hGlobal);
    try
      Str := String(CharStr);
    finally
      GlobalUnlock(StgMedium.hGlobal);
    end;
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsText.


procedure TmcmDropEnum.GetAsRTF(var RichText : TRichEdit);
var Str         : string;
(*    {$IFNDEF DCB3}
    RichEditOle : IRichEditOle;
    {$ENDIF}
*)
begin
  RichText.Lines.Clear;
  // Try and get richedit to deal with it...
(*  {$IFNDEF DCB3}
  if (RichText.Perform(EM_GETOLEINTERFACE, 0, LParam(@RichEditOle)) <> 0)
  then RichEditOle.ImportDataObject(FDataObject, 0, 0)
  else begin
  {$ENDIF}
*)
       // If Delphi 3 or RichText can't do it call GetAsSimpleRTF.
       GetAsSimpleRTF(Str);
       RichText.Lines.Text := Str;
(*  {$IFNDEF DCB3}
  end;
  {$ENDIF}
*)
end; // TmcmDropEnum.GetAsRTF.


procedure TmcmDropEnum.GetAsSimpleRTF(var Str : String);
var StgMedium : TStgMedium;
    CharStr   : PChar;
begin
  InitFormatEtc(CF_RTF, TYMED_HGLOBAL);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    CharStr := GlobalLock(StgMedium.hGlobal);
    try
      Str := string(CharStr);
    finally
      GlobalUnlock(StgMedium.hGlobal);
    end;
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsSimpleRTF.


procedure TmcmDropEnum.GetAsFileName(var Str : String);
var StgMedium : TStgMedium;
    CharStr   : PChar;
begin
  InitFormatEtc(CF_FILENAME, TYMED_HGLOBAL);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    CharStr := GlobalLock(StgMedium.hGlobal);
    try
      Str := String(CharStr);
    finally
      GlobalUnlock(StgMedium.hGlobal);
    end
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsFileName.


procedure TmcmDropEnum.GetAsShellIDList(IDList : TStrings);
var StgMedium    : TStgMedium;
    IDA          : PIDA;
    PIDL         : PItemIDList;
    Loop         : Integer;
    FileInfo     : TSHFileInfoW;
    DisplayStr   : WideString;
    ParentFolder : WideString;
begin
  InitFormatEtc(CF_IDLIST, TYMED_HGLOBAL);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    IDA := GlobalLock(StgMedium.hGlobal);
    try
      IDList.Clear;
      for Loop := 0 to IDA.cidl
      do begin
         PIDL := PItemIDList(DWord(IDA) + IDA.aoffset[Loop]);
         SHGetFileInfoW(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo), SHGFI_PIDL or SHGFI_DISPLAYNAME);
         DisplayStr := FileInfo.szDisplayName;
         if (Loop = 0)
         then ParentFolder := DisplayStr
         else if (Length(DisplayStr) > 0)
              then IDList.Add(ParentFolder + '\' + DisplayStr);
      end;
    finally
      GlobalUnlock(StgMedium.hGlobal);
    end;
  finally
    ReleaseStgMedium(StgMedium);
  end;
end; // TmcmDropEnum.GetAsShellIDList.


procedure TmcmDropEnum.GetDescriptor(StgMedium : TStgMedium; List : TStrings);
var ObjDesc : PObjectDescriptor;
    Txt     : string;
begin
  ObjDesc := GlobalLock(StgMedium.hGlobal);
  try
    List.Clear;
    List.Add(Format('%s', [ClassIDToProgID(ObjDesc.clsid)]));
    List.Add(Format('%s', [GuidToString(ObjDesc.clsid)]));

    case ObjDesc.dwDrawAspect of
    0                : List.Add('App didn''t originally draw object');
    DVASPECT_CONTENT : List.Add('Can be displayed as embedded content');
    DVASPECT_ICON    : List.Add('Iconic representation');
    end;

    List.Add(Format('Object extent: (%d,%d)', [ObjDesc.size.x, ObjDesc.size.y]));
    List.Add(Format('Object was clicked at: (%d,%d)', [ObjDesc.point.x, ObjDesc.point.y]));

    if (ObjDesc.dwStatus <> 0)
    then begin
         List.Add(Format('Characteristics: ($%x)', [ObjDesc.dwStatus]));
         if ObjDesc.dwStatus and OLEMISC_RECOMPOSEONRESIZE <> 0
         then List.Add('   Object wants to take charge of resizing image');
         if ObjDesc.dwStatus and OLEMISC_ONLYICONIC <> 0
         then List.Add('   No useful content apart from icon');
         if ObjDesc.dwStatus and OLEMISC_INSERTNOTREPLACE <> 0
         then List.Add('   Object initialised itself from data in container''s current selection');
         if ObjDesc.dwStatus and OLEMISC_STATIC <> 0
         then List.Add('   Static object (no data, only presentation)');
         if ObjDesc.dwStatus and OLEMISC_CANTLINKINSIDE <> 0
         then List.Add('   Cannot be link source that, when bound to, runs the object');
         if ObjDesc.dwStatus and OLEMISC_CANLINKBYOLE1 <> 0
         then List.Add('   Can be linked to by OLE 1 containers');
         if ObjDesc.dwStatus and OLEMISC_ISLINKOBJECT <> 0
         then List.Add('   This is a link object');
         if ObjDesc.dwStatus and OLEMISC_INSIDEOUT <> 0
         then List.Add('   Can be activated in-place without menus or toolbars');
         if ObjDesc.dwStatus and OLEMISC_ACTIVATEWHENVISIBLE <> 0
         then List.Add('   Should be activated whenever visible');
         if ObjDesc.dwStatus and OLEMISC_RENDERINGISDEVICEINDEPENDENT <> 0
         then List.Add('   Appearance will be identical on all target devices');
    end;
    if ObjDesc.dwFullUserTypeName = 0
    then Txt := 'unknown user type'
    else Txt := String(PWideChar(DWord(ObjDesc) + DWord(ObjDesc.dwFullUserTypeName)));
    List.Add(Format('Full user type: %s', [Txt]));
    if ObjDesc.dwSrcOfCopy = 0
    then Txt := 'unknown source'
    else Txt := String(PWideChar(DWord(ObjDesc) + DWord(ObjDesc.dwSrcOfCopy)));
    List.Add(Format('Transfer source: %s', [Txt]));
  finally
    GlobalUnlock(StgMedium.hGlobal);
  end;
end; // TmcmDropEnum.GetDescriptor.


procedure TmcmDropEnum.GetAsObjectDescriptor(ObjDescList: TStrings);
var StgMedium : TStgMedium;
begin
  InitFormatEtc(CF_OBJECTDESCRIPTOR, TYMED_HGLOBAL);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    GetDescriptor(StgMedium, ObjDescList);
  finally
    ReleaseStgMedium(StgMedium);
  end;
end;

(*
procedure TmcmDropEnum.GetAsLinkSrcDescriptor(LinkSrcDescList : TStrings);
var StgMedium : TStgMedium;
begin
  InitFormatEtc(CF_LINKSRCDESCRIPTOR, TYMED_HGLOBAL);
  OleCheck(FDataObject.GetData(FFormatEtc, StgMedium));
  try
    GetDescriptor(StgMedium, LinkSrcDescList)
  finally
    ReleaseStgMedium(StgMedium);
  end;
end;
*)


//------------------------------------------------------------------------------
// TmcmDropTarget.
//------------------------------------------------------------------------------

constructor TmcmDropTarget.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FActive          := False;
  FDragDropEvent   := Nil;
  FDragOverEvent   := Nil;
  FExDragOverEvent := Nil;
  FDropObject      := Nil;
  FParentHandle    := (AOwner as TWinControl).Handle;
  // FParentHandle  := (AOwner as TForm).Handle;

  {$IFNDEF DCB3_5} {$WARN SYMBOL_DEPRECATED OFF} {$ENDIF}
  FNewDefWndProc := MakeObjectInstance(NewDefWndProc); // Deprecated method
  {$IFNDEF DCB3_5} {$WARN SYMBOL_DEPRECATED ON} {$ENDIF}
  FOldDefWndProc := pointer(SetWindowLong(FParentHandle, GWL_WNDPROC, longint(FNewDefWndProc)));
end; // TmcmDropTarget.Create.


destructor TmcmDropTarget.Destroy;
begin
  Active := False;
  SetWindowLong(FParentHandle, GWL_WndProc, longint(FOldDefWndProc));
  {$IFNDEF DCB3_5} {$WARN SYMBOL_DEPRECATED OFF} {$ENDIF}
  FreeObjectInstance(FNewDefWndProc); // Deprecated method
  {$IFNDEF DCB3_5} {$WARN SYMBOL_DEPRECATED ON} {$ENDIF}
  Inherited Destroy;
end; // TmcmDropTarget.Destroy.


procedure TmcmDropTarget.NewDefWndProc(var Msg : TMessage);
begin
  if FActive and (Msg.Msg = WM_DESTROY)
  then Active := False;

  with Msg
  do Result := CallWindowProc(FOldDefWndProc, FParentHandle, Msg, wParam, lParam);
end; // TmcmDropTarget.NewDefWndProc.


procedure TmcmDropTarget.SetActive(Value : boolean);
var hRes : HRESULT;
begin
  if (Value <> FActive)
  then begin
       FActive := Value;
       if Not(csDesigning in ComponentState)
       then begin
            if (FParentHandle <> 0)
            then if FActive
                 then begin
                      // Enable Drag and Drop
                      hRes := RegisterDragDrop(FParentHandle, Self);
                      Assert(hRes = 0, Format('Register Drag Drop failed ($%x)', [hRes]));
                      if (hRes = S_OK)
                      then ;
                      hRes := CoLockObjectExternal(Self, True, False);
                      if (hRes <> S_OK)
                      then Assert(hRes = 0, Format('Couldn''t lock drop target ($%x)', [hRes]));
                 end
                 else begin
                      hRes := RevokeDragDrop(FParentHandle);
                      Assert(hRes = 0, Format('Revoke Drag Drop failed ($%x)', [hRes]));
                      if (hRes = S_OK)
                      then ;
                      hRes := CoLockObjectExternal(Self, False, True);
                      if (hRes <> S_OK)
                      then Assert(hRes = 0, Format('Couldn''t unlock drop target ($%x)', [hRes]));
                      if Assigned(FDropObject)
                      then FDropObject.Free;
                      FDropObject := Nil;
                 end;
       end;
  end;
end; // TmcmDropTarget.SetActive.


function TmcmDropTarget.DoDragEvent(    State       : TDragState;
                                        grfKeyState : longint;
                                        Point       : TPoint;
                                    var dwEffect    : longint) : HResult;
var Receiver  : TObject;
    // hReceiver : THandle;
    Shift     : TShiftState;
begin
  FAcceptDragObj := True;
  if Assigned(FExDragOverEvent)
  then begin
       // Translate key state
       Shift := TranslateKeyState(grfKeyState);
       FPoint := Point;
       {
       hReceiver := WindowFromPoint(Point);
       Receiver := FindControl(hReceiver);
       }
       Receiver := FindDragTarget(FPoint, False); // Find target Delphi control.
       FExDragOverEvent(Receiver, FDropObject, Shift, FPoint.x, FPoint.y, State, FAcceptDragObj, FCopyObj, FMoveObj);

       if FCopyObj
       then dwEffect := DROPEFFECT_COPY
       else if FMoveObj
            then dwEffect := DROPEFFECT_MOVE;
  end
  else if Assigned(FDragOverEvent)
       then begin
            FPoint := Point;
            {
            hReceiver := WindowFromPoint(Point);
            Receiver := FindControl(hReceiver);
            }
            Receiver := FindDragTarget(FPoint, False); // Find target Delphi control.
            FDragOverEvent(Receiver, FDropObject, FPoint.x, FPoint.y, State, FAcceptDragObj);

            if FAcceptDragObj
            then dwEffect := DROPEFFECT_COPY;
       end
       else FAcceptDragObj := False;

  if Not(FAcceptDragObj)
  then dwEffect := DROPEFFECT_NONE;

  Result := S_OK;
end; // TmcmDropTarget.DoDragEvent.


function TmcmDropTarget.DragEnter(const DataObj     : IDataObject;
                                        grfKeyState : longint;
                                        Point       : TPoint;
                                  var   dwEffect    : longint) : HResult;
begin
  // An object is dragged into our application.
  FDropObject  := TmcmDropEnum.Create(DataObj);
  FCopyObj     := False;
  FMoveObj     := False;
  FdwEffect    := dwEffect;
  FgrfKeyState := grfKeyState;
  Result := DoDragEvent(dsDragEnter, grfKeyState, Point, dwEffect);
end; // TmcmDropTarget.DragEnter.


function TmcmDropTarget.DragOver(      grfKeyState : longint;
                                       Point       : TPoint;
                                 var   dwEffect    : longint) : HResult;
begin
  // An object is being dragged over our application form.
  FdwEffect    := dwEffect;
  FgrfKeyState := grfKeyState;
  Result := DoDragEvent(dsDragMove, grfKeyState, Point, dwEffect);
end; // TmcmDropTarget.DragOver.


function TmcmDropTarget.DragLeave : HResult; stdcall;
begin
  // The dragged object/cursor is leaving our application form.
  Result := DoDragEvent(dsDragLeave, FgrfKeyState, FPoint, FdwEffect);

  // Free Drop object.
  FDropObject.Free;
  FDropObject := Nil;
end; // TmcmDropTarget.DragLeave.


function TmcmDropTarget.Drop(const DataObj     : IDataObject;
                                   grfKeyState : longint;
                                   Point       : TPoint;
                             var   dwEffect    : longint) : HResult;
var Receiver  : TObject;
    // hReceiver : THandle;
    Shift     : TShiftState;
begin
  if Assigned(FExDragDropEvent)
  then begin
       // Translate key state
       Shift := TranslateKeyState(grfKeyState);
       FPoint := Point;
       {
       hReceiver := WindowFromPoint(Point);
       Receiver := FindControl(hReceiver);
       }
       Receiver := FindDragTarget(FPoint, False); // Find target Delphi control.
       FExDragDropEvent(Receiver, FDropObject, Shift, FPoint.x, FPoint.y);
  end
  else if Assigned(FDragDropEvent)
       then begin
            FPoint := Point;
            {
            hReceiver := WindowFromPoint(Point);
            Receiver := FindControl(hReceiver);
            }
            Receiver := FindDragTarget(FPoint, False); // Find target Delphi control.
            FDragDropEvent(Receiver, FDropObject, FPoint.x, FPoint.y);
       end;

  Result := DragLeave;
end; // TmcmDropTarget.Drop.


//------------------------------------------------------------------------------
// TmcmFormatEtcItem.
//------------------------------------------------------------------------------

constructor TmcmFormatEtcItem.Create(Collection : TCollection);
begin
  Inherited Create(Collection);
  FName := 'FormatEtcItem';

  with FFormatEtc
  do begin
     cfFormat := CF_NULL;
     dwAspect := DVASPECT_CONTENT;
     ptd      := Nil;
     tymed    := TYMED_HGLOBAL; 
     lindex   := -1;
  end;
end; // TmcmFormatEtcItem.Create.


procedure TmcmFormatEtcItem.Assign(Source : TPersistent);
var Item : TmcmFormatEtcItem;
begin
  if (Source is TmcmFormatEtcItem)
  then begin
       Item := TmcmFormatEtcItem(Source);
       Name := Item.Name;
       Aspect := Item.Aspect;
       Medium := Item.Medium;
       FormatIndex := Item.FormatIndex;
  end
  else Inherited Assign(Source);
end; // TmcmFormatEtcItem.Assign.


function TmcmFormatEtcItem.GetDisplayName : string;
begin
  Result := Name;
  if Result = ''
  then Result := Inherited GetDisplayName;
end; // TmcmFormatEtcItem.GetDisplayName.


procedure TmcmFormatEtcItem.SetName(const Value : string);
begin
  // Get format based on a registered format name.
  if (FName <> Value)
  then begin
       FName := Value;
       (*
       // Try find the name matching the register format value...
       if (FFormatEtc.cfFormat = 0)
       then FFormatEtc.cfFormat := RegisterClipboardFormat(PAnsiChar(Value));
       *)
  end;
end; // TmcmFormatEtcItem.SetName.


function TmcmFormatEtcItem.GetAspect : longint;
begin
  Result := FFormatEtc.dwAspect;
end; // TmcmFormatEtcItem.GetAspect.


procedure TmcmFormatEtcItem.SetAspect(const Value : longint);
begin
  if (FFormatEtc.dwAspect <> Value)
  then begin
       FFormatEtc.dwAspect := Value;
  end;
end; // TmcmFormatEtcItem.SetAspect.


function  TmcmFormatEtcItem.GetFormat : TClipFormat;
begin
  Result := FFormatEtc.cfFormat;
end; // TmcmFormatEtcItem.GetFormat.


procedure TmcmFormatEtcItem.SetFormat(const Value : TClipFormat);
begin
  if (FFormatEtc.cfFormat <> Value)
  then begin
       FFormatEtc.cfFormat := Value;

       // Get format name based on the format enumeration value.
       SetLength(FName, 1024);
       GetClipboardFormatName(Value, PChar(FName), 1024);
       SetLength(FName, StrLen(PChar(FName)))
  end;
end; // TmcmFormatEtcItem.SetFormat.


function TmcmFormatEtcItem.GetFormatIndex : integer;
begin
  Result := FFormatEtc.lindex;
end; // TmcmFormatEtcItem.GetFormatIndex.


procedure TmcmFormatEtcItem.SetFormatIndex(const Value : integer);
begin
  if (FFormatEtc.lindex <> Value)
  then begin
       FFormatEtc.lindex := Value;
  end;
end; // TmcmFormatEtcItem.SetFormatIndex.


function TmcmFormatEtcItem.GetMedium : longint;
begin
  Result := FFormatEtc.tymed;
end; // TmcmFormatEtcItem.GetMedium.


procedure TmcmFormatEtcItem.SetMedium(const Value : longint);
begin
  if (FFormatEtc.tymed <> Value)
  then begin
       FFormatEtc.tymed := Value;
  end;
end; // TmcmFormatEtcItem.SetMedium.


function TmcmFormatEtcItem.GetFormatEtc : TFormatEtc;
begin
  Result := FFormatEtc;
end; // TmcmFormatEtcItem.GetFormatEtc.


//------------------------------------------------------------------------------
// TmcmFormatEtcList.
//------------------------------------------------------------------------------

constructor TmcmFormatEtcList.Create(AOwner : TComponent);
begin
  Inherited Create(TmcmFormatEtcItem);
end; // TmcmFormatEtcList.Create.


function TmcmFormatEtcList.GetItem(Index : integer) : TmcmFormatEtcItem;
begin
  Result := TmcmFormatEtcItem(Inherited GetItem(Index));
end; // TmcmFormatEtcList.GetItem.


procedure TmcmFormatEtcList.SetItem(Index : integer; Value : TmcmFormatEtcItem);
begin
  Inherited SetItem(Index, Value);
end; // TmcmFormatEtcList.SetItem.


function TmcmFormatEtcList.Add : TmcmFormatEtcItem;
begin
  Result := TmcmFormatEtcItem(Inherited Add);
end; // TmcmFormatEtcList.Add.


//------------------------------------------------------------------------------
// TmcmIntfEnumFormatEtc.
//------------------------------------------------------------------------------

constructor TmcmIntfEnumFormatEtc.Create(List : TmcmFormatEtcList; Index : integer);
begin
  Inherited Create(Nil);
  FList := List;
  FIndex := Index;
end; // TmcmIntfEnumFormatEtc.Create.


destructor TmcmIntfEnumFormatEtc.Destroy;
begin
  Inherited Destroy;
end; // TmcmIntfEnumFormatEtc.Destroy.


function TmcmIntfEnumFormatEtc.Next(celt : longint; out elt; pceltFetched : PLongint) : HResult;
var i : integer;
begin
  Result := S_OK;
  try
    i := 0;
    while (i < celt) and (FIndex < FList.Count)
    do begin
       TFormatEtc(elt) := FList[FIndex].FormatEtc;
       inc(FIndex);
       inc(i);
    end;
    if (pceltFetched <> Nil)
    then pceltFetched^ := i;

    if (i <> celt)
    then Result := S_FALSE;
  except
    Result := E_UNEXPECTED;
  end;
end; // TmcmIntfEnumFormatEtc.Next.


function TmcmIntfEnumFormatEtc.Skip(celt : longint) : HResult;
begin
  Result := S_OK;
  try
    if (Celt <= FList.Count - FIndex)
    then FIndex := FIndex + Celt
    else begin
         FIndex := FList.Count;
         Result := S_FALSE;
    end
  except
    Result := E_UNEXPECTED;
  end;
end; // TmcmIntfEnumFormatEtc.Skip.


function TmcmIntfEnumFormatEtc.Reset : HResult;
begin
  Result := S_OK;
  try
    FIndex := 0;
  except
    Result := E_UNEXPECTED;
  end;
end; // TmcmIntfEnumFormatEtc.Reset.


function TmcmIntfEnumFormatEtc.Clone(out Enum : IEnumFormatEtc) : HResult;
begin
  Result := S_OK;
  try
    Enum := TmcmIntfEnumFormatEtc.Create(FList, FIndex);
  except
    Result := E_UNEXPECTED;
  end;
end; // TmcmIntfEnumFormatEtc.Clone.


//------------------------------------------------------------------------------
// TmcmIntfDataSource.
//------------------------------------------------------------------------------

function CompareTargetDevice(ptd1, ptd2 : PDVTargetDevice) : boolean;
begin
  // Compares two target device records.
  Result := (ptd1 = ptd2) or
            (Assigned(ptd1) and Assigned(ptd2) and (ptd1.tdSize = ptd2.tdSize) and
            CompareMem(ptd1, ptd2, ptd1.tdSize));
end; // CompareTargetDevice.


function CompareFormatEtc(const FormatEtc1, FormatEtc2 : TFormatEtc) : integer;
begin
  // Compares two TFormatEtc structures.
  // Are these formats the same ?
  if (FormatEtc1.cfFormat <> FormatEtc2.cfFormat) or
     (FormatEtc1.lindex   <> FormatEtc2.lIndex) or
     // are Target Device same
     Not(CompareTargetDevice(FormatEtc1.ptd, FormatEtc2.ptd)) or
     // Is Aspect1 equal to or a subset of Aspect2
     (integer(FormatEtc2.dwAspect) and Not(integer(FormatEtc1.dwAspect)) <> 0) or
     // is tymed1 a subset or equal to tymed2
     (integer(FormatEtc2.tymed) and Not(integer(FormatEtc1.tymed)) <> 0)
  then Result := 0 // No match
  else if (FormatEtc1.dwAspect = FormatEtc2.dwAspect) and
          (FormatEtc1.tymed = FormatEtc2.tymed)
       then Result := 1 // Exact match
       else Result := -1; // Partial match.
end; // CompareFormatEtc.


constructor TmcmIntfDataSource.Create(Formats : TmcmFormatEtcList; Handles : TmcmDataHandles);
begin
  Inherited Create;

  FFormatEtcList := Formats;
  FHandles       := Handles;
end; // TmcmIntfDataSource.Create.


destructor TmcmIntfDataSource.Destroy;
var i      : integer;
    Medium : TStgMedium;
begin
  // Release data copies.
  Medium.unkForRelease := Nil;
  for i := 0 to (FFormatEtcList.Count - 1)
  do begin
     Medium.Tymed := FFormatEtcList.Items[i].Medium;
     Medium.hGlobal := FHandles[i];
     ReleaseStgMedium(Medium);
  end;
  // Free FormateEtc list
  FFormatEtcList.Free;

  Inherited Destroy;
end; // TmcmIntfDataSource.Destroy.


function TmcmIntfDataSource.GetData(const FormatEtcIn : TFormatEtc; out Medium : TStgMedium) : HResult;
var i : integer;
begin
  Result := DV_E_FORMATETC; // <- If no matching FormatEtc record is found.
  try
    ZeroMemory(@Medium, sizeof (TStgMedium));

    for i := 0 to (FFormatEtcList.Count - 1)
    do begin
       if (CompareFormatEtc(FormatEtcIn, FFormatEtcList[i].FormatEtc) <> 0)
       then begin
            Medium.tymed := FFormatEtcList[i].FormatEtc.tymed;  // use the media we have
            if ((FormatEtcIn.tymed and TYMED_HGLOBAL) = TYMED_HGLOBAL) or
               ((FormatEtcIn.tymed and TYMED_MFPICT) = TYMED_MFPICT) or
               ((FormatEtcIn.tymed and TYMED_ENHMF) = TYMED_ENHMF)
            then begin
                 Medium.hGlobal := OleDuplicateData(FHandles[i], FormatEtcIn.cfFormat, 0);
                 Assert(Medium.hGlobal <> 0, 'OleDuplicateData Failed');
            end
            else if ((FormatEtcIn.tymed and TYMED_GDI) = TYMED_GDI)
                 then begin
                      if (FormatEtcIn.cfFormat = CF_BITMAP)
                      then Medium.hBitmap := OleDuplicateData(FHandles[i], FormatEtcIn.cfFormat, 0)
                      else Medium.hGlobal := OleDuplicateData(FHandles[i], FormatEtcIn.cfFormat, 0);
                 end
                 // TYMED_FILE      - File
                 // TYMED_ISTREAM   - Stream interface
                 // TYMED_ISTORAGE  - Storage interface
                 else Medium.hGlobal := FHandles[i];

            Result := S_OK;
            Break;
       end;
    end;
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.GetData.


function TmcmIntfDataSource.GetDataHere(const FormatEtc : TFormatEtc; out Medium : TStgMedium) : HResult;
begin
  Result := E_NOTIMPL;
  try
//    GetDataHere(formatetc, medium, integer (Result))
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.GetDataHere.


function TmcmIntfDataSource.QueryGetData(const FormatEtc : TFormatEtc) : HResult;
var i : integer;
begin
  Result := DV_E_FORMATETC;
  try
    for i := 0 to (FFormatEtcList.Count - 1)
    do if (CompareFormatEtc(FormatEtc, FFormatEtcList[i].FormatEtc) <> 0)
       then begin
            Result := S_OK;
            Break;
       end;
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.QueryGetData.


function TmcmIntfDataSource.GetCanonicalFormatEtc(const FormatEtc : TFormatEtc; out FormatEtcOut : TFormatEtc) : HResult;
begin
  FormatEtcOut.ptd := nil;
  Result := DATA_S_SAMEFORMATETC;
  try
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.GetCanonicalFormatEtc.


function TmcmIntfDataSource.SetData(const FormatEtc : TFormatEtc; var Medium : TStgMedium; fRelease : BOOL) : HResult;
begin
  Result := E_NOTIMPL;
  try
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.SetData.


function TmcmIntfDataSource.EnumFormatEtc(dwDirection : Longint; out enumFormatEtc_ : IEnumFormatEtc) : HResult;
begin
  try
     if (dwDirection = DATADIR_GET)
     then begin
          enumFormatEtc_ := TmcmIntfEnumFormatEtc.Create(FFormatEtcList, 0);
          Result := S_OK;
     end
     else Result := E_NOTIMPL;
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.EnumFormatEtc.


function TmcmIntfDataSource.DAdvise(const FormatEtc : TFormatEtc; advf : Longint; const advSink : IAdviseSink; out dwConnection : Longint) : HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
  try
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.DAdvise.


function TmcmIntfDataSource.DUnadvise(dwConnection : Longint) : HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
  try
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.DUnadvise.


function TmcmIntfDataSource.EnumDAdvise(out enumAdvise : IEnumStatData) : HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
  try
  except
    Result := E_UNEXPECTED;
  end
end; // TmcmIntfDataSource.EnumDAdvise.


//------------------------------------------------------------------------------
// TmcmCustomDataSource.
//------------------------------------------------------------------------------

constructor TmcmCustomDataSource.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FIDHandle     := 0; // Source forms/controls Handle. 
  FEnableDesc   := False;
  FEnableEffect := False; 
  FEnableDesc   := True;
  FEnableEffect := True;
end; // TmcmCustomDataSource.Create.


destructor TmcmCustomDataSource.Destroy;
begin
  Inherited Destroy;
end; // TmcmCustomDataSource.Destroy.


procedure TmcmCustomDataSource.Notification(AComponent : TComponent;
                                            Operation  : TOperation);
begin
  Inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FComponent)
  then FComponent := Nil;
end; // TmcmCustomDataSource.Notification.


function TmcmCustomDataSource.SetAs(var Data; Size : integer) : hGlobal;
var pData : pointer;
begin
  Result := GlobalAlloc(GHND, Size);
  if (Result = 0)
  then OutOfMemoryError;
  pData := GlobalLock(Result);
  try
    Move(Data, pData^, Size);
  finally
    GlobalUnlock(Result);
  end
end; // TmcmCustomDataSource.SetAsInteger.


function TmcmCustomDataSource.SetAsImage(AFormat : word; Image : TmcmImage) : hGlobal;
var AHandle : THandle;
begin
  Image.SaveToClipboardFormat(AFormat, AHandle);
  Result := AHandle;
end; // TmcmCustomDataSource.SetAsImage.


function TmcmCustomDataSource.SetAsBitmap(Bitmap : TBitmap) : HBITMAP;
var Format : TClipFormat;
    Pal    : hPalette;
begin
  if (Not(Assigned(Bitmap)) or Bitmap.Empty)
  then Result := 0
  else begin
       Bitmap.SaveToClipboardFormat(Format, THandle(Result), Pal);
       if (Format <> CF_BITMAP)
       then begin
            DeleteObject(Result);
            Result := 0;
       end;
       if (Pal <> 0)
       then DeleteObject(Pal);
  end
end; // TmcmCustomDataSource.SetAsBitmap.


function TmcmCustomDataSource.SetAsFilename(StrList : TStringList) : hGlobal;
var pStr : PChar;
begin
  Result := 0;
  if (StrList.Count > 0)
  then begin
       Result := GlobalAlloc(GHND, Length(StrList.Strings[0]) + 1);
       pStr := GlobalLock(Result);
       try
         StrPCopy(pStr, StrList.Strings[0]);
       finally
         GlobalUnlock(Result);
       end;
  end;
end; // TmcmCustomDataSource.SetAsFilename.


type
  TDropFiles = packed record
               Size     : DWORD;        // Offset of file list
               MousePos : TPoint;       // Drop point (coordinates depend on fNC)
               fNC      : BOOL;         // True if mouse was in the client area
               fWide    : BOOL;         // True if file contains wide characters.
               // Name list             // File names zero terminated (ends #0#0 eg
  end;                                  // name1#0name2#0name3#0#0 )
  PDropFiles = ^TDropFiles;

function TmcmCustomDataSource.SetAsHDrop(StrList : TStringList) : hGlobal;
var i            : integer;
    HandleStream : TmcmHandleStream;
    AHandle      : THandle;
    DropFiles    : TDropFiles;
    ByteStr      : String;
    WideStr      : WideString;
    ZeroByte     : byte;
begin
  Result := 0;
  if (StrList.Count > 0)
  then begin
       ZeroByte := 0;

       DropFiles.Size := SizeOf(TDropFiles);
       GetCursorPos(DropFiles.MousePos);
       AHandle := WindowFromPoint(DropFiles.MousePos);
       DropFiles.fNC := SendMessage(AHandle, WM_NCHITTEST, 0, MakeLParam(DropFiles.MousePos.X, DropFiles.MousePos.Y)) <> HTCLIENT;
       {$IFDEF GE_DXE2}WinApi.{$ENDIF}Windows.ScreenToClient(AHandle, DropFiles.MousePos);
       DropFiles.fWide := (Win32Platform = VER_PLATFORM_WIN32_NT); // Determite if windows has unicode capability

       HandleStream := TmcmHandleStream.Create(0);
       HandleStream.Write(DropFiles, SizeOf(TDropFiles));

       for i := 0 to (StrList.Count - 1)
       do begin
          if DropFiles.fWide
          then begin
               WideStr := StrList.Strings[i];
               HandleStream.WriteBuffer(WideStr[1], 2 * Length(WideStr));
          end
          else begin
               ByteStr := StrList.Strings[i];
               HandleStream.WriteBuffer(ByteStr[1], Length(ByteStr));
          end;
          HandleStream.Write(ZeroByte, SizeOf(byte)); // Null termination.
          if DropFiles.fWide
          then HandleStream.Write(ZeroByte, SizeOf(byte)); // Null termination.
       end;
       HandleStream.Write(ZeroByte, SizeOf(byte)); // Double null termination.
       if DropFiles.fWide
       then HandleStream.Write(ZeroByte, SizeOf(byte)); // Null termination.

       Result := HandleStream.ReleaseHandle;
       HandleStream.Free;
  end;
end; // TmcmCustomDataSource.SetAsHDrop.


function TmcmCustomDataSource.SetAsObjectDesc(OD : TObjectDescriptor; Src : string) : hGlobal;
var SrcLen       : integer;
    UTNLen       : integer;
    DescSize     : integer;
    HandleStream : TmcmHandleStream;
    ResultWStr   : PWideChar;
begin
  UTNLen := 0;
  // ...
  if (UTNLen > 0)
  then OD.dwFullUserTypeName := SizeOf(TObjectDescriptor)
  else OD.dwFullUserTypeName := 0;

  if (Src <> '')
  then begin
       {$IFDEF UNICODE}
         SrcLen := Length(Src) + 1;
       {$ELSE}
         SrcLen := MultiByteToWideChar(0, 0, PAnsiChar(Src), Length(Src), nil, 0) + 1;
       {$ENDIF}
  end
  else SrcLen := 0;
  if (SrcLen > 0)
  then begin
       if (OD.dwFullUserTypeName = 0)
       then OD.dwSrcOfCopy := SizeOf(TObjectDescriptor)
       else OD.dwSrcOfCopy := OD.dwFullUserTypeName + UTNLen * SizeOf(WideChar);
  end
  else OD.dwSrcOfCopy := 0;

  DescSize := SizeOf(TObjectDescriptor) + (UTNLen + SrcLen) * SizeOf(WideChar);
  OD.cbSize := DescSize;

  HandleStream := TmcmHandleStream.Create(0);
  HandleStream.SetSize(DescSize);
  HandleStream.Write(OD, SizeOf(TObjectDescriptor));

  if (UTNLen > 0)
  then begin
       // write FullUserTypeName....
  end;

  if (SrcLen > 0)
  then begin
       ResultWStr := PWideChar(integer(HandleStream.Memory) + OD.dwSrcOfCopy);
       {$IFDEF UNICODE}
         CopyMemory(ResultWStr, @Src[1], Length(Src) * SizeOf(WideChar));
       {$ELSE}
         MultiByteToWideChar(0, 0, PAnsiChar(Src), Length(Src), ResultWStr, SrcLen);
       {$ENDIF}
         ResultWStr[SrcLen-1] := #0;
  end;

  Result := HandleStream.ReleaseHandle;
  HandleStream.Free;
end; // TmcmCustomDataSource.SetAsObjectDesc.


function TmcmCustomDataSource.GetData(Format : TClipFormat; Medium : longint; Aspect : dword; AIndex : integer) : THandle;
var ObjectDescriptor : TObjectDescriptor;
    HandleStream     : TmcmHandleStream;
begin
  Result := CF_NULL;

  if (Format = CF_MCMSOURCEID)
  then begin
       Result := SetAs(FIDHandle, SizeOf(THandle));
       Exit;
  end;

  if (Format = CF_PREFERREDDROPEFFECT)
  then begin
       Result := SetAs(FDropEffect, SizeOf(FDropEffect));
       Exit;
  end;

  if (Format = CF_OBJECTDESCRIPTOR)
  then begin
       ZeroMemory(@ObjectDescriptor, Sizeof(TObjectDescriptor));

       ObjectDescriptor.point.x := 0;
       ObjectDescriptor.point.y := 0;
       ObjectDescriptor.size.x := 0;
       ObjectDescriptor.size.y := 0;

       Result := SetAsObjectDesc(ObjectDescriptor, Application.ExeName);
       Exit;
  end;

  // TStringList, file names
  if (FComponent is TStringList)
  then begin
       if (Format = CF_FILENAME)
       then Result := SetAsFilename((FComponent as TStringList));

       if (Format = CF_HDROP)
       then Result := SetAsHDrop((FComponent as TStringList));
  end;

  // TmcmImageCtrl and TmcmImage
  if (FComponent is TmcmImage) or (FComponent is TmcmImageCtrl)
  then begin
       if (FComponent is TmcmImage)
       then begin
            if (Format = CF_DIB) or (Format = CF_BITMAP)
            then Result := SetAsImage(Format, (FComponent as TmcmImage));
       end
       else begin
            if (Format = CF_DIB) or (Format = CF_BITMAP)
            then Result := SetAsImage(Format, (FComponent as TmcmImageCtrl).Image);
       end;
  end;

  if (FComponent is TImage) and Assigned((FComponent as TImage).Picture)
  then begin
       if Assigned((FComponent as TImage).Picture.Graphic)
       then begin
            if (Format = CF_BITMAP)
            then begin
                 if ((FComponent as TImage).Picture.Graphic is TBitmap)
                 then Result := SetAsBitmap((FComponent as TImage).Picture.Bitmap);
                 (*
                 if ((FComponent as TImage).Picture.Graphic is TJPEGImage)
                 then Result := 0;
                 *)
            end;

            (*
            if (Format = CF_DIB)
            then begin
                 HandleStream := TmcmHandleStream.Create(0);
                 if ((FComponent as TImage).Picture.Graphic is TBitmap)
                 then (FComponent as TImage).Picture.Graphic.SaveToStream(HandleStream);
                 Result := HandleStream.ReleaseHandle;
                 HandleStream.Free;
            end;
            *)
            if (Medium = TYMED_GDI) and (Format = CF_PALETTE)
            then begin
                 Result := CopyPalette((FComponent as TImage).Picture.Graphic.Palette);
            end;

            if (Medium = TYMED_GDI) and (Format = CF_ICON)
            then begin
                 HandleStream := TmcmHandleStream.Create(0);
                 if ((FComponent as TImage).Picture.Graphic is TIcon)
                 then (FComponent as TImage).Picture.Icon.SaveToStream(HandleStream);
                 Result := HandleStream.ReleaseHandle;
                 HandleStream.Free;

            end;
       end;
  end;

end; // TmcmCustomDataSource.GetData.


function TmcmCustomDataSource.GetFormats : TmcmFormatEtcList;
var Pal        : HPalette;
    i          : integer;
    IsFileName : boolean;
begin
  Result := TmcmFormatEtcList.Create(Nil);

  // Image
  if (FComponent is TmcmImage) or (FComponent is TmcmImageCtrl)
  then begin
       with Result.Add
       do begin
          Format := CF_DIB;
          Medium := TYMED_HGLOBAL;
       end;

       with Result.Add
       do begin
          Format := CF_BITMAP;
          Medium := TYMED_GDI;
       end;

       Pal := 0;
       if (FComponent is TmcmImage)
       then Pal := (FComponent as TmcmImage).Palette
       else if (FComponent is TmcmImageCtrl)
            then Pal := (FComponent as TmcmImageCtrl).Image.Palette;
       if (Pal <> 0)
       then begin
            DeleteObject(Pal);
            with Result.Add
            do begin
               Format := CF_PALETTE;
               Medium := TYMED_GDI;
            end;
       end;
  end;

  // Image
  if (FComponent is TImage) and Assigned((FComponent as TImage).Picture)
  then begin
       if Assigned((FComponent as TImage).Picture.Graphic)
       then begin
            if ((FComponent as TImage).Picture.Graphic is TBitmap)
               (*or
               ((FComponent as TImage).Picture.Graphic is TJPEGImage)
               *)
            then begin
                 with Result.Add
                 do begin
                    Format := CF_BITMAP;
                    Medium := TYMED_GDI;
                 end;

                 (*
                 with Result.Add
                 do begin
                    Format := CF_DIB;
                    Medium := TYMED_HGLOBAL;
                 end;
                 *)

                 if ((FComponent as TImage).Picture.Graphic.Palette <> 0)
                 then begin
                      with Result.Add
                      do begin
                         Format := CF_PALETTE;
                         Medium := TYMED_GDI;
                      end;
                 end;
            end;

            if ((FComponent as TImage).Picture.Graphic is TIcon)
            then begin
                 with Result.Add
                 do begin
                    Format := CF_ICON;
                    Medium := TYMED_HGLOBAL;
                 end;
            end;
       end;
  end;

  // Text or file names
  if (FComponent is TStringList)
  then begin
       if ((FComponent as TStringList).Count > 0)
       then begin
            // Check if StringList contains text or file names.
            IsFileName := True;
            i := 0;
            while (i < (FComponent as TStringList).Count) and IsFileName
            do begin
               IsFileName := FileExists((FComponent as TStringList).Strings[i]);
               inc(i);
            end;

            if IsFileName
            then begin
                 // StringList contains File names.
                 with Result.Add
                 do begin
                    Format := CF_FILENAME;
                    Medium := TYMED_HGLOBAL;
                 end;

                 with Result.Add
                 do begin
                    Format := CF_HDROP;
                    Medium := TYMED_HGLOBAL;
                 end;
            end
            else begin
                 // StringList contains plain text.
                 with Result.Add
                 do begin
                    Format := CF_TEXT;
                    Medium := TYMED_HGLOBAL;
                 end;
            end;
       end;
  end;

  if (Result.Count > 0)
  then begin
       // Internal Window identifier CF_MCMSOURCEID
       if (FIDHandle <> 0)
       then begin
            with Result.Add
            do begin
               Format := CF_MCMSOURCEID;
               Medium := TYMED_HGLOBAL;
            end;
       end;

       // Drop effects
       if FEnableEffect
       then begin
            with Result.Add
            do begin
               Format := CF_PREFERREDDROPEFFECT;
               Medium := TYMED_HGLOBAL;
            end;
       end;

       // Object description
       if FEnableDesc
       then begin
            with Result.Add
            do begin
               Format := CF_OBJECTDESCRIPTOR;
               Medium := TYMED_HGLOBAL;
            end;
       end;
  end;
end; // TmcmCustomDataSource.GetFormats.


function TmcmCustomDataSource.GetDataObject : IDataObject;
var i       : integer;
    AMedium : longint;
    Handles : TmcmDataHandles;
    Formats : TmcmFormatEtcList;
begin
  Result := Nil;

  Formats := GetFormats;
  FillMemory(@Handles, SizeOf(Handles), 0);
  with Formats
  do if (Count > 0)
     then begin
          //SetLength(Handles, Count);
          for i := 0 to (Count - 1)
          do with Items[i]
             do begin
                // We only know about Global, GDI, MetaFilePict and
                // EnhancedMetaFile type medium (all global memory).
                AMedium := TYMED_HGLOBAL;
                if (Medium = TYMED_GDI)
                then AMedium := TYMED_GDI
                else if (Medium = TYMED_MFPICT)
                     then AMedium := TYMED_MFPICT
                     else if (Medium = TYMED_ENHMF)
                          then AMedium := TYMED_ENHMF;
                Handles[i] := GetData(Format, AMedium, Aspect, FormatIndex);
             end;
          Result := TmcmIntfDataSource.Create(Formats, Handles);
     end;
end; // TmcmCustomDataSource.GetDataObject.


procedure TmcmCustomDataSource.CopyToClipboard;
begin
  FDropEffect := DROPEFFECT_COPY;
  OleCheck(OleSetClipboard (DataObject))
end; // TmcmCustomDataSource.CopyToClipboard.


procedure TmcmCustomDataSource.CutToClipboard;
begin
  FDropEffect := DROPEFFECT_MOVE;
  OleCheck(OleSetClipboard (DataObject))
end; // TmcmCustomDataSource.CutToClipboard.


//------------------------------------------------------------------------------
// TmcmDropSource.
//------------------------------------------------------------------------------

constructor TmcmDropSource.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  Control := (AOwner as TControl);
  FComponent := FControl;

  FCopy     := True;
  FLink     := False;
  FMove     := False;
  FDragging := False;
  FDragMode := dmManual;

  FDragInverval  := 500;
  FDragThreshold := 5;
  FDragTimer := Nil;

  FDataSource     := Nil;
  FOldDefWndProc  := Nil;
  FEndDragEvent   := Nil;
  FOnPreStartDrag := Nil;
  FStartDragEvent := Nil;
end; // TmcmDropSource.Create.


destructor TmcmDropSource.Destroy;
begin
  Disconnect;
  Inherited Destroy
end; // TmcmDropSource.Destroy.


procedure TmcmDropSource.NewDefWndProc(var Msg : TMessage);
var Pos : TPoint;
begin
  FOldDefWndProc(Msg);

  if (Msg.Msg = WM_LBUTTONDOWN)
  then begin
       FMousePos.x := LOWORD(Msg.LParam);  // horizontal position of cursor
       FMousePos.y := HIWORD(Msg.LParam);  // vertical position of cursor
       FDragTimer.Enabled := True;
  end;

  if (Msg.Msg = WM_LBUTTONUP)
  then begin
       FDragTimer.Enabled := False;
  end;

  if (Msg.Msg = WM_MOUSEMOVE)
  then begin
       if FDragTimer.Enabled
       then begin
            Pos.x := LOWORD(Msg.LParam);  // horizontal position of cursor
            Pos.y := HIWORD(Msg.LParam);  // vertical position of cursor

            if ((Sqr(FMousePos.x - Pos.x) + Sqr(FMousePos.y - Pos.y)) > FDragThreshold * FDragThreshold)
            then begin
                 FDragTimer.Enabled := False;
                 BeginDrag;
            end;
       end;
  end;
end; // TmcmDropSource.NewDefWndProc.


procedure TmcmDropSource.OnDragTimer(Sender : TObject);
begin
  FDragTimer.Enabled := False;
  BeginDrag;
end; // TmcmDropSource.OnDragTimer.


procedure TmcmDropSource.Connect;
begin
  FDragTimer := TTimer.Create(Self);
  FDragTimer.Enabled  := False;
  FDragTimer.Interval := FDragInverval;
  FDragTimer.OnTimer  := OnDragTimer;

  if Not(csDesigning in ComponentState)
  then begin
       if Assigned(FControl) and (FControl is TControl)
       then begin
            FOldDefWndProc := (FControl as TControl).WindowProc;
            (FControl as TControl).WindowProc := NewDefWndProc;
       end;
  end;
end; // TmcmDropSource.Connect.


procedure TmcmDropSource.Disconnect;
begin
  if Assigned(FDragTimer)
  then FDragTimer.Free;
  FDragTimer := Nil;

  if Assigned(FOldDefWndProc)
  then begin
       if Assigned(FControl) and (FControl is TControl)
       then begin
            (FControl as TControl).WindowProc := FOldDefWndProc;
            FOldDefWndProc := Nil;
       end;
  end;
end; // TmcmDropSource.Disconnect.


procedure TmcmDropSource.SetControl(Value : TControl);
begin
  if (FControl <> Value)
  then begin
       Disconnect;
       FControl := Value;
       FComponent := FControl;
       if Assigned(FControl)
       then begin
            if (FDragMode = dmAutomatic)
            then Connect;
            FControl.FreeNotification(Self);
       end;
  end;
end; // TmcmDropSource.SetControl.


procedure TmcmDropSource.Notification(AComponent : TComponent;
                                      Operation  : TOperation);
begin
  Inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FControl)
  then begin
       Disconnect;
       FControl := Nil;
  end;
end; // TmcmDropSource.Notification.


procedure TmcmDropSource.SetDragMode(Value : TDragMode);
begin
  if (FDragMode <> Value)
  then begin
       FDragMode := Value;
       if (FDragMode = dmAutomatic)
       then Connect
       else Disconnect;
  end;
end; // TmcmDropSource.SetDragMode.


function TmcmDropSource.BeginDrag : boolean;
var Effects    : integer;
    hRes       : HRESULT;
    DataObject : IDataObject;
    Cancel     : boolean;
    FreeData   : boolean;
    DragObject : TDragObject;
    CursorPos  : TPoint;
begin
  Result := False;
  if Not(FDragging)
  then begin
       if Assigned(FOnPreStartDrag)
       then begin
            GetCursorPos(CursorPos);
            FOnPreStartDrag(Self, CursorPos.X, CursorPos.Y, FComponent);
            if Not(Assigned(FComponent))
            then Exit;
       end;

       // Set-up drag effect/cursor.
       Effects := DROPEFFECT_NONE;
       if FCopy
       then Effects := Effects or DROPEFFECT_COPY;
       if FMove
       then Effects := Effects or DROPEFFECT_MOVE;
       if FLink
       then Effects := Effects or DROPEFFECT_LINK;
       // DROPEFFECT_SCROLL

       FreeData := False;
       if Not(Assigned(FDataSource))
       then begin
            FDataSource := TmcmCustomDataSource.Create(Self);
            FDataSource.LinkControl := FComponent;
            FreeData := True;
       end;

       try
         if (Effects <> DROPEFFECT_NONE) and Assigned(FDataSource)
         then begin
              // Used to identify who the Drop source owner is.
              if (Owner is TWinControl)
              then FDataSource.IDHandle := (Owner as TWinControl).Handle
              else FDataSource.IDHandle := THandle(Owner);

              // Tell the dataobject what effects are called for
              FDataSource.DropEffect := Effects;

              // Get IDataObject to drag.
              DataObject := FDataSource.DataObject;
              try
                // Fire the OnBeforeDrag event passing the dataobject, the effects
                // and allow the punter to cancel the drag
                Cancel := False;
                DragObject := Nil;
                if Assigned(FStartDragEvent)
                then FStartDragEvent(FComponent, DragObject);

                if Not(Assigned(DataObject))
                then Cancel := True;

                if Cancel
                then hRes := DRAGDROP_S_CANCEL
                else begin
                     // Call the API
                     FDragging := True;
                     try
                       hRes := DoDragDrop(DataObject, Self, Effects, FEffect);
                     finally
                       FDragging := False;
                     end;
                end;
                // Deal with the results, returning the function true if a drop occured,
                // False if a cancel occurred, raise an exception on error (via OleCheck)
                case hRes of
                DRAGDROP_S_DROP   : Result := True;
                DRAGDROP_S_CANCEL : ; // Result := False;
                //E_OUTOFMEMORY
                //E_UNSPEC
                else OleCheck(hRes);
                end;

                // Fire the AfterDrag event to allow the punter to take action
                if Assigned(FEndDragEvent)
                then begin
                     GetCursorPos(CursorPos);
                     FEndDragEvent(FComponent, Nil, CursorPos.x, CursorPos.y);
                end;
              finally
                // Release DataObject.
                DataObject := Nil;
              end;
         end;
       finally
         if FreeData
         then begin
              FDataSource.Free;
              FDataSource := Nil;
         end;
       end;
  end;
end; // TmcmDropSource.BeginDrag.


function TmcmDropSource.QueryContinueDrag(fEscapePressed : BOOL;
                                          grfKeyState    : longint) : HResult;
begin
  Result := S_OK;

  if fEscapePressed
  then Result := DRAGDROP_S_CANCEL // Cancel the drop operation.
  else if Not((grfKeyState and MK_LBUTTON) = MK_LBUTTON)
       then Result := DRAGDROP_S_DROP; // Object was Dropped.
end; // TmcmDropSource.QueryContinueDrag.


function TmcmDropSource.GiveFeedback(dwEffect : longint) : HResult;
//var Scrolling : boolean;
begin
  //  Scrolling := (dwEffect and DROPEFFECT_SCROLL) <> 0;
  if (dwEffect = DROPEFFECT_NONE) // Drop target cannot accept the data.
  then Result := DRAGDROP_S_USEDEFAULTCURSORS // S_OK
  else Result := DRAGDROP_S_USEDEFAULTCURSORS;

  if ((dwEffect and DROPEFFECT_COPY) = DROPEFFECT_COPY)
  then  // Drop results in a copy. The original data is untouched by the drag source.
  else if ((dwEffect and DROPEFFECT_MOVE) = DROPEFFECT_MOVE)
       then // Drag source should remove the data.
       else if ((dwEffect and DROPEFFECT_LINK) = DROPEFFECT_LINK)
            then ;// Drag source should create a link to the original data.
end; // TmcmDropSource.GiveFeedback.


initialization // Initialise OLE.
  OleCheck(OleInitialize(Nil));

  // Register additional formats.
  CF_FILENAME          := RegisterClipboardFormat(CFSTR_FILENAMEA);
  CF_RTF               := RegisterClipboardFormat(CFSTR_RTF);
  CF_IDLIST            := RegisterClipboardFormat(CFSTR_SHELLIDLIST);
  CF_OBJECTDESCRIPTOR  := RegisterClipboardFormat(CFSTR_OBJECTDESCRIPTOR);
  //CF_LINKSRCDESCRIPTOR := RegisterClipboardFormat(CFSTR_LINKSRCDESCRIPTOR);
  CF_PREFERREDDROPEFFECT := RegisterClipboardFormat(CFSTR_PREFERREDDROPEFFECT);  // Preferred DropEffect
  CF_ICON              := RegisterClipboardFormat(CFSTR_ICON);
  CF_MCMSOURCEID       := RegisterClipboardFormat(CFSTR_MCMSOURCEID);

finalization
  OleUninitialize;

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
