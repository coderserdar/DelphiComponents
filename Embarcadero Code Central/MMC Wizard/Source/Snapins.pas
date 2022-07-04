(*==========================================================================*
 | unit Snapins                                                             |
 |                                                                          |
 | Wrapper for MMC snapins                                                  |
 |                                                                          |
 | * LIMITATION - Does not support out-of-process snapins.  But I'm not     |
 |                sure that MMC does, either!                               |
 |                                                                          |
 | * NB.  If you use Active Forms as view pane controls, you *may* have     |
 |        to say SendMessage (ParkingWindow, WM_CLOSE, 0, 0) in the         |
 |        active form's destructor.                                         |
 |                                                                          |
 | * NB   Version nos from Package version info.                            |
 |                                                                          |
 | Version  Date     By            Description                              |
 | -------  -------  ------------  -----------------------------------------|
 | 1.1.0.0  17/8/99  Colin Wilson  Original                                 |
 |          27/7/00  Colin Wilson  Finally Released                         |
 |                                                                          |
 | 1.2.0.0  8/8/00   Colin Wilson  Correct Version Info used if dynamically |
 |                                 linked.                                  |
 |                                                                          |
 |                                 Menus now work for sub-scope items       |
 |                                                                          |
 | Outstanding issues:                                                      |
 |                                                                          |
 |   Can't add menu buttons at run-time ?                                   |
 |   Menus don't work for sub-scope items.                                  |
 |                                                                          |
 | Customer Requests:                                                       |
 |                                                                          |
 |   Support for multiple result panes per scope at design time ?           |
 *==========================================================================*)
unit Snapins;

interface

uses Windows, Messages, ActiveX, SysUtils, ComObj, Classes, Menus, MMC_TLB,SnapinData, Graphics, CommCtrl, imglist, controls , contnrs;

type

HSCOPEITEM = LongInt;

(*--------------------------------------------------------------------------*
 | class TInternal                                                          |
 |                                                                          |
 | Internal objects get sent by MMC in IDataObjects using the               |
 | CCF_SNAPIN_INTERNAL format.  The TObject will either be a TScopeItem or  |
 | a TResultItem object.                                                    |
 *--------------------------------------------------------------------------*)
TInternal = record
  m_type : _DATA_OBJECT_TYPES;        // What context is the data object.
  m_cookie : LongInt;                 // What object the cookie represents
  m_clsid : TGuid;                    // Class ID of who created this data object
  m_object : TObject;                 // Hence won't work with DCOM...
end;
PInternal = ^TInternal;

TSnapinComponent = class;

(*--------------------------------------------------------------------------*
 | class TSnapinComponentData                                               |
 |                                                                          |
 | Snapin 'doc' object                                                      |
 *--------------------------------------------------------------------------*)
TSnapinComponentData = class (TComObject, IComponentData, IExtendContextMenu, IExtendPropertySheet)
private
  fConsole : IConsole;
  fConsole2 : IConsole2;
  fConsoleNameSpace : IConsoleNameSpace;
  fScopeImageList : IImageList;
  fSnapinData : TSnapinData;
  FInitialized: boolean;
  fIsDirty : boolean;
  fExpanding : boolean;
  fSnapinComponents : TObjectList;
  fActiveComponent : Integer;

  function GetNodeForDataObject (const dataObj : IDataObject; var ownerDataIdx : Integer) : TObject;
  function GetNodeForCookie (cookie : Integer) : TObject;
  function GetWindowHandle: HWND;
  procedure UpdateScopeSettings (scope : TScopeItem);

protected
  function IComponentData.Initialize = ComponentDataInitialize;
  function IComponentData.CreateComponent = ComponentDataCreateComponent;
  function IComponentData.Notify = ComponentDataNotify;
  function IComponentData.Destroy = ComponentDataDestroy;
  function IComponentData.QueryDataObject = ComponentDataQueryDataObject;
  function IComponentData.GetDisplayInfo = ComponentDataGetDisplayInfo;
  function IComponentData.CompareObjects = ComponentDataCompareObjects;

  function IExtendContextMenu.Command = ExtendContextMenuCommand;
  function IExtendContextMenu.AddMenuItems = ExtendContextMenuAddMenuItems;

  function IExtendPropertySheet.CreatePropertyPages = ExtendPropertySheetCreatePropertyPages;
  function IExtendPropertySheet.QueryPagesFor = ExtendPropertySheetQueryPagesFor;

  function ComponentDataInitialize(const pUnknown: IUnknown): HResult; stdcall;
  function ComponentDataCreateComponent(out ppComponent: IComponent): HResult; stdcall;
  function ComponentDataNotify(const lpDataObject: IDataObject; event: _MMC_NOTIFY_TYPE; arg: Integer;
                  param: Integer): HResult; stdcall;
  function ComponentDataDestroy: HResult; stdcall;
  function ComponentDataQueryDataObject(cookie: Integer; _type: _DATA_OBJECT_TYPES;
                           out ppDataObject: IDataObject): HResult; stdcall;
  function ComponentDataGetDisplayInfo(var pScopeDataItem: _SCOPEDATAITEM): HResult; stdcall;
  function ComponentDataCompareObjects(const lpDataObjectA: IDataObject; const lpDataObjectB: IDataObject): HResult; stdcall;

  function ExtendContextMenuCommand (nCommandID : Integer; const dataObject : IDataObject) : HRESULT; stdcall;
  function ExtendContextMenuAddMenuItems (const dataObject : IDataObject;  const piCallback: IContextMenuCallback; var pInsertionAllowed: Integer) : HRESULT; stdcall;

  function ExtendPropertySheetCreatePropertyPages(const lpProvider: IPropertySheetCallback; handle: Integer;
                                 const lpIDataObject: IDataObject): HResult; stdcall;
  function ExtendPropertySheetQueryPagesFor(const lpDataObject: IDataObject): HResult; stdcall;

  function GetSnapinData : TSnapinData; virtual; abstract;

  procedure EnumerateScopePane (dataObject : IDataObject; pParent : HSCOPEITEM);
public
  property SnapinData : TSnapinData read fSnapinData;
  procedure Initialize; override;
  property Initialized : boolean read FInitialized;
  procedure SetScopeImages;
  property WindowHandle : HWND read GetWindowHandle;
  procedure Update (Item : TObject);
  procedure UpdateMenuItem (item : TMenuItem);

  property ActiveView : Integer read fActiveComponent;
  function SelectedScopeItem (viewIdx : Integer = -1) : TScopeItem;
  function SelectedResultItem (viewIdx : Integer = -1) : TResultItem;
  function FocusedResultItem (viewIdx : Integer = -1) : TResultItem;
  procedure SelectItem (item : TObject; state : boolean; viewIdx : Integer = -1);
  procedure FocusItem (item : TObject; state : boolean; viewIdx : Integer = -1);
  procedure ExpandItem (item : TObject; expand : boolean; viewIdx : Integer = -1);
  function ResultItemState (item : TResultItem; viewIdx : Integer = -1) : SYSUINT;
end;

(*--------------------------------------------------------------------------*
 | class TSnapinAbout                                                       |
 *--------------------------------------------------------------------------*)
TSnapinAbout = class (TComObject, ISnapinAbout)
private
  fSnapinData : TSnapinData;
  procedure LoadSnapinData;
protected
  function GetSnapinDescription(out lpDescription: PWideChar): HResult; stdcall;
  function GetProvider(out lpName : PWideChar): HResult; stdcall;
  function GetSnapinVersion(out lpVersion: PWideChar): HResult; stdcall;
  function GetSnapinImage(out hAppIcon: wireHICON): HResult; stdcall;
  function GetStaticFolderImage(out hSmallImage: wireHBITMAP; out hSmallImageOpen: wireHBITMAP;
                                out hLargeImage: wireHBITMAP; out cMask: UINT): HResult; stdcall;

  function GetSnapinData : TSnapinData; virtual; abstract;
public

end;

(*--------------------------------------------------------------------------*
 | class TSnapinComponent                                                   |
 *--------------------------------------------------------------------------*)
TSnapinComponent = class (TInterfacedObject, IComponent, IExtendContextMenu, IExtendControlBar, IResultDataCompare, IExtendPropertySheet, IResultOwnerData)
private
  fConsole : IConsole;
  fConsole2 : IConsole2;
  fHeader : IHeaderCtrl;
  fResultData : IResultData;
  fParent : TSnapinComponentData;
  fConsoleVerb : IConsoleVerb;
  fResultImages : IImageList;
  fDeletingItem : boolean;
  fSelectedScopeItem : TScopeItem;
  fInShowing : boolean;
  fControlBar : IControlBar;
  fToolbar : IToolbar;
  fMenuButton : IMenuButton;

  fCurrentButtonsScope : TScopeItem;
  fCurrentMenuButtonsScope : TScopeItem;
  fOwnerResultItem : TResultItem;
  fIOCX : IUnknown;

  procedure SetResultImages (useScopeImages : boolean);
  procedure InitializeHeaders (node : TScopeItem);
  procedure EnumerateResultPane (node : TScopeItem);
  procedure HandleStandardVerbs (bDeselectAll : boolean; arg : LongInt; scopeItem : TScopeItem; resultItem : TResultItem);
  procedure DecodeDataObject (dataObject : IDataObject; var scopeItem : TScopeItem; var resultItem : TResultItem);
  function GetScopeItemFromDataObject (dataObject : IDataObject) : TScopeItem;
  procedure UpdateResultItemSettings (viewIdx : Integer; item : TResultItem);
  procedure SetResultItemState (viewIdx : Integer; item : TResultItem; state : SYSUINT; enable : boolean);
  function GetResultItemState (viewIdx : Integer; item : TResultItem) : SYSUINT;
  function SelectedResultItem : TResultItem;
  function FocusedResultItem : TResultItem;
  procedure UpdateScopePane (item : TScopeItem);

protected
  function IComponent.Initialize = ComponentInitialize;
  function IComponent.Notify = ComponentNotify;
  function IComponent.Destroy = ComponentDestroy;
  function IComponent.QueryDataObject = ComponentQueryDataObject;
  function IComponent.GetResultViewType = ComponentGetResultViewType;
  function IComponent.GetDisplayInfo = ComponentGetDisplayInfo;
  function IComponent.CompareObjects = ComponentCompareObjects;

  function IExtendContextMenu.Command = ExtendContextMenuCommand;
  function IExtendContextMenu.AddMenuItems = ExtendContextMenuAddMenuItems;

  function IExtendControlbar.SetControlbar = ExtendControlbarSetControlbar;
  function IExtendControlbar.ControlbarNotify = ExtendControlbarControlbarNotify;

  function IResultDataCompare.Compare = ResultDataCompareCompare;

  function IExtendPropertySheet.CreatePropertyPages = ExtendPropertySheetCreatePropertyPages;
  function IExtendPropertySheet.QueryPagesFor = ExtendPropertySheetQueryPagesFor;

  function IResultOwnerData.FindItem = ResultOwnerDataFindItem;
  function IResultOwnerData.CacheHint = ResultOwnerDataCacheHint;
  function IResultOwnerData.SortItems = ResultOwnerDataSortItems;

  function ComponentInitialize(const lpConsole: IConsole): HResult; stdcall;
  function ComponentNotify(const lpDataObject: IDataObject; event: _MMC_NOTIFY_TYPE; arg: Integer;
                  param: Integer): HResult; stdcall;
  function ComponentDestroy(cookie: Integer): HResult; stdcall;
  function ComponentQueryDataObject(cookie: Integer; _type: _DATA_OBJECT_TYPES;
                           out ppDataObject: IDataObject): HResult; stdcall;
  function ComponentGetResultViewType(cookie: Integer; out ppViewType: PWideChar; out pViewOptions: Integer): HResult; stdcall;
  function ComponentGetDisplayInfo(var pResultDataItem: _RESULTDATAITEM): HResult; stdcall;
  function ComponentCompareObjects(const lpDataObjectA: IDataObject; const lpDataObjectB: IDataObject): HResult; stdcall;

  function ExtendContextMenuCommand (nCommandID : Integer; const dataObject : IDataObject) : HRESULT; stdcall;
  function ExtendContextMenuAddMenuItems (const dataObject : IDataObject;  const piCallback: IContextMenuCallback; var pInsertionAllowed: Integer) : HRESULT; stdcall;

  // IExtendControlbar
  function ExtendControlbarSetControlbar(const pControlbar: IControlbar): HResult; stdcall;
  function ExtendControlbarControlbarNotify(event: _MMC_NOTIFY_TYPE; arg, param: Integer): HResult; stdcall;

  // IResultDataCompare
  function ResultDataCompareCompare(lUserParam, cookieA, cookieB: Integer; var pnResult: SYSINT): HResult; stdcall;

  // IExtendPropertySheet
  function ExtendPropertySheetCreatePropertyPages(const lpProvider: IPropertySheetCallback; handle: Integer;
                                 const lpIDataObject: IDataObject): HResult; stdcall;
  function ExtendPropertySheetQueryPagesFor(const lpDataObject: IDataObject): HResult; stdcall;

  // IResultOwnerData;
  function ResultOwnerDataFindItem(var pFindInfo: _RESULTFINDINFO; out pnFoundIndex: SYSINT): HResult; stdcall;
  function ResultOwnerDataCacheHint(nStartIndex: SYSINT; nEndIndex: SYSINT): HResult; stdcall;
  function ResultOwnerDataSortItems(nColumn: SYSINT; dwSortOptions: UINT; lUserParam: Integer): HResult; stdcall;
end;

TDataObject = class (TInterfacedObject, IDataObject)
private
  fInternal : TInternal;
    function CreateCoClassID(var medium: TStgMedium): HRESULT;
    function CreateData(buffer: pointer; len: Integer;
      var medium: TStgMedium): HRESULT;
    function CreateDisplayName(var medium: TStgMedium): HRESULT;
    function CreateInternal(var medium: TstgMedium): HRESULT;
    function CreateNodeTypeData(var medium: TStgMedium): HRESULT;
    function CreateszNodeType(var medium: TstgMedium): HRESULT;
protected
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium):
      HResult; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HResult;
      stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc;
      out formatetcOut: TFormatEtc): HResult; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
    function EnumFormatEtc(dwDirection: Longint; out enumFormatEtc:
      IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: Longint;
      const advSink: IAdviseSink; out dwConnection: Longint): HResult; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
      stdcall;
end;

TComponentDataClass = class of TSnapinComponentData;
TSnapinAboutClass = class of TSnapinAbout;

function MMCPropertyChangeNotify (handle : LongInt; param : LParam) : HRESULT; stdcall;

implementation

uses Registry;

const
                                          // IDataObject formats for snapin data
  s_cfDisplayName    : Integer = 0;
  s_cfNodeType       : Integer = 0;
  s_cfCoClass        : Integer = 0;
  s_cfInternal       : Integer = 0;
  s_cfMultiSel       : Integer = 0;
  s_cfszNodeType     : Integer = 0;

  cNodeTypeStatic       : TGuid = '';   // Odd buggers these.  MMC requires a unique GUID for
  cObjectTypeResultItem : TGuid = '';   // node types, but it claims they've got nothing to do
  cNodeTypeScope        : TGuid = '';   // with COM.  Maybe extension snapins use these somehow.

  /////////////////////////////////////////////////////////////////////////////
  // These should be in MMC_TLB, but the import did clever COM stuff - so
  // they're not...

                                          // MMC clipboard (IDataObject) formats
  CCF_SNAPIN_INTERNAL              = 'SNAPIN_INTERNAL';
  CCF_NODETYPE                     = 'CCF_NODETYPE';
  CCF_NODEID                       = 'CCF_NODEID';
  CCF_SZNODETYPE                   = 'CCF_SZNODETYPE';
  CCF_DISPLAY_NAME                 = 'CCF_DISPLAY_NAME';
  CCF_SNAPIN_CLASSID               = 'CCF_SNAPIN_CLASSID';
  CCF_MMC_MULTISELECT_DATAOBJECT   = 'CCF_MMC_MULTISELECT_DATAOBJECT';
  CCF_MULTI_SELECT_SNAPINS         = 'CCF_MULTI_SELECT_SNAPINS';
  CCF_OBJECT_TYPES_IN_MULTI_SELECT = 'CCF_OBJECT_TYPES_IN_MULTI_SELECT';

                                          // MMC View enums
  MMC_VIEW_OPTIONS_NONE          = 0;
  MMC_VIEW_OPTIONS_NOLISTVIEWS   = $1;
  MMC_VIEW_OPTIONS_MULTISELECT   = $2;
  MMC_VIEW_OPTIONS_OWNERDATALIST = $4;
  MMC_VIEW_OPTIONS_FILTERED      = $8;
  MMC_VIEW_OPTIONS_CREATENEW     = $10;

                                         //  MMC_CONSOLE_VERB enum
  MMC_VERB_NONE       = 0;
  MMC_VERB_OPEN       = $8000;
  MMC_VERB_COPY       = $8001;
  MMC_VERB_PASTE      = $8002;
  MMC_VERB_DELETE     = $8003;
  MMC_VERB_PROPERTIES = $8004;
  MMC_VERB_RENAME     = $8005;
  MMC_VERB_REFRESH    = $8006;
  MMC_VERB_PRINT      = $8007;

                                           // MMC_NOTIFY_TYPE enum
  MMCN_ACTIVATE        = $8001;
  MMCN_ADD_IMAGES      = $8002;
  MMCN_BTN_CLICK       = $8003;
  MMCN_CLICK           = $8004;
  MMCN_COLUMN_CLICK    = $8005;
  MMCN_CONTEXTMENU     = $8006;
  MMCN_CUTORMOVE       = $8007;
  MMCN_DBLCLICK        = $8008;
  MMCN_DELETE          = $8009;
  MMCN_DESELECT_ALL    = $800a;
  MMCN_EXPAND          = $800b;
  MMCN_HELP            = $800c;
  MMCN_MENU_BTNCLICK   = $800d;
  MMCN_MINIMIZED       = $800e;
  MMCN_PASTE           = $800f;
  MMCN_PROPERTY_CHANGE = $8010;
  MMCN_QUERY_PASTE     = $8011;
  MMCN_REFRESH         = $8012;
  MMCN_REMOVE_CHILDREN = $8013;
  MMCN_RENAME          = $8014;
  MMCN_SELECT          = $8015;
  MMCN_SHOW            = $8016;
  MMCN_VIEW_CHANGE     = $8017;
  MMCN_SNAPINHELP      = $8018;
  MMCN_CONTEXTHELP     = $8019;
  MMCN_INITOCX         = $801a;
  MMCN_FILTER_CHANGE   = $801B;
  MMCN_FILTERBTN_CLICK = $801C;   // NOT USED
  MMCN_RESTORE_VIEW    = $801D;
  MMCN_PRINT           = $801E;
  MMCN_PRELOAD         = $801F;
  MMCN_LISTPAD         = $8020;
  MMCN_EXPANDSYNC      = $8021;
                                           // SCOPEDATAITEM constants
  SDI_STR        = $2;
  SDI_IMAGE      = $4;
  SDI_OPENIMAGE  = $8;
  SDI_STATE      = $10;
  SDI_PARAM      = $20;
  SDI_CHILDREN   = $40;
  SDI_PARENT     = 0;
  SDI_PREVIOUS   = $10000000;
  SDI_NEXT       = $20000000;
  SDI_FIRST      = $8000000;
  MMC_CALLBACK = PWideChar (-1);

  RDI_STR    = $2;                          // RESULTDATAITEM constants
  RDI_IMAGE  = $4;
  RDI_STATE  = $8;
  RDI_PARAM  = $10;
  RDI_INDEX  = $20;
  RDI_INDENT = $40;

type
  PMENUBUTTONDATA = ^_MENUBUTTONDATA;

const
  IID_IPropertySheetChange : TGuid = '{D700DD8E-2646-11D0-A2A7-00C04FD909DD}';
  CLSID_NodeManager        : TGuid = '{43136EB5-D36C-11CF-ADBC-00AA00A80033}';

type
  IPropertySheetChange = interface(IUnknown)
    ['{D700DD8E-2646-11D0-A2A7-00C04FD909DD}']
    function Notify (handle : LongInt; param : LParam) : HRESULT; stdcall;
  end;

(*--------------------------------------------------------------------------*
 | function MMCPropertyChangeNotify                                         |
 |                                                                          |
 | Microsoft up to their old tricks here.  They implement this function in  |
 | MMC.LIB - which of course we can't get at through Delphi.  It uses the   |
 | completely undocumented interface IPropertySheetChange.  In MMC 1.2 this |
 | name has been changed to IPropertySheetNotify - but the IID remains the  |
 | it still works the same way.                                             |
 |                                                                          |
 | From "How To Break Every Rule In The Book. Vol 17"                       |
 |      "Copyright (c) Microsoft Corps 1998-2000"                           |
 |                                                                          |
 | I got this code from disassembling MMC.LIB                               |
 *--------------------------------------------------------------------------*)
function MMCPropertyChangeNotify (handle : LongInt; param : LParam) : HRESULT; stdcall;
var
  hr : HRESULT;
  i : IPropertySheetChange;
begin
  hr := OleInitialize (Nil);
  if SUCCEEDED (hr) then
  begin
    CoCreateInstance (CLSID_NodeManager, Nil, CLSCTX_INPROC_SERVER or CLSCTX_INPROC_HANDLER, IID_IPropertySheetChange, i);
    i.Notify (handle, param);
    i := Nil;

    OleUninitialize;
  end;

  result := hr
end;

(*--------------------------------------------------------------------------*
 | function MMCFreeNotifyHandle                                             |
 |                                                                          |
 | Another trick MMC.LIB function                                           |
 |                                                                          |
 | I got this code from disassembling MMC.LIB.                              |
 *--------------------------------------------------------------------------*)
function MMCFreeNotifyHandle (handle : LongInt) : HRESULT;
begin
  if handle = 0 then
    result := E_INVALIDARG
  else
    if IsBadReadPtr (pointer (handle), $10) then
      result := E_FAIL
    else
    begin
      GlobalFree (handle);
      result := S_OK
    end
end;

(*--------------------------------------------------------------------------*
 | function HandleException : HResult                                       |
 |                                                                          |
 | Convert exceptions into HResult.  No snapin functions should raise       |
 | exceptions.                                                              |
 *--------------------------------------------------------------------------*)
function HandleException: HResult;
var
  E: TObject;
begin
  E := ExceptObject;
  if (E is EOleSysError) and (EOleSysError(E).ErrorCode < 0) then
    Result := EOleSysError(E).ErrorCode else
    Result := E_UNEXPECTED;
end;

(*--------------------------------------------------------------------------*
 | function IS_SPECIAL_DATAOBJECT ()                                        |
 |                                                                          |
 | The IS_SPECIAL_DATAOBJECT macro determines whether an LPDATAOBJECT       |
 | passed by MMC in a call to the snap-in's Notify method is a special type |
 | of data object instead of a pointer to an actual IDataObject object.     |
 |                                                                          |
 | Parameters:                                                              |
 |   d : IDataObject               The data object to test                  |
 *--------------------------------------------------------------------------*)
function IS_SPECIAL_DATAOBJECT (d : IDataObject) : BOOL;
begin
  result := (Integer (d) >= -10) and (Integer (d) <= 0)
end;

(*--------------------------------------------------------------------------*
 | function ExtractFromDataObject () : HRESULT                              |
 |                                                                          |
 | Extract data from a data object in the required format                   |
 |                                                                          |
 | Parameters:                                                              |
 |   dataObject : IDataObject             The data object to extract from   |
 |   cf : UINT                            The required clipboard format.    |
 |   cb : UINT                            Number of bytes to extract.       |
 |   var hglob : HGLOBAL                  Returns the data chunk.  This     |
 |                                        must be free'd with GlobalFree.   |
 |                                                                          |
 | The function returns an OLE success code.                                |
 *--------------------------------------------------------------------------*)
function ExtractFromDataObject (dataObject : IDataObject; cf : UINT; cb : LongInt; var hglob : HGLOBAL) : HRESULT;
var
  stgmedium : TStgMedium;
  formatetc : TFormatETC;
begin
  try
    result := S_FALSE;
    try
      stgmedium.tymed := TYMED_HGLOBAL;
      stgmedium.hBitmap := 0;

      formatetc.cfFormat := cf;
      formatetc.ptd := Nil;
      formatetc.dwAspect := DVASPECT_CONTENT;
      formatetc.lindex := -1;
      formatetc.tymed := TYMED_HGLOBAL;

      hglob := 0;

      stgmedium.hGlobal := GlobalAlloc (GMEM_SHARE, cb);
      if stgmedium.hGlobal = 0 then
        result := E_OUTOFMEMORY
      else
      begin
        result := dataObject.GetDataHere (formatetc, stgmedium);
        if result = S_OK then
        begin
          hglob := stgmedium.hGlobal;
          stgmedium.hGlobal := 0
        end
      end;

    finally
      if (result <> S_OK) and (stgmedium.hGlobal <> 0) then
        GlobalFree (stgmedium.hGlobal)
    end

  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function ExtractInternalFormat () : PInternal;                           |
 |                                                                          |
 | Extract internal format data from the data object.  The internal format  |
 | data can contain a scope item or a result item.                          |
 |                                                                          |
 | The pointer returned must be free'd with GlobalFree                      |
 |                                                                          |
 | Parameters:                                                              |
 |   dataObject : IDataObject      The data object to extract from.         |
 |                                                                          |
 | The function returns the internal format data.                           |
 *--------------------------------------------------------------------------*)
function ExtractInternalFormat (dataObject : IDataObject) : PInternal;
var
  data : HGLOBAL;
  rc : HRESULT;
begin
  rc := ExtractFromDataObject (dataObject, s_cfInternal, sizeof (TInternal), data);
  if rc = S_OK then
    result := PInternal (data)
  else
    result := Nil
end;

(*--------------------------------------------------------------------------*
 | function ImageListToBitmap () : HBITMAP;                                 |
 |                                                                          |
 | Convert an image list to a bitmap strip of images, each one 'size'       |
 | pixels wide and high.                                                    |
 |                                                                          |
 | Note that imageList.GetImageBitmap doesn't appear to work...             |
 |                                                                          |
 | Note that the bitmap returned must be free'd with DeleteObject           |
 |                                                                          |
 | Parameters:                                                              |
 |   imageList : TImageList       The image list to convert                 |
 |   size : Integer               The size of each image in the strip.      |
 |                                                                          |
 | The function returns the strip bitmap                                    |
 *--------------------------------------------------------------------------*)
function ImageListToBitmap (imageList : TImageList; size : Integer; var mask : UINT) : HBITMAP;
var
  bitmap, bmp : TBitmap;
  i : Integer;
  r : TRect;

const
  n : Integer = 0;

begin
  bmp := Nil;
  bitmap := TBitmap.Create;
  mask := RGB ($12, $34, $56);
  try
    bmp := TBitmap.Create;
    bmp.Width := size;
    bmp.Height := size;
                                           // Create a bitmap to hold all the
                                           // images.
    bitmap.Width := size * imageList.Count;
    bitmap.Height := size;

    for i := 0 to imageList.count - 1 do   // Add each image to the bitmap
    begin
      bmp.Canvas.FillRect (rect (0, 0, bmp.width, bmp.height));
      imageList.GetBitmap (i, bmp);
      r.Left := i * size;
      r.Right := r.Left + size;
      r.Top := 0;
      r.Bottom := size;
      bitmap.Canvas.StretchDraw (r, bmp);
      mask := ColorToRGB (bitmap.Canvas.Pixels [0, bmp.height - 1]);
    end;

    result := bitmap.ReleaseHandle;

  finally
    bitmap.Free;
    bmp.Free
  end
end;

(*--------------------------------------------------------------------------*
 | procedure SetImageList ()                                                |
 |                                                                          |
 | Set an IImageList's bitmaps to contain the images from two image lists.  |
 | IImageList insists that it's bitmaps are 16x16 and 32x32, so we have to  |
 | enforce that, converting images to the correct size if possible.         |
 |                                                                          |
 | Parameters:                                                              |
 |   imageList : IImageList       The image list to fill                    |
 |   smallImageList,                                                        |
 |   largeImageLis : TImageList   The image lists to get the images from.   |
 *--------------------------------------------------------------------------*)
procedure SetImageList (imageList : IImageList; smallImageList, largeImageList : TImageList);
var
  smallImages, largeImages : HBITMAP;
  mask : UINT;
begin
  if Assigned (smallImageList) or Assigned (largeImageList) then
  begin
    smallImages := 0;
    largeImages := 0;
    try
      if Assigned (smallImageList) then
        smallImages := ImageListToBitmap (smallImageList, 16, mask)
      else
        smallImages := ImageListToBitmap (largeImageList, 16, mask);

      if Assigned (largeImageList) then
        largeImages := ImageListToBitmap (largeImageList, 32, mask)
      else
        largeImages := ImageListToBitmap (smallImageList, 32, mask);

      OleCheck (imageList.ImageListSetStrip (smallImages, largeImages, 0, mask));
    finally
      if (smallImages <> 0) then DeleteObject (smallImages);
      if (largeImages <> 0) then DeleteObject (largeImages);
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure GetContextMenuFlags                                        |
 |                                                                      |
 | Get the flags and special flags that MMC requires from a TMenuItem   |
 | instance.                                                            |
 |                                                                      |
 | Parameters:                                                          |
 |   itm : TMenuItem            The TMenuItem instance                  |
 |   var flags : Integer        Returns the MMC menu flags              |
 |   var specialFlags : Integer Returns the MMC menu special flags      |
 *----------------------------------------------------------------------*)
procedure GetContextMenuFlags (itm : TMenuItem; var flags, specialFlags : Integer);
begin
  flags := 0;
  specialFlags := 0;

  if itm.Enabled then
    flags := flags or MF_ENABLED
  else
    flags := flags or MF_DISABLED or MF_GRAYED;

  if itm.Checked then
    flags := flags or MF_CHECKED
  else
    flags := flags or MF_UNCHECKED;

  if itm.Default then
    specialFlags := specialFlags or CCM_SPECIAL_DEFAULT_ITEM;
end;

{ TSnapinComponentData }

(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.ComponentDataCompareObjects ()             |
 |                                                                          |
 | Parameters:                                                              |
 |   lpDataObjectA,                                                         |
 |   lpDataObjectB  : IDataObject           The objects to compare.         |
 |                                                                          |
 | The function returns S_OK if the objects match, or S_FALSE if they don't |
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.ComponentDataCompareObjects(const lpDataObjectA,
  lpDataObjectB: IDataObject): HResult;
var
  objA, objB : PInternal;
begin
  try
    result := S_FALSE;

    objA := ExtractInternalFormat (lpDataObjectA);
    if Assigned (objA) then
    try
      objB := ExtractInternalFormat (lpDataObjectB);

      if Assigned (objB) then
      try
        if (ObjA^.m_type = ObjB^.m_type) and (ObjA^.m_Cookie = ObjB^.m_Cookie) then
          result := S_OK;
      finally
        GlobalFree (THandle (ObjB))
      end
    finally
      GlobalFree (THandle (ObjA))
    end
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.ComponentDataCreateComponent ()            |
 |                                                                          |
 | Create a 'result' pane component for the component data.                 |
 |                                                                          |
 | Parameters:                                                              |
 |   out ppComponent : IComponent       The created component.              |
 |                                                                          |
 | The function returns an OLE success code                                 |
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.ComponentDataCreateComponent(
  out ppComponent: IComponent): HResult;
var
  Component : TSnapinComponent;
begin
  try
    Component := TSnapinComponent.Create;
    Component.fParent := self;
    fSnapinComponents.Add (Component);
    ppComponent := Component;
    result := S_OK;
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.ComponentDataDestroy.                      |
 |                                                                          |
 | Destroy the component data.  Release the interfaces obtained in          |
 | Initialize.                                                              |
 |                                                                          |
 | The function returns an OLE success code                                 |
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.ComponentDataDestroy: HResult;
begin
  try
    fScopeImageList := Nil;
    fConsole := Nil;
    fConsole2 := Nil;
    fConsoleNameSpace := Nil;
    fSnapinComponents.Free;
    result := S_OK;
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.ComponentDataGetDisplayInfo                |
 |                                                                          |
 | Return display information - The only thing used is the 'text' - which   |
 | appears in the scope pane tree view root for the scope data item.        |
 |                                                                          |
 | Parameters:                                                              |
 |   var pScopeDataItem : _SCOPEDATAITEM  On entry, lParam contains the     |
 |                        scope node, and 'mask' contains flags for the     |
 |                        in formation required.                            |
 |                                                                          |
 | The function returns an OLE success code                                 |
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.ComponentDataGetDisplayInfo(
  var pScopeDataItem: _SCOPEDATAITEM): HResult;
var
  node : TScopeItem;
begin
  try
    node := TScopeItem (pScopeDataItem.lParam);
    if (pScopeDataItem.mask and SDI_STR) <> 0 then

                                // (Probably!) don't use StringToLPOleStr here
      pScopeDataItem.displayName := PWideChar (WideString (node.Text));

    result := S_OK
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.ComponentDataInitialize                    |
 |                                                                          |
 | Initialize the component data by obtaining the IConsole and associated   |
 | interfaces.                                                              |
 |                                                                          |
 | Parameters:                                                              |
 |   pUnknown : IUnknown          Unknown interface that must inherit from  |
 |                                IConsole.                                 |
 |                                                                          |
 | The function returns an OLE success code                                 |
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.ComponentDataInitialize(
  const pUnknown: IUnknown): HResult;
begin
  FIsDirty := True;
  try
    fConsole := pUnknown as IConsole;
    try
      fConsole2 := fConsole as IConsole2;
    except
      fConsole2 := Nil
    end;
    fConsoleNameSpace := pUnknown as IConsoleNameSpace;
    fConsole.QueryScopeImageList (fScopeImageList);
    fSnapinComponents := TObjectList.Create;
    fSnapinComponents.OwnsObjects := False;

    FInitialized := True;
    SetScopeImages;                     // Set the scope pane images.


    result := S_OK;
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.ComponentDataNotify                        |
 |                                                                          |
 | Handle ComponentData (Scope Pane) notifications.                         |
 |                                                                          |
 | Parameters:                                                              |
 |   lpDataObject : IDataObject         Either a 'special data object' or   |
 |                                      a scope item object.                |
 |   event         : _MMC_NOTIFY_TYPE   The nofication                      |
 |   arg           : Integer            Notification  argument.             |
 |   param         : Integer            Additional notifcation parameter.   |
 |                                                                          |
 | The function returns an OLE success code                                 |
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.ComponentDataNotify(
  const lpDataObject: IDataObject; event: _MMC_NOTIFY_TYPE; arg,
  param: Integer): HResult;
var
  scopeItem : TScopeItem;
  obj : TObject;
  allow : boolean;
  ownerDataIdx : Integer;

begin
  try
    result := S_OK;

    if event = MMCN_PROPERTY_CHANGE then
      obj := TObject (param)
    else
      obj := GetNodeForDataObject (lpDataObject, ownerDataIdx);

    if obj is TScopeItem then
      scopeItem := TScopeItem (obj)
    else
      scopeItem := Nil;

    if Assigned (scopeItem) then
    case event of
      MMCN_REMOVE_CHILDREN : ; // TODO - Does this need implementing ??
      MMCN_BTN_CLICK       : ; // TODO - Does this need implementing ??
      MMCN_DELETE          : ; // TODO - Does this need implementing ??
//        MMCN_PRELOAD         : ; // TODO - Does this need implementing ??
      MMCN_RENAME          : ; // TODO - Does this need implementing ??
      MMCN_EXPANDSYNC      : ; // TODO - Does this need implementing ??
      MMCN_EXPAND          :

        begin              // The most important notification!
                           // Enumerate the scope pane to return the child nodes
            allow := True;
            fExpanding := True;
            scopeItem.ItemID := Param;        // This also gets set in 'InsertItem' ** Except for the root scope !!
            try
              if Assigned (scopeItem.SnapinData.OnScopeExpand) then
                scopeItem.SnapinData.OnScopeExpand (scopeItem, BOOL (arg), allow);
              if (BOOL (arg) = TRUE) and allow then
                EnumerateScopePane (lpDataObject, param)
            finally
              fExpanding := False
            end
        end;

      MMCN_PROPERTY_CHANGE :
        Update (scopeItem);
      else
        MessageBeep ($ffff);   // We should never get here, unless MSDN is lying....
    end
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.ComponentDataQueryDataObject               |
 |                                                                          |
 | Create a data object for this ComponentDataObject.                       |
 |                                                                          |
 | Parameters:                                                              |
 |   cookie : Integer           The cookie to associate with the data       |
 |   _type  : DATA_OBJECT_TYPE  Whether it's a scope or result data object  |
 |   out ppDataObject : IDataObject    Returns the new data object.         |
 |                                                                          |
 | The function returns an OLE success code                                 |
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.ComponentDataQueryDataObject(cookie: Integer;
  _type: _DATA_OBJECT_TYPES; out ppDataObject: IDataObject): HResult;
var
  dataObject : TDataObject;
begin
  try
    dataObject := TDataObject.Create;
    dataObject.fInternal.m_type := _type;
    dataObject.fInternal.m_cookie := cookie;
    dataObject.fInternal.m_clsid := Factory.ClassID;
    dataObject.fInternal.m_object := self;          // fInternal points to 'self'
                                                    // so that we can retrieve
                                                    // ourself from data objects
    ppDataObject := dataObject as IDataObject;
    result := S_OK;
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | procedure TSnapinComponentData.EnumerateScopePane                        |
 |                                                                          |
 | Called in response to a MMCN_EXPAND notification                         |
 |                                                                          |
 | Parameters:                                                              |
 |   dataObject : IDataObject.  Contains the scope item to enumerate        |
 |   pParent    : HSCOPEITEM    The scope item's parent node handle.        |
 |                                                                          |
 *--------------------------------------------------------------------------*)

procedure TSnapinComponentData.EnumerateScopePane(dataObject: IDataObject; pParent: HSCOPEITEM);
var
  i : Integer;
  item : _SCOPEDATAITEM;
  hr : HRESULT;
  root, node : TScopeItem;
  ownerDataIdx : Integer;

begin
  root := GetNodeForDataObject (dataObject, ownerDataIdx) as TScopeItem;

  if Assigned (root) then
  begin
                         // Insert the scope items.

    for i := 0 to root.ScopeItems.Count - 1 do
    begin
      node := root.ScopeItems [i];
      FillChar (item, sizeof (item), 0);
      item.mask := SDI_STR or SDI_PARAM or SDI_PARENT or SDI_CHILDREN;

      if node.ImageIndex > -1 then      // Set the image
      begin
        item.mask := item.mask or SDI_IMAGE or SDI_OPENIMAGE;
        item.nImage := node.ImageIndex;
        item.nOpenImage := node.ImageIndex;
      end;

      item.cChildren := node.ScopeItems.Count;
      if (item.cChildren = 0) and (node.HasChildren) then
        item.cChildren := 1;
      item.relativeID := pParent;
      item.displayname := MMC_CALLBACK;
      item.lParam := Integer (node);
      hr := FConsoleNameSpace.InsertItem (item);
      node.itemID := item.ID;
      OleCheck (hr);
    end
  end
end;


(*--------------------------------------------------------------------------*
 | procedure TSnapinComponentData.ExpandItem                                |
 |                                                                          |
 | Public method.  Expand a scope item.                                     |
 |                                                                          |
 | Parameters:                                                              |
 |   item : TObject                The scope item to expand                 |
 |   expand : boolean              Whether to expand or collapse            |
 |   viewIdx : Integer             Which result pane to use.  -1 for the    |
 |                                 current one.                             |
 *--------------------------------------------------------------------------*)
procedure TSnapinComponentData.ExpandItem(item: TObject; expand: boolean; viewIdx : Integer);
var
  cmp : TSnapinComponent;
begin
  if Item is TScopeItem then
  begin
    if viewIdx = -1 then
      viewIdx := fActiveComponent;

    if (viewIdx >= 0) and (viewIdx < fSnapinComponents.Count) then
    begin
      cmp := TSnapinComponent (fSnapinComponents [viewIdx]);
      cmp.fConsole2.Expand (TScopeItem (item).itemID, Integer (BOOL (expand)))
    end
  end
end;

(*----------------------------------------------------------------------*
 | procedure TSnapinComponentData.ExtendContextMenuAddMenuItems         |
 |                                                                      |
 | IExtendContextMenu.AddMenuItems.  Add context menu items             |
 *----------------------------------------------------------------------*)
function TSnapinComponentData.ExtendContextMenuAddMenuItems(
  const dataObject: IDataObject; const piCallback: IContextMenuCallback;
  var pInsertionAllowed: Integer): HRESULT;
var
  scopeItem : TScopeItem;
  i : Integer;
  item : _CONTEXTMENUITEM;
  itm : TMenuItem;
  ownerDataIdx : Integer;
begin
  try
    result := S_OK;
    scopeItem := GetNodeForDataObject (dataObject, ownerDataIdx) as TScopeItem;
    if Assigned (scopeItem) and Assigned (scopeItem.ContextMenu) then
    begin
                        // Get the menu item details from the scopeItem.ContextMenu
                        // that they set up at design time..

      for i := 0 to scopeItem.ContextMenu.Items.Count - 1 do
      begin
        FillChar (item, sizeof (item), 0);
        itm := scopeItem.ContextMenu.Items [i];
        item.strName := PWideChar (WideString (itm.Caption));
        item.strStatusBarText := PWideChar (WideString (itm.Hint));
        item.lCommandID := itm.Command;

        GetContextMenuFlags (itm, item.fFlags, item.fSpecialFlags);
        item.lInsertionPointID := Integer (CCM_INSERTIONPOINTID_PRIMARY_TOP);

        result := piCallback.AddItem (item);
        if not Succeeded (result) then
          break;
      end
    end
  except
    result := HandleException
  end
end;


(*----------------------------------------------------------------------*
 | function TSnapinComponentData.ExtendContextMenuCommand               |
 |                                                                      |
 | Handle a context menu command by dispatching it back to the scope    |
 | item, which will perform the Delphi action.                          |
 *----------------------------------------------------------------------*)
function TSnapinComponentData.ExtendContextMenuCommand(nCommandID: Integer;
  const dataObject: IDataObject): HRESULT;
var
  node : TScopeItem;
  ownerDataIdx : Integer;
begin
  try
    node := GetNodeForDataObject (dataObject, ownerDataIdx) as TScopeItem;
    if Assigned (node) and Assigned (node.ContextMenu) then
      node.ContextMenu.DispatchCommand (nCommandID);

    result := S_OK;
  except
    result := HandleException
  end
end;


(*----------------------------------------------------------------------*
 | function TSnapinComponentData.ExtendPropertySheetCreatePropertyPages |
 |                                                                      |
 | We should create property pages here, but we don't want to get into  |
 | that dialog template rubbish!  Call OnScopeProperties, so that we    |
 | can display a neat Delphi dialog instead - and return S_FALSE so     |
 | mmc doesn't pop-up the property sheet container dialog.              |
 *----------------------------------------------------------------------*)
function TSnapinComponentData.ExtendPropertySheetCreatePropertyPages(
  const lpProvider: IPropertySheetCallback; handle: Integer;
  const lpIDataObject: IDataObject): HResult;
var
  scope : TObject;
  x : Integer;
  changed : boolean;
begin
  result := S_FALSE;
  scope := GetNodeForDataObject (lpIDataObject, x);
  if scope is TScopeItem then
  begin
    changed := False;
    TScopeItem (scope).OnScopeProperties (scope, changed);

    if changed then
      MMCPropertyChangeNotify (handle, Integer (scope))
  end;

  MMCFreeNotifyHandle (handle);
end;

(*----------------------------------------------------------------------*
 | function TSnapinComponentData.ExtendPropertySheetQueryPagesFor       |
 |                                                                      |
 | Return S_OK if the scope item has an 'OnScopeProperties' handler     |
 | - otherwise return S_FALSE;                                          |
 *----------------------------------------------------------------------*)
function TSnapinComponentData.ExtendPropertySheetQueryPagesFor(
  const lpDataObject: IDataObject): HResult;
var
  scope : TObject;
  x : Integer;
begin
  result := S_FALSE;
  scope := GetNodeForDataObject (lpDataObject, x);
  if scope is TScopeItem then
    if Assigned (TScopeItem (scope).OnScopeProperties) then
      result := S_OK
end;

function TSnapinComponentData.FocusedResultItem(
  viewIdx: Integer): TResultItem;
begin
  if viewIdx = -1 then
    viewIdx := fActiveComponent;

  if (viewIdx >= 0) and (viewIdx < fSnapinComponents.Count) then
    result := TSnapinComponent (fSnapinComponents [viewIdx]).FocusedResultItem
  else
    result := Nil
end;

procedure TSnapinComponentData.FocusItem(item: TObject; state : boolean; viewIdx: Integer);
var
  i : TResultItem;
  cmp : TSnapinComponent;
begin
  if viewIdx = -1 then
    viewIdx := fActiveComponent;

  if Item is TScopeItem then
    fConsole.SelectScopeItem (TScopeItem (item).itemID)
  else
  begin
    i := Item as TResultItem;
    if (viewIdx >= 0) and (viewIdx < fSnapinComponents.Count) then
    begin
      cmp := TSnapinComponent (fSnapinComponents [viewIdx]);
      if cmp.fSelectedScopeItem = i.ScopeItem then
        cmp.SetResultItemState (viewIdx, i, LVIS_FOCUSED, state)
    end
  end
end;

function TSnapinComponentData.GetNodeForCookie(
  cookie: Integer): TObject;
begin
  if cookie = 0 then
    result := FSnapinData.ScopeItem
  else
    result := TObject (cookie);
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.GetNodeForDataObject(
  const dataObj: IDataObject; var ownerDataIdx : Integer): TObject;
var
  internal : PInternal;
begin
  Internal := ExtractInternalFormat (dataObj);
  result := Nil;
  if Assigned (Internal) then
  try
    if (Internal^.m_type = CCT_RESULT) and Assigned (SelectedScopeItem.OnOwnerData) then
    begin
      result := Nil;
      ownerDataIdx := Internal^.m_cookie
    end
    else
      result := GetNodeForCookie (Internal^.m_cookie);
  finally
    GlobalFree (Integer (internal))
  end
end;

function TSnapinComponentData.GetWindowHandle: HWND;
begin
  if Assigned (fConsole) then
    fConsole.GetMainWindow (wireHWND (result))
  else
    result := 0;
end;

procedure TSnapinComponentData.Initialize;

  function CreateGUID : string;
  var
    r : TGUID;
  begin
    CoCreateGUID (r);
    result := GUIDToString (r);
  end;

begin
  fSnapinData := GetSnapinData;
  fSnapinData.Parent := self;
  fActiveComponent := 0;
  s_cfInternal       := RegisterClipboardFormat (CCF_SNAPIN_INTERNAL);
  s_cfDisplayName    := RegisterClipboardFormat (CCF_DISPLAY_NAME);
  s_cfNodeType       := RegisterClipboardFormat (CCF_NODETYPE);
  s_cfszNodeType     := RegisterClipboardFormat (CCF_SZNODETYPE);
  s_cfCoClass        := RegisterClipboardFormat (CCF_SNAPIN_CLASSID);
  s_cfMultiSel       := RegisterClipboardFormat (CCF_OBJECT_TYPES_IN_MULTI_SELECT);

  cNodeTypeStatic       := StringToGUID (CreateGUID);
  cObjectTypeResultItem := StringToGUID (CreateGUID);
  cNodeTypeScope        := StringToGUID (CreateGUID);
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponentData.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponentData.ResultItemState(item: TResultItem; viewIDx : Integer): SYSUINT;
begin
  if viewIdx = -1 then
    viewIdx := fActiveComponent;

  if (viewIdx >= 0) and (viewIdx < fSnapinComponents.Count) then
    result := TSnapinComponent (fSnapinComponents [viewIdx]).GetResultItemState (viewIdx, item)
  else
    result := 0
end;

function TSnapinComponentData.SelectedResultItem(
  viewIdx: Integer): TResultItem;
begin
  if viewIdx = -1 then
    viewIdx := fActiveComponent;

  if (viewIdx >= 0) and (viewIdx < fSnapinComponents.Count) then
    result := TSnapinComponent (fSnapinComponents [viewIdx]).SelectedResultItem
  else
    result := Nil
end;

function TSnapinComponentData.SelectedScopeItem(
  viewIdx: Integer): TScopeItem;
begin
  if viewIdx = -1 then
    viewIdx := fActiveComponent;

  if (viewIdx >= 0) and (viewIdx < fSnapinComponents.Count) then
    result := TSnapinComponent (fSnapinComponents [viewIdx]).fSelectedScopeItem
  else
    result := Nil
end;

procedure TSnapinComponentData.SelectItem(item: TObject; state : boolean; viewIdx : Integer);
var
  i : TResultItem;
  cmp : TSnapinComponent;
begin
  if viewIdx = -1 then
    viewIdx := fActiveComponent;

  if Item is TScopeItem then
    fConsole.SelectScopeItem (TScopeItem (item).itemID)
  else
  begin
    i := Item as TResultItem;
    if (viewIdx >= 0) and (viewIdx < fSnapinComponents.Count) then
    begin
      cmp := TSnapinComponent (fSnapinComponents [viewIdx]);
      if cmp.fSelectedScopeItem = i.ScopeItem then
        cmp.SetResultItemState (viewIdx, i, LVIS_SELECTED, state)
    end
  end
end;

procedure TSnapinComponentData.SetScopeImages;
begin
  if Assigned (fSnapinData) and Assigned (fScopeImageList) and Initialized then
    SetImageList (fScopeImageList, fSnapinData.ScopeSmallImages, fSnapinData.ScopeLargeImages);
end;

procedure TSnapinComponentData.Update (Item : TObject);
var
  dataObject : IDataObject;
  i : Integer;
  ritem : TResultItem;
  cmp : TSnapinComponent;
begin
  if fExpanding then Exit;
  if Item is TScopeItem then
  begin
    UpdateScopeSettings (TScopeItem (item));
    ComponentDataQueryDataObject (Integer (Item), 0, dataObject);
    fConsole.UpdateAllViews  (dataObject, TScopeItem (item).ResultItems.Count, TScopeItem (item).ItemID)
  end
  else
  begin
    ritem := item as TResultItem;
    for i := 0 to fSnapinComponents.Count - 1 do
    begin
      cmp := TSnapinComponent (fSnapinComponents [i]);
      if cmp.fSelectedScopeItem = ritem.ScopeItem then
        cmp.UpdateResultItemSettings (i, ritem)
    end
  end
end;

procedure TSnapinComponentData.UpdateMenuItem(item: TMenuItem);
var
  component : TSnapinComponent;
begin
  if fActiveComponent <> -1 then
  begin
    component := TSnapinComponent (fSnapinComponents [fActiveComponent]);
    component.fMenuButton.SetButtonState (item.Command, ENABLED, Integer (BOOL (item.Enabled)));
    component.fMenuButton.SetButtonState (item.Command, HIDDEN, Integer (BOOL (not item.Visible)))
  end
end;

procedure TSnapinComponentData.UpdateScopeSettings(scope: TScopeItem);
var
  item : _SCOPEDATAITEM;
begin
  if scope.ItemID <> 0 then
  begin
    FillChar (item, sizeof (item), 0);
    item.mask := SDI_STR;
    if scope.ImageIndex > -1 then      // Set the image
    begin
      item.mask := item.mask or SDI_IMAGE or SDI_OPENIMAGE;
      item.nImage := scope.ImageIndex;
      item.nOpenImage := scope.ImageIndex;
    end;

    item.ID := scope.ItemID;
    item.displayname := MMC_CALLBACK;
    OleCheck (fConsoleNameSpace.SetItem (item))
  end
end;

{ TDataObject }

(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.CreateData(buffer: pointer; len: Integer;
  var medium: TStgMedium): HRESULT;
var
  stream : IStream;
  written : LongInt;
begin
  try
    if (not Assigned (buffer)) then
       result := E_POINTER
    else
    begin
      result := DV_E_TYMED;

      if medium.tymed = TYMED_HGLOBAL then
      begin
        result := CreateStreamOnHGlobal (medium.hGlobal, FALSE, stream);

        if result = S_OK then
        begin
          result := stream.Write (buffer, len, @written);
        end
      end
    end
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.CreateCoClassID (var medium : TStgMedium) : HRESULT;
begin
  result :=CreateData (@fInternal.m_clsid, sizeof (TGuid), medium)
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.CreateDisplayName(var medium: TStgMedium): HRESULT;
var
  szDispName : WideString;
  componentData : TSnapinComponentData;
begin
  componentData := TSnapinComponentData (fInternal.m_Object);
  szDispName := componentData.fSnapinData.ScopeItem.Text;
  result := CreateData (PWideChar (szDispName), (Length (szDispName) + 1) * sizeof (WideChar), medium);
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.CreateInternal (var medium : TstgMedium) : HRESULT;
begin
  result := CreateData (@fInternal, sizeof (fInternal), medium);
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.CreateNodeTypeData(var medium: TStgMedium): HRESULT;
var
  pcObjectType : PGuid;
begin
  result := S_OK;
  pcObjectType := Nil;
  if fInternal.m_cookie = 0 then
    pcObjectType := @cNodeTypeStatic
  else
    if fInternal.m_type = CCT_SCOPE then
      pcObjectType := @cNodeTypeScope
    else
      if fInternal.m_type = CCT_RESULT then
        pcObjectType := @cObjectTypeResultItem;

  if result = S_OK then
    result := CreateData (pcObjectType, sizeof (TGuid), medium);
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.DAdvise(const formatetc: TFormatEtc; advf: Integer;
  const advSink: IAdviseSink; out dwConnection: Integer): HResult;
begin
  result := E_NOTIMPL;
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.DUnadvise(dwConnection: Integer): HResult;
begin
  result := E_NOTIMPL;
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.EnumDAdvise(out enumAdvise: IEnumStatData): HResult;
begin
  result := E_NOTIMPL;
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.EnumFormatEtc(dwDirection: Integer;
  out enumFormatEtc: IEnumFormatEtc): HResult;
begin
  result := E_NOTIMPL;
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.GetCanonicalFormatEtc(const formatetc: TFormatEtc;
  out formatetcOut: TFormatEtc): HResult;
begin
  result := E_NOTIMPL;
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.GetData(const formatetcIn: TFormatEtc;
  out medium: TStgMedium): HResult;
begin
  result := E_NOTIMPL;
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.GetDataHere(const formatetc: TFormatEtc;
  out medium: TStgMedium): HResult;
var
  cf : word;
begin
  try
    result := DV_E_CLIPFORMAT;
    cf := formatetc.cfFormat;

    if cf = s_cfNodeType then
      result := CreateNodeTypeData (medium)
    else
      if cf = s_cfCoClass then
        result := CreateCoClassID (medium)
      else
        if cf = s_cfszNodeType then
          result := CreateszNodeType (medium)
        else
          if cf = s_cfDisplayName then
            result := CreateDisplayName (medium)
          else
            if cf = s_cfInternal then
              result := CreateInternal (medium)
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.QueryGetData(const formatetc: TFormatEtc): HResult;
begin
  result := E_NOTIMPL;
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.SetData(const formatetc: TFormatEtc;
  var medium: TStgMedium; fRelease: BOOL): HResult;
begin
  result := E_NOTIMPL;
end;


(*--------------------------------------------------------------------------*
 | function TDataObject.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TDataObject.CreateszNodeType(var medium: TstgMedium): HRESULT;
var
  wszNodeType : WideString;
  pcObjectType : PGUID;
begin
  try
    pcObjectType := Nil;
    if fInternal.m_cookie = 0 then
      pcObjectType := @cNodeTypeStatic
    else
      if fInternal.m_type = CCT_SCOPE then
        pcObjectType := @cNodeTypeStatic
      else
        if fInternal.m_type = CCT_RESULT then
          pcObjectType := @cObjectTypeResultItem;

    if Assigned (pcObjectType) then
    begin
      wszNodeType := GUIDToString (pcObjectType^);
      result := CreateData (PWideChar (wszNodeType), (Length (wszNodeType) + 1) * sizeof (WideChar), medium)
    end
    else
      result := E_FAIL
  except
    result := HandleException
  end
end;


{ TSnapinComponent }

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ComponentCompareObjects(const lpDataObjectA,
  lpDataObjectB: IDataObject): HResult;
var
  objA, objB : PInternal;
begin
  try
    result := S_FALSE;

    objA := ExtractInternalFormat (lpDataObjectA);
    if Assigned (objA) then
    try
      objB := ExtractInternalFormat (lpDataObjectB);

      if Assigned (objB) then
      try
        if (ObjA^.m_type = ObjB^.m_type) and (ObjA^.m_Cookie = ObjB^.m_Cookie) then
          result := S_OK;
      finally
        GlobalFree (THandle (ObjB))
      end
    finally
      GlobalFree (THandle (ObjA))
    end
  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ComponentDestroy(cookie: Integer): HResult;
begin
  try
    if Assigned (fControlBar) then
    begin
      if Assigned (fMenuButton) then
      begin
        fControlBar.Detach (fMenuButton);
        fMenuButton := Nil
      end;

      if Assigned (fToolBar) then
      begin
        fControlBar.Detach (fToolBar);
        fToolBar := Nil
      end
    end;

    fConsole2 := Nil;
    fResultData := Nil;
    fResultImages := Nil;
    fConsole := Nil;
    fHeader := Nil;
    fConsoleVerb := Nil;
    fControlBar := Nil;
    fToolbar := Nil;
    fMenuButton := Nil;
    fOwnerResultItem.Free;
    fIOCX := Nil;
    fParent.fSnapinComponents.Remove (self);
//    fCurrentScopeItem := Nil;
    result := S_OK;
  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ComponentGetDisplayInfo(
  var pResultDataItem: _RESULTDATAITEM): HResult;
var
  item : TScopeItem;
  resultItem : TResultItem;
  o : TObject;
begin
  try
    if pResultDataItem.bScopeItem <> 0 then
    begin
      item := TObject (pResultDataItem.lParam) as TScopeItem;

      with pResultDataItem do
      begin
        if (mask and RDI_STR) <> 0 then
          if nCol = 0 then

          // The docs define str as LPOLESTR - contrasts with SCOPEDATAITEM but
          // who are we to disagree ?

            str := StringToLPOleStr (item.Text)
          else
            if nCol <= Item.SubItems.Count then
              str := StringToLPOleStr (item.SubItems [nCol - 1])
            else
              str := '';

        nImage := item.ImageIndex
      end
    end
    else
    begin
      o := TObject (pResultDataItem.lParam);
      resultItem := o as TResultItem;
      if not Assigned (resultItem) then
        if Assigned (fSelectedScopeItem) and Assigned (fSelectedScopeItem.OnOwnerData) then
        begin
          if not Assigned (fOwnerResultItem) then
            fOwnerResultItem := TResultItem.Create (nil);

          resultItem := fOwnerResultItem;

          if (fSelectedScopeItem <> resultItem.ScopeItem) or (resultItem.ItemID [fParent.fActiveComponent] <> pResultDataItem.nIndex) then
          begin
            resultItem.ScopeItem := fSelectedScopeItem;
            resultItem.Text := '';
            resultItem.SubItems.Clear;
            resultItem.ImageIndex := -1;
            resultItem.ItemID [fParent.fActiveComponent] := pResultDataItem.nIndex;

            fSelectedScopeItem.OnOwnerData (fSelectedScopeItem, resultItem);
          end;

          with pResultDataItem do
          begin
            if (mask and RDI_STR) <> 0 then
              if nCol = 0 then
                str := StringToLPOleStr (resultItem.Text)
              else
                if nCol <= resultItem.SubItems.Count then
                  str := StringToLPOleStr (resultItem.SubItems [nCol - 1])
                else
                  str := '';

            if (mask and RDI_IMAGE) <> 0 then
              nImage := resultItem.ImageIndex;
          end
        end
        else
          with pResultDataItem do
          begin
            if (mask and RDI_STR) <> 0 then
              str := StringToLPOleStr ('Data Row='+ IntToStr (nIndex)+' Col='+IntToStr (nCol));


            nImage := 0
          end
      else
        with pResultDataItem do
        begin
          if (mask and RDI_STR) <> 0 then
            if nCol = 0 then
              str := StringToLPOleStr (resultItem.Text)
            else
              if nCol <= resultItem.SubItems.Count then
                str := StringToLPOleStr (resultItem.SubItems [nCol - 1])
              else
                str := '';

          nImage := resultItem.ImageIndex;
        end
    end;

    result := S_OK
  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ComponentGetResultViewType(cookie: Integer;
  out ppViewType: PWideChar; out pViewOptions: Integer): HResult;
var
  ScopeItem : TScopeItem;
  s : string;

  function LookupCLSID (const s : string) : string;
  var
    reg : TRegistry;
  begin
    if (s = '') or (s [1] = '{') then
      result := s
    else
    try
      reg := TRegistry.Create (KEY_READ);
      try
        reg.RootKey := HKEY_CLASSES_ROOT;
        if reg.OpenKey (s + '\' + 'CLSID', False) then
          result := reg.ReadString ('');
      finally
        reg.Free
      end
    except
      result := s;
    end
  end;

begin
  try
    ScopeItem := fParent.GetNodeForCookie (cookie) as TScopeItem;

    if Assigned (ScopeItem) then
      case ScopeItem.ViewType of
        vtListView:
          begin
            ppViewType := Nil;
            if ScopeItem.MultiSelect then
              pViewOptions := MMC_VIEW_OPTIONS_MULTISELECT
            else
              pViewOptions := 0;

            if Assigned (ScopeItem.OnOwnerData) then
              pViewOptions := pViewOptions or MMC_VIEW_OPTIONS_OWNERDATALIST;
          end;

        vtGUID :
          begin
            s := LookupCLSID (ScopeItem.ViewTypeObject);
            ppViewType := StringToLPOleStr (s);
            pViewOptions := MMC_VIEW_OPTIONS_NOLISTVIEWS// or MMC_VIEW_OPTIONS_CREATENEW
          end;

        vtHTML :
          begin
            ppViewType := StringToLPOleStr (ScopeItem.ViewTypeHTML);
            pViewOptions := MMC_VIEW_OPTIONS_NOLISTVIEWS
          end
      end;
    result := S_OK;
  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ComponentInitialize(
  const lpConsole: IConsole): HResult;
begin
  try
    fConsole := lpConsole as IConsole;
    try
      fConsole2 := fConsole as IConsole2;
    except
      fConsole2 := Nil
    end;

    fResultData := fConsole as IResultData;
    try
      fHeader := fConsole as IHeaderCtrl;
      fConsole.SetHeader (fHeader);
    except
      fHeader := Nil
    end;
    fConsole.QueryConsoleVerb (fConsoleVerb);
    fConsole.QueryResultImageList (fResultImages);

    result := S_OK;
  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ComponentNotify(const lpDataObject: IDataObject;
  event: _MMC_NOTIFY_TYPE; arg, param: Integer): HResult;
var
  resultItem : TResultItem;
  scopeItem : TScopeItem;
  allow : boolean;
  i, iid : Integer;
  bSelect : boolean;
begin
  try
    result := S_OK;

    if event = MMCN_INITOCX then
      fIOCX := IUnknown (param);

    if IS_SPECIAL_DATAOBJECT (lpDataObject) then
    begin
      case event of
        MMCN_SNAPINHELP      : fParent.fSnapinData.HelpCommand;
        MMCN_PROPERTY_CHANGE :
          fParent.Update (TObject (param))
      end
    end
    else
    begin
      if not fDeletingItem then
        DecodeDataObject (lpDataObject, scopeItem, resultItem)
      else
      begin
        scopeItem := Nil;
        resultItem := Nil
      end;

      case event of
        MMCN_INITOCX :
          begin
//            if Assigned (scopeItem) and Assigned (scopeItem.OnInitOCX) then
//              scopeItem.OnInitOCX (scopeItem, fIOCX)
          end;

        MMCN_ACTIVATE :
          if BOOL (arg) then
            fParent.fActiveComponent := fParent.fSnapinComponents.IndexOf (self)
          else
            fParent.fActiveComponent := 0;

        MMCN_VIEW_CHANGE :
            if Assigned (resultItem) then
            begin
              iid := resultItem.itemID [fParent.fActiveComponent];
              fResultData.UpdateItem (iid)
            end
            else
              if Assigned (scopeItem) then
              begin
                if (scopeItem.ViewType = vtListView) then
                begin
                  if (not fInShowing) then // and (scopeItem = fCurrentScopeItem) then
                  begin
                    InitializeHeaders (scopeItem);
                    if arg <> 0 then
                    begin

//  --- BUG.  It would make sense to set the image list here, but it corrupts the image list....
//
//                      SetResultImages (false);
//
                      EnumerateResultPane (scopeItem);
                    end
                    else
                      fResultData.DeleteAllRsltItems
                  end
                end;
                UpdateScopePane (scopeItem);
              end;

        MMCN_SHOW:
          begin
            bSelect := arg <> 0;

//            if not bSelect then
//              fIOCX := Nil;

            if bSelect then
              fSelectedScopeItem := scopeItem
            else
              fSelectedScopeItem := Nil;

            if Assigned (scopeItem) then
            begin
              if Assigned (scopeItem.SnapinData.OnScopeShow) then
              begin
                fInShowing := True;
                try
                  scopeItem.SnapinData.OnScopeShow (scopeItem, bSelect);
                finally
                  fInShowing := False
                end
              end;

              if scopeItem.ViewType = vtListView then
              begin
                if bSelect then
                begin
                  InitializeHeaders (scopeItem);
                  SetResultImages (scopeItem.scopeItems.Count > 0);
                  EnumerateResultPane (scopeItem);
                end
                else
                  fResultData.DeleteAllRsltItems
              end
              else
                if bSelect and (scopeItem.ViewType = vtGUID) and Assigned (scopeItem.OnInitOCX) then
                  scopeItem.OnInitOCX (scopeItem, fIOCX);
            end
          end;

        MMCN_CONTEXTHELP:
          scopeItem.HelpCommand;

        MMCN_CONTEXTMENU     : MessageBox (0, 'TTTestSnapin.MMCN_HELP', Nil, MB_ICONINFORMATION);
        MMCN_DESELECT_ALL,
        MMCN_SELECT       :
          begin
            bSelect := HIWORD (arg) <> 0;
            if bSelect and (event = MMCN_SELECT) then
              fParent.fActiveComponent := fParent.fSnapinComponents.IndexOf (self);
            HandleStandardVerbs(event = MMCN_DESELECT_ALL, arg, scopeItem, resultItem)
          end;

        MMCN_DBLCLICK :
          if Assigned (resultItem) and Assigned (ScopeItem.OnResultDblClick) then
            ScopeItem.OnResultDblClick (resultItem);

        MMCN_DELETE :
          if Assigned (resultItem) and Assigned (ScopeItem.OnResultDelete) then
          begin
            allow := True;
            ScopeItem.OnResultDelete (resultItem, allow);
            if allow then
            begin
              fDeletingItem := True;
              try
                for i := 0 to fParent.fSnapinComponents.Count - 1 do
                begin
                  iid := resultItem.itemID [i];
                  fResultData.DeleteItem (iid, 0)
                end
              finally
                fDeletingItem := False
              end
            end
          end;

        MMCN_RENAME :
          if Assigned (resultItem) and Assigned (ScopeItem.OnResultRename) then
          begin
            allow := True;
            ScopeItem.OnResultRename (resultItem, POleStr (param), allow);
            if not allow then result := S_FALSE;
          end;
      end
    end
  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ComponentQueryDataObject(cookie: Integer;
  _type: _DATA_OBJECT_TYPES; out ppDataObject: IDataObject): HResult;
var
  dataObject : TDataObject;
begin
  try
    dataObject := TDataObject.Create;
    dataObject.fInternal.m_type := _type;
    dataObject.fInternal.m_cookie := cookie;
    dataObject.fInternal.m_clsid := fParent.Factory.ClassID;
    dataObject.fInternal.m_object := self;
    ppDataObject := dataObject as IDataObject;
    result := S_OK;
  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
procedure TSnapinComponent.DecodeDataObject(dataObject: IDataObject;
  var scopeItem: TScopeItem; var resultItem: TResultItem);
var
  node : TObject;
  ownerDataIdx : Integer;
begin
  ownerDataIdx := -1;
  node := fParent.GetNodeForDataObject (dataObject, ownerDataIdx);
  resultItem := Nil;
  scopeItem := Nil;

  if Assigned (node) then
    if node is TScopeItem then
      scopeItem := TScopeItem (node)
    else
    begin
      resultItem := node as TResultItem;
      scopeItem := resultItem.ScopeItem
    end
  else
    if ownerDataIdx > -1 then
    begin
      scopeItem := fSelectedScopeItem;
      resultItem := fOwnerResultItem
    end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
procedure TSnapinComponent.EnumerateResultPane(node: TScopeItem);
var
  i : Integer;
  resultItem : _RESULTDATAITEM;
  idx : Integer;
begin
  idx := fParent.fSnapinComponents.IndexOf (self);
  with node do
  begin
    if Assigned (OnOwnerData) then
      fResultData.SetItemCount (ResultItems.Count, 0)
    else
    begin
      fResultData.SetItemCount (ResultItems.Count, 0);
      for i := 0 to ResultItems.Count - 1 do
      begin
        FillChar (resultItem, sizeof (resultItem), 0);
        resultItem.mask := RDI_STR or RDI_IMAGE or RDI_PARAM or RDI_INDEX;
        resultItem.str := MMC_CALLBACK;
        resultItem.nImage := node.ResultItems [i].ImageIndex;
        resultItem.lParam := Integer (node.ResultItems [i]);
        fResultData.InsertItem (resultItem);
        node.ResultItems [i].itemID [idx] := resultItem.itemID;
      end;

      fResultData.Sort (0, 0, -1)
    end
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ExtendContextMenuAddMenuItems(
  const dataObject: IDataObject; const piCallback: IContextMenuCallback;
  var pInsertionAllowed: Integer): HRESULT;
var
  scopeItem : TScopeItem;
  i : Integer;
  item : _CONTEXTMENUITEM;
  itm : TMenuItem;
begin
  try
    result := S_OK;
    scopeItem := GetScopeItemFromDataObject (dataObject);
    if Assigned (scopeItem) and Assigned (ScopeItem.ResultItemsContextMenu) then
    begin
      for i := 0 to ScopeItem.ResultItemsContextMenu.Items.Count - 1 do
      begin
        FillChar (item, sizeof (item), 0);
        itm := ScopeItem.ResultItemsContextMenu.Items [i];
        item.strName := PWideChar (WideString (itm.Caption));
        item.strStatusBarText := PWideChar (WideString (itm.Hint));
        item.lCommandID := itm.Command;
        GetContextMenuFlags (itm, item.fFlags, item.fSpecialFlags);
        item.lInsertionPointID := Integer (CCM_INSERTIONPOINTID_PRIMARY_TOP);

        result := piCallback.AddItem (item);
        if not Succeeded (result) then
          break;
      end
    end
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ExtendContextMenuCommand(nCommandID: Integer;
  const dataObject: IDataObject): HRESULT;
var
  scope : TScopeItem;
begin
  try
    scope := GetScopeItemFromDataObject (dataObject);
    if Assigned (scope) and Assigned (scope.ResultItemsContextMenu) then
      scope.ResultItemsContextMenu.DispatchCommand (nCommandID);

    result := S_OK;
  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ExtendControlbarControlbarNotify(
  event: _MMC_NOTIFY_TYPE; arg, param: Integer): HResult;
var
  bSelect : BOOL;
  scope, menuScope : TScopeITem;
  dataObject : IDataObject;
  h : HWND;
  i, idx, hiddenCount : Integer;
  menuItem : TMenuItem;

  procedure CreateToolbars (scopeItem : TScopeItem);
  var
    pToolbar : IUnknown;
    buttons : TSnapinToolBarButtons;
    button : TSnapinToolbarButton;
    images : TImageList;
    btn : _MMCButton;
    bmp : HBITMAP;
    mask : DWORD;
    i : Integer;

  begin
    if Assigned (fControlBar) and (scopeItem.ToolbarButtons.Count > 0) then
    begin
      buttons := scopeItem.ToolbarButtons;
      images := scopeItem.SnapinData.ScopeSmallImages;

      OleCheck (fControlBar.Create (TOOLBAR, self as IExtendControlbar, pToolbar));
      fToolbar := pToolbar as IToolbar;

      if Assigned (images) then
      begin
        bmp := ImageListToBitmap (images, 16, mask);
        try
          OleCheck (fToolbar.AddBitmap (fParent.FSnapinData.ScopeSmallImages.Count, wireHBitmap (bmp)^, 16, 16, mask));
        finally
          DeleteObject (bmp);
        end
      end;

      for i := 0 to Buttons.Count - 1 do
      begin
        button := Buttons.Items [i];
        ZeroMemory (@btn, sizeof (btn));

        btn.nBitmap := button.ImageIndex;
        btn.lpButtonText := StringToLPOleStr (button.Caption); // PWideChar (WideString (button.Caption));
        btn.lpTooltipText := StringToLPOleStr (button.Hint); // PWideChar (WideString (button.Hint));
        if button.Enabled then
          btn.fsState := TBSTATE_ENABLED
        else
          btn.fsState := TBSTATE_INDETERMINATE;

        if not button.Visible then
          btn.fsState := btn.fsState or TBSTATE_HIDDEN;

        if button.Wrap then
          btn.fsState := btn.fsState or TBSTATE_WRAP;

        btn.fsType := TBSTYLE_BUTTON;
        btn.idCommand := button.Command;
        OleCheck (fToolbar.InsertButton (i, btn))
      end;

      fControlBar.Attach (TOOLBAR, fToolbar);
    end
  end;

  procedure CreateMenuButtons (scope : TScopeItem);
  var
    pMenuButton : IUnknown;
    i : Integer;
  begin
    if Assigned (fControlBar) and Assigned (scope.MainMenu) and (scope.MainMenu.Items.Count > 0) then
    begin
      if not Assigned (FMenuButton) then
      begin
        OleCheck (fControlBar.Create (MENUBUTTON, self as IExtendControlbar, pMenubutton));
        FMenuButton := pMenuButton as IMenuButton
      end;

      for i := 0 to scope.MainMenu.ITems.Count - 1 do
      begin
        FMenuButton.AddButton (
          scope.MainMenu.Items [i].Command,
          StringToLPOleStr (scope.MainMenu.Items [i].Caption),
          StringToLPOleStr (scope.MainMenu.Items [i].Hint));

        with scope.MainMenu.Items [i] do
          FMenuButton.SetButtonState (Command, MMC_TLB.ENABLED, Integer (BOOL (Enabled)));

        with scope.MainMenu.Items [i] do
          FMenuButton.SetButtonState (Command, MMC_TLB.HIDDEN, Integer (BOOL (not Visible)));
      end;

      fControlBar.Attach (MENUBUTTON, FMenuButton);
    end
  end;

begin
  try
//    bScope := BOOL (LOWORD (arg));
    bSelect := BOOL (HIWORD (arg));

    case event of
      MMCN_BTN_CLICK     :
        begin
          dataObject := IDataObject (arg);
          scope := GetScopeItemFromDataObject (dataObject);
          while Assigned (scope) and scope.ParentToolbarButtons do
            scope := scope.Parent;
          if Assigned (scope) and Assigned (scope.ToolbarButtons) then
            scope.ToolbarButtons.DispatchCommand (param)
        end;

      MMCN_SELECT        :
        begin
          dataObject := IDataObject (param);
          scope := GetScopeItemFromDataObject (dataObject);
          menuScope := scope;

          while Assigned (scope) and scope.ParentToolbarButtons do
            scope := scope.Parent;

          while Assigned (menuScope) and menuScope.ParentMainMenu do
            menuScope := menuScope.Parent;

          if bSelect then
          begin
            if (scope <> fCurrentButtonsScope) then
            begin
              if Assigned (fToolbar) then
              begin
                fControlBar.Detach (fToolbar);
                fToolBar := Nil
              end;

              CreateToolBars (scope);
              fCurrentButtonsScope := Scope
            end
            else
              fControlBar.Attach (TOOLBAR, fToolbar);

            if (menuScope <> fCurrentMenuButtonsScope) then
            begin
              if Assigned (fMenuButton) then
              begin
                fControlBar.Detach (fMenuButton);
                fMenuButton := Nil
              end;

              CreateMenuButtons (menuScope);
            end
            else
              CreateMenuButtons (menuScope);
          end
        end;

      MMCN_MENU_BTNCLICK :
        begin
          dataObject := IDataObject (arg);
          scope := GetScopeItemFromDataObject (dataObject);

          while Assigned (scope) and scope.ParentMainMenu do
            scope := scope.Parent;

          if Assigned (scope) and Assigned (scope.MainMenu) then
          begin
            idx := -1;
            hiddenCount := 0;
            for i := 0 to scope.MainMenu.Items.Count - 1 do
              if scope.MainMenu.Items [i].Command = PMENUBUTTONDATA (param)^.idCommand then
              begin
                idx := i;
                break
              end
              else
                if not scope.MainMenu.Items [i].Visible then
                  Inc (hiddenCount);


            Dec (idx, hiddenCount);
            if idx > -1 then
            begin
              OleCheck (FConsole2.GetMainWindow (WireHWND (h)));
              i := Integer (TrackPopupMenuEx (GetSubMenu (scope.MainMenu.Handle, idx), TPM_NONOTIFY or TPM_RETURNCMD,
                PMENUBUTTONDATA (param)^.x, PMENUBUTTONDATA (param)^.y, h, nil));
              menuItem := scope.MainMenu.FindItem (i, fkCommand);
              if Assigned (menuItem) then
                menuItem.Click;
            end
          end
        end;
      MMCN_DESELECT_ALL :;
    end;
    result := S_OK;
  except
    result := HandleException
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ExtendControlbarSetControlbar(
  const pControlbar: IControlbar): HResult;
begin
  if pControlBar <> fControlBar then
  begin
    if Assigned (fToolBar) then
    begin
      fControlBar.Detach (fToolBar);
      fToolBar := Nil
    end;

    if Assigned (fMenuButton) then
    begin
      fControlBar.Detach (fMenuButton);
      fMenuButton := Nil
    end;

    fCurrentButtonsScope := Nil;
    fCurrentMenuButtonsScope := Nil;
  end;

  fControlBar := pControlBar;

  result := S_OK;
end;


function TSnapinComponent.ExtendPropertySheetCreatePropertyPages(
  const lpProvider: IPropertySheetCallback; handle: Integer;
  const lpIDataObject: IDataObject): HResult;
var
  node : TResultItem;
  scope : TScopeItem;
  changed : boolean;
begin
  DecodeDataObject (lpIdataObject, scope, node);
  if Assigned (node) and Assigned (scope.OnResultProperties) then
  begin
    changed := False;
    scope.OnResultProperties (node, changed);

    if changed then
      MMCPropertyChangeNotify (handle, Integer (node));

    MMCFreeNotifyHandle (handle);
    result := S_FALSE;   // return false, otherwise MMC displays an empty propert sheet...
  end
  else
    result := S_FALSE
end;

function TSnapinComponent.ExtendPropertySheetQueryPagesFor(
  const lpDataObject: IDataObject): HResult;
var
  node : TResultItem;
  scope : TScopeItem;
begin
  DecodeDataObject (lpdataObject, scope, node);
  if Assigned (node) and Assigned (scope.OnResultProperties) then
    result := S_OK
  else
    result := S_FALSE;
end;

function TSnapinComponent.FocusedResultItem: TResultItem;
var
  i : _RESULTDATAITEM;
begin
  ZeroMemory (@i, sizeof (i));
  i.nIndex := -1;
  i.nState := LVIS_FOCUSED;
  if Succeeded (fResultData.GetNextItem (i)) and (i.bScopeItem = 0) then
    result := TResultItem (i.lParam)
  else
    result := Nil
end;

function TSnapinComponent.GetResultItemState(viewIdx: Integer;
  item: TResultItem): SYSUINT;
var
  i : _RESULTDATAITEM;
begin
  FillChar (i, sizeof (i), 0);
  i.mask := RDI_STATE;
  i.itemID := item.ItemID [viewIdx];
  fResultData.GetItem (i);
  result := i.nState;
end;

function TSnapinComponent.GetScopeItemFromDataObject(
  dataObject: IDataObject): TScopeItem;
var
  node : TObject;
  ownerDataIdx : Integer;
  resultItem : TResultItem;
begin
  ownerDataIdx := -1;
  node := fParent.GetNodeForDataObject (dataObject, ownerDataIdx);
  result := Nil;

  if Assigned (node) then
    if node is TScopeItem then
      result := TScopeItem (node)
    else
    begin
      resultItem := node as TResultItem;
      result := resultItem.ScopeItem
    end
  else
    if ownerDataIdx > -1 then
      result := fSelectedScopeItem;
end;

procedure TSnapinComponent.HandleStandardVerbs(bDeselectAll: boolean;
  arg: Integer; scopeItem : TScopeItem; resultItem : TResultItem);
var
  bSelect : boolean;
  bScope : boolean;

  procedure ShowResultOption (option: Integer; show : boolean);
  var
    iHide, iEnable : Integer;
  begin
    if show then
    begin
      iHide := 0;
      iEnable := 1
    end
    else
    begin
      iHide := 1;
      iEnable := 0
    end;
    fConsoleVerb.SetVerbState(option, HIDDEN, iHide);
    fConsoleVerb.SetVerbState(option, ENABLED, iEnable)
  end;

  procedure ShowResultOptions (scope : boolean; show : boolean);
  begin
    if scope then
    begin
      ShowResultOption (MMC_VERB_DELETE, False);
      ShowResultOption (MMC_VERB_PROPERTIES, show and Assigned (scopeItem.OnScopeProperties));
      ShowResultOption (MMC_VERB_RENAME, False);
    end
    else
    begin
      ShowResultOption (MMC_VERB_DELETE, show and Assigned (scopeItem.OnResultDelete));
      ShowResultOption (MMC_VERB_PROPERTIES, show and Assigned (scopeItem.OnResultProperties));
      ShowResultOption (MMC_VERB_RENAME, show and Assigned (scopeItem.OnResultRename))
    end
  end;

begin
  if scopeItem.ViewType <> vtListView then
    ShowResultOptions (False, False)
  else
  begin
    bScope := WORDBOOL (LOWORD (arg));
    bSelect := WORDBOOL (HIWORD (arg)) and not bDeselectAll;

    if not bScope and not Assigned (resultITem) then
      bSelect := False;

    ShowResultOptions (bScope, bSelect)
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
procedure TSnapinComponent.InitializeHeaders(node: TScopeItem);
var
  i : Integer;
  column : TSnapinColumn;

  caption : WideString;
  align : Integer;
begin
  if Assigned (fHeader) then
  begin
    while Succeeded (fHeader.DeleteColumn (0)) do;
    for i := 0 to node.Columns.Count - 1 do
    begin
      column := node.Columns [i];
      caption := column.Caption;
      case column.Alignment of
        taCenter : align := LVCFMT_CENTER;
        taRightJustify : align := LVCFMT_RIGHT;
        else align := LVCFMT_LEFT
      end;

      if caption = '' then
        caption := 'xx';
      OleCheck (fHeader.InsertColumn (i, PWideChar (caption), align, column.Width))
    end
  end
end;


(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ResultDataCompareCompare(lUserParam, cookieA,
  cookieB: Integer; var pnResult: SYSINT): HResult;
var
  nCol : Integer;
  szStringA, szStringB : string;
  pDataA, pDataB : TResultItem;
begin
  try
    nCol := pnResult;
    pnResult := 0;

    pDataA := TResultItem (cookieA);
    pDataB := TResultItem (cookieB);

    if nCol = 0 then
    begin
      szStringA := pDataA.Text;
      szStringB := pDataB.Text;
    end
    else
    begin
      Dec (nCol);
      if nCol < pDataA.SubItems.Count then
        szStringA := pDataA.SubItems [nCol]
      else
        szStringA := '';

      if nCol < pDataB.SubItems.Count then
        szStringB := pDataB.SubItems [nCol]
      else
        szStringB := ''
    end;

    if szStringA < szStringB then
      pnResult := -1
    else
      if szStringA > szStringB then
        pnResult := 1
      else
        pnResult := 0;

    result := S_OK;

  except
    result := HandleException
  end
end;

(*--------------------------------------------------------------------------*
 | function TSnapinComponent.
 |                                                                          |
 |
 |                                                                          |
 | Parameters:                                                              |
 |
 |                                                                          |
 | The function returns an OLE success code
 *--------------------------------------------------------------------------*)
function TSnapinComponent.ResultOwnerDataCacheHint(nStartIndex,
  nEndIndex: SYSINT): HResult;
begin
  result := S_OK;
end;

function TSnapinComponent.ResultOwnerDataFindItem(
  var pFindInfo: _RESULTFINDINFO; out pnFoundIndex: SYSINT): HResult;
begin
  result := S_FALSE;
end;

function TSnapinComponent.ResultOwnerDataSortItems(nColumn: SYSINT;
  dwSortOptions: UINT; lUserParam: Integer): HResult;
begin
  result := S_FALSE;
end;

function TSnapinComponent.SelectedResultItem: TResultItem;
var
  i : _RESULTDATAITEM;
begin
  ZeroMemory (@i, sizeof (i));
  i.nIndex := -1;
  i.nState := LVIS_SELECTED;
  if Succeeded (fResultData.GetNextItem (i)) and (i.bScopeItem = 0) then
    result := TResultItem (i.lParam)
  else
    result := Nil
end;

procedure TSnapinComponent.SetResultImages (useScopeImages : boolean);
begin
  if Assigned (fParent.fSnapinData) and fParent.Initialized then
    if useScopeImages then
      SetImageList (fResultImages, fParent.fSnapinData.ScopeSmallImages, fParent.fSnapinData.ScopeLargeImages)
    else
      SetImageList (fResultImages, fParent.fSnapinData.ResultSmallImages, fParent.fSnapinData.ResultLargeImages);
end;

(*----------------------------------------------------------------------*
 | function TSnapinAbout.GetProvider                                    |
 |                                                                      |
 | Get provider name for about box and snapin list                      |                                                                      |
 |                                                                      |
 | Parameters:                                                          |
 |    out lpName : PWideChar    The provider name                       |
 |                                                                      |
 | The function returns an OLE success code                             |
 *----------------------------------------------------------------------*)
function TSnapinAbout.GetProvider(out lpName: PWideChar): HResult;
begin
  try
    LoadSnapinData;
    lpName := StringToLPOleStr (fSnapinData.Provider);
    result := S_OK;
  except
    result := HandleException
  end
end;

(*----------------------------------------------------------------------*
 | function TSnapinAbout.GetSnapinDescription                           |
 |                                                                      |
 | Get snapin description for about box and snapin list                 |
 |                                                                      |
 | Parameters:                                                          |
 |    out lpDescription : PWideChar    The provider name                |
 |                                                                      |
 | The function returns an OLE success code                             |
 *----------------------------------------------------------------------*)
function TSnapinAbout.GetSnapinDescription(
  out lpDescription: PWideChar): HResult;
begin
  try
    LoadSnapinData;
    lpDescription := StringToLPOleStr (fSnapinData.FileDescription);
    result := S_OK;
  except
    result := HandleException
  end
end;

(*----------------------------------------------------------------------*
 | function TSnapinAbout.GetSnapinImage                                 |
 |                                                                      |
 | Get icon for  about box and snapin list                              |
 |                                                                      |
 | Parameters:                                                          |
 |    out hAppIcon : wireICON    The icon                               |
 |                                                                      |
 | The function returns an OLE success code                             |
 *----------------------------------------------------------------------*)
function TSnapinAbout.GetSnapinImage(out hAppIcon: wireHICON): HResult;
var
  ico : HIcon;
begin
  try
    result := S_FALSE;
    ico := LoadIcon (HInstance, MakeIntResource (1));
    if ico <> 0 then
    begin
      hAppIcon := wireHICON (ico);
      result := S_OK
    end
  except
    result := HandleException
  end
end;

(*----------------------------------------------------------------------*
 | function TSnapinAbout.GetSnapinVersion                               |
 |                                                                      |
 | Get version for about box and snapin list                            |
 |                                                                      |
 | Parameters:                                                          |
 |    out lpVersion : PWideChar    The version string                   |
 |                                                                      |
 | The function returns an OLE success code                             |
 *----------------------------------------------------------------------*)
function TSnapinAbout.GetSnapinVersion(out lpVersion: PWideChar): HResult;
begin
  try
    LoadSnapinData;
    lpVersion := StringToLPOleStr (fSnapinData.FileVersion);
    result := S_OK;
  except
    result := HandleException
  end
end;

(*----------------------------------------------------------------------*
 | function TSnapinAbout.GetStaticFolderImage                           |
 |                                                                      |
 | Get image for static scope                                           |
 |                                                                      |
 | Parameters:                                                          |
 |    out hSmallImage : wireHBITMAP       Small image                   |
 |    out hSmallImageOpen : wireHBITMAP   Small open image              |
 |    out hLargeImage : wireHBITMAP       Large image                   |
 |    out cMask : UINT                    Color mask                    |
 |                                                                      |
 | The function returns an OLE success code                             |
 *----------------------------------------------------------------------*)
function TSnapinAbout.GetStaticFolderImage(out hSmallImage,
  hSmallImageOpen, hLargeImage: wireHBITMAP; out cMask: UINT): HResult;
var
  smBitmap : TBitmap;
  bmp : TBitmap;
begin
  try
    LoadSnapinData;
    smBitmap := Nil;
    bmp := TBitmap.Create;
    cMask := RGB (123, 234, 235);
    try
      smBitmap := TBitmap.Create;
      if Assigned (fSnapinData.ScopeItem) and Assigned (fSnapinData.ScopeSmallImages) and (fSnapinData.ScopeItem.ImageIndex > -1) and (fSnapinData.ScopeItem.ImageIndex < fSnapinData.ScopeSmallImages.Count) then
      begin
        fSnapinData.ScopeSmallImages.GetBitmap (fSnapinData.ScopeItem.ImageIndex, smBitmap);
        bmp.Assign (smBitmap);
        hSmallImage := wireHBITMAP (bmp.ReleaseHandle)
      end
      else
        hSmallImage := Nil;

      if Assigned (fSnapinData.ScopeItem) and Assigned (fSnapinData.ScopeSmallImages) and (fSnapinData.ScopeItem.OpenImageIndex > -1) and (fSnapinData.ScopeItem.OpenImageIndex < fSnapinData.ScopeSmallImages.Count) then
      begin
        fSnapinData.ScopeSmallImages.GetBitmap (fSnapinData.ScopeItem.OpenImageIndex, smBitmap);
        bmp.Assign (smBitmap);
        hSmallImageOpen := wireHBITMAP (bmp.ReleaseHandle)
      end
      else
        hSmallImageOpen := Nil;

      if Assigned (fSnapinData.ScopeItem) and Assigned (fSnapinData.ScopeLargeImages) and (fSnapinData.ScopeItem.ImageIndex > -1) and (fSnapinData.ScopeItem.ImageIndex < fSnapinData.ScopeLargeImages.Count) then
      begin
        fSnapinData.ScopeLargeImages.GetBitmap (fSnapinData.ScopeItem.ImageIndex, smBitmap);
        bmp.Assign (smBitmap);
        hLargeImage := wireHBITMAP (bmp.ReleaseHandle)
      end
      else
        hLargeImage := wireHBITMAP (LoadBitmap (HInstance, PChar (2)));

      result := S_OK;
    finally
      smBitmap.Free;
      bmp.Free;
    end
  except
    result := HandleException
  end
end;

(*----------------------------------------------------------------------*
 | function TSnapinAbout.LoadSnapinData                                 |
 |                                                                      |
 | Load the data for the snapin.                                        |
 *----------------------------------------------------------------------*)
procedure TSnapinAbout.LoadSnapinData;
begin
  if not Assigned (fSnapinData) then
    fSnapinData := GetSnapinData
end;

procedure TSnapinComponent.SetResultItemState(viewIdx : Integer; item: TResultItem;
  state: SYSUINT; enable: boolean);
var
  iid : Integer;
begin
  iid := item.itemID [viewIdx];

  if enable then
    fResultData.ModifyItemState (0, iid, state, 0)
  else
    fResultData.ModifyItemState (0, iid, state, 0);
end;

procedure TSnapinComponent.UpdateResultItemSettings(viewIdx : Integer; item: TResultItem);
var
  i : _RESULTDATAITEM;
begin
  if Assigned (item.ScopeItem.OnOwnerData) then
    OleCheck (fResultData.UpdateItem (item.ItemID [viewIdx]))
  else
  begin
    FillChar (i, sizeof (i), 0);
    i.itemID := item.itemID [viewIdx];
    i.mask := RDI_STR;
    i.str := MMC_CALLBACK;

    if item.ImageIndex > -1 then
    begin
      i.Mask := i.Mask or RDI_IMAGE;
      i.nImage := item.ImageIndex
    end;

    OleCheck (fResultData.SetItem (i))
  end
end;

procedure TSnapinComponent.UpdateScopePane(item : TScopeItem);
var
  ns : IConsoleNameSpace;
  cookie : Integer;
  i, idx : Integer;
  deleteList : TList;
  itm : _SCOPEDATAITEM;
  node : TScopeItem;
  hr : HRESULT;
begin
  deleteList := TList.Create;
  try
    ns := fParent.fConsoleNameSpace;
    if Succeeded (ns.GetChildItem (item.ItemID, i, cookie)) then
      while i <> 0 do
      begin
        deleteList.Add (pointer (i));
        ns.GetNextItem (i, i, cookie)
      end;

    for i := 0 to item.ScopeItems.Count - 1 do
    begin
      node := item.ScopeItems [i];
      idx := deleteList.IndexOf (pointer (node.ItemID));
      if (idx >= 0) and (node.itemID <> 0) then
        deleteList [idx] := pointer (0)
      else
      begin
        FillChar (itm, sizeof (itm), 0);
        itm.mask := SDI_STR or SDI_PARAM or SDI_PARENT or SDI_CHILDREN;

        if node.ImageIndex > -1 then      // Set the image
        begin
          itm.mask := itm.mask or SDI_IMAGE or SDI_OPENIMAGE;
          itm.nImage := node.ImageIndex;
          itm.nOpenImage := node.ImageIndex;
        end;

        itm.cChildren := node.ScopeItems.Count;
        if (itm.cChildren = 0) and (node.HasChildren) then
          itm.cChildren := 1;
        itm.relativeID := item.ItemID;
        itm.displayname := MMC_CALLBACK;
        itm.lParam := Integer (node);
        hr := ns.InsertItem (itm);
        node.itemID := itm.ID;
        OleCheck (hr);
      end
    end;

    for i := 0 to deleteList.Count - 1 do
      if Integer (deleteList [i]) <> 0 then
        ns.DeleteItem (Integer (deleteList [i]), 1);
  finally
    deleteList.Free
  end
end;

end.
