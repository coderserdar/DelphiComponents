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
// $Log:  23262: mcmControls.pas
//
//    Rev 1.16    2014-02-02 21:09:52  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.15    01-03-2011 20:39:54  mcm    Version: IMG 3.4
//
//    Rev 1.14    25-10-2009 17:23:48  mcm
// Support for Delphi 2010
//
//    Rev 1.13    26-08-2009 23:31:38  mcm    Version: IMG 3.2
// Delphi 3 correction to Delphi 2009 support
//
//    Rev 1.12    15-08-2009 13:23:42  mcm    Version: IMG 3.3
// Delphi 2009 suport
//
//    Rev 1.10    20-08-2007 20:28:26  mcm
// Added support for Delphi 2007
//
//    Rev 1.9    05-06-2006 22:48:08  mcm    Version: IMG 3.0
// Corrected selection of supported files based on extensions.
//
//    Rev 1.8    29-01-2006 12:21:02  mcm    Version: IMG 2.14
// Initial Unicode support.
//
//   Rev 1.7    22-09-2005 20:56:26  mcm    Version: IMG 2.9
// Corrected a problem filtering files by extension.
// Corrected problems related to Delphi 2005 and the TmcmShellListView.

//
//   Rev 1.6    15-05-2005 19:38:04  mcm    Version: IMG 2.9
// Fixed Path/file name differentiation.

//
//   Rev 1.5    27-10-2004 22:19:50  mcm    Version: IMG 2.6

//
//   Rev 1.4    20-07-2004 23:29:16  mcm    Version: IMG 2.5
// TmcmShellComboBox.Change, delayed calling inherited until FListView has been
// updatred.

//
//   Rev 1.3    08-07-2004 21:42:40  mcm    Version: IMG 2.5
// Fixed resource leaks.
// Fixed problems regarding Windows 95 & 98.
// General, corrected how information is obtained from SHGetFileinfo.
// Modified presentation of the controls in design mode.
// Modified how images are obtained from Window Shell.

//
//   Rev 1.2    25-06-2004 21:15:38  mcm
// Temporary ...

//
//   Rev 1.1    31-05-2004 23:53:34  mcm    Version: IMG 2.5
// Minor corrections.

//
//   Rev 1.0    19-05-2004 21:57:02  mcm    Version: IMG 2.5
// Initial revision.

unit mcmControls;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms, Dialogs,
      StdCtrls, ComCtrls,
      {$IFNDEF DCB3} ImgList, {$ENDIF}
      CommCtrl, ShlObj, ActiveX;
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
      Vcl.ImgList, WinApi.CommCtrl, WinApi.ShlObj, WinApi.ActiveX;
     {$ENDIF}      

type
  EmcmInvalidPath = class(Exception);


  TShellFolderCapability = (fcCanCopy,
                            fcCanDelete,
                            fcCanLink,
                            fcCanMove,
                            fcCanRename,
                            fcDropTarget,
                            fcHasPropSheet);
  TShellFolderCapabilities = set of TShellFolderCapability;

  TmcmOnFileChange = procedure(Sender : TObject; FileName : String) of object;

  IShellCommandVerb = interface
    ['{7D2A7245-2376-4D33-8008-A130935A2E8B}']
    procedure ExecuteCommand(Verb: WideString; var Handled: boolean);
    procedure CommandCompleted(Verb: WideString; Succeeded: boolean);
  end;


  TmcmFileType = (mftFolders, mftNonFolders, mftHidden);
  TmcmFileTypes = set of TmcmFileType;

  TmcmShellListView = class;

  //----------------------------------------------------------------------------
  // TmcmShellFolder
  // Maintains data related to a folder and file.
  //----------------------------------------------------------------------------

  TmcmShellFolder = class
  private
  protected
    FLevel        : integer;
    FPIDL         : PItemIDList;
    FFullPIDL     : PItemIDList;
    FIShellFolder : IShellFolder;
    FParent       : TmcmShellFolder;
    FAttributes   : WideString;
    FDate         : WideString;
    FSize         : integer;
    FTypeName     : WideString;

    function    GetDisplayName : WideString;
    procedure   GetDetails;
    function    GetPath : WideString;
  public
    constructor Create(AParent      : TmcmShellFolder;
                       APIDL        : PItemIDList;
                       AShellFolder : IShellFolder);
    destructor  Destroy; override;
    function    Capabilities : TShellFolderCapabilities;
    function    IsFolder : boolean;
    function    ParentShellFolder : IShellFolder;
    function    Rename(NewName : WideString) : boolean;

    function    GetImageIndex : integer;

    property    Attributes : WideString
      read      FAttributes;
    property    Date : WideString
      read      FDate;
    property    DisplayName : WideString
      read      GetDisplayName;
    property    TypeName : WideString
      read      FTypeName;
    property    Path : WideString
      read      GetPath;
    property    Level : integer
      read      FLevel;
    property    PIDL : PItemIDList
      read      FFullPIDL;
    property    Size : integer
      read      FSize;
    property    ShellFolder : IShellFolder
      read      FIShellFolder;

    property    AbsoluteID : PItemIDLIst
      read      FFullPIDL;
    property    RelativeID : PItemIDList
      read      FPIDL;
  end;


  //----------------------------------------------------------------------------
  // TmcmShellComboBox
  //----------------------------------------------------------------------------

  TmcmShellComboBox = class(TCustomComboBox)
  private
    FFolders     : TList;
    FListView    : TmcmShellListView;
    FUpdating    : boolean;
    FOnChange    : TNotifyEvent;
    FImageList   : TImageList;
    procedure   Createimages;
    function    GetPath : WideString;
    procedure   SetPath(NewPath : WideString);
    procedure   SetListView(Value : TmcmShellListView);
  protected
    function    AddDesktop : TmcmShellFolder;
    procedure   AddItems(Index : integer; ParentFolder : TmcmShellFolder);
    procedure   CreateWnd; override;
    procedure   ClearItems;
    procedure   DestroyWnd; override;
    procedure   DrawItem(Index : integer; Rect : TRect; State : TOwnerDrawState); override;
    function    GetFolder(Index : integer) : TmcmShellFolder;
    function    GetIndex : integer;
    function    IndexFromID(AbsoluteID : PItemIDList) : integer;
    procedure   Init;
    function    InitItem(ParentFolder : TmcmShellFolder; ID : PItemIDList) : TmcmShellFolder;
    procedure   Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure   SetIndex(Value : integer);
    procedure   SetPathFromID(ID : PItemIDList);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function    GetCount : integer; {$IFNDEF DCB3_5} override; {$ENDIF}
    procedure   Change; override;
    {$IFNDEF DCB3_5}
    procedure   Clear; override;
    {$ENDIF}
    procedure   Click; override;
    property    Count : integer
      read      GetCount;
    property    Index : integer
      read      GetIndex
      write     SetIndex;
    property    Folder[Index : integer] : TmcmShellFolder
      read      GetFolder;
  published
    {$IFNDEF DCB3}
    property    Anchors;
    property    BiDiMode;
    {$ENDIF}
    property    Ctl3D;
    property    Color;
    {$IFNDEF DCB3}
    property    Constraints;
    {$ENDIF}
    property    Cursor;
    property    DragCursor;
    {$IFNDEF DCB3}
    property    DragKind;
    {$ENDIF}
    property    DragMode;
    property    DropDownCount;
    property    Enabled;
    property    Font;
    property    Height;
    property    HelpContext;
    property    Hint;
    property    ImeMode;
    property    ImeName;
    property    ItemHeight;
    property    Items;
    property    Left;
    property    mcmShellListView : TmcmShellListView
      read      FListView
      write     SetListView;
    property    Name;
    {$IFNDEF DCB3}
    property    ParentBiDiMode;
    {$ENDIF}
    property    ParentColor;
    property    ParentCtl3D;
    property    ParentFont;
    property    ParentShowHint;
    property    Path : WideString
      read      GetPath
      write     SetPath;
    property    PopupMenu;
    property    ShowHint;
    property    TabOrder;
    property    TabStop;
    property    Tag;
    property    Top;
    property    Visible;
    property    Width;

    property    OnChange;
    property    OnClick;
    {$IFNDEF DCB3_4}
    property    OnContextPopup;
    {$ENDIF}
    property    OnDblClick;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnDrawItem;
    property    OnDropDown;
    {$IFNDEF DCB3}
    property    OnEndDock;
    {$ENDIF}
    property    OnEndDrag;
    property    OnEnter;
    property    OnExit;
    property    OnKeyDown;
    property    OnKeyPress;
    property    OnKeyUp;
    property    OnMeasureItem;
    {$IFNDEF DCB3}
    property    OnStartDock;
    {$ENDIF}
    property    OnStartDrag;
  end;


  //----------------------------------------------------------------------------
  // TmcmShellComboBox
  //----------------------------------------------------------------------------

  TmcmShellListView = class(TCustomListView, IShellCommandVerb)
  private
    FShellComboBox    : TmcmShellComboBox;
    FRoot             : TmcmShellFolder;
    FFolders          : TList;
    FUpdating         : boolean;
    FAllowExecute     : boolean;
    FFileName         : WideString;
    FFileType         : TmcmFileTypes;
    FFileMask         : WideString;
    FFileOpMode       : integer;
    FSortColumn       : integer;
    FSortForward      : boolean;
    FSelectedFiles    : TStrings;
    FOnFileChange     : TmcmOnFileChange;
    FAutoContext      : boolean;
    FObjectFlags      : integer;
    FSavePath         : WideString;

    FLargeImages      : integer;
    FSmallImages      : integer;
  protected
    procedure   ClearItems;
    procedure   Click; override;
    procedure   CompareFiles(Sender : TObject;
                             Item1, Item2 : TListItem;
                             Data         : integer;
                             var Compare  : integer);
    procedure   ColumnClicked(Sender : TObject; Column : TListColumn);
    function    CreateRootFromPIDL(ID : PItemIDList) : TmcmShellFolder;
    procedure   DblClick; override;
    procedure   Edit(const Item : TLVItem); override;
    {$IFNDEF DCB3_4}
    procedure   DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    {$ELSE}
    {$ENDIF}
    function    GetFolder(Index : integer) : TmcmShellFolder;
    function    GetIndex : integer;
    function    GetFileName: WideString;
    function    GetPath : WideString;
    function    GetSelectedCount : integer;
    procedure   WndProc(var Message: TMessage); override;
    procedure   KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure   KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure   Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure   RootChanged;
    procedure   SetFileMask(const Masks : WideString);
    procedure   SetFileName(NewFile: WideString);
    procedure   SetFileType(NewFileType: TmcmFileTypes);
    procedure   SetIndex(Value : integer);
    procedure   SetPath(NewPath : WideString);
    procedure   SetPathFromID(ID : PItemIDList);
    procedure   SetShellComboBox(Value : TmcmShellComboBox);
  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   CreateWnd; override;
    procedure   DestroyWnd; override;
    function    GetCount : integer; {$IFNDEF DCB3_5} override; {$ENDIF}
    procedure   Back;
    procedure   Clear; {$IFNDEF DCB3_5} override; {$ENDIF}
    procedure   CommandCompleted(Verb : WideString; Succeeded : boolean);
    procedure   ExecuteCommand(Verb : WideString; var Handled : boolean);
    procedure   NewFolder;
    procedure   OneLevelUp;

    property    AllowExecute : boolean
      read      FAllowExecute
      write     FAllowExecute;
    property    Column;
    property    Columns;
    property    Count : integer
      read      GetCount;
    property    FileName : WideString
      read      GetFileName
      write     SetFileName;
    property    Folder[Index : integer] : TmcmShellFolder
      read      GetFolder;
    property    IconOptions;
    property    Index : integer
      read      GetIndex
      write     SetIndex;
    property    Items;
    property    SelCount : integer
      read      GetSelectedCount;
    property    Selected;
    procedure   UpdateView;
    property    VisibleRowCount;
  published
    property    Align;
    property    AllocBy;
    {$IFNDEF DCB3}
    property    Anchors;
    property    AutoContext : boolean
      read      FAutoContext
      write     FAutoContext default True;
    property    BevelEdges;
    property    BevelInner;
    property    BevelKind;
    property    BevelOuter;
    property    BevelWidth;
    {$ENDIF}
    {$IFNDEF DCB3}
    property    BiDiMode;
    {$ENDIF}
    property    BorderStyle;
    property    Color;
    property    ColumnClick;
    {$IFNDEF DCB3}
    property    Constraints;
    {$ENDIF}
    property    Ctl3D;
    {$IFNDEF DCB3}
    property    DragKind;
    {$ENDIF}
    property    DragMode;
    property    DragCursor;
    property    FileMask : WideString
      read      FFileMask
      write     SetFileMask;
    property    FileType : TmcmFileTypes
      read      FFileType
      write     SetFileType default [mftFolders, mftNonFolders];
    {$IFNDEF DCB3}
    property    FlatScrollBars;
    {$ENDIF}
    property    Font;
    {$IFNDEF DCB3}
    property    FullDrag;
    {$ENDIF}
    property    GridLines;
    property    HideSelection default False;
    property    HotTrack;
    {$IFNDEF DCB3}
    property    HotTrackStyles;
    {$ENDIF}
    {$IFNDEF DCB3_4}
    property    HoverTime;
    {$ENDIF}
    property    mcmShellComboBox : TmcmShellComboBox
      read      FShellComboBox
      write     SetShellComboBox;
    property    MultiSelect default False;
    {$IFNDEF DCB3}
    property    ParentBiDiMode;
    {$ENDIF}
    property    ParentColor;
    property    ParentCtl3D;
    property    ParentFont;
    property    ParentShowHint;
    property    Path : WideString
      read      GetPath
      write     SetPath;
    property    ReadOnly default False;
    property    RowSelect default False;
    property    ShowColumnHeaders default True;
    property    ShowHint;
    property    TabOrder;
    property    TabStop;
    property    ViewStyle;
    (* 21.06.2004 mcm - Causes Delphi 6 & 7 to crash (Endless loop in SetViewStyle.
      read      GetViewStyle
      write     SetViewStyle default vsList;
    *)
    property    Visible;

    property    OnChange;
    property    OnChanging;
    property    OnClick;
    property    OnColumnClick;
    property    OnCompare;
    property    OnDblClick;
    property    OnDeletion;
    property    OnDragDrop;
    property    OnDragOver;
    property    OnEdited;
    property    OnEditing;
    property    OnEndDrag;
    property    OnEnter;
    property    OnExit;
    property    OnFileChange : TmcmOnFileChange
      read      FOnFileChange
      write     FOnFileChange;
    property    OnInsert;
    property    OnKeyDown;
    property    OnKeyPress;
    property    OnKeyUp;
    property    OnMouseDown;
    property    OnMouseMove;
    property    OnMouseUp;
    property    OnStartDrag;
  end;


function DoSHFileOp(Handle: THandle; OpMode: UInt; Src: WideString; Dest: WideString; var Aborted: boolean): boolean;


implementation

uses {$IFNDEF GE_DXE2}
      {$IFDEF DCB3_5} FileCtrl, {$ENDIF}
      ShellAPI,
      ComObj,
      {$IFDEF GE_D2009} Types, {$ENDIF}
     {$ELSE}
      Vcl.FileCtrl,
      WinApi.ShellAPI,
      System.Win.ComObj,
      System.Types,
       {$IFDEF GE_DXE4}
         System.AnsiStrings,
       {$ENDIF}
     {$ENDIF}
     mcmImageResStr;

const
  //SRFDesktop     = 'rfDesktop'; // Do not localize
  SCmdVerbOpen   = 'open';      // Do not localize
  SCmdVerbRename = 'rename';    // Do not localize
  SCmdVerbDelete = 'delete';    // Do not localize
  SCmdVerbPaste  = 'paste';     // Do not localize

type
  TRoot       = type WideString;
  TRootFolder = (rfDesktop, rfMyComputer, rfNetwork, rfRecycleBin, rfAppData,
                 rfCommonDesktopDirectory, rfCommonPrograms, rfCommonStartMenu,
                 rfCommonStartup, rfControlPanel, rfDesktopDirectory, rfFavorites,
                 rfFonts, {$IFNDEF VER100} rfInternet, {$ENDIF} rfPersonal,
                 rfPrinters, rfPrintHood, rfPrograms, rfRecent, rfSendTo,
                 rfStartMenu, rfStartup, rfTemplates);

const
  nFolder : array[TRootFolder] of integer =
            (CSIDL_DESKTOP, CSIDL_DRIVES, CSIDL_NETWORK, CSIDL_BITBUCKET,
             CSIDL_APPDATA, CSIDL_COMMON_DESKTOPDIRECTORY, CSIDL_COMMON_PROGRAMS,
             CSIDL_COMMON_STARTMENU, CSIDL_COMMON_STARTUP, CSIDL_CONTROLS,
             CSIDL_DESKTOPDIRECTORY, CSIDL_FAVORITES, CSIDL_FONTS,
             {$IFNDEF VER100} CSIDL_INTERNET, {$ENDIF} CSIDL_PERSONAL,
             CSIDL_PRINTERS, CSIDL_PRINTHOOD, CSIDL_PROGRAMS, CSIDL_RECENT,
             CSIDL_SENDTO, CSIDL_STARTMENU, CSIDL_STARTUP, CSIDL_TEMPLATES);


var ICM2 : IContextMenu2 = nil;


//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

function DoSHFileOp(Handle: THandle; OpMode: UInt; Src: WideString; Dest: WideString; var Aborted: boolean): boolean;
var Return : integer;
    ipFileOp : TSHFileOpStructW;
begin
  FillChar(ipFileOp, SizeOf(ipFileOp), 0);
  with ipFileOp
  do begin
     wnd := Handle;
     wFunc := OpMode;
     pFrom := pWideChar(Src);
     pTo := pWideChar(Dest);
     fFlags := FOF_ALLOWUNDO; // or FOF_SIMPLEPROGRESS;
     fAnyOperationsAborted := False;
     hNameMappings := nil;
     lpszProgressTitle := '';
  end;
  try
    Return := SHFileOperationW(ipFileOp);
    Aborted := ipFileOp.fAnyOperationsAborted;
  except
    Return := 1;
  end;
  Result := (Return = 0);
end; // DoSHFileOp.


function AddSlash(Path : WideString) : WideString;
begin
  if Path[Length(Path)] <> '\'
  then Result := Path + '\'
  else Result := Path;
end; // AddSlash.


function FileTimeToDateTimeStr(FileTime : TFileTime): WideString;
var LocFTime     : TFileTime;
    SysFTime     : TSystemTime;
    DateStr      : WideString;
    TimeStr      : WideString;
    FDateTimeStr : WideString;
    Dt, Tm       : TDateTime;
begin
  FileTimeToLocalFileTime(FileTime, LocFTime);
  FileTimeToSystemTime(LocFTime, SysFTime);
  try
    with SysFTime
    do begin
       Dt := EncodeDate(wYear, wMonth, wDay);
       DateStr := DateToStr(Dt);
       Tm := EncodeTime(wHour, wMinute, wSecond, wMilliseconds);
       Timestr := TimeToStr(Tm);
       FDateTimeStr := DateStr + '   ' + TimeStr;
    end;
    Result := DateTimeToStr(StrToDateTime(FDateTimeStr));
  except
    Result := '';
  end;
end; // FileTimeToDateTimeStr.


//------------------------------------------------------------------------------
//
//------------------------------------------------------------------------------

function DesktopShellFolder : IShellFolder;
begin
  OleCheck(SHGetDesktopFolder(Result));
end; // DesktopShellFolder.


function SamePIDL(ID1, ID2 : PItemIDList) : boolean;
begin
  Result := DesktopShellFolder.CompareIDs(0, ID1, ID2) = 0;
end; // SamePIDL.


function GetIShellFolder(IFolder : IShellFolder;
                         PIDL    : PItemIDList;
                         Handle  : THandle) : IShellFolder;
var HR : HResult;
begin
  if Assigned(IFolder)
  then begin
       HR := IFolder.BindToObject(PIDL, Nil, IID_IShellFolder, Pointer(Result));
       if (HR <> S_OK)
       then IFolder.GetUIObjectOf(Handle, 1, PIDL, IID_IShellFolder, Nil, Pointer(Result));
       if (HR <> S_OK)
       then IFolder.CreateViewObject(Handle, IID_IShellFolder, Pointer(Result));
  end;
  if Not(Assigned(Result))
  then DesktopShellFolder.BindToObject(PIDL, Nil, IID_IShellFolder, Pointer(Result));
end; // GetIShellFolder.


function DesktopPIDL : PItemIDList;
begin
  OleCheck(SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, Result));
end; // DesktopPIDL.


function CreatePIDL(Size : integer) : PItemIDList;
var Malloc : IMalloc;
begin
  OleCheck(SHGetMalloc(Malloc));
  Result := Malloc.Alloc(Size);
  if Assigned(Result)
  then FillChar(Result^, Size, 0); // unicode
end; // CreatePIDL.


function NextPIDL(IDList : PItemIDList) : PItemIDList;
begin
  Result := IDList;
  Inc(PAnsiChar(Result), IDList^.mkid.cb);
end; // NextPIDL.


procedure StripLastID(IDList : PItemIDList);
var MarkerID : PItemIDList;
begin
  MarkerID := IDList;
  if Assigned(IDList)
  then begin
       while (IDList.mkid.cb <> 0)
       do begin
          MarkerID := IDList;
          IDList := NextPIDL(IDList);
       end;
       MarkerID.mkid.cb := 0;
  end;
end; // StripLastID.


function GetPIDLSize(IDList : PItemIDList) : integer;
begin
  Result := 0;
  if (IDList <> Nil)
  then begin
       Result := SizeOf(IDList^.mkid.cb);
       while ((IDList <> Nil) and (IDList^.mkid.cb <> 0))
       do begin
          Result := Result + IDList^.mkid.cb;
          IDList := NextPIDL(IDList);
       end;
  end;
end; // GetPIDLSize.


function CopyPIDL(IDList : PItemIDList) : PItemIDList;
var Size : integer;
begin
  Size := GetPIDLSize(IDList);
  Result := CreatePIDL(Size);
  if Assigned(Result)
  then CopyMemory(Result, IDList, Size);
end; // CopyPIDL.


function ConcatPIDLs(IDList1, IDList2 : PItemIDList) : PItemIDList;
var cb1, cb2 : integer;
begin
  if Assigned(IDList1)
  then cb1 := GetPIDLSize(IDList1) - SizeOf(IDList1^.mkid.cb)
  else cb1 := 0;
  cb2 := GetPIDLSize(IDList2);

  Result := CreatePIDL(cb1 + cb2);
  if Assigned(Result)
  then begin
       if Assigned(IDList1)
       then CopyMemory(Result, IDList1, cb1);
       CopyMemory(PAnsiChar(Result) + cb1, IDList2, cb2);
  end;
end; // ConcatPIDLs.


procedure DisposePIDL(PIDL : PItemIDList);
var MAlloc : IMAlloc;
begin
  OLECheck(SHGetMAlloc(MAlloc));
  MAlloc.Free(PIDL);
end; // DisposePIDL.


function CreatePIDLList(ID : PItemIDList) : TList;
var TempID : PItemIDList;
begin
  Result := TList.Create;
  TempID := ID;
  while (TempID.mkid.cb <> 0)
  do begin
     TempID := CopyPIDL(TempID);
     Result.Insert(0, TempID); // 0 = lowest level PIDL.
     StripLastID(TempID);
  end;
end; // CreatePIDLList.


procedure DestroyPIDLList(var List : TList);
var i : integer;
begin
  if (List = Nil)
  then Exit;
  for i := 0 to (List.Count - 1)
  do DisposePIDL(List[i]);
  List.Free;
  List := Nil;
end; // DestroyPIDLList.


function GetItemCount(IDList : PItemIDList) : integer;
begin
  Result := 0;
  while (IDList^.mkid.cb <> 0)
  do begin
     Inc(Result);
     IDList := NextPIDL(IDList);
  end;
end; // GetItemCount.


function RelativeFromAbsolute(AbsoluteID : PItemIDList) : PItemIDList;
begin
  Result := AbsoluteID;
  while (GetItemCount(Result) > 1)
  do Result := NextPIDL(Result);
  Result := CopyPIDL(Result);
end; // RelativeFromAbsolute.


function StrRetToString(PIDL : PItemIDList; StrRet : TStrRet; Flag : WideString) : WideString;
var P : PWideChar;
begin // unicode
  case StrRet.uType of
  STRRET_CSTR   : SetString(Result, StrRet.cStr, {$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrLen(StrRet.cStr));
  STRRET_OFFSET : begin
                    P := @PIDL.mkid.abID[StrRet.uOffset - SizeOf(PIDL.mkid.cb)];
                    SetString(Result, P, PIDL.mkid.cb - StrRet.uOffset);
                  end;
  STRRET_WSTR   : begin
                    if Assigned(StrRet.pOleStr)
                    then Result := StrRet.pOleStr
                    else Result := '';
                    // mcm, Must de-allocate pOleStr!
                    DisposePIDL(Pointer(StrRet.pOleStr));
                    StrRet.pOleStr := Nil;
                  end;
  end;
  // Fix to get around Windows Shell Controls returning spurious "?"s in
  // date/time detail fields.
  {$IFNDEF VER100}
   if (Length(Result) > 1) and (Result[1] = '?') and (('0' <= Result[2]) and (Result[2] <= '9')) // (Result[2] in ['0'..'9'])
   then Result := StringReplace(Result, '?', '', [rfReplaceAll]);
  {$ELSE}
   if (Length(Result) > 1) and (Result[1] = '?') and (('0' <= Result[2]) and (Result[2] <= '9')) // (Result[2] in ['0'..'9'])
   then ; //Result := ;
  {$ENDIF}
end; // StrRetToString.


function GetShellDisplayName(Parentfolder : IShellFolder;
                             PIDL         : PItemIDList;
                             Flags        : DWORD) : WideString;
var StrRet : TStrRet;
begin
  Result := '';
  if (ParentFolder = Nil)
  then begin
       Result := 'parentfolder = nil';  { Do not localize }
       Exit;
  end;
  FillChar(StrRet, SizeOf(StrRet), 0);
  StrRet.uType := STRRET_WSTR; //STRRET_CSTR;
  ParentFolder.GetDisplayNameOf(PIDL, Flags, StrRet);
  Result := StrRetToString(PIDL, StrRet, '');
  // TODO 2 -oMGD -cShell Controls :
  //  Remove this hack (on Win2k, GUIDs are returned for the
  //  PathName of standard folders)
  if (Pos(WideString('::{'), Result) = 1)
  then Result := GetShellDisplayName(ParentFolder, PIDL, SHGDN_NORMAL);
end; // GetShellDisplayName.


function GetIsFolder(ParentFolder : IShellFolder; PIDL : PItemIDList): Boolean;
var
  Flags: dword;
begin
  Flags := SFGAO_FOLDER;
  ParentFolder.GetAttributesOf(1, PIDL, Flags);
  Result := SFGAO_FOLDER and Flags <> 0;
end; // GetIsFolder.

(*
function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
var
  LUnknown: IUnknown;
begin
  Result := (Instance <> nil) and
            ((Instance.GetInterface(IUnknown, LUnknown) {and
              Supports(LUnknown, IID, Intf)}) or
             Instance.GetInterface(IID, Intf));
end;
*)

{$IFNDEF DCB3_4}

procedure InvokeContextMenu(Owner : TWinControl; AFolder : TmcmShellFolder; X, Y : integer);
var PIDL    : PItemIDList;
    CM      : Pointer; //IContextMenu;
    Menu    : HMenu;
    {$IFDEF DCB_UNIC}
    ICI     : TCMInvokeCommandInfoEx;   
    {$ELSE}
    ICI     : TCMInvokeCommandInfo;
    {$ENDIF}
    P       : TPoint;
    Command : LongBool;
    ICmd    : integer;
    ZVerb   : array[0..255] of AnsiChar;
    Verb    : WideString;
    Handled : boolean;
    SCV     : IShellCommandVerb;
    HR      : HResult;
begin
  if (AFolder = Nil)
  then Exit;
  PIDL := AFolder.FPIDL;
  AFolder.ParentShellFolder.GetUIObjectOf(Owner.Handle, 1, PIDL, IID_IContextMenu, nil, CM);
  if (CM = Nil)
  then Exit;
  P.X := X;
  P.Y := Y;

  {$IFDEF GE_DXE2} WinApi.{$ENDIF}Windows.ClientToScreen(Owner.Handle, P);
  Menu := CreatePopupMenu;
  try
    IContextMenu(CM).QueryContextMenu(Menu, 0, 1, $7FFF, CMF_EXPLORE or CMF_CANRENAME);
    IContextMenu(CM).QueryInterface(IID_IContextMenu2, ICM2); //To handle submenus.
    try
      Command := TrackPopupMenu(Menu, TPM_LEFTALIGN or
                                      TPM_LEFTBUTTON or
                                      TPM_RIGHTBUTTON or
                                      TPM_RETURNCMD,
                                      P.X, P.Y, 0, Owner.Handle, Nil);
    finally
      ICM2 := Nil;
    end;
    if Command
    then begin
         ICmd := LongInt(Command) - 1;
         HR := IContextMenu(CM).GetCommandString(ICmd, GCS_VERBA, nil, ZVerb, SizeOf(ZVerb));
         Verb := WideString({$IFDEF GE_DXE4}System.AnsiStrings.{$ENDIF}StrPas(ZVerb));
         Handled := False;
         if Supports(Owner, IShellCommandVerb, SCV)
         then begin
              HR := 0;
              SCV.ExecuteCommand(Verb, Handled);
         end;
         if Not(Handled)
         then begin
              FillChar(ICI, SizeOf(ICI), #0);
              with ICI
              do begin
                 cbSize := SizeOf(ICI);
                 hWND := Owner.Handle;
                 {$IFDEF DCB_UNIC}
                   lpVerbW := MakeIntResource(ICmd);
                 {$ELSE}
                   lpVerb := MakeIntResource(ICmd);
                 {$ENDIF}
                 nShow := SW_SHOWNORMAL;
              end;
              HR := IContextMenu(CM).InvokeCommand(PCMInvokeCommandInfo(@ICI)^);
         end;

         if Assigned(SCV)
         then SCV.CommandCompleted(Verb, HR = S_OK);
    end;
  finally
    DestroyMenu(Menu);
  end;
end; // InvokeContextMenu.

{$ENDIF}


//------------------------------------------------------------------------------
// TmcmShellFolder
//------------------------------------------------------------------------------

constructor TmcmShellFolder.Create(AParent      : TmcmShellFolder;
                                   APIDL        : PItemIDList;
                                   AShellFolder : IShellFolder);
var DesktopID : PItemIDList;
begin
  Inherited Create;
  FParent       := AParent;
  FIShellFolder := AShellFolder;
  FPIDL         := CopyPIDL(APIDL);

  FAttributes   := '';
  FDate         := '';
  FSize         := 0;
  FTypeName     := '';

  if (FParent <> Nil)
  then FFullPIDL := ConcatPIDLs(AParent.FFullPIDL, APIDL)
  else begin
       DesktopID := DesktopPIDL;
       try
         FFullPIDL := ConcatPIDLs(DesktopID, APIDL);
       finally
         DisposePIDL(DesktopID);
       end;
  end;

  if Not(Assigned(FParent))
  then FLevel := 0
  else FLevel := FParent.Level + 1;
end; // TmcmShellFolder.Create.


destructor TmcmShellFolder.Destroy;
begin
  FIShellFolder := Nil;
  if (FPIDL <> Nil)
  then DisposePIDL(FPIDL);
  FPIDL := Nil;
  if (FFullPIDL <> Nil)
  then DisposePIDL(FFullPIDL);
  FFullPIDL := Nil;
  Inherited Destroy;
end; // TmcmShellFolder.Destroy.


function TmcmShellFolder.GetDisplayName : WideString;
var FileInfo : TSHFileInfoW;
begin
  SHGetFileInfoW(PChar(AbsoluteID), 0, FileInfo, SizeOf(TSHFileInfoW), SHGFI_DISPLAYNAME or SHGFI_PIDL);
  Result := FileInfo.szDisplayName;
end; // TmcmShellFolder.GetDisplayName.


function TmcmShellFolder.GetPath : WideString;
var ShellFolder : IShellFolder;
    TempPath    : WideString;
begin
  ShellFolder := DesktopShellFolder;
  TempPath := GetShellDisplayName(ShellFolder, AbsoluteID, SHGDN_FORPARSING);
  // Never dispose or release this ShellFolder instance, Win 95 & 98 will fail.

  // Is TempPath a file or a directory
  if Not(FileExists(TempPath)) // unicode
  then TempPath := AddSlash(TempPath);

  Result := TempPath;
end; // TmcmShellFolder.GetPath.


function TmcmShellFolder.GetImageIndex : integer;
var FileInfo : TSHFileInfoW;
begin
  SHGetFileInfoW(PChar(AbsoluteID), 0, FileInfo, SizeOf(TSHFileInfoW),
                 SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_PIDL);
  Result := FileInfo.iIcon;
end; // TmcmShellFolder.GetImageIndex.


procedure TmcmShellFolder.GetDetails;
var hFindFile : THandle;
    Win32FD   : TWin32FindDataW;

  function AttrStr(Attr : integer) : WideString;
  begin
    Result := '';
    if ((FILE_ATTRIBUTE_DIRECTORY and Attr) > 0)
    then Result := Result + '';
    if ((FILE_ATTRIBUTE_ARCHIVE and Attr) > 0)
    then Result := Result + 'A';
    if ((FILE_ATTRIBUTE_READONLY and Attr) > 0)
    then Result := Result + 'R';
    if ((FILE_ATTRIBUTE_HIDDEN and Attr) > 0)
    then Result := Result + 'H';
    if ((FILE_ATTRIBUTE_SYSTEM and Attr) > 0)
    then Result := Result + 'S';
  end; // AtstrStr.

var Name     : WideString;
    FileInfo : TSHFileInfoW;
begin
  Name := GetPath;
  if (Name <> '')
  then begin
       FillChar(FileInfo, SizeOf(TSHFileInfoW), #0);
       SHGetFileInfoW(PChar(AbsoluteID), 0, FileInfo, SizeOf(TSHFileInfoW),
                     SHGFI_ATTRIBUTES or SHGFI_TYPENAME or SHGFI_DISPLAYNAME or SHGFI_PIDL);
       FTypeName := FileInfo.szTypeName;

       hFindFile := FindFirstFileW(PWideChar(Name), Win32FD);
       if (hFindFile <> INVALID_HANDLE_VALUE)
       then begin
            try
              with Win32FD
              do begin
                 FSize := (nFileSizeHigh * MAXDWORD) + nFileSizeLow;
                 FDate := FileTimeToDateTimeStr(ftLastWriteTime);
                 FAttributes := AttrStr(dwFileAttributes);
              end;
            finally
              {$IFDEF GE_DXE2} WinApi.{$ENDIF}windows.FindClose(hFindFile);
            end;
       end;
  end;
end; // TmcmShellFolder.GetDetails.


function TmcmShellFolder.ParentShellFolder : IShellFolder;
begin
  if (FParent <> Nil)
  then Result := FParent.ShellFolder
  else OLECheck(SHGetDesktopFolder(Result));
end; // TmcmShellFolder.ParentShellFolder.


function TmcmShellFolder.IsFolder : boolean;
begin
  Result := GetIsFolder(ParentShellFolder, FPIDL);
end; // TmcmShellFolder.IsFolder.


function TmcmShellFolder.Capabilities : TShellFolderCapabilities;
var Flags : dword;
begin
  Result := [];
  Flags := SFGAO_CAPABILITYMASK;
  ParentShellFolder.GetAttributesOf(1, FPIDL, Flags);
  if ((SFGAO_CANCOPY and Flags) <> 0)
  then Include(Result, fcCanCopy);
  if ((SFGAO_CANDELETE and Flags) <> 0)
  then Include(Result, fcCanDelete);
  if ((SFGAO_CANLINK and Flags) <> 0)
  then Include(Result, fcCanLink);
  if ((SFGAO_CANMOVE and Flags) <> 0)
  then Include(Result, fcCanMove);
  if ((SFGAO_CANRENAME and Flags) <> 0)
  then Include(Result, fcCanRename);
  if ((SFGAO_DROPTARGET and Flags) <> 0)
  then Include(Result, fcDropTarget);
  if ((SFGAO_HASPROPSHEET and Flags) <> 0)
  then Include(Result, fcHasPropSheet);
end; // TmcmShellFolder.Capabilities.


function TmcmShellFolder.Rename(NewName : Widestring) : boolean;
var NewPIDL : PItemIDList;
begin
  Result := False;
  if (fcCanRename in Capabilities)
  then begin
       Result := ParentShellFolder.SetNameOf(0, FPIDL, PWideChar(NewName),
                                             SHGDN_NORMAL, NewPIDL) = S_OK;
       if Result
       then begin
            DisposePIDL(FPIDL);
            DisposePIDL(FFullPIDL);
            FPIDL := NewPIDL;
            if (FParent <> Nil)
            then FFullPIDL := ConcatPIDLs(FParent.FPIDL, NewPIDL)
            else FFullPIDL := CopyPIDL(NewPIDL);
       end
       else Raise Exception.Create(resFailedToOpen + NewName);
  end;
end; // TmcmShellFolder.Rename.


//------------------------------------------------------------------------------
// TmcmShellComboBox
//------------------------------------------------------------------------------

constructor TmcmShellComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUpdating := False;
  FListView := Nil;
  Style := csOwnerDrawFixed;
  if (ItemHeight < 18)
  then ItemHeight := 18;
  FImageList := Nil;
  CreateImages;
  FFolders := TList.Create;
  Self.Sorted := False;
end; // TmcmShellComboBox.Create.


destructor TmcmShellComboBox.Destroy;
begin
  ClearItems;
  if Assigned(FFolders)
  then FFolders.Free;
  FFolders := Nil;
  if Assigned(FImageList)
  then FImageList.Free;
  inherited Destroy;
end; // TmcmShellComboBox.Destroy.


procedure TmcmShellComboBox.CreateWnd;
begin
  inherited CreateWnd;
 // Init;
end; // TmcmShellComboBox.CreateWnd.


procedure TmcmShellComboBox.DestroyWnd;
begin
 // ClearItems;
  inherited DestroyWnd;
end; // TmcmShellComboBox.DestroyWnd.


{$IFNDEF DCB3_5}
procedure TmcmShellComboBox.Clear;
begin
  ClearItems;
  Inherited Clear;
end; // TmcmShellComboBox.Clear.
{$ENDIF}

procedure TmcmShellComboBox.ClearItems;
var i : integer;
begin
  if HasParent
  then begin
       Items.BeginUpdate;
       for i := 0 to (Items.Count - 1)
       do Items.Objects[i] := Nil;
       Items.Clear;
  end;

  if Assigned(FFolders)
  then begin
       for i := 0 to (FFolders.Count - 1)
       do begin
          if Assigned(FFolders[i])
          then begin
               TmcmShellFolder(FFolders[i]).Free;
               FFolders[i] := Nil;
          end;
       end;
       FFolders.Clear;
  end;
  if HasParent
  then Items.EndUpdate;
end; // TmcmShellComboBox.ClearItems.


function TmcmShellComboBox.GetPath : WideString;
var Folder : TmcmShellFolder;
begin
  Result := '';
  if (ItemIndex > -1)
  then begin
       Folder := TmcmShellFolder(Items.Objects[ItemIndex]);
       if Assigned(Folder)
       then Result := Folder.Path
       else Result := '';
  end;
end; // TmcmShellComboBox.GetPath.


procedure TmcmShellComboBox.SetPath(NewPath : WideString);
var P        : PWideChar;
    NewPIDL  : PItemIDList;
    Flags    : dword;
    NumChars : dword;
begin
  if FUpdating
  then Exit;

  if {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(NewPath) // unicode
  then begin
       NewPath := AddSlash(NewPath);
       NumChars := Length(NewPath);
       Flags := 0;
       P := StringToOleStr(NewPath);
       try
         OLECheck(DesktopShellFolder.ParseDisplayName(0, Nil, P, NumChars, NewPIDL, Flags));
         if (NewPIDL <> Nil)
         then SetPathFromID(NewPIDL);
       except
         on EOleSysError
         do raise EmcmInvalidPath.CreateFmt(resErrorSettingPath, [NewPath]);
       end;
  end
  else begin
       if Not(csDesigning in ComponentState)
       then begin
            Init;
            Change;
       end
       else begin
            Items.BeginUpdate;
            ClearItems;
            Items.EndUpdate;
       end;
  end;
end; // TmcmShellComboBox.SetPath.


function TmcmShellComboBox.GetCount : integer;
begin
  Result := Items.Count;
end; // TmcmShellComboBox.GetCount.


function TmcmShellComboBox.GetIndex : integer;
begin
  Result := ItemIndex;
end; // TmcmShellComboBox.GetIndex.


procedure TmcmShellComboBox.SetIndex(Value : integer);
begin
  if (Value < Items.Count)
  then ItemIndex := Value;
end; // TmcmShellComboBox.SetIndex.


function TmcmShellComboBox.GetFolder(Index : integer) : TmcmShellFolder;
begin
  if (0 <= Index) and (Index < FFolders.Count)
  then Result := TmcmShellFolder(Items.Objects[Index])
  else Result := Nil;
end; // TmcmShellComboBox.GetFolder.


procedure TmcmShellComboBox.SetListView(Value : TmcmShellListView);
var Node : TmcmShellFolder;
begin
  if (Value <> FListView) and (ItemIndex >= 0)
  then begin
       if (Value <> Nil)
       then begin
           Node := TmcmShellFolder(Items.Objects[ItemIndex]);
           Value.FShellComboBox := Self;
           FUpdating := True;
           try
             if Assigned(Node)
             then Value.SetPathFromID(Node.AbsoluteID);
           finally
             FUpdating := False;
           end;
       end
       else begin
            if (FListView <> Nil)
            then FListView.FShellComboBox := Nil;
       end;
       if (FListView <> Nil)
       then FListView.FreeNotification(Self);
       FListView := Value;
  end;
end; // TmcmShellComboBox.SetListView.


procedure TmcmShellComboBox.Notification(AComponent : TComponent;
                                         Operation  : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove)
  then begin
       if (AComponent = FListView)
       then FListView := Nil
       else ;
  end;
end; // TmcmShellComboBox.Notification.


procedure TmcmShellComboBox.CreateImages;
var hImage   : integer;
    FileInfo : TSHFileInfoW;
    Path     : WideString;
begin
  if Assigned(FImageList)
  then FImageList.Free;
  FImageList := TImageList.Create(self);
  if Assigned(FImageList)
  then begin
       Path := '.\';
       hImage := SHGetFileInfoW(PChar(Pointer(Path)), 0, FileInfo, SizeOf(TSHFileInfoW),
                                SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
       if (hImage <> 0)
       then begin
            FImageList.Handle := hImage;
            FImageList.ShareImages := True;
       end;
  end;
end; // TmcmShellComboBox.CreateImages.


function TmcmShellComboBox.IndexFromID(AbsoluteID : PItemIDList) : integer;
var DesktopFolder : IShellFolder;
begin
  Result := Items.Count - 1;
  DesktopFolder := DesktopShellFolder;
  while (Result >= 0)
  do begin
     if Assigned(Items.Objects[Result])
     then if (DesktopFolder.CompareIDs(0, AbsoluteID, TmcmShellFolder(Items.Objects[Result]).AbsoluteID) = 0)
          then Exit;
     dec(Result);
  end;
end; // TmcmShellComboBox.IndexFromID.


procedure TmcmShellComboBox.SetPathFromID(ID : PItemIDList);
var PIDLs   : TList;
    i       : integer;
    Item    : integer;
    Temp    : integer;
    AFolder : TmcmShellFolder;
    RelID   : PItemIDList;
begin
  if FUpdating
  then Exit;
  Item := -1;
  Items.BeginUpdate;
  try
    Init;
    PIDLs := CreatePIDLList(ID);
    try
      i := PIDLs.Count - 1;
      while (i >= 0)
      do begin
         Item := IndexFromID(PIDLs[i]);
         if (Item <> -1)
         then Break;
         dec(i);
      end;

      if (i < 0)
      then Exit;

      while (i < PIDLs.Count - 1)
      do begin
         inc(i);
         RelID := RelativeFromAbsolute(PIDLs[i]);
         AFolder := InitItem(TmcmShellFolder(Items.Objects[Item]), RelID);
         Items.InsertObject(Item + 1, AFolder.DisplayName, AFolder);
         inc(Item);
      end;

      Temp := IndexFromID(ID);
      if (Temp < 0)
      then begin
           RelID := RelativeFromAbsolute(ID);
           AFolder := InitItem(TmcmShellFolder(Items.Objects[Item]), RelID);
           Temp := Item + 1;
           Items.InsertObject(Item + 1, AFolder.DisplayName, AFolder);
      end;
      ItemIndex := Temp;
    finally
      DestroyPIDLList(PIDLs);
    end;
  finally
    Items.EndUpdate;
  end;
  Change;
end; // TmcmShellComboBox.SetPathFromID.


function TmcmShellComboBox.AddDesktop : TmcmShellFolder;
var ShellFolder  : IShellFolder;
    FDesktopPIDL : PItemIDList;
    FolderName   : WideString;
    AFolder      : TmcmShellFolder;
begin
  // Get Desktop folder.
  FDesktopPIDL := DesktopPIDL;
  try
    ShellFolder := GetIShellFolder(DesktopShellFolder, FDesktopPIDL, 0);
    if (ShellFolder = Nil)
    then ShellFolder := DesktopShellFolder;

    // Create TmcmShellFolder manager object for a IShellFolder object.
    AFolder := TmcmShellFolder.Create(Nil, FDesktopPIDL, ShellFolder);
    FolderName := AFolder.DisplayName;
    FFolders.Add(AFolder);
    Items.AddObject(FolderName, AFolder);
  finally
    DisposePIDL(FDesktopPIDL);
  end;
  Result := AFolder;
end;  // TmcmShellComboBox.AddDesktop.


var CompareFolder : TmcmShellFolder = Nil;

function ComboSortFunc(Item1, Item2 : Pointer): integer;
begin
  Result := 0;
  if (CompareFolder = Nil)
  then Exit;
  Result := SmallInt(CompareFolder.ShellFolder.CompareIDs(0, PItemIDList(Item1), PItemIDList(Item2)));
end; // ComboSortFunc.


function TmcmShellComboBox.InitItem(ParentFolder : TmcmShellFolder; ID : PItemIDList) : TmcmShellFolder;
var ShellFolder : IShellFolder;
begin
  ShellFolder := GetIShellFolder(ParentFolder.ShellFolder, ID, 0);
  Result := TmcmShellFolder.Create(ParentFolder, ID, ShellFolder);
end; // TmcmShellComboBox.InitItem.


procedure TmcmShellComboBox.AddItems(Index : integer; ParentFolder : TmcmShellFolder);
var EnumList      : IEnumIDList;
    ID            : PItemIDList;
    i             : integer;
    IDList        : TList;
    FolderName    : WideString;
    AFolder       : TmcmShellFolder;
    Flags         : integer;
    {$IFDEF DCB3}
     pCeltPrefetch : integer;
    {$ELSE}
     pCeltPrefetch : Cardinal;
    {$ENDIF}
begin
  Flags := SHCONTF_FOLDERS;// or SHCONTF_SHAREABLE; // or SHCONTF_INCLUDEHIDDEN
  OLECheck(ParentFolder.ShellFolder.EnumObjects(Application.Handle, Flags, EnumList));
  CompareFolder := ParentFolder;
  Items.BeginUpdate;

  IDList := TList.Create;
  try
    pCeltPrefetch := 0;
    while (EnumList.Next(1, ID, pCeltPrefetch) = S_OK)
    do IDList.Add(ID);

    IDList.Sort(ComboSortFunc);

    for i := 0 to (IDList.Count - 1)
    do begin
       ID := IDList.Items[i];
       AFolder := InitItem(ParentFolder, ID);
       DisposePIDL(ID);
       ID := Nil;
       IDList.Items[i] := Nil;

       if Assigned(AFolder)
       then begin
            FolderName := AFolder.DisplayName;
            FFolders.Add(AFolder);
            Items.InsertObject(Index + i + 1, AFolder.DisplayName, AFolder);
       end;
    end;
  finally
    CompareFolder := Nil;
    IDList.Free;
    Items.EndUpdate;
  end;
end; // TmcmShellComboBox.AddItems.


procedure TmcmShellComboBox.Init;
var AParent      : TmcmShellFolder;
    Index        : integer;
    MyComputer   : PItemIDList;
begin
  Items.BeginUpdate;
  ClearItems;
  ItemIndex := -1;
  if (csDesigning in ComponentState) or Visible
  then begin
       AParent := AddDesktop;
       AddItems(0, AParent);
       SHGetSpecialFolderLocation(0, CSIDL_DRIVES, MyComputer);
       Index := IndexFromID(MyComputer);
       if (Index <> -1)
       then AddItems(Index, TmcmShellFolder(Items.Objects[Index]));
       ItemIndex := 0;
  end;
  Items.EndUpdate;
end; // TmcmShellComboBox.Init.


procedure TmcmShellComboBox.Change;
var Node : TmcmShellFolder;
begin
  if (ItemIndex > -1) and Not(FUpdating) and Not(DroppedDown)
  then begin
       FUpdating := True;
       try
         Node := TmcmShellFolder(Items.Objects[ItemIndex]);
         if Assigned(Node)
         then begin
              {
              if Assigned(FTreeView)
              then FTreeView.SetPathFromID(Node.AbsoluteID);
              }
              if Assigned(FListView)
              then FListView.SetPathFromID(Node.AbsoluteID);
         end;
       finally
         FUpdating := False;
       end;
  end;
  inherited Change;
  if Assigned(FOnChange)
  then FOnChange(Self);
end; // TmcmShellComboBox.Change.


procedure TmcmShellComboBox.Click;
var Temp : PItemIDList;
begin
  FUpdating := True;
  try
    Temp := CopyPIDL(TmcmShellFolder(Items.Objects[ItemIndex]).RelativeID);
    // Folder will destroy when removing the lower level Shell folders.
    try
      SetPathFromID(Temp);
      inherited Click;
    finally
      DisposePIDL(Temp);
    end;
  finally
    FUpdating := False;
  end;
end; // TmcmShellComboBox.Click.


procedure TmcmShellComboBox.DrawItem(Index : integer;
                                     Rect  : TRect;
                                     State : TOwnerDrawState);
var FileInfo : TSHFileInfoW;
    Tab      : integer;
    Mode     : integer;
    yOfs     : integer;
    Folder   : TmcmShellFolder;
    Count    : integer;
    DispStr  : WideString;
begin
  inherited DrawItem(Index, Rect, State);

  Canvas.FillRect(Rect);
  if (Items.Objects[Index] <> Nil)
  then begin
       Folder := TmcmShellFolder(Items.Objects[Index]);
       if Assigned(Folder)
       then begin
            Mode := SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME;
            if (odSelected in State)
            then Mode := Mode or SHGFI_OPENICON;
            SHGetFileInfoW(PChar(Folder.AbsoluteID), 0, FileInfo, SizeOf(TSHFileInfoW), Mode or SHGFI_PIDL);

            if Not(DroppedDown)
            then Tab := 0
            else Tab := Folder.Level * 8;

            DispStr := FileInfo.szDisplayName;
            Count := Length(DispStr);
            ExtTextOutW(Canvas.Handle, Rect.left + 20 + Tab, Rect.top + 2,
                        ETO_OPAQUE, Nil, FileInfo.szDisplayName, Count, Nil);
            // Canvas.TextOut (Rect.left + 20 + Tab, Rect.top + 2, FileInfo.szDisplayName);

            FImageList.BkColor := clWhite;
            FImageList.BlendColor := FImageList.BkColor;
            FImageList.DrawingStyle := dsTransparent;
            if odselected in state
            then begin
                 FImageList.BlendColor := clActiveCaption; // clHighlight
                 FImageList.DrawingStyle := dsTransparent;
            end;

            yOfs := ((Rect.Bottom - Rect.Top) - FImageList.Height) div 2;
            FImageList.Draw(Canvas, Rect.left + 2 + Tab, Rect.top + yOfs, FileInfo.iIcon);
            FImageList.DrawingStyle := dsNormal;
       end;
  end;
end; // TmcmShellComboBox.DrawItem.


//------------------------------------------------------------------------------
// TmcmShellListView
//------------------------------------------------------------------------------

constructor TmcmShellListView.Create(AOwner : TComponent);
var FileInfo : TSHFileInfoW;
    Path     : WideString;
begin
  inherited Create(AOwner);
  ParentFont := False;
  FRoot      := Nil;
  FFolders   := TList.Create;
  FUpdating  := False;
  FOnFileChange := Nil;

  OnCompare      := CompareFiles;
  OnColumnClick  := ColumnClicked;

  FSelectedFiles := TStringList.Create;
  FFileMask      := '';
  FFileType      := [];
  FileType       := [mftFolders, mftNonFolders];
  FFileOpMode    := -1;

  FSortForward   := True;
  FSortColumn    := 0;

  HideSelection  := False;
  MultiSelect    := False;
  ReadOnly       := False;
  ViewStyle      := vsList;
  FAutoContext   := True;

  Path := '.\';
  FSmallImages := SHGetFileInfoW(PChar(Pointer(Path)), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  FLargeImages := SHGetFileInfoW(PChar(Pointer(Path)), 0, FileInfo, SizeOf(FileInfo), SHGFI_SYSICONINDEX or SHGFI_LARGEICON);
end; // TmcmShellListView.Create.


destructor TmcmShellListView.Destroy;
begin
  ClearItems;
  if Assigned(FFolders)
  then FFolders.Free;
  if Assigned(FRoot)
  then FRoot.Free;
  if Assigned(FSelectedFiles)
  then FSelectedFiles.Free;
  inherited Destroy;
end; // TmcmShellListView.Destroy.


procedure TmcmShellListView.CreateWnd;
begin
  inherited CreateWnd;

  if HandleAllocated
  then begin
       if (FSmallImages <> 0)
       then SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_SMALL, FSmallImages);
       if (FLargeImages <> 0)
       then SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_NORMAL, FLargeImages);
  end;
  (*
  FLargeImages.BkColor := clWhite;
  FLargeImages.BlendColor := FLargeImages.BkColor;
  FLargeImages.DrawingStyle := dsTransparent;
  *)

  Font.Size := 8;
  Font.Name := 'MS Sans Serif';
  if (Columns.Count = 0)
  then begin
       with Columns.Add
       do begin
          Caption := resName;
          Width := 140;
       end;
       with Columns.Add
       do begin
          Caption := resSize;
          Width := 70;
          Alignment := taRightJustify;
       end;
       with Columns.Add
       do begin
          Caption := resType;
          Width := 90;
       end;
       with Columns.Add
       do begin
          Caption := resModified;
          Width := 100;
       end;
       with Columns.Add
       do begin
          Caption := resAttributes;
       end;
  end;
end; // TmcmShellListView.CreateWnd.


procedure TmcmShellListView.DestroyWnd;
begin
 // ClearItems;
  inherited DestroyWnd;
end; // TmcmShellListView.DestroyWnd.


procedure TmcmShellListView.ClearItems;
var i : integer;
begin
  if Not(csDestroying in ComponentState)
  then Items.BeginUpdate;

  for i := 0 to (Items.Count - 1)
  do begin
     Items[i].ImageIndex := -1;
     Items[i].SubItems.Clear;
  end;

  for i := 0 to (Items.Count - 1)
  do Items[i].Data := Nil;
  Items.Clear;

  if Assigned(FFolders)
  then begin
       for i := (FFolders.Count - 1) downto 0
       do begin
          if Assigned(FFolders[i])
          then begin
               TmcmShellFolder(FFolders[i]).Free;
               FFolders[i] := Nil;
          end;
       end;
       FFolders.Clear;
  end;

  if Not(csDestroying in ComponentState)
  then begin
       Items.Clear;
       Items.EndUpdate;
  end;
end; // TmcmShellListView.ClearItems.


procedure TmcmShellListView.Clear;
begin
  ClearItems;
  {$IFNDEF DCB3_5}
  Inherited Clear;
  {$ENDIF}
end; // TmcmShellListView.Clear.


procedure TmcmShellListView.CommandCompleted(Verb : WideString; Succeeded : boolean);
begin
  if Succeeded
  then begin
       if (Verb = SCmdVerbDelete) or (Verb = SCmdVerbPaste)
       then Refresh
       else if (Verb = SCmdVerbOpen)
            then SetCurrentDirectoryW(PWideChar(FSavePath));
  end;
end; // TmcmShellListView.CommandCompleted.


procedure TmcmShellListView.ExecuteCommand(Verb : WideString; var Handled : boolean);
var lPath : array[0..MAX_PATH] of char;
begin
  if (Verb = SCmdVerbRename)
  then begin
       // Go into edit mode
       SendMessage(Handle, LVM_EDITLABEL, Selected.Index, 0);
       Handled := True;
  end
  else if (Verb = SCmdVerbOpen)
       then begin
            GetCurrentDirectory(MAX_PATH, lPath);
            FSavePath := StrPas(lPath);
            StrPCopy(lPath, ExtractFilePath(TmcmShellFolder(Selected.Data).Path));
            SetCurrentDirectory(lPath);
       end;
end; // TmcmShellListView.ExecuteCommand.


{$IFNDEF DCB3_4}

procedure TmcmShellListView.DoContextPopup(MousePos : TPoint; var Handled : Boolean);
var AFolder : TmcmShellFolder;
    i       : integer;
begin
  AFolder := Nil;
  i := Index;
  if (i >= 0)
  then AFolder := TmcmShellFolder(Selected.Data);
  if FAutoContext and (AFolder <> Nil)
  then begin
       InvokeContextMenu(Self, AFolder, MousePos.X, MousePos.Y);
       Handled := True;
  end
  else inherited;
end; // TmcmShellListView.DoContextPopup.
{$ENDIF}


procedure TmcmShellListView.Edit(const Item : TLVItem);
var NewName : WideString;
begin
  with Item
  do begin
     if (iItem < FFolders.Count)
     then begin
          if (pszText <> Nil)
          then begin
               NewName := pszText;
               if TmcmShellFolder(Items[iItem].Data).Rename(NewName)
               then Items[iItem].Caption := NewName;
               ListView_RedrawItems(Handle, iItem, iItem);
          end;
     end;
  end;
end; // TmcmShellListView.Edit.


procedure TmcmShellListView.SetShellComboBox(Value : TmcmShellComboBox);
begin
  if (FShellComboBox <> Value)
  then begin
       if (Value <> Nil)
       then begin
            Value.Path := GetPath;
            Value.FListView := Self;
       end;
       if (FShellComboBox <> Nil)
       then FShellComboBox.FListView := Nil;
       if (FShellComboBox <> Nil)
       then FShellComboBox.FreeNotification(Self);
       FShellComboBox := Value;
  end;
end; // TmcmShellListView.SetShellComboBox.


procedure TmcmShellListView.Notification(AComponent : TComponent;
                                         Operation  : TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove)
  then begin
       if (AComponent = FShellComboBox)
       then FShellComboBox := Nil
       else ;
  end;
end; // TmcmShellListView.Notification.


function TmcmShellListView.CreateRootFromPIDL(ID : PItemIDList) : TmcmShellFolder;
var ShellFolder : IShellFolder;
begin
  ShellFolder := GetIShellFolder(DesktopShellFolder, ID, 0);
  if (ShellFolder = Nil)
  then ShellFolder := DesktopShellFolder;
  Result := TmcmShellFolder.Create(Nil, ID, ShellFolder);
end; // TmcmShellListView.CreateRootFromPIDL.


procedure TmcmShellListView.SetPathFromID(ID : PItemIDList);
begin
  if FUpdating or (Assigned(FRoot) and SamePIDL(FRoot.AbsoluteID, ID))
  then Exit;
  if Not(csLoading in ComponentState)
  then if IsEditing
       then Exit;

  if Assigned(FRoot)
  then FRoot.Free;
  FRoot := CreateRootFromPIDL(ID);
  RootChanged;
end; // TmcmShellListView.SetPathFromID.


procedure TmcmShellListView.RootChanged;
begin
  if FUpdating
  then Exit;
  FUpdating := True;
  try
    UpdateView;
    if (FShellComboBox <> Nil)
    then FShellComboBox.SetPathFromID(FRoot.AbsoluteID);
  finally
    FUpdating := False;
  end;
  SetFileName(AddSlash(FRoot.Path));
end; // TmcmShellListView.RootChanged.


function ListSortFunc(Item1, Item2 : Pointer) : integer;
const R : array[boolean] of byte = (0, 1);
begin
  Result := 0;
  if (Item1 = Nil) or (Item2 = Nil)
  then Exit;
  Result := R[TmcmShellFolder(Item2).IsFolder] - R[TmcmShellFolder(Item1).IsFolder];
  if (Result = 0) and (TmcmShellFolder(Item1).ParentShellFolder <> nil)
  then Result := smallint(TmcmShellFolder(Item1).ParentShellFolder.CompareIDs(0,
                 TmcmShellFolder(Item1).RelativeID,
                 TmcmShellFolder(Item2).RelativeID));
end; // ListSortFunc.


procedure TmcmShellListView.UpdateView;
var ID         : PItemIDList;
    EnumList   : IEnumIDList;
    NumIDs     : dword;
    SaveCursor : TCursor;
    HR         : HResult;
    CanAdd     : Boolean;
    NewFolder  : IShellFolder;
    i, Count   : integer;
    AFolder    : TmcmShellFolder;
    Extension  : WideString;
    NewItem    : TListItem;
    SizeStr    : WideString;
    iSize      : longint;
begin
  if Not(Assigned(FRoot)) or (csLoading in ComponentState) 
     {$IFNDEF DCB_UNIC} or Not(HandleAllocated) {$ENDIF}
  then Exit;

  if Not(csLoading in ComponentState)
  then if IsEditing
       then Exit;

  Items.BeginUpdate;
  try
    ClearItems;
    Count := 0;
    SaveCursor := Screen.Cursor;
    try
      Screen.Cursor := crHourglass;
      if (FFileMask <> '')
      then begin
           HR := FRoot.ShellFolder.EnumObjects(Application.Handle, FObjectFlags, EnumList);
           if (HR = 0)
           then begin
                while (EnumList.Next(1, ID, NumIDs) = S_OK)
                do begin
                   NewFolder := GetIShellFolder(FRoot.ShellFolder, ID, 0);
                   AFolder := TmcmShellFolder.Create(FRoot, ID, NewFolder);
                   DisposePIDL(ID);
                   ID := Nil;

                   // Determine if the item should be added to the list.
                   CanAdd := False;
                   if (Pos(WideString('*.*'), FFileMask) > 0)
                   then begin
                        if (AFolder.DisplayName <> '')
                        then CanAdd := True;
                   end
                   else begin
                        if Not(AFolder.IsFolder)
                        then begin
                             Extension := lowerCase(ExtractFileExt(AFolder.GetPath)) + ';';
                             if (Pos(Extension, FFileMask) > 0) and (Extension <> ';')
                             then CanAdd := True;
                        end
                        else CanAdd := True;
                   end;
                   if CanAdd
                   then begin
                        inc(Count);
                        FFolders.Add(AFolder);
                   end
                   else AFolder.Free;
                end;

                // Sort files/folders
                CompareFolder := FRoot;
                try
                  FFolders.Sort(@ListSortFunc);
                finally
                  CompareFolder := Nil;
                end;
           end;

           // Get File/Folder information and add to item columns.
           for i := 0 to (Count - 1)
           do begin
              AFolder := TmcmShellFolder(FFolders.Items[i]);
              NewItem := Items.Add;
              with NewItem
              do begin
                 ImageIndex := AFolder.GetImageIndex;
                 Data := AFolder;
                 with AFolder
                 do begin
                    Caption := AFolder.DisplayName;

                    AFolder.GetDetails;
                    SubItems.Clear;
                    if AFolder.IsFolder
                    then SubItems.Add('')
                    else begin
                         SizeStr := '';
                         iSize := Size;
                         if (iSize > 1073741824)
                         then begin
                              iSize := iSize div 1073741824;
                              SizeStr := resGigaBytes;
                         end;
                         if (iSize > 1048576)
                         then begin
                              iSize := iSize div 1048576;
                              SizeStr := resMegaBytes;
                         end;
                         if (iSize > 1024)
                         then begin
                              iSize := iSize div 1024;
                              SizeStr := resKiloBytes;
                         end;
                         SubItems.Add(Trim(IntToStr(iSize) + ' ' + SizeStr));
                    end;
                    SubItems.Add(TypeName);
                    SubItems.Add(Date);
                    SubItems.Add(Attributes);
                 end;
              end;
           end;
      end;
    finally
      Screen.Cursor := SaveCursor;
    end;
  finally
    Items.EndUpdate;
  end;
end; // TmcmShellListView.UpdateView.


function TmcmShellListView.GetCount : integer;
begin
  Result := Items.Count;
end; // TmcmShellListView.GetCount.


function TmcmShellListView.GetFolder(Index : integer) : TmcmShellFolder;
begin
  if (0 <= Index) and (Index < Items.Count)
  then Result := TmcmShellFolder(Items.Item[Index].Data)
  else Result := Nil;
end; // TmcmShellListView.GetFolder.


function TmcmShellListView.GetIndex : integer;
begin
  if (Selected <> Nil)
  then Result := GetItemIndex(Selected)
  else Result := -1;
end; // TmcmShellListView.GetIndex.


procedure TmcmShellListView.SetIndex(Value : integer);
begin
  if (0 <= Value) and (Value < Items.Count)
  then Selected := Items.Item[Value];
end; // TmcmShellListView.SetIndex.


function TmcmShellListView.GetPath : WideString;
begin
  Result := '';
  if (Count > 0) and (Selected <> Nil)
  then begin
       if (Selected.Data <> Nil)
       then begin
            if TmcmShellFolder(Selected.Data).IsFolder
            then Result := AddSlash(TmcmShellFolder(Selected.Data).Path)
            else Result := AddSlash(ExtractFilePath(TmcmShellFolder(Selected.Data).Path));
       end;
  end
  else begin
       if Assigned(FRoot)
       then Result := AddSlash(FRoot.Path);
  end;
end; // TmcmShellListView.GetPath.


procedure TmcmShellListView.SetPath(NewPath : WideString);
var P        : PWideChar;
    NewPIDL  : PItemIDList;
    Flags    : dword;
    NumChars : dword;
begin
  if FUpdating
  then Exit;
  if {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(NewPath) //and HandleAllocated
  then begin
       NewPath := AddSlash(NewPath);
       NumChars := Length(NewPath);
       Flags := 0;
       P := StringToOleStr(NewPath);
       try
         OLECheck(DesktopShellFolder.ParseDisplayName(0, Nil, P, NumChars, NewPIDL, Flags));
         if Assigned(NewPIDL)
         then SetPathFromID(NewPIDL);
         DisposePIDL(NewPIDL);
         NewPIDL := Nil;
       except
         on EOleSysError
         do raise EmcmInvalidPath.CreateFmt(resErrorSettingPath, [NewPath]);
       end;
  end
  else begin
       ClearItems;
       if (NewPath = '')
       then if Assigned(FRoot)
            then begin
                 FRoot.Free;
                 FRoot := Nil;
            end;
  end;
end; // TmcmShellListView.SetPath.


procedure TmcmShellListView.SetFileMask(const Masks : WideString);
begin
  if (FFileMask <> Masks)
  then begin
       FFileMask := Masks;
       if (FFileMask[Length(FFileMask)] <> ';')
       then FFileMask := FFileMask + ';';
       UpdateView;
  end;
end; // TmcmShellListView.SetFileMask.


procedure TmcmShellListView.SetFileType(NewFileType : TmcmFileTypes);
begin
  if (NewFileType <> FFileType)
  then begin
       FFileType := NewFileType;
       FObjectFlags := 0;
       if (mftFolders in FFileType)
       then FObjectFlags := FObjectFlags or SHCONTF_FOLDERS;
       if (mftNonFolders in FFileType)
       then FObjectFlags := FObjectFlags or SHCONTF_NONFOLDERS;
       if (mftHidden in FFileType)
       then FObjectFlags := FObjectFlags or SHCONTF_INCLUDEHIDDEN;
       UpdateView;
  end;
end; // TmcmShellListView.SetFileType.


function TmcmShellListView.GetFileName : WideString;
begin
  Result := FFileName;
end; // TmcmShellListView.GetFileName.


procedure TmcmShellListView.SetFileName(NewFile : WideString);
begin
  if (FFileName <> NewFile)
  then begin
       FFileName := NewFile;
       if Assigned(FOnFileChange)
       then FOnFileChange(Self, FFileName);
  end;
end; // TmcmShellListView.SetFileName.


function TmcmShellListView.GetSelectedCount : integer;
begin
  Result := inherited SelCount;
end; // TmcmShellListView.GetSelectedCount.


procedure TmcmShellListView.Back;
var RootPIDL : PItemIDList;
begin
  RootPIDL := CopyPIDL(FRoot.RelativeID);
  try
    StripLastID(RootPIDL);
    SetPathFromID(RootPIDL);
  finally
    DisposePIDL(RootPIDL);
  end;
end; // TmcmShellListView.Back.


procedure TmcmShellListView.OneLevelUp;
var RootPIDL : PItemIDList;
begin
  RootPIDL := CopyPIDL(FRoot.RelativeID);
  try
    StripLastID(RootPIDL);
    SetPathFromID(RootPIDL);
  finally
    DisposePIDL(RootPIDL);
  end;
end; // TmcmShellListView.OneLevelUp.


procedure TmcmShellListView.CompareFiles(    Sender  : TObject;
                                             Item1   : TListItem;
                                             Item2   : TListItem;
                                             Data    : integer;
                                         var Compare : integer);
var Caption1 : WideString;
    Caption2 : WideString;
    Size1    : integer;
    Size2    : integer;
    Date1    : TDateTime;
    Date2    : TDateTime;
    Result   : integer;
begin
  Result := 0;

  if (Item1.SubItems[0] = '') and (Item2.SubItems[0] <> '')
  then Result := -1
  else if (Item1.SubItems[0] <> '') and (Item2.SubItems[0] = '')
       then Result := 1
       else begin
            case FSortColumn of
            0 : begin
                  Caption1 := AnsiUpperCase(Item1.Caption);
                  Caption2 := AnsiUpperCase(Item2.Caption);
                  if (Caption1 > Caption2)
                  then Result := 1
                  else if (Caption1 < Caption2)
                       then Result := -1;
                end;
            1 : begin
                  if (Item1.SubItems.Strings[0] = '') or
                     (Item2.SubItems.Strings[0] = '')
                  then begin
                       if (Item1.SubItems.Strings[0] = '')
                       then begin
                            if (Item2.SubItems.Strings[0] = '')
                            then Result := 0
                            else Result := -1;
                       end
                       else if (Item2.SubItems.Strings[0] = '')
                            then Result := 1;
                  end
                  else begin
                       Size1 := TmcmShellFolder(Item1.Data).Size;
                       Size2 := TmcmShellFolder(Item2.Data).Size;
                       if (Size1 > Size2)
                       then Result := 1
                       else if (Size1 < Size2)
                            then Result := -1;
                  end;
                end;
            3 : begin
                  Date1 := StrToDateTime(Item1.SubItems.Strings[2]);
                  Date2 := StrToDateTime(Item2.SubItems.Strings[2]);
                  Result := Round(1E5 * (Date1 - Date2));
                end;
            else begin
                 Result := CompareText(Item1.SubItems.Strings[FSortColumn - 1],
				       Item2.SubItems.Strings[FSortColumn - 1]);
            end;
            end;
       end;
  if FSortForward
  then Compare := -Result
  else Compare := Result;
end; // TmcmShellListView.CompareFiles.


procedure TmcmShellListView.ColumnClicked(Sender : TObject; Column : TListColumn);
var RequiredColumn : integer;
begin
  RequiredColumn := Column.Index;
  if (RequiredColumn = FSortColumn)
  then FSortForward := Not(FSortForward)
  else begin
       FSortColumn := RequiredColumn;
       FSortForward := True;
  end;
  SortType := stData;
  SortType := stNone;
end; // TmcmShellListView.ColumnClicked.


procedure TmcmShellListView.Click;
begin
  if (Selected <> Nil)
  then if (Selected.Data <> Nil)
       then if (FileExists(TmcmShellFolder(Selected.Data).Path))
            then SetFileName(TmcmShellFolder(Selected.Data).Path);
  inherited Click;
end; // TmcmShellListView.Click.


procedure TmcmShellListView.DblClick;
var WidePath : WideString;
    APath    : string;
    Count    : integer;
begin
  if (Selected <> Nil)
  then with TmcmShellFolder(Selected.Data)
       do if IsFolder
          then SetPathFromID(AbsoluteID)
          else if FAllowExecute
               then begin
                    APath := ExtractFilePath(FFileName);
                    Count := Length(APath);
                    SetLength(WidePath, Count);
                    StringToWideChar(APath, PWideChar(WidePath), Count);
                    ShellExecuteW(Handle, nil, PWideChar(FFileName), nil, PWideChar(WidePath), 0); // unicode
               end;
  inherited DblClick;
end; // TmcmShellListView.DblClick.


procedure TmcmShellListView.KeyDown(var Key : Word; Shift : TShiftState);
begin
  case Key of
  {
  // Ctrl + A, Select all
  // Ctrl + C, Copy
  $43 : if (Shift = [ssCtrl])
        then if (Selected <> Nil)
             then CutCopy(0);
  // Ctrl + X, Cut
  $58 : if (Shift = [ssCtrl])
        then if (Selected <> Nil)
             then CutCopy(2);
  // Ctrl + V, Paste
  $50 : if (Shift = [ssCtrl])
        then if (FSelectedFiles.Count <> 0)
             then Paste;
  VK_DELETE : if (Selected <> Nil)
              then DeleteFiles;
  }
  // Open
  VK_RETURN : if (Selected <> Nil) and Not(IsEditing)
              then DblClick;
  VK_F5     : UpdateView;
  end;
  inherited KeyDown(Key, Shift);
end; // TmcmShellListView.KeyDown.


procedure TmcmShellListView.KeyUp(var Key : Word; Shift : TShiftState);
begin
  // Ensures that Filename is updated when navigating using keyboard keys.
  inherited KeyUp(Key, Shift);
  case Key of
  VK_RETURN : if IsEditing
              then ; //Self.Edit();
  VK_UP,
  VK_DOWN,
  VK_LEFT,
  VK_RIGHT,
  VK_PRIOR,
  VK_NEXT,
  VK_END,
  VK_HOME : if (Selected <> Nil)
            then if (Selected.Data <> Nil)
                 then if (FileExists(TmcmShellFolder(Selected.Data).Path))
                      then SetFileName(TmcmShellFolder(Selected.Data).Path);
  else // Handle alpha & Numeric key presses.
       if (Selected <> Nil)
       then if (Selected.Data <> Nil)
            then if (FileExists(TmcmShellFolder(Selected.Data).Path))
                 then SetFileName(TmcmShellFolder(Selected.Data).Path);
  end;
end; // TmcmShellListView.KeyUp.


procedure TmcmShellListView.WndProc(var Message : TMessage);
begin
  //to handle submenus of context menus.
  with Message
  do if ((Msg = WM_INITMENUPOPUP) or
         (Msg = WM_DRAWITEM) or
         (Msg = WM_MENUCHAR) or
         (Msg = WM_MEASUREITEM)) and
         Assigned(ICM2)
     then begin
          {$IFDEF DCB3}
            ICM2.HandleMenuMsg(Msg);
          {$ELSE}
            ICM2.HandleMenuMsg(Msg, wParam, lParam);
          {$ENDIF}
          Result := 0;
     end;
  inherited;
end; // TmcmShellListView.WndProc.


procedure TmcmShellListView.NewFolder;
var NewName : String; // unicode
    NewDir  : WideString;
begin
  if ReadOnly
  then Exit;
  repeat
    NewName := '';
    NewDir  := '';
    if not InputQuery(resNewFolder, resEnterNewFolder, NewName)
    then Exit;
    NewDir := GetPath + '\' + NewName;
    if {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(NewDir)
    then ShowMessage(resFolder + ' "' + NewName + '" ' + resAlreadyExist + '!');
  until {$IFDEF GE_DXE2} System.SysUtils.{$ENDIF}DirectoryExists(NewDir) = False;
  CreateDir(NewDir);
  UpdateView;
end; // TmcmShellListView.NewFolder.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

initialization
  CoInitialize(Nil);           
end.

