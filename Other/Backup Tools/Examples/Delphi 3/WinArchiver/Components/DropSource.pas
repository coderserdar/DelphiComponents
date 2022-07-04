UNIT dropsource;
  
  // -----------------------------------------------------------------------------
  // Project:         Drag and Drop Source Components
  // Component Names: TDropTextSource, TDropFileSource, TDropURLSource 
  // Module:          DropSource
  // Description:     Implements Dragging & Dropping of text, files and URLs
  //                  FROM your application to another.
  // Version:	        3.1
  // Date:            01-OCT-1998
  // Target:          Win32, Delphi 3 & 4
  // Authors:         Angus Johnson,   ajohnson@rpi.net.au
  //                  Anders Melander, anders@melander.dk
  //                                   http://www.melander.dk
  //                  Graham Wideman,  graham@sdsu.edu
  //                                   http://www.wideman-one.com
  // Copyright        ©1998 Angus Johnson, Anders Melander & Graham Wideman
  // -----------------------------------------------------------------------------
  // You are free to use this source but please give us credit for our work.
  // if you make improvements or derive new components from this code,
  // we would very much like to see your improvements. FEEDBACK IS WELCOME.
  // -----------------------------------------------------------------------------

  // Acknowledgements:
  // 1. Thanks to Jim O'Brien for his tips on Shortcuts and Scrap files. We
  //    were on the right path afterall.
  // 2. Thanks to Zbysek Hlinka for sugestions on Copying to Clipboard.
  // -----------------------------------------------------------------------------

  // History:
  // dd/mm/yy  Version  Changes
  // --------  -------  ----------------------------------------
  // 01.10.98  3.1      * Removed the "AutoDrag" feature introduced in Version 3.0.
  //                      This feature only seems possible if TDropSource hooks
  //                      the Source TWinControl's message handler. Although
  //                      it works it was potentially dangerous when hooking is implemented
  //                      from within a component (see DropTarget module history).
  //                      The component user is now responsible for calling the
  //                      TDropSource descendant's Execute method by implementing
  //                      a couple of lines of code in the Source TWinControl's 
  //                      OnMouseDown and OnMouseMove events. (See demo.)
  //                    * Added TDropURLSource component.
  //                    * General code tidy up.
  // 22.09.98  3.0      * Shortcuts (links) for TDropFileSource now enabled.
  //                    * Scrap files for TDropTextSource now enabled.
  //                    * DropSource property added to implement autodrag feature.
  //                    * New events - OnStartDrag and OnEndDrag added (See demo).
  //                      OnStartDrag is called automatically if DropSource
  //                      Control is assigned otherwise use Execute function to
  //                      manually initiate a drag op.
  //                    * TDropSource.DoEnumFormatEtc() no longer declared abstract.
  // 08.09.98  2.0      * No significant changes to this module
  //                      but the version was updated to coincide with the
  //                      new DropTarget module included with this demo.
  // 31.08.98  1.5      * Fixed a Delphi 4 bug!
  //                      (I cut and pasted the wrong line!)
  //                    * Demo code now MUCH tidier and easier to read (I think).
  // 19.08.98  1.4      * CopyToClipboard method added.
  //                    * Should now compile in Delphi 4. 
  //                    * Another tidy up of the code.
  // 21.07.98  1.3      * Fixed a bug in Ver 1.2 where OnDrop event was never called.
  //                    * Now able to drag text to WordPad.
  //                    * Added OnFeedback Event.
  //                    * Added dtLink to TDragType enumeration but
  //                      still not able to get it to work.
  //                    * Code tidy up.
  // 19.07.98  1.2      * Changed TDragType enumeration type to a
  //                      TDragTypes set (AM's suggestion) ready to
  //                      implement dtLink later.
  //                    * Added known bugs to header and demo.
  // 17.07.98  1.1      * Reenabled end-user option to select either
  //                      Copy or Move operation while dragging.
  // 15.07.98  1.0      * Initial Delphi 3 component implementation of AM's
  //                      DropSource unit. I released a Delphi 2 D'n'D
  //                      component (TDragFilesSrc) over 12 months ago. However
  //                      with the significant changes in COM implementations
  //                      between Delphi versions I decided to use AM's code
  //                      as the springboard for my new Delphi 3 D'n'D components.
  //                      Thanks to Anders for the excellent start and
  //                      suggestions along the way!
  // -----------------------------------------------------------------------------

  // BASIC USAGE: (See demo for more detailed examples)
  // 1. Add this non-visual component to the form you wish to drag FROM.
  // 2. Add DragPoint (TPoint) variable to the Form declaration;
  // 3. In the Source TWinControl MouseDown method add ...
  //      DragPoint := Point(X,Y);
  // 4. In the Source TWinControl MouseMove method add ...
  //      if ((Shift <> [ssLeft]) and (Shift <> [ssRight])) or
  //      ((abs(DragPoint.X - X) <10) and (abs(DragPoint.Y - Y) <10)) then exit;
  //      //Add data to TDropSource descendant (eg:) ...
  //      DropURLSource1.URL := Edit1.text;
  //      //Initiate drag (eg:) ...
  //      DropURLSource1.Execute;
  // -----------------------------------------------------------------------------

INTERFACE
  USES
    Controls, Windows, ActiveX, Classes, ShlObj, SysUtils, ClipBrd;

  const
    MaxFormats = 20;

  type
    
  TInterfacedComponent = class(TComponent, IUnknown)
  Private
    fRefCount: Integer;
  Protected
    function QueryInterface(const IID: TGuid; OUT Obj): HRESULT;
               {$ifdef VER120} reintroduce; {$endif} StdCall;
    function _AddRef: Integer; StdCall;
    function _Release: Integer; StdCall;
  Public
     property RefCount: Integer Read fRefCount;
  end;

  TDragType = (dtCopy, dtMove, dtLink);
  TDragTypes = SET of TDragType;

  TDragResult = (drDropCopy, drDropMove, drDropLink, drCancel, drOutMemory, drUnknown);

  TDropEvent = procedure(Sender: TObject; DragType: TDragType;
               var ContinueDrop: Boolean) of Object;
  TFeedbackEvent = procedure(Sender: TObject; Effect: LongInt) of Object;

  TDropSource = class(TInterfacedComponent, IDropSource, IDataObject)
  Private
    fDragTypes      : TDragTypes;
    FeedbackEffect  : LongInt;
    fDropEvent      : TDropEvent;
    fFBEvent        : TFeedBackEvent;
    fDataFormats    : array[0..MaxFormats-1] of TFormatEtc;
    DataFormatsCount: integer;
  Protected
    // IDropSource implementation

    function QueryContinueDrag(fEscapePressed: bool; grfKeyState: LongInt): HRESULT; StdCall;
    function GiveFeedback(dwEffect: LongInt): HRESULT; StdCall;

    // IDataObject implementation
    function GetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT; StdCall;
    function GetDataHere(const FormatEtc: TFormatEtc; OUT Medium: TStgMedium):HRESULT; StdCall;
    function QueryGetData(const FormatEtc: TFormatEtc): HRESULT; StdCall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
             OUT FormatEtcOut: TFormatEtc): HRESULT; StdCall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
             fRelease: Bool): HRESULT; StdCall;
    function EnumFormatEtc(dwDirection: LongInt; OUT EnumFormatEtc: IEnumFormatEtc): HRESULT; StdCall;
    function dAdvise(const FormatEtc: TFormatEtc; advf: LongInt;
             const advsink: IAdviseSink; OUT dwConnection: LongInt): HRESULT; StdCall;
    function dUnadvise(dwConnection: LongInt): HRESULT; StdCall;
    function EnumdAdvise(OUT EnumAdvise: IEnumStatData): HRESULT; StdCall;

    //New functions...
    function DoGetData(const FormatEtcIn: TFormatEtc;
             OUT Medium: TStgMedium):HRESULT; Virtual; Abstract;
  Public
    constructor Create(aowner: TComponent); Override;
    destructor Destroy; override;
    function Execute: TDragResult;
    function CopyToClipboard: boolean; Virtual;
  Published
    property Dragtypes: TDragTypes read fDragTypes write fDragTypes;
    property OnFeedback: TFeedbackEvent Read fFBEvent Write fFBEvent;
    property OnDrop: TDropEvent Read fDropEvent Write fDropEvent;
  end;

  TDropTextSource = class(TDropSource)
  Private
    fText: String;
  Protected
    function DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT; Override;
  Public
    constructor Create(aOwner: TComponent); Override;
    function CopyToClipboard: boolean; Override;
    property Text: String Read fText Write fText;
  end;

  TDropFileSource = class(TDropSource)
  Private
    fFiles: TStrings;
  Protected
    function DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT; Override;
  Public
    constructor Create(aOwner: TComponent); Override;
    destructor Destroy; Override;
    function CopyToClipboard: boolean; Override;
    property Files: TStrings Read fFiles;
  end;

  TDropURLSource = class(TDropSource)
  Private
    fURL: String;
    //procedure SetURL(url: string);
  Protected
    function DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT; Override;
  Published
    property Dragtypes: TDragTypes read fDragTypes; //ReadOnly as only dtLink allowed
  Public
    constructor Create(aOwner: TComponent); Override;
    function CopyToClipboard: boolean; Override;
    property URL: String Read fURL Write fURL;
  end;

  procedure Register;

  var
    CF_FILEGROUPDESCRIPTOR, CF_FILECONTENTS,
    CF_IDLIST, CF_PREFERREDDROPEFFECT, CF_URL: UINT; //see initialization.

IMPLEMENTATION

  // -----------------------------------------------------------------------------
  //			Miscellaneous declarations and functions.
  // -----------------------------------------------------------------------------

  TYPE
    TMyDropFiles = PACKED RECORD
      dropfiles: TDropFiles;
      filenames: ARRAY[1..1] of Char;
    end;
    pMyDropFiles = ^TMyDropFiles;

  //******************* ConvertURLToFilename *************************
  function ConvertURLToFilename(url: string): string;
  const
    Invalids = '\/:?*<>,|''" ';
  var
    i: integer;
  begin
    if lowercase(copy(url,1,7)) = 'http://' then
      url := copy(url,8,128) // limit to 120 chars.
    else if lowercase(copy(url,1,6)) = 'ftp://' then
      url := copy(url,7,127)
    else if lowercase(copy(url,1,7)) = 'mailto:' then
      url := copy(url,8,128)
    else if lowercase(copy(url,1,5)) = 'file:' then
      url := copy(url,6,126);

    if url = '' then url := 'untitled';
    result := url;
    for i := 1 to length(result) do
      if result[i] = '/'then
      begin
        result := copy(result,1,i-1);
        break;
      end
      else if pos(result[i],Invalids) <> 0 then
        result[i] := '_';
     appendstr(result,'.url');
  end;

  //******************* GetSizeOfPidl *************************
  function GetSizeOfPidl(pidl: pItemIDList): integer;
  var
    i: integer;
  begin
    result := SizeOf(Word);
    repeat
      i := pSHItemID(pidl)^.cb;
      inc(result,i);
      inc(longint(pidl),i);
    until i = 0;
  end;

  //******************* GetShellFolderOfPath *************************
  function GetShellFolderOfPath(FolderPath: TFileName): IShellFolder;
  var
    DeskTopFolder: IShellFolder;
    PathPidl: pItemIDList;
    OlePath: Array[0..MAX_PATH] of WideChar;
    dummy,pdwAttributes: ULONG;
    ShellMalloc: IMalloc;
  begin
    result := nil;
    StringToWideChar( FolderPath, OlePath, MAX_PATH );
    try
      SHGetMalloc(ShellMalloc);
      if not (SHGetDesktopFolder(DeskTopFolder) = NOERROR) then exit;
      if (DesktopFolder.ParseDisplayName(0,
            nil,OlePath,dummy,PathPidl,pdwAttributes) = NOERROR) and
            (pdwAttributes and SFGAO_FOLDER <> 0) then
        DesktopFolder.BindToObject(PathPidl,nil,IID_IShellFolder,pointer(result));
      ShellMalloc.free(PathPidl);
    except
    end;
  end;

  //******************* GetFullPIDLFromPath *************************
  function GetFullPIDLFromPath(Path: TFileName): pItemIDList;
  var
     DeskTopFolder: IShellFolder;
     OlePath: Array[0..MAX_PATH] of WideChar;
     dummy1,dummy2: ULONG;
  begin
    result := nil;
    StringToWideChar( Path, OlePath, MAX_PATH );
    try
      if (SHGetDesktopFolder(DeskTopFolder) = NOERROR) then
        DesktopFolder.ParseDisplayName(0,nil,OlePath,dummy1,result,dummy2);
    except
    end;
  end;

  //******************* GetSubPidl *************************
  function GetSubPidl(Folder: IShellFolder; Sub: TFilename): pItemIDList;
  var
    dummy1,dummy2: ULONG;
    OleFile: Array[0..MAX_PATH] of WideChar;
  begin
    result := nil;
    try
      StringToWideChar( Sub, OleFile, MAX_PATH );
      Folder.ParseDisplayName(0,nil,OleFile,dummy1,result,dummy2);
    except
    end;
  end;

  //See "Clipboard Formats for Shell Data Transfers" in Ole.hlp...
  //(Needed to drag links (shortcuts).)

  //******************* ConvertFilesToShellIDList *************************
  type
    POffsets = ^TOffsets;
    TOffsets = array[0..$FFFF] of UINT;

  function ConvertFilesToShellIDList(path: string; files: TStrings): HGlobal;
  var
    shf: IShellFolder;
    ShellMalloc: IMalloc;
    PathPidl, pidl: pItemIDList;
    Ida: PIDA;
    pOffset: POffsets;
    ptrByte: ^Byte;
    i, PathPidlSize, IdaSize, PreviousPidlSize: integer;
  begin
    result := 0;
    shf := GetShellFolderOfPath(path);
    if shf = nil then exit;
    //Calculate size of IDA structure ...
    // cidl: UINT ; Directory pidl offset: UINT ; all file pidl offsets
    IdaSize := (files.count + 2) * sizeof(UINT);

    PathPidl := GetFullPIDLFromPath(path);
    if PathPidl = nil then exit;
    PathPidlSize := GetSizeOfPidl(PathPidl);
    SHGetMalloc(ShellMalloc);

    //Add to IdaSize space for ALL pidls...
    IdaSize := IdaSize + PathPidlSize;
    for i := 0 to files.count-1 do
    begin
      pidl := GetSubPidl(shf,files[i]);
      IdaSize := IdaSize + GetSizeOfPidl(Pidl);
      ShellMalloc.free(pidl);
    end;

    //Allocate memory...
    Result := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, IdaSize);
    if (Result = 0) then
    begin
      ShellMalloc.free(PathPidl);
      Exit;
    end;

    Ida := GlobalLock(Result);
    try
      FillChar(Ida^,IdaSize,0);

      //Fill in offset and pidl data...
      Ida^.cidl := files.count; //cidl = file count
      pOffset := @(Ida^.aoffset); //otherwise I would have to turn off range checking.
      pOffset^[0] := (files.count+2) * sizeof(UINT); //offset of Path pidl

      ptrByte := pointer(Ida);
      inc(ptrByte,pOffset^[0]); //ptrByte now points to Path pidl
      move(PathPidl^, ptrByte^, PathPidlSize); //copy path pidl
      ShellMalloc.free(PathPidl);

      PreviousPidlSize := PathPidlSize;
      for i := 1 to files.count do
      begin
        pidl := GetSubPidl(shf,files[i-1]);
        pOffset^[i] := pOffset^[i-1] + UINT(PreviousPidlSize); //offset of pidl
        PreviousPidlSize := GetSizeOfPidl(Pidl);

        ptrByte := pointer(Ida);
        inc(ptrByte,pOffset^[i]); //ptrByte now points to current file pidl
        move(Pidl^, ptrByte^, PreviousPidlSize); //copy file pidl
                              //PreviousPidlSize = current pidl size here
        ShellMalloc.free(pidl);
      end;
    finally
      GlobalUnLock(Result);
    end;
  end;

  //******************* Register *************************
  procedure Register;
  begin
    RegisterComponents('Samples', [TDropFileSource, TDropTextSource, TDropURLSource]);
  end;


  // -----------------------------------------------------------------------------
  //			TInterfacedComponent
  // -----------------------------------------------------------------------------

  // QueryInterface now returns HRESULT (needed for Delphi 4).
  // Thanks to 'Scotto the Unwise' - scottos@gtcom.net
  //******************* TInterfacedComponent.QueryInterface *************************
  function TInterfacedComponent.QueryInterface(const IID: TGuid; OUT Obj): HRESULT;
  begin
    if GetInterface(IID, Obj) then result := 0 else result := E_NOINTERFACE;
  end;

  //******************* TInterfacedComponent._AddRef *************************
  function TInterfacedComponent._AddRef: Integer;
  begin
    result := InterlockedIncrement(fRefCount);
  end;

  //******************* TInterfacedComponent._Release *************************
  function TInterfacedComponent._Release: Integer;
  begin
    Result := InterlockedDecrement(fRefCount);
    if (Result = 0) then
      Free;
  end;

  // -----------------------------------------------------------------------------
  //			TEnumFormatEtc
  // -----------------------------------------------------------------------------

  type

  pFormatList = ^TFormatList;
  TFormatList = ARRAY[0..255] of TFormatEtc;

  TEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  Private
    fFormatList: pFormatList;
    fFormatCount: Integer;
    fIndex: Integer;
  Public
    constructor Create(FormatList: pFormatList; FormatCount, Index: Integer);
    { IEnumFormatEtc }
    function Next(Celt: LongInt; OUT Elt; pCeltFetched: pLongInt): HRESULT; StdCall;
    function Skip(Celt: LongInt): HRESULT; StdCall;
    function Reset: HRESULT; StdCall;
    function Clone(OUT Enum: IEnumFormatEtc): HRESULT; StdCall;
  end;

//******************* TEnumFormatEtc.Create *************************
  constructor TEnumFormatEtc.Create(FormatList: pFormatList;
              FormatCount, Index: Integer);
  begin
    inherited Create;
    fFormatList := FormatList;
    fFormatCount := FormatCount;
    fIndex := Index;
  end;

  //******************* TEnumFormatEtc.Next *************************
  function TEnumFormatEtc.Next(Celt: LongInt; OUT Elt; pCeltFetched: pLongInt): HRESULT;
  var
    i: Integer;
  begin
    i := 0;
    WHILE (i < Celt) and (fIndex < fFormatCount) do
    begin
      TFormatList(Elt)[i] := fFormatList[fIndex];
      Inc(fIndex);
      Inc(i);
    end;
    if pCeltFetched <> NIL then pCeltFetched^ := i;
    if i = Celt then result := S_OK else result := S_FALSE;
  end;

  //******************* TEnumFormatEtc.Skip *************************
  function TEnumFormatEtc.Skip(Celt: LongInt): HRESULT;
  begin
    if Celt <= fFormatCount - fIndex then
    begin
      fIndex := fIndex + Celt;
      result := S_OK;
    end else
    begin
      fIndex := fFormatCount;
      result := S_FALSE;
    end;
  end;

  //******************* TEnumFormatEtc.Reset *************************
  function TEnumFormatEtc.ReSet: HRESULT;
  begin
    fIndex := 0;
    result := S_OK;
  end;

  //******************* TEnumFormatEtc.Clone *************************
  function TEnumFormatEtc.Clone(OUT Enum: IEnumFormatEtc): HRESULT;
  begin
    enum := TEnumFormatEtc.Create(fFormatList, fFormatCount, fIndex);
    result := S_OK;
  end;

  // -----------------------------------------------------------------------------
  //			TDropSource
  // -----------------------------------------------------------------------------

  //******************* TDropSource.Create *************************
  constructor TDropSource.Create(aOwner: TComponent);
  begin
    inherited Create(aOwner);
    DragTypes := [dtCopy]; //default to Copy.
    //to avoid premature release ...
    _AddRef;
    DataFormatsCount := 0;
  end;

  //******************* TDropSource.Destroy *************************
  destructor TDropSource.Destroy;
  begin
    inherited Destroy;
  end;

  //******************* TDropSource.GiveFeedback *************************
  function TDropSource.GiveFeedback(dwEffect: LongInt): HRESULT; StdCall;
  begin
    FeedbackEffect := dwEffect;
    //NB: Use the OnFeedback event sparingly as it will effect performance...
    if Assigned(OnFeedback) then OnFeedback(Self, dwEffect);
    result:=DRAGDROP_S_USEDEFAULTCURSORS;
  end;

  //******************* TDropSource.GetCanonicalFormatEtc *************************
  function TDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
           OUT FormatEtcOut: TFormatEtc): HRESULT;
  begin
    result := DATA_S_SAMEFORMATETC;
  end;

  //******************* TDropSource.SetData *************************
  function TDropSource.SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
           fRelease: Bool): HRESULT;
  begin
    result := E_NOTIMPL;
  end;

  //******************* TDropSource.DAdvise *************************
  function TDropSource.DAdvise(const FormatEtc: TFormatEtc; advf: LongInt;
           const advSink: IAdviseSink; OUT dwConnection: LongInt): HRESULT;
  begin
    result := OLE_E_ADVISENOTSUPPORTED;
  end;

  //******************* TDropSource.DUnadvise *************************
  function TDropSource.DUnadvise(dwConnection: LongInt): HRESULT;
  begin
    result := OLE_E_ADVISENOTSUPPORTED;
  end;

  //******************* TDropSource.EnumDAdvise *************************
  function TDropSource.EnumDAdvise(OUT EnumAdvise: IEnumStatData): HRESULT;
  begin
    result := OLE_E_ADVISENOTSUPPORTED;
  end;

  //******************* TDropSource.GetData *************************
  function TDropSource.GetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT; StdCall;
  begin
    result := DoGetData(FormatEtcIn, Medium);
  end;

  //******************* TDropSource.GetDataHere *************************
  function TDropSource.GetDataHere(const FormatEtc: TFormatEtc;
           OUT Medium: TStgMedium):HRESULT; StdCall;
  begin
    result := E_NOTIMPL;
  end;

  //******************* TDropSource.QueryGetData *************************
  function TDropSource.QueryGetData(const FormatEtc: TFormatEtc): HRESULT; StdCall;
  var
    i: integer;
  begin
    result:= S_OK;
    for i := 0 to DataFormatsCount-1 do
      with fDataFormats[i] do
      begin
        if (FormatEtc.cfFormat = cfFormat) and
           (FormatEtc.dwAspect = dwAspect) and
           (FormatEtc.tymed and tymed <> 0) then exit; //result:= S_OK;
      end;
    result:= E_FAIL;
  end;

  //******************* TDropSource.EnumFormatEtc *************************
  function TDropSource.EnumFormatEtc(dwDirection: LongInt;
           OUT EnumFormatEtc:IEnumFormatEtc): HRESULT; StdCall;
  begin
    if (dwDirection = DATADIR_GET) then
    begin
      EnumFormatEtc :=
        TEnumFormatEtc.Create(@fDataFormats, DataFormatsCount, 0);
      result := S_OK;
    end else if (dwDirection = DATADIR_SET) then
      result := E_NOTIMPL
    else result := E_INVALIDARG;
  end;

  //******************* TDropSource.QueryContinueDrag *************************
  function TDropSource.QueryContinueDrag(fEscapePressed: bool;
    grfKeyState: LongInt): HRESULT; StdCall;
  var
    ContinueDrop: Boolean;
    dragtype: TDragType;
  begin
    if fEscapePressed then
      result := DRAGDROP_S_CANCEL
    // will now allow drag and drop with either mouse button.
    else if (grfKeyState and (MK_LBUTTON or MK_RBUTTON) = 0) then
    begin
      ContinueDrop := True;
      dragtype := dtCopy;
      if (FeedbackEffect and DROPEFFECT_COPY <> 0) then DragType := dtCopy
      else if (FeedbackEffect and DROPEFFECT_MOVE <> 0) then dragtype := dtMove
      else if (FeedbackEffect and DROPEFFECT_LINK <> 0) then dragtype := dtLink
      else ContinueDrop := False;

      //if a valid drop then do OnDrop event if assigned...
      if ContinueDrop and
        (DragType in [dtCopy, dtMove, dtLink] * dragtypes) and
        Assigned(OnDrop) then OnDrop(Self, dragtype, ContinueDrop);

      if ContinueDrop then result := DRAGDROP_S_DROP
      else result := DRAGDROP_S_CANCEL;
    end else
      result := NOERROR;
  end;

  //******************* TDropSource.Execute *************************
  function TDropSource.Execute: TDragResult;
  var
    res: HRESULT;
    okeffect, effect: longint;
  begin
    result := drUnknown;
    okeffect := DROPEFFECT_NONE;
    if (dtCopy in fDragTypes) then okeffect := okeffect or DROPEFFECT_COPY;
    if (dtMove in fDragTypes) then okeffect := okeffect or DROPEFFECT_MOVE;
    if (dtLink in fDragTypes) then okeffect := okeffect or DROPEFFECT_LINK;

    res := DoDragDrop(Self as IDataObject, Self as IDropSource, okeffect, effect);
    case res of
      DRAGDROP_S_DROP:   begin
                           if (okeffect and effect <> 0) then
                           begin
                             if (effect and DROPEFFECT_COPY <> 0) then
                               result := drDropCopy
                             else if (effect and DROPEFFECT_MOVE <> 0) then
                               result := drDropMove
                             else result := drDropLink;
                           end else
                             result := drCancel;
                         end;
      DRAGDROP_S_CANCEL: result := drCancel;
      E_OUTOFMEMORY:     result := drOutMemory;
    end;
  end;

  //******************* TDropSource.CopyToClipboard *************************
  function TDropSource.CopyToClipboard: boolean;
  begin
    result := false;
  end;

  // -----------------------------------------------------------------------------
  //			TDropTextSource
  // -----------------------------------------------------------------------------

  //******************* TDropTextSource.Create *************************
  constructor TDropTextSource.Create(aOwner: TComponent);
  begin
    inherited Create(aOwner);
    fText := '';

    fDataFormats[0].cfFormat := CF_TEXT;
    fDataFormats[0].ptd := NIL;
    fDataFormats[0].dwAspect := DVASPECT_CONTENT;
    fDataFormats[0].lIndex := -1;
    fDataFormats[0].tymed := TYMED_HGLOBAL;

    fDataFormats[1].cfFormat := CF_FILEGROUPDESCRIPTOR;
    fDataFormats[1].ptd := NIL;
    fDataFormats[1].dwAspect := DVASPECT_CONTENT;
    fDataFormats[1].lIndex := -1;
    fDataFormats[1].tymed := TYMED_HGLOBAL;

    fDataFormats[2].cfFormat := CF_FILECONTENTS;
    fDataFormats[2].ptd := NIL;
    fDataFormats[2].dwAspect := DVASPECT_CONTENT;
    fDataFormats[2].lIndex := -1;
    fDataFormats[2].tymed := TYMED_HGLOBAL;

    DataFormatsCount := 3;
  end;

  //******************* TDropTextSource.CopyToClipboard *************************
  function TDropTextSource.CopyToClipboard: boolean;
  var
    FormatEtcIn: TFormatEtc;
    Medium: TStgMedium;
  begin
    FormatEtcIn.cfFormat := CF_TEXT;
    FormatEtcIn.dwAspect := DVASPECT_CONTENT;
    FormatEtcIn.tymed := TYMED_HGLOBAL;
    if fText = '' then result := false
    else if GetData(formatetcIn,Medium) = S_OK then
    begin
      Clipboard.SetAsHandle(CF_TEXT,Medium.hGlobal);
      result := true;
    end else result := false;
  end;

  //******************* TDropTextSource.DoGetData *************************
  function TDropTextSource.DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT;
  var
    pFGD: PFileGroupDescriptor;
    pText: PChar;
  begin

    Medium.tymed := 0;
    Medium.UnkForRelease := NIL;
    Medium.hGlobal := 0;

    //--------------------------------------------------------------------------
    if ((FormatEtcIn.cfFormat = CF_TEXT) or
      (FormatEtcIn.cfFormat = CF_FILECONTENTS)) and
      (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
      (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
    begin
      Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, Length(fText)+1);
      if (Medium.hGlobal = 0) then
        result := E_OUTOFMEMORY
      else
      begin
        medium.tymed := TYMED_HGLOBAL;
        pText := PChar(GlobalLock(Medium.hGlobal));
        try
          StrCopy(pText, PChar(fText));
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
        result := S_OK;
      end;
    end
    //--------------------------------------------------------------------------
    else if (FormatEtcIn.cfFormat = CF_FILEGROUPDESCRIPTOR) and
      (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
      (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
    begin
      Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, SizeOf(TFileGroupDescriptor));
      if (Medium.hGlobal = 0) then
      begin
        result := E_OUTOFMEMORY;
        Exit;
      end;
      medium.tymed := TYMED_HGLOBAL;
      pFGD := pointer(GlobalLock(Medium.hGlobal));
      try
        with pFGD^ do
        begin
          cItems := 1;
          fgd[0].dwFlags := FD_LINKUI;
          fgd[0].cFileName := 'Text Scrap File.txt';
        end;
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      result := S_OK;
    end else
      result := DV_E_FORMATETC;
  end;

  // -----------------------------------------------------------------------------
  //			TDropFileSource
  // -----------------------------------------------------------------------------

  //******************* TDropFileSource.Create *************************
  constructor TDropFileSource.Create(aOwner: TComponent);
  begin
    inherited Create(aOwner);
    fFiles := TStringList.Create;

    fDataFormats[0].cfFormat := CF_HDROP;
    fDataFormats[0].ptd      := NIL;
    fDataFormats[0].dwAspect := DVASPECT_CONTENT;
    fDataFormats[0].lIndex   := -1;
    fDataFormats[0].tymed    := TYMED_HGLOBAL;

    fDataFormats[1].cfFormat := CF_IDLIST;
    fDataFormats[1].ptd      := NIL;
    fDataFormats[1].dwAspect := DVASPECT_CONTENT;
    fDataFormats[1].lIndex   := -1;
    fDataFormats[1].tymed    := TYMED_HGLOBAL;

    fDataFormats[2].cfFormat := CF_PREFERREDDROPEFFECT;
    fDataFormats[2].ptd      := NIL;
    fDataFormats[2].dwAspect := DVASPECT_CONTENT;
    fDataFormats[2].lIndex   := -1;
    fDataFormats[2].tymed    := TYMED_HGLOBAL;

    //DataFormatsCount := 3;
    //Ignore CF_PREFERREDDROPEFFECT for the moment as still testing it.
    DataFormatsCount := 2;
  end;

  //******************* TDropFileSource.Destroy *************************
  destructor TDropFileSource.destroy;
  begin
    fFiles.Free;
    inherited Destroy;
  end;

  //******************* TDropFileSource.CopyToClipboard *************************
  function TDropFileSource.CopyToClipboard: boolean;
  var
    FormatEtcIn: TFormatEtc;
    Medium: TStgMedium;
  begin
    FormatEtcIn.cfFormat := CF_HDROP;
    FormatEtcIn.dwAspect := DVASPECT_CONTENT;
    FormatEtcIn.tymed := TYMED_HGLOBAL;
    if Files.count = 0 then result := false
    else if GetData(formatetcIn,Medium) = S_OK then
    begin
      Clipboard.SetAsHandle(CF_HDROP,Medium.hGlobal);
      result := true;
    end else result := false;
  end;

  //******************* TDropFileSource.DoGetData *************************
  function TDropFileSource.DoGetData(const FormatEtcIn: TFormatEtc;
           OUT Medium: TStgMedium):HRESULT;
  var
    i: Integer;
    dropfiles: pMyDropFiles;
    pText: PChar;
    DropEffect: ^DWORD;
    strlength: Integer;
    tmpFilenames: TStringList;
  begin
    Medium.tymed := 0;
    Medium.UnkForRelease := NIL;
    Medium.hGlobal := 0;

    if fFiles.count = 0 then result := E_UNEXPECTED
    //--------------------------------------------------------------------------
    else if (FormatEtcIn.cfFormat = CF_HDROP) and
      (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
      (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
    begin
      strlength := 0;
      for i := 0 to fFiles.Count-1 do
        Inc(strlength, Length(fFiles[i])+1);
      Medium.hGlobal :=
        GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, SizeOf(TDropFiles)+strlength+1);
      if (Medium.hGlobal = 0) then
        result:=E_OUTOFMEMORY
      else
      begin
        Medium.tymed := TYMED_HGLOBAL;
        dropfiles := GlobalLock(Medium.hGlobal);
        try
          dropfiles^.dropfiles.pfiles := SizeOf(TDropFiles);
          dropfiles^.dropfiles.fwide := False;
          pText := @(dropfiles^.filenames);
          for i := 0 to fFiles.Count-1 do
          begin
            StrPCopy(pText, fFiles[i]);
            Inc(pText, Length(fFiles[i])+1);
          end;
          pText^ := #0;
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
        result := S_OK;
      end;
    end
    //--------------------------------------------------------------------------
    else if (FormatEtcIn.cfFormat = CF_IDLIST) and
      (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
      (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
    begin
      tmpFilenames := TStringList.create;
      try
        Medium.tymed := TYMED_HGLOBAL;
        for i := 0 to fFiles.count-1 do
          tmpFilenames.add(extractfilename(fFiles[i]));
        Medium.hGlobal :=
            ConvertFilesToShellIDList(extractfilepath(fFiles[0]),tmpFilenames);
        if Medium.hGlobal = 0 then
          result:=E_OUTOFMEMORY else
          result := S_OK;
      finally
        tmpFilenames.free;
      end;
    end
    //--------------------------------------------------------------------------
    //This next format does not work for Win95 but should for Win98, WinNT ...
    //It stops the shell from prompting (with a popup menu) for the choice of
    //Copy/Move/Shortcut when dropping a file shortcut onto Desktop or Explorer.
    //*****************NOTE: STILL TESTING THIS**********************
    else if (FormatEtcIn.cfFormat = CF_PREFERREDDROPEFFECT) and
      (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
      (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
    begin
      Medium.tymed := TYMED_HGLOBAL;
      Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, SizeOf(DWORD));
      if Medium.hGlobal = 0 then
        result:=E_OUTOFMEMORY
      else
      begin
        DropEffect := GlobalLock(Medium.hGlobal);
        try
          DropEffect^ := FeedbackEffect;
        finally
          GlobalUnLock(Medium.hGlobal);
        end;
        result := S_OK;
      end;
    end
    else
      result := DV_E_FORMATETC;
  end;


  // -----------------------------------------------------------------------------
  //			TDropURLSource
  // -----------------------------------------------------------------------------

  //******************* TDropURLSource.Create *************************
  constructor TDropURLSource.Create(aOwner: TComponent);
  begin
    inherited Create(aOwner);
    fURL := '';
    fDragTypes := [dtLink]; // Only dtLink allowed

    fDataFormats[0].cfFormat := CF_URL;
    fDataFormats[0].ptd := NIL;
    fDataFormats[0].dwAspect := DVASPECT_CONTENT;
    fDataFormats[0].lIndex := -1;
    fDataFormats[0].tymed := TYMED_HGLOBAL;

    fDataFormats[1].cfFormat := CF_FILEGROUPDESCRIPTOR;
    fDataFormats[1].ptd := NIL;
    fDataFormats[1].dwAspect := DVASPECT_CONTENT;
    fDataFormats[1].lIndex := -1;
    fDataFormats[1].tymed := TYMED_HGLOBAL;

    fDataFormats[2].cfFormat := CF_FILECONTENTS;
    fDataFormats[2].ptd := NIL;
    fDataFormats[2].dwAspect := DVASPECT_CONTENT;
    fDataFormats[2].lIndex := 0;
    fDataFormats[2].tymed := TYMED_HGLOBAL;

    fDataFormats[3].cfFormat := CF_TEXT;
    fDataFormats[3].ptd := NIL;
    fDataFormats[3].dwAspect := DVASPECT_CONTENT;
    fDataFormats[3].lIndex := -1;
    fDataFormats[3].tymed := TYMED_HGLOBAL;

    DataFormatsCount := 4;
  end;

(*

//******************* TDropURLSource.SetURL *************************
  procedure TDropURLSource.SetURL(url: string);
  begin
    if url = '' then fURL := ''
    else if (copy(lowercase(url),1,7) = 'http://') or
       (copy(lowercase(url),1,6) = 'ftp://') or
       (copy(lowercase(url),1,5) = 'file:') or
       (copy(lowercase(url),1,7) = 'mailto:')  then
      fURL := url
    else if (copy(lowercase(url),1,4) = 'www.') then
      fURL := 'http://' + url
    else if (pos('@',url) > 0) and (pos('@',url) < 20) and (pos(' ',url) = 0) then
      fURL := 'mailto:' + url
    else if (copy(url,2,2) = ':\') and
      (lowercase(url[1]) >= 'a') and (lowercase(url[1]) <= 'z') then
      fURL := 'file:' + url
    else fURL := '';
  end;
*)

  //******************* TDropURLSource.CopyToClipboard *************************
  function TDropURLSource.CopyToClipboard: boolean;
  var
    FormatEtcIn: TFormatEtc;
    Medium: TStgMedium;
  begin
    FormatEtcIn.cfFormat := CF_URL;
    FormatEtcIn.dwAspect := DVASPECT_CONTENT;
    FormatEtcIn.tymed := TYMED_HGLOBAL;
    if fURL = '' then result := false
    else if GetData(formatetcIn,Medium) = S_OK then
    begin
      Clipboard.SetAsHandle(CF_URL,Medium.hGlobal);
      result := true;
    end else result := false;
  end;

  //******************* TDropURLSource.DoGetData *************************
  function TDropURLSource.DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT;
  const
    URLPrefix = '[InternetShortcut]'#10'URL=';
  var
    pFGD: PFileGroupDescriptor;
    pText: PChar;
  begin

    Medium.tymed := 0;
    Medium.UnkForRelease := NIL;
    Medium.hGlobal := 0;

    //--------------------------------------------------------------------------
    if ((FormatEtcIn.cfFormat = CF_URL) or (FormatEtcIn.cfFormat = CF_TEXT)) and
      (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
      (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
    begin
      Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, Length(fURL)+1);
      if (Medium.hGlobal = 0) then
        result := E_OUTOFMEMORY
      else
      begin
        medium.tymed := TYMED_HGLOBAL;
        pText := PChar(GlobalLock(Medium.hGlobal));
        try
          StrCopy(pText, PChar(fURL));
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
        result := S_OK;
      end;
    end
    //--------------------------------------------------------------------------
    else if (FormatEtcIn.cfFormat = CF_FILECONTENTS) and
      (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
      (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
    begin
      Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, Length(URLPrefix + fURL)+1);
      if (Medium.hGlobal = 0) then
        result := E_OUTOFMEMORY
      else
      begin
        medium.tymed := TYMED_HGLOBAL;
        pText := PChar(GlobalLock(Medium.hGlobal));
        try
          StrCopy(pText, PChar(URLPrefix + fURL));
        finally
          GlobalUnlock(Medium.hGlobal);
        end;
        result := S_OK;
      end;
    end
    //--------------------------------------------------------------------------
    else if (FormatEtcIn.cfFormat = CF_FILEGROUPDESCRIPTOR) and
      (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
      (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
    begin
      Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, SizeOf(TFileGroupDescriptor));
      if (Medium.hGlobal = 0) then
      begin
        result := E_OUTOFMEMORY;
        Exit;
      end;
      medium.tymed := TYMED_HGLOBAL;
      pFGD := pointer(GlobalLock(Medium.hGlobal));
      try
        with pFGD^ do
        begin
          cItems := 1;
          fgd[0].dwFlags := FD_LINKUI;
          StrPCopy(fgd[0].cFileName,ConvertURLToFilename(fURL));
        end;
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      result := S_OK;
    //--------------------------------------------------------------------------
    end else
      result := DV_E_FORMATETC;
  end;

  //********************************************
  //********************************************

initialization
  OleInitialize(NIL);
  CF_FILECONTENTS := RegisterClipboardFormat(CFSTR_FILECONTENTS);
  CF_FILEGROUPDESCRIPTOR := RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
  CF_IDLIST := RegisterClipboardFormat(CFSTR_SHELLIDLIST);
  CF_PREFERREDDROPEFFECT := RegisterClipboardFormat('Preferred DropEffect');
  CF_URL := RegisterClipboardFormat('UniformResourceLocator');
finalization
  OleUninitialize;

end.
