unit uDragFilesSrc;

{
TDragFilesSrc Component
© Angus Johnson
ajohnson@rpi.net.au
Version 1.0
September 1997.

DESCRIPTION: Enables dragging of files FROM your Form TO Windows Explorer
and other applications which can receive files.
Single or multiple files can either be MOVED or COPIED.

PUBLISHED PROPERTIES:
  DropEffect: TDropEffect (deCopy, deMove)
  VerifyFiles: boolean
PUBLIC PROPERTIES: (not available at design time)
  FileCount: TStringList (read only)
PUBLIC METHODS:
  AddFile(string)
  AddFiles(Tstrings)
  ClearFiles
  Execute: TDragResult (drInvalid, drCancelled, drDropped)
  OnDropping: TNotifyEvent

USAGE:
1. Add this non-visual component to the Form you wish to drag from.
2. Set DragEffect to either deMove or deCopy.
3. Before the Execute function is called, files need to be added
   to the FileList using one of the following functions -
   DragFilesSrc1.AddFile(filename:string);
   DragFilesSrc1.AddFiles(FileList: Tstrings);
   and DragFilesSrc1.ClearFiles - to clear files!
4. The function Execute (starts the drag operation). This function is
   usually called in a MouseDown or MouseMove method and has no parameters.
   The function returns - drInvalid, drCancelled, or drDropped - depending
   on success.
5. The OnDropping event is triggered (if assigned) immediately
   after the mouse is released at a valid drop point. This event may be used
   in the following situations (as examples):-
   Extracting files from an archive; or Downloading files from the net.
   It is MUCH more efficient to do these procedures here rather than in the method
   calling the Execute function, as the drag operation may be cancelled
   without the need for extracting or downloading files at all.
   In these 2 examples extract or download files into the temp directory (GetTempPath API)
   using this OnDropping event method, then move them from the temp directory
   by setting DragEffect to deMove.
6. Set VerifyFiles to either true or false.
   If true this verifies the existance of files on Execute
   and will immediately return 'drInvalid' if any one file listed does not exist.
   However, you may not wish to check for the existance of the files until
   the OnDropping event occurs (when the mouse is released at a valid drop point)
   as they may not even be created till then! (See the above examples.)
   Obviously, set VerifyFiles prior to calling the Execute function.

DISCLAIMER:
This software may be freely used but no guarantees are given
as to reliability. Please keep this header to acknowledge source.
USE AT YOUR OWN RISK.

PROBLEMS/COMMENTS/THANKS ...
ajohnson@rpi.net.au
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OLE2;

type
  TDragResult = (drInvalid, drCancelled, drDropped);
  TDropEffect = (deCopy, deMove);

  //From SHLOBJ unit
  PDropFiles = ^TDropFiles;
  TDropFiles = packed record
    pFiles: DWORD;                       { offset of file list }
    pt: TPoint;                          { drop point (client coords) }
    fNC: BOOL;                           { is it on NonClient area }
    fWide: BOOL;                         { WIDE character switch }
  end;

  TDragFilesSrc = class(TComponent)
  private
    fFileList: TStringList;
    fVerifyFiles: boolean;
    fDropEffect: TDropEffect;
    fDropping: TNotifyEvent;
    function GetFileCount: integer;
  public
    procedure ClearFiles;
    procedure AddFile(filename: string);
    procedure AddFiles(FileList: TStrings);

    constructor create(aOwner : TComponent); override;
    destructor destroy; override;

    Function Execute: TDragResult;
  published
    property DropEffect: TDropEffect read fDropEffect write fDropEffect;
    property VerifyFiles: boolean read fVerifyFiles write fVerifyFiles;
    property OnDropping: TNotifyEvent read fDropping write fDropping;
    property FileCount: integer read GetFileCount;
  end;

procedure Register;

implementation

{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Local Type Declarations of IDataObject , TMyEnum & IDropSource
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}
type

  TMyDataObject = class(IDataObject)
  private
    RefCount: integer;

    FileList: TStrings;
    FileListBytes: integer;
    ptrDropFile: pdropfiles;

  public
    function QueryInterface(const iid: TIID; var obj): HResult; override; stdcall;
    function AddRef: Longint; override; stdcall;
    function Release: Longint; override; stdcall;
    function GetData(var formatetcIn: TFormatEtc; var medium: TStgMedium):
      HResult; override; stdcall;
    function GetDataHere(var formatetc: TFormatEtc; var medium: TStgMedium):
      HResult; override; stdcall;
    function QueryGetData(var formatetc: TFormatEtc): HResult;
      override; stdcall;
    function GetCanonicalFormatEtc(var formatetc: TFormatEtc;
      var formatetcOut: TFormatEtc): HResult; override; stdcall;
    function SetData(var formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult; override; stdcall;
    function EnumFormatEtc(dwDirection: Longint; var enumFormatEtc:
      IEnumFormatEtc): HResult; override; stdcall;
    function DAdvise(var formatetc: TFormatEtc; advf: Longint;
      advSink: IAdviseSink; var dwConnection: Longint): HResult; override; stdcall;
    function DUnadvise(dwConnection: Longint): HResult; override; stdcall;
    function EnumDAdvise(var enumAdvise: IEnumStatData): HResult;
      override; stdcall;
    constructor Create(sl: TStrings);
    destructor Destroy; override;
  end;

  TMyEnum = class(IEnumFormatEtc)
  private
    RefCount: integer;
    Index: integer;

  public
    function QueryInterface(const iid: TIID; var obj): HResult; override; stdcall;
    function AddRef: Longint; override; stdcall;
    function Release: Longint; override; stdcall;
    function Next(celt: Longint; var elt;
      pceltFetched: PLongint): HResult; override; stdcall;
    function Skip(celt: Longint): HResult; override; stdcall;
    function Reset: HResult; override; stdcall;
    function Clone(var enum: IEnumFormatEtc): HResult; override; stdcall;
  end;

  TMyDropSource = class(IDropSource)
  private
    RefCount: integer;
    srcDropEffect: longint;
    srcDFS: TDragFilesSrc;
  public
    function QueryInterface(const iid: TIID; var obj): HResult; override; stdcall;
    function AddRef: Longint; override; stdcall;
    function Release: Longint; override; stdcall;
    function QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; override; stdcall;
    function GiveFeedback(dwEffect: Longint): HResult; override; stdcall;
    constructor Create(dfs: TDragFilesSrc);
  end;

{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
TMyDataObject methods:
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

constructor TMyDataObject.Create(sl: TStrings);
var
  i: integer;
begin
  inherited Create;
  FileList := TStringList.create;
  FileList.assign(sl);
  FileListBytes := 1;
  for i := 1 to FileList.count do
    inc(FileListBytes,length(FileList[i-1])+1);
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

destructor TMyDataObject.Destroy;
begin
  FileList.free;
  inherited destroy;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.QueryInterface(const iid: TIID; var obj): HResult; stdcall;
begin
  if IsEqualIID(iid, IID_IUnknown) or IsEqualIID(iid, IID_IDataObject) then begin
    Pointer(obj) := self;
    AddRef;
    Result := S_OK;
  end else begin
    Pointer(obj) := nil;
    Result := E_NOINTERFACE;
  end;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.AddRef: Longint; stdcall;
begin
  Inc(RefCount);
  Result := RefCount;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.Release: Longint; stdcall;
begin
  Dec(RefCount);
  Result := RefCount;
  if RefCount = 0 then
    Free;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.GetData(var formatetcIn: TFormatEtc; var medium: TStgMedium):
      HResult; stdcall;
var
	h: HGlobal;
   i, offset: integer;
begin
  Result := DV_E_FORMATETC;
  if not Failed(QueryGetData(formatetcIn)) then begin
      h := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT,FileListBytes+sizeof(tdropfiles));
		if h = 0 then begin
			Result:= E_OUTOFMEMORY;
			Exit;
		end;
      ptrdropfile:=globallock(h);
      with ptrdropfile^ do begin
          pfiles:=sizeof(Tdropfiles);
          pt.x:=0;
          pt.y:=0;
          longint(fnc) := 0;
          longint(Fwide) := 0;
      end;

      //Add the filenames after header
      offset := sizeof(tdropfiles);
      for i := 1 to FileList.count do begin
        if i = FileList.count then
          strPcopy( pchar(longint(ptrdropfile)+offset), FileList[i-1]+#0#0)
        else
          strPcopy( pchar(longint(ptrdropfile)+offset), FileList[i-1]+#0);
        offset := offset + length(FileList[i-1])+1;
      end;

      globalunlock(h);

		with medium do begin
    		tymed:=TYMED_HGLOBAL;
			hGlobal := h;
			unkForRelease := nil;
	 	end;
      result:=S_OK;
	end;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.GetDataHere(var formatetc: TFormatEtc; var medium: TStgMedium):
      HResult; stdcall;
begin
	Result := E_NOTIMPL;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.QueryGetData(var formatetc: TFormatEtc): HResult; stdcall;
begin
  with formatetc do begin
    if cfFormat <> CF_HDROP then
      Result := DV_E_FORMATETC
    else if (tymed and TYMED_HGLOBAL) = 0 then
      Result := DV_E_TYMED
    else
      Result := S_OK;
  end;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.GetCanonicalFormatEtc(var formatetc: TFormatEtc;
      var formatetcOut: TFormatEtc): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.SetData(var formatetc: TFormatEtc; var medium: TStgMedium;
      fRelease: BOOL): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.EnumFormatEtc(dwDirection: Longint; var enumFormatEtc:
      IEnumFormatEtc): HResult; stdcall;
begin
  if dwDirection = DATADIR_GET then begin
    enumFormatEtc := TMyEnum.Create;
    enumFormatEtc.AddRef;
    Result := S_OK;
  end else begin
    enumFormatEtc := nil;
    Result := E_NOTIMPL;
  end;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.DAdvise(var formatetc: TFormatEtc; advf: Longint;
      advSink: IAdviseSink; var dwConnection: Longint): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.DUnadvise(dwConnection: Longint): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDataObject.EnumDAdvise(var enumAdvise: IEnumStatData): HResult; stdcall;
begin
  Result := E_NOTIMPL;
end;

{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
TMyEnum methods (called by TMyDataObject)
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyEnum.QueryInterface(const iid: TIID; var obj): HResult; stdcall;
begin
  if IsEqualIID(iid, IID_IUnknown) or IsEqualIID(iid, IID_IEnumFormatEtc) then begin
    Pointer(obj) := self;
    AddRef;
    Result := S_OK;
  end else begin
    Pointer(obj) := nil;
    Result := E_NOINTERFACE;
  end;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyEnum.AddRef: Longint; stdcall;
begin
  Inc(RefCount);
  Result := RefCount;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyEnum.Release: Longint; stdcall;
begin
  Dec(RefCount);
  Result := RefCount;
  if RefCount = 0 then
    Free;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyEnum.Next(celt: Longint; var elt;
      pceltFetched: PLongint): HResult; stdcall;
begin
  Result := S_FALSE;
  if (Index = 0) and (celt > 0) then begin
    Inc(Index);
    with TFormatEtc(elt) do begin
      cfFormat := CF_HDROP;
      ptd := nil; // not sure I should do this!
      dwAspect := DVASPECT_CONTENT;
      lindex := -1;
      tymed := TYMED_HGLOBAL;
    end;

    if pceltFetched <> nil then pceltFetched^ := 1;
    if celt = 1 then Result := S_OK;
  	 end
  else begin
    if pceltFetched <> nil then pceltFetched^ := 0;
  end;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyEnum.Skip(celt: Longint): HResult; stdcall;
begin
  Inc(Index, celt);
  if Index > 1 then Result := S_FALSE else Result := S_OK;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyEnum.Reset: HResult; stdcall;
begin
  Index := 0;
  Result := S_OK;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyEnum.Clone(var enum: IEnumFormatEtc): HResult; stdcall;
begin
  enum := TMyEnum.Create;
  enum.AddRef;
  TMyEnum(enum).Index := Index;
  Result := S_OK;
end;

{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
TMyDropDSource methods
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

constructor TMyDropSource.Create(dfs: TDragFilesSrc);
begin
  inherited Create;
  srcDFS := dfs;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDropSource.QueryInterface(const iid: TIID; var obj): HResult; stdcall;
begin
  if IsEqualIID(iid, IID_IUnknown) or IsEqualIID(iid, IID_IDropSource) then begin
    Pointer(obj) := self;
    AddRef;
    Result := S_OK;
  end else begin
    Pointer(obj) := nil;
    Result := E_NOINTERFACE;
  end;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDropSource.AddRef: Longint; stdcall;
begin
  Inc(RefCount);
  Result := RefCount;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDropSource.Release: Longint; stdcall;
begin
  Dec(RefCount);
  Result := RefCount;
  if RefCount = 0 then
    Free;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDropSource.QueryContinueDrag(fEscapePressed: BOOL;
      grfKeyState: Longint): HResult; stdcall;
var
  drpEffect: integer;
begin
   if srcDFS.DropEffect = deCopy then drpEffect := DROPEFFECT_COPY
   else drpEffect := DROPEFFECT_MOVE;

  if fEscapePressed then Result := DRAGDROP_S_CANCEL
  else if (grfKeyState and MK_LBUTTON) = 0 then begin
    if (srcDropEffect = drpEffect) and assigned( srcDFS.OnDropping ) then
      srcDFS.OnDropping(srcDFS); {do just before dropping}
    //Note: cancel a drop from OnDropping event by clearing FileList..
    if srcDFS.FileCount = 0 then Result := DRAGDROP_S_CANCEL
    else Result := DRAGDROP_S_DROP;
    end
  else
    Result := S_OK;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TMyDropSource.GiveFeedback(dwEffect: Longint): HResult; stdcall;
begin
  srcDropEffect := dwEffect;
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;

{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
TDragFilesSrc methods:
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

constructor TDragFilesSrc.create(aOwner : Tcomponent);
begin
  inherited create(aOwner);
  fFileList := TStringList.create;
  fFileList.sorted := true;
  fFileList.Duplicates := dupIgnore;
  fVerifyFiles := false;
  fDropEffect := deCopy;
  fDropping := nil;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

destructor TDragFilesSrc.destroy;
begin
  fFileList.free;
  inherited Destroy;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

procedure TDragFilesSrc.ClearFiles;
begin
  fFileList.clear;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

procedure TDragFilesSrc.AddFile(filename: string);
begin
  fFileList.add(filename);
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

procedure TDragFilesSrc.AddFiles(FileList: TStrings);
var
  i: integer;
begin
  for i := 1 to FileList.count do
    if FileList[i-1] <> '' then fFileList.add(FileList[i-1]);
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TDragFilesSrc.GetFileCount: integer;
begin
  result := fFileList.count;
end;
{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

function TDragFilesSrc.Execute: TDragResult;
var
    i: integer;
    dwEffect: Longint;
    DropSource : TMyDropSource;
    Dataobject : TMyDataObject;
begin
   Result := drInvalid;
   //Check that there are files in the list!
   if (fFileList.count = 0) or (fFileList[0] = '') then exit;
   if fVerifyFiles then
     for i := 1 to fFileList.count do
       if not fileexists(fFileList[i-1]) then exit;

	try
	 DataObject:=TMyDataObject.create(fFileList);
    DataObject.AddRef;
    try
    	DropSource:=TMyDropSource.create(self);
       DropSource.AddRef;
       //Note: DROPEFFECT_COPY =1, DROPEFFECT_MOVE =2
       //      hence the following is a crude typecast...
       //      DROPEFFECT := byte(fDropEffect)+1
       //      ie: deCopy -> DROPEFFECT_COPY, deMove -> DROPEFFECT_MOVE
    	if (DoDragDrop(dataobject, dropsource, byte(fDropEffect)+1, dwEffect) = DRAGDROP_S_DROP)
			and (dwEffect = byte(fDropEffect)+1) then Result := drDropped
    	else Result := drCancelled;
	 	DropSource.release;
 	 finally
    	DataObject.release;
	 end;
	except
	end;
end;

{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
Register
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

procedure Register;
begin
  RegisterComponents('Samples', [TDragFilesSrc]);
end;


{=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
 Startup/Shutdown
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=}

initialization
  OleInitialize(nil);

finalization
  OleUninitialize;
end.

