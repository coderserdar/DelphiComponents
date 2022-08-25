unit magsubs2;
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off} 
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{
DELPHI RAS COMPONENT - Common Shell Subroutines
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright 2010, Magenta Systems Ltd
Thanks to Brian Long for assistance on the PIPLs
Thanks to Enzo from iMazzo for short cut improvements


26th May 2002 - Release 4.80 - Baseline
26th July 2002 - Release 4.90 - added ScutSearchDUN
11th Oct 2004 - Release 4.94 - added SCutSpecLinkEx and SCutAddLinkEx with more options
                               support some foreign languages for 'Dial' literal
4th August 2008 - Release 5.40 - made compatible with Delphi 2009
10th August 2010 - Release 5.60 - fixed cast warnings for Delphi 2009 and later



There is a demo/test program for these functions called scuttest.dpr/scutmain.pas/dfm
}

interface

uses Windows, SysUtils, Classes, ActiveX, ShlObj, ComObj ;

{ Literals for SCutSpecLink and GetShellPath, to get the windows path to specified
  system shell directories. }
const
    CSIDL_DESKTOP                 = $0000 ;     // <desktop>
    CSIDL_INTERNET                = $0001 ;     // Internet Explorer (icon on desktop)
    CSIDL_PROGRAMS                = $0002 ;     // Start Menu\Programs
    CSIDL_CONTROLS                = $0003 ;     // My Computer\Control Panel
    CSIDL_PRINTERS                = $0004 ;     // My Computer\Printers
    CSIDL_PERSONAL                = $0005 ;     // My Documents
    CSIDL_FAVORITES               = $0006 ;     // <user name>\Favorites
    CSIDL_STARTUP                 = $0007 ;     // Start Menu\Programs\Startup
    CSIDL_RECENT                  = $0008 ;     // <user name>\Recent
    CSIDL_SENDTO                  = $0009 ;     // <user name>\SendTo
    CSIDL_BITBUCKET               = $000a ;     // <desktop>\Recycle Bin
    CSIDL_STARTMENU               = $000b ;     // <user name>\Start Menu
    CSIDL_MYDOCUMENTS             = $000c ;     // the user's My Documents folder
    CSIDL_MYMUSIC                 = $000d ;
    CSIDL_MYVIDEO                 = $000e ;
    CSIDL_DESKTOPDIRECTORY        = $0010 ;     // <user name>\Desktop         16
    CSIDL_DRIVES                  = $0011 ;     // My Computer
    CSIDL_NETWORK                 = $0012 ;     // Network Neighborhood
    CSIDL_NETHOOD                 = $0013 ;     // <user name>\nethood
    CSIDL_FONTS                   = $0014 ;     // windows\fonts               20
    CSIDL_TEMPLATES               = $0015 ;
    CSIDL_COMMON_STARTMENU        = $0016 ;     // All Users\Start Menu
    CSIDL_COMMON_PROGRAMS         = $0017 ;     // All Users\Programs
    CSIDL_COMMON_STARTUP          = $0018 ;     // All Users\Startup           24
    CSIDL_COMMON_DESKTOPDIRECTORY = $0019 ;     // All Users\Desktop
    CSIDL_APPDATA                 = $001a ;     // <user name>\Application Data
    CSIDL_PRINTHOOD               = $001b ;     // <user name>\PrintHood
    CSIDL_LOCAL_APPDATA           = $001C ;     // non roaming, user\Local Settings\Application Data
    CSIDL_ALTSTARTUP              = $001d ;     // non localized startup
    CSIDL_COMMON_ALTSTARTUP       = $001e ;     // non localized common startup 30
    CSIDL_COMMON_FAVORITES        = $001f ;
    CSIDL_INTERNET_CACHE          = $0020 ;
    CSIDL_COOKIES                 = $0021 ;
    CSIDL_HISTORY                 = $0022 ;     //                                34
    CSIDL_COMMON_APPDATA          = $0023 ;     // All Users\Application Data
    CSIDL_WINDOWS                 = $0024 ;     // GetWindowsDirectory()
    CSIDL_SYSTEM                  = $0025 ;     // GetSystemDirectory()
    CSIDL_PROGRAM_FILES           = $0026 ;     // C:\Program Files             38
    CSIDL_MYPICTURES              = $0027 ;     // My Pictures, new for Win2K
    CSIDL_PROGRAM_FILES_COMMON    = $002b ;     // C:\Program Files\Common
    CSIDL_COMMON_DOCUMENTS        = $002e ;     // All Users\Documents          46
    CSIDL_COMMON_ADMINTOOLS       = $002f ;     // All Users\Start Menu\Programs\Administrative Tools
    CSIDL_ADMINTOOLS              = $0030 ;     // <user name>\Start Menu\Programs\Administrative Tools  48
    CSIDL_CONNECTIONS             = $0031 ;     // Network and Dial-up Connections - not Win9x           49
    CSIDL_COMMON_MUSIC            = $0035 ;
    CSIDL_COMMON_PICTURES         = $0036 ;
    CSIDL_COMMON_VIDEO            = $0037 ;
    CSIDL_RESOURCES               = $0038 ;
    CSIDL_RESOURCES_LOCALIZED     = $0039 ;
    CSIDL_COMMON_OEM_LINKS        = $003A ;
    CSIDL_CDBURN_AREA             = $003B ;
    CSIDL_COMPUTERSNEARME         = $003D ;

    CSIDL_FLAG_CREATE             = $8000 ;     // combine with CSIDL_ value to force folder creation in SHGetFolderPath()
    CSIDL_FLAG_DONT_VERIFY        = $4000 ;     // combine with CSIDL_ value to return an unverified folder path
    CSIDL_FLAG_NO_ALIAS           = $1000 ;
    CSIDL_FLAG_PER_USER_INIT      = $0800 ;
    CSIDL_FLAG_MASK               = $FF00 ;     // mask for all possible flag values

// exported functions

{ Adds a shortcut (or link) to the windows desktop for a non-file object such as
  a DUN connection or printer specified by name and special directory, or returning
  a list of items in the non-file directory.  Note that the special directories may
  be platform dependent with many new directories being added by W2K.

  FolderId is a CSIDL_xxx literal specifying the special directory, CSIDL_PRINTERS
  Printers), CSIDL_CONTROLS (Control Panel) or CSIDL_CONNECTIONS (Network and Dial-Up
  Connections) have been tested, CSIDL_CONNECTIONS is W2K and later only but is handled
  as a special case for Win9x by searching for the older Dial-Up Connections folder.

  If ScutName is not blank, it is searched in the directory, the fully qualified PIDL
  built and SCutAddLink called to add a shortcut (or link) to the windows desktop.

  If ScutName is blank, an empty (but created) TString or TStringList should be
  passed as Items and will be returned with a list of all items in the specified
  special directory, no shortcut is added.  This is designed so the application can
  list special directory items for selection in a second call.

  Property set is Err with a ASCII hex COM error if return is false. }
function SCutSpecLink (FolderId: integer; Items: TStrings;
                                ScutName: string ; var Err: String): boolean ;

{ Adds a shortcut (or link) to the windows desktop for a non-file object such as
  a DUN connection or printer specified by name and special directory, or returning
  a list of items in the non-file directory.  Note that the special directories may
  be platform dependent with many new directories being added by W2K.
  Similar to SCutSpecLink, but allows an icon to be specified, and the short
  cut make available to for all users.  }
function SCutSpecLinkEx (FolderId: integer; Items: TStrings;
                         ScutName, ScutDesc, IconLoc: string ; var Err: String;
                         ToCommonDesktopFolder : boolean): boolean ;

{ Adds a shortcut (or link) to the windows desktop.  This may be to an application
  program or a non-file object such as a DUN connection or printer specified by a PIDL.

  ScutName is the display name for the shortcut, ScutDesc is the description for the
  properties page, for an application program ScutProg and ScutArgs should be specified,
  for a non file object ScutPidl should be used instead of ScutProg (normally only by
  the SCutSpecLink function).  IconLoc is an optional file name for an alternative icon,
  which may normally left left blank, IconIdx is index of the icon within the file.

  Property set is Err with a ASCII hex COM error if return is false. }
function SCutAddLink (ScutName, ScutProg, ScutArgs, ScutDesc: string ; ScutPidl:
            PItemIdList; IconLoc: string; IconIdx: integer; var Err: String): boolean ;

{ Adds a shortcut (or link) to the windows desktop.  This may be to an application
  program or a non-file object such as a DUN connection or printer specified by a PIDL.
  Similar to SCutAddLink, but allows an icon to be specified, and the short
  cut make available to for all users.  }
function SCutAddLinkEx(ScutName, ScutProg, ScutArgs, ScutDesc: string ; ScutPidl:
             PItemIdList; IconLoc: string; IconIdx: integer;
              var Err: String; ToCommonDesktopFolder : boolean): boolean ;

var

{ For Win9x, the top shell index needs to be searched for the 'Dialup Networking'
  folder, in English a search for 'Dial' will work (the actual folder names varies),
  for other languages ScutSearchDial must be set to something suitable before calling
  SCutSpecLink. }

    ScutSearchDUN: string = 'Dial' ;

implementation

// PIDL stuff borrowed from Borland D6 ShellCtrls.pas (in demos)

function CreatePIDL(Size: Integer): PItemIDList;
var
  Malloc: IMalloc;
begin
  OleCheck(SHGetMalloc(Malloc));

  Result := Malloc.Alloc(Size);
  if Assigned(Result) then
    FillChar(Result^, Size, 0);
end;

function NextPIDL(IDList: PItemIDList): PItemIDList;
begin
  Result := IDList;
  Inc(PAnsiChar(Result), IDList^.mkid.cb);
end;

function GetPIDLSize(IDList: PItemIDList): Integer;
begin
  Result := 0;
  if Assigned(IDList) then
  begin
    Result := SizeOf(IDList^.mkid.cb);
    while IDList^.mkid.cb <> 0 do
    begin
      Result := Result + IDList^.mkid.cb;
      IDList := NextPIDL(IDList);
    end;
  end;
end;

function ConcatPIDLs(IDList1, IDList2: PItemIDList): PItemIDList;
var
  cb1, cb2: Integer;
begin
  if Assigned(IDList1) then
    cb1 := GetPIDLSize(IDList1) - SizeOf(IDList1^.mkid.cb)
  else
    cb1 := 0;

  cb2 := GetPIDLSize(IDList2);

  Result := CreatePIDL(cb1 + cb2);
  if Assigned(Result) then
  begin
    if Assigned(IDList1) then
      CopyMemory(Result, IDList1, cb1);
    CopyMemory(PAnsiChar(Result) + cb1, IDList2, cb2);
  end;
end;

procedure DisposePIDL(PIDL: PItemIDList);
var
  MAlloc: IMAlloc;
begin
  OLECheck(SHGetMAlloc(MAlloc));
  MAlloc.Free(PIDL);
end;

procedure FreeMem(var P: Pointer) ;
var
    Malloc: IMalloc;
begin
    OLECheck (SHGetMAlloc(MAlloc)) ;
    MAlloc.Free (P) ;
end;

//------------------------------------------------------------------------------
// add a special short cut link to an object in a virtual desktop menu
// such as DUN connections, printers, control panel, etc
// note only Win9x, W2K and XP - not NT4

function SCutSpecLink(FolderId: integer; Items: TStrings; ScutName: string ;
                                     var Err: String): boolean ; //Backwards compatible
begin
  result := SCutSpecLinkEx (FolderId, Items, ScutName,'','', Err, False);
end;

function SCutSpecLinkEx (FolderId: integer; Items: TStrings ;
                         ScutName, ScutDesc, IconLoc: string ;
                             var Err: String; ToCommonDesktopFolder : boolean): boolean ;
var
    DesktopFolder: IShellFolder ;
    ShellFolder: IShellFolder ;
    DriveFolder: IShellFolder ;
    FolderPidl: PItemIdList ;
    ItemPidl: PItemIdList ;
    RootPidl: PItemIdList ;
    FullItemPidl: PItemIdList ;
    ItemsFetched: ULONG;
    EnumIdList: IEnumIdList ;
    Ret: DWORD ;
    ItemName: string ;

    function GetDispName (const ShellFolder: IShellFolder; IDList: PItemIDList;
                                         Flags: DWORD; var Name: string): boolean ;
    var
        DisplayNameRet: TStrRet;

    begin
        result := true ;
        Name := '' ;
        if ShellFolder.GetDisplayNameOf (IDList, Flags, DisplayNameRet) = NOERROR then
        begin
            case DisplayNameRet.uType of
                STRRET_WSTR: Name := WideCharToString (DisplayNameRet.pOleStr) ;
                STRRET_OFFSET: Name := String (PAnsiChar (LongInt (IDList) +
                                            LongInt (DisplayNameRet.uOffset))) ; // 9 Aug 2010
                STRRET_CSTR:   Name := String (AnsiString (DisplayNameRet.cStr)) ;  // 9 Aug 2010
                else
                    result := false;
            end ;
            FreeMem (Pointer (DisplayNameRet.pOleStr)) ;
        end
        else
            result := false;
    end;

    // Get the real OS interface language, not the locale settings
    function GetUILanguage: string;

        function GetWindowsSystemFolder: string;
        var
            Required: cardinal;
        begin
            Result   := '';
            Required := GetSystemDirectory(nil, 0);
            if Required <> 0 then
            begin
                SetLength(Result, Required);
                GetSystemDirectory(PChar(Result), Required);
                SetLength(Result, StrLen(PChar(Result)));
            end;
        end;

    type
        TBuf = array [1..4] of smallint;
        PBuf = ^TBuf;
    const
        CheckFName = 'USER.EXE';

    var
        BSize, DummySize, Handle: longword;
        { For Delphi 3, change Longword to DWORD }
        Language, FName: string;
        Buffer:     PBuf;
        InfoBuffer: Pointer;
    begin
        FName := GetWindowsSystemFolder;
        if FName[Length(FName)] <> '\' then
            FName := FName + '\';
        FName := FName + CheckFName; {By default: %BOOTDRIVE%\%OS%\%SYSTEM%\USER.EXE }
        BSize := GetFileVersionInfoSize(PChar(FName), Handle);
        if BSize = 0 then
            Result := ''
        else
        begin
            GetMem(InfoBuffer, BSize);
            try
                if GetFileVersionInfo(PChar(FName), Handle, BSize, InfoBuffer) then
                begin
                    VerQueryValue(InfoBuffer, PChar('\VarFileInfo\Translation'),
                      Pointer(Buffer), DummySize);

                    SetLength(Language, 255);
                    VerLanguageName(Buffer^ [1], PChar(Language), 255);
                    SetLength(Language, StrLen(PChar(Language)));
                    Result := Language;
                end
                else
                    Result := '';
            finally
                FreeMem(InfoBuffer);
            end;
        end;
    end;

    function GetScutSearchDUN:string;
    var
        UILanguage : string;
    begin
        result:='Dial';
        UILanguage:=GetUILanguage;
        if UILanguage = 'English (United States)' then result:='Dial';
        if UILanguage = 'Nederlands (Nederland)' then result:='Inbel';
        if UILanguage = 'Nederlands (standaard)' then result:='Inbel';
        //TODO: ADD all other languages and DUN unique filter result data
        //if UILanguage = '' then result:='';// DE  ?
        //if UILanguage = '' then result:='';// FR  ?
        //if UILanguage = '' then result:='';// ES  ?
        //if UILanguage = '' then result:='';// DK  ?
        //if UILanguage = '' then result:='';// NO  ?
        //if UILanguage = '' then result:='';// TK  ?
    end;

begin
    result := false ;
    Err := '' ;
    if ScutName = '' then
    begin
        if NOT Assigned (Items) then exit ;
        Items.Clear ;  // returning list of items in folder
    end ;

  // get desktop and special shell folder interfaces
    RootPidl := Nil ;
    FolderPidl := Nil ;
    ItemPidl := Nil ;

    SHGetDesktopFolder(DesktopFolder);

    if ((Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5)) or //TODO: check for NT4 ServicePack level and add support
                                            (FolderId <> CSIDL_CONNECTIONS) then
    begin
        Ret := SHGetSpecialFolderLocation (0, FolderId, FolderPidl) ;
        Err := IntToHex (Ret, 8) ;
        if Ret <> S_OK then exit ;

        Ret := DesktopFolder.BindToObject (FolderPidl, nil,
                                     IID_IShellFolder, Pointer (ShellFolder)) ;
        Err := IntToHex (Ret, 8) ;
        if Ret <> S_OK then exit ;
        RootPidl := ConcatPIDLs (Nil, FolderPidl) ;
    end
    else
    begin

    // special case for Win9x Dial-Up Networking, needs to be located via Drives/My Computer folder
        Ret := SHGetSpecialFolderLocation (0, CSIDL_DRIVES, FolderPidl) ;
        Err := IntToHex (Ret, 8) ;
        if Ret <> S_OK then exit ;
        Ret := DesktopFolder.BindToObject (FolderPidl, nil,
                                     IID_IShellFolder, Pointer (DriveFolder)) ;
        Err := IntToHex (Ret, 8) ;
        if Ret <> S_OK then exit ;
        Ret := DriveFolder.EnumObjects (0, SHCONTF_FOLDERS OR SHCONTF_NONFOLDERS,
                                                                         EnumIdList) ;
        Err := IntToHex (Ret, 8) ;
        if Ret = S_OK then
        begin

        // run through list of items in Drives/My Computer folder looking for DUN folder
            while Ret = 0 do
            begin
                Ret := EnumIdList.Next (1, ItemPidl, ItemsFetched) ;
                if Ret = S_FALSE then break ;
                Err := IntToHex (Ret, 8) ;
                if (Ret <> NO_ERROR) then break ;

            // get display name - note there is also a parse name

                GetDispName (DriveFolder, ItemPidl, SHGDN_INFOLDER, ItemName) ;

            // search for Dial-Up Networking folder, but allow translation for non-English
                ScutSearchDUN:=GetScutSearchDUN; //NEW
                if Pos (ScutSearchDUN, ItemName) = 1 then
                begin
                    RootPidl := ConcatPIDLs (FolderPidl, ItemPidl) ;
                    Ret := DriveFolder.BindToObject (ItemPidl, nil,
                                           IID_IShellFolder, Pointer (ShellFolder)) ;
                    Err := IntToHex (Ret, 8) ;
                    if Ret <> S_OK then exit ;
                    break ;
                end ;
            end ;
        end ;
        if Ret <> S_OK then exit ;
    end ;

  // enumerate the folder to get list of items
    Ret := ShellFolder.EnumObjects (0, SHCONTF_FOLDERS OR
                        SHCONTF_NONFOLDERS OR SHCONTF_INCLUDEHIDDEN, EnumIdList) ;
    Err := IntToHex (Ret, 8) ;
    if Ret = S_OK then
    begin

    // run through list of items in folder, one at a time, getting relative item PIDL
        while (EnumIdList.Next (1, ItemPidl, ItemsFetched) = NO_ERROR) do
        begin

        // get display name - note there is also a parse name
            GetDispName (ShellFolder, ItemPidl, SHGDN_INFOLDER, ItemName) ;

        // just returning list of names
            if ScutName = '' then
            begin
                Items.Add (ItemName) ;
                result := true ;  // got at least one item
            end
            else

        // adding shortcut, see if found the matching name
            begin
                if ScutName = ItemName then
                begin
                // create fully qualified PIDL, then add the shell link to the desktop
                    FullItemPidl := ConcatPIDLs (RootPidl, ItemPidl) ;
                    result := SCutAddLinkEx(ItemName + '.lnk', '', '', ScutDesc,
                                                        FullItemPidl, IconLoc, 0, Err, ToCommonDesktopFolder) ;  //TODO Change to add Icon
                    DisposePIDL (FullItemPidl) ;
                    DisposePIDL (RootPidl) ;
                    DisposePIDL (ItemPidl) ;
                    DisposePIDL (FolderPidl) ;
                    exit ;  // done link, no more enumerating
                end ;
            end ;
        end ;
        DisposePIDL (RootPidl) ;
        DisposePIDL (ItemPidl) ;
        DisposePIDL (FolderPidl) ;
     end ;
end ;

// add a shortcut to a program or object to the windows desktop, backwards compatible
function SCutAddLink (ScutName, ScutProg, ScutArgs, ScutDesc: string ; ScutPidl:
            PItemIdList; IconLoc: string; IconIdx: integer; var Err: String): boolean ;
begin
  result:=SCutAddLinkEx(ScutName, ScutProg, ScutArgs, ScutDesc,
                                    ScutPidl, IconLoc, IconIdx, Err, False);
end;

//NEW  add a shortcut to a program or object to the windows (common) desktop
function SCutAddLinkEx (ScutName, ScutProg, ScutArgs, ScutDesc: string ; ScutPidl:
                PItemIdList; IconLoc: string; IconIdx: integer;
                        var Err: String; ToCommonDesktopFolder : boolean): boolean ;
var
    DesktopPidl: PItemIdList ;
    DesktopPath: array [0..MAX_PATH] of char;
    IfUnknown: IUnknown ;
    IfShellLink: IShellLink ;
    IfPersistFile: IPersistFile ;
    ScutLinkName: WideString ;
    Ret: DWORD ;
begin
    result := false ;
    { BEGIN NEW }
    if ToCommonDesktopFolder then
    begin
        // location of CommonDesktop folder
        SHGetSpecialFolderLocation (0, CSIDL_COMMON_DESKTOPDIRECTORY, DesktopPidl) ;
    end
    else
    begin
        // location of desktop folder
       SHGetSpecialFolderLocation (0, CSIDL_DESKTOPDIRECTORY, DesktopPidl) ;
    end;

    { END NEW }

    if NOT SHGetPathFromIDList (DesktopPidl, DesktopPath) then exit ;
    DisposePIDL (DesktopPidl);

  // create a com object
    IfUnknown := CreateComObject (CLSID_ShellLink) ;

  // cast the IUnknown interface to a IShellLink
    IfShellLink := IfUnknown as IShellLink ;

  // cast the IUnknown interface to a IPersistFile
    IfPersistFile := IfUnknown as IPersistFile;

  // using the Interface to the Shell link call some of it's methods.
    with IfShellLink do
    begin

    // see if adding a link to a virtual item like a printer or DUN connection
        if ScutPidl = Nil then
        begin
            SetArguments (PChar (ScutArgs)) ;   // program arguments only, not program name
            SetPath (PChar (ScutProg)) ;        // program name, no arguments
            SetWorkingDirectory (PChar (ExtractFilePath (ScutProg))) ; // directory only
        end
        else
            SetIDList (ScutPidl) ;    // full PIDL (not relative)
        SetDescription (PChar (ScutDesc)) ;
        if IconLoc <> '' then SetIconLocation (Pchar (IconLoc), IconIdx) ;
    end ;

  // need a wide string
    ScutLinkName := String (DesktopPath) + '\' + ScutName ;  // assume .lnk included

  // save link to the desktop folder
    Ret := IfPersistFile.Save (PWChar (ScutLinkName), False) ;
    Err := IntToHex (Ret, 8) ;
    result := (Ret = S_OK) ;
end ;

end.
