
{=============================================================================}
{ Unit LsFileExplorer28.pas - ver2.886                                    }
{                                                                             }
{=============================================================================}
{
  Archive       :- FileExpl.zip

  Components    :- TLsDirTree21,
                   TLsDirTreeCombo28 and
                   TLsFilelistView28.

  Compiler      :- Delphi 4, 5, 6 and 7 (also **C++Builder 4, 5 and 6)

                   ** Although not tested by the author, many users reported
                      that these components are working fine with C++Builder
                      4, 5 and 6.

  Version       :- 2.8.8.6

  Author        :- Leo D. Shih <ldshih@telus.net>

  Last UpDate   :- January 2006
}
{-----------------------------------------------------------------------------}
{
LICENSE AND DISCLAIMER
======================

  Copyright (C)1998/2006 Leo D. Shih, All rights reserved
  -------------------------------------------------------

TLsDirTree21, TlsDirTreeCombo28 and TlsFileListView28 (components) are currently
distributed as Freeware, thus

1. You may copy and distribute verbatim copies of these components as you
receive them, in any medium, provided that this copyright notice included
in the source code file (LsFileExplorer28.pas) is kept intact.

2. You may modify the source code of these components to improve their features,
performance, etc. provided that

  2.1 You may not change the original copyright notice.

  2.2 The modified source code should contain descriptions what you have
      changed, and your name and e-mail address.

  2.3 Send the modified portion of the source code to the author so that the
      author may incorporate the modification into future releases of these
      components, with appropriate acknowledgement.

3. You may use these components, or any part thereof, in your programs,
provided that you meet following conditions:

  3.1 This notice may not be removed or altered from any source distribution,
      and the origin of these components must not be misrepresented.

  3.2 Appropriated credit to the author should be included in the "AboutForm"
      or the "HelpFile", and the documentation of your program.

  3.3 If your program is to be distributed as commercial products, in addition
      to above conditions, please write to the author for permission.

These components are provided 'as-is', without warranty of any kind, either
expressed or implied. In no event shall the author be liable for any problems
or damages arising from the use of these components.
}
{-----------------------------------------------------------------------------}
{
ACKNOWLEDGMENTS
===============
  I'm most grateful to those people in the Delphi community who have
  reported bugs, suggested fixings or recommanded improvements.

  In particular, I would like to thank Ales TRtnik for the necessary
  code to convert LsFileListview28 to virtual mode; also Andreas Roth,
  Brad Huggins, Claude Hunter, Detlef Scheil, Harrie Roaymans,
  Marcelo Rodrigues, Steve Pinneo, Tom Lisjac, Maximo Yarritu,
  Bogdan Giusca, Bernd Ohse, Jake Jones, Martin Berta, Matjaz Prtenjak
  and Budi Sukmawan for their professional suggestions.

  I would also like thank Andreas Roth, Bernd Ohse, Bernard Bourguignon,
  Ferruccio Accalai, David Abdaleon, Alberto Meyer, Sam Francke, Zenon
  Mieszkuniec, Martin Berta, Scarfman Lin, Yoshihiro Sugahara, Matjaz
  Prtenjak, Olle Johansson and Do-wan Kim for their German, French, Italian,
  Spanish, Brazillian-Portuguese, Dutch, Polish, Slovak, Chinese (Traditional
  and Simplified), Japanese, Slovenian, Swedish and Korean language translations.
}
{-----------------------------------------------------------------------------}
{
Brief Description
=================

  1. TLsDirTree21
  ---------------
  TLsDirTree21 is a simple but fully functional Directory Treeview with a
  popup FileListDlg (i.e. a simplified File ListView), that
  (a) displays drives/directories in the DirectoryTree and files of the
      selected directory in a popup FileListDlg.  Thus directories can be
      selected from the DirectoryTree, and files can be selected or opened
      in the FileListDlg;
  (b) supports creating, renaming, deleting, copying, moving and Dragging-
      and-Dropping operations on directories;
  (c) calculates the size of a selected portion of the DirectoryTree.
  (d) connects and disconnects network drives.

  2. TLsDirTreeCombo28
  --------------------
  TLsDirTreeCombo28 is a specialized ComboBox with a dropdown Directory
  TreeView, that displays a hierarchical tree of drives and directories of
  the File System.

  3. TLsFilelistView28 w/TLsFilelistView28PopUp
  ---------------------------------------------
  TLsFilelistView28 is a File ListView component, which is operating in
  virtual mode and is optimized for high capacity and fast speed access.
  In addition to its normal functions, it can also:
  (a) perform various file management tasks (such as cut, copy, paste,
      rename, delete, open, view and send_to operations on files);
  (b) copy files to, or paste files from, the Windows Explorer;
  (c) accept droppped files from the Windows Explorer.
  (d) produces statistics on Disk-FreeSpace, Selected-Numbers and
      Selected-Size.

  TLsDirTree21 and TLsFilelistView28 provide with Chinese ( both Traditional
  and Simplified), Dutch, French, German, Italian, Japanese, Polish,
  Brazilian-Portuguese, Slovak, Slovanenian, Spanish and Swedish languages
  support in addition to the default English language.

  -> For further information please refer to LsFileExplorer28.txt
}
{=============================================================================}

{$INCLUDE LSCOMP.INC}
{$IFDEF D6_OR_HIGHER}   //870
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFDEF D7_OR_HIGHER}  //877
  {$WARNINGS OFF}
{$ENDIF}

unit LsFileExplorer28;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Buttons,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, CommCtrl, ShellAPI, ClipBrd, Menus,
  FileCtrl, Registry, LsConsts, ShlObj, ActiveX, ComObj, ImgList,
  LsTheme;


const
  // User defined message
  WM_DIRCHANGE = WM_USER + 2;  //877<
  // Used by SGChangeNotifyRegister
  SHCNF_ACCEPT_INTERRUPTS     = $0001;
  SHCNF_ACCEPT_NON_INTERRUPTS = $0002;
  SHCNF_NO_PROXY              = $8000;  //877>

//Ales  //882<
Type
  TFileInfo = Record
    fiFileName: String;   // FileName
    fiFilePath: String;   // Path of file With \
    fiFileAttr: String;   // ARHS
    fiFileDate: String;   // Time of file in string
    fiType: String;       // 'file', 'dir', 'drv'
    fiSize: Int64;        // Size of file / disk
    fiFree: Int64;        // free disk
    fiDate: String;       // Time of file for sort
    fiAttr: Integer;      // dwFileAttributes  //883cl
    fiSHOK: Boolean;
    fiSHIcon: Integer;
    fiSHType: String;
  End;
  TFlieInfoArr = Array Of TFileInfo;
//Ales  //882>

type
  PNOTIFYREGISTER = ^NOTIFYREGISTER;  //877<
  NOTIFYREGISTER = record
    pidlPath      : PItemIDList;
    bWatchSubtree : boolean;  //877>
  end;

  SHChangeNotifyRegister = function(  //887 //881+<
    hWnd        : HWND;
    dwFlags     : integer;
    wEventMask  : cardinal;
    uMsg        : UINT;
    cItems      : integer;
    lpItems     : PNOTIFYREGISTER) : HWND; stdcall;

  SHChangeNotifyDeregister = function(
    hWnd        : HWND) : boolean; stdcall; //877 //881+>

Type
//  TFileAttr = (ftReadOnly, ftHidden, ftSystem, ftArchive, ftNormal);  //886
//  TFileType = set of TFileAttr;   //886

  TFileSize = (fsKB, fsByte);  //876

  TLanguage = (lnSysDefault, lnEnglish, lnChinese_Tra, lnChinese_Sim,
               lnDutch, lnFrench, lnGerman, lnItalian, lnJapanese, lnPolish,
               lnPortuguese, lnSlovenian, lnSlovak, lnSpanish, lnSwedish,
               lnKorean, lnTurkish);  //872 -> //886

  TDtFormat = (df_MMddyyyy, df_MMddyy, df_ddMMyyyy, //70
               df_ddMMyy_GB, df_ddMMyy_DE, df_ddMMyy_IT,
               df_yyyyMMdd, df_yyMMdd, df_Customize);

  TTmFormat = (tfLongFormat, tfShortFormat, tfCustomize);  //85

  //881>
  TLsListViewAction = (vaCopy, vaPaste, vaCut, vaOpen, vaView, vaNewFolder,
                       vaSelectAll, vaDelete, vaBack, vaRename, vaFindFile,
                       vaRefresh);
  TLsListViewActions = set of TLsListViewAction;
  //881<

  TSystemPath = (spDeskTop, spFavorites, spNetHood, spPersonal, spPrograms,
                 spSendTo, spStartMenu, spStartUp, spWinRoot, spWinSys);  //874

  // for LsDirTreeCombo28
  TPathChangeEvent =
    procedure(Sender: TObject; SelectedPath: string) of object;  //83
  // for LsDirTree21
  TDirChangeEvent =
    procedure(Sender: TObject; SelectedPath: string) of object;  //83
  TFileChangeEvent =
    procedure(Sender: TObject; SelectedFile: string) of Object;  //83
  // for LsFileListView28
  TSelItemChangeEvent =
    procedure(Sender: TObject; SelectedItem: string) of Object;  //83

  TLsFilelistView28 = class;
  TLsFilelistView28PopUp = class;
  TLsDirTreeCombo28 = class;
  TLsDirTreeView = class;
  TLsDirTree21 = class;
  TLsDirTree21PopUp = class;
  TLsSpeedButton = Class;  //862
  TNetPathDlg = Class;  //880
  TLsFileListShortCuts = class;  //881
  TListItemColors = class;  //883cl

{*********************************************************}
{                      TLsDirTreeview                     }
{          Drop-down TreeView of TLsDirTreeCombo28        }
{*********************************************************}

  TLsDirTreeView = class(TCustomTreeView)
  private
    TreeViewPath: string;
    FSelectedPath: string;
    FPreviousPath: string;  //875
    FExpandRoot: Boolean;
    FMouseInControl: Boolean;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMMouseMove(var Message: TWMMouseMove); message WM_MOUSEMOVE;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message
      WM_LBUTTONDOWN;

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Expand(Node: TTreeNode); override;
    procedure LoadDrives;
    procedure MakePath(Node: TTreeNode);
    procedure AddSubs(Path: string; Node: TTreeNode);
    procedure Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure OpenPath(sPath: string);  //875
    procedure CloseTree;  //876++
    procedure SelectPath; //876++
  end;

//================= End of TLsDirTreeview ===============//


{*********************************************************}
{                  TNewWindowProc        //879_MouseWheel }
{        Add MouseWheeel support to TLsDirTreeView        }
{*********************************************************}

  TNewWindowProc = Class(TObject)   //879_MouseWheel<
  private
    FOldWinProc  : TWndMethod;
    FWinCtrl: TWinControl;
  protected
    procedure NewWndProc(var Message: TMessage);
    procedure Restore;
  Public
    constructor Create(WinControl: TWinControl);
  end;   //879_MouseWheel>

//============== End of TNewWindowProc ==================//


{*********************************************************}
{                  TLsDirTreeCombo28                      }
{*********************************************************}

  TLsDirTreeCombo28 = class(TCustomEdit)
    Btn: TSpeedButton;
    Tree: TLsDirTreeView;

  private
    FFileList: TLsFilelistView28;
    FNetPathDlg: TNetPathDlg;
    FWinDir: string;
    ImageIndex: integer;
    ImagLst: TImageList;
    FCanvas: TControlCanvas;
    FOnPathChange: TPathChangeEvent;  //83
    FAbout: String;  //85
    FInitialDir: String;  //85
    NotifyRegister: PNotifyRegister;   //877
    NotifyHandle: hWnd;  //877
    FMessageHandle: HWnd;  //877
    FOldWindowProc: TWndMethod;  //879_MouseWheel+
    procedure CMMouseEnter(var Message: TMessage); Message CM_MOUSEENTER;  //876++
    procedure CMMouseLeave(var Message: TMessage); Message CM_MOUSELEAVE;  //876++
    procedure ProcessMsg(var Message: TMessage); //877

  protected
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetEditRect;
    procedure BtnClick(Sender: TObject);
    function GetFileList: TLsFilelistView28;
    procedure SetFileList(Value: TLsFilelistView28);
    function GetBtnGlyph: TBitmap;
    procedure SetBtnGlyph(NewValue: TBitmap);
    function GetTreeHeight: Integer;
    procedure SetTreeHeight(newValue: Integer);
    function GetSelectedPath: string;
    procedure SetSelectedPath(Value: string);
    function GetExpandRoot: Boolean;
    procedure SetExpandRoot(Value: Boolean);
    function GetHideSelection: Boolean;
    procedure SetHideSelection(Value: Boolean);
    function GetSelected: TTreeNode;  //875

    procedure SetSelected(Value: TTreeNode);  //875
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure Keydown(var Key: Word; Shift: TShiftState); override; //52
    procedure SetAbout(Value: String);  //85
    procedure SetInitialDir(Value: String);  //85
    procedure WMRButtonUp(var Message: TWMRButtonUp); message WM_RBUTTONUP;  //862
    procedure WMKillFocus(var Message: TWMKILLFOCUS); message WM_KILLFOCUS;  //862

  public
  { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure SetBounds(Left, Top, Width, Height: Integer); override;
    procedure OpenPath(dPath: string);
    procedure ResetTreeView;
    procedure ConnectNetResource(Sender: TObject);  //862+
    procedure DisConnectNetResource(Sender: TObject);  //862+
    property InitialDir: String read FInitialDir write SetInitialDir;  //870
    property Selected: TTreeNode read GetSelected write SetSelected;  //875

  published
    property SelectedPath: string read GetSelectedPath write SetSelectedPath;
    property FileList: TLsFilelistView28 read GetFileList write SetFileList;
    property ExpandRoot: Boolean read GetExpandRoot write SetExpandRoot
      default True;
    property HideSelection: Boolean read GetHideSelection
      write SetHideSelection default True;
    property About: String read FAbout write SetAbout;  //85
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property Hint;
    property Left;
    property Name;
    property Glyph: TBitmap read GetBtnGlyph write SetBtnGlyph;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly; //53+
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property TreeHeight: Integer read GetTreeHeight write SetTreeHeight;
    property Visible;
    property Width;
    property OnPathChange: TPathChangeEvent read FOnPathChange
      write FOnPathChange;  //83
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
  end;

//================= TLsDirTreeCombo28 ===================//



{*********************************************************}
{                   TLsSpeedButton                   //862}
{          Used as button of TLsDirTreeCombo28            }
{*********************************************************}

  TLsSpeedButton = Class(TSpeedButton)  //862
  private
    procedure WMRButtonUp(var Message: TWMRButtonUp); Message WM_RBUTTONUP;
  end;

//881>
{********************************************************}
{                 TLsFileListShortCuts                   }
{********************************************************}
  TLsFileListShortCuts = class(TPersistent)
  private
    FCopy     ,
    FPaste    ,
    FCut      ,
    FOpen     ,
    FView     ,
    FNewFolder,
    FSelectAll,
    FDelete   ,
    FBack     ,
    FRename   ,
    FFindFile ,
    FRefresh  : TShortCut;

    FActions  : TLsListViewActions;
  public
    constructor Create(AOwner : TComponent);
    destructor Destroy; override;
    procedure Assign(Source : TPersistent); override;
    procedure SetDefaults;
    procedure ClearAll;

  published
    property scCopy         : TShortCut read FCopy       write FCopy;
    property scPaste        : TShortCut read FPaste      write FPaste;
    property scCut          : TShortCut read FCut        write FCut;
    property scOpen         : TShortCut read FOpen       write FOpen;
    property scView         : TShortCut read FView       write FView;
    property scNewFolder    : TShortCut read FNewFolder  write FNewFolder;
    property scSelectAll    : TShortCut read FSelectAll  write FSelectAll;
    property scDelete       : TShortCut read FDelete     write FDelete;
    property scBack         : TShortCut read FBack       write FBack;
    property scRename       : TShortCut read FRename     write FRename;
    property scFindFile     : TShortCut read FFindFile   write FFindFile;
    property scRefresh      : TShortCut read FRefresh    write FRefresh;

    property Actions        : TLsListViewActions read FActions write FActions;
  end;  //881<

//============= End of TLsFileListShortCuts =============//


{********************************************************}
{                  LsFileListItemColors                  }
{********************************************************}
//883cl<
  TListItemColors = Class(TPersistent)
  protected
    FAttr_Archive  : TColor;
    FAttr_Hidden   : TColor;
    FAttr_Readonly : TColor;
    FAttr_System   : TColor;

  public
    Constructor Create; virtual;

  published
    property Attr_Archive  : TColor  read FAttr_Archive
                                     write FAttr_Archive
                                     default clBlack;
    property Attr_Hidden   : TColor  read FAttr_Hidden
                                     write FAttr_Hidden
                                     default clGrayText;
    property Attr_Readonly : TColor  read FAttr_Readonly
                                     write FAttr_Readonly
                                     default clTeal;
    property Attr_System   : TColor  read FAttr_System
                                     write FAttr_System
                                     default clRed;
  end;
//883cl>
//============= End of LsFileListItemColors =============//


{*********************************************************}
{                   TLsDirTree21                      //60}
{*********************************************************}

  TLsDirTree21 = class(TCustomTreeView)
  private
    FFileList: TLsFilelistView28;
    FNetPathDlg: TNetPathDlg;  //880
    FPreviousPath: string;  //875
    FSelectedPath: string;
    TreeViewPath: string;
    FPopUpMenu: TLsDirTree21PopUp;
    FPopUpMenuEnabled: Boolean;
    FIsNewFolder: Boolean;
    FSelectedFile: string;  //80^
    DlgForm: TForm;  //82
    FileView: TListView;  //82
    BtnOK: TBitBtn;   //82
    SImgList: TImageList;  //82
    FOnDirChange: TDirChangeEvent;  //83
    FOnFileChange: TFileChangeEvent;  //83
    FAbout: String;  //85
    FInitialDir: String;  //85
    FisCutCopy: Boolean;  //86
    FSrcPath: string;  //86
    FDestPath: String;  //86
    FNetDrive: array[0..25] of Boolean;  //871
    FTvLanguage: TLanguage;  //872ln
    NotifyRegister: PNotifyRegister;   //877
    NotifyHandle: hWnd;  //877
    FMessageHandle: HWnd;  //877
    FNodeDrag: TTreeNode;  //878DragDrop
    FNodeDrop: TTreeNode;  //878DragDrop
    FDragDropEnabled: Boolean;  //878DragDrop
    FFolderType: TFileType; //881

  protected
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Expanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure LoadRoot;
    procedure LoadDrives;
    procedure Loaded; override;
    procedure AddSubs(Path: string; Node: TTreeNode);
    procedure MakePath(Node: TTreeNode);
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;  //84
    procedure SetAbout(Value: String);  //85
//    procedure SetFolderType(NewFolderType: TFileType); //886
    procedure SetFileList(Value: TLsFilelistView28);
    procedure SetInitialDir(Value: string);  //85
    procedure SetTvLanguage(Value: TLanguage);  //872ln
    procedure SetPopUpMenuEnabled(Value: Boolean);
    procedure SetSelectedPath(Value: string);
    procedure SetSelectedFile(Value: string); //80^
    procedure SetDragDropEnabled(Value: Boolean);
    procedure InitializeVar;  //872ln

    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure Edit(const Item: TTVItem); override;
    function GetPathFromNode(Node: TTreeNode): string;
    procedure OpenFileListDlg(Sender: TObject);  //82
    procedure DlgFormResize(Sender: TObject);  //82
    procedure FileViewDblClick(Sender: TObject);  //80^ //82
    procedure OKBtnClick(Sender: TObject);  //82
    procedure ProcessMsg(var Message: TMessage); //877

  public
  { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor destroy; override;
    procedure ReLoad;
    procedure OpenPath(dPath: string);
    procedure SHowFolderContents; //80^
    function AddNewNode(ParentNode: TTreeNode; NodeName: string):
      Boolean;
    function DeleteNode(Node: TTreeNode): Boolean;
    function GetTreeSize: double; //integer;   //85
    procedure ConnectNetResource(Sender: TObject);  //82
    procedure DisConnectNetResource(Sender: TObject);  //82
    procedure CutOrCopyNode(Mode: integer);  //86
    procedure PasteNode;  //86
    procedure DragOver(Source: TObject; X, Y: integer; State: TDragState;
      var Accept: Boolean); override; //878DragDrop
    procedure DragDrop(Source: TObject; X, Y: integer); override; //878DragDrop
    procedure ShowNodeProperties;
    procedure SetFolderType(NewFolderType: TFileType); //886

    property Images;  //85
    property Items;   //85
    property InitialDir: string read FInitialDir Write SetInitialDir;  //870

  published
    property FolderType: TFileType read FFolderType write SetFolderType default [ftArchive]; //881
    property FileList: TLsFilelistView28 read FFileList write SetFileList;
    property Language: TLanguage read FTvLanguage write SetTvLanguage;  //872ln //881
    property PopUpMenuEnabled: Boolean read FPopUpMenuEnabled
      write SetPopUpMenuEnabled default True;
    property SelectedPath: string read FSelectedPath write SetSelectedPath;
    property SelectedFile: string read FSelectedFile write SetSelectedFile;  //80^
    property About: string read FAbout write SetAbout;  //85
    property DragDropEnabled: Boolean read FDragDropEnabled
      write SetDragDropEnabled Default True;  //878DragDrop
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property Indent;
    property Left;
    property Name;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property SortType;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property Visible;
    property Width;
    property OnDirChange: TDirChangeEvent read FOnDirChange write FOnDirChange;  //83
    property OnFileChange: TFileChangeEvent read FOnFileChange write FOnFileChange;  //83
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
  end;

//================= End of TLsDirTree21 =================//


{*********************************************************}
{                 TLsDirTree21PopUp                       }
{*********************************************************}

  TLsDirTree21PopUp = class(TPopupMenu)
  private
    FDirTree: TLsDirTree21; //70
    Bmp1,
      Bmp2,
      Bmp3,
      Bmp4,  //86
      Bmp5,  //86
      Bmp6,  //86
      Bmp7,  //70
      Bmp8,  //80
      Bmp9,  //82
      Bmp10,  //82
      Bmp11: HBitmap;

    TvItemID_0,    //872ln>
      TvItemID_1,
      TvItemID_2,
      TvItemID_4,
      TvItemID_5,
      TvItemID_6,
      TvItemID_8,
      TvItemID_9,
      TvItemID_11,
      TvItemID_12,
      TvItemID_14: string;  //872ln<

  protected
    function AddNewItem(const aCaption: string; aShortCut: TShortCut;
      aChecked, aEnabled: Boolean; aGroup: integer; aOnClick: TNotifyEvent;
      hCtx: word; const aName: string; aTag: Integer; aBitMap: HBitMap): TMenuItem;

    procedure SetDirTree(Value: TLsDirTree21);
    procedure ItemOnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildItems;
    procedure Popup(X, Y: integer); override;
    property DirTree: TLsDirTree21 read FDirTree;
  end;

//============== End of TLsDirTree21PopUp ===============//


{*********************************************************}
{                  TLsFilelistView28                      }
{*********************************************************}

  TLsFilelistView28 = class(TCustomListView)
  private
  { Private declarations }
    FAbout: string;  //85
    //80 <
    FColWidth_Name: Integer;
    FColWidth_Size: Integer;
    FColWidth_Type: Integer;
    FColWidth_Mod: Integer;
    FColWidth_Attr: Integer;
    //80 >
    FDirTreeCombo: TLsDirTreeCombo28;
    FDirTree: TLsDirTree21;
    FDirectory: string;
    FDirectorySize: integer;
    FSelectedItem: string;
    FFileType: TFileType;
    FFileSize: TFileSize;  //876
    FMask: string;
    FParentDirEnabled: Boolean;  //870
    FPopupMenu: TLsFilelistView28PopUp;
    FPopUpMenuEnabled: Boolean;
    FSelectedFiles: TStrings;
    FSelFileList: String;  //878Clipbrd
    FShowFolders: Boolean;
    FSortColumn: integer;
    FSortForward: boolean;
    OldFName: string;
    FOpenByDblClick: Boolean;  //882
    FOpenByEnter: Boolean;    //882
    FDateFormat: TDtFormat;  //70
    FTimeFormat: TTmFormat; //85
    FDFormatStr: string; //70>
    FTFormatStr: string;
    FHideFileExt: Boolean; //70<
    FtextColor: Boolean;  //883

    SImgLst: TImageList;
    LImgLst: TImageList;
    ParentImgIdx: integer;  //872
    Bmp_Up28: HBitMap; //70>
    Bmp_Down28: HBitMap;
    FBitMap: TBitmap; //70>
    FileColExists: Boolean;  //872
    FParentDirCaption : string;  //872
    FColumnClickEnabled : Boolean;  //70
    FOnItemChange: TSelItemChangeEvent;  //83
    FLanguage: TLanguage;  //872ln<
    DrvTypeStr0: string;
    DrvTypeStr1: string;
    DrvTypeStr2: string;
    DrvTypeStr3: string;
    DrvTypeStr4: string;
    DrvTypeStr5: string;
    DrvTypeStr6: string;  //872ln>
    //878DragDrop<
    FDroppedFiles: TStrings;
    FNumDropped: Integer;
    FEnabled: Boolean;
    FWndHandle: HWND;
    FDefProc: Pointer;
    FWndProcInstance: Pointer;
    //878DragDrop>
    FShortCuts: TLsFileListShortCuts; //881
    Required_column: integer;  //883
    FItemColors: TListItemColors;  //883cl

    function GetDriveFreeSpace: Int64;  //LongInt;  //874
    function GetSelectedSize: Int64; //LongInt;  //874
    function GetSelectedNum: Integer;

    procedure Createimages;
    procedure ColumnClick(Sender: TObject; Column: TListColumn);
    procedure SetPopUpMenuEnabled(Value: Boolean);
    procedure SetOpenByDblClick(Value: Boolean);  //881
    procedure SetParentDirEnabled(Value: Boolean);  //870
    procedure SetItemColors(Value: TListItemColors);  //883cl
    procedure InitializeVar;  //872ln

  protected
  { Protected declarations }
//Ales  //882<
    procedure Loaded; override;
    function OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean; override;
//Ales  //882>
    procedure CustomDrawItem(Sender: TCustomListView; Item: TListItem;
                             State: TCustomDrawState; var DefaultDraw: Boolean); //883

    function AddFiles(FileMask: string; Attr: DWORD): Boolean;
    function CanEdit(Item: TListItem): Boolean; override;
    function GetSelectedItem: string;
    function GetDirectory: string;
    function GetWkgMask(var MaskStr: string): string;

    procedure AddDrives;
    procedure Click; override;
    procedure CreateWnd; override;
    procedure CreateDriveColumns;
    procedure CreateFileColumns;
    procedure DblClick; override;
    procedure Edit(const Item: TLVItem); override;
    procedure Keydown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;  //84
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure SetAbout(Value: string);  //85
    procedure SetColWidth_Name(Value: Integer);  //80
    procedure SetColWidth_Size(Value: Integer);  //80
    procedure SetColWidth_Type(Value: Integer);  //80
    procedure SetColWidth_Mod(Value: Integer);   //80
    procedure SetColWidth_Attr(Value: Integer);  //80

    procedure SetDirectory(NewDir: string);
    procedure SetDirTreeCombo(Val: TLsDirTreeCombo28);
    procedure SetDirTree(VaL: TLsDirTree21);
//    procedure SetFileType(NewFileType: TFileType);   //886
    procedure SetHideFileExt(Value: Boolean); //70
    procedure SetMask(const NewMasks: string);
    procedure SetLanguage(Value: TLanguage);  //872ln
    procedure SetParentDirCaption(Value: string);  //872
    procedure SetShowFolders(Value: Boolean);
    procedure SetSelectedItem(NewItem: string);
    procedure SetDaTeFormat(Value: TDtFormat); Virtual; //70
    procedure SetTimeFormat(Value: TTmFormat); Virtual; //85
    procedure SendTo(SubItems: integer);
    procedure SendTo2(Path: string);
    procedure SendToPath(DestPath: string);
    procedure SendToDrive(DriveID: string);
    procedure SetFileSize(Value: TFileSize);  //876
    procedure CopyToClipboard(FileList: String);  //878clipbrd
    procedure PasteFromClipboard;  //878Clipbrd     //882
    //878DragDrop<
    procedure DropFiles( hDropHandle: HDrop );
    {$IFNDEF D4_OR_HIGHER}  //879
    procedure WndProc(var Msg: TMessage);   //879
    {$ELSE}
    procedure WndProc( var Msg: TMessage );  reintroduce;  //879
    {$ENDIF}  //879
    procedure InitFDragControl;
    procedure DestroyFDragControl;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    //878DragDrop>

    //< budskman
    procedure SetColumnBitmap;
    procedure WMNotify(var message: TWMNotify); message WM_NOTIFY;
    // budskman >

  public
  { Public declarations }
//Ales  //882<
    FFlieInfo: TFlieInfoArr;
    FFlieInfoCount: Integer;
//Ales  //882>
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CutCopy(Mode: integer);
    procedure DeleteItems;
    procedure FindFile;
    procedure NewFolder;
    procedure OneLevelUp;
    procedure OpenItem;
    procedure Paste;
    procedure RenameFile;
    procedure UpdateFileList;
    procedure ViewFile;
    procedure CreateShortCut;  //85
    procedure ShowFileProperties;
    procedure SetFileType(NewFileType: TFileType);  //886

    property DirectorySize: integer read FDirectorySize;
    property SelCount;
    property Selected;

  published
  { Published declarations }
    property About: string read FAbout write SetAbout;  //85
    property ColWidth_Name: Integer read FColWidth_Name
      write SetColWidth_Name Default 165;  //80
    property ColWidth_Size: Integer read FColWidth_Size
      write SetColWidth_Size Default 75;  //80
    property ColWidth_Type: Integer read FColWidth_Type
      write SetColWidth_Type Default 95;  //80
    property ColWidth_Mod:  Integer read FColWidth_Mod
      write SetColWidth_Mod Default 115;   //80
    property ColWidth_Attr: Integer read FColWidth_Attr
      write SetColWidth_Attr Default 40;  //80
    property DateFormat: TDtFormat read FDaTeFormat write SetDaTeFormat
      default df_Customize;   //82
    property Directory: string read GetDirectory write SetDirectory;
    property DirTreeCombo: TLsDirTreeCombo28 read FDirTreeCombo
      write SetDirTreeCombo;
    property DirTree: TLsDirTree21 read FDirTree
      write SetDirTree; //60
    property DriveFreeSpace: Int64 read GetDriveFreeSpace;  //874
    property OpenByDblClick: Boolean read FOpenByDblClick
      write SetOpenByDblClick default True;   //882
    property OpenByEnter: Boolean read FOpenByEnter write FOpenByEnter
      default True;   //882
    property FileType: TFileType read FFileType write SetFileType default
      [ftNormal];
    property FileSize: TFileSize read FFileSize write SetFileSize default
      fsKB;  //876
    property HideFileExt: Boolean read FHideFileExt write SetHideFileExt
      default False; //70
    property Language: TLanguage read FLanguage write SetLanguage;  //872ln
    property Mask: string read FMask write SetMask;
    property ParentDirEnabled: Boolean read FParentDirEnabled
      write SetParentDirEnabled default True;  //870
    property ParentDirCaption: string read FParentDirCaption
      write SetParentDirCaption;  //872
    property PopUpMenuEnabled: Boolean read FPopUpMenuEnabled
      write SetPopUpMenuEnabled default True;
    property SelectedItem: string read GetSelectedItem write SetSelectedItem;
    property SelectedNumber: Integer read GetSelectedNum;
    property SelectedSize: Int64  read GetSelectedSize;  //874
    property ShowFolders: Boolean read FShowFolders write SetShowFolders
      default True;
    property TimeFormat: TTmFormat read FTimeFormat write SetTimeFormat;  //85
    property ShortCuts : TLsFileListShortCuts read FShortCuts write FShortCuts; //881
    property LItemColorByATTR: Boolean Read FTextColor write FTextColor
      default True;  //883cl
    property LItemColors: TListItemColors read FItemColors write SetItemColors; //883cl
    property Align;
    property BorderStyle;
    property Color;
    property Columns Stored False;  //882
    property Ctl3D;
    property Cursor;
    property DragMode;
    property DragCursor;
    property Enabled;
    property Font;
    property Height;
    property HideSelection default False;
    property Hint;
    property IconOptions;
    property Items;
    property Left;
    property MultiSelect default True;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default False;
    property RowSelect default False;
    property ShowColumnHeaders default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Top;
    property ViewStyle default vsReport;
    property Visible;
    property Width;
    property OnItemChange: TSelItemChangeEvent read FOnItemChange
      write FOnItemChange;  //83
    property OnClick;
    property OnColumnClick;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property HotTrack;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property HotTrackStyles;
  end;

//============== End of TLsFileListView28 ===============//


{*********************************************************}
{                TLsFilelistView28PopUp                   }
{*********************************************************}

  TLsFilelistView28PopUp = class(TPopupMenu)
  private
    SendToList: TStrings;
    FFileListView: TLsFilelistView28;
    Bmp1, Bmp2,
      Bmp3, Bmp4,
      Bmp5, Bmp6,
      Bmp7, Bmp8,
      Bmp9, Bmp10,
      Bmp11, Bmp12,
      Bmp13 : HBitmap; //70 //85

    LvItemID_0,    //872ln<
      LvItemID_1,
      LvItemID_3,
      LvItemID_5,
      LvItemID_6,
      LvItemID_7,
      LvItemID_9,
      LvItemID_10,
      LvItemID_12,
      LvItemID_14,
      LvItemID_30,
      LvItemID_31,
      LvItemID_32,
      LvItemID_33: string;  //872ln>

  protected
    function AddNewItem(const aCaption: string; aShortCut: TShortCut;
      aChecked, aEnabled: Boolean; aGroup: integer; aOnClick: TNotifyEvent;
      hCtx: word; const aName: string; aTag: Integer; aBitMap: HBitMap): TMenuItem;  //862
    procedure SetFileListView(Value: TLsFilelistView28);
    procedure GetSendToSubMenu;
    procedure ItemOnClick(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildItems;
    procedure Popup(X, Y: integer); override;
    property FileListView: TLsFilelistView28 read FFileListView;
  end;

//=========== End of TLsFilelistView28PopUp =============//


{*********************************************************}
{                    TNetPathDlg                          }
{               For Map Network Drives                    }
{*********************************************************}

  TNetPathDlg = class(TWinControl)
  private
    { Private declarations }
    FDlgForm: TForm;
    FLabel1: TLabel;
    FLabel2: TLabel;
    FLabel3: TLabel;
    FCbxDrvLetter: TComboBox;
    FEdtNetPath: TEdit;
    FCBReconnect: TCheckBox;
    FBtnOk: TBitBtn;
    FBtnCancel: TBitBtn;
    FBtnBrowse: TButton;
    FDrvLetter: string;
    FNetPath: string;
    FReconnect: Boolean;
    FDrvLtrList: TStrings;

  protected
    procedure CreateWnd;  override;
    procedure BrowseClick(Sender: TObject);
    procedure SetDrvLetter(Value: String);
    procedure SetNetPath(Value: String);
    procedure CreateDrvLtrList;
  public
    { Public declarations }
    constructor create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean;
    property DrvLetter: string read FDrvLetter write SetDrvLetter;
    property NetPath: string  read FNetPath write SetNetPath;
    property Reconnect: Boolean read FReconnect write FReconnect;
  end;  //880<
//================ End of TNetPathDlg ===================//


{*********************************************************}
{             Global Functions/Procedures                 }
{*********************************************************}

function ExecuteFile(const Operation, FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
function DoSHFileOp(Handle: THandle; OpMode: UInt; Src: string;
  Dest: string; var Aborted: Boolean): Boolean;
function AddNullToStr(Path: string): string;
function StrContains(Str1, Str2: string): Boolean;
function BrowseForFolder(const FormHandle: HWND; var DirPath: string;
  nRoot: integer; inclFiles: Boolean): Boolean;   //880
function numpos(a: char; b: string; c: integer): integer;
function PosCount(a: char; b: string): integer;
function LastPos(ch: char; s: string): Integer;
function GetDiskSize(Root: string): Int64;  //LongInt;  //874
function GetFreeDiskSize(Root: string): Int64;  //LongInt;  //874
function ConvertSize(FSize: Int64; FAttr: string): String;
function DiskinDrive(Drive: Char; ShowMsg: word): Boolean;
function SlashSep(Path, FName: string): string;
function AddSlash(Path: string): string;
function DelSlash(Path: string): string;
function FileTimeToDateTimeStr(FTime: TFileTime; DFormat: string; //70
  TFormat: string): string;
function FileDirExist(FDName: string): Boolean;
function GetNormalIcon(Path: string): integer;
function GetSelectedIcon(Path: string): Integer;
function ExtractFileNameOnly(FName: string): String;  //85
function GetSystemPath(SysPath: TSystemPath): string;
procedure ShowProperties(FName: string);
procedure Delay(mSecs: integer);  //878
function GetVolumeName(DriveLtr: Char): String;  //877>
function ConnectDrive(_drvLetter: string; _netPath: string; _showError: Boolean;
  _reconnect: Boolean): DWORD;  //879+
function IsWindowsXP: Boolean;
Function DeleteNonNumberChars(Const InString: String): string;  //882
{*
// ======================
// Static loading of DLLs
// ======================
function SHChangeNotifyRegister(hWnd: HWND; dwFlags: integer;
  wEventMask: cardinal; uMsg: UINT;
  cItems: integer; lpItems: PNOTIFYREGISTER) : HWND;
  stdcall external 'shell32.dll' name 'SHChangeNotifyRegister';  //877 //881
function SHChangeNotifyDeRegister(hWnd: HWND) : boolean;
  stdcall external 'shell32.dll' name 'SHChangeNotifyDeRegister';  //887 //881
*}


procedure Register;


implementation

{$R LsFileExplorer28.Res}

const
  InvalidDOSChars = '\*?/="<>|:,;+^';
  DefaultMask = '*.*';
  FileOpMode: array[0..3] of UInt =
    (FO_COPY, FO_DELETE, FO_MOVE, FO_RENAME);

  FileExt: array[0..24] of string = ('.C', '.CPP', '.DPK', '.DPR', '.H',
    '.HPP', '.BPK', '.BPR', '.INF', '.INI', '.PAS', '.PRG', '.TXT', '.DOC',
    '.RTF', '.WRI', '.BMP', '.GIF', '.JPG', '.TIFF', '.PNG', '.WMF', '.ICO',
    '.HTML', '.HTM');  //85  //881

  LsFileExplorerKey = 'Software\LdShih\LsFileExplorer';

var
  //shared variables
  SelectedDir : String;  //80^
  Drives      : Set of 0..25; //80^
  LvLangID    : TLanguage;  //872ln>
  TvLangID    : TLanguage;
  FOpMode     : Integer;  //878
  FDgDpMode   : Integer;  //879

  //Language-specific messages
  ewDrive,  //873
  ewFolder,  //873
  ewError,
  ewFile,
  ewFrom,
  ewTo,
  ewCancel,
  ewBrowse,
  ewReadOnly,
  ewArchive,
  ewHidden,
  ewSystem,
  esCannot,
  esSpecifyDir,
  esInvalidDrvID,
  esDrvNotReady,
  esExists,
  esInvalidDirName,
  esConfirmRename,
  esCannotAddDrv,
  esNewFolder,
  esInvalidChars,
  esNotFound,
  esFilesIn,
  esFileOpFailed,
  esReadOnly,
  esNoFileSelected,
  esSendToFolder,
  esSendToPath,
  esPersistSaveError,
  esSetAttr,
  esTreeSize,  //873
  esAllSubDir: string;  //872LN<

{*********************************************************}
{              Global Functions/Procedures                }
{*********************************************************}

function ExecuteFile(const Operation, FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zOperation: array[0..79] of Char;
  zFileName: array[0..MAX_PATH] of Char;  //870
  zParams: array[0..79] of Char;
  zDir: array[0..MAX_PATH] of Char;       //870
begin
  Result := ShellExecute(Application.Handle,
    StrPCopy(zOperation, Operation),
    StrPCopy(zFileName, FileName),
    StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
  if Result <= 32 then
//    MessageDlg('ERROR - Can''t ' + Operation + ' file  ' +
    MessageDlg(esCannot + Operation + ewFile +
      FileName, mtError, [mbOK], 0);   //872ln
end; {= ExecuteFile =}

function DoSHFileOp(Handle: THandle; OpMode: UInt; Src: string;
  Dest: string; var Aborted: Boolean): Boolean;
var
  ipFileOp: TSHFileOpStruct;
begin
  Src := AddNullToStr(Src);
  Dest := AddNullToStr(Dest);
  FillChar(ipFileOp, SizeOf(ipFileOp), 0);
  with ipFileOp do
  begin
    wnd := GetActiveWindow;  //Handle;
    wFunc := OpMode;
    pFrom := pChar(Src);
    pTo := pChar(Dest);
    fFlags := FOF_ALLOWUNDO;  // or FOF_RENAMEONCOLLISION;
    fAnyOperationsAborted := Aborted;
    hNameMappings := nil;
    lpszProgressTitle := '';
  end;
  Result := SHFileOperation(ipFileOp) = 0;
  if ipFileOp.fAnyOperationsAborted = True then
    Result := False;
end; {= DoSHFileOp =}

function AddNullToStr(Path: string): string; //70
begin
  if Path = '' then exit;
  if Path[Length(Path)] <> #0 then
    Result := Path + #0
  else
    Result := Path;
end; {= AddnullToStr =}

function StrContains(Str1, Str2: string): Boolean;
var
  i: Integer;
begin
  for i := 1 to Length(Str1) do
    if Pos(Str1[i], Str2) <> 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end; {= StringCountains =}

function BrowseForFolder(const FormHandle: HWND; var DirPath: string;
  nRoot: integer; inclFiles: Boolean): Boolean;   //880<
// nRoot:     1 = DESKTOP    2 = Common_Program    3 = Printers
//            4 = Personal   5 = Common_StartMenu  6 = Network
// inclFiles: ulFlags = BIF_BROWSEINCLUDEFILES;
var
  pidl: PItemIDList;
  FBrowseInfo: TBrowseInfo;
  Success: Boolean;
  TitleName: string;
  Buffer: array[0..Max_Path] of Char;
  iGetRoot: PItemIDList;
begin
  Result := False;
  ZeroMemory(@FBrowseInfo, SizeOf(FBrowseInfo));
  try
    GetMem(FBrowseInfo.pszDisplayName, MAX_PATH);
    FBrowseInfo.HWndOwner := FormHandle;
//    TitleName := 'Please specify a directory';   //872ln
    TitleName := esSpecifyDir;  //872ln
    FBrowseInfo.lpszTitle := PChar(TitleName);
    if inclFiles = True then
      FBrowseInfo.ulFlags := BIF_BROWSEINCLUDEFILES;  //
    try
      case nRoot of
      1:  SHGetSpecialFolderLocation(0, CSIDL_DESKTOP, iGetRoot);
      2:  SHGetSpecialFolderLocation(0, CSIDL_COMMON_PROGRAMS, iGetRoot);
      3:  SHGetSpecialFolderLocation(0, CSIDL_PRINTERS, iGetRoot);
      4:  SHGetSpecialFolderLocation(0, CSIDL_PERSONAL, iGetRoot);
      5:  SHGetSpecialFolderLocation(0, CSIDL_COMMON_STARTMENU, iGetRoot);
      6:  SHGetSpecialFolderLocation(0, CSIDL_NETWORK, iGetRoot);
      end;
    finally
      FBrowseInfo.pidlRoot := iGetRoot;
    end;
    pidl := SHBrowseForFolder(FBrowseInfo);
    if pidl <> nil then
    begin
      Success := SHGetPathFromIDList(pidl, Buffer);
      if Success then
      begin
        DirPath := Buffer;
        if DirPath[Length(DirPath)] <> '\' then
          DirPath := DirPath + '\';
        result := True;
      end;
      GlobalFreePtr(pidl);
    end;
  finally
    if Assigned(FBrowseInfo.pszDisplayName) then
      FreeMem(FBrowseInfo.pszDisplayName, Max_Path);
  end;
end; {= BrowseForPath =}  //880>

function numpos(a: char; b: string; c: integer): integer;
var
  it: integer;
  az: integer;
begin
  result := maxint;
  if length(b) > 0 then
  begin
    az := 0;
    for it := 1 to length(b) do
      if b[it] = a then
      begin
        inc(az);
        if az = c then
        begin
          result := it;
          exit;
        end;
      end;
  end;
end; {= numpos =}

function PosCount(a: char; b: string): integer;
var
  i: integer;
begin
  result := 0;
  if length(b) > 0 then
    for i := 1 to length(b) do
      if b[i] = a then inc(result);
end; {= getcount =}

function LastPos(ch: char; s: string): Integer;
var
  i: integer;
begin
  Result := 0;
  for i := Length(s) downto 1 do
  begin
    if s[i] = ch then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function GetDiskSize(Root: string): Int64;  //LongInt; //874
var
  DrvID: Byte;
begin
  Root := UpperCase(Root);
  DrvID := Ord(Root[1]) - 64;
  Result := DiskSize(DrvID) div 1024;  //in KB
end; {= GetDiskSize =}

function GetFreeDiskSize(Root: string): Int64;  //LongInt;  //874
var
  DrvID: Byte;
begin
  Root := UpperCase(Root);
  DrvID := Ord(Root[1]) - 64;
  Result := DiskFree(DrvID) div 1024;  //in KB
end; {= GetFreeDiskSize =}

function DiskinDrive(Drive: Char; ShowMsg: word): Boolean;
var
  ErrorMode: word;
begin
  if Drive in ['a'..'z'] then
    Dec(Drive, $20);
  if not (Drive in ['A'..'Z']) then
//    MessageDlg('Not a valid Drive ID', mtError, [mbOK], 0);
    MessageDlg(esInvalidDrvID, mtError, [mbOK], 0);  //872ln

  ErrorMode := SetErrorMode(SEM_FailCriticalErrors);
  try
    if DiskSize(Ord(Drive) - $40) = -1 then
    begin
      if ShowMsg > 0 then
      begin
        MessageBeep(MB_IconHand);
//        MessageDlg('There is no disk in Drive ' + Drive + #13 +
//                   'or Drive ' + Drive + ': is not ready',
//                   mtWarning, [mbOK], 0);                //872ln
        MessageDlg(esDrvNotReady, mtWarning, [mbOK], 0);   //872ln
      end;
      Result := False
    end
    else
      Result := True;
  finally
    SetErrorMode(ErrorMode);
  end;
end; {DiskinDrive}

function SlashSep(Path, FName: string): string;
begin
  if (Path = '') or (FName = '') then exit;
  Result := AddSlash(Path) + FName;
end; {SlashSep}

function AddSlash(Path: string): string;
begin
  if Path = '' then exit;
  if Path[Length(Path)] <> '\' then
    Result := Path + '\'
  else
    Result := Path;
end; {= AddSlash =}

function DelSlash(Path: string): string;
begin
  Result := Path;
  if Path <> '' then
    if Path[Length(Path)] = '\' then
      Delete(Result, Length(Path), 1);
end; {= DelSlash =}

function FileTimeToDateTimeStr(FTime: TFileTime; DFormat,
  TFormat: string): string;  //70
var
  SysTime       : TSystemTime;
  DateTime      : TDateTime;
  LocalFileTime : TFileTime;
begin
  FileTimeToLocalFileTime(Ftime, LocalFileTime);
  FileTimeToSystemTime(LocalFileTime, SysTime);
  DateTime := SystemTimeToDateTime(SysTime);
  Result := FormatDateTime(DFormat + ' ' + TFormat, DateTime);
end; {= FileTimeToDateTimeStr =}

function FileDirExist(FDName: string): Boolean;
var
  SRec: TSearchRec;
  FName: string;
begin
  FillChar(SRec, SizeOf(TSearchRec), 0);
  Result := FindFirst(AddNullToStr(FDName), faAnyFile or faDirectory, SRec) = 0;
  if Result then
  begin
    FName := ExtractFileName(DelSlash(FDName));
    if (FName[Length(FName)] = #0) then
      FName := Copy(FName, 1, Length(FName) - 1);
//    MessageDlg('"' + FName + '" already exists !', mtError, [mbOK], 0);  //872ln
    MessageDlg('"' + FName + '" ' + esExists, mtError, [mbOK], 0);  //872ln
  end;
  SysUtils.FindClose(SRec);
end; {= FileDirExist =}

function GetNormalIcon(Path: string): integer;
var
  sfi: TShFileInfo;
begin
  SHGetFileInfo(Pchar(Path), 0, sfi, SizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  Result := sfi.iIcon;
end; {= GetNormalIcon =}

function GetSelectedIcon(Path: string): Integer;
var
  sfi: TShFileInfo;
begin
  SHGetFileInfo(Pchar(Path), 0, sfi, sizeOf(TSHFileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_OPENICON);
  Result := sfi.iIcon;
end; {= GetSelectedIcon =}

function ConvertSize(FSize: Int64; FAttr: string): String;
begin
  if (FSize < 1024) and (FSize > 0) then
    FSize := 1024;
  Result := IntToStr(FSize div 1024) + ' KB';  //80
  if (UpperCase(FAttr) = 'DIR') then
    Result := ' ';  //'0';   //80
end; {= ConvertSize =}

function GetSystemPath(SysPath: TSystemPath): string;  //874>
var
  p: PChar;
  Reg: TRegistry;
begin
  Result := '';
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders',
      True) then
    begin
      Case SysPath of
        spDeskTop    :  Result := Reg.ReadString('DeskTop');
        spFavorites  :  Result := Reg.ReadString('Favorites');
        spNetHood    :  Result := Reg.ReadString('NetHood');
        spPersonal   :  Result := Reg.ReadString('Personal');
        spPrograms   :  Result := Reg.ReadString('Programs');
        spSendTo     :  Result := Reg.ReadString('SendTo');
        spStartMenu  :  Result := Reg.ReadString('Start Menu');
        spStartUp    :  Result := Reg.ReadString('StartUp');
        spWinRoot    :  Begin
                        GetMem(p, MAX_PATH);
                        GetWindowsDirectory(p, MAX_PATH);
                        Result := StrPas(p);
                        FreeMem(p);
                      end;
        spWinSys     :  begin
                        GetMem(p, MAX_PATH);
                        GetSystemDirectory(p, MAX_PATH);
                        Result := StrPas(p);
                        FreeMem(p);
                      end;
      end;  //case
    end;  //Reg. ..
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
  if (Result <> '') and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
end;  {= GetSystemPath =}  //874<

function ExtractFileNameOnly(FName: string): String;  //85 >>
var
  Ext: string;
begin
  Result := ExtractFileName(FName);
  Ext := ExtractFileExt(FName);
  if Ext <> '' then
    Delete(Result, Pos(Ext, Result), Length(Ext));
end;  {= ExtractFileNameOnly =}  //85 <<

procedure ShowProperties(FName: string);    //862 >>
var
  sei: SHELLEXECUTEINFO;
  err: longint;
begin
  try
    ZeroMemory(addr(sei),sizeof(sei));
    sei.cbSize := sizeof(sei);
    sei.lpFile := pchar(FName);
    sei.lpVerb := 'properties';
    sei.fMask  := SEE_MASK_INVOKEIDLIST;
    ShellExecuteEx(addr(sei));
  except
    err:=getlasterror;
    case err of
      ERROR_FILE_NOT_FOUND: showmessage('The specified file was not found.');
      ERROR_PATH_NOT_FOUND: showmessage('The specified path was not found.');
      ERROR_DDE_FAIL: showmessage('The DDE transaction failed.');
      ERROR_NO_ASSOCIATION: showmessage(
        'There is no application associated with the given filename extension.');
      ERROR_ACCESS_DENIED: showmessage('Access denied');
      ERROR_DLL_NOT_FOUND : showmessage('DLL not found');
      ERROR_CANCELLED : showmessage('The function prompted the user for the ' +
        'location of the application, but the user cancelled the request.');
      ERROR_NOT_ENOUGH_MEMORY: showmessage('Not enough memory for this operation');
      ERROR_SHARING_VIOLATION: showmessage('Sharing violation');
    end;
  end;
end;  {= ShowFileProperties =}  //862 <<

procedure Delay(mSecs: integer);  //878>
var
  FirstTickCount: Cardinal;  //879
begin
  FirstTickCount := GetTickCount;
  repeat
    Application.ProcessMessages;
  until (LongInt(GetTickCount - FirstTickCount) >= LongInt(mSecs));  //879
end;  {= Delay =}   //878

function GetVolumeName(DriveLtr: Char): String;  //877<
var
  MaximumComponentLength : dWord;
  FileSystemFlags : dWord;
  VolumeName : String;
begin
 SetLength(VolumeName, MAX_PATH);
 GetVolumeInformation(PChar(DriveLtr+':\'),
                      PChar(VolumeName),
                      Length(VolumeName),
                      nil,
                      MaximumComponentLength,
                      FileSystemFlags,
                      nil,
                      0);
 Result := Copy(VolumeName, 0, StrLen(PChar(VolumeName)));
end;  {= GetVolumeName =}  //877>

function ConnectDrive(_drvLetter: string; _netPath: string; _showError: Boolean;
  _reconnect: Boolean): DWORD;   //879+<
var
  nRes: TNetResource;
  errCode: DWORD;
  dwFlags: DWORD;
begin
  FillChar(NRes, SizeOf(NRes), #0);
  nRes.dwType := RESOURCETYPE_DISK;
  // Set Driveletter and Networkpath
  nRes.lpLocalName  := PChar(_drvLetter);
  nRes.lpRemoteName := PChar(_netPath);
  // Check if it should be reconnected at logon
  if _reconnect then
    dwFlags := CONNECT_UPDATE_PROFILE and CONNECT_INTERACTIVE
  else
    dwFlags := CONNECT_INTERACTIVE;
  errCode := WNetAddConnection3(Application.Handle, nRes, nil, nil, dwFlags);
  // Show Errormessage, if flag is set
  if (errCode <> NO_ERROR) and (_showError) then
  begin
    Application.MessageBox(PChar('An error occured while connecting:' + #13#10 +
      SysErrorMessage(GetLastError)), 'Error while connecting!', MB_OK);
  end;
  Result := errCode; { NO_ERROR }
end;  {= ConnectDrive =}   //879+>

function IsWindowsXP: Boolean;  //880<
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and
            (Win32MajorVersion >= 5) and
            (Win32MinorVersion >= 1);
end;  {= IsWindowsXP =}  //880>

Function DeleteNonNumberChars(Const InString: String): string;  //882>
  var
    S: string;
    ch: char;
    n, i: integer;
  begin
    n := Length(InString);
    S := '';
    i := 1;
    while (i <= n) do
    begin
      Ch := InString[i];
      if (ch in ['0'..'9', '-', '+']) or (Ch = DecimalSeparator) then
      begin
        S := S + Ch;
      end;
      i := i + 1;
    end;  //while
//    if InString = #32 then S := '0';
    Result := S;
  end;  //  {= DeleteNonNumberChars =}  //882

//============== End of Global Functions ================//



{*********************************************************}
{                     LsDirTreeView                       }
{      the Drop-down TreeView of TLsDirTreeCombo28        }
{*********************************************************}

procedure TLsDirTreeView.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;  //862
    WindowClass.Style := CS_SAVEBITS;
  end;
end;  {= CreateParams =}

procedure TLsDirTreeView.CreateWnd;
begin
  inherited CreateWnd;
//  Font.Size := 8;                 //82
//  Font.Name := 'MS Sans Serif';   //82
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, wm_SetFocus, 0, 0);
end; {= CreateWnd =}

procedure TLsDirTreeView.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseInControl := True;
  ReleaseCapture;
end; {= CMMouseEnter =}

procedure TLsDirTreeView.CMMouseLeave(var Message: TMessage);
var
  x, y: integer;
  PtPos: TPoint;
begin
  inherited;
  FMouseInControl := False;
  if not visible then
  begin
    x := 0;
    y := 0;
    PtPos := Point(x, y);
    PtPos := ClientToScreen(PtPos);
    SetCaptureControl(ControlAtPos(PtPos, False));
  end
  else
    SetCaptureControl(Self);
end; {= CMMouseLeave =}

procedure TLsDirTreeView.WMMouseMove(var Message: TWMMouseMove);
var
  TreeHitTest: THitTests;
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    TreeHitTest := GetHitTestInfoAt(Message.XPos, Message.YPos);
    if htOnLabel in TreeHitTest then
      Selected := GetNodeAt(Message.XPos, Message.YPos);
  end;
end; {= WMMouseMove =}

procedure TLsDirTreeView.WMLButtonDown(var Message: TWMLButtonDown);
var
  HitTest: THitTests;
//  DrvCh: Char;
begin
  inherited;
  case FMouseInControl of
    False:
      CloseTree;  //876++
    True:
      begin
        SendMessage((Owner as TLsDirTreeCombo28).Handle, WM_LBUTTONDOWN, 0, 0);
        HitTest := GetHitTestInfoAt(Message.XPos, Message.YPos);
        if (htOnItem in HitTest) or (htOnIcon in HitTest) then
        begin
          SelectPath;  //876++
          with (Owner as TLsDirTreeCombo28) do
          begin
            SetSelectedPath(FSelectedPath);
            ImageIndex := Tree.Selected.ImageIndex;
            Text := Selected.Text;
            if Assigned(FOnPathChange) then
              FOnPathChange(Self, FSelectedPath);  //83
          end;
          CloseTree;  //876++
        end; //Hittest
      end;
  end;  //Case
end; {= WMLButtonDown =}

procedure TLsDirTreeView.SelectPath;  //876++
var
  DrvCh: Char;
begin
  FPreviousPath := AddSlash(FSelectedPath);
  MakePath(Selected);
  if (Selected = Items[0]) then
    FSelectedPath := 'My Computers'  //'Drives'  //855r2
  else
    FSelectedPath := TreeViewPath;

  if Selected.Level = 1 then  //875>
    if GetDriveType(PChar(FSelectedPath)) in
      [DRIVE_REMOVABLE, DRIVE_CDROM] then
    begin
      DrvCh := FSelectedPath[1];
      if not DiskInDrive(DrvCh, 1) then
      begin
        OpenPath(FPreviousPath);
        FSelectedPath :=FPreviousPath;
        exit;
      end;
    end;  //875<
  FPreviousPath := FSelectedPath;  //875
end; {= SelectPath =}  //876++

procedure TLsDirTreeView.Expand(Node: TTreeNode);
begin
  Items.BeginUpdate;
  Node.AlphaSort;
  Items.EndUpdate;
  inherited Expand(Node);
end; {= Expand =}

procedure TLsDirTreeView.LoadDrives;
var
  ADrive: integer;
  DriveLetter: char;
  DriveString: string;
  DrvName: string;
  Sfi: TSHFileInfo;
  Root: TTreenode;
  idRoot: PItemIDList;
begin
  Root := nil;
  Items.BeginUpdate;
  Items.Clear;
  if SHGetSpecialFolderLocation(Handle, CSIDL_DRIVES, idRoot) = NOERROR then
    if SHGetFileInfo(PChar(idRoot), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_PIDL
      or SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME) <> 0 then
    begin
      Root := items.AddFirst(nil, Sfi.szDisplayName);
      Root.ImageIndex := Sfi.iIcon;
      Root.SelectedIndex := Sfi.iIcon;
    end;
  Integer(Drives) := GetLogicalDrives;
  for ADrive := 0 to 25 do
  begin
    if (ADrive in Drives) then
    begin
      DriveLetter := Chr(ADrive + ord('A'));
      DriveString := DriveLetter + ':\';
      SHGetFileInfo(PChar(DriveString), 0, Sfi, SizeOf(Sfi),
        SHGFI_DISPLAYNAME);
      DrvName := Copy(Sfi.szDisplayName, 1, (Pos('(', Sfi.szDisplayName) - 1));
      with Items do
      begin
        AddChild(Root, ' (' + DriveLetter + ':)  ' + DrvName);
        Items[Count - 1].HasChildren := true;
        Items[Count - 1].ImageIndex := GetNormalIcon(DriveString);
        Items[Count - 1].SelectedIndex := GetSelectedIcon(DriveString);
      end;
    end;
  end;
  Items.EndUpdate;
end; {= LoadDrives =}

procedure TLsDirTreeView.MakePath(Node: TTreeNode);

  procedure MakeSubPath;
  begin
    if Node.Level = 1 then
      TreeViewPath := Copy(Node.Text, 3, 2) + '\' + TreeViewPath
    else if Node.Level > 1 then
      if TreeViewPath = '' then
        TreeViewPath := Node.Text
      else
        TreeViewPath := Node.Text + '\' + TreeViewPath;
  end; {= MakeSubPath =}

begin
  TreeViewPath := '';
  MakeSubPath;
  while Node.Parent <> nil do
  begin
    Node := Node.Parent;
    MakeSubPath;
  end;
  if TreeViewPath = '' then
    TreeViewPath := 'My Computer';  //'Drives';  //876++  //885r2
end; {= MakePath =}

procedure TLsDirTreeView.AddSubs(Path: string; Node: TTreeNode);
var
  ANode: TTreeNode;
  APath: string;
  hFindFile: THandle;
  Win32FD: TWin32FindData;

  function IsDirectory(dWin32FD: TWin32FindData): Boolean;
  var
    FName: string;
  begin
    FName := StrPas(dWin32FD.cFileName);
    with dWin32FD do
      Result := (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
        FILE_ATTRIBUTE_DIRECTORY) and (FName <> '.') and (FName <> '..');
  end; {= IsDirectory =}

  function HasSubs(sPath: string): Boolean;
  var
    sAPath: string;
    shFindFile: THandle;
    sWin32FD: TWin32FindData;
  begin
    Result := False;
    sAPath := sPath;
    sAPath := AddSlash(sAPath);
    shFindFile := FindFirstFile(PChar(sAPath + '*.*'), sWin32FD);
    if shFindFile <> INVALID_HANDLE_VALUE then
    try
      repeat
        if IsDirectory(sWin32FD) then
        begin
          Result := True;
          Break;
        end;
      until not FindNextFile(shFindFile, sWin32FD);
    finally
      Windows.FindClose(shFindFile);
    end;
  end; {= HasSubs =}

begin
  APath := Path;
  APath := AddSlash(APath);
  hFindFile := FindFirstFile(PChar(APath + '*.*'), Win32FD);
  if hFindFile <> INVALID_HANDLE_VALUE then
  try
    repeat
      if IsDirectory(Win32FD) then
      begin
        ANode := Items.AddChild(Node, Win32FD.cFileName);
        ANode.HasChildren := HasSubs(APath + Win32FD.cFileName);
        ANode.ImageIndex := GetNormalIcon(APath + Win32FD.cFileName);
        ANode.SelectedIndex := GetSelectedIcon(APath + Win32FD.cFileName);
      end;
    until not FindNextFile(hFindFile, Win32FD);
  finally
    Windows.FindClose(hFindFile);
  end;
end; {= AddSubs =}

procedure TLsDirTreeView.Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  OldCursor: TCursor;  //872
begin
  if Node.GetFirstChild = nil then
  begin
    OldCursor := Screen.Cursor;   //872
    Screen.Cursor := crHourGlass; //872
    try
      MakePath(Node);
      Node.HasChildren := false;
      AddSubs(TreeViewPath, Node);
      Node.AlphaSort;
    finally
      Screen.Cursor := OldCursor;  //872
    end;
  end;
end; {= Expending =}

procedure TLsDirTreeView.OpenPath(sPath: string);  //875<
var
  CurItem: TTreeNode;
  count: Integer;
  TempPath: string;
  CurPath: string;
begin
  Items.BeginUpdate;
  try  //879
    CurItem := Items.GetFirstNode;
    if CurItem.Expanded then
      CurItem.Collapse(True);
    while Pos('\', sPath) > 0 do
    begin
      count := Pos('\', sPath);
      tempPath := Copy(sPath, 1, count);
      sPath := Copy(sPath, count + 1, Length(sPath));
      CurItem := CurItem.getFirstChild;
      while CurItem <> nil do
      begin
        if CurItem.Level = 1 then
          CurPath := Copy(CurItem.Text, 3, 2) + '\'
        else if CurItem.Level > 1 then
          CurPath := CurItem.Text + '\';
        if AnsiCompareText(CurPath, TempPath) = 0 then //51
        begin
          CurItem.Selected := True;
          CurItem.Expand(False);
          Break;
        end;
        CurItem := CurItem.getNextSibling;  //CurItem.GetNext;  //876+
        if CurItem = nil then
          exit;
      end;
    end;
  finally  //879
    Items.EndUpdate;
  end;     //879
end; {= OpenPath =}  //875>

procedure TLsDirTreeView.CloseTree;  //876++
begin
  if Enabled then
    Parent.SetFocus;
  ReleaseCapture;
  Enabled := False;
  Visible := False;
end;  {= CloseTree =}  //876++

//============== End of TLsDirTreeView ==================//



{*********************************************************}
{                   TLsDirTreeCombo28                     }
{*********************************************************}

constructor TLsDirTreeCombo28.Create;
var
  sfi: TShFileInfo;
  theFunction: SHChangeNotifyRegister;  //881+
  hDLL: THandle;                        //881+
  buf: array [0..144] of char;          //881+
begin
  inherited Create(AOwner);  //AOwner);
  Width := 216;
  Height := 21;
  TabOrder := 0;
  ReadOnly := True; //53+
  if Text <> '' then
    HideSelection := True;  //862

  FCanvas := TControlCanvas.create;
  FCanvas.Control := self;

  Btn := TLsSpeedButton.Create(Self);  //862
  Btn.Parent := Self;

  Tree := TLsDirTreeView.Create(Self);
  with Tree do
  begin
    Parent := Self;
    Top := Self.Top + Self.Height;
    Height := 240;
    Visible := False;  //85
    FOldWindowProc := WindowProc;  //879_MouseWheel+
    TNewWindowProc.Create(Self);   //879_MouseWheel
  end;

  ImagLst := TImageList.Create(Self);
  try
    FWinDir := GetSystemPath(spWinRoot);  //874
    ImagLst.Handle := SHGetFileInfo(PChar(FWinDir), 0, sfi, sizeOf(sfi),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
//    ImagLst.BkColor := clNone;  //871
    ImagLst.ShareImages := True;
//    ImagLst.BlendColor := clHighLight;  //871
  finally
    Tree.Images := ImagLst;
  end;

  FMessageHandle := AllocateHWnd(ProcessMsg);  //877>
  // Creates a new dynamic variable
  new(NotifyRegister);
  with NotifyRegister^ do
  begin
    pidlPath      := nil;
    bWatchSubtree := true;
  end; //877<

  // Dynamically load Shell32.dll  //881+
  hDLL := LoadLibrary(StrPCopy(buf, 'shell32.dll'));  //881+<
  if hDLL <> 0 then
  begin
    try
      @theFunction := GetProcAddress(hDLL, MAKEINTRESOURCE(2));
      if @theFunction <> nil then
      begin
      // Register a change notification handle.
        NotifyHandle := theFunction(
                    fMessageHandle,
                    SHCNF_ACCEPT_INTERRUPTS + SHCNF_ACCEPT_NON_INTERRUPTS,
                    SHCNE_MKDIR             // events to be watched
                    + SHCNE_RENAMEFOLDER
                    + SHCNE_RMDIR
                    + SHCNE_DRIVEADD
                    + SHCNE_DRIVEREMOVED,
                    WM_DIRCHANGE,           // message for notifications.
                    1,                      // Only DeskTop being watched.
                    NotifyRegister);        // Indicates what's being watched.

        if NotifyHandle = 0 then
          raise Exception.Create('Unable to register SHChangeNotify'); //877<
      end else
        raise Exception.Create('Unable to link to function SHChangeNotifyRegister');
    finally
      FreeLibrary(hDLL);
    end;  //try
  end  //if hDLL <> 0
  else
    raise Exception.Create('Unable to load Shell32.dll');
  //881+>
  ShowHint := True;   //876++
  FAbout := 'Version 2.8.8.6';
end; {= Create =}

destructor TLsDirTreeCombo28.destroy;
var
  i: integer;
  theFunction: SHChangeNotifyDeregister;  //881+
  hDLL: THandle;                          //881+
begin
  DeAllocateHWnd(FMessageHandle);  //877

  try   //881+>
    if assigned(NotifyRegister) then
      Dispose(NotifyRegister);
  finally
    NotifyRegister := nil;
  end;

  if NotifyHandle <> 0 then
  begin
    hDLL := 0;
    try
      hDLL := LoadLibrary('shell32.dll');  //881+
      if hDLL <> 0 then
      begin
          @theFunction := GetProcAddress(hDLL, MAKEINTRESOURCE(4));
          if @theFunction <> nil then
            theFunction(NotifyHandle);
      end;
    finally
      NotifyHandle := 0;
      FreeLibrary(hDLL);
    end;
  end;  //881+<

  for i := Tree.Items.Count - 1 downto 0 do //70
    Tree.Items[i].Free;
  ImagLst.Free;
//  TNewWindowProc(Self).Restore;  //879_MouseWheel+
  Tree.WindowProc := FOldWindowProc;  //879_MouseWheel+
  inherited Destroy;
end; {= destroy =}

procedure TLsDirTreeCombo28.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
end; {= CreateWnd =}

procedure TLsDirTreeCombo28.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.style := Params.style or ES_MULTILINE or WS_CLIPCHILDREN;  //862
end;  {= CrateParams =}

procedure TLsDirTreeCombo28.CreateWindowHandle(const Params: TCreateParams);
begin
  inherited CreateWindowHandle(Params);
//  Font.Name := 'MS Sans Serif';  //82
//  Font.Size := 8;                //82
  with Btn do
  begin
    Top := 0;  //Self.Top + 1; //870
    Left := Self.Left + Self.Width - 21;  //19;  //85
    Width := 17;
    Height := 17;
    Cursor := crArrow;
    Down := False;
    OnClick := BtnClick;
    Glyph.Handle := LoadBitmap(0, pChar(OBM_COMBO));
    NumGlyphs := 1;
  end; {Btn}

  with Tree do
  begin
    Left := 0;
    Top := 0;
    Width := 0;
    BorderStyle := bsSingle;
    Ctl3D := False;
    LoadDrives;
    OnExpanding := Expanding;
    ExpandRoot := True;
    ReadOnly := True;
    Enabled := False;
    Visible := False;
  end; {Tree}

  if Tree.Selected = nil then
  begin
    ImageIndex := Tree.Items[0].ImageIndex;
    Text := Tree.Items[0].Text;
    Tree.FSelectedPath := 'My Computer';    //'Drives';  //Tree.Items[0].GetNamePath;  //876+ //885r2
  end;

  if not (csDesigning in ComponentState) then  //876++<
  begin
    with Application do
    begin
      HintColor := clInfoBk;
      HintShortPause := 25;
      HintPause := 500;
      HintHidePause := 10000;
    end;
  end;  //876++>
end; {= CreateWindowHandle =}

procedure TLsDirTreeCombo28.SetEditRect;
var
  Loc: TRect;
begin
  if (ImageIndex >= 0) and (ImageIndex < Tree.Images.Count) then
  begin
    SetRect(Loc, 23, 0, ClientWidth - Btn.Width - 2, ClientHeight + 1);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  end
  else
  begin
    SetRect(Loc, 0, 0, ClientWidth - Btn.Width - 2, ClientHeight + 1);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@Loc));
  end;
end; {= SetEditRect =}

procedure TLsDirTreeCombo28.BtnClick(Sender: TObject);
var
  CP, SP: TPoint;
begin
  SetFocus;  //876++
  CP.X := Left;
  CP.Y := Top + Height;
  SP := Parent.ClientToScreen(CP);
  if (SP.Y + Tree.Height) > Screen.Height then  //85<
  begin
    CP.Y := Top - Tree.Height;
    SP := Parent.ClientToScreen(CP);
  end;  //85>
  with Tree do
  begin
    Left := SP.X;
    Top := SP.Y;
    Width := Self.Width;
    if Enabled = False then
    begin
      Enabled := True;
      Visible := True;
      BringToFront; //60
      SetCaptureControl(Tree);
      if not Focused then
        SetFocus;  //862
    end
    else
    begin
      SendToBack; //60
      Tree.CloseTree;  //876++
    end;
  end;
end; {= BtnClick =}

procedure TLsDirTreeCombo28.SetBounds(Left, Top, Width, Height: Integer);
begin
  case Parent <> nil of
    True:
      begin
        inherited SetBounds(Left, Top, Width, Height);
        with Btn do
        begin
          Left := Self.Width - Btn.Width - 4;
          Height := Self.Height - 4;
        end;
        SetEditRect;
      end;
    False: inherited SetBounds(Left, Top, Width, Height);
  end;
end; {= SetBounds =}

procedure TLsDirTreeCombo28.OpenPath(dPath: string);
var
  FullPath: string; //51
begin
  if (dPath = '') or (Length(dPath) = 1) then exit; //53
  if not DirectoryExists(dPath) then exit; //53
  dPath := AddSlash(dPath); //51
  FullPath := dPath;

  Tree.OpenPath(dPath);   //875

  ImageIndex := Tree.Selected.ImageIndex;
  Text := Tree.Selected.Text; //875
  if AnsiCompareText(Tree.FSelectedPath, FullPath) <> 0 then //51
    Tree.FSelectedPath := FullPath; //51
  if Assigned(FOnPathChange) then
    FOnPathChange(Self, Tree.FSelectedPath);  //84
end; {= OpenPath =}

procedure TLsDirTreeCombo28.ResetTreeView;
begin
  with Tree do
  begin
    Items.BeginUpdate;
    try
      Selected := nil;
      Items.Clear;
      LoadDrives;
      OnExpanding := Expanding;
    finally
      Items.EndUpdate;
    end;
  end;
end; {= ResetTreeView =}

procedure TLsDirTreeCombo28.WMPaint(var Message: TWMPaint);
var
  Img: TCustomImageList;
  YPos: integer;
begin
  inherited PaintHandler(Message);
  Cursor := crArrow;  //874
  HideCaret(Self.Handle); //874
  Img := Tree.Images;
  if (ImageIndex >= 0) and (ImageIndex < Tree.Images.Count) then
  begin
    YPos := ClientHeight div 2 - 8;
    Img.Draw(FCanvas, 5, YPos, ImageIndex);
  end;
end; {= WMPaint =}

function TLsDirTreeCombo28.GetSelectedPath: string;
begin
  GetSelectedPath := Tree.FSelectedPath;
end; {= GetSelectedPath =}

procedure TLsDirTreeCombo28.SetSelectedPath(Value: string);
begin
  if AnsiCompareText(Tree.FSelectedPath, Value) <> 0 then
    Tree.FSelectedPath := Value;
  if Assigned(FFileList) then
    FFileList.Directory := Tree.FSelectedPath;
end; {= SetSelectedPath =}

procedure TLsDirTreeCombo28.SetInitialDir(Value: String);  //85<
begin
  if (Value = '') or (AnsiCompareText(FInitialDir, Value) = 0 ) then
    exit;
  Value := AddSlash(Value);
  if not DirectoryExists(Value) then
    exit
  else begin
    FInitialDir := Value;
    OpenPath(FInitialDir);
    if Assigned(FFileList) then
      FFileList.Directory := FInitialDir;
  end;
end;  {= SetInitialDir =}  //85>

function TLsDirTreeCombo28.GetFileList: TLsFilelistView28;
begin
  GetFileList := FFileList;
end; {= GetFileList =}

procedure TLsDirTreeCombo28.SetFileList(Value: TLsFilelistView28);
begin
  if FFileList <> Value then
    FFileList := Value;
  if (FFileList <> nil) and (Tree.FSelectedPath <> '') then
  begin
    FFileList.Directory := Tree.FSelectedPath;
    FFileList.UpdateFileList;
  end;
end; {= SetFileList =}

function TLsDirTreeCombo28.GetExpandRoot: Boolean;
begin
  GetExpandRoot := Tree.FExpandRoot;
end; {= GetExpandRoot =}

procedure TLsDirTreeCombo28.SetExpandRoot(Value: Boolean);
begin
  Tree.FExpandRoot := Value;
  with Tree do
    if FExpandRoot and Assigned(Items[0]) then
      Items[0].Expand(False);
end; {= SetExpandRoot =}

procedure TLsDirTreeCombo28.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFileList) then
    FFileList := nil;
end; {= Notification =}

function TLsDirTreeCombo28.GetBtnGlyph: TBitmap;
begin
  GetBtnGlyph := Btn.Glyph;
end; {= GetBtnGlyph =}

procedure TLsDirTreeCombo28.SetBtnGlyph(NewValue: TBitmap);
begin
  if NewValue <> nil then
    Btn.Glyph.Assign(NewValue);
end; {= SetBtnGlyph =}

function TLsDirTreeCombo28.GetTreeHeight: integer;
begin
  GetTreeHeight := Tree.Height;
end; {= GetTreeHeight =}

procedure TLsDirTreeCombo28.SetTreeHeight(newValue: Integer);
begin
  Tree.Height := newValue;
end; {= SetTreeHeight =}

function TLsDirTreeCombo28.GetHideSelection: Boolean;
begin
  GetHideSelection := Tree.HideSelection;
end; {= GetHideSelection =}

procedure TLsDirTreeCombo28.SetHideSelection(Value: Boolean);
begin
  Tree.HideSelection := Value;
end; {= SetHideSelection =}

function TLsDirTreeCombo28.GetSelected: TTreeNode;  //875
begin
  GetSelected := Tree.Selected;
end;  {= GetSelected =}

procedure TLsDirTreeCombo28.SetSelected(Value: TTreeNode);  //875
begin
  Tree.Selected := Value;
end;  {= SetSelected =}

procedure TLsDirTreeCombo28.Keydown(var Key: Word; Shift: TShiftState);
var
  CurItem: TTreeNode;
begin
  case Key of

    VK_LEFT, VK_UP,
    VK_RIGHT, VK_DOWN:
      begin
        if (Selected = nil) then
        begin
          Selected := Tree.Items[0];
          Tree.Selected.Expand(False);
        end;

        if (key = VK_RIGHT) then
            Tree.Selected.Expand(False);  //Expand Selected Node

        if (key = VK_LEFT) then  //Collapse Selected Node
        begin
          with Tree do
          begin
            if Selected.Expanded then
              Selected.Collapse(True);
            if (Selected.getPrevSibling = Items[0]) or
               (Selected.GetPrevVisible = Items[0]) or
               (Selected = Items[0]) then exit;
            CurItem := Selected.GetPrevVisible;
            CurItem.Selected := True;
          end;
        end;  //vk_left

        if (key = VK_DOWN) then  //Move to NextChild or nextVisible
        begin
          if (not Tree.Enabled) then   //876++<
          begin
            BtnClick(Self);
            Key := VK_F4;
          end  //876++>
          else begin
            with Tree do
            begin
              if (Selected.GetNext = nil) or
                 (Selected.getNextVisible = nil) then exit; //53
              if Selected.Expanded then
                CurItem := Selected.GetNext
              else
                CurItem := Selected.getNextVisible;
              CurItem.Selected := True;
            end;
          end;
        end;  //vk_down

        if (key = VK_UP) then  //Move to prevChild or prevVisible
        begin
          with Tree do
          begin
            if Selected = Items[0] then exit;
            CurItem := Selected.GetPrevVisible;
            CurItem.Selected := True;
          end;
        end;  //vk_up
      end;  //Key of $25, $26, $27, $28

    VK_F4:  //Activate LsDirTreeView
      begin
        BtnClick(Self);
        if Tree.Visible then
          Tree.SetFocus;
      end;  //vk_F4

    VK_F5:  //872<  Refresh
      begin
        ResetTreeView;
        OpenPath(Tree.FSelectedPath);
        if assigned(FFileList) then
          FFileList.UpdateFileList;
      end; //vk_F5  //872>

    VK_ESCAPE:  //Close LsDirTreeView
      begin
        with Tree do
        begin
          if Enabled = True then
          begin
            Parent.SetFocus;  //876
            Enabled := False;
            Visible := False;
          end;
        end;
      end;  //vk_escape

    VK_RETURN:  //52 set the SelectedPath
      begin
        with Tree do
        begin
          SelectPath;
          SetSelectedPath(FSelectedPath);
          if Assigned(FOnPathChange) then
            FOnPathChange(Self, FSelectedPath);  //83
          if visible then
            Parent.SetFocus;
          SendToBack;
          Enabled := False;
          Visible := False;
        end;
        Text := Tree.selected.Text;
        ImageIndex := Tree.Selected.ImageIndex;  //876++
      end;  //VK_Return
  end;  //case key of
  inherited KeyDown(Key, Shift);
end; {= Keydown =}

procedure TLsDirTreeCombo28.SetAbout(Value: string);  //85
begin
  // Property About is Readonly.
end;

procedure TLsDirTreeCombo28.WMRButtonUp(var Message: TWMRButtonUp);  //862
begin
  Message.Result := 1;
end;  {= WMRButtonUp =} //862

procedure TLsDirTreeCombo28.WMKillFocus
(var Message: TWMKILLfOCUS);  //862<
begin
  if Tree.Enabled then
  begin
    ReleaseCapture;
    Tree.Enabled := False;
    Tree.Visible := False;
  end;
  Inherited;  //876++
end;  {= WMKillFocus =}  //862>

//
procedure TLsDirTreeCombo28.ConnectNetResource(Sender: TObject);  //862+<
var
  sDrvLetter,  //880
    sNetPath: string;  //880
  CurNode: TTreeNode;
  bReConnect: Boolean;
  Success: Boolean;
begin
  Success := False;       //880
  sDrvLetter := '';
  sNetPath := '';
  if IsWindowsXP then
  begin
    if WNetConnectionDialog(Application.Handle, RESOURCETYPE_DISK) = NO_ERROR then
      Success := True;
  end
  else begin
    FNetPathDlg := TNetPathDlg.create(Self);
    if FNetPathDlg.Execute then  //880<
    begin
      sDrvLetter := FNetPathDlg.DrvLetter;
      sNetPath := FNetPathDlg.NetPath;
      bReconnect := FNetPathDlg.Reconnect;
    end else
      exit;
    if ConnectDrive(sDrvLetter, sNetPath, True, bReconnect) = NO_ERROR then
      Success := True;
  end;  //880
  if Success then
  begin
    ResetTreeView;
    Tree.Items[0].Expand(False);
    CurNode := Tree.Items[0].GetLastChild;
    CurNode.Selected := True;
    Tree.FSelectedPath := sDrvLetter + '\';
    OpenPath(Tree.FSelectedPath);
    if Assigned(FFileList) then
      FFilelist.Directory := Tree.FSelectedPath;
  end else
    exit;    //880>
end;  {= ConnectNetResource =}

procedure TLsDirTreeCombo28.DisConnectNetResource(Sender: TObject);  //862+>>
begin
  WNetDisconnectDialog(Application.Handle, RESOURCETYPE_DISK);
  ReSetTreeview;
  Tree.Items[0].Expand(False);
  Tree.Items[0].Selected := True;
  SelectedPath := 'My Computer'; //'Drives';   //885r2
  if Assigned(FFileList) then
  begin
    FFileList.Directory := SelectedPath;
    FFileList.UpdateFileList;
  end;
  OpenPath(Tree.FSelectedPath);
  ImageIndex := Tree.Selected.ImageIndex;
  Text := Tree.Items[0].Text;
end;  {= DisConnectNetResource =}  //862+<<


procedure TLsDirTreeCombo28.CMMouseEnter(var Message: TMessage);  //876++
begin
  inherited;
  if Tree.Visible = False then
  begin
    Self.ShowHint := True;
    if Tree.Selected <> nil then
      Self.Hint := 'SelectedPath =' + #13 + Tree.FSelectedPath
    else
      Self.Hint := 'Selected = nil';
    Application.ShowHint := True;
  end
  else if Tree.Visible = True then
    Self.ShowHint := False;
end;  {= CMMouseEnter =}

procedure TLsDirTreeCombo28.CMMouseLeave(var Message: TMessage);  //876++
begin
  inherited;
  Self.ShowHint := False;
end;  {= CMMouseLeave =}

procedure TLsDirTreeCombo28.ProcessMsg(var Message: TMessage);   //877<
var
  CurPath: string;
  EventID: integer;
begin
  CurPath := Tree.FSelectedPath;
  if Message.Msg = WM_DIRCHANGE then
  begin
    EventID := Message.LParam;
    if (EventID = SHCNE_MKDIR) or
       (EventID = SHCNE_RMDIR) or
       (EventID = SHCNE_RENAMEFOLDER) or
       (EventID = SHCNE_DRIVEADD) or
       (EventID = SHCNE_DRIVEREMOVED) then
    begin
      LockWindowUpdate(Self.Handle);
      ResetTreeView;
      if not Tree.Items[0].Expanded then
        Tree.Items[0].Expand(False);
      if CurPath <> '' then
        OpenPath(CurPath);
      if assigned(FFileList) then
      begin
        FFileList.Directory := CurPath;
        FFileList.UpdateFileList;
      end;
      LockWindowUpdate(0);
    end;
  end  //if Message.Msg
  else  //879
    Message.Result := DefWindowProc(FMessageHandle, Message.Msg,
                                      Message.wParam, Message.lParam);  //879
end;  {= ProcessMsg =}  //877>

//============= End of TLsDirTreeCombo28 ================//



{*********************************************************}
{                     TLsSpeedButton                //862 }
{           Use as button in TLsDEirTreeCombo28           }
{*********************************************************}

procedure TLsSpeedButton.WMRButtonUp(var Message: TWMRButtonUp);  //862>>
begin
  Message.Result := 1;
end;  {= WMRButtonUp =}  //862<<

//============== End of TLsSpeedButton ==================//



{*********************************************************}
{                  TNewWindowProc        //879_MouseWheel }
{        Add MouseWheeel support to TLsDirTreeView        }
{*********************************************************}

constructor TNewWindowProc.Create(WinControl: TWinControl);  //879_MouseWheel<
begin
  with TLsDirTreeCombo28 do
  begin
    FWinCtrl := WinControl;
    FOldWinProc := WinControl.WindowProc;
    WinControl.WindowProc := NewWndProc;
  end;
end;  {= Create =}  //879_MouseWheel>

procedure TNewWindowProc.Restore;   //879_MouseWheel<
begin
  FWinCtrl.WindowProc := FOldWinProc;
end;  {= Restore =}  //879_MouseWheel>

procedure TNewWindowProc.NewWndProc(var Message: TMessage);  //879_MouseWheel
begin
  if (Message.Msg = WM_MOUSEWHEEL) then
  begin
    if (TWMMouseWheel(Message).WheelDelta > 0) then
      SendMessage(FWinCtrl.Handle, WM_KEYDOWN, VK_UP, 0)
    else
      SendMessage(FWinCtrl.Handle, WM_KEYDOWN, VK_DOWN, 0);
  end
  else FOldWinProc(Message);
end;  {= NewWndProc =}  //879_MouseWheel>

//============== End of TNewWindowProc ==================//


//881> MPX3
{*********************************************************}
{                   TLsFileListShortCuts                  }
{*********************************************************}
constructor TLsFileListShortCuts.Create(AOwner : TComponent);
begin
  inherited Create;

  SetDefaults();
end;

destructor TLsFileListShortCuts.Destroy;
begin
  inherited;
end;

procedure TLsFileListShortCuts.Assign(Source: TPersistent);
begin
  if Source is TLsFileListShortCuts then
    with TLsFileListShortCuts(Source) do begin
      Self.scCopy         := scCopy;
      Self.scPaste        := scPaste;
      Self.scCut          := scCut;
      Self.scOpen         := scOpen;
      Self.scView         := scView;
      Self.scNewFolder    := scNewFolder;
      Self.scSelectAll    := scSelectAll;
      Self.scDelete       := scDelete;
      Self.scBack         := scBack;
      Self.scRename       := scRename;
      Self.scFindFile     := scFindFile;
      Self.scRefresh      := scRefresh;

      Self.Actions        := Actions
    end else
      inherited; //raises an exception
end;

procedure TLsFileListShortCuts.SetDefaults();
begin
    FCopy       := ShortCut(Ord('C') , [ssCtrl]);
    FPaste      := ShortCut(Ord('V') , [ssCtrl]);
    FCut        := ShortCut(Ord('X') , [ssCtrl]);
    FOpen       := ShortCut(Ord('O') , [ssCtrl]);
    FView       := ShortCut(Ord('F') , [ssCtrl]);
    FNewFolder  := ShortCut(Ord('N') , [ssCtrl]);
    FSelectAll  := ShortCut(Ord('A') , [ssCtrl]);
    FDelete     := ShortCut(VK_DELETE, []);
    FBack       := ShortCut(VK_BACK  , []);
    FRename     := ShortCut(VK_F2    , []);
    FFindFile   := ShortCut(VK_F3    , []);
    FRefresh    := ShortCut(VK_F5    , []);

    FActions        := [vaCopy, vaPaste, vaCut, vaOpen, vaView, vaNewFolder,
                       vaSelectAll, vaDelete, vaBack, vaRename, vaFindFile,
                       vaRefresh];
end; {= SetDefaults =}

procedure TLsFileListShortCuts.ClearAll();
begin
    FCopy       := 0;
    FPaste      := 0;
    FCut        := 0;
    FOpen       := 0;
    FView       := 0;
    FNewFolder  := 0;
    FSelectAll  := 0;
    FDelete     := 0;
    FBack       := 0;
    FRename     := 0;
    FFindFile   := 0;
    FRefresh    := 0;

    FActions    := [];
end; {= ClearAll =}
//881<
//=========== End of TLsFileListShortCuts ===============//


{********************************************************}
{                  LsFileListItemColors                  }
{********************************************************}
//883cl>
constructor TListItemColors.Create;
begin
  inherited Create;
  //create color objects and assign defaults
  FAttr_Archive  := clBlack;
  FAttr_Hidden   := clGrayText;
  FAttr_Readonly := clTeal;
  FAttr_System   := clRed;
end;  {= Create =}
//883cl<
//============ End of LsFileListItemColors ==============//


{*********************************************************}
{                      TLsDirTree21                  //70 }
{*********************************************************}

procedure TLsDirTree21.CreateWnd;
begin
  inherited CreateWnd;

  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
  if not (csLoading in ComponentState) then
    LoadRoot;
end; {= CreateWnd =}


constructor TLsDirTree21.Create;
var
  sfi: TShFileInfo;
  PathInfo: String;  //881si
//  Pid: PItemIdList;  //881si
  hImgLst: Uint;
  theFunction: SHChangeNotifyRegister;  //881+
  hDLL: THandle;                         //881+
  buf: array [0..144] of char;           //881+
begin
  inherited Create(AOwner);
//  Font.size := 8;                //80
//  Font.name := 'MS Sans Serif';  //80

  Width := 180;
  Height := 120;

  Images := TImageList.Create(Self);
  PathInfo := GetSystemPath(spDeskTop); //spWinRoot); //881si
  hImgLst := SHGetFileInfo(PChar(PathInfo), 0,
                           sfi, SizeOf(sfi),
                           SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if hImgLst <> 0 then
  begin
    Images.Handle := hImgLst;
    Images.BkColor := ClNone;  //878
    Images.ShareImages := True;
  end;

  SetTvLanguage(lnEnglish);  //872ln
  InitializeVar;

  FFolderType := [ftReadOnly, ftHidden, ftSystem, ftArchive]; //881

  FMessageHandle := AllocateHWnd(ProcessMsg);  //877>

  // Creates a new dynamic variable
  new(NotifyRegister);
  with NotifyRegister^ do
  begin
    pidlPath      := nil;
    bWatchSubtree := true;
  end; //877<

  // Dynamically load Shell32.dll  //881+
  hDLL := LoadLibrary(StrPCopy(buf, 'shell32.dll'));  //881+
  if hDLL <> 0 then
  begin
    try
      @theFunction := GetProcAddress(hDLL, MAKEINTRESOURCE(2));
      if @theFunction <> nil then
      begin
        // Register a changenotification handle.
        NotifyHandle := theFunction(
                    fMessageHandle,
                    SHCNF_ACCEPT_INTERRUPTS + SHCNF_ACCEPT_NON_INTERRUPTS,
                    SHCNE_MKDIR             // events to be watched
                    + SHCNE_RENAMEFOLDER
                    + SHCNE_RMDIR
                    + SHCNE_DRIVEADD
                    + SHCNE_DRIVEREMOVED
                    + SHCNE_MEDIAINSERTED
                    + SHCNE_MEDIAREMOVED,
                    WM_DIRCHANGE,           // message for notifications.
                    1,                      // Only DeskTop being watched.
                    NotifyRegister);        // Indicates what's being watched.
        if NotifyHandle = 0 then
          raise Exception.Create('Unable to register SHChangeNotify'); //877<
      end else
        raise Exception.Create('Unable to link to function SHChangeNotifyRegister');
    finally
      FreeLibrary(hDLL);
    end;
  end
  else
    raise Exception.Create('Unable to load Shell32.dll');
  //881+<
  FPopupMenu := TLsDirTree21PopUp.Create(Self);
  FPopupMenu.BuildItems;
  FPopupMenu.AutoPopup := True;
  FPopupMenu.FDirTree := Self;
  PopupMenu := FPopupMenu;
  FPopUpMenuEnabled := True;

  OnExpanding := Expanding;
  ReadOnly := False;
  SortType := stNone;
  HideSelection := False;
  FIsNewFolder := False;
  FisCutCopy := False;  //86
  FDragDropEnabled := True;  //878DragDrop
  DragMode := dmManual;  //878DragDrop
  FAbout := 'Version 2.1.8.6';  //861
end; {= Create =}

destructor TLsDirTree21.destroy;  //881+>
var
  i: integer;
  theFunction: SHChangeNotifyDeregister;  //881+
  hDLL: THandle;                        //881+
begin
  DeAllocateHWnd(FMessageHandle);  //877
  try                                    //881+<
    if assigned(NotifyRegister) then
      Dispose(NotifyRegister);
  finally
    NotifyRegister := nil;
  end;

  if NotifyHandle <> 0 then
  begin
    hDLL := 0;
    try
      hDLL := LoadLibrary('shell32.dll');  //881+
      if hDLL <> 0 then
      begin
          @theFunction := GetProcAddress(hDLL, MAKEINTRESOURCE(4));
          if @theFunction <> nil then
            theFunction(NotifyHandle);
      end;
    finally
      NotifyHandle := 0;
      FreeLibrary(hDLL);
    end;
  end;   //881+>

  for i := Items.Count - 1 downto 0 do
    Items[i].Free;
  Images.Free;
  inherited Destroy;
end; {= Destroy =}

procedure TLsDirTree21.Notification(AComponent: TComponent;  //877<
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFileList) then
    FFileList := nil;
end; {= Notification =}  //877>

procedure TLsDirTree21.LoadRoot;
var
  Sfi: TSHFileInfo;
  Root: TTreenode;
  idRoot: PItemIDList;
begin
  Items.BeginUpdate;
  Items.Clear;
  if SHGetSpecialFolderLocation(Handle, CSIDL_DRIVES, idRoot) = NOERROR then
    if SHGetFileInfo(PChar(idRoot), 0, Sfi, SizeOf(TSHFileInfo), SHGFI_PIDL
      or
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME) <> 0 then
    begin
      Root := items.AddFirst(nil, Sfi.szDisplayName);
      Root.ImageIndex := Sfi.iIcon;
      Root.SelectedIndex := Sfi.iIcon;
    end;
  Items.EndUpdate;
end; {= LoadRoot =}

procedure TLsDirTree21.LoadDrives;
var
  ADrive: integer;
  DriveLetter: char;
  DriveString: string;
  DrvName: string;
  Sfi: TSHFileInfo;
begin
  Items.BeginUpdate;
  Integer(Drives) := GetLogicalDrives;
  for ADrive := 0 to 25 do
  begin
    if ADrive in Drives then
    begin
      DriveLetter := Chr(ADrive + ord('A'));
      DriveString := DriveLetter + ':\';
      SHGetFileInfo(PChar(DriveString), 0, Sfi, SizeOf(Sfi),
        SHGFI_DISPLAYNAME);
      DrvName := Copy(Sfi.szDisplayName, 1, (Pos('(', Sfi.szDisplayName) - 1));
      with Items do
      begin
        AddChild(Items[0], ' (' + DriveLetter + ':)  ' + DrvName);
        ShowButtons := True;
        Items[Count - 1].HasChildren := true;
        Items[Count - 1].ImageIndex := GetNormalIcon(DriveString);
        Items[Count - 1].SelectedIndex := GetSelectedIcon(DriveString);
        if GetDriveType(PChar(DriveString)) = DRIVE_REMOTE then  //871<<
          FNetDrive[ADrive] := True
        else
          FNetDrive[ADrive] := False;  //871>>
      end;
    end;
  end;
  Items.EndUpdate;
end; {= LoadDrives =}

procedure TLsDirTree21.MakePath(Node: TTreeNode);

  procedure MakeSubPath;
  begin
    if Node.Level = 1 then
      TreeViewPath := Copy(Node.Text, 3, 2) + '\' + TreeViewPath
    else if Node.Level > 1 then
      if TreeViewPath = '' then
        TreeViewPath := Node.Text
      else
        TreeViewPath := Node.Text + '\' + TreeViewPath;
  end; {= MakeSubPath =}

begin
  TreeViewPath := '';
  MakeSubPath;
  while Node.Parent <> nil do
  begin
    Node := Node.Parent;
    MakeSubPath;
  end;
end; {= MakePath =}

procedure TLsDirTree21.AddSubs(Path: string; Node: TTreeNode);
var
  ANode: TTreeNode;
  APath: string;
  hFindFile: THandle;
  Win32FD: TWin32FindData;
  DriveLtr: Char;  //871
  DirAttr  : DWORD;     //881
  AttrIndex: TFileAttr; //881

const
  dwFileAttr: array[TFileAttr] of DWord =
    (FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_HIDDEN,
     FILE_ATTRIBUTE_SYSTEM, FILE_ATTRIBUTE_DIRECTORY,
     FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_NORMAL,
     FILE_ATTRIBUTE_COMPRESSED);

  function IsDirectory(dWin32FD: TWin32FindData): Boolean;
  var
    FName: string;
    TmpAttr: DWORD;
  begin
    FName := StrPas(dWin32FD.cFileName);
    //881>
    with dWin32FD do
    begin
      TmpAttr := dwFileAttributes and
         (FILE_ATTRIBUTE_READONLY or FILE_ATTRIBUTE_HIDDEN or
          FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_ARCHIVE or
          FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_DIRECTORY);    //886-

      Result := (TmpAttr and FILE_ATTRIBUTE_DIRECTORY = FILE_ATTRIBUTE_DIRECTORY) and
                (TmpAttr and DirAttr = TmpAttr) and
                (FName <> '.') and
                (FName <> '..');
    end;  //881<
    {*
    with dWin32FD do
      Result := (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY =
        FILE_ATTRIBUTE_DIRECTORY) and (FName <> '.') and (FName <> '..');
    *}
  end; {= IsDirectory =}

  function HasSubs(sPath: string): Boolean;
  var
    sAPath: string;
    shFindFile: THandle;
    sWin32FD: TWin32FindData;
  begin
    Result := False;
    sAPath := sPath;
    sAPath := AddSlash(sAPath);
    shFindFile := FindFirstFile(PChar(sAPath + '*.*'), sWin32FD);
    if shFindFile <> INVALID_HANDLE_VALUE then
    try
      repeat
        if IsDirectory(sWin32FD) then
        begin
          Result := True;
          Break;
        end;
      until not FindNextFile(shFindFile, sWin32FD);
    finally
      Windows.FindClose(shFindFile);
    end;
  end; {= HasSubs =}

begin
  //881>
  DirAttr := 0;
  for AttrIndex := ftReadOnly to ftArchive do  //ftNormal do   //881--
    if AttrIndex in FFolderType then
      DirAttr := DirAttr or dwFileAttr[AttrIndex];
  DirAttr := DirAttr or FILE_ATTRIBUTE_DIRECTORY;
  //881<
  APath := Path;
  APath := AddSlash(APath);
  DriveLtr := UpCase(Path[1]);
  hFindFile := FindFirstFile(PChar(APath + '*.*'), Win32FD);
  if hFindFile <> INVALID_HANDLE_VALUE then
  try
    repeat
      if IsDirectory(Win32FD) then
      begin
        ANode := Items.AddChild(Node, Win32FD.cFileName);
        if FNetDrive[ord(DriveLtr) - 65] = True then  //871
          ANode.HasChildren := True
        else  //871<<
          ANode.HasChildren := HasSubs(APath + Win32FD.cFileName);
        ANode.ImageIndex := GetNormalIcon(APath + Win32FD.cFileName);
        ANode.SelectedIndex := GetSelectedIcon(APath + Win32FD.cFileName);
      end;
    until not FindNextFile(hFindFile, Win32FD);
  finally
    Windows.FindClose(hFindFile);
  end;
end; {= addSubs =}

procedure TLsDirTree21.ReLoad;
begin
  Items.BeginUpdate;
  Items.Clear;
  LoadRoot;
  LoadDrives;
  Items.EndUpdate;
end; {= Reload =}

procedure TLsDirTree21.Loaded;  //70
begin
  inherited Loaded;
  Reload;
  if Items.GetFirstNode <> nil then
    Items.GetFirstNode.Expand(False);
end; {= Loaded =}

procedure TLsDirTree21.Expanding(Sender: TObject; Node: TTreeNode;
  var AllowExpansion: Boolean);
var
  OldCursor: TCursor;  //872
begin
  if Node.GetFirstChild = nil then
  begin
    OldCursor := Screen.Cursor;   //872
    Screen.Cursor := crHourGlass;  //872
    try
      MakePath(Node);
      Node.HasChildren := false;
      AddSubs(TreeViewPath, Node);
      Node.AlphaSort;
    finally
      Screen.Cursor := OldCursor;  //872
    end;
  end;
end; {= Expanding =}

procedure TLsDirTree21.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  vHitTest: THitTests;
  DrvChar: Char;
begin
  inherited MouseDown(button, shift, x, y);  //85

  TreeViewPath := '';
  FPreviousPath := FSelectedPath;  //875
  vHitTest := GetHitTestInfoAt(X, Y);
  if (htOnItem in vHitTest) or (htOnIcon in vHitTest) then
//     or htOnButton in vHitTest) then  //874
  begin
    Selected := GetNodeAt(X, Y);  //85
    if (Selected.Level = 0) and (Items[0].getFirstChild = nil) then
      LoadDrives
    else
      MakePath(Selected);

    if Selected = Items[0] then
      FSelectedPath := 'My Computer'  //'Drives'  //885r2
    else
      FSelectedPath := TreeViewPath;

    if Selected.Level = 1 then
      if GetDriveType(PChar(FSelectedPath)) in
           [DRIVE_REMOVABLE, DRIVE_CDROM] then
      begin
        DrvChar := FSelectedPath[1];
        if not DiskInDrive(DrvChar, 1) then
        begin
          OpenPath(FPreviousPath);  //875
          FSelectedPath := FPreviousPath;  //877
          exit;
        end;
      end;

    if Assigned(FOnDirChange) then        //83
      FOnDirChange(Self, FSelectedPath);
    if Assigned(FFileList) and (FSelectedPath <> '') then
    begin                                            //886<
      FFileList.Directory := FSelectedPath;
      FFileList.Click;       //886
    end;                                             //>886
    FPreviousPath := FSelectedPath;  //875

    //878DragDrop>
    if (FDragDropEnabled = True) and (Button = mbLeft)  then
    begin
      vHitTest := GetHitTestInfoAt(X, Y);
      FNodeDrag := GetNodeAt(X, Y);
      if (htOnItem in vHitTest) and (Assigned(FNodeDrag)) then
      begin
        MakePath(FNodeDrag);
        FSrcPath := GetPathFromNode(FNodeDrag);
        FNodeDrag.Selected := True;
        FNodeDrag.Focused := True;
        Application.ProcessMessages;  //
        if (DragMode = dmManual) and (csLButtonDown in ControlState) then
        begin
          if (ssCtrl in Shift) then
            FDgDpMode := 0   //Copy
          else
            FDgDpMode := 2;  //Move
//          showmessage('FDgDpMode  = '+ IntToStr(FDgDpMode) + #13 +
//                      'NodeDrag = ' + FNodeDrag.Text);  //debugging
          BeginDrag(False, Mouse.DragThreshold + 8);
        end;
      end
      else if assigned(Selected) then
        Selected.Selected := False;
    end;  //DragDropEnabled & mbLeft
    //878DragDrop<
  end;
end; {= MouseDown =}

procedure TLsDirTree21.SetSelectedPath(Value: string);
begin
  if AnsiCompareText(Value, FSelectedPath) = 0 then
    exit;
  FSelectedPath := Value;
end; {= SetSelectedPath =}

procedure TLsDirTree21.SetSelectedFile(Value: string); //80^
begin
  if AnsiCompareText(Value, FSelectedFile) = 0 then exit;
    FSelectedFile := Value;
end;  {= SetSelectedFile =}

procedure TLsDirTree21.SetInitialDir(Value: string);  //85 <<
begin
  if (Value = '') or (AnsiCompareText(Value, FInitialDir) = 0)  then
    exit;
  Value := AddSlash(Value);
  if (not DirectoryExists(Value)) then
    exit
  else begin
    FInitialDir := Value;
    OpenPath(FInitialDir);
    if Assigned(FFileList) then
      FFileList.Directory := FInitialDir;
  end;
end;   {= SetInitialDir =}  //85 >>

//881<
procedure TLsDirTree21.SetFolderType(NewFolderType: TFileType); //MPX4
begin
  if NewFolderType <> FFolderType then
  begin
    FFolderType := NewFolderType;
  end;
end; {= SetFolderType =}  //881>

procedure TLsDirTree21.OpenPath(dPath: string);
var
  CurNode: TTreeNode;
  count: Integer;
  TempPath: string;
  CurPath: string;
  FullPath: string;
begin
  if (dPath = '') or (Length(dPath) = 1) then exit;
  if not DirectoryExists(dPath) then exit;
  dPath := AddSlash(dPath);
  FullPath := dPath;
  Items.BeginUpdate;
  CurNode := Items.GetFirstNode;  //70
  if CurNode.getFirstChild = nil then
    LoadDrives;
  if CurNode.Expanded then
    CurNode.Collapse(True);
  CurNode := Items.GetFirstNode;
  while Pos('\', dPath) > 0 do
  begin
    count := Pos('\', dPath);
    tempPath := Copy(dPath, 1, count);
    dPath := Copy(dPath, count + 1, Length(dPath));
    CurNode := CurNode.getFirstChild;

    while CurNode <> nil do
    begin
      if CurNode.Level = 1 then
        CurPath := Copy(CurNode.Text, 3, 2) + '\'
      else if CurNode.Level > 1 then
        CurPath := CurNode.Text + '\';
      if AnsiCompareText(CurPath, tempPath) = 0 then
      begin
        CurNode.Selected := True;
        CurNode.Expand(False);
        Break;
      end;
      CurNode := CurNode.getNextSibling;    //GetNext;  //878
      if CurNode = nil then exit;
    end;
  end;
  if CurNode <> nil then CurNode.MakeVisible;  //878
  Items.EndUpdate;
  if AnsiCompareText(FSelectedPath, FullPath) <> 0 then
  begin
    FullPath := AddSlash(FullPath);
    FSelectedPath := FullPath;
  end;
  if Assigned(FOnDirChange) then
    FOnDirChange(Self, FSelectedPath);  //83
end; {= OpenPath =}

procedure TLsDirTree21.KeyUp(var Key: Word; Shift: TShiftState);  //84
var
  DrvChar: Char;
begin
  if (Key = VK_UP) or (Key = VK_DOWN) or (Key = VK_LEFT) or
      (Key = VK_RIGHT) or (Key = VK_Back) then    //885r2
  begin
    inherited KeyUp(Key, Shift);

    if selected = nil then exit;
    if (Selected.Level = 0) and (Items[0].getFirstChild = nil) then
      LoadDrives
    else
      MakePath(Selected);

    if (Selected.Level = 0) then
      FSelectedPath := 'My Computer'  //'Drives'  //885r2
    else
      FSelectedPath := TreeViewPath;

    if Selected.Level = 1 then
      if GetDriveType(PChar(FSelectedPath)) in
        [DRIVE_REMOVABLE, DRIVE_CDROM] then
      begin
        DrvChar := FSelectedPath[1];
        if not DiskInDrive(DrvChar, 1) then
          exit;
      end;

    if Assigned(FOnDirChange) then
      FOnDirChange(Self, FSelectedPath);  //83
    if Assigned(FFileList) and (FSelectedPath <> '') then    //886<
    begin
      FFileList.Directory := FSelectedPath;
      FFileList.Click;
    end;                                                    //>886
  end;
  //881>
  if Selected <> nil then
  begin
    if ssCtrl in Shift then
    begin
      if (Key = Ord('N')) then AddNewNode(Selected, '')  //Ord('N') = $4E
      else if (Key = Ord('C')) then CutOrCopyNode(0)     //Ord('C') = $43
      else if (Key = Ord('X')) then CutOrCopyNode(2)     //Ord('X') = $58
      else if (Key = Ord('V')) then PasteNode;           //Ord('V') = $56
    end;
    if (Shift = [ssAlt]) and (Key = VK_RETURN) then
      ShowNodeProperties;
    if Shift = [] then
    begin
      Case Key of
        VK_F2     :  Selected.EditText;
        VK_F5     :  begin   //872>
                       Reload;
                       OpenPath(FSelectedPath);
                       if assigned(FFileList) then
                         FFileList.UpdateFileList;  //876
                     end; //872<
      end;  //case
    end;
  end; //if Selected ...
  //881<
end; {= KeyUp =}  //84

function TLsDirTree21.GetPathFromNode(Node: TTreeNode): string;
begin
  Result := '';
  if Node = nil then exit;
  if Assigned(Node) then
  begin
    MakePath(Node);
    Result := TreeViewPath;
  end;
end; {= GetPathFromNode =}

function TLsDirTree21.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := False;
  if (Assigned(Node.Parent)) and (Node.Level > 1) and
    (not ReadOnly) then
    Result := inherited CanEdit(Node);
end; {= CanEdit =}

procedure TLsDirTree21.Edit(const Item: TTVItem);
var
  OldDirName: string;
  NewDirName: string;
  Aborted: Boolean;
  OldCur: TCursor;
  Rslt: Boolean;
  SelNode: TTreeNode;

  function GetNodeFromItem(Item: TTVItem): TTreeNode;
  begin
    with Item do
      if (State and TVIF_PARAM) <> 0 then
        Result := Pointer(lParam)
      else
        Result := Items.GetNode(hItem);
  end; {= GetNodeFromItem =}

begin
  SelNode := GetNodeFromItem(Item);
  if not Assigned(SelNode) or
    (SelNode = Items[0]) or (SelNode.Level = 1) then
    exit;

  if (Length(Item.pszText) = 0)
    or (StrContains(InvalidDosChars, Item.pszText)) then
  begin
    MessageBeep(MB_ICONHAND);
    if (Length(Item.pszText) > 0) then
//      MessageDlg('Error - Invalid Directory Name' + #13 +    //872ln
      MessageDlg(esInvalidDirName + #13 +    //872ln
        Item.pszText, mtError, [mbOK], 0);
    Exit;
  end;
  if SelNode <> nil then
  begin
    OldDirName := GetPathFromNode(SelNode);
    if OldDirName = '' then
      exit;
    OldCur := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    try
      Items.BeginUpdate;
      Aborted := False;
      OldDirName := DelSlash(OldDirName);
      NewDirName := ExtractFilePath(OldDirName) + Item.pszText;

      if not FIsNewFolder then
      begin
//      if MessageDlg('Do you want to rename the selected folder' + #13 + #13 +
//        '    From  : ' + ExtractFileName(OldDirName) + #13 +
//        '    To     : ' + ExtractFileName(NewDirName), mtConfirmation,
//        [mbYes, mbNo], 0) = mrNo then  //872ln
        if MessageDlg(esConfirmRename + #13 + #13 +
                      '    ' + ewFrom + ExtractFileName(OldDirName) + #13 +
                      '    ' + ewTo + ExtractFileName(NewDirName), mtConfirmation,
                      [mbYes, mbNo], 0) = mrNo then   //872ln
        begin
          Items.EndUpdate;
          exit;
        end;
      end;

      Rslt := DoSHFileOp(Parent.Handle, FO_RENAME, OldDirName,
        NewDirName, Aborted);

      if Rslt then
      begin
        Reload;
        OpenPath(NewDirName);
        if Assigned(FFileList) then
        begin
          FFileList.Directory := NewDirName;
        end;
      end;
    finally
      Items.EndUpdate;
      Screen.Cursor := OldCur;
      FIsNewFolder := False;
    end;
  end;
end; {= Edit =}

function TLsDirTree21.AddNewNode(ParentNode: TTreeNode;
  NodeName: string): Boolean;
var
  Path: string;
  Dir: string;
  NewNode: TTreeNode;
begin
  Result := False;
  if ParentNode = nil then
    ParentNode := Selected;
  if ParentNode.Level = 0 then
  begin
//    MessageDlg('Can''t add drives', mtError, [mbOK], 0);
    MessageDlg(ewError + esCannotAddDrv , mtError, [mbOK], 0);  //872ln
    exit;
  end;

  if NodeName = '' then
  begin
//    NodeName := 'New Folder';
    NodeName := esNewFolder;  //872ln
    FIsNewFolder := True;
  end;
  try
    Path := AddSlash(GetPathFromNode(ParentNode));
    if Path = '' then exit;
    Dir := AddSlash(Path + NodeName);

    if StrContains(InvalidDosChars, NodeName) then
    begin
      MessageBeep(MB_ICONHAND);
//      MessageDlg('Folder Name contains invalid characters', mtError, [mbOK], 0);
      MessageDlg(esInvalidChars, mtError, [mbOK], 0);  //872ln
      exit;
    end;
    Items.BeginUpdate;

    Result := CreateDirectory(PChar(Dir), nil);

    if Result then
    begin
      ReLoad;
      OpenPath(Dir);
      NewNode := Selected;
      if (NewNode <> nil) and (NodeName = 'New Folder') then
        NewNode.EditText;
    end;
  finally
    Items.EndUpdate;
  end;
end; {= AddNewNode =}

function TLsDirTree21.DeleteNode(Node: TTreeNode): Boolean;
var
  DelDir: string;
  DelPath: string;
  PrevNode: TTreeNode;
  oldCur: TCursor;
  Aborted: Boolean;
begin
  Result := False;
  Aborted := True;
  PrevNode := Node.Parent;
  if (Assigned(Node)) and (Node.Level > 1) then
  begin
    oldCur := Screen.Cursor;
    Screen.Cursor := crHourGlass;
    if Selected <> nil then
      DelDir := GetPathFromNode(Selected);
    if DelDir = '' then
    begin
      Screen.Cursor := OldCur;
      exit;
    end;
    if not DirectoryExists(Deldir) then
    begin
      MessageBeep(MB_ICONHAND);
//      MessageDlg(DelDir + 'not found', mtError, [mbOK], 0);
      MessageDlg(DelDir + esNotFound, mtError, [mbOK], 0);
      Screen.Cursor := oldCur;
      exit;
    end;
    DelDir := DelSlash(Deldir);
    DelPath := ExtractFilePath(DelDir);
 //   MessageBeep(MB_ICONHAND);
    Result := DoSHFileOp(Parent.Handle, FO_DELETE, DelDir, '', Aborted);
    if Result then
    begin
      if Assigned(PrevNode) then
        Selected := PrevNode;
      Node.Delete;
      if Assigned(FFileList) then
        FFileList.Directory := GetPathFromNode(Selected);
    end;
    Screen.Cursor := oldCur;
  end;
end; {= DeleteNode =}

procedure TLsDirTree21.SetFileList(Value: TLsFilelistView28);
begin
  if FFileList <> Value then
    FFileList := Value;
  if (Assigned(FFileList)) and (FSelectedPath <> '') then
    FFileList.Directory := FSelectedPath;
end; {= SetFileList =}

procedure TLsDirTree21.SetPopUpMenuEnabled(Value: Boolean);
begin
  if Value <> FPopUpMenuEnabled then
  begin
    FPopUpMenuEnabled := Value;
    if FPopUpMenuEnabled then
      PopupMenu := FPopupMenu
    else
      PopupMenu := nil;
  end;
end; {= SetPopUpMenuEnabled =}

procedure TLsDirTree21.SetDragDropEnabled(Value: Boolean);
begin
  if Value <> FDragDropEnabled then
    FDragDropEnabled := Value;
end;  {= SetDragDropEnabled =}

procedure TLsDirTree21.SetTvLanguage(Value: TLanguage);  //872ln
var
  LangID,
    wPrimaryLangID,
    wSubLangID: integer;

  function PRIMARYLANGID(lgid: WORD): WORD;
  begin
    Result:=lgid and $03FF;
  end;

  function SUBLANGID(lgid: WORD): WORD;
  begin
    Result:=lgid shr 10;
  end;

begin
  if value = lnSysDefault then
  begin
//    LangID := GetUserDefaultLangID and $3FF;  //SysLocale.PriLangID;
    LangID := GetUserDefaultLangID;
    wPrimaryLangID := PRIMARYLANGID(LangID); //SysLocale.PriLangID;
    wSubLangID := SUBLANGID(LangID);

    Case wPrimaryLangID of   // LangID of   //879
      LANG_ENGLISH      : Value := lnEnglish;    //$09
      LANG_DUTCH        : Value := lnDutch;      //$13  //875
      LANG_FRENCH       : Value := lnFrench;     //$0c
      LANG_GERMAN       : Value := lnGerman;     //$07
      LANG_ITALIAN      : Value := lnItalian;    //$10  //874
      LANG_JAPANESE     : Value := lnJapanese;   //$11  //880
      LANG_KOREAN       : Value := lnKorean;     //$12  //885
      LANG_POLISH       : Value := lnPolish;     //$15  //876
      LANG_PORTUGUESE   : Value := lnPortuguese; //$16
      LANG_SLOVAK       : Value := lnSlovak;     //$1b  //876
      LANG_SLOVENIAN    : Value := lnSlovenian;  //$24  //881
      LANG_SPANISH      : Value := lnSpanish;    //$0a
      LANG_SWEDISH      : Value := lnSwedish;    //$1d  //882
      LANG_TURKISH      : Value := lnTurkish;    //$if  //886
      LANG_CHINESE      :                        //$04  //879
        begin  //879<
          case wSubLangID of
            SUBLANG_CHINESE_SIMPLIFIED: Value := lnChinese_Sim;  //$0804
          else
            Value := lnChinese_Tra;  //$0404
          end;
        end;  //879>
    else
      Value := lnEnglish;
    end;
  end;
  if value <> FTvLanguage then
  begin
    FTvLanguage := Value;
    TvLangID := FTvLanguage;
    InitializeVar;
    FPopupMenu.Free;
    Application.ProcessMessages;
    FPopupMenu := TLsDirTree21PopUp.Create(Self);
    FPopupMenu.BuildItems;
    FPopupMenu.AutoPopup := True;
    FPopupMenu.FDirTree := Self;
    PopupMenu := FPopupMenu;
    FPopUpMenuEnabled := True;
    ReCreateWnd;
  end;
end;   {= SetTvLanguage =} //872ln

//872ts<
function TLsDirTree21.GetTreeSize: double;
var
  CurNode: TTreeNode;
  CurPath, CurDir, DispSize, DispName: string;
  FTreeSize, TreeSize: double;
  OldCur: TCurSor;
  ActualSize: string; //872ts

  function ComputeFolderSize(ADir: String): Double;
  var
    hFnd: THandle;
    Fnd: TWin32FindData;
    Temp: integer;
    FDir,FName: string;
  begin
    Result := 0;
    if Length(ADir) <> 0 then
    begin
      if ADir[Length(ADir)] <> '\' then
        ADir := ADir + '\';
      FDir := ADir;
    end
    else
      Exit;
    hFnd := FindFirstFile(PChar(FDir + '*.*'),Fnd);
    if hFnd <> INVALID_HANDLE_VALUE then
    begin
      try
        repeat
          FName := Trim(StrPas(Fnd.cFileName));
          if (FName <> '.') and (FName <> '..') then
          begin
            if (Fnd.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <>
              FILE_ATTRIBUTE_DIRECTORY then
            begin
              Temp := (Fnd.nFileSizeHigh * MAXDWORD)  + Fnd.nFileSizeLow;
              Result := Result + Temp;
            end
            else
              Result := Result + ComputeFolderSize(FDir + FName);
         end;
       until not FindNextFile(hFnd,Fnd);
     finally
       Windows.FindClose(hFnd);
     end;
    end;
  end;

begin
  CurPath := '';
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Items.BeginUpdate;
  try
    CurNode := Selected;
    CurPath := GetPathFromNode(CurNode);
    CurDir := ExtractFileName(CurPath);
    FTreeSize := ComputeFolderSize(CurPath);
    if (CurNode.Level = 1) then
//    DispName := Drv + CurPath
    DispName := ewDrive + CurPath
    else if (CurNode.Level > 1) then
//    DispName := Fldr + CurDir + '"';
    DispName := ewFolder + CurDir + '"';
  finally
    Items.EndUpdate;
    Screen.CurSor := OldCur;
  end;
  Result := FTreeSize;
  ActualSize := Format('%0.0n Bytes',[FTreeSize]);
  if (FTreeSize > 0) and (FTreeSize < 1024) then
    TreeSize := 1.0
  else
    TreeSize := FTreeSize / 1024;
  if TreeSize <= 99999 then
    DispSize := Format('%.2n KB',[TreeSize])
  else
    DispSize := Format('%.2n MB',[TreeSize/1024]);

  MessageDlg( '===== ' + esTreeSize + ' =====' + #13#13 +   //873
             DispName + #13 + esAllSubDir + #13 +           //873
             ' =  ' + ActualSize + #13 +
             '     (i.e. ' + DispSize + ' )', mtInformation,[mbOK],0);  //872ln<
end;  {= GetTreeSize =}
//872ts>

procedure TLsDirTree21.ShowFolderContents; //80^
var
  CurFolder: string;
begin
  SelectedDir := '';
  SelectedDir := Self.FSelectedPath;
  if Selected.Level = 1 then
    CurFolder := '" ' + SelectedDir[1] + ':\ "'
  else if Selected.Level > 1 then
    CurFolder := '" ..\' + ExtractFileName(DelSlash(SelectedDir)) + ' "';
  if Items.GetFirstNode <> nil then
    Items.GetFirstNode.Collapse(True);
  OpenFileListDlg(Self);
  with DlgForm do
  begin
    Caption := 'Files in ' + CurFolder;
    Show;
  end;
end;  {= ShowFolderContents =}

procedure TLsDirTree21.ConnectNetResource(Sender: TObject);  //82
var
  sDrvLetter,  //880
    sNetPath: string;  //880
  CurNode: TTreeNode;
  bReConnect: Boolean;
  Success: Boolean;
begin
  Success := False;       //880>
  sDrvLetter := '';
  sNetPath := '';
//  if IsWindowsXP then
//  begin
//    if WNetConnectionDialog(Application.Handle, RESOURCETYPE_DISK) = NO_ERROR then
//      Success := True;
//  end
//  else begin
    FNetPathDlg := TNetPathDlg.create(Self);
    if FNetPathDlg.Execute then  //880>
    begin
      sDrvLetter := FNetPathDlg.DrvLetter;
      sNetPath := FNetPathDlg.NetPath;
      bReconnect := FNetPathDlg.Reconnect;
    end else
      exit;
    if ConnectDrive(sDrvLetter, sNetPath, True, bReconnect) = NO_ERROR then
      Success := True;
//  end;  //880
  if Success then
  begin
    Reload;
    Items[0].Expand(False);
    CurNode := Items[0].GetLastChild;
    CurNode.Selected := True;
    FSelectedPath := GetPathFromNode(CurNode);  //sDrvLetter + '\';
    if Assigned(FFileList) then
      FFilelist.Directory := FSelectedPath;
  end else
    exit;
end;  {= ConnecttoNetResource =}  //82


procedure TLsDirTree21.DisConnectNetResource(Sender: TObject);  //82
var
  CurNode: TTreeNode;
begin
  if (WNetDisconnectDialog(Application.Handle, RESOURCETYPE_DISK) = NO_ERROR) then
  begin
    Reload;
    CurNode := Items.GetFirstNode;
    if not CurNode.Expanded then
      CurNode.Expand(False);
    CurNode.Selected := True;
    SelectedPath := 'My Computer';  //'Drives';  //885r2
    if Assigned(FFileList) then
    begin
      FFileList.Directory := SelectedPath;
      FFileList.UpdateFileList;
    end;
  end;
end;   {= DisConnectNetResource =}   //82

//82 >>
procedure TLsDirTree21.OpenFileListDlg(Sender: TObject);  //82 <<
var
  FFont: TFont;
  Panel1,
    Panel2: TPanel;

  procedure CreateImages;  //80^
  var
    sfi: TSHFileInfo;
    hImgList: Uint;
  begin
    SImgList := TImageList.Create(Self);
    hImgList := SHGetFileInfo('', 0, sfi, SizeOf(sfi), SHGFI_SYSICONINDEX or
                SHGFI_SMALLICON);
    if hImgList <> 0 then
    begin
      SImgList.Handle := hImgList;
      SImgList.ShareImages := True;
    end;
    FileView.SmallImages := SImgList;
  end;  {= CreateImages =}

  procedure BuildFileList; //80 //82
  var
    CurDir,
    FName,
      FileName: string;
    sfi: TSHFileInfo;
    hFindFile: THandle;
    Win32FD: TWin32FindData;
    OldCur: TCursor;
  begin
    OldCur := Screen.Cursor;
    CurDir := AddSlash(SelectedDir);
    hFindFile := FindFirstFile(PChar(CurDir + '*.*'), Win32FD);
    if hFindFile <> INVALID_HANDLE_VALUE then
    try
      Screen.Cursor := crHourGlass;
      FileView.Items.BeginUpdate;
      repeat
        with Win32FD do
        begin
          if (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY > 0) then
            Continue;
          FName := StrPas(Win32FD.cFileName);
          FileName := CurDir + FName;
          SHGetFileInfo(PChar(FileName), 0, sfi, SizeOf(sfi),
            SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME);

          with FileView.Items.Add do
          begin
            Caption := FName;
            ImageIndex := sfi.iIcon;
            SubItems.Add(FileName);
          end;
        end;
      until not FindNextFile(hFindFile, Win32FD);
    finally
      Windows.FindClose(hFindFile);
      FileView.Items.EndUpdate;
      Screen.Cursor := OldCur;
    end;
  end;  {= BuildList =}

begin
  DlgForm := TForm.Create(Self);
  with DlgForm do
  begin
    Parent := self;
    Align := alClient;
    BorderStyle := bsSizeToolWin; //bsSizeable;
    BorderIcons := BorderIcons - [biMaximize] - [biMinimize] -
      [biSystemMenu];
    FFont := TFont.Create;
    if owner is TForm then
      FFont.Assign(TForm(Owner).Font);
    Enabled := True;
    TabStop := False;
    FormStyle := fsStayOnTop;
    OnResize := DlgFormResize;  //82

    Panel1 := TPanel.Create(Self);
    with Panel1 do
    begin
      Parent := DlgForm;
      Align := alBottom;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Caption := '';
      Height := 22;
      TabStop := False;  //84
    end;

    Panel2 := TPanel.Create(Self);
    with Panel2 do
    begin
      Parent := Panel1;
      Height := 22;
      Width := 80;
      Align := alRight;
      BevelInner := bvNone;
      BevelOuter := bvNone;
      Caption := '';
      TabStop := False;  //84
    end;

    BtnOK := TBitBtn.Create(Self);  //84
    with BtnOk do
    begin
      Parent := Panel2;
      Height := 22;
      Width := 60;
      Top := 1;
      Left := 1;
      Caption := '&OK';
      Kind := bkOK;  //84
      Layout := blGlyphLeft;
      TabStop := True;
//      Glyph.Handle := LoadBitmap(hInstance, 'OK28');  //84
      OnClick := OKBtnClick;
      ModalResult := mrOK;
    end;

    FileView := TListView.Create(Self);
    with FileView do
    begin
      ParentWindow := DlgForm.Handle;
      parent := DlgForm;
      Align := alClient;
      Color := Self.Color;            //880+
      Font.Color := Self.Font.Color;  //880+
      Enabled := True;
      ViewStyle := vsList;
      HideSelection := False;  //84
      Hint := 'LeftClick - Select' +#13 +
              'DblClick  - Open';
      ShowHint := True;
      SortType := stText;
      TabStop := True;  //84
      Visible := True;
      OnDblClick := FileViewDblClick;
      CreateImages;
      BuildFileList;
      AlphaSort;
      if (Visible = True) and (Items.Count <> 0) then
        Items[0].Focused := True;  //84
    end;
  end; //DlgForm
end;  {= OpenFileListDlg =}  //82 >>

procedure TLsDirTree21.DlgFormResize(Sender: TObject);   //82 <<
begin
  with DlgForm do
  begin
    FileView.Width := DlgForm.ClientWidth;
    FileView.Height := DlgForm.ClientHeight;
  end;
end;  {= DlgFormResize =} //82 >>


procedure TLsDirTree21.FileViewDblClick(Sender: TObject);  //82 <<
var
  sFile,
  sDir: string;
begin
  with DlgForm do
  begin
    if FileView.Selected <> nil then
    begin
      hide;
      sFile := ExtractFileName(FileView.Selected.SubItems[0]);
      sDir := ExtractFilePath(FileView.Selected.SubItems[0]);
      ExecuteFile('Open', sFile, '', sDir, SW_SHOW);
    end;
    visible := True;
  end;
end;  {= FileViewDblClick =}  //82 >>


procedure TLsDirTree21.OKBtnClick(Sender: TObject);  //82 <<
begin
  FSelectedFile := '';
  with DlgForm do
  try
    if FileView.Selected <> nil then   //82
    begin
      FSelectedFile := FileView.Selected.SubItems[0];
//      ShowMessage(SelectedFile);  //for debugging
      if Assigned(FOnFileChange) then      //83
        FOnFileChange(Self, FSelectedFile);
    end;
  finally
    SImgList.Free;
    FileView.Free;
    DlgForm.Close;
  end;
  OpenPath(SelectedDir);
end;  {= OKBtnClick =}  //82 >>

procedure TLsDirTree21.SetAbout(Value: String);  //85 <<
Begin
  // About property is ReadOnly.
end;  {= SetAbout =} //85 >>

procedure TLsDirTree21.CutOrCopyNode(Mode: integer);  //86 <<
begin
  FOpMode := -1;
  if (Selected = nil) or (SelectedPath = '') then
    exit;
  FSrcPath := SelectedPath;
  FOpMode := Mode;
  FisCutCopy := True;
end;  {= CutOrCopyNode =} //86 >>

procedure TLsDirTree21.PasteNode;  //86 <<
var
  Abort: Boolean;
begin
  if (Selected = nil) or (SelectedPath = '') or
    (FSrcPath = '') then
  begin
    FisCutCopy := False;
    exit;
  end;
  Abort := False;
  FDestPath := AddSlash(SelectedPath);
  if DoSHFileOp(Parent.Handle, FileOpMode[FOpMode], FSrcPath, FDestPath, Abort) then
  begin
    Reload;
    OpenPath(FDestPath)
  end else
//    MessageDlg('File operation failed', mtError, [mbOK], 0);
    MessageDlg(esFileOpFailed, mtError, [mbOK], 0);  //872ln
  FisCutCopy := False;
end;  {= PasteNode =}  //86 >>


//=============== Drag and Drop routines ================//

procedure TLsDirTree21.DragOver(Source: TObject; X, Y: integer;
  State: TDragState; var Accept: Boolean);  //878DragDrop<
begin
  inherited DragOver(Source, X, Y, State, Accept);

  if not FDragDropEnabled then   //880
    exit;                        //880

  Accept := False;
  if (Source is TLsDirTree21) or
     ((Source is TLsFileListview28) and Assigned(FFileList)) then
  begin
    FNodeDrop := GetNodeAt(X, Y);
    if (FNodeDrop <> FNodeDrag) and Assigned(FNodeDrop) then
      Accept := True
    else Accept := False;
  end;
end;  {= DragOver =}   //878DragDrop<

procedure TLsDirTree21.DragDrop(Source: TObject; X, Y: integer);  //878DragDrop>
var
  Abort: Boolean;
  HT: THitTests;
  i: integer;
  FSelectedItems: TStrings;
  SrcItem,
    DesItem,
    DFName: String;
begin
  if not FDragDropEnabled then   //880
    exit;                        //880
  if Selected = nil then
    exit;
  FDestPath := '';
  HT := GetHitTestInfoAt(X, Y);
  FNodeDrop := GetNodeAt(X, Y);
  if (HT - [htOnIcon, htOnItem, htOnLabel, htOnRight] <> HT) then
  begin
    if (FNodeDrop = nil) or (ReadOnly = True) then exit;
    FDestPath := AddSlash(GetPathFromNode(FNodeDrop));
//    ShowMessage('Src: ' + FSrcPath + '; Des: ' + FDestPath );  //debugging
    if FDestPath <> '' then
    try
      if (Source is TLsDirTree21) then
      begin
        Application.ProcessMessages;
        Abort := False;
        Items.BeginUpdate;
        if not (DoSHFileOp(Parent.Handle, FileOpMode[FDgDpMode], FSrcPath,
                FDestPath, Abort)) then
        begin
          Items.EndUpdate;
          MessageDlg(esFileOpFailed, mtError, [mbOK], 0);
          exit;
        end;
        Items.EndUpdate;
      end  //Source is TLsDirTree21

      else if (Source is TLsFileListview28) and (Assigned(FFileList)) then
      begin
        FSelectedItems := TStringList.Create;
        FSelectedItems.Clear;
        with FFileList do
        begin
          if Selected = nil then
            exit;
          for i := 0 to Items.Count - 1 do
          begin
            if UpperCase(Items[i].SubItems[5]) = 'DIR' then
              Items[i].Selected := False;
            if Items[i].Selected then
              FSelectedItems.Add(AddNullToStr(Items[i].SubItems[4]));
          end;
          if FSelectedItems.Count = 0 then
          begin
            MessageDlg(esNoFileSelected, mtWarning, [mbOK], 0);
            exit;
          end;
        end;  //With FFileList

        if FSelectedItems.Count <> 0 then
        try
          Items.BeginUpdate;
          for i := 0 to FSelectedItems.Count - 1 do
          begin
            Abort := False;
            SrcItem := '';
            DesItem := '';
            SrcItem := FSelectedItems.Strings[i];
            DFName := ExtractFileName(SrcItem);
            DesItem := AddNullToStr(SlashSep(FDestPath, DFName));
            if (SrcItem <> '') and (DesItem <> '') then
            begin
              Application.ProcessMessages;
              DoSHFileOp(Parent.Handle, FileOpMode[FDgDpMode],
                              SrcItem, DesItem, Abort);
            end;
          end;
        finally
          Items.EndUpdate;
        end;  //try
        FSelectedItems.Free;
      end;  //else if Source = FFileList ...
    finally
      Application.ProcessMessages;
      Reload;
      OpenPath(FDestPath);
      if Assigned(FFileList) then
      begin
        FFileList.Directory := FDestPath;
        FFileList.UpdateFileList;
      end;
    end;  //try
  end;
  Inherited DragDrop(Source, X, Y);
end;  {= DragDrop =} //878DragDrop>

//================== End of DragDrop ====================//



procedure TLsDirTree21.ShowNodeProperties;    //862 <<
var
  FolderPath: string;
begin
  if Selected = nil then
    exit;
  FolderPath := '';
  folderPath := Self.SelectedPath;
  if FolderPath <> '' then
    ShowProperties(FolderPath);
end;  {= ShowNodeProperties =}  //862 >>

procedure TLsDirTree21.ProcessMsg(var Message: TMessage);   //877<
var
  CurPath: string;
  EventID: integer;
begin
  CurPath := FSelectedPath;
  if Message.Msg = WM_DIRCHANGE then  //881--
  begin
    EventID := Message.LParam;
    if (EventID = SHCNE_MKDIR) or
       (EventID = SHCNE_RMDIR) or
       (EventID = SHCNE_RENAMEFOLDER) or
       (EventID = SHCNE_DRIVEADD) or
       (EventID = SHCNE_DRIVEREMOVED) or  //then
       (EventID = SHCNE_MEDIAINSERTED) or     //881+
       (EventID = SHCNE_MEDIAREMOVED) then
    begin
      LockWindowUpdate(Self.Handle);
      Reload;
      if not Items[0].Expanded then
        Items[0].Expand(False);
      if CurPath <> '' then
        OpenPath(CurPath);
      if assigned(FFileList) then
      begin
        FFileList.Directory := CurPath;
        FFileList.UpdateFileList;
      end;
      LockWindowUpdate(0);
    end;
  end  //Message.Msg
  else  //879
    Message.Result := DefWindowProc(FMessageHandle, Message.Msg,
                                    Message.wParam, Message.lParam);  //879
end; {= ProcessMsg =} //877>

procedure TLsDirTree21.InitializeVar;  //872ln
begin
  Case TvLangID of
    lnEnglish:
      begin  //872<
        ewDrive            :=  ENGLISH_ewDrive;   //873
        ewFolder           :=  ENGLISH_ewFolder;  //873
        ewError            :=  ENGLISH_ewError;
        ewFile             :=  ENGLISH_ewFile;
        ewFrom             :=  ENGLISH_ewFrom;
        ewTo               :=  ENGLISH_ewTo;
        esCannot           :=  ENGLISH_esCannot;
        esSpecifyDir       :=  ENGLISH_esSpecifyDir;
        esInvalidDrvID     :=  ENGLISH_esInvalidDrvID;
        esDrvNotReady      :=  ENGLISH_esDrvNotReady;
        esExists           :=  ENGLISH_esExists;
        esInvalidDirName   :=  ENGLISH_esInvalidDirName;
        esConfirmRename    :=  ENGLISH_esConfirmRename;
        esCannotAddDrv     :=  ENGLISH_esCannotAddDrv;
        esNewFolder        :=  ENGLISH_esNewFolder;
        esInvalidChars     :=  ENGLISH_esInvalidChars;
        esNotFound         :=  ENGLISH_esNotFound;
        esFilesIn          :=  ENGLISH_esFilesIn;
        esFileOpFailed     :=  ENGLISH_esFileOpFailed;
        esTreeSize         :=  ENGLISH_esTreeSize;   //873
        esAllSubDir        :=  ENGLISH_esAllSubDir;  //873
      end;  //872>
    lnChinese_Tra:
      begin  //879<
        ewDrive            :=  CHINESE_Tra_ewDrive;
        ewFolder           :=  CHINESE_Tra_ewFolder;
        ewError            :=  CHINESE_Tra_ewError;
        ewFile             :=  CHINESE_Tra_ewFile;
        ewFrom             :=  CHINESE_Tra_ewFrom;
        ewTo               :=  CHINESE_Tra_ewTo;
        esCannot           :=  CHINESE_Tra_esCannot;
        esSpecifyDir       :=  CHINESE_Tra_esSpecifyDir;
        esInvalidDrvID     :=  CHINESE_Tra_esInvalidDrvID;
        esDrvNotReady      :=  CHINESE_Tra_esDrvNotReady;
        esExists           :=  CHINESE_Tra_esExists;
        esInvalidDirName   :=  CHINESE_Tra_esInvalidDirName;
        esConfirmRename    :=  CHINESE_Tra_esConfirmRename;
        esCannotAddDrv     :=  CHINESE_Tra_esCannotAddDrv;
        esNewFolder        :=  CHINESE_Tra_esNewFolder;
        esInvalidChars     :=  CHINESE_Tra_esInvalidChars;
        esNotFound         :=  CHINESE_Tra_esNotFound;
        esFilesIn          :=  CHINESE_Tra_esFilesIn;
        esFileOpFailed     :=  CHINESE_Tra_esFileOpFailed;
        esTreeSize         :=  CHINESE_Tra_esTreeSize;
        esAllSubDir        :=  CHINESE_Tra_esAllSubDir;
      end;  //879>
    lnChinese_Sim:
      begin  //879<
        ewDrive            :=  CHINESE_Sim_ewDrive;
        ewFolder           :=  CHINESE_Sim_ewFolder;
        ewError            :=  CHINESE_Sim_ewError;
        ewFile             :=  CHINESE_Sim_ewFile;
        ewFrom             :=  CHINESE_Sim_ewFrom;
        ewTo               :=  CHINESE_Sim_ewTo;
        esCannot           :=  CHINESE_Sim_esCannot;
        esSpecifyDir       :=  CHINESE_Sim_esSpecifyDir;
        esInvalidDrvID     :=  CHINESE_Sim_esInvalidDrvID;
        esDrvNotReady      :=  CHINESE_Sim_esDrvNotReady;
        esExists           :=  CHINESE_Sim_esExists;
        esInvalidDirName   :=  CHINESE_Sim_esInvalidDirName;
        esConfirmRename    :=  CHINESE_Sim_esConfirmRename;
        esCannotAddDrv     :=  CHINESE_Sim_esCannotAddDrv;
        esNewFolder        :=  CHINESE_Sim_esNewFolder;
        esInvalidChars     :=  CHINESE_Sim_esInvalidChars;
        esNotFound         :=  CHINESE_Sim_esNotFound;
        esFilesIn          :=  CHINESE_Sim_esFilesIn;
        esFileOpFailed     :=  CHINESE_Sim_esFileOpFailed;
        esTreeSize         :=  CHINESE_Sim_esTreeSize;   //873
        esAllSubDir        :=  CHINESE_Sim_esAllSubDir;  //873
      end;  //879>
    lnDutch:
      begin  //875<
        ewDrive            :=  DUTCH_ewDrive;
        ewFolder           :=  DUTCH_ewFolder;
        ewError            :=  DUTCH_ewError;
        ewFile             :=  DUTCH_ewFile;
        ewFrom             :=  DUTCH_ewFrom;
        ewTo               :=  DUTCH_ewTo;
        esCannot           :=  DUTCH_esCannot;
        esSpecifyDir       :=  DUTCH_esSpecifyDir;
        esInvalidDrvID     :=  DUTCH_esInvalidDrvID;
        esDrvNotReady      :=  DUTCH_esDrvNotReady;
        esExists           :=  DUTCH_esExists;
        esInvalidDirName   :=  DUTCH_esInvalidDirName;
        esConfirmRename    :=  DUTCH_esConfirmRename;
        esCannotAddDrv     :=  DUTCH_esCannotAddDrv;
        esNewFolder        :=  DUTCH_esNewFolder;
        esInvalidChars     :=  DUTCH_esInvalidChars;
        esNotFound         :=  DUTCH_esNotFound;
        esFilesIn          :=  DUTCH_esFilesIn;
        esFileOpFailed     :=  DUTCH_esFileOpFailed;
        esTreeSize         :=  DUTCH_esTreeSize;
        esAllSubDir        :=  DUTCH_esAllSubDir;
      end; //875>
    lnFrench:
      begin  //872<
        ewDrive            :=  FRENCH_ewDrive;       //873
        ewFolder           :=  FRENCH_ewFolder;      //873
        ewError            :=  FRENCH_ewError;
        ewFile             :=  FRENCH_ewFile;
        ewFrom             :=  FRENCH_ewFrom;
        ewTo               :=  FRENCH_ewTo;
        esCannot           :=  FRENCH_esCannot;
        esSpecifyDir       :=  FRENCH_esSpecifyDir;
        esInvalidDrvID     :=  FRENCH_esInvalidDrvID;
        esDrvNotReady      :=  FRENCH_esDrvNotReady;
        esExists           :=  FRENCH_esExists;
        esInvalidDirName   :=  FRENCH_esInvalidDirName;
        esConfirmRename    :=  FRENCH_esConfirmRename;
        esCannotAddDrv     :=  FRENCH_esCannotAddDrv;
        esNewFolder        :=  FRENCH_esNewFolder;
        esInvalidChars     :=  FRENCH_esInvalidChars;
        esNotFound         :=  FRENCH_esNotFound;
        esFilesIn          :=  FRENCH_esFilesIn;
        esFileOpFailed     :=  FRENCH_esFileOpFailed;
        esTreeSize         :=  FRENCH_esTreeSize;   //873
        esAllSubDir        :=  FRENCH_esAllSubDir;   //873
      end;  //872>
    lnGerman:
      begin  //872<
        ewDrive            :=  GERMAN_ewDrive;   //873
        ewFolder           :=  GERMAN_ewFolder;  //873
        ewError            :=  GERMAN_ewError;
        ewFile             :=  GERMAN_ewFile;
        ewFrom             :=  GERMAN_ewFrom;
        ewTo               :=  GERMAN_ewTo;
        esCannot           :=  GERMAN_esCannot;
        esSpecifyDir       :=  GERMAN_esSpecifyDir;
        esInvalidDrvID     :=  GERMAN_esInvalidDrvID;
        esDrvNotReady      :=  GERMAN_esDrvNotReady;
        esExists           :=  GERMAN_esExists;
        esInvalidDirName   :=  GERMAN_esInvalidDirName;
        esConfirmRename    :=  GERMAN_esConfirmRename;
        esCannotAddDrv     :=  GERMAN_esCannotAddDrv;
        esNewFolder        :=  GERMAN_esNewFolder;
        esInvalidChars     :=  GERMAN_esInvalidChars;
        esNotFound         :=  GERMAN_esNotFound;
        esFilesIn          :=  GERMAN_esFilesIn;
        esFileOpFailed     :=  GERMAN_esFileOpFailed;
        esTreeSize         :=  GERMAN_esTreeSize;   //873
        esAllSubDir        :=  GERMAN_esAllSubDir;   //873
      end;  //872>
    lnItalian:
      begin  //874<
        ewDrive            :=  ITALIAN_ewDrive;
        ewFolder           :=  ITALIAN_ewFolder;
        ewError            :=  ITALIAN_ewError;
        ewFile             :=  ITALIAN_ewFile;
        ewFrom             :=  ITALIAN_ewFrom;
        ewTo               :=  ITALIAN_ewTo;
        esCannot           :=  ITALIAN_esCannot;
        esSpecifyDir       :=  ITALIAN_esSpecifyDir;
        esInvalidDrvID     :=  ITALIAN_esInvalidDrvID;
        esDrvNotReady      :=  ITALIAN_esDrvNotReady;
        esExists           :=  ITALIAN_esExists;
        esInvalidDirName   :=  ITALIAN_esInvalidDirName;
        esConfirmRename    :=  ITALIAN_esConfirmRename;
        esCannotAddDrv     :=  ITALIAN_esCannotAddDrv;
        esNewFolder        :=  ITALIAN_esNewFolder;
        esInvalidChars     :=  ITALIAN_esInvalidChars;
        esNotFound         :=  ITALIAN_esNotFound;
        esFilesIn          :=  ITALIAN_esFilesIn;
        esFileOpFailed     :=  ITALIAN_esFileOpFailed;
        esTreeSize         :=  ITALIAN_esTreeSize;
        esAllSubDir        :=  ITALIAN_esAllSubDir;
      end; //874>
    lnJapanese:
      begin  //880<
        ewDrive            :=  JAPANESE_ewDrive;
        ewFolder           :=  JAPANESE_ewFolder;
        ewError            :=  JAPANESE_ewError;
        ewFile             :=  JAPANESE_ewFile;
        ewFrom             :=  JAPANESE_ewFrom;
        ewTo               :=  JAPANESE_ewTo;
        esCannot           :=  JAPANESE_esCannot;
        esSpecifyDir       :=  JAPANESE_esSpecifyDir;
        esInvalidDrvID     :=  JAPANESE_esInvalidDrvID;
        esDrvNotReady      :=  JAPANESE_esDrvNotReady;
        esExists           :=  JAPANESE_esExists;
        esInvalidDirName   :=  JAPANESE_esInvalidDirName;
        esConfirmRename    :=  JAPANESE_esConfirmRename;
        esCannotAddDrv     :=  JAPANESE_esCannotAddDrv;
        esNewFolder        :=  JAPANESE_esNewFolder;
        esInvalidChars     :=  JAPANESE_esInvalidChars;
        esNotFound         :=  JAPANESE_esNotFound;
        esFilesIn          :=  JAPANESE_esFilesIn;
        esFileOpFailed     :=  JAPANESE_esFileOpFailed;
        esTreeSize         :=  JAPANESE_esTreeSize;
        esAllSubDir        :=  JAPANESE_esAllSubDir;
      end;  //880>
    lnKorean :
      begin  //885<
        ewDrive            :=  KOREAN_ewDrive;   //873
        ewFolder           :=  KOREAN_ewFolder;  //873
        ewError            :=  KOREAN_ewError;
        ewFile             :=  KOREAN_ewFile;
        ewFrom             :=  KOREAN_ewFrom;
        ewTo               :=  KOREAN_ewTo;
        esCannot           :=  KOREAN_esCannot;
        esSpecifyDir       :=  KOREAN_esSpecifyDir;
        esInvalidDrvID     :=  KOREAN_esInvalidDrvID;
        esDrvNotReady      :=  KOREAN_esDrvNotReady;
        esExists           :=  KOREAN_esExists;
        esInvalidDirName   :=  KOREAN_esInvalidDirName;
        esConfirmRename    :=  KOREAN_esConfirmRename;
        esCannotAddDrv     :=  KOREAN_esCannotAddDrv;
        esNewFolder        :=  KOREAN_esNewFolder;
        esInvalidChars     :=  KOREAN_esInvalidChars;
        esNotFound         :=  KOREAN_esNotFound;
        esFilesIn          :=  KOREAN_esFilesIn;
        esFileOpFailed     :=  KOREAN_esFileOpFailed;
        esTreeSize         :=  KOREAN_esTreeSize;   //873
        esAllSubDir        :=  KOREAN_esAllSubDir;  //873
      end;  //885>
    lnPolish:
      begin  //876<
        ewDrive            :=  POLISH_ewDrive;
        ewFolder           :=  POLISH_ewFolder;
        ewError            :=  POLISH_ewError;
        ewFile             :=  POLISH_ewFile;
        ewFrom             :=  POLISH_ewFrom;
        ewTo               :=  POLISH_ewTo;
        esCannot           :=  POLISH_esCannot;
        esSpecifyDir       :=  POLISH_esSpecifyDir;
        esInvalidDrvID     :=  POLISH_esInvalidDrvID;
        esDrvNotReady      :=  POLISH_esDrvNotReady;
        esExists           :=  POLISH_esExists;
        esInvalidDirName   :=  POLISH_esInvalidDirName;
        esConfirmRename    :=  POLISH_esConfirmRename;
        esCannotAddDrv     :=  POLISH_esCannotAddDrv;
        esNewFolder        :=  POLISH_esNewFolder;
        esInvalidChars     :=  POLISH_esInvalidChars;
        esNotFound         :=  POLISH_esNotFound;
        esFilesIn          :=  POLISH_esFilesIn;
        esFileOpFailed     :=  POLISH_esFileOpFailed;
        esTreeSize         :=  POLISH_esTreeSize;
        esAllSubDir        :=  POLISH_esAllSubDir;
      end; //876>
    lnPortuguese:
      begin
        ewDrive            :=  BRAZ_PORT_ewDrive;
        ewFolder           :=  BRAZ_PORT_ewFolder;
        ewError            :=  BRAZ_PORT_ewError;
        ewFile             :=  BRAZ_PORT_ewFile;
        ewFrom             :=  BRAZ_PORT_ewFrom;
        ewTo               :=  BRAZ_PORT_ewTo;
        esCannot           :=  BRAZ_PORT_esCannot;
        esSpecifyDir       :=  BRAZ_PORT_esSpecifyDir;
        esInvalidDrvID     :=  BRAZ_PORT_esInvalidDrvID;
        esDrvNotReady      :=  BRAZ_PORT_esDrvNotReady;
        esExists           :=  BRAZ_PORT_esExists;
        esInvalidDirName   :=  BRAZ_PORT_esInvalidDirName;
        esConfirmRename    :=  BRAZ_PORT_esConfirmRename;
        esCannotAddDrv     :=  BRAZ_PORT_esCannotAddDrv;
        esNewFolder        :=  BRAZ_PORT_esNewFolder;
        esInvalidChars     :=  BRAZ_PORT_esInvalidChars;
        esNotFound         :=  BRAZ_PORT_esNotFound;
        esFilesIn          :=  BRAZ_PORT_esFilesIn;
        esFileOpFailed     :=  BRAZ_PORT_esFileOpFailed;
        esTreeSize         :=  BRAZ_PORT_esTreeSize;
        esAllSubDir        :=  BRAZ_PORT_esAllSubDir;
      end;  //873>
    lnSlovak:
      begin  //876<
        ewDrive            :=  SLOVAK_ewDrive;
        ewFolder           :=  SLOVAK_ewFolder;
        ewError            :=  SLOVAK_ewError;
        ewFile             :=  SLOVAK_ewFile;
        ewFrom             :=  SLOVAK_ewFrom;
        ewTo               :=  SLOVAK_ewTo;
        esCannot           :=  SLOVAK_esCannot;
        esSpecifyDir       :=  SLOVAK_esSpecifyDir;
        esInvalidDrvID     :=  SLOVAK_esInvalidDrvID;
        esDrvNotReady      :=  SLOVAK_esDrvNotReady;
        esExists           :=  SLOVAK_esExists;
        esInvalidDirName   :=  SLOVAK_esInvalidDirName;
        esConfirmRename    :=  SLOVAK_esConfirmRename;
        esCannotAddDrv     :=  SLOVAK_esCannotAddDrv;
        esNewFolder        :=  SLOVAK_esNewFolder;
        esInvalidChars     :=  SLOVAK_esInvalidChars;
        esNotFound         :=  SLOVAK_esNotFound;
        esFilesIn          :=  SLOVAK_esFilesIn;
        esFileOpFailed     :=  SLOVAK_esFileOpFailed;
        esTreeSize         :=  SLOVAK_esTreeSize;
        esAllSubDir        :=  SLOVAK_esAllSubDir;
      end;  //876>
    lnSlovenian:  //MPX1
      begin  //881<
        ewDrive            :=  SLO_ewDrive;
        ewFolder           :=  SLO_ewFolder;
        ewError            :=  SLO_ewError;
        ewFile             :=  SLO_ewFile;
        ewFrom             :=  SLO_ewFrom;
        ewTo               :=  SLO_ewTo;
        esCannot           :=  SLO_esCannot;
        esSpecifyDir       :=  SLO_esSpecifyDir;
        esInvalidDrvID     :=  SLO_esInvalidDrvID;
        esDrvNotReady      :=  SLO_esDrvNotReady;
        esExists           :=  SLO_esExists;
        esInvalidDirName   :=  SLO_esInvalidDirName;
        esConfirmRename    :=  SLO_esConfirmRename;
        esCannotAddDrv     :=  SLO_esCannotAddDrv;
        esNewFolder        :=  SLO_esNewFolder;
        esInvalidChars     :=  SLO_esInvalidChars;
        esNotFound         :=  SLO_esNotFound;
        esFilesIn          :=  SLO_esFilesIn;
        esFileOpFailed     :=  SLO_esFileOpFailed;
        esTreeSize         :=  SLO_esTreeSize;
        esAllSubDir        :=  SLO_esAllSubDir;
      end;  //881>
    lnSpanish:
      begin  //872<
        ewDrive            :=  SPANISH_ewDrive;   //873
        ewFolder           :=  SPANISH_ewFolder;   //873
        ewError            :=  SPANISH_ewError;
        ewFile             :=  SPANISH_ewFile;
        ewFrom             :=  SPANISH_ewFrom;
        ewTo               :=  SPANISH_ewTo;
        esCannot           :=  SPANISH_esCannot;
        esSpecifyDir       :=  SPANISH_esSpecifyDir;
        esInvalidDrvID     :=  SPANISH_esInvalidDrvID;
        esDrvNotReady      :=  SPANISH_esDrvNotReady;
        esExists           :=  SPANISH_esExists;
        esInvalidDirName   :=  SPANISH_esInvalidDirName;
        esConfirmRename    :=  SPANISH_esConfirmRename;
        esCannotAddDrv     :=  SPANISH_esCannotAddDrv;
        esNewFolder        :=  SPANISH_esNewFolder;
        esInvalidChars     :=  SPANISH_esInvalidChars;
        esNotFound         :=  SPANISH_esNotFound;
        esFilesIn          :=  SPANISH_esFilesIn;
        esFileOpFailed     :=  SPANISH_esFileOpFailed;
        esTreeSize         :=  SPANISH_esTreeSize;  //873
        esAllSubDir        :=  SPANISH_esAllSubDir;  //873
      end;  //872>
    lnSwedish:
      begin  //882<
        ewDrive            :=  SWEDISH_ewDrive;
        ewFolder           :=  SWEDISH_ewFolder;
        ewError            :=  SWEDISH_ewError;
        ewFile             :=  SWEDISH_ewFile;
        ewFrom             :=  SWEDISH_ewFrom;
        ewTo               :=  SWEDISH_ewTo;
        esCannot           :=  SWEDISH_esCannot;
        esSpecifyDir       :=  SWEDISH_esSpecifyDir;
        esInvalidDrvID     :=  SWEDISH_esInvalidDrvID;
        esDrvNotReady      :=  SWEDISH_esDrvNotReady;
        esExists           :=  SWEDISH_esExists;
        esInvalidDirName   :=  SWEDISH_esInvalidDirName;
        esConfirmRename    :=  SWEDISH_esConfirmRename;
        esCannotAddDrv     :=  SWEDISH_esCannotAddDrv;
        esNewFolder        :=  SWEDISH_esNewFolder;
        esInvalidChars     :=  SWEDISH_esInvalidChars;
        esNotFound         :=  SWEDISH_esNotFound;
        esFilesIn          :=  SWEDISH_esFilesIn;
        esFileOpFailed     :=  SWEDISH_esFileOpFailed;
        esTreeSize         :=  SWEDISH_esTreeSize;  //873
        esAllSubDir        :=  SWEDISH_esAllSubDir;  //873
      end;   //882>
    lnTurkish:
      begin  //886<
        ewDrive            :=  TURKISH_ewDrive;
        ewFolder           :=  TURKISH_ewFolder;
        ewError            :=  TURKISH_ewError;
        ewFile             :=  TURKISH_ewFile;
        ewFrom             :=  TURKISH_ewFrom;
        ewTo               :=  TURKISH_ewTo;
        esCannot           :=  TURKISH_esCannot;
        esSpecifyDir       :=  TURKISH_esSpecifyDir;
        esInvalidDrvID     :=  TURKISH_esInvalidDrvID;
        esDrvNotReady      :=  TURKISH_esDrvNotReady;
        esExists           :=  TURKISH_esExists;
        esInvalidDirName   :=  TURKISH_esInvalidDirName;
        esConfirmRename    :=  TURKISH_esConfirmRename;
        esCannotAddDrv     :=  TURKISH_esCannotAddDrv;
        esNewFolder        :=  TURKISH_esNewFolder;
        esInvalidChars     :=  TURKISH_esInvalidChars;
        esNotFound         :=  TURKISH_esNotFound;
        esFilesIn          :=  TURKISH_esFilesIn;
        esFileOpFailed     :=  TURKISH_esFileOpFailed;
        esTreeSize         :=  TURKISH_esTreeSize;
        esAllSubDir        :=  TURKISH_esAllSubDir;
      end;  //>886
  end;  //case
end;  {= InitializeVar =}  //872

//============== End of TLsDirTree21 ====================//



{*********************************************************}
{                  TLsDirTree21PopUp                      }
{*********************************************************}

constructor TLsDirTree21PopUp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //85 86<<
  Bmp1 := LoadImage(hInstance, 'NewFolder28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp2 := LoadImage(hInstance, 'EditFolder28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp3 := LoadImage(hInstance, 'DelFolder28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp4 := LoadImage(hInstance, 'Cut28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp5 := LoadImage(hInstance, 'Copy28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp6 := LoadImage(hInstance, 'Paste28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp7 := LoadImage(hInstance, 'TreeSize28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp8 := LoadImage(hInstance, 'DirContents28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp9 := LoadImage(hInstance, 'NetConnect28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp10 := LoadImage(hInstance, 'NetDisConnect28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp11 := LoadImage(hInstance, 'Attributes28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);  //862
  //85 86>>
end; {= Create =}

destructor TLsDirTree21PopUp.Destroy;
begin
  DeleteObject(Bmp1); //70
  DeleteObject(Bmp2); //70
  DeleteObject(Bmp3); //70
  DeleteObject(Bmp4); //86
  DeleteObject(Bmp5); //86
  DeleteObject(Bmp6); //86
  DeleteObject(Bmp7); //70
  DeleteObject(Bmp8);  //80
  DeleteObject(Bmp9);  //82
  DeleteObject(Bmp10); //82
  DeleteObject(Bmp11); //862
  inherited Destroy;
end; {Destroy}

function TLsDirTree21PopUp.AddNewItem(const aCaption: string;
  aShortCut: TShortCut; aChecked, aEnabled: Boolean; aGroup: integer;
  aOnClick: TNotifyEvent; hCtx: word; const aName: string; aTag: integer;
  aBitMap: HBitmap): TMenuItem;  //70 //862
begin
  Result := TMenuItem.Create(nil);
  with result do
  begin
    Caption := aCaption;
    ShortCut := aShortCut;
    Checked := aChecked;
    Enabled := aEnabled;
    GroupIndex := aGroup;
    OnClick := aOnClick;
    Name := aName;
    Tag := aTag;  //862
    BITMAP.Handle := aBitmap;
  end;
end; {= AddNewItem =}

procedure TLsDirTree21PopUp.SetDirTree(Value: TLsDirTree21);
begin
  FDirTree := Value;
end; {= SetDirTree =}

procedure TLsDirTree21PopUp.BuildItems; //70  //862<
begin
  //Set language-specific MenuItem captions
  Case TvLangID of
    lnEnglish:
      begin  //872ln<
        TvItemID_0  :=  ENGLISH_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  ENGLISH_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  ENGLISH_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  ENGLISH_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  ENGLISH_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  ENGLISH_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  ENGLISH_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  ENGLISH_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  ENGLISH_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  ENGLISH_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  ENGLISH_TvItemID_14; //'&Property';
      end;  {lnEnglish}  //872ln>
    lnChinese_Tra:
      begin  //879<
        TvItemID_0  :=  CHINESE_Tra_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  CHINESE_Tra_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  CHINESE_Tra_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  CHINESE_Tra_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  CHINESE_Tra_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  CHINESE_Tra_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  CHINESE_Tra_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  CHINESE_Tra_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  CHINESE_Tra_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  CHINESE_Tra_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  CHINESE_Tra_TvItemID_14; //'&Property';
      end;  {lnChinese_Tra}  //879>
    lnChinese_Sim:
      begin  //879<
        TvItemID_0  :=  CHINESE_Sim_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  CHINESE_Sim_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  CHINESE_Sim_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  CHINESE_Sim_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  CHINESE_Sim_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  CHINESE_Sim_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  CHINESE_Sim_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  CHINESE_Sim_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  CHINESE_Sim_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  CHINESE_Sim_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  CHINESE_Sim_TvItemID_14; //'&Property';
      end;  {lnChinese_Sim}  //879>
    lnDutch:
      begin  //875<
        TvItemID_0  :=  DUTCH_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  DUTCH_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  DUTCH_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  DUTCH_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  DUTCH_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  DUTCH_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  DUTCH_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  DUTCH_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  DUTCH_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  DUTCH_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  DUTCH_TvItemID_14; //'&Property';
      end;  {lnDutch}  //875>
    lnFrench:
      begin  //872<
        TvItemID_0  :=  FRENCH_TvItemID_0;  //'&Nouveau Dossier';
        TvItemID_1  :=  FRENCH_TvItemID_1;  //'&Renommer le Dossier';
        TvItemID_2  :=  FRENCH_TvItemID_2;  //'&Effacer le Dossier';
        TvItemID_4  :=  FRENCH_TvItemID_4;  //'Cou&per';
        TvItemID_5  :=  FRENCH_TvItemID_5;  //'&Copier';
        TvItemID_6  :=  FRENCH_TvItemID_6;  //'Co&ller';
        TvItemID_8  :=  FRENCH_TvItemID_8;  //'Taille de l''&arborescence';
        TvItemID_9  :=  FRENCH_TvItemID_9;  //'&Contenu du Dossier';
        TvItemID_11 :=  FRENCH_TvItemID_11; //'&Connecter Lecteur Réseau ...';
        TvItemID_12 :=  FRENCH_TvItemID_12; //'&Déconnecter Lecteur Réseau';
        TvItemID_14 :=  FRENCH_TvItemID_14; //'&Propriétés';
      end;  {lnFrench}  //872>
    lnGerman:
      begin  //872<
        TvItemID_0  :=  GERMAN_TvItemID_0;  //'&Neuer Ordner';
        TvItemID_1  :=  GERMAN_TvItemID_1;  //'&Ordner umbenennen';
        TvItemID_2  :=  GERMAN_TvItemID_2;  //'Ordner &löschen';
        TvItemID_4  :=  GERMAN_TvItemID_4;  //'ausschnei&den';
        TvItemID_5  :=  GERMAN_TvItemID_5;  //'&kopieren';
        TvItemID_6  :=  GERMAN_TvItemID_6;  //'&einfügen';
        TvItemID_8  :=  GERMAN_TvItemID_8;  //'Ordnergröße';
        TvItemID_9  :=  GERMAN_TvItemID_9;  //'&Ordner Inhalt';
        TvItemID_11 :=  GERMAN_TvItemID_11; //'Netzwerk &verbinden...';
        TvItemID_12 :=  GERMAN_TvItemID_12; //'Netzwerk &trennen';
        TvItemID_14 :=  GERMAN_TvItemID_14; //'Ei&genschaft';
      end;  {lnGerman}  //872>
    lnItalian:
      begin  //874<
        TvItemID_0  :=  ITALIAN_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  ITALIAN_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  ITALIAN_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  ITALIAN_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  ITALIAN_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  ITALIAN_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  ITALIAN_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  ITALIAN_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  ITALIAN_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  ITALIAN_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  ITALIAN_TvItemID_14; //'&Property';
      end;  {lnItalian}  //874>
    lnJapanese:
      begin  //880<
        TvItemID_0  :=  JAPANESE_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  JAPANESE_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  JAPANESE_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  JAPANESE_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  JAPANESE_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  JAPANESE_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  JAPANESE_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  JAPANESE_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  JAPANESE_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  JAPANESE_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  JAPANESE_TvItemID_14; //'&Property';
      end;  {lnJapanese}  //880>
    lnKorean:
      begin  //885<
        TvItemID_0  :=  KOREAN_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  KOREAN_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  KOREAN_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  KOREAN_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  KOREAN_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  KOREAN_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  KOREAN_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  KOREAN_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  KOREAN_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  KOREAN_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  KOREAN_TvItemID_14; //'&Property';
      end;  {lnKorean}  //885>
    lnPolish:
      begin  //876<
        TvItemID_0  :=  POLISH_TvItemID_0;   //'Nowy &Folder';
        TvItemID_1  :=  POLISH_TvItemID_1;   //'&Zmiana Nazwy Foldera';
        TvItemID_2  :=  POLISH_TvItemID_2;   //'&Usuñ Folder';
        TvItemID_4  :=  POLISH_TvItemID_4;   //'Wyt&nij';
        TvItemID_5  :=  POLISH_TvItemID_5;   //'&Kopiuj';
        TvItemID_6  :=  POLISH_TvItemID_6;   //'Wkl&ej';
        TvItemID_8  :=  POLISH_TvItemID_8;   //'Poka¿ &Drzewo Katalogów';
        TvItemID_9  :=  POLISH_TvItemID_9;   //'P&oka¿ Zawartoœæ Foldera';
        TvItemID_11 :=  POLISH_TvItemID_11;  //'&Po³¹cz z Dyskiem Sieciowym';
        TvItemID_12 :=  POLISH_TvItemID_12;  //'&Roz³¹cz Dysk Sieciowy';
        TvItemID_14 :=  POLISH_TvItemID_14;  //'&W³aœciwoœci';
      end;  {lnPolish}  //876>
    lnPortuguese:
      begin  //873<
        TvItemID_0  :=  BRAZ_PORT_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  BRAZ_PORT_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  BRAZ_PORT_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  BRAZ_PORT_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  BRAZ_PORT_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  BRAZ_PORT_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  BRAZ_PORT_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  BRAZ_PORT_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  BRAZ_PORT_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  BRAZ_PORT_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  BRAZ_PORT_TvItemID_14; //'&Property';
      end;  {lnPortuguese}  //873>
    lnSlovak:
      begin  //876<
        TvItemID_0  :=  SLOVAK_TvItemID_0;  //'&Nový prieèinok';
        TvItemID_1  :=  SLOVAK_TvItemID_1;  //'Pre&menova prieèinok';
        TvItemID_2  :=  SLOVAK_TvItemID_2;  //'O&dstráni prieèinok';
        TvItemID_4  :=  SLOVAK_TvItemID_4;  //'&Vystrihnú';
        TvItemID_5  :=  SLOVAK_TvItemID_5;  //'&Kopírova';
        TvItemID_6  :=  SLOVAK_TvItemID_6;  //'&Prilepi';
        TvItemID_8  :=  SLOVAK_TvItemID_8;  //'Ve¾kos pod&stromu';
        TvItemID_9  :=  SLOVAK_TvItemID_9;  //'&Obsah prieèinka';
        TvItemID_11 :=  SLOVAK_TvItemID_11; //'Pripo&ji sieový disk ...';
        TvItemID_12 :=  SLOVAK_TvItemID_12; //'Odpo&ji sieový disk';
        TvItemID_14 :=  SLOVAK_TvItemID_14; //'Vl&astnosti';
      end;  {lnSlovak}  //876>
    lnSlovenian:  //MPX1
      begin  //881<
        TvItemID_0  :=  SLO_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  SLO_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  SLO_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  SLO_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  SLO_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  SLO_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  SLO_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  SLO_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  SLO_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  SLO_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  SLO_TvItemID_14; //'&Property';
      end;  //881>
    lnSpanish:
      begin  //872ln<
        TvItemID_0  :=  SPANISH_TvItemID_0;  //'&Nueva carpeta';
        TvItemID_1  :=  SPANISH_TvItemID_1;  //'&Renombrar carpeta';
        TvItemID_2  :=  SPANISH_TvItemID_2;  //'&Borrar carpeta';
        TvItemID_4  :=  SPANISH_TvItemID_4;  //'Cor&tar';
        TvItemID_5  :=  SPANISH_TvItemID_5;  //'&Copiar';
        TvItemID_6  :=  SPANISH_TvItemID_6;  //'&Pegar';
        TvItemID_8  :=  SPANISH_TvItemID_8;  //'Ta&maño del árbol';
        TvItemID_9  :=  SPANISH_TvItemID_9;  //'C&ontenido de la carpeta';
        TvItemID_11 :=  SPANISH_TvItemID_11; //'Con&ectar unidad de red';
        TvItemID_12 :=  SPANISH_TvItemID_12; //'De&sconectar unidad de red';
        TvItemID_14 :=  SPANISH_TvItemID_14; //'&Propiedades';
      end;  {lnSpanish}  //872ln>
    lnSwedish:
      begin  //882<
        TvItemID_0  :=  SWEDISH_TvItemID_0;  //'&Ny Katalog';
        TvItemID_1  :=  SWEDISH_TvItemID_1;  //'&Döp om Katalog';
        TvItemID_2  :=  SWEDISH_TvItemID_2;  //'&Radera Katalog';
        TvItemID_4  :=  SWEDISH_TvItemID_4;  //'Klip&p ut';
        TvItemID_5  :=  SWEDISH_TvItemID_5;  //'&Kopiera';
        TvItemID_6  :=  SWEDISH_TvItemID_6;  //'&Klistra IN';
        TvItemID_8  :=  SWEDISH_TvItemID_8;  //'Träd &Storlek';
        TvItemID_9  :=  SWEDISH_TvItemID_9;  //'&Katalog Innehåll';
        TvItemID_11 :=  SWEDISH_TvItemID_11; //'&Koppla Nätverks Disk ...';
        TvItemID_12 :=  SWEDISH_TvItemID_12; //'Frigör Nätv&erksdisk';
        TvItemID_14 :=  SWEDISH_TvItemID_14; //'&Egenskaper';
      end;  //882>
    lnTurkish:
      begin  //886<
        TvItemID_0  :=  TURKISH_TvItemID_0;  //'&New Folder';
        TvItemID_1  :=  TURKISH_TvItemID_1;  //'&Rename Folder';
        TvItemID_2  :=  TURKISH_TvItemID_2;  //'&Delete Folder';
        TvItemID_4  :=  TURKISH_TvItemID_4;  //'Cu&t';
        TvItemID_5  :=  TURKISH_TvItemID_5;  //'&Copy';
        TvItemID_6  :=  TURKISH_TvItemID_6;  //'&Paste';
        TvItemID_8  :=  TURKISH_TvItemID_8;  //'Tree &Size';
        TvItemID_9  :=  TURKISH_TvItemID_9;  //'&Folder Contents';
        TvItemID_11 :=  TURKISH_TvItemID_11; //'&Map Network Drive ...';
        TvItemID_12 :=  TURKISH_TvItemID_12; //'Dis&Connect Network Drive';
        TvItemID_14 :=  TURKISH_TvItemID_14; //'&Property';
      end;  {lnTurkish}  //>886
  end;  //case

  //Build MenuItems
  Items.Add(AddNewItem(TvItemID_0, 0, False, True, 0,   //872ln
    ItemOnClick, 0, 'AddNode', 1, Bmp1));     //Items[0]
  Items.Add(AddNewItem(TvItemID_1, 0, False, True, 0,
    ItemOnClick, 0, 'EditNode', 2, Bmp2));    //Items[1]
  Items.Add(AddNewItem(TvItemID_2, 0, False, True, 0,
    ItemOnClick, 0, 'DelNode', 3, Bmp3));     //Items[2]
  Items.Add(NewLine);
  //86 <<
  Items.Add(AddNewItem(TvItemID_4, 0, False, True, 0,
    ItemOnClick, 0, 'CutNode', 4, Bmp4));     //Items[4]
  Items.Add(AddNewItem(TvItemID_5, 0, False, True, 0,
    ItemOnClick, 0, 'CopyNode', 5, Bmp5));    //Items[5]
  Items.Add(AddNewItem(TvItemID_6, 0, False, True, 0,
    ItemOnClick, 0, 'PasteNode', 6, Bmp6));   //Items[6]
  Items.Add(NewLine);
  //86 >>
  Items.Add(AddNewItem(TvItemID_8, 0, False, True, 0,
    ItemOnClick, 0, 'GetTreeSize', 7, Bmp7)); //Items[8]
  Items.Add(AddNewItem(TvItemID_9, 0, False, True, 0,
    ItemOnClick, 0, 'ShowFolderContents', 8, Bmp8));  //Items[9]  //80
  Items.Add(NewLine);  //82
  Items.Add(AddNewItem(TvItemID_11, 0, False, True, 0,
    ItemOnClick, 0, 'ConnectNetResource', 9, Bmp9));  //Items[11]  //82
  Items.Add(AddNewItem(TvItemID_12, 0, False, True, 0,
    ItemOnClick, 0, 'DisConnectNetResource', 10, Bmp10)); //Items[12]  //82
  Items.Add(NewLine);  //862
  Items.Add(AddNewItem(TvItemID_14, 0, False, True, 0,
    ITemOnClick, 0, 'ItemProperty', 11, Bmp11)); //Items[14]  //862
end; {= BuildItems =}

procedure TLsDirTree21PopUp.ItemOnClick(Sender: TObject);
var
  Node: TTreeNode;
begin
  if (TMenuItem(Sender).Name = '') or (TMenuItem(Sender).Tag = 0) then
    exit;
  with Sender as TMenuItem do  //862
  begin
    case Tag of
      1: FDirTree.AddNewNode(FDirTree.Selected, '');
      2: FDirTree.Selected.EditText;
      3: begin
           Node := FDirTree.Selected;
           if Assigned(Node) then
             FDirTree.DeleteNode(Node);
         end;
      4: FDirTree.CutOrCopyNode(2);
      5: FDirTree.CutOrCopyNode(0);
      6: FDirTree.PasteNode;
      7: FDirTree.GetTreeSize;
      8: FDirTree.SHowFolderContents;  //80
      9: FDirTree.ConnectNetResource(Sender); //82
      10: FDirTree.DisConnectNetResource(Sender); //82
      11: FDirTree.ShowNodeProperties; //862
    end;  //case
  end;  //with ...
end; {= ItemOnClick =}

procedure TLsDirTree21PopUp.PopUp(X, Y: integer);
begin
  with FDirTree do
  begin
    Self.Items[0].Enabled := (Selected.Level > 0) and (Selected <> nil);
    Self.Items[1].Enabled := (Selected.Level > 1) and (Selected <> nil);
    Self.Items[2].Enabled := (Selected.Level > 1) and (Selected <> nil);
    //86 >>
    Self.Items[4].Enabled := (Selected.Level > 1) and (Selected <> nil);
    Self.Items[5].Enabled := (Selected.Level > 1) and (Selected <> nil);
    Self.Items[6].Enabled := (Selected.Level > 1) and (Selected <> nil)
      and (FisCutCopy = True) ;
    //86 <<
    Self.ITems[8].Enabled := (Selected.Level > 0) and (Selected <> nil);
    Self.ITems[9].Enabled := (Selected.Level > 0) and (Selected <> nil);  //80^
    Self.Items[14].Enabled := (Selected.Level > 0) and (Selected <> nil);  //862
  end;
  inherited Popup(X + 10, Y + 10);
end; {= PopUp =}

//============== End of TLsDirTree21PopUp ==============//


{*********************************************************}
{                  LsFilelistView28                       }
{*********************************************************}

constructor TLsFilelistView28.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAbout := 'Version 2.8.8.6';  //861
  FSelectedFiles := TStringList.Create;
  MultiSelect := True;
  ReadOnly := False;
  SetDaTeFormat(df_Customize);  //(df_MMddyyyy); //81
  SetTimeFormat(tfShortFormat); //85
  HideSelection := False;
  SetParentDirEnabled(True);  //870
  Createimages;

  ViewStyle := vsReport;
  FMask := DefaultMask;
  FSortForward := True;
  FSortColumn := 0;
  OnColumnClick := ColumnClick;
  OnCustomDrawItem := CustomDrawItem; //883

  Bmp_Up28 := LoadImage(hInstance, 'Bmp_Up28', IMAGE_BITMAP, 0, 0,
              LR_LOADTRANSPARENT or LR_LOADMAP3DCOLORS);  //873
  Bmp_Down28 := LoadImage(hInstance, 'Bmp_Down28', IMAGE_BITMAP, 0, 0,
              LR_LOADTRANSPARENT or LR_LOADMAP3DCOLORS); //873
  FBitmap := TBitmap.Create; //70

  FColWidth_Name := 165;  //80
  FColWidth_Size := 75;   //80
  FColWidth_Type := 95;   //80
  FColWidth_Mod  := 125;  //80
  FColWidth_Attr := 40;   //80

  ShowColumnHeaders := True;
  FShowFolders := True;
  FOpenByDblClick := True;  //882
  FOpenByEnter := True;   //882
  FColumnClickEnabled := True;  //70
  FTextColor:= False;

  SetLanguage(lnEnglish);  //872ln
  InitializeVar;  //872ln

  FPopupMenu := TLsFilelistView28PopUp.Create(Self);
  FPopupMenu.BuildItems;
  FPopupMenu.AutoPopup := True;
  FPopupMenu.FFileListView := Self;
  PopupMenu := FPopupMenu;
  FPopUpMenuEnabled := True;
  FFileType := [ftReadOnly, ftHidden, ftSystem, ftArchive];
  if FParentDirCaption = '' then
    FParentDirCaption := ' Up one level';  //872
  FDirectory := 'My Computer'; //'Drives';  //885r2

  //878DragDrop>
  FNumDropped := 0;
  FDroppedFiles := TStringList.Create;
  FWndHandle := 0;
  InitFDragControl;
  DragAcceptFiles( FWndHandle, True );
  //878DragDrop<
  FShortCuts := TLsFileListShortCuts.Create(Self);  //881 MPX3
  FItemColors := TListItemColors.Create;  //883cl

  //Ales  //882<
  OwnerData := True;
  FFlieInfo := Nil;
  FFlieInfoCount := 0;
  //Ales  //>882
end; {= Create =}

destructor TLsFilelistView28.Destroy;
begin
//Ales  //882<
  FFlieInfoCount := 0;
  SetLength (FFlieInfo, FFlieInfoCount);
//Ales  //>882
  FShortCuts.Free;  //881 MPX3
  LImgLst.Free;
  SImgLst.Free;
  FPopupMenu.Free;
  FSelectedFiles.Free;
  FBitMap.Free;
  DeleteObject(Bmp_Up28);
  DeleteObject(Bmp_Down28);
  //878DragDrop>
  DestroyFDragControl;
  //878DragDrop<
  //883cl>
  FItemColors.Free;
  FItemColors := nil;
  //883cl<
  inherited Destroy;
end; {= Destroy =}

procedure TLsFilelistView28.CreateWnd;
begin
  inherited CreateWnd;
//  Font.Size := 8;  //80
//  Font.Name := 'MS Sans Serif';  //80
  if not (csLoading in ComponentState) then
    CreateFileColumns;
end; {= CreateWnd =}

function TLsFilelistView28.GetDriveFreeSpace: Int64; //LongInt;  //874
begin
  Result := GetFreeDiskSize(Copy(FDirectory, 1, 1) + ':\');
end; {= GetDriveFreeSpace =}

function TLsFilelistView28.GetSelectedNum: Integer;
begin
  Result := SelCount;
  if Result = 0 then Result := Items.Count;
end; {=GetSelectedNum =}

function TLsFilelistView28.GetSelectedSize: Int64;  //874  //882ss>
var
  i: UInt;
  FSize: LongInt;
//  FName: string;
//  win32FD: TWin32FindData;
//  hFindFile: THandle;
begin
  Result := 0;
  FSize := 0;
//  hFindFile := 0;
  if SelCount = 0 then exit;
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].selected then
//    begin
      FSize := FSize + FFlieInfo[Items[i].Index].fiSize;
//      FName := ExtractFileName(Items[i].SubItems[4] + #0);
//      hFindFile := FindFirstFile(pChar(FName), win32FD);
//      if hFindFile <> INVALID_HANDLE_VALUE then
//        FSize := FSize + ((win32FD.nFileSizeHigh * MAXDWORD) +
//          win32FD.nFileSizeLow);
//    end;
  end;
//  Windows.FindClose(hFindFile);
  Result := FSize;
end; {= GetSelectedSize =}  //882ss

function TLsFilelistView28.GetDirectory: string;
begin
  Result := FDirectory;
end; {= GetDirectory =}

procedure TLsFilelistView28.SetColWidth_Name(Value: Integer);  //80+
begin
  if Value <> FColWidth_Name then
  begin
    FColWidth_Name := Value;
    if FileColExists then
      Columns[0].Width := FColWidth_Name; //881
  end;
end;  {= SetColWidth_Name =}

procedure TLsFilelistView28.SetColWidth_Size(Value: Integer);  //80+
begin
  if Value <> FColWidth_Size then
  begin
    FColWidth_Size := Value;
    if FileColExists then
      Columns[1].Width := FColWidth_Size; //881
  end;
end;  {= SetColWidth_Size =}

procedure TLsFilelistView28.SetColWidth_Type(Value: Integer);  //80+
begin
  if Value <> FColWidth_Type then
  begin
    FColWidth_Type := Value;
    if FileColExists then
      Columns[2].Width := FColWidth_Type; //881
  end;
end;  {= SetColWidth_Type =}

procedure TLsFilelistView28.SetColWidth_Mod(Value: Integer);  //80+
begin
  if Value <> FColWidth_Mod then
  begin
    FColWidth_Mod := Value;
    if FileColExists then
      Columns[3].Width := FColWidth_Mod; //881
  end;
end;  {= SetColWidth_Mod =}

procedure TLsFilelistView28.SetColWidth_Attr(Value: Integer);  //80+
begin
  if Value <> FColWidth_Attr then
  begin
    FColWidth_Attr := Value;
    if FileColExists then Columns[4].Width := FColWidth_Attr; //881
  end;
end;  {= SetColWidth_Attr =}

procedure TLsFilelistView28.SetDirectory(NewDir: string);
begin
  if AnsiCompareText(NewDir, FDirectory) = 0 then exit;
  if (AnsiCompareText(NewDir, 'My Computer') = 0) then  //'DRIVES') then  //885r2
  begin
    FDirectory := NewDir;
    UpdateFileList;
  end
  else
  begin
    if not DirectoryExists(NewDir) then exit;
    NewDir := AddSlash(NewDir);
    SetCurrentDir(NewDir);
    FDirectory := NewDir;
    UpdateFileList;
  end;
end; {= SetDirectory =}

procedure TLsFilelistView28.SetDirTreeCombo(val: TLsDirTreeCombo28);
begin
  if FDirTreeCombo = Val then
    exit
  else
  begin
    if Assigned(FDirTreeCombo) then
      FDirTreeCombo.FileList := nil;
    FDirTreeCombo := Val;
  end;
  if Assigned(FDirTreeCombo) then
    FDirTreeCombo.FileList := self;
end; {= SetDirTreeCombo =}

procedure TLsFilelistView28.SetDirTree(VaL: TLsDirTree21);
begin
  if FDirTree = Val then
    exit
  else
  begin
    if Assigned(FDirTree) then
      FDirTree.FileList := nil;
    FDirTree := Val;
  end;
  if Assigned(FDirTree) then
    FDirTree.FileList := self;
end; {= SetDirTree =}

//883cl>
procedure TLsFileListView28.SetItemColors(Value: TListItemColors);
begin
  FItemColors.FAttr_Archive  := Value.FAttr_Archive;
  FItemColors.FAttr_Hidden   := Value.FAttr_Hidden;
  FItemColors.FAttr_Readonly := Value.FAttr_Readonly;
  FItemColors.FAttr_System   := Value.FAttr_System;
end;  {= SetItemColors =}
//883cl<

procedure TLsFilelistView28.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDirTree) then
    FDirTree := nil;
  if (Operation = opRemove) and (AComponent = FDirTreeCombo) then
    FDirTreeCombo := nil;
end; {= Notification =}

procedure TLsFilelistView28.SetMask(const NewMasks: string);
begin
  if FMask <> NewMasks then
  begin
    FMask := NewMasks;
    UpdateFileList;
  end;
end; {= SetMask =}

function TLsFilelistView28.GetSelectedItem: string;
begin
  Result := FSelectedItem;
end; {= GetSelectedItem =}

procedure TLsFilelistView28.SetSelectedItem(NewItem: string);
begin
  if FSelectedItem = NewItem then exit;
  FSelectedItem := NewItem;
end; {= SetSelectedItem =}

procedure TLsFilelistView28.SetFileType(NewFileType: TFileType);
begin
  if NewFileType <> FFileType then
  begin
    FFileType := NewFileType;
    UpdateFileList;
  end;
end; {= SetFileType =}

procedure TLsFileListview28.SetFileSize(Value: TFileSize);  //876
begin
  if Value <> FFileSize then
    FFileSize := Value;
end;  {= SetFileSize =}

procedure TLsFilelistView28.SetDaTeFormat(Value: TDtFormat); //70
var
  DefaultLCID: LCID;
begin
  if Value <> FDateFormat then
    FDateFormat := Value;
  DefaultLCID := GetThreadLocale;
  case FDateFormat of
    df_MMddyyyy: FDFormatStr := 'MM/dd/yyyy';
    df_MMddyy: FDFormatStr := 'MM/dd/yy';
    df_ddMMyyyy: FDFormatStr := 'dd/MM/yyyy';
    df_ddMMyy_GB: FDFormatStr := 'dd/MM/yy';
    df_ddMMyy_DE: FDFormatStr := 'dd.MM.yy';
    df_ddMMyy_IT: FDFormatStr := 'dd-MM-yy';
    df_yyyyMMdd: FDFormatStr := 'yyyy-MM-dd';
    df_yyMMdd: FDFormatStr := 'yy-MM-dd';
    df_Customize: FDFormatStr :=
      GetLocaleStr(DefaultLCID, LOCALE_SSHORTDATE, '');
  end;
end; {= SetDaTeFormat =}

procedure TLsFileListView28.SetTimeFormat(Value: TTmFormat);  //85 <<
var
  DeFaultLCID: LCID;
  i: integer;
begin
  if Value <> FTimeFormat then
    FTimeFormat := Value;
  case FTimeFormat of
    tfLongFormat :  FTFormatStr := LongTimeFormat;
    tfShortFormat:  FTFormatStr := ShortTimeFormat;
    tfCustomize  :
      begin
        DefaultLCID := GetThreadLocale;
        FTFormatStr := '';
        FTFormatStr := GetLocaleStr(DefaultLCID, LOCALE_STIMEFORMAT, '');
        i := Pos('t', FTFormatStr);
        if i > 0 then
          FTFormatStr := Copy(FTFormatStr, 1, i -1 );
        FTFormatStr := FTFormatStr + ' AM/PM';
//        ShowMessage(FTFormatStr);  //
      end;
  end;
end; {= SetTFormatStr =}  //85 >>

procedure TLsFilelistView28.SetShowFolders(Value: Boolean);
begin
  if FShowFolders = Value then
    exit;
  FShowFolders := Value;
  UpdateFileList;
end; {= SetShowFolders =}

procedure TLsFilelistView28.SetHideFileExt(Value: Boolean);
begin
  if Value <> FHideFileExt then
    FHideFileExt := Value;
end; {= SetHideFileExt =}

procedure TLsFileListView28.SetParentDirEnabled(Value: Boolean);  //870 <<
begin
  if Value <> FParentDirEnabled then
    FParentDirEnabled := Value;
end;  {= SetParentDirEnabled =}  //870 >>

procedure TLsFilelistView28.SetParentDirCaption(Value: string);  //872
begin
  Value := Trim(Value);
  if ansiCompareText(Value, Trim(FParentDirCaption)) <> 0 then
    FParentDirCaption := #32 + Value;  //' ' + Value;  //876
end;  {= SetParentDirCaption =}  //872

procedure TLsFilelistView28.SetLanguage(Value: TLanguage);  //872ln
var
  wPrimaryLangID,
    wSubLangID,
    LangID: integer;

  function PRIMARYLANGID(lgid: WORD): WORD;
  begin
    Result:=lgid and $03FF;
  end;

  function SUBLANGID(lgid: WORD): WORD;
  begin
    Result:=lgid shr 10;
  end;

begin
  if Value = lnSysDefault then
  begin
//    LangID := GetSystemDefaultLangID and $3FF; //SysLocale.PriLangID;
    LangID := GetSystemDefaultLangID;    //879>
    wPrimaryLangID := PRIMARYLANGID(LangID);      //SysLocale.PriLangID;
    wSubLangID := SUBLANGID(LangID);     //879<
    // ShowMessage(IntToStr(LangID));             //debugging
    Case wPrimaryLangID of  //LangID of    //879
      LANG_ENGLISH      : Value := lnEnglish;     //$09
      LANG_DUTCH        : Value := lnDutch;       //$13  //875
      LANG_FRENCH       : Value := lnFrench;      //$0C
      LANG_GERMAN       : Value := lnGerman;      //$07
      LANG_ITALIAN      : Value := lnItalian;     //$10  //874
      LANG_JAPANESE     : Value := lnJapanese;    //$11  //880
      LANG_KOREAN       : Value := lnKorean;      //$12  //885
      LANG_POLISH       : Value := lnPolish;      //$15  //876
      LANG_PORTUGUESE   : Value := lnPortuguese;  //$16  //873
      LANG_SLOVAK       : Value := lnSlovak;      //$1B  //876
      LANG_SLOVENIAN    : Value := lnSlovenian;   //$24  //881 //MPX1
      LANG_SPANISH      : Value := lnSpanish;     //$0A
      LANG_SWEDISH      : Value := lnSwedish;     //$1d  //882
      LANG_TURKISH      : Value := lnTurkish;     //$1f  //886
      LANG_CHINESE      :                         //$04
        begin
          case wSubLangID of
            SUBLANG_CHINESE_SIMPLIFIED: Value := lnChinese_Sim;
          else
            Value := lnChinese_Tra;
          end;
        end;
    else
      Value := lnEnglish;
    end;  //case
  end;

  if value <> FLanguage then
  begin
    FLanguage := Value;
    LvLangID := FLanguage;
    InitializeVar;
    FPopupMenu.Free;
    Application.ProcessMessages;
    FPopupMenu := TLsFilelistView28PopUp.Create(Self);
    FPopupMenu.BuildItems;
    FPopupMenu.AutoPopup := True;
    FPopupMenu.FFileListView := Self;
    PopupMenu := FPopupMenu;
    FPopUpMenuEnabled := True;
    ReCreateWnd;
  end;
end;  {= SetLanguage =}  //872ln


procedure TLsFilelistView28.Createimages;
var
  sfi: TSHFileInfo;
  Reg: TRegistry;  //872

  procedure LoadParentBmp;  //872<
  begin
    LImgLst.ResInstLoad(hInstance, rtBitmap, 'Parent_L282', clNone);
    SImgLst.ResInstLoad(hInstance, rtBitmap, 'Parent_S282', clNone);
    ParentImgIdx := LImgLst.Count - 1;
  end;  //872>

  function CompareBmps(Idx: integer): Boolean;  //872<
  var
    Bmp1, Bmp2: TBitmap;
    Ms1, Ms2: TMemoryStream;
    Idx2: integer;
  begin
    Result := False;
    Idx2 := -1;
    Bmp1 := TBitmap.Create;
    Bmp2 := TBitmap.Create;
    Ms1 := TMemoryStream.Create;
    Ms2 := TMemoryStream.Create;
    try
      LImgLst.GetBitmap(Idx, Bmp1);
      LImgLst.ResInstLoad(hInstance, rtBitmap, 'Parent_L282', clNone);
      Idx2 := LImgLst.Count - 1;
      LImgLst.GetBitmap(Idx2, Bmp2);
      Bmp1.SaveToStream(Ms1);
      Bmp2.SaveToStream(Ms2);
      if Ms1.Size = Ms2.Size then
        Result := CompareMem(Ms1.Memory, Ms2.Memory, Ms1.Size);
    finally
      LImgLst.Delete(Idx2);
      Bmp1.Free;
      Bmp2.Free;
      Ms1.Free;
      Ms2.Free;
    end;
  end;  //872>

begin   //862
  ParentImgIdx := -1;
  LImgLst := TImageList.Create(Self);
  LImgLst.Handle := SHGetFileInfo('', 0, sfi, SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX or SHGFI_LARGEICON);  //or SHGFI_SHELLICONSIZE);  //871
  if LImgLst.Handle <> 0 then
    LImgLst.ShareImages := True;

  SImgLst := TImageList.Create(Self);
  SImgLst.Handle := SHGetFileInfo('', 0, sfi, SizeOf(TSHFileInfo),
      SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if SImgLst.Handle <> 0 then
    SImgLst.ShareImages := True;
  //872<
  Reg := TRegistry.Create;
  try
    Reg.Rootkey := HKEY_CURRENT_USER;
    Reg.OpenKey(LsFileExplorerKey, True);
    try
      case  Reg.ValueExists('ParentImgIdx') of
        True:
          begin
            ParentImgIdx := Reg.ReadInteger('ParentImgIdx');
            if (CompareBmps(ParentImgIdx) = False) or
               (ParentImgIdx > LImgLst.Count - 1) then
            begin
              LoadParentBmp;
              Reg.WriteInteger('ParentImgIdx', ParentImgIdx);
            end;
          end;
        False:
          begin
            LoadParentBmp;
            Reg.WriteInteger('ParentImgIdx', ParentImgIdx);
          end;
      end;  //case
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;  //872>
  LargeImages := LImgLst;
  SmallImages := SImgLst;
end; {= CreateImages =}

procedure TLsFilelistView28.CreateDriveColumns;
var
  i: integer;
begin
  //872ln<
  Columns.Clear;
  Columns.BeginUpdate;
  for i := 0 to 4 do
    Columns.Add;
  //Caption
  Case FLanguage of
    lnEnglish:
      begin  //872>
        Columns[0].Caption := ENGLISH_ColIdName;
        Columns[1].Caption := ENGLISH_ColIdType;
        Columns[2].Caption := ENGLISH_ColIdHdSize;
        Columns[3].Caption := ENGLISH_ColIdFSpace;
        Columns[4].Caption := ENGLISH_ColIdAttr;
      end;  //872>
    lnChinese_Tra:
      begin  //879<
        Columns[0].Caption := CHINESE_Tra_ColIdName;
        Columns[1].Caption := CHINESE_Tra_ColIdType;
        Columns[2].Caption := CHINESE_Tra_ColIdHdSize;
        Columns[3].Caption := CHINESE_Tra_ColIdFSpace;
        Columns[4].Caption := CHINESE_Tra_ColIdAttr;
      end;    //879>
    lnChinese_Sim:
      begin  //879<
        Columns[0].Caption := CHINESE_Sim_ColIdName;
        Columns[1].Caption := CHINESE_Sim_ColIdType;
        Columns[2].Caption := CHINESE_Sim_ColIdHdSize;
        Columns[3].Caption := CHINESE_Sim_ColIdFSpace;
        Columns[4].Caption := CHINESE_Sim_ColIdAttr;
      end;  //879>
    lnDutch:
      begin  //875<
        Columns[0].Caption := DUTCH_ColIdName;
        Columns[1].Caption := DUTCH_ColIdType;
        Columns[2].Caption := DUTCH_ColIdHdSize;
        Columns[3].Caption := DUTCH_ColIdFSpace;
        Columns[4].Caption := DUTCH_ColIdAttr;
      end;  //875>
    lnFrench:
      begin  //872<
        Columns[0].Caption := FRENCH_ColIdName;
        Columns[1].Caption := FRENCH_ColIdType;
        Columns[2].Caption := FRENCH_ColIdHdSize;
        Columns[3].Caption := FRENCH_ColIdFSpace;
        Columns[4].Caption := FRENCH_ColIdAttr;
      end;
    lnGerman:
      begin
        Columns[0].Caption := GERMAN_ColIdName;
        Columns[1].Caption := GERMAN_ColIdType;
        Columns[2].Caption := GERMAN_ColIdHdSize;
        Columns[3].Caption := GERMAN_ColIdFSpace;
        Columns[4].Caption := GERMAN_ColIdAttr;
      end;  //872>
    lnItalian:
      begin  //874<
        Columns[0].Caption := ITALIAN_ColIdName;
        Columns[1].Caption := ITALIAN_ColIdType;
        Columns[2].Caption := ITALIAN_ColIdHdSize;
        Columns[3].Caption := ITALIAN_ColIdFSpace;
        Columns[4].Caption := ITALIAN_ColIdAttr;
      end;  //874>
    lnJapanese:
      begin  //880<
        Columns[0].Caption := JAPANESE_ColIdName;
        Columns[1].Caption := JAPANESE_ColIdType;
        Columns[2].Caption := JAPANESE_ColIdHdSize;
        Columns[3].Caption := JAPANESE_ColIdFSpace;
        Columns[4].Caption := JAPANESE_ColIdAttr;
      end;  //880>
    lnKorean:
      begin  //885<
        Columns[0].Caption := KOREAN_ColIdName;
        Columns[1].Caption := KOREAN_ColIdType;
        Columns[2].Caption := KOREAN_ColIdHdSize;
        Columns[3].Caption := KOREAN_ColIdFSpace;
        Columns[4].Caption := KOREAN_ColIdAttr;
      end;  //885>
    lnPolish:
      begin  //876<
        Columns[0].Caption := POLISH_ColIdName;
        Columns[1].Caption := POLISH_ColIdType;
        Columns[2].Caption := POLISH_ColIdHdSize;
        Columns[3].Caption := POLISH_ColIdFSpace;
        Columns[4].Caption := POLISH_ColIdAttr;
      end;  //876>
    lnPortuguese:
      begin  //873<
        Columns[0].Caption := BRAZ_PORT_ColIdName;
        Columns[1].Caption := BRAZ_PORT_ColIdType;
        Columns[2].Caption := BRAZ_PORT_ColIdHdSize;
        Columns[3].Caption := BRAZ_PORT_ColIdFSpace;
        Columns[4].Caption := BRAZ_PORT_ColIdAttr;
      end;  //873>
    lnSlovak:
      begin  //876<
        Columns[0].Caption := SLOVAK_ColIdName;
        Columns[1].Caption := SLOVAK_ColIdType;
        Columns[2].Caption := SLOVAK_ColIdHdSize;
        Columns[3].Caption := SLOVAK_ColIdFSpace;
        Columns[4].Caption := SLOVAK_ColIdAttr;
      end;  //876>
    lnSlovenian:  //MPX1
      begin  //881<
        Columns[0].Caption := SLO_ColIdName;
        Columns[1].Caption := SLO_ColIdType;
        Columns[2].Caption := SLO_ColIdHdSize;
        Columns[3].Caption := SLO_ColIdFSpace;
        Columns[4].Caption := SLO_ColIdAttr;
      end;  //881>
    lnSpanish:
      begin
        Columns[0].Caption := SPANISH_ColIdName;
        Columns[1].Caption := SPANISH_ColIdType;
        Columns[2].Caption := SPANISH_ColIdHdSize;
        Columns[3].Caption := SPANISH_ColIdFSpace;
        Columns[4].Caption := SPANISH_ColIdAttr;
      end;  //872>
    lnSwedish:
      begin  //882<
        Columns[0].Caption := SWEDISH_ColIdName;
        Columns[1].Caption := SWEDISH_ColIdType;
        Columns[2].Caption := SWEDISH_ColIdHdSize;
        Columns[3].Caption := SWEDISH_ColIdFSpace;
        Columns[4].Caption := SWEDISH_ColIdAttr;
      end;  //882>
    lnTurkish:
      begin  //886>
        Columns[0].Caption := TURKISH_ColIdName;
        Columns[1].Caption := TURKISH_ColIdType;
        Columns[2].Caption := TURKISH_ColIdHdSize;
        Columns[3].Caption := TURKISH_ColIdFSpace;
        Columns[4].Caption := TURKISH_ColIdAttr;
      end;  //>886
  end;  //case
  //Width
  Columns[0].Width := 160;
  Columns[1].Width := 110;
  Columns[2].Width := 90;
  Columns[3].Width := 90;
  Columns[4].Width := 40;
  //Alignment
  Columns[0].Alignment := taLeftJustify;
  Columns[1].Alignment := taLeftJustify;
  Columns[2].Alignment := taRightJustify;
  Columns[3].Alignment := taRightJustify;
  Columns[4].Alignment := taLeftJustify;
  Columns.EndUpdate;
  FileColExists := False;  //872ln
end; {= CreateDriveColumns =}

procedure TLsFilelistView28.CreateFileColumns;
var
  i: integer;
begin
  //872ln<
  Columns.Clear;
  Columns.BeginUpdate;
  for i := 0 to 4 do
    Columns.Add;
  //Caption
  Case FLanguage of
    lnEnglish:
      begin  //872<
        Columns[0].Caption := ENGLISH_ColIDName;
        Columns[1].Caption := ENGLISH_ColIDSize;
        Columns[2].Caption := ENGLISH_ColIDType;
        Columns[3].Caption := ENGLISH_ColIDDate;
        Columns[4].Caption := ENGLISH_ColIDAttr;
      end;  //872>
    lnChinese_Tra:
      begin  //879<
        Columns[0].Caption := CHINESE_Tra_ColIDName;
        Columns[1].Caption := CHINESE_Tra_ColIDSize;
        Columns[2].Caption := CHINESE_Tra_ColIDType;
        Columns[3].Caption := CHINESE_Tra_ColIDDate;
        Columns[4].Caption := CHINESE_Tra_ColIDAttr;
      end;
    lnChinese_Sim:
      begin
        Columns[0].Caption := CHINESE_Sim_ColIDName;
        Columns[1].Caption := CHINESE_Sim_ColIDSize;
        Columns[2].Caption := CHINESE_Sim_ColIDType;
        Columns[3].Caption := CHINESE_Sim_ColIDDate;
        Columns[4].Caption := CHINESE_Sim_ColIDAttr;
      end;  //879>
    lnDutch:
      begin  //875<
        Columns[0].Caption := DUTCH_ColIDName;
        Columns[1].Caption := DUTCH_ColIDSize;
        Columns[2].Caption := DUTCH_ColIDType;
        Columns[3].Caption := DUTCH_ColIDDate;
        Columns[4].Caption := DUTCH_ColIDAttr;
      end;  //875>
    lnFrench:
      begin  //872<
        Columns[0].Caption := FRENCH_ColIDName;
        Columns[1].Caption := FRENCH_ColIDSize;
        Columns[2].Caption := FRENCH_ColIDType;
        Columns[3].Caption := FRENCH_ColIDDate;
        Columns[4].Caption := FRENCH_ColIDAttr;
      end;  //872>
    lnGerman:
      begin  //872<
        Columns[0].Caption := GERMAN_ColIDName;
        Columns[1].Caption := GERMAN_ColIDSize;
        Columns[2].Caption := GERMAN_ColIDType;
        Columns[3].Caption := GERMAN_ColIDDate;
        Columns[4].Caption := GERMAN_ColIDAttr;
      end;  //872><
    lnItalian:
      begin  //874<
        Columns[0].Caption := ITALIAN_ColIDName;
        Columns[1].Caption := ITALIAN_ColIDSize;
        Columns[2].Caption := ITALIAN_ColIDType;
        Columns[3].Caption := ITALIAN_ColIDDate;
        Columns[4].Caption := ITALIAN_ColIDAttr;
      end;  //874>
    lnJapanese:
      begin  //880<
        Columns[0].Caption := JAPANESE_ColIDName;
        Columns[1].Caption := JAPANESE_ColIDSize;
        Columns[2].Caption := JAPANESE_ColIDType;
        Columns[3].Caption := JAPANESE_ColIDDate;
        Columns[4].Caption := JAPANESE_ColIDAttr;
      end;  //880>
    lnKorean:
      begin  //885<
        Columns[0].Caption := KOREAN_ColIDName;
        Columns[1].Caption := KOREAN_ColIDSize;
        Columns[2].Caption := KOREAN_ColIDType;
        Columns[3].Caption := KOREAN_ColIDDate;
        Columns[4].Caption := KOREAN_ColIDAttr;
      end;  //885>
    lnPolish:
      begin  //876<
        Columns[0].Caption := POLISH_ColIDName;
        Columns[1].Caption := POLISH_ColIDSize;
        Columns[2].Caption := POLISH_ColIDType;
        Columns[3].Caption := POLISH_ColIDDate;
        Columns[4].Caption := POLISH_ColIDAttr;
      end;  //876>
    lnPortuguese:
      begin  //873<
        Columns[0].Caption := BRAZ_PORT_ColIDName;
        Columns[1].Caption := BRAZ_PORT_ColIDSize;
        Columns[2].Caption := BRAZ_PORT_ColIDType;
        Columns[3].Caption := BRAZ_PORT_ColIDDate;
        Columns[4].Caption := BRAZ_PORT_ColIDAttr;
      end;  //873>
    lnSlovak:
      begin  //876<
        Columns[0].Caption := SLOVAK_ColIDName;
        Columns[1].Caption := SLOVAK_ColIDSize;
        Columns[2].Caption := SLOVAK_ColIDType;
        Columns[3].Caption := SLOVAK_ColIDDate;
        Columns[4].Caption := SLOVAK_ColIDAttr;
      end;  //876>
    lnSlovenian:  //MPX1
      begin  //881<
        Columns[0].Caption := SLO_ColIDName;
        Columns[1].Caption := SLO_ColIDSize;
        Columns[2].Caption := SLO_ColIDType;
        Columns[3].Caption := SLO_ColIDDate;
        Columns[4].Caption := SLO_ColIDAttr;
      end;  //881>
    lnSpanish:
      begin  //872<
        Columns[0].Caption := SPANISH_ColIDName;
        Columns[1].Caption := SPANISH_ColIDSize;
        Columns[2].Caption := SPANISH_ColIDType;
        Columns[3].Caption := SPANISH_ColIDDate;
        Columns[4].Caption := SPANISH_ColIDAttr;
      end;  //872>
    lnSwedish:
      begin  //882<
        Columns[0].Caption := SWEDISH_ColIDName;
        Columns[1].Caption := SWEDISH_ColIDSize;
        Columns[2].Caption := SWEDISH_ColIDType;
        Columns[3].Caption := SWEDISH_ColIDDate;
        Columns[4].Caption := SWEDISH_ColIDAttr;
      end;   //882>
    lnTurkish:
      begin  //886<
        Columns[0].Caption := TURKISH_ColIDName;
        Columns[1].Caption := TURKISH_ColIDSize;
        Columns[2].Caption := TURKISH_ColIDType;
        Columns[3].Caption := TURKISH_ColIDDate;
        Columns[4].Caption := TURKISH_ColIDAttr;
      end;  //>886
  end;  //case
  //Width
  Columns[0].Width := FColWidth_Name;
  Columns[1].Width := FColWidth_Size;
  Columns[2].Width := FColWidth_Type;
  Columns[3].Width := FColWidth_Mod;
  Columns[4].Width := FColWidth_Attr;
  //Alignment
  Columns[0].Alignment := taLeftJustify;
  Columns[1].Alignment := taRightJustify;
  Columns[2].Alignment := taLeftJustify;
  Columns[3].Alignment := taLeftJustify;
  Columns[4].Alignment := taLeftJustify;
  Columns.EndUpdate;
  FileColExists := True;  //872ln<
end; {= CreateFileColumns =}

//< budskman  //883<
procedure TLsFilelistView28.WMNotify(var Message:TWMNotify);
var
  hHeader:HWND;
begin
  inherited;
  hHeader := SendMessage(Handle, LVM_GETHEADER, 0, 0);
  if (hHeader <> 0) and (message.NMHdr^.hWndFrom = hHeader) then
    with message.NMHdr^ do
      if (code = HDN_ENDTRACK) then
        SetColumnBitmap;
end;


procedure TLsFilelistView28.SetColumnBitmap; //70
var
  Hditem: THdItem;
  i: Integer;
begin
  for i := 0 to Columns.Count - 1 do
  begin
    FBitmap.Releasehandle;
    Hditem.Mask := HDI_FORMAT;
    Header_GetItem(GetDlgItem(Self.Handle, 0), i, Hditem);
    Hditem.Mask := HDI_BITMAP or HDI_FORMAT;

    if i = Required_Column then
    begin
      if ThemesEnabled then
      begin
        // XP theme enable
        if FSortForward then
          Hditem.fmt := (Hditem.fmt and $FF) or HDF_STRING or HDF_SORTDOWN
        else
          Hditem.fmt := (Hditem.fmt and $FF) or HDF_STRING or HDF_SORTUP;
        SendMessage(Self.Handle, LVM_SETSELECTEDCOLUMN, MAKEWPARAM(Required_Column, 0), 0);
        Invalidate;
      end
      else begin
        if FSortForward then
          FBitmap.Handle := Bmp_Down28
        else
          FBitmap.Handle := Bmp_Up28;
          Hditem.fmt := Hditem.fmt or HDF_BITMAP or HDF_BITMAP_ON_RIGHT;
      end;
    end
    else begin
      if ThemesEnabled then
      begin
        Hditem.fmt := (Hditem.fmt and $FF) or HDF_STRING
      end
      else begin
        FBitmap.Handle := Bmp_Down28;  //Bmp_Up28;
        Hditem.fmt := Hditem.fmt and not (HDF_BITMAP or HDF_BITMAP_ON_RIGHT);
      end;
    end;
    Hditem.hbm := FBitmap.Handle;
    Header_SetItem(GetDlgItem(Self.Handle, 0), i, hditem);
  end;
end; {= SetColumnBitmap =}
//883 budskman >

procedure TLsFilelistView28.ColumnClick(Sender: TObject; Column: TListColumn);
//882
  Function Compare(Item1, Item2: TFileInfo): Integer;
  var
    TopItem: Boolean;
    S1, S2: string;
  begin
    Result := 0;

    if (Item1.fiFileName = '..') or (Item2.fiFileName = '..') then
      TopItem := True
    else
      TopItem := False;

    if Item1.fiFileName = '..' then
      Result := -1;
    if Item2.fiFileName = '..' then
      Result := 1;

    if Item1.fiType <> Item2.fiType then
    begin
      if Item1.fiType = 'dir' then
        Result := -1
      else
        Result := 1;
    end
    else begin
      Case FSortColumn of
        // sort on file name
        0: Result := AnsiCompareText (Item1.fiFileName, Item2.fiFileName);   //s
        // sort on file size
        1: if Item1.fiSize > Item2.fiSize then
             Result := 1
           else if Item1.fiSize < Item2.fiSize then
             Result := -1
           else
             Result := 0;
        // sort on files' type
        2: begin
             S1 := AnsiLowerCase(Item1.fiSHType);
             S2 := AnsiLowerCase(Item2.fiSHType);
             if S1 = S2 then
               Result := 0
             else if S1 = '' then
               Result := -1
             else if S2 = '' then
               Result := 1
             else
               Result := LStrCmp( PChar(S1), PChar(S2));
           end;
        // sort on files' modified date
        3: if Item1.fiDate > Item2.fiDate then
             Result := 1
           else if Item1.fiDate < Item2.fiDate then
             Result := -1
           else
             Result := 0;
        // sort on ATTR
        4: Result := AnsiCompareText (Item1.fiFileAttr, Item2.fiFileAttr);
      end;  //case
    end;  //else

    // To keep ParentDir at the top regardless sort direction.
    if  FSortForward then
    begin
      if TopItem then
        Result := Result
      else
        Result := -Result;
    end else
      Result := Result;
  end;  {= Compare =}  //882s

//Ales  //882  //884 - fixing irregularities
  procedure QuickSort(L, R: Integer);
  var
    I, J: Integer;
    T, K: TFileInfo;
  begin
    repeat
      I := L;
      J := R;

      K := FFlieInfo[(L + R) shr 1];
      repeat
        while Compare(FFlieInfo[I], K) < 0 do
          Inc(I);
        while Compare(FFlieInfo[J], K) > 0 do
          Dec(J);
        if I <= J then
        begin
          if I < J then
          begin
            T := FFlieInfo[I];
            FFlieInfo[I] := FFlieInfo[J];
            FFlieInfo[J] := T;
          end;
          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then
        QuickSort(L, J);
      L := I;
    until I >= R;
  end;  {= QuickSort =}
//Ales //882s  //884

begin
  if not FColumnClickEnabled then exit;  //70
  required_column := Column.Index;
  if required_column = FSortColumn then
    FSortForward := not FSortForward
  else
  begin
    FSortColumn := required_column;
    FSortForward := True;
  end;
//Ales //882s
  QuickSort(0, Items.Count - 1);
  Items.Count := 0;
  Items.Count := FFlieInfoCount;
//Ales //882s
  SetColumnBitmap; //70
end; {= ColumnClick =}

//881<
procedure TLsFilelistView28.Keydown(var Key: Word; Shift: TShiftState);
var
  ShortKey    : TShortCut;
  i           : integer;  //84
  CurPath     : String;
  DelFilePath : string;
  oldCur      : TCursor;
begin
//  inherited KeyDown(Key, Shift);
  ShortKey := ShortCut(Key, Shift);

  if (not isEditing) then
  begin
    if (Selected <> nil) then
    begin
      if ((ShortKey = FShortCuts.scCopy  ) and (vaCopy   in FShortCuts.Actions)) then CutCopy(0);
      if ((ShortKey = FShortCuts.scCut   ) and (vaCut    in FShortCuts.Actions)) then CutCopy(2);
      if ((ShortKey = FShortCuts.scOpen  ) and (vaOpen   in FShortCuts.Actions)) then OpenItem;
      if ((ShortKey = FShortCuts.scView  ) and (vaView   in FShortCuts.Actions)) then ViewFile;
      if ((ShortKey = FShortCuts.scDelete) and (vaDelete in FShortCuts.Actions)) then DeleteItems;
      if ((ShortKey = FShortCuts.scRename) and (vaRename in FShortCuts.Actions)) then RenameFile;
      if ((ShortKey = FShortCuts.scPaste ) and (vaPaste  in FShortCuts.Actions)) and
         ((FSelectedFiles.Count <> 0) or (Clipboard.HasFormat(CF_HDROP))) then Paste;
      if ((ShortKey = FShortCuts.scNewFolder) and (vaNewFolder in FShortCuts.Actions) and
         (AnsiCompareText(FDirectory, 'My Computer') <> 0)) then NewFolder;  //885R2
    end;  //Selected <> nil

    if ((ShortKey = FShortCuts.scBack    ) and (vaBack    in FShortCuts.Actions)) then
    begin
      if (AnsiCompareText(FDirectory, 'My Computer') = 0 ) then   //885+
        exit;                                                      //885+
      OneLevelUp;
    end;

    if ((ShortKey = FShortCuts.scFindFile) and (vaFindFile in FShortCuts.Actions)) then
      FindFile;

    if ((ShortKey = FShortCuts.scRefresh ) and (vaRefresh  in FShortCuts.Actions)) then
    begin
      UpdateFileList;
      if assigned(FDirTreeCombo) then
      begin
        with FDirTreeCombo do
        begin
          CurPath := '';
          CurPath := Tree.FSelectedPath;
          ReSetTreeview;
          OpenPath(CurPath);
        end;
      end;
      if assigned(FDirTree) then
      begin
        with FDirTree do
        begin
          CurPath := '';
          CurPath := FSelectedPath;
          Reload;
          OpenPath(CurPath);
        end;
      end;
    end;  //Refresh

    if ((ShortKey = FShortCuts.scSelectAll) and (vaSelectAll in FShortCuts.Actions)) then
    begin
      if Items.Count <> 0 then
      begin
        for i := 0 to Items.Count -1 do
          Items[i].Selected := True;
      end;
    end;  //SelectAll

    if (Key = VK_RETURN) and (FOpenByEnter = True) then
      OpenItem;   //881<   //882

    // NOTE:- The following Keyboard Shortcuts can't be changed by Object
    //        Inspector

    // Delete seleted items permanently without placing them in Recycle Bin
    if ((Shift = [ssShift]) and (Key = VK_DELETE)) then  //876
    begin
      oldCur := Screen.Cursor;
      if SelCount = 0 then exit;
      try
        for i := 0 to Items.Count - 1 do
        begin
          if Items[i].Selected then
          begin
            If FFlieInfo[i].fiType = 'dir' then    //882
              exit;
            DelFilePath := '';
            DelFilePath := AddNullToStr(FFlieInfo[I].fiFilePath);
            if MessageDlg('Are you sure you want to permanently delete file' + #13 +
                          '  "' + Selected.Caption + '"?', mtConfirmation,
                          [mbYes, mbNo], 0) = mrYes then
              DeleteFile(PChar(DelFilePath));
            Items[i].Selected := False;
          end;
        end;
      finally
        UpdateFileList;
        Screen.Cursor := oldCur;
      end;
    end;  //SHIFT+DELETE

    // Open DirTreeCombo
    if (Key = VK_F4) then
    begin
      if not assigned(DirTreeCombo) then exit;
      DirTreeCombo.BtnClick(Self);
      if DirTreeCombo.Tree.Visible then
        DirTreeCombo.Tree.SetFocus;
    end;

    // Close DirTreeCombo
    if (Key = VK_ESCAPE) then
    begin
      if not assigned(DirTreeCombo) then exit;
      if (DirTreeCombo.Tree.Enabled = True) then
      begin
        DirTreeCombo.Tree.Enabled := False;
        DirTreeCombo.Tree.Visible := False;
        DirTreeCombo.SendToBack;
      end;
    end;
  end;  //..not isEditing ...

  inherited KeyDown(Key, Shift);
end;  {= KeyDown =}
//881>

procedure TLsFilelistView28.KeyUp(var Key: Word; Shift: TShiftState);  //84
begin
  Inherited KeyUp(Key, Shift);
  if (Key = VK_UP) or (Key = VK_DOWN) then  Click;
end;  {= KeyUp =}

function TLsFileListView28.GetWkgMask(var MaskStr: string): string; //84
var
  SepPos: integer;
begin
  SepPos := Pos(';', MaskStr);
  if SepPos = 0 then
    Result := MaskStr
  else
    Result := Copy(MaskStr, 1, SepPos - 1);
  System.Delete(MaskStr, 1, (Length(Result) + 1));
end;  {= GetWkgMask =}

procedure TLsFilelistView28.Loaded;  //882<
begin
  inherited;
  UpdateFileList;
end;  {= lOADED =}  //882>

procedure TLsFilelistView28.UpdateFileList;
var
  oldCur: TCursor;
//  MaskPtr: PChar;  //84
  TmpMask,
    WkgMask: string;  //84
  AttrIndex: TFileAttr;
//  Ptr: PChar;  //84
  DirAttr,
    FileAttr: DWORD;
  FName,
    CurPath: string;
const
  dwFileAttr: array[TFileAttr] of DWord =
    (FILE_ATTRIBUTE_READONLY, FILE_ATTRIBUTE_HIDDEN,
     FILE_ATTRIBUTE_SYSTEM, FILE_ATTRIBUTE_DIRECTORY,
     FILE_ATTRIBUTE_ARCHIVE, FILE_ATTRIBUTE_NORMAL,
     FILE_ATTRIBUTE_COMPRESSED);    //886
begin
  if csLoading in ComponentState then Exit;
//Ales  //882>
  FFlieInfoCount := 0;
  SetLength (FFlieInfo, 0);
  SetLength (FFlieInfo, 500);
//Ales  //882<
  Items.Clear;
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  FDirectorySize := 0;
  // design state
  if csDesigning in ComponentState then   //885<
  begin
    try
      if AnsiCompareText(FDirectory, 'My Computer') = 0 then   //885r2
        CreateDriveColumns
      else if not FileColExists then
        CreateFileColumns;   //885>
    except
    end;
  end
  else begin
  // runtime state
    try
      // Add Drives to LsFilelistView28
      if AnsiCompareText(FDirectory, 'My Computer') = 0 then   //885r2
      begin
        CreateDriveColumns;
        AddDrives;
        FPopupMenu.AutoPopup := False;  //878
      end
      else begin
        // Add directories to LsFilelistView28
        FPopupMenu.AutoPopup := True;  //878
        if not FileColExists then
          CreateFileColumns;   //872
        FileAttr := 0;
        for AttrIndex := ftReadOnly to ftNormal do
          if AttrIndex in FileType then
            FileAttr := FileAttr or dwFileAttr[AttrIndex];
        DirAttr := FileAttr or FILE_ATTRIBUTE_DIRECTORY;
        CurPath := AddSlash(FDirectory);
        FName := CurPath + '*.*';

        if ShowFolders then
          AddFiles(FName, DirAttr);

        //84
        TmpMask := FMask;
        While (Length(TmpMask) > 0) and (TmpMask[1] = ' ') do
          System.Delete(TmpMask, 1, 1);
        While (Length(TmpMask) > 0) and (TmpMask[Length(TmpMask)] = ' ') do
          System.Delete(TmpMask, Length(TmpMask), 1);
        if Length(TmpMask) =  0 then
          TmpMask := '*.*';

        While Length(TmpMask) > 0 do
        begin
          WkgMask := GetWkgMask(TmpMask);
          AddFiles((CurPath + WkgMask), FileAttr);
        end;
      end; // else  FDirectory ...
    finally
      FSortForward := True;
      if (Items.Count > 0) and (Columns.Count > 0) then  //876  //883
        ColumnClick(Self, Columns[0]);
    end; // try
  end; // if component state
  Screen.Cursor := oldCur;
  if (Items.Count > 0) and (SelCount = 0) then
    ItemFocused := Items[0];
  Application.ProcessMessages;
end; {= UpdateFileList =}

procedure TLsFilelistView28.AddDrives;
var
  sfi: TSHFileInfo;
  i: Integer;
  DiskType: Integer;
  Drv: Char;

  function GetDriveTypeStr(Root: string): string;
  var
    DrvType: Integer;
  begin
    Case LvLangID of
      lnEnglish:
        begin  //872ln<
          DrvTypeStr0 :=  ENGLISH_DrvTypeStr0;   //Unknown
          DrvTypeStr1 :=  ENGLISH_DrvTypeStr1;   //Not exist
          DrvTypeStr2 :=  ENGLISH_DrvTypeStr2;   //Removable Disk
          DrvTypeStr3 :=  ENGLISH_DrvTypeStr3;   //Fixed Disk
          DrvTypeStr4 :=  ENGLISH_DrvTypeStr4;   //Network Disk
          DrvTypeStr5 :=  ENGLISH_DrvTypeStr5;   //CD-Rom Disk
          DrvTypeStr6 :=  ENGLISH_DrvTypeStr6;   //RAM Disk
        end;  //872ln>

      lnChinese_Tra: // scarfman
        begin  //879<
          DrvTypeStr0 :=  CHINESE_Tra_DrvTypeStr0;   //Unknown
          DrvTypeStr1 :=  CHINESE_Tra_DrvTypeStr1;   //Not exist
          DrvTypeStr2 :=  CHINESE_Tra_DrvTypeStr2;   //Removable Disk
          DrvTypeStr3 :=  CHINESE_Tra_DrvTypeStr3;   //Fixed Disk
          DrvTypeStr4 :=  CHINESE_Tra_DrvTypeStr4;   //Network Disk
          DrvTypeStr5 :=  CHINESE_Tra_DrvTypeStr5;   //CD-Rom Disk
          DrvTypeStr6 :=  CHINESE_Tra_DrvTypeStr6;   //RAM Disk
        end;  //879>
      lnChinese_Sim: // scarfman
        begin  //879<
          DrvTypeStr0 :=  CHINESE_Sim_DrvTypeStr0;   //Unknown
          DrvTypeStr1 :=  CHINESE_Sim_DrvTypeStr1;   //Not exist
          DrvTypeStr2 :=  CHINESE_Sim_DrvTypeStr2;   //Removable Disk
          DrvTypeStr3 :=  CHINESE_Sim_DrvTypeStr3;   //Fixed Disk
          DrvTypeStr4 :=  CHINESE_Sim_DrvTypeStr4;   //Network Disk
          DrvTypeStr5 :=  CHINESE_Sim_DrvTypeStr5;   //CD-Rom Disk
          DrvTypeStr6 :=  CHINESE_Sim_DrvTypeStr6;   //RAM Disk
        end;  //879>
      lnDutch:
        begin  //875<
          DrvTypeStr0 :=  DUTCH_DrvTypeStr0;
          DrvTypeStr1 :=  DUTCH_DrvTypeStr1;
          DrvTypeStr2 :=  DUTCH_DrvTypeStr2;
          DrvTypeStr3 :=  DUTCH_DrvTypeStr3;
          DrvTypeStr4 :=  DUTCH_DrvTypeStr4;
          DrvTypeStr5 :=  DUTCH_DrvTypeStr5;
          DrvTypeStr6 :=  DUTCH_DrvTypeStr6;
        end;  //875>
      lnFrench:
        begin  //872<
          DrvTypeStr0 :=  FRENCH_DrvTypeStr0;
          DrvTypeStr1 :=  FRENCH_DrvTypeStr1;
          DrvTypeStr2 :=  FRENCH_DrvTypeStr2;
          DrvTypeStr3 :=  FRENCH_DrvTypeStr3;
          DrvTypeStr4 :=  FRENCH_DrvTypeStr4;
          DrvTypeStr5 :=  FRENCH_DrvTypeStr5;
          DrvTypeStr6 :=  FRENCH_DrvTypeStr6;
        end;  //872>
      lnGerman:
        begin  //872<
          DrvTypeStr0 :=  GERMAN_DrvTypeStr0;
          DrvTypeStr1 :=  GERMAN_DrvTypeStr1;
          DrvTypeStr2 :=  GERMAN_DrvTypeStr2;
          DrvTypeStr3 :=  GERMAN_DrvTypeStr3;
          DrvTypeStr4 :=  GERMAN_DrvTypeStr4;
          DrvTypeStr5 :=  GERMAN_DrvTypeStr5;
          DrvTypeStr6 :=  GERMAN_DrvTypeStr6;
        end;  //872>
      lnItalian:
        begin  //874<
          DrvTypeStr0 :=  ITALIAN_DrvTypeStr0;
          DrvTypeStr1 :=  ITALIAN_DrvTypeStr1;
          DrvTypeStr2 :=  ITALIAN_DrvTypeStr2;
          DrvTypeStr3 :=  ITALIAN_DrvTypeStr3;
          DrvTypeStr4 :=  ITALIAN_DrvTypeStr4;
          DrvTypeStr5 :=  ITALIAN_DrvTypeStr5;
          DrvTypeStr6 :=  ITALIAN_DrvTypeStr6;
        end;  //874>
      lnJapanese:
        begin  //880<
          DrvTypeStr0 :=  JAPANESE_DrvTypeStr0;   //Unknown
          DrvTypeStr1 :=  JAPANESE_DrvTypeStr1;   //Not exist
          DrvTypeStr2 :=  JAPANESE_DrvTypeStr2;   //Removable Disk
          DrvTypeStr3 :=  JAPANESE_DrvTypeStr3;   //Fixed Disk
          DrvTypeStr4 :=  JAPANESE_DrvTypeStr4;   //Network Disk
          DrvTypeStr5 :=  JAPANESE_DrvTypeStr5;   //CD-Rom Disk
          DrvTypeStr6 :=  JAPANESE_DrvTypeStr6;   //RAM Disk
        end;  //880>
      lnKorean:
        begin  //885<
          DrvTypeStr0 :=  KOREAN_DrvTypeStr0;   //Unknown
          DrvTypeStr1 :=  KOREAN_DrvTypeStr1;   //Not exist
          DrvTypeStr2 :=  KOREAN_DrvTypeStr2;   //Removable Disk
          DrvTypeStr3 :=  KOREAN_DrvTypeStr3;   //Fixed Disk
          DrvTypeStr4 :=  KOREAN_DrvTypeStr4;   //Network Disk
          DrvTypeStr5 :=  KOREAN_DrvTypeStr5;   //CD-Rom Disk
          DrvTypeStr6 :=  KOREAN_DrvTypeStr6;   //RAM Disk
        end;  //885>
      lnPolish:
        begin  //876<
          DrvTypeStr0 :=  POLISH_DrvTypeStr0;
          DrvTypeStr1 :=  POLISH_DrvTypeStr1;
          DrvTypeStr2 :=  POLISH_DrvTypeStr2;
          DrvTypeStr3 :=  POLISH_DrvTypeStr3;
          DrvTypeStr4 :=  POLISH_DrvTypeStr4;
          DrvTypeStr5 :=  POLISH_DrvTypeStr5;
          DrvTypeStr6 :=  POLISH_DrvTypeStr6;
        end;  //876>
      lnPortuguese:
        begin  //873<
          DrvTypeStr0 :=  BRAZ_PORT_DrvTypeStr0;
          DrvTypeStr1 :=  BRAZ_PORT_DrvTypeStr1;
          DrvTypeStr2 :=  BRAZ_PORT_DrvTypeStr2;
          DrvTypeStr3 :=  BRAZ_PORT_DrvTypeStr3;
          DrvTypeStr4 :=  BRAZ_PORT_DrvTypeStr4;
          DrvTypeStr5 :=  BRAZ_PORT_DrvTypeStr5;
          DrvTypeStr6 :=  BRAZ_PORT_DrvTypeStr6;
        end;  //873>
      lnSlovak:
        begin  //876<
          DrvTypeStr0 :=  SLOVAK_DrvTypeStr0;
          DrvTypeStr1 :=  SLOVAK_DrvTypeStr1;
          DrvTypeStr2 :=  SLOVAK_DrvTypeStr2;
          DrvTypeStr3 :=  SLOVAK_DrvTypeStr3;
          DrvTypeStr4 :=  SLOVAK_DrvTypeStr4;
          DrvTypeStr5 :=  SLOVAK_DrvTypeStr5;
          DrvTypeStr6 :=  SLOVAK_DrvTypeStr6;
        end;  //876>
      lnSlovenian:  //MPX1
        begin  //881<
          DrvTypeStr0 :=  SLO_DrvTypeStr0;
          DrvTypeStr1 :=  SLO_DrvTypeStr1;
          DrvTypeStr2 :=  SLO_DrvTypeStr2;
          DrvTypeStr3 :=  SLO_DrvTypeStr3;
          DrvTypeStr4 :=  SLO_DrvTypeStr4;
          DrvTypeStr5 :=  SLO_DrvTypeStr5;
          DrvTypeStr6 :=  SLO_DrvTypeStr6;
        end;  //881>
      lnSpanish:
        begin  //872ln<
          DrvTypeStr0 :=  SPANISH_DrvTypeStr0;
          DrvTypeStr1 :=  SPANISH_DrvTypeStr1;
          DrvTypeStr2 :=  SPANISH_DrvTypeStr2;
          DrvTypeStr3 :=  SPANISH_DrvTypeStr3;
          DrvTypeStr4 :=  SPANISH_DrvTypeStr4;
          DrvTypeStr5 :=  SPANISH_DrvTypeStr5;
          DrvTypeStr6 :=  SPANISH_DrvTypeStr6;
        end;  //872ln>
      lnSwedish:
        begin  //882<
          DrvTypeStr0 :=  SWEDISH_DrvTypeStr0;
          DrvTypeStr1 :=  SWEDISH_DrvTypeStr1;
          DrvTypeStr2 :=  SWEDISH_DrvTypeStr2;
          DrvTypeStr3 :=  SWEDISH_DrvTypeStr3;
          DrvTypeStr4 :=  SWEDISH_DrvTypeStr4;
          DrvTypeStr5 :=  SWEDISH_DrvTypeStr5;
          DrvTypeStr6 :=  SWEDISH_DrvTypeStr6;
        end;  //882>
      lnTurkish:
        begin  //886<
          DrvTypeStr0 :=  TURKISH_DrvTypeStr0;
          DrvTypeStr1 :=  TURKISH_DrvTypeStr1;
          DrvTypeStr2 :=  TURKISH_DrvTypeStr2;
          DrvTypeStr3 :=  TURKISH_DrvTypeStr3;
          DrvTypeStr4 :=  TURKISH_DrvTypeStr4;
          DrvTypeStr5 :=  TURKISH_DrvTypeStr5;
          DrvTypeStr6 :=  TURKISH_DrvTypeStr6;
        end;  //>886
    end;  //case
    DrvType := GetDriveType(PChar(Root));
    case DrvType of
      0               : Result := DrvTypeStr0;
      1               : Result := DrvTypeStr1;
      DRIVE_REMOVABLE : Result := DrvTypeStr2;
      DRIVE_FIXED     : Result := DrvTypeStr3;
      DRIVE_REMOTE    : Result := DrvTypeStr4;
      DRIVE_CDROM     : Result := DrvTypeStr5;
      DRIVE_RAMDISK   : Result := DrvTypeStr6;  //872ln
    end;
  end; {= GetDriveTypeStr =}

begin
//  isFileList := False;  //878
  FColumnClickEnabled := False; //70
  Integer(Drives) := GetLogicalDrives;
  for i := 0 to 25 do
    if (i in Drives) then
    begin
      Drv := Char(i + Ord('A'));
      Inc (FFlieInfoCount);
      If Length (FFlieInfo) < FFlieInfoCount Then
        SetLength (FFlieInfo, FFlieInfoCount + 499);
      With FFlieInfo[FFlieInfoCount-1] Do try
        fiType := 'drv';
        fiSHOK := True;
        SHGetFileInfo(PChar(Drv + ':\'), 0, sfi, SizeOf(sfi), SHGFI_SYSICONINDEX or SHGFI_DISPLAYNAME or SHGFI_TYPENAME);
        if SmallImages <> nil then
          fiSHIcon := sfi.Iicon;
        fiSHType := GetDriveTypeStr(Drv + ':\');
        fiFileName := ' (' + Drv + ':)  ' + copy(sfi.szDisplayName, 1, (Pos('(', sfi.szDisplayName) - 1));
        fiFilePath := Drv + ':\';

        DiskType := GetDriveType(PChar(Drv + ':\'));
        if (DiskType <> DRIVE_REMOVABLE) and (DiskType <> DRIVE_CDROM) then begin
          fiSize := GetDiskSize(Drv + ':\');
          fiFree := GetFreeDiskSize(Drv + ':\');
        end else begin
          fiSize := 0;
          fiFree := 0;
        end;
      except
        Dec (FFlieInfoCount);
      end;
    end;
  Items.Count := FFlieInfoCount;
end; {= AddDrives =}

//Ales  //882<
function TLsFilelistView28.OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean;
Var
  sfi: TSHFileInfo;
  FName: String;
  i: Integer;
begin
  Result := FFlieInfoCount > Item.Index;
  If Result Then
    With FFlieInfo[Item.Index] Do
      if fiType = 'drv' then
      Begin
        Item.Caption := fiFileName;
        Item.ImageIndex := fiSHIcon;
        Item.SubItems.Add(fiSHType);
        Item.SubItems.Add(FormatFloat ('#,##0 Mb', fiSize / 1024));
        Item.SubItems.Add(FormatFloat ('#,##0 Mb', fiFree / 1024));
        Item.SubItems.Add(' ');
      end Else
      begin
        FName := fiFileName;

      if not fiSHOK then
      begin
        SHGetFileInfo(PChar(fiFilePath), 0, sfi, SizeOf(sfi),
          SHGFI_TYPENAME or SHGFI_SYSICONINDEX or   //SHGFI_ICON or  //871
          SHGFI_DISPLAYNAME);
        fiSHIcon := sfi.iIcon;
        fiSHType := sfi.szTypeName;
        fiSHOK := True;
      end;

      if (FName = '..') then begin //70
        Item.Caption := FParentDirCaption; //' Parent';  //872
        Item.ImageIndex := ParentImgIdx;   //872
      end else begin
        if FHideFileExt then
        begin
          i := Length(ExtractFileExt(FName));  //870
          if i > 0 then
            Item.Caption := Copy(FName, 1, Length(FName) - i)  //870
          else Item.Caption := FName;
        end else Item.Caption := FName;
        Item.ImageIndex := fiSHIcon;   //862
      end;  //else begin

      if FFileSize = fsKB then   //876>
        Item.SubItems.Add(ConvertSize(fiSize, fiType))  //SubItems[0]-Size
      else if FFileSize = fsByte then
      begin
        if fiType = 'dir' then
          Item.SubItems.Add(#32)   //' ')
        else if fiType = 'file' then
          Item.SubItems.Add(IntToStr(fiSize));
      end;  //876<

      if FName = '..' then begin  //874>
        Item.SubItems.Add(' ');
        Item.SubItems.Add(' ');
        Item.SubItems.Add(' ');
      end else
      begin
        Item.SubItems.Add(fiSHType);       //SubItems[1]-TypeName
        Item.SubItems.Add(fiFileDate);     //SubItems[2]-DateTime
        Item.SubItems.Add(fiFileAttr);     //SubItems[3]-Attributes
      end;  //874
      Item.SubItems.Add(fiFilePath);       //SubItems[4]-Path+FileName
      Item.SubItems.Add(fiType);           //SubItems[5]-dir or file
      Item.SubItems.Add(fiDate);           //SubItems[6]-FileTime  //861
    end;
end;  {= OwnerDataFetch =}
//Ales  //882>

//Ales  //882<
function TLsFilelistView28.AddFiles(FileMask: string; Attr: DWord): Boolean;
Var
  hFindFile: THandle;
  Win32FD: TWin32FindData;
  FName: String;
  FileSize: Int64;

  function AttrStr(Attr: integer): string;
  begin
    Result := '';
    if (FILE_ATTRIBUTE_ARCHIVE and Attr) > 0 then Result := Result + 'A';
    if (FILE_ATTRIBUTE_READONLY and Attr) > 0 then Result := Result + 'R';
    if (FILE_ATTRIBUTE_HIDDEN and Attr) > 0 then Result := Result + 'H';
    if (FILE_ATTRIBUTE_SYSTEM and Attr) > 0 then Result := Result + 'S';
  end;

begin
  Result := False;
  FColumnClickEnabled := True; //70
  hFindFile := FindFirstFile(PChar(FileMask), Win32FD);
  if hFindFile <> INVALID_HANDLE_VALUE then
  try
    repeat
      with Win32FD do
      begin
        if ((Attr and FILE_ATTRIBUTE_DIRECTORY) = (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY)) and
           ((Attr and FILE_ATTRIBUTE_READONLY) >= (dwFileAttributes and FILE_ATTRIBUTE_READONLY)) and
           ((Attr and FILE_ATTRIBUTE_HIDDEN) >= (dwFileAttributes and FILE_ATTRIBUTE_HIDDEN)) and
           ((Attr and FILE_ATTRIBUTE_SYSTEM) >= (dwFileAttributes and FILE_ATTRIBUTE_SYSTEM)) then
        begin
          FName := StrPas(Win32FD.cFilename);
          if FName = '.' then continue; //70
          if not FParentDirEnabled and (FName = '..') then continue;  //70

          FileSize := (nFileSizeHigh * MAXDWORD) + nFileSizeLow;
          FDirectorySize := FDirectorySize + FileSize;

          Inc (FFlieInfoCount);
          if Length (FFlieInfo) < FFlieInfoCount Then
            SetLength (FFlieInfo, FFlieInfoCount + 499);

          with FFlieInfo[FFlieInfoCount-1] do
          begin
            fiFileName := FName;
            fiFilePath := AddSlash(FDirectory) + FName;
            if (dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) > 0 then
              fiType := 'dir'
            else fiType := 'file';
            fiAttr := dwFileAttributes;  //883
            fiFileAttr := AttrStr(dwFileAttributes);  //874
            fiSize := FileSize;
            fiFileDate := FileTimeToDateTimeStr(ftLastWriteTime, FDFormatStr, FTFormatStr);
            fiDate := FloatToStr(Int64(ftLastWriteTime.dwHighDateTime) shl 32 + ftLastWriteTime.dwLowDateTime);
          end;
          Result := True;
        end; // Attr ...
      end; // with Win32FD
    until not FindNextFile(hFindFile, Win32FD);
  finally
    windows.FindClose(hFindFile);
    Items.Count := FFlieInfoCount;
  end; // try
end;  {= AddFiles =}
//Ales  //882>

//883
procedure TLsFileListview28.CustomDrawItem(Sender: TCustomListView; Item: TListItem;
             State: TCustomDrawState; var DefaultDraw: Boolean); //883
var
  Attrs: integer;
begin
  if Item = nil then exit;
  if (ViewStyle = vsReport) or (ViewStyle = vsList) then
  begin
    Attrs := FFlieInfo[Item.Index].fiAttr;
    if FTextColor then
    begin
      if Bool(Attrs and FILE_ATTRIBUTE_ARCHIVE) then
        Canvas.Font.Color :=  FItemColors.FAttr_Archive;   //883cl
      if Bool(Attrs and FILE_ATTRIBUTE_HIDDEN) then
        Canvas.Font.Color := FItemColors.FAttr_Hidden;     //883cl
      if Bool(Attrs and FILE_ATTRIBUTE_READONLY) then
        Canvas.Font.Color :=  FItemColors.FAttr_Readonly;  //883cl
      if Bool(Attrs and FILE_ATTRIBUTE_SYSTEM) then
        Canvas.Font.Color := FItemColors.FAttr_System;     //883cl
    end else
      Canvas.Font.Color := GetSysColor(COLOR_WINDOWTEXT);  //clDefault;
  end;
end;  {= CustomDrawItem =}
//883


procedure TLsFilelistView28.OneLevelUp;
var
  NewDir: string;
  OldDir: string;  //873
  i: integer;      //873
  j: integer;      //885
begin
  OldDir := AddSlash(FDirectory);  //873
  if AnsiCompareText(FDirectory, 'My Computer') = 0 then   //885r2
    exit;
  if (FDirTreeCombo <> nil) and (FDirTreeCombo.Tree.Selected.Level = 0) then
    exit;
  if (FDirTree <> nil) and (FDirTree.Selected.Level = 0) then
    exit;
  FDirectory := AddSlash(FDirectory);
  if (FDirectory[Length(FDirectory) - 1] = ':') then
  begin
  //885<
    if (FDirTreeCombo <> nil) then
    begin
      with FDirTreeCombo do
      begin
        if Selected.Expanded then
          Selected.Collapse(True);
        Selected := FDirTreeCombo.Tree.Items[0];
        Text := 'My Computer';     //++
      end;
    end;
    if (FDirTree <> nil) then
    begin
      with FDirTree do
      begin
        if Selected.Expanded then
          Selected.Collapse(True);
        Selected := FDirTree.Items[0];
        SetSelectedPath('My Computer');
      end;
    end;
    SetDirectory('My Computer');      //885R2
  end   //if (FDirectory[Length ..
  else begin
    j := LastPos('\', FDirectory);
    FDirectory := Copy(FDirectory, 1, (j - 1));
    NewDir := ExtractFilePath(FDirectory);
    SetDirectory(NewDir);
//    NewDir :=  DelSlash(NewDir);          //886
    FDirectory := NewDir;
    if FDirTree <> nil then
    begin
      FDirTree.OpenPath(NewDir);
      if FDirTree.Selected.Level = 1 then
        FDirTree.Selected.Collapse(True);
    end;
    if FDirTreeCombo <> nil then
    begin
      FDirTreeCombo.OpenPath(NewDir);
      if FDirTreeCombo.Selected.Level = 1 then
        FDirTreeCombo.Selected.Collapse(True);
    end;
    //>885
    for i := 0 to Items.Count - 1 do       //873<
    begin
      if (AddSlash(Items[i].SubItems[4]) = OldDir) then   //882 ??
      begin
//        Items[i].Selected := True;       //885
        Items[i].Focused := True;          //874
        Click;                             //876
        exit;                              //874
      end;
    end;  //>873
  end;
end; {= OneLevelUp =}

procedure TLsFilelistView28.Click;
begin
  inherited Click;
  if Selected = nil then  //85
    exit;
  FSelectedItem := FFlieInfo[Selected.Index].fiFilePath;  //Ales //882
  if Assigned(FOnItemChange) then  //83
    FOnItemChange(Self, FSelectedItem);  //83
//  inherited Click;
end; {= Click =}

procedure TLsFilelistView28.SetOpenByDblClick(Value: Boolean);  //882
begin
  if Value <> FOpenByDblClick then
    FOpenByDblClick := Value;
end; {= SetOpenByDblClick =}

procedure TLsFilelistView28.DblClick;
begin
  if (Selected = nil) or (Selected.Caption = '') then
    exit;                                              //81
  if FOpenByDblClick then //70   //882
    OpenItem
  else begin
    if (Selected.Caption = FParentDirCaption) then  //' Parent' //872
      OneLevelUp
    else
      inherited DblClick;
  end;
end; {= DblClick =}

procedure TLsFilelistView28.SetPopUpMenuEnabled(Value: Boolean);
begin
  if Value <> FPopUpMenuEnabled then
  begin
    FPopUpMenuEnabled := Value;
    if FPopUpMenuEnabled then
      PopupMenu := FPopupMenu
    else
    begin
      PopupMenu := nil;
      MouseCapture := False; //80
    end;
  end;
end; {= SetPopUpMenuEnabled =}


//================= FileOperations ======================//

function TLsFilelistView28.CanEdit(Item: TListItem): Boolean;
begin
  Result := False;
  if ((not ReadOnly) and (vaRename in FShortCuts.Actions)) then  //881
  begin
    OldFName := FFlieInfo[Selected.Index].fiFilePath;  //882
    Result := inherited CanEdit(Item);
  end;
end; {= CanEdit =}

procedure TLsFilelistView28.Edit(const Item: TLVItem);
var
  Path,
    newFName,
    DirOrFile: string;
  Abort: Boolean;
  OldCur: TCursor;
begin
  if not (vaRename in FShortCuts.Actions) then exit;  //881
  inherited Edit(Item);
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Items.BeginUpdate;
  try
    if Selected <> nil then
    begin
      Path := ExtractFilePath(OldFName);
      DirOrFile := FFlieInfo[Selected.Index].fiType;   //882
      newFName := AddNullToStr(Path + Item.pszText);  //882
      if AnsiCompareText(OldFName, newFName) = 0 then exit;

      if not FileDirExist(newFName) then
        DoSHFileOp(Parent.Handle, FO_RENAME, OldFName, newFName, Abort);

      UpdateFileList;

      if AnsiCompareText(DirOrFile, 'DIR') = 0 then
      begin
        if Assigned(FDirTree) then
        begin
          FDirTree.Reload;
          FDirTree.OpenPath(Path);
        end
        else if Assigned(FDirTreeCombo) then
        begin
          FDirTreeCombo.ResetTreeView;
          FDirTreeCombo.OpenPath(Path);
        end;
      end;
    end;
  finally
    Items.EndUpdate;
    Screen.Cursor := oldCur;
  end;
end; {= Edit =}

procedure TLsFilelistView28.NewFolder;
var
  NewDir: string;
  i: integer;
begin
  //81 >>
  NewDir := SlashSep(FDirectory, esNewFolder);  //872ln AndyRoth
  if DirectoryExists(NewDir) then
  begin
    i := 1;
    Repeat
      inc(i);
    until not DirectoryExists(NewDir + '(' + IntToStr(i) + ')');
    NewDir := NewDir + '(' + IntToStr(i) + ')';
  end;  //81 <<
  CreateDir(NewDir);
  UpdateFileList;
  //81 >>
  Selected := nil;
  For i := 0 to Items.Count - 1 do
  begin
    if Items[i].SubItems[4] = NewDir then
      Selected := Items[i];
  end;
  CanEdit(Selected);
  Selected.EditCaption;
  //81 <<
  if Assigned(DirTreeCombo) then
  begin
    FDirTreeCombo.ResetTreeView;
    FDirTreeCombo.OpenPath(NewDir);
  end
  else if Assigned(FDirTree) then
  begin
    FDirTree.Reload;
    FDirTree.OpenPath(NewDir);
  end;
end; {= NewFolder =}

procedure TLsFilelistView28.RenameFile;
var
  SelItem: TListItem;
begin
  if Selected = nil then exit;
  if ReadOnly or (UpperCase(Selected.SubItems[3]) = 'R') then
  begin
//    MessageDlg('It''s ReadOnly', mtWarning, [mbOK], 0);
    MessageDlg(esReadOnly, mtWarning, [mbOK], 0);  //872ln
    exit;
  end;
  OldFName := AddNullToStr(Selected.SubItems[4]);
  SelItem := Selected;
  CanEdit(SelItem);
  Selected.EditCaption;
end; {= ReNameFile =}

procedure TLsFilelistView28.DeleteItems;
var
  i: integer;
  Abort: Boolean;
  DelFName: string;
  oldCur: TCursor;
begin
  Abort := False;
  oldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if not SetCurrentDir(FDirectory) then exit;
  try
    FSelectedFiles.Clear;
    DelFName := '';
    for i := 0 to Items.Count - 1 do
    begin
      if Items[i].Caption = FParentDirCaption then
        Items[i].Selected := False;   //878
      if Items[i].Selected then
        DelFName := DelFName + AddNullToStr(Items[i].SubItems[4]); //81
    end;
    DoSHFileOp(Parent.Handle, FileOpMode[1], DelFName, '', Abort);

  finally
    UpdateFileList;
    FSelectedFiles.Clear;
    if Assigned(FDirTreeCombo) then
    begin
      FDirTreeCombo.ResetTreeView;
      FDirTreeCombo.OpenPath(FDirectory);
    end
    else if Assigned(FDirTree) then
    begin
      FDirTree.Reload;
      FDirTree.OpenPath(FDirectory);
    end;
  end;
  Screen.Cursor := oldCur;
end; {= DeleteItems =}

//Copy selected files to Clipboard, thus, allowing other application
//to retrieve them by Paste function
procedure TLsFilelistView28.CopyToClipboard(FileList: String);  //878Clipbrd>
var
  DropFiles: PDropFiles;
  hMem: THandle;
  iLen: integer;
begin
  iLen := Length(FileList) + 2;
  FileList := FileList + #0#0;
  hMem := GlobalAlloc(GMEM_SHARE or GMEM_MOVEABLE or GMEM_ZEROINIT,
                     SizeOf(TDropFiles) + iLen);
  if hMem <> 0 then
  begin
    try
      DropFiles := GlobalLock(hMem);
      DropFiles^.pFiles := SizeOf(TDropFiles);
      Move(FileList[1], (PChar(DropFiles) + SizeOf(TDropFiles))^, iLen);
    finally
      GlobalUnLock(hMem);
    end;
    Clipboard.Open;
    try
      Clipboard.SetAsHandle(CF_HDROP, hMem);
    finally
      Clipboard.Close;
    end;
  end;
end;  {= CopyToClipboard =} //878Clipbrd<

//Retrieves filenames from Clipboard
procedure TLsFileListView28.PasteFromClipboard;  //878Clipbrd>
var
  i, numFiles: integer;
  h: THandle;
  buffer: array[0..Max_Path] of Char;
begin
  if not Clipboard.HasFormat(CF_HDROP) then
    exit;
  Clipboard.Open;
  try
    h := Clipboard.GetAsHandle(CF_HDROP);
    IF h <> 0 then
    begin
      numFiles := DragQueryFile(h, $FFFFFFFF, nil, 0);
      FSelectedFiles.Clear;
      for i := 0 to numFiles -1 do
      begin
        DragQueryFile(h, i, buffer, sizeOf(buffer));
        FSelectedFiles.Add(AddNullToStr(buffer));
//        ShowMessage(IntToStr(FSelectedFiles.Count));  debugging
      end;
    end;
  finally
    Clipboard.Close;
  end;
end;  {= PasteFromClipboard =}  //878Clipbrd<

// Copy: Mode = 0, Cut: mode = 2
procedure TLsFilelistView28.CutCopy(Mode: integer);
var
  i: integer;
begin
  FOpMode := -1;
  FSelectedFiles.Clear;
  FSelFileList := '';
  for i := 0 to Items.Count - 1 do
  begin
    if Items[i].Caption = FParentDirCaption then  //878
      Items[i].Selected := False;   //878
    if Items[i].selected then
    begin
      FSelectedFiles.Add(AddNullToStr(Items[i].SubItems[4]));
      FSelFileList := FSelFileList + AddNullToStr(Items[i].SubItems[4]);  //878Clipbrd
    end;
  end;
  FOpMode := Mode;
  if FSelFileList <> '' then
    CopyToClipboard(FSelFileList);  //878Clipbrd
end; {= CutCopy =}

procedure TLsFilelistView28.Paste;
var
  i: integer;
  FSrc,
    FDes,
    DFName: string;
  Abort: Boolean;
  oldCur: TCursor;
begin
  Abort := False;
  oldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if not SetCurrentDir(FDirectory) then
    exit;
  //Check if any files copied to the clipboard by other applications,
  //if so enable procedure PastefromClipboard   //878Clipbrd>
  if FSelectedFiles.Count = 0 then
  begin
    if not Clipboard.HasFormat(CF_HDROP) then
      exit;
    PasteFromClipboard;
  end;
  //878Clipbrd<
  if FSelectedFiles.Count = 0 then
  begin
//    MessageDlg('No file(s) selected', mtWarning, [mbOK], 0);
    MessageDlg(esNoFileSelected, mtWarning, [mbOK], 0);  //872ln
    exit;
  end;
  try
    for i := 0 to FSelectedFiles.Count - 1 do
    begin
      FSrc := '';
      FDes := '';
      FSrc := FSelectedFiles.Strings[i];
      DFName := ExtractFileName(FSrc);
      FDes := SlashSep(FDirectory, DFName);
      FDes := AddNullToStr(FDes);
      DoSHFileOp(Parent.Handle, FileOpMode[FOpMode], FSrc, FDes, Abort);
    end;
  finally
    UpdateFileList;
    FSelectedFiles.Clear;
    //81 >>
    if Assigned(FDirTreeCombo) then
    begin
      FDirTreeCombo.ResetTreeView;
      FDirTreeCombo.OpenPath(FDirectory);
    end
    else if Assigned(FDirTree) then
    begin
      FDirTree.Reload;
      FDirTree.OpenPath(FDirectory);
    end;
    //81 <<
  end;
  Screen.Cursor := oldCur;
end; {= Paste =}

procedure TLsFilelistView28.OpenItem;
var
  sFile,
    sDir: string;
  cDrv: Char;
  oldCur: TCursor;
begin
  if Selected = nil then exit;

  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  if (Selected.Caption = FParentDirCaption) then   //' Parent') then //872
  begin
    OneLevelUp;
    Screen.Cursor := oldCur;
    exit;
  end;

  if (FFlieInfo[Selected.Index].fiType = 'dir') or (FFlieInfo[Selected.Index].fiType = 'drv') then
  begin
    sDir := FFlieInfo[Selected.Index].fiFilePath;
    sDir := AddSlash(sDir);
    if FFlieInfo[Selected.Index].fiType = 'drv' then
      if GetDriveType(PChar(sDir)) = DRIVE_REMOVABLE then
      begin
        cDrv := sDir[1];
        if not DiskInDrive(cDrv, 1) then
        begin
          Screen.Cursor := OldCur;
          exit;
        end;
      end;
    SetDirectory(sDir);
    if FDirTree <> nil then
      FDirTree.OpenPath(sDir);
    if FDirTreeCombo <> nil then
      FDirTreeCombo.OpenPath(sDir);
  end
  else if (FFlieInfo[Selected.Index].fiType = 'file') then
  begin
    sFile := ExtractFileName(FFlieInfo[Selected.Index].fiFilePath);
    sDir := ExtractFilePath(FFlieInfo[Selected.Index].fiFilePath);
    ExecuteFile('Open', sFile, '', sDir, SW_SHOW);
  end;
  Screen.Cursor := OldCur;
end; {= OpenItem =}

procedure TLsFilelistView28.ViewFile;
var
  i: integer;
  sExt,
    sFName,
    sPath,
    WinDir,
    PicViewer : string;
begin
  if isWindowsXP then   //881>
    PicViewer := 'SYSTEM32\MSPaint.exe'
  else
    PicViewer := 'PBrush.exe';  //881<
  if Selected <> nil then
  begin
    sPath := AddSlash(ExtractFilePath(Selected.SubItems[4]));
    sFName := ExtractFileName(Selected.SubItems[4]);
    sExt := UpperCase(ExtractFileExt(Selected.SubItems[4]));
    WinDir := GetSystemPath(spWinRoot);  //874
    for i := Low(FileExt) to High(FileExt) do
      if (i <= 12) and (sExt = FileExt[i]) then
        ExecuteFile('Open', WinDir + 'NotePad.exe', sFName, sPath, SW_SHOW)
      else if (i > 12) and (i <= 15) and (sExt = FileExt[i]) then
        ExecuteFile('Open', WinDir + 'Write.exe', sFName, sPath, SW_SHOW)
      else if (i > 15) and (i <=22) and (sExt = FileExt[i]) then
        ExecuteFile('Open', WinDir + PicViewer, sFName, sPath, SW_SHOW)
      else if (i > 22) and (i <=24) and (sExt = FileExt[i]) then
        ExecuteFile('Open', WinDir + 'Explorer.exe', sFName, sPath, SW_SHOW);
          //85  //881
  end;
end; {= ViewFile =}

procedure TLsFilelistView28.FindFile;
begin
  if Focused then
    ExecuteFile('Find', Directory, '', '', SW_SHOWNORMAL);
end; {= FindFile =}

procedure TLsFilelistView28.SendTo(SubItems: integer);
var
  ADirPath,
    SelPath: string;
  CopyBrd: TRichEdit;
  FNMemo: TMemo;
  AImage: TImage;
  FPath,
    FExt: string;
  i: integer;
  oldCur: TCursor;

  function InputDlg(AValue: string): string;
  var
    TDlgForm: TForm;
    Prompt: TLabel;
    Edit: TEdit;
    BtnOk: TBitBtn;
    BtnCancel: TBitBtn;
    BtnBrowse: TBitBtn;
    ButtonTop: Integer;
    ButtonWidth: Integer;
    ButtonHeight: Integer;
    mrBrowse: TModalResult;
  begin
    Result := '';
    mrBrowse := mrNo + 3;
    TDlgForm := TForm.Create(Application);
    with TDlgForm do
    try
      Canvas.Font.Name := 'MS Sans Serif';
      Canvas.Font.Size := 8;
      BorderStyle := bsDialog;
      Caption := esSendToFolder; //872ln AndyRoth
      ClientWidth := 333;
      ClientHeight := 135;
      Position := poScreenCenter;
      Prompt := TLabel.Create(TDlgForm);
      with Prompt do
      begin
        Parent := TDlgForm;
        AutoSize := True;
        Left := 20;
        Top := 12;
        Caption := esSendToPath; //872ln AndyRoth
      end;
      Edit := TEdit.Create(TDlgForm);
      with Edit do
      begin
        Parent := TDlgForm;
        Left := Prompt.Left;
        Top := 32;
        Width := 293;
        MaxLength := 285;
        Text := AValue;
        SelectAll;
      end;
      ButtonTop := 80;
      ButtonWidth := 75;
      ButtonHeight := 25;
      BtnOK := TBitBtn.Create(TDlgForm);
      with BtnOK do
      begin
        Parent := TDlgForm;
        Kind := bkOK;
        Caption := 'OK';
        ModalResult := mrOk;
        Default := True;
        Cancel := True;
        SetBounds(37, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      BtnCancel := TBitBtn.Create(TDlgForm);
      with BtnCancel do
      begin
        Parent := TDlgForm;
        Kind := bkCancel;
        Caption := ewCancel; //872ln AndyRoth
        ModalResult := mrCancel;
        Cancel := True;
        SetBounds(129, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      BtnBrowse := TBitBtn.Create(TDlgForm);
      with BtnBrowse do
      begin
        Parent := TDlgForm;
        Kind := bkHelp;
        Caption := ewBrowse; //872ln AndyRoth
        Cancel := False;
        ModalResult := mrBrowse;
        SetBounds(221, ButtonTop, ButtonWidth, ButtonHeight);
      end;
      ShowModal;
      if ModalResult = mrBrowse then
      begin
        if BrowseForFolder(Application.Handle, ADirPath, 1, False) then   //880
          SelPath := InputDlg(ADirPath);
      end;
      if ModalResult = mrOK then
      begin
        AValue := Edit.Text;
        if AValue <> '' then
          Result := AValue;
        TDlgForm.Close;
        exit;
      end;
      if ModalResult = mrCancel then
      begin
        Result := '';
      end;
    finally
      TDlgForm.Free;
    end;
  end; {InputDlg}

begin
  AImage := nil;
  case SubItems of
    0:
      begin
        ADirPath := '';
        InputDlg(ADirPath);
        if SelPath <> '' then
          SendToPath(SelPath);
      end;
    1:
      begin
//        FPath := Selected.SubItems[4];                //882
        FPath := FFlieInfo[Selected.Index].fiFilePath;  //882
        FExt := UpperCase(ExtractFileExt(FPath));
        if (FExt = '.BMP') or (FExt = '.WMF') then
        begin
          try
            AImage := TImage.Create(Self);
            AImage.Parent := Self;
            AImage.Picture.LoadFromFile(FPath);
            ClipBoard.Assign(AImage.Picture);    //85
          finally
            AImage.Free;
          end;
        end
        else
        begin
          OldCur := Screen.Cursor;
          Screen.Cursor := crHourGlass;
          CopyBrd := TRichEdit.Create(Self);
          with CopyBrd do
          begin
            Parent := Self;
            PlainText := False;
            Visible := False;
          end;
          if Selected <> nil then
          try
            CopyBrd.Lines.LoadFromFile(FPath);
            CopyBrd.SelectAll;
            CopyBrd.CopyToClipboard;
          finally
            CopyBrd.Free;
            Screen.Cursor := OldCur;
          end;
        end;
      end;
    2:
      begin
        FNMemo := TMemo.Create(Self);
        FNMemo.Parent := Self;
        FNMemo.Lines.Clear;
        try
          for i := 0 to Items.Count - 1 do
          begin
            if Items[i].Selected then
//              FNMemo.Lines.Add(Selected.SubItems[4] + #0);   //882
              FNMemo.Lines.Add(FFlieInfo[Selected.Index].fiFilePath + #0);  //882
          end;
          FNMemo.SelectAll;
          FNMemo.CopyToClipboard;
        finally
          FNMemo.Free;
        end;
      end;
    3:
      begin
        if Selected <> nil then
          CreateShortCut;  //85
      end;
  end;
end; {= SendTo =}


procedure TLsFilelistView28.SendTo2(Path: string);
var
  FName: string;
  PName: string;
  DPath: string;
  Drv: string;

 // Resolving Shortcuts
  function GetShellLinkPath(Handle: THandle; LinkFileName: string):
      string;
  var
    pShlLnk: IShellLink;
    pszPath: array[0..MAX_PATH - 1] of Char;
    win32FD: TWin32FindData;
    ppF: IPersistFile;
    hRes: hResult;
//  {$IFNDEF D3_OR_HIGHER}   //82
//    pSource: array[0..MAX_PATH - 1] of wideChar;  //82
//  {$ELSE}   //82
    IUnk: IUnknown;
    pSource: WideString;
//  {$ENDIF}  //82
  begin
    Result := '';

//{$IFNDEF D3_OR_HIGHER}  //82 <<
//    CoInitialize(nil);
//    if CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
//      IID_IShellLink, pShlLnk) = S_OK then
//      if pShlLnk.QueryInterface(IID_IPersistFile, ppF) = S_OK then
//      try
//        MultiByteToWideChar(CP_ACP, 0, @LinkFileName[1], -1, pSource,
//          MAX_PATH);
//        //  ShowMessage(pSource);    // for debug
//        if ppF.Load(pSource, STGM_READ) = S_OK then
//        begin
//          hRes := pShlLnk.GetPath(pszPath, MAX_PATH, win32FD, SLGP_SHORTPATH
//            );
//          if hRes = S_OK then
//            Result := StrPas(pszPath)
//          else
//            exit;
//        end;
//      finally
//        ppF.Release;
//        CoUnInitialize;
//      end;
//{$ELSE}  //82 >>

    IUnk := CreateComObject(CLSID_ShellLink);
    pShlLnk := IUnk as IShellLink;
    ppF := IUnk as IPersistFile;
    pSource := LinkFileName;

    hRes := ppF.Load(pWideChar(pSource), STGM_READ);
    if Succeeded(hRes) then
    begin
      hRes := pShlLnk.Resolve(Application.Handle, SLR_ANY_MATCH);
      if Succeeded(hRes) then
      begin
        hRes := pShlLnk.GetPath(@pszPath, MAX_PATH, Win32FD, 0);
        if Succeeded(hRes) then
          Result := string(pChar(@pszPath));
      end;
    end;
//{$ENDIF}  //82
  end; {= GetShellLinkPath =}

begin
  if Selected = nil then exit;
  if UpperCase(ExtractFileExt(Path)) = '.LNK' then
  begin
    FName := GetShellLinkPath(Handle, Path);
    PName := ExtractFileName(Selected.SubItems[4]);
    DPath := ExtractFilePath(Selected.SubItems[4]);
    // ShowMessage(FName); // for debug
    if FName = '' then exit;
    Drv := Copy(FName, 1, 3);
    if (GetDriveType(PChar(Drv)) = DRIVE_REMOVABLE) then
      SendToDrive(FName)
    else
    begin
      if DirectoryExists(FName) then
        SendToPath(FName)
      else
        ExecuteFile('Open', FName, PName, DPath, SW_SHOW);
    end;
  end
  else if DirectoryExists(Path) then
    SendToPath(Path);
end; {= SendTo2 =}

procedure TLsFilelistView28.SendToPath(DestPath: string);
var
  i: integer;
  FSrc: string;
  FDes: string;
  DFName: string;
  Abort: Boolean;
  OldCur: TCursor;
begin
  if not SetCurrentDir(FDirectory) then exit;
  Abort := False;
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  FSelectedFiles.Clear;
  try
    for i := 0 to Items.Count - 1 do
      if Items[i].Selected then
        FSelectedFiles.Add(Items[i].SubItems[4] + #0);
    for i := 0 to FSelectedFiles.Count - 1 do
    begin
      FSrc := '';
      FDes := '';
      FSrc := FSelectedFiles.Strings[i];
      DFName := ExtractFileName(FSrc);
      FDes := DestPath + DFName + #0;
      DoSHFileOp(Parent.Handle, FileOpMode[0], FSrc, FDes, Abort);
    end;
  finally
    FSelectedFiles.Clear;
    Screen.Cursor := OldCur;
  end;
end; {= SendToPath =}

procedure TLsFilelistView28.SendToDrive(DriveID: string);
var
  Drv: Char;
  OldCur: TCursor;
begin
  OldCur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Drv := DriveID[1];
  if not DiskInDrive(Drv, 1) then
  begin
    Screen.Cursor := oldCur;
    exit
  end
  else
    SendToPath(DriveID);
  Screen.Cursor := oldCur;
end; {= SendToDrive =}

procedure TLsFileListView28.CreateShortCut;  //85 <<
var
//  sPath: array[0..Max_Path] of Char;
  SrcFile,
    SrcName,
    DesPath: string;
  WidePath: WideString;
  IShLink: IShellLink;
  IPFile: IPersistFile;
  IUnk: IUnknown;
  hRes: hResult;
begin
//  GetWindowsDirectory(sPath, Max_Path);  //874
//  DesPath := AddSlash(StrPas(sPath)) + 'DESKTOP\';  //874
  DesPath := GetSystemPath(spDeskTop);  //874

  if Selected = nil then
  begin
//    MessageDlg('No file selected', mtError, [mbOK], 0);
    MessageDlg(esNoFileSelected, mtError, [mbOK], 0);  //872ln
    exit;
  end;

  SrcFile := Selected.SubItems[4];
  SrcName := ExtractFileName(SrcFile);
  SrcName := ExtractFileNameOnly(SrcName);

  IUnk := CreateComObject(CLSID_ShellLink);
  IShLink := IUnk as IShellLink;
  IPFile := IUnk as IPersistFile;

  with IShLink do
  begin
    hRes := SetPath(PChar(SrcFile));
    if Succeeded(hRes) then
      hRes := SetWorkingDirectory(PChar(ExtractFilePath(SrcFile)));
    if Succeeded(hRes) then
      SetDescription(PChar(SrcName))
  end;

  WidePath :=  AddSlash(DesPath) + SrcName + '.lnk';
  if Succeeded(hRes) then
    hRes := IPFile.Save(PWChar(WidePath), False);
  if Failed(hRes) then
//    MessageDlg('Error - PersistFile.Save failed', mtError, [mbOK], 0);
    MessageDlg(esPersistSaveError, mtError, [mbOK], 0);  //872ln
end; {= CreateShortCut =}  //85 >>

procedure TLsFilelistView28.ShowFileProperties;    //861<< //862<<
var
  Filename: TFileName;
begin
  if Selected = nil then
    exit;
  Filename := Selected.SubItems[4];
  if FileName = '' then exit;
  ShowProperties(FileName);
end;  {= ShowFileProperties =}  //861 >>

procedure TLsFilelistView28.SetAbout(Value: string);  //85 >
Begin
  // Property About is ReadOnly.
end; {= SetAbout =}  //85 <

procedure TLsFilelistView28.InitializeVar;  //872ln>
begin
  Case LvLangID of
    lnEnglish:
      begin  //872<
        ewError            :=  ENGLISH_ewError;
        ewFile             :=  ENGLISH_ewFile;
        ewCancel           :=  ENGLISH_ewCancel;
        ewBrowse           :=  ENGLISH_ewBrowse;
        ewReadOnly         :=  ENGLISH_ewReadOnly;
        ewArchive          :=  ENGLISH_ewArchive;
        ewHidden           :=  ENGLISH_ewHidden;
        ewSystem           :=  ENGLISH_ewSystem;

        esCannot           :=  ENGLISH_esCannot;
        esSpecifyDir       :=  ENGLISH_esSpecifyDir;
        esInvalidDrvID     :=  ENGLISH_esInvalidDrvID;
        esDrvNotReady      :=  ENGLISH_esDrvNotReady;
        esExists           :=  ENGLISH_esExists;
        esNewFolder        :=  ENGLISH_esNewFolder;
        esReadOnly         :=  ENGLISH_esReadOnly;
        esNoFileSelected   :=  ENGLISH_esNoFileSelected;
        esSendToFolder     :=  ENGLISH_esSendToFolder;
        esSendToPath       :=  ENGLISH_esSendToPath;
        esPersistSaveError :=  ENGLISH_esPersistSaveError;
        esSetAttr          :=  ENGLISH_esSetAttr;
      end;  //872>

    lnChinese_Tra: // scarfman
      begin  //879<
        ewError            :=  CHINESE_Tra_ewError;
        ewFile             :=  CHINESE_Tra_ewFile;
        ewCancel           :=  CHINESE_Tra_ewCancel;
        ewBrowse           :=  CHINESE_Tra_ewBrowse;
        ewReadOnly         :=  CHINESE_Tra_ewReadOnly;
        ewArchive          :=  CHINESE_Tra_ewArchive;
        ewHidden           :=  CHINESE_Tra_ewHidden;
        ewSystem           :=  CHINESE_Tra_ewSystem;

        esCannot           :=  CHINESE_Tra_esCannot;
        esSpecifyDir       :=  CHINESE_Tra_esSpecifyDir;
        esInvalidDrvID     :=  CHINESE_Tra_esInvalidDrvID;
        esDrvNotReady      :=  CHINESE_Tra_esDrvNotReady;
        esExists           :=  CHINESE_Tra_esExists;
        esNewFolder        :=  CHINESE_Tra_esNewFolder;
        esReadOnly         :=  CHINESE_Tra_esReadOnly;
        esNoFileSelected   :=  CHINESE_Tra_esNoFileSelected;
        esSendToFolder     :=  CHINESE_Tra_esSendToFolder;
        esSendToPath       :=  CHINESE_Tra_esSendToPath;
        esPersistSaveError :=  CHINESE_Tra_esPersistSaveError;
        esSetAttr          :=  CHINESE_Tra_esSetAttr;
      end;
    lnChinese_Sim: // scarfman
      begin
        ewError            :=  CHINESE_Sim_ewError;
        ewFile             :=  CHINESE_Sim_ewFile;
        ewCancel           :=  CHINESE_Sim_ewCancel;
        ewBrowse           :=  CHINESE_Sim_ewBrowse;
        ewReadOnly         :=  CHINESE_Sim_ewReadOnly;
        ewArchive          :=  CHINESE_Sim_ewArchive;
        ewHidden           :=  CHINESE_Sim_ewHidden;
        ewSystem           :=  CHINESE_Sim_ewSystem;

        esCannot           :=  CHINESE_Sim_esCannot;
        esSpecifyDir       :=  CHINESE_Sim_esSpecifyDir;
        esInvalidDrvID     :=  CHINESE_Sim_esInvalidDrvID;
        esDrvNotReady      :=  CHINESE_Sim_esDrvNotReady;
        esExists           :=  CHINESE_Sim_esExists;
        esNewFolder        :=  CHINESE_Sim_esNewFolder;
        esReadOnly         :=  CHINESE_Sim_esReadOnly;
        esNoFileSelected   :=  CHINESE_Sim_esNoFileSelected;
        esSendToFolder     :=  CHINESE_Sim_esSendToFolder;
        esSendToPath       :=  CHINESE_Sim_esSendToPath;
        esPersistSaveError :=  CHINESE_Sim_esPersistSaveError;
        esSetAttr          :=  CHINESE_Sim_esSetAttr;
      end;  //879>
    lnDutch:
      begin  //875<
        ewError            :=  DUTCH_ewError;
        ewFile             :=  DUTCH_ewFile;
        ewCancel           :=  DUTCH_ewCancel;
        ewBrowse           :=  DUTCH_ewBrowse;
        ewReadOnly         :=  DUTCH_ewReadOnly;
        ewArchive          :=  DUTCH_ewArchive;
        ewHidden           :=  DUTCH_ewHidden;
        ewSystem           :=  DUTCH_ewSystem;

        esCannot           :=  DUTCH_esCannot;
        esSpecifyDir       :=  DUTCH_esSpecifyDir;
        esInvalidDrvID     :=  DUTCH_esInvalidDrvID;
        esDrvNotReady      :=  DUTCH_esDrvNotReady;
        esExists           :=  DUTCH_esExists;
        esNewFolder        :=  DUTCH_esNewFolder;
        esReadOnly         :=  DUTCH_esReadOnly;
        esNoFileSelected   :=  DUTCH_esNoFileSelected;
        esSendToFolder     :=  DUTCH_esSendToFolder;
        esSendToPath       :=  DUTCH_esSendToPath;
        esPersistSaveError :=  DUTCH_esPersistSaveError;
        esSetAttr          :=  DUTCH_esSetAttr;
      end;  //875>
    lnFrench:
      begin  //872<
        ewError            :=  FRENCH_ewError;
        ewFile             :=  FRENCH_ewFile;
        ewCancel           :=  FRENCH_ewCancel;
        ewBrowse           :=  FRENCH_ewBrowse;
        ewReadOnly         :=  FRENCH_ewReadOnly;
        ewArchive          :=  FRENCH_ewArchive;
        ewHidden           :=  FRENCH_ewHidden;
        ewSystem           :=  FRENCH_ewSystem;

        esCannot           :=  FRENCH_esCannot;
        esSpecifyDir       :=  FRENCH_esSpecifyDir;
        esInvalidDrvID     :=  FRENCH_esInvalidDrvID;
        esDrvNotReady      :=  FRENCH_esDrvNotReady;
        esExists           :=  FRENCH_esExists;
        esNewFolder        :=  FRENCH_esNewFolder;
        esReadOnly         :=  FRENCH_esReadOnly;
        esNoFileSelected   :=  FRENCH_esNoFileSelected;
        esSendToFolder     :=  FRENCH_esSendToFolder;
        esSendToPath       :=  FRENCH_esSendToPath;
        esPersistSaveError :=  FRENCH_esPersistSaveError;
        esSetAttr          :=  FRENCH_esSetAttr;
      end;
    lnGerman:
      begin
        ewError            :=  GERMAN_ewError;
        ewFile             :=  GERMAN_ewFile;
        ewCancel           :=  GERMAN_ewCancel;
        ewBrowse           :=  GERMAN_ewBrowse;
        ewReadOnly         :=  GERMAN_ewReadOnly;
        ewArchive          :=  GERMAN_ewArchive;
        ewHidden           :=  GERMAN_ewHidden;
        ewSystem           :=  GERMAN_ewSystem;

        esCannot           :=  GERMAN_esCannot;
        esSpecifyDir       :=  GERMAN_esSpecifyDir;
        esInvalidDrvID     :=  GERMAN_esInvalidDrvID;
        esDrvNotReady      :=  GERMAN_esDrvNotReady;
        esExists           :=  GERMAN_esExists;
        esNewFolder        :=  GERMAN_esNewFolder;
        esReadOnly         :=  GERMAN_esReadOnly;
        esNoFileSelected   :=  GERMAN_esNoFileSelected;
        esSendToFolder     :=  GERMAN_esSendToFolder;
        esSendToPath       :=  GERMAN_esSendToPath;
        esPersistSaveError :=  GERMAN_esPersistSaveError;
        esSetAttr          :=  GERMAN_esSetAttr;
      end;  //872>
    lnItalian:
      begin  //874<
        ewError            :=  ITALIAN_ewError;
        ewFile             :=  ITALIAN_ewFile;
        ewCancel           :=  ITALIAN_ewCancel;
        ewBrowse           :=  ITALIAN_ewBrowse;
        ewReadOnly         :=  ITALIAN_ewReadOnly;
        ewArchive          :=  ITALIAN_ewArchive;
        ewHidden           :=  ITALIAN_ewHidden;
        ewSystem           :=  ITALIAN_ewSystem;

        esCannot           :=  ITALIAN_esCannot;
        esSpecifyDir       :=  ITALIAN_esSpecifyDir;
        esInvalidDrvID     :=  ITALIAN_esInvalidDrvID;
        esDrvNotReady      :=  ITALIAN_esDrvNotReady;
        esExists           :=  ITALIAN_esExists;
        esNewFolder        :=  ITALIAN_esNewFolder;
        esReadOnly         :=  ITALIAN_esReadOnly;
        esNoFileSelected   :=  ITALIAN_esNoFileSelected;
        esSendToFolder     :=  ITALIAN_esSendToFolder;
        esSendToPath       :=  ITALIAN_esSendToPath;
        esPersistSaveError :=  ITALIAN_esPersistSaveError;
        esSetAttr          :=  ITALIAN_esSetAttr;
      end;  //874>
    lnJapanese:
      begin  //880<
        ewError            :=  JAPANESE_ewError;
        ewFile             :=  JAPANESE_ewFile;
        ewCancel           :=  JAPANESE_ewCancel;
        ewBrowse           :=  JAPANESE_ewBrowse;
        ewReadOnly         :=  JAPANESE_ewReadOnly;
        ewArchive          :=  JAPANESE_ewArchive;
        ewHidden           :=  JAPANESE_ewHidden;
        ewSystem           :=  JAPANESE_ewSystem;

        esCannot           :=  JAPANESE_esCannot;
        esSpecifyDir       :=  JAPANESE_esSpecifyDir;
        esInvalidDrvID     :=  JAPANESE_esInvalidDrvID;
        esDrvNotReady      :=  JAPANESE_esDrvNotReady;
        esExists           :=  JAPANESE_esExists;
        esNewFolder        :=  JAPANESE_esNewFolder;
        esReadOnly         :=  JAPANESE_esReadOnly;
        esNoFileSelected   :=  JAPANESE_esNoFileSelected;
        esSendToFolder     :=  JAPANESE_esSendToFolder;
        esSendToPath       :=  JAPANESE_esSendToPath;
        esPersistSaveError :=  JAPANESE_esPersistSaveError;
        esSetAttr          :=  JAPANESE_esSetAttr;
      end;  //880>
    lnKorean:
      begin  //885<
        ewError            :=  KOREAN_ewError;
        ewFile             :=  KOREAN_ewFile;
        ewCancel           :=  KOREAN_ewCancel;
        ewBrowse           :=  KOREAN_ewBrowse;
        ewReadOnly         :=  KOREAN_ewReadOnly;
        ewArchive          :=  KOREAN_ewArchive;
        ewHidden           :=  KOREAN_ewHidden;
        ewSystem           :=  KOREAN_ewSystem;

        esCannot           :=  KOREAN_esCannot;
        esSpecifyDir       :=  KOREAN_esSpecifyDir;
        esInvalidDrvID     :=  KOREAN_esInvalidDrvID;
        esDrvNotReady      :=  KOREAN_esDrvNotReady;
        esExists           :=  KOREAN_esExists;
        esNewFolder        :=  KOREAN_esNewFolder;
        esReadOnly         :=  KOREAN_esReadOnly;
        esNoFileSelected   :=  KOREAN_esNoFileSelected;
        esSendToFolder     :=  KOREAN_esSendToFolder;
        esSendToPath       :=  KOREAN_esSendToPath;
        esPersistSaveError :=  KOREAN_esPersistSaveError;
        esSetAttr          :=  KOREAN_esSetAttr;
      end;  //885>
    lnPolish:
      begin  //876<
        ewError            :=  POLISH_ewError;
        ewFile             :=  POLISH_ewFile;
        ewCancel           :=  POLISH_ewCancel;
        ewBrowse           :=  POLISH_ewBrowse;
        ewReadOnly         :=  POLISH_ewReadOnly;
        ewArchive          :=  POLISH_ewArchive;
        ewHidden           :=  POLISH_ewHidden;
        ewSystem           :=  POLISH_ewSystem;

        esCannot           :=  POLISH_esCannot;
        esSpecifyDir       :=  POLISH_esSpecifyDir;
        esInvalidDrvID     :=  POLISH_esInvalidDrvID;
        esDrvNotReady      :=  POLISH_esDrvNotReady;
        esExists           :=  POLISH_esExists;
        esNewFolder        :=  POLISH_esNewFolder;
        esReadOnly         :=  POLISH_esReadOnly;
        esNoFileSelected   :=  POLISH_esNoFileSelected;
        esSendToFolder     :=  POLISH_esSendToFolder;
        esSendToPath       :=  POLISH_esSendToPath;
        esPersistSaveError :=  POLISH_esPersistSaveError;
        esSetAttr          :=  POLISH_esSetAttr;
      end;  //876>
    lnPortuguese:
      begin  //873<
        ewError            :=  BRAZ_PORT_ewError;
        ewFile             :=  BRAZ_PORT_ewFile;
        ewCancel           :=  BRAZ_PORT_ewCancel;
        ewBrowse           :=  BRAZ_PORT_ewBrowse;
        ewReadOnly         :=  BRAZ_PORT_ewReadOnly;
        ewArchive          :=  BRAZ_PORT_ewArchive;
        ewHidden           :=  BRAZ_PORT_ewHidden;
        ewSystem           :=  BRAZ_PORT_ewSystem;

        esCannot           :=  BRAZ_PORT_esCannot;
        esSpecifyDir       :=  BRAZ_PORT_esSpecifyDir;
        esInvalidDrvID     :=  BRAZ_PORT_esInvalidDrvID;
        esDrvNotReady      :=  BRAZ_PORT_esDrvNotReady;
        esExists           :=  BRAZ_PORT_esExists;
        esNewFolder        :=  BRAZ_PORT_esNewFolder;
        esReadOnly         :=  BRAZ_PORT_esReadOnly;
        esNoFileSelected   :=  BRAZ_PORT_esNoFileSelected;
        esSendToFolder     :=  BRAZ_PORT_esSendToFolder;
        esSendToPath       :=  BRAZ_PORT_esSendToPath;
        esPersistSaveError :=  BRAZ_PORT_esPersistSaveError;
        esSetAttr          :=  BRAZ_PORT_esSetAttr;
      end;  //873>
    lnSlovak:
      begin  //876<
        ewError            :=  SLOVAK_ewError;
        ewFile             :=  SLOVAK_ewFile;
        ewCancel           :=  SLOVAK_ewCancel;
        ewBrowse           :=  SLOVAK_ewBrowse;
        ewReadOnly         :=  SLOVAK_ewReadOnly;
        ewArchive          :=  SLOVAK_ewArchive;
        ewHidden           :=  SLOVAK_ewHidden;
        ewSystem           :=  SLOVAK_ewSystem;

        esCannot           :=  SLOVAK_esCannot;
        esSpecifyDir       :=  SLOVAK_esSpecifyDir;
        esInvalidDrvID     :=  SLOVAK_esInvalidDrvID;
        esDrvNotReady      :=  SLOVAK_esDrvNotReady;
        esExists           :=  SLOVAK_esExists;
        esNewFolder        :=  SLOVAK_esNewFolder;
        esReadOnly         :=  SLOVAK_esReadOnly;
        esNoFileSelected   :=  SLOVAK_esNoFileSelected;
        esSendToFolder     :=  SLOVAK_esSendToFolder;
        esSendToPath       :=  SLOVAK_esSendToPath;
        esPersistSaveError :=  SLOVAK_esPersistSaveError;
        esSetAttr          :=  SLOVAK_esSetAttr;
      end;  //876>
    lnSlovenian:  //MPX1
      begin  //881
        ewError            :=  SLO_ewError;
        ewFile             :=  SLO_ewFile;
        ewCancel           :=  SLO_ewCancel;
        ewBrowse           :=  SLO_ewBrowse;
        ewReadOnly         :=  SLO_ewReadOnly;
        ewArchive          :=  SLO_ewArchive;
        ewHidden           :=  SLO_ewHidden;
        ewSystem           :=  SLO_ewSystem;

        esCannot           :=  SLO_esCannot;
        esSpecifyDir       :=  SLO_esSpecifyDir;
        esInvalidDrvID     :=  SLO_esInvalidDrvID;
        esDrvNotReady      :=  SLO_esDrvNotReady;
        esExists           :=  SLO_esExists;
        esNewFolder        :=  SLO_esNewFolder;
        esReadOnly         :=  SLO_esReadOnly;
        esNoFileSelected   :=  SLO_esNoFileSelected;
        esSendToFolder     :=  SLO_esSendToFolder;
        esSendToPath       :=  SLO_esSendToPath;
        esPersistSaveError :=  SLO_esPersistSaveError;
        esSetAttr          :=  SLO_esSetAttr;
      end;  //881>
    lnSpanish:
      begin  //873<
        ewError            :=  SPANISH_ewError;
        ewFile             :=  SPANISH_ewFile;
        ewCancel           :=  SPANISH_ewCancel;
        ewBrowse           :=  SPANISH_ewBrowse;
        ewReadOnly         :=  SPANISH_ewReadOnly;
        ewArchive          :=  SPANISH_ewArchive;
        ewHidden           :=  SPANISH_ewHidden;
        ewSystem           :=  SPANISH_ewSystem;

        esCannot           :=  SPANISH_esCannot;
        esSpecifyDir       :=  SPANISH_esSpecifyDir;
        esInvalidDrvID     :=  SPANISH_esInvalidDrvID;
        esDrvNotReady      :=  SPANISH_esDrvNotReady;
        esExists           :=  SPANISH_esExists;
        esNewFolder        :=  SPANISH_esNewFolder;
        esReadOnly         :=  SPANISH_esReadOnly;
        esNoFileSelected   :=  SPANISH_esNoFileSelected;
        esSendToFolder     :=  SPANISH_esSendToFolder;
        esSendToPath       :=  SPANISH_esSendToPath;
        esPersistSaveError :=  SPANISH_esPersistSaveError;
        esSetAttr          :=  SPANISH_esSetAttr;
      end;  //873>
    lnSwedish:
      begin  //882<
        ewError            :=  SWEDISH_ewError;
        ewFile             :=  SWEDISH_ewFile;
        ewCancel           :=  SWEDISH_ewCancel;
        ewBrowse           :=  SWEDISH_ewBrowse;
        ewReadOnly         :=  SWEDISH_ewReadOnly;
        ewArchive          :=  SWEDISH_ewArchive;
        ewHidden           :=  SWEDISH_ewHidden;
        ewSystem           :=  SWEDISH_ewSystem;

        esCannot           :=  SWEDISH_esCannot;
        esSpecifyDir       :=  SWEDISH_esSpecifyDir;
        esInvalidDrvID     :=  SWEDISH_esInvalidDrvID;
        esDrvNotReady      :=  SWEDISH_esDrvNotReady;
        esExists           :=  SWEDISH_esExists;
        esNewFolder        :=  SWEDISH_esNewFolder;
        esReadOnly         :=  SWEDISH_esReadOnly;
        esNoFileSelected   :=  SWEDISH_esNoFileSelected;
        esSendToFolder     :=  SWEDISH_esSendToFolder;
        esSendToPath       :=  SWEDISH_esSendToPath;
        esPersistSaveError :=  SWEDISH_esPersistSaveError;
        esSetAttr          :=  SWEDISH_esSetAttr;
      end;  //882>
    lnTurkish:
      begin  //886<
        ewError            :=  TURKISH_ewError;
        ewFile             :=  TURKISH_ewFile;
        ewCancel           :=  TURKISH_ewCancel;
        ewBrowse           :=  TURKISH_ewBrowse;
        ewReadOnly         :=  TURKISH_ewReadOnly;
        ewArchive          :=  TURKISH_ewArchive;
        ewHidden           :=  TURKISH_ewHidden;
        ewSystem           :=  TURKISH_ewSystem;

        esCannot           :=  TURKISH_esCannot;
        esSpecifyDir       :=  TURKISH_esSpecifyDir;
        esInvalidDrvID     :=  TURKISH_esInvalidDrvID;
        esDrvNotReady      :=  TURKISH_esDrvNotReady;
        esExists           :=  TURKISH_esExists;
        esNewFolder        :=  TURKISH_esNewFolder;
        esReadOnly         :=  TURKISH_esReadOnly;
        esNoFileSelected   :=  TURKISH_esNoFileSelected;
        esSendToFolder     :=  TURKISH_esSendToFolder;
        esSendToPath       :=  TURKISH_esSendToPath;
        esPersistSaveError :=  TURKISH_esPersistSaveError;
        esSetAttr          :=  TURKISH_esSetAttr;
      end;  //>886
    end;  //Case
end;  {= InitializeVar =}  //872ln>


//===== Accept Dropped Files from Windows Explorer =====//
//878_DragDrop<
procedure TLsFilelistView28.InitFDragControl;
var
  WinCtl: TWinControl;
begin
   if Owner is TWinControl then
    begin
      // Subclass the owner so this control can capture the WM_DROPFILES message
      WinCtl := TWinControl( Owner );
      FWndHandle := WinCtl.Handle;
      FWndProcInstance := MakeObjectInstance( WndProc );
      FDefProc := Pointer( GetWindowLong( FWndHandle, GWL_WNDPROC ));
      SetWindowLong( FWndHandle, GWL_WNDPROC, Longint( FWndProcInstance ));
    end
   else
    FEnabled := False;
end;  {= InitFDragControl =}  //878_DragDrop>

procedure TLsFilelistView28.DestroyFDragControl;  //878_DragDrop<
begin
  if FWndHandle <> 0 then
   begin
     // Restore the original window procedure
     SetWindowLong( FWndHandle, GWL_WNDPROC, Longint( FDefProc ));
     FreeObjectInstance(FWndProcInstance);
   end
end;  {= DestroyFDragControl =}  //878_DragDrop>

procedure TLsFilelistView28.DropFiles( hDropHandle: HDrop );  //878_DragDrop<
var
  pszFileName , pszFile: PChar;
  iFile, iStrLen, iTempLen: Integer;
begin
  iStrLen := 128;
  pszFileName := StrAlloc( iStrLen );
  iFile := 0;
  pszFile := '';  //879
  FDroppedFiles.Clear;

  // Retrieve the number of dropped files
  FNumDropped := DragQueryFile( hDropHandle, $FFFFFFFF, pszFile, iStrLen );

  // Retrieve each file being dropped
  while ( iFile < FNumDropped ) do
  begin
    // Get the length of the file. If file length > iStrLen, re-allocate it
    iTempLen := DragQueryFile( hDropHandle, iFile, nil, 0 ) + 1;
    if ( iTempLen > iStrLen ) then
    begin
      iStrLen := iTempLen;
      StrDispose( pszFileName );
      pszFileName := StrAlloc( iStrLen );
    end;
    // Retrieve the filename with path, then add it to StringList FDroppedFiles
    DragQueryFile( hDropHandle, iFile, pszFileName, iStrLen );
    FDroppedFiles.Add( StrPas( pszFileName ));
    Inc( iFile );
  end;
  StrDispose(pszFileName);

  // paste dropped files into FileSystem
  if FDroppedFiles.Count <> 0 then
  begin
    FSelectedFiles := FDroppedFiles;
    FOpMode := 0;
    Paste;
  end;
end;  {= DropFiles =}  //878_DragDrop>

procedure TLsFilelistView28.WndProc( var Msg: TMessage );  //878_DragDrop<
begin
  with Msg do
  begin
    // If message is drop files, process,
    // otherwise call the original window procedure
    Case Msg Of
      WM_DROPFILES : DropFiles( HDrop( wParam ));
    else
      Result := CallWindowProc( FDefProc, FWndHandle, Msg, WParam, LParam);
    end;
  end;
end;  {= WndProc =}   //878_DragDrop>

procedure TLsFileListview28.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);  //878_DragDrop<
begin
  if Button = mbLeft then
  begin
    if (ssCtrl in Shift) then
      FDgDpMode := 0   //Copy
    else
      FDgDpMode := 2;  //Move
//    ShowMessage('FDgDpMode = '+ IntToStr(FDgDpMode));  //debugging
    if (Selected <> nil) and (Assigned(FDirTree)) and
       (FDirTree.DragDropEnabled = True) then
      BeginDrag(False, Mouse.DragThreshold + 10);
  end;
end;   {= MouseDown =}   //878_DragDrop>

//===== End of Accept Dropped Files from Explorer ======//

//============== End of TLsFilelistView28 ==============//



{*********************************************************}
{                 LsFilelistView28PopUp                   }
{*********************************************************}

constructor TLsFilelistView28PopUp.Create(AOwner: TComponent);
var
  sfi: TSHFileInfo;
  hImgLst: Uint;
begin
  inherited Create(AOwner);
  SendToList := TStringList.Create;
  Images := TImageList.Create(self);
  hImgLst := SHGetFileInfo('', 0,
                           sfi, SizeOf(sfi),
                           SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  if hImgLst <> 0 then
  begin
    Images.Handle := hImgLst;
    Images.BkColor := clNone;  //80+
    Images.ShareImages := True;
  end;

  // Load Bitmaps from Resource  //85 <<
  Bmp1 := LoadImage(hInstance, 'Open28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp2 := LoadImage(hInstance, 'View28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp3 := LoadImage(hInstance, 'SendTo28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp4 := LoadImage(hInstance, 'Cut28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp5 := LoadImage(hInstance, 'Copy28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp6 := LoadImage(hInstance, 'Paste28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp7 := LoadImage(hInstance, 'ReName28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp8 := LoadImage(hInstance, 'Delete28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp9 := LoadImage(hInstance, 'Attributes28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp10 := LoadImage(hInstance, 'NewFolder28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp11 := LoadImage(hInstance, 'AnyFolder28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp12 := LoadImage(hInstance, 'ClipBoard28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  Bmp13 := LoadImage(hInstance, 'DeskTop28', IMAGE_BITMAP, 0, 0, LR_LOADMAP3DCOLORS);
  //85 >>
end; {= Create =}

destructor TLsFilelistView28PopUp.Destroy;
begin
  SendToList.Free;
  Images.Free;  //70
  DeleteObject(Bmp1); //70 <<
  DeleteObject(Bmp2);
  DeleteObject(Bmp3);
  DeleteObject(Bmp4);
  DeleteObject(Bmp5);
  DeleteObject(Bmp6);
  DeleteObject(Bmp7);
  DeleteObject(Bmp8);
  DeleteObject(Bmp9);
  DeleteObject(Bmp10);
  DeleteObject(Bmp11);
  DeleteObject(Bmp12); //70 >>
  DeleteObject(Bmp13); //85
  inherited Destroy;
end; {= Destroy =}

function TLsFilelistView28PopUp.AddNewItem(const aCaption: string;
  aShortCut: TShortCut; aChecked, aEnabled: Boolean;
  aGroup: integer; aOnClick: TNotifyEvent;
  hCtx: word; const aName: string; aTag: Integer;
  aBitMap: HBitmap): TMenuItem; //70  //862
begin
  Result := TMenuItem.Create(nil);
  with result do
  begin
    Caption := aCaption;
    ShortCut := aShortCut;
    Checked := aChecked;
    Enabled := aEnabled;
    GroupIndex := aGroup;
    OnClick := aOnClick;
    Name := aName;
    Tag := aTag;   //862
    BITMAP.Handle := aBitmap;
  end;
end; {= AddNewItem =}


procedure TLsFilelistView28PopUp.SetFileListView(Value: TLsFilelistView28);
begin
  FFileListView := Value;
end; {= SetFileListView =}


procedure TLsFilelistView28PopUp.GetSendToSubMenu;
var
  SendToDir,
    FName,
    FullName: string;
  win32FD: TWin32FindData;
  hFindFile: THandle;
  sfi: TSHFileInfo;
  i: integer;
  NewItem: TMenuItem;

  function GetShellPath(Handle: THandle; var DestPath: string;
    nFldr: integer): Boolean;
  var
    ShellMalloc: IMALLOC;
    shBuff: pChar;
    idRoot: PItemIDList;
  begin
    Result := False;
    SetLength(DestPath, MAX_PATH);
    if CoGetMalloc(1, ShellMalloc) = NOERROR then
    try
      shBuff := PChar(ShellMalloc.Alloc(MAX_PATH));
      if assigned(shBuff) then
      begin
        SHGetSpecialFolderLocation(Handle, nFldr, idRoot);
        // Convert idRoot to a file system path and pass to shBuff.
        if SHGetPathFromIDList(idRoot, shBuff) then
        begin
          DestPath := shBuff;
          Result := True;
        end;
      end;
    finally
      ShellMalloc.Free(idRoot);
      ShellMalloc._Release; //53
      //  ShellMalloc.Free(shBuff);   //53
    end;
  end; {= GetShellPath =}

begin
  SendToList.Clear;
  i := 0;
  if GetShellPath(Handle, SendToDir, CSIDL_SENDTO) then
  begin
    hFindFile := FindFirstFile(PChar(SlashSep(SendToDir, '*.LNK')), win32FD);
    if hFindFile <> INVALID_HANDLE_VALUE then
    try
      repeat
        with win32FD do
        begin
          FName := StrPas(cFileName);
          if (FName = '.') or (FName = '..') then continue;
          FullName := SlashSep(SendToDir, FName);
          SHGetFileInfo(PChar(FullName), 0, sfi, SizeOf(sfi),
            SHGFI_SYSICONINDEX or SHGFI_SMALLICON or SHGFI_DISPLAYNAME); //60

          NewItem := AddNewItem(StrPas(sfi.szDisplayName), 0, False,
            True, 1, ItemOnClick, 0, 'Send_To' + IntToStr(i + 4), 34 + i, 0);  //862
          NewItem.ImageIndex := sfi.iIcon; //60
          Items.Items[3].Add(NewItem);
          SendToList.Add(FullName);
          inc(i);
        end;
      until not FindNextFile(hFindFile, win32FD);
    finally
      Windows.FindClose(hFindFile);
    end;
  end;
end; {= GetSendToSubMenu =}


procedure TLsFilelistView28PopUp.BuildItems; //70  //872ln
begin
  //define language specific MenuItem's captions
  Case LvLangID of
    lnEnglish:
      begin   //872<
        LvItemID_0  :=  ENGLISH_LvItemID_0;
        LvItemID_1  :=  ENGLISH_LvItemID_1;
        LvItemID_3  :=  ENGLISH_LvItemID_3;
        LvItemID_5  :=  ENGLISH_LvItemID_5;
        LvItemID_6  :=  ENGLISH_LvItemID_6;
        LvItemID_7  :=  ENGLISH_LvItemID_7;
        LvItemID_9  :=  ENGLISH_LvItemID_9;
        LvItemID_10 :=  ENGLISH_LvItemID_10;
        LvItemID_12 :=  ENGLISH_LvItemID_12;
        LvItemID_14 :=  ENGLISH_LvItemID_14;
        LvItemID_30 :=  ENGLISH_LvItemID_30;
        LvItemID_31 :=  ENGLISH_LvItemID_31;
        LvItemID_32 :=  ENGLISH_LvItemID_32;
        LvItemID_33 :=  ENGLISH_LvItemID_33;
      end;  //872>

    lnChinese_Tra:
      begin  //879<
        LvItemID_0  :=  CHINESE_Tra_LvItemID_0;
        LvItemID_1  :=  CHINESE_Tra_LvItemID_1;
        LvItemID_3  :=  CHINESE_Tra_LvItemID_3;
        LvItemID_5  :=  CHINESE_Tra_LvItemID_5;
        LvItemID_6  :=  CHINESE_Tra_LvItemID_6;
        LvItemID_7  :=  CHINESE_Tra_LvItemID_7;
        LvItemID_9  :=  CHINESE_Tra_LvItemID_9;
        LvItemID_10 :=  CHINESE_Tra_LvItemID_10;
        LvItemID_12 :=  CHINESE_Tra_LvItemID_12;
        LvItemID_14 :=  CHINESE_Tra_LvItemID_14;
        LvItemID_30 :=  CHINESE_Tra_LvItemID_30;
        LvItemID_31 :=  CHINESE_Tra_LvItemID_31;
        LvItemID_32 :=  CHINESE_Tra_LvItemID_32;
        LvItemID_33 :=  CHINESE_Tra_LvItemID_33;
      end;   //879>
    lnChinese_Sim:
      begin  //879<
        LvItemID_0  :=  CHINESE_Sim_LvItemID_0;
        LvItemID_1  :=  CHINESE_Sim_LvItemID_1;
        LvItemID_3  :=  CHINESE_Sim_LvItemID_3;
        LvItemID_5  :=  CHINESE_Sim_LvItemID_5;
        LvItemID_6  :=  CHINESE_Sim_LvItemID_6;
        LvItemID_7  :=  CHINESE_Sim_LvItemID_7;
        LvItemID_9  :=  CHINESE_Sim_LvItemID_9;
        LvItemID_10 :=  CHINESE_Sim_LvItemID_10;
        LvItemID_12 :=  CHINESE_Sim_LvItemID_12;
        LvItemID_14 :=  CHINESE_Sim_LvItemID_14;
        LvItemID_30 :=  CHINESE_Sim_LvItemID_30;
        LvItemID_31 :=  CHINESE_Sim_LvItemID_31;
        LvItemID_32 :=  CHINESE_Sim_LvItemID_32;
        LvItemID_33 :=  CHINESE_Sim_LvItemID_33;
      end;  //879>
    lnDutch:
      begin   //875<
        LvItemID_0  :=  DUTCH_LvItemID_0;
        LvItemID_1  :=  DUTCH_LvItemID_1;
        LvItemID_3  :=  DUTCH_LvItemID_3;
        LvItemID_5  :=  DUTCH_LvItemID_5;
        LvItemID_6  :=  DUTCH_LvItemID_6;
        LvItemID_7  :=  DUTCH_LvItemID_7;
        LvItemID_9  :=  DUTCH_LvItemID_9;
        LvItemID_10 :=  DUTCH_LvItemID_10;
        LvItemID_12 :=  DUTCH_LvItemID_12;
        LvItemID_14 :=  DUTCH_LvItemID_14;
        LvItemID_30 :=  DUTCH_LvItemID_30;
        LvItemID_31 :=  DUTCH_LvItemID_31;
        LvItemID_32 :=  DUTCH_LvItemID_32;
        LvItemID_33 :=  DUTCH_LvItemID_33;
      end;  //875>
    lnFrench:
      begin  //872<
        LvItemID_0  :=  FRENCH_LvItemID_0;
        LvItemID_1  :=  FRENCH_LvItemID_1;
        LvItemID_3  :=  FRENCH_LvItemID_3;
        LvItemID_5  :=  FRENCH_LvItemID_5;
        LvItemID_6  :=  FRENCH_LvItemID_6;
        LvItemID_7  :=  FRENCH_LvItemID_7;
        LvItemID_9  :=  FRENCH_LvItemID_9;
        LvItemID_10 :=  FRENCH_LvItemID_10;
        LvItemID_12 :=  FRENCH_LvItemID_12;
        LvItemID_14 :=  FRENCH_LvItemID_14;
        LvItemID_30 :=  FRENCH_LvItemID_30;
        LvItemID_31 :=  FRENCH_LvItemID_31;
        LvItemID_32 :=  FRENCH_LvItemID_32;
        LvItemID_33 :=  FRENCH_LvItemID_33;
      end;
    lnGerman:
      begin
        LvItemID_0  :=  GERMAN_LvItemID_0;
        LvItemID_1  :=  GERMAN_LvItemID_1;
        LvItemID_3  :=  GERMAN_LvItemID_3;
        LvItemID_5  :=  GERMAN_LvItemID_5;
        LvItemID_6  :=  GERMAN_LvItemID_6;
        LvItemID_7  :=  GERMAN_LvItemID_7;
        LvItemID_9  :=  GERMAN_LvItemID_9;
        LvItemID_10 :=  GERMAN_LvItemID_10;
        LvItemID_12 :=  GERMAN_LvItemID_12;
        LvItemID_14 :=  GERMAN_LvItemID_14;
        LvItemID_30 :=  GERMAN_LvItemID_30;
        LvItemID_31 :=  GERMAN_LvItenID_31;
        LvItemID_32 :=  GERMAN_LvItemID_32;
        LvItemID_33 :=  GERMAN_LvItemID_33;
      end;  //872>
    lnItalian:
      begin   //874<
        LvItemID_0  :=  ITALIAN_LvItemID_0;
        LvItemID_1  :=  ITALIAN_LvItemID_1;
        LvItemID_3  :=  ITALIAN_LvItemID_3;
        LvItemID_5  :=  ITALIAN_LvItemID_5;
        LvItemID_6  :=  ITALIAN_LvItemID_6;
        LvItemID_7  :=  ITALIAN_LvItemID_7;
        LvItemID_9  :=  ITALIAN_LvItemID_9;
        LvItemID_10 :=  ITALIAN_LvItemID_10;
        LvItemID_12 :=  ITALIAN_LvItemID_12;
        LvItemID_14 :=  ITALIAN_LvItemID_14;
        LvItemID_30 :=  ITALIAN_LvItemID_30;
        LvItemID_31 :=  ITALIAN_LvItemID_31;
        LvItemID_32 :=  ITALIAN_LvItemID_32;
        LvItemID_33 :=  ITALIAN_LvItemID_33;
      end; //874>
    lnJapanese:
      begin  //880<
        LvItemID_0  :=  JAPANESE_LvItemID_0;
        LvItemID_1  :=  JAPANESE_LvItemID_1;
        LvItemID_3  :=  JAPANESE_LvItemID_3;
        LvItemID_5  :=  JAPANESE_LvItemID_5;
        LvItemID_6  :=  JAPANESE_LvItemID_6;
        LvItemID_7  :=  JAPANESE_LvItemID_7;
        LvItemID_9  :=  JAPANESE_LvItemID_9;
        LvItemID_10 :=  JAPANESE_LvItemID_10;
        LvItemID_12 :=  JAPANESE_LvItemID_12;
        LvItemID_14 :=  JAPANESE_LvItemID_14;
        LvItemID_30 :=  JAPANESE_LvItemID_30;
        LvItemID_31 :=  JAPANESE_LvItemID_31;
        LvItemID_32 :=  JAPANESE_LvItemID_32;
        LvItemID_33 :=  JAPANESE_LvItemID_33;
      end;  //880>
    lnKorean:
      begin   //885<
        LvItemID_0  :=  KOREAN_LvItemID_0;
        LvItemID_1  :=  KOREAN_LvItemID_1;
        LvItemID_3  :=  KOREAN_LvItemID_3;
        LvItemID_5  :=  KOREAN_LvItemID_5;
        LvItemID_6  :=  KOREAN_LvItemID_6;
        LvItemID_7  :=  KOREAN_LvItemID_7;
        LvItemID_9  :=  KOREAN_LvItemID_9;
        LvItemID_10 :=  KOREAN_LvItemID_10;
        LvItemID_12 :=  KOREAN_LvItemID_12;
        LvItemID_14 :=  KOREAN_LvItemID_14;
        LvItemID_30 :=  KOREAN_LvItemID_30;
        LvItemID_31 :=  KOREAN_LvItemID_31;
        LvItemID_32 :=  KOREAN_LvItemID_32;
        LvItemID_33 :=  KOREAN_LvItemID_33;
      end;  //885>
    lnPolish:
      begin  //876<
        LvItemID_0  :=  POLISH_LvItemID_0;
        LvItemID_1  :=  POLISH_LvItemID_1;
        LvItemID_3  :=  POLISH_LvItemID_3;
        LvItemID_5  :=  POLISH_LvItemID_5;
        LvItemID_6  :=  POLISH_LvItemID_6;
        LvItemID_7  :=  POLISH_LvItemID_7;
        LvItemID_9  :=  POLISH_LvItemID_9;
        LvItemID_10 :=  POLISH_LvItemID_10;
        LvItemID_12 :=  POLISH_LvItemID_12;
        LvItemID_14 :=  POLISH_LvItemID_14;
        LvItemID_30 :=  POLISH_LvItemID_30;
        LvItemID_31 :=  POLISH_LvItemID_31;
        LvItemID_32 :=  POLISH_LvItemID_32;
        LvItemID_33 :=  POLISH_LvItemID_33;
      end; //876>
    lnPortuguese:
      begin  //873<
        LvItemID_0  :=  BRAZ_PORT_LvItemID_0;
        LvItemID_1  :=  BRAZ_PORT_LvItemID_1;
        LvItemID_3  :=  BRAZ_PORT_LvItemID_3;
        LvItemID_5  :=  BRAZ_PORT_LvItemID_5;
        LvItemID_6  :=  BRAZ_PORT_LvItemID_6;
        LvItemID_7  :=  BRAZ_PORT_LvItemID_7;
        LvItemID_9  :=  BRAZ_PORT_LvItemID_9;
        LvItemID_10 :=  BRAZ_PORT_LvItemID_10;
        LvItemID_12 :=  BRAZ_PORT_LvItemID_12;
        LvItemID_14 :=  BRAZ_PORT_LvItemID_14;
        LvItemID_30 :=  BRAZ_PORT_LvItemID_30;
        LvItemID_31 :=  BRAZ_PORT_LvItemID_31;
        LvItemID_32 :=  BRAZ_PORT_LvItemID_32;
        LvItemID_33 :=  BRAZ_PORT_LvItemID_33;
      end;  //873>
    lnSlovak:
      begin  //876<
        LvItemID_0  :=  SLOVAK_LvItemID_0;
        LvItemID_1  :=  SLOVAK_LvItemID_1;
        LvItemID_3  :=  SLOVAK_LvItemID_3;
        LvItemID_5  :=  SLOVAK_LvItemID_5;
        LvItemID_6  :=  SLOVAK_LvItemID_6;
        LvItemID_7  :=  SLOVAK_LvItemID_7;
        LvItemID_9  :=  SLOVAK_LvItemID_9;
        LvItemID_10 :=  SLOVAK_LvItemID_10;
        LvItemID_12 :=  SLOVAK_LvItemID_12;
        LvItemID_14 :=  SLOVAK_LvItemID_14;
        LvItemID_30 :=  SLOVAK_LvItemID_30;
        LvItemID_31 :=  SLOVAK_LvItemID_31;
        LvItemID_32 :=  SLOVAK_LvItemID_32;
        LvItemID_33 :=  SLOVAK_LvItemID_33;
      end; //876>
    lnSlovenian:  //MPX1
      begin  //881<
        LvItemID_0  :=  SLO_LvItemID_0;
        LvItemID_1  :=  SLO_LvItemID_1;
        LvItemID_3  :=  SLO_LvItemID_3;
        LvItemID_5  :=  SLO_LvItemID_5;
        LvItemID_6  :=  SLO_LvItemID_6;
        LvItemID_7  :=  SLO_LvItemID_7;
        LvItemID_9  :=  SLO_LvItemID_9;
        LvItemID_10 :=  SLO_LvItemID_10;
        LvItemID_12 :=  SLO_LvItemID_12;
        LvItemID_14 :=  SLO_LvItemID_14;
        LvItemID_30 :=  SLO_LvItemID_30;
        LvItemID_31 :=  SLO_LvItemID_31;
        LvItemID_32 :=  SLO_LvItemID_32;
        LvItemID_33 :=  SLO_LvItemID_33;
      end;  //881>
    lnSpanish:
      begin
        LvItemID_0  :=  SPANISH_LvItemID_0;
        LvItemID_1  :=  SPANISH_LvItemID_1;
        LvItemID_3  :=  SPANISH_LvItemID_3;
        LvItemID_5  :=  SPANISH_LvItemID_5;
        LvItemID_6  :=  SPANISH_LvItemID_6;
        LvItemID_7  :=  SPANISH_LvItemID_7;
        LvItemID_9  :=  SPANISH_LvItemID_9;
        LvItemID_10 :=  SPANISH_LvItemID_10;
        LvItemID_12 :=  SPANISH_LvItemID_12;
        LvItemID_14 :=  SPANISH_LvItemID_14;
        LvItemID_30 :=  SPANISH_LvItemID_30;
        LvItemID_31 :=  SPANISH_LvItemID_31;
        LvItemID_32 :=  SPANISH_LvItemID_32;
        LvItemID_33 :=  SPANISH_LvItemID_33;
      end;  //872>
    lnSwedish:
      begin  //882<
        LvItemID_0  :=  SWEDISH_LvItemID_0;
        LvItemID_1  :=  SWEDISH_LvItemID_1;
        LvItemID_3  :=  SWEDISH_LvItemID_3;
        LvItemID_5  :=  SWEDISH_LvItemID_5;
        LvItemID_6  :=  SWEDISH_LvItemID_6;
        LvItemID_7  :=  SWEDISH_LvItemID_7;
        LvItemID_9  :=  SWEDISH_LvItemID_9;
        LvItemID_10 :=  SWEDISH_LvItemID_10;
        LvItemID_12 :=  SWEDISH_LvItemID_12;
        LvItemID_14 :=  SWEDISH_LvItemID_14;
        LvItemID_30 :=  SWEDISH_LvItemID_30;
        LvItemID_31 :=  SWEDISH_LvItemID_31;
        LvItemID_32 :=  SWEDISH_LvItemID_32;
        LvItemID_33 :=  SWEDISH_LvItemID_33;
      end;  //882>
    lnTurkish:
      begin   //886<
        LvItemID_0  :=  TURKISH_LvItemID_0;
        LvItemID_1  :=  TURKISH_LvItemID_1;
        LvItemID_3  :=  TURKISH_LvItemID_3;
        LvItemID_5  :=  TURKISH_LvItemID_5;
        LvItemID_6  :=  TURKISH_LvItemID_6;
        LvItemID_7  :=  TURKISH_LvItemID_7;
        LvItemID_9  :=  TURKISH_LvItemID_9;
        LvItemID_10 :=  TURKISH_LvItemID_10;
        LvItemID_12 :=  TURKISH_LvItemID_12;
        LvItemID_14 :=  TURKISH_LvItemID_14;
        LvItemID_30 :=  TURKISH_LvItemID_30;
        LvItemID_31 :=  TURKISH_LvItemID_31;
        LvItemID_32 :=  TURKISH_LvItemID_32;
        LvItemID_33 :=  TURKISH_LvItemID_33;
      end;  //>886
  end;  //case
  //Build MenuItems
  Items.Add(AddNewItem(LvItemID_0, 0, False, True, 0,
    ItemOnClick, 0, 'ItemOpen', 1, Bmp1)); //Items[0]
  Items.Add(AddNewItem(LvItemID_1, 0, False, True, 0,
    ItemOnClick, 0, 'ItemView', 2, Bmp2)); //Items[1]
  Items.Add(NewLine);
  Items.Add(AddNewItem(LvItemID_3, 0, False, True, 0,
    ItemOnClick, 0, 'SubMenuSend', 3, Bmp3)); //Items[3]
  Items.Add(NewLine);
  Items.Add(AddNewItem(LvItemID_5, 0, False, True, 0,
    ItemOnClick, 0, 'ItemOpCut', 4, Bmp4)); //Items[5]
  Items.Add(AddNewItem(LvItemID_6, 0, False, True, 0,
    ItemOnClick, 0, 'ItemOpCopy', 5, Bmp5)); //Items[6]
  Items.Add(AddNewItem(LvItemID_7, 0, False, True, 0,
    ItemOnClick, 0, 'ItemOpPaste', 6, Bmp6)); //Items[7]
  Items.Add(NewLine);
  Items.Add(AddNewItem(LvItemID_9, 0, False, True, 0,
    ItemOnClick, 0, 'ItemRename', 7, Bmp7)); //Items[9]
  Items.Add(AddNewItem(LvItemID_10, 0, False, True, 0,
    ItemOnClick, 0, 'ItemDelete', 8, Bmp8)); //Items[10]
  Items.Add(NewLine);
  Items.Add(AddNewItem(LvItemID_12, 0, False, True, 0,
    ITemOnClick, 0, 'ItemProperty', 9, Bmp9)); //Items[12]
  Items.Add(NewLine);
  Items.Add(AddNewItem(LvItemID_14, 0, False, True, 0,
    ItemOnClick, 0, 'ItemFolder', 10, Bmp10)); //Items[14]
  // Items[3] SendTo SubItems
  Items.Items[3].Add(AddNewItem(LvItemID_30, 0, False, True, 0,
    ItemOnClick, 0, 'Send_To' + IntToStr(0), 30, Bmp11));
  Items.Items[3].Add(AddNewItem(LvItemID_31, 0, False, True, 0,
    ItemOnClick, 0, 'Send_To' + IntToStr(1), 31, Bmp12));
  Items.Items[3].Add(AddNewItem(LvItemID_32, 0, False, True, 0,
    ItemOnClick, 0, 'Send_To' + IntToStr(2), 32, Bmp12));
  Items.Items[3].Add(AddNewItem(LvItemID_33, 0, False, True, 0,
    ItemOnClick, 0, 'Send_To' + IntToStr(3), 33, Bmp13));
  GetSendToSubMenu;  //872ln<
end; {= BuildItems =}

procedure TLsFilelistView28PopUp.ItemOnClick(Sender: TObject);
var
  i: integer;
begin
  if (TMenuItem(Sender).Name = '') or (TMenuItem(Sender).Tag = 0) then
    exit;
  with Sender as TMenuItem do   //862<<
  begin
    Case Tag of
      1:  FFileListView.OpenItem;
      2:  FFileListView.ViewFile;
      4:  FFileListView.CutCopy(2);
      5:  FFileListView.CutCopy(0);
      6:  FFileListView.Paste;
      7:  FFileListView.RenameFile;
      8:  FFileListView.DeleteItems;
      9:  FFileListView.ShowFileProperties;
      10:  FFileListView.NewFolder;   //862>>
      else begin
        if Pos('Send_To', TMenuItem(Sender).Name) = 1 then
        begin
          i := StrToIntDef(Copy(TMenuItem(Sender).Name, 8, 2), -1);
          if (i > -1) and (i < 4) then  //85
            FFileListView.SendTo(i)
          else if (i > 3) then  //85
            FFileListView.SendTo2(SendToList[i - 4]);
        end;
      end; //else
    end; //case
  end;  //with Sender ...
end; {= ItemOnClick =}

procedure TLsFilelistView28PopUp.PopUp(X, Y: integer);
var
  i: integer;
  Ext: string;
begin
  Ext := '';
  if (FFileListView.Selected <> nil) then
    Ext := (Uppercase(ExtractFileExt(FFileListView.Selected.SubItems[4]))
      );
  Items[0].Enabled := FFileListView.Selected <> nil;
  Items[1].Enabled := False;
  if (FFileListView.Selected <> nil) then
  begin
    for i := Low(FileExt) to High(FileExt) do
      if (Ext = FileExt[i]) then
        Items[1].Enabled := True;
  end;
  Items[3].Enabled := FFileListView.Selected <> nil;
  Items[5].Enabled := FFileListView.Selected <> nil;
  Items[6].Enabled := FFileListView.Selected <> nil;
  Items[7].Enabled := (FFileListView.FSelectedFiles.Count <> 0) OR
                      (Clipboard.HasFormat(CF_HDROP));   //878Clipbrd
  Items[9].Enabled := (FFileListView.Selected <> nil) and
                      (FFileListView.SelCount = 1);
  Items[10].Enabled := FFileListView.Selected <> nil;
  Items[12].Enabled := FFileListView.Selected <> nil;
  Items[14].Enabled := (AnsiCompareText(FFileListView.Directory,'My Computer') <> 0 );  //885r2
  Items[3][0].Enabled := FFileListView.Selected <> nil;
  Items[3][1].Enabled := False;  //85 >>
  if (FFileListView.Selected <> nil) and (FFileListView.SelCount = 1) then
  begin
    for i := Low(FileExt) to (High(FileExt) - 2) do
      if (Ext = FileExt[i]) or (Ext = '.RTF') or (Ext = '.WMF') then
        Items[3][1].Enabled := True;
  end; //85
  Items[3][2].Enabled := FFileListView.Selected <> nil;
  Items[3][3].Enabled := FFileListView.Selected <> nil;
  inherited Popup(X + 10, Y + 10);
end; {= PopUp =}

//============ End of TLsFilelistView28PopUp ===========//


{*********************************************************}
{                      TNetPathDlg                 //880  }
{*********************************************************}

constructor TNetPathDlg.Create(AOwner: TComponent);  //880<
begin
  inherited Create(AOwner);
  FDrvLetter := '';
  FNetPath := '';
  FDlgForm := TForm.Create(Self);
  with FDlgForm do
  begin
    Color := clBtnFace;
    Canvas.Font.Name := 'MS Sans Serif';
    Canvas.Font.Size := 8;
    BorderStyle := bsDialog;
    ClientWidth := 382;
    ClientHeight := 160;
    Position := poScreenCenter;
    Caption := 'Map Network Drive';
    //Create Labels
    FLabel1 := TLabel.Create(Self);
    with FLabel1 do
    begin
      Parent := FDlgForm;
      SetBounds(28, 8, 326, 26);  //880
      Caption := 'Specify an avilable Drive Letter in My Computer for the connection ' + #13 +
                 'and the path to the Network Computer that you want to connect to:';
      WordWrap := True;
    end;  //Label1
    FLabel2 := TLabel.Create(Self);
    with FLabel2 do
    begin
      Parent := FDlgForm;
      Setbounds(28, 56, 25, 13);  //880
      Caption := 'Drive';
    end;  //Label2
    FLabel3 := TLabel.Create(Self);
    with FLabel3 do
    begin
      Parent := FDlgForm;
      Setbounds(28, 88, 25, 13);  //880
      Caption := 'Path';
    end;  //Label3
    //Create Edits
    FCbxDrvLetter := TComboBox.Create(Self);
    with FCbxDrvLetter do
    begin
      Parent := FDlgForm;
      Setbounds(60, 52, 189, 21);  //880
      TabOrder := 0;
      Text := FDrvLetter;
    end;  //DrvLetter
    FEdtNetPath := TEdit.Create(Self);
    with FEdtNetPath do
    begin
      Parent := FDlgForm;
      Setbounds(60, 84, 189, 21);  //880
      TabOrder := 1;
      Text := FNetPath;
    end;  //EdtNetDrvPath
    //Create Buttons
    FBtnOk := TBitbtn.Create(Self);
    with FBtnOk do
    begin
      Parent := FDlgForm;
      Kind := bkCustom;
      Caption := 'OK';
      ModalResult := mrOK;
      Default := True;
      Cancel := True;
      TabOrder := 2;
      SetBounds(272, 52, 75, 23);
    end;  //BtnOK
    FBtnCancel := TBitbtn.Create(Self);
    with FBtnCancel do
    begin
      Parent := FDlgForm;
      Kind := bkCustom;
      Caption := 'Cancel';
      ModalResult := mrCancel;
      Cancel := True;
      TabOrder := 3;
      SetBounds(272, 116, 75, 23);
    end;  //BtnCancel
    FBtnBrowse := TButton.Create(Self);
    With FBtnBrowse do
    begin
      Parent := FDlgForm;
      Caption := 'Browse';
//      ModalResult := mrBrowse;
      Cancel := False;
      TabOrder := 4;
      SetBounds(272, 84, 75, 23);
      OnClick := BrowseClick;
    end;  //BtnBrowse
    //Create CheckBox
    FCBReconnect := TCheckBox.Create(FDlgForm);
    with FCBReconnect do
    begin
      parent := FDlgForm;
      Setbounds(60, 116, 141, 21);
      Caption := 'Reconnect at logon';
      Checked := True;
    end;  //CBReconnect
  end;  //FDlgForm
  CreateDrvLtrList;
end;  {= Constructor =}

destructor TNetPathDlg.Destroy;
begin
  FDrvLtrList.Free;
  inherited Destroy;
end;  {= Destroy =}

procedure TNetPathDlg.CreateWnd;
begin
  inherited createWnd;
  FCbxDrvLetter.Items.Assign(FDrvLtrList);
end;  {= CreateWnd =}

procedure TNetPathDlg.BrowseClick(Sender: TObject);
var
  DrvDirPath: string;
begin
  if BrowseForFolder(Application.Handle, DrvDirPath, 6, False) then    //880
  begin
    FNetPath := DelSlash(DrvDirPath);
    FEdtNetPath.Text := FNetPath;
  end;
end;  {= BrowseClick =}

procedure TNetPathDlg.CreateDrvLtrList;
var
  nDrv, FreeDrv, i: integer;
  cDrv: string;
  Drives: set of 0..25;
begin
  nDrv := 0;   //880
  FDrvLtrList := TStringList.Create;
  integer(Drives) := GetLogicalDrives;
  for i := 0 to 25 do
    if i in Drives then
      nDrv := i;
  cDrv := Char(nDrv + Ord('B'));
  FDrvLetter := cDrv + ':';
  FreeDrv := nDrv + 1;

  FDrvLtrList.BeginUpdate;
  for i := FreeDrv to 25 do
  begin
    cDrv := Char(i + Ord('A'));
    FDrvLtrList.Add(cDrv + ':');
  end;
  FCbxDrvLetter.Items.Assign(FDrvLtrList);
end;   {= CreateDrvLtrList =}

function TNetPathDlg.Execute: Boolean;
begin
//  TNetPathDlg.Create(Application);
//  CreateDrvLtrList;
  FCbxDrvLetter.Text := FDrvLetter;
  FEdtNetPath.Text := NetPath;
  FDlgForm.ShowModal;
  if FCBReconnect.Checked then
    FReconnect := True
  else
    FReconnect := False;
  Result := FDlgForm.ModalResult = mrOK;
end;  {= Execute =}

procedure TNetPathDlg.SetDrvLetter(Value: String);
begin
  if FDrvLetter <> Value then
    FDrvLetter := Value;
end;  {= SetDrvLetter =}

procedure TNetPathDlg.SetNetPath(Value: String);
begin
  if FNetPath <> Value then
    FNetPath := Value;
end;  {= SetNetPath =}   //880>

//=============== End of TNetPathDlg ===================//



{*********************************************************}
{                        Register                         }
{*********************************************************}

procedure Register;
begin
  RegisterComponents('LsComp', [TLsDirTree21]);
  RegisterComponents('LsComp', [TLsDirTreeCombo28]);
  RegisterComponents('LsComp', [TLsFilelistView28]);
end;


end.




