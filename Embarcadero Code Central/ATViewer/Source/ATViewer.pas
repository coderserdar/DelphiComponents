{************************************************}
{                                                }
{  ATViewer Component                            }
{  Copyright (C) 2006-09 Alexey Torgashin        }
{  http://atorg.net.ru                           }
{  support@uvviewsoft.com                        }
{                                                }
{************************************************}

{$OPTIMIZATION OFF} //Delphi 5 cannot compile this with optimization on.
{$BOOLEVAL OFF}    //Short boolean evaluation required.
{$RANGECHECKS OFF} //For LOWORD/HIWORD functions to work.

{$I Compilers.inc}      //Compilers defines
{$I ATViewerOptions.inc} //ATViewer options

{$ifdef MEDIA_WMP64} {$define MEDIA_WMP} {$endif}
{$ifdef MEDIA_WMP9} {$define MEDIA_WMP} {$endif}

unit ATViewer;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics,
  StdCtrls, ExtCtrls, ComCtrls, Forms, Menus, Jpeg,
  {$ifdef TNT} TntExtCtrls, {$endif}
  {$ifdef WLX} WLXProc, {$endif}
  {$ifdef MEDIA_PLAYER} MPlayer, {$endif}
  {$ifdef MEDIA_WMP64} MediaPlayer_TLB, {$endif}
  {$ifdef MEDIA_WMP9} MediaPlayer9_TLB, {$endif}
  {$ifdef MEDIA_WMP} ActiveX, {$endif}
  {$ifdef IE4X} WebBrowser4_TLB, {$else} SHDocVw, {$endif}
  {$ifdef PRINT} Dialogs, {$endif}
  {$ifdef SEARCH} ATStreamSearch, {$endif}
  {$ifdef PREVIEW} ATPrintPreview, ATxPrintProc, {$endif}
  ATBinHex,
  ATImageBox,
  ATxCodepages,
  ATxREProc;

type
  TATViewerMode = (
    vmodeNone,
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode,
    vmodeRTF,
    vmodeMedia,
    vmodeWeb
    {$ifdef WLX}, vmodeWLX {$endif}
    );

  TATViewerModes = set of TATViewerMode;

  TATViewerMediaMode = (
    vmmodeNone
    {$ifdef MEDIA_PLAYER}, vmmodeMCI {$endif}
    {$ifdef MEDIA_WMP64}, vmmodeWMP64 {$endif}
    {$ifdef MEDIA_WMP9}, vmmodeWMP9 {$endif}
    );

const
  cATViewerMediaModeNames: array[TATViewerMediaMode] of AnsiString = (
    ''
    {$ifdef MEDIA_PLAYER}, 'MCI'                     {$endif}
    {$ifdef MEDIA_WMP64}, 'Windows Media Player 6.4' {$endif}
    {$ifdef MEDIA_WMP9}, 'Windows Media Player 9.0' {$endif}
    );

const
  vmmodeDefault =
    {$ifdef MEDIA_WMP64}
    vmmodeWMP64
    {$else}
      {$ifdef MEDIA_PLAYER}
      vmmodeMCI
      {$else}
        {$ifdef MEDIA_WMP9}
        vmmodeWMP9
        {$else}
        vmmodeNone
        {$endif}
      {$endif}
    {$endif}
    ;

type
  TATViewerImageEffect = (
    vieRotate90,
    vieRotate270,
    vieGrayscale,
    vieNegative,
    vieFlipVertical,
    vieFlipHorizontal
    );

{$ifdef IVIEW}
type
  TATIViewIntegration = record
    Enabled: Boolean;
    ExeName: AnsiString; //WideString currently not supported
    ExtList: AnsiString;
    HighPriority: Boolean;
  end;
{$endif}

{$ifdef IJL}
type
  TATIJLIntegration = record
    Enabled: Boolean;
    ExtList: AnsiString;
  end;
{$endif}

type
  TATViewerLoadImageStream = procedure(Sender: TObject;
    AImageBox: TATImageBox; AStream: TStream) of object;
  TATViewerLoadWebStream = procedure(Sender: TObject;
    ABrowser: TWebBrowser; AStream: TStream) of object;

type
  TATViewer = class(TPanel)
  private
    FFileName: WideString;
    FFileSize: Int64;
    FFileTime: TFileTime;
    FStream: TStream;
    FBinHex: TATBinHex;
    FTextPanel: TPanel;
    FImageBox: TATImageBox;
    FImageUpDown: TUpDown;
    FEdit: TRichEdit;
    FEditMenu: TPopupMenu;
    FEditMenuItemCopy: TMenuItem;
    FEditMenuItemSelectAll: TMenuItem;
    FEditMenuItemSep: TMenuItem;

    {$ifdef MEDIA_PLAYER}
    FMedia: TMediaPlayer;
    FMediaPanel: TPanel;
    FMediaPanel1: TPanel;
    FMediaPanel2: TPanel;
    FMediaBar: TTrackBar;
    FMediaTimer: TTimer;
    FMediaTimerBusy: Boolean;
    {$endif}

    {$ifdef MEDIA_WMP64}
    FWMP6: TWMP;
    FWMP6Controls: Boolean;
    FWMP6Tracker: Boolean;
    {$endif}

    {$ifdef MEDIA_WMP9}
    FWMP9: TWMP9;
    {$endif}

    FBrowser: TWebBrowser;

    {$ifdef WLX}
    FPlugins: TWlxPlugins;
    FPluginsHighPriority: Boolean;
    {$endif}

    FMediaEndTimer: TTimer;

    {$ifdef PRINT}
    FPrintDialog: TPrintDialog;
    FPageSetupDialog: {$ifdef COMPILER_7_UP} TPageSetupDialog {$else} TPrintDialog {$endif};
    {$endif}

    {$ifdef IVIEW}
    FIViewIntegration: TATIViewIntegration;
    FIViewObject: TObject;
    {$endif}

    {$ifdef IJL}
    FIJLIntegration: TATIJLIntegration;
    {$endif}

    FMode: TATViewerMode;
    FModeDetect: Boolean;
    FModeUndetected: TATViewerMode;
    FModeUndetectedCfm: Boolean;
    FModesDisabledForDetect: TATViewerModes;
    FMediaMode: TATViewerMediaMode;
    FSourceType: TATFileSource;
    FTextEncoding: TATEncoding;
    FTextWrap: Boolean;
    FTextDetect: Boolean;
    FTextDetectSize: DWORD;
    FTextDetectLimit: DWORD;
    FTextDetectOEM: Boolean;
    FMediaAutoPlay: Boolean;
    FMediaLoop: Boolean;
    FMediaPlayCount: Integer;
    FMediaPlaylistPause: Integer;
    FMediaFit: Boolean;
    FMediaFitOnlyBig: Boolean;
    FMediaCenter: Boolean;
    FMediaVolume: Integer;
    FMediaMute: Boolean;
    {$ifdef OFFLINE}
    FWebOffline: Boolean;
    {$endif}
    FWebAcceptAllFiles: Boolean;
    FWebWaitForNavigate: Boolean;
    FTextColor: TColor;
    FTextAutoCopy: Boolean;
    FFocused: Boolean;
    FBorderStyleInner: TBorderStyle;

    {$ifdef SEARCH}
    FFindText: WideString;
    FFindOptions: TATStreamSearchOptions;
    FFindFinished: Boolean;
    FEditLastSearch: TRELastSearch;
    {$endif}

    FIsImage: Boolean;       //Image is currently loaded
    FIsImageBefore: Boolean; //Image was loaded before the last FreeData call
    FIsImageIView: Boolean;  //Image was loaded using IrfanView/XnView
    FIsImageIJL: Boolean;    //Image was loaded using IJL
    FIsIcon: Boolean;        //Icon is currently loaded
    FIsMetafile: Boolean;    //Metafile is currently loaded
    FIsMedia: Boolean;       //Media clip is currently loaded

    FImageBPP: Integer;
    FImageColor: TColor;
    FImageTransparent: Boolean;
    FImageResample: Boolean;
    FImageKeepPosition: Boolean;
    FImageDrag: Boolean;
    FImageCursor: TCursor;
    FImageDragCursor: TCursor;
    FImageError: Boolean;
    FImageErrorMessage: AnsiString;
    FImageErrorMessageBox: Boolean;
    FImagePage: Integer;
    FImagePagesCount: Integer;

    FSearchIndentVert: Integer;
    FSearchIndentHorz: Integer;

    FOnMediaPlaybackEnd: TNotifyEvent;
    FOnWebDocumentComplete: TNotifyEvent;
    FOnWebNavigateComplete: TNotifyEvent;
    FOnWebStatusTextChange: TWebBrowserStatusTextChange;
    FOnWebTitleChange: TWebBrowserTitleChange;
    FOnFileUnload: TNotifyEvent;
    FOnFileLoad: TNotifyEvent;
    FOnOptionsChange: TNotifyEvent;
    FOnLoadImageStream: TATViewerLoadImageStream;
    FOnLoadWebStream: TATViewerLoadWebStream;

    {$ifdef PRINT}
    procedure InitDialogs;
    {$endif}

    procedure InitEdit;
    procedure InitImage;
    procedure InitMediaEndTimer;
    procedure InitMedia;
    procedure InitWeb;
    procedure FreeMedia;
    function CanSetFocus: Boolean;
    procedure SetMode(AValue: TATViewerMode);
    procedure SetMediaMode(AValue: TATViewerMediaMode);
    function GetTextEncoding: TATEncoding;
    procedure SetTextEncoding(AValue: TATEncoding);
    procedure SetTextWrap(AValue: Boolean);
    function GetTextWidth: Integer;
    function GetTextWidthHex: Integer;
    function GetTextWidthFit: Boolean;
    function GetTextWidthFitHex: Boolean;
    function GetTextWidthFitUHex: Boolean;
    function GetTextOemSpecial: Boolean;
    function GetTextNonPrintable: Boolean;
    function GetTextUrlHilight: Boolean;

    function GetTextGutter: Boolean;
    function GetTextGutterLines: Boolean;
    function GetTextGutterLinesStep: Integer;
    function GetTextGutterLinesBufSize: Integer;
    function GetTextGutterLinesCount: Integer;
    function GetTextGutterLinesExtUse: Boolean;
    function GetTextGutterLinesExtList: AnsiString;

    procedure SetTextWidth(AValue: Integer);
    procedure SetTextWidthHex(AValue: Integer);
    procedure SetTextWidthFit(AValue: Boolean);
    procedure SetTextWidthFitHex(AValue: Boolean);
    procedure SetTextWidthFitUHex(AValue: Boolean);
    procedure SetTextOemSpecial(AValue: Boolean);
    procedure SetTextNonPrintable(AValue: Boolean);
    procedure SetTextUrlHilight(AValue: Boolean);

    procedure SetTextGutter(AValue: Boolean);
    procedure SetTextGutterLines(AValue: Boolean);
    procedure SetTextGutterLinesStep(AValue: Integer);
    procedure SetTextGutterLinesBufSize(AValue: Integer);
    procedure SetTextGutterLinesCount(AValue: Integer);
    procedure SetTextGutterLinesExtUse(AValue: Boolean);
    procedure SetTextGutterLinesExtList(const AValue: AnsiString);

    procedure SetSearchIndentVert(AValue: Integer);
    procedure SetSearchIndentHorz(AValue: Integer);
    procedure SetMediaPosition;
    procedure SetMediaLoop(AValue: Boolean);
    procedure SetMediaFit(AValue: Boolean);
    procedure SetMediaFitOnlyBig(AValue: Boolean);
    procedure SetMediaCenter(AValue: Boolean);
    {$ifdef OFFLINE}
    procedure SetWebOffline(AValue: Boolean);
    {$endif}
    procedure SetTextColor(AValue: TColor);
    procedure SetTextFont(AValue: TFont);
    procedure SetTextFontOEM(AValue: TFont);
    procedure SetTextFontFooter(AValue: TFont);
    procedure SetTextFontGutter(AValue: TFont);
    function GetTextFont: TFont;
    function GetTextFontOEM: TFont;
    function GetTextFontFooter: TFont;
    function GetTextFontGutter: TFont;
    function GetTextColorHex: TColor;
    function GetTextColorHex2: TColor;
    function GetTextColorHexBack: TColor;
    function GetTextColorLines: TColor;
    function GetTextColorError: TColor;
    function GetTextColorGutter: TColor;
    function GetTextColorURL: TColor;
    procedure SetTextColorHex(AValue: TColor);
    procedure SetTextColorHex2(AValue: TColor);
    procedure SetTextColorHexBack(AValue: TColor);
    procedure SetTextColorLines(AValue: TColor);
    procedure SetTextColorError(AValue: TColor);
    procedure SetTextColorGutter(AValue: TColor);
    procedure SetTextColorURL(AValue: TColor);

    procedure DetectMode;
    procedure LoadRTF;
    procedure LoadRTFStream;
    procedure LoadBinary;
    procedure LoadBinaryStream;
    procedure LoadImage(APicture: TPicture = nil; ANewImage: Boolean = True);
    procedure LoadMedia(APicture: TPicture = nil);
    procedure UnloadImage;
    procedure ShowImageError(const Msg: AnsiString);
    procedure LoadImageStream;
    procedure LoadWeb;
    procedure LoadWebStream;

    {$ifdef WLX}
    function LoadWLX: Boolean;
    procedure HideWLX;
    procedure SendWLXCommand(ACmd, AParam: Integer);
    procedure SendWLXParams;
    function GetPluginsBeforeLoading: TWlxNameEvent;
    function GetPluginsAfterLoading: TWlxNameEvent;
    procedure SetPluginsBeforeLoading(AProc: TWlxNameEvent);
    procedure SetPluginsAfterLoading(AProc: TWlxNameEvent);
    function OpenByPlugins(AFileIsNew: Boolean): Boolean;
    function GetActivePluginSupportsSearch: Boolean;
    function GetActivePluginSupportsPrint: Boolean;
    function GetActivePluginSupportsCommands: Boolean;
    function GetActivePluginWindowHandle: THandle;
    procedure CloseActivePlugin;
    {$endif}

    procedure FreeSearch;
    procedure FreeData(AFreeImage: Boolean = True);
    procedure HideAll;
    procedure HideEdit;
    procedure HideImage;
    procedure HideMedia;
    procedure HideWeb;
    procedure Enter(Sender: TObject);
    procedure ImageUpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure ImageBoxScroll(Sender: TObject);
    procedure ImageBoxScrollAlt(Sender: TObject; Inc: Boolean);

    {$ifdef SEARCH}
    function GetSearchStarted: Boolean;
    function GetOnTextSearchProgress: TATStreamSearchProgress;
    procedure SetOnTextSearchProgress(AValue: TATStreamSearchProgress);
    {$endif}

    {$ifdef MEDIA_PLAYER}
    procedure MediaBarChange(Sender: TObject);
    procedure MediaTimerTimer(Sender: TObject);
    procedure MediaNotify(Sender: TObject);
    {$endif}

    procedure WebBrowserDocumentComplete(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
    procedure WebBrowserNavigateComplete2(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
    procedure WebBrowserStatusTextChange(Sender: TObject; const Text: WideString);
    procedure WebBrowserTitleChange(Sender: TObject; const Text: WideString);
    {$ifdef IE4X}
    procedure WebBrowserFileDownload(Sender: TObject; ActiveDocument: WordBool; var Cancel: WordBool);
    {$endif}
    function WebBrowserSafe: Boolean;
    function GetWebBusy: Boolean;

    {$ifdef PRINT}
    function PrinterCaption: AnsiString;
    procedure PrintEdit(ASelectionOnly: Boolean; ACopies: Integer);
    {$endif}

    function GetActivePluginName: AnsiString;
    function GetPosPercent: Integer;
    procedure SetPosPercent(APos: Integer);
    function GetPosOffset: Int64;
    procedure SetPosOffset(const APos: Int64);

    procedure EditMenuItemCopyClick(Sender: TObject);
    procedure EditMenuItemSelectAllClick(Sender: TObject);
    procedure TextURLClick(Sender: TObject; const S: AnsiString);
    procedure TextSelectionChange(Sender: TObject);
    procedure TextPanelClick(Sender: TObject);
    function DetectTextAndUnicode: Boolean;

    {$ifdef MEDIA_WMP64}
    procedure SetMediaFit_WMP6(WMP: TWMP);
    procedure PlayStateChange_WMP6(Sender: TObject; OldState: Integer; NewState: Integer);
    {$endif}

    {$ifdef MEDIA_WMP9}
    procedure SetMediaFit_WMP9(WMP: TWMP9);
    procedure PlayStateChange_WMP9(Sender: TObject; NewState: Integer);
    {$endif}

    procedure MediaEndTimerTimer(Sender: TObject);
    procedure PreparePlaybackEnd;
    procedure DoPlaybackEnd;
    procedure DoWebDocumentComplete;
    procedure DoWebNavigateComplete;
    procedure DoWebStatusTextChange(const Text: WideString);
    procedure DoWebTitleChange(const Text: WideString);
    procedure DoFileUnload;
    procedure DoFileLoad;
    procedure DoOptionsChange;
    procedure DoLoadImageStream;
    procedure DoLoadWebStream;

    {$ifdef NOTIF}
    function GetTextAutoReload: Boolean;
    function GetTextAutoReloadBeep: Boolean;
    function GetTextAutoReloadFollowTail: Boolean;
    procedure SetTextAutoReload(AValue: Boolean);
    procedure SetTextAutoReloadBeep(AValue: Boolean);
    procedure SetTextAutoReloadFollowTail(AValue: Boolean);
    function GetOnTextFileReload: TNotifyEvent;
    procedure SetOnTextFileReload(AEvent: TNotifyEvent);
    {$endif}

    procedure SetTextPopupCaption(AIndex: TATPopupCommand; const AValue: AnsiString);
    function GetTextTabSize: Integer;
    procedure SetTextTabSize(AValue: Integer);
    function GetTextPopupCommands: TATPopupCommands;
    procedure SetTextPopupCommands(AValue: TATPopupCommands);
    function GetTextMaxLengths(AIndex: TATBinHexMode): Integer;
    procedure SetTextMaxLengths(AIndex: TATBinHexMode; AValue: Integer);
    function GetTextEncodingName: AnsiString;
    procedure FocusWebBrowser;
    function GetSelStart: Int64;
    procedure SetSelStart(const AValue: Int64);
    function GetSelLength: Int64;
    procedure SetSelLength(const AValue: Int64);
    function GetSelText: AnsiString;
    function GetSelTextShort: AnsiString;
    function GetSelTextW: WideString;
    function GetSelTextShortW: WideString;
    function GetImageDrag: Boolean;
    procedure SetImageDrag(AValue: Boolean);
    function GetImageCursor: TCursor;
    procedure SetImageCursor(AValue: TCursor);
    function GetImageDragCursor: TCursor;
    procedure SetImageDragCursor(AValue: TCursor);
    function GetImageWidth: Integer;
    function GetImageHeight: Integer;
    function GetImageBPP: Integer;
    function GetImageScale: Integer;
    procedure SetImageScale(AValue: Integer);
    function GetMediaFit: Boolean;
    function GetMediaFitOnlyBig: Boolean;
    function GetMediaCenter: Boolean;
    function GetMediaShowControls: Boolean;
    procedure SetMediaShowControls(AValue: Boolean);
    function GetMediaShowTracker: Boolean;
    procedure SetMediaShowTracker(AValue: Boolean);
    function GetTextMaxClipboardDataSizeMb: Integer;
    procedure SetTextMaxClipboardDataSizeMb(AValue: Integer);
    function GetMediaVolume: Integer;
    procedure SetMediaVolume(AValue: Integer);
    function GetMediaMute: Boolean;
    procedure SetMediaMute(AValue: Boolean);
    procedure SetOnOptionsChange(AEvent: TNotifyEvent);
    procedure MediaSyncVolume;
    function ActualExtImages: AnsiString;
    procedure SetPosLine(ALine: Integer);
    function GetPosLine: Integer;

    {$ifdef PRINT}
    function GetMarginLeft: Extended;
    function GetMarginTop: Extended;
    function GetMarginRight: Extended;
    function GetMarginBottom: Extended;
    function GetPrintFooter: Boolean;
    procedure SetMarginLeft(const AValue: Extended);
    procedure SetMarginTop(const AValue: Extended);
    procedure SetMarginRight(const AValue: Extended);
    procedure SetMarginBottom(const AValue: Extended);
    procedure SetPrintFooter(const AValue: Boolean);
    function MarginsRectPx: TRect;
    {$endif}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open(const AFileName: WideString; APicture: TPicture = nil): Boolean;
    function OpenStream(AStream: TStream; AMode: TATViewerMode): Boolean;
    procedure Reload;
    procedure CopyToClipboard(AsHex: Boolean = False);
    procedure SelectAll;
    procedure SelectNone;
    procedure webWait;

    {$ifdef SEARCH}
    function FindFirst(const AText: WideString; AOptions: TATStreamSearchOptions): Boolean;
    function FindNext(AFindPrevious: Boolean = False): Boolean;
    function FindDialog(AFindNext: Boolean): Boolean;
    {$endif}

    {$ifdef PRINT}
    procedure PrintDialog;
    procedure PrintPreview;
    procedure PrintSetup;
    {$endif}
    {$ifdef PREVIEW}
    function PrintOptions(ACopies: Integer; AFailOnErrors: Boolean): TATPrintOptions;
    {$endif}

    procedure FocusActiveControl;
    procedure TextScroll(const APos: Int64; AIndentVert, AIndentHorz: Integer);
    procedure TextPopupMenu(AX, AY: Integer);
    procedure TextEncodingsMenu(AX, AY: Integer);
    function ImageEffect(AEffect: TATViewerImageEffect): Boolean;
    procedure WebGoBack;
    procedure WebGoForward;

    {$ifdef WLX}
    function OpenFolder(const AFolderName: WideString): Boolean;
    procedure InitPluginsParams(AParent: TWinControl; const AIniFilename: AnsiString);
    procedure ResizeActivePlugin(const Rect: TRect);
    procedure RemovePlugins;
    function AddPlugin(const AFileName: TWlxFilename; const ADetectStr: TWlxDetectString): Boolean;
    function GetPlugin(AIndex: Word; var AFileName: TWlxFilename; var ADetectStr: TWlxDetectString): Boolean;
    property ActivePluginSupportsSearch: Boolean read GetActivePluginSupportsSearch;
    property ActivePluginSupportsPrint: Boolean read GetActivePluginSupportsPrint;
    property ActivePluginSupportsCommands: Boolean read GetActivePluginSupportsCommands;
    property ActivePluginWindowHandle: THandle read GetActivePluginWindowHandle;
    property PluginsHighPriority: Boolean read FPluginsHighPriority write FPluginsHighPriority;
    procedure PluginsSendMessage(const AMessage: TMessage);
    {$endif}

    property ActivePluginName: AnsiString read GetActivePluginName;
    procedure IncreaseScale(AIncrement: Boolean);
    property FileName: WideString read FFileName;
    property FileSize: Int64 read FFileSize;
    property FileTime: TFileTime read FFileTime;
    property IsImage: Boolean read FIsImage;
    property IsIcon: Boolean read FIsIcon;
    property IsMetafile: Boolean read FIsMetafile;
    property IsMedia: Boolean read FIsMedia;
    property BinHex: TATBinHex read FBinHex;
    property ImageBox: TATImageBox read FImageBox;
    property ImageWidth: Integer read GetImageWidth;
    property ImageHeight: Integer read GetImageHeight;
    property ImageBPP: Integer read FImageBPP; //saved on image reading

    property ImageScale: Integer read GetImageScale write SetImageScale;
    property ImageError: Boolean read FImageError;
    property ImageErrorMessage: AnsiString read FImageErrorMessage;

    procedure ImageScaleInc;
    procedure ImageScaleDec;
    procedure MediaDoPlayPause;
    property MediaVolume: Integer read GetMediaVolume write SetMediaVolume;
    property MediaMute: Boolean read GetMediaMute write SetMediaMute;
    property WebBusy: Boolean read GetWebBusy;

    property PosPercent: Integer read GetPosPercent write SetPosPercent;
    property PosOffset: Int64 read GetPosOffset write SetPosOffset;
    property PosLine: Integer read GetPosLine write SetPosLine;

    {$ifdef SEARCH}
    property SearchStarted: Boolean read GetSearchStarted;
    property SearchFinished: Boolean read FFindFinished;
    {$endif}

    {$ifdef PRINT}
    property MarginLeft: Extended read GetMarginLeft write SetMarginLeft;
    property MarginTop: Extended read GetMarginTop write SetMarginTop;
    property MarginRight: Extended read GetMarginRight write SetMarginRight;
    property MarginBottom: Extended read GetMarginBottom write SetMarginBottom;
    property PrintFooter: Boolean read GetPrintFooter write SetPrintFooter;
    {$endif}

    property TextPopupCaption[AIndex: TATPopupCommand]: AnsiString write SetTextPopupCaption;
    property TextMaxLengths[AIndex: TATBinHexMode]: Integer read GetTextMaxLengths write SetTextMaxLengths;
    property TextMaxClipboardDataSizeMb: Integer read GetTextMaxClipboardDataSizeMb write SetTextMaxClipboardDataSizeMb;
    property TextSelStart: Int64 read GetSelStart write SetSelStart;
    property TextSelLength: Int64 read GetSelLength write SetSelLength;
    property TextSelText: AnsiString read GetSelText;
    property TextSelTextShort: AnsiString read GetSelTextShort;
    property TextSelTextW: WideString read GetSelTextW;
    property TextSelTextShortW: WideString read GetSelTextShortW;
    property TextEncodingName: AnsiString read GetTextEncodingName;

  protected
    procedure Click; override;
    procedure Resize; override;

  published
    property Mode: TATViewerMode read FMode write SetMode default vmodeText;
    property ModeUndetected: TATViewerMode read FModeUndetected write FModeUndetected default vmodeBinary;
    property ModeUndetectedCfm: Boolean read FModeUndetectedCfm write FModeUndetectedCfm default True;
    property ModeDetect: Boolean read FModeDetect write FModeDetect default True;
    property ModesDisabledForDetect: TATViewerModes read FModesDisabledForDetect write FModesDisabledForDetect default [];

    property TextDetect: Boolean read FTextDetect write FTextDetect default True;
    property TextDetectOEM: Boolean read FTextDetectOEM write FTextDetectOEM default True;
    property TextDetectSize: DWORD read FTextDetectSize write FTextDetectSize default 2;
    property TextDetectLimit: DWORD read FTextDetectLimit write FTextDetectLimit default 0;
    property TextEncoding: TATEncoding read GetTextEncoding write SetTextEncoding default vencANSI;
    property TextWrap: Boolean read FTextWrap write SetTextWrap default False;
    property TextWidth: Integer read GetTextWidth write SetTextWidth default 80;
    property TextWidthHex: Integer read GetTextWidthHex write SetTextWidthHex default 16;
    property TextWidthFit: Boolean read GetTextWidthFit write SetTextWidthFit default False;
    property TextWidthFitHex: Boolean read GetTextWidthFitHex write SetTextWidthFitHex default False;
    property TextWidthFitUHex: Boolean read GetTextWidthFitUHex write SetTextWidthFitUHex default False;
    property TextOemSpecial: Boolean read GetTextOemSpecial write SetTextOemSpecial default False;
    property TextUrlHilight: Boolean read GetTextUrlHilight write SetTextUrlHilight default True;

    property TextGutter: Boolean read GetTextGutter write SetTextGutter default False;
    property TextGutterLines: Boolean read GetTextGutterLines write SetTextGutterLines default True;
    property TextGutterLinesStep: Integer read GetTextGutterLinesStep write SetTextGutterLinesStep default 5;
    property TextGutterLinesBufSize: Integer read GetTextGutterLinesBufSize write SetTextGutterLinesBufSize stored False;
    property TextGutterLinesCount: Integer read GetTextGutterLinesCount write SetTextGutterLinesCount stored False;
    property TextGutterLinesExtUse: Boolean read GetTextGutterLinesExtUse write SetTextGutterLinesExtUse default False;
    property TextGutterLinesExtList: AnsiString read GetTextGutterLinesExtList write SetTextGutterLinesExtList;

    property TextNonPrintable: Boolean read GetTextNonPrintable write SetTextNonPrintable default False;
    property TextSearchIndentVert: Integer read FSearchIndentVert write SetSearchIndentVert default 5;
    property TextSearchIndentHorz: Integer read FSearchIndentHorz write SetSearchIndentHorz default 5;
    property TextTabSize: Integer read GetTextTabSize write SetTextTabSize default 8;
    property TextColor: TColor read FTextColor write SetTextColor default clWindow;
    property TextColorHex: TColor read GetTextColorHex write SetTextColorHex default clNavy;
    property TextColorHex2: TColor read GetTextColorHex2 write SetTextColorHex2 default clBlue;
    property TextColorHexBack: TColor read GetTextColorHexBack write SetTextColorHexBack default cATBinHexBkColor;
    property TextColorLines: TColor read GetTextColorLines write SetTextColorLines default clGray;
    property TextColorError: TColor read GetTextColorError write SetTextColorError default clRed;
    property TextColorGutter: TColor read GetTextColorGutter write SetTextColorGutter default clLtGray;
    property TextColorURL: TColor read GetTextColorURL write SetTextColorURL default clBlue;
    property TextFont: TFont read GetTextFont write SetTextFont;
    property TextFontOEM: TFont read GetTextFontOEM write SetTextFontOEM;
    property TextFontFooter: TFont read GetTextFontFooter write SetTextFontFooter;
    property TextFontGutter: TFont read GetTextFontGutter write SetTextFontGutter;
    property TextAutoCopy: Boolean read FTextAutoCopy write FTextAutoCopy default False;
    property TextPopupCommands: TATPopupCommands read GetTextPopupCommands write SetTextPopupCommands default cATBinHexCommandSet;

    property MediaMode: TATViewerMediaMode read FMediaMode write SetMediaMode default vmmodeDefault;
    property MediaAutoPlay: Boolean read FMediaAutoPlay write FMediaAutoPlay default True;
    property MediaLoop: Boolean read FMediaLoop write SetMediaLoop;
    property MediaPlayCount: Integer read FMediaPlayCount write FMediaPlayCount default 1;
    property MediaPlaylistPause: Integer read FMediaPlaylistPause write FMediaPlaylistPause default 500;
    property MediaShowControls: Boolean read GetMediaShowControls write SetMediaShowControls;
    property MediaShowTracker: Boolean read GetMediaShowTracker write SetMediaShowTracker;
    property MediaFit: Boolean read GetMediaFit write SetMediaFit default True;
    property MediaFitOnlyBig: Boolean read GetMediaFitOnlyBig write SetMediaFitOnlyBig default True;
    property MediaCenter: Boolean read GetMediaCenter write SetMediaCenter default True;

    property ImageColor: TColor read FImageColor write FImageColor default clDkGray;
    property ImageDrag: Boolean read GetImageDrag write SetImageDrag default True;
    property ImageCursor: TCursor read GetImageCursor write SetImageCursor default crDefault;
    property ImageDragCursor: TCursor read GetImageDragCursor write SetImageDragCursor default crSizeAll;
    property ImageTransparent: Boolean read FImageTransparent write FImageTransparent default False;
    property ImageResample: Boolean read FImageResample write FImageResample default False;
    property ImageKeepPosition: Boolean read FImageKeepPosition write FImageKeepPosition default True;
    property ImageErrorMessageBox: Boolean read FImageErrorMessageBox write FImageErrorMessageBox default True;

    {$ifdef NOTIF}
    property TextAutoReload: Boolean read GetTextAutoReload write SetTextAutoReload default False;
    property TextAutoReloadBeep: Boolean read GetTextAutoReloadBeep write SetTextAutoReloadBeep default False;
    property TextAutoReloadFollowTail: Boolean read GetTextAutoReloadFollowTail write SetTextAutoReloadFollowTail default True;
    property OnTextFileReload: TNotifyEvent read GetOnTextFileReload write SetOnTextFileReload;
    {$endif}

    {$ifdef IVIEW}
    property IViewIntegration: TATIViewIntegration read FIViewIntegration write FIViewIntegration;
    {$endif}

    {$ifdef IJL}
    property IJLIntegration: TATIJLIntegration read FIJLIntegration write FIJLIntegration;
    {$endif}

    {$ifdef OFFLINE}
    property WebOffline: Boolean read FWebOffline write SetWebOffline default False;
    {$endif}

    property WebAcceptAllFiles: Boolean read FWebAcceptAllFiles write FWebAcceptAllFiles default False;
    property WebWaitForNavigate: Boolean read FWebWaitForNavigate write FWebWaitForNavigate default False;
    property IsFocused: Boolean read FFocused write FFocused default False;
    property BorderStyleInner: TBorderStyle read FBorderStyleInner write FBorderStyleInner default bsSingle;

    property OnMediaPlaybackEnd: TNotifyEvent read FOnMediaPlaybackEnd write FOnMediaPlaybackEnd;
    property OnWebDocumentComplete: TNotifyEvent read FOnWebDocumentComplete write FOnWebDocumentComplete;
    property OnWebNavigateComplete: TNotifyEvent read FOnWebNavigateComplete write FOnWebNavigateComplete;
    property OnWebStatusTextChange: TWebBrowserStatusTextChange read FOnWebStatusTextChange write FOnWebStatusTextChange;
    property OnWebTitleChange: TWebBrowserTitleChange read FOnWebTitleChange write FOnWebTitleChange;

    property OnFileUnload: TNotifyEvent read FOnFileUnload write FOnFileUnload;
    property OnFileLoad: TNotifyEvent read FOnFileLoad write FOnFileLoad;
    property OnOptionsChange: TNotifyEvent read FOnOptionsChange write SetOnOptionsChange;
    property OnLoadImageStream: TATViewerLoadImageStream read FOnLoadImageStream write FOnLoadImageStream;
    property OnLoadWebStream: TATViewerLoadWebStream read FOnLoadWebStream write FOnLoadWebStream;

    {$ifdef SEARCH}
    property OnTextSearchProgress: TATStreamSearchProgress read GetOnTextSearchProgress write SetOnTextSearchProgress;
    {$endif}

    {$ifdef WLX}
    property OnPluginsBeforeLoading: TWlxNameEvent read GetPluginsBeforeLoading write SetPluginsBeforeLoading;
    property OnPluginsAfterLoading: TWlxNameEvent read GetPluginsAfterLoading write SetPluginsAfterLoading;
    {$endif}
  end;


type
  TATViewerGlobalOptions = record
    ExtText,
    ExtRTF,
    ExtImages,
    ExtMedia,
    ExtWeb: AnsiString;
  end;

var
  ATViewerOptions: TATViewerGlobalOptions;

procedure ATViewerOptionsReset;


procedure Register;


implementation

uses
  Clipbrd,
  {$ifdef TNT} TntClasses, {$endif}
  {$ifdef WLX} WLXPlugin, {$endif}
  {$ifdef IVIEW} nmzIrfanXnView, {$endif}
  {$ifdef GEX} GraphicEx, {$endif}
  {$ifdef GIF} GIFImage, {$endif}
  {$ifdef PNG} PNGImage, {$endif}
  {$ifdef IJL} IJL, JPEG_IO, {$endif}
  {$ifdef JP2} jp2img, {$endif}
  ATxSProc, ATxFProc, ATxWBProc, ATxREUrl,
  ATxImageProc, ATViewerMsg, ATxPanel;

{ Helper functions }

{$I ATViewerExt.inc}

procedure DoCursorHours;
begin
  Screen.Cursor := crHourGlass;
end;

procedure DoCursorDefault;
begin
  Screen.Cursor := crDefault;
end;

{$ifdef SEARCH}
function ATSSOptionsToREOptions(AOptions: TATStreamSearchOptions): TSearchTypes;
begin
  Result := [];
  if asoWholeWords in AOptions then
    Include(Result, stWholeWord);
  if asoCaseSens in AOptions then
    Include(Result, stMatchCase);
end;
{$endif}

{
Volume property:

WMP 6.4:
http://msdn.microsoft.com/msdnmag/issues/01/02/web/
As it turns out, the Windows Media Player object exposes a volume property.
It's a little strange and awkward to use (in version 6.4 of the Media Player).
Max volume is 0, min volume is -10000, and the perceived volume ramp is not
a linear progression between those two values.

Note that if you display the default Windows Media Player controls and the
user modifies the volume with the displayed slider, this value does not appear
to get reported back via that volume property (in version 6.4).

WMP 7+:
Zero specifies no volume and 100 specifies full volume. If no value
is specified for this property, it defaults to the last volume setting
established for the player.

ATViewer:
Let's always use the volume range from 0 to 10. WMP 6.4 visual control also
has this range. And, for example, Media Player Classic has the range 0 to 13.
}

const
  cVolMin = 0;
  cVolMax = 10;

var
  //Approximate volume values that form the linear scale:
  cVolW6: array[cVolMin .. cVolMax] of Integer =
    //(-10000, -1500, -1200, -980, -760, -570, -420, -300, -200, -100, 0);
    (-9640, -2000, -1440, -1070, -810, -610, -450, -320, -200, -100, 0);

function Vol_W6toA(AValue: Integer): Integer;
var
  i: Integer;
begin
  Result:= cVolMin;
  for i := cVolMin to cVolMax do
    if cVolW6[i] >= AValue then
    begin
      Result:= i;
      Break
    end;
end;

function Vol_AtoW6(AValue: Integer): Integer;
begin
  ILimitMin(AValue, cVolMin);
  ILimitMax(AValue, cVolMax);
  Result:= cVolW6[AValue];
end;

function Vol_W9toA(AValue: Integer): Integer;
begin
  Result:= AValue div 10;
end;

function Vol_AtoW9(AValue: Integer): Integer;
begin
  ILimitMin(AValue, cVolMin);
  ILimitMax(AValue, cVolMax);
  Result:= AValue * 10;
end;


{ TATViewer }

function TATViewer.CanSetFocus: Boolean;
begin
  Result :=
    CanFocus and
    (FFocused or Focused) and
    ([csLoading, csDesigning] * ComponentState = [])
end;

constructor TATViewer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  //Init inherited properties
  Caption := '';
  BevelOuter := bvNone;
  Width := 200;
  Height := 150;

  //Init variables
  FFileName := '';
  FFileSize := 0;
  FillChar(FFileTime, SizeOf(TFileTime), 0);

  FStream := nil;
  FSourceType := vfSrcNone;
  FMode := vmodeText;
  FModeDetect := True;
  FModeUndetected := vmodeBinary;
  FModeUndetectedCfm := True;
  FModesDisabledForDetect := [];
  FMediaMode := vmmodeDefault;
  FTextColor := clWindow;
  FTextEncoding := vencANSI;
  FTextWrap := False;
  FTextDetect := True;
  FTextDetectOEM := True;
  FTextDetectSize := 2;
  FTextDetectLimit := 0;

  FMediaAutoPlay := True;
  FMediaLoop := False;
  FMediaPlayCount := 1;
  FMediaPlaylistPause := 500;
  FMediaFit := True;
  FMediaFitOnlyBig := True;
  FMediaCenter := True;
  FMediaVolume := Pred(Pred(cVolMax));
  FMediaMute := False;

  {$ifdef OFFLINE}
  FWebOffline := False;
  {$endif}
  FWebAcceptAllFiles := False;
  FWebWaitForNavigate := False;

  FSearchIndentVert := 5;
  FSearchIndentHorz := 5;

  FFocused := False;
  FBorderStyleInner := bsSingle;

  FIsImage := False;
  FIsImageBefore := False;
  FIsImageIView := False;
  FIsImageIJL := False;
  FIsIcon := False;
  FIsMetafile := False;
  FIsMedia := False;

  FImageBPP := 0;
  FImageColor := clDkGray;
  FImageTransparent := False;
  FImageResample:= False;
  FImageKeepPosition := True;
  FImageDrag := True;
  FImageCursor := crDefault;
  FImageDragCursor := crSizeAll;
  FImageError := False;
  FImageErrorMessage := '';
  FImageErrorMessageBox := True;
  FImagePage := 0;
  FImagePagesCount := 1;

  //Init event handlers
  OnEnter := Enter;

  //Init objects
  FBinHex := TATBinHex.Create(Self);
  with FBinHex do
  begin
    Width := 1; //To "hide" control initially during text loading
    Height := 1;
    Parent := Self;
    Align := alClient;
    Color := FTextColor;
    OnSelectionChange := TextSelectionChange;
    OnClickURL := TextURLClick;
  end;

  FTextPanel := TTextPanel.Create(Self);
  with FTextPanel do
  begin
    Parent := Self;
    Align := alTop;
    Visible := False;
    TTextPanel(FTextPanel).OnLabClick := TextPanelClick;
  end;

  FImageBox := nil;

  FEdit := nil;
  FEditMenuItemCopy := nil;
  FEditMenuItemSelectAll := nil;
  FEditMenuItemSep := nil;
  FEditMenu := nil;

  {$ifdef MEDIA_PLAYER}
  FMedia := nil;
  FMediaPanel := nil;
  FMediaPanel1 := nil;
  FMediaPanel2 := nil;
  FMediaBar := nil;
  FMediaTimer := nil;
  FMediaTimerBusy := False;
  {$endif}

  {$ifdef MEDIA_WMP64}
  FWMP6 := nil;
  FWMP6Controls := True;
  FWMP6Tracker := True;
  {$endif}

  {$ifdef MEDIA_WMP9}
  FWMP9 := nil;
  {$endif}

  FMediaEndTimer := nil;

  FBrowser := nil;

  {$ifdef WLX}
  FPlugins := TWlxPlugins.Create;
  FPluginsHighPriority := True;
  {$endif}

  {$ifdef PRINT}
  FPrintDialog := nil;
  FPageSetupDialog := nil;
  {$endif}

  {$ifdef IVIEW}
  with FIViewIntegration do
  begin
    Enabled := False;
    ExeName := '';
    ExtList := cIViewDefaultExtensions;
    HighPriority := False;
  end;
  FIViewObject := nil;
  {$endif}

  {$ifdef IJL}
  with FIJLIntegration do
  begin
    Enabled := True;
    ExtList := cIJLDefaultExtensions;
  end;
  {$endif}

  //Init events
  FOnMediaPlaybackEnd := nil;
  FOnWebDocumentComplete := nil;
  FOnWebNavigateComplete := nil;
  FOnWebStatusTextChange := nil;
  FOnWebTitleChange := nil;

  FOnFileUnload := nil;
  FOnFileLoad := nil;
  FOnOptionsChange := nil;
  FOnLoadImageStream := nil;
  FOnLoadWebStream := nil;

  //Hide all
  HideAll;
end;

destructor TATViewer.Destroy;
begin
  FreeData;

  {$ifdef IVIEW}
  if Assigned(FIViewObject) then
    FIViewObject.Free;
  {$endif}

  {$ifdef WLX}
  FPlugins.Free;
  {$endif}

  FreeMedia;

  inherited Destroy;
end;

{$ifdef PRINT}
procedure TATViewer.InitDialogs;
begin
  if not Assigned(FPrintDialog) then
  begin
    FPrintDialog := TPrintDialog.Create(Self);
  end;

  {$ifdef COMPILER_7_UP}
  if not Assigned(FPageSetupDialog) then
  begin
    FPageSetupDialog := TPageSetupDialog.Create(Self);
    FPageSetupDialog.Units := pmMillimeters;
  end;
  {$endif};
end;
{$endif}

procedure TATViewer.InitEdit;
begin
  if not Assigned(FEdit) then
  begin
    FEdit := TRichEditURL.Create(Self);
    with FEdit do
    begin
      Parent := Self;
      Align := alClient;
      ReadOnly := True;
      ScrollBars := ssBoth;
      HideSelection := False;
      OnSelectionChange := TextSelectionChange;
      TRichEditURL(FEdit).OnURLClick := TextURLClick;
    end;

    FEditMenuItemCopy := TMenuItem.Create(Self);
    with FEditMenuItemCopy do
    begin
      Caption := 'Copy';
      OnClick := EditMenuItemCopyClick;
    end;

    FEditMenuItemSelectAll := TMenuItem.Create(Self);
    with FEditMenuItemSelectAll do
    begin
      Caption := 'Select all';
      OnClick := EditMenuItemSelectAllClick;
    end;

    FEditMenuItemSep := TMenuItem.Create(Self);
    with FEditMenuItemSep do
    begin
      Caption := '-';
    end;

    FEditMenu := TPopupMenu.Create(Self);
    with FEditMenu do
    begin
      Items.Add(FEditMenuItemCopy);
      Items.Add(FEditMenuItemSep);
      Items.Add(FEditMenuItemSelectAll);
    end;

    FEdit.PopupMenu := FEditMenu;
  end;
end;

procedure TATViewer.InitImage;
begin
  if not Assigned(FImageBox) then
  begin
    FImageBox := TATImageBox.Create(Self);
    with FImageBox do
    begin
      Width := 1; //To "hide" control initially during image loading
      Height := 1;
      Parent := Self;
      Align := alClient;
      OnOptionsChange := Self.FOnOptionsChange;
      OnScroll := ImageBoxScroll;
      OnScrollAlt := ImageBoxScrollAlt;
    end;

    FImageUpDown := TUpDown.Create(Self);
    with FImageUpDown do
    begin
      Parent := FImageBox;
      Visible := False;
      Orientation := udHorizontal;
      Height := 20;
      Width := Height * 2;
      Position := 0;
      Min := Low(ShortInt);
      Max := High(ShortInt);
      ShowHint := True;
      OnClick := ImageUpDownClick;
    end;
  end;
end;

procedure TATViewer.InitMediaEndTimer;
begin
  if not Assigned(FMediaEndTimer) then
  begin
    FMediaEndTimer := TTimer.Create(Self);
    with FMediaEndTimer do
    begin
      Enabled := False;
      Interval := 500;
      OnTimer := MediaEndTimerTimer;
    end;
  end;
end;

procedure TATViewer.InitMedia;
begin
  InitMediaEndTimer;

  {$ifdef MEDIA_PLAYER}
  if (FMediaMode = vmmodeMCI) and not Assigned(FMedia) then
  begin
    FMediaPanel := TPanel.Create(Self);
    with FMediaPanel do
    begin
      Caption := '';
      BevelOuter := bvNone;
      Parent := Self;
      Align := alClient;
    end;

    FMediaPanel1 := TPanel.Create(Self);
    with FMediaPanel1 do
    begin
      Caption := '';
      BevelOuter := bvNone;
      Parent := FMediaPanel;
      Align := alBottom;
      Height := 30;
    end;

    FMediaPanel2 := TPanel.Create(Self);
    with FMediaPanel2 do
    begin
      Caption := '';
      BevelOuter := bvNone;
      Parent := FMediaPanel;
      Align := alClient;
    end;

    FMedia := TMediaPlayer.Create(Self);
    with FMedia do
    begin
      Parent := FMediaPanel1;
      Height := FMediaPanel1.Height;
      Display := FMediaPanel2;
      VisibleButtons := [btPlay, btPause, btStop];
      AutoRewind := True;
      Shareable := False;
      TimeFormat := tfMilliseconds;
      Notify := True;
      OnNotify := MediaNotify;
    end;

    FMediaBar := TTrackBar.Create(Self);
    with FMediaBar do
    begin
      Parent := FMediaPanel1;
      Left := 90;
      Top := 2;
      Width := 100;
      Height := FMediaPanel1.Height - 2 * Top;
      PageSize := 10; 
      TickMarks := tmBoth;
      TickStyle := tsNone;
      ThumbLength := 18;
      OnChange := MediaBarChange;
    end;

    FMediaTimer := TTimer.Create(Self);
    with FMediaTimer do
    begin
      OnTimer := MediaTimerTimer;
    end;
  end;
  {$endif}

  {$ifdef MEDIA_WMP64}
  if (FMediaMode = vmmodeWMP64) and not Assigned(FWMP6) then
    try
      FWMP6 := TWMP.Create(Self);
      with FWMP6 do
      begin
        Align := alClient;
        Parent := Self;
        //Parent assignment must be after Align assignment!
        AutoStart := False;
        AutoRewind := True;
        OnPlayStateChange := PlayStateChange_WMP6;
      end;
    except
      MsgError(Format(MsgViewerErrInitControl, ['Windows Media Player 6.4 ActiveX']));
    end;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if (FMediaMode = vmmodeWMP9) and not Assigned(FWMP9) then
    try
      FWMP9 := TWMP9.Create(Self);
      with FWMP9 do
      begin
        Align := alClient;
        Parent := Self;
        //Parent assignment must be after Align assignment!
        Settings.AutoStart := False;
        OnPlayStateChange := PlayStateChange_WMP9;
      end;
    except
      MsgError(Format(MsgViewerErrInitControl, ['Windows Media Player 9 ActiveX']));
    end;
  {$endif}

  HideMedia;
end;

procedure TATViewer.InitWeb;
begin
  if not Assigned(FBrowser) then
  begin
    FBrowser := TWebBrowser.Create(Self);
    with FBrowser do
    begin
      TControl(FBrowser).Parent := Self;
      Align := alClient;
      Silent := True;
      //Workaround for WebBrowser bug: it first opens BMP files
      //in a new window:
      Navigate('about:blank');
      OnDocumentComplete := WebBrowserDocumentComplete;
      OnNavigateComplete2 := WebBrowserNavigateComplete2;
      OnStatusTextChange := WebBrowserStatusTextChange;
      OnTitleChange := WebBrowserTitleChange;
      {$ifdef IE4X}
      OnFileDownload := WebBrowserFileDownload;
      {$endif}
    end;
    HideWeb;
  end;
end;

procedure TATViewer.FreeMedia;
begin
  {$ifdef MEDIA_PLAYER}
  if Assigned(FMedia) then
  begin
    FMediaTimer.Free;
    FMediaBar.Free;
    FMedia.Free;
    FMediaPanel1.Free;
    FMediaPanel2.Free;
    FMediaPanel.Free;

    FMediaTimer := nil;
    FMediaBar := nil;
    FMedia := nil;
    FMediaPanel1 := nil;
    FMediaPanel2 := nil;
    FMediaPanel := nil;
  end;
  {$endif}
    
  {$ifdef MEDIA_WMP64}
  if Assigned(FWMP6) then
  begin
    FWMP6.Parent := nil;
    FWMP6.Free;
    FWMP6 := nil;
  end;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if Assigned(FWMP9) then
  begin
    FWMP9.Parent := nil;
    FWMP9.Free;
    FWMP9 := nil;
  end;
  {$endif}
end;

procedure TATViewer.HideAll;
var
  IsEmpty, IsImage: Boolean;
begin
  IsEmpty := (FFileName = '');
  IsImage := (FFileName <> '') and SFileExtensionMatch(FFileName, ActualExtImages);

  //Hide Edit/BinHex/Browser controls when different mode is set
  if IsEmpty or not (FMode in [vmodeText, vmodeBinary, vmodeHex, vmodeUnicode]) then
    FBinHex.Hide;

  if IsEmpty or (FMode <> vmodeRTF) then
    HideEdit;

  if IsEmpty or (FMode <> vmodeWeb) then
    HideWeb;

  //Hide image control when non-image is to be loaded
  if IsEmpty or (FMode <> vmodeMedia) or (not IsImage) then
    HideImage;

  //Hide media control when non-media is to be loaded
  if IsEmpty or (FMode <> vmodeMedia) or IsImage then
    HideMedia;

  //Hide plugins when different mode is set
  {$ifdef WLX}
  if IsEmpty or (FMode <> vmodeWLX) then
    HideWLX;
  {$endif}

  FTextPanel.Hide;
end;

procedure TATViewer.HideMedia;
begin
  {$ifdef MEDIA_PLAYER}
  if Assigned(FMedia) then
  begin
    FMediaPanel.Hide;
    FMedia.Enabled := False;
    FMediaBar.Enabled := False;
  end;
  {$endif}

  {$ifdef MEDIA_WMP64}
  if Assigned(FWMP6) then
    FWMP6.Hide;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if Assigned(FWMP9) then
    FWMP9.Hide;
  {$endif}
end;

procedure TATViewer.HideEdit;
begin
  if Assigned(FEdit) then
    FEdit.Hide;
end;

procedure TATViewer.HideImage;
begin
  if Assigned(FImageBox) then
    FImageBox.Hide;
end;

procedure TATViewer.HideWeb;
begin
  if Assigned(FBrowser) then
    FBrowser.Hide;
end;


function TATViewer.OpenStream(AStream: TStream; AMode: TATViewerMode): Boolean;
begin
  Result := True;
  FSourceType := vfSrcNone;

  FStream := AStream;
  FMode := AMode;

  FreeData;
  HideAll;

  if AStream = nil then Exit;

  FSourceType := vfSrcStream;
  FFileName := '';
  FFileSize := FStream.Size;
  FillChar(FFileTime, SizeOf(FFileTime), 0);

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      LoadBinaryStream;
    vmodeRTF:
      LoadRTFStream;
    vmodeMedia:
      LoadImageStream;
    vmodeWeb:
      LoadWebStream;
  end;
end;


function TATViewer.Open(const AFileName: WideString; APicture: TPicture = nil): Boolean;
var
  NewFileName: WideString;
begin
  Result := True;
  FSourceType := vfSrcNone;

  //Need to expand given filename, since filename can be passed from application
  //without path at all, and this causes problems with WebBrowser and plugins.
  NewFileName := FGetFullPathName(AFileName);

  if (FFileName <> NewFileName) then
  begin
    DoFileUnload;

    FFileName := NewFileName;
    FGetFileInfo(FFileName, FFileSize, FFileTime);
    FreeData;

    if FFileName = '' then
    begin
      HideAll;
      while WebBusy and not Application.Terminated do //Wait for PDF
      begin
        Application.ProcessMessages;
        Sleep(0);
      end;
      Result := True;
      Exit
    end;

    if not IsFileExist(FFileName) then
    begin
      FFileName := '';
      HideAll;
      MsgError(SFormatW(MsgViewerErrCannotFindFile, [NewFileName]));
      Result := False;
      Exit
    end;

    if not IsFileAccessed(FFileName) then
    begin
      FFileName := '';
      HideAll;
      MsgError(SFormatW(MsgViewerErrCannotOpenFile, [NewFileName]));
      Result := False;
      Exit
    end;

    FSourceType := vfSrcFile;
    if FModeDetect then
      DetectMode; //LoadWLX called implicitly
    HideAll;

    case FMode of
      vmodeNone:
        begin
          TTextPanel(FTextPanel).Caption := MsgViewerShowCfm;
          FTextPanel.Show;
        end;
      vmodeText,
      vmodeBinary,
      vmodeHex,
      vmodeUnicode:
        LoadBinary;
      vmodeRTF:
        LoadRTF;
      vmodeMedia:
        LoadMedia(APicture);
      vmodeWeb:
        LoadWeb;
      {$ifdef WLX}
      vmodeWLX:
        begin
          //When FModeDetect = True, there is no need to call LoadWLX,
          //it's already called in DetectMode above.
          if not FModeDetect then
            LoadWLX;
        end;
      {$endif}
    end;

    DoFileLoad;
  end;
end;


{$ifdef WLX}
function TATViewer.OpenFolder(const AFolderName: WideString): Boolean;
var
  NewFolderName: WideString;
begin
  Result := True;

  //Need to expand given filename, since filename can be passed from application
  //without path at all, and this causes problems with WebBrowser and plugins.
  NewFolderName := FGetFullPathName(AFolderName);

  if (FFileName <> NewFolderName) then
  begin
    DoFileUnload;

    FFileName := NewFolderName;
    FFileSize := 0;
    FillChar(FFileTime, SizeOf(FFileTime), 0);

    FMode := vmodeWLX;
    FreeData;
    HideAll;

    if FFileName = '' then
    begin
      Exit
    end;

    if not IsDirExist(FFileName) then
    begin
      MsgError(SFormatW(MsgViewerErrCannotFindFolder, [NewFolderName]));
      FFileName := '';
      Result := False;
      Exit
    end;

    if not OpenByPlugins(True) then
    begin
      FFileName := '';
      Result := False;
      Exit;
    end;

    DoFileLoad;
  end;
end;
{$endif}

procedure TATViewer.Reload;
begin
  Assert(FFileName <> '', 'File not loaded: Reload');

  SetMode(FMode);
end;


procedure TATViewer.FreeSearch;
begin
  {$ifdef SEARCH}
  FFindText := '';
  FFindOptions := [];
  FFindFinished := False;
  {$endif}
end;

procedure TATViewer.FreeData(AFreeImage: Boolean = True);
begin
  FBinHex.Open('', False);
  FBinHex.OpenStream(nil, False);

  FIsImageBefore := Assigned(FImageBox) and Assigned(FImageBox.Image.Picture.Graphic);
  if FIsImageBefore and AFreeImage then
    FImageBox.Image.Picture := nil;

  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FEdit) and (FEdit.Lines.Count > 0) then
      FEdit.Lines.Clear;

    {$ifdef MEDIA_PLAYER}
    if Assigned(FMedia) and (FMedia.FileName <> '') then
    begin
      FMedia.Close;
      FMedia.FileName := '';
    end;
    {$endif}

    {$ifdef MEDIA_WMP64}
    if Assigned(FWMP6) and (FWMP6.FileName <> '') then
      FWMP6.FileName := '';
    {$endif}

    {$ifdef MEDIA_WMP9}
    if Assigned(FWMP9) and (FWMP9.URL <> '') then
      FWMP9.URL := '';
    {$endif}

    if Assigned(FBrowser) then
      FreeAndNil(FBrowser);
      //WB_NavigateBlank(FBrowser); //Doesnt work for some MHT
  end;

  FIsImage := False;
  FIsIcon := False;
  FIsMetafile := False;
  FIsMedia := False;

  FreeSearch;
end;

procedure TATViewer.DetectMode;
begin
  //Reset encoding
  if FTextDetectOEM then
    FTextEncoding := vencANSI;

  {$ifdef WLX}
  if (not (vmodeWLX in FModesDisabledForDetect)) and
    FPluginsHighPriority and OpenByPlugins(True) then FMode := vmodeWLX else
  {$endif}

  if (not (vmodeRTF in FModesDisabledForDetect)) and
    SFileExtensionMatch(FFileName, ATViewerOptions.ExtRTF) then FMode := vmodeRTF else

  if (not (vmodeText in FModesDisabledForDetect)) and
    SFileExtensionMatch(FFileName, ATViewerOptions.ExtText) then FMode := vmodeText else

  if (not (vmodeMedia in FModesDisabledForDetect)) and
    SFileExtensionMatch2(FFileName, ActualExtImages, ATViewerOptions.ExtMedia)
                                                 then FMode := vmodeMedia else

  if (not (vmodeWeb in FModesDisabledForDetect)) and
    SFileExtensionMatch(FFileName, ATViewerOptions.ExtWeb) then FMode := vmodeWeb else

  //Test for FModesDisabledForDetect is in DetectTextAndUnicode
  if FTextDetect and DetectTextAndUnicode then begin end else

  {$ifdef WLX}
  if (not (vmodeWLX in FModesDisabledForDetect)) and
    (not FPluginsHighPriority) and OpenByPlugins(True) then FMode := vmodeWLX else
  {$endif}

  //If no mode detected, set default
  if FModeUndetectedCfm then
    FMode := vmodeNone
  else
    FMode := FModeUndetected;
end;


{$ifdef MEDIA_WMP64}
procedure TATViewer.SetMediaFit_WMP6(WMP: TWMP);
const
  cWMPDisplaySize: array[Boolean] of MPDisplaySizeConstants =
    (mpDefaultSize, mpFitToSize);
begin
  if Assigned(WMP) then
    WMP.DisplaySize := cWMPDisplaySize[FMediaFit];
end;

procedure TATViewer.PlayStateChange_WMP6(Sender: TObject; OldState: Integer; NewState: Integer);
begin
  if NewState = MediaPlayer_TLB.mpStopped then
  begin
    PreparePlaybackEnd;
  end;
end;
{$endif}

{$ifdef MEDIA_WMP9}
procedure TATViewer.SetMediaFit_WMP9(WMP: TWMP9);
begin
  if Assigned(WMP) then
  try
    with WMP do
      (IDispatch(OleObject) as IWMPPlayer4).StretchToFit := FMediaFit;
  except
  end;
end;

procedure TATViewer.PlayStateChange_WMP9(Sender: TObject; NewState: Integer);
begin
  if NewState = MediaPlayer9_TLB.wmppsStopped then
  begin
    PreparePlaybackEnd;
  end;
end;
{$endif}


function TATViewer.ActualExtImages: AnsiString;
begin
  Result := ATViewerOptions.ExtImages;
  {$ifdef IVIEW}
  if IViewIntegration.Enabled then
    Result := Result + ',' + IViewIntegration.ExtList;
  {$endif}
end;


procedure TATViewer.UnloadImage;
begin
  if Assigned(FImageBox) and
    Assigned(FImageBox.Image) then
  begin
    FImageBox.Image.Picture := nil;
    FImageBox.UpdateImageInfo;
  end;  
end;

procedure TATViewer.ShowImageError(const Msg: AnsiString);
begin
  UnloadImage;
  FImageError := True;
  FImageErrorMessage := Msg;
  if FImageErrorMessageBox then
    MsgError(Msg);
end;


procedure TATViewer.LoadImageStream;
begin
  Assert(FStream <> nil, 'Stream not assigned');
  FreeData(False);

  FIsImage := True;
  FIsIcon := False;
  FIsMetafile := False;

  FImageError := False;
  FImageErrorMessage := '';
  FImageBPP := 0;
  FIsImageIView := False;
  FIsImageIJL := False;

  InitImage;
  if Assigned(FImageBox) then
  begin
    FImageBox.Color := FImageColor;
    FImageBox.ImageDrag := FImageDrag;
    FImageBox.Image.Cursor := FImageCursor;
    FImageBox.ImageDragCursor := FImageDragCursor;
    FImageBox.Image.Transparent := FImageTransparent;
    FImageBox.Image.Resample := FImageResample;
    FImageBox.Image.ResampleBackColor := FImageColor;

    try
      DoLoadImageStream;
    except
      on E: EInvalidGraphic do
        ShowImageError(E.Message)
      else
        ShowImageError(MsgViewerErrImage);
    end;

    FImageBox.ImageFitToWindow := FMediaFit;
    FImageBox.ImageFitOnlyBig := FMediaFitOnlyBig;
    FImageBox.ImageCenter := FMediaCenter;
    FImageBox.ImageKeepPosition := FImageKeepPosition;
    FImageBox.BorderStyle := FBorderStyleInner;
    FImageBox.Show;
    if CanSetFocus then
      FImageBox.SetFocus;
  end;
end;


procedure TATViewer.LoadImage(APicture: TPicture = nil; ANewImage: Boolean = True);
  //
  {$ifdef TIFF}
  function LoadTiff: Boolean;
  var
    h: THandle;
    Func: function(S: TStream; B: TBitmap; Page: Integer; var PagesCount: Integer): Boolean;
    Bmp: TBitmap;
    S: TStream;
  begin
    Result := False;
    h := LoadLibrary('VTiff.dll');
    if h = 0 then
      raise EInvalidGraphic.Create(Format(MsgViewerErrCannotLoadFile, ['VTiff.dll']));
    try
      Func := GetProcAddress(h, 'ReadTIFFIntoBitmap');
      if @Func = nil then Exit;

      S := {$IFDEF TNT}TTntFileStream{$ELSE}TFileStream{$ENDIF}.Create(
        FFileName, fmOpenRead or fmShareDenyNone);
      Bmp := TBitmap.Create;

      try
        if Func(S, Bmp, FImagePage, FImagePagesCount) then
        begin
          FImageBox.Image.Transparent := False;
          FImageBox.Image.Picture.Assign(Bmp);
          FImageBox.UpdateImageInfo;
          Result := True;
        end;
      finally
        FreeAndNil(Bmp);
        FreeAndNil(S);
      end;
    finally
      FreeLibrary(h);
    end;
  end;
  {$endif}

  //
  {$ifdef ANI}
  function LoadAni: Boolean;
  var
    GetFrames: function(FileName: PChar): THandle; stdcall;
    GetCursorCreator: function(Handle: THandle; FileName: PChar): ShortString; stdcall;
    GetCursorTitle: function(Handle: THandle; FileName: PChar): ShortString; stdcall;
  var
    b: TBitmap;
    IL: TImageList;
    hLib, hIL: THandle;
    fn, s1, s2: AnsiString;
    i: Integer;
  const
    CH = 16; //row height
    CF = 9; //font size
    C2 = 2; //border 2px
  begin
    Result := False;
    hLib := LoadLibrary('amnani.dll');
    if hLib = 0 then
      raise EInvalidGraphic.Create(Format(MsgViewerErrCannotLoadFile, ['amnani.dll']));

    try
      GetFrames := GetProcAddress(hLib, 'GetFrames');
      if @GetFrames = nil then Exit;
      GetCursorCreator := GetProcAddress(hLib, 'GetCursorCreator');
      if @GetCursorCreator = nil then Exit;
      GetCursorTitle := GetProcAddress(hLib, 'GetCursorTitle');
      if @GetCursorTitle = nil then Exit;

      fn := FFileNameWideToAnsi(FFileName);
      hIL := GetFrames(PAnsiChar(fn));
      if hIL = 0 then Exit;
      s1 := MsgViewerAniTitle + GetCursorTitle(Handle, PAnsiChar(fn));
      s2 := MsgViewerAniCreator + GetCursorCreator(Handle, PAnsiChar(fn));

      IL := TImageList.Create(Self);
      b := TBitmap.Create;
      try
        IL.Handle := hIL;

        b.PixelFormat := pf16bit;
        b.Canvas.Font.Name := Font.Name;
        b.Canvas.Font.Size := CF;
        b.Canvas.Font.Color := clBtnText;
        b.Width := IMax(IMax(
          IL.Width * IL.Count,
          b.Canvas.TextWidth(s1)),
          b.Canvas.TextWidth(s2)) + C2 * 2;
        b.Height := IL.Height + CH * 2 + C2 * 3;

        b.Canvas.Brush.Color := clBtnface;
        b.Canvas.FillRect(Rect(0, 0, b.Width, b.Height));
        b.Canvas.TextOut(C2, C2, s1);
        b.Canvas.TextOut(C2, C2 + CH, s2);
        for i := 0 to IL.Count - 1 do
          IL.Draw(b.Canvas, C2 + i * IL.Width, CH * 2 + C2 * 2, i);

        FImageBox.Image.Transparent := False;
        FImageBox.Image.Picture.Assign(b);
        FImageBox.UpdateImageInfo;
        Result := True;
      finally
        b.Free;
        IL.Free;
      end;
    finally
      FreeLibrary(hLib);
    end;
  end;
  {$endif}

  //
  function LoadCur: Boolean;
  const
    cSize = 32; //max cursor size
  var
    h: HCursor;
    B: TBitmap;
  begin
    {if Win32Platform = VER_PLATFORM_WIN32_NT then
      h := LoadCursorFromFileW(PWideChar(FFileName))
    else }
      h := LoadCursorFromFileA(PAnsiChar(FFileNameWideToAnsi(FFileName)));

    Result := h <> 0;
    if Result then
    begin
      B := TBitmap.Create;
      try
        B.PixelFormat := pf16bit;
        B.Width := cSize;
        B.Height := cSize;
        B.Canvas.Brush.Color := clBtnface;
        B.Canvas.FillRect(Rect(0, 0, b.Width, b.Height));
        DrawIcon(B.Canvas.Handle, 0, 0, h);

        FImageBox.Image.Transparent := False;
        FImageBox.Image.Picture.Assign(b);
        FImageBox.UpdateImageInfo;
      finally
        B.Free;
        DestroyCursor(h);
      end;
    end;
  end;

  //
  {$ifdef IVIEW}
  function LoadImageWithIView: Boolean;
  var
    Bmp: TBitmap;
  begin
    FIsImageIView := True;

    //if IView exe does not exist, raise special exception:
    if not IsFileExist(IViewIntegration.ExeName) then
    begin
      raise EInvalidGraphic.Create(
        SFormatW(MsgViewerErrCannotFindFile, [IViewIntegration.ExeName]) );
    end;

    Bmp := TBitmap.Create;
    try
      if not Assigned(FIViewObject) then
        FIViewObject := TIrfanXnView.Create('');
      with TIrfanXnView(FIViewObject) do
      begin
        Host := IViewIntegration.ExeName;
        Bmp.PixelFormat := pf24bit;
        Bmp.Handle := GetBitmap(FFileNameWideToAnsi(FFileName));
        Host := '';
      end;
      Result := Bmp.Handle <> 0;
      if Result then
      begin
        //If IView could load file, put it into Image object:
        FImageBox.Image.Transparent := FImageTransparent and SFileExtensionMatch(FFileName, 'bmp');
        FImageBox.Image.Picture.Assign(Bmp);
        FImageBox.UpdateImageInfo;
      end
      else
      begin
        //If IView could not load file, raise an exception that
        //will be immediately handled and "Unsupported image format"
        //message will be shown:
        raise Exception.Create('');
      end;
    finally
      Bmp.Free;
    end;
  end;
  {$endif}

  //
  procedure LoadImageWithDelphi;
  begin
    FImageBox.Image.Picture.LoadFromFile(FFileName);
    FImageBox.UpdateImageInfo;
  end;

  //
  {$ifdef IJL}
  function LoadImageWithIJL: Boolean;
  var
    bmp: TBitmap;
  begin
    Result := False;
    FIsImageIJL := True;

    //Load IJL dinamycally here
    if (ijlLib = 0) then
      if not LoadIJL then Exit;

    bmp := TBitmap.Create;
    try
      if LoadBmpFromJpegFile(bmp, FFileNameWideToAnsi(FFileName), True) then
      begin
        FImageBox.Image.Transparent := False;
        FImageBox.Image.Picture.Assign(bmp);
        FImageBox.UpdateImageInfo;
        Result := True;
      end;
    finally
      bmp.Free;
    end;
  end;
  {$endif}

  //
  procedure LoadImageFromPicture(APicture: TPicture);
  begin
    FImageBox.Image.Picture.Assign(APicture);
    FImageBox.UpdateImageInfo;
  end;

{$ifdef IVIEW}
var
  IViewHighPriority: Boolean;
{$endif}
var
  OldImageScale: Integer;
begin
  FImageError := False;
  FImageErrorMessage := '';
  FImageBPP := 0;
  FIsImageIView := False;
  FIsImageIJL := False;
  if ANewImage then
  begin
    FImagePage := 0;
    FImagePagesCount := 1;
  end;
  OldImageScale := GetImageScale;

  //If an image was loaded before, then we need to switch between
  //"internal library" and "IrfanView" modes. We do this by setting the
  //IViewHighPriority local variable to False/True, otherwise it's set 
  //according to IViewIntegration.HighPriority property.

  {$ifdef IVIEW}
  if FIsImageBefore then
    IViewHighPriority := not FIsImageIView
  else
    IViewHighPriority := FIViewIntegration.HighPriority;
  {$endif}

  if Assigned(FImageBox) then
    try
      try
        DoCursorHours;

        //Load from TPicture object
        if Assigned(APicture) then
        begin
          LoadImageFromPicture(APicture);
          Exit;
        end;

        {$ifdef IVIEW}
        //1) Load with IView with high priority
        if IViewIntegration.Enabled and IViewHighPriority then
          if SFileExtensionMatch(FFileName, IViewIntegration.ExtList) then
          begin
            LoadImageWithIView;
            Exit;
          end;
        {$endif}

        {$ifdef ANI}
        //2a) Load ANI
        if SFileExtensionMatch(FFileName, 'ani') then
        begin
          if not LoadAni then
            raise Exception.Create('');
          Exit;
        end;
        {$endif}

        {$ifdef TIFF}
        //2b) Load TIFF
        if SFileExtensionMatch(FFileName, 'tif,tiff') then
        begin
          if not LoadTiff then
            raise Exception.Create('');
          Exit;
        end;
        {$endif}

        //2c) Load CUR
        if SFileExtensionMatch(FFileName, 'cur') then
        begin
          if not LoadCur then
            raise Exception.Create('');
          Exit;
        end;

        {$ifdef IJL} 
        //3) Load with IJL
        if FIJLIntegration.Enabled then
          if SFileExtensionMatch(FFileName, FIJLIntegration.ExtList) then
          begin
            if LoadImageWithIJL then Exit;
          end;
        {$endif}

        //4) Load with Delphi
        if SFileExtensionMatch(FFileName, ATViewerOptions.ExtImages) then
        begin
          try
            LoadImageWithDelphi;
          except
            {$ifdef IVIEW}
            //If library couldn't load an image, switch to IView implicitly
            //(so useless error messagebox won't appear)
            if IViewIntegration.Enabled then
              if SFileExtensionMatch(FFileName, IViewIntegration.ExtList) then
              begin
                LoadImageWithIView;
                Exit;
              end;
            {$endif}
            //If IView couldn't help here, show error messagebox finally
            raise;
          end;
          Exit;
        end;

        {$ifdef IVIEW}
        //5) Load with IView with low priority
        if IViewIntegration.Enabled and (not IViewHighPriority) then
          if SFileExtensionMatch(FFileName, IViewIntegration.ExtList) then
          begin
            LoadImageWithIView;
            Exit;
          end;
        {$endif}

        UnloadImage;

      finally
        DoCursorDefault;
        FImageBPP := GetImageBPP; //Updated only on image reading
        FImageUpDown.Visible := FImagePagesCount > 1;
        if not ANewImage then //Restore scale for prev page
          SetImageScale(OldImageScale);
      end;
    except
      on E: EInvalidGraphic do
        ShowImageError(E.Message)
      else
        ShowImageError(MsgViewerErrImage);
    end;
end;


procedure TATViewer.LoadMedia(APicture: TPicture = nil);
  //
  procedure ShowMediaError(const Msg: AnsiString);
  begin
    if Msg <> '' then
      MsgError(Msg)
    else
      MsgError(MsgViewerErrMedia);
  end;
  //
begin
  Assert(FFileName <> '', 'FileName not assigned');

  if SFileExtensionMatch(FFileName, ActualExtImages) then
    try
      FreeData(False);
      FIsImage := True;
      InitImage;

      if Assigned(FImageBox) then
      begin
        FImageBox.Color := FImageColor;
        FImageBox.ImageDrag := FImageDrag;
        FImageBox.Image.Cursor := FImageCursor;
        FImageBox.ImageDragCursor := FImageDragCursor;
        FImageBox.Image.Transparent := FImageTransparent;
        FImageBox.Image.Resample := FImageResample;
        FImageBox.Image.ResampleBackColor := FImageColor;

        LoadImage(APicture);

        {$ifdef GIF}
        if Assigned(FImageBox.Image.Picture.Graphic) and
          (FImageBox.Image.Picture.Graphic is TGifImage) then
          with (FImageBox.Image.Picture.Graphic as TGifImage) do
          begin
            if FImageTransparent or 
              ((Images.Count > 1) and IsTransparent) //Always set transparency for (animaged + transparent) images
            then
              DrawOptions := DrawOptions + [goTransparent]
            else
              DrawOptions := DrawOptions - [goTransparent];
          end;
        {$endif}

        FImageBox.ImageFitToWindow := FMediaFit;
        FImageBox.ImageFitOnlyBig := FMediaFitOnlyBig;
        FImageBox.ImageCenter := FMediaCenter;
        FImageBox.ImageKeepPosition := FImageKeepPosition;
        FImageBox.BorderStyle := FBorderStyleInner;
        FImageBox.Show;
        if CanSetFocus then
          FImageBox.SetFocus;

        FIsIcon := SFileExtensionMatch(FFileName, 'ico'); //FImageBox.Image.Picture.Graphic is TIcon;
        FIsMetafile := FImageBox.Image.Picture.Graphic is TMetafile;
      end;
    except
    end

  else
  begin
    FreeData;
    FIsMedia := True;
    MediaSyncVolume;
    InitMedia;

    {$ifdef MEDIA_PLAYER}
    if (FMediaMode = vmmodeMCI) and Assigned(FMedia) then
      try
        try
          DoCursorHours;
          FMediaPanel.Show;
          SetMediaPosition;
          FMedia.FileName := FFileName;
          FMedia.Notify := True;
          FMedia.Open;
          FMedia.Enabled := True;
          FMediaBar.Enabled := True;
          FMediaBar.Max := FMedia.Length;
          if CanSetFocus then
            FMedia.SetFocus;
          if FMediaAutoPlay then
            FMedia.Play;
        finally
          DoCursorDefault;
        end;
      except
        on E: EMCIDeviceError do
          ShowMediaError(E.Message)
        else
          ShowMediaError('');
      end;
    {$endif}

    {$ifdef MEDIA_WMP64}
    if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
      try
        with FWMP6 do
        begin
          VideoBorder3D := FBorderStyleInner <> bsNone;
          ShowStatusBar := True;
          ShowControls := FWMP6Controls;
          ShowTracker := FWMP6Tracker;

          Show;
          if CanSetFocus then
            SetFocus;

          Volume:= Vol_AtoW6(FMediaVolume);
          Mute:= FMediaMute;
          if FMediaLoop then
            PlayCount := MaxInt
          else
            PlayCount := FMediaPlayCount;
          AutoStart := FMediaAutoPlay;
          SetMediaFit_WMP6(FWMP6);
          SetMediaPosition;

          FileName := FFileName;
        end;
      except
        on E: Exception do
          ShowMediaError(E.Message);
      end;
    {$endif}

    {$ifdef MEDIA_WMP9}
    if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
      try
        with FWMP9 do
        begin
          Show;
          if CanSetFocus then
            SetFocus;

          Settings.Volume:= Vol_AtoW9(FMediaVolume);
          Settings.Mute:= FMediaMute;
          if FMediaLoop then
            Settings.PlayCount := MaxInt
          else
            Settings.PlayCount := FMediaPlayCount;
          Settings.AutoStart := FMediaAutoPlay;
          SetMediaFit_WMP9(FWMP9);
          SetMediaPosition;

          URL := FFileName;
        end;
      except
        on E: Exception do
          ShowMediaError(E.Message);
      end;
    {$endif}
  end;
end;


procedure TATViewer.LoadBinaryStream;
begin
  Assert(FStream <> nil, 'Stream not assigned');
  FreeData;
  FreeSearch;

  with FBinHex do
  begin
    Color := FTextColor;
    BorderStyle := FBorderStyleInner;
    TextEncoding := FTextEncoding;
    TextWrap := FTextWrap;

    case Self.FMode of
      vmodeText:
        Mode := vbmodeText;
      vmodeBinary:
        Mode := vbmodeBinary;
      vmodeHex:
        Mode := vbmodeHex;
      vmodeUnicode:
        Mode := vbmodeUnicode;
    end;

    OpenStream(FStream);

    Show;
    if CanSetFocus then
      SetFocus;
  end;
end;


procedure TATViewer.LoadBinary;
var
  ANewFile: Boolean;
begin
  Assert(FFileName <> '', 'FileName not assigned');

  //Is file new for ATBinHex component?
  ANewFile := FFileName <> FBinHex.FileName;

  //Clear data only when file is new,
  //and clear search anyway:
  if ANewFile then
    FreeData;
  FreeSearch;

  with FBinHex do
  begin
    Color := FTextColor;
    BorderStyle := FBorderStyleInner;
    TextEncoding := FTextEncoding;
    TextWrap := FTextWrap;

    case Self.FMode of
      vmodeText:
        Mode := vbmodeText;
      vmodeBinary:
        Mode := vbmodeBinary;
      vmodeHex:
        Mode := vbmodeHex;
      vmodeUnicode:
        //If Unicode mode already activated, switch to UHex mode:
        if (not ANewFile) and (Mode = vbmodeUnicode) then
          Mode := vbmodeUHex
        else
          Mode := vbmodeUnicode;
    end;

    if ANewFile then
      Open(FFileName);

    Show;
    if CanSetFocus then
      SetFocus;
  end;
end;


procedure TATViewer.LoadRTFStream;
begin
  Assert(FStream <> nil, 'Stream not assigned');
  FreeData;

  InitEdit;
  if Assigned(FEdit) then
    with FEdit do
    begin
      //work around RichEdit bug, reset font
      Font.Name := 'Webdings';
      Font.Size := 8;
      Font.Color := clWhite;
      Font := GetTextFont;

      //RichEdit bug: WordWrap assignment must be after Font assignment, or font will be broken
      Color := FTextColor;
      WordWrap := FTextWrap;
      BorderStyle := FBorderStyleInner;
      
      try
        try
          DoCursorHours;
          RE_LoadStream(FEdit, FStream, 0, 0);
          TextSelectionChange(Self);
        finally
          DoCursorDefault;
        end;
      except
        MsgError(MsgViewerErrCannotReadStream);
      end;

      Show;
      if CanSetFocus then
        SetFocus;
    end;
end;

procedure TATViewer.LoadRTF;
begin
  Assert(FFileName <> '', 'FileName not assigned');
  FreeData;

  InitEdit;
  if Assigned(FEdit) then
    with FEdit do
    begin
      //work around RichEdit bug, reset font
      Font.Name := 'Webdings';
      Font.Size := 8;
      Font.Color := clWhite;
      Font := GetTextFont;

      //RichEdit bug: WordWrap assignment must be after Font assignment, or font will be broken
      Color := FTextColor;
      WordWrap := FTextWrap;
      BorderStyle := FBorderStyleInner;
      
      try
        try
          DoCursorHours;
          RE_LoadFile(FEdit, FFileName, 0, 0);
          TextSelectionChange(Self);
        finally
          DoCursorDefault;
        end;
      except
        MsgError(SFormatW(MsgViewerErrCannotLoadFile, [FFileName]));
      end;

      Show;
      if CanSetFocus then
        SetFocus;
    end;
end;


procedure TATViewer.LoadWebStream;
begin
  Assert(FStream <> nil, 'Stream not assigned');
  FreeData;

  InitWeb;
  if Assigned(FBrowser) then
    try
      if WebBrowserSafe then
        if FBorderStyleInner = bsNone then
          WB_Set3DBorderStyle(FBrowser, FBorderStyleInner <> bsNone);
      FBrowser.Show;
      {$ifdef OFFLINE}
      WB_SetGlobalOffline(FWebOffline);
      {$endif}
      DoLoadWebStream;
    except
    end;
end;

procedure TATViewer.LoadWeb;
begin
  Assert(FFileName <> '', 'FileName not assigned');
  FreeData;

  InitWeb;
  if Assigned(FBrowser) then
    try
      if WebBrowserSafe then
        if FBorderStyleInner = bsNone then
          WB_Set3DBorderStyle(FBrowser, FBorderStyleInner <> bsNone);
      FBrowser.Show;
      {$ifdef OFFLINE}
      WB_SetGlobalOffline(FWebOffline);
      {$endif}
      WB_NavigateFilename(FBrowser, FFileName, FWebWaitForNavigate);
    except
    end;
end;

procedure TATViewer.SetMode(AValue: TATViewerMode);
begin
  Assert(FSourceType <> vfSrcStream,
    'Mode cannot be changed for streams');

  DoFileUnload;

  FMode := AValue;
  HideAll;

  if FFileName <> '' then
  begin
    case FMode of
      vmodeText,
      vmodeBinary,
      vmodeHex,
      vmodeUnicode:
        LoadBinary;
      vmodeRTF:
        LoadRTF;
      vmodeMedia:
        LoadMedia;
      vmodeWeb:
        LoadWeb;
      {$ifdef WLX}
      vmodeWLX:
        LoadWLX;
      {$endif}
    end;

    DoFileLoad;
  end;
end;

procedure TATViewer.SetMediaMode(AValue: TATViewerMediaMode);
begin
  if FMediaMode <> AValue then
  begin
    FreeData;
    //FreeMedia; //FreeMedia commented: causes strange AV
    FMediaMode := AValue;
  end;
end;

function TATViewer.GetTextEncoding: TATEncoding;
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FTextEncoding := FBinHex.TextEncoding;
  end;
  Result := FTextEncoding;
end;

procedure TATViewer.SetTextEncoding(AValue: TATEncoding);
begin
  if FTextEncoding <> AValue then
  begin
    FTextEncoding := AValue;

    case FMode of
      vmodeText,
      vmodeBinary,
      vmodeHex,
      vmodeUnicode:
        FBinHex.TextEncoding := FTextEncoding;

      {$ifdef WLX}
      vmodeWLX:
        SendWLXParams;
      {$endif}
    end;
  end;
end;

procedure TATViewer.SetTextWrap(AValue: Boolean);
begin
  if FTextWrap <> AValue then
  begin
    FTextWrap := AValue;

    FBinHex.TextWrap := FTextWrap;

    if Assigned(FEdit) then
      FEdit.WordWrap := FTextWrap;

    {$ifdef WLX}
    if FMode = vmodeWLX then
      SendWLXParams;
    {$endif}
  end;
end;

function TATViewer.GetTextWidth: Integer;
begin
  Result := FBinHex.TextWidth;
end;

function TATViewer.GetTextWidthHex: Integer;
begin
  Result := FBinHex.TextWidthHex;
end;

function TATViewer.GetTextWidthFit: Boolean;
begin
  Result := FBinHex.TextWidthFit;
end;

function TATViewer.GetTextWidthFitHex: Boolean;
begin
  Result := FBinHex.TextWidthFitHex;
end;

function TATViewer.GetTextWidthFitUHex: Boolean;
begin
  Result := FBinHex.TextWidthFitUHex;
end;

function TATViewer.GetTextOemSpecial: Boolean;
begin
  Result := FBinHex.TextOemSpecial;
end;

function TATViewer.GetTextUrlHilight: Boolean;
begin
  Result := FBinHex.TextUrlHilight;
end;

function TATViewer.GetTextGutter: Boolean;
begin
  Result := FBinHex.TextGutter;
end;

function TATViewer.GetTextGutterLines: Boolean;
begin
  Result := FBinHex.TextGutterLines;
end;

function TATViewer.GetTextGutterLinesStep: Integer;
begin
  Result := FBinHex.TextGutterLinesStep;
end;

function TATViewer.GetTextGutterLinesBufSize: Integer;
begin
  Result := FBinHex.TextGutterLinesBufSize;
end;

function TATViewer.GetTextGutterLinesCount: Integer;
begin
  Result := FBinHex.TextGutterLinesCount;
end;

function TATViewer.GetTextGutterLinesExtUse: Boolean;
begin
  Result := FBinHex.TextGutterLinesExtUse;
end;

function TATViewer.GetTextGutterLinesExtList: AnsiString;
begin
  Result := FBinHex.TextGutterLinesExtList;
end;

function TATViewer.GetTextNonPrintable: Boolean;
begin
  Result := FBinHex.TextNonPrintable;
end;

procedure TATViewer.SetTextWidth(AValue: Integer);
begin
  FBinHex.TextWidth := AValue;
end;

procedure TATViewer.SetTextWidthHex(AValue: Integer);
begin
  FBinHex.TextWidthHex := AValue;
end;

procedure TATViewer.SetTextWidthFit(AValue: Boolean);
begin
  FBinHex.TextWidthFit := AValue;
end;

procedure TATViewer.SetTextWidthFitHex(AValue: Boolean);
begin
  FBinHex.TextWidthFitHex := AValue;
end;

procedure TATViewer.SetTextWidthFitUHex(AValue: Boolean);
begin
  FBinHex.TextWidthFitUHex := AValue;
end;

procedure TATViewer.SetTextOemSpecial(AValue: Boolean);
begin
  FBinHex.TextOemSpecial := AValue;
end;

procedure TATViewer.SetTextUrlHilight(AValue: Boolean);
begin
  FBinHex.TextUrlHilight := AValue;
end;

procedure TATViewer.SetTextGutter(AValue: Boolean);
begin
  FBinHex.TextGutter := AValue;
end;

procedure TATViewer.SetTextGutterLines(AValue: Boolean);
begin
  FBinHex.TextGutterLines := AValue;
end;

procedure TATViewer.SetTextGutterLinesStep(AValue: Integer);
begin
  FBinHex.TextGutterLinesStep := AValue;
end;

procedure TATViewer.SetTextGutterLinesBufSize(AValue: Integer);
begin
  FBinHex.TextGutterLinesBufSize := AValue;
end;

procedure TATViewer.SetTextGutterLinesCount(AValue: Integer);
begin
  FBinHex.TextGutterLinesCount := AValue;
end;

procedure TATViewer.SetTextGutterLinesExtUse(AValue: Boolean);
begin
  FBinHex.TextGutterLinesExtUse := AValue;
end;

procedure TATViewer.SetTextGutterLinesExtList(const AValue: AnsiString);
begin
  FBinHex.TextGutterLinesExtList := AValue;
end;

procedure TATViewer.SetTextNonPrintable(AValue: Boolean);
begin
  FBinHex.TextNonPrintable:= AValue;
end;

procedure TATViewer.SetSearchIndentVert(AValue: Integer);
const
  cMaxIndent = 80;
begin
  if FSearchIndentVert <> AValue then
  begin
    FSearchIndentVert := AValue;
    ILimitMin(FSearchIndentVert, 0);
    ILimitMax(FSearchIndentVert, cMaxIndent);
    FBinHex.TextSearchIndentVert := FSearchIndentVert;
  end;
end;

procedure TATViewer.SetSearchIndentHorz(AValue: Integer);
const
  cMaxIndent = 80;
begin
  if FSearchIndentHorz <> AValue then
  begin
    FSearchIndentHorz := AValue;
    ILimitMin(FSearchIndentHorz, 0);
    ILimitMax(FSearchIndentHorz, cMaxIndent);
    FBinHex.TextSearchIndentHorz := FSearchIndentHorz;
  end;
end;

procedure TATViewer.SetMediaPosition;
begin
  {$ifdef MEDIA_PLAYER}
  if (FMediaMode = vmmodeMCI) and Assigned(FMedia) then
    with FMediaBar do
      Width := Self.Width - Left;
  {$endif}

  {$ifdef MEDIA_WMP64}
  if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
    try
      with FWMP6 do 
        (IDispatch(OleObject) as IOleInPlaceObject).SetObjectRects(BoundsRect, Rect(0, 0, 32767, 32767)); 
    except
    end;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
    try
      with FWMP9 do 
        (IDispatch(OleObject) as IOleInPlaceObject).SetObjectRects(BoundsRect, Rect(0, 0, 32767, 32767)); 
    except
    end;
  {$endif}
end;

procedure TATViewer.SetMediaFit(AValue: Boolean);
begin
  if GetMediaFit <> AValue then
  begin
    FMediaFit := AValue;
    case FMode of
      vmodeMedia:
        begin
          if Assigned(FImageBox) then
            FImageBox.ImageFitToWindow := FMediaFit;

          {$ifdef MEDIA_WMP64}
          if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
            SetMediaFit_WMP6(FWMP6);
          {$endif}

          {$ifdef MEDIA_WMP9}
          if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
            SetMediaFit_WMP9(FWMP9);
          {$endif}
        end;

      {$ifdef WLX}
      vmodeWLX:
        SendWLXParams;
      {$endif}
    end;
  end;
end;

procedure TATViewer.SetMediaFitOnlyBig(AValue: Boolean);
begin
  if GetMediaFitOnlyBig <> AValue then
  begin
    FMediaFitOnlyBig := AValue;

    case FMode of
      vmodeMedia:
        if Assigned(FImageBox) then
          FImageBox.ImageFitOnlyBig := FMediaFitOnlyBig;

      {$ifdef WLX}
      vmodeWLX:
        SendWLXParams;
      {$endif}
    end;
  end;
end;

procedure TATViewer.SetMediaCenter(AValue: Boolean);
begin
  if GetMediaCenter <> AValue then
  begin
    FMediaCenter := AValue;

    case FMode of 
      vmodeMedia:
        if Assigned(FImageBox) then
          FImageBox.ImageCenter := FMediaCenter;

      {$ifdef WLX}
      vmodeWLX:
        SendWLXParams;
      {$endif}
    end;
  end;
end;

{$ifdef OFFLINE}
procedure TATViewer.SetWebOffline(AValue: Boolean);
begin
  if FWebOffline <> AValue then
  begin
    FWebOffline := AValue;
    if Assigned(FBrowser) then
    begin
      WB_SetGlobalOffline(FWebOffline);
      if (FMode = vmodeWeb) and (FBrowser.Visible) then
        WB_NavigateFilename(FBrowser, FFileName, FWebWaitForNavigate);
    end;
  end;
end;
{$endif}

procedure TATViewer.SetTextColor(AValue: TColor);
begin
  if FTextColor <> AValue then
  begin
    FTextColor := AValue;
    FBinHex.Color := FTextColor;
    if Assigned(FEdit) then
      FEdit.Color := FTextColor;
  end;
end;

function TATViewer.GetTextFont: TFont;
begin
  Result := FBinHex.Font;
end;

procedure TATViewer.SetTextFont(AValue: TFont);
begin
  FBinHex.Font := AValue;

  if Assigned(FEdit) then
    FEdit.Font := AValue;
end;

function TATViewer.GetTextFontOEM: TFont;
begin
  Result := FBinHex.FontOEM;
end;

procedure TATViewer.SetTextFontOEM(AValue: TFont);
begin
  FBinHex.FontOEM := AValue;
end;

function TATViewer.GetTextFontFooter: TFont;
begin
  Result := FBinHex.FontFooter;
end;

function TATViewer.GetTextFontGutter: TFont;
begin
  Result := FBinHex.FontGutter;
end;

procedure TATViewer.SetTextFontFooter(AValue: TFont);
begin
  FBinHex.FontFooter := AValue;
end;

procedure TATViewer.SetTextFontGUtter(AValue: TFont);
begin
  FBinHex.FontGutter:= AValue;
end;

function TATViewer.GetTextColorHex: TColor;
begin
  Result := FBinHex.TextColorHex;
end;

function TATViewer.GetTextColorHex2: TColor;
begin
  Result := FBinHex.TextColorHex2;
end;

function TATViewer.GetTextColorHexBack: TColor;
begin
  Result := FBinHex.TextColorHexBack;
end;

function TATViewer.GetTextColorLines: TColor;
begin
  Result := FBinHex.TextColorLines;
end;

function TATViewer.GetTextColorError: TColor;
begin
  Result := FBinHex.TextColorError;
end;

function TATViewer.GetTextColorGutter: TColor;
begin
  Result := FBinHex.TextColorGutter;
end;

function TATViewer.GetTextColorURL: TColor;
begin
  Result := FBinHex.TextColorURL;
end;

procedure TATViewer.SetTextColorHex(AValue: TColor);
begin
  FBinHex.TextColorHex := AValue;
end;

procedure TATViewer.SetTextColorHex2(AValue: TColor);
begin
  FBinHex.TextColorHex2 := AValue;
end;

procedure TATViewer.SetTextColorHexBack(AValue: TColor);
begin
  FBinHex.TextColorHexBack := AValue;
end;

procedure TATViewer.SetTextColorLines(AValue: TColor);
begin
  FBinHex.TextColorLines := AValue;
end;

procedure TATViewer.SetTextColorError(AValue: TColor);
begin
  FBinHex.TextColorError := AValue;
end;

procedure TATViewer.SetTextColorGutter(AValue: TColor);
begin
  FBinHex.TextColorGutter := AValue;
end;

procedure TATViewer.SetTextColorURL(AValue: TColor);
begin
  FBinHex.TextColorURL := AValue;
end;


{$ifdef SEARCH}
function TATViewer.GetSearchStarted: Boolean;
begin
  if FMode in [vmodeText, vmodeBinary, vmodeHex, vmodeUnicode] then
    Result := FBinHex.SearchStarted
  else
    Result := (FFindText <> '');
end;
{$endif}

procedure TATViewer.Enter(Sender: TObject);
begin
  FocusActiveControl;
end;

procedure TATViewer.FocusWebBrowser;
begin
  if Assigned(FBrowser) then
    if FBrowser.Visible and (FMode = vmodeWeb) then
      if CanSetFocus then
      begin
        if WebBrowserSafe then
          WB_SetFocus(FBrowser); 
      end;
end;

procedure TATViewer.FocusActiveControl;
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      if FBinHex.Visible and FBinHex.Enabled then
        FBinHex.SetFocus;

    vmodeRTF:
      if Assigned(FEdit) then
        if FEdit.Visible and FEdit.Enabled then
          FEdit.SetFocus;

    vmodeMedia:
      begin
        if FIsImage then
        begin
          if Assigned(FImageBox) then
            if FImageBox.Visible and FImageBox.Enabled then
              FImageBox.SetFocus;
        end
        else
        begin
          {$ifdef MEDIA_PLAYER}
          if Assigned(FMedia) and FMedia.Enabled then
            FMedia.SetFocus;
          {$endif}

          {$ifdef MEDIA_WMP64}
          if Assigned(FWMP6) and FWMP6.Visible then
            FWMP6.SetFocus;
          {$endif}

          {$ifdef MEDIA_WMP9}
          if Assigned(FWMP9) and FWMP9.Visible then
            FWMP9.SetFocus;
          {$endif}
        end;
      end;

    vmodeWeb:
      FocusWebBrowser;

    {$ifdef WLX}
    vmodeWLX:
      FPlugins.SetFocusToActive;
    {$endif}
  end;
end;

procedure TATViewer.Click;
begin
  SetFocus;
end;

{$ifdef MEDIA_PLAYER}

procedure TATViewer.MediaBarChange(Sender: TObject);
begin
  if Assigned(FMedia) then
    with FMedia do
      if Visible and Enabled and (not FMediaTimerBusy) then
        Position := FMediaBar.Position;
end;

procedure TATViewer.MediaTimerTimer(Sender: TObject);
begin
  FMediaTimerBusy := True;
  if Assigned(FMedia) then
    with FMedia do
      if Visible and Enabled and (FileName <> '') then
      begin
        if Position = Length then
        begin
          Position := 0;
          Stop;
        end;
        FMediaBar.Position := Position;
      end;
  FMediaTimerBusy := False;
end;

procedure TATViewer.MediaNotify(Sender: TObject);
begin
  if FMedia.Mode = MPlayer.mpStopped then
  begin
    FMedia.Notify := False;
    PreparePlaybackEnd;
    Exit
  end;
  FMedia.Notify := True;
end;
{$endif}

{$ifdef SEARCH}
function TATViewer.FindFirst(const AText: WideString; AOptions: TATStreamSearchOptions): Boolean;
var
  AEditStartPos: Integer;
begin
  Result := False;

  FFindText := AText;
  FFindOptions := AOptions;

  if FFindText = '' then Exit;

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      begin
        try
          if not Assigned(OnTextSearchProgress) then
            DoCursorHours;

          Result := FBinHex.FindFirst(FFindText, FFindOptions);
        finally
          if not Assigned(OnTextSearchProgress) then
            DoCursorDefault;
        end;

        if Result then
          with FBinHex do
            SetSelection(SearchResultStart, SearchResultLength, True);
      end;

    vmodeRTF:
      if Assigned(FEdit) then
        try
          DoCursorHours;

          AEditStartPos := 0;
          if (asoFromPage in AOptions) then
            AEditStartPos := RE_PosFromLine(FEdit, RE_CurrentLine(FEdit));

          Result := RE_FindFirst(
            FEdit,
            AText,
            AEditStartPos,
            ATSSOptionsToREOptions(AOptions),
            FSearchIndentVert,
            FSearchIndentHorz,
            FEditLastSearch);
        finally
          DoCursorDefault;
        end;

    {$ifdef WLX}
    vmodeWLX:
      try
        DoCursorHours;
        Result := FPlugins.SearchActive(
          FFindText,
          True, //AFindFirst
          asoWholeWords in FFindOptions,
          asoCaseSens in FFindOptions,
          asoBackward in FFindOptions);
      finally
        DoCursorDefault;
      end;
    {$endif}
  end;

  FFindFinished := not Result;
end;
{$endif}

{$ifdef SEARCH}
function TATViewer.FindNext(AFindPrevious: Boolean = False): Boolean;
begin
  Result := False;

  if FFindText = '' then
    begin FFindFinished := True; Exit end;

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      begin
        try
          if not Assigned(OnTextSearchProgress) then
            DoCursorHours;

          Result := FBinHex.FindNext(AFindPrevious);
        finally
          if not Assigned(OnTextSearchProgress) then
            DoCursorDefault;
        end;

        if Result then
          with FBinHex do
            SetSelection(SearchResultStart, SearchResultLength, True);
      end;

    vmodeRTF:
      if Assigned(FEdit) then
        try
          DoCursorHours;
          Result := RE_FindNext(
            FEdit,
            FSearchIndentVert,
            FSearchIndentHorz,
            FEditLastSearch);
        finally
          DoCursorDefault;
        end;

    {$ifdef WLX}
    vmodeWLX:
      try
        DoCursorHours;
        Result := FPlugins.SearchActive(
          FFindText,
          False, //AFindFirst
          asoWholeWords in FFindOptions,
          asoCaseSens in FFindOptions,
          asoBackward in FFindOptions);
      finally
        DoCursorDefault;
      end;
    {$endif}
  end;

  FFindFinished := not Result;
end;
{$endif}

{$ifdef SEARCH}
function TATViewer.FindDialog(AFindNext: Boolean): Boolean;
begin
  Result := False;

  case FMode of
    vmodeWeb:
      if Assigned(FBrowser) then
      begin
        WB_ShowFindDialog(FBrowser);
        Result := True;
      end;

    {$ifdef WLX}
    vmodeWLX:
      Result := FPlugins.SearchDialogActive(AFindNext);
    {$endif}
  end;
end;
{$endif}

procedure TATViewer.CopyToClipboard(AsHex: Boolean = False);
begin
  case FMode of
    vmodeRTF:
      if Assigned(FEdit) then
        try
          FEdit.CopyToClipboard;
        except
          MsgError(MsgViewerErrCannotCopyData);
        end;

    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.CopyToClipboard(AsHex);

    vmodeMedia:
      if Assigned(FImageBox) then
        try
          if FIsImage and (not FIsIcon) then
            Clipboard.Assign(FImageBox.Image.Picture);
        except
          MsgError(MsgViewerErrCannotCopyData);
        end;

    vmodeWeb:
      if Assigned(FBrowser) then
        WB_Copy(FBrowser);

    {$ifdef WLX}
    vmodeWLX:
      SendWLXCommand(lc_copy, 0);
    {$endif}
  end;
end;

procedure TATViewer.SelectAll;
begin
  case FMode of
    vmodeRTF:
      if Assigned(FEdit) then
        FEdit.SelectAll;

    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.SelectAll;

    vmodeWeb:
      if Assigned(FBrowser) then
        WB_SelectAll(FBrowser);

    {$ifdef WLX}
    vmodeWLX:
      SendWLXCommand(lc_selectall, 0);
    {$endif}
  end;
end;

procedure TATViewer.SelectNone;
begin
  case FMode of
    vmodeRTF:
      if Assigned(FEdit) then
        FEdit.SelLength := 0;

    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.SelectNone;

    vmodeWeb:
      if Assigned(FBrowser) then
        WB_SelectNone(FBrowser);
  end;
end;

procedure TATViewer.PreparePlaybackEnd;
begin
  if Assigned(FMediaEndTimer) then
  begin
    FMediaEndTimer.Interval := FMediaPlaylistPause;
    FMediaEndTimer.Enabled := True;
  end;
end;

procedure TATViewer.MediaEndTimerTimer;
begin
  if Assigned(FMediaEndTimer) then
  begin
    FMediaEndTimer.Enabled := False;
    DoPlaybackEnd;
  end;
end;

procedure TATViewer.DoPlaybackEnd;
begin
  if Assigned(FOnMediaPlaybackEnd) then
    FOnMediaPlaybackEnd(Self);
end;

procedure TATViewer.DoWebDocumentComplete;
begin
  if Assigned(FOnWebDocumentComplete) then
    FOnWebDocumentComplete(Self);
end;

procedure TATViewer.DoWebNavigateComplete;
begin
  if Assigned(FOnWebNavigateComplete) then
    FOnWebNavigateComplete(Self);
end;

procedure TATViewer.DoWebStatusTextChange;
begin
  if Assigned(FOnWebStatusTextChange) then
    FOnWebStatusTextChange(Self, Text);
end;

procedure TATViewer.DoWebTitleChange;
begin
  if Assigned(FOnWebTitleChange) then
    FOnWebTitleChange(Self, Text);
end;

procedure TATViewer.DoFileUnload;
begin
  if FFileName <> '' then
    if Assigned(FOnFileUnload) then
      FOnFileUnload(Self);
end;

procedure TATViewer.DoFileLoad;
begin
  if FFileName <> '' then
    if Assigned(FOnFileLoad) then
      FOnFileLoad(Self);
end;

procedure TATViewer.DoOptionsChange;
begin
  if Assigned(FOnOptionsChange) then
    FOnOptionsChange(Self);
end;

procedure TATViewer.DoLoadImageStream;
begin
  if Assigned(FOnLoadImageStream) then
    FOnLoadImageStream(Self, FImageBox, FStream);
end;

procedure TATViewer.DoLoadWebStream;
begin
  if Assigned(FOnLoadWebStream) then
    FOnLoadWebStream(Self, FBrowser, FStream);
end;


procedure TATViewer.Resize;
begin
  inherited Resize;
  SetMediaPosition;
end;


{$ifdef PRINT}
function TATViewer.PrinterCaption: AnsiString;
begin
  Result := MsgViewerCaption + ' - ' + SExtractFileName(FFileName);
end;
{$endif}

{$ifdef PRINT}
procedure TATViewer.PrintEdit(ASelectionOnly: Boolean; ACopies: Integer);
var
  OldSelStart,
  OldSelLength: Integer;
begin
  if Assigned(FEdit) then
    with FEdit do
      try
        Lines.BeginUpdate;

        OldSelStart := SelStart;
        OldSelLength := SelLength;

        FEdit.PageRect := MarginsRectPx;
        RE_Print(
          FEdit,
          ASelectionOnly,
          ACopies,
          PrinterCaption);

        //Reload file after printing
        //and restore selection
        try
          try
            DoCursorHours;
            RE_LoadFile(FEdit, FFileName, OldSelStart, OldSelLength);
          finally
            DoCursorDefault;
          end;
        except
          MsgError(SFormatW(MsgViewerErrCannotLoadFile, [FFileName]));
        end;

      finally
        Lines.EndUpdate;
      end;
end;
{$endif}


procedure TATViewer.WebBrowserDocumentComplete(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
begin
  if Assigned(FBrowser) then
    if FBrowser.Visible and (FMode = vmodeWeb) then
      if WebBrowserSafe then
      begin
        FocusWebBrowser;
        if FBorderStyleInner = bsNone then
          WB_Set3DBorderStyle(FBrowser, FBorderStyleInner <> bsNone);
      end;
  DoWebDocumentComplete;
end;

procedure TATViewer.WebBrowserNavigateComplete2(Sender: TObject; const pDisp: IDispatch; var URL: OleVariant);
begin
  DoWebNavigateComplete;
end; 

procedure TATViewer.WebBrowserStatusTextChange(Sender: TObject; const Text: WideString);
begin
  DoWebStatusTextChange(Text);
end; 

procedure TATViewer.WebBrowserTitleChange(Sender: TObject; const Text: WideString);
begin
  if Text <> 'about:blank' then
    DoWebTitleChange(Text);
end; 


{$ifdef PRINT}
procedure TATViewer.PrintDialog;
var
  ASelection: Boolean;
  {$ifdef PREVIEW}
  APrintOptions: TATPrintOptions;
  {$endif}
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode,
    vmodeRTF,
    vmodeMedia:
      begin
        InitDialogs;
        if Assigned(FPrintDialog) then
          with FPrintDialog do
          begin
            Copies := 1;
            PrintRange := prAllPages;

            MinPage := 1;
            MaxPage := MaxInt;
            FromPage := 1;
            ToPage := 1;

            case FMode of
              vmodeText,
              vmodeBinary,
              vmodeHex,
              vmodeUnicode:
                begin
                  Options := [poWarning, poPageNums];
                  ASelection := (FBinHex.SelLength > 0);
                end;
              vmodeRTF:
                begin
                  Options := [poWarning];
                  ASelection := Assigned(FEdit) and (FEdit.SelLength > 0);
                end;
              else
                begin
                  Options := [poWarning];
                  ASelection := False;
                end;
            end;

            if ASelection then
            begin
              Options := Options + [poSelection];
              PrintRange := prSelection;
            end;

            if Execute then
              case FMode of
                vmodeText,
                vmodeBinary,
                vmodeHex,
                vmodeUnicode:
                  FBinHex.Print(PrintRange, FromPage, ToPage, Copies);

                vmodeRTF:
                  PrintEdit(PrintRange = prSelection, Copies);

                {$ifdef PREVIEW}
                vmodeMedia:
                  if IsImage then
                    if Assigned(FImageBox) then
                    begin
                      APrintOptions := PrintOptions(Copies, False);
                      if PicturePrint(
                        FImageBox.Image.Picture,
                        APrintOptions) then
                        begin
                          MarginLeft := APrintOptions.OptMargins.Left;
                          MarginTop := APrintOptions.OptMargins.Top;
                          MarginRight := APrintOptions.OptMargins.Right;
                          MarginBottom := APrintOptions.OptMargins.Bottom;
                          PrintFooter := APrintOptions.OptFooter.Enabled;
                        end;
                    end;
                {$endif}
              end;
          end;
      end;

    vmodeWeb:
      if Assigned(FBrowser) then
        WB_ShowPrintDialog(FBrowser);

    {$ifdef WLX}
    vmodeWLX:
      FPlugins.PrintActive(Rect(
        Trunc(MarginLeft * 10),
        Trunc(MarginTop * 10),
        Trunc(MarginRight * 10),
        Trunc(MarginBottom * 10)));
    {$endif}
  end;
end;
{$endif}

{$ifdef PREVIEW}
function TATViewer.PrintOptions(
  ACopies: Integer;
  AFailOnErrors: Boolean): TATPrintOptions;
begin
  FillChar(Result, SizeOf(Result), 0);
  with Result do
  begin
    Copies := ACopies;
    OptFit := pFitNormal;
    OptFitSize.X := 100.0;
    OptFitSize.Y := 100.0;
    OptPosition := pPosCenter;
    with OptMargins do
    begin
      Left := MarginLeft;
      Top := MarginTop;
      Right := MarginRight;
      Bottom := MarginBottom;
    end;
    OptUnit := pUnitMm;
    OptGamma := 1.0;
    with OptFooter do
    begin
      Enabled := PrintFooter;
      Caption := SExtractFileName(FFileName);
      FontName := TextFontFooter.Name;
      FontSize := TextFontFooter.Size;
      FontStyle := TextFontFooter.Style;
      FontColor := TextFontFooter.Color;
      FontCharset := TextFontFooter.Charset;
    end;
    PixelsPerInch := Screen.PixelsPerInch;
    JobCaption := PrinterCaption;
    FailOnErrors := AFailOnErrors;
  end;
end;
{$endif}

{$ifdef PRINT}
procedure TATViewer.PrintPreview;
{$ifdef PREVIEW}
  var
    APrintOptions: TATPrintOptions;
{$endif}
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      begin
        FBinHex.PrintPreview;
      end;

    {$ifdef PREVIEW}
    vmodeMedia:
      begin
        if IsImage then
          if Assigned(FImageBox) then
          begin
            APrintOptions := PrintOptions(1, True);
            if PicturePrint(
              FImageBox.Image.Picture,
              APrintOptions) then
              begin
                MarginLeft := APrintOptions.OptMargins.Left;
                MarginTop := APrintOptions.OptMargins.Top;
                MarginRight := APrintOptions.OptMargins.Right;
                MarginBottom := APrintOptions.OptMargins.Bottom;
                PrintFooter := APrintOptions.OptFooter.Enabled;
              end;
          end;
      end;
    {$endif}

    vmodeWeb:
      begin
        if Assigned(FBrowser) then
          WB_ShowPrintPreview(FBrowser);
      end;
  end;
end;
{$endif}

{$ifdef PRINT}
procedure TATViewer.PrintSetup;
const
  cIn = 100; //For millimeters
begin
  if FMode <> vmodeWeb then
  begin
    InitDialogs;
    if Assigned(FPageSetupDialog) then
      with FPageSetupDialog do
      begin
        MarginLeft := Trunc(Self.MarginLeft) * cIn;
        MarginTop := Trunc(Self.MarginTop) * cIn;
        MarginRight := Trunc(Self.MarginRight) * cIn;
        MarginBottom := Trunc(Self.MarginBottom) * cIn;
        if Execute then
        begin
          Self.MarginLeft := MarginLeft / cIn;
          Self.MarginTop := MarginTop / cIn;
          Self.MarginRight := MarginRight / cIn;
          Self.MarginBottom := MarginBottom / cIn;
        end;
      end;
  end
  else
  begin
    if Assigned(FBrowser) then
      WB_ShowPageSetup(FBrowser);
  end;
end;
{$endif}

function TATViewer.GetPosPercent: Integer;
var
  Num: Integer;
begin
  Result := 0;

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      Result := FBinHex.PosPercent;

    vmodeRTF:
      if Assigned(FEdit) then
      begin
        Num := FEdit.Lines.Count;
        if Num = 0 then
          Result := 0
        else
          Result := RE_CurrentLine(FEdit) * 100 div Num;
      end;

    vmodeWeb:
      if Assigned(FBrowser) then
        if WebBrowserSafe then
        begin
          Num := WB_GetScrollHeight(FBrowser);
          if Num = 0 then
            Result := 0
          else
            Result := WB_GetScrollTop(FBrowser) * 100 div Num;
        end;

    {$ifdef WLX}
    vmodeWLX:
      Result := FPlugins.ActivePosPercent;
    {$endif}
  end;
end;

procedure TATViewer.SetPosPercent(APos: Integer);
begin
  ILimitMin(APos, 0);
  ILimitMax(APos, 100);

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.PosPercent := APos;

    vmodeRTF:
      if Assigned(FEdit) then
        RE_ScrollToPercent(FEdit, APos);

    vmodeWeb:
      if Assigned(FBrowser) then
        if WebBrowserSafe then
          WB_SetScrollTop(FBrowser, WB_GetScrollHeight(FBrowser) * APos div 100);

    {$ifdef WLX}
    vmodeWLX:
      SendWLXCommand(lc_setpercent, APos);
      //ActivePosPercent property is just informational
    {$endif}
  end; 
end;

function TATViewer.GetPosOffset: Int64;
begin
  Result := 0;

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      Result := FBinHex.PosOffset;

    vmodeRTF:
      if Assigned(FEdit) then
        Result := FEdit.SelStart;

    vmodeWeb:
      if Assigned(FBrowser) then
        if WebBrowserSafe then
          Result := WB_GetScrollTop(FBrowser);
  end;
end;

procedure TATViewer.SetPosOffset(const APos: Int64);
var
  Pos, Len: Integer;
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.PosOffset := APos;

    vmodeRTF:
      if Assigned(FEdit) then
      begin
        Len := Length(FEdit.Text);
        if APos <= 0 then Pos := 0 else
          if Len = 0 then Pos := 0 else
            if APos > Len - 1 then
              Pos := Len - 1
            else
              Pos := APos;
        RE_ScrollToLine(FEdit, RE_LineFromPos(FEdit, Pos), 0);
      end;

    vmodeWeb:
      if Assigned(FBrowser) then
        if WebBrowserSafe then
          WB_SetScrollTop(FBrowser, APos);
  end;
end;

{$ifdef WLX}
procedure TATViewer.HideWLX;
begin
  CloseActivePlugin;
end;

function TATViewer.LoadWLX: Boolean;
begin
  Assert(FFileName <> '', 'FileName not assigned');
  FreeData;

  Result := OpenByPlugins(False);
  //Param is False here because LoadWLX always called for the loaded file.
  //Param is True in DetectString, because DetectString always called for the new file.
end;

procedure TATViewer.SendWLXCommand(ACmd, AParam: Integer);
begin
  FPlugins.SendCommandToActive(ACmd, AParam);
end;

procedure TATViewer.SendWLXParams;
begin
  FPlugins.SendParamsToActive(
    FMediaFit,
    FMediaFitOnlyBig,
    FMediaCenter,
    FTextWrap,
    FTextEncoding = vencANSI
    );
end;

procedure TATViewer.InitPluginsParams(AParent: TWinControl; const AIniFilename: AnsiString);
begin
  FPlugins.InitParams(AParent, AIniFilename);
end;

procedure TATViewer.ResizeActivePlugin(const Rect: TRect);
begin
  FPlugins.ResizeActive(Rect);
end;

procedure TATViewer.CloseActivePlugin;
begin
  FPlugins.CloseActive;
end;

procedure TATViewer.RemovePlugins;
begin
  FPlugins.Clear;
end;

function TATViewer.AddPlugin(const AFileName: TWlxFilename; const ADetectStr: TWlxDetectString): Boolean;
begin
  Result := FPlugins.AddPlugin(AFileName, ADetectStr);
end;

function TATViewer.GetPlugin(AIndex: Word; var AFileName: TWlxFilename; var ADetectStr: TWlxDetectString): Boolean;
begin
  Result := FPlugins.GetPlugin(AIndex, AFileName, ADetectStr);
end;

function TATViewer.GetPluginsBeforeLoading: TWlxNameEvent;
begin
  Result := FPlugins.OnBeforeLoading;
end;

function TATViewer.GetPluginsAfterLoading: TWlxNameEvent;
begin
  Result := FPlugins.OnAfterLoading;
end;

procedure TATViewer.SetPluginsBeforeLoading(AProc: TWlxNameEvent);
begin
  FPlugins.OnBeforeLoading := AProc;
end;

procedure TATViewer.SetPluginsAfterLoading(AProc: TWlxNameEvent);
begin
  FPlugins.OnAfterLoading := AProc;
end;

function TATViewer.OpenByPlugins(AFileIsNew: Boolean): Boolean;
begin
  FPlugins.IsFocused := FFocused;
  Result := FPlugins.OpenMatched(
    FFileName,
    FMediaFit,
    FMediaFitOnlyBig,
    FMediaCenter,
    FTextWrap,
    FTextEncoding = vencANSI,
    AFileIsNew);
end;

function TATViewer.GetActivePluginSupportsSearch: Boolean;
begin
  Result := FPlugins.ActiveSupportsSearch;
end;

function TATViewer.GetActivePluginSupportsPrint: Boolean;
begin
  Result := FPlugins.ActiveSupportsPrint;
end;

function TATViewer.GetActivePluginSupportsCommands: Boolean;
begin
  Result := FPlugins.ActiveSupportsCommands;
end;

function TATViewer.GetActivePluginWindowHandle: THandle;
begin
  Result := FPlugins.ActiveWindowHandle;
end;

procedure TATViewer.PluginsSendMessage(const AMessage: TMessage);
begin
  case AMessage.Msg of
    WM_COMMAND:
      begin
        if AMessage.WParamHi = itm_percent then
          FPlugins.ActivePosPercent := AMessage.WParamLo;
      end;
    else
      FPlugins.SendMessageToActive(AMessage);
  end;
end;

{$endif}


//Function can be used not only in Plugins mode
function TATViewer.GetActivePluginName: AnsiString;
begin
  Result := '';

  case FMode of
    //Stub to compile when defines are commented
    vmodeText:
      Result := '';

    {$ifdef IVIEW}
    vmodeMedia:
      if FIsImage then
      begin
        if FIsImageIView then Result := ChangeFileExt(ExtractFileName(IViewIntegration.ExeName), '') else
         if FIsImageIJL then Result := 'IJLib';
      end;
    {$endif}
    
    {$ifdef WLX}
    vmodeWLX:
      Result := FPlugins.GetActiveName;
    {$endif}
  end;
end;


{$ifdef SEARCH}
function TATViewer.GetOnTextSearchProgress: TATStreamSearchProgress;
begin
  Result := FBinHex.OnSearchProgress;
end;

procedure TATViewer.SetOnTextSearchProgress(AValue: TATStreamSearchProgress);
begin
  FBinHex.OnSearchProgress := AValue;
end;
{$endif}

procedure TATViewer.EditMenuItemCopyClick(Sender: TObject);
begin
  if Assigned(FEdit) then
    try
      FEdit.CopyToClipboard;
    except
      MsgError(MsgViewerErrCannotCopyData);
    end;
end;

procedure TATViewer.EditMenuItemSelectAllClick(Sender: TObject);
begin
  if Assigned(FEdit) then
    FEdit.SelectAll;
end;

procedure TATViewer.TextSelectionChange(Sender: TObject);
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeUnicode:
      with FBinHex do
      begin
        if FTextAutoCopy then
          if SelLength > 0 then
            CopyToClipboard;
      end;

    vmodeRTF:
      if Assigned(FEdit) then
        with FEdit do
        begin
          FEditMenuItemCopy.Enabled := SelLength > 0;
          FEditMenuItemSelectAll.Enabled := not ((SelStart = 0) and (SelLength >= Length(Text)));
          if FTextAutoCopy then
            if SelLength > 0 then
              CopyToClipboard;
        end;
  end;
end;

procedure TATViewer.SetTextPopupCaption(AIndex: TATPopupCommand; const AValue: AnsiString);
begin
  FBinHex.TextPopupCaption[AIndex] := AValue;

  case AIndex of
    vpCmdCopy:
      if Assigned(FEditMenuItemCopy) then
        FEditMenuItemCopy.Caption := AValue;
    vpCmdSelectAll:
      if Assigned(FEditMenuItemSelectAll) then
        FEditMenuItemSelectAll.Caption := AValue;
  end;
end;

function TATViewer.DetectTextAndUnicode: Boolean;
const
  Enc: array[Boolean] of TATEncoding = (vencANSI, vencOEM);
var
  h: THandle;
  IsOEM: Boolean;
begin
  Result := False;

  h := FFileOpen(FFileName);
  if h = INVALID_HANDLE_VALUE then Exit;

  try
    if (not (vmodeUnicode in FModesDisabledForDetect)) and
      IsFileUnicode(h) then
        begin FMode := vmodeUnicode; Result := True end
    else

    if (not (vmodeRTF in FModesDisabledForDetect)) and
      IsFileUTF8(h) then
        begin FMode := vmodeRTF; Result := True end
    else

    if (not (vmodeWeb in FModesDisabledForDetect)) and
      IsFileWeb(h) then
        begin FMode := vmodeWeb; Result := True end
    else

    if (not (vmodeText in FModesDisabledForDetect)) and
      ((FTextDetectLimit = 0) or (FFileSize <= Int64(FTextDetectLimit) * 1024)) and
      IsFileText(h, FTextDetectSize, FTextDetectOEM, IsOEM) then
      begin
        FMode := vmodeText;
        if FTextDetectOEM then
          FTextEncoding := Enc[IsOEM];
        Result := True
      end;
  finally
    CloseHandle(h);
  end;
end;

{$ifdef NOTIF}

function TATViewer.GetTextAutoReload: Boolean;
begin
  Result := FBinHex.AutoReload;
end;

function TATViewer.GetTextAutoReloadBeep: Boolean;
begin
  Result := FBinHex.AutoReloadBeep;
end;

function TATViewer.GetTextAutoReloadFollowTail: Boolean;
begin
  Result := FBinHex.AutoReloadFollowTail;
end;

procedure TATViewer.SetTextAutoReload(AValue: Boolean);
begin
  FBinHex.AutoReload := AValue;
end;

procedure TATViewer.SetTextAutoReloadBeep(AValue: Boolean);
begin
  FBinHex.AutoReloadBeep := AValue;
end;

procedure TATViewer.SetTextAutoReloadFollowTail(AValue: Boolean);
begin
  FBinHex.AutoReloadFollowTail := AValue;
end;

function TATViewer.GetOnTextFileReload: TNotifyEvent;
begin
  Result := FBinHex.OnFileReload;
end;

procedure TATViewer.SetOnTextFileReload(AEvent: TNotifyEvent);
begin
  FBinHex.OnFileReload := AEvent;
end;

{$endif}

function TATViewer.GetTextTabSize: Integer;
begin
  Result := FBinHex.TextTabSize;
end;

procedure TATViewer.SetTextTabSize(AValue: Integer);
begin
  FBinHex.TextTabSize := AValue;
end;

function TATViewer.GetTextPopupCommands: TATPopupCommands;
begin
  Result := FBinHex.TextPopupCommands;
end;

procedure TATViewer.SetTextPopupCommands(AValue: TATPopupCommands);
begin
  FBinHex.TextPopupCommands := AValue;
end;


function TATViewer.ImageEffect(AEffect: TATViewerImageEffect): Boolean;
const
  Effects: array[TATViewerImageEffect] of TATImageEffect = (
    aieRotate90,
    aieRotate270,
    aieGrayscale,
    aieNegative,
    aieFlipVertical,
    aieFlipHorizontal
    );
begin
  Result := False;
  if Assigned(FImageBox) and Assigned(FImageBox.Image.Picture) then
  begin
    Result := PictureEffect(FImageBox.Image.Picture, Effects[AEffect], FImageColor);
    if Result then
      FImageBox.UpdateImageInfo;
  end;
end;

function TATViewer.GetTextMaxLengths(AIndex: TATBinHexMode): Integer;
begin
  Result := FBinHex.MaxLengths[AIndex];
end;

procedure TATViewer.SetTextMaxLengths(AIndex: TATBinHexMode; AValue: Integer);
begin
  FBinHex.MaxLengths[AIndex] := AValue;
end;

{$ifdef IE4X}
procedure TATViewer.WebBrowserFileDownload(Sender: TObject; ActiveDocument: WordBool; var Cancel: WordBool);
begin
  Cancel := not FWebAcceptAllFiles;
end;
{$endif}


function TATViewer.GetImageScale: Integer;
begin
  Result := 100;
  if Assigned(FImageBox) then
    Result := FImageBox.ImageScale;
end;

procedure TATViewer.SetImageScale(AValue: Integer);
begin
  if Assigned(FImageBox) then
    FImageBox.ImageScale := AValue;
end;

procedure TATViewer.ImageScaleInc;
begin
  if Assigned(FImageBox) then
    FImageBox.IncreaseImageScale(True);
end;

procedure TATViewer.ImageScaleDec;
begin
  if Assigned(FImageBox) then
    FImageBox.IncreaseImageScale(False);
end;

procedure TATViewer.TextPopupMenu(AX, AY: Integer);
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.PopupMenu.Popup(AX, AY);

    vmodeRTF:
      if Assigned(FEdit) then
        FEdit.PopupMenu.Popup(AX, AY);
  end;
end;

procedure TATViewer.TextEncodingsMenu(AX, AY: Integer);
begin
  FBinHex.TextEncodingsMenu(AX, AY);
end;

procedure TATViewer.WebGoBack;
begin
  if Assigned(FBrowser) then
    try
      FBrowser.GoBack;
    except
    end;
end;

procedure TATViewer.WebGoForward;
begin
  if Assigned(FBrowser) then
    try
      FBrowser.GoForward;
    except
    end;
end;

function TATViewer.GetSelStart: Int64;
begin
  Result := 0;

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      Result := FBinHex.SelStart;

    vmodeRTF:
      if Assigned(FEdit) then
        Result := FEdit.SelStart;
  end;
end;

procedure TATViewer.SetSelStart(const AValue: Int64);
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      with FBinHex do
        SetSelection(AValue, SelLength, False{Don't scroll});

    vmodeRTF:
      if Assigned(FEdit) then
        FEdit.SelStart := AValue;
  end;
end;

function TATViewer.GetSelLength: Int64;
begin
  Result := 0;

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      Result := FBinHex.SelLength;

    vmodeRTF:
      if Assigned(FEdit) then
        Result := FEdit.SelLength;
  end;
end;

procedure TATViewer.SetSelLength(const AValue: Int64);
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      with FBinHex do
        SetSelection(SelStart, AValue, False{Don't scroll});

    vmodeRTF:
      if Assigned(FEdit) then
        FEdit.SelLength := AValue;
  end;
end;

function TATViewer.GetSelText: AnsiString;
begin
  Result := '';

  Assert(FMode <> vmodeUnicode, 'TextSelText is called in Unicode mode');

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      Result := FBinHex.SelText;

    vmodeRTF:
      if Assigned(FEdit) then
        Result := FEdit.SelText;
  end;
end;

const
  cMaxShortLength = 256;

function TATViewer.GetSelTextShort: AnsiString;
begin
  Result := '';

  Assert(FMode <> vmodeUnicode, 'TextSelText is called in Unicode mode');

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      Result := FBinHex.SelTextShort;

    vmodeRTF:
      if Assigned(FEdit) then
        Result := Copy(FEdit.SelText, 1, cMaxShortLength);
  end;
end;

function TATViewer.GetSelTextW: WideString;
begin
  Assert(FMode = vmodeUnicode, 'TextSelTextW is called in non-Unicode mode');

  Result := FBinHex.SelTextW;
end;

function TATViewer.GetSelTextShortW: WideString;
begin
  Assert(FMode = vmodeUnicode, 'TextSelTextW is called in non-Unicode mode');

  Result := FBinHex.SelTextShortW;
end;

procedure TATViewer.TextScroll(const APos: Int64; AIndentVert, AIndentHorz: Integer);
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.Scroll(APos, AIndentVert, AIndentHorz);

    vmodeRTF:
      if Assigned(FEdit) then
        RE_ScrollToPos(FEdit, APos, AIndentVert, AIndentHorz);
  end;
end;

procedure TATViewer.IncreaseScale(AIncrement: Boolean);
begin
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.IncreaseFontSize(AIncrement);

    vmodeRTF:
      if Assigned(FEdit) then
      begin
        if TextIncreaseFontSize(FEdit.Font, Self.Canvas, AIncrement) then
          GetTextFont.Size := FEdit.Font.Size;
      end;

    vmodeMedia:
      if IsImage then
      begin
        if AIncrement then
          ImageScaleInc
        else
          ImageScaleDec;
      end;

    vmodeWeb:
      if Assigned(FBrowser) then
        if WebBrowserSafe then
          WB_IncreaseFont(FBrowser, AIncrement);
  end;
end;

function TATViewer.GetImageDrag: Boolean;
begin
  Result := FImageDrag;
  if Assigned(FImageBox) then
    Result := FImageBox.ImageDrag;
end;

procedure TATViewer.SetImageDrag(AValue: Boolean);
begin
  FImageDrag := AValue;
  if Assigned(FImageBox) then
    FImageBox.ImageDrag := AValue;
end;

function TATViewer.GetImageCursor: TCursor;
begin
  Result := FImageCursor;
  if Assigned(FImageBox) then
    Result := FImageBox.Image.Cursor;
end;

procedure TATViewer.SetImageCursor(AValue: TCursor);
begin
  FImageCursor := AValue;
  if Assigned(FImageBox) then
    FImageBox.Image.Cursor := AValue;
end;

function TATViewer.GetImageDragCursor: TCursor;
begin
  Result := FImageDragCursor;
  if Assigned(FImageBox) then
    Result := FImageBox.ImageDragCursor;
end;

procedure TATViewer.SetImageDragCursor(AValue: TCursor);
begin
  FImageDragCursor := AValue;
  if Assigned(FImageBox) then
    FImageBox.ImageDragCursor := AValue;
end;

function TATViewer.GetImageWidth: Integer;
begin
  Result := 0;
  if Assigned(FImageBox) then
    Result := FImageBox.ImageWidth;
end;

function TATViewer.GetImageHeight: Integer;
begin
  Result := 0;
  if Assigned(FImageBox) then
    Result := FImageBox.ImageHeight;
end;

function TATViewer.GetImageBPP: Integer;
var
  G: TGraphic;
const
  PN: array[TPixelFormat] of Integer =
    (0, 1, 4, 8, 15, 16, 24, 32, 0);
begin
  Result := 0;
  if Assigned(FImageBox) and
    Assigned(FImageBox.Image) and
    Assigned(FImageBox.Image.Picture) then
  begin
    G := FImageBox.Image.Picture.Graphic;

    {$ifdef GIF}
    if (G is TGifImage) then
      Result := (G as TGifImage).BitsPerPixel;
    {$endif}

    {$ifdef GEX}
    if (G is TGraphicExGraphic) then
      Result := (G as TGraphicExGraphic).ImageProperties.BitsPerPixel;
    {$endif}

    {$ifdef PNG}
    //Code similar to PngImage
    if (G is TPngObject) then
      with (G as TPngObject).Header do
        case ColorType of
          COLOR_GRAYSCALE, COLOR_PALETTE:
            Result := BitDepth;
          COLOR_RGB:
            Result := BitDepth * 3;
          COLOR_GRAYSCALEALPHA:
            Result := BitDepth * 2;
          COLOR_RGBALPHA:
            Result := BitDepth * 4;
          else
            Result := 0;
        end;
    {$endif}

    if (G is TBitmap) and (not FIsIcon) then
      Result := PN[(G as TBitmap).PixelFormat];
  end;
end;

//We need to get MediaFit from ImageBox object, since current FMediaFit
//value can be not actual (fit option can be changed during scaling)
function TATViewer.GetMediaFit: Boolean;
begin
  Result := FMediaFit;
  if (FMode = vmodeMedia) and FIsImage then
    if Assigned(FImageBox) then
      Result := FImageBox.ImageFitToWindow;
end;

function TATViewer.GetMediaFitOnlyBig: Boolean;
begin
  Result := FMediaFitOnlyBig;
  if (FMode = vmodeMedia) and FIsImage then
    if Assigned(FImageBox) then
      Result := FImageBox.ImageFitOnlyBig;
end;

function TATViewer.GetMediaCenter: Boolean;
begin
  Result := FMediaCenter;
  if (FMode = vmodeMedia) and FIsImage then
    if Assigned(FImageBox) then
      Result := FImageBox.ImageCenter;
end;

function TATViewer.GetTextMaxClipboardDataSizeMb: Integer;
begin
  Result := FBinHex.MaxClipboardDataSizeMb;
end;

procedure TATViewer.SetTextMaxClipboardDataSizeMb(AValue: Integer);
begin
  FBinHex.MaxClipboardDataSizeMb := AValue;
end;

procedure TATViewer.MediaDoPlayPause;
begin
  {$ifdef MEDIA_PLAYER}
  if (FMediaMode = vmmodeMCI) and Assigned(FMedia) then
    with FMedia do
      begin
        if Mode = MPlayer.mpPlaying then
          Pause
        else
          Play;
      end;
  {$endif}

  {$ifdef MEDIA_WMP64}
  if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
    try
      with FWMP6 do 
        begin
          if PlayState = MediaPlayer_TLB.mpPlaying then
            Pause
          else
            Play;
        end;
    except
    end;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
    try
      with FWMP9 do 
        begin
          if PlayState = MediaPlayer9_TLB.wmppsPlaying then
            Controls.Pause
          else
            Controls.Play;
        end;
    except
    end;
  {$endif}
end;


function TATViewer.GetMediaVolume: Integer;
begin
  Result:= FMediaVolume;

  {$ifdef MEDIA_WMP64}
  if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
    try
      with FWMP6 do 
        Result:= Vol_W6toA(Volume);
    except
    end;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
    try
      with FWMP9 do 
        Result:= Vol_W9toA(Settings.Volume);
    except
    end;
  {$endif}
end;

procedure TATViewer.SetMediaVolume(AValue: Integer);
begin
  FMediaVolume:= AValue;

  {$ifdef MEDIA_WMP64}
  if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
    try
      with FWMP6 do 
        Volume:= Vol_AtoW6(FMediaVolume);
    except
    end;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
    try
      with FWMP9 do 
        Settings.Volume:= Vol_AtoW9(FMediaVolume);
    except
    end;
  {$endif}
end;

function TATViewer.GetMediaMute: Boolean;
begin
  Result:= FMediaMute;

  {$ifdef MEDIA_WMP64}
  if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
    try
      with FWMP6 do 
        Result:= Mute;
    except
    end;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
    try
      with FWMP9 do 
        Result:= Settings.Mute;
    except
    end;
  {$endif}
end;

procedure TATViewer.SetMediaMute(AValue: Boolean);
begin
  FMediaMute:= AValue;

  {$ifdef MEDIA_WMP64}
  if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
    try
      with FWMP6 do 
        Mute:= FMediaMute;
    except
    end;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
    try
      with FWMP9 do 
        Settings.Mute:= FMediaMute;
    except
    end;
  {$endif}
end;

procedure TATViewer.MediaSyncVolume;
begin
  //Refresh the values of FMediaVolume/FMediaMute fields from the actual
  //WMP's properties values (if WMP object initialized)
  FMediaVolume := GetMediaVolume;
  FMediaMute := GetMediaMute;
end;

procedure TATViewer.SetOnOptionsChange(AEvent: TNotifyEvent);
begin
  FOnOptionsChange := AEvent;
  FBinHex.OnOptionsChange := FOnOptionsChange;
  if Assigned(FImageBox) then
    FImageBox.OnOptionsChange := FOnOptionsChange;
end;

function TATViewer.WebBrowserSafe: Boolean;
begin
  //Acrobat OCX can be non-safe in some cases, e.g.
  //after focusing it the program may crash on exit.
  Result := not SFileExtensionMatch(FFileName, 'pdf');
end;

function TATViewer.GetWebBusy: Boolean;
begin
  Result := False;
  if Assigned(FBrowser) then
  begin
    if FBrowser.Busy then
      FBrowser.Stop;
    Result := FBrowser.Busy;
  end;
end;


function TATViewer.GetTextEncodingName: AnsiString;
begin
  Result := '';

  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      Result := FBinHex.TextEncodingName;
    vmodeRTF:
      if Assigned(FEdit) then
        if FEdit.PlainText then
          Result := 'UTF-8'
        else
          Result := 'RTF';
  end;
end;


{$ifdef PRINT}
function TATViewer.GetMarginLeft;
begin
  Result := FBinHex.MarginLeft;
end;

function TATViewer.GetMarginTop;
begin
  Result := FBinHex.MarginTop;
end;

function TATViewer.GetMarginRight;
begin
  Result := FBinHex.MarginRight;
end;

function TATViewer.GetMarginBottom;
begin
  Result := FBinHex.MarginBottom;
end;

procedure TATViewer.SetMarginLeft;
begin
  FBinHex.MarginLeft := AValue;
end;

procedure TATViewer.SetMarginTop;
begin
  FBinHex.MarginTop := AValue;
end;

procedure TATViewer.SetMarginRight;
begin
  FBinHex.MarginRight := AValue;
end;

procedure TATViewer.SetMarginBottom;
begin
  FBinHex.MarginBottom := AValue;
end;

function TATViewer.MarginsRectPx: TRect;
begin
  Result := FBinHex.MarginsRectRealPx;
end;

function TATViewer.GetPrintFooter;
begin
  Result := FBinHex.PrintFooter;
end;

procedure TATViewer.SetPrintFooter;
begin
  FBinHex.PrintFooter := AValue;
end;
{$endif}

procedure TATViewer.SetMediaLoop(AValue: Boolean);
begin
  FMediaLoop := AValue;

  {$ifdef MEDIA_WMP64}
  if (FMediaMode = vmmodeWMP64) and Assigned(FWMP6) then
    with FWMP6 do
      if FMediaLoop then
        PlayCount := MaxInt
      else
        PlayCount := FMediaPlayCount;
  {$endif}

  {$ifdef MEDIA_WMP9}
  if (FMediaMode = vmmodeWMP9) and Assigned(FWMP9) then
    with FWMP9 do
      if FMediaLoop then
        Settings.PlayCount := MaxInt
      else
        Settings.PlayCount := FMediaPlayCount;
  {$endif}
end;


procedure TATViewer.SetPosLine(ALine: Integer);
begin
  case FMode of 
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      FBinHex.PosLine := ALine;
    vmodeRTF:
      if Assigned(FEdit) then
        RE_ScrollToLine(FEdit, ALine - 1, 0);
  end;
end;

function TATViewer.GetPosLine: Integer;
begin
  Result := 0;
  case FMode of 
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      Result := FBinHex.PosLine;
    vmodeRTF:
      if Assigned(FEdit) then
        Result := RE_CurrentLine(FEdit) + 1;
  end;
end;


function TATViewer.GetMediaShowControls: Boolean;
begin
  {$ifdef MEDIA_WMP64}
  Result := FWMP6Controls;
  {$else}
  Result := True;
  {$endif}
end;

procedure TATViewer.SetMediaShowControls(AValue: Boolean);
begin
  {$ifdef MEDIA_WMP64}
  FWMP6Controls := AValue;
  if Assigned(FWMP6) then
    FWMP6.ShowControls := AValue;
  {$endif}
end;

function TATViewer.GetMediaShowTracker: Boolean;
begin
  {$ifdef MEDIA_WMP64}
  Result := FWMP6Tracker;
  {$else}
  Result := True;
  {$endif}
end;

procedure TATViewer.SetMediaShowTracker(AValue: Boolean);
begin
  {$ifdef MEDIA_WMP64}
  FWMP6Tracker := AValue;
  if Assigned(FWMP6) then
    FWMP6.ShowTracker := AValue;
  {$endif}
end;

procedure TATViewer.TextURLClick(Sender: TObject; const S: AnsiString);
begin
  FOpenURL(S, Handle);
end;

procedure TATViewer.ImageUpDownClick(Sender: TObject; Button: TUDBtnType);
begin
  case Button of
    ComCtrls.btNext:
      if FImagePage < Pred(FImagePagesCount) then
      begin
        Inc(FImagePage);
        LoadImage(nil, False);
      end
      else
        MessageBeep(MB_ICONWARNING);
    ComCtrls.btPrev:
      if FImagePage > 0 then
      begin
        Dec(FImagePage);
        LoadImage(nil, False);;
      end
      else
        MessageBeep(MB_ICONWARNING);
  end;
end;

procedure TATViewer.ImageBoxScroll;
begin
  with FImageUpDown do
  begin
    Left := Parent.ClientWidth - Width;
    Top := 0;
    Hint := Format(MsgViewerPageHint, [Succ(FImagePage), FImagePagesCount]);
  end;
end;

procedure TATViewer.ImageBoxScrollAlt;
begin
  if Inc then
    ImageUpDownClick(Self, ComCtrls.btNext)
  else
    ImageUpDownClick(Self, ComCtrls.btPrev);
end;

procedure TATViewer.TextPanelClick;
begin
  FTextPanel.Hide;
  FMode := FModeUndetected;
  case FMode of
    vmodeText,
    vmodeBinary,
    vmodeHex,
    vmodeUnicode:
      LoadBinary;
    vmodeRTF:
      LoadRTF;
  end;
  DoFileLoad;
  DoOptionsChange;
end;

procedure TATViewer.WebWait;
begin
  if Assigned(FBrowser) then
    WB_Wait(FBrowser);
end;


{ Registration }

procedure Register;
begin
  RegisterComponents('Samples', [TATViewer]);
end;


initialization

  {$ifdef JP2}
  TJP2Image.RegisterFileFormats;
  {$endif}

  ATViewerOptionsReset;

end.
