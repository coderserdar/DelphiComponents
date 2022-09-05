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
// $Log:  21507: uFormMainOCR.pas 
//
//    Rev 1.7    05-11-2006 19:01:38  mcm    Version: IMG 3.1
// Added support for showing all images in a multi-paged file, in the image
// browser.
//
//    Rev 1.6    05-03-2006 10:52:52  mcm    Version: IMG 2.16
// Added check for image being 8 bit grey scale.
//
//   Rev 1.5    08-07-2004 23:06:46  mcm    Version: IMG 2.5
// Implemented the new TmcmOpenDialog.

//
//   Rev 1.4    30-01-2004 20:46:04  mcm    Version: IMG 2.3

//
//   Rev 1.3    24-11-2003 20:16:32  mcm

//
//   Rev 1.2    17-11-2003 10:09:24  mcm    Version: OCR 1.0

//
//   Rev 1.1    16-10-2003 11:33:14  mcm    Version: OCR 1.0

//
//   Rev 1.0    25-09-2003 23:25:48  mcm    Version: IMG 1.5

unit uFormMainOCR;

interface

{$IFDEF VER100} {$DEFINE DCB3_6}
                {$DEFINE MCMDELPHI3} {$ENDIF} { DELPHI 3  = VER100 }
{$IFDEF VER110} {$DEFINE DCB3_6}
                {$DEFINE MCMDELPHI3} {$ENDIF} { BUILDER 3 = VER110 }
{$IFDEF VER120} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER125} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER130} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER135} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER140} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER145} {$DEFINE DCB3_6} {$ENDIF}

{$IFNDEF DCB3_6} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     Menus, ToolWin, ComCtrls, StdCtrls, ExtCtrls,
     mcmTWAIN, mcmTWAINKernel, mcmTWAINIntf,
     uChildWin,
     umcmIntE,
     mcmImageTypeDef,
     mcmFileDialogues,
     mcmImage,
     mcmImageFile,
     mcmRegion,
     mcmImageColor,
     mcmImageTransform,
     mcmOCR, ImgList;

const RegKey = '\SOFTWARE\MCM DESIGN\mcmImaging\';

type
  TFormMain = class(TForm)
    MainMenu           : TMainMenu;
    // File menu
    FileMenu           : TMenuItem;
    OpenItem           : TMenuItem;
    BrowseItem         : TMenuItem;
    N1                 : TMenuItem;
    AcquireItem        : TMenuItem;
    SourceItem         : TMenuItem;
    N2                 : TMenuItem;
    ExitItem           : TMenuItem;

    // Edit menu
    EditMenu           : TMenuItem;
    CopyItem           : TMenuItem;
    CopySectionItem    : TMenuItem;
    PasteItem          : TMenuItem;
    PasteNewImageItem  : TMenuItem;
    PasteAsSectionItem : TMenuItem;
    DuplicateItem      : TMenuItem;
    EmptyClipboardItem : TMenuItem;

    // Image menu
    ImageMenu          : TMenuItem;
    InformationItem    : TMenuItem;

    // Color menu
    ColorMenu          : TMenuItem;
    UsedColorsItem     : TMenuItem;
    InvertItem         : TMenuItem;
    ThresholdItem      : TMenuItem;
    N4                 : TMenuItem;
    ConvertToMenu      : TMenuItem;
    GreyScaleItem      : TMenuItem;
    N5                 : TMenuItem;
    DecColorDepthMenu  : TMenuItem;
    Dec2Colors         : TMenuItem;
    Dec16Colors        : TMenuItem;
    Dec256Colors       : TMenuItem;
    Dec32KColors       : TMenuItem;
    IncColorDepthMenu  : TMenuItem;
    Inc16Colors        : TMenuItem;
    Inc256Colors       : TMenuItem;
    Inc32KColors       : TMenuItem;
    Inc16MillColors    : TMenuItem;
    N6                 : TMenuItem;
    SplitColorMenu     : TMenuItem;
    RGBMenu            : TMenuItem;
    RedItem            : TMenuItem;
    GreenItem          : TMenuItem;
    BlueItem           : TMenuItem;
    YCbCrMenu          : TMenuItem;
    YChannelItem       : TMenuItem;
    CbChannelItem      : TMenuItem;
    CrChannelItem      : TMenuItem;

    // Transform menu
    TransformMenu      : TMenuItem;
    StretchItem        : TMenuItem;
    FlipItem           : TMenuItem;
    MirrorItem         : TMenuItem;
    RotateItem         : TMenuItem;
    WindowsMenu        : TMenuItem;
    WindowCascadeItem  : TMenuItem;
    WindowTileItem     : TMenuItem;

    // Help - About menu
    HelpMenu           : TMenuItem;
    AboutItem          : TMenuItem;
    mcmTWAIN           : TmcmTWAIN;

    ToolBar            : TToolBar;
    tbExit             : TToolButton;
    tbOpen             : TToolButton;
    ToolButton7        : TToolButton;
    tbSelect           : TToolButton;
    tbScan             : TToolButton;
    ToolButton10       : TToolButton;
    lZoom              : TLabel;
    rsZoom             : TmcmRealSpin;
    tbFitInWindow      : TToolButton;
    ToolButton11       : TToolButton;
    tbPick             : TToolButton;
    tbSelectArea       : TToolButton;

    MenuImageList      : TImageList;
    StatusBar          : TStatusBar;
    RotateFixItem      : TMenuItem;
    Rotate180Item      : TMenuItem;
    Rotate270Item      : TMenuItem;
    IncludeWindowsPaletteItem: TMenuItem;


    OCRMenu            : TMenuItem;
    OCRLearnItem       : TMenuItem;
    LoadOCRItem        : TMenuItem;
    OCRReadItem        : TMenuItem;
    OpenDialogOCR      : TOpenDialog;
    mcmOCR             : TmcmOCR;
    mcmOpenDialog      : TmcmOpenDialog;

    procedure FormCreate(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormActivate(Sender : TObject);

    procedure ImageToolClick(Sender : TObject);

    procedure tbFitInWindowClick(Sender : TObject);
    procedure rsZoomChange(Sender : TObject);

    procedure FileMenuClick(Sender : TObject);
    procedure OpenItemClick(Sender : TObject);
    procedure BrowseItemClick(Sender : TObject);
    procedure AcquireItemClick(Sender : TObject);
    procedure SourceItemClick(Sender : TObject);
    procedure ExitItemClick(Sender : TObject);

    // Remove function call and IFDEF below which does not match your Delphi
    // version. Code Insight in Delphi fails with this IFDEF
 {$IFNDEF MCMDELPHI3}
    // Delphi 4, 5 and 6
    procedure mcmTWAINImageReady(Sender : TObject; pBmp : Pointer;
      pBmpInfo : PBitmapInfo; hImage : hBitmap; FilePath : String);
 {$ELSE}
    // Delphi 3
    procedure mcmTWAINImageReady(Sender : TObject; pBmp : Pointer;
      pBmpInfo: PBitmapInfo; hImage : Integer; FilePath : String);
 {$ENDIF}

    procedure EditMenuClick(Sender : TObject);
    procedure CopyItemClick(Sender : TObject);
    procedure CopySectionItemClick(Sender : TObject);
    procedure PasteItemClick(Sender : TObject);
    procedure PasteSectionItemClick(Sender : TObject);
    procedure DuplicateItemClick(Sender : TObject);
    procedure EmptyClipboardItemClick(Sender : TObject);

    procedure ImageMenuClick(Sender : TObject);
    procedure InformationItemClick(Sender : TObject);

    procedure ColorMenuClick(Sender : TObject);
    procedure UsedColorsItemClick(Sender : TObject);
    procedure InvertItemClick(Sender : TObject);
    procedure ThresholdItemClick(Sender : TObject);

    procedure GreyScaleItemClick(Sender : TObject);

    procedure DecColorDepthMenuClick(Sender : TObject);
    procedure IncludeWindowsPaletteItemClick(Sender : TObject);
    procedure Dec2ColorsClick(Sender : TObject);
    procedure Dec16ColorsClick(Sender : TObject);
    procedure Dec256ColorsClick(Sender : TObject);
    procedure Dec32KColorsClick(Sender : TObject);
    procedure IncColorDepthMenuClick(Sender : TObject);
    procedure Inc16ColorsClick(Sender : TObject);
    procedure Inc256ColorsClick(Sender : TObject);
    procedure Inc32KColorsClick(Sender : TObject);
    procedure Inc16MillColorsClick(Sender : TObject);

    procedure RedChannelItemClick(Sender : TObject);
    procedure GreenChannelItemClick(Sender : TObject);
    procedure BlueChannelItemClick(Sender : TObject);
    procedure YChannelItemClick(Sender : TObject);
    procedure CbChannelItemClick(Sender : TObject);
    procedure CrChannelItemClick(Sender : TObject);

    procedure TransformMenuClick(Sender : TObject);
    procedure FlipItemClick(Sender : TObject);
    procedure MirrorItemClick(Sender : TObject);
    procedure StretchItemClick(Sender : TObject);
    procedure RotateItemClick(Sender : TObject);
    procedure RotateFixItemClick(Sender : TObject);

    procedure WindowCascadeItemClick(Sender : TObject);
    procedure WindowTileItemClick(Sender : TObject);

    procedure AboutItemClick(Sender : TObject);

    procedure OCRLearnItemClick(Sender: TObject);
    procedure OCRReadItemClick(Sender: TObject);
    procedure LoadOCRItemClick(Sender: TObject);
  private
    { Private declarations }
    AppPath          : string;
    FNextImageWin    : integer;
    FScale           : double;
    {$IFDEF MCMDELPHI3}
    FStartTime       : TLargeInteger;
    FEndTime         : TLargeInteger;
    FPerformanceFreq : TLargeInteger;
    {$ELSE}
    FStartTime       : int64;
    FEndTime         : int64;
    FPerformanceFreq : int64;
    {$ENDIF}
    FImageIndex      : cardinal;
    FSaveCursor      : TCursor;
    FOCRFileName     : string;
    mcmImageColor    : TmcmImageColor;

    procedure AppException(Sender: TObject; E: Exception);
    procedure UpdateMenu;
    procedure ChildGettingFocus(Sender : TObject);
    procedure ChildLoosingFocus(Sender : TObject);
    function  CreateImageWindow(AName        : string;
                                AWidth       : integer;
                                AHeight      : integer;
                                AImageFormat : TmcmImageFormat) : TFormChild;
    procedure OpenImageFile(Sender : TObject; Filename : string; ImageIndex : integer);
    procedure OnSetProcessTime(Sender : TObject; STime, ETime :
                               {$IFDEF MCMDELPHI3}TLargeInteger{$ELSE}int64{$ENDIF});
  public
    { Public declarations }
    FChild : TFormChild;
    procedure ImageMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    procedure ImageRegionChanged(Sender : TObject; NewRegion : TRect);
    function  GetImageWindow : TFormChild;
    function  GetNextImageWindow : TFormChild;

    procedure OnShowGlyph(Sender : TObject; GlyphRect : TRect);
  end;

var FormMain : TFormMain;

implementation

{$R *.DFM}

uses Registry, Clipbrd, IniFiles,
     uFormImageInfo,
     uFormThreshold,
     uFormBrowse,
     uFormStretch,
     uFormRotate,
     uFormOCRLearn,
     uFormOCRText;

//------------------------------------------------------------------------------
// Time measurement
// Use
//    QueryPerformanceFrequency(PerformanceFreq);
// to obtain max timer frequency (ticks per second).
// Use
//    QueryPerformanceCounter(TimeTick);
// to obtain the current "time".
//------------------------------------------------------------------------------

procedure TFormMain.FormCreate(Sender : TObject);
var Reg       : TRegIniFile;
begin
  AppPath := ExtractFilePath(Application.ExeName);

  // Recall last directory and file extension.
  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_LOCAL_MACHINE; // HKEY_CURRENT_USER;
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         Left   := Reg.ReadInteger('window', 'left', 10);
         Top    := Reg.ReadInteger('window', 'top', 10);
         Width  := Reg.ReadInteger('window', 'width', 640);
         Height := Reg.ReadInteger('window', 'height', 480);

         mcmOpenDialog.InitialDir  := Reg.ReadString('', 'Path', '');
         mcmOpenDialog.FilterIndex := Reg.ReadInteger('', 'rFilter', 0);
       except
         on E:Exception
         do ;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;
  FScale := 1.0;

  //----------------------------------------------------------------------------
  // Specify which formats to support - if not all!
  // ImageFileManager.SetSupportedFormats([IF_TIFF, IF_BMP, IF_DIB]);
  mcmOpenDialog.ImageFileManager := ImageFileManager;

  // Set-up the Window menu as the menu that list's our open image windows.
  Self.WindowMenu := WindowsMenu;

  // Set-up time measurement of processes.
  QueryPerformanceFrequency(FPerformanceFreq);
  QueryPerformanceCounter(FStartTime);
  QueryPerformanceCounter(FEndTime);


  {$IFDEF MCMDELPHI3}
    StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(1000.0 * (FEndTime.QuadPart - FStartTime.QuadPart) / FPerformanceFreq.QuadPart, ffFixed, 15, 4) + ' ms';
  {$ELSE}
    StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(1000.0 * (FEndTime - FStartTime) / FPerformanceFreq, ffFixed, 15, 4) + ' ms';
  {$ENDIF}

  FImageIndex := 1; // Index used when creating Image windows.

  mcmImageColor := TmcmImageColor.Create(Self);

  // General exception handler. AppException is called when Exception are
  // unhandled, i.e. occur outside a try - except
  Application.OnException := AppException;
end; // TFormMain.FormCreate.


procedure TFormMain.FormDestroy(Sender : TObject);
var Reg : TRegIniFile;
begin
  mcmImageColor.Free;

  // Recall last directory and file extension.
  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_LOCAL_MACHINE; // HKEY_CURRENT_USER;
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         Reg.WriteInteger('window', 'left', Left);
         Reg.WriteInteger('window', 'top', Top);
         Reg.WriteInteger('window', 'width', Width);
         Reg.WriteInteger('window', 'height', Height);
       except
         on E:Exception
         do ;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;
end; // TFormMain.FormDestroy.


procedure TFormMain.FormActivate(Sender : TObject);
begin
  Self.WindowMenu := WindowsMenu;
end; // TFormMain.FormActivate.


procedure TFormMain.AppException(Sender : TObject; E : Exception);
begin
  // Unhandled exceptions
  Application.ShowException(E);
  // It's up to you how to handle exceptions! You could terminate the application.
  // Application.Terminate;
end; // TFormMain.AppException.


procedure TFormMain.OnSetProcessTime(Sender : TObject; STime, ETime : {$IFDEF MCMDELPHI3}TLargeInteger{$ELSE}int64{$ENDIF});
var Duration : double;
begin
  {$IFDEF MCMDELPHI3}
    Duration := (ETime.QuadPart - STime.QuadPart) / FPerformanceFreq.QuadPart;
    // StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(1000.0 * (ETime.QuadPart - STime.QuadPart) / FPerformanceFreq.QuadPart, ffFixed, 15, 4) + ' ms';
  {$ELSE}
    Duration := (ETime - STime) / FPerformanceFreq;
    // StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(1000.0 * (ETime - STime) / FPerformanceFreq, ffFixed, 15, 4) + ' ms';
  {$ENDIF}
  if (Duration < 1.0)
  then StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(1000.0 * Duration, ffFixed, 15, 4) + ' ms'
  else StatusBar.Panels[2].Text := 'Time: ' + FloatToStrF(Duration, ffFixed, 15, 4) + ' s';
end; // TFormMain.OnSetProcessTime.


procedure TFormMain.UpdateMenu;
var HasImage : boolean;
begin
  HasImage := False;
  if Assigned(FChild)
  then HasImage := Not(FChild.ImageCtrl.Image.Empty);

  if HasImage
  then rsZoom.Value := FChild.Scale;
  rsZoom.Enabled := HasImage;
  tbFitInWindow.Enabled := HasImage;

  EditMenu.Enabled := True;
  ImageMenu.Enabled := HasImage;
  ColorMenu.Enabled := HasImage;
  TransformMenu.Enabled := HasImage;
end; // TFormMain.UpdateMenu.


//------------------------------------------------------------------------------
// Events fire by MDI child windows.
//------------------------------------------------------------------------------

procedure TFormMain.ChildGettingFocus(Sender : TObject);
begin
  FChild := TFormChild(Sender);
  UpdateMenu;
  if Assigned(FChild)
  then begin
       if Not(FChild.Image.Empty)
       then begin
       end;
  end;
end; // End TFormMain.ChildGettingFocus.


procedure TFormMain.ChildLoosingFocus(Sender : TObject);
begin
  FChild := Nil;
  UpdateMenu;
end; // TFormMain.ChildLoosingFocus.


procedure TFormMain.ImageMouseMove(Sender : TObject;
                                   Shift  : TShiftState;
                                   X, Y   : integer);
var Color  : TColor;
    xs, ys : integer;
begin
  if Assigned(Sender)
  then begin
       xs := Trunc(x / FScale);
       ys := Trunc(y / FScale);
       if (0 <= xs) and (xs < TFormChild(Sender).Image.Width) and
          (0 <= ys) and (ys < TFormChild(Sender).Image.Height)
       then begin
            Color := TFormChild(Sender).Image.Pixel[xs, ys];
            StatusBar.Panels[0].Text := '[X,Y]: ' +
                                        IntToStr(xs) + ',' + IntToStr(ys);

            StatusBar.Panels[1].Text := 'Color [R,G,B]: ' + IntToStr(GetRValue(Color)) + ',' +
                                                            IntToStr(GetGValue(Color)) + ',' +
                                                            IntToStr(GetBValue(Color));
       end;
  end;
end; // TFormMain.ImageMouseMove.


procedure TFormMain.ImageRegionChanged(Sender : TObject; NewRegion : TRect);
begin
  // Called by the active client window, to notify that the selected region has
  // changed.
end; // TFormMain.ImageRegionChanged.


//------------------------------------------------------------------------------
// Create MDI childen.
//------------------------------------------------------------------------------

function TFormMain.CreateImageWindow(AName        : string;
                                     AWidth       : integer;
                                     AHeight      : integer;
                                     AImageFormat : TmcmImageFormat) : TFormChild;
begin
  FChild := TFormChild.Create(Self);
  FChild.LoosingFocus := ChildLoosingFocus;
  FChild.GettingFocus := ChildGettingFocus;
  if (AWidth > 0) and (AHeight > 0) and (AImageFormat <> IF_NONE)
  then begin
       FChild.Image.Width  := AWidth;
       FChild.Image.Height := AHeight;
       FChild.Image.ImageFormat := AImageFormat;
       FChild.Image.ImageInfo.DateTime := Now;
  end
  else begin
       FChild.Caption := AName + IntToStr(FImageIndex);
       inc(FImageIndex);
  end;
  FChild.OnXYColor := ImageMouseMove;
  FChild.OnRegionChanged := ImageRegionChanged;

  Result := FChild;
  FChild.GettingFocus(FChild);
  UpdateMenu;
end; // TFormMain.CreateImageWindow.


function TFormMain.GetImageWindow : TFormChild;
begin
  Result := Nil;
  FNextImageWin := 0;
  if (MDIChildCount > 0)
  then if (ActiveMDIChild is TFormChild)
       then Result := TFormChild(ActiveMDIChild)
end; // TFormMain.GetImageWindow.


function TFormMain.GetNextImageWindow : TFormChild;
begin
  Result := Nil;
  inc(FNextImageWin);
  if (MDIChildCount > 1)
  then begin
       while (FNextImageWin < MDIChildCount) and
             Not(MDIChildren[FNextImageWin] is TFormChild)
       do inc(FNextImageWin);
       if (MDIChildren[FNextImageWin] is TFormChild)
       then Result := TFormChild(MDIChildren[FNextImageWin])
  end;
end; // TFormMain.GetNextImageWindow.


//------------------------------------------------------------------------------
// Image File methods.
//------------------------------------------------------------------------------

procedure TFormMain.OpenImageFile(Sender : TObject; Filename : string; ImageIndex : integer);
var mcmImage : TmcmImage;
    hImage   : HBitmap;
    ExtName  : string;
    IncName  : string;
begin
  FSaveCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;

  QueryPerformanceCounter(FStartTime);

  // Use ImageFileManager to open image file. This allows you to return the
  // handle to a TBitmap.Handle.
  {
  FileFormat := FF_DETECT;
  FChild := CreateImageWindow(0, 0, IF_NONE);
  FChild.Image.Image.DibHandle := ImageFileManager.LoadImage(OpenDialog.FileName, ImageFormat);
  FChild.Image.Image.XResolution := Round(ImageFileManager.ImageInfo.XResolution);
  FChild.Image.Image.YResolution := Round(ImageFileManager.ImageInfo.YResolution);
  FChild.Caption := FileName;
  }

  // Read image from disk
  mcmImage := TmcmImage.Create;
  mcmImage.FileOpen(FileName);
  if Not(mcmImage.Empty)
  then begin
       FChild := CreateImageWindow(FileName, 0, 0, IF_NONE);
       FChild.Image := mcmImage;
       FChild.Caption := FileName;
  end;

  // If the file contains additional images "IsMultiImage" is True.
  if ImageFileManager.IsMultiImage
  then begin
       ExtName := ExtractFileExt(FileName);
       repeat
         // Let's load all sub-images from the file.
         hImage := ImageFileManager.LoadNext(FileName, FF_DETECT, -1);
         if (hImage <> 0)
         then begin
              IncName := ChangeFileExt(FileName, '_' + IntToStr(ImageFileManager.ImageIndex + 1) +
                                                 ExtName);
              FChild := CreateImageWindow(FileName, 0, 0, IF_NONE);
              FChild.Image.DibHandle := hImage;
              FChild.Image.ImageInfo.Assign(ImageFileManager.ImageInfo);
              FChild.Image.ImageInfo.FileName := IncName;
              FChild.Caption := IncName;
         end;
       until (hImage = 0)
  end;

  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);
  Screen.Cursor := FSaveCursor;

  if (ImageFileManager.Error <> EC_OK)
  then ShowMessage('Error reading image: ' + CErrorStrings[word(ImageFileManager.Error)]);
end; // TFormMain.OpenImageFile.


//------------------------------------------------------------------------------
// Image Tools
//------------------------------------------------------------------------------


procedure TFormMain.ImageToolClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild) and (ActiveMDIChild is TFormChild)
  then begin
       FChild.ImageTool := TImageTool((Sender as TToolButton).Tag);
  end;
end; // TFormMain.ImageToolClick.


//------------------------------------------------------------------------------
// Zoom tools
//------------------------------------------------------------------------------

procedure TFormMain.tbFitInWindowClick(Sender : TObject);
var SaveNotify : TNotifyEvent;
begin
  if Assigned(FChild)
  then begin
       FChild.ScaleToFit := True;
       SaveNotify := rsZoom.OnChange;
       rsZoom.OnChange := Nil;
       FScale := FChild.Scale;
       rsZoom.Value := FScale;
       rsZoom.OnChange := SaveNotify;
  end;
end; // TFormMain.tbFitInWindowClick.


procedure TFormMain.rsZoomChange(Sender : TObject);
begin
  if Assigned(FChild)
  then begin
       if (rsZoom.Value > 0.0)
       then begin
            FScale := rsZoom.Value;
            FChild.Scale := FScale;
            if (FScale < 1.0)
            then rsZoom.Increment := FScale
            else rsZoom.Increment := 1.0;
       end
       else begin
            FScale := FChild.Scale;
            FScale := FScale / 2.0;
            rsZoom.Value := FScale;
            rsZoom.Increment := FScale;
       end;
  end;
end; // TFormMain.rsZoomChange.


//------------------------------------------------------------------------------
// File menu
//------------------------------------------------------------------------------

procedure TFormMain.FileMenuClick(Sender : TObject);
begin
  FChild := GetImageWindow;
end; // TFormMain.FileItemClick.


procedure TFormMain.OpenItemClick(Sender : TObject);
var Reg : TRegIniFile;
    i   : integer;
begin
  if (mcmOpenDialog.Execute)
  then begin
       Update;
       FSaveCursor := Screen.Cursor;
       Screen.Cursor := crHourGlass;
       mcmOpenDialog.FileName := LowerCase(mcmOpenDialog.FileName);
       mcmOpenDialog.InitialDir := ExtractFileDir(mcmOpenDialog.FileName);

       // Store last directory and file extension.
       Reg := TRegIniFile.Create('');
       Reg.RootKey := HKEY_LOCAL_MACHINE; // HKEY_CURRENT_USER;
       if Reg.OpenKey(RegKey, True)
       then begin
            try
              Reg.WriteString('', 'Path', mcmOpenDialog.InitialDir);
              Reg.WriteInteger('', 'rFilter', mcmOpenDialog.FilterIndex);
              Reg.WriteInteger('', 'rViewStyle', integer(mcmOpenDialog.ViewStyle));
            except
              on E:Exception
              do ;
            end;
            Reg.CloseKey;
       end;
       Reg.Free;
       Screen.Cursor := FSaveCursor;

       // If multiple files were selected - open all.
       if (mcmOpenDialog.Files.Count > 1)
       then begin
            // Mutiple files were selected - now load each one.
            for i := 0 to (mcmOpenDialog.Files.Count - 1)
            do OpenImageFile(Self, mcmOpenDialog.Files[i], -1);
       end
       else OpenImageFile(Self, mcmOpenDialog.FileName, -1); // Load the selected file.
       UpdateMenu;
  end;
end; // TFormMain.OpenItemClick.


procedure TFormMain.BrowseItemClick(Sender : TObject);
begin
  if (FormBrowse = Nil)
  then begin
       FormBrowse := TFormBrowse.Create(Self);
       FormBrowse.OnProcessTime := OnSetProcessTime;
       FormBrowse.mcmThumbView.OnLoadImage := OpenImageFile;
  end;
  FormBrowse.Show;
end; // TFormMain.BrowseItemClick.


procedure TFormMain.AcquireItemClick(Sender : TObject);
begin
  mcmTWAIN.Acquire('');
end; // TFormMain.AcquireItemClick.


procedure TFormMain.SourceItemClick(Sender : TObject);
begin
  mcmTWAIN.SelectSource;
end; // TFormMain.SourceItemClick.


{$IFNDEF MCMDELPHI3}
procedure TFormMain.mcmTWAINImageReady(Sender   : TObject;
                                       pBmp     : Pointer;
                                       pBmpInfo : PBitmapInfo;
                                       hImage   : hBitmap;
                                       FilePath : String);
{$ELSE}
procedure TFormMain.mcmTWAINImageReady(Sender   : TObject;
                                       pBmp     : Pointer;
                                       pBmpInfo : PBitmapInfo;
                                       hImage   : Integer;
                                       FilePath : String);
{$ENDIF}
begin
  if (hImage <> 0) or FileExists(FilePath)
  then begin
       FChild := CreateImageWindow('TWAIN_', 0, 0, IF_NONE);
       if (hImage <> 0)
       then begin
            FChild.Image.DibHandle   := hImage;
            FChild.Image.XResolution := pBmpInfo^.bmiHeader.biXPelsPerMeter;
            FChild.Image.YResolution := pBmpInfo^.bmiHeader.biYPelsPerMeter;
       end;
       if FileExists(FilePath)
       then FChild.Image.FileOpen(FilePath);
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
  UpdateMenu;
end; // TFormMain.mcmTWAINImageReady.


procedure TFormMain.ExitItemClick(Sender : TObject);
begin
  Close;
end; // TFormMain.ExitItemClick.


//------------------------------------------------------------------------------
// Edit menu
//------------------------------------------------------------------------------

procedure TFormMain.EditMenuClick(Sender : TObject);
begin
  CopyItem.Enabled := (GetImageWindow <> Nil);
  if CopyItem.Enabled
  then CopySectionItem.Enabled := FChild.mcmRegion.Visible;
  DuplicateItem.Enabled := CopyItem.Enabled;
  PasteItem.Enabled := Clipboard.HasFormat(CF_DIB);
  EmptyClipboardItem.Enabled := (Clipboard.FormatCount > 0);
end; // TFormMain.EditMenuClick.


procedure TFormMain.CopyItemClick(Sender: TObject);
var AFormat : word;
    AData   : THandle;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       QueryPerformanceCounter(FStartTime);
       FChild.Image.SaveToClipboardFormat(AFormat, AData);
       if (AData <> 0)
       then begin
            Clipboard.Open;
            Clipboard.SetAsHandle(AFormat, AData);
            Clipboard.Close;
       end;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);
  end;
end; // TFormMain.CopyItemClick.


procedure TFormMain.CopySectionItemClick(Sender : TObject);
var AFormat     : word;
    AData       : THandle;
    RegionImage : TmcmImage;
    Region      : TRect;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       QueryPerformanceCounter(FStartTime);
       RegionImage := Nil;
       Region := FChild.Region;
       FChild.Image.CopyRegion(RegionImage, Region);
       if Assigned(RegionImage)
       then begin
            RegionImage.SaveToClipboardFormat(AFormat, AData);
            if (AData <> 0)
            then begin
                 Clipboard.Open;
                 Clipboard.SetAsHandle(AFormat, AData);
                 Clipboard.Close;
            end;
            RegionImage.Free;
       end;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);
  end;
end; // TFormMain.CopySectionItemClick.


procedure TFormMain.PasteItemClick(Sender : TObject);
var AFormat : word;
    AData   : THandle;
begin
  Clipboard.Open;
  try
    AFormat :=  CF_DIB;
    // CF_BITMAP; // is not supported yet.
    AData := Clipboard.GetAsHandle(AFormat);
    if (AData <> 0)
    then begin
         FChild := CreateImageWindow('Paste_', 0, 0, IF_NONE);
         QueryPerformanceCounter(FStartTime);
         FChild.Image.LoadFromClipboardFormat(AFormat, AData);
         FChild.Image.ImageInfo.FileName := FChild.Caption;
         QueryPerformanceCounter(FEndTime);
         OnSetProcessTime(Self, FStartTime, FEndTime);
    end;
  finally
    Clipboard.Close;
  end;
end; // TFormMain.PasteItemClick.


procedure TFormMain.PasteSectionItemClick(Sender : TObject);
var AFormat    : word;
    AData      : THandle;
    PasteImage : TmcmImage;
    DoPaste    : boolean;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       Clipboard.Open;
       try
         AFormat :=  CF_DIB;
         // CF_BITMAP; // is not supported yet.
         AData := Clipboard.GetAsHandle(AFormat);
         if (AData <> 0)
         then begin
              DoPaste := True;
              PasteImage := TmcmImage.Create;
              PasteImage.LoadFromClipboardFormat(AFormat, AData);

              if (PasteImage.ImageFormat <> FChild.Image.ImageFormat)
              then begin
                   DoPaste := False;
                   ShowMessage('The color resolution of the image you are pasting into the selected image does not match!' + Chr($0D) +
                                'Paste to a new image and modify the color resolution, copy the modified image and then paste "As section"');
              end
              else if (PasteImage.ImageFormat in [IF_PAL4,IF_PAL8]) and
                      (FChild.Image.ImageFormat in [IF_PAL4,IF_PAL8])
                   then if (MessageDlg('You are pasting a palette indexed image into another palette indexed image.' + Chr($0D) +
                                       'If the palette from both images do not match, the result will look odd!' + Chr($0D) +
                                       'Continue?', mtWarning, [mbYes,mbNo], 0) = mrNo)
                        then DoPaste := False;
              if DoPaste
              then FChild.RegionImage := PasteImage
              else PasteImage.Free;
         end;
       finally
         Clipboard.Close;
       end;
  end;
end; // TFormMain.PasteSectionItemClick.


procedure TFormMain.DuplicateItemClick(Sender : TObject);
var SourceChild : TFormChild;
    ResultChild : TFormChild;
begin
  SourceChild := GetImageWindow;
  if Assigned(SourceChild)
  then begin
       ResultChild := CreateImageWindow('Duplicate_', 0, 0, IF_NONE);
       QueryPerformanceCounter(FStartTime);
       ResultChild.ImageCtrl.Image.Assign(SourceChild.ImageCtrl.Image);
       FChild.Image.ImageInfo.FileName := FChild.Caption;
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);
  end;
end; // TFormMain.DuplicateItemClick.


procedure TFormMain.EmptyClipboardItemClick(Sender : TObject);
begin
  Clipboard.Clear;
end; // TFormMain.EmptyClipboardItemClick.


//------------------------------------------------------------------------------
// Image Menu
//------------------------------------------------------------------------------

procedure TFormMain.ImageMenuClick(Sender : TObject);
var bHasImage : boolean;
    i         : integer;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild)
  then bHasImage := Assigned(FChild.Image);

  for i := 0 to ((Sender as TMenuItem).Count - 1)
  do (Sender as TMenuItem).Items[i].Enabled := bHasImage;
end; // TFormMain.ImageMenuClick


procedure TFormMain.InformationItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormImageInfo := TFormImageInfo.Create(Self);
       FormImageInfo.Image := FChild.Image;
       FormImageInfo.ShowModal;
       FormImageInfo.Free;
  end;
end; // TFormMain.InformationItemClick.


//------------------------------------------------------------------------------
// Color menu
//------------------------------------------------------------------------------

procedure TFormMain.ColorMenuClick(Sender : TObject);
var bHasImage : boolean;
    i         : integer;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild)
  then bHasImage := Assigned(FChild.Image);

  for i := 0 to ((Sender as TMenuItem).Count - 1)
  do (Sender as TMenuItem).Items[i].Enabled := bHasImage;
end; // TFormMain.ColorMenuClick


procedure TFormMain.UsedColorsItemClick(Sender : TObject);
var NoColors : integer;
begin
  FChild := GetImageWindow;
  QueryPerformanceCounter(FStartTime);
  NoColors := FChild.Image.UniqueColors;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);
  ShowMessage('Used colors: ' + IntToStr(NoColors));
end; // TFormMain.UsedColorsItemClick


procedure TFormMain.InvertItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  mcmImageColor.SourceImage[0] := FChild.Image;
  mcmImageColor.ResultImage  := FChild.Image;

  // Get start time
  QueryPerformanceCounter(FStartTime);

  mcmImageColor.Invert;

  // Get end time and display processing time.
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild.Invalidate;
end; // TFormMain.InvertItemClick.


procedure TFormMain.ThresholdItemClick(Sender : TObject);
begin
  // Get source/MDI child window.
  FChild := GetImageWindow;

  // Create Threshold dialogue.
  FormThreshold := TFormThreshold.Create(Self);
  FormThreshold.Method := 0;
  FormThreshold.Level := 127;

  // Set source image.
  FormThreshold.SourceImage := FChild.Image;

  // Show dialogue to user.
  if (FormThreshold.ShowModal = mrOK)
  then begin
       // Get processing time.
       FStartTime := FormThreshold.TimeStart;
       FEndTime   := FormThreshold.TimeEnd;
       OnSetProcessTime(Self, FStartTime, FEndTime);

       // Create a new MDI window
       FChild := CreateImageWindow('Threshold_', 0, 0, IF_NONE);

       // Get result image from threshold dialogue and assign to MDI window.
       FChild.Image := FormThreshold.ResultImage;
       FChild.Image.ImageInfo.FileName := FChild.Caption;
  end;
  FormThreshold.Free;
end; // TFormMain.ThresholdItemClick.


//------------------------------------------------------------------------------
// Convert to
//------------------------------------------------------------------------------

procedure TFormMain.GreyScaleItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetIntensity;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Grey_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.GreyscaleItemClick.


//------------------------------------------------------------------------------
// Color decrease
//------------------------------------------------------------------------------

procedure TFormMain.DecColorDepthMenuClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       case FChild.Image.ImageFormat of
       IF_BW     : begin
                     IncludeWindowsPaletteItem.Enabled := False;
                     Dec2Colors.Enabled   := False;
                     Dec16Colors.Enabled  := False;
                     Dec256Colors.Enabled := False;
                     Dec32KColors.Enabled := False;
                   end;
       IF_GREY4,
       IF_PAL4   : begin
                     IncludeWindowsPaletteItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := False;
                     Dec256Colors.Enabled := False;
                     Dec32KColors.Enabled := False;
                   end;
       IF_GREY8,
       IF_PAL8   : begin
                     IncludeWindowsPaletteItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := True;
                     Dec256Colors.Enabled := False;
                     Dec32KColors.Enabled := False;
                   end;
       IF_RGB15,
       IF_RGB16  : begin
                     IncludeWindowsPaletteItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := True;
                     Dec256Colors.Enabled := True;
                     Dec32KColors.Enabled := False;
                   end;
       IF_RGB24  : begin
                     IncludeWindowsPaletteItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := True;
                     Dec256Colors.Enabled := True;
                     Dec32KColors.Enabled := True;
                   end;
       IF_RGBA32 : begin
                     IncludeWindowsPaletteItem.Enabled := True;
                     Dec2Colors.Enabled   := True;
                     Dec16Colors.Enabled  := True;
                     Dec256Colors.Enabled := True;
                     Dec32KColors.Enabled := True;
                   end;
       end;
  end;
end; // TFormMain.DecColorDepthMenuClick.


procedure TFormMain.IncludeWindowsPaletteItemClick(Sender : TObject);
begin
  IncludeWindowsPaletteItem.Checked := Not(IncludeWindowsPaletteItem.Checked);
end; // TFormMain.IncludeWindowsPaletteItemClick.


procedure TFormMain.Dec2ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_BW, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo2Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec2Colors1bitClick.


procedure TFormMain.Dec16ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_PAL4, IncludeWindowsPaletteItem.Checked);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo4Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec16Colors4bitClick.


procedure TFormMain.Dec256ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_PAL8, IncludeWindowsPaletteItem.Checked);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo8Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec256Colors8bitClick.


procedure TFormMain.Dec32KColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_RGB15, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('DecTo15Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Dec32KColorsClick.


//------------------------------------------------------------------------------
// Color increase
//------------------------------------------------------------------------------

procedure TFormMain.IncColorDepthMenuClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       case FChild.Image.ImageFormat of
       IF_BW     : begin
                     Inc16Colors.Enabled     := True;
                     Inc256Colors.Enabled    := True;
                     Inc32KColors.Enabled    := True;
                     Inc16MillColors.Enabled := True;
                   end;
       IF_GREY4,
       IF_PAL4   : begin
                     Inc16Colors.Enabled     := False;
                     Inc256Colors.Enabled    := True;
                     Inc32KColors.Enabled    := True;
                     Inc16MillColors.Enabled := True;
                   end;
       IF_GREY8,
       IF_PAL8   : begin
                     Inc16Colors.Enabled     := False;
                     Inc256Colors.Enabled    := False;
                     Inc32KColors.Enabled    := True;
                     Inc16MillColors.Enabled := True;
                   end;
       IF_RGB15,
       IF_RGB16  : begin
                     Inc16Colors.Enabled     := False;
                     Inc256Colors.Enabled    := False;
                     Inc32KColors.Enabled    := False;
                     Inc16MillColors.Enabled := True;
                   end;
       IF_RGB24  : begin
                     Inc16Colors.Enabled     := False;
                     Inc256Colors.Enabled    := False;
                     Inc32KColors.Enabled    := False;
                     Inc16MillColors.Enabled := False;
                   end;
       IF_RGBA32 : begin
                     Inc16Colors.Enabled     := False;
                     Inc256Colors.Enabled    := False;
                     Inc32KColors.Enabled    := False;
                     Inc16MillColors.Enabled := False;
                   end;
       end;
  end;
end; // TFormMain.IncColorDepthMenuClick.


procedure TFormMain.Inc16ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_PAL4, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo4Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Inc16Colors4bitClick.


procedure TFormMain.Inc256ColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_PAL8, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo8Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Inc256Colors8bitClick.


procedure TFormMain.Inc32KColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_RGB15, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo15Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.Inc32KColorsClick.


procedure TFormMain.Inc16MillColorsClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.ConvertTo(IF_RGB24, False);
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('IncTo24Bit_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.N16MillColors24bit1Click.


//------------------------------------------------------------------------------
// Color split
//------------------------------------------------------------------------------

procedure TFormMain.RedChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetRedChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Red_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.RedChannelItemClick.


procedure TFormMain.GreenChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetGreenChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Green_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.GreenChannelItemClick.


procedure TFormMain.BlueChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetBlueChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Blue_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.BlueChannelItemClick.


procedure TFormMain.YChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetLuminanceChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Y_Luminance_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.YChannelItemClick.


procedure TFormMain.CbChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetCbChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Cb_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.CbChannelItemClick.


procedure TFormMain.CrChannelItemClick(Sender : TObject);
var TheImage : TmcmImage;
begin
  FChild := GetImageWindow;

  mcmImageColor.ResultImage := Nil;
  mcmImageColor.SourceImage[0] := FChild.Image;
  QueryPerformanceCounter(FStartTime);
  TheImage := mcmImageColor.GetCrChannel;
  QueryPerformanceCounter(FEndTime);
  OnSetProcessTime(Self, FStartTime, FEndTime);

  FChild := CreateImageWindow('Cr_', 0, 0, IF_NONE);
  FChild.Image := TheImage;
  FChild.Image.ImageInfo.FileName := FChild.Caption;
end; // TFormMain.CrChannelItemClick.


//------------------------------------------------------------------------------
// Transform menu
//------------------------------------------------------------------------------

procedure TFormMain.TransformMenuClick(Sender : TObject);
var bHasImage : boolean;
begin
  FChild := GetImageWindow;
  bHasImage := False;
  if Assigned(FChild)
  then bHasImage := Assigned(FChild.Image);

  StretchItem.Enabled := bHasImage and
                        (FChild.Image.ImageFormat in [IF_PAL8, IF_GREY8, IF_RGB24, IF_RGBA32]);
  RotateItem.Enabled := StretchItem.Enabled;
  FlipItem.Enabled   := bHasImage;
  MirrorItem.Enabled := bHasImage and (FChild.Image.BitCount <> 4);
end; // TFormMain.TransformMenuClick.


procedure TFormMain.FlipItemClick(Sender : TObject);
var mcmImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       mcmImageTransform := TmcmImageTransform.Create(Self);
       mcmImageTransform.SourceImage[0] := FChild.Image;
       mcmImageTransform.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       mcmImageTransform.Flip;

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       mcmImageTransform.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.FlipItemClick.


procedure TFormMain.MirrorItemClick(Sender : TObject);
var mcmImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       mcmImageTransform := TmcmImageTransform.Create(Self);
       mcmImageTransform.SourceImage[0] := FChild.Image;
       mcmImageTransform.ResultImage := FChild.Image;

       // Get start time
       QueryPerformanceCounter(FStartTime);

       mcmImageTransform.Mirror;

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       mcmImageTransform.Free;
       FChild.Invalidate;
  end;
end; // TFormMain.MirrorItemClick.


procedure TFormMain.StretchItemClick(Sender : TObject);
var ImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormResize := TFormResize.Create(Self);
       FormResize.Image := FChild.Image;
       FormResize.Interpolate := ST_NEAREST; //ST_LINEAR;
       if (FormResize.ShowModal = mrOK)
       then begin
            if (FormResize.PixelWidth <> FChild.Image.Width) or
               (FormResize.PixelHeight <> FChild.Image.Height)
            then begin
                 ImageTransform := TmcmImageTransform.Create(Self);
                 ImageTransform.SourceImage[0] := FChild.Image;

                 ImageTransform.ResultImage  := TmcmImage.Create;
                 ImageTransform.ResultImage.Width   := FormResize.PixelWidth;
                 ImageTransform.ResultImage.Height  := FormResize.PixelHeight;
                 ImageTransform.ResultImage.ImageFormat := FChild.Image.ImageFormat;
                 ImageTransform.ResultImage.Palette := FChild.Image.Palette;

                 // Get start time
                 QueryPerformanceCounter(FStartTime);

                 ImageTransform.Resize(FormResize.Interpolate);

                 // Get end time and display processing time.
                 QueryPerformanceCounter(FEndTime);
                 OnSetProcessTime(Self, FStartTime, FEndTime);

                 if (ImageTransform.Error = EC_OK)
                 then begin
                      FChild := CreateImageWindow('Stretch_', 0, 0, IF_NONE);
                      FChild.Image := ImageTransform.ResultImage;
                      FChild.Image.ImageInfo.FileName := FChild.Caption;
                      FChild.Image.XResolution := FormResize.Resolution;
                      FChild.Image.YResolution := FormResize.Resolution;
                 end
                 else begin
                      ImageTransform.ResultImage.Free;
                      ShowMessage('Error scaling/stretching image: ' + CErrorStrings[word(ImageTransform.Error)]);
                 end;

                 ImageTransform.Free;
            end
            else begin
                 FChild.Image.XResolution := FormResize.Resolution;
                 FChild.Image.YResolution := FormResize.Resolution;
            end;
       end;
       FormResize.Free;
  end;
end; // TFormMain.StretchItemClick.


procedure TFormMain.RotateItemClick(Sender : TObject);
var ImageTransform : TmcmImageTransform;
    Dir            : boolean;
    Degrees        : double;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       FormRotate := TFormRotate.Create(Self);
       FormRotate.Interpolate := ST_NEAREST; //ST_LINEAR;
       FormRotate.Image := FChild.Image;

       if (FormRotate.ShowModal = mrOK)
       then begin
            Dir         := FormRotate.Direction;
            Degrees     := FormRotate.Degrees;
            ImageTransform := TmcmImageTransform.Create(Self);
            ImageTransform.SourceImage[0] := FChild.Image;

            ImageTransform.ResultImage := TmcmImage.Create;
            ImageTransform.ResultImage.ImageFormat := FChild.Image.ImageFormat;
            ImageTransform.ResultImage.Palette := FChild.Image.Palette;
            ImageTransform.BKColor := $FFFFFF;
            ImageTransform.Interpolate := FormRotate.Interpolate;

            // ImageTransform automatically creates the result image.

            // Get start time
            QueryPerformanceCounter(FStartTime);

            ImageTransform.Rotate(Dir, Degrees);

            // Get end time and display processing time.
            QueryPerformanceCounter(FEndTime);
            OnSetProcessTime(Self, FStartTime, FEndTime);

            if (ImageTransform.Error = EC_OK)
            then begin
                 FChild := CreateImageWindow('Rotate_', 0, 0, IF_NONE);
                 FChild.Image := ImageTransform.ResultImage;
                 FChild.Image.ImageInfo.FileName := FChild.Caption;
            end
            else begin
                 ImageTransform.ResultImage.Free;
                 ShowMessage('Error rotating image: ' + CErrorStrings[word(ImageTransform.Error)]);
            end;

            ImageTransform.Free;
       end;
       FormRotate.Free;
  end;
end; // TFormMain.RotateItemClick.


procedure TFormMain.RotateFixItemClick(Sender : TObject);
var ImageTransform : TmcmImageTransform;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       ImageTransform := TmcmImageTransform.Create(Self);
       ImageTransform.SourceImage[0] := FChild.Image;
       ImageTransform.BKColor := $FFFFFF;
       // As we are rotating 90,180 or 270 degrees interpolation is not
       // necessary and nearest neighbour is the right choice.
       ImageTransform.Interpolate := ST_NEAREST;
       
       // ImageTransform automatically creates the result image.
       // Get start time
       QueryPerformanceCounter(FStartTime);

       ImageTransform.Rotate(True, TMenuItem(Sender).Tag);

       // Get end time and display processing time.
       QueryPerformanceCounter(FEndTime);
       OnSetProcessTime(Self, FStartTime, FEndTime);

       if (ImageTransform.Error = EC_OK)
       then FChild.Image := ImageTransform.ResultImage
       else begin
            ImageTransform.ResultImage.Free;
            ShowMessage('Error rotating image: ' + CErrorStrings[word(ImageTransform.Error)]);
       end;

       FChild.Scale := FChild.Scale;
       ImageTransform.Free;
  end;
end; // TFormMain.RotateFixItemClick.


//------------------------------------------------------------------------------
// OCR menu
//------------------------------------------------------------------------------

procedure TFormMain.OCRLearnItemClick(Sender : TObject);
begin
  FChild := GetImageWindow;
  if Not(Assigned(FormOCRLearn))
  then FormOCRLearn := TFormOCRLearn.Create(Self)
  else FOCRFileName := FormOCRLearn.OCRFileName;
  if Assigned(FChild)
  then begin
       FormOCRLearn.Image := FChild.Image;
       FormOCRLearn.Region := FChild.Region;
       FormOCRLearn.OnShowGlyph := OnShowGlyph;
       FormOCRLearn.OnProcessTime := OnSetProcessTime;
       FormOCRLearn.OCRFileName := FOCRFileName;
  end;
  if (FormOCRLearn.Error = EC_OK)
  then FormOCRLearn.Show
  else begin
       ShowMessage('OCR Error: ' + CErrorStrings[word(FormOCRLearn.Error)]);
       FormOCRLearn.Free;
  end;
end; // TFormMain.OCRLearnItemClick.


procedure TFormMain.LoadOCRItemClick(Sender : TObject);
begin
  // Read OCR file from disk.
  if OpenDialogOCR.Execute
  then begin
       if FileExists(OpenDialogOCR.FileName)
       then FOCRFileName := OpenDialogOCR.FileName
       else FOCRFileName := '';
  end;
end; // TFormMain.LoadOCRItemClick.


procedure TFormMain.OCRReadItemClick(Sender : TObject);
var i : integer;
begin
  FChild := GetImageWindow;
  if Assigned(FChild)
  then begin
       if Not(FileExists(FOCRFileName))
       then LoadOCRItemClick(Sender);
       // Read OCR file from disk.
       if FileExists(FOCRFileName)
       then begin
            mcmOCR.LoadFromFile(FOCRFileName);
            mcmOCR.Image := FChild.Image;
            if (mcmOCR.Error = EC_OK)
            then begin
                 mcmOCR.Region := FChild.Region;

                 // Get start time
                 QueryPerformanceCounter(FStartTime);

                 mcmOCR.Threshold;
                 mcmOCR.LocateGlyphs;
                 mcmOCR.ReadText;

                 // Get end time and display processing time.
                 QueryPerformanceCounter(FEndTime);
                 OnSetProcessTime(Self, FStartTime, FEndTime);

                 if (mcmOCR.NoLines > 0)
                 then begin
                      // Create text window that receives characters.
                      FormOCRText := TFormOCRText.Create(Self);
                      FormOCRText.Memo.Clear;
                      for i := 0 to (mcmOCR.NoLines - 1)
                      do FormOCRText.Memo.Lines.Add(mcmOCR.Text[i]);
                 end;
                 // Show found text lines.
                 //for i := 0 to (mcmOCR.NoLines - 1)
                 //do FChild.AddRegion(mcmOCR.LineRect[i], CLBLUE);
            end
            else begin
                 ShowMessage('OCR Error: ' + CErrorStrings[word(mcmOCR.Error)]);
            end;
       end;
       //FChild.Invalidate;
  end;
end; // TFormMain.OCRReadItemClick.


procedure TFormMain.OnShowGlyph(Sender : TObject; GlyphRect : TRect);
begin
  FChild.ClearRegions;
  if (GlyphRect.Left = -1)
  then FOCRFileName := FormOCRLearn.OCRFileName
  else FChild.AddRegion(GlyphRect, CLBLUE);
end; // TFormMain.OnShowGlyph.


//------------------------------------------------------------------------------
// Window menu
//------------------------------------------------------------------------------

procedure TFormMain.WindowCascadeItemClick(Sender : TObject);
begin
  Cascade;
end; // TFormMain.WindowCascadeItemClick.


procedure TFormMain.WindowTileItemClick(Sender : TObject);
begin
  TileMode := tbVertical;
  Tile;
end; // TFormMain.WindowTileItemClick.


//------------------------------------------------------------------------------
// About menu
//------------------------------------------------------------------------------

procedure TFormMain.AboutItemClick(Sender : TObject);
begin
  mcmOCR.About;
end; // TFormMain.AboutItemClick.

{$UNDEF MCMDELPHI3}


end.
