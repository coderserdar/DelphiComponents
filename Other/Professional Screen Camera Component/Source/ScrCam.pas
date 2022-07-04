{
Professional Screen Camera Component (Delphi 7 to above)
Developed 2008 by Mohammad Reza Hanifeh Pour (MRH Software Co.)
Author E-Mail: mrh.info2007@gmail.com
Centeral Office Tel: +98(21)(7764)(4130).   Everyday 9AM ~ 4PM.
Office Address: F2 29 Rezai St. Namjo Av. Tehran-Iran.
................................................................................
Version history:

v4.7.1.0: Updated 01/01/2009
    New features:
      1) Add unit hightimar.pas for a threaded timer in preview or recording.
      2) Add canvas.trylock and canvas.unlock for all parts of image processing.
      3) Included all necessary units of Wave Audio Package and TEffect in my component. 
    Modify features:
      1) Fixed some routines in function PowerDeleteFile, Because long time waiting for deleting a file.
    Remove features:
      No thing
 
v4.4.1.1: Updated 12/11/2008
    New features:
      1) Screen Camera Unit converted to component packege (Delphi 7 to above)
      2) Add info frame rate to preview routine
    Modify features:
      1) Replaced PreviewScreenFrame routine with CaptureScreenFrame routine in preview mode
    Remove features:
      1) Delete PreviewScreenFrame routine, Because between record and preview
         eventuate to memory stack overflow

v4.2.2.1: Updated 12/03/2008
    New features:
      1) Add recording from multi monitor
      2) Add Noise effect to image effects
    Modify features:
      1) Fixed some errors
      2) Fixed memory overflow in low frame rate
    Remove features:
      1) Remove solarize filter effect from image effects

v4.0.1.0: Updated 11/18/2008
    New features:
      1) Add grayscale drawing (Capture And Preview)
      2) Add some image effects (Rotation, Brightness, Contrast, Color Adjusting, Saturation, Solarize)
    Modify features:
      1) Fixed some errors
    Remove features:
      No thing

v3.8.2.0: Updated 04/03/2008
    New features:
      No thing
    Modify features:
      1) Fixed error on selecting audio input.
    Remove features:
      No thing

v3.8.1.0: Updated 03/18/2008
    New features:
      1) Add overlay event for draw objects, picture, text and more over image.
      2) Add deleting event.
      3) Add correct frame rate info.
    Modify features:
      1) correction elapsed timer.
    Remove features:
      No thing

v3.5.3.2: Updated 03/07/2008
    New features:
      No thing
    Modify features:
      1) Canceling select region from object and windows on start record, that correct.
      2) Not synchronized record time with play time in full auto mode, that correct.
      3) Corrected some internal errors.
    Remove features:
      1) Remove capture timer and elapsed timer and add into record routin.
      2) Remove sleep timer on record (For full motion).

v3.5.0.1: Updated 02/28/2008
    New features:
      1) Upper interval TTimer (Because, sometimes system error).
      2) Lower sleep on upper frame rate during record (Softer motion).
      3) Not delete already temp audio/video files from temp directory, But can now.
      4) Add freehand window for free select region.
      5) Add select object window for select region from object or
          windows under mouse pointer.
    Modify features:
      No thing
    Remove features:
      1) Remove recompressing after record (Because, Some codecs, more the size of file).

v3.0.0.0: Released 11/20/2007
    First release.
................................................................................
}

{$WARNINGS OFF}
{$HINTS OFF}
{$RANGECHECKS OFF}

unit ScrCam;

interface

uses
  // Use professional screen camera units
  Vfw,
  FlashWnd,
  FreeHandWnd,
  SelObjWnd,
  scConsts,
  scAboutBox,
  HighTimer,
  //=======================================
  // a free component by www.myart.bz/pisarev.net   or   www.pisarev.net
  scEffects,
  //=======================================
  // a free component by www.delphiarea.com
  scWaveMixer,
  scWaveUtils,
  scWaveRecorders,
  //=======================================
  // Use delphi standard units
  Windows, Messages,
  SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms,
  Dialogs, StdCtrls,
  ExtCtrls, MMSystem,
  JPEG, Math;
  //=======================================

type
  TSCEvent = procedure(Sender: TObject) of object;
  TSCErrorEvent = procedure(Sender: TObject;
    ErrorMessage: string) of object;
  TPreviewEvent = procedure(Sender: TObject; PreviewBitmap: TBitmap;
    Preview: Boolean; Recording: Boolean) of object;
  TSaveEvent = procedure(Sender: TObject; Percent: Integer;
    StatusCaption: String; var Continue: Boolean) of object;
  TOverlayEvent = procedure(Sender: TObject; HDCBitmap: HDC;
    bmpWidth, bmpHeight: Integer) of object;
  TRecordAVIThread = class;
  TTimerRecord = record
    TimerON : Boolean;
    Hour    : Byte;
    Min     : Byte;
    Sec     : Byte;
  end;
  THMSM = record
    Hour        : Integer;
    Minute      : Integer;
    Second      : Integer;
    MilliSecond : Integer;
  end;
  TGetAudioInfo = record
    AudioInputNames   : TStringList;
    AudioInputIndex   : Integer;
    AudioInputVolume  : Integer;
    AudioInputEnabled : Boolean;
  end;
  TScreenRegion = (SelObject, FreeHand, FixedMoving, FixedStable, FullScreen);
  TScreenRotate = (R90D, R180D, R270D, Mirror0D, Mirror180D);
  TOperation = (None, Success, Fail);
  TICINFOS = array[0..50] of TICINFO;

  TScreenCamera = class(TComponent)
  private
    FCurrentCapturedFPS,
    FRealCapturingFPS     : Extended;
    FourCC,
    CompfccHandler        : DWORD;
    FOnUpdate,
    FOnStart,
    FOnStop               : TSCEvent;
    FOnError              : TSCErrorEvent;
    FOnPreview            : TPreviewEvent;
    FOnSaving,
    FOnDeleting           : TSaveEvent;
    FOnOverlay            : TOverlayEvent;
    FVideoCompressorInfo  : TICINFOS;
    FFrame                : TFlashingWnd;
    FPreviewTimer         : THighTimer;
    FCursorPos            : TMouse;
    FRecordAVIThread      : TRecordAVIThread;
    nColors               : TPixelFormat;
    Bits,
    MaxXScreen,
    MaxYScreen,
    FComputedFrameNo,
    FActualFrameNo,
    FSkippedFrames,
    FHotKey1,
    FHotKey2,
    FHotKey3,
    FPlaybackFPS,
    FMSPFRecord,
    FKeyFramesEvery,
    FSelectedCompressor,
    FCompressionQuality,
    FAudioFormatsIndex,
    FFilterCopy,
    FCurrentMonitor,
    FScreenLeft,
    FScreenTop,
    FScreenWidth,
    FScreenHeight,
    FNoise,
    FSaturation,
    FBrightness,
    FContrast,
    FRedValue,
    FGreenValue,
    FBlueValue,
    FUpdateRate,
    FVideoCompressorCount : Integer;
    FPriority             : TThreadPriority;
    FRecordCursor,
    FAudioRecord,
    FFlashingRect,
    FLineRectClear,
    FMinimize,
    FRestore,
    FShowPreview,
    RecordState,
    FSelectObject,
    FAutoPan,
    FFullScreen,
    FSelCodecRepeat,
    FOvelayDrawing,
    FEffectToOvelayDraw,
    FGrayScale,
    FRecordAllMonitors,
    FUseColorAdjust,
    FUseContrast,
    FUseBrightness,
    FUseColorAdjusting,
    FUseSaturation,
    FUseNoise,
    FUseRotateImage,
    FOnRecordPreview,
    FFreeHandMode         : Boolean;
    FVideoCodecList,
    FAudioFormatList      : TStringList;
    FTimerRecord          : TTimerRecord;
    FAbout,
    StrCodec,
    FElapsedTime          : String;
    FWndHandle            : HWnd;
    FScreenRegion         : TScreenRegion;
    FScreenRotate         : TScreenRotate;
    FAudioInfo            : TGetAudioInfo;
    TempCompressed        : APAVIStream;
    //..........................................................................
    procedure ConvertToGrayScale(var AnImage: TBitmap);
    procedure SetCurrentMonitor(Value: Integer);
    procedure SetBrightness(Value: Integer);
    procedure SetContrast(Value: Integer);
    procedure SetNColors(Value: TPixelFormat);
    procedure SetPriority(Value: TThreadPriority);
    procedure SetInterval(Value: Integer);
    procedure SetUpdateRate(Value: Integer);
    procedure SetQuality(Value: Integer);
    procedure SetRecord_MSPF(Value: Integer);
    procedure SetFilterCopy(Value: Integer);
    procedure SetScreenWidth(Value: Integer);
    procedure SetScreenHeight(Value: Integer);
    procedure SetLineRectClear(Value: Boolean);
    procedure SetShowPreview(Value: Boolean);
    procedure GlobalHotKey(var Msg: TMessage);
    procedure ThreadDone(Sender: TObject);
    procedure FFrameMinimize(Sender: TObject);
    procedure FShowPreviewTimer(Sender: TObject);
    procedure LoadAVIFileToStream(const FileName: String;
      var TempAVIStream: PAVIStream);
    procedure SetVersion(Value: String);
    function FinalSaveAvi(const FileName: String; nStreams: Integer;
      Streams: APAVISTREAM): Boolean;
    function FFreeHandFrameDraw: Boolean;
    function FSelObjectFrameDraw: Boolean;
    function GetVideoCompressorsInfo: TStringList;
    function GetAudioFormatsInfo: TStringList;
    function GetAudioInputsInfo: TGetAudioInfo;
    function GetMonitorsCount: Integer;
    function GetCurrentMonitor: Integer;
    function GetBrightness: Integer;
    function GetContrast: Integer;
    function GetNColors: TPixelFormat;
    function GetPriority: TThreadPriority;
    function GetInterval: Integer;
    function GetUpdateRate: Integer;
    function GetQuality: Integer;
    function GetRecord_MSPF: Integer;
    function GetFilterCopy: Integer;
    function GetScreenWidth: Integer;
    function GetScreenHeight: Integer;
    function GetLineRectClear: Boolean;
    function GetShowPreview: Boolean;
    function CaptureScreenFrame(Mode: Integer; TempImage: TBitmap;
      Left, Top, Width, Height: Integer; CopyMode: Integer): PBitmapInfoHeader;
    function RecordVideo(szFileName: String): Integer;
    function HBitmap2DDB(HBitmap: HBitmap; nBits: LongWord): THandle;
    function MilliSecond2Time(TimeAtMillisecond: Extended): THMSM;
    function GetVersion: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override; 
    procedure ShowAbout;
    procedure SetAudioInputIndex(Index: Integer);
    procedure SetAudioInputVolume(Index, Volume: Integer);
    procedure StopRecording;
    procedure CompressorHasFeatures(Compressor: Byte;
      var HasAbout: Boolean; var hasConfig: Boolean);
    procedure CompressorAbout(Compressor: Byte; WND: HWND);
    procedure CompressorConfigure(Compressor: Byte; WND: HWND);
    procedure SetSizeFullScreen;
    procedure GetMinimumScreenSize(var W, H: Integer);
    procedure GetMaximumScreenSize(var W, H: Integer);
    function StartRecording(szFileName: String): Boolean;
    // report values (read-only)
    property CapturedFrames     : Integer          read FActualFrameNo;     // Number of captured frames
    property DropedFrames       : Integer          read FSkippedFrames;     // show skipped frames on recording
    property RealCapturingFPS   : Extended         read FRealCapturingFPS;  // Real capturing FPS rate = should be Playback-fps rate on fast machines!
    property CurrentCapturedFPS : Extended         read FCurrentCapturedFPS;// Current captured FPS rate = should be Playback-fps rate on fast machines!
    property ElapsedTime        : String           read FElapsedTime;       // Show elapsed time
    property VideoCodecName     : String           read StrCodec;           // Show used codec name
    property VideoCodecsList    : TStringList      read GetVideoCompressorsInfo; // Get list of video codecs
    property AudioFormatsList   : TStringList      read GetAudioFormatsInfo;// Get of list audio formats
    property GetAudioInputInfo  : TGetAudioInfo    read GetAudioInputsInfo; // Get audio input information
    property IsRecording        : Boolean          read RecordState;        // Test recording state
    property MonitorsCount      : Integer          read GetMonitorsCount;   // Get number of monitors
    // options
    property ShowPreview        : Boolean          read GetShowPreview      write SetShowPreview;
    property AudioFormatsIndex  : Integer          read FAudioFormatsIndex  write FAudioFormatsIndex;
  protected
  published
    // options
    property About              : String           read FAbout              write FAbout stored False;
    property PSC_Version        : String           read GetVersion          write SetVersion;
    property CurrentMonitor     : Integer          read GetCurrentMonitor   write SetCurrentMonitor;
    property RecordAllMonitors  : Boolean          read FRecordAllMonitors  write FRecordAllMonitors;
    property UpdateRate         : Integer          read GetUpdateRate       write SetUpdateRate;
    property PlayBack_FPS       : Integer          read GetInterval         write SetInterval;
    property Record_MSPF        : Integer          read GetRecord_MSPF      write SetRecord_MSPF;
    property KeyFramesEvery     : Integer          read FKeyFramesEvery     write FKeyFramesEvery;
    property CompressionQuality : Integer          read GetQuality          write SetQuality;
    property Colors             : TPixelFormat     read GetNColors          write SetNColors;
    property SelectedCompressor : Integer          read FSelectedCompressor write FSelectedCompressor;
    property ScreenRegion       : TScreenRegion    read FScreenRegion       write FScreenRegion;
    property ScreenLeft         : Integer          read FScreenLeft         write FScreenLeft;
    property ScreenTop          : Integer          read FScreenTop          write FScreenTop;
    property ScreenWidth        : Integer          read GetScreenWidth      write SetScreenWidth;
    property ScreenHeight       : Integer          read GetScreenHeight     write SetScreenHeight;
    property UseAudioRecord     : Boolean          read FAudioRecord        write FAudioRecord;
    property RecordCursor       : Boolean          read FRecordCursor       write FRecordCursor;
    property DrawAreaCapture    : Boolean          read FFlashingRect       write FFlashingRect;
    property LineRectClear      : Boolean          read GetLineRectClear    write SetLineRectClear;
    property MinimizeAppOnStart : Boolean          read FMinimize           write FMinimize;
    property RestoreAppOnStop   : Boolean          read FRestore            write FRestore;
    property VideoPriority      : TThreadPriority  read GetPriority         write SetPriority;
    property SetTimer           : TTimerRecord     read FTimerRecord        write FTimerRecord;
    property FilterColor        : Integer          read GetFilterCopy       write SetFilterCopy;
    property OvelayDrawing      : Boolean          read FOvelayDrawing      write FOvelayDrawing;
    property EffectToOvelayDraw : Boolean          read FEffectToOvelayDraw write FEffectToOvelayDraw;
    property GrayScale          : Boolean          read FGrayScale          write FGrayScale;
    property EffectScreenRotate : TScreenRotate    read FScreenRotate       write FScreenRotate;
    property UseRotateImage     : Boolean          read FUseRotateImage     write FUseRotateImage;
    property EffectNoise        : Integer          read FNoise              write FNoise;
    property UseNoise           : Boolean          read FUseNoise           write FUseNoise;
    property EffectSaturation   : Integer          read FSaturation         write FSaturation;
    property UseSaturation      : Boolean          read FUseSaturation      write FUseSaturation;
    property EffectRedValue     : Integer          read FRedValue           write FRedValue;
    property EffectGreenValue   : Integer          read FGreenValue         write FGreenValue;
    property EffectBlueValue    : Integer          read FBlueValue          write FBlueValue;
    property UseColorAdjusting  : Boolean          read FUseColorAdjusting  write FUseColorAdjusting;
    property EffectBrightness   : Integer          read GetBrightness       write SetBrightness;
    property UseBrightness      : Boolean          read FUseBrightness      write FUseBrightness;
    property EffectContrast     : Integer          read GetContrast         write SetContrast;
    property UseContrast        : Boolean          read FUseContrast        write FUseContrast;
    // events
    property OnError            : TSCErrorEvent    read FOnError            write FOnError;
    property OnUpdate           : TSCEvent         read FOnUpdate           write FOnUpdate;
    property OnStart            : TSCEvent         read FOnStart            write FOnStart;
    property OnStop             : TSCEvent         read FOnStop             write FOnStop;
    property OnPreview          : TPreviewEvent    read FOnPreview          write FOnPreview;
    property OnSaving           : TSaveEvent       read FOnSaving           write FOnSaving;
    property OnDeleting         : TSaveEvent       read FOnDeleting         write FOnDeleting;
    property OnOverlay          : TOverlayEvent    read FOnOverlay          write FOnOverlay;
  end;

  TRecordAVIThread = class(TThread)
  private
    FScrCam: TScreenCamera;
  protected
    procedure Execute; override;
  public
    constructor Create(ScrCam: TScreenCamera);
  end;

implementation

var
  TimeExpended1,
  OldUpdateTime1,
  InitialTime1,
  OldTime1,
  OldTime2           : Extended;
  FSuccess           : TOperation;
  SC                 : TScreenCamera;
  FRegColor          : TColor = clRed;
  TempVideoFile,
  TempAudioFile,
  FFileName          : String;
  CancelRecording,
  SavingSuccess,
  StartRegionSel     : Boolean;
  Hur, Min, Sec,
  Mil, X1, Y1,
  X2, Y2             : Integer;

procedure FreeFrame(var alpbi: PBitmapInfoHeader);
begin
	if alpbi <> nil then begin
//    ZeroMemory(alpbi, SizeOf(TBitmapInfoHeader));
   	GlobalFreePtr(alpbi);
	  alpbi := nil;
    end;
end;

procedure TScreenCamera.ConvertToGrayScale(var AnImage: TBitmap);
var
  BMPImage  : TBitmap;
  JPGImage  : TJPEGImage;
  MemStream : TMemoryStream;
begin
  BMPImage := TBitmap.Create;
  try
    BMPImage.Width  := AnImage.Width;
    BMPImage.Height := AnImage.Height;
    JPGImage := TJPEGImage.Create;
    try
      JPGImage.Assign(AnImage);
      JPGImage.CompressionQuality := 100;
      JPGImage.Compress;
      JPGImage.Grayscale := True;
      BMPImage.Canvas.Draw(0, 0, JPGImage);
      if Assigned(BMPImage) then
        BMPImage.PixelFormat := nColors;
      MemStream := TMemoryStream.Create;
      try
        BMPImage.SaveToStream(MemStream);
        //you need to reset the position of the MemoryStream to 0
        MemStream.Position := 0;
        AnImage.LoadFromStream(MemStream);
      finally
        MemStream.Clear;
        MemStream.Free;
      end;
    finally
      JPGImage.Free;
    end;
  finally
    BMPImage.Free;
  end;
end;

{ If you want to get rid of a file normally you just delete it.
  But someone else can undelete it if the file hasn't been wiped correctly.
  For security purposes, to insure that certain files are permanently
  gone, the WipeFile procedure writes over the data in the file with
  random characters and then erases it. }
function PowerDeleteFile(FileName: String): Boolean;
const
  TryCount = 50;
var
  I         : Integer;
  C         : Boolean;
  OldCursor : TCursor;

  // Check file is used or not?
  function IsFileInUse(FileName: String): Boolean;
  var
    hFileRes: HFile;
  begin
    Result := False;
    if not FileExists(FileName) then Exit;
    hFileRes := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0,
                           nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    Result := (hFileRes = INVALID_HANDLE_VALUE);
    if not Result then CloseHandle(hFileRes);
  end;
begin
  OldCursor := Screen.Cursor;
  Screen.Cursor := crAppStart;
  C := True;
  if (not IsFileInUse(FileName)) and (FileExists(FileName)) then begin
    I := 1;
    while I <= TryCount do begin
      if FileExists(FileName) then begin
        if Assigned(SC) then begin
          if Assigned(SC.OnDeleting) then
            SC.OnDeleting(SC, I * 2, DeletingMsg, C);
          if not C then begin
            SC.OnDeleting(SC, 100, '', C);
            Result := False;
            Break;
            end;
          end;
        Result := DeleteFile(FileName);
        end
      else begin
        if Assigned(SC) then
          if Assigned(SC.OnDeleting) then
            SC.OnDeleting(SC, 100, '', C);
        Result := True;
        Break;
        end;
      Inc(I);
      end;
    end
  else begin
    I := 1;
    while I <= TryCount do begin
      if FileExists(FileName) then begin
        if Assigned(SC) then begin
          if Assigned(SC.OnDeleting) then
            SC.OnDeleting(SC, I * 2, DeletingMsg, C);
          if not C then begin
            SC.OnDeleting(SC, 100, '', C);
            Result := False;
            Break;
            end;
          end;
        Result := MoveFileEx(PChar(FileName),
                             nil,
                             MOVEFILE_REPLACE_EXISTING or
                             MOVEFILE_DELAY_UNTIL_REBOOT);
        end
      else begin
        if Assigned(SC) then
          if Assigned(SC.OnDeleting) then
            SC.OnDeleting(SC, 100, '', C);
        Result := True;
        Break;
        end;
      Inc(I);
      end;
    end;
  Screen.Cursor := OldCursor;
end;

function TScreenCamera.MilliSecond2Time(TimeAtMillisecond: Extended): THMSM;
var
  HMSM : THMSM;
  S1,
  S2   : String;
begin
  HMSM.Hour   := ABS(Trunc(TimeAtMillisecond / 3600000));
  HMSM.Minute := ABS(Trunc((TimeAtMillisecond - (HMSM.Hour * 3600000)) / 60000));
  HMSM.Second := ABS(Trunc((TimeAtMillisecond -
                          ((HMSM.Hour * 3600000) +
                           (HMSM.Minute * 60000))) / 1000));
  S1 := FloatToStr(Frac(ABS((TimeAtMillisecond -
                           ((HMSM.Hour * 3600000) +
                            (HMSM.Minute * 60000))) / 1000)));
  if Pos('0.', S1) <> 0 then begin
    Delete(S1, Pos('0.', S1), 2);
    S2 := Copy(S1, 1, 3);
    HMSM.MilliSecond := StrToInt(S2);
    end
  else
    HMSM.MilliSecond := 0;
  Result := HMSM;
end;

//---------------------------------------------------

procedure TScreenCamera.GetMinimumScreenSize(var W, H: Integer);
begin
  if FRecordAllMonitors then begin
    W := Screen.DesktopLeft;
    H := Screen.DesktopTop;
    end
  else begin
    W := Screen.Monitors[FCurrentMonitor - 1].Left;
    H := Screen.Monitors[FCurrentMonitor - 1].Top;
    end;
end;

//---------------------------------------------------

procedure TScreenCamera.GetMaximumScreenSize(var W, H: Integer);
begin
  if FRecordAllMonitors then begin
    W := Screen.DesktopWidth;
    H := Screen.DesktopHeight;
    end
  else begin
    W := Screen.Monitors[FCurrentMonitor - 1].Width;
    H := Screen.Monitors[FCurrentMonitor - 1].Height;
    end;
end;

//---------------------------------------------------

procedure TScreenCamera.SetSizeFullScreen;
begin
  if FRecordAllMonitors then begin
    FScreenLeft := Screen.DesktopLeft;
    FScreenTop  := Screen.DesktopTop;
    SetScreenWidth(Screen.DesktopWidth);
    SetScreenHeight(Screen.DesktopHeight);
    end
  else begin
    FScreenLeft := Screen.Monitors[FCurrentMonitor - 1].Left;
    FScreenTop  := Screen.Monitors[FCurrentMonitor - 1].Top;
    SetScreenWidth(Screen.Monitors[FCurrentMonitor - 1].Left +
                   Screen.Monitors[FCurrentMonitor - 1].Width);
    SetScreenHeight(Screen.Monitors[FCurrentMonitor - 1].Top +
                    Screen.Monitors[FCurrentMonitor - 1].Height);
    end;
end;

//---------------------------------------------------

function TScreenCamera.GetMonitorsCount: Integer;
begin
  Result := Screen.MonitorCount;
end;

//---------------------------------------------------

function TScreenCamera.GetCurrentMonitor: Integer;
begin
  Result := FCurrentMonitor;
end;

procedure TScreenCamera.SetCurrentMonitor(Value: Integer);
begin
  if (Value <> FCurrentMonitor) and
     (Value <= GetMonitorsCount) and
     (not (Value < 1)) then
    FCurrentMonitor := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetVersion: String;
begin
  Result := isVersion;
end;

procedure TScreenCamera.SetVersion(Value: String);
begin
  Value := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetBrightness: Integer;
begin
  Result := FBrightness;
end;

procedure TScreenCamera.SetBrightness(Value: Integer);
begin
  if FBrightness <> Value then
    FBrightness := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetContrast: Integer;
begin
  Result := FContrast;
end;

procedure TScreenCamera.SetContrast(Value: Integer);
begin
  if FContrast <> Value then
    FContrast := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetNColors: TPixelFormat;
begin
  Result := nColors;
end;

procedure TScreenCamera.SetNColors(Value: TPixelFormat);
begin
  nColors := Value;
  // Set color for record
  case nColors of
    pf8bit  : Bits := 8;
    pf15bit,
    pf16bit :
      begin
        nColors := pf16bit;
        Bits    := 16;
      end;
    pf24bit : Bits := 24;
    pf32bit : Bits := 32;
    else
      begin
        nColors := pf32bit;
        Bits    := 32;
      end;
  end;
end;

//---------------------------------------------------

function TScreenCamera.GetPriority: TThreadPriority;
begin
  Result := FPriority;
end;

procedure TScreenCamera.SetPriority(Value: TThreadPriority);
begin
  if FPriority <> Value then begin
    FPriority := Value;
    if Assigned(FRecordAVIThread) then
      FRecordAVIThread.Priority := FPriority;
    end;
end;

//---------------------------------------------------

function TScreenCamera.GetInterval: Integer;
begin
  Result := FPlaybackFPS;
end;

procedure TScreenCamera.SetInterval(Value: Integer);
begin
  if (FPlaybackFPS <> Value) and
     (Value <= 1000) and
     (Value > 0) then
    FPlaybackFPS := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetUpdateRate: Integer;
begin
  Result := FUpdateRate;
end;

procedure TScreenCamera.SetUpdateRate(Value: Integer);
begin
  if (FUpdateRate <> Value) and
     (Value <= 500) and
     (Value > 0) then
    FUpdateRate := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetQuality: Integer;
begin
  Result := FCompressionQuality;
end;

procedure TScreenCamera.SetQuality(Value: Integer);
begin
  if (FCompressionQuality <> Value) and
     (Value <= 10000) and
     (Value > 0) then
    FCompressionQuality := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetRecord_MSPF: Integer;
begin
  Result := FMSPFRecord;
end;

procedure TScreenCamera.SetRecord_MSPF(Value: Integer);
begin
  if (FMSPFRecord <> Value) and
     ((Value <= 1000) and
     (Value > 0)) then
    FMSPFRecord := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetFilterCopy: Integer;
begin
  Result := FFilterCopy;
end;

procedure TScreenCamera.SetFilterCopy(Value: Integer);
begin
  if (FFilterCopy <> Value) and
     ((Value <= 1) and
     (Value >= 0)) then
    FFilterCopy := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetScreenWidth: Integer;
begin
  Result := FScreenWidth;
end;

procedure TScreenCamera.SetScreenWidth(Value: Integer);
begin
  if (FScreenWidth <> Value) then
    FScreenWidth := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetScreenHeight: Integer;
begin
  Result := FScreenHeight;
end;

procedure TScreenCamera.SetScreenHeight(Value: Integer);
begin
  if (FScreenHeight <> Value) then
    FScreenHeight := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetLineRectClear: Boolean;
begin
  Result := FLineRectClear;
end;

procedure TScreenCamera.SetLineRectClear(Value: Boolean);
begin
  if (FLineRectClear <> Value) then
    FLineRectClear := Value;
end;

//---------------------------------------------------

function TScreenCamera.GetShowPreview: Boolean;
begin
  Result := not FShowPreview;
end;

procedure TScreenCamera.SetShowPreview(Value: Boolean);
begin
  OldUpdateTime1 := 0;
  OldTime2       := 0;
  InitialTime1   := TimeGetTime; {GetTickCount;};
  if FShowPreview <> Value then
    FShowPreview := Value;
  if FShowPreview then begin
    if not Assigned(FPreviewTimer) then begin
      FPreviewTimer := THighTimer.Create(Self);
      FPreviewTimer.OnTimer  := FShowPreviewTimer;
      FPreviewTimer.Interval := 1000 div FPlaybackFPS;
      FPreviewTimer.ThreadPriority := FPriority;
      FPreviewTimer.Synchronize := True;
      FPreviewTimer.UseThread := True;
      end;
    FPreviewTimer.Enabled := False;
    if Assigned(FFrame) then
      ShowWindow(FFrame.Handle, SW_HIDE);
    FPreviewTimer.Enabled := True;
    end
  else begin
    if not RecordState then begin
      if Assigned(FPreviewTimer) then
        FPreviewTimer.Enabled := False;
      if Assigned(FFrame) then
        ShowWindow(FFrame.Handle, SW_HIDE);
      if Assigned(FOnPreview) then
        FOnPreview(Self, nil, False, False);
      end;
    end;
end;

//---------------------------------------------------

constructor TScreenCamera.Create(AOwner: TComponent);
var
  TempDir   : String;
  GetLength : Integer;
begin
  inherited Create(AOwner);
  SetLength(TempDir, MAX_PATH + 1);
  GetLength              := GetTempPath(MAX_PATH, pchar(TempDir));
  SetLength(TempDir, GetLength);
  if Copy(TempDir, Length(TempDir), 1) <> '\' then
    TempDir              := TempDir + '\';
  TempVideoFile          := TempDir + 'tmpVideoStream.avi';
  TempAudioFile          := TempDir + 'tmpAudioStream.wav';
  RecordState            := False;
	MaxXScreen             := Screen.DesktopWidth;
	MaxYScreen             := Screen.DesktopHeight;
	FourCC                 := mmioFOURCC('M', 'S', 'V', 'C');
  FVideoCodecList        := TStringList.Create;
  StrCodec               := 'MS Video Codec';
  FVideoCodecList.Add(StrCodec);
  FSelCodecRepeat        := False;
  FAudioFormatList       := TStringList.Create;
  FAudioInfo.AudioInputNames := TStringList.Create;
  SavingSuccess          := False;
  StartRegionSel         := False;
  X1                     := 0;
  Y1                     := 0;
  X2                     := 0;
  Y2                     := 0;
  FTimerRecord.TimerON   := False;       // Timer recording (default = false)
  FTimerRecord.Hour      := 0;
  FTimerRecord.Min       := 0;
  FTimerRecord.Sec       := 0;
  FUpdateRate            := 250;         // Info update refresh rate (1ms ~ 1000ms)
  FMSPFRecord            := 5;           // FPS Record rate (1000 = 1 Second)(1 = 1 Millisecond)
  FPlaybackFPS           := 200;         // FPS Playback rate
  FKeyFramesEvery        := 20;          // Default every 20 frames keyframe
  FCompressionQuality    := 10000;       // Quality picture (range 1 ~ 10000)
  FVideoCompressorCount  := 0;           // Number of current video compressors
  FSelectedCompressor    := -1;          // No selected any codec
  FPriority              := tpNormal;    // Video Priority (tpNormal = Normal)
	SetNColors(pf32bit);                   // The number of image colors
  FScreenLeft            := 0;
  FScreenTop             := 0;
  FScreenWidth           := 300;
  FScreenHeight          := 300;
  FScreenRegion          := FixedMoving; // FixedMoving = Move window by mouse
  FRecordCursor          := True;
  FFlashingRect          := True;
  FLineRectClear         := True;
  FMinimize              := True;
  FRestore               := True;
  FAudioRecord           := False;
  FSelectObject          := False;
  FAutoPan               := False;
  FFreeHandMode          := False;
  FFullScreen            := False;
  FOnRecordPreview       := False;
  FAudioFormatsIndex     := -1;          // none selected audio format
  FSuccess               := None;
  FAutoPan               := False;
  FRecordAVIThread       := nil;
  FFilterCopy            := 0;           // 0 = Normal filter captured image
  FOvelayDrawing         := False;
  FEffectToOvelayDraw    := False;       // Allow to company overlay draw to effective
  FGrayScale             := False;       // Grayscale drawing
  FScreenRotate          := R90D;        // ( R90D, R180D, R270D, Mirror0D, Mirror180D )
  FUseRotateImage        := False;
  FNoise                 := 0;           // 0 ~ 255  0 = Normal
  FUseNoise              := False;
  FSaturation            := 0;           // 0 ~ 255  0 = Normal
  FUseSaturation         := False;
  FRedValue              := 0;           // -255 ~ 255  0 = Normal
  FGreenValue            := 0;           // -255 ~ 255  0 = Normal
  FBlueValue             := 0;           // -255 ~ 255  0 = Normal
  UseColorAdjusting      := False;
  FBrightness            := 0;           // -255 ~ 255  0 = Normal
  FUseBrightness         := False;
  FContrast              := 0;           // -255 ~ 255 0 = Normal
  FUseContrast           := False;
  FCurrentMonitor        := 1;           // Current monitor ( 1 ~ GetMonitorsCount() )
  FRecordAllMonitors     := False;
  GetAudioFormatsInfo;                   // Initialize audio formats
  GetAudioInputsInfo;                    // Initialize audio inputs
  GetVideoCompressorsInfo;               // Initialize video Compressors
  Application.OnMinimize := FFrameMinimize;
  // All application hot keys
  if not (csDesigning in ComponentState) then begin
    FWndHandle             := AllocateHWnd(GlobalHotKey);
    FHotKey1               := GlobalAddAtom('SHIFT + ESCAPE');
    RegisterHotKey(FWndHandle, FHotKey1, MOD_SHIFT, VK_ESCAPE);
    FHotKey2               := GlobalAddAtom('ESCAPE');
    RegisterHotKey(FWndHandle, FHotKey2, 0, VK_ESCAPE);
    FHotKey3               := GlobalAddAtom('CONTROL + RETURN');
    RegisterHotKey(FWndHandle, FHotKey3, MOD_CONTROL, VK_RETURN);
    end;
  SC := Self; //for callback
end;

destructor TScreenCamera.Destroy;
begin
  if RecordState then begin
    RecordState := False;
    FSuccess    := Fail;
    end;
  if Assigned(SC) then
    SC := nil;
  if Assigned(FPreviewTimer) then begin
    FPreviewTimer.Enabled := False;
    FPreviewTimer.Free;
    end;
  if Assigned(FFrame) then
    FFrame.Free;
  FAudioInfo.AudioInputNames.Free;
  FVideoCodecList.Free;
  FAudioFormatList.Free;
  // If already exist temp files try to delete them
  PowerDeleteFile(TempVideoFile);
  PowerDeleteFile(TempAudioFile);
  if not (csDesigning in ComponentState) then begin
    UnRegisterHotKey(FWndHandle, FHotKey1);
    UnRegisterHotKey(FWndHandle, FHotKey2);
    UnRegisterHotKey(FWndHandle, FHotKey3);
    end;
  if FWndHandle <> 0 then
    DeAllocateHWnd(FWndHandle);
  FWndHandle := 0;
  inherited Destroy;
end;

procedure TScreenCamera.ShowAbout;
begin
  with TfrmAbout.Create(Self) do
    try
      ShowAbout(isComponentName, 'v' + isVersion + '    ' + isHistory);
    finally
      Free;
    end;
end;

function TScreenCamera.FFreeHandFrameDraw: Boolean;
begin
  FreeHandWindow := TFreeHandWindow.Create(Application);
  try
    SetWindowPos(FreeHandWindow.Handle,
                 HWND_TOPMOST,
                 0,
                 0,
                 0,
                 0,
                 SWP_NOREPOSITION or
                 SWP_NOMOVE or
                 SWP_NOSIZE or
                 SWP_NOACTIVATE);
    if FreeHandWindow.ShowModal = mrOK then begin
      FScreenLeft := FreeHandWindow.PRegion.Left;
      FScreenTop  := FreeHandWindow.PRegion.Top;
      SetScreenWidth(FreeHandWindow.PRegion.Right);
      SetScreenHeight(FreeHandWindow.PRegion.Bottom);
      if FRecordAllMonitors then begin
        if FScreenLeft < Screen.DesktopLeft then begin
          SetScreenWidth(FScreenWidth - (Screen.DesktopLeft - FScreenLeft));
          FScreenLeft := Screen.DesktopLeft;
          end;
        if FScreenTop < Screen.DesktopTop then begin
          SetScreenHeight(FScreenHeight - (Screen.DesktopTop - FScreenTop));
          FScreenTop := Screen.DesktopTop;
          end;
        if (FScreenLeft + FScreenWidth) > (Screen.DesktopLeft + Screen.DesktopWidth) then
          SetScreenWidth(FScreenWidth - ((FScreenLeft + FScreenWidth) -
                                         (Screen.DesktopLeft + Screen.DesktopWidth)));
        if (FScreenTop + FScreenHeight) > (Screen.DesktopTop + Screen.DesktopHeight) then
          SetScreenHeight(FScreenHeight - ((FScreenTop + FScreenHeight) -
                                           (Screen.DesktopTop + Screen.DesktopHeight)));
        end
      else begin
        if FScreenLeft < Screen.Monitors[FCurrentMonitor - 1].Left then begin
          SetScreenWidth(FScreenWidth - (Screen.Monitors[FCurrentMonitor - 1].Left - FScreenLeft));
          FScreenLeft := Screen.Monitors[FCurrentMonitor - 1].Left;
          end;
        if FScreenTop < Screen.Monitors[FCurrentMonitor - 1].Top then begin
          SetScreenHeight(FScreenHeight - (Screen.Monitors[FCurrentMonitor - 1].Top - FScreenTop));
          FScreenTop := Screen.Monitors[FCurrentMonitor - 1].Top;
          end;
        if (FScreenLeft + FScreenWidth) > (Screen.Monitors[FCurrentMonitor - 1].Left +
                                           Screen.Monitors[FCurrentMonitor - 1].Width) then
          SetScreenWidth(FScreenWidth - ((FScreenLeft + FScreenWidth) -
                                         (Screen.Monitors[FCurrentMonitor - 1].Left +
                                          Screen.Monitors[FCurrentMonitor - 1].Width)));
        if (FScreenTop + FScreenHeight) > (Screen.Monitors[FCurrentMonitor - 1].Top +
                                           Screen.Monitors[FCurrentMonitor - 1].Height) then
          SetScreenHeight(FScreenHeight - ((FScreenTop + FScreenHeight) -
                                           (Screen.Monitors[FCurrentMonitor - 1].Top +
                                            Screen.Monitors[FCurrentMonitor - 1].Height)));
        end;
      FScreenRegion := FixedStable;
      if StartRegionSel then begin
        StartRegionSel := False;
        if FFileName <> '' then
          StartRecording(FFileName);
        end;
      Result := True;
      end
    else
      Result := False;
  finally;
    FreeHandWindow.Free;
    end;
end;

function TScreenCamera.FSelObjectFrameDraw: Boolean;
begin
  SelObjWindow := TSelObjWindow.Create(Application);
  try
    if SelObjWindow.ShowModal = mrOK then begin
      FScreenLeft := SelObjWindow.PRegion.Left;
      FScreenTop  := SelObjWindow.PRegion.Top;
      SetScreenWidth(SelObjWindow.PRegion.Right);
      SetScreenHeight(SelObjWindow.PRegion.Bottom);
      if FRecordAllMonitors then begin
        if FScreenLeft < Screen.DesktopLeft then begin
          SetScreenWidth(FScreenWidth - (Screen.DesktopLeft - FScreenLeft));
          FScreenLeft := Screen.DesktopLeft;
          end;
        if FScreenTop < Screen.DesktopTop then begin
          SetScreenHeight(FScreenHeight - (Screen.DesktopTop - FScreenTop));
          FScreenTop := Screen.DesktopTop;
          end;
        if (FScreenLeft + FScreenWidth) > (Screen.DesktopLeft + Screen.DesktopWidth) then
          SetScreenWidth(FScreenWidth - ((FScreenLeft + FScreenWidth) -
                                         (Screen.DesktopLeft + Screen.DesktopWidth)));
        if (FScreenTop + FScreenHeight) > (Screen.DesktopTop + Screen.DesktopHeight) then
          SetScreenHeight(FScreenHeight - ((FScreenTop + FScreenHeight) -
                                           (Screen.DesktopTop + Screen.DesktopHeight)));
        end
      else begin
        if FScreenLeft < Screen.Monitors[FCurrentMonitor - 1].Left then begin
          SetScreenWidth(FScreenWidth - (Screen.Monitors[FCurrentMonitor - 1].Left - FScreenLeft));
          FScreenLeft := Screen.Monitors[FCurrentMonitor - 1].Left;
          end;
        if FScreenTop < Screen.Monitors[FCurrentMonitor - 1].Top then begin
          SetScreenHeight(FScreenHeight - (Screen.Monitors[FCurrentMonitor - 1].Top - FScreenTop));
          FScreenTop := Screen.Monitors[FCurrentMonitor - 1].Top;
          end;
        if (FScreenLeft + FScreenWidth) > (Screen.Monitors[FCurrentMonitor - 1].Left +
                                           Screen.Monitors[FCurrentMonitor - 1].Width) then
          SetScreenWidth(FScreenWidth - ((FScreenLeft + FScreenWidth) -
                                         (Screen.Monitors[FCurrentMonitor - 1].Left +
                                          Screen.Monitors[FCurrentMonitor - 1].Width)));
        if (FScreenTop + FScreenHeight) > (Screen.Monitors[FCurrentMonitor - 1].Top +
                                           Screen.Monitors[FCurrentMonitor - 1].Height) then
          SetScreenHeight(FScreenHeight - ((FScreenTop + FScreenHeight) -
                                           (Screen.Monitors[FCurrentMonitor - 1].Top +
                                            Screen.Monitors[FCurrentMonitor - 1].Height)));
        end;
      FScreenRegion := FixedStable;
      if StartRegionSel then begin
        StartRegionSel := False;
        if FFileName <> '' then
          StartRecording(FFileName);
        end;
      Result := True;
      end
    else
      Result := False;
  finally;
    SelObjWindow.Free;
    end;
end;

procedure TScreenCamera.FShowPreviewTimer(Sender: TObject);
var
  mImage     : TBitmap;
  RemindTime : THMSM;
  CurentTime : Extended;
begin
  TimeExpended1 := TimeGetTime {GetTickCount} - InitialTime1;
  OldTime1      := TimeGetTime; {GetTickCount;}

  if not RecordState then begin
    case FScreenRegion of
      SelObject  : begin
                     FSelectObject := True;
                     FFreeHandMode := False;
                     FAutoPan      := False;
                     FFullScreen   := False;
                   end;
      FreeHand   : begin
                     FSelectObject := False;
                     FFreeHandMode := True;
                     FAutoPan      := False;
                     FFullScreen   := False;
                   end;
      FixedMoving: begin
                     FSelectObject := False;
                     FFreeHandMode := False;
                     FAutoPan      := True;
                     FFullScreen   := False;
                   end;
      FixedStable: begin
                     FSelectObject := False;
                     FFreeHandMode := False;
                     FAutoPan      := False;
                     FFullScreen   := False;
                   end;
      FullScreen : begin
                     FSelectObject := False;
                     FFreeHandMode := False;
                     FAutoPan      := False;
                     FFullScreen   := True;
                   end;
      end;
    end
  else begin
    if (FScreenRegion = FreeHand) or
       (FScreenRegion = SelObject) then begin
      FScreenRegion := FixedStable;
      FSelectObject := False;
      FFreeHandMode := False;
      FAutoPan      := False;
      FFullScreen   := False;
      end;
    end;

  if StartRegionSel or FShowPreview or (RecordState and not FFullScreen) then begin
    if FFullScreen then begin
      SetSizeFullScreen;
      end
    else begin
      if FFreeHandMode then begin
        FPreviewTimer.Enabled := False;
        FPreviewTimer.Enabled := FFreeHandFrameDraw;
        if not FPreviewTimer.Enabled then begin
          Exit;
          end;
        end
      else begin
        if FSelectObject then begin
          FPreviewTimer.Enabled := False;
          FPreviewTimer.Enabled := FSelObjectFrameDraw;
          if not FPreviewTimer.Enabled then begin
            Exit;
            end;
          end
        else begin
          if FAutoPan then begin
            FScreenLeft := FCursorPos.CursorPos.X - (FScreenWidth div 2);
            FScreenTop  := FCursorPos.CursorPos.Y - (FScreenHeight div 2);
            if FRecordAllMonitors then begin
              if FScreenLeft < Screen.DesktopLeft then
                FScreenLeft := Screen.DesktopLeft;
              if FScreenTop < Screen.DesktopTop then
                FScreenTop  := Screen.DesktopTop;
              if (FScreenLeft + FScreenWidth) > Screen.DesktopWidth then
                FScreenLeft := (Screen.DesktopWidth - FScreenWidth);
              if (FScreenTop + FScreenHeight) > Screen.DesktopHeight then
                FScreenTop  := (Screen.DesktopHeight - FScreenHeight);
              end
            else begin
              if FScreenLeft < Screen.Monitors[FCurrentMonitor - 1].Left then
                FScreenLeft := Screen.Monitors[FCurrentMonitor - 1].Left;
              if FScreenTop < Screen.Monitors[FCurrentMonitor - 1].Top then
                FScreenTop  := Screen.Monitors[FCurrentMonitor - 1].Top;
              if (FScreenLeft + FScreenWidth) > (Screen.Monitors[FCurrentMonitor - 1].Left +
                                                 Screen.Monitors[FCurrentMonitor - 1].Width) then
                FScreenLeft := ((Screen.Monitors[FCurrentMonitor - 1].Left +
                                 Screen.Monitors[FCurrentMonitor - 1].Width) - FScreenWidth);
              if (FScreenTop + FScreenHeight) > (Screen.Monitors[FCurrentMonitor - 1].Top +
                                                 Screen.Monitors[FCurrentMonitor - 1].Height) then
                FScreenTop  := ((Screen.Monitors[FCurrentMonitor - 1].Top +
                                 Screen.Monitors[FCurrentMonitor - 1].Height) - FScreenHeight);
              end;
            end;
          end;
        end;
      end;
    if (FFlashingRect) then begin
      if not Assigned(FFrame) then
        FFrame := TFlashingWnd.Create(Self);

      //Update color around region
      if (TimeExpended1 >= OldTime2 + FUpdateRate) then begin
        OldTime2 := TimeExpended1;
        case FRegColor of
          clRed: FRegColor := clLime;
          clLime: FRegColor := clRed;
        end;  
        end;

      if (not RecordState) and FShowPreview then begin
        FFrame.SetUpRegion(FScreenLeft,
                           FScreenTop,
                           FScreenWidth,
                           FScreenHeight,
                           FLineRectClear,
                           PreviewMsg);
        FFrame.PaintBorder(FRegColor,
                           PreviewMsg);
        end
      else begin
        if (RecordState) then begin
          FFrame.SetUpRegion(FScreenLeft,
                             FScreenTop,
                             FScreenWidth,
                             FScreenHeight,
                             FLineRectClear,
                             RecordingMsg);
          FFrame.PaintBorder(FRegColor,
                             RecordingMsg);
          end;
        end;
      ShowWindow(FFrame.Handle, SW_SHOW);
      SetWindowPos(FFrame.Handle,
                   HWND_TOPMOST,
                   0,
                   0,
                   0,
                   0,
                   SWP_NOREPOSITION or
                   SWP_NOMOVE or
                   SWP_NOSIZE or
                   SWP_NOACTIVATE);
      end
    else begin
      if Assigned(FFrame) then
        ShowWindow(FFrame.Handle, SW_HIDE);
      end;
    if Assigned(FOnPreview) then begin
      if FShowPreview and not RecordState then begin
        mImage := TBitmap.Create;
        try
          CaptureScreenFrame(0,
                             mImage,
                             FScreenLeft,
                             FScreenTop,
                             FScreenWidth,
                             FScreenHeight,
                             FFilterCopy);
          if mImage.Canvas.TryLock then
            try
              FOnPreview(Self, mImage, True, False);
            finally
              mImage.Canvas.UnLock;
            end;
        finally
          mImage.Free;
        end;
        end
      else begin
        if (FShowPreview) and (RecordState)
           //Notice: some of sizes on record mode for preview equal memory crashing.
           and not (FScreenWidth  > 640)  // Width of region
           and not (FScreenHeight > 480)  // Height of region
           then
          FOnRecordPreview := True
        else begin
          FOnRecordPreview := False;
          if RecordState then
            FOnPreview(Self, nil, False, True)
          else
            FOnPreview(Self, nil, False, False);
          end;
        end;
      end;
    end
  else begin
    if Assigned(FFrame) then
      ShowWindow(FFrame.Handle, SW_HIDE);
    FOnRecordPreview := False;
    if Assigned(FOnPreview) then begin
      if RecordState then
        FOnPreview(Self, nil, False, True)
      else
        FOnPreview(Self, nil, False, False);
      end;
    end;
  if FShowPreview and not RecordState then begin
    RemindTime := MilliSecond2Time(TimeExpended1);
    Hur := RemindTime.Hour;
    Min := RemindTime.Minute;
    Sec := RemindTime.Second;
    Mil := RemindTime.MilliSecond;
    FElapsedTime := IntToStr(Hur) + ' : ' +
                    IntToStr(Min) + ' : ' +
                    IntToStr(Sec) + ' : ' +
                    IntToStr(Mil);
    StrCodec            := '';
    FActualFrameNo      := 0;
    FSkippedFrames      := 0;
    FCurrentCapturedFPS := 0;
    // Real capturing frames at one second...
    CurentTime := TimeGetTime {GetTickCount};
    if (CurentTime - OldTime1) > 0 then
      FRealCapturingFPS := 1000 / (CurentTime - OldTime1)
    else
      FRealCapturingFPS := 1000;
    //Update record stats
    if (TimeExpended1 >= OldUpdateTime1 + FUpdateRate) then begin
      OldUpdateTime1 := TimeExpended1;
      if Assigned(FOnUpdate) then FOnUpdate(Self); // user event for current status
      end;
    OldTime1 := CurentTime;
    end;
end;

procedure TScreenCamera.FFrameMinimize(Sender: TObject);
begin
  if Assigned(FFrame) then
    if FFrame.Showing then
      SendMessage(FFrame.Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
end;

procedure TScreenCamera.GlobalHotKey(var Msg: TMessage);
begin
  case Msg.Msg of
    WM_HOTKEY:
      begin
        if Msg.WParam = FHotKey1 then
          FSuccess := Success;

        if Msg.WParam = FHotKey2 then begin
          FSuccess := Fail;

          if Assigned(FreeHandWindow) then
            if FreeHandWindow.Showing then
              FreeHandWindow.ModalResult := mrCancel;

          if Assigned(SelObjWindow) then
            if SelObjWindow.Showing then begin
              SelObjWindow.Timer1.Enabled := False;
              SelObjWindow.ModalResult := mrCancel;
              end;

          StartRegionSel := False;

          if FShowPreview or RecordState then
            if FRestore then
              Application.Restore;

          if FShowPreview and not RecordState then
            SetShowPreview(False);
          end;

        if Msg.WParam = FHotKey3 then begin
          if Assigned(SelObjWindow) then
            if SelObjWindow.Showing then begin
              SelObjWindow.Timer1.Enabled := False;
              if (FScreenWidth <> 0) and (FScreenHeight <> 0) then
                SelObjWindow.ModalResult := mrOk
              else
                FreeHandWindow.ModalResult := mrCancel;
              end;
          end;

        if RecordState then begin
          RecordState := False;
          if not FShowPreview then
            SetShowPreview(False)
          else
            SetShowPreview(True);
          end;
      end;
  end;
end;

procedure TScreenCamera.CompressorAbout(Compressor: Byte; WND: HWND);
var
  icv: hic;
begin
  if Compressor >= FVideoCompressorCount then Exit;
	icv := ICOpen(FVideoCompressorInfo[Compressor].fccType,
                FVideoCompressorInfo[compressor].fccHandler,
                ICMODE_QUERY);
	if (icv <> 0) then
  begin
  	ICAbout(icv, WND);
    ICClose(icv);
  end;
end;

procedure TScreenCamera.CompressorConfigure(Compressor: Byte; WND: HWND);
var
  icv: hic;
begin
  if Compressor >= FVideoCompressorCount then Exit;
	icv := ICOpen(FVideoCompressorInfo[Compressor].fccType,
                FVideoCompressorInfo[Compressor].fccHandler,
                ICMODE_QUERY);
	if (icv <> 0) then
  begin
  	ICConfigure(icv, WND);
    ICClose(icv);
  end;
end;


procedure TScreenCamera.CompressorHasFeatures(Compressor: Byte;
  var hasAbout: Boolean; var hasConfig: Boolean);
var
  icv: hic;
begin
  hasAbout  := False;
  hasConfig := False;
  if Compressor >= FVideoCompressorCount then Exit;
	icv := ICOpen(FVideoCompressorInfo[Compressor].fccType,
                FVideoCompressorInfo[Compressor].fccHandler,
                ICMODE_QUERY);
	if (icv <> 0) then
  begin
  	hasAbout  := ICQueryAbout(icv);
  	hasConfig := ICQueryConfigure(icv);
    ICClose(icv);
  end;
end;

function TScreenCamera.HBitmap2DDB(HBitmap: HBitmap; nBits: LongWord): THandle;
var
	hDIB     : THandle;
  aHDC     : HDC;
	hSDC     : HDC;
  hDesktop : HWND;
	Bitmap   : Windows.TBitmap;
	wLineLen : LongWord;
	dwSize   : DWORD;
	wColSize : DWORD;
	lpbi     : PBitmapInfoHeader;
	lpBits   : PByte;
  Msg      : TMessage;
  InfoSize : Integer;
  MsgStr   : String;

  procedure SetBitmapInfoHeader(var nlpbi: PBitmapInfoHeader);
  begin
    nlpbi^.biSize          := SizeOf(BitmapInfoHeader);
    nlpbi^.biWidth         := Bitmap.bmWidth;
	  nlpbi^.biHeight        := Bitmap.bmHeight;
	  nlpbi^.biPlanes        := 1;
    nlpbi^.biBitCount      := nBits;
	  nlpbi^.biCompression   := BI_RGB;
  	nlpbi^.biSizeImage     := dwSize - SizeOf(BitmapInfoHeader) - wColSize;
	  nlpbi^.biXPelsPerMeter := 0;
  	nlpbi^.biYPelsPerMeter := 0;
	  nlpbi^.biClrImportant  := 0;
    if nBits <= 8 then
      nlpbi^.biClrUsed     := (1 shl nBits)
    else
      nlpbi^.biClrUsed     := 0;
  end;

  label Error;

begin
  CancelRecording := False;
	GetObject(HBitmap, SizeOf(Bitmap), @Bitmap);

	// DWORD align the width of the DIB
	// Figure out the size of the colour table
	// Calculate the size of the DIB
	wLineLen := (Bitmap.bmWidth * nBits + 31) div 32 * 4;
  if (nBits <= 8) then
    wColSize := SizeOf(RGBQUAD) * (1 shl nBits)
  else
    wColSize := 0;
	dwSize := SizeOf(BitmapInfoHeader) + wColSize + (wLineLen * Bitmap.bmHeight);

	// Allocate room for a DIB and set the LPBI fields
	hDIB := GlobalAlloc(GHND, dwSize); //allocate bitmap handle
	if (hDIB = 0) then begin
		Result := hDIB;
    CancelRecording := True;
    MsgStr := ErrorMsg1;
    goto Error;
    end;

  // Lock bitmap handle and get back pointer
	lpbi := GlobalLock(hDIB);
	if not Assigned(lpbi) then begin
		Result := hDIB;
    CancelRecording := True;
    MsgStr := ErrorMsg2;
    goto Error;
    end;

  // Set bitmap info header
  SetBitmapInfoHeader(lpbi);

	// Get the bits from the bitmap and stuff them after the LPBI
  if nBits <= 8 then
    InfoSize := lpbi^.biSize + lpbi^.biClrUsed * SizeOf(RGBQUAD)
  else
    InfoSize := lpbi^.biSize;

	lpBits   := PByte(LongWord(lpbi) + InfoSize);
  hDesktop := GetDesktopWindow;
  hSDC     := GetWindowDC(hDesktop);
	aHDC     := CreateCompatibleDC(hSDC);

  { Retrieve the bits of hbitmap and copy them into
    the buffer lpBits using the specified format in lpbi }
  if GetDIBits(aHDC,
               HBitmap,
               0,
               Bitmap.bmHeight,
               lpBits,
               PBitmapInfo(lpbi)^,
               DIB_RGB_COLORS) = 0 then begin
    CancelRecording := True;
    MsgStr := ErrorMsg3;
    end;

  // Set again bitmap info header
  SetBitmapInfoHeader(lpbi);

  ReleaseDC(hDesktop, hSDC);
	DeleteDC(aHDC);
	GlobalUnlock(hDIB);
	Result := hDIB;

Error:
  // Error on retriving bitmap bits and cancel recording
  if CancelRecording then begin
    Msg.Msg    := WM_HOTKEY;
    Msg.WParam := FHotKey2;
    GlobalHotKey(Msg);
    if Assigned(FOnError) then FOnError(Self, MsgStr);
    FSuccess := Fail;
    RecordState := False;
    if Assigned(FOnStop) then FOnStop(Self);
    end;
end;


function TScreenCamera.CaptureScreenFrame(Mode: Integer; TempImage: TBitmap;
  Left, Top, Width, Height: Integer; CopyMode: Integer): PBitmapInfoHeader;
var
  hbm        : HBitmap;
  hMemDC     : HBitmap;
	hScreenDC  : HDC;
  hDesktop   : HWND;
  IconInf    : TIconInfo;
  CursorInf  : TCursorInfo;
  Icon       : TIcon;
  pBM_HEADER : PBitmapInfoHeader;
  CopySrc    : Integer;
  TmBitmap,
  tmpBitmap  : TBitmap;
  Effects    : TEffects;
begin
  case CopyMode of
    0  : CopySrc := SRCCOPY;
    1  : CopySrc := NOTSRCCOPY;
    else CopySrc := SRCCOPY;
  end;
  hDesktop  := GetDesktopWindow;
  hScreenDC := GetWindowDC(hDesktop);
  hMemDC    := CreateCompatibleDC(hScreenDC);
  hbm       := CreateCompatibleBitmap(hScreenDC, Width, Height);
  SelectObject(hMemDC, hbm);
  // A bit-block transfer of hScreenDC to hMemDC
  BitBlt(hMemDC,
         0,
         0,
         Width,
         Height,
         hScreenDC,
         Left,
         Top,
         CopySrc);
	//Draw the cursor
  if (FRecordCursor) then begin
    Icon := TIcon.Create;
    try
      CursorInf.cbSize := SizeOf(TCursorInfo);
      if GetCursorInfo(CursorInf) then
        if CursorInf.Flags = CURSOR_SHOWING then begin
          Icon.Handle := CursorInf.hCursor;
          if GetIconInfo(Icon.Handle, IconInf) then
            try
              DrawIcon(hMemDC,
                       CursorInf.ptScreenPos.X - (IconInf.xHotspot + Left),
                       CursorInf.ptScreenPos.Y - (IconInf.yHotspot + Top),
                       Icon.Handle);
            finally
              DeleteObject(IconInf.hbmMask);
              DeleteObject(IconInf.hbmColor);
              end;
          end;
    finally
      Icon.Free;
      end;
    end;
  if FEffectToOvelayDraw then
    if Assigned(FOnOverlay) and FOvelayDrawing then begin
      BitBlt(hMemDC,
             0,
             0,
             Width,
             Height,
             hMemDC,
             0,
             0,
             CopySrc);
      FOnOverlay(Self, hMemDC, Width, Height);
      BitBlt(hMemDC,
             0,
             0,
             Width,
             Height,
             hMemDC,
             0,
             0,
             CopySrc);
      end;
  Effects := TEffects.Create(Self);
  try
    tmpBitmap := TBitmap.Create;
    try
      tmpBitmap.Width  := Width;
      tmpBitmap.Height := Height;
      if tmpBitmap.Canvas.TryLock then begin
        try
          BitBlt(tmpBitmap.Canvas.Handle,
                 0,
                 0,
                 Width,
                 Height,
                 hMemDC,
                 0,
                 0,
                 SRCCOPY);
          tmpBitmap.PixelFormat := nColors;
          Effects.PixelSize     := nColors;
          if FUseNoise then
            //Noise Effect
            Effects.Noise(tmpBitmap, PositiveSel, FNoise);
          if FUseSaturation then
            //Saturation Effect
            Effects.Saturation(tmpBitmap, PositiveSel, High(Byte) - FSaturation);
          if FUseColorAdjusting then
            //Color Adjusting Effect
            Effects.Increase(tmpBitmap, PositiveSel, NegativeSel,
              Increments(FRedValue, FGreenValue, FBlueValue));
          if FUseBrightness then
            //Brightness Effect
            Effects.Increase(tmpBitmap, PositiveSel, NegativeSel,
              Increments(FBrightness, FBrightness, FBrightness));
          if FUseContrast then
            //Contrast Effect
            Effects.Contrast(tmpBitmap, PositiveSel,
              Increments(FContrast, FContrast, FContrast));
          BitBlt(hMemDC,
                 0,
                 0,
                 Width,
                 Height,
                 tmpBitmap.Canvas.Handle,
                 0,
                 0,
                 SRCCOPY);
          if FGrayScale then begin
            ConvertToGrayScale(tmpBitmap);
            BitBlt(hMemDC,
                   0,
                   0,
                   Width,
                   Height,
                   tmpBitmap.Canvas.Handle,
                   0,
                   0,
                   SRCCOPY);
            end;
        if FUseRotateImage then begin
          TmBitmap := TBitmap.Create;
          try
            BitBlt(tmpBitmap.Canvas.Handle,
                   0,
                   0,
                   Width,
                   Height,
                   hMemDC,
                   0,
                   0,
                   SRCCOPY);
            TmBitmap.Width  := tmpBitmap.Width;
            TmBitmap.Height := tmpBitmap.Height;
            if TmBitmap.Canvas.TryLock then begin
              try
                TmBitmap.Assign(tmpBitmap);
                TmBitmap.PixelFormat := nColors;
                if FScreenRotate = R90D then
                  //Rotate Image 90 Degree
                  Effects.RotateCustom(TmBitmap, tmpBitmap, DegToRad(90));
                if FScreenRotate = R180D then
                  //Rotate Image 180 Degree
                  Effects.Rotate(TmBitmap, tmpBitmap);
                if FScreenRotate = R270D then
                  //Rotate Image 270 Degree
                  Effects.RotateCustom(TmBitmap, tmpBitmap, DegToRad(270));
                if FScreenRotate = Mirror0D then
                  //Rotate Image Mirror 0 Degree
                  Effects.Mirror(TmBitmap, tmpBitmap);
                if FScreenRotate = Mirror180D then
                  //Rotate Image Mirror 180 Degree
                  Effects.RotateAndMirror(TmBitmap, tmpBitmap);
                BitBlt(hMemDC,
                       0,
                       0,
                       Width,
                       Height,
                       tmpBitmap.Canvas.Handle,
                       0,
                       0,
                       SRCCOPY);
              finally
                TmBitmap.Canvas.Unlock;
              end;
              end;
          finally
            TmBitmap.Free;
          end;
          end;
        finally
          tmpBitmap.Canvas.Unlock;
        end;
        end;
    finally
      tmpBitmap.Free;
      end;
  finally
    Effects.Free;
  end;

  if not FEffectToOvelayDraw then
    if Assigned(FOnOverlay) and FOvelayDrawing then
      FOnOverlay(Self, hMemDC, Width, Height);

  case Mode of
    0: begin
         TempImage.Width  := Width;
         TempImage.Height := Height;
         if TempImage.Canvas.TryLock then
           try
             TempImage.PixelFormat := nColors;
             BitBlt(TempImage.Canvas.Handle,
                    0,
                    0,
                    Width,
                    Height,
                    hMemDC,
                    0,
                    0,
                    SRCCOPY);
           finally
             TempImage.Canvas.UnLock;
           end;
         pBM_HEADER := nil;
       end;
    1: begin
         TempImage := nil;
         // Lock bitmap handle and get pointer
	       pBM_HEADER := GlobalLock(HBitmap2DDB(hbm, Bits)); // DDB is fastest than DIB
       end;
    2: begin
         TempImage.Width  := Width;
         TempImage.Height := Height;
         if TempImage.Canvas.TryLock then
           try
             TempImage.PixelFormat := nColors;
             BitBlt(TempImage.Canvas.Handle,
                    0,
                    0,
                    Width,
                    Height,
                    hMemDC,
                    0,
                    0,
                    SRCCOPY);
           finally
             TempImage.Canvas.UnLock;
           end;
         // Lock bitmap handle and get pointer
	       pBM_HEADER := GlobalLock(HBitmap2DDB(hbm, Bits)); // DDB is fastest than DIB
       end;
  end;
  ReleaseDC(hDesktop, hScreenDC);
  DeleteDC(hMemDC);
	DeleteObject(hbm);
  Result := pBM_HEADER;
end;

function TScreenCamera.GetAudioInputsInfo: TGetAudioInfo;
var
  I,
  J,
  Index      : Integer;
  AudioMixer : TAudioMixer;
begin
  AudioMixer := TAudioMixer.Create(Self);
  try
    FAudioInfo.AudioInputNames.Clear;
    for I := 0 to AudioMixer.MixerCount - 1 do begin
      AudioMixer.MixerID             := I;
      if LowerCase(AudioMixer.DestinationName) = 'recording control' then
        Index                        := I;
      end;
    AudioMixer.MixerID               := Index;
    for I := 0 to AudioMixer.DestinationCount - 1 do begin
      AudioMixer.DestinationID       := I;
      if (mcSelect in AudioMixer.Master.AvailableControls) then begin
        FAudioInfo.AudioInputIndex   := AudioMixer.Master.SelectedLine;
        FAudioInfo.AudioInputVolume  := AudioMixer.Master.Mixer.Lines[FAudioInfo.AudioInputIndex].Volume;
        FAudioInfo.AudioInputEnabled := mcVolume in AudioMixer.Master.Mixer.Lines[FAudioInfo.AudioInputIndex].AvailableControls;
        for J := 0 to AudioMixer.Master.Mixer.LineCount - 1 do begin
          FAudioInfo.AudioInputNames.Add(AudioMixer.Master.Mixer.Lines[J].Name);
          end;
        end
      else
        if (mcVolume in AudioMixer.Master.AvailableControls) or
           (mcMute in AudioMixer.Master.AvailableControls) then begin
        FAudioInfo.AudioInputIndex   := I;
        FAudioInfo.AudioInputVolume  := AudioMixer.Master.Volume;
        FAudioInfo.AudioInputEnabled := mcVolume in AudioMixer.Master.AvailableControls;
        FAudioInfo.AudioInputNames.Add(AudioMixer.Master.Name);
        end;
      end;
  finally
    AudioMixer.Free;
  end;
  Result := FAudioInfo;
end;

procedure TScreenCamera.SetAudioInputIndex(Index: Integer);
var
  I,
  _Index     : Integer;
  AudioMixer : TAudioMixer;
begin
  AudioMixer := TAudioMixer.Create(Self);
  try
    for I := 0 to AudioMixer.MixerCount - 1 do begin
      AudioMixer.MixerID           := I;
      if LowerCase(AudioMixer.DestinationName) = 'recording control' then
        _Index                     := I;
      end;
    AudioMixer.MixerID             := _Index;
    for I := 0 to AudioMixer.DestinationCount - 1 do begin
      AudioMixer.DestinationID     := I;
      if (mcSelect in AudioMixer.Master.AvailableControls) then
        AudioMixer.Master.SelectedLine := Index
      end;
   finally
     AudioMixer.Free;
   end;
end;

procedure TScreenCamera.SetAudioInputVolume(Index, Volume: Integer);
var
  I,
  _Index     : Integer;
  AudioMixer : TAudioMixer;
begin
  AudioMixer := TAudioMixer.Create(Self);
  try
    for I := 0 to AudioMixer.MixerCount - 1 do begin
      AudioMixer.MixerID           := I;
      if LowerCase(AudioMixer.DestinationName) = 'recording control' then
        _Index                     := I;
      end;
    AudioMixer.MixerID             := _Index;
    for I := 0 to AudioMixer.DestinationCount - 1 do begin
      AudioMixer.DestinationID     := I;
      if (mcSelect in AudioMixer.Master.AvailableControls) then
        AudioMixer.Master.Mixer.Lines[Index].Volume := Volume
      else
        if (mcVolume in AudioMixer.Master.AvailableControls) or
           (mcMute in AudioMixer.Master.AvailableControls) then begin
          AudioMixer.Master.Volume := Volume;
          end;
      end;
  finally
    AudioMixer.Free;
  end;
end;

function TScreenCamera.GetAudioFormatsInfo: TStringList;
var
  pcm                : TPCMFormat;
  WaveFormatEx       : TWaveFormatEx;
  StockAudioRecorder : TStockAudioRecorder;
begin
  StockAudioRecorder := TStockAudioRecorder.Create(Self);
  try
    FAudioFormatList.Clear;
    for pcm := Succ(Low(TPCMFormat)) to High(TPCMFormat) do begin
      SetPCMAudioFormatS(@WaveFormatEx, pcm);
      FAudioFormatList.Add(GetWaveAudioFormat(@WaveFormatEx));
      end;
    if FAudioFormatsIndex = -1 then
      FAudioFormatsIndex := Ord(StockAudioRecorder.PCMFormat) - 1;
  finally
    StockAudioRecorder.Free;
  end;
  Result := FAudioFormatList;
end;

function TScreenCamera.GetVideoCompressorsInfo: TStringList;
var
  ICV    : HIC;
 	Source : PBitmapInfoHeader;
  I      : Integer;
  mImage : TBitmap;
begin
  mImage := TBitmap.Create;
  try
    Source := CaptureScreenFrame(1,
                                 mImage,
                                 FScreenLeft,
                                 FScreenTop,
                                 FScreenWidth,
                                 FScreenHeight,
                                 FFilterCopy);
    FVideoCompressorCount := 0;
    FVideoCodecList.Clear;
    for I := 0 to 50 do begin
     	ICInfo(ICTYPE_VIDEO,
             I,
             @FVideoCompressorInfo[FVideoCompressorCount]);
   		ICV := ICOpen(FVideoCompressorInfo[FVideoCompressorCount].fccType,
                    FVideoCompressorInfo[FVideoCompressorCount].fccHandler,
                    ICMODE_QUERY);
  		if (ICV <> 0) then begin
	  		if (ICCompressQuery(ICV, Source, nil) = ICERR_OK) then begin
		  		ICGetInfo(ICV,
                    @FVideoCompressorInfo[FVideoCompressorCount],
                    SizeOf(TICINFO));
      		Inc(FVideoCompressorCount);
          end;
		  	ICClose(ICV);
        end;
      end;
    for I := 0 to FVideoCompressorCount - 1 do
      FVideoCodecList.Add(FVideoCompressorInfo[I].szDescription);
  finally
    mImage.Free;
  end;
  // Free memory
	FreeFrame(Source);
  Result := FVideoCodecList;
end;

function TScreenCamera.RecordVideo(szFileName: String): Integer;
var
	alpbi          : PBitmapInfoHeader;
	strhdr         : TAVISTREAMINFO;
	pfile          : PAVIFile;
	ps,
  psCompressed   : PAVIStream;
  opts           : TAVICOMPRESSOPTIONS;
	Option         : PAVICOMPRESSOPTIONS;
	hr             : HRESULT;
  wVer           : WORD;
	szTitle        : String;
  ic             : HIC;
  fTime,
  pTime,
  FrameRate,
  InitialTime,
 	TimeExpended,
  OldFrameTime,
  OldTime,
  OldUpdateTime  : Extended;
  InfoImageSize,
  N,
  align,
  I,
  hm,
  wm,
  OldComputedFrameNo,
  newleft,
  newtop,
  newwidth,
  newheight      : Integer;
  CapturedFrame  : Boolean;
  mImage         : TBitmap;
  RemindTime     : THMSM;
  StockAudioRecorder : TStockAudioRecorder;

label Error;

begin
  mImage := TBitmap.Create;
	wVer := HIWORD(VideoForWindowsVersion);
	if (wVer < $010a) then begin
    if Assigned(FOnError) then FOnError(Self, ErrorMsg4);
    RecordState := False;
    FSuccess    := Fail;
    if Assigned(FOnStop) then FOnStop(Self);
    if not FShowPreview then begin
      if Assigned(FFrame) then
        ShowWindow(FFrame.Handle, SW_HIDE);
      if Assigned(FPreviewTimer) then begin
        FPreviewTimer.Enabled := False;
        end;
      end;
		Result := 0;
    if FRestore then
      Application.Restore;
    mImage.Free;
    Exit;
  	end;

	// CAPTURE FIRST FRAME -------------------------------------------
	alpbi := CaptureScreenFrame(1,
                              mImage,
                              FScreenLeft,
                              FScreenTop,
                              FScreenWidth,
                              FScreenHeight,
                              FFilterCopy);

  if alpbi = nil then begin
    mImage.Free;
    // Free memory
    FreeFrame(alpbi);
    if Assigned(FOnError) then FOnError(Self, ErrorMsg5);
    Result := 0;
    FSuccess := Fail;
    RecordState := False;
    if Assigned(FOnStop) then FOnStop(Self);
    if FRestore then
      Application.Restore;
    Exit;
    end;
  // ---------------------------------------------------------------

	// TEST VALIDITY OF COMPRESSOR
  if (FSelectedCompressor <> -1) and (not FSelCodecRepeat) then begin
		ic := ICOpen(FVideoCompressorInfo[FSelectedCompressor].fccType,
                 FVideoCompressorInfo[FSelectedCompressor].fccHandler,
                 ICMODE_QUERY);
		if (ic <> 0) then begin
      align := 1;
			while (ICCompressQuery(ic, alpbi, nil) <> ICERR_OK) do begin
				//Try adjusting width/height a little bit
				align := align * 2;
				if (align > 8) then Break;

				newleft := FScreenLeft;
				newtop  := FScreenTop;
				wm      := (FScreenWidth mod align);
				if (wm > 0) then begin
					newwidth := FScreenWidth + (align - wm);
          SetScreenWidth(newwidth);
					if (newwidth > MaxXScreen) then
						newwidth := FScreenWidth - wm;
				  end;

				hm := (FScreenHeight mod align);
				if (hm > 0) then begin
					newheight := FScreenHeight + (align - hm);
          SetScreenHeight(newheight);
					if (newheight > MaxYScreen) then
						newwidth := FScreenHeight - hm;
  				end;

        // Free memory
        FreeFrame(alpbi);
 				alpbi := CaptureScreenFrame(1,
                                    mImage,
                                    newleft,
                                    newtop,
                                    newwidth,
                                    newheight,
                                    FFilterCopy);

        if alpbi = nil then begin
          mImage.Free;
          // Free memory
          FreeFrame(alpbi);
          if Assigned(FOnError) then FOnError(Self, ErrorMsg6);
          Result := 0;
          FSuccess := Fail;
          RecordState := False;
          if Assigned(FOnStop) then FOnStop(Self);
          if FRestore then
            Application.Restore;
          Exit;
          end;
        end;

  		//if succeed with new width/height, use the new width and height
	  	//else if still fails ==> default to MS Video 1 (MSVC)
		  if (align = 1) then begin
			  //Compressor has no problem with the current dimensions...so proceed
        CompfccHandler := FVideoCompressorInfo[FSelectedCompressor].fccHandler;
        StrCodec       := FVideoCompressorInfo[FSelectedCompressor].szName;
	  		ICClose(ic);
		  	end
      else if (align <= 8) then begin
	  		//Compressor can work if the dimensions is adjusted slightly
		  	FScreenLeft    := newleft;
  			FScreenTop     := newtop;
	  		SetScreenWidth(newwidth);
		  	SetScreenHeight(newheight);
        CompfccHandler := FVideoCompressorInfo[FSelectedCompressor].fccHandler;
        StrCodec       := FVideoCompressorInfo[FSelectedCompressor].szName;
	  		ICClose(ic);
		    end
      else begin
      	if (MessageBox(Application.MainForm.Handle, ErrorMsg7, 'Notice',
                       MB_YESNO or MB_ICONEXCLAMATION) = IDYES) then begin
  		    CompfccHandler  := FourCC;
          StrCodec        := 'MS Video Codec';
   	  		ICClose(ic);
          FSelCodecRepeat := True;
          end
        else begin
          mImage.Free;
          // Free memory
          FreeFrame(alpbi);
          if Assigned(FOnError) then FOnError(Self, ErrorMsg8);
          RecordState := False;
          FSuccess    := Fail;
          if Assigned(FOnStop) then FOnStop(Self);
          if not FShowPreview then begin
            if Assigned(FOnPreview) then
              FOnPreview(Self, nil, False, False);
            if Assigned(FFrame) then
              ShowWindow(FFrame.Handle, SW_HIDE);
            if Assigned(FPreviewTimer) then begin
              FPreviewTimer.Enabled := False;
              end;
            end;
          ICClose(ic);
     	    Result := 0;
          if FRestore then
            Application.Restore;
          Exit;
          end;
  			end;
		  end
    else begin
      if (MessageBox(Application.MainForm.Handle, ErrorMsg9, 'Notice',
                     MB_YESNO or MB_ICONEXCLAMATION) = IDYES) then begin
        CompfccHandler  := FourCC;
        StrCodec        := 'MS Video Codec';
 	  		ICClose(ic);
        FSelCodecRepeat := True;
        end
      else begin
        mImage.Free;
        // Free memory
        FreeFrame(alpbi);
        if Assigned(FOnError) then FOnError(Self, ErrorMsg8);
        RecordState := False;
        FSuccess    := Fail;
        if Assigned(FOnStop) then FOnStop(Self);
        if not FShowPreview then begin
          if Assigned(FOnPreview) then
            FOnPreview(Self, nil, False, False);
          if Assigned(FPreviewTimer) then begin
            FPreviewTimer.Enabled := False;
            end;
          if Assigned(FFrame) then
            ShowWindow(FFrame.Handle, SW_HIDE);
          end;
	  		ICClose(ic);
 	      Result := 0;
        if FRestore then
          Application.Restore;
        Exit;
        end;
	  	end;
	  end
  else begin
    if not FSelCodecRepeat then begin
      if (MessageBox(Application.MainForm.Handle, ErrorMsg10,
                     'Notice', MB_YESNO or MB_ICONEXCLAMATION) = IDYES) then begin
 	      CompfccHandler  := FourCC;
        StrCodec        := 'MS Video Codec';
        FSelCodecRepeat := True;
        end
      else begin
        mImage.Free;
        // Free memory
        FreeFrame(alpbi);
        if Assigned(FOnError) then FOnError(Self, ErrorMsg8);
        RecordState := False;
        FSuccess    := Fail;
        if Assigned(FOnStop) then FOnStop(Self);
        if not FShowPreview then begin
          if Assigned(FOnPreview) then
            FOnPreview(Self, nil, False, False);
          if Assigned(FPreviewTimer) then begin
            FPreviewTimer.Enabled := False;
            end;
          if Assigned(FFrame) then
            ShowWindow(FFrame.Handle, SW_HIDE);
          end;
     	  Result := 0;
        if FRestore then
          Application.Restore;
        Exit;
        end;
      end;
    end;
	// INIT AVI USING FIRST FRAME
	AVIFileInit;

  // If already exist video temp file try to delete it
  if not PowerDeleteFile(TempVideoFile) then begin
    mImage.Free;
    // Free memory
    FreeFrame(alpbi);
    if Assigned(FOnError) then FOnError(Self, ErrorMsg11);
    hr := 1;
    goto Error;
    end;

	// Open the movie file for writing....
	hr := AVIFileOpen(pfile, PChar(TempVideoFile), OF_WRITE or OF_CREATE, nil);
	if (hr <> AVIERR_OK) then begin
    mImage.Free;
    // Free memory
    FreeFrame(alpbi);
    if Assigned(FOnError) then FOnError(Self, ErrorMsg12);
    goto Error;
    end;

	// Fill in the header for the video stream....
	// The video stream will run in 15ths of a second....
  FillChar(strhdr, SizeOf(strhdr), 0);
	strhdr.fccType     := streamtypeVIDEO;        // stream type
	strhdr.fccHandler  := CompfccHandler;         // selected video codec
	strhdr.dwScale     := 1;                      // no time scaling
  strhdr.dwQuality   := FCompressionQuality;    // Compress quality 0-10,000
	strhdr.dwRate      := FPlaybackFPS;           // set playback rate in fps
	strhdr.dwFlags  	 := AVICOMPRESSF_VALID or
                        AVICOMPRESSF_KEYFRAMES; // flags
  for I := 0 to 15 do
    strhdr.szName[I] := Char(FVideoCompressorInfo[FSelectedCompressor].szName[I]);
	strhdr.dwSuggestedBufferSize := alpbi^.biSizeImage;
	SetRect(strhdr.rcFrame,                       // rectangle for stream
          0,
          0,
	        alpbi^.biWidth,
	        alpbi^.biHeight);

	// INIT AVI STREAM
  AVIStreamInit;

	// And create the stream;
	hr := AVIFileCreateStream(pfile,	ps, @strhdr); // returns ps as uncompressed stream pointer
	if (hr <> AVIERR_OK) then	begin
    mImage.Free;
    // Free memory
    FreeFrame(alpbi);
    if Assigned(FOnError) then FOnError(Self, ErrorMsg13);
    goto Error;
    end;

  FillChar(opts, SizeOf(opts), 0);
  LongWord(Option)          := LongWord(@opts);
	Option^.fccType	          := streamtypeVIDEO;        // Stream type
	Option^.fccHandler        := CompfccHandler;         // Selected video codec
	Option^.dwKeyFrameEvery	  := FKeyFramesEvery;        // Keyframe rate
	Option^.dwQuality         := FCompressionQuality;    // Compress quality 0-10,000
	Option^.dwBytesPerSecond  := 0; 	                   // Bytes per second
	Option^.dwFlags	          := AVICOMPRESSF_VALID or
                               AVICOMPRESSF_KEYFRAMES; // Flags
	Option^.lpFormat          := $00;                    // Save format
	Option^.cbFormat          := 0;
	Option^.dwInterleaveEvery := 0;	                     // For non-video streams only

  // Compress ps stream to psCompressed
	hr := AVIMakeCompressedStream(psCompressed,
                                ps,
                                @opts,
                                nil);

	if (hr <> AVIERR_OK) then	begin
    mImage.Free;
    // Free memory
    FreeFrame(alpbi);
    if Assigned(FOnError) then FOnError(Self, ErrorMsg14);
    goto Error;
    end;

  if alpbi^.biBitCount <= 8 then
    InfoImageSize := alpbi^.biSize + alpbi^.biClrUsed * SizeOf(RGBQUAD)
  else
    InfoImageSize := alpbi^.biSize;
  // Stream set format
	hr := AVIStreamSetFormat(psCompressed,
                           0,
    	                     alpbi,	           // Stream format      (this is the first frame!)
			                     InfoImageSize);   // Format size
  // Free memory
 	FreeFrame(alpbi);

	if (hr <> AVIERR_OK) then begin
    mImage.Free;
    if Assigned(FOnError) then FOnError(Self, ErrorMsg16);
    goto Error;
    end;

  if Assigned(FOnStart) then FOnStart(Self); // Notify to start record

  StockAudioRecorder := TStockAudioRecorder.Create(Self);
  if FAudioRecord then begin  // Start audio recording
    StockAudioRecorder.PCMFormat := TPCMFormat(FAudioFormatsIndex + 1);
    StockAudioRecorder.Async     := True;
    // If already exist audio temp file try to delete it
    if not PowerDeleteFile(TempAudioFile) then begin
      mImage.Free;
      if Assigned(FOnError) then FOnError(Self, ErrorMsg17);
      hr := 1;
      goto Error;
      end;
    StockAudioRecorder.RecordToFile(TempAudioFile);
    end;

  mImage.Free;
  // Free memory
  FreeFrame(alpbi);

  Hur                 := 0;
  Min                 := 0;
  Sec                 := 0;
  Mil                 := 0;
  OldUpdateTime       := 0;
	OldComputedFrameNo  := -1;
  FActualFrameNo      := 0;
  FRealCapturingFPS   := 0;
  FSkippedFrames      := 0;
  pTime               := 10;
  OldFrameTime        := TimeGetTime {GetTickCount};
  OldTime             := TimeGetTime {GetTickCount};
	InitialTime         := TimeGetTime {GetTickCount};
  // ==========================  recording loop start ==========================
 	while (RecordState) do begin //repeatedly loop
    CapturedFrame := False;
    // timeexpended = verstrichene Zeit seit Video-Beginn in ms
    TimeExpended  := TimeGetTime {GetTickCount} - InitialTime;

    mImage := TBitmap.Create;
    alpbi  := CaptureScreenFrame(2,
                                 mImage,
                                 FScreenLeft,
                                 FScreenTop,
                                 FScreenWidth,
                                 FScreenHeight,
                                 FFilterCopy);

    if alpbi = nil then begin
      if Assigned(mImage) then
        mImage.Free;
      // Free memory
     	FreeFrame(alpbi);
      if Assigned(FOnError) then FOnError(Self, ErrorMsg18);
      Break;
      end;

    FComputedFrameNo := Round(TimeExpended / FMSPFRecord); // loop duty - time syncronous

    if (FComputedFrameNo - OldComputedFrameNo) > 1 then
      Inc(FSkippedFrames, ((FComputedFrameNo - OldComputedFrameNo) - 1));

    // (video start) or (new loop=(keyframe) necessary) ?
    if FComputedFrameNo > OldComputedFrameNo then begin
      pTime := TimeGetTime {GetTickCount};

      if alpbi^.biBitCount <= 8 then
        InfoImageSize := alpbi^.biSize + alpbi^.biClrUsed * SizeOf(RGBQUAD)
      else
        InfoImageSize := alpbi^.biSize;

	    // if frameno repeats...the avistreamwrite will cause an error
      hr := AVIStreamWrite(psCompressed,           // stream pointer
        	                 FComputedFrameNo,	     // number this frame
                           1,			         	       // number to write
                           PByte(LongWord(alpbi) + // pointer to data
                           InfoImageSize),
                           alpbi^.biSizeImage,	   // size of this frame
                           AVIIF_KEYFRAME,		  	 // flags....
                           nil,
                           nil);

      if (hr <> AVIERR_OK) then begin
        if Assigned(mImage) then
          mImage.Free;
        // Free memory
        FreeFrame(alpbi);
        if Assigned(FOnError) then FOnError(Self, ErrorMsg19);
        Break;
        end;

      Inc(FActualFrameNo); // just a counter
      OldComputedFrameNo := FComputedFrameNo;

      CapturedFrame := True;

      // Current captured frames at one second...
      fTime := TimeGetTime {GetTickCount};
      if (fTime - OldFrameTime) > 0 then
        FrameRate := (1000 / (fTime - OldFrameTime));
      if FrameRate <= FPlaybackFPS then
        FCurrentCapturedFPS := FrameRate
      else
        FCurrentCapturedFPS := FPlaybackFPS;
      // Initialize old frame time status
      OldFrameTime := fTime;

      pTime := TimeGetTime {GetTickCount} - pTime;
      end;

    if mImage.Canvas.TryLock then begin
      try
      if FOnRecordPreview then
        if Assigned(FOnPreview) then
          FOnPreview(Self, mImage, True, True);
      finally
        mImage.Canvas.UnLock;
      end;
      end;

    if Assigned(mImage) then
      mImage.Free;
    // Free memory
    FreeFrame(alpbi);

    RemindTime := MilliSecond2Time(TimeExpended);
    Hur := RemindTime.Hour;
    Min := RemindTime.Minute;
    Sec := RemindTime.Second;
    Mil := RemindTime.MilliSecond;

    FElapsedTime := IntToStr(Hur) + ' : ' +
                    IntToStr(Min) + ' : ' +
                    IntToStr(Sec) + ' : ' +
                    IntToStr(Mil);

    if FTimerRecord.TimerON then
      if ((FTimerRecord.Hour > 0)   or
          (FTimerRecord.Min  > 0)   or
          (FTimerRecord.Sec  > 0))  and
         ((Hur = FTimerRecord.Hour) and
          (Min = FTimerRecord.Min)  and
          (Sec = FTimerRecord.Sec)) then
         StopRecording;

    // Real capturing frames at one second...
    fTime := TimeGetTime {GetTickCount};
    if (fTime - OldTime) > 0 then begin
      if CapturedFrame then
        FRealCapturingFPS := (1000 / (fTime - OldTime))
      else
        FRealCapturingFPS := (1000 / ((fTime - OldTime) + pTime));
      end;
    // Initialize old time status
    OldTime := fTime;

    //Update record stats
    if (TimeExpended > OldUpdateTime + FUpdateRate) then begin
      OldUpdateTime := TimeExpended;
      if Assigned(FOnUpdate) then FOnUpdate(Self); // user event for current status
      end;
	  end;
  // ========================= recording loop ends =============================

Error:

  if Assigned(FOnStop) then FOnStop(Self);

  if Assigned(Option) then
    AVISaveOptionsFree(1, Option);

	if Assigned(ps) then
    AVIStreamRelease(ps);

  if Assigned(psCompressed) then
    AVIStreamRelease(psCompressed);

	if Assigned(pfile) then
    AVIFileRelease(pfile);

  if FAudioRecord and StockAudioRecorder.Active then begin
    // Stop audio recording
    StockAudioRecorder.Active := False;
    StockAudioRecorder.Stop;
    end;
  StockAudioRecorder.Free;

  if FRestore then
    Application.Restore;

  if (hr = AVIERR_OK) and (FSuccess = Success) then begin
    TempCompressed[0] := nil; // Video temp stream
    TempCompressed[1] := nil; // Audio temp stream

    // Extract video temp stream to TempCompressed[0]
    LoadAVIFileToStream(TempVideoFile, TempCompressed[0]);

    if FAudioRecord then
      // Extract audio temp stream to TempCompressed[1]
      LoadAVIFileToStream(TempAudioFile, TempCompressed[1]);

    // N = Number of streams { Video(0)-Audio(1) }
    if Assigned(TempCompressed[1]) then N := 2 else N := 1;

    // Save all streams to final file
    if Assigned(TempCompressed[0]) or Assigned(TempCompressed[1]) then
      SavingSuccess := FinalSaveAvi(szFileName, N, TempCompressed);

    // Release all streams
    if Assigned(TempCompressed[0]) then
      AVIStreamRelease(TempCompressed[0]);
    if Assigned(TempCompressed[1]) then
      AVIStreamRelease(TempCompressed[1]);
    end;

  AVIStreamExit;
 	AVIFileExit;

	if (hr <> AVIERR_OK) then begin
 		if (CompfccHandler <> FourCC)	then begin
			if (IDYES = MessageBox(Application.MainForm.Handle, PChar(ErrorMsg9), 'Notice',
                                MB_YESNO or MB_ICONEXCLAMATION)) then begin
				CompfccHandler  := FourCC;
        StrCodec        := 'MS Video Codec';
        // indicate to restart recording...
        FSuccess        := Fail;
        Result          := -1;
        FSelCodecRepeat := True;
			  end
      else begin
        if Assigned(FOnError) then FOnError(Self, ErrorMsg8);
        if not FShowPreview then begin
          if Assigned(FOnPreview) then
            FOnPreview(Self, nil, False, False);
          if Assigned(FPreviewTimer) then begin
            FPreviewTimer.Enabled := False;
            end;
          if Assigned(FFrame) then
            ShowWindow(FFrame.Handle, SW_HIDE);
          end;
        // indicate to cancel recording...
        FSuccess := Fail;
        RecordState := False;
        if Assigned(FOnStop) then FOnStop(Self);
        Result   := 0;
        FSelCodecRepeat := False;
        if FRestore then
          Application.Restore;
        end;
		  end
    else begin
      if Assigned(FOnError) then FOnError(Self, ErrorMsg20);
      if not FShowPreview then begin
        if Assigned(FOnPreview) then
          FOnPreview(Self, nil, False, False);
        if Assigned(FPreviewTimer) then begin
          FPreviewTimer.Enabled := False;
          end;
        if Assigned(FFrame) then
          ShowWindow(FFrame.Handle, SW_HIDE);
        end;
      // indicate to cancel recording...
      FSuccess := Fail;
      RecordState := False;
      if Assigned(FOnStop) then FOnStop(Self);
      Result := 0;
      FSelCodecRepeat := False;
      if FRestore then
        Application.Restore;
      end;
    Exit;
    end;

  FSelCodecRepeat := False;
	//Save the file on success
  Result := 1;
end;

procedure TScreenCamera.StopRecording;
begin
  FSuccess    := Success;
  RecordState := False;
  if not FShowPreview then
    SetShowPreview(False)
  else
    SetShowPreview(True);
end;

function TScreenCamera.StartRecording(szFileName: string): Boolean;
begin
  if RecordState then begin
    Result := False;
    Exit; // exit if still recording
    end;
  FFileName := szFileName;
  if FMinimize then
    Application.Minimize;
  case FScreenRegion of
    SelObject  : begin
                   FSelectObject := True;
                   FFreeHandMode := False;
                   FAutoPan      := False;
                   FFullScreen   := False;
                 end;
    FreeHand   : begin
                   FSelectObject := False;
                   FFreeHandMode := True;
                   FAutoPan      := False;
                   FFullScreen   := False;
                 end;
    FixedMoving: begin
                   FSelectObject := False;
                   FFreeHandMode := False;
                   FAutoPan      := True;
                   FFullScreen   := False;
                 end;
    FixedStable: begin
                   FSelectObject := False;
                   FFreeHandMode := False;
                   FAutoPan      := False;
                   FFullScreen   := False;
                 end;
    FullScreen : begin
                   FSelectObject := False;
                   FFreeHandMode := False;
                   FAutoPan      := False;
                   FFullScreen   := True;
                 end;
    end;
  if not Assigned(FPreviewTimer) then begin
    FPreviewTimer := THighTimer.Create(Self);
    FPreviewTimer.OnTimer  := FShowPreviewTimer;
    FPreviewTimer.Interval := 1000 div FPlaybackFPS;
    FPreviewTimer.ThreadPriority := FPriority;
    FPreviewTimer.Synchronize := True;
    FPreviewTimer.UseThread := True;
    end;
  FPreviewTimer.Enabled := True;
  if FFreeHandMode or FSelectObject then begin
    StartRegionSel := True;
    Result         := False;
    Exit; // exit if region selecting
    end;
  StartRegionSel := False;
  if FFullScreen then
    SetSizeFullScreen;
  FRecordAVIThread := TRecordAVIThread.Create(Self);
  FRecordAVIThread.OnTerminate := ThreadDone;
  SetPriority(FPriority);
  RecordState := True;
  Result      := True;
end;

// message from thread informing that it is done
procedure TScreenCamera.ThreadDone(Sender: TObject);
begin
  RecordState      := False;
  FRecordAVIThread := nil;
end;

function SaveCallBack(nPercent: Integer): LONG; stdcall;
var
  C : Boolean;
begin
  Result := 0;
  if Assigned(SC) then begin
    if Assigned(SC.OnSaving) then begin
      C := True;
      SC.OnSaving(SC, nPercent, SavingMsg, C);
      if not C then
        Result := -1;
      end;
    end;  
end;

function TScreenCamera.FinalSaveAvi(const FileName: String;
         nStreams: Integer; Streams: APAVISTREAM): Boolean;
var
  AVIERR          : Cardinal;
  ErrMess         : string;
  CompressOptions : APAVICOMPRESSOPTIONS;
  OldCursor       : TCursor;
begin
  CompressOptions[0] := nil;
  CompressOptions[1] := nil;

  // If already exist video file try to delete it
  if not PowerDeleteFile(FileName) then begin
    if Assigned(FOnError) then FOnError(Self, ErrorMsg21);
    Result := False;
    Exit;
    end;

  OldCursor     := Screen.Cursor;
  Screen.Cursor := crAppStart;
  AVIERR := AVISaveV(PChar(FileName),   // File name
                     nil,               // File handler
                     SaveCallBack,      // Callback
                     nStreams,          // Number of streams
                     Streams,           // Audio/Video streams
                     CompressOptions);  // Compress options for Streams
  Screen.Cursor := OldCursor;

  ErrMess := '';
  case AVIERR of
      AVIERR_OK:
            begin
              Result := True;
              Exit;
            end;
      AVIERR_UNSUPPORTED:
          ErrMess := ErrorMsg22;
      AVIERR_BADFORMAT:
          ErrMess := ErrorMsg23;
      AVIERR_FILEREAD:
          ErrMess := ErrorMsg24;
      AVIERR_FILEWRITE:
          ErrMess := ErrorMsg25;
      AVIERR_MEMORY:
          ErrMess := ErrorMsg26;
      AVIERR_INTERNAL:
          ErrMess := ErrorMsg27;
      AVIERR_BADFLAGS:
          ErrMess := ErrorMsg28;
      AVIERR_BADPARAM:
          ErrMess := ErrorMsg29;
      AVIERR_BADSIZE:
          ErrMess := ErrorMsg30;
      AVIERR_BADHANDLE:
          ErrMess := ErrorMsg31;
      AVIERR_FILEOPEN:
          ErrMess := ErrorMsg32;
      AVIERR_COMPRESSOR:
          ErrMess := ErrorMsg33;
      AVIERR_NOCOMPRESSOR:
          ErrMess := ErrorMsg34;
      AVIERR_READONLY:
          ErrMess := ErrorMsg35;
      AVIERR_NODATA:
          ErrMess := ErrorMsg36;
      AVIERR_BUFFERTOOSMALL:
          ErrMess := ErrorMsg37;
      AVIERR_CANTCOMPRESS:
          ErrMess := ErrorMsg38;
      AVIERR_USERABORT:
          ErrMess := ErrorMsg39;
      AVIERR_ERROR:
          ErrMess := ErrorMsg40;
      else
        ErrMess   := ErrorMsg41 + IntToStr(AVIERR);
      end;
  if ErrMess <> '' then begin
    if Assigned(FOnError) then FOnError(Self, ErrMess);
    if Assigned(FOnStop) then FOnStop(Self);
    Result := False;
    end;
end;

procedure TScreenCamera.LoadAVIFileToStream(const FileName: String;
          var TempAVIStream: PAVIStream);
var
   InputFile : PAVIFILE;
   hr        : Integer;
   Msg       : String;
begin
   // Open the video or audio file.
   if FileExists(FileName) then begin
     hr := AVIFileOpen(InputFile, PChar(FileName), OF_READ, nil);
     case hr of
        AVIERR_OK           : Msg := '';
        AVIERR_BADFORMAT    : Msg := ErrorMsg42;
        AVIERR_MEMORY       : Msg := ErrorMsg43;
        AVIERR_FILEREAD     : Msg := ErrorMsg44;
        AVIERR_FILEOPEN     : Msg := ErrorMsg45;
        REGDB_E_CLASSNOTREG : Msg := ErrorMsg46;
        else                  Msg := ErrorMsg47;
     end;
     if Msg <> '' then
       if Assigned(FOnError) then FOnError(Self, Msg);
     // Open the stream.
     try
       if (AVIFileGetStream(InputFile, TempAVIStream, 0, 0) <> AVIERR_OK) then
         if Assigned(FOnError) then FOnError(Self, ErrorMsg48);
     finally
         AviFileRelease(InputFile);
       end;
     end;
end;

// --------------------------------------------------------------------------
//    TRecordAVI Thread
// --------------------------------------------------------------------------

constructor TRecordAVIThread.Create(ScrCam: TScreenCamera);
begin
  inherited Create(False);
  FScrCam := ScrCam;
  FreeOnTerminate := True;
end;


{ The Execute method is called when the thread starts }
procedure TRecordAVIThread.Execute;
var
  Res      : Integer;
  Variable : Boolean;
  function IsDirectory(const DirName: string): Boolean;
  var
    Attr: Integer;  // directory's file attributes
  begin
    Attr := SysUtils.FileGetAttr(DirName);
    Result := (Attr <> -1) and (Attr and SysUtils.faDirectory = SysUtils.faDirectory);
  end;
begin
  if IsDirectory(ExtractFilePath(FFileName)) then begin
    repeat
      Res := FScrCam.RecordVideo(FFileName);
    until not (Res = -1);
    Variable := True;
    if SavingSuccess then
      if Assigned(SC) then
        if Assigned(SC.OnSaving) then
          SC.OnSaving(SC, 100, SavingSuccessMsg, Variable);
    end
  else begin
    if Assigned(SC) then
      if Assigned(SC.OnError) then
        SC.OnError(SC, ErrorMsg49);
    end;
  // If already exist temp files try to delete them
  PowerDeleteFile(TempVideoFile);
  PowerDeleteFile(TempAudioFile);
end;

end.

