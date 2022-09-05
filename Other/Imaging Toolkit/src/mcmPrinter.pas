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
// $Log:  17577: mcmPrinter.pas 
//
//    Rev 1.9    2014-02-02 21:10:06  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.8    20-08-2007 20:28:26  mcm
// Added support for Delphi 2007
//
//    Rev 1.7    01-07-2006 11:37:06  mcm    Version: IMG 3.0
// Enabled displaying "fax" images in correct dimension by using the horizontal
// and vertical resolution to scale the vertical direction additionally.
//
//    Rev 1.6    29-01-2006 12:07:14  mcm    Version: IMG 2.14
// Fixed transparency view in TmcmPrintPreview.
//
//    Rev 1.5    14-01-2006 10:46:24  mcm
//
//   Rev 1.4    29-09-2003 18:43:06  mcm    Version: IMG 1.6
// Added option to disable Range check.

//
//   Rev 1.3    06-07-2003 10:54:10  mcm    Version: IMG 1.3.4
// Modified to work in BCB.

//
//   Rev 1.2    17-02-2003 10:32:04  mcm    Version: IMG 1.3
// Clean-up

//
//   Rev 1.1    27-01-2003 13:43:00  mcm

//
//   Rev 1.0    27-05-2002 16:22:22  mcm

unit mcmPrinter;

interface

{$Include 'mcmDefines.pas'}

{$IFOPT H-}{$DEFINE LONGSTRINGSON}{$H+}{$ENDIF}

{$IFDEF VER150} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls,
      Forms, Dialogs, Printers, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
      Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Printers, Vcl.ExtCtrls,
     {$ENDIF}
     mcmImage,
     mcmPrintProgress;

const                     
  // Pixels
  // DefaultBorderWidth = 2;
  DefaultDPI = 300;

  // Inches
  DefaultAvailablePageHeightIn = 10.5;
  DefaultAvailablePageWidthIn = 8.0;
  DefaultGutterLeftIn = 0.25;
  DefaultGutterTopIn = 0.25;
  DefaultPhysicalPageHeightIn = 11.0;
  DefaultPhysicalPageWidthIn = 8.5;

  // Millimeters
  DefaultAvailablePageHeightMm = 284.0;
  DefaultAvailablePageWidthMm = 198.0;
  DefaultGutterLeftMm = 6.0;
  DefaultGutterTopMm = 6.0;
  DefaultPhysicalPageHeightMm = 297.0;
  DefaultPhysicalPageWidthMm = 210.0;


  // Progress Dialog Messages
  ProgressFinishMsg = 'Finished';
  SendingPagesMsg = 'Sending page(s) to printer';

type
  EPagePrinter    = class(EPrinter);
  ECancelPrinting = class(EPagePrinter);

  TUnitOfMeasurement = (UM_INCH, UM_MILLIMETER);
  TZoomMode          = (ZM_FITTOPAGE, ZM_FITTOWIDTH, ZM_FITTOHEIGHT, ZM_FITTOPERCENT);
  TZoomPos           = (ZP_TOPLEFT, ZP_TOPCENTER, ZP_CENTER);
  TPageDir           = (PD_HORIZ, PD_VERT);
  TPageBorder        = (PB_TOP, PB_BOTTOM, PB_LEFT, PB_RIGHT);
  TPageBorders       = set of TPageBorder;


type
  TmcmPrintPage = class(TmcmImage)
  private
    // Private declarations
    FFrames : TList;
  protected
    // Protected declarations
  public
    // public declarations
    constructor Create; override;
    destructor Destroy; override;
  published
    // published declarations
  end;


  TmcmPrintPageList = class(TList)
  private
    // Private declarations
  protected
    // Protected declarations
  public
    // public declarations
    destructor Destroy; override;
    function   GetPage(const Index : integer) : TmcmPrintPage;
  end;

  // Forward declaration.
  TmcmPrintPreview = class;

  TmcmPrinter = class(TComponent)
  private
    { Private declarations }
    FmcmPrintPreview : TmcmPrintPreview;
    FAbortOnCancel   : boolean; // Abort
    FCancelPrint     : boolean; // Cancel print in progress

    FCollate         : boolean;
    FCopies          : integer;

    Fcx                : integer; // Current x
    Fcy                : integer; // Current y

    FFileName          : WideString;
    FTextFile          : TextFile;

    FForceMargin       : boolean;
    FMarginBottom      : double;
    FMarginLeft        : double;
    FMarginRight       : double;
    FMarginTop         : double;

    FUnitOfMeasurement : TUnitOfMeasurement;

    FOnNewPage         : TNotifyEvent;
    FPage              : TmcmPrintPage; // Is TmcmImage
    FPageBorderColor   : TColor;
    FPageBorderOffset  : double;
    FPageBorderWidth   : integer;
    FPageBorders       : TPageBorders;
    FPageNo            : integer;
    FPages             : TmcmPrintPageList;
    FPrintProgress     : TmcmPrintProgress;
    FPrinter           : TPrinter;
    FPrintFromPage     : integer;
    FPrinting          : boolean;
    FPrintingToFile    : boolean;
    FPrintToFile       : boolean;
    FPrintToPage       : integer;
    FProgressMessage   : WideString;
    FShowCancel        : boolean;
    FShowProgress      : boolean;
    FCreating          : boolean;
    FTextMetrics       : TTextMetric;
    FUpdateRefCount    : integer;
    FImageCenter       : boolean;
    FImageFitToPage    : boolean;
    FImageScale        : word;
    FSymetricGutter    : boolean;

    function    GetAvailablePageHeight : double;
    function    GetAvailablePageWidth : double;
    function    GetCollate : boolean;
    function    GetCopies : integer;
    function    GetFileName : WideString;
    function    GetGutterBottom : double;
    function    GetGutterLeft : double;
    function    GetGutterRight : double;
    function    GetGutterTop : double;
    function    GetHorizDPI : integer;
    function    GetImageCenter : boolean;
    function    GetImageFitToPage : boolean;
    function    GetImageScale : word;
    function    GetMarginBottom : double;
    function    GetMarginLeft : double;
    function    GetMarginRight : double;
    function    GetMarginTop : double;
    function    GetOrientation : TPrinterOrientation;
    function    GetPageBorderOffset : double;
    function    GetPageBorders : TPageBorders;
    function    GetPageCount : integer;
    function    GetPages(Index : word) : TmcmPrintPage;
    function    GetPhysicalPageHeight : double;
    function    GetPhysicalPageWidth : double;
    function    GetPrintableHeight : double;
    function    GetPrintableWidth : double;
    function    GetPrintFromPage : integer;
    function    GetPrinting : boolean;
//    function    GetPrintToFile : boolean;
    function    GetPrintToPage : integer;
    function    GetProgressMessage : WideString;
    function    GetShowCancel : boolean;
    function    GetShowProgress : boolean;
    function    GetTitle : WideString;
    function    GetUnitOfMeasurement : TUnitOfMeasurement;
    function    GetVertDPI : integer;

    procedure   OnCancelPrinting(Sender : TObject);
    procedure   OnPageChanged(Sender : TObject);
    function    PixelPrintHeight : integer;
    function    PixelPrintWidth : integer;
    procedure   ProcessPage;
    procedure   ResetPageList(CreateForReal : boolean);
    procedure   SetCollate(Value : boolean);
    procedure   SetCopies(Value : integer);
    procedure   SetFileName(Value : WideString);
    procedure   SetImageCenter(Value : boolean);
    procedure   SetImageFitToPage(Value : boolean);
    procedure   SetImageScale(Value : word);
    procedure   SetMarginBottom(Value : double);
    procedure   SetMarginLeft(Value : double);
    procedure   SetMarginRight(Value : double);
    procedure   SetMarginTop(Value : double);
    procedure   SetOrientation(Value : TPrinterOrientation);
    procedure   SetPageBorderOffset(Value : double);
    procedure   SetPageBorders(Value : TPageBorders);
    procedure   SetPrintFromPage(Value : integer);
//    procedure   SetPrintToFile(Value : boolean);
    procedure   SetPrintToPage(Value : integer);
    procedure   SetProgressMessage(Value : WideString);
    procedure   SetShowCancel(Value : boolean);
    procedure   SetShowProgress(Value : boolean);
    procedure   SetSymetricGutter(Value : boolean);
    procedure   SetTitle(Value : WideString);
    procedure   SetUnitOfMeasurement(Value : TUnitOfMeasurement);
    function    StartingBottom : integer;
    function    StartingLeft : integer;
    function    StartingRight : integer;
    function    StartingTop : integer;
  protected
    { Protected declarations }
    function    GetPrinterHandle : HDC;
    procedure   Loaded; override;
    procedure   SetPixelsPerInch;
    procedure   UpdateProgressDlg(const Status: WideString; const CurrentPage, FromPage, ToPage : integer);
    property    mcmPrintPreview : TmcmPrintPreview
      read      FmcmPrintPreview
      write     FmcmPrintPreview;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;

    function    AddPage : integer;
    procedure   BeginDoc;
    procedure   BeginUpdate;
    procedure   Clear;
    procedure   EndDoc;
    procedure   EndUpdate;
    function    HorizPixelsToUnit(const Value : integer) : double;
    function    HorizUnitToPixels(const Value : double) : integer;
    function    VertPixelsToUnit(const Value : integer) : double;
    function    VertUnitToPixels(const Value : double) : integer;
    function    Print : boolean;
    procedure   RefreshProperties;

    property    AvailablePageHeight : double
      read      GetAvailablePageHeight;
    property    AvailablePageWidth : double
      read      GetAvailablePageWidth;
    property    GutterBottom : double
      read      GetGutterBottom;
    property    GutterLeft : double
      read      GetGutterLeft;
    property    GutterRight : double
      read      GetGutterRight;
    property    GutterTop : double
      read      GetGutterTop;
   property     HorizDPI : integer
     read       GetHorizDPI;
    property    PageCount : integer
      read      GetPageCount;
    property    Pages[Index : word] : TmcmPrintPage
      read      GetPages;
    property    PhysicalPageHeight : double
      read      GetPhysicalPageHeight;
    property    PhysicalPageWidth : double
      read      GetPhysicalPageWidth;
    property    PrintableHeight : double
      read      GetPrintableHeight;
    property    PrintableWidth : double
      read      GetPrintableWidth;
    property    PrintFromPage : integer
      read      GetPrintFromPage
      write     SetPrintFromPage default 0;
    property    Printing : boolean
      read      GetPrinting;
    property    PrintToPage : integer
      read      GetPrintToPage
      write     SetPrintToPage default 0;
   property     VertDPI : integer
     read       GetVertDPI;
  published
    { Published declarations }

    {Because everything in TmcmPrinter depends on it, this property MUST be the first
    TmcmPrinter-specific property loaded.  If you edit the form as text and move it
    around in the streaming order, things may not work correctly.  This dependency kind
    of stinks, but I can't think of any way around it.  Sorry.}

    property    UnitOfMeasurement : TUnitOfMeasurement
      read      GetUnitOfMeasurement
      write     SeTUnitOfMeasurement default um_Inch;
    property    AbortOnCancel : boolean
      read      FAbortOnCancel
      write     FAbortOnCancel default False;
    property    Collate : boolean
      read      GetCollate
      write     SetCollate default True;
    property    Copies : integer
      read      GetCopies
      write     SetCopies default 1;
    property    FileName : WideString
      read      GetFileName
      write     SetFileName;
    property    ForceMargin : boolean
      read      FForceMargin
      write     FForceMargin default False;
    property    ImageCenter : boolean
      read      GetImageCenter
      write     SetImageCenter default False;
    property    ImageFitToPage : boolean
      read      GetImageFitToPage
      write     SetImageFitToPage default False;
    property    ImageScale : word
      read      GetImageScale
      write     SetImageScale default 100;
    property    MarginBottom : double
      read      GetMarginBottom
      write     SetMarginBottom;
    property    MarginLeft : double
      read      GetMarginLeft
      write     SetMarginLeft;
    property    MarginRight : double
      read      GetMarginRight
      write     SetMarginRight;
    property    MarginTop : double
      read      GetMarginTop
      write     SetMarginTop;
    property    OnNewPage : TNotifyEvent
      read      FOnNewPage
      write     FOnNewPage;
    property    Orientation : TPrinterOrientation
      read      GetOrientation
      write     SetOrientation default poPortrait;
    property    PageBorderColor : TColor
      read      FPageBorderColor
      write     FPageBorderColor default 0;
    property    PageBorderOffset : double
      read      GetPageBorderOffset
      write     SetPageBorderOffset;
    property    PageBorders : TPageBorders
      read      GetPageBorders
      write     SetPageBorders default [];
    property    PageBorderWidth : integer
      read      FPageBorderWidth
      write     FPageBorderWidth default 2;
    {
    property    PrintToFile : boolean
      read      GetPrintToFile
      write     SetPrintToFile default False;
    }
    property    ProgressText : WideString
      read      GetProgressMessage
      write     SetProgressMessage;
    property    ShowCancel : boolean
      read      GetShowCancel
      write     SetShowCancel default True;
    property    ShowProgress : boolean
      read      GetShowProgress
      write     SetShowProgress default True;
    property    SymetricGutter : boolean
      read      FSymetricGutter
      write     SetSymetricGutter default False;
    property    Title : WideString
      read      GetTitle
      write     SetTitle nodefault;
  end;

  TmcmPrintPreview = class(TScrollBox)
  private
    { Private declarations }
    FmcmPrinter      : TmcmPrinter;
    FPrintPage       : TmcmImageCtrl;
    FPrinter         : TPrinter;
    FCreating        : boolean;

    FPaintBox        : TPaintBox;
    FShadowColor     : TColor;
    FShadowOffset    : integer;

    FZoomMode        : TZoomMode;
    FZoomPos         : TZoomPos;
    FZoomPercent     : integer;
    FResizeEnabled   : boolean;

    FShowMargins     : boolean;
    FTextMetrics     : TTextMetric;
    FShowPage        : integer;

    procedure   CMFontChanged(var Msg : TMessage); Message CM_FONTCHANGED;
  protected
    { Protected declarations }
    function    GetImageCenter : boolean;
    function    GetImageFitToPage : boolean;
    function    GetPreviewPageHoriz : integer;
    function    GetPreviewPageVert : integer;
    function    GetScaleFactor(Dir : TPageDir) : double;
    function    GetShadowColor : TColor;
    function    GetShadowOffset : integer;
    function    GetShowMargins : boolean;
    function    GetShowPage : integer;
    function    GetZoomPercent : integer;
    procedure   Loaded; override;
    procedure   OnPageChanged(Sender : TObject);
    procedure   PaintPreview(Sender : TObject); virtual; // Event handle for TPaintBox
    procedure   Resize; override;
    procedure   Resizing(State : TWindowState); {$IFNDEF DCB3_4} override; {$ENDIF}
    function    ScreenScaleValue(Value : double; Dir : TPageDir): integer;
    procedure   SetImageCenter(Value : boolean);
    procedure   SetImageFitToPage(Value : boolean);
    procedure   SetmcmPrinter(Value : TmcmPrinter);
    procedure   SetShadowColor(Value : TColor);
    procedure   SetShadowOffset(Value : integer);
    procedure   SetShowMargins(Value : boolean);
    procedure   SetShowPage(Value : integer);
    procedure   SetZoomPercent(Value : integer);
    function    UnitsToScreen(const Value : double; Dir : TPageDir) : integer;
    procedure   UpdateDesigner;
    procedure   UpdatePagePreview;
    property    ImageCenter : boolean
      read      GetImageCenter
      write     SetImageCenter;
    property    ImageFitToPage : boolean
      read      GetImageFitToPage
      write     SetImageFitToPage;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   Invalidate; override;
    procedure   ZoomToFit;
    procedure   ZoomToHeight;
    procedure   ZoomToWidth;
    property    PageIndex : integer
      read      GetShowPage
      write     SetShowPage;
  published
    { Published declarations }
    property    Align;
    property    Color;
    property    DragCursor;
    property    DragMode;
    property    Enabled;
    property    mcmPrinter : TmcmPrinter
      read      FmcmPrinter
      write     SetmcmPrinter default Nil;
    property    OnStartDrag;
    property    ParentColor;
    property    ParentFont default True;
    property    ShadowColor : TColor
      read      GetShadowColor
      write     SetShadowColor default clBtnShadow;
    property    ShadowOffset : integer
      read      GetShadowOffset
      write     SetShadowOffset default 5;
    property    ShowHint;
    property    ShowMargins : boolean
      read      GetShowMargins
      write     SetShowMargins default True;
    property    Visible;
    property    ZoomPercent : integer
      read      GetZoomPercent
      write     SetZoomPercent default 25;
    property    ZoomPos : TZoomPos
      read      FZoomPos
      write     FZoomPos default zp_TopLeft;
  end;

implementation

{$IFDEF GE_DXE2} uses System.Types; {$ENDIF}

{ $R mcmPrinter.dcr}


function Minimum(Value1, Value2 : integer) : integer;
begin
  if (Value1 > Value2)
  then Result := Value2
  else Result := Value1;
end; // Minimum.


//------------------------------------------------------------------------------
// TmcmPrintPage.
//------------------------------------------------------------------------------

constructor TmcmPrintPage.Create;
begin
  Inherited Create;
  FFrames := TList.Create;
end; // TmcmPrintPage.Create.


destructor TmcmPrintPage.Destroy;
begin
  FFrames.Free;
  Inherited Destroy;
end; // TmcmPrintPage.Destroy.

//------------------------------------------------------------------------------
// TPageList.
//------------------------------------------------------------------------------

destructor TmcmPrintPageList.Destroy;
var i    : Integer;
    Page : TmcmPrintPage;
begin
  for i := 0 to (Count - 1)
  do begin
     Page := GetPage(i);
     if (Page <> Nil)
     then begin
          Page.ReleaseHandle;
          Page.Free;
     end;
  end;
  inherited Destroy;
end;


function TmcmPrintPageList.GetPage(const Index : Integer): TmcmPrintPage;
begin
  Result := TmcmPrintPage(Items[Index]);
end;


//------------------------------------------------------------------------------
// TmcmPrinter
//------------------------------------------------------------------------------

constructor TmcmPrinter.Create(AOwner : TComponent);
begin
  FmcmPrintPreview := Nil;
  FPage := Nil;
  FCreating := True;
  inherited Create(AOwner);


  FUpdateRefCount := 0;

  FPrintProgress := Nil;
  FCancelPrint   := False;
  AbortOnCancel  := False;

  Fcx     := 0;
  Fcy     := 0;
  FPageNo := 0;
  FPrintingToFile := False;

  FillChar(FTextFile, SizeOf(TextFile), #0);

  FPrinter := {$IFDEF GE_DXE2}Vcl.{$ENDIF}Printers.Printer;

  FPrinting       := False;
  FSymetricGutter := False;

  ShowProgress   := True;
  ShowCancel     := True;

  ResetPageList(False);

  Title              := '';

  PageBorders        := [];
  FPageBorderColor   := RGB(0, 0, 0);
  FPageBorderWidth   := 2;
  PageBorderOffset   := 0;

  FPrintToFile       := False;
  Collate            := True;
  Copies             := 1;
  Orientation        := poPortrait;
  PrintToPage        := 0;
  PrintFromPage      := 0;

  FUnitOfMeasurement := um_Inch;
  FForceMargin       := False;
  MarginTop          := GutterTop;
  MarginBottom       := GutterBottom;
  MarginLeft         := GutterLeft;
  MarginRight        := GutterRight;
  FImageCenter       := False;
  FImageFitToPage    := False;
  FImageScale        := 100;

  FCreating := False;
end;


destructor TmcmPrinter.Destroy;
begin
  FPages.Free;
  inherited Destroy;
end;


procedure TmcmPrinter.Clear;
begin
  if Not(Printing)
  then ResetPageList(False)
  else raise EPagePrinter.Create('Cannot clear contents while printing.');
end;


function TmcmPrinter.GetHorizDPI : integer;
begin
  Result := GetDeviceCaps(FPrinter.Handle, LOGPIXELSX);
end; // TmcmPrinter.GetHorizDPI.


function TmcmPrinter.GetVertDPI : integer;
begin
  Result := GetDeviceCaps(FPrinter.Handle, LOGPIXELSY);
end; // TmcmPrinter.GetVertDPI.


procedure TmcmPrinter.Loaded;
begin
  inherited Loaded;
  ResetPageList(False);
end;

procedure TmcmPrinter.OnPageChanged(Sender : TObject);
begin
  if Assigned(FmcmPrintPreview)
  then begin
       FmcmPrintPreview.PageIndex := FmcmPrintPreview.PageIndex;
  end;
end;

procedure TmcmPrinter.BeginDoc;
begin
  RefreshProperties;
  FPrinting := True;

  if FPrintToFile
  then begin
       SetPixelsPerInch;
       AssignFile(FTextFile, FileName);
       Rewrite(FTextFile);
       FPrintingToFile := True;
  end
  else ResetPageList(True);

  // Ensure that the Font.PixelsPerInch is property set correctly
  SetPixelsPerInch;
  FPageNo := 1;
  ProcessPage;
end;


procedure TmcmPrinter.EndDoc;
begin
  FPrinting := False;

  if FPrintToFile
  then begin
       CloseFile(FTextFile);
       FillChar(FTextFile, SizeOf(FTextFile), #0);
       FPrintingToFile := False;
  end
  else begin
       // UpdatePagePreview;
       // Invalidate;
  end;
end;


function TmcmPrinter.AddPage : integer;
begin
  FPage := TmcmPrintPage.Create;
  FPages.Add(FPage);
  FPage.OnChange := OnPageChanged;
  FPage := Nil;
  if (FPages.Count = 1)
  then if Assigned(FmcmPrintPreview)
       then begin
            FmcmPrintPreview.PageIndex := 1;
       end;
  FPageNo := FPages.Count - 1;

  if Assigned(FOnNewPage)
  then FOnNewPage(Self);
  
  Result := FPageNo;
end;


procedure TmcmPrinter.RefreshProperties;
begin
  if Not(Printing)
  then begin
       // Ensure margins are correct according to the current printer
       SetMarginTop(MarginTop);
       SetMarginBottom(MarginBottom);
       SetMarginLeft(MarginLeft);
       SetMarginRight(MarginRight);
  end;
  // Ensure font gets re-sized for the page
  GetTextMetrics(GetPrinterHandle, FTextMetrics);
  SetPixelsPerInch;
  if Assigned(FmcmPrintPreview)
  then FmcmPrintPreview.UpdatePagePreview;
end;


procedure TmcmPrinter.ProcessPage;
var
//   PixelPageBorderOffset: integer;
//   OldFont: TFont;
   CurX, CurY : Integer;
begin
  // Reset the fields and fire new page event
  Fcx := 0;
  Fcy := 0;
  {
  if Assigned(FOnNewPage)
  then FOnNewPage(Self);
  }
  // Save values in case OnNewPage modifies them
  CurX := Fcx;
  CurY := Fcy;

//  OldFont := TFont.Create;
//  OldFont.Assign(Font);

  try
    // Print header !!!!!

    // Print footer !!!!!

  finally
    // Restore the original values
//    Font.Assign(OldFont);
//    OldFont.Free;
  end;

  Fcx := CurX;
  Fcy := CurY;

  // Print PageBorders.
  (*
  with Canvas
  do begin
     Pen.Width := DefaultBorderWidth * (GetDeviceCaps(FPrinter.Handle, LOGPIXELSY) div DefaultDPI);
     PixelPageBorderOffset := VertUnitToPixels(PageBorderOffset);

     if (pb_Top in PageBorders)
     then begin
          MoveTo(StartingLeft - PixelPageBorderOffset, StartingTop - PixelPageBorderOffset);
          LineTo(StartingLeft + PixelPrintWidth + PixelPageBorderOffset, StartingTop - PixelPageBorderOffset);
     end;

     if (pb_Bottom in PageBorders)
     then begin
          MoveTo(StartingLeft - PixelPageBorderOffset, StartingTop + PixelPrintHeight + PixelPageBorderOffset);
          LineTo(StartingLeft + PixelPrintWidth + PixelPageBorderOffset, StartingTop + PixelPrintHeight + PixelPageBorderOffset);
     end;

     Pen.Width := DefaultBorderWidth * (GetDeviceCaps(FPrinter.Handle, LOGPIXELSX) div DefaultDPI);
     PixelPageBorderOffset := HorzUnitToPixels(PageBorderOffset);

     if (pb_Left in PageBorders)
     then begin
          MoveTo(StartingLeft - PixelPageBorderOffset, StartingTop - PixelPageBorderOffset);
          LineTo(StartingLeft - PixelPageBorderOffset, StartingTop + PixelPrintHeight + PixelPageBorderOffset);
     end;

     if (pb_Right in PageBorders)
     then begin
          MoveTo(StartingLeft + PixelPrintWidth + PixelPageBorderOffset, StartingTop - PixelPageBorderOffset);
          LineTo(StartingLeft + PixelPrintWidth + PixelPageBorderOffset, StartingTop + PixelPrintHeight + PixelPageBorderOffset);
     end;
  end;

  *)
end;


function TmcmPrinter.PixelPrintWidth : integer;
begin
  try
    Result := HorizUnitToPixels(AvailablePageWidth) - StartingLeft - StartingRight;
  except
    On ERangeError
    do Result := 0;
  end;
end;


function TmcmPrinter.PixelPrintHeight : integer;
begin
  try
    Result := VertUnitToPixels(AvailablePageHeight) - StartingTop - StartingBottom;
  except
    On ERangeError
    do Result := 0;
  end;
end;


function TmcmPrinter.GetTitle : WideString;
begin
  try
    Result := FPrinter.Title;
  except
    On EPrinter
    do Result := 'Unknown';
  end;
end;


procedure TmcmPrinter.SetTitle(Value : WideString);
begin
  try
    FPrinter.Title := Value;
  except
    On EPrinter
    do ;
  end;
end;


function TmcmPrinter.GetOrientation : TPrinterOrientation;
begin
  try
    Result := FPrinter.Orientation;
  except
    On EPrinter
    do Result := poPortrait;
  end;
end;


procedure TmcmPrinter.SetOrientation(Value : TPrinterOrientation);
begin
  if Not(Printing)
  then begin
       try
         FPrinter.Orientation := Value;
       except
         On EPrinter
         do ;
       end;
       if Assigned(FmcmPrintPreview)
       then begin
            FmcmPrintPreview.UpdatePagePreview;
            FmcmPrintPreview.Invalidate;
       end;
  end
  else raise EPagePrinter.Create('Cannot change orientation while printing!');
end;


procedure TmcmPrinter.SetSymetricGutter(Value : boolean);
begin
  if (Value <> FSymetricGutter)
  then begin
       FSymetricGutter := Value;
       if Assigned(FmcmPrintPreview)
       then begin
            FmcmPrintPreview.UpdatePagePreview;
            FmcmPrintPreview.Invalidate;
       end;
  end;
end;


function TmcmPrinter.GetAvailablePageHeight : double;
begin
  try
    if FSymetricGutter
    then Result := PhysicalPageHeight - GutterTop - GutterBottom
    else Result := VertPixelsToUnit(GetDeviceCaps(FPrinter.Handle, VERTRES));
   except
     On EPrinter
     do if (UnitOfMeasurement = um_Inch)
        then Result := DefaultAvailablePageHeightIn
        else Result := DefaultAvailablePageHeightMm;
   end;
end;


function TmcmPrinter.GetAvailablePageWidth : double;
begin
  try
    if FSymetricGutter
    then Result := PhysicalPageWidth - GutterLeft - GutterRight
    else Result := HorizPixelsToUnit(GetDeviceCaps(FPrinter.Handle, HORZRES));
  except
    On EPrinter
    do if (UnitOfMeasurement = um_Inch)
       then Result := DefaultAvailablePageWidthIn
       else Result := DefaultAvailablePageWidthMm;
  end;
end;


function TmcmPrinter.GetPrinting : boolean;
begin
  if FPrintToFile
  then Result := FPrintingToFile
  else Result := FPrinting;
end;


procedure TmcmPrinter.SetPixelsPerInch;
//var FontSize : integer;
begin
(*
  if Assigned(Canvas)
  then begin
       // Fix the Delphi font bug
       FontSize := Canvas.Font.Size;
       try
         Canvas.Font.PixelsPerInch := GetDeviceCaps(FPrinter.Handle, LOGPIXELSY);
       except
         On EPrinter
         do Canvas.Font.PixelsPerInch := DefaultDPI;
       end;
       if (FontSize < 144)
       then Canvas.Font.Size := FontSize + 1
       else Canvas.Font.Size := FontSize - 1;
       Canvas.Font.Size := FontSize;
  end;
*)
end;


procedure TmcmPrinter.SetMarginTop(Value : double);
begin
  if Not(Printing)
  then begin
       if (Value >= GutterTop) or (FForceMargin and (Value >= 0))
       then begin
            if (Value <= (PhysicalPageHeight - GutterBottom))
            then FMarginTop := Value
            else FMarginTop := PhysicalPageHeight - GutterBottom;
       end
       else FMarginTop := GutterTop;
       if Assigned(FmcmPrintPreview)
       then begin
            FmcmPrintPreview.UpdatePagePreview;
            FmcmPrintPreview.Invalidate;
       end;
  end
  else raise EPagePrinter.Create('Cannot change margin while printing!');
end;


procedure TmcmPrinter.SetMarginBottom(Value : double);
begin
  if Not(Printing)
  then begin
       if (Value >= GutterBottom) or (FForceMargin and (Value >= 0))
       then begin
            if (Value <= (PhysicalPageHeight - GutterTop))
            then FMarginBottom := Value
            else FMarginBottom := PhysicalPageHeight - GutterTop;
       end
       else FMarginBottom := GutterBottom;

       if Assigned(FmcmPrintPreview)
       then begin
            FmcmPrintPreview.UpdatePagePreview;
            FmcmPrintPreview.Invalidate;
       end;
  end
  else raise EPagePrinter.Create('Cannot change margin while printing!');
end;


procedure TmcmPrinter.SetMarginLeft(Value : double);
begin
  if Not(Printing)
  then begin
       if (Value >= GutterLeft) or (FForceMargin and (Value >= 0))
       then begin
            if (Value <= (PhysicalPageWidth - GutterRight))
            then FMarginLeft := Value
            else FMarginLeft := PhysicalPageWidth - GutterRight;
       end
       else FMarginLeft := GutterLeft;
       if Assigned(FmcmPrintPreview)
       then begin
            FmcmPrintPreview.UpdatePagePreview;
            FmcmPrintPreview.Invalidate;
       end;
  end
  else raise EPagePrinter.Create('Cannot change margin while printing!');
end;


procedure TmcmPrinter.SetMarginRight(Value : double);
begin
  if Not(Printing)
  then begin
       if (Value >= GutterRight) or (FForceMargin and (Value >= 0))
       then begin
            if (Value < (PhysicalPageWidth - GutterLeft))
            then FMarginRight := Value
            else FMarginRight := PhysicalPageWidth - GutterLeft;
       end
       else FMarginRight := GutterRight;
       if Assigned(FmcmPrintPreview)
       then begin
            FmcmPrintPreview.UpdatePagePreview;
            FmcmPrintPreview.Invalidate;
       end;
  end
  else raise EPagePrinter.Create('Cannot change margin while printing!');
end;


procedure TmcmPrinter.SetUnitOfMeasurement(Value : TUnitOfMeasurement);
begin
  if (Value <> FUnitOfMeasurement)
  then begin
       FUnitOfMeasurement := Value;
       if Not(FCreating)
       then begin
            // Update the measurements if the units have changed
            if (UnitOfMeasurement = um_Inch)
            then begin
                 MarginTop        := MarginTop / 25.4;
                 MarginBottom     := MarginBottom / 25.4;
                 MarginLeft       := MarginLeft / 25.4;
                 MarginRight      := MarginRight / 25.4;
                 PageBorderOffset := PageBorderOffset / 25.4;
            end
            else begin
                 MarginTop        := MarginTop * 25.4;
                 MarginBottom     := MarginBottom * 25.4;
                 MarginLeft       := MarginLeft * 25.4;
                 MarginRight      := MarginRight * 25.4;
                 PageBorderOffset := PageBorderOffset * 25.4;
            end;
            RefreshProperties;
       end;
  end;
end;


procedure TmcmPrinter.UpdateProgressDlg(const Status : WideString; const CurrentPage, FromPage, ToPage : integer);
const DefaultProgClientHeight = 98;
begin
  if ShowProgress and (Status <> ProgressFinishMsg)
  then begin
       // Create it if is doesn't already exist
       if (FPrintProgress = Nil)
       then begin
            FPrintProgress := TmcmPrintProgress.Create(Application);

            // Set the event handle for cancel
            FPrintProgress.btnCancel.OnClick := OnCancelPrinting;

            // Hide the cancel button if set
            if Not(ShowCancel)
            then begin
                 FPrintProgress.btnCancel.Enabled := False;
                 FPrintProgress.btnCancel.Visible := False;
                 FPrintProgress.ClientHeight := (DefaultProgClientHeight * FPrintProgress.PixelsPerInch) div 96;
            end;
       end;

       // Show print progress
       FPrintProgress.Show;

       with FPrintProgress
       do begin
            Caption := Title;
            if Status = ''
            then lStatus.Caption := SendingPagesMsg
            else lStatus.Caption := Status;
            ProgressBar.Max := Copies * (ToPage - FromPage + 1);
            ProgressBar.StepIt;
            lPageNumber.Caption := 'Page ' + IntToStr(CurrentPage);
            if (FromPage = 1)
            then begin
                 lPageNumber.Caption := lPageNumber.Caption + ' of ' + IntToStr(ToPage - FromPage + 1);
                 if (Copies > 1)
                 then lPageNumber.Caption := lPageNumber.Caption + ', ' + IntToStr(Copies) + ' copies';
            end;
            if Showing
            then Update;
       end;
  end
  else begin
       if (FPrintProgress <> nil)
       then begin
            if FPrintProgress.Visible
            then FPrintProgress.Close;
            FPrintProgress.Free;
            FPrintProgress := Nil;
       end;
  end;
end;


function TmcmPrinter.GetGutterTop : double;
begin
  try
    Result := VertPixelsToUnit(GetDeviceCaps(FPrinter.Handle, PHYSICALOFFSETY));
  except
    On EPrinter
    do if (UnitOfMeasurement = um_Inch)
       then Result := DefaultGutterTopIn
       else Result := DefaultGutterTopMm;
  end;
end;


function TmcmPrinter.GetGutterBottom : double;
begin
    if FSymetricGutter
    then Result := GutterTop
    else Result := PhysicalPageHeight - AvailablePageHeight - GutterTop;
end;


function TmcmPrinter.GetGutterLeft : double;
begin
  try
    Result := HorizPixelsToUnit(GetDeviceCaps(FPrinter.Handle, PHYSICALOFFSETX));
  except
    On EPrinter
    do if (UnitOfMeasurement = um_Inch)
       then Result := DefaultGutterLeftIn
       else Result := DefaultGutterLeftMm;
  end;
end;


function TmcmPrinter.GetGutterRight : double;
begin
  if FSymetricGutter
  then Result := GutterLeft
  else Result := PhysicalPageWidth - AvailablePageWidth - GutterLeft;
end;


function TmcmPrinter.StartingLeft : integer;
begin
  Result := HorizUnitToPixels(MarginLeft - GutterLeft);
end;


function TmcmPrinter.StartingRight : integer;
begin
  Result := HorizUnitToPixels(MarginRight - GutterRight);
end;


function TmcmPrinter.StartingTop : integer;
begin
  Result := VertUnitToPixels(MarginTop - GutterTop);
end;


function TmcmPrinter.StartingBottom : integer;
begin
  Result := VertUnitToPixels(MarginBottom - GutterBottom);
end;


function TmcmPrinter.HorizUnitToPixels(const Value : double) : integer;
var Pixels : double;
begin
  case FUnitOfMeasurement of
  um_Millimeter : Pixels := Value / 25.4;
  else            Pixels := Value;
  end;
  try
    Result := Round(Pixels * GetDeviceCaps(FPrinter.Handle, LOGPIXELSX));
  except
    Result := Round(Pixels * DefaultDPI);
  end;
end;


function TmcmPrinter.VertUnitToPixels(const Value : double) : integer;
var Pixels : double;
begin
  case FUnitOfMeasurement of
  um_Millimeter : Pixels := Value / 25.4;
  else            Pixels := Value;
  end;
  try
    Result := Round(Pixels * GetDeviceCaps(FPrinter.Handle, LOGPIXELSY));
  except
    Result := Round(Pixels * DefaultDPI);
  end;
end;


function TmcmPrinter.HorizPixelsToUnit(const Value : integer) : double;
begin
  try
    Result := Value / GetDeviceCaps(FPrinter.Handle, LOGPIXELSX);
  except
    Result := Value / DefaultDPI;
  end;
  case FUnitOfMeasurement of
  um_Millimeter : Result := Result * 25.4;
  end;
end;


function TmcmPrinter.VertPixelsToUnit(const Value : integer) : double;
begin
  try
    Result := Value / GetDeviceCaps(FPrinter.Handle, LOGPIXELSY);
  except
    Result := Value / DefaultDPI;
  end;
  case FUnitOfMeasurement of
  um_Millimeter : Result := Result * 25.4;
  end;
end;


procedure TmcmPrinter.SetPageBorderOffset(Value : double);
begin
  FPageBorderOffset := Value;
end;


function TmcmPrinter.GetPhysicalPageHeight : double;
begin
  try
    Result := VertPixelsToUnit(GetDeviceCaps(FPrinter.Handle, PHYSICALHEIGHT));
  except
    On EPrinter
    do if (UnitOfMeasurement = um_Inch)
       then Result := DefaultPhysicalPageHeightIn
       else Result := DefaultPhysicalPageHeightMm;
  end;
end;


function TmcmPrinter.GetPhysicalPageWidth : double;
begin
  try
    Result := HorizPixelsToUnit(GetDeviceCaps(FPrinter.Handle, PHYSICALWIDTH));
  except
    On EPrinter
    do if (UnitOfMeasurement = um_Inch)
       then Result := DefaultPhysicalPageWidthIn
       else Result := DefaultPhysicalPageWidthMm;
  end;
end;


function TmcmPrinter.GetPrintableWidth : double;
begin
  Result := PhysicalPageWidth - MarginLeft - MarginRight;
end;


function TmcmPrinter.GetPrintableHeight : double;
begin
  Result := PhysicalPageHeight - MarginTop - MarginBottom;
end;

{
function TmcmPrinter.GetPrintToFile : boolean;
begin
  Result := FPrintToFile;
end;
}

{
procedure TmcmPrinter.SetPrintToFile(Value : boolean);
begin
  if (Value <> FPrintToFile)
  then begin
       if Not(Printing)
       then FPrintToFile := Value
       else raise EPagePrinter.Create('Cannot change print mode while printing!');
  end;
end;
}

function TmcmPrinter.GetFileName : WideString;
begin
  Result := FFileName;
end;


procedure TmcmPrinter.SetFileName(Value : WideString);
begin
  if (Value <> FFileName)
  then begin
       if Not(Printing)
       then FFileName := Trim(Value)
       else raise EPagePrinter.Create('Cannot change file name while printing!');
  end;
end;


function TmcmPrinter.GetPageCount : integer;
begin
  if Assigned(FPages)
  then Result := FPages.Count
  else Result := 0;
end;


procedure TmcmPrinter.ResetPageList(CreateForReal : boolean);
begin
  // Destroy old list and all pages
  FPages.Free;
  FPages := Nil;

  // Create new empty list
  FPages  := TmcmPrintPageList.Create;
  FPageNo := 0;

  if Assigned(FmcmPrintPreview)
  then begin
       FmcmPrintPreview.PageIndex := 1;
       FmcmPrintPreview.Invalidate;
  end;
end;


function TmcmPrinter.GetPrinterHandle : HDC;
begin
  try
    Result := FPrinter.Handle;
  except
    On EPrinter
    do Result := 0;
  end;
end;


procedure TmcmPrinter.SetCopies(Value : integer);
begin
  if (Value > 0)
  then FCopies := Value
  else FCopies := 1;
end;


function TmcmPrinter.GetCopies : integer;
begin
  GetCopies := FCopies;
end;


procedure TmcmPrinter.SetCollate(Value : boolean);
begin
  FCollate := Value;
end;


function TmcmPrinter.GetPrintToPage : integer;
begin
  Result := FPrintToPage;
end;


procedure TmcmPrinter.SetPrintToPage(Value : integer);
begin
  if (Value <> FPrintToPage)
  then begin
       FPrintToPage := Value;
       if (FPrintToPage < FPrintFromPage)
       then PrintFromPage := FPrintToPage;
  end;
end;


function TmcmPrinter.GetPrintFromPage : integer;
begin
  Result := FPrintFromPage;
end;


procedure TmcmPrinter.SetPrintFromPage(Value : integer);
begin
  if (Value <> FPrintFromPage)
  then begin
       FPrintFromPage := Value;
       if (FPrintFromPage > FPrintToPage)
       then PrintToPage := FPrintFromPage;
  end;
end;


function TmcmPrinter.Print : boolean;
var Margins : TRect;
    Gutter  : TRect;


    procedure PrintPage(Pg, FromPage, ToPage : integer; LastPage : boolean);
    var Oldxy  : TPoint;
        x, y   : integer;
        HScale : double;
        VScale : double;
        Scale  : double;
        BorderOffset : integer;
    begin
      // Cancel printing if necessary
      if ShowProgress and ShowCancel
      then Application.ProcessMessages;
      if FCancelPrint
      then raise ECancelPrinting.Create('Printing Cancelled');

      // print page
      UpdateProgressDlg(ProgressText, Pg, FromPage, ToPage);

      Oldxy := Pages[Pg-1].GetOrigo;
      Pages[Pg-1].SetOrigo(Margins.TopLeft);

      Scale := FImageScale / 100.0;
      if FImageFitToPage
      then begin
           HScale := 1.0;
           VScale := 1.0;
           if (Pages[Pg-1].DispWidth > 0.0)
           then HScale := PixelPrintWidth / Pages[Pg-1].DispWidth;
           if (Pages[Pg-1].DispHeight > 0.0)
           then VScale := PixelPrintHeight / Pages[Pg-1].DispHeight;
           if (HScale < VScale)
           then Scale := HScale
           else Scale := VScale;
      end;

      if FImageCenter
      then begin
           x := Round((PixelPrintWidth - Scale * Pages[Pg-1].DispWidth) / 2) + Margins.Left;
           y := Round((PixelPrintHeight - Scale * Pages[Pg-1].DispHeight) / 2) + Margins.Top;
           Pages[Pg-1].SetOrigo(Point(x, y));
      end;

      Pages[Pg-1].Draw(FPrinter.Canvas.Handle, Scale);
      Pages[Pg-1].SetOrigo(Oldxy);

      FPrinter.Canvas.Pen.Color   := RGB(255, 255, 255);
      FPrinter.Canvas.Pen.Style   := PSSOLID;
      FPrinter.Canvas.Brush.Color := RGB(255, 255, 255);
      FPrinter.Canvas.Brush.Style := BSSOLID;

      // Ensure left margin.
      FPrinter.Canvas.Rectangle(Gutter.Left, Gutter.Top, Margins.Left, Gutter.Bottom);

      // Ensure right margin.
      FPrinter.Canvas.Rectangle(Margins.Right, Gutter.Top, Gutter.Right, Gutter.Bottom);


      // Ensure Top margin.
      FPrinter.Canvas.Rectangle(Gutter.Left, Gutter.Top, Gutter.Right, Margins.Top);

      // Ensure bottom margin.
      FPrinter.Canvas.Rectangle(Gutter.Left, Margins.Bottom, Gutter.Right, Gutter.Bottom);


      // Print PageBorders.
      with FPrinter.Canvas
      do begin
         Pen.Color := FPageBorderColor;
         Pen.Width := FPageBorderWidth * (GetDeviceCaps(FPrinter.Handle, LOGPIXELSY) div DefaultDPI);
         BorderOffset := VertUnitToPixels(PageBorderOffset);

         if (pb_Top in PageBorders)
         then begin
              MoveTo(StartingLeft - BorderOffset, StartingTop - BorderOffset);
              LineTo(StartingLeft + PixelPrintWidth + BorderOffset, StartingTop - BorderOffset);
         end;

         if (pb_Bottom in PageBorders)
         then begin
              MoveTo(StartingLeft - BorderOffset, StartingTop + PixelPrintHeight + BorderOffset);
              LineTo(StartingLeft + PixelPrintWidth + BorderOffset, StartingTop + PixelPrintHeight + BorderOffset);
         end;

         Pen.Width := FPageBorderWidth * (GetDeviceCaps(FPrinter.Handle, LOGPIXELSX) div DefaultDPI);
         BorderOffset := HorizUnitToPixels(PageBorderOffset);

         if (pb_Left in PageBorders)
         then begin
              MoveTo(StartingLeft - BorderOffset, StartingTop - BorderOffset);
              LineTo(StartingLeft - BorderOffset, StartingTop + PixelPrintHeight + BorderOffset);
         end;

         if (pb_Right in PageBorders)
         then begin
              MoveTo(StartingLeft + PixelPrintWidth + BorderOffset, StartingTop - BorderOffset);
              LineTo(StartingLeft + PixelPrintWidth + BorderOffset, StartingTop + PixelPrintHeight + BorderOffset);
         end;
      end;

      if Not(LastPage)
      then FPrinter.NewPage;
    end;

var Cp, Pg        : integer;
    PrevPrnCopies : integer;
    ToPage        : integer;
    FromPage      : integer;
begin
  Result := True; // Return true if print completed.

  if Printing
  then EndDoc;

  if (PageCount > 0)
  then begin
       // Determine which pages to print
       if PrintFromPage = 0
       then FromPage := 1
       else FromPage := Minimum(PrintFromPage, PageCount);

       if (PrintToPage = 0)
       then ToPage := PageCount
       else ToPage := Minimum(PrintToPage, PageCount);

       // Copies are handled here, not by the printer
       PrevPrnCopies := FPrinter.Copies;
       FPrinter.Copies := 1;

       // Set up cancel mode
       FCancelPrint := False;
       try
         // Print the pages on the printer using Collate and Copies
         try
           FPrinter.BeginDoc;

           Gutter.Left    := 0;
           Gutter.Top     := 0;
           Gutter.Right   := HorizUnitToPixels(AvailablePageWidth);
           Gutter.Bottom  := VertUnitToPixels(AvailablePageHeight);

           Margins.Left   := StartingLeft;
           Margins.Top    := StartingTop;
           Margins.Right  := PixelPrintWidth + Margins.Left;
           Margins.Bottom := PixelPrintHeight + Margins.Top;

           if Collate // Print pages 1,2,3.. 1,2,3..
           then begin
                for Cp := 1 to Copies
                do for Pg := FromPage to ToPage
                   do PrintPage(Pg, FromPage, ToPage, (Pg = ToPage) and (Cp = Copies));
           end
           else begin // Print pages 1,1.. 2,2.. 3,3..
                for Pg := FromPage to ToPage
                do for Cp := 1 to Copies
                   do PrintPage(Pg, FromPage, ToPage, (Pg = ToPage) and (Cp = Copies));
           end;
           FPrinter.EndDoc;
         finally
           UpdateProgressDlg(ProgressFinishMsg, 0, 0, 0);
           if Not(FPrinter.Printing)
           then FPrinter.Copies := PrevPrnCopies;
         end;
       except
         On ECancelPrinting
         do begin
            Result := False; // Return False due to cancel
            if FPrinter.Printing
            then begin
                 // Calling Abort multiple times may crash programs.
                 // Thus, optionally use EndDoc.
                 if AbortOnCancel
                 then FPrinter.Abort
                 else FPrinter.EndDoc;
            end;
         end;
       end;
  end;
end;


procedure TmcmPrinter.BeginUpdate;
begin
  inc(FUpdateRefCount);
end;


procedure TmcmPrinter.EndUpdate;
begin
  if (FUpdateRefCount > 0)
  then dec(FUpdateRefCount);
  if (FUpdateRefCount = 0)
  then if Assigned(FmcmPrintPreview)
       then FmcmPrintPreview.Invalidate;
end;


function TmcmPrinter.GetPages(Index : word) : TmcmPrintPage;
begin
  if (Index < FPages.Count)
  then Result := FPages.GetPage(Index)
  else Result := Nil;
end; // TmcmPrinter.GetPages.


function TmcmPrinter.GetProgressMessage : WideString;
begin
  Result := FProgressMessage;
end;


procedure TmcmPrinter.SetProgressMessage(Value : WideString);
begin
  FProgressMessage := Value;
end;


function TmcmPrinter.GetCollate : boolean;
begin
  Result := FCollate;
end;


function TmcmPrinter.GetMarginBottom : double;
begin
  Result := FMarginBottom;
end;


function TmcmPrinter.GetMarginLeft : double;
begin
  Result := FMarginLeft;
end;


function TmcmPrinter.GetMarginRight : double;
begin
  Result := FMarginRight;
end;


function TmcmPrinter.GetMarginTop : double;
begin
  Result := FMarginTop;
end;


function TmcmPrinter.GetUnitOfMeasurement : TUnitOfMeasurement;
begin
  Result := FUnitOfMeasurement;
end;


function TmcmPrinter.GetPageBorderOffset : double;
begin
  Result := FPageBorderOffset;
end;


function TmcmPrinter.GetShowProgress : boolean;
begin
  Result := FShowProgress;
end;


function TmcmPrinter.GetPageBorders : TPageBorders;
begin
  Result := FPageBorders;
end;


procedure TmcmPrinter.SetPageBorders(Value : TPageBorders);
begin
  FPageBorders := Value;
  if Assigned(FmcmPrintPreview)
  then begin
       FmcmPrintPreview.UpdatePagePreview;
       FmcmPrintPreview.Invalidate;
  end;
end;


procedure TmcmPrinter.OnCancelPrinting(Sender : TObject);
begin
  FCancelPrint := True;
end;


procedure TmcmPrinter.SetShowProgress(Value : boolean);
begin
  FShowProgress := Value;
end;


function TmcmPrinter.GetShowCancel : boolean;
begin
  Result := FShowCancel;
end;


procedure TmcmPrinter.SetShowCancel(Value : boolean);
begin
  FShowCancel := Value;
end;


function TmcmPrinter.GetImageCenter : boolean;
begin
  Result := FImageCenter;
end;


procedure TmcmPrinter.SetImageCenter(Value : boolean);
begin
  FImageCenter := Value;
  if Assigned(mcmPrintPreview)
  then mcmPrintPreview.ImageCenter := FImageCenter;
end;


function TmcmPrinter.GetImageFitToPage : boolean;
begin
  Result := FImageFitToPage;
end;


procedure TmcmPrinter.SetImageFitToPage(Value : boolean);
begin
  FImageFitToPage := Value;
  if FImageFitToPage
  then begin
       
  end;
  if Assigned(mcmPrintPreview)
  then begin
       mcmPrintPreview.ImageFitToPage := FImageFitToPage;
  end;
end;


function TmcmPrinter.GetImageScale : word;
begin
  Result := FImageScale;
end;


procedure TmcmPrinter.SetImageScale(Value : word);
begin
  FImageScale := Value;
  if Assigned(mcmPrintPreview)
  then begin
       mcmPrintPreview.UpdatePagePreview;
       FmcmPrintPreview.Invalidate;
  end;
end;


//------------------------------------------------------------------------------
// TmcmPrintPreview
//------------------------------------------------------------------------------

constructor TmcmPrintPreview.Create(AOwner : TComponent);
begin
  Inherited Create(AOwner);
  FCreating := True;

  FResizeEnabled := True;

  // TmcmPrinter does not accept controls, ie. it's not at container.
  ControlStyle := ControlStyle - [csAcceptsControls];
  ControlStyle := ControlStyle + [csOpaque];

  mcmPrinter := Nil;
  FPrinter   := {$IFDEF GE_DXE2}Vcl.{$ENDIF}Printers.Printer;

  FShowPage  := 0;
  Width      := 89;
  Height     := 115;

  // Setup scrollbars
  HorzScrollBar.Tracking  := True;
  HorzScrollBar.Increment := 16;
  VertScrollBar.Tracking  := True;
  VertScrollBar.Increment := 16;

  // Drawing surface
  FPaintBox         := TPaintBox.Create(Self);
  FPaintBox.Parent  := Self;
  FPaintBox.Align   := alClient;
  FPaintBox.OnPaint := PaintPreview;

  FPrintPage        := TmcmImageCtrl.Create(Self);
  FPrintPage.ControlStyle := FPrintPage.ControlStyle + [csOpaque];
  FPrintPage.Width  := 0;
  FPrintPage.Height := 0;
  FPrintPage.Parent := Self;

  FZoomMode         := zm_FitToPage;
  FZoomPercent      := 25;
  FZoomPos          := zp_TopLeft;

  FShadowColor      := clBtnShadow;
  FShadowOffset     := 5;
  FShowMargins      := True;

  FCreating         := False;
end;


destructor TmcmPrintPreview.Destroy;
begin
  if Assigned(FPaintBox)
  then FPaintBox.Free;
  if Assigned(FPrintPage)
  then FPrintPage.Free;
  inherited Destroy;
end;


procedure TmcmPrintPreview.SetmcmPrinter(Value : TmcmPrinter);
begin
  if Assigned(Value)
  then begin
       FmcmPrinter := Value;
       FmcmPrinter.mcmPrintPreview := Self;
  end
  else begin
       if Assigned(FmcmPrinter)
       then FmcmPrinter.mcmPrintPreview := Nil;
       FmcmPrinter := Nil;
  end;
end;


procedure TmcmPrintPreview.OnPageChanged(Sender : TObject);
begin
  FmcmPrinter.RefreshProperties;
end; // TmcmPrintPreview.OnPageChanged.


procedure TmcmPrintPreview.PaintPreview(Sender : TObject);
var PagePixelsWidth  : integer;
    PagePixelsHeight : integer;
    XOffset          : integer;
    YOffset          : integer;
    R, MarginRect    : TRect;
    BorderOffset     : integer;
begin
  with FPaintBox.Canvas
  do begin
     PagePixelsHeight := GetPreviewPageVert;
     PagePixelsWidth  := GetPreviewPageHoriz;

     // Calculate page area
     XOffset := (FPaintBox.Width - PagePixelsWidth) div 2;
     YOffset := (FPaintBox.Height - PagePixelsHeight) div 2;

     R.Left     := XOffset;
     R.Top      := YOffset;
     R.Right    := PagePixelsWidth + XOffset;
     R.Bottom   := PagePixelsHeight + YOffset;
     MarginRect := R;

     Pen.Width  := 1;
     Pen.Style  := PSSOLID;
     if (ShadowOffset > 0)
     then begin
          // Draw shadow
          Brush.Color := ShadowColor;
          Pen.Color   := ShadowColor;

          // Right section
          Rectangle(R.Left + PagePixelsWidth,
                    R.Top + ShadowOffset,
                    R.Right + ShadowOffset,
                    R.Bottom + ShadowOffset);

          // Bottom section
          Rectangle(R.Left + ShadowOffset,
                    R.Top + PagePixelsHeight,
                    R.Right,
                    R.Bottom + ShadowOffset);
     end;

     // Draw page
     Brush.Color := clWhite;
     Pen.Color   := clBlack;
     Rectangle(R.Left, R.Top, R.Right, R.Bottom);

     if Assigned(FmcmPrinter)
     then begin
          inc(MarginRect.Left, ScreenScaleValue(FmcmPrinter.MarginLeft, pd_Horiz));
          inc(MarginRect.Top, ScreenScaleValue(FmcmPrinter.MarginTop, pd_Vert));
          dec(MarginRect.Right, ScreenScaleValue(FmcmPrinter.MarginRight, pd_Horiz));
          dec(MarginRect.Bottom, ScreenScaleValue(FmcmPrinter.MarginBottom, pd_Vert));

          // Draw margins
          if ShowMargins
          then begin
               Pen.Width   := 1;
               Pen.Style   := PSDOT;
               Pen.Color   := clSilver;
               Brush.Style := BSCLEAR;
               Rectangle(MarginRect.Left - 1, MarginRect.Top - 1, MarginRect.Right + 1, MarginRect.Bottom + 1);
          end;

          with FmcmPrinter
          do begin
             // Horizontal lines
             Pen.Width := FPageBorderWidth * (GetDeviceCaps(FPrinter.Handle, LOGPIXELSY) div DefaultDPI);
             Pen.Color := FPageBorderColor;
             Pen.Style := PSSOLID;
             BorderOffset := VertUnitToPixels(PageBorderOffset);

             if (pb_Top in PageBorders)
             then begin
                  MoveTo(MarginRect.Left - ScreenScaleValue(BorderOffset, pd_Horiz),
                         MarginRect.Top - ScreenScaleValue(BorderOffset, pd_Vert));
                  LineTo(MarginRect.Right + ScreenScaleValue(BorderOffset, pd_Horiz),
                         MarginRect.Top - ScreenScaleValue(BorderOffset, pd_Vert));
             end;

             if (pb_Bottom in PageBorders)
             then begin
                  MoveTo(MarginRect.Left - ScreenScaleValue(BorderOffset, pd_Horiz),
                         MarginRect.Bottom + ScreenScaleValue(BorderOffset, pd_Vert));
                  LineTo(MarginRect.Right + ScreenScaleValue(BorderOffset, pd_Horiz),
                         MarginRect.Bottom + ScreenScaleValue(BorderOffset, pd_Vert));
             end;

             // Vertical lines
             Pen.Width := FPageBorderWidth * (GetDeviceCaps(FPrinter.Handle, LOGPIXELSX) div DefaultDPI);
             BorderOffset := HorizUnitToPixels(PageBorderOffset);

             if (pb_Left in PageBorders)
             then begin
                  MoveTo(MarginRect.Left - ScreenScaleValue(BorderOffset, pd_Horiz),
                         MarginRect.Top - ScreenScaleValue(BorderOffset, pd_Vert));
                  LineTo(MarginRect.Left - ScreenScaleValue(BorderOffset, pd_Horiz),
                         MarginRect.Bottom + ScreenScaleValue(BorderOffset, pd_Vert));
             end;

             if (pb_Right in PageBorders)
             then begin
                  MoveTo(MarginRect.Right + ScreenScaleValue(BorderOffset, pd_Horiz),
                         MarginRect.Top - ScreenScaleValue(BorderOffset, pd_Vert));
                  LineTo(MarginRect.Right + ScreenScaleValue(BorderOffset, pd_Horiz),
                         MarginRect.Bottom + ScreenScaleValue(BorderOffset, pd_Vert));
             end;
          end;

     end;
     Pen.Color := Color;
  end;
end;


procedure TmcmPrintPreview.Resize;
begin
  Inherited Resize;

  // Preview page has resized.
  if FResizeEnabled
  then begin
       UpdatePagePreview;
       case FZoomMode of
       zm_FitToPage    : ZoomToFit;
       zm_FitToWidth   : ZoomToWidth;
       zm_FitToHeight  : ZoomToHeight;
       zm_FitToPercent : begin
                           InvalidateRect(Handle, Nil, True);
                         end;
       end;
  end;
end; // TmcmPrintPreview.Resize.


procedure TmcmPrintPreview.Resizing(State : TWindowState);
begin
  // Preview page has resized.
  if FResizeEnabled
  then begin
       UpdatePagePreview;
       case FZoomMode of
       zm_FitToPage    : ZoomToFit;
       zm_FitToWidth   : ZoomToWidth;
       zm_FitToHeight  : ZoomToHeight;
       zm_FitToPercent : begin
                           InvalidateRect(Handle, Nil, True);
                         end;
       end;
  end;
end;


procedure TmcmPrintPreview.CMFontChanged(var Msg : TMessage);
begin
  inherited;
  {
  if Assigned(Canvas)
  then Canvas.Font.Assign(Font);
  SetPixelsPerInch;
  }
  if Assigned(FmcmPrinter)
  then GetTextMetrics(FmcmPrinter.GetPrinterHandle, FTextMetrics);
end;


procedure TmcmPrintPreview.Invalidate;
begin
  {if (FUpdateRefCount = 0)
  then} InvalidateRect(Handle, Nil, True);
end;


function TmcmPrintPreview.GetShadowColor : TColor;
begin
  Result := FShadowColor;
end;


procedure TmcmPrintPreview.SetShadowColor(Value : TColor);
begin
  if (Value <> FShadowColor)
  then begin
       FShadowColor := Value;
       Invalidate;
  end;
end;


function TmcmPrintPreview.GetShadowOffset : integer;
begin
  Result := FShadowOffset;
end;


procedure TmcmPrintPreview.SetShadowOffset(Value : integer);
begin
  if (Value <> FShadowOffset)
  then begin
       FShadowOffset := Value;
       UpdatePagePreview;
       Invalidate;
  end;
end;


function TmcmPrintPreview.GetShowMargins : boolean;
begin
  Result := FShowMargins;
end;


procedure TmcmPrintPreview.SetShowMargins(Value : boolean);
begin
  if (Value <> FShowMargins)
  then begin
       FShowMargins := Value;
       Invalidate;
  end;
end;


function TmcmPrintPreview.GetPreviewPageHoriz : integer;
var DefaultVal : double;
begin
  try
    Result := GetDeviceCaps(FPrinter.Handle, PHYSICALWIDTH);
  except
    On EPrinter
    do begin
       if Assigned(FmcmPrinter)
       then begin
            if (FmcmPrinter.UnitOfMeasurement = um_Inch)
            then DefaultVal := DefaultPhysicalPageWidthIn
            else DefaultVal := DefaultPhysicalPageWidthMm;
       end
       else DefaultVal := DefaultPhysicalPageWidthIn;
       Result := UnitsToScreen(DefaultVal, pd_Horiz);
    end;
  end;
  Result := Round((Result * ZoomPercent / GetScaleFactor(pd_Horiz)) / 100);
end;


function TmcmPrintPreview.GetPreviewPageVert : integer;
var DefaultVal : double;
begin
  try
    Result := GetDeviceCaps(FPrinter.Handle, PHYSICALHEIGHT);
  except
    On EPrinter
    do begin
       if Assigned(FmcmPrinter)
       then begin
            if (FmcmPrinter.UnitOfMeasurement = um_Inch)
            then DefaultVal := DefaultPhysicalPageHeightIn
            else DefaultVal := DefaultPhysicalPageHeightMm;
       end
       else DefaultVal := DefaultPhysicalPageHeightIn;
       Result := UnitsToScreen(DefaultVal, pd_Vert);
    end;
  end;
  Result := Round((Result * ZoomPercent / GetScaleFactor(pd_Vert)) / 100);
end;


function TmcmPrintPreview.GetScaleFactor(Dir : TPageDir) : Double;
var Index : integer;
begin
  Result := 1;
  if Not(FCreating)
  then begin
       if (Dir = pd_Horiz)
       then Index := LOGPIXELSX
       else Index := LOGPIXELSY;
       try
         Result := GetDeviceCaps(FPrinter.Handle, Index) / GetDeviceCaps(FPaintBox.Canvas.Handle, Index);
       except
         On EPrinter
         do Result := 1;
       end;
  end;
end;


function TmcmPrintPreview.ScreenScaleValue(Value : double; Dir : TPageDir) : integer;
begin
  Result := UnitsToScreen(Value, Dir);
  Result := Round((Result * ZoomPercent) / 100.0);
end;


function TmcmPrintPreview.UnitsToScreen(const Value : double; Dir : TPageDir) : integer;
var TmpVal : double;
    Index  : integer;
begin
  TmpVal := Value;
  if Assigned(FmcmPrinter)
  then begin
       if FmcmPrinter.UnitOfMeasurement = um_Millimeter
       then TmpVal := Value / 25.4;
  end;
  if (Dir = pd_Horiz)
  then Index := LOGPIXELSX
  else Index := LOGPIXELSY;
  if Not(FCreating)
  then Result := Round(TmpVal * GetDeviceCaps(FPaintBox.Canvas.Handle, Index))
  else Result := Round(TmpVal * Screen.PixelsPerInch);
end;


function TmcmPrintPreview.GetZoomPercent : integer;
begin
  Result := FZoomPercent;
end;


procedure TmcmPrintPreview.SetZoomPercent(Value : integer);
begin
  if {(FZoomPercent <> Value) and} (Value > 0)
  then begin
       FResizeEnabled := False;
       HorzScrollBar.Range := 0;
       VertScrollBar.Range := 0;
       FZoomPercent := Value;
       UpdatePagePreview;
       FZoomMode := zm_FitToPercent;
       InvalidateRect(Handle, Nil, True);
       FResizeEnabled := True;
  end;
end;


procedure TmcmPrintPreview.ZoomToFit;
var PagePixelsWidth  : integer;
    PagePixelsHeight : integer;
begin
  FResizeEnabled := False;
  HorzScrollBar.Range := 0;
  VertScrollBar.Range := 0;
  PagePixelsHeight := GetPreviewPageVert;
  PagePixelsWidth  := GetPreviewPageHoriz;
  if (PagePixelsWidth / PagePixelsHeight) > (FPaintBox.ClientWidth / FPaintBox.ClientHeight)
  then ZoomToWidth
  else ZoomToHeight;
  FZoomMode := zm_FitToPage;
  FResizeEnabled := True;
end;


procedure TmcmPrintPreview.ZoomToWidth;
var ScrollWidth : integer;
begin
  FResizeEnabled := False;
  HorzScrollBar.Range := 0;
  VertScrollBar.Range := 0;
  ScrollWidth := GetSystemMetrics(SM_CXVSCROLL);
  if Assigned(FmcmPrinter)
  then begin
       with FmcmPrinter
       do ZoomPercent := Trunc(((100.0 * (FPaintBox.ClientWidth - ScrollWidth -
                               2 * (ShadowOffset + 1))) *
                               GetScaleFactor(pd_Horiz)) / HorizUnitToPixels(PhysicalPageWidth));
  end
  else ZoomPercent := 25;
  FZoomMode := zm_FitToWidth;
  FResizeEnabled := True;
end;


procedure TmcmPrintPreview.ZoomToHeight;
var ScrollHeight : integer;
begin
  FResizeEnabled := False;
  HorzScrollBar.Range := 0;
  VertScrollBar.Range := 0;
  ScrollHeight := GetSystemMetrics(SM_CYHSCROLL);
  if Assigned(FmcmPrinter)
  then begin
       with FmcmPrinter
       do ZoomPercent  := Trunc(((100.0 * (FPaintBox.ClientHeight - ScrollHeight -
                                2 * (ShadowOffset + 1))) *
                                GetScaleFactor(pd_Vert)) / VertUnitToPixels(PhysicalPageHeight));
  end
  else ZoomPercent := 25;
  FZoomMode    := zm_FitToHeight;
  FResizeEnabled := True;
end;


procedure TmcmPrintPreview.Loaded;
begin
  inherited Loaded;
  if Assigned(FmcmPrinter)
  then begin
       FmcmPrinter.RefreshProperties;
  end;
  UpdatePagePreview;
  FPaintBox.Canvas.Pen.Color := Color;
  Invalidate;
end;


procedure TmcmPrintPreview.UpdateDesigner;
begin
  if (csDesigning in ComponentState)
  then if (GetParentForm(Self) <> nil)
       then if (GetParentForm(Self).Designer <> nil)
            then GetParentForm(Self).Designer.Modified;
end;


procedure TmcmPrintPreview.UpdatePagePreview;
var PagePixelsWidth  : integer;
    PagePixelsHeight : integer;
    XOffset          : integer;
    YOffset          : integer;
    R                : TRect;
    MarginRect       : TRect;
begin
  // Set-up scrolling region
  HorzScrollBar.Range := GetPreviewPageHoriz + 2 * (ShadowOffset + 1);
  VertScrollBar.Range := GetPreviewPageVert + 2 * (ShadowOffset + 1);
  if not(FCreating)
  then begin
       case ZoomPos of
       zp_TopLeft   : begin
                        HorzScrollBar.Position := 1;
                        HorzScrollBar.Position := 0;
                        VertScrollBar.Position := 1;
                        VertScrollBar.Position := 0;
                      end;
       zp_TopCenter : begin // Center page horizontally and go to top
                        HorzScrollBar.Position := (HorzScrollBar.Range - ClientWidth) div 2;
                        VertScrollBar.Position := 1;
                        VertScrollBar.Position := 0;
                      end;
       zp_Center    : begin
                        HorzScrollBar.Position := (HorzScrollBar.Range - ClientWidth) div 2;
                        VertScrollBar.Position := (VertScrollBar.Range - ClientHeight) div 2;
                      end;
       end;
  end;

  if Assigned(FmcmPrinter)
  then begin
       PagePixelsHeight := GetPreviewPageVert;
       PagePixelsWidth  := GetPreviewPageHoriz;

       // Calculate page area
       XOffset := (FPaintBox.Width - PagePixelsWidth) div 2;
       YOffset := (FPaintBox.Height - PagePixelsHeight) div 2;

       R.Left     := XOffset;
       R.Top      := YOffset;
       R.Right    := PagePixelsWidth + XOffset;
       R.Bottom   := PagePixelsHeight + YOffset;
       MarginRect := R;

       inc(MarginRect.Left, ScreenScaleValue(FmcmPrinter.MarginLeft, pd_Horiz));
       inc(MarginRect.Top, ScreenScaleValue(FmcmPrinter.MarginTop, pd_Vert));
       dec(MarginRect.Right, ScreenScaleValue(FmcmPrinter.MarginRight, pd_Horiz));
       dec(MarginRect.Bottom, ScreenScaleValue(FmcmPrinter.MarginBottom, pd_Vert));

       if (FPrintPage.Left <> MarginRect.Left)
       then FPrintPage.Left := MarginRect.Left;
       if (FPrintPage.Top <> MarginRect.Top)
       then FPrintPage.Top := MarginRect.Top;
       if (FPrintPage.Width <> MarginRect.Right - MarginRect.Left)
       then FPrintPage.Width := MarginRect.Right - MarginRect.Left;
       if (FPrintPage.Height <> MarginRect.Bottom - MarginRect.Top)
       then FPrintPage.Height := MarginRect.Bottom - MarginRect.Top;

       if Not(FPrintPage.ScaleToFit)
       then FPrintPage.Scale := (ZoomPercent / 100.0) * (FmcmPrinter.ImageScale / 100.0) / GetScaleFactor(pd_Horiz);
  end;
end;


function TmcmPrintPreview.GetShowPage : integer;
begin
  if Assigned(FmcmPrinter)
  then Result := FShowPage + 1
  else Result := -1;
end;


procedure TmcmPrintPreview.SetShowPage(Value : integer);
begin
  if Assigned(FmcmPrinter)
  then begin
       if (0 < Value) and (Value <= FmcmPrinter.PageCount)
       then begin
            FShowPage := Value - 1;
            if Assigned(FmcmPrinter.Pages[FShowPage])
            then begin
                 FPrintPage.Visible := False;
                 FPrintPage.OnChange := OnPageChanged;
                 FPrintPage.Image.DibHandle := FmcmPrinter.Pages[FShowPage].DibHandle;
                 FPrintPage.Image.ReleaseHandle;
                 FPrintPage.Image.Transparent := FmcmPrinter.Pages[FShowPage].Transparent;
                 FPrintPage.Image.TransparentColor := FmcmPrinter.Pages[FShowPage].TransparentColor;
                 if Assigned(FmcmPrinter.Pages[FShowPage].ImageInfo)
                 then FPrintPage.Image.ImageInfo.Assign(FmcmPrinter.Pages[FShowPage].ImageInfo);
                 FPrintPage.Color := RGB(255, 255, 255);
                 FPrintPage.Visible := True;
            end
            else FPrintPage.Image.DibHandle := 0;
       end;
       if (Value < 0)
       then FPrintPage.Image.DibHandle := 0;
  end;
end;


function TmcmPrintPreview.GetImageCenter : boolean;
begin
  Result := FPrintPage.Center;
end;


procedure TmcmPrintPreview.SetImageCenter(Value : boolean);
begin
  FPrintPage.Center := Value;
  UpdatePagePreview;
end;


function TmcmPrintPreview.GetImageFitToPage : boolean;
begin
  Result := FPrintPage.ScaleToFit;
end;


procedure TmcmPrintPreview.SetImageFitToPage(Value : boolean);
begin
  FPrintPage.ScaleToFit := Value;
  UpdatePagePreview;
end;

{$IFDEF LONGSTRINGSON} {$H-} {$UNDEF LONGSTRINGSON} {$ENDIF}

initialization

finalization

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.

