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
// $Log:  15906: uDSUserInterface.pas 
//
//    Rev 1.8    2014-01-15 13:42:02  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.7    2013-12-04 23:16:16  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.6    25-10-2009 16:44:30  mcm    Version: DT 3.10
// Support for Delphi 2010
//
//    Rev 1.5    26-08-2009 22:39:04  mcm    Version: DT 3.9
// Fixed unicode issues (PChar -> PAnsiChar)
//
//   Rev 1.4    04-11-2003 23:44:42  mcm    Version: DT3.0
// Currected reference between WM_DOEXITCLICK and DoSendExit method.

//
//   Rev 1.3    14-06-2003 10:54:16  mcm    Version: DT 2.4
// Added the WM_DOEXITCLICK message constant called from mcmTWAINLayer.EnableDS
// when the user interface is hidden.

//
//   Rev 1.2    16-05-2003 20:56:46  mcm    Version: DT 2.4
// Added support for shortcut's, ref. FormKeyDown.

//
//   Rev 1.1    06-03-2003 11:39:40  mcm    Version: DT 2.2
// Added conditional define to disable warnings on "Unsafe Type, Cast and Code"
// for Delphi 7.

//
//   Rev 1.0    04-12-2001 16:49:10  mcm    Version: DT 2.0

unit uDSUserInterface;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFDEF GE_DXE2}
     WinApi.Windows, System.SysUtils, WinApi.Messages, System.Classes,
     Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Printers,
     Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ToolWin, Vcl.ImgList, Vcl.Imaging.jpeg,
     {$ELSE}
     Windows, SysUtils, Messages, Classes, Graphics, Controls,
     Forms, Dialogs, Menus, Printers, StdCtrls, ComCtrls, ToolWin,
     {$IFNDEF VER100}
     ImgList,
     {$ENDIF}
     {$IFNDEF VER90} // JPEG is not included in Delphi 2.
     JPEG,
     {$ENDIF}
     {$ENDIF}

     USAbout,
     twain,
     mcmTWAINContainer,
     mcmTWAINDSLayer;


const
  WM_DOEXITCLICK   = WM_USER + 1999;

type
  TFormSource = class(TForm)
    OpenDialog   : TOpenDialog;
    SaveDialog   : TSaveDialog;
    SetupDialog  : TPrinterSetupDialog;
    PrintDialog  : TPrintDialog;
    MainMenu     : TMainMenu;
    FileMenu     : TMenuItem;
    FileNew      : TMenuItem;
    FileOpen     : TMenuItem;
    FileSave     : TMenuItem;
    N1           : TMenuItem;
    FileSnap     : TMenuItem;
    FileGrab     : TMenuItem;
    N2           : TMenuItem;
    PrintItem    : TMenuItem;
    SetupPrinter : TMenuItem;
    N3           : TMenuItem;
    FileExit     : TMenuItem;

    ImageMenu    : TMenuItem;
    GainOffset   : TMenuItem;
    Invert       : TMenuItem;
    Resolution   : TMenuItem;

    SimulateItem : TMenuItem;
    EventsItem   : TMenuItem;

    HelpMenu     : TMenuItem;
    HelpItem     : TMenuItem;
    N4           : TMenuItem;
    AboutItem    : TMenuItem;
    N5           : TMenuItem;
    AppInfoItem  : TMenuItem;

    // A cure for a strange problem!
    // Don't delete the MsgBtn. If you need a MainMenu on your form and do not
    // have other components that goes into the form's Control list. Adding this
    // button somehow ensurs that messages are processed properly by the
    // MainMenu.
    MsgBtn       : TButton;

    StatusBar    : TStatusBar;
    BtnImageList : TImageList;

    ToolBar      : TToolBar;
    tbOpen       : TToolButton;
    tbSave       : TToolButton;
    tbTransfer   : TToolButton;
    ToolButton3  : TToolButton;
    tbCopy       : TToolButton;
    ToolButton5  : TToolButton;
    tbGrab       : TToolButton;

    procedure FormCreate       (    Sender : TObject);
    procedure FormClose        (    Sender : TObject;
                                var Action : TCloseAction);
    procedure FormDestroy      (    Sender : TObject);
    procedure FormPaint        (    Sender : TObject);
    procedure UpdateMenuItems  (    Sender : TObject);

    procedure FileNewClick     (    Sender : TObject);
    procedure FileOpenClick    (    Sender : TObject);
    procedure FileSaveClick    (    Sender : TObject);
    procedure FileSnapClick    (    Sender : TObject);
    procedure FileGrabClick    (    Sender : TObject);
    procedure PrintItemClick   (    Sender : TObject);
    procedure SetupPrinterClick(    Sender : TObject);
    procedure FileExitClick    (    Sender : TObject);
    procedure GainOffsetClick  (    Sender : TObject);
    procedure InvertClick      (    Sender : TObject);
    procedure ResolutionClick  (    Sender : TObject);
    procedure AboutItemClick   (    Sender : TObject);
    procedure AppInfoItemClick (    Sender : TObject);
    procedure EventsItemClick  (    Sender : TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FAppIdentity        : TW_IDENTITY;
    FContainerList      : TtwnContainerList;
    FOnNotifyCloseDSReq : TNotifyEvent; // Notify TWAINSource to close.
    FOnNotifyXferReady  : TNotifyEvent; // Notify TWAINSource that image is ready.
    FOnDeviceEvent      : TOnDeviceEvent;

    procedure ConvertResolution(OldUnit, NewUnit : TW_UINT16;
                                Cap              : TW_UINT16;
                                ContainerList    : TtwnContainerList);
    procedure DoSendExit(var Msg    : TMessage);   message WM_DOEXITCLICK;
  protected
  public
    { Public declarations }
    IsDeviceOK     : boolean;       // Is Hardware Device OK.
    IsDirty        : boolean;       // Does the TWAIN interface hold an image.
    IsNewFile      : boolean;       //
    IsSaved        : boolean;       // Is image saved ?
    Bitmap         : TBitmap;       // The Bitmap contains the image.
    ImageStream    : TMemoryStream; // Memory stream used to hold DIB.
    pBmpInfo       : PBitmapInfo;   // Pointer to Bitmap Info in ImageStream.
    pBmp           : Pointer;       // Pointer to Bitmap data in ImageStream.

    XferMech       : integer;
    PixelType      : integer;
    BitDepth       : integer;
    Brightness     : integer;
    Contrast       : integer;
    xyUnit         : integer;
    xResolution    : double;
    yResolution    : double;
    xScaling       : integer;
    yScaling       : integer;
    ShowIndicator  : boolean;
    CloseAction    : TCloseAction;
    ImageFrame     : TRect;

    {$IFNDEF VER90}
      JPEGImage    : TJPEGImage;
    {$ENDIF}

    procedure SaveToFile(FileName : string);
    function  CopyBmpToStream : boolean; // Makes a copy of bitmap in Stream.
    procedure ClearImageBuffer;
    // IMPORTANT !!!!
    // Assign function below to Bitmap.OnChange, whenever Bitmap is Created.
    // This ensures that functions elsewhere relying on pBmp & pBmpInfo, auto-
    // matically copy bitmap onto the streamer when needed, but only when the
    // bitmap has changed format (contained image).
    procedure ImageChanged(Sender : TObject);

    function  GetMaxPixelWidth : integer;
    function  GetMaxPixelHeight : integer;

    //--------------------------------------------------------------------------
    // IMPORTANT !!!!
    // The following functions/procedures must be implemented. They are called
    // from the TWAINSource object (in twd_prot.pas).
    //--------------------------------------------------------------------------
    procedure SetAppID(Identity : pTW_IDENTITY);
    procedure GetSupportedCaps(var Container : TtwnsContainer);
    procedure GetExtendedCaps(Container : TtwnsContainer);
    function  ResetCapability(ContainerList : TtwnContainerList; Cap : TW_UINT16) : word;
    function  ChangeCapability(ContainerList : TtwnContainerList; Cap : TW_UINT16) : word;
    procedure SetImageLayout(Frame : TRect);
    procedure GetImageLayout(var Frame : TRect);
    procedure ResetImageLayout(var Frame : TRect);

    procedure UserInterface(DoShow : boolean); // Data source is enabled/disabled.
    procedure StartAutoScan;

    property  OnDeviceEvent : TOnDeviceEvent
      read    FOnDeviceEvent
      write   FOnDeviceEvent;
    property  OnNotifyCloseDSRequest : TNotifyEvent
      read    FOnNotifyCloseDSReq
      write   FOnNotifyCloseDSReq;
    property  OnNotifyXferReady : TNotifyEvent
      read    FOnNotifyXferReady
      write   FOnNotifyXferReady;
  end;

var FormSource : TFormSource;


implementation

uses mcmTWAINFix, uTwnDlgHandle, uTwnDlgAppInfo, uTwnDlgSimEvents;

{$R *.DFM}

procedure TFormSource.FormCreate(Sender : TObject);
begin
  CloseAction := caNone;
  Screen.OnActiveFormChange := UpdateMenuItems;

  FOnNotifyCloseDSReq := Nil;
  OnNotifyXferReady   := Nil;

  //
  AddDlgHandle(Handle);
  AddDlgHandle(MainMenu.WindowHandle);
  AddDlgHandle(Toolbar.Handle);
  AddDlgHandle(MsgBtn.Handle);

  ClientWidth  := 512; // Any default client Width.
  ClientHeight := 512; // Any default client Height.

  IsNewFile    := True;
  IsDirty      := False;
  IsSaved      := True;

  // Set up bitmap handles.
  pBmp            := Nil;
  pBmpInfo        := Nil;
  Bitmap          := TBitmap.Create;
  Bitmap.OnChange := ImageChanged;
  ImageStream     := TMemoryStream.Create;

  XferMech        := TWSX_NATIVE;
  PixelType       := TWPT_RGB;
  BitDepth        := 24;
  Brightness      := 0;
  Contrast        := 0;
  xyUnit          := TWUN_INCHES;
  xResolution     := 300.0;
  yResolution     := 300.0;
  xScaling        := 100;
  yScaling        := 100;

  ImageFrame.Left   := 0;
  ImageFrame.Top    := 0;
  ImageFrame.Right  := 0;
  ImageFrame.Bottom := 0;

  //---- Check Device Hardware. ----
  // Insert Your code.
  // Check if device is present.
  if (True) // Is_Device_Present ?
  then begin
       IsDeviceOK := True;

       //---- Initialize Device Hardware. ----
       // Insert Your code.

  end
  else begin // Hardware was not present or functioning correctly !
       messagebeep(0);
       IsDeviceOK := False;
       Messagebox(Handle, 'Return to App.', 'Device Not Found !', Mb_OK);
       if Assigned(FOnNotifyCloseDSReq)
       then FOnNotifyCloseDSReq(Self);
  end;

  // Making sure that the button is visible but doesn't show on the main form.
  // See note above!
  MsgBtn.Top     := 0;
  MsgBtn.Left    := 0;
  MsgBtn.Width   := 0;
  MsgBtn.Height  := 0;
  MsgBtn.Visible := True;

  FContainerList := Nil;
  ResetImageLayout(ImageFrame);
end; { End TFormSource.FormCreate.                                             }


procedure TFormSource.SetAppID(Identity : pTW_IDENTITY);
begin
  Move(Identity^, FAppIdentity, SizeOf(TW_IDENTITY));
end; { End TFormSource.SetAppID.                                               }


procedure TFormSource.GetSupportedCaps(var Container : TtwnsContainer);
begin
  // This procedure is called when the data source is opened. You should fill in
  // the capabilities that you wish your TWAIN driver to support.
  // The capability containers are automatically created when leaving this
  // procedure.
  Container.NumItems  := 22;
  Container.Items[0]  := CAP_AUTHOR;
  Container.Items[1]  := CAP_INDICATORS;
  Container.Items[2]  := CAP_UICONTROLLABLE;
  Container.Items[3]  := CAP_XFERCOUNT;
  Container.Items[4]  := CAP_DEVICEEVENT;
  Container.Items[5]  := ICAP_XFERMECH;
  Container.Items[6]  := ICAP_IMAGEFILEFORMAT;
  Container.Items[7]  := ICAP_PIXELTYPE;
  Container.Items[8]  := ICAP_PLANARCHUNKY;
  Container.Items[9]  := ICAP_BITDEPTH;
  Container.Items[10] := ICAP_BITORDER;
  Container.Items[11] := ICAP_COMPRESSION;
  Container.Items[12] := ICAP_HALFTONES;
  Container.Items[13] := ICAP_BRIGHTNESS;
  Container.Items[14] := ICAP_CONTRAST;
  Container.Items[15] := ICAP_UNITS;
  Container.Items[16] := ICAP_XRESOLUTION;
  Container.Items[17] := ICAP_XSCALING;
  Container.Items[18] := ICAP_YRESOLUTION;
  Container.Items[19] := ICAP_YSCALING;
  Container.Items[20] := ICAP_PHYSICALHEIGHT;
  Container.Items[21] := ICAP_PHYSICALWIDTH;
end; { End TFormSource.GetSupportedCaps.                                       }


procedure TFormSource.GetExtendedCaps(Container : TtwnsContainer);
begin
  // This procedure is called when the data source is opened. You should fill in
  // the capabilities that you wish your TWAIN driver to support.
  // The capability containers are automatically created when leaving this
  // procedure.
  Container.NumItems  := 17;
  Container.Items[0]  := CAP_XFERCOUNT;
  Container.Items[1]  := CAP_DEVICEEVENT;
  Container.Items[2]  := ICAP_XFERMECH;
  Container.Items[3]  := ICAP_IMAGEFILEFORMAT;
  Container.Items[4]  := ICAP_PIXELTYPE;
  Container.Items[5]  := ICAP_PLANARCHUNKY;
  Container.Items[6]  := ICAP_BITDEPTH;
  Container.Items[7]  := ICAP_BITORDER;
  Container.Items[8]  := ICAP_COMPRESSION;
  Container.Items[9]  := ICAP_HALFTONES;
  Container.Items[10] := ICAP_BRIGHTNESS;
  Container.Items[11] := ICAP_CONTRAST;
  Container.Items[12] := ICAP_UNITS;
  Container.Items[13] := ICAP_XRESOLUTION;
  Container.Items[14] := ICAP_XSCALING;
  Container.Items[15] := ICAP_YRESOLUTION;
  Container.Items[16] := ICAP_YSCALING;
end; { End TFormSource.GetExtendedCaps.                                        }


function TFormSource.ResetCapability(ContainerList : TtwnContainerList; Cap : TW_UINT16) : word;
var PixWidth  : integer;
    PixHeight : integer;
    Container : TtwnsContainer;
begin
  Result := TWRC_SUCCESS;
  FContainerList := ContainerList;
  // This procedure is called when the data source is opened to initialise all
  // the capabilities listed in the procedure GetSupportedCaps, and when the
  // calling application requests that a capability is reset to it's default
  // values.
  // The passed Container correspond to the passed Cap value.

  // Note, you should also change capabilities that depends on the capability
  // being reset now.

  Container := TtwnsContainer(ContainerList.Items[Cap]);
  if Assigned(Container)
  then begin
       case Container.Capability of
       CAP_AUTHOR           : begin // Author.
                                Container.ContainerType := TWON_ONEVALUE;
                                {$IFDEF UNICODE}
                                {$ELSE}
                                {$ENDIF}
                                {$IFDEF UNICODE}
                                 Container.Items[0] := AnsiString('MCM DESIGN');
                                {$ELSE}
                                 Container.Items[0] := 'MCM DESIGN';
                                {$ENDIF}

                                // This capability can only be read (Our choice).
                                Container.QuerySupport := TWQC_GET;
                              end;
       CAP_INDICATORS       : begin // Indicator
                                Container.ContainerType := TWON_ONEVALUE;
                                Container.Items[0] := False;
                              end;
       CAP_UICONTROLLABLE   : begin // User interface controllable.
                                Container.ContainerType := TWON_ONEVALUE;
                                Container.Items[0] := True;

                                // This capability can only be read (My choice).
                                Container.QuerySupport := TWQC_GET;
                               end;
       CAP_DEVICEEVENT      : begin
                                Container.Clear;
                                Container.ContainerType := TWON_ARRAY;
                                Container.Capability := CAP_DEVICEEVENT;
                                Container.NumItems := 0;
                                EventsItem.Enabled := False;
                              end;
       ICAP_XFERMECH        : begin // Transfer mechanism.
                                XferMech := TWSX_NATIVE;
                                Container.ContainerType := TWON_ENUMERATION;
                                Container.NumItems := 3;
                                Container.Items[0] := TWSX_NATIVE;
                                Container.Items[1] := TWSX_FILE;
                                Container.Items[2] := TWSX_MEMORY;
                                Container.DefaultIndex := 0;
                                Container.CurrentIndex := 0;
                              end;
       ICAP_IMAGEFILEFORMAT : begin // Image file format.
                                Container.ContainerType := TWON_ENUMERATION;
                                case XferMech of
                                TWSX_NATIVE,
                                TWSX_MEMORY : begin
                                                Container.NumItems := 1;
                                                Container.Items[0] := TWFF_BMP;
                                                Container.DefaultIndex := 0;
                                                Container.CurrentIndex := 0;
                                              end;
                                TWSX_FILE   : begin
                                                Container.NumItems := 2;
                                                Container.Items[0] := TWFF_BMP;
                                                Container.Items[1] := TWFF_JFIF;
                                                Container.DefaultIndex := 0;
                                                // Container.CurrentIndex := 0;
                                              end;
                                end;
                              end;
       ICAP_COMPRESSION     : begin // Image compression.
                                Container.ContainerType := TWON_ONEVALUE;
                                Container.Items[0] := TWCP_NONE;

                                // This capability can only be read.
                                Container.QuerySupport := TWQC_GET;
                              end;
       CAP_XFERCOUNT        : begin // Transfer count.
                                Container.ContainerType := TWON_ONEVALUE;
                                Container.Items[0] := 1;
                              end;
       ICAP_PIXELTYPE       : begin // Pixel type.
                                Container.ContainerType := TWON_ENUMERATION;
                                Container.NumItems := 4;
                                Container.Items[0] := TWPT_RGB;
                                Container.Items[1] := TWPT_BW;
                                Container.Items[2] := TWPT_GRAY;
                                Container.Items[3] := TWPT_PALETTE;
                                PixelType := TWPT_RGB;
                                Container.DefaultIndex := 0;
                                Container.CurrentIndex := 0;
                              end;
       ICAP_PLANARCHUNKY    : begin // RGB image format (planar/Thuncky).
                                Container.ContainerType := TWON_ENUMERATION;
                                Container.NumItems := 1;
                                Container.DefaultValue := TWPC_CHUNKY;
                              end;
       ICAP_BITDEPTH        : begin // Bit depth (Bits per pixel).
                                Container.ContainerType := TWON_ENUMERATION;
                                Container.NumItems := 1;
                                case PixelType of
                                TWPT_RGB     : BitDepth := 24; // Other values 15, 16, 24, 32, 36
                                TWPT_BW      : BitDepth := 1;
                                TWPT_GRAY    : BitDepth := 8; // Other values 4, 8, 12, 16
                                TWPT_PALETTE : BitDepth := 8; // Other values 4, 8, 12, 16
                                end;
                                Container.Items[0] := BitDepth;
                                Container.DefaultValue := BitDepth;
                                Container.CurrentValue := BitDepth;
                              end;
       ICAP_BITORDER        : begin // Direction of bits (LSB, MSB).
                                Container.ContainerType := TWON_ONEVALUE;
                                // As this sample source only supports TWBO_MSBFIRST
                                // container type is set to TWON_ONEVALUE.
                                // If you add support for ICAP_BITORDER to use
                                // both formats set
                                // Container.ContainerType := TWON_ENUMERATION;
                                Container.Items[0] := TWBO_MSBFIRST;
                              end;
       ICAP_HALFTONES       : begin // Half tone patterns.
                                Container.ContainerType := TWON_ENUMERATION;
                                Container.NumItems := 4;
                                {$IFDEF UNICODE}
                                 Container.Items[0] := AnsiString('Half tone 1');
                                 Container.Items[1] := AnsiString('Half tone 2');
                                 Container.Items[2] := AnsiString('Half tone 3');
                                 Container.Items[3] := AnsiString('Half tone 4');
                                {$ELSE}
                                 Container.Items[0] := 'Half tone 1';
                                 Container.Items[1] := 'Half tone 2';
                                 Container.Items[2] := 'Half tone 3';
                                 Container.Items[3] := 'Half tone 4';
                                {$ENDIF}
                                Container.DefaultIndex := 0;
                                Container.CurrentIndex := 0;
                              end;
       ICAP_UNITS           : begin // Resolution unit.
                                Container.ContainerType := TWON_ENUMERATION;
                                Container.NumItems := 6;
                                Container.Items[0] := TWUN_INCHES;
                                Container.Items[1] := TWUN_CENTIMETERS;
                                Container.Items[2] := TWUN_PIXELS;
                                Container.Items[3] := TWUN_PICAS;
                                Container.Items[4] := TWUN_POINTS;
                                Container.Items[5] := TWUN_TWIPS;
                                Container.DefaultValue := TWUN_INCHES;
                                Container.CurrentValue := TWUN_INCHES;
                                // Update capabilities dependend on Units.
                                ChangeCapability(ContainerList, Cap);
                                xyUnit := TWUN_INCHES;
                              end;
       ICAP_XRESOLUTION     : begin // X (Horizontal) resolution.
                                xResolution := 300.0;
                                Container.ContainerType := TWON_RANGE;
                                Container.MinValue     := 0.10;
                                Container.MaxValue     := 32767.0;
                                Container.StepValue    := 0.10;
                                Container.DefaultValue := xResolution;
                                Container.CurrentValue := xResolution;
                              end;
       ICAP_XSCALING        : begin // X (Horizontal) scaling.
                                xScaling := 100;
                                Container.ContainerType := TWON_RANGE;
                                Container.MinValue     := 1.0;
                                Container.MaxValue     := 100.0;
                                Container.StepValue    := 25.0;
                                Container.DefaultValue := xScaling;
                                Container.CurrentValue := xScaling;
                              end;
       ICAP_YRESOLUTION     : begin // Y (Vertical) resolution.
                                yResolution := 300.0;
                                Container.ContainerType := TWON_RANGE;
                                Container.MinValue     := 0.10;
                                Container.MaxValue     := 32767.0;
                                Container.StepValue    := 0.10;
                                Container.DefaultValue := yResolution;
                                Container.CurrentValue := yResolution;
                              end;
       ICAP_YSCALING        : begin // Y (Vertical) scaling.
                                yScaling := 100;
                                Container.ContainerType := TWON_RANGE;
                                Container.MinValue     := 1.0;
                                Container.MaxValue     := 100.0;
                                Container.StepValue    := 25.0;
                                Container.DefaultValue := yScaling;
                                Container.CurrentValue := yScaling;
                              end;
       ICAP_PHYSICALHEIGHT  : begin // Max height (vertical size).
                                PixHeight := GetMaxPixelHeight;
                                Container.ContainerType := TWON_ONEVALUE;
                                case xyUnit of
                                TWUN_INCHES,
                                TWUN_CENTIMETERS,
                                TWUN_PICAS       : Container.Items[0] := PixHeight / yResolution;
                                TWUN_POINTS,
                                TWUN_PIXELS      : Container.Items[0] := 1.0 * PixHeight;
                                end;

                                // This capability can only be read.
                                Container.QuerySupport := TWQC_GET;
                              end;
       ICAP_PHYSICALWIDTH   : begin // Max width (horizontal size).
                                PixWidth  := GetMaxPixelWidth;
                                Container.ContainerType := TWON_ONEVALUE;
                                case xyUnit of
                                TWUN_INCHES,
                                TWUN_CENTIMETERS,
                                TWUN_PICAS       : Container.Items[0] := PixWidth  / xResolution;
                                TWUN_POINTS,
                                TWUN_PIXELS      : Container.Items[0] := 1.0 * PixWidth;
                                end;

                                // This capability can only be read.
                                Container.QuerySupport := TWQC_GET;
                              end;
       ICAP_BRIGHTNESS      : begin
                                Brightness := 0;
                                Container.ContainerType := TWON_RANGE;
                                Container.MinValue     := -1000.0;
                                Container.MaxValue     := 1000.0;
                                Container.StepValue    := 1.0;
                                Container.DefaultValue := Brightness;
                                Container.CurrentValue := Brightness;
                              end;
       ICAP_CONTRAST        : begin
                                Contrast := 0;
                                Container.ContainerType := TWON_RANGE;
                                Container.MinValue     := -1000.0;
                                Container.MaxValue     := 1000.0;
                                Container.StepValue    := 1.0;
                                Container.DefaultValue := Contrast;
                                Container.CurrentValue := Contrast;
                              end;
       else Result := TWCC_BADCAP;
       end;
  end
  else Result := TWCC_BADCAP;
end; { End TFormSource.ResetCapability.                                        }


function TFormSource.ChangeCapability(ContainerList : TtwnContainerList; Cap : TW_UINT16) : word;
var Container : TtwnsContainer;
    NewUnit   : word;
begin
  // This procedure is called whenever a capability has changed it's default
  // value. If this happens before the user interface becomes visible make
  // sure to reflect this in the selectable user interface controls.
  // If you support XXX you must update the same controls when the user
  // interface is visible.
  Result := TWRC_SUCCESS;

  Container := TtwnsContainer(ContainerList.Items[Cap]);
  if Assigned(Container)
  then begin
       case Container.Capability of
       CAP_AUTHOR           : ; // This capability we do not allow changed.
       CAP_INDICATORS       : begin
                                ShowIndicator := Container.CurrentValue;
                              end;
       CAP_UICONTROLLABLE   : begin
                                // See ResetCapability.
                                // Cannot be changed. Whether it was chosen
                                // that the data sources UI can be Shown/
                                // hidden at the applications will.
                              end;
       CAP_XFERCOUNT        : begin
                                // How many images to scan!
                                // If you allow more that one image to be
                                // acquired, the application can send the
                                // following values:
                                // -1 -> The application accepts any number
                                // of images that you can supply.
                                // 1 or n -> The application accepts 1 or n
                                // images to be transferred.
                              end;
       CAP_DEVICEEVENT      : begin
                                if (Container.NumItems > 0)
                                then EventsItem.Enabled := True
                                else EventsItem.Enabled := False;
                              end;
       ICAP_BITDEPTH        : begin
                                BitDepth := Container.CurrentValue;
                              end;
       ICAP_BITORDER        : begin // Doesn't realy support this now.
                              end;
       ICAP_COMPRESSION     : begin // Supported, but allow only TWCP_NONE.
                              end;
       ICAP_HALFTONES       : begin // Just added for the example.
                              end;
       ICAP_IMAGEFILEFORMAT : begin // The image format to be returned.
                              end;
       ICAP_PIXELTYPE       : begin
                                // Update: ICAP_BITDEPTH
                                //         ICAP_BITDEPTHREDUCTION
                                PixelType := Container.CurrentValue;
                                ResetCapability(ContainerList, ICAP_BITDEPTH);
                              end;
       ICAP_PLANARCHUNKY    : begin // Supported, but allow only TWPC_CHUNKY
                              end;
       ICAP_UNITS           : begin
                                // Update: ICAP_XNATIVERESOLUTION
                                //         ICAP_YNATIVERESOLUTION
                                //         ICAP_XRESOLUTION
                                //         ICAP_YRESOLUTION
                                //         ICAP_FRAMES
                                //         ICAP_PHYSICALHEIGHT
                                //         ICAP_PHYSICALWIDTH
                                //         ICAP_MINIMUMHEIGHT
                                //         ICAP_MINIMUMWIDTH
                                NewUnit := Container.CurrentValue;
                                if (NewUnit <> xyUnit)
                                then begin
                                     ConvertResolution(xyUnit, NewUnit, ICAP_XRESOLUTION, ContainerList);
                                     ChangeCapability(ContainerList, ICAP_XRESOLUTION);
                                     ConvertResolution(xyUnit, NewUnit, ICAP_YRESOLUTION, ContainerList);
                                     ChangeCapability(ContainerList, ICAP_YRESOLUTION);
                                     // ConvertResolution(xyUnit, NewUnit, ICAP_XNATIVERESOLUTION, ContainerList);
                                     // ChangeCapability(ContainerList, ICAP_XNATIVERESOLUTION);
                                     // ConvertResolution(xyUnit, NewUnit, ICAP_YNATIVERESOLUTION, ContainerList);
                                     // ChangeCapability(ContainerList, ICAP_YNATIVERESOLUTION);
                                     // If used ICAP_FRAMES should also reflect
                                     // the UNITS change.

                                     ResetCapability(ContainerList, ICAP_PHYSICALWIDTH);
                                     ResetCapability(ContainerList, ICAP_PHYSICALHEIGHT);
                                     // Similar and if supported, ICAP_MINIMUMWIDTH
                                     // and ICAP_MINIMUMHEIGHT could also be updated
                                     // by resetting.
                                     // ResetCapability(ContainerList, ICAP_MINIMUMWIDTH];
                                     // ResetCapability(ContainerList, ICAP_MINIMUMHEIGHT);
                                end;
                                xyUnit := NewUnit;
                              end;
       ICAP_XFERMECH        : begin
                                XferMech := Container.CurrentValue;
                                ResetCapability(ContainerList, ICAP_IMAGEFILEFORMAT);
                              end;
       ICAP_XRESOLUTION     : begin
                                xResolution := Container.CurrentValue;
                                ResetCapability(ContainerList, ICAP_PHYSICALWIDTH);
                              end;
       ICAP_XSCALING        : begin
                                xScaling := Container.CurrentValue;
                              end;
       ICAP_YRESOLUTION     : begin
                                yResolution := Container.CurrentValue;
                                ResetCapability(ContainerList, ICAP_PHYSICALHEIGHT);
                              end;
       ICAP_YSCALING        : begin
                                yScaling := Container.CurrentValue;
                              end;
       ICAP_BRIGHTNESS      : begin
                                Brightness := Container.CurrentValue;
                                if (Brightness < -1000)
                                then Brightness := -1000;
                                if (Brightness > 1000)
                                then Brightness := 1000;
                              end;
       ICAP_CONTRAST        : begin
                                Contrast := Container.CurrentValue;
                                if (Contrast < -1000)
                                then Contrast := -1000;
                                if (Contrast > 1000)
                                then Contrast := 1000;
                              end;
       else Result := TWCC_BADCAP;
       end;
  end
  else Result := TWCC_BADCAP;
end; { End TFormSource.ChangeCapability.                                       }


procedure TFormSource.UserInterface(DoShow : boolean);
begin
  if (Visible <> DoShow)
  then Visible := DoShow;
end; { End TFormSource.UserInterface.                                          }


procedure TFormSource.StartAutoScan;
begin
  // This procedure is only called if the data source is opened with the
  // User interface disabled (Not visible).
  pBmp     := Nil;
  pBmpInfo := Nil;
  Bitmap.Free;
  Bitmap := TBitmap.Create;
  Bitmap.OnChange := ImageChanged;

  // Scan/Acquire image automatically.
  // ACQUIRE/LOAD YOUR IMAGE INTO BITMAP HERE !!!!!!!
  FileOpenClick(Self);
end; { End TFormSource.StartAutoScan.                                          }


procedure TFormSource.ConvertResolution(OldUnit, NewUnit : TW_UINT16;
                                        Cap              : TW_UINT16;
                                        ContainerList    : TtwnContainerList);
var Container : TtwnsContainer;

   function FormUnitToUnit(Value : double) : double;
   var ppm : extended;
   begin
     // Convert from "Pixels Per OldUnit" to "Pixels Per Meters".
     ppm := 1;
     case OldUnit of
     TWUN_INCHES      : begin // m = 100.0 * Inch / 2.54
                          ppm := 100.0 * Value / 2.54;
                        end;
     TWUN_CENTIMETERS : begin // m = cm * 100.0
                          ppm := 100.0 * Value;
                        end;
     TWUN_PICAS       : begin // m = 600 * picas / 2.54
                          ppm := 600.0 * Value / 2.54;
                        end;
     TWUN_POINTS      : begin // m = 7200.0 * points / 2.54
                          ppm := 7200.0 * Value / 2.54;
                        end;
     TWUN_PIXELS      : begin // pixel = dimensionless.
                          ppm := 1.0;
                        end;
     TWUN_TWIPS       : begin // twips = inch / 1440
                          ppm := 140000.0 * Value / 2.54;
                        end;
     else Value := 30000.0 / 2.54;
     end;

     // Convert from "Pixels Per Meters" to "Pixels Per NewUnit".
     case NewUnit of
     TWUN_INCHES      : begin // inch = 2.54 * m / 100.0
                          Value := 2.54 * ppm / 100.0;
                        end;
     TWUN_CENTIMETERS : begin // cm = m / 100.0
                          Value := ppm / 100.0;
                        end;
     TWUN_PICAS       : begin // picas = m * 2.54 / 600
                          Value := 2.54 * ppm / 600.0;
                        end;
     TWUN_POINTS      : begin // points = 2.54 * (m / 7200.0)
                          Value := 2.54 * ppm / 7200.0;
                        end;
     TWUN_PIXELS      : begin // pixel = dimensionless.
                          Value := 1.0;
                        end;
     TWUN_TWIPS       : begin // twips = inch / 1440
                          Value := 2.54 * ppm / 140000.0;
                        end;
     end;
     Result := Value;
   end; { End FormUnitToUnit.                                                  }

var i     : integer;
    dStep : double;
begin
  Container := TtwnsContainer(ContainerList.Items[Cap]);

  if Assigned(Container)
  then begin
       if (OldUnit = TWUN_PIXELS)
       then ResetCapability(ContainerList, Cap)
       else begin
            case Container.ContainerType of
            TWON_ENUMERATION : with Container
                               do begin
                                  for i := 0 to (NumItems - 1)
                                  do Items[i] := FormUnitToUnit(Items[i]);
                               end;
            TWON_RANGE       : with Container
                               do begin
                                  dStep := StepValue;
                                  CurrentValue := dStep * Round(FormUnitToUnit(CurrentValue) / dStep);
                                  DefaultValue := dStep * Round(FormUnitToUnit(DefaultValue) / dStep);
                                  // MinValue     := dStep * Round(FormUnitToUnit(MinValue) / dStep);
                                  MaxValue     := dStep * Round(FormUnitToUnit(MaxValue) / dStep);
                               end;
            TWON_ONEVALUE    : with Container
                               do CurrentValue := FormUnitToUnit(CurrentValue);
            end;
       end;
  end;
end; { End TFormSource.ConvertResolution.                                      }


procedure TFormSource.SetImageLayout(Frame : TRect);
begin
  ImageFrame := Frame;
end; { End TFormSource.SetImageLayout.                                         }


procedure TFormSource.GetImageLayout(var Frame : TRect);
begin
  Frame := ImageFrame;
end; { End TFormSource.GetImageLayout.                                         }


procedure TFormSource.ResetImageLayout(var Frame : TRect);
begin
  ImageFrame.Left   := 0;
  ImageFrame.Top    := 0;
  ImageFrame.Right  := GetMaxPixelWidth;
  ImageFrame.Bottom := GetMaxPixelHeight;
  Frame := ImageFrame;
end; { End TFormSource.ResetImageLayout.                                       }


function TFormSource.GetMaxPixelWidth : integer;
begin
  Result := 32767;
end; { End TFormSource.GetMaxPixelWidth.                                       }


function TFormSource.GetMaxPixelHeight : integer;
begin
  Result := 32767;
end; { End TFormSource.GetMaxPixelHeight.                                      }


procedure TFormSource.FormClose(Sender : TObject; var Action : TCloseAction);
var i : integer;
begin
  //----------------------------------------------------------------------------
  // Without the following two blocks #1 and 2 all required windows messages
  // does not reach the data source, before it is closed (CloseDS), which
  // causes the calling application to fail and terminate abroutly.
  //----------------------------------------------------------------------------
  // #1, Make sure that ALL visible control's are disabled.
  for i := 0 to ControlCount - 1
  do begin
     Controls[i].Enabled := False;
     Controls[i].Visible := False;
  end;
  Update;

  // #2, Make sure that ALL Windows messages for controls are processed.
  (*
  // WARNING - DONT USE Application.ProcessMessages
  //-----------------------------------------------
  // NOT A GOOD SOLUTION - TAKES OVER APP'S MESSAGE QUEUE AND MAY HALT
  // PROCESSING!
  for i := 0 to 100
  do Application.ProcessMessages;
  *)

  if (CloseAction = caNone)
  then begin
       if IsDeviceOK
       then begin
       //---- Stop image acquisition. ----
       // Insert Your code.

       end;
       // Should not return an image.
       if Assigned(FOnNotifyCloseDSReq)
       then FOnNotifyCloseDSReq(Self);
  end
  else Action := CloseAction;
end; { End TFormSource.FormClose.                                              }


procedure TFormSource.FormDestroy(Sender : TObject);
begin
  // Set Screen.OnActiveFormChange to Nil to avoid access vialation.
  Screen.OnActiveFormChange := Nil;

  // Clear Bitmap, if any.
  Bitmap.Free;
  ImageStream.Free;

  // DeleteDlgHandle(Toolbar.Handle);
  // DeleteDlgHandle(Handle);
end; { End TFormSource.FormDestroy.                                            }


procedure TFormSource.DoSendExit(var Msg : TMessage);
begin
  FormSource.FileExitClick(FormSource)
end; { End TFormSource.DoSendExit.                                             }


procedure TFormSource.FormPaint(Sender : TObject);
begin
  if Not(Bitmap.Empty)
  then Canvas.Draw(0, ToolBar.Top + ToolBar.Height, Bitmap);
end; { End TFormSource.FormPaint.                                              }


procedure TFormSource.UpdateMenuItems(Sender : TObject);
var EnableMenu : boolean;
begin
  EnableMenu         := Not(Bitmap.Empty);
  FileSave.Enabled   := EnableMenu;
  PrintItem.Enabled  := EnableMenu;

  GainOffset.Enabled := EnableMenu;
  Invert.Enabled     := EnableMenu;
  Resolution.Enabled := EnableMenu;
end; { End TFormSource.UpdateMenuItems.                                        }


{------------------------------------------------------------------------------}
{ File Menu.                                                                   }
{------------------------------------------------------------------------------}

procedure TFormSource.FileNewClick(Sender : TObject);
begin
  pBmp     := Nil;
  pBmpInfo := Nil;
  Bitmap.Free;
  Bitmap := TBitmap.Create;
  Bitmap.OnChange := ImageChanged;

  if IsDeviceOK
  then begin
       IsDirty   := False;
       IsNewFile := True;
       IsSaved   := True;
       FileSave.Enabled := False;

       //---- Clear image from Device. ----
       // Insert Your code.

       Refresh;
  end;
  UpdateMenuItems(Sender);
end; { End TFormSource.FileNewClick.                                           }


procedure TFormSource.FileOpenClick(Sender : TObject);
begin
  OpenDialog.Filter   := 'Bitmap|*.BMP';
  {$IFNDEF VER90}
    OpenDialog.Filter := OpenDialog.Filter + '|JPEG|*.JPG';
  {$ENDIF}
  {$IFDEF INCTIFF}
    OpenDialog.Filter := OpenDialog.Filter + '|TIFF 5.0|*.TIF';
  {$ENDIF}
  case OpenDialog.FilterIndex of
  1 : OpenDialog.FileName := '*.BMP';
  2 : OpenDialog.FileName := '*.JPG';
  3 : OpenDialog.FileName := '*.TIF';
  end;
  if OpenDialog.Execute
  then begin
       // Clean up, remove previously loaded bitmaps.
       Bitmap.Free;
       Bitmap := TBitmap.Create;
       Bitmap.OnChange := ImageChanged;

       if (Pos('.BMP', Uppercase(OpenDialog.FileName)) <> 0)
       then Bitmap.LoadFromFile(OpenDialog.FileName);
       {$IFNDEF VER90}
         if (Pos('.JPG', Uppercase(OpenDialog.FileName)) <> 0)
         then begin
              JPEGImage := TJPEGImage.Create;
              JPEGImage.Performance := jpBestQuality;
              JPEGImage.LoadFromFile(OpenDialog.FileName);
              Bitmap.Width  := JPEGImage.Width;
              Bitmap.Height := JPEGImage.Height;
              Bitmap.Canvas.Draw(0, 0, JPEGImage);
              JPEGImage.Free;

              // Not all programs accept odd pixel formats, so...
              case Bitmap.PixelFormat of
              pf1bit,
              pf4bit  : Bitmap.PixelFormat := pf8bit;
              pf8bit  : ; // Don't do anything, be happy !
              pf15bit,
              pf16bit,
              pf32bit : Bitmap.PixelFormat := pf24bit;
              pf24bit : ; // Don't do anything, be happy !
              else Bitmap.PixelFormat := pf24bit;
              end;
         end;
       {$ENDIF}
       {$IFDEF INCTIFF}
         if (Pos('.TIF', Uppercase(OpenDialog.FileName)) <> 0)
         then LoadTIFFToBitmap(PChar(Lowercase(OpenDialog.FileName)), Bitmap);
       {$ENDIF}

       ClientWidth   := Bitmap.Width;
       ClientHeight  := Bitmap.Height + StatusBar.Height + ToolBar.Height;

       Refresh;
  end;
  UpdateMenuItems(Sender);
end; { End TFormSource.FileOpenClick.                                          }


procedure TFormSource.SaveToFile(FileName : string);
var FileExt : string;
begin
  FileExt := lowercase(ExtractFileExt(FileName));
  if (Length(FileExt) = 0)
  then FileExt := '.bmp';

  if (CompareStr('.bmp', FileExt) = 0)
  then Bitmap.SaveToFile(FileName);

  {$IFNDEF VER90}
    if (CompareStr('.jpg', FileExt) = 0)
    then begin
         JPEGImage := TJPEGImage.Create;
         JPEGImage.Assign(Bitmap);
         if (Bitmap.PixelFormat <> pf24bit)
         then JPEGImage.PixelFormat := jf24bit;
         JPEGImage.Scale := jsFullSize;
         // Dont compress JPEG image, ie. set quality to 100.
         JPEGImage.CompressionQuality  := 100;
         JPEGImage.ProgressiveEncoding := True;
         JPEGImage.SaveToFile(FileName);
         JPEGImage.Free;
    end;
  {$ENDIF}

  {$IFDEF INCTIFF}
    if (CompareStr('.tif', FileExt) = 0)
    then SaveTIFFToBitmap(PChar(FileName), Bitmap);
  {$ENDIF}
end; { End TFormSource.SaveToFile.                                             }


procedure TFormSource.FileSaveClick(Sender : TObject);
begin
  SaveDialog.Filter   := 'Bitmap|*.BMP';
  {$IFNDEF VER90}
    SaveDialog.Filter := SaveDialog.Filter + '|JPEG|*.JPG';
  {$ENDIF}
  {$IFDEF INCTIFF}
    SaveDialog.Filter := SaveDialog.Filter + '|TIFF 5.0|*.TIF';
  {$ENDIF}
  case SaveDialog.FilterIndex of
  1 : SaveDialog.FileName := '*.BMP';
  2 : SaveDialog.FileName := '*.JPG';
  3 : SaveDialog.FileName := '*.TIF';
  end;
  if (Pos('.BMP', SaveDialog.FileName) <> 0)
  then begin
       SaveDialog.FilterIndex := 1;
       SaveDialog.FileName := '*.BMP';
  end;
  if (Pos('.JPG', SaveDialog.FileName) <> 0)
  then begin
       SaveDialog.FilterIndex := 2;
       SaveDialog.FileName := '*.JPG';
  end;
  if (Pos('.TIF', SaveDialog.FileName) <> 0)
  then begin
       SaveDialog.FilterIndex := 3;
       SaveDialog.FileName := '*.TIF';
  end;

  if SaveDialog.Execute
  then begin
       if (Pos('.', SaveDialog.FileName) > 0)
       then SaveDialog.FileName := Copy(SaveDialog.FileName, 1, Pos('.', SaveDialog.FileName) - 1);
       case SaveDialog.FilterIndex of
       1 : SaveDialog.FileName := SaveDialog.FileName + '.BMP';
       2 : SaveDialog.FileName := SaveDialog.FileName + '.JPG';
       3 : SaveDialog.FileName := SaveDialog.FileName + '.TIF';
       end;
       SaveToFile(SaveDialog.FileName);
  end;
end; { End TFormSource.FileSaveClick.                                          }


procedure TFormSource.FileSnapClick(Sender : TObject);
begin
  // Clean up, remove previously loaded bitmaps.
  pBmp     := Nil;
  pBmpInfo := Nil;
  Bitmap.Free;
  Bitmap := TBitmap.Create;
  Bitmap.OnChange := ImageChanged;

  // Insert your code, that assigns an image to Bitmap.
  MessageBox(Handle, 'This function is Not implemented.', 'TWDSCR32', MB_OK);
end; { End TFormSource.FileSnapClick.                                          }


procedure TFormSource.FileGrabClick(Sender : TObject);
begin
  // Clean up, remove previously loaded bitmaps.
  pBmp     := Nil;
  pBmpInfo := Nil;
  Bitmap.Free;
  Bitmap := TBitmap.Create;
  Bitmap.OnChange := ImageChanged;

  // Insert your code, that assigns an image to Bitmap.
  MessageBox(Handle, 'This function is Not implemented.', 'TWDSCR32', MB_OK);
end; { End TFormSource.FileGrabClick.                                          }


procedure TFormSource.PrintItemClick(Sender : TObject);
begin
  if (Bitmap <> Nil)
  then begin
       if Not(Bitmap.Empty)
       then begin
            if PrintDialog.Execute
            then begin
                 FormSource.PrintScale := POPRINTTOFIT;
                 FormSource.Print;
            end;
       end
       else ShowMessage('There is nothing to print!');
  end;

  if PrintDialog.Execute
  then begin
       Printer.BeginDoc;
       Printer.Canvas.TextOut(100, 100, 'Print of picture acquired by TWAIN Driver');
       Printer.Canvas.Draw(100, 150, Bitmap);
       Printer.EndDoc;
  end;
end; { End TFormSource.PrintClick.                                             }


procedure TFormSource.SetupPrinterClick(Sender : TObject);
begin
  SetupDialog.Execute;
end; { End TFormSource.SetupPrinterClick.                                      }


procedure TFormSource.FileExitClick(Sender : TObject);
begin
  // Ask the application to close this driver ??
  if IsDeviceOK
  then begin
  //---- Stop image acquisition.
  // Insert Your code.

       if Bitmap.Empty
       then IsNewFile := True
       else IsNewFile := False;
  end
  else IsNewFile := True;

  if IsNewFile
  then begin
       if Assigned(FOnNotifyCloseDSReq)
       then FOnNotifyCloseDSReq(Self);
  end
  // Ask the application to close us ??
  // Send message back to the application
  // that we are ready
  else begin
       {
       // Create temporary file name. Needed for file transfer.
       GetTempPath(SizeOf(TWAINFileName) - 1, TWAINFileName);
       GetTempFileName(TWAINFileName, '', word(0), TWAINFileName);
       EraseFile(TWAINFileName);
       }

       // Notify receiving app, that transfer is ready.
       if Assigned(FOnNotifyXferReady)
       then FOnNotifyXferReady(Self);
  end;
end; { End TFormSource.FileExitClick.                                          }


{------------------------------------------------------------------------------}
{ Image Menu.                                                                  }
{------------------------------------------------------------------------------}

procedure TFormSource.GainOffsetClick(Sender : TObject);
begin
  // Some function to enhance contrast in your image !
  MessageBox(Handle, 'This function is Not implemented.', 'TWDSCR32', MB_OK);
end; { End TFormSource.GainOffsetClick.                                        }


procedure TFormSource.InvertClick(Sender : TObject);
begin
  // Some function to invert your image !
  MessageBox(Handle, 'This function is Not implemented.', 'TWDSCR32', MB_OK);
end; { End TFormSource.InvertClick.                                            }


procedure TFormSource.ResolutionClick(Sender : TObject);
begin
  // function to enable the user to change resolution of the image.
  MessageBox(Handle, 'This function is Not implemented.', 'TWDSCR32', MB_OK);
end; { End TFormSource.ResolutionClick.                                        }


{------------------------------------------------------------------------------}
{ Help Menu.                                                                   }
{------------------------------------------------------------------------------}

procedure TFormSource.AboutItemClick(Sender : TObject);
begin
  // The About Box.
  FormAbout := TFormAbout.Create(Application);
  FormAbout.ShowModal;
  FormAbout.Close;
end; { End TFormSource.AboutClick.                                             }


function TFormSource.CopyBmpToStream : boolean;
var pBmpFileHeader : PBitmapFileHeader;
    LongWidth      : longint;
    DestRect       : TRect;
begin
  // Make a copy of the bitmap (DIB) to a memory stream.
  // Ensure that you get hold of the bitmap info & data.
  // Oddly, Borland does not allow you to get this directly from the
  // TBitmap.Handle !

  Result   := False;
  pBmp     := Nil;
  pBmpInfo := Nil;
  if Assigned(Bitmap) and
     Assigned(ImageStream)
  then begin
       if Not(Bitmap.Empty)
       then begin
            // If the image is actually a JPEG compressed image loaded into the
            // Bitmap, then a call to: oh - extra tricks need be done !?!
            // Actually the image would then have been loaded into a Picture ->
            // the call is then:
            // TJPEG(ThePicture.Picture).DIBNeeded;
            // and ThePicture is of type TImage.
            // The ThePicture.Picture.Graphics will afterwoods have to be
            // assigned to the Bitmap.

            // Check ImagePixFrame (in twd_prot) for position/size of image to
            // return.
            // Pixel, Inch, centimeter are all treated as pixels.
            // The image before entering in Bitmap should be croped according to
            // ImagePixFrame.
            // Check if application did set Image Layout.
            if (ImageFrame.Left > 0)  or (ImageFrame.Top > 0) or
               (ImageFrame.Right > 0) or (ImageFrame.Bottom > 0)
            then begin // Crop image to rectangle given by ImageLayout.Frame.
                 if (ImageFrame.Left > Bitmap.Width)
                 then ImageFrame.Left := Bitmap.Width;
                 if (ImageFrame.Top > Bitmap.Height)
                 then ImageFrame.Top := Bitmap.Height;
                 if (ImageFrame.Right > Bitmap.Width)
                 then ImageFrame.Right := Bitmap.Width;
                 if (ImageFrame.Bottom > Bitmap.Height)
                 then ImageFrame.Bottom := Bitmap.Height;

                 DestRect.Left   := 0;
                 DestRect.Top    := 0;
                 DestRect.Right  := ImageFrame.Right - ImageFrame.Left;
                 DestRect.Bottom := ImageFrame.Bottom - ImageFrame.Top;

                 Bitmap.Canvas.CopyRect(DestRect, Bitmap.Canvas, ImageFrame);
                 Bitmap.Width  := DestRect.Right;
                 Bitmap.Height := DestRect.Bottom;
            end;

            Bitmap.HandleType := bmDIB;

            Bitmap.SaveToStream(ImageStream); // Get DIB copy in memory.
            ImageStream.Position := 0;
            pBmpFileHeader := ImageStream.Memory;
            pBmpInfo       := PBitmapInfo(@PAnsiChar(pBmpFileHeader)[SizeOf(TBitmapFileHeader)]);

            with pBmpInfo^.bmiHeader
            do begin
               // Compensate for strange Delphi behaviour with certain bitmaps !
               biSize         := 40;
               biWidth        := Bitmap.Width;
               biHeight       := Bitmap.Height;
               biPlanes       := 1;
               case Bitmap.PixelFormat of
               pf1bit  : biBitCount := 1;
               pf4bit  : biBitCount := 4;
               pf8bit  : biBitCount := 8;
               pf15bit : biBitCount := 15;
               pf16bit : biBitCount := 16;
               pf24bit : biBitCount := 24;
               pf32bit : biBitCount := 32;
               end;
               LongWidth := (((biWidth * biBitCount) + 31) div 32) * 4;
               biSizeImage    := LongWidth * biHeight;
               biClrUsed      := 0;
               biClrImportant := 0;
               biCompression  := BI_RGB;

               // Calculate resolution in pixels per meter.
               case xyUnit of
               TWUN_INCHES      : begin
                                    biXPelsPerMeter := Round(100 * xResolution / 2.54);
                                    biYPelsPerMeter := Round(100 * yResolution / 2.54);
                                  end;
               TWUN_CENTIMETERS : begin
                                    biXPelsPerMeter := Round(100 * xResolution);
                                    biYPelsPerMeter := Round(100 * yResolution);
                                  end;
               TWUN_PICAS,
               TWUN_POINTS,
               TWUN_PIXELS      : begin // Set dpi to 300.
                                    biXPelsPerMeter := Round(100 * 300 / 2.54);
                                    biYPelsPerMeter := Round(100 * 300 / 2.54);
                                  end;
               end;
            end;
            pBmp           := @PAnsiChar(pBmpFileHeader)[pBmpFileHeader^.bfoffBits];
            Result         := True;
       end;
  end;
end; { End TFormSource.CopyBmpToStream.                                        }


procedure TFormSource.ClearImageBuffer;
begin
  ImageStream.Clear;
end; { End TFormSource.ClearImageBuffer.                                       }


procedure TFormSource.ImageChanged(Sender : TObject);
begin
  // This function is called whenever the content of Bitmap is changed.
  pBmp     := Nil;
  pBmpInfo := Nil;
  ImageStream.Clear;
end; { End TFormSource.ImageChanged.                                           }


procedure TFormSource.AppInfoItemClick(Sender : TObject);
begin
  FormAppInfo := TFormAppInfo.Create(Self);
  FormAppInfo.SetAppID(@FAppIdentity);
  FormAppInfo.ShowModal;
  FormAppInfo.Destroy;
end; { End TFormSource.AppInfoItemClick.                                       }


procedure TFormSource.EventsItemClick(Sender : TObject);
var DeviceEvent : TW_DEVICEEVENT;
begin
  FormEvents := TFormEvents.Create(Self);
  FormEvents.DeviceEventContainer := TtwnContainer(FContainerList.Items[CAP_DEVICEEVENT]);
  if (FormEvents.ShowModal = mrOK)
  then begin
       if Assigned(FOnDeviceEvent)
       then begin
            FillChar(DeviceEvent, SizeOf(TW_DEVICEEVENT), #0);
            DeviceEvent.Event := FormEvents.Event;
            case TTwnDeviceEvent(DeviceEvent.Event) of
            TWDE_CHECKAUTOMATICCAPTURE  : begin
                                            DeviceEvent.AutomaticCapture := 0;
                                            DeviceEvent.TimeBeforeFirstCapture := 0;
                                            DeviceEvent.TimeBetweenCaptures := 0;
                                          end;
            TWDE_CHECKBATTERY           : begin
                                            DeviceEvent.BatteryMinutes := -1;
                                            DeviceEvent.BatteryPercentage := 100;
                                          end;
            TWDE_CHECKFLASH             : begin
                                            DeviceEvent.FlashUsed2 := TWFL_NONE;
                                          end;
            TWDE_CHECKPOWERSUPPLY       : begin
                                            DeviceEvent.PowerSupply := TWPS_EXTERNAL;
                                          end;
            TWDE_CHECKRESOLUTION        : begin
                                            DeviceEvent.XResolution := FloatToFIX32(xResolution);
                                            DeviceEvent.YResolution := FloatToFIX32(YResolution);
                                          end;

            end;
            FormEvents.Free;

            FOnDeviceEvent(Sender, @DeviceEvent);
       end;
  end
  else FormEvents.Free;
end; { End TFormSource.EventsItemClick.                                        }


procedure TFormSource.FormKeyDown(    Sender : TObject;
                                  var Key    : Word;
                                      Shift  : TShiftState);
begin
  // Handles menu short-cuts
  if (Shift = [ssAlt]) // With Alt key
  then begin
  end;
  if (Shift = [ssCtrl]) // With Ctrl key
  then begin
       if (Key = word(VkKeyScan('n'))) // File New - Ctrl+N
       then FileNewClick(Sender);
       if (Key = word(VkKeyScan('o'))) // File Open - Ctrl+O
       then FileOpenClick(Sender);
       if (Key = word(VkKeyScan('s'))) // File Save - Ctrl+S
       then FileSaveClick(Sender);
       if (Key = word(VkKeyScan('p'))) // File Print - Ctrl+P
       then PrintItemClick(Sender);
       if (Key = word(VkKeyScan('x'))) // // File Exit - Ctrl+X
       then FileExitClick(Sender);
  end;
  if (Shift = []) // No Shift, Ctrl or Alt key is down.
  then begin
       case Key of
       VK_F4 : begin
               end;
       VK_F5 : begin
               end;
       VK_F6 : begin
               end;
       VK_F7 : begin
               end;
       VK_F8 : begin
               end;
       VK_F9 : begin
               end;
       end;
  end;
end; // TFormSource.FormKeyDown.

end.

