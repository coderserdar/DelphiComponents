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
// $Log:  15910: uTwainForm.pas
//
//    Rev 1.16    2014-01-15 13:42:02  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.15    2013-12-04 23:16:16  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.14    08-01-2009 21:09:20  mcm
// Added support for Delphi 2009
//
//   Rev 1.13    15-05-2005 22:04:02  mcm    Version: DT 3.5
// Modified, scan count, bit depth, Bit depth reduction, and brightness &
// contrast negotiation.

//
//   Rev 1.12    19-02-2005 00:33:36  mcm    Version: DT 3.4
// Added support for configuration mode of data sources via their user interface.

//
//   Rev 1.11    14-03-2004 21:29:50  mcm

//
//   Rev 1.10    04-03-2004 21:02:32  mcm    Version: DT3.0
// Modified to additional argument in OnNegotiation event. This sample will now
// cancel a scan if the feeder isn't loaded.

//
//   Rev 1.9    06-11-2003 09:39:46  mcm    Version: DT3.0

//
//   Rev 1.8    31-07-2003 11:27:18  mcm

//
//   Rev 1.7    06-07-2003 11:06:36  mcm    Version: DT 2.5
// Added check in OnNegotiation for Nil containers when setting X and
// YResolution.

//
//   Rev 1.6    14-06-2003 11:11:28  mcm    Version: DT 2.4
// Added a general proceudre to handle application exceptions.
// Added the DeviceNotReady event procedure.

//
//   Rev 1.5    15-04-2003 10:46:50  mcm    Version: DT 2.3
// Added new properties on TmcmTWAIN: AutoBrightness, ClearBatchBuffers,
// MaxBatchBuffers, NativeX/YResolution, and X/YScaling. 
// Modified Get/SetCapabilityMsg to check if a capability a CAP_xxx and ICAP_xxx
// is supported by the data source. All properties and methods on TmcmTWAIN uses
// these two methods. Checking with IsCapSupported as a seperate call is
// therefore no longer necessary.
// Modified the OnNegotiation event procedure to use the new properties and
// support ADF in this example. 
// Added a sub-menu to choose the color format in the File menu. 

//
//   Rev 1.4    06-03-2003 11:07:52  mcm    Version: DT 2.2
// Added the TmcmSTI component (Microsoft´s Still Image system) allowing
// application to react to push buttons on scanners and cameras.

//
//   Rev 1.3    07-10-2002 15:46:26  mcm    Version: DT2.1

//
//   Rev 1.2    21-01-2002 14:08:54  mcm    Version: DT 2.0

//
//   Rev 1.1    18-01-2002 15:12:26  mcm
// Temp

//
//   Rev 1.0    04-12-2001 16:49:10  mcm    Version: DT 2.0

unit uTwainForm;

{$INCLUDE mcmDefines.pas}

interface

uses {$IFNDEF GE_DXE2}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     Menus, Buttons, ComCtrls, StdCtrls, ExtCtrls, JPEG,
     twain, mcmTWAIN, mcmTWAINContainer, mcmTWAINKernel, mcmTWAINIntf, mcmSTI;
     {$ELSE}
     WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
     Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Controls,
     Vcl.Buttons, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.jpeg,
     twain, mcmTWAIN, mcmTWAINContainer, mcmTWAINKernel, mcmTWAINIntf, mcmSTI;
     {$ENDIF}

type
  TFormTWAIN           = class(TForm)
    mcmSTI             : TmcmSTI;
    mcmTWAIN           : TmcmTWAIN;
    StatusBar1         : TStatusBar;

    Panel1             : TPanel;
    sbAcquire          : TSpeedButton;
    sbSource           : TSpeedButton;
    SourceList         : TComboBox;

    ScrollBox          : TScrollBox;
    Image1             : TImage;

    MainMenu1          : TMainMenu;
    FileMenu           : TMenuItem;
    Acquire1           : TMenuItem;
    Source1            : TMenuItem;
    N1                 : TMenuItem;
    TransferMode1      : TMenuItem;
    File2              : TMenuItem;
    Memory1            : TMenuItem;
    Native1            : TMenuItem;
    ErrorLevel1        : TMenuItem;
    None1              : TMenuItem;
    Information1       : TMenuItem;
    Error1             : TMenuItem;
    Full1              : TMenuItem;
    ShowUIItem         : TMenuItem;
    DisableAfterItem   : TMenuItem;
    ADFItem            : TMenuItem;
    EnableADFItem      : TMenuItem;
    EnableAutoFeedItem : TMenuItem;
    N2                 : TMenuItem;
    StretchImageItem   : TMenuItem;
    N3                 : TMenuItem;
    Exit1              : TMenuItem;

    SpecialMenu        : TMenuItem;
    OpenSMItem         : TMenuItem;
    OpenSItem          : TMenuItem;
    SendItem           : TMenuItem;
    EnableItem         : TMenuItem;
    EnableTransferItem : TMenuItem;
    DisableItem        : TMenuItem;
    CloseSItem         : TMenuItem;
    CloseSMItem        : TMenuItem;
    AboutItem          : TMenuItem;
    PreferenceItem     : TMenuItem;
    EnableNegotiatItem : TMenuItem;
    N4                 : TMenuItem;
    SourceInfoItem     : TMenuItem;
    SourceIconItem     : TMenuItem;

    ColorFormatMenu    : TMenuItem;
    BWItem             : TMenuItem;
    GrayItem           : TMenuItem;
    PaletteItem        : TMenuItem;
    RGBItem            : TMenuItem;
    ConfigureSourceItem: TMenuItem;

    procedure FormCreate             (    Sender    : TObject);

    // Remove function call and IFDEF below which does not match your Delphi
    // version. Code Insight in Delphi fails with this IFDEF
    (* *)
    {$IFNDEF VER100} // Delphi 4, 5 and 6
      procedure mcmTWAINImageReady   (    Sender    : TObject;
                                          pBmp      : Pointer;
                                          pBmpInfo  : PBitmapInfo;
                                          hImage    : hBitmap;
                                          FileName  : string);
    {$ELSE}         // Delphi 3
    (* *)
      procedure mcmTWAINImageReady   (    Sender    : TObject;
                                          pBmp      : Pointer;
                                          pBmpInfo  : PBitmapInfo;
                                          hImage    : Integer;
                                          FileName  : string);
    (* *)
    {$ENDIF}
    (* *)

    procedure SourceClick            (    Sender    : TObject);
    procedure AcquireClick           (    Sender    : TObject);

    procedure TransferMode1Click     (    Sender    : TObject);
    procedure XFerClick              (    Sender    : TObject);
    procedure ErrorLevelClick        (    Sender    : TObject);
    procedure ShowUIItemClick        (    Sender    : TObject);
    procedure Exit1Click             (    Sender    : TObject);

    procedure OpenSMItemClick        (    Sender    : TObject);
    procedure OpenSItemClick         (    Sender    : TObject);
    procedure SendItemClick          (    Sender    : TObject);
    procedure EnableItemClick        (    Sender    : TObject);
    procedure EnableTransferItemClick(    Sender    : TObject);
    procedure DisableItemClick       (    Sender    : TObject);
    procedure CloseSItemClick        (    Sender    : TObject);
    procedure CloseSMItemClick       (    Sender    : TObject);
    procedure mcmTWAINNegotiation    (    Sender    : TObject;
                                      var DoCancel  : boolean);
    procedure mcmTWAINEnableMenus    (    Sender    : TObject);
    procedure mcmTWAINDisableMenus   (    Sender    : TObject);
    procedure AboutItemClick         (    Sender    : TObject);
    procedure mcmTWAINXferReady      (    Sender    : TObject);
    procedure mcmTWAINXferNext       (    Sender    : TObject;
                                      var NumImages : Integer;
                                      var SkipNext  : boolean);
    procedure FormResize             (    Sender    : TObject);
    procedure PreferenceItemClick    (    Sender    : TObject);
    procedure StretchImageItemClick  (    Sender    : TObject);
    procedure DisableAfterItemClick  (    Sender    : TObject);
    procedure FileMenuClick          (    Sender    : TObject);
    procedure ErrorLevel1Click       (    Sender    : TObject);
    procedure EnableADFItemClick     (    Sender    : TObject);
    procedure EnableAutoFeedItemClick(    Sender    : TObject);
    procedure mcmTWAINDeviceEvent    (    Sender    : TObject;
                                          Event     : TTwnDeviceEvent;
                                          DeviceName: String;
                                          A, B, C   : Variant);
    procedure EnableNegotiatItemClick( Sender    : TObject);
    procedure SourceInfoItemClick    (    Sender    : TObject);
    procedure mcmTWAINMemXferSize    (    Sender    : TObject;
                                          MinSize,
                                          MaxSize   : Integer;
                                      var BufSize   : Integer;
                                          pBmpInfo  : PBitmapInfo);
    procedure mcmTWAINFailure        (    Sender    : TObject;
                                          DG        : longint;
                                          DAT,
                                          CAP,
                                          MSG       : Word;
                                          Error,
                                          Status    : Integer);
    procedure SourceIconItemClick    (    Sender    : TObject);
    procedure mcmTWAINCloseSource(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ColorFormatItemClick(Sender: TObject);
    procedure mcmTWAINDeviceNotReady(Sender: TObject; var DoOpenSource: Boolean);
    procedure ConfigureSourceItemClick(Sender: TObject);
  private
    { Private declarations }
    FColorFormat   : integer;
    FImageFileName : string;
    FImageIndex    : integer;
    procedure AppException(Sender : TObject; E : Exception);
    procedure AdjustImageSize;
    procedure ShowHint               (    Sender    : TObject);
    procedure EnableSpecialMenu      (    Mgr       : boolean;
                                          Src       : boolean);
  public
    { Public declarations }
  end;

var FormTWAIN : TFormTWAIN;

implementation

uses uTwnDlgSrcInfo;

{$R *.DFM}
{$IFOPT X-} {$X+} {$ENDIF}

procedure TFormTWAIN.FormCreate(Sender : TObject);
var ActualDeviceName : string;
begin
  Application.OnHint := ShowHint;
  // General exception handler. AppException is called when Exception are
  // unhandled, i.e. occur outside a try - except
  // Note: Exceptions that occur in the data source is catched by TmcmTWAIN
  // and returned via OnFailure.
  Application.OnException := AppException;

  FImageIndex := 1;
  FImageFileName := '.\image1.bmp';

  // Register the application via registry keys for use in installation programs.
  // HKLM"SOFTWARE\Microsoft\Windows\CurrentVersion\StillImage\Registered Applications",
  // "TWAIN Toolkit for Delphi",,"fullpath\d32twain.exe\StiDevice:%%1\StiEvent:%%2"

  // An application should register itself each time it is launced, just in case
  // the user relocated the application.
  if mcmSTI.RegisterApplication(Caption, Application.ExeName)
  then begin
       ActualDeviceName := mcmSTI.LaunchedBySTI;
       if (ActualDeviceName <> '')
       then begin
            mcmSTI.GetDeviceInfo('');
            case mcmSTI.DeviceType of // Get type of hardware - just for fun.
            StiDeviceTypeDefault :
            StatusBar1.SimpleText := '[Default] ';
            StiDeviceTypeScanner :
            StatusBar1.SimpleText := '[Scanner] ';
            StiDeviceTypeDigitalCamera :
            StatusBar1.SimpleText := '[Camera] ';
            end;
            StatusBar1.SimpleText := StatusBar1.SimpleText +
                                     'Launched by STI: ' + mcmSTI.ActualDeviceName;
            mcmTWAIN.Acquire(ActualDeviceName);
       end;
  end;

  FColorFormat := TWPT_RGB;
  RGBItem.Checked := True;
end; // TFormTWAIN.FormCreate.


procedure TFormTWAIN.FormActivate(Sender : TObject);
var OldCursor : hCursor;
begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  Update;
  //mcmTWAIN.SetParentWnd(Self.Handle);
  // Get list of available TWAIN sources.
  if (SourceList.Items.Count = 0)
  then mcmTWAIN.GetSourceList(SourceList.Items);
  SourceList.Items.Insert(0, '');
  SourceList.ItemIndex := 0;
  Screen.Cursor := OldCursor;
end; // TFormTWAIN.FormActivate.


procedure TFormTWAIN.AppException(Sender : TObject; E : Exception);
begin
  // Unhandled exceptions
  Application.ShowException(E);
  // It's up to you how to handle exceptions! You could terminate the application.
  // Application.Terminate;
end; // TFormTWAIN.AppException.


procedure TFormTWAIN.ShowHint(Sender : TObject);
begin
  StatusBar1.SimpleText := Application.Hint;
end; // TFormTWAIN.ShowHint.


procedure TFormTWAIN.FormResize(Sender : TObject);
begin
  AdjustImageSize;
end; // TFormTWAIN.FormResize.

//------------------------------------------------------------------------------
// File menu.
//------------------------------------------------------------------------------

procedure TFormTWAIN.FileMenuClick(Sender : TObject);
begin
  ShowUIItem.Checked       := mcmTWAIN.ShowUI;
  DisableAfterItem.Checked := mcmTWAIN.DisableAfterAcquire;
end; // TFormTWAIN.FileMenuClick.


procedure TFormTWAIN.SourceClick(Sender : TObject);
begin
  mcmTWAIN.SelectSource;
end; // TFormTWAIN.SourceClick.


procedure TFormTWAIN.SourceIconItemClick(Sender : TObject);
begin
//  mcmTWAIN.SelectSourceIcon;
end; // TFormTWAIN.SourceIconItemClick.


procedure TFormTWAIN.AcquireClick(Sender : TObject);
begin
  FImageIndex := 1;
  FImageFileName := '.\image1.bmp';
  mcmTWAIN.Filename := FImageFileName;
  if Not(mcmTWAIN.Acquire(SourceList.Text))
  then ShowMessage('Could not open data source!');
end; // TFormTWAIN.AcquireClick.


procedure TFormTWAIN.PreferenceItemClick(Sender: TObject);
begin
  mcmTWAIN.Preferrences;
end; // TFormTWAIN.PreferenceItemClick.


procedure TFormTWAIN.ColorFormatItemClick(Sender : TObject);
var i : integer;
begin
  for i := 0 to (ColorFormatMenu.Count - 1)
  do ColorFormatMenu.Items[i].Checked := False;
  if (Sender is TMenuItem)
  then begin
       FColorFormat := (Sender as TMenuItem).Tag;
       (Sender as TMenuItem).Checked := True;
  end;
end; // TFormTWAIN.ColorFormatItemClick.


procedure TFormTWAIN.TransferMode1Click(Sender : TObject);
var i : integer;
begin
  // Check the right menu item when Transfer Mode menu opens.
  for i := 0 to 2
  do TransferMode1.Items[i].Checked := False;
  case mcmTwain.XferMech of
  TWFX_Native      : Native1.Checked := True;
  TWFX_Files       : File2.Checked   := True;
  TWFX_Memory      : Memory1.Checked := True;
  end;
end; // TFormTWAIN.TransferMode1Click.


procedure TFormTWAIN.XFerClick(Sender : TObject);
begin
  // Set selected Transfer mode.
  case (Sender as TMenuItem).Tag of
  0 : mcmTwain.XferMech := TWFX_Native;
  1 : mcmTwain.XferMech := TWFX_Files;
  2 : mcmTwain.XferMech := TWFX_Memory;
  end;
end; // TFormTWAIN.XFerClick.


procedure TFormTWAIN.ErrorLevel1Click(Sender : TObject);
var i : integer;
begin
  // Check right ErrorLevel menu when Error Level menu opens.
  for i := 0 to 3
  do ErrorLevel1.Items[i].Checked := False;
  case mcmTwain.MessageLevel of
  ML_NONE  : None1.Checked := True;
  ML_ERROR : Error1.Checked := True;
  ML_INFO  : Information1.Checked := True;
  ML_FULL  : Full1.Checked := True;
  end;
end; // TFormTWAIN.ErrorLevel1Click.


procedure TFormTWAIN.ErrorLevelClick(Sender : TObject);
begin
  case (Sender as TMenuItem).Tag of
  0 : mcmTwain.MessageLevel := ML_None;
  1 : mcmTwain.MessageLevel := ML_Info;
  2 : mcmTwain.MessageLevel := ML_Error;
  3 : mcmTwain.MessageLevel := ML_Full;
  end;
end; // TFormTWAIN.ErrorLevelClick.


procedure TFormTWAIN.ShowUIItemClick(Sender : TObject);
begin
  ShowUIItem.Checked := Not(ShowUIItem.Checked);
  mcmTwain.ShowUI := ShowUIItem.Checked;
end; // TFormTWAIN.ShowUIItemClick.


procedure TFormTWAIN.DisableAfterItemClick(Sender : TObject);
begin
  mcmTWAIN.DisableAfterAcquire := Not(DisableAfterItem.Checked);
end; // TFormTWAIN.DisableAfterItemClick.


procedure TFormTWAIN.EnableNegotiatItemClick(Sender : TObject);
begin
  EnableNegotiatItem.Checked := Not(EnableNegotiatItem.Checked);
end; // TFormTWAIN.EnableNegotiatItemClick.


procedure TFormTWAIN.EnableADFItemClick(Sender : TObject);
begin
  EnableADFItem.Checked := Not(EnableADFItem.Checked);
end; // TFormTWAIN.EnableADFItemClick.


procedure TFormTWAIN.EnableAutoFeedItemClick(Sender : TObject);
begin
  EnableAutoFeedItem.Checked := Not(EnableAutoFeedItem.Checked);
end; // TFormTWAIN.EnableAutoFeedItemClick.


procedure TFormTWAIN.StretchImageItemClick(Sender : TObject);
begin
  StretchImageItem.Checked := Not(StretchImageItem.Checked);
  AdjustImageSize;
  Repaint;
end; // TFormTWAIN.StretchImageItemClick.


procedure TFormTWAIN.Exit1Click(Sender : TObject);
begin
  // Close this application.
  Close;
end; // TFormTWAIN.Exit1Click.

//------------------------------------------------------------------------------
// Special menu.
//------------------------------------------------------------------------------

procedure TFormTWAIN.OpenSMItemClick(Sender : TObject);
var OldCursor : hCursor;
begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  mcmTwain.OpenSourceMgr;
  EnableSpecialMenu(True, False);
  Screen.Cursor := OldCursor;
end; // TFormTWAIN.OpenSMItemClick.


procedure TFormTWAIN.OpenSItemClick(Sender : TObject);
var OldCursor : hCursor;
begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  // Disable the OnNegotiation event.
  // When using the Special menu, we only want to negotiage capabilities
  // chosen via the Send Dialogue.
  mcmTwain.OnNegotiation := Nil;
  mcmTwain.OpenSource('');
  // Re-instate the OnNegotiation event.
  mcmTwain.OnNegotiation := mcmTWAINNegotiation;
  Screen.Cursor := OldCursor;
end; // TFormTWAIN.OpenSItemClick.


procedure TFormTWAIN.SendItemClick(Sender : TObject);
begin
  mcmTWAIN.ExecuteDlgSend;
end; // TFormTWAIN.SendItemClick.


procedure TFormTWAIN.EnableItemClick(Sender : TObject);
var OldCursor : hCursor;
begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  mcmTwain.EnableSource(ShowUIItem.Checked);
  // Update menu "Show User Interface" to reflect whether the data source's UI
  // is shown or hidden.
  ShowUIItem.Checked := mcmTWAIN.ShowUI;
  Screen.Cursor := OldCursor;
end; // TFormTWAIN.EnableItemClick.


procedure TFormTWAIN.EnableTransferItemClick(Sender : TObject);
var OldCursor : hCursor;
begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  mcmTwain.EnableSource(False);
  Screen.Cursor := OldCursor;
end; // TFormTWAIN.EnableTransferItemClick.


procedure TFormTWAIN.DisableItemClick(Sender : TObject);
var OldCursor : hCursor;
begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  mcmTwain.DisableSource;
  Screen.Cursor := OldCursor;
end; // TFormTWAIN.DisableItemClick.


procedure TFormTWAIN.CloseSItemClick(Sender : TObject);
var OldCursor : hCursor;
begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  mcmTwain.CloseSource;
  Screen.Cursor := OldCursor;
end; // TFormTWAIN.CloseSItemClick.


procedure TFormTWAIN.CloseSMItemClick(Sender : TObject);
var OldCursor : hCursor;
begin
  OldCursor     := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  mcmTwain.CloseSourceMgr;
  EnableSpecialMenu(False, False);
  Screen.Cursor := OldCursor;
end; // TFormTWAIN.CloseSMItemClick.

//------------------------------------------------------------------------------
// About menu.
//------------------------------------------------------------------------------

procedure TFormTWAIN.AboutItemClick(Sender : TObject);
begin
  mcmTWAIN.About;
end; // TFormTWAIN.AboutItemClick.

//------------------------------------------------------------------------------
// Adjust image size.
//------------------------------------------------------------------------------

procedure TFormTWAIN.AdjustImageSize;
var ch, cw : longint;
    ih, iw : longint;
    ScaleH : real;
    ScaleV : real;
begin
  if Not(Image1.Picture.Bitmap.Empty)
  then begin
       if StretchImageItem.Checked
       then begin
            ScrollBox.HorzScrollBar.Position := 0;
            ScrollBox.VertScrollBar.Position := 0;
            Image1.Align   := alNone;
            Image1.Stretch := True;

            ch := ClientHeight - StatusBar1.Height - Panel1.Height;
            cw := ClientWidth;

            if (Image1.Picture.Bitmap.Width > 0)
            then ScaleH := cw / Image1.Picture.Bitmap.Width
            else ScaleH := 1.0;
            if (Image1.Picture.Bitmap.Height > 0)
            then ScaleV := ch / Image1.Picture.Bitmap.Height
            else ScaleV := 1.0;

            if (ScaleH < ScaleV)
            then begin
                 ih := Round(ScaleH * Image1.Picture.Bitmap.Height);
                 Image1.Top := abs(ch - ih) div 2 + Panel1.Height;
                 if (Image1.Top < 0)
                 then Image1.Top := 0;
                 Image1.Height := ih;

                 Image1.Left := 0;
                 Image1.Width := Round(ScaleH * Image1.Picture.Bitmap.Width);
            end
            else begin
                 iw := Round(ScaleV * Image1.Picture.Bitmap.Width);
                 Image1.Left := (cw - iw) div 2;
                 if (Image1.Left < 0)
                 then Image1.Left := 0;
                 Image1.Width := iw;
                 Image1.Top := 0;
                 Image1.Height := Round(ScaleV * Image1.Picture.Bitmap.Height);
            end;
       end
       else begin
            Image1.Align := alNone;
            Image1.Stretch := False;
            Image1.Center := False;
            Image1.Left := 0;
            Image1.Top := 0;
            Image1.Width := Image1.Picture.Bitmap.Width;
            Image1.Height := Image1.Picture.Bitmap.Height;
       end;
  end;
end; // TFormTWAIN.AdjustImageSize.


procedure TFormTWAIN.EnableSpecialMenu(Mgr : boolean; Src : boolean);
begin
  OpenSMItem.Enabled         := Not(Mgr);

  OpenSItem.Enabled          := Not(Src) and Mgr;
  SendItem.Enabled           := Src;
  EnableItem.Enabled         := Src;
  EnableTransferItem.Enabled := Src;
  DisableItem.Enabled        := Src;
  CloseSItem.Enabled         := Src;

  CloseSMItem.Enabled        := True;
end; // End TFormTWAIN.EnableSpecialMenu.

//------------------------------------------------------------------------------
// mcmTWAIN events.
//------------------------------------------------------------------------------

procedure TFormTWAIN.mcmTWAINEnableMenus(Sender : TObject);
begin
//------------------------------------------------------------------------------
// Enable TWAIN menus.
// -------------------
// This event is fired after the Data Source is closed.
// Use the OnDisableMenus event to disable menus etc.
//------------------------------------------------------------------------------
  Acquire1.Enabled  := True;
  Source1.Enabled   := True;
  sbSource.Enabled  := True;
  sbAcquire.Enabled := True;
  EnableSpecialMenu(False, False);
end; // TFormTWAIN.mcmTWAINEnableMenus.


procedure TFormTWAIN.mcmTWAINDisableMenus(Sender : TObject);
begin
//------------------------------------------------------------------------------
// Disable TWAIN menus.
// --------------------
// This event is fired just before the Data Source is opened.
// Use the OnEnableMenus event to re-enable menus etc.
//------------------------------------------------------------------------------
  Acquire1.Enabled  := False;
  Source1.Enabled   := False;
  sbSource.Enabled  := False;
  sbAcquire.Enabled := False;
  EnableSpecialMenu(True, True);

  // Update menu "Show User Interface" to reflect whether the data source's UI
  // is shown or hidden.
  ShowUIItem.Checked := mcmTWAIN.ShowUI;
end; // TFormTWAIN.mcmTWAINDisableMenus.



procedure TFormTWAIN.mcmTWAINCloseSource(Sender : TObject);
begin
  StatusBar1.SimpleText := 'Data Source was closed on request.';
end; // TFormTWAIN.mcmTWAINCloseSource.


procedure TFormTWAIN.mcmTWAINDeviceNotReady(Sender : TObject; var DoOpenSource : Boolean);
begin
  // First we check if the device is on-line.
  case MessageDlg('Device is not attached, powered on or communicating! ' + chr($0D) +
                  'Would you like to retry ?', mtConfirmation, [mbYes, mbNo], 0) of
  mrYes   : Sleep(500);
  mrNo    : DoOpenSource := False;
  end;
end; // TFormTWAIN.mcmTWAINDeviceNotReady.


procedure TFormTWAIN.mcmTWAINNegotiation(Sender : TObject; var DoCancel  : boolean);
var i             : integer;
    r             : double;
    Resolution    : double;
    xRes, yRes    : double;
    xMin, yMin    : double;
    xMax, yMax    : double;
    ImageLayout   : TImageLayout;
    Container     : TtwnContainer;
    FADFAvailable : boolean;
    FBitDepth     : integer;
    FBitReduction : integer;
begin
//------------------------------------------------------------------------------
// Negotiate capabilities.
// -----------------------
// This event is fired when the Data Source is opened, but before it becomes
// enabled.
// When you negotiate capabilities, you should alway check that the capability
// is supported by calling IsCapSupported(CAP_xxxx).
// Unfortunatly some data sources promis just a bit more than they realy can
// live up. Therefore it's a fairly good idear to GET the capability before
// SETting it.
//
// Below, you'll find many examples on how to change the data source settings.
// You do not have to implement all or any of these negotiations, just the ones
// required by your application. But remember that some capabilities do depend
// on others.
//------------------------------------------------------------------------------

  if Not(EnableNegotiatItem.Checked)
  then exit;

  //----------------------------------------------------------------------------
  // Feeder capabilities.

  // Is Feeder selected in the menu, then enable it.
  // Call FeederEnabled to enable/disable the feeder.
  if mcmTWAIN.FeederEnabled(EnableADFItem.Checked)
  then FADFAvailable := EnableADFItem.Checked
  else begin
       Container := mcmTWAIN.Containers.Items[CAP_FEEDERENABLED];
       if Assigned(Container)
       then FADFAvailable := Container.CurrentValue
       else FADFAvailable := False;
  end;

  if FADFAvailable
  then begin
       if mcmTWAIN.PaperDetectable
       then if Not(mcmTWAIN.FeederLoaded)
            then begin
                 // Inform user that feeder is not loaded.
                 ShowMessage('Could not detect paper in feeder.');
                 DoCancel := True;
                 // Could loop until paper is inserted or quit
                 // acquisition!
            end;

       // Enable auto-feed.
       if EnableAutoFeedItem.Checked
       then mcmTWAIN.AutoFeed(True)
       else mcmTWAIN.AutoFeed(False);

       // Check for Duplex scan unit, if found then enable it.
       if (mcmTWAIN.Duplex <> TWDX_NONE)
       then mcmTWAIN.DuplexEnabled := True;
  end
  // Feeder could not be enabled. The scanner properly doesn't have
  // a feeder!
  else EnableADFItem.Checked := False;


  //----------------------------------------------------------------------------
  // Negotiate dimension unit.

  // Get/Set current unit.
  if (mcmTWAIN.Units <> TWUN_INCHES)
  then mcmTWAIN.Units := TWUN_INCHES;

  // Get native resolution (Devices optical resolution).
  // Use this and set to XResolution & YResolution if f. ex. you want an image
  // recorded with the scanners native/optical resolution.
  //xRes := mcmTWAIN.NativeXResolution;
  //yRes := mcmTWAIN.NativeYResolution;

  // Get physical max size.
  xMax := mcmTWAIN.PhysicalWidth;
  yMax := mcmTWAIN.PhysicalHeight;

  // Get scanners/cameras minimum scan height and width.
  if mcmTWAIN.IsCapSupported(ICAP_MINIMUMHEIGHT)
  then yMin := mcmTWAIN.MinimumHeight
  else yMin := 0;
  if (yMin < 0)
  then yMin := 0;
  if mcmTWAIN.IsCapSupported(ICAP_MINIMUMWIDTH)
  then xMin := mcmTWAIN.MinimumWidth
  else xMin := 0;
  if (xMin < 0)
  then xMin := 0;

  //----------------------------------------------------------------------------
  // Negotiate Color.

  // Get/Set current bit order.
  if (mcmTWAIN.BitOrder <> TWBO_MSBFIRST)
  then mcmTWAIN.BitOrder := TWBO_MSBFIRST;

  // Get/Set current pixel type.
  // FColorFormat := TWPT_RGB; //TWPT_BW; // TWPT_RGB; Value set via menu!
  if mcmTWAIN.IsCapSupported(ICAP_PIXELTYPE)
  then begin
       if (mcmTWAIN.PixelType <> FColorFormat)
       then begin
            mcmTWAIN.PixelType := FColorFormat;
            FColorFormat := mcmTWAIN.PixelType;
       end;
  end;

  // If you would like to negotiate other capabilities not directly supported
  // by the TmcmTWAIN component, this is how to proceed.
  // For example for setting PixelType:
  (*
  if mcmTwain.IsCapSupported(ICAP_PIXELTYPE)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_PIXELTYPE, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            Container.CurrentValue := TWPT_GRAY;
            if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) <> TWRC_SUCCESS)
            then { An Error occured. } ;
       end;
       Container := Nil;
  end;
  *)

  //----------------------------------------------------------------------------
  // Get/Set current bit depth.
  // The number of bits, are the total bits for a pixel (color).
  // TWPC_BW      -> ICAP_BITDEPTH = 1
  // TWPC_GRAY    -> ICAP_BITDEPTH = 4, 8 (maybe higher, if source supports this).
  // TWPC_PALETTE -> ICAP_BITDEPTH = 4 or 8.
  // TWPC_RGB     -> ICAP_BITDEPTH = 16, 24, 32 (maybe higher, if source supports this).
  FBitDepth := mcmTWAIN.BitDepth;
  case FColorFormat of
  TWPT_BW      : begin
                   if (FBitDepth <> 1)
                   then begin
                        mcmTWAIN.BitDepth := 1;
                        FBitDepth := mcmTWAIN.BitDepth;
                   end;
                 end;
  TWPT_GRAY,
  TWPT_PALETTE : begin
                   if (FBitDepth <> 8)
                   then begin
                        mcmTWAIN.BitDepth := 8;
                        FBitDepth := mcmTWAIN.BitDepth;
                   end;
                 end;
  TWPT_RGB     : begin
                   if (FBitDepth = 24)
                   then FBitDepth := 8; // Should be 8 (bits per channel).
                   if (FBitDepth <> 8)
                   then begin
                        mcmTWAIN.BitDepth := 8;
                        FBitDepth := mcmTWAIN.BitDepth;
                   end;
                 end;
  end;

  // Did we get the requested bit depth ?
  if (FBitDepth in [1,8])
  then ; //

  // Get/Set current pixel flavor.
  if (mcmTWAIN.PixelFlavor <> TWPF_CHOCOLATE)
  then mcmTWAIN.PixelFlavor := TWPF_CHOCOLATE;

  // RGB Pixel data arrangement.
  if (FColorFormat = TWPT_RGB)
  then begin
       // We want the bitmap data returned as "RGBRGB..." not "RRR..GGG..BBB..".
       if (mcmTWAIN.PlanarChunky <> TWPC_CHUNKY)
       then mcmTWAIN.PlanarChunky := TWPC_CHUNKY;
  end;

  if (FColorFormat in [TWPT_BW{,TWPT_GRAY,TWPT_PALETTE}])
  then begin
       FBitReduction := -1;
       // Most commenly used with Black & White images.
       // Therefore, support for TWPT_GRAY and TWPT_PALETTE may not be available!
       if mcmTWAIN.IsCapSupported(ICAP_BITDEPTHREDUCTION)
       then begin
            Container := Nil;
            if (mcmTWAIN.GetCapabilityMsg(ICAP_BITDEPTHREDUCTION, MSG_GET, Container) = TWRC_SUCCESS)
            then begin
                 case FColorFormat of
                 TWPT_BW      : FBitReduction := TWBR_THRESHOLD;
                 //TWPT_GRAY    : FBitReduction := TWBR_DIFFUSION;
                 //TWPT_PALETTE : FBitReduction := TWBR_HALFTONE;
                 end;
                 if (FBitReduction <> Container.CurrentValue)
                 then begin
                      Container.CurrentValue := FBitReduction;
                      if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                      then mcmTWAIN.GetCapabilityMsg(ICAP_BITDEPTHREDUCTION, MSG_GET, Container);
                      FBitReduction := Container.CurrentValue;
                 end;
            end;
       end;

       // Threshold or Half-tones? Above Threshold was selected!
       case FBitReduction of
       TWBR_THRESHOLD    : if mcmTWAIN.IsCapSupported(ICAP_THRESHOLD)
                           then begin // Threshold method was selected.
                                Container := Nil;
                                if (mcmTWAIN.GetCapabilityMsg(ICAP_THRESHOLD, MSG_GET, Container) = TWRC_SUCCESS)
                                then begin
                                     Container.CurrentValue := 128;
                                     if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                                     then ; // Current value was changed to default item in the list.
                                end;
                           end;
       TWBR_HALFTONE     : if mcmTWAIN.IsCapSupported(ICAP_HALFTONES)
                           then begin // Half tone method was selected.
                                Container := Nil;
                                if (mcmTWAIN.GetCapabilityMsg(ICAP_HALFTONES, MSG_GET, Container) = TWRC_SUCCESS)
                                then begin
                                     // The current halftone is available through Container.CurrentValue.
                                     if (Container.NumItems >= 1)
                                     then begin
                                          // To iterate through items in a container,
                                          //for i := 0 to (Container.NumItems - 1)
                                          //do ShowMessage('Item[' + IntToStr(i) + '] := ' + Container.Items[i]);
                                     end;
                                     // We'll selected the source default.
                                     if (Container.CurrentIndex <> Container.DefaultIndex)
                                     then begin
                                          Container.CurrentIndex := Container.DefaultIndex;
                                          if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                                          then ; // Current value was changed to default item in the list.
                                     end;
                                end;
                           end;
       TWBR_CUSTHALFTONE : ;
       TWBR_DIFFUSION    : ;
       end;
  end;

  //----------------------------------------------------------------------------
  // Negotiate dimensions.

  Resolution := 150.0; // We'll try 150 dpi.

  // Get X resolution.
  mcmTWAIN.XResolution := Resolution;
  xRes := mcmTWAIN.XResolution;
  if (xRes <> Resolution)
  then begin
       // Lets check that the resolution is in range.
       Container := mcmTWAIN.Containers.Items[ICAP_XRESOLUTION];
       if Assigned(Container)
       then if (Container.ContainerType <> TWON_ONEVALUE)
            then begin
                 if (Container.MinValue <= Resolution) and (Resolution <= Container.MaxValue)
                 then mcmTWAIN.XResolution := Resolution // Our choice was OK
                 else if (Container.MinValue > Resolution)
                      then mcmTWAIN.XResolution := Container.MinValue // Nop, it's too small
                      else if (Resolution > Container.MaxValue)
                           then mcmTWAIN.XResolution := Container.MaxValue; // Nop, it's too big
       end
       else mcmTWAIN.XResolution := Resolution;
       xRes := mcmTwain.XResolution;
       if (xRes = -1)
       then begin
            Container := Nil;
            mcmTWAIN.GetCapabilityMsg(ICAP_XRESOLUTION, MSG_RESET, Container);
            if (Container <> Nil)
            then xRes := Container.CurrentValue;
       end;

       // Had we just set "Resolution" to mcmTWAIN.XResolution and this value was
       // outside the range, mcmTWAIN would override the choice and use the
       // CurrentValue. Note that the same goes of all capabilities negotiated.
  end;

  // Get Y resolution.
  mcmTWAIN.YResolution := Resolution;
  yRes := mcmTWAIN.YResolution;
  if (yRes <> Resolution)
  then begin
       // Lets check that the resolution is in range.
       Container := mcmTWAIN.Containers.Items[ICAP_YRESOLUTION];
       if Assigned(Container)
       then if (Container.ContainerType <> TWON_ONEVALUE)
            then begin
                 if (Container.MinValue <= Resolution) and (Resolution <= Container.MaxValue)
                 then mcmTWAIN.YResolution := Resolution // Our choice was OK
                 else if (Container.MinValue > Resolution)
                      then mcmTWAIN.YResolution := Container.MinValue // Nop, it's too small
                      else if (Resolution > Container.MaxValue)
                           then mcmTWAIN.YResolution := Container.MaxValue; // Nop, it's too big
       end
       else mcmTWAIN.YResolution := Resolution;
       yRes := mcmTwain.YResolution;
       if (yRes = -1)
       then begin
            Container := Nil;
            mcmTWAIN.GetCapabilityMsg(ICAP_YRESOLUTION, MSG_RESET, Container);
            if (Container <> Nil)
            then yRes := Container.CurrentValue;
       end;
  end;
  Container := Nil;

  // Negotiate X & Y Scaling to default 1.0.
  if (mcmTWAIN.XScaling <> 1.0)
  then mcmTWAIN.XScaling := 1.0;
  if (mcmTWAIN.YScaling <> 1.0)
  then mcmTWAIN.YScaling := 1.0;

  //----------------------------------------------------------------------------
  // NOTE: If required negotiate Zoom factor here.

  //----------------------------------------------------------------------------
  // Negotiate Auto Brightness.
  if mcmTWAIN.IsCapSupported(ICAP_AUTOBRIGHT)
  then mcmTWAIN.AutoBrightness := false;

  //----------------------------------------------------------------------------
  // NOTE: If required negotiate Brightness, Contract etc. here
  if Not(mcmTWAIN.AutoBrightness) and mcmTWAIN.IsCapSupported(ICAP_BRIGHTNESS)
  then mcmTWAIN.Brightness := 0; // Set neutral value

  if mcmTWAIN.IsCapSupported(ICAP_CONTRAST)
  then mcmTWAIN.Contrast := 0; // Set neutral value

  //----------------------------------------------------------------------------
  // We'll disable undefined images size, automatic boarder detection, rotate
  // and deskew!

  if mcmTwain.IsCapSupported(ICAP_UNDEFINEDIMAGESIZE)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_UNDEFINEDIMAGESIZE, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            Container.CurrentValue := False;
            if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
            then { An Error occured. }
            else begin
                 if (mcmTWAIN.GetCapabilityMsg(ICAP_UNDEFINEDIMAGESIZE, MSG_GET, Container) = TWRC_SUCCESS)
                 then begin
                      if (Container.CurrentValue = True)
                      then begin
                           if mcmTwain.IsCapSupported(ICAP_AUTOMATICBORDERDETECTION)
                           then begin
                                Container := Nil;
                                if (mcmTWAIN.GetCapabilityMsg(ICAP_AUTOMATICBORDERDETECTION, MSG_GET, Container) = TWRC_SUCCESS)
                                then begin
                                     Container.CurrentValue := False;
                                     if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                                     then { An Error occured. } ;
                                end;
                           end;
                      end;
                 end;
            end;
       end;
  end;

  if mcmTwain.IsCapSupported(ICAP_AUTOMATICDESKEW)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_AUTOMATICDESKEW, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            if (Container.CurrentValue <> False)
            then begin
                 Container.CurrentValue := False;
                 if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then { An Error occured. } ;
            end;
       end;
  end;

  if mcmTwain.IsCapSupported(ICAP_AUTOMATICROTATE)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_AUTOMATICROTATE, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            if (Container.CurrentValue <> False)
            then begin
                 Container.CurrentValue := False;
                 if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then { An Error occured. } ;
            end;
       end;
  end;

  //----------------------------------------------------------------------------
  // Negotiate paper size to an A4.
  if (mcmTWAIN.PageType <> TWSS_A4LETTER)  // See TWAIN.PAS for other fixed sizes.
  then mcmTWAIN.PageType := TWSS_A4LETTER; // f.ex. TWSS_USLETTER, TWSS_USLEGAL

  //----------------------------------------------------------------------------
  // Negotiate the Image layout her !
  // Remember to negotiate Unit and Resolution first, as above.

  // You should do this only after negotiating ADF, Units and resolution and
  // color.
  // We are requesting the a quater of the maximum area.

  mcmTWAIN.ResetImageLayout;
  ImageLayout := mcmTWAIN.GetImageLayout;
  with ImageLayout
  do begin
     // Set frame layout to an A4 size
     Frame.Left   := 0;
     Frame.Top    := 0;
     Frame.Right  := 8.2677;
     Frame.Bottom := 11.6929;

     if (yMin < (Frame.Bottom - Frame.Top))
     then ; // Correct for scanners minimum scan height!

     if (xMin < (Frame.Right - Frame.Left))
     then ; // Correct for scanners minimum scan height!

     // You could use xMax and yMax inquired earlier to ensure that the
     // requested frame is within limits.
     if (xMax > 0.0)
     then if (Frame.Right > xMax)
          then Frame.Right := xMax;
     if (yMax > 0)
     then if (Frame.Bottom > yMax)
          then Frame.Bottom := yMax;
  end;
  mcmTWAIN.SetImageLayout(ImageLayout);

  //----------------------------------------------------------------------------
  // Negotiate Frames!
  // Remember to negotiate Unit and Resolution first, as above.
  (*
  if mcmTwain.IsCapSupported(ICAP_MAXFRAMES) and
     (mcmTWAIN.DSResult <> TWRC_SUCCESS) // <- Setting ImageLayout didn't succeed.
  then begin
       // if data source provides more frames that app is willing to handle
       // then set the number of frames.
       mcmTWAIN.MaxFrames := 1;

       // Get maximum number of frames that the data source can provide.
       if (mcmTWAIN.MaxFrames > 0)
       then begin
            // Set-up one frame.
            Container := mcmTWAIN.GetFrames;
            if (Container <> Nil)
            then begin
                 Container.NumItems := 1;
                 Container.Frames[0].Left   := 0.0;
                 Container.Frames[0].Top    := 0.0;
                 Container.Frames[0].Right  := 8.27;
                 Container.Frames[0].Bottom := 11.69;
                 mcmTWAIN.SetFrames(Container);
            end;
       end;
  end;
  *)

  //----------------------------------------------------------------------------
  // Negotiate Orientation / Rotation.

  // Negotiating Orientation and/or Rotation shall be done after considering
  // the DAT_IMAGELAYOUT and ICAP_FRAMES.
  i := mcmTWAIN.Orientation;
  if (i <> TWOR_ROT0)
  then mcmTWAIN.Orientation := TWOR_ROT0;

  if mcmTwain.IsCapSupported(ICAP_FLIPROTATION)
  then begin
       Container := Nil;
       if (mcmTWAIN.GetCapabilityMsg(ICAP_FLIPROTATION, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            if (Container.CurrentValue <> TWFR_BOOK) // Alternativ to TWFR_BOOK is TWFR_FANFOLD
            then begin
                 Container.CurrentValue := TWFR_BOOK;
                 if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then { An Error occured. } ;
            end;
       end;
       Container := Nil;
  end;

  r := mcmTWAIN.Rotation;
  if (r <> 0.0)
  then mcmTWAIN.Rotation := 0.0;

  (*
  if (mcmTWAIN.XferMech = TWFX_FILES) or (mcmTWAIN.XferMech = TWFX_MEMORY)
  then begin
       // Might want to check that file format is set to TIFF.
       //...
       if mcmTWAIN.IsCapSupported(ICAP_COMPRESSION)
       then begin
            Container := Nil;
            if (mcmTWAIN.GetCapabilityMsg(ICAP_COMPRESSION, MSG_GET, Container) = TWRC_SUCCESS)
            then begin
                 Container.CurrentValue := TWCP_GROUP4;
                 if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) = TWRC_SUCCESS)
                 then begin
                      Container := Nil;
                      if (mcmTWAIN.GetCapabilityMsg(ICAP_CCITTKFACTOR, MSG_GET, Container) = TWRC_SUCCESS)
                      then begin
                           Container.CurrentValue := 4; // valid values are 0..(2^16) - 1.
                           if (mcmTWAIN.SetCapabilityMsg(MSG_SET, True, Container) <> TWRC_SUCCESS)
                           then ; // Didn't succeed!
                      end;
                 end;
            end;
       end;
  end;
  *)

  //----------------------------------------------------------------------------
  // Just for fun - let's set the Author of the acquisition
  if mcmTWAIN.IsCapSupported(CAP_AUTHOR)
  then begin
       // If you do not have a fully initialized Container, make sure to set
       // it to Nil before calling GetCapabilityMsg. GetCapabilityMsg will
       // return a container holding the Cap's data. Note, that any Container
       // returned by mcmTWAIN is maintained by mcmTWAIN Container list.
       Container := Nil;

       // GetCapabilityMsg returnes TWRC_SUCCESS if the capability was obtained
       // successfully. If the returned value is different from TWRC_SUCCESS
       // the Container is not valid.
       if (mcmTWAIN.GetCapabilityMsg(CAP_AUTHOR, MSG_GET, Container) = TWRC_SUCCESS)
       then begin
            // Authors name is in ->
            // Container.CurrentValue - a string;
       end;

       // After calling Containers.DeleteItem the Container holding CAP_AUTHOR
       // is not vaild. Note, you do not have to delete the container. This
       // will be done automatically when the session is closed.
       mcmTWAIN.Containers.DeleteItem(CAP_AUTHOR);
  end;

  if FADFAvailable
  then begin
        // Control the number of pages to transfer. In the case transfer
        // we want all available images use "-1".
        mcmTWAIN.NumImagesToScan(-1);

        if mcmTWAIN.AutoScan(True)
        then begin
             mcmTWAIN.MaxBatchBuffers := 1;
             if (mcmTWAIN.MaxBatchBuffers > 0)
             then mcmTWAIN.ClearBatchBuffers := TWCB_CLEAR; // TWCB_AUTO, TWCB_NOCLEAR
        end;
  end;
end; // TFormTWAIN.mcmTWAINNegotiation.


{$IFNDEF VER100}

procedure TFormTWAIN.mcmTWAINImageReady(Sender   : TObject;
                                        pBmp     : Pointer;
                                        pBmpInfo : PBitmapInfo;
                                        hImage   : hBitmap;
                                        FileName : string);

{$ELSE}

procedure TFormTWAIN.mcmTWAINImageReady(Sender   : TObject;
                                        pBmp     : Pointer;
                                        pBmpInfo : PBitmapInfo;
                                        hImage   : integer;
                                        FileName : string);
{$ENDIF}

var JPEGImage : TJPEGImage;
begin
//------------------------------------------------------------------------------
// Image is available.
// -------------------
// This event is fired after the Data Source has transferred the complete image
// to the mcmTWAIN component.
// This event returns the acquired image.
//------------------------------------------------------------------------------
  if (hImage <> 0)
  then begin
       if (mcmTWAIN.DIBHandleType <> THDT_DIBRAW)
       then begin
            // If you want to use the pBmpInfo pointer, make a copy!
            // The pBmpInfo pointer is only valid in this event procedure.
            // This pointer is only created when THDT_DIBSEC is chosen in
            // DIBHandleType.
            with Image1.Picture
            do begin
               // Do not set any properties on the TImage until after the assignment
               //   Bitmap.Handle := hImage;
               // Especially, omit doing
               //   Bitmap.Width  := mcmTWAIN.ImageWidth;
               //   Bitmap.Height := mcmTWAIN.ImageHeight;
               // as this will increase memory usage unnecessaryly.
               Bitmap.Handle := hImage;
            end;
       end
       // This example does not support the THDT_DIBRAW format.
       // In THDT_DIBRAW format, the hImage containes the BitmapInfo, i.e.
       // (BitmapInfoHeader and RGBQUAD palette) followed by the bitmap data.
       else GlobalFree(hImage);

       StatusBar1.SimpleText := 'Image was received in a memory Handle';
  end
  else begin // A file name was returned.
       StatusBar1.SimpleText := 'Image was received in file: '  + FileName;
       if FileExists(FileName)
       then begin
            if (Pos('.bmp', lowercase(FileName)) = Length(FileName) - 3)
            then Image1.Picture.Bitmap.LoadFromFile(FileName)
            else if (Pos('.jpg', lowercase(FileName)) = Length(FileName) - 3)
                 then begin
                      JPEGImage := TJPEGImage.Create;
                      try
                        JPEGImage.LoadFromFile(FileName);
                        JPEGImage.DIBNeeded;
                        Image1.Picture.Bitmap.Width  := JPEGImage.Width;
                        Image1.Picture.Bitmap.Height := JPEGImage.Height;
                        Image1.Picture.Bitmap.HandleType := bmDIB;
                        Image1.Picture.Bitmap.Canvas.Draw(0, 0, JPEGImage);
                      except
                      end;
                      JPEGImage.Free;
                 end
                 //-------------------------------------------------------------
                 // To read tiff images you must supply your own import filter!
                 else if (Pos('.tif', lowercase(FileName)) = Length(FileName) - 3)
                      then ;
       end;
  end;
  AdjustImageSize;
  Repaint;
end; // TFormTWAIN.mcmTWAINImageReady.


procedure TFormTWAIN.mcmTWAINXferReady(Sender : TObject);
var ImageInfo : TImageInfo;
begin
//------------------------------------------------------------------------------
// Image transfer is ready.
// ------------------------
// This event is fired just before the data source begins to transfer an image.
// Use this event to obtain information on the image about to be transferred.
//------------------------------------------------------------------------------
  // Get image information.
  ImageInfo := mcmTWAIN.GetImageInfo;
end; // TFormTWAIN.mcmTWAINXferReady.


procedure TFormTWAIN.mcmTWAINXferNext(Sender : TObject; var NumImages : Integer; var SkipNext : boolean);
var FileExt : string;
begin
//------------------------------------------------------------------------------
// Next image transfer.
// --------------------
// This event is fired before the data source transfers the next image.
// Use this event to decide whether this next image should be scanned, skipped
// or to cancel scanning additional images.
//------------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // (ADF). More images are available in current session.
  // To stop acquisition set NumImages to zero.
  // If NumImages > 0 the data source knows how many images/pages are left to
  // scan.
  // If NumImages < 0 the data source doesn't knows how many images/pages that
  // are left to scan, just that there are more.
  // If NumImages = 0 the data source has transferred all images.
  //----------------------------------------------------------------------------

  if (NumImages <> 0)
  then begin
       case MessageDlg('Number of images: ' + IntToStr(NumImages) + chr($0D) +
                       'Get next image ?', mtConfirmation, [mbYes, mbNo, mbAbort], 0) of
       mrYes   : ;
       mrNo    : SkipNext := True;
       mrAbort : NumImages := 0;
       end;
  end;

  //----------------------------------------------------------------------------
  // When transferring multiple images in file mode, change the file name here.
  // You should only do so, if the transfer is done without user interaction.
  // mcmTWAIN.Filename := '.\newname.ext';
  if (mcmTWAIN.XferMech = TWFX_Files) and ((NumImages > 0) or (NumImages = -1))
  then begin
       // Change the filename, but re-use the extension.
       FileExt := ExtractFileExt(mcmTWAIN.Filename);
       inc(FImageIndex);
       FImageFileName := '.\image' + IntToStr(FImageIndex) + FileExt;
       mcmTWAIN.Filename := FImageFileName;
  end;
end; // TFormTWAIN.mcmTWAINXferNext.


procedure TFormTWAIN.mcmTWAINDeviceEvent(Sender     : TObject;
                                         Event      : TTwnDeviceEvent;
                                         DeviceName : String;
                                         A, B, C    : Variant);
var MsgStr    : string;
    Container : TtwnContainer;
begin
//------------------------------------------------------------------------------
// Device Event.
// --------------------
// Depending on the "Event" that triggered mcmTWAINDeviceEvent to fire
// A, B and C represents different field of the original TW_DEVICEEVENT
// structure.
// Only TWDE_CHECKAUTOMATICCAPTURE, TWDE_CHECKBATTERY, TWDE_CHECKFLASH,
// TWDE_CHECKPOWERSUPPLY and TWDE_CHECKRESOLUTION returns values in A, B and C.
// For in-depth information please refer to the TWAIN Specifications (Look for
// CAP_DEVICEEVENT, TW_DEVICEEVENT and TWAIN Articals - Device Events) and view
// example code below.
//
// The Events which you want your application to receive from the source are
// specified in the property mcmTWAIN.DeviceEventTypes. By default this set
// is empty = [] resulting in no events.
//
// Note that all device events only occurres when the device status changes or
// as a result of user interaction.
// An event does(should) never occure as a result of the application changing
// properties in the TWAIN driver.
//------------------------------------------------------------------------------

  case Event of
  TWDE_CHECKAUTOMATICCAPTURE : begin
                                 // A = longword - AutomaticCapture - No. of images the
                                 //     Camera will capture.
                                 // B = longword - TimeBeforeFirstCapture.
                                 // C = longword - TimeBetweenCaptures.
                                 MsgStr := 'Check - Automatic capture settings changed.' + chr($0D) +
                                           'Images to capture: ' + IntToStr(A) + chr($0D) +
                                           'Sec. to first capture: ' + IntToStr(B) + chr($0D) +
                                           'Hundreths of a seconds between captures: ' + IntToStr(C);
                               end;
  TWDE_CHECKBATTERY          : begin
                                 // A = longword - BatteryMinutes.
                                 // B = integer - BatteryPercentage.
                                 // Note that the data source will fill in
                                 // the value A or B it supports.
                                 MsgStr := 'Check - Battery status changed.' + chr($0D);
                                 if (A <> 0)
                                 then MsgStr := MsgStr + 'Battery minutes: ' + IntToStr(A)
                                 else MsgStr := MsgStr + 'Battery percentage: ' + IntToStr(B);
                               end;
  TWDE_CHECKDEVICEONLINE     : begin
                                 MsgStr := 'Device has been turned ';
                                 // Use CAP_DEVICEONLINE for more information.
                                 Container := Nil;
                                 if (mcmTWAIN.GetCapabilityMsg(CAP_DEVICEONLINE, MSG_GET, Container) = TWRC_SUCCESS)
                                 then begin
                                      if Container.CurrentValue
                                      then MsgStr := MsgStr + 'ON'
                                      else MsgStr := MsgStr + 'OFF';
                                 end
                                 else MsgStr := MsgStr + 'ON/OFF ?';
                               end;
  TWDE_CHECKFLASH            : begin
                                 // A = longword - FlashUsed status.
                                 MsgStr := 'Flash has been turned ';
                                 case A of
                                 TWFL_NONE   : MsgStr := MsgStr + 'None.';
                                 TWFL_OFF    : MsgStr := MsgStr + 'OFF.';
                                 TWFL_ON     : MsgStr := MsgStr + 'ON.';
                                 TWFL_AUTO   : MsgStr := MsgStr + 'to Auto.';
                                 TWFL_REDEYE : MsgStr := MsgStr + 'Redeye.';
                                 end;
                                 // If necessary use ICAP_FLASHUSED2 to modify
                                 // setting.
                               end;
  TWDE_CHECKPOWERSUPPLY      : begin
                                 // The power supply changed.
                                 // A = Power supply being used.
                                 MsgStr := 'Power supply has changed to ';
                                 if (A = TWPS_EXTERNAL)
                                 then MsgStr := MsgStr + 'AC'
                                 else MsgStr := MsgStr + 'Battery';
                               end;
  TWDE_CHECKRESOLUTION       : begin
                                 // A = double - Current X resolution.
                                 // B = double - Current Y resolution.
                                 MsgStr := 'Current resolution has been changed to' + chr($0D) +
                                           'X: ' + FloatToStrF(A, ffFixed, 8, 4) + chr($0D) +
                                           'Y: ' + FloatToStrF(B, ffFixed, 8, 4);
                                 // If necessary use ICAP_XRESOLUTION and
                                 // ICAP_XRESOLUTION.
                               end;
  TWDE_DEVICEADDED           : MsgStr := 'Device added (Memory card, etc.)';
  TWDE_DEVICEOFFLINE         : MsgStr := 'Device is unavailable.';
  TWDE_DEVICEREADY           : MsgStr := 'Device is ready to acquire an image.';
  TWDE_DEVICEREMOVED         : MsgStr := 'Device has been removed.';
  TWDE_IMAGECAPTURED         : MsgStr := 'Image has been captured.';
  TWDE_IMAGEDELETED          : MsgStr := 'Image has been deleted.';
  TWDE_PAPERDOUBLEFEED       : MsgStr := '?';
  TWDE_PAPERJAM              : MsgStr := 'Paper jam.';
  TWDE_LAMPFAILURE           : MsgStr := 'Light source failed.';
  TWDE_POWERSAVE             : MsgStr := 'Device powered down to save energy.';
  TWDE_POWERSAVENOTIFY       : MsgStr := 'Device is about to power down.';
  else MsgStr := 'Unknown Device Event!';
  end;

  // In this example it has been chosen to display the event-message.
  // In most cases you should not display a message but use the information
  // to obtain the desired functionality.
  MessageDlg(DeviceName + chr($0D) + 'fired the following event:' + chr($0D) +
             MsgStr, mtInformation, [mbOK], 0);
end; // TFormTWAIN.mcmTWAINDeviceEvent.


procedure TFormTWAIN.SourceInfoItemClick(Sender : TObject);
begin
  // Display Data Source information.
  // mcmTWAIN.SourceInfo returns the information on either the opened data
  // source or the default (last selected).
  FormSrcInfo := TFormSrcInfo.Create(self);
  FormSrcInfo.SourceInfo := mcmTWAIN.SourceInfo;
  FormSrcInfo.ShowModal;
  FormSrcInfo.Free;
end; // TFormTWAIN.SourceInfoItemClick.


procedure TFormTWAIN.mcmTWAINMemXferSize(    Sender   : TObject;
                                             MinSize,
                                             MaxSize  : Integer;
                                         var BufSize  : Integer;
                                             pBmpInfo : PBitmapInfo);
var LongWidth  : integer;
    BufferSize : integer;
    FNoLines   : integer;
begin
  // Set-up number of lines/bytes to (Memory) transfer in each image chunk.
  with pBmpInfo^.bmiHeader
  do LongWidth := (((Longint(biWidth * biBitCount) + 31) div 32) * 4);
  FNoLines  := pBmpInfo^.bmiHeader.biHeight div 4; // Number of lines to transfer.
  BufferSize := FNoLines * LongWidth;

  // Validate that BufferSize satisfy the condition specified by the data source,
  // i.e. MinSize <= BufferSize <= MaxSize.
  if (BufferSize < MinSize)
  then begin
       FNoLines := Trunc(0.9999 + MinSize / LongWidth);
       BufferSize := FNoLines * LongWidth;
  end;

  if (BufferSize > MaxSize) and (MaxSize <> integer(TWON_DONTCARE32))
  then begin
       FNoLines := Trunc(MaxSize / LongWidth);
       BufferSize := FNoLines * LongWidth;
  end;

  BufSize := BufferSize;
end; // TFormTWAIN.mcmTWAINMemXferSize.


procedure TFormTWAIN.mcmTWAINFailure(Sender        : TObject;
                                     DG            : longint;
                                     DAT, CAP, MSG : Word;
                                     Error, Status : Integer);
begin
  if (Error = TWRC_EXCEPTION)
  then ShowMessage('A fatal error (exception) occured in the driver: ' +
                   mcmTWAIN.SourceInfo.ProductName);

  if (DG in [DG_CONTROL, DG_IMAGE])
  then begin
       case DAT of
       DAT_CAPABILITY : case CAP of
                        ICAP_PIXELTYPE : ShowMessage('Something went wrong!');
                        end;
       end;
  end;
end; // TFormTWAIN.mcmTWAINFailure.



procedure TFormTWAIN.ConfigureSourceItemClick(Sender: TObject);
begin
  if Not(mcmTWAIN.ConfigureSource(SourceList.Text))
  then ShowMessage('Could not open data source for configuration!');
end;

end.
