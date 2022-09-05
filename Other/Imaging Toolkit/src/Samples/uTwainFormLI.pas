{------------------------------------------------------------------------------}
{ MCM DESIGN                                                                   }
{                                                                              }
{ For further information / comments, visit our WEB site at                    }
{   www.mcm-design.com                                                         }
{ or e-mail to                                                                 }
{   CustomerCare@mcm-design.dk                                                 }
{------------------------------------------------------------------------------}
{}
{ $Log:  15912: uTwainFormLI.pas 
//
//    Rev 1.4    2014-01-15 13:42:02  mcm
// Added support for XE2, XE3, XE4 and XE5.
// Fixed unicode/pchar problems in the data source. 
//
//    Rev 1.3    2013-12-04 23:16:16  mcm    Version: DT 4.0
// Support for Delphi XE2
// Internal threads are limited to only run during a TWAIN session. Resolved
// compiler warnings using deprecated methods 
//
//    Rev 1.2    26-08-2009 22:38:16  mcm    Version: DT 3.9
// Fixed unicode issues (PChar -> PAnsiChar)
//
//   Rev 1.1    03-01-2005 18:35:18  mcm    Version: DT 3.3
// Added support for Delphi 2005.

//
//   Rev 1.0    04-12-2001 16:49:12  mcm    Version: DT 2.0

{
{   Rev 1.3    11-12-00 12:31:49  mcm    Version: 1.9.1
{ Updated mcmTWAINXferNext event to omit showing the 
{ MessageDlg if there are no more pending transfers.
}
{
{   Rev 1.2    08-06-00 18:31:35  mcm    Version: 1.9.0
{ Added typecast to TWON_DONTCARE32.
}
{
{   Rev 1.1    04-06-00 22:05:57  mcm    Version: 1.9.0
{ Updated for version 1.9
}
{
{   Rev 1.0    09-04-00 15:30:56  mcm    Version: 1.8.4
{ Initial version
}
{}
unit uTwainFormLI;

{$INCLUDE mcmDefines.pas}

interface

//------------------------------------------------------------------------------
// This example shows how to use the combination of XferMech = TWFX_Memory and
// DIBHandleType = THDT_MEMPTR. Note, the DIBHandleType can only be set to
// THDT_MEMPTR when XferMech equals TWFX_Memory.
// Using this set-up allows you to acquire extremly large images without having
// to allocat memory for the entire image.
// This is of interest if you do not require the entire image, but rather need
// portions of the image for processing / extraction of data contained in the
// image. Here returned images are simply converted to gray scale.
// The required memory to transfer chunks of the image, is negotiated in the
// OnMemXferSize event procedure.
// The actual image chunks are returned via the OnMemXferBuffer event.
// Fore more information, see comments included in these two event procedure
// below.
//
// Very important: The returned image chunks are not directly compatible with
// Windows Bitmap's.
// When transferring RGB images, pixels are (in TWPC_CHUNKY mode) transferred
// R G B R G B and not as the Windows Bitmap format where pixels are store B G R
// B G R.

{$IFDEF GE_DXE2} {$DEFINE GE_DXE} {$ENDIF}

uses {$IFDEF GE_DXE2}
     WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes,
     Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.Controls,
     Vcl.Buttons, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.jpeg,
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
     Menus, Buttons, ComCtrls, StdCtrls, ExtCtrls, JPEG,
     {$ENDIF}
     twain, mcmTWAIN, mcmTWAINKernel, mcmTWAINIntf;

type
  TFormTWAIN           = class(TForm)
    mcmTWAIN           : TmcmTWAIN;
    StatusBar1         : TStatusBar;
    Image1             : TImage;

    Panel1             : TPanel;
    sbAcquire          : TSpeedButton;
    sbSource           : TSpeedButton;

    MainMenu1          : TMainMenu;
    FileMenu           : TMenuItem;
    Acquire1           : TMenuItem;
    Source1            : TMenuItem;
    N1                 : TMenuItem;
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
    AboutItem          : TMenuItem;
    NegotiateItem: TMenuItem;

    procedure FormCreate             (    Sender    : TObject);
    procedure SourceClick            (    Sender    : TObject);
    procedure AcquireClick           (    Sender    : TObject);
    procedure ErrorLevelClick        (    Sender    : TObject);
    procedure ShowUIItemClick        (    Sender    : TObject);
    procedure Exit1Click             (    Sender    : TObject);
    procedure mcmTWAINEnableMenus    (    Sender    : TObject);
    procedure mcmTWAINDisableMenus   (    Sender    : TObject);
    procedure AboutItemClick         (    Sender    : TObject);
    procedure mcmTWAINXferReady      (    Sender    : TObject);
    procedure mcmTWAINXferNext       (    Sender    : TObject;
                                      var NumImages : Integer;
                                      var SkipNext  : boolean);
    procedure FormResize             (    Sender    : TObject);
    procedure StretchImageItemClick  (    Sender    : TObject);
    procedure DisableAfterItemClick  (    Sender    : TObject);
    procedure FileMenuClick          (    Sender    : TObject);
    procedure ErrorLevel1Click       (    Sender    : TObject);
    procedure EnableADFItemClick     (    Sender    : TObject);
    procedure EnableAutoFeedItemClick(    Sender    : TObject);
    procedure mcmTWAINMemXferSize    (    Sender    : TObject;
                                          MinSize,
                                          MaxSize   : Integer;
                                      var BufSize   : Integer;
                                          pBmpInfo  : PBitmapInfo);
    procedure mcmTWAINMemXferBuffer (     Sender    : TObject;
                                          pBmpInfo  : PBitmapInfo;
                                          BytesPerRow,
                                          Rows,
                                          DataSize  : Integer;
                                      var pData     : Pointer);
    procedure NegotiateItemClick(Sender: TObject);
    procedure mcmTWAINNegotiation(Sender: TObject;
      var CancelScan: Boolean);
  private
    { Private declarations }
    FOddTrans : boolean;
    FNoLines  : integer;
    FThisLine : integer;
    hBuffer   : THandle;
    pBuffer   : pointer;
    procedure AdjustImageSize;
    procedure ShowHint               (    Sender    : TObject);
    function  CreateGrayPal : HPALETTE;
  public
    { Public declarations }
  end;

var FormTWAIN : TFormTWAIN;

implementation

uses uTwnDlgSend;

{$R *.DFM}
{$IFOPT X-} {$X+} {$ENDIF}

procedure TFormTWAIN.FormCreate(Sender : TObject);
begin
  Application.OnHint := ShowHint;
  hBuffer := 0;
end;

procedure TFormTWAIN.ShowHint(Sender : TObject);
begin
  StatusBar1.SimpleText := Application.Hint;
end;

procedure TFormTWAIN.FormResize(Sender: TObject);
begin
  AdjustImageSize;
end;

{------------------------------------------------------------------------------}
{ File menu.                                                                   }
{------------------------------------------------------------------------------}

procedure TFormTWAIN.FileMenuClick(Sender: TObject);
begin
  ShowUIItem.Checked       := mcmTWAIN.ShowUI;
  DisableAfterItem.Checked := mcmTWAIN.DisableAfterAcquire;
end;

procedure TFormTWAIN.SourceClick(Sender : TObject);
begin
  mcmTWAIN.SelectSource;
end;

procedure TFormTWAIN.AcquireClick(Sender : TObject);
begin
  mcmTWAIN.Acquire('');
end;

procedure TFormTWAIN.ErrorLevel1Click(Sender: TObject);
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
end;

procedure TFormTWAIN.ErrorLevelClick(Sender : TObject);
begin
  case (Sender as TMenuItem).Tag of
  0 : mcmTwain.MessageLevel := ML_None;
  1 : mcmTwain.MessageLevel := ML_Info;
  2 : mcmTwain.MessageLevel := ML_Error;
  3 : mcmTwain.MessageLevel := ML_Full;
  end;
end;

procedure TFormTWAIN.ShowUIItemClick(Sender : TObject);
begin
  ShowUIItem.Checked := Not(ShowUIItem.Checked);
  mcmTwain.ShowUI := ShowUIItem.Checked;
end;

procedure TFormTWAIN.DisableAfterItemClick(Sender: TObject);
begin
  mcmTWAIN.DisableAfterAcquire := Not(DisableAfterItem.Checked);
end;

procedure TFormTWAIN.NegotiateItemClick(Sender: TObject);
begin
  NegotiateItem.Checked := Not(NegotiateItem.Checked);
end;

procedure TFormTWAIN.EnableADFItemClick(Sender : TObject);
begin
  EnableADFItem.Checked := Not(EnableADFItem.Checked);
end;

procedure TFormTWAIN.EnableAutoFeedItemClick(Sender : TObject);
begin
  EnableAutoFeedItem.Checked := Not(EnableAutoFeedItem.Checked);
end;

procedure TFormTWAIN.StretchImageItemClick(Sender: TObject);
begin
  StretchImageItem.Checked := Not(StretchImageItem.Checked);
  AdjustImageSize;
  Repaint;
end;

procedure TFormTWAIN.Exit1Click(Sender : TObject);
begin
  // Close application.
  Close;
end;

{------------------------------------------------------------------------------}
{ About menu.                                                                  }
{------------------------------------------------------------------------------}

procedure TFormTWAIN.AboutItemClick(Sender: TObject);
begin
  mcmTWAIN.About;
end;

{------------------------------------------------------------------------------}
{ Adjust image size.                                                           }
{------------------------------------------------------------------------------}

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
            Image1.Align   := alNone;
            Image1.Stretch := True;

            ch := ClientHeight - StatusBar1.Height - Panel1.Height;
            cw := ClientWidth;

            ScaleH := cw / Image1.Picture.Bitmap.Width;
            ScaleV := ch / Image1.Picture.Bitmap.Height;

            if (ScaleH < ScaleV)
            then begin
                 ih := Round(ScaleH * Image1.Picture.Bitmap.Height);
                 Image1.Top := abs(ch - ih) div 2 + Panel1.Height;
                 if (Image1.Top < Panel1.Height)
                 then Image1.Top := Panel1.Height;
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
                 Image1.Top := Panel1.Height;
                 Image1.Height := Round(ScaleV * Image1.Picture.Bitmap.Height);
            end;
       end
       else begin
            Image1.Align := alClient;
            Image1.Stretch := False;
            Image1.Center := True;
       end;
  end;
end;

{------------------------------------------------------------------------------}
{ mcmTWAIN events.                                                             }
{------------------------------------------------------------------------------}

procedure TFormTWAIN.mcmTWAINEnableMenus(Sender : TObject);
begin
{------------------------------------------------------------------------------}
{ Enable TWAIN menus.                                                          }
{ -------------------                                                          }
{ This event is fired after the Data Source is closed.                         }
{ Use the OnDisableMenus event to disable menus etc.                           }
{------------------------------------------------------------------------------}
  Acquire1.Enabled  := True;
  Source1.Enabled   := True;
  sbSource.Enabled  := True;
  sbAcquire.Enabled := True;
end;

procedure TFormTWAIN.mcmTWAINDisableMenus(Sender : TObject);
begin
{------------------------------------------------------------------------------}
{ Disable TWAIN menus.                                                         }
{ --------------------                                                         }
{ This event is fired just before the Data Source is opened.                   }
{ Use the OnEnableMenus event to re-enable menus etc.                          }
{------------------------------------------------------------------------------}
  Acquire1.Enabled  := False;
  Source1.Enabled   := False;
  sbSource.Enabled  := False;
  sbAcquire.Enabled := False;
end;


procedure TFormTWAIN.mcmTWAINNegotiation(Sender: TObject; var CancelScan: Boolean);
var i           : integer;
    xMax, yMax  : real;
    ImageLayout : TImageLayout;
begin
  if Not(NegotiateItem.Checked)
  then Exit;

{------------------------------------------------------------------------------}
{ Negotiate capabilities.                                                      }
{ -----------------------                                                      }
{ This event is fired when the Data Source is opened, but before it becomes    }
{ enabled.                                                                     }
{ When you negotiate capabilities, you should alway check that the capability  }
{ is supported by calling IsCapSupported(CAP_xxxx).                            }
{ Unfortunatly some data sources promis just a bit more than they realy can    }
{ live up. Therefore it's a fairly good idear to GET the capability before     }
{ SETting it.                                                                  }
{                                                                              }
{ Below, you'll find many examples on how to change the data source settings.  }
{ You do not have to implement all or any of these negotiations, just the ones }
{ required by your application. But remember that some capabilities do depend  }
{ on others.                                                                   }
{------------------------------------------------------------------------------}

  {----------------------------------------------------------------------------}
  { Negotiate Color.                                                           }

  { Get/Set current pixel type.                                                }
  if mcmTwain.IsCapSupported(ICAP_PIXELTYPE)
  then begin
       i := mcmTwain.PixelType;
       if (i <> TWPT_RGB)
       then mcmTwain.PixelType := TWPT_RGB;
  end;

  { Get/Set current bit depth.                                                 }
  if mcmTwain.IsCapSupported(ICAP_BITDEPTH)
  then begin
       i := mcmTwain.BitDepth;
       if (i <> 24)
       then mcmTwain.BitDepth := 24;
  end;

  { Get/Set current bit order.                                                 }
  if mcmTwain.IsCapSupported(ICAP_BITORDER)
  then begin
       i := mcmTwain.BitOrder;
       if (i <> TWBO_MSBFIRST)
       then mcmTwain.BitOrder := TWBO_MSBFIRST;
  end;

  { Get/Set current pixel flavor.                                              }
  if mcmTwain.IsCapSupported(ICAP_PIXELFLAVOR)
  then begin
       i := mcmTwain.PixelFlavor;
       if (i <> TWPF_CHOCOLATE)
       then mcmTwain.PixelFlavor := TWPF_CHOCOLATE;
  end;

  if mcmTwain.IsCapSupported(ICAP_PLANARCHUNKY)
  then begin
       i := mcmTwain.PlanarChunky;
       if (i <> TWPC_CHUNKY)
       then mcmTwain.PlanarChunky := TWPC_CHUNKY;
  end;

  {----------------------------------------------------------------------------}
  { Negotiate dimensions.                                                      }
  { Get/Set current unit.                                                      }
  if mcmTwain.IsCapSupported(ICAP_UNITS)
  then begin
       i := mcmTwain.Units;
       if (i <> TWUN_INCHES)
       then mcmTwain.Units := TWUN_INCHES;
  end;

  { Negotiate max image size.                                                  }
  xMax := 8.0; // inch.
  if mcmTwain.IsCapSupported(ICAP_PHYSICALWIDTH)
  then xMax := mcmTWAIN.PhysicalWidth;

  yMax := 11.0; // inch.
  if mcmTwain.IsCapSupported(ICAP_PHYSICALHEIGHT)
  then yMax := mcmTWAIN.PhysicalHeight;

  {----------------------------------------------------------------------------}
  { Negotiate the Image layout her !                                           }
  { Remember to negotiate Unit and Resolution first, as above.                 }

  // You should do this only after negotiating ADF, Units and resolution.
  // I have just requesting the maximum area.
  ImageLayout := mcmTWAIN.GetImageLayout;
  with ImageLayout
  do begin
     Frame.Left   := Frame.Left;
     Frame.Top    := Frame.Top;
     Frame.Right  := Frame.Right;
     Frame.Bottom := Frame.Bottom;

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
end;


procedure TFormTWAIN.mcmTWAINXferReady(Sender : TObject);
var ImageInfo : TImageInfo;
begin
{------------------------------------------------------------------------------}
{ Image transfer is ready.                                                     }
{ ------------------------                                                     }
{ This event is fired just before the data source begins to transfer an image. }
{ Use this event to obtain information on the image about to be transferred.   }
{------------------------------------------------------------------------------}
  // Get image information.
  ImageInfo := mcmTWAIN.GetImageInfo;
end;


procedure TFormTWAIN.mcmTWAINXferNext(Sender : TObject; var NumImages : Integer; var SkipNext : boolean);
begin
{------------------------------------------------------------------------------}
{ Next image transfer.                                                         }
{ --------------------                                                         }
{ This event is fired before the data source transfers the next image.         }
{ Use this event to decide whether this next image should be scanned, skipped  }
{ or to cancel scanning additional images.                                     }
{------------------------------------------------------------------------------}

  //----------------------------------------------------------------------------
  // (ADF). More images are available in current session.
  // To stop acquisition set NumImages to zero.
  // If NumImages > 0 the data source knows how many images/pages are left to
  // scan.
  // If NumImages < 0 the data source doesn't knows how many images/pages that
  // are left to scan, just that there are more.
  //----------------------------------------------------------------------------
  if (NumImages > 0)
  then case MessageDlg('Number of images: ' + IntToStr(NumImages) + chr($0D) +
                       'Get next image ?', mtConfirmation, [mbYes, mbNo, mbAbort], 0) of
       mrYes   : ;
       mrNo    : SkipNext := True;
       mrAbort : NumImages := 0;
       end;

  //----------------------------------------------------------------------------
  // When transferring multiple images in file mode, change the file name here.
  // You should only do so, if the transfer is done without user interaction.
  // mcmTWAIN.Filename := '.\newname';
end;


function TFormTWAIN.CreateGrayPal : HPALETTE;
var Pal  : PLogPalette;
    hPal : HPALETTE;
    i    : integer;
begin
  Result := 0;
  Pal := nil;
  try
    GetMem(Pal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 256);
    Pal.palVersion := $300;
    Pal.palNumEntries := 256;
    for i := 0 to 255
    do begin
       Pal.palPalEntry[i].peRed   := i;
       Pal.palPalEntry[i].peGreen := i;
       Pal.palPalEntry[i].peBlue  := i;
    end;
    hPal := CreatePalette(Pal^);
    if hPal <> 0
    then Result := hPal;
  finally
    FreeMem(Pal);
  end;
end;


procedure TFormTWAIN.mcmTWAINMemXferSize(Sender: TObject; MinSize,
  MaxSize: Integer; var BufSize: Integer; pBmpInfo: PBitmapInfo);
var LongWidth : integer;
    ThisSize  : integer;
begin
{------------------------------------------------------------------------------}
{ Memory buffer size.                                                          }
{ -------------------                                                          }
{ This event is fired before the data source transfers the first chunk of      }
{ image data.                                                                  }
{ Use this event to specify the buffer size used when transferring the image.  }
{ The Buffer size "BufSize" must be greater or equal to MinSize, and less or   }
{ equal to MaxSize.                                                            }
{ Note:                                                                        }
{ 1. MaxSize may be equal to -1 (TWON_DONTCARE32), meaning that an             }
{ indeterminately large buffer is accepted.                                    }
{ 2. The data source may not care about any of these specification, in which   }
{ case these are set to -1 (TWON_DONTCARE32).                                  }
{------------------------------------------------------------------------------}

  // Set-up receiving bitmap.
  with pBmpInfo^.bmiHeader
  do begin
     Image1.Picture.Bitmap.Width  := biWidth;
     Image1.Picture.Bitmap.Height := biHeight;
     Image1.Picture.Bitmap.PixelFormat := pf8bit;

     // Get gray palette.
     Image1.Picture.Bitmap.Palette := CreateGrayPal;
  end;

  // Set-up number of lines/bytes to transfer in each image chunk.
  with pBmpInfo^.bmiHeader
  do LongWidth := (((Longint(biWidth * biBitCount) + 31) div 32) * 4);
  FNoLines  := 128;
  ThisSize := FNoLines * LongWidth;

  // Validate that ThisSize satisfiy the condition specified by the data source,
  // i.e. MinSize <= ThisSize <= MaxSize.
  if (ThisSize < MinSize)
  then begin
       FNoLines := Trunc(0.9999 + MinSize / LongWidth);
       ThisSize := FNoLines * LongWidth;
  end;
  if (ThisSize > MaxSize) and (MaxSize <> integer(TWON_DONTCARE32))
  then begin
       FNoLines := Trunc(MaxSize / LongWidth);
       ThisSize := FNoLines * LongWidth;
  end;

  // Create a buffer twice the size needed.
  // You could settle with creating a buffer of size ThisSize, but in this
  // example, we change where in the buffer data is returned after each
  // transfer (see below in mcmTWAINMemXferBuffer).
  hBuffer := GlobalAlloc(GMEM_FIXED, 2 * ThisSize);
  pBuffer := GlobalLock(hBuffer);

  BufSize := ThisSize;
end;


procedure TFormTWAIN.mcmTWAINMemXferBuffer(Sender: TObject;
  pBmpInfo: PBitmapInfo; BytesPerRow, Rows, DataSize: Integer;
  var pData: Pointer);
var i, j, k : integer;
    pSource : PAnsiChar;
    pDest   : PAnsiChar;
begin
{------------------------------------------------------------------------------}
{ Memory buffer ready.                                                         }
{ --------------------                                                         }
{ This event is fired when the data source has filled the buffer with image    }
{ data. This even is also called once before each new image transfer begins to }
{ obtain the initial pointer to the allocated buffer (see above).              }
{                                                                              }
{ When pData equals NIL this event marks the beginning of the transfer. It is  }
{ your responsibility to assign pData to point to a valid memory buffer.       }
{                                                                              }
{ When DataSize = 0 no more chucks from the current image is returned.         }
{                                                                              }
{ During the transfer you can change the buffer into which the next chunk is   }
{ returned. Simply change where pData points to. Make sure that the new memory }
{ area is valid and large enough to receive the negotiated number of bytes.    }
{                                                                              }
{ If pData is set to NIL during an image transfer, the current transfer is     }
{ skipped. If additional "images" are available to the data source, the next   }
{ "image" will be scanned.                                                     }
{------------------------------------------------------------------------------}
  if (pData = Nil)
  then begin // Start of image transfer. Assign initial memory area to pData
       pData := pBuffer;
       FThisLine := 0;
       FOddTrans := True;
  end
  else begin
       if (DataSize <> 0)
       then begin // Incomming image data.
            // Process the returned image chunk.
            case pBmpInfo^.bmiHeader.biBitCount of
            8  : begin
                   pSource := pData;
                   for i := 0 to (Rows - 1)
                   do begin
                      pDest := Image1.Picture.Bitmap.ScanLine[i+FThisLine];
                      for j := 0 to (BytesPerRow - 1)
                      do begin
                         pDest[j] := pSource[j];
                      end;
                      // Increment pSource to point to the next line.
                      pSource := PAnsiChar(integer(pData) + i * BytesPerRow);
                   end;
                 end;
            24 : begin
                   pSource := pData;
                   for i := 0 to (Rows - 1)
                   do begin
                      pDest := Image1.Picture.Bitmap.ScanLine[i+FThisLine];
                      for j := 0 to (BytesPerRow div 3 - 1)
                      do begin
                         k := j * 3;
                         pDest[j] := AnsiChar((byte(pSource[k]) + byte(pSource[k+1]) + byte(pSource[k+2])) div 3);
                      end;
                      // Increment pSource to point to the next line.
                      pSource := PAnsiChar(integer(pData) + i * BytesPerRow);
                   end;
                 end;
            else if (FThisLine = 0)
                 then ShowMessage('This example only supports 8 & 24 bit images!');
            end;
            inc(FThisLine, Rows); // Number of image lines processed so far.

            // Set pData to point to where the next image chunk is to be
            // transferred to.
            // This should only be necessary if you require history data. If you
            // do not require alternating between buffers this just leave pData
            // unchanged.
            FOddTrans := Not(FOddTrans);
            if FOddTrans
            then pData := pBuffer
            else pData := pointer(integer(pBuffer) + DataSize);
       end
       else begin // DataSize = 0 specifies the end of image data.
            // Free allocated memory.
            GlobalUnlock(hBuffer);
            GlobalFree(hBuffer);
            hBuffer := 0;
            pBuffer := Nil;

            // Adjust image and display it.
            AdjustImageSize;
            Repaint;
       end;
  end;
end;

end.
