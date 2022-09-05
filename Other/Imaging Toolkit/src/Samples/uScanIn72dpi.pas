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
// $Log:  22701: uScanIn72dpi.pas 
//
//   Rev 1.2    03-01-2005 18:35:20  mcm    Version: DT 3.3
// Added support for Delphi 2005.

//
//   Rev 1.1    31-01-2004 13:55:16  mcm

//
//   Rev 1.0    20-01-2004 21:05:52  mcm    Version: DT3.0
// Initial revision.

//
//   Rev 1.0    03-01-2004 13:02:58  mcm
// Initial edition.

unit uScanIn72dpi;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, mcmTWAINKernel, mcmTWAINIntf, mcmTWAIN;

type
  TForm1 = class(TForm)
    mcmTWAIN: TmcmTWAIN;
    btnAcquire: TButton;
    btnSelect: TButton;
    Image1: TImage;
    procedure btnAcquireClick(Sender : TObject);
    procedure btnSelectClick(Sender : TObject);
    procedure mcmTWAINDisableMenus(Sender: TObject);
    procedure mcmTWAINEnableMenus(Sender: TObject);
    procedure mcmTWAINNegotiation(Sender: TObject; var CancelScan: Boolean);
    {$IFNDEF VER100}
     // Delphi 4 - 7
    procedure mcmTWAINImageReady(Sender: TObject; pBmp: Pointer; pBmpInfo: PBitmapInfo; hImage : hBitmap; FilePath: String);
    {$ELSE}
     // Delphi 3
    procedure mcmTWAINImageReady(Sender: TObject; pBmp: Pointer; pBmpInfo: PBitmapInfo; hImage : Integer; FilePath: String);
    {$ENDIF}
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var Form1 : TForm1;

implementation

uses TWAIN, mcmTWAINContainer;

{$R *.DFM}

procedure TForm1.btnAcquireClick(Sender : TObject);
begin
  mcmTWAIN.Acquire('');
end;

procedure TForm1.btnSelectClick(Sender : TObject);
begin
  mcmTWAIN.SelectSource;
end;

procedure TForm1.mcmTWAINDisableMenus(Sender : TObject);
begin
  btnAcquire.Enabled := False;
  btnSelect.Enabled := False;
end;

procedure TForm1.mcmTWAINEnableMenus(Sender: TObject);
begin
  btnAcquire.Enabled := True;
  btnSelect.Enabled := True;
end;

procedure TForm1.mcmTWAINImageReady(Sender: TObject; pBmp: Pointer;
  pBmpInfo: PBitmapInfo; {$IFNDEF VER100} hImage : hBitmap; {$ELSE} hImage : Integer; {$ENDIF} FilePath: String);
begin
  if (hImage <> 0)
  then Image1.Picture.Bitmap.Handle := hImage;
end;


procedure TForm1.mcmTWAINNegotiation(Sender: TObject;
  var CancelScan: Boolean);
var Resolution    : double;
    xRes, yRes    : double;
    Container     : TtwnContainer;
begin
  //----------------------------------------------------------------------------
  // Negotiate resolution.

  Resolution := 72.0; // We'll try 150 dpi.

  // Get X resolution.
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
end;

end.
