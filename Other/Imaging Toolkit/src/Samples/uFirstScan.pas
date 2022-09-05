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
// $Log:  22650: uFirstScan.pas 
//
//   Rev 1.0    03-01-2004 13:02:58  mcm
// Initial edition.

unit uFirstScan;

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

end.
