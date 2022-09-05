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
// $Log:  19424: uFormSelectView.pas 
//
//    Rev 1.3    2014-02-02 21:10:12  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.2    27-07-2004 15:05:48  mcm    Version: IMG 2.5
// Fixed mcmRegion leaving parts of its border in the image when window is first
// shown.

//
//   Rev 1.1    06-07-2003 10:41:46  mcm    Version: IMG 1.3.4

//
//   Rev 1.0    08-03-2003 19:05:18  mcm    Version: IMG 1.3.2
// Use by TmcmImageDualView to select an area of interest from the entire image.

unit uFormSelectView;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ExtCtrls,
     {$ELSE}
      WinApi.Windows, System.Classes, Vcl.Controls, Vcl.Graphics,
      Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
     {$ENDIF}
     mcmImage, mcmRegion;

type
  TFormSelectView = class(TForm)
    Panel1       : TPanel;
    Panel2       : TPanel;
    mcmImageCtrl : TmcmImageCtrl;
    mcmRegion    : TmcmRegion;
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure mcmRegionMoved(Sender: TObject);
  private
    { Private declarations }
    function  GetImage : TmcmImage;
    function  GetRegion : TRect;
    procedure SetImage(AImage : TmcmImage);
    procedure SetRegion(ARegion : TRect);
  public
    { Public declarations }
    property  Image : TmcmImage
      read    GetImage
      write   SetImage;
    property  Region : TRect
      read    GetRegion
      write   SetRegion;
  end;

var FormSelectView : TFormSelectView;

implementation

uses {$IFNDEF GE_DXE2}
      Messages, SysUtils;
     {$ELSE}
      WinApi.Messages, System.SysUtils;
     {$ENDIF}

{$R *.DFM}

procedure TFormSelectView.FormCreate(Sender : TObject);
var ADc : HDC;
begin
  ADc := GetDC(0);
  Width := GetDeviceCaps(ADc, HORZRES) div 3;
  Height := GetDeviceCaps(ADc, VERTRES) div 3;
  if (Width < 32)
  then Width := 32;
  if (Height < 32)
  then Height := 32;
  ReleaseDC(0, ADc);
  mcmRegion.LinePen.Color := clGray;
  mcmRegion.LinePen.Style := PSSOLID;
  mcmRegion.LinePen.Mode := pmMaskPenNot;
end; // TFormSelectView.FormCreate.


procedure TFormSelectView.FormMouseUp(Sender : TObject;
                                      Button : TMouseButton;
                                      Shift  : TShiftState;
                                      X, Y   : Integer);
begin
 Close;
end; // TFormSelectView.FormMouseUp.


function TFormSelectView.GetImage : TmcmImage;
begin
  Result := mcmImageCtrl.Image;
end; // TFormSelectView.GetImage.


procedure TFormSelectView.SetImage(AImage : TmcmImage);
var dw, dh : integer;
begin
  mcmImageCtrl.Image.Assign(AImage);
  dh := Panel1.Height - mcmImageCtrl.Height;
  dw := Panel1.Width - mcmImageCtrl.Width;
  ClientHeight := Round(mcmImageCtrl.Image.Height * mcmImageCtrl.Scale) + dh;
  ClientWidth  := Round(mcmImageCtrl.Image.Width * mcmImageCtrl.Scale) + dw;

  mcmRegion.Scale := mcmImageCtrl.Scale;
  mcmRegion.MaxWidth  := Round(mcmImageCtrl.Scale * mcmImageCtrl.Image.Width);
  mcmRegion.MaxHeight := Round(mcmImageCtrl.Scale * mcmImageCtrl.Image.Height);
end; // TFormSelectView.SetImage.


function TFormSelectView.GetRegion : TRect;
var Region : TRect;
begin
  Region.Left := Round(mcmRegion.Left / mcmRegion.Scale);
  Region.Top := Round(mcmRegion.Top / mcmRegion.Scale);
  Region.Right := Round(mcmRegion.Right / mcmRegion.Scale);
  Region.Bottom := Round(mcmRegion.Bottom / mcmRegion.Scale);
  Result := Region;
end; // TFormSelectView.GetRegion.


procedure TFormSelectView.SetRegion(ARegion : TRect);
begin
  mcmRegion.Height := 1;
  mcmRegion.Width  := 1;
  mcmRegion.Left := Round(ARegion.Left * mcmRegion.Scale);
  mcmRegion.Top := Round(ARegion.Top * mcmRegion.Scale);
  mcmRegion.Right := Round(ARegion.Right * mcmRegion.Scale);
  mcmRegion.Bottom := Round(ARegion.Bottom * mcmRegion.Scale);
end; // TFormSelectView.SetRegion.


procedure TFormSelectView.FormActivate(Sender: TObject);
var x, y : integer;
begin
  Update; // make sure window is fully painted.

  // Move cursor to centre of region.
  x := mcmRegion.Left + Left + 2 + (mcmRegion.Width shr 1);
  y := mcmRegion.Top + Top + 2 + (mcmRegion.Height shr 1);
  SetCursorPos(x, y);

  // Simulate that the left mouse button is pressed down.
  mcmRegion.SetFocus;
  PostMessage(Handle, WM_CAPTURECHANGED, 0, mcmRegion.Handle);
  PostMessage(mcmRegion.Handle, WM_NCLBUTTONDOWN, HTCAPTION, (y shl 16) + x);
  PostMessage(mcmRegion.Handle,  WM_NCMOUSEMOVE, HTCAPTION, (y shl 16) + x);
  mcmImageCtrl.DrawImage; // Ensure that border from mcmRegion is properly draw
  // and doesn't leave parts of the border on the image when moved.
end; // TFormSelectView.FormActivate.


procedure TFormSelectView.mcmRegionMoved(Sender : TObject);
begin
  // Make sure that window is closed when left button is released.
  if (GetAsyncKeyState(VK_LBUTTON) = 0)
  then Close;
end; // TFormSelectView.mcmRegionMoved.

end.
