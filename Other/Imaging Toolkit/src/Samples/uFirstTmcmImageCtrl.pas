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
// $Log:  22199: uFirstTmcmImageCtrl.pas 
//
//   Rev 1.1    26-07-2004 20:00:06  mcm
// Added calculation of x,y coordinates based on mouse movement in the image
// controls.

//
//   Rev 1.0    17-11-2003 00:47:12  mcm    Version: IMG 2.0
// Initial edition

unit uFirstTmcmImageCtrl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Spin, ExtCtrls,
  mcmImage, mcmImageFile, mcmImageTypeDef, umcmIntE, ComCtrls;

//------------------------------------------------------------------------------
// This sample show some of the feature of, and how to use the TmcmImageCtrl
// component on a TForm and TScrollBox.
//------------------------------------------------------------------------------

type
  TFormImageCtrl = class(TForm)
    Panel1: TPanel;
    rsScale: TmcmRealSpin;
    lScale: TLabel;
    cbCenter: TCheckBox;
    cbFitToWindow: TCheckBox;
    cbTransparentWindow: TCheckBox;
    cbBorder: TCheckBox;
    mcmImageCtrl1: TmcmImageCtrl;
    HorzScrollBar: TScrollBar;
    VertScrollBar: TScrollBar;
    OpenDialog1: TOpenDialog;
    ScrollBox1: TScrollBox;
    mcmImageCtrl2: TmcmImageCtrl;
    StatusBar: TStatusBar;
    procedure rsScaleChange(Sender: TObject);
    procedure cbCenterClick(Sender: TObject);
    procedure cbFitToWindowClick(Sender: TObject);
    procedure mcmImageCtrl1Change(Sender: TObject);
    procedure cbTransparentWindowClick(Sender: TObject);
    procedure cbBorderClick(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mcmImageCtrl2Change(Sender: TObject);
    procedure mcmImageCtrl2MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure mcmImageCtrl1MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
  private
    { Private declarations }
    FBtnDown : boolean;
    FStartX  : integer;
    FStartY  : integer;
    FEndX    : integer;
    FEndY    : integer;
    FOfsX    : integer;
    FOfsY    : integer;
  public
    { Public declarations }
  end;

var FormImageCtrl : TFormImageCtrl;

implementation

{$R *.DFM}

procedure TFormImageCtrl.rsScaleChange(Sender : TObject);
begin
  // Scale/zoom image in and out.
  if Assigned(mcmImageCtrl1)
  then begin
       mcmImageCtrl1.Scale := rsScale.Value;
       mcmImageCtrl1Change(Sender);
  end;
  
  if Assigned(mcmImageCtrl2)
  then begin
       mcmImageCtrl2.Scale := rsScale.Value;
       mcmImageCtrl2Change(Sender);
  end;
end; // TFormImageCtrl.rsScaleChange.


procedure TFormImageCtrl.cbCenterClick(Sender : TObject);
begin
  // Center image in mcmImageCtrl.
  mcmImageCtrl1.Center := cbCenter.Checked;
  mcmImageCtrl1Change(Sender);

  mcmImageCtrl2.Center := cbCenter.Checked;
  mcmImageCtrl2Change(Sender);
end; // TFormImageCtrl.cbCenterClick.


procedure TFormImageCtrl.cbFitToWindowClick(Sender : TObject);
begin
  // Scale image to fit into mcmImageCtrl's client rectangle.
  rsScale.Enabled := Not(cbFitToWindow.Checked);

  // Scroll to (0,0)
  if cbFitToWindow.Checked
  then mcmImageCtrl1.Image.SetOrigo(Point(0, 0));

  mcmImageCtrl1.ScaleToFit := cbFitToWindow.Checked;
  mcmImageCtrl2.ScaleToFit := cbFitToWindow.Checked;

  if rsScale.Enabled
  then begin
       mcmImageCtrl1.Scale := rsScale.Value;
       mcmImageCtrl2.Scale := rsScale.Value;
  end;

  mcmImageCtrl1Change(Sender);
  mcmImageCtrl2Change(Sender);
end; // TFormImageCtrl.cbFitToWindowClick.


procedure TFormImageCtrl.cbTransparentWindowClick(Sender : TObject);
begin
  // Make background transparent, i.e. the area not covered by the image.
  if cbTransparentWindow.Checked
  then begin
       mcmImageCtrl1.Color := clNone;
       mcmImageCtrl2.Color := clNone;
  end
  else begin
       mcmImageCtrl1.Color := clWhite;
       mcmImageCtrl2.Color := clWhite;
  end;
end; // TFormImageCtrl.cbTransparentWindowClick.


procedure TFormImageCtrl.cbBorderClick(Sender : TObject);
begin
  // Set or Remove border in mcmImageCtrl1.
  if cbBorder.Checked
  then mcmImageCtrl1.BorderStyle := BS_SINGLE
  else mcmImageCtrl1.BorderStyle := BS_NONE;
end; // TFormImageCtrl.cbBorderClick.


procedure TFormImageCtrl.mcmImageCtrl1Change(Sender : TObject);
var x, y : integer;
begin
  if Assigned(mcmImageCtrl1)
  then begin
       if Not(mcmImageCtrl1.Center or mcmImageCtrl1.ScaleToFit)
       then begin
            if Assigned(HorzScrollBar)
            then begin
                 // Get Horiz/Vert scroll range.
                 x := Round(mcmImageCtrl1.Scale * mcmImageCtrl1.Image.Width) - mcmImageCtrl1.Width;
                 if (x > 0)
                 then HorzScrollBar.Max := x
                 else HorzScrollBar.Max := 0;
            end;
            if Assigned(VertScrollBar)
            then begin
                 y := Round(mcmImageCtrl1.Scale * mcmImageCtrl1.Image.Height) - mcmImageCtrl1.Height;
                 if (y > 0)
                 then VertScrollBar.Max := y
                 else VertScrollBar.Max := 0;
            end;
       end
       else begin
            HorzScrollBar.Max := 0;
            VertScrollBar.Max := 0;
       end;
  end;
end; // TFormImageCtrl.mcmImageCtrl1Change.


procedure TFormImageCtrl.mcmImageCtrl2Change(Sender : TObject);
begin
  if Not(mcmImageCtrl2.Center or mcmImageCtrl2.ScaleToFit)
  then begin
       ScrollBox1.HorzScrollBar.Range := Round(mcmImageCtrl2.Scale * mcmImageCtrl2.Image.Width);
       ScrollBox1.VertScrollBar.Range := Round(mcmImageCtrl2.Scale * mcmImageCtrl2.Image.Height);

       // These two lines prevents ScrollBox from causing flicker, when either
       // scrollbar is at it maximum.
       // Otherwise ScrollBox doesn't cause flicker.
       ValidateRect(ScrollBox1.Handle, Nil);
       mcmImageCtrl2.Repaint;
  end
  else begin
       ScrollBox1.HorzScrollBar.Range := 0;
       ScrollBox1.VertScrollBar.Range := 0;
  end;
end; // TFormImageCtrl.mcmImageCtrl2Change


procedure TFormImageCtrl.ScrollBarChange(Sender : TObject);
begin
  // Scroll image - we're using SetOrigo on the TmcmImage to scroll the image.
  mcmImageCtrl1.Image.SetOrigo(Point(-HorzScrollBar.Position, -VertScrollBar.Position));
  mcmImageCtrl1.Repaint;
end; // TFormImageCtrl.ScrollBarChange.


procedure TFormImageCtrl.FormShow(Sender : TObject);
begin
  // When the form is opened, read an image.
  OpenDialog1.Filter := ImageFileManager.GetReadFilter;
  OpenDialog1.FilterIndex := 14;
  // Open an image file
  if OpenDialog1.Execute
  then begin
       mcmImageCtrl1.Image.FileOpen(OpenDialog1.FileName);
       mcmImageCtrl2.Image.FileOpen(OpenDialog1.FileName);
  end;
  rsScale.Value := mcmImageCtrl1.Scale;
end; // TFormImageCtrl.FormShow.


procedure TFormImageCtrl.mcmImageCtrl1MouseMove(Sender : TObject;
                                                Shift  : TShiftState;
                                                X, Y   : Integer);
var Origo : TPoint;
begin
  // Show coordinate of cursor in the image contained by mcmImageCtrl1.
  Origo := mcmImageCtrl1.Image.GetOrigo;
  if cbCenter.Checked
  then begin
       // Have to add centred x, y offset.
       x := x - Round((mcmImageCtrl1.Width - mcmImageCtrl1.Scale * mcmImageCtrl1.Image.Width) / 2);
       y := y - Round((mcmImageCtrl1.Height - mcmImageCtrl1.Scale * mcmImageCtrl1.Image.Height) / 2);
  end;
  x := Trunc((x - Origo.x) / mcmImageCtrl1.Scale);
  y := Trunc((y - Origo.y) / mcmImageCtrl1.Scale);
  Statusbar.SimpleText := '(x,y) = ' + IntToStr(X) + ', ' + IntToStr(Y);
end; // TFormImageCtrl.mcmImageCtrl1MouseMove.


procedure TFormImageCtrl.mcmImageCtrl2MouseMove(Sender : TObject;
                                                Shift  : TShiftState;
                                                X, Y   : Integer);
begin
  // Show coordinate of cursor in the image contained by mcmImageCtrl2.
  if cbCenter.Checked
  then begin
       // Have to add centred x, y offset.
       x := x - Round((mcmImageCtrl2.Width - mcmImageCtrl2.Scale * mcmImageCtrl2.Image.Width) / 2);
       y := y - Round((mcmImageCtrl2.Height - mcmImageCtrl2.Scale * mcmImageCtrl2.Image.Height) / 2);
  end;
  x := Trunc(x / mcmImageCtrl2.Scale);
  y := Trunc(y / mcmImageCtrl2.Scale);
  Statusbar.SimpleText := '(x,y) = ' + IntToStr(X) + ', ' + IntToStr(Y);
end; // TFormImageCtrl.mcmImageCtrl2MouseMove.

end.
