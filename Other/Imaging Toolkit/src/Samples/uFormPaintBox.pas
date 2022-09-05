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
// $Log:  27290: uFormPaintBox.pas
//
//    Rev 1.5    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//    Rev 1.4    11-08-2009 10:20:36  mcm
// Modified registry keys.
//
//    Rev 1.3    04-03-2006 18:18:04  mcm    Version: IMG 2.16
// Clean-up.
//
//    Rev 1.2    25/02/2006 21:31:52  mcm
// Added more tools.
//
//    Rev 1.1    22/02/2006 00:09:16  mcm
//
//    Rev 1.0    19-02-2006 23:03:26  mcm
unit uFormPaintBox;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, Registry, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      ComCtrls, ExtCtrls, ToolWin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.Win.Registry, System.SysUtils, System.Classes,
      Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
      Vcl.ToolWin,
     {$ENDIF}
     mcmImage;

type
  TFormPaintBox = class(TForm)
    PanelColors  : TPanel;
    icFrontColor : TmcmImageCtrl;
    icBackColor  : TmcmImageCtrl;
    ToolBar1     : TToolBar;
    tbPencil     : TToolButton;
    ImageList    : TImageList;
    tbPick       : TToolButton;
    tbPipette    : TToolButton;
    tbLine       : TToolButton;
    tbRectangle  : TToolButton;
    tbCircle     : TToolButton;
    tbSelectArea : TToolButton;
    tbProfile    : TToolButton;
    procedure icFrontColorClick(Sender : TObject);
    procedure icBackColorClick(Sender : TObject);
    procedure FormCreate(Sender : TObject);
    procedure tbButtonClick(Sender : TObject);
  private
    { Private declarations }
    FToolChanged : TNotifyEvent;
    FFrontColor  : TColor;
    FBackColor   : TColor;
    FTool        : integer;
    procedure SetFrontColor(Value : TColor);
    procedure SetBackColor(Value : TColor);
    procedure SetTool(Value : integer);
  public
    { Public declarations }
    property BackColor : TColor
      read   FBackColor
      write  SetBackColor;
    property FrontColor : TColor
      read   FFrontColor
      write  SetFrontColor;
    property Tool : integer
      read   FTool
      write  SetTool;
    property ToolChanged : TNotifyEvent
      read   FToolChanged
      write  FToolChanged;
  end;

var FormPaintBox : TFormPaintBox;

implementation

uses mcmImageTypeDef, uFormColorSelect;

{$R *.DFM}

procedure TFormPaintBox.FormCreate(Sender : TObject);
var Reg : TRegIniFile;
begin
  FToolChanged := Nil;

  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE; 
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         FFrontColor := Reg.ReadInteger('PaintBox', 'FrontColor', integer($FF000000));
         FBackColor  := Reg.ReadInteger('PaintBox', 'BackColor', integer($FFFFFFFF));
       except
         on E:Exception
         do ;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;

  icFrontColor.Image.Width := icFrontColor.Width;
  icFrontColor.Image.Height := icFrontColor.Height;
  icFrontColor.Image.ImageFormat := IF_RGBA32;
  SetFrontColor(FFrontColor);

  icBackColor.Image.Width := icBackColor.Width;
  icBackColor.Image.Height := icBackColor.Height;
  icBackColor.Image.ImageFormat := IF_RGBA32;
  SetBackColor(FBackColor);
end; // TFormPaintBox.FormCreate.


procedure TFormPaintBox.icFrontColorClick(Sender : TObject);
begin
  FormColorSelect := TFormColorSelect.Create(Self);
  FormColorSelect.OldColor := icFrontColor.Image.Pixel[1,1];
  if (FormColorSelect.ShowModal = mrOK)
  then SetFrontColor(FormColorSelect.NewColor);
  FormColorSelect.Free;
end; // TFormPaintBox.icFrontColorClick.


procedure TFormPaintBox.icBackColorClick(Sender : TObject);
begin
  FormColorSelect := TFormColorSelect.Create(Self);
  FormColorSelect.OldColor := icBackColor.Image.Pixel[1,1];
  if (FormColorSelect.ShowModal = mrOK)
  then SetBackColor(FormColorSelect.NewColor);
  FormColorSelect.Free;
end; // TFormPaintBox.icBackColorClick.


procedure TFormPaintBox.SetFrontColor(Value : TColor);
var Reg : TRegIniFile;
begin
  FFrontColor := Value;
  icFrontColor.Image.FillRGB(FFrontColor);
  icFrontColor.DrawImage;

  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE; 
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         Reg.WriteInteger('PaintBox', 'FrontColor', FFrontColor);
       except
         on E:Exception
         do ;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;
end; // TFormPaintBox.SetFrontColor.


procedure TFormPaintBox.SetBackColor(Value : TColor);
var Reg : TRegIniFile;
begin
  FBackColor := Value;
  icBackColor.Image.FillRGB(FBackColor);
  icBackColor.DrawImage;

  Reg := TRegIniFile.Create('');
  Reg.RootKey := HKEY_CURRENT_USER; // HKEY_LOCAL_MACHINE; 
  if Reg.OpenKey(RegKey, True)
  then begin
       try
         Reg.WriteInteger('PaintBox', 'BackColor', FBackColor);
       except
         on E:Exception
         do ;
       end;
       Reg.CloseKey;
  end;
  Reg.Free;
end; // TFormPaintBox.SetBackColor.


procedure TFormPaintBox.SetTool(Value : integer);
begin
  FTool := Value;
end; // TFormPaintBox.SetTool.


procedure TFormPaintBox.tbButtonClick(Sender : TObject);
begin
  TToolButton(Sender).Down := True;
  SetTool(TToolButton(Sender).Tag); 
  if Assigned(FToolChanged)
  then FToolChanged(Sender);
end; // TFormPaintBox.tbButtonClick.

end.
