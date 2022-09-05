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
// $Log:  22492: uFormPalette.pas 
//
//    Rev 1.4    2014-02-02 21:10:10  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.3    20-12-2004 22:58:10  mcm
// Modified to use TmcmInt

//
//   Rev 1.2    22-12-2003 16:00:24  mcm

//
//   Rev 1.1    09-12-2003 16:47:38  mcm

//
//   Rev 1.0    05-12-2003 16:28:10  mcm    Version: IMG 2.2

unit uFormPalette;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ExtCtrls, Spin,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Samples.Spin,
     {$ENDIF}
     mcmImage,
     mcmColorGrid,
     umcmIntE;

{$IFDEF VER100} {$DEFINE DCB3} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$ENDIF}
{$IFDEF VER110} {$DEFINE DCB3} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$ENDIF}
{$IFDEF VER120} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$ENDIF}
{$IFDEF VER125} {$DEFINE DCB3_6} {$DEFINE DCB3_4} {$ENDIF}
{$IFDEF VER130} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER135} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER140} {$DEFINE DCB3_6} {$ENDIF}
{$IFDEF VER145} {$DEFINE DCB3_6} {$ENDIF}

{$IFNDEF DCB3_6} // Don't show "Unsafe code type and cast warnings".
{$WARN UNSAFE_TYPE OFF}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_CAST OFF}
{$ENDIF}

{$IFOPT R+}{$DEFINE RANGE_OFF}{$R-}{$ENDIF}

type
  TFormPalette = class(TForm)
    btnOK      : TButton;
    btnCancel  : TButton;
    gbPalette  : TGroupBox;
    ColorGrid  : TmcmColorGrid;
    gbEditColor: TGroupBox;
    sNewColor  : TShape;
    sOldColor  : TShape;
    lA         : TLabel;
    lB         : TLabel;
    lC         : TLabel;
    lPalIndex  : TLabel;
    seA        : TmcmIntSpin;
    seB        : TmcmIntSpin;
    seC        : TmcmIntSpin;
    procedure ColorGridPaletteIndex(Sender : TObject; NewIndex : Integer);
    procedure ChangeColor(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FImage : TmcmImage;
    FSavePalette : PLogPalette;
    procedure  SetImage(Value : TmcmImage);
  public
    { Public declarations }
    property Image : TmcmImage
      read   FImage
      write  SetImage;
  end;

var FormPalette : TFormPalette;

implementation

{$IFDEF GE_DXE2} uses System.UITypes; {$ENDIF}

{$R *.DFM}

procedure TFormPalette.FormCreate(Sender : TObject);
begin
  sNewColor.ControlStyle := sNewColor.ControlStyle + [csOpaque];
  sOldColor.ControlStyle := sOldColor.ControlStyle + [csOpaque];
  GetMem(FSavePalette, 256 * SizeOf(TPaletteEntry) + SizeOf(TLogPalette));
end; //TFormPalette.FormCreate.


procedure TFormPalette.FormDestroy(Sender : TObject);
begin
  FreeMem(FSavePalette);
end; // TFormPalette.FormDestroy.



procedure TFormPalette.ColorGridPaletteIndex(Sender : TObject; NewIndex : Integer);
var SaveNotify : TNotifyEvent;
begin
  sOldColor.Brush.Color := ColorGrid.SelectedColor;
  sNewColor.Brush.Color := ColorGrid.SelectedColor;

  lPalIndex.Caption := 'Index: ' + IntToStr(NewIndex);

  SaveNotify := seA.OnChange;
  seA.OnChange := Nil;
  seA.Value := GetRValue(sOldColor.Brush.Color);
  seA.OnChange := SaveNotify;

  SaveNotify := seB.OnChange;
  seB.OnChange := Nil;
  seB.Value := GetGValue(sOldColor.Brush.Color);
  seB.OnChange := SaveNotify;

  SaveNotify := seC.OnChange;
  seC.OnChange := Nil;
  seC.Value := GetBValue(sOldColor.Brush.Color);
  seC.OnChange := SaveNotify;
end; // TFormPalette.ColorGridPaletteIndex.


procedure TFormPalette.ChangeColor(Sender : TObject);
begin
  if (seA.Text <> '') and (seB.Text <> '') and (seC.Text <> '')
  then begin
       try
         sNewColor.Brush.Color := RGB(seA.Value, seB.Value, seC.Value);
         ColorGrid.SelectedColor := sNewColor.Brush.Color;
       except
       end;
  end;
end; // TFormPalette.ChangeColor.


procedure TFormPalette.SetImage(Value : TmcmImage);
begin
  FImage := Value;
  if Assigned(FImage)
  then FImage.GetPaletteEntries(FSavePalette);
  ColorGrid.Image := FImage;
end; // TFormPalette.SetImage.


procedure TFormPalette.btnCancelClick(Sender : TObject);
begin
  if Assigned(FImage)
  then FImage.SetPaletteEntries(FSavePalette);
end; // TFormPalette.btnCancelClick.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}

end.
