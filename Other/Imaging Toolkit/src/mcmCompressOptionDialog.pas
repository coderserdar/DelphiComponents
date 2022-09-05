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
// $Log:  23282: mcmCompressOptionDialog.pas 
//
//    Rev 1.4    2014-02-02 21:09:52  mcm    Version: IMG 4.0
// Added support for Delphi XE2, 3, 4 and 5
//
//   Rev 1.3    26-09-2004 11:11:20  mcm    Version: IMG 2.6
// Added disconnected event on TmcmIntSpin

//
//   Rev 1.2    16-09-2004 20:40:52  mcm
// Enchanged TSpinEdit with TmcmIntSpin.

//
//   Rev 1.1    25-06-2004 20:43:06  mcm    Version: IMG 2.5
// Included a Help button.

//
//   Rev 1.0    31-05-2004 23:47:30  mcm

unit mcmCompressOptionDialog;

interface

{$Include 'mcmDefines.pas'}

uses {$IFNDEF GE_DXE2}
      Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ComCtrls, ExtCtrls,
     {$ELSE}
      WinApi.Windows, WinApi.Messages, System.SysUtils, System.Classes, Vcl.Controls,
      Vcl.Graphics, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
     {$ENDIF}
     mcmImageTypeDef, umcmIntE;

type
  TFormCompressOption = class(TForm)
    PageControl  : TPageControl;
    tsGIF        : TTabSheet;
    tsJPEG       : TTabSheet;
    tsPNG        : TTabSheet;
    rgGIF        : TRadioGroup;
    Panel        : TPanel;
    btnOK        : TButton;
    btnCancel    : TButton;
    btnHelp      : TButton;
    gbJPEG       : TGroupBox;
    gbPNG        : TGroupBox;
    lJPEGQuality : TLabel;
    lPNGQuality  : TLabel;
    cbPNGQuality : TComboBox;
    seQuality: TmcmIntSpin;
    procedure rgGIFClick(Sender : TObject);
    procedure cbPNGQualityChange(Sender : TObject);
    procedure seQuality2Change(Sender : TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    { Private declarations }
    FFileFormat : TmcmFileFormat;
    FInterlaced : boolean;
    FQuality    : integer;
    procedure SetFileFormat(Value : TmcmFileFormat);
    procedure SetInterlaced(Value : boolean);
    procedure SetQuality(Value : integer);
  public
    { Public declarations }
    procedure ShowHelp(Show : boolean);
    property FileFormat : TmcmFileFormat
      read   FFileFormat
      write  SetFileFormat;
    property Interlaced : boolean
      read   FInterlaced
      write  SetInterlaced;
    property Quality : integer
      read   FQuality
      write  SetQuality;
  end;

var FormCompressOption : TFormCompressOption;

implementation

{$R *.DFM}

procedure TFormCompressOption.ShowHelp(Show : boolean);
begin
  BtnHelp.Visible := Show;
end; // TFormCompressOption.ShowHelp.


procedure TFormCompressOption.SetFileFormat(Value : TmcmFileFormat);
begin
  FFileFormat := Value;
  case FFileFormat of
  FF_GIF  : PageControl.ActivePage := tsGIF;
  FF_JPEG : PageControl.ActivePage := tsJPEG;
  FF_PNG  : PageControl.ActivePage := tsPNG;
  end;
end; // TFormCompressOption.SetFileFormat.


procedure TFormCompressOption.SetInterlaced(Value : boolean);
begin
  // Requires that FileFormat is set!
  FInterlaced := Value;
  case FFileFormat of
  FF_GIF  : if FInterlaced
            then rgGIF.ItemIndex := 0
            else rgGIF.ItemIndex := 1;
  FF_JPEG : ; // No effect.
  FF_PNG  : ; // No effect.
  end;
end; // TFormCompressOption.SetInterlaced.


procedure TFormCompressOption.SetQuality(Value : integer);
begin
  // Requires that FileFormat is set!
  FQuality := Value;
  case FFileFormat of
  FF_GIF  : ; // No effect.
  FF_JPEG : seQuality.Value := FQuality;
  FF_PNG  : case FQuality of
            0..25 : cbPNGQuality.ItemIndex := 0;
            26..50 : cbPNGQuality.ItemIndex := 1;
            51..75 : cbPNGQuality.ItemIndex := 2;
            76..100 : cbPNGQuality.ItemIndex := 3;
            end;
  end;
end; // TFormCompressOption.SetQuality.


procedure TFormCompressOption.rgGIFClick(Sender : TObject);
begin
  FInterlaced := (rgGIF.ItemIndex = 0);
end; // TFormCompressOption.rgGIFClick.


procedure TFormCompressOption.seQuality2Change(Sender : TObject);
begin
  try
    if (seQuality.Text <> '')
    then FQuality := seQuality.Value;
  except
  end;
end; // TFormCompressOption.seQualityChange.


procedure TFormCompressOption.cbPNGQualityChange(Sender : TObject);
begin
  case cbPNGQuality.ItemIndex of
  0 : FQuality := 25;
  1 : FQuality := 50;
  2 : FQuality := 75;
  3 : FQuality := 100;
  end;
end; // TFormCompressOption.cbPNGQualityChange.

{$IFDEF RANGE_OFF}{$UNDEF RANGE_OFF}{$R+}{$ENDIF}


procedure TFormCompressOption.btnHelpClick(Sender : TObject);
begin
  Application.HelpContext(HelpContext);
end; // TFormCompressOption.btnHelpClick.

end.
