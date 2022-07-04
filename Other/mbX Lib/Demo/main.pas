unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XPMan, mbXPVerticalRuler, mbXPHorizontalRuler, mbXPSizeGrip,
  mbXPBevel, mbXPPenWidthCombo, mbXPPenStyleCombo, mbXPImageListCombo,
  mbXPImageComboBox, mbXPFontCombo, mbXPBrushStyleCombo, mbXPBooleanCombo,
  StdCtrls, mbXPArtCombo, ImgList, mbXPCheckRadio, mbXPSpin, Menus,
  mbXPJustCombo, mbXPFloatSpinEdit, mbXPSpinEdit, mbXPArrowEdit,
  mbXPImageRadio, mbXPImageCheck, IniFiles{for internet shortcuts};

type
  TForm1 = class(TForm)
    mbXPSizeGrip1: TmbXPSizeGrip;
    hRuler: TmbXPHorizontalRuler;
    vRuler: TmbXPVerticalRuler;
    XPManifest1: TXPManifest;
    mbXPBevel1: TmbXPBevel;
    Label1: TLabel;
    mbXPArtCombo1: TmbXPArtCombo;
    mbXPBooleanCombo1: TmbXPBooleanCombo;
    mbXPBrushStyleCombo1: TmbXPBrushStyleCombo;
    mbXPFontCombo1: TmbXPFontCombo;
    mbXPImageComboBox1: TmbXPImageComboBox;
    mbXPImageListCombo1: TmbXPImageListCombo;
    mbXPPenStyleCombo1: TmbXPPenStyleCombo;
    mbXPPenWidthCombo1: TmbXPPenWidthCombo;
    ImageList1: TImageList;
    ImageList2: TImageList;
    mbXPCheckRadio1: TmbXPCheckRadio;
    mbXPCheckRadio2: TmbXPCheckRadio;
    mbXPCheckRadio3: TmbXPCheckRadio;
    mbXPCheckRadio4: TmbXPCheckRadio;
    mbXPCheckRadio5: TmbXPCheckRadio;
    mbXPSpinButton1: TmbXPSpinButton;
    mbXPSpinEdit1: TmbXPSpinEdit;
    mbXPFloatSpinEdit1: TmbXPFloatSpinEdit;
    mbXPJustCombo1: TmbXPJustCombo;
    mbXPArrowEdit1: TmbXPArrowEdit;
    PopupMenu1: TPopupMenu;
    justapopup1: TMenuItem;
    popup1: TMenuItem;
    N1: TMenuItem;
    menu1: TMenuItem;
    mbXPImageCheck1: TmbXPImageCheck;
    mbXPImageRadio1: TmbXPImageRadio;
    mbXPImageCheck3: TmbXPImageCheck;
    mbXPImageCheck4: TmbXPImageCheck;
    mbXPImageCheck5: TmbXPImageCheck;
    mbXPImageCheck6: TmbXPImageCheck;
    Label2: TLabel;
    mbXPImageRadio2: TmbXPImageRadio;
    mbXPImageRadio3: TmbXPImageRadio;
    mbXPImageRadio4: TmbXPImageRadio;
    mbXPImageRadio5: TmbXPImageRadio;
    mbXPBevel2: TmbXPBevel;
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mdx, mdy: integer;

implementation

{$R *.dfm}
{$R mxico.res} //MXS icon resource file, for internet shortcut only

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
hRuler.UpdateDisplay(x);
vRuler.UpdateDisplay(y - vRuler.top);
if ssLeft in Shift then
 begin
  hRuler.markfragment(mdx, x);
  vRuler.markfragment(mdy, y-vruler.top);
 end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
mdx := x;
mdy := y - vRuler.top;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
 hRuler.markfragment(mdx, x);
 vRuler.markfragment(mdy, y-vruler.top);
end;

// only for internet shortcuts
procedure TForm1.FormCreate(Sender: TObject);
begin
 with TIniFile.Create(ExtractFilePath(Application.ExeName) + '\MXS Website.url') do
  try
   WriteString('InternetShortcut','URL', 'http://mxs.bergsoft.net');
   WriteInteger('InternetShortcut','IconIndex', 1);
   WriteString('InternetShortcut','IconFile', '"' + Application.ExeName + '"');
  finally
   Free;
  end;
end;

end.
