unit unit_Image;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls,
  CHImage, CHAdvancedLabel;

type
  TfrmCHImage = class(TForm)
    MainMenu1: TMainMenu;
    close1: TMenuItem;
    Info1: TMenuItem;
    Label1: TLabel;
    imgSmile1: TCHImage;
    Label2: TLabel;
    imgSmile2: TCHImage;
    CHImage2: TCHImage;
    GroupBox1: TGroupBox;
    cmbEffect: TComboBox;
    CHAdvancedLabel1: TCHAdvancedLabel;
    tbValue: TTrackBar;
    CHAdvancedLabel2: TCHAdvancedLabel;
    CHImage_Effect: TCHImage;
    CHImage3: TCHImage;
    CHImage4: TCHImage;
    CHImage5: TCHImage;
    CHAdvancedLabel3: TCHAdvancedLabel;
    CHAdvancedLabel4: TCHAdvancedLabel;
    CHAdvancedLabel5: TCHAdvancedLabel;
    CHAdvancedLabel6: TCHAdvancedLabel;
    CHAdvancedLabel7: TCHAdvancedLabel;
    CHAdvancedLabel8: TCHAdvancedLabel;
    procedure close1Click(Sender: TObject);
    procedure Info1Click(Sender: TObject);
    procedure tbValueChange(Sender: TObject);
    procedure cmbEffectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  frmCHImage: TfrmCHImage;

implementation

uses unit_About, _CHTypes;

{$R *.dfm}

procedure TfrmCHImage.close1Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmCHImage.Info1Click(Sender: TObject);
begin
  frmAbout := TfrmAbout.Create(Self);
  frmAbout.ShowModal;
end;


procedure TfrmCHImage.tbValueChange(Sender: TObject);
begin
  CHImage_Effect.Effect.Value := Trunc(tbValue.Position);
end;

procedure TfrmCHImage.cmbEffectChange(Sender: TObject);
begin
  tbValue.Max := 255;
  tbValue.Frequency := 15;
  tbValue.PageSize := 15;


  if cmbEffect.ItemIndex = 0 then
    CHImage_Effect.Effect.Effectname := ieDarkness
  else if cmbEffect.ItemIndex = 1 then
    CHImage_Effect.Effect.Effectname := ieLightness
  else if cmbEffect.ItemIndex = 2 then
    CHImage_Effect.Effect.Effectname := ieContrast
  else if cmbEffect.ItemIndex = 3 then
    CHImage_Effect.Effect.Effectname := ieBlur
  else if cmbEffect.ItemIndex = 4 then
  begin
    tbValue.Max := 10;
    tbValue.Frequency := 1;
    tbValue.PageSize := 1;
    CHImage_Effect.Effect.Effectname := ieFlashlight
  end
  else if cmbEffect.ItemIndex = 5 then
  begin
    tbValue.Max := 10;
    tbValue.Frequency := 1;
    tbValue.PageSize := 1;
    CHImage_Effect.Effect.Effectname := iePixel
  end
  else if cmbEffect.ItemIndex = 6 then
    CHImage_Effect.Effect.Effectname := ieSaturation
  else if cmbEffect.ItemIndex = 7 then
    CHImage_Effect.Effect.Effectname := iePosterize
  else if cmbEffect.ItemIndex = 8 then
    CHImage_Effect.Effect.Effectname := ieSolorize
  else
  begin
    cmbEffect.ItemIndex := 0;
    CHImage_Effect.Effect.Effectname := ieDarkness;
  end;
end;

procedure TfrmCHImage.FormCreate(Sender: TObject);
begin
  GroupBox1.DoubleBuffered := True;
end;

end.
