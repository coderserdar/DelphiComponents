unit UFormMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ATImageBox, ExtCtrls, StdCtrls, ComCtrls, ExtDlgs, Jpeg;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Box: TATImageBox;
    GroupBox1: TGroupBox;
    chkFit: TCheckBox;
    chkCenter: TCheckBox;
    chkFitOnlyBig: TCheckBox;
    GroupBox2: TGroupBox;
    TrackBar1: TTrackBar;
    Label1: TLabel;
    chkKeepPos: TCheckBox;
    btnLoad: TButton;
    chkBorder: TCheckBox;
    OpenPictureDialog1: TOpenPictureDialog;
    LabelScale: TLabel;
    chkLabel: TCheckBox;
    chkDrag: TCheckBox;
    Bevel1: TBevel;
    chkResample: TCheckBox;
    labResampleDelay: TLabel;
    edResampleDelay: TEdit;
    procedure btnLoadClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkFitClick(Sender: TObject);
    procedure chkFitOnlyBigClick(Sender: TObject);
    procedure chkCenterClick(Sender: TObject);
    procedure chkBorderClick(Sender: TObject);
    procedure chkKeepPosClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure chkLabelClick(Sender: TObject);
    procedure chkDragClick(Sender: TObject);
  private
    { Private declarations }
    FUpdatingSelfOptions: Boolean;
    procedure UpdateImageOptions;
    procedure UpdateImageScaleOptions;
    procedure UpdateSelfOptions;
    procedure UpdateSelfScaleOptions;
    procedure UpdateImageLabel;
  public
    { Public declarations }
    procedure LoadImage(const AFileName: string);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.DFM}

procedure TFormMain.LoadImage(const AFileName: string);
begin
  try
    try
      Box.Image.Picture.LoadFromFile(AFileName);
    finally
      Box.UpdateImageInfo;
      UpdateSelfScaleOptions;
    end;
  except
    Application.MessageBox(
      PChar(Format('Could not load image from "%s"', [AFileName])),
      'Error', MB_OK or MB_ICONERROR);
  end;
end;

procedure TFormMain.UpdateImageOptions;
const
  Borders: array[Boolean] of TBorderStyle = (bsNone, bsSingle);
begin
  if not FUpdatingSelfOptions then
  begin
    Box.ImageFitToWindow := chkFit.Checked;
    Box.ImageFitOnlyBig := chkFitOnlyBig.Checked;
    Box.ImageCenter := chkCenter.Checked;
    Box.Image.Resample:= chkResample.Checked;
    Box.Image.ResampleDelay:= StrToIntDef(edResampleDelay.Text, Box.Image.ResampleDelay);
    Box.BorderStyle := Borders[chkBorder.Checked];
    Box.ImageLabel.Visible := chkLabel.Checked;
    Box.ImageDrag := chkDrag.Checked;
    Box.ImageKeepPosition := chkKeepPos.Checked;
  end;
end;

procedure TFormMain.UpdateImageScaleOptions;
begin
  if not FUpdatingSelfOptions then
  begin
    Box.ImageScale := TrackBar1.Position;
  end;
end;

procedure TFormMain.UpdateSelfOptions;
begin
  FUpdatingSelfOptions := True;

  chkFit.Checked := Box.ImageFitToWindow;
  UpdateSelfScaleOptions;

  FUpdatingSelfOptions := False;
end;

procedure TFormMain.UpdateSelfScaleOptions;
begin
  FUpdatingSelfOptions := True;

  TrackBar1.Position := Box.ImageScale;
  LabelScale.Caption := Format('%d%%', [Box.ImageScale]);
  UpdateImageLabel;

  FUpdatingSelfOptions := False;
end;

procedure TFormMain.UpdateImageLabel;
begin
  with Box do
    ImageLabel.Caption := Format(
      'Original size: %d x %d'#13'Current scale: %d%%',
      [ImageWidth, ImageHeight, ImageScale]);
end;

procedure TFormMain.btnLoadClick(Sender: TObject);
begin
  with OpenPictureDialog1 do
  begin
    InitialDir := ExtractFileDir(Application.ExeName);
    if Execute then
      LoadImage(FileName);
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
var
  FileName: AnsiString;
begin
  FileName := ExtractFilePath(Application.ExeName) + 'Test_flowers.jpg';
  if FileExists(FileName) then
    LoadImage(FileName);
end;

procedure TFormMain.chkFitClick(Sender: TObject);
begin
  UpdateImageOptions;
  UpdateSelfScaleOptions;
end;

procedure TFormMain.chkFitOnlyBigClick(Sender: TObject);
begin
  UpdateImageOptions;
  UpdateSelfScaleOptions;
end;

procedure TFormMain.chkCenterClick(Sender: TObject);
begin
  UpdateImageOptions;
end;

procedure TFormMain.chkBorderClick(Sender: TObject);
begin
  UpdateImageOptions;
  UpdateSelfScaleOptions;
end;

procedure TFormMain.chkLabelClick(Sender: TObject);
begin
  UpdateImageOptions;
end;

procedure TFormMain.chkKeepPosClick(Sender: TObject);
begin
  UpdateImageOptions;
end;

procedure TFormMain.TrackBar1Change(Sender: TObject);
begin
  UpdateImageScaleOptions;
  UpdateSelfOptions;
end;

procedure TFormMain.chkDragClick(Sender: TObject);
begin
  UpdateImageOptions;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Box.OnOptionsChange := FormResize;
  Box.ImageLabel.Font.Color := clBlue;

  FUpdatingSelfOptions := False;
  UpdateImageOptions;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  UpdateSelfScaleOptions;
end;


end.
