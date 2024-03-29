unit Ctrls;

{$I RX.INC}

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, RxPlacemnt, StdCtrls, RXCtrls, TabNotBk, RXSlider, ExtCtrls,
  RXSwitch, Mask, RxToolEdit, Spin, RxCurrEdit, FileCtrl, RXSplit, Grids,
  Outline, DirOutln, RXDice, RXClock, RxAnimate, RXSpin, RxCombos, ComCtrls;

type
  TControlsForm = class(TForm)
    TabbedNotebook1: TTabbedNotebook;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    ColorComboBox1: TColorComboBox;
    FontComboBox1: TFontComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    rxSlider1: TrxSlider;
    Label4: TLabel;
    Label5: TLabel;
    ComboBox2: TComboBox;
    Label6: TLabel;
    Label7: TLabel;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Label8: TLabel;
    ComboBox3: TComboBox;
    Label9: TLabel;
    rxSwitch1: TrxSwitch;
    Label10: TLabel;
    ComboBox4: TComboBox;
    Label11: TLabel;
    ComboBox5: TComboBox;
    CheckBox5: TCheckBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    GroupBox9: TGroupBox;
    GroupBox10: TGroupBox;
    ComboEdit1: TComboEdit;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    DateEdit1: TDateEdit;
    FilenameEdit1: TFilenameEdit;
    DirectoryEdit1: TDirectoryEdit;
    CurrencyEdit1: TCurrencyEdit;
    Label22: TLabel;
    Edit1: TEdit;
    rxLabel1: TrxLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    ColorComboBox2: TColorComboBox;
    ComboBox6: TComboBox;
    SpinEdit2: TSpinEdit;
    Button1: TButton;
    FontDialog1: TFontDialog;
    GroupBox11: TGroupBox;
    Label26: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Memo1: TMemo;
    rxSplitter1: TrxSplitter;
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    rxSplitter2: TrxSplitter;
    FileListBox1: TFileListBox;
    Label27: TLabel;
    Label28: TLabel;
    GroupBox12: TGroupBox;
    GroupBox13: TGroupBox;
    GroupBox14: TGroupBox;
    GroupBox15: TGroupBox;
    AnimatedImage1: TAnimatedImage;
    TextListBox1: TTextListBox;
    rxClock1: TrxClock;
    rxDice1: TrxDice;
    Label30: TLabel;
    RunnerImage: TImage;
    Label31: TLabel;
    ComboBox7: TComboBox;
    Button2: TButton;
    Label32: TLabel;
    SpinEdit3: TSpinEdit;
    Label33: TLabel;
    SpinEdit4: TSpinEdit;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    ComboBox8: TComboBox;
    Button3: TButton;
    Label37: TLabel;
    CheckBox6: TCheckBox;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    Label38: TLabel;
    Button4: TButton;
    Label39: TLabel;
    SpinEdit8: TSpinEdit;
    Label40: TLabel;
    SpinEdit9: TSpinEdit;
    Button5: TButton;
    FlagImage: TImage;
    SearchImage: TImage;
    BookImage: TImage;
    SwOff: TImage;
    SwOn: TImage;
    Shape1: TShape;
    GroupBox16: TGroupBox;
    rxSpinEdit1: TrxSpinEdit;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    ComboBox9: TComboBox;
    SpinEdit1: TSpinEdit;
    FormStorage1: TFormStorage;
    ADHThumb: TImage;
    ADHRuler: TImage;
    ADVThumb: TImage;
    ADVRuler: TImage;
    CheckBox4: TCheckBox;
    Button6: TButton;
    OpenDialog: TOpenDialog;
    RxCalcEdit1: TRxCalcEdit;
    CheckBox7: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ColorComboBox1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure rxSlider1Change(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure ComboBox3Change(Sender: TObject);
    procedure ComboBox4Change(Sender: TObject);
    procedure ComboBox5Change(Sender: TObject);
    procedure rxSwitchOn(Sender: TObject);
    procedure rxSwitch1Off(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure FontComboBox1Change(Sender: TObject);
    procedure ComboEdit1ButtonClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure ColorComboBox2Change(Sender: TObject);
    procedure ComboBox6Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FileListBox1Change(Sender: TObject);
    procedure ComboBox7Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SpinEdit3Change(Sender: TObject);
    procedure SpinEdit4Change(Sender: TObject);
    procedure AnimatedImage1StartStop(Sender: TObject);
    procedure ComboBox8Change(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure SpinEdit5Change(Sender: TObject);
    procedure SpinEdit6Change(Sender: TObject);
    procedure SpinEdit7Change(Sender: TObject);
    procedure rxClock1Alarm(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure SpinEdit8Change(Sender: TObject);
    procedure SpinEdit9Change(Sender: TObject);
    procedure rxDice1Change(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure ComboBox9Change(Sender: TObject);
    procedure SpinEdit1Change(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses RxAniFile, RxPickDate, RxDateUtil;

procedure TControlsForm.FormCreate(Sender: TObject);
begin
{$IFDEF WIN32}
  if NewStyleControls then DirectoryEdit1.DialogKind := dkWin32;
{$ENDIF}
{$IFDEF RX_D3}
  AnimatedImage1.AsyncDrawing := True;
{$ENDIF}
end;

procedure TControlsForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TControlsForm.FormShow(Sender: TObject);
begin
  FontComboBox1.FontName := 'Ms Sans Serif';
  ComboBox1.ItemIndex := 0;
  ComboBox2.ItemIndex := 0;
  ComboBox3.ItemIndex := 0;
  ComboBox4.ItemIndex := 0;
  ComboBox5.ItemIndex := 0;
  ComboBox6.ItemIndex := 0;
  ComboBox7.ItemIndex := 0;
  ComboBox7Change(ComboBox7);
  ComboBox8.ItemIndex := 1;
  ComboBox9.ItemIndex := 0;
end;

procedure TControlsForm.ColorComboBox1Change(Sender: TObject);
begin
  Shape1.Brush.Color := ColorComboBox1.ColorValue;
end;

procedure TControlsForm.CheckBox1Click(Sender: TObject);
begin
  FontComboBox1.TrueTypeOnly := CheckBox1.Checked;
end;

procedure TControlsForm.ComboBox1Change(Sender: TObject);
begin
  FontComboBox1.Device := TFontDevice(ComboBox1.ItemIndex);
end;

procedure TControlsForm.ComboBox2Change(Sender: TObject);
begin
  with rxSlider1 do
    Orientation := TSliderOrientation(ComboBox2.ItemIndex);
end;

procedure TControlsForm.rxSlider1Change(Sender: TObject);
begin
  Label6.Caption := Format('Value: %d', [rxSlider1.Value]);
end;

procedure TControlsForm.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    rxSlider1.Options := rxSlider1.Options + [soSmooth]
  else
    rxSlider1.Options := rxSlider1.Options - [soSmooth];
end;

procedure TControlsForm.CheckBox3Click(Sender: TObject);
begin
  if CheckBox3.Checked then
    rxSlider1.Options := rxSlider1.Options + [soShowPoints]
  else
    rxSlider1.Options := rxSlider1.Options - [soShowPoints];
end;

procedure TControlsForm.ComboBox3Change(Sender: TObject);
begin
  with rxSlider1 do
    case ComboBox3.ItemIndex of
      0:
        begin
          NumThumbStates := 2;
          ImageHRuler := nil;
          ImageHThumb := nil;
          ImageVRuler := nil;
          ImageVThumb := nil;
        end;
      1:
        begin
          NumThumbStates := 1;
          ImageHRuler := ADHRuler.Picture.Bitmap;
          ImageHThumb := ADHThumb.Picture.Bitmap;
          ImageVRuler := ADVRuler.Picture.Bitmap;
          ImageVThumb := ADVThumb.Picture.Bitmap;
        end;
    end;
end;

procedure TControlsForm.ComboBox4Change(Sender: TObject);
begin
  rxSwitch1.TextPosition := TTextPos(ComboBox4.ItemIndex);
end;

procedure TControlsForm.ComboBox5Change(Sender: TObject);
begin
  with rxSwitch1 do
    case ComboBox5.ItemIndex of
      0:
        begin
          GlyphOn := nil;
          GlyphOff := nil;
        end;
      1:
        begin
          GlyphOn := SwOn.Picture.Bitmap;
          GlyphOff := SwOff.Picture.Bitmap;
        end;
    end;

end;

procedure TControlsForm.rxSwitchOn(Sender: TObject);
begin
  rxSwitch1.Caption := 'On';
end;

procedure TControlsForm.rxSwitch1Off(Sender: TObject);
begin
  rxSwitch1.Caption := 'Off';
end;

procedure TControlsForm.CheckBox5Click(Sender: TObject);
begin
  if CheckBox5.Checked then
    rxSwitch1.BorderStyle := bsSingle
  else
    rxSwitch1.BorderStyle := bsNone;
end;

procedure TControlsForm.FontComboBox1Change(Sender: TObject);
begin
  Label2.Font.Name := FontComboBox1.FontName;
end;

procedure TControlsForm.ComboEdit1ButtonClick(Sender: TObject);
var
  ADate: TDateTime;
begin
  ADate := StrToDateDef(ComboEdit1.Text, Now);
  if PopupDate(ADate, ComboEdit1) then
    ComboEdit1.Text := DateToStr(ADate);
  if ComboEdit1.CanFocus then ComboEdit1.SetFocus;
end;

procedure TControlsForm.Edit1Change(Sender: TObject);
begin
  CurrencyEdit1.DisplayFormat := Edit1.Text;
end;

procedure TControlsForm.CheckBox7Click(Sender: TObject);
begin
  CurrencyEdit1.FormatOnEditing := CheckBox7.Checked;
end;

procedure TControlsForm.ColorComboBox2Change(Sender: TObject);
begin
  rxLabel1.ShadowColor := ColorComboBox2.ColorValue;
end;

procedure TControlsForm.ComboBox6Change(Sender: TObject);
begin
  rxLabel1.ShadowPos := TShadowPosition(ComboBox6.ItemIndex);
end;

procedure TControlsForm.SpinEdit2Change(Sender: TObject);
begin
  rxLabel1.ShadowSize := SpinEdit2.Value;
end;

procedure TControlsForm.Button1Click(Sender: TObject);
begin
  FontDialog1.Font := rxLabel1.Font;
  if FontDialog1.Execute then
    rxLabel1.Font := FontDialog1.Font;
end;

procedure TControlsForm.FileListBox1Change(Sender: TObject);
begin
  try
    Memo1.Lines.LoadFromFile(FileListBox1.FileName);
  except
    Memo1.Clear;
  end;
end;

procedure TControlsForm.ComboBox7Change(Sender: TObject);
var
  Animated: Boolean;
begin
  Animated := AnimatedImage1.Active;
  AnimatedImage1.Active := False;
  case ComboBox7.ItemIndex of
    0:
      begin
        AnimatedImage1.Glyph := RunnerImage.Picture.Bitmap;
        AnimatedImage1.NumGlyphs := 12;
      end;
    1:
      begin
        AnimatedImage1.Glyph := FlagImage.Picture.Bitmap;
        AnimatedImage1.NumGlyphs := 4;
      end;
    2:
      begin
        AnimatedImage1.Glyph := SearchImage.Picture.Bitmap;
        AnimatedImage1.NumGlyphs := 12;
      end;
    3:
      begin
        AnimatedImage1.Glyph := BookImage.Picture.Bitmap;
        AnimatedImage1.NumGlyphs := 5;
      end;
  end;
  AnimatedImage1.Active := Animated;
end;

procedure TControlsForm.Button2Click(Sender: TObject);
begin
  AnimatedImage1.Active := not AnimatedImage1.Active;
end;

procedure TControlsForm.SpinEdit3Change(Sender: TObject);
begin
  AnimatedImage1.Interval := SpinEdit3.Value;
end;

procedure TControlsForm.SpinEdit4Change(Sender: TObject);
begin
  AnimatedImage1.GlyphNum := SpinEdit4.Value;
end;

procedure TControlsForm.AnimatedImage1StartStop(Sender: TObject);
begin
  SpinEdit4.Value := AnimatedImage1.GlyphNum;
  SpinEdit3.Value := AnimatedImage1.Interval;
  if AnimatedImage1.Active then
    Button2.Caption := 'Stop'
  else
    Button2.Caption := 'Start';
end;

procedure TControlsForm.ComboBox8Change(Sender: TObject);
begin
  rxClock1.ShowMode := TShowClock(ComboBox8.ItemIndex);
end;

procedure TControlsForm.Button3Click(Sender: TObject);
begin
  FontDialog1.Font := rxClock1.Font;
  if FontDialog1.Execute then
    rxClock1.Font := FontDialog1.Font;
end;

procedure TControlsForm.CheckBox6Click(Sender: TObject);
begin
  rxClock1.AlarmEnabled := CheckBox6.Checked;
end;

procedure TControlsForm.SpinEdit5Change(Sender: TObject);
begin
  rxClock1.AlarmHour := SpinEdit5.Value;
end;

procedure TControlsForm.SpinEdit6Change(Sender: TObject);
begin
  rxClock1.AlarmMinute := SpinEdit6.Value;
end;

procedure TControlsForm.SpinEdit7Change(Sender: TObject);
begin
  rxClock1.AlarmSecond := SpinEdit7.Value;
end;

procedure TControlsForm.rxClock1Alarm(Sender: TObject);
begin
  MessageDlg('Alarm!', mtInformation, [mbOk], 0);
end;

procedure TControlsForm.Button4Click(Sender: TObject);
begin
  rxDice1.Rotate := not rxDice1.Rotate;
  if rxDice1.Rotate then
    Button4.Caption := 'Stop'
  else
    Button4.Caption := 'Start';
end;

procedure TControlsForm.SpinEdit8Change(Sender: TObject);
begin
  rxDice1.Interval := SpinEdit8.Value;
end;

procedure TControlsForm.SpinEdit9Change(Sender: TObject);
begin
 rxDice1.Value := SpinEdit9.Value;
end;

procedure TControlsForm.rxDice1Change(Sender: TObject);
begin
  SpinEdit9.Value := rxDice1.Value;
end;

procedure TControlsForm.Button5Click(Sender: TObject);
begin
  with TextListBox1.Items do begin
    if Count = 3 then begin
      Insert(2, 'Very Long Item Very Long Item Very Long Item');
      Button5.Caption := 'Back';
    end
    else begin
      Delete(2);
      Button5.Caption := 'Show';
    end;
  end;
end;

procedure TControlsForm.ComboBox9Change(Sender: TObject);
begin
  rxSpinEdit1.ValueType := TValueType(ComboBox9.ItemIndex);
end;

procedure TControlsForm.SpinEdit1Change(Sender: TObject);
begin
  rxSpinEdit1.Increment := SpinEdit1.Value;
end;

procedure TControlsForm.CheckBox4Click(Sender: TObject);
begin
  ColorComboBox1.DisplayNames := CheckBox4.Checked;
end;

procedure TControlsForm.Button6Click(Sender: TObject);
var
  Image: TAnimatedCursorImage;
begin
  if OpenDialog.Execute then begin
    OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
    Image := TAnimatedCursorImage.Create;
    try
      AnimatedImage1.Active := False;
      Image.LoadFromFile(OpenDialog.FileName);
      Image.AssignToBitmap(AnimatedImage1.Glyph, clFuchsia, False,
        AnimatedImage1.Orientation = goVertical);
      ComboBox7.ItemIndex := -1;
      AnimatedImage1.Interval := Image.DefaultRate;
      SpinEdit3.Value := AnimatedImage1.Interval;
      AnimatedImage1.TransparentColor := clFuchsia;
      AnimatedImage1.Active := True;
    finally
      Image.Free;
    end;
  end;
end;

initialization
  RegisterClasses([TBevel]);
end.
