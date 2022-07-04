unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Buttons, Bitbtn, StdCtrls, ExtDlgs, Spin;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    OpenPictureDialog1: TOpenPictureDialog;
    CheckBox1: TCheckBox;
    Panel4: TPanel;
    DDMBitBtn2: TDDMBitBtn;
    GroupBox1: TGroupBox;
    Image1: TImage;
    GroupBox2: TGroupBox;
    DDMBitBtn3: TDDMBitBtn;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    SpinEdit1: TSpinEdit;
    DDMBitBtn1: TDDMBitBtn;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Button1: TButton;
    Image2: TImage;
    CheckBox4: TCheckBox;
    Edit1: TEdit;
    Splitter2: TSplitter;
    Label2: TLabel;
    CheckBox5: TCheckBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    DDMBitBtn4: TDDMBitBtn;
    DDMBitBtn5: TDDMBitBtn;
    DDMBitBtn6: TDDMBitBtn;
    DDMBitBtn7: TDDMBitBtn;
    CheckBox6: TCheckBox;
    procedure Image1Click(Sender: TObject);
    procedure CheckBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox3MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure CheckBox4MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure DDMBitBtn1ChangeGlyph(Sender: TObject);
    procedure CheckBox5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DDMBitBtn3Click(Sender: TObject);
    procedure DDMBitBtn1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckBox6MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Image1Click(Sender: TObject);
begin
  if openPictureDialog1.execute then
    image1.Picture.LoadFromFile(openPictureDialog1.filename);
end;

procedure TForm1.CheckBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DDMBitBtn1.Enabled := Checkbox1.checked;
end;

procedure TForm1.CheckBox2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DDMBitBtn1.Transparent := Checkbox2.checked;
end;

procedure TForm1.CheckBox3MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DDMBitBtn1.ShowCaption := Checkbox3.checked;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  DDMBitBtn1.NumGlyphs := SpinEdit1.Value;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if ColorDialog1.Execute then
  begin
    ddmbitbtn1.TransparentColor := ColorDialog1.Color;
  end;
end;

procedure TForm1.CheckBox4MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DDMBitBtn1.AutoSize := checkbox4.checked;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  Edit1.text := DDMBitBtn1.Caption;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  DDMBitBtn1.Caption := Edit1.Text;
end;

procedure TForm1.DDMBitBtn1ChangeGlyph(Sender: TObject);
begin
  showMessage('Changed');
end;

procedure TForm1.CheckBox5MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DDMBitBtn1.Down := CheckBox5.Checked;
end;

procedure TForm1.DDMBitBtn3Click(Sender: TObject);
begin
  DDMBitBtn1.Glyph.Assign(Image1.Picture.Bitmap);
end;

procedure TForm1.DDMBitBtn1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  checkbox5.Checked := DDMBitBtn1.Down;
end;

procedure TForm1.CheckBox6MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DDMBitBtn1.Flat := checkbox6.Checked;
end;

end.
