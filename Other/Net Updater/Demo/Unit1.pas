unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, kmNetUpdate, kmTypes;

type
  TForm1 = class(TForm)
    kmNetUpdate1: TkmNetUpdate;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Button1: TButton;
    Memo1: TMemo;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure CheckBox6Click(Sender: TObject);
    procedure CheckBox7Click(Sender: TObject);
    procedure CheckBox8Click(Sender: TObject);
    procedure CheckBox9Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormShow(Sender: TObject);
begin
  CheckBox1.Checked := kmNetUpdate1.Active;
  CheckBox2.Checked := kmNetUpdate1.CacheOptions = [];
  CheckBox3.Checked := kmNetUpdate1.CreateBackup;
  CheckBox4.Checked := kmNetUpdate1.HideFileLocation;
  CheckBox5.Checked := mAskUpgrade in kmNetUpdate1.ShowMessages;
  CheckBox6.Checked := mNoUpdateAvailable in kmNetUpdate1.ShowMessages;
  CheckBox7.Checked := kmNetUpdate1.StayOnTop;
  CheckBox8.Checked := kmNetUpdate1.WarnOnCancel;
  CheckBox9.Checked := kmNetUpdate1.WarnOnRestart;

  ComboBox1.ItemIndex := Integer(kmNetUpdate1.RunMode);
  Memo1.Lines.LoadFromFile(ChangeFileExt(Application.ExeName,'.txt'));
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  kmNetUpdate1.Check;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  kmNetUpdate1.RunMode := TnuRunMode(ComboBox1.ItemIndex);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  kmNetUpdate1.Active := CheckBox1.Checked;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  if CheckBox2.Checked then
    kmNetUpdate1.CacheOptions := [] else
    kmNetUpdate1.CacheOptions := [coAlwaysReload];
end;

procedure TForm1.CheckBox3Click(Sender: TObject);
begin
  kmNetUpdate1.CreateBackup := CheckBox3.Checked;
end;

procedure TForm1.CheckBox4Click(Sender: TObject);
begin
  kmNetUpdate1.HideFileLocation := CheckBox4.Checked;
end;

procedure TForm1.CheckBox5Click(Sender: TObject);
begin
  if CheckBox5.Checked then
    kmNetUpdate1.ShowMessages := kmNetUpdate1.ShowMessages + [mAskUpgrade] else
    kmNetUpdate1.ShowMessages := kmNetUpdate1.ShowMessages - [mAskUpgrade];
end;

procedure TForm1.CheckBox6Click(Sender: TObject);
begin
  if CheckBox6.Checked then begin
    kmNetUpdate1.ShowMessages := kmNetUpdate1.ShowMessages + [mNoUpdateAvailable];
    kmNetUpdate1.VersionNumber := '3.0';
    end
  else begin
    kmNetUpdate1.ShowMessages := kmNetUpdate1.ShowMessages - [mNoUpdateAvailable];
    kmNetUpdate1.VersionNumber := '1.0';
    end;
end;

procedure TForm1.CheckBox7Click(Sender: TObject);
begin
  kmNetUpdate1.StayOnTop:= CheckBox7.Checked;
end;

procedure TForm1.CheckBox8Click(Sender: TObject);
begin
  kmNetUpdate1.WarnOnCancel := CheckBox8.Checked;
end;

procedure TForm1.CheckBox9Click(Sender: TObject);
begin
  kmNetUpdate1.WarnOnRestart := CheckBox9.Checked;
end;

end.
