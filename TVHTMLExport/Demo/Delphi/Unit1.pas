unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, TVHTMLExport, Spin, ExtCtrls;

type
  TForm1 = class(TForm)
    TreeView1: TTreeView;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    Button1: TButton;
    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    CheckBox2: TCheckBox;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    Label4: TLabel;
    Edit3: TEdit;
    CheckBox7: TCheckBox;
    TVHTMLExport1: TTVHTMLExport;
    SpinEdit2: TSpinEdit;
    Label5: TLabel;
    HBold: TCheckBox;
    FBold: TCheckBox;
    Memo1: TMemo;
    Memo2: TMemo;
    Label6: TLabel;
    Memo3: TMemo;
    GroupBox1: TGroupBox;
    parseURLEvent: TCheckBox;
    canUse: TCheckBox;
    Edit1: TEdit;
    Label7: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SetElements;
    procedure Button1Click(Sender: TObject);
    procedure TVHTMLExport1ParseURL(URL: String; var UseURL: Boolean);
  private
    { Private-Deklarationen}
  public
    { Public-Deklarationen}
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if opendialog1.execute then
    treeview1.loadfromfile(opendialog1.filename);
end;

procedure TForm1.SetElements;
var
  i: integer;
begin
  with TVHTMLExport1 do
  begin
    Indent              := SpinEdit1.Value;
    ShowFolderImage     := CheckBox1.Checked;
    ShowBorder          := CheckBox2.Checked;
    UseInternalImages   := CheckBox3.Checked;
    Title.Show          := CheckBox4.Checked;
    Header.Show         := CheckBox5.Checked;
    Footer.Show         := CheckBox6.Checked;
    Title.text          := Edit3.text;
    Footer.Lines.Clear;
    Header.Lines.Clear;
    StyleSheet.Clear;
    for i := 0 to Memo1.lines.Count -1 do
      Header.Lines.Add(Memo1.Lines[i]);
    for i := 0 to Memo2.lines.Count -1 do
      Footer.Lines.Add(Memo2.Lines[i]);
    for i := 0 to Memo3.lines.Count -1 do
      StyleSheet.Add(Memo3.Lines[i]);
    CreateURL           := CheckBox7.Checked;
    Header.Bold         := HBold.Checked;
    Footer.Bold         := FBold.Checked;
    case Spinedit2.Value of
      1 : Title.Size := h1;
      2 : Title.Size := h2;
      3 : Title.Size := h3;
      4 : Title.Size := h4;
      5 : Title.Size := h5;
      6 : Title.Size := h6;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  SetElements;
  TVHTMLExport1.Show(TreeView1);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SetElements;
  TVHTMLExport1.SaveToHTML('c:\test.html',treeview1);
end;

procedure TForm1.TVHTMLExport1ParseURL(URL: String; var UseURL: Boolean);
begin
  if not parseURLEvent.checked then exit;
  showmessage(url);
  if Uppercase(Edit1.text) = Uppercase(url) then
    useurl := canuse.checked;
end;

end.
