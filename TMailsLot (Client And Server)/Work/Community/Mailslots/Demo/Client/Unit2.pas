unit Unit2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Mailslots;

type
  TForm2 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    ComboBox1: TComboBox;
    Edit2: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    MailslotClient1: TMailslotClient;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  Form2: TForm2;

implementation

{$R *.DFM}

procedure TForm2.FormCreate(Sender: TObject);
begin
   Combobox1.ItemIndex := 3;
   Edit2.Text := '.';
   Edit2.Readonly := False;

end;

procedure TForm2.ComboBox1Change(Sender: TObject);
begin
   Case ComboBox1.ItemIndex Of
      0: Begin
            Edit2.Text := '*';
            Edit2.Enabled := False;
         End;
      1, 2: Begin
            Edit2.Text := '';
            Edit2.Enabled := True;
         End;
      3: Begin
            Edit2.Text := '.';
            Edit2.Enabled := False;
         End;
   End;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
   MailslotClient1.Active := False;
   Case ComboBox1.ItemIndex Of
      0: MailslotClient1.MailslotKind := mkBroadcast;
      1: Begin
            MailslotClient1.MailslotKind := mkComputer;
            MailslotClient1.MailslotTarget := Edit2.Text;
         End;
      2: Begin
            MailslotClient1.MailslotKind := mkDomain;
            MailslotClient1.MailslotTarget := Edit2.Text;            
         End;
      3: MailslotClient1.MailslotKind := mkLocal;
   End;
   MailslotClient1.MailslotName := Edit1.Text;
   MailslotClient1.MessageString := Memo1.Text;
   MailslotClient1.Active := True;
end;

end.
