unit u_bk8x00_settings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls;

type
  Tfbk8x00_settings = class(TForm)
    Panel1: TPanel;
    Apply: TButton;
    Cancel: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    procedure CancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ApplyClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    fport: integer;
    fbaudrate: integer;
    fbusaddress: integer;
    fwritelength: integer;
    freadlength: integer;
    fexecute: boolean;
  end;

var
  fbk8x00_settings: Tfbk8x00_settings;

implementation

uses
  API_bk8x00;

{$R *.dfm}

procedure Tfbk8x00_settings.CancelClick(Sender: TObject);
begin
  close;
end;

procedure Tfbk8x00_settings.FormShow(Sender: TObject);
begin
  fexecute:= false;
  combobox1.ItemIndex:= fport;
  combobox2.text:= inttostr(fbaudrate);
  edit1.text:= inttostr(fbusaddress);
  edit2.text:= inttostr(fwritelength);
end;

procedure Tfbk8x00_settings.ApplyClick(Sender: TObject);
begin
  try
    fport:= combobox1.itemindex;
    fbaudrate:= strtoint(combobox2.text);
    fbusaddress:= strtoint(edit1.text);
    fwritelength:= strtoint(edit2.text);
  except
    messagedlg('Error converting to numbers.', mterror, [mbok], 0);
    exit;
  end;
  fexecute:=true;
  close;
end;

procedure Tfbk8x00_settings.FormCreate(Sender: TObject);
var
  i: integer;
begin
  // create combobox contents
  combobox1.Clear;
  combobox1.Items.add('None');
  for i:=1 to 256 do
    combobox1.items.add('Port '+inttostr(i));
end;

end.
