unit fmconnect;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons, sql3_defs;

type  
  Tfconnect = class(TForm)
    edb: TEdit;
    SpeedButton1: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    edll: TEdit;
    SpeedButton2: TSpeedButton;
    RadioGroup1: TRadioGroup;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    OpenD: TOpenDialog;
    checkexists: TCheckBox;
    etitle: TEdit;
    Label3: TLabel;
    checkforeign: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure eChange(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
    procedure set_state;
  public
    { Public declarations }
  end;

function ConnectionDialog(db: TSivak3Database; var title: String): Integer;

implementation

uses udm;

{$R *.dfm}

function ConnectionDialog(db: TSivak3Database; var title: String): Integer;
begin
  with Tfconnect.Create(Application) do
  try
    etitle.Text := title;
    edb.Text := db.DatabaseFile;
    edll.Text := db.Driver;
    checkexists.Checked := not db.MustExists;
    checkforeign.Checked := Boolean(db.Tag); 
    set_state;
    Result := ShowModal;
    if Result = mrOK then
    begin
      title := etitle.Text;
      db.DatabaseFile := edb.Text;
      db.Driver := edll.Text;
      db.MustExists := not checkexists.Checked;
      db.Tag := ord(checkforeign.Checked);
    end;
  finally
    Free;
  end;
end;

{ Tfconnect }

procedure Tfconnect.set_state;
begin
  Button1.Enabled := not str_empty(edll.Text) and not str_empty(edb.Text) and not str_empty(etitle.Text);
  if Button1.Enabled then
  Button1.Enabled := FileExists(edll.Text);
  checkexists.Enabled := not FileExists(edb.Text);
  RadioGroup1.Enabled := checkexists.Enabled;
end;

procedure Tfconnect.eChange(Sender: TObject);
begin
  set_state;
end;

procedure Tfconnect.SpeedButton1Click(Sender: TObject);
begin
  OpenD.FileName := edb.Text;
  OpenD.Filter := 'Database file (*.dbs, *.db3)|*.dbs;*.db3';
  if OpenD.Execute then
  edb.Text := OpenD.FileName;
end;

procedure Tfconnect.SpeedButton2Click(Sender: TObject);
begin
  OpenD.FileName := edll.Text;
  OpenD.Filter := 'Library file (*.dll)|*.dll';
  if OpenD.Execute then
  edll.Text := OpenD.FileName;
end;

end.
