unit ce_db_create;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls;

type
  Tfce_db_create = class(TForm)
    epath: TEdit;
    encoding: TRadioGroup;
    SpeedButton1: TSpeedButton;
    Button1: TButton;
    Cancel: TButton;
    Label1: TLabel;
    SaveDlg: TSaveDialog;
    edesc: TEdit;
    Label2: TLabel;
    ChecOver: TCheckBox;
    procedure SpeedButton1Click(Sender: TObject);
    procedure epathChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure Tfce_db_create.SpeedButton1Click(Sender: TObject);
begin
  if SaveDlg.Execute then
  begin
    epath.Text := SaveDlg.FileName;
    ChecOver.Checked := true;
  end;
end;

procedure Tfce_db_create.epathChange(Sender: TObject);
begin
  Button1.Enabled := length(trim(epath.Text)) > 0;
end;

end.
