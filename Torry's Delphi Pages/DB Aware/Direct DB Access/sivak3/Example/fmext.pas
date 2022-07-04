unit fmext;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  Tfext = class(TForm)
    Combo: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure ComboChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetExtension(OwnerForm: TForm; var res: String): Boolean;

implementation

uses udm;

{$R *.dfm}

function GetExtension(OwnerForm: TForm; var res: String): Boolean;
begin
  with Tfext.Create(OwnerForm) do
  try
    Result := ShowModal = mrOK;
    if Result then
    res := Trim(Combo.Text);
  finally
    Free;
  end;
end;

procedure Tfext.ComboChange(Sender: TObject);
begin
  Button1.Enabled := not str_empty(Trim(Combo.Text));
end;

end.
