unit fmCreateFolder;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons;

type
  TCreateFolder = class(TForm)
    Label1: TLabel;
    lPath: TLabel;
    edPath: TEdit;
    Label2: TLabel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  CreateFolder: TCreateFolder;

implementation

{$R *.DFM}

procedure TCreateFolder.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := (edPath.Text <> '') or (ModalResult <> mrOk);
end;

procedure TCreateFolder.FormShow(Sender: TObject);
begin
  edPath.Text := '';
end;

end.
