unit fmSelectFiles;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type
  TSelectFiles = class(TForm)
    Panel1: TPanel;
    lbFiles: TListBox;
    BitBtn1: TBitBtn;
    btnAddFiles: TButton;
    btnClearAll: TButton;
    OpenDialog1: TOpenDialog;
    procedure btnClearAllClick(Sender: TObject);
    procedure btnAddFilesClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  SelectFiles: TSelectFiles;

implementation

{$R *.DFM}

procedure TSelectFiles.btnClearAllClick(Sender: TObject);
begin
  lbFiles.Items.Clear;
end;

procedure TSelectFiles.btnAddFilesClick(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      if Execute then
        lbFiles.Items.Assign( Files );
    end;
end;

end.
