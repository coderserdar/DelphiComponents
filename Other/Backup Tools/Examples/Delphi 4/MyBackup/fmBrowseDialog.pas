unit fmBrowseDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, FileCtrl;

type
  TBrowseDialog = class(TForm)
    DriveComboBox1: TDriveComboBox;
    DirectoryListBox1: TDirectoryListBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    edPath: TEdit;
    Label1: TLabel;
    procedure DirectoryListBox1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  BrowseDialog: TBrowseDialog;

implementation

{$R *.DFM}

procedure TBrowseDialog.DirectoryListBox1Change(Sender: TObject);
begin
  with Sender as TDirectoryListBox do
    edPath.Text := Directory;
end;

procedure TBrowseDialog.FormShow(Sender: TObject);
begin
  with DirectoryListBox1 do
    edPath.Text := Directory;
end;

end.
