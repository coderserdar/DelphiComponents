unit fmDelete;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, unTranslation;

type
  TDelete = class(TForm)
    rgFiles: TRadioGroup;
    edFiles: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure edFilesChange(Sender: TObject);
  private
    { Déclarations privées }
    procedure WMTranslate( var Message : TMessage ); message WM_TRANSLATE;
  public
    { Déclarations publiques }
  end;

var
  Delete: TDelete;

implementation

{$R *.DFM}

procedure TDelete.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if (ModalResult = mrOk) and (rgFiles.ItemIndex = 2) then
    begin
      CanClose := edFiles.Text <> '';
      if not CanClose then
        begin
          MessageDlg( 'You must enter a filter !', mtWarning, [mbOk], 0 );
          edFiles.SetFocus;
        end;
    end;
end;

procedure TDelete.edFilesChange(Sender: TObject);
begin
  rgFiles.ItemIndex := 2;
end;

procedure TDelete.WMTranslate( var Message : TMessage );
begin
  rgFiles.Items.Text := GetStr(1202);
end;

end.
