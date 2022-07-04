unit UFormViewRename;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, TntStdCtrls, ExtCtrls;

type
  TFormViewRename = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edFilename: TTntEdit;
    labRename: TLabel;
    Timer1: TTimer;
    procedure edFilenameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    FOldName: WideString;
  public
    { Public declarations }
  end;

implementation

uses
  ATxMsgProc, ATxSProc;

{$R *.dfm}

procedure TFormViewRename.edFilenameChange(Sender: TObject);
begin
  btnOK.Enabled:= (edFilename.Text <> '') and (edFilename.Text <> FOldName);
end;

procedure TFormViewRename.FormShow(Sender: TObject);
begin
  {$I Lang.FormViewRename.inc}
  FOldName:= edFilename.Text;
  edFilenameChange(Self);
end;

procedure TFormViewRename.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:= false;

  edFilename.SelStart:= 0;
  edFilename.SelLength:= Length(edFilename.Text) - Length(SExtractFileExt(edFilename.Text));
end;

end.
