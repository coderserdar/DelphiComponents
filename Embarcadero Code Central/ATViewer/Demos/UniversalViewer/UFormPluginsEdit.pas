unit UFormPluginsEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls;

type
  TFormPluginsEdit = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    boxMain: TGroupBox;
    labFilename: TLabel;
    edFilename: TEdit;
    labDetect: TLabel;
    edDetect: TEdit;
    btnDetectDefault: TButton;
    labEditIni: TLabel;
    btnEditIni: TButton;
    procedure btnDetectDefaultClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnEditIniClick(Sender: TObject);
  private
    { Private declarations }
    FIniName: AnsiString; //No support for WideString currently
  public
    { Public declarations }
  end;

implementation

uses
  ATxMsgProc, ATxUtils, ATxFProc, WLXProc;

{$R *.DFM}

procedure TFormPluginsEdit.btnDetectDefaultClick(Sender: TObject);
begin
  edDetect.Text:= WlxGetDetectString(edFilename.Text);
end;

procedure TFormPluginsEdit.FormShow(Sender: TObject);
begin
  {$I Lang.FormPluginsEdit.inc}

  FIniName:= ChangeFileExt(edFilename.Text, '.ini');
  btnEditIni.Enabled:= IsFileExist(FIniName);
end;

procedure TFormPluginsEdit.btnEditIniClick(Sender: TObject);
begin
  FOpenURL(FIniName, Handle);
end;

end.
