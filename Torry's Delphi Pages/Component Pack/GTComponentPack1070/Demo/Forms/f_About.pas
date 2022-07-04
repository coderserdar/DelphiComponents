unit f_About;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, StdCtrls, ComCtrls, o_FileInfoListView,
  o_LinkLabel;

type
  TFrmAbout = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    btnOk: TButton;
    GroupBox1: TGroupBox;
    gtFileInfoListView1: TgtFileInfoListView;
    Panel2: TPanel;
    InfoMemo: TMemo;
    gtLinkLabel1: TgtLinkLabel;
    procedure Image1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmAbout: TFrmAbout;


procedure ShowAboutDialog;


implementation

uses
   ShellApi
  ;

{$R *.dfm}

procedure ShowAboutDialog;
begin
  FrmAbout := TFrmAbout.Create(nil);
  try
    FrmAbout.gtFileInfoListView1.FileName := ParamStr(0);
    FrmAbout.gtFileInfoListView1.Columns[0].Width := 100;
    FrmAbout.gtFileInfoListView1.Columns[1].Width := 135;
    FrmAbout.ShowModal;
  finally
    FreeAndNil(FrmAbout);
  end;
end;


{------------------------------------------------------------------------------}
procedure TFrmAbout.Image1Click(Sender: TObject);
begin
  ShellExecute(Handle,'open','http://www.gtdelphicomponents.gr',nil,nil,0);
end;
{------------------------------------------------------------------------------}



end.
