unit fmabout;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, shellapi;

type
  TfAbout = class(TForm)
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    OKButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowAbout;

implementation

{$R *.dfm}

procedure ShowAbout;
begin
  with TfAbout.Create(Application) do
  ShowModal;
end;

procedure TfAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfAbout.Label1Click(Sender: TObject);
begin
  ShellExecute(0, 'open', 'http://www.filedepot.eu', nil, nil, SW_NORMAL);
end;

end.
 
