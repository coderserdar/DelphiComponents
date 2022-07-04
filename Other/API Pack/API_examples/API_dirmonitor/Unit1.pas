unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, API_files, StdCtrls, ExtCtrls, API_dirmonitor;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    API_files1: TAPI_files;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    API_DirMonitor1: TAPI_DirMonitor;
    procedure Edit1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure API_DirMonitor1Change(sender: TObject; var keepactive: boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Edit1Click(Sender: TObject);
begin
  edit1.text:= api_files1.BrowseFolderDialog('Select Folder to Monitor',edit1.text,false,false);
  api_dirmonitor1.Folder:= edit1.text;
end;

procedure TForm1.API_DirMonitor1Change(sender: TObject; var keepactive: boolean);
begin
  label1.Caption:= 'Changed '+datetimetostr(now);
  keepactive:= true;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  api_dirmonitor1.Active:= checkbox1.checked;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if api_dirmonitor1.Active then
  begin
    label2.Caption:= 'Active';
  end else
  begin
    label2.caption:= 'Stopped';
    checkbox1.checked:= false;
  end;
end;

end.
