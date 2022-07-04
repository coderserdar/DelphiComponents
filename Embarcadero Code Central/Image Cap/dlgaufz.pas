unit dlgaufz;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls;

type
  TDlgVPara = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Edit1: TEdit;
    CheckAudio: TCheckBox;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Edit3: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Edit4: TEdit;
    Label7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DlgVPara: TDlgVPara;

implementation

uses mainf;

{$R *.DFM}


procedure TDlgVPara.FormCreate(Sender: TObject);
begin
  with Main.VideoCap1 do
  begin
    Checkaudio.Checked := capaudio;
    Edit1.text:= inttostr(FrameRate);
    edit2.text:= inttostr(captimeLimit);
    edit3.text:= inttostr(CapIndexSize);
    edit4.text:= inttostr(BufferfileSize);
  end;
end;

procedure TDlgVPara.OKBtnClick(Sender: TObject);
var f,a,i,g:integer;
begin
 modalresult:= mrNone;
 f:= strtoInt(edit1.text);
 a:= strtoint(edit2.Text);
 i:= Strtoint(edit3.text);
 g:=strtoint( edit4.text);


 with Main.VideoCap1 do
  begin
   capAudio:=CheckAudio.Checked;
   frameRate:= f;
   captimeLimit:= a;
   capIndexSize:= i;
   BufferFileSize:= g;
  end;
 modalresult:= mrOk;

 end;

end.
