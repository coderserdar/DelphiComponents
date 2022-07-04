unit main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, XPInformationDialog, XPStyler, XPInformationPanel, ExtCtrls,
  XPInformationBar, ImgList, XPBubbleHint;

type
  TForm1 = class(TForm)
    XPInformationBar1: TXPInformationBar;
    XPInformationPanel1: TXPInformationPanel;
    XPStyler1: TXPStyler;
    XPInformationDialog1: TXPInformationDialog;
    Label1: TLabel;
    Button1: TButton;
    Button2: TButton;
    XPInformationBar2: TXPInformationBar;
    XPInformationPanel2: TXPInformationPanel;
    Shape1: TShape;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    XPInformationBar3: TXPInformationBar;
    XPInformationPanel3: TXPInformationPanel;
    Label3: TLabel;
    XPInformationPanel4: TXPInformationPanel;
    Label4: TLabel;
    Button3: TButton;
    XPBubbleHint1: TXPBubbleHint;
    ImageList1: TImageList;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button2Click(Sender: TObject);
begin
 close;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 XPInformationDialog1.Execute;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 XPBubbleHint1.left:=Form1.left+XPInformationBar1.Left+XPInformationPanel1.Left+Button3.Left+(Button3.Width div 2);
 XPBubbleHint1.top:=Form1.top+XPInformationBar1.top+XPInformationPanel1.top+XPInformationPanel1.Headerheight+Button3.top+(Button3.height div 2);
 XPBubbleHint1.Execute;
end;

end.
