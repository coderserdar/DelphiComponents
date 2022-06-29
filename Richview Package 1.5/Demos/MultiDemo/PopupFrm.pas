unit PopupFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVStyle, RVScroll, RichView, ExtCtrls;

type
  TfrmPopup = class(TForm)
    Panel1: TPanel;
    rv: TRichView;
    rvs: TRVStyle;
    procedure rvKeyPress(Sender: TObject; var Key: Char);
    procedure rvClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure Build(DemoNo: Integer);
    procedure MoveToMouse;
  public
    { Public declarations }
    procedure ShowTopic(DemoNo: Integer);
  end;

implementation

{$R *.DFM}
uses MainFrm;

procedure TfrmPopup.rvKeyPress(Sender: TObject; var Key: Char);
begin
  Key:=#0;
  Close;
end;

procedure TfrmPopup.rvClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmPopup.FormDeactivate(Sender: TObject);
begin
  Close;
end;

procedure TfrmPopup.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmPopup.Build(DemoNo: Integer);
begin
  case DemoNo of
    1:
      begin
        rv.AddBulletEx('',1,frmMain.il,1);
        rv.Add('"Checkpoints"', 1);
        rv.AddNL('Synchronizing document scrolling with list of contents', 0,0);
        rv.AddNL('(This demo loads *.pas files from application directory)', 0,0);
      end;
    2:
      begin
        rv.AddBulletEx('',1,frmMain.il,1);
        rv.Add('Customizing Styles', 1);
        rv.AddNL('Interactive customizing of document', 0,0);
      end;
    3:
      begin
        rv.AddBulletEx('',1,frmMain.il,1);
        rv.Add('Query Summary', 1);
        rv.AddNL('Complex query at one look', 0,0);
      end;
    4:
      begin
        rv.AddBulletEx('',1,frmMain.il,1);
        rv.Add('Interactive document', 1);
        rv.AddNL('Document is a control itself', 0,0);
      end;
    5:
      begin
        rv.AddBulletEx('',1,frmMain.il,1);
        rv.Add('Chat Simulation', 1);
        rv.AddNL('Autoscroll, URL detection', 0,0);
      end;
    6:
      begin
        rv.AddBulletEx('',1,frmMain.il,1);
        rv.Add('Credits Demo:', 1);
        rv.AddNL('Scrolling text and images on timer', 0,0);
      end;
    7:
      begin
        rv.AddBulletEx('',1,frmMain.il,1);
        rv.Add('Search and mark:', 1);
        rv.AddNL('Search and mark words in editor', 0,0);
        rv.AddNL('(This demo loads MainFrm.pas from application directory)', 0,0);        
      end;
  end;
  rv.Format;
  Height := rv.DocumentHeight+20;
end;

procedure TfrmPopup.MoveToMouse;
var p: TPoint;
begin
  GetCursorPos(p);
  if p.x+Width>Screen.Width then
    p.x := Screen.Width-Width;
  if p.y+Height>Screen.Height then
    p.y := Screen.Height-Height;
  Left := p.x;
  Top  := p.y;
end;

procedure TfrmPopup.ShowTopic(DemoNo: Integer);
begin
  Build(DemoNo);
  MoveToMouse;
  Show;
end;

end.
