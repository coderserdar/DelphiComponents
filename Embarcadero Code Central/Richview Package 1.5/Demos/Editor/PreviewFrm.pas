unit PreviewFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RVScroll, CRVPP, RVPP, ExtCtrls;

type
  TfrmPreview = class(TForm)
    Panel1: TPanel;
    rvpp: TRVPrintPreview;
    cmb: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure cmbExit(Sender: TObject);
    procedure cmbKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rvppZoomChanged(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateZoom; 
  public
    { Public declarations }
  end;

var
  frmPreview: TfrmPreview;

implementation

{$R *.DFM}
{-----------------------------------------------------------------------}
procedure TfrmPreview.Button1Click(Sender: TObject);
begin
  rvpp.First;
  Label1.Caption :=Format('%d of %d', [rvpp.PageNo, rvpp.RVPrint.PagesCount]);
end;
{-----------------------------------------------------------------------}
procedure TfrmPreview.Button2Click(Sender: TObject);
begin
  rvpp.Prev;
  Label1.Caption :=Format('%d of %d', [rvpp.PageNo, rvpp.RVPrint.PagesCount]);
end;
{-----------------------------------------------------------------------}
procedure TfrmPreview.Button3Click(Sender: TObject);
begin
  rvpp.Next;
  Label1.Caption :=Format('%d of %d', [rvpp.PageNo, rvpp.RVPrint.PagesCount]);
end;
{-----------------------------------------------------------------------}
procedure TfrmPreview.Button4Click(Sender: TObject);
begin
  rvpp.Last;
  Label1.Caption :=Format('%d of %d', [rvpp.PageNo, rvpp.RVPrint.PagesCount]);
end;
{-----------------------------------------------------------------------}
procedure TfrmPreview.cmbExit(Sender: TObject);
begin
  UpdateZoom;
end;
{-----------------------------------------------------------------------}
procedure TfrmPreview.cmbKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_RETURN then begin
    UpdateZoom;
    Key := 0;
  end;
end;
{-----------------------------------------------------------------------}
procedure TfrmPreview.UpdateZoom;
var s: String;
    zoom: Integer;
begin
  s := Trim(cmb.Text);
  if s='Page width' then begin
    rvpp.ZoomMode := rvzmPageWidth;
    exit;
  end;
  if s='Full page' then begin
    rvpp.ZoomMode := rvzmFullPage;
    exit;
  end;
  if (s<>'') and (s[Length(s)]='%') then
    s := Copy(s,1,Length(s)-1);
  zoom := StrToIntDef(s,0);
  if (zoom<10) or (zoom>500) then
    Application.MessageBox('Please enter number from 10 to 500','Scale',MB_OK or MB_ICONSTOP)
  else
    rvpp.SetZoom(zoom);
end;
{-----------------------------------------------------------------------}
procedure TfrmPreview.rvppZoomChanged(Sender: TObject);
begin
  cmb.Text := IntToStr(rvpp.ZoomPercent)+'%';
end;

end.
