unit Demo2Frm;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVStyle,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}  
  Menus;

type
  TfrmDemo2 = class(TForm)
    rv: TRichView;
    rvs: TRVStyle;
    fdlg: TFontDialog;
    cdlg: TColorDialog;
    pm: TPopupMenu;
    mitBack: TMenuItem;
    mitBreak: TMenuItem;
    mitText: TMenuItem;
    mitHighlight: TMenuItem;
    il: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rvRVMouseDown(Sender: TRichView; Button: TMouseButton;
      Shift: TShiftState; ItemNo, X, Y: Integer);
    procedure mitBackClick(Sender: TObject);
    procedure mitBreakClick(Sender: TObject);
    procedure mitTextClick(Sender: TObject);
    procedure rvRVMouseUp(Sender: TRichView; Button: TMouseButton;
      Shift: TShiftState; ItemNo, X, Y: Integer);
    procedure mitHighlightClick(Sender: TObject);
  private
    { Private declarations }
    procedure ChangeBackgroundColor;
    procedure ChangeTextStyle(StyleNo: Integer);
    procedure ChangeBreakColor(ItemNo: Integer);
    procedure ChangeHighlightColor(StyleNo: Integer);    
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}
{-----------------------------------------------------------}
procedure TfrmDemo2.ChangeBackgroundColor;
begin
  cdlg.Color := rvs.Color;
  if cdlg.Execute then begin
    rvs.Color := cdlg.Color;
    rv.Invalidate;
  end;
end;
{-----------------------------------------------------------}
procedure TfrmDemo2.ChangeBreakColor(ItemNo: Integer);
var BreakWidth,BreakWidth2: Byte;
    BreakTag, i: Integer;
    BreakColor: TColor;
    BreakStyle: TRVBreakStyle;
begin
  rv.GetBreakInfo(ItemNo, BreakWidth, BreakStyle, BreakColor, BreakTag);
  cdlg.Color := BreakColor;
  // RichView has no styles of "breaks", each "break" is individual
  // So for changing color of all "breaks" with specified width we need
  // to check all document
  if cdlg.Execute then begin
    for i := 0 to rv.ItemCount-1 do
      if rv.GetItemStyle(i)=rvsBreak then begin
        rv.GetBreakInfo(i, BreakWidth2, BreakStyle, BreakColor, BreakTag);
        if BreakWidth2=BreakWidth then
          rv.SetBreakInfo(i, BreakWidth2, BreakStyle, cdlg.Color, BreakTag);
      end;
    rv.Invalidate;
  end;
end;  
{-----------------------------------------------------------}
procedure TfrmDemo2.ChangeHighlightColor(StyleNo: Integer);
begin
  cdlg.Color := rvs.TextStyles[StyleNo].HoverColor;
  if cdlg.Execute then
    rvs.TextStyles[StyleNo].HoverColor := cdlg.Color;
end;
{-----------------------------------------------------------}
procedure TfrmDemo2.ChangeTextStyle(StyleNo: Integer);
begin
  fdlg.Font.Assign(rvs.TextStyles[StyleNo]);
  if fdlg.Execute then begin
    rvs.TextStyles[StyleNo].Assign(fdlg.Font);
    rv.Format;
  end;
end;
{-----------------------------------------------------------}
procedure TfrmDemo2.FormCreate(Sender: TObject);
begin
  rv.AddNL('Click on text, line or background to customize',1,1);
  rv.AddNL('Right click for menu',1,1);
  rv.AddBreakEx(1,rvbsLine,clGreen);
  rv.AddBulletEx('', 0, il, 0);
  rv.Add(' - thin line', 0);
  rv.AddBulletEx('', 1, il, 0);
  rv.Add(' - thick line', 0);
  rv.AddBreakEx(2,rvbsLine,clSilver);
  rv.AddNL('', 0,0);
  rv.AddNL('This is a normal text with ', 0,0);
  rv.Add('hypertext jump',2);
  rv.Add('.',0);
  rv.AddNL('', 0,0);
  rv.AddNL('This is a bottom text ', 3,0);
  rv.AddBreakEx(1,rvbsLine,clGreen);
  rv.AddNL('ESC closes window',1,1);
  rv.Format;
end;
{-----------------------------------------------------------}
procedure TfrmDemo2.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key=VK_ESCAPE then Close;
end;
{-----------------------------------------------------------}
procedure TfrmDemo2.rvRVMouseDown(Sender: TRichView; Button: TMouseButton;
  Shift: TShiftState; ItemNo, X, Y: Integer);
var StyleNo: Integer;
begin
  if Button<>mbLeft then exit;
  if ItemNo=-1 then begin
    ChangeBackgroundColor;
    exit;
  end;
  StyleNo := rv.GetItemStyle(ItemNo);
  case StyleNo of
    rvsBullet:
      Application.MessageBox('This is just a pointer to "break"', 'Bullet',
                             MB_OK or MB_ICONINFORMATION);
    rvsBreak:
      ChangeBreakColor(ItemNo);
    else
      ChangeTextStyle(StyleNo);
  end;
end;
{-----------------------------------------------------------}
procedure TfrmDemo2.rvRVMouseUp(Sender: TRichView; Button: TMouseButton;
  Shift: TShiftState; ItemNo, X, Y: Integer);
var StyleNo: Integer;
    p: TPoint;
begin
  if Button<>mbRight then exit;
  pm.Tag := ItemNo;
  StyleNo := 0;// avoiding warning
  if ItemNo<>-1 then
    StyleNo := rv.GetItemStyle(ItemNo);

  mitBack.Visible := ItemNo=-1;
  mitBreak.Visible := (ItemNo>=0) and (StyleNo=rvsBreak);
  mitText.Visible := (ItemNo>=0) and (StyleNo>=0);
  mitHighlight.Visible := mitText.Visible and rvs.TextStyles[StyleNo].Jump;
  p := rv.ClientToScreen(Point(X,Y));
  pm.Popup(p.X,p.Y);
end;
{-----------------------------------------------------------}
procedure TfrmDemo2.mitBackClick(Sender: TObject);
begin
  ChangeBackgroundColor;
end;

procedure TfrmDemo2.mitBreakClick(Sender: TObject);
begin
  ChangeBreakColor(pm.Tag);
end;

procedure TfrmDemo2.mitTextClick(Sender: TObject);
begin
  ChangeTextStyle(rv.GetItemStyle(pm.Tag));
end;

procedure TfrmDemo2.mitHighlightClick(Sender: TObject);
begin
  ChangeHighlightColor(rv.GetItemStyle(pm.Tag));
end;

end.
