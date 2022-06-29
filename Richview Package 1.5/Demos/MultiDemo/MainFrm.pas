unit MainFrm;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVStyle,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  MMSystem;

type
  TfrmMain = class(TForm)
    rv: TRichView;
    rvs: TRVStyle;
    rvTop: TRichView;
    rvsForDemos: TRVStyle;
    il: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure rvJump(Sender: TObject; id: Integer);
    procedure rvRVMouseMove(Sender: TObject; id: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;


{ Constants for text styles, rvsForDemos.TextStyles }
const sncomNormal = 0;
      sncomHeading = 1;
      sncomKeyword = 2;
      sncomMarked = 3;

{ Constants for text styles, rvs.TextStyles }
const snNormal  = 0;
      snHeading = 1;
      snExit    = 3;
      snJump    = 4;

implementation

uses PopupFrm,
     Demo1Frm, Demo2Frm, Demo3Frm,
     Demo4Frm, Demo5Frm, Demo6Frm,
     Demo7Frm;
{$R *.DFM}
{-----------------------------------------------------}
procedure TfrmMain.FormCreate(Sender: TObject);
begin

  rvTop.BackgroundBitmap := rv.BackgroundBitmap;
  rvTop.AddNL(' ', snNormal, 1);
  rvTop.AddNL('RichView Demos', snHeading, 1);
  rvTop.AddNL(' ', snNormal, 1);
  rvTop.Format;
  rvTop.Height := rvTop.DocumentHeight;

  rv.AddNL('Click links for demos, click balls for hints', snNormal, 1);
  rv.AddHotspotExTag('', 0,1,il, 1, 1);
  rv.Add('1. ', snNormal);
  rv.AddTag('"Checkpoints"', snJump, 1);
  rv.AddHotspotExTag('', 0,1,il, 1, 2);
  rv.Add('2. ', snNormal);
  rv.AddTag('Customizing Styles', snJump, 2);
  rv.AddHotspotExTag('', 0,1,il, 1, 3);
  rv.Add('3. ', snNormal);
  rv.AddTag('Query Summary', snJump, 3);
  rv.AddHotspotExTag('', 0,1,il, 1, 4);
  rv.Add('4. ', snNormal);
  rv.AddTag('Interactive document', snJump, 4);
  rv.AddHotspotExTag('', 0,1,il, 1, 5);
  rv.Add('5. ', snNormal);
  rv.AddTag('Chat Simulation', snJump, 5);
  rv.AddHotspotExTag('', 0,1,il, 1, 6);
  rv.Add('6. ', snNormal);
  rv.AddTag('Credits Demo', snJump, 6);
  rv.AddHotspotExTag('', 0,1,il, 1, 7);
  rv.Add('7. ', snNormal);
  rv.AddTag('Search and mark', snJump, 7);
  rv.AddBreakEx(2, rvbsLine, clGreen);
  rv.AddNL('Exit', snExit, 1);
  rv.Format;
end;
{-----------------------------------------------------}
procedure TfrmMain.rvJump(Sender: TObject; id: Integer);
var ItemNo, StyleNo, ItemTag: Integer;
    frm: TFrmPopup;
    frmDemo: TForm;
begin
  ItemNo  := rv.GetJumpPointItemNo(id);
  StyleNo := rv.GetItemStyle(ItemNo);
  ItemTag := rv.GetItemTag(ItemNo);
  if StyleNo=rvsHotspot then begin
    frm := TFrmPopup.Create(Self);
    frm.ShowTopic(ItemTag);
    end
  else begin
    frmDemo := nil;
    case ItemTag of
      1: frmDemo := TfrmDemo1.Create(Application);
      2: frmDemo := TfrmDemo2.Create(Application);
      3: frmDemo := TfrmDemo3.Create(Application);
      4: frmDemo := TfrmDemo4.Create(Application);
      5: frmDemo := TfrmDemo5.Create(Application);
      6: frmDemo := TfrmDemo6.Create(Application);
      7: frmDemo := TfrmDemo7.Create(Application);      
      0: Close;
    end;
    if frmDemo<>nil then begin
      frmDemo.ShowModal;
      frmDemo.Free;
    end;
  end;
end;
{-----------------------------------------------------}
procedure TfrmMain.rvRVMouseMove(Sender: TObject; id: Integer);
begin
  if id<>-1 then
    PlaySound(PChar(ExtractFilePath(Application.ExeName)+'UCS.wav'),
              0,SND_ASYNC or SND_FILENAME);
end;

end.
