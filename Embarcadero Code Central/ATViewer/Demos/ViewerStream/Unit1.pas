unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ATViewer, StdCtrls, XPMan, ExtDlgs, ComCtrls,
  ATImageBox, jpeg, SHDocVw, ActiveX;

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    ATViewer1: TATViewer;
    GroupBox2: TGroupBox;
    XPManifest1: TXPManifest;
    OpenPictureDialog1: TOpenPictureDialog;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Memo1: TMemo;
    bFile: TButton;
    cText: TRadioButton;
    cBinary: TRadioButton;
    cHex: TRadioButton;
    cUni: TRadioButton;
    cRtf: TRadioButton;
    bText: TButton;
    Image1: TImage;
    bImage: TButton;
    bFile2: TButton;
    Label1: TLabel;
    bFree: TButton;
    TabSheet3: TTabSheet;
    Memo2: TMemo;
    bWeb: TButton;
    procedure bTextClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure bFileClick(Sender: TObject);
    procedure bFile2Click(Sender: TObject);
    procedure bImageClick(Sender: TObject);
    procedure bFreeClick(Sender: TObject);
    procedure bWebClick(Sender: TObject);
  private
    { Private declarations }
    MS: TMemoryStream;
    procedure LoadImageStream(Sender: TObject;
      ImageBox: TATImageBox; Stream: TStream);
    procedure LoadWebStream(Sender: TObject;
      Browser: TWebBrowser; Stream: TStream);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.bTextClick(Sender: TObject);
var
  M: TATViewerMode;
begin
  if cText.Checked then M:= vmodeText else
   if cBinary.Checked then M:= vmodeBinary else
    if cHex.Checked then M:= vmodeHex else
     if cUni.Checked then M:= vmodeUnicode else
      if cRtf.Checked then M:= vmodeRTF else
       M:= vmodeText;

  MS.Clear;
  Memo1.Lines.SaveToStream(MS);
  ATViewer1.OpenStream(MS, M);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MS:= TMemoryStream.Create;
  ATViewer1.OnLoadImageStream:= LoadImageStream;
  ATViewer1.OnLoadWebStream:= LoadWebStream;
  //PageControl1.ActivePageIndex:= 0;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  MS.Free;
end;

procedure TForm1.bFileClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      Memo1.Lines.LoadFromFile(FileName);
end;

procedure TForm1.bFile2Click(Sender: TObject);
begin
  with OpenPictureDialog1 do
    if Execute then
      Image1.Picture.LoadFromFile(FileName);
end;

procedure TForm1.bImageClick(Sender: TObject);
begin
  MS.Clear;
  Image1.Picture.Graphic.SaveToStream(MS);
  ATViewer1.OpenStream(MS, vmodeMedia);
end;

procedure TForm1.LoadImageStream(Sender: TObject; ImageBox: TATImageBox;
  Stream: TStream);
var
  J: TJpegImage;
  MF: TMetafile;
begin
  Stream.Position:= 0;
  if Image1.Picture.Graphic is TBitmap then
  begin
    ImageBox.Image.Picture.Bitmap.LoadFromStream(Stream);
    ImageBox.UpdateImageInfo;
  end
  else
  if Image1.Picture.Graphic is TJpegImage then
  begin
    J:= TJpegImage.Create;
    try
      J.LoadFromStream(Stream);
      ImageBox.Image.Picture.Assign(J);
      ImageBox.UpdateImageInfo;
    finally
      J.Free;
    end
  end
  else
  if Image1.Picture.Graphic is TMetafile then
  begin
    MF:= TMetafile.Create;
    try
      MF.LoadFromStream(Stream);
      ImageBox.Image.Picture.Assign(MF);
      ImageBox.UpdateImageInfo;
    finally
      MF.Free;
    end
  end  
  else
    Application.MessageBox('Cannot handle current picture type',
      'Error', mb_ok or mb_iconerror);

  ImageBox.Image.Resample := True;
  ImageBox.ImageLabel.Visible:= True;
  ImageBox.ImageLabel.Font.Color:= clBlue;
  ImageBox.ImageLabel.Caption:= Format('Stream size: %dK', [Stream.Size div 1024]);
end;

procedure TForm1.bFreeClick(Sender: TObject);
begin
  ATViewer1.OpenStream(nil, vmodeText);
end;

procedure WaitForBrowser(WB: TWebbrowser);
begin
   while (WB.ReadyState <> READYSTATE_COMPLETE)
     and not (Application.Terminated) do
   begin
     Application.ProcessMessages;
     Sleep(0);
   end;
end;

procedure WB_LoadDocFromStream(WB: TWebBrowser; Stream: TStream);
begin
  WB.Navigate('about:blank');
  WaitForBrowser(WB);
  (WB.Document as IPersistStreamInit).Load(TStreamAdapter.Create(Stream));
end;

procedure TForm1.bWebClick(Sender: TObject);
begin
  MS.Clear;
  Memo2.Lines.SaveToStream(MS);
  ATViewer1.OpenStream(MS, vmodeWeb);
end;

procedure TForm1.LoadWebStream(Sender: TObject; Browser: TWebBrowser;
  Stream: TStream);
begin
  Stream.Position:= 0;
  WB_LoadDocFromStream(Browser, Stream);
end;

end.
