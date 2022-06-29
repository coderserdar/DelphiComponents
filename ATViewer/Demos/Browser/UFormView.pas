unit UFormView;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  VirtualExplorerTree, VirtualTrees, ExtCtrls,
  StdCtrls, ComCtrls, ATViewer;

type
  TFormView = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    TreeEx: TVirtualExplorerTreeview;
    Splitter2: TSplitter;
    LVEx: TVirtualExplorerListview;
    boxViewer: TGroupBox;
    Viewer: TATViewer;
    boxOptions: TGroupBox;
    boxModeOptions: TGroupBox;
    pnText: TPanel;
    chkTextWrap: TCheckBox;
    pnMedia: TPanel;
    chkMediaFit: TCheckBox;
    labScale: TLabel;
    TrackBar1: TTrackBar;
    chkImageResample: TCheckBox;
    chkTextOEM: TCheckBox;
    boxMode: TGroupBox;
    chkModeDetect: TCheckBox;
    chkModeText: TRadioButton;
    chkModeBinary: TRadioButton;
    chkModeHex: TRadioButton;
    chkModeMedia: TRadioButton;
    chkModeWeb: TRadioButton;
    chkModeUnicode: TRadioButton;
    chkModeRTF: TRadioButton;
    procedure LVExChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure chkModeDetectClick(Sender: TObject);
    procedure chkModeTextClick(Sender: TObject);
    procedure chkModeBinaryClick(Sender: TObject);
    procedure chkModeHexClick(Sender: TObject);
    procedure chkModeMediaClick(Sender: TObject);
    procedure chkModeWebClick(Sender: TObject);
    procedure chkModeUnicodeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure chkModeRTFClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure chkMediaFitClick(Sender: TObject);
    procedure chkTextWrapClick(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkImageResampleClick(Sender: TObject);
    procedure chkTextOEMClick(Sender: TObject);
  private
    { Private declarations }
    FLoad: boolean;
    FLabelUpdate: boolean;
    procedure UpdateImageLabel;
  public
    { Public declarations }
    procedure UpdateView(en: boolean);
  end;

var
  FormView: TFormView;

implementation

uses
  ATBinHex, ATxSProc, ATxFProc, ATxCodepages,
  {$ifdef TNT} TntForms, {$endif}
  VirtualShellUtilities;

{$R *.DFM}

procedure TFormView.LVExChange(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  NS: TNamespace;
  S: WideString;
begin
  with LVEx do
    if ValidateNameSpace(GetFirstSelected, NS) then
      begin
      S:= NS.NameForParsing;
      if IsFileExist(S)
        then begin if Viewer.Open(S) then UpdateView(true); end
        else begin Viewer.Open(''); UpdateView(false); end;
      UpdateImageLabel;
      end;
end;

procedure TFormView.FormCreate(Sender: TObject);
begin
  FLoad:= false;
  FLabelUpdate:= false;
  Viewer.OnOptionsChange:= FormResize;
end;

procedure TFormView.chkModeDetectClick(Sender: TObject);
begin
  if not FLoad then
    Viewer.ModeDetect:= chkModeDetect.Checked;
end;

procedure TFormView.chkModeTextClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vmodeText;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeBinaryClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vmodeBinary;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeHexClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vmodeHex;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeMediaClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vmodeMedia;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeWebClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vmodeWeb;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeUnicodeClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vmodeUnicode;
    UpdateView(true);
    end;
end;

procedure TFormView.chkModeRTFClick(Sender: TObject);
begin
  if not FLoad then
    begin
    Viewer.Mode:= vmodeRTF;
    UpdateView(true);
    end;
end;

procedure TFormView.UpdateView(en: boolean);
begin
  FLoad:= true;

  chkModeDetect.Checked:= Viewer.ModeDetect;
  chkModeText.Checked:= Viewer.Mode=vmodeText;
  chkModeBinary.Checked:= Viewer.Mode=vmodeBinary;
  chkModeHex.Checked:= Viewer.Mode=vmodeHex;
  chkModeMedia.Checked:= Viewer.Mode=vmodeMedia;
  chkModeWeb.Checked:= Viewer.Mode=vmodeWeb;
  chkModeUnicode.Checked:= Viewer.Mode=vmodeUnicode;
  chkModeRTF.Checked:= Viewer.Mode=vmodeRTF;

  chkModeText.Enabled:= en;
  chkModeBinary.Enabled:= en;
  chkModeHex.Enabled:= en;
  chkModeMedia.Enabled:= en;
  chkModeWeb.Enabled:= en;
  chkModeUnicode.Enabled:= en;
  chkModeRTF.Enabled:= en;

  pnText.Visible:= en and (Viewer.Mode in [vmodeText, vmodeBinary, vmodeHex, vmodeUnicode, vmodeRTF]);
  chkTextWrap.Enabled:= Viewer.Mode in [vmodeText, vmodeUnicode, vmodeRTF];
  chkTextWrap.Checked:= Viewer.TextWrap;
  chkTextOEM.Enabled:= Viewer.Mode in [vmodeText, vmodeBinary, vmodeHex];
  chkTextOEM.Checked:= Viewer.TextEncoding = vencOEM;

  pnMedia.Visible:= en and (Viewer.Mode=vmodeMedia);
  chkMediaFit.Checked:= Viewer.MediaFit;
  chkImageResample.Enabled:= Viewer.IsImage;
  chkImageResample.Checked:= Assigned(Viewer.ImageBox) and Viewer.ImageBox.Image.Resample;
  TrackBar1.Enabled:= Viewer.IsImage;

  FLoad:= false;
end;

procedure TFormView.FormShow(Sender: TObject);
var
  fn: WideString;
begin
  UpdateView(false);

  fn:= SExtractFilePath(Application.ExeName)+'Files';
  if IsDirExist(fn) then
    begin
    LVEx.RootFolder:= rfCustom;
    LVEx.RootFolderCustomPath:= fn;
    end;
end;

procedure TFormView.FormResize(Sender: TObject);
begin
  UpdateImageLabel;
end;

procedure TFormView.UpdateImageLabel;
begin
  with Viewer do
    if IsImage and Assigned(ImageBox) then
      begin
      ImageBox.ImageLabel.Visible:= true;
      ImageBox.ImageLabel.Caption:= Format(
        'Original size: %d x %d'#13'Current scale: %d%%',
        [ImageWidth, ImageHeight, ImageScale]);

      FLabelUpdate:= true;
      TrackBar1.Position:= ImageScale;
      FLabelUpdate:= false;
      end;
end;


procedure TFormView.chkTextWrapClick(Sender: TObject);
begin
  Viewer.TextWrap:= chkTextWrap.Checked;
end;

procedure TFormView.chkTextOEMClick(Sender: TObject);
begin
  if chkTextOEM.Checked then
    Viewer.TextEncoding:= vencOEM
  else
    Viewer.TextEncoding:= vencANSI;
end;

procedure TFormView.chkMediaFitClick(Sender: TObject);
begin
  Viewer.MediaFit:= chkMediaFit.Checked;
  UpdateImageLabel;
end;

procedure TFormView.chkImageResampleClick(Sender: TObject);
begin
  if Assigned(Viewer.ImageBox) then
    Viewer.ImageBox.Image.Resample:= chkImageResample.Checked;
end;

procedure TFormView.TrackBar1Change(Sender: TObject);
begin
  if not FLabelUpdate then
    with Viewer do
      if IsImage then
        begin
        ImageScale:= TrackBar1.Position;
        chkMediaFit.Checked:= Viewer.MediaFit;
        UpdateImageLabel;
        end;
end;

end.
