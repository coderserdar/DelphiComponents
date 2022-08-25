unit uClipboard;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxColorComboBox,
  JvExForms, JvClipboardViewer, JvComponentBase, JvClipboardMonitor, PNGImage;

type
  TFormClipboard = class( TForm )
    Panel1: TPanel;
    Clear1: TButton;
    Close1: TButton;
    JvClipboardViewer1: TJvClipboardViewer;
    cxColorComboBox1: TcxColorComboBox;
    ClipboardFormatNames1: TComboBox;
    cxLookAndFeelController1: TcxLookAndFeelController;
    JvClipboardMonitor1: TJvClipboardMonitor;
    procedure Clear1Click( Sender: TObject );
    procedure Close1Click( Sender: TObject );
    procedure cxColorComboBox1PropertiesChange( Sender: TObject );
    procedure FormDestroy( Sender: TObject );
    procedure FormClose( Sender: TObject; var Action: TCloseAction );
    procedure JvClipboardMonitor1Change( Sender: TObject );
    procedure FormActivate( Sender: TObject );
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    CF_PNGIMAGE: word;
  public
    { Public declarations }
  end;

var
  FormClipboard: TFormClipboard;

implementation

uses Clipbrd, uMain;
{$R *.dfm}

procedure TFormClipboard.Clear1Click( Sender: TObject );
begin
  JvClipboardViewer1.EmptyClipboard;
  JvClipboardViewer1.Update;
end;

procedure TFormClipboard.Close1Click( Sender: TObject );
begin
  Close;
end;

procedure TFormClipboard.cxColorComboBox1PropertiesChange( Sender: TObject );
begin
  JvClipboardViewer1.Color := cxColorComboBox1.EditValue;
  JvClipboardViewer1.Update;
end;

procedure TFormClipboard.FormActivate( Sender: TObject );
var
  i: integer;
  j: integer;
begin
  ClipboardFormatNames1.Clear;
  for i := 0 to Clipboard.FormatCount - 1 do
  begin
    ClipboardFormatNames1.AddItem( JvClipboardViewer1.ClipboardFormatNames[ i ], nil );
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'Bitmap' then
      JvClipboardViewer1.ViewFormat := cvBitmap;
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'OemText' then
      JvClipboardViewer1.ViewFormat := cvOemText;
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'Text' then
      JvClipboardViewer1.ViewFormat := cvText;
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'Borland IDE Block Type' then
      JvClipboardViewer1.ViewFormat := cvText;
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'Delphi PNG Image' then
      JvClipboardViewer1.ViewFormat := cvBitmap;
  end;
  for j := 0 to ClipboardFormatNames1.Items.Count - 1 do
    if ClipboardFormatNames1.Items[ j ] = '' then
      ClipboardFormatNames1.Items.Delete( j );
  case JvClipboardViewer1.ViewFormat of
    cvDefault:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Default' );
    cvEmpty:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Empty' );
    cvUnknown:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Unknown' );
    cvText:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Text' );
    cvMetafile:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Metafile' );
    cvBitmap:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Bitmap' );
    cvOemText:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'OemText' );
    cvPicture:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Picture' );
    cvComponent:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Component' );
    cvIcon:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Icon' );
  end;
end;

procedure TFormClipboard.FormClose( Sender: TObject; var Action: TCloseAction );
begin
  FormMain.Clipboard1.Down := False;
end;

procedure TFormClipboard.FormCreate(Sender: TObject);
begin
  // Register TPngImage with Windows and TPicture
  CF_PNGIMAGE := RegisterClipboardFormat(PChar(string('PNG Image')));
  TPicture.RegisterClipboardFormat(CF_PNGIMAGE, TPngImage);
end;

procedure TFormClipboard.FormDestroy( Sender: TObject );
begin
  FormMain.Clipboard1.Down := False;
end;

procedure TFormClipboard.JvClipboardMonitor1Change( Sender: TObject );
var
  i: integer;
  j: integer;
  CF_dxPNGIMAGE: word;
  CF_PNGIMAGE: word;
  AData: THandle;
  APalette: hPalette;
begin
  ClipboardFormatNames1.Clear;
  for i := 0 to Clipboard.FormatCount - 1 do
  begin
    ClipboardFormatNames1.AddItem( JvClipboardViewer1.ClipboardFormatNames[ i ], nil );
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'Bitmap' then
      JvClipboardViewer1.ViewFormat := cvBitmap;
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'OemText' then
      JvClipboardViewer1.ViewFormat := cvOemText;
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'Text' then
      JvClipboardViewer1.ViewFormat := cvText;
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'Borland IDE Block Type' then
      JvClipboardViewer1.ViewFormat := cvText;
    if JvClipboardViewer1.ClipboardFormatNames[ i ] = 'Delphi PNG Image' then
      JvClipboardViewer1.ViewFormat := cvBitmap;
  end;
  for j := 0 to ClipboardFormatNames1.Items.Count - 1 do
    if ClipboardFormatNames1.Items[ j ] = '' then
      ClipboardFormatNames1.Items.Delete( j );
  case JvClipboardViewer1.ViewFormat of
    cvDefault:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Default' );
    cvEmpty:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Empty' );
    cvUnknown:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Unknown' );
    cvText:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Text' );
    cvMetafile:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Metafile' );
    cvBitmap:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Bitmap' );
    cvOemText:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'OemText' );
    cvPicture:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Picture' );
    cvComponent:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Component' );
    cvIcon:
      ClipboardFormatNames1.ItemIndex := ClipboardFormatNames1.Items.IndexOf( 'Icon' );
  end;
end;

end.
