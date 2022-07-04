unit UFormViewEXIF;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, EXIF_Intf, EXIF_Defs, ComCtrls, StdCtrls, ExtCtrls;

type
  TFormViewEXIF = class(TForm)
    List: TListView;
    tc: TTabControl;
    Image: TImage;
    ImagePanel: TPanel;
    Label1: TLabel;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
    procedure tcChange(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    EXIF: TEXIF;
  public
    procedure DisplayIFD0;
    procedure DisplayIFD1;
    procedure DisplayEXIFSubIFD;
    procedure DisplayEXIFInteropIFD;
    procedure DisplayMakerNote;
    procedure DisplayThumbnail;
  end;

procedure ShowEXIF(const FN: string);

implementation

uses
  JPEG, ATxMsg, ATxMsgProc;

{$R *.dfm}

procedure TFormViewEXIF.FormCreate(Sender: TObject);
begin
  EXIF:=TEXIF.Create;
end;

procedure TFormViewEXIF.ListDblClick(Sender: TObject);
var
  s: string;
begin
  try
    s:=TListView(Sender).Selected.SubItems[1];
    InputQuery(TListView(Sender).Selected.SubItems[0],TListView(Sender).Selected.Caption,s);
  except
  end;
end;

procedure TFormViewEXIF.tcChange(Sender: TObject);
begin
  List.Hide;
  ImagePanel.Hide;
  case tc.TabIndex of
    0: DisplayIFD0;
    1: DisplayEXIFSubIFD;
    2: DisplayEXIFInteropIFD;
    3: DisplayMakerNote;
    4: DisplayIFD1;
    5: DisplayThumbnail;
  end;
end;

procedure TFormViewEXIF.DisplayEXIFInteropIFD;
var
  i: Integer;
begin
  with EXIF do begin
    List.Items.BeginUpdate;
    try
      List.Items.Clear;
      for i:=0 to INTEROPCount-1 do
        with List.Items.Add do begin
          Caption:=Format('0x%x',[INTEROP_Index[i].Header.Tag]);
          SubItems.Add(INTEROP_Index[i].Header.Name);
          SubItems.Add(TranslateINTEROP(INTEROP_Index[i]));
          ImageIndex:=-1;
        end;
    finally
      List.Items.EndUpdate;
    end;
  end;
  List.Show;
end;

procedure TFormViewEXIF.DisplayEXIFSubIFD;
var
  i: Integer;
begin
  with EXIF do begin
    List.Items.BeginUpdate;
    try
      List.Items.Clear;
      for i:=0 to EXIFCount-1 do
        with List.Items.Add do begin
          Caption:=Format('0x%x',[EXIF_Index[i].Header.Tag]);
          SubItems.Add(EXIF_Index[i].Header.Name);
          SubItems.Add(TranslateEXIF(EXIF_Index[i]));
          ImageIndex:=-1;
        end;
    finally
      List.Items.EndUpdate;
    end;
  end;
  List.Show;
end;

procedure TFormViewEXIF.DisplayIFD0;
var
  i: Integer;
begin
  with EXIF do begin
    List.Items.BeginUpdate;
    try
      List.Items.Clear;
      for i:=0 to TIFFCount-1 do
        with List.Items.Add do begin
          Caption:=Format('0x%x',[TIFF_Index[i].Header.Tag]);
          SubItems.Add(TIFF_Index[i].Header.Name);
          SubItems.Add(TranslateTIFF(TIFF_Index[i]));
          ImageIndex:=-1;
        end;
    finally
      List.Items.EndUpdate;
    end;
  end;
  List.Show;
end;

procedure TFormViewEXIF.DisplayMakerNote;
var
  i: Integer;
begin
  with EXIF do begin
    List.Items.BeginUpdate;
    try
      List.Items.Clear;
      for i:=0 to MakerNoteCount-1 do
        with List.Items.Add do begin
          Caption:=Format('0x%x',[MakerNote_Index[i].Header.Tag]);
          SubItems.Add(MakerNote_Index[i].Header.Name);
          SubItems.Add(TranslateMakerNote(MakerNoteFormat,MakerNote_Index[i]));
          ImageIndex:=-1;
        end;
    finally
      List.Items.EndUpdate;
    end;
  end;
  List.Show;
end;

procedure TFormViewEXIF.DisplayIFD1;
var
  i: Integer;
begin
  with EXIF do begin
    List.Items.BeginUpdate;
    try
      List.Items.Clear;
      for i:=0 to EXIF_IFD1Count-1 do
        with List.Items.Add do begin
          Caption:=Format('0x%x',[EXIF_IFD1_Index[i].Header.Tag]);
          SubItems.Add(EXIF_IFD1_Index[i].Header.Name);
          SubItems.Add(TranslateTIFF(EXIF_IFD1_Index[i]));
          ImageIndex:=-1;
        end;
    finally
      List.Items.EndUpdate;
    end;
  end;
  List.Show;
end;

procedure TFormViewEXIF.DisplayThumbnail;
begin
  if Assigned(EXIF.Thumbnail) and (EXIF.Thumbnail.Height>0) then begin
    Image.Picture.Assign(EXIF.Thumbnail);
    ImagePanel.Show;
    Label1.Caption:=Format('%d x %d',[Image.Picture.Width,Image.Picture.Height]);
  end;
end;

procedure TFormViewEXIF.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure ShowEXIF(const FN: string);
begin
  with TFormViewEXIF.Create(nil) do
    try
      {$I Lang.FormViewEXIF.inc}

      EXIF.LoadFromFile(FN);
      with EXIF do
        if (TIFFCount = 0) and
          (EXIFCount = 0) and
          (EXIF_IFD1Count = 0) and
          (INTEROPCount = 0) and
          (MakerNoteCount = 0) and
          not Assigned(Thumbnail) then
      begin
        Application.MessageBox(PChar(MsgViewerExifMissed), PChar(Caption), MB_OK or MB_ICONWARNING);
        Exit
      end;

      tcChange(nil);
      ShowModal;
    finally
      Release;
    end;
end;


end.
