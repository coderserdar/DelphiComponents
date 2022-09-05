unit DXGlueItEdit;
//(c)2007 Jaro Benes
//All Rights Reserved  

{
Complex application for users of unDelphiX as component editor:

Supported:
 a) many small images glue into one image.
 b) generate font image as one image with subpictures.
 c) use image effect for DIB.
 d) animation the glued image and font preview.
 e) restructuralize of images to square size or unlimeted in one size direction up 2048 px.
 f) it works in 24bit deepth always.
 g) store into DXImageList directly.
 h) reset of transparent color.
 i) force size for all images with different size.
 j) use the loupe preview for glued image (in preview page).
 k) generate mask for alphachannel for characters of fonts.
 
}

interface

{$I DelphiXcfg.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, ShellAPI, StdCtrls, Dialogs,
  DXClass, DIB, jpeg, Menus, DXDraws, ComCtrls, Buttons, Controls, ExtCtrls, Forms,
  {$IFDEF VER17UP}System.UITypes,{$ENDIF}
  ExtDlgs,{$IFDEF VER6UP} Types,{$ENDIF} ActnList, ImgList;

type
  TOperationGlueIt = (ogiNew, ogiEdit);
  {injected class}
  TEdit = class(StdCtrls.TEdit)
  private
    function GetAsInteger: Integer;
    procedure SetAsInteger(const Value: Integer);
  published
  public
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
  end;
  TDXGlueItEditor = class(TForm)
    OpenDialog1: TOpenDialog;
    DXTimer1: TDXTimer;
    SaveDialog1: TSaveDialog;
    DXImageList1: TDXImageList;
    Panel5: TPanel;
    btnExit: TButton;
    Button1: TButton;
    PopupMenu1: TPopupMenu;
    LoadImagedirectly1: TMenuItem;
    mainPageControl: TPageControl;
    tsGlueIt: TTabSheet;
    tsFontGen: TTabSheet;
    Panel1: TPanel;
    grManagementOfTheFrames: TGroupBox;
    Panel4: TPanel;
    chbCrop: TCheckBox;
    chbCentered: TCheckBox;
    ListBox1: TListBox;
    Panel3: TPanel;
    btnAddImages: TSpeedButton;
    btnDeleteSelected: TSpeedButton;
    btnClearAll: TSpeedButton;
    btnSave: TSpeedButton;
    btnGlueIt: TSpeedButton;
    btnGlue2Iso: TSpeedButton;
    btnUpSelection: TSpeedButton;
    btnDownSelection: TSpeedButton;
    chbTransparent: TCheckBox;
    Panel2: TPanel;
    grPictureAnimationPreview: TGroupBox;
    DXDraw1: TDXDraw;
    Panel6: TPanel;
    btnStop: TSpeedButton;
    btnPlay: TSpeedButton;
    LAnimationSpeed: TLabel;
    pbAnimationSpeed: TProgressBar;
    tsPreview: TTabSheet;
    GroupBox1: TGroupBox;
    Panel7: TPanel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    GroupBox4: TGroupBox;
    Panel8: TPanel;
    gbFontSettings: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    Label10: TLabel;
    cbFontName: TComboBox;
    FontSize: TEdit;
    gbFontEffects: TGroupBox;
    Label5: TLabel;
    Label7: TLabel;
    eOffset: TEdit;
    eOpacity: TEdit;
    gbColorSettings: TGroupBox;
    Label14: TLabel;
    Label15: TLabel;
    btnFontGenerate: TButton;
    FontPageControl: TPageControl;
    tsFont: TTabSheet;
    ScrollBox2: TScrollBox;
    imgFont: TImage;
    tsMask: TTabSheet;
    ScrollBox3: TScrollBox;
    imgMask: TImage;
    tsPreviewFont: TTabSheet;
    PreviewMemo: TMemo;
    PreviewDraw: TDXDraw;
    PreviewTimer: TDXTimer;
    FontDXImageList: TDXImageList;
    Label16: TLabel;
    btnAllChars: TButton;
    memAlphabet: TMemo;
    cbAntialias: TCheckBox;
    cbDrawGrid: TCheckBox;
    SavePictureDialog: TSavePictureDialog;
    ColorDialog: TColorDialog;
    pnlFG: TPanel;
    pnlBG: TPanel;
    Panel20: TPanel;
    btnABold: TSpeedButton;
    btnAItalic: TSpeedButton;
    btnAUnderline: TSpeedButton;
    DXDIB1: TDXDIB;
    Label11: TLabel;
    Sources: TMemo;
    Panel9: TPanel;
    btnFontAnimationStop: TSpeedButton;
    btnFontAnimationStart: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Panel10: TPanel;
    LDuration: TLabel;
    Label6: TLabel;
    pbDuration: TProgressBar;
    cbEffectsList: TComboBox;
    btnApply: TButton;
    Panel11: TPanel;
    grSubimages: TGroupBox;
    LPatternWidth: TLabel;
    LPatternHeight: TLabel;
    ePatternWidth: TEdit;
    ePatternHeight: TEdit;
    btnResize: TButton;
    btnReplace: TButton;
    Panel12: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Label2: TLabel;
    panTColor: TPanel;
    EWidthOfImages: TEdit;
    EHeightOfImages: TEdit;
    LWidthOfImages: TLabel;
    LHeightOfImages: TLabel;
    chbForceSize: TCheckBox;
    ImageList1: TImageList;
    ActionList1: TActionList;
    acAddImages: TAction;
    acDeleteAll: TAction;
    acDeleteOne: TAction;
    acSaveToFile: TAction;
    acGlueIt: TAction;
    acGlueIzonometrics: TAction;
    acUpSelection: TAction;
    acDownSelection: TAction;
    acAnimateOn: TAction;
    acAnimateStop: TAction;
    acAnimeFontOff: TAction;
    acAnimeFontOn: TAction;
    Image2: TImage;
    GroupBox2: TGroupBox;
    Label13: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Slider: TTrackBar;
    chbZoomOut: TCheckBox;
    CheckBox1: TCheckBox;
    btnGetTransparentcolor: TSpeedButton;
    btnCrop: TSpeedButton;
    btnFill: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    btnWand: TSpeedButton;
    btnMask: TSpeedButton;
    acLoadImage: TAction;
    acSaveImage: TAction;
    acGetTransparent: TAction;
    Label8: TLabel;
    panBColor: TPanel;
    Panel13: TPanel;
    EFromImage: TEdit;
    EToImage: TEdit;
    Label3: TLabel;
    LToImage: TLabel;
    chbAutoAply: TCheckBox;
    procedure chbAutoAplyClick(Sender: TObject);
    procedure pbDurationMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbDurationMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbEffectsListChange(Sender: TObject);
    procedure chbForceSizeClick(Sender: TObject);
    procedure acGetTransparentExecute(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnGetTransparentcolorClick(Sender: TObject);
    procedure btnResizeClick(Sender: TObject);
    procedure btnReplaceClick(Sender: TObject);
    procedure btnFontAnimationStartClick(Sender: TObject);
    procedure btnFontAnimationStopClick(Sender: TObject);
    procedure PreviewMemoChange(Sender: TObject);
    procedure PreviewTimerTimer(Sender: TObject; LagCount: Integer);
    procedure btnAUnderlineClick(Sender: TObject);
    procedure btnAItalicClick(Sender: TObject);
    procedure btnABoldClick(Sender: TObject);
    procedure btnFontGenerateClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnAllCharsClick(Sender: TObject);
    procedure pnlFGClick(Sender: TObject);
    procedure cbFontNameDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure pbAnimationSpeedMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure LoadImagedirectly1Click(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure pbDurationMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ListBox1MeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
    procedure ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DXTimer1Timer(Sender: TObject; LagCount: Integer);
    procedure btnStopClick(Sender: TObject);
    procedure btnAddImagesClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure btnUpSelectionClick(Sender: TObject);
    procedure btnDownSelectionClick(Sender: TObject);
    procedure btnDeleteSelectedClick(Sender: TObject);
    procedure btnGlueItClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);

    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction;
      var Handled: Boolean);
    procedure chbZoomOutClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure mainPageControlChange(Sender: TObject);
  private
    { Private declarations }
    tmpPicture: TPicture;
    StartingPoint: TPoint;
    FDuration: Integer;
    ListObjects: TStringList;
    dX, dY: Integer;
    FFontStyles: TFontStyles;
    charArr: string;
    sizesArr: array of Integer;
    Zdvih: Integer;
    WPattern: Integer;
    InitIntInMs: Integer;
    WCounter: Integer;
    FOperationGlueIt: TOperationGlueIt;
    SelectionOfTransparentColor: Boolean;
    procedure WMDropFiles(var Message: TWMDropFiles); message wm_DropFiles;
    procedure RestructuralizeWithResize(NewWidth, NewHeight: Integer; TranspColor: TColor = clMaroon);
    procedure SetOperationGlueIt(const Value: TOperationGlueIt);
    procedure DoBitmapEffect(Picture: TPicture);
  public
    { Public declarations }
    property Operation: TOperationGlueIt read FOperationGlueIt write SetOperationGlueIt;
    procedure LoadImageFromList(const iName: string; Image: TPicture; PatternWidth,
      PatternHeight: Integer; Transparent: Boolean; TransparentColor: Integer);
    procedure SaveImageIntoList(oItem: TPictureCollectionItem);
  end;

var
  DXGlueItEditor: TDXGlueItEditor;

implementation

{$R *.DFM}


uses {$IFDEF StandardDX}DirectDraw{$ELSE}DirectX{$ENDIF};

{ TEdit }

function TEdit.GetAsInteger: Integer;
begin
  try
    Result := StrToInt(Self.Text);
  except
    Result := 0;
  end;
end;

procedure TEdit.SetAsInteger(const Value: Integer);
begin
  Self.Text := IntToStr(Value)
end;

procedure TDXGlueItEditor.btnStopClick(Sender: TObject);
begin
  DXTimer1.Enabled := False;
end;

{  TDXGlueItEditor  }

procedure TDXGlueItEditor.btnAddImagesClick(Sender: TObject);
var
  i: Integer;
  TI: TPicture;
begin
  if OpenDialog1.Execute then
    if OpenDialog1.Files.Count > 0 then begin
      ListObjects.Clear;
      with OpenDialog1.Files do
        for I := 0 to Count - 1 do
          if FileExists(Strings[I]) then
          begin
            TI := TPicture.Create;
            TI.LoadFromFile(Strings[I]);
            EWidthOfImages.AsInteger := Max(EWidthOfImages.AsInteger, TI.Width);
            EHeightOfImages.AsInteger := Max(EHeightOfImages.AsInteger, TI.Height);
            ListObjects.AddObject(Strings[I], TI);
          end;
      ListBox1.Items.Assign(ListObjects);
    end;
end;

procedure TDXGlueItEditor.btnClearAllClick(Sender: TObject);
var
  I: Integer;
begin
  if MessageDlg('Do you really want delete all frames?', mtWarning, [mbYes, mbNo], 0) = mrNo then Exit;
  btnStop.Click;
  for I := 0 to ListBox1.Items.Count - 1 do (ListBox1.Items.Objects[I] as TPicture).Free;
  ListBox1.Clear;
  ListObjects.Clear;
  DXImageList1.Items.Clear;
end;

procedure TDXGlueItEditor.btnUpSelectionClick(Sender: TObject);
begin
  if ListBox1.ItemIndex > 0 then begin
    btnStop.Click;
    ListBox1.Items.Move(ListBox1.ItemIndex, ListBox1.ItemIndex - 1);
  end;
end;

procedure TDXGlueItEditor.btnDownSelectionClick(Sender: TObject);
begin
  if (ListBox1.ItemIndex <> -1) and (ListBox1.ItemIndex < (ListBox1.Items.Count - 1)) then begin
    btnStop.Click;
    ListBox1.Items.Move(ListBox1.ItemIndex, ListBox1.ItemIndex + 1);
  end;
end;

procedure TDXGlueItEditor.btnDeleteSelectedClick(Sender: TObject);
begin
  if ListBox1.ItemIndex <> -1 then begin
    if MessageDlg('Do you want delete selected item?', mtWarning, [mbYes, mbNo], 0) = mrYes then begin
      (ListBox1.Items.Objects[ListBox1.ItemIndex] as TPicture).Free;
      ListObjects.Delete(ListBox1.ItemIndex);
      ListBox1.Items.Delete(ListBox1.ItemIndex);
    end;
  end;
end;

procedure TDXGlueItEditor.FormCreate(Sender: TObject);
var
  cnt: Integer;
begin
  DXImageList1.Items.MakeColorTable;
  DXDraw1.ColorTable := DXImageList1.Items.ColorTable;
  DXDraw1.DefColorTable := DXImageList1.Items.ColorTable;
  DXDraw1.UpdatePalette;
  tmpPicture := TPicture.Create;
  ListObjects := TStringList.Create;
  DragAcceptFiles(Handle, True);
  WPattern := 0; InitIntInMs := 200; //5 pict per sec
  WCounter := 0;
  for cnt := 0 to Screen.Fonts.Count - 1 do
    cbFontName.Items.Add(Screen.Fonts.Strings[cnt]);
  pbAnimationSpeed.Position := 200;
  pbDuration.Position := 5;
  mainPageControl.ActivePage := tsGlueIt;
  FontPageControl.ActivePage := tsPreviewFont;
  if chbZoomOut.Checked then Image1.OnMouseMove := Image1MouseMove
  else Image1.OnMouseMove := nil;
  Tag := 0;
  {$IFDEF VER4UP}
  pbAnimationSpeed.Smooth := True;
  pbDuration.Smooth := True;
  {$ENDIF}
end;

procedure TDXGlueItEditor.SaveImageIntoList(oItem: TPictureCollectionItem);
begin
  oItem.Picture.Assign(DXImageList1.Items[0].Picture);
  oItem.Transparent := DXImageList1.Items[0].Transparent;
  oItem.TransparentColor := DXImageList1.Items[0].TransparentColor;
  oItem.Name := DXImageList1.Items[0].Name;
  oItem.PatternWidth := DXImageList1.Items[0].Width;
  oItem.PatternHeight := DXImageList1.Items[0].Height;
end;

procedure TDXGlueItEditor.LoadImageFromList(const iName: string; Image: TPicture;
  PatternWidth, PatternHeight: Integer; Transparent: Boolean; TransparentColor: Integer);
{function for loading existing image from dximagelist do component editor}
var
  D: TPictureCollectionItem;
begin
  btnPlay.Click;
  DXImageList1.Items.Clear;
  D := TPictureCollectionItem(DXImageList1.Items.Add);
  D.Picture.Assign(Image);
  D.Name := Name;
  Image1.Picture.Assign(Image);
  D.PatternWidth := PatternWidth;
  D.PatternHeight := PatternHeight;
  D.Transparent := Transparent;
  D.TransparentColor := TransparentColor;
  DXImageList1.Items.Restore;
  ePatternWidth.AsInteger := PatternWidth;
  ePatternHeight.AsInteger := PatternHeight;
end;

procedure TDXGlueItEditor.btnGlueItClick(Sender: TObject);
//  function GetScale(SourceWidth, SourceHeight, TargetWidth, TargetHeight: Integer): Single;
//  from DIB unit
//  var
//    XScale, YScale: Single;
//  begin
//    XScale := 1;
//    YScale := 1;
//    if TargetWidth < SourceWidth then
//      XScale := TargetWidth / SourceWidth;
//    if TargetHeight < SourceHeight then
//      YScale := TargetHeight / SourceHeight;
//    Result := XScale;
//    if YScale < Result then
//      Result := YScale;
//  end;
var
  i: Integer;
  XL, YL, X, Y, QX, QY: Integer;
  P: TPicture;
  C: Double;
  Rz: Integer;
  B, BB: TBitmap; Icon: TIcon; DIB: TDIB;
  CI: TPictureCollectionItem;
  ImageIsBigger: Boolean;
  OldName: string;
begin
  XL := 0; YL := 0;
  B := TBitmap.Create;
  B.PixelFormat := pf24bit;
  try
    if chbForceSize.Checked then begin
      XL := EWidthOfImages.AsInteger;
      YL := EHeightOfImages.AsInteger;
    end
    else begin
      //must be the same size
      for i := 0 to ListBox1.Items.Count - 1 do begin
        P := ListBox1.Items.Objects[i] as TPicture;
        if Assigned(P) then begin
          XL := Max(XL, P.Width);
          YL := Max(YL, P.Height);
        end;
      end;
    end;
    //square od image
    C := Sqrt(ListBox1.Items.Count);
    Rz := Trunc(C);
    if Frac(C) > 0 then Inc(Rz);
    //dimension of Image
    B.Width := Rz * XL;
    B.Height := Rz * YL;
    {set color by user settings}
    B.Canvas.Brush.Color := panBColor.Color;
    B.Canvas.FillRect(Bounds(0, 0, B.Width, B.Height));
    for i := 0 to ListBox1.Items.Count - 1 do begin
      P := ListBox1.Items.Objects[i] as TPicture;
      if Assigned(P) then begin
        if P.Graphic is TIcon then begin
          Icon := (P.Graphic as TIcon);
          BB := TBitmap.Create;
          try
            BB.Width := Icon.Width;
            BB.Height := Icon.Height;
            BB.Canvas.Draw(0, 0, Icon);
            BB.Transparent := True;
            P.Graphic := BB;
          finally
            BB.Free;
          end;
        end;
        X := (i mod Rz) * XL;
        Y := (i div Rz) * YL;
        if chbForceSize.Checked then begin
          ImageIsBigger := (P.Width > XL) or (P.Height > YL);
          if ImageIsBigger then begin
            {image will be crop}
            if chbCrop.Checked then begin
              B.Canvas.CopyRect(Bounds(X, Y, XL, YL), P.Bitmap.Canvas, Bounds(0, 0, XL, YL))
            end
            else begin {image will be shrink}
              C := GetScale(P.Width, P.Height, XL, YL);
              DIB := TDIB.Create;
              try
                DIB.SetSize(P.Width, P.Height, 24);
                DIB.Canvas.Draw(0, 0, P.Graphic);
                DIB.DoResample(Round(P.Width * C), Round(P.Height * C), ftrLanczos3);
                B.Canvas.StretchDraw(Bounds(X, Y, Round(P.Width * C), Round(P.Height * C)), {P.Graphic} DIB);
              finally
                DIB.Free;
              end;
            end;
          end
          else begin
            QX := 0;
            QY := 0;
            if chbCentered.Checked then begin
              QX := (XL - P.Width) div 2;
              QY := (YL - P.Height) div 2;
            end;
            if not chbTransparent.Checked then
              B.Canvas.Draw(X + QX, Y + QY, P.Graphic)
            else
              B.Canvas.BrushCopy(Bounds(X + QX, Y + QY, P.Width, P.Height), P.Bitmap, Bounds(0, 0, P.Width, P.Height), P.Bitmap.Canvas.Pixels[0, P.Height - 1]);
          end;
        end
        else
          if not chbTransparent.Checked then
            B.Canvas.Draw(X, Y, P.Graphic)
          else
            B.Canvas.BrushCopy(Bounds(X, Y, XL, YL), P.Bitmap, Bounds(0, 0, XL, YL), P.Bitmap.Canvas.Pixels[0, P.Height - 1]);
      end;
    end;
    Image1.Picture.Assign(B);

    {reset after image assign}
    cbEffectsList.ItemIndex := -1; pbDuration.Position := 5; chbAutoAply.Checked := False;

    OldName := 'Test';
    if DXImageList1.Items.Count > 0 then
      if Operation = ogiEdit then
        OldName := DXImageList1.Items[0].Name; {puvodni jmeno}

    DXImageList1.Items.Clear;
    CI := TPictureCollectionItem(DXImageList1.Items.Add);
    CI.Name := OldName;
    CI.Picture.Assign(B);
    CI.Transparent := chbTransparent.Checked;
    CI.PatternWidth := XL;
    CI.PatternHeight := YL;
    ePatternWidth.AsInteger := XL;
    ePatternHeight.AsInteger := YL;
    DXImageList1.Items.Restore;

    EFromImage.AsInteger := 1;
    EToImage.AsInteger := ListBox1.Items.Count;
  finally
    B.Free;
  end;
  mainPageControl.ActivePage := tsPreview;
end;

procedure TDXGlueItEditor.btnPlayClick(Sender: TObject);
begin
  DXTimer1.Enabled := True;
  if Image1.Picture.Bitmap.Empty then
    btnStop.Click;
end;

procedure TDXGlueItEditor.btnSaveClick(Sender: TObject);
begin
  if MessageDlg('Do you want save image to file?', mtWarning, [mbYes, mbNo], 0) = mrNo then Exit;
  if SaveDialog1.Execute then
    Image1.Picture.SaveToFile(SaveDialog1.FileName);
end;

procedure TDXGlueItEditor.btnExitClick(Sender: TObject);
begin
  btnStop.Click;
  btnFontAnimationStop.Click;
  Tag := 1;
  Close
end;

function SpeedConst(InitValue, PerSec, LagCount: Integer): Integer; {$IFDEF VER9UP}inline; {$ENDIF}
begin
  Result := InitValue + (PerSec * Round(LagCount / 1000))
end;

procedure TDXGlueItEditor.DXTimer1Timer(Sender: TObject; LagCount: Integer);
begin
  if DXImageList1.Items.Count <= 0 then Exit;
  if not DXDraw1.CanDraw then Exit;
  DXDraw1.Surface.Fill(DXDraw1.Surface.ColorMatch(clBlack));
  DXDraw1.BeginScene;
  try
    {clear surface with predefined windows color}
    DXDraw1.Surface.Fill(DXDraw1.Surface.ColorMatch(clBlack));

    //----------------------------------------------------------------------------
    {All drawing here like}
    Inc(WCounter, LagCount);
    {timming}
    if WCounter > InitIntInMs then begin
      Inc(WPattern);
      if WPattern > DXImageList1.Items[0].PatternCount then WPattern := 0;
      {only for interval from EFromImage to EToImage}
      if WPattern > (EToImage.AsInteger - 1) then WPattern := EFromImage.AsInteger - 1;
      {reset counter}
      WCounter := 0;
    end;
    {drawing}
    with DXImageList1.Items[0] do
      Draw(DXDraw1.Surface, dX-(Width div 2), dY-(Height div 2), WPattern);
    //----------------------------------------------------------------------------
  finally
    DXDraw1.EndScene;
  end;

  { Draw FrameRate }
  with DXDraw1.Surface.Canvas do
  try
    Brush.Style := bsClear;
    Font.Color := clWhite;
    Font.Size := 10;
    Textout(3, 3, 'FPS: ' + IntToStr(DXTimer1.FrameRate));
    if doHardware in DXDraw1.NowOptions then begin
      Textout(3, 14, 'Device: Hardware');
    end
    else begin
      Textout(3, 14, 'Device: Software');
    end;
  finally
    Release; {  Indispensability  }
  end;
  DXDraw1.Flip;
end;

procedure TDXGlueItEditor.FormDestroy(Sender: TObject);
begin
  tmpPicture.Free;
  ListObjects.Free;
  DragAcceptFiles(Handle, False);
end;

procedure TDXGlueItEditor.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StartingPoint.X := X;
  StartingPoint.Y := Y;
end;

procedure TDXGlueItEditor.ListBox1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = ListBox1;
end;

procedure TDXGlueItEditor.ListBox1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  DropPosition, StartPosition: Integer;
  DropPoint: TPoint;
begin
  DropPoint.X := X;
  DropPoint.Y := Y;
  with Source as TListBox do
  begin
    StartPosition := ItemAtPos(StartingPoint, True);
    DropPosition := ItemAtPos(DropPoint, True);

    Items.Move(StartPosition, DropPosition);
  end;
end;

procedure TDXGlueItEditor.WMDropFiles(var Message: TWMDropFiles);
var
  FileCount, I: Integer;
  FileName: PChar;
  FileNameSize: Integer;
  S: string;
  TI: TPicture;
begin
  try
    FileCount := DragQueryFile(Message.Drop, Cardinal(-1), nil, 0);
    for I := 0 to FileCount - 1 do
    begin
      FileNameSize := DragQueryFile(Message.Drop, I, nil, 0);
      FileName := AllocMem(FileNameSize + 1);
      try
        DragQueryFile(Message.Drop, I, FileName, FileNameSize + 1);
        S := ExtractFileExt(Filename);
        if (S = '.bmp') or (S = '.dib') or (S = '.jpg') then begin
          TI := TPicture.Create;
          TI.LoadFromFile(Filename);
          ListBox1.Items.AddObject(FileName, TObject(TI));
        end;
      finally
        FreeMem(FileName);
      end;
    end;
  finally
    DragFinish(Message.Drop);
  end;
end;

procedure TDXGlueItEditor.ListBox1MeasureItem(Control: TWinControl; Index: Integer;
  var Height: Integer);
var
  I: TPicture;
begin
  I := TPicture(ListObjects.Objects[Index]);
  if Assigned(I) then
    Height := I.Height;
end;

procedure TDXGlueItEditor.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  I: TPicture; Ri: TRect; S: string; A: array[0..255] of Char;
  Dest, Src: TRect; Icon: TIcon; B: TBitmap;
begin
  ListBox1.Canvas.FillRect(Rect);
  I := ListBox1.Items.Objects[Index] as TPicture;
  if Assigned(I) then begin
    if I.Graphic is TIcon then begin
      Icon := (I.Graphic as TIcon);
      B := TBitmap.Create;
      try
        B.Width := Icon.Width;
        B.Height := Icon.Height;
        B.Canvas.Draw(0, 0, Icon);
        B.Transparent := True;
        I.Graphic := B;
      finally
        B.Free;
      end;
    end;
    Dest := Rect;
    Dest.Right := I.Width - 1;
    Src := Bounds(0, 0, I.Width, I.Height);
    if chbTransparent.Checked then
      ListBox1.Canvas.BrushCopy(Dest, I.Bitmap, Src, I.Bitmap.Canvas.Pixels[0, I.Height])
    else
      ListBox1.Canvas.Draw(Rect.Left, Rect.Top, I.Graphic);
    Ri := Rect;
    Ri.Left := Ri.Left + 4 + I.Width;
    S := ExtractFileName(ListBox1.Items[Index]);
    DrawText(ListBox1.Canvas.Handle, StrPCopy(A, S), -1, Ri, dt_SingleLine or dt_Left or dt_VCenter);
  end;
end;

procedure TDXGlueItEditor.pbDurationMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  newPosition: integer;
begin
  with Sender as TProgressBar do begin
    if ssLeft in Shift then
    begin
      Cursor := crHSplit;
      newPosition := Round(x * Max / ClientWidth);
      Position := newPosition;
    end
    else
    begin
      Cursor := crDefault;
    end;
    FDuration := Position;
    LDuration.Caption := Format('Duration (%d)', [FDuration]);
  end;
  if ssLeft in Shift then
    if chbAutoAply.Checked and (cbEffectsList.ItemIndex <> -1) and not tmpPicture.Graphic.Empty then begin
      DoBitmapEffect(tmpPicture);
      Application.ProcessMessages;
    end;
end;

procedure TDXGlueItEditor.DoBitmapEffect(Picture: TPicture);
var
  I, dX, dY: Integer;
  TT: TDIB;
  tbp: Integer;
begin
  TT := TDIB.Create;
  try
    if Assigned(Picture.Bitmap) then
      TT.Assign(Picture.Bitmap)
    else begin
      TT.Width := Picture.Width;
      TT.Height := Picture.Height;
      TT.Canvas.Draw(0, 0, Picture.Graphic);
    end;

    dX := TT.Width;
    dY := TT.Height;
    if pbDuration.Position > 5 then
    begin
      dX := Trunc(pbDuration.Position / 100 * TT.Width);
      dY := Trunc(pbDuration.Position / 100 * TT.Height);
    end;
    tbp := pbDuration.Position;
    { E F F E C T S }
    with TT do
      case cbEffectsList.ItemIndex of
        0: DoGaussianBlur(tbp);
        1: DoSplitBlur(tbp);
        2: DoAddColorNoise(tbp * 3);
        3: DoAddMonoNoise(tbp * 3);
        4: for i := 1 to tbp do DoAntiAlias;
        5: DoContrast(tbp * 3);
        6: DoFishEye(tbp div 10 + 1);
        7: DoLightness(tbp * 2);
        8: DoDarkness(tbp * 2);
        9: DoSaturation(255 - ((tbp * 255) div 100));
        10: DoMosaic(tbp div 2);
        11: DoTwist(200 - (tbp * 2) + 1);
        12: DoSplitlight(tbp div 20);
        13: DoTile(tbp div 10);
        14: DoSpotLight(tbp, Rect(tbp, tbp, tbp + tbp * 2, tbp + tbp * 2));
        15: DoTrace(tbp div 10);
        16: for i := 1 to tbp do DoEmboss;
        17: DoSolorize(255 - ((tbp * 255) div 100));
        18: DoPosterize(((tbp * 255) div 100) + 1);
        19: DoGrayscale;
        20: DoInvert;
        21: DoBrightness(tbp);
        22: DoColorize(clRed, clBlue);
        {resampling functions}
        23: DoResample(dX, dY, ftrBox);
        24: DoResample(dX, dY, ftrTriangle);
        25: DoResample(dX, dY, ftrHermite);
        26: DoResample(dX, dY, ftrBell);
        27: DoResample(dX, dY, ftrBSpline);
        28: DoResample(dX, dY, ftrLanczos3);
        29: DoResample(dX, dY, ftrMitchell);
      end; {Case}
    Image1.Picture.Bitmap := TT.CreateBitmapFromDIB;
    Image1.Invalidate;
  finally
    TT.Free;
  end;
end;

procedure TDXGlueItEditor.btnApplyClick(Sender: TObject);
begin
  if not Assigned(Image1.Picture.Graphic) then begin
    MessageDlg('Not graphics found in image, please glue it first.', mtWarning, [mbOK], 0);
    Exit;
  end;
  btnApply.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    DoBitmapEffect(Image1.Picture); Application.ProcessMessages;
  finally
    Screen.Cursor := crDefault;
    btnApply.Enabled := True;
  end;
end;

procedure TDXGlueItEditor.LoadImagedirectly1Click(Sender: TObject);
var
  Q: TPictureCollectionItem;
begin
  OpenDialog1.Options := OpenDialog1.Options - [ofAllowMultiSelect];
  try
    if OpenDialog1.Execute then begin
      Image1.Picture.LoadFromFile(OpenDialog1.FileName);
      DXImageList1.Items.Clear;
      Q := TPictureCollectionItem(DXImageList1.Items.Add);
      Q.Name := ExtractFileName(OpenDialog1.FileName);
      Q.Picture.LoadFromFile(OpenDialog1.FileName);
      Q.Transparent := True;
      if Q.Picture.Graphic is TBitmap then begin
        Q.TransparentColor := Q.Picture.Bitmap.Canvas.Pixels[Q.Width - 1, Q.Height - 1];
        panTColor.Color := Q.TransparentColor;
      end;
      DXImageList1.Items.Restore;
    end;
  finally
    OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect]
  end;
end;

procedure TDXGlueItEditor.DXDraw1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  dX := X;
  dY := Y;
end;

procedure TDXGlueItEditor.pbAnimationSpeedMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  newPosition: integer;
begin
  with Sender as TProgressBar do begin
    if ssLeft in Shift then
    begin
      Cursor := crHSplit;
      newPosition := Round(x * Max / ClientWidth);
      Position := newPosition;
    end
    else
    begin
      Cursor := crDefault;
    end;
    InitIntInMs := Position;
    LAnimationSpeed.Caption := Format('Animation speed (%f/sec):', [InitIntInMs / 1000]);
  end;
end;

procedure TDXGlueItEditor.cbFontNameDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TComboBox).Canvas do
  begin
    Font.Name := Screen.Fonts.Strings[Index];
    //Font.Size := seSize.AsInteger;
    FillRect(Rect);
    TextOut(Rect.Left, Rect.Top, PChar(Screen.Fonts.Strings[Index]))
  end;
end;

procedure TDXGlueItEditor.pnlFGClick(Sender: TObject);
begin
  ColorDialog.Color := (Sender as TPanel).Color;
  if ColorDialog.Execute then
    (Sender as TPanel).Color := ColorDialog.Color;
end;

procedure TDXGlueItEditor.btnAllCharsClick(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to 255 do
    if Char(I) < ' ' then S := S + ' ' else S := S + Char(I);
  memAlphabet.Lines.Add(S);
end;

procedure TDXGlueItEditor.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssShift in Shift then with Image1.Canvas do begin
      Brush.Color := panTColor.Color;
      Brush.Style := bsSolid;
      FloodFill(X, Y, Pixels[X, Y], fsSurface);
    end;
end;

procedure TDXGlueItEditor.btnFontGenerateClick(Sender: TObject);
var
  S, vizfntname: string;
  C: Double;
  Rz, vX, vY, I, absX, absY, V, X, Y, offC: Integer;
  B: TBitmap;
  LogFont: TLogFont;
  fnt: TFont;
  dib, alpha: TDIB;
  q, d: TPictureCollectionItem;
begin
  {no preview}
  btnFontAnimationStop.Click;
  {private font def.}
  fnt := TFont.Create;
  try
    fnt.Assign(cbFontName.Font);
    V := eOpacity.AsInteger;
    fnt.Color := RGB(V, V, V); //0..255
    fnt.Name := cbFontName.Text;
    fnt.Size := FontSize.AsInteger;
    fnt.Style := FFontStyles;
    {antialiased/ttf}
    if not cbAntialias.Checked then begin
      GetObject(fnt.Handle, SizeOf(LogFont), Addr(LogFont));
      with LogFont do begin
        if cbAntialias.Checked then
          lfQuality := ANTIALIASED_QUALITY
        else
          lfQuality := NONANTIALIASED_QUALITY;
        lfOutPrecision := OUT_TT_ONLY_PRECIS;
      end;
      fnt.Handle := CreateFontIndirect(LogFont);
    end;
    {get set of chars}
    S := TrimRight(memAlphabet.Lines.Text);
    if S = '' then begin
      btnAllChars.Click;
      S := TrimRight(memAlphabet.Lines.Text);
    end;
    {target square}
    C := Sqrt(Length(S));
    Rz := Trunc(C);
    if Frac(C) > 0 then Inc(Rz);
    {generate mask font}
    B := TBitmap.Create;
    try
      B.PixelFormat := pf8bit;
      B.Canvas.Brush.Color := clBlack;
      B.Canvas.Font.Assign(fnt);
      {absolute sizes}
      charArr := s;
      SetLength(sizesArr, Length(charArr));
      vY := B.Canvas.TextHeight(S); Zdvih := vY;
      vX := 0;
      for I := 0 to Length(S) - 1 do begin
        sizesarr[I] := B.Canvas.TextWidth(S[I + 1]);
        vX := Max(vX, sizesarr[I]);
      end;

      offC := eOffset.AsInteger;
      vX := vX + offC;
      vY := vY + offC;

      absX := Rz * vX;
      absY := Rz * vY;

      B.Width := absX;
      B.Height := absY;

      B.Canvas.FillRect(Bounds(0, 0, B.Width, B.Height));
      B.Canvas.Brush.Style := bsClear;
      {shadow as offset}
      if eOffset.AsInteger > 0 then
        for I := 0 to Length(S) - 1 do begin
          X := offC + (i mod Rz) * vX;
          Y := offC + (i div Rz) * vY;
          B.Canvas.TextOut(X, Y, S[I + 1]);
        end;
      {masked chars}
      B.Canvas.Font.Color := clWhite;
      for I := 0 to Length(S) - 1 do begin
        X := (i mod Rz) * vX;
        Y := (i div Rz) * vY;
        B.Canvas.TextOut(X, Y, S[I + 1]);
      end;
      imgMask.Picture.Assign(B);
    finally
      B.Free;
    end;
    {generate font}
    B := TBitmap.Create;
    try
      B.PixelFormat := pf24bit;
      B.Width := absX;
      B.Height := absY;
      B.Canvas.Brush.Color := pnlBG.Color;
      B.Canvas.Font.Assign(fnt);
      B.Canvas.Font.Color := pnlFG.Color;
      B.Width := absX;
      B.Height := absY;
      B.Canvas.FillRect(Bounds(0, 0, B.Width, B.Height));
      B.Canvas.Brush.Style := bsClear;
      for I := 0 to Length(S) - 1 do begin
        X := (i mod Rz) * vX;
        Y := (i div Rz) * vY;
        B.Canvas.TextOut(X, Y, S[I + 1]);
      end;
      imgFont.Picture.Assign(B);
    finally
      B.Free;
    end;
  finally
    vizfntname := fnt.name;
    fnt.Free;
  end;

  Sources.Lines.Clear;
  Sources.Lines.Add('{Generated constants for simple use of the font}');
  Sources.Lines.Add('const');
  Sources.Lines.Add(Format('  offsY = %d;', [Zdvih]));
  Sources.Lines.Add(Format('  offYbyOrder: array [0..%d] of Integer = (', [Length(charArr) - 1]));
  s := '';
  for i := Low(sizesArr) to High(sizesArr) do
    s := s + IntToStr(sizesArr[i]) + ', ';
  Delete(s, Length(s) - 2, 2);
  Sources.Lines.Add(s);
  Sources.Lines.Add(');');

  dib := TDIB.Create;
  alpha := TDIB.Create;
  try
    alpha.Assign(imgMask.Picture.Bitmap);
    dib.Assign(imgFont.Picture.Bitmap);
    dib.AssignAlphaChannel(alpha);
    {for preview}
    FontDXImageList.Items.Clear;
    q := TPictureCollectionItem(FontDXImageList.Items.Add);
    q.Picture.Assign(dib);
    q.Name := vizfntname;
    q.PatternWidth := vX;
    q.PatternHeight := vY;
    q.Transparent := True;
    q.TransparentColor := pnlBG.Color;
    FontDXImageList.Items.Restore;
    {showing}
    Image1.Picture := nil;
    Image1.Picture.Bitmap := dib.CreateBitmapFromDIB;
    {for exchange with master thread}
    DXImageList1.Items.Clear;
    d := TPictureCollectionItem(DXImageList1.Items.Add);
    d.Picture.Assign(dib);
    d.Name := vizfntname;
    d.PatternWidth := vX;
    d.PatternHeight := vY;
    d.Transparent := True;
    d.TransparentColor := pnlBG.Color;
    DXImageList1.Items.Restore;

  finally
    alpha.Free;
    dib.Free;
  end;
  if PreviewMemo.Lines.Count > 0 then
    btnFontAnimationStart.Click;
end;

procedure TDXGlueItEditor.btnABoldClick(Sender: TObject);
begin
  if btnABold.Down then FFontStyles := FFontStyles + [fsBold]
  else FFontStyles := FFontStyles - [fsBold]
end;

procedure TDXGlueItEditor.btnAItalicClick(Sender: TObject);
begin
  if btnAItalic.Down then FFontStyles := FFontStyles + [fsItalic]
  else FFontStyles := FFontStyles - [fsItalic]
end;

procedure TDXGlueItEditor.btnAUnderlineClick(Sender: TObject);
begin
  if btnAUnderline.Down then FFontStyles := FFontStyles + [fsUnderline]
  else FFontStyles := FFontStyles - [fsUnderline]
end;

procedure TDXGlueItEditor.PreviewTimerTimer(Sender: TObject; LagCount: Integer);
var i, x, y, j: Integer;
  s: string;
begin
  if not PreviewDraw.CanDraw then Exit;

  PreviewDraw.Surface.Fill(PreviewDraw.Surface.ColorMatch(pnlBG.Color));
  PreviewDraw.BeginScene;
  PreviewDraw.Surface.FillRect(Bounds(0, 0, PreviewDraw.Width, PreviewDraw.Height), pnlBG.Color);
  y := 5;
  for i := 0 to PreviewMemo.Lines.Count - 1 do begin
    s := PreviewMemo.Lines[i];
    x := 5;
    for j := 1 to Length(s) do begin
      FontDXImageList.Items[0].Draw(PreviewDraw.Surface, x, y, Ord(s[j]));
      x := x + sizesarr[Ord(s[j])];
    end;
    y := y + Zdvih;
  end;
  PreviewDraw.EndScene;
  PreviewDraw.Flip;
end;

procedure TDXGlueItEditor.PreviewMemoChange(Sender: TObject);
begin
  PreviewTimer.Enabled := PreviewMemo.Lines.Text <> '';
end;

procedure TDXGlueItEditor.btnFontAnimationStopClick(Sender: TObject);
begin
  PreviewTimer.Enabled := False;
  PreviewDraw.Surface.FillRect(Bounds(0, 0, PreviewDraw.Width, PreviewDraw.Height), pnlBG.Color);
  PreviewDraw.Flip;
end;

procedure TDXGlueItEditor.btnFontAnimationStartClick(Sender: TObject);
begin
  PreviewTimer.Enabled := True;
end;

procedure TDXGlueItEditor.RestructuralizeWithResize(NewWidth, NewHeight: Integer; TranspColor: TColor = clMaroon);
var
  C: Double;
  Rz: Integer;
  Q: TPictureCollectionItem;
  IMG: TBitmap;
  DIB: TDIB;
  I, X, Y, dX, dY: Integer;
  DDS: TDirectDrawSurface;
begin
  dX := DXImageList1.Items[0].PatternWidth;
  dY := DXImageList1.Items[0].PatternHeight;
  if NewWidth <> dX then dX := NewWidth;
  if NewHeight <> dY then dY := NewHeight;
  {target square}
  C := Sqrt(DXImageList1.Items[0].PatternCount);
  Rz := Trunc(C);
  if Frac(C) > 0 then Inc(Rz);
  {new picture};
  IMG := TBitmap.Create; {glued picture}
  DIB := TDIB.Create; {converted sub-image}
  DDS := TDirectDrawSurface.Create(DXDraw1.DDraw); {dds}
  DDS.SetSize(dX, dY);
  {note: there occur error, when surface is not self, but nothing created}
  {for this correct run has to set size as power two and must be remaps to mesh}
  {or better simple turn off D3D acceleration}
  {$IFDEF D3D_deprecated}
  DXDraw1.Options := DXDraw1.Options - [do3d]; // dds may be any size
  {$ENDIF}
  try
    IMG.Width := Rz * dX; {new size}
    IMG.Height := Rz * dY;
    IMG.PixelFormat := pf24bit; {implicit}
    IMG.Canvas.Brush.Color := clMaroon; {fill it wit}
    IMG.Canvas.FillRect(Bounds(0, 0, IMG.Width, IMG.Height)); {fill now}
    {for all non restructuralized image}
    for i := 0 to DXImageList1.Items[0].PatternCount - 1 do begin
      {refill by transparent color as background}
      DDS.Fill(DDS.ColorMatch(TranspColor));
      {draw sub-image to dds}
      DXImageList1.Items[0].Draw(DDS, 0, 0, i);
      {convert to dib}
      DIB.Assign(DDS);
      {draw to new position}
      X := (i mod Rz) * dX;
      Y := (i div Rz) * dY;
      IMG.Canvas.Draw(X, Y, DIB)
    end;
    {for preview}
    Image1.Picture.Assign(IMG);
    {to collection item}
    Q := TPictureCollectionItem(DXImageList1.Items.Add);
    Q.Picture.Assign(IMG);
    Q.PatternWidth := dX;
    Q.PatternHeight := dY;
    Q.Name := DXImageList1.Items[0].Name; //it has to have name
    Q.Transparent := DXImageList1.Items[0].Transparent;
    Q.TransparentColor := DXImageList1.Items[0].TransparentColor;
    {original image get out}
{$IFNDEF VER5UP}
    DXImageList1.Items[0].Free;
{$ELSE}
    DXImageList1.Items.Delete(0);
{$ENDIF}
    {Indispensability restore}
    DXImageList1.Items.Restore;
  finally
    {freeing resources}
    IMG.Free;
    DIB.Free;
    DDS.Free;
    {$IFDEF D3D_deprecated}
    DXDraw1.Options := DXDraw1.Options + [do3d];
    {$ENDIF}
  end;
end;

procedure TDXGlueItEditor.btnReplaceClick(Sender: TObject);
begin
  if DXImageList1.Items.Count > 0 then
    if (ePatternWidth.AsInteger > 0) and (ePatternHeight.AsInteger > 0) then begin
      {must be set subimage first}
      if DXImageList1.Items[0].PatternWidth <> ePatternWidth.AsInteger then
        DXImageList1.Items[0].PatternWidth := ePatternWidth.AsInteger;
      if DXImageList1.Items[0].PatternHeight <> ePatternHeight.AsInteger then
        DXImageList1.Items[0].PatternHeight := ePatternHeight.AsInteger;

      DXImageList1.Items.Restore;
      RestructuralizeWithResize(DXImageList1.Items[0].PatternWidth,
        DXImageList1.Items[0].PatternHeight, panTColor.Color);
    end
    else
      MessageDlg('Please set subimage size first!', mtConfirmation, [mbOK], 0);
end;

procedure TDXGlueItEditor.btnResizeClick(Sender: TObject);
begin
  if DXImageList1.Items.Count > 0 then {contain image}
    if (ePatternWidth.AsInteger > 0) and (ePatternHeight.AsInteger > 0) then begin
      {must be set subimage first}
      if DXImageList1.Items[0].PatternWidth <> ePatternWidth.AsInteger then
        DXImageList1.Items[0].PatternWidth := ePatternWidth.AsInteger;
      if DXImageList1.Items[0].PatternHeight <> ePatternHeight.AsInteger then
        DXImageList1.Items[0].PatternHeight := ePatternHeight.AsInteger;

      DXImageList1.Items.Restore;
      RestructuralizeWithResize(ePatternWidth.AsInteger, ePatternHeight.AsInteger, panTColor.Color);
    end
    else
      MessageDlg('Please set subimage size first!', mtConfirmation, [mbOK], 0);
end;

procedure TDXGlueItEditor.ActionList1Update(Action: TBasicAction;
  var Handled: Boolean);
begin
  btnAllChars.Enabled := Trim(memAlphabet.Lines.Text) = '';
  acDeleteAll.Enabled := ListBox1.Items.Count > 0;
  acDeleteOne.Enabled := ListBox1.ItemIndex <> -1;
  acSaveToFile.Enabled := not Image1.Picture.Bitmap.Empty;
  acGlueIt.Enabled := ListBox1.Items.Count > 0;
  btnGetTransparentcolor.Enabled := not Image1.Picture.Bitmap.Empty;
end;

procedure TDXGlueItEditor.chbZoomOutClick(Sender: TObject);
begin
  if (Sender as TCheckBox).Checked then Image1.OnMouseMove := Image1MouseMove
  else Image1.OnMouseMove := nil;
end;

procedure TDXGlueItEditor.Image1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Srect, Drect: TRect;
  iWidth, iHeight, DmX, DmY: Integer;
  iTmpX, iTmpY: Real;
  C: TCanvas;
  hDesktop: Hwnd;
  dx, dy: Integer;
  PP: TPoint;
begin
  PP := Image1.ClientToScreen(Point(X, Y));
  dx := PP.x;
  dy := PP.y;
  hDesktop := GetDesktopWindow;

  iWidth := Image2.Width;
  iHeight := Image2.Height;
  Drect := Rect(0, 0, iWidth, iHeight);
  iTmpX := iWidth / (Slider.Position * 4);
  iTmpY := iHeight / (Slider.Position * 4);
  Srect := Rect(dx, dy, dx, dy);
  InflateRect(Srect, Round(iTmpX), Round(iTmpY));

  if Srect.Left < 0 then OffsetRect(Srect, -Srect.Left, 0);
  if Srect.Top < 0 then OffsetRect(Srect, 0, -Srect.Top);
  if Srect.Right > Screen.Width then OffsetRect(Srect, -(Srect.Right - Screen.Width), 0);
  if Srect.Bottom > Screen.Height then OffsetRect(Srect, 0, -(Srect.Bottom - Screen.Height));

  C := TCanvas.Create;
  try
    C.Handle := GetDC(GetDesktopWindow);
    Image2.Canvas.CopyRect(Drect, C, Srect);
  finally
    ReleaseDC(hDesktop, C.Handle);
    C.Free;
  end;
  with Image2.Canvas do begin
    DmX := Slider.Position * 2 * (dX - Srect.Left);
    DmY := Slider.Position * 2 * (dY - Srect.Top);
    MoveTo(DmX - (iWidth div 4), DmY); // -
    LineTo(DmX + (iWidth div 4), DmY); // -
    MoveTo(DmX, DmY - (iHeight div 4)); // |
    LineTo(DmX, DmY + (iHeight div 4)); // |
  end;
end;

procedure TDXGlueItEditor.mainPageControlChange(Sender: TObject);
begin
  Image2.Visible := mainPageControl.ActivePage = tsPreview;
end;

procedure TDXGlueItEditor.SetOperationGlueIt(const Value: TOperationGlueIt);
begin
  FOperationGlueIt := Value;
  if FOperationGlueIt = ogiNew then
    mainPageControl.ActivePage := tsGlueIt
  else
    mainPageControl.ActivePage := tsPreview;
end;

procedure TDXGlueItEditor.btnGetTransparentcolorClick(Sender: TObject);
begin
  SelectionOfTransparentColor := (Sender as TSpeedButton).Down;
end;

procedure TDXGlueItEditor.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if SelectionOfTransparentColor then begin
    panTColor.Color := Image1.Picture.Bitmap.Canvas.Pixels[X, Y];
    btnGetTransparentcolor.Down := False;
    SelectionOfTransparentColor := False;
  end;
end;

procedure TDXGlueItEditor.acGetTransparentExecute(Sender: TObject);
begin
  SelectionOfTransparentColor := (Sender as TSpeedButton).Down;
end;

procedure TDXGlueItEditor.chbForceSizeClick(Sender: TObject);
begin
  EWidthOfImages.Enabled := chbForceSize.Checked;
  LWidthOfImages.Enabled := chbForceSize.Checked;
  EHeightOfImages.Enabled := chbForceSize.Checked;
  LHeightOfImages.Enabled := chbForceSize.Checked;
  chbCentered.Enabled := chbForceSize.Checked;
  chbCrop.Enabled := chbForceSize.Checked;
end;

procedure TDXGlueItEditor.cbEffectsListChange(Sender: TObject);
begin
  if cbEffectsList.ItemIndex <> -1 then
    if Image1.Picture.Bitmap.Empty then begin
      ShowMessage('Image has not to be empty for effects!');
      cbEffectsList.ItemIndex := -1;
    //tmpBitmap.Assign(Image1.Picture.Bitmap);
    end;
end;

procedure TDXGlueItEditor.pbDurationMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if chbAutoAply.Checked then
    if not Image1.Picture.Bitmap.Empty then begin
      tmpPicture.Assign(Image1.Picture.Bitmap); //save default image
      if not tmpPicture.Graphic.Empty then begin
        DoBitmapEffect(tmpPicture);
        Application.ProcessMessages;
      end;
    end;
end;

procedure TDXGlueItEditor.pbDurationMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if chbAutoAply.Checked then
    if not Image1.Picture.Bitmap.Empty then
      if (MessageDlg('Do you want make changes permanent?', mtConfirmation, [mbYes, mbNo], 0) = mrNo) then
        Image1.Picture.Bitmap.Assign(tmpPicture); //restore default image
end;

procedure TDXGlueItEditor.chbAutoAplyClick(Sender: TObject);
begin
  btnApply.Enabled := not chbAutoAply.Checked;
end;

end.
