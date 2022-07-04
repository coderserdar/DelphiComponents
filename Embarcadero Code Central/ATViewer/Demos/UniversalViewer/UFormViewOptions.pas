{$I ATViewerOptions.inc}
{$I-}

unit UFormViewOptions;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ATxToolbarList, ExtCtrls, TntDialogs;

type
  TFormProc = procedure of object;

type
  TFormViewOptions = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    PageControl1: TPageControl;
    tabIntf: TTabSheet;
    tabShortcuts: TTabSheet;
    boxIntf: TGroupBox;
    labLang: TLabel;
    edLang: TComboBox;
    chkShell: TCheckBox;
    chkToolbar: TCheckBox;
    tabMisc: TTabSheet;
    ListKeys: TListView;
    HotKey1: THotKey;
    labShortcut: TLabel;
    btnKeyOk: TButton;
    chkBorder: TCheckBox;
    chkSingleInst: TCheckBox;
    chkMenu: TCheckBox;
    chkStatusBar: TCheckBox;
    chkNav: TCheckBox;
    boxMisc: TGroupBox;
    chkResolveLinks: TCheckBox;
    chkShowHidden: TCheckBox;
    chkMenuIcons: TCheckBox;
    tabText: TTabSheet;
    tabMedia: TTabSheet;
    boxText: TGroupBox;
    labTextFixedWidth: TLabel;
    labTabSize: TLabel;
    labTextLength: TLabel;
    edTextWidth: TEdit;
    chkTextWidthFit: TCheckBox;
    chkTextAutoCopy: TCheckBox;
    edTextTabSize: TEdit;
    edTextLength: TEdit;
    boxMedia: TGroupBox;
    labMediaPlayCount: TLabel;
    labMediaMode: TLabel;
    chkMediaStart: TCheckBox;
    edMediaPlayCount: TEdit;
    edMediaMode: TComboBox;
    boxTextFont: TGroupBox;
    labTextFont1: TLabel;
    btnTextFont: TButton;
    labTextFontShow: TLabel;
    labTextColors: TLabel;
    btnTextColor: TButton;
    btnTextColorHexBack: TButton;
    btnTextColorHex1: TButton;
    btnTextColorHex2: TButton;
    btnTextColorGutter: TButton;
    chkTextOemSpecial: TCheckBox;
    btnTextFontOEM: TButton;
    labTextFontShowOEM: TLabel;
    boxImage: TGroupBox;
    chkImageResample: TCheckBox;
    chkImageTransp: TCheckBox;
    labColorImage: TLabel;
    btnMediaColor: TButton;
    btnMediaColorLabel: TButton;
    btnMediaColorLabelErr: TButton;
    boxTextSearch: TGroupBox;
    edSearchIndent: TEdit;
    labSearchIndent: TLabel;
    chkSearchSel: TCheckBox;
    boxTextReload: TGroupBox;
    chkTextReload: TCheckBox;
    chkTextReloadBeep: TCheckBox;
    chkTextReloadTail: TCheckBox;
    chkSearchNoMsg: TCheckBox;
    FontDialog2: TFontDialog;
    boxPrint: TGroupBox;
    labFontFooter: TLabel;
    btnFontFooter: TButton;
    labFooterFontShow: TLabel;
    edIcon: TComboBox;
    labIcon: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    chkMediaLoop: TCheckBox;
    chkTextWrap: TCheckBox;
    chkTextNonPrint: TCheckBox;
    chkImageFit: TCheckBox;
    chkImageFitBig: TCheckBox;
    chkImageCenter: TCheckBox;
    tabFile: TTabSheet;
    boxExt: TGroupBox;
    labText: TLabel;
    labImages: TLabel;
    labMedia: TLabel;
    labInternet: TLabel;
    labRTF: TLabel;
    edText: TEdit;
    edImages: TEdit;
    edMedia: TEdit;
    edInternet: TEdit;
    edRTF: TEdit;
    btnTextOptions: TButton;
    btnImageOptions: TButton;
    btnGutterOptions: TButton;
    labViewerTitle: TLabel;
    labViewerMode: TLabel;
    edViewerTitle: TComboBox;
    edViewerMode: TComboBox;
    chkImageFitWindow: TCheckBox;
    chkImageLabel: TCheckBox;
    boxInternet: TGroupBox;
    chkWebAcceptAll: TCheckBox;
    chkWebOffline: TCheckBox;
    SaveDialog1: TSaveDialog;
    chkTextURLs: TCheckBox;
    btnTextColorURL: TButton;
    labFileSort: TLabel;
    edFileSort: TComboBox;
    tabHistory: TTabSheet;
    boxHistory: TGroupBox;
    chkSaveFolder: TCheckBox;
    chkSavePosition: TCheckBox;
    chkSaveRecents: TCheckBox;
    chkSaveSearch: TCheckBox;
    btnClearRecent: TButton;
    btnClearSearch: TButton;
    chkShowCfm: TCheckBox;
    procedure btnTextFontClick(Sender: TObject);
    procedure btnTextColorClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnTextOptionsClick(Sender: TObject);
    procedure chkTextWidthFitClick(Sender: TObject);
    procedure btnMediaColorClick(Sender: TObject);
    procedure btnTextColorHex1Click(Sender: TObject);
    procedure btnTextColorHex2Click(Sender: TObject);
    procedure edLangChange(Sender: TObject);
    procedure btnTextFontOEMClick(Sender: TObject);
    procedure ListKeysSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnKeyOkClick(Sender: TObject);
    procedure chkTextReloadClick(Sender: TObject);
    procedure btnTextColorHexBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edMediaModeChange(Sender: TObject);
    procedure btnImageOptionsClick(Sender: TObject);
    procedure chkTextOemSpecialClick(Sender: TObject);
    procedure btnTextColorGutterClick(Sender: TObject);
    procedure btnMediaColorLabelClick(Sender: TObject);
    procedure btnMediaColorLabelErrClick(Sender: TObject);
    procedure btnClearRecentClick(Sender: TObject);
    procedure btnClearSearchClick(Sender: TObject);
    procedure btnFontFooterClick(Sender: TObject);
    procedure edIconChange(Sender: TObject);
    procedure btnGutterOptionsClick(Sender: TObject);
    procedure btnTextColorURLClick(Sender: TObject);
  private
    { Private declarations }
    procedure FSaveIconsNames(const FileName: string);

  public
    { Public declarations }
    ffImgList: TImageList; //Original (not current!) ImageList
    ffToolbar: TToolbarList; //Toolbar object

    ffTextFontName: string;
    ffTextFontSize: integer;
    ffTextFontColor: TColor;
    ffTextFontStyle: TFontStyles;
    ffTextFontCharset: TFontCharset;

    ffTextFontOEMName: string;
    ffTextFontOEMSize: integer;
    ffTextFontOEMColor: TColor;
    ffTextFontOEMStyle: TFontStyles;
    ffTextFontOEMCharset: TFontCharset;

    ffFooterFontName: string;
    ffFooterFontSize: integer;
    ffFooterFontColor: TColor;
    ffFooterFontStyle: TFontStyles;
    ffFooterFontCharset: TFontCharset;

    ffTextBackColor: TColor;
    ffTextHexColor1: TColor;
    ffTextHexColor2: TColor;
    ffTextHexColorBack: TColor;
    ffTextGutterColor: TColor;
    ffTextUrlColor: TColor;
    ffTextDetect: boolean;
    ffTextDetectOEM: boolean;
    ffTextDetectSize: DWORD;
    ffTextDetectLimit: DWORD;
    ffMediaColor: TColor;
    ffMediaColorLabel: TColor;
    ffMediaColorLabelErr: TColor;
    ffOptLang: string;
    ffOptIcon: string;
    
    ffIViewEnabled: boolean;
    ffIViewExeName: string;
    ffIViewExtList: string;
    ffIViewHighPriority: boolean;
    ffIJLEnabled: boolean;
    ffIJLExtList: string;

    ffShowGutter: boolean;
    ffShowLines: boolean;
    ffLinesBufSize: integer;
    ffLinesCount: integer;
    ffLinesStep: integer;
    ffLinesExtUse: boolean;
    ffLinesExtList: string;

    ffGutterFontName: string;
    ffGutterFontSize: integer;
    ffGutterFontColor: TColor;
    ffGutterFontStyle: TFontStyles;
    ffGutterFontCharset: TFontCharSet;

    ffClearRecent: TFormProc;
    ffClearSearch: TFormProc;
  end;

implementation

uses
  ATViewer, ATxSProc, ATxParamStr,
  ATxMsgProc, ATxMsg,
  ATxUtils, ATxIconsProc,
  Menus,
  UFormViewOptionsText,
  UFormViewOptionsImages,
  UFormViewOptionsGutter;

{$R *.DFM}

procedure TFormViewOptions.btnTextFontClick(Sender: TObject);
begin
  with FontDialog1 do
    begin
    Font.Name:= ffTextFontName;
    Font.Size:= ffTextFontSize;
    Font.Color:= ffTextFontColor;
    Font.Style:= ffTextFontStyle;
    Font.CharSet:= ffTextFontCharset;
    if Execute then
      begin
      ffTextFontName:= Font.Name;
      ffTextFontSize:= Font.Size;
      ffTextFontColor:= Font.Color;
      ffTextFontStyle:= Font.Style;
      ffTextFontCharset:= Font.CharSet;
      labTextFontShow.Caption:= ffTextFontName + ', ' + IntToStr(ffTextFontSize);
      end;
    end;
end;

procedure TFormViewOptions.btnTextFontOEMClick(Sender: TObject);
begin
  with FontDialog1 do
    begin
    Font.Name:= ffTextFontOEMName;
    Font.Size:= ffTextFontOEMSize;
    Font.Color:= ffTextFontOEMColor;
    Font.Style:= ffTextFontOEMStyle;
    Font.CharSet:= ffTextFontOEMCharset;
    if Execute then
      begin
      ffTextFontOEMName:= Font.Name;
      ffTextFontOEMSize:= Font.Size;
      ffTextFontOEMColor:= Font.Color;
      ffTextFontOEMStyle:= Font.Style;
      ffTextFontOEMCharset:= Font.CharSet;
      labTextFontShowOEM.Caption:= ffTextFontOEMName + ', ' + IntToStr(ffTextFontOEMSize);
      end;
    end;
end;

procedure TFormViewOptions.btnTextColorClick(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffTextBackColor;
    if Execute then
      ffTextBackColor:= Color;
    end;
end;

procedure TFormViewOptions.btnTextColorHexBackClick(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffTextHexColorBack;
    if Execute then
      ffTextHexColorBack:= Color;
    end;
end;

procedure TFormViewOptions.btnTextColorHex1Click(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffTextHexColor1;
    if Execute then
      ffTextHexColor1:= Color;
    end;
end;

procedure TFormViewOptions.btnTextColorHex2Click(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffTextHexColor2;
    if Execute then
      ffTextHexColor2:= Color;
    end;
end;

procedure TFormViewOptions.btnTextColorGutterClick(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffTextGutterColor;
    if Execute then
      ffTextGutterColor:= Color;
    end;
end;

procedure TFormViewOptions.btnTextColorURLClick(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffTextUrlColor;
    if Execute then
      ffTextUrlColor:= Color;
    end;
end;


procedure TFormViewOptions.FormShow(Sender: TObject);
var
  h: THandle;
  fdA: TWin32FindDataA;
  fdW: TWin32FindDataW;
  Mask: WideString;
  S: string;
  n: integer;
  Rec: PToolbarButtonRec;
begin
  //Init captions
  {$I Lang.FormViewOptions.inc}

  //Update controls
  labTextFontShow.Caption:= ffTextFontName + ', ' + IntToStr(ffTextFontSize);
  labTextFontShowOEM.Caption:= ffTextFontOEMName + ', ' + IntToStr(ffTextFontOEMSize);
  labFooterFontShow.Caption:= ffFooterFontName + ', ' + IntToStr(ffFooterFontSize);

  chkTextWidthFitClick(Self);
  chkTextOemSpecialClick(Self);
  chkTextReloadClick(Self);
  edMediaModeChange(Self);

  //List languages
  Mask:= SLangFN('*');
  if Win32Platform=VER_PLATFORM_WIN32_NT
    then h:= FindFirstFileW(PWChar(Mask), fdW)
    else h:= FindFirstFileA(PAnsiChar(AnsiString(Mask)), fdA);

  if h<>INVALID_HANDLE_VALUE then
    with edLang do
      try
        Items.BeginUpdate;
        Items.Clear;

        repeat
          if Win32Platform=VER_PLATFORM_WIN32_NT
            then S:= fdW.cFileName
            else S:= fdA.cFileName;
          S:= ChangeFileExt(S, '');
          Items.Append(S);

          if Win32Platform=VER_PLATFORM_WIN32_NT
            then begin if not FindNextFileW(h, fdW) then Break end
            else begin if not FindNextFileA(h, fdA) then Break end;
        until false;

        n:= Items.IndexOf(ffOptLang);
        if n >= 0
          then ItemIndex:= n
          else ItemIndex:= Items.IndexOf('English');
      finally
        Windows.FindClose(h);
        Items.EndUpdate;
      end;

  //List icons
  with edIcon do
    try
      Items.BeginUpdate;
      Items.Clear;
      Items.Add(MsgViewerIconDef);

      Mask:= SIconsFN('*');
      if Win32Platform=VER_PLATFORM_WIN32_NT
        then h:= FindFirstFileW(PWChar(Mask), fdW)
        else h:= FindFirstFileA(PAnsiChar(AnsiString(Mask)), fdA);

      if h<>INVALID_HANDLE_VALUE then
        repeat
          if Win32Platform=VER_PLATFORM_WIN32_NT
            then S:= fdW.cFileName
            else S:= fdA.cFileName;
          S:= ChangeFileExt(S, '');
          Items.Append(S);

          if Win32Platform=VER_PLATFORM_WIN32_NT
            then begin if not FindNextFileW(h, fdW) then Break end
            else begin if not FindNextFileA(h, fdA) then Break end;
        until false;

      n:= Items.IndexOf(ffOptIcon);
      if n >= 0
        then ItemIndex:= n
        else ItemIndex:= 0;

      Items.Add(MsgViewerIconSave);
    finally
      Windows.FindClose(h);
      Items.EndUpdate;
    end;


  //List shortcuts
  ffToolbar.RestoreShortcuts;

  with ListKeys do
    begin
    Items.BeginUpdate;
    Items.Clear;
    SmallImages:= ffToolbar.ImageList;

    for n:= 1 to cToolbarButtonsMax do
      begin
      if not ffToolbar.GetAvail(n, Rec) then Break;
      if Rec.FMenuItem.Caption<>'-' then
        with Items.Add do
          begin
          Caption:= GetToolbarButtonId(Rec^);
          SubItems.Add(ShortcutToText(Rec.FMenuItem.Shortcut));
          ImageIndex:= Rec.FMenuItem.ImageIndex;
          Data:= pointer(n);
          end;
      end;

    Items.EndUpdate;

    if Items.Count>0 then
      Selected:= Items[0];
    end;
end;

procedure TFormViewOptions.btnTextOptionsClick(Sender: TObject);
begin
  with TFormViewOptionsText.Create(Self) do
    try
      chkDetect.Checked:= ffTextDetect;
      chkDetectOEM.Checked:= ffTextDetectOEM;
      edDetectSize.Text:= IntToStr(ffTextDetectSize);
      edDetectLimit.Text:= IntToStr(ffTextDetectLimit);
      if ShowModal=mrOk then
        begin
        ffTextDetect:= chkDetect.Checked;
        ffTextDetectOEM:= chkDetectOEM.Checked;
        ffTextDetectSize:= StrToIntDef(edDetectSize.Text, ffTextDetectSize);
        ffTextDetectLimit:= StrToIntDef(edDetectLimit.Text, ffTextDetectLimit);
        end;
    finally
      Release;
    end;
end;

procedure TFormViewOptions.chkTextWidthFitClick(Sender: TObject);
begin
  edTextWidth.Enabled:= not chkTextWidthFit.Checked;
  labTextFixedWidth.Enabled:= edTextWidth.Enabled;
end;

procedure TFormViewOptions.btnMediaColorClick(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffMediaColor;
    if Execute then
      ffMediaColor:= Color;
    end;
end;


procedure TFormViewOptions.edLangChange(Sender: TObject);
begin
  with edLang do
    begin
    if ItemIndex>=0
      then ffOptLang:= Items[ItemIndex]
      else ffOptLang:= 'English';
    DroppedDown:= false;
    end;
  SetMsgLanguage(ffOptLang);
  FormShow(Self);
end;

procedure TFormViewOptions.ListKeysSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  Rec: PToolbarButtonRec;
begin
  with ListKeys do
    begin
    btnKeyOk.Enabled:= Assigned(Selected);
    HotKey1.Enabled:= btnKeyOk.Enabled;
    if Assigned(Selected) then
      with Selected do
        begin
        if ffToolbar.GetAvail(integer(Data), Rec) then
          HotKey1.HotKey:= Rec.FMenuItem.Shortcut;
        end;
    end;
end;

procedure TFormViewOptions.btnKeyOkClick(Sender: TObject);
var
  Rec: PToolbarButtonRec;
begin
  with ListKeys do
    if Assigned(Selected) then
      with Selected do
        begin
        if ffToolbar.GetAvail(integer(Data), Rec) then
          begin
          Rec.FMenuItem.Shortcut:= HotKey1.HotKey;
          SubItems[0]:= ShortCutToText(HotKey1.HotKey);
          end;
        end;
end;

procedure TFormViewOptions.chkTextReloadClick(Sender: TObject);
begin
  chkTextReloadTail.Enabled:= chkTextReload.Checked;
  chkTextReloadBeep.Enabled:= chkTextReload.Checked;
end;


procedure TFormViewOptions.FormCreate(Sender: TObject);
var
  M: TATViewerMediaMode;
begin
  ffImgList:= nil;
  ffToolbar:= nil;

  with edMediaMode do
    begin
    Items.Clear;
    for M:= Low(TATViewerMediaMode) to High(TATViewerMediaMode) do
      if M <> Low(TATViewerMediaMode) then
        Items.Add(cATViewerMediaModeNames[M]);
    Enabled:= Items.Count > 0;
    end;
end;

procedure TFormViewOptions.edMediaModeChange(Sender: TObject);
begin
  {$ifdef MEDIA_PLAYER}
  edMediaPlayCount.Enabled:= edMediaMode.ItemIndex > 0;
  labMediaPlayCount.Enabled:= edMediaPlayCount.Enabled;
  {$endif}
end;

procedure TFormViewOptions.btnImageOptionsClick(Sender: TObject);
begin
  with TFormViewOptionsImages.Create(Self) do
    try
      chkUseIView.Checked:= ffIViewEnabled;
      edExeIView.Text:= ffIViewExeName;
      edExtIView.Text:= ffIViewExtList;
      chkPriority.Checked:= ffIViewHighPriority;
      chkUseIJL.Checked:= ffIJLEnabled;
      edExtIJL.Text:= ffIJLExtList;
      if ShowModal=mrOk then
        begin
        ffIViewEnabled:= chkUseIView.Checked;
        ffIViewExeName:= edExeIView.Text;
        ffIViewExtList:= edExtIView.Text;
        ffIViewHighPriority:= chkPriority.Checked;
        ffIJLEnabled:= chkUseIJL.Checked;
        ffIJLExtList:= edExtIJL.Text;
        end;
    finally
      Release;
    end;
end;

procedure TFormViewOptions.btnGutterOptionsClick(Sender: TObject);
begin
  with TFormViewOptionsGutter.Create(Self) do
    try
      chkShowGutter.Checked:= ffShowGutter;
      chkShowLines.Checked:= ffShowLines;
      chkLineExt.Checked:= ffLinesExtUse;
      edLineExt.Text:= ffLinesExtList;
      edLineSize.Text:= IntToStr(ffLinesBufSize);
      edLineCount.Text:= IntToStr(ffLinesCount);
      edLineStep.Text:= IntToStr(ffLinesStep);

      ffFontName:= ffGutterFontName;
      ffFontSize:= ffGutterFontSize;
      ffFontColor:= ffGutterFontColor;
      ffFontStyle:= ffGutterFontStyle;
      ffFontCharset:= ffGutterFontCharset;

      if ShowModal=mrOk then
        begin
        ffShowGutter:= chkShowGutter.Checked;
        ffShowLines:= chkShowLines.Checked;
        ffLinesExtUse:= chkLineExt.Checked;
        ffLinesExtList:= edLineExt.Text;
        ffLinesBufSize:= StrToIntDef(edLineSize.Text, 300);
        ffLinesCount:= StrToIntDef(edLineCount.Text, 2000);
        ffLinesStep:= StrToIntDef(edLineStep.Text, 5);

        ffGutterFontName:= ffFontName;
        ffGutterFontSize:= ffFontSize;
        ffGutterFontColor:= ffFontColor;
        ffGutterFontStyle:= ffFontStyle;
        ffGutterFontCharset:= ffFontCharset;
        end;
    finally
      Release;
    end;
end;


procedure TFormViewOptions.chkTextOemSpecialClick(Sender: TObject);
begin
  labTextFontShowOEM.Enabled:= chkTextOemSpecial.Checked;
  btnTextFontOEM.Enabled:= chkTextOemSpecial.Checked;
end;


procedure TFormViewOptions.btnMediaColorLabelClick(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffMediaColorLabel;
    if Execute then
      ffMediaColorLabel:= Color;
    end;
end;

procedure TFormViewOptions.btnMediaColorLabelErrClick(Sender: TObject);
begin
  with ColorDialog1 do
    begin
    Color:= ffMediaColorLabelErr;
    if Execute then
      ffMediaColorLabelErr:= Color;
    end;
end;

procedure TFormViewOptions.btnClearRecentClick(Sender: TObject);
begin
  if Assigned(ffClearRecent) then
    ffClearRecent;
end;

procedure TFormViewOptions.btnClearSearchClick(Sender: TObject);
begin
  if Assigned(ffClearSearch) then
    ffClearSearch;
end;

procedure TFormViewOptions.btnFontFooterClick(Sender: TObject);
begin
  with FontDialog2 do
    begin
    Font.Name:= ffFooterFontName;
    Font.Size:= ffFooterFontSize;
    Font.Color:= ffFooterFontColor;
    Font.Style:= ffFooterFontStyle;
    Font.CharSet:= ffFooterFontCharset;
    if Execute then
      begin
      ffFooterFontName:= Font.Name;
      ffFooterFontSize:= Font.Size;
      ffFooterFontColor:= Font.Color;
      ffFooterFontStyle:= Font.Style;
      ffFooterFontCharset:= Font.CharSet;
      labFooterFontShow.Caption:= ffFooterFontName + ', ' + IntToStr(ffFooterFontSize);
      end;
    end;
end;

procedure TFormViewOptions.FSaveIconsNames(const FileName: string);
var
  f: TextFile;
  Rec: PToolbarButtonRec;
  n, i: integer;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  if IOResult <> 0 then Exit;

  Writeln(f, Format('Icons order in the saved "%s" file:', [ChangeFileExt(FileName, '.bmp')]));
  Writeln(f);

  try
    for i:= 0 to ffImgList.Count - 1 do
      for n:= 1 to cToolbarButtonsMax do
        if ffToolbar.GetAvail(n, Rec) then
          if Rec.FMenuItem.ImageIndex = i then
            begin
            Writeln(f, Format('%d  %s', [i, GetToolbarButtonId(Rec^)]));
            Break;
            end;
  finally
    CloseFile(f);
  end;
end;


procedure TFormViewOptions.edIconChange(Sender: TObject);
var
  L: TImageList;
  i: Integer;
begin
  with edIcon do
    //Save template
    if (ItemIndex = Items.Count - 1) then
      begin
      with SaveDialog1 do
        if Execute then
          begin
          FSaveIcons(ffImgList, FileName);
          FSaveIconsNames(ChangeFileExt(FileName, '.txt'));
          end;
      end
    else
    //Default set
    if (ItemIndex = 0) then
      begin
      Panel1.Visible := False;
      ffOptIcon := '';
      end
    else
    //Custom set
      with Image1 do
        begin
        Panel1.Visible := True;
        ffOptIcon := Text;

        L:= TImageList.CreateSize(16, 16);
        try
          FLoadIcons(L, SIconsFN(edIcon.Text));

          with Panel1 do
            SetBounds(Left, edIcon.Top + edIcon.Height - L.Height, 6 * L.Width + 2, L.Height + 2);
          Picture.Bitmap.Width := Panel1.Width;
          Picture.Bitmap.Height := Panel1.Height;

          Canvas.Brush.Color:= clBtnFace;
          Canvas.FillRect(Rect(0, 0, Width, Height));

          for i:= 0 to 6 do
            L.Draw(Canvas, i * L.Width, 0, i);
        finally
          L.Free;
        end;

        Invalidate;
        end;
end;

end.
