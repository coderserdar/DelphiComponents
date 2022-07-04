// unit PlayListConfig
//
// This unit takes charge of managing Playlist for Demo program.
// You can create playlists and add items to playlist, delete items from playlist
//  using this unit.
//
//       written by Silhwan Hyun  (hyunsh@hanafos.com)
//
// Ver 1.1                        29 Jan 2009
//   - Changed form appearance with added procedures for skin
//   - Added procedures to support playlist file (*.pls, *.m3u)
//
//
// Ver 1.0                        16 Jul 2008
//   - Initial release

unit PlayListConfig;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, FileCtrl, inifiles, PluginCtrl, DnDListBox;

const
  WM_PlayListConfig = WM_USER + 116; // message to notify that an event occured, from Play List Manager
  PlayList_Close = 0;
  PlayList_Delete = 1;
  PlayList_DBClick = 2;
  Title_Changed = 3;

type
  TSizingDir = (Top_Left, Top_Side, Top_Right, Right_Side,
                Botom_Right, Bottom_Side, Bottom_Left, Left_Side);

  TImage = class(ExtCtrls.TImage)
  private
    FOnMouseEnter : TNotifyEvent;
    FOnMouseLeave : TNotifyEvent;

  private
    procedure SetMouseEnter(evt:TNotifyEvent);
    procedure SetMouseLeave(evt:TNotifyEvent);

  protected
    procedure WndProc(var Message:TMessage); override;

  public
    constructor Create(AOwner: TComponent); override;

  public
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetMouseEnter ;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetMouseLeave ;

  end;

  TPlayListConfigForm = class(TForm)
    picMap: TImage;
    picTopL: TImage;
    picTopF: TImage;
    picTopText: TImage;
    picTopM: TImage;
    picTopStretchL: TImage;
    picTopC: TImage;
    picTopStretchR: TImage;
    picClose: TImage;
    picTopTextSrc: TImage;
    picBorderL: TImage;
    picBorderR: TImage;
    picBottomL: TImage;
    picBottomM: TImage;
    picBottomR: TImage;
    Panel1: TPanel;
    PlayListBox: TDnDListBox;
    cbShowArtist: TCheckBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    picAddFileBtn: TImage;
    picAddURLBtn: TImage;
    picAddListBtn: TImage;
    picAddDirBtn: TImage;
    picDeleteFileBtn: TImage;
    picDeleteAllBtn: TImage;
    picFileInfoBtn: TImage;
    picSaveListBtn: TImage;
    picMenu: TImage;
    procedure FormCanResize(Sender: TObject; var NewWidth,
      NewHeight: Integer; var Resize: Boolean);
    procedure FormResize(Sender: TObject);
    procedure picCloseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picBottomRMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picBottomRMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure picTopItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picTopItemsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure picTopMDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure PlayListBoxClick(Sender: TObject);
    procedure PlayListBoxDblClick(Sender: TObject);
    procedure PlayListBoxDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure PlayListBoxFileDropped(Sender: TObject; FileName: String;
      var accept: Boolean);
    procedure picButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picButtonMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnAddFileClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnFileInfoClick(Sender: TObject);
    procedure btnAddDirClick(Sender: TObject);
    procedure btnDeleteAllClick(Sender: TObject);
    procedure btnADDListClick(Sender: TObject);
    procedure btnSaveListClick(Sender: TObject);
    procedure btnAddURLClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure picTopItemsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picBorderLMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure picBorderLMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picBorderLMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picBorderRMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure picBorderRMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picBorderRMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picBottomItemsMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure picBottomItemsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picBottomItemsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure picCloseClick(Sender: TObject);

  private
    { Private declarations }
    CURSOR_RESIZE  : HCURSOR;
    CURSOR_RESIZE2 : HCURSOR;
    CURSOR_RESIZE3 : HCURSOR;

    m_bAllowDock  : boolean;
    m_bFocused : boolean;
    m_bShown : boolean;
    m_ptResizeFrom : TPOINT;
    m_bSizing  : boolean;
    b_BtnPressed : boolean;

    Show_LocX : integer;
    Show_LocY : integer;
    Show_Width : integer;
    Show_Height : integer;

    i_CursorOn : integer;
    CurMode : TPlayerMode;
    CurIndex : integer;

    procedure ApplyLocalLanguage;
    procedure LoadSkin;
    procedure SetImage(Destination: TImage;
                                x, y, W, H: integer;
                                Source: TImage;
                                startX, StartY: integer);
    function  WriteTitle(title : string; GetWidthOnly : boolean) : integer;
    procedure UpperImageCopy;
    procedure UpperImageLoad;
    procedure ImageCopy;
    procedure RelocateElement(Width_, Height_ : integer);
    procedure ScaleScreen;

    procedure BtnImageLoad(Btn_No : integer; b_Pressed, b_Highlighted : boolean);
    procedure ImageMouseEnter(Sender: TObject);
    procedure ImageMouseLeave(Sender: TObject);
    procedure SetImageMouseEvent(img: TImage);

    procedure RegisterFile(FileName, Artist, Title : string);
    function  FillPlayListItems(PlaylistName : string) : integer;

  public
    { Public declarations }
    procedure ShowAtLoc(Loc_X, Loc_Y, Init_Width, Init_Height : integer; AllowDock : boolean);
    procedure CheckTitleBar;
  //  procedure WndProc(var Msg : TMessage); override;
    procedure  AdjustWindSize(point : TPOINT; sDirection : TSizingDir);

    procedure ClearPlayList;
    function  AddPlayList(S : string) : integer;
    procedure AddTo_PlayList(FilePath : string; Title, Artist : string; SkipFileCheck : boolean);  // * Changed Ver 1.00.01
    procedure ChangeTitle(FilePath : string; Title, Artist : string);

    function  NumEntry : integer;
    function  SelectedEntry : integer;
    function  SelectedFile  : string;
    function  SelectEntry(ListIndex : integer) : string;
    function  SelectEntryByName(FilePath : string) : integer;
    procedure SetCurrentMode(Mode : TPlayerMode);

    procedure WMEnterSizeMove(var Msg: TMessage); message WM_ENTERSIZEMOVE;     // * Added at Ver 1.1
    procedure WMSizing(var Msg: TMessage); message WM_SIZING;                   // * Added at Ver 1.1
    procedure WMMoving(var Msg: TMessage); message WM_MOVING;                   // * Added at Ver 1.1
    procedure WMExitSizeMove(var Msg: TMessage); message WM_EXITSIZEMOVE;       // * Added at Ver 1.1
    procedure WMShowHideWindow(var Msg: TMessage); message WM_WINDOWPOSCHANGED; // * Added at Ver 1.1
    procedure WMSysCommand(var Msg: TMessage); message WM_SYSCOMMAND;           // * Added at Ver 1.1
    procedure WMCommand(var Msg: TMessage); message WM_COMMAND;                 // * Added at Ver 1.1
  end;

var
  PlayListConfigForm: TPlayListConfigForm;

implementation

uses BassTest, InputURL;

{$R *.DFM}


const
   msgCount = 14;

   stdWidth   = 340;
   stdHeight  = 300;
   MIN_WIDTH  = 339;  // should be (stdWidth - RESIZE_WIDTH) <= x < stdWidth
   MIN_HEIGHT = 150;
   RESIZE_WIDTH	 = 10;
   RESIZE_HEIGHT = 10;

   L_Margin = 11;   // Margin Left side
   R_Margin = 8;    // Margin Right side
   T_Margin = 20;   // Margin Top side
   B_Margin = 14;   // Margin Bottom side for frame

   m_nSnapOffset = 10;

var
   PicMapDC  : hdc;
   picMap2   : TImage;

   MagneticWndProc: TSubClass_Proc;
   FilePathList : TStringList;
   StreamInfo: TStreamInfo;
   SupportedBy: TSupportedBy;

   msg : array[1..msgCount] of string
          = ('No selected item',
             'No valid stream file',
             'Already registered stream file',
             'Selected item is in playing or paused state',
             'Cannot open file (BASSPlayer is not ready)',
             'Non-local, unsupported or invalid stream file',
             'Confirm',                                  //  7
             'Information',                              //  8
             'Skipped an item which is in use',          //  9
             'Playlist is empty',                        // 10
             'Playlist is being rewritten, OK to proceed ?',  // 11
             'Playlist is being cleared, OK to proceed ?',    // 12
             'Not a valid URL',                               // 13
             'Already registered URL');                       // 14

function RegisteredFile(FilePath : string; var RegNo : integer) : boolean;
var
   i : integer;
begin
   result := false;

   if FilePathList.Count = 0 then
      exit;

   for i := 1 to FilePathList.Count do
      if FilePathList[i-1] = FilePath then
      begin
         RegNo := i - 1;
         result := true;
         break;
      end;

end;


{ TMyImage }
// Adapted from the example code of Sungho Jang, KOREA
constructor TImage.Create(AOwner: TComponent); 
begin 
  FOnMouseEnter:=nil;
  FOnMouseLeave:=nil;
  inherited; 
end; 

procedure TImage.SetMouseEnter(evt: TNotifyEvent); 
begin 
  FOnMouseEnter:=evt; 
end;

procedure TImage.SetMouseLeave(evt: TNotifyEvent); 
begin
  FOnMouseLeave:=evt; 
end; 

procedure TImage.WndProc(var Message: TMessage); 
begin 
  if Message.Msg = CM_MOUSEENTER then 
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(self); 

  if Message.Msg = CM_MOUSELEAVE then 
    if Assigned(FOnMouseLeave) then 
      FOnMouseLeave(self); 

  inherited; 

end;


procedure TPlayListConfigForm.ApplyLocalLanguage;  // * New at Ver 1.22
var
  F: TextFile;
  SearchRec: TSearchRec;
  S: string;
  FormName : string;
  MyFormEntry : boolean;
  CommonEntry : boolean;

  i, j, SepPos : integer;
  ItemType : string[1];
  ItemName, ItemVal : string;
  Temp: TComponent;

  Lang_Str : array of string;
  Entries_Lang_Str : integer;
  Items_Lang_Str : integer;
 // tmpStr : string;
  S_org, S_local : string;

begin
  if FindFirst(ExtractFilePath(ParamStr(0)) + 'lang_*.txt', faAnyFile, SearchRec) <> 0 then
  begin
     FindClose(SearchRec);
     exit;
  end;
  
  MyFormEntry := false;
  CommonEntry := false;
  Items_Lang_Str := 0;
  SetLength(Lang_Str, 16);
  Entries_Lang_Str := 16;

  AssignFile(F, ExtractFilePath(ParamStr(0)) + SearchRec.Name);
  Reset(F);
  FindClose(SearchRec);

  while not Eof(F) do
  begin
     Readln(F, S);
     S := trim(S);

     if S = '' then
        continue;
     if copy(S, 1, 2) = '//' then
        continue;

     if (S[1] = '[') and (S[length(S)] = ']') then
     begin
        FormName := copy(S, 2, length(S) - 2);
        if FormName = Self.Name then
        begin
           MyFormEntry := true;
           CommonEntry := false;
           continue;
        end else if uppercase(FormName) = 'COMMON' then
        begin
           MyFormEntry := false;
           CommonEntry := true;
           continue;
        end else if MyFormEntry then
           break
        else begin
           MyFormEntry := false;
           CommonEntry := false;
           continue;
        end   
     end
     else if (not MyFormEntry) and (not CommonEntry) then
        continue;

     SepPos := pos('=', S);
     if SepPos = 0 then
        Continue;

     ItemVal := trim(copy(S, SepPos + 1, length(S) - SepPos));
     if ItemVal = '' then
        continue;
     if ItemVal[1] = '"' then
        ItemVal := copy(ItemVal, 2, length(ItemVal) - 1);
     if ItemVal[length(ItemVal)] = '"' then
        ItemVal := copy(ItemVal, 1, length(ItemVal) - 1);
     if ItemVal = '' then
        continue;

     if ((S[1] = '&') or (S[1] = '$') or (S[1] = '*')) then
     begin
        ItemType := S[1];
        ItemName := trim(copy(S, 2, SepPos - 2));

        if (ItemType = '&') and (ItemName = Self.Name) then  // Caption setting ?
        begin
           Self.Caption := ItemVal;
           continue;
        end;

        for i := ComponentCount - 1 downto 0 do
        begin
           Temp := Components[i];
           if not (Temp is TControl) then
              continue;
           if Temp.Name = ItemName then
           begin
              if ItemType = '&' then
              begin
                 if (Temp is TLabel) then
                    (Temp as TLabel).Caption := ItemVal
                 else if (Temp is TButton) then
                    (Temp as TButton).Caption := ItemVal
                 else if (Temp is TCheckBox) then
                    (Temp as TCheckBox).Caption := ItemVal
              end else if ItemType = '$' then
              begin
                 if (Temp is TEdit) then
                    (Temp as TEdit).Text := ItemVal;
              end else if ItemType = '*' then
                 (Temp as TControl).Hint := ItemVal;
           end;
        end;
     end else
     begin
     // Store message strings (format : Original=Local)
        Lang_Str[Items_Lang_Str] := trim(copy(S, 1, SepPos - 1)) + '=' + ItemVal;
        inc(Items_Lang_Str);
        if Items_Lang_Str = Entries_Lang_Str then
        begin
           Entries_Lang_Str := Entries_Lang_Str + 16;
           SetLength(Lang_Str, Entries_Lang_Str);
        end;
     end;
  end;

  CloseFile(F);
  SetLength(Lang_Str, Items_Lang_Str);

 // Substitute message string with local text.
  for i := 1 to Items_Lang_Str do
  begin
     SepPos := pos('=', Lang_Str[i - 1]);
     S_org := copy(Lang_Str[i - 1], 1, SepPos - 1);
     S_local := copy(Lang_Str[i - 1], SepPos + 1, length(Lang_Str[i - 1]) - SepPos);
     for j := 1 to msgCount do
        if msg[j] = S_org then
        begin
           msg[j] := S_local;
           break;
        end;
  end;

  SetLength(Lang_Str, 0);
end;

procedure TPlayListConfigForm.LoadSkin;
begin
  if FileExists(ExtractFilePath(ParamStr(0)) + 'Skin\Gen_.bmp') then
  begin
     try
       picMap.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skin\Gen_.bmp');
     except
       picMap.Picture.Bitmap.LoadFromResourceName(HInstance, 'GEN');
     end;
  end else
     picMap.Picture.Bitmap.LoadFromResourceName(HInstance, 'GEN');

  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;

  if FileExists(ExtractFilePath(ParamStr(0)) + 'Skin\CButtons_.bmp') then
  begin
     try
       picMap2.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skin\CButtons_.bmp');
     except
       picMap2.Picture.Bitmap.LoadFromResourceName(HInstance, 'CBUTTONS');
     end;
  end else
     picMap2.Picture.Bitmap.LoadFromResourceName(HInstance, 'CBUTTONS');

end;


procedure TPlayListConfigForm.ScaleScreen;
begin
  if WindowState <> wsMaximized then
    WindowState := wsMaximized
  else
  if WindowState = wsMaximized then
    WindowState := wsNormal;
end;


procedure TPlayListConfigForm.RelocateElement(Width_, Height_ : integer);
begin
 //  The placement of graphic element in title bar is as follows
 //  ------------------------------------------------------------------
 //  | TopL | TopStretchL | TopF | TopText | TopM | TopStretchR | TopC |
 //  ------------------------------------------------------------------

 // Self.Width := Width_;
 // Self.Height := Height_;
  SetBounds(Self.Left, Self.Top, Width_, Height_);

  picTopC.Canvas.MoveTo(Self.Width - picTopC.Width, 0);
  picClose.Canvas.MoveTo(Self.Width - 12, 2);
  picTopText.Canvas.MoveTo((Self.Width div 2) - (picTopText.Width div 2), 0);
 { picTopTextSrc.Canvas.MoveTo(
    (Self.Width div 2) - (picTopTextSrc.Width div 2), picTopText.Top + 4); }
  picTopL.Canvas.MoveTo(0, 0);
  picTopStretchL.Canvas.MoveTo(picTopL.Width, 0);
  picTopF.Canvas.MoveTo(picTopText.Left - picTopF.Width, 0);
  picTopM.Canvas.MoveTo(picTopText.Left + picTopText.Width, 0);
  picTopStretchR.Canvas.MoveTo(picTopM.Left + picTopM.Width, 0);

  picBorderL.Canvas.MoveTo(0, 20);
  picBorderR.Canvas.MoveTo(Self.Width - 8, 20);

  picBottomL.Canvas.MoveTo(0, Self.Height - 14);
  picBottomR.Canvas.MoveTo(Self.Width - picBottomR.Width, Self.Height - 14);
  picBottomM.Canvas.MoveTo(picBottomL.Width, Self.Height - 14);

end;

procedure TPlayListConfigForm.SetImage(Destination: TImage;
                                x, y, W, H: integer;
                                Source: TImage;
                                startX, StartY: integer);
var
  DestDC: HDC;
  SrcDC:  HDC;

begin
  DestDC := Destination.Picture.Bitmap.Canvas.Handle;
  SrcDC  := Source.Picture.Bitmap.Canvas.Handle;

  BitBlt(DestDC, x, y, W, H, SrcDC, startX, StartY, SRCCOPY);
  Destination.Refresh;
end;

procedure TPlayListConfigForm.UpperImageCopy;
begin
  if m_bFocused then
  begin
    SetImage(picTopC, 0, 0, 25, T_Margin, picMap, 130, 0);
    SetImage(picTopL, 0, 0, 25, T_Margin, picMap, 156, 0);
    SetImage(picTopF, 0, 0, 25, T_Margin, picMap, 26, 0);
    SetImage(picTopM, 0, 0, 25, T_Margin, picMap, 78, 0);
    SetImage(picMenu, 0, 0, 11, 10, picTopL, 4, 4);
  end else
  begin
    SetImage(picTopC, 0, 0, 25, T_Margin, picMap, 130, 21);
    SetImage(picTopL, 0, 0, 25, T_Margin, picMap, 156, 21);
    SetImage(picTopF, 0, 0, 25, T_Margin, picMap, 26, 21);
    SetImage(picTopM, 0, 0, 25, T_Margin, picMap, 78, 21);
    SetImage(picMenu, 0, 0, 11, 10, picTopL, 4, 4);
  end;

  SetImage(picClose, 0, 0, 9, 10, picTopC, 13, 2);
end;

procedure TPlayListConfigForm.UpperImageLoad;
var
  picTop_HDC: HDC;
  picW      : integer;

begin
  picTop_HDC := picTopText.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picTopText.Width;
  if m_bFocused then
     StretchBlt(picTop_HDC, 0, 0, picW, 20, PicMapDC, 52, 0, 25, 20, SRCCOPY)
  else
     StretchBlt(picTop_HDC, 0, 0, picW, 20, PicMapDC, 52, 21, 25, 20, SRCCOPY);
  picTopText.Refresh;

  picTop_HDC := picTopStretchL.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picTopStretchL.Width;
  if m_bFocused then
     StretchBlt(picTop_HDC, 0, 0, picW, 20, PicMapDC, 104, 0, 25, 20, SRCCOPY)
  else
     StretchBlt(picTop_HDC, 0, 0, picW, 20, PicMapDC, 104, 21, 25, 20, SRCCOPY);
  picTopStretchL.Refresh;

  picTop_HDC := picTopStretchR.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picTopStretchR.Width;
  if m_bFocused then
     StretchBlt(picTop_HDC, 0, 0, picW, 20, PicMapDC, 104, 0, 25, 20, SRCCOPY)
  else
     StretchBlt(picTop_HDC, 0, 0, picW, 20, PicMapDC, 104, 21, 25, 20, SRCCOPY);
  picTopStretchR.Refresh;

end;

procedure TPlayListConfigForm.ImageCopy;
var
  picBottomM_HDC: HDC;
  picW      : integer;

begin
  UpperImageCopy;   // for fixed width items
  UpperImageLoad;   // for variable width items

  picBottomM_HDC := picBottomM.Picture.Bitmap.Canvas.Handle;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picW := picBottomM.Width;
  StretchBlt(picBottomM_HDC, 0, 0, picW, B_Margin, PicMapDC, 127, 72, 25, B_Margin, SRCCOPY);
  picBottomM.Refresh;

  SetImage(picBottomR, 0, 0, 125, B_Margin, picMap, 0, 104{57});
  SetImage(picBottomL, 0, 0, 125, B_Margin, picMap, 0, 42);
  SetImage(picBorderL, 0, 0, L_Margin, 29, picMap, 127, 42);
  SetImage(picBorderR, 0, 0, R_Margin, 29, picMap, 139, 42);

  BtnImageLoad(picAddFileBtn.Tag, false, false);
  BtnImageLoad(picAddURLBtn.Tag, false, false);
  BtnImageLoad(picDeleteFileBtn.Tag, false, false);
  BtnImageLoad(picFileInfoBtn.Tag, false, false);
  BtnImageLoad(picAddDirBtn.Tag, false, false);
  BtnImageLoad(picDeleteAllBtn.Tag, false, false);
  BtnImageLoad(picAddListBtn.Tag, false, false);
  BtnImageLoad(picSaveListBtn.Tag, false, false);
end;

procedure TPlayListConfigForm.AdjustWindSize(point : TPOINT; sDirection : TSizingDir);
var
   rcWnd, rcNew : TRect;
   bResize : boolean;
begin
   GetWindowRect(Self.Handle, rcWnd);
   rcNew := rcWnd;
   bResize := false;

   case sDirection of
     Top_Side : begin
       if (point.y >= (m_ptResizeFrom.y + RESIZE_HEIGHT)) then
       begin   //Resize in Y direction downwards
         rcNew.Top := rcNew.Top + RESIZE_HEIGHT;
         if (rcNew.bottom - rcNew.Top) >= MIN_HEIGHT then
         begin
           bResize := TRUE;
           m_ptResizeFrom.y := m_ptResizeFrom.y + RESIZE_HEIGHT;
         end else
         begin
           rcNew.Top := rcNew.bottom - MIN_HEIGHT;
           if rcNew.Top <> rcWnd.Top then
              bResize := TRUE;

           m_ptResizeFrom.y := rcNew.Top;
         end
       end
       else if (point.y <= (m_ptResizeFrom.y - RESIZE_HEIGHT)) then
       begin   //Resize in Y direction upwards
         rcNew.Top := rcNew.Top - RESIZE_HEIGHT;
         bResize := TRUE;
         m_ptResizeFrom.y := m_ptResizeFrom.y - RESIZE_HEIGHT;
       end;
     end;   // end of sDirection = Top_Side

     Bottom_Side : begin
       if (point.y >= (m_ptResizeFrom.y + RESIZE_HEIGHT)) then
       begin   //Resize in Y direction downwards
         rcNew.Bottom := rcNew.Bottom + RESIZE_HEIGHT;
         bResize := TRUE;
         m_ptResizeFrom.y := m_ptResizeFrom.y + RESIZE_HEIGHT;
       end
       else if (point.y <= (m_ptResizeFrom.y - RESIZE_HEIGHT)) then
       begin  //Resize in Y direction upwards
         if (rcNew.bottom - rcNew.Top) >= MIN_HEIGHT then
         begin
           bResize := TRUE;
           rcNew.bottom := rcNew.bottom - RESIZE_HEIGHT;
           m_ptResizeFrom.y := m_ptResizeFrom.y - RESIZE_HEIGHT;
         end else
         begin
           rcNew.bottom := rcNew.Top + MIN_HEIGHT;
           if rcNew.bottom <> rcWnd.bottom then
              bResize := TRUE;

           m_ptResizeFrom.y := rcNew.Top;
         end
       end;
     end;   // end of sDirection = Bottom_Side

     Left_Side : begin
       if (point.x >= (m_ptResizeFrom.x + RESIZE_WIDTH)) then
       begin   //Resize in X direction inwards
         rcNew.Left := rcNew.Left + RESIZE_WIDTH;
         if (rcNew.Right - rcNew.Left) >= MIN_WIDTH then
         begin
           bResize := TRUE;
           m_ptResizeFrom.x := m_ptResizeFrom.x + RESIZE_WIDTH;
         end else
         begin
           rcNew.Left := rcNew.Right - MIN_WIDTH;
           if rcNew.Left <> rcWnd.Left then
              bResize := TRUE;

           m_ptResizeFrom.x := rcNew.Left;
         end
       end
       else if (point.x <= (m_ptResizeFrom.x - RESIZE_WIDTH)) then
       begin   //Resize in Y direction outwards
         rcNew.Left := rcNew.Left - RESIZE_WIDTH;
         bResize := TRUE;
         m_ptResizeFrom.x := m_ptResizeFrom.x - RESIZE_WIDTH;
       end;
     end;   // end of sDirection = Left_Side

     Right_Side : begin
       if (point.x >= (m_ptResizeFrom.x + RESIZE_WIDTH)) then
       begin   //Resize in X direction outwards
         rcNew.Right := rcNew.Right + RESIZE_WIDTH;
         bResize := TRUE;
         m_ptResizeFrom.x := m_ptResizeFrom.x + RESIZE_WIDTH;
       end
       else if (point.x <= (m_ptResizeFrom.x - RESIZE_WIDTH)) then
       begin   //Resize in X direction inwards
         rcNew.Right := rcNew.Right - RESIZE_WIDTH;
         if (rcNew.Right - rcNew.Left) >= MIN_WIDTH then
         begin
           bResize := TRUE;
           m_ptResizeFrom.x := m_ptResizeFrom.x - RESIZE_WIDTH;
         end else
         begin
           rcNew.Right := rcNew.Left + MIN_WIDTH;
           if rcNew.Right <> rcWnd.Right then
              bResize := TRUE;

           m_ptResizeFrom.x := rcNew.Right;
         end
       end;
     end;   // end of sDirection = Right_Side

   end;


   if bResize then
   begin
      Panel1.Visible := false;   // To reduce flickering

  // Send Message to get the adjusted RECT for re-sizing at the edge of family window.
      case sDirection of
        Top_Side : SendMessage(Self.Handle, WM_SIZING, WMSZ_TOP, integer(@rcNew));
        Bottom_Side : SendMessage(Self.Handle, WM_SIZING, WMSZ_BOTTOM, integer(@rcNew));
        Left_Side : SendMessage(Self.Handle, WM_SIZING, WMSZ_LEFT, integer(@rcNew));
        Right_Side : SendMessage(Self.Handle, WM_SIZING, WMSZ_RIGHT, integer(@rcNew));
      end;

      MoveWindow(Self.Handle, rcNew.left, rcNew.top,
                 (rcNew.right - rcNew.left), (rcNew.bottom - rcNew.Top), true);
      Panel1.Visible := true;
   end;               
end;

procedure TPlayListConfigForm.FormResize(Sender: TObject);
var
  picBorderL_hDC: HDC;
  picBorderR_hDC: HDC;

  WidthTotal : integer;
begin
 //  The placement of graphic element in title bar is as follows
 //  ------------------------------------------------------------------
 //  | TopL | TopStretchL | TopF | TopText | TopM | TopStretchR | TopC |
 //  ------------------------------------------------------------------

  PlayListBox.Left := 12;
  PlayListBox.Top := 22;
  PlayListBox.Width  := Self.Width - 23;
  PlayListBox.Height := Self.Height - Panel1.Height - 36;
  Panel1.Left := 11;
  Panel1.Width := Self.Width - 19;
  Panel1.Top := PlayListBox.Top + PlayListBox.Height + 2;

  picTopL.left := 0;
  picTopL.Top  := 0;

  picTopC.left := Self.Width - picTopC.Width;
  picTopC.top  := 0;

  picClose.Left   := Self.Width - 12;
  picClose.Top    := 2;

  picTopText.Left   := (Self.Width div 2) - ((picTopText.Width) div 2);
  picTopText.Top    := 0;

// picTopTextSrc.Left := (Self.Width div 2) - (picTopTextSrc.Width div 2);  //+ 2
//  picTopTextSrc.Top  := picTopText.Top + 4;

  picTopF.left   := picTopText.Left - picTopF.Width;
  picTopF.top    := 0;

  picTopStretchL.left   := picTopL.Left + picTopL.Width;
  picTopStretchL.top    := 0;
  picTopStretchL.Width := picTopF.Left - picTopStretchL.left;

  picTopM.left   := picTopText.Left + picTopText.Width;
  picTopM.top    := 0;

  picTopStretchR.left   := picTopM.Left + picTopM.Width;
  picTopStretchR.top    := 0;
  picTopStretchR.Width  := picTopC.left - picTopStretchR.left;

  WidthTotal := picTopL.Width + picTopStretchL.Width + picTopF.Width + picTopText.Width
                + picTopM.Width + picTopStretchR.Width + picTopC.Width;
  if WidthTotal < Self.Width then
  begin
     picTopStretchL.Width := picTopStretchL.Width + ((Self.Width - WidthTotal + 1) div 2);
     picTopStretchR.Width := picTopStretchR.Width + ((Self.Width - WidthTotal + 1) div 2);
  end;

  picBottomL.left := 0;
  picBottomL.Top  := Self.Height - 14;

  picBottomR.left := Self.Width - 125;
  picBottomR.top  := Self.Height - 14;

  picBorderL.Canvas.MoveTo(0, 20);
  picBorderL.Left   := 0;
  picBorderL.Top    := 20;
  picBorderL.Height := Self.Height - 34;

  picBorderL_hDC := picBorderL.Picture.Bitmap.Canvas.Handle;
  StretchBlt(picBorderL_hDC, 0, 0, 11, Self.Height - 34, PicMapDC, 127, 42, 11, 29, SRCCOPY);
  picBorderL.Refresh;

  picBorderR.Canvas.MoveTo(Self.Width - 8, 20);
  picBorderR.left   := Self.Width - 8;
  picBorderR.Top    := 20;
  picBorderR.Height := Self.Height - 34;

  picBorderR_hDC := picBorderR.Picture.Bitmap.Canvas.Handle;
  StretchBlt(picBorderR_hDC, 0, 0, 8, Self.Height - 34, PicMapDC, 139, 42, 8, 29, SRCCOPY);
  picBorderR.Refresh;

  picBottomM.left   := picBottomL.Width;
  picBottomM.Top    := Self.Height - 14;
  picBottomM.Width  := Self.Width - picBottomL.Width - picBottomR.Width;

end;

function TPlayListConfigForm.WriteTitle(title : string; GetWidthOnly : boolean) : integer;
var
  tmpBitmap : TBitmap;
begin
   if GetWidthOnly then
   begin
      tmpBitmap := TBitmap.Create;
      with tmpBitmap do
      begin
        Canvas.Font.Charset := ANSI_CHARSET;
        Canvas.Font.Name := 'Arial';
        Canvas.Font.Height := 12;
        result := Canvas.TextWidth(title);
        tmpBitmap.Free;
      end;
   end else
     with picTopText.Picture.Bitmap do
     begin
       Canvas.Brush.Style := bsClear;
       Canvas.Font.Charset := ANSI_CHARSET;
       Canvas.Font.Name := 'Arial';
       Canvas.Font.Height := 12;
       if m_bFocused then
          Canvas.Font.Color := RGB(0, 255, 0)
       else
          Canvas.Font.Color := RGB(64, 160, 0);
       Canvas.TextOut(4, 4, title);

       result := Canvas.TextWidth(title);
     end;
end;


procedure TPlayListConfigForm.FormShow(Sender: TObject);
var
  picMap_    : TImage;
begin
  if not m_bShown then
  begin
    DoubleBuffered := True;  // Set to reduce the amount of flicker
    m_bFocused := true;

  // Get the width for the region of title display
    picTopText.Width := WriteTitle(Self.Caption, true) + 8;
  //  RelocateElement(stdWidth, stdHeight);
    RelocateElement(Show_Width, Show_Height);
    ImageCopy;      // Copy basic bitmap images
    WriteTitle(Self.Caption, false);

  // Window position
    SetWindowPos(Self.Handle,
                 HWND_TOP,     // placement-order handle
                 Show_LocX,    // horizontal position
                 Show_LocY,    // vertical position
                 0,   // width
                 0,   // height
                 SWP_NOSIZE + SWP_NOACTIVATE);

    if PlayListBox.UseBackgroundPicture then
     begin
        picMap_    := TImage.Create(Self);
        if FileExists(ExtractFilePath(ParamStr(0)) + 'Skin\PlaylistBack_.bmp') then
        begin
          try
            picMap_.Picture.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Skin\PlaylistBack_.bmp');
          except
            picMap_.Picture.Bitmap.LoadFromResourceName(HInstance, 'PLAYLISTBACK');
          end;
        end else
          picMap_.Picture.Bitmap.LoadFromResourceName(HInstance, 'PLAYLISTBACK');

        PlayListBox.BackgroundPicture := PicMap_.Picture.Bitmap;
        picMap_.Free;
     end;

    m_bShown := true;
  end;
end;


procedure TPlayListConfigForm.CheckTitleBar;
begin
   if GetForegroundWindow <> Self.Handle then
   begin
      if m_bFocused then
      begin
         m_bFocused := false;
         UpperImageCopy;
         UpperImageLoad;
         WriteTitle(Self.Caption, false);
      end;
   end else
     if GetForegroundWindow = Self.Handle then
       if not m_bFocused then
       begin
         m_bFocused := true;
         UpperImageCopy;
         UpperImageLoad;
         WriteTitle(Self.Caption, false);
       end;
end;

procedure TPlayListConfigForm.FormCreate(Sender: TObject);
begin
   ApplyLocalLanguage;
   picMap2 := TImage.Create(Self);
   LoadSkin;

   SetImageMouseEvent(picAddFileBtn);
   SetImageMouseEvent(picAddURLBtn);
   SetImageMouseEvent(picDeleteFileBtn);
   SetImageMouseEvent(picFileInfoBtn);
   SetImageMouseEvent(picAddDirBtn);
   SetImageMouseEvent(picDeleteAllBtn);
   SetImageMouseEvent(picAddListBtn);
   SetImageMouseEvent(picSaveListBtn);
   SetImageMouseEvent(picClose);

   CURSOR_RESIZE := LoadCursor(0, IDC_SIZENWSE);
   CURSOR_RESIZE2 := LoadCursor(0, IDC_SIZENS);
   CURSOR_RESIZE3 := LoadCursor(0, IDC_SIZEWE);
   
   FilePathList := TStringList.Create;
end;

procedure TPlayListConfigForm.ShowAtLoc(Loc_X, Loc_Y, Init_Width, Init_Height : integer; AllowDock : boolean);
begin
   Show_Width := Init_Width;
   if Show_Width < MIN_WIDTH then
      Show_Width := MIN_WIDTH;
   Show_Height := Init_Height;
   if Show_Height < MIN_HEIGHT then
      Show_Height := MIN_HEIGHT;

   Show_LocX := Loc_X;
   Show_LocY := Loc_Y;
 //  Self.Left := Loc_X;
 //  Self.Top := Loc_Y;
   m_bAllowDock := AllowDock;

   Self.Show;
end;

procedure TPlayListConfigForm.FormActivate(Sender: TObject);
begin
   if m_bAllowDock then
      if not Assigned(MagneticWndProc) then
         BassPlayer1.MagneticWindowAdd(Self.Handle, MainForm.Handle, MagneticWndProc);
end;


// Following procedure is not necessary if you adjust the window size using TForm's
//  native function.
{ procedure TPlayListConfigForm.ResizeWindow(point : TPOINT);
var
   rcWnd, rcNew : TRect;
   bResize : boolean;
begin
	GetWindowRect(Self.Handle, rcWnd);
	rcNew := rcWnd;

	bResize := FALSE;

	//Resize in X direction outwards
	if (point.x >= (m_ptResizeFrom.x + RESIZE_WIDTH)) then
	begin
		rcNew.right := rcNew.right + RESIZE_WIDTH;
		if (rcNew.right - rcNew.left) >= MB_MIN_WIDTH then
		begin
			bResize := TRUE;
			m_ptResizeFrom.x := m_ptResizeFrom.x + RESIZE_WIDTH;
		end else
			rcNew.right := rcWnd.right;
	end
	//Resize in X direction inwards
	else if (point.x <= (m_ptResizeFrom.x - RESIZE_WIDTH)) then
	begin
		rcNew.right := rcNew.right - RESIZE_WIDTH;
		if (rcNew.right - rcNew.left) >= (MB_MIN_WIDTH + RESIZE_WIDTH) then
		begin
			bResize := TRUE;
			m_ptResizeFrom.x := m_ptResizeFrom.x - RESIZE_WIDTH;
		end
		else
			rcNew.right := rcWnd.right;
	end;

	//Resize in Y direction downwards
	if (point.y >= (m_ptResizeFrom.y + RESIZE_HEIGHT)) then
	begin
		rcNew.bottom := rcNew.bottom + RESIZE_HEIGHT;
		if (rcNew.bottom - rcNew.Top) >= MB_MIN_HEIGHT then
		begin
			bResize := TRUE;
			m_ptResizeFrom.y := m_ptResizeFrom.y + RESIZE_HEIGHT;
		end	else
			rcNew.bottom := rcWnd.bottom;
	end
	//Resize in Y direction upwards
	else if (point.y <= (m_ptResizeFrom.y - RESIZE_HEIGHT)) then
	begin
		rcNew.bottom := rcNew.bottom - RESIZE_HEIGHT;
		if (rcNew.bottom - rcNew.Top) >= (MB_MIN_HEIGHT + RESIZE_HEIGHT) then
		begin
			bResize := TRUE;
			m_ptResizeFrom.y := m_ptResizeFrom.y - RESIZE_HEIGHT;
		end	else
			rcNew.bottom := rcWnd.bottom;
	end;

	if bResize then
		SetWindowPos(Self.Handle, 0, rcNew.left, rcNew.top,
                (rcNew.right - rcNew.left), (rcNew.bottom - rcNew.Top), SWP_SHOWWINDOW);
end;  }


procedure TPlayListConfigForm.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
 // Restrict the minimum size of form.
   if NewWidth < MIN_WIDTH then
      NewWidth := MIN_WIDTH;
   if NewHeight < MIN_HEIGHT then
      NewHeight := MIN_HEIGHT;

   Resize := true;
end;

procedure TPlayListConfigForm.FormDestroy(Sender: TObject);
var
   p : string;
   f : TextFile;
   i : integer;
begin
   // Save current contents of Playlist as a "Default.M3u" file
   if FilePathList.Count > 0 then
   begin
     p := ExtractFilePath(ParamStr(0));
     if not DirectoryExists(p + 'Playlist') then
        CreateDirectory(pChar(p + 'Playlist'), nil);
     AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Playlist\Default.M3U');
     Rewrite(f);
     Writeln(f, '# Playlist created by BASSPlay');
     for i := 1 to FilePathList.Count do
       writeln(f, FilePathList[i-1]);
     closeFile(f);
   end;

   MagneticWndProc := nil;  // Disable Magnetic effect
   
   FilePathList.Free;
   picMap2.Free;
end;

procedure TPlayListConfigForm.btnCloseClick(Sender: TObject);
begin
   MagneticWndProc := nil;  // Disable Magnetic effect
   Close;
end;

//-------------------------- procedures for Image Buttons -----------------------

procedure TPlayListConfigForm.BtnImageLoad(Btn_No : integer; b_Pressed, b_Highlighted : boolean);
begin
   case Btn_No of
      41 : if b_Pressed then
             SetImage(picAddFileBtn, 0, 0, 22, 18, picMap2, 23, 57)
          else
             SetImage(picAddFileBtn, 0, 0, 22, 18, picMap2, 0, 57);
      42 : if b_Pressed then
             SetImage(picDeleteFileBtn, 0, 0, 22, 18, picMap2, 69, 57)
          else
             SetImage(picDeleteFileBtn, 0, 0, 22, 18, picMap2, 46, 57);
      43 : if b_Pressed then
             SetImage(picFileInfoBtn, 0, 0, 22, 18, picMap2, 23, 76)
          else
             SetImage(picFileInfoBtn, 0, 0, 22, 18, picMap2, 0, 76);
      44 : if b_Pressed then
             SetImage(picAddDirBtn, 0, 0, 22, 18, picMap2, 115, 57)
          else
             SetImage(picAddDirBtn, 0, 0, 22, 18, picMap2, 92, 57);
      45 : if b_Pressed then
             SetImage(picDeleteAllBtn, 0, 0, 22, 18, picMap2, 115, 76)
          else
             SetImage(picDeleteAllBtn, 0, 0, 22, 18, picMap2, 92, 76);
      46 : if b_Pressed then
             SetImage(picAddListBtn, 0, 0, 22, 18, picMap2, 161, 57)
          else
             SetImage(picAddListBtn, 0, 0, 22, 18, picMap2, 138, 57);
      47 : if b_Pressed then
             SetImage(picSaveListBtn, 0, 0, 22, 18, picMap2, 161, 76)
          else
             SetImage(picSaveListBtn, 0, 0, 22, 18, picMap2, 138, 76);
      48 : if b_Pressed then
             SetImage(picAddURLBtn, 0, 0, 22, 18, picMap2, 69, 76)
          else
             SetImage(picAddURLBtn, 0, 0, 22, 18, picMap2, 46, 76);
      99 : if b_Pressed then
              SetImage(picClose, 0, 0, 9, 10, picTopC, 13, 2);
   end;
end;

procedure TPlayListConfigForm.ImageMouseEnter(Sender: TObject);
begin
  i_CursorOn := (Sender as TComponent).Tag;

  if b_BtnPressed then
     exit;

  case (Sender as TComponent).Tag of
    41..48 : BtnImageLoad((Sender as TComponent).Tag, false, true);
  end;
end;

procedure TPlayListConfigForm.ImageMouseLeave(Sender: TObject);
begin
  if (Sender as TComponent).Tag = i_CursorOn then
     i_CursorOn := 0;

 { if b_BtnPressed then
     b_BtnPressed := false; }

  case (Sender as TComponent).Tag of
    41..48 : BtnImageLoad((Sender as TComponent).Tag, false, false);
    99 : BtnImageLoad((Sender as TComponent).Tag, b_BtnPressed, false);
  end;

  if b_BtnPressed then
     b_BtnPressed := false;
end;

procedure TPlayListConfigForm.SetImageMouseEvent(img: TImage);
begin
  img.OnMouseEnter := ImageMouseEnter;
  img.OnMouseLeave := ImageMouseLeave;

  img.Picture.Bitmap.Width := img.Width;
  img.Picture.Bitmap.Height := img.Height;
end;

procedure TPlayListConfigForm.picButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   b_BtnPressed := true;

   SetCapture(Panel1.Handle);
  // BtnImageLoad(Btn_No : integer; b_Pressed, b_Highlighted : boolean);
   BtnImageLoad((Sender as TComponent).Tag, true, false);
end;

procedure TPlayListConfigForm.picButtonMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   b_BtnPressed := false;

   ReleaseCapture;
   if (Sender as TComponent).Tag = i_CursorOn then
   begin
      BtnImageLoad((Sender as TComponent).Tag, false, true);
      case (Sender as TComponent).Tag of
         41 : btnAddFileClick(Self);
         42 : btnDeleteClick(Self);
         43 : btnFileInfoClick(Self);
         44 : btnAddDirClick(Self);
         45 : btnDeleteAllClick(Self);
         46 : btnADDListClick(Self);
         47 : btnSaveListClick(Self);
         48 : btnAddURLClick(Self);
      end;
   end else
      BtnImageLoad((Sender as TComponent).Tag, false, false);
end;

{procedure TPlayListConfigForm.picTopTextSrcMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
   ReleaseCapture;
   Perform(wm_syscommand,$f012, 0);
end; }

procedure TPlayListConfigForm.picTopMDblClick(Sender: TObject);
begin
  ScaleScreen;
end;

procedure TPlayListConfigForm.picCloseMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  picClose_hDC: HDC;

begin
  if not (ssLeft in Shift) then
      exit;

  b_BtnPressed := true;
  PicMapDC := PicMap.Picture.Bitmap.Canvas.Handle;  // ** should be re-defined for each use
  picClose_hDC := picClose.Picture.Bitmap.Canvas.Handle;
  BitBlt(picClose_hDC, 0, 0, 9, 9, PicMapDC, 148, 42, SRCCOPY);
  picClose.Refresh;
end;

procedure TPlayListConfigForm.picCloseClick(Sender: TObject);
begin
   Close;
end;

procedure TPlayListConfigForm.picTopItemsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   m_point  : TPOINT;
begin
   if m_bSizing or (Y <= 2) then
   begin
      SetCursor(CURSOR_RESIZE2);
      if m_bSizing then
      begin
        GetCursorPos(m_point);
        AdjustWindSize(m_point, Top_Side);
     end;
   end;
end;

procedure TPlayListConfigForm.picTopItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
     if (Y <= 2) then
     begin
       SetCursor(CURSOR_RESIZE2);

    // Method 1  (use of TForm's native function)
      { ReleaseCapture;
       Perform(wm_syscommand, $f003, 0); }

    // Method 1 drags form downward if we continue to move mouse cursor downward
    //  after the form's vertical size becomes minimum vertical size.
    // Method 1 may show rectangle line only while moving the window.

    // Method 2  (use of custom made function)
        m_bSizing := True;
        GetCursorPos(m_ptResizeFrom);
     end else
     begin
       ReleaseCapture;
       Perform(wm_syscommand,$f012, 0);   // Drags form
     end;
end;

procedure TPlayListConfigForm.picTopItemsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if m_bSizing then
      m_bSizing := false;
end;

procedure TPlayListConfigForm.picBottomRMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
   if (X >= (picBottomR.Width - 15)) then
      SetCursor(CURSOR_RESIZE);

 // Activate following sentences if you want to resize form by procedure ResizeWindow.
  { if (ssLeft in Shift) and mDrag then
   begin
      GetCursorPos(m_point);
      ResizeWindow(m_point);
   end; }
end;

// Activate procedure picBottomRMouseUp if you want to resize form by procedure ResizeWindow.
{ procedure TPlayListConfigForm.picBottomRMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if mDrag then
      mDrag := false;
end;  }

procedure TPlayListConfigForm.picBottomRMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 // Use following code if you adjust window size using procedure ResizeWindow
 { if (X >= (picBottomR.Width - 15)) then
      if ssLeft in Shift then
      begin
         mDrag := True;
         SetCursor(CURSOR_RESIZE);
         GetCursorPos(m_ptResizeFrom);
      end; }

 // Use following code if you adjust window size using TForm's native function
   if (X >= (picBottomR.Width - 15)) then
      if ssLeft in Shift then
      begin
         SetCursor(CURSOR_RESIZE);

         ReleaseCapture;
         Perform(wm_syscommand,$F008, 0);
      end;
end;

procedure TPlayListConfigForm.picBorderLMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   m_point  : TPOINT;
begin
   if m_bSizing or (X <= 2) then
   begin
      SetCursor(CURSOR_RESIZE3);
      if m_bSizing then
      begin
        GetCursorPos(m_point);
        AdjustWindSize(m_point, Left_Side);
     end;
   end;

end;

procedure TPlayListConfigForm.picBorderLMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
     if (X <= 2) then
     begin
       SetCursor(CURSOR_RESIZE3);

    // Method 1  (use of TForm's native function)
      { ReleaseCapture;
       Perform(wm_syscommand, $f001, 0); }

    // Method 1 drags form rightward if we continue to move mouse cursor rightward
    // after the form's horizontal size becomes minimum horizontal size.

    // Method 2  (use of custom made function)
        m_bSizing := True;
        GetCursorPos(m_ptResizeFrom);
     end;
end;

procedure TPlayListConfigForm.picBorderLMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if m_bSizing then
      m_bSizing := false;
end;

procedure TPlayListConfigForm.picBorderRMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   m_point  : TPOINT;
begin
   if m_bSizing or ((X >= 5) and (X <= 7)) then
   begin
      SetCursor(CURSOR_RESIZE3);
      if m_bSizing then
      begin
        GetCursorPos(m_point);
        AdjustWindSize(m_point, Right_Side);
     end;
   end;

end;

procedure TPlayListConfigForm.picBorderRMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
     if ((X >= 5) and (X <= 7)) then
     begin
       SetCursor(CURSOR_RESIZE3);

    // Method 1  (use of TForm's native function)
      { ReleaseCapture;
       Perform(wm_syscommand, $f002, 0); }

    // Method 1 drags form rightward if we continue to move mouse cursor rightward
    // after the form's horizontal size becomes minimum horizontal size.

    // Method 2  (use of custom made function)
        m_bSizing := True;
        GetCursorPos(m_ptResizeFrom);
     end;
end;

procedure TPlayListConfigForm.picBorderRMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if m_bSizing then
      m_bSizing := false;
end;

//---------------------- end of  procedures for Image Buttons --------------------

//------------------- procedures for Magnetic Effects ---------------------------

procedure TPlayListConfigForm.WMEnterSizeMove(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_ENTERSIZEMOVE, Msg, bHandled);
end;

procedure TPlayListConfigForm.WMSizing(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   if not Assigned(MagneticWndProc) then
      inherited
   else
      if MagneticWndProc(Self.Handle, WM_SIZING, Msg, bHandled) then
         if not bHandled then
            inherited;
end;

procedure TPlayListConfigForm.WMMoving(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   if not Assigned(MagneticWndProc) then
      inherited
   else
      if MagneticWndProc(Self.Handle, WM_MOVING, Msg, bHandled) then
         if not bHandled then
            inherited;
end;

procedure TPlayListConfigForm.WMExitSizeMove(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_EXITSIZEMOVE, Msg, bHandled);
end;

procedure TPlayListConfigForm.WMShowHideWindow(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_WINDOWPOSCHANGED, Msg, bHandled);
end;

procedure TPlayListConfigForm.WMSysCommand(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_SYSCOMMAND, Msg, bHandled);
end;

procedure TPlayListConfigForm.WMCommand(var Msg: TMessage);
var
   bHandled: Boolean;
begin
   inherited;

   if Assigned(MagneticWndProc) then
      MagneticWndProc(Self.Handle, WM_COMMAND, Msg, bHandled);
end;

//------------------ end of procedures for Magnetic Effects----------------------


procedure TPlayListConfigForm.ClearPlayList;
begin
   if (CurMode = plmPlaying) or (CurMode = plmPaused) then
      exit;

   PlayListBox.Clear;
   FilePathList.Clear;
   BassPlayer1.ClearPlayList;
   CurIndex := -1;
end;

procedure TPlayListConfigForm.AddTo_PlayList(FilePath : string; Title, Artist : string; SkipFileCheck : boolean);
begin
   if SkipFileCheck then
   begin
      if Title <> '' then
      begin
         if trim(Artist) = '' then
            PlayListBox.Items.Add(Title)
         else if (not cbShowArtist.Checked) then
            PlayListBox.Items.Add(Title)
         else
            PlayListBox.Items.Add(Artist + ' - ' + Title);

         BassPlayer1.AddToPlayList(FilePath, Title, Artist);
      end
      else begin
         PlayListBox.Items.Add(FilePath);
         BassPlayer1.AddToPlayList(FilePath, FilePath, Artist);
      end;
      FilePathList.Add(FilePath);
   end else
   if BassPlayer1.GetStreamInfo(FilePath, StreamInfo, SupportedBy) then
   begin
      if trim(StreamInfo.Artist) = '' then
         PlayListBox.Items.Add(StreamInfo.Title)
      else if (not cbShowArtist.Checked) then
         PlayListBox.Items.Add(StreamInfo.Title)
      else
         PlayListBox.Items.Add(StreamInfo.Artist + ' - ' + StreamInfo.Title);

      FilePathList.Add(FilePath);
      BassPlayer1.AddToPlayList(FilePath, StreamInfo.Title, StreamInfo.Artist);
   end else
   begin
      PlayListBox.Items.Add(FilePath);
      FilePathList.Add(FilePath);
      BassPlayer1.AddToPlayList(FilePath, FilePath, '');
   end;

   if (FilePathList.Count > 0) and (PlayListBox.ItemIndex = -1) then
   begin
      CurIndex := 0;
      BassPlayer1.PlayListIndex := 0;
      PlayListBox.ItemIndex := 0;
      PlayListBox.Refresh;
   end;
end;

// procedure ChangeTitle is needed for the streams from net.
// The title can be changed continuously if the stream comes from Net radio,
// or the title information may not be obtained at opening the stream. (=> can be
// obtained at the message event of WM_DownLoaded or WM_GetMeta later. )
procedure TPlayListConfigForm.ChangeTitle(FilePath : string; Title, Artist : string);
var
   RegisteredNo : integer;
begin
   if RegisteredFile(FilePath, RegisteredNo) then
   begin
      if trim(Title)  = '' then
         Title := FilePath;
      if cbShowArtist.Checked then
         if Artist <> '' then
            PlayListBox.Items[RegisteredNo] := Artist + ' - ' + Title
         else
            PlayListBox.Items[RegisteredNo] := Title
      else
         PlayListBox.Items[RegisteredNo] := Title;

      BassPlayer1.PlayListChangeTitle(FilePath, Title);
   end;
end;

procedure TPlayListConfigForm.PlayListBoxClick(Sender: TObject);
begin
   if (CurMode <> plmPlaying) and (CurMode <> plmPaused) then
   begin
      CurIndex := PlayListBox.ItemIndex;
      BassPlayer1.PlayListIndex := CurIndex;
      PlayListBox.Refresh;
   end;
end;

procedure TPlayListConfigForm.PlayListBoxDblClick(Sender: TObject);
begin
   if (CurMode = plmPlaying) and (CurIndex = PlayListBox.ItemIndex) then
      exit;

   if FilePathList.Count = 0 then
      exit;

   CurIndex := PlayListBox.ItemIndex;
   BassPlayer1.PlayListIndex := CurIndex;
   PlayListBox.Refresh;
   PostMessage(MainForm.Handle, WM_PlayListConfig, PlayList_DBClick, CurIndex);
end;

procedure TPlayListConfigForm.PlayListBoxDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
   with (Control as TDnDListBox).Canvas do
   begin
      if odSelected in State then
         Brush.Color := clHighlight
      else
         Brush.Color := (Control as TDnDListBox).Color;

    	FillRect(Rect);       // clear the rectangle

      if Index = CurIndex then
         if odSelected in State then
            Font.Color := clYellow
         else
            Font.Color := clRed
      else
         if odSelected in State then
            Font.Color := clWhite
         else
            Font.Color := clBlack;

      Font.Charset := ANSI_CHARSET;
      Font.Name := 'Arial';
      Font.Size := 9;
      TextOut(Rect.Left + 3, Rect.Top + 2, PlayListBox.Items[Index]);
      if odFocused in State then
         DrawFocusRect(Rect);
   end;
end;

procedure TPlayListConfigForm.PlayListBoxFileDropped(Sender: TObject;
  FileName: String; var accept: Boolean);
var
   RegisteredNo : integer;
begin
   if not RegisteredFile(FileName, RegisteredNo) then
   begin
      if BassPlayer1.GetStreamInfo(FileName, StreamInfo, SupportedBy) then
      begin
         if cbShowArtist.Checked then
         begin
            if trim(StreamInfo.Artist) <> '' then
               PlayListBox.Items.Add(StreamInfo.Artist + ' - ' + StreamInfo.Title)
            else
               PlayListBox.Items.Add(StreamInfo.Title);
         end else
            PlayListBox.Items.Add(StreamInfo.Title);

         FilePathList.Add(FileName);
         BassPlayer1.AddToPlayList(FileName, StreamInfo.Title, StreamInfo.Artist);
      end else
         Application.MessageBox(pChar(msg[2] + chr(10) + ' -> ' +
                             FileName), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
   end else
      Application.MessageBox(pChar(msg[3] + chr(10) + ' -> ' +
                             FileName), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);

   if (FilePathList.Count > 0) and (PlayListBox.ItemIndex = -1) then
   begin
      CurIndex := 0;
      BassPlayer1.PlayListIndex := 0;
      PlayListBox.ItemIndex := 0;
      PlayListBox.Refresh;
   end;
end;

procedure TPlayListConfigForm.btnAddFileClick(Sender: TObject);
var
   s1, s2, s3 : string;
   RegisteredNo : integer;
begin
   OpenDialog1.FileName := '';
   s1 := BassPlayer1.NativeFileExts;
   s2 := BassPlayer1.PluginFileExts;
   s3 := BassPlayer1.BASSAddonExts;
   if (s2 = '') and (s3 = '') then
      OpenDialog1.Filter := 'BASS native files (' + s1 + ')|' + s1 + '|'
   else
      OpenDialog1.Filter := 'All playable files |' + s1 + s2 + s3 + '|' +
                            'BASS native files (' + s1 + ')|' + s1 + '|';
   if s2 <> '' then
      OpenDialog1.Filter := OpenDialog1.Filter + 'Winamp plug-in supported files (' + s2 + ')|' + s2 + '|';
   if s3 <> '' then
      OpenDialog1.Filter := OpenDialog1.Filter + 'BASS add-on supported files (' + s3 + ')|' + s3 + '|' ;

   if OpenDialog1.Execute then
      if not RegisteredFile(OpenDialog1.FileName, RegisteredNo) then
      begin
         if BassPlayer1.GetStreamInfo(OpenDialog1.FileName, StreamInfo, SupportedBy) then
         begin
            if trim(StreamInfo.Artist) = '' then
               PlayListBox.Items.Add(StreamInfo.Title)
            else if (not cbShowArtist.Checked) then
               PlayListBox.Items.Add(StreamInfo.Title)
            else
               PlayListBox.Items.Add(StreamInfo.Artist + ' - ' + StreamInfo.Title);

            FilePathList.Add(OpenDialog1.FileName);
            BassPlayer1.AddToPlayList(OpenDialog1.FileName, StreamInfo.Title, StreamInfo.Artist);
         end else
            Application.MessageBox(pChar(msg[2] + chr(10) + ' -> ' +
                 OpenDialog1.FileName), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
      end else
         Application.MessageBox(pChar(msg[3] + chr(10) + ' -> ' +
                 OpenDialog1.FileName), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);

   if (FilePathList.Count > 0) and (PlayListBox.ItemIndex = -1) then
   begin
      CurIndex := 0;
      BassPlayer1.PlayListIndex := 0;
      PlayListBox.ItemIndex := 0;
      PlayListBox.Refresh;
   end;

end;

procedure TPlayListConfigForm.btnAddURLClick(Sender: TObject);
var
   s : string;
   RegisteredNo : integer;
begin
   URLInputForm.ShowModal;

   if URLInputForm.RequestToOpen then
   begin
     s := trim(URLInputForm.ComboBox1.Text);
     if s <> '' then
        if not RegisteredFile(s, RegisteredNo) then
        begin
           if BassPlayer1.IsValidURL(s) then
           begin
              PlayListBox.Items.Add(s);
              FilePathList.Add(s);
              BassPlayer1.AddToPlayList(s, s, '');
           end else
              Application.MessageBox(pChar(msg[13] + chr(10) + ' -> ' +
                     s), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
        end else
           Application.MessageBox(pChar(msg[14] + chr(10) + ' -> ' +
                 OpenDialog1.FileName), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
   end else
      exit;

   if (FilePathList.Count > 0) and (PlayListBox.ItemIndex = -1) then
   begin
      CurIndex := 0;
      BassPlayer1.PlayListIndex := 0;
      PlayListBox.ItemIndex := 0;
      PlayListBox.Refresh;
   end;

end;


function TPlayListConfigForm.FillPlayListItems(PlaylistName : string) : integer;
var
   s_ext : string;
   Header1, Header2 : string;

 function IsURLHeader(S : string) : boolean;
 begin
    Header1 := copy(S, 1, 7);
    Header2 := copy(S, 1, 6);
    if (Header1 = 'http://') or (Header2 = 'mms://') then
       result := true
    else
       result := false;
 end;

 function AddFromM3UList(M3UListName : string) : integer;
 var
    i, AddedCounter : Integer;
    WinAmpList : TStringList;
    ext : string;
 begin
    result := 0;
    AddedCounter := 0;
    WinAmpList := TStringList.Create;
  //  M3UListName := ExpandFileName (M3UListName);

    try
      { try to open playlist file }
      WinAmpList.LoadFromFile(M3UListName);
    except
      exit;
    end; {try}

  { if playlist file has been read ok then we should process it }
    for i := 0 to WinAmpList.Count - 1 do begin
      if WinAmpList.Strings[i][1] = '#' then
         continue;

      if IsURLHeader(WinAmpList.Strings[i]) then
      begin
      // No pre-checking for URLs
         RegisterFile(WinAmpList.Strings[i], '', WinAmpList.Strings[i]);
         inc(AddedCounter);
         Continue;
      end else if WinAmpList.Strings[i][2] <> ':' then
        if WinAmpList.Strings[i][1] = '\' then
          { if item path starts with \ then add Playlist drive }
          WinAmpList.Strings[i] := Copy(M3UListName, 1, 2) + WinAmpList.Strings[i]
        else
          { in other cases assume that item contains relative path to PlayList path }
            WinAmpList.Strings[i] := ExtractFilePath(M3UListName) + WinAmpList.Strings[i];

      if FileExists(WinAmpList.Strings[i]) then
      begin
        ext := LowerCase(ExtractFileExt(WinAmpList.Strings[i]));
        if ext <> '' then
          if pos(ext + ';', s_ext) <> 0 then   // currently supported stream type ?
          // pre-check for local files
            if BassPlayer1.GetStreamInfo(WinAmpList.Strings[i], StreamInfo, SupportedBy) then
            begin
               RegisterFile(WinAmpList.Strings[i], StreamInfo.Artist, StreamInfo.Title);
               inc(AddedCounter);
            end;
      end;

      { if FileList.Count >= NumLimit then
           break; }
      { On each 30 iterations, release processor to allow OS do something
        else too }
      if (i MOD 30) = 0 then Application.ProcessMessages;
    end; { for }

    WinAmpList.Free;
    result := AddedCounter;
 end;

 function AddFromPLSList(PLSListName : string) : integer;
 var
    i, count : Integer;
    AddedCounter : integer;
    PLSList : TINIFile;
    tempstr : string;
    ext : string;
 begin
    result := 0;
    AddedCounter := 0;
    { Load playlist }
    PLSList := TINIFile.Create(PLSListName);

  { read data }
    try
      count := PLSList.ReadInteger('playlist', 'NumberOfEntries', 0);
    except
      exit;
    end; {try}

  { if playlist file has been read ok then we should process it }
    for i := 0 to count do begin
      tempstr := PLSList.ReadString('playlist', 'File'+IntToStr(i), '');
      if tempstr <> '' then begin
        if IsURLHeader(tempstr) then
        begin
         // No pre-checking for URLs
           RegisterFile(tempstr, '', tempstr);
           inc(AddedCounter);
           Continue;
        end else if tempstr[2] <> ':' then
          if tempstr[1] = '\' then
            { if item path starts with \ then add Playlist drive }
            tempstr := Copy(PLSListName, 1, 2) + tempstr
          else
            { in other cases assume that item contains relative path to PlayList path }
              tempstr := ExtractFilePath(PLSListName) + tempstr;

        if FileExists(tempstr) then
        begin
          ext := LowerCase(ExtractFileExt(tempstr));

          if ext <> '' then
            if pos(ext + ';', s_ext) <> 0 then   // currently supported stream type ?
            // pre-check for local files
              if BassPlayer1.GetStreamInfo(tempstr, StreamInfo, SupportedBy) then
              begin
                 RegisterFile(tempstr, StreamInfo.Artist, StreamInfo.Title);
                 inc(AddedCounter);
              end;
        end;

        { if FileList.Count >= NumLimit then
             break;  }

        { On each 30 iterations, release processor to allow OS do something
          else too }
        if (i MOD 30) = 0 then Application.ProcessMessages;
      end; { if }
    end; { for }

    PLSList.Free;
    result := AddedCounter;
 end;

begin
   result := 0;
   s_ext := BassPlayer1.NativeFileExts + BassPlayer1.PluginFileExts + BassPlayer1.BASSAddonExts;

   if UpperCase(ExtractFileExt(PlaylistName)) = '.M3U' then
      result := AddFromM3UList(PlaylistName)
   else if UpperCase(ExtractFileExt(PlaylistName)) = '.PLS' then
      result := AddFromPLSList(PlaylistName);
end;

function TPlayListConfigForm.AddPlayList(S : string) : integer;
begin
   if not FileExists(S) then
   begin
      result := 0;
      exit;
   end;

   result := FillPlayListItems(S);

   if (FilePathList.Count > 0) and (PlayListBox.ItemIndex = -1) then
   begin
      CurIndex := 0;
      BassPlayer1.PlayListIndex := 0;
      PlayListBox.ItemIndex := 0;
      PlayListBox.Refresh;
   end;
end;

procedure TPlayListConfigForm.btnADDListClick(Sender: TObject);
var
   p : string;
begin
   if OpenDialog1.InitialDir = '' then
   begin
      p := ExtractFilePath(ParamStr(0));
      if not DirectoryExists(p + 'Playlist') then
         CreateDirectory(pChar(p + 'Playlist'), nil);

      OpenDialog1.InitialDir := p + 'Playlist';
   end;

   OpenDialog1.Filter := 'Playlist files (*.M3U;*.PLS;) |*.M3U;*.PLS;';
   OpenDialog1.FileName := '';  // Clear previously selected item
   if not OpenDialog1.Execute then
      exit;

   AddPlayList(OpenDialog1.FileName);
end;


procedure TPlayListConfigForm.RegisterFile(FileName, Artist, Title : string);
var
  RegisteredNo : integer;

begin
      if not RegisteredFile(FileName, RegisteredNo) then
      begin
          if trim(Artist) = '' then
             PlayListBox.Items.Add(Title)
          else if (not cbShowArtist.Checked) then
             PlayListBox.Items.Add(Title)
          else
             PlayListBox.Items.Add(Artist + ' - ' + Title);

          FilePathList.Add(FileName);
          BassPlayer1.AddToPlayList(FileName, Title, Artist);
      end;
end;

procedure TPlayListConfigForm.btnAddDirClick(Sender: TObject);
var
   f, ext, s_ext : string;
   Dir_Path : string;
   SearchRec: TSearchRec;
   StreamInfo: TStreamInfo;
   SupportedBy: TSupportedBy;
 //  RegisteredNo : integer;

begin
   if not SelectDirectory('Open Directory', '', Dir_Path) then
      exit;

   if FindFirst(Dir_Path + '\*.*', faAnyFile, SearchRec) <> 0 then
   begin
     FindClose(SearchRec);
     exit;
   end;

   s_ext := BassPlayer1.NativeFileExts + BassPlayer1.PluginFileExts + BassPlayer1.BASSAddonExts;

   f := SearchRec.Name;
   ext := LowerCase(ExtractFileExt(f));
   if ext <> '' then
      if pos(ext + ';', s_ext) <> 0 then
         if BassPlayer1.GetStreamInfo(Dir_Path + '\' + f, StreamInfo, SupportedBy) then
            RegisterFile(Dir_Path + '\' + f, StreamInfo.Artist, StreamInfo.Title);

   while FindNext(SearchRec) = 0 do
   begin
      f := SearchRec.Name;
      ext := LowerCase(ExtractFileExt(f));
      if ext <> '' then
        if pos(ext + ';', s_ext) <> 0 then
           if BassPlayer1.GetStreamInfo(Dir_Path + '\' + f, StreamInfo, SupportedBy) then
              RegisterFile(Dir_Path + '\' + f, StreamInfo.Artist, StreamInfo.Title);
   end;

   FindClose(SearchRec);

   if (FilePathList.Count > 0) and (PlayListBox.ItemIndex = -1) then
   begin
      CurIndex := 0;
      BassPlayer1.PlayListIndex := 0;
      PlayListBox.ItemIndex := 0;
      PlayListBox.Refresh;
   end;
end;

procedure TPlayListConfigForm.btnDeleteClick(Sender: TObject);
var
   DeleteIndex : integer;
   DeletedPath : string;
   D_Path : pchar;
begin
   if PlayListBox.ItemIndex = -1 then
   begin
      Application.MessageBox(pChar(msg[1]), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   DeleteIndex := PlayListBox.ItemIndex;
   if (CurMode = plmPlaying) or (CurMode = plmPaused) then
      if DeleteIndex = CurIndex then
      begin
         Application.MessageBox(pChar(msg[4]), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
         exit;
      end;

   DeletedPath := FilePathList[DeleteIndex];
   PlayListBox.Items.Delete(DeleteIndex);
   FilePathList.Delete(DeleteIndex);
   BassPlayer1.DeleteFromPlayList(DeleteIndex);
   if DeleteIndex < CurIndex then
      Dec(CurIndex);

   if DeleteIndex = FilePathList.Count then
      PlayListBox.ItemIndex := DeleteIndex - 1
   else
      PlayListBox.ItemIndex := DeleteIndex;

   PlayListBox.SetFocus;

   if FilePathList.Count = 0 then
      CurIndex := -1;

   D_Path := pChar(DeletedPath);
   PostMessage(MainForm.Handle, WM_PlayListConfig, PlayList_Delete, integer(D_Path));
   BassPlayer1.PlayListIndex := CurIndex;

end;

procedure TPlayListConfigForm.btnDeleteAllClick(Sender: TObject);
var
   i : integer;
   DeleteIndex : integer;
   DeletedPath : string;
   D_Path : pchar;
   SkippedAnItem : boolean;
begin
   if PlayListBox.Items.Count = 0 then
      exit;

   SkippedAnItem := false;

   if Application.MessageBox(pChar(msg[12]), pChar(msg[7]), MB_OKCANCEL) <> IDOK then
      exit;

   for i := PlayListBox.Items.Count downto 1 do
   begin
      DeleteIndex := PlayListBox.Items.Count - 1;

      if DeleteIndex = CurIndex then
        if (CurMode = plmPlaying) or (CurMode = plmPaused) then
        begin
          SkippedAnItem := true;
          dec(DeleteIndex);
          if DeleteIndex = - 1 then
             break;
        end;

     DeletedPath := FilePathList[DeleteIndex];
     PlayListBox.Items.Delete(DeleteIndex);
     FilePathList.Delete(DeleteIndex);
     BassPlayer1.DeleteFromPlayList(DeleteIndex);
     if DeleteIndex < CurIndex then
        Dec(CurIndex);

     if DeleteIndex = FilePathList.Count then
       PlayListBox.ItemIndex := DeleteIndex - 1
     else
       PlayListBox.ItemIndex := DeleteIndex;

     if FilePathList.Count = 0 then
       CurIndex := -1;

     D_Path := pChar(DeletedPath);
     PostMessage(MainForm.Handle, WM_PlayListConfig, PlayList_Delete, integer(D_Path));
     BassPlayer1.PlayListIndex := CurIndex;
   end;

   if SkippedAnItem then
      Application.MessageBox(pChar(msg[9]), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
end;

procedure TPlayListConfigForm.btnFileInfoClick(Sender: TObject);
var
   ViewIndex : integer;
   ChangedAny : boolean;
begin
   if not BassPlayer1.PlayerReady then
   begin
      Application.MessageBox(pChar(msg[5]), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   if PlayListBox.ItemIndex = -1 then
   begin
      Application.MessageBox(pChar(msg[1]), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   ViewIndex := PlayListBox.ItemIndex;

   if not BassPlayer1.FileInfoBox(FilePathList[ViewIndex]) then
   begin
      Application.MessageBox(pChar(msg[6]), pChar(msg[8]), MB_OK + MB_ICONINFORMATION);
      exit;
   end;

   ChangedAny := false;
   if BassPlayer1.GetStreamInfo(FilePathList[ViewIndex], StreamInfo, SupportedBy) then
      if cbShowArtist.Checked then
      begin
         if trim(StreamInfo.Artist) <> '' then
         begin
            if PlayListBox.Items[ViewIndex] <> (StreamInfo.Artist + ' - ' + StreamInfo.Title) then
            begin
               PlayListBox.Items[ViewIndex] := StreamInfo.Artist + ' - ' + StreamInfo.Title;
               ChangedAny := true;
            end;
         end else
            if PlayListBox.Items[ViewIndex] <> StreamInfo.Title then
            begin
               PlayListBox.Items[ViewIndex] := StreamInfo.Title;
               ChangedAny := true;
            end;
      end else
      if PlayListBox.Items[ViewIndex] <> StreamInfo.Title then
      begin
         PlayListBox.Items[ViewIndex] := StreamInfo.Title;
         ChangedAny := true;
      end;

   if ChangedAny then
   begin
      if FilePathList[ViewIndex] = BassPlayer1.StreamPath then
         PostMessage(MainForm.Handle, WM_PlayListConfig, Title_Changed, 0);

      BassPlayer1.PlayListChangeTitle(FilePathList[ViewIndex], StreamInfo.Title);
   end;

end;

procedure TPlayListConfigForm.btnSaveListClick(Sender: TObject);
var
   f : TextFile;
   i : integer;
   p : string;
begin
   if FilePathList.Count = 0 then
   begin
      Application.MessageBox(pChar(msg[10]), pChar(msg[7]), MB_OK or MB_ICONEXCLAMATION);
      exit;
   end;

   if SaveDialog1.InitialDir = '' then
   begin
      p := ExtractFilePath(ParamStr(0));
      if not DirectoryExists(p + 'Playlist') then
         CreateDirectory(pChar(p + 'Playlist'), nil);
      SaveDialog1.InitialDir := p + 'Playlist';
   end;

   SaveDialog1.Filter := 'Winamp Playlist file (*.M3U;) |*.M3U;';
   if OpenDialog1.FileName <> '' then
      SaveDialog1.FileName := ExtractFileName(OpenDialog1.FileName);

   if not SaveDialog1.Execute then
      exit;

   if UpperCase(ExtractFileExt(SaveDialog1.FileName)) <> '.M3U' then
      SaveDialog1.FileName := SaveDialog1.FileName + '.M3U';

   if FileExists(SaveDialog1.FileName) then
   begin
      if Application.MessageBox(pChar(msg[11]), pChar(msg[7]), MB_OKCANCEL) <> IDOK then
         exit;
   end;

   AssignFile(f, SaveDialog1.FileName);
   Rewrite(f);
   Writeln(f, '# Playlist created by BASSPlay');
   for i := 1 to FilePathList.Count do
       writeln(f, FilePathList[i-1]);

   closeFile(f);
end;

function TPlayListConfigForm.NumEntry : integer;
begin
   result := FilePathList.Count;
end;

function TPlayListConfigForm.SelectedEntry : integer;
begin
   result := CurIndex;
end;

function TPlayListConfigForm.SelectedFile  : string;
begin
   if FilePathList.Count > 0 then
      result := FilePathList[CurIndex]
   else
      result := '';
end;

function TPlayListConfigForm.SelectEntry(ListIndex : integer) : string;
// Selects an entry by index number.
// returns the file path of selected entry on success, null on failure
begin
   if (ListIndex < 0) or (ListIndex >= FilePathList.Count) then
   begin
      result := '';
      exit;
   end else
   begin
      CurIndex := ListIndex;
      BassPlayer1.PlayListIndex := CurIndex;
      PlayListBox.Refresh;
      result := FilePathList[ListIndex];
   end;
end;

function TPlayListConfigForm.SelectEntryByName(FilePath : string) : integer;
// Selects an entry by file path.
// returns the index number of selected entry on success, -1 on failure
var
   i : integer;
begin
   result := -1;

   if (FilePathList.Count > 0) then
      for i := 0 to (FilePathList.Count - 1) do
      begin
         if FilePathList[i] = FilePath then
         begin
            CurIndex := i;
            BassPlayer1.PlayListIndex := CurIndex;
            PlayListBox.Refresh;
            result := CurIndex;
         end;
      end;
end;

procedure TPlayListConfigForm.SetCurrentMode(Mode : TPlayerMode);
begin
   CurMode := Mode;

end;

procedure TPlayListConfigForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 // inform to main form
   PostMessage(MainForm.Handle, WM_PlayListConfig, PlayList_Close, 0);
end;


procedure TPlayListConfigForm.picBottomItemsMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
   m_point  : TPOINT;
begin
   if ((Sender as TImage).Name = 'picBottomR') and (X >= (picBottomR.Width - 15)) then
      SetCursor(CURSOR_RESIZE)
   else if m_bSizing or (Y >= 11) then
   begin
      SetCursor(CURSOR_RESIZE2);
      if m_bSizing then
      begin
        GetCursorPos(m_point);
        AdjustWindSize(m_point, Bottom_Side);
     end;
   end;

end;

procedure TPlayListConfigForm.picBottomItemsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if ssLeft in Shift then
   begin
     if ((Sender as TImage).Name = 'picBottomR') and
         (X >= (picBottomR.Width - 15)) then
     begin
         SetCursor(CURSOR_RESIZE);

         ReleaseCapture;
         Perform(wm_syscommand,$F008, 0);
     end
     else if (Y >= 11) then
     begin
       SetCursor(CURSOR_RESIZE2);

    // Method 1  (use of TForm's native function)
      { ReleaseCapture;
       Perform(wm_syscommand, $f006, 0); }

    // Method 1 drags form downward if we continue to move mouse cursor downward
    //  after the form's vertical size becomes minimum vertical size.
    // Method 1 may show rectangle line only while moving the window.

    // Method 2  (use of custom made function)
        m_bSizing := True;
        GetCursorPos(m_ptResizeFrom);
     end;
  end;   
end;

procedure TPlayListConfigForm.picBottomItemsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
   if m_bSizing then
      m_bSizing := false;
end;


end.
