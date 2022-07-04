unit CHFolderDialog;

interface

uses Windows, Messages, SysUtils, Classes, ShellAPI, Forms, ShlObj, ActiveX;

type
  TOnSelChanged = procedure (Sender: TObject; const Path: string;
   var EnableOk: Boolean) of object;

  TOnSetPosition = procedure (Sender: TObject; var Left, Top: Integer;
   Width, Height: Integer) of object;


  TCHFolderDialog = class (TComponent)
  private
    FPath: string;
    FStatusText: string;
    FOnSelChanged: TOnSelChanged;
    FTitle: string;
    FNewDialogStyle: Boolean;
    FEditBox: Boolean;
    FWnd: HWND;
    FOnSetPosition: TOnSetPosition;
    procedure CallbackInitialized(Wnd: HWND);
    procedure CallbackSelChanged(Wnd: HWND; PIDL: PItemIDList); overload;
    procedure DoSelChanged(Path: string; var EnableOk: Boolean);
    procedure DoSetPosition(var Left, Top: Integer; Width, Height: Integer);
    procedure SetOnSelChanged(const Value: TOnSelChanged);
    procedure SetOnSetPosition(const Value: TOnSetPosition);
  protected
    procedure SelChanged(Path: string; var EnableOk: Boolean); virtual;
    procedure SetPosition(var Left, Top: Integer; Width, Height: Integer); virtual;
  public
    constructor Create(AOwner : TComponent); override;

    { The Execute function displays the browsing dialog. This function
      returns True and places selected folder name to the Path property
      when the user presses OK button of the browsing dialog. Otherwise
      this function returns False and makes no changes in the Path property. }
    function Execute: Boolean;
    { Use the SetStatusText method to display new text in status area of
      the browing dialog. This method may be used in OnSelChanged event
      handler only. }
    procedure SetStatusText(const StatusText: string);
  published
    { Use the Path to specify the folder that would be initially selected
      in the browsing dialog. This propery is also used to obtain the
      folder name that is selected by user when the Execute method
      returnes True. }
    property Path: string read FPath write FPath;
    { The StatusText property specifies the message that is displayed
      below the browsing dialog title.}
    property StatusText: string read FStatusText write FStatusText;
    { The Title property specifies the text that is displayed
      below the browsing dialog caption and above the status text area.}
    property Title: string read FTitle write FTitle;
    { The NewDialogStyle property specifies the new or old user-interface.
      Setting property to True provides the user with a larger dialog box
      that can be resized. It has several new capabilities including:
      drag and drop capability within the dialog box, reordering,
      context menus, new folders, delete, and other context menu commands.
      This property is ignored when the user system has the Shell32.dll
      with the version lower than 5.00 }
    property NewDialogStyle: Boolean read FNewDialogStyle write FNewDialogStyle;
    { Setting the EditBox property to True includes an edit control in the
     browse dialog that allows the user to type the name of an item. }
    property EditBox: Boolean read FEditBox write FEditBox;
    { The OnSelChanged event occurs when the user chooses a folder in
      the browsing dialog. The Sender parameter of this event (see above)
      is the object whose event handler is called. The Path parameter is
      the name of a choosen folder. EnableOk variable parameter specifies
      enabling or disabling the OK button in the browsing dialog. }
    property OnSelChanged: TOnSelChanged read FOnSelChanged
      write SetOnSelChanged;
    { The OnSetPosition event occurs prior to displaying the browsing dialog.
      Use this property to change the default dialog box position by changing
      the Left and Top variables at this event handler. The unchangeable Width
      and Height parameters specify the brosing dialog size. }
    property OnSetPosition: TOnSetPosition read FOnSetPosition write SetOnSetPosition;
  end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('CH Pack', [TCHFolderDialog]);
end;

{ TCHFolderDialog }

constructor TCHFolderDialog.Create(AOwner: TComponent);
begin
  inherited;
  FTitle := 'Folder';
  FNewDialogStyle := True;
end;

function BrowseForFolderCallBack(Wnd: HWND; Msg: UINT;
  lParam: PItemIDList; lpData: TCHFolderDialog): Integer stdcall;
begin
 case Msg of
  BFFM_INITIALIZED: lpData.CallbackInitialized(Wnd);
  BFFM_SELCHANGED : lpData.CallbackSelChanged(Wnd, lParam);
 end;
 Result:=0;
end;

procedure TCHFolderDialog.CallbackInitialized(Wnd: HWND);
var
 Rect: TRect;
 Left, Top, Width, Height: Integer;
begin
 if FStatusText <> '' then
  SendMessage(Wnd, BFFM_SETSTATUSTEXT, 0, LongInt(PChar(FStatusText)));
 if FPath <> '' then
  SendMessage(Wnd, BFFM_SETSELECTION, Integer(True), LongInt(PChar(FPath)));
 GetWindowRect(Wnd, Rect);
 Left:=Rect.Left;
 Top:=Rect.Top;
 Width:=Rect.Right-Rect.Left;
 Height:=Rect.Bottom-Rect.Top;
 DoSetPosition(Left, Top, Width, Height);
 if (Left <> Rect.Left) or (Top <> Rect.Top)
  then MoveWindow(Wnd, Left, Top, Width, Height, False);
end;

procedure TCHFolderDialog.CallbackSelChanged(Wnd: HWND;
  PIDL: PItemIDList);
var
 Buffer: array [0..MAX_PATH-1] of AnsiChar;
 EnableOk: Boolean;
 Dir: string;
begin
 FWnd:=Wnd;
 try
  SHGetPathFromIDList(PIDL, @Buffer[0]);
  Dir:=PChar(@Buffer[0]);
  EnableOk:=DirectoryExists(Dir);
  DoSelChanged(PChar(@Buffer[0]), EnableOk);
  SendMessage(Wnd, BFFM_EnableOk, 0, Integer(EnableOk));
 finally
  FWnd:=0;
 end;
end;



procedure TCHFolderDialog.DoSelChanged(Path: string; var EnableOk: Boolean);
begin
 if Assigned(FOnSelChanged) then FOnSelChanged(Self, Path, EnableOk);
 SelChanged(Path, EnableOk);
end;

procedure TCHFolderDialog.DoSetPosition(var Left, Top: Integer; Width,
  Height: Integer);
begin
 if Assigned(FOnSetPosition) then FOnSetPosition(Self, Left, Top, Width, Height);
 SetPosition(Left, Top, Width, Height);
end;

function TCHFolderDialog.Execute: Boolean;
var
 BrowseInfo: TBrowseInfo;
 RootPIDL, ResultPIDL: PItemIDList;
 Buffer: array [0..MAX_PATH-1] of AnsiChar;
 SHMalloc: IMalloc;
begin
 SHGetSpecialFolderLocation(Application.Handle, CSIDL_DESKTOP, RootPIDL);
 FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
 BrowseInfo.hwndOwner:=Application.Handle;
 BrowseInfo.pidlRoot:=RootPIDL;
 BrowseInfo.pszDisplayName:=nil;
 if FTitle<>'' then BrowseInfo.lpszTitle:=PChar(FTitle) else
  BrowseInfo.lpszTitle:=nil;
 BrowseInfo.ulFlags:=0;
 if (FStatusText<>'') and
  (not FNewDialogStyle) then BrowseInfo.ulFlags:=BrowseInfo.ulFlags or $0004; // BIF_STATUSTEXT;
 if FEditBox then BrowseInfo.ulFlags:=BrowseInfo.ulFlags or $0010; //BIF_EDITBOX;
 if FNewDialogStyle then begin
  OleInitialize(nil);
  BrowseInfo.ulFlags:=BrowseInfo.ulFlags or $0040; //BIF_NEWDIALOGSTYLE;
 end;
 BrowseInfo.lpfn:=@BrowseForFolderCallback;
 BrowseInfo.iImage:=0;
 BrowseInfo.lParam:=Integer(Self);
 FWnd:=0;
 ResultPIDL:=SHBrowseForFolder(BrowseInfo);
 Result:=False;
 if Assigned(ResultPIDL) then begin
  SHGetPathFromIDList(ResultPIDL, @Buffer[0]);
  FPath:=PChar(@Buffer[0]);
  Result:=True;
  SHGetMalloc(SHMalloc);
  SHMalloc.Free(ResultPIDL);
 end;
 if FNewDialogStyle then OleUninitialize;
end;

procedure TCHFolderDialog.SelChanged(Path: string; var EnableOk: Boolean);
begin
 // no action by default
end;

procedure TCHFolderDialog.SetOnSelChanged(const Value: TOnSelChanged);
begin
  FOnSelChanged := Value;
end;

procedure TCHFolderDialog.SetOnSetPosition(const Value: TOnSetPosition);
begin
  FOnSetPosition := Value;
end;

procedure TCHFolderDialog.SetPosition(var Left, Top: Integer; Width,
  Height: Integer);
begin
 // no action by default
end;

procedure TCHFolderDialog.SetStatusText(const StatusText: string);
begin
 if (FWnd<>0) and (FStatusText<>'') then
  SendMessage(FWnd, BFFM_SETSTATUSTEXT, 0, LongInt(PChar(StatusText)));
end;

end.
